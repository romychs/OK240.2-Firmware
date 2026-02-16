; =======================================================
; Ocean-240.2
; CPM CPP, ROM PART
; AT 0xDB00
;
; Disassembled by Romych 2025-09-09
; =======================================================

    INCLUDE "equates.inc"
    INCLUDE "ram.inc"

    IFNDEF  BUILD_ROM
        OUTPUT ccp_rom.bin
    ENDIF

    MODULE  CCP_ROM

    ORG 0xDB00

@ccp_entry:
    LD   HL, 0x0                                    ; prevent stack overflow
    ADD  HL, SP
    LD   (CPM_VARS.saved_stack_ptr), HL
    LD   SP, CPM_VARS.ccp_safe_stack

    CALL get_cmd_index
    LD   HL, ccp_commands                           ;= DB6Ch
    LD   E, A
    LD   D, 0x0
    ADD  HL, DE
    ADD  HL, DE
    LD   A, (HL)
    INC  HL
    LD   H, (HL)
    LD   L, A
    JP   (HL)                                       ; jump to command

ccp_commands_str:
    DB  "SDIR READ WRITE"

; -------------------------------------------------------
; Search user command position in available commands list
; -------------------------------------------------------
get_cmd_index:
    LD   HL, ccp_commands_str                       ; -> 'DIR'
    LD   C, 0x0
.cmd_next:
    LD   A, C
    CP   CCP_COMMAND_COUNT
    RET  NC
    LD   DE, CCP_RAM.ccp_current_fcb_fn
    LD   B, CCP_COMMAND_SIZE
.cmp_nxt:
    LD   A, (DE)
    CP   (HL)                                       ; -> 'DIR'
    JP   NZ, .no_eq
    INC  DE
    INC  HL
    DEC  B
    JP   NZ, .cmp_nxt
    LD   A, (DE)
    CP   ASCII_SP
    JP   NZ, .inc_next
    LD   A, C
    RET
.no_eq:
    INC  HL
    DEC  B
    JP   NZ, .no_eq
.inc_next:
    INC  C
    JP   .cmd_next

; --------------------------------------------------
; Command handlers ref table
; --------------------------------------------------
ccp_commands:
    DW   ccp_dir
    DW   ccp_read
    DW   ccp_write
    DW   ccp_ret                                    ; r8
;    DW   ccp_exit1                                 ; r8

ccp_ret:
    LD   HL, (CPM_VARS.saved_stack_ptr)
    LD   SP, HL
    JP   CCP_RAM.ccp_unk_cmd

;ccp_exit1:
;    JP   MON.mon_hexb
; --------------------------------------------------
; DIR [filemask] command handler
; --------------------------------------------------
ccp_dir:
    CALL CCP_RAM.ccp_cv_first_to_fcb
    CALL CCP_RAM.ccp_drive_sel
    ; chech some filemask specified in command line
    LD   HL, CCP_RAM.ccp_current_fcb_fn
    LD   A, (HL)
    CP   ' '
    JP   NZ, .has_par

    ; no filemask, fill with wildcard '?'
    LD   B, 11
.fill_wildcard:
    LD   (HL), '?'
    INC  HL
    DEC  B
    JP   NZ, .fill_wildcard

    ; find file by specified mask
.has_par:
    CALL CCP_RAM.ccp_find_first
    JP   NZ, .f_found
    ; no files found, print and exit
    CALL CCP_RAM.ccp_out_no_file
    JP   CCP_RAM.ccp_cmdline_back

.f_found:
    CALL CCP_RAM.ccp_out_crlf
    LD   HL, 0x0
    LD   (CPM_VARS.tmp_dir_total), HL
    LD   E, 0

.do_next_direntry:
    PUSH DE
    CALL CCP_RAM.ccp_find_first
    POP  DE
    PUSH DE

    ; Find file with e number
.find_file_e:
    DEC  E
    JP   M, .file_out_next
    PUSH DE
    CALL CCP_RAM.ccp_bdos_find_next
    POP  DE
    JP   Z, .file_e_found
    JP   .find_file_e

.file_out_next:
    LD   A, (CCP_RAM.ccp_bdos_result_code)
    ; calc address of DIR entry in DMA buffer
    ; A[6:5] = A[1:0] = 32*A
    RRCA                                            ; [C] -> [7:0] -> [C]
    RRCA                                            ;
    RRCA                                            ;
    AND  01100000b                                  ; mask
    LD   C, A
    PUSH BC
    CALL CCP_RAM.ccp_out_crlf                       ; start new line
    CALL CCP_RAM.ccp_bdos_drv_get
    INC  A
    LD   (dma_buffer), A                            ; disk
    POP  BC
    LD   B, 0x0
    LD   HL, dma_buffer+FCB_FN                      ; filename
    LD   E, L
    LD   D, H
    ADD  HL, BC

    ; copy filename to tmp FCB and out to screen
    LD   B, 0x1
.copy_next:
    LD   A, (HL)
    LD   (DE), A
    LD   C, A
    CALL BIOS.conout_f
    INC  HL
    INC  DE
    INC  B
    LD   A, B
    CP   FN_LEN                                     ; >12 end of name
    JP   C, .copy_next

.zero_up_36:
    XOR  A
    LD   (DE), A                                    ; zero at end
    INC  B
    LD   A, B
    CP   36
    JP   C, .zero_up_36

    ; calc file size for current entry
    LD   DE, dma_buffer
    CALL cpp_bdos_f_size
    LD   HL, (fcb_ra_record_num)                    ; file size in blocks

    ; get disk blk size
    LD   A, (CPM_VARS.bdos_curdsk)
    OR   A
    JP   NZ, .no_dsk_a0
    LD   B, 3                                       ; for A - blk=3
    JP   .dsk_a0

.no_dsk_a0:
    LD   B, 4                                       ; for other disks - blk=4
.dsk_a0:
    LD   C, L

    ; convert 128b OS block to disk blocks
.mul_to_dsk_blk:
    XOR  A
    LD   A, H
    RRA
    LD   H, A
    LD   A, L
    RRA
    LD   L, A
    DEC  B
    JP   NZ, .mul_to_dsk_blk
    ; round up
    LD   A, (CPM_VARS.bdos_curdsk)
    OR   A
    JP   NZ, .no_dsk_a1
    LD   A, 00000111b                               ; for A - ~(~0 << 3)
    JP   .ds_skip1
.no_dsk_a1:
    LD   A, 00001111b                               ; for other dsk - ~(~0 << 4)
.ds_skip1:
    AND  C
    JP   Z, .cvt_blk_kb
    INC  HL

    ; Convert blocks to kilobytes   (A-1k B-2k)
.cvt_blk_kb:
    LD   A, (CPM_VARS.bdos_curdsk)
    OR   A
    JP   Z, .ds_skip2
    ADD  HL, HL         ; 2k

    ; add file size to total dir size
.ds_skip2:
    EX   DE, HL
    LD   HL, (CPM_VARS.tmp_dir_total)
    ADD  HL, DE
    LD   (CPM_VARS.tmp_dir_total), HL

    ; display size in K
    LD   C, ' '
    CALL BIOS.conout_f
    CALL BIOS.conout_f
    CALL ccp_cout_num
    LD   C, 'K'
    CALL BIOS.conout_f
    CALL CCP_RAM.ccp_getkey_no_wait
    JP   NZ, CCP_RAM.ccp_cmdline_back
    POP  DE
    INC  E
    JP   .do_next_direntry

.file_e_found:
    POP  DE
    LD   HL, msg_free_space                          ;= "\r\nFREE SPACE   "

    ; Out: FREE SPACE
    CALL ccp_out_str_z
    LD   A, (CPM_VARS.bdos_curdsk)
    OR   A
    JP   NZ, .no_ram_dsk
    LD   HL, (BIOS.disk_a_size)
    JP   .calc_remanis_ds

.no_ram_dsk:
    DEC  A
    LD   HL, (BIOS.disk_b_size)
    JP   Z, .calc_remanis_ds

    LD   HL, (BIOS.disk_c_size)
    LD   A, (bios_var3)
    OR   A
    JP   NZ, .calc_remanis_ds

    LD   A, H
    CP   1
    JP   Z, .d720
    LD   HL, 360
    JP   .calc_remanis_ds
.d720:
    LD   HL, 720

    ; Disk size - Dir size = Free
.calc_remanis_ds:
    EX   DE, HL
    LD   HL, (CPM_VARS.tmp_dir_total)
    LD   A, E
    SUB  L
    LD   E, A
    LD   A, D
    SBC  A, H
    LD   D, A
    CALL ccp_cout_num
    LD   C, 'K'
    CALL BIOS.conout_f
    CALL CCP_RAM.ccp_out_crlf
    JP   CCP_RAM.ccp_cmdline_back

msg_free_space:
    DB   "\r\nFREE SPACE   ",0

ccp_cout_num:
    LD   A, D
    AND  11100000b
    JP   Z, .less_224
    LD   C, '*'
    CALL BIOS.conout_f
    CALL BIOS.conout_f
    CALL BIOS.conout_f
    CALL BIOS.conout_f
    RET

.less_224:
    LD   HL, 0x0
    ; copy number to BC
    LD   B, D
    LD   C, E
    LD   DE, 0x1
    LD   A, 13

.bc_rra:
    PUSH AF
    PUSH HL
    ; BC >> 1
    LD   A, B
    RRA
    LD   B, A
    LD   A, C
    RRA
    LD   C, A
    JP   NC, .bc_rra_ex
    POP  HL
    CALL cpp_daa16
    PUSH HL

.bc_rra_ex:
    LD   L, E
    LD   H, D
    CALL cpp_daa16
    EX   DE, HL
    POP  HL
    POP  AF
    DEC  A
    JP   NZ, .bc_rra
    LD   D, 4
    LD   B, 0

.next_d:
    LD   E, 4

.next_e:
    LD   A, L
    RLA
    LD   L, A
    LD   A, H
    RLA
    LD   H, A
    LD   A, C
    RLA
    LD   C, A
    DEC  E
    JP   NZ, .next_e
    LD   A, C
    AND  0xf
    ADD  A, '0'
    CP   '0'
    JP   NZ, .no_zero
    DEC  B
    INC  B
    JP   NZ, .b_no_one
    LD   A, D
    DEC  A
    JP   Z, .d_one
    LD   A, ' '
    JP   .b_no_one

.d_one:
    LD   A, '0'
.no_zero:
    LD   B, 0x1
.b_no_one:
    LD   C, A
    CALL    BIOS.conout_f
    DEC  D
    JP   NZ, .next_d
    RET

; -------------------------------------------------------
; ADD with correction HL=HL+DE
; -------------------------------------------------------
cpp_daa16:
    LD   A, L
    ADD  A, E
    DAA
    LD   L, A
    LD   A, H
    ADC  A, D
    DAA
    LD   H, A
    RET

; -------------------------------------------------------
; Call BDOS function 35 (F_SIZE) - Compute file size
; -------------------------------------------------------
cpp_bdos_f_size:
    LD   C, F_SIZE
    JP   jp_bdos_enter

; -------------------------------------------------------
; Read Intel HEX data from serial port
; -------------------------------------------------------
ccp_read:
    LD   DE, msg_read_hex
    CALL out_dollar_str
    LD   HL, 0x0
    LD   (CCP_RAM.hex_length), HL

    ; Wait for start of Intel HEX line
.wait_colon:
    CALL MON.mon_serial_in
    CP   ':'
    JP   NZ, .wait_colon

    ; Init checksum
    XOR  A
    LD   D, A

    CALL ser_read_hexb                             ; read byte_count
    JP   Z, .end_of_file
    LD   E, A
    CALL ser_read_hexb                             ; read address hi
    LD   H, A
    CALL ser_read_hexb                             ; read address lo
    LD   L, A                                       ; HL - dst address
    CALL ser_read_hexb                             ; read rec type

    ; calculate length += byte_count
    PUSH HL
    LD   HL, (CCP_RAM.hex_length)
    LD   A, L
    ADD  A, E
    LD   L, A
    LD   A, H
    ADC  A, 0
    LD   H, A
    LD   (CCP_RAM.hex_length), HL
    POP  HL

    LD   C, E

    ; receive next E=byte_count bytes
.receive_rec:
    CALL ser_read_hexb
    LD   (HL), A
    INC  HL
    DEC  E
    JP   NZ, .receive_rec

    CALL ser_read_hexb                             ; receive checksum
    JP   NZ, .load_error                              ; jump if error
    JP   .wait_colon                                ; jump to wait next line

.end_of_file:
    ; read tail 4 bytes: 00 00 01 ff
    CALL ser_read_hexb
    CALL ser_read_hexb
    CALL ser_read_hexb
    CALL ser_read_hexb
    JP   Z, .load_complete

.load_error:
    LD   DE, .msg_error
    JP   out_dollar_str

.load_complete:
    ; Out message with length of received file
    LD   HL, (CCP_RAM.hex_length)
    LD   A, H
    CALL MON.mon_hexb
    LD   A, L
    CALL MON.mon_hexb
    LD   DE, .msg_bytes
    CALL out_dollar_str
    ; Calculate number of pages
    LD   HL, (CCP_RAM.hex_length)
    LD   A, L
    ADD  A, 0xff
    LD   A, H
    ADC  A, 0x0
    RLA
    LD   (CCP_RAM.hex_sectors), A
    ; Out message with number of pages
    CALL MON.mon_hexb
    LD   DE, .msg_pages
    CALL out_dollar_str

    ; Check for file name specified in cmd line
    CALL CCP_RAM.ccp_cv_first_to_fcb
    CALL CCP_RAM.ccp_drive_sel
    LD   A, (CCP_RAM.ccp_current_fcb_fn)
    CP   ASCII_SP
    JP   Z, .warm_boot

    ; Create file
    LD   DE, CCP_RAM.ccp_current_fcb
    LD   C, F_MAKE
    CALL jp_bdos_enter
    INC  A
    JP   Z, .load_error
    LD   HL, tpa_start
    LD   (CCP_RAM.hex_buff), HL

.wr_sector:
    ; set source buffer address
    LD   HL, (CCP_RAM.hex_buff)
    EX   DE, HL
    LD   C, F_DMAOFF
    CALL jp_bdos_enter
    ; write source buffer to disk
    LD   DE, CCP_RAM.ccp_current_fcb
    LD   C, F_WRITE
    CALL jp_bdos_enter
    ; check errors
    OR   A
    JP   NZ, .load_error

    ; rewind forward to next sector
    LD   HL, (CCP_RAM.hex_buff)
    LD   DE, 128                                    ; sector size
    ADD  HL, DE
    LD   (CCP_RAM.hex_buff), HL

    ; decrement sector count
    LD   A, (CCP_RAM.hex_sectors)
    DEC  A
    LD   (CCP_RAM.hex_sectors), A
    JP   NZ, .wr_sector                             ; jump if remains sectors

    ; close file
    LD   DE, CCP_RAM.ccp_current_fcb
    LD   C, F_CLOSE
    CALL jp_bdos_enter
    ; check errors
    CP   0xff
    JP   Z, .load_error

.warm_boot:
    LD   C, P_TERMCPM
    JP   jp_bdos_enter

.msg_bytes:
    DB  "h bytes ($"

.msg_pages:
    DB  " pages)\n\r$"

.msg_error:
    DB  "error!\n\r$"

; ---------------------------------------------------
; Read next two symbols from serial and convert to
; byte
; Out: A - byte
;      CF set if error
; ---------------------------------------------------
ser_read_hexb:
    PUSH BC
    CALL MON.mon_serial_in
    CALL hex_to_nibble
    RLCA
    RLCA
    RLCA
    RLCA
    LD   C, A
    CALL MON.mon_serial_in
    CALL hex_to_nibble
    OR   C
    LD   C, A
    ADD  A, D
    LD   D, A
    LD   A, C
    POP  BC
    RET

; ---------------------------------------------------
; Convert hex symbol to byte
; Inp: A - '0'..'F'
; Out: A - 0..15
;      CF set if error
; ---------------------------------------------------
hex_to_nibble:
    SUB  '0'                                         ; < '0' - error
    RET  C
    ADD  A, 233                                      ; F -> 255
    RET  C                                           ; > F - error
    ADD  A, 6
    JP   P, .l1
    ADD  A, 7
    RET  C
.l1:
    ADD  A, 10
    OR   A
    RET

; ---------------------------------------------------
; Out $ ended string
; Inp: DE -> string$
; ---------------------------------------------------
out_dollar_str:
    LD   C, C_WRITESTR
    JP   jp_bdos_enter

msg_read_hex:
    DB   "\n\rRead HEX from RS232... $"

filler1:
    DS   62,  0

; -------------------------------------------------------
; Out zerro ended string
; In: HL -> strZ
; -------------------------------------------------------
ccp_out_str_z:
    LD   A, (HL)
    OR   A
    RET  Z
    LD   C, A
    CALL BIOS.conout_f
    INC  HL
    JP   ccp_out_str_z

; -------------------------------------------------------
; Delete file and out No Space message
; -------------------------------------------------------
ccp_del_f_no_space:
    LD   DE, CCP_RAM.ccp_current_fcb
    CALL CCP_RAM.ccp_bdos_era_file
    JP   CCP_RAM.ccp_no_space

; -------------------------------------------------------
; Read current file next block
; Out: A=0 - Ok, 0xFF - HW Error;
; -------------------------------------------------------
cpp_read_f_blk:
    LD   DE, CPM_VARS.ccp_fcb                       ; FCB here
    JP   CCP_RAM.ccp_bdos_read_f

ccp_write:
    CALL CCP_RAM.ccp_cv_first_to_fcb
    CALL CCP_RAM.ccp_drive_sel
    LD   HL, CCP_RAM.ccp_current_fcb_fn
    LD   A, (HL)
    CP   ' '
    JP   NZ, .find_f
    LD   B, 11

.fill_with_wc:
    LD   (HL), '?'
    INC  HL
    DEC  B
    JP   NZ, .fill_with_wc

.find_f:
    CALL CCP_RAM.ccp_find_first
    JP   NZ, .found_f
    CALL CCP_RAM.ccp_out_no_file
    JP   CCP_RAM.ccp_cmdline_back

.found_f:
    LD   E, 0            ; file counter

.do_next_f1:
    PUSH DE
    CALL CCP_RAM.ccp_find_first
    POP  DE
    PUSH DE

.do_next_f2:
    DEC  E
    JP   M, .do_file
    PUSH DE
    CALL CCP_RAM.ccp_bdos_find_next
    POP  DE
    JP   Z, .no_more_f
    JP   .do_next_f2

.do_file:
    POP  BC
    PUSH BC
    LD   A, C
    OR   A
    JP   Z, .calc_addr
    LD   DE, 1200

    ; Delay with key interrupt check
.delay_1:
    XOR  A
.delay_2:
    DEC  A
    JP   NZ, .delay_2
    PUSH DE
    CALL CCP_RAM.ccp_getkey_no_wait
    POP  DE
    JP   NZ, CCP_RAM.ccp_cmdline_back
    DEC  DE
    LD   A, D
    OR   E
    JP   NZ, .delay_1

.calc_addr:
    LD   A, (CCP_RAM.ccp_bdos_result_code)
    ; A=0-3 - for Ok, calc address of DIR entry in DMA buffer
    RRCA
    RRCA
    RRCA
    AND  01100000b
    LD   C, A
    PUSH BC
    CALL CCP_RAM.ccp_out_crlf
    CALL CCP_RAM.ccp_bdos_drv_get
    INC  A
    LD   (CPM_VARS.ccp_fcb_dr), A                   ; set drive number
    POP  BC
    LD   B, 0x0
    LD   HL, dma_buffer+1
    ADD  HL, BC
    LD   DE, CPM_VARS.ccp_fcb_fn
    LD   B, 0x1

.copy_fn:
    LD   A, (HL)
    LD   (DE), A
    INC  HL
    INC  DE
    INC  B
    LD   A, B
    CP   12
    JP   C, .copy_fn

.fillz_fn:
    XOR  A
    LD   (DE), A
    INC  DE
    INC  B
    LD   A, B
    CP   36
    JP   C, .fillz_fn
    LD   HL, CPM_VARS.ccp_fcb_fn
    CALL ccp_out_str_z
    LD   HL, dma_buffer

    ; Empty first 128 bytes of DMA buffer
    LD   B, 128
.clear_buf:
    LD   (HL), 0x0
    INC  HL
    DEC  B
    JP   NZ, .clear_buf

    ; Copy file name at buffer start
    LD   HL, dma_buffer
    LD   DE, CPM_VARS.ccp_fcb_fn
    LD   B, 8

.find_sp:
    LD   A, (DE)
    CP   ' '
    JP   Z, .sp_rep_dot                             ; ' ' -> '.'
    LD   (HL), A
    INC  HL
    INC  DE
    JP   .find_sp
.sp_rep_dot:
    LD   (HL), '.'
    INC  HL
    CALL CCP_RAM.ccp_find_no_space

.cont_copy_fn:
    LD   A, (DE)
    LD   (HL), A
    OR   A
    JP   Z, .end_copy
    INC  HL
    INC  DE
    JP   .cont_copy_fn

.end_copy:
    LD   DE, CPM_VARS.ccp_fcb
    CALL CCP_RAM.ccp_bdos_open_f
    LD   DE, 0x8000                                 ; Block ID
    LD   HL, dma_buffer
    CALL cpp_pause_tape_wr_blk
    CALL cpp_pause_tape_wr_blk
    LD   DE, 0x1

    ; Read file block and write to Tape
.read_f_write_t:
    PUSH DE
    CALL cpp_read_f_blk
    ; a=0xff if error; a=1 - EOF
    DEC  A
    JP   Z, .eof
    LD   A, (CPM_VARS.ccp_fcb_cr)
    AND  0x7f
    CP   0x1
    JP   NZ, .write_once
    ; Write block to Tape with ID=0 twice
    LD   DE, 0x0         ; Block ID=0
    LD   HL, dma_buffer
    CALL cpp_pause_tape_wr_blk

.write_once:
    CALL CCP_RAM.ccp_getkey_no_wait
    LD   HL, dma_buffer
    POP  DE
    JP   NZ, CCP_RAM.ccp_cmdline_back
    CALL cpp_pause_tape_wr_blk
    ; Inc Block ID and continue
    INC  DE
    JP   .read_f_write_t

.eof:
    POP  DE
    EX   DE, HL
    LD   (dma_buffer), HL
    EX   DE, HL

    ; Final block ID=0xFFFF
    LD   DE, 0xffff
    ; Write twice
    CALL cpp_pause_tape_wr_blk
    CALL cpp_pause_tape_wr_blk
    POP  DE
    INC  E
    JP   .do_next_f1

.no_more_f:
    POP  DE
    CALL CCP_RAM.ccp_out_crlf
    JP   CCP_RAM.ccp_cmdline_back

; -------------------------------------------------------
; Write block to tape after pause
; -------------------------------------------------------
cpp_pause_tape_wr_blk:
    LD   BC, 3036
.delay:
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, .delay
    JP   BIOS.tape_write_f

    DB 0x4c

; -------------------------------------------------------
; Filler to align blocks in ROM
; -------------------------------------------------------
LAST        EQU     $
CODE_SIZE   EQU     LAST-0xDB00
FILL_SIZE   EQU     0x500-CODE_SIZE

    DISPLAY "| CCP_ROM\t| ",/H,ccp_entry,"  | ",/H,CODE_SIZE," | ",/H,FILL_SIZE," |"

    ; Check integrity
    ASSERT ccp_dir = 0xdb62
    ASSERT ccp_dir.find_file_e = 0xdb97
    ASSERT ccp_dir.file_out_next = 0xdba6
    ASSERT ccp_dir.file_e_found = 0xdc3e
    ASSERT ccp_dir.calc_remanis_ds = 0xdc72
    ASSERT ccp_dir.no_ram_dsk = 0xdc52
    ASSERT msg_free_space = 0xdc8a
    ASSERT ccp_cout_num = 0xdc9a

FILLER
    DS   FILL_SIZE-1, 0xff
    DB   0xaa

    ENDMODULE

    IFNDEF  BUILD_ROM
        OUTEND
    ENDIF
