; ======================================================
; Ocean-240.2
; CP/M V2.2 R8.
; CCP (Console Command Processor) Resident part
; ORG C000 at RPM, moved to B200-BA09
;
; Disassembled by Romych 2026-02-01
; ======================================================

	INCLUDE "equates.inc"
	INCLUDE "ram.inc"

	IFNDEF	BUILD_ROM
		OUTPUT ccp_ram.bin
	ENDIF

	MODULE	CCP_RAM

	ORG	 0xB200

ccp_ram_ent:
    JP	 ccp_ent
    JP	 ccp_clear_buff

ccp_inbuff:
    DB	 0x7F, 0x00

ccp_inp_line:
    DB	 ASCII_SP, ASCII_SP

msg_copyright:
    DB   "              COPYRIGHT (C) 1979, DIGITAL RESEARCH  ", 0x00
    DS   73, 0x0

ccp_inp_line_addr:
    DW	 ccp_inp_line

cur_name_ptr:
    DW	 0h

; ---------------------------------------------------
; Call BDOS function 2 (C_WRITE) - Console output
; Inp: A - char to output
; ---------------------------------------------------
ccp_print:
    LD	 E, A
    LD	 C, 2
    JP	 jp_bdos_enter

; ---------------------------------------------------
; Put char to console
; Inp: A - char
; ---------------------------------------------------
ccp_putc:
    PUSH BC
    CALL ccp_print
    POP	 BC
    RET

ccp_out_crlf:
    LD	 A, ASCII_CR
    CALL ccp_putc
    LD	 A, ASCII_LF
    JP	 ccp_putc
ccp_out_space:
    LD	 A,' '
    JP	 ccp_putc

; ---------------------------------------------------
; Out message from new line
; Inp: BC -> Message
; ---------------------------------------------------
ccp_out_crlf_msg:
    PUSH BC
    CALL ccp_out_crlf
    POP	 HL

; ---------------------------------------------------
; Out asciiz message
; Inp: HL -> Message
; ---------------------------------------------------
ccp_out_msg:
    LD	 A, (HL)										;= "READ ERROR"
    OR	 A
    RET	 Z
    INC	 HL
    PUSH HL
    CALL ccp_print
    POP	 HL
    JP	 ccp_out_msg

; ---------------------------------------------------
; Call BDOS function 13 (DRV_ALLRESET) - Reset discs
; ---------------------------------------------------
ccp_bdos_drv_allreset:
    LD	 C,13
    JP	 jp_bdos_enter

; ---------------------------------------------------
; Call BDOS function 14 (DRV_SET) - Select disc
; Inp: A - disk
; ---------------------------------------------------
ccp_bdos_drv_set:
    LD	 E, A
    LD	 C, 14
    JP	 jp_bdos_enter

; ---------------------------------------------------
; Call BDOS fn and return result
; Inp: C - fn no
; Out: A - error + 1
; ---------------------------------------------------
ccp_call_bdos:
    CALL jp_bdos_enter
    LD	 (ccp_bdos_result_code), A
    INC	 A
    RET

; ---------------------------------------------------
; BDOS function 15 (F_OPEN) - Open file /Dir
; Inp: DE -> FCB
; Out: A=0 for error, or 1-4 for success
; ---------------------------------------------------
ccp_bdos_open_f:
    LD	 C, 15
    JP	 ccp_call_bdos

; ---------------------------------------------------
; Open file by current FCB
; ---------------------------------------------------
ccp_open_cur_fcb:
    XOR	 A
    LD	 (ccp_current_fcb_cr), A                    ; clear current record counter
    LD	 DE, ccp_current_fcb
    JP	 ccp_bdos_open_f

; ---------------------------------------------------
; BDOS function 16 (F_CLOSE) - Close file
; ---------------------------------------------------
ccp_bdos_close_f:
    LD	 C, 16
    JP	 ccp_call_bdos

; ---------------------------------------------------
; Call BDOS function 17 (F_SFIRST) - search for first
; Out: A = 0 in error, 1-4 if success
; ---------------------------------------------------
ccp_bdos_find_first:
    LD	 C, 17
    JP	 ccp_call_bdos

; ---------------------------------------------------
; Call BDOS function 18 (F_SNEXT) - search for next
; Out: A = 0 in error, 1-4 if success
; ---------------------------------------------------
ccp_bdos_find_next:
    LD	 C, 18
    JP	 ccp_call_bdos		                        ; BDOS 18 (F_SNEXT) - search for next ?

; ---------------------------------------------------
; Call BDOS F_FIRST with current FCB
; ---------------------------------------------------
ccp_find_first:
    LD	 DE, ccp_current_fcb
    JP	 ccp_bdos_find_first

; ---------------------------------------------------
; Call BDOS function 19 (F_DELETE) - delete file
; ---------------------------------------------------
ccp_bdos_era_file:
    LD	C,19
    JP	jp_bdos_enter

; ---------------------------------------------------
; Call BDOS and set ZF by result
; ---------------------------------------------------
ccp_bdos_enter_zf:
    CALL jp_bdos_enter
    OR	 A
    RET

; ---------------------------------------------------
; Read next 128 bytes of file
; Inp: DE -> FCB
; Out: a = 0 - ok;
; 1 - EOF;
; 9 - invalid FCB;
; 10 - Media changed;
; 0xFF - HW error.
; ---------------------------------------------------
ccp_bdos_read_f:
    LD	 C, 20
    JP	 ccp_bdos_enter_zf

; ---------------------------------------------------
; Read file by current FCB
; ---------------------------------------------------
ccp_read_f_fcb:
    LD	 DE, ccp_current_fcb
    JP	 ccp_bdos_read_f

; ---------------------------------------------------
; Call BDOS function 21 (F_WRITE) - write next record
; ---------------------------------------------------
ccp_bdos_f_write:
    LD	 C, 21
    JP	 ccp_bdos_enter_zf

; ---------------------------------------------------
; Call BDOS function 22 (F_MAKE) - create file
; ---------------------------------------------------
ccp_bdos_make_f:
    LD	 C, 22
    JP	 ccp_call_bdos

; ---------------------------------------------------
; Call BDOS function 23 (F_RENAME) - Rename file
; ---------------------------------------------------
ccp_bdos_rename_f:
    LD	C, 23
    JP	jp_bdos_enter

; ---------------------------------------------------
; Call BDOS function 32 (F_USERNUM) for get user number
; Out: A - user no
; ---------------------------------------------------
ccp_bdos_get_user:
    LD	E, 0xff

; ---------------------------------------------------
; Call BDOS function 32 (F_USERNUM) set user number
; Inp: E - user no 0-15, or 0xFF for get user
; Out: A - user no
; ---------------------------------------------------
ccp_bdos_set_user:
    LD	 C, 32
    JP	 jp_bdos_enter

; ---------------------------------------------------
; Get user no and store in upper nibble of sys var
; ---------------------------------------------------
ccp_set_cur_drv:
    CALL	ccp_bdos_get_user
    ADD	 A, A
    ADD	 A, A
    ADD	 A, A
    ADD	 A, A                                       ; user no at upper nibble
    LD	 HL, ccp_cur_drive
    OR	 (HL)
    LD	 (cur_user_drv), A
    RET

; ---------------------------------------------------
; Replace new drive in lower nibble of sys var
; reset user no
; Out: A - drive no
; ---------------------------------------------------
ccp_reset_cur_drv:
    LD	 A, (ccp_cur_drive)
    LD	 (cur_user_drv), A
    RET

; ---------------------------------------------------
; Uppercase character a..z
; Inp: A - character
; Out: A - character in upper case
; ---------------------------------------------------
char_to_upper:
    CP	 'a'
    RET	 C
    CP	 '{'
    RET	 NC
    ; character in a..z range
    AND	 0x5f
    RET

; ---------------------------------------------------
; Get user input from console or from batch
; file $$$.SUB
; ---------------------------------------------------
ccp_get_inp:
    LD	 A, (ccp_batch)
    OR	 A
    JP	 Z, ccp_gin_no_batch

    ; select drive A for $$$.SUB
    LD	 A, (ccp_cur_drive)
    OR	 A
    LD	 A, 0x0
    CALL NZ, ccp_bdos_drv_set

    ; open batch file
    LD	 DE, ccp_batch_fcb
    CALL ccp_bdos_open_f
    JP	 Z, ccp_gin_no_batch                        ; go to con inp if file not found

    ; read last record
    LD	 A, (ccp_batch_fcb_rc)
    DEC	 A
    LD	 (ccp_batch_fcb_cr), A
    LD	 DE, ccp_batch_fcb
    CALL ccp_bdos_read_f
    JP	 NZ, ccp_gin_no_batch                       ; stop on EOF

    ; move data from file to buffer
    LD	 DE, ccp_inbuff+1
    LD	 HL, dma_buffer
    LD	 B, DMA_BUFF_SIZE                           ; 0x80
    CALL ccp_mv_hlde_b
    LD	 HL, ccp_batch_fcb_s2
    LD	 (HL), 0x0
    INC	 HL
    DEC	 (HL)                                       ; decriment record count
    ; close batch file
    LD	 DE, ccp_batch_fcb
    CALL ccp_bdos_close_f
    JP	 Z, ccp_gin_no_batch
    ; reselect old drive if not 0
    LD	 A, (ccp_cur_drive)
    OR	 A
    CALL NZ, ccp_bdos_drv_set
    ; print command line
    LD	 HL, ccp_inp_line		                    ; ccp_inbuff+2
    CALL ccp_out_msg
    CALL ccp_getkey_no_wait
    JP	 Z, ccp_gin_nokey
    ; terminate batch processing on any key
    CALL ccp_del_batch
    JP	 ccp_get_command

    ; get user input from keyboard
ccp_gin_no_batch:
    CALL ccp_del_batch
    CALL ccp_set_cur_drv

    LD	 C, 10
    LD	 DE, ccp_inbuff
    ; Call BDOS C_READSTR DE -> inp buffer
    CALL jp_bdos_enter

    CALL ccp_reset_cur_drv

ccp_gin_nokey:
    LD	 HL, ccp_inbuff+1
    LD	 B, (HL)

ccp_gin_uppr:
    INC	 HL
    LD	 A, B
    OR	 A
    JP	 Z, ccp_gin_uppr_end
    LD	 A, (HL)		                            ;= 2020h
    CALL char_to_upper
    LD	 (HL), A		                            ;= 2020h
    DEC	 B
    JP	 ccp_gin_uppr

ccp_gin_uppr_end:
    LD	 (HL), A                                    ; set last character to 0
    LD	 HL, ccp_inp_line		                    ;
    LD	 (ccp_inp_line_addr), HL		                ;
    RET

; ---------------------------------------------------
; Check keyboard
; Out: A - pressed key code
;      ZF set if no key pressed
; ---------------------------------------------------
ccp_getkey_no_wait:
    LD	 C,11
	; Call BDOS (C_STAT) - Console status
    CALL jp_bdos_enter
    OR	 A
    RET	 Z				 ; ret if no character waiting
    LD	 C,1
	; Call BDOS (C_READ) - Console input
    CALL jp_bdos_enter
    OR	 A
    RET

; ---------------------------------------------------
; Call BDOS function 25 (DRV_GET) - Return current drive
; Out: A - drive 0-A, 1-B...
; ---------------------------------------------------
ccp_bdos_drv_get:
    LD	 C, 25
    JP	 jp_bdos_enter

; ---------------------------------------------------
; Set disk buffer address to default buffer
; ---------------------------------------------------
cpp_set_disk_buff_addr:
    LD	 DE, dma_buffer

; ---------------------------------------------------
; Call BDOS function 26 (F_DMAOFF) - Set DMA address
; Inp: DE - address
; ---------------------------------------------------
ccp_bdos_dma_set:
    LD	 C, 26
    JP	 jp_bdos_enter

; ---------------------------------------------------
; Delete batch file created by submit
; ---------------------------------------------------
ccp_del_batch:
    LD	 HL, ccp_batch
    LD	 A, (HL)
    OR	 A
    RET	 Z                                          ; return if no active batch file
    LD	 (HL), 0x0                                  ; mark as inactive
    XOR	 A
    CALL ccp_bdos_drv_set                           ; select drive 0
    LD	 DE, ccp_batch_fcb
    CALL ccp_bdos_era_file
    LD	 A, (ccp_cur_drive)
    JP	 ccp_bdos_drv_set

; --------------------------------------------------
; Check "serial number" of CP/M
; --------------------------------------------------
ccp_verify_pattern:
    LD	 DE, cpm_pattern	                        ; = F9h
    LD	 HL, 0x0000
    LD	 B, 6

ccp_chk_patt_nex:
    LD	 A, (DE)
    CP	 (HL)
    NOP                                             ; JP NZ HALT was here
    NOP
    NOP
    INC	 DE
    INC	 HL
    DEC	 B
    JP	 NZ, ccp_chk_patt_nex
    RET

; --------------------------------------------------
; Print syntax error indicator
; --------------------------------------------------
print_syn_err:
    CALL ccp_out_crlf
    LD	 HL, (cur_name_ptr)
pse_next:
    LD	 A, (HL)
    CP	 ASCII_SP
    JP	 Z, pse_end
    OR	 A
    JP	 Z, pse_end
    PUSH HL

    CALL ccp_print
    POP	 HL
    INC	 HL
    JP	 pse_next
pse_end:
    LD	 A, '?'
    CALL ccp_print
    CALL ccp_out_crlf
    CALL ccp_del_batch
    JP	 ccp_get_command

; --------------------------------------------------
; Check user input characters for legal range
; Inp: [DE] - pointer to character
; --------------------------------------------------
cpp_valid_inp:
    LD	 A, (DE)
    OR	 A
    RET	 Z
    CP	ASCII_SP                                    ; >= Space
    JP	 C, print_syn_err
    RET	 Z
    CP	 '='
    RET	 Z
    CP	 '_'
    RET	 Z
    CP	 '.'
    RET	 Z
    CP	 ':'
    RET	 Z
    CP	 ';'
    RET	 Z
    CP	 '<'
    RET	 Z
    CP	 '>'
    RET	 Z
    RET

; ---------------------------------------------------
; Find non space character until end
; Inp: DE -> current character
; Out: DE -> non space
;       A - character
;       ZF set on EOL
; ---------------------------------------------------
ccp_find_no_space:
    LD	 A, (DE)
    OR	 A
    RET	 Z
    CP	 ASCII_SP
    RET	 NZ
    INC	 DE
    JP	 ccp_find_no_space

; ---------------------------------------------------
; HL=HL+A
; ---------------------------------------------------
sum_hl_a:
    ADD	 A, L
    LD	 L, A
    RET	 NC
    INC	 H                                          ; inc H if CF is set
    RET

; ---------------------------------------------------
; Convert first name to fcb
; ---------------------------------------------------
ccp_cv_first_to_fcb:
    LD	 A, 0x0

; Convert filename from cmd to fcb
; replace '*' to '?'
; Inp: A - offset in fcb filename
; Out: A - count of '?' in file name
;      ZF is set for fegular file name
ccp_cv_fcb_filename:
    LD	 HL, ccp_current_fcb
    CALL sum_hl_a
    PUSH HL
    PUSH HL
    XOR	 A
    LD	 (ccp_chg_drive), A
    LD	 HL, (ccp_inp_line_addr)		            ; HL -> input line
    EX	 DE, HL
    CALL ccp_find_no_space                          ; get next non blank char
    EX	 DE, HL
    LD	 (cur_name_ptr), HL                         ; save name ptr
    EX	 DE, HL
    POP	 HL
    LD	 A, (DE)		                            ; load first name char
    OR	 A
    JP	 Z, cur_cvf_n_end
    SBC	 A, 'A'-1			                        ; 0x40 for drive letter
    LD	 B, A
    INC	 DE
    LD	 A, (DE)
    CP	 ':'			                            ; is ':' after drive letter?
    JP	 Z, cur_cvf_drv_ltr
    DEC	 DE                                         ; no, step back

cur_cvf_n_end:
    LD	 A, (ccp_cur_drive)
    LD	 (HL), A
    JP	 cur_cvf_basic_fn

cur_cvf_drv_ltr:
    LD	 A, B
    LD	 (ccp_chg_drive), A                         ; set change drive flag
    LD	 (HL), B
    INC	 DE

cur_cvf_basic_fn:
    LD	 B, 8                                       ; file name length

cur_cvf_chr_nxt:
    CALL cpp_valid_inp
    JP	 Z, cur_cvf_sp_remains
    INC	 HL
    CP	 '*'
    JP	 NZ, cur_cvf_no_star
    LD	 (HL), '?'
    JP	 cur_cvf_nxt1

cur_cvf_no_star:
    LD	 (HL), A
    INC	 DE

cur_cvf_nxt1:
    DEC	 B
    JP	 NZ, cur_cvf_chr_nxt

cur_cvf_nxt_delim:
    CALL cpp_valid_inp
    JP	 Z, cur_cvf_ext
    INC	 DE
    JP	 cur_cvf_nxt_delim

    ; fill remains with spaces
cur_cvf_sp_remains:
    INC	 HL
    LD	 (HL), ASCII_SP
    DEC	 B
    JP	 NZ, cur_cvf_sp_remains

    ; file name extension
cur_cvf_ext:
    LD	 B, 3
    CP	 '.'
    JP	 NZ, cur_cvf_ext_fill_sp
    INC	 DE
    ; handle current ext char
cur_cvf_ext_nxt:
    CALL cpp_valid_inp
    JP	 Z, cur_cvf_ext_fill_sp
    INC	 HL
    ; change * to ?
    CP	 '*'
    JP	 NZ, cur_cvf_no_star2
    LD	 (HL), '?'
    JP	 cur_cvf_nxt2
cur_cvf_no_star2:
    LD	 (HL), A
    INC	 DE
cur_cvf_nxt2:
    DEC	 B
    JP	 NZ, cur_cvf_ext_nxt

cur_cvf_ext_skip:
    CALL cpp_valid_inp
    JP	 Z, cur_cvf_rst_attrs
    INC	 DE
    JP	 cur_cvf_ext_skip

    ; skip remains ext pos with dpaces
cur_cvf_ext_fill_sp:
    INC	 HL
    LD	 (HL), ASCII_SP
    DEC	 B
    JP	 NZ, cur_cvf_ext_fill_sp

    ; set fcb extent, res1, res2 to 0
cur_cvf_rst_attrs:
    LD	 B, 3

cur_cvf_attrs_nxt:
    INC	 HL
    LD	 (HL), 0x0
    DEC	 B
    JP	 NZ, cur_cvf_attrs_nxt

    EX	 DE, HL
    LD	 (ccp_inp_line_addr), HL		            ; save input line pointer

    ; Check for ambigeous file name
    POP	 HL                                         ; -> file name in fcb
    LD	 BC, 0x000b                                 ; b=0, c=11
cur_cvf_ambig:
    INC	 HL
    LD	 A, (HL)
    CP	 '?'
    JP	 NZ, cur_cvf_ambig_nxt1
    INC	 B
cur_cvf_ambig_nxt1:
    DEC	 C
    JP	 NZ, cur_cvf_ambig
    LD	 A, B                                       ; a = count of '?'
    OR	 A                                          ; set ZF if regular filename
    RET

; ---------------------------------------------------
; CP/M command table
; ---------------------------------------------------
cpm_cmd_tab:
    DB	"DIR "
    DB  "ERA "
    DB  "TYPE"
    DB  "SAVE"
    DB  "REN "
    DB  "USER"

cpm_pattern:
    DB	0xF9, 0x16, 0, 0, 0, 0x6B                   ; CP/M serial number?
    ;LD   SP, HL
    ;LD   D, 0x00
    ;NOP
    ;NOP
    ;LD   L,E

; ---------------------------------------------------
; Search for CP/M command
; Out: A - command number
; ---------------------------------------------------
ccp_search_cmd:
    LD	 HL, cpm_cmd_tab
    LD	 C, 0x0

ccp_sc_cmd_nxt:
    LD	 A, C
    CP	 CCP_COMMAND_CNT                            ; 6
    RET	 NC
    LD	 DE, ccp_current_fcb_fn                     ; fcb filename
    LD	 B, CCP_COMMAND_LEN                         ; max cmd len
ccp_sc_chr_nxt:
    LD	 A, (DE)
    CP	 (HL)		  								; compate fcb fn and command table
    JP	 NZ, ccp_sc_no_match                        ; cmd not match
    INC	 DE
    INC	 HL
    DEC	 B
    JP	 NZ, ccp_sc_chr_nxt
    ; last can be space for 3-letter commands
    LD	 A, (DE)
    CP	 ASCII_SP
    JP	 NZ, ccp_sc_skip_cmd
    LD	 A, C                                       ; return command number in A
    RET

    ; skip remains
ccp_sc_no_match:
    INC	 HL
    DEC	 B
    JP	 NZ, ccp_sc_no_match
    ; go to next cmd
ccp_sc_skip_cmd:
    INC	 C
    JP	 ccp_sc_cmd_nxt

; --------------------------------------------------
; Clear command buffer and go to command processor
; --------------------------------------------------
ccp_clear_buff:
    XOR	 A
    LD	 (ccp_inbuff+1), A                          ; actual buffer len = 0

; --------------------------------------------------
; Entrance to CCP
; Inp: C - current user * 16
; --------------------------------------------------
@ccp_ent:
    LD	 SP, ccp_stack
    PUSH BC                                         ;
    LD	 A, C
    ; / 16
    RRA
    RRA
    RRA
    RRA
    ; cur user no in low nibble
    AND	 0x0f                                       ; user 0..15
    LD	 E, A
    CALL ccp_bdos_set_user
    CALL ccp_bdos_drv_allreset
    LD	 (ccp_batch), A
    POP	 BC
    LD	 A, C                                        ; a = user*16
    AND	 0xf                                         ; low nibble - drive
    LD	 (ccp_cur_drive), A
    CALL ccp_bdos_drv_set
    LD	 A, (ccp_inbuff+1)
    OR	 A
    JP	 NZ, ccp_process_cmd

; --------------------------------------------------
; Out prompt and get user command from console
; --------------------------------------------------
ccp_get_command:
    LD	 SP, ccp_stack                              ; reset stack pointer
    CALL ccp_out_crlf                               ; from new line
    CALL ccp_bdos_drv_get
    ADD	 A, 65                                      ; convert drive no to character
    CALL ccp_print                                  ; print current drive letter
    LD	 A, '>'                                     ; and prompt
    CALL ccp_print
    CALL ccp_get_inp                                ; and wait string

; --------------------------------------------------
; Process command
; --------------------------------------------------
ccp_process_cmd:
    LD	 DE, dma_buffer
    CALL ccp_bdos_dma_set                           ; setup buffer
    CALL ccp_bdos_drv_get
    LD	 (ccp_cur_drive), A                         ; store cur drive
    CALL ccp_cv_first_to_fcb                        ; convert first command parameter to fcb
    CALL NZ, print_syn_err                          ; if wildcard, out error message
    LD	 A, (ccp_chg_drive)                         ; check drive change flag
    OR	 A
    JP	 NZ, ccp_unk_cmd                            ; if drive changed, handle as unknown command
    CALL ccp_search_cmd                             ; ret A = command number
    LD	 HL, ccp_cmd_addr
    ; DE = command number
    LD	 E, A
    LD	 D, 0x0
    ADD	 HL, DE
    ADD	 HL, DE                                     ; HL = HL + 2*command_number
    LD	 A, (HL)
    INC	 HL
    LD	 H, (HL)
    LD	 L, A
    JP	 (HL)                                       ; jump to command handler

ccp_cmd_addr:
    DW   cmd_dir
    DW   cmd_erase
    DW   cmd_type
    DW   cmd_save
    DW   cmd_ren
    DW   cmd_user
    DW   ccp_entry

; ---------------------------------------------------
; di+halt if serial number validation failed
; ---------------------------------------------------
cpp_halt:
    LD   HL, 0x76f3
    LD	 (ccp_ram_ent), HL
    LD	 HL, ccp_ram_ent
    JP	 (HL)

ccp_type_rd_err:
    LD	 BC, .msg_read_error	  					; BC -> "READ ERROR"
    JP	 ccp_out_crlf_msg

.msg_read_error:
    DB	 "READ ERROR", 0

; ---------------------------------------------------
; Out message 'NO FILE'
; ---------------------------------------------------
ccp_out_no_file:
    LD	 BC, .msg_no_file	      					; BC -> "NO FILE"
    JP	 ccp_out_crlf_msg
.msg_no_file:
    DB	 "NO FILE", 0

; ---------------------------------------------------
; Decode a command in form: "A>filename number"
; Out: A - number
; ---------------------------------------------------
ccp_decode_num:
    CALL ccp_cv_first_to_fcb
    LD	 A, (ccp_chg_drive)
    OR	 A
    JP	 NZ, print_syn_err                          ; error if drive letter specified
    LD	 HL, ccp_current_fcb_fn
    LD	 BC, 11                                     ; b=0, c=11 (filename len)

    ; decode number
ccp_dff_nxt_num:
    LD	 A, (HL)
    CP	 ASCII_SP
    JP	 Z, ccp_dff_num_fin                         ; space - end of number
    ; check for digit
    INC	 HL
    SUB	 '0'
    CP	 10
    JP	 NC, print_syn_err                          ; not a digit
    LD	 D, A                                       ; d = number
    LD	 A, B
    AND	 0xe0                                       ; check B (sum) overflow
    JP	 NZ, print_syn_err
    ; A=B*10
    LD	 A, B
    RLCA
    RLCA
    RLCA                                            ; *8
    ADD	 A, B                                       ; *9
    JP	 C, print_syn_err                           ; error if overflow
    ADD	 A, B                                       ; * 10
    JP	 C, print_syn_err                           ; error if overflow
    ; B = B + B*10
    ADD	 A, D
    JP	 C, print_syn_err                           ; error if overflow
    LD	 B, A
    ; to next number
    DEC	 C
    JP	 NZ, ccp_dff_nxt_num
    RET

ccp_dff_num_fin:
    LD	 A, (HL)
    CP	 ASCII_SP
    JP	 NZ, print_syn_err                          ; will be space after number
    INC	 HL
    DEC	 C
    JP	 NZ, ccp_dff_num_fin
    LD	 A, B
    RET

; --------------------------------------------------
; Move 3 bytes from [HL] to [DE]
; (Used only to move file extension)
; --------------------------------------------------
ccp_mv_hlde_3:
    LD	 B, 3

; --------------------------------------------------
; Move B bytes from [HL] to [DE]
; --------------------------------------------------
ccp_mv_hlde_b:
    LD	 A, (HL)
    LD	 (DE), A
    INC	 HL
    INC	 DE
    DEC	 B
    JP	 NZ, ccp_mv_hlde_b
    RET

; --------------------------------------------------
; Return byte from address dma_buffer[A+C]
; Out: A - byte at dma_buffer[A+C]
;      HL - dma_buffer+A+C
; --------------------------------------------------
get_std_buff_ac:
    LD	 HL, dma_buffer
    ADD	 A, C
    CALL sum_hl_a
    LD	 A, (HL)
    RET

; --------------------------------------------------
; Check drive change and select if needed
; Reset drive byte in fcb to 0 (use default)
; --------------------------------------------------
ccp_drive_sel:
    XOR	 A
    LD	 (ccp_current_fcb), A
    LD	 A, (ccp_chg_drive)
    OR	 A
    RET	 Z                                          ; no need to change cur drive
    DEC	 A
    LD	 HL, ccp_cur_drive
    CP	 (HL)
    RET	 Z                                          ; current and new drive is same
    JP	 ccp_bdos_drv_set                           ; change

; --------------------------------------------------
; Restore previous drive if changed during operation
; --------------------------------------------------
ccp_restor_drv:
    LD	 A, (ccp_chg_drive)
    OR	 A
    RET	 Z                                          ; not changed
    DEC	 A
    LD	 HL, ccp_cur_drive                          ; chk cur drive
    CP	 (HL)
    RET	 Z                                          ; new and previous drive is same
    LD	 A, (ccp_cur_drive)
    JP	 ccp_bdos_drv_set                           ; restore to previous drive

; --------------------------------------------------
; Handle user DIR command
; --------------------------------------------------
cmd_dir:
    CALL ccp_cv_first_to_fcb
    CALL ccp_drive_sel
    ; check filemask specified
    LD	 HL, ccp_current_fcb_fn
    LD	 A, (HL)
    CP	 ASCII_SP
    JP	 NZ, .dir_fmask                             ; yes specified

    ; fill with wildcard symbol
    LD	 B, 11
.fill_wc:
    LD	 (HL), '?'
    INC	 HL
    DEC	 B
    JP	 NZ, .fill_wc

.dir_fmask:
    LD	 E, 0x0                                     ; cursor at 0
    PUSH DE
    ; find first file
    CALL ccp_find_first
    CALL Z, ccp_out_no_file                         ; no file found

.dir_f_next:
    JP	 Z, .dir_f_no_more
    LD	 A, (ccp_bdos_result_code)
    ; find filename pos in direntry
    ; a = a * 32
    RRCA
    RRCA
    RRCA
    AND	 0x60

    LD	 C, A
    LD	 A, 0x0a

    ; get std_buff[10+pos*8]
    CALL get_std_buff_ac
    RLA                                             ; CF<-[7:0]<-CF
    JP	 C, .dir_dont_lst                           ; don't display sys files
    POP	 DE
    LD	 A, E                                       ; a = cursor
    INC	 E
    PUSH DE                                         ; cursor++
    AND	 0x3
    PUSH AF
    JP	 NZ, .dir_no_eol
    ; eol, print new line
    CALL ccp_out_crlf
    ; print A:
    PUSH BC
    CALL ccp_bdos_drv_get
    POP	 BC
    ADD	 A, 'A'
    CALL ccp_putc
    LD	 A, ':'
    CALL ccp_putc
    JP	 .dir_out_sp
.dir_no_eol:
    ; add space between filenames
    CALL ccp_out_space
    LD	 A, ':'
    CALL ccp_putc
.dir_out_sp:
    CALL ccp_out_space
    LD	 B, 0x1
.dir_get_one:
    LD	 A, B
    CALL get_std_buff_ac
    AND	 0x7f                                       ; mask status bit
    CP	 ASCII_SP                                   ; name end?
    JP	 NZ, .no_name_end
    ; at end of file name
    POP	 AF
    PUSH AF
    CP	 0x3
    JP	 NZ, .dir_end_ext
    LD	 A, 0x9
    CALL get_std_buff_ac                            ; chk ext
    AND	 0x7f                                       ; 7bit
    CP	 ASCII_SP
    JP	 Z, .dir_skp_sp                             ; do not print space
.dir_end_ext:
    LD	 A, ASCII_SP
.no_name_end:
    CALL ccp_putc
    INC	 B
    LD	 A, B
    CP	 12
    JP	 NC, .dir_skp_sp                            ; print until end of file ext
    CP	 9
    JP	 NZ, .dir_get_one                           ; start of file ext?
    CALL ccp_out_space                              ; print sep space
    JP	 .dir_get_one
.dir_skp_sp:
    POP	 AF
.dir_dont_lst:
    ; stop if key pressed
    CALL ccp_getkey_no_wait
    JP	 NZ, .dir_f_no_more

    ; find next directory entry
    CALL ccp_bdos_find_next
    JP	 .dir_f_next

    ; no more to print
.dir_f_no_more:
    POP	 DE
    JP	 ccp_cmdline_back

; --------------------------------------------------
; Handle user ERA command
; --------------------------------------------------
cmd_erase:
    CALL ccp_cv_first_to_fcb
    ; check for *.*
    CP	 11
    JP	 NZ, .era_no_wc
    ; confirm erase all
    LD	 BC, msg_all_yn		                        ;= "ALL (Y/N)?"
    CALL ccp_out_crlf_msg
    CALL ccp_get_inp
    LD	 HL, ccp_inbuff+1
    ; check user input
    DEC	 (HL)
    JP	 NZ, ccp_get_command
    INC	 HL
    LD	 A, (HL)		                            ; user input, first letter
    CP	 'Y'
    JP	 NZ, ccp_get_command                        ; return in not exactly 'Y'
    INC	 HL
    LD	(ccp_inp_line_addr), HL

.era_no_wc:
    CALL ccp_drive_sel                              ; select drive
    LD	 DE, ccp_current_fcb                        ; specify current fcb
    CALL ccp_bdos_era_file                            ; and delete file
    INC	 A
    CALL Z, ccp_out_no_file
    JP	 ccp_cmdline_back                           ; go back to command line

msg_all_yn:
    DB	 "ALL (Y/N)?", 0

; --------------------------------------------------
; Handle user TYPE command
; --------------------------------------------------
cmd_type:
    CALL ccp_cv_first_to_fcb
    JP	 NZ, print_syn_err                          ; error if wildcard
    ; select drive and open
    CALL ccp_drive_sel
    CALL ccp_open_cur_fcb
    JP	 Z, .not_found                              ; cant open file
    CALL ccp_out_crlf
    LD	 HL, ccp_bytes_ctr
    LD	 (HL), 0xff                                 ; 255>128 for read first sector

.cont_or_read:
    LD	 HL, ccp_bytes_ctr
    LD	 A, (HL)
    CP	 128
    JP	 C, .out_next_char

    ; read 128 bytes
    PUSH HL
    CALL ccp_read_f_fcb
    POP	 HL
    JP	 NZ, .read_no_ok
    ; clear counter
    XOR	 A
    LD	 (HL), A

.out_next_char:
    ; get next byte from buffer
    INC	 (HL)
    LD	 HL, dma_buffer
    ; calc offset
    CALL sum_hl_a
    LD	 A, (HL)
    CP	 ASCII_SUB                                  ; Ctrl+Z end of text file
    JP	 Z, ccp_cmdline_back                        ; yes, back to cmd line
    CALL ccp_print                                  ; print char to output device
    ; interrupt if key pressed
    CALL ccp_getkey_no_wait
    JP	 NZ, ccp_cmdline_back
    ;
    JP	 .cont_or_read

    ; non zero result from f_read
.read_no_ok:
    DEC	 A
    JP	 Z, ccp_cmdline_back                        ; A=1 - EOF, return to cmd line
    CALL ccp_type_rd_err                            ; else read error

.not_found:
    CALL ccp_restor_drv
    JP	 print_syn_err

; --------------------------------------------------
; Handle user SAVE command
; --------------------------------------------------
cmd_save:
    CALL ccp_decode_num                             ; get num of pages
    PUSH AF                                         ; and store
    CALL ccp_cv_first_to_fcb                        ; conv filename to fcb
    JP	 NZ, print_syn_err                          ; error if wildcard
    ; delete specified file
    CALL ccp_drive_sel
    LD	 DE, ccp_current_fcb
    PUSH DE
    CALL ccp_bdos_era_file
    POP	 DE
    ; create specified file
    CALL ccp_bdos_make_f
    JP	 Z, ccp_no_space                            ; 0xff+1 if error
    XOR	 A
    LD	 (ccp_current_fcb_cr), A                ; curr record = 0
    POP	 AF                                         ; a = num pages
    LD	 L, A
    LD	 H, 0
    ADD	 HL, HL                                     ; HL = A * 2 - number of sectors
    LD	 DE, tpa_start

.write_next:
    ; all sectors written?
    LD 	 A, H
    OR	 L
    JP	 Z, ccp_close_f_cur                         ; no more sectors to write
    DEC	 HL
    PUSH HL
    ; set buffer address to memory to write
    LD	 HL, 128
    ADD	 HL, DE
    PUSH HL
    CALL ccp_bdos_dma_set
    ; and write sector
    LD	 DE, ccp_current_fcb
    CALL ccp_bdos_f_write
    POP	 DE
    POP	 HL
    JP	 NZ, ccp_no_space                           ; check for no space left
    JP	 .write_next

; Close current file
ccp_close_f_cur:
    LD	 DE, ccp_current_fcb
    CALL ccp_bdos_close_f
    INC	 A
    JP	 NZ, rest_buf_ret_cmd

; --------------------------------------------------
; Out error message about no space left
; --------------------------------------------------
ccp_no_space:
    LD	 BC, msg_no_space		                    ; BC -> "NO SPACE"
    CALL ccp_out_crlf_msg

rest_buf_ret_cmd:
    CALL cpp_set_disk_buff_addr
    JP	 ccp_cmdline_back

msg_no_space:
    DB	"NO SPACE", 0

; --------------------------------------------------
; Handle user REN command
; --------------------------------------------------
cmd_ren:
    CALL ccp_cv_first_to_fcb                        ; get first file name
    JP	 NZ, print_syn_err                          ; error if wildcard
    LD	 A, (ccp_chg_drive)                         ; remember drive change flag
    PUSH AF
    ; check file already exists
    CALL ccp_drive_sel
    CALL ccp_find_first
    JP	 NZ, .file_exists
    ; move filename to "second slot"
    LD	 HL, ccp_current_fcb
    LD	 DE, ccp_current_fcb_fn+15
    LD	 B, 16
    CALL ccp_mv_hlde_b
    ;
    LD	 HL, (ccp_inp_line_addr)		            ; restore cmd line pointer
    EX	 DE, HL
    CALL ccp_find_no_space                          ; skip spaces between parameters
    CP	 '='
    JP	 Z, .do_rename
    CP	 '_'
    JP	 NZ, .rename_err

.do_rename:
    EX	 DE, HL
    INC	 HL                                         ; skip sep
    LD	 (ccp_inp_line_addr), HL		            ; -> second param
    CALL ccp_cv_first_to_fcb                        ; get second name
    JP	 NZ, .rename_err                            ; error if wildcard
    ; if drive specified it will be same as previous
    POP	 AF
    LD	 B, A
    LD	 HL, ccp_chg_drive
    LD	 A, (HL)
    OR	 A
    JP	 Z, .same_drive                             ; ok, it is same
    CP	 B
    LD	 (HL), B                                    ; restore first drive
    JP	 NZ, .rename_err

.same_drive:
    LD	 (HL), B
    ; check for seacond file not exists
    XOR	 A
    LD	 (ccp_current_fcb), A
    CALL ccp_find_first
    JP	 Z, .second_exists
    ; calll bdos to rename
    LD	 DE, ccp_current_fcb
    CALL ccp_bdos_rename_f
    JP	 ccp_cmdline_back

.second_exists:
    CALL ccp_out_no_file
    JP	 ccp_cmdline_back

.rename_err:
    CALL ccp_restor_drv
    JP	 print_syn_err

.file_exists:
    LD	 BC, .msg_file_exists		                ; BC -> "FILE EXISTS"
    CALL ccp_out_crlf_msg
    JP	 ccp_cmdline_back

.msg_file_exists:
    DB	 "FILE EXISTS", 0

; --------------------------------------------------
; Handle user USER command
; --------------------------------------------------
cmd_user:
    CALL ccp_decode_num                             ; get user number
    ; user will be 0..15
    CP	 16
    JP	 NC, print_syn_err                          ; >15 - error
    LD	 E, A                                       ; save in E
    ; check for other parameters
    LD	 A, (ccp_current_fcb_fn)
    CP	 ASCII_SP
    JP	 Z, print_syn_err                           ; error if other parameters specified
    ; call bdos to set current user
    CALL ccp_bdos_set_user
    JP	 ccp_cmdline_back1

ccp_unk_cmd:
    CALL ccp_verify_pattern                         ; check if system valid
    ; check for file to execute specified
    LD	 A, (ccp_current_fcb_fn)
    CP	 ASCII_SP
    JP	 NZ, .exec_file
    ; drive change?
    LD	 A, (ccp_chg_drive)
    OR	 A
    JP	 Z, ccp_cmdline_back1                       ; no, return to cmd line
    ; change drive
    DEC	 A
    LD	 (ccp_cur_drive), A
    CALL ccp_reset_cur_drv
    CALL ccp_bdos_drv_set
    JP	 ccp_cmdline_back1

.exec_file:
    ; check file extension
    LD	 DE, ccp_current_fcb_ft
    LD	 A, (DE)
    CP	 ASCII_SP
    JP	 NZ, print_syn_err
    PUSH DE
    ; select specified drive
    CALL ccp_drive_sel
    POP	 DE
    ; set file ext to 'COM'
    LD	 HL, msg_com			                    ; HL -> 'COM'
    CALL ccp_mv_hlde_3
    CALL ccp_open_cur_fcb
    JP	 Z, cant_open_exe
    LD	 HL, tpa_start                              ; load to start of TPA 0x100

.read_next_sec:
    PUSH HL                                         ; store start pointer
    EX	 DE, HL
    CALL ccp_bdos_dma_set

    LD	 DE, ccp_current_fcb
    CALL ccp_bdos_read_f
    JP	 NZ, .read_no_ok                            ; check for read error

    ; shift start pointer for sector size
    POP	 HL
    LD	 DE, 128
    ADD	 HL, DE
    ; check for enough space in RAM
    LD	 DE, ccp_ram_ent
    LD	 A, L
    SUB	 E
    LD	 A, H
    SBC	 A, D
    JP	 NC, ccp_bad_load
    JP	 .read_next_sec

.read_no_ok:
    POP	 HL
    DEC	 A
    JP	NZ, ccp_bad_load                            ; it is not EOF, is error
    ; ok, EOF
    CALL ccp_restor_drv                             ; get first filename
    CALL ccp_cv_first_to_fcb
    LD	 HL, ccp_chg_drive                          ; hl -> buff[16]
    PUSH HL
    LD	A, (HL)
    LD	(ccp_current_fcb), A                        ; set drive letter in current fcb
    LD	A, 16
    CALL ccp_cv_fcb_filename                        ; replace wildcards
    POP	 HL
    ; set drive for second file in reserved fcb area
    LD	 A, (HL)
    LD	 (ccp_current_fcb_al), A
    ; clear record count
    XOR	 A
    LD	 (ccp_current_fcb_cr), A
    ; Move current to default FCB
    LD	 DE, fcb1
    LD	 HL, ccp_current_fcb
    LD	 B, 33
    CALL ccp_mv_hlde_b

    ; move remainder of cmd line to 0x0080
    LD	 HL, ccp_inp_line
.skip_nosp:
    LD	 A, (HL)
    OR	 A
    JP	 Z, .z_or_sp
    CP	 ASCII_SP
    JP	 Z, .z_or_sp
    INC	 HL
    JP	 .skip_nosp

.z_or_sp:
    LD	 B, 0                                       ; len of cmd line for program = 0
    LD	 DE, p_cmd_line                             ; destination address for cmd line

.copy_cmd_line:
    LD	 A, (HL)
    LD	 (DE), A
    OR	 A
    JP	 Z, .stor_len
    INC	 B
    INC	 HL
    INC	 DE
    JP	 .copy_cmd_line

.stor_len:
    LD	 A, B
    LD	 (p_cmd_line_len), A
    ; next line
    CALL ccp_out_crlf
    ; set buffer to cmd line
    CALL cpp_set_disk_buff_addr
    ; set drive
    CALL ccp_set_cur_drv
    ; and call loaded program
    CALL tpa_start
    ; restore stack first
    LD	 SP, ccp_stack
    ; restore current drive
    CALL ccp_reset_cur_drv
    CALL ccp_bdos_drv_set
    ; return back to command line mode
    JP	 ccp_get_command

cant_open_exe:
    CALL ccp_restor_drv
    JP	 print_syn_err

ccp_bad_load:
    LD	 BC, .msg_bad_load		                    ; BC -> "BAD LOAD"
    CALL ccp_out_crlf_msg
    JP	 ccp_cmdline_back

.msg_bad_load:
    DB	 "BAD LOAD", 0

msg_com:
    DB	 "COM"

; --------------------------------------------------
; Return back to command line
; --------------------------------------------------
ccp_cmdline_back:
    CALL ccp_restor_drv

ccp_cmdline_back1:
    CALL ccp_cv_first_to_fcb
    LD	 A, (ccp_current_fcb_fn)
    SUB	 0x20
    LD	 HL, ccp_chg_drive
    OR	 (HL)
    JP	 NZ, print_syn_err
    JP	 ccp_get_command

    DW	0h, 0h, 0h, 0h, 0h, 0h, 0h, 0h

ccp_stack EQU $

ccp_batch:
    DB	0h

ccp_batch_fcb:
    DB	0h                                          ; drive code, 0 - default
    DB  "$$$     SUB"                               ; filename
    DB  0h                                          ; extent
    DB  0h                                          ; S1
ccp_batch_fcb_s2:
    DB	0h                                          ; S2 Extent [6:0] bits and [7] write flag
ccp_batch_fcb_rc:
    DB  0h                                          ; sectors count
ccp_batch_fcb_al:
    DS 16, 0                                        ; reserved by CPM use only
ccp_batch_fcb_cr:
    DB	0h                                          ; current sector to read/write

ccp_current_fcb:
    DB	0h
ccp_current_fcb_fn:
    DS  8, 0
ccp_current_fcb_ft:
    DS  3, 0
ccp_current_fcb_ex:
    DB  0h                                          ; extent
    DB  0h                                          ; s1
    DB	0h                                          ; s2
    DB  0h                                          ; sectors count
ccp_current_fcb_al:
    DS  16, 0                                       ; reserved by CPM use only
ccp_current_fcb_cr:
    DB	0h                                          ; current sector to read/write

ccp_bdos_result_code:
    DB	0h

ccp_cur_drive:
    DB	0h

ccp_chg_drive:                                      ; change drive flag, 0 - no change
    DB 0
ccp_bytes_ctr:
    DW 0
    ; reserved
    DS 13, 0


bdos_enter_jump		EQU	$+6

; -------------------------------------------------------
; Filler to align blocks in ROM
; -------------------------------------------------------
LAST        EQU     $
CODE_SIZE   EQU     LAST-0xB200
;FILL_SIZE   EQU     0x500-CODE_SIZE

    DISPLAY "| CCP_RAM\t| ",/H,ccp_ram_ent,"  | ",/H,CODE_SIZE," | \t    |"

    ; Check integrity
    ASSERT ccp_cmdline_back = 0xb986

	ENDMODULE

	IFNDEF	BUILD_ROM
		OUTEND
	ENDIF