; =======================================================
; Ocean-240.2
; CP/M BDOS at 0xC800:D5FF
;
; Disassembled by Romych 2025-09-09
; ======================================================

    INCLUDE "io.inc"
    INCLUDE "equates.inc"
    INCLUDE "ram.inc"

    IFNDEF BUILD_ROM
        OUTPUT bdos.bin
    ENDIF

    MODULE BDOS

    ORG  0xc800

bdos_start:
    LD   SP, HL
    LD   D, 0x00
    NOP
    NOP
    LD   L, E

; --------------------------------------------------
; BDOS Entry point
; --------------------------------------------------
bdos_enter:
    JP   bdos_entrance

bdos_pere_addr:
    DW   bdos_persub                               ; permanent error
bdos_sele_addr:
    DW    bdos_selsub                               ; select error
bdos_rode_addr:
    DW    bdos_rodsub                               ; write to ro disk error
bdos_rofe_addr:
    DW    bdos_rofsub                               ; write to ro file error

; -------------------------------------------------------
; BDOS Handler
; Inp: C - func no
;      DE or E - parameter
; Out: A or HL - result
; -------------------------------------------------------
bdos_entrance:
    ; store parameter DE
    EX   DE, HL
    LD   (CPM_VARS.bdos_info), HL
    EX   DE, HL
    ; Store E
    LD   A, E
    LD   (CPM_VARS.bdos_linfo), A
    ; value to return, default = 0
    LD   HL, 0x00
    LD   (CPM_VARS.bdos_aret), HL

    ; Save user's stack pointer, set to local stack
    ADD  HL, SP
    LD   (CPM_VARS.bdos_usersp), HL

    LD   SP, CPM_VARS.bdos_stack                    ; local stack setup
    XOR  A
    LD   (CPM_VARS.bdos_fcbdsk), A                  ; fcbdsk,resel=false
    LD   (CPM_VARS.bdos_resel), A                   ; bdos_resel = FALSE
    LD   HL, bdos_goback
    PUSH HL                                         ; push goback address to return after jump
    LD   A, C
    CP   BDOS_NFUNCS
    RET  NC                                         ; return in func no out of range
    LD   C, E                                       ; store param E to C
    ; calculate offset in functab
    LD   HL, functab                                ; DE=func_no, HL->functab
    LD   E, A
    LD   D, 0
    ADD  HL, DE
    ADD  HL, DE
    LD   E, (HL)
    INC  HL
    LD   D, (HL)                                    ; DE=functab(func)
    LD   HL, (CPM_VARS.bdos_info)                   ; restore input parameter
    EX   DE, HL
    JP   (HL)                                       ; dispatch function

; -------------------------------------------------------
; BDOS function handlers address table
; -------------------------------------------------------
functab:
    DW   BIOS.wboot_f                                ; fn0
    DW   bdos_con_inp                                ; fn1
    DW   bdos_outcon                                 ; fn2
    DW   bdos_aux_inp                                ; fn3
    DW   BIOS.punch_f                                ; fn4
    DW   BIOS.list_f                                 ; fn5
    DW   bdos_dir_con_io                             ; fn6
    DW   bdos_get_io_byte                            ; fn7
    DW   bdos_set_io_byte                            ; fn8
    DW   bdos_print_str                              ; fn9
    DW   bdos_read                                   ; fn10
    DW   bdos_get_con_st                             ; fn11
    DW   bdos_get_version                            ; fn12
    DW   bdos_reset_disks                            ; fn13
    DW   bdos_select_disk                            ; fn14
    DW   bdos_open_file                              ; fn15
    DW   bdos_close_file                             ; fn16
    DW   bdos_search_first                           ; fn17
    DW   bdos_search_next                            ; fn18
    DW   bdos_rm_file                                ; fn19
    DW   bdos_read_file                              ; fn20
    DW   bdos_write_file                             ; fn21
    DW   bdos_make_file                              ; fn22
    DW   bdos_ren_file                               ; fn23
    DW   bdos_get_login_vec                          ; fn24
    DW   bdos_get_cur_drive                          ; fn25
    DW   bdos_set_dma_addr                           ; fn26
    DW   bdos_get_alloc_addr                         ; fn27
    DW   bdos_set_ro                                 ; fn28
    DW   bdos_get_wr_protect                         ; fn29
    DW   bdos_set_attr                               ; fn30
    DW   bdos_get_dpb                                ; fn31
    DW   bdos_set_user                               ; fn32
    DW   bdos_rand_read                              ; fn33
    DW   bdos_rand_write                             ; fn34
    DW   bdos_compute_fs                             ; fn35
    DW   bdos_set_random                             ; fn36
    DW   bdos_reset_drives                           ; fn37
    DW   bdos_not_impl                               ; fn38 (Access Drive)
    DW   bdos_not_impl                               ; fn39 (Free Drive)
    DW   bdos_rand_write_z                           ; fn40

; -------------------------------------------------------
; Report permanent error
; -------------------------------------------------------
bdos_persub:
    LD   HL, permsg                                  ; = 'B'
    CALL bdos_print_err
    CP   BDOS_CTLC
    JP   Z, warm_boot
    RET

; -------------------------------------------------------
; Report select error
; -------------------------------------------------------
bdos_selsub:
    LD   HL, selmsg                                  ;= 'S'
    JP   bdos_wait_err

; -------------------------------------------------------
; Report write to read/only disk
; -------------------------------------------------------
bdos_rodsub:
    LD   HL, rodmsg                                  ;= 'R'
    JP   bdos_wait_err

; -------------------------------------------------------
; Report read/only file
; -------------------------------------------------------
bdos_rofsub:
    LD   HL, rofmsg                                  ;= 'F'

; -------------------------------------------------------
; Wait for response before boot
; -------------------------------------------------------
bdos_wait_err:
    CALL bdos_print_err
    JP   warm_boot

; -------------------------------------------------------
; Error messages
; -------------------------------------------------------
dskmsg:
    DB   "Bdos Err On "
dskerr:
    DB   " : $"
permsg:
    DB   "Bad Sector$"
selmsg:
    DB   "Select$"
rofmsg:
    DB   "File "
rodmsg:
    DB   "R/O$"

; -------------------------------------------------------
; Print error to console, message address in HL
; -------------------------------------------------------
bdos_print_err:
    PUSH HL                                          ; save second message pointer
    CALL bdos_crlf
    ; set drive letter to message
    LD   A, (CPM_VARS.bdos_curdsk)
    ADD  A, 'A'
    LD   (dskerr), A

    LD   BC, dskmsg                                  ; print first error message
    CALL bdos_print
    POP  BC
    CALL bdos_print                                  ; print second message

; -------------------------------------------------------
; Console handlers
; Read console character to A
; -------------------------------------------------------
bdos_conin:
    LD   HL, CPM_VARS.bdos_kbchar
    LD   A, (HL)
    LD   (HL), 0x00
    OR   A
    RET  NZ
    ;no previous keyboard character ready
    JP   BIOS.conin_f

; -------------------------------------------------------
; Read character from console with echo
; -------------------------------------------------------
bdos_conech:
    CALL bdos_conin
    CALL bdos_chk_ctrl_char
    RET  C
    PUSH AF
    LD   C, A
    CALL bdos_outcon
    POP  AF
    RET

; -------------------------------------------------------
; Check for control char
; Onp: A - character
; Out: ZF if cr, lf, tab, or backspace
;      CF if code < 0x20
; -------------------------------------------------------
bdos_chk_ctrl_char:
    CP   ASCII_CR
    RET  Z
    CP   ASCII_LF
    RET  Z
    CP   ASCII_TAB
    RET  Z
    CP   ASCII_BS
    RET  Z
    CP   ASCII_SP
    RET

; -------------------------------------------------------
; check console  during output for
; Ctrl-S and Ctrl-C
; -------------------------------------------------------
bdos_conbrk:
    LD   A, (CPM_VARS.bdos_kbchar)
    OR   A
    JP   NZ, .if_key                                ; skip if key pressed
    CALL BIOS.const_f
    AND  0x1
    RET  Z                                          ; return if no char ready
    CALL BIOS.conin_f
    CP   CTRL_S
    JP   NZ, .no_crtl_s
    ; stop on crtl-s
    CALL BIOS.conin_f
    CP   CTRL_C
    ; reboot on crtl-c
    JP   Z, warm_boot
    XOR  A
    RET
.no_crtl_s:
    LD   (CPM_VARS.bdos_kbchar), A                  ; character in A, save it
.if_key:
    LD   A, 0x1                                     ; return with true set in accumulator
    RET

; --------------------------------------------------
; Out char C to screen (and printer)
; --------------------------------------------------
bdos_conout:
    LD   A, (CPM_VARS.bdos_no_outflag)                 ; if set, skip output, only move cursor
    OR   A
    JP   NZ, .compout
    ; check console break
    PUSH BC
    CALL bdos_conbrk
    POP  BC
    ; output c
    PUSH BC
    CALL BIOS.conout_f
    POP  BC

    ; send to printer (list) device
    PUSH BC
    LD   A, (CPM_VARS.bdos_prnflag)
    OR   A
    CALL NZ, BIOS.list_f
    POP  BC

; -------------------------------------------------------
; Update cursor position
; -------------------------------------------------------
.compout:
    LD   A, C
    ; recall the character
    ; and compute column position
    LD   HL, CPM_VARS.bdos_column
    CP   DEL_KEY
    RET  Z                                         ; 0x7F dont move cursor
    INC  (HL)                                      ; inc column
    CP   ASCII_SP                                  ; return for normal character > 0x20
    RET  NC

    ; restore column position
    DEC  (HL)
    LD   A, (HL)
    OR   A
    RET  Z                                         ; return if column=0

    LD   A, C
    CP   ASCII_BS
    JP   NZ, .not_backsp
    ; backspace character
    DEC  (HL)                                      ; BKSP  -> column-1
    RET

.not_backsp:
    CP   ASCII_LF
    RET  NZ
    LD   (HL), 0                                   ; LF -> column=0
    RET

; -------------------------------------------------------
; Send C character with possible preceding '^' char
; -------------------------------------------------------
bdos_ctlout:
    LD   A, C
    CALL bdos_chk_ctrl_char                        ; cy if not graphic (or special case)
    JP   NC, bdos_outcon                           ; normal output if non control char (tab, cr, lf)

    PUSH AF
    LD   C, CTRL                                   ; '^'
    CALL bdos_conout
    POP  AF
    OR   0x40                                      ; convert to letter equivalent (^M, ^J and so on)
    LD   C, A                                      ;

bdos_outcon:
    LD   A, C
    CP   ASCII_TAB
    JP   NZ, bdos_conout

    ; Print spaces until tab stop position
.to_tabpos:
    LD   C, ' '
    CALL bdos_conout
    LD   A, (CPM_VARS.bdos_column)
    AND  00000111b                                  ; column mod 8 = 0 ?
    JP   NZ, .to_tabpos
    RET

; -------------------------------------------------------
; Output backspace - erase previous character
; -------------------------------------------------------
bdos_out_bksp:
    CALL bdos_prn_bksp
    LD   C, ASCII_SP
    CALL BIOS.conout_f

; -------------------------------------------------------
; Send backspace to console
; -------------------------------------------------------
bdos_prn_bksp:
    LD   C, ASCII_BS
    JP   BIOS.conout_f

; -------------------------------------------------------
; print #, cr, lf for ctlx, ctlu, ctlr functions
; then move to strtcol (starting column)
; -------------------------------------------------------
bdos_newline:
    LD   C, '#'
    CALL bdos_conout
    CALL bdos_crlf
    ; move the cursor to the starting position
.print_sp:
    LD   A, (CPM_VARS.bdos_column)                 ; a=column
    LD   HL, CPM_VARS.bdos_strtcol
    CP   (HL)
    RET  NC                                        ; ret id startcol>column
    LD   C, ' '                                    ; print space
    CALL bdos_conout
    JP   .print_sp

; -------------------------------------------------------
; Out carriage return line feed sequence
; -------------------------------------------------------
bdos_crlf:
    LD   C, ASCII_CR
    CALL bdos_conout
    LD   C, ASCII_LF
    JP   bdos_conout

; -------------------------------------------------------
; Print $-ended string
; Inp: BC -> str$
; -------------------------------------------------------
bdos_print:
    LD   A, (BC)
    CP   '$'
    RET  Z
    INC  BC
    PUSH BC
    LD   C, A
    CALL bdos_outcon
    POP  BC
    JP   bdos_print

; -------------------------------------------------------
; Buffered console input
; Reads characters from the keyboard into a memory buffer
; until RETURN is pressed.
; Inp: C=0Ah
; DE=address or zero
; -------------------------------------------------------
bdos_read:
    LD   A, (CPM_VARS.bdos_column)
    LD   (CPM_VARS.bdos_strtcol), A
    LD   HL, (CPM_VARS.bdos_info)
    LD   C, (HL)
    INC  HL
    PUSH HL
    LD   B, 0
    ; B = current buffer length,
    ; C = maximum buffer length,
    ; HL= next to fill - 1
.readnx:
    PUSH BC
    PUSH HL
.readn1:
    CALL bdos_conin
    AND  0x7f                                        ; strip 7th bit
    POP  HL
    POP  BC
    ; end of line?
    CP   ASCII_CR
    JP   Z, .read_end
    CP   ASCII_LF
    JP   Z, .read_end

    ; backspace ?
    CP   ASCII_BS
    JP   NZ, .no_bksp
    LD   A, B
    OR   A
    JP   Z, .readnx                                  ; ignore BKSP for column=0
    DEC  B
    LD   A, (CPM_VARS.bdos_column)
    LD   (CPM_VARS.bdos_no_outflag), A
    JP   .linelen

    ; not a backspace
.no_bksp:
    CP   DEL_KEY
    JP   NZ, .no_del
    LD   A, B
    OR   A
    JP   Z, .readnx                                  ; ignore DEL for column=0
    LD   A, (HL)                                     ; cur char
    DEC  B                                           ; dec column
    DEC  HL                                          ; point to previous char
    JP   .echo                                       ; out previous char

.no_del:
    CP    CTRL_E                                    ; ^E - physical end of line
    JP    NZ, .no_ctrle
    PUSH  BC
    PUSH  HL
    CALL  bdos_crlf                                 ; out CR+LF
    XOR   A
    LD    (CPM_VARS.bdos_strtcol), A                ; reset start column to 0
    JP    .readn1

.no_ctrle:
    CP    CTRL_P                                    ; ^P
    JP    NZ, .no_ctrlp
    PUSH  HL
    LD    HL, CPM_VARS.bdos_prnflag
    LD    A, 0x1
    SUB   (HL)                                      ; flip printer output flag
    LD    (HL), A
    POP   HL
    JP    .readnx

.no_ctrlp:
    CP    CTRL_X                                    ; cancel current cmd line
    JP    NZ, .no_ctrlx
    POP   HL
.backx:
    LD    A, (CPM_VARS.bdos_strtcol)
    LD    HL, CPM_VARS.bdos_column
    CP    (HL)
    JP    NC, bdos_read
    DEC   (HL)
    CALL  bdos_out_bksp
    JP    .backx

.no_ctrlx:
    CP    CTRL_U                                    ; cancel current cmd line
    JP    NZ, .no_ctrlu

    CALL  bdos_newline
    POP   HL
    JP    bdos_read

.no_ctrlu:
    CP    CTRL_R                                    ; repeats current cmd line
    JP    NZ, .stor_echo

    ; start new line and retype
.linelen:
    PUSH  BC
    CALL  bdos_newline
    POP   BC
    POP   HL
    PUSH  HL
    PUSH  BC
.next_char:
    LD    A, B
    OR    A
    JP    Z, .endof_cmd                             ; end of cmd line?
    INC   HL
    LD    C, (HL)
    DEC   B
    PUSH  BC
    PUSH  HL
    CALL  bdos_ctlout
    POP   HL
    POP   BC
    JP    .next_char
.endof_cmd:
    PUSH  HL
    LD    A, (CPM_VARS.bdos_no_outflag)
    OR    A
    JP    Z, .readn1                                ; if no display, read next characters
    ; update cursor pos
    LD    HL, CPM_VARS.bdos_column
    SUB   (HL)
    LD    (CPM_VARS.bdos_no_outflag), A
    ; move back compcol-column spaces

.backsp:
    CALL  bdos_out_bksp
    LD    HL, CPM_VARS.bdos_no_outflag
    DEC   (HL)
    JP    NZ, .backsp
    JP    .readn1

; Put and Echo normal character
.stor_echo:
    INC   HL
    LD    (HL), A
    INC   B
.echo:
    PUSH  BC
    PUSH  HL
    LD    C, A
    CALL  bdos_ctlout
    POP   HL
    POP   BC

    ; warm boot on ctrl+c only at start of line
    LD   A, (HL)
    CP   CTRL_C
    LD   A, B
    JP   NZ, .no_ctrlc
    CP   1
    JP   Z, warm_boot

.no_ctrlc:
    CP   C                                          ; buffer is full?
    JP   C, .readnx                                 ; get next char in not full

    ; End of read operation, store length
.read_end:
    POP  HL
    LD   (HL), B
    LD   C, ASCII_CR
    JP   bdos_conout                                ; Our CR and return

; -------------------------------------------------------
; Console input with echo (C_READ)
; Out: A=L=character
; -------------------------------------------------------
bdos_con_inp:
    CALL bdos_conech
    JP   bdos_ret_a

; -------------------------------------------------------
; Console input chartacter
; Out: A=L=ASCII character
; -------------------------------------------------------
bdos_aux_inp:
    CALL BIOS.reader_f
    JP   bdos_ret_a

; -------------------------------------------------------
; Direct console I/O
; Inp: E=code.
; E=code. Returned values (in A) vary.
; 0xff  Return a character without echoing if one is
; waiting; zero if none
; 0xfe  Return console input status. Zero if no
; character is waiting
; Out: A
; -------------------------------------------------------
bdos_dir_con_io:
    LD   A, C
    INC  A
    JP   Z, .dirinp
    INC  A
    JP   Z, BIOS.const_f
    JP   BIOS.conout_f
.dirinp:
    CALL BIOS.const_f
    OR   A
    JP   Z, bdos_ret_mon
    CALL BIOS.conin_f
    JP   bdos_ret_a

; -------------------------------------------------------
; Get I/O byte
; Out: A = I/O byte.
; -------------------------------------------------------
bdos_get_io_byte:
    LD   A, (iobyte)
    JP   bdos_ret_a

; -------------------------------------------------------
; Set I/O byte
; Inp: C=I/O byte.
; TODO: Check, will be E ?
; -------------------------------------------------------
bdos_set_io_byte:
    LD   HL, iobyte
    LD   (HL), C
    RET

; -------------------------------------------------------
; Out $-ended string
; Inp: DE -> string$.
; -------------------------------------------------------
bdos_print_str:
    EX   DE, HL
    LD   C, L
    LD   B, H
    JP   bdos_print

; -------------------------------------------------------
; Get console status
; Inp: A=L=status
; -------------------------------------------------------
bdos_get_con_st:
    CALL bdos_conbrk
bdos_ret_a:
    LD   (CPM_VARS.bdos_aret), A
bdos_not_impl:
    RET

; -------------------------------------------------------
; Set IO Error status = 1
; -------------------------------------------------------
set_ioerr_1:
    LD   A, 1
    JP   bdos_ret_a

; -------------------------------------------------------
; Report select error
; -------------------------------------------------------
bdos_sel_error:
    LD   HL, bdos_sele_addr

; indirect Jump to [HL]
bdos_jump_hl:
    LD   E, (HL)
    INC  HL
    LD   D, (HL)                                    ; ->bdos_sele_addr+1
    EX   DE, HL
    JP   (HL)                                       ; ->bdos_selsub

; -------------------------------------------------------
; Move C bytes from [DE] to [HL]
; -------------------------------------------------------
bdos_move_dehl:
    INC  C
.move_next:
    DEC  C
    RET  Z
    LD   A, (DE)
    LD   (HL), A
    INC  DE
    INC  HL
    JP   .move_next

; -------------------------------------------------------
; Select the disk drive given by curdsk, and fill
; the base addresses curtrka - alloca, then fill
; the values of the disk parameter block
; Out: ZF - for big disk
; -------------------------------------------------------
bdos_sel_dsk:
    LD   A, (CPM_VARS.bdos_curdsk)
    LD   C, A
    CALL BIOS.seldsk_f                              ; HL filled by call
    ; HL = 0000 if error, otherwise -> DPH
    LD   A, H
    OR   L
    RET  Z                                          ; return if not valid drive

    LD   E, (HL)
    INC  HL
    LD   D, (HL)
    ; DE -> disk header

    ; save pointers
    INC  HL
    LD   (CPM_VARS.bdos_cdrmax), HL
    INC  HL
    INC  HL
    LD   (CPM_VARS.bdos_curtrk), HL
    INC  HL
    INC  HL
    LD   (CPM_VARS.bdos_currec), HL
    INC  HL
    INC  HL
    ; save translation vector (table) addr
    EX   DE, HL
    LD   (CPM_VARS.bdos_tranv), HL                  ; tran vector
    LD   HL, CPM_VARS.bdos_buffa                    ; DE=source for move, HL=dest
    LD   C, 8
    CALL bdos_move_dehl

    ; Fill the disk parameter block
    LD   HL, (CPM_VARS.bdos_dpbaddr)
    EX   DE, HL
    LD   HL, CPM_VARS.dbos_sectpt
    LD   C, 15
    CALL bdos_move_dehl

    ; Set single/double map mode
    LD   HL, (CPM_VARS.bdos_maxall)                 ; largest allocation number
    LD   A, H                                       ; a=h>0 for large disks (>255 blocks)
    LD   HL, CPM_VARS.bdos_small_disk
    LD   (HL), TRUE                                 ; small
    OR   A
    JP   Z, .is_small
    LD   (HL), FALSE                                ; big disk > 255 sec
.is_small:
    LD   A, 0xff
    OR   A
    RET

; -------------------------------------------------------
; Move to home position,
; then reset pointers for track and sector
; -------------------------------------------------------
bdos_home:
    CALL BIOS.home_f                                ; home head
    XOR  A
    ;  set track pointer to 0x0000
    LD   HL, (CPM_VARS.bdos_curtrk)
    LD   (HL), A
    INC  HL
    LD   (HL), A

    ; set current record pointer to 0x0000
    LD   HL, (CPM_VARS.bdos_currec)
    LD   (HL), A
    INC   HL
    LD   (HL), A

    RET

; -------------------------------------------------------
; Read buffer and check condition
; -------------------------------------------------------
bdos_rdbuff:
    CALL  BIOS.read_f
    JP    diocomp                                    ; check for i/o errors

; -------------------------------------------------------
; Write buffer and check condition
; Inp: C - write type
; 0 - normal write operation
; 1 - directory write operation
; 2 - start of new block
; -------------------------------------------------------
bdos_wrbuff:
    CALL  BIOS.write_f

; -------------------------------------------------------
; Check for disk errors
; -------------------------------------------------------
diocomp:
    OR    A
    RET   Z
    LD    HL, bdos_pere_addr                         ; = C899h
    JP    bdos_jump_hl

; -------------------------------------------------------
; Seek the record containing the current dir entry
; -------------------------------------------------------
bdos_seekdir:
    LD    HL, (CPM_VARS.bdos_dcnt)
    LD    C, DSK_SHF
    CALL  bdos_hlrotr
    LD    (CPM_VARS.bdos_arecord), HL
    LD    (CPM_VARS.bdos_drec), HL

; -------------------------------------------------------
; Seek the track given by arecord (actual record)
; local equates for registers
; -------------------------------------------------------
bdos_seek:
    LD   HL, CPM_VARS.bdos_arecord
    LD   C, (HL)
    INC  HL
    LD   B, (HL)
    ; BC = sector number

    LD   HL, (CPM_VARS.bdos_currec)
    LD   E, (HL)
    INC  HL
    LD   D, (HL)
    ; DE = current sector number

    LD   HL, (CPM_VARS.bdos_curtrk)
    LD   A, (HL)
    INC  HL
    LD   H, (HL)
    LD   L, A
    ; HL = current track

.sec_less_cur:
    ; check specified sector before current
    LD   A, C
    SUB  E
    LD   A, B
    SBC  A, D
    JP   NC, .sec_ge_cur
    ; yes, decrement current sectors for one track
    PUSH HL
    LD   HL, (CPM_VARS.dbos_sectpt)                 ; sectors per track
    LD   A, E
    SUB  L
    LD   E, A
    LD   A, D
    SBC  A, H
    LD   D, A
    ; current track-1
    POP  HL
    DEC  HL
    JP   .sec_less_cur
    ; specified sector >= current
.sec_ge_cur:
    PUSH HL
    LD   HL, (CPM_VARS.dbos_sectpt)
    ADD  HL, DE                                     ; current sector+track
    JP   C, .track_found                            ; jump if after current
    ; sector before current
    LD   A, C
    SUB  L
    LD   A, B
    SBC  A, H
    JP   C, .track_found
    EX   DE, HL
    POP  HL
    ; increment track
    INC  HL
    JP   .sec_ge_cur
    ; determined track number with specified sector
.track_found:
    POP  HL
    PUSH BC
    PUSH DE
    PUSH HL
    ; Stack contains (lowest) BC=arecord, DE=currec, HL=curtrk
    EX   DE, HL
    LD   HL, (CPM_VARS.bdos_offset)                 ; adjust if have reserved tracks
    ADD  HL, DE
    LD   B, H
    LD   C, L
    CALL BIOS.settrk_f                              ; select track

    ; save current track
    POP  DE                                         ; curtrk
    LD   HL, (CPM_VARS.bdos_curtrk)
    LD   (HL), E
    INC  HL
    LD   (HL), D

    ; save current record
    POP  DE
    LD   HL, (CPM_VARS.bdos_currec)
    LD   (HL), E
    INC  HL
    LD   (HL), D

    ; calc relative offset between arecord and currec
    POP  BC
    LD   A, C
    SUB  E
    LD   C, A
    LD   A, B
    SBC  A, D
    LD   B, A

    LD   HL, (CPM_VARS.bdos_tranv)                  ; HL -> translation table
    EX   DE, HL
    ; translate via BIOS call
    CALL BIOS.sectran_f
    ; select translated sector and return
    LD   C, L
    LD   B, H
    JP   BIOS.setsec_f

; -------------------------------------------------------
; Compute block number from record number and extent
; number
; -------------------------------------------------------
bdos_dm_position:
    LD   HL, CPM_VARS.bdos_blkshf
    LD   C, (HL)
    LD   A, (CPM_VARS.bdos_vrecord)                ; record number
    ;  a = a / 2^blkshf

dmpos_l0:
    OR   A
    RRA
    DEC  C
    JP   NZ, dmpos_l0
    ; A = shr(vrecord, blkshf) = vrecord/2^(sect/block)
    LD   B, A
    LD   A, 8
    SUB  (HL)
    ; c = 8 - blkshf
    LD   C, A
    LD   A, (CPM_VARS.bdos_extval)
    ; a = extval * 2^(8-blkshf)
dmpos_l1:
    DEC  C
    JP   Z, dmpos_l2
    OR   A
    RLA
    JP   dmpos_l1
dmpos_l2:
    ADD  A, B
    RET                                             ; with dm_position in A

; -------------------------------------------------------
; Return disk map value from position given by BC
; -------------------------------------------------------
bdos_getdm:
    LD   HL, (CPM_VARS.bdos_info)
    LD   DE, DSK_MAP
    ADD  HL, DE
    ADD  HL, BC
    LD   A, (CPM_VARS.bdos_small_disk)
    OR   A
    JP   Z,getdmd
    LD   L, (HL)
    LD   H, 0x00
    RET

getdmd:
    ADD  HL, BC
    LD   E, (HL)
    INC  HL
    LD   D, (HL)
    EX   DE, HL
    RET

; -------------------------------------------------------
; Compute disk block number from current FCB
; -------------------------------------------------------
bdos_index:
    CALL bdos_dm_position
    LD   C, A
    LD   B, 0
    CALL bdos_getdm
    LD   (CPM_VARS.bdos_arecord), HL
    RET

; -------------------------------------------------------
; Called following index to see if block allocated
; Out: ZF if not allocated
; -------------------------------------------------------
bdos_allocated:
    LD   HL, (CPM_VARS.bdos_arecord)
    LD   A, L
    OR   H
    RET

; -------------------------------------------------------
; Compute actual record address, assuming index called
; -------------------------------------------------------
bdos_atran:
    LD   A, (CPM_VARS.bdos_blkshf)
    LD   HL, (CPM_VARS.bdos_arecord)
bdatarn_l0:
    ADD  HL, HL
    DEC  A
    JP   NZ, bdatarn_l0
    LD   (CPM_VARS.bdos_arecord1), HL
    LD   A, (CPM_VARS.bdos_blmsk)
    LD   C, A
    LD   A, (CPM_VARS.bdos_vrecord)
    AND  C
    OR   L
    LD   L, A
    LD   (CPM_VARS.bdos_arecord), HL
    RET

; -------------------------------------------------------
; Get current extent field address to A
; -------------------------------------------------------
bdos_getexta:
    LD   HL, (CPM_VARS.bdos_info)
    LD   DE, FCB_EXT
    ADD  HL, DE                                     ; HL -> .fcb(extnum)
    RET

; -------------------------------------------------------
; Compute reccnt and nxtrec addresses for get/setfcb
; Out: HL -> record count byte in fcb
;      DE -> next record number byte in fcb
; -------------------------------------------------------
bdos_fcb_rcnr:
    LD   HL, (CPM_VARS.bdos_info)
    LD   DE, FCB_RC
    ADD  HL, DE
    EX   DE, HL
    LD   HL, 17                                   ; (nxtrec-reccnt)
    ADD  HL, DE
    RET

; -------------------------------------------------------
; Set variables from currently addressed fcb
; -------------------------------------------------------
bdos_stor_fcb_var:
    CALL bdos_fcb_rcnr
    LD   A, (HL)
    LD   (CPM_VARS.bdos_vrecord), A
    EX   DE, HL
    LD   A, (HL)
    LD   (CPM_VARS.bdos_rcount), A
    CALL bdos_getexta
    LD   A, (CPM_VARS.bdos_extmsk)
    AND  (HL)
    LD   (CPM_VARS.bdos_extval), A
    RET

; -------------------------------------------------------
; Set the next record to access.
; Inp: HL -> fcb next record byte
; -------------------------------------------------------
bdos_set_nxt_rec:
    CALL bdos_fcb_rcnr
    LD   A, (CPM_VARS.bdos_seqio)                  ; sequential mode flag (=1)
    CP   2
    JP   NZ, bsfcb_l0
    XOR  A                                         ; if 2 - no adder needed
bsfcb_l0:
    LD   C, A
    LD   A, (CPM_VARS.bdos_vrecord)                ; last record number
    ADD  A, C
    LD   (HL), A
    EX   DE, HL
    LD   A, (CPM_VARS.bdos_rcount)                 ; next record byte
    LD   (HL), A
    RET

; -------------------------------------------------------
; Rotate HL right by C bits
; -------------------------------------------------------
bdos_hlrotr:
    INC  C
bhlr_nxt:
    DEC  C
    RET  Z
    LD   A, H
    OR   A
    RRA
    LD   H, A
    LD   A,L
    RRA
    LD   L, A
    JP   bhlr_nxt

; -------------------------------------------------------
; Compute checksum for current directory buffer
; Out: A - checksum
; -------------------------------------------------------
bdos_compute_cs:
    LD   C, DIR_BUFF_SIZE                           ; size of directory buffer
    LD   HL, (CPM_VARS.bdos_buffa)
    XOR  A
bcompcs_add_nxt:
    ADD  A, (HL)
    INC  HL
    DEC  C
    JP   NZ, bcompcs_add_nxt
    RET

; -------------------------------------------------------
; Rotate HL left by C bits
; -------------------------------------------------------
bdos_hlrotl:
    INC  C
.nxt:
    DEC  C
    RET  Z
    ADD  HL, HL
    JP   .nxt

; -------------------------------------------------------
; Set a "1" value in curdsk position
; Inp: BC - vector
; Out: HL - vector with bit set
; -------------------------------------------------------
bdos_set_dsk_bit:
    PUSH BC
    LD   A, (CPM_VARS.bdos_curdsk)
    LD   C, A
    LD   HL, 0x0001
    CALL bdos_hlrotl
    POP  BC
    LD   A, C
    OR   L
    LD   L, A
    LD   A, B
    OR   H
    LD   H, A
    RET

; -------------------------------------------------------
; Get write protect status
; -------------------------------------------------------
bdos_nowrite:
    LD   HL, (CPM_VARS.bdos_rodsk)
    LD   A, (CPM_VARS.bdos_curdsk)
    LD   C, A
    CALL bdos_hlrotr
    LD   A, L
    AND  0x1
    RET

; -------------------------------------------------------
; Temporarily set current drive to be read-only;
; attempts to write to it will fail
; -------------------------------------------------------
bdos_set_ro:
    LD   HL, CPM_VARS.bdos_rodsk
    LD   C, (HL)
    INC  HL
    LD   B, (HL)

    CALL bdos_set_dsk_bit
    LD   (CPM_VARS.bdos_rodsk), HL
    ; high water mark in directory goes to max
    LD   HL, (CPM_VARS.bdos_dirmax)
    INC  HL
    EX   DE, HL
    LD   HL, (CPM_VARS.bdos_cdrmax)
    LD   (HL), E
    INC  HL
    LD   (HL), D
    RET

; -------------------------------------------------------
; Check current directory element for read/only status
; -------------------------------------------------------
bdos_check_rodir:
    CALL bdos_getdptra

; -------------------------------------------------------
; Check current buff(dptr) or fcb(0) for r/o status
; -------------------------------------------------------
bdos_check_rofile:
    LD   DE, RO_FILE
    ADD  HL, DE
    LD   A, (HL)
    RLA
    RET  NC
    LD   HL, bdos_rofe_addr                         ; = C8B1h
    JP   bdos_jump_hl

; -------------------------------------------------------
; Check for write protected disk
; -------------------------------------------------------
bdos_check_write:
    CALL bdos_nowrite
    RET  Z
    LD   HL, bdos_rode_addr                         ; = C8ABh
    JP   bdos_jump_hl

; -------------------------------------------------------
; Compute the address of a directory element at
; positon dptr in the buffer
; -------------------------------------------------------
bdos_getdptra:
    LD   HL, (CPM_VARS.bdos_buffa)
    LD   A, (CPM_VARS.bdos_dptr)

; -------------------------------------------------------
; HL = HL + A
; -------------------------------------------------------
bdos_hl_add_a:
    ADD  A,L
    LD   L, A
    RET  NC
    INC  H
    RET

; -------------------------------------------------------
; Compute the address of the s2 from fcb
; Out: A - module number
;      HL -> module number
; -------------------------------------------------------
bdos_get_s2:
    LD    HL, (CPM_VARS.bdos_info)
    LD    DE, FCB_S2
    ADD   HL, DE
    LD    A, (HL)
    RET

; -------------------------------------------------------
; Clear the module number field for user open/make
; -------------------------------------------------------
bdos_clear_s2:
    CALL  bdos_get_s2
    LD    (HL), 0
    RET

; -------------------------------------------------------
; Set FWF (File Write Flag)
; -------------------------------------------------------
bdos_set_fwf:
    CALL  bdos_get_s2
    OR    FWF_MASK
    LD    (HL), A
    RET

; -------------------------------------------------------
; Check for free entries in directory
; Out: CF is set - directory have more file entries
;      CF is reset - directory is full
; -------------------------------------------------------
bdos_dir_is_free:
    LD    HL, (CPM_VARS.bdos_dcnt)
    EX    DE, HL                                        ; DE = directory counter
    LD    HL, (CPM_VARS.bdos_cdrmax)                    ; HL = cdrmax
    LD    A, E
    SUB   (HL)
    INC   HL
    LD    A, D
    SBC   A, (HL)
    ; Condition dcnt - cdrmax produces cy if cdrmax>dcnt
    RET

; -------------------------------------------------------
; If not (cdrmax > dcnt) then cdrmax = dcnt+1
; -------------------------------------------------------
bdos_setcdr:
    CALL  bdos_dir_is_free
    RET   C
    INC   DE
    LD    (HL), D
    DEC   HL
    LD    (HL), E
    RET

; -------------------------------------------------------
; HL = DE - HL
; -------------------------------------------------------
bdos_de_sub_hl:
    LD    A, E
    SUB   L
    LD    L, A
    LD    A, D
    SBC   A, H
    LD    H, A
    RET

; Set directory checksubm byte
bdos_newchecksum:
    LD    C, 0xff


; -------------------------------------------------------
bdos_checksum:
    LD    HL, (CPM_VARS.bdos_drec)
    EX    DE, HL
    LD    HL, (CPM_VARS.bdos_alloc1)
    CALL  bdos_de_sub_hl
    RET   NC
    PUSH  BC
    CALL  bdos_compute_cs                            ; Compute checksum to A for current dir buffer
    LD    HL, (CPM_VARS.bdos_cksum)
    EX    DE, HL
    LD    HL, (CPM_VARS.bdos_drec)
    ADD   HL, DE
    POP   BC
    INC   C
    JP    Z, initial_cs
    CP    (HL)
    RET   Z
    CALL  bdos_dir_is_free
    RET   NC
    CALL  bdos_set_ro
    RET
initial_cs:
    LD    (HL), A
    RET

; -------------------------------------------------------
; Write the current directory entry, set checksum
; -------------------------------------------------------
bdos_wrdir:
    CALL  bdos_newchecksum
    CALL  bdos_setdir
    LD    C, 0x1
    CALL  bdos_wrbuff
    JP    bdos_setdata

; -------------------------------------------------------
; Read a directory entry into the directory buffer
; -------------------------------------------------------
bdos_rd_dir:
    CALL  bdos_setdir
    CALL  bdos_rdbuff

; -------------------------------------------------------
; Set data dma address
; -------------------------------------------------------
bdos_setdata:
    LD    HL, CPM_VARS.bdos_dmaad
    JP    bdos_set_dma

; -------------------------------------------------------
; Set directory dma address
; -------------------------------------------------------
bdos_setdir:
    LD    HL, CPM_VARS.bdos_buffa

; -------------------------------------------------------
; HL -> dma address to set (i.e., buffa or dmaad)
; -------------------------------------------------------
bdos_set_dma:
    LD    C, (HL)
    INC   HL
    LD    B, (HL)
    JP    BIOS.setdma_f

; -------------------------------------------------------
; Copy the directory entry to the user buffer
; after call to search or searchn by user code
; -------------------------------------------------------
bdos_dir_to_user:
    LD    HL, (CPM_VARS.bdos_buffa)
    EX    DE, HL
    LD    HL, (CPM_VARS.bdos_dmaad)
    LD    C, DIR_BUFF_SIZE
    JP    bdos_move_dehl

; -------------------------------------------------------
; return zero flag if at end of directory, non zero
; if not at end (end of dir if dcnt = 0ffffh)
; -------------------------------------------------------
bdos_end_of_dir:
    LD   HL, CPM_VARS.bdos_dcnt
    LD   A, (HL)
    INC  HL
    CP   (HL)
    RET  NZ
    INC  A
    RET

; -------------------------------------------------------
; Set dcnt to the end of the directory
; -------------------------------------------------------
bdos_set_end_dir:
    LD   HL, ENDDIR
    LD   (CPM_VARS.bdos_dcnt), HL
    RET

; -------------------------------------------------------
; Read next directory entry, with C=true if initializing
; -------------------------------------------------------
bdos_read_dir:
    LD   HL, (CPM_VARS.bdos_dirmax)
    EX   DE, HL
    LD   HL, (CPM_VARS.bdos_dcnt)
    INC  HL
    LD   (CPM_VARS.bdos_dcnt), HL
    CALL bdos_de_sub_hl
    JP   NC, bdrd_l0
    JP   bdos_set_end_dir
bdrd_l0:
    LD   A, (CPM_VARS.bdos_dcnt)
    AND  DSK_MSK
    LD   B, FCB_SHF
bdrd_l1:
    ADD  A, A
    DEC  B
    JP   NZ, bdrd_l1
    LD   (CPM_VARS.bdos_dptr), A
    OR   A
    RET  NZ
    PUSH BC
    CALL bdos_seekdir
    CALL bdos_rd_dir
    POP  BC
    JP   bdos_checksum

; -------------------------------------------------------
; Given allocation vector position BC, return with byte
; containing BC shifted so that the least significant
; bit is in the low order accumulator position.  HL is
; the address of the byte for possible replacement in
; memory upon return, and D contains the number of shifts
; required to place the returned value back into position
; -------------------------------------------------------
bdos_getallocbit:
    LD   A, C
    AND  00000111b
    INC  A
    LD   E, A
    LD   D, A
    LD   A, C
    RRCA
    RRCA
    RRCA
    AND  00011111b
    LD   C, A
    LD   A, B
    ADD  A, A
    ADD  A, A
    ADD  A, A
    ADD  A, A
    ADD  A, A
    OR   C
    LD   C, A
    LD   A, B
    RRCA
    RRCA
    RRCA
    AND  00011111b
    LD   B, A
    LD   HL, (CPM_VARS.bdos_alloca)                  ; Base address of allocation vector
    ADD  HL, BC
    LD   A, (HL)
ga_rotl:
    RLCA
    DEC  E
    JP   NZ, ga_rotl
    RET

; -------------------------------------------------------
; BC is the bit position of ALLOC to set or reset.  The
; value of the bit is in register E.
; -------------------------------------------------------
bdos_setallocbit:
    PUSH DE
    CALL bdos_getallocbit
    AND  11111110b
    POP  BC
    OR   C

; -------------------------------------------------------
; Rotate and replace
; Inp: A - Byte value from ALLOC
;      D - bit position (1..8)
;      HL - target ALLOC position
; -------------------------------------------------------
bdos_sab_rotr:
    RRCA
    DEC  D
    JP   NZ, bdos_sab_rotr
    LD   (HL), A
    RET

; -------------------------------------------------------
; Set or clear space used bits in allocation map
; Inp: C=0 - clear, C=1 - set
; -------------------------------------------------------
bdos_scandm:
    CALL bdos_getdptra
    ; HL addresses the beginning of the directory entry
    LD   DE, DSK_MAP
    ADD  HL, DE                                     ; HL -> block number byte
    PUSH BC
    LD   C, 17                                      ; fcblen-dskmap+1
    ; scan all 17 bytes
.scan_next:
    POP  DE
    DEC  C
    RET  Z                                          ; return if done

    PUSH DE
    ; small or bug disk?
    LD   A, (CPM_VARS.bdos_small_disk)
    OR   A
    JP   Z, .scan_big
    PUSH BC
    PUSH HL
    ; small, one byte
    LD   C, (HL)
    LD   B, 0
    JP   .scan_cmn
.scan_big:
    ; big, two byte
    DEC  C
    PUSH BC
    LD   C, (HL)
    INC  HL
    LD   B, (HL)
    PUSH HL

.scan_cmn:
    ; this block used?
    LD   A, C
    OR   B
    JP   Z, .not_used
    ; have free space on disk?
    LD   HL, (CPM_VARS.bdos_maxall)
    LD   A, L
    SUB  C
    LD   A, H
    SBC  A, B
    CALL NC, bdos_setallocbit                       ; enough space, set bit

.not_used:
    POP  HL                                         ; HL -> next block in fcb
    INC  HL
    POP  BC
    JP   .scan_next

; -------------------------------------------------------
; Initialize the current disk
; lret = false, set to true if $ file exists
; compute the length of the allocation vector - 2
; -------------------------------------------------------
bdos_initialize:
    ; compute size of alloc table
    LD   HL, (CPM_VARS.bdos_maxall)
    LD   C, 3
    CALL bdos_hlrotr                                ; HL = disk size / 8 + 1
    INC  HL
    LD   B, H
    LD   C, L                                       ; BC = alloc table size
    LD   HL, (CPM_VARS.bdos_alloca)                 ; address of allocation table
    ; fill with zeroes
.alt_fill_0:
    LD   (HL), 0
    INC  HL
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, .alt_fill_0

    LD   HL, (CPM_VARS.bdos_dirblk)                 ; DE = initial space, used by directory
    EX   DE, HL
    LD   HL, (CPM_VARS.bdos_alloca)                 ; HL -> allocation map
    LD   (HL), E
    INC   HL
    LD   (HL), D                                    ; [HL] = DE
    CALL bdos_home                                  ; home drive, set initial head, track, sector

    LD   HL, (CPM_VARS.bdos_cdrmax)
    LD   (HL), 3                                    ; next dir read
    INC  HL
    LD   (HL), 0
    CALL bdos_set_end_dir                           ; mark end of dir

.rd_next_f:
    ; read next file name
    LD   C, 0xff
    CALL bdos_read_dir
    CALL bdos_end_of_dir                            ; is have another file?
    RET  Z
    ; get params of file, and check deleted
    CALL bdos_getdptra
    LD   A, FILE_DELETED
    CP   (HL)
    JP   Z, .rd_next_f
    ; check user code
    LD   A, (CPM_VARS.bdos_userno)
    CP   (HL)
    JP   NZ, .set_as_used
    INC  HL
    LD   A, (HL)
    SUB  '$'                                        ; check for $name file
    JP   NZ, .set_as_used
    ; dollar file found, mark in lret
    DEC  A
    LD   (CPM_VARS.bdos_aret), A

.set_as_used:
    LD   C, 1
    CALL bdos_scandm
    CALL bdos_setcdr
    JP   .rd_next_f

; -------------------------------------------------------
; Return directory location as lret
; used in delete, rename, ...
; -------------------------------------------------------
bdos_ret_dirloc:
    LD   A, (CPM_VARS.bdos_dirloc)
    JP   bdos_ret_a

; -------------------------------------------------------
; Compare extent# in A with that in C, return nonzero
; if they do not match
; -------------------------------------------------------
bdos_compext:
    PUSH BC
    PUSH AF
    LD   A, (CPM_VARS.bdos_extmsk)
    CPL
    LD   B, A
    LD   A, C
    AND  B
    LD   C, A
    POP  AF
    AND  B
    SUB  C
    AND  MAX_EXT                                    ; check only bits [4:0] (MAX_EXT)
    POP  BC
    RET

; -------------------------------------------------------
; Search for directory element of length C at info
; -------------------------------------------------------
bdos_search:
    LD   A, 0xff
    LD   (CPM_VARS.bdos_dirloc), A
    LD   HL, CPM_VARS.bdos_searchl                  ; length in bytes to match
    LD   (HL), C
    LD   HL, (CPM_VARS.bdos_info)                   ; address filename to match
    LD   (CPM_VARS.bdos_searcha), HL
    CALL bdos_set_end_dir
    CALL bdos_home

; -------------------------------------------------------
; Search for the next directory element, assuming
; a previous call on search which sets searcha and
; searchl
; -------------------------------------------------------
bdos_search_nxt:
    LD   C, 0
    CALL bdos_read_dir                              ; get next filename entry
    CALL bdos_end_of_dir                            ; at end? pos=0xffff
    JP   Z, .not_found                              ; jump at end

    LD   HL, (CPM_VARS.bdos_searcha)
    EX   DE, HL
    ; skip if file deleted
    LD   A, (DE)
    CP   FILE_DELETED
    JP   Z, .search_next

    PUSH DE
    CALL bdos_dir_is_free
    POP  DE
    JP   NC, .not_found                             ; Is full, no more file entries

.search_next:
    CALL bdos_getdptra                              ; get address of FCB
    LD   A, (CPM_VARS.bdos_searchl)                 ; BC=length to compare
    LD   C, A
    LD   B, 0

    ; compare loop
.search_loop:
    LD   A, C
    OR   A
    JP   Z, .search_end                             ; search completed?
    ; check byte
    LD   A, (DE)
    CP   '?'
    JP   Z, .search_ok                              ; match if wildcard
    LD   A, B
    CP   13                                         ; ignore 13th byte
    JP   Z, .search_ok
    CP   FCB_EXT                                    ; extent byte
    LD   A, (DE)
    JP   Z, .search_ext
    SUB  (HL)                                       ; compare bytes
    AND  0x7f                                       ; mask 7th bit
    JP   NZ, bdos_search_nxt                        ; if not match, check next file entry
    JP   .search_ok

    ; compare ext
.search_ext:
    PUSH BC
    LD   C, (HL)
    CALL bdos_compext
    POP  BC
    JP   NZ, bdos_search_nxt                        ; if not match, check next file entry

    ; current bytes matches, increment pointers, decrement counter
.search_ok:
    INC  DE
    INC  HL
    INC  B
    DEC  C
    JP   .search_loop                               ; compare next byte

    ; entiry name matches, return dir position
.search_end:
    LD   A, (CPM_VARS.bdos_dcnt)
    AND  0x3
    LD   (CPM_VARS.bdos_aret), A
    LD   HL, CPM_VARS.bdos_dirloc
    LD   A, (HL)
    RLA
    RET  NC
    XOR  A
    LD   (HL), A
    RET

    ; end of directory, or empty name
.not_found:
    CALL bdos_set_end_dir
    LD   A, 0xff
    JP   bdos_ret_a

; -------------------------------------------------------
; Delete the currently addressed file
; -------------------------------------------------------
bdos_era_file:
    CALL bdos_check_write                           ; check write rotection
    LD   C, 12                                      ; length of filename
    CALL bdos_search                                ; search file in directory (with wildcards)
.del_next:
    CALL bdos_end_of_dir
    RET  Z                                          ; no more dir entries - exit
    CALL bdos_check_rodir                           ; check file RO
    CALL bdos_getdptra                              ; get file info
    LD   (HL), FILE_DELETED                         ; set deleted marker at first symbol
    LD   C, 0
    CALL bdos_scandm                                ; clear space at map
    CALL bdos_wrdir                                 ; write directory to disk
    CALL bdos_search_nxt                            ; find next file
    JP   .del_next

; -------------------------------------------------------
; Given allocation vector position BC, find the zero bit
; closest to this position by searching left and right.
; if found, set the bit to one and return the bit position
; in hl.  if not found (i.e., we pass 0 on the left, or
; maxall on the right), return 0000 in hl
; -------------------------------------------------------
bdos_get_block:
    LD    D, B
    LD    E, C
.prev_test:
    LD    A, C
    OR    B
    JP    Z, .next_test                             ; jump if block 0 specified
    ; check previous block
    DEC   BC
    PUSH  DE
    PUSH  BC
    CALL  bdos_getallocbit
    RRA
    JP    NC, .ret_block                            ; block is empty, use it

    ; not empty, check more
    POP   BC
    POP   DE

    ; look at next block
.next_test:
    ; check for free space on disk
    LD    HL, (CPM_VARS.bdos_maxall)
    LD    A, E
    SUB   L
    LD    A, D
    SBC   A, H
    JP    NC, .ret_no_block
    ; have space,  move to next block
    INC   DE
    PUSH  BC
    PUSH  DE
    LD    B, D
    LD    C, E
    CALL  bdos_getallocbit
    RRA
    JP    NC, .ret_block                            ; block is empty, use it
    POP   DE
    POP   BC
    JP    .prev_test

    ; mark block as used and return in HL
.ret_block:
    RLA
    INC   A
    CALL  bdos_sab_rotr
    POP   HL
    POP   DE
    RET

    ; no free blocks found
.ret_no_block:
    LD    A, C
    OR    B
    JP    NZ, .prev_test                            ; if BC != 0 try to find before
    LD    HL, 0x00                                  ; not found
    RET

; -------------------------------------------------------
; Copy the entire file control block
; -------------------------------------------------------
bdos_copy_fcb:
    LD    C, 0x00
    LD    E, FCB_LEN
; -------------------------------------------------------
; copy fcb information starting at C for E bytes
; into the currently addressed directory entry
; -------------------------------------------------------
dbos_copy_dir:
    PUSH  DE
    LD    B, 0x00
    LD    HL, (CPM_VARS.bdos_info)
    ADD   HL, BC
    EX    DE, HL
    CALL  bdos_getdptra
    POP   BC
    CALL  bdos_move_dehl

; -------------------------------------------------------
; Enter from close to seek and copy current element
; -------------------------------------------------------
bdos_seek_copy:
    CALL  bdos_seekdir
    JP    bdos_wrdir

; -------------------------------------------------------
; Rename the file described by the first half of
; the currently addressed FCB to new name, contained in
; the last half of the currently addressed FCB.
; -------------------------------------------------------
bdos_rename:
    CALL  bdos_check_write                          ; check for RO disk
    LD    C, 12                                     ; use 12 symbols of name to compare
    CALL  bdos_search                               ; find first file
    LD    HL, (CPM_VARS.bdos_info)                  ; file info
    LD    A, (HL)                                   ; user number
    LD    DE, 16                                    ; shift to place second file info
    ADD   HL, DE
    LD    (HL), A                                   ; set same user no

.ren_next:
    CALL  bdos_end_of_dir
    RET   Z                                         ; return if end of directory

    CALL  bdos_check_rodir                          ; check file RO flag
    LD    C, 16                                     ; start from 16th byte
    LD    E, FN_LEN                                 ; and copy 12 byte
    CALL  dbos_copy_dir
    CALL  bdos_search_nxt                           ; search next file
    JP    .ren_next

; -------------------------------------------------------
; Update file attributes for current fcb
; -------------------------------------------------------
bdos_update_attrs:
    LD    C, FN_LEN
    CALL  bdos_search                               ; search file by 12 bytes of name
.set_next:
    CALL  bdos_end_of_dir
    RET   Z                                         ; return if not found

    LD    C, 0
    LD    E, FN_LEN
    CALL  dbos_copy_dir                             ; copy name to FCB and save dir
    CALL  bdos_search_nxt
    JP    .set_next                                 ; ; do it for next file

; --------------------------------------------------
; Open file, name specified in FCB
; -------------------------------------------------------
open:
    LD   C, FCB_INFO_LEN
    CALL bdos_search                                ; search file
    CALL bdos_end_of_dir
    RET  Z                                          ; return if not found

bdos_open_copy:
    CALL bdos_getexta
    LD   A, (HL)                                    ; get extent byte
    PUSH AF                                         ; save ext
    PUSH HL                                         ; and it's address
    CALL bdos_getdptra
    EX   DE, HL
    ; move to user space
    LD   HL, (CPM_VARS.bdos_info)
    LD   C, 32
    PUSH DE
    CALL bdos_move_dehl
    CALL bdos_set_fwf                                ; set 7th bit s2 "unmodified" flag

    POP  DE
    LD   HL, FCB_EXT
    ADD  HL, DE
    LD   C, (HL)                                    ; C = extent byte

    LD   HL, FCB_RC
    ADD  HL, DE
    LD   B, (HL)                                    ; B = record count

    POP  HL
    POP  AF
    LD   (HL), A
    LD   A, C
    CP   (HL)                                       ; cmp extent bytes
    LD   A, B
    JP   Z, .set_rec_cnt
    ; user specified extent is not same, reset record count to 0
    LD   A, 0
    JP   C, .set_rec_cnt
    ; set to maximum
    LD   A, 128

.set_rec_cnt:
    ; set record count in user FCB to A
    LD   HL, (CPM_VARS.bdos_info)
    LD   DE, FCB_RC
    ADD  HL, DE
    LD   (HL), A
    RET

; --------------------------------------------------
; HL = .fcb1(i), DE = .fcb2(i),
; if fcb1(i) = 0 then fcb1(i) := fcb2(i)
; --------------------------------------------------
bdos_mergezero:
    LD   A, (HL)
    INC  HL
    OR   (HL)
    DEC  HL
    RET  NZ
    LD   A, (DE)
    LD   (HL), A
    INC  DE
    INC  HL
    LD   A, (DE)
    LD   (HL), A
    DEC  DE
    DEC  HL
    RET

; --------------------------------------------------
; Close file specified by FCB
; --------------------------------------------------
bdos_close:
    XOR  A
    ; clear status and file position bytes
    LD   (CPM_VARS.bdos_aret), A
    LD   (CPM_VARS.bdos_dcnt), A
    LD   (CPM_VARS.bdos_dcnt+1), A
    CALL bdos_nowrite                               ; get write protection
    RET  NZ                                         ; return if set

    CALL bdos_get_s2
    AND  FWF_MASK
    RET  NZ                                         ; return if not modified flag set
    ; search file
    LD   C, FCB_INFO_LEN
    CALL bdos_search
    CALL bdos_end_of_dir
    RET  Z                                          ; return if not found

    LD   BC, DSK_MAP                                ; offset of records used
    CALL bdos_getdptra
    ADD  HL, BC
    EX   DE, HL

    LD   HL, (CPM_VARS.bdos_info)                   ; same for user FCB
    ADD  HL, BC
    LD   C, 16                                      ; bytes in extent

bdos_merge0:
    LD   A, (CPM_VARS.bdos_small_disk)              ;  small/big disk flag
    OR   A
    JP   Z, bdos_merge                              ; jump if big (16 bit)
    ; small disk (8 bit)
    LD   A, (HL)                                    ; from user FCB
    OR   A
    LD   A, (DE)                                    ; from DIR FCB
    JP   NZ, bdm_fcbnzero
    LD   (HL), A                                    ; user is 0, set from directory fcb

bdm_fcbnzero:
    OR   A
    JP   NZ, bdm_buffnzero
    ; dir is 0, set from user fcb
    LD   A, (HL)
    LD   (DE), A

bdm_buffnzero:
    CP   (HL)
    JP   NZ, bdm_mergerr                            ; if both non zero, close error
    JP   bdm_dmset                                  ; merged ok, go to next fcb byte

bdos_merge:
    CALL bdos_mergezero                             ; update user fcb if it is zero
    EX   DE, HL
    CALL bdos_mergezero                             ; update dir fcb if it is zero
    EX   DE, HL
    LD   A, (DE)
    CP   (HL)
    JP   NZ, bdm_mergerr                            ; if both is same, close error

    ; next byte
    INC  DE
    INC  HL
    LD   A, (DE)
    CP   (HL)
    JP   NZ, bdm_mergerr                            ; if both is same, close error
    DEC  C

bdm_dmset:
    ; next
    INC  DE
    INC  HL
    DEC  C
    JP   NZ, bdos_merge0                            ; merge next if C>0

    LD   BC, 0xffec                                 ; -(fcblen-extnum) (-20)
    ADD  HL, BC
    EX   DE, HL
    ADD  HL, BC
    LD   A, (DE)
    CP   (HL)
    JP   C, bdm_endmerge                            ; directory extent > user extent -> end
    ; update record count in dir FCB
    LD   (HL), A
    LD   BC, 0x3                                    ; (reccnt-extnum)
    ADD  HL, BC
    EX   DE, HL
    ADD  HL, BC
    LD   A, (HL)                                    ; get from user FCB
    LD   (DE), A                                    ; set to directory FCB

bdm_endmerge:
    LD   A, 0xff
    LD   (CPM_VARS.bdos_fcb_closed), A              ; set was open and closed flag
    JP   bdos_seek_copy                             ; update directory

    ; set return status and return
bdm_mergerr:
    LD   HL, CPM_VARS.bdos_aret
    DEC  (HL)
    RET

; -------------------------------------------------------
; Create a new file by creating a directory entry
; then opening the file
; -------------------------------------------------------
bdos_make:
    CALL bdos_check_write                               ; check Write Protection
    LD   HL, (CPM_VARS.bdos_info)                       ; save user FCB
    PUSH HL                                             ; on stack
    LD   HL, CPM_VARS.bdos_efcb                         ; empty FCB
    ; Save FCB address, look for 0xE5
    LD   (CPM_VARS.bdos_info), HL                       ; set empty FCB
    ; search firs empty slot in directory
    LD   C, 0x1                                         ; compare one byte 0xE5
    CALL bdos_search
    CALL bdos_end_of_dir                                ; found flag
    POP  HL
    LD   (CPM_VARS.bdos_info), HL                       ; restore user FCB address
    RET  Z                                              ; return if not found (no space in dir)
    EX   DE, HL
    LD   HL, FCB_RC                                    ; number or record for this file
    ADD  HL, DE

    ; clear FCB tail
    LD   C, 17                                          ; fcblen-namlen
    XOR  A
.fcb_set_0:
    LD   (HL), A
    INC  HL
    DEC  C
    JP   NZ, .fcb_set_0

    LD   HL, FCB_S1
    ADD  HL, DE
    LD   (HL), A                                        ; Current record within extent?
    CALL bdos_setcdr
    CALL bdos_copy_fcb
    JP   bdos_set_fwf

; -------------------------------------------------------
; Close the current extent, and open the next one to read
; if possible.  RMF is true if in read mod
; -------------------------------------------------------
bdos_open_reel:
    XOR  A
    LD   (CPM_VARS.bdos_fcb_closed), A                  ; clear close flag
    CALL bdos_close                                     ; close extent
    CALL bdos_end_of_dir                                ; check space
    RET  Z                                              ; ret if no more space
    ; get extent byte from user FCB
    LD   HL, (CPM_VARS.bdos_info)
    LD   BC, FCB_EXT
    ADD  HL, BC
    LD   A, (HL)
    ; and increment it, mask to 0..31 and store
    INC  A
    AND  MAX_EXT
    LD   (HL), A
    JP   Z, .overflow
    ; mask extent byte
    LD   B, A
    LD   A, (CPM_VARS.bdos_extmsk)
    AND  B
    LD   HL, CPM_VARS.bdos_fcb_closed
    AND  (HL)
    JP   Z, .read_nxt_extent                            ; read netx extent
    JP   .extent_in_mem

.overflow:
    LD   BC, 0x2                                        ; S2
    ADD  HL, BC
    INC  (HL)
    LD   A, (HL)
    AND  MAX_MOD
    JP   Z, .error

.read_nxt_extent:
    LD   C, FCB_INFO_LEN
    CALL bdos_search
    CALL bdos_end_of_dir
    JP   NZ, .extent_in_mem                             ; jump if success
    ; not extent found
    LD   A, (CPM_VARS.bdos_rfm)
    INC  A
    JP   Z, .error                                      ; can not get extent
    CALL bdos_make                                      ; make new empty dir entry for extent
    CALL bdos_end_of_dir                                ; chk no space
    JP   Z, .error                                      ; jmp to error if no space in directory
    JP   .almost_done
.extent_in_mem:
    ; open extent
    CALL bdos_open_copy
.almost_done:
    CALL bdos_stor_fcb_var                              ; move updated data to new extent
    XOR  A                                              ; clear error flag
    JP   bdos_ret_a
.error:
    CALL set_ioerr_1
    JP   bdos_set_fwf                                    ; clear bit 7 in s2

; -------------------------------------------------------
; Sequential disk read operation
; -------------------------------------------------------
bdos_seq_disk_read:
    LD   A, 0x1
    LD   (CPM_VARS.bdos_seqio), A
    ; drop through to diskread

bdos_disk_read:
    LD   A, TRUE
    LD   (CPM_VARS.bdos_rfm), A                         ; dont allow read unwritten data
    CALL bdos_stor_fcb_var
    LD   A, (CPM_VARS.bdos_vrecord)                     ; next record to read
    LD   HL, CPM_VARS.bdos_rcount                       ; number of records in extent
    ; in this extent?
    CP   (HL)
    JP   C, .recordok
    ; no, in next extent, check this exctent fully used
    CP   128
    JP   NZ, .error_opn
    ;open next extent
    CALL bdos_open_reel
    XOR  A
    ; reset record number to read
    LD   (CPM_VARS.bdos_vrecord), A
    ; check open status
    LD   A, (CPM_VARS.bdos_aret)
    OR   A
    JP   NZ, .error_opn
.recordok:
    ; compute block number to read
    CALL bdos_index
    ; check if it in bounds
    CALL bdos_allocated
    JP   Z, .error_opn

    CALL bdos_atran                                 ; convert to logical sector
    CALL bdos_seek                                  ; seec track and sector
    CALL bdos_rdbuff                                ; read sector
    JP   bdos_set_nxt_rec

.error_opn:
    JP   set_ioerr_1

; -------------------------------------------------------
; Sequential write disk
; -------------------------------------------------------
bdos_seq_disk_write:
    LD   A, 0x1
    LD   (CPM_VARS.bdos_seqio), A
bdos_disk_write:
    LD   A, FALSE
    LD   (CPM_VARS.bdos_rfm), A                     ; allow open new extent
    CALL bdos_check_write                           ; check write protection
    LD   HL, (CPM_VARS.bdos_info)                   ; HL -> FCB
    CALL bdos_check_rofile                          ; check RO file
    CALL bdos_stor_fcb_var
    LD   A, (CPM_VARS.bdos_vrecord)                 ; get record number to write to
    CP   128                                        ; lstrec+1
    JP   NC, set_ioerr_1                            ; not in range - error
    CALL bdos_index                                 ; compute block number
    CALL bdos_allocated                             ; check number
    LD   C, 0
    JP   NZ, .disk_wr1
    CALL bdos_dm_position                           ; get next block number within FCB
    LD   (CPM_VARS.bdos_dminx), A                   ; and store it
    LD   BC, 0x00                                   ; start looking for space from start
    OR   A
    JP   Z, .nop_block                            ; zero - not allocated
    ; extract previous blk from fcb
    LD   C, A
    DEC  BC
    CALL bdos_getdm
    LD   B, H
    LD   C, L
.nop_block:
    CALL bdos_get_block                             ; get next free nearest block
    LD   A, L
    OR   H
    JP   NZ, .block_ok
    ; error, no more space
    LD   A, 2
    JP   bdos_ret_a
.block_ok:
    LD   (CPM_VARS.bdos_arecord), HL                ; set record to access
    EX   DE, HL                                     ; to DE
    LD   HL, (CPM_VARS.bdos_info)                   ;
    LD   BC, FCB_AL                                 ; 16
    ADD  HL, BC
    ; small/big disk
    LD   A, (CPM_VARS.bdos_small_disk)
    OR   A
    LD   A, (CPM_VARS.bdos_dminx)                   ; rel block
    JP   Z, .alloc_big                              ; jump for 16bit
    CALL bdos_hl_add_a                              ; HL = HL + A
    LD   (HL), E                                    ; save record to access
    JP   .disk_wru

.alloc_big:
    ; save record to acces 16bit
    LD   C, A
    LD   B, 0
    ADD  HL, BC
    ADD  HL, BC
    LD   (HL), E
    INC  HL
    LD   (HL), D

    ; Disk write to previously unallocated block
.disk_wru:
    LD   C, 2                                       ; C=2 - write to unused disk space
.disk_wr1:
    LD   A, (CPM_VARS.bdos_aret)                    ; check status
    OR   A
    RET  NZ                                         ; return on error

    PUSH BC                                         ; store C write flag for BIOS
    CALL bdos_atran                                 ; convert block number to logical sector
    LD   A, (CPM_VARS.bdos_seqio)                   ; get access mode (0=random, 1=sequential, 2=special)
    DEC  A
    DEC  A
    JP   NZ, .normal_wr
    ; special
    POP  BC
    PUSH BC
    LD   A, C                                       ; A = write status flag (2=write unused space)
    DEC  A
    DEC  A
    JP   NZ, .normal_wr
    ; fill buffer with zeroes
    PUSH HL
    LD   HL, (CPM_VARS.bdos_buffa)
    LD   D, A
.fill0:
    LD   (HL), A
    INC  HL
    INC  D
    JP   P, .fill0
    CALL bdos_setdir                                ; Tell BIOS buffer address to directory access
    LD   HL, (CPM_VARS.bdos_arecord1)               ; get sector that starts current block
    LD   C, 2                                       ; write status flag (2=write unused space)
.fill1:
    LD   (CPM_VARS.bdos_arecord), HL                ; set sector to write
    PUSH BC
    CALL bdos_seek                                  ; seek track and sector number
    POP  BC
    CALL bdos_wrbuff                                ; write zeroes
    LD   HL, (CPM_VARS.bdos_arecord)                ; get sector number
    LD   C, 0                                       ; normal write flag
    LD   A, (CPM_VARS.bdos_blmsk)                   ; block mask
    ; write entire block?
    LD   B, A
    AND  L
    CP   B
    INC  HL
    JP   NZ, .fill1                                 ; continue until (BLKMSK+1) sectors written
    ; reset sector number
    POP  HL
    LD   (CPM_VARS.bdos_arecord), HL
    CALL bdos_setdata                               ; reset DMA address

    ; Normal write.
.normal_wr:
    CALL bdos_seek                                  ; Set track and sector
    POP  BC                                         ; restore C - write status flag
    PUSH BC
    CALL bdos_wrbuff                                ; write
    POP  BC
    LD   A, (CPM_VARS.bdos_vrecord)                 ; last file record
    LD   HL, CPM_VARS.bdos_rcount                   ; last written record
    CP   (HL)
    JP   C, .disk_wr2
    ; update record count
    LD   (HL), A
    INC  (HL)
    LD   C, 2
.disk_wr2:
    DEC  C                                          ; patch: NOP
    DEC  C                                          ; patch: NOP
    JP   NZ, .no_update                             ; patch: LD HL,0
    PUSH AF
    CALL bdos_get_s2                             ; set 'extent written to' flag
    AND  0x7f                                       ; clear 7th bit
    LD   (HL), A
    POP  AF                                         ; record count

.no_update:
    ; it is full
    CP   127
    JP   NZ, .set_nxt_rec
    ; sequential mode?
    LD   A, (CPM_VARS.bdos_seqio)
    CP   1
    JP   NZ, .set_nxt_rec

    CALL bdos_set_nxt_rec                           ; set next record
    CALL bdos_open_reel                             ; get space in dir
    LD   HL, CPM_VARS.bdos_aret                     ; check status
    LD   A, (HL)
    OR   A
    JP   NZ, .no_space                              ; no more space

    DEC  A
    LD   (CPM_VARS.bdos_vrecord), A                 ; -1

.no_space:
    LD   (HL), 0x00                                 ; clear status

.set_nxt_rec:
    JP   bdos_set_nxt_rec

; -------------------------------------------------------
; Random access seek operation, C=0ffh if read mode
; FCB is assumed to address an active file control block
; (modnum has been set to 1100$0000b if previous bad seek)
; -------------------------------------------------------
bdos_rseek:
    XOR  A
    LD   (CPM_VARS.bdos_seqio), A                   ; set random mode

bdos_rseek1:
    PUSH BC
    LD   HL, (CPM_VARS.bdos_info)
    EX   DE, HL
    LD   HL, FCB_RN                                 ; Random access record number
    ADD  HL, DE
    LD   A, (HL)
    AND  7Fh                                        ; [6:0] bits record number to access
    PUSH AF
    ; get bit 7
    LD   A, (HL)
    RLA
    INC  HL
    ; get high byte [3:0] bits
    LD   A, (HL)
    RLA
    AND  00011111b
    ; get extent byte
    LD   C, A
    LD   A, (HL)
    RRA
    RRA
    RRA
    RRA
    AND  0x0f
    LD   B, A
    POP  AF
    INC  HL
    ; get next byte
    LD   L, (HL)
    ; check overflow
    INC  L
    DEC  L
    LD   L, 6
    JP   NZ, .seek_err                              ; error 6 if overflow

    ; save current record in FCB
    LD   HL, FCB_CR
    ADD  HL, DE
    LD   (HL), A

    ; check extent byte
    LD   HL, FCB_EXT
    ADD  HL, DE
    LD   A, C
    SUB  (HL)
    JP   NZ, .close                                 ; not same as previous
    ; check extra s2 byte
    LD   HL, FCB_S2
    ADD  HL, DE
    LD   A, B
    SUB  (HL)
    AND  0x7f
    JP   Z, .seek_ok                                ; same, is ok

.close:
    PUSH BC
    PUSH DE
    CALL bdos_close                                 ; close current extent
    POP  DE
    POP  BC
    LD   L, 3                                       ; Cannot close error #3
    LD   A, (CPM_VARS.bdos_aret)                    ; check status
    INC  A
    JP   Z, .bad_seek                               ; status != 0 - error
    ; set extent byte to FCB
    LD   HL, FCB_EXT
    ADD  HL, DE
    LD   (HL), C
    ; set S2 - extra extent byte
    LD   HL, FCB_S2
    ADD  HL, DE
    LD   (HL), B

    CALL open
    LD   A, (CPM_VARS.bdos_aret)
    INC  A
    JP   NZ, .seek_ok
    POP  BC
    PUSH BC
    LD   L, 0x4                                         ; Seek to unwritten extent #4
    INC  C
    JP   Z, .bad_seek
    CALL bdos_make
    LD   L, 0x5                                         ; Cannot create new extent #5
    LD   A, (CPM_VARS.bdos_aret)
    INC  A
    JP   Z, .bad_seek
.seek_ok:
    POP  BC
    XOR  A
    JP   bdos_ret_a
.bad_seek:
    PUSH HL
    CALL bdos_get_s2
    LD   (HL), 11000000b
    POP  HL
.seek_err:
    POP  BC
    LD   A,L
    LD   (CPM_VARS.bdos_aret), A
    JP   bdos_set_fwf

; -------------------------------------------------------
; Random disk read operation
; -------------------------------------------------------
bdos_rand_disk_read:
    LD   C, 0xff                                    ; mode
    CALL bdos_rseek                                 ; positioning
    CALL Z, bdos_disk_read                          ; read if ok
    RET

; -------------------------------------------------------
; Random disk write operation
; -------------------------------------------------------
bdos_rand_disk_write:
    LD   C, 0                                       ; mode
    CALL bdos_rseek                                 ; positioning
    CALL Z, bdos_disk_write                         ; read if ok
    RET

; -------------------------------------------------------
; Compute random record position
; Inp: HL -> FCB
;      DE - relative location of record number
; Out: C - r0 byte
;      B - r1 byte
;      A - r2 byte
;      ZF set - Ok, reset - overflow
; -------------------------------------------------------
bdos_compute_rr:
    EX   DE, HL                                     ; DE -> FCB
    ADD  HL, DE                                     ; HL = FCB+DE
    LD   C, (HL)                                    ; get record number
    LD   B, 0                                       ; in BC
    LD   HL, FCB_EXT
    ADD  HL, DE
    LD   A, (HL)                                    ; A = extent
    ; calculate BC = recnum + extent*128
    RRCA                                            ; A[0] -> A[7]
    AND  0x80                                       ; ignore other bits
    ADD  A, C                                       ; add to record number
    LD   C, A                                       ; C=r0
    LD   A, 0
    ADC  A, B
    LD   B, A                                       ; B=r1
    ;
    LD   A, (HL)                                    ; A = extent
    RRCA
    AND  0x0f                                       ; A = EXT[4:1]
    ADD  A, B                                       ; add to r1 byte
    LD   B, A

    LD   HL, FCB_S2
    ADD  HL, DE
    LD   A, (HL)                                    ; A = extra extent bits
    ADD  A, A                                       ; *2
    ADD  A, A                                       ; *4
    ADD  A, A                                       ; *8
    ADD  A, A                                       ; *16
    PUSH AF                                         ; save flags (need only CF)
    ADD  A, B                                       ; add to r1
    LD   B, A
    PUSH AF                                         ; save flags
    POP  HL                                         ; flags on L
    LD   A, L                                       ; A bit 0 - CF
    POP  HL
    OR   L                                          ; merge both carry flags
    AND  0x1                                        ; mask only CF
    RET

; -------------------------------------------------------
; Compute logical file size for current FCB.
; Setup r0, r1, r2 bytes - maximum record number
; -------------------------------------------------------
bdos_get_file_size:
    LD   C, FN_LEN
    CALL bdos_search                                ; get first dir record (first extent)
    LD   HL, (CPM_VARS.bdos_info)                   ; HL -> FCB
    ; zeroing r0, r1, r2
    LD   DE, FCB_RN                                 ; D=0
    ADD  HL, DE
    PUSH HL
    LD   (HL), D
    INC  HL
    LD   (HL), D
    INC  HL
    LD   (HL), D
    ;
.get_extent:
    CALL bdos_end_of_dir
    JP   Z, .done                                   ; if not more extents found, return
    CALL bdos_getdptra                              ; HL -> FCB
    LD   DE, FCB_RC
    CALL bdos_compute_rr                            ; compute random access parameters
    POP  HL
    PUSH HL
    ; Now let's compare these values ​​with those indicated
    LD   E, A
    LD   A, C
    SUB  (HL)
    INC  HL
    LD   A, B
    SBC  A, (HL)
    INC  HL
    LD   A, E
    SBC  A, (HL)
    JP   C, .less_size
    ; found larger extent (size), save it to fcb
    LD   (HL), E
    DEC  HL
    LD   (HL), B
    DEC  HL
    LD   (HL), C
.less_size:
    CALL bdos_search_nxt
    JP   .get_extent
.done:
    POP  HL
    RET

; -------------------------------------------------------
; Set the random record count bytes of the FCB to the number
; of the last record read/written by the sequential I/O calls.
; -------------------------------------------------------
bdos_set_random:
    LD   HL, (CPM_VARS.bdos_info)                   ; HL -> FCB
    LD   DE, FCB_CR                                 ; Current record within extent
    CALL bdos_compute_rr                            ; Compute random access parameters
    LD   HL, FCB_RN
    ADD  HL, DE                                     ; HL -> Random access record number
    ; set r0, r1, r2
    LD   (HL), C
    INC  HL
    LD   (HL), B
    INC  HL
    LD   (HL), A
    RET

; -------------------------------------------------------
; Select disk to bdos_curdsk for subsequent input or
; output ops
; -------------------------------------------------------
bdos_select:
    LD   HL, (CPM_VARS.bdos_dlog)                   ; login vector
    LD   A, (CPM_VARS.bdos_curdsk)                  ; current drive
    LD   C, A
    CALL bdos_hlrotr                                ; shift active bit for this drive
    PUSH HL
    EX   DE, HL
    CALL bdos_sel_dsk                               ; select drive
    POP  HL
    CALL Z, bdos_sel_error                          ; check for error drive
    ; new active drive?
    LD   A, L
    RRA
    RET  C                                          ; no, return
    ; yes, update login vector
    LD   HL, (CPM_VARS.bdos_dlog)
    LD   C, L
    LD   B, H
    CALL bdos_set_dsk_bit                           ; set bits in vector
    LD   (CPM_VARS.bdos_dlog), HL                   ; store new vector

    JP   bdos_initialize

; -------------------------------------------------------
; Select disc
; Inp: C=0x0E
;      E=drive number 0-A, 1-B .. 15-P
; Out: L=A=0 - ok or 0xFF - error
; -------------------------------------------------------
bdos_select_disk:
    ; check disk change
    LD   A, (CPM_VARS.bdos_linfo)
    LD   HL, CPM_VARS.bdos_curdsk
    CP   (HL)
    RET  Z                                          ; is same, return
    ; login new disk
    LD   (HL), A
    JP   bdos_select

; -------------------------------------------------------
; Auto select disk by FCB_DR
; -------------------------------------------------------
bdos_reselect:
    LD   A, TRUE
    LD   (CPM_VARS.bdos_resel), A                   ; set flag
    LD   HL, (CPM_VARS.bdos_info)                   ; HL -> FCB
    LD   A, (HL)                                    ; get specified disk
    AND  0x1f                                       ; A = A[4:0]
    DEC  A                                          ; adjust for 0-A, 1-B and so on
    LD   (CPM_VARS.bdos_linfo), A                   ; set as parameter
    CP   0x1e                                       ; no change?
    JP   NC, .no_select
    ; change, but save current to old
    LD   A, (CPM_VARS.bdos_curdsk)
    LD   (CPM_VARS.bdos_olddsk), A
    ; save FCB_DR byte
    LD   A, (HL)
    LD   (CPM_VARS.bdos_fcbdsk), A
    ; clear FCB_DR[4:0] bits - user code
    AND  11100000b
    LD   (HL), A
    CALL bdos_select_disk

.no_select:
    ; set user to FCB_DR low bits
    LD   A, (CPM_VARS.bdos_userno)
    LD   HL, (CPM_VARS.bdos_info)
    OR   (HL)
    LD   (HL), A
    RET

; -------------------------------------------------------
; Return version number
; -------------------------------------------------------
bdos_get_version:
    LD   A, CPM_VERSION                                   ; 0x22 - v2.2
    JP   bdos_ret_a

; -------------------------------------------------------
; Reset disk system - initialize to disk 0
; -------------------------------------------------------
bdos_reset_disks:
    LD   HL, 0x00
    LD   (CPM_VARS.bdos_rodsk), HL                      ; clear ro flags
    LD   (CPM_VARS.bdos_dlog), HL                       ; clear login vector
    XOR  A                                              ; disk 0
    LD   (CPM_VARS.bdos_curdsk), A                      ; set disk 'A' as current
    LD   HL, dma_buffer                                 ; set 'DMA' buffer address to default
    LD   (CPM_VARS.bdos_dmaad), HL
    CALL bdos_setdata
    JP   bdos_select                                    ; select drive 'A'

; -------------------------------------------------------
; Open file
; Inp: DE -> FCB
; Out: BA and HL - error.
; -------------------------------------------------------
bdos_open_file:
    CALL bdos_clear_s2
    CALL bdos_reselect
    JP   open

; -------------------------------------------------------
; Close file
; Inp: DE -> FCB
; Out: BA and HL - error.
; -------------------------------------------------------
bdos_close_file:
    CALL bdos_reselect
    JP   bdos_close

; -------------------------------------------------------
; Search for first occurrence of a file in directory
; Inp: DE -> FCB
; Out: BA and HL - error.
; -------------------------------------------------------
bdos_search_first:
    LD   C, 0x00                                    ; special search
    EX   DE, HL
    LD   A, (HL)
    CP   '?'                                        ; '?' is first byte?
    JP   Z, .qselect                                ; return first matched entry

    CALL bdos_getexta                               ; get extent byte from FCB
    LD   A, (HL)
    CP   '?'                                        ; it is '?'
    CALL NZ, bdos_clear_s2                          ; set S2 to 0 if not
    CALL bdos_reselect                              ; select disk by FCB
    LD   C, FCB_INFO_LEN                            ; match 1first 15 bytes
.qselect:
    CALL bdos_search                                ; search first entry
    JP   bdos_dir_to_user                           ; move entry to user space

; -------------------------------------------------------
; Search for next occurrence of a file name
; Out: BA and HL - error.
; -------------------------------------------------------
bdos_search_next:
    LD   HL, (CPM_VARS.bdos_searcha)                ; restore FCB address
    LD   (CPM_VARS.bdos_info), HL                   ; set as parameter
    CALL bdos_reselect                              ; select drive by FCB
    CALL bdos_search_nxt                            ; search next
    JP   bdos_dir_to_user                           ; move entry to user space

; -------------------------------------------------------
; Remove directory
; Inp: DE -> FCB
; Out: BA and HL - error.
; -------------------------------------------------------
bdos_rm_file:
    CALL bdos_reselect                              ; Select drive by FCB
    CALL bdos_era_file                              ; Delete file
    JP   bdos_ret_dirloc                            ; Return directory location in A

; -------------------------------------------------------
; Read next 128b record
; Inp: DE -> FCB
; Out: BA and HL - error.
; A=0 - Ok,
; 1 - end of file,
; 9 - invalid FCB,
; 10 - media changed,
; 0FFh - hardware error.
; -------------------------------------------------------
bdos_read_file:
    CALL bdos_reselect                              ; select drive
    JP   bdos_seq_disk_read                         ; and read

; -------------------------------------------------------
; Write next 128b record
; Inp: DE -> FCB
; Out: BA and HL - error.
; A=0 - Ok,
; 1 - directory full,
; 2 - disc full,
; 9 - invalid FCB,
; 10 - media changed,
; 0FFh - hardware error.
; -------------------------------------------------------
bdos_write_file:
    CALL bdos_reselect                              ; select drive
    JP   bdos_seq_disk_write                        ; and write

; -------------------------------------------------------
; Create file
; Inp: DE -> FCB.
; Out: Error in BA and HL
; A=0 - Ok,
; 0FFh - directory is full.
; -------------------------------------------------------
bdos_make_file:
    CALL bdos_clear_s2                              ; clear S2 byte
    CALL bdos_reselect                              ; select drive
    JP   bdos_make                                  ; and make file

; -------------------------------------------------------
; Rename file. New name, stored at FCB+16
; Inp: DE -> FCB.
; Out: Error in BA and HL
; A=0-3 if successful;
; A=0FFh if error.
; -------------------------------------------------------
bdos_ren_file:
    CALL bdos_reselect                              ; select drive
    CALL bdos_rename                                ; rename file
    JP   bdos_ret_dirloc                            ; Return directory location in A

; -------------------------------------------------------
; Return bitmap of logged-in drives
; Out: bitmap in HL.
; -------------------------------------------------------
bdos_get_login_vec:
    LD   HL, (CPM_VARS.bdos_dlog)
    JP   sthl_ret

; -------------------------------------------------------
; Return current drive
; Out: A - currently selected drive. 0 => A:, 1 => B: etc.
; -------------------------------------------------------
bdos_get_cur_drive:
    LD   A, (CPM_VARS.bdos_curdsk)
    JP   bdos_ret_a

; -------------------------------------------------------
; Set DMA address
; Inp: DE - address of DMA buffer
; -------------------------------------------------------
bdos_set_dma_addr:
    EX   DE, HL
    LD   (CPM_VARS.bdos_dmaad), HL
    JP   bdos_setdata

; -------------------------------------------------------
; fn27: Return the address of cur disk allocation map
; Out: HL - address
; -------------------------------------------------------
bdos_get_alloc_addr:
    LD   HL, (CPM_VARS.bdos_alloca)
    JP   sthl_ret

; -------------------------------------------------------
; Get write protection status
; -------------------------------------------------------
bdos_get_wr_protect:
    LD   HL, (CPM_VARS.bdos_rodsk)
    JP   sthl_ret

; -------------------------------------------------------
; Set file attributes (ro, system)
; -------------------------------------------------------
bdos_set_attr:
    CALL bdos_reselect
    CALL bdos_update_attrs
    JP   bdos_ret_dirloc

; -------------------------------------------------------
; Return address of disk parameter block of the
; current drive
; -------------------------------------------------------
bdos_get_dpb:
    LD   HL, (CPM_VARS.bdos_dpbaddr)

; -------------------------------------------------------
; Common code to return Value from BDOS functions
; -------------------------------------------------------
sthl_ret:
    LD   (CPM_VARS.bdos_aret), HL
    RET

; -------------------------------------------------------
; Get/set user number
; Inp: E - 0..15 - set user. 0xFF - get user
; Out: A - returns user number
; -------------------------------------------------------
bdos_set_user:
    LD   A, (CPM_VARS.bdos_linfo)
    CP   0xff
    JP   NZ, .set_user
    LD   A, (CPM_VARS.bdos_userno)
    JP   bdos_ret_a
.set_user:
    AND  0x1f                                      ; mask for 0..31  but will be < 16
    LD   (CPM_VARS.bdos_userno), A
    RET

; -------------------------------------------------------
; Random read. Record specified in the random record
; count area of the FCB, at the DMA address
; Inp: DE -> FCB
; Out: Error codes in BA and HL.
; -------------------------------------------------------
bdos_rand_read:
    CALL bdos_reselect                              ; select drive by FCB
    JP   bdos_rand_disk_read                        ; and read

; -------------------------------------------------------
; Random access write record.
; Record specified in the random record count area of the FCB, at the DMA address
; Inp: DE -> FCB
; Out: Error codes in BA and HL.
; -------------------------------------------------------
bdos_rand_write:
    CALL bdos_reselect                              ; select drive by FCB
    JP   bdos_rand_disk_write                       ; and write

; -------------------------------------------------------
; Compute file size.
; Set the random record count bytes of the FCB to the
; number of 128-byte records in the file.
; -------------------------------------------------------
bdos_compute_fs:
    CALL bdos_reselect                              ; select drive by FCB
    JP   bdos_get_file_size                         ; and get file size

; -------------------------------------------------------
; Selectively logoff (reset) disc drives
; Out: A=0 - Ok, 0xff if error
; -------------------------------------------------------
bdos_reset_drives:
    LD   HL, (CPM_VARS.bdos_info)                   ; HL - drives to logoff map
    LD   A, L
    CPL
    LD   E, A
    LD   A, H
    CPL
    LD   HL, (CPM_VARS.bdos_dlog)                   ; get vector
    AND  H                                          ; reset in hi byte
    LD   D, A                                       ; and store in D
    LD   A, L                                       ; reset low byte
    AND  E
    LD   E, A                                       ; and store in E
    LD   HL, (CPM_VARS.bdos_rodsk)                  ; HL - disk RO map
    EX   DE, HL
    LD   (CPM_VARS.bdos_dlog), HL                   ; store new login vector
    ; reset RO flags
    LD   A, L
    AND  E
    LD   L, A
    LD   A, H
    AND  D
    LD   H, A
    ; and store new value
    LD   (CPM_VARS.bdos_rodsk), HL
    RET

; -------------------------------------------------------
; Reach this point at the end of processing
; to return the data to the user.
; -------------------------------------------------------
bdos_goback:
    LD   A, (CPM_VARS.bdos_resel)                   ; check for selection drive by user FCB
    OR   A
    JP   Z, bdos_ret_mon                            ; return if not selected

    LD   HL, (CPM_VARS.bdos_info)                   ; HL -> FCB_DR
    LD   (HL), 0
    LD   A, (CPM_VARS.bdos_fcbdsk)
    OR   A                                          ; restore drive
    JP   Z, bdos_ret_mon                            ; return if default
    LD   (HL), A                                    ; restore
    LD   A, (CPM_VARS.bdos_olddsk)                  ; previous drive
    LD   (CPM_VARS.bdos_linfo), A                   ; set parameter
    CALL bdos_select_disk                           ; select previous drive

; -------------------------------------------------------
; Return from the disk monitor
; -------------------------------------------------------
bdos_ret_mon:
    LD   HL, (CPM_VARS.bdos_usersp)                 ; return user stack
    LD   SP, HL
    LD   HL, (CPM_VARS.bdos_aret)                   ; get return value
    LD   A, L                                       ; ver 1.4 CP/M compatibility
    LD   B, H
    RET

; -------------------------------------------------------
; Random disk write with zero fill of
; unallocated block
; -------------------------------------------------------
bdos_rand_write_z:
    CALL bdos_reselect                              ; select drive
    LD   A, 2                                       ; special write mode
    LD   (CPM_VARS.bdos_seqio), A
    LD   C, FALSE                                   ; write indicator
    CALL bdos_rseek1                                ; seek position in a file
    CALL Z, bdos_disk_write                         ; write if no errors
    RET

; -------------------------------------------------------
; Unuser initialized data
; -------------------------------------------------------
filler:
    DB   0xF1, 0xE1                                 ; POP AF, POP HL

; -------------------------------------------------------
; Filler to align blocks in ROM
; -------------------------------------------------------
LAST      EQU $
CODE_SIZE EQU LAST-0xC800
FILL_SIZE EQU 0xE00-CODE_SIZE

    DISPLAY "| BDOS\t| ",/H,bdos_start,"  | ",/H,CODE_SIZE," | ",/H,FILL_SIZE," |"

FILLER
    DS    FILL_SIZE, 0xFF

    ENDMODULE

    IFNDEF    BUILD_ROM
        OUTEND
    ENDIF
