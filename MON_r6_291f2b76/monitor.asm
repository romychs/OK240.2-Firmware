; ======================================================
; Ocean-240.2
; Monitor r7
; crc32: 93bd95db
;
; Disassembled by Romych 2026-02-16
; ======================================================

	DEVICE NOSLOT64K

	INCLUDE	"io.inc"
	INCLUDE "equates.inc"
	INCLUDE "ram.inc"
	INCLUDE "bios_entries.inc"

	OUTPUT mon_E000.bin


	MODULE	MONITOR

	ORG		0xe000

; ------------------------------------------------------
; Monitor Entry points
; ------------------------------------------------------

mon_start:          JP  m_start                     ; E000
mon_cold_start:     JP  m_cold_start                ; E003
mon_con_status:		JP	m_con_status				; E006
mon_con_in:			JP	m_con_in					; E009
mon_con_out:		JP	m_con_out					; E00C
mon_serial_in:		JP	m_serial_in					; E00F
mon_serial_out:		JP	m_serial_out				; E012
mon_char_print:		JP	m_char_print				; E015
mon_tape_read:		JP	m_tape_read					; E018
mon_tape_write:		JP	m_tape_write				; E01B
mon_ram_disk_read:	JP	m_ramdisk_read				; E01E
mon_ram_disk_write:	JP	m_ramdisk_write				; E021
mon_tape_read_ram:  JP  m_tape_read_ram             ; E024
mon_tape_write_ram: JP  m_tape_write_ram            ; E027
mon_tape_wait:		JP	m_tape_wait					; E02A
mon_tape_detect:	JP	m_tape_blk_detect			; E02D
mon_read_floppy:	JP	m_read_floppy				; E030
mon_write_floppy:	JP	m_write_floppy				; E033


; ------------------------------------------------------
; Init system devices
; ------------------------------------------------------
m_start:
    DI
    LD   A, 10000000b                               ; DD17 all ports to out
    OUT  (SYS_DD17CTR), A                           ; VV55 Sys CTR
    OUT  (DD67CTR), A                               ; VV55 Video CTR

    ; init_kbd_tape
    LD   A, 010010011b
    OUT  (KBD_DD78CTR), A

    LD   A, 01111111b                               ; VSU=0, C/M=1, FL=111, COL=111
    OUT  (VID_DD67PB), A                            ; color mode
    LD   A, 00000001b
    OUT  (SYS_DD17PB), A                            ; Access to VRAM
    LD   B, 0x0                                     ; TODO: replace to LD HL, 0x3f00   LD B,L
    LD   HL, 0x3f00
    LD   A, H
    ADD  A, 0x41                                    ; A=128 0x80

    ; Clear memory from 0x3F00 to 0x7FFF
.fill_video:
    LD   (HL), B
    INC  HL
    CP   H
    JP   NZ, .fill_video

    ;XOR  A
    LD   A, 0
    OUT  (SYS_DD17PB), A                            ; Disable VRAM
    LD   A, 00000111b
    OUT  (SYS_DD17PC), A                            ; pix shift to 7
    LD   (M_VARS.pix_shift), A

    XOR  A
    LD   (M_VARS.screen_mode), A
    LD   (M_VARS.row_shift), A

    ; Set color mode and palette
    LD   (M_VARS.curr_color+1), A
    CPL
    LD   (M_VARS.curr_color), A
    LD   A, 00000011b
    LD   (M_VARS.cur_palette), A
    ; VSU=0, C/M=1, FL=000, COL=011
    ; color mode, black border
    ; 00-black, 01-red, 10-purple, 11-white
    LD   A, 01000011b
    OUT  (VID_DD67PB), A

    ; config LPT
    LD   A, 0x4
    OUT  (DD67PC), A                                ; bell=1, strobe=0
    LD   (M_VARS.strobe_state), A                   ; store strobe
    LD   HL, 1024                                   ; 683us
    LD   (M_VARS.beep_period), HL
    LD   HL, 320                                    ; 213us
    LD   (M_VARS.beep_duration), HL

conf_uart:
    ; Config UART
    LD   A, 11001110b
    OUT  (UART_DD72RR), A
    LD   A, 00100101b
    OUT  (UART_DD72RR), A

    ; Config Timer#1 for UART clock
    LD   A, 01110110b                               ; tmr#1, load l+m bin, sq wave
    OUT  (TMR_DD70CTR), A

    ; 1.5M/20 = 75kHz
    LD   A, 20
    OUT  (TMR_DD70C2), A
    XOR  A
    OUT  (TMR_DD70C2), A
conf_pic:
    ; Config PIC
    LD   A,00010010b                                ; ICW1 edge trigger, interval 8, sin...
    OUT  (PIC_DD75RS), A
    XOR  A
    OUT  (PIC_DD75RM), A                            ; ICW2
    CPL
    OUT  (PIC_DD75RM), A                            ; ICW3 no slave
    LD   A,00100000b
    OUT  (PIC_DD75RS), A                            ; Non-specific EOI command, End of I...
    LD   A, PIC_POLL_MODE
    OUT  (PIC_DD75RS), A                            ; Poll mode, poll on next RD
    ; Config KBD
    LD   A, 0x80
    OUT  (KBD_DD78PC), A                            ; TODO: - Check using this 7th bit
    NOP
    NOP
    XOR  A
    OUT  (KBD_DD78PC), A

    ; Init cursor
    LD   SP, M_VARS.stor_de
    CALL m_draw_cursor


    ; Init BIOS entry
    LD   HL, M_VARS.stor_sp
    LD   (M_VARS.stack_0), HL

    LD   A, JP_OPCODE
    LD   (RST1), A

    LD   HL, m_rst1_handler
    LD   (RST1_handler_addr), HL

    ; Beep
    LD   C, ASCII_BELL
    CALL m_con_out
jump_bios:
    ; If CP/M BIOS exists, jump to it
    LD   A, (BIOS.boot_f)
    CP   JP_OPCODE
    JP   Z, BIOS.boot_f

    ; No CP/M, Go to monitor
    LD   HL, msg_hw_mon
    CALL m_out_strz
    JP   m_cold_start

; --------------------------------------------------
; Output ASCIIZ string
; Inp: HL -> string
; --------------------------------------------------
m_out_strz:
    LD	 C, (HL)
    LD	 A, C
    OR	 A
    RET	 Z
    CALL m_con_out
    INC	 HL
	JP	 m_out_strz

msg_hw_mon:
    DB  "\r\n240/7 MONITOR\r\n", 0

; ---------------------------------------------------
reset_m_stack:
    LD   HL,M_VARS.stor_e
    LD   SP,HL
    CALL get_cmd_letter
    INC  HL

; ---------------------------------------------------
; Monitor entry point
; ---------------------------------------------------
m_cold_start:
    LD   HL,M_VARS.stor_e
    LD   SP,HL
    DI
    CALL get_a_ref_sp
    CALL get_cmd_letter
    LD   L,205
    DB   0xdd
    PUSH AF
    CP   ASCII_CR
    JP   Z,m_cold_start
    LD   HL,cmd_handlers
    LD   C,11

seek_cmd:
    CP   (HL)
    JP   Z,cmd_found
    INC  HL
    INC  HL
    INC  HL
    DEC  C
    JP   NZ,seek_cmd
    JP   reset_m_stack

cmd_found:
    INC  HL
    ; get cmd handler address
    LD   A,(HL)
    INC  HL
    LD   H,(HL)
    LD   L,A
    ; RET to cold_start
    LD   DE,m_cold_start
    PUSH DE
    ; Jump to cmd handler
    LD   C,2
    JP   (HL)

; ---------------------------------------------------
; Monitor commands handlers table
; ---------------------------------------------------
cmd_handlers:
    DB   'D'
    DW   cmd_D
    DB   'F'
    DW   cmd_F
    DB   'G'
    DW   cmd_G
    DB   'L'
    DW   cmd_L
    DB   'M'
    DW   cmd_M
    DB   'R'
    DW   cmd_R
    DB   'S'
    DW   cmd_S
    DB   'W'
    DW   cmd_W
    DB   'X'
    DW   cmd_X
    DB   'B'
    DW   cmd_B
    DB   'A'
    DW   cmd_A

; ---------------------------------------------------
; B[n] - Move block n from RAM2 to RAM1
; ---------------------------------------------------
cmd_B
    DEC  C
    CALL get_address
    POP  DE
    LD   HL, tpa_start
    CALL m_ramdisk_read
    RET

; ---------------------------------------------------
; A[n] - Move block n from RAM1 to RAM2
; ---------------------------------------------------
cmd_A:
    DEC  C
    CALL get_address
    POP  DE
    LD   HL,256
    CALL m_ramdisk_write
    RET

; ---------------------------------------------------
; D[addr] Dump 128 bytes
; ---------------------------------------------------
cmd_D:
    DEC  C
    CALL get_address
    POP  HL
    LD   DE,127
    EX   DE,HL
    ADD  HL,DE
    EX   DE,HL
d_next_line:
    CALL get_a_ref_sp
    CALL out_sp_key
    CALL out_hex_word
d_next_byte:
    CALL out_sp_key
    LD   A,(HL)
    CALL out_hex_byte
    CALL next_hl_de
    RET  C
    LD   A,L
    AND  0x7
    JP   NZ,d_next_byte
    JP   d_next_line

; ---------------------------------------------------
; F[addr1][addr2]val Fill RAM ares
; ---------------------------------------------------
cmd_F:
    INC  C
    CALL get_address
    POP  BC
    POP  DE
    POP  HL
f_next_b:
    LD   (HL),C
    CALL next_hl_de
    JP   NC,f_next_b
    RET

; ---------------------------------------------------
; M[addr1][addr2][addr3] - Move mem blk ad
; ---------------------------------------------------
cmd_M:
    INC  C
    CALL get_address
    POP  BC
    POP  DE
    POP  HL
m_next_b:
    LD   A,(HL)
    LD   (BC),A
    INC  BC
    CALL next_hl_de
    JP   NC,m_next_b
    RET

; ---------------------------------------------------
; L - Load Intel HEX from serial interface
; ---------------------------------------------------
cmd_L:
    CALL get_key_with_check
    JP   NC,reset_m_stack
    CALL get_a_ref_sp

    ; wait for hex line start
l_wait_colon:
    CALL m_serial_in
    CP   ':'
    JP   NZ,l_wait_colon
    XOR  A
    LD   D,A
    CALL read_hex_serial
    JP   Z,l_exit
    ; read line len and addr
    LD   E,A
    CALL read_hex_serial
    LD   H,A
    CALL read_hex_serial
    LD   L,A
    CALL read_hex_serial
    LD   C,E
l_next_byte:
    CALL read_hex_serial
    LD   (HL),A
    INC  HL
    DEC  E
    JP   NZ,l_next_byte
    CALL read_hex_serial
    JP   NZ,reset_m_stack
    JP   l_wait_colon
    ; read and ignore final 4 hex bytes in las
l_exit:
    CALL read_hex_serial
    CALL read_hex_serial
    CALL read_hex_serial
    CALL read_hex_serial
    JP   NZ,reset_m_stack
    RET

; ---------------------------------------------------
; S[addr] Out and modify ram byte
; ---------------------------------------------------
cmd_S:
    CALL s_get_hex_addr
    RET  C
s_next_byte:
    LD   A,(HL)
    CALL out_hex_byte
    CALL get_cmd_letter
    DEC  L
    ; get new value
    CALL get_key_with_check
    RET  C
    JP   Z,s_next_in
    EX   DE,HL
    CALL hex_keys_to_addr
    EX   DE,HL
    LD   (HL),E
    RET  C
s_next_in:
    INC  HL
    JP   s_next_byte

; ---------------------------------------------------
; X[r] - Out and modify register value
; ---------------------------------------------------
cmd_X:
    LD   HL,registers_tab
    CALL get_key_with_check
    JP   C,x_ret
    LD   C,9
x_seek_reg:
    CP   (HL)
    JP   Z,x_reg_found
    INC  HL
    INC  HL
    INC  HL
    DEC  C
    JP   NZ,x_seek_reg
    JP   reset_m_stack
x_reg_found:
    CALL out_sp_key
    CALL out_reg_value
    CALL get_cmd_letter
    DEC  L
    CALL get_key_with_check
    RET  C
    JP   Z,reset_m_stack
    PUSH BC
    ; get new reg value and store
    CALL hex_keys_to_addr
    LD   A,L
    LD   (DE),A
    POP  AF
    OR   A
    JP   M,x_wreg
    INC  DE
    LD   A,H
    LD   (DE),A
x_wreg:
    CALL get_a_ref_sp
    RET
x_ret:
    CALL get_a_ref_sp
x_next_reg:
    XOR  A
    OR   (HL)
    RET  M
    CP   'M'
    CALL Z,get_a_ref_sp
    CALL out_sp_key
    LD   C,(HL)
    CALL get_key_out
    CALL get_cmd_letter
    DEC  A
    CALL out_reg_value
    JP   x_next_reg

; ---------------------------------------------------
registers_tab:
    DB   'A', LOW(M_VARS.stor_a), 0
    DB   'B', LOW(M_VARS.stor_b), 0
    DB   'C', LOW(M_VARS.stor_c), 0
    DB   'D', LOW(M_VARS.stor_d), 0
    DB   'E', LOW(M_VARS.stor_e), 0
    DB   'F', LOW(M_VARS.stor_f), 0
    DB   'M', LOW(M_VARS.stor_m), 1
    DB   'P', LOW(M_VARS.stor_p), 1
    DB   'S', LOW(M_VARS.stor_s), 1
    DB   0xff

; ------------------------------------------------------
;  Console status
;  Out: A = 0 - not ready
;       A = 0xFF - ready (key pressed)
; ------------------------------------------------------
m_con_status:
    IN   A, (PIC_DD75RS)                            ; Read PIC status
    NOP
    AND  KBD_IRQ                                    ; Check keyboard request RST1
    LD   A, 0
    RET  Z                                          ; no key pressed
    CPL
    RET                                             ; key pressed

; ------------------------------------------------------
;  Wait and read data from UART
;  Out: A - 7 bit data
; ------------------------------------------------------
m_serial_in:
    IN   A, (UART_DD72RR)
    AND  RX_READY
    JP   Z, m_serial_in                             ; wait for rx data ready
    IN   A, (UART_DD72RD)
    AND  0x7f                                       ; leave 7 bits
    RET

; ------------------------------------------------------
;  Read key
;  Out: A
; ------------------------------------------------------
m_con_in:
    CALL m_con_status
    OR   A
    JP   Z, m_con_in                                ; wait key
    IN   A, (KBD_DD78PA)                            ; get key
    AND  0x7f                                       ; reset hi bit, leave 0..127 code
    PUSH AF
    ; TODO: Check if it is keyboard ACK
    ; PC7 Set Hi (ACK?)
    LD   A, 0x80
    OUT  (KBD_DD78PC), A
    ; PC7 Set Lo
    XOR  A
    OUT  (KBD_DD78PC), A
    POP  AF
    RET

; ------------------------------------------------------
;  Send data by UART
;  Inp: C - data to transmitt
; ------------------------------------------------------
m_serial_out:
    IN   A, (UART_DD72RR)
    AND  TX_READY
    JP   Z, m_serial_out                             ; Wait for TX ready
    LD   A, C
    OUT  (UART_DD72RD), A
    RET

; ------------------------------------------------------
;
; ------------------------------------------------------
read_key_if_pressed:
    CALL m_con_status
    OR   A
    RET  Z                                          ; ret if no key pressed
    JP   handle_key_pressed

; ------------------------------------------------------
;
; ------------------------------------------------------
get_a_ref_sp:
    CALL get_cmd_letter
    DEC  C
    CALL get_cmd_letter
    LD   A,(BC)
    RET

; ------------------------------------------------------
;
; ------------------------------------------------------
get_cmd_letter:
    EX   (SP),HL
    LD   C,(HL)
    INC  HL
    EX   (SP),HL
    JP   get_key_out

; ------------------------------------------------------
;  Send character to printer
;  Inp: C - character
; ------------------------------------------------------
m_char_print:
    ; wait printer ready
    IN   A, (PIC_DD75RS)
    AND  PRINTER_IRQ
    JP   Z, m_char_print

    LD   A, C
   ;NOP
    OUT  (LPT_DD67PA), A
    ; set LP strobe
    LD   A, 00010100b
    OUT  (DD67PC),A

.wait_lp:
    ; wait printer ack
    IN   A, (PIC_DD75RS)
    AND  PRINTER_IRQ
    JP   NZ, .wait_lp
    ; remove LP strobe
    LD   A, 00000100b
    OUT  (DD67PC), A
    RET


; ------------------------------------------------------
;
; ------------------------------------------------------
out_sp_key:
    LD   C, ASCII_SP

; ------------------------------------------------------
;
; ------------------------------------------------------
get_key_out:
    CALL read_key_if_pressed


; ------------------------------------------------------
;  Out char to console
;  Inp: C - char
; ------------------------------------------------------
m_con_out:
    PUSH HL
    PUSH DE
    PUSH BC
    CALL m_con_out_int
    POP  BC
    POP  DE
    POP  HL
    RET

; ------------------------------------------------------
; Out char C to console
; ------------------------------------------------------
m_con_out_int:
    LD   DE, M_VARS.esc_mode
    LD   A, (DE)
    DEC  A
    OR   A                                          ; TODO: unused (save 1b 4t)
    JP   M, m_print_no_esc                          ; esc_mode=0 - standart print no ESC mode
    JP   NZ, m_print_at_xy                          ; esc_mode=2 (graphics)

    ; handle ESC param (esc_mode=1)
    INC  DE                                         ; TODO: replace to INC E  E=0xd3 save 2t
    LD   A, (DE)
    OR   A
    JP   P, get_esc_param
    LD   A, C
    AND  0xf                                        ; convert char to command code
    LD   (DE), A
    INC  DE                                         ; TODO: replace to INC E  E=0xd3 save 2t
    XOR  A
    LD   (DE), A
    RET

get_esc_param:
    LD   HL, M_VARS.esc_cmd
    LD   B, (HL)
    INC  HL
    LD   A, (HL)
    INC  A
    LD   (HL), A
    LD   E, A
    LD   D, 0x0
    ADD  HL, DE
    LD   (HL), C
    LD   HL, esc_params_tab
    LD   E, B
    ADD  HL, DE
    CP   (HL)
    RET  M
    ; -----------------
    LD   A, (M_VARS.esc_cmd)
    AND  0xf
    CP   0xf
    JP   Z, .l1
    CP   0xb
    LD   C, 0x5
    JP   Z, .l2
    CP   0x4
    JP   P, .l6

.l1:
    LD   C, 0x4

.l2:
    LD   HL, M_VARS.esc_param
    LD   D, H
    LD   E, L

.l3:
    LD   A, (HL)
    CP   ':'
    JP   M, .l4
    SUB  0x7

.l4:
    AND  0xf
    ADD  A, A
    ADD  A, A
    ADD  A, A
    ADD  A, A
    LD   B, A
    INC  HL
    LD   A, (HL)
    CP   ':'
    JP   M, .l5
    SUB  0x7

.l5:
    AND  0xf
    OR   B
    INC  HL
    LD   (DE), A
    INC  DE
    DEC  C
    JP   NZ, .l3

.l6:
    LD   HL, M_VARS.esc_cmd
    LD   A, (HL)
    AND  0xf
    LD   E, A
    DEC  HL
    OR   A
    LD   (HL), 0x2
    RET  Z
    LD   D, 0x0
    LD   (HL), D
    DEC  DE
    LD   HL, esc_handler_tab
    ADD  HL, DE
    ADD  HL, DE
    LD   E, (HL)
    INC  HL
    LD   D, (HL)
    EX   DE, HL
    CP   0x4
    JP   P, .no_draw_fn
    LD   A, (M_VARS.screen_mode)
    AND  0x7
    JP   NZ, esc_exit
.no_draw_fn:
    LD   DE, esc_exit
    PUSH DE
    JP   (HL)
esc_exit:
    XOR  A
    LD   (M_VARS.esc_mode), A
    RET


; --------------------------------------------------
; Count of parameters for ESC commands
; --------------------------------------------------
esc_params_tab:
    DB   4, 8, 8, 4, 1, 2, 1, 1
    DB   1, 1, 1, 10, 1, 1, 1, 8

esc_handler_tab:
    DW   esc_draw_fill_rect                         ;8 <ESC>1x1y1x2y2m
    DW   esc_draw_line                              ;8 <ESC>2x1y1x2y2
    DW   esc_draw_dot                               ;4 <ESC>3xxyy
    DW   esc_set_color                              ;1 <ESC>4N N=1..4
    DW   esc_set_cursor                             ;2 <ESC>5rc r-Row, c-Col
    DW   esc_set_vmode                              ;1 <ESC>6m  m-mode:
                                                    ; C  0   - 40x25 cursor on
                                                    ; M  1,2 - 64x25 cursor on
                                                    ; M  3   - 80x25 cursor on
                                                    ; C  4   - 40x25 cursor off
                                                    ; M  5,6 - 64x25 cursor off
                                                    ; M  7   - 80x25 cursor off
                                                    ; M  8   - 20rows mode
                                                    ;    9   - cursor off
                                                    ;    10  - cursor on
    DW   esc_set_charset                            ;1 <ESC>7n where n is:
                                                    ;    0 - LAT  Both cases
                                                    ;    1 - RUS  Both cases
                                                    ;    2 - LAT+RUS Upper case
    DW   esc_set_palette                            ;1 <ESC>8c c - Foreground+Backgound
    DW   esc_set_cursor2                            ;1 <ESC>9xy
    DW   esc_print_screen                           ;1 <ESC>:
    DW   esc_draw_circle                            ;10 <ESC>;xyraxay   X,Y, Radius, aspect ratio X, aspect ratio Y
    DW   esc_unimpl                                 ;1 <ESC><
    DW   esc_unimpl                                 ;1 <ESC>=
    DW   esc_unimpl                                 ;1 <ESC>>
    DW   esc_set_beep                               ;8 <ESC>?ppdd   pp-period (word), dd - duration (word)

; --------------------------------------------------
;
; --------------------------------------------------
esc_unimpl:
    RET

; --------------------------------------------------
;
; --------------------------------------------------
esc_set_beep:
    ; param byte 1+2 -> period
    LD   DE, M_VARS.esc_param
    LD   A, (DE)
    LD   H, A
    INC  DE
    LD   A, (DE)
    LD   L, A
    LD   (M_VARS.beep_period), HL
    ; param byte 3+4 -> duration
    INC  DE
    LD   A, (DE)
    LD   H, A
    INC  DE
    LD   A, (DE)
    LD   L, A
    LD   (M_VARS.beep_duration), HL
    RET

; --------------------------------------------------
;
; --------------------------------------------------
esc_set_cursor2:
    POP  DE
    XOR  A
    LD   (M_VARS.esc_mode), A
    LD   A, (M_VARS.screen_mode)
    RET

; --------------------------------------------------
;
; --------------------------------------------------
esc_print_screen:
    LD   A, (M_VARS.screen_mode)
    AND  00000111b
    RET  NZ                                         ; ret if not 0-3 mode
    LD   DE, 0x30ff
    CALL m_print_hor_line
    DEC  E
    LD   D, 0xf0

.chk_keys:
    CALL m_con_status
    OR   A
    JP   Z, .no_keys
    CALL m_con_in
    CP   ASCII_ESC
    RET  Z

.no_keys:
    CALL m_print_hor_line
    DEC  E
    JP   NZ, .chk_keys
    LD   D, 0xe0                                    ; 224d
    CALL m_print_hor_line
    RET

; ------------------------------------------------------
; Print line to printer
; D - width
; ------------------------------------------------------
m_print_hor_line:
    LD  HL, cmd_esc_set_X0

    ; Set printer X coordinate = 0
    CALL m_print_cmd
    LD   HL, 4
    LD   (M_VARS.ul_var0), HL                       ; Set start coord X = 4
    LD   B, 0x0                                     ; TODO: LD B, H  (save 1b 3t)

.print_next_col:
    LD   C, 0x0
    ; 1
    CALL m_get_7vpix
    AND  D
    CALL NZ, m_print_vert_7pix
    LD   HL, (M_VARS.ul_var0)
    INC  HL

    ; inc X
    LD   (M_VARS.ul_var0), HL
    LD   C, 0x1
    ; 2
    CALL m_get_7vpix
    AND  D
    CALL NZ, m_print_vert_7pix
    LD   HL, (M_VARS.ul_var0)
    INC  HL
    ; inc X
    LD   (M_VARS.ul_var0), HL
    INC  B
    LD   A, B
    CP   236
    JP   C, .print_next_col
    LD   HL, cmd_esc_inc_Y2
    CALL m_print_cmd
    RET

; ------------------------------------------------------
; Send command to printer
; Inp: HL -> command bytes array
; ------------------------------------------------------
m_print_cmd:
    PUSH BC
.print_nxt:
    LD   A, (HL)
    CP   ESC_CMD_END
    JP   Z, .cmd_end
    LD   C, A
    CALL m_char_print
    INC  HL
    JP   .print_nxt
.cmd_end:
    POP  BC
    RET

; ------------------------------------------------------
;  Print 7 vertical pixels to printer
;  Inp: A - value to print
; ------------------------------------------------------
m_print_vert_7pix:
    PUSH AF
    ; Set coordinate X to 0
    LD   HL, cmd_esc_set_X
    CALL m_print_cmd
    LD   HL, (M_VARS.ul_var0)
    LD   C,H
    CALL m_char_print
    LD   C,L
    CALL m_char_print
    ; Set column print mode
    LD   HL, cmd_esc_print_col
    CALL m_print_cmd
    POP  AF
    ; Print 7 vertical pixels
    LD   C, A
    CALL m_char_print
    RET

; ------------------------------------------------------
; Control codes for printer УВВПЧ-30-004
; ------------------------------------------------------
; <ESC>Zn - Increment Y coordinate
cmd_esc_inc_Y2:
    DB   ASCII_ESC
    DB   'Z'
    DB   2h
    DB   ESC_CMD_END

; <ESC>Xnn - Set X coordinate
cmd_esc_set_X0:
    DB   ASCII_ESC
    DB   'X'
    DB   0h                                          ; 0..479
    DB   0h
    DB   ESC_CMD_END

; ------------------------------------------------------
; <ESC>X - Start on "Set X coordinate" command
; ------------------------------------------------------
cmd_esc_set_X:
    DB   ASCII_ESC
    DB   'X'
    DB   ESC_CMD_END

; <ESC>O - Column print (vertical 7 bit)
cmd_esc_print_col:
    DB   ASCII_ESC
    DB   'O'
    DB   ESC_CMD_END

; ------------------------------------------------------
;  Get 7 vertical pixels from screen
;  Inp: C - sheet
;  Out: A - byte
; ------------------------------------------------------
m_get_7vpix:
    LD   A, (M_VARS.row_shift)
    ADD  A, B
    ADD  A, 19                                       ; skip first 20pix
    LD   L, A
    PUSH DE
    PUSH BC
    LD   A, E

.calc_pix_no:
    AND  0x7
    LD   B, A
    LD   A, E
    ; calc hi addr
    RRA                                             ; /8
    RRA
    RRA
    AND  0x1f
    ADD  A, A                                       ; *2
    ADD  A, 64                                      ; bytes per row
    LD   H, A
    ; select sheet 0|1
    LD   A, C
    AND  0x1
    ADD  A, H
    LD   H, A
    ; HL = pix addr, turn on VRAM access
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   E, (HL)                                    ; read pixel
    INC  H                                          ; HL += 512
    INC  H
    LD   D, (HL)                                    ; read pixel row+1

    ; turn off VRAM access
    ;v8 XOR A
    LD   A, 0
    OUT  (SYS_DD17PB), A
.for_all_pix:
    DEC  B
    JP   M, .all_shifted
    ; shift pixels D >> [CF] >> E
    LD   A, D
    RRA
    LD   D, A
    LD   A, E
    RRA
    LD   E, A
    JP   .for_all_pix
.all_shifted:
    LD   A, E
    LD   D, 0
    RRA
    JP   NC,.not_1_1
    LD   D,00110000b
.not_1_1:
    RRA
    JP   NC, .not_1_2
    LD   A, D
    OR   11000000b
    LD   D, A
.not_1_2:
    LD   A, D
    POP  BC
    POP  DE
    RET

; --------------------------------------------------
;
; --------------------------------------------------
esc_set_palette:
    LD  A, (M_VARS.esc_param)
    AND 00111111b                                   ; bgcol[2,1,0],pal[2,1,0]
    LD  (M_VARS.cur_palette), A
    LD  B, A
    LD  A, (M_VARS.screen_mode)
    AND 00000011b
    LD  A, 0x0
    JP  NZ, .no_colr
    LD  A, 0x40

.no_colr:
    OR   B
    OUT  (VID_DD67PB), A
    RET

; --------------------------------------------------
;
; --------------------------------------------------
esc_set_charset:
    LD   A, (M_VARS.esc_param)
    AND  0x3                                         ; charset 0..3
    LD   (M_VARS.codepage), A
    RET

; ------------------------------------------------------
; Get address for draw symbol glyph
; Inp: A - ascii code
; Out: HL -> glyph offset
; ------------------------------------------------------
m_get_glyph:
    LD   L, A                                       ; L = ascii code
    LD   E, A                                       ; E = ascii code
    XOR  A
    LD   D, A
    LD   H, A
    ; HL = DE = ascii code
    ADD  HL, HL
    ADD  HL, DE
    ADD  HL, HL
    ADD  HL, DE
    ; HL = A * 7
    LD   A, E                                        ; A = A at proc entry
    CP   '@'
    ; First 64 symbols is same for all codepages
    JP   M, .cp_common
    LD   A, (M_VARS.codepage)
    OR   A
    ; cp=0 - Latin letters
    JP   Z, .cp_common
    DEC  A
    ; cp=1 - Russian letters
    JP   Z, .cp_rus
    ; cp=2 - 0x40..0x5F - displayed as Lat
    ; 0x60 - 0x7F - displayed as Rus
    LD   A, E
    CP   0x60
    JP   M, .cp_common
.cp_rus:
    LD   DE, 448                                    ; +448=64*7 Offset for cp1
    ADD  HL, DE

.cp_common:
    LD   DE, m_font_cp0-224                         ; m_font_cp0-32*7
    ADD  HL, DE                                     ; add symbol glyph offset
    RET

; --------------------------------------------------
; Console output
; Inp: C - char
; --------------------------------------------------
m_print_no_esc:
    LD   A, C
    AND  0x7f
    CP   0x20
    JP   M, m_handle_esc_code
    CALL m_get_glyph
    EX   DE, HL
    LD   A, (M_VARS.screen_mode)
    AND  0x3
    JP   NZ, mp_mode_64
    CALL calc_addr_40
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    EX   DE, HL
    LD   A, B
    OR   B
    JP   Z, mpn_l1
    DEC  B
    JP   Z, mpn_l2
    DEC  B
    JP   Z, mpn_l4
    JP   mpn_l6

mpn_l1:
    XOR  A
    LD   (DE), A
    INC  D
    LD   (DE), A
    DEC  D
    INC  E

mpn_l1_1:
    LD   A, (M_VARS.curr_color)
    AND  (HL)
    LD   B, A
    LD   A, (DE)
    AND  0xc0
    OR   B
    LD   (DE), A
    LD   A, (M_VARS.curr_color+1)
    AND  (HL)
    LD   B, A
    INC  D
    LD   A, (DE)
    AND  0xc0
    OR   B
    LD   (DE), A
    DEC  D
    INC  HL
    INC  E
    DEC  C
    JP   NZ, mpn_l1_1
    JP   mpn_clos_vram

mpn_l2:
    XOR  A
    LD   (DE), A
    INC  D
    LD   (DE), A
    DEC  D
    DEC  D
    LD   (DE), A
    DEC  D
    LD   (DE), A
    INC  D
    INC  D
    INC  E

mpn_l3:
    LD   A, (M_VARS.curr_color+1)
    AND  (HL)
    RRCA
    RRCA
    LD   (M_VARS.ul_var3), A
    LD   A, (M_VARS.curr_color)
    AND  (HL)
    RRCA
    RRCA
    LD   (M_VARS.ul_var2), A
    AND  0xf
    LD   B, A
    LD   A, (DE)
    AND  0xf0
    OR   B
    LD   (DE), A
    LD   A, (M_VARS.ul_var3)
    AND  0xf
    LD   B, A
    INC  D
    LD   A, (DE)
    AND  0xf0
    OR   B
    LD   (DE), A
    DEC  D
    LD   A, (M_VARS.ul_var3)
    AND  0xc0
    LD   B, A
    DEC  D
    LD   A, (DE)
    AND  0x3f
    OR   B
    LD   (DE), A
    LD   A, (M_VARS.ul_var2)
    AND  0xc0
    LD   B, A
    DEC  D
    LD   A, (DE)
    AND  0x3f
    OR   B
    LD   (DE), A
    INC  D
    INC  D
    INC  HL
    INC  E
    DEC  C
    JP   NZ, mpn_l3
    JP   mpn_clos_vram

mpn_l4:
    XOR  A
    LD   (DE), A
    INC  D
    LD   (DE), A
    DEC  D
    DEC  D
    LD   (DE), A
    DEC  D
    LD   (DE), A
    INC  D
    INC  D
    INC  E

mpn_l5:
    LD   A, (M_VARS.curr_color+1)
    AND  (HL)
    RRCA
    RRCA
    RRCA
    RRCA
    LD   (M_VARS.ul_var3), A
    LD   A, (M_VARS.curr_color)
    AND  (HL)
    RRCA
    RRCA
    RRCA
    RRCA
    LD   (M_VARS.ul_var2), A
    AND  0x3
    LD   B, A
    LD   A, (DE)
    AND  0xfc
    OR   B
    LD   (DE), A
    LD   A, (M_VARS.ul_var3)
    AND  0x3
    LD   B, A
    INC  D
    LD   A, (DE)
    AND  0xfc
    OR   B
    LD   (DE), A
    DEC  D
    LD   A, (M_VARS.ul_var3)
    AND  0xf0
    LD   B, A
    DEC  D
    LD   A, (DE)
    AND  0xf
    OR   B
    LD   (DE), A
    LD   A, (M_VARS.ul_var2)
    AND  0xf0
    LD   B, A
    DEC  D
    LD   A, (DE)
    AND  0xf
    OR   B
    LD   (DE), A
    INC  D
    INC  D
    INC  HL
    INC  E
    DEC  C
    JP   NZ, mpn_l5
    JP   mpn_clos_vram

mpn_l6:
    DEC  D
    XOR  A
    LD   (DE), A
    DEC  D
    LD   (DE), A
    INC  D
    INC  E

mpn_l7:
    LD   A, (M_VARS.curr_color+1)
    AND  (HL)
    RLCA
    RLCA
    LD   B, A
    LD   A, (DE)
    AND  0x3
    OR   B
    LD   (DE), A
    LD   A, (M_VARS.curr_color)
    AND  (HL)
    RLCA
    RLCA
    LD   B, A
    DEC  D
    LD   A, (DE)
    AND  0x3
    OR   B
    LD   (DE), A
    INC  D
    INC  HL
    INC  E
    DEC  C
    JP   NZ, mpn_l7
    INC  D

mpn_clos_vram:
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    LD   HL, m_draw_cursor
    PUSH HL
    LD   HL, M_VARS.cursor_row

; --------------------------------------------------
; Handle ASCII_CAN (cursor right)
; Inp: HL - cursor pos
; --------------------------------------------------
m40_rt:
    INC  HL
    LD   A, (HL)                                    ; a = col
    ADD  A, 1                                       ; col+1
    AND  0x3f                                       ; screen column 0..63
    LD   (HL), A                                    ; save new col
    CP   40
    DEC  HL
    RET  M                                          ; Return if no wrap

m40_wrap_rt:
    INC  HL
    XOR  A
    LD   (HL), A
    DEC  HL
    LD   A, (M_VARS.screen_mode)
    AND  0x08                                       ; screen_mode=8?
    JP   NZ, m2_lf

; --------------------------------------------------
; Handle ASCII_LF (cursor down)
; Inp: HL - cursor pos
; --------------------------------------------------
m40_lf:
    LD   A, (HL)
    ADD  A, 11
    CP   250
    JP   NC, scroll_up
    LD   (HL), A
    RET

; --------------------------------------------------
; Handle ASCII_BS (cursor left)
; Inp: HL - cursor pos
; --------------------------------------------------
m40_bksp:
    INC  HL
    LD   A, (HL)
    SUB  1                                          ; TODO: DEC A
    AND  0x3f                                       ; A=0..63
    CP   0x3f
    JP   Z, .wrap
    LD   (HL), A
    DEC  HL
    RET

.wrap:
    LD   A, 39
    LD   (HL), A
    DEC  HL
    ; and cursor up

; --------------------------------------------------
; Handle ASCII_EM (cursor up)
; Inp: HL - cursor pos
; --------------------------------------------------
m40_up:
    LD   A, (HL)
    SUB  11                                         ; 10 rows per symbol
    JP   NC, .up_no_minus
    LD   A, 242                                     ; wrap to bottom
.up_no_minus:
    LD   (HL), A
    RET

; --------------------------------------------------
; Handle ASCII_TAB (cursor right 8 pos) 20rows mode
; Inp: HL - cursor pos
; --------------------------------------------------
m20_tab:
    INC  HL
    LD   A, (HL)
    ADD  A, 8
    AND  0x3f                                       ; wrap A=0..63
    LD   (HL), A
    CP   40
    DEC  HL
    RET  M                                          ; ret if column <40
    JP   m40_wrap_rt                                ; or wrap to next line

; --------------------------------------------------
; Calculate VRAM address in 40 column mode
; --------------------------------------------------
calc_addr_40:
    LD   HL, (M_VARS.cursor_row)
    LD   A, (M_VARS.row_shift)
    ADD  A, L
    LD   L, A
    LD   A, H
    CP   0x4
    LD   B, A
    JP   M, .l2
    AND  0x3
    LD   B, A
    LD   A, H
    OR   A
    RRA
    OR   A
    RRA
    LD   C, A
    LD   H, 0x6
    XOR  A
.l1:
    ADD  A, H
    DEC  C
    JP   NZ, .l1
    ADD  A, B
.l2:
    ADD  A, B
    ADD  A, 0x42
    LD   H, A
    LD   C, 0x7
    RET

; --------------------------------------------------
;
; --------------------------------------------------
m2_lf:
    LD   A, (HL)
    ADD  A, 11
    CP   15
    JP   NC, .lf_nowr
    LD   (HL), A
    RET

.lf_nowr:
    LD   A, (M_VARS.row_shift)
    LD   L, A
    ADD  A, 11
    LD   E, A
    LD   C, 8
    ; Access to VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

.cas_l5:
    LD   B, 0x40
    LD   H, 0x40                                    ; TODO: LD H, B  save 1b 3t
    LD   D, H

.cas_l6:
    LD   A, (DE)
    LD   (HL), A
    INC  H
    INC  D
    DEC  B
    JP   NZ, .cas_l6
    INC  L
    INC  E
    DEC  C
    JP   NZ, .cas_l5
    LD   C, 11
    LD   A, (M_VARS.row_shift)
    ADD  A, 8
    LD   E, A

.cas_l7:
    LD   B, 0x40
    LD   D, 0x40                                    ; TODO: LD D, B  save 1b 3t
    XOR  A

.cas_l8:
    LD   (DE),A
    INC  D
    DEC  B
    JP   NZ,.cas_l8
    INC  E
    DEC  C
    JP   NZ,.cas_l7
    LD   A,0x0
    OUT  (SYS_DD17PB),A
    RET


; ---------------------------------------------------
; Handle ASCII_BS (cursor left) in 20row mode
; ---------------------------------------------------
m20_bksp:
    INC  HL
    LD   A, (HL)
    OR   A
    DEC  HL
    RET  Z

    INC  HL
    SUB  1                                          ; TODO: DEC A - save 1b 2t
    AND  0x3f
    LD   (HL), A
    DEC  HL
    RET

; ---------------------------------------------------
; Print symbol in 64x25 mode
; ---------------------------------------------------
mp_mode_64:
    CP   0x3
    JP   Z, mp_mode_80
    PUSH AF
    LD   HL, (M_VARS.cursor_row)
    LD   A, (M_VARS.row_shift)
    ADD  A, L
    LD   L, A
    LD   A, H
    ADD  A, 0x40
    LD   H, A
    LD   C, 0x7
    POP  AF
    CP   0x2
    JP   Z, m64_put_vram
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    EX   DE, HL
    XOR  A
    LD   (DE), A
    INC  E

m64_next_row:
    LD   A, (HL)
    ADD  A, A
    LD   (DE), A
    INC  HL
    INC  E
    DEC  C
    JP   NZ, m64_next_row
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    LD   HL, m_draw_cursor
    PUSH HL
    LD   HL, M_VARS.cursor_row

; --------------------------------------------------
; Handle ASCII_CAN (cursor right) in 64x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m64_rt:
    INC  HL
    LD   A, (HL)
    ADD  A, 0x1
    AND  0x3f
    LD   (HL), A
    DEC  HL
    RET  NZ

; --------------------------------------------------
; Handle ASCII_LF (cursor down) in 64x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m64_lf:
    LD   A, (HL)
    ADD  A, 11
    CP   0xfa
    JP   NC, scroll_up
    LD   (HL), A
    RET

; --------------------------------------------------
; Scroll Up for 10 rows
; --------------------------------------------------
scroll_up:
    LD   A, (M_VARS.row_shift)
    ADD  A, 11
    OUT  (SYS_DD17PA), A                            ; Scroll via VShift register
    LD   (M_VARS.row_shift), A                      ; store new VShift value
    ; calc bottom 16 rows address in VRAM
    LD   HL, 0x40f0                                 ; 240th VRAM byte
    ADD  A, L
    LD   L, A
    LD   C, H

    ; Access to VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

    XOR  A
    LD   DE, 0x1040                                 ; D=16 E=64 (512/8 bytes in row)

.next_row:
    LD   H, C
    LD   B, E

    ; clear 64 bytes (512px in mono or 256px in color mode)
.next_col:
    LD   (HL), A
    INC  H                                          ; next column
    DEC  B
    JP   NZ, .next_col
    INC  L                                          ; next row address
    DEC  D                                          ; row counter - 1
    JP   NZ, .next_row

    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

; --------------------------------------------------
; Handle ASCII_BS (cursor left) in 64x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m64_bs:
    INC  HL
    LD   A, (HL)
    SUB  1                                          ; TODO: DEC A - save 1b 2t
    AND  0x3f                                       ; wrap column (0..63)
    LD   (HL), A
    CP   63
    DEC  HL
    RET  NZ
    ; cursor up if wrapped

; --------------------------------------------------
; Handle ASCII_EM (cursor up) in 64x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m64_up:
    LD   A, (HL)
    SUB  11
    JP   NC, .no_wrap
    LD   A, 242

.no_wrap:
    LD   (HL), A
    RET

; --------------------------------------------------
; Handle ASCII_TAB (cursor column + 8) in 64x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m64_tab:
    INC  HL
    LD   A, (HL)
    ADD  A, 8
    AND  0x38
    LD   (HL), A
    DEC  HL
    RET  NZ                                         ; return if no wrap
    ; cursor down if wrap
    JP   m64_lf

; --------------------------------------------------
; Print symbols in 80x25 mode
; --------------------------------------------------
mp_mode_80:
    CALL calc_addr_80
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    EX   DE, HL
    LD   A, B
    OR   B
    JP   Z, m80_l1
    DEC  B
    JP   Z, m80_l3
    DEC  B
    JP   Z, m80_l5
    JP   m80_l7

m80_l1:
    XOR  A
    LD   (DE), A
    INC  E
m80_l2:
    LD   B, (HL)
    LD   A, (DE)
    AND  0xc0
    OR   B
    LD   (DE), A
    INC  HL
    INC  E
    DEC  C
    JP   NZ, m80_l2
    JP   m80_l9
m80_l3:
    XOR  A
    LD   (DE), A
    DEC  D
    LD   (DE), A
    INC  D
    INC  E
m80_l4:
    LD   A, (HL)
    RRCA
    RRCA
    AND  0x7
    LD   B, A
    LD   A, (DE)
    AND  0xf0
    OR   B
    LD   (DE), A
    LD   A, (HL)
    RRCA
    RRCA
    AND  0xc0
    LD   B, A
    DEC  D
    LD   A, (DE)
    AND  0x1f
    OR   B
    LD   (DE), A
    INC  D
    INC  HL
    INC  E
    DEC  C
    JP   NZ, m80_l4
    JP   m80_l9
m80_l5:
    XOR  A
    LD   (DE), A
    DEC  D
    LD   (DE), A
    INC  D
    INC  E
m80_l6:
    LD   A, (HL)
    RRCA
    RRCA
    RRCA
    RRCA
    AND  0x1
    LD   B, A
    LD   A, (DE)
    AND  0xfc
    OR   B
    LD   (DE), A
    LD   A, (HL)
    RRCA
    RRCA
    RRCA
    RRCA
    AND  0xf0
    LD   B, A
    DEC  D
    LD   A, (DE)
    AND  0x7
    OR   B
    LD   (DE), A
    INC  D
    INC  HL
    INC  E
    DEC  C
    JP   NZ, m80_l6
    JP   m80_l9

m80_l7:
    DEC  D
    XOR  A
    LD   (DE), A
    INC  E
m80_l8:
    LD   A, (HL)
    RLCA
    RLCA
    LD   B, A
    LD   A, (DE)
    AND  0x1
    OR   B
    LD   (DE), A
    INC  HL
    INC  E
    DEC  C
    JP   NZ, m80_l8
    INC  D

m80_l9:
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    LD   HL, m_draw_cursor
    PUSH HL
    LD   HL, M_VARS.cursor_row

; --------------------------------------------------
; Handle ASCII_CAN (cursor right) in 80x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m80_rt:
    INC  HL
    LD   A, (HL)
    ADD  A, 0x1
    AND  0x7f
    LD   (HL), A
    CP   0x50
    DEC  HL
    RET  M

m80_col_wrap:
    INC  HL
    XOR  A
    LD   (HL), A
    DEC  HL
    ; and move cursor to next row

; --------------------------------------------------
; Handle ASCII_LF (cursor down) in 80x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m80_lf:
    LD   A, (HL)
    ADD  A, 11
    CP   250
    JP   NC, scroll_up
    LD   (HL), A
    RET

; --------------------------------------------------
; Handle ASCII_BS (cursor left) in 80x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m80_bs:
    INC  HL
    LD   A, (HL)
    SUB  0x1
    AND  0x7f
    CP   0x7f
    JP   Z, .wrap
    LD   (HL), A
    DEC  HL
    RET
.wrap:
    LD   A, 0x4f
    LD   (HL), A
    DEC  HL

    ; and move cursor to previous line

; --------------------------------------------------
; Handle ASCII_EM (cursor up) in 80x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m80_up:
    LD   A, (HL)
    SUB  0xb
    JP   NC, .wrap
    LD   A, 0xf2
.wrap
    LD   (HL), A
    RET

; --------------------------------------------------
; Handle ASCII_TAB (cursor column + 8) in 80x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m80_tab:
    INC  HL
    LD   A, (HL)
    ADD  A, 0x8
    AND  0x7f
    LD   (HL), A
    CP   0x50
    DEC  HL
    RET  M
    JP   m80_col_wrap

; --------------------------------------------------
; Calculate address for cursor pos for 80x25 mode
; Out: HL -> VRAM
;      B -> pixel pos in byte
; --------------------------------------------------
calc_addr_80:
    LD   HL, (M_VARS.cursor_row)
    LD   A, (M_VARS.row_shift)
    ADD  A, L
    LD   L, A
    LD   A, H
    CP   0x4
    LD   B, A
    JP   M, ca80_l2
    AND  0x3
    LD   B, A
    LD   A, H
    OR   A
    RRA
    OR   A
    RRA
    LD   C, A
    LD   H, 0x3
    XOR  A
ca80_l1:
    ADD  A, H
    DEC  C
    JP   NZ, ca80_l1
    ADD  A, B
ca80_l2:
    ADD  A, 0x42
    LD   H, A
    LD   C, 0x7
    RET

; --------------------------------------------------
m64_put_vram:
    LD   A, (M_VARS.cursor_col)
    CP   0x40
    JP   M, pv_access_vram
    LD   HL, M_VARS.cursor_row
    CALL m_draw_cursor
    RET

pv_access_vram:
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    EX   DE, HL
    XOR  A
    LD   (DE), A
    INC  E

pv_rept:
    LD   A, (HL)
    ADD  A, A
    LD   (DE), A
    INC  HL
    INC  E
    DEC  C
    JP   NZ, pv_rept
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    LD   HL, m_draw_cursor
    PUSH HL
    LD   HL, M_VARS.cursor_row
    INC  HL
    LD   A, (HL)
    ADD  A, 0x1
    CP   0x40
    JP   M, pv_end
    LD   A, 0x40
pv_end:
    LD   (HL), A
    DEC  HL
    RET

; --------------------------------------------------
; Clear screen and set cursor to 0,0
; Inp: HL -> cursor position
; --------------------------------------------------
m_clear_screen:
    LD   A, (M_VARS.screen_mode)
    AND  0x8
    JP   NZ, m_clear_20_rows                         ; for bit 4 is set, clear only 20 rows
    ; all in black
    LD   A, 01111111b
    OUT  (VID_DD67PB), A                             ; C/M=1 FL=111 CL=111 All black
    ; Access VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   DE, video_ram
    EX   DE, HL
    LD   A, H
    ADD  A, 0x40                                      ; A=0x80
    LD   B, 0

.fill_scrn:
    LD   (HL), B
    INC  HL
    CP   H
    JP   NZ, .fill_scrn                               ; fill while HL<0x8000

    EX   DE, HL
    LD   A, (M_VARS.cur_palette)
    LD   B, A                                         ; B = current palette
    LD   A, (M_VARS.screen_mode)
    AND  0x3                                          ; color?
    LD   A, 0x0
    JP   NZ, .mono_mode
    LD   A, 01000000b
.mono_mode:
    OR   B
    ; Restore mode and palette
    OUT  (VID_DD67PB), A

    ; And set cursor to home position

; --------------------------------------------------
; Set cursor to 0,0 and close VRAM access
; Inp: HL -> cursor_row
; --------------------------------------------------
m_cursor_home:
    XOR  A
    NOP
    NOP
    LD   (HL), A
    INC  HL
    XOR  A
    LD   (HL), A
    DEC  HL
    ;XOR  A
    LD   A, 0
    ; Disable VRAM access
    OUT  (SYS_DD17PB), A
    RET

; Clear only 20 rows
m_clear_20_rows:
    ; take row shift in account
    LD   A, (M_VARS.row_shift)
    LD   L, A
    LD   C, 20

    ; Access VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

.next_row:
    LD   H, 0x40                                     ; HL = 0x4000 + shift_row
    LD   B, 64                                       ; 64 bytes at row
    XOR  A
.next_col:
    LD   (HL), A
    INC  H                                           ; next column
    DEC  B
    JP   NZ, .next_col
    INC  L                                           ; next row
    DEC  C
    JP   NZ, .next_row
    ; Disabe VRAM access
    LD   A, 0
    OUT  (SYS_DD17PB), A
    JP   m_cursor_home

; --------------------------------------------------
; Draw cursor at current cursor position
; if not hidden
; --------------------------------------------------
m_draw_cursor:
    LD   A, (M_VARS.screen_mode)
    AND  0x4
    RET  NZ
    LD   A, (M_VARS.screen_mode)
    AND  0x3
    JP   NZ, .dc_mode_64
    EX   DE, HL
    LD   HL, (M_VARS.cursor_row)
    LD   A, H
    CP   0x28
    EX   DE, HL
    RET  P
    EX   DE, HL
    CALL calc_addr_40
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   C, 0x8
    LD   A, B
    OR   B
    JP   Z, .l1
    DEC  B
    JP   Z, .l2
    DEC  B
    JP   Z, .l3
    JP   .l4

.l1:
    LD   A, (HL)
    AND  0xc0
    LD   B, A
    LD   A, (HL)
    XOR  0x1f
    AND  0x1f
    OR   B
    LD   (HL), A
    INC  H
    LD   A, (HL)
    AND  0xc0
    LD   B, A
    LD   A, (HL)
    XOR  0x1f
    AND  0x1f
    OR   B
    LD   (HL), A
    DEC  H
    INC  L
    DEC  C
    JP   NZ, .l1
    JP   .l6

.l2:
    DEC  H
    LD   A, (HL)
    AND  0x3f
    LD   B, A
    LD   A, (HL)
    XOR  0xc0
    AND  0xc0
    OR   B
    LD   (HL), A
    DEC  H
    LD   A, (HL)
    AND  0x3f
    LD   B, A
    LD   A, (HL)
    XOR  0xc0
    AND  0xc0
    OR   B
    LD   (HL), A
    INC  H
    INC  H
    LD   A, (HL)
    AND  0xf0
    LD   B, A
    LD   A, (HL)
    XOR  0x7
    AND  0x7
    OR   B
    LD   (HL), A
    INC  H
    LD   A, (HL)
    AND  0xf0
    LD   B, A
    LD   A, (HL)
    XOR  0x7
    AND  0x7
    OR   B
    LD   (HL), A
    DEC  H
    INC  L
    DEC  C
    JP   NZ, .l2
    JP   .l6

.l3:
    DEC  H
    LD   A, (HL)
    AND  0xf
    LD   B, A
    LD   A, (HL)
    XOR  0xf0
    AND  0xf0
    OR   B
    LD   (HL), A
    DEC  H
    LD   A, (HL)
    AND  0xf
    LD   B, A
    LD   A, (HL)
    XOR  0xf0
    AND  0xf0
    OR   B
    LD   (HL), A
    INC  H
    INC  H
    LD   A, (HL)
    AND  0xfc
    LD   B, A
    LD   A, (HL)
    XOR  0x1
    AND  0x1
    OR   B
    LD   (HL), A
    INC  H
    LD   A, (HL)
    AND  0xfc
    LD   B, A
    LD   A, (HL)
    XOR  0x1
    AND  0x1
    LD   (HL), A
    DEC  H
    INC  L
    DEC  C
    JP   NZ, .l3
    JP   .l6

.l4:
    DEC  H

.l5:
    LD   A, (HL)
    AND  0x3
    LD   B, A
    LD   A, (HL)
    XOR  0x7c
    AND  0x7c
    OR   B
    LD   (HL), A
    DEC  H
    LD   A, (HL)
    AND  0x3
    LD   B, A
    LD   A, (HL)
    XOR  0x7c
    AND  0x7c
    OR   B
    LD   (HL), A
    INC  H
    INC  L
    DEC  C
    JP   NZ, .l5
    INC  H

.l6:
    EX   DE, HL
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

.dc_mode_64:
    CP   0x3
    JP   Z, .l8
    EX   DE, HL
    LD   HL, (M_VARS.cursor_row)
    LD   A, (M_VARS.row_shift)
    ADD  A, L
    LD   L, A
    LD   A, H
    CP   0x40
    EX   DE, HL
    RET  P
    EX   DE, HL
    ADD  A, 0x40
    LD   H, A
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   BC, 0x7f08

.l7:
    LD   A, (HL)
    XOR  B
    LD   (HL), A
    INC  L
    DEC  C
    JP   NZ, .l7
    EX   DE, HL
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

.l8:
    EX   DE, HL
    LD   HL, (M_VARS.cursor_row)
    LD   A, H
    CP   0x50
    EX   DE, HL
    RET  P
    EX   DE, HL
    CALL calc_addr_80
    LD   C, 0x8
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   A, B
    OR   B
    JP   Z, .l9
    DEC  B
    JP   Z, .l10
    DEC  B
    JP   Z, .l11
    JP   .l12

.l9:
    LD   A, (HL)
    AND  0xc0
    LD   B, A
    LD   A, (HL)
    XOR  0x1f
    AND  0x1f
    OR   B
    LD   (HL), A
    INC  L
    DEC  C
    JP   NZ, .l9
    JP   .exit

.l10:
    DEC  H
    LD   A, (HL)
    AND  0x1f
    LD   B, A
    LD   A, (HL)
    XOR  0xc0
    AND  0xc0
    OR   B
    LD   (HL), A
    INC  H
    LD   A, (HL)
    AND  0xf0
    LD   B, A
    LD   A, (HL)
    XOR  0x7
    AND  0x7
    OR   B
    LD   (HL), A
    INC  L
    DEC  C
    JP   NZ, .l10
    JP   .exit

.l11:
    DEC  H
    LD   A, (HL)
    AND  0x7
    LD   B, A
    LD   A, (HL)
    XOR  0xf0
    AND  0xf0
    OR   B
    LD   (HL), A
    INC  H
    LD   A, (HL)
    AND  0xfc
    LD   B, A
    LD   A, (HL)
    XOR  0x1
    AND  0x1
    OR   B
    LD   (HL), A
    INC  L
    DEC  C
    JP   NZ, .l11
    JP   .exit

.l12:
    DEC  H
.l13:
    LD   A, (HL)
    AND  0x1
    LD   B, A
    LD   A, (HL)
    XOR  0x7c
    AND  0x7c
    OR   B
    LD   (HL), A
    INC  L
    DEC  C
    JP   NZ, .l13
    INC  H

.exit:
    EX   DE, HL
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

; --------------------------------------------------
; If ESC character, turn esc_mode ON
; Inp: A - ASCII symbol
; --------------------------------------------------
m_handle_esc_code:
    CP   ASCII_ESC
    JP   NZ, m_handle_control_code
    ; turn on ESC mode for next chars
    LD   HL, M_VARS.esc_mode
    LD   (HL), 0x1                                   ; turn on ESC mode
    INC  HL
    LD   (HL), 0xff                                  ; esc_cmd = 0xff
    RET

; --------------------------------------------------
; Handle one byte ASCII control code
; Inp: A - ASCII symbol
; --------------------------------------------------
m_handle_control_code:
    CP   ASCII_BELL
    JP   Z, m_beep
    LD   HL, m_draw_cursor
    PUSH HL
    LD   HL, M_VARS.cursor_row
    PUSH AF
    CALL m_draw_cursor
    LD   A, (M_VARS.screen_mode)
    AND  0x08                                        ; 20-rows mode?
    JP   Z, handle_cc_common                         ; jump for normal screen modes

    ; for hidden cursor modes
    POP  AF
    CP   ASCII_TAB                                   ; TAB
    JP   Z, m20_tab
    CP   ASCII_BS                                    ; BKSP
    JP   Z, m20_bksp
    CP   ASCII_CAN                                   ; Cancel
    JP   Z, m40_rt
    CP   ASCII_US                                    ; ASCII Unit separator
    JP   Z, m_clear_20_rows
    CP   ASCII_LF                                    ; LF
    JP   Z, m2_lf
    CP   ASCII_CR                                    ; CR
    RET  NZ                                          ; ret on unknown
    INC  HL
    LD   (HL), 0x0
    DEC  HL
    RET

; --------------------------------------------------
; Handle cursor for 40x25, 64x25, 80x25 modes
; --------------------------------------------------
handle_cc_common:
    POP  AF
    CP   ASCII_US
    JP   Z, m_clear_screen
    CP   ASCII_FF
    JP   Z, m_cursor_home
    PUSH AF
    LD   A, (M_VARS.screen_mode)
    AND  3                                          ; check for color modes
    JP   NZ, handle_cc_mono
    ; 32x25 text mode
    POP  AF
    CP   ASCII_TAB                                  ; cursor right +8
    JP   Z, m20_tab
    CP   ASCII_BS                                   ; cursor left
    JP   Z, m40_bksp
    CP   ASCII_CAN                                  ; cursor right
    JP   Z, m40_rt
    CP   ASCII_EM                                   ; cursor up
    JP   Z, m40_up
    CP   ASCII_SUB
    JP   Z, m40_lf                                  ; cursor down
    CP   ASCII_LF
    JP   Z, m40_lf
    CP   ASCII_CR
    RET  NZ
    INC  HL
    LD   (HL), 0x0                                  ; move cursor to first column for CR
    DEC  HL
    RET

; --------------------------------------------------
; Handle control chars for 64x25 or 80x25 modes
; --------------------------------------------------
handle_cc_mono:
    LD   A, (M_VARS.screen_mode)
    CP   0x3
    JP   Z, handle_cc_80x25
    CP   0x7
    JP   Z, handle_cc_80x25
    AND  0x2
    JP   NZ, handle_cc_colr
    POP  AF
    CP   ASCII_TAB
    JP   Z, m64_tab
    CP   ASCII_BS
    JP   Z, m64_bs
    CP   ASCII_CAN
    JP   Z, m64_rt
    CP   ASCII_EM
    JP   Z, m64_up
    CP   ASCII_SUB
    JP   Z, m64_lf
    CP   ASCII_LF
    JP   Z, m64_lf
    CP   ASCII_CR
    RET  NZ
    INC  HL
    LD   (HL), 0x0
    DEC  HL
    RET

handle_cc_colr:
    POP  AF
    CP   ASCII_LF
    JP   Z, m64_lf
    CP   ASCII_CR
    RET  NZ
    INC  HL
    LD   (HL), 0x0
    DEC  HL
    RET


; --------------------------------------------------
; Handle control chars for 80x25 modes
; --------------------------------------------------
handle_cc_80x25:
    POP  AF
    CP   ASCII_TAB
    JP   Z, m80_tab
    CP   ASCII_BS
    JP   Z, m80_bs
    CP   ASCII_CAN
    JP   Z, m80_rt
    CP   ASCII_EM
    JP   Z, m80_up
    CP   ASCII_SUB
    JP   Z, m80_lf
    CP   ASCII_LF
    JP   Z, m80_lf
    CP   ASCII_CR
    RET  NZ
    INC  HL
    LD   (HL), 0x0
    DEC  HL
    RET

; --------------------------------------------------
;
; --------------------------------------------------
m_beep:
    LD   HL, (M_VARS.beep_duration)
    EX   DE, HL
    LD   HL, (M_VARS.beep_period)
    LD   A, 00110110b                                 ; TMR#0 LSB+MSB Square Wave Generator
    OUT  (TMR_DD70CTR), A
    LD   A, L                                         ; LSB
    OUT  (TMR_DD70C1), A
    LD   A, H                                         ; MSB
    OUT  (TMR_DD70C1), A
    LD   A, (M_VARS.strobe_state)
    LD   B, A
m_bell_cont:
    LD   A, D                                        ; DE=duration
    OR   E
    RET  Z                                           ; ret if enough
    DEC  DE
    LD   A, B
    XOR BELL_PIN
    LD   B, A
    OUT  (DD67PC), A                                 ; Invert bell pin
m_bell_wait_tmr1:
    IN   A, (PIC_DD75RS)
    AND  TIMER_IRQ                                   ; 0x10
    JP   NZ, m_bell_wait_tmr1
    LD   A, B
    XOR  BELL_PIN                                    ; Flip pin again
    LD   B, A
    OUT  (DD67PC), A
m_bell_wait_tmr2:
    IN   A, (PIC_DD75RS)
    AND  TIMER_IRQ
    JP   Z,m_bell_wait_tmr2
    JP   m_bell_cont


; ------------------------------------------------------
; <ESC>5<row><col> Set cursor position
; ------------------------------------------------------
esc_set_cursor:
    LD   A, (M_VARS.screen_mode)
    AND  0x8
    RET  NZ
    CALL m_draw_cursor
    LD   DE, M_VARS.esc_param
    LD   HL, M_VARS.cursor_col
    INC  DE
    LD   A, (DE)
    SUB  0x20
    LD   B, A
    LD   A, (M_VARS.screen_mode)
    CP   0x3
    JP   Z, .mode80
    CP   0x7
    JP   Z, .mode80
    OR   A
    JP   Z, .mode40
    CP   0x4
    JP   Z, .mode40
    LD   A, B
    CP   0x40
    JP   M, .common
    LD   A, 0x40
    JP   .common
.mode40:
    LD   A, B
    CP   40
    JP   M, .common
    LD   A, 40
    JP   .common
.mode80:
    LD   A, B
    CP   80
    JP   M, .common
    LD   A, 80
.common:
    LD   (HL), A
    DEC  DE
    DEC  HL
    LD   A, (DE)
    SUB  32
    CP   22
    JP   C, .le_24
    LD   A, 22
.le_24:
    LD   B, A
    ADD  A, A
    ADD  A, A
    ADD  A, B
    ADD  A, A
    ADD  A, B
    LD   (HL), A
    CALL m_draw_cursor
    RET

; ------------------------------------------------------
;  <ESC>6n Set video mode or cursor visibility
;  Inp: n is
;  0   - C 32x25 with cursor;       0000
;  1   - M 64x25 with cursor;       0001
;  2   - M 64x25 with cursor;       0010
;  3   - M 80x25 with cursor;       0011
;  4   - C 32x25 no cursor;         0100
;  5   - M 64x25 no cursor;         0101
;  6   - M 64x25 no cursor;         0110
;  7   - M 80x25 no cursor;         0111
;  8   - M 20rows mode              1000
;  9   - hide cursor                1001
;  10  - show cursor                1010
; ------------------------------------------------------
esc_set_vmode:
    LD   HL, M_VARS.screen_mode
    LD   A, (M_VARS.cur_palette)
    LD   B, A
    LD   A, (M_VARS.esc_param)
    LD   C, A
    AND  0x8
    LD   A, C
    JP   Z, .no_20_mode
    LD   A, 0x8
.no_20_mode:
    AND  0xf
    LD   (HL), A
    CP   0x4
    JP   Z, .m32x25c
    AND  0x3
    LD   A, 0x0
    JP   NZ, .t_mode
.m32x25c:
    LD   A, 0x40
.t_mode:
    OR   B
    OUT  (VID_DD67PB), A
    LD   HL, M_VARS.cursor_row
    CALL m_clear_screen
    CALL m_draw_cursor
    RET

; ------------------------------------------------------
; <ESC>4n n=1..4 Set drawing color
; ------------------------------------------------------
esc_set_color:
    LD  A, (M_VARS.esc_param)
m_set_color:
    AND  0x3
    RRA
    LD   B, A
    LD   A, 0x0                                     ; TODO: unused
    SBC  A, A
    LD   (M_VARS.curr_color), A
    LD   A, B
    DEC  A
    CPL
    LD   (M_VARS.curr_color+1), A
    RET

;---------------------------------------------------
; Print symbol or print sprite at X,Y coordinates
; Inp: param x,y
;      C - character or sprite_no to draw
;---------------------------------------------------
m_print_at_xy:
    LD        A,(M_VARS.screen_mode)
    AND       0x7
    JP        NZ,esc_exit
    LD        A,C
    AND       0x7f
    LD        C,A
    CP        0x20
    JP        M,esc_exit
    LD        HL,M_VARS.esc_param
    LD        A,(HL)
    LD        E,A
    ADD       A,0x8
    JP        C,esc_exit
    LD        (HL),A
    INC       HL
    LD        A,0xf7
    CP        (HL)
    JP        C,esc_exit
    LD        D,(HL)
    CALL      calc_px_addr
    LD        A,L
    SUB       0x7
    LD        L,A
    PUSH      HL
    LD        A,C
    CALL      m_get_glyph
    POP       DE
    LD        C,0x7
.l1:
    PUSH      HL
    LD        A,0x1
    OUT       (SYS_DD17PB),A
    LD        L,(HL)
    LD        H,0x0
    LD        A,B
    OR        A
    JP        Z,.l3
.l2:
    ADD       HL,HL
    DEC       A
    JP        NZ,.l2
.l3:
    EX        DE,HL
    PUSH      BC
    LD        A,(M_VARS.curr_color)
    CPL
    LD        B,A
    LD        A,(HL)
    XOR       B
    OR        E
    XOR       B
    LD        (HL),A
    INC       H
    INC       H
    LD        A,(HL)
    XOR       B
    OR        D
    XOR       B
    LD        (HL),A
    DEC       H
    LD        A,(M_VARS.curr_color+1)
    CPL
    LD        B,A
    LD        A,(HL)
    XOR       B
    OR        E
    XOR       B
    LD        (HL),A
    INC       H
    INC       H
    LD        A,(HL)
    XOR       B
    OR        D
    XOR       B
    LD        (HL),A
    DEC       H
    DEC       H
    DEC       H
    INC       L
    EX        DE,HL
    POP       BC
    LD        A,0x0
    OUT       (SYS_DD17PB),A
    POP       HL
    INC       HL
    DEC       C
    JP        NZ,.l1
    RET

; --------------------------------------------------
; Calculate address of pixel in Video RAM
; Inp: DE - Y, X
; Out: HL - address
;      B - offset in byte
; --------------------------------------------------
calc_px_addr:
    ; take into account the vertical displacement
    LD   A, (M_VARS.row_shift)
    SUB  D
    DEC  A
    LD   L, A

    LD   A, E
    AND  0x07                                       ; X mod 8 - offset in byte
    LD   B, A

    LD   A, E
    RRA
    RRA
    AND  00111110b
    ADD  A, 0x40                                    ; VRAM at 0x4000
    LD   H, A
    RET


;---------------------------------------------------
; Draw Filled Rectange
; Inp: esc param X1,Y2,X2,Y2
; --------------------------------------------------
esc_draw_fill_rect:
    LD   HL, M_VARS.esc_param+3
    LD   DE, M_VARS.esc_param+1
    LD   A, (DE)
    LD   B, (HL)
    CP   B
    JP   NC, .l1
    LD   (HL), A
    LD   A, B
    LD   (DE), A
.l1:
    DEC  DE
    DEC  HL
    LD   A, (DE)
    LD   B, (HL)
    CP   B
    JP   C, .l2
    LD   (HL), A
    LD   A, B
    LD   (DE), A
.l2:
    EX   DE, HL
    LD   E, (HL)
    INC  HL
    LD   D, (HL)
    CALL calc_px_addr
    PUSH HL
    XOR  A
.l3:
    SCF
    RLA
    DEC  B
    JP   P, .l3
    RRA
    LD   D, A
    LD   HL, M_VARS.esc_param+2
    LD   A, (HL)
    AND  0x7
    LD   B, A
    XOR  A
.l4:
    SCF
    RLA
    DEC  B
    JP   P, .l4
    CPL
    LD   E, A
    LD   A, (HL)
    DEC  HL
    DEC  HL
    SUB  (HL)
    RRCA
    RRCA
    RRCA
    AND  0x1f
    LD   C, A
    INC  HL
    LD   A, (HL)
    INC  HL
    INC  HL
    SUB  (HL)
    JP   NZ, .l5
    INC  A
.l5:
    LD   B, A
    POP  HL
    LD   A, E
    LD   (M_VARS.esc_param+4), A
.l6:
    PUSH DE
    PUSH HL
    PUSH BC
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   A, C
    OR   A
    JP   NZ, .l8
    LD   A, D
    OR   E
.l7:
    LD   D, A
.l8:
    LD   B, D
    EX   DE, HL
    LD   HL, (M_VARS.curr_color)
    EX   DE, HL
    LD   A, (HL)
    XOR  E
    AND  B
    XOR  E
    LD   (HL), A
    INC  H
    LD   A, (HL)
    XOR  D
    AND  B
    XOR  D
    LD   (HL), A
    INC  H
    LD   A, C
    OR   A
    JP   Z, .l11
    DEC  C
.l9:
    LD   A, (M_VARS.esc_param+4)
    JP   Z, .l7
.l10:
    LD   (HL), E
    INC  H
    LD   (HL), D
    INC  H
    DEC  C
    JP   NZ, .l10
    JP   .l9
.l11:
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    POP  BC
    POP  HL
    POP  DE
    INC  L
    DEC  B
    JP   NZ, .l6
    RET

;---------------------------------------------------
; <ESC>2x1y1x2y2 Draw Line
;---------------------------------------------------
esc_draw_line:
    LD   HL, M_VARS.esc_param
    LD   E, (HL)                                    ; E=X1
    INC  HL
    LD   D, (HL)                                    ; D=Y1
    INC  HL
    LD   A, (HL)
    INC  HL
    LD   H, (HL)                                    ; H=Y2
    LD   L, A                                       ; L=X2
    CP   E
    JP   C, .x1_le_x2
    EX   DE, HL                                     ; exchange if X1>X2
.x1_le_x2:
    LD   (M_VARS.esc_param), HL                     ; store x1,y1 back
    LD   A, E
    SUB  L
    LD   L, A                                       ; L - width
    LD   A, D
    SUB  H
    LD   H, A                                       ; H - height
    PUSH AF
    JP   NC, .pos_height
    ; change sign
    CPL
    INC  A
    LD   H, A
.pos_height:
    EX   DE, HL
    LD   HL, (M_VARS.esc_param)
    EX   DE, HL
    JP   Z, height0
    LD   A, L
    OR   A
    JP   Z, .width0
    LD   B, A
    POP  AF
    LD   A, 0x0
    ADC  A, A
    LD   (M_VARS.esc_param+4), A
    ; HL = E/B   height/width
    LD   E, H
    LD   C, 16
    LD   D, 0
.next_16:
    ADD  HL, HL
    EX   DE, HL
    ADD  HL, HL
    EX   DE, HL
    LD   A, D
    JP   C, .edl_l4
    CP   B
    JP   C, .edl_l5
.edl_l4:
    SUB  B
    LD   D, A
    INC  HL
.edl_l5:
    DEC  C
    JP   NZ, .next_16
    LD   DE, 0x0
    PUSH DE
    ; save result at stack
    PUSH HL

    LD   HL, (M_VARS.esc_param)                     ; x1,y1
    EX   DE, HL
    LD   C, B
    CALL calc_px_addr
    ; HL - address, B - offset in byte
    ; make mask
    LD   A, 10000000b
.roll_l:
    RLCA
    DEC  B
    JP   P, .roll_l
    CPL
    LD   B, A                                       ; b - inv mask

.edl_l7
    POP  DE
    EX   (SP), HL                                   ; save HL on top of stack
    LD   A, H
    ADD  HL, DE
    SUB  H
    CPL
    INC  A
    EX   (SP), HL
    PUSH DE
    PUSH BC
    LD   C, A
    EX   DE, HL

    LD   HL, (M_VARS.curr_color)
    EX   DE, HL
    ; Access VideoRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

    LD   A, (M_VARS.esc_param+4)                    ; sign of delta Y
    OR   A
    JP   NZ, .next_down
.next_up:
    ; firs byte
    LD   A, (HL)
    XOR  E
    AND  B
    XOR  E
    LD   (HL), A
    ; second byte
    INC  H
    LD   A, (HL)
    XOR  D
    AND  B
    XOR  D
    LD   (HL), A
    DEC  H
    LD   A, C
    OR   A
    JP   Z, .is_last
    DEC  C
    ; draw up
    DEC  L
    JP   .next_up
.next_down:
    ; first byte
    LD   A, (HL)
    XOR  E
    AND  B
    XOR  E
    LD   (HL), A
    ; second byte
    INC  H
    LD   A, (HL)
    XOR  D
    AND  B
    XOR  D
    LD   (HL), A
    ;
    DEC  H
    LD   A, C
    OR   A
    JP   Z, .is_last
    DEC  C
    ; draw down
    INC  L
    JP   .next_down
.is_last:
    ; Disable VideoRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    POP  BC
    LD   A, B
    ; <<1px
    SCF
    RLA
    JP   C, .edl_l11
    RLA
    INC  H
    INC  H
.edl_l11
    LD   B, A
    DEC  C
    JP   NZ, .edl_l7
    POP  HL
    POP  HL
    RET

; --------------------------------------------------
; draw vertical line
; Inp: DE - YX
;       L - length
; --------------------------------------------------
.width0
    LD   C, H
    CALL calc_px_addr

    ; make pixel mask
    LD   A, 10000000b
.edl_l13:
    RLCA
    DEC  B
    JP   P, .edl_l13
    CPL
    LD   B, A

    EX   DE, HL
    LD   HL, (M_VARS.curr_color)
    EX   DE, HL
    POP  AF

    ; Enable VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    JP   C, .next_row_down

.next_row_up:
    ; first byte
    LD   A, (HL)
    XOR  E
    AND  B
    XOR  E
    LD   (HL), A
    ; second byte
    INC  H
    LD   A, (HL)
    XOR  D
    AND  B
    XOR  D
    LD   (HL), A
    ; next Y
    DEC  H
    LD   A, C
    OR   A
    JP   Z, close_vram_ret
    DEC  C
    ; dec row
    DEC  L
    JP   .next_row_up

.next_row_down:
    ; first byte
    LD   A, (HL)
    XOR  E
    AND  B
    XOR  E
    LD   (HL), A
    ; second byte
    INC  H
    LD   A, (HL)
    XOR  D
    AND  B
    XOR  D
    LD   (HL), A
    ; next address
    DEC  H
    LD   A, C
    OR   A
    JP   Z, close_vram_ret
    DEC  C
    ; inc row
    INC  L
    JP   .next_row_down

close_vram_ret:
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

; --------------------------------------------------
; Draw horizontal line
; Inp: DE - YX
;       L - length
; --------------------------------------------------
height0:
    POP  AF
    LD   C, L
    LD   A, L
    OR   A
    JP   NZ, .len_ne0
    INC  C                                          ; length 1 at least
.len_ne0:
    CALL calc_px_addr
    ; make pixel mask
    LD   A, 10000000b
.edl_l19
    RLCA
    DEC  B
    JP   P, .edl_l19
    CPL
    LD   B, A

    EX   DE, HL
    LD   HL, (M_VARS.curr_color)
    EX   DE, HL

    ; Enable VRAM access
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

.next_col:
    ; set 1st byte
    LD   A, (HL)
    XOR  E
    AND  B
    XOR  E
    LD   (HL), A
    ; set 2nd byte
    INC  H
    LD   A, (HL)
    XOR  D
    AND  B
    XOR  D
    LD   (HL), A
    ; next byte
    DEC  H
    ; next (right) horizontal pixel
    LD   A, B
    SCF
    RLA
    JP   C, .edl_l21
    RLA
    INC  H
    INC  H
.edl_l21
    LD   B, A
    DEC  C
    JP   NZ, .next_col
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

; --------------------------------------------------
; ESC Draw Dot
; --------------------------------------------------
esc_draw_dot:
    LD   HL, (M_VARS.esc_param)
    EX   DE, HL
    CALL calc_px_addr
    LD   A, 0x80
.l1:
    RLCA
    DEC  B
    JP   P, .l1
    LD   B, A
    EX   DE, HL
    LD   HL, (M_VARS.curr_color)
    EX   DE, HL
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   A, (HL)
    XOR  B
    LD   (HL), A
    INC  H
    LD   A, (HL)
    XOR  B
    LD   (HL), A
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

; ---------------------------------------------------
; Draw circle
; Inp: param x,y,radius, aspect_x, aspect_y
; ---------------------------------------------------
esc_draw_circle:
    LD   A, (M_VARS.esc_param+2)                    ; radius
    LD   B, A
    OR   A
    RET  Z                                          ; exit ir radius 0
    LD   A, 0x7f
    CP   B
    RET  M                                          ; exit if radius>127

    XOR  A
    LD   D, A                                       ; 0
    LD   E, B                                       ; r
    CALL dc_draw_8px

    LD   A, 1
    LD   H, A
    SUB  B
    LD   C, A
    LD   A, B
    RLCA
    LD   B, A
    LD   A, 0x1
    SUB  B
    LD   L, A
    CCF                                             ; TODO: unused
.l1:
    INC  D
    LD   A, E
    CP   D
    JP   Z, dc_draw_8px
    CALL dc_draw_8px
    LD   A, H
    ADD  A, 0x2
    LD   H, A
    LD   A, L
    ADD  A, 0x2
    LD   L, A
    LD   A, C
    ADD  A, H
    LD   C, A
    JP   NC, .l1
.l2:
    CCF                                             ; TODO: unused
    INC  D
    DEC  E
    LD   A, D
    CP   E
    JP   Z, dc_draw_8px
    SUB  E
    CP   0x1
    RET  Z
    LD   A, E
    SUB  D
    CP   0x1
    JP   Z, dc_draw_8px
    CALL dc_draw_8px
    LD   A, H
    ADD  A, 0x2
    LD   H, A
    LD   A, L
    ADD  A, 0x4
    LD   L, A
    JP   NC, .l3
    CCF                                             ; TODO: unused
.l3:
    LD   A, C
    ADD  A, L
    LD   C, A
    JP   NC, .l1
    JP   .l2

; ---------------------------------------------------
;
; ---------------------------------------------------
dc_draw_8px:
    PUSH HL
    PUSH DE
    PUSH BC
    PUSH DE
    CALL dc_aspect_ratio_1
    LD   HL, (M_VARS.esc_param)                     ; HL=Y,X
    CALL dc_draw_4px_bc
    POP  DE
    CALL dc_aspect_ratio2
    LD   HL, (M_VARS.esc_param)                     ; HL=Y,X
    CALL dc_draw_4px_cb
    POP  BC
    POP  DE
    POP  HL
    XOR  A
    RET

; ---------------------------------------------------
; Scale circle axis dy specified aspect ratio
; if aspect_x = 0  C = D else C = D * aspect_x / 256
; if aspect_y = 0  B = E else B = E * aspect_y / 256
; ---------------------------------------------------
dc_aspect_ratio_1:
    LD   HL, (M_VARS.esc_param+3)                   ; aspect_x -> L, aspect_y -> H
    LD   A, L
    OR   A
    LD   C, D
    LD   B, E
    JP   NZ, .dc_ax_ne0
    LD   A, H
    OR   A
    JP   NZ, .dc_ay_ne0
    RET
.dc_ax_ne0:
    LD   A, H
    LD   H, L
    LD   E, C
    CALL dc_mul_e_h
    LD   C, E
    OR   A
    RET  Z
.dc_ay_ne0:
    LD   H, A
    LD   E, B
    CALL dc_mul_e_h
    LD   B, E
    RET

; ---------------------------------------------------
; if aspect_x = 0 B = E else B = E * aspect_x / 256
; if aspect_y = 0 C = D else C = D * aspect_y / 256
; ---------------------------------------------------
dc_aspect_ratio2:
    LD   HL, (M_VARS.esc_param+3)                   ; aspect_x -> L, aspect_y -> H
    LD   A, L
    OR   A
    LD   C, D
    LD   B, E
    JP   NZ, .dc_ax_ne0
    LD   A, H
    OR   A
    JP   NZ, .dc_ay_ne0
    RET
.dc_ax_ne0:
    LD   A, H
    LD   H, L
    LD   E, B
    CALL dc_mul_e_h
    LD   B, E
    OR   A
    RET  Z

.dc_ay_ne0:
    LD   H, A
    LD   E, C
    CALL dc_mul_e_h
    LD   C, E
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
dc_mul_e_h:
    LD   D, 0x0
    LD   L, D
    ADD  HL, HL
    JP   NC, .l1
    ADD  HL, DE
.l1:
    ADD  HL, HL
    JP   NC, .l2
    ADD  HL, DE
.l2:
    ADD  HL, HL
    JP   NC, .l3
    ADD  HL, DE
.l3:
    ADD  HL, HL
    JP   NC, .l4
    ADD  HL, DE
.l4:
    ADD  HL, HL
    JP   NC, .l5
    ADD  HL, DE
.l5:
    ADD  HL, HL
    JP   NC, .l6
    ADD  HL, DE
.l6:
    ADD  HL, HL
    JP   NC, .l7
    ADD  HL, DE
.l7:
    ADD  HL, HL
    JP   NC, .l8
    ADD  HL, DE
.l8:
    LD   E, H
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
dc_draw_4px_bc:
    ; draw pixel(H+B, L+C) if in screen
    LD   A, H
    ADD  A, B
    JP   C, .l1
    LD   D, A
    LD   A, L
    ADD  A, C
    LD   E, A
    CALL dc_put_pixel
.l1:
    ; draw pixel(H+B, L-C) if in screen
    LD   A, H
    ADD  A, B
    JP   C, .l2
    LD   D, A
    LD   A, L
    SUB  C
    LD   E, A
    CALL dc_put_pixel
.l2:
    ; draw pixel(H-B, L-C) if in screen
    LD   A, H
    SUB  B
    JP   C, .l3
    LD   D, A
    LD   A, L
    SUB  C
    LD   E, A
    CALL dc_put_pixel
.l3:
    ; draw pixel(H-B, L+C) if in screen
    LD   A, H
    SUB  B
    RET  C
    LD   D, A
    LD   A, L
    ADD  A, C
    LD   E, A
    CALL dc_put_pixel                               ; TODO: replace call+ret to jp
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
dc_draw_4px_cb:
    ; draw pixel(H+C, L+B) if in screen
    LD   A, H
    ADD  A, C
    JP   C, .l1
    LD   D, A
    LD   A, L
    ADD  A, B
    LD   E, A
    CALL dc_put_pixel
.l1:
    ; draw pixel(H+C, L-B) if in screen
    LD   A, H
    ADD  A, C
    JP   C, .l2
    LD   D, A
    LD   A, L
    SUB  B
    LD   E, A
    CALL dc_put_pixel
.l2:
    ; draw pixel(H-C, L-B) if in screen
    LD   A, H
    SUB  C
    JP   C, .l3
    LD   D, A
    LD   A, L
    SUB  B
    LD   E, A
    CALL dc_put_pixel
.l3:
    ; draw pixel(H-C, L+B) if in screen
    LD   A, H
    SUB  C
    RET  C
    LD   D, A
    LD   A, L
    ADD  A, B
    LD   E, A
    CALL dc_put_pixel                               ; TODO: replace call+ret to jp
    RET

; ---------------------------------------------------
; Draw pixel on screen
; Inp: DE - X, Y
; ---------------------------------------------------
dc_put_pixel:
    RET  C                                          ; return if CF set (out of screen)
    PUSH HL
    PUSH BC
    CALL calc_px_addr
    ; calculate B = pixel mask
    LD   A, 10000000b
.roll:
    RLCA                                            ; [07654321] <- [76547210], [7] -> CF
    DEC  B
    JP   P, .roll
    CPL
    LD   B, A
    ; DE = foreground color low and hi bytes
    EX   DE, HL
    LD   HL, (M_VARS.curr_color)
    EX   DE, HL
    ; Turn on Video RAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    ; Load VRAM[HL] byte (low byte), mask and set
    LD   A, (HL)
    XOR  E
    AND  B
    XOR  E
    LD   (HL), A
    ; Load VRAM[HL+1] byte (low byte), mask and set
    INC  H
    LD   A, (HL)
    XOR  D
    AND  B
    XOR  D
    LD   (HL), A
    ; Turn off Video RAM
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    POP  BC
    POP  HL
    RET

    ; Full charset, Common + RU letters (160*7=1120b)
    INCLUDE "font-6x7.inc"

; ---------------------------------------------------
; Read hex value from serial interface
; Out: A - value
; ---------------------------------------------------
read_hex_serial:
    PUSH BC
    CALL m_serial_in
    CALL hex_nibble
    RLCA
    RLCA
    RLCA
    RLCA
    LD   C, A
    CALL m_serial_in
    CALL hex_nibble
    OR   C
    LD   C, A
    ADD  A, D
    LD   D, A
    LD   A, C
    POP  BC
    RET

; ---------------------------------------------------
; Convert digit 0..F to character '0'..'F'
; Inp: A - digit
; Out: A - character
; ---------------------------------------------------
hex_to_char:
    AND  0xf
    ADD  A, 0x90
    DAA
    ADC  A, 0x40
    DAA
    LD   C, A
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
out_reg_value:
    INC  HL                                         ; second value in register t
    LD   E, (HL)
    LD   D,  HIGH M_VARS.stor_de                    ; DE -> 0xBFxx - var reg sto
    INC  HL
    LD   B, (HL)
    INC  HL
    LD   A, (DE)                                    ; Load register value
    CALL out_hex_byte
    DEC  B
    RET  M                                          ; ret if single register
    ; print word register
    DEC  DE
    LD   A, (DE)
    JP   out_hex_byte

get_address:
    CALL s_get_hex_addr
    EX   (SP), HL
    PUSH HL
    DEC  C
    JP   NC, ga_word
    JP   NZ, reset_m_stack
    RET
ga_word:
    JP   NZ, get_address
    JP   reset_m_stack


; ---------------------------------------------------
; Next HL address until DE
; Inp: HL -start address
;      DE - end address
; Out: HL = HL + 1
;      CF set if no more
; ---------------------------------------------------
next_hl_de:
    INC  HL
    LD   A, H
    OR   L
    SCF
    RET  Z                                          ; return if HL=0
    LD   A, E
    SUB  L
    LD   A, D
    SBC  A, H
    RET

; ---------------------------------------------------
; Out hex word to screen
; Inp: HL - word to display
; ---------------------------------------------------
out_hex_word:
    LD   A, H
    CALL out_hex_byte
    LD   A, L

; ---------------------------------------------------
; Out hex byte to screen
; Inp: A - byte to display
; ---------------------------------------------------
out_hex_byte:
    PUSH AF
    RRCA
    RRCA
    RRCA
    RRCA
    CALL out_hex_nibble
    POP  AF

; ---------------------------------------------------
;
; ---------------------------------------------------
out_hex_nibble:
    CALL hex_to_char
    JP   get_key_out

; ---------------------------------------------------
;
; ---------------------------------------------------
s_get_hex_addr:
    CALL get_key_with_check
    JP   Z, reset_m_stack

; ---------------------------------------------------
; Out: HL - address
; ---------------------------------------------------
hex_keys_to_addr:
    LD   HL, 0x0
cvt_next_hex:
    LD   B, A
    CALL hex_nibble
    JP   C, non_hex_symb
    ; HL << 4
    ADD  HL, HL                                      ; HL*2
    ADD  HL, HL                                      ; HL*4
    ADD  HL, HL                                      ; HL*8
    ADD  HL, HL                                      ; HL*16
    OR   L
    LD   L, A
    CALL handle_key_pressed
    JP   cvt_next_hex
non_hex_symb:
    LD   A, B
    CALL check_sep_key
    JP   NZ, reset_m_stack                           ; jump if no separator key
    RET

hex_nibble:
    SUB  '0'
    RET  C
    ADD  A, 233
    RET  C
    ADD  A, 6
    JP   P, .is_alpha
    ADD  A, 7
    RET  C
.is_alpha
    ADD  A, 10
    OR   A
    RET

; ---------------------------------------------------
; Read key from keyboard with check for separator cha
; Out: A - key pressed,  converted to Upper Case
;      ZF set if sep or CR
;      CF set if CR
; ---------------------------------------------------
get_key_with_check:
    CALL handle_key_pressed


; ---------------------------------------------------
; Inp: A - key
; Out: ZF set - if ', ' or <CR> or <SP>
;      CF set - if <CR>
; ---------------------------------------------------
check_sep_key:
    CP   ASCII_SP
    RET  Z
    CP   ','
    RET  Z
    CP   ASCII_CR
    SCF
    RET  Z
    CCF
    RET

; ---------------------------------------------------
; Read key,  reset stack if ESC. Print symbol to console
; Out: A - key code
; ---------------------------------------------------
handle_key_pressed:
    PUSH BC
    CALL m_con_in
    CALL hex_to_upper
    CP   ASCII_ESC
    JP   Z, reset_m_stack
    LD   C, A
    CALL m_con_out
    LD   A, C
    POP  BC
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
hex_to_upper:
    CP   'a'
    RET  M
    CP   155
    RET  P
    AND  11011111b
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
m_rst1_handler:
    DI
    LD   (M_VARS.store_hl), HL
    LD   HL, 0x2
    ADD  HL, SP
    LD   (M_VARS.stack_0), HL
    POP  HL
    LD   SP, M_VARS.stack_0
    PUSH AF
    LD   A, 0x0
    PUSH AF
    LD   A, 0x0
    NOP
    NOP
    PUSH BC
    PUSH DE
    LD   A, 0x0
    NOP
    NOP
    LD   (M_VARS.store_pc), HL
    CALL get_cmd_letter
    INC  HL
    CALL out_hex_word
    JP   m_cold_start


; ---------------------------------------------------
; G[addr] Run program from addr
; ---------------------------------------------------
cmd_G:
    CALL get_key_with_check
    JP   Z, .sep
    CALL hex_keys_to_addr
    LD   (M_VARS.store_pc), HL
.sep:
    JP   NC, reset_m_stack                          ; reset if unknown key
    CALL get_a_ref_sp
    LD   A, JP_OPCODE                               ; JP
    LD   (M_VARS.start_jp), A
    LD   SP, M_VARS.stor_e
    ; restore registers and stack
    POP  DE
    POP  BC
    POP  AF
    NOP
    NOP
    POP  AF
    POP  HL
    LD   SP, HL
    ; restore HL and start code at specified address
    LD   HL, (M_VARS.store_hl)
    JP   M_VARS.start_jp

; ---------------------------------------------------
; W - Write RAM to tape
; ---------------------------------------------------
cmd_W:
    CALL get_key_with_check
    JP   NC, reset_m_stack
    CALL get_a_ref_sp

; ---------------------------------------------------
; Wtite RAM-Disk 64K to TAPE
; ---------------------------------------------------
m_tape_write_ram:
    LD   HL, M_VARS.buffer
    LD   C, 128
.cl_stack:
    LD   (HL), 0x0
    INC  HL
    DEC  C
    JP   NZ, .cl_stack
    LD   HL, M_VARS.buffer
    LD   DE, 0xffff
    ; write empty block
    ; DE - block ID
    ; HL -> block
    CALL m_tape_write
    CALL twr2_delay
    LD   DE, 0x0
    CALL m_tape_write
    CALL twr2_delay
    LD   BC, 512
    LD   DE, 0x0
.nxt_blk:
    PUSH BC
    LD   HL, M_VARS.buffer
    CALL m_ramdisk_read
    INC  DE
    CALL m_tape_write
    CALL twr2_delay
    POP  BC
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, .nxt_blk
    RET

; ---------------------------------------------------
; Pause between blocks on tape
; ---------------------------------------------------
twr2_delay:
    LD   BC, 250
.delay:
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, .delay
    RET

; ---------------------------------------------------
; R - Read from tape to RAM
; ---------------------------------------------------
cmd_R:
    CALL get_key_with_check
    JP   NC, reset_m_stack
    CALL get_a_ref_sp

; ---------------------------------------------------
; Read RAM-Disk 64K from TAPE
; ---------------------------------------------------
m_tape_read_ram:
    LD   A, 100
    CALL m_tape_wait
    OR   A
    JP   NZ, .end
    LD   E, 6

.srch_first:
    DEC  E
    JP   Z, .not_found
    ; read block
    LD   HL, M_VARS.buffer
    CALL m_tape_read
    CP   4
    JP   Z, .end
    OR   A
    JP   NZ, .srch_first
    LD   A, B
    OR   C
    JP   NZ, .srch_first

    LD   BC, 512
    LD   DE, 0x0

.rd_next:
    PUSH BC
    ; Read block from tape
    CALL m_tape_read
    OR   A
    JP   NZ, .rd_error
    DEC  BC
    LD   A, B
    CP   D
    JP   NZ, .inv_id
    LD   A, C
    CP   E
    JP   NZ, .inv_id
    ; Ok,  write block to RAM disk
    CALL m_ramdisk_write
    INC  DE
    POP  BC
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, .rd_next
    RET
.not_found:
    LD   HL, msg_no_start_rec
    CALL me_out_strz                               ; TODO: replace call+ret to jp
    RET
.rd_error:
    CP   2
    JP   Z, .err_ubi
    CP   4
    JP   Z, .err_ibu
    LD   HL, msg_checksum
    CALL me_out_strz
    CALL out_hexw
    POP  BC
    RET

    ; Illegal sequence of blocks
.inv_id:
    LD   HL, msg_sequence
    CALL me_out_strz
    INC  BC
    CALL out_hexw
    POP  BC
    RET

.err_ubi:
    LD   HL, msg_ibg
    CALL me_out_strz
    POP  BC
    RET

    ; Interrupted by user
.err_ibu:
    POP  BC

.end:
    LD   HL, msg_break
    CALL me_out_strz                                ; TODO: replace call+ret to jp
    RET

; --------------------------------------------------
; Output hex word
; Inp: BC - word to output
; --------------------------------------------------
out_hexw:
    PUSH BC
    LD   A, B
    CALL out_hex_byte
    POP  BC
    LD   A, C
    CALL out_hex_byte                                     ; TODO: replace call+ret to jp
    RET

msg_no_start_rec:
    DB "NO START RECORD",  0
msg_checksum:
    DB "CHECKSUM ",  0
msg_sequence:
    DB "SEQUENCE ",  0
msg_ibg:
    DB  "IBG",  0
msg_break:
    DB  "BREAK",  0

; ---------------------------------------------------
; Out ASCIIZ message
; Inp: HL -> zero ended string
; ---------------------------------------------------
me_out_strz:
    LD   A, (HL)
    OR   A
    RET  Z
    PUSH BC
    LD   C, A
    CALL m_con_out
    INC  HL
    POP  BC
    JP   me_out_strz



; ---------------------------------------------------
; Read from RAM-disk to RAM
; Inp: DE - source sector
;      HL -> destination buffer
; ---------------------------------------------------
m_ramdisk_read:
    PUSH HL
    PUSH DE
    LD   A, D
    ; Build value to access ext RAM  (A17, A16, 32k bits)
    AND  00000001b                                  ; Low 32K
    OR   0x2                                        ; Set A16 address line
    OR   0x0                                        ; TODO: nothing, remove
    LD   B, A                                       ; B - value to turn on access to Ext RAM
    ; Calculate DE = address from sector number
    XOR  A
    LD   A, E                                       ; E - low address
    RRA                                             ; [CF] -> [7:0] -> [CF]
    LD   D, A                                       ; D = E/2
    LD   A, 0x0
    RRA                                             ; [CF] -> E
    LD   E, A
.read:
    ; Access to ExtRAM
    LD   A, B
    OUT  (SYS_DD17PB), A
    ; Get Byte
    LD   A, (DE)
    LD   C, A
    ; Access to RAM
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    ; Set Byte
    LD   (HL), C
    ; HL++, DE++
    INC  HL
    INC  DE
    LD   A, E
    ADD  A, A
    JP   NZ, .read                                  ; jump if has more bytes

    ; Access to RAM
    LD   A, 0x0
    OUT  (SYS_DD17PB), A

    POP  DE
    POP  HL
    RET

; ---------------------------------------------------
; Write sector to RAM disk
; Inp: HL -> source buffer
;      DE - destination sector
; ---------------------------------------------------
m_ramdisk_write:
    PUSH HL
    PUSH DE
    LD   A, D
    AND  0x1
    OR   0x2                                        ; build value to access ext RAM  (A16, 32k bits)
    OR   0x0                                        ; TODO: remove unused
    LD   B, A
    XOR  A
    LD   A, E
    RRA
    LD   D, A
    LD   A, 0x0
    RRA
    LD   E, A
.wr_byte:
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    LD   C, (HL)
    LD   A, B
    OUT  (SYS_DD17PB), A
    LD   A, C
    LD   (DE), A
    INC  HL
    INC  DE
    LD   A, E
    ADD  A, A
    JP   NZ, .wr_byte
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    POP  DE
    POP  HL
    RET

; --------------------------------------------------
;  Write block to Tape
;  Inp: DE - block ID,
;       HL -> block of data.
; --------------------------------------------------
m_tape_write:
    PUSH HL
    PUSH DE
    PUSH DE
    LD   BC, 2550
    LD   A, PIC_POLL_MODE                           ; pool mode
    OUT  (PIC_DD75RS), A
    LD   A,TMR0_SQWAVE                              ; tmr0, load lsb+msb, sq wave, bin
    OUT  (TMR_DD70CTR), A
    LD   A, C
    OUT  (TMR_DD70C1), A
    LD   A, B
    OUT  (TMR_DD70C1), A
    ; Write Hi+Lo, Hi+Lo
    LD   DE, 4                                      ; repeat next 4 times
.l1:
    IN   A, (PIC_DD75RS)
    AND  TIMER_IRQ                                  ; check rst4 from timer#0
    JP   NZ, .l1
    LD   A, D
    CPL
    LD   D, A
    OR   A
    LD   A, TL_HIGH                                 ; tape level hi
    JP   NZ, .set_lvl
    LD   A, TL_LOW                                  ; tape level low
.set_lvl:
    OUT (DD67PC), A                                 ; set tape level
    LD   A, TMR0_SQWAVE                             ; tmr0, load lsb+msb, swq, bin
    ; timer on
    OUT  (TMR_DD70CTR), A
    LD   A, C
    OUT  (TMR_DD70C1), A
    LD   A, B
    OUT  (TMR_DD70C1), A
    DEC  E
    JP   NZ, .l1

.l2:
    IN  A, (PIC_DD75RS)
    AND TIMER_IRQ
    JP  NZ, .l2

    ; Write 00 at start
    LD  A, 0x0
    CALL m_tape_wr_byte
    ; Write 0xF5 marker
    LD  A, 0xf5
    CALL m_tape_wr_byte
    LD  E, 0x0                                      ; checksum=0
    ; Write block ID
    POP BC
    LD  A, C
    CALL m_tape_wr_byte
    LD  A, B
    CALL m_tape_wr_byte
    ; Write 128 data bytes
    LD  B, 128
.next_byte:
    LD  A, (HL)
    CALL m_tape_wr_byte
    INC HL
    DEC B
    JP  NZ, .next_byte
    ; Write checksum
    LD  A, E
    CALL m_tape_wr_byte
    ; Write final zero byte
    LD  A, 0x0
    CALL m_tape_wr_byte
.wait_end:
    IN  A, (PIC_DD75RS)
    AND TIMER_IRQ
    JP  NZ, .wait_end
    LD  A, TL_MID                                   ; tape level middle
    OUT (DD67PC), A
    POP DE
    POP HL
    RET


; ------------------------------------------------------
;  Write byte to tape
;  Inp: A - byte top write
;       D - current level
;       E - current checksum
; ------------------------------------------------------
m_tape_wr_byte:
    PUSH BC
    ; calc checksum
    LD   B, A
    LD   A, E
    SUB  B
    LD   E, A
    LD   C, 8                                       ; 8 bit in byte
.get_bit:
    LD   A, B
    RRA
    LD   B, A
    JP   C, .bit_hi
.wait_t:
    IN   A, (PIC_DD75RS)
    AND  TIMER_IRQ
    JP   NZ, .wait_t
    LD   A, TMR0_SQWAVE
    OUT  (TMR_DD70CTR), A
    ; program for 360 cycles
    LD   A, 0x68
    OUT  (TMR_DD70C1), A
    LD   A, 0x1
    OUT  (TMR_DD70C1), A
    ; change amplitude
    LD   A, D
    CPL
    LD   D, A
    OR   A
    LD   A, TL_HIGH
    JP   NZ, .out_bit
    LD   A, TL_LOW
.out_bit:
    OUT  (DD67PC), A
    DEC  C
    JP   NZ,.get_bit
    POP  BC
    RET
.bit_hi:
    IN   A, (PIC_DD75RS)
    AND  TIMER_IRQ
    JP   NZ, .bit_hi
    ; program for 660 cycles
    LD   A, TMR0_SQWAVE
    OUT  (TMR_DD70CTR), A
    LD   A, 0x94
    OUT  (TMR_DD70C1), A
    LD   A, 0x2
    OUT  (TMR_DD70C1), A
    ; change amplitude
    LD   A, D
    CPL
    LD   D, A
    OR   A
    LD   A, TL_HIGH
    JP   NZ, .out_bit_hi
    LD   A, TL_LOW
.out_bit_hi:
    OUT  (DD67PC), A
    DEC  C
    JP   NZ, .get_bit
    POP  BC
    RET

; ------------------------------------------------------
;  Load block from Tape
;  Inp: HL -> buffer to receive bytes from Tape
;  Out: A = 0 - ok,
;       1 - CRC error,
;       2 - unexpected block Id
;       4 - key pressed
; ------------------------------------------------------
m_tape_read:
    PUSH HL
    PUSH DE
    LD   A, PIC_POLL_MODE                          ; pool mode
    OUT  (PIC_DD75RS), A
    LD   A, TMR0_SQWAVE
    OUT  (TMR_DD70CTR), A                          ; tmr0, load lsb+msb, sq wave
    LD   A, 0x0
    ; tmr0 load 0x0000
    OUT  (TMR_DD70C1), A
    OUT  (TMR_DD70C1), A
    LD   C, 3
.wait_3_changes:
    CALL read_tape_bit_kbd
    INC  A
    JP   Z, .key_pressed
    LD   A, B
    ADD  A, 4
    JP   P, .wait_3_changes
    DEC  C
    JP   NZ, .wait_3_changes
.wait_4th_change:
    CALL read_tape_bit_kbd
    INC  A
    JP   Z, .key_pressed
    LD   A, B
    ADD  A, 4
    JP   M, .wait_4th_change
    LD   C, 0x0
.wait_f5_marker:
    CALL read_tape_bit_kbd
    INC  A
    JP   Z, .key_pressed
    DEC  A
    RRA
    LD   A, C
    RRA
    LD   C, A
    CP   0xf5
    JP   NZ, .wait_f5_marker
    LD   E, 0x0                                     ; checksum = 0
    ; Read blk ID
    CALL m_tape_read_byte
    JP   NC, .err_read_id
    LD   C, D
    CALL m_tape_read_byte
    JP   NC, .err_read_id
    LD   B, D
    PUSH BC
    ; Read block, 128 bytes
    LD   C, 128
.read_next_b:
    CALL m_tape_read_byte
    JP   NC, .err_read_blk
    LD   (HL), D
    INC  HL
    DEC  C
    JP   NZ, .read_next_b

    ; Read checksum
    CALL m_tape_read_byte
    JP   NC, .err_read_blk
    LD   A, E
    OR   A
    JP   Z, .checksum_ok
    LD   A, 0x1                                     ; bad checksum
.checksum_ok:
    POP  BC
.return:
    POP  DE
    POP  HL
    RET

.err_read_blk:
    POP  BC
    LD   BC, 0x0
.err_read_id:
    LD   A, 0x2                                     ; read error
    JP   .return
.key_pressed:
    CALL m_con_in
    LD   C, A                                       ; store key code in C
    LD   B, 0x0
    LD   A, 0x4
    JP   .return

; ------------------------------------------------------
;  Read byte from Tape
;  Out: D - byte
;       CF is set if ok, cleared if error
; ------------------------------------------------------
m_tape_read_byte:
    PUSH BC
    LD   C, 8
.next_bit:
    CALL m_read_tape_bit
    ; push bit from lo to hi in D
    RRA
    LD   A, D
    RRA
    LD   D, A
    LD   A, 4
    ADD  A, B
    JP   NC, .ret_err
    DEC  C
    JP   NZ, .next_bit
    ; calc checksum
    LD   A, D
    ADD  A, E
    LD   E, A
    SCF
.ret_err:
    POP  BC
    RET

; ------------------------------------------------------
;  Read bit from tape
;  Out: A - bit from tape
;       B - time from last bit
; ------------------------------------------------------
m_read_tape_bit:
    IN   A, (KBD_DD78PB)                            ; Read Tape bit 5 (data)
    AND  TAPE_P
    LD   B, A
.wait_change:
    IN   A, (KBD_DD78PB)
    AND  TAPE_P
    CP   B
    JP   Z, .wait_change
    LD   A, TMR0_SQWAVE
    OUT  (TMR_DD70CTR), A
    ; [360...480...660] 0x220=544d
    IN   A, (TMR_DD70C1)                            ; get tmer#0 lsb
    ADD  A, 0x20
    IN   A, (TMR_DD70C1)                            ; get tmer#0 msb
    LD   B, A
    ADC  A, 0x2
    ; reset timer to 0
    LD   A, 0x0
    OUT  (TMR_DD70C1), A
    OUT  (TMR_DD70C1), A
    ; For 0 - 65535-360+544 -> overflow P/V=1
    ; For 1 - 65535-660+544 -> no overflow P/V=0
    RET  P
    INC  A
    RET

; ------------------------------------------------------
;  Read bit from tape with keyboard interruption
;  Out: A - bit from tape
;       B - time from last bit
; ------------------------------------------------------
read_tape_bit_kbd:
    IN   A, (KBD_DD78PB)
    AND  TAPE_P
    LD   B, A                                       ; save tape bit state
    ; wait change with keyboard check
.wait_change:
    IN   A, (PIC_DD75RS)
    AND  KBD_IRQ
    JP   NZ, .key_pressed
    IN   A, (KBD_DD78PB)
    AND  TAPE_P
    CP   B
    JP   Z, .wait_change
    ; measure time
    LD   A, TMR0_SQWAVE
    OUT  (TMR_DD70CTR), A
    ; read lsb+msb
    IN   A, (TMR_DD70C1)
    ADD  A, 0x20
    IN   A, (TMR_DD70C1)
    LD   B, A
    ADC  A, 0x2
    ; reset timer#0
    LD   A, 0x0
    OUT  (TMR_DD70C1), A
    OUT  (TMR_DD70C1), A
    ; flag P/V is set for 0
    RET  P
    INC  A
    RET
.key_pressed:
    LD   A, 0xff
    RET

; ------------------------------------------------------
;  Wait tape block
;  Inp: A - periods to wait
;  Out: A=4 - interrupded by keyboard, C=key
; ------------------------------------------------------
m_tape_wait:
    OR   A
    RET  Z
    PUSH DE
    LD   B, A
.wait_t4:
    LD   C,B
    IN   A, (KBD_DD78PB)
    AND  TAPE_P                                     ; Get TAPE4 (Wait det) and save
    LD   E, A                                       ; store T4 state to E
.wait_next_2ms:
    LD   A,TMR0_SQWAVE
    OUT  (TMR_DD70CTR), A
    ; load 3072 = 2ms
    XOR  A
    OUT  (TMR_DD70C1), A
    LD   A, 0xc
    OUT  (TMR_DD70C1), A
.wait_tmr_key:
    IN   A, (PIC_DD75RS)
    AND  KBD_IRQ                                    ; RST1 flag (keyboard)
    JP   NZ, .key_pressed
    IN   A, (PIC_DD75RS)
    AND  TIMER_IRQ                                  ; RST4 flag (timer out)
    JP   Z, .wait_no_rst4
    IN   A, (KBD_DD78PB)
    AND  TAPE_P                                     ; TAPE4 not changed?
    CP   E
    JP   NZ, .wait_t4                               ; continue wait
    JP   .wait_tmr_key
.wait_no_rst4:
    DEC  C
    JP   NZ, .wait_next_2ms
    XOR  A
    POP  DE
    RET

.key_pressed:
    CALL m_con_in
    LD   C, A                                       ; C = key pressed
    LD   A, 0x4                                     ; a=4 interrupted by key
    POP  DE
    RET

; ------------------------------------------------------
;  Check block marker from Tape
;  Out: A=0 - not detected, 0xff - detected
; ------------------------------------------------------
m_tape_blk_detect:
    IN   A, (KBD_DD78PB)
    AND  TAPE_D                                      ; TAPE5 - Pause detector
    LD   A, 0x0
    RET  Z
    CPL
    RET

; ======================================================
; FDC DRIVER
; ======================================================
fdc_unload_head:
    LD   A,0x0
    OUT  (FDC_DATA),A
    LD   A,0x3
    OUT  (FDC_CMD),A
    NOP
    NOP
fuh.wait_no_busy:
    IN   A,(FDC_CMD)
    AND  0x5                                         ; TR0. BYSY
    CP   0x4                                         ;
    JP   Z,fuh.tr0_ok
    IN   A,(FLOPPY)
    RLCA                                             ; MOT_ST -> CF
    JP   NC,fuh.wait_no_busy
    LD   A,0x20
    RET
fuh.tr0_ok:
    LD   A,0x1
    LD   (M_VARS.ul_var1),A
    XOR  A
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
m_select_drive:
    CALL delay_1.4mS
    CP   0x1
    JP   Z, .sel_a
    LD   A, 0x2
    JP   .sel_b
.sel_a:
    LD   A, 0x5
.sel_b:
    LD   B, A
    IN   A, (FLOPPY)
    AND  0x40
    RRA
    OR   B
    OUT  (FLOPPY), A
    LD   B, A
    LD   A, D
    CP   0x28
    JP   C, .l3
    SUB  0x28
    LD   D, A
    LD   A, C
    OR   0x8
    LD   C, A
    LD   A, B
    AND  0x20
    OR   A
    RET  NZ
    LD   A, B
    OR   0x20
    OUT  (FLOPPY), A
    CALL delay_136uS
    RET
.l3:
    LD   A, B
    AND  0x20
    OR   A
    RET  Z
    LD   A, B
    AND  0x7
    OUT  (FLOPPY), A
    CALL delay_136uS
    RET


; ---------------------------------------------------
; Delay for 136uS
; ---------------------------------------------------
delay_136uS:
    LD   B, 16                                      ; 7

; ---------------------------------------------------
; Delay for B*8uS
; ---------------------------------------------------
delay_b:
    DEC  B                                          ; 4
    JP   NZ, delay_b                                ; 10
    RET                                             ; 10

; ---------------------------------------------------
; Delay for 1.4mS
; ---------------------------------------------------
delay_1.4mS:
    LD   B, 175                                     ; 7
    JP   delay_b                                    ; 10

; ---------------------------------------------------
;
; ---------------------------------------------------
m_read_floppy:
    CALL m_select_drive
    CALL m_start_seek_track
    JP   C, fdc_ret
    CALL m_fdc_read_c_bytes
    JP   C, fdc_ret
    XOR  A
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
m_write_floppy:
    CALL m_select_drive
    CALL m_start_seek_track
    JP   C, fdc_ret
    CALL m_fdc_write_bytes
    JP   C, fdc_ret
    XOR  A
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
m_start_seek_track:
    CALL m_start_floppy
    RET  C
    CALL m_fdc_seek_trk
    RET  C
    RET                                             ; TODO: remove

; ---------------------------------------------------
;
; ---------------------------------------------------
m_start_floppy:
    IN   A, (FLOPPY)
    RLCA
    JP   C, .wait_motor_st
    IN   A, (FDC_CMD)
    AND  0x80
    RET  Z
.wait_motor_st:
    PUSH BC
    LD   BC, 64000
    CALL fdc_init
.wait_rdy1:
    IN   A, (FDC_CMD)
    AND  0x80
    JP   Z, .long_delay
    IN   A, (FLOPPY)
    RLCA
    JP   NC, .wait_rdy1
    LD   A, 0x20
    JP   .mot_st_exit
.long_delay:
    DEC  C
    JP   NZ, .long_delay
    DEC  B
    JP   NZ, .long_delay
    XOR  A
.mot_st_exit:
    POP  BC
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
fdc_init:
    IN   A, (FLOPPY)
    AND  01001110b                                  ; Get SSEL,  DRSEL,  MOT1,  MOT0
    RRA
    OUT  (FLOPPY), A
    OR   0x08                                       ; Set INIT bit
    OUT  (FLOPPY), A
    RET

; ---------------------------------------------------
; Seek track on floppy
;   Inp: DE - track/sector
; ---------------------------------------------------
m_fdc_seek_trk:
    LD   A, (M_VARS.ul_var1)
    AND  0x1
    CALL Z, fdc_unload_head
    LD   A, D
    OUT  (FDC_DATA), A
    LD   A, 0x1f
    OUT  (FDC_CMD), A
    NOP
    NOP
    IN   A, (FDC_WAIT)
    IN   A, (FDC_CMD)
    AND  ASCII_EM
    CP   0x0
    JP   NZ, .l1
    JP   .l2
.l1:
    LD   A, D
    OUT  (FDC_DATA), A
    LD   A, 0x2f
    OUT  (FDC_CMD), A
    NOP
    NOP
    IN   A, (FDC_WAIT)
    IN   A, (FDC_CMD)
    AND  ASCII_EM
    CP   0x0
    JP   Z, .l2
    SCF
    LD   A, 0x40
.l2:
    PUSH AF
    LD   A, E
    OUT  (FDC_SECT), A
    POP  AF
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
m_fdc_write_bytes:
    LD   A, C
    OUT  (FDC_CMD), A
.w_next:
    IN   A, (FDC_WAIT)
    RRCA
    LD   A, (HL)
    OUT  (FDC_DATA), A
    INC  HL
    JP   C, .w_next
    CALL fdc_check_status                           ; TODO: replace call+ret to jp
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
m_fdc_read_c_bytes:
    LD   A, C
    OUT  (FDC_CMD), A
    JP   .l2
.l1:
    LD   (HL), A
    INC  HL
.l2:
    IN   A, (FDC_WAIT)
    RRCA
    IN   A, (FDC_DATA)
    JP   C,  .l1
    CALL fdc_check_status                           ; TODO: replace call+ret to jp
    RET

; ---------------------------------------------------
; Check fdc status for errors
; Out: CF set if errors
; ---------------------------------------------------
fdc_check_status:
    IN        A,(FDC_CMD)
    AND       11011111b
    CP        0x0
    JP        Z,fdc_ret
    IN        A,(FDC_CMD)
    CP        0x80
    LD        C,0x80
    JP        Z,fcs_error
    CP        0x40
    LD        C,0x3
    JP        Z,fcs_error
    CP        0x10
    LD        C,0x4
    JP        Z,fcs_error
    CP        0x8
    LD        C,0x9
    JP        Z,fcs_error
    LD        C,0x10
fcs_error:
    SCF
    LD        A,C
fdc_ret:
    RET

    DB  "\r\n DRIVE IS NOT READY \r\n", 0, "I"

; ------------------------------------------------------

LAST        EQU     $
CODE_SIZE   EQU     LAST-0xe000
FILL_SIZE   EQU     ROM_CHIP_SIZE-CODE_SIZE


    ASSERT m_start             = 0xe036
    ASSERT conf_uart           = 0xe08c
    ASSERT conf_pic            = 0xe09f
    ASSERT jump_bios           = 0xe0d6
    ASSERT m_out_strz          = 0xe0e7
    ASSERT m_cold_start        = 0xe10c
    ASSERT registers_tab       = 0xe27e
    ASSERT m_char_print        = 0xe2e8
    ASSERT m_con_out           = 0xe307
    ASSERT m_con_out_int       = 0xe311
    ASSERT get_esc_param       = 0xe32b
    ASSERT esc_params_tab      = 0xe3ae
    ASSERT esc_set_beep        = 0xe3dd
    ASSERT esc_print_screen    = 0xe3fb
    ASSERT m_print_hor_line    = 0xe424
    ASSERT m_get_7vpix         = 0xe49e
    ASSERT esc_set_palette     = 0xe4e8
    ASSERT m_get_glyph         = 0xe50a
    ASSERT m_print_no_esc      = 0xe533
    ASSERT calc_addr_40        = 0xe6b2
    ASSERT mp_mode_64          = 0xe72e
    ASSERT scroll_up           = 0xe777
    ASSERT mp_mode_80          = 0xe7c2
    ASSERT m80_rt              = 0xe85d
    ASSERT calc_addr_80        = 0xe8a0
    ASSERT m_clear_screen      = 0xe900
    ASSERT m_cursor_home       = 0xe933
    ASSERT m_draw_cursor       = 0xe961
    ASSERT m_handle_esc_code   = 0xeb04
    ASSERT handle_cc_common    = 0xeb4c
    ASSERT handle_cc_mono      = 0xeb87
    ASSERT handle_cc_80x25     = 0xebce
    ASSERT m_beep              = 0xebf5
    ASSERT esc_set_cursor      = 0xec2b
    ASSERT esc_set_vmode       = 0xec8b
    ASSERT esc_set_color       = 0xecbc
    ASSERT m_print_at_xy       = 0xecd0
    ASSERT calc_px_addr        = 0xed4b
    ASSERT esc_draw_fill_rect  = 0xed5e
    ASSERT esc_draw_line       = 0xedf7
    ASSERT esc_draw_dot        = 0xef3e
    ASSERT esc_draw_circle     = 0xef62
    ASSERT dc_put_pixel        = 0xf0a8
    ASSERT get_key_with_check  = 0xf5ce
    ASSERT m_tape_write_ram    = 0xf654
    ASSERT m_tape_read_ram     = 0xf6a4
    ASSERT m_ramdisk_read      = 0xf76a
    ASSERT m_ramdisk_write     = 0xf794
    ASSERT m_tape_write        = 0xf7be
    ASSERT m_tape_wr_byte      = 0xf835
    ASSERT m_tape_read         = 0xf88e
    ASSERT m_read_tape_bit     = 0xf92f
    ASSERT m_tape_wait         = 0xf97f
    ASSERT m_select_drive      = 0xf9e8

    ASSERT m_read_floppy       = 0xfa36
    ASSERT m_write_floppy      = 0xfa47
    ASSERT m_start_floppy      = 0xfa61
    ASSERT fdc_init            = 0xfa90
    ASSERT m_fdc_seek_trk      = 0xfa9c
    ASSERT m_fdc_write_bytes   = 0xfad8
    ASSERT m_fdc_read_c_bytes  = 0xfae9
    ASSERT fdc_check_status    = 0xfafd


    ;DISPLAY "m_handle_esc_code: ", /H, m_handle_esc_code

FILLER
    DS  FILL_SIZE, 0xFF
    DISPLAY "Free size is: ", /D, FILL_SIZE, " bytes."

    ENDMODULE

    OUTEND

    OUTPUT m_vars.bin
        ; put in separate waste file
        INCLUDE "m_vars.inc"
    OUTEND
