; ======================================================
; Ocean-240.2
; Monitor V8.8
;
; Disassembled by Romych 2026-02-05
; ======================================================

    DEVICE NOSLOT64K

    INCLUDE "io.inc"
    INCLUDE "equates.inc"
    INCLUDE "ram.inc"
    INCLUDE "bios_entries.inc"

    OUTPUT monitor_E000.bin


    MODULE  MONITOR

    ORG     0xE000

; ------------------------------------------------------
; Monitor Entry points
; ------------------------------------------------------

start:              JP  m_hot_start                 ; E000
mon_hexb:           JP  m_hexb                      ; E003
non_con_status:     JP  m_con_status                ; E006
mon_con_in:         JP  m_con_in                    ; E009
mon_con_out:        JP  m_con_out                   ; E00C
mon_serial_in:      JP  m_serial_in                 ; E00F
mon_serial_out:     JP  m_serial_out                ; E012
mon_char_print:     JP  m_char_print                ; E015
mon_tape_read:      JP  m_tape_read                 ; E018
mon_tape_write:     JP  m_tape_write                ; E01B
ram_disk_read:      JP  m_ramdisk_read              ; E01E
ram_disk_write:     JP  m_ramdisk_write             ; E021
mon_tape_read_ram:  JP  m_tape_read_ram2            ; E024
mon_tape_write_ram: JP  m_tape_write_ram2           ; E027
mon_tape_wait:      JP  m_tape_wait                 ; E02A
mon_tape_detect:    JP  m_tape_blk_detect           ; E02D
read_floppy:        JP  m_read_floppy               ; E030
write_floppy:       JP  m_write_floppy              ; E033
mon_out_str_z:      JP  m_out_strz                  ; E036
                    JP  m_fn_39                     ; E039
                    JP  get_image_hdr               ; E03C
                    JP  esc_picture                 ; E03F
                    JP  m_print_at_xy             ; E042  C-char esc_param[0]=X, [1]=Y
                    JP  esc_draw_fill_rect          ; E045
                    JP  esc_paint                   ; E048
                    JP  esc_draw_line               ; E04B
                    JP  esc_draw_circle             ; E04E


; ------------------------------------------------------
; Init system devices
; ------------------------------------------------------
m_hot_start:
    DI
   ;LD   SP, M_VARS.rst_ret_jp
    LD   A, 10000000b                               ; DD17 all ports to out
    OUT  (SYS_DD17CTR), A                           ; VV55 Sys CTR
    OUT  (DD67CTR), A                               ; VV55 Video CTR

    ; init_kbd_tape
    LD   A, 0x93
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

.conf_uart:
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
.conf_pic:
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

    LD   A, 0x80
    OUT  (KBD_DD78PC), A                            ; TODO: - Check using this 7th bit
    NOP
    NOP
    XOR  A
    OUT  (KBD_DD78PC), A

    ; Init cursor
    LD   SP, M_VARS.stack1
    CALL m_draw_cursor

    ; Beep
    LD   C, ASCII_BELL
    CALL m_con_out

    LD   A, (BIOS.boot_f)
    CP   JP_OPCODE
    JP   Z, BIOS.boot_f
    LD   HL, mgs_system_nf
    CALL m_out_strz
    JP   m_sys_halt

; --------------------------------------------------
; Output ASCIIZ string
; Inp: HL -> string
; --------------------------------------------------
m_out_strz:
    LD   C, (HL)
    LD   A, C
    OR   A
    RET  Z
    CALL m_con_out
    INC  HL
    JP   m_out_strz

mgs_system_nf:
    DB "\r\nSYSTEM NOT FOUND\r\n", 0

m_sys_halt:
    HALT

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
;  Send character to printer
;  Inp: C - character
; ------------------------------------------------------
m_char_print:
    ; wait printer ready
    IN   A, (PIC_DD75RS)
    AND  PRINTER_IRQ
    JP   Z, m_char_print

    LD   A, C
    NOP
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
    LD   B, (HL)                                    ; TODO: replace to INC L  L=0xd4 save 2t
    INC  HL                                         ; HL -> param count
    LD   A, (HL)
    INC  A
    LD   (HL), A
    ; store new param
    LD   E, A
    LD   D, 0x0
    ADD  HL, DE                                     ; HL -> parameter[param_count]
    LD   (HL), C                                    ; store letter as esc parameter
    ; get params count for esc command
    LD   HL, esc_params_tab
    LD   E, B                                       ; d=0, b = cmd
    ADD  HL, DE                                     ; DE - command offset
    CP   (HL)
    ; return if enough
    RET  M

;esc_set_mode:
    LD   HL, M_VARS.esc_cmd
    LD   A, (HL)
    AND  0x0f                                       ; mask (cmd=0..15)
    LD   E, A
    DEC  HL                                         ; HL -> esc_mode
    OR   A
    LD   (HL), 2                                    ; mode=2 for cmd=0
    RET  Z                                          ; just return, no handler there

    LD   D, 0                                       ; TODO: remove, D already 0
    LD   (HL), D                                    ; reset mode to 0 for other
    DEC  DE                                         ; DE = cmd-1

;co_get_hdlr:
    ; Calc ESC command handler offset
    LD   HL, esc_handler_tab
    ADD  HL, DE
    ADD  HL, DE
    LD   E, (HL)
    INC  HL
    LD   D, (HL)
    ; HL = addr of handler func
    EX   DE, HL
    ; It is 1..4 func DRAW_* func?
    CP   0x4
    JP   P, esc_no_draw_fn
    LD   A, (M_VARS.screen_mode)
    AND  00000011b
    ; If not in graphics mode - exit
    JP   NZ, esc_exit

esc_no_draw_fn:
    LD   DE, esc_exit
    PUSH DE

    ; Jump to ESC func handler
    JP   (HL)

esc_exit:
    XOR  A
    LD   (M_VARS.esc_mode), A
    RET

    ; Count of parameters for ESC commands
    ; 0xe1cb
esc_params_tab:
    DB   3, 5, 4, 3, 1, 2, 1, 1
    DB   1, 2, 1, 5, 5, 7, 6, 4

esc_handler_tab:
    DW   esc_draw_fill_rect                         ;5 <ESC>1x1y1x2y2m
    DW   esc_draw_line                              ;4 <ESC>2x1y1x2y2
    DW   esc_draw_dot                               ;3 <ESC>3xxyy
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
    DW   esc_set_cursor2                            ;2 <ESC>9xy
    DW   esc_print_screen                           ;1 <ESC>:
    DW   esc_draw_circle                            ;5 <ESC>;xyraxay   X,Y, Radius, aspect ratio X, aspect ratio Y
    DW   esc_paint                                  ;5 <ESC><xym
    DW   esc_get_put_image                          ;7 <ESC>=
    DW   esc_picture                                ;6 <ESC>>
    DW   esc_set_beep                               ;4 <ESC>?ppdd   pp-period (word), dd - duration (word)

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

esc_set_cursor2:
    JP   esc_set_cursor

esc_print_screen:
    LD   A, (M_VARS.screen_mode)
    AND  00000011b
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
; 0xe2a5
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

esc_set_palette:
    LD  A, (M_VARS.esc_param)
    AND 00111111b                                   ; bgcol[2,1,0],pal[2,1,0]
    LD  (M_VARS.cur_palette), A
    LD  B, A
    LD  A, (M_VARS.screen_mode)
    AND 00000011b
    LD  A, 0x0
    JP  NZ, esp_no_colr
    LD  A, 0x40

esp_no_colr:
    OR   B
    OUT  (VID_DD67PB), A
    RET

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
    AND  0x7f                                       ; C = 0..127 ASCII code
    CP   ASCII_SP                                   ; C < ' '?
    JP   M, m_handle_esc_code                       ; jump if less
    CALL m_get_glyph
    EX   DE, HL
    LD   A, (M_VARS.screen_mode)
    AND  0x3
    JP   NZ, mp_mode_64                             ; jump to non color modes

    CALL calc_addr_40
    INC  L
    ; Access to VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    DEC  H
    DEC  H
    ; one or two bytes
    LD   A, B
    OR   B
    JP   Z, .l1
    DEC  B
    JP   Z, .l2
    DEC  B
    JP   Z, .l3
    JP   .l4
.l1:
    INC  H
    INC  H
    LD   BC, 0xffc0
    LD   A, 0x0
    JP   .l5
.l2:
    LD   BC, 0xf03f
    LD   A, 0x6
    JP   .l5
.l3:
    LD   BC, 0xfc0f
    LD   A, 0x4
    JP   .l5
.l4:
    LD   BC, 0xff03
    LD   A, 0x2
.l5:
    LD   (M_VARS.esc_var1), A
    EX   DE, HL

.sym_draw:
    LD   A, (M_VARS.esc_var1)
    PUSH HL
    LD   L, (HL)
    LD   H, 0x0
    OR   A
    JP   Z, .pne_l8

.pne_l7:
    ADD  HL, HL
    DEC  A
    JP   NZ, .pne_l7

.pne_l8:
    EX   DE, HL
    LD   A, (HL)
    AND  C
    LD   (HL), A
    LD   A, (M_VARS.curr_color)
    AND  E
    OR   (HL)
    LD   (HL), A
    INC  H
    LD   A, (HL)
    AND  C
    LD   (HL), A
    LD   A, (M_VARS.curr_color+1)
    AND  E
    OR   (HL)
    LD   (HL), A
    INC  H
    LD   A, (HL)
    AND  B
    LD   (HL), A
    LD   A, (M_VARS.curr_color)
    AND  D
    OR   (HL)
    LD   (HL), A
    INC  H
    LD   A, (HL)
    AND  B
    LD   (HL), A
    LD   A, (M_VARS.curr_color+1)
    AND  D
    OR   (HL)
    LD   (HL), A
    INC  L
    DEC  H
    DEC  H
    DEC  H
    EX   DE, HL
    POP  HL
    INC  HL
    LD   A, (M_VARS.esc_var0)
    DEC  A
    LD   (M_VARS.esc_var0), A
    JP   NZ, .sym_draw

    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A

    ; draw cursor on return
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
    ADD  A, 10
    CP   248
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
    SUB  10                                         ; 10 rows per symbol
    JP   NC, .up_no_minus
    LD   A, 240                                     ; wrap to bottom
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
    CP   4
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
    ADD  A, 66
    LD   H, A
    LD   A, 0x7
    LD   (M_VARS.esc_var0),A
    RET

m2_lf:
    LD   A, (HL)
    ADD  A, 10
    CP   15
    JP   NC, .lf_nowr
    LD   (HL), A
    RET

.lf_nowr:
    LD   A, (M_VARS.row_shift)
    LD   L, A
    ADD  A, ASCII_LF
    LD   E, A
    LD   C, ASCII_BS
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
    LD   C, 10
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
    CP   3                                          ;
    JP   Z, mp_mode_80                              ; jump for screen_mode=3
    ; calc symbol address in VRAM
    LD   HL, (M_VARS.cursor_row)
    LD   A, (M_VARS.row_shift)
    ADD  A, L
    LD   L, A
    LD   A, H
    ADD  A, 0x40
    LD   H, A
    ;
    LD   C, 7                                       ; symbol height

    ; Access VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

    EX   DE, HL
    XOR  A
    LD   (DE), A
    INC  E

.next_row:
    LD   A, (HL)
    ADD  A, A
    LD   (DE), A
    INC  HL
    INC  E
    DEC  C
    JP   NZ, .next_row
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    ; draw cursor at end
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
    ADD  A, 1
    AND  0x3f                                       ; wrap
    LD   (HL), A
    DEC  HL
    RET  NZ                                         ; ret if no wrap

; --------------------------------------------------
; Handle ASCII_LF (cursor down) in 64x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m64_lf:
    LD   A, (HL)
    ADD  A, 10
    CP   248
    JP   NC, scroll_up
    LD   (HL), A
    RET

; --------------------------------------------------
; Scroll Up for 10 rows
; --------------------------------------------------
scroll_up:
    LD   A, (M_VARS.row_shift)
    ADD  A, 10
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
    SUB  10
    JP   NC, .no_wrap
    LD   A, 240

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
    ; Access to VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

    ; fix address
    EX   DE, HL
    INC  E
    ; make bitmask
    LD   A, B
    OR   A
    JP   Z, .l1
    DEC  A
    JP   Z, .l2
    DEC  A
    JP   Z, .l3
    JP   .l4

.l1:
    LD   B, (HL)
    LD   A, (DE)
    AND  0xc0
    OR   B
    LD   (DE), A
    INC  HL
    INC  E
    DEC  C
    JP   NZ, .l1
    JP   .l6
.l2:
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
    JP   NZ, .l2
    JP   .l6
.l3:
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
    JP   NZ, .l3
    JP   .l6
.l4:
    DEC  D
.l5:
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
    JP   NZ, .l5
    INC  D

.l6:
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    ; Draw cursor after symbol
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
    ADD  A, 1                                       ; inc column
    AND  0x7f
    LD   (HL), A
    CP   80
    DEC  HL
    RET  M                                          ; return if no wrap

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
    ADD  A, 10
    CP   248
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
    SUB  1                                          ; TODO: DEC A - save 1b 2t
    AND  0x7f                                       ;  mask [0..127]
    CP   127
    JP   Z, .wrap
    LD   (HL), A
    DEC  HL
    RET

.wrap:
    LD   A, 79
    LD   (HL), A
    DEC  HL
    ; and move cursor to previous line

; --------------------------------------------------
; Handle ASCII_EM (cursor up) in 80x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m80_up:
    LD   A, (HL)
    SUB  10
    JP   NC, .no_wrap
    LD   A, 240

.no_wrap:
    LD   (HL), A
    RET

; --------------------------------------------------
; Handle ASCII_TAB (cursor column + 8) in 80x25 mode
; Inp: HL - cursor pos
; --------------------------------------------------
m80_tab:
    INC  HL
    LD   A, (HL)
    ADD  A, 8
    AND  0x7f
    LD   (HL), A
    CP   80
    DEC  HL
    RET  M                                          ; return if no cursor wrap
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
    CP   4
    LD   B, A
    JP   M, mns_ep_fm_0
    AND  3
    LD   B, A
    LD   A, H
    OR   A
    RRA
    OR   A
    RRA
    LD   C, A
    LD   H, 3
    XOR  A
mns_l1:
    ADD  A, H
    DEC  C
    JP   NZ, mns_l1
    ADD  A, B
mns_ep_fm_0:
    ADD  A, 0x42
    LD   H, A
    LD   C, 0x7
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
    AND  0x4                                        ; check hidden cursor bit
    RET  NZ                                         ; return if hidden
    LD   A, (M_VARS.screen_mode)
    AND  0x3                                        ; check color mode (40 column mode 6x7 font)
    JP   NZ, .dc_mode_64

    EX   DE, HL
    LD   HL, (M_VARS.cursor_row)
    LD   A, H                                       ; cursor column
    CP   40                                         ; > 40?
    EX   DE, HL
    RET  P                                          ; ret if column out of screen

    PUSH HL
    EX   DE, HL
    CALL calc_addr_40
    ; Access to VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    ; previous address
    DEC  H
    DEC  H
    INC  L
    LD   C, 7                                       ; cursor size
    ; build masks
    LD   A, B
    OR   B
    JP   Z, .dc_rt2
    DEC  B
    JP   Z, .dc_mid
    DEC  B
    JP   Z, .dc_lt
    JP   .dc_rt1
.dc_rt2:
    INC  H
    INC  H
    LD   DE, 0x001f
    JP   .dc_put
.dc_mid:
    LD   DE, 0x07c0
    JP   .dc_put
.dc_lt:
    LD   DE, 0x01f0
    JP   .dc_put
.dc_rt1:
    LD   DE, 0x007c

.dc_put:
    ; xor cursor mask with VRAM[HL] value
    ; left bytes
    LD   A, (HL)
    XOR  E
    LD   (HL), A
    INC  H
    LD   A, (HL)
    XOR  E
    LD   (HL), A
    ; right bytes
    INC  H
    LD   A, (HL)
    XOR  D
    LD   (HL), A
    INC  H
    LD   A, (HL)
    XOR  D
    LD   (HL), A
    ; next cursor row address
    INC  L
    DEC  H
    DEC  H
    DEC  H
    DEC  C
    JP   NZ, .dc_put                                ; draw next cursor row if c>0
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    POP  HL
    RET

    ; draw cursor in 64 column mode
.dc_mode_64:
    CP   3                                          ; screen_mode = 3 - 80 rows
    JP   Z, .dc_mode_80
    EX   DE, HL
    LD   HL, (M_VARS.cursor_row)                    ; H - col, L - row
    ; take into account the vertical shift
    LD   A, (M_VARS.row_shift)
    ADD  A, L
    LD   L, A
    ;
    LD   A, H
    CP   64                                         ; check column
    EX   DE, HL
    RET  P                                          ; return if column out of screen
    EX   DE, HL
    ; calc VRAM address
    ADD  A, 0x40
    LD   H, A
    ; Access to VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

    LD   BC, 0x7f08                                 ; B=01111111b - mask, C=8 - cursor size
.cur_64_next:
    ; xor with VRAM content
    LD   A, (HL)
    XOR  B
    LD   (HL), A
    ; next row address
    INC  L
    DEC  C
    JP   NZ, .cur_64_next
    EX   DE, HL
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

    ; draw cursor in 80 column mode
.dc_mode_80
    EX   DE, HL
    LD   HL, (M_VARS.cursor_row)

    LD   A, H
    CP   80
    EX   DE, HL
    RET  P                                          ; return if column > 80

    PUSH HL
    CALL calc_addr_80
    LD   C, 7                                       ; cursor size
    INC  L

    ; Access to VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    ; mask
    LD   A, B
    OR   A
    LD   B, 0x1f
    JP   Z, .dc_1_byte
    DEC  A
    LD   DE, 0xc007
    JP   Z, .dc_2_byte
    DEC  A
    LD   DE, 0xf001
    JP   Z, .dc_2_byte
    LD   B, 0x7c
    DEC  H
    JP   .dc_1_byte                                     ; TODO: unused

.dc_1_byte:
    ; xor with VRAM byte
    LD   A, (HL)
    XOR  B
    LD   (HL), A
    INC  L
    DEC  C
    JP   NZ, .dc_1_byte
    JP   .dc_80_end

.dc_2_byte:
    ; xor with previous byte
    DEC  H
    LD   A, (HL)
    XOR  D
    LD   (HL), A
    ; xor with current byte
    INC  H
    LD   A, (HL)
    XOR  E
    LD   (HL), A
    ; next cursor address
    INC  L
    DEC  C
    JP   NZ, .dc_2_byte

.dc_80_end:
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    POP  HL
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
    JP   NZ, .handle_cc_mono
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
.handle_cc_mono:
    LD   A, (M_VARS.screen_mode)
    CP   3
    JP   Z, handle_cc_80x25
    CP   7
    JP   Z, handle_cc_80x25
    ; 64x25 screen mode
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
    CALL m_draw_cursor
    LD   DE, M_VARS.esc_param
    LD   HL, M_VARS.cursor_col
    INC  DE
    LD   A, (DE)                                    ; column
    SUB  32
    LD   B, A
    LD   A, (M_VARS.screen_mode)
    CP   3
    JP   Z, .mode_80
    CP   7
    JP   Z, .mode_80
    OR   A
    JP   Z, .mode_40
    CP   4
    JP   Z, .mode_40
    ; mode 64x25
    LD   A, B
    CP   64
    JP   M, .common
    LD   A, 64
    JP   .common
    ; mode 40x25
.mode_40:
    LD   A, B
    CP   40
    JP   M, .common
    LD   A, 40
    JP   .common
    ; mode 80x25
.mode_80:
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
    CP   24
    JP   C, esc_le_24
    LD   A, 24
esc_le_24:
    LD   B, A
    ADD  A, A
    ADD  A, A
    ADD  A, B
    ADD  A, A
    LD   (HL), A
    CALL m_draw_cursor                              ; TODO change call+ret to jp
    RET                                             ;

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
    LD   A, (M_VARS.esc_param)                      ; first parameter - video mode
    AND  0xf
    CP   11
    RET  NC                                         ; return if not valid input parameter
    CP   9
    JP   Z, .cursor_hide
    CP   10
    JP   Z, .cursor_show
    LD   (HL), A                                    ; store new mode
    CP   4
    JP   Z, .set_color_mode
    AND  0x3                                        ; monochrome (80x25, 64x25) mode?
    LD   A, 0                                       ; mode 512x254 mono
    JP   NZ, .skip_for_mono_mode
    ; mode=0 or 4 -> 256x256px color
.set_color_mode:
    LD   A, 0x40                                    ; color mode
.skip_for_mono_mode:
    OR   B                                          ; color mode with palette
    OUT  (VID_DD67PB), A                            ; configure screen mode

    LD   HL, M_VARS.cursor_row
    CALL m_clear_screen

.draw_cursor:
    CALL m_draw_cursor                              ; TODO change call+ret to jp
    RET

.cursor_hide:
    LD   A, (HL)                                    ; screen_mode
    OR   00000100b                                  ; cursor hide
    LD   (HL), A
    LD   HL, M_VARS.cursor_row
    JP   .draw_cursor

.cursor_show:
    LD   A, (HL)                                    ; screen_mode
    AND  11111011b                                  ; cursor show
    LD   (HL), A
    JP   .draw_cursor


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
    ; check video mode
    LD   A, (M_VARS.screen_mode)
    AND  0x3                                        ; color?
    JP   NZ, esc_exit                               ; exit for mono modes

    LD   A, C
    AND  0x7f
    LD   C, A                                       ; C = C with 7th bit reset
    CP   0x1
    JP   Z, .sprites_en                             ; enable sprite mode

    CP   ASCII_SP
    JP   M, mode2_exit                              ; codes 0..31 - turm off game_mode

    ; check X, Y range to prevent drawing symbols out of screen
    LD   HL, M_VARS.esc_param
    LD   A, (HL)
    LD   E, A
    ADD  A, 8
    JP   C, mode2_exit                              ; exit if esc_param[0]>247
    LD   (HL), A
    INC  HL                                         ; HL -> esc_param[1]
    LD   A, 247
    CP   (HL)
    JP   C, mode2_exit                              ; exit if esc_param[1]>247
    ; calculate X,Y pixel address in VRAN
    LD   D, (HL)
    CALL calc_px_addr
    ; HL - address, B - pixel pos in byte
    LD   A, L
    SUB  8
    LD   L, A
    PUSH HL                                         ; save address

    LD   A, (M_VARS.esc_var2)
    OR   A
    JP   NZ, .mode_sp

    ; font
    LD   A, C
    CALL m_get_glyph
    LD   C, 7
    POP  DE
    INC  E
    JP   .out_sp

    ; sprite mode
.mode_sp:
    LD   A, C
    SUB  32
    CP   35
    JP   NC, co_ex_l08

    ; Calc sprite address
    LD   L, A                                       ; HL=A - sprite_no
    XOR  A
    LD   H, A
    ADD  HL, HL
    ADD  HL, HL
    ADD  HL, HL                                     ; HL=HL*8
    LD   DE, game_sprite_tab
    ADD  HL, DE                                     ; HL -> sprite
    LD   C, 8                                       ; bytes count
    POP  DE

    ; Out sprite
    ; DE -> VRAM address
    ; C - height
.out_sp:
    LD   A, (M_VARS.esc_param+2)
    DEC  A
    JP   Z, out_no_xor

.next_line:
    PUSH HL
    ; Access Video RAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   L, (HL)                                    ; load from table
    LD   H, 0x0
    LD   A, B
    OR   A
    JP   Z, .l05
.l04:
    ADD  HL, HL
    DEC  A
    JP   NZ, .l04
.l05:
    EX   DE, HL
    LD   A, (M_VARS.curr_color)
    AND  E
    XOR  (HL)
    LD   (HL), A
    INC  H
    INC  H
    LD   A, (M_VARS.curr_color)
    AND  D
    XOR  (HL)
    LD   (HL), A
    DEC  H
    LD   A, (M_VARS.curr_color+1)
    AND  E
    XOR  (HL)
    LD   (HL), A
    INC  H
    INC  H
    LD   A, (M_VARS.curr_color+1)
    AND  D
    XOR  (HL)
    LD   (HL), A
    DEC  H
    DEC  H
    DEC  H
    INC  L
    EX   DE, HL
    ; Disable VRAM
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    POP  HL
    INC  HL
    DEC  C
    JP   NZ, .next_line
    RET

.sprites_en:
    LD   (M_VARS.esc_var2), A
    RET

mode2_exit:
    XOR  A
    LD   (M_VARS.esc_var2), A
    JP   esc_exit

co_ex_l08:
    POP  DE
    JP   mode2_exit

out_no_xor:
    PUSH HL
    ; Acess to VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

    LD   L, (HL)
    LD   H, 0x0
    LD   A, B
    OR   A
    JP   Z, .l11
.l10:
    ADD  HL, HL
    DEC  A
    JP   NZ, .l10
.l11:
    EX   DE, HL
    PUSH BC
    LD   A, (M_VARS.curr_color)
    CPL
    LD   B, A
    LD   A, (HL)
    XOR  B
    OR   E
    XOR  B
    LD   (HL), A
    INC  H
    INC  H
    LD   A, (HL)
    XOR  B
    OR   D
    XOR  B
    LD   (HL), A
    DEC  H
    LD   A, (M_VARS.curr_color+1)
    CPL
    LD   B, A
    LD   A, (HL)
    XOR  B
    OR   E
    XOR  B
    LD   (HL), A
    INC  H
    INC  H
    LD   A, (HL)
    XOR  B
    OR   D
    XOR  B
    LD   (HL), A
    DEC  H
    DEC  H
    DEC  H
    INC  L
    EX   DE, HL
    POP  BC

    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A

    POP  HL
    INC  HL
    DEC  C
    JP   NZ, out_no_xor
    RET

game_sprite_tab:
    DB   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ; 0x00
    DB   0x7E, 0x81, 0xA5, 0x81, 0xBD, 0x99, 0x81, 0x7E ; 0x01
    DB   0x7E, 0xFF, 0xDB, 0xFF, 0xC3, 0xE7, 0xFF, 0x7E ; 0x02
    DB   0x00, 0x08, 0x08, 0x14, 0x63, 0x14, 0x08, 0x08 ; 0x03
    DB   0x36, 0x7F, 0x7F, 0x7F, 0x3E, 0x1C, 0x08, 0x00 ; 0x04
    DB   0x08, 0x1C, 0x3E, 0x7F, 0x3E, 0x1C, 0x08, 0x00 ; 0x05
    DB   0x1C, 0x3E, 0x1C, 0x7F, 0x7F, 0x6B, 0x08, 0x1C ; 0x06
    DB   0x08, 0x08, 0x1C, 0x3E, 0x7F, 0x3E, 0x08, 0x1C ; 0x07
    DB   0x3C, 0x66, 0x66, 0x66, 0x3C, 0x18, 0x7E, 0x18 ; 0x08
    DB   0x18, 0xDB, 0x3C, 0xE7, 0xE7, 0x3C, 0xDB, 0x18 ; 0x09
    DB   0xE7, 0xE7, 0x00, 0x7E, 0x7E, 0x00, 0xE7, 0xE7 ; 0x0a
    DB   0x7E, 0x81, 0x81, 0xFF, 0xFF, 0x81, 0x81, 0x7E ; 0x0b
    DB   0x00, 0x18, 0x3C, 0x7E, 0x18, 0x18, 0x18, 0x18 ; 0x0c
    DB   0x18, 0x18, 0x18, 0x18, 0x7E, 0x3C, 0x18, 0x00 ; 0x0d
    DB   0x00, 0x10, 0x30, 0x7F, 0x7F, 0x30, 0x10, 0x00 ; 0x0e
    DB   0x00, 0x08, 0x0C, 0xFE, 0xFE, 0x0C, 0x08, 0x00 ; 0x0f
    DB   0x88, 0x44, 0x22, 0x11, 0x88, 0x44, 0x22, 0x11 ; 0x10
    DB   0x11, 0x22, 0x44, 0x88, 0x11, 0x22, 0x44, 0x88 ; 0x11
    DB   0x0F, 0x0F, 0x0F, 0x0F, 0xF0, 0xF0, 0xF0, 0xF0 ; 0x12
    DB   0xF0, 0xF0, 0xF0, 0xF0, 0x0F, 0x0F, 0x0F, 0x0F ; 0x13
    DB   0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF ; 0x14
    DB   0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF ; 0x15
    DB   0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F ; 0x16
    DB   0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0 ; 0x17
    DB   0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00 ; 0x18
    DB   0xCC, 0xCC, 0x33, 0x33, 0xCC, 0xCC, 0x33, 0x33 ; 0x19
    DB   0x70, 0x08, 0x76, 0xFF, 0xFF, 0xFF, 0x7E, 0x18 ; 0x1a
    DB   0xC3, 0xDB, 0xDB, 0x18, 0x18, 0xDB, 0xDB, 0xC3 ; 0x1b
    DB   0xFC, 0xCC, 0xFC, 0x0C, 0x0C, 0x0E, 0x0F, 0x07 ; 0x1c
    DB   0xFE, 0xC6, 0xFE, 0xC6, 0xC6, 0xE6, 0x67, 0x03 ; 0x1d
    DB   0x18, 0x3C, 0x3C, 0x18, 0x7E, 0x18, 0x24, 0x66 ; 0x1e
    DB   0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80 ; 0x1f
    DB   0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01 ; 0x20
    DB   0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01 ; 0x21
    DB   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF ; 0x22

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
; Draw filled rectanger
; Inp: esc param X1,Y2,X2,Y2
; --------------------------------------------------
esc_draw_fill_rect:
    LD   HL, M_VARS.esc_param
    LD   E, (HL)                                    ; E=X1
    INC  HL
    LD   C, (HL)                                    ; C=Y1
    INC  HL
    INC  HL
    LD   D, (HL)                                    ; D=Y2
    LD   A, D
    SUB  C                                          ; delta Y
    JP   NZ, .non_zero_h
    INC  A                                          ; 1 as minimum
.non_zero_h:
    LD   C, A                                       ; C = height
    ; DE = Y2, X1
    CALL calc_px_addr
    ; HL -> videomem offset, b - pixel offset

    ; build pixel mask
    XOR  A
.shift_mask_l:
    SCF
    RLA
    DEC  B
    JP   P, .shift_mask_l
    RRA
    LD   (M_VARS.pixel_mask_l), A
    CPL                                             ; invert
    LD   (M_VARS.pixel_mask_l_i), A
    LD   A, (M_VARS.esc_param+2)                    ; X2
    AND  0x7                                        ; 0..7
    LD   B, A
    XOR  A
.shift_mask_r:
    SCF
    RLA
    DEC  B
    JP   P, .shift_mask_r
    LD   (M_VARS.pixel_mask_r_i), A
    LD   B, C
    ; calc end address
    LD   A, (M_VARS.esc_param+2)                    ; X2
    RRA
    RRA
    AND  00111110b
    ADD  A, 0x40
    SUB  H
    RRCA
    LD   C, A                                       ; C - width
    INC  B
    LD   A, (M_VARS.esc_param+4)
    DEC  A
    JP   NZ, .rectangle_xor
    LD   A, (M_VARS.pixel_mask_r_i)
    CPL
    LD   (M_VARS.pixel_mask_r), A
    LD   D, A
    LD   A, (M_VARS.pixel_mask_l)
    LD   E, A
    ; draw B horisontal lines
.next_line:
    PUSH DE
    PUSH HL
    PUSH BC
    CALL draw_line_h
    POP  BC
    POP  HL
    POP  DE
    INC  L
    DEC  B
    JP   NZ, .next_line
    RET

    ; draw B horisontal lines (xor)
.rectangle_xor:
    LD   A, (M_VARS.pixel_mask_r_i)
    LD   (M_VARS.pixel_mask_r), A
    LD   D, A
    LD   A, (M_VARS.pixel_mask_l_i)
    LD   E, A
    ; Access to VideoRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A

.edf_l6:
    PUSH DE
    PUSH HL
    PUSH BC
    LD   A, C
    OR   A
    JP   NZ, .w_ne_0                                ; jump if width != 0
    LD   A, E                                       ; merge masks E=E or D
    OR   D
.next_8px:
    LD   E, A
.w_ne_0:
    LD   B, E                                       ; B - mask
    EX   DE, HL
    LD   HL, (M_VARS.curr_color)                    ; color
    EX   DE, HL
    ; Set pixels - VideoRAM[HL] = color xor VideoRAM[HL]
    LD   A, E
    AND  B
    XOR  (HL)
    LD   (HL), A
    ; And next byte
    INC  H
    LD   A, D
    AND  B
    XOR  (HL)
    LD   (HL), A
    ; ----------
    INC  H
    LD   A, C
    OR   A
    JP   Z, .complete
    DEC  C
    ; right tail of line, use right mask
.r_mask:
    LD   A, (M_VARS.pixel_mask_r)
    JP   Z, .next_8px
    ; full 8 bits without mask at middle of line
.next_full:
    LD   A, (HL)
    XOR  E
    LD   (HL), A
    INC  H
    LD   A, (HL)
    XOR  D
    LD   (HL), A
    INC  H
    DEC  C
    JP   NZ, .next_full
    JP   .r_mask
.complete:
    POP  BC
    POP  HL
    POP  DE
    INC  L
    DEC  B
    JP   NZ, .edf_l6
    ; Disable VideoRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

;---------------------------------------------------
; Paint screen
; Inp: params X,Y,Color,repColor
;---------------------------------------------------
esc_paint:
    ; Save stack
    LD   HL, 0x0
    ADD  HL, SP
    LD   (M_VARS.paint_sp_save), HL

    ; Set our own stack
    LD   HL, M_VARS.paint_stack                     ; TODO: Z80 LD SP,var i800 - LXI SP,nn
    LD   SP, HL

    ; save current color
    LD   HL, (M_VARS.curr_color)
    LD   (M_VARS.tmp_color), HL

    ; set color from param 3
    LD   A, (M_VARS.esc_param+2)
    DEC  A
    CALL m_set_color

    ; color to replace, from param 4
    LD   A, (M_VARS.esc_param+3)
    DEC  A
    LD   (M_VARS.cmp_color), A

    ; HL - Y,X
    LD   A, (M_VARS.esc_param)
    LD   L, A
    LD   A, (M_VARS.esc_param+1)
    LD   H, A
    LD   (M_VARS.paint_y), A

    LD   A, (M_VARS.esc_param+4)                    ; 0 - full fill, 1 - fast fill
    DEC  A
    LD   (M_VARS.esc_param), A

    LD   A, 0x2
    LD   (M_VARS.paint_var5), A                     ; task_no=2

    EX   DE, HL
    CALL calc_px_addr
    LD   (M_VARS.esc_param+1), HL                   ; temporary ctore address of start fill point
    ; make mask
    LD   A, 10000000b
.l1:
    RLCA
    DEC  B
    JP   P, .l1

    LD   B, A
    LD   (M_VARS.esc_param+3), A                    ; store mask

    ; find left border
    LD   A, (M_VARS.cmp_color)
    LD   C, A
    LD   D, E                                       ; D = X
    CALL paint_find_left
    ; find right border
    LD   HL, (M_VARS.esc_param+1)                   ; restore HL
    LD   A, (M_VARS.esc_param+3)                    ; restore mask
    LD   B, A
    CALL paint_find_right
    ;
    LD   HL, 0x0
    PUSH HL
    PUSH HL
    ;
    LD   A, (M_VARS.esc_param)                      ; A = fill mode
    OR   A
    JP   Z, ep_fm_0
    ; push fill task parameters
    LD   A, (M_VARS.paint_var5)
    DEC  A                                          ; task_no-1
    LD   H, A
    LD   L, E
    PUSH HL
    LD   A, (M_VARS.paint_y)
    LD   H, A
    LD   L, D
    PUSH HL

ep_fm_0:
    ; push fill task parameters
    LD   A, (M_VARS.paint_var5)                     ; task_no
    LD   H, A
    LD   L, E
    PUSH HL
    LD   A, (M_VARS.paint_y)
    LD   H, A
    LD   L, D
    PUSH HL
    JP   paint_task                                 ; exec task

ep_task_end:
    LD   A, (M_VARS.cmp_color)
    LD   C, A                                       ; color to compare

    LD   A, (M_VARS.esc_param)                      ; fill mode 0 - full, 1 - fast
    OR   A
    JP   NZ, ep_f_fast

    LD   A, 0x2
    LD   (M_VARS.paint_var7), A
    LD   A, (M_VARS.paint_var2)
    CP   2
    JP   Z, ep_l4
    JP   ep_l5                                      ; TODO: change to one JP NZ

ep_l4:
    LD   A, 1
    LD   (M_VARS.paint_var5), A                     ; task_no?
    JP   ep_l8

ep_l5:
    LD   A, 2
    LD   (M_VARS.paint_var5), A
    JP   ep_l11

ep_l6:
    LD   A, (M_VARS.paint_var7)
    OR   A
    JP   Z, paint_task
    LD   A, (M_VARS.paint_var2)
    CP   2
    JP   Z, ep_l5                                   ; TODO: change to one JP NZ
    JP   ep_l4

ep_f_fast:
    LD   A, (M_VARS.paint_var2)
    LD   (M_VARS.paint_var5), A
    CP   1                                          ; TODO: DEC A - save 1b 3t
    JP   Z, ep_l8                                   ; TODO: change to one JP NZ
    JP   ep_l11

ep_l8:
    LD   A, (M_VARS.paint_var3)
    LD   D, A
    LD   A, (M_VARS.paint_var1)
    LD   E, A
    LD   HL, (M_VARS.esc_param+1)
    LD   A, (M_VARS.esc_param+3)
    LD   B, A
    LD   A, (M_VARS.paint_var4)
    DEC  A
    JP   Z, ep_l10
    LD   (M_VARS.paint_y), A
    INC  L
    CALL paint_find_next_right
    JP   Z, ep_l10
    LD   HL, (M_VARS.esc_param+4)
    LD   A, (M_VARS.esc_param+6)
    LD   B, A
    INC  L
    CALL paint_find_next_left
    JP   Z, ep_l10
    LD   A, (M_VARS.esc_param)
    OR   A
    JP   NZ, ep_l9
    JP   ep_l12

ep_l9:
    LD   A, (M_VARS.paint_var5)
    LD   H, A
    LD   L, E
    PUSH HL
    LD   A, (M_VARS.paint_y)
    LD   H, A
    LD   L, D
    PUSH HL
    JP   paint_task

ep_l10:
    LD   A, (M_VARS.esc_param)
    OR   A
    JP   NZ, paint_task
    LD   A, (M_VARS.paint_var7)
    DEC  A
    LD   (M_VARS.paint_var7), A
    JP   ep_l6

ep_l11:
    LD   A, (M_VARS.paint_var3)
    LD   D, A
    LD   A, (M_VARS.paint_var1)
    LD   E, A
    LD   HL, (M_VARS.esc_param+1)
    LD   A, (M_VARS.esc_param+3)
    LD   B, A
    LD   A, (M_VARS.paint_var4)
    INC  A
    CP   0xff
    JP   Z, ep_l10
    LD   (M_VARS.paint_y), A
    DEC  L
    CALL paint_find_next_right
    JP   Z, ep_l10
    LD   HL, (M_VARS.esc_param+4)
    LD   A, (M_VARS.esc_param+6)
    LD   B, A
    DEC  L
    CALL paint_find_next_left
    JP   Z, ep_l10
    LD   A, (M_VARS.esc_param)
    OR   A
    JP   NZ, ep_l9
    JP   ep_l12

; ---------------------------------------------------
;
; ---------------------------------------------------
paint_find_next_right:
    CALL get_pixel
    JP   NZ, .l1
    CALL paint_find_left
    LD   A, 0xff
    OR   A
    RET

.l1:
    LD   A, D
    CP   E
    RET  Z
    INC  D
    LD   A, B
    RLCA
    LD   B, A
    JP   NC, .l2
    INC  H
    INC  H
.l2:
    CALL get_pixel
    JP   NZ, .l1
    LD   A, 0xff
    OR   A
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
paint_find_next_left:
    CALL get_pixel
    JP   NZ, .l1
    CALL paint_find_right
    LD   A, 0xff
    OR   A
    RET
.l1:
    LD   A, E
    CP   D
    RET  Z
    DEC  E
    LD   A, B
    RRCA
    LD   B, A
    JP   NC, .l2
    DEC  H
    DEC  H
.l2:
    CALL get_pixel
    JP   NZ, .l1
    LD   A, 0xff
    OR   A
    RET

ep_l12:
    LD   A, D
    LD   (M_VARS.pixel_mask_r), A
    LD   A, (M_VARS.paint_var5)
    LD   D, A
    PUSH DE
    LD   A, (M_VARS.pixel_mask_r)
    CP   E
    JP   NZ, ep_l13
    LD   L, A
    LD   A, (M_VARS.paint_y)
    LD   H, A
    PUSH HL
    JP   ep_l16
ep_l13:
    LD   D, E
    CALL paint_find_left
    LD   E, D
    LD   A, (M_VARS.paint_y)
    LD   D, A
    PUSH DE
    LD   A, (M_VARS.pixel_mask_r)
    LD   D, A
    CP   E
    JP   Z, ep_l16
ep_l14:
    DEC  E
    LD   A, B
    RRCA
    LD   B, A
    JP   NC, ep_l15
    DEC  H
    DEC  H
ep_l15:
    CALL get_pixel
    JP   NZ, ep_l14
    JP   ep_l12
ep_l16:
    JP   ep_l10

; ---------------------------------------------------
; Find rightmost pixel to fill
; In/Out: E = x_right
;         HL - current pixel address
;         B - pixel mask
; ---------------------------------------------------
paint_find_right:
    LD   A, E
    CP   0xff
    RET  Z                                          ; return if X=right border
    INC  E                                          ; x=x+1
    ; rotate pixel mask right
    LD   A, B
    RLCA
    LD   B, A
    JP   NC, .in_byte
    ; inc addr+2 (2 byte per 8 pixels)
    INC  H
    INC  H

.in_byte:
    CALL get_pixel
    JP   Z, paint_find_right                        ; find until same color
    ; border found, x-1
    DEC  E
    ; rotate mask back 1 px
    LD   A, B
    RRCA
    LD   B, A
    RET  NC
    ; addr-2 if previous byte
    DEC  H
    DEC  H
    RET

; ---------------------------------------------------
; Find leftmost pixel to fill
; In/Out: D = x_left
;         HL - current pixel address
;         B - pixel mask
; ---------------------------------------------------
paint_find_left:
    LD   A, D
    OR   A
    RET  Z                                          ; return if x=0

    DEC  D                                          ; x-1
    LD   A, B
    RRCA                                            ; rotate mask to right
    LD   B, A
    JP   NC, .in_byte
    DEC  H                                          ; addr-2 (2 byte for 8px)
    DEC  H

.in_byte:
    CALL get_pixel
    JP   Z, paint_find_left                         ; repeat until same color

    INC  D                                          ; border found, x+1
    ; mask rotate right
    LD   A, B
    RLCA
    LD   B, A
    RET  NC
    ; if CF, inc address+2
    INC  H
    INC  H
    RET

; ---------------------------------------------------
; Inp: HL - address
;      B - pixel mask
;      C - color to compare
; Out: A - 0,1,2
;      ZF - set if color match
; ---------------------------------------------------
get_pixel:
    ; Access to VRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    ; get pixel and mask
    LD   A, (HL)
    AND  B
    JP   NZ, .bit1_set
    INC  H
    LD   A, (HL)
    DEC  H
    AND  B
    JP   NZ, .bit2_set
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    CP   C
    RET

.bit1_set:
    INC  H
    LD   A, (HL)
    DEC  H
    AND  B
    JP   NZ, .bit12_set
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    LD   A, 0x1
    CP   C
    RET

.bit2_set:
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    LD   A, 0x2
    CP   C
    RET

.bit12_set:
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    LD   A, 3
    CP   C
    RET

paint_task:
    POP  HL                                         ; L=x0, H=Y
    LD   (M_VARS.paint_var3), HL
    EX   DE, HL

    POP  HL                                         ; L=x1, H=mode
    LD   A, H
    OR   A
    JP   Z, paint_exit                              ; jump for mode=0

    ; calc leftmost pixel address, mask for draw horisontal line
    LD   (M_VARS.paint_var1), HL
    CALL calc_px_addr
    LD   (M_VARS.esc_param+1), HL
    LD   C, B
    LD   A, 0x80
.lmp_mask:
    RLCA
    DEC  B
    JP   P, .lmp_mask
    LD   (M_VARS.esc_param+3), A
    ; calc rightmos pixel address and mask
    LD   A, (M_VARS.paint_var1)
    LD   E, A
    LD   A, (M_VARS.paint_var4)
    LD   D, A
    CALL calc_px_addr
    LD   (M_VARS.esc_param+4), HL
    LD   D, B
    LD   A, 0x80
.rmp_mask:
    RLCA
    DEC  B
    JP   P, .rmp_mask
    LD   (M_VARS.esc_param+6), A
    LD   A, (M_VARS.esc_param+3)                    ; TODO: unused code

    XOR  A
.lmi_mask:
    SCF
    RLA
    DEC  C
    JP   P, .lmi_mask
    RRA
    LD   E, A                                       ; E - left inv mask

    XOR  A
.rmi_mask:
    SCF
    RLA
    DEC  D
    JP   P, .rmi_mask
    CPL
    LD   D, A                                       ; D - right inv mask

    LD   (M_VARS.pixel_mask_r), A
    LD   HL, (M_VARS.esc_param+1)                   ; HL -> lext pix address
    LD   A, (M_VARS.esc_param+5)                    ; right pix address (low byte)
    SUB  H                                          ; delta x
    RRCA                                            ; 2 byte for 8 pix
    LD   C, A                                       ; C - line width
    CALL draw_line_h
    JP   ep_task_end

paint_exit:
    LD   HL, (M_VARS.tmp_color)                     ; restore previous current color
    LD   (M_VARS.curr_color), HL
    LD   HL, (M_VARS.paint_sp_save)                 ; restore previous stack
    LD   SP, HL
    RET

;---------------------------------------------------
; Draw horizontal line
; Inp: C - width
;      DE - left & right pixel mask
;      HL - address of first byte of line
;---------------------------------------------------
draw_line_h:
    ; Access to VideoRAM
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   A, C
    OR   A
    JP   NZ, .width_ne0
    LD   A, E                                       ; join left and right masks
    OR   D
.next_byte:
    LD   E, A
.width_ne0:
    LD   B, E
    EX   DE, HL
    LD   HL, (M_VARS.curr_color)
    EX   DE, HL
    ; Get pixels, apply colors
    LD   A, (HL)
    XOR  E
    AND  B
    XOR  E
    LD   (HL), A                                    ; store first
    ; Same for second byte
    INC  H
    LD   A, (HL)
    XOR  D
    AND  B
    XOR  D
    LD   (HL), A
    ; move to next byte
    INC  H
    LD   A, C
    OR   A
    JP   Z, .complete
    DEC  C
.r_mask:
    ; use right mask for last right byte
    LD   A, (M_VARS.pixel_mask_r)
    JP   Z, .next_byte
.full_8:
    LD   (HL), E
    INC  H
    LD   (HL), D
    INC  H
    DEC  C
    JP   NZ, .full_8
    JP   .r_mask
.complete:                                          ; TODO: duplicate close_vram_ret
    ; Disable VideoRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
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

close_vram_ret
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

; --------------------------------------------------
; draw horizontal line
; Inp: DE - YX
;       L - length
; --------------------------------------------------
height0
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
; ESC   Draw Dot
; --------------------------------------------------
esc_draw_dot:
    LD   HL, (M_VARS.esc_param)
    EX   DE, HL
    CALL calc_px_addr
    LD   A, 0x80
edd_l1
    RLCA
    DEC  B
    JP   P, edd_l1
    LD   B, A
    LD   A, (M_VARS.esc_param+2)
    CP   0x3
    JP   Z, edd_ep_task_end
    LD   A, B
    CPL
    LD   B, A
    LD   A, (M_VARS.esc_param+2)
    CP   0x2
    JP   Z, edd_ep_fm_0
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   A, (HL)
    AND  B
    LD   (HL), A
    INC  H
    LD   A, (HL)
    AND  B
    LD   (HL), A
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET
edd_ep_fm_0
    EX   DE, HL
    LD   HL, (M_VARS.curr_color)
    EX   DE, HL
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
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
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET
edd_ep_task_end
    CALL get_pixel
    LD   (M_VARS.esc_var3), A
    RET

; --------------------------------------------------
;
; --------------------------------------------------
esc_picture:
    LD   HL, (M_VARS.esc_param+3)
    LD   A, (HL)
    CP   ':'
    RET  NZ
    INC  HL
    LD   E, (HL)
    INC  HL
    LD   D, (HL)
    INC  HL
    LD   A, (HL)
    LD   (M_VARS.esc_var0), A
    INC  HL
    LD   A, (HL)
    LD   (M_VARS.esc_var1), A
    INC  HL
    PUSH HL
    LD   C, (HL)
    INC  HL
    LD   B, (HL)
    INC  HL
    INC  HL
    EX   DE, HL
    PUSH DE

    ; Enable VRAM access
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    CALL pict_sub1
    ; Disable VRAM access
    LD   A, 0x0
    OUT  (SYS_DD17PB), A

    POP  DE
    POP  HL
    LD   (HL), C
    INC  HL
    LD   (HL), B
    CALL pict_sub2                                  ; TODO: replace call+ret to jp;
    RET

pict_sub1:
    LD   A, (M_VARS.esc_param)
    CP   ASCII_EM
    JP   Z, gih_up
    CP   ASCII_CAN
    JP   Z, gih_rt
    CP   ASCII_SUB
    JP   Z, gih_ctrl_z
    CP   ASCII_BS
    JP   Z, gih_bs
    CP   ASCII_US
    JP   Z, pict_clr
    RET

pict_clr:
    LD   L, C
    LD   H, B
    LD   A, (M_VARS.esc_var0)
    LD   C, A
    LD   A, (M_VARS.esc_var1)
    LD   B, A
    CALL put_image
    POP  HL
    POP  HL
    POP  HL
    RET

ehd_l1:
    CP   0x2
    JP   Z, get_image_hdr
    CP   0x3
    JP   Z, m_fn_39
    RET

; --------------------------------------------------
; Function 39
; --------------------------------------------------
m_fn_39:
    LD   HL, (M_VARS.esc_param+2)                   ; pr 3,4
    INC  L
    LD   C, L
    LD   B, H
    LD   E, L
    CALL dc_mul_e_h
    ADD  HL, HL
    EX   DE, HL
    LD   HL, (M_VARS.esc_param)                     ; par 1,2
    PUSH BC
    PUSH DE
    LD   BC, 10
    LD   E, L
    LD   D, H
    ADD  HL, BC
    EX   DE, HL
    LD   (HL), E
    INC  HL
    LD   (HL), D
    INC  HL
    POP  BC
    LD   A, 4
.l1:
    PUSH AF                                 ; TODO: remove AF not changed
    EX   DE, HL
    ADD  HL, BC
    EX   DE, HL
    LD   (HL), E
    INC  HL
    LD   (HL), D
    INC  HL
    POP  AF                                 ; TODO: remove AF not changed
    DEC  A
    JP   NZ, .l1
    EX   DE, HL
    LD   HL, 0x0
    ADD  HL, BC                             ; HL=BC
    ADD  HL, BC                             ; HL=2*BC
    ADD  HL, HL                             ; HL=4*BC
    ADD  HL, BC                             ; HL=5*BC
    ADD  HL, HL                             ; HL=10*BC
    EX   DE, HL                             ; DE=10*BC
    LD   A, 0x0
    LD   B, A                               ; B=A=0
    ; fill DE bytes at [HL] with 0
.l2:
    LD   (HL), B
    INC  HL
    DEC  DE
    CP   E
    JP   NZ, .l2
    CP   D
    JP   NZ, .l2

    XOR  A
    LD   (M_VARS.esc_var0), A
    POP  BC
    LD   HL, (M_VARS.esc_param)
    LD   DE, 10
    ADD  HL, DE
.l3:
    EX   DE, HL
    LD   HL, (M_VARS.esc_param+4)
    EX   DE, HL
    PUSH BC
    CALL fn39_sub2
    POP  BC
    LD   A, (M_VARS.esc_var0)
    ADD  A, 0x2
    LD   (M_VARS.esc_var0), A
    CP   10
    RET  Z
    JP   .l3

; ---------------------------------------------------
; Function 3C
; ---------------------------------------------------
get_image_hdr:
    LD   HL, (M_VARS.esc_param+4)
    LD   (HL), 58                                    ; ':'
    INC  HL
    PUSH HL
    LD   HL, (M_VARS.esc_param+2)
    INC  L
    LD   C, L
    LD   A, H
    ADD  A, 0x4
    LD   B, A
    LD   H, B
    LD   E, C
    CALL dc_mul_e_h
    ADD  HL, HL
    EX   DE, HL
    POP  HL
    LD   (HL), E
    INC  HL
    LD   (HL), D
    INC  HL
    LD   (HL), C
    INC  HL
    LD   (HL), B
    INC  HL
    EX   DE, HL
    LD   HL, (M_VARS.esc_param)
    ADD  HL, HL
    EX   DE, HL
    PUSH BC
    PUSH HL
    CALL calc_px_addr
    POP  DE
    LD   A, L
    LD   (DE), A
    INC  DE
    LD   A, H
    LD   (DE), A
    INC  DE
    LD   A, B
    LD   (DE), A
    INC  DE
    POP  BC
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    CALL get_image                                  ; TODO: replace call+ret to jp
    RET

; ---------------------------------------------------
; <ESC>=
; ---------------------------------------------------
esc_get_put_image:
    LD   A, (M_VARS.esc_param+6)
    CP   0x2
    JP   NC, ehd_l1
    LD   HL, M_VARS.esc_param
    LD   E, (HL)
    INC  HL
    LD   D, (HL)
    INC  HL
    LD   C, (HL)
    INC  HL                                         ; TODO: next call to calc_px_addr
    LD   B, (HL)                                    ; destroy value of B and HL
    CALL calc_px_addr
    EX   DE, HL
    LD   HL, (M_VARS.esc_param+4)
    LD   A, H
    CP   128
    RET  C

    CP   184
    RET  NC

    EX   DE, HL

    ; Enable VRAM access
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    LD   A, (M_VARS.esc_param+6)
    OR   A
    JP   NZ, put_image

; ---------------------------------------------------
; Get image from VRAM to user buffer
; Inp: HL -> VRAM[x,y]
;      DE -> buffer
;      BC - width, height
; ---------------------------------------------------
get_image:
    PUSH HL
    PUSH BC
.next_row:
    ; byte 1
    LD   A, (HL)
    LD   (DE), A
    INC  H                                          ; next Y (row)
    INC  DE
    ; byte 2                                        ; next dst addr
    LD   A, (HL)
    LD   (DE), A
    INC  H
    LD   A, H
    CP   128                                        ; last row?
    JP   NZ, .l2
    LD   H, 0x40                                    ; reset Y
.l2:
    INC  DE
    DEC  C
    JP   NZ, .next_row
    POP  BC
    POP  HL
    INC  L
    DEC  B                                          ; dec width
    JP   NZ, get_image
    JP   img_task_end

; ---------------------------------------------------
; Put image from buffer to VRAM
; Inp: HL -> VRAM[x,y]
;      DE -> buffer
;      BC - width, height
; ---------------------------------------------------
put_image:
    PUSH HL
    PUSH BC
.next_row:
    ; two bytes for 8 pixels
    ; byte 1
    LD   A, (DE)                                    ; get from buffer
    LD   (HL), A                                    ; put to screen
    INC  H                                          ; next Y (row)
    INC  DE                                         ; next src addr
    ; byte 2
    LD   A, (DE)
    LD   (HL), A
    INC  DE
    INC  H
    LD   A, H
    CP   128                                        ; last row?
    JP   NZ, .l2
    LD   H, 0x40                                    ; reset
.l2:
    DEC  C
    JP   NZ, .next_row
    POP  BC
    POP  HL
    ; next column
    INC  L
    DEC  B
    JP   NZ, put_image

img_task_end:
    LD   A, 0x0
    OUT  (SYS_DD17PB), A
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
fn39_sub2:
    DEC   C
.l1:
    PUSH  BC
.l2:
    EX   DE, HL
    PUSH BC
    PUSH HL
    LD   L, (HL)
    LD   H, 0x0
    LD   A, (M_VARS.esc_var0)
    LD   B, A
    OR   A
    JP   Z, .l4
.l3:
    ADD  HL, HL
    DEC  A
    JP   NZ, .l3
.l4:
    LD   A, B
    LD   C, L
    LD   B, H
    POP  HL
    INC  HL
    PUSH HL
    LD   L, (HL)
    LD   H, 0x0
    OR   A
    JP   Z, .l6
.l5:
    ADD  HL, HL
    DEC  A
    JP   NZ, .l5
.l6:
    EX   DE, HL
    LD   A, (HL)
    OR   C
    LD   (HL), A
    INC  HL
    LD   A, (HL)
    OR   E
    LD   (HL), A
    INC  HL
    LD   (HL), B
    INC  HL
    LD   (HL), D
    DEC  HL
    POP  DE
    INC  DE
    POP  BC
    DEC  C
    JP   NZ, .l2
    POP  BC
    INC  HL
    INC  HL
    DEC  B
    JP   NZ, .l1
    RET

; --------------------------------------------------
; Handle ASCII_CAN symbol (cursor right)
; --------------------------------------------------
gih_rt:
    DEC  DE
    LD   A, (DE)
    ADD  A, 0x2
    LD   (DE), A
    CP   9
    RET  C
    LD   A, 0x2
    LD   (DE), A
    INC  DE
    PUSH BC
    LD   H, D
    LD   L, E
    INC  HL
    INC  HL
    LD   A, (M_VARS.esc_var0)
    PUSH AF
    DEC  A
    LD   (M_VARS.esc_var0), A
    INC  A
    ADD  A, A
    ADD  A, B
    CP   128
    JP   C, .l1
    SUB  0x40
.l1:
    LD   B, A
    LD   A, (M_VARS.esc_var1)
.l2:
    PUSH AF
    LD   A, (M_VARS.esc_var0)
.l3:
    PUSH AF
    LD   A, (HL)
    LD   (DE), A
    INC  HL
    INC  DE
    LD   A, (HL)
    LD   (DE), A
    INC  HL
    INC  DE
    POP  AF
    DEC  A
    JP   NZ, .l3
    LD   A, (BC)
    LD   (DE), A
    INC  DE
    INC  HL
    INC  B
    LD   A, (BC)
    LD   (DE), A
    INC  HL
    INC  DE
    DEC  B
    INC  C
    POP  AF
    DEC  A
    JP   NZ, .l2
    POP  AF
    LD   (M_VARS.esc_var0), A
    POP  BC
    INC  B
    INC  B
    LD   A, B
    CP   0x80
    RET  NZ
    LD   B, 0x40
    RET

; --------------------------------------------------
; Handle ASCII_BS (BackSpace) symbol
; --------------------------------------------------
gih_bs:
    DEC  DE
    LD   A, (DE)
    SUB  0x2
    LD   (DE), A
    RET  NC

    LD   A, 6
    LD   (DE), A
    INC  DE
    PUSH BC
    ADD  HL, DE
    DEC  HL
    LD   D, H
    LD   E, L
    DEC  HL
    DEC  HL
    LD   A, (M_VARS.esc_var1)
    ADD  A, C
    DEC  A
    LD   C, A
    LD   A, B
    DEC  A
    CP   0x3f
    JP   NZ, .l1
    LD   A, 0x7f                                    ; [DEL]?
.l1:
    LD   B, A
    LD   A, (M_VARS.esc_var0)
    PUSH AF
    DEC  A
    LD   (M_VARS.esc_var0), A
    LD   A, (M_VARS.esc_var1)
.l2:
    PUSH AF
    LD   A, (M_VARS.esc_var0)
.l3:
    PUSH AF
    LD   A, (HL)
    LD   (DE), A
    DEC  HL
    DEC  DE
    LD   A, (HL)
    LD   (DE), A
    DEC  HL
    DEC  DE
    POP  AF
    DEC  A
    JP   NZ, .l3
    LD   A, (BC)
    LD   (DE), A
    DEC  HL
    DEC  DE
    DEC  B
    LD   A, (BC)
    LD   (DE), A
    DEC  HL
    DEC  DE
    INC  B
    DEC  C
    POP  AF
    DEC  A
    JP   NZ, .l2
    POP  AF
    LD   (M_VARS.esc_var0), A
    POP  BC
    DEC  B
    DEC  B
    LD   A, B
    CP   0x3e
    RET  NZ
    LD   B, 0x7e
    RET

; --------------------------------------------------
; Handle ASCII_SUB symbol
; --------------------------------------------------
gih_ctrl_z:
    PUSH BC
    LD   A, (M_VARS.esc_var0)
    ADD  A, A
    ADD  A, A
    ADD  A, E
    LD   L, A
    LD   H, D
    LD   A, (M_VARS.esc_var1)
    ADD  A, C
    LD   C, A
    PUSH BC
    LD   A, (M_VARS.esc_var1)
    DEC  A
    DEC  A
    LD   C, A
.l1:
    LD   A, (M_VARS.esc_var0)
    LD   B, A
.l2:
    LD   A, (HL)
    LD   (DE), A
    INC  HL
    INC  DE
    LD   A, (HL)
    LD   (DE), A
    INC  HL
    INC  DE
    DEC  B
    JP   NZ, .l2
    DEC  C
    JP   NZ, .l1
    POP  BC
    LD   L, 0x2
.l3:
    LD   A, (M_VARS.esc_var0)
    LD   H, A
    PUSH BC
.l4:
    LD   A, (BC)
    LD   (DE), A
    INC  DE
    INC  B
    LD   A, (BC)
    LD   (DE), A
    INC  DE
    INC  B
    LD   A, B
    CP   0x80
    JP   NZ, .l5
    LD   B, 0x40
.l5:
    DEC  H
    JP   NZ, .l4
    POP  BC
    INC  C
    DEC  L
    JP   NZ, .l3
    POP  BC
    INC  C
    INC  C
    RET

; --------------------------------------------------
; Handle ASCII_EM symbol
; --------------------------------------------------
gih_up:
    PUSH BC
    LD   A, (M_VARS.esc_var0)
    ADD  A, A
    ADD  A, B
    CP   128
    JP   Z, .l1
    JP   C, .l1
    SUB  64
.l1:
    DEC  A
    LD   B, A
    DEC  C
    PUSH BC
    ADD  HL, DE
    LD   A, (M_VARS.esc_var0)
    ADD  A, A
    ADD  A, A
    LD   E, A
    LD   A, L
    SUB  E
    LD   E, A
    LD   D, H
    DEC  DE
    DEC  HL
    EX   DE, HL
    LD   A, (M_VARS.esc_var1)
    DEC  A
    DEC  A
    LD   C, A
.l2:
    LD   A, (M_VARS.esc_var0)
    LD   B, A
.l3:
    LD   A, (HL)
    LD   (DE), A
    DEC  HL
    DEC  DE
    LD   A, (HL)
    LD   (DE), A
    DEC  HL
    DEC  DE
    DEC  B
    JP   NZ, .l3
    DEC  C
    JP   NZ, .l2
    POP  BC
    LD   L, 0x2

.l4:
    LD   A, (M_VARS.esc_var0)
    LD   H, A
    PUSH BC

.l5:
    LD   A, (BC)
    LD   (DE), A
    DEC  DE
    DEC  B
    LD   A, (BC)
    LD   (DE), A
    DEC  DE
    DEC  B
    LD   A, B
    CP   0x3f
    JP   NZ, .l6
    LD   B, 0x7f

.l6:
    DEC  H
    JP   NZ, .l5
    POP  BC
    DEC  C
    DEC  L
    JP   NZ, .l4
    POP  BC
    DEC  C
    DEC  C
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
pict_sub2:
    PUSH DE
    DEC  DE
    LD   A, (DE)
    LD   E, A
    LD   D, 0x0
    LD   HL, (M_VARS.esc_param+1)
    ADD  HL, DE
    LD   E, (HL)
    INC  HL
    LD   D, (HL)
    POP  HL
    PUSH BC
    PUSH HL
    PUSH HL
    LD   HL, (M_VARS.esc_param+3)
    INC  HL
    LD   C, (HL)
    INC  HL
    LD   B, (HL)
    POP  HL
    ADD  HL, BC
    LD   C, L
    LD   B, H
    POP  HL
    CALL mov_hl_bc
    LD   A, (M_VARS.esc_param+5)
    OR   A
    JP   Z, .l1
    EX   DE, HL
.l1:
    LD   A, (M_VARS.esc_var1)
    SUB  0x4
.l2:
    PUSH AF
    LD   A, (M_VARS.esc_var0)
.l3:
    PUSH AF
    LD   A, (DE)
    INC  DE
    PUSH DE
    PUSH AF
    LD   A, (DE)
    LD   E, A
    POP  AF
    LD   D, A
    OR   E
    CPL
    PUSH AF
    AND  (HL)
    OR   D
    LD   (BC), A
    INC  BC
    INC  HL
    POP  AF
    AND  (HL)
    OR   E
    LD   (BC), A
    INC  BC
    INC  HL
    POP  DE
    INC  DE
    POP  AF
    DEC  A
    JP   NZ, .l3
    POP  AF
    DEC  A
    JP   NZ, .l2
    LD   A, (M_VARS.esc_param+5)
    OR   A
    JP   Z, .l4
    EX   DE, HL
.l4:
    CALL mov_hl_bc
    POP  DE
    EX   DE, HL
    LD   A, (M_VARS.esc_var0)
    LD   C, A
    LD   A, (M_VARS.esc_var1)
    LD   B, A
    LD   A, 0x1
    OUT  (SYS_DD17PB), A
    CALL put_image                                  ; TODO: replace call+ret to jp
    RET

; ---------------------------------------------------
; Move form [HL] to [BC] count of bytes
; Inp: HL -> src
;      BC -> dst
;      esc_var0*4 - count
; ---------------------------------------------------
mov_hl_bc:
    PUSH DE
    LD   A, (M_VARS.esc_var0)
    ADD  A, A
    ADD  A, A
    LD   E, A                                       ; E = param * 4
    ; move [HL] -> [BC] E bytes
.next:
    LD   A, (HL)
    LD   (BC), A
    INC  HL
    INC  BC
    DEC  E
    JP   NZ, .next
    POP  DE
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
    JP   C, l3
    LD   D, A
    LD   A, L
    SUB  B
    LD   E, A
    CALL dc_put_pixel
l3:
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

    ; Full charset, Common + Latin letters  (112*8=890)
    INCLUDE "font-6x7.inc"

; ---------------------------------------------------
; Convert 0h..Fh decimal value to symbol '0'..'F'
; ---------------------------------------------------
conv_nibble:
    AND  0xf
    ADD  A, 0x90
    DAA
    ADC  A, 0x40
    DAA
    LD   C, A
    RET

; ---------------------------------------------------
; Print byte in HEX
; Inp: A - byte to print
; ---------------------------------------------------
m_hexb:
    PUSH AF
    RRCA
    RRCA
    RRCA
    RRCA
    CALL out_hex
    POP  AF

out_hex:
    CALL conv_nibble
    CALL m_con_out                                  ; TODO: replace call+ret to jp
    RET

; ---------------------------------------------------
; Wtite RAM-Disk 64K to TAPE
; ---------------------------------------------------
m_tape_write_ram2:
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
; Read RAM-Disk 64K from TAPE
; ---------------------------------------------------
m_tape_read_ram2:
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
    CALL m_hexb
    POP  BC
    LD   A, C
    CALL m_hexb                                     ; TODO: replace call+ret to jp
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
    AND  0x1                                        ; Low 32K or Hi page or Ext RAM
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
    OR   0x2
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
;  In: DE - block ID,
;      HL -> block of data.
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
;  In: HL -> buffer to receive bytes from Tape
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
    LD   A, 0x0
    OUT  (FDC_DATA), A
    LD   A, FDC_RESTORE_UH_NV
    OUT  (FDC_CMD), A
    NOP
    NOP

.wait_no_busy:
    IN   A, (FDC_CMD)
    AND  00000101b                                  ; Track0 ,  Busy
    CP   00000100b
    JP   Z, .tr0_ok

    IN   A, (FLOPPY)
    RLCA                                            ; MOTST -> CF
    JP   NC, .wait_no_busy
    LD   A, 0x20
    RET
.tr0_ok:
    LD   A, B
    DEC  A
    LD   A, 0x1
    JP   Z, .b1
    LD   (M_VARS.ul_A_var1), A
    XOR  A
    LD   (M_VARS.ul_A_var4), A
    RET

.b1:
    LD   (M_VARS.ul_B_var2), A
    XOR  A
    LD   (M_VARS.ul_B_var5), A
    RET

; ---------------------------------------------------
;
; ---------------------------------------------------
m_select_drive:
    PUSH AF
    CALL delay_1.4mS
    CP   0x1                                        ; TODO: DEC A to save 1b 3t
    JP   Z, .sel_A
    LD   A, 0x2
    JP   .sel_B
.sel_A
    LD   A, 0x5
.sel_B
    LD   B, A                                       ; 010b or 101b
    IN   A, (FLOPPY)                                ; read Floppy controller reg
    AND  0x40                                       ; SSEL
    RRA                                             ; SSEL for out
    OR   B                                          ; 0x22 or 0x25 if WP
    OUT  (FLOPPY), A                                ; Select drive A or B
    LD   B, A
    POP  AF
    DEC  A
    JP   Z, .dpar_a
    LD   A, (bios_var2)
    JP   .dpar_b
.dpar_a
    LD   A, (BIOS.bios_var04)
.dpar_b
    PUSH BC
    LD   B, A                                        ; B = dpar_A or B
    LD   A, D                                        ; compare with D?
    CP   B
    JP   C, .l_le                                    ; D < B
    SUB  B
    LD   D, A
    POP  BC
    LD   A, C
    OR   0x8
    LD   C, A
    LD   A, B
    AND  0x20
    OR   A
    RET  NZ
    LD   A, B
    OR   0x20                                       ; set SSEL to 1
    OUT  (FLOPPY), A
    CALL delay_136uS
    RET
.l_le
    POP  BC
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
    PUSH AF
    CALL m_select_drive
    POP  AF
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
    PUSH AF
    CALL m_select_drive
    POP  AF
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
    LD   B, A
    LD   A, (M_VARS.ul_var3)
    CP   B
    JP   Z, .need_m_start
    CALL .wait_motor                                ; TODO: replace call+ret to jp
    RET
.need_m_start:
    IN   A, (FLOPPY)
    RLCA                                            ; check MOTST
    JP   C, .wait_motor
    IN   A, (FDC_CMD)
    AND  FDC_NOT_READY                              ; not ready flag
    RET  Z

; ---------------------------------------------------
;
; ---------------------------------------------------
.wait_motor:
    PUSH BC
    LD   BC, 65535
    CALL fdc_init
.wait_rdy1:
    IN   A, (FDC_CMD)
    AND  FDC_NOT_READY
    JP   Z, .long_delay
    IN   A, (FLOPPY)
    RLCA                                            ; CF<-A[7] MOTST flag
    JP   NC, .wait_rdy1
    LD   A, 0x20
    JP   .mst_exi
.long_delay:
    DEC  BC
    LD   A, B
    OR   A
    JP   NZ, .long_delay
.mst_exi:
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
    LD   A, B
    DEC  A
    JP   Z, .drv_b
    LD   A, (M_VARS.ul_A_var1)
    OR   A
    CALL Z, fdc_unload_head
    RET  C
    LD   A, (M_VARS.ul_A_var4)
    OUT  (FDC_TRACK), A
    LD   A, D
    LD   (M_VARS.ul_A_var4), A
    JP   .cmn
.drv_b:
    LD   A, (M_VARS.ul_B_var2)
    OR   A
    CALL Z, fdc_unload_head
    RET  C
    LD   A, (M_VARS.ul_B_var5)
    OUT  (FDC_TRACK), A
    LD   A, D
    LD   (M_VARS.ul_B_var5), A
.cmn:
    LD   A, (M_VARS.ul_var3)
    CP   B
    LD   A, B
    LD   (M_VARS.ul_var3), A
    JP   NZ, .l2
    IN   A, (FDC_TRACK)
    CP   D
    JP   Z, .l2
    JP   C, .l1
    LD   A, (M_VARS.ul_var6)
    OR   A
    JP   NZ, .l2
    LD   B, 0xff
    CALL delay_b
    LD   A, 0x1
    LD   (M_VARS.ul_var6), A
    JP   .l2
.l1:
    LD   A, (M_VARS.ul_var6)
    OR   A
    JP   Z, .l2
    LD   B, 0xff
    CALL delay_b
    LD   A, 0x0
    LD   (M_VARS.ul_var6), A
.l2:
    LD   A, D
    OUT  (FDC_DATA), A
    LD   A, 0x1f
    OUT  (FDC_CMD), A
    NOP
    NOP
    IN   A, (FDC_WAIT)
    IN   A, (FDC_CMD)
    AND  0x19
    CP   0x0
    JP   NZ, .l3
    JP   .l4
.l3:
    SCF
    LD   A, 0x40
.l4:
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
    IN   A, (FDC_CMD)
    AND  11011111b
    CP   0x0
    JP   Z, fdc_ret
    SCF
fdc_ret:
    RET

filler1:
    DB        20h

; filler:
;     ds        169, 0xff

; ------------------------------------------------------

LAST        EQU     $
CODE_SIZE   EQU     LAST-0xE000
FILL_SIZE   EQU     ROM_CHIP_SIZE-CODE_SIZE


    ASSERT m_hot_start = 0xe051
    ASSERT m_out_strz = 0xe0f1
    ASSERT m_con_out_int = 0xe16d
    ASSERT get_esc_param = 0xe187
    ASSERT esc_params_tab = 0xe1cb
    ASSERT esc_handler_tab = 0xe1db
    ASSERT esc_set_beep = 0xe1f9
    ASSERT m_print_hor_line = 0xe23a
    ASSERT m_get_7vpix = 0xe2b4
    ASSERT esc_set_palette = 0xe2fe
    ASSERT m_get_glyph = 0xe320
    ASSERT m_print_no_esc = 0xe349
    ASSERT calc_addr_40 = 0xe439
    ASSERT mp_mode_64 = 0xe4b8
    ASSERT calc_addr_80 = 0xe612
    ASSERT m_clear_screen = 0xe639
    ASSERT m_cursor_home = 0xe66c
    ASSERT m_draw_cursor = 0xe69a
    ASSERT m_handle_esc_code = 0xe77c
    ASSERT handle_cc_common = 0xe7c4
    ASSERT handle_cc_80x25 = 0xe833
    ASSERT m_beep = 0xe85a
    ASSERT esc_set_cursor = 0xe890
    ASSERT esc_set_vmode = 0xe8e9
    ASSERT esc_set_color = 0xe92f
    ASSERT m_print_at_xy = 0xe943
    ASSERT game_sprite_tab = 0xea39
    ASSERT esc_draw_fill_rect = 0xeb64
    ASSERT draw_line_h = 0xeed1
    ASSERT esc_draw_line = 0xef0b
    ASSERT esc_draw_dot = 0xf052
    ASSERT esc_picture = 0xf0a4
    ASSERT m_fn_39 = 0xf10f
    ASSERT get_image_hdr = 0xf177
    ASSERT esc_get_put_image = 0xf1b5
    ASSERT pict_sub2 = 0xf3ca
    ASSERT m_font_cp0 = 0xf5bc
    ASSERT me_out_strz = 0xfb36
    ASSERT m_ramdisk_write = 0xfb6d
    ASSERT m_tape_write = 0xfb97
    ASSERT m_tape_wr_byte = 0xfc0e
    ASSERT m_tape_read_byte = 0xfcee
    ASSERT m_read_tape_bit = 0xfd08
    ASSERT m_tape_wait = 0xfd58


   ; DISPLAY "Code size is: ", /A, CODE_SIZE


FILLER
    DS  FILL_SIZE, 0xFF
   ; DISPLAY "Free size is: ", /A, FILL_SIZE

    ENDMODULE

    OUTEND

    OUTPUT m_vars.bin
        ; put in separate waste file
        INCLUDE "m_vars.inc"
    OUTEND
