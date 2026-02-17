; ======================================================
; Ocean-240.2
; Equates for all assembly sources
;
; By Romych 2025-09-09
; ======================================================

	IFNDEF	_EQUATES
	DEFINE	_EQUATES

ADDLIST				EQU	0x08
ASCII_BELL			EQU	0x07                        ; Make Beep
ASCII_BS			EQU	0x08                        ; Move cursor left (Back Space)
ASCII_TAB			EQU	0x09                        ; Move cursor right +8 pos
ASCII_LF			EQU	0x0A                        ; Move cursor down (Line Feed)
ASCII_FF			EQU	0x0C                        ; Move cursor to home (Form Feed)
ASCII_CR			EQU	0x0D                        ; Move gursor to 1st pos (Cariage Return)
ASCII_CAN			EQU	0x18                        ; Move cursor right
ASCII_EM			EQU	0x19                        ; Move cursor up
ASCII_SUB			EQU	0x1A                        ; CTRL-Z - end of text file marker
ASCII_ESC			EQU	0x1B                        ;
ASCII_US			EQU	0x1F                        ; Clear screen
ASCII_SP            EQU 0x20
ASCII_DEL			EQU	0x7F

; ------------------------------------------------------
BDOS_NFUNCS			EQU	41
BELL_PIN			EQU	0x08                        ; DD67 Pin PC3 - "BELL"
; ------------------------------------------------------
CCP_COMMAND_SIZE	EQU	5                           ; max length of CCP command
CCP_COMMAND_COUNT	EQU	3                           ; Count of CCP commands
CCP_COMMAND_CNT     EQU 6
CCP_COMMAND_LEN     EQU 4

CCP_SRC_ADDR		EQU	0xc000                      ; Address of CCP resident part in ROM
CCP_DST_ADDR		EQU 0xb200                      ; Address of CCP resident part in RAM
CCP_SIZE			EQU 0x809

CPM_VERSION         EQU 0x22                        ; Version of CP/M as byte nibbles '2.2'

CTRL				EQU	0x5E                        ; ^
CTRL_C				EQU	0x03                        ; Warm boot
CTRL_H		        EQU	0x08                        ; Backspace
CTRL_E				EQU	0x05                        ; Move to beginning of new line (Physical EOL)
CTRL_J              EQU 0x0A                        ; LF - Line Feed
CTRL_M              EQU 0x0D                        ; CR - Carriage Return
CTRL_P				EQU	0x10                        ; turn on/off printer
CTRL_R              EQU 0x12                        ; Repeat current cmd line
CTRL_S				EQU	0x13                        ; Temporary stop display data to console (aka DC3)
CTRL_U              EQU 0x15                        ; Cancel (erase) current cmd line
CTRL_X              EQU 0x18                        ; Cancel (erase) current cmd line

; ------------------------------------------------------
DBPLIST				EQU	0x0F
DEF_DISK_A_SIZE		EQU	0x3F
DEF_DISK_B_SIZE		EQU	0x0168
DIR_BUFF_SIZE		EQU	128
DSK_MAP				EQU	0x10
DSK_MSK				EQU	0x03
DSK_SHF				EQU	0x02
DMA_BUFF_SIZE       EQU 0x80
; ------------------------------------------------------
ENDDIR				EQU	0xFFFF
ESC_CMD_END			EQU	0x1A
;EXT_NUM   		    EQU	12                          ; Extent byte offset
; ------------------------------------------------------
FALSE				EQU	0x00
FDC_DD80RB			EQU	0x21
FDC_NOT_READY		EQU	0x80
FDC_RESTORE_L		EQU	0x08
FDC_SEEK_LV			EQU	0x1C
FDC_RESTORE_UH_NV   EQU 0x03                        ; Restore Unload Head, No Verify, 15ms Rate
FILE_DELETED        EQU	0xE5
FWF_MASK			EQU	0x80                        ; File Write Flag mask

; ---------------------------------------------------
; FCB Offsets
; ---------------------------------------------------
FCB_LEN				EQU	32                          ; length of FCB
FCB_SHF				EQU	5
FN_LEN              EQU 12                          ; Length of filename in FCB

FCB_DR              EQU 0                           ; Drive. 0 for default, 1-16 for A-P
FCB_FN              EQU 1                           ; Fn - Filename, 7-bit ASCII. The top bits - attributes
FCB_FT              EQU 9                           ; Filetype, 7-bit ASCII.
                                                    ; T1' to T3' have the following
                                                    ; T1' - Read-Only
                                                    ; T2' - System (hidden)
                                                    ; T3' - Archive
FCB_EXT             EQU 12                          ; EX - Set this to 0 when opening a file and then leave it to
                                                    ; CP/M. You can rewind a file by setting EX, RC, S2 and CR to 0.
FCB_S1              EQU 13                          ; S1 - Reserved.
FCB_S2              EQU 14                          ; S2 - Reserved. Bit 7 - wile write flag, [6:0] module number (Extent hi bits)
FCB_RC              EQU 15                          ; RC - Set this to 0 when opening a file and then leave it to CP/M.
FCB_AL              EQU 16                          ; AL - Image of the second half of the directory entry,
                                                    ; containing the file's allocation (which disc blocks it owns).
FCB_CR              EQU 32                          ; CR - Current record within extent. It is usually best to set
                                                    ; this to 0 immediately after a file has been opened and
                                                    ; then ignore it.
FCB_RN              EQU 33                          ; Rn - Random access record number. A 16-bit (with R2 used for overflow)


; ------------------------------------------------------
JP_OPCODE			EQU	0xC3
; ------------------------------------------------------
IRQ_0               EQU 0x01
IRQ_1               EQU 0x02
IRQ_2               EQU 0x04
;LP_IRQ              EQU 0x08
KBD_IRQ				EQU	0x02

KBD_ACK				EQU	0x10

KEY_ALF				EQU	0x0D
KEY_FIX				EQU	0x15
; ------------------------------------------------------
LST_REC				EQU	0x7F
; ------------------------------------------------------
MAX_EXT				EQU	0x1F
MAX_MOD				EQU	0x0F
;MOD_NUM			EQU	0x0E
; ------------------------------------------------------
FCB_INFO_LEN		EQU	15                          ; length of FCB info bytes to match
;NXT_REC				EQU	0x20
; ------------------------------------------------------
PIC_POLL_MODE		EQU	0x0A
PORT_C4				EQU	0x10
PRINTER_ACK			EQU	0x10
PRINTER_IRQ			EQU	0x08
; ------------------------------------------------------
;RAN_REC				EQU	0x21
;FCB_RC				EQU	0x0F
RO_FILE				EQU	0x09
ROM_CHIP_SIZE       EQU 8192                        ; ROM Size, used to limit monitor code size

RX_READY			EQU	0x02
; ------------------------------------------------------
TAPE_D				EQU	0x08
TAPE_P				EQU	0x04
TIMER_IRQ			EQU	0x10
TL_HIGH				EQU	0x05
TL_LOW				EQU	0x03
TL_MID				EQU	0x04
TMR0_SQWAVE			EQU	0x36
TRUE				EQU	0xFF
TX_READY			EQU	0x01
; ------------------------------------------------------

	ENDIF