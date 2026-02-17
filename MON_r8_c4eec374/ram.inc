; =======================================================
; Ocean-240.2
;
; RAM area at address: 0x0000 - 0x0100, used by CP/M and
; HW-Monitor
; By Romych 2026-02-03
; =======================================================

	IFNDEF	_RAM
	DEFINE	_RAM

	MODULE	RAM

@warm_boot			EQU 0x0000                      ; Jump warm_boot (Restart)
@warm_boot_addr		EQU 0x0001                      ; address of warm boot entry point
@iobyte  			EQU 0x0003                      ; Input/Output mapping
@cur_user_drv		EQU 0x0004                      ; [7:4] - curent user, [3:0] - current drive
@jp_bdos_enter		EQU 0x0005                      ; Jump bdos (CALL 5 to make CP/M requests)
@bdos_ent_addr		EQU 0x0006                      ; addres of BDOS entry point
@RST1				EQU 0x0008
@RST1_handler_addr	EQU 0x0009
;RST2				EQU 0x0010
;RST3				EQU 0x0018
;RST4				EQU 0x0020
;RST5				EQU 0x0028
;RST6				EQU 0x0030
;RST7				EQU 0x0038
;reserve1			EQU 0x003b
@bios_var0			EQU 0x0040                      ; 0xaa - bios init r8
@bios_var1			EQU 0x0041                      ; 0xaa - bios init r8
@bios_var2			EQU 0x0042                      ; 0x00 - bios init r8
@bios_var3			EQU 0x0043                      ; 0xff - bios init r8
@interleave_0       EQU 0x0044
;reserve2			EQU 0x0050
@fcb1		        EQU 0x005c                      ; Default FCB, 16 bytes
@fcb2		        EQU 0x006c
;NMI_ISR				EQU 0x0066

@dma_buffer		    EQU 0x0080                      ; Default "DMA" 128 bytes buffer
@p_cmd_line_len     EQU 0x0080                      ; command line character count
@p_cmd_line         EQU 0x0081                      ; command line buffer
@fcb_ra_record_num	EQU 0x00a1
@bios_stack  		EQU	0x0100
@tpa_start		    EQU	0x0100                      ; start of program
@video_ram			EQU	0x4000

	ENDMODULE

	ENDIF