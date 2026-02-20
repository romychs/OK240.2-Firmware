; =======================================================
; Ocean-240.2
;
; CPM filler, between BIOS and CCP_ROM
;
; By Romych 2025-09-09
; =======================================================

	IFNDEF	BUILD_ROM
		OUTPUT ccp_fill_1.bin
	ENDIF

	MODULE CPM_FILL_1
	ORG	0xda00

FILL1 DS   107, 0
FILL2 DB   0x7b
FILL3 DS   148, 0xff

	ENDMODULE

	IFNDEF	BUILD_ROM
		OUTEND
	ENDIF