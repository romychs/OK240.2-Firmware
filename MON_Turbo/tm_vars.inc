; =======================================================
; Ocean-240.2
; Module TM_VARS - Turbo monitor variables
; RAM Range: 0xBA09-0xBFFF
;
; Disassembled by Romych 2025-09-09
; =======================================================

	IFNDEF	_TM_VARS
	DEFINE	_TM_VARS

	MODULE  TM_VARS
	ORG	    0xbf00

tm_stack		ds 80				                ; 0xbf00 search text or buffer to search?
tm_ltb 			ds 32				                ; 0xbf50
tm_strt			ds 16				                ; 0xbf70
tm_track_no		ds 1					            ; 0xbf80
tm_sector_no	ds 1					            ; 0xbf81
tm_num_sect		ds 1					            ; 0xbf82
tm_disk_op		ds 1					            ; 0xbf83
tm_rw_disk		ds 1					            ; 0xbf84
tm_drive		ds 2					            ; 0xbf85
tm_tbu			ds 1					            ; 0xbf87

; user entered command buffer
tm_hrg			ds 48   				            ; 0xbf88
tm_stack_top	ds 2				                ; 0xbfb8
tm_stsp			ds 6            	                ; 0xbfba
rst_af_save		ds 2				                ; 0xbfc0
rst_bc_save		ds 2				                ; 0xbfc2
rst_de_save		ds 2					            ; 0xbfc4
rst_hl_save		ds 2					            ; 0xbfc6
rst_sp_save		ds 2				                ; 0xbfc8
rst_ret_JP		ds 1					            ; 0xbfca
rst_ret_addr	ds 2					            ; 0xbfcb
				ds 2                                ; 0xbfcd
				ds 3                                ; 0xbfcf
tm_stack_0	    ds 2                    			; 0xbfd2
				ds 2					            ; 0xbfd4
				ds 2					            ; 0xbfd6
				ds 1				                ; 0xbfd8
m_esc_mode		ds 1					            ; 0xbfd9
m_esc_cmd		ds 1					            ; 0xbfda
m_esc_param_cnt	ds 1					            ; 0xbfdb
m_esc_param_1	ds 1					            ; 0xbfdc
m_esc_param_2	ds 1					            ; 0xbfdd
m_esc_param_3	ds 1					            ; 0xbfde
m_esc_param_4	ds 1					            ; 0xbfdf
m_esc_hex_cmd	ds 1					            ; 0xbfe0
m_esc_param_6	ds 1					            ; 0xbfe1
				ds 4    				            ; 0xbfe2
m_screen_mode	ds 1					            ; 0xbfe6
m_cursor_row	ds 1					            ; 0xbfe7
m_cursor_col	ds 1					            ; 0xbfe8
m_curr_color	ds 2				                ; 0xbfe9
m_row_shift		ds 1					            ; 0xbfeb
m_codepage		ds 1					            ; 0xbfec
m_cur_palette	ds 1					            ; 0xbfed
m_beep_period	ds 2				                ; 0xbfee
m_beep_duration	ds 2				                ; 0xbff0
m_pix_shift		ds 1					            ; 0xbff2
m_strobe_state	ds 1					            ; 0xbff3
m_print_pos_x	ds 2				                ; 0xbff4
				ds 2
				ds 2
m_last_key		ds 1					            ; 0xbffa
m_last_shifts	ds 1					            ; 0xbffb
mc_fix_state	ds 1					            ; 0xbffc
mc_stored_key	ds 1					            ; 0xbffd

	ASSERT tm_stack_top   = 0xbfb8
	ASSERT tm_stack       = 0xbf00
	ASSERT tm_hrg         = 0xbf88
	ASSERT rst_hl_save    = 0xbfc6
	ASSERT rst_ret_addr	  = 0xbfcb
	ASSERT tm_stack_0     = 0xbfd2
	ASSERT m_esc_mode 	  = 0xbfd9
	ASSERT m_screen_mode  = 0xbfe6
	ASSERT mc_stored_key  = 0xbffd

	; DISPLAY " m_esc_mode is: ", m_esc_mode
	; DISPLAY "     m_esc_mode is: ", m_esc_mode
	; DISPLAY "   rst_ret_addr is: ", rst_ret_addr
	; DISPLAY " tm_stack_top is: ", tm_stack_top
	; DISPLAY " mc_stored_key is: ", mc_stored_key

	ENDMODULE

	ENDIF