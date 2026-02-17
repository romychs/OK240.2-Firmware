; =======================================================
; Ocean-240.2
; Module M_VARS - Monitor variables
; RAM Range: 0xBA09-0xBFFF
;
; Disassembled by Romych 2025-02-05
; =======================================================

    IFNDEF  _M_VARS
    DEFINE  _M_VARS

    MODULE M_VARS
    ORG   0xbf00

buffer          DS 128                              ; 0xbf00 search text or buffer to search?
                DS 36
paint_stack     EQU $                               ; 0xbfa4
                DS 32
stack1:         EQU $                               ; 0xbfc4
                DS 1
paint_var1      DS 1                                ; 0xbfc5
paint_var2      DS 1                                ; 0xbfc6
paint_var3      DS 1                                ; 0xbfc7
paint_var4      DS 1                                ; 0xbfc8
paint_var5      DS 1                                ; 0xbfc9
paint_y         DS 1                                ; 0xbfca
paint_var7      DS 1                                ; 0xbfcb
cmp_color       DS 1                                ; 0xbfcc
paint_sp_save   DS 2

; Right pixel mask ex: 11111000
pixel_mask_r    DS 1                                ; 0xbfcf
tmp_color       DS 1                                ; 0xbfd0
rect_var2       DS 1                                ; 0xbfd1
stack_0         EQU $
rect_var3       DS 1                                ; 0xbfd2
esc_mode        DS 1                                ; 0xbfd3
esc_cmd         DS 1                                ; 0xbfd4

esc_param_cnt   DS 1                                ; 0xbfd5
esc_param       DS 7                                ; 0xbfd6

; Left inverse pixel mask ex: 00011111
pixel_mask_l_i  DS 1
; Right inverse pixel mask ex: 00011111
pixel_mask_r_i  DS 1
; Left pixel mask ex: 11100000
pixel_mask_l    DS 1

; Current screen mode, bit 3 - hide/show cursor, bit 4 - only 20 rows
screen_mode     DS 1                                ; 0xbfe0
cursor_row      DS 1                                ; 0xbfe1 Cursor Y position
cursor_col      DS 1                                ; 0xbfe2 Cursor X position
curr_color      DS 2                                ; 0xbfe3 Current color low and hi bytes
esc_hex_cmd:
row_shift       DS 1                                ; 0xbfe5
codepage        DS 1                                ; 0xbfe6


cur_palette     DS 1                                ; 0xbfe7 00bbbfff - background and foreground colors
beep_period     DS 2                                ; 0xbfe8
beep_duration   DS 2                                ; 0xbfea
pix_shift       DS 1                                ; 0xbfec
strobe_state    DS 1                                ; 0xbfed
ul_var0         DS 1                                ; 0xbfee
ul_A_var1       DS 1                                ; 0xbfef
ul_B_var2       DS 1                                ; 0xbff0
ul_var3         DS 1                                ; 0xbff1
ul_A_var4       DS 1                                ; 0xbff2
ul_B_var5       DS 1                                ; 0xbff3
ul_var6         DS 1                                ; 0xbff4
esc_var0        DS 1                                ; 0xbff5
esc_var1        DS 1                                ; 0xbff6
esc_var2        DS 1                                ; 0xbff7
esc_var3        DS 1                                ; 0xbff8
                DS 1                                ; 0xbff9
                DS 1                                ; 0xbffa
                DS 1                                ; 0xbffb
                DS 1                                ; 0xbffc
                DS 1                                ; 0xbffd
                DS 1                                ; 0xbffe
                DS 1                                ; 0xbfff

    ASSERT stack1           = 0xbfc4
    ASSERT buffer           = 0xbf00
    ASSERT paint_var1       = 0xbfc5
    ASSERT pixel_mask_r     = 0xbfcf
    ASSERT stack_0          = 0xbfd2
    ASSERT esc_mode         = 0xbfd3
    ASSERT screen_mode      = 0xbfe0
    ASSERT cur_palette      = 0xbfe7
    ASSERT ul_var0          = 0xbfee
    ASSERT paint_stack      = 0xbfa4
    ASSERT esc_var3         = 0xbff8

    ;DISPLAY "screen_mode: ", /H, screen_mode
    ;DISPLAY "fn48_var1: ", /H, fn48_var1

    ENDMODULE

    ENDIF
