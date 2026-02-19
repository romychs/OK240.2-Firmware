; =======================================================
; Ocean-240.2
; Module M_VARS - Monitor variables
; RAM Range: 0xBA09-0xBFFF
;
; Disassembled by Romych 2026-02-18
; =======================================================

    IFNDEF  _M_VARS
    DEFINE  _M_VARS

    MODULE  M_VARS
    ORG     0xbf00

buffer:
        DS 128                                      ; bf00
        DS 64                                       ; bf80
stor_sp:
        DS 14                                       ; bfc0
stack_2:
        DS 2                                        ; bfce
stor_de:
stor_e:
        DS 1                                        ; bfd0
stor_d
        DS 1                                        ; bfd1
stor_bc:
stor_c:
        DS 1                                        ; bfd2
stor_b:
        DS 1                                        ; bfd3
        DS 2                                        ; bfd4
stor_af:
stor_f:
        DS 1                                        ; bfd6
stor_a:
        DS 1                                        ; bfd7
stack_0:
        DS 1                                        ; bfd8
stor_s:
        DS 1                                        ; bfd9
store_hl
        DS 1                                        ; bfda
stor_m
        DS 1                                        ; bfdb
start_jp
        DS 1                                        ; bfdc
store_pc:
        DS 1                                        ; bfdd
stor_p:
        DS 1                                        ; bfde
esc_mode:
        DS 1                                        ; bfdf
esc_cmd:
        DS 1                                        ; bfe0
esc_param_cnt:
        DS 1                                        ; bfe1
esc_param:
        DS 10                                       ; bfe2
screen_mode:
        DS 1                                        ; bfec
cursor_row:
        DS 1                                        ; bfed
cursor_col:
        DS 1                                        ; bfee
curr_color:
        DS 2                                        ; bfef
row_shift:
        DS 1                                        ; bff1
codepage:
        DS 1                                        ; bff2
cur_palette:
        DS 1                                        ; bff3
beep_period:
        DS 2                                        ; bff4
beep_duration:
        DS 2                                        ; bff6
pix_shift:
        DS 1                                        ; bff8
strobe_state:
        DS 1                                        ; bff9
ul_var0:
        DS 1                                        ; bffa
ul_var1:
        DS 1                                        ; bffb
ul_var2:
        DS 1                                        ; bffc
ul_var3:
        DS 1                                        ; bffd
free_vars:
        DS 1                                        ; bffe
        DS 1                                        ; bfff

    ; ASSERT stack_top   = 0xbfb8
    ; ASSERT stack       = 0xbf00
    ; ASSERT hrg         = 0xbf88
    ; ASSERT rst_hl_save    = 0xbfc6
    ; ASSERT rst_ret_addr   = 0xbfcb
    ; ASSERT stack_0     = 0xbfd2
    ; ASSERT esc_mode     = 0xbfd9
    ; ASSERT screen_mode  = 0xbfe6
    ; ASSERT mc_stored_key  = 0xbffd

    ; DISPLAY " esc_mode is: ", esc_mode
    ; DISPLAY " esc_mode is: ", esc_mode
    ; DISPLAY " rst_ret_addr is: ", rst_ret_addr
    ; DISPLAY " tstack_top is: ", tstack_top
    ; DISPLAY " mc_stored_key is: ", mc_stored_key

    ENDMODULE

    ENDIF
