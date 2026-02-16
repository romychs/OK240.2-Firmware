; =======================================================
; Ocean-240.2
; Module CPM_VARS - BIOS + BDOS + CPP variables
; RAM Range: 0xBA09-0xBF00
;
; Disassembled by Romych 2025-09-09
; =======================================================

    IFNDEF _CPM_VARS
    DEFINE _CPM_VARS

    MODULE CPM_VARS
    ORG 0xBA09

cpm_vars_start  EQU $
; Filled by zeroes by BIOS at cold boot
; until ccp_vars_end

; Output disabel if non zero
bdos_no_outflag DB  0
bdos_strtcol    DB  0
bdos_column     DB  0

; Copy output to printer in non zero
bdos_prnflag    DB  0
bdos_kbchar     DB  0

; Stored user stack pointer on enter bdos fn
bdos_usersp     DW  0                               ; 0xba0e

; BDOS Stack
                DS  48, 0
bdos_stack      EQU $

; -------------------------------------------------------
; Common values shared between bdosi and bdos
; -------------------------------------------------------

; current user number 0..15
bdos_userno     DB 0x00                             ; 0xba40

; current disk number
bdos_curdsk     DB 0x00

; reg DE (pointer) value to call BDOS fn
bdos_info       DW 0x0000                           ; 0xba42

; Address value to return
bdos_aret       DW 0x0000                           ; 0xba44
; Empty FCB
bdos_efcb       DB 0x00                             ; 0xba46 = 0xE5 at init
; Read-only disks flag
bdos_rodsk      DW 0x0000                           ; 0xba47
; Drive login vector
bdos_dlog       DW 0x0000                           ; 0xba49
; Address of DMA buffer
bdos_dmaad      DW 0x0000                           ; 0xba4b = 0x80 at init
bdos_cdrmax     DW 0x0000                           ; 0xba4d
; Current track
bdos_curtrk     DW 0x0000                           ; 0xba4f
; Current record (sector)
bdos_currec     DW 0x0000                           ; 0xba51
; Address of user buffer
bdos_buffa      DW 0x0000                           ; 0xba53

; Address of cur disk DPB
bdos_dpbaddr    DW 0x0000                           ; 0xba55
; Calculated checksum
bdos_cksum      DW 0x0000                           ; 0xba57
; Address of cur disk allocation map
bdos_alloca     DW 0x0000                           ; 0xba59
; Sectors per track
dbos_sectpt     DW 0x0000                           ; 0xba5b
bdos_blkshf     DB 0x00                             ; 0xba5d
bdos_blmsk      DB 0x00                             ; 0xba5e
bdos_extmsk     DB 0x00                             ; 0xba5f
; Max allocation number (disk size)
bdos_maxall     DW 0x0000                           ; 0xba60
bdos_dirmax     DW 0x0000                           ; 0xba62
; Size of directory
bdos_dirblk     DW 0x0000                           ; 0xba64
bdos_alloc1     DW 0x0000                           ; 0xba66
; First track offset
bdos_offset     DW 0x0000                           ; 0xba68
; Disk traslation vector address
bdos_tranv      DW 0x0000                           ; 0xba6a
; Open and closed flag 0xff - closed
bdos_fcb_closed DB 0x00                             ; 0xba6c
; Read file modified flag. 0xff - not allow read unwritten
bdos_rfm        DB 0x00                             ; 0xba6d
bdos_dirloc     DB 0x00                             ; 0xba6e
; File access mode (0=random, 1=sequential, 2=special)
bdos_seqio      DB 0x00                             ; 0xba6f
; Reg E value to call BDOS fn
bdos_linfo      DB 0x00                             ; 0xba70
bdos_dminx      DB 0x00                             ; 0xba71
; Length in bytes to match when search
bdos_searchl    DB 0x00                             ; 0xba72
; Address of buffer where to search
bdos_searcha    DW 0x0000                           ; 0xba73
                DW 0x0000
; 0 - big disk (>255 blk), 0xff - small disk
bdos_small_disk DB 0x00                             ; 0xba77
; Drive selected (0xff) by user specified FCB flag
bdos_resel      DB 0x00                             ; 0xba78
bdos_olddsk     DB 0x00                             ; 0xba79
; Saved disk of user FCB
bdos_fcbdsk     DB 0x00                             ; 0xba7a
; Number of records in extent
bdos_rcount     DB 0x00                             ; 0xba7b
bdos_extval     DB 0x00                             ; 0xba7c
; Record number to read
bdos_vrecord    DW 0x0000                           ; 0xba7d
; Record to access
bdos_arecord    DW 0x0000                           ; 0xba7f
bdos_arecord1   DW 0x0000                           ; 0xba81
bdos_dptr       DB 0x00                             ; 0xba83
bdos_dcnt       DW 0x00                             ; 0xba84
bdos_drec       DS 20, 0x00                         ; 0xba86
tmp_dir_total   DW 0x0000                           ; 0xba9a
; Save pointer at ccp entry, restore on exit
saved_stack_ptr DW 0x0000                           ; 0xba9c
                DS 22, 0
ccp_safe_stack
                DW 0x0000                           ; 0xbab4

; --------------------------------------------------
; FCB
; --------------------------------------------------
ccp_fcb
ccp_fcb_dr      DB 0                                ; 0xbab6 drive letter to write file
ccp_fcb_fn      DS 8,0                              ; 0xbab7 file name
ccp_fcb_ft      DS 3,0                              ; file type
ccp_fcb_ext     DB 0                                ; extent
ccp_fcb_s1      DB 0
ccp_fcb_s2      DB 0                                ; extent hi bits
ccp_fcb_rc      DB 0
ccp_fcb_al      DS 16, 0                            ; Second half of FCB
ccp_fcb_cr      DB 0x00                             ; Current record
ccp_fcb_rn      DB 0x00                             ; Random access record number
                DW 0x00
                DW 0x00
                DW 0x00

ccp_vars_end    EQU $

; --------------------------------------------------
; Disk parameters headers/blocks in RAM
; --------------------------------------------------
DPH_RAM         DS 16, 0                            ; 0xbade
DPH1_RAM        DS 16, 0                            ; 0xbaee
DPH2_RAM        DS 16, 0                            ; 0xbafe
tmp_buff9       DS 9, 0
DPB_A_RAM       DS 15, 0                            ; 0xbb17
DPB_B_RAM       DS 15, 0                            ; 0xbb26
DPB_C_RAM       DS 15, 0                            ; 0xbb35

; --------------------------------------------------
cur_disk        DB 0x00                             ; 0xbb44
dma_addr        DW 0x0000                           ; 0xbb46
curr_track      DB 0x00                             ; 0xbb47
curr_sec        DB 0x00                             ; 0xbb48
slicer_disk     DB 0x00                             ; 0xbb49
slicer_track    DB 0x00                             ; 0xbb4a
slicer_real_sector  DB 0x00                         ; 0xbb4b
tmp_slicer_real_sector  DB 0x00                     ; 0xbb4c
slicer_has_data DB 0x00                             ; 0xbb4d
slicer_need_save    DB 0x00                         ; 0xbb4e
slicer_uninited_count   DB 0x00                     ; 0xbb4f
slicer_uninited_disk    DB 0x00                     ; 0xbb42
slicer_uninited_track   DB 0x00                     ; 0xbb43
slicer_uninited_sector_128  DB 0x00                 ; 0xbb44
tmp_slicer_result   DB 0x00                         ; 0xbb45
tmp_slicer_can_read DB 0x00                         ; 0xbb46
tmp_slicer_operation    DB 0x00                     ; 0xbb47
tmp_slicer_flush    DB 0x00                         ; 0xbb48

; Directory buffer used by BDOS for directory operations
dir_buffer      DS DIR_BUFF_SIZE, 0                 ; 0xbb49

; Allocation map for drive A (Mark used allocation blocks)
AL_MAP_A        DS 31, 0                            ; 248 bit
; Check vector for drive A  (Checksumm of dir entries, used to detect disk change)
CHK_VEC_A       DS 16, 0                            ; 0xbbf6

; Allocation map for drive B
AL_MAP_B        DS 45, 0                            ; 184 bit
; Check vector for drive B
CHK_VEC_B       DS 32, 0                            ; 0xbc33

; Allocation map for drive C
AL_MAP_C        DS 45, 0                            ; 184 bit
; Check vector for drive C
CHK_VEC_C       DS 32, 0                            ; 0xbc80
                DS 96, 0

slicer_buffer   DS 512, 0                           ; 0xbd00

cpm_vars_end    EQU     $

ccp_vars_size   EQU ccp_vars_end-cpm_vars_start
cpm_vars_size   EQU cpm_vars_end-cpm_vars_start

    ; check integrity of original ROM
    ASSERT tmp_dir_total   = 0xba9a
    ASSERT saved_stack_ptr = 0xba9c
    ASSERT ccp_fcb_fn      = 0xbab7
    ASSERT DPH_RAM         = 0xbade
    ASSERT cur_disk        = 0xbb44
    ASSERT slicer_uninited_count = 0xbb4f
    ASSERT slicer_buffer  = 0xbd00

	DISPLAY "| CCP_VARS\t| ",/H,cpm_vars_start,"  | ",/H,cpm_vars_size," |\t    |"

    DISPLAY "cur_disk:",/H,cur_disk

    ENDMODULE

    ENDIF
