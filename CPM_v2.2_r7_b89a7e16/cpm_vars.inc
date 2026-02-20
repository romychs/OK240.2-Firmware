; =======================================================
; Ocean-240.2
; Module CPM - BIOS/BDOS/CPP variables
; RAM Range: 0xba09-0xbf00
;
; Disassembled by Romych 2025-09-09
; =======================================================

    IFNDEF _CPM_VARS
    DEFINE _CPM_VARS

    MODULE CPM_VARS
    ORG 0xba09

cpm_vars_start  EQU $
; Filled by zeroes by BIOS at cold boot
; until ccp_vars_end

; Output disabel if non zero
bdos_no_outflag DS 1
bdos_strtcol    DS 1
bdos_column     DS 1

; Copy output to printer in non zero
bdos_prnflag    DS 1
bdos_kbchar     DS 1

; Stored user stack pointer on enter bdos fn
bdos_usersp     DS 2                             ; 0xba0e

; BDOS Stack
                DS 48
bdos_stack      EQU $

; -------------------------------------------------------
; Common values shared between bdosi and bdos
; -------------------------------------------------------

; current user number 0..15
bdos_userno     DS 1                                 ; 0xba40

; current disk number
bdos_curdsk     DS 1

; reg DE (pointer) value to call BDOS fn
bdos_info       DS 2                                 ; 0xba42

; Address value to return
bdos_aret       DS 2                                 ; 0xba44
; Empty FCB
bdos_efcb       DS 1                                 ; 0xba46 = 0xE5 at init
; Read-only disks flag
bdos_rodsk      DS 2                                 ; 0xba47
; Drive login vector
bdos_dlog       DS 2                                 ; 0xba49
; Address of DMA buffer
bdos_dmaad      DS 2                                 ; 0xba4b = 0x80 at init
bdos_cdrmax     DS 2                                 ; 0xba4d
; Current track
bdos_curtrk     DS 2                                 ; 0xba4f
; Current record (sector)
bdos_currec     DS 2                                 ; 0xba51
; Address of user buffer
bdos_buffa      DS 2                                 ; 0xba53

; Address of cur disk DPB
bdos_dpbaddr    DS 2                                 ; 0xba55
; Calculated checksum
bdos_cksum      DS 2                                 ; 0xba57
; Address of cur disk allocation map
bdos_alloca     DS 2                                 ; 0xba59
; Sectors per track
dbos_sectpt     DS 2                                 ; 0xba5b
bdos_blkshf     DS 1                                 ; 0xba5d
bdos_blmsk      DS 1                                 ; 0xba5e
bdos_extmsk     DS 1                                 ; 0xba5f
; Max allocation number (disk size)
bdos_maxall     DS 2                                 ; 0xba60
bdos_dirmax     DS 2                                 ; 0xba62
; Size of directory
bdos_dirblk     DS 2                                 ; 0xba64
bdos_alloc1     DS 2                                 ; 0xba66
; First track offset
bdos_offset     DS 2                                 ; 0xba68
; Disk traslation vector address
bdos_tranv      DS 2                                 ; 0xba6a
; Open and closed flag 0xff - closed
bdos_fcb_closed DS 1                                 ; 0xba6c
; Read file modified flag. 0xff - not allow read unwritten
bdos_rfm        DS 1                                 ; 0xba6d
bdos_dirloc     DS 1                                 ; 0xba6e
; File access mode (0=random, 1=sequential, 2=special)
bdos_seqio      DS 1                                 ; 0xba6f
; Reg E value to call BDOS fn
bdos_linfo      DS 1                                 ; 0xba70
bdos_dminx      DS 1                                 ; 0xba71
; Length in bytes to match when search
bdos_searchl    DS 1                                 ; 0xba72
; Address of buffer where to search
bdos_searcha    DS 2                                 ; 0xba73
                DS 2
; 0 - big disk (>255 blk), 0xff - small disk
bdos_small_disk DS 1                                 ; 0xba77
; Drive selected (0xff) by user specified FCB flag
bdos_resel      DS 1                                 ; 0xba78
bdos_olddsk     DS 1                                 ; 0xba79
; Saved disk of user FCB
bdos_fcbdsk     DS 1                                 ; 0xba7a
; Number of records in extent
bdos_rcount     DS 1                                 ; 0xba7b
bdos_extval     DS 1                                 ; 0xba7c
; Record number to read
bdos_vrecord    DS 2                                 ; 0xba7d
; Record to access
bdos_arecord    DS 2                                 ; 0xba7f
bdos_arecord1   DS 2                                 ; 0xba81
bdos_dptr       DS 1                                 ; 0xba83
bdos_dcnt       DS 2                                 ; 0xba84
;bdos_dcnt_hi   DS 1                                ; 0xba85
bdos_drec       DS 20                                ; 0xba86
tmp_dir_total   DS 2                                 ; 0xba9a
; Save pointer at ccp entry, restore on exit
saved_stack_ptr DS 2                                 ; 0xba9c
                DS 22
ccp_safe_stack
                DS 2                                 ; 0xbab4

; --------------------------------------------------
; FCB
; --------------------------------------------------
ccp_FCB
ccp_FCB_DR      DS 1                                ; 0xbab6 drive letter to write file
ccp_FCB_FN      DS 8                                ; 0xbab7 file name
ccp_FCB_FT      DS 3                                ; file type
cpp_FCB_EXT     DS 1                                ; extent
cpp_FCB_S1      DS 1
cpp_FCB_S2      DS 1                                ; extent hi bits
cpp_FCB_RC      DS 1
cpp_FCB_AL      DS 16                               ; Second half of FCB
ccp_FCB_CR      DS 1                                ; Current record
ccp_FCB_Rn      DS 1                                ; Random access record number
                DS 2
                DS 2
                DS 2

ccp_vars_end    EQU $

; --------------------------------------------------
; Disk parameters headers/blocks in RAM
; --------------------------------------------------
DPH_RAM         DS 16, 0                             ; 0xbade
DPH1_RAM        DS 16, 0
DPH2_RAM        DS 16, 0
DPB64K_RAM      DS 15, 0                             ; 0xbb0e
DPB360K_RAM     DS 15, 0                             ; 0xbb1d

; --------------------------------------------------
cur_disk        DS 1                                 ; 0xbb2c
dma_addr        DS 2                                 ; 0xbb2d
curr_track      DS 1                                 ; 0xbb2f
curr_sec        DS 1                                 ; 0xbb30
slicer_disk     DS 1                                 ; 0xbb31
slicer_track    DS 1                                 ; 0xbb32
slicer_real_sector
                DS 1                                 ; 0xbb33
tmp_slicer_real_sector
                DS 1                                 ; 0xbb34
slicer_has_data DS 1                                 ; 0xbb35
slicer_need_save
                DS 1                                 ; 0xbb36
slicer_uninited_count
                DS 1                                 ; 0xbb37
slicer_uninited_disk
                DS 1                                 ; 0xbb38
slicer_uninited_track
                DS 1                                 ; 0xbb39
slicer_uninited_sector_128
                DS 1                                 ; 0xbb3a
tmp_slicer_result
                DS 1                                 ; 0xbb3b
tmp_slicer_can_read
                DS 1                                 ; 0xbb3c
tmp_slicer_operation
                DS 1                                 ; 0xbb3d
tmp_slicer_flush
                DS 1                                 ; 0xbb3e

; The directory buffer is an area used by the BDOS for directory operations.
; There is only one of these for the entire system.
DIRBUFF         DS DIR_BUFF_SIZE                     ; 0xbb3f

; Allocation map for drive A (Mark used allocation blocks)
AL_MAP_A        DS 31                                ; 248 bit
; Check vector for drive A  (Checksumm of dir entries, used to detect disk change)
CHK_VEC_A       DS 16                                ; 0xbbde

; Allocation map for drive B
AL_MAP_B        DS 23                                ; 184 bit
; Check vector for drive B
CHK_VEC_B       DS 16                                ; 0xbc05

; Allocation map for drive C
AL_MAP_C        DS 23                                ; 184 bit
; Check vector for drive C
CHK_VEC_C       DS 16                                ; 0xbc2c

                DS 196

slicer_buffer   DS 512                               ; 0xbd00

cpm_vars_end    EQU $

ccp_vars_size   EQU ccp_vars_end-cpm_vars_start
cpm_vars_size   EQU cpm_vars_end-cpm_vars_start

; check integrity of original ROM
    ASSERT tmp_dir_total         = 0xba9a
    ASSERT saved_stack_ptr       = 0xba9c
    ASSERT ccp_FCB_FN            = 0xbab7
    ASSERT DPH_RAM               = 0xbade
    ASSERT slicer_uninited_count = 0xbb37
    ASSERT slicer_buffer         = 0xbd00

    DISPLAY "| CCP_VARS\t| ",/H,cpm_vars_start,"  | ",/H,cpm_vars_size," |\t    |"

    ENDMODULE

    ENDIF
