; =======================================================
; Ocean-240.2
;
; CP/M BIOS
;
; Disassembled by Romych 2025-09-09
; =======================================================

    INCLUDE "equates.inc"
    INCLUDE "ram.inc"
    INCLUDE "mon_entries.inc"

    IFNDEF  BUILD_ROM
        OUTPUT bios.bin
    ENDIF

    MODULE  BIOS

    ORG 0xD600

; -------------------------------------------------------
; BIOS JUMP TABLE
; -------------------------------------------------------
boot_f:
    JP   bios_boot
wboot_f:
    JP   bios_wboot

; -------------------------------------------------------
; console status to reg-a
; -------------------------------------------------------
const_f:
    JP   MON.non_con_status

; -------------------------------------------------------
; console character to reg-a
; -------------------------------------------------------
conin_f:
    JP   MON.mon_con_in

; -------------------------------------------------------
; console character from c to console out
; -------------------------------------------------------
conout_f:
    JP   MON.mon_con_out

; -------------------------------------------------------
; list device out
; -------------------------------------------------------
list_f:
    JP   MON.mon_char_print

; -------------------------------------------------------
; punch device out
; -------------------------------------------------------
punch_f:
    JP   MON.mon_serial_out

; -------------------------------------------------------
; reader character in to reg-a
; -------------------------------------------------------
reader_f:
    JP   MON.mon_serial_in

; -------------------------------------------------------
; move to home position, treat as track 00 seek
; -------------------------------------------------------
home_f:
    JP   home

; -------------------------------------------------------
; select disk given by register c
; -------------------------------------------------------
seldsk_f:
    JP   sel_disk
settrk_f:
    JP   set_trk
setsec_f:
    JP   set_sec

; -------------------------------------------------------
; Set DMA address from BC
; -------------------------------------------------------
setdma_f:
    JP   set_dma
read_f:
    JP   read
write_f:
    JP   write
listst_f:
    JP   list_st
sectran_f:
    JP   sec_tran

; -------------------------------------------------------
; Reserved
; -------------------------------------------------------
    JP   warm_boot
    JP   warm_boot

; -------------------------------------------------------
; Tape read
; -------------------------------------------------------
tape_read_f:
    JP   MON.mon_tape_read

; -------------------------------------------------------
; Tape write
; -------------------------------------------------------
tape_write_f:
    JP   MON.mon_tape_write

; -------------------------------------------------------
; Tape wait block
; -------------------------------------------------------
tape_wait_f:
    JP   MON.mon_tape_wait

    JP   warm_boot                                  ; r8

disk_a_size DW  0x00C0                              ; 192k disk A size
disk_b_size DW  0x02d0                              ; 720  disk B size
disk_c_size DW  0x02d0                              ; 720  disk C size
bios_var04  DB  0x50
bios_var05  DB  0x50

; -------------------------------------------------------
; cold start
; -------------------------------------------------------
bios_boot:
    LD   HL, (bdos_ent_addr)                        ; 0xba06
    LD   DE, 0x10000-CCP_RAM.bdos_enter_jump        ; 0x45fa
    ADD  HL, DE                                     ; 0xba06+0x45fa=10000
    LD   A, H
    OR   L
    JP   Z, bios_signon

    ; >> r8
    LD   HL, (bios_var0)                            ; if bios_var0 = 0xaaaa (initialized)
    LD   DE, 0x5556
    ADD  HL, DE                                     ; 0xaaaa+0x5556=0x10000 if initialized
    LD   A, H
    OR   L
    JP   Z, bios_signon                             ; if initialized, go to logon, skip init
    ; <<

    ; Init DMA buffer
    LD   HL, dma_buffer                             ; 0x8000
    LD   B, DMA_BUFF_SIZE                           ; 0x80
.init_dma_buff:
    LD   (HL), FILE_DELETED                         ; 0xE5
    INC  HL
    DEC  B
    JP   NZ, .init_dma_buff

    ; Init RAM disk
    LD   HL, dma_buffer
    LD   DE, 0x0000
    LD   B, 8

.init_ram_dsk:
    PUSH BC
    CALL MON.ram_disk_write
    POP  BC
    INC  DE
    DEC  B
    JP   NZ, .init_ram_dsk

    ; Init user to 0 and drive to 0
    XOR  A
    LD   (cur_user_drv), A

    ; init bios variables
    CALL  bios_init_ilv                             ; r8

bios_signon:
    LD   SP, bios_stack
    ; Print CP/M hello message
    LD   HL, msg_hello
    CALL print_strz
    JP   bios_wboot

;bios_wboot:
;    LD   SP, bios_stack

; r8 >>
bios_init_ilv:
    LD   HL, bios_var0
    LD   DE, bios_ini_vals
    LD   C, 13
    CALL mov_dlhe_c
    LD   A, (bios_var05)                            ; 0x50
    LD   (bios_var2), A
    RET
; <<

; -------------------------------------------------------
; BIOS Warm start entry
; -------------------------------------------------------
bios_wboot:                                         ; r8

    LD   HL, (bios_var0)
    LD   DE, 0x5556                                 ; 0xaaaa + 0x5556 = 0x10000
    ADD  HL, DE
    LD   A, H
    OR   L
    CALL NZ, bios_init_ilv                          ; init if not initialized before
    LD   SP, bios_stack

    ; Move CPP from 0xC000 to 0xB200
    LD   HL, CCP_DST_ADDR
    LD   DE, CCP_SRC_ADDR
    LD   BC, CCP_SIZE
.move_ccp:
    LD   A, (DE)
    LD   (HL), A
    INC  DE
    INC  HL
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, .move_ccp

    ; Init variables with zeroes
    LD   HL, CPM_VARS.cpm_vars_start
    LD   BC, CPM_VARS.ccp_vars_size                 ; 213

.clr_cpm_vars:
    LD   (HL), 0x0
    INC  HL
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, .clr_cpm_vars

    LD   A, FILE_DELETED
    LD   (CPM_VARS.bdos_efcb), A                    ; mark empty FCB
    LD   A, low dma_buffer                          ; 0x80
    LD   (CPM_VARS.bdos_dmaad), A                   ; 0x0080

    ; Move DPH + DPB to RAM
    LD   HL, CPM_VARS.DPH_RAM
    LD   DE, dph_disk_a
    LD   BC, DPB_END-dph_disk_a                     ; 0x39 -> 57d

.move_dph:
    LD   A, (DE)
    LD   (HL), A
    INC  HL
    INC  DE
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, .move_dph

    LD   BC, dma_buffer                          ; DMA default buffer addr
    CALL setdma_f

    ; Setup JP to Warm boot after CPU reset
    LD   A, JP_OPCODE
    LD   (warm_boot), A
    LD   HL, wboot_f
    LD   (warm_boot_addr), HL

    ; Setup JP to BDOS entrance
    LD   (jp_bdos_enter), A
    LD   HL, CCP_RAM.bdos_enter_jump
    LD   (bdos_ent_addr), HL

    LD   HL, CPM_VARS.DPB_A_RAM
    LD   C, 0xf
    LD   DE, dpb_ram
    LD   A, (disk_a_size+1)
    OR   A
    JP   Z, set_curr_dpb
    LD   DE, dpb_empty

set_curr_dpb:
    CALL mov_dlhe_c
    LD   HL, CPM_VARS.DPB_B_RAM
    LD   C, 0xf
    LD   DE, dpb_flop_360k
    LD   A, (disk_b_size+1)
    CP   0x1
    JP   Z, .l1
    LD   DE, dpb_flop_720k
.l1
    CALL mov_dlhe_c
    LD   HL, CPM_VARS.DPB_C_RAM
    LD   C, 0xf
    LD   A, (disk_c_size+1)
    CP   0x2
    JP   Z, .l2
    LD   DE, dpb_flop_360k
    LD   A, (bios_var3)
    OR   A
    JP   NZ, .l3
    LD   DE, dpb_flop_720k
    JP   .l3
.l2
    LD   DE, dpb_flop_720k
    LD   A, (bios_var3)
    OR   A
    JP   NZ, .l3
    LD   DE, dpb_flop_360k
.l3
    CALL  mov_dlhe_c
    XOR   A
    LD    (CPM_VARS.slicer_has_data),A
    LD    (CPM_VARS.slicer_uninited_count),A
    LD    A,(cur_user_drv)
    LD    C,A
    ; go to CCP
    JP    CCP_DST_ADDR

; -------------------------------------------------------
; Move C bytes from [DE] to [HL]
; -------------------------------------------------------
mov_dlhe_c:
    LD   A,(DE)                                     ; [DE]->[HL]
    LD   (HL),A
    INC  HL
    INC  DE
    DEC  C
    JP   NZ, mov_dlhe_c
    RET

list_st:
    XOR  A
    RET

; -------------------------------------------------------
; Select disk
; Inp: C - disk,
;      E - active drive flag
; Out: HL -> DPH
; -------------------------------------------------------
sel_disk:
    LD   HL, 0x00
    LD   A, C
    CP   CTRL_C
    RET  NC

    LD   (CPM_VARS.cur_disk), A
    OR   A
    JP   Z, .get_dph_addr                           ; skip next for disk 0 - RAM disk
    LD   A, E                                       ; selected disk map
    AND  0x1                                        ; bit 0 is set if disk already selected
    JP   NZ, .get_dph_addr
    ; Reset disk
    LD  (CPM_VARS.slicer_has_data), A
    LD  (CPM_VARS.slicer_uninited_count), A
    ; calc DPH address of new drive
.get_dph_addr:
    LD  L, C
    LD  H, 0x0
    ADD HL, HL                                      ; *2
    ADD HL, HL                                      ; *4
    ADD HL, HL                                      ; *8
    ADD HL, HL                                      ; *16 (size of DBH)
    LD  DE, CPM_VARS.DPH_RAM
    ADD HL, DE
    RET

; -------------------------------------------------------
; move to track 00
; -------------------------------------------------------
home:
    LD   A, (CPM_VARS.cur_disk)
    OR   A
    JP   Z, .is_default
    LD   A, (CPM_VARS.slicer_need_save)
    OR   A
    JP   NZ, .is_default
    LD   (CPM_VARS.slicer_has_data), A              ; set to 0, no data

.is_default:
    LD   C, 0                                       ; set track to 0

; -------------------------------------------------------
; set track address (0..76) for subsequent read/write
; -------------------------------------------------------
set_trk:
    LD   HL, CPM_VARS.curr_track
    LD   (HL), C
    RET

; -------------------------------------------------------
; set sector address (1,..., 26) for subsequent read/write
; -------------------------------------------------------
set_sec:
    LD   HL, CPM_VARS.curr_sec
    LD   (HL), C
    RET

; -------------------------------------------------------
; set subsequent dma address (initially 80h)
; -------------------------------------------------------
set_dma:
    LD   L, C
    LD   H, B
    LD   (CPM_VARS.dma_addr), HL
    RET

sec_tran:
    LD   L, C
    LD   H, B
    RET

; -------------------------------------------------------
; read track/sector to preset dma address
; -------------------------------------------------------
read:
    LD   A, (CPM_VARS.cur_disk)
    OR   A
    JP   NZ, read_phys                               ; for physical disk use special routine
    CALL ram_disk_calc_addr
    CALL MON.ram_disk_read
    XOR  A
    RET

; -------------------------------------------------------
; write track/sector from preset dma address
; -------------------------------------------------------
write:
    LD   A, (CPM_VARS.cur_disk)
    OR   A
    JP   NZ,write_phys
    CALL ram_disk_calc_addr
    CALL MON.ram_disk_write
    XOR  A
    RET

; -------------------------------------------------------
; Calculate address for current sector and track
; -------------------------------------------------------
ram_disk_calc_addr:
    LD   HL, CPM_VARS.curr_track
    ; HL = cur_track * 16
    LD   L, (HL)
    LD   H, 0x0
    ADD  HL, HL
    ADD  HL, HL
    ADD  HL, HL
    ADD  HL, HL
    ; DE = HL + cur_sec
    EX   DE, HL
    LD   HL, CPM_VARS.curr_sec
    LD   L, (HL)
    LD   H, 0x0
    ADD  HL, DE
    EX   DE, HL
    ; store address
    LD   HL, (CPM_VARS.dma_addr)
    RET

read_phys:
    CALL read_phys_op
    RET

write_phys:
    CALL write_phys_op
    RET

read_phys_op:
    XOR A
    ; reset counter
    LD   (CPM_VARS.slicer_uninited_count), A
    LD   A, 0x1
    LD   (CPM_VARS.tmp_slicer_operation), A         ; 0 - write; 1 - read
    LD   (CPM_VARS.tmp_slicer_can_read), A          ; enable read fron disk
    LD   A, 0x2
    LD   (CPM_VARS.tmp_slicer_flush), A             ; disable flush data to disk
    JP   base_read_write

write_phys_op:
    XOR  A
    LD   (CPM_VARS.tmp_slicer_operation), A
    LD   A, C
    LD   (CPM_VARS.tmp_slicer_flush), A
    CP   0x2
    JP   NZ, .mode_ne_2
    LD   A, 0x10                                    ; 2048/128
    LD   (CPM_VARS.slicer_uninited_count), A
    LD   A, (CPM_VARS.cur_disk)
    LD   (CPM_VARS.slicer_uninited_disk), A
    LD   A, (CPM_VARS.curr_track)
    LD   (CPM_VARS.slicer_uninited_track), A
    LD   A, (CPM_VARS.curr_sec)
    LD   (CPM_VARS.slicer_uninited_sector_128), A
.mode_ne_2:
    LD   A, (CPM_VARS.slicer_uninited_count)
    OR   A
    JP   Z, slicer_read_write
    DEC  A
    LD   (CPM_VARS.slicer_uninited_count), A
    LD   A, (CPM_VARS.cur_disk)
    LD   HL, CPM_VARS.slicer_uninited_disk
    CP   (HL)
    JP   NZ, slicer_read_write
    LD   A, (CPM_VARS.curr_track)
    LD   HL, CPM_VARS.slicer_uninited_track
    CP   (HL)
    JP   NZ, slicer_read_write
    LD   A, (CPM_VARS.curr_sec)
    LD   HL, CPM_VARS.slicer_uninited_sector_128
    CP   (HL)
    JP   NZ, slicer_read_write
    INC  (HL)
    LD   A, (HL)
    CP   36                                         ; Sectors per track
    JP   C, .no_inc_track
    LD   (HL), 0x0
    LD   A, (CPM_VARS.slicer_uninited_track)
    INC  A
    LD   (CPM_VARS.slicer_uninited_track), A

.no_inc_track:
    XOR  A
    LD   (CPM_VARS.tmp_slicer_can_read), A
    JP   base_read_write

; --------------------------------------------------
slicer_read_write:
    XOR  A
    LD   (CPM_VARS.slicer_uninited_count), A
    INC  A
    LD   (CPM_VARS.tmp_slicer_can_read), A

; --------------------------------------------------
base_read_write:
    XOR  A
    LD   (CPM_VARS.tmp_slicer_result), A
    LD   A, (CPM_VARS.curr_sec)
    OR   A
    RRA
    OR   A
    RRA
    LD   (CPM_VARS.tmp_slicer_real_sector), A
    LD   HL, CPM_VARS.slicer_has_data
    LD   A, (HL)
    LD   (HL), 0x1
    OR   A
    JP   Z, .no_data
    LD   A, (CPM_VARS.cur_disk)
    LD   HL, CPM_VARS.slicer_disk
    CP   (HL)
    JP   NZ, .pos_diff
    LD   A, (CPM_VARS.curr_track)
    LD   HL, CPM_VARS.slicer_track
    CP   (HL)
    JP   NZ, .pos_diff
    LD   A, (CPM_VARS.tmp_slicer_real_sector)
    LD   HL, CPM_VARS.slicer_real_sector
    CP   (HL)
    JP   Z,calc_sec_addr_in_bfr
.pos_diff:
    LD   A, (CPM_VARS.slicer_need_save)
    OR   A
    CALL    NZ, slicer_save_buffer                  ; save buffer if needed
.no_data:
    LD   A, (CPM_VARS.cur_disk)
    LD   (CPM_VARS.slicer_disk), A
    LD   A, (CPM_VARS.curr_track)
    LD   (CPM_VARS.slicer_track), A
    LD   A, (CPM_VARS.tmp_slicer_real_sector)
    LD   (CPM_VARS.slicer_real_sector), A
    LD   A, (CPM_VARS.tmp_slicer_can_read)
    OR   A
    CALL NZ,slicer_read_buffer
    XOR  A
    LD   (CPM_VARS.slicer_need_save), A

; --------------------------------------------------
calc_sec_addr_in_bfr:
    LD   A, (CPM_VARS.curr_sec)
    AND  0x3
    LD   L, A
    LD   H, 0x0
    ADD  HL, HL
    ADD  HL, HL
    ADD  HL, HL
    ADD  HL, HL
    ADD  HL, HL
    ADD  HL, HL
    ADD  HL, HL                                     ; *128
    LD   DE, CPM_VARS.slicer_buffer
    ADD  HL, DE
    EX   DE, HL
    LD   HL, (CPM_VARS.dma_addr)
    LD   C, 0x80
    LD   A, (CPM_VARS.tmp_slicer_operation)
    OR   A
    JP   NZ, .no_save
    LD   A, 0x1
    LD   (CPM_VARS.slicer_need_save), A
    EX   DE, HL
.no_save:
    LD   A, (DE)
    INC  DE
    LD   (HL), A
    INC  HL
    DEC  C
    JP   NZ, .no_save
    LD   A, (CPM_VARS.tmp_slicer_flush)
    CP   0x1
    LD   A, (CPM_VARS.tmp_slicer_result)
    RET  NZ
    OR   A
    RET  NZ
    XOR  A
    LD   (CPM_VARS.slicer_need_save), A
    CALL slicer_save_buffer
    LD   A, (CPM_VARS.tmp_slicer_result)
    RET

; --------------------------------------------------
slicer_save_buffer:
    CALL slicer_get_floppy_args
    LD   C, 0xA6                                    ; VG93 CMD
    CALL MON.write_floppy
    LD   (CPM_VARS.tmp_slicer_result), A
    RET

; --------------------------------------------------
slicer_read_buffer:
    CALL slicer_get_floppy_args
    LD   C, 0x86                                    ; VG93 CMD
    CALL MON.read_floppy
    LD   (CPM_VARS.tmp_slicer_result), A
    RET

; --------------------------------------------------
slicer_get_floppy_args:
    LD   HL, CPM_VARS.tmp_buff9
    LD   A, (CPM_VARS.slicer_disk)
    DEC  A
    JP   Z, .non_interleave
    LD   HL, interleave_0
.non_interleave
    LD   A, (CPM_VARS.slicer_real_sector)
    ADD  A, L
    LD   L, A
    LD   E, (HL)
    LD   A, (CPM_VARS.slicer_track)
    LD   D, A
    LD   HL, CPM_VARS.slicer_buffer
    LD   A, (CPM_VARS.slicer_disk)
    RET


; -------------------------------------------------------
; Print zerro ended string
; Inp: HL -> string
; -------------------------------------------------------
print_strz:
    LD   A, (HL)
    OR   A
    RET  Z
    LD   C, A
    PUSH HL
    CALL conout_f
    POP  HL
    INC  HL
    JP   print_strz

msg_hello:
    DB   ASCII_ESC, "60"                            ; Режим 32x18 <ESC>60
    DB   ASCII_ESC, "83"
    DB   ASCII_ESC, "5!%"
    DB   ASCII_ESC, "41"
    DB   ASCII_ESC, "1", 0x16, 0xe2, 0xe2, 0xfc, 0x01
    DB   ASCII_ESC, "40"
    DB   ASCII_ESC, "1", 0x1e, 0xe6, 0xdb, 0xf8, 0x01
    DB   ASCII_ESC, "43"
    DB  "OKEAH-240 CP/M (V2.2) REL.8'\r\n\n"
    DB   ASCII_ESC, "42", 0


; --------------------------------------------------
; Disk parameters blocks in ROM (DPBs)
; Tables of memory that describe the characteristics
; of discs on our system. There is one DPB for each
; disc type

; ----------------------------------------
; Block size | No of sectors | BSH | BLM |
; ----------------------------------------
;     1K     |  8            |  3  | 7   |
;     2K     |  16           |  4  | 15  |
;     4K     |  32           |  5  | 31  |
;     8K     |  64           |  6  | 63  |
;     16K    |  128          |  7  | 127 |
; ----------------------------------------

; -------------------------------------
; Block size|      Extent mask  (EXM) |
;           | Small disk | Large disk |
; -------------------------------------
;     2K    |      1     |     0      |
;     4K    |      3     |     1      |
;     8K    |      7     |     3      |
;    16K    |     15     |     7      |
; -------------------------------------
; CKS - number of dir sectors to check before write, 0 for HDD

; For RAM-Disk 128k
dpb_ram:
    DW  0010h                                       ; SPT Sector (128b) per track (16d)
    DB  03h                                         ; BSH 1k
    DB  07h                                         ; BLM 1k;  Allocation block size = (BLM + 1) * 128 = 1k
    DB  00h                                         ; EXM extent mask
    DW  191                                         ; DSM Disk size blocks - 1
    DW  31                                          ; DRM Dir elements - 1
    DB  10000000b                                   ; AL0 Dir map byte 1
    DB  00000000b                                   ; AL1 Dir map byte 2
    DW  0008h                                       ; CKS checksum vector size (8 sectors=1k)
    DW  0000h                                       ; OFF (tracks reserved for system)

dpb_empty:
    DS  15, 0xff

; For FLOPPY 720k
dpb_flop_720k:
    DW  0024h                                       ; SPT Sector (128b) per track 36 * 128 = 18KB
    DB  04h                                         ; BSH 2k
    DB  0Fh                                         ; BLM 2k;  Allocation block size = (BLM + 1) * 128 = 2k
    DB  00h                                         ; EXM extent mask
    DW  359                                         ; DSM Disk size blocks - 1 (359d)
    DW  127                                         ; DRM Directory entries - 1 (127d)
    DB  11000000b                                   ; AL0 Dir map byte 1 (2 dir blk)
    DB  00000000b                                   ; AL1 Dir map byte 2
    DW  32                                          ; CKS checksum vector size (32 sectors = 2k)
    DW  0000h                                       ; OFF (No of tracks reserved for system)

; For FLOPPY 360k
dpb_flop_360k:
    DW  0024h                                       ; SPT Sector (128b) per track 36 * 128 = 18KB
    DB  04h                                         ; BSH 2k
    DB  0Fh                                         ; BLM 2k;  Allocation block size = (BLM + 1) * 128 = 2k
    DB  01h                                         ; EXM extent mask
    DW  179                                         ; DSM Disk size blocks - 1 (179d)
    DW  127                                         ; DRM Directory entries - 1 (127d)
    DB  11000000b                                   ; AL0 Dir map byte 1 (2 dir blk)
    DB  00000000b                                   ; AL1 Dir map byte 2
    DW  32                                          ; CKS checksum vector size (32 sectors = 2k)
    DW  0000h                                       ; OFF (No of tracks reserved for system)

bios_ini_vals:
    DB  0xaa, 0xaa, 0, 0xff, 1, 8, 6, 4, 2, 9, 7, 5, 3

; --------------------------------------------------
; Disk parameters headers in ROM
; --------------------------------------------------
; Disk A RAM
dph_disk_a:
    DW  00h                                         ; Sector translate table pointer
    DW  00h, 00h, 00h                               ; Scratchpad area
    ; 0xda00
    DW  CPM_VARS.dir_buffer                         ; Directory buffer pointer
    DW  CPM_VARS.DPB_A_RAM                          ; DPB Pointer
    DW  CPM_VARS.CHK_VEC_A                          ; Check Vector pointer
    DW  CPM_VARS.AL_MAP_A                           ; Allocation map pointer

; Disk B Floppy
dph_disk_b:
    DW  00h                                         ; Sector translate table pointer
    DW  00h, 00h, 00h                               ; Scratchpad area
    DW  CPM_VARS.dir_buffer                         ; Directory buffer pointer
    DW  CPM_VARS.DPB_B_RAM                          ; DPB Pointer
    DW  CPM_VARS.CHK_VEC_B                          ; Check Vector pointer
    DW  CPM_VARS.AL_MAP_B                           ; Allocation map pointer

; Disk C Floppy
dph_disk_c:
    DW  00h                                         ; Sector translate table pointer
    DW  00h, 00h, 00h                               ; Scratchpad area
    DW  CPM_VARS.dir_buffer                         ; Directory buffer pointer
    DW  CPM_VARS.DPB_C_RAM                          ; DPB Pointer
    DW  CPM_VARS.CHK_VEC_C                          ; Check Vector pointer
    DW  CPM_VARS.AL_MAP_C                           ; Allocation map pointer



res_data:                                           ; 0xda28
    DB  1,8,6,4,2,9,7,5,3
DPB_END     EQU $
    DB  0x0e, 3


; -------------------------------------------------------
; Filler to align blocks in ROM
; -------------------------------------------------------
LAST        EQU     $
CODE_SIZE   EQU     LAST-0xD600
FILL_SIZE   EQU     0x500-CODE_SIZE

    DISPLAY "| BIOS\t| ",/H,boot_f,"  | ",/H,CODE_SIZE," | ",/H,FILL_SIZE," |"

    ; Check integrity
    ASSERT bios_wboot = 0xd6a8
    ASSERT set_curr_dpb = 0xd722
    ASSERT sel_disk = 0xd781
    ASSERT home = 0xd7a7
    ASSERT ram_disk_calc_addr = 0xd7eb
    ASSERT write_phys_op = 0xd81e
    ASSERT base_read_write = 0xd88a
    ASSERT calc_sec_addr_in_bfr = 0xd8e4
    ASSERT slicer_save_buffer = 0xd928
    ASSERT print_strz = 0xd95e
    ASSERT msg_hello = 0xd96b
    ASSERT dpb_ram = 0xd9af
    ASSERT dph_disk_a = 0xd9f8
    ASSERT res_data = 0xda28

FILLER:
    DS  FILL_SIZE, 0xff

    ENDMODULE

    IFNDEF  BUILD_ROM
        OUTEND
    ENDIF
