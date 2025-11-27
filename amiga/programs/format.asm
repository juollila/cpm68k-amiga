; Amiga CP/M-68k Format Program
;
; Copyright (c) 2025 Juha Ollila
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
;

SECTORS_PER_TRACK   = 44    ; One track contains 44 CP/M logical sectors
SECTOR_SIZE         = 128   ; Logical sector size
BOOT_TRACKS         = 8     ; Number of boot tracks.
                            ; Boot tracks contain boot loader, CCP, BDOS and BIOS 
ALL_TRACKS          = 160   ; Number of all tracks

BIOS_SELECT_DRIVE   = 9
BIOS_SET_TRACK      = $a
BIOS_SET_SECTOR     = $b
BIOS_SET_DMA_ADDR   = $c
BIOS_READ_SECTOR    = $d
BIOS_WRITE_SECTOR   = $e

BDOS_SYSTEM_RESET   = 0
BDOS_CONSOLE_INPUT  = 1
BDOS_CONSOLE_OUTPUT = 2
BDOS_PRINT_STRING   = 9
BDOS_SELECT_DISK    = $e
BDOS_CURRENT_DISK   = $19


    org     $500-$1c            ; TEXT_START - (HEADER_END-HEADER_START)

HEADER_START:
    dc.w    $601a               ; text, data, and bss are contiguous
    dc.l    TEXT_END-TEXT_START ; number of bytes in text segment
    dc.l    DATA_END-DATA_START ; number of bytes in data segment
    dc.l    0                   ; number of bytes in bss segment
    dc.l    0                   ; number of bytes in symbol table
    dc.l    0                   ; reserved
    dc.l    TEXT_START          ; beginning of text segment
    dc.w    $ffff               ; no relocation bits
HEADER_END:

TEXT_START:
    move.l  #title_str,d1
    jsr     print_string

    jsr     read_boot_tracks
    jsr     get_current_drive
    move.w  d0,d4               ; save current drive to d4

; ask drive
.ask_drive:
    move.l  #select_str,d1
    jsr     print_string
    jsr     get_char
    move.w  d0,d3               ; save drive letter to d3  

; insert disk to be formatted
    move.l  #insert_str,d1
    jsr     print_string
    move.w  d3,d1               ; print drive letter
    jsr     put_char

; ask confirmation
    move.l  #confirm_str,d1
    jsr print_string
    jsr     get_char
    cmp.b   #'Y',d0
    bne     .ask_another

; select disk drive
    move.w  d3,d1
    sub.w   #'A',d1
    jsr     set_current_drive
    cmp.l   #0,d0
    beq     .no_drive

; format disk
    jsr     format_disk

; write boot tracks
    jsr     write_boot_tracks

; done
    move.l  #done_str,d1
    jsr     print_string

; ask if another disk should be formatted
.ask_another:
    move.l  #another_str,d1
    jsr     print_string
    jsr     get_char
    cmp.b   #'Y',d0
    beq     .ask_drive

    move.w  d4,d1           ; restore current drive
    jsr     set_current_drive  

    jsr     print_cr
    rts

.no_drive:
    move.l  #no_drive_str,d1
    jsr     print_string
    jmp     exit


; print string calling bdos
; d1 = address to string
print_string:
    move.w  #BDOS_PRINT_STRING,d0
    trap    #2
    rts

; print cr lf
print_cr:
    move.l  #cr_lf_str,d1
    jsr     print_string
    rts

; get character using BDOS direct console I/O
; and converts it to upper case
;
; returns:
; d0: upper case char
get_char:
    move.w  #BDOS_CONSOLE_INPUT,d0
    trap    #2
    and.w   #%11011111,d0           ; change to upper case
    rts

; put character using BDOS direct console I/O
; d1 = charater to be printed
put_char:
    move.w  #BDOS_CONSOLE_OUTPUT,d0
    trap    #2
    rts

; get_current_drive
;
; returns:
; d0 = current drive
get_current_drive:
    move.w  #BDOS_CURRENT_DISK,d0
    trap    #2
    rts

; select disk drive
; d1 = drive number (0..15)
set_current_drive:
    move.w  #BIOS_SELECT_DRIVE,d0
    trap    #3
    rts


read_boot_tracks:
    jsr     init_dma_address
.read_sector:
    jsr     set_track_sector_and_dma
    move.w  #BIOS_READ_SECTOR,d0
    trap    #3
    cmp.w   #0,d0
    bne     .error
    addq.w  #1,d4
    cmp.w   #SECTORS_PER_TRACK,d4
    bne     .continue
    move.w  #0,d4
    addq.w  #1,d3
    cmp.w   #BOOT_TRACKS,d3
    beq     .done
.continue:
    add.l   #SECTOR_SIZE,a3
    bra     .read_sector
.done:
    rts
.error:
    move.l  #cannot_read_str,d1
    jsr     print_string
    jmp     exit

write_boot_tracks:
    move.l  #installing_str,d1
    jsr     print_string
    jsr     init_dma_address
.write_sector:
    jsr     set_track_sector_and_dma
    move.w  #BIOS_WRITE_SECTOR,d0
    trap    #3
    cmp.w   #0,d0
    bne     .error
    addq.w  #1,d4
    cmp.w   #SECTORS_PER_TRACK,d4
    bne     .continue
    move.w  #'.',d1
    jsr     put_char
    move.w  #0,d4
    addq.w  #1,d3
    cmp.w   #BOOT_TRACKS,d3
    beq     .done
.continue:
    add.l   #SECTOR_SIZE,a3
    bra     .write_sector
.done:
    rts
.error:
    move.l  #cannot_write_str,d1
    jsr     print_string
    jmp     exit

; set track, sector and dma address to initial values
init_dma_address:
    move.w  #0,d3               ; d3 = track number
    move.w  #0,d4               ; d4 = logical sector number
    lea     boot_track_data,a3  ; a3 = dma address
    rts

; set track number, locical sector number and dma address
; d3 = track number
; d4 = logical sector number
; a3 = dma address
set_track_sector_and_dma:
    move.w  #BIOS_SET_TRACK,d0
    move.w  d3,d1
    trap    #3
    move.w  #BIOS_SET_SECTOR,d0
    move.w  d4,d1
    trap    #3
    move.w  #BIOS_SET_DMA_ADDR,d0
    move.l  a3,d1
    trap    #3
    rts

; init sector data
; d0.l = value to be filled
init_sector_data:
    move.w	#((SECTOR_SIZE/4)-1),d1
    lea     sector_data,a0
.loop:
    move.l  d0,(a0)+
    dbf     d1,.loop
    rts

format_disk:
    move.l  #formatting_str,d1
    jsr     print_string
    move.w  #0,d3               ; d3 = track number
    move.w  #0,d4               ; d4 = logical sector number
    bra     .init_sector_data   ; start fill tracks with 0
.write_sector:
    lea     sector_data,a3    ; a3 = dma address
    jsr     set_track_sector_and_dma
    move.w  #BIOS_WRITE_SECTOR,d0
    trap    #3
    cmp.w   #0,d0
    bne     .error
    addq.w  #1,d4
    cmp.w   #SECTORS_PER_TRACK,d4
    bne     .write_sector
    move.w  #'.',d1
    jsr     put_char
    move.w  #0,d4
    addq.w  #1,d3
    cmp.w   #ALL_TRACKS,d3
    beq     .done
    cmp.w   #8,d3               ; dir entries are in track 8
    beq     .init_dir_data
    cmp.w   #9,d3               ; rest of tracks are filled with 0
    beq     .init_sector_data
    cmp.w   #67,d3              ; print cr lf after 67 tracks
    beq     .print_cr
    cmp.w   #146,d3             ; print cr lf after 146 tracks
    beq     .print_cr
    bra     .write_sector
.init_dir_data:
    move.l  #$e5e5e5e5,d0
    jsr     init_sector_data
    bra     .write_sector
.init_sector_data:
    move.l  #0,d0
    jsr     init_sector_data
    bra     .write_sector
.print_cr
    jsr     print_cr
    bra     .write_sector
.done:
    rts
.error:
    move.l  #cannot_write_str,d1
    jsr     print_string
    jmp     exit
    
exit:
    move.w  #0,d0
    trap    #2


TEXT_END:

DATA_START:
title_str:
    dc.b    13,10,"Amiga CP/M-68k Format Program, Nov 26 2025","$"
select_str:
    dc.b    13,10,13,10,"Please select disk drive (A or B): $"
insert_str:
    dc.b    13,10,13,10,"Insert disk TO BE FORMATTED in drive $"
confirm_str:
    dc.b    13,10,13,10,"Type Y when ready, any other key to abort: $"
cr_lf_str:
    dc.b    13,10,"$"
formatting_str:
    dc.b    13,10,13,10,"Formatting: $"
installing_str:
    dc.b    13,10,13,10,"Installing CP/M to disk: $"
done_str:
    dc.b    13,10,13,10,"Disk was formatted and CP/M was installed.",13,10,"$"
another_str:
    dc.b    13,10,"Do you want to format another disk? $"
cannot_read_str:
    dc.b    13,10,13,10,"Failed to read from disk",13,10,"$"
cannot_write_str:
    dc.b    13,10,13,10,"Failed to write to disk",13,10,"$"
no_drive_str:
    dc.b    13,10,13,10,"No such drive",13,10,"$"
    even
DATA_END:

sector_data:
boot_track_data = sector_data+SECTOR_SIZE

END:
    dcb.b   SECTOR_SIZE-((END-HEADER_START)%SECTOR_SIZE)    ; padding
