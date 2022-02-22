; CP/M-68k BIOS for Amiga
;
; Copyright (c) 2021 Juha Ollila
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

CUSTOM		= $dff000	; Start of custom chips
CPMSTART	= $60000	; Start of CP/M
CCPSTART	= $600bc	; Start of CCP
CPMSTRING	= $64f50	; CP/M copyright string
BIOSSTART	= $66000	; Start of BIOS

; Custom chip register offsets
DMACONR = $002	; DMA control read
INTREQR = $01e  ; Interrupt request bits read
DSKPTH	= $020	; Disk track buffer pointer (high)
DSKLEN	= $024	; Disk track buffer length
BLTCON0 = $040	; Blitter control register 0
BLTCON1 = $042	; Blitter control register 1
BLTAFWM	= $044	; Blitter first word mask for source A
BLTALWM	= $046	; Blitter last word mask for source A
BLTAPTH	= $050	; Blitter pointer to source A (high)
BLTDPTH	= $054	; Blitter pointer to destination D (high)
BLTSIZE	= $058	; Blit size (starts blit)
BLTAMOD	= $064	; Blitter modulo for source A
BLTDMOD	= $066	; Blitter modulo for destination D
DSKSYNC	= $07e	; Disk sync pattern
COP1LCH	= $080	; Copper pointer 1 (high)
COPJMP1	= $088	; Trigger copper 1 (any value)
DIWSTRT	= $08e	; Display window start
DIWSTOP = $090	; Display window stop
DDFSTRT	= $092	; Display bitplane data fetch start
DDFSTOP	= $094	; Display bitplane data fetch stop
DMACON	= $096	; DMA control write
INTENA	= $09a	; Interrupt enable bits
INTREQ	= $09c	; Interrupt request bits
ADKCON	= $09e	; Audio, disk and UART control
BPL1PTH	= $0e0	; Bitplane pointer 1 (high)
BPLCON0	= $100	; Bitplane depth and screen mode
BPLCON1	= $102	; Bitplane/playfield horizontal scroll values
BPL1MOD	= $108	; Bitplane modulo (odd planes)
COLOR0	= $180	; Color 0
COLOR1	= $182	; Color 1

; Keyboard constants
LEFTSHIFT	= $60
RIGHTSHIFT	= $61
CAPSLOCK	= $62
CONTROL		= $63

; Screen constants
COLS		= $50
ROWS		= $20
WIDTH		= 640
HEIGHT		= 256

; Floppy constants
CYLINDERS	= 80
HEADS		= 2
TRACKS		= CYLINDERS*HEADS
SECTORSPERTRACK	= 11
SECTORSIZE	= 512
TRACKSIZE	= SECTORSPERTRACK*SECTORSIZE
SECTORS		= TRACKS*SECTORSPERTRACK
RESERVEDTRACKS	= 8	; boot loader + 24k (bdos + ccp) + 8k (bios)
BLOCKSIZE	= 2048	; CPM FS block size
DIRENTRIES	= 128	; CPM directory entries
DRIVES		= 2	; Only 2 floppy drives supported (Amiga can support upto 4)
MFM_TRACKSIZE	= 13630 ; MFM track size
GAPSIZE		= 1656  ; 13630 - (11*(64+512+512)) - 4 = 1658, 1656 is divisible by 4

; Offsets in floppy drive structure
FD_TRACK	= 0
FD_SIDE		= 2
FD_SECTOR	= 4

; CIA base addresses
CIAA	= $bfe001
CIAB	= $bfd000

; CIA register offsets
PRA	= $000	; Data register A
PRB	= $100	; Data register B
DDRA	= $200	; Data direction register A
DDRB	= $300	; Data direction register B
TALO	= $400	; Timer A low value
TAHI	= $500	; Timer A high value
TBLO	= $600	; Timer B low value
TBHI	= $700	; Timer B high value
TODLOW	= $800	; Time Of Day (low)
TODMID	= $900	; Time Of Day (mid)
TODHI	= $a00	; Time Of Day (high)
NOTUSED	= $b00
SDR	= $c00	; Serial data register
ICR	= $d00	; Interrupt control register
CRA	= $e00	; Control register A
CRB	= $f00	; Control register B

;	code_c
	org	BIOSSTART

;
; Function 0: Initialization
;
; Entry parameters:
;	d0.w: $00
; Return value: d0.w: user/disk numbers
;
_init:	
	lea	CIAA,a0
	lea	CIAB,a1
	lea	CUSTOM,a6
	bsr	takeover
	bsr	initscreen
	bsr	inittimers
	bsr	initkeyboard
	bsr	initfloppy
	bsr	inittrap

	; clear screen
	move.w	#$1a,d1
	bsr	conout
	; print bios string
	lea	bios_str(pc),a0
	bsr	printstring
	; print cp/m string
	; we do not utilize cp/m loader thus cp/m string should be printed separately 
	lea	CPMSTRING,a0
	bsr	printstring
	bsr	printcr
	move.w	#0,d0				; select drive A: (0)
	move.w	d0,fd_drive
	bsr	fd_select
	bsr	fd_sync				; synchronize drive
	bsr	fd_deselect			; deselect drive
	move	#$2000,sr			; probably not needed
	clr.l	d0				; log on disk A, user 0
	rts					; return to BDOS

takeover:
	; take over system
	move.w	#$7fff,INTENA(a6)	; disable interrupts
	move.w	#$7fff,INTREQ(a6)	; clear interrupts
	move.w	#$7fff,DMACON(a6)	; disable dma (dmacon)
	rts

initscreen:
	; setup screen
	move.w	#$9200,BPLCON0(a6)	; hires, one bit plane, color enabled
	move.w	#0,BPLCON1(a6)		; horizontal scroll value = 0
	move.w	#0,BPL1MOD(a6)		; set modulo 0 for all odd bit planes
	move.w	#$3c,DDFSTRT(a6)	; data fetch start for hires
	move.w	#$d4,DDFSTOP(a6)	; data fetch stop
	move.w	#$2c81,DIWSTRT(a6)	; display window start
	move.w	#$2cc1,DIWSTOP(a6)	; display window stop
	move.w	#$ccc,COLOR0(a6)	; color0: light gray
	move.w	#$000,COLOR1(a6)	; color1: black
	; init copper list
	move.l	#screen,d0		; copy screen address to copper list
	move.w	d0,copper+6
	swap	d0
	move.w	d0,copper+2
	move.l	#copper,COP1LCH(a6)	; set copper1 list address
	clr.w	COPJMP1(a6)		; jump to copper1 list
	; enable bit-plane, blitter and copper dma
	move.w	#$83c0,DMACON(a6)
	rts

inittimers:
	moveq	#0,d0
	move.b	d0,CRA(a0)		; disable timer A in CIAA
	move.b	d0,CRB(a0)		; disable timer B in CIAA
	move.b	d0,CRA(a1)		; disable timer A in CIAB
	move.b	d0,CRB(a1)		; disable timer B in CIAB
	move.b	#$7f,d0
	move.b	d0,ICR(a0)		; disable all interrupts in CIAA
	move.b	d0,ICR(a1)		; disable all interrupts in CIAB
	rts

initkeyboard:
	move.b	#$88,ICR(a0)		; enable serial port interrupt in CIAA
	move.l	#keyboard_int,$68.w	; keyboard interrupt vector to level 2 autovector
	move.w	#$c008,INTENA(a6)	; enable CIAA interrupts
	rts	

initfloppy:
	move.b	#$03,DDRA(a0)		; audio filter & rom overlay are outputs (CIAA)
					; joy0 fire, joy1 fire, drive ready, track 0, write prot & disk change are inputs
	move.b	#$ff,DDRB(a1)		; motor, sel3, sel2, sel1, sel0, side, dir and step are outputs (CIAB)
	move.b	#$ff,PRB(a1)		; disable all
	move.w	#$6200,ADKCON(a6)	; disable precomp1, precomp0 and msbsync
	move.w	#$9500,ADKCON(a6)	; enable mfmprec, wordsync and fast
	move.w	#$4489,DSKSYNC(a6)	; set mfm sync mark
	move.w	#$8010,DMACON(a6)	; enable disk DMA
	rts

inittrap:
	move.l	#traphandler,$8c	; set up trap #3 handler
	rts

traphandler:
	cmp	#NUMBER_OF_FUNCTIONS,d0
	bcc	.skip
	lsl	#2,d0			; multiply by 4
	lea	biosbase,a0
	move.l	(a0,d0.w),a0
	jsr	(a0)
.skip:
	rte

NUMBER_OF_FUNCTIONS	= 23

biosbase:
	dc.l	_init
	dc.l	warmboot
	dc.l	constatus
	dc.l	conin
	dc.l	conout
	dc.l	notimplemented ;listchar
	dc.l	notimplemented ;auxout
	dc.l	notimplemented ;auxin
	dc.l	home
	dc.l	setdrive
	dc.l	settrack
	dc.l	setsector
	dc.l	setdma
	dc.l	readsector
	dc.l	writesector
	dc.l	notimplemented ;listst
	dc.l	sectortranslate
	dc.l	notimplemented ;
	dc.l	getaddresstable
	dc.l	notimplemented ;segiob
	dc.l	notimplemented ;setiob
	dc.l	flush
	dc.l	setexception

waitblit:
	tst.w	CUSTOM+DMACONR
.waitblit0:
	btst	#14-8,CUSTOM+DMACONR	; wait blit to complete
	bne	.waitblit0
	rts

notimplemented:
	lsr	#2,d0
	bsr	printbyte
	bsr	printcr
	lea	not_implemented_str,a0
	bsr	printstring
.halt:	jmp	.halt
	rts

; Function 1: Warm boot
;
; Entry parameters:
;	d0.w: $01
; Return value: None
warmboot:
	bsr	flush
	jmp	CCPSTART

;
; Function 2: Console status
;
; Entry parameters:
;	d0.w: $02
; Return value:
;	d0.w: $00ff if ready
;	d0.w: $0000 if not ready
;
constatus:
	move.w	key_index.l,d0
	cmp.w	#16,d0
	bcc	.notready
	move.l	#$ff,d0
	rts
.notready:
	clr.l	d0
	rts
;
; Function 3: Read console character
;
; Entry parameters:
;	d0.w: $03
; Return value:
;	d0.w: character
;
conin:	bsr	constatus
	beq	conin
	move.w	key_index.l,d1
	and.l	#$ff,d1
	lea	keyboard_buffer,a0
	move.b	0(a0,d1),d0
	add.w	#1,d1
	move.w	d1,key_index.l
	rts
;
; Function 4: Write console character
;
; Entry parameters:
;	d0.w: $04
;	d1.w: Character
; Return value: None
;
conout:
	and.l	#$000000ff,d1	; use only low byte
	cmp.w	#0,.escseq	; check if esc sequence is ongoing
	bne	.checksequence
.conout1:
	cmp.b	#$1b,d1		; check if esc
	beq	.escape
	cmp.b	#$20,d1
	bcs	.controlch
	cmp.b	#$7f,d1
	bcc	.controlch
	; calculate character address in font
	sub.b	#$20,d1		; space is first character in font
	move.l	d1,d0
	and.b	#$0f,d1		; each font row is 16 columns
	asr.w	#4,d0
	mulu.w	#16*8,d0
	add.l	d1,d0		; character address = column address + row address in font
	add.l	#font,d0
	movea.l	d0,a0
	; calculate destination address in screen
	move.w	.row,d0
	mulu.w	#80*8,d0
	move.w	.column,d1
	add.l	d1,d0
	add.l	#screen,d0
	movea.l	d0,a1
	bsr	waitblit	; we don't want mess up with screen if screen blit is ongoing
	; copy character to screen
	moveq	#8,d0
.loop:	move.b	(a0),(a1)
	add.l	#16,a0
	add.l	#80,a1
	subq	#1,d0
	bne	.loop
	; update cursor position
	move.w	.column,d0
	addq.w	#1,d0
	cmp.b	#80,d0
	bne	.noteol
	bsr	.boline
	bsr	.cursordown
	rts
.noteol
	move.w	d0,.column
	rts

; handle control characters

;	ADM-3A screen codes
;
;	ESC = row col		cursor position
;	^H			cursor left
;	^L			cursor right
;	^J			cursor down
;	^K			cursor up
;	^Z			home and clear screen
;	^M			carrage return
;	^G			bell

.controlch:
	cmp.b	#$0d,d1		; cr
	beq	.boline
	cmp.b	#$0a,d1		; line feed / ctrl+j
	beq	.cursordown
	cmp.b	#$0b,d1		; vertical tab / ctrl+k
	beq	.cursorup
	cmp.b	#8,d1		; backspace / ctrl+h
	beq	.cursorleft
	cmp.b	#$c,d1		; form feed / ctrl+l
	beq	.cursorright 
	cmp.b	#9,d1		; tab
	beq	.tab
	cmp.b	#$1a,d1		; home / clear screen
	beq	.cls
	; TODO rest of control characters
	rts

	; go to beginning of line
.boline:
	move.w	#0,.column
	rts

	; new line
.cursordown:
	move.w	.row,d0
	addq.w	#1,d0
	cmp.b	#32,d0
	beq	.scroll
	move.w	d0,.row
	rts

	; cursor up
.cursorup:
	move.w	.row,d0
	subq	#1,d0
	bmi	.cursorup1
	move.w	d0,.row
.cursorup1
	rts

	; cursor left
.cursorleft:
	move.w	.column,d0
	sub.w	#1,d0
	bmi	.cursorleft1
	move.w	d0,.column
.cursorleft1:
	rts

	; cursor right
.cursorright:
	move.w	.column,d0
	add.w	#1,d0
	cmp.w	#80,d0
	bcc	.cursorright1
	move.w	d0,.column
.cursorright1:
	rts

	; make tab
.tab:
	move.w	.column,d0
	and.w	#$f8,d0
	add.w	#8,d0
	cmp.w	#80,d0
	bcc	.boline
	move.w	d0,.column
	rts

	; clear screen
.cls:
	bsr	waitblit
	lea	CUSTOM,a0
	move.l	#screen,BLTDPTH(a0)		; destination address (bltdpth)
	move.w	#0,BLTDMOD(a0)			; zero modulo (bltdmod)
	move.w	#0,BLTCON1(a0)			; bltcon1
	move.w	#$100,BLTCON0(a0)		; use D, minterm = none (bltcon0)
	move.w	#((256<<6)|(80/2)),BLTSIZE(a0)	; height = 256, width = 40 words
	rts

	; scroll screen buffer
.scroll:
	bsr	waitblit
	lea	CUSTOM,a0
	move.w	#31,.row
	move.l	#screen,d0
	move.l	d0,BLTDPTH(a0)	; destination address (bltdpth)
	add.l	#8*80,d0
	move.l	d0,BLTAPTH(a0)	; source address (bltapth)
	move.w	#0,BLTAMOD(a0)	; zero modulo (bltamod)
	move.w	#0,BLTDMOD(a0)	; zero modulo (bltdmod)
	move.w	#-1,BLTAFWM(a0)	; first word mask (bltafwm)
	move.w	#-1,BLTALWM(a0)	; last word mask (bltalwm)
	move.w	#0,BLTCON1(a0)	; bltcon1
	move.w	#$9f0,BLTCON0(a0)		; use A&D, minterm = A (bltcon0)
	move.w	#((248<<6)|(80/2)),BLTSIZE(a0)	; height = 248, width = 40 words
	bsr	waitblit
	move.l	#screen,d0
	add.l	#248*80,d0
	move.l	d0,BLTDPTH(a0)			; destination address (bltdpth)
	move.w	#0,BLTDMOD(a0)			; zero modulo (bltdmod)
	move.w	#0,BLTCON1(a0)			; bltcon1
	move.w	#$100,BLTCON0(a0)		; use D, minterm = none (bltcon0)
	move.w	#((8<<6)|(80/2)),BLTSIZE(a0)	; height = 8, width = 40 words
	bsr	waitblit
	rts

	; handle escape sequence
.escape:
	move.w	#1,.escseq
	rts
.checksequence:
	cmp.w	#1,.escseq	; check if = is expected
	beq	.checkequal
	cmp.w	#2,.escseq	; check if x coord expected
	beq	.getx
	bne	.gety	
.checkequal:
	cmp.b	#$3d,d1
	bne	.notseq
	move.w	#2,.escseq
	rts
.getx:
	sub.w	#$20,d1
	move.w	d1,.escx
	move.w	#4,.escseq
	rts
.gety:
	sub.w	#$20,d1
	move.w	d1,.escy
.movecursor:
	move.w	.escx,d0	; store x if valid
	bmi	.notseq
	cmp.w	#COLS,d0
	bcc	.notseq
	move.w	d0,.column
	move.w	.escy,d0	; store y if valid
	bmi	.notseq
	cmp.w	#ROWS,d0
	bcc	.notseq
	move.w	d0,.row
.notseq:
	clr.w	d0
	move.w	d0,.escseq
	move.w	d0,.escx
	move.w	d0,.escy
	rts


.row:		dc.w	0
.column:	dc.w	0
.escseq		dc.w	0
.escx		dc.w	0
.escy		dc.w	0

keyboard_int:
	movem.l	d0-d1/a0-a1,-(sp)
	lea	CUSTOM,a0
	move.b	CIAA+SDR,d0		; read key
	not.b	d0
	ror.b	d0
	bmi	.keyreleased
	and.l	#$7f,d0
	cmp.b	#$60,d0
	bcc	.specialkey
	lea	keymap_us,a0
	lsl.b	#1,d0
	cmp.w	#0,ctrl_key
	bne	.storectrl		; branch if control key
	move.w	left_shift.l,d1
	or.w	right_shift.l,d1
	or.w	caps_lock.l,d1
	cmp.w	#0,d1
	beq	.noshift
	move.b	1(a0,d0),d1
	bra	.storekey
.storectrl:
	move.b	0(a0,d0),d1
	and.b	#$1f,d1			; clear 3 top most bits
	bra	.storekey
.noshift:
	move.b  0(a0,d0),d1
.storekey:
	move.w	key_index.l,d0
	bmi	.exit
	sub.w	#1,d0
	bmi	.exit			; keyboard buffer full
	move.w	d0,key_index.l
	lea	keyboard_buffer,a0
	move.b	d1,0(a0,d0)
	bra	.exit
.specialkey:
	moveq	#1,d1
	bra	.specialkey0
.keyreleased:
	and.w	#$7f,d0
	moveq	#0,d1
.specialkey0:
	cmp.w	#LEFTSHIFT,d0
	bne	.specialkey1
	move.w	d1,left_shift.l
.specialkey1:
	cmp.w	#RIGHTSHIFT,d0
	bne	.specialkey2
	move.w	d1,right_shift.l
.specialkey2:
	cmp.w	#CAPSLOCK,d0
	bne	.specialkey3
	move.w	d1,caps_lock.l
.specialkey3:
	cmp.w	#CONTROL,d0
	bne	.exit
	move.w	d1,ctrl_key.l
.exit:
	lea	CIAA,a1
	move.b	ICR(a1),d0		; clear interrupt in CIAA

	; start keyboard ack
	move.b	#$48,CRA(a1)		; Enable timer A, serial port output
	move.b	#$71,TALO(a1)		; requirement: min 75 microseconds, but 100 microseconds used
	move.b	#$00,TAHI(a1)		; 709379 Hz PAL * 0.000100 = 71
.wait:	btst.b	#0,ICR(a1)		; check timer A interrupt
	bne	.wait
	move.b	#0,CRA(a1)		; serial port input

	move.w	#$0008,CUSTOM+INTREQ	; clear CIA interrupt in Paula
	movem.l (sp)+,d0-d1/a0-a1
	rte

;
; Function 5: List character output
;
; Entry parameters:
;	d0.w: $05
;	d1.w: Character
; Return value: None
;
listchar:
	rts				; not implemented

;
; Function 6: Auxiliary output
;
; Entry parameters:
;	d0.w: $06
;	d1.w: Character
; Return value:
;	d0.w: Character
;
auxout:
	rts				; not implemented

;
; Function 5: Auxiliary input
;
; Entry parameters:
;	d0.w: $07
;	d1.w: Character
; Return value:
;	d0.w: Character
;
auxin:
	rts				; not implemented

;
; Function : Home
;
; Entry parameters:
;	d0.w: $08
; Return value: None
;
home:
	moveq	#0,d1
	bra	settrack

;
; Function 9: Select disk drive
;
; Entry parameters:
;	d0.w: $09
;	d1.b: Disk drive
;	d2.b: Logged in flag
; Return value:
;	d0.l: Address of selected drive's DPH
;
setdrive:
	moveq	#0,d0			; no DPH
	cmp.b	#0,d1
	bcs	.exit
	cmp.b	#DRIVES,d1
	bcc	.exit
	and.w	#$f,d1
	move.w	d1,cpm_drive
	and.w	#$ffff,d2
	move.w	d2,cpm_logged
	lsl.w	#2,d1			; multiply by 4 (offset in table)
	lea	floppy_dph_table,a0
	move.l	(a0,d1.w),d0
.exit
	rts

;
; Function 10: Set track number
;
; Entry parameters:
;	d0.w: $0a
;	d1.w: Disk track number
; Return value: None
;
settrack:
	cmp.w	#0,d1
	bcs	.exit
	cmp.w	#TRACKS,d1
	bcc	.exit
	move.w	d1,cpm_track
.exit:
	rts

;
; Function 11: Set logical sector number
;
; Entry parameters:
;	d0.w: $0b
;	d1.w: Logical sector number
; Return value: None
;
setsector:
	cmp.w	#(TRACKSIZE/128),d1
	bcc	.exit
	move.w	d1,cpm_sector
.exit:
	rts

; Function 12 Set DMA address
;
; Entry parameters:
;	d0.w:	$c
;	d1.l:	DMA address
; Return value: None
;
setdma:
	move.l	d1,cpm_dma
	rts

; Function 13 Read sector
;
; Entry parameters:
;	d0.w: $d
; Return value:
;	d0.w: 0 if no error, 1 if physical error
;
readsector:
	bsr	fd_read_track		; read track
	bne	.error			; branch if error

	move.w	cpm_sector,d0		; calculate src address in sector data
	lsl.w	#7,d0			; multiply by 128 (cp/m sector size)
	and.l	#$ffff,d0
	lea	sector_data,a0
	add.l	d0,a0

	move.l	cpm_dma,a1		; destination address
	
	move.w	#0,d1			; copy cp/m sector to "dma buffer"
.copy:	
	move.l	(a0)+,(a1)+
	addq.w	#4,d1
	cmp.w	#128,d1
	bne	.copy
	clr.w	d0			; no error
	rts
.error:
	move.w	#1,d0
	rts


; Function 14 Write sector
;
; Entry parameters:
;	d0.w: $e
;	d1.w: 0 = normal write, 1 = directory sector, 2 = new block
; Return value:
;	d0.w: 0 if no error, 1 if physical error
;
writesector:
	move.w	d1,fd_write		; save write type
	bsr	fd_read_track		; read track (if needed)
	bne	.error			; branch if error

	move.w	cpm_sector,d0		; calculate destination address in sector data
	lsl.w	#7,d0			; multiply by 128 (cp/m sector size)
	and.l	#$ffff,d0
	lea	sector_data,a0
	add.l	d0,a0

	move.l	cpm_dma,a1		; source address

	move.w	#0,d1			; copy "dma buffer" to sector data
.copy:
	move.l	(a1)+,(a0)+
	addq.w	#4,d1
	cmp.w	#128,d1
	bne	.copy

	move.b	#1,fd_dirty		; mark track as dirty
	move.w	cpm_track,fd_dirty_track
	move.w	cpm_drive,fd_dirty_drive
	cmp.w	#1,fd_write		; flush if directory sector
	bne	.exit
	bsr	fd_flush
	bne	.error
.exit:
	clr.w	d0			; no error
	rts
.error:
	move.w	#1,d0
	rts

; Function 15 Return list status: Not needed at first phase

; Function 16 Sector translate
;
; Entry parameters:
;	d0.w: $10
;	d1.w: Logical sector number
;	d2.l: Address of translate table
; Return value:
;	d0.w: physical sector number
sectortranslate:
	move.w	d1,d0
	rts

; Function 17 ?: Missing from CP/M-68k

; Function 18 Get address of memory region table
;
; Entry parameters:
;	d0.w: $12
; Return value:
;	d0.l: Memory region table address
getaddresstable:
	move.l	#.table,d0
	rts
.table:
	dc.w	1	; count
	dc.l	$400	; base address of first region
	dc.l	CPMSTART-$408

; Function 19 Get I/O byte: Not needed at first phase
; Function 20 Set I/O byte: Not needed at first phase

; Function 21 Flush buffers: Not started (dummy function)
;
;	Entry parameters: d0.w: $15
;	Return value: d0.w: 0 if no error, $ffff if physical error
flush:
	move.w	fd_dirty,d0		; check if disk changed
	bsr	fd_select
	bsr	fd_disk_change
	beq	.error			; cannot flush if disk changed
	bsr	fd_flush
	bne	.error
	clr.w	d0
	rts
.error:
	bsr	fd_deselect
	move.w	#-1,d0
	rts

; Function 22 Set exception handler address: Not started
;
; Entry parameters:
;	d0.w: $16
;	d1.w: Exception vector number
;	d2.l: Exception vector address
; Return value:
;	d0.l: Previous vector contents
setexception:
	and.l	#$ff,d1		; exception should be between 0-255
	lsl.l	#2,d1		; multiply by 4
	move.l	d1,a0
	move.l	(a0),d0		; return old vector value
	move.l	d2,(a0)		; insert new vector
	rts

;
; disk routines
;

; read track
;
; Input parameters: None
; Returns: Zero flag set if successful
;

fd_read_track:
	move.w	cpm_drive,d0		; check if disk changed
	bsr	fd_select
	bsr	fd_disk_change
	bne	.notchanged
	move.w	fd_dirty_drive,d0	; check if dirty drive same as drive with disk change
	cmp.w	cpm_drive,d0
	beq	.noflush		; we cannot flush if same drive
	bsr	fd_flush
	bne	.error			; branch if error
.noflush:
	bsr	fd_sync			; synchronization, fd_cache_ok = 0, fd_dirty = 0
	bne	.error
.notchanged:
	cmp.w	#0,fd_cache_ok		; read track if cache is not ok
	beq	.readtrack
	move.w	fd_drive,d0		; read track if drive is different
	cmp.w	cpm_drive,d0
	bne	.readtrack
	;lea	fd_drive_table,a0	; read track if track is different
	;move.w	fd_drive,d1
	;lsl.w	#2,d1
	;move.l	(a0,d1.w),a0
	;move.w	FD_TRACK(a0),d0
	bsr	fd_get_current_track	; read track if track is different
	cmp.w	cpm_track,d0
	bne	.readtrack
	bra	.cacheok
.readtrack
	bsr	fd_flush
	bne	.error			; branch if error
.readtrack2:
	move.w	#0,fd_cache_ok		; invalidate cache
	move.w	cpm_drive,d0
	move.w	d0,fd_drive
	bsr	fd_select
	move.w	cpm_track,d0
	bsr	fd_seek			; seek track
	clr.w	d0			; read
	bsr	fd_rw_track
	bne	.error			; branch if error
	bsr	fd_decode_track
	bne	.error			; branch if not successful

	move.w	#1,fd_cache_ok		; cache is ok
.cacheok:
	bsr	fd_deselect
	clr.w	d0
	rts
.error:
	bsr	fd_deselect
	move.b	#1,d0
	rts

; Input parameters: None
; Returns: zero flag if successful
fd_flush:
	cmp.w	#0,fd_dirty
	beq	.exit
	move.w	fd_dirty_drive,d0
	bsr	fd_select
	move.w	fd_dirty_track,d0
	bsr	fd_seek
	bsr	fd_encode_track
	move.w	#$4000,d0			; write
	bsr	fd_rw_track
	bne	.error
	bsr	fd_deselect
	move.w	#0,fd_dirty
.exit:
	rts
.error:
	bsr	fd_deselect
	move.b	#1,d0
	rts


;
; Select floppy drive
;
; Entry parameters:
;	d0.w: drive
; Return value: None
;
fd_select:
	;move.w	fd_drive,d1
	;cmp.w	d0,d1
	;beq	.fd_select1
	;clr.w	fd_cache_ok
.fd_select1:
	;move.w	d0,fd_drive
	and.b	#$7f,CIAB+PRB	; motor on
	move.b	#$f7,d1		; select drive
	rol.b	d0,d1
	and.b	d1,CIAB+PRB
	rts

;
; Deselect all floppy drives
;
; Entry parameters: None
; Return value: None
;
fd_deselect:
	or.b	#$78,CIAB+PRB	; deselect all drives
	or.b	#$80,CIAB+PRB	; motor off
	move.b	fd_drive,d0	; select drive, selected drive's motor will be off
	move.b	#$f7,d1
	rol.b	d0,d1
	and.b	d1,CIAB+PRB
	or.b	#$78,CIAB+PRB	; deselect all drives
	rts

;
; Select floppy side
;
; Entry parameters:
;	d0.w: side
; Return value: None
;
fd_set_side:
	cmp.w	#0,d0
	beq	.side0
	and.b	#$fb,CIAB+PRB
	bra	.exit
.side0	or.b	#$4,CIAB+PRB
.exit:	lea	fd_drive_table,a0
	move.w	fd_drive,d1
	lsl.w	#2,d1
	move.l	(a0,d1.w),a0
	move.b	d0,FD_SIDE(a0)
	and.w	#$fffe,FD_TRACK(a0)	; Adjust current track info
	or.w	d0,FD_TRACK(a0)
	rts

;
; Check if track zero
;
; Entry parameters: None
; Return value: zero flag set if track zero
;
fd_track_zero:
	move.b	CIAA+PRA,d0
	and.b	#$10,d0
	rts

;
; Check if disk change
;
; Entry parameters: None
; Return value: zero flag set if disk changed
;
fd_disk_change:
	move.b	CIAA+PRA,d0
	and.b	#$04,d0
	rts

;
; Set step direction
;
; Entry parameters:
;	d0.w: 0=forward, 1=backward
; Return value: None
;
fd_step_direction:
	move.b	CIAB+PRB,d1	; previous direction
	and.b	#$02,d1
	beq	.checkdir1	; branch if prev dir = forward
	bne	.checkdir2	; branch if prev dir = backward
.checkdir1:			; prev dir = forward
	and.b	#$01,d0
	beq	.exit		; branch if new dir = forward
	or.b	#$02,CIAB+PRB	; set direction backward
	bra	.delay18ms
.checkdir2:			; prev dir = backward
	and.b	#$01,d0
	bne	.exit		; branch if new dir = backward
	and.b	#$fd,CIAB+PRB	; set direction forward
.delay18ms:
	move.w	#18,d0
	bsr	delay
.exit:
	rts

;
; Move (step) one track
;
; Entry parameters: None
; Return value: None
;
fd_step:
	and.b	#$fe,CIAB+PRB
	or.b	#$01,CIAB+PRB
.delay3ms:
	move.w	#3,d0
	bsr	delay
	rts

;
; Resynchronize floppy drive
;
; Entry parameters: None
; Return value: zero flag set if successful

fd_sync:
	clr.w	fd_cache_ok		; set cache is not OK
	clr.w	fd_dirty		; no track yet written
	bsr	fd_track_zero
	bne	.skip
	move.w	#0,d0			; go forward one step if in track zero
	bsr	fd_step_direction
	bsr	fd_step
.skip:	move.w	#1,d0
	bsr	fd_step_direction
	move.l	d2,-(sp)
	clr.w	d2			; a number of steps
.loop:					; step track until track zero is reached
	bsr	fd_track_zero
	beq	.exit
	bsr	fd_step
	addq.w	#1,d2
	cmp.w	#CYLINDERS+15,d2
	bne	.loop
.fail:	move.l	(sp)+,d2
	lea	no_track_zero_str(pc),a0
	bsr	printstring
	or.w	#1,d0			; clear zero flag i.e. error
	rts
.exit:
	lea	fd_drive_table,a0	; set current track to zero
	move.w	fd_drive,d1
	lsl.w	#2,d1
	move.l	(a0,d1.w),a0
	move.w	#0,FD_TRACK(a0)
	move.l	(sp)+,d2
.delay15ms:
	move.w	#15,d0
	bsr	delay
	clr.w	d0			; set zero flag i.e. track zero was found
	rts

;fd_detect_drive:
;	rts

; Get current track
;
; Entry parameters: None
; Return values: d0.w track
;                a0 address of drive structure
fd_get_current_track:
	lea	fd_drive_table,a0	; get current track number
	move.w	fd_drive,d1
	lsl.w	#2,d1
	move.l	(a0,d1.w),a0
	move.w	FD_TRACK(a0),d0
	rts

; Seek track
;
; Entry parameters: d0.w track
; Return value: zero flag set if successful
fd_seek:
	movem.l	d2-d3,-(sp)
	move.w	d0,d2			; track number
	and.w	#1,d0			; set side
	bsr	fd_set_side
	bsr	fd_get_current_track
	move.w	d2,d3			; track
	sub.w	d0,d3			; offset = track - current track
	beq	.exit			; exit if offset is zero
	bcs	.neg			; offset < 0
	clr.w	d0			; step forward
	bra	.setdir
.neg:	moveq	#1,d0			; step backward
.setdir:
	bsr	fd_step_direction
.loop:
	bsr	fd_get_current_track	; exit if current track = track
	cmp.w	d0,d2
	beq	.exit
	bsr	fd_step
	bsr	fd_get_current_track
	move.b	CIAB+PRB,d1		; get current direction and update track number
	and.b	#$02,d1
	beq	.add
	subq.w	#2,d0
	bra	.store
.add:	addq.w	#2,d0
.store:	move.w	d0,FD_TRACK(a0)	
	bra	.loop
.exit:	clr.w	d0			; set zero flag
	movem.l	(sp)+,d2-d3
	rts

; wait motor on
;
; Entry parameters: None
; Return value: zero flag set if successful
fd_wait_motor:
	move.w	#500,d0			; 500 milliseconds timeout
	bsr	starttimer
.wait:	btst.b	#5,CIAA+PRA
	beq	.exit 
	btst.b	#1,CIAB+ICR		; check timer B interrupt
	beq	.wait
	moveq	#1,d0
	rts
.exit:
	clr.b	CIAB+CRA		; disable timer A in CIAB
	clr.b	CIAB+CRB		; disable timer B in CIAB
	move.b	#$7f,CIAB+ICR		; disable all interrupts in CIAB
	clr.w	d0			; set zero flag
	rts

; wait disk dma
;
; Entry parameters: None
; Return value: zero flag set if successful
fd_wait_dma:
	move.w	#1000,d0		; 1000 milliseconds timeout
	bsr	starttimer
.wait:	move.w	CUSTOM+INTREQR,d0
	and.b	#2,d0			; check disk block finished interrupt
	bne	.exit 
	btst.b	#1,CIAB+ICR		; check timer B interrupt
	beq	.wait
	moveq	#1,d0
	rts
.exit:
	clr.b	CIAB+CRA		; disable timer A in CIAB
	clr.b	CIAB+CRB		; disable timer B in CIAB
	move.b	#$7f,CIAB+ICR		; disable all interrupts in CIAB
	clr.w	d0			; set zero flag
	rts

; read/write track
;
; Entry parameters: d0.w ($4000=write, 0=read)
; Return value: Zero flag set if successful
fd_rw_track:
	move.w	d0,fd_rw
	bsr	fd_wait_motor		; check that motor is on
	beq	.ok1
	lea	motor_error_str(pc),a0
	bsr	printstring
	move.b	#1,d0
	rts
.ok1:
	move.w	#$4000,CUSTOM+DSKLEN	; dma disable + read
	lea	mfm_track(pc),a1	; set buffer address
	move.l	a1,CUSTOM+DSKPTH
	move.w	#($8000+(MFM_TRACKSIZE/2)),d1	; dma enable + buffer size in words
	move.w	fd_rw,d0
	or.w	d0,d1			; set read/write mode
	move.w	d1,CUSTOM+DSKLEN	; transfer is triggered by writing reg twice
	move.w	d1,CUSTOM+DSKLEN
	bsr	fd_wait_dma
	beq	.ok2
	lea	dma_error_str(pc),a0
	bsr	printstring
	move.w	#$4000,CUSTOM+DSKLEN	; dma disable + read
	move.b	#1,d0
	rts
.ok2:
	move.w	#$4000,CUSTOM+DSKLEN	; dma disable + read
	move.w	#2,CUSTOM+INTREQ	; clear disk dma interrupt
	clr.w	d0
	rts

; mfm decode long
;
; Entry parameters: d1.l (checksum)
;                   d2.w (offset to even long)
;                   a0 (address to odd long)
; Internally uses also:
;                   d3.l (decoded odd long)
;                   d4.l (decoded even long)
; Return values: d0.l (decoded long)
;                d1.l (checksum)
;                a0 (address to next odd long)
fd_decode_long:
	movem.l	d3-d4,-(sp)
	move.l	0(a0),d3		; decode odd long
	and.l	#$55555555,d3
	move.l	(a0,d2.w),d4		; decode even long
	and.l	#$55555555,d4
	move.l	d3,d0
	lsl.l	#1,d0			; odd long = odd long << 1
	or.l	d4,d0			; result = (odd long << 1) | even long
	eor.l	d3,d4			; checksum = chekcsum xor decoded odd long xor decoded even long
	eor.l	d4,d1
	;bsr	printlong
	;bsr	printcr
	addq.l	#4,a0
	movem.l	(sp)+,d3-d4
	rts

; MFM Sector Format
; =================
;
; 2xword mfm value $aaaaaaaa (when decoded, two bytes of 0)
;
; Synchronization
; $4489 Synchronization
; $4489 Synchronization
;
; Header
; 1xlong Info (odd bits)
; 1xlong Info (even bits)
; 4xlong Sector label (odd)
; 4xlong Sector label (even)
; End of Header
;
; 1xlong Header checksum (odd)
; 1xlong Header checksum (even)
; 
; 1xlong Data checksum (odd)
; 1xlong Data checksum (even)
;
; Data
; 512xbyte Data (odd)
; 512xbyte Data (even)
; End of Data
;

; decode track
;
; Entry parameters: None
; Returns: Zero flag set if successful
;
fd_decode_track:
	; a0 = current position (mfm track)
	; a1 = destination (current position of sector data)
	; a2 = current position (mfm track) + offset
	; a3 = next sector (mfm track)
	; d0.l = decoded long
	; d1.l = checksum
	; d2.w = current offset to even long
	; d3.w = a number of sectors decoded
	; d4.l = sector info $ffTTSSSG
	;        $ff = Amiga v1.0 format
	;        TT = track number ( 3 means cylinder 1, head 1)
	;        SS = sector number (0 to 10)
	;        SG = sectors until end of writing (including current one)
	; d5.w = current mfm sector number
	; d6.w = current mfm track number
	; d7.l = tmp

	 movem.l d0-d7/a0-a3,-(sp)
	;bsr	fd_get_current_track		; get current track number
	;move.w	d0,d6
	;move.w	FD_SIDE(a0),d0
	;add.w	d0,d6
	lea	mfm_track,a0			; set mfm buffer address
	clr.w	d3				; number of sectors decoded = 0

	; skip sync words
.loop:	cmp.l	#(mfm_track+MFM_TRACKSIZE),a0	; is current position >= mfm track address + mfm track size
	bcc	.toofewsectors
	cmp.w	#$4489,(a0)			; skip sync words
	bne	.skipsyncdone
	addq.l	#2,a0
	bra	.loop
.skipsyncdone:
	move.l	a0,a2				; is current position + 1088 >= mfm track address + mfm track size
	add.l	#1088,a2
	cmp.l	#(mfm_track+MFM_TRACKSIZE),a2
	bcc	.toofewsectors
	move.l	a0,a3				; next sector = current position + 1082
	add.l	#1082,a3

	; get sector info
	clr.l	d1				; checksum = 0
	move.w	#4,d2				; decode sector info
	bsr	fd_decode_long
	move.l	d0,d4				; save sector info
	addq.l	#4,a0				; current position = current position + 4
	;and.l	#$ff000000,d0			; check high byte of sector info, $ff = Amiga v1.0 format
	;cmp.l	#$ff000000,d0
	;bne	.badsectorheader
	move.l	d4,d5				; check sector number (should be 0-10)
	and.l	#$ff00,d5
	lsr.w	#8,d5
	cmp.w	#11,d5
	bcc	.badsectorheader
	cmp.w	#0,d5
	bcs	.badsectorheader

	; decode sector label
	move.w	#16,d2				; offset to even long
	clr.w	d7
.decodelabel:
	bsr	fd_decode_long
	addq.w	#4,d7
	cmp.w	#16,d7
	bne	.decodelabel
	add.l	#16,a0				; current position = current position + 16

	; check header checksum
	move.l	d1,d7				; save checksum
	move.w	#4,d2				; offset to even long
	bsr	fd_decode_long
	cmp.l	d0,d7				; compare checksum
	bne	.badsectorheaderchecksum
	addq.l	#4,a0				; current position = current position + 4

	; decode data checksum
	bsr	fd_decode_long
	move.l	d0,d1
	addq.l	#4,a0				; current position = current position + 4
	
	; decode data
	move.l	d5,d0
	moveq.l	#9,d7
	lsl.l	d7,d0				; number of sector * 512
	lea	sector_data,a1
	add.l	d0,a1				; destination address = sector buffer + number of sector * 512
	clr.w	d7
	move.w	#512,d2				; offset to even long
.decodedata:
	bsr	fd_decode_long
	move.l	d0,(a1)+
	addq.w	#4,d7
	cmp.w	#512,d7
	bne	.decodedata

	; check data checksum
	cmp.l	#0,d1
	bne	.badsectorchecksum

	; check number of sectors decoded
	addq.w	#1,d3
	cmp.w	#11,d3
	beq	.exit

	; skip non sync words
	move.l	a3,a0				; current position = beginnig of next sector - 6
.skip:	cmp.w	#$4489,(a0)			skip non sync words
	beq	.loop
	addq.l	#2,a0
	cmp.l	#(mfm_track+MFM_TRACKSIZE),a0	; is current position >= mfm track address + mfm track size
	bcc	.toofewsectors
	bra	.skip

.badsectorheader:
	lea	bad_sector_header_str,a0
	bsr	printstring
	bra	.exit0
.badsectorheaderchecksum:
	lea	bad_sector_header_checksum_str,a0
	bsr	printstring
	bra	.exit0
.toofewsectors:
	lea	too_few_sectors_str,a0
	bsr	printstring
	bra	.exit0
.badsectorchecksum:
	lea	bad_sector_checksum_str,a0
	bsr	printstring
.exit0:
	move.b	#1,d0
.exit:
	movem.l (sp)+,d0-d7/a0-a3
	rts

; Encode track
;
; Entry parameters: None
; Returns: None
fd_encode_track:
	; a0 = current position (mfm track)
	; a1 = current position of sector data
	; d0.l = odd
	; d1.l = even
	; d3.l = number of sectors encoded
	; d4.l = sector info $ffTTSSSG
	;        $ff = Amiga v1.0 format
	;        TT = track number ( 3 means cylinder 1, head 1)
	;        SS = sector number (0 to 10)
	;        SG = sectors until end of writing (including current one)
	; d5.l = checksum
	; d6.l = tmp
	; d7.l = tmp

	movem.l d0-d7/a0-a1,-(sp)
	bsr	fd_get_current_track		; get current track number
	move.l	d0,d4
	and.l	#$000000ff,d4			; 000000TT sector header info
	moveq.l	#16,d7
	lsl.l	d7,d4				; 00TT0000
	or.l	#$ff000000,d4			; ffTT0000
	
	lea	mfm_track,a0			; set mfm buffer address
	lea	sector_data,a1

	; fill gap
	move.l	#0,d7
.fillgap:
	move.l	#$aaaaaaaa,(a0,d7.w)
	add.w	#4,d7
	cmp.w	#GAPSIZE,d7
	bne	.fillgap

	clr.l	d3				; number of sectors encoded = 0
	add.l	d7,a0				; current position = mfm_track + gapsize
.fillsector:

	; fill 2 x zero words and 2 x sync words
	moveq.l	#1,d7				; check previous bit
	and.l	-4(a0),d7
	beq	.clockbit1
	move.l	#$2aaaaaaa,(a0)			; 2 x zero words
	bra	.clockbit2
.clockbit1:
	move.l	#$aaaaaaaa,(a0)			; 2 x zero words
.clockbit2:
	move.l	#$44894489,4(a0)		; 2 x sync words

	; update sector header info
	and.l	#$ffff0000,d4			; ffTT0000 sector header info
	move.l	d3,d7
	lsl.w	#8,d7
	or.l	d7,d4				; ffTTSS00
	moveq.l	#11,d7
	sub.l	d3,d7
	or.l	d7,d4				; ffTTSSSG

	move.l	d4,d1				; even (sector header info)
	move.l	d4,d0				; odd = even >> 1
	lsr.l	#1,d0

	and.l	#$55555555,d1			; even = even & $55555555
	move.l	d1,12(a0)

	and.l	#$55555555,d0			; odd = odd & $55555555
	move.l	d0,8(a0)

	; calculate header checksum
	clr.l	d5
	eor.l	d0,d5
	eor.l	d1,d5

	; save header checksum
	move.l	d5,52(a0)
	lsr.l	#1,d5				; odd = even >> 1
	move.l	d5,48(a0)

	; fill rest of header with zeroes
	move.w	#16,d7
.clear:
	move.l	#0,(a0,d7.w)
	addq.w	#4,d7
	cmp.w	#48,d7
	bne	.clear

	; copy sector data
	clr.l	d7				; offset to odd data
	move.l	#512,d6				; offset to even data
	clr.l	d5				; clear checksum
.copydata:
	move.l	(a1,d7.w),d1			; copy from sector data to even
	move.l	d1,d0				; odd = even >> 1
	lsr.l	#1,d0
	and.l	#$55555555,d0			; odd = odd & $55555555
	and.l	#$55555555,d1			; even = even & $55555555
	move.l	d0,64(a0,d7.w)			; copy odd data
	move.l	d1,64(a0,d6.w)			; copy even data
	addq.l	#4,d7				; update offsets
	addq.l	#4,d6
	eor.l	d0,d5				; update checksum
	eor.l	d1,d5
	cmp.w	#512,d7
	bne	.copydata

	; save data checksum
	move.l	d5,60(a0)
	lsr.l	#1,d5				; odd = even >> 1
	move.l	d5,56(a0)

	; mfm encode
	; d0.l = previous
	; d1.l = v
	; d5.l = mask1
	; d6.l = mask2
	; d7.l = offset
	move.l	4(a0),d0			; previous
	clr.l	d7
.encode:
	move.l	8(a0,d7.w),d1			; copy long word from mfm data
	and.l	#$55555555,d1			; mfm data = mfm data & $55555555
	ror.l	#1,d0				; previous << 31
	and.l	#$80000000,d0
	move.l	d1,d5				; v >> 1
	lsr.l	#1,d5
	or.l	d0,d5				; mask1 = (previous << 31) | (v >> 1)
	move.l	d1,d6
	lsl.l	#1,d6				; mask2 = v << 1
	eor.l	#$ffffffff,d5			; mask1 = mask1 xor $ffffffff
	eor.l	#$ffffffff,d6			; mask1 = mask2 xor $ffffffff
	and.l	d5,d6				; mask1 & mask2
	and.l	#$aaaaaaaa,d6			; & $aaaaaaaa
	move.l	d1,d0				; previous = mfm data
	or.l	d6,d1
	move.l	d1,8(a0,d7.w)
	addq.l	#4,d7
	cmp.l	#1080,d7
	bne	.encode

	; update pointers
	add.l	#512,a1
	add.l	#1088,a0

	; check if last sector
	addq.l	#1,d3
	cmp.l	#11,d3
	bne	.fillsector

	moveq.l	#1,d7				; check previous bit
	and.l	-4(a0),d7
	beq	.clockbit3
	move.l	#$2aaaaaaa,(a0)			; 2 x zero words
	bra	.clockbit4
.clockbit3:
	move.l	#$aaaaaaaa,(a0)			; 2 x zero words
.clockbit4:	
	
	movem.l (sp)+,d0-d7/a0-a1
	rts

;
; print routines
;

; print string
;
; a0 = address of string
printstring:
	movem.l	d0-d1/a0-a2,-(sp)
	move.l	a0,a2
.loop:
	move.b	(a2)+,d1
	beq	.exit
	bsr	conout
	bra	.loop
.exit:
	movem.l	(sp)+,d0-d1/a0-a2
	rts

; print cr+lf
;
;
printcr:
	move.l	d0,-(sp)
	move.b	#13,d0
	bsr	printchar
	move.b	#10,d0
	bsr	printchar
	move.l	(sp)+,d0
	rts

; print a character
; does not change any registers
;
; d0.b = char to print
printchar:
	movem.l	d0-d1/a0-a1,-(sp)
	move.b	d0,d1
	bsr	conout
	movem.l	(sp)+,d0-d1/a0-a1
	rts

; print byte as hex
;
; d0.b = byte to print
printbyte:
	movem.l	d0-d1/a0-a1,-(sp)
	move.b	d0,d1
	move.l	d1,-(sp)
	rol.b	#4,d1
	and.w	#$f,d1
	lea	hex(pc),a0
	move.b	0(a0,d1.w),d1
	bsr	conout
	move.l	(sp)+,d1
	and.w	#$f,d1
	lea	hex(pc),a0
	move.b	0(a0,d1.w),d1
	bsr	conout
	movem.l	(sp)+,d0-d1/a0-a1
	rts

; print word as hex
;
; d0.w = word to print
printword:
	move.l	d0,-(sp)
	rol.w	#8,d0
	bsr	printbyte
	rol.w	#8,d0
	bsr	printbyte
	move.l	(sp)+,d0
	rts

; print long as hex
;
; d0.l = long to print
printlong:
	move.l	d0,-(sp)
	swap	d0
	bsr	printword
	swap	d0
	bsr	printword
	move.l	(sp)+,d0
	rts

hex:	dc.b	"0123456789ABCDEF"

;
; timer routines
;

; start timer
;
; Entry parameters:
;	d0.w:	time in milliseconds
starttimer:
	movem.l	d0/a1,-(sp)
	lea	CIAB,a1
	move.b	ICR(a1),d1		; ack CIAB interrupts
	move.b	#$01,CRA(a1)		; timer A: continuous mode
	move.b	#$48,CRB(a1)		; timer B: count timer A underflows + one shot mode
	move.b	#$c5,TALO(a1)		; set timer A count, 2c5 = 709, about 1 milliseconds
	move.b	#2,TAHI(a1)
	move.b	d0,TBLO(a1)		; set timer B count
	lsr.w	#8,d0
	move.b	d0,TBHI(a1)
	movem.l	(sp)+,d0/a1
	rts

; delay in milliseconds
;
; Entry parameters:
;	d0.w:	delay in milliseconds
delay:
	bsr	starttimer
.wait:	btst.b	#1,CIAB+ICR		; check timer B interrupt
	beq	.wait
	clr.b	CIAB+CRA		; disable timer A in CIAB
	clr.b	CIAB+CRB		; disable timer B in CIAB
	move.b	#$7f,CIAB+ICR		; disable all interrupts in CIAB
	rts


;
; data
;

; copper list
copper:
	dc.w	$e0,0
	dc.w	$e2,0
	dc.w	$ffff,$fffe

; keyboard variables
key_index:
	dc.w	16
keyboard_buffer:
	blk.b	16
left_shift:
	dc.w	0
right_shift:
	dc.w	0
caps_lock:
	dc.w	0
ctrl_key:
	dc.w	0

; floppy variables
fd_write:
	dc.w	0
fd_dirty:
	dc.w	0
fd_dirty_drive:
	dc.w	0
fd_dirty_track:
	dc.w	0
fd_rw:
	dc.w	0
fd_drive:
	dc.w	0
fd_cache_ok:
	dc.w	0
fd_cache_dirty:
	dc.w	0
cpm_drive:
	dc.w	0
cpm_logged:
	dc.w	0
cpm_track:
	dc.w	0
cpm_sector:
	dc.w	0
cpm_dma:
	dc.l	0
fd_drive_table:
	dc.l	df0
	dc.l	df1
	dc.l	df2
	dc.l	df3
df0:
	dc.w	0	; track
	dc.w	0	; side
	dc.w	0	; sector
df1:
	dc.w	0	; track
	dc.w	0	; side
	dc.w	0	; sector
df2:
	dc.w	0	; track
	dc.w	0	; side
	dc.w	0	; sector
df3:
	dc.w	0	; track
	dc.w	0	; side
	dc.w	0	; sector

; pointers to disk parameter headers
floppy_dph_table:
	dc.l	floppy_dph1
	dc.l	floppy_dph2

; floppy disk parameter header for drive A
floppy_dph1:
	dc.l	0			; xlt, no sector translation table
	dc.w	0			; scratch pad
	dc.w	0			; scratch pad
	dc.w	0			; scratch pad
	dc.l	floppy_dir_buffer	; address of directory buffer
	dc.l	floppy_dpb		; address of disk parameter block
	dc.l	floppy_csv1		; address of checksum vector
	dc.l	floppy_alv1		; address of scratchpad area

; floppy disk parameter header for drive B
floppy_dph2:
	dc.l	0			; xlt, no sector translation table
	dc.w	0			; scratch pad
	dc.w	0			; scratch pad
	dc.w	0			; scratch pad
	dc.l	floppy_dir_buffer	; address of directory buffer
	dc.l	floppy_dpb		; address of disk parameter block
	dc.l	floppy_csv2		; address of checksum vector
	dc.l	floppy_alv2		; address of scratchpad area
	
; floppy disk parameter block
floppy_dpb:
	dc.w	TRACKSIZE/128			; spt, 11*512/128=44, number of 128 byte logical sectors per track
	dc.b	4 				; bsh, block shift factor = 5 when block size = 2048
	dc.b	15 				; blm, block mask = 15 when block size = 2048
	dc.b	0				; exm, extent mask = 0 when block size = 2048 and dsm > 255
	dc.b	0				; reserved
	dc.w	TRACKS*TRACKSIZE/BLOCKSIZE-1	; dsm, number of last block (2*80)*(11*512)/2048-1 = 439
	dc.w	DIRENTRIES-1			; drm, directory entries - 1
	dc.w	0				; reserved, directory mask?
	dc.w	DIRENTRIES/4			; cks, size of directory check vector (drm/4)+1
	dc.w	RESERVEDTRACKS			; off, number of reserved tracks
; floppy directory buffer
floppy_dir_buffer
	blk.b	128,0
; floppy checksum vector
floppy_csv1:
	blk.b	DIRENTRIES/4,0
floppy_csv2:
	blk.b	DIRENTRIES/4,0
; floppy scratchpad area
floppy_alv1:
	blk.b	TRACKS*TRACKSIZE/BLOCKSIZE/8
	even
floppy_alv2:
	blk.b	TRACKS*TRACKSIZE/BLOCKSIZE/8
	even
; strings
bios_str:
	dc.b	"*** SturmBIOS for Commodore Amiga v0.39 ***",13,10
        dc.b    "***   Coded by Juha Ollila  2021-2022   ***",13,10,13,10,0
motor_error_str:
	dc.b	13,10,"BIOS Error: Drive not ready",13,10,0
dma_error_str:
	dc.b	13,10,"BIOS Error: Disk DMA timeout",13,10,0
too_few_sectors_str:
	dc.b	13,10,"BIOS Error: Too few sectors",13,10,0
bad_sector_header_str:
	dc.b	13,10,"BIOS Error: Bad sector header",13,10,0
bad_sector_header_checksum_str:
	dc.b	13,10,"BIOS Error: Bad sector header checksum",13,10,0
bad_sector_checksum_str:
	dc.b	13,10,"BIOS Error: Bad sector checksum",13,10,0
no_track_zero_str:
	dc.b	13,10,"BIOS Error: Cannot find track zero",13,10,0
not_implemented_str:
	dc.b	13,10,"BIOS Error: Not implemented",13,10,0
	even

; keymap (raw code to ascii)
keymap_us:
	dc.b	"`~","1!","2@","3#","4$","5%","6^","7&"
	dc.b	"8*","9(","0)","-_","=+","\|",0,0,"00"
	dc.b	"qQ","wW","eE","rR","tT","yY","uU","iI"
	dc.b	"oO","pP","[{","]}",0,0,"11","22","33"
	dc.b	"aA","sS","dD","fF","gG","hH","jJ","kK"
	dc.b	"lL",";:","'",$22,0,0,0,0,"44","55","66"
	dc.b	0,0,"zZ","xX","cC","vV","bB","nN","mM"
	dc.b	",<",".>","/?",0,0,"..","77","88","99"
	dc.b	"  ",$8,$8,$9,$9,$d,$d,$d,$d,$1b,$1b,$7f,$7f,0,0
	dc.b	0,0,0,0,"--",0,0,$b,$b,$a,$a,$c,$c,$8,$8
	dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dc.b	0,0,0,0,"((","))","//","**","++",0,0
	even

; 8x8 Topaz system font
font:
	incbin ../font/topaz128x112x1.raw
	even

sector_data:
	; 11*512 bytes
mfm_track = (sector_data+TRACKSIZE)
	; 13630 bytes
screen = (mfm_track+MFM_TRACKSIZE)
	; 20 kB
end_address = (screen+(640*256/8))
