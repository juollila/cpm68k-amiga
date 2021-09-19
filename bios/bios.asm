CUSTOM	= $dff000	; Start of custom chips

; Custom chip register offsets
DMACONR = $002	; DMA control read
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
RESERVEDTRACKS	= 6	; boot loader + 24k (bdos + ccp) + 8k (bios)
BLOCKSIZE	= 2048	; CPM FS block size
DIRENTRIES	= 128	; CPM directory entries
DRIVES		= 2	; Only 2 floppy drives supported (Amiga can support upto 4)

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




	code_c

; Function 0: Initialization
;
; Entry parameters:
;	d0.w: $00
; Return value: d0.w: user/disk numbers

_init:	
	lea	CUSTOM,a6
	bsr	takeover
	bsr	initscreen
	bsr	initkeyboard
	bsr	initfloppy

	lea	biosstring(pc),a0
	bsr	printstring
	move.l	#$1234ABCD,d0
	bsr	printlong

	bra	dosomestuff

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
	; clear screen
	move.w	#$1a,d1
	bsr	conout
	rts

initkeyboard:
	lea	CIAA,a0
	move.b	#0,CRA(a0)		; disable timer A
	move.b	#0,CRB(a0)		; disable timer B
	move.b	#$7f,ICR(a0)		; clear all interrupts in CIAA
	move.b	#$88,ICR(a0)		; enable serial port interrupt in CIAA
	move.b	#$7f,CIAB+ICR		; clear all interrupts in CIAB
	move.l	#keyboard_int,$68.w	; keyboard interrupt vector to level 2 autovector
	move.w	#$c008,INTENA(a6)	; enable CIA interrupts
	rts	

initfloppy:
	lea	CIAA,a0
	lea	CIAB,a1
	move.b	#$03,DDRA(a0)		; audio filter & rom overlay are outputs
					; joy0 fire, joy1 fire, drive ready, track 0, write prot & disk change are inputs
	move.b	#$ff,DDRB(a1)		; motor, sel3, sel2, sel1, sel0, side, dir and step are outputs
	move.b	#$ff,PRB(a1)		; disable all
	move.w	#$6200,ADKCON(a6)	; disable precomp1, precomp0 and msbsync
	move.w	#$9500,ADKCON(a6)	; enable mfmprec, wordsync and fast
	move.w	#$4489,DSKSYNC(a6)	; set mfm sync mark
	move.w	#$8010,DMACON(a6)	; enable disk DMA
	rts

; print string
;
; a0 = address of string
printstring:
	move.l	a2,-(sp)
	move.l	a0,a2
.loop:
	move.b	(a2)+,d1
	beq	.exit
	bsr	conout
	bra	.loop
.exit:
	move.l	(sp)+,a2
	rts

; print byte
;
; d0.b = byte to print
printbyte:
	move.l	d0,-(sp)
	move.b	d0,d1
	rol.b	#4,d1
	and.b	#$f,d1
	lea	hex(pc),a0
	move.b	0(a0,d1),d1
	bsr	conout
	move.l	(sp)+,d0
	move.l	d0,-(sp)
	move.b	d0,d1
	and.b	#$f,d1
	lea	hex(pc),a0
	move.b	0(a0,d1),d1
	bsr	conout
	move.l	(sp)+,d0
	rts

; print word as hex
;
; d0.w = word to print
printword:
	rol.w	#8,d0
	bsr	printbyte
	rol.w	#8,d0
	bsr	printbyte
	rts

; print long as hex
;
; d0.l = long to print
printlong:
	swap	d0
	bsr	printword
	swap	d0
	bsr	printword
	rts

hex:	dc.b	"0123456789ABCDEF"


dosomestuff:
.skip:
	bsr	conin
	move.w	d0,d1
	bsr	conout
	bra	.skip

	; TODO: setting up trap handler, internal variables (e.g. iobyte)
	; move.l	#traphandler,$8c	; set trap 3 handler
	clr.l	d0			; login drive A, user 0
	rts

waitblit:
	tst.w	CUSTOM+DMACONR
.waitblit0:
	btst	#14-8,CUSTOM+DMACONR	; wait blit to complete
	bne	.waitblit0
	rts 

; Function 1: Warm boot
;
; Entry parameters:
;	d0.w: $01
; Return value: None
warmboot:
	; jmp	_ccp

; Function 2: Console status
;
; Entry parameters:
;	d0.w: $02
; Return value:
;	d0.w: $00ff if ready
;	d0.w: $0000 if not ready
constatus:
	move.w	key_index.l,d0
	cmp.w	#16,d0
	bcc	.notready
	move.l	#$ff,d0
	rts
.notready:
	clr.l	d0
	rts

; Function 3: Read console character
;
; Entry parameters:
;	d0.w: $03
; Return value:
;	d0.w: character
conin:	bsr	constatus
	beq	conin
	move.w	key_index.l,d1
	and.l	#$ff,d1
	lea	keyboard_buffer,a0
	move.b	0(a0,d1),d0
	add.w	#1,d1
	move.w	d1,key_index.l
	rts

; Function 4: Write console character
;
; Entry parameters:
;	d0.w: $04
;	d1.w: Character
; Return value: None
	
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
	lea	$dff000,a0
	move.w	.column,d0
	addq	#1,d0
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
	addq	#1,d0
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
	move.l	#screen,BLTDPTH(a0)		; destination address (bltdpth)
	move.w	#0,BLTDMOD(a0)			; zero modulo (bltdmod)
	move.w	#0,BLTCON1(a0)			; bltcon1
	move.w	#$100,BLTCON0(a0)		; use D, minterm = none (bltcon0)
	move.w	#((256<<6)|(80/2)),BLTSIZE(a0)	; height = 256, width = 40 words
	rts

	; scroll screen buffer
.scroll:
	bsr	waitblit
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
	;move.w	#$ffff,screen
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
	eor.w	#$fff,color
	move.w	color,COLOR1(a0)
	;move.w	#$00f,COLOR1(a0)
	; TODO: read scan code from SDR, convert to ASCII and save
	lea	CIAA,a1
	move.b	SDR(a1),d0		; read key
	not.b	d0
	ror.b	d0
	bmi	.keyreleased
	and.l	#$7f,d0
	cmp.b	#$60,d0
	bcc	.specialkey
	lea	keymap,a0
	lsl.b	#1,d0
	move.w	left_shift.l,d1
	or.w	right_shift.l,d1
	or.w	caps_lock.l,d1
	cmp.w	#0,d1
	beq	.noshift
	move.b	1(a0,d0),d1
	;movem.l	d0-d1/a0-a1,-(sp)
	;bsr	conout
	;movem.l (sp)+,d0-d1/a0-a1
	bra	.storekey
.noshift:
	move.b	0(a0,d0),d1
	;movem.l	d0-d1/a0-a1,-(sp)
	;bsr	conout
	;movem.l (sp)+,d0-d1/a0-a1
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
	move.w	#$f00,CUSTOM+COLOR1
	move.w	d1,left_shift.l
.specialkey1:
	cmp.w	#RIGHTSHIFT,d0
	bne	.specialkey2
	move.w	#$0f0,CUSTOM+COLOR1
	move.w	d1,right_shift.l
.specialkey2:
	cmp.w	#CAPSLOCK,d0
	bne	.specialkey3
	move.w	#$880,CUSTOM+COLOR1
	move.w	d1,caps_lock.l
.specialkey3:
	cmp.w	#CONTROL,d0
	bne	.exit
	move.w	d1,ctrl_key.l
.exit:
	move.b	ICR(a1),d0		; clear interrupt in CIAA

	; start keyboard ack
	move.b	#$48,CRA(a1)		; Enable timer A, serial port output
	move.b	#$71,TALO(a1)		; requirement: min 75 microseconds, but 100 microseconds used
	move.b	#$00,TAHI(a1)		; 709379 MHz PAL * 0.000100 = 71
.wait:	btst.b	#0,ICR(a1)		; check timer A interrupt
	bne	.wait
	move.b	#0,CRA(a1)		; serial port input

	move.w	#$0008,CUSTOM+INTREQ	; clear CIA interrupt in Paula
	movem.l (sp)+,d0-d1/a0-a1
	rte

; Function 5: List character output
;
; Entry parameters:
;	d0.w: $05
;	d1.w: Character
; Return value: None
listchar:
	rts				; not implemented

; Function 6: Auxiliary output
;
; Entry parameters:
;	d0.w: $06
;	d1.w: Character
; Return value:
;	d0.w: Character
auxout:
	rts				; not implemented

; Function 5: Auxiliary input
;
; Entry parameters:
;	d0.w: $07
;	d1.w: Character
; Return value:
;	d0.w: Character
auxin:
	rts				; not implemented

; Function : Home
;
; Entry parameters:
;	d0.w: $08
; Return value: None
home:
	moveq	#0,d1
	bra	settrack

; Function 9: Select disk drive
;
; Entry parameters:
;	d0.w: $09
;	d1.b: Disk drive
;	d2.b: Logged in flag
; Return value:
;	d0.l: Address of selected drive's DPH
selectdrive:
	moveq	#0,d0			; no DPH
	cmp.b	#0,d1
	bcs	.exit
	cmp.b	#DRIVES,d1
	bcc	.exit
	and.w	#$f,d1
	move.w	d1,floppy_drive
	; TODO: logged in parameter handling
	move.l	#floppy_dph,d0
.exit
	rts

; Function 10: Set track number
;
; Entry parameters:
;	d0.w: $0a
;	d1.w: Disk track number
; Return value: None
settrack:
	cmp.w	#0,d1
	bcs	.exit
	cmp.w	#TRACKS,d1
	bcc	.exit
	move.w	d1,floppy_track
.exit:
	rts

; Function 11: Set logical sector number
;
; Entry parameters:
;	d0.w: $0b
;	d1.w: Logical sector number
; Return value: None
setsector:
	move.w	d1,d0
	bmi	.exit
	lsr.w	#2,d0		; divide by 4, one sector contains 4 CP/M logical sectors
	cmp.w	#SECTORS,d0
	bcc	.exit
	move.w	d0,floppy_sector
	move.w	d1,floppy_logical
.exit:
	rts

; copper list
copper:
	dc.w	$e0,0
	dc.w	$e2,0
	dc.w	$ffff,$fffe

color:	dc.w	$00f

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
floppy_track:
	dc.w	0
floppy_drive:
	dc.w	0
floppy_sector:
	dc.w	0
floppy_logical:
	dc.w	0
floppy_dph:
	dc.l	0			; xlt, no sector translation table
	dc.w	0			; scratch pad
	dc.w	0			; scratch pad
	dc.w	0			; scratch pad
	dc.l	floppy_dir_buffer	; address of directory buffer
	dc.l	floppy_dpb		; address of disk parameter block
	dc.l	floppy_csv		; address of checksum vector
	dc.l	floppy_alv		; address of scratchpad area
	
; floppy disk parameter block
floppy_dpb:
	dc.l	TRACKSIZE/128			; spt, 11*512/128=44, number of 128 byte logical sectors per track
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
floppy_csv:
	blk.b	DIRENTRIES/4,0
; floppy scratchpad area
floppy_alv:
	blk.b	TRACKS*TRACKSIZE/BLOCKSIZE/8

; strings
biosstring:
	dc.b	"CP/M-68k BIOS - Coded by Juha Ollila 2021",13,10,0
	even

; keymap (raw code to ascii)
keymap:
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

; screen buffer (requires 20kB)
screen:
	;blk.b	640*256*1/8, 0
