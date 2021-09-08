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
COP1LCH	= $080	; Copper pointer 1 (high)
COPJMP1	= $088	; Trigger copper 1 (any value)
DIWSTRT	= $08e	; Display window start
DIWSTOP = $090	; Display window stop
DDFSTRT	= $092	; Display bitplane data fetch start
DDFSTOP	= $094	; Display bitplane data fetch stop
DMACON	= $096	; DMA control write
INTENA	= $09a	; Interrupt enable bits
INTREQ	= $09c	; Interrupt request bits
BPL1PTH	= $0e0	; Bitplane pointer 1 (high)
BPLCON0	= $100	; Bitplane depth and screen mode
BPLCON1	= $102	; Bitplane/playfield horizontal scroll values
BPL1MOD	= $108	; Bitplane modulo (odd planes)
COLOR0	= $180	; Color 0
COLOR1	= $182	; Color 1

; Constants
COLS	= 80
ROWS	= 32
WIDTH	= 640
HEIGHT	= 256

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
	move.w	#$7fff,INTENA(a6)	; disable interrupts
	move.w	#$7fff,INTREQ(a6)	; clear interrupts
	move.w	#$7fff,DMACON(a6)	; disable dma (dmacon)
	move.w	#$9200,BPLCON0(a6)	; hires, one bit plane, color enabled
	move.w	#0,BPLCON1(a6)		; horizontal scroll value = 0
	move.w	#0,BPL1MOD(a6)		; set modulo 0 for all odd bit planes
	;move.w	#0,$10a(a6)		; bplmod2: set modulo 0 for all even bit planes
	move.w	#$3c,DDFSTRT(a6)	; data fetch start for hires
	move.w	#$d4,DDFSTOP(a6)	; data fetch stop
	move.w	#$2c81,DIWSTRT(a6)	; display window start
	move.w	#$2cc1,DIWSTOP(a6)	; display window stop
	move.w	#$ccc,COLOR0(a6)	; color0: light gray
	move.w	#$000,COLOR1(a6)	; color1: black

	move.l	#screen,d0		; copy screen address to copper list
	move.w	d0,copper+6
	swap	d0
	move.w	d0,copper+2

	move.l	#copper,COP1LCH(a6)	; set copper1 list address
	clr.w	COPJMP1(a6)		; jump to copper1 list
	move.w	#$83c0,DMACON(a6)	; dmacon: enable bit-plane, blitter and copper dma

	lea	CIAA,a0
	move.b	#0,CRA(a0)		; disable timer A
	move.b	#0,CRB(a0)		; disable timer B
	move.b	#$03,DDRA(a0)		; two low bits are output in port a of CIAA
	move.b	#$7f,ICR(a0)		; clear all interrupts in CIAA
	move.b	#$88,ICR(a0)		; enable serial port interrupt in CIAA

	move.b	#$7f,CIAB+ICR		; clear all interrupts in CIAB
	
	move.l	#keyboard_int,$68.w	; keyboard interrupt vector to level 2 autovector
	move.w	#$c008,INTENA(a6)	; enable CIA interrupts
	

	lea	text,a2
.loop:
	move.b	(a2)+,d1
	beq	.skip
	bsr	conout
	bra	.loop
.skip:
	jmp	*
text:	;dc.b	"Hello, World!",13,10,"Does this work?",13,10
	;dc.b	"Very long line:",13,10
	;dc.b    "1234567890123456789012345678901234567890123456789012345678901234567890"
	;dc.b    "1234567890123456789012345678901234567890123456789012345678901234567890"
	;dc.b    "1234567890123456789012345678901234567890123456789012345678901234567890",$1a,0
	dc.b	"	code_c",13,10,13,10
	dc.b	"; Function 0: Initialization",13,10
	dc.b	"; Entry parameters:",13,10
	dc.b	";	d0.w: $00",13,10
	dc.b	"; Return value: d0.w: user/disk numbers",13,10
	dc.b	"_init:",13,10
	dc.b	"	lea	$dff000,a6",13,10
	dc.b	"	move.w	#$7fff,$9a(a6)		; disable interrupts (intena)",13,10
	dc.b	"	move.w	#$7fff,$9c(a6)		; clear interrupts (intreq)",13,10
	dc.b	"	move.w	#$7fff,$96(a6)		; disable dma (dmacon)",13,10
	dc.b	"	move.w	#$9200,$100(a6)		; bplcon0: hires, one bit plane, color enabled",13,10
	dc.b	"	move.w	#0,$102(a6)		; bplcon1: horizontal scroll value = 0",13,10
	dc.b	"	move.w	#0,$108(a6)		; bplmod1: set modulo 0 for all odd bit planes",13,10
	dc.b	"	move.w	#0,$10a(a6)		; bplmod2: set modulo 0 for all even bit planes",13,10
	dc.b	"	move.w	#$3c,$92(a6)		; ddfstrt: data fetch start for hires",13,10
	dc.b	"	move.w	#$d4,$94(a6)		; ddfstop: data fetch stop",13,10
	dc.b	"	move.w	#$2c81,$8e(a6)		; diwstrt: display window start",13,10
	dc.b	"	move.w	#$2cc1,$90(a6)		; diwstop: display window stop",13,10
	dc.b	"	move.w	#$ccc,$180(a6)		; color0: light gray",13,10
	dc.b	"	move.w	#$000,$182(a6)		; color1: black",13,10,13,10

	dc.b	"	move.l	#screen,d0		; set copper list",13,10,13,10
	dc.b	"	move.w	d0,copper+6",13,10
	dc.b	"	swap	d0",13,10
	dc.b	"	move.w	d0,copper+2",13,10

	dc.b	"	move.l	#copper,$80(a6)		; set copper list address",13,10
	dc.b	"	move.w	$88(a6),d0		; jump to copper 1 list",13,10
	dc.b	"	move.w	#$83c0,$96(a6)		; dmacon: enable bit-plane, blitter and copper dma",13,10,13,10

	dc.b	"	lea	text,a2",13,10
	dc.b	".loop:",13,10
	dc.b	"	move.b	(a2)+,d1",13,10
	dc.b	"	beq	.skip",13,10
	dc.b	"	bsr	conout",13,10
	dc.b	"	bra	.loop",13,10
	dc.b	".skip:",13,10
	dc.b	"	jmp	*",13,10
	dc.b	$1b,"=  ","testing",0
;	dc.b	"yksi",8,8,8,8,8,"kaksi",11,"kolme",13,"nelja",11,"viisi",0

;	dc.b	"1",13,"2",13,"3",13,"4",13,"5",13,"6",13,"7",13,"8",13,"9",13,"10",13
;	dc.b	"11",13,"12",13,"13",13,"14",13,"15",13,"16",13,"17",13,"18",13,"19",13,"20",13
;	dc.b	"21",13,"22",13,"23",13,"24",13,"25",13,"26",13,"27",13,"28",13,"29",13,"30",13
;	dc.b	"31",13,"32",13,"33",13,"34",13,"35",13,"36",13,"37",13,"38",13,"39",13,"40",13,0
	even



	; TODO: setting up trap handler, internal variables (e.g. iobyte)
	; move.l	#traphandler,$8c	; set trap 3 handler
	clr.l	d0			; login drive A, user 0
	rts

waitblit:
	lea	$dff000,a0
.waitblit0:
	btst	#14,$2(a0)	; wait blit to complete (dmaconr)
	bne	.waitblit0
	rts 

; Function 1: Warm boot
;
; Entry parameters:
;	d0.w: $01
; Return value: None
warmboot:
	; jmp	_ccp

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
	jsr	waitblit
	move.l	#screen,BLTDPTH(a0)		; destination address (bltdpth)
	move.w	#0,BLTDMOD(a0)			; zero modulo (bltdmod)
	move.w	#0,BLTCON1(a0)			; bltcon1
	move.w	#$100,BLTCON0(a0)		; use D, minterm = none (bltcon0)
	move.w	#((256<<6)|(80/2)),BLTSIZE(a0)	; height = 256, width = 40 words
;.waitmouse:
;	btst	#6,$bfe001
;	bne	.waitmouse
;	move.w	#$ffff,screen
	rts

	; scroll screen buffer
.scroll:
	jsr	waitblit
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
	jsr	waitblit
	move.l	#screen,d0
	add.l	#248*80,d0
	move.l	d0,BLTDPTH(a0)			; destination address (bltdpth)
	move.w	#0,BLTDMOD(a0)			; zero modulo (bltdmod)
	move.w	#0,BLTCON1(a0)			; bltcon1
	move.w	#$100,BLTCON0(a0)		; use D, minterm = none (bltcon0)
	move.w	#((8<<6)|(80/2)),BLTSIZE(a0)	; height = 8, width = 40 words
	jsr	waitblit
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
	movem.l	d0/a0-a1,-(sp)
	lea	CUSTOM,a0
	eor.w	#$fff,color
	move.w	color,COLOR1(a0)
	;move.w	#$00f,COLOR1(a0)
	; TODO: read scan code from SDR, convert to ASCII and save
	lea	CIAA,a1
	move.b	ICR(a1),d0		; clear interrupt in CIAA

	; start keyboard ack
	move.b	#$48,CRA(a1)		; Enable timer A, serial port output
	move.b	#$71,TALO(a1)		; requirement: min 75 microseconds, but 100 microseconds used
	move.b	#$00,TAHI(a1)		; 709379 MHz PAL * 0.000100 = 71
.wait:	btst.b	#0,ICR(a1)		; check timer A interrupt
	bne	.wait
	move.b	#0,CRA(a1)		; serial port input
	
	move.w	#$0008,INTREQ(a0)	; clear CIA interrupt in Paula
	movem.l (sp)+,d0/a0-a1
	rte

copper:
	dc.w	$e0,0
	dc.w	$e2,0
	dc.w	$ffff,$fffe
color:	dc.w	$00f
screen:
	blk.b	640*256*1/8, 0
font:
	incbin ../font/topaz128x112x1.raw
