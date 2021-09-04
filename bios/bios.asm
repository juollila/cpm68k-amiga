	code_c

; Function 0: Initialization
; Entry parameters:
;	d0.w: $00
; Return value: d0.w: user/disk numbers
_init:
	lea	$dff000,a6
	move.w	#$7fff,$9a(a6)		; disable interrupts (intena)
	move.w	#$7fff,$9c(a6)		; clear interrupts (intreq)
	move.w	#$7fff,$96(a6)		; disable dma (dmacon)
	move.w	#$9200,$100(a6)		; bplcon0: hires, one bit plane, color enabled
	move.w	#0,$102(a6)		; bplcon1: horizontal scroll value = 0
	move.w	#0,$108(a6)		; bplmod1: set modulo 0 for all odd bit planes
	move.w	#0,$10a(a6)		; bplmod2: set modulo 0 for all even bit planes
	move.w	#$3c,$92(a6)		; ddfstrt: data fetch start for hires
	move.w	#$d4,$94(a6)		; ddfstop: data fetch stop
	move.w	#$2c81,$8e(a6)		; diwstrt: display window start
	move.w	#$2cc1,$90(a6)		; diwstop: display window stop
	move.w	#$ccc,$180(a6)		; color0: light gray
	move.w	#$000,$182(a6)		; color1: black

	move.l	#screen,d0		; set copper list
	move.w	d0,copper+6
	swap	d0
	move.w	d0,copper+2

	move.l	#copper,$80(a6)		; set copper list address
	move.w	$88(a6),d0		; jump to copper 1 list
	move.w	#$83c0,$96(a6)		; dmacon: enable bit-plane, blitter and copper dma

	lea	text,a2
.loop:
	move.b	(a2)+,d1
	beq	.skip
	bsr	conout
	bra	.loop
.skip:
	jmp	*
text:	;dc.b	"Hello, World!",13,"Does this work?",13
	;dc.b	"Very long line:",13
	;dc.b    "1234567890123456789012345678901234567890123456789012345678901234567890"
	;dc.b    "1234567890123456789012345678901234567890123456789012345678901234567890"
	;dc.b    "1234567890123456789012345678901234567890123456789012345678901234567890",$1a,0
	dc.b	"	code_c",13,13
	dc.b	"; Function 0: Initialization",13
	dc.b	"; Entry parameters:",13
	dc.b	";	d0.w: $00",13
	dc.b	"; Return value: d0.w: user/disk numbers",13
	dc.b	"_init:",13
	dc.b	"	lea	$dff000,a6",13
	dc.b	"	move.w	#$7fff,$9a(a6)		; disable interrupts (intena)",13
	dc.b	"	move.w	#$7fff,$9c(a6)		; clear interrupts (intreq)",13
	dc.b	"	move.w	#$7fff,$96(a6)		; disable dma (dmacon)",13
	dc.b	"	move.w	#$9200,$100(a6)		; bplcon0: hires, one bit plane, color enabled",13
	dc.b	"	move.w	#0,$102(a6)		; bplcon1: horizontal scroll value = 0",13
	dc.b	"	move.w	#0,$108(a6)		; bplmod1: set modulo 0 for all odd bit planes",13
	dc.b	"	move.w	#0,$10a(a6)		; bplmod2: set modulo 0 for all even bit planes",13
	dc.b	"	move.w	#$3c,$92(a6)		; ddfstrt: data fetch start for hires",13
	dc.b	"	move.w	#$d4,$94(a6)		; ddfstop: data fetch stop",13
	dc.b	"	move.w	#$2c81,$8e(a6)		; diwstrt: display window start",13
	dc.b	"	move.w	#$2cc1,$90(a6)		; diwstop: display window stop",13
	dc.b	"	move.w	#$ccc,$180(a6)		; color0: light gray",13
	dc.b	"	move.w	#$000,$182(a6)		; color1: black",13,13

	dc.b	"	move.l	#screen,d0		; set copper list",13,13
	dc.b	"	move.w	d0,copper+6",13
	dc.b	"	swap	d0",13
	dc.b	"	move.w	d0,copper+2",13

	dc.b	"	move.l	#copper,$80(a6)		; set copper list address",13
	dc.b	"	move.w	$88(a6),d0		; jump to copper 1 list",13
	dc.b	"	move.w	#$83c0,$96(a6)		; dmacon: enable bit-plane, blitter and copper dma",13,13

	dc.b	"	lea	text,a2",13
	dc.b	".loop:",13
	dc.b	"	move.b	(a2)+,d1",13
	dc.b	"	beq	.skip",13
	dc.b	"	bsr	conout",13
	dc.b	"	bra	.loop",13
	dc.b	".skip:",13
	dc.b	"	jmp	*",13

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
; Entry parameters:
;	d0.w: $01
; Return value: None
warmboot:
	; jmp	_ccp

; Function 4: Write console character
; Entry parameters:
;	d0.w: $04
;	d1.w: Character
; Return value: None

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
conout:
	;move.b	#0,.row		; check if printable character
	;move.b	#0,.column
	and.l	#$000000ff,d1
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
	beq	.eol
	move.w	d0,.column
	rts
	; handle end of line
.eol:	move.w	#0,.column
	move.w	.row,d0
	addq	#1,d0
	cmp.b	#32,d0
	beq	.scroll
	move.w	d0,.row
	rts
.scroll:
	jsr	waitblit
	move.w	#31,.row
	move.l	#screen,d0
	move.l	d0,$54(a0)	; destination address (bltdpth)
	add.l	#8*80,d0
	move.l	d0,$50(a0)	; source address (bltapth)
	move.w	#0,$64(a0)	; zero modulo (bltamod)
	move.w	#0,$66(a0)	; zero modulo (bltdmod)
	move.w	#$ffff,$44(a0)	; first word mask (bltafwm)
	move.w	#$ffff,$46(a0)	; last word mask (bltalwm)
	move.w	#0,$42(a0)	; bltcon1
	move.w	#$9f0,$40(a0)	; use A&D, minterm = A (bltcon0)
	move.w	#((248<<6)|(80/2)),$58(a0)	; height = 248, width = 40 words
	jsr	waitblit
	move.l	#screen,d0
	add.l	#248*80,d0
	move.l	d0,$54(a0)	; destination address (bltdpth)
	move.w	#0,$66(a0)	; zero modulo (bltdmod)
	move.w	#0,$42(a0)	; bltcon1
	move.w	#$100,$40(a0)	; use D, minterm = none (bltcon0)
	move.w	#((8<<6)|(80/2)),$58(a0)	; height = 8, width = 40 words
	jsr	waitblit
	;move.w	#$ffff,screen
	rts
.cls:
	jsr	waitblit
	move.l	#screen,$54(a0)	; destination address (bltdpth)
	move.w	#0,$66(a0)	; zero modulo (bltdmod)
	move.w	#0,$42(a0)	; bltcon1
	move.w	#$100,$40(a0)	; use D, minterm = none (bltcon0)
	move.w	#((256<<6)|(80/2)),$58(a0)	; height = 256, width = 40 words
.waitmouse:
	btst	#6,$bfe001
	bne	.waitmouse
	move.w	#$ffff,screen
	rts
.tab:
	move.w	.column,d0
	and.w	#$f8,d0
	add.w	#8,d0
	cmp.w	#80,d0
	bcc	.eol
	move.w	d0,.column
	rts
.backspace:
	move.w	.column,d0
	sub.w	#1,d0
	bmi	.backspace1
	move.w	d0,.column
.backspace1:
	rts
.formfeed:
	move.w	.column,d0
	add.w	#1,d0
	cmp.w	#80,d0
	bcc	.formfeed1
	move.w	d0,.column
.formfeed1:
	rts
.controlch:
	cmp.b	#$0d,d1		; cr
	beq	.eol
	cmp.b	#$1a,d1		; home / clear screen
	beq	.cls
	cmp.b	#9,d1		; tab
	beq	.tab
	cmp.b	#8,d1		; backspace / ctrl+h
	beq	.backspace
	cmp.b	#$c,d1		; form feed / ctrl+l
	beq	.formfeed
	; TODO rest of control characters
	rts


	even
.row:		dc.w	0
.column:	dc.w	0



copper:
	dc.w	$e0,0
	dc.w	$e2,0
	dc.w	$ffff,$fffe
screen:
	blk.b	640*256*1/8, 0
font:
	incbin ../font/topaz128x112x1.raw
