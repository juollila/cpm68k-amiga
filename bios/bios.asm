	code_c
start:
	lea	$dff000,a6
	move.w	#$7fff,$9a(a6)		; disable interrupts (intena)
	move.w	#$7fff,$9c(a6)		; clear interrupts (intreq)
	move.w	#$7fff,$96(a6)		; disable dma (dmacon)
	move.w	#$9200,$100(a6)		; bplcon0: hires, one bit plane, color enabled
	;move.w	#$1200,$100(a6)		; bplcon0: lores, one bit plane, color enabled
	move.w	#0,$102(a6)		; bplcon1: horizontal scroll value = 0
	move.w	#0,$108(a6)		; bplmod1: set modulo 0 for all odd bit planes
	move.w	#0,$10a(a6)		; bplmod2: set modulo 0 for all even bit planes
	move.w	#$3c,$92(a6)		; ddfstrt: data fetch start for hires
	;move.w	#$38,$92(a6)		; ddfstrt: data fetch start for lores
	move.w	#$d4,$94(a6)		; ddfstop: data fetch stop
	;move.w	#$d0,$94(a6)		; ddfstop: data fetch stop
	move.w	#$2c81,$8e(a6)		; diwstrt: display window start
	move.w	#$2cc1,$90(a6)		; diwstop: display window stop
	move.w	#$ccc,$180(a6)		; color0: light gray
	move.w	#$000,$182(a6)		; color1: black

	lea	font,a0			; copy font to the screen buffer
	lea	screen,a1
	moveq	#0,d0
loop:
	move.l	(a0)+,(a1)+
	move.l	(a0)+,(a1)+	
	move.l	(a0)+,(a1)+	
	move.l	(a0)+,(a1)+
	add.l	#64,a1
	;add.l	#24,a1
	addq	#1,d0
	cmp	#112,d0
	bne	loop

	move.l	#screen,d0		; set copper list
	move.w	d0,copper+6
	swap	d0
	move.w	d0,copper+2

	move.l	#copper,$80(a6)		; set copper list address
	move.w	$88(a6),d0		; jump to copper 1 list
	move.w	#$8380,$96(a6)		; dmacon: enable bit-plane and copper dma
	jmp	*

	data_c
copper:
	dc.w	$e0,0
	dc.w	$e2,0
	dc.w	$ffff,$fffe
screen:
	ds.b	640*256*1/8, 0
font:
	incbin ../font/topaz128x112x1.raw
