; Amiga CP/M-68k Set Keymap Program
;
; Copyright (c) 2026 Juha Ollila
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

SECTOR_SIZE		= 128

XBIOS_SET_KEYMAP	= 8

BDOS_CONSOLE_INPUT	= 1
BDOS_CONSOLE_OUTPUT	= 2
BDOS_PRINT_STRING	= 9

	org		$500-$1c	; TEXT_START - (HEADER_END-HEADER_START)

HEADER_START:
	dc.w	$601a			; text, data, and bss are contiguous
	dc.l	TEXT_END-TEXT_START	; number of bytes in text segment
	dc.l	DATA_END-DATA_START	; number of bytes in data segment
	dc.l	0			; number of bytes in bss segment
	dc.l	0			; number of bytes in symbol table
	dc.l	0			; reserved
	dc.l	TEXT_START		; beginning of text segment
	dc.w	$ffff			; no relocation bits
HEADER_END:

TEXT_START:
	; print title
	move.l	#title_str,d1
	bsr	print_string


	; print keymap choises
	lea	keymap_table(pc),a2
.loop:
	move.l	(a2),d1
	beq	.select
	bsr	print_string
	addq.l	#8,a2
	bne	.loop

	; select keymap
.select:
	move.l	#choose_str,d1
	bsr	print_string
	bsr	get_char
	move.w	d0,d2			; save selection to d2
	sub.w	#'1',d2
	cmp.w	#0,d2
	bcs	.select
	cmp.w	#4,d2
	bcc	.select
	bsr	print_cr

	; xbios set keymap
	lsl.w	#3,d2			; multiply by 8
	and.l	#$ffff,d2
	move.l	#(keymap_table+4),a0
	add.l	d2,a0
	move.l	(a0),d1
	move.w	#XBIOS_SET_KEYMAP,d0
	trap	#4

	move.l	#done_str,d1
	bsr	print_string
.exit:
	rts

; print string calling bdos
; d1 = address to string
print_string:
	move.w	#BDOS_PRINT_STRING,d0
	trap	#2
	rts

print_cr:
	move.w	#cr_str,d1
	jmp	print_string

; get character using BDOS direct console I/O
; and converts it to upper case
;
; returns:
; d0: upper case char
get_char:
	move.w	#BDOS_CONSOLE_INPUT,d0
	trap	#2
	rts

; put character using BDOS direct console I/O
; d1 = charater to be printed
put_char:
	move.w	#BDOS_CONSOLE_OUTPUT,d0
	trap	#2
	rts

TEXT_END:

DATA_START:

keymap_table:
	dc.l	usa_title, usa_map
	dc.l	uk_title, uk_map
	dc.l	fi_title, fi_map
	dc.l	de_title, de_map
	dc.l	0,0

usa_title:
	dc.b	"1.) American (USA) keymap",13,10,"$"
uk_title:
	dc.b	"2.) British (UK) kaymap",13,10,"$"
fi_title:
	dc.b	"3.) Finnish/Swedish (FI/SE) keymap",13,10,"$"
de_title:
	dc.b	"4.) German (DE) keymap",13,10,"$"
	even

usa_map:
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
uk_map:
	dc.b	"`~","1!","2",$22,"3",$A3,"4$","5%","6^","7&"
	dc.b	"8*","9(","0)","-_","=+","\|",0,0,"00"
	dc.b	"qQ","wW","eE","rR","tT","yY","uU","iI"
	dc.b	"oO","pP","[{","]}",0,0,"11","22","33"
	dc.b	"aA","sS","dD","fF","gG","hH","jJ","kK"
	dc.b	"lL",";:","#@","'",0,0,0,"44","55","66"
	dc.b	0,0,"zZ","xX","cC","vV","bB","nN","mM"
	dc.b	",<",".>","/?",0,0,"..","77","88","99"
	dc.b	"  ",$8,$8,$9,$9,$d,$d,$d,$d,$1b,$1b,$7f,$7f,0,0
	dc.b	0,0,0,0,"--",0,0,$b,$b,$a,$a,$c,$c,$8,$8
	dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dc.b	0,0,0,0,"((","))","//","**","++",0,0
fi_map:
	dc.b	"`~","1!","2",$22,"3&","4$","5%","6&","7/"
	dc.b	"8(","9)","0=","+?",0,0,"\|",0,0,"00"
	dc.b	"qQ","wW","eE","rR","tT","yY","uU","iI"
	dc.b	"oO","pP",$e5,$c5,0,"^",0,0,"11","22","33"
	dc.b	"aA","sS","dD","fF","gG","hH","jJ","kK"
	dc.b	"lL",$f6,$d6,$e4,$c4,"'*",0,0,"44","55","66"
	dc.b	"<>","zZ","xX","cC","vV","bB","nN","mM"
	dc.b	",;",".:","-_",0,0,"..","77","88","99"
	dc.b	"  ",$8,$8,$9,$9,$d,$d,$d,$d,$1b,$1b,$7f,$7f,0,0
	dc.b	0,0,0,0,"--",0,0,$b,$b,$a,$a,$c,$c,$8,$8
	dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dc.b	0,0,0,0,"[{","]}","//","**","++",0,0
de_map:
	dc.b	"`~","1!","2",$22,"3",$a7,"4$","5%","6&","7/"
	dc.b	"8(","9)","0=",$df,"?",0,0,"\|",0,0,"00"
	dc.b	"qQ","wW","eE","rR","tT","zZ","uU","iI"
	dc.b	"oO","pP",$fc,$dc,"+*",0,0,"11","22","33"
	dc.b	"aA","sS","dD","fF","gG","hH","jJ","kK"
	dc.b	"lL",$f6,$d6,$e4,$c4,"^#",0,0,"44","55","66"
	dc.b	"<>","yY","xX","cC","vV","bB","nN","mM"
	dc.b	",;",".:","-_",0,0,"..","77","88","99"
	dc.b	"  ",$8,$8,$9,$9,$d,$d,$d,$d,$1b,$1b,$7f,$7f,0,0
	dc.b	0,0,0,0,"--",0,0,$b,$b,$a,$a,$c,$c,$8,$8
	dc.b	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	dc.b	0,0,0,0,"[{","]}","//","**","++",0,0
title_str:
	dc.b	13,10,"Amiga CP/M-68k Set Keymap, Feb 25 2026",13,10,13,10,"$"
choose_str:
	dc.b	13,10,"Please choose keymap (1-4): $"
done_str:
	dc.b	"New keymap was taken into use.",13,10,"$"
cr_str:
	dc.b	13,10,"$"
	even
	
DATA_END:

END:
	dcb.b	SECTOR_SIZE-((END-HEADER_START)%SECTOR_SIZE)	; padding


