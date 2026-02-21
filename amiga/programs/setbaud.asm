; Amiga CP/M-68k Set Serial Parameters Program
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

SECTOR_SIZE		= 128

XBIOS_GET_BAUD_RATE	= $1
XBIOS_SET_BAUD_RATE	= $2
XBIOS_GET_FLOW_CONTROL	= $3
XBIOS_SET_FLOW_CONTROL	= $4

BDOS_SYSTEM_RESET	= 0
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

	; show current configuration
	bsr	get_configuration
	bsr	print_configuration

	; ask if settings should be changed
	move.l	#change_str,d1
	bsr	print_string
	bsr     get_char
	and.w	#%11011111,d0		; change to upper case
	cmp.b   #'Y',d0
	bne    .exit			; go to exit if settings are not changed

	; print supported baud rates and ask baud rate selection
	bsr	print_baud_rates
.select1:
	move.l	#select_str,d1		; select baud rate
	bsr	print_string
	bsr	get_char
	move.w	d0,d2			; save selection to d2
	sub.w	#'1',d2
	cmp.w	#0,d2
	bcs	.select1
	cmp.w	#9,d2
	bcc	.select1
	bsr	print_cr

.select2:
	move.l	#change_flow_str,d1	; enable/disable rts/cts flow control
	bsr	print_string
	bsr	get_char
	and.w	#%11011111,d0		; change to upper case
	cmp.b   #'Y',d0
	beq	.flow_enabled
	cmp.b	#'N',d0
	bne	.select2
	move.w	#0,d5
	bra	.update
.flow_enabled:
	move.w	#1,d5

	; update configuration
.update:
	bsr	put_configuration

	; show current configuration
	bsr	get_configuration
	bsr	print_configuration
.exit:
	rts


get_configuration:
	move.w	#XBIOS_GET_FLOW_CONTROL,d0
	trap	#4
	move.w	d0,d4			; save RTS/CTS
	move.w	#XBIOS_GET_BAUD_RATE,d0
	trap	#4
	rts

; set configuration to XBIOS
; Entry parameters:
; d2 = baud rate index
; d5 = flow control, 1=Yes, 0=No
put_configuration:
	lsl	#1,d2			; multiply by 2
	lea	baud_rates,a0
	move.w	(a0,d2.w),d1		; d1 = baud rate
	move.w	#8,d2			; d2 = data bits (8)
	move.w	#0,d3			; d3 = parity (none)
	move.w	#1,d4			; d4 = stop bits (1)
	move.w	#XBIOS_SET_BAUD_RATE,d0
	trap	#4
	cmp.w	#0,d0
	bne	.error
	move.w	d5,d1			; CTS/RTS -> d1
	move.w	#XBIOS_SET_FLOW_CONTROL,d0
	trap	#4
	beq	.exit
.error:
	move.l	#configuration_error_str,d1
	bsr		print_string
.exit:
	rts

; print configuration
; Entry parameters:
; d0 = baud rate (int)
; d4 = flow control
print_configuration:
	move.w	d0,d3			; baud rate
	move.l	#current_str,d1
	bsr	print_string
	move.l	#data_str,d1
	bsr	print_string
	move.l	#baud_str,d1
	bsr	print_string
	move.w	#0,d0
	lea	baud_rates,a0
	lea	baud_rate_strings-4,a1
.find_baud_str:
	lea	4(a1),a1		; just increment address
	cmp.w	#0,(a0)			; check if all baud rates were already checked
	beq	.error
	cmp.w	(a0)+,d3		; check if baud rate matches
	bne	.find_baud_str
	move.l	(a1),d1
	bra	.parity
.error:
	move.l	unsupported_str,d1
.parity:
	bsr	print_string
	bsr	print_cr
	move.l	#parity_str,d1
	bsr	print_string
	move.l	#stop_str,d1
	bsr	print_string
	move.l	#flow_str,d1
	bsr	print_string
	cmp.w	#1,d4
	bne	.printno
	move.l	#yes_str,d1
	bsr	print_string
	rts
.printno:
	move.l	#no_str,d1
	bsr	print_string
	rts

print_baud_rates:
	bsr	print_cr
	bsr	print_cr
	lea	baud_rate_strings-4,a2
	move.w	#0,d4			; d4 = option number
.loop:
	lea	4(a2),a2
	move.l	(a2),d5			; d5 = baud string
	cmp.l	#0,d5
	beq	.exit
	addq.w	#1,d4
	move.w	d4,d1
	add.w	#'0',d1
	bsr	put_char
	move.l	#option_str,d1
	bsr	print_string
	move.l	d5,d1
	bsr	print_string
	bsr	print_cr
	bra	.loop
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
title_str:
	dc.b 13,10,"Amiga CP/M-68k Set Serial Parameters, Dec 15 2025$"
current_str:
	dc.b 13,10,13,10,"Current configuration:",13,10,"$"
data_str:
	dc.b "Data bits:   8",13,10,"$"
baud_str:
	dc.b "Baud rate:   $"
parity_str:
	dc.b "Parity bit:  None",13,10,"$"
stop_str:
	dc.b "Stop bits:   1",13,10,"$"
flow_str:
	dc.b "RTS/CTS:     $"
change_str:
	dc.b 13,10,"Do you want to change configuration? $"
supported_str:
	dc.b 13,10,"Supported baud rates are:",13,10,"$"
select_str:
	dc.b 13,10,"Please select baud rate (1-9): $"
change_flow_str:
	dc.b 13,10,"Do you want RTS/CTS flow control (y/n): $"
configuration_error_str:
	dc.b 13,10,"Configuration failed",13,10,"$"
cr_str:
	dc.b 13,10,"$"
yes_str:
	dc.b "Yes",13,10,"$"
no_str:
	dc.b "No",13,10,"$"
option_str:
	dc.b ".) $"
unsupported_str:
	dc.b "UNSUPPORTED$"
	even
baud_rates:
	dc.w 300, 1200, 2400, 4800, 9600, 14400, 19200, 38400, 57600, 0
baud_rate_strings:
	dc.l baud_300, baud_1200, baud_2400, baud_4800, baud_9600
	dc.l baud_14400, baud_19200, baud_38400, baud_57600, 0
baud_300:
	dc.b "300$"
baud_1200:
	dc.b "1200$"
baud_2400:
	dc.b "2400$"
baud_4800:
	dc.b "4800$"
baud_9600:
	dc.b "9600$"
baud_14400:
	dc.b "14400$"
baud_19200:
	dc.b "19200$"
baud_38400:
	dc.b "38400$"
baud_57600:
	dc.b "57600$"
	even
DATA_END:

END:
	dcb.b	SECTOR_SIZE-((END-HEADER_START)%SECTOR_SIZE)	; padding

