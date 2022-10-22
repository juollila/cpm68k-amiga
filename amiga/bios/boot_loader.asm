; Boot loader for SturmBIOS and CP/M-68k
;

LOAD_ADDRESS 	= $60000
BOOT_DRIVE	= $66002

; library call offsets
SUPERSTATE	= -150
OPENDEVICE	= -444
DOIO		= -456
; standard io request offsets
IO_MESSAGE	= 0
IO_DEVICE	= 20
IO_UNIT		= 24
IO_COMMAND	= 28
IO_FLAGS	= 30
IO_ERROR	= 31
IO_ACTUAL	= 32
IO_LENGTH	= 36
IO_DATA		= 40
IO_OFFSET	= 44
; IO commands
CMD_INVALID	= 0
CMD_RESET	= 1
CMD_READ	= 2
CMD_WRITE	= 3
CMD_UPDATE	= 4
CMD_CLEAR	= 5
CMD_STOP	= 6
CMD_START	= 7
CMD_FLUSH	= 8
CMD_NONSTD	= 9
; message structure offsets
MN_REPLYPORT	= 14
MN_LENGTH	= 18

; trackdisk device commands
TD_MOTOR	= CMD_NONSTD+0
TD_SEEK		= CMD_NONSTD+1
TD_FORMAT	= CMD_NONSTD+2
TD_REMOVE	= CMD_NONSTD+3
TD_CHANGENUM	= CMD_NONSTD+4
TD_CHANGESTATE	= CMD_NONSTD+5
TD_PROTSTATUS	= CMD_NONSTD+6
TD_RAWREAD	= CMD_NONSTD+7
TD_RAWWRITE	= CMD_NONSTD+8
TD_GETDRIVETYPE	= CMD_NONSTD+9
TD_GETNUMTRACKS	= CMD_NONSTD+10
TD_ADDCHANGEINT	= CMD_NONSTD+11
TD_REMCHANGEINT	= CMD_NONSTD+12
TD_GETGEOMETRY	= CMD_NONSTD+13
TD_EJECT	= CMD_NONSTD+14
TD_LASTCOMM	= CMD_NONSTD+15

; other constants
TD_SECTOR	= 512		; sector size

	org	0		; boot block contain position independent code

boot_block:
	dc.b	"DOS", 0	; magic string
	dc.l	0		; checksum
	dc.l	880

; a6 = SysBase
; a1 = trackdisk IoStdReq
start:
	move.l	a1,a5		; IoStdReq
	; check boot drive
	moveq	#0,d6		; df0
	bsr	open_trackdisk
	tst.l	d0
	bne	notdf0
	lea	io_std_req(pc),a0
	move.l	IO_UNIT(a0),d0
	cmp.l	IO_UNIT(a5),d0
	beq	found
notdf0:
	moveq	#1,d6		; df1
	bsr	open_trackdisk
	tst.l	d0
	bne	notdf1
	lea	io_std_req(pc),a0
	move.l	IO_UNIT(a0),d0
	cmp.l	IO_UNIT(a5),d0
	beq	found
notdf1:
	jmp	notdf1		; at the moment only df0 and df1 are supported
found:
	move.l	a5,a1
	; load bios + ccp + bdos
	lea	LOAD_ADDRESS,a2
	move.l	#cpm_end-cpm_start,IO_LENGTH(a1)
	move.l	a2,IO_DATA(a1)
	move.l	#cpm_start-boot_block,IO_OFFSET(a1)
	jsr	DOIO(a6)

	; turn off motor
	move.l	a5,a1
	move.w	#TD_MOTOR,IO_COMMAND(a1)
	clr.l	IO_LENGTH(a1)
	jsr	DOIO(a6)

	; change to supervisor state
	jsr	SUPERSTATE(a6)
	
	; start cp/m
	move.w	d6,BOOT_DRIVE
	jmp	(a2)

; open trackdisk
; d6 = unit number
open_trackdisk:
	lea	trackdisk_name(pc),a0
	lea	io_std_req(pc),a1
	; set reply port address
	lea	port(pc),a2
	move.l	a2,reply_port-io_std_req(a1)
	lea	reply_port(pc),a2
	move.l	a2,MN_REPLYPORT(a1)
	; try to open device
	move.l	d6,d0		; device number
	moveq	#0,d1		; flags
	jsr	OPENDEVICE(a6)
	rts

io_std_req:	ds.b	56
reply_port:	dc.l	0
port:		ds.b	100
trackdisk_name:
		dc.b	"trackdisk.device",0

	rorg	2*TD_SECTOR

cpm_start:
	incbin	"cpm.bin"
	cnop	0,TD_SECTOR
cpm_end:
