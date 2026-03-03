;======================================================================
;
;       SetCPU V1.60
;       by Dave Haynie, April 13, 1990
;       Released to the Public Domain
;
;       IDENTS.A MODULE
;
;       This module contains the functions that ID the CPU, MMU, and FPU
;       type indtalled in the system.
;
;	Modified by Juha Ollila for Amiga CP/M-68k, February 24, 2026
;
;======================================================================

; ATTN flags
AFB_68010	= 0	; also set for 68020
AFB_68020	= 1	; also set for 68030
AFB_68030	= 2	; also set for 68040
AFB_68040	= 3

CIB_ENABLE	= 0
CIB_BURST	= 4
CIB_ENABLE40	= 15

;======================================================================
;
;	This function returns the type of the CPU in the system as a
;	longword: 68000, 68010, 68020, or 68030.  The testing must be done
;	in reverse order, in that any higher CPU also has the bits set for
;	a lower CPU.  Also, since 1.3 doesn't recognize the 68030, if I
;	find the 68020 bit set, I always check for the presence of a 
;	68030.
;
;	This routine should be the first test routine called under 1.2
;	and 1.3.
;
;	Input parameters:
;	d0 = attn flags (read from exec base)
;	d1 = exec library version
;
;======================================================================

_GetCPUType:
	move.w	d0,flags
	move.w	d1,lib_version
	bsr	testflags		; Check extended CPU types
	cmp.l	#0,d0
	beq	.cpurealtest
	rts
.cpurealtest:
	btst.b	#AFB_68020,flags+1	; Maybe a 68020
	bne	.findreal32
	btst.b	#AFB_68010,flags+1	; Maybe a 68010?
	bne	.found10
	move.l	#68000,d0		; Just a measley '000
	rts
.found10:
	move.l	#68010,d0		; Yup, we're an '010
	rts
.findreal32:
	move.w	lib_version,d0		; Are we in 2.0?
	cmp.w	#36,d0			; If so, we don't need to test
	bge	.no40

	; we are already in supervisor state so we can call .supergct
	bsr	.supergct

	btst.l	#CIB_BURST,d0		; Do we have a set burst bit?
	beq	.no30
	move.l	#68030,d0		; It's a 68030
	bset.b	#AFB_68030,flags+1
	rts
.no30:	
	btst.l	#CIB_ENABLE40,d1	; Do we have 040 cache enable?
	beq	.no40
	move.l	#68040,d0		; It's a 68040
	bset.b	#AFB_68040,flags+1
	rts
.no40:
	move.l	#68020,d0		; Guess we're a plain old '020
	rts

	; This routine tries to set a few interesting CACR bits, and
	; returns the actual register value that took in d0.
.supergct:
	movec	cacr,d1			; Get the cache register
	move.l	d1,d0			; Make a copy
	bset.l	#CIB_BURST,d0		; Set the inst burst bit 030
	bclr.l	#CIB_ENABLE,d0		; Clear the inst cache bit 030
	bset.l	#CIB_ENABLE40,d0	; Set the inst cache bit 040
	movec	d0,cacr			; Try to set the CACR
	movec	cacr,d0			; Save the real value
	movec	d1,cacr			; Restore it
	rts

;======================================================================
;
;	This routine checks CPU flags early in ExecBase for extended
;	CPUs that test as a 68020 under 1.3.  If these flags are set,
;	the actual CPU/MMU type test can be skipped.
;
;======================================================================

testflags:
	moveq.l	#0,d0
	btst.b	#AFB_68040,flags+1	; Does the OS think an '040 is here?
	beq	.noearly40
	move.l	#68040,d0
	rts
.noearly40:
	btst.b	#AFB_68030,flags+1	; Does the OS think an '030 is here?
	beq	.noearly30
	move.l	#68030,d0		; Sure does...
.noearly30:
	rts

flags:
	dc.w	0
lib_version:
	dc.w	0



