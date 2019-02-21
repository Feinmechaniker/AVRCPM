;    RAM disk driver
;
;    Copyright (C) 2010 Leo C.
;
;    This file is part of avrcpm.
;
;    avrcpm is free software: you can redistribute it and/or modify it
;    under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    avrcpm is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with avrcpm.  If not, see <http://www.gnu.org/licenses/>.
;
;    $Id: dsk_ram.asm 93 2014-01-03 16:32:32Z rapid $
;

#ifndef RAMDISKCNT
	#define RAMDISKCNT 0	
#endif

#if RAMDISKCNT

#if DRAM_8BIT

; -------------- RAM disk (8-bit DRAM) --------------

	.dseg
rdskbuf:
	.byte	32
	
	.cseg
; ----------------------------------------------

rdsk_adr:
	ldi	zl,0
	lds	zh,seeksec
	lds	temp2,seektrk
	lsr	zh
	ror	zl			;Col 0..7
	
	mov	temp,temp2
	andi	temp,0x0f
	swap	temp
	or	zh,temp			;Row  0..7

	ldi	yl,~((1<<RAM_A10)|(1<<RAM_A9)|(1<<RAM_A8)) ;Col  8..10
	mov	yh,yl			;Row  8..10
	lds	temp,seekdsk
	subi	temp,RAMDISKNR
	
	sbrc	temp2,4
	 sbr	yl,(1<<RAM_A8)
	sbrc	temp2,5
	 sbr	yh,(1<<RAM_A8)
	sbrc	temp2,6
	 sbr	yl,(1<<RAM_A9)
	sbrc	temp2,7
	 sbr	yh,(1<<RAM_A9)
	sbrc	temp,0
	 sbr	yl,(1<<RAM_A10)
	sbrc	temp,1
	 sbr	yh,(1<<RAM_A10)
	ret

;----------------------------------------------

rdsk_read:
	lds	xl,dmaadr
	lds	xh,dmaadr+1
	rcall	rdsk_adr
	ldi	temp3,4
rdsk_rdlo:
;	cli
	out	PORTB,yh			;  Row 8..10
	out	PORTD,zh			;  Row 0..7
	out	PORTC,_RAS0
	out	PORTB,yl			;1 Col 8..10
	push	yh
	push	yl

	ldi	yl,low (rdskbuf)
	ldi	yh,high(rdskbuf)
	ldi	temp2,32
rdsk_rdli:
	out	PORTD,zl			;1 Col 0..7
	out	PORTC,_CAS0			;1 
	out	DDRD,_0				;1 
	out	PORTC,_OE			;1
	inc	zl				;1
	dec	temp2				;1
	dram_wait DRAM_WAITSTATES		;
	in	temp,PIND			;1
	out	PORTC,_RAS0			;1
	out	DDRD,_255			;1
	st	y+,temp				;2
	brne	rdsk_rdli			;2  --> 12 * 128 = 1536 = 77µs
	out	PORTC,_255			;1
;	sei
	out	PORTB,_255			;1

	ldi	yl,low (rdskbuf)
	ldi	yh,high(rdskbuf)
	ldi	temp2,32	
rdsk_rdstl:
	ld	temp,y+				;2
	mem_write_d x				;14 (?)
	adiw	x,1				;2  --> 18 * 128 = 2304 = 115µs
	dec	temp2
	brne	rdsk_rdstl
	pop	yl
	pop	yh
	dec	temp3
	brne	rdsk_rdlo
	ret
	

rdsk_write:
	lds	xl,dmaadr
	lds	xh,dmaadr+1
	rcall	rdsk_adr
	ldi	temp3,4
	push	yh
	push	yl
rdsk_wrlo:
	ldi	yl,low (rdskbuf)
	ldi	yh,high(rdskbuf)
	ldi	temp2,32
rdsk_wrldl:
	mem_read_s x
	st	y+,temp
	adiw	x,1
	dec	temp2
	brne	rdsk_wrldl

	pop	yl
	pop	yh
	push	yh
	push	yl
	ldi	temp2,32
;	cli
	out	PORTB,yh
	out	PORTD,zh
	out	PORTC,_RAS0
	out	PORTB,yl			;1
	ldi	yl,low (rdskbuf)
	ldi	yh,high(rdskbuf)
rdsk_wrli:
	out	PORTD,zl
	out	PORTC,_CAS0
	ld	temp,y+
	out	PORTD,temp
	out	PORTC,_WE
	out	PORTC,_RAS0
	inc	zl
	dec	temp2
	brne	rdsk_wrli
	
	out	PORTC,_255
;	sei
	out	PORTB,_255
	dec	temp3
	brne	rdsk_wrlo
	pop	yl
	pop	yh
	ret

#else	/* 4-bit DRAM */


;-------------------------------------- Defines for RAMDISK Structures

;----------------------------------------------- Start of Data Segment

	.dseg

rdskbuf:	.byte	128			; Buffer for RAM-Disk interaktions

; ---------------------------------------------- Start of Code Segment
	.cseg

; ====================================================================
; Function: Calculate an sets the adress of Sector within the RAMDISK
; ====================================================================
; Parameters
; --------------------------------------------------------------------
; Registers  :  none
; Variables  :  [r] seeksec		Sector to read
;               [r] seektrk		Track  to read
;				[w] temp3		Number of Bytes per Sector (128)		
; --------------------------------------------------------------------
; Description:
; ====================================================================


rdsk_adr:
	ldi	xl,0
	lds	xh,seeksec
	lds	temp2,seektrk
	
	lsr	xh
	ror	xl				;Col 0..7
	
	mov	 temp,temp2
	andi temp,0x0f
	swap temp
	or	 xh,temp		;Row  0..7
	
	ldiw	z,rdskbuf
	ldi	temp3,128
	DRAM_SETADDR xh, ~0,(1<<ram_ras), ~0,(1<<ram_a8)|(1<<ram_oe)
	cbi	P_RAS,ram_ras

.if DISK_DEBUG > 1
	printstring " "
	mov	temp,xh
	rcall	printhex
	printstring " "
	mov	temp,xl
	rcall	printhex
	printstring " "
.endif
	ret

; ====================================================================
; Function: Does a read opperation on a RAMDISK
; ====================================================================
; Parameters
; --------------------------------------------------------------------
; Registers  :  none
; Variables  :  [r] seeksec		Sector to read
;               [r] seektrk		Track  to read
;				[r] flags		RW operation Flags
;				[w] erflag		Error Status of the operation
; --------------------------------------------------------------------
; Description:
; ====================================================================


rdsk_read:

.if DISK_DEBUG > 1
	printnewline
	printstring "rd-adr: "
.endif
	rcall	rdsk_adr

rdsk_rdl:
	DRAM_SETADDR xl, ~(1<<ram_ras),0, ~((1<<ram_oe)), (1<<ram_a8)
	cbi	P_CAS,ram_cas
	cbi	P_A8,ram_a8
	inc	xl
	dram_wait DRAM_WAITSTATES	;
	in	temp,P_DQ-2		; PIN
	sbi	P_CAS,ram_cas

	cbi	P_CAS,ram_cas
	andi	temp,0x0f
	swap	temp
	dram_wait DRAM_WAITSTATES	;
	in	temp2,P_DQ-2		; PIN
	andi	temp2,0x0f
	or	temp,temp2

	sbi	P_OE,ram_oe
	sbi	P_CAS,ram_cas
	dec	temp3
	st	z+,temp
	brne	rdsk_rdl

	sbi	P_RAS,ram_ras
	ldiw	z,rdskbuf
	lds	xl,dmaadr
	lds	xh,dmaadr+1
	ldi	temp3,128	
rdsk_rdstl:
	ld	temp,z+
	mem_write
	adiw	x,1
	dec	temp3
	brne	rdsk_rdstl
	ret
	
; ====================================================================
; Function: Does a write opperation on a RAMDISK
; ====================================================================
; Parameters
; --------------------------------------------------------------------
; Registers  :  none
; Variables  :  [r] seeksec		Sector to read
;               [r] seektrk		Track  to read
;				[r] flags		RW operation Flags
;				[w] erflag		Error Status of the operation
; --------------------------------------------------------------------
; Description:
; ====================================================================

rdsk_write:
.if DISK_DEBUG > 1
	printnewline
	printstring "wr-adr: "
.endif	
	lds	xl,dmaadr
	lds	xh,dmaadr+1
	ldiw	z,rdskbuf
	ldi	temp3,128	
rdsk_wrldl:
	mem_read
	st	z+,temp
	adiw	x,1
	dec	temp3
	brne	rdsk_wrldl	

	ldi	temp2,RAM_DQ_MASK | (1<<ram_w) | (1<<ram_cas)
	out	DDRC,temp2
	rcall	rdsk_adr
rdsk_wrl:
	ld	temp,z+
	mov	temp2,temp
	andi	temp,RAM_DQ_MASK & ~(1<<ram_w)
	ori	temp,(1<<ram_cas)
	out	PORTC,temp
	DRAM_SETADDR xl, ~(1<<ram_ras),0, ~((1<<ram_a8)),(1<<ram_oe)
	cbi	PORTC,ram_cas
	sbi	PORTD,ram_a8
	sbi	PORTC,ram_cas
	swap	temp2
	andi	temp2,RAM_DQ_MASK & ~(1<<ram_w)
	ori	temp2,(1<<ram_cas)
	out	PORTC,temp2
	cbi	PORTC,ram_cas
	inc	xl
	sbi	PORTC,ram_cas
	dec	temp3
	brne	rdsk_wrl

	sbi	P_RAS,ram_ras
	ldi	temp,~RAM_DQ_MASK | (1<<ram_w) | (1<<ram_cas)
	out	DDRC,temp
	out	PORTC,temp
	ret


rdsk_add_partition:
	ret



#endif	/* DRAM_8BIT */


#else  /* no ram disk */

rdsk_read:
	ret
rdsk_write:
	ret
rdsk_add_partition:
	ret

#endif	/* RAMDISKCNT */

