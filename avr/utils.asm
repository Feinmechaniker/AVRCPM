;    Print and Debug functions
;
;    Copyright (C) 2010-2013 Leo C.
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
;    $Id: utils.asm 93 2014-01-03 16:32:32Z rapid $
;


	.cseg


;Print a unsigned lonng value to the uart
; r15:r14:temp2:temp = value

print_ultoa:
	push	yh
	push	yl
	push	z_flags
	push	r15
	push	r14
	push	temp2
	push	temp
				
	clr	yl		;yl = stack level

ultoa1:	ldi	z_flags, 32	;yh = r15:temp % 10
	clr	yh		;r15:temp /= 10
ultoa2:	lsl	temp	
	rol	temp2	
	rol	r14	
	rol	r15	
	rol	yh	
	cpi	yh,10	
	brcs	ultoa3	
	subi	yh,10	
	inc	temp
ultoa3:	dec	z_flags	
	brne	ultoa2
	cpi	yh, 10	;yh is a numeral digit '0'-'9'
	subi	yh, -'0'
	push	yh		;Stack it
	inc	yl	
	cp	temp,_0		;Repeat until r15:temp gets zero
	cpc	temp2,_0
	cpc	r14,_0
	cpc	r15,_0
	brne	ultoa1	
	
	ldi	temp, '0'
ultoa5:	cpi	yl,3		; at least 3 digits (ms)
	brge	ultoa6
	push	temp	
	inc	yl
	rjmp	ultoa5

ultoa6:	pop	temp		;Flush stacked digits
	rcall	uartputc
	dec	yl	
	brne	ultoa6	

	pop	temp
	pop	temp2
	pop	r14
	pop	r15
	pop	z_flags
	pop	yl
	pop	yh
	ret


;Prints temp2:temp in hex to the uart
printhexw:
	push	temp
	mov	temp,temp2
	rcall	printhex
	pop	temp
	;fall thru

;Prints temp in hex to the uart
printhex:
	swap temp
	rcall printhexn
	swap temp	
	;fall thru

;Prints the lower nibble
printhexn:
	push temp
	andi temp,0xf
	cpi temp,0xA
	brlo printhexn_isno
	subi temp,-7
printhexn_isno:
	subi temp,-'0'
	rcall uartputc
	pop temp
	ret


; Prints a single space

printspace:
	push	temp
	ldi	temp,' '
	rcall	uartputc
	pop	temp
	ret

;-----------------------------------------------------------------------
;Prints the zero-terminated string following the call statement. 

printstr:
	push	zh		;SP+5
	push	zl		;   4
	push	yh		;   3
	push	yl		;   2
	push	temp		;   1
	in	yh,sph
	in	yl,spl
	ldd	zl,y+7		;SP+7  == "return adr." == String adr.
	ldd	zh,y+6		;SP+6

	lsl zl			;word to byte conv.
	rol zh
printstr_loop:
	lpm temp,z+
	cpi temp,0
	breq printstr_end
	rcall uartputc
	cpi temp,13
	brne printstr_loop
	ldi temp,10
	rcall uartputc
	rjmp printstr_loop

printstr_end:
	adiw zl,1		;rounding up
	lsr zh			;byte to word conv.
	ror zl

	std	y+7,zl
	std	y+6,zh
	pop	temp
	pop	yl
	pop	yh
	pop	zl
	pop	zh
	ret
	
; ------------------------ String functions -------------------------
;

#if 0
; String compare (z, y), one z-string in flash.

strcmp_p:
	ld	temp,y+
	lpm	_tmp0, z+
	sub	temp,_tmp0
	brne	strcmp_pex
	tst	_tmp0
	brne	strcmp_p
	sub	temp,temp
strcmp_pex:
	ret

#endif

#if 0

strcmp_p:
	ld	temp,y+
	lpm	_tmp0,z+
	sub	temp,_tmp0
	cpse	_tmp0,_0
	breq	strcmp_p
	ret


#endif

;-----------------------------------------------------------------------
; String compare (x, y, temp2). Max temp2 bytes are compared.

strncmp_p:
	subi	temp2,1
	brcs	strncmp_peq
	ld	temp,y+
	lpm	_tmp0, z+
	sub	temp,_tmp0
	brne	strncmp_pex
	tst	_tmp0
	brne	strncmp_p
strncmp_peq:
	sub	temp,temp
strncmp_pex:
	ret

;-----------------------------------------------------------------------
; Memory compare: DRAM - AVR-RAM
; 	DRAM-Addr. in x, AVRRAM-Addr. in y 
;	Compare temp3 bytes.
;
;	Return Z-Flag == 1 if match
;	temp, _tmp0  destroyed
;

memcmp_d:
	rcall	dram_read_pp
	ld	_tmp0,y+
	cp	temp,_tmp0
	brne	memcmpd_nomatch
	dec	temp3
	brne	memcmp_d
memcmpd_nomatch:
	ret

; --------------- Debugging stuff ---------------


.if SRAM_FILL

stackusage_print:
	ldiw	z,ramtop
	ldi	temp,  low(RAMEND+1)
	ldi	temp2,high(RAMEND+1)
	ldi	temp3,SRAMFILL_VAL
stack_search_l:
	ld	_tmp0,z+
	cp	temp3,_tmp0
	brne	stack_search_found
	cp	zl,temp
	cpc	zh,temp2
	brne	stack_search_l

stack_search_found:
	sbiw	z,1
	sub	temp, zl
	sbc	temp2,zh
	printnewline
	printstring "Stack used (bytes): "
	push	r15
	push	r14
	clr	r14
	clr	r15
	rcall	print_ultoa
	pop	r14
	pop	r15
	ret
.endif


.if MEMDUMP_DEBUG

;-----------------------------------------------------------------------
; Prints 16 bytes RAM, pointed to by Z in hex.

dbg_hexdump_line:			;Address in z
	push	temp2
	push	temp
;	printnewline
	movw	temp,z			;Print address
	rcall	printhexw
	printstring ":"
	ldi	temp2,16		;16 byte per line
dbg_hdl1:
	cpi	temp2,8
	brne	PC+2
	rcall	printspace
	
	rcall	printspace
	ld	temp,z+
	rcall	printhex
	dec	temp2
	brne	dbg_hdl1
	sbiw	z,16
	
	rcall	printspace
	rcall	printspace
	ldi	temp2,16
dbg_hdl2:
	ld	temp,z+
	cpi	temp,' '
	brlo	dbg_hdlpd
	cpi	temp,0x7F
	brlo	dbg_hdlp
dbg_hdlpd:
	ldi	temp,'.'
dbg_hdlp:
	rcall	uartputc
	dec	temp2
	brne	dbg_hdl2
	sbiw	z,16
	rcall	printspace
	pop	temp
	pop	temp2
	printnewline
	ret
	

; Prints temp2 bytes RAM, pointed to by Z in hex.

dbg_hexdump:				;Address in z
	push	temp
	push	temp2
;	printnewline
	movw	temp,z			;Print address
	rcall	printhexw
	printstring ":"
	pop	temp2
	push	temp2
dbg_hd1:
	rcall	printspace
	ld	temp,z+
	rcall	printhex
	dec	temp2
	brne	dbg_hd1
	pop	temp2
	sub	zl,temp2
	sbc	zh,_0
	rcall	printspace
	rcall	printspace
dbg_hd2:
	ld	temp,z+
	cpi	temp,' '
	brlo	dbg_hdpd
	cpi	temp,0x7F
	brlo	dbg_hdp
dbg_hdpd:
	ldi	temp,'.'
dbg_hdp:
	rcall	uartputc
	dec	temp2
	brne	dbg_hd2
	pop	temp
	printnewline
	ret

.endif
	
;-----------------------------------------------------------------------
; Print a line with the 8080/Z80 registers

printregs:
	mov	temp,z_flags
	rcall	printflags
	printstring "  A ="
	mov	temp,z_a
	rcall	printhex	
	printstring " BC ="
#if 1
	movw	temp,z_c
#else
	ldd	temp2,y+oz_b
	ldd	temp,y+oz_c
#endif
	rcall	printhexw
	printstring " DE ="
#if 1
	movw	temp,z_e
#else
	ldd	temp2,y+oz_d
	ldd	temp,y+oz_e
#endif
	rcall	printhexw
	printstring " HL ="
#if 1
	movw	temp,z_l
#else
	ldd	temp,y+oz_l
	ldd	temp2,y+oz_h
#endif
	rcall	printhexw
	printstring " SP="
	movw	temp, z_spl
	rcall	printhexw
	printstring " PC="
	movw	temp, z_pcl
	rcall	printhexw
	printstring "       "
	movw 	xl,z_pcl
	lcall	dram_read_pp
	rcall	printhex
	printstring " "
	lcall	dram_read_pp
	rcall	printhex
	printstring " "
	lcall	dram_read
	rcall	printhex
	printstring " "

#if EM_Z80
	ldd	temp,y+oz_f2
	rcall	printflags
	printstring "  a'="
	ldd	temp,y+oz_a2
	rcall	printhex	
	printstring " bc'="
	ldd	temp2,y+oz_b2
	ldd	temp,y+oz_c2
	rcall	printhexw
	printstring " de'="
	ldd	temp2,y+oz_d2
	ldd	temp,y+oz_e2
	rcall	printhexw
	printstring " hl'="
	ldd	temp2,y+oz_h2
	ldd	temp,y+oz_l2
	rcall	printhexw
	printstring " IX="
	ldd	temp2,y+oz_xh
	ldd	temp,y+oz_xl
	rcall	printhexw
	printstring " IY="
	ldd	temp2,y+oz_yh
	ldd	temp,y+oz_yl
	rcall	printhexw
	printstring " I="
	ldd	temp,y+oz_i
	rcall	printhex	

	printstring "       "
#endif
	ret


#if EM_Z80
zflags_to_ch:
	.db	"SZ H VNC",0,0
#else	
zflags_to_ch:
	.db	"SZ H PNC",0,0
#endif
	
printflags:
	push	temp2
	mov	temp2,temp
	printnewline
	push	zl
	push	zh
	ldiw	z,zflags_to_ch*2
pr_zfl_next:
	lpm	temp,z+
	tst	temp
	breq	pr_zfl_end
	cpi	temp,' '			; Test if no flag
	breq	pr_zfl_noflag
	sbrs	temp2,7			; 
	 ldi	temp,' '			; Flag not set
	rcall	uartputc
pr_zfl_noflag:
	rol	temp2
	rjmp	pr_zfl_next
pr_zfl_end:
	pop	zh
	pop	zl	
	pop	temp2
	ret

; vim:set ts=8 noet nowrap

