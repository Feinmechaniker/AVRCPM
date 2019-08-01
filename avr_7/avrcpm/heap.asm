;    Simple memory management module.
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
;    $Id: heap.asm 93 2014-01-03 16:32:32Z rapid $
;


	.dseg
hp_top: .byte	2

	.cseg

.if HEAP_DEBUG
hp_print_free:
	push	r15
	push	r14
	push	temp2
	push	temp
	printstring ", bytes free: "
	lds	r14,hp_top
	lds	r15,hp_top+1
	ldi	temp,0
	ldi	temp2,0
	sub	temp,r14
	sbc	temp2,r15
	clr	r14
	clr	r15
	rcall	print_ultoa
	printstring " "
	pop	temp
	pop	temp2
	pop	r14
	pop	r15
	ret
.endif		


; Init heap
; temp:temp2 = first free memory location (heap start)

heap_init:
.if HEAP_DEBUG
	printnewline
	printstring "Heap init: Start: "
	rjmp	hp_dbg1
.endif		
heap_release:
.if HEAP_DEBUG
	printnewline
	printstring "Heap release: Start: "
hp_dbg1:
.endif		
	sts	hp_top,temp
	sts	hp_top+1,temp2
.if HEAP_DEBUG
	rcall	printhexw
	rcall	hp_print_free
.endif		
	ret

; Get memory block from heap.
; temp2:temp = size of block
; return temp2:temp = pointer to allocated block
; return 0 if not enough space

heap_get:
	push	r15
	push	r14
.if HEAP_DEBUG
	push	temp2
	push	temp
	printnewline
	printstring "Heap get: "
	clr	r14
	clr	r15
	rcall	print_ultoa
	pop	temp
	pop	temp2	
.endif		
	lds	r14,hp_top
	lds	r15,hp_top+1
	add	temp,r14
	adc	temp2,r15
	brcs	hp_full
		
; zero flag clear here

	sts	hp_top,temp
	sts	hp_top+1,temp2
	movw	temp,r14
	rjmp	hp_get_ex
hp_full:
	clr	temp
	clr	temp2		;(sets zero flag)
hp_get_ex:
.if HEAP_DEBUG
	brne	hp_get_dbg1
	printstring "Error: "
hp_get_dbg1:
	rcall	hp_print_free
	mov	r14,temp	;restore zero flag
	or	r14,temp2
.endif
	pop	r14
	pop	r15
	ret


; vim:set ts=8 noet nowrap
				
