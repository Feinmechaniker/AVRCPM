; DRAM interface for *one* 256K x 4 bit DRAM chip.
; This is part of the Z80-CP/M emulator written by Sprite_tm.

;    Copyright (C) 2010 Sprite_tm
;    Copyright (C) 2010 Leo C.

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
;    $Id: dram-4bit.asm 93 2014-01-03 16:32:32Z rapid $
;


;----------------------------------------------

; Loads the byte on address xh:xl into temp.
; Uses temp2. Must not alter xh:xl

	.cseg
dram_read:
;	cli
	DRAM_SETADDR xh, ~0,(1<<ram_ras), ~(1<<ram_a8), (1<<ram_oe)
	cbi P_RAS,ram_ras
	DRAM_SETADDR xl, ~(1<<ram_ras),0, ~((1<<ram_oe)), (1<<ram_a8)
	cbi P_CAS,ram_cas
	cbi P_A8,ram_a8
	dram_wait DRAM_WAITSTATES	;
	in  temp,P_DQ-2		; PIN
	out P_CAS,_255

	cbi P_CAS,ram_cas
	andi temp,0x0f
	swap temp
	dram_wait DRAM_WAITSTATES	;
	in  temp2,P_DQ-2	; PIN
	andi temp2,0x0f
	or  temp,temp2

	out P_OE, _255
	out P_CAS,_255
	out P_RAS,_255
;	sei
	ret


;Writes the byte in temp to  xh:xl
; Uses temp2. Must not alter xh:xl

dram_write:
;	cli
	ldi temp2,RAM_DQ_MASK | (1<<ram_w) | (1<<ram_cas)
	out DDRC,temp2

	mov  temp2,temp
	andi temp,RAM_DQ_MASK & ~(1<<ram_w)
	ori temp,(1<<ram_cas)
	out PORTC,temp
	DRAM_SETADDR xh, ~0,(1<<ram_ras), ~(1<<ram_a8),(1<<ram_oe)
	cbi P_RAS,ram_ras
	DRAM_SETADDR xl, ~(1<<ram_ras),0, ~((1<<ram_a8)),(1<<ram_oe)
	cbi PORTC,ram_cas
	sbi PORTC,ram_cas

	sbi PORTD,ram_a8
	swap temp2

	andi temp2,RAM_DQ_MASK & ~(1<<ram_w)
	ori temp2,(1<<ram_cas)
	out PORTC,temp2
	cbr temp2,(1<<ram_cas)
	out PORTC,temp2
	ldi temp,~RAM_DQ_MASK | (1<<ram_w) | (1<<ram_cas)
	out PORTC,_255
	out DDRC,temp
	out P_RAS,_255
;	sei
	ret

; -------------------------------------------------------------------

dram_readw_pp:
	rcall	dram_read
	adiw	x,1
	push	temp
	rcall	dram_read
	adiw	x,1
	mov	temp2,temp
	pop	temp
	ret
	
dram_read_pp:
	rcall	dram_read
	adiw	x,1
	ret

; -------------------------------------------------------------------

dram_writew_pp:
	push	temp2
	rcall	dram_write
	adiw	x,1
	pop	temp
dram_write_pp:
	rcall	dram_write
	adiw	x,1
	ret

; -------------------------------------------------------------------
; vim:set ts=8 noet nowrap

