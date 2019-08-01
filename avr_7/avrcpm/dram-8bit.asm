; DRAM interface for 2 RAM chips. Supports up to 4 Mbyte of DRAM.
; This is part of the Z80-CP/M emulator written by Sprite_tm.

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
;    $Id: dram-8bit.asm 93 2014-01-03 16:32:32Z rapid $
;

	.cseg

; ------------------ DRAM routines -------------

;Loads the byte on address xh:xl into temp.
;must not alter xh:xl

dram_read:
;	cli				;
	out	PORTD,xh		;1
	out	PORTC,_RAS0		;1
	out	PORTD,xl		;1
	out	PORTC,_CAS0		;1
	out	DDRD,_0			;1
	out	PORTC,_OE		;1
	rjmp	PC+1			;2
	dram_wait DRAM_WAITSTATES	;
	in	temp,PIND		;1
	out	PORTC,_255		;1
	out	DDRD,_255		;1
;	sei				;
	ret



;Writes the byte in temp to  xh:xl
;must not alter xh:xl

dram_write:
;	cli
	out	PORTD,xh		;1
	out	PORTC,_RAS0		;1
	out	PORTD,xl		;1
	out	PORTC,_CAS0		;1
	out	PORTD,temp		;1
	out	PORTC,_WE		;1
	out	PORTC,_255		;1  = 7
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
	rcall	dram_write
	adiw	x,1
	mov	temp,temp2
dram_write_pp:
	rcall	dram_write
	adiw	x,1
	ret


; -------------------------------------------------------------------
; vim:set ts=8 noet nowrap

