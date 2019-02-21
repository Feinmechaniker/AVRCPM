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
;    $Id: dram-8bit.inc 93 2014-01-03 16:32:32Z rapid $
;


; -------------------- DRAM  macros---------------

; add wait states
;	dram_wait  number_of_cycles

.macro dram_wait
.if @0 > 1
	rjmp	PC+1
	dram_wait @0 - 2
.elif @0 > 0
	nop
	dram_wait @0 - 1
.endif
.endm

;----------------------------------------------
; Read byte from DRAM
;	mem_read_ds  destreg, memaddr

.macro	mem_read_ds
;	cli				;1
	out	PORTD,@1h		;1
	out	PORTC,_RAS0		;1
	out	PORTD,@1l		;1
	out	PORTC,_CAS0		;1
	out	DDRD,_0			;1
	out	PORTC,_OE		;1
	rjmp	PC+1			;2
	dram_wait DRAM_WAITSTATES	;
	in	@0,PIND			;1
	out	PORTC,_255		;1
;	sei				;1
	out	DDRD,_255		;1  = 14 + DRAM_WAITSTATES
.endm

;----------------------------------------------
; Read byte from DRAM
;	mem_read_d  destreg
;  	x = memaddr

.macro	mem_read_d
	mem_read_ds  @0, x
.endm

;----------------------------------------------
; Read byte from DRAM
;	mem_read_s  memaddr
;  	temp = destreg

.macro	mem_read_s
	mem_read_ds  temp, @0
.endm

;----------------------------------------------
; Read byte from DRAM
;	mem_read
;  	temp = destreg, x = memaddr

.macro	mem_read
	mem_read_ds  temp, x
.endm

;----------------------------------------------
; Write byte to DRAM
;	mem_write_ds  memaddr, sourcereg

.macro	mem_write_ds
;	cli				;1
	out	PORTD,@0h		;1
	out	PORTC,_RAS0		;1
	out	PORTD,@0l		;1
	out	PORTC,_CAS0		;1
	out	PORTD,@1		;1
	out	PORTC,_WE		;1
;	sei				;1
	out	PORTC,_255		;1  = 9
.endm

;----------------------------------------------
; Write byte to DRAM
;	mem_write_d  memaddr
;  	temp = srcreg

.macro	mem_write_d
	mem_write_ds @0, temp	
.endm

;----------------------------------------------
; Write byte to DRAM
;	mem_write_s  sourcereg
;  	xh = memaddrh, xl = memaddrl

.macro	mem_write_s
	mem_write_ds x, @0	
.endm

;----------------------------------------------
; Write byte to DRAM
;	mem_write
;  	xh = memaddrh, xl = memaddrl, temp = srcreg

.macro	mem_write
	mem_write_ds x, temp
.endm


