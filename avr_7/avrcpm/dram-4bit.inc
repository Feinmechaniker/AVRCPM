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
;    $Id: dram-4bit.inc 93 2014-01-03 16:32:32Z rapid $
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

; ------------------------------------------------

; DRAM_SETADDR val, low_and_mask, low_or_mask, high_and_mask, high_or_mask

.macro DRAM_SETADDR
	mov temp,@0
.if low(@1) != 0xff
	andi temp,@1
.endif
.if  low(@2) != 0
	ori temp, @2
.endif
	out P_AL,temp
	
	mov temp,@0
.if low(@3) != 0xff
	andi temp,@3
.endif
	ori temp, @4 | (1<<mmc_cs)
	out P_AH,temp
.endm


;----------------------------------------------
; Read byte from DRAM
;  	temp = destreg, xh = memaddrh, xl = memaddrl

.macro	mem_read
	lcall	dram_read
.endm

;----------------------------------------------
; Read byte from DRAM
;	mem_read  memaddr
;  	temp = destreg

.macro	mem_read_s
	movw	xl,@0l
	lcall	dram_read
.endm

;----------------------------------------------
; Read byte from DRAM
;	mem_read  destreg
;  	xh = memaddrh, xl = memaddrl

.macro	mem_read_d
	lcall	dram_read
	mov	@0,temp
.endm

;----------------------------------------------
; Read byte from DRAM
;	mem_read  destreg, memaddr

.macro	mem_read_ds
	movw	xl,@1l
	lcall	dram_read
	mov	@0,temp
.endm

;----------------------------------------------
; Write byte to DRAM
;  	xh = memaddrh, xl = memaddrl, temp = srcreg

.macro	mem_write
	lcall	dram_write
.endm

;----------------------------------------------
; Write byte to DRAM
;  	temp = srcreg
;	mem_write  memaddr

.macro	mem_write_d
	movw	x,@0l
	lcall	dram_write
.endm

;----------------------------------------------
; Write byte to DRAM
;  	xh = memaddrh, xl = memaddrl
;	mem_write  sourcereg

.macro	mem_write_s
	mov	temp,@0
	lcall	dram_write
.endm

;----------------------------------------------
; Write byte to DRAM
;	mem_write  memaddr, sourcereg

.macro	mem_write_ds
	movw	x,@0l
	mov	temp,@1
	lcall	dram_write
.endm

