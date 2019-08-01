;    This is the main file, glueing all parts together.

;    Copyright (C) 2010 Sprite_tm
;    Copyright (C) 2010,2012,2013 Leo C.
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
;    $Id: avrcpm.asm 93 2014-01-03 16:32:32Z rapid $
;

	.nolist

#if defined atmega8
	.include "m8def.inc"
#elif defined atmega88
	.include "m88def.inc"
#elif defined atmega168
	.include "m168def.inc"
#else                               /* default */
	.include "m328Pdef.inc"
#endif
	.include "config.inc"
	.include "svnrev.inc"
	.include "macros.inc"
#if DRAM_8BIT	/* Implies software uart */
	.include "dram-8bit.inc"
#else
	.include "dram-4bit.inc"
#endif

	.list

#if MMCBOOTLOADER
	.cseg
	.org FLASHEND-3 - BOOTLDRSIZE/2	;
	.db	DEVID_S			;
  #if TESTVERSION
	.db	0,0
  #else
	.db	VMINOR, VMAJOR		;
  #endif
	.dw	0			;placeholder for crc
#endif /* MMCBOOTLOADER */

	.cseg
	.org 0
	ljmp start		; reset vector
    	
	.org INT_VECTORS_SIZE

#if DRAM_8BIT			/* Implies software uart */
	.include "sw-uart.asm"
#else				/* 4 bit RAM, hardware uart */
	.include "hw-uart.asm"
#endif

	.include "utils.asm"
	.include "init.asm"
	.include "timer.asm"
	.include "heap.asm"
	.include "mmc.asm"

#if DRAM_8BIT			/* Implies software uart */
	.include "dram-8bit.asm"
#else				/* 4 bit RAM, hardware uart */
	.include "dram-4bit.asm"
#endif
#if DRAM_8BIT			/* Implies software uart */
	.include "i2c.asm"
#endif
; >>>-------------------------------------- File System Management
	.include "dsk_cpm.asm"		; CPM- Disk Interaktion
	.include "dsk_fat16.asm"	; FAT16-DISK Interaktion
	.include "dsk_ram.asm"		; RAM- Disk Interaktion
	.include "dsk_mgr.asm"		; Disk- Manager
	.include "dsk_fsys.asm"		; Basic Filesystem definitions

; <<<-------------------------------------- File System Management

;	.include "8080int-orig.asm"	;Old 8080 interpreter.
;	.include "8080int.asm"		;New 8080 interpreter.
;	.include "8080int-t3.asm"	;Another 8080 interpreter
;	.include "8080int-t3-jmp.asm"	;Can't get enough
;	.include "8080int-jmp.asm"	;
	.include "Z80int-jmp.asm"	;

	.include "virt_ports.asm"	; Virtual Ports for BIOS
	.include "dram-refresh.asm"


	.dseg
ramtop:	.byte	0
	
	.cseg

; vim:set ts=8 noet nowrap

