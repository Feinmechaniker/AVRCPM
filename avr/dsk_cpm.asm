;    Various functions for the Interaction with the CPM Filesystem
;
;    Copyright (C) 2010 Frank Zoll
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
;    $Id: dsk_cpm.asm 93 2014-01-03 16:32:32Z rapid $
;

#if CPMDSK_SUPPORT


	.cseg

cpm_lba_to_phys:
	ldd	temp ,z+PTAB_START+0	; get startsector of partition
	ldd	temp2,z+PTAB_START+1
	ldd	yl,z+PTAB_START+2
	ldd	yh,z+PTAB_START+3

	add	xl,temp			; add offset to startsector
	adc	xh,temp2
	adc	yl,_0
	adc	yh,_0

	ret

; ====================================================================
; Function: Add's a CP/M Partition to the Partition table
; ====================================================================
; Parameters
; --------------------------------------------------------------------
; Registers  : none
; Variables  : [r] seekdsk		Number of Disk to Read
;	       [r] seeksec		Sector to read
;              [r] seektrk		Track  to read
; --------------------------------------------------------------------
; Description:
; ====================================================================	
cpm_add_partition:
	
	ldi 	temp,dskType_CPM
	std	y+0,temp

	ldd	temp,z+PART_START
	std	y+1,temp
	ldd	temp,z+PART_START+1
	std	y+2,temp
	ldd	temp,z+PART_START+2
	std	y+3,temp
	ldd	temp,z+PART_START+3
	std	y+4,temp
	
	ldd	temp,z+PART_SIZE+2
	ldd	temp2,z+PART_SIZE+3
	or	temp,temp2		;part size larger than 65535 sectors?
	brne	cpm_add_prune

	ldd	temp,z+PART_SIZE
	std	y+5,temp
	ldd	temp,z+PART_SIZE+1
	std	y+6,temp
	rjmp	cpm_add_e

cpm_add_prune:
	std	y+5,_255
	std	y+6,_255

cpm_add_e:
	ret
		
#endif /* CPMDSK_SUPPORT */

; vim:set ts=8 noet nowrap

