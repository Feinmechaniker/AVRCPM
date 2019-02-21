;    DRAM refresh
;
;    Copyright (C) 2010 Sprite_tm
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
;    $Id: dram-refresh.asm 93 2014-01-03 16:32:32Z rapid $
;


; ------------------- DRAM Refresh Interrupt --------------------

	.cseg
	
	INTERRUPT OC2Aaddr
	reti					; only for SRAM	

	sbis	P_RAS,ram_ras	;2
	reti
				;       CAS  RAS  
	cbi	P_CAS,ram_cas	;2       1|   1|  
				;        1|   1|  
	cbi	P_RAS,ram_ras	;2      |0    1|  
				;       |0    1|  
	nop			;1      |0   |0   
;	nop			;1      |0   |0   
	sbi	P_RAS,ram_ras	;2      |0   |0   
				;       |0   |0   
	dram_wait DRAM_WAITSTATES-1 ;   |    |
;	nop			;1      |0   |0   
	cbi	P_RAS,ram_ras	;2      |0    1|  
				;       |0    1|  
	sbi	P_CAS,ram_cas	;2      |0   |0   
				;       |0   |0   
	sbi	P_RAS,ram_ras	;2       1|  |0   
				;        1|   1|  
	reti			;4  --> 21 cycles


; vim:set ts=8 noet nowrap

