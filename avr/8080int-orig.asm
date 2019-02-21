; Z80 Interpreter.
; This is part of the Z80-CP/M emulator written by Sprite_tm.
; The Z80-specific instructions themselves actually aren't
; implemented yet, making this more of an i8080 emulator.

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
;    $Id: 8080int-orig.asm 93 2014-01-03 16:32:32Z rapid $
;

	.dseg

z_b:	.byte	1
z_c:	.byte	1
z_d:	.byte	1
z_e:	.byte	1
z_h:	.byte	1
z_l:	.byte	1

	.cseg

;Init z80
z80_init:
	ldi z_pcl,low (IPLADDR)
	ldi z_pch,high(IPLADDR)

	cbi	flags,trace
	printnewline
	printstring "Ok, CPU is live!"
	printnewline

main:
.if INS_DEBUG
	cbi	flags,trace
	cpi z_pch,DBG_TRACE_BOTTOM
	brlo notraceon
	cpi z_pch,DBG_TRACE_TOP
	brsh notraceon
	sbi	flags,trace
notraceon:
.endif

.if PRINT_PC
	cpi z_pch,DBG_TRACE_BOTTOM
	brlo noprintpc
	cpi z_pch,DBG_TRACE_TOP
	brsh noprintpc

	printnewline
	printstring "PC="
	movw temp,z_pcl
	rcall printhexw
	printstring " "
noprintpc:
.endif

	; *** Stage 1: Fetch next opcode
	mem_read_s z_pc


.if INS_DEBUG
	sbis	flags,trace
	rjmp	notrace1
	printnewline
	printstring "PC="
	push temp
	movw temp,z_pcl
	rcall printhexw
	pop temp
	printstring ", opcode="
	rcall printhex
notrace1:
.endif
	adiw z_pcl,1

	; *** Stage 2: Decode it using the ins_table.
	ldi zh,high(inst_table*2)
	mov zl,temp
	add zl,temp
	adc zh,_0
	lpm insdecl,Z+
	lpm insdech,Z

.if INS_DEBUG
	sbis	flags,trace
	rjmp	notrace2
	printstring ", decoded="
	movw	temp,insdecl
	rcall	printhexw
notrace2:
.endif

	; *** Stage 3: Fetch operand. Use the fetch jumptable for this.
	mov temp,insdecl
	andi temp,0x1F
	breq nofetch
	ldi zl,low(fetchjumps)
	ldi zh,high(fetchjumps)
	add zl,temp
	adc zh,_0
	icall

.if INS_DEBUG
	sbis	flags,trace
	rjmp	notrace3
	printstring "  pre: op="
	movw 	temp,opl
	rcall 	printhexw
notrace3:
	rjmp	nonofetch
.endif

nofetch:
.if INS_DEBUG
	sbis	flags,trace
	rjmp	nonofetch
	printstring "              "

nonofetch:
.endif
	; *** Stage 4: Execute operation :) Use the op jumptable for this.
	mov temp,insdech
	andi temp,0xFC
	breq nooper
	lsr temp
	lsr temp
	ldi zl,low(opjumps)
	ldi zh,high(opjumps)
	add zl,temp
	adc zh,_0
	icall

.if INS_DEBUG
	sbis	flags,trace
	rjmp	notrace4
	printstring "  post: op="
	movw	temp,opl
	rcall	printhexw
notrace4:
	rjmp	nonooper
.endif

nooper:
.if INS_DEBUG
	sbis	flags,trace
	rjmp	nonooper
	printstring "               "

nonooper:
.endif
	; *** Stage 5: Store operand. Use the store jumptable for this.
	swap insdecl
	swap insdech
	movw temp,insdecl
	andi temp,0x0E
	andi temp2,0x30
	or temp,temp2
	breq nostore
	lsr temp
	ldi zl,low(storejumps)
	ldi zh,high(storejumps)
	add zl,temp
	adc zh,_0
	icall

.if INS_DEBUG
	sbis	flags,trace
	rjmp	notrace5
	printstring " stored "
notrace5:
.endif

nostore:
	;All done. Neeeext!
	rjmp main

; --------------------------------------------------------------

; ------------ Fetch phase stuff -----------------

.equ FETCH_NOP	= (0<<0)
.equ FETCH_A	= (1<<0)
.equ FETCH_B	= (2<<0)
.equ FETCH_C	= (3<<0)
.equ FETCH_D	= (4<<0)
.equ FETCH_E	= (5<<0)
.equ FETCH_H	= (6<<0)
.equ FETCH_L	= (7<<0)
.equ FETCH_AF	= (8<<0)
.equ FETCH_BC	= (9<<0)
.equ FETCH_DE	= (10<<0)
.equ FETCH_HL	= (11<<0)
.equ FETCH_SP	= (12<<0)
.equ FETCH_MBC	= (13<<0)
.equ FETCH_MDE	= (14<<0)
.equ FETCH_MHL	= (15<<0)
.equ FETCH_MSP	= (16<<0)
.equ FETCH_DIR8	= (17<<0)
.equ FETCH_DIR16= (18<<0)
.equ FETCH_RST	= (19<<0)


;Jump table for fetch routines. Make sure to keep this in sync with the .equs!
fetchjumps:
	rjmp do_fetch_nop
	rjmp do_fetch_a
	rjmp do_fetch_b
	rjmp do_fetch_c
	rjmp do_fetch_d
	rjmp do_fetch_e
	rjmp do_fetch_h
	rjmp do_fetch_l
	rjmp do_fetch_af
	rjmp do_fetch_bc
	rjmp do_fetch_de
	rjmp do_fetch_hl
	rjmp do_fetch_sp
	rjmp do_fetch_mbc
	rjmp do_fetch_mde
	rjmp do_fetch_mhl
	rjmp do_fetch_msp
	rjmp do_fetch_dir8
	rjmp do_fetch_dir16
	rjmp do_fetch_rst

do_fetch_nop:
	ret

do_fetch_a:
	mov opl,z_a
	ret

do_fetch_b:
	lds opl,z_b
	ret

do_fetch_c:
	lds opl,z_c
	ret

do_fetch_d:
	lds opl,z_d
	ret

do_fetch_e:
	lds opl,z_e
	ret

do_fetch_h:
	lds opl,z_h
	ret

do_fetch_l:
	lds opl,z_l
	ret

do_fetch_af:
	mov opl,z_flags
	mov oph,z_a
	ret

do_fetch_bc:
	lds opl,z_c
	lds oph,z_b
	ret

do_fetch_de:
	lds opl,z_e
	lds oph,z_d
	ret

do_fetch_hl:
	lds opl,z_l
	lds oph,z_h
	ret

do_fetch_sp:
	movw opl,z_spl
	ret

do_fetch_mbc:
	lds xh,z_b
	lds xl,z_c
	mem_read_d z_a
	ret

do_fetch_mde:
	lds xh,z_d
	lds xl,z_e
	mem_read_d z_a
	ret

do_fetch_mhl:
	lds xh,z_h
	lds xl,z_l
	mem_read_d opl
	ret

do_fetch_msp:
	movw x,z_spl
	mem_read_d opl
	adiw x,1
	mem_read_d oph
	ret

do_fetch_dir8:
	mem_read_ds opl, z_pc
	adiw z_pcl,1
	ret

do_fetch_dir16:
	mem_read_ds opl, z_pc
	adiw z_pcl,1
	mem_read_ds oph, z_pc
	adiw z_pcl,1
	ret

do_fetch_rst:
	movw x,z_pcl
	sbiw x,1
	mem_read_d opl
	andi opl,0x38
	ldi oph,0
	ret
	
; ------------ Store phase stuff -----------------

.equ STORE_NOP	= (0<<5)
.equ STORE_A	= (1<<5)
.equ STORE_B	= (2<<5)
.equ STORE_C	= (3<<5)
.equ STORE_D	= (4<<5)
.equ STORE_E	= (5<<5)
.equ STORE_H	= (6<<5)
.equ STORE_L	= (7<<5)
.equ STORE_AF	= (8<<5)
.equ STORE_BC	= (9<<5)
.equ STORE_DE	= (10<<5)
.equ STORE_HL	= (11<<5)
.equ STORE_SP	= (12<<5)
.equ STORE_PC	= (13<<5)
.equ STORE_MBC	= (14<<5)
.equ STORE_MDE	= (15<<5)
.equ STORE_MHL	= (16<<5)
.equ STORE_MSP	= (17<<5)
.equ STORE_RET	= (18<<5)
.equ STORE_CALL	= (19<<5)
.equ STORE_AM	= (20<<5)

;Jump table for store routines. Make sure to keep this in sync with the .equs!
storejumps:
	rjmp do_store_nop
	rjmp do_store_a
	rjmp do_store_b
	rjmp do_store_c
	rjmp do_store_d
	rjmp do_store_e
	rjmp do_store_h
	rjmp do_store_l
	rjmp do_store_af
	rjmp do_store_bc
	rjmp do_store_de
	rjmp do_store_hl
	rjmp do_store_sp
	rjmp do_store_pc
	rjmp do_store_mbc
	rjmp do_store_mde
	rjmp do_store_mhl
	rjmp do_store_msp
	rjmp do_store_ret
	rjmp do_store_call
	rjmp do_store_am


do_store_nop:
	ret

do_store_a:
	mov z_a,opl
	ret

do_store_b:
	sts z_b,opl
	ret

do_store_c:
	sts z_c,opl
	ret

do_store_d:
	sts z_d,opl
	ret

do_store_e:
	sts z_e,opl
	ret

do_store_h:
	sts z_h,opl
	ret

do_store_l:
	sts z_l,opl
	ret

do_store_af:
	mov z_a,oph
	mov z_flags,opl
	ret

do_store_bc:
	sts z_b,oph
	sts z_c,opl
	ret

do_store_de:
	sts z_d,oph
	sts z_e,opl
	ret

do_store_hl:
	sts z_h,oph
	sts z_l,opl
	ret

do_store_mbc:
	lds xh,z_b
	lds xl,z_c
	mem_write_s z_a
	ret

do_store_mde:
	lds xh,z_d
	lds xl,z_e
	mem_write_s z_a
	ret

do_store_mhl:
	lds xh,z_h
	lds xl,z_l
	mem_write_s opl
	ret

do_store_msp:
	movw xl,z_spl
	mem_write_s opl
	adiw xl,1
	mem_write_s oph
	ret

do_store_sp:
	movw z_spl,opl
	ret

do_store_pc:
	movw z_pcl,opl
	ret

do_store_ret:
	movw	x,z_spl
	mem_read_d z_pcl
	adiw	x,1
	mem_read_d z_pch
	adiw	x,1
	movw	z_spl,x

.if STACK_DBG
	printnewline
	printstring "Stack pop  "
	movw temp,z_pcl
	rcall printhexw
	printstring ", SP is now "
	movw temp,z_spl
	rcall printhexw
	printstring ". "
.endif
	ret

do_store_call:
	movw	xl,z_spl
	sbiw	x,1
	mem_write_s z_pch
	sbiw	x,1
	mem_write_s z_pcl
	movw	z_spl,xl

.if STACK_DBG
	printnewline
	printstring "Stack push "
	movw temp,z_pcl
	rcall printhexw
	printstring ", SP is now "
	movw temp,z_spl
	rcall printhexw
	printstring ". "
.endif
	movw z_pcl,opl
	ret


do_store_am:
	mem_write_ds op, z_a
	ret


; ------------ Operation phase stuff -----------------


.equ OP_NOP	= (0<<10)
.equ OP_INC	= (1<<10)
.equ OP_DEC	= (2<<10)
.equ OP_INC16	= (3<<10)
.equ OP_DEC16	= (4<<10)
.equ OP_RLCA 	= (5<<10)
.equ OP_RRCA 	= (6<<10)
.equ OP_RRA 	= (7<<10)
.equ OP_RLA	= (8<<10)
.equ OP_ADDA	= (9<<10)
.equ OP_ADCA	= (10<<10)
.equ OP_SUBFA	= (11<<10)
.equ OP_SBCFA	= (12<<10)
.equ OP_ANDA	= (13<<10)
.equ OP_ORA	= (14<<10)
.equ OP_XORA	= (15<<10)
.equ OP_ADDHL	= (16<<10)
.equ OP_STHL	= (17<<10) ;store HL in fetched address
.equ OP_RMEM16	= (18<<10) ;read mem at fetched address
.equ OP_RMEM8	= (19<<10) ;read mem at fetched address
.equ OP_DA	= (20<<10)
.equ OP_SCF	= (21<<10)
.equ OP_CPL	= (22<<10)
.equ OP_CCF	= (23<<10)
.equ OP_POP16	= (24<<10)
.equ OP_PUSH16	= (25<<10)
.equ OP_IFNZ	= (26<<10)
.equ OP_IFZ	= (27<<10)
.equ OP_IFNC	= (28<<10)
.equ OP_IFC		= (29<<10)
.equ OP_IFPO	= (30<<10)
.equ OP_IFPE	= (31<<10)
.equ OP_IFP		= (32<<10)
.equ OP_IFM		= (33<<10)
.equ OP_OUTA	= (34<<10)
.equ OP_IN		= (35<<10)
.equ OP_EXHL	= (36<<10)
.equ OP_DI		= (37<<10)
.equ OP_EI		= (38<<10)
.equ OP_INV		= (39<<10)
.equ OP_CPFA	= (40<<10)
.equ OP_INCA	= (41<<10)
.equ OP_DECA	= (42<<10)

opjumps:
	rjmp do_op_nop
	rjmp do_op_inc
	rjmp do_op_dec
	rjmp do_op_inc16
	rjmp do_op_dec16
	rjmp do_op_rlca
	rjmp do_op_rrca
	rjmp do_op_rra
	rjmp do_op_rla
	rjmp do_op_adda
	rjmp do_op_adca
	rjmp do_op_subfa
	rjmp do_op_sbcfa
	rjmp do_op_anda
	rjmp do_op_ora
	rjmp do_op_xora
	rjmp do_op_addhl
	rjmp do_op_sthl
	rjmp do_op_rmem16
	rjmp do_op_rmem8
	rjmp do_op_da
	rjmp do_op_scf
	rjmp do_op_cpl
	rjmp do_op_ccf
	rjmp do_op_pop16
	rjmp do_op_push16
	rjmp do_op_ifnz
	rjmp do_op_ifz
	rjmp do_op_ifnc
	rjmp do_op_ifc
	rjmp do_op_ifpo
	rjmp do_op_ifpe
	rjmp do_op_ifp
	rjmp do_op_ifm
	rjmp do_op_outa
	rjmp do_op_in
	rjmp do_op_exhl
	rjmp do_op_di
	rjmp do_op_ei
	rjmp do_op_inv
	rjmp do_op_cpfa
	rjmp do_op_inca
	rjmp do_op_deca


;----------------------------------------------------------------
;|                                                              |
;|                            Zilog                             |
;|                                                              |
;|                 ZZZZZZZ    88888      000                    |
;|                      Z    8     8    0   0                   |
;|                     Z     8     8   0   0 0                  |
;|                    Z       88888    0  0  0                  |
;|                   Z       8     8   0 0   0                  |
;|                  Z        8     8    0   0                   |
;|                 ZZZZZZZ    88888      000                    |
;|                                                              |
;|          Z80 MICROPROCESSOR Instruction Set Summary          |
;|                                                              |
;----------------------------------------------------------------
;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;|----------+------+---------------------+----------------------|
;|ADC A,s   |***V0*|Add with Carry       |A=A+s+CY              |
;|ADC HL,ss |**?V0*|Add with Carry       |HL=HL+ss+CY           |
;|ADD A,s   |***V0*|Add                  |A=A+s                 |
;|ADD HL,ss |--?-0*|Add                  |HL=HL+ss              |
;|ADD IX,pp |--?-0*|Add                  |IX=IX+pp              |
;|ADD IY,rr |--?-0*|Add                  |IY=IY+rr              |
;|AND s     |**1P00|Logical AND          |A=A&s                 |
;|BIT b,m   |?*1?0-|Test Bit             |m&{2^b}               |
;|CALL cc,nn|------|Conditional Call     |If cc CALL            |
;|CALL nn   |------|Unconditional Call   |-[SP]=PC,PC=nn        |
;|CCF       |--?-0*|Complement Carry Flag|CY=~CY                |
;|CP s      |***V1*|Compare              |A-s                   |
;|CPD       |****1-|Compare and Decrement|A-[HL],HL=HL-1,BC=BC-1|
;|CPDR      |****1-|Compare, Dec., Repeat|CPD till A=[HL]or BC=0|
;|CPI       |****1-|Compare and Increment|A-[HL],HL=HL+1,BC=BC-1|
;|CPIR      |****1-|Compare, Inc., Repeat|CPI till A=[HL]or BC=0|
;|CPL       |--1-1-|Complement           |A=~A                  |
;|DAA       |***P-*|Decimal Adjust Acc.  |A=BCD format          |
;|DEC s     |***V1-|Decrement            |s=s-1                 |
;|DEC xx    |------|Decrement            |xx=xx-1               |
;|DEC ss    |------|Decrement            |ss=ss-1               |
;|DI        |------|Disable Interrupts   |                      |
;|DJNZ e    |------|Dec., Jump Non-Zero  |B=B-1 till B=0        |
;|EI        |------|Enable Interrupts    |                      |
;|EX [SP],HL|------|Exchange             |[SP]<->HL             |
;|EX [SP],xx|------|Exchange             |[SP]<->xx             |
;|EX AF,AF' |------|Exchange             |AF<->AF'              |
;|EX DE,HL  |------|Exchange             |DE<->HL               |
;|EXX       |------|Exchange             |qq<->qq'   (except AF)|
;|HALT      |------|Halt                 |                      |
;|IM n      |------|Interrupt Mode       |             (n=0,1,2)|
;|IN A,[n]  |------|Input                |A=[n]                 |
;|IN r,[C]  |***P0-|Input                |r=[C]                 |
;|INC r     |***V0-|Increment            |r=r+1                 |
;|INC [HL]  |***V0-|Increment            |[HL]=[HL]+1           |
;|INC xx    |------|Increment            |xx=xx+1               |
;|INC [xx+d]|***V0-|Increment            |[xx+d]=[xx+d]+1       |
;|INC ss    |------|Increment            |ss=ss+1               |
;|IND       |?*??1-|Input and Decrement  |[HL]=[C],HL=HL-1,B=B-1|
;|INDR      |?1??1-|Input, Dec., Repeat  |IND till B=0          |
;|INI       |?*??1-|Input and Increment  |[HL]=[C],HL=HL+1,B=B-1|
;|INIR      |?1??1-|Input, Inc., Repeat  |INI till B=0          |
;|JP [HL]   |------|Unconditional Jump   |PC=[HL]               |
;|JP [xx]   |------|Unconditional Jump   |PC=[xx]               |
;|JP nn     |------|Unconditional Jump   |PC=nn                 |
;|JP cc,nn  |------|Conditional Jump     |If cc JP              |
;|JR e      |------|Unconditional Jump   |PC=PC+e               |
;|JR cc,e   |------|Conditional Jump     |If cc JR(cc=C,NC,NZ,Z)|
;|LD dst,src|------|Load                 |dst=src               |
;|LD A,i    |**0*0-|Load                 |A=i            (i=I,R)|
;|LDD       |--0*0-|Load and Decrement   |[DE]=[HL],HL=HL-1,#   |
;|LDDR      |--000-|Load, Dec., Repeat   |LDD till BC=0         |
;|LDI       |--0*0-|Load and Increment   |[DE]=[HL],HL=HL+1,#   |
;|LDIR      |--000-|Load, Inc., Repeat   |LDI till BC=0         |
;|NEG       |***V1*|Negate               |A=-A                  |
;|NOP       |------|No Operation         |                      |
;|OR s      |**0P00|Logical inclusive OR |A=Avs                 |
;|OTDR      |?1??1-|Output, Dec., Repeat |OUTD till B=0         |
;|OTIR      |?1??1-|Output, Inc., Repeat |OUTI till B=0         |
;|OUT [C],r |------|Output               |[C]=r                 |
;|OUT [n],A |------|Output               |[n]=A                 |
;|OUTD      |?*??1-|Output and Decrement |[C]=[HL],HL=HL-1,B=B-1|
;|OUTI      |?*??1-|Output and Increment |[C]=[HL],HL=HL+1,B=B-1|
;|POP xx    |------|Pop                  |xx=[SP]+              |
;|POP qq    |------|Pop                  |qq=[SP]+              |
;|PUSH xx   |------|Push                 |-[SP]=xx              |
;|PUSH qq   |------|Push                 |-[SP]=qq              |
;|RES b,m   |------|Reset bit            |m=m&{~2^b}            |
;|RET       |------|Return               |PC=[SP]+              |
;|RET cc    |------|Conditional Return   |If cc RET             |
;|RETI      |------|Return from Interrupt|PC=[SP]+              |
;|RETN      |------|Return from NMI      |PC=[SP]+              |
;|RL m      |**0P0*|Rotate Left          |m={CY,m}<-            |
;|RLA       |--0-0*|Rotate Left Acc.     |A={CY,A}<-            |
;|RLC m     |**0P0*|Rotate Left Circular |m=m<-                 |
;|RLCA      |--0-0*|Rotate Left Circular |A=A<-                 |
;|RLD       |**0P0-|Rotate Left 4 bits   |{A,[HL]}={A,[HL]}<- ##|
;|RR m      |**0P0*|Rotate Right         |m=->{CY,m}            |
;|RRA       |--0-0*|Rotate Right Acc.    |A=->{CY,A}            |
;|RRC m     |**0P0*|Rotate Right Circular|m=->m                 |
;|RRCA      |--0-0*|Rotate Right Circular|A=->A                 |
;|RRD       |**0P0-|Rotate Right 4 bits  |{A,[HL]}=->{A,[HL]} ##|
;|RST p     |------|Restart              | (p=0H,8H,10H,...,38H)|
;|SBC A,s   |***V1*|Subtract with Carry  |A=A-s-CY              |
;|SBC HL,ss |**?V1*|Subtract with Carry  |HL=HL-ss-CY           |
;|SCF       |--0-01|Set Carry Flag       |CY=1                  |
;|SET b,m   |------|Set bit              |m=mv{2^b}             |
;|SLA m     |**0P0*|Shift Left Arithmetic|m=m*2                 |
;|SRA m     |**0P0*|Shift Right Arith.   |m=m/2                 |
;|SRL m     |**0P0*|Shift Right Logical  |m=->{0,m,CY}          |
;|SUB s     |***V1*|Subtract             |A=A-s                 |
;|XOR s     |**0P00|Logical Exclusive OR |A=Axs                 |
;|----------+------+--------------------------------------------|
;| F        |-*01? |Flag unaffected/affected/reset/set/unknown  |
;| S        |S     |Sign flag (Bit 7)                           |
;| Z        | Z    |Zero flag (Bit 6)                           |
;| HC       |  H   |Half Carry flag (Bit 4)                     |
;| P/V      |   P  |Parity/Overflow flag (Bit 2, V=overflow)    |
;| N        |    N |Add/Subtract flag (Bit 1)                   |
;| CY       |     C|Carry flag (Bit 0)                          |
;|-----------------+--------------------------------------------|
;| n               |Immediate addressing                        |
;| nn              |Immediate extended addressing               |
;| e               |Relative addressing (PC=PC+2+offset)        |
;| [nn]            |Extended addressing                         |
;| [xx+d]          |Indexed addressing                          |
;| r               |Register addressing                         |
;| [rr]            |Register indirect addressing                |
;|                 |Implied addressing                          |
;| b               |Bit addressing                              |
;| p               |Modified page zero addressing (see RST)     |
;|-----------------+--------------------------------------------|
;|DEFB n(,...)     |Define Byte(s)                              |
;|DEFB 'str'(,...) |Define Byte ASCII string(s)                 |
;|DEFS nn          |Define Storage Block                        |
;|DEFW nn(,...)    |Define Word(s)                              |
;|-----------------+--------------------------------------------|
;| A  B  C  D  E   |Registers (8-bit)                           |
;| AF  BC  DE  HL  |Register pairs (16-bit)                     |
;| F               |Flag register (8-bit)                       |
;| I               |Interrupt page address register (8-bit)     |
;| IX IY           |Index registers (16-bit)                    |
;| PC              |Program Counter register (16-bit)           |
;| R               |Memory Refresh register                     |
;| SP              |Stack Pointer register (16-bit)             |
;|-----------------+--------------------------------------------|
;| b               |One bit (0 to 7)                            |
;| cc              |Condition (C,M,NC,NZ,P,PE,PO,Z)             |
;| d               |One-byte expression (-128 to +127)          |
;| dst             |Destination s, ss, [BC], [DE], [HL], [nn]   |
;| e               |One-byte expression (-126 to +129)          |
;| m               |Any register r, [HL] or [xx+d]              |
;| n               |One-byte expression (0 to 255)              |
;| nn              |Two-byte expression (0 to 65535)            |
;| pp              |Register pair BC, DE, IX or SP              |
;| qq              |Register pair AF, BC, DE or HL              |
;| qq'             |Alternative register pair AF, BC, DE or HL  |
;| r               |Register A, B, C, D, E, H or L              |
;| rr              |Register pair BC, DE, IY or SP              |
;| s               |Any register r, value n, [HL] or [xx+d]     |
;| src             |Source s, ss, [BC], [DE], [HL], nn, [nn]    |
;| ss              |Register pair BC, DE, HL or SP              |
;| xx              |Index register IX or IY                     |
;|-----------------+--------------------------------------------|
;| +  -  *  /  ^   |Add/subtract/multiply/divide/exponent       |
;| &  ~  v  x      |Logical AND/NOT/inclusive OR/exclusive OR   |
;| <-  ->          |Rotate left/right                           |
;| [ ]             |Indirect addressing                         |
;| [ ]+  -[ ]      |Indirect addressing auto-increment/decrement|
;| { }             |Combination of operands                     |
;| #               |Also BC=BC-1,DE=DE-1                        |
;| ##              |Only lower 4 bits of accumulator A used     |
;----------------------------------------------------------------

;How the flags are supposed to work:
;7 ZFL_S - Sign flag (=MSBit of result)
;6 ZFL_Z - Zero flag. Is 1 when the result is 0
;4 ZFL_H - Half-carry (carry from bit 3 to 4)
;2 ZFL_P - Parity/2-complement Overflow
;1 ZFL_N - Subtract - set if last op was a subtract
;0 ZFL_C - Carry
;
;I sure hope I got the mapping between flags and instructions correct...

.equ ZFL_S = 7
.equ ZFL_Z = 6
.equ ZFL_H = 4
.equ ZFL_P = 2
.equ ZFL_N = 1
.equ ZFL_C = 0

.equ AVR_T = SREG_T
.equ AVR_H = SREG_H
.equ AVR_S = SREG_S
.equ AVR_V = SREG_V
.equ AVR_N = SREG_N
.equ AVR_Z = SREG_Z
.equ AVR_C = SREG_C

;------------------------------------------------;
; Load table value from flash indexed by source reg.
;
;	ldpmx	dstreg,tablebase,indexreg
;
; (6 words, 8 cycles)

.macro	ldpmx
	ldi	zh,high(@1*2)	; table must be page aligned
	mov	zl,@2		       
	lpm	@0,z	
.endm

.macro	do_z80_flags_V
#if EM_Z80
	bmov	z_flags, ZFL_P, temp, AVR_V
#endif
.endm

.macro	do_z80_flags_set_N
#if EM_Z80
	ori	z_flags, (1<<ZFL_N)       ; Negation auf 1
#endif
.endm

.macro	do_z80_flags_set_HN
#if EM_Z80
	ori 	z_flags,(1<<ZFL_N)|(1<<ZFL_H)
#endif
.endm

.macro	do_z80_flags_clear_N
#if EM_Z80
	andi	z_flags,~(1<<ZFL_N)
#endif
.endm

.macro	do_z80_flags_clear_HN
#if EM_Z80
	andi	z_flags,~((1<<ZFL_H)|(1<<ZFL_N))
#endif
.endm

	
.macro	do_z80_flags_copy_HC
#if EM_Z80
	bmov	z_flags, ZFL_H, z_flags, ZFL_H
#endif
.endm

.macro	do_z80_flags_op_rotate
	; must not change avr carry flag!
#if EM_Z80
	andi   z_flags, ~( (1<<ZFL_H) | (1<<ZFL_N) | (1<<ZFL_C) )
#else
	andi   z_flags, ~( (1<<ZFL_C) )
#endif
.endm

.macro	do_z80_flags_op_and
#if EM_Z80
	ori	z_flags,(1<<ZFL_H)
#endif
.endm

.macro	do_z80_flags_op_or
#if EM_Z80
			;nothing to do
#endif
.endm


;----------------------------------------------------------------

do_op_inv:
	sbiw	z_pcl,1
	lcall printregs
	printstring "Invalid opcode! "

haltinv:
	rjmp haltinv
	 
do_op_nop:
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|OUT [n],A |------|Output               |[n]=A                 |
;
;
;Interface with peripherials goes here :)
do_op_outa: ; out (opl),a
.if PORT_DEBUG
	printnewline
	printstring "Port write: "
	mov temp,z_a
	rcall printhex
	printstring " -> ("
	mov temp,opl
	rcall printhex
	printstring ") "
.endif
	mov temp,z_a
	mov temp2,opl
	lcall portWrite
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|IN A,[n]  |------|Input                |A=[n]                 |
;
;
do_op_in:	; in a,(opl)
.if PORT_DEBUG
	printnewline
	printstring "Port read: ("
	mov temp,opl
	rcall printhex
	printstring ") -> "
.endif

	mov temp2,opl
	lcall portRead
	mov opl,temp

.if PORT_DEBUG
	rcall printhex
	printstring " "
.endif
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|INC r     |***V0-|Increment            |r=r+1                 |
;|INC [HL]  |***V0-|Increment            |[HL]=[HL]+1           |
;|INC [xx+d]|***V0-|Increment            |[xx+d]=[xx+d]+1       |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|INC r     |**-P0-|Increment            |r=r+1                 |
;|INC [HL]  |**-P0-|Increment            |[HL]=[HL]+1           |
;
; 
do_op_inc:
	ldi	temp,1
	add	opl,temp
	in	temp, sreg
	andi	z_flags,(1<<ZFL_C)		; preserve C-flag
	ldpmx	temp2, sz53p_tab, opl
	or	z_flags,temp2		;
	bmov	z_flags, ZFL_H, temp, AVR_H
	do_z80_flags_V
	ret

do_op_inca:
	ldi	temp,1
	add	z_a,temp
	in	temp, sreg
	andi	z_flags,(1<<ZFL_C)		; preserve C-flag
	ldpmx	temp2, sz53p_tab, z_a
	or	z_flags,temp2		;
	bmov	z_flags, ZFL_H, temp, AVR_H
	do_z80_flags_V
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|DEC r     |***V1-|Decrement            |s=s-1                 |
;|INC [HL]  |***V0-|Increment            |[HL]=[HL]+1           |
;|INC [xx+d]|***V0-|Increment            |[xx+d]=[xx+d]+1       |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|DEC r     |**-P -|Increment            |r=r+1                 |
;|DEC [HL]  |**-P -|Increment            |[HL]=[HL]+1           |
;
;
do_op_dec:
	subi	opl,1
	in    temp, sreg
	andi	z_flags,(1<<ZFL_C)		; preserve C-flag
	ldpmx	temp2, sz53p_tab, opl
	or	z_flags,temp2		;
	bmov	z_flags, ZFL_H, temp, AVR_H
	do_z80_flags_V
	do_z80_flags_set_N
	ret

do_op_deca:
	ldi	opl,1
	sub	z_a,opl
	in    temp, sreg
	andi	z_flags,(1<<ZFL_C)		; preserve C-flag
	ldpmx	temp2, sz53p_tab, z_a
	or	z_flags,temp2		;
	bmov	z_flags, ZFL_H, temp, AVR_H
	do_z80_flags_V
	do_z80_flags_set_N
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|INC xx    |------|Increment            |xx=xx+1               |
;|INC ss    |------|Increment            |ss=ss+1               |
;
; 
do_op_inc16:
	subi	opl,low(-1)
	sbci	oph,high(-1)
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|DEC xx    |------|Decrement            |xx=xx-1               |
;|DEC ss    |------|Decrement            |ss=ss-1               |
;
; 
do_op_dec16:
	subi   opl, 1
	sbci   oph, 0
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|RLCA      |--0-0*|Rotate Left Circular |A=A<-                 |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|RLCA      |---- *|Rotate Left Circular |A=A<-                 |
;
;
do_op_rlca:
	;Rotate Left Cyclical. All bits move 1 to the 
	;left, the msb becomes c and lsb.
	do_z80_flags_op_rotate
	lsl	z_a
	brcc   do_op_rlc_noc
	ldi	temp,1
	or	z_a,temp
	ori    z_flags, (1<<ZFL_C)
do_op_rlc_noc:
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|RRCA      |--0-0*|Rotate Right Circular|A=->A                 |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|RRCA      |---- *|Rotate Right Circular|A=->A                 |
;
;
do_op_rrca: 
	;Rotate Right Cyclical. All bits move 1 to the 
	;right, the lsb becomes c and msb.
	do_z80_flags_op_rotate
	lsr	z_a
	brcc   do_op_rrc_noc
	ldi	temp,0x80
	or	z_a,temp
	ori    z_flags, (1<<ZFL_C)
do_op_rrc_noc:
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|RRA       |--0-0*|Rotate Right Acc.    |A=->{CY,A}            |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|RRA       |---- *|Rotate Right Acc.    |A=->{CY,A}            |
;
; 
do_op_rra: 
	;Rotate Right. All bits move 1 to the right, the lsb 
	;becomes c, c becomes msb.
	clc				; get z80 carry to avr carry
	sbrc    z_flags,ZFL_C
	sec
	do_z80_flags_op_rotate		; (clear ZFL_C, doesn't change AVR_C)
	bmov	z_flags,ZFL_C, z_a,0	; Bit 0 --> CY
	ror     z_a
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|RLA       |--0-0*|Rotate Left Acc.     |A={CY,A}<-            |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|RLA       |---- *|Rotate Left Acc.     |A={CY,A}<-            |
;
; 
do_op_rla:
	;Rotate Left. All bits move 1 to the left, the msb 
	;becomes c, c becomes lsb.
	clc
	sbrc z_flags,ZFL_C
	 sec
	do_z80_flags_op_rotate		; (clear ZFL_C, doesn't change AVR_C)
	bmov	z_flags,ZFL_C, z_a,7	; Bit 7 --> CY
	rol z_a
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|ADD A,s   |***V0*|Add                  |A=A+s                 |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|ADD A,s   |***P *|Add                  |A=A+s                 |
;
;
do_op_adda:
	add z_a,opl
	in temp,sreg
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,P flag
	bmov	z_flags,ZFL_C, temp,AVR_C
	bmov	z_flags,ZFL_H, temp,AVR_H
	do_z80_flags_V
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|ADC A,s   |***V0*|Add with Carry       |A=A+s+CY              |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|ADC A,s   |***P *|Add with Carry       |A=A+s+CY              |
;
;
do_op_adca:
	clc
	sbrc z_flags,ZFL_C
	 sec
	adc z_a,opl
	in temp,sreg
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,P
	bmov	z_flags,ZFL_C, temp,AVR_C
	bmov	z_flags,ZFL_H, temp,AVR_H
	do_z80_flags_V
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|SUB s     |***V1*|Subtract             |A=A-s                 |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|SUB s     |***P *|Subtract             |A=A-s                 |

;
do_op_subfa:
	sub z_a,opl
	in temp,sreg
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,P
	bmov	z_flags,ZFL_C, temp,AVR_C
	bmov	z_flags,ZFL_H, temp,AVR_H
	do_z80_flags_V
	do_z80_flags_set_N
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CP s      |***V1*|Compare              |A-s                   |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|CP s      |***P *|Compare              |A-s                   |

;
do_op_cpfa:
	mov temp2,z_a
	sub temp2,opl
	in temp,sreg
	ldpmx	z_flags,sz53p_tab,temp2		;S,Z,P
	bmov	z_flags,ZFL_C, temp,AVR_C
	bmov	z_flags,ZFL_H, temp,AVR_H
	do_z80_flags_V
	do_z80_flags_set_N
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|SBC A,s   |***V1*|Subtract with Carry  |A=A-s-CY              |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|SBC A,s   |***P *|Subtract with Carry  |A=A-s-CY              |
;
;
do_op_sbcfa:
	clc
	sbrc z_flags,ZFL_C
	 sec
	sbc z_a,opl
	in temp,sreg
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,P
	bmov	z_flags,ZFL_C, temp,AVR_C
	bmov	z_flags,ZFL_H, temp,AVR_H
	do_z80_flags_V
	do_z80_flags_set_N
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|AND s     |**1P00|Logical AND          |A=A&s                 |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|AND s     |**-P 0|Logical AND          |A=A&s                 |
;
;
do_op_anda:
	and z_a,opl				;
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,P,N,C
	do_z80_flags_op_and
	ret


;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|OR s      |**0P00|Logical inclusive OR |A=Avs                 |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|OR s      |**-P00|Logical inclusive OR |A=Avs                 |
;
;
do_op_ora:
	or z_a,opl
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,H,P,N,C
	do_z80_flags_op_or
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|XOR s     |**0P00|Logical Exclusive OR |A=Axs                 |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|XOR s     |**-P 0|Logical Exclusive OR |A=Axs                 |
;
;
do_op_xora:
	eor z_a,opl
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,H,P,N,C
	do_z80_flags_op_or
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|ADD HL,ss |--?-0*|Add                  |HL=HL+ss              |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|ADD HL,ss |---- *|Add                  |HL=HL+ss              |
;
;
do_op_addhl:
	lds	temp,z_l
	lds	temp2,z_h
	add opl,temp
	adc oph,temp2
	in temp,sreg
	bmov	z_flags,ZFL_H, temp,AVR_H
	bmov	z_flags,ZFL_C, temp,AVR_C
	do_z80_flags_clear_N
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|LD dst,src|------|Load                 |dst=src               |
;
;
do_op_sthl: ;store hl to mem loc in opl:h
	movw xl,opl
	lds temp,z_l
	mem_write
	adiw xl,1
	lds temp,z_h
	mem_write
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|LD dst,src|------|Load                 |dst=src               |
;
; 
do_op_rmem16:
	movw xl,opl
	mem_read_d opl
	adiw x,1
	mem_read_d oph
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|LD dst,src|------|Load                 |dst=src               |
;
;
do_op_rmem8:
	mem_read_ds opl, op
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|DAA       |***P-*|Decimal Adjust Acc.  |                      |
;|----------|SZHP C|---------- 8080 ----------------------------|
;
; Not yet checked

; Description (http://www.z80.info/z80syntx.htm#DAA):
;  This instruction conditionally adjusts the accumulator for BCD addition
;  and subtraction operations. For addition (ADD, ADC, INC) or subtraction
;  (SUB, SBC, DEC, NEC), the following table indicates the operation performed:
;
; -------------------------------------------------------------------
; |       |C Flag |HEX value in|H Flag |HEX val in | Number |C flag |
; |  Oper |Before |upper digit |Before |lower digit| added  |After  |
; |       |DAA    |(bit 7-4)   |DAA    |(bit 3-0)  | to A   |DAA    |
; |-------+-------+------------+-------+-----------+--------+-------|
; |          |    0    |     0-9      |   0    |     0-9      |   00    |   0   |
; |   ADD    |    0    |     0-8      |   0    |     A-F      |   06    |   0   |
; |          |    0    |     0-9      |   1    |     0-3      |   06    |   0   |
; |   ADC    |    0    |     A-F      |   0    |     0-9      |   60    |   1   |
; |          |    0    |     9-F      |   0    |     A-F      |   66    |   1   |
; |   INC    |    0    |     A-F      |   1    |     0-3      |   66    |   1   |
; |          |    1    |     0-2      |   0    |     0-9      |   60    |   1   |
; |          |    1    |     0-2      |   0    |     A-F      |   66    |   1   |
; |          |    1    |     0-3      |   1    |     0-3      |   66    |   1   |
; |-------+-------+------------+-------+-----------+--------+-------|
; |   SUB    |    0    |     0-9      |   0    |     0-9      |   00    |   0   |
; |   SBC    |    0    |     0-8      |   1    |     6-F      |   FA    |   0   |
; |   DEC    |    1    |     7-F      |   0    |     0-9      |   A0    |   1   |
; |   NEG    |    1    |     6-F      |   1    |     6-F      |   9A    |   1   |
; -------------------------------------------------------------------
;
; The H flag is affected as follows:
;
; ---------------------
; | N | H | low   |H' |
; |   |   |nibble |   |  
; |---+---+-------+---|
; | 0 | * |  0-9  | 0 | 
; | 0 | * |  a-f  | 1 | 
; | 1 | 0 |   *   | 0 | 
; | 1 | 1 |  6-f  | 0 | 
; | 1 | 1 |  0-5  | 1 | 
; ---------------------
;
; Ohter flags:
;     N:   Unaffected.
;     P/V: Set if Acc. is even parity after operation, reset otherwise.
;     H:   See instruction.
;     Z:   Set if Acc. is Zero after operation, reset otherwise.
;     S:   Set if most significant bit of Acc. is 1 after operation, reset otherwise.



#if 1

do_op_da:

#if EM_Z80
	sbrc	z_flags,ZFL_N			;if add-op	
	rjmp	op_da_sub			;then
#endif

op_da_add:
	ldi	temp2,0				;  new C and H flag
	sbrc	z_flags,ZFL_H			;  |
	rjmp	op_da_a01			;  if (H flag ...
	mov	temp,opl			;  |
	andi	temp,0x0f			;  |
	cpi	temp,0x0a			;  or (lower nibble >= 0x0A))
	brlo	op_da_a10			;  |
op_da_a01:					;  then
	ldi	oph,0x06			;    add 6 to lower nibble
	add	opl,oph				;    
	brhc	op_da_02			;    if 
	ori	temp2,(1<<ZFL_H)		;      set new H flag
op_da_02:					;
	brcc	op_da_a10			;    if
	ori	temp2,(1<<ZFL_C)		;      set new H flag
op_da_a10:					;  endif
	sbrc	z_flags,ZFL_C			;  |
	rjmp	op_da_a12			;  if (C flag ...
	cpi	opl,0xA0			;  |... or upper nibble >= 0xA0)
	brlo	op_da_a13			; 
op_da_a12:					;
	ldi	oph,0x60			;    add 6 to lower nibble
	add	opl,oph				;
	ori	temp2,(1<<ZFL_C)		;      set new C flag
op_da_a13:					;
	ldpmx	z_flags, sz53p_tab, opl		;  get S,Z,P flag
	or	z_flags,temp2			;  merge new C and H flag
	ret

#if EM_Z80

op_da_sub:					;else (sub-op)
	rcall do_op_inv				;  TODO: !
	ret					;endif
#endif

#else

do_op_da:
	ldi	temp2,0				;new C and H flag
	ldi	oph,0				;oph: what to add

	sbrc	z_flags,ZFL_N			; if add-op	
	rjmp	op_da_sub			;then
op_da_add:
	mov	temp,opl			;  |
	andi	temp,0x0f			;  |
	cpi	temp,0x0a			;  if (lower nibble >= 0x0A)
	brlo	op_da_a10			;  |
	ori	oph,0x06			;    add 6
	ori	temp2,(1<<ZFL_H)		;    set new H flag

	sbrc	z_flags,ZFL_C			;    |
	rjmp	op_da_a02			;    if (C flag ...
	cpi	opl,0x90			;    |... or upper nibble >= 0x90)
	brlo	op_da_a03			;    |
op_da_a02:				
	ori	oph,0x60			;      add 0x60
	ori	temp2,(1<<ZFL_C)		;      set new C flag
op_da_a03:					;    endif
	rjmp	op_da_ae
op_da_a10:					;  else (lower nibble is 0x09 or lower)
	sbrc	z_flags,ZFL_C			;    |
	rjmp	op_da_a12			;    if (C flag ...
	cpi	opl,0xA0			;    |... or upper nibble >= 0xA0)
	brlo	op_da_a13			; 
op_da_a12:				
	ori	oph,0x60			;      add 0x60
	ori	temp2,(1<<ZFL_C)		;      set new C flag
op_da_a13:
	sbrs	z_flags,ZFL_H			;    if (H flag)
	rjmp	op_da_ae			;    |
	ori	oph,0x06			;      add 0x06
	mov	temp,opl			;      |
	andi	temp,0x0f			;      |
	cpi	temp,0x06			;      if (lower nibble >= 0x0A)
	brsh	op_da_ae			;      |
	ori	temp2,(1<<ZFL_H)		;        set new H flag
						;      endif
						;    endif
op_da_ae:
	add	opl,oph
	ldpmx	z_flags, sz53p_tab, opl		; get S,Z,P flag
	or	z_flags,temp2			; merge new C and H flag
	ret
	
op_da_sub:					;else (sub-op)
	rcall do_op_inv				;  TODO: !
	ret					;endif
#endif


;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|SCF       |--0-01|Set Carry Flag       |CY=1                  |
;|----------|SZHP C|---------- 8080 ----------------------------|
;
;
do_op_scf:
	do_z80_flags_clear_HN
	ori 	z_flags,(1<<ZFL_C)
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CCF       |--?-0*|Complement Carry Flag|CY=~CY, HC=previous CY|
;|----------|SZHP C|---------- 8080 ----------------------------|
;|CCF       |---- 1|Set Carry Flag       |CY=1                  |
;
do_op_ccf:
	do_z80_flags_clear_N
	do_z80_flags_copy_HC
	ldi temp,(1<<ZFL_C)
	eor z_flags,temp
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CPL       |--1-1-|Complement           |A=~A                  |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|CPL       |---- -|Complement           |A=~A                  |
;
;
do_op_cpl:
	com z_a
	do_z80_flags_set_HN
	ret


;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|PUSH xx   |------|Push                 |-[SP]=xx              |
;|PUSH qq   |------|Push                 |-[SP]=qq              |
;
;
do_op_push16:
	movw	xl,z_spl
	sbiw	x,1
	mem_write_s oph
	sbiw	x,1
	mem_write_s opl
	movw	z_spl,xl

.if STACK_DBG
	printnewline
	printstring "Stack push "
	movw temp,opl
	rcall printhexw
	printstring ", SP is now "
	movw temp,z_spl
	rcall printhexw
	printstring ". "
.endif

	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|POP xx    |------|Pop                  |xx=[SP]+              |
;|POP qq    |------|Pop                  |qq=[SP]+              |
;
;
do_op_pop16:
	movw	x,z_spl
	mem_read_d opl
	adiw	x,1
	mem_read_d oph
	adiw	x,1
	movw	z_spl,x

.if STACK_DBG
	printnewline
	printstring "Stack pop  "
	movw temp,opl
	rcall printhexw
	printstring ", SP is now "
	movw temp,z_spl
	rcall printhexw
	printstring ". "
.endif
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|EX [SP],HL|------|Exchange             |[SP]<->HL             |
;|EX DE,HL  |------|Exchange             |DE<->HL               |
;-----------------------------Z80--------------------------------
; 
do_op_exhl:
	lds temp,z_l
	lds temp2,z_h
	sts z_l,opl
	sts z_h,oph
	movw opl,temp
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;
; TODO: Implement IFF1, IFF2
do_op_di:
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;
; TODO: Implement IFF1, IFF2
do_op_ei:
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CALL cc,nn|------|Conditional Call     |If cc CALL            |
;|JP cc,nn  |------|Conditional Jump     |If cc JP              |
;|RET cc    |------|Conditional Return   |If cc RET             |
;
;
do_op_ifnz:
	sbrs z_flags, ZFL_Z
	ret
	clr insdech
	clr insdecl
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CALL cc,nn|------|Conditional Call     |If cc CALL            |
;|JP cc,nn  |------|Conditional Jump     |If cc JP              |
;|RET cc    |------|Conditional Return   |If cc RET             |
;
;
do_op_ifz:
	sbrc z_flags, ZFL_Z
	ret
	clr insdech
	clr insdecl
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CALL cc,nn|------|Conditional Call     |If cc CALL            |
;|JP cc,nn  |------|Conditional Jump     |If cc JP              |
;|RET cc    |------|Conditional Return   |If cc RET             |
;
;
do_op_ifnc:
	sbrs z_flags, ZFL_C
	ret
	clr insdech
	clr insdecl
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CALL cc,nn|------|Conditional Call     |If cc CALL            |
;|JP cc,nn  |------|Conditional Jump     |If cc JP              |
;|RET cc    |------|Conditional Return   |If cc RET             |
;
;
do_op_ifc:
	sbrc z_flags, ZFL_C
	ret
	clr insdech
	clr insdecl
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CALL cc,nn|------|Conditional Call     |If cc CALL            |
;|JP cc,nn  |------|Conditional Jump     |If cc JP              |
;|RET cc    |------|Conditional Return   |If cc RET             |
;
;
do_op_ifpo:
	sbrs z_flags, ZFL_P
	ret
	clr insdech
	clr insdecl
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CALL cc,nn|------|Conditional Call     |If cc CALL            |
;|JP cc,nn  |------|Conditional Jump     |If cc JP              |
;|RET cc    |------|Conditional Return   |If cc RET             |
;
;
do_op_ifpe:
	sbrc z_flags, ZFL_P
	ret
	clr insdech
	clr insdecl
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CALL cc,nn|------|Conditional Call     |If cc CALL            |
;|JP cc,nn  |------|Conditional Jump     |If cc JP              |
;|RET cc    |------|Conditional Return   |If cc RET             |
;
;
do_op_ifp: ;sign positive, aka s=0
	sbrs z_flags, ZFL_S
	 ret
	clr insdech
	clr insdecl
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CALL cc,nn|------|Conditional Call     |If cc CALL            |
;|JP cc,nn  |------|Conditional Jump     |If cc JP              |
;|RET cc    |------|Conditional Return   |If cc RET             |
;
;
do_op_ifm: ;sign negative, aka s=1
	sbrc z_flags, ZFL_S
	 ret
	clr insdech
	clr insdecl
	ret

	 
; ----------------------- Opcode decoding -------------------------

; Lookup table for Z80 opcodes. Translates the first byte of the instruction word into three
; operations: fetch, do something, store.
; The table is made of 256 words. These 16-bit words consist of 
; the fetch operation (bit 0-4), the processing operation (bit 10-16) and the store 
; operation (bit 5-9).
.org (PC+255) & 0xff00
inst_table:
.dw (FETCH_NOP  | OP_NOP	| STORE_NOP)	 ; 00		NOP
.dw (FETCH_DIR16| OP_NOP	| STORE_BC )	 ; 01 nn nn	LD BC,nn
.dw (FETCH_NOP  | OP_NOP	| STORE_MBC)	 ; 02		LD (BC),A
.dw (FETCH_BC   | OP_INC16	| STORE_BC )	 ; 03		INC BC
.dw (FETCH_B    | OP_INC	| STORE_B  )	 ; 04       	INC B
.dw (FETCH_B    | OP_DEC	| STORE_B  )	 ; 05       	DEC B
.dw (FETCH_DIR8	| OP_NOP	| STORE_B  )	 ; 06 nn    	LD B,n
.dw (FETCH_NOP  | OP_RLCA	| STORE_NOP)	 ; 07       	RLCA
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; 08       	EX AF,AF'	(Z80)
.dw (FETCH_BC   | OP_ADDHL	| STORE_HL )	 ; 09       	ADD HL,BC
.dw (FETCH_MBC	| OP_NOP	| STORE_NOP)	 ; 0A       	LD A,(BC)
.dw (FETCH_BC   | OP_DEC16	| STORE_BC )	 ; 0B       	DEC BC
.dw (FETCH_C    | OP_INC	| STORE_C  )	 ; 0C       	INC C
.dw (FETCH_C    | OP_DEC	| STORE_C  )	 ; 0D       	DEC C
.dw (FETCH_DIR8 | OP_NOP	| STORE_C  )	 ; 0E nn    	LD C,n
.dw (FETCH_NOP  | OP_RRCA	| STORE_NOP)	 ; 0F       	RRCA
.dw (FETCH_NOP  | OP_INV	| STORE_NOP)	 ; 10 oo    	DJNZ o		(Z80)
.dw (FETCH_DIR16| OP_NOP	| STORE_DE )	 ; 11 nn nn	LD DE,nn
.dw (FETCH_NOP  | OP_NOP	| STORE_MDE)	 ; 12		LD (DE),A
.dw (FETCH_DE	| OP_INC16	| STORE_DE )	 ; 13		INC DE
.dw (FETCH_D	| OP_INC	| STORE_D  )	 ; 14		INC D
.dw (FETCH_D	| OP_DEC	| STORE_D  )	 ; 15		DEC D
.dw (FETCH_DIR8	| OP_NOP	| STORE_D  )	 ; 16 nn	LD D,n
.dw (FETCH_NOP  | OP_RLA	| STORE_NOP)	 ; 17		RLA
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; 18 oo	JR o		(Z80)
.dw (FETCH_DE	| OP_ADDHL	| STORE_HL )	 ; 19		ADD HL,DE
.dw (FETCH_MDE	| OP_NOP	| STORE_NOP)	 ; 1A		LD A,(DE)
.dw (FETCH_DE	| OP_DEC16	| STORE_DE )	 ; 1B		DEC DE
.dw (FETCH_E	| OP_INC	| STORE_E  )	 ; 1C		INC E
.dw (FETCH_E	| OP_DEC	| STORE_E  )	 ; 1D		DEC E
.dw (FETCH_DIR8	| OP_NOP	| STORE_E  )	 ; 1E nn	LD E,n
.dw (FETCH_NOP  | OP_RRA	| STORE_NOP)	 ; 1F		RRA
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; 20 oo	JR NZ,o		(Z80)
.dw (FETCH_DIR16| OP_NOP	| STORE_HL )	 ; 21 nn nn	LD HL,nn
.dw (FETCH_DIR16| OP_STHL	| STORE_NOP)	 ; 22 nn nn	LD (nn),HL
.dw (FETCH_HL	| OP_INC16	| STORE_HL )	 ; 23		INC HL
.dw (FETCH_H	| OP_INC	| STORE_H  )	 ; 24		INC H
.dw (FETCH_H	| OP_DEC	| STORE_H  )	 ; 25		DEC H
.dw (FETCH_DIR8	| OP_NOP	| STORE_H  )	 ; 26 nn	LD H,n
.dw (FETCH_A    | OP_DA		| STORE_A  )	 ; 27		DAA
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; 28 oo	JR Z,o		(Z80)
.dw (FETCH_HL	| OP_ADDHL	| STORE_HL )	 ; 29		ADD HL,HL
.dw (FETCH_DIR16| OP_RMEM16	| STORE_HL )	 ; 2A nn nn	LD HL,(nn)
.dw (FETCH_HL	| OP_DEC16	| STORE_HL )	 ; 2B		DEC HL
.dw (FETCH_L	| OP_INC	| STORE_L  )	 ; 2C		INC L
.dw (FETCH_L	| OP_DEC	| STORE_L  )	 ; 2D		DEC L
.dw (FETCH_DIR8	| OP_NOP	| STORE_L  )	 ; 2E nn	LD L,n
.dw (FETCH_NOP  | OP_CPL	| STORE_NOP)	 ; 2F		CPL
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; 30 oo	JR NC,o		(Z80)
.dw (FETCH_DIR16| OP_NOP	| STORE_SP )	 ; 31 nn nn	LD SP,nn
.dw (FETCH_DIR16| OP_NOP	| STORE_AM )	 ; 32 nn nn	LD (nn),A
.dw (FETCH_SP	| OP_INC16	| STORE_SP )	 ; 33		INC SP
.dw (FETCH_MHL	| OP_INC	| STORE_MHL)	 ; 34		INC (HL)
.dw (FETCH_MHL	| OP_DEC	| STORE_MHL)	 ; 35		DEC (HL)
.dw (FETCH_DIR8	| OP_NOP	| STORE_MHL)	 ; 36 nn	LD (HL),n
.dw (FETCH_NOP	| OP_SCF	| STORE_NOP)	 ; 37		SCF
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; 38 oo	JR C,o		(Z80)
.dw (FETCH_SP	| OP_ADDHL	| STORE_HL )	 ; 39		ADD HL,SP
.dw (FETCH_DIR16| OP_RMEM8	| STORE_A  )	 ; 3A nn nn	LD A,(nn)
.dw (FETCH_SP	| OP_DEC16	| STORE_SP )	 ; 3B		DEC SP
.dw (FETCH_NOP  | OP_INCA	| STORE_NOP)	 ; 3C		INC A
.dw (FETCH_NOP  | OP_DECA	| STORE_NOP)	 ; 3D		DEC A
.dw (FETCH_DIR8	| OP_NOP	| STORE_A  )	 ; 3E nn	LD A,n
.dw (FETCH_NOP	| OP_CCF	| STORE_NOP)	 ; 3F		CCF (Complement Carry Flag, gvd)
.dw (FETCH_B	| OP_NOP	| STORE_B  )	 ; 40		LD B,r
.dw (FETCH_C	| OP_NOP	| STORE_B  )	 ; 41		LD B,r
.dw (FETCH_D	| OP_NOP	| STORE_B  )	 ; 42		LD B,r
.dw (FETCH_E	| OP_NOP	| STORE_B  )	 ; 43		LD B,r
.dw (FETCH_H	| OP_NOP	| STORE_B  )	 ; 44		LD B,r
.dw (FETCH_L	| OP_NOP	| STORE_B  )	 ; 45		LD B,r
.dw (FETCH_MHL	| OP_NOP	| STORE_B  )	 ; 46		LD B,r
.dw (FETCH_A    | OP_NOP	| STORE_B  )	 ; 47		LD B,r
.dw (FETCH_B	| OP_NOP	| STORE_C  )	 ; 48		LD C,r
.dw (FETCH_C	| OP_NOP	| STORE_C  )	 ; 49		LD C,r
.dw (FETCH_D	| OP_NOP	| STORE_C  )	 ; 4A		LD C,r
.dw (FETCH_E	| OP_NOP	| STORE_C  )	 ; 4B		LD C,r
.dw (FETCH_H	| OP_NOP	| STORE_C  )	 ; 4C		LD C,r
.dw (FETCH_L	| OP_NOP	| STORE_C  )	 ; 4D		LD C,r
.dw (FETCH_MHL	| OP_NOP	| STORE_C  )	 ; 4E		LD C,r
.dw (FETCH_A    | OP_NOP	| STORE_C  )	 ; 4F		LD C,r
.dw (FETCH_B	| OP_NOP	| STORE_D  )	 ; 50		LD D,r
.dw (FETCH_C	| OP_NOP	| STORE_D  )	 ; 51		LD D,r
.dw (FETCH_D	| OP_NOP	| STORE_D  )	 ; 52		LD D,r
.dw (FETCH_E	| OP_NOP	| STORE_D  )	 ; 53		LD D,r
.dw (FETCH_H	| OP_NOP	| STORE_D  )	 ; 54		LD D,r
.dw (FETCH_L	| OP_NOP	| STORE_D  )	 ; 55		LD D,r
.dw (FETCH_MHL	| OP_NOP	| STORE_D  )	 ; 56		LD D,r
.dw (FETCH_A    | OP_NOP	| STORE_D  )	 ; 57		LD D,r
.dw (FETCH_B	| OP_NOP	| STORE_E  )	 ; 58		LD E,r
.dw (FETCH_C	| OP_NOP	| STORE_E  )	 ; 59		LD E,r
.dw (FETCH_D	| OP_NOP	| STORE_E  )	 ; 5A		LD E,r
.dw (FETCH_E	| OP_NOP	| STORE_E  )	 ; 5B		LD E,r
.dw (FETCH_H	| OP_NOP	| STORE_E  )	 ; 5C		LD E,r
.dw (FETCH_L	| OP_NOP	| STORE_E  )	 ; 5D		LD E,r
.dw (FETCH_MHL	| OP_NOP	| STORE_E  )	 ; 5E		LD E,r
.dw (FETCH_A    | OP_NOP	| STORE_E  )	 ; 5F		LD E,r
.dw (FETCH_B	| OP_NOP	| STORE_H  )	 ; 60		LD H,r
.dw (FETCH_C	| OP_NOP	| STORE_H  )	 ; 61		LD H,r
.dw (FETCH_D	| OP_NOP	| STORE_H  )	 ; 62		LD H,r
.dw (FETCH_E	| OP_NOP	| STORE_H  )	 ; 63		LD H,r
.dw (FETCH_H	| OP_NOP	| STORE_H  )	 ; 64		LD H,r
.dw (FETCH_L	| OP_NOP	| STORE_H  )	 ; 65		LD H,r
.dw (FETCH_MHL	| OP_NOP	| STORE_H  )	 ; 66		LD H,r
.dw (FETCH_A    | OP_NOP	| STORE_H  )	 ; 67		LD H,r
.dw (FETCH_B	| OP_NOP	| STORE_L  )	 ; 68		LD L,r
.dw (FETCH_C	| OP_NOP	| STORE_L  )	 ; 69		LD L,r
.dw (FETCH_D	| OP_NOP	| STORE_L  )	 ; 6A		LD L,r
.dw (FETCH_E	| OP_NOP	| STORE_L  )	 ; 6B		LD L,r
.dw (FETCH_H	| OP_NOP	| STORE_L  )	 ; 6C		LD L,r
.dw (FETCH_L	| OP_NOP	| STORE_L  )	 ; 6D		LD L,r
.dw (FETCH_MHL	| OP_NOP	| STORE_L  )	 ; 6E		LD L,r
.dw (FETCH_A    | OP_NOP	| STORE_L  )	 ; 6F		LD L,r
.dw (FETCH_B	| OP_NOP	| STORE_MHL)	 ; 70		LD (HL),r
.dw (FETCH_C	| OP_NOP	| STORE_MHL)	 ; 71		LD (HL),r
.dw (FETCH_D	| OP_NOP	| STORE_MHL)	 ; 72		LD (HL),r
.dw (FETCH_E	| OP_NOP	| STORE_MHL)	 ; 73		LD (HL),r
.dw (FETCH_H	| OP_NOP	| STORE_MHL)	 ; 74		LD (HL),r
.dw (FETCH_L	| OP_NOP	| STORE_MHL)	 ; 75		LD (HL),r
.dw (FETCH_NOP	| OP_NOP	| STORE_NOP)	 ; 76		HALT
.dw (FETCH_A    | OP_NOP	| STORE_MHL)	 ; 77		LD (HL),r
.dw (FETCH_B	| OP_NOP	| STORE_A  )	 ; 78		LD A,r
.dw (FETCH_C	| OP_NOP	| STORE_A  )	 ; 79		LD A,r
.dw (FETCH_D	| OP_NOP	| STORE_A  )	 ; 7A		LD A,r
.dw (FETCH_E	| OP_NOP	| STORE_A  )	 ; 7B		LD A,r
.dw (FETCH_H	| OP_NOP	| STORE_A  )	 ; 7C		LD A,r
.dw (FETCH_L	| OP_NOP	| STORE_A  )	 ; 7D		LD A,r
.dw (FETCH_MHL	| OP_NOP	| STORE_A  )	 ; 7E		LD A,r
.dw (FETCH_A    | OP_NOP	| STORE_A  )	 ; 7F		LD A,r
.dw (FETCH_B	| OP_ADDA	| STORE_NOP)	 ; 80		ADD A,r
.dw (FETCH_C	| OP_ADDA	| STORE_NOP)	 ; 81		ADD A,r
.dw (FETCH_D	| OP_ADDA	| STORE_NOP)	 ; 82		ADD A,r
.dw (FETCH_E	| OP_ADDA	| STORE_NOP)	 ; 83		ADD A,r
.dw (FETCH_H	| OP_ADDA	| STORE_NOP)	 ; 84		ADD A,r
.dw (FETCH_L	| OP_ADDA	| STORE_NOP)	 ; 85		ADD A,r
.dw (FETCH_MHL	| OP_ADDA	| STORE_NOP)	 ; 86		ADD A,r
.dw (FETCH_A    | OP_ADDA	| STORE_NOP)	 ; 87		ADD A,r
.dw (FETCH_B	| OP_ADCA	| STORE_NOP)	 ; 88		ADC A,r
.dw (FETCH_C	| OP_ADCA	| STORE_NOP)	 ; 89		ADC A,r
.dw (FETCH_D	| OP_ADCA	| STORE_NOP)	 ; 8A		ADC A,r
.dw (FETCH_E	| OP_ADCA	| STORE_NOP)	 ; 8B		ADC A,r
.dw (FETCH_H	| OP_ADCA	| STORE_NOP)	 ; 8C		ADC A,r
.dw (FETCH_L	| OP_ADCA	| STORE_NOP)	 ; 8D		ADC A,r
.dw (FETCH_MHL	| OP_ADCA	| STORE_NOP)	 ; 8E		ADC A,r
.dw (FETCH_A    | OP_ADCA	| STORE_NOP)	 ; 8F		ADC A,r
.dw (FETCH_B	| OP_SUBFA	| STORE_NOP)	 ; 90		SUB A,r
.dw (FETCH_C	| OP_SUBFA	| STORE_NOP)	 ; 91		SUB A,r
.dw (FETCH_D	| OP_SUBFA	| STORE_NOP)	 ; 92		SUB A,r
.dw (FETCH_E	| OP_SUBFA	| STORE_NOP)	 ; 93		SUB A,r
.dw (FETCH_H	| OP_SUBFA	| STORE_NOP)	 ; 94		SUB A,r
.dw (FETCH_L	| OP_SUBFA	| STORE_NOP)	 ; 95		SUB A,r
.dw (FETCH_MHL	| OP_SUBFA	| STORE_NOP)	 ; 96		SUB A,r
.dw (FETCH_A    | OP_SUBFA	| STORE_NOP)	 ; 97		SUB A,r
.dw (FETCH_B	| OP_SBCFA	| STORE_NOP)	 ; 98		SBC A,r
.dw (FETCH_C	| OP_SBCFA	| STORE_NOP)	 ; 99		SBC A,r
.dw (FETCH_D	| OP_SBCFA	| STORE_NOP)	 ; 9A		SBC A,r
.dw (FETCH_E	| OP_SBCFA	| STORE_NOP)	 ; 9B		SBC A,r
.dw (FETCH_H	| OP_SBCFA	| STORE_NOP)	 ; 9C		SBC A,r
.dw (FETCH_L	| OP_SBCFA	| STORE_NOP)	 ; 9D		SBC A,r
.dw (FETCH_MHL	| OP_SBCFA	| STORE_NOP)	 ; 9E		SBC A,r
.dw (FETCH_A    | OP_SBCFA	| STORE_NOP)	 ; 9F		SBC A,r
.dw (FETCH_B	| OP_ANDA	| STORE_NOP)	 ; A0		AND A,r
.dw (FETCH_C	| OP_ANDA	| STORE_NOP)	 ; A1		AND A,r
.dw (FETCH_D	| OP_ANDA	| STORE_NOP)	 ; A2		AND A,r
.dw (FETCH_E	| OP_ANDA	| STORE_NOP)	 ; A3		AND A,r
.dw (FETCH_H	| OP_ANDA	| STORE_NOP)	 ; A4		AND A,r
.dw (FETCH_L	| OP_ANDA	| STORE_NOP)	 ; A5		AND A,r
.dw (FETCH_MHL	| OP_ANDA	| STORE_NOP)	 ; A6		AND A,r
.dw (FETCH_A    | OP_ANDA	| STORE_NOP)	 ; A7		AND A,r
.dw (FETCH_B	| OP_XORA	| STORE_NOP)	 ; A8		XOR A,r
.dw (FETCH_C	| OP_XORA	| STORE_NOP)	 ; A9		XOR A,r
.dw (FETCH_D	| OP_XORA	| STORE_NOP)	 ; AA		XOR A,r
.dw (FETCH_E	| OP_XORA	| STORE_NOP)	 ; AB		XOR A,r
.dw (FETCH_H	| OP_XORA	| STORE_NOP)	 ; AC		XOR A,r
.dw (FETCH_L	| OP_XORA	| STORE_NOP)	 ; AD		XOR A,r
.dw (FETCH_MHL	| OP_XORA	| STORE_NOP)	 ; AE		XOR A,r
.dw (FETCH_A    | OP_XORA	| STORE_NOP)	 ; AF		XOR A,r
.dw (FETCH_B	| OP_ORA	| STORE_NOP)	 ; B0		OR A,r
.dw (FETCH_C	| OP_ORA	| STORE_NOP)	 ; B1		OR A,r
.dw (FETCH_D	| OP_ORA	| STORE_NOP)	 ; B2		OR A,r
.dw (FETCH_E	| OP_ORA	| STORE_NOP)	 ; B3		OR A,r
.dw (FETCH_H	| OP_ORA	| STORE_NOP)	 ; B4		OR A,r
.dw (FETCH_L	| OP_ORA	| STORE_NOP)	 ; B5		OR A,r
.dw (FETCH_MHL	| OP_ORA	| STORE_NOP)	 ; B6		OR A,r
.dw (FETCH_A    | OP_ORA	| STORE_NOP)	 ; B7		OR A,r
.dw (FETCH_B	| OP_CPFA	| STORE_NOP)	 ; B8		CP A,r
.dw (FETCH_C	| OP_CPFA	| STORE_NOP)	 ; B9		CP A,r
.dw (FETCH_D	| OP_CPFA	| STORE_NOP)	 ; BA		CP A,r
.dw (FETCH_E	| OP_CPFA	| STORE_NOP)	 ; BB		CP A,r
.dw (FETCH_H	| OP_CPFA	| STORE_NOP)	 ; BC		CP A,r
.dw (FETCH_L	| OP_CPFA	| STORE_NOP)	 ; BD		CP A,r
.dw (FETCH_MHL	| OP_CPFA	| STORE_NOP)	 ; BE		CP A,r
.dw (FETCH_A    | OP_CPFA	| STORE_NOP)	 ; BF	 	CP A,r
.dw (FETCH_NOP  | OP_IFNZ	| STORE_RET)	 ; C0		RET NZ
.dw (FETCH_NOP  | OP_POP16	| STORE_BC )	 ; C1		POP BC
.dw (FETCH_DIR16| OP_IFNZ	| STORE_PC )	 ; C2 nn nn	JP NZ,nn
.dw (FETCH_DIR16| OP_NOP	| STORE_PC )	 ; C3 nn nn	JP nn
.dw (FETCH_DIR16| OP_IFNZ	| STORE_CALL)	 ; C4 nn nn	CALL NZ,nn
.dw (FETCH_BC	| OP_PUSH16	| STORE_NOP)	 ; C5		PUSH BC
.dw (FETCH_DIR8	| OP_ADDA	| STORE_NOP)	 ; C6 nn	ADD A,n
.dw (FETCH_RST	| OP_NOP	| STORE_CALL)	 ; C7		RST 0
.dw (FETCH_NOP	| OP_IFZ	| STORE_RET)	 ; C8		RET Z
.dw (FETCH_NOP	| OP_NOP	| STORE_RET)	 ; C9		RET
.dw (FETCH_DIR16| OP_IFZ	| STORE_PC )	 ; CA nn nn	JP Z,nn
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; CB 		(Z80 specific)
.dw (FETCH_DIR16| OP_IFZ	| STORE_CALL)	 ; CC nn nn	CALL Z,nn
.dw (FETCH_DIR16| OP_NOP	| STORE_CALL)	 ; CD nn nn	CALL nn
.dw (FETCH_DIR8	| OP_ADCA	| STORE_NOP)	 ; CE nn	ADC A,n
.dw (FETCH_RST	| OP_NOP	| STORE_CALL)	 ; CF		RST 8H
.dw (FETCH_NOP	| OP_IFNC	| STORE_RET)	 ; D0		RET NC
.dw (FETCH_NOP  | OP_POP16	| STORE_DE )	 ; D1		POP DE
.dw (FETCH_DIR16| OP_IFNC	| STORE_PC )	 ; D2 nn nn	JP NC,nn
.dw (FETCH_DIR8	| OP_OUTA	| STORE_NOP)	 ; D3 nn	OUT (n),A
.dw (FETCH_DIR16| OP_IFNC	| STORE_CALL)	 ; D4 nn nn	CALL NC,nn
.dw (FETCH_DE	| OP_PUSH16	| STORE_NOP)	 ; D5		PUSH DE
.dw (FETCH_DIR8	| OP_SUBFA	| STORE_NOP)	 ; D6 nn	SUB n
.dw (FETCH_RST	| OP_NOP	| STORE_CALL)	 ; D7		RST 10H
.dw (FETCH_NOP	| OP_IFC	| STORE_RET)	 ; D8		RET C
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; D9		EXX		(Z80)
.dw (FETCH_DIR16| OP_IFC	| STORE_PC )	 ; DA nn nn	JP C,nn
.dw (FETCH_DIR8	| OP_IN 	| STORE_A  )	 ; DB nn	IN A,(n)
.dw (FETCH_DIR16| OP_IFC	| STORE_CALL)	 ; DC nn nn	CALL C,nn
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; DD 		(Z80)
.dw (FETCH_DIR8	| OP_SBCFA	| STORE_NOP)	 ; DE nn	SBC A,n
.dw (FETCH_RST	| OP_NOP	| STORE_CALL)	 ; DF		RST 18H
.dw (FETCH_NOP	| OP_IFPO	| STORE_RET)	 ; E0		RET PO
.dw (FETCH_NOP	| OP_POP16	| STORE_HL )	 ; E1		POP HL
.dw (FETCH_DIR16| OP_IFPO	| STORE_PC )	 ; E2 nn nn	JP PO,nn
.dw (FETCH_MSP	| OP_EXHL	| STORE_MSP)	 ; E3		EX (SP),HL
.dw (FETCH_DIR16| OP_IFPO	| STORE_CALL)	 ; E4 nn nn	CALL PO,nn
.dw (FETCH_HL	| OP_PUSH16	| STORE_NOP)	 ; E5		PUSH HL
.dw (FETCH_DIR8	| OP_ANDA	| STORE_NOP)	 ; E6 nn	AND n
.dw (FETCH_RST	| OP_NOP	| STORE_CALL)	 ; E7		RST 20H
.dw (FETCH_NOP	| OP_IFPE	| STORE_RET)	 ; E8		RET PE
.dw (FETCH_HL	| OP_NOP	| STORE_PC )	 ; E9		JP (HL)
.dw (FETCH_DIR16| OP_IFPE	| STORE_PC )	 ; EA nn nn	JP PE,nn
.dw (FETCH_DE	| OP_EXHL	| STORE_DE )	 ; EB		EX DE,HL
.dw (FETCH_DIR16| OP_IFPE	| STORE_CALL)	 ; EC nn nn	CALL PE,nn
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; ED		(Z80 specific)
.dw (FETCH_DIR8	| OP_XORA	| STORE_NOP)	 ; EE nn	XOR n
.dw (FETCH_RST	| OP_NOP	| STORE_CALL)	 ; EF		RST 28H
.dw (FETCH_NOP	| OP_IFP	| STORE_RET)	 ; F0		RET P
.dw (FETCH_NOP	| OP_POP16	| STORE_AF )	 ; F1		POP AF
.dw (FETCH_DIR16| OP_IFP	| STORE_PC )	 ; F2 nn nn	JP P,nn
.dw (FETCH_NOP	| OP_DI		| STORE_NOP)	 ; F3		DI
.dw (FETCH_DIR16| OP_IFP	| STORE_CALL)	 ; F4 nn nn	CALL P,nn
.dw (FETCH_AF	| OP_PUSH16	| STORE_NOP)	 ; F5		PUSH AF
.dw (FETCH_DIR8	| OP_ORA	| STORE_NOP)	 ; F6 nn	OR n
.dw (FETCH_RST	| OP_NOP	| STORE_CALL)	 ; F7		RST 30H
.dw (FETCH_NOP	| OP_IFM	| STORE_RET)	 ; F8		RET M
.dw (FETCH_HL	| OP_NOP	| STORE_SP )	 ; F9		LD SP,HL
.dw (FETCH_DIR16| OP_IFM	| STORE_PC )	 ; FA nn nn	JP M,nn
.dw (FETCH_NOP	| OP_EI 	| STORE_NOP)	 ; FB		EI
.dw (FETCH_DIR16| OP_IFM	| STORE_CALL)	 ; FC nn nn	CALL M,nn
.dw (FETCH_NOP	| OP_INV	| STORE_NOP)	 ; FD 		(Z80 specific)
.dw (FETCH_DIR8	| OP_CPFA	| STORE_NOP)	 ; FE nn	CP n
.dw (FETCH_RST	| OP_NOP	| STORE_CALL)	 ; FF		RST 38H

;----------------------------------------------------------------
; Lookup table, stolen from z80ex, Z80 emulation library.
; http://z80ex.sourceforge.net/

; The S, Z, 5 and 3 bits and the parity of the lookup value 

.org (PC+255) & 0xff00
sz53p_tab:
	.db 0x44,0x00,0x00,0x04,0x00,0x04,0x04,0x00
	.db 0x08,0x0c,0x0c,0x08,0x0c,0x08,0x08,0x0c
	.db 0x00,0x04,0x04,0x00,0x04,0x00,0x00,0x04
	.db 0x0c,0x08,0x08,0x0c,0x08,0x0c,0x0c,0x08
	.db 0x20,0x24,0x24,0x20,0x24,0x20,0x20,0x24
	.db 0x2c,0x28,0x28,0x2c,0x28,0x2c,0x2c,0x28
	.db 0x24,0x20,0x20,0x24,0x20,0x24,0x24,0x20
	.db 0x28,0x2c,0x2c,0x28,0x2c,0x28,0x28,0x2c
	.db 0x00,0x04,0x04,0x00,0x04,0x00,0x00,0x04
	.db 0x0c,0x08,0x08,0x0c,0x08,0x0c,0x0c,0x08
	.db 0x04,0x00,0x00,0x04,0x00,0x04,0x04,0x00
	.db 0x08,0x0c,0x0c,0x08,0x0c,0x08,0x08,0x0c
	.db 0x24,0x20,0x20,0x24,0x20,0x24,0x24,0x20
	.db 0x28,0x2c,0x2c,0x28,0x2c,0x28,0x28,0x2c
	.db 0x20,0x24,0x24,0x20,0x24,0x20,0x20,0x24
	.db 0x2c,0x28,0x28,0x2c,0x28,0x2c,0x2c,0x28
	.db 0x80,0x84,0x84,0x80,0x84,0x80,0x80,0x84
	.db 0x8c,0x88,0x88,0x8c,0x88,0x8c,0x8c,0x88
	.db 0x84,0x80,0x80,0x84,0x80,0x84,0x84,0x80
	.db 0x88,0x8c,0x8c,0x88,0x8c,0x88,0x88,0x8c
	.db 0xa4,0xa0,0xa0,0xa4,0xa0,0xa4,0xa4,0xa0
	.db 0xa8,0xac,0xac,0xa8,0xac,0xa8,0xa8,0xac
	.db 0xa0,0xa4,0xa4,0xa0,0xa4,0xa0,0xa0,0xa4
	.db 0xac,0xa8,0xa8,0xac,0xa8,0xac,0xac,0xa8
	.db 0x84,0x80,0x80,0x84,0x80,0x84,0x84,0x80
	.db 0x88,0x8c,0x8c,0x88,0x8c,0x88,0x88,0x8c
	.db 0x80,0x84,0x84,0x80,0x84,0x80,0x80,0x84
	.db 0x8c,0x88,0x88,0x8c,0x88,0x8c,0x8c,0x88
	.db 0xa0,0xa4,0xa4,0xa0,0xa4,0xa0,0xa0,0xa4
	.db 0xac,0xa8,0xa8,0xac,0xa8,0xac,0xac,0xa8
	.db 0xa4,0xa0,0xa0,0xa4,0xa0,0xa4,0xa4,0xa0
	.db 0xa8,0xac,0xac,0xa8,0xac,0xa8,0xa8,0xac
	
; vim:set ts=8 noet nowrap

