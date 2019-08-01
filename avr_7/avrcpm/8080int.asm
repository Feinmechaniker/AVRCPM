; 8080 Interpreter.
; This is part of the Z80-CP/M emulator written by Sprite_tm.
; 

;    Copyright (C) 2010 Sprite_tm
;    Copyright (C) 2010 Leo C.
;    Copyright (C) 2010 Horst S.

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
;    $Id: 8080int.asm 93 2014-01-03 16:32:32Z rapid $
;

;--------------------------------------------------
; Generate a table entry for one instruction
;
; 	instr fetch, op, store
;
.macro	instr	
	.db	low(@2), low(do_@1), high(do_@1), low(@0)
.endm


	.dseg
	
z_b:	.byte	1
z_c:	.byte	1
z_d:	.byte	1
z_e:	.byte	1
z_h:	.byte	1
z_l:	.byte	1

	.equ	oz_b = 0
	.equ	oz_c = 1
	.equ	oz_d = 2
	.equ	oz_e = 3
	.equ	oz_h = 4
	.equ	oz_l = 5


#if EM_Z80
z_b2:	.byte	1
z_c2:	.byte	1
z_d2:	.byte	1
z_e2:	.byte	1
z_h2:	.byte	1
z_l2:	.byte	1
z_f2:	.byte	1
z_a2:	.byte	1
	.equ r2ofs = z_b2-z_b
	.equ	oz_b2 = 6
	.equ	oz_c2 = 7
	.equ	oz_d2 = 8
	.equ	oz_e2 = 9
	.equ	oz_h2 = 10
	.equ	oz_l2 = 11
	.equ	oz_f2 = 12
	.equ	oz_a2 = 13

z_xh:	.byte	1
z_xl:	.byte	1
z_yh:	.byte	1
z_yl:	.byte	1

z_i:	.byte	1
z_r:	.byte	1
	.equ	oz_xh = 14
	.equ	oz_xl = 15
	.equ	oz_yh = 16
	.equ	oz_yl = 17
	.equ	oz_i = 18
	.equ	oz_r = 19

z_istat: .byte	1
	.equ	oz_istat = 20

	.equ IM_MASK = 0x03		;Mask IM 0..2
	.equ IM0     = 0
	.equ IM1     = 1
	.equ IM2     = 2

	.equ IFF1 = 2			;IFF1 Flag
	.equ IFF2 = 3			;IFF2 Flag

#endif

	.cseg

;Init z80
z80_init:
	ldi z_pcl,low (IPLADDR)
	ldi z_pch,high(IPLADDR)

	cbi	flags,trace
	printnewline
	printstring "Ok, CPU is live!"
	printnewline

;----------------------------------------------------------
;	1				2			3			4
;.db 	(low)do_store	(low)do_op  (hihg)do_op	(low)do_fetch
;
;das kommt auf den stapel
;  	main				da solls zum schluss weitergehen
;do_store			wohin damit		beenden mit RET
;do_op				was tun			beenden mit RET
;
;das wird direkt angesprungen
;do_fetch			woher			beenden mit RET
;
;
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
	lcall printhexw
	printstring " "
noprintpc:
.endif

.if INS_DEBUG
	sbic	flags,trace
	 rcall	printregs
.endif

	;hier kommt die Interruptbehandlung rein
	
	ldi 	zl,low(main)				;da will ich wieder hin.
	ldi 	zh,high(main)				;
	push	zl					;
	push	zh					;
	mem_read_s z_pc					;temp=memReadByte(z_pc)
	adiw	z_pcl,1					;++z_pc
	ldi	zl,low(todo_table*2)			;zhl=todo_table
	ldi	zh,high(todo_table*2)			;
	ldi	temp2,4					;1
	mul	temp,temp2				;2
	add	zl,r0					;1
	adc	zh,r1					;1
	ldi	temp2,high(store_ops)			;
	lpm	temp,Z+					;do_store 
	push	temp					;         low
	push	temp2					;         high

	lpm	temp,Z+					;do_op    
	push	temp					;         low
	lpm	temp,Z+					;         high
	push	temp					;

	lpm	zl,Z					;do_fetch
	
;mov	zh,temp2				;
	ldi	zh,high(fetch_ops)
	ijmp						;direkt



; ------------ Fetch phase stuff -----------------

.org (PC+255) & 0xff00			; wichtig !!!fetch und store muessen in einer page liegen
fetch_ops:
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

.org (PC+255) & 0xff00			; wichtig !!!fetch und store muessen in einer page liegen
store_ops:
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
;|ADD HL,ss |--*-0*|Add                  |HL=HL+ss              |
;|ADD IX,pp |--*-0*|Add                  |IX=IX+pp              |
;|ADD IY,rr |--*-0*|Add                  |IY=IY+rr              |
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
;|DI        |------|Disable Interrupts   |IFF1 = IFF2 = 0       |
;|DJNZ e    |------|Dec., Jump Non-Zero  |B=B-1 till B=0        |
;|EI        |------|Enable Interrupts    |IFF1 = IFF2 = 1       |
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
;|SBC HL,ss |***V1*|Subtract with Carry  |HL=HL-ss-CY           |
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
;ldpmx	dstreg,tablebase,indexreg
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

.macro	do_z80_flags_H
#if EM_Z80
	bmov	z_flags, ZFL_H, temp, AVR_H
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
	lcall printhex
	printstring " -> ("
	mov temp,opl
	lcall printhex
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
do_op_ina:				; in a,(opl)
.if PORT_DEBUG
	printnewline
	printstring "Port read: ("
	mov temp,opl
	lcall printhex
	printstring ") -> "
.endif

	mov temp2,opl
	lcall portRead
	mov z_a,temp

.if PORT_DEBUG
	lcall printhex
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
	sts	z_l,opl
	sts	z_h,oph
	in temp,sreg
	bmov	z_flags,ZFL_C, temp,AVR_C
	do_z80_flags_H
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

	sbrc	z_flags,ZFL_N			;if add-op	
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
	pop	temp				; nix tun
	pop temp				; direkt zurueck zu main	
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
	pop	temp				; nix tun
	pop temp				; direkt zurueck zu main	
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
	pop	temp				; nix tun
	pop temp				; direkt zuruech zu main	
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
	pop	temp				; nix tun
	pop temp				; direkt zuruech zu main	
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
	pop	temp				; nix tun
	pop temp				; direkt zuruech zu main	
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
	pop	temp				; nix tun
	pop temp				; direkt zuruech zu main	
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
	pop	temp				; nix tun
	pop temp				; direkt zuruech zu main	
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
	pop	temp				; nix tun
	pop temp				; direkt zuruech zu main	
	ret

	 
; ----------------------- Opcode decoding -------------------------

; Lookup table for Z80 opcodes. Translates the first byte of the instruction word into three
; operations: fetch, do something, store.
; The table is made of 256 words. 

;.org (PC+255) & 0xff00
todo_table:
instr 	do_fetch_nop,	op_nop,		do_store_nop	;00		;NOP
instr 	do_fetch_DIR16,	op_nop,		do_store_BC	;01 nn nn	;LD BC,nn
instr 	do_fetch_nop,	op_nop,		do_store_MBC	;02		;LD (BC),A
instr 	do_fetch_BC,	op_INC16,	do_store_BC	;03		;INC BC
instr 	do_fetch_B,	op_INC,		do_store_B	;04		;INC B
instr 	do_fetch_B,	op_DEC,		do_store_B	;05		;DEC B
instr 	do_fetch_DIR8,	op_nop,		do_store_B	;06		;LD B,n
instr 	do_fetch_nop,	op_RLCA,	do_store_nop	;07		;RLCA
instr 	do_fetch_nop,	op_INV,		do_store_nop	;08		;EX AF,AF'
instr 	do_fetch_BC,	op_ADDHL,	do_store_nop	;09		;ADD HL,BC
instr 	do_fetch_MBC,	op_nop,		do_store_nop	;0A       	;LD A,(BC)
instr 	do_fetch_BC,	op_DEC16,	do_store_BC	;0B       	;DEC BC
instr 	do_fetch_C,	op_INC,		do_store_C	;0C       	;INC C
instr 	do_fetch_C,	op_DEC,		do_store_C	;0D       	;DEC C
instr 	do_fetch_DIR8,	op_nop,		do_store_C	;0E nn    	;LD C,n
instr 	do_fetch_nop,	op_RRCA,	do_store_nop	;0F       	;RRCA
instr 	do_fetch_nop,	op_INV,		do_store_nop	;10 oo    	;DJNZ o
instr 	do_fetch_DIR16,	op_nop,		do_store_DE	;11 nn nn	;LD DE,nn
instr 	do_fetch_nop,	op_nop,		do_store_MDE	;12		;LD (DE),A
instr 	do_fetch_DE,	op_INC16,	do_store_DE	;13		;INC DE
instr 	do_fetch_D,	op_INC,		do_store_D	;14		;INC D
instr 	do_fetch_D,	op_DEC,		do_store_D	;15		;DEC D
instr 	do_fetch_DIR8,	op_nop,		do_store_D	;16 nn		;LD D,n
instr 	do_fetch_nop,	op_RLA,		do_store_nop	;17		;RLA
instr 	do_fetch_nop,	op_INV,		do_store_nop	;18 oo		;JR o
instr 	do_fetch_DE,	op_ADDHL,	do_store_nop	;19		;ADD HL,DE
instr 	do_fetch_MDE,	op_nop,		do_store_nop	;1A		;LD A,(DE)
instr 	do_fetch_DE,	op_DEC16,	do_store_DE	;1B		;DEC DE
instr 	do_fetch_E,	op_INC,		do_store_E	;1C		;INC E
instr 	do_fetch_E,	op_DEC,		do_store_E	;1D		;DEC E
instr 	do_fetch_DIR8,	op_nop,		do_store_E	;1E nn		;LD E,n
instr 	do_fetch_nop,	op_RRA,		do_store_nop	;1F		;RRA
instr 	do_fetch_nop,	op_INV,		do_store_nop	;20 oo		;JR NZ,o
instr 	do_fetch_DIR16,	op_nop,		do_store_HL	;21 nn nn	;LD HL,nn
instr 	do_fetch_DIR16,	op_STHL,	do_store_nop	;22 nn nn	;LD (nn),HL
instr 	do_fetch_HL,	op_INC16,	do_store_HL	;23		;INC HL
instr 	do_fetch_H,	op_INC,		do_store_H	;24		;INC H
instr 	do_fetch_H,	op_DEC,		do_store_H	;25		;DEC H
instr 	do_fetch_DIR8,	op_nop,		do_store_H	;26 nn		;LD H,n
instr 	do_fetch_A,	op_DA,		do_store_A	;27		;DAA
instr 	do_fetch_nop,	op_INV,		do_store_nop	;28 oo		;JR Z,o
instr 	do_fetch_HL,	op_ADDHL,	do_store_nop	;29		;ADD HL,HL
instr 	do_fetch_DIR16,	op_RMEM16,	do_store_HL	;2A nn nn	;LD HL,(nn)
instr 	do_fetch_HL,	op_DEC16,	do_store_HL	;2B		;DEC HL
instr 	do_fetch_L,	op_INC,		do_store_L	;2C		;INC L
instr 	do_fetch_L,	op_DEC,		do_store_L	;2D		;DEC L
instr 	do_fetch_DIR8,	op_nop,		do_store_L	;2E nn		;LD L,n
instr 	do_fetch_nop,	op_CPL,		do_store_nop	;2F		;CPL
instr 	do_fetch_nop,	op_INV,		do_store_nop	;30 oo		;JR NC,o
instr 	do_fetch_DIR16,	op_nop,		do_store_SP	;31 nn nn	;LD SP,nn
instr 	do_fetch_DIR16,	op_nop,		do_store_AM	;32 nn nn	;LD (nn),A
instr 	do_fetch_SP,	op_INC16,	do_store_SP	;33		;INC SP
instr 	do_fetch_MHL,	op_INC,		do_store_MHL	;34		;INC (HL)
instr 	do_fetch_MHL,	op_DEC,		do_store_MHL	;35		;DEC (HL)
instr 	do_fetch_DIR8,	op_nop,		do_store_MHL	;36 nn		;LD (HL),n
instr 	do_fetch_nop,	op_SCF,		do_store_nop	;37		;SCF
instr 	do_fetch_nop,	op_INV,		do_store_nop	;38 oo		;JR C,o
instr 	do_fetch_SP,	op_ADDHL,	do_store_nop	;39		;ADD HL,SP
instr 	do_fetch_DIR16,	op_RMEM8,	do_store_A	;3A nn nn	;LD A,(nn)
instr 	do_fetch_SP,	op_DEC16,	do_store_SP	;3B		;DEC SP
instr 	do_fetch_nop,	op_INCA,	do_store_nop	;3C		;INC A
instr 	do_fetch_nop,	op_DECA,	do_store_nop	;3D		;DEC A
instr 	do_fetch_DIR8,	op_nop,		do_store_A	;3E nn		;LD A,n
instr 	do_fetch_nop,	op_CCF,		do_store_nop	;3F		;CCF (Complement Carry Flag, gvd)
instr 	do_fetch_nop,	op_nop,		do_store_nop	;40		;LD B,B
instr 	do_fetch_C,	op_nop,		do_store_B	;41		;LD B,C
instr 	do_fetch_D,	op_nop,		do_store_B	;42		;LD B,D
instr 	do_fetch_E,	op_nop,		do_store_B	;43		;LD B,E
instr 	do_fetch_H,	op_nop,		do_store_B	;44		;LD B,H
instr 	do_fetch_L,	op_nop,		do_store_B	;45		;LD B,L
instr 	do_fetch_MHL,	op_nop,		do_store_B	;46		;LD B,(HL)
instr 	do_fetch_A,	op_nop,		do_store_B	;47		;LD B,A
instr 	do_fetch_B,	op_nop,		do_store_C	;48		;LD C,B
instr 	do_fetch_nop,	op_nop,		do_store_nop	;49		;LD C,C
instr 	do_fetch_D,	op_nop,		do_store_C	;4A		;LD C,D
instr 	do_fetch_E,	op_nop,		do_store_C	;4B		;LD C,E
instr 	do_fetch_H,	op_nop,		do_store_C	;4C		;LD C,H
instr 	do_fetch_L,	op_nop,		do_store_C	;4D		;LD C,L
instr 	do_fetch_MHL,	op_nop,		do_store_C	;4E		;LD C,(HL)
instr 	do_fetch_A,	op_nop,		do_store_C	;4F		;LD C,A
instr 	do_fetch_B,	op_nop,		do_store_D	;50		;LD D,B
instr 	do_fetch_C,	op_nop,		do_store_D	;51		;LD D,C
instr 	do_fetch_nop,	op_nop,		do_store_nop	;52		;LD D,D
instr 	do_fetch_E,	op_nop,		do_store_D	;53		;LD D,E
instr 	do_fetch_H,	op_nop,		do_store_D	;54		;LD D,H
instr 	do_fetch_L,	op_nop,		do_store_D	;55		;LD D,L
instr 	do_fetch_MHL,	op_nop,		do_store_D	;56		;LD D,(HL)
instr 	do_fetch_A,	op_nop,		do_store_D	;57		;LD D,A
instr 	do_fetch_B,	op_nop,		do_store_E	;58		;LD E,B
instr 	do_fetch_C,	op_nop,		do_store_E	;59		;LD E,C
instr 	do_fetch_D,	op_nop,		do_store_E	;5A		;LD E,D
instr 	do_fetch_nop,	op_nop,		do_store_nop	;5B		;LD E,E
instr 	do_fetch_H,	op_nop,		do_store_E	;5C		;LD E,H
instr 	do_fetch_L,	op_nop,		do_store_E	;5D		;LD E,L
instr 	do_fetch_MHL,	op_nop,		do_store_E	;5E		;LD E,(HL)
instr 	do_fetch_A,	op_nop,		do_store_E	;5F		;LD E,A
instr 	do_fetch_B,	op_nop,		do_store_H	;60		;LD H,B
instr 	do_fetch_C,	op_nop,		do_store_H	;61		;LD H,C
instr 	do_fetch_D,	op_nop,		do_store_H	;62		;LD H,D
instr 	do_fetch_E,	op_nop,		do_store_H	;63		;LD H,E
instr 	do_fetch_nop,	op_nop,		do_store_nop	;64		;LD H,H
instr 	do_fetch_L,	op_nop,		do_store_H	;65		;LD H,L
instr 	do_fetch_MHL,	op_nop,		do_store_H	;66		;LD H,(HL)
instr 	do_fetch_A,	op_nop,		do_store_H	;67		;LD H,A
instr 	do_fetch_B,	op_nop,		do_store_L	;68		;LD L,B
instr 	do_fetch_C,	op_nop,		do_store_L	;69		;LD L,C
instr 	do_fetch_D,	op_nop,		do_store_L	;6A		;LD L,D
instr 	do_fetch_E,	op_nop,		do_store_L	;6B		;LD L,E
instr 	do_fetch_H,	op_nop,		do_store_L	;6C		;LD L,H
instr 	do_fetch_nop,	op_nop,		do_store_nop	;6D		;LD L,L
instr 	do_fetch_MHL,	op_nop,		do_store_L	;6E		;LD L,(HL)
instr 	do_fetch_A,	op_nop,		do_store_L	;6F		;LD L,A
instr 	do_fetch_B,	op_nop,		do_store_MHL	;70		;LD (HL),B
instr 	do_fetch_C,	op_nop,		do_store_MHL	;71		;LD (HL),C
instr 	do_fetch_D,	op_nop,		do_store_MHL	;72		;LD (HL),D
instr 	do_fetch_E,	op_nop,		do_store_MHL	;73		;LD (HL),E
instr 	do_fetch_H,	op_nop,		do_store_MHL	;74		;LD (HL),H
instr 	do_fetch_L,	op_nop,		do_store_MHL	;75		;LD (HL),L
instr 	do_fetch_nop,	op_INV,		do_store_nop	;76		;HALT
instr 	do_fetch_A,	op_nop,		do_store_MHL	;77		;LD (HL),A
instr 	do_fetch_B,	op_nop,		do_store_A	;78		;LD A,B
instr 	do_fetch_C,	op_nop,		do_store_A	;79		;LD A,C
instr 	do_fetch_D,	op_nop,		do_store_A	;7A		;LD A,D
instr 	do_fetch_E,	op_nop,		do_store_A	;7B		;LD A,E
instr 	do_fetch_H,	op_nop,		do_store_A	;7C		;LD A,H
instr 	do_fetch_L,	op_nop,		do_store_A	;7D		;LD A,L
instr 	do_fetch_MHL,	op_nop,		do_store_A	;7E		;LD A,(HL)
instr 	do_fetch_nop,	op_nop,		do_store_nop	;7F		;LD A,A
instr 	do_fetch_B,	op_ADDA,	do_store_nop	;80		;ADD A,B
instr 	do_fetch_C,	op_ADDA,	do_store_nop	;81		;ADD A,C
instr 	do_fetch_D,	op_ADDA,	do_store_nop	;82		;ADD A,D
instr 	do_fetch_E,	op_ADDA,	do_store_nop	;83		;ADD A,E
instr 	do_fetch_H,	op_ADDA,	do_store_nop	;84		;ADD A,H
instr 	do_fetch_L,	op_ADDA,	do_store_nop	;85		;ADD A,L
instr 	do_fetch_MHL,	op_ADDA,	do_store_nop	;86		;ADD A,(HL)
instr 	do_fetch_A,	op_ADDA,	do_store_nop	;87		;ADD A,A
instr 	do_fetch_B,	op_ADCA,	do_store_nop	;88		;ADC A,B
instr 	do_fetch_C,	op_ADCA,	do_store_nop	;89		;ADC A,C
instr 	do_fetch_D,	op_ADCA,	do_store_nop	;8A		;ADC A,D
instr 	do_fetch_E,	op_ADCA,	do_store_nop	;8B		;ADC A,E
instr 	do_fetch_H,	op_ADCA,	do_store_nop	;8C		;ADC A,H
instr 	do_fetch_L,	op_ADCA,	do_store_nop	;8D		;ADC A,L
instr 	do_fetch_MHL,	op_ADCA,	do_store_nop	;8E		;ADC A,(HL)
instr 	do_fetch_A,	op_ADCA,	do_store_nop	;8F		;ADC A,A
instr 	do_fetch_B,	op_SUBFA,	do_store_nop	;90		;SUB A,B
instr 	do_fetch_C,	op_SUBFA,	do_store_nop	;91		;SUB A,C
instr 	do_fetch_D,	op_SUBFA,	do_store_nop	;92		;SUB A,D
instr 	do_fetch_E,	op_SUBFA,	do_store_nop	;93		;SUB A,E
instr 	do_fetch_H,	op_SUBFA,	do_store_nop	;94		;SUB A,H
instr 	do_fetch_L,	op_SUBFA,	do_store_nop	;95		;SUB A,L
instr 	do_fetch_MHL,	op_SUBFA,	do_store_nop	;96		;SUB A,(HL)
instr 	do_fetch_A,	op_SUBFA,	do_store_nop	;97		;SUB A,A
instr 	do_fetch_B,	op_SBCFA,	do_store_nop	;98		;SBC A,B
instr 	do_fetch_C,	op_SBCFA,	do_store_nop	;99		;SBC A,C
instr 	do_fetch_D,	op_SBCFA,	do_store_nop	;9A		;SBC A,D
instr 	do_fetch_E,	op_SBCFA,	do_store_nop	;9B		;SBC A,E
instr 	do_fetch_H,	op_SBCFA,	do_store_nop	;9C		;SBC A,H
instr 	do_fetch_L,	op_SBCFA,	do_store_nop	;9D		;SBC A,L
instr 	do_fetch_MHL,	op_SBCFA,	do_store_nop	;9E		;SBC A,(HL)
instr 	do_fetch_A,	op_SBCFA,	do_store_nop	;9F		;SBC A,A
instr 	do_fetch_B,	op_ANDA,	do_store_nop	;A0		;AND A,B
instr 	do_fetch_C,	op_ANDA,	do_store_nop	;A1		;AND A,C
instr 	do_fetch_D,	op_ANDA,	do_store_nop	;A2		;AND A,D
instr 	do_fetch_E,	op_ANDA,	do_store_nop	;A3		;AND A,E
instr 	do_fetch_H,	op_ANDA,	do_store_nop	;A4		;AND A,H
instr 	do_fetch_L,	op_ANDA,	do_store_nop	;A5		;AND A,L
instr 	do_fetch_MHL,	op_ANDA,	do_store_nop	;A6		;AND A,(HL)
instr 	do_fetch_A,	op_ANDA,	do_store_nop	;A7		;AND A,A
instr 	do_fetch_B,	op_XORA,	do_store_nop	;A8		;XOR A,B
instr 	do_fetch_C,	op_XORA,	do_store_nop	;A9		;XOR A,C
instr 	do_fetch_D,	op_XORA,	do_store_nop	;AA		;XOR A,D
instr 	do_fetch_E,	op_XORA,	do_store_nop	;AB		;XOR A,E
instr 	do_fetch_H,	op_XORA,	do_store_nop	;AC		;XOR A,H
instr 	do_fetch_L,	op_XORA,	do_store_nop	;AD		;XOR A,L
instr 	do_fetch_MHL,	op_XORA,	do_store_nop	;AE		;XOR A,(HL)
instr 	do_fetch_A,	op_XORA,	do_store_nop	;AF		;XOR A,A
instr 	do_fetch_B,	op_ORA,		do_store_nop	;B0		;OR A,B
instr 	do_fetch_C,	op_ORA,		do_store_nop	;B1		;OR A,C
instr 	do_fetch_D,	op_ORA,		do_store_nop	;B2		;OR A,D
instr 	do_fetch_E,	op_ORA,		do_store_nop	;B3		;OR A,E
instr 	do_fetch_H,	op_ORA,		do_store_nop	;B4		;OR A,H
instr 	do_fetch_L,	op_ORA,		do_store_nop	;B5		;OR A,L
instr 	do_fetch_MHL,	op_ORA,		do_store_nop	;B6		;OR A,(HL)
instr 	do_fetch_A,	op_ORA,		do_store_nop	;B7		;OR A,A
instr 	do_fetch_B,	op_CPFA,	do_store_nop	;B8		;CP A,B
instr 	do_fetch_C,	op_CPFA,	do_store_nop	;B9		;CP A,C
instr 	do_fetch_D,	op_CPFA,	do_store_nop	;BA		;CP A,D
instr 	do_fetch_E,	op_CPFA,	do_store_nop	;BB		;CP A,E
instr 	do_fetch_H,	op_CPFA,	do_store_nop	;BC		;CP A,H
instr 	do_fetch_L,	op_CPFA,	do_store_nop	;BD		;CP A,L
instr 	do_fetch_MHL,	op_CPFA,	do_store_nop	;BE		;CP A,(HL)
instr 	do_fetch_A,	op_CPFA,	do_store_nop	;BF 		;CP A,A
instr 	do_fetch_nop,	op_IFNZ,	do_store_RET	;C0		;RET NZ
instr 	do_fetch_nop,	op_POP16,	do_store_BC	;C1		;POP BC
instr 	do_fetch_DIR16,	op_IFNZ,	do_store_PC	;C2 nn nn	;JP NZ,nn
instr 	do_fetch_DIR16,	op_nop,		do_store_PC	;C3 nn nn	;JP nn
instr 	do_fetch_DIR16,	op_IFNZ,	do_store_CALL	;C4 nn nn	;CALL NZ,nn
instr 	do_fetch_BC,	op_PUSH16,	do_store_nop	;C5		;PUSH BC
instr 	do_fetch_DIR8,	op_ADDA,	do_store_nop	;C6 nn		;ADD A,n
instr 	do_fetch_RST,	op_nop,		do_store_CALL	;C7		;RST 0
instr 	do_fetch_nop,	op_IFZ,		do_store_RET	;C8		;RET Z
instr 	do_fetch_nop,	op_nop,		do_store_RET	;C9		;RET
instr 	do_fetch_DIR16,	op_IFZ,		do_store_PC	;CA nn nn	;JP Z,nn
instr 	do_fetch_nop,	op_INV,		do_store_nop	;CB 		;(Z80 specific)
instr 	do_fetch_DIR16,	op_IFZ,		do_store_CALL	;CC nn nn	;CALL Z,nn
instr 	do_fetch_DIR16,	op_nop,		do_store_CALL	;CD nn nn	;CALL nn
instr 	do_fetch_DIR8,	op_ADCA,	do_store_nop	;CE nn		;ADC A,n
instr 	do_fetch_RST,	op_nop,		do_store_CALL	;CF		;RST 8H
instr 	do_fetch_nop,	op_IFNC,	do_store_RET	;D0		;RET NC
instr 	do_fetch_nop,	op_POP16,	do_store_DE	;D1		;POP DE
instr 	do_fetch_DIR16,	op_IFNC,	do_store_PC	;D2 nn nn	;JP NC,nn
instr 	do_fetch_DIR8,	op_OUTA,	do_store_nop	;D3 nn		;OUT (n),A
instr 	do_fetch_DIR16,	op_IFNC,	do_store_CALL	;D4 nn nn	;CALL NC,nn
instr 	do_fetch_DE,	op_PUSH16,	do_store_nop	;D5		;PUSH DE
instr 	do_fetch_DIR8,	op_SUBFA,	do_store_nop	;D6 nn		;SUB n
instr 	do_fetch_RST,	op_nop,		do_store_CALL	;D7		;RST 10H
instr 	do_fetch_nop,	op_IFC,		do_store_RET	;D8		;RET C
instr 	do_fetch_nop,	op_nop,		do_store_nop	;D9		;EXX
instr 	do_fetch_DIR16,	op_IFC,		do_store_PC	;DA nn nn	;JP C,nn
instr 	do_fetch_DIR8,	op_INA,		do_store_nop	;DB nn		;IN A,(n)
instr 	do_fetch_DIR16,	op_IFC,		do_store_CALL	;DC nn nn	;CALL C,nn
instr 	do_fetch_nop,	op_INV,		do_store_nop	;DD 		;(Z80 specific)
instr 	do_fetch_DIR8,	op_SBCFA,	do_store_nop	;DE nn		;SBC A,n
instr 	do_fetch_RST,	op_nop,		do_store_CALL	;DF		;RST 18H
instr 	do_fetch_nop,	op_IFPO,	do_store_RET	;E0		;RET PO
instr 	do_fetch_nop,	op_POP16,	do_store_HL	;E1		;POP HL
instr 	do_fetch_DIR16,	op_IFPO,	do_store_PC	;E2 nn nn	;JP PO,nn
instr 	do_fetch_MSP,	op_EXHL,	do_store_MSP	;E3		;EX (SP),HL
instr 	do_fetch_DIR16,	op_IFPO,	do_store_CALL	;E4 nn nn	;CALL PO,nn
instr 	do_fetch_HL,	op_PUSH16,	do_store_nop	;E5		;PUSH HL
instr 	do_fetch_DIR8,	op_ANDA,	do_store_nop	;E6 nn		;AND n
instr 	do_fetch_RST,	op_nop,		do_store_CALL	;E7		;RST 20H
instr 	do_fetch_nop,	op_IFPE,	do_store_RET	;E8		;RET PE
instr 	do_fetch_HL,	op_nop,		do_store_PC	;E9		;JP HL
instr 	do_fetch_DIR16,	op_IFPE,	do_store_PC	;EA nn nn	;JP PE,nn
instr 	do_fetch_DE,	op_EXHL,	do_store_DE	;EB		;EX DE,HL
instr 	do_fetch_DIR16,	op_IFPE,	do_store_CALL	;EC nn nn	;CALL PE,nn
instr 	do_fetch_nop,	op_INV,		do_store_nop	;ED		;(Z80 specific)
instr 	do_fetch_DIR8,	op_XORA,	do_store_nop	;EE nn		;XOR n
instr 	do_fetch_RST,	op_nop,		do_store_CALL	;EF		;RST 28H
instr 	do_fetch_nop,	op_IFP,		do_store_RET	;F0		;RET P
instr 	do_fetch_nop,	op_POP16,	do_store_AF	;F1		;POP AF
instr 	do_fetch_DIR16,	op_IFP,		do_store_PC	;F2 nn nn	;JP P,nn
instr 	do_fetch_nop,	op_DI,		do_store_nop	;F3		;DI
instr 	do_fetch_DIR16,	op_IFP,		do_store_CALL	;F4 nn nn	;CALL P,nn
instr 	do_fetch_AF,	op_PUSH16,	do_store_nop	;F5		;PUSH AF
instr 	do_fetch_DIR8,	op_ORA,		do_store_nop	;F6 nn		;OR n
instr 	do_fetch_RST,	op_nop,		do_store_CALL	;F7		;RST 30H
instr 	do_fetch_nop,	op_IFM,		do_store_RET	;F8		;RET M
instr 	do_fetch_HL,	op_nop,		do_store_SP	;F9		;LD SP,HL
instr 	do_fetch_DIR16,	op_IFM,		do_store_PC	;FA nn nn	;JP M,nn
instr 	do_fetch_nop,	op_EI,		do_store_nop	;FB		;EI
instr 	do_fetch_DIR16,	op_IFM,		do_store_CALL	;FC nn nn	;CALL M,nn
instr 	do_fetch_nop,	op_INV,		do_store_nop	;FD 		;(Z80 specific)
instr 	do_fetch_DIR8,	op_CPFA,	do_store_nop	;FE nn		;CP n
instr 	do_fetch_RST,	op_nop,		do_store_CALL	;FF		;RST 38H


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

