; 8080/Z80 Interpreter.
; This is part of the Z80-CP/M emulator written by Sprite_tm.
; 

;    Copyright (C) 2010 Sprite_tm
;    Copyright (C) 2010-2013 Leo C.
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
;    $Id: Z80int-jmp.asm 93 2014-01-03 16:32:32Z rapid $
;

#if EM_Z80

	.dseg
z_regs:	
z_c2:
	.equ	oz_c2 = z_c2 - z_regs
	.byte	1
z_b2:
	.equ	oz_b2 = z_b2 - z_regs
	.byte	1
z_e2:
	.equ	oz_e2 = z_e2 - z_regs
	.byte	1
z_d2:
	.equ	oz_d2 = z_d2 - z_regs
	.byte	1
z_l2:
	.equ	oz_l2 = z_l2 - z_regs
	.byte	1
z_h2:
	.equ	oz_h2 = z_h2 - z_regs
	.byte	1
z_f2:
	.equ	oz_f2 = z_f2 - z_regs
	.byte	1
z_a2:
	.equ	oz_a2 = z_a2 - z_regs
	.byte	1

z_xl:
	.equ	oz_xl = z_xl - z_regs
	.byte	1
z_xh:
	.equ	oz_xh = z_xh - z_regs
	.byte	1
z_yl:
	.equ	oz_yl = z_yl - z_regs
	.byte	1
z_yh:
	.equ	oz_yh = z_yh - z_regs
	.byte	1
z_i:
	.equ	oz_i  = z_i - z_regs
	.byte	1
z_r:
	.equ	oz_r  = z_r - z_regs
	.byte	1

z_istat:
	.equ	oz_istat = z_istat - z_regs
	.byte	1

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
	ldiw	z_pc,IPLADDR
	ldiw	y,z_regs

	clr	intstat

	printnewline

.if INS_DEBUG
	sbr	intstat,(1<<i_trace)
.endif

	printstring "Ok, "CPUSTR"-CPU is live!"
	printnewline

main:

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

	;TODO: hier kommt die Interruptbehandlung rein

	cpse	intstat,_0			; 2 Fast path if no trace, int, break, ...
	rjmp	int_test

int_instr:
	mem_read_ds zl,z_pc			;11 zl = memReadByte(z_pc)
	adiw	z_pcl,1				; 2 ++z_pc
	ldi	zh,high(opcjmp)			; 1
	icall					; 3  (+4 ret)
	rjmp	main				; 2
						; / 25 cycles minimum (NOP)
int_test:
	sbrs	intstat,i_trace
	rjmp	int_notrace
	sbrc	intstat,i_halt
	rjmp	int_notrace
	cpi	z_pch,DBG_TRACE_BOTTOM
	brlo	int_notrace
	cpi	z_pch,DBG_TRACE_TOP
	brsh	int_notrace
	lcall	printregs
int_notrace:
	sbrs	intstat,i_break
	rjmp	int_nobreak
	cbr	intstat,(1<<i_break)|(1<<i_halt)
	printnewline
	printstring "Break detected! "
	mov	z_pcl,_0
	mov	z_pch,_0
int_nobreak:

	rjmp	int_instr		;Continue with normal instruction interpretation


;--------------------------------------------------
; init opcode table 
;
; 	opctable opc_name, pos
;

.macro	opctable


  .set opcjmp_table_pos_ = (@1 + 255) & -0x100		;0xff00

  .ifndef opc_tabnext_
    .set opc_tabnext_ = opcjmp_table_pos_

    .set opc_tablow_0 = 0
    .set opc_tablen_0 = 0
    .set opc_tablow_1 = 0
    .set opc_tablen_1 = 0
  .endif

  .if opcjmp_table_pos_ < opc_tabnext_
    .set opcjmp_table_pos_ = opc_tabnext_
  .endif

  .if opc_tablow_0 == 0
    .set opc_tablow_0 = opcjmp_table_pos_
    .set opc_tablen_0 = 256
;.message "add tab_0"
  .elif opc_tablow_1 == 0
    .if (opc_tablow_0 + opc_tablen_0) == opcjmp_table_pos_
      .set opc_tablen_0 = opc_tablen_0 + 256
;.message "    tab_0++"
    .else
      .set opc_tablow_1 = opcjmp_table_pos_
      .set opc_tablen_1 = 256
;.message "add tab_1"
    .endif
  .else  
    .if (opc_tablow_1 + opc_tablen_1) == opcjmp_table_pos_
      .set opc_tablen_1 = opc_tablen_1 + 256
;.message     "tab_1++"
    .else
      .error "Tab full_"
    .endif
  .endif

  .set opc_tabnext_ = opcjmp_table_pos_ + 256
  .equ @0 = opcjmp_table_pos_

.endm

;--------------------------------------------------
; 
;	checkspace frompos, size
;
.macro checkspace

  .ifdef opc_tablow_0
    .if @0 <=  opc_tablow_0
      .if (@0 + @1) >  opc_tablow_0
        .org opc_tablow_0 + opc_tablen_0
;        .message "skip tab, remove tab_0"
        .if opc_tablow_1 == 0
          .set opc_tablow_0 = 0
          .set opc_tablen_0 = 0
        .else
          .set opc_tablow_0 = opc_tablow_1
          .set opc_tablen_0 = opc_tablen_1
          .set opc_tablow_1 = 0
          .set opc_tablen_1 = 0
;          .message "remove tab_1"
        .endif
      .endif
    .endif
  .endif
.endm

;--------------------------------------------------
; Generate a table entry for one instruction
;
; 	instr fetch, op, store
;
.macro	instr	

  .set fetch_ = (do_@0 != do_fetch_nop)		; must call or jump to fetch action
  .set op_    = (do_@1 != do_op_nop)		; must call or jump to op action
  .set store_ = (do_@2 != do_store_nop)		; must jump to store action
  .set cnt_ = fetch_ + op_ + store_		; number of actions for this instruction


  .set done_ = 0
  .set pc_save_ = PC

  .if cnt_ == 0					; nothing to do (nop)
    .org opcjmp_table_pos_
    ret						; go back to main
    .org pc_save_
    .set done_ = 1
  .elif cnt_ == 1				; jump direct to action
    .if fetch_
      .set action_1_ = do_@0
    .elif op_
      .set action_1_ = do_@1
    .else
      .set action_1_ = do_@2
    .endif
    .if (opcjmp_table_pos_ - action_1_) <= 2047
       .org opcjmp_table_pos_
        rjmp action_1_				; do op and return to main
       .org pc_save_
      .set done_ = 1
    .endif
  .endif

  .if !done_

    .if defined (l_@0_@1_@2)

      .if (opcjmp_table_pos_ - l_@0_@1_@2) <= 2047
        .org opcjmp_table_pos_
        rjmp l_@0_@1_@2				; generate a jump to action table
        .org pc_save_
      .else
        checkspace pc_save_, 2
        .set pc_save_ = PC
        .org opcjmp_table_pos_
        rjmp pc_save_
        .org pc_save_
	jmp l_@0_@1_@2
      .endif

    .else

      checkspace pc_save_, 2*cnt_
      .set pc_save_ = PC

      .org opcjmp_table_pos_
      .equ l_@0_@1_@2 = pc_save_		; make a label
      rjmp l_@0_@1_@2				; generate a jump to action table

      .org l_@0_@1_@2

      .if fetch_				; must fetch
        .if op_ || store_ 
          .if do_@0 == 0
            m_do_@0
          .else
            lcall do_@0				; fetch and come back here
          .endif
        .else
          .if do_@0 == 0
            m_do_@0
            ret
          .else
            ljmp do_@0				; do op and return to main
          .endif
        .endif
      .endif
      .if op_					; must exec op
        .if store_
          .if do_@1 == 0
            m_do_@1
          .else
            lcall do_@1				; fetch and come back here
          .endif
        .else
          .if do_@1 == 0
            m_do_@1
            ret
          .else
            ljmp do_@1				; do op and return to main
          .endif
        .endif
      .endif
      .if store_				; must store
        .if do_@2 == 0
          m_do_@2
          ret
        .else
          ljmp do_@2				; store is allways last
        .endif
      .endif    

    .endif
  .endif

  .set opcjmp_table_pos_ = opcjmp_table_pos_ + 1

.endm


do_x_nop:
	ret

; ------------ Fetch phase stuff -----------------


fetch_ops:
.equ do_fetch_nop = do_x_nop

do_fetch_rst:
	movw x,z_pcl
	sbiw x,1
	mem_read_d opl
	andi opl,0x38
	ldi oph,0
	ret

.macro m_do_fetch_a
	mov opl,z_a
.endm

.equ do_fetch_a = 0
;	mov opl,z_a
;	ret

.macro m_do_fetch_b
	mov	opl,z_b
.endm

.equ do_fetch_b = 0
;	mov	opl,z_b
;	ldd opl,y+oz_b
;	ret

.macro m_do_fetch_c
	mov	opl,z_c
.endm

.equ do_fetch_c = 0
;	mov	opl,z_c
;	ldd	opl,y+oz_c
;	ret

.macro m_do_fetch_d
	mov	opl,z_d
.endm

.equ do_fetch_d = 0
;	ldd	opl,y+oz_d
;	ret

.macro m_do_fetch_e
	mov	opl,z_e
.endm

.equ do_fetch_e = 0
;	ldd	opl,y+oz_e
;	ret

.macro m_do_fetch_h
	mov	opl,z_h
.endm

.equ do_fetch_h = 0
;	mov	opl,z_h
;	ret

.macro m_do_fetch_l
	mov	opl,z_l
.endm

.equ do_fetch_l = 0
;	mov	opl,z_l
;	ret

.macro m_do_fetch_af
	movw	opl,z_flags
.endm

.equ do_fetch_af = 0
;	movw	opl,z_flags
;	ret

.macro m_do_fetch_bc
	movw	opl,z_c
.endm

.equ do_fetch_bc = 0
;	movw	opl,z_c
;	ret

.macro m_do_fetch_de
	movw	opl,z_e
.endm

.equ do_fetch_de = 0
;	movw	opl,z_e
;	ret

.macro m_do_fetch_hl
	movw	opl,z_l
.endm

.equ do_fetch_hl = 0
;	movw	opl,z_l
;	ret

.macro m_do_fetch_sp
	movw opl,z_spl
.endm

.equ do_fetch_sp = 0
;	movw opl,z_spl
;	ret

do_fetch_mbc:
;	movw	x,z_c
	mem_read_ds z_a, z_bc
	ret

do_fetch_mde:
;	movw	x,z_e
	mem_read_ds z_a, z_de
	ret

do_fetch_mhl:
;	movw	x,z_l
	mem_read_ds opl, z_hl
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

; ------------ Store phase stuff -----------------

store_ops:
.equ do_store_nop = do_x_nop

	
do_store_a:
	mov z_a,opl
	ret

;.macro m_do_store_b 
;	std	y+oz_b,opl
;.endm
;.equ do_store_b = 0
do_store_b:
	mov	z_b,opl
	ret

do_store_c:
	mov	z_c,opl
	ret

do_store_d:
	mov	z_d,opl
	ret

do_store_e:
	mov	z_e,opl
	ret

do_store_h:
	mov	z_h,opl
	ret

do_store_l:
	mov	z_l,opl
	ret

do_store_af:
	movw z_flags,opl
	ret

do_store_bc:
	movw	z_c,opl
	ret

do_store_de:
	movw	z_e,opl
;	std	y+oz_d,oph
;	std	y+oz_e,opl
	ret

do_store_hl:
	movw	z_l,opl
	ret

do_store_mbc:
;	movw	x,z_c
	mem_write_ds z_bc, z_a
	ret

do_store_mde:
;	movw	x,z_e
	mem_write_ds z_de, z_a
	ret

do_store_mhl:
;	movw	x,z_l
	mem_write_ds z_hl, opl
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

do_store_pcrel:				;add displacement to PC
#if EM_Z80
	clr	oph
	tst	opl			;sign extend
	brpl	stpcr1
	com	oph
stpcr1:
	add	z_pcl,opl
	adc	z_pch,oph
	ret
#else
	rjmp	do_op_inv
#endif

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
	movw	z_pcl,opl
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

; TODO: check Z80 flag settings

;------------------------------------------------;
; Load table value from flash indexed by source reg.
;
;	ldpmx	dstreg,tablebase,indexreg
;
; (3 words, 5 cycles)

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
	bmov	z_flags, ZFL_H, z_flags, ZFL_C
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

.equ do_op_nop = do_x_nop

do_op_inv:
	sbiw	z_pcl,1
	lcall printregs
	printstring "Invalid opcode! "

haltinv:
	rjmp haltinv


;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|HALT      |------|Halt                 |                      |


do_op_HALT:
	sbiw	z_pcl,1
	sbrc	intstat,i_halt
	ret
	sbr	intstat,(1<<i_halt)
	lcall printregs
	printstring "CPU halted! "
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
	lcall	portWrite
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|IN A,[n]  |------|Input                |A=[n]                 |
;
;
do_op_ina:				; in a,(opl)
.if PORT_DEBUG
	push	opl	
	cp	opl,_0		; don't debug port 0 (con stat)
	breq	dbg_op_ina_1
	printnewline
	printstring "Port read: ("
	mov temp,opl
	lcall printhex
	printstring ") -> "
dbg_op_ina_1:
.endif

	mov temp2,opl
	lcall portRead
	mov z_a,temp

.if PORT_DEBUG
	pop	temp
	cp	temp,_0
	breq	dbg_op_ina_2
	lcall printhex
	printstring " "
dbg_op_ina_2:
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

do_op_inc:
#if EM_Z80
#if 1
	andi	z_flags,(1<<ZFL_C)		; preserve C-, Clear N-flag
	subi	opl,-1
	in	temp,sreg
	sbrc	temp,AVR_N
	sbr	z_flags,(1<<ZFL_S)
	sbrc	temp,AVR_Z
	sbr	z_flags,(1<<ZFL_Z)
	sbrs	temp,AVR_H
	sbr	z_flags,(1<<ZFL_H)
	sbrc	temp,AVR_V
	sbr	z_flags,(1<<ZFL_P)
#else
	andi	z_flags,(1<<ZFL_C)		; preserve C-, Clear N-flag
	ldi	temp,1
	add	opl,temp
	in	temp,sreg
	bmov	z_flags,ZFL_S, temp,AVR_N
	bmov	z_flags,ZFL_Z, temp,AVR_Z
	bmov	z_flags,ZFL_H, temp,AVR_H
	bmov	z_flags,ZFL_P, temp,AVR_V
#endif
#else /* 8080 */
	andi	z_flags,(1<<ZFL_C)|(1<<ZFL_H)	; preserve C- and H-flag
	inc	opl
	ldpmx	temp,sz53p_tab,opl		;S,Z,P flag
	or	z_flags,temp
#endif
	ret

#if 0
	bst	z_flags,ZFL_C			; save C flag
	subi	opl,-1
	in	temp,sreg
	ldpmx	z_flags,flagmap_tab,temp	
	bld	z_flags,ZFL_C
	ret
#endif

do_op_inca:
#if EM_Z80
	andi	z_flags,(1<<ZFL_C)		; preserve C-, Clear N-flag
	subi	z_a,-1
	in	temp,sreg
	bmov	z_flags,ZFL_S, temp,AVR_N
	bmov	z_flags,ZFL_Z, temp,AVR_Z
	sbrs	temp,AVR_H
	sbr	z_flags,(1<<ZFL_H)
	bmov	z_flags,ZFL_P, temp,AVR_V
#else /* 8080 */
	andi	z_flags,(1<<ZFL_C)|(1<<ZFL_H)	; preserve C- and H-flag
	inc	z_a
	ldpmx	temp,sz53p_tab,z_a		; S,Z,P flag
	or	z_flags,temp
#endif
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|DEC r     |***V1-|Decrement            |s=s-1                 |
;|DEC [HL]  |***V1-|Increment            |[HL]=[HL]-1           |
;|DEC [xx+d]|***V1-|Increment            |[xx+d]=[xx+d]-1       |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|DEC r     |**-P -|Increment            |r=r-1                 |
;|DEC [HL]  |**-P -|Increment            |[HL]=[HL]-1           |
;

do_op_dec:
	subi	opl,1
#if EM_Z80
	in	temp,sreg
	bmov	z_flags,ZFL_S, temp,AVR_N
	bmov	z_flags,ZFL_Z, temp,AVR_Z
	bmov	z_flags,ZFL_H, temp,AVR_H
	bmov	z_flags,ZFL_P, temp,AVR_V
	ori	z_flags,(1<<ZFL_N)		; Set N-flag
#else /* 8080 */
	andi	z_flags,(1<<ZFL_C)|(1<<ZFL_H)	; preserve C- and H-flag
	ldpmx	temp,sz53p_tab,opl		; S,Z,P flag
	or	z_flags,temp
#endif
	ret


do_op_deca:
#if EM_Z80
	subi	z_a,1
	in	temp,sreg
	bmov	z_flags,ZFL_S, temp,AVR_N
	bmov	z_flags,ZFL_Z, temp,AVR_Z
	bmov	z_flags,ZFL_H, temp,AVR_H
	bmov	z_flags,ZFL_P, temp,AVR_V
	ori	z_flags,(1<<ZFL_N)		; Set N-flag
#else /* 8080 */
	dec	z_a
	andi	z_flags,(1<<ZFL_C)|(1<<ZFL_H)	; preserve C- and H-flag
	ldpmx	temp,sz53p_tab,z_a		; S,Z,P flag
	or	z_flags,temp
#endif
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

do_op_incHL:
	sub	z_l,_255
	sbc	z_h,_255
	ret

do_op_incDE:
	sub	z_e,_255
	sbc	z_d,_255
	ret

do_op_incBC:
	sub	z_c,_255
	sbc	z_b,_255
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

do_op_decHL:
	add	z_l,_255
	adc	z_h,_255
	ret

do_op_decDE:
	add	z_e,_255
	adc	z_d,_255
	ret

do_op_decBC:
	add	z_c,_255
	adc	z_b,_255
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
	brcc	do_op_rlc_noc
	ldi	temp,1
	or	z_a,temp
	ori	z_flags, (1<<ZFL_C)
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
	brcc	do_op_rrc_noc
	ldi	temp,0x80
	or	z_a,temp
	ori	z_flags, (1<<ZFL_C)
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
	eor	z_a,opl
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,H,P,N,C
	do_z80_flags_op_or
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|ADD HL,ss |--*-0*|Add                  |HL=HL+ss              |
;|----------|SZHP C|---------- 8080 ----------------------------|
;|ADD HL,ss |---- *|Add                  |HL=HL+ss              |
;
;
do_op_addhl:
	add	z_l,opl
	adc	z_h,oph
	in	temp,sreg
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
	movw	xl,opl
	mem_write_s z_l
	adiw	xl,1
	mem_write_s z_h
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
; |       |   0   |    0-9     |   0   |    0-9    |   00   |   0   |
; |  ADD  |   0   |    0-8     |   0   |    A-F    |   06   |   0   |
; |       |   0   |    0-9     |   1   |    0-3    |   06   |   0   |
; |  ADC  |   0   |    A-F     |   0   |    0-9    |   60   |   1   |
; |       |   0   |    9-F     |   0   |    A-F    |   66   |   1   |
; |  INC  |   0   |    A-F     |   1   |    0-3    |   66   |   1   |
; |       |   1   |    0-2     |   0   |    0-9    |   60   |   1   |
; |       |   1   |    0-2     |   0   |    A-F    |   66   |   1   |
; |       |   1   |    0-3     |   1   |    0-3    |   66   |   1   |
; |-------+-------+------------+-------+-----------+--------+-------|
; |  SUB  |   0   |    0-9     |   0   |    0-9    |   00   |   0   |
; |  SBC  |   0   |    0-8     |   1   |    6-F    |   FA   |   0   |
; |  DEC  |   1   |    7-F     |   0   |    0-9    |   A0   |   1   |
; |  NEG  |   1   |    6-F     |   1   |    6-F    |   9A   |   1   |
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



#if 0

#if EM_Z80
	sbrc	z_flags,ZFL_N			;if add-op	
	rjmp	op_da_sub			;then
#endif

do_op_DAA:
op_da_add:
	ldi	temp2,0				;  new C, H and N flag
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

op_da_sub:				;Else (sub-op)
	ldi	temp2,(1<<ZFL_N)		;  new C, H and N flag
	sbrc	z_flags,ZFL_H			;  |
	rjmp	op_da_s01			;  if (H flag ...
	mov	temp,opl			;  |
	andi	temp,0x0f			;  |
	cpi	temp,0x0a			;  or (lower nibble >= 0x0A))
	brlo	op_da_s10			;  |
op_da_s01:					;  then
	ldi	oph,0x06			;    add 6 to lower nibble
	sub	opl,oph				;    
	brhc	PC+2				;    if 
	 ori	temp2,(1<<ZFL_H)		;      set new H flag
	brcc	op_da_s10			;    if
	ori	temp2,(1<<ZFL_C)		;      set new C flag
op_da_s10:					;  endif
	sbrc	z_flags,ZFL_C			;  |
	rjmp	op_da_s12			;  if (C flag ...
	cpi	opl,0x90			;  |... or upper nibble >= 0xA0)
	brlo	op_da_s13			; 
op_da_s12:					;
	ldi	oph,0x60			;    add 6 to lower nibble
	sub	opl,oph				;
	ori	temp2,(1<<ZFL_C)		;      set new C flag
op_da_s13:					;
	ldpmx	z_flags, sz53p_tab, opl		;  get S,Z,P flag
	or	z_flags,temp2			;  merge new C and H flag
	ret

#endif

#else

do_op_DAA:
	ldi	oph,0				;oph: what to add

#if EM_Z80
	sbrc	z_flags,ZFL_N			;if add-op	
	rjmp	op_da_sub			;then
#endif

op_da_add:
	ldi	temp2,0				;  new C, H and N flag
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
	cpi	temp,0x06			;      if (lower nibble < 0x6)
	brsh	op_da_ae			;      |
;	ori	temp2,(1<<ZFL_H)		;        set new H flag
						;      endif
						;    endif
op_da_ae:
	add	opl,oph
	ldpmx	z_flags, sz53p_tab, opl		; get S,Z,P flag
	or	z_flags,temp2			; merge new C and H flag
	ret

#if EM_Z80

op_da_sub:				;Else (sub-op)
	ldi	temp2,(1<<ZFL_N)		;  new C, H and N flag
	mov	temp,opl			;  |
	andi	temp,0x0f			;  |
	cpi	temp,0x0a			;  if (lower nibble >= 0x0A)
	brlo	op_da_s10			;  |
	ori	oph,0x06			;    sub 6

	sbrc	z_flags,ZFL_C			;    |
	rjmp	op_da_s02			;    if (C flag ...
	cpi	opl,0x90			;    |... or upper nibble >= 0x90)
	brlo	op_da_s03			;    |
op_da_s02:				
	ori	oph,0x60			;      sub 0x60
	ori	temp2,(1<<ZFL_C)		;      set new C flag
op_da_s03:					;    endif
	rjmp	op_da_se
op_da_s10:					;  else (lower nibble is 0x09 or lower)
	sbrc	z_flags,ZFL_C			;    |
	rjmp	op_da_s12			;    if (C flag ...
	cpi	opl,0xA0			;    |... or upper nibble >= 0xA0)
	brlo	op_da_s13			; 
op_da_s12:				
	ori	oph,0x60			;      sub 0x60
	ori	temp2,(1<<ZFL_C)		;      set new C flag
op_da_s13:
	sbrs	z_flags,ZFL_H			;    if (H flag)
	rjmp	op_da_se			;    |
	ori	oph,0x06			;      sub 0x06
	mov	temp,opl			;      |
	andi	temp,0x0f			;      |
	cpi	temp,0x06			;      if (lower nibble < 0x06)
	brcc	op_da_se			;      |
	ori	temp2,(1<<ZFL_H)		;        set new H flag
						;      endif
						;    endif
op_da_se:
	sub	opl,oph
	ldpmx	z_flags, sz53p_tab, opl		; get S,Z,P flag
	or	z_flags,temp2			; merge new C and H flag
	ret				;Endif

#endif  /* EM_Z80 */
#endif  /* alternatives */


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
	movw	temp,z_l
	movw	z_l,opl
	movw	opl,temp
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|DI        |------|Disable Interrupts   |IFF1 = IFF2 = 0       |
;|EI        |------|Enable Interrupts    |IFF1 = IFF2 = 1       |
;

do_op_DI:
#if EM_Z80
	ldd	temp,y+oz_istat
	andi	temp,~((1<<IFF2) | (1<<IFF1))
	std	y+oz_istat,temp
#endif
	ret

do_op_EI:
#if EM_Z80
	ldd	temp,y+oz_istat
	ori	temp,(1<<IFF2) | (1<<IFF1)
	std	y+oz_istat,temp
#endif
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

;----------------------------------------------------------------

#if EM_Z80

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|DJNZ e    |------|Dec., Jump Non-Zero  |B=B-1 till B=0        |
;
;The b register is decremented, and if not zero, the signed value e is added to pc.
;The jump is measured from the start of the instruction opcode.
;e = Relative addressing (PC=PC+2+offset)

#if 1

				; (Joe G.)
do_op_DJNZ:			; decremt B, jump B=0
	dec	z_b		; B decrementieren
	breq	do_op_DJNZ_Z	; bei B=0
	subi	opl, 0x80	; z_pc + e im Zweierkomplement
	subi	z_pcl,0x80
	sbc	z_pch,_0
	add	z_pcl,opl
	adc	z_pch,_0
do_op_DJNZ_Z:
	ret

#else

do_op_djnz:
	dec	z_b
	brne	opdjnze
	pop	temp				; nix tun
	pop	temp				; direkt zurueck zu main	
opdjnze:
	ret

#endif

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|EX AF,AF' |------|Exchange             |AF<->AF'              |

do_op_EXAF:
	ldd	temp,y+oz_f2
	ldd	temp2,y+oz_a2
	std	y+oz_f2,z_flags
	std	y+oz_a2,z_a
	mov	z_flags,temp
	mov	z_a,temp2
	ret


;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|EXX       |------|Exchange             |qq<->qq'   (except AF)|


#if 1

do_op_EXX:
	ldd	temp ,y+oz_c2
	ldd	temp2,y+oz_b2
	std	y+oz_c2,z_c
	std	y+oz_b2,z_b
	movw	z_c,temp

	ldd	temp ,y+oz_e2
	ldd	temp2,y+oz_d2
	std	y+oz_e2,z_e
	std	y+oz_d2,z_d
	movw	z_e,temp

	ldd	temp ,y+oz_l2
	ldd	temp2,y+oz_h2
	std	y+oz_l2,z_l
	std	y+oz_h2,z_h
	movw	z_l,temp
	ret
#else

do_op_EXX:
	ldiw	z,z_b
	ldi	temp3,6
opexx_loop:
	ld	temp,z
	ldd	temp2,z+r2ofs
	std	z+r2ofs,temp
	st	z+,temp2
	dec	temp3
	brne	opexx_loop
	ret

#endif

#else
do_op_djnz:
do_op_EXAF:
do_op_EXX:
	ljmp	do_op_inv
	ret
#endif

#if EM_Z80

do_op_prefixED:
	mem_read_ds zl,z_pc			;zl = memReadByte(z_pc)
	adiw	z_pcl,1				;++z_pc
	ldi	zh,high(EDjmp)			;
	ijmp


do_op_prefixDD:
	cbi	flags,prefixfd
	mem_read_ds zl,z_pc			;zl = memReadByte(z_pc)
	adiw	z_pcl,1				;++z_pc
	ldi	zh,high(DDFDjmp)		;
	ijmp


do_op_prefixFD:
	sbi	flags,prefixfd
	mem_read_ds zl,z_pc			;zl = memReadByte(z_pc)
	adiw	z_pcl,1				;++z_pc
	ldi	zh,high(DDFDjmp)		;
	ijmp

do_op_prefixCB:
	mem_read_ds zl,z_pc			;zl = memReadByte(z_pc)
	adiw	z_pcl,1				;++z_pc
	ldi	zh,high(CBjmp)			;
	ijmp


do_op_prefixDDFDCB:
	sbic	flags,prefixfd
	rjmp	opprxcb_fd
	ldd	xh,y+oz_xh
	ldd	xl,y+oz_xl
	rjmp	opprxcb_1
opprxcb_fd:
	ldd	xh,y+oz_yh
	ldd	xl,y+oz_yl
opprxcb_1:
	mem_read_s z_pc				;get displacement
	adiw z_pcl,1				;++z_pc
	clr	temp2				;sign extend
	tst	temp
	brpl	PC+2
	 com	temp2
	add	xl,temp				;add displacement
	adc	xh,temp2
	mem_read_d opl

	mem_read_ds zl,z_pc			;zl = opcode
	adiw	z_pcl,1				;++z_pc
	ldi	zh,high(DDFDCBjmp)		;
	icall
	mem_write_s opl
	ret


#else		; TODO: geht das so?

do_op_prefixED:
do_op_prefixDD:
do_op_prefixFD:
do_op_prefixCB:
	ljmp	do_op_inv
	ret
#endif



; ----------------------- Opcode decoding -------------------------

; Lookup table for Z80 opcodes. Translates the first byte of the instruction word into three
; operations: fetch, do something, store.
; The table is made of 256 words. 

	opctable opcjmp, PC	;+3*256
	 
instr 	fetch_nop,	op_nop,		store_nop	;00		;NOP
instr 	fetch_DIR16,	op_nop,		store_BC	;01 nn nn	;LD BC,nn
instr 	fetch_nop,	op_nop,		store_MBC	;02		;LD (BC),A
;instr 	fetch_BC,	op_INC16,	store_BC	;03		;INC BC
instr 	fetch_nop,	op_INCBC,	store_nop	;03		;INC BC
instr 	fetch_B,	op_INC,		store_B		;04		;INC B
instr 	fetch_B,	op_DEC,		store_B		;05		;DEC B
instr 	fetch_DIR8,	op_nop,		store_B		;06		;LD B,n
instr 	fetch_nop,	op_RLCA,	store_nop	;07		;RLCA
instr 	fetch_nop,	op_EXAF,	store_nop	;08		;EX AF,AF'
instr 	fetch_BC,	op_ADDHL,	store_nop	;09		;ADD HL,BC
instr 	fetch_MBC,	op_nop,		store_nop	;0A       	;LD A,(BC)
;instr 	fetch_BC,	op_DEC16,	store_BC	;0B       	;DEC BC
instr 	fetch_nop,	op_DECBC,	store_nop	;0B		;DEC BC
instr 	fetch_C,	op_INC,		store_C		;0C       	;INC C
instr 	fetch_C,	op_DEC,		store_C		;0D       	;DEC C
instr 	fetch_DIR8,	op_nop,		store_C		;0E nn    	;LD C,n
instr 	fetch_nop,	op_RRCA,	store_nop	;0F       	;RRCA
instr 	fetch_DIR8,	op_DJNZ,	store_nop	;10 oo    	;DJNZ o
instr 	fetch_DIR16,	op_nop,		store_DE	;11 nn nn	;LD DE,nn
instr 	fetch_nop,	op_nop,		store_MDE	;12		;LD (DE),A
;instr 	fetch_DE,	op_INC16,	store_DE	;13		;INC DE
instr 	fetch_nop,	op_INCDE,	store_nop	;13		;INC DE
instr 	fetch_D,	op_INC,		store_D		;14		;INC D
instr 	fetch_D,	op_DEC,		store_D		;15		;DEC D
instr 	fetch_DIR8,	op_nop,		store_D		;16 nn		;LD D,n
instr 	fetch_nop,	op_RLA,		store_nop	;17		;RLA
instr 	fetch_DIR8,	op_nop,		store_pcrel	;18 oo		;JR o
instr 	fetch_DE,	op_ADDHL,	store_nop	;19		;ADD HL,DE
instr 	fetch_MDE,	op_nop,		store_nop	;1A		;LD A,(DE)
;instr 	fetch_DE,	op_DEC16,	store_DE	;1B		;DEC DE
instr 	fetch_nop,	op_DECDE,	store_nop	;1B		;DEC DE
instr 	fetch_E,	op_INC,		store_E		;1C		;INC E
instr 	fetch_E,	op_DEC,		store_E		;1D		;DEC E
instr 	fetch_DIR8,	op_nop,		store_E		;1E nn		;LD E,n
instr 	fetch_nop,	op_RRA,		store_nop	;1F		;RRA
instr 	fetch_DIR8,	op_IFNZ,	store_pcrel	;20 oo		;JR NZ,o
instr 	fetch_DIR16,	op_nop,		store_HL	;21 nn nn	;LD HL,nn
instr 	fetch_DIR16,	op_STHL,	store_nop	;22 nn nn	;LD (nn),HL
;instr 	fetch_HL,	op_INC16,	store_HL	;23		;INC HL
instr 	fetch_nop,	op_INCHL,	store_nop	;23		;INC HL
instr 	fetch_H,	op_INC,		store_H		;24		;INC H
instr 	fetch_H,	op_DEC,		store_H		;25		;DEC H
instr 	fetch_DIR8,	op_nop,		store_H		;26 nn		;LD H,n
instr 	fetch_A,	op_DAA,		store_A		;27		;DAA
instr 	fetch_DIR8,	op_IFZ,		store_pcrel	;28 oo		;JR Z,o
instr 	fetch_HL,	op_ADDHL,	store_nop	;29		;ADD HL,HL
instr 	fetch_DIR16,	op_RMEM16,	store_HL	;2A nn nn	;LD HL,(nn)
;instr 	fetch_HL,	op_DEC16,	store_HL	;2B		;DEC HL
instr 	fetch_nop,	op_DECHL,	store_nop	;2B		;DEC HL
instr 	fetch_L,	op_INC,		store_L		;2C		;INC L
instr 	fetch_L,	op_DEC,		store_L		;2D		;DEC L
instr 	fetch_DIR8,	op_nop,		store_L		;2E nn		;LD L,n
instr 	fetch_nop,	op_CPL,		store_nop	;2F		;CPL
instr 	fetch_DIR8,	op_IFNC,	store_pcrel	;30 oo		;JR NC,o
instr 	fetch_DIR16,	op_nop,		store_SP	;31 nn nn	;LD SP,nn
instr 	fetch_DIR16,	op_nop,		store_AM	;32 nn nn	;LD (nn),A
instr 	fetch_SP,	op_INC16,	store_SP	;33		;INC SP
instr 	fetch_MHL,	op_INC,		store_MHL	;34		;INC (HL)
instr 	fetch_MHL,	op_DEC,		store_MHL	;35		;DEC (HL)
instr 	fetch_DIR8,	op_nop,		store_MHL	;36 nn		;LD (HL),n
instr 	fetch_nop,	op_SCF,		store_nop	;37		;SCF
instr 	fetch_DIR8,	op_IFC,		store_pcrel	;38 oo		;JR C,o
instr 	fetch_SP,	op_ADDHL,	store_nop	;39		;ADD HL,SP
instr 	fetch_DIR16,	op_RMEM8,	store_A		;3A nn nn	;LD A,(nn)
instr 	fetch_SP,	op_DEC16,	store_SP	;3B		;DEC SP
instr 	fetch_nop,	op_INCA,	store_nop	;3C		;INC A
instr 	fetch_nop,	op_DECA,	store_nop	;3D		;DEC A
instr 	fetch_DIR8,	op_nop,		store_A		;3E nn		;LD A,n
instr 	fetch_nop,	op_CCF,		store_nop	;3F		;CCF (Complement Carry Flag, gvd)
instr 	fetch_nop,	op_nop,		store_nop	;40		;LD B,B
instr 	fetch_C,	op_nop,		store_B		;41		;LD B,C
instr 	fetch_D,	op_nop,		store_B		;42		;LD B,D
instr 	fetch_E,	op_nop,		store_B		;43		;LD B,E
instr 	fetch_H,	op_nop,		store_B		;44		;LD B,H
instr 	fetch_L,	op_nop,		store_B		;45		;LD B,L
instr 	fetch_MHL,	op_nop,		store_B		;46		;LD B,(HL)
instr 	fetch_A,	op_nop,		store_B		;47		;LD B,A
instr 	fetch_B,	op_nop,		store_C		;48		;LD C,B
instr 	fetch_nop,	op_nop,		store_nop	;49		;LD C,C
instr 	fetch_D,	op_nop,		store_C		;4A		;LD C,D
instr 	fetch_E,	op_nop,		store_C		;4B		;LD C,E
instr 	fetch_H,	op_nop,		store_C		;4C		;LD C,H
instr 	fetch_L,	op_nop,		store_C		;4D		;LD C,L
instr 	fetch_MHL,	op_nop,		store_C		;4E		;LD C,(HL)
instr 	fetch_A,	op_nop,		store_C		;4F		;LD C,A
instr 	fetch_B,	op_nop,		store_D		;50		;LD D,B
instr 	fetch_C,	op_nop,		store_D		;51		;LD D,C
instr 	fetch_nop,	op_nop,		store_nop	;52		;LD D,D
instr 	fetch_E,	op_nop,		store_D		;53		;LD D,E
instr 	fetch_H,	op_nop,		store_D		;54		;LD D,H
instr 	fetch_L,	op_nop,		store_D		;55		;LD D,L
instr 	fetch_MHL,	op_nop,		store_D		;56		;LD D,(HL)
instr 	fetch_A,	op_nop,		store_D		;57		;LD D,A
instr 	fetch_B,	op_nop,		store_E		;58		;LD E,B
instr 	fetch_C,	op_nop,		store_E		;59		;LD E,C
instr 	fetch_D,	op_nop,		store_E		;5A		;LD E,D
instr 	fetch_nop,	op_nop,		store_nop	;5B		;LD E,E
instr 	fetch_H,	op_nop,		store_E		;5C		;LD E,H
instr 	fetch_L,	op_nop,		store_E		;5D		;LD E,L
instr 	fetch_MHL,	op_nop,		store_E		;5E		;LD E,(HL)
instr 	fetch_A,	op_nop,		store_E		;5F		;LD E,A
instr 	fetch_B,	op_nop,		store_H		;60		;LD H,B
instr 	fetch_C,	op_nop,		store_H		;61		;LD H,C
instr 	fetch_D,	op_nop,		store_H		;62		;LD H,D
instr 	fetch_E,	op_nop,		store_H		;63		;LD H,E
instr 	fetch_nop,	op_nop,		store_nop	;64		;LD H,H
instr 	fetch_L,	op_nop,		store_H		;65		;LD H,L
instr 	fetch_MHL,	op_nop,		store_H		;66		;LD H,(HL)
instr 	fetch_A,	op_nop,		store_H		;67		;LD H,A
instr 	fetch_B,	op_nop,		store_L		;68		;LD L,B
instr 	fetch_C,	op_nop,		store_L		;69		;LD L,C
instr 	fetch_D,	op_nop,		store_L		;6A		;LD L,D
instr 	fetch_E,	op_nop,		store_L		;6B		;LD L,E
instr 	fetch_H,	op_nop,		store_L		;6C		;LD L,H
instr 	fetch_nop,	op_nop,		store_nop	;6D		;LD L,L
instr 	fetch_MHL,	op_nop,		store_L		;6E		;LD L,(HL)
instr 	fetch_A,	op_nop,		store_L		;6F		;LD L,A
instr 	fetch_B,	op_nop,		store_MHL	;70		;LD (HL),B
instr 	fetch_C,	op_nop,		store_MHL	;71		;LD (HL),C
instr 	fetch_D,	op_nop,		store_MHL	;72		;LD (HL),D
instr 	fetch_E,	op_nop,		store_MHL	;73		;LD (HL),E
instr 	fetch_H,	op_nop,		store_MHL	;74		;LD (HL),H
instr 	fetch_L,	op_nop,		store_MHL	;75		;LD (HL),L
instr 	fetch_nop,	op_HALT,	store_nop	;76		;HALT
instr 	fetch_A,	op_nop,		store_MHL	;77		;LD (HL),A
instr 	fetch_B,	op_nop,		store_A		;78		;LD A,B
instr 	fetch_C,	op_nop,		store_A		;79		;LD A,C
instr 	fetch_D,	op_nop,		store_A		;7A		;LD A,D
instr 	fetch_E,	op_nop,		store_A		;7B		;LD A,E
instr 	fetch_H,	op_nop,		store_A		;7C		;LD A,H
instr 	fetch_L,	op_nop,		store_A		;7D		;LD A,L
instr 	fetch_MHL,	op_nop,		store_A		;7E		;LD A,(HL)
instr 	fetch_nop,	op_nop,		store_nop	;7F		;LD A,A
instr 	fetch_B,	op_ADDA,	store_nop	;80		;ADD A,B
instr 	fetch_C,	op_ADDA,	store_nop	;81		;ADD A,C
instr 	fetch_D,	op_ADDA,	store_nop	;82		;ADD A,D
instr 	fetch_E,	op_ADDA,	store_nop	;83		;ADD A,E
instr 	fetch_H,	op_ADDA,	store_nop	;84		;ADD A,H
instr 	fetch_L,	op_ADDA,	store_nop	;85		;ADD A,L
instr 	fetch_MHL,	op_ADDA,	store_nop	;86		;ADD A,(HL)
instr 	fetch_A,	op_ADDA,	store_nop	;87		;ADD A,A
instr 	fetch_B,	op_ADCA,	store_nop	;88		;ADC A,B
instr 	fetch_C,	op_ADCA,	store_nop	;89		;ADC A,C
instr 	fetch_D,	op_ADCA,	store_nop	;8A		;ADC A,D
instr 	fetch_E,	op_ADCA,	store_nop	;8B		;ADC A,E
instr 	fetch_H,	op_ADCA,	store_nop	;8C		;ADC A,H
instr 	fetch_L,	op_ADCA,	store_nop	;8D		;ADC A,L
instr 	fetch_MHL,	op_ADCA,	store_nop	;8E		;ADC A,(HL)
instr 	fetch_A,	op_ADCA,	store_nop	;8F		;ADC A,A
instr 	fetch_B,	op_SUBFA,	store_nop	;90		;SUB A,B
instr 	fetch_C,	op_SUBFA,	store_nop	;91		;SUB A,C
instr 	fetch_D,	op_SUBFA,	store_nop	;92		;SUB A,D
instr 	fetch_E,	op_SUBFA,	store_nop	;93		;SUB A,E
instr 	fetch_H,	op_SUBFA,	store_nop	;94		;SUB A,H
instr 	fetch_L,	op_SUBFA,	store_nop	;95		;SUB A,L
instr 	fetch_MHL,	op_SUBFA,	store_nop	;96		;SUB A,(HL)
instr 	fetch_A,	op_SUBFA,	store_nop	;97		;SUB A,A
instr 	fetch_B,	op_SBCFA,	store_nop	;98		;SBC A,B
instr 	fetch_C,	op_SBCFA,	store_nop	;99		;SBC A,C
instr 	fetch_D,	op_SBCFA,	store_nop	;9A		;SBC A,D
instr 	fetch_E,	op_SBCFA,	store_nop	;9B		;SBC A,E
instr 	fetch_H,	op_SBCFA,	store_nop	;9C		;SBC A,H
instr 	fetch_L,	op_SBCFA,	store_nop	;9D		;SBC A,L
instr 	fetch_MHL,	op_SBCFA,	store_nop	;9E		;SBC A,(HL)
instr 	fetch_A,	op_SBCFA,	store_nop	;9F		;SBC A,A
instr 	fetch_B,	op_ANDA,	store_nop	;A0		;AND A,B
instr 	fetch_C,	op_ANDA,	store_nop	;A1		;AND A,C
instr 	fetch_D,	op_ANDA,	store_nop	;A2		;AND A,D
instr 	fetch_E,	op_ANDA,	store_nop	;A3		;AND A,E
instr 	fetch_H,	op_ANDA,	store_nop	;A4		;AND A,H
instr 	fetch_L,	op_ANDA,	store_nop	;A5		;AND A,L
instr 	fetch_MHL,	op_ANDA,	store_nop	;A6		;AND A,(HL)
instr 	fetch_A,	op_ANDA,	store_nop	;A7		;AND A,A
instr 	fetch_B,	op_XORA,	store_nop	;A8		;XOR A,B
instr 	fetch_C,	op_XORA,	store_nop	;A9		;XOR A,C
instr 	fetch_D,	op_XORA,	store_nop	;AA		;XOR A,D
instr 	fetch_E,	op_XORA,	store_nop	;AB		;XOR A,E
instr 	fetch_H,	op_XORA,	store_nop	;AC		;XOR A,H
instr 	fetch_L,	op_XORA,	store_nop	;AD		;XOR A,L
instr 	fetch_MHL,	op_XORA,	store_nop	;AE		;XOR A,(HL)
instr 	fetch_A,	op_XORA,	store_nop	;AF		;XOR A,A
instr 	fetch_B,	op_ORA,		store_nop	;B0		;OR A,B
instr 	fetch_C,	op_ORA,		store_nop	;B1		;OR A,C
instr 	fetch_D,	op_ORA,		store_nop	;B2		;OR A,D
instr 	fetch_E,	op_ORA,		store_nop	;B3		;OR A,E
instr 	fetch_H,	op_ORA,		store_nop	;B4		;OR A,H
instr 	fetch_L,	op_ORA,		store_nop	;B5		;OR A,L
instr 	fetch_MHL,	op_ORA,		store_nop	;B6		;OR A,(HL)
instr 	fetch_A,	op_ORA,		store_nop	;B7		;OR A,A
instr 	fetch_B,	op_CPFA,	store_nop	;B8		;CP A,B
instr 	fetch_C,	op_CPFA,	store_nop	;B9		;CP A,C
instr 	fetch_D,	op_CPFA,	store_nop	;BA		;CP A,D
instr 	fetch_E,	op_CPFA,	store_nop	;BB		;CP A,E
instr 	fetch_H,	op_CPFA,	store_nop	;BC		;CP A,H
instr 	fetch_L,	op_CPFA,	store_nop	;BD		;CP A,L
instr 	fetch_MHL,	op_CPFA,	store_nop	;BE		;CP A,(HL)
instr 	fetch_A,	op_CPFA,	store_nop	;BF		;CP A,A
instr 	fetch_nop,	op_IFNZ,	store_RET	;C0		;RET NZ
instr 	fetch_nop,	op_POP16,	store_BC	;C1		;POP BC
instr 	fetch_DIR16,	op_IFNZ,	store_PC	;C2 nn nn	;JP NZ,nn
instr 	fetch_DIR16,	op_nop,		store_PC	;C3 nn nn	;JP nn
instr 	fetch_DIR16,	op_IFNZ,	store_CALL	;C4 nn nn	;CALL NZ,nn
instr 	fetch_BC,	op_PUSH16,	store_nop	;C5		;PUSH BC
instr 	fetch_DIR8,	op_ADDA,	store_nop	;C6 nn		;ADD A,n
instr 	fetch_RST,	op_nop,		store_CALL	;C7		;RST 0
instr 	fetch_nop,	op_IFZ,		store_RET	;C8		;RET Z
instr 	fetch_nop,	op_nop,		store_RET	;C9		;RET
instr 	fetch_DIR16,	op_IFZ,		store_PC	;CA nn nn	;JP Z,nn
instr 	fetch_nop,	op_prefixCB,	store_nop	;CB 		;(CB opcode prefix)
instr 	fetch_DIR16,	op_IFZ,		store_CALL	;CC nn nn	;CALL Z,nn
instr 	fetch_DIR16,	op_nop,		store_CALL	;CD nn nn	;CALL nn
instr 	fetch_DIR8,	op_ADCA,	store_nop	;CE nn		;ADC A,n
instr 	fetch_RST,	op_nop,		store_CALL	;CF		;RST 8H
instr 	fetch_nop,	op_IFNC,	store_RET	;D0		;RET NC
instr 	fetch_nop,	op_POP16,	store_DE	;D1		;POP DE
instr 	fetch_DIR16,	op_IFNC,	store_PC	;D2 nn nn	;JP NC,nn
instr 	fetch_DIR8,	op_OUTA,	store_nop	;D3 nn		;OUT (n),A
instr 	fetch_DIR16,	op_IFNC,	store_CALL	;D4 nn nn	;CALL NC,nn
instr 	fetch_DE,	op_PUSH16,	store_nop	;D5		;PUSH DE
instr 	fetch_DIR8,	op_SUBFA,	store_nop	;D6 nn		;SUB n
instr 	fetch_RST,	op_nop,		store_CALL	;D7		;RST 10H
instr 	fetch_nop,	op_IFC,		store_RET	;D8		;RET C
instr 	fetch_nop,	op_EXX,		store_nop	;D9		;EXX
instr 	fetch_DIR16,	op_IFC,		store_PC	;DA nn nn	;JP C,nn
instr 	fetch_DIR8,	op_INA,		store_nop	;DB nn		;IN A,(n)
instr 	fetch_DIR16,	op_IFC,		store_CALL	;DC nn nn	;CALL C,nn
instr 	fetch_nop,	op_prefixDD,	store_nop	;DD 		;(DD opcode prefix)
instr 	fetch_DIR8,	op_SBCFA,	store_nop	;DE nn		;SBC A,n
instr 	fetch_RST,	op_nop,		store_CALL	;DF		;RST 18H
instr 	fetch_nop,	op_IFPO,	store_RET	;E0		;RET PO
instr 	fetch_nop,	op_POP16,	store_HL	;E1		;POP HL
instr 	fetch_DIR16,	op_IFPO,	store_PC	;E2 nn nn	;JP PO,nn
instr 	fetch_MSP,	op_EXHL,	store_MSP	;E3		;EX (SP),HL
instr 	fetch_DIR16,	op_IFPO,	store_CALL	;E4 nn nn	;CALL PO,nn
instr 	fetch_HL,	op_PUSH16,	store_nop	;E5		;PUSH HL
instr 	fetch_DIR8,	op_ANDA,	store_nop	;E6 nn		;AND n
instr 	fetch_RST,	op_nop,		store_CALL	;E7		;RST 20H
instr 	fetch_nop,	op_IFPE,	store_RET	;E8		;RET PE
instr 	fetch_HL,	op_nop,		store_PC	;E9		;JP HL
instr 	fetch_DIR16,	op_IFPE,	store_PC	;EA nn nn	;JP PE,nn
instr 	fetch_DE,	op_EXHL,	store_DE	;EB		;EX DE,HL
instr 	fetch_DIR16,	op_IFPE,	store_CALL	;EC nn nn	;CALL PE,nn
instr 	fetch_nop,	op_prefixED,	store_nop	;ED		;(ED opcode prefix)
instr 	fetch_DIR8,	op_XORA,	store_nop	;EE nn		;XOR n
instr 	fetch_RST,	op_nop,		store_CALL	;EF		;RST 28H
instr 	fetch_nop,	op_IFP,		store_RET	;F0		;RET P
instr 	fetch_nop,	op_POP16,	store_AF	;F1		;POP AF
instr 	fetch_DIR16,	op_IFP,		store_PC	;F2 nn nn	;JP P,nn
instr 	fetch_nop,	op_DI,		store_nop	;F3		;DI
instr 	fetch_DIR16,	op_IFP,		store_CALL	;F4 nn nn	;CALL P,nn
instr 	fetch_AF,	op_PUSH16,	store_nop	;F5		;PUSH AF
instr 	fetch_DIR8,	op_ORA,		store_nop	;F6 nn		;OR n
instr 	fetch_RST,	op_nop,		store_CALL	;F7		;RST 30H
instr 	fetch_nop,	op_IFM,		store_RET	;F8		;RET M
instr 	fetch_HL,	op_nop,		store_SP	;F9		;LD SP,HL
instr 	fetch_DIR16,	op_IFM,		store_PC	;FA nn nn	;JP M,nn
instr 	fetch_nop,	op_EI,		store_nop	;FB		;EI
instr 	fetch_DIR16,	op_IFM,		store_CALL	;FC nn nn	;CALL M,nn
instr 	fetch_nop,	op_prefixFD,	store_nop	;FD 		;(FD opcode prefix)
instr 	fetch_DIR8,	op_CPFA,	store_nop	;FE nn		;CP n
instr 	fetch_RST,	op_nop,		store_CALL	;FF		;RST 38H


#if EM_Z80



	checkspace PC, 2

do_op_noni:
	sbiw	z_pcl,1				;--z_pc
	ret

	checkspace PC, 16

do_fetch_dir8_2:
	movw	xl,z_pcl
	adiw	xl,1
	mem_read_d opl
	ret

	checkspace PC, 5

do_fetch_xh:
	sbis	flags,prefixfd
	ldd	opl,y+oz_xh
	sbic	flags,prefixfd
	ldd	opl,y+oz_yh
	ret

	checkspace PC, 5

do_fetch_xl:
	sbis	flags,prefixfd
	ldd	opl,y+oz_xl
	sbic	flags,prefixfd
	ldd	opl,y+oz_yl
	ret


	checkspace PC, 41

do_fetch_mxx:
	sbic	flags,prefixfd
	rjmp	fetchmxx_fd
	ldd	xh,y+oz_xh
	ldd	xl,y+oz_xl
	rjmp	fetchmxx1
fetchmxx_fd:
	ldd	xh,y+oz_yh
	ldd	xl,y+oz_yl
fetchmxx1:
	mem_read_ds opl, z_pc			;get displacement
	adiw z_pcl,1
	clr	oph				;sign extend
	tst	opl
	brpl	fetchmxx2
	com	oph
fetchmxx2:
	add	xl,opl				;add displacement
	adc	xh,oph
	mem_read_d opl				;get operand
	ret					;(Ix+d) still in xl,xh


	checkspace PC, 8

do_fetch_xx:
	sbic	flags,prefixfd
	rjmp	fetchxx_fd
	ldd	opl,y+oz_xl
	ldd	oph,y+oz_xh
	ret
fetchxx_fd:
	ldd	opl,y+oz_yl
	ldd	oph,y+oz_yh
	ret

	checkspace PC, 5

do_store_xh:
	sbis	flags,prefixfd
	std	y+oz_xh,opl
	sbic	flags,prefixfd
	std	y+oz_yh,opl
	ret

	checkspace PC, 5

do_store_xl:
	sbis	flags,prefixfd
	std	y+oz_xl,opl
	sbic	flags,prefixfd
	std	y+oz_yl,opl
	ret

	checkspace PC, 37

do_store_mxx:
	sbic	flags,prefixfd
	rjmp	storemxx_fd
	ldd	xh,y+oz_xh
	ldd	xl,y+oz_xl
	rjmp	storemxx1
storemxx_fd:
	ldd	xh,y+oz_yh
	ldd	xl,y+oz_yl
storemxx1:
	mem_read_s z_pc				;get displacement
	adiw z_pcl,1
	clr	temp2				;sign extend
	tst	temp
	brpl	storemxx2
	com	temp2
storemxx2:
	add	xl,temp				;add displacement
	adc	xh,temp2
	mem_write_s opl				;store operand
	ret

	checkspace PC, 10

do_store_mxx_0:
	mem_write_s opl				;store operand
	ret

	checkspace PC, 38

do_store_mxx_2:
	sbic	flags,prefixfd
	rjmp	storemxx2_fd
	ldd	xh,y+oz_xh
	ldd	xl,y+oz_xl
	rjmp	storemxx21
storemxx2_fd:
	ldd	xh,y+oz_yh
	ldd	xl,y+oz_yl
storemxx21:
	mem_read_s z_pc				;get displacement
	adiw	z_pcl,1
	adiw	z_pcl,1
	clr	temp2				;sign extend
	tst	temp
	brpl	storemxx22
	com	temp2
storemxx22:
	add	xl,temp				;add displacement
	adc	xh,temp2
	mem_write_s opl				;store operand
	ret

	checkspace PC, 8

do_store_xx:
	sbic	flags,prefixfd
	rjmp	storexx_fd
	std	y+oz_xl,opl
	std	y+oz_xh,oph
	ret
storexx_fd:
	std	y+oz_yl,opl
	std	y+oz_yh,oph
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|LD dst,src|------|Load                 |dst=src               |
;

	checkspace PC, 30

do_op_stxx: 		;store xx to mem loc in opl:h

	movw	xl,opl
	sbis	flags,prefixfd
	ldd	temp,y+oz_xl
	sbic	flags,prefixfd
	ldd	temp,y+oz_yl
	mem_write
	adiw xl,1
	sbis	flags,prefixfd
	ldd	temp,y+oz_xh
	sbic	flags,prefixfd
	ldd	temp,y+oz_yh
	mem_write
	ret


;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|EX [SP],IX|------|Exchange             |[SP]<->IX             |
;|EX [SP],IY|------|Exchange             |[SP]<->IY             |
; 
	checkspace PC, 13

do_op_EXxx:
	sbic	flags,prefixfd
	rjmp	opexxx_fd
	ldd	temp,y+oz_xl
	ldd	temp2,y+oz_xh
	std	y+oz_xl,opl
	std	y+oz_xh,oph
	rjmp	opexxxe
opexxx_fd:
	ldd	temp,y+oz_yl
	ldd	temp2,y+oz_yh
	std	y+oz_yl,opl
	std	y+oz_yh,oph
opexxxe:
	movw	opl,temp
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|ADD IX,pp |--*-0*|Add                  |IX=IX+pp              |
;|ADD IY,rr |--*-0*|Add                  |IY=IY+rr              |
;

	checkspace PC, 25

do_op_addxx:
	sbic	flags,prefixfd
	rjmp	opadx_fd
	ldd	temp,y+oz_xl
	ldd	temp2,y+oz_xh
	add	opl,temp
	adc	oph,temp2
	std	y+oz_xl,opl
	std	y+oz_xh,oph
	rjmp	opadx_e
opadx_fd:
	ldd	temp,y+oz_yl
	ldd	temp2,y+oz_yh
	add	opl,temp
	adc	oph,temp2
	std	y+oz_yl,opl
	std	y+oz_yh,oph
opadx_e:
	in	temp,sreg
	bmov	z_flags,ZFL_C, temp,AVR_C
	do_z80_flags_H
	do_z80_flags_clear_N
	ret


	opctable DDFDjmp, PC	;+256

instr 	fetch_nop,	op_noni,		store_nop	;00		;
instr 	fetch_nop,	op_noni,	store_nop	;01		;
instr 	fetch_nop,	op_noni,	store_nop	;02		;
instr 	fetch_nop,	op_noni,	store_nop	;03		;
instr 	fetch_nop,	op_noni,	store_nop	;04		;
instr 	fetch_nop,	op_noni,	store_nop	;05		;
instr 	fetch_nop,	op_noni,	store_nop	;06		;
instr 	fetch_nop,	op_noni,	store_nop	;07		;
instr 	fetch_nop,	op_noni,	store_nop	;08		;
instr 	fetch_BC,	op_ADDxx,	store_nop	;09		;ADD xx,BC
instr 	fetch_nop,	op_noni,	store_nop	;0A		;
instr 	fetch_nop,	op_noni,	store_nop	;0B		;
instr 	fetch_nop,	op_noni,	store_nop	;0C		;
instr 	fetch_nop,	op_noni,	store_nop	;0D		;
instr 	fetch_nop,	op_noni,	store_nop	;0E		;
instr 	fetch_nop,	op_noni,	store_nop	;0F		;
instr 	fetch_nop,	op_noni,	store_nop	;10		;
instr 	fetch_nop,	op_noni,	store_nop	;11		;
instr 	fetch_nop,	op_noni,	store_nop	;12		;
instr 	fetch_nop,	op_noni,	store_nop	;13		;
instr 	fetch_nop,	op_noni,	store_nop	;14		;
instr 	fetch_nop,	op_noni,	store_nop	;15		;
instr 	fetch_nop,	op_noni,	store_nop	;16		;
instr 	fetch_nop,	op_noni,	store_nop	;17		;
instr 	fetch_nop,	op_noni,	store_nop	;18		;
instr 	fetch_DE,	op_ADDxx,	store_nop	;19		;ADD xx,DE
instr 	fetch_nop,	op_noni,	store_nop	;1A		;
instr 	fetch_nop,	op_noni,	store_nop	;1B		;
instr 	fetch_nop,	op_noni,	store_nop	;1C		;
instr 	fetch_nop,	op_noni,	store_nop	;1D		;
instr 	fetch_nop,	op_noni,	store_nop	;1E		;
instr 	fetch_nop,	op_noni,	store_nop	;1F		;
instr 	fetch_nop,	op_noni,	store_nop	;20		;
instr 	fetch_DIR16,	op_nop,		store_xx	;21		;LD xx,nn
instr 	fetch_DIR16,	op_STxx,	store_nop	;22		;LD (nn),xx
instr 	fetch_xx,	op_INC16,	store_xx	;23		;INC xx
instr 	fetch_xH,	op_INC,		store_xH	;24		;INC xh
instr 	fetch_xH,	op_DEC,		store_xH	;25		;DEC xh
instr 	fetch_DIR8,	op_nop,		store_xH	;26		;LD xh,n
instr 	fetch_nop,	op_noni,	store_nop	;27		;
instr 	fetch_nop,	op_noni,	store_nop	;28		;
instr 	fetch_xx,	op_ADDxx,	store_nop	;29		;ADD xx,xx
instr 	fetch_DIR16,	op_RMEM16,	store_xx	;2A		;LD xx,(nn)
instr 	fetch_xx,	op_DEC16,	store_xx	;2B		;DEC xx
instr 	fetch_xL,	op_INC,		store_xL	;2C		;INC xl
instr 	fetch_xL,	op_DEC,		store_xL	;2D		;DEC xl
instr 	fetch_DIR8,	op_nop,		store_xL	;2E		;LD xl,n
instr 	fetch_nop,	op_noni,	store_nop	;2F		;
instr 	fetch_nop,	op_noni,	store_nop	;30		;
instr 	fetch_nop,	op_noni,	store_nop	;31		;
instr 	fetch_nop,	op_noni,	store_nop	;32		;
instr 	fetch_nop,	op_noni,	store_nop	;33		;
instr 	fetch_MXX,	op_INC,		store_MXX_0	;34		;INC (xx+d)
instr 	fetch_MXX,	op_DEC,		store_MXX_0	;35		;DEC (xx+d)
instr 	fetch_DIR8_2,	op_nop,		store_MXX_2	;36		;LD (xx+d),n
instr 	fetch_nop,	op_noni,	store_nop	;37		;
instr 	fetch_nop,	op_noni,	store_nop	;38		;
instr 	fetch_SP,	op_ADDxx,	store_nop	;39		;ADD xx,SP
instr 	fetch_nop,	op_noni,	store_nop	;3A		;
instr 	fetch_nop,	op_noni,	store_nop	;3B		;
instr 	fetch_nop,	op_noni,	store_nop	;3C		;
instr 	fetch_nop,	op_noni,	store_nop	;3D		;
instr 	fetch_nop,	op_noni,	store_nop	;3E		;
instr 	fetch_nop,	op_noni,	store_nop	;3F		;
instr 	fetch_nop,	op_noni,	store_nop	;40		;
instr 	fetch_nop,	op_noni,	store_nop	;41		;
instr 	fetch_nop,	op_noni,	store_nop	;42		;
instr 	fetch_nop,	op_noni,	store_nop	;43		;
instr 	fetch_xH,	op_nop,		store_B		;44		;LD B,xh
instr 	fetch_xL,	op_nop,		store_B		;45		;LD B,xl
instr 	fetch_MXX,	op_nop,		store_B		;46		;LD B,(xx+d)
instr 	fetch_nop,	op_noni,	store_nop	;47		;
instr 	fetch_nop,	op_noni,	store_nop	;48		;
instr 	fetch_nop,	op_noni,	store_nop	;49		;
instr 	fetch_nop,	op_noni,	store_nop	;4A		;
instr 	fetch_nop,	op_noni,	store_nop	;4B		;
instr 	fetch_xH,	op_nop,		store_C		;4C		;LD C,xh
instr 	fetch_xL,	op_nop,		store_C		;4D		;LD C,xl
instr 	fetch_MXX,	op_nop,		store_C		;4E		;LD C,(xx+d)
instr 	fetch_nop,	op_noni,	store_nop	;4F		;
instr 	fetch_nop,	op_noni,	store_nop	;50		;
instr 	fetch_nop,	op_noni,	store_nop	;51		;
instr 	fetch_nop,	op_noni,	store_nop	;52		;
instr 	fetch_nop,	op_noni,	store_nop	;53		;
instr 	fetch_xH,	op_nop,		store_D		;54		;LD D,xh
instr 	fetch_xL,	op_nop,		store_D		;55		;LD D,xl
instr 	fetch_MXX,	op_nop,		store_D		;56		;LD D,(xx+d)
instr 	fetch_nop,	op_noni,	store_nop	;57		;
instr 	fetch_nop,	op_noni,	store_nop	;58		;
instr 	fetch_nop,	op_noni,	store_nop	;59		;
instr 	fetch_nop,	op_noni,	store_nop	;5A		;
instr 	fetch_nop,	op_noni,	store_nop	;5B		;
instr 	fetch_xH,	op_nop,		store_E		;5C		;LD E,xh
instr 	fetch_xL,	op_nop,		store_E		;5D		;LD E,xl
instr 	fetch_MXX,	op_nop,		store_E		;5E		;LD E,(xx+d)
instr 	fetch_nop,	op_noni,	store_nop	;5F		;
instr 	fetch_B,	op_nop,		store_xH	;60		;LD xh,B
instr 	fetch_C,	op_nop,		store_xH	;61		;LD xh,C
instr 	fetch_D,	op_nop,		store_xH	;62		;LD xh,D
instr 	fetch_E,	op_nop,		store_xH	;63		;LD xh,E
instr 	fetch_nop,	op_noni,	store_nop	;64		;LD xh,xh
instr 	fetch_xL,	op_nop,		store_xH	;65		;LD xh,xl
instr 	fetch_MXX,	op_nop,		store_H		;66		;LD H,(xx+d)
instr 	fetch_A,	op_nop,		store_xH	;67		;LD xh,A
instr 	fetch_B,	op_nop,		store_xL	;68		;LD xl,B
instr 	fetch_C,	op_nop,		store_xL	;69		;LD xl,C
instr 	fetch_D,	op_nop,		store_xL	;6A		;LD xl,D
instr 	fetch_E,	op_nop,		store_xL	;6B		;LD xl,E
instr 	fetch_xH,	op_nop,		store_xL	;6C		;LD xl,xh
instr 	fetch_nop,	op_noni,	store_nop	;6D		;LD xl,xl
instr 	fetch_MXX,	op_nop,		store_L		;6E		;LD L,(xx+d)
instr 	fetch_A,	op_nop,		store_xL	;6F		;LD xl,A
instr 	fetch_B,	op_nop,		store_MXX	;70		;LD (xx+d),B
instr 	fetch_C,	op_nop,		store_MXX	;71		;LD (xx+d),C
instr 	fetch_D,	op_nop,		store_MXX	;72		;LD (xx+d),D
instr 	fetch_E,	op_nop,		store_MXX	;73		;LD (xx+d),E
instr 	fetch_H,	op_nop,		store_MXX	;74		;LD (xx+d),H
instr 	fetch_L,	op_nop,		store_MXX	;75		;LD (xx+d),L
instr 	fetch_nop,	op_noni,	store_nop	;76		;
instr 	fetch_A,	op_nop,		store_MXX	;77		;LD (xx+d),A
instr 	fetch_nop,	op_noni,	store_nop	;78		;
instr 	fetch_nop,	op_noni,	store_nop	;79		;
instr 	fetch_nop,	op_noni,	store_nop	;7A		;
instr 	fetch_nop,	op_noni,	store_nop	;7B		;
instr 	fetch_xH,	op_nop,		store_A		;7C		;LD A,xh
instr 	fetch_xL,	op_nop,		store_A		;7D		;LD A,xl
instr 	fetch_MXX,	op_nop,		store_A		;7E		;LD A,(xx+d)
instr 	fetch_nop,	op_noni,	store_nop	;7F		;
instr 	fetch_nop,	op_noni,	store_nop	;80		;
instr 	fetch_nop,	op_noni,	store_nop	;81		;
instr 	fetch_nop,	op_noni,	store_nop	;82		;
instr 	fetch_nop,	op_noni,	store_nop	;83		;
instr 	fetch_xH,	op_ADDA,	store_nop	;84		;ADD A,xh
instr 	fetch_xL,	op_ADDA,	store_nop	;85		;ADD A,xl
instr 	fetch_MXX,	op_ADDA,	store_nop	;86		;ADD A,(xx)
instr 	fetch_nop,	op_noni,	store_nop	;87		;
instr 	fetch_nop,	op_noni,	store_nop	;88		;
instr 	fetch_nop,	op_noni,	store_nop	;89		;
instr 	fetch_nop,	op_noni,	store_nop	;8A		;
instr 	fetch_nop,	op_noni,	store_nop	;8B		;
instr 	fetch_xH,	op_ADCA,	store_nop	;8C		;ADC A,xh
instr 	fetch_xL,	op_ADCA,	store_nop	;8D		;ADC A,xl
instr 	fetch_MXX,	op_ADCA,	store_nop	;8E		;ADC A,(xx)
instr 	fetch_nop,	op_noni,	store_nop	;8F		;
instr 	fetch_nop,	op_noni,	store_nop	;90		;
instr 	fetch_nop,	op_noni,	store_nop	;91		;
instr 	fetch_nop,	op_noni,	store_nop	;92		;
instr 	fetch_nop,	op_noni,	store_nop	;93		;
instr 	fetch_xH,	op_SUBFA,	store_nop	;94		;SUB A,xh
instr 	fetch_xL,	op_SUBFA,	store_nop	;95		;SUB A,xl
instr 	fetch_MXX,	op_SUBFA,	store_nop	;96		;SUB A,(xx)
instr 	fetch_nop,	op_noni,	store_nop	;97		;
instr 	fetch_nop,	op_noni,	store_nop	;98		;
instr 	fetch_nop,	op_noni,	store_nop	;99		;
instr 	fetch_nop,	op_noni,	store_nop	;9A		;
instr 	fetch_nop,	op_noni,	store_nop	;9B		;
instr 	fetch_xH,	op_SBCFA,	store_nop	;9C		;SBC A,xh
instr 	fetch_xL,	op_SBCFA,	store_nop	;9D		;SBC A,xl
instr 	fetch_MXX,	op_SBCFA,	store_nop	;9E		;SBC A,(xx)
instr 	fetch_nop,	op_noni,	store_nop	;9F		;
instr 	fetch_nop,	op_noni,	store_nop	;A0		;
instr 	fetch_nop,	op_noni,	store_nop	;A1		;
instr 	fetch_nop,	op_noni,	store_nop	;A2		;
instr 	fetch_nop,	op_noni,	store_nop	;A3		;
instr 	fetch_xH,	op_ANDA,	store_nop	;A4		;AND A,xh
instr 	fetch_xL,	op_ANDA,	store_nop	;A5		;AND A,xl
instr 	fetch_MXX,	op_ANDA,	store_nop	;A6		;AND A,(xx)
instr 	fetch_nop,	op_noni,	store_nop	;A7		;
instr 	fetch_nop,	op_noni,	store_nop	;A8		;
instr 	fetch_nop,	op_noni,	store_nop	;A9		;
instr 	fetch_nop,	op_noni,	store_nop	;AA		;
instr 	fetch_nop,	op_noni,	store_nop	;AB		;
instr 	fetch_xH,	op_XORA,	store_nop	;AC		;XOR A,xh
instr 	fetch_xL,	op_XORA,	store_nop	;AD		;XOR A,xl
instr 	fetch_MXX,	op_XORA,	store_nop	;AE		;XOR A,(xx)
instr 	fetch_nop,	op_noni,	store_nop	;AF		;
instr 	fetch_nop,	op_noni,	store_nop	;B0		;
instr 	fetch_nop,	op_noni,	store_nop	;B1		;
instr 	fetch_nop,	op_noni,	store_nop	;B2		;
instr 	fetch_nop,	op_noni,	store_nop	;B3		;
instr 	fetch_xH,	op_ORA,		store_nop	;B4		;OR A,xh
instr 	fetch_xL,	op_ORA,		store_nop	;B5		;OR A,xl
instr 	fetch_MXX,	op_ORA,		store_nop	;B6		;OR A,(xx)
instr 	fetch_nop,	op_noni,	store_nop	;B7		;
instr 	fetch_nop,	op_noni,	store_nop	;B8		;
instr 	fetch_nop,	op_noni,	store_nop	;B9		;
instr 	fetch_nop,	op_noni,	store_nop	;BA		;
instr 	fetch_nop,	op_noni,	store_nop	;BB		;
instr 	fetch_xH,	op_CPFA,	store_nop	;BC		;CP A,xh
instr 	fetch_xL,	op_CPFA,	store_nop	;BD		;CP A,xl
instr 	fetch_MXX,	op_CPFA,	store_nop	;BE		;CP A,(xx)
instr 	fetch_nop,	op_noni,	store_nop	;BF		;
instr 	fetch_nop,	op_noni,	store_nop	;C0		;
instr 	fetch_nop,	op_noni,	store_nop	;C1		;
instr 	fetch_nop,	op_noni,	store_nop	;C2		;
instr 	fetch_nop,	op_noni,	store_nop	;C3		;
instr 	fetch_nop,	op_noni,	store_nop	;C4		;
instr 	fetch_nop,	op_noni,	store_nop	;C5		;
instr 	fetch_nop,	op_noni,	store_nop	;C6		;
instr 	fetch_nop,	op_noni,	store_nop	;C7		;
instr 	fetch_nop,	op_noni,	store_nop	;C8		;
instr 	fetch_nop,	op_noni,	store_nop	;C9		;
instr 	fetch_nop,	op_noni,	store_nop	;CA		;
instr 	fetch_nop,	op_prefixDDFDCB,store_nop	;CB		;
instr 	fetch_nop,	op_noni,	store_nop	;CC		;
instr 	fetch_nop,	op_noni,	store_nop	;CD		;
instr 	fetch_nop,	op_noni,	store_nop	;CE		;
instr 	fetch_nop,	op_noni,	store_nop	;CF		;
instr 	fetch_nop,	op_noni,	store_nop	;D0		;
instr 	fetch_nop,	op_noni,	store_nop	;D1		;
instr 	fetch_nop,	op_noni,	store_nop	;D2		;
instr 	fetch_nop,	op_noni,	store_nop	;D3		;
instr 	fetch_nop,	op_noni,	store_nop	;D4		;
instr 	fetch_nop,	op_noni,	store_nop	;D5		;
instr 	fetch_nop,	op_noni,	store_nop	;D6		;
instr 	fetch_nop,	op_noni,	store_nop	;D7		;
instr 	fetch_nop,	op_noni,	store_nop	;D8		;
instr 	fetch_nop,	op_noni,	store_nop	;D9		;
instr 	fetch_nop,	op_noni,	store_nop	;DA		;
instr 	fetch_nop,	op_noni,	store_nop	;DB		;
instr 	fetch_nop,	op_noni,	store_nop	;DC		;
instr 	fetch_nop,	op_noni,	store_nop	;DD		;
instr 	fetch_nop,	op_noni,	store_nop	;DE		;
instr 	fetch_nop,	op_noni,	store_nop	;DF		;
instr 	fetch_nop,	op_noni,	store_nop	;E0		;
instr 	fetch_nop,	op_POP16,	store_xx	;E1		;POP xx
instr 	fetch_nop,	op_noni,	store_nop	;E2		;
instr 	fetch_MSP,	op_EXxx,	store_MSP	;E3		;EX (SP),xx
instr 	fetch_nop,	op_noni,	store_nop	;E4		;
instr 	fetch_xx,	op_PUSH16,	store_nop	;E5		;PUSH xx
instr 	fetch_nop,	op_noni,	store_nop	;E6		;
instr 	fetch_nop,	op_noni,	store_nop	;E7		;
instr 	fetch_nop,	op_noni,	store_nop	;E8		;
instr 	fetch_xx,	op_nop,		store_PC	;E9		;JP xx
instr 	fetch_nop,	op_noni,	store_nop	;EA		;
instr 	fetch_nop,	op_noni,	store_nop	;EB		;
instr 	fetch_nop,	op_noni,	store_nop	;EC		;
instr 	fetch_nop,	op_noni,	store_nop	;ED		;
instr 	fetch_nop,	op_noni,	store_nop	;EE		;
instr 	fetch_nop,	op_noni,	store_nop	;EF		;
instr 	fetch_nop,	op_noni,	store_nop	;F0		;
instr 	fetch_nop,	op_noni,	store_nop	;F1		;
instr 	fetch_nop,	op_noni,	store_nop	;F2		;
instr 	fetch_nop,	op_noni,	store_nop	;F3		;
instr 	fetch_nop,	op_noni,	store_nop	;F4		;
instr 	fetch_nop,	op_noni,	store_nop	;F5		;
instr 	fetch_nop,	op_noni,	store_nop	;F6		;
instr 	fetch_nop,	op_noni,	store_nop	;F7		;
instr 	fetch_nop,	op_noni,	store_nop	;F8		;
instr 	fetch_xx,	op_nop,		store_SP	;F9		;LD SP,xx
instr 	fetch_nop,	op_noni,	store_nop	;FA		;
instr 	fetch_nop,	op_noni,	store_nop	;FB		;
instr 	fetch_nop,	op_noni,	store_nop	;FC		;
instr 	fetch_nop,	op_noni,	store_nop	;FD		;
instr 	fetch_nop,	op_noni,	store_nop	;FE		;
instr 	fetch_nop,	op_noni,	store_nop	;FF		;



;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|RLC m     |**0P0*|Rotate Left Circular |m=m<-                 |
;|RRC m     |**0P0*|Rotate Right Circular|m=->m                 |
;|RL m      |**0P0*|Rotate Left          |m={CY,m}<-            |
;|RR m      |**0P0*|Rotate Right         |m=->{CY,m}            |
;|SLA m     |**0P0*|Shift Left Arithmetic|m=m*2                 |
;|SRA m     |**0P0*|Shift Right Arith.   |m=m/2                 |
;|SLL m     |**0P0*|Shift Right Logical  |
;|SRL m     |**0P0*|Shift Right Logical  |m=->{0,m,CY}          |


	checkspace PC, 9

do_op_rlc:
	;Rotate Left Cyclical. All bits move 1 to the 
	;left, the msb becomes c and lsb.
	clr	temp
	lsl	opl
	adc	temp,_0
	or	opl,temp
	ldpmx	z_flags,sz53p_tab,opl		;S,Z,H,P,N	
	or	z_flags,temp
	ret

	checkspace PC, 9

do_op_rrc: 
	;Rotate Right Cyclical. All bits move 1 to the 
	;right, the lsb becomes c and msb.
	lsr	opl
	brcc	PC+2
	ori	opl,0x80
	ldpmx	z_flags,sz53p_tab,opl		;S,Z,H,P,N	
	bmov	z_flags,ZFL_C, opl,7
	ret


	checkspace PC, 11

do_op_rl:
	;Rotate Left. All bits move 1 to the left, the msb 
	;becomes c, c becomes lsb.
	clc
	sbrc	z_flags,ZFL_C
	 sec
	rol	opl
	in	temp,sreg
	ldpmx	z_flags,sz53p_tab,opl		;S,Z,H,P,N	
	bmov	z_flags,ZFL_C, temp,AVR_C
	ret


	checkspace PC, 10

do_op_rr:
	;Rotate Right. All bits move 1 to the right, the lsb 
	;becomes c, c becomes msb.

	ror     opl
	in	temp,sreg		;CY
	bmov	opl,7, z_flags,ZFL_C		;old CY --> Bit 7
	ldpmx	z_flags,sz53p_tab,opl		;S,Z,H,P,N	
	bmov	z_flags,ZFL_C, temp,AVR_C	;
	ret

	checkspace PC, 9

do_op_sla:
	lsl	opl
	in	temp,sreg
	ldpmx	z_flags,sz53p_tab,opl		;S,Z,H,P,N	
	bmov	z_flags,ZFL_C, temp,AVR_C	;
	ret

	checkspace PC, 11

do_op_sra:
	lsr	opl
	in	temp,sreg
	bmov	opl,7, opl,6			;old CY --> Bit 7
	ldpmx	z_flags,sz53p_tab,opl		;S,Z,H,P,N	
	bmov	z_flags,ZFL_C, temp,AVR_C	;
	ret

	checkspace PC, 9

do_op_sll:
	sec
	rol	opl
	in	temp,sreg
	ldpmx	z_flags,sz53p_tab,opl		;S,Z,H,P,N	
	bmov	z_flags,ZFL_C, temp,AVR_C	;
	ret

	checkspace PC, 8

do_op_srl:
	lsr	opl
	in	temp,sreg
	ldpmx	z_flags,sz53p_tab,opl		;S,Z,H,P,N	
	bmov	z_flags,ZFL_C, temp,AVR_C	;
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|BIT b,m   |?*1?0-|Test Bit             |m&{2^b}               |
;|RES b,m   |------|Reset bit            |m=m&{~2^b}            |
;|SET b,m   |------|Set bit              |m=mv{2^b}             |


	checkspace PC, 2
do_op_BIT7:
	ldi	temp,0x80
	rjmp	opbit
	checkspace PC, 2
do_op_BIT6:
	ldi	temp,0x40
	rjmp	opbit
	checkspace PC, 2
do_op_BIT5:
	ldi	temp,0x20
	rjmp	opbit
	checkspace PC, 2
do_op_BIT4:
	ldi	temp,0x10
	rjmp	opbit
	checkspace PC, 2
do_op_BIT3:
	ldi	temp,0x08
	rjmp	opbit
	checkspace PC, 2
do_op_BIT2:
	ldi	temp,0x04
	rjmp	opbit
	checkspace PC, 2
do_op_BIT1:
	ldi	temp,0x02
	rjmp	opbit

	checkspace PC, 7
do_op_BIT0:
	ldi	temp,0x01
opbit:
	andi	z_flags,~((1<<ZFL_N)|(1<<ZFL_Z))
	ori	z_flags,(1<<ZFL_H)
	and	temp,opl
	brne	opbite
	ori	z_flags,(1<<ZFL_Z)
opbite:
	ret


.macro m_do_op_RES7
	andi	opl,~0x80
.endm
.equ	do_op_RES7 = 0
;	andi	opl,~0x80
;	ret

.macro m_do_op_RES6
	andi	opl,~0x40
.endm
.equ	do_op_RES6 = 0
;	andi	opl,~0x40
;	ret

.macro m_do_op_RES5
	andi	opl,~0x20
.endm
.equ	do_op_RES5 = 0
;	andi	opl,~0x20
;	ret

.macro m_do_op_RES4
	andi	opl,~0x10
.endm
.equ	do_op_RES4 = 0
;	andi	opl,~0x10
;	ret

.macro m_do_op_RES3
	andi	opl,~0x08
.endm
.equ	do_op_RES3 = 0
;	andi	opl,~0x08
;	ret

.macro m_do_op_RES2
	andi	opl,~0x04
.endm
.equ	do_op_RES2 = 0
;	andi	opl,~0x04
;	ret

.macro m_do_op_RES1
	andi	opl,~0x02
.endm
.equ	do_op_RES1 = 0
;	andi	opl,~0x02
;	ret

.macro m_do_op_RES0
	andi	opl,~0x01
.endm
.equ	do_op_RES0 = 0
;	andi	opl,~0x01
;	ret

.macro m_do_op_SET7
	ori	opl,0x80
.endm
.equ	do_op_SET7 = 0
;	ori	opl,0x80
;	ret

.macro m_do_op_SET6
	ori	opl,0x40
.endm
.equ	do_op_SET6 = 0
;	ori	opl,0x40
;	ret

.macro m_do_op_SET5
	ori	opl,0x20
.endm
.equ	do_op_SET5 = 0
;	ori	opl,0x20
;	ret

.macro m_do_op_SET4
	ori	opl,0x10
.endm
.equ	do_op_SET4 = 0
;	ori	opl,0x10
;	ret

.macro m_do_op_SET3
	ori	opl,0x08
.endm
.equ	do_op_SET3 = 0
;	ori	opl,0x08
;	ret

.macro m_do_op_SET2
	ori	opl,0x04
.endm
.equ	do_op_SET2 = 0
;	ori	opl,0x04
;	ret

.macro m_do_op_SET1
	ori	opl,0x02
.endm
.equ	do_op_SET1 = 0
;	ori	opl,0x02
;	ret

.macro m_do_op_SET0
	ori	opl,0x01
.endm
.equ	do_op_SET0 = 0
;	ori	opl,0x01
;	ret


;.macro m_do_store_b 
;	std	y+oz_b,opl
;.endm
;.equ do_store_b = 0
	checkspace PC, 2
do_store2_b:
	mov	z_b,opl
	ret

	checkspace PC, 2
do_store2_c:
	mov	z_c,opl
	ret

	checkspace PC, 2
do_store2_d:
	mov	z_d,opl
	ret

	checkspace PC, 2
do_store2_e:
	mov	z_e,opl
	ret

	checkspace PC, 2
do_store2_h:
	mov	z_h,opl
	ret

	checkspace PC, 2
do_store2_l:
	mov	z_l,opl
	ret

	checkspace PC, 2
do_store2_a:
	mov z_a,opl
	ret

	checkspace PC, 4
do_fetch2_mhl:
;	movw	x,z_l
	mem_read_ds opl, z_hl
	ret

	opctable CBjmp, PC	;+256

instr 	fetch_B,	op_RLC,		store2_B	;00		;RLC B
instr 	fetch_C,	op_RLC,		store2_C	;01		;RLC C
instr 	fetch_D,	op_RLC,		store2_D	;02		;RLC D
instr 	fetch_E,	op_RLC,		store2_E	;03		;RLC E
instr 	fetch_H,	op_RLC,		store2_H	;04		;RLC H
instr 	fetch_L,	op_RLC,		store2_L	;05		;RLC L
instr 	fetch2_mhl,	op_RLC,		store_MHL	;06		;RLC (HL)
instr 	fetch_A,	op_RLC,		store2_A	;07		;RLC A
instr 	fetch_B,	op_RRC,		store2_B	;08		;RRC B
instr 	fetch_C,	op_RRC,		store2_C	;09		;RRC C
instr 	fetch_D,	op_RRC,		store2_D	;0A		;RRC D
instr 	fetch_E,	op_RRC,		store2_E	;0B		;RRC E
instr 	fetch_H,	op_RRC,		store2_H	;0C		;RRC H
instr 	fetch_L,	op_RRC,		store2_L	;0D		;RRC L
instr 	fetch2_mhl,	op_RRC,		store_MHL	;0E		;RRC (HL)
instr 	fetch_A,	op_RRC,		store2_A	;0F		;RRC A
instr 	fetch_B,	op_RL,		store2_B	;10		;RL B
instr 	fetch_C,	op_RL,		store2_C	;11		;RL C
instr 	fetch_D,	op_RL,		store2_D	;12		;RL D
instr 	fetch_E,	op_RL,		store2_E	;13		;RL E
instr 	fetch_H,	op_RL,		store2_H	;14		;RL H
instr 	fetch_L,	op_RL,		store2_L	;15		;RL L
instr 	fetch2_mhl,	op_RL,		store_MHL	;16		;RL (HL)
instr 	fetch_A,	op_RL,		store2_A	;17		;RL A
instr 	fetch_B,	op_RR,		store2_B	;18		;RR B
instr 	fetch_C,	op_RR,		store2_C	;19		;RR C
instr 	fetch_D,	op_RR,		store2_D	;1A		;RR D
instr 	fetch_E,	op_RR,		store2_E	;1B		;RR E
instr 	fetch_H,	op_RR,		store2_H	;1C		;RR H
instr 	fetch_L,	op_RR,		store2_L	;1D		;RR L
instr 	fetch2_mhl,	op_RR,		store_MHL	;1E		;RR (HL)
instr 	fetch_A,	op_RR,		store2_A	;1F		;RR A
instr 	fetch_B,	op_SLA,		store2_B	;20		;SLA B
instr 	fetch_C,	op_SLA,		store2_C	;21		;SLA C
instr 	fetch_D,	op_SLA,		store2_D	;22		;SLA D
instr 	fetch_E,	op_SLA,		store2_E	;23		;SLA E
instr 	fetch_H,	op_SLA,		store2_H	;24		;SLA H
instr 	fetch_L,	op_SLA,		store2_L	;25		;SLA L
instr 	fetch2_mhl,	op_SLA,		store_MHL	;26		;SLA (HL)
instr 	fetch_A,	op_SLA,		store2_A	;27		;SLA A
instr 	fetch_B,	op_SRA,		store2_B	;28		;SRA B
instr 	fetch_C,	op_SRA,		store2_C	;29		;SRA C
instr 	fetch_D,	op_SRA,		store2_D	;2A		;SRA D
instr 	fetch_E,	op_SRA,		store2_E	;2B		;SRA E
instr 	fetch_H,	op_SRA,		store2_H	;2C		;SRA H
instr 	fetch_L,	op_SRA,		store2_L	;2D		;SRA L
instr 	fetch2_mhl,	op_SRA,		store_MHL	;2E		;SRA (HL)
instr 	fetch_A,	op_SRA,		store2_A	;2F		;SRA A
instr 	fetch_B,	op_SLL,		store2_B	;30		;SLL B
instr 	fetch_C,	op_SLL,		store2_C	;31		;SLL C
instr 	fetch_D,	op_SLL,		store2_D	;32		;SLL D
instr 	fetch_E,	op_SLL,		store2_E	;33		;SLL E
instr 	fetch_H,	op_SLL,		store2_H	;34		;SLL H
instr 	fetch_L,	op_SLL,		store2_L	;35		;SLL L
instr 	fetch2_mhl,	op_SLL,		store_MHL	;36		;SLL (HL)
instr 	fetch_A,	op_SLL,		store2_A	;37		;SLL A
instr 	fetch_B,	op_SRL,		store2_B	;38		;SRL B
instr 	fetch_C,	op_SRL,		store2_C	;39		;SRL C
instr 	fetch_D,	op_SRL,		store2_D	;3A		;SRL D
instr 	fetch_E,	op_SRL,		store2_E	;3B		;SRL E
instr 	fetch_H,	op_SRL,		store2_H	;3C		;SRL H
instr 	fetch_L,	op_SRL,		store2_L	;3D		;SRL L
instr 	fetch2_mhl,	op_SRL,		store_MHL	;3E		;SRL (HL)
instr 	fetch_A,	op_SRL,		store2_A	;3F		;SRL A
instr 	fetch_B,	op_BIT0,	store_nop	;40		;BIT 0,B
instr 	fetch_C,	op_BIT0,	store_nop	;41		;BIT 0,C
instr 	fetch_D,	op_BIT0,	store_nop	;42		;BIT 0,D
instr 	fetch_E,	op_BIT0,	store_nop	;43		;BIT 0,E
instr 	fetch_H,	op_BIT0,	store_nop	;44		;BIT 0,H
instr 	fetch_L,	op_BIT0,	store_nop	;45		;BIT 0,L
instr 	fetch2_mhl,	op_BIT0,	store_nop	;46		;BIT 0,(HL)
instr 	fetch_A,	op_BIT0,	store_nop	;47		;BIT 0,A   
instr 	fetch_B,	op_BIT1,	store_nop	;48		;BIT 1,B
instr 	fetch_C,	op_BIT1,	store_nop	;49		;BIT 1,C
instr 	fetch_D,	op_BIT1,	store_nop	;4A		;BIT 1,D
instr 	fetch_E,	op_BIT1,	store_nop	;4B		;BIT 1,E
instr 	fetch_H,	op_BIT1,	store_nop	;4C		;BIT 1,H
instr 	fetch_L,	op_BIT1,	store_nop	;4D		;BIT 1,L
instr 	fetch2_mhl,	op_BIT1,	store_nop	;4E		;BIT 1,(HL)
instr 	fetch_A,	op_BIT1,	store_nop	;4F		;BIT 1,A
instr 	fetch_B,	op_BIT2,	store_nop	;50		;BIT 2,B
instr 	fetch_C,	op_BIT2,	store_nop	;51		;BIT 2,C
instr 	fetch_D,	op_BIT2,	store_nop	;52		;BIT 2,D
instr 	fetch_E,	op_BIT2,	store_nop	;53		;BIT 2,E
instr 	fetch_H,	op_BIT2,	store_nop	;54		;BIT 2,H
instr 	fetch_L,	op_BIT2,	store_nop	;55		;BIT 2,L
instr 	fetch2_mhl,	op_BIT2,	store_nop	;56		;BIT 2,(HL)
instr 	fetch_A,	op_BIT2,	store_nop	;57		;BIT 2,A
instr 	fetch_B,	op_BIT3,	store_nop	;58		;BIT 3,B
instr 	fetch_C,	op_BIT3,	store_nop	;59		;BIT 3,C
instr 	fetch_D,	op_BIT3,	store_nop	;5A		;BIT 3,D
instr 	fetch_E,	op_BIT3,	store_nop	;5B		;BIT 3,E
instr 	fetch_H,	op_BIT3,	store_nop	;5C		;BIT 3,H
instr 	fetch_L,	op_BIT3,	store_nop	;5D		;BIT 3,L
instr 	fetch2_mhl,	op_BIT3,	store_nop	;5E		;BIT 3,(HL)
instr 	fetch_A,	op_BIT3,	store_nop	;5F		;BIT 3,A
instr 	fetch_B,	op_BIT4,	store_nop	;60		;BIT 4,B
instr 	fetch_C,	op_BIT4,	store_nop	;61		;BIT 4,C
instr 	fetch_D,	op_BIT4,	store_nop	;62		;BIT 4,D
instr 	fetch_E,	op_BIT4,	store_nop	;63		;BIT 4,E
instr 	fetch_H,	op_BIT4,	store_nop	;64		;BIT 4,H
instr 	fetch_L,	op_BIT4,	store_nop	;65		;BIT 4,L
instr 	fetch2_mhl,	op_BIT4,	store_nop	;66		;BIT 4,(HL)
instr 	fetch_A,	op_BIT4,	store_nop	;67		;BIT 4,A
instr 	fetch_B,	op_BIT5,	store_nop	;68		;BIT 5,B
instr 	fetch_C,	op_BIT5,	store_nop	;69		;BIT 5,C
instr 	fetch_D,	op_BIT5,	store_nop	;6A		;BIT 5,D
instr 	fetch_E,	op_BIT5,	store_nop	;6B		;BIT 5,E
instr 	fetch_H,	op_BIT5,	store_nop	;6C		;BIT 5,H
instr 	fetch_L,	op_BIT5,	store_nop	;6D		;BIT 5,L
instr 	fetch2_mhl,	op_BIT5,	store_nop	;6E		;BIT 5,(HL)
instr 	fetch_A,	op_BIT5,	store_nop	;6F		;BIT 5,A
instr 	fetch_B,	op_BIT6,	store_nop	;70		;BIT 6,B
instr 	fetch_C,	op_BIT6,	store_nop	;71		;BIT 6,C
instr 	fetch_D,	op_BIT6,	store_nop	;72		;BIT 6,D
instr 	fetch_E,	op_BIT6,	store_nop	;73		;BIT 6,E
instr 	fetch_H,	op_BIT6,	store_nop	;74		;BIT 6,H
instr 	fetch_L,	op_BIT6,	store_nop	;75		;BIT 6,L
instr 	fetch2_mhl,	op_BIT6,	store_nop	;76		;BIT 6,(HL)
instr 	fetch_A,	op_BIT6,	store_nop	;77		;BIT 6,A
instr 	fetch_B,	op_BIT7,	store_nop	;78		;BIT 7,B
instr 	fetch_C,	op_BIT7,	store_nop	;79		;BIT 7,C
instr 	fetch_D,	op_BIT7,	store_nop	;7A		;BIT 7,D
instr 	fetch_E,	op_BIT7,	store_nop	;7B		;BIT 7,E
instr 	fetch_H,	op_BIT7,	store_nop	;7C		;BIT 7,H
instr 	fetch_L,	op_BIT7,	store_nop	;7D		;BIT 7,L
instr 	fetch2_mhl,	op_BIT7,	store_nop	;7E		;BIT 7,(HL)
instr 	fetch_A,	op_BIT7,	store_nop	;7F		;BIT 7,A
instr 	fetch_B,	op_RES0,	store2_B	;80		;RES 0,B
instr 	fetch_C,	op_RES0,	store2_C	;81		;RES 0,C
instr 	fetch_D,	op_RES0,	store2_D	;82		;RES 0,D
instr 	fetch_E,	op_RES0,	store2_E	;83		;RES 0,E
instr 	fetch_H,	op_RES0,	store2_H	;84		;RES 0,H
instr 	fetch_L,	op_RES0,	store2_L	;85		;RES 0,L
instr 	fetch2_mhl,	op_RES0,	store_MHL	;86		;RES 0,(HL)
instr 	fetch_A,	op_RES0,	store2_A	;87		;RES 0,A
instr 	fetch_B,	op_RES1,	store2_B	;88		;RES 1,B
instr 	fetch_C,	op_RES1,	store2_C	;89		;RES 1,C
instr 	fetch_D,	op_RES1,	store2_D	;8A		;RES 1,D
instr 	fetch_E,	op_RES1,	store2_E	;8B		;RES 1,E
instr 	fetch_H,	op_RES1,	store2_H	;8C		;RES 1,H
instr 	fetch_L,	op_RES1,	store2_L	;8D		;RES 1,L
instr 	fetch2_mhl,	op_RES1,	store_MHL	;8E		;RES 1,(HL)
instr 	fetch_A,	op_RES1,	store2_A	;8F		;RES 1,A
instr 	fetch_B,	op_RES2,	store2_B	;90		;RES 2,B
instr 	fetch_C,	op_RES2,	store2_C	;91		;RES 2,C
instr 	fetch_D,	op_RES2,	store2_D	;92		;RES 2,D
instr 	fetch_E,	op_RES2,	store2_E	;93		;RES 2,E
instr 	fetch_H,	op_RES2,	store2_H	;94		;RES 2,H
instr 	fetch_L,	op_RES2,	store2_L	;95		;RES 2,L
instr 	fetch2_mhl,	op_RES2,	store_MHL	;96		;RES 2,(HL)
instr 	fetch_A,	op_RES2,	store2_A	;97		;RES 2,A
instr 	fetch_B,	op_RES3,	store2_B	;98		;RES 3,B
instr 	fetch_C,	op_RES3,	store2_C	;99		;RES 3,C
instr 	fetch_D,	op_RES3,	store2_D	;9A		;RES 3,D
instr 	fetch_E,	op_RES3,	store2_E	;9B		;RES 3,E
instr 	fetch_H,	op_RES3,	store2_H	;9C		;RES 3,H
instr 	fetch_L,	op_RES3,	store2_L	;9D		;RES 3,L
instr 	fetch2_mhl,	op_RES3,	store_MHL	;9E		;RES 3,(HL)
instr 	fetch_A,	op_RES3,	store2_A	;9F		;RES 3,A
instr 	fetch_B,	op_RES4,	store2_B	;A0		;RES 4,B
instr 	fetch_C,	op_RES4,	store2_C	;A1		;RES 4,C
instr 	fetch_D,	op_RES4,	store2_D	;A2		;RES 4,D
instr 	fetch_E,	op_RES4,	store2_E	;A3		;RES 4,E
instr 	fetch_H,	op_RES4,	store2_H	;A4		;RES 4,H
instr 	fetch_L,	op_RES4,	store2_L	;A5		;RES 4,L
instr 	fetch2_mhl,	op_RES4,	store_MHL	;A6		;RES 4,(HL)
instr 	fetch_A,	op_RES4,	store2_A	;A7		;RES 4,A
instr 	fetch_B,	op_RES5,	store2_B	;A8		;RES 5,B
instr 	fetch_C,	op_RES5,	store2_C	;A9		;RES 5,C
instr 	fetch_D,	op_RES5,	store2_D	;AA		;RES 5,D
instr 	fetch_E,	op_RES5,	store2_E	;AB		;RES 5,E
instr 	fetch_H,	op_RES5,	store2_H	;AC		;RES 5,H
instr 	fetch_L,	op_RES5,	store2_L	;AD		;RES 5,L
instr 	fetch2_mhl,	op_RES5,	store_MHL	;AE		;RES 5,(HL)
instr 	fetch_A,	op_RES5,	store2_A	;AF		;RES 5,A
instr 	fetch_B,	op_RES6,	store2_B	;B0		;RES 6,B
instr 	fetch_C,	op_RES6,	store2_C	;B1		;RES 6,C
instr 	fetch_D,	op_RES6,	store2_D	;B2		;RES 6,D
instr 	fetch_E,	op_RES6,	store2_E	;B3		;RES 6,E
instr 	fetch_H,	op_RES6,	store2_H	;B4		;RES 6,H
instr 	fetch_L,	op_RES6,	store2_L	;B5		;RES 6,L
instr 	fetch2_mhl,	op_RES6,	store_MHL	;B6		;RES 6,(HL)
instr 	fetch_A,	op_RES6,	store2_A	;B7		;RES 6,A
instr 	fetch_B,	op_RES7,	store2_B	;B8		;RES 7,B
instr 	fetch_C,	op_RES7,	store2_C	;B9		;RES 7,C
instr 	fetch_D,	op_RES7,	store2_D	;BA		;RES 7,D
instr 	fetch_E,	op_RES7,	store2_E	;BB		;RES 7,E
instr 	fetch_H,	op_RES7,	store2_H	;BC		;RES 7,H
instr 	fetch_L,	op_RES7,	store2_L	;BD		;RES 7,L
instr 	fetch2_mhl,	op_RES7,	store_MHL	;BE		;RES 7,(HL)
instr 	fetch_A,	op_RES7,	store2_A	;BF		;RES 7,A
instr 	fetch_B,	op_SET0,	store2_B	;C0		;SET 0,B
instr 	fetch_C,	op_SET0,	store2_C	;C1		;SET 0,C
instr 	fetch_D,	op_SET0,	store2_D	;C2		;SET 0,D
instr 	fetch_E,	op_SET0,	store2_E	;C3		;SET 0,E
instr 	fetch_H,	op_SET0,	store2_H	;C4		;SET 0,H
instr 	fetch_L,	op_SET0,	store2_L	;C5		;SET 0,L
instr 	fetch2_mhl,	op_SET0,	store_MHL	;C6		;SET 0,(HL)
instr 	fetch_A,	op_SET0,	store2_A	;C7		;SET 0,A
instr 	fetch_B,	op_SET1,	store2_B	;C8		;SET 1,B
instr 	fetch_C,	op_SET1,	store2_C	;C9		;SET 1,C
instr 	fetch_D,	op_SET1,	store2_D	;CA		;SET 1,D
instr 	fetch_E,	op_SET1,	store2_E	;CB		;SET 1,E
instr 	fetch_H,	op_SET1,	store2_H	;CC		;SET 1,H
instr 	fetch_L,	op_SET1,	store2_L	;CD		;SET 1,L
instr 	fetch2_mhl,	op_SET1,	store_MHL	;CE		;SET 1,(HL)
instr 	fetch_A,	op_SET1,	store2_A	;CF		;SET 1,A
instr 	fetch_B,	op_SET2,	store2_B	;D0		;SET 2,B
instr 	fetch_C,	op_SET2,	store2_C	;D1		;SET 2,C
instr 	fetch_D,	op_SET2,	store2_D	;D2		;SET 2,D
instr 	fetch_E,	op_SET2,	store2_E	;D3		;SET 2,E
instr 	fetch_H,	op_SET2,	store2_H	;D4		;SET 2,H
instr 	fetch_L,	op_SET2,	store2_L	;D5		;SET 2,L
instr 	fetch2_mhl,	op_SET2,	store_MHL	;D6		;SET 2,(HL)
instr 	fetch_A,	op_SET2,	store2_A	;D7		;SET 2,A
instr 	fetch_B,	op_SET3,	store2_B	;D8		;SET 3,B
instr 	fetch_C,	op_SET3,	store2_C	;D9		;SET 3,C
instr 	fetch_D,	op_SET3,	store2_D	;DA		;SET 3,D
instr 	fetch_E,	op_SET3,	store2_E	;DB		;SET 3,E
instr 	fetch_H,	op_SET3,	store2_H	;DC		;SET 3,H
instr 	fetch_L,	op_SET3,	store2_L	;DD		;SET 3,L
instr 	fetch2_mhl,	op_SET3,	store_MHL	;DE		;SET 3,(HL)
instr 	fetch_A,	op_SET3,	store2_A	;DF		;SET 3,A
instr 	fetch_B,	op_SET4,	store2_B	;E0		;SET 4,B
instr 	fetch_C,	op_SET4,	store2_C	;E1		;SET 4,C
instr 	fetch_D,	op_SET4,	store2_D	;E2		;SET 4,D
instr 	fetch_E,	op_SET4,	store2_E	;E3		;SET 4,E
instr 	fetch_H,	op_SET4,	store2_H	;E4		;SET 4,H
instr 	fetch_L,	op_SET4,	store2_L	;E5		;SET 4,L
instr 	fetch2_mhl,	op_SET4,	store_MHL	;E6		;SET 4,(HL)
instr 	fetch_A,	op_SET4,	store2_A	;E7		;SET 4,A
instr 	fetch_B,	op_SET5,	store2_B	;E8		;SET 5,B
instr 	fetch_C,	op_SET5,	store2_C	;E9		;SET 5,C
instr 	fetch_D,	op_SET5,	store2_D	;EA		;SET 5,D
instr 	fetch_E,	op_SET5,	store2_E	;EB		;SET 5,E
instr 	fetch_H,	op_SET5,	store2_H	;EC		;SET 5,H
instr 	fetch_L,	op_SET5,	store2_L	;ED		;SET 5,L
instr 	fetch2_mhl,	op_SET5,	store_MHL	;EE		;SET 5,(HL)
instr 	fetch_A,	op_SET5,	store2_A	;EF		;SET 5,A
instr 	fetch_B,	op_SET6,	store2_B	;F0		;SET 6,B
instr 	fetch_C,	op_SET6,	store2_C	;F1		;SET 6,C
instr 	fetch_D,	op_SET6,	store2_D	;F2		;SET 6,D
instr 	fetch_E,	op_SET6,	store2_E	;F3		;SET 6,E
instr 	fetch_H,	op_SET6,	store2_H	;F4		;SET 6,H
instr 	fetch_L,	op_SET6,	store2_L	;F5		;SET 6,L
instr 	fetch2_mhl,	op_SET6,	store_MHL	;F6		;SET 6,(HL)
instr 	fetch_A,	op_SET6,	store2_A	;F7		;SET 6,A
instr 	fetch_B,	op_SET7,	store2_B	;F8		;SET 7,B
instr 	fetch_C,	op_SET7,	store2_C	;F9		;SET 7,C
instr 	fetch_D,	op_SET7,	store2_D	;FA		;SET 7,D
instr 	fetch_E,	op_SET7,	store2_E	;FB		;SET 7,E
instr 	fetch_H,	op_SET7,	store2_H	;FC		;SET 7,H
instr 	fetch_L,	op_SET7,	store2_L	;FD		;SET 7,L
instr 	fetch2_mhl,	op_SET7,	store_MHL	;FE		;SET 7,(HL)
instr 	fetch_A,	op_SET7,	store2_A	;FF		;SET 7,A


	opctable DDFDCBjmp, PC ;+256

instr 	fetch_nop,	op_RLC,		store2_B	;00		;RLC (Ix+d),B
instr 	fetch_nop,	op_RLC,		store2_C	;01		;RLC (Ix+d),C
instr 	fetch_nop,	op_RLC,		store2_D	;02		;RLC (Ix+d),D
instr 	fetch_nop,	op_RLC,		store2_E	;03		;RLC (Ix+d),E
instr 	fetch_nop,	op_RLC,		store2_H	;04		;RLC (Ix+d),H
instr 	fetch_nop,	op_RLC,		store2_L	;05		;RLC (Ix+d),L
instr 	fetch_nop,	op_RLC,		store_nop	;06		;RLC (Ix+d)  
instr 	fetch_nop,	op_RLC,		store2_A	;07		;RLC (Ix+d),A
instr 	fetch_nop,	op_RRC,		store2_B	;08		;RRC (Ix+d),B
instr 	fetch_nop,	op_RRC,		store2_C	;09		;RRC (Ix+d),C
instr 	fetch_nop,	op_RRC,		store2_D	;0A		;RRC (Ix+d),D
instr 	fetch_nop,	op_RRC,		store2_E	;0B		;RRC (Ix+d),E
instr 	fetch_nop,	op_RRC,		store2_H	;0C		;RRC (Ix+d),H
instr 	fetch_nop,	op_RRC,		store2_L	;0D		;RRC (Ix+d),L
instr 	fetch_nop,	op_RRC,		store_nop	;0E		;RRC (Ix+d)  
instr 	fetch_nop,	op_RRC,		store2_A	;0F		;RRC (Ix+d),A
instr 	fetch_nop,	op_RL,		store2_B	;10		;RL  (Ix+d),B
instr 	fetch_nop,	op_RL,		store2_C	;11		;RL  (Ix+d),C
instr 	fetch_nop,	op_RL,		store2_D	;12		;RL  (Ix+d),D
instr 	fetch_nop,	op_RL,		store2_E	;13		;RL  (Ix+d),E
instr 	fetch_nop,	op_RL,		store2_H	;14		;RL  (Ix+d),H
instr 	fetch_nop,	op_RL,		store2_L	;15		;RL  (Ix+d),L
instr 	fetch_nop,	op_RL,		store_nop	;16		;RL  (Ix+d)  
instr 	fetch_nop,	op_RL,		store2_A	;17		;RL  (Ix+d),A
instr 	fetch_nop,	op_RR,		store2_B	;18		;RR  (Ix+d),B
instr 	fetch_nop,	op_RR,		store2_C	;19		;RR  (Ix+d),C
instr 	fetch_nop,	op_RR,		store2_D	;1A		;RR  (Ix+d),D
instr 	fetch_nop,	op_RR,		store2_E	;1B		;RR  (Ix+d),E
instr 	fetch_nop,	op_RR,		store2_H	;1C		;RR  (Ix+d),H
instr 	fetch_nop,	op_RR,		store2_L	;1D		;RR  (Ix+d),L
instr 	fetch_nop,	op_RR,		store_nop	;1E		;RR  (Ix+d)  
instr 	fetch_nop,	op_RR,		store2_A	;1F		;RR  (Ix+d),A
instr 	fetch_nop,	op_SLA,		store2_B	;20		;SLA (Ix+d),B
instr 	fetch_nop,	op_SLA,		store2_C	;21		;SLA (Ix+d),C
instr 	fetch_nop,	op_SLA,		store2_D	;22		;SLA (Ix+d),D
instr 	fetch_nop,	op_SLA,		store2_E	;23		;SLA (Ix+d),E
instr 	fetch_nop,	op_SLA,		store2_H	;24		;SLA (Ix+d),H
instr 	fetch_nop,	op_SLA,		store2_L	;25		;SLA (Ix+d),L
instr 	fetch_nop,	op_SLA,		store_nop	;26		;SLA (Ix+d)  
instr 	fetch_nop,	op_SLA,		store2_A	;27		;SLA (Ix+d),A
instr 	fetch_nop,	op_SRA,		store2_B	;28		;SRA (Ix+d),B
instr 	fetch_nop,	op_SRA,		store2_C	;29		;SRA (Ix+d),C
instr 	fetch_nop,	op_SRA,		store2_D	;2A		;SRA (Ix+d),D
instr 	fetch_nop,	op_SRA,		store2_E	;2B		;SRA (Ix+d),E
instr 	fetch_nop,	op_SRA,		store2_H	;2C		;SRA (Ix+d),H
instr 	fetch_nop,	op_SRA,		store2_L	;2D		;SRA (Ix+d),L
instr 	fetch_nop,	op_SRA,		store_nop	;2E		;SRA (Ix+d)  
instr 	fetch_nop,	op_SRA,		store2_A	;2F		;SRA (Ix+d),A
instr 	fetch_nop,	op_SLL,		store2_B	;30		;SLL (Ix+d),B
instr 	fetch_nop,	op_SLL,		store2_C	;31		;SLL (Ix+d),C
instr 	fetch_nop,	op_SLL,		store2_D	;32		;SLL (Ix+d),D
instr 	fetch_nop,	op_SLL,		store2_E	;33		;SLL (Ix+d),E
instr 	fetch_nop,	op_SLL,		store2_H	;34		;SLL (Ix+d),H
instr 	fetch_nop,	op_SLL,		store2_L	;35		;SLL (Ix+d),L
instr 	fetch_nop,	op_SLL,		store_nop	;36		;SLL (Ix+d)  
instr 	fetch_nop,	op_SLL,		store2_A	;37		;SLL (Ix+d),A
instr 	fetch_nop,	op_SRL,		store2_B	;38		;SRL (Ix+d),B
instr 	fetch_nop,	op_SRL,		store2_C	;39		;SRL (Ix+d),C
instr 	fetch_nop,	op_SRL,		store2_D	;3A		;SRL (Ix+d),D
instr 	fetch_nop,	op_SRL,		store2_E	;3B		;SRL (Ix+d),E
instr 	fetch_nop,	op_SRL,		store2_H	;3C		;SRL (Ix+d),H
instr 	fetch_nop,	op_SRL,		store2_L	;3D		;SRL (Ix+d),L
instr 	fetch_nop,	op_SRL,		store_nop	;3E		;SRL (Ix+d)  
instr 	fetch_nop,	op_SRL,		store2_A	;3F		;SRL (Ix+d),A
instr 	fetch_nop,	op_BIT0,	store_nop	;40		;BIT 0,(Ix+d),B
instr 	fetch_nop,	op_BIT0,	store_nop	;41		;BIT 0,(Ix+d),C
instr 	fetch_nop,	op_BIT0,	store_nop	;42		;BIT 0,(Ix+d),D
instr 	fetch_nop,	op_BIT0,	store_nop	;43		;BIT 0,(Ix+d),E
instr 	fetch_nop,	op_BIT0,	store_nop	;44		;BIT 0,(Ix+d),H
instr 	fetch_nop,	op_BIT0,	store_nop	;45		;BIT 0,(Ix+d),L
instr 	fetch_nop,	op_BIT0,	store_nop	;46		;BIT 0,(Ix+d)
instr 	fetch_nop,	op_BIT0,	store_nop	;47		;BIT 0,(Ix+d),A   
instr 	fetch_nop,	op_BIT1,	store_nop	;48		;BIT 1,(Ix+d),B
instr 	fetch_nop,	op_BIT1,	store_nop	;49		;BIT 1,(Ix+d),C
instr 	fetch_nop,	op_BIT1,	store_nop	;4A		;BIT 1,(Ix+d),D
instr 	fetch_nop,	op_BIT1,	store_nop	;4B		;BIT 1,(Ix+d),E
instr 	fetch_nop,	op_BIT1,	store_nop	;4C		;BIT 1,(Ix+d),H
instr 	fetch_nop,	op_BIT1,	store_nop	;4D		;BIT 1,(Ix+d),L
instr 	fetch_nop,	op_BIT1,	store_nop	;4E		;BIT 1,(Ix+d)
instr 	fetch_nop,	op_BIT1,	store_nop	;4F		;BIT 1,(Ix+d),A
instr 	fetch_nop,	op_BIT2,	store_nop	;50		;BIT 2,(Ix+d),B
instr 	fetch_nop,	op_BIT2,	store_nop	;51		;BIT 2,(Ix+d),C
instr 	fetch_nop,	op_BIT2,	store_nop	;52		;BIT 2,(Ix+d),D
instr 	fetch_nop,	op_BIT2,	store_nop	;53		;BIT 2,(Ix+d),E
instr 	fetch_nop,	op_BIT2,	store_nop	;54		;BIT 2,(Ix+d),H
instr 	fetch_nop,	op_BIT2,	store_nop	;55		;BIT 2,(Ix+d),L
instr 	fetch_nop,	op_BIT2,	store_nop	;56		;BIT 2,(Ix+d)
instr 	fetch_nop,	op_BIT2,	store_nop	;57		;BIT 2,(Ix+d),A
instr 	fetch_nop,	op_BIT3,	store_nop	;58		;BIT 3,(Ix+d),B
instr 	fetch_nop,	op_BIT3,	store_nop	;59		;BIT 3,(Ix+d),C
instr 	fetch_nop,	op_BIT3,	store_nop	;5A		;BIT 3,(Ix+d),D
instr 	fetch_nop,	op_BIT3,	store_nop	;5B		;BIT 3,(Ix+d),E
instr 	fetch_nop,	op_BIT3,	store_nop	;5C		;BIT 3,(Ix+d),H
instr 	fetch_nop,	op_BIT3,	store_nop	;5D		;BIT 3,(Ix+d),L
instr 	fetch_nop,	op_BIT3,	store_nop	;5E		;BIT 3,(Ix+d)
instr 	fetch_nop,	op_BIT3,	store_nop	;5F		;BIT 3,(Ix+d),A
instr 	fetch_nop,	op_BIT4,	store_nop	;60		;BIT 4,(Ix+d),B
instr 	fetch_nop,	op_BIT4,	store_nop	;61		;BIT 4,(Ix+d),C
instr 	fetch_nop,	op_BIT4,	store_nop	;62		;BIT 4,(Ix+d),D
instr 	fetch_nop,	op_BIT4,	store_nop	;63		;BIT 4,(Ix+d),E
instr 	fetch_nop,	op_BIT4,	store_nop	;64		;BIT 4,(Ix+d),H
instr 	fetch_nop,	op_BIT4,	store_nop	;65		;BIT 4,(Ix+d),L
instr 	fetch_nop,	op_BIT4,	store_nop	;66		;BIT 4,(Ix+d)
instr 	fetch_nop,	op_BIT4,	store_nop	;67		;BIT 4,(Ix+d),A
instr 	fetch_nop,	op_BIT5,	store_nop	;68		;BIT 5,(Ix+d),B
instr 	fetch_nop,	op_BIT5,	store_nop	;69		;BIT 5,(Ix+d),C
instr 	fetch_nop,	op_BIT5,	store_nop	;6A		;BIT 5,(Ix+d),D
instr 	fetch_nop,	op_BIT5,	store_nop	;6B		;BIT 5,(Ix+d),E
instr 	fetch_nop,	op_BIT5,	store_nop	;6C		;BIT 5,(Ix+d),H
instr 	fetch_nop,	op_BIT5,	store_nop	;6D		;BIT 5,(Ix+d),L
instr 	fetch_nop,	op_BIT5,	store_nop	;6E		;BIT 5,(Ix+d)
instr 	fetch_nop,	op_BIT5,	store_nop	;6F		;BIT 5,(Ix+d),A
instr 	fetch_nop,	op_BIT6,	store_nop	;70		;BIT 6,(Ix+d),B
instr 	fetch_nop,	op_BIT6,	store_nop	;71		;BIT 6,(Ix+d),C
instr 	fetch_nop,	op_BIT6,	store_nop	;72		;BIT 6,(Ix+d),D
instr 	fetch_nop,	op_BIT6,	store_nop	;73		;BIT 6,(Ix+d),E
instr 	fetch_nop,	op_BIT6,	store_nop	;74		;BIT 6,(Ix+d),H
instr 	fetch_nop,	op_BIT6,	store_nop	;75		;BIT 6,(Ix+d),L
instr 	fetch_nop,	op_BIT6,	store_nop	;76		;BIT 6,(Ix+d)
instr 	fetch_nop,	op_BIT6,	store_nop	;77		;BIT 6,(Ix+d),A
instr 	fetch_nop,	op_BIT7,	store_nop	;78		;BIT 7,(Ix+d),B
instr 	fetch_nop,	op_BIT7,	store_nop	;79		;BIT 7,(Ix+d),C
instr 	fetch_nop,	op_BIT7,	store_nop	;7A		;BIT 7,(Ix+d),D
instr 	fetch_nop,	op_BIT7,	store_nop	;7B		;BIT 7,(Ix+d),E
instr 	fetch_nop,	op_BIT7,	store_nop	;7C		;BIT 7,(Ix+d),H
instr 	fetch_nop,	op_BIT7,	store_nop	;7D		;BIT 7,(Ix+d),L
instr 	fetch_nop,	op_BIT7,	store_nop	;7E		;BIT 7,(Ix+d)
instr 	fetch_nop,	op_BIT7,	store_nop	;7F		;BIT 7,(Ix+d),A
instr 	fetch_nop,	op_RES0,	store2_B	;80		;RES 0,(Ix+d),B
instr 	fetch_nop,	op_RES0,	store2_C	;81		;RES 0,(Ix+d),C
instr 	fetch_nop,	op_RES0,	store2_D	;82		;RES 0,(Ix+d),D
instr 	fetch_nop,	op_RES0,	store2_E	;83		;RES 0,(Ix+d),E
instr 	fetch_nop,	op_RES0,	store2_H	;84		;RES 0,(Ix+d),H
instr 	fetch_nop,	op_RES0,	store2_L	;85		;RES 0,(Ix+d),L
instr 	fetch_nop,	op_RES0,	store_nop	;86		;RES 0,(Ix+d)
instr 	fetch_nop,	op_RES0,	store2_A	;87		;RES 0,(Ix+d),A
instr 	fetch_nop,	op_RES1,	store2_B	;88		;RES 1,(Ix+d),B
instr 	fetch_nop,	op_RES1,	store2_C	;89		;RES 1,(Ix+d),C
instr 	fetch_nop,	op_RES1,	store2_D	;8A		;RES 1,(Ix+d),D
instr 	fetch_nop,	op_RES1,	store2_E	;8B		;RES 1,(Ix+d),E
instr 	fetch_nop,	op_RES1,	store2_H	;8C		;RES 1,(Ix+d),H
instr 	fetch_nop,	op_RES1,	store2_L	;8D		;RES 1,(Ix+d),L
instr 	fetch_nop,	op_RES1,	store_nop	;8E		;RES 1,(Ix+d)
instr 	fetch_nop,	op_RES1,	store2_A	;8F		;RES 1,(Ix+d),A
instr 	fetch_nop,	op_RES2,	store2_B	;90		;RES 2,(Ix+d),B
instr 	fetch_nop,	op_RES2,	store2_C	;91		;RES 2,(Ix+d),C
instr 	fetch_nop,	op_RES2,	store2_D	;92		;RES 2,(Ix+d),D
instr 	fetch_nop,	op_RES2,	store2_E	;93		;RES 2,(Ix+d),E
instr 	fetch_nop,	op_RES2,	store2_H	;94		;RES 2,(Ix+d),H
instr 	fetch_nop,	op_RES2,	store2_L	;95		;RES 2,(Ix+d),L
instr 	fetch_nop,	op_RES2,	store_nop	;96		;RES 2,(Ix+d)
instr 	fetch_nop,	op_RES2,	store2_A	;97		;RES 2,(Ix+d),A
instr 	fetch_nop,	op_RES3,	store2_B	;98		;RES 3,(Ix+d),B
instr 	fetch_nop,	op_RES3,	store2_C	;99		;RES 3,(Ix+d),C
instr 	fetch_nop,	op_RES3,	store2_D	;9A		;RES 3,(Ix+d),D
instr 	fetch_nop,	op_RES3,	store2_E	;9B		;RES 3,(Ix+d),E
instr 	fetch_nop,	op_RES3,	store2_H	;9C		;RES 3,(Ix+d),H
instr 	fetch_nop,	op_RES3,	store2_L	;9D		;RES 3,(Ix+d),L
instr 	fetch_nop,	op_RES3,	store_nop	;9E		;RES 3,(Ix+d)
instr 	fetch_nop,	op_RES3,	store2_A	;9F		;RES 3,(Ix+d),A
instr 	fetch_nop,	op_RES4,	store2_B	;A0		;RES 4,(Ix+d),B
instr 	fetch_nop,	op_RES4,	store2_C	;A1		;RES 4,(Ix+d),C
instr 	fetch_nop,	op_RES4,	store2_D	;A2		;RES 4,(Ix+d),D
instr 	fetch_nop,	op_RES4,	store2_E	;A3		;RES 4,(Ix+d),E
instr 	fetch_nop,	op_RES4,	store2_H	;A4		;RES 4,(Ix+d),H
instr 	fetch_nop,	op_RES4,	store2_L	;A5		;RES 4,(Ix+d),L
instr 	fetch_nop,	op_RES4,	store_nop	;A6		;RES 4,(Ix+d)
instr 	fetch_nop,	op_RES4,	store2_A	;A7		;RES 4,(Ix+d),A
instr 	fetch_nop,	op_RES5,	store2_B	;A8		;RES 5,(Ix+d),B
instr 	fetch_nop,	op_RES5,	store2_C	;A9		;RES 5,(Ix+d),C
instr 	fetch_nop,	op_RES5,	store2_D	;AA		;RES 5,(Ix+d),D
instr 	fetch_nop,	op_RES5,	store2_E	;AB		;RES 5,(Ix+d),E
instr 	fetch_nop,	op_RES5,	store2_H	;AC		;RES 5,(Ix+d),H
instr 	fetch_nop,	op_RES5,	store2_L	;AD		;RES 5,(Ix+d),L
instr 	fetch_nop,	op_RES5,	store_nop	;AE		;RES 5,(Ix+d)
instr 	fetch_nop,	op_RES5,	store2_A	;AF		;RES 5,(Ix+d),A
instr 	fetch_nop,	op_RES6,	store2_B	;B0		;RES 6,(Ix+d),B
instr 	fetch_nop,	op_RES6,	store2_C	;B1		;RES 6,(Ix+d),C
instr 	fetch_nop,	op_RES6,	store2_D	;B2		;RES 6,(Ix+d),D
instr 	fetch_nop,	op_RES6,	store2_E	;B3		;RES 6,(Ix+d),E
instr 	fetch_nop,	op_RES6,	store2_H	;B4		;RES 6,(Ix+d),H
instr 	fetch_nop,	op_RES6,	store2_L	;B5		;RES 6,(Ix+d),L
instr 	fetch_nop,	op_RES6,	store_nop	;B6		;RES 6,(Ix+d)
instr 	fetch_nop,	op_RES6,	store2_A	;B7		;RES 6,(Ix+d),A
instr 	fetch_nop,	op_RES7,	store2_B	;B8		;RES 7,(Ix+d),B
instr 	fetch_nop,	op_RES7,	store2_C	;B9		;RES 7,(Ix+d),C
instr 	fetch_nop,	op_RES7,	store2_D	;BA		;RES 7,(Ix+d),D
instr 	fetch_nop,	op_RES7,	store2_E	;BB		;RES 7,(Ix+d),E
instr 	fetch_nop,	op_RES7,	store2_H	;BC		;RES 7,(Ix+d),H
instr 	fetch_nop,	op_RES7,	store2_L	;BD		;RES 7,(Ix+d),L
instr 	fetch_nop,	op_RES7,	store_nop	;BE		;RES 7,(Ix+d)
instr 	fetch_nop,	op_RES7,	store2_A	;BF		;RES 7,(Ix+d),A
instr 	fetch_nop,	op_SET0,	store2_B	;C0		;SET 0,(Ix+d),B
instr 	fetch_nop,	op_SET0,	store2_C	;C1		;SET 0,(Ix+d),C
instr 	fetch_nop,	op_SET0,	store2_D	;C2		;SET 0,(Ix+d),D
instr 	fetch_nop,	op_SET0,	store2_E	;C3		;SET 0,(Ix+d),E
instr 	fetch_nop,	op_SET0,	store2_H	;C4		;SET 0,(Ix+d),H
instr 	fetch_nop,	op_SET0,	store2_L	;C5		;SET 0,(Ix+d),L
instr 	fetch_nop,	op_SET0,	store_nop	;C6		;SET 0,(Ix+d)
instr 	fetch_nop,	op_SET0,	store2_A	;C7		;SET 0,(Ix+d),A
instr 	fetch_nop,	op_SET1,	store2_B	;C8		;SET 1,(Ix+d),B
instr 	fetch_nop,	op_SET1,	store2_C	;C9		;SET 1,(Ix+d),C
instr 	fetch_nop,	op_SET1,	store2_D	;CA		;SET 1,(Ix+d),D
instr 	fetch_nop,	op_SET1,	store2_E	;CB		;SET 1,(Ix+d),E
instr 	fetch_nop,	op_SET1,	store2_H	;CC		;SET 1,(Ix+d),H
instr 	fetch_nop,	op_SET1,	store2_L	;CD		;SET 1,(Ix+d),L
instr 	fetch_nop,	op_SET1,	store_nop	;CE		;SET 1,(Ix+d)
instr 	fetch_nop,	op_SET1,	store2_A	;CF		;SET 1,(Ix+d),A
instr 	fetch_nop,	op_SET2,	store2_B	;D0		;SET 2,(Ix+d),B
instr 	fetch_nop,	op_SET2,	store2_C	;D1		;SET 2,(Ix+d),C
instr 	fetch_nop,	op_SET2,	store2_D	;D2		;SET 2,(Ix+d),D
instr 	fetch_nop,	op_SET2,	store2_E	;D3		;SET 2,(Ix+d),E
instr 	fetch_nop,	op_SET2,	store2_H	;D4		;SET 2,(Ix+d),H
instr 	fetch_nop,	op_SET2,	store2_L	;D5		;SET 2,(Ix+d),L
instr 	fetch_nop,	op_SET2,	store_nop	;D6		;SET 2,(Ix+d)
instr 	fetch_nop,	op_SET2,	store2_A	;D7		;SET 2,(Ix+d),A
instr 	fetch_nop,	op_SET3,	store2_B	;D8		;SET 3,(Ix+d),B
instr 	fetch_nop,	op_SET3,	store2_C	;D9		;SET 3,(Ix+d),C
instr 	fetch_nop,	op_SET3,	store2_D	;DA		;SET 3,(Ix+d),D
instr 	fetch_nop,	op_SET3,	store2_E	;DB		;SET 3,(Ix+d),E
instr 	fetch_nop,	op_SET3,	store2_H	;DC		;SET 3,(Ix+d),H
instr 	fetch_nop,	op_SET3,	store2_L	;DD		;SET 3,(Ix+d),L
instr 	fetch_nop,	op_SET3,	store_nop	;DE		;SET 3,(Ix+d)
instr 	fetch_nop,	op_SET3,	store2_A	;DF		;SET 3,(Ix+d),A
instr 	fetch_nop,	op_SET4,	store2_B	;E0		;SET 4,(Ix+d),B
instr 	fetch_nop,	op_SET4,	store2_C	;E1		;SET 4,(Ix+d),C
instr 	fetch_nop,	op_SET4,	store2_D	;E2		;SET 4,(Ix+d),D
instr 	fetch_nop,	op_SET4,	store2_E	;E3		;SET 4,(Ix+d),E
instr 	fetch_nop,	op_SET4,	store2_H	;E4		;SET 4,(Ix+d),H
instr 	fetch_nop,	op_SET4,	store2_L	;E5		;SET 4,(Ix+d),L
instr 	fetch_nop,	op_SET4,	store_nop	;E6		;SET 4,(Ix+d)
instr 	fetch_nop,	op_SET4,	store2_A	;E7		;SET 4,(Ix+d),A
instr 	fetch_nop,	op_SET5,	store2_B	;E8		;SET 5,(Ix+d),B
instr 	fetch_nop,	op_SET5,	store2_C	;E9		;SET 5,(Ix+d),C
instr 	fetch_nop,	op_SET5,	store2_D	;EA		;SET 5,(Ix+d),D
instr 	fetch_nop,	op_SET5,	store2_E	;EB		;SET 5,(Ix+d),E
instr 	fetch_nop,	op_SET5,	store2_H	;EC		;SET 5,(Ix+d),H
instr 	fetch_nop,	op_SET5,	store2_L	;ED		;SET 5,(Ix+d),L
instr 	fetch_nop,	op_SET5,	store_nop	;EE		;SET 5,(Ix+d)
instr 	fetch_nop,	op_SET5,	store2_A	;EF		;SET 5,(Ix+d),A
instr 	fetch_nop,	op_SET6,	store2_B	;F0		;SET 6,(Ix+d),B
instr 	fetch_nop,	op_SET6,	store2_C	;F1		;SET 6,(Ix+d),C
instr 	fetch_nop,	op_SET6,	store2_D	;F2		;SET 6,(Ix+d),D
instr 	fetch_nop,	op_SET6,	store2_E	;F3		;SET 6,(Ix+d),E
instr 	fetch_nop,	op_SET6,	store2_H	;F4		;SET 6,(Ix+d),H
instr 	fetch_nop,	op_SET6,	store2_L	;F5		;SET 6,(Ix+d),L
instr 	fetch_nop,	op_SET6,	store_nop	;F6		;SET 6,(Ix+d)
instr 	fetch_nop,	op_SET6,	store2_A	;F7		;SET 6,(Ix+d),A
instr 	fetch_nop,	op_SET7,	store2_B	;F8		;SET 7,(Ix+d),B
instr 	fetch_nop,	op_SET7,	store2_C	;F9		;SET 7,(Ix+d),C
instr 	fetch_nop,	op_SET7,	store2_D	;FA		;SET 7,(Ix+d),D
instr 	fetch_nop,	op_SET7,	store2_E	;FB		;SET 7,(Ix+d),E
instr 	fetch_nop,	op_SET7,	store2_H	;FC		;SET 7,(Ix+d),H
instr 	fetch_nop,	op_SET7,	store2_L	;FD		;SET 7,(Ix+d),L
instr 	fetch_nop,	op_SET7,	store_nop	;FE		;SET 7,(Ix+d)
instr 	fetch_nop,	op_SET7,	store2_A	;FF		;SET 7,(Ix+d),A

.macro m_do_fetch_0
	ldi	opl,0
.endm
.equ do_fetch_0 = 0
;	ldi	opl,0
;	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|IN r,[C]  |***P0-|Input                |r=[C]                 |
;

do_op_in:			; in opl,(opl)
.if PORT_DEBUG
	push	opl	
	cp	opl,_0		; don't debug port 0 (con stat)
	breq	dbg_op_in_1
	printnewline
	printstring "Port read: ("
	mov temp,opl
	lcall printhex
	printstring ") -> "
dbg_op_in_1:
.endif

	mov	temp2,opl
	lcall	portRead
	mov	opl,temp
	bst	z_flags,ZFL_C			;save Carry
	ldpmx	z_flags,sz53p_tab,temp		;S,Z,P
	bld	z_flags,ZFL_C

.if PORT_DEBUG
	pop	temp
	cp	temp,_0
	breq	dbg_op_in_2
	lcall printhex
	printstring " "
dbg_op_in_2:
.endif
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|OUT [C],r |------|Output               |[C]=r                 |
;

do_op_out: 			; out (c),opl
.if PORT_DEBUG
	printnewline
	printstring "Port write: "
	mov temp,opl
	lcall printhex
	printstring " -> ("
	mov	temp,z_c
	lcall printhex
	printstring ") "
.endif
	mov	temp,opl
	mov	temp2,z_c
	lcall	portWrite
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|LD dst,src|------|Load                 |dst=src               |
;

do_op_stbc: 		;store bc to mem loc in opl:h
	movw xl,opl
	mem_write_s z_c
	adiw xl,1
	mem_write_s z_b
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|LD dst,src|------|Load                 |dst=src               |
;
;
do_op_stde: 		;store de to mem loc in opl:h
	movw xl,opl
	mem_write_s z_e
	adiw xl,1
	mem_write_s z_d
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|LD dst,src|------|Load                 |dst=src               |
;
;
do_op_stsp: 		;store sp to mem loc in opl:h
	movw xl,opl
	mem_write_s z_spl
	adiw xl,1
	mem_write_s z_sph
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|ADC HL,ss |***V0*|Add with Carry       |HL=HL+ss+CY           |
;

do_op_ADCHL:
	lsr	z_flags				; ZFL_C --> Carry
	ldi	z_flags,0			; clear N
	adc	z_l,opl
	in	temp,sreg			; save lower Z 
	adc	z_h,oph
	in	temp2,sreg

	and	temp,temp2			; 16bit Z
	bmov	z_flags,ZFL_C, temp2,AVR_C
	bmov	z_flags,ZFL_P, temp2,AVR_V
	bmov	z_flags,ZFL_H, temp2,AVR_H
	bmov	z_flags,ZFL_Z, temp,AVR_Z
	bmov	z_flags,ZFL_S, temp2,AVR_N
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|SBC HL,ss |***V1*|Subtract with carry  |HL=HL-ss-CY           |
;

	checkspace PC, 24

do_op_sbchl:
	lsr	z_flags				; get Z80 carry
	sez					; set z
	sbc	z_l,opl
	sbc	z_h,oph
	in temp,sreg
	ldi	z_flags,(1<<ZFL_N)		; set N
	bmov	z_flags,ZFL_C, temp,AVR_C
	bmov	z_flags,ZFL_P, temp,AVR_V
	bmov	z_flags,ZFL_H, temp,AVR_H
	bmov	z_flags,ZFL_Z, temp,AVR_Z
	bmov	z_flags,ZFL_S, temp,AVR_N
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|NEG       |***V1*|Negate A             |A=0-A                 |

;
do_op_NEG:
	ldi	temp,0
	sub 	temp,z_a
	mov	z_a,temp
	in 	temp,sreg
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,P
	bmov	z_flags,ZFL_C, temp,AVR_C
	bmov	z_flags,ZFL_H, temp,AVR_H
	do_z80_flags_V
	do_z80_flags_set_N
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|RETI      |------|Return from Interrupt|PC=[SP]+              |
;|RETN      |------|Return from NMI      |  Copy IFF2 to IFF1   |


do_op_RETI:
do_op_RETN:
	ldd	temp,y+oz_istat
	bmov	temp,IFF1, temp,IFF2
	std	y+oz_istat,temp
	ljmp	do_store_ret


;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|IM n      |------|Interrupt Mode       |             (n=0,1,2)|

do_op_IM0:
	ldd	temp,y+oz_istat
	andi	temp, ~IM_MASK
	std	y+oz_istat,temp
	ret

do_op_IM1:
	ldd	temp,y+oz_istat
	andi	temp,~IM_MASK
	ori	temp,IM1
	std	y+oz_istat,temp
	ret

do_op_IM2:
	ldd	temp,y+oz_istat
	andi	temp, ~IM_MASK
	ori	temp,IM2
	std	y+oz_istat,temp
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|LD A,i    |**0*0-|Load                 |(i=I,R)  IFF2 --> P   |
;|LD i,A    |------|Load                 |(i=I,R)               |

do_op_ldai:
	ldd	z_a,y+oz_i
	rjmp	op_ldar1

do_op_ldar:
	ldd	z_a,y+oz_r
op_ldar1:
	bst	z_flags,ZFL_C			;save C
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,H,P,N	
	bld	z_flags,ZFL_C			;
	ldd	temp,y+oz_istat
	bmov	z_flags,ZFL_P, temp,IFF2
	ret

do_op_ldia:
	std	y+oz_i,z_a
	ret

do_op_ldra:
	std	y+oz_r,z_a
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|RLD       |**0P0-|Rotate Left 4 bits   |{A,[HL]}={A,[HL]}<- ##|
;|RRD       |**0P0-|Rotate Right 4 bits  |{A,[HL]}=->{A,[HL]} ##|

do_op_rld:
	swap	opl
	mov	oph,opl
	andi	opl,0xf0
	andi	oph,0x0f
	mov	temp,z_a
	andi	temp,0x0f
	or	opl,temp
	mov	temp,z_a
	andi	temp,0xf0
	or	temp,oph
	mov	z_a,temp
	bst	z_flags,ZFL_C			;save C
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,H,P,N	
	bld	z_flags,ZFL_C			;
	ret

do_op_rrd:
	mov	oph,opl
	andi	opl,0xf0
	andi	oph,0x0f
	mov	temp,z_a
	andi	temp,0x0f
	or	opl,temp
	swap	opl
	mov	temp,z_a
	andi	temp,0xf0
	or	temp,oph
	mov	z_a,temp
	bst	z_flags,ZFL_C			;save C
	ldpmx	z_flags,sz53p_tab,z_a		;S,Z,H,P,N	
	bld	z_flags,ZFL_C			;
	ret


;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|LDD       |--0*0-|Load and Decrement   |[DE]=[HL],HL=HL-1,#   |
;|LDDR      |--000-|Load, Dec., Repeat   |LDD till BC=0         |
;|LDI       |--0*0-|Load and Increment   |[DE]=[HL],HL=HL+1,#   |
;|LDIR      |--000-|Load, Inc., Repeat   |LDI till BC=0         |
;

	checkspace PC, 13

op_LDxx_common:
;	movw	x,z_l			;
;	lcall	dram_read		; temp = (HL)
	mem_read_ds temp, z_hl
;	movw	x,z_e			;
;	lcall	dram_write		; (DE) = temp
	mem_write_ds z_de, temp

	cbr	z_flags,(1<<ZFL_H) | (1<<ZFL_P) | (1<<ZFL_N)

	movw	x,z_c
	sbiw	x,1			;BC--
	movw	z_c,x
	breq	PC+2
	 sbr	z_flags,(1<<ZFL_P)
	ret

	checkspace PC, 6

do_op_LDI:
	rcall	op_LDxx_common
	sub	z_e,_255		;-low(-1)	DE++
	sbc	z_d,_255		;-high(-1)
	sub	z_l,_255		;-low(-1)	HL++
	sbc	z_h,_255		;-high(-1)
	ret

	checkspace PC, 6

do_op_LDD:
	rcall	op_LDxx_common
	add	z_e,_255		;+low(-1)	DE--
	adc	z_d,_255		;+high(-1)
	add	z_l,_255		;+low(-1)	HL--
	adc	z_h,_255		;+high(-1)
	ret

	checkspace PC, 5

do_op_LDIR:
	rcall	do_op_LDI
#if 1
	sbrc	z_flags,ZFL_P
	 rjmp	do_op_LDIR
	ret
#else
	sbrs	z_flags,ZFL_P
	 ret
	sbiw	z_pcl,2
	ret
#endif

	checkspace PC, 5

do_op_LDDR:
	rcall	do_op_LDD
#if 1
	sbrc	z_flags,ZFL_P
	 rjmp	do_op_LDDR
	ret
#else
	sbrs	z_flags,ZFL_P
	 ret
	sbiw	z_pcl,2
	ret
#endif

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|CPD       |****1-|Compare and Decrement|A-[HL],HL=HL-1,BC=BC-1|
;|CPDR      |****1-|Compare, Dec., Repeat|CPD till A=[HL]or BC=0|
;|CPI       |****1-|Compare and Increment|A-[HL],HL=HL+1,BC=BC-1|
;|CPIR      |****1-|Compare, Inc., Repeat|CPI till A=[HL]or BC=0|


	checkspace PC, 21

op_CPxx_common:
	movw	x,z_l			; HL
	
	movw	z,z_c			;BC

	cbr	z_flags,(1<<ZFL_S)|(1<<ZFL_Z)|(1<<ZFL_H)|(1<<ZFL_P)
	sbr	z_flags,(1<<ZFL_N)
	lcall	dram_read		; temp = (HL)

	cp	z_a,temp		; A - (HL)

	brpl	PC+2
	 sbr	z_flags,(1<<ZFL_S)
	brne	PC+2
	 sbr	z_flags,(1<<ZFL_Z)
	brhc	PC+2
	 sbr	z_flags,(1<<ZFL_H)

	sbiw	z,1			; BC--
	breq	PC+2
	 sbr	z_flags,(1<<ZFL_P)
	movw	z_c,z			;BC
	ret

	checkspace PC, 5

do_op_CPI:
	rcall	op_CPxx_common
	adiw	x,1			; HL++
	movw	z_l,x			; HL
	ret


	checkspace PC, 5

do_op_CPD:
	rcall	op_CPxx_common
	sbiw	x,1			; HL--
	movw	z_l,x			; HL
	ret

	checkspace PC, 7

do_op_CPIR:
	rcall	do_op_CPI
	sbrc	z_flags,ZFL_Z
	 ret
	sbrs	z_flags,ZFL_P
	 ret
	sbiw	z_pcl,2
	ret

	checkspace PC, 7

do_op_CPDR:
	rcall	do_op_CPD
	sbrc	z_flags,ZFL_Z
	 ret
	sbrs	z_flags,ZFL_P
	 ret
	sbiw	z_pcl,2
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|INI       |?*??1-|Input and Increment  |[HL]=[C],HL=HL+1,B=B-1|
;|IND       |?*??1-|Input and Decrement  |[HL]=[C],HL=HL-1,B=B-1|
;|INIR      |?1??1-|Input, Inc., Repeat  |INI till B=0          |
;|INDR      |?1??1-|Input, Dec., Repeat  |IND till B=0          |

	checkspace PC, 12

op_INxx_common:
	cbr	z_flags,(1<<ZFL_Z)
	sbr	z_flags,(1<<ZFL_N)
	mov	temp2,z_c		;C
	lcall	portRead
	movw	x,z_l			;HL
	lcall	dram_write
	dec	z_b			;B
	brne	PC+2
	 sbr	z_flags,(1<<ZFL_Z)
	ret

	checkspace PC, 4

do_op_INI:
	rcall	op_INxx_common
	adiw	x,1
	movw	z_l,x			;HL
	ret

	checkspace PC, 4

do_op_IND:
	rcall	op_INxx_common
	sbiw	x,1
	movw	z_l,x			;HL
	ret

	checkspace PC, 5

do_op_INIR:
	rcall	do_op_INI
	sbrc	z_flags,ZFL_Z
	 ret
	sbiw	z_pcl,2
	ret

	checkspace PC, 5

do_op_INDR:
	rcall	do_op_IND
	sbrc	z_flags,ZFL_Z
	 ret
	sbiw	z_pcl,2
	ret

;----------------------------------------------------------------
;|Mnemonic  |SZHPNC|Description          |Notes                 |
;----------------------------------------------------------------
;|OUTI      |?*??1-|Output and Increment |[C]=[HL],HL=HL+1,B=B-1|
;|OUTD      |?*??1-|Output and Decrement |[C]=[HL],HL=HL-1,B=B-1|
;|OTIR      |?1??1-|Output, Inc., Repeat |OUTI till B=0         |
;|OTDR      |?1??1-|Output, Dec., Repeat |OUTD till B=0         |

	checkspace PC, 12

op_OUTxx_common:
	movw	x,z_l			;HL
	lcall	dram_read		;temp = (z)
	mov	temp2,z_c		;C
	lcall	portWrite
	cbr	z_flags,(1<<ZFL_Z)
	sbr	z_flags,(1<<ZFL_N)
	dec	z_b			;B
	brne	PC+2
	 sbr	z_flags,(1<<ZFL_Z)
	ret

	checkspace PC, 4

do_op_OUTI:
	rcall	op_OUTxx_common
	sub	z_l,_255		;-low(-1)
	sbc	z_h,_255		;-high(-1)
	ret

	checkspace PC, 4

do_op_OUTD:
	rcall	op_OUTxx_common
	add	z_l,_255		;+low(-1)
	adc	z_h,_255		;+high(-1)
	ret

	checkspace PC, 5

do_op_OTIR:
	rcall	do_op_OUTI
	sbrc	z_flags,ZFL_Z
	 ret
	sbiw	z_pcl,2
	ret

	checkspace PC, 5

do_op_OTDR:
	rcall	do_op_OUTD
	sbrc	z_flags,ZFL_Z
	 ret
	sbiw	z_pcl,2
	ret

#if 1
	opctable EDjmp, PC 	;+ 2*256

instr 	fetch_nop,	op_nop,		store_nop	;00		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;01		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;02		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;03		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;04		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;05		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;06		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;07		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;08		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;09		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;0A		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;0B		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;0C		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;0D		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;0E		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;0F		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;10		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;11		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;12		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;13		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;14		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;15		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;16		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;17		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;18		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;19		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;1A		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;1B		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;1C		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;1D		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;1E		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;1F		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;20		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;21		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;22		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;23		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;24		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;25		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;26		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;27		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;28		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;29		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;2A		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;2B		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;2C		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;2D		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;2E		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;2F		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;30		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;31		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;32		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;33		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;34		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;35		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;36		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;37		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;38		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;39		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;3A		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;3B		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;3C		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;3D		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;3E		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;3F		;NOP
instr 	fetch_C,	op_IN,		store2_B	;40		;IN B,(C)
instr 	fetch_B,	op_OUT,		store_nop	;41		;OUT (C),B
instr 	fetch_BC,	op_SBCHL,	store_nop	;42		;SBC HL,BC
instr 	fetch_DIR16,	op_STBC,	store_nop	;43		;LD (nn),BC
instr 	fetch_nop,	op_NEG,		store_nop	;44		;NEG
instr 	fetch_nop,	op_RETN,	store_nop	;45		;RETN
instr 	fetch_nop,	op_IM0,		store_nop	;46		;IM 0
instr 	fetch_nop,	op_ldia,	store_nop	;47		;LD I,A
instr 	fetch_C,	op_IN,		store2_C	;48		;IN C,(C)
instr 	fetch_C,	op_OUT,		store_nop	;49		;OUT (C),C
instr 	fetch_BC,	op_ADCHL,	store_nop	;4A		;ADC HL,BC
instr 	fetch_DIR16,	op_RMEM16,	store_BC	;4B nn nn	;LD BC,(nn)
instr 	fetch_nop,	op_NEG,		store_nop	;4C		;NEG
instr 	fetch_nop,	op_RETI,	store_nop	;4D		;RETI
instr 	fetch_nop,	op_IM0,		store_nop	;4E		;IM 0
instr 	fetch_nop,	op_ldra,	store_nop	;4F		;LD R,A
instr 	fetch_C,	op_IN,		store2_D	;50		;IN D,(C)
instr 	fetch_D,	op_OUT,		store_nop	;51		;OUT (C),D
instr 	fetch_DE,	op_SBCHL,	store_nop	;52		;SBC HL,DE
instr 	fetch_DIR16,	op_STDE,	store_nop	;53 nn nn	;LD (nn),DE
instr 	fetch_nop,	op_NEG,		store_nop	;54		;NEG
instr 	fetch_nop,	op_RETN,	store_nop	;55		;RETN
instr 	fetch_nop,	op_IM1,		store_nop	;56		;IM 1
instr 	fetch_nop,	op_ldai,	store_nop	;57		;LD A,I
instr 	fetch_C,	op_IN,		store2_E	;58		;IN E,(C)
instr 	fetch_E,	op_OUT,		store_nop	;59		;OUT (C),E
instr 	fetch_DE,	op_ADCHL,	store_nop	;5A		;ADC HL,DE
instr 	fetch_DIR16,	op_RMEM16,	store_DE	;5B nn nn	;LD DE,(nn)
instr 	fetch_nop,	op_NEG,		store_nop	;5C		;NEG
instr 	fetch_nop,	op_RETN,	store_nop	;5D		;RETN
instr 	fetch_nop,	op_IM2,		store_nop	;5E		;IM 2
instr 	fetch_nop,	op_ldar,	store_nop	;5F		;LD A,R
instr 	fetch_C,	op_IN,		store2_H	;60		;IN H,(C)
instr 	fetch_H,	op_OUT,		store_nop	;61		;OUT (C),H
instr 	fetch_HL,	op_SBCHL,	store_nop	;62		;SBC HL,HL
instr 	fetch_DIR16,	op_STHL,	store_nop	;63 nn nn	;LD (nn),HL
instr 	fetch_nop,	op_NEG,		store_nop	;64		;NEG
instr 	fetch_nop,	op_RETN,	store_nop	;65		;RETN
instr 	fetch_nop,	op_IM0,		store_nop	;66		;IM 0
instr 	fetch2_mhl,	op_RRD,		store_mhl	;67		;RRD
instr 	fetch_C,	op_IN,		store2_L	;68		;IN L,(C)
instr 	fetch_L,	op_OUT,		store_nop	;69		;OUT (C),L
instr 	fetch_HL,	op_ADCHL,	store_nop	;6A		;ADC HL,HL
instr 	fetch_DIR16,	op_RMEM16,	store_HL	;6B nn nn	;LD HL,(nn)
instr 	fetch_nop,	op_NEG,		store_nop	;6C		;NEG
instr 	fetch_nop,	op_RETN,	store_nop	;6D		;RETN
instr 	fetch_nop,	op_IM0,		store_nop	;6E		;IM 0
instr 	fetch2_mhl,	op_RLD,		store_mhl	;6F		;RLD
instr 	fetch_C,	op_IN,		store_nop	;70		;IN (C)
instr 	fetch_0,	op_OUT,		store_nop	;71		;OUT (C),0
instr 	fetch_SP,	op_SBCHL,	store_nop	;72		;SBC HL,SP
instr 	fetch_DIR16,	op_STSP,	store_nop	;73 nn nn	;LD (nn),SP
instr 	fetch_nop,	op_NEG,		store_nop	;74		;NEG
instr 	fetch_nop,	op_RETN,	store_nop	;75		;RETN
instr 	fetch_nop,	op_IM1,		store_nop	;76		;IM 1
instr 	fetch_nop,	op_nop,		store_nop	;77		;NOP
instr 	fetch_C,	op_IN,		store2_A	;78		;IN A,(C)
instr 	fetch_A,	op_OUT,		store_nop	;79		;OUT (C),A
instr 	fetch_SP,	op_ADCHL,	store_nop	;7A		;ADC HL,SP
instr 	fetch_DIR16,	op_RMEM16,	store_SP	;7B nn nn	;LD SP,(nn)
instr 	fetch_nop,	op_NEG,		store_nop	;7C		;NEG
instr 	fetch_nop,	op_RETN,	store_nop	;7D		;RETN
instr 	fetch_nop,	op_IM2,		store_nop	;7E		;IM 2
instr 	fetch_nop,	op_nop,		store_nop	;7F		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;80		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;81		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;82		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;83		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;84		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;85		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;86		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;87		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;88		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;89		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;8A		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;8B		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;8C		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;8D		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;8E		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;8F		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;90		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;91		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;92		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;93		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;94		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;95		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;96		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;97		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;98		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;99		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;9A		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;9B		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;9C		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;9D		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;9E		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;9F		;NOP
instr 	fetch_nop,	op_LDI,		store_nop	;A0		;LDI
instr 	fetch_nop,	op_CPI,		store_nop	;A1		;CPI
instr 	fetch_nop,	op_INI,		store_nop	;A2		;INI
instr 	fetch_nop,	op_OUTI,	store_nop	;A3		;OUTI
instr 	fetch_nop,	op_nop,		store_nop	;A4		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;A5		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;A6		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;A7		;NOP
instr 	fetch_nop,	op_LDD,		store_nop	;A8		;LDD
instr 	fetch_nop,	op_CPD,		store_nop	;A9		;CPD
instr 	fetch_nop,	op_IND,		store_nop	;AA		;IND
instr 	fetch_nop,	op_OUTD,	store_nop	;AB		;OUTD
instr 	fetch_nop,	op_nop,		store_nop	;AC		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;AD		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;AE		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;AF		;NOP
instr 	fetch_nop,	op_LDIR,	store_nop	;B0		;LDIR
instr 	fetch_nop,	op_CPIR,	store_nop	;B1		;CPIR
instr 	fetch_nop,	op_INIR,	store_nop	;B2		;INIR
instr 	fetch_nop,	op_OTIR,	store_nop	;B3		;OTIR
instr 	fetch_nop,	op_nop,		store_nop	;B4		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;B5		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;B6		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;B7		;NOP
instr 	fetch_nop,	op_LDDR,	store_nop	;B8		;LDDR
instr 	fetch_nop,	op_CPDR,	store_nop	;B9		;CPDR
instr 	fetch_nop,	op_INDR,	store_nop	;BA		;INDR
instr 	fetch_nop,	op_OTDR,	store_nop	;BB		;OTDR
instr 	fetch_nop,	op_nop,		store_nop	;BC		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;BD		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;BE		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;BF		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C0		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C1		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C2		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C3		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C4		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C5		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C6		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C7		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C8		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;C9		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;CA		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;CB		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;CC		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;CD		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;CE		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;CF		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D0		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D1		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D2		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D3		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D4		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D5		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D6		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D7		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D8		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;D9		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;DA		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;DB		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;DC		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;DD		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;DE		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;DF		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E0		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E1		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E2		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E3		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E4		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E5		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E6		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E7		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E8		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;E9		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;EA		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;EB		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;EC		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;ED		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;EE		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;EF		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F0		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F1		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F2		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F3		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F4		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F5		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F6		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F7		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F8		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;F9		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;FA		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;FB		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;FC		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;FD		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;FE		;NOP
instr 	fetch_nop,	op_nop,		store_nop	;FF		;NOP
#endif

#endif

;----------------------------------------------------------------
; Lookup table, stolen from z80ex, Z80 emulation library.
; http://z80ex.sourceforge.net/

; The S, Z, 5 and 3 bits and the parity of the lookup value 

	checkspace PC, 256

	.org (PC+255) & 0xff00
;	.org opcjmp + 256
;	.org FLASHEND & 0xff00

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

