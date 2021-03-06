;    Virtual Ports for the BIOS Interaction
;
;    Copyright (C) 2010-2013 Leo C.
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
;    $Id$
;


;
;   Port        Direction  Function
;hex	dez
;-------------------------------------------------------------------------
; 00  	0 	in	- Con status. (deprecated)
;			  Returns 0xFF if the UART has a byte, 0 otherwise.
; 01  	1 	in/out	- Console input, aka UDR. / Console Output
; 02  	2 	out	- Console Output (deprecated)
; 03  	3	in	- "UART" status: bit 0 = rx (UARTRXRDY)
;			                 bit 1 = tx (UARTTXRDY)
; 03	3	out	- "UART" control (tbd)
; 04  	4	in/out	- "UART" data register, no wait
;
;------------------------ Virtual I2C interface --------------------------
; 05	5	out	- Control Port: 1 = Start read operation
;					2 = Start write operation
;					3 = Write 1 byte subaddress, then read
; 05	5	in	- Status of last Transfer
; 06	6	in/out	- Number of bytes to transfer, including Slave address
; 07, 08	7,8	in/out	- Read/Write address low/high
;
;------------------------ Debugging --------------------------------------
; 09		out	- MEM dump: Number of bytes to print
; 0A, 0B	10, 11	in/out	- MEM dump: Start address
;
;------------------------ Version Information ----------------------------
; 0C	12	out	- 1 = Read VMAJOR
;			  2 = Read VMINOR
;			  4 = Read Version String
;
;------------------------ Disk I/O ---------------------------------------
; 0D, 0E	13, 14	in/out	- Set address of Bios Controll Block
; 0F  	15	in/out	- Disk select
; 10, 11  16, 17 	in/out	- Track select
; 12, 13 	18, 19 	in/out	- Sector select
; 14, 15 	20, 21 	in/out	- Write addr
;
; 16  	22 	out	- Trigger disk i/o operations
;			  Bit 7 = 1: Read sector
;			  Bit 6 = 1: Write sector
;			  Bit 5 = 1: BIOS WBOOT
;			  Bit 4 = 1: BIOS Home
;			  Only one of bits 4..7  may be set.
;			  If Write function (bit 6=1):
;			   Bits 0..2: 0 - write to allocated
;				      1 - write to directory
;				      2 - write unallocated
;				      3 - write to directory
;
; 16  	22	in	- Result of last read/write operation.
;			  0x00 = ok, 0xff = error (--> Bad Sector)
;
;
;------------------------ ADC Interface ----------------------------------
; 17-19	23,25	in	- ADC Channels 6,7 and 8 (Temp-Sensor)
;				ADC 6,7 only Devices in 32 pin Case (TQFP/MLF)
;				8 Bit only
;				Fixed ADC clock (FCPU/128, 156KHz at 20MHz CPU)
;				Vref = VCC
; 20, 21	32, 33	in	- ADC: Measure VCC
;
;------------------------ Wall Clock and Timers --------------------------
; 40  	64-71	in/out	- Timer/Clock control.
; 41-46
;
; 47-4D		in/out	- clock in BCD format: ss, mm, hh,  DD, MM, YYl, YYh
;
;------------------------ Debugging --------------------------------------
; 4F		out	- Debug: start/stop trace, print stack, ...
;
;------------------------ ISC16IS740 UART (16 ports) --------------------------------
; 50	RHR	in	  Receive Holding
; 50	THR 	out	  Transmit Holding
; 51	IER 	in/out	  Interrupt Enable
; 52	IIR	in	  Interrupt Identification
; 52	FCR	out	  FIFO Control
; 53	LCR 	in/out	  Line Control
; 54	MCR 	in/out	  Modem Control
; 55	LSR 	in	  Line Status
; 56	MSR 	in	  Modem Status
; 57	SPR 	in/out	  Scratchpad
; 56	TCR 	in/out	  Transmission Control
; 57	TLR	in/out	  Trigger Level
; 58	TXLVL	in	  Transmit FIFO Level
; 59	RXLVL	in	  Receive FIFO Level
; 5F	EFCR	in/out	  Extra Features
; -- extra registers (access requires LCR[7]=1)
; 50 	DLL	in/out	  divisor latch LSB
; 51 	DLH	in/out	  divisor latch MSB
; -- enhanced registers (access requires LCR=0xBF)
; 52 	EFR	in/out	  Enhanced Feature
; 54 	XON1 	in/out	  Xon1 word
; 55 	XON2  	in/out	  Xon2 word
; 56 	XOFF1 	in/out	  Xoff1 word
; 57 	XOFF2 	in/out	  Xoff2 word
;
;------------------------ MCP23017 GPIO (22 ports) --------------------------------
; 60 	IODIRA	== I2C_MCP23017_PORT
; 61 	IODIRB	
; 62 	IPOLA	
; 63 	IPOLB	
; 62 	GPINTENA	
; 62 	GPINTENB	
; 66 	DEFVALA	
; 67 	DEFVALB	
; 68 	INTCONA	
; 69 	INTCONB	
; 6A 	IOCON	
; 6B 	IOCON	
; 6C 	GPPUA	
; 6D 	GPPUB	
; 6E 	INTFA	
; 6F 	INTFB	
; 70 	INTCAPA	
; 71 	INTCAPB	
; 72 	GPIOA	
; 73 	GPIOB	
; 74 	OLATA	
; 75 	OLATB	

;------------------------ PCF8574 (8 Ports) ------------------------------------------
;80-87		in/out	- Port-Expander PCF8574 (max. 8 Chips)
;88-8F		in/out	- Port-Expander PCF8574A (not implemented yet!)


; ---------------------------------------------- Start of Code Segment

	.cseg
vport_tbl:
	.db	00,1		;Port 0, length, 1 deprecated
	.dw	conStatus	;	in
	.dw	dbgOut		;	out

	.db	UARTDR,1	;Port UARTDR, length 1
	.dw	uartgetc	;	in
	.dw	uartputc	;	out

;	.db	02,1		;Port 2 (old console output)
;	.dw	uartgetc	; filler
;	.dw	uartputc	; deprecated

	.db	UARTCSR,1
	.dw	uartstat
	.dw	vport_out_dummy

	.db	04,1
	.dw	uartin
	.dw	uartout

	.db	13,9		; Port 13-21, (length 9)
	.dw	dsk_param_get
	.dw	dsk_param_set
	.db	22,1
	.dw	dskErrorRet
	.dw	dskDoIt

	.db	TIMERPORT,7
	.dw	utimeget
	.dw	utimeput

	.db	CLOCKPORT,7	;Clock format (bcd): ss, mm, hh,  DD, MM, YYl, YYh
	.dw	clockget
	.dw	clockput

#if I2C_SUPPORT
	.db	I2CCTRL,1
	.dw	vi2c_stat_get
	.dw	vi2c_ctrl

	.db	I2CBLEN,3	;
	.dw	vi2c_param_get
	.dw	vi2c_param_set

#if I2C_UART_SUPPORT
	.db	I2C_SC16IS740_PORT,I2C_SC16IS740_REGS
	.dw	SC16IS740_in
	.dw	SC16IS740_out
#endif

/* -MH- */
#if I2C_PCF8574_SUPPORT
	.db	I2C_PCF8574_PORT,I2C_PCF8574_REGS
	.dw	pcf8574_in
	.dw	pcf8574_out
#endif

; -MH-
#if I2C_MCP23017_SUPPORT
	.db	I2C_MCP23017_PORT,I2C_MCP23017_REGS   ; 22 byte registers
	.dw	MCP23017_in
	.dw	MCP23017_out
#endif
; -MH-

; -MH-
#endif ; I2C_SUPPORT

#if ADC_SUPPORT
	.db	ADC80,3		;2 Channels ADC80 ADC81 + Temp Sensor
;	.dw	adc_read8
	.dw	vport_out_dummy

	.db	0x20,2		;
;	.dw	adc_readvcc
	.dw	vport_out_dummy
#endif
	.db	DEBUGPORT,1
	.dw	dbg_stat
	.dw	dbg_ctrl

.if MEMDUMP_DEBUG
	.db	MEMDUMPPORT,3
	.dw	dbg_dump_rd
	.dw	dbg_dump
.endif
	.db	0x0C,1
	.dw	version_get
	.dw	version_ctrl

	.db	0,0		; Stop mark

;---------------------------------------------------------------------

;Called with port in temp2 and value in temp.
portWrite:
	set
	rjmp	vprw_start

;Called with port in temp2. Should return value in temp.
portRead:
	clt

vprw_start:
	push	yh
	push	yl
.if PORT_DEBUG > 1
	tst	temp2
	brne	dvp_1		;don't debug console status
	brts	dvp_1
	rjmp	conStatus
dvp_1:
	printnewline
	brts	dvp_11
	printstring	"Port In:  "
	rjmp	dvp_12
dvp_11:
	printstring	"Port Out: "
dvp_12:
	push	temp
	mov	temp,temp2
	rcall	printhex
	pop	temp
.endif
	ldiw	z,vport_tbl*2

vprw_loop:
	lpm	_tmp0,z+
	lpm	_tmp1,z+	;length
	cp	_tmp1,_0
	breq	vprw_exit	;no more ports

	mov	temp3,temp2
	sub	temp3,_tmp0	;base port
	brcs	vprw_next	;port # too high
	cp	temp3,_tmp1     ;may be in range
	brcs	vprw_found	;
vprw_next:			;port # not in range, test next block.
	adiw	z,4
	rjmp	vprw_loop
vprw_found:
	brtc	PC+2		;read or write?
	adiw	z,2		;skip read function pointer
	lpm	_tmp0,z+
	lpm	zh,z
	mov	zl,_tmp0

.if PORT_DEBUG > 1
	push	temp2
	push	temp
	printstring ", exec: "
	movw	temp,z
	rcall	printhexw
	printstring ", rel port: "
	mov	temp,temp3
	rcall	printhex
	pop	temp
	pop	temp2
	printstring ", val: "
	brts	dvp_2
	icall
	rcall	printhex
	printstring " "
	pop	yl
	pop	yh
	ret
dvp_2:
	rcall	printhex
	printstring " "
				; relative port # in temp3
	icall
	pop	yl
	pop	yh
	ret
.else
	icall
	pop	yl
	pop	yh
	ret
.endif

vprw_exit:
				; trap for nonexistent port?
.if PORT_DEBUG > 1
	printstring ", not found!"
.endif
	ldi	temp,0xff
	pop	yl
	pop	yh
	ret

vport_out_dummy:
	ret

;---------------------------------------------------------------------

uartstat:
	clr	temp
	lds	temp2,rxcount
	cpse	temp2,_0
	 sbr	temp,UARTRXRDY
	lds	temp2,txcount
	cpi	temp2,TXBUFSIZE
	breq	uartst_1
	 sbr	temp,UARTTXRDY
uartst_1:
	ret

uartin:
	clr	temp
	lds	temp2,rxcount
	cpse	temp2,_0
	 ljmp	uartgetc
	ret

uartout:
	lds	temp2,txcount
	cpi	temp2,TXBUFSIZE
	breq	uartout_1
	ljmp uartputc
uartout_1:
	ret


conStatus:
	lds	temp,rxcount
	cpse	temp,_0
	 ldi	temp,0xff
	ret


;---------------------------------------------------------------------

dbgOut:
	printnewline
	printstring "Debug: "
	lcall printhex
	ret

dbg_stat:
	ldi	temp,0
	ret

dbg_ctrl:
	bmov	intstat,i_trace, temp,0
.if SRAM_FILL
	sbrc	temp,1
	lcall	stackusage_print
.endif
	ret

;---------------------------------------------------------------------

.if MEMDUMP_DEBUG

	.dseg

dbg_dump_addr:
	.byte	2


	.cseg

dbg_dump_rd:
	cpse	temp3,_0
	rjmp	dbg_dump_rdad
	ldi	temp,0
	ret

dbg_dump_rdad:
	dec	temp3
	brne	dbg_dump_rdad1
	lds	temp,dbg_dump_addr+0
	ret
dbg_dump_rdad1:
	lds	temp,dbg_dump_addr+1
	ret


dbg_dump:
	cpse	temp3,_0
	rjmp	dbg_dump_store

	mov	temp3,temp
	ldsw	z,dbg_dump_addr

	tst	temp3
	breq	dbg_dumpl_1
dbg_dumpl:
	cpi	temp3,16
	brlo	dbg_dump_u16
dbg_dumpl_1:
	lcall	dbg_hexdump_line
	subi	temp3,16
	adiw	z,16
	rjmp	dbg_dumpl

dbg_dump_u16:
	tst	temp3
	breq	dbg_dump_e
	mov	temp2,temp3
	lcall	dbg_hexdump
dbg_dump_e:
	ret

dbg_dump_store:
	dec	temp3
	brne	dbg_dump_st1
	sts	dbg_dump_addr+0,temp
	ret
dbg_dump_st1:
	sts	dbg_dump_addr+1,temp
	ret

.endif

;---------------------------------------------------------------------

	.dseg
vers_cmd:
	.byte	1
vers_pstr:
	.byte	2

	.cseg
version_ctrl:
	sts	vers_cmd,temp
	cpi	temp,4
	brne	vc_e

	ldiw	z,version_string*2
	stsw	vers_pstr,z
vc_e:
	ret

version_get:
	lds	temp2,vers_cmd
	ldi	temp,0
	cpi	temp2,0
	breq	vc_g_e
	ldi	temp,VMAJOR
	cpi	temp2,1
	breq	vc_g_e
	ldi	temp,VMINOR
	cpi	temp2,2
	breq	vc_g_e
	ldi	temp,0xFF
	cpi	temp2,4
	brne	vc_g_e

	ldsw	z,vers_pstr
	lpm	temp,z+
	tst	temp
	breq	vc_g_e
	stsw	vers_pstr,z

vc_g_e:
	ret


;---------------------------------------------------------------------
; vim:set ts=8 noet nowrap
