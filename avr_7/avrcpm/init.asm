;    Hardware initialisation, disk, mmc, timer, DRAM test
;
;    Copyright (C) 2010 Sprite_tm
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
;    $Id: init.asm 241 2015-12-10 09:38:25Z rapid $
;

#define REFR_PRE    8           /* timer prescale factor  1/8 */
#define REFR_CS     0x02        /* timer clock select for 1/8 */
#define REFR_CNT    F_CPU / REFR_RATE / REFR_PRE

	.cseg
regval_tab:
	.db 0,0
	.db 0xFE,0xFC	; _RAS0  _CAS0
	.db 0xF8,0xF4	; _OE    _WE
	.db 255,0	; _255  _0
regval_tab_e:

start:
	ldi temp,low(RAMEND)	; top of memory; vim:set ts=8 noet nowrap

	out SPL,temp		; init stack pointer
	ldi temp,high(RAMEND)	; top of memory
	out SPH,temp		; init stack pointer

; - Load some registers with constant values

	ldiw	z,regval_tab*2
	ldiw	y,0
cp_l:	lpm	xh,z+
	st	y+,xh
	cpi	zl,low(regval_tab_e*2)
	brne	cp_l

; - Kill wdt

	wdr
	out MCUSR,_0

	ldi temp,(1<<WDCE) | (1<<WDE)
	outm8	WDTCSR,temp
	ldi temp,(1<<WDCE)
	outm8	WDTCSR,temp

; - Setup Ports

;	ldi 	temp,(1<<PUD)		;disable pullups
;	outm8	P_PUD,temp
	out 	PORTD,_255		;all pins high (enables pullup on input ports)
	out 	PORTB,_255
	out 	PORTC,_255
	out 	DDRD,_255		; PD all outputs
#if I2C_SUPPORT
	ldi	temp,~((1<<SCL)|(1<<SDA))
	out	DDRC,temp
#endif
#if DRAM_8BIT
	ldi	temp,~(1<<RXD)
	out	DDRB,temp
#endif

	outm8	TIMSK1,_0
	outm8	TIMSK2,_0
	outm8	TCCR2A,_0
	outm8	TCCR2B,_0

; - Clear RAM

	ldiw	z,SRAM_START
	ldi	temp2,high(ramtop)
clr_loop:
	st	z+,_0
	cpi	zl,low(ramtop)
	cpc	zh,temp2
	brne	clr_loop

; - Fill unused RAM (stack)

	ldi	temp2,high(RAMEND+1)
	ldi	temp,SRAMFILL_VAL
fill_loop:
	st	z+,temp
	cpi	zl,low(RAMEND+1)
	cpc	zh,temp2
	brne	fill_loop

; Init clock/timer system

; Init timer 1 as 1 ms system clock tick.

	ldi	temp, low (TC_1MS)
	ldi	temp2,high(TC_1MS)
	outm8	OCR1BH,temp2
	outm8	OCR1BL,temp
	ldi	temp,(1<<ICNC1)|(1<<CS10)	;Noise cancel, fall. edge, Normal Mode, clk/1
	outm8	TCCR1B,temp
	inm8	temp,TIMSK1
	ori	temp,(1<<OCIE1B)		;Enable 1ms int.
	outm8	TIMSK1,temp

; - Init serial port

	rcall	uart_init


;Init timer2. Refresh-call should happen every (8ms/512) cycles.

	ldi	temp,REFR_CNT*2			; 2 cycles per int
	outm8	OCR2A,temp
	inm8	temp,TCCR2A
	ori	temp,(1<<WGM21)			;CTC mode
	outm8	TCCR2A,temp
	inm8	temp,TCCR2B
	ori	temp,REFR_CS			;clk/REFR_PRE
	outm8	TCCR2B,temp
	inm8	temp,TIMSK2
	ori	temp, (1<<OCIE2A)
	outm8	TIMSK2,temp
	sei


#if I2C_SUPPORT
	rcall	i2c_init			; Init I2C master
	rcall	rtc_get
#endif


.if BOOTWAIT
	ldi temp,10
	rcall delay_ms

.endif

	rcall	printstr
	.db	'\r', '\r'
version_string:
	makestring "CPM on an AVR, v" VERS_STR " r" SVN_REVSTR TESTSTR

.if MEMTEST
	printnewline
	printstring "Testing RAM: fill..."

;Fill RAM
	ldiw	x,0
ramtestw:
	mov temp,xh
	eor temp,xl
	rcall	dram_write_pp
	brcc ramtestw
	printstring "wait..."

	ldi	temp2,8
ramtestwl:
	ldi	temp,255
	rcall	delay_ms
	dec	temp2
	brne	ramtestwl

	printstring "reread..."

;re-read RAM
	ldiw	x,0
	clr	temp3			;Error counter
ramtestr:
	rcall	dram_read

;	ori	temp,0x04		;simulate error
;	andi	temp,0xF7		;another error

	mov	temp2,xh
	eor	temp2,xl
	cp	temp,temp2
	breq	ramtestrok
	tst	temp3
	brne	ramtestr1
	printnewline
	printstring "Addr xx yy "
ramtestr1:
	printnewline
	mov	zl,temp
	movw	temp,x
	rcall	printhexw
	rcall	printspace
	mov	temp,xh
	eor	temp,xl
	mov	temp2,temp
	rcall	printhex
	rcall	printspace
	mov	temp,zl
	rcall	printhex
	rcall	printspace
	mov	temp,temp2
	eor	temp,zl
	and	temp,temp2
	rcall	printxbits
	rcall	printspace
	mov	temp,temp2
	eor	temp,zl
	com	temp2
	and	temp,temp2
	rcall	printxbits

	inc	temp3
	cpi	temp3,16		;
	brsh	ramtestrex
ramtestrok:
	adiw	xl,1
	brcc	ramtestr
ramtestrex:
	tst	temp3			;any errors?
	breq	ramtestend

	printstring " System halted!"
halted_loop:
	rjmp	halted_loop		;keep AVR in an endless loop

printxbits:
	push	temp2
	push	temp3
	mov	temp2,temp
	ldi	temp3,8
prntxb0:
	ldi	temp,'-'
	lsl	temp2
	brcc	prntxb1
	ldi	temp,'X'
prntxb1:
	rcall	uartPutc
	dec	temp3
	brne	prntxb0
	pop	temp3
	pop	temp2
	ret

ramtestend:

.endif

.if MEMFILL
	ldiw	x,0
	ldi	temp,MEMFILL_VAL
ramfillw:
	rcall	dram_write_pp
	brcc ramfillw
.endif


;----------------------------------------------------------------------------

boot_again:
	printnewline
	printstring "Initing mmc..."
	printnewline
	lcall	mgr_init_partitions

	cbr	temp,0x80
	brne	boot_ipl2
	printstring "No bootable CP/M disk found! Please change MMC/SD-Card."
	printnewline
	ldi	temp2,18
boot_iplwl:
	ldi	temp,255
	rcall	delay_ms
	dec	temp2
	brne	boot_iplwl
	rjmp	boot_again


boot_ipl2:
	lcall	mgr_prnt_parttbl
	printnewline
	printstring "Partinit done."

; Init (de)blocking buffer

	lcall	dsk_inval_hostbuf

; Read first sector of first CP/M partition (ipl)

	ldiw	y,fsys_vars

;	Disk 0
	std	y+o_seekdsk,_0
;	Track 0
	std	y+o_seektrk,  _0
	std	y+o_seektrk+1,_0
; 	Sector 0
	std	y+o_seeksec,_0

;	Destination
	ldiw	x,IPLADDR
	std	y+o_dmaadr+0,xl
	std	y+o_dmaadr+1,xh

	ldi	temp,1<<READ_FUNC
	lcall	dskDoIt

;	lift off
	ljmp z80_init


; vim:set ts=8 noet nowrap
