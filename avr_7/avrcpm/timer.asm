;    Timer module
;
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
;    $Id: timer.asm 153 2014-11-12 12:59:42Z rapid $
;

	.dseg

timer_var:
delay_timer1:
.equ ot_timer1	= delay_timer1-timer_var
	.byte	1
delay_timer2:
.equ ot_timer2	= delay_timer2-timer_var
	.byte	1
timer_base:
timer_ms:
.equ ot_ms	= timer_ms-timer_var
	.byte	2
timer_sec:
.equ ot_sec	= timer_sec-timer_var
	.byte	4

; don't change order here, clock put/get depends on it.
cntms_out:		; register for ms
	.byte	2
utime_io:		; register for uptime. 
	.byte	4	
cnt_1ms:
.equ ot_1ms	= cnt_1ms-timer_var
	.byte	2
uptime:
.equ ot_uptime	= uptime-timer_var
	.byte	4
timer_top:
.equ timer_size	= timer_top - timer_base
	
.equ utofs	= cnt_1ms-cntms_out
.equ timerofs	= cnt_1ms-timer_ms
 
clk_out:
.equ oclk_out	= clk_out-timer_var
	.byte	7	;
clock:
.equ o_clock	= clock-timer_var
	.byte	7	;Format (bin): s m h D M YY
.equ clkofs	= clock-clk_out

	.cseg	

; ------------- system timer 1ms ---------------


; Timer/Counter1 Compare Match B interrupt
	
	INTERRUPT OC1Baddr

.if TIMER_DEBUG
	cbi	PORTC,5
.endif	
	push    zl
	in      zl,SREG
	push    zl
	push	zh
	inm8	zl,OCR1BL
	inm8	zh,OCR1BH
	addiw	z,TC_1MS
	outm8	OCR1BH,zh
	outm8	OCR1BL,zl
	
	push	yl
	push	yh
	ldiw	y,timer_var

#if DRAM_8BIT	/* Implies software uart */
	lds	zl,srx_char_to			;try to decrement character timout
	subi	zl,1
	brcs	syscl0				;timer was 0 before (not running)
	sts	srx_char_to,zl			;timer is running, store new value
	brne	syscl0				
	rcall	srx_to
syscl0:
#endif
	ldd	zl,y+ot_timer1
	subi	zl,1
	brcs	syscl_t1n
	std	y+ot_timer1,zl
syscl_t1n:	
	ldd	zl,y+ot_timer2
	subi	zl,1
	brcs	syscl_t2n
	std	y+ot_timer2,zl
syscl_t2n:

	ldd     zl,y+ot_1ms			;count milli seconds
	ldd     zh,y+ot_1ms+1
	adiw	z,1
	std	y+ot_1ms,zl
	std	y+ot_1ms+1,zh
	cpi	zl,low(1000)			;one second ?
	ldi	zl,high(1000)			;doesn't change flags
	cpc	zh,zl
	brlt	syscl_end
;	brge	syscl_utime
;	rjmp	syscl_end

syscl_utime:
	std	y+ot_1ms,_0
	std	y+ot_1ms+1,_0

	ldd	zl,y+ot_uptime+0
	ldd	zh,y+ot_uptime+1
	adiw	z,1
	std	y+ot_uptime+0,zl
	std	y+ot_uptime+1,zh
	brne	syscl_clk
	ldd	zl,y+ot_uptime+2
	ldd	zh,y+ot_uptime+3
	adiw	z,1
	std	y+ot_uptime+2,zl
	std	y+ot_uptime+3,zh

syscl_clk:
	ldd	zl,y+o_clock+0		;sec
	inc	zl
	std	y+o_clock+0,zl
	cpi	zl,60
	brlo	syscl_end
	std	y+o_clock+0,_0
	ldd	zl,y+o_clock+1		;min
	inc	zl
	std	y+o_clock+1,zl
	cpi	zl,60
	brlo	syscl_end
	std	y+o_clock+1,_0
	ldd	zl,y+o_clock+2		;hour
	inc	zl
	std	y+o_clock+2,zl
	cpi	zl,24
	brlo	syscl_end

syscl_clk_date:
	std	y+o_clock+2,_0
	push	temp

	ldiw	z,dayspm_tab*2 - 1
	ldd	temp,y+o_clock+4		;month
	add	zl,temp
	adc	zh,_0
	lpm	zh,z			;days this month
	cpi	temp,2
	brne	syscl_clknl		;february, may be leap year
	ldd	zl,y+o_clock+5		;year
	andi	zl,0x03
	brne	syscl_clknl
	inc	zh			;leap year 
syscl_clknl:
	ldd	zl,y+o_clock+3		;day
	inc	zl
	std	y+o_clock+3,zl
	cp	zh,zl
	brsh	syscl_clke
	ldi	zl,1
	std	y+o_clock+3,zl
	inc	temp			;month
	std	y+o_clock+4,temp
	cpi	temp,13
	brlo	syscl_clke

	std	y+o_clock+4,zl
	ldd	zl,y+o_clock+5		;low year
	ldd	zh,y+o_clock+6		;high year
	adiw	z,1
	std	y+o_clock+5,zl
	std	y+o_clock+6,zh

syscl_clke:
	pop	temp
syscl_end:
	pop	yh
	pop     yl
	pop	zh
	pop     zl
	out     SREG,zl
	pop     zl
.if TIMER_DEBUG
	sbi	PORTC,5
.endif	
	reti

; days per month

dayspm_tab:
	.db	31,28,31,30,31,30
	.db	31,31,30,31,30,31

; ----------------------------------------------
; 	delay
;
; wait for temp ms
;

delay_ms:
	sts	delay_timer1,temp
dly_loop:
	lds	temp,delay_timer1
	cpi	temp,0
	brne	dly_loop
	ret

; ----------------------------------------------
; 
clockget:
	ldiw	z,clk_out
	tst	temp3
	breq	clkget_copy		;lowest byte requestet, latch clock
	
	add	zl,temp3
	adc	zh,_0
	ld	temp,z
clkget_end:
	ret
	

clkget_copy:
	ldi	temp3,7
	cli
clkget_l:
	ldd	temp,z+clkofs
	st	z+,temp
	dec	temp3
	brne	clkget_l
	sei

	ld	temp2,-z
	ld	temp, -z
	rcall	binbcd4
	std	z+0,temp
	std	z+1,temp2
	ldi	temp3,5
clkget_l2:
	ld	temp,-z	
	rcall	binbcd2
	st	z,temp
	dec	temp3
	brne	clkget_l2

	lds	temp,clk_out
	ret				;req. byte in temp

; ----------------------------------------------
; 
clockput:
	ldiw	z,clk_out
	add	zl,temp3
	adc	zh,_0
	st	z,temp
	tst	temp3
	breq	clkput_copy		;lowest byte stored, latch clock
clkput_end:
	ret
		

clkput_copy:
	ldi	temp3,5
clkput_l2:
	ld	temp,z	
	rcall	bcdbin2
	st	z+,temp
	dec	temp3
	brne	clkput_l2

	ldd	temp,z+0
	ldd	temp2,z+1
	rcall	bcdbin4
	st	z+,temp
	st	z+,temp2

	ldi	temp3,7
	cli
clkput_l:
	ld	temp,-z
	std	z+clkofs,temp
	dec	temp3
	brne	clkput_l
	sei
#if I2C_SUPPORT
	rcall	rtc_set			; set hardware clock
#endif
	ret


; ----------------------------------------------

; convert binary to bcd
; (only range 0 - 99)

binbcd2:
	push	temp2
	ldi	temp2,10
	mov	_tmp0,_255
tobcd_l:
	inc	_tmp0
	sub	temp,temp2
	brcc	tobcd_l
	add	temp,temp2
	swap	_tmp0
	add	temp,_tmp0
	pop	temp2
	ret
	

binbcd4:
	ldi	temp3,16
	movw	_tmp0,temp
	clr	temp
	clr	temp2
binbcd4l:
	lsl	_tmp0
	rol	_tmp1
	rol	temp
	rol	temp2
	dec	temp3
	breq	binbcd4e

	subi	temp2,-0x03
	sbrs	temp2,3
	subi	temp2,0x03
	subi	temp2,-0x30
	sbrs	temp2,7
	subi	temp2,0x30
	subi	temp,-0x03
	sbrs	temp,3
	subi	temp,0x03
	subi	temp,-0x30
	sbrs	temp,7
	subi	temp,0x30
	rjmp	binbcd4l

binbcd4e:
	ret

; convert bcd to binary

bcdbin2:
	push	temp2
	mov	temp2,temp		;temp2 = high digit
	swap	temp2
	andi	temp2,0x0f
	andi	temp,0x0f		;temp  = low digit
	mov	r0,temp2
	ldi	temp2,10
	mul	temp2,r0		;high digit * 10
	add	temp,r0			;high digit * 10 + low digit
	pop	temp2
	ret

bcdbin4:
	rcall	bcdbin2
	push	temp
	mov	temp,temp2
	rcall	bcdbin2
	ldi	temp2,100
	mul	temp,temp2
	pop	temp
	mov	temp2,r1
	add	temp,r0
	adc	temp2,_0
	ret


#if I2C_SUPPORT

; ----------------------------------------------
; Set software clock from hardware clock

rtc_get:
	push	_0			;Placeholder for month/weekday
	push	_0			;day/year
	push	_0			;hours
	push	_0			;minutes
	ldi	temp,0x10		;register address
	push	temp			;save reg adr/placeholder for sec
	in	zh,sph
	in	zl,spl
	ldi	temp,0xA0		;PCF8583 slave address
	push	temp

	ldi	temp2,2
	rcall	i2c_write
	ldi	temp2,3
	rcall	i2c_read		;get year (stored in RTC-RAM addr. 10h)
	andi	temp,0x3
	breq	rtc_get_e		;i2c error

	ldd	temp3,z+1		;save year
	ldd	xl,   z+2

	ldi	temp2,2			;register pointer. 2 = secs
	std	z+1,temp2
	rcall	i2c_write
	ldi	temp2,6
	rcall	i2c_read
	andi	temp,0x3
	breq	rtc_get_e		;i2c error

	mov	temp2,xl		;year century
	ldd	temp,z+4		;get year
	rol	temp
	rol	temp
	rol	temp
	eor	temp,temp3
	andi	temp,0x03
	breq	rtc_get_1
	subi    temp3, low(-1)  
	sbci    temp2, high(-1)
rtc_get_1:
	ldiw	x,clock
	cli

	ldd	temp,z+1		;get seconds
	rcall	bcdbin2
	st	x+,temp
	ldd	temp,z+2		;get minutes
	rcall	bcdbin2
	st	x+,temp
	ldd	temp,z+3		;get hours
	rcall	bcdbin2
	st	x+,temp
	ldd	temp,z+4		;get day
	andi	temp,0x3f
	rcall	bcdbin2
	st	x+,temp
	ldd	temp,z+5		;get months
	andi	temp,0x1f
	rcall	bcdbin2
	st	x+,temp			;store month
	st	x+,temp3		;store year
	st	x+,temp2		;store year century
	sei

rtc_get_e:
	pop	temp
	pop	temp
	pop	temp
	pop	temp
	pop	temp
	pop	temp
	ret

;----------------------------------------------
; Set hardware clock from software clock
; 
; Register:	temp2:	s
;		temp3:	m
;		xh:	h
;		xl:	D
;		temp:	M
;		yl:	Yl
;		yh:	Yh

rtc_set:
	ldiw	z,clock
	cli
	ldd	temp2,z+0		;sec
	ldd	temp3,z+1		;min
	ldd	xh,z+2			;hours
	ldd	xl,z+3			;day
	ldd	temp,z+4		;month
	ldd	yl,z+5			;yearl
	ldd	yh,z+6			;yearh
	sei
	rcall	binbcd2
	push	temp			;-1 save month
	mov	temp,xl
	rcall	binbcd2
	mov	xl,yl			;   least significant 2 bits of year
	swap	xl
	lsl	xl
	lsl	xl
	andi	xl,0xc0
	or	temp,xl			;   combine with day
	push	temp			;-2 save year/day
	mov	temp,xh
	rcall	binbcd2
	push	temp			;-3 save hours
	mov	temp,temp3
	rcall	binbcd2
	push	temp			;-4 save min
	mov	temp,temp2
	rcall	binbcd2
	push	temp			;-5 save sec

	push	_0			;-6 1/10s, 1/100s
	ldi	temp,0x84		;   stop count, alarm enable
	push	temp			;-7
	push	_0			;-8 register address
	in	zh,sph
	in	zl,spl
	ldi	temp,0xA0		;   PCF8583 slave address
	push	temp			;(-9)

	ldi	temp2,9
	rcall	i2c_write
	ldi	temp,0x04		;enable counting
	std	z+2,temp
	ldi	temp2,3
	rcall	i2c_write
	std	z+3,yh			;store year in RTC RAM
	std	z+2,yl
	ldi	temp,0x10
	std	z+1,temp
	ldi	temp2,4
	rcall	i2c_write

	addiw	z,8			;remove buffer from stack
	cli
	out	spl,zl
	sei
	out	sph,zh

	ret
#endif /* I2C_SUPPORT */

; ----------------------------------------------
; 

utimeget:
	ldi	temp,0xFF
	ldiw	z,cntms_out
	subi	temp3,1
	brcs	utimget_end		;Rel. port number = 0 ? (controlport)
	breq	utimget_copy		;lowest byte requestet, latch clock
	
	add	zl,temp3
	adc	zh,_0
	ld	temp,z
utimget_end:
	ret
	
utimget_copy:
	ldi	temp2,6
	cli
utimget_l:
	ldd	temp,z+utofs
	st	z+,temp
	dec	temp2
	brne	utimget_l
	sei
	lds	temp,cntms_out
					;req. byte in temp
	ret

utimeput:
	subi	temp3,1
	brcc	utput__1
	
	; clock control

	cpi	temp,starttimercmd
	breq	timer_start		;timer_ms
	cpi	temp,quitTimerCmd
	breq	utput_quit		;
	cpi	temp,printTimerCmd
	breq	timer_print		;timer_ms
	cpi	temp,uptimeCmd
	brne	utcp_ex
	rjmp	uptime_print		;cnt_1ms
utcp_ex:
	ret	
	
utput_quit:
	rcall	timer_print
	rjmp	timer_start

utput__1:
	ldiw	z,cntms_out
	breq	utput__copy		;lowest byte requestet, latch clock
	
	add	zl,temp3
	adc	zh,_0
	st	z,temp
	ret
		
utput__copy:
	st	z,temp
	adiw	z,5
	ldi	temp2,6
	cli
utput__l:
	ldd	temp,z+utofs
	st	z+,temp
	dec	temp2
	brne	utput__l
	sei
	ret

; start/reset timer
;
timer_start:
	ldiw	z,timer_ms
	ldi	temp2,6
	cli
ts_loop:
	ldd	temp,z+timerofs
	st	z+,temp
	dec	temp2
	brne	ts_loop
	sei
	ret


; print timer
;
	
timer_print:
	push	r15		;
	push	r14		;
	push	yh
	push	yl
	ldiw	z,timer_ms

; put ms on stack (16 bit)

	cli
	ldd	yl,z+timerofs
	ld	temp2,z+
	sub	yl,temp2
	ldd	yh,z+timerofs
	ld	temp2,z+
	sbc	yh,temp2
	brsh	tp_s
	
	addiw	y,1000
	sec	
tp_s:
	push	yh
	push	yl

	ldd	temp,z+timerofs
	ld	yl,z+
	sbc	temp,yl

	ldd	temp2,z+timerofs
	ld	yh,z+
	sbc	temp2,yh

	ldd	r14,z+timerofs
	ld	yl,z+
	sbc	r14,yl

	sei
	ldd	r15,z+timerofs
	ld	yh,z+
	sbc	r15,yh
	
	printnewline
	printstring "Timer running. Elapsed: "
	rcall	print_ultoa

	printstring "."
	pop	temp
	pop	temp2
	clr	r14
	clr	r15
	rcall	print_ultoa
	printstring "s."

	pop	yl
	pop	yh
	pop	r14
	pop	r15
	ret
	
uptime_print:
	push	r15
	push	r14
	ldiw	z,cnt_1ms
	cli
	ld	temp,z+
	push	temp
	ld	temp,z+
	push	temp
	
	ld	temp,z+
	ld	temp2,z+
	ld	r14,z+
	sei
	ld	r15,z+
	
	printnewline
	printstring "Uptime: "
	
	rcall	print_ultoa
	printstring ","

	clr	r14
	clr	r15
	pop	temp2
	pop	temp
	rcall print_ultoa
	printstring "s."

	pop	r14
	pop	r15
	ret

; vim:set ts=8 noet nowrap

