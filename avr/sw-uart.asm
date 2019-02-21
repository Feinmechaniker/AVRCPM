; Serial interface using the ATmega8/88 USART. 
; This is part of the Z80-CP/M emulator written by Sprite_tm.
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
;    $Id: sw-uart.asm 93 2014-01-03 16:32:32Z rapid $
;

#ifdef  __ATmega8__
  #error "ATmega8 is not supported (yet)! Please update this driver, or buy an ATmega88."
#endif

#define SSER_BIT_TC	(F_CPU+BAUD/2) / BAUD 
#define SSER_CHAR_TC	(10 * 1000 / BAUD) + 2

#define RXBUFMASK  	RXBUFSIZE-1
#define TXBUFMASK  	TXBUFSIZE-1

	.dseg
	
srx_state:
	.byte	1
srx_char_to:
	.byte	1
srx_char_time:
	.byte	1
srx_dr:
	.byte	1
srx_lastedge:
	.byte	2
stx_bitcount:
	.byte	1
stx_dr:
	.byte	1
rxcount:
	.byte	1
rxidx_w:
	.byte	1
rxidx_r:
	.byte	1
txcount:
	.byte	1
txidx_w:
	.byte	1
txidx_r:
	.byte	1
rxfifo:
	.byte	RXBUFSIZE
txfifo:
	.byte	TXBUFSIZE


	.cseg

; Init 
uart_init:

; - Init clock/timer system and serial port

; Init timer 1 as 
; - Soft UART TX (OC1A/OCR1A).
; - Soft UART RX (ICP1/ICR1).
; - 1ms System timer is already configured at this point.


	cbi	P_TXD-1,TXD			;TXD pin as input
	ldi	temp,(1<<COM1A1)|(1<<COM1A0)	;OC1A high on compare match (UART TX)
	ldi	temp2,(1<<FOC1A)		;force compare match
	outm8	TCCR1A,temp
	outm8	TCCR1C,temp2
	sbi	P_TXD-1,TXD			;TXD pin now output (OC1A)

	ldi	temp,(1<<ICF1)			;clear pending input capture int
	out	TIFR1,temp			;
	inm8	temp,TIMSK1			;
	ori	temp,(1<<ICIE1)			;Enable input capture int.  (UART RX)
	outm8	TIMSK1,temp			;

	ldi	temp,SSER_CHAR_TC		;Character TO
	sts	srx_char_time,temp

	ret
	
;------------------------------------------------------------------

	.cseg

; Timer/Counter1 Input Capture interrupt
	
	INTERRUPT ICP1addr
	
	push	temp
	in	temp,sreg
	push	temp
	push	zh
	push	zl
	inm8	zl,ICR1L
	inm8	zh,ICR1H
	push	temp2
	ldi	temp2,(1<<ICES1)
	inm8	temp,TCCR1B
	eor	temp,temp2			;toggle edge
	outm8	TCCR1B,temp
	ldi	temp,(1<<ICF1)			;clear pending int
	out	TIFR1,temp
	
#if 0
	lds	temp,srx_state
	subi	temp,-'0'
	rcall	uartputc
	lds	temp,srx_dr
	rcall	printhex
#endif	
	lds	temp,srx_state
	cpi	temp,0
	brne	srxi_S1		

; State 0: Wait for start bit

	sts	srx_lastedge,zl			;save beginning of start bit
	sts	srx_lastedge+1,zh
;	movw	srx_lastedgel,zl
	sts	srx_dr,_0
	ldi	temp,1
	sts	srx_state,temp
	lds	temp,srx_char_time
	sts	srx_char_to,temp
	sbis	P_RXD-2,RXD			;RXD still low?
	rjmp	srxi_end
	ldi	zl,(1<<ICNC1)|(1<<CS10)		;next edge is falling edge
	outm8	TCCR1B,zl
	ldi	zh,(1<<ICF1)			;clear pending int
	out	TIFR1,zh
	sts	srx_state,_0
	sts	srx_char_to,_0
	rjmp	srxi_end

srxi_S1:
	cpi	temp,1
	brne	srxi_S2

; State 1: Check start bit (and collect 0-bits)

	lds	temp,srx_lastedge
	lds	temp2,srx_lastedge+1
	sts	srx_lastedge,zl
	sts	srx_lastedge+1,zh

;	movw	temp,srx_lastedgel
;	movw	srx_lastedgel,zl

	sub	zl,temp
	sbc	zh,temp2
	subi	zl,low ((SSER_BIT_TC+1)/2)
	sbci	zh,high((SSER_BIT_TC+1)/2)
	brcs	srxi_sberr

;	mov	temp,zh	
;	rcall	printhex
;	mov	temp,zl
;	rcall	printhex

	ldi	temp,0x80
srxi_1l:
	subi	zl,low(SSER_BIT_TC)
	sbci	zh,high(SSER_BIT_TC)
	brcs	srxi_1be
	lsr	temp
	brcc	srxi_1l

	subi	zl,low(SSER_BIT_TC)		; stop bit?
	sbci	zh,high(SSER_BIT_TC)
	brcc	srxi_1fe
	rjmp	srxi_complete0			; ok, x00 (^@) received
srxi_1fe:
	sts	srx_char_to,_0			; no stop bit --> framing error --> break
	sts	srx_state,_0
	sbr	intstat,(1<<i_break)		;
	sts	rxcount,_0			;clear rx buffer
	sts	rxidx_w,_0
	sts	rxidx_r,_0

	rjmp	srxi_end

srxi_1be:
	sts	srx_dr,temp
	ldi	temp,2
	sts	srx_state,temp
	rjmp	srxi_end

srxi_sberr:
	ldi	temp,(1<<ICNC1)|(1<<CS10)	;next edge is falling edge
	outm8	TCCR1B,temp
	ldi	temp,(1<<ICF1)		;clear pending int
	out	TIFR1,temp
	sts	srx_state,_0		;next state
#if 1
	ldi	temp,'?'
	rcall	uartputc
	subi	zl,low (-(SSER_BIT_TC+1)/2)
	sbci	zh,high(-(SSER_BIT_TC+1)/2)
	mov	temp,zh
	rcall	printhex
	mov	temp,zl
	rcall	printhex
#endif
	rjmp	srxi_end

srxi_S2:
	cpi	temp,2
	brne	srxi_S3

; State 2: collect 1-bits

	lds	temp,srx_lastedge
	lds	temp2,srx_lastedge+1
	sts	srx_lastedge,zl
	sts	srx_lastedge+1,zh

;	movw	temp,srx_lastedgel
;	movw	srx_lastedgel,zl

	sub	zl,temp
	sbc	zh,temp2
	subi	zl,low ((SSER_BIT_TC+1)/2)
	sbci	zh,high((SSER_BIT_TC+1)/2)

	lds	temp,srx_dr
srxi_2l:
	sec				;one more 1 bit
	ror	temp
	brcs	srxi_complete1		;8 bits recieved
	subi	zl,low(SSER_BIT_TC)
	sbci	zh,high(SSER_BIT_TC)
	brcc	srxi_2l
	
	sts	srx_dr,temp
	ldi	temp,3
	sts	srx_state,temp
	rjmp	srxi_end
	
srxi_complete1:
	ldi	temp2,1			;We are in start bit now.
	sts	srx_state,temp2
	lds	temp2,srx_char_time
	sts	srx_char_to,temp2
	rjmp	srxi_complete
	
srxi_S3:
	cpi	temp,3
	brne	srxi_S4

; State 3: collect 0-bits

	lds	temp,srx_lastedge
	lds	temp2,srx_lastedge+1
	sts	srx_lastedge,zl
	sts	srx_lastedge+1,zh

;	movw	temp,srx_lastedgel
;	movw	srx_lastedgel,zl

	sub	zl,temp
	sbc	zh,temp2
	subi	zl,low ((SSER_BIT_TC+1)/2)
	sbci	zh,high((SSER_BIT_TC+1)/2)

	lds	temp,srx_dr
srxi_3l:
					;one more 0 bit
	lsr	temp
	brcs	srxi_complete0		;8 bits recieved
	subi	zl,low(SSER_BIT_TC)
	sbci	zh,high(SSER_BIT_TC)
	brcc	srxi_3l
	
	sts	srx_dr,temp
	ldi	temp,2
	sts	srx_state,temp
	rjmp	srxi_end

srxi_S4:
	ldi	zl,(1<<ICNC1)|(1<<CS10)	;next edge is falling edge
	outm8	TCCR1B,zl
	ldi	zl,(1<<ICF1)		;clear pending int
	sts	srx_state,_0		;next state
	rjmp	srxi_end

srxi_complete0:	
	sts	srx_char_to,_0		;clear timeout
	sts	srx_state,_0		;next state
srxi_complete:
#if 0
	ldi	zl,(1<<ICNC1)|(1<<CS10)	;next edge is falling edge
	outm8	TCCR1B,zl
	ldi	zl,(1<<ICF1)		;clear pending int
	out	TIFR1,zl
#endif

; Save received character in a circular buffer. Do nothing if buffer overflows.

	lds	zh,rxcount			;2   if rxcount < RXBUFSIZE
	cpi	zh,RXBUFSIZE			;1      (room for at least 1 char?)
	brsh	srxi_ov				;1 
	inc	zh				;1
	sts	rxcount,zh			;2      rxcount++

	ldi	zl,low(rxfifo)			;1  
	lds	zh,rxidx_w			;2
	add	zl,zh				;1
	inc	zh				;1
	andi	zh,RXBUFMASK			;1
	sts	rxidx_w,zh			;2      rxidx_w = ++rxidx_w % RXBUFSIZE
	ldi	zh,high(rxfifo)			;1
	adc	zh,_0				;1
	st	z,temp				;2      rxfifo[rxidx_w] = char
srxi_ov:					;=19     endif

srxi_end:
	pop	temp2
	pop	zl
	pop	zh
	pop	temp
	out	sreg,temp
	pop	temp
	reti


;----------------------------------------------------------------------

	.cseg

; Timer/Counter1 Compare Match A interrupt
	
	INTERRUPT OC1Aaddr
	
	push	zl
	in	zl,sreg
	push	zl
	push	zh

	inm8	zl,OCR1AL
	inm8	zh,OCR1AH
	subi	zl,low(-SSER_BIT_TC)
	sbci	zh,high(-SSER_BIT_TC)
	outm8	OCR1AH,zh
	outm8	OCR1AL,zl

	lds	zl,stx_bitcount
	dec	zl
	brpl	stxi_nextbit
	
; bit counter was 0, more characters?

stxi_nxtchar:
	lds	zl,txcount		;if txcount != 0
	dec	zl
	brmi	stxi_dis

; get next char
	sts	txcount,zl		;   --txcount
	push	temp			;
	ldi	zl,low(txfifo)		;  
	ldi	zh,high(txfifo)		;
	lds	temp,txidx_r		;
	add	zl,temp			;
	adc	zh,_0
	inc	temp			;
	andi	temp,TXBUFMASK		;
	sts	txidx_r,temp		;
	ld	temp,z
	com	temp
	sts	stx_dr,temp
	ldi	temp,9
	sts	stx_bitcount,temp
	pop	temp

	ldi	zh,(1<<COM1A1)
	rjmp	stxi_ex

; disable transmitter
stxi_dis:
	inm8	zl,TIMSK1
	andi	zl,~(1<<OCIE1A)
	outm8	TIMSK1,zl

	ldi	zh,(1<<COM1A1)|(1<<COM1A0)
	rjmp	stxi_ex

stxi_nextbit:
	sts	stx_bitcount,zl

	ldi	zh,(1<<COM1A1)
	lds	zl,stx_dr
	sbrs	zl,0
	ldi	zh,(1<<COM1A1)|(1<<COM1A0)
	lsr	zl
	sts	stx_dr,zl
stxi_ex:
	outm8	TCCR1A,zh
	pop	zh
	pop	zl
	out	sreg,zl
	pop	zl
	reti

;------------------------------------------------------------------

srx_to:
#if 0
	ldi	zl,(1<<ICNC1)|(1<<CS10)	;next edge is falling edge
	outm8	TCCR1B,zl
	ldi	zl,(1<<ICF1)		;clear pending int
	out	TIFR1,zl
#endif
	sts	srx_state,_0		;next state
	push	temp

#if 0
	ldi	temp,'|'
	rcall	uartputc
#endif
	lds	temp,srx_dr		;only 0 if timeout after leading edge of start bit.
	tst	temp			; --> break
	brne	srxto_store
	sbr	intstat,(1<<i_break)
	sts	rxcount,_0		;clear rx buffer
	sts	rxidx_w,_0
	sts	rxidx_r,_0
	rjmp	srxto_ov

srxto_store:
	mov	zl,temp
	com	zl
	andi	zl,0x80
srxto_l:
	lsr	temp
	or	temp,zl
	brcc	srxto_l
	
; Save received character in a circular buffer. Do nothing if buffer overflows.

	lds	zh,rxcount		;if rxcount < RXBUFSIZE
	cpi	zh,RXBUFSIZE		;   (room for at least 1 char?)
	brsh	srxto_ov			; 
	inc	zh			;
	sts	rxcount,zh		;   rxcount++

	ldi	zl,low(rxfifo)		;  
	lds	zh,rxidx_w		;
	add	zl,zh			;
	inc	zh			;
	andi	zh,RXBUFMASK		;
	sts	rxidx_w,zh		;   rxidx_w = ++rxidx_w % RXBUFSIZE
	ldi	zh,high(rxfifo)		;
	brcc	PC+2			;
	inc	zh			;
	st	z,temp			;   rxfifo[rxidx_w] = char
srxto_ov:					;endif
	
	pop	temp
	ret
	
	
;Fetches a char from the buffer to temp. If none available, waits till one is.

uartgetc:
	push	zh
	push	zl
	push	temp2
ugetc_w:
	lds	temp,rxcount		;Number of characters in buffer
	tst	temp
	breq	ugetc_w			;Wait for char
	
	ldi	zl,low(rxfifo)
	ldi	zh,high(rxfifo)
	lds	temp2,rxidx_r
	add	zl,temp2
	adc	zh,_0
	inc	temp2
	andi	temp2,RXBUFMASK
	cli
	lds	temp,rxcount
	subi	temp,1
	brcc	ugetc_fin
	sei
	rjmp	ugetc_w

ugetc_fin:
	sts	rxcount,temp
	sts	rxidx_r,temp2
	sei
	ld	temp,z		;don't forget to get the char
	pop	temp2
	pop	zl
	pop	zh
	ret

;Sends a char from temp to the soft uart. 

uartputc:
	push	zh
	push	zl
	in	zl,sreg
	push	zl
	push	temp
sputc_l:
	lds	temp,txcount		;do {
	cpi	temp,TXBUFSIZE		;
	brsh	sputc_l			;} while (txcount >= TXBUFSIZE)

	cli
	ldi	zl,low(txfifo)		;  
	ldi	zh,high(txfifo)		;
	lds	temp,txidx_w		;
	add	zl,temp			;
	adc	zh,_0			;
	inc	temp			;
	andi	temp,TXBUFMASK		;
	sts	txidx_w,temp		;   txidx_w = ++txidx_w % TXBUFSIZE
	pop	temp			;
	st	z,temp			;   txfifo[txidx_w] = char
;	cli
	lds	zh,txcount
	inc	zh
	sts	txcount,zh
	cpi	zh,1
	brne	sputc_e
; Enable transmitter
	inm8	zh,TIMSK1
	sbrc	zh,OCIE1A
	rjmp	sputc_e
	ori	zh,(1<<OCIE1A)
	outm8	TIMSK1,zh

	inm8	zl,TCNT1L		;
	inm8	zh,TCNT1H		;
	adiw	zl,30			;
	outm8	OCR1AH,zh		;
	outm8	OCR1AL,zl		;
	ldi	zl,(1<<OCF1A)
	outm8	TIFR1,zl

sputc_e:
	pop	zl
	out	sreg,zl
	pop	zl
	pop	zh
	ret


; Wait, till tx buffer is empty.

uart_wait_empty:
	push	temp
uwe_loop:
	lds	temp,txcount
	tst	temp
	brne	uwe_loop
	pop	temp
	ret


; vim:set ts=8 noet nowrap

