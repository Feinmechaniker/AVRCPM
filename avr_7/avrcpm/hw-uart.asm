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
;    $Id: hw-uart.asm 93 2014-01-03 16:32:32Z rapid $
;

#define UBRR_VAL  ((F_CPU+BAUD*8)/(BAUD*16)-1)  /* clever rounding */

#define RXBUFMASK  RXBUFSIZE-1
#define TXBUFMASK  TXBUFSIZE-1

	.dseg
	
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

	ldi temp, (1<<TXEN0) | (1<<RXEN0) | (1<<RXCIE0)
	outm8 UCSR0B,temp
.ifdef URSEL
	ldi temp, (1<<URSEL) | (1<<UCSZ01) | (1<<UCSZ00)
.else
	ldi temp, (1<<UCSZ01) | (1<<UCSZ00)
.endif
	outm8 UCSR0C,temp
	ldi temp, HIGH(UBRR_VAL)
	outm8 UBRR0H,temp
	ldi temp, LOW(UBRR_VAL)
	outm8 UBRR0L,temp
	ret

; Save received character in a circular buffer. Do nothing if buffer overflows.

; USART receive interrupt

	INTERRUPT URXCaddr   

	push	temp
	in	temp,sreg
	push	temp
	push	zh
	push	zl
	inm8	temp,RXTXDR0
	lds	zh,rxcount		;if rxcount < RXBUFSIZE
	cpi	zh,RXBUFSIZE		;   (room for at least 1 char?)
	brsh	rxi_ov			; 
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
rxi_ov:					;endif
	pop	zl
	pop	zh
	pop	temp
	out	sreg,temp
	pop	temp
	reti


;Fetches a char from the buffer to temp. If none available, waits till one is.

uartgetc:
	lds	temp,rxcount		; Number of characters in buffer
	tst	temp
	breq	uartgetc		;Wait for char
	
	push	zh
	push	zl
	ldi	zl,low(rxfifo)
	ldi	zh,high(rxfifo)
	lds	temp,rxidx_r
	add	zl,temp
	brcc	PC+2
	inc	zh
	inc	temp
	andi	temp,RXBUFMASK
	sts	rxidx_r,temp
	cli
	lds	temp,rxcount
	dec	temp
	sts	rxcount,temp
	sei
	ld	temp,z		;don't forget to get the char
	pop	zl
	pop	zh
	ret

; USART transmit interrupt

	INTERRUPT UDREaddr

	push	temp
	in	temp,sreg
	push	temp
	lds	temp,txcount		;if txcount != 0
	tst	temp			;
	breq	txi_e			; 

	dec	temp			;
	sts	txcount,temp		;   --txcount
	push	zh			;
	push	zl			;
	ldi	zl,low(txfifo)		;  
	ldi	zh,high(txfifo)		;
	lds	temp,txidx_r		;
	add	zl,temp			;
	brcc	PC+2			;
	inc	zh			;
	inc	temp			;
	andi	temp,TXBUFMASK		;
	sts	txidx_r,temp		;
	ld	temp,z
	outm8 RXTXDR0,temp
	pop	zl
	pop	zh
txi_e:					;endif
	lds	temp,txcount
	tst	temp
	brne	txi_x
	ldi temp, (1<<TXEN0) | (1<<RXEN0) | (1<<RXCIE0)
	outm8 UCSR0B,temp
txi_x:
	pop	temp
	out	sreg,temp
	pop	temp
	reti


;Sends a char from temp to the uart. 
uartputc:
	push	zh
	push	zl
	push	temp
putc_l:
	lds	temp,txcount		;do {
	cpi	temp,TXBUFSIZE		;
	brsh	putc_l			;} while (txcount >= TXBUFSIZE)

	ldi	zl,low(txfifo)		;  
	ldi	zh,high(txfifo)		;
	lds	temp,txidx_w		;
	add	zl,temp			;
	brcc	PC+2			;
	inc	zh			;
	inc	temp			;
	andi	temp,TXBUFMASK		;
	sts	txidx_w,temp		;   txidx_w = ++txidx_w % TXBUFSIZE
	pop	temp			;
	st	z,temp			;   txfifo[txidx_w] = char
	cli
	lds	zl,txcount
	inc	zl
	sts	txcount,zl
	ldi	zl, (1<<UDRIE0) | (1<<TXEN0) | (1<<RXEN0) | (1<<RXCIE0)
	outm8	UCSR0B,zl
	sei
	pop	zl
	pop	zh
	ret

; vim:set ts=8 noet nowrap

