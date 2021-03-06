; I2C (TWI) master interface.
; This is part of the Z80-CP/M emulator written by Sprite_tm.
;
;    Copyright (C) 2013 Leo C.
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

#if I2C_SUPPORT


/* General TWI Master status codes */
#define TWI_START                  0x08  /* START has been transmitted */
#define TWI_REP_START              0x10  /* Repeated START has been transmitted */
#define TWI_ARB_LOST               0x38  /* Arbitration lost */

/* TWI Master Transmitter status codes */
#define TWI_MTX_ADR_ACK            0x18  /* SLA+W has been transmitted and ACK received */
#define TWI_MTX_ADR_NACK           0x20  /* SLA+W has been transmitted and NACK received */
#define TWI_MTX_DATA_ACK           0x28  /* Data byte has been transmitted and ACK received */
#define TWI_MTX_DATA_NACK          0x30  /* Data byte has been transmitted and NACK received */

/* TWI Master Receiver status codes */
#define TWI_MRX_ADR_ACK            0x40  /* SLA+R has been transmitted and ACK received */
#define TWI_MRX_ADR_NACK           0x48  /* SLA+R has been transmitted and NACK received */
#define TWI_MRX_DATA_ACK           0x50  /* Data byte has been received and ACK transmitted */
#define TWI_MRX_DATA_NACK          0x58  /* Data byte has been received and NACK transmitted */

/* TWI Miscellaneous status codes */
#define TWI_NO_STATE               0xF8  /* No relevant state information available */
#define TWI_BUS_ERROR              0x00  /* Bus error due to an illegal START or STOP condition */


#define I2C_BR ((F_CPU / (2 * I2C_CLOCK)) - 8) /* I2C Bit Rate */

;----------------------------------------------------------------------
;
; TWINT: TWI Interrupt Flag
; TWEA:  TWI Enable Acknowledge Bit
; TWSTA: TWI START Condition Bit
; TWSTO: TWI STOP Condition Bit
; TWEN:  TWI Enable Bit
; TWIE:  TWI Interrupt Enable
;
; 	(1<<TWEN)|(1<<TWIE)|(1<<TWINT)
; 	(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|           (1<<TWEA)
; 	(1<<TWEN)|(1<<TWIE)|(1<<TWINT)
;
; default:
; 	(1<<TWEN)|          (1<<TWINT)|           (1<<TWSTO)
;
; Init:
; 	(1<<TWEN)
;
; start read/write:
; 	(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
; 	(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
; 	(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
; 	(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
;
; wait ready:
; 	 (1<<TWIE)|(1<<TWSTO)
;
;----------------------------------------------------------------------
;
;i2c_result
;
;	0b10000000	Busy (Transmission in progress)
;	0b01000000	Timeout
;	0b00010000	Read after Write
;	0b00001000	Start transmitted
;	0b00000100	Slave acknowledged address
;	0b00000010	Data byte(s) transmitted/received
;	0b00000001	Transmission completed
;
;
;----------------------------------------------------------------------

	.dseg

i2c_var:
i2ci_idx:
	.byte	1
i2c_result:
	.byte	1
i2c_txcnt:
	.byte	1
i2c_rxcnt:
	.byte	1
i2c_buf:
	.byte	I2C_BUFSIZE

	.equ oi2ci_idx   = 0
	.equ oi2c_result = 1
	.equ oi2c_txcnt  = 2
	.equ oi2c_rxcnt  = 3
	.equ oi2c_buf    = 4

;------------------------------------------------------------------

	.cseg


	INTERRUPT TWIaddr

	push	temp
	in	temp,sreg
	push	temp
	inm8	temp,TWSR
.if I2C_STATE_DEBUG
	push	temp
.endif
	push	temp2
	push	temp3
	push	zh
	push	zl

	ldiw	z,i2c_var
	ldd	temp2,z+oi2ci_idx
	ldd	temp3,z+oi2c_result

	cpi	temp,TWI_START
	breq	i2ci_START
	cpi	temp,TWI_REP_START
	breq	i2ci_REP_START
	cpi	temp,TWI_MTX_ADR_ACK
	breq	i2ci_MTX_ADR_ACK
	cpi	temp,TWI_MTX_DATA_ACK
	breq	i2ci_MTX_DATA_ACK
	cpi	temp,TWI_MTX_DATA_NACK
	breq	i2ci_MTX_DATA_NACK
	cpi	temp,TWI_MRX_ADR_ACK
	breq	i2ci_MRX_ADR_ACK
	cpi	temp,TWI_MRX_DATA_ACK
	breq	i2ci_MRX_DATA_ACK
	cpi	temp,TWI_MRX_DATA_NACK
	breq	i2ci_MRX_DATA_NACK

	rjmp	i2ci_default

i2ci_REP_START:					;Repeated START has been transmitted
	cbr	temp3,0b00010000
i2ci_START:					;START has been transmitted
	clr	temp2					;reset buffer index
	ori	temp3,0b10001000
	rjmp	i2ci_11
i2ci_MTX_ADR_ACK:				;SLA+W has been transmitted and ACK received
	ori	temp3,0b00000100
	rjmp	i2ci_11
i2ci_MTX_DATA_ACK:				;Data byte has been transmitted and ACK received
	ori	temp3,0b00000010
i2ci_11:
	ldd	temp,z+oi2c_txcnt
	cp	temp2,temp				;all bytes transmited?
	brsh	i2ci_12					; yes
	add	zl,temp2
	adc	zh,_0
	inc	temp2
	ldd	temp,z+oi2c_buf				;next byte
	outm8	TWDR,temp
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)
	rjmp	i2ci_end

i2ci_MTX_DATA_NACK:				;Data byte has been transmitted and NACK received
i2ci_12:
	ori	temp3,0b00000001			;tx complete
	sbrs	temp3,4					;Read after Write?
	rjmp	i2ci_default				;  no, stop transceiver

	lds	temp,i2c_buf
	sbr	temp,0x01				;<SLA+R>
	sts	i2c_buf,temp
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
	rjmp	i2ci_end

i2ci_MRX_ADR_ACK:				;SLA+R has been transmitted and ACK received
	ori	temp3,0b00000100
	rjmp	i2ci_31

i2ci_MRX_DATA_ACK:				;Data byte has been received and ACK transmitted
	ori	temp3,0b00000010
	add	zl,temp2
	adc	zh,_0
	inc	temp2
	inm8	temp,TWDR
	std	z+oi2c_buf,temp
i2ci_31:
	lds	temp,i2c_rxcnt
	dec	temp
	cp	temp2,temp
	brsh	i2ci_32
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWEA)
	rjmp	i2ci_end
i2ci_32:
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)
	rjmp	i2ci_end

i2ci_MRX_DATA_NACK:				;Data byte has been received and NACK transmitted
	ori	temp3,0b00000011			;rx complete
	add	zl,temp2
	adc	zh,_0
	inc	temp2
	inm8	temp,TWDR
	std	z+oi2c_buf,temp
;	fall thru

i2ci_default:
	andi	temp3,~0b10000000
	ldi	temp,(1<<TWEN)|(0<<TWIE)|(1<<TWINT)|(1<<TWSTO)

i2ci_end:
	outm8	TWCR,temp
	sts	i2c_result,temp3
	sts	i2ci_idx,temp2
	pop	zl
	pop	zh
	pop	temp3
	pop	temp2

.if I2C_STATE_DEBUG
	ldi	temp,'|'
	sei
	rcall	uartputc
	pop	temp
	rcall	printhex
.endif

	pop	temp
	out	sreg,temp
	pop	temp
	reti

;------------------------------------------------------------------

i2c_init:
	outm8	TWCR,_0				;Disable TWI, disable TWI interrupt.
						;(Reset TWI hardware state machine.)
	ldi	temp,(5 * TC_1US+3)/3		;1  Delay 5 us
i2c_iwl:					;
	dec	temp				;1
	brne	i2c_iwl				;2

	ldi	temp,I2C_BR
	outm8	TWBR,temp
	outm8	TWDR,_255			;
	ldi	temp,(1<<TWEN)			;Enable TWI, disable TWI interrupt.
	outm8	TWCR,temp

;	sts	i2c_result,_0

	ret

;------------------------------------------------------------------

i2c_waitready:

	ldi	temp,30
	sts	delay_timer1,temp
i2c_wrl:
	inm8	temp,TWCR
	andi	temp,(1<<TWIE)|(1<<TWSTO)
	breq	i2c_wre
	lds	temp,delay_timer1
	tst	temp
	brne	i2c_wrl

	rcall	i2c_init

	ldi	temp,0b01000000

i2c_wre:
	lds	_tmp0,i2c_result
	or	temp,_tmp0
	sts	i2c_result,temp
	ret

;------------------------------------------------------------------
;
; 	z:	Pointer to the data to write.
;		First byte is slave address
;	temp2:	Number of bytes to write including address byte.
;

i2c_write:

	rcall	i2c_waitready
	cpi	temp,0b01000000
	brsh	i2c_we
	push	zh
	push	zl
	push	yh
	push	yl

	ldiw	y,i2c_var
	ldi	temp,0b10000000
	std	y+oi2c_result,temp		;result = busy
	std	y+oi2c_txcnt,temp2		;store size
	adiw	y,oi2c_buf
	ld	temp,z+				;get SLA
	cbr	temp,0x01			;
i2c_wl:
	st	y+,temp
	dec	temp2
	breq	i2c_wle
	ld	temp,z+
	rjmp	i2c_wl
i2c_wle:
	; Enable TWI, TWI int and initiate start condition
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
	outm8	TWCR,temp

	pop	yl
	pop	yh
	pop	zl
	pop	zh
i2c_we:
	ret


;------------------------------------------------------------------
;
; 	z:	Pointer to data buffer.
;		First byte of buffer is slave address
;	temp2:	Buffer len. (Number of bytes to read + address byte.)
;
;	temp:	return (fail < 0, else succsess)

i2c_read:
	rcall	i2c_waitready
	cpi	temp,0b01000000
	brsh	i2c_re

	push	zh
	push	zl
	push	yh
	push	yl
	ldiw	y,i2c_var
	ldi	temp,0b10000000
	std	y+oi2c_result,temp		;result = busy
	std	y+oi2c_rxcnt,temp2		;store size
	ld	temp,z
	sbr	temp,0x01			;<SLA+R>
	std	y+oi2c_buf,temp

	; Enable TWI, TWI int and initiate start condition
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
	outm8	TWCR,temp

	rcall	i2c_waitready

	sbrs	temp,1				;at least 1 byte received
	rjmp	i2c_ex				;
	ldd	temp2,y+oi2ci_idx
	adiw	y,oi2c_buf
i2c_rl:
	ld	_tmp0,y+
	st	z+,_tmp0
	dec	temp2
	brne	i2c_rl

i2c_ex:
	pop	yl
	pop	yh
	pop	zl
	pop	zh
i2c_re:
	lds	temp,i2c_result
	ret

;------------------------------------------------------------------
;
; 	z:	Pointer to the data to write/read.
;		First byte is slave address
;	temp2:	Number of bytes to read, including address byte.
;

i2c_write_read:

	rcall	i2c_waitready
	cpi	temp,0b01000000
	brsh	i2c_wr_e
	push	yh
	push	yl
	push	zh
	push	zl

	ldiw	y,i2c_var
	ldi	temp,0b10010000
	std	y+oi2c_result,temp		;result = busy
	ldi	temp,2
	std	y+oi2c_txcnt,temp		;store tx size
	std	y+oi2c_rxcnt,temp2		;store rx size
	adiw	y,oi2c_buf

	mov	_tmp0,temp			;save tx count
	ld	temp,z+				;get SLA
	cbr	temp,0x01			;
i2c_wr_wl:
	st	y+,temp
	dec	_tmp0
	breq	i2c_wr_wle
	ld	temp,z+
	rjmp	i2c_wr_wl
i2c_wr_wle:
	; Enable TWI, TWI int and initiate start condition
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
	outm8	TWCR,temp

	rcall	i2c_waitready

	sbrc	temp,4				;
	rjmp	i2c_wr_ex				;
	sbrs	temp,1				;at least 1 byte received
	rjmp	i2c_wr_ex				;
	ldiw	y,i2c_var
	ldd	temp2,y+oi2ci_idx
	adiw	y,oi2c_buf
	pop	zl
	pop	zh
	push	zh
	push	zl
i2c_wr_rl:
	ld	_tmp0,y+
	st	z+,_tmp0
	dec	temp2
	brne	i2c_wr_rl

i2c_wr_ex:
	pop	zl
	pop	zh
	pop	yl
	pop	yh
i2c_wr_e:
	lds	temp,i2c_result
	ret


;------------------------------------------------------------------

	.dseg

vi2c_stat:
	.byte	1
vi2c_blen:
	.byte	1
vi2c_addr:
	.byte	2

	.cseg

vi2c_stat_get:
	lds	temp,i2c_result
	ret

vi2c_param_get:
	tst	temp3
	brne	vi2c_pg2

	lds	temp,i2ci_idx
	rjmp	vi2c_pge

vi2c_pg2:
	ldiw	z,vi2c_blen
	add	zl,temp3
	adc	zh,_0
	ld	temp,z
vi2c_pge:
	ret

vi2c_param_set:
	ldiw	z,vi2c_blen
	add	zl,temp3
	adc	zh,_0
	st	z,temp
	ret

;------------------------------------------------------------------
;
; 	vi2c_addr:	Pointer to the data to write.
;			First byte is slave address
;	vi2c_blen:	Number of bytes to write including address byte.
;

vi2c_write:

	rcall	i2c_waitready
	cpi	temp,0b01000000
	brsh	vi2c_wex
	ldiw	z,i2c_var
	ldi	temp,0b10000000
	std	z+oi2c_result,temp		;result = busy
	lds	temp3,vi2c_blen
	cpi	temp3,I2C_BUFSIZE
	brlo	vi2c_w1
	ldi	temp3,I2C_BUFSIZE
vi2c_w1:
	std	z+oi2c_txcnt,temp3		;store size
	adiw	z,oi2c_buf
	ldsw	x,vi2c_addr
	lcall	dram_read_pp
	cbr	temp,0x01
vi2c_wl:
	st	z+,temp
	dec	temp3
	breq	vi2c_wle
	lcall	dram_read_pp
	rjmp	vi2c_wl
vi2c_wle:
	; Enable TWI, TWI int and initiate start condition
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
	outm8	TWCR,temp
vi2c_wex:
	ret

;------------------------------------------------------------------
;
; 	x:	Pointer to the data buffer.
;		First byte is slave address
;	temp3:	Buffer len. (Number of bytes to read + address byte.)
;

vi2c_read:

	rcall	i2c_waitready
	cpi	temp,0b01000000
	brsh	vi2c_rex

	ldiw	z,i2c_var
	ldi	temp,0b10000000
	std	z+oi2c_result,temp		;result = busy
	lds	temp3,vi2c_blen
	cpi	temp3,I2C_BUFSIZE
	brlo	vi2c_r1
	ldi	temp3,I2C_BUFSIZE
vi2c_r1:
	std	z+oi2c_rxcnt,temp3		;store size
	adiw	z,oi2c_buf
	ldsw	x,vi2c_addr
	lcall	dram_read_pp
	sbr	temp,0x01
	st	z+,temp
	dec	temp3

	; Enable TWI, TWI int and initiate start condition
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
	outm8	TWCR,temp

	rcall	i2c_waitready
	andi	temp,0b00000011
	breq	vi2c_rex				;
vi2c_rl:
	ld	temp,z+
	lcall	dram_write_pp
	dec	temp3
	brne	vi2c_rl

vi2c_rex:
	ret

;------------------------------------------------------------------
;
; 	vi2c_addr:	Pointer to the data to write.
;			First byte is slave address
;	temp2:		Number of bytes to write including address byte.
;
;	vi2c_blen:	Number of bytes to read including address byte.
;

vi2c_write_read:

	rcall	i2c_waitready
	cpi	temp,0b01000000
	brsh	vi2c_wr_ex
	ldiw	z,i2c_var
	ldi	temp,0b10010000
	std	z+oi2c_result,temp		;result = busy
	std	z+oi2c_txcnt,temp2		;store tx size
	lds	temp3,vi2c_blen
	cpi	temp3,I2C_BUFSIZE
	brlo	vi2c_wr_w1
	ldi	temp3,I2C_BUFSIZE
vi2c_wr_w1:
	std	z+oi2c_rxcnt,temp3		;store rx size
	adiw	z,oi2c_buf
	ldsw	x,vi2c_addr
	lcall	dram_read_pp
	cbr	temp,0x01
vi2c_wr_wl:
	st	z+,temp
	dec	temp2
	breq	vi2c_wr_wle
	lcall	dram_read_pp
	rjmp	vi2c_wr_wl
vi2c_wr_wle:
	; Enable TWI, TWI int and initiate start condition
	ldi	temp,(1<<TWEN)|(1<<TWIE)|(1<<TWINT)|(1<<TWSTA)
	outm8	TWCR,temp

	rcall	i2c_waitready

	sbrc	temp,4				;
	rjmp	i2c_wr_ex			;
	andi	temp,0b00000011			;
	breq	vi2c_rex			;
	ldiw	z,i2c_var
	ldd	temp2,z+oi2ci_idx
	adiw	z,oi2c_buf+1
	ldsw	x,vi2c_addr
	adiw	x,1
	rjmp	vi2c_wr_rl0
vi2c_wr_rl:
	ld	temp,z+
	lcall	dram_write_pp
vi2c_wr_rl0:
	dec	temp2
	brne	vi2c_wr_rl

vi2c_wr_ex:
	ret

;------------------------------------------------------------------


vi2c_ctrl:
	cpi	temp,1				;read ?
	brne	vi2c_c1
	rjmp	vi2c_read

vi2c_c1:
	cpi	temp,2				;write?
	brne	vi2c_c2
	rjmp	vi2c_write
vi2c_c2:
	cpi	temp,3				;write and read ?
	brne	vi2c_c3
	ldi	temp2,2				;write 1 byte (subaddress), then read
	rjmp	vi2c_write_read
vi2c_c3:
vi2c_ce:
	ret

;------------------------------------------------------------------

/* -MH- */
#if I2C_PCF8574_SUPPORT
pcf8574_in:
	; send address byte and read data byte
	; make a buffer[0:1] on stack
	push	_255                 ; place holder for input value buffer[0]
	in	zh,sph
	in	zl,spl
	ldi	temp,PCF8574_ADDR    ; PCF8574 address (7 bit)
	add	temp,temp3           ; usually == 0
	lsl	temp                 ; make into command byte (slave address << 1) without R/W flag
	push	temp                 ; push command byte to buffer[1]
	ldi	temp2,2              ; buffer length: command + 1 byte to read
	rcall	i2c_read             ; sends command byte and reads the byte to the buffer[0]
	pop	temp                 ; drop command byte from buffer[1]
	pop	temp                 ; drop value from buffer[0] into temp as return value
	ret

pcf8574_out:
	; send address byte and data byte
	; make a buffer[0:1] on stack
	push	temp                 ; output value to buffer[0]
	in 	zh,sph
	in 	zl,spl
	ldi	temp,PCF8574_ADDR    ; PCF8574 address (7 bit)
	add	temp,temp3           ; usually == 0
	lsl	temp                 ; make into command byte (slave address << 1) without R/W flag
	push	temp                 ; push command byte to buffer[1]
	ldi	temp2,2              ; 2 bytes to send: command and value
	rcall	i2c_write            ; sends the 2 bytes slave address + value
	pop	temp                 ; drop command byte from buffer[1]
	pop	temp                 ; drop value buffer[0] 
	ret
#endif /* I2C_PCF8574_SUPPORT */
/* -MH- */


;------------------------------------------------------------------

#if I2C_UART_SUPPORT

SC16IS740_in:
	; send address byte, register index and read data byte
	; make a buffer[0:1] on stack
	; massage internal register number into bits 6:3 of temp3
	swap	temp3       ;     7654.3210 -> 3210.7654
	lsr	temp3       ;     3210.7654 -> -321.0765 as per data sheet
	push	temp3       ; register index:  -A[3:0]00- to buffer[0]
	in	zh,sph
	in	zl,spl
	ldi	temp,SC16IS740_ADDR  ; slave address (7 bit)
	lsl	temp                 ; (slave address << 1) (aka 8 bit address or command byte)
	push	temp        ; command byte to buffer[1]
	ldi	temp2,2     ; buffer has command byte + index byte
	rcall	i2c_write_read   ; send command byte + register index, place result into buffer[0]
	pop	temp        ; drop command byte from buffer[1]
	pop	temp        ; drop input value from buffer[0] into temp as return value
	ret

SC16IS740_out:
	; send address byte, register index and data byte
	; make a buffer[0:1] on stack
	push	temp        ; output value buffer[0]
	; massage internal register number into bits 6:3 of temp3
	swap	temp3       ;
	lsr	temp3       ;
	push	temp3       ; register index -A[3:0]00- to buffer[1]
	in	zh,sph
	in	zl,spl
	ldi	temp,SC16IS740_ADDR  ; slave address (7 bit)
	lsl	temp                 ; (slave address << 1) (aka 8 bit address or command byte)
	push	temp        ; command byte to buffer[2]
	ldi	temp2,3
	rcall	i2c_write   ; send command byte + register index + data byte
	pop	temp			; drop command byte from buffer[2]
	pop	temp			; drop register index from buffer[1]
	pop	temp			; drop output value from buffer[0]
	ret

#endif /* I2C_UART_SUPPORT */

/* -MH- very similar to I2C_UART_SUPPORT except for the register address byte */
#if I2C_MCP23017_SUPPORT

MCP23017_in:
	; send address+R/W byte, register index and read data byte
	; temp:  return value as read from I2C
	; temp2: port number
	; temp3: register index (== given_port - base_port)
	; the MCP23017 expects 2 bytes: [slave address][register index] and sends one byte back
	
	; prepare a buffer[0:1] on the stack ([register index],[slave address])
	push	temp3                ; -> register index byte to buffer[0] and SP->buffer[1]
	in 	zh,sph               ; prepare 16-bit pointer to stack ...
	in 	zl,spl               ; ... complete pointer to buffer[1]
	ldi	temp,MCP23017_ADDR   ; load  slave address (7 bit)
	lsl	temp                 ; (slave address << 1) (aka 8 bit address or command byte)
	push	temp                 ; -> (slave address << 1) to buffer[1]
	ldi	temp2,2              ; 2 bytes to send: address and register index
	rcall	i2c_write_read       ; send 2 bytes in buffer and read one byte to buffer[0]
	pop	temp                 ; drop (slave address << 1) from buffer[1]
	pop	temp                 ; drop input byte from buffer[0] into temp as return value
	ret

MCP23017_out:
	; send address+R/W byte, register index and data byte
	; temp:  value to output
	; temp2: port number
	; temp3: register index(== given_port - base_port)
	; the MCP23017 expects 3 bytes: [slave address][register index][data byte to write]
	
	; prepare a buffer[0:2] on the stack (data bytes are pushed right to left)
	push	temp                 ; -> data byte to buffer[0]           and SP->buffer[1]
	push	temp3	               ; -> register index byte to buffer[1] and SP->buffer[2]
	in 	zh,sph               ; prepare 16-bit pointer to buffer[2] ...
	in 	zl,spl               ; ... complete pointer to stack
	ldi	temp,MCP23017_ADDR   ; load  slave address (7 bit)
	lsl	temp                 ; (slave address << 1) (aka 8 bit address or command byte)
	push	temp                 ; -> (slave address << 1) to to buffer[2]
	ldi	temp2,3              ; 3 bytes to send: address, register index and, data byte
	rcall	i2c_write            ; send the 3 bytes (in buffer pointed to by Z)
	pop	temp                 ; drop (slave address << 1) from buffer[2]
	pop	temp                 ; drop register index from buffer[1]
	pop	temp                 ; drop output value from buffer[0]
	ret

#endif /* I2C_MCP23017_SUPPORT */
/* -MH- */

#endif /* I2C_SUPPORT */
;------------------------------------------------------------------
; vim:set ts=8 noet nowrap
