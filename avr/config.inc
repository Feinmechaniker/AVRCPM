;    Configuration, hardware definition, ...
;
;    Copyright (C) 2010 Sprite_tm
;    Copyright (C) 2010,2012,2013 Leo C.
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


#define VMAJOR   3		/* Version number */
#define VMINOR   5
#define VERS_STR "3.5"


;-----------------------------------------------------------------------
;
;             Hardware and feature configuration
;

; Supported MCUs are: atmega328P atmega168 atmega88 atmega8
; Z80 emulation currently needs atmega328P, so this is the default.
;#define atmega328p

; MCU frequency in Hz.
;#define F_CPU 20000000

; Set this to 0, if you have a (one) DRAM chip whith 4 Bit data bus.
;#define DRAM_8BIT 1

; Baudrate of serial port (console).
; #define BAUD 115200

; I2C_SUPPORT defaults to 1 on 8-bit-ram systems and must be 0 on 4-bit-ram systems.
; Set this do 0, if you don't have  I2C peripherals and want to save some flash
; or use the I2C pins for other purposes.
;#define I2C_SUPPORT

; Support for the SC16IS740 I2C UART chip is included automatically, when
; I2C support is enabled. Uncomment and set to 0 if you don't want support for
; the SC16IS740.
;#define I2C_UART_SUPPORT 1

; -MH-
; Support for the MCP23017 I2C 16-bit GPIO chip is included automatically, when
; I2C support is enabled. Uncomment and set to 0 if you don't want support for
; the MCP23017.
;#define I2C_MCP23017_SUPPORT 1

; Support for the PCF8574 I2C 8-bit GPIO chip is included automatically, when
; I2C support is enabled. Uncomment and set to 0 if you don't want support for
; the PCF8574.
;#define I2C_PCF8574_SUPPORT 1
; -MH-


; Emulate Z80 cpu if 1, 8080 otherwise.
;
;#define EM_Z80	1

; Uncomment and set RAMDISKCOUNT to 1-4 if you want to test the experimental and
; unsupported ramdisk feature . Number depends on actual ram size (256kx4 - 4Mx8)
; and bios dpb/dph definitions.
;#define RAMDISKCNT    0

; Uncomment and set to 0, if you need to save flash (ie. ATmega88).
;#define FAT16_SUPPORT 1

; Uncomment and set to 0, if you don't want to use CP/M partitions.
; (Saves some bytes.)
;#define CPMDSK_SUPPORT 1

; Uncomment and set to 0, if you need to save flash.
;#define MMCBOOTLOADER 1

; Uncomment to disable ADC support
;#define ADC_SUPPORT 0

;-----------------------------------------------------------------------
;Debugging aids

.equ BOOTWAIT        = 1
.equ MEMTEST         = 1
.equ MEMFILL         = 1	/* Fill DRAM */
.equ SRAM_FILL       = 1	/* Fill unused SRAM */
.equ MMC_DEBUG       = 0	/* Increase for more debugging */
.equ MMC_DEBUG_RDW   = 0
.equ DSKSEL_DEBUG    = 0
.equ DISK_DEBUG      = 0	/* 1 = BOOT/HOME debug, 2 = +R/W debug */
.equ HOSTRW_DEBUG    = 0
.equ FAT16_DEBUG     = 0
.equ FAT16_DBG_FAT   = 0
.equ HEAP_DEBUG      = 0
.equ PORT_DEBUG      = 0
.equ INS_DEBUG       = 0
.equ STACK_DBG       = 0
.equ PRINT_PC        = 0
.equ TIMER_DEBUG     = 0
.equ I2C_STATE_DEBUG = 0
.equ MEMDUMP_DEBUG   = 1

;-----------------------------------------------------------------------
; Z80/8080 Virtual Ports

#define UARTDR		1		/* UART Data Register */
#define UARTCSR		3		/* UART Control/Status Register */
#define UARTRXRDY	0x01		/* RxReady Status Bit Mask */
#define UARTTXRDY	0x02		/* TxReady Status Bit Mask */

#define READ_FUNC	7
#define WRITE_FUNC	6
#define BOOT_FUNC	5
#define HOME_FUNC	4

#define	TIMERPORT   0x40		/* Base z80 port address for clock access */
#define TIMER_CTL   TIMERPORT
#define TIMER_MSECS TIMERPORT+1
#define TIMER_SECS  TIMER_MSECS+2
#define	CLOCKPORT   TIMERPORT+7		/* Real time clock BCD (ss,mm,hh,DD,MM,YYYY) */

#define starttimercmd	1
#define quitTimerCmd	2
#define printTimerCmd	15
#define uptimeCmd	16

#define MEMDUMPPORT	0x09
#define DEBUGPORT	0x4F

#define startTraceCmd	0x01		/* 'OUT (DEBUGPORT),startTraceCmd' starts tracing */
#define stopTraceCmd	0x00
#define PrintStackCmd	0x02

; Virtual I2C Interface
#define I2CSTAT	0x05
#define I2CCTRL	0x05
#define I2CBLEN	0x06
#define I2CADR	0x07
#define I2CADRL	0x07
#define I2CADRH	0x08

; Simple ADC Interface
#define ADC80	0x17			/* Read Value from ADC6 Pin, 8 bit */
#define ADC81	0x18			/* Read Value from ADC7 Pin, 8 bit */
#define ADCTEMP	0x19			/* Read Value from Temp Sensor, 8 bit */
#define ADCBGL	0x20			/* Read 1.1V Bandgap, 10 bit */
#define ADCBGH	0x21			/* Read 1.1V Bandgap, 10 bit */

; ISC16IS740 UART (0x50...0x5F))
#define I2C_SC16IS740_PORT 0x50			/* ISC16IS740 virtual base port */
#define I2C_SC16IS740_REGS	16

; -MH-
; MCP23017 16-bit (2x8) GPIO from 0x60...0x75
#define I2C_MCP23017_PORT 0x60			/* MCP23017 virtual base port */
#define I2C_MCP23017_REGS 22				/* MCP23017 has 22 registers of 1 byte each */
; -MH-

; Port-Expander PCF8574 (0x80...0x87)
#define I2C_PCF8574_PORT	0x80			/* first PCF8574 port */
#define I2C_PCF8574_REGS	7
#define PORT0	0x80			/* 1st PCF8574 chip */
#define PORT1	0x81			/* 2nd PCF8574 chip */
#define PORT2	0x82			/* 3rd PCF8574 chip */
#define PORT3	0x83			/* 4th PCF8574 chip */
#define PORT4	0x84			/* 5th PCF8574 chip */
#define PORT5	0x85			/* 6th PCF8574 chip */
#define PORT6	0x86			/* 7th PCF8574 chip */
; #define PORT7	0x87			/* used  for MCP23017 */

;-----------------------------------------------------------------------
;

#define IPLADDR	0x2000		/* Bootloader load address */

#define DRAM_WAITSTATES 1	/* Number of additional clock cycles for dram read access */
#define REFR_RATE   64000       /* dram refresh rate in cycles/s. */
				/* Most drams need 1/15.6µs. */
#define	RXBUFSIZE 128		/* USART recieve buffer size. Must be power of 2 */
.if SRAM_SIZE > 1024
 .equ TXBUFSIZE = 128		/* USART transmit buffer size. Must be power of 2 */
.else
 .equ TXBUFSIZE = 32		/* USART transmit buffer size. Must be power of 2 */
.endif

.if SRAM_SIZE > 1024
  .equ FAT16_FATBUF = 1
.else
  .equ FAT16_FATBUF = 0
.endif


#define I2C_CLOCK  100000	/* 100kHz */
#define I2C_BUFSIZE    66 	/* largest message size + slave address (SLA) + subaddress */


#define MMC_SPI2X  1		/* 0 = SPI CLK/4, 1 = SPI CLK/2 */

#define MEMFILL_VAL      0x76	/* Fill ram with HALT opcode. */
#define DBG_TRACE_BOTTOM 0x01	/* Page boundaries for INS_DEBUG and PRINT_PC  */
#define DBG_TRACE_TOP	 0xdc	/* Trace is off, below bottom page and above top page. */
#define SRAMFILL_VAL	 0x5A   /* Fill unused SRAM */

; define PARTID 0x52		/* Partition table id */
				/* http://www.win.tue.nl/~aeb/partitions/partition_types-1.html */
; Partition Table Structures

#define PART_TYPE   4
#define PART_START  8
#define PART_SIZE  12

/*
 *  Partition table id
 *	(see http://www.win.tue.nl/~aeb/partitions/partition_types-1.html)
 */
#define PARTID1_FAT16 0x0E
#define PARTID2_FAT16 0x06
#define PARTID_CPM    0x52


;-----------------------------------------------------------------------
;
#ifndef DRAM_8BIT
  #define DRAM_8BIT 1		/* 1 = 8bit wide data bus to DRAM (ie two 4-bit Chips)*/
#endif				/* 0 = only one 4 bit wide DRAM chip */
#ifndef F_CPU
  #define F_CPU	20000000	/* system clock in Hz; defaults to 20MHz */
#endif
#ifndef BAUD
  #define BAUD   115200		/* console baud rate */
#endif
#ifndef I2C_SUPPORT
  #define I2C_SUPPORT DRAM_8BIT	/* I2C requires 8 bit DRAM */
#endif
#if I2C_SUPPORT && !DRAM_8BIT
  #error "I2C_SUPPORT requires 8 bit DRAM (DRAM_8BIT=1)!"
#endif
#ifndef ADC_SUPPORT
  #define ADC_SUPPORT 1
#endif

#ifndef I2C_UART_SUPPORT
  #define I2C_UART_SUPPORT     I2C_SUPPORT
#endif

/* -MH- */
#ifndef I2C_MCP23017_SUPPORT
  #define I2C_MCP23017_SUPPORT I2C_SUPPORT   ; if not explicitely defined above: enable if I2C is supported
#endif

#ifndef I2C_PCF8574_SUPPORT
  #define I2C_PCF8574_SUPPORT  I2C_SUPPORT   ; if not explicitely defined above: enable if I2C is supported
#endif
/* -MH- */

/* -MH- */
#if I2C_UART_SUPPORT
  #define SC16IS740_ADDR  0x48	   /* SC16IS740 I2C slave address. (A0=VDD, A1=VDD => 1001.000) */
  /* #define SC16IS740_CLOCK 9216000  -MH- this is unused */
#endif
/* -MH- */

/* -MH- */
#if I2C_MCP23017_SUPPORT
  #define MCP23017_ADDR  0x20	   /* MCP23017 slave address. (A0=GND, A1=GND, A2=GND, 0100.000) */
#endif
/* -MH- */

/* -MH- */
#if I2C_PCF8574_SUPPORT
  #define PCF8574_ADDR   0x20	   /* PCF8574 address (e.g. LCD backpack module with all 3 solder bridges open == pulled up to VDD => 0x27) */
#endif
/* -MH- */

#ifndef EM_Z80
  #define EM_Z80 1		/* Emulate Z80 if true, else 8080 */
#endif

#ifndef CPMDSK_SUPPORT
  #define CPMDSK_SUPPORT 1	/* Include Support for CP/M partitions */
#endif

#ifndef FAT16_SUPPORT
  #define FAT16_SUPPORT 1	/* Include Support for a FAT16 partition */
#endif				/*  which may contain CP/M image files. */

#ifndef RAMDISKCNT
  #define RAMDISKCNT   0	/* Number of RAM disks */
#endif
#define RAMDISKNR     'I'-'A'	/* Drive "letter" for first RAM disk */

/* MMC/SD Bootloader support.   */
#define BOOTLDRSIZE 2048 	/* Bytes needed for Bootloader (1024 Words Bootsection Size) */
#define DEVID_S "ACPM"
#ifndef MMCBOOTLOADER
  #define MMCBOOTLOADER 1
#endif
#ifndef TESTVERSION
  #define TESTVERSION 0
#endif

#define TC_1MS (F_CPU/1000)
#define TC_1US (F_CPU/1000000)

;-----------------------------------------------------------------------
; Port declarations

#if DRAM_8BIT	/* Implies software uart */

;Port D
.equ RAM_D0	= 0
.equ RAM_D1	= 1
.equ RAM_D2	= 2
.equ RAM_D3	= 3
.equ RAM_D4	= 4
.equ RAM_D5	= 5
.equ RAM_D6	= 6
.equ RAM_D7	= 7
.equ RAM_A0	= 0
.equ RAM_A1	= 1
.equ RAM_A2	= 2
.equ RAM_A3	= 3
.equ RAM_A4	= 4
.equ RAM_A5	= 5
.equ RAM_A6	= 6
.equ RAM_A7	= 7

;Port B
.equ MMC_MOSI	= 3
.equ MMC_MISO	= 4
.equ MMC_SCK	= 5
.equ RAM_A8	= 3
.equ RAM_A9	= 4
.equ RAM_A10	= 5

.equ RXD	= 0
.equ TXD	= 1
.equ MMC_CS	= 2

.equ P_MMC_CS	= PORTB
.equ P_A8	= PORTB
.equ P_RXD	= PORTB
.equ P_TXD	= PORTB

;Port C
.equ RAM_RAS	= 0
.equ RAM_CAS	= 1
.equ RAM_OE	= 2
.equ RAM_W	= 3
.equ SDA	= 4
.equ SCL	= 5

.equ P_RAS	= PORTC
.equ P_CAS	= PORTC
.equ P_OE	= PORTC
.equ P_W	= PORTC
.equ P_I2C	= PORTC


#else	/* 4 bit RAM, hardware uart */

; Port D
.equ RXD	= 0
.equ TXD	= 1
.equ RAM_OE	= 2
.equ RAM_A8	= 3
.equ MMC_CS	= 4
.equ RAM_A5	= 5
.equ RAM_A6	= 6
.equ RAM_A7	= 7

.equ P_RXD	= PORTD
.equ P_OE	= PORTD
.equ P_A8	= PORTD
.equ P_MMC_CS	= PORTD
.equ P_AH	= PORTD
	             ; RAM_A[7..5]
.equ RAM_AH_MASK = (1<<RAM_A8)|(1<<RAM_A7)|(1<<RAM_A6)|(1<<RAM_A5)
.equ PD_OUTPUT_MASK = (1<<MMC_CS) | (1<<RAM_OE) | RAM_AH_MASK


;Port B
.equ RAM_A4	= 0
.equ RAM_A3	= 1
.equ RAM_A2	= 2
.equ RAM_A1	= 3
.equ MMC_MOSI	= 3
.equ RAM_A0	= 4
.equ MMC_MISO	= 4
.equ RAM_RAS	= 5
.equ MMC_SCK	= 5

.equ P_RAS	= PORTB
.equ P_AL	= PORTB
			; RAM_A[4..0]
.equ RAM_AL_MASK = (1<<RAM_A4)|(1<<RAM_A3)|(1<<RAM_A2)|(1<<RAM_A1)|(1<<RAM_A0)
.equ PB_OUTPUT_MASK = (1<<RAM_ras) | RAM_AL_MASK

;Port C
.equ RAM_D0	= 0
.equ RAM_D1	= 1
.equ RAM_D2	= 2
.equ RAM_D3	= 3
.equ RAM_W	= 4
.equ RAM_CAS	= 5

.equ P_DQ	= PORTC
.equ P_W	= PORTC
.equ P_CAS	= PORTC

.equ RAM_DQ_MASK = (1<<RAM_D3)|(1<<RAM_D2)|(1<<RAM_D1)|(1<<RAM_D0)
.equ PC_OUTPUT_MASK = (1<<RAM_CAS)|(1<<RAM_W)

#endif	/* DRAM_8BIT */


;-----------------------------------------------------------------------
;Register definitions

.def	_tmp0	= r0
.def	_tmp1	= r1

.def	_RAS0	= r2
.def	_CAS0	= r3
.def	_OE	= r4
.def	_WE	= r5
.def	_255	= r6
.def	_0	= r7

.def	z_c	= r8	;
.def	z_b	= r9	;
.def	z_bcl	= r8	;
.def	z_bch	= r9	;
.def	z_e	= r10	;
.def	z_d	= r11	;
.def	z_del	= r10	;
.def	z_deh	= r11	;
.def	z_l	= r12	;
.def	z_h	= r13	;
.def	z_hll	= r12	;
.def	z_hlh	= r13	;
.def	z_spl	= r14	;
.def	z_sph	= r15	;
.def	temp	= r16 	;
.def	temp2	= r17 	;
.def	temp3	= r18	;
;.def	temp4	= r19	;
.def	intstat	= r19 	;interpreter status / interrupt status
.def	z_flags	= r20	;
.def	z_a	= r21	;
.def	opl	= r22	;
.def	oph	= r23	;
.def	z_pcl	= r24	;
.def	z_pch	= r25	;
; xl 		;r26
; xh		;r27
; yl		;r28
; yh		;r29
; zl		;r30
; zh		;r31


.equ	i_break = 0			;break detected flag
.equ	i_trace = 1			;cpu interpreter trace flag
.equ	i_halt  = 2			;executing halt instruction

#if defined __ATmega8__
 #if DRAM_8BIT

	.equ	flags	= UBRRL		;UART is unused with 8-Bit RAM
 #else
	.equ	flags	= TWBR		;TWI is unused with 4-Bit RAM
 #endif
	.equ	P_PUD	= SFIOR

#else

	.equ	flags	= GPIOR0
	.equ	P_PUD	= MCUCR

#endif /* __ATmega8__ */

; Flags:
	.equ	hostact	= 7		;host active flag
	.equ	hostwrt	= 6		;host written flag
	.equ	rsflag	= 5		;read sector flag
	.equ	readop	= 4		;1 if read operation

	.equ	prefixfd = 1		;Opcode prefix DD=0, FD=1


#if EM_Z80
  #define CPUSTR "Z80"
#else
  #define CPUSTR "8080"
#endif
#if TESTVERSION
  #define TESTSTR " Test"
#else
  #define TESTSTR ""
#endif

#if defined __ATmega8__
.equ	RXTXDR0	= UDR
.equ	UCSR0A	= UCSRA
.equ	 UDRE0	= UDRE
.equ	UCSR0B	= UCSRB
.equ	 RXCIE0	= RXCIE
.equ	 UDRIE0	= UDRIE
.equ	 RXEN0	= RXEN
.equ	 TXEN0	= TXEN
.equ	UCSR0C	= UCSRC
.equ	 UCSZ00	= UCSZ0
.equ	 UCSZ01	= UCSZ1
.equ	UBRR0H	= UBRRH
.equ	UBRR0L	= UBRRL
.equ	OCR2A	= OCR2
.equ	OC2Aaddr= OC2addr
.equ	TCCR2A	= TCCR2
.equ	TCCR2B	= TCCR2
.equ	TIMSK1	= TIMSK
.equ	TIMSK2	= TIMSK
.equ	OCIE2A	= OCIE2
.equ	TIFR1	= TIFR
.equ	ICIE1	= TICIE1
#else
.equ	RXTXDR0	= UDR0
#endif

; vim:set ts=8 noet nowrap
