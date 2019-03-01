''***************************************
''*  Full-Duplex Serial Driver v1.1     *
''*  Author: Chip Gracey                *
''*  Copyright (c) 2006 Parallax, Inc.  *
''*  See end of file for terms of use.  *
''***************************************
'' buffer size adaptetd for AVR CP/M , Joe. G. 2013
{
Log

02.01.2013      correct storage location in the buffer

}

CON
' Buffer sizes
  'Enlarge buffer size again as workaround for overflow
  RX_SIZE = 2000                         'Receive buffer size
  TX_SIZE = 160                          'Transmit buffer size

' Buffer thresholds
  RTS_TH_LOW      = RX_SIZE * 1/4       'RTS on
  RTS_TH_HIGH     = RX_SIZE * 3/4       'RTS off
  RX_XON_TH_XON   = RX_SIZE * 1/8       'send XON
  RX_XON_TH_XOFF1 = RX_SIZE * 1/2       'send XOFF
  RX_XON_TH_XOFF2 = RX_SIZE * 7/8       'send XOFF


' Modes
  M_RTS_EN        = 1 << 0
  M_CTS_EN        = 1 << 1
  M_DSR_EN        = 1 << 2
  M_RX_XON        = 1 << 3
  'M_TX_XON       = 1 <<

VAR

  long  cog                             'cog flag/id
  long  dtr_pin

  word  rx_head                         '4 contiguous words
  word  rx_tail
  word  tx_head
  word  tx_tail
  byte  rx_buffer[RX_SIZE]
  byte  tx_buffer[TX_SIZE]


PUB start(rxpin, txpin, rtspin, ctspin, dtrpin, dcdpin, dsrpin, mode, baudrate) : okay

'' Start serial driver - starts a cog
'' returns false if no cog available
''
'' mode bit 0 = invert rx
'' mode bit 1 = invert tx
'' mode bit 2 = open-drain/source tx
'' mode bit 3 = ignore tx echo on rx

  stop

  wordfill(@rx_head, 0, 4)                            'clear buffer pointers

  rxbuff := @rx_buffer
  rx_head_ptr := @rx_head
  rx_tail_ptr := @rx_tail
  rxhead      := 0
  rxtail      := 0
  txbuff := @tx_buffer
  tx_head_ptr := @tx_head
  tx_tail_ptr := @tx_tail

  dtr_pin := dtrpin
  if dtr_pin => 0
   outa[dtr_pin]~~
   dira[dtr_pin]~~
  baudrate := clkfreq / baudrate                      'convert baudrate to clock tics
  longmove(@rxmask, @rxpin, 9)

  okay := cog := cognew(@entry, 0) + 1


PUB stop

'' Stop serial driver - frees a cog

  if cog
    cogstop(cog~ - 1)
  wordfill(@rx_head, 0, 4)


PUB dtr_set(val)
  if dtr_pin => 0
   outa[dtr_pin] := val

PUB rxflush

'' Flush receive buffer

  repeat while rxcheck => 0


PUB rxcheck : rxbyte

'' Check if byte received (never waits)
'' returns -1 if no byte received, $00..$FF if byte

  rxbyte--
  if rx_tail <> rx_head
    rxbyte := rx_buffer[rx_tail]
    rx_tail := (rx_tail + 1) // RX_SIZE                   'Change Buffer size


PUB rxtime(ms) : rxbyte | t

'' Wait ms milliseconds for a byte to be received
'' returns -1 if no byte received, $00..$FF if byte

  t := cnt
  repeat until (rxbyte := rxcheck) => 0 or (cnt - t) / (clkfreq / 1000) > ms


PUB rx : rxbyte

'' Receive byte (may wait for byte)
'' returns $00..$FF

  repeat while (rxbyte := rxcheck) < 0


PUB tx(txbyte)

'' Send byte (may wait for room in buffer)

  result := tx_head
  tx_buffer[result] := txbyte
  result := ++result // TX_SIZE
  repeat until result <> tx_tail
  tx_head := result



PUB str(stringptr)

'' Send string

  repeat strsize(stringptr)
    tx(byte[stringptr++])


PUB dec(value) | i

'' Print a decimal number

  if value < 0
    -value
    tx("-")

  i := 1_000_000_000

  repeat 10
    if value => i
      tx(value / i + "0")
      value //= i
      result~~
    elseif result or i == 1
      tx("0")
    i /= 10


PUB hex(value, digits)

'' Print a hexadecimal number

  value <<= (8 - digits) << 2
  repeat digits
    tx(lookupz((value <-= 4) & $F : "0".."9", "A".."F"))


PUB bin(value, digits)

'' Print a binary number

  value <<= 32 - digits
  repeat digits
    tx((value <-= 1) & 1 + "0")


DAT

                        '***********************************
                        '* Assembly language serial driver *
                        '***********************************

                        org     0
'
' Entry
'
entry
                        mov     t1,rxmask             wc     'convert bit number to mask
        if_c            mov     rxmask,#0                    'mask = 0 if bit number < 0
        if_nc           mov     rxmask,#1
                        shl     rxmask,t1

                        mov     t1,txmask             wc     'convert bit number to mask
        if_c            mov     txmask,#0
        if_nc           mov     txmask,#1
                        shl     txmask,t1

                        mov     t1,rtsmask            wc     'convert bit number to mask
        if_c            mov     rtsmask,#0
        if_nc           mov     rtsmask,#1
                        shl     rtsmask,t1

                        mov     t1,ctsmask            wc     'convert bit number to mask
        if_c            mov     ctsmask,#0
        if_nc           mov     ctsmask,#1
                        shl     ctsmask,t1

                        mov     t1,dtrmask            wc     'convert bit number to mask
        if_c            mov     dtrmask,#0
        if_nc           mov     dtrmask,#1
                        shl     dtrmask,t1

                        mov     t1,dcdmask            wc     'convert bit number to mask
        if_c            mov     dcdmask,#0
        if_nc           mov     dcdmask,#1
                        shl     dcdmask,t1

                        mov     t1,dsrmask            wc     'convert bit number to mask
        if_c            mov     dsrmask,#0
        if_nc           mov     dsrmask,#1
                        shl     dsrmask,t1

                        or      outa,txmask                  'init all output pins to high level (inactive)
                        or      dira,txmask
'                        or      outa,dtrmask
'                        or      dira,dtrmask
                        or      outa,rtsmask
                        or      dira,rtsmask

                        mov     txcode,#transmit            'initialize ping-pong multitasking
'
' Receive
'
receive                 jmpret  rxcode,txcode               'run a chunk of transmit code, then return

                        test    rxmask,ina            wz    'wait for high level on rx pin
        if_z            jmp     #receive

waitstart               jmpret  rxcode,txcode               'run a chunk of transmit code, then return

                        test    rxmask,ina            wz    'wait for start bit on rx pin
        if_z            jmp     #startbit

                        mov     t1,rxhead
                        rdword  rxtail,rx_tail_ptr              'get tail
                        sub     t1,rxtail             wc
        if_c            add     t1,rxbuffsize
                        cmp     t1,rts_thh            wc    'number of bytes >= high threshold ?
        if_nc           or      outa,rtsmask                '   switch RTS off
                        cmp     t1,rts_thl            wc    'number of bytes < low threshold ?
        if_c            andn    outa,rtsmask                '   switch RTS on
                        jmp     #waitstart

startbit                mov     rxbits,#8                   'ready to receive byte
                        mov     rxcnt,bitticks
                        shr     rxcnt,#1
                        add     rxcnt,cnt
                        'add     rxcnt,bitticks

:bit                    add     rxcnt,bitticks              'ready next bit period

:wait                   jmpret  rxcode,txcode               'run a chuck of transmit code, then return

                        mov     t1,rxcnt                    'check if bit receive period done
                        sub     t1,cnt
                        cmps    t1,#0                 wc
        if_nc           jmp     #:wait

                        test    rxmask,ina            wc    'receive bit on rx pin
                        rcr     rxdata,#1
                        djnz    rxbits,#:bit

                        shr     rxdata,#32-8          wz    'justify and trim received byte
                        'and     rxdata,#$FF           wz
        if_z            jmp     #receive                    'no stop bit, ignore byte with framing error and start over

                        jmpret  rxcode,txcode               'run a chuck of transmit code, then return

                                                            'save received byte and inc head
                        mov     t1,rxbuff
                        add     t1,rxhead
                        wrbyte  rxdata,t1
                        mov     t1,rxhead
                        add     t1,#1
                        rdword  rxtail,rx_tail_ptr          'get tail
                        cmpsub  t1,rxbuffsize               '
                        cmp     t1,rxtail             wz    'don't update, if pointer equal (buffer full)
        if_nz           wrword  t1,rx_head_ptr
        if_nz           mov     rxhead,t1
                        jmp     #receive                    'byte done, receive next byte
'
' Transmit
'
transmit                jmpret  txcode,rxcode               'run a chunk of receive code, then return

                        rdword  t2,tx_head_ptr              'check for head <> tail
                        rdword  t3,tx_tail_ptr
                        cmp     t2,t3                 wz
        if_z            jmp     #transmit

                        add     t3,txbuff                   'get byte and inc tail
                        rdbyte  txdata,t3
                        sub     t3,txbuff
                        add     t3,#1
                        cmpsub  t3,txbuffsize
                        wrword  t3,tx_tail_ptr

                        shl     txdata,#2                  'ready byte to transmit
                        or      txdata,txframebits
                        mov     txbits,#11
                        mov     txcnt,cnt

:bit                    shr     txdata,#1             wc    'output bit on tx pin
                        muxc    outa,txmask
                        add     txcnt,bitticks              'ready next cnt

:wait                   jmpret  txcode,rxcode               'run a chunk of receive code, then return

                        mov     t1,txcnt                    'check if bit transmit period done
                        sub     t1,cnt
                        cmps    t1,#0                 wc
        if_nc           jmp     #:wait

                        djnz    txbits,#:bit                'another bit to transmit?

                        jmp     #transmit                   'byte done, transmit next byte

' Data

txframebits             long    $0401
rxbuffsize              long    RX_SIZE
rxbuff                  long    0
rxhead                  long    0
rxtail                  long    0
rx_head_ptr             long    0
rx_tail_ptr             long    0

txbuffsize              long    TX_SIZE
txbuff                  long    0
tx_head_ptr             long    0
tx_tail_ptr             long    0

rts_thl                 long    RTS_TH_LOW
rts_thh                 long    RTS_TH_HIGH
xon_thon                long    RX_XON_TH_XON
xon_thoff1              long    RX_XON_TH_XOFF1
xon_thoff2              long    RX_XON_TH_XOFF2


rxmask                  long    0
txmask                  long    0
rtsmask                 long    0
ctsmask                 long    0
dtrmask                 long    0
dcdmask                 long    0
dsrmask                 long    0
rxtxmode                long    0
bitticks                long    0

'
' Uninitialized data
'
t1                      res     1
t2                      res     1
t3                      res     1

rxdata                  res     1
rxbits                  res     1
rxcnt                   res     1
rxcode                  res     1

txdata                  res     1
txbits                  res     1
txcnt                   res     1
txcode                  res     1

                        fit     $1f0

{{
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                   TERMS OF USE: MIT License                                                  │
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    │
│files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    │
│modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software│
│is furnished to do so, subject to the following conditions:                                                                   │
│                                                                                                                              │
│The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.│
│                                                                                                                              │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          │
│WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         │
│COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   │
│ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
}}