''***************************************
''*  PS/2 Keyboard Driver v1.0.1        *
''*  Author: Chip Gracey                *
''*  Copyright (c) 2004 Parallax, Inc.  *
''*  See end of file for terms of use.  *
''***************************************

{-----------------REVISION HISTORY---------------------------------
 v1.0.1 - Updated 6/15/2006 to work with Propeller Tool 0.96
 ------------------------------------------------------------------}

{-----------------KEYBOARD LAYOUT HISTORY--------------------------
 2009-08-31 (Y-M-D)
  Patch for german keyboard layout
  Author: oog
  Added german keyboard layout.
  Original layout is commented as keyboard-us-en (US-English).
  German layout is commented as keyboard-de (de = deutsch = "german").

  Now there are two tables for keys with and without SHIFT-Key.
  It should be easier to implement different international layouts.
  However, it's bigger now and uses more memory.

 2009-09-05 (Y-M-D)
  Fixed bugs
  - code bug on home-key fixed
    new replace-codes for de_ae, de_ou and de_ue

  New
  - Documentation of control-key bits
    This should be helpful for translations of this driver into
    different languages.
  - table_alt_r
    This table contains characters for the german "AltGr" key.
  - AltGr+F1..F12
    Returns line-characters, selected from $90..$9f

 2009-09-06 (Y-M-D)
  Fixed bugs
  - patch table_shift for "?"

  Differences to Parallax driver:
  - Different codes for NumLock, CapsLock and ScrLock to avoid
    conflict with german "ß"
    Codes are defined as constants and easy to change
  - Easy Cursor codes implemented to avoit conflict with the "Ä"-key
    Easy Cursor codes are easy to understand,
    for example "Cursor Left" is the character "←"

 2011-11-02 (Y-M-D)
  Fixed bugs
  - patch table_shift for "§" from $14 to $A7

 ------------------------------------------------------------------}

con
  de_ae = $A6   'replace code - not used on keyboard
  de_oe = $A7   'replace code - not used on keyboard
  de_ue = $A8   'replace code - not used on keyboard
  lock  = $BC   'Parallax used codes $DD, $DE and $DF
                'There was a conflict between $DF=NumLock="ß"
  ScrLk = lock
  CpsLk = lock+1
  NumLk = lock+2

'
'Uncomment one of the next constant blocks
'
'


{{Easy cursor codes start for AVR CP/M Joe G. 2013}}


' CrsLt = $02E4   '←
' CrsRt = $03E6   '→
' CrsUp = $04E8   '↑
' CrsDn = $05E2   '↓
  CrsLt = $C0E4   '←
  CrsRt = $C1E6   '→
  CrsUp = $C2E8   '↑
  CrsDn = $C3E2   '↓
  CrsHm = $06E7   '◀
  CrsEn = $07E1   '▶
  PgUp  = $A0E9   '
  PgDn  = $A2E3   '
  Bksp  = $00C8   'È
  Del   = $BAEA   '
  Ins   = $BBE0   '
  Esc   = $001B   '
  Apps  = $CC00   'Ì
  Power = $CD00   'Í
  Sleep = $CE00   'Î
  WkUp  = $CF00   'Ï

{{Easy cursor codes end}}

VAR

  long  cog

  long  par_tail        'key buffer tail        read/write      (19 contiguous longs)
  long  par_head        'key buffer head        read-only
  long  par_present     'keyboard present       read-only
  long  par_states[8]   'key states (256 bits)  read-only
  long  par_keys[8]     'key buffer (16 words)  read-only       (also used to pass initial parameters)


PUB start(dpin, cpin) : okay

'' Start keyboard driver - starts a cog
'' returns false if no cog available
''
''   dpin  = data signal on PS/2 jack
''   cpin  = clock signal on PS/2 jack
''
''     use 100-ohm resistors between pins and jack
''     use 10K-ohm resistors to pull jack-side signals to VDD
''     connect jack-power to 5V, jack-gnd to VSS
''
'' all lock-keys will be enabled, NumLock will be initially 'on',
'' and auto-repeat will be set to 15cps with a delay of .5s

  okay := startx(dpin, cpin, %0_000_000, %01_01000)


PUB startx(dpin, cpin, locks, auto) : okay

'' Like start, but allows you to specify lock settings and auto-repeat
''
''   locks = lock setup
''           bit 6 disallows shift-alphas (case set soley by CapsLock)
''           bits 5..3 disallow toggle of NumLock/CapsLock/ScrollLock state
''           bits 2..0 specify initial state of NumLock/CapsLock/ScrollLock
''           (eg. %0_001_100 = disallow ScrollLock, NumLock initially 'on')
''
''   auto  = auto-repeat setup
''           bits 6..5 specify delay (0=.25s, 1=.5s, 2=.75s, 3=1s)
''           bits 4..0 specify repeat rate (0=30cps..31=2cps)
''           (eg %01_00000 = .5s delay, 30cps repeat)

  stop
  longmove(@par_keys, @dpin, 4)
  okay := cog := cognew(@entry, @par_tail) + 1


PUB stop

'' Stop keyboard driver - frees a cog

  if cog
    cogstop(cog~ -  1)
  longfill(@par_tail, 0, 19)


PUB present : truefalse

'' Check if keyboard present - valid ~2s after start
'' returns t|f

  truefalse := -par_present


PUB key : keycode

'' Get key (never waits)
'' returns key (0 if buffer empty)

  if par_tail <> par_head
    keycode := par_keys.word[par_tail]
    par_tail := ++par_tail & $F


PUB getkey : keycode

'' Get next key (may wait for keypress)
'' returns key

  repeat until (keycode := key)


PUB newkey : keycode

'' Clear buffer and get new key (always waits for keypress)
'' returns key

  par_tail := par_head
  keycode := getkey


PUB gotkey : truefalse

'' Check if any key in buffer
'' returns t|f

  truefalse := par_tail <> par_head


PUB clearkeys

'' Clear key buffer

  par_tail := par_head


PUB keystate(k) : state

'' Get the state of a particular key
'' returns t|f

  state := -(par_states[k >> 5] >> k & 1)


DAT

'******************************************
'* Assembly language PS/2 keyboard driver *
'******************************************

                        org
'
'
' Entry
'
entry                  movd    :par,#_dpin             'load input parameters _dpin/_cpin/_locks/_auto
                        mov     x,par
                        add     x,#11*4
                        mov     y,#4
:par                    rdlong  0,x
                        add     :par,dlsb
                        add     x,#4
                        djnz    y,#:par

                        mov     dmask,#1                'set pin masks
                        shl     dmask,_dpin
                        mov     cmask,#1
                        shl     cmask,_cpin

                        test    _dpin,#$20      wc      'modify port registers within code
                        muxc    _d1,dlsb
                        muxc    _d2,dlsb
                        muxc    _d3,#1
                        muxc    _d4,#1
                        test    _cpin,#$20      wc
                        muxc    _c1,dlsb
                        muxc    _c2,dlsb
                        muxc    _c3,#1

                        mov     _head,#0                'reset output parameter _head
'
'
' Reset keyboard
'
reset                   mov     dira,#0                 'reset directions
                        mov     dirb,#0

                        movd    :par,#_present          'reset output parameters _present/_states[8]
                        mov     x,#1+8
:par                    mov     0,#0
                        add     :par,dlsb
                        djnz    x,#:par

                        mov     stat,#8                 'set reset flag
'
'
' Update parameters
'
update                  movd    :par,#_head             'update output parameters _head/_present/_states[8]
                        mov     x,par
                        add     x,#1*4
                        mov     y,#1+1+8
:par                    wrlong  0,x
                        add     :par,dlsb
                        add     x,#4
                        djnz    y,#:par

                        test    stat,#8         wc      'if reset flag, transmit reset command
        if_c            mov     data,#$FF
        if_c            call    #transmit
'
'
' Get scancode
'
newcode                 mov     stat,#0                 'reset state

:same                   call    #receive                'receive byte from keyboard in data
'*******************************************************************************
'                        mov     vscl, base
'                        add     vscl, indx
'                        wrbyte  data, vscl
'                        add     indx, #1                ' next location
'                        and     indx, #$3F              ' limit to 0..32
'*******************************************************************************

                        cmp     data,#$83+1     wc      'if scancode? C flag is set if data < $83+1

        if_nc           cmp     data,#$AA       wz      'if powerup/reset? Z flag ist set if data equal $AA
        if_nc_and_z     jmp     #configure              'configure

        if_nc           cmp     data,#$E0       wz      'if extended scancode?
        if_nc_and_z     or      stat,#1                 'then stat=1
        if_nc_and_z     jmp     #:same                  'next code for $E0

        if_nc           cmp     data,#$F0       wz      'if released? (Breakcode)
        if_nc_and_z     or      stat,#2                 'then stat=2
        if_nc_and_z     jmp     #:same                  'next code

        if_nc           jmp     #newcode                'unknown, ignore, go to start
'
'
' Translate scancode and enter into buffer              'data < $83+1 (valid scancode)
'
                        test    stat,#1         wc      'lookup code with extended flag (first code ist $E0)
                        rcl     data,#1
                        mov     data_s,data             'keyboard-de: store scancode for next table lookup with shift
                        call    #look                   'look ASCII-Code

                        cmp     data,#0         wz      'if code unknown, ignore
        if_z            jmp     #newcode                'go to start

                        mov     t,_states+6             'remember lock keys in _states

                        mov     x,data                  'set/clear key bit in _states
                        shr     x,#5
                        add     x,#_states
                        movd    :reg,x
                        mov     y,#1
                        shl     y,data
                        test    stat,#2         wc
:reg                    muxnc   0,y

        if_nc           cmpsub  data,#$F0       wc      'if released or shift/ctrl/alt/win, done
        if_c            jmp     #update

                        mov     y,_states+7             'get shift/ctrl/alt/win bit pairs
                        shr     y,#16

                        cmpsub  data,#$E0       wc      'translate keypad, considering numlock
        if_c            test    _locks,#%100    wz
        if_c_and_z      add     data,#@keypad1-@table
        if_c_and_nz     add     data,#@keypad2-@table
        if_c            call    #look
        if_c            jmp     #:flags

                        'for keyboard-de changed #$DD to #lock
                        'in next code segment

                        cmpsub  data,#lock      wc      'handle scrlock/capslock/numlock
        if_c            mov     x,#%001_000
        if_c            shl     x,data
        if_c            andn    x,_locks
        if_c            shr     x,#3
        if_c            shr     t,#29                   'ignore auto-repeat
        if_c            andn    x,t             wz
        if_c            xor     _locks,x
        if_c            add     data,#lock
        if_c_and_nz     or      stat,#4                 'if change, set configure flag to update leds


{{ for keyboard-de start }}

                        cmp     data,#de_ae     wz      'replace ae
        if_z            mov     data,#$E4
                        cmp     data,#de_oe     wz      'replace oe
        if_z            mov     data,#$F6
                        cmp     data,#de_ue     wz      'replace ue
        if_z            mov     data,#$FC

'
'Documentation of control-key bits
'
'                       test    y,#%00000011    wz      'get SHIFT  into nz
'                       test    y,#%00000100    wz      'get CTRL-L into nz
'                       test    y,#%00001000    wz      'get CTRL-R into nz
'                       test    y,#%00010000    wz      'get ALT-L  into nz
'                       test    y,#%00100000    wz      'get ALT-R  into nz
'                       test    y,#%01000000    wz      'get WIN-L  into nz
'                       test    y,#%10000000    wz      'get WIN-R into nz


'
'Translate scan-codes with characters from "table_shift"
'

                        test    y,#%00000011    wz      'get shift into nz
                        test    _locks,#$40     wc
        if_nz_and_nc    mov     data,data_s             'reload scancode
        if_nz_and_nc    call    #look_shift             'translate by table_shift

'
'Translate scan-codes with characters from "table_alt_r"
'

                        test    y,#%00100000    wz      'get ALT-R (AltGr) into nz
        if_nz           mov     data,data_s             'reload scancode
        if_nz           call    #look_alt_r             'translate by table_alt_r


{{ for keyboard-de end }}


:flags                  ror     data,#8                 'add shift/ctrl/alt/win flags
                        mov     x,#4                    '+$100 if shift
:loop                   test    y,#%11          wz      '+$200 if ctrl
                        shr     y,#2                    '+$400 if alt
        if_nz           or      data,#1                 '+$800 if win
                        ror     data,#1
                        djnz    x,#:loop
                        rol     data,#12

                        rdlong  x,par                   'if room in buffer and key valid, enter
                        sub     x,#1
                        and     x,#$F
                        cmp     x,_head         wz
        if_nz           test    data,#$FF       wz
        if_nz           mov     x,par
        if_nz           add     x,#11*4
        if_nz           add     x,_head
        if_nz           add     x,_head
        if_nz           wrword  data,x
        if_nz           add     _head,#1
        if_nz           and     _head,#$F

                        test    stat,#4         wc      'if not configure flag, done
        if_nc           jmp     #update                 'else configure to update leds
'
'
' Configure keyboard
'
configure               mov     data,#$F3               'set keyboard auto-repeat
                        call    #transmit
                        mov     data,_auto
                        and     data,#%11_11111
                        call    #transmit

                        mov     data,#$ED               'set keyboard lock-leds
                        call    #transmit
                        mov     data,_locks
                        rev     data,#-3 & $1F
                        test    data,#%100      wc
                        rcl     data,#1
                        and     data,#%111
                        call    #transmit

                        mov     x,_locks                'insert locks into _states
                        and     x,#%111
                        shl     _states+7,#3
                        or      _states+7,x
                        ror     _states+7,#3

                        mov     _present,#1             'set _present

                        jmp     #update                 'done

{{ for keyboard-de start }}
'
' Lookup byte in table_shift
'
look_shift              ror     data,#2                 'perform lookup
                        movs    :reg,data
                        add     :reg,#table_shift
                        shr     data,#27
                        mov     x,data
:reg                    mov     data,0
                        shr     data,x
                        and     data,#$FF               'isolate byte
look_shift_ret          ret

'
' Lookup byte in table_alt_r
'
look_alt_r              ror     data,#2                 'perform lookup
                        movs    :reg,data
                        add     :reg,#table_alt_r
                        shr     data,#27
                        mov     x,data
:reg                    mov     data,0
                        shr     data,x
                        and     data,#$FF               'isolate byte
look_alt_r_ret          ret

{{ for keyboard-de end }}

'
'
' Lookup byte in table
'
look                    ror     data,#2                 'perform lookup
                        movs    :reg,data
                        add     :reg,#table
                        shr     data,#27
                        mov     x,data
:reg                    mov     data,0
                        shr     data,x

                        jmp     #rand                   'isolate byte
'
'
' Transmit byte to keyboard
'
transmit
_c1                     or      dira,cmask              'pull clock low
                        movs    napshr,#13              'hold clock for ~128us (must be >100us)
                        call    #nap
_d1                     or      dira,dmask              'pull data low
                        movs    napshr,#18              'hold data for ~4us
                        call    #nap
_c2                     xor     dira,cmask              'release clock

                        test    data,#$0FF      wc      'append parity and stop bits to byte
                        muxnc   data,#$100
                        or      data,dlsb

                        mov     x,#10                   'ready 10 bits
transmit_bit            call    #wait_c0                'wait until clock low
                        shr     data,#1         wc      'output data bit
_d2                     muxnc   dira,dmask
                        mov     wcond,c1                'wait until clock high
                        call    #wait
                        djnz    x,#transmit_bit         'another bit?

                        mov     wcond,c0d0              'wait until clock and data low
                        call    #wait
                        mov     wcond,c1d1              'wait until clock and data high
                        call    #wait

                        call    #receive_ack            'receive ack byte with timed wait
                        cmp     data,#$FA       wz      'if ack error, reset keyboard
        if_nz           jmp     #reset

transmit_ret            ret
'
'
' Receive byte from keyboard
'
receive                 test    _cpin,#$20      wc      'wait indefinitely for initial clock low
                        waitpne cmask,cmask
receive_ack
                        mov     x,#11                   'ready 11 bits
receive_bit             call    #wait_c0                'wait until clock low
                        movs    napshr,#16              'pause ~16us
                        call    #nap
_d3                     test    dmask,ina       wc      'input data bit
                        rcr     data,#1
                        mov     wcond,c1                'wait until clock high
                        call    #wait
                        djnz    x,#receive_bit          'another bit?

                        shr     data,#22                'align byte
                        test    data,#$1FF      wc      'if parity error, reset keyboard
        if_nc           jmp     #reset
rand                    and     data,#$FF               'isolate byte

look_ret
receive_ack_ret
receive_ret             ret
'
'
' Wait for clock/data to be in required state(s)
'
wait_c0                 mov     wcond,c0                '(wait until clock low)

wait                    mov     y,tenms                 'set timeout to 10ms

wloop                   movs    napshr,#18              'nap ~4us
                        call    #nap
_c3                     test    cmask,ina       wc      'check required state(s)
_d4                     test    dmask,ina       wz      'loop until got state(s) or timeout
wcond   if_never        djnz    y,#wloop                '(replaced with c0/c1/c0d0/c1d1)

                        tjz     y,#reset                'if timeout, reset keyboard
wait_ret
wait_c0_ret             ret


c0      if_c            djnz    y,#wloop                '(if_never replacements)
c1      if_nc           djnz    y,#wloop
c0d0    if_c_or_nz      djnz    y,#wloop
c1d1    if_nc_or_z      djnz    y,#wloop
'
'
' Nap
'
nap                     rdlong  t,#0                    'get clkfreq
napshr                  shr     t,#18/16/13             'shr scales time
                        min     t,#3                    'ensure waitcnt won't snag
                        add     t,cnt                   'add cnt to time
                        waitcnt t,#0                    'wait until time elapses (nap)

nap_ret                 ret
'
'
' Initialized data
'
'
dlsb                    long    1 << 9
tenms                   long    10_000 / 4
'*******************************************************************************
'base                    long    $7FC0
'indx                    long    0
'*******************************************************************************
'
'
' Lookup table
'                               ascii   scan    extkey  regkey  ()=keypad
'

{{keyboard-de start}}

table
                        '
                        '$00  ---   F9    ---   F5    F3    F1    F2    F12
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$00D8,$0000,$00D4,$00D2,$00D0,$00D1,$00DB

                        '
                        '$08  ---   F10   F8    F6    F4    TAB   _^_   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$00D9,$00D7,$00D5,$00D3,$0009,$005E,$0000

                        '          ALT-R Left        CTRL-R
                        '$10  ---  ALT-L SHIFT  ---  CTRL_L  q    1   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$F5F4,$00F0,$0000,$F3F2,$0071,$0031,$0000

                        '                                              WIN-L
                        '$18  ---   ---   _y_    s     a     w     2    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$0079,$0073,$0061,$0077,$0032,$F600

                        '                                              WIN-R
                        '$20  ---    c     x     d     e     4     3    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0063,$0078,$0064,$0065,$0034,$0033,$F700

                        '                                              Apps
                        '$28  ---   Spc    v     f     t     r     5    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0020,$0076,$0066,$0074,$0072,$0035,Apps

                        '                                              Power
                        '$30  ---    n     b     h     g    _z_    6    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$006E,$0062,$0068,$0067,$007A,$0036,Power

                        '                                              Sleep
                        '$38  ---   ---    m     j     u     7     8    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$006D,$006A,$0075,$0037,$0038,Sleep

                        '
                        '$40  ---    ,     k     i     o     0     9    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$002C,$006B,$0069,$006F,$0030,$0039,$0000

                        '                 (/)
                        '$48  ---    .    _-_    l   _oe_    p   _sz_   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$002E,$EF2D,$006C,de_oe,$0070,$00DF,$0000

                        '
                        '$50  ---   ---  _ae_   ---  _ue_   _'_   ---   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,de_ae,$0000,de_ue,$0060,$0000,$0000

                        '    CAPS  Right (ENTER)                 Wk.up
                        '$58 LOCK  SHIFT ENTER  _+_   ---    #    ---   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word CpsLk,$00F1,$EB0D,$002B,$0000,$0023,WkUp ,$0000

                        '
                        '$60  ---   _<_   ---   ---   ---   ---  BkSpc  ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$003C,$0000,$0000,$0000,$0000,BkSp ,$0000

                        '           End        Left  Home
                        '$68  ---   (1)   ---   (4)   (7)   ---   ---   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,CrsEn,$0000,CrsLt,CrsHm,$0000,$0000,$0000

                        '     Ins   Del  Down   ---  Right  Up
                        '$70  (0)   (.)   (2)   (5)   (6)   (8)   Esc  NumLock
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word  Ins , Del ,CrsDn,$00E5,CrsRt,CrsUp, Esc ,NumLk

                        '                PgDn        PrScr PgUp
                        '$78  F11   (+)   (3)   (-)   (*)   (9)  ScrLock ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $00DA,$00EC,PgDn ,$00ED,$DCEE,PgUp ,ScrLk,$0000

                        '
                        '$80  ---   ---   ---   F7
                        '    ===== ===== ===== =====
                        word $0000,$0000,$0000,$00D6


keypad1                 byte    $CA, $C5, $C3, $C7, $C0, 0, $C1, $C4, $C2, $C6, $C9, $0D, "+-*/"

keypad2                 byte    "0123456789.", $0D, "+-*/"


table_shift
                        '
                        '$00  ---   F9    ---   F5    F3    F1    F2    F12
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$00D8,$0000,$00D4,$00D2,$00D0,$00D1,$00DB

                        '
                        '$08  ---   F10   F8    F6    F4    TAB   _°_   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$00D9,$00D7,$00D5,$00D3,$0009,$00B0,$0000

                        '          ALT-R Left        CTRL-R
                        '$10  ---  ALT-L SHIFT  ---  CTRL_L  Q     !    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$F5F4,$00F0,$0000,$F3F2,$0051,$0021,$0000

                        '                                              WIN-L
                        '$18  ---   ---   _Y_    S     A     W     "    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$0059,$0053,$0041,$0057,$0022,$F600

                        '                                              WIN-R
                        '$20  ---    C     X     D     E     $     §   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0043,$0058,$0044,$0045,$0024,$00A7,$F700

                        '                                              Apps
                        '$28  ---   Spc    V     F     T     R     %    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0020,$0056,$0046,$0054,$0052,$0025,Apps

                        '                                              Power
                        '$30  ---    N     B     H     G    _Z_    &    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$004E,$0042,$0048,$0047,$005A,$0026,Power

                        '                                              Sleep
                        '$38  ---   ---    M     J     U     /     (    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$004D,$004A,$0055,$002F,$0028,Sleep

                        '
                        '$40  ---    ;     K     I     O    _=_    )    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$003B,$004B,$0049,$004F,$003D,$0029,$0000

                        '                 (/)
                        '$48  ---    :    ___    L   _OE_    P   _sz_   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$003A,$EF5F,$004C,$00D6,$0050,$003F,$0000

                        '
                        '$50  ---   ---  _AE_   ---  _UE_   _'_   ---   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$00C4,$0000,$00DC,$0060,$0000,$0000

                        '    CAPS  Right (ENTER)                 Wk.up
                        '$58 LOCK  SHIFT ENTER  _*_   ---   _'_   ---   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word CpsLk,$00F1,$EB0D,$002A,$0000,$0027,WkUp ,$0000

                        '
                        '$60  ---   _>_   ---   ---   ---   ---  BkSpc  ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$003E,$0000,$0000,$0000,$0000,BkSp ,$0000

                        '           End        Left  Home
                        '$68  ---   (1)   ---   (4)   (7)   ---   ---   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,CrsEn,$0000,CrsLt,CrsHm,$0000,$0000,$0000

                        '     Ins   Del  Down   ---  Right  Up
                        '$70  (0)   (.)   (2)   (5)   (6)   (8)   Esc  NumLock
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word  Ins , Del ,CrsDn,$00E5,CrsRt,CrsUp, Esc ,NumLk

                        '                PgDn        PrScr PgUp
                        '$78  F11   (+)   (3)   (-)   (*)   (9)  ScrLock ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $00DA,$00EC,PgDn ,$00ED,$DCEE,PgUp ,ScrLk,$0000

                        '
                        '$80  ---   ---   ---   F7
                        '    ===== ===== ===== =====
                        word $0000,$0000,$0000,$00D6



table_alt_r
                        '
                        '$00  ---   F9    ---   F5    F3    F1    F2    F12
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0097,$0000,$0090,$009D,$009F,$009E,$0094

                        '
                        '$08  ---   F10   F8    F6    F4    TAB   _^_   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0096,$0093,$0091,$009C,$0009,$0000,$0000

                        '          ALT-R Left        CTRL-R
                        '$10  ---  ALT-L SHIFT  ---  CTRL_L  q    1   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$F5F4,$00F0,$0000,$F3F2, "@",  "¹", $0000

                        '                                              WIN-L
                        '$18  ---   ---   _y_    s     a     w     2    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$0000,$0000,$0000,$0000, "²", $F600

                        '                                              WIN-R
                        '$20  ---    c     x     d     e     4     3    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$0000,$0000, "€", $0000, "³", $F700

                        '                                              Apps
                        '$28  ---   Spc    v     f     t     r     5    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$0000,$0000,$0000,$0000,$0000,Apps

                        '                                              Power
                        '$30  ---    n     b     h     g    _z_    6    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$0000,$0000,$0000,$0000,$0000,Power

                        '                                              Sleep
                        '$38  ---   ---    m     j     u     7     8    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000, "µ", $0000,$0000, "{" , "[" ,Sleep

                        '
                        '$40  ---    ,     k     i     o     0     9    ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$0000,$0000,$0000, "}",  "]", $0000

                        '                 (/)
                        '$48  ---    .    _-_    l   _oe_    p   _sz_   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$0000,$0000,$0000,$0000, "\", $0000

                        '
                        '$50  ---   ---  _ae_   ---  _ue_   _'_   ---   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000

                        '    CAPS  Right (ENTER)                 Wk.up
                        '$58 LOCK  SHIFT ENTER  _+_   ---    #    ---   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word CpsLk,$00F1,$EB0D, "~", $0000,$0000,WkUp ,$0000

                        '
                        '$60  ---   _<_   ---   ---   ---   ---  BkSpc  ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000, "|", $0000,$0000,$0000,$0000,BkSp ,$0000

                        '           End        Left  Home
                        '$68  ---   (1)   ---   (4)   (7)   ---   ---   ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0000,CrsEn,$0000,CrsLt,CrsHm,$0000,$0000,$0000

                        '     Ins   Del  Down   ---  Right  Up
                        '$70  (0)   (.)   (2)   (5)   (6)   (8)   Esc  NumLock
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word  Ins , Del ,CrsDn,$00E5,CrsRt,CrsUp, Esc ,NumLk

                        '                PgDn        PrScr PgUp
                        '$78  F11   (+)   (3)   (-)   (*)   (9)  ScrLock ---
                        '    ===== ===== ===== ===== ===== ===== ===== =====
                        word $0095,$00EC,PgDn ,$00ED,$DCEE,PgUp ,ScrLk,$0000

                        '
                        '$80  ---   ---   ---   F7
                        '    ===== ===== ===== =====
                        word $0000,$0000,$0000,$0092


{{keyboard-de end}}

'
'
' Uninitialized data
'
dmask                   res     1
cmask                   res     1
stat                    res     1
data                    res     1
data_s                  res     1       'Scancode-Storage (for keyboard-de)
x                       res     1
y                       res     1
t                       res     1

_head                   res     1       'write-only
_present                res     1       'write-only
_states                 res     8       'write-only
_dpin                   res     1       'read-only at start
_cpin                   res     1       'read-only at start
_locks                  res     1       'read-only at start
_auto                   res     1       'read-only at start

''
''
''      _________
''      Key Codes
''
''      00..DF  = keypress and keystate
''      E0..FF  = keystate only
''
''
''      09      Tab
''      0D      Enter
''      20      Space
''      21      !
''      22      "
''      23      #
''      24      $
''      25      %
''      26      &
''      27      '
''      28      (
''      29      )
''      2A      *
''      2B      +
''      2C      ,
''      2D      -
''      2E      .
''      2F      /
''      30      0..9
''      3A      :
''      3B      ;
''      3C      <
''      3D      =
''      3E      >
''      3F      ?
''      40      @
''      41..5A  A..Z
''      5B      [
''      5C      \
''      5D      ]
''      5E      ^
''      5F      _
''      60      `
''      61..7A  a..z
''      7B      {
''      7C      |
''      7D      }
''      7E      ~
''
''      80-BF   (future international character support)
''
''      C0      Left Arrow
''      C1      Right Arrow
''      C2      Up Arrow
''      C3      Down Arrow
''      C4      Home
''      C5      End
''      C6      Page Up
''      C7      Page Down
''      C8      Backspace
''      C9      Delete
''      CA      Insert
''      CB      Esc
''      CC      Apps
''      CD      Power
''      CE      Sleep
''      CF      Wakeup
''
''      D0..DB  F1..F12
''      DC      Print Screen
''      DD      Scroll Lock
''      DE      Caps Lock
''      DF      Num Lock
''
''      E0..E9  Keypad 0..9
''      EA      Keypad .
''      EB      Keypad Enter
''      EC      Keypad +
''      ED      Keypad -
''      EE      Keypad *
''      EF      Keypad /
''
''      F0      Left Shift
''      F1      Right Shift
''      F2      Left Ctrl
''      F3      Right Ctrl
''      F4      Left Alt
''      F5      Right Alt
''      F6      Left Win
''      F7      Right Win
''
''      FD      Scroll Lock State
''      FE      Caps Lock State
''      FF      Num Lock State
''
''      +100    if Shift
''      +200    if Ctrl
''      +400    if Alt
''      +800    if Win
''
''      eg. Ctrl-Alt-Delete = $6C9
''
''
'' Note: Driver will buffer up to 15 keystrokes, then ignore overflow.

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