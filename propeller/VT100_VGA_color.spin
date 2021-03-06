'' PropCOMM VT100_VGA V2.0b Original By Jeff Ledger

{{
Info            : VT100 Terminal for AVR CP/M
Autor           : Joe G.
Version         : 1.7.7
Subversion      : 03
Funktion        : Keyboard Driver DE, VGA-640x480 Driver, V24 Driver, VT100 Terminal
Komponenten     : VGA_640
                  Keyb-DE
                  FullDuplexSerial
                  VGA_HiRes_Text_Color

Log

25.03.2013      Start
                V24 Buffer 256 Byte
                Key-DE
26.03.2013      ESC Code Terminal Reset
10.04.2013      new codes
13.04.2013      BS
14.04.2013      ESC [ Y;X H
26.04.2013      arrow Keys, cursor (not) visible, colormanagement
25.05.2013      Bell, CP/M Reset
26.05.2013      all VT100 colors, hot key Alt+i (toggle inverse)
02.01.2014      makes Cursor visible by Reset (F1 und ESC c), correct storage location in the Rx&Tx buffer
03.01.2014      with German letters Ä,Ö,Ü,ä,ö,ü,ß and ESC[H, ESC[f, bug in ESC[M
19.01.2014      second character font
28.01.2014      new VGA Mode 640x480 full color
27.03.2018      ALT+e Toggle ECHO + US Keyboard Layout
05.03.2019      ALT+Home Reset all terminal settings to default

See file control-codes.md for a list of currently supported control codes.

}}

CON
  'clock configuration
  _clkmode  = xtal1 + pll16x
  _xinfreq  = 5_000_000         '5 MHz Quarz AVR CP/M

  'Hardware Variant
  #0, AVRCPM, VT100

  'pin configuration
  KEY_DATA    = 26                'Keyboard Data Pin
  KEY_CLK     = 27                'Keyboard CLK Pin
  VIDEO       = 16                'Video output Pin(s)
  TXD         = 30                'Serial Transmit
  RXD         = 31                'Serial Receive

  'Only on VT100:
  RTS         = 0
  CTS         = 1
  DTR         = 4
  DCD         = 3
  DSR         = 2
  RING        = 5                 'not used

  PORTSWITCH  = 6                 'Communication Port Switch Pin
  BELL_VT100  = 7                 'Beep

  'Only on AVR-CP/M
  BELL_AVRCPM = 1                 'Beep
  CPM_RESET   = 0                 'CP/M Reset

' ISO-6429 color numbers
  #0, black,    red,  green,  yellow,  blue,  magenta,  cyan, gray
     darkgray, lred, lgreen, lyellow, lblue, lmagenta, lcyan, white

' Text attributes
  ATTR_BOLD       = text#ATTR_BOLD
  ATTR_INVERS     = text#ATTR_INVERS
  ATTR_UNDERLINE  = text#ATTR_UNDERLINE


'Control Characters (C0) Recognized by VT100 +

  NUL       = $00           'Ignored when received (not stored in input buffer) and used as a fill character.
  ETX       = $03           'Can be selected as a half-duplex turnaround character.
  EOT       = $04           'Can be selected as a disconnect character or half-duplex turnaround character. When used as a turnaround character, the disconnect character is DLE-EOT.
  ENQ       = $05           'Transmits answerback message.
  BEL       = $07           'Generates bell tone.
  BS        = $08           'Moves cursor to the left one character position; if cursor is at left margin, no action occurs.
  HT        = $09           'Moves cursor to next tab stop, or to right margin if there are no more tab stops.
  LF        = $0A           'Causes a linefeed or a new line operation. (See Linefeed/New Line). Also causes printing if auto print operation is selected.
  VT        = $0B           'Processed as LF.
  FF        = $0C           'Processed as LF. FF can also be selected as a half-duplex turnaround character.
  CR        = $0D           'Moves cursor to left margin on current line. CR can also be selected as a half-duplex turnaround character.
  SO        = $0E           'Selects G1 character set designated by a select character set sequence.
  SI        = $0F           'Selects G0 character set designated by a select character set sequence.
  DC1       = $11           'Processed as XON. DC1 causes terminal to continue transmitting characters.
  DC3       = $13           'Processed as XOFF. DC3 causes terminal to stop transmitting all characters except XOFF and XON. DC3 can also be selected as a half-duplex turnaround character.
  CAN       = $18           'If received during an escape or control sequence, cancels the sequence and displays substitution character ([]).
  SUBST     = $1A           'Processed as CAN.
  ESC       = $1B           'Processed as a sequence introducer.
  DEL       = $7F

'Control Characters (C1) Recognized by VT200 +

  IND       = $84           'ESC D    Index   Moves cursor down one line in same column. If cursor is at bottom margin, screen performs a scroll up.
  NEL       = $85           'ESC E    Next line   Moves cursor to first position on next line. If cursor is at bottom margin, screen performs a scroll up.
  HTS       = $88           'ESC H    Horizontal tab set  Sets one horizontal tab stop at the column where the cursor is.
  RI        = $8D           'ESC M    Reverse index   Moves cursor up one line in same column. If cursor is at top margin, screen performs a scroll down.
  SS2       = $8E           'ESC N    Single shift G2   Temporarily invokes G2 character set into GL for the next graphic character. G2 is designated by a select-character-set (SCS) sequence.
  SS3       = $8F           'ESC O    Single shift G3   Temporarily invokes G3 character set into GL for the next graphic character. G3 is designated by a select-character-set (SCS) sequence.
  DCS       = $90           'ESC P    Device control string   Processed as opening delimiter of a device control string for device control use.
  CSI       = $9B           'ESC [    Control sequence introducer   Processed as control sequence introducer.
  ST        = $9C           'ESC \    String terminator   Processed as closing delimiter of a string opened by DCS.


  bspKey            = $C8
  delKey            = $BA
  ArrowTop          = $C2
  ArrowDown         = $C3
  ArrowLeft         = $C0
  ArrowRight        = $C1

  escape            = $CB
  backspace         = 8
  fReturn           = 13
  fLinefeed         = 10
  fEof              = -1

  ' Terminal Operating States
  #0, S_ONLINE
  S_LOCAL
  S_SETUP
  S_ECHO

  'Communication Port
  #0, comV24, comUSB

DAT
  strVersion byte "VT100 Terminal V1.7.7", 0
  strV24 byte "V24",0
  strUSB byte "USB",0

  BaudTab long 300, 1200, 2400, 4800, 9600, 19200, 38400, 57600, 115200

  PortTab word @strV24, @strUSB

  CS_US   byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F
          byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F
          byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F
          byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F
          byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F
          byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,$7C,$7D,$7E,$7F

  CS_UK   byte $20,$21,$22,$1E,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F
          byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F
          byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F
          byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F
          byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F
          byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,$7C,$7D,$7E,$7F

  CS_DEC  byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F
          byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F
          byte $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F
          byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F
          byte $01,$7F,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$0F,$10
          byte $11,$12,$13,$14,$15,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F,$7F

  CS_DE   byte $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F
          byte $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F
          byte $1E,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F
          byte $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$03,$04,$05,$5E,$5F
          byte $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F
          byte $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$06,$09,$0A,$02,$7F

'                0   1   2   3   4   5   6   7   8   9  10  11  12  13  15  15
  CS_SUPP byte $20,$20,$20,$1E,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20  '10
          byte $07,$08,$20,$20,$20,$20,$20,$1F,$20,$20,$20,$20,$20,$20,$20,$20  '11
          byte $20,$20,$20,$20,$03,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20  '12
          byte $20,$20,$20,$20,$20,$20,$04,$20,$20,$20,$20,$20,$05,$20,$20,$02  '13
          byte $20,$20,$20,$20,$06,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20  '14
          byte $20,$20,$20,$20,$20,$20,$09,$20,$20,$20,$20,$20,$0A,$20,$20,$20  '15

Var
  word  attr_color                                  'low byte: color; high byte: other attributes (underline, invers)
  byte  ss_state                                    'single shift
  byte  gl_sel
  byte  gr_sel
  word  g_set[4]
  word  save_g_set[4]
  byte  terminal_state

  byte  variant
  byte  bell_pin


  byte  baud                                          'baud rate (index)
  byte  fg_color                                      'foreground color
  byte  bg_color                                      'background color
  byte  c_mode                                        'cursor mode
  byte  comport                                       'I/O port
  byte  font
  byte  online_stat

'Temp storage
  long  cfg[config#CfgSize]                           'configuration storage

OBJ
  text   : "VGA_640"                                  'Full Color VGA-640x480 Driver
  key    : "keyb-DE"                                  'new Keyboard Driver (german)
  ser    : "FullDuplexSerial"                         'Full Duplex Serial Controller
  config : "eeprom"                                   'I2C EEPROM access


PUB main | ch

  init                                                'initialization
  vt_reset                                            '(pre)set parameters
  titlescreen                                         'print startmessage

{
  repeat
    result := key.getkey
    Text.hex(result,3)
    Text.str(string(" : "))
    text.emit(result)
    text.cmd($0d)
    ser.tx(result)
}

  terminal_state := online_stat
  ser.dtr_set(0)
  vtparse_state := S_GROUND


  repeat
    ch := key.key

    if ch > 0
      if ch & $FFC0 == $0240                          'CRTL code
        ch &=  $001F                                  'redefine as CTRL+char
      if ch > 451 and ch < 477                        'Ä,Ü,Ö
        ch:=ch-256                                    'redefine as ASCII Letter Ä,Ü,Ö

      case ch

        $02DC:                                        'Ctrl+PrintScreen (should be Alt+PrintScreen)
          ser.dtr_set(1)
          setup                                       'Enter Setup
          if terminal_state == S_ONLINE
            ser.dtr_set(0)
          next

        $0469:                                        'Alt + i
          attr_color := bg_color<<4 + fg_color        'set default color
          text.attr_color(attr_color)
          beep                                        'beep
          next

        $0465:                                        'Alt + e
          if terminal_state == S_ONLINE               'Toggle Echo
           terminal_state := S_ECHO
          else
           terminal_state := S_ONLINE
          beep                                        'beep
          next

        $0406:                                        'Alt + Home | pos1
          terminal_state := S_LOCAL                   'switch to local parser
          key_send($1B)                               'ESC c (Reset all terminal settings to default)
          key_send($63)
          terminal_state := S_ONLINE                  'switch to serial port
          next

        $0407:                                        'Alt + End | ende
          terminal_state := S_LOCAL                   'switch to local parser
          key_send($1B)                               'ESC#9 (Show Font)
          key_send($23)
          key_send($39)
          terminal_state := S_ONLINE                  'switch to serial port
          next

        $06BA,$06C9:                                  'Ctrl-Alt-Delete
          if variant == AVRCPM
            do_cpm_reset                              'CP/M Reset
            next

        bspKey:
          key_send($08)                               'send BS

        delKey:                                       'send DEL
          key_send($7F)

        ArrowTop:                                     'send ESC [ A
          key_send($1B)
          key_send($5B)
          key_send($41)

        ArrowDown:
          key_send($1B)                               'send ESC [ B
          key_send($5B)
          key_send($42)

        ArrowRight:                                   'send ESC [ C
          key_send($1B)
          key_send($5B)
          key_send($43)

        ArrowLeft:                                    'send ESC [ D
          key_send($1B)
          key_send($5B)
          key_send($44)

        $0D:
          key_send($0D)
          if text.get_mode(text#C_LNM)                          'Newline Mode?
            key_send($0A)

        OTHER:                                        'send code
          key_send(ch & $ff)

    if (terminal_state == S_ONLINE) OR (terminal_state == S_ECHO)
      vt_parse(ser.rxcheck)                           'process received character



PRI key_send(char)

  case terminal_state
    S_ONLINE:
      ser.tx(char)
    S_LOCAL:
      vt_parse(char)
    S_ECHO:
      ser.tx(char)
      vt_parse(char)


PRI init

  dira[PORTSWITCH] := 0                                 'test portswitch pin
  if (variant := ina[PORTSWITCH]) == AVRCPM
    dira[CPM_RESET] := 0                                'input (open drain)
    bell_pin := BELL_AVRCPM
  else
    bell_pin := BELL_VT100

  dira[bell_pin] := 1                                   'output

  config.initialize                                     'initialize EEPROM

  if config.readCfg(@cfg) == 0                          'read EEPROM, is config zero, set default config
     cfg[0] := 8                                        'baud
     cfg[1] := green                                    'foreground color
     cfg[2] := black                                    'background color
     cfg[3] := 6                                        'cursor mode
     cfg[4] := comUSB                                   'switch port, EN is high (select FTDI, deselect MAX3241)
     cfg[5] := 0                                        'font 0
     cfg[6] := S_ONLINE


  baud := cfg[0]                                        'baud rate is default (115200)
  fg_color := cfg[1]                                    'foreground color is default (dgreen)
  bg_color := cfg[2]                                    'background color is default (black)
  c_mode := cfg[3]                                      'cursor mode is default 6 (underscore, cursor on, blink slow)
  comport := cfg[4]                                     'port 1 is default (USB)
  font := cfg[5]
  online_stat := cfg[6]


  outa[PORTSWITCH] := comport                           'set portswitch to configured state
  dira[PORTSWITCH] := 1                                 '  ..and output it
  attr_color := bg_color<<4 + fg_color                  'default color

  text.start(VIDEO)                                     'start terminal screen
  key.start(KEY_DATA,KEY_CLK)                           'start new keyboard driver
  if variant == VT100
    ser.start(RXD,TXD,RTS,CTS,DTR,DCD,DSR,0,BaudTab[baud])'start host communication
  else
    ser.start(RXD,TXD,-1,-1,-1,-1,-1,0,BaudTab[baud])'start host communication
  text.cursor_set(c_mode)                               'set cursor mode
  text.set_font(font)



PRI titlescreen
  beep
  text.str(@strVersion)
  text.newline
  text.dec(BaudTab[baud])
  text.str(string(",8,N,1"))
  if variant == VT100
    text.str(string(" | Port: "))
    text.str(@@PortTab[comport])
  text.newline
  text.str(string("Terminal Ready"))
  text.newline

PRI vt_reset
  g_set[0]  := @CS_US
  g_set[1]  := @CS_DEC
  g_set[2]  := @CS_SUPP
  g_set[3]  := @CS_US
  gl_sel := 0
  gr_sel := 2
  wordmove(@save_g_set, @g_set, 4)

  text.cmd(text#C_DECAWM+1)                             'Autowrap on
  text.cmd(text#C_DECTECM+1)                            'makes cursor visible
  attr_color := bg_color<<4 + fg_color                  'default color
  text.attr_color(attr_color)
  text.ed(2)
  text.cmd(text#C_DECOM+0)                      'origin mode absolute
  text.decstbm(0,0)                             'scrolling region, 0,0 == full screen

PRI beep
' TODO: beep in background (extra cog or timer)

  outa[bell_pin] := 1                           'set bell high
  waitcnt(clkfreq/8 + cnt)                      'wait a moment (125ms)
  outa[bell_pin] := 0                           'set bell low


PRI do_cpm_reset

  outa[CPM_RESET] := 0                          'set CP/M reset low (just in case)
  dira[CPM_RESET] := 1                          'output (open drain active)
  waitcnt(clkfreq/128 + cnt)                    'wait a moment (7.8ms)
  dira[CPM_RESET] := 0                          'input (inactive)


'' ===================================== Terminal Input Parser =====================================
{{

See http://www.vt100.net/emu/dec_ansi_parser for details.


State           Event                 Next State      Action
--------------------------------------------------------------------
'anywhere'      00-17,19,1C-1F        -               execute
                18, 1A                ground          execute
                80-8F,91-97,99,9A     ground          execute
                9C                    ground          -
                1B                    escape          clear_esc
--------------------------------------------------------------------
ground          20-7F                 -               print
--------------------------------------------------------------------
escape          7F                    -               ignore
                20-2F                 esc_interm      collect
                5B                    csi_entry       clear_csi
                30-5A,5C-7E           ground          esc_dispatch
--------------------------------------------------------------------
esc_interm      7F                    -               ignore
                20-2F                 -               collect
                30-7E                 ground          esc_dispatch
--------------------------------------------------------------------
csi_entry       7F                    -               ignore
                20-2F                 csi_interm      collect
                3A                    csi_ignore      -
                30-39,3B              csi_param       csi_param
                3C-3F                 csi_param       collect
                40-7E                 ground          csi_dispatch
--------------------------------------------------------------------
csi_interm      7F                    -               ignore
                20-2F                 -               collect
                30-3F                 csi_ignore      -
                40-7E                 ground          csi_dispatch
--------------------------------------------------------------------
csi_ignore      20-3F,7F              -               ignore
                40-7E                 ground          -
--------------------------------------------------------------------
csi_param       7F                    -               ignore
                20-2F                 csi_interm      collect
                30-39,3B              -               csi_param
                3A,3C-3F              csi_ignore      -
                40-7E                 ground          csi_dispatch
--------------------------------------------------------------------

}}

CON

  CSI_PAR_MAX   = 10                                    'Maximum number of control sequence parameters

  ' Input Parser States
  #0, S_GROUND
  S_ESCAPE
  S_ESCAPE_INTERMEDIATE
  S_CSI_ENTRY
  S_CSI_INTERMEDIATE
  S_CSI_IGNORE
  S_CSI_PARAM

VAR

  ' Input parser state variables
  long   vtparse_state
  long   intermediate_char
  long   csi_par_cnt
  byte   csi_par[CSI_PAR_MAX]


PRI vt_parse(ch)

  if  ch < 0
    return

  if (ch & $7F) < $20
    'Process control characters immediately. (everywhere pseudostate)
    case ch
      ' C0 characters

      'ENQ:                                             'ENQ: Transmits answerback message.
        'TODO
      BEL:                                              'BEL: Generates bell tone.
        beep
      BS:                                               'BS: Moves cursor to the left one character position; if cursor is at left margin, no action occurs.
        text.cmd(text#C_CUB)
                                                        'HT: Moves cursor to next tab stop, or to right margin if there are no more tab stops.
      HT:
        text.cmd(text#C_HT)                             'CR: Moves cursor to left margin on current line.
      CR:
        text.cmd(text#C_CR)
                                                        'LF: Causes a linefeed or a new line operation. (See Linefeed/New Line).
                                                        'VT: Processed as LF.
                                                        'FF: Processed as LF.
      LF, VT, FF:
        text.cmd(text#C_IND)

      SO:                                               'Selects G1 character set designated by a select character set sequence.
        gl_sel := 1
      SI:                                               'Selects G0 character set designated by a select character set sequence.
        gl_sel := 0

      'DC1                                              'Processed as XON. DC1 causes terminal to continue transmitting characters.
      'DC3                                              'Processed as XOFF. DC3 causes terminal to stop transmitting all characters except XOFF and XON. DC3 can also be selected as a half-duplex turnaround character.
        'TODO

                                                        'CAN: If received during an escape or control sequence, cancels the sequence and displays substitution character ([]).
                                                        'SUB: Processed as CAN.
      CAN, SUBST:
        'TODO: text.emit(**subst char**)
        vtparse_state := S_GROUND

      ESC:                                              'ESC: Processed as a sequence introducer.
        'clear_esc
        intermediate_char := 0
        vtparse_state := S_ESCAPE

      ' C1 characters

      'IND                                              'ESC D     Index
      IND:
        text.cmd(text#C_IND)
        vtparse_state := S_GROUND
      'NEL                                              'ESC E     Next line
      NEL:
        text.cmd(text#C_CR)
        text.cmd(text#C_IND)
        vtparse_state := S_GROUND
      'HTS                                              'ESC H     Horizontal tab set
      HTS:
        'TODO?
        vtparse_state := S_GROUND
      'RI                                               'ESC M     Reverse index
      RI:
        text.cmd(text#C_RI)
        vtparse_state := S_GROUND
      'SS2                                              'ESC N     Single shift G2
      SS2:
        'TODO:
        vtparse_state := S_GROUND
      'SS3                                              'ESC O     Single shift G3
      SS2:
        'TODO:
        vtparse_state := S_GROUND
      'DCS                                              'ESC P     Device control string
      DCS:
        'TODO:
        vtparse_state := S_GROUND
      'CSI                                              'ESC [     Control sequence introducer
      CSI:
        csi_par_cnt := 0
        csi_par[0] := 0
        vtparse_state := S_CSI_ENTRY
      'ST                                               'ESC \     String terminator
      ST:
        vtparse_state := S_GROUND
      other:
        if ch => $80
          vtparse_state := S_GROUND

  else

    case vtparse_state
      S_GROUND:
        'print(ch)
        if ch < $80
          text.emit(byte [g_set[gl_sel]][ch-$20])
        else
          text.emit(byte [g_set[gr_sel]][ch-$A0])

      S_ESCAPE:
        case ch
          DEL:
              'ignore

          $20..$2F:                                     '" ".."/"
              collect(ch)
              vtparse_state := S_ESCAPE_INTERMEDIATE

          $5B:                                          '"["
              'clear csi
              csi_par_cnt := 0
              csi_par[0] := 0
              vtparse_state := S_CSI_ENTRY

          other:
              esc_dispatch(ch)
              vtparse_state := S_GROUND

      S_ESCAPE_INTERMEDIATE:
        case ch
          DEL:
              'ignore

          $20..$2F:                                     '" ".."/"
              collect(ch)
              vtparse_state := S_CSI_INTERMEDIATE

          other:
              esc_dispatch(ch)
              vtparse_state := S_GROUND

      S_CSI_ENTRY:
        case ch
          DEL:
              'ignore

          $20..$2F:                                     '" ".."/"
              collect(ch)

          $3A:                                          '":"
              vtparse_state := S_CSI_IGNORE

          $30..$39,$3B:                                 '"0".."9",";"
              param(ch)
              vtparse_state := S_CSI_PARAM

          $3C..$3F:                                     '"<".."?"
              collect(ch)
              vtparse_state := S_CSI_PARAM

          other:
              csi_dispatch(ch)
              vtparse_state := S_GROUND

      S_CSI_INTERMEDIATE:
        case ch
          DEL:
              'ignore

          $20..$2F:                                     '" ".."/"
              collect(ch)

          $30..$3F:                                     '"0".."?"
              vtparse_state := S_CSI_IGNORE

          other:
              csi_dispatch(ch)
              vtparse_state := S_GROUND

      S_CSI_IGNORE:
        case ch
          $20..$3F,DEL:                                 '" ".."?",DEL
              'ignore

          other:
              vtparse_state := S_GROUND

      S_CSI_PARAM:
        case ch
          DEL:
              'ignore

          $20..$2F:                                     '" ".."/"
              collect(ch)
              vtparse_state := S_CSI_INTERMEDIATE

          $3A,$3C..$3F:                                 '":","<".."?"
              vtparse_state := S_CSI_IGNORE

          $30..$39,$3B:                                 '"0".."9",";"
              param(ch)

          other:
              csi_dispatch(ch)
              vtparse_state := S_GROUND



PRI collect (ch)
   if intermediate_char == 0                            'Store only one (the first) intermediate char
      intermediate_char := ch


PRI param (ch)
   if csi_par_cnt < CSI_PAR_MAX
      if ch == ";"
         csi_par_cnt++
         if csi_par_cnt < CSI_PAR_MAX
            csi_par[csi_par_cnt] := 0
      else
         csi_par[csi_par_cnt] := csi_par[csi_par_cnt] * 10 + ch - "0"


PRI get_param (index)
  if index > csi_par_cnt
    return 0
  return csi_par[index]


PRI esc_dispatch (ch)

  case intermediate_char
    0:
      case ch
        "c":                                            'RIS    Reset to Initial State (Power Up)
          vt_reset
          titlescreen
        "~":                                            'LS1R   Invoke G1 into GR (VT200 mode only).
          gr_sel := 1
        "n":                                            'LS2    Invoke G2 into GL (VT200 mode only).
          gl_sel := 2
        "}":                                            'LS2R   Invoke G2 into GR (default) (VT200 mode only).
          gr_sel := 2
        "o":                                            'LS3    Invoke G3 into GL (VT200 mode only).
          gl_sel := 3
        "|":                                            'LS3R   Invoke G3 into GR (VT200 mode only).
          gr_sel := 3
        "D":                                            'IND    Linefeed.
          text.cmd(text#C_IND)
        "E":                                            'NEL    Newline.
          text.cmd(text#C_CR)
          text.cmd(text#C_IND)
        "H":                                            'HTS    Set tab stop at current column.
          'TODO?
        "M":                                            'RI     Reverse linefeed. (Curser up)
          text.cmd(text#C_RI)
        "N":                                            'SS2    Single shift 2
          'TODO:
        "O":                                            'SS3    Single shift 3
          'TODO:
        "Z":                                            'DECID  DEC private identification. The terminal returns  ESC [ ? 6 c, claiming that it is a VT102.
          tx_csi(string("?6c"))
        "7":                                            'DECSC  Save current state (cursor coordinates, attributes, character sets pointed at by G0, G1).
          text.cmd(text#C_DECSC)
          wordmove(@save_g_set, @g_set, 4)
        "8":                                            'DECRC  Restore state most recently saved by ESC 7.
          text.cmd(text#C_DECRC)
          wordmove(@g_set, @save_g_set, 4)

    "(", ")", "*", "+":                                 'G0..G3 designator
      result := lookdownz(intermediate_char: "(", ")", "*", "+")
      case ch
        "A":                                            'UK
          g_set[result] := @CS_UK
        "B":                                            'US
          g_set[result] := @CS_US
        "K":                                            'DE
          g_set[result] := @CS_DE
        "<":                                            'DEC supplemental
          g_set[result] := @CS_SUPP
        "0":                                            'DEC special characters and line drawing set
          g_set[result] := @CS_DEC
        '"1":                                           'Alternate character ROM
        '"2":                                           'Alternate character ROM - special characters
          'TODO:
    "#":
      case ch
        "8":                                            'DECALN   DEC screen alignment test - fill screen with E's.
            Text.decaln
        "9":                                            '
            Text.showfont
    '"?":                                                'Private functions


PRI csi_dispatch (ch) | i

  case intermediate_char
    0:
      case ch
        "@":                                            'ICH    Insert character(s) (VT220)
          text.ich(get_param(0))
        "A":                                            'CUU    Move cursor up
          repeat (get_param(0) #>1)
            text.cmd(text#C_CUU)

        "B":                                            'CUD    Move cursor down
          repeat (get_param(0) #>1)
            text.cmd(text#C_CUD)

        "C":                                            'CUF    Move cursor right
          repeat (get_param(0) #>1)
            text.cmd(text#C_CUF)

        "D":                                            'CUB    Move cursor left
          repeat (get_param(0) #>1)
            text.cmd(text#C_CUB)

        "H", "f":                                       'CUP,HVP Move cursor to row, column (origin at 1,1).
          text.cup((get_param(1) #>1) - 1, (get_param(0) #>1) - 1)

        "J":                                            'ED     Erase Display
          text.ed(get_param(0))

        "K":                                            'EL     Erase in line
          text.el(get_param(0))

        "L":                                            'IL     Insert line(s)
          text.il(get_param(0))

        "M":                                            'DL     Delete line(s)
          text.dl(get_param(0))

        "P":                                            'DCH    Delete character(s)
          text.dch(get_param(0))

        "X":                                            'ECH    Erase character(s) (VT220)
          text.ech(get_param(0))

        "m":                                            'SGR    Set character attributes
          repeat i from 0 to csi_par_cnt
            case result := get_param(i)
              0:                                        'All attributes off
                attr_color := bg_color<<4 | fg_color     '(default color)
              1:                                        'bold (increased intensity)
                attr_color |= ATTR_BOLD                 'set bit for high color
              2:                                        'half-bright (simulated with color)
              4:                                        'underline (simulated with color)
                attr_color |= ATTR_UNDERLINE            'set bit for underline

              5:                                        'blink
              7:                                        'negative (reverse) image
                attr_color |= ATTR_INVERS
              10..12:                                   'font
                font := result
              22:                                       'bold off (normal intensity)
                attr_color &= ! ATTR_BOLD
              24:                                       'not underlined
                attr_color &= ! ATTR_UNDERLINE          'underline off
              25:                                       'not blinking
              27:                                       'positive image
                attr_color &= ! ATTR_INVERS
              30..37:                                   'fg (character) color
                attr_color := (attr_color & %11111111_11111000) | (result - 30)
              39:                                       'default fg color
                'TODO: attr_color := (attr_color & %11111111_11111000) | fg_color
              40..47:                                   'bg color
                attr_color := ((result - 40) << 4) | (attr_color & %11111111_00001111)
              49:                                       'default bg color
                'TODO: attr_color := (bg_color << 4) | (attr_color & %11111111_00001111)
{
              90..97:                                   'fg (character) color
                attr_color := (attr_color & %11111111_11111000) | (result - 90+7)
              100..107:                                 'bg color
                attr_color := ((result - 100+7) << 4) | (attr_color & %11111111_00001111)
}

          text.attr_color(attr_color)
          if font => 10
            text.set_font(font -= 10)

        "c":                                            'DA     Answer ESC [ ? 6 c: "I am a VT102".
          tx_csi(string("?6c"))

        "g":                                            'TBC    Without parameter: clear tab stop at current position.
          'TODO or not TODO?                            '       ESC [ 3 g: delete all tab stops.

        "h","l":                                        'SM/RM     Set/Reset Mode
          result := ||(ch == "h")
          repeat i from 0 to csi_par_cnt
            case get_param(i)
              '3:                                       'DECCRM Display control chars. (default off)
                ' TODO?
              4:                                        'IRM    Set/Reset insert mode. (default off)
                text.cmd(text#C_IRM+result)
              20:                                       'LNM    Automatically follow echo of LF, VT or FF with CR. (default off)
                text.cmd(text#C_LNM+result)

        "r":                                            'DECSTBM Set scrolling region; parameters are top and bottom row.
          text.decstbm(get_param(0), get_param(1))

        "n":                                            'DSR    Status report
          repeat i from 0 to csi_par_cnt
            case get_param(i)
              5:                                        'DSR    Device status report: Answer is ESC [ 0 n (Terminal OK).
                tx_csi(string("0n"))
              6:                                        'CPR    Cursor position report: Answer is ESC [ y ; x R
                'repeat 1
                waitcnt(10*clkfreq/BaudTab[baud] + cnt) 'debug wait 1 char time
                result := text.cpr
                ser.tx(ESC)
                ser.tx("[")
                ser.dec(result >> 16 + 1)
                ser.tx(";")
                ser.dec(result & $FF + 1)
                ser.tx("R")
        "]":                                            'From Linux console
          case get_param(0)
            1:                                          'Set color n as the underline color
              'TODO
            2:                                          'Set color n as the dim color
              'TODO
            8:                                          'Make the current color pair the default attributes.
              'TODO

    "?":                                                'Private mode
      case ch
        "h","l":                                        'DECSET/DECRST Set/Reset DEC Private Mode
          result := ||(ch == "h")
          repeat i from 0 to csi_par_cnt
            case get_param(i)
              3:                                        'DECCOLM Number of columns (default off = 80 cols)
                text.ed(2)                              'Not supported, but clear the sceen anyway
                text.cmd(text#C_DECOM)                  'Origin Mode Absolute
                text.decstbm(0,0)
              5:                                        'DECSCNM Set reverse-video mode. (default off)
              6:                                        'DECOM   Origin Mode; curser pos. in scrolling region. (default off)
                text.cmd(text#C_DECOM+result)
              7:                                        'DECAWM  Set autowrap on/off. (default on)
                text.cmd(text#C_DECAWM+result)
              25:                                       'DECTECM Cursor visible
                text.cmd(text#C_DECTECM+result)

    other:
      '


PRI tx_csi(seq)

  'repeat 5
  waitcnt(10*clkfreq/BaudTab[baud] + cnt)               'debug wait 1 char time
  ser.tx($1B)                                           'send ESC [
  ser.tx($5B)
  ser.str(seq)


' ===================================== Setup Menu Functions =====================================

CON

  ROWS        = text#ROWS
  COLS        = text#COLS

  M_COLOR     = black<<4 + cyan
  M_COLOR_INV = black + cyan << 4

VAR

  word screenbuffer[2+80*3]

PRI setup | ch, update, cursorpos

  update~~
  cursorpos := text.cpr_abs                           'save cursor position
  screen_push(0, ROWS-3, COLS, 3)

  menu_fill(0, ROWS-3, COLS, M_COLOR_INV, " ")
  menu_fill(0, ROWS-2, COLS, M_COLOR,     " ")
  menu_fill(0, ROWS-1, COLS, M_COLOR_INV, " ")
  menu_str((COLS-strsize(@strVersion))>>1, ROWS-3, M_COLOR_INV, @strVersion)
  menu_str(0, ROWS-2, M_COLOR, string("Baud Rate:          Fore/Back Color              Cursor:     Font:     Save+Exit"))
  menu_str(8, ROWS-1, M_COLOR_INV,     string("F2               F3/F4                       F6        F7         F10"))
  if variant == VT100
    menu_str(41, ROWS-1, M_COLOR_INV, string("F5"))
    menu_str(37, ROWS-2, M_COLOR, string("Port:"))

  repeat
    if update~
      text.cursor_set(0)                                'cusor invisible
      text.attr_color(M_COLOR)
      text.cup_abs(10, 28)
      text.decw(BaudTab[baud], 6, " ")
      if variant == VT100
        menu_str(43, ROWS-2, M_COLOR, @@PortTab[comport])
      text.cup_abs(57, ROWS-2)
      text.dec(c_mode)
      text.cup_abs(67, ROWS-2)
      text.dec(font)
      text.cup_abs(30, ROWS-2)
      text.attr_color(bg_color << 4 + fg_color)         'restore color
      text.decw(fg_color, 2,"0")
      text.str(string("/"))
      text.decw(bg_color, 2,"0")
      text.cup_abs(cursorpos & $FFFF, cursorpos >> 16)   'restore old cursor position
      text.cursor_set(c_mode)                            'restore cusor

    if ch := key.key
      update~~
      case ch
        209:                                          'if F2 (Baud Rate)
          baud := ++baud // 9
          if variant == VT100
            ser.start(RXD,TXD,RTS,CTS,DTR,DCD,DSR,0,BaudTab[baud])'restart host communication
          else
            ser.start(RXD,TXD, -1, -1, -1, -1, -1,0,BaudTab[baud])'start host communication

        210:                                          'if F3 (Fore Color)
          fg_color := ++fg_color // 16                'increment foreground colur
          attr_color := attr_color & $FFF0 | fg_color
          text.attr_color(attr_color)                           'set textcolor

        211:                                          'if F4 (Back Color)
          bg_color := ++bg_color // 16                'increment background colur
          attr_color := attr_color & $FF0F | bg_color<<4
          text.attr_color(attr_color)                           'set textcolor

        212:                                          'if F5 (Port)
          if variant == VT100
            comport ^= 1                                'toggle comport
            outa[PORTSWITCH] := comport                 'toggle portswitch pin
            ser.rxflush                                 'clear Rx buffer

        213:                                          'if F6 (Cursor)
          c_mode := ++c_mode // 8                     'increment cursor mode
          text.cursor_set(c_mode)                     'set corsor mode

        214:                                          'if F7 (Font)
          font := ++font // 2                         'increment Font #
          text.set_font(font)

        ESC, $02dc, 217:
          if ch == 217                                'if F10 (Save+Exit)
            cfg[0] := baud                              'baud
            cfg[1] := fg_color                          'foreground color
            cfg[2] := bg_color                          'background color
            cfg[3] := c_mode                            'cursor mode
            cfg[4] := comport                           'switch comport
            cfg[5] := font
            cfg[6] := online_stat
            config.writeCfg(@cfg)                       'write EEPROM with configdata

          quit

  text.attr_color(attr_color)                                 'resore old screen color and attributes
  screen_pop


PRI menu_fill(x, y, count, attrib, char)

  text.attr_color(attrib)
  text.cup_abs(x, y)
  repeat count
    text.emit(char)

PRI menu_str(x, y, attrib, string_ptr)

  text.attr_color(attrib)
  text.cup_abs(x, y)
  text.str(string_ptr)


PRI screen_push(x, y, w, h) | buf, scr

  buf := @screenbuffer + 2<<1
  scr := text.get_screen_ptr + (y*COLS+x)<<1

  repeat h
    wordmove(buf , scr, w)
    buf += w<<1
    scr += COLS<<1

  screenbuffer[0] := x + y<<8
  screenbuffer[1] := w + h<<8


PRI screen_pop | x, y, w, h, buf, scr

  x := screenbuffer[0] & $FF
  y := screenbuffer[0] >> 8
  w := screenbuffer[1] & $FF
  h := screenbuffer[1] >> 8

  buf := @screenbuffer + 2<<1
  scr := text.get_screen_ptr + (y*COLS+x)<<1

  repeat h
    wordmove(scr, buf, w)
    buf += w<<1
    scr += COLS<<1


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
