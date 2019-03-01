{{
Proramm         : VGA_640.spin
Info            : VT100 Terminal for AVR CP/M
Autor           : Joe G.
Version         : 01
Subversion      : 03
Funktion        : VT100 code interpreter
Komponenten     : VGA_HiRes_Color

Log

10.04.2013      Start, CR, LF
12.04.2013      code CA, BS
17.04.2013      fast insert line
26.04.2013      arrow keys, cursor (not) visible
27.04.2013      bug in ESC[K
25.05.2013      bell
03.01.2014      with German letters, bug in ESC[M
19.01.2014      second character font
28.01.2014      new VGA Mode
}}


CON
  COLS     = 80                 'number of screen columns
  ROWS     = 30                 'number of screen rows
  CHARS    = ROWS*COLS          'number of screen characters

' Text attributes
  ATTR_BOLD       = 1 << 3
  ATTR_INVERS     = 1 << 8
  ATTR_UNDERLINE  = 1 << 9

  #0,  C_NOOP
  #9,  C_HT       'Moves cursor to next tab stop, or to right margin if there are no more tab stops.
  #10, C_IND      'Linefeed.
  #13, C_CR       'Moves cursor to left margin on current line.
  #14, C_G1       'Selects G1 character set designated by a select character set sequence.
  #15, C_G0       'Selects G0 character set designated by a select character set sequence.
  #31
  C_SGR           'Set character attributes
  C_DECSTBM       'Set scrolling region; parameters are top and bottom row.
  C_CUU           'Cursor up
  C_CUD           'Cursor down
  C_CUF           'Cursor right
  C_CUB           'Cursor left
  C_CUP           'Cursor Position
  C_HVP           'Horizontal/Vertical Position (same as CUP)
  C_RI            'Reverse linefeed. (Curser up)
  C_NEL           'Newline.
  C_DECSC         'Save current state (cursor coordinates, attributes, character sets pointed at by G0, G1).
  C_DECRC         'Restore state most recently saved by ESC 7.
  C_HTS           'Set tab stop at current column.
  C_TBC           'Clear TAB Stop. Without parameter: at current position.
  C_EL            'Erase in line
  C_ED            'Erase Display
  C_DCH           'Delete character(s)
  C_DSR           'Device status report: Answer is ESC [ 0 n (Terminal OK).
  C_DA            'Device Attributes (Answer ESC [ ? 6 c: "I am a VT102".)
  C_DECID         'DEC private identification. The terminal returns  ESC [ ? 6 c, claiming that it is a VT102.
  C_RIS           'Reset to Initial State (Power Up)
  C_DECALN        'DEC screen alignment test - fill screen with E's.
  'VT220
  C_ICH           'Insert Character(s) (VT220)
  C_ECH           'Erase character(s) (VT220)
  '
  'Modes:
  '
  C_IRM[2]        'Insert/replace (default off)
  C_SRM[2]        'Send/receive
  C_LNM[2]        'Automatically follow echo of LF, VT or FF with CR. (default off)
  C_DECCOLM[2]    'Number of columns (default off = 80 cols)
  C_DECSCNM[2]    'Set reverse-video mode. (default off)
  C_DECOM[2]      'Origin Mode; curser pos. in scrolling region. (default off)
  C_DECAWM[2]     'Set autowrap on/off. (default on)
  ' VT220
  C_DECTECM[2]    'Cursor not visible/visible

  C_DECCRM[2]     'Display control chars. (default off)
  C_DECRST        'Reset Mode

OBJ
'  vga : "VGA_HiRes_Text_Color"
'  vga : "VGA_HiRes_Text_Color-rot"
  vga : "vgacolour"
  font: "fonts-lin"

DAT
                   ' RGB   RGB   RGB   RGB   RGB   RGB   RGB   RGB
  defaultclut byte %%000,%%200,%%020,%%220,%%002,%%202,%%022,%%222     'normal colors
              byte %%111,%%311,%%131,%%330,%%113,%%313,%%133,%%333     'bright colors

VAR
  word  screen[CHARS]                                   'screen character buffer
  word  fontaddr
  long  sync
  byte  xloc, yloc, cursor_mode                         'First curser
  byte  cursor2[3]                                      '2nd cursor, currently not used. Cursorbytes must be contiguous
  long  loc                                             'actual position in screen buffer
  long  wrapped

  long  char_color                                      'foreground colour bits 3..0, background colour bits 7..4
  long  bg_fill
  byte  mode_autowrap                                   'true if autowrap
  byte  mode_insert                                     'true: insert, false: replace (default)
  byte  mode_newline                                    'true: CR/LF, false LF only (default)
  byte  cursor_stat
  byte  rtop, rbottom                                   'scrolling region
  byte  mode_origin

  long  save_xloc
  long  save_yloc
  long  save_char_color                                 'TODO: init on power up/terminal reset

  byte clut[16]


PUB start(BasePin)
''start vga

  fontaddr := font.get_fontptr(0)
  bytemove(@clut, @defaultclut, 16)

  vga.start(BasePin, @screen, @xloc, @fontaddr, @clut, @sync)
  waitcnt(clkfreq * 1 + cnt)    'wait 1 second for cogs to start


''init cursor attributes
  xloc := 127
  yloc := 63
  cursor_mode := %110                                   'init cursor 0 to underscore with slow blink
  cursor2[0] := 127
  cursor2[1] := 63
  cursor2[2] := %100                                    'cursor 1 not visible

  mode_origin := 0
  rtop := 0
  rbottom := ROWS


PUB bin(value, digits)
'' Print a binary number, specify number of digits

  repeat while digits > 32
    emit("0")
    digits--

  value <<= 32 - digits

  repeat digits
    emit((value <-= 1) & 1 + "0")

PUB dec(value) | i
'' Print a decimal number

  if value < 0
    -value
    emit("-")

  i := 1_000_000_000

  repeat 10
    if value => i
      emit(value/i + "0")
      value //= i
      result~~
    elseif result or i == 1
      emit("0")
    i /= 10

PUB decw(value, width, fill) | sign, n, digit[10]
'' Print a decimal number

  width #>= 1
  sign~
  if value < 0
    -value
    sign~~
    width--

  n~
  repeat
    digit[n++] := value // 10 + "0"
    value /= 10
  while value

  if fill == "0"
    if sign
      emit("-")
  repeat while width-- > n
    emit(fill)
  if fill <> "0"
    if sign
      emit("-")
  repeat while n
      emit(digit[--n])


PUB hex(value, digits)
'' Print a hexadecimal number, specify number of digits

  repeat while digits > 8
    emit("0")
    digits--

  value <<= (8 - digits) << 2

  repeat digits
    emit(lookupz((value <-= 4) & $f : "0".."9", "A".."F"))


PUB binFP(value) | bitnum, bit, bitval
'' Prints FP long in special Binary format: sign, exp, mantissa

  repeat bitnum from 31 to 0
    bit := 1 << bitnum                                  ' create mask bit
    bitval := (bit & value) >> bitnum                   ' extract bit and shift back to bit 0

    bin(bitval, 1)                                      ' display one bit

    case bitnum
      27,20,16,12,8,4: emit($20)                        ' space after every 4 in group
      31,23: str(string("  "))                          ' two after sign and exponent

PUB str(string_ptr)

'' Print a zero terminated string

  repeat while result := byte[string_ptr++]
    emit(result)

PUB newline

  cmd(C_CR)
  cmd(C_IND)


PUB decaln

'' DECALN - Screen Alignment Display (DEC Private)
'' Fill the entire screen area with uppercase 'E's for screen focus and alignment.

  cup_abs(0,0)
  longfill(@screen, char_color | $00450045, CHARS/2)


PUB showfont | row, col, px, py

  ed(2)
  cup_abs(px := (COLS-52)/2, py := (ROWS-18)/2)
  str(string( "  | 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F"))
  cup_abs(px-1, ++py)
  str(string("---+------------------------------------------------"))

  repeat row from 0 to 15
    cup_abs(px, ++py)
    hex(row,1)
    str(string(" |"))
    repeat col from 0 to 15
      emit(" ")
      emit(row*16 + col)
      emit(" ")
  cup_abs(0, py+2)


PUB ed (what)
'' Erase in Display

  case what
    0:                                                  'Erase from cursor to end of screen, inclusive.
      wordfill(@screen+loc<<1, bg_fill, CHARS-loc)
    1:                                                  'Erase from beginning of screen to cursor, inclusive.
      wordfill(@screen, bg_fill, loc+1)
    2:                                                  'Erase complete display. Cursor does not move.
      longfill(@screen, bg_fill, CHARS/2)



PUB el (what)
'' Erase in line

  wrapped~
  case what
    0:                                                  'Erase from cursor to end of line, inclusive.
      wordfill(@screen+loc*2, bg_fill, COLS-xloc)
    1:                                                  'Erase from beginning of line to cursor, inclusive.
      wordfill(@screen+yloc*COLS*2, bg_fill, xloc+1)
    2:                                                  'Erase complete line. Cursor does not move.
      wordfill(@screen+yloc*COLS*2, bg_fill, COLS)



PUB il(count) | src, dst, len
 ''insert line(s) and roll down "ESC[L"

  if yloc => rtop                                       'execute only, if inside scolling region
    if yloc < rbottom

      count := count #> 1 <# rbottom - yloc

      src := @screen + (yloc*COLS)<<1                   'yloc addr, current position
      dst := src + (count*COLS)<<1                      'destination line (count lines down)
      len := ((rbottom - (yloc+count))*COLS)>>1         '
      longmove(dst, src, len)                           'shift screen down
      wordfill(src, bg_fill, COLS*count)                'fill line with space



PUB dl(count) | dst, src, len
'' delete line(s) and roll up "ESC[M"

  if yloc => rtop                                       'execute only, if inside scolling region
    if yloc < rbottom

      count := count #> 1 <# rbottom - yloc

      dst := @screen + (yloc*COLS)<<1                   'yloc addr, current position
      src := dst + (count*COLS)<<1                      'next line
      len := ((rbottom - (yloc+count))*COLS)>>1
      longmove(dst, src, len)                           'shift screen up
      dst := @screen + ((rbottom - count)*COLS)<<1      'last line
      wordfill(dst, bg_fill, COLS*count)                'delete last line



PUB ech (count)
'' Erase count characters on current line.

  wrapped~
  wordfill(@screen + loc<<1, bg_fill, count #> 1 <# COLS-xloc)


PUB dch (count)
'' Delete count characters on current line.

  wrapped~
  count := count #> 1 <# COLS-xloc

  wordmove(@screen+loc<<1, @screen+(loc+count)<<1, COLS-(xloc+count))

  'VT102 spec:
  'result := screen[yloc*COLS + (COLS-1)] & $FF00 | $20  'space with color from last char in line
  'VT220 spec:
  result := bg_fill                                      'TODO: set colors to normal!
  wordfill(@screen+(loc+COLS-(xloc+count))<<1, result,  count)


PUB ich (count)
'' Insert count blank characters at the cursor position,
'' with the character attributes set to normal. (VT220)

  wrapped~
  count := count #> 1 <# COLS-xloc

  wordmove(@screen+(loc+count)<<1, @screen+loc<<1, COLS-(xloc+count))

  'result := char_color | $20                           'TODO: set colors to normal!
  wordfill(@screen+loc<<1, bg_fill,  count)


PUB attr_color(attribs)
''reset screen colors and attributes

  result := attribs & $FF
  if attribs & ATTR_INVERS
    result := result >> 4 | (result & $F) << 4
  char_color := result << 8 + result << 24
  bg_fill := char_color | $00200020
  if attribs & ATTR_UNDERLINE
    char_color |= $00800080

PUB set_font(num)
'' select font 0..2

  fontaddr := font.get_fontptr(num)


PUB get_screen_ptr
    return @screen

PUB get_mode(which) : rc
  case which
    C_IRM:                                              'Insert/replace (default off)
      rc := mode_insert
    C_SRM:                                              'Send/receive
    C_LNM:                                              'Automatically follow echo of LF, VT or FF with CR. (default off)
      rc := mode_newline
    C_DECCOLM:                                          'Number of columns (default off = 80 cols)
    C_DECSCNM:                                          'Set reverse-video mode. (default off)
    C_DECOM:                                            'Origin Mode; curser pos. in scrolling region. (default off)
      rc := mode_origin
    C_DECAWM:                                           'Set autowrap on/off. (default on)
      rc := mode_autowrap
    C_DECTECM:                                          'Cursor not visible/visible
      'TODO:


PUB cup_abs(x, y)
'' cursor position x,y absolute

  wrapped~
  xloc := x <# COLS-1
  yloc := y <# ROWS-1
  loc  := xloc + yloc*COLS

PUB cup(x, y)
'' move cursor to x, y position

  wrapped~
  if mode_origin
    y += rtop
    yloc := y <# rbottom-1
  else
    yloc := y <# ROWS-1

  xloc := x <# COLS-1
  loc  := xloc + yloc*COLS


PUB decstbm(top, bottom)
'' set top and bottom margins of scrolling region

  rtop    := top-1 #> 0 <# ROWS-2
  if bottom == 0
    bottom := ROWS
  rbottom := bottom #> rtop+2 <# ROWS
  cup(0,0)

PUB cpr_abs
'' return cursor position absolute

  return yloc<<16 + xloc


PUB cpr
'' return cursor position (0 based)

  if mode_origin
    return (yloc-rtop)<<16 + xloc
  else
    return yloc<<16 + xloc


PUB cursor_set(value)
'' set cursor0 attributes

  cursor_stat :=  value & %111                          'set cursor value
  cursor_mode :=  cursor_stat


PRI scroll_up | p, len

  if yloc => rtop                                       'execute only, if inside scolling region
    if yloc < rbottom

      p := @screen + (rtop * COLS)<<1                   'start of 1st row
      len := ((rbottom - rtop - 1) * COLS)>>1
      longmove(p, p + COLS*2, len)                      'shift screen up one line

      'clear last line
      p := @screen + ((rbottom-1)*COLS)<<1              'row start position
      longfill(p, bg_fill, COLS/2)                      'fill line with space


PRI scroll_down | p, len

  if yloc => rtop                                       'execute only, if inside scolling region
    if yloc < rbottom

      p := @screen + (rtop * COLS)<<1                   'start of 1st row
      len := ((rbottom - rtop - 1) * COLS)>>1
      longmove(p + COLS*2, p, len)                      'shift screen down one line

      'clear top line
      wordfill(p, bg_fill, COLS/2)                      'fill line with space


PUB emit(c)
'' Print a character

  if wrapped~
    if mode_autowrap
      xloc := 0
      loc++
      if yloc == rbottom-1
        scroll_up
        loc -= COLS
      else
        ++yloc

  if mode_insert
    wordmove(@screen+((loc+1)<<1), @screen+(loc<<1), COLS-1-xloc)

  screen[loc] := char_color  + c                        'output the character

  if xloc == COLS-1                                     'last row postion?
    wrapped~~
  else
    ++loc
    ++xloc


PUB cmd(c)
'' execute a control function

  wrapped~
  case c

    C_HT:                                               ' tab command
      cup((xloc+8) & constant(!7), yloc)                'tabstop every 8 cols for now. (TODO)

    C_IND:                                              'Index/LF
      if yloc == rbottom - 1                            'last row of scrolling region?
        scroll_up                                       'cursor position not changed
      else                                              'move cursor down
        if yloc <> ROWS - 1                             '... unless last row of screen
          ++yloc                                        'new cursor position
          loc += COLS                                   'next output position
      if mode_newline
        loc -= xloc~                                    'move to 1st char of line


    C_CR:                                               'CR - move to 1st char of line
      loc -= xloc~

    C_CUU:                                              'Cursor up
      if yloc  and yloc<>rtop                           'skip if at top of screen or scrolling region
        --yloc                                          'reset 'y' cursor position
        loc -= COLS                                     'move screen entry index back one row

    C_CUD:                                              'Cursor down
      if yloc<>ROWS-1 and yloc<>rbottom-1               'skip if at bottom of screen
        ++yloc
        loc += COLS

    C_CUF:                                              'Cursor right
      if xloc <> COLS - 1                               'skip if at right margin of screen
        ++xloc
        ++loc

    C_CUB:                                              'Cursor left, BS
      if xloc > 0
        loc--                                           'screenpointer - 1
        xloc--                                          'cursorpointer - 1

    C_RI:                                               'Reverse LF
      if yloc == rtop                                   'on first row?
        scroll_down                                     'cursor position not changed
      else                                              '
        if yloc
          --yloc                                        'new row cursor position
          loc -= COLS                                   'next output position

    C_DECSC:                                            'save cursor
      save_xloc := xloc
      save_yloc := yloc
      save_char_color := char_color

    C_DECRC:                                            'restore cursor
      xloc := save_xloc
      yloc := save_yloc
      loc := xloc + yloc*COLS
      char_color := save_char_color

    C_DECTECM+0:
      cursor_mode := %100                               'cursor 0 not visible

    C_DECTECM+1:
      cursor_mode := cursor_stat

    C_IRM+0:                                            'Insert/replace mode: replace
      mode_insert := FALSE

    C_IRM+1:                                            'Insert/replace mode: insert
      mode_insert := TRUE

    C_LNM+0:                                            'Insert/replace mode: replace
      mode_newline := FALSE

    C_LNM+1:                                            'Insert/replace mode: insert
      mode_newline := TRUE

    C_DECOM+0:                                          'Reset Origin Mode: Select home position in upper left of screen
      mode_origin := FALSE
      cup(0,0)

    C_DECOM+1:                                          'Origin Mode: Select home position in scrolling region
      mode_origin := TRUE
      cup(0,0)

    C_DECAWM+0:  'TODO                                  'Autowrap off
      mode_autowrap := FALSE

    C_DECAWM+1:  'TODO                                  'Autowrap on
      mode_autowrap := TRUE
