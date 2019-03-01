''***************************************
''*  VGA High-Res Text Driver v1.0      *
''*  Author: Chip Gracey                *
''*  Copyright (c) 2006 Parallax, Inc.  *
''*  See end of file for terms of use.  *
''***************************************
''
'' This object generates a 640x480 VGA signal which contains 80 columns x 30
'' rows of 8x16 characters. Each character can have a unique forground/background
'' color combination and each character can be inversed and high-lighted.
'' There are also two cursors which can be independently controlled (ie. mouse
'' and keyboard). A sync indicator signals each time the screen is refreshed
'' (you may ignore).
''
'' You must provide buffers for the screen, cursors, and sync. Once started,
'' all interfacing is done via memory. To this object, all buffers are
'' read-only, with the exception of the sync indicator which gets written with
'' -1. You may freely write all buffers to affect screen appearance. Have fun!
''

CON

{
' 640 x 480 @ 62,751Hz settings: 80 x 30 characters
'   GTF SPREADSHEET BY ANDY MORRISH  1/5/97)
' --> fH: 31,2 KHz  fV 59,4 Hz

        hp = 640        ' horizontal pixels
        vp = 480        ' vertical pixels
        hf = 16         ' horizontal front porch pixels
        hs = 64         ' horizontal sync pixels
        hb = 80         ' horizontal back porch pixels
        vf = 1          ' vertical front porch lines
        vs = 3          ' vertical sync lines
        vb = 14         ' vertical back porch lines
        hn = 1          ' horizontal normal sync state (0|1)
        vn = 1          ' vertical normal sync state (0|1)
        pr = 25         ' pixel rate in MHz at 80MHz system clock (5MHz granularity)
}

' 640 x 480 @ 60Hz settings: 80 x 30 characters
'   http://tinyvga.com/vga-timing/640x480@60Hz
' --> fH: 31,3 KHz  fV 59,5 Hz

        hp = 640        ' horizontal pixels
        vp = 480        ' vertical pixels
        hf = 16         ' horizontal front porch pixels
        hs = 96         ' horizontal sync pixels
        hb = 48         ' horizontal back porch pixels
        vf = 10         ' vertical front porch lines
        vs = 2          ' vertical sync lines
        vb = 33         ' vertical back porch lines
        hn = 1          ' horizontal normal sync state (0|1)
        vn = 1          ' vertical normal sync state (0|1)
        pr = 25         ' pixel rate in MHz at 80MHz system clock (5MHz granularity)

{
' 640 x 480 @ 69Hz settings: 80 x 30 characters

        hp = 640        ' horizontal pixels
        vp = 480        ' vertical pixels
        hf = 24         ' horizontal front porch pixels
        hs = 40         ' horizontal sync pixels
        hb = 128        ' horizontal back porch pixels
        vf = 20         ' vertical front porch lines
        vs = 3          ' vertical sync lines
        vb = 17         ' vertical back porch lines
        hn = 1          ' horizontal normal sync state (0|1)
        vn = 1          ' vertical normal sync state (0|1)
        pr = 30         ' pixel rate in MHz at 80MHz system clock (5MHz granularity)
}
{
' 640 x 480 @ 69Hz settings: 80 x 30 characters

        hp = 640        ' horizontal pixels
        vp = 480        ' vertical pixels
        hf = 30         ' horizontal front porch pixels
        hs = 44         ' horizontal sync pixels
        hb = 86        ' horizontal back porch pixels
        vf = 9          ' vertical front porch lines
        vs = 3          ' vertical sync lines
        vb = 28         ' vertical back porch lines
        hn = 1          ' horizontal normal sync state (0|1)
        vn = 1          ' vertical normal sync state (0|1)
        pr = 30         ' pixel rate in MHz at 80MHz system clock (5MHz granularity)
}
{
' 640 x 480 @ 69Hz settings: 80 x 30 characters

        hp = 640        ' horizontal pixels
        vp = 480        ' vertical pixels
        hf = 16         ' horizontal front porch pixels
        hs = 95         ' horizontal sync pixels
        hb = 48        ' horizontal back porch pixels           !!!!!!
        vf = 9          ' vertical front porch lines
        vs = 3          ' vertical sync lines
        vb = 28         ' vertical back porch lines
        hn = 1          ' horizontal normal sync state (0|1)
        vn = 1          ' vertical normal sync state (0|1)
        pr = 30         ' pixel rate in MHz at 80MHz system clock (5MHz granularity)
}


' columns and rows

        cols = hp / 8
        rows = vp / 16


VAR long cog[2]

PUB start(BasePin, ScreenPtr, CursorPtr, FontPtr, ClutPtr, SyncPtr) : okay | i, j

'' Start VGA driver - starts two COGs
'' returns false if two COGs not available
''
''      BasePin = VGA starting pin (0, 8, 16, 24, etc.)
''
''      ScreenPtr = Pointer to 80x30 words containing Latin-1 codes and colors for
''              each of the 80x30 screen characters. The lower byte of the word
''              contains the Latin-1 code to display. The upper byte contains
''              the foreground colour in bits 11..8 and the background colour in
''              bits 15..12.
''
''              screen word example: %00011111_01000001 = "A", white on blue
''
''      CursorPtr = Pointer to 6 bytes which control the cursors:
''
''              bytes 0,1,2: X, Y, and MODE of cursor 0
''              bytes 3,4,5: X, Y, and MODE of cursor 1
''
''              X and Y are in terms of screen characters
''              (left-to-right, top-to-bottom)
''
''              MODE uses three bottom bits:
''
''                      %x00 = cursor off
''                      %x01 = cursor on
''                      %x10 = cursor on, blink slow
''                      %x11 = cursor on, blink fast
''                      %0xx = cursor is solid block
''                      %1xx = cursor is underscore
''
''              cursor example: 127, 63, %010 = blinking block in lower-right
''
''      FontPtr = Pointer to current font data
''
''      ClutPtr = Pointer to color look up table (optional)
''              If ClutPtr == 0, build in default clut gets used.
''
''      SyncPtr = Pointer to long which gets written with -1 upon each screen
''              refresh. May be used to time writes/scrolls, so that chopiness
''              can be avoided. You must clear it each time if you want to see
''              it re-trigger.

        ' if driver is already running, stop it
        stop

        ' implant pin settings
        reg_vcfg := $200000FF + (BasePin & %111000) << 6
        i := $FF << (BasePin & %011000)
        j := BasePin & %100000 == 0
        reg_dira := i & j
        reg_dirb := i & !j

        ' implant CNT value to sync COGs to
        sync_cnt := cnt + $10000

        ' implant pointers
        longmove(@screen_base, @ScreenPtr, 4)

        ' implant unique settings and launch first COG
        vf_lines.byte := vf
        vb_lines.byte := vb
        font_part := 1
        cog[1] := cognew(@d0, SyncPtr) + 1

        ' allow time for first COG to launch
        waitcnt($2000 + cnt)

        ' differentiate settings and launch second COG
        vf_lines.byte := vf+4
        vb_lines.byte := vb-4
        font_part := 0
        cog[0] := cognew(@d0, SyncPtr) + 1

        ' if both COGs launched, return true
        if cog[0] and cog[1]
                return true

        ' else, stop any launched COG and return false
        stop


PUB stop | i

'' Stop VGA driver - frees two COGs

        repeat i from 0 to 1
        if cog[i]
        cogstop(cog[i]~ - 1)


CON

        #1, scanbuff[80], colorbuff[80], scancode[2*80-1+3], maincode   'enumerate COG RAM usage

        main_size = $1F0 - maincode                             'size of main program

        hv_inactive = (hn << 1 + vn) * $0101                    'H,V inactive states


DAT

'*****************************************************
'* Assembly language VGA high-resolution text driver *
'*****************************************************

' This program runs concurrently in two different COGs.
'
' Each COG's program has different values implanted for front-porch lines and
' back-porch lines which surround the vertical sync pulse lines. This allows
' timed interleaving of their active display signals during the visible portion
' of the field scan. Also, they are differentiated so that one COG displays
' even four-line groups while the other COG displays odd four-line groups.
'
' These COGs are launched in the PUB 'start' and are programmed to synchronize
' their PLL-driven video circuits so that they can alternately prepare sets of
' four scan lines and then display them. The COG-to-COG switchover is seemless
' due to two things: exact synchronization of the two video circuits and the
' fact that all COGs' driven output states get OR'd together, allowing one COG
' to output lows during its preparatory state while the other COG effectively
' drives the pins to create the visible and sync portions of its scan lines.
' During non-visible scan lines, both COGs output together in unison.
'
' COG RAM usage:        $000    = d0 - used to inc destination fields for indirection
'                $001-$050 = scanbuff - longs which hold 4 scan lines
'                $051-$010 = colorbuff - longs which hold colors for 80 characters
'                $0a1-$142 = scancode - stacked WAITVID/SHR for fast display
'                $143-$1EF = maincode - main program loop which drives display

                        org     0                               ' set origin to $000 for start of program

d0                      long    1 << 9                          ' d0 always resides here at $000, executes as NOP


' Initialization code and data - after execution, space gets reused as scanbuff

                        ' Move main program into maincode area

:move                   mov     $1EF, main_begin + main_size - 1
                        sub     :move,d0s0                      ' (do reverse move to avoid overwrite)
                        djnz    main_ctr,#:move

                        ' Build scanbuff display routine into scancode

:waitvid                mov     scancode+0, i0                  ' org   scancode
:shr                    mov     scancode+1, i1                  ' waitvid colorbuff+0, scanbuff+0
                        add     :waitvid, d1                    ' shr   scanbuff+0,#8
                        add     :shr, d1                        ' waitvid colorbuff+1, scanbuff+1
                        add     i0, d0s0                        ' shr   scanbuff+1,#8
                        add     i1, d0                          ' ...
                        djnz    scan_ctr, #:waitvid             ' waitvid colorbuff+cols-1, scanbuff+cols-1

                        mov     scancode+cols*2-1, i2           ' mov   vscl,#hf
                        mov     scancode+cols*2+0, i3           ' waitvid hvsync,#0
                        mov     scancode+cols*2+1, i4           ' jmp   #scanret

                        ' Init I/O registers and sync COGs' video circuits

                        mov     dira, reg_dira                  ' set pin directions
                        mov     dirb, reg_dirb
                        movi    frqa, #(pr / 5) << 2            ' set pixel rate
                        mov     vcfg, reg_vcfg                  ' set video configuration
                        mov     vscl, #1                        ' set video to reload on every pixel
                        waitcnt sync_cnt, colormask             ' wait for start value in cnt, add ~1ms
                        movi    ctra, #%00001_110               ' COGs in sync! enable PLLs now - NCOs locked!
                        waitcnt sync_cnt, #0                    ' wait ~1ms for PLLs to stabilize - PLLs locked!
                        mov     vscl, #100                      ' insure initial WAITVIDs lock cleanly

                        ' Jump to main loop

                        jmp     #main_begin                     ' jump to vsync - WAITVIDs will now be locked!

                        ' Data

d0s0                    long    1 << 9 + 1
d1                      long    1 << 10
main_ctr                long    main_size
scan_ctr                long    cols

i0                      waitvid colorbuff+0, scanbuff+0
i1                      shr     scanbuff+0, #8
i2                      mov     vscl, #hf
i3                      waitvid hvsync, #0
i4                      jmp     #scanret

reg_dira                long    0                               ' set at runtime
reg_dirb                long    0                               ' set at runtime
reg_vcfg                long    0                               ' set at runtime
sync_cnt                long    0                               ' set at runtime

                        ' Directives

                        fit     scancode                        ' make sure initialization code and data fit
main_begin              org     maincode                        ' main code follows (gets moved into maincode)

' Main loop, display field - each COG alternately builds and displays four scan lines

vsync                   mov     x, #vs                  wz       ' do vertical sync lines
                        call    #blank_vsync

vb_lines                mov     x, #vb                  wz       ' do vertical back porch lines (# set at runtime)
                        call    #blank_vsync

                        rdword  font_base, font_base_ptr

                        mov     screen_ptr, screen_base         ' reset screen pointer to upper-left character
                        mov     row, #0                         ' reset row counter for cursor insertion
                        mov     fours, #rows * 4 / 2            ' set number of 4-line builds for whole screen

                        ' Build four scan lines into scanbuff

fourline                mov     font_ptr, font_part             ' get address of appropriate font section
                        shl     font_ptr, #2
                        add     font_ptr, font_base

                        movd    :pixa, #scanbuff-1              ' reset scanbuff address (pre-decremented)
                        movd    :pixb, #scanbuff-1              ' reset scanbuff address (pre-decremented)
                        movd    :cola, #colorbuff-1             ' reset colorbuff address (pre-decremented)

                        mov     y, #2                           ' must build scanbuff in two sections because
                        mov     vscl, vscl_line2x               ' ..pixel counter is limited to twelve bits

:halfrow
                        waitvid underscore, #0                  ' output lows to let other COG drive VGA pins
                        mov     x, #cols/2                      ' ..for 2 scan lines, ready for half a row
:column
                        add     :pixa, d0                       ' increment scanbuff destination addresses
                        rdword  z, screen_ptr                   ' get character and colors from screen memory
                        mov     bg, z
                        and     z, #$7f                         ' mask character code
                        shl     z, #4                           ' font has 16 byte per character
                        add     z, font_ptr                     ' add font section address to point to 8*4 pixels
                        add     :pixb, d0                       ' increment scanbuff destination addresses
                        add     screen_ptr, #2                  ' increment screen memory address
:pixa                   rdlong  scanbuff, z                     ' read pixel long (8*4) into scanbuff
                        test    bg,#$80         wc              ' underline attribute set?
                        ror     bg, #12                         ' background color in bits 3..0
                        cmp     font_part, #3   wz              ' if last font section
:pixb   if_c_and_z      or      scanbuff, underline
                        mov     fg, bg                          ' foreground color in bits 31..28
                        shr     fg, #28                         ' bits 3..0
                        add     fg, #clut                       ' + offset to CLUT
                        movs    :lookup_fg,fg
                        add     bg, #clut                       ' + offset to CLUT
                        movs    :lookup_bg,bg
                        add     :cola, d0                       ' increment colorbuff destination addresses
:lookup_fg              mov     z, 0-0                          ' get foreground color value
                        shl     z,#8
:lookup_bg              or      z, 0-0                          ' combine with background color value
:cola                   mov     colorbuff, z

                        djnz    x, #:column                     ' another character in this half-row?

                        djnz    y, #:halfrow                    ' loop to do 2nd half-row, time for 2nd WAITVID

                        sub     screen_ptr, #2*cols             ' back up to start of same row in screen memory

                        ' Insert cursors into scanbuff

                        mov     z, #2                           ' ready for two cursors

:cursor                 rdbyte  x, cursor_base                  ' x in range?
                        add     cursor_base, #1
                        cmp     x, #cols        wc

                        rdbyte  y, cursor_base                  ' y match?
                        add     cursor_base, #1
                        cmp     y, row          wz

                        rdbyte  y, cursor_base                  ' get cursor mode
                        add     cursor_base, #1

        if_nc_or_nz     jmp     #:nocursor                      ' if cursor not in scanbuff, no cursor

                        add     x, #scanbuff                    ' cursor in scanbuff, set scanbuff address
                        movd    :xor, x

                        test    y, #%010        wc              ' get mode bits into flags
                        test    y, #%001        wz
        if_nc_and_z     jmp     #:nocursor                      ' if cursor disabled, no cursor

        if_c_and_z      test    slowbit, cnt    wc              ' if blink mode, get blink state
        if_c_and_nz     test    fastbit, cnt    wc

                        test    y, #%100        wz              ' get box or underscore cursor piece
        if_z            mov     x, longmask
        if_nz           mov     x, underscore
        if_nz           cmp     font_part, #3   wz              ' if underscore, must be last font section

:xor    if_nc_and_z     xor     scanbuff, x                     ' conditionally xor cursor into scanbuff

:nocursor               djnz    z, #:cursor                     ' second cursor?

                        sub     cursor_base, #3*2               ' restore cursor base

                        ' Display four scan lines from scanbuff

                        mov     y, #4                           ' ready for four scan lines

scanline                mov     vscl, vscl_chr                  ' set pixel rate for characters
                        jmp     #scancode                       ' jump to scanbuff display routine in scancode
scanret                  mov    vscl, #hs                       ' do horizontal sync pixels
                        waitvid hvsync, #1                      ' #1 makes hsync active
                        mov     vscl, #hb                       ' do horizontal back porch pixels
                        waitvid hvsync, #0                      ' #0 makes hsync inactive
                        shr     scanbuff+cols-1, #8             ' shift last column's pixels right by 8
                        djnz    y, #scanline                    ' another scan line?

                        ' Next group of four scan lines

                        add     font_part, #2                   ' if font_part + 2 => 4, subtract 4 (new row)
                        cmpsub  font_part, #4           wc      ' c=0 for same row, c=1 for new row
        if_c            add     screen_ptr, #2*cols             ' if new row, advance screen pointer
        if_c            add     row, #1                         ' if new row, increment row counter
                        djnz    fours, #fourline        wz      ' another 4-line build/display?

                        ' Visible section done, do vertical sync front porch lines

                        wrlong  longmask,par                    ' write -1 to refresh indicator

vf_lines                mov     x,#vf                           ' do vertical front porch lines (# set at runtime)
                        call    #blank

                        jmp     #vsync                          ' new field, loop to vsync

                        ' Subroutine - do blank lines

blank_vsync             xor     hvsync,#$101                    ' flip vertical sync bits

blank                   mov     vscl, hx                        ' do blank pixels
                        waitvid hvsync, #0

        if_z            call    #upd_clut                       ' update clut only once per frame

                        mov     vscl, #hf                       ' do horizontal front porch pixels
                        waitvid hvsync, #0
                        mov     vscl, #hs                       ' do horizontal sync pixels
                        waitvid hvsync, #1
                        mov     vscl, #hb                       ' do horizontal back porch pixels
                        waitvid hvsync, #0
                        djnz    x,#blank                wz      ' another line?
blank_ret
blank_vsync_ret
                        ret

                        ' Update color lookup table
upd_clut
                        cmp     clut_ptr,#0             wz
        if_z            jmp     #:no_clutupd

                        mov     bg,#16
                        movd     :upclut,#clut
:nextval
                        rdbyte  fg,clut_ptr
                        shl     fg,#2
                        or      fg,#$03
:upclut                 mov     clut,fg
                        add     clut_ptr,#1
                        add     :upclut,d0
                        djnz    bg,#:nextval
                        sub     clut_ptr,#16
:no_clutupd
upd_clut_ret
                        ret


                        ' Data

screen_base             long    0                               ' set at runtime (3 contiguous longs)
cursor_base             long    0                               ' set at runtime
font_base_ptr           long    0                               ' set at runtime
clut_ptr                long    0

font_base               long    0                               ' set at runtime
font_part               long    0                               ' set at runtime

hx                      long    hp                              ' visible pixels per scan line
vscl_line               long    hp + hf + hs + hb               ' total number of pixels per scan line
vscl_line2x             long    (hp + hf + hs + hb) * 2         ' total number of pixels per 2 scan lines
vscl_chr                long    1 << 12 + 8                     ' 1 clock per pixel and 8 pixels per set
colormask               long    $FCFC                           ' mask to isolate R,G,B bits from H,V
longmask                long    $FFFFFFFF                       ' all bits set
slowbit                 long    1 << 25                         ' cnt mask for slow cursor blink
fastbit                 long    1 << 24                         ' cnt mask for fast cursor blink
underscore              long    $FFFF0000                       ' underscore cursor pattern
underline               long    $0000FF00
hv                      long    hv_inactive                     ' -H,-V states
hvsync                  long    hv_inactive ^ $200              ' +/-H,-V states

clut                    '         RGB_S
                        long    %%000_3                     ' black
                        long    %%200_3                     ' red
                        long    %%020_3                     ' green
                        long    %%220_3                     ' yellow
                        long    %%002_3                     ' blue
                        long    %%202_3                     ' magenta
                        long    %%022_3                     ' cyan
                        long    %%222_3                     ' gray
                        long    %%111_3                     ' darkgray
                        long    %%311_3                     ' lred
                        long    %%131_3                     ' lgreen
                        long    %%330_3                     ' lyellow
                        long    %%113_3                     ' lblue
                        long    %%313_3                     ' lmagenta
                        long    %%133_3                     ' lcyan
                        long    %%333_3                     ' white

                        ' Uninitialized data

screen_ptr              res     1
font_ptr                res     1

x                       res     1
y                       res     1
z                       res     1
fg                      res     1
bg                      res     1

row                     res     1
fours                   res     1


                        fit     $1f0



{{
+------------------------------------------------------------------------------------------------------------------------------+
|                                   TERMS OF USE: Parallax Object Exchange License                                             |
+------------------------------------------------------------------------------------------------------------------------------+
|Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    |
|files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    |
|modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software|
|is furnished to do so, subject to the following conditions:                                                                   |
|                                                                                                                              |
|The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.|
|                                                                                                                              |
|THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          |
|WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         |
|COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   |
|ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         |
+------------------------------------------------------------------------------------------------------------------------------+
}}
