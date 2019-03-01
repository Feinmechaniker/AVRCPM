Stand: v1.7.4
=============

    Ps  Numerischer, optionaler Parameter. bestehend aus einer oder mehrerer Ziffern.
    Pm  Durch ; getrennte Folge von numerischen Parametern Ps.


###   Control characters
##### C0 characters
    BEL                     Generates bell tone.
    BS                      Moves cursor to the left one character position; if cursor is at left margin, no action occurs.
    HT                      Moves cursor to next tab stop, or to right margin if there are no more tab stops.
    CR                      Moves cursor to left margin on current line.
    LF                      Causes a linefeed.
    VT                      Processed as LF.
    FF                      Processed as LF.
    SO                      Invoke G1 character set.
    SI                      Invoke G0 character set. (default)
    CAN                     If received during an escape or control sequence, cancels the sequence.
    SUB                     Processed as CAN.
    ESC                     Processed as a sequence introducer.

##### C1 characters
    IND                     ESC D   Index
    NEL                     ESC E   Next line
    RI                      ESC M   Reverse index
    CSI                     ESC [   Control sequence introducer
    ST                      ESC \   String terminator

### Escape sequences
    ESC D                   IND     Linefeed.
    ESC E                   NEL     Newline.
    ESC M                   RI      Reverse linefeed. (Curser up)
    ESC Z                   DECID   DEC private identification. The terminal returns  ESC [ ? 6 c, claiming that it is a VT102.
    ESC c                   RIS     Reset to Initial State (Power Up)
    ESC ( A                 SCS     Select Character Set UK  --> G0
    ESC ( B                 SCS     Select Character Set US ASCII --> G0
    ESC ( K                 SCS     Select Character Set DE  --> G0
    ESC ( <                 SCS     Select Character Set DEC supplemental  --> G0
    ESC ( 0                 SCS     Select Character Set DEC special characters and line drawing set --> G0
    ESC ) A                 SCS     Select Character Set UK  --> G1
    ESC ) B                 SCS     Select Character Set US ASCII  --> G1
    ESC ) K                 SCS     Select Character Set DE  --> G1
    ESC ) <                 SCS     Select Character Set DEC supplemental  --> G1
    ESC ) 0                 SCS     Select Character Set DEC special characters and line drawing set  --> G1
    ESC * A                 SCS     Select Character Set UK  --> G2
    ESC * B                 SCS     Select Character Set US ASCII  --> G3
    ESC * K                 SCS     Select Character Set DE  --> G3
    ESC * <                 SCS     Select Character Set DEC supplemental  --> G3
    ESC * 0                 SCS     Select Character Set DEC special characters and line drawing set  --> G3
    ESC + A                 SCS     Select Character Set UK  --> G4
    ESC + B                 SCS     Select Character Set US ASCII  --> G4
    ESC + K                 SCS     Select Character Set DE  --> G4
    ESC + <                 SCS     Select Character Set DEC supplemental  --> G4
    ESC + 0                 SCS     Select Character Set DEC special characters and line drawing set  --> G4
    ESC 7                   DECSC   Save current state (cursor coordinates, attributes, character sets pointed at by G0, G1).
    ESC 8                   DECRC   Restore state most recently saved by ESC 7.
    ESC # 8                 DECALN  DEC screen alignment test - fill screen with E's.
    ESC # 9                 Show Font (Debug)
    ESC ~                   LS1R    Invoke G1 into GR (VT200 mode only).
    ESC n                   LS2     Invoke G2 into GL (VT200 mode only).
    ESC }                   LS2R    Invoke G2 into GR (default) (VT200 mode only).
    ESC o                   LS3     Invoke G3 into GL (VT200 mode only).
    ESC |                   LS3R    Invoke G3 into GR (VT200 mode only).

### Control sequences
    ESC [ Ps @              ICH     Insert Ps blank characters  (defaut = 1) (VT220)
    ESC [ Ps A              CUU     Move cursor up
    ESC [ Ps B              CUD     Move cursor down
    ESC [ Ps C              CUF     Move cursor right
    ESC [ Ps D              CUB     Move cursor left
    ESC [ Ps ; Ps H         CUP     Move cursor to [row; column] (default = [1,1])
    ESC [ Ps J              ED      Erase in Display
            Ps = 0          Erase below (default)
            Ps = 1          Erase above
            Ps = 2          Erase all
    ESC [ Ps K              EL      Erase in line
            PS = 0          EL      Erase to right (default)
            PS = 1          EL      Erase to left
            PS = 2          EL      Erase all
    ESC [ Ps L              IL      Insert Ps lines (defaut = 1)
    ESC [ Ps M              DL      Delete Ps lines (defaut = 1)
    ESC [ Ps P              DCH     Delete Ps characters (defaut = 1)
    ESC [ Ps X              ECH     Erase Ps characters  (defaut = 1) (VT220)
    ESC [ Ps c              DA      Send device attributes
            PS = 0 or ommitted      Request attributes from terminal
                                      Answer ESC [ ? 6 c: "I am a VT102")
    ESC [ Ps ; Ps f         HVP     Move cursor to [row; column] (default = [1,1])
    ESC [ Pm h              SM      Set Mode
            Ps = 4          IRM     Set insert mode
            Ps = 20         LNM     Automatic Newline
    ESC [ ? Pm h            DECSET  DEC Private Mode Set
            Ps = 6          DECOM   Origin Mode Relative
            PS = 7          DECAWM  Set autowrap on (default)
            PS = 25         DECTECM Cursor visible
    ESC [ Pm l              RM      Reset Mode
            Ps = 4          IRM     Set replace mode (default)
            Ps = 20         LNM     Normal Linefeed (default)
    ESC [ ? Pm l            DECRST  DEC Private Mode Reset
            Ps = 6          DECOM   Origin Mode Absolute
            PS = 7          DECAWM  Set autowrap off
            PS = 25         DECTECM Cursor not visible
    ESC [ Pm m              SGR     Set character attributes
            Ps = 0          All attributes off
            Ps = 1          bold (increased intensity)
            Ps = 4          underline (simulated with color)
            Ps = 7          negative (reverse) image
            Ps = 10         Select font 0 (Debug/experimental)
            Ps = 11         Select font 1 (Debug/experimental)
            Ps = 22         bold off (normal intensity)
            Ps = 24         not underlined
            Ps = 27         positive image
            Ps = 30         Set foreground colorto black
            Ps = 31         Set foreground colorto red
            Ps = 32         Set foreground colorto green
            Ps = 33         Set foreground colorto yellow
            Ps = 34         Set foreground colorto blue
            Ps = 35         Set foreground colorto magenta
            Ps = 36         Set foreground colorto cyan
            Ps = 37         Set foreground colorto white
            Ps = 40         Set background colorto black
            Ps = 41         Set background colorto red
            Ps = 42         Set background colorto green
            Ps = 43         Set background colorto yellow
            Ps = 44         Set background colorto blue
            Ps = 45         Set background colorto magenta
            Ps = 46         Set background colorto cyan
            Ps = 47         Set background colorto white
    ESC [ Ps n              DSR     Device Status report
            Ps = 5          DSR     Device status report: Answer is ESC [ 0 n (Terminal OK).
            Ps = 6          CPR     Cursor position report: Answer is ESC [ y ; x R
    ESC [ Ps ; Ps r         DECSTBM Set Scrolling Region [top;bottom]



TODO: (or not todo)
-----
### Control characters

    ENQ:                    Transmits answerback message.
    DC1                     Processed as XON. DC1 causes terminal to continue transmitting characters.
    DC3                     Processed as XOFF. DC3 causes terminal to stop transmitting all characters except XOFF and XON.
                            DC3 can also be selected as a half-duplex turnaround character.

### Escape sequences


    ESC H     HTS           Set tab stop at current column.
    ESC >     DECPNM        Set numeric keypad mode
    ESC =     DECPAM        Set application keypad mode
    ESC N     SS2           Single shift 2
    ESC O     SS3           Single shift 3


### Control sequences
    ESC [ Ps g              TBC     Tab Clear
            Ps = 0          Clear Current Column (default).
            Ps = 3          Clear All.
    ESC [ Pm m              SGR     Set character attributes
            PS = 5          Set blink
            PS = 25         Blink off (steady)
            Ps = 39         Set foreground color to default (original).
            Ps = 49         Set background color to default (original).

    ESC [ Ps q              DECLL   Load LEDs
            Ps = 0          Clear all LEDS (default).
            Ps = 1          Light Num Lock.
            Ps = 2          Light Caps Lock.
            Ps = 3          Light Scroll Lock.
            Ps = 2  1       Extinguish Num Lock.
            Ps = 2  2       Extinguish Caps Lock.
            Ps = 2  3       Extinguish Scroll Lock.
    ESC [ Ps SP q           DECSCUSR  Set cursor style (VT520).
            Ps = 0          blinking block.
            Ps = 1          blinking block (default).
            Ps = 2          steady block.
            Ps = 3          blinking underline.
            Ps = 4          steady underline.
            Ps = 5          blinking bar (xterm).
            Ps = 6          steady bar (xterm).
                                      (default = full size of window)
    ESC [ ? Pm h            DECSET      DEC Private Set Mode Set
            Ps = 3          DECCOLM     (default off = 80 columns): 80/132 col mode switch.
            Ps = 5          DECSCNM     (default off): Set reverse-video mode.

### Linux Console Private CSI Sequences
    ESC [ 1 ; n ]          Set color n as the underline color
    ESC [ 2 ; n ]          Set color n as the dim color
    ESC [ 8 ]              Make the current color pair the default attributes.
