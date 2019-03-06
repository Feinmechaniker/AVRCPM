{ *** A simple MODE/STTY command for text files.
  ***
  *** Usage:    MODE baud,parity,databits,stopbits
  *** Examples: MODE 19200,N,8,1
  ***           MODE 38400,E,5,15
  ***           MODE
  *** To set 1.5 stop bits enter '15'
  *** If no arguments are given, the current settings are output.
  ***
  *** Martin Hepperle, September 2018
  *** }
Program Stty;

Const
  I2C_UART_BASE    = $50;  { base port address of I2C register set }
  I2C_UART_THR     = $00;  { transmit hold register }
  I2C_UART_FCR     = $02;  { FIFO control register }
  I2C_UART_LCR     = $03;  { line control register }
  I2C_UART_LSR     = $05;  { line state register }
  I2C_UART_IODIR   = $0A;  { I/O direction register }
  I2C_UART_IOSTATE = $0B;  { I/O state register }
  I2C_UART_IOCR    = $0E;  { I/O control register }

Var
  i,j,p:    Integer;
  Buffer:   String[128];
  Param:    String[20];
  Baud:     Real;
  Code:     Integer;

{ ------------------------------------- }

Procedure DumpRegister ( Register: Integer );
Var
  Status: Byte;
  i:      Integer;
  Buffer: String[20];
Begin
  Status := Port[I2C_UART_BASE + Register];
  Write ( 'UART Register[', Register, '] = ');
  WriteLn ( Status );

  WriteLn ( ' 7 6 5 4 3 2 1 0' );
  Buffer := '';
  For  i := 1 To 8 Do
  Begin
    If ( (Status and 1) = 1 ) Then
      Buffer := ' 1' + Buffer
    Else
      Buffer := ' 0' + Buffer;
    Status := Status Shr 1;
  End;
  WriteLn ( Buffer );
End;

{ ------------------------------------- }

Procedure SetupUART;
Var
  R:     Byte;

Begin

  { --- FIFO Control Register }
  R := Port[I2C_UART_BASE + I2C_UART_FCR];
  { --- reset TX and RX and enable FIFO }
  Port[I2C_UART_BASE + I2C_UART_FCR] := R or $07;

  { --- I/O Control Register }
  R := Port[I2C_UART_BASE + I2C_UART_IOCR];
  { --- use the GPIO pins for I/O, not as modem lines }
  Port[I2C_UART_BASE + I2C_UART_IOCR] := R or $01;

  { --- GPIO Direction Register }
  { --- use all GPIOs for input }
  Port[I2C_UART_BASE + I2C_UART_IODIR] := $00;

End;

{ ------------------------------------- }

Procedure SetIODir ( Pin: Integer; Dir: Char );
{ set the direction of the given pin }
{ Pin: 0...7 }
{ Dir: 'i'=input, 'O'=output }
Var
  R:     Byte;

Begin

  { --- get current content of GPIO Direction Register }
  R := Port[I2C_UART_BASE + I2C_UART_IODIR];

  If UpCase(Dir) = 'O' Then
    { --- use this pin for output }
    Port[I2C_UART_BASE + I2C_UART_IODIR] := R or (1 shl Pin)
  Else
    { --- use this pin for input }
    Port[I2C_UART_BASE + I2C_UART_IODIR] := R and (not (1 shl Pin));

End;

{ ------------------------------------- }

Procedure SetBaudRate ( BaudRate: Real );
  { --- setup of I2C-UART baud rate }
  { BaudRate: 50 ... 115200 }
Var
  R:     Byte;
  Divisor: Integer;

Begin

  { -- save current settings }
  R := Port[I2C_UART_BASE + I2C_UART_LCR];
  { --- allow access special registers 0 and 1 }
  Port[I2C_UART_BASE + I2C_UART_LCR] := 128;

  { --- set divisor for baud rate (for 14.7456 MHz crystal) }
  { XTALFREQ / (BaudRate * 16) = 14745600 / BaudRate / 16 = 921600 / BaudRate }
  Divisor := Trunc( 9216 / (BaudRate/100) );
  Port[I2C_UART_BASE + $00] := Divisor and $FF;
  Port[I2C_UART_BASE + $01] := (Divisor shr 8) and $FF;

  { --- disallow access special registers 0 and 1 }
  Port[I2C_UART_BASE + I2C_UART_LCR] := R;

End;

{ ------------------------------------- }

Function GetBaudRate: Real;
  { --- return the I2C-UART baud rate }
Var
  R:      Byte;

Begin

  { -- save current settings }
  R := Port[I2C_UART_BASE + I2C_UART_LCR];

  { --- allow access special registers 0 and 1 }
  Port[I2C_UART_BASE + I2C_UART_LCR] := 128;

  { --- set divisor for baud rate (for 14.7456 MHz crystal) }
  GetBaudRate := 921600.0 / ( Port[I2C_UART_BASE + $00] * 1.0 +
                              Port[I2C_UART_BASE + $01] * 256 );

  { --- disallow access special registers 0 and 1 }
  Port[I2C_UART_BASE + I2C_UART_LCR] := R;

End;

{ ------------------------------------- }

Procedure SetBitCount ( BitCount: Integer );
  { --- setup of I2C-UART bit count }
  { BitCount: 5,6,7,8 }

Var
  R: Byte;

Begin

  { -- get current settings }
  R := Port[I2C_UART_BASE + I2C_UART_LCR];

  R := R and $FC; { clear bits 0 and 1: == default of 5 bit }

  Case BitCount Of
    5: Begin End;                  { 5 bit, alread set }
    6: Begin R := R or 1; End; { 6 bit }
    7: Begin R := R or 2; End; { 7 bit }
    8: Begin R := R or 3; End; { 8 bit }
    Else Begin WriteLn('Unknown bit count: ',BitCount); End;
  End;

  Port[I2C_UART_BASE + I2C_UART_LCR] := R;

End;

{ ------------------------------------- }

Function GetBitCount: Integer;
  { --- return the I2C-UART bit count }
  { BitCount: 5,6,7,8 }

Var
  R: Byte;

Begin

  { -- get current settings }
  R := Port[I2C_UART_BASE + I2C_UART_LCR];

  R := R and $03; { mask bits 0 and 1 }

  Case R Of
    0: Begin GetBitCount := 5; End; { 5 bit }
    1: Begin GetBitCount := 6; End; { 6 bit }
    2: Begin GetBitCount := 7; End; { 7 bit }
    3: Begin GetBitCount := 8; End; { 8 bit }
  End;

End;

{ ------------------------------------- }

Procedure SetParity ( Parity: Char );
  { --- setup of I2C-UART Parity bits }
  { Parity: 'n', 'e', 'o', '0', 'S', '1', 'M' }
Var
  R: Byte;

Begin

  { -- get current settings }
  R := Port[I2C_UART_BASE + I2C_UART_LCR];

  R := R and $C7; { clear bits 3,4 and, 5 }

  Parity := UpCase(Parity);
  Case Parity Of
    'N':     Begin R := R or $00; End; { xx0--- none }
    'E':     Begin R := R or $18; End; { 011--- even }
    'O':     Begin R := R or $08; End; { 001--- odd }
    '1','M': Begin R := R or $28; End; { 101--- mark, forced 1 }
    '0','S': Begin R := R or $38; End; { 111--- space, forced 0 }
    Else Begin WriteLn('Unknown parity: ',Parity); End;
  End;

  Port[I2C_UART_BASE + I2C_UART_LCR] := R;

End;

{ ------------------------------------- }

Function GetParity: Char;
  { --- return the I2C-UART Parity bits }
  { Parity: 'n', 'e', 'o', '0', '1' }
Var
  R: Byte;

Begin

  { -- get current settings }
  R := Port[I2C_UART_BASE + I2C_UART_LCR];

  R := (R and $38) shr 3; { mask bits 3,4 and, 5 }

  If (R and $01) = 0 Then
  Begin
    GetParity := 'N';                 { xx0--- none }
  End
  Else
  Begin
    Case R Of
      1: Begin GetParity := 'O'; End; { 001--- odd }
      3: Begin GetParity := 'E'; End; { 011--- even }
      5: Begin GetParity := 'M'; End; { 101--- 1, mark }
      7: Begin GetParity := 'S'; End; { 111--- 0, space }
    End
  End;

End;

{ ------------------------------------- }

Procedure SetStopBits ( StopBits: Byte );
  { --- setup of I2C-UART Stop bits }
  { StopBits: 1,15,2 (must be compatible to BitCount) }
Var
  R: Byte;

Begin

  { -- get current settings }
  R := Port[I2C_UART_BASE + I2C_UART_LCR];

  R := R and $FB; { clear bit 2 }

  Case StopBits Of
    1:    Begin R := R or $00; End; { 0-- 1   for 5,6,7,8 bits }
    2:    Begin R := R or $04; End; { 1-- 1.5 for 5 bits }
    15:   Begin R := R or $04; End; { 1-- 2   for 6,7,8 bits }
    Else Begin WriteLn('Unknown stop bit count: ',StopBits); End;
  End;

  Port[I2C_UART_BASE + I2C_UART_LCR] := R;

End;

{ ------------------------------------- }

Function GetStopBits: Byte;
  { --- return the I2C-UART Stop bits }
  { StopBits: 1,15,2 (must be compatible to BitCount) }
Var
  R: Byte;

Begin

  { -- get current settings }
  R := Port[I2C_UART_BASE + I2C_UART_LCR];

  R := R and $04; { mask bit 2 }

  Case R Of
    0:   Begin GetStopBits := 1; End;  { 0-- 1   for 5,6,7,8 bits }
    4:   Begin
           If GetBitCount = 5 Then GetStopBits := 15
                              Else GetStopBits := 2;
         End;
  End;

End;

{ ------------------------------------- }

Procedure SetIOPin( Pin, State: Integer );
  { --- set the state of an I2C-UART I/O pin }
  { Pin: 0...7
    State: 0,1 }
Var
  R: Byte;

Begin

  { -- get current settings }
  R := Port[I2C_UART_BASE + I2C_UART_IOSTATE];

  { mask pin }
  If State = 1 Then
    R := R or (1 shl Pin)
  Else
    R := R and (not (1 shl Pin));

  Port[I2C_UART_BASE + I2C_UART_IOSTATE] := R;

End;

{ ------------------------------------- }

Function GetIOPin( Pin: Integer): Integer;
  { --- return the state of an I2C-UART I/O pin }
  { Pin: 0...7 }
Var
  R: Byte;

Begin

  { -- get current settings }
  R := Port[I2C_UART_BASE + I2C_UART_IOSTATE];

  { mask pin }
  If R and (1 shl Pin) = 0 Then
    GetIOPin := 0
  Else
    GetIOPin := 1;

End;

{ ------------------------------------- }

Begin

  If ParamCount > 0 Then
  Begin

    { perform basic initialization }
    SetupUART;

    Buffer := ParamStr(1) + ',';
    p := 1;
    i := 1;
    For j:=1 To Length(Buffer) Do
    Begin
      If Buffer[j] = ',' Then
      Begin
        Param := Copy(ParamStr(1),i,j-i);

        If Param = '/?' Then
        Begin
          WriteLn('*** Usage:    MODE baud,parity,databits,stopbits');
          WriteLn('*** Examples: MODE 19200,N,8,1');
          WriteLn('***           MODE 38400,E,5,15');
          WriteLn('***           MODE');
          WriteLn('*** To set 1.5 stop bits enter "15"');
          WriteLn('*** If no arguments are given, the current settings are output.');
        End
        Else
        Begin
          Case p Of
            1: Begin Val(Param,Baud,Code); SetBaudRate(Baud); End;
            2: Begin SetParity(Param[1]);                     End;
            3: Begin Val(Param,i,Code);    SetBitCount(i);    End;
            4: Begin Val(Param,i,Code);    SetStopBits(i);    End;
          End;
        End;
        p := p+1;
        i := j+1;
      End;
    End;
  End
  Else
  Begin
     { create left aligned intere representation of baud rate }
     Str(GetBaudRate:6:0,Buffer);
     i:=1;
     While Buffer[i] = ' ' Do
     Begin
       i := i+1;
     End;
     WriteLn ( 'Speed    = ', Copy(Buffer,i,Length(Buffer)-i+1) );
     WriteLn ( 'Parity   = ', GetParity );
     WriteLn ( 'BitCount = ', GetBitCount );
     WriteLn ( 'StopBits = ', GetStopBits );
     WriteLn ( 'I/O Pins = ', GetIOPin(0),GetIOPin(1),GetIOPin(2),GetIOPin(3),
                              GetIOPin(4),GetIOPin(5),GetIOPin(6),GetIOPin(7) );
  End;
End.

{ ------------------------------------- }
