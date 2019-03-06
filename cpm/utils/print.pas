{ *** A simple PRINT command for text files.
  *** Sends output via I2C UART.
  *** Text files may have up to 255 character per line.
  *** Martin Hepperle, September 2018
  *** }
Program Print;

Const
  I2C_UART_BASE = $50; { base port address of I2C register set }
  I2C_UART_THR  = $0;  { transmit hold register }
  I2C_UART_LCR  = $3;  { line control register }
  I2C_UART_LSR  = $5;  { line status register }

Type String255 = String[255];

Var
  Status:   Byte;
  Mask:     Byte;
  Data:     Byte;
  i:        Integer;
  theFile:  Text;
  theLine:  String[255];

{ ------------------------------------- }

Function GetBaudRate: Real;
  { --- return the I2C-UART baud rate }
Var
  R:      Byte;

Begin

  { -- save current settings }
  R := Port[I2C_UART_BASE + I2C_UART_LCR];

  { --- allow access to special registers 0 and 1 }
  Port[I2C_UART_BASE + I2C_UART_LCR] := 128;

  { --- set divisor for baud rate (for 14.7456 MHz crystal) }
  GetBaudRate := 921600.0 / ( Port[I2C_UART_BASE + $00] * 1.0 +
                              Port[I2C_UART_BASE + $01] * 256 );

  { --- disallow access special registers 0 and 1 }
  Port[I2C_UART_BASE + I2C_UART_LCR] := R;

End;

{ ------------------------------------- }

Procedure SendByte ( Data: Byte );
{ Wait for UART ready, then place a byte into its FIFO buffer }
Var
  Status: Byte;
Begin
    Repeat
      Status := Port[I2C_UART_BASE + I2C_UART_LSR];
    Until Status and 32 = 32; { THR empty? }
    Port[I2C_UART_BASE + I2C_UART_THR] := Data;
End;

{ ------------------------------------- }

Procedure SendString ( Buffer: String255 );
{ Send a whole string of bytes to the UART }
Var
  i:      Integer;
Begin
    { --- output this line }
    For i:=1 To Length(Buffer) Do
    Begin
      SendByte( Ord(Buffer[i]) );
    End;
End;

{ ------------------------------------- }

Begin

  If GetBaudRate < 100 Then
  Begin
    WriteLn('ERROR: Baud rate not set properly - use MODE first.');
    Exit;
  End;

  If ParamCount > 0 Then
  Begin

    If ParamStr(1) = '/?' Then
    Begin
      WriteLn('*** Usage: PRINT filename');
    End
    Else
    Begin
      Write ( 'Printing ', ParamStr(1), '...' );

      Assign ( theFile, ParamStr(1) );
      Reset ( theFile );

      While not Eof( theFile ) Do
      Begin
        ReadLn ( theFile, theLine );

        { --- append CR/LF for output }
        theLine := theLine + Chr(13) + Chr(10);

        { --- output this line }
        SendString(theLine);
      End;
      WriteLn(' Done.')
    End;
  End
  Else
  Begin
    { --- test output }
    WriteLn('*** Usage: PRINT filename');

    For Data := Ord('A') To Ord('z') Do
    Begin
      { --- check status to avoid THR overrun }
      SendByte ( Data );
    End;
    { terminate line }
    SendByte( 13 );
    SendByte( 10 );
  End;
End.
