{ *** A simple RECEIVE command for text files.
  *** Receives input via I2C UART.
  *** Text files may have up to 255 character per line.
  ***
  *** Each line must end with aLineFeed character, usually a CR/LF pair.
  *** The file must end with an CTRL-Z character (ASCII 26)
  ***
  *** If you are using a terminal program like TeraTerm, it may be necessary
  *** to add delays between characters and/or line ends to obtain reliable
  *** results when the baud rate is high (e.g. 115200 baud).
  *** You can also type text manually and terminate it with a CTRL-Z.
  ***
  *** Martin Hepperle, September 2018
  *** }
Program Receive;

Const
  I2C_UART_BASE = $50; { base port address of I2C register set }
  I2C_UART_RHR  = $0;  { receive hold register }
  I2C_UART_LCR  = $3;  { line control register }
  I2C_UART_LSR  = $5;  { line status register }

Type String255 = String[255];

Var
  Status:    Byte;
  Mask:      Byte;
  Data:      Byte;
  i:         Integer;
  theFile:   Text;
  theLine:   String255;
  Receiving: Boolean;
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

Function ReceiveByte: Byte;
{ Wait for UART ready, then receive a byte from its FIFO buffer }
Var
  Status: Byte;
Begin
    Repeat
      Status := Port[I2C_UART_BASE + I2C_UART_LSR];
    Until Status and 1 = 1; { data in receiver }
    ReceiveByte := Port[I2C_UART_BASE + I2C_UART_RHR];
End;

{ ------------------------------------- }

Function ReceiveLine: String255;
{ Receive a string of bytes from the UART }
{ The string must be terminale by 0x0A }
Var
  i,j:    Integer;
  c:      Byte;
  Buffer: String255;
Begin
    { --- input one line }
    Buffer := '';
    i := 1;
    While i < 256 Do
    Begin
      c := ReceiveByte;
      Buffer[i] := Chr(c);
      j := i; { save length }

      If (c=10) Or (c=26) Then
      Begin
        i := 255; { Done: LF or CTRL+Z }
      End;
      i := i+1;
    End;

    Buffer[0] := Chr(j);   { string length }
    ReceiveLine := Buffer;
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
      WriteLn('*** Usage: RECEIVE filename');
    End
    Else
    Begin
      Write ( 'Receiving ', ParamStr(1), '...' );

      Assign ( theFile, ParamStr(1) );
      Rewrite ( theFile );

      Receiving := true;

      While Receiving Do
      Begin
        theLine := ReceiveLine;

        { --- output this line }
        Write ( theFile, theLine );

        If Ord(theLine[Length(theLine)]) = 26 Then
        Begin
          Receiving := false;
          Write('CTRL-Z');
          Close ( theFile );
        End;
      End;
      WriteLn(' Done.')
    End;
  End
  Else
  Begin
    WriteLn('*** Usage: RECEIVE filename');
    WriteLn('    Each line sent must end with a CR/LF or a LF.');
    WriteLn('    The file sent must be terminated with a CTRL-Z (Char(26)');
  End;
End.
