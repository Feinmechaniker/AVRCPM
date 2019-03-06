{ *** A I2C bus scanner.
  ***
  *** Detects devices on the bus and outputs their I2C address.
  ***
  *** Martin Hepperle, September 2018
  *** }
Program I2C;

Type String2 = String[2];
Type String8 = String[8];

{ ------------------------------------------------------------ }

Function Bin( Value: Byte ): String8;
Var
  Mask:    Byte;
  Buffer:  String8;
  i:       Integer;
Begin
  Buffer := '11111111';
  Mask := $80;
  i := 1;
  While Mask <> $00  Do
  Begin
    If (Value And Mask) = 0 Then Buffer[i] := '0';
    Mask := Mask shr 1;
    i := i+1;
  End;

  Bin := Buffer;
End;

{ ------------------------------------------------------------ }

Function Hex( Value: Byte ): String2;
Var
  Nibble: Byte;
  Buffer: String2;
Begin
  Buffer := '  ';
  Nibble := Value shr 4;
  If Nibble < 10 Then Buffer[1] := Chr(Nibble+48) Else Buffer[1] := Chr(Nibble+55);
  Nibble := Value and $0F;
  If Nibble < 10 Then Buffer[2] := Chr(Nibble+48) Else Buffer[2] := Chr(Nibble+55);

  Hex := Buffer;
End;

{ ------------------------------------------------------------ }

Var
  i2c:    Integer;
  b:      Byte;
  buffer: array [1..8] of Byte;

Begin

  WriteLn('Scanning I2C bus for devices...');

  Port[$07] := Lo(Addr(buffer));
  Port[$08] := Hi(Addr(buffer));
  buffer[2] := 0;

  For i2c := 0 To 127 Do
  Begin
    buffer[1] := (i2c and $FF) shl 1;
    Port[$06] := 2; { 1 byte slave address, 1 byte to read }
    Port[$05] := 1; { read 1 byte (slave address) }
    Delay(10);
    b := Port[$05]; { get status (ACK) }
    if b = 15 Then
    Begin
      WriteLn('*** Device found at slave address ',
        Hex(i2c),'h = ',i2c, 'd (8-bit: ',
        Hex(i2c*2),'h = ',i2c*2, 'd)' );
    End;
    { WriteLn(i2c, '=', '0x', Hex(i2c),' => ', b, ' ', buffer[1], ' ', buffer[2] ); }
  End;

  WriteLn( 'Done.' );

End.

{ ------------------------------------------------------------ }
