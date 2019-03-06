{ ***
  *** Exerciser for I2C GPIO chip MCP23017
  ***
  *** Martin Hepperle, 2018
  *** }

Program MCP23017;

Const
  I2C_BASE = $60;
  _IODIRA = $00;  _IODIRB = $01;
  _IPOLA  = $02;  _IPOLB  = $03;
  _GPPUA  = $0C;  _GPPUB  = $0D;
  _GPIOA  = $12;  _GPIOB  = $13;
  _OLATA  = $14;  _OLATB = $15;

Type String2 = String[2];
Type String8 = String[8];

{ ------------------------------------------------------------ }

Function Bin( Value: Byte ): String8;
{ Convert a byte to a string of eight '0' and '1' characters }
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
{ Convert a byte to a string of two hex characters [0-9,A-F]  }
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

Procedure DumpRegister ( Register: Byte );
{ dump a register in human readable verbose form }
Var
  Status: Byte;
  i,n:    Integer;
  Buffer: String[80];
  Number: String[20];
  S2:     String2;
Begin
  Status := Port[I2C_BASE + Register];
  Str(Register,Number);
  S2 := Hex(Register);
  Buffer := 'Register[' + Number + ':0x' + S2 + '] = ';
  Str(Status,Number);
  Buffer := Buffer + Number + ' = ';
  n := Length(Buffer);

  { bits are spaced for better readability }
  Buffer := Buffer + ' 7 6 5 4 3 2 1 0';
  WriteLn ( Buffer );
  Buffer := '';
  For  i := 1 To 8 Do
  Begin
    If ( (Status and 1) = 1 ) Then
      Buffer := ' 1' + Buffer
    Else
      Buffer := ' 0' + Buffer;
    Status := Status Shr 1;
  End;
  Buffer := Copy('                          ',1,n) + Buffer;
  WriteLn ( Buffer );
End;

{ ------------------------------------------------------------ }

Var
  i:      Integer;
  b:      Byte;
  DEBUG:  Boolean;

Begin

  { Configure the two ports
    After power-up all bits in _IODIRA/B are input = '1'
    and all other registers are '0'
    This means that IOCON.BANK = 0 which affects how the chip
    interprets the the register indices }

  DEBUG := false;

  { --------------------------------- }
  If DEBUG Then
  Begin

  { GPIOA: all input }
  Port[I2C_BASE + _IODIRA] := $FF;
  { GPIOB: all output }
  Port[I2C_BASE + _IODIRB] := $00;

  WriteLn('--- bits in GPIOA (0x13) should be 00000000');

  WriteLn('--- setting GPIOB (0x13) to 10101010');
  { set a bit pattern to GPIOB }
  Port[I2C_BASE + _GPIOB] := $AA;

  { show what we have got }
  For i:=0 To 21 Do
    DumpRegister(i);

  { flip input polarity if GPIOA }
  { R12: what was 00000000 will now be 11111111 }
  WriteLn('--- flipping bits in GPIOA (0x12) to 11111111');
  Port[I2C_BASE + _IPOLA] := $FF;

  WriteLn('--- setting GPIOB (0x13) to 01010101');
  { toggle bit pattern to GPIOB }
  Port[I2C_BASE + _GPIOB] := $55;

  WriteLn('--- GPIOA and GPIOB:');
  { GPIOA and B }
  DumpRegister($12);
  DumpRegister($13);

  WriteLn('--- Latches for GPIOA and GPIOB:');
  { latches should reflect the same data }
  DumpRegister($14);
  DumpRegister($15);
  b := Port[I2C_BASE + _GPIOB];
  WriteLn('--- Latches for GPIOA and GPIOB:');
  { latches should reflect the same data }
  DumpRegister($14);
  DumpRegister($15);

  End; { DEBUG }
  { --------------------------------- }

  ClrScr;

  { --- define the input/output roles of GPIOA and GPIOB }

  { GPIOA: all input }
  Port[I2C_BASE + _IODIRA] := $FF;
  { GPIOB: all output }
  Port[I2C_BASE + _IODIRB] := $00;

  GotoXY(1,1);
  Write('===== MCP23017 16-port GPIO =====');

  GotoXY(2,3);
  Write('N');
  GotoXY (10,3);
  Write ( ' GPIO A       GPIO B' );

  GotoXY(1,8);
  Write('IODIR:');
  GotoXY(10,8);
  Write( Bin(Port[I2C_BASE + _IODIRA]) );
  Write('     ');
  Write( Bin(Port[I2C_BASE + _IODIRB]) );
  GotoXY(40,8);
  Write('0=output, 1=input');

  GotoXY(1,9);
  Write('IPOL:');
  GotoXY(10,9);
  Write( Bin(Port[I2C_BASE + _IPOLA]) );
  Write('     ');
  Write( Bin(Port[I2C_BASE + _IPOLB]) );
  GotoXY(40,9);
  Write('0=normal, 1=inverted (input only)');

  GotoXY(1,10);
  Write('GPPU:');
  GotoXY(10,10);
  Write( Bin(Port[I2C_BASE + _GPPUA]) );
  Write('     ');
  Write( Bin(Port[I2C_BASE + _GPPUB]) );
  GotoXY(40,10);
  Write('0=disabled, 1=enabled (input only)');

  { --- toggle alternating bit pattern in GPIOB and then let one bit walk right-to-left }

  For i:=1 To 16 Do
  Begin
    GotoXY(1,4);
    Write(i);
    GotoXY (10,4);
    Write ( Bin(Port[I2C_BASE + _GPIOA]) );
    Write ( '     ' );
    Write ( Bin(Port[I2C_BASE + _GPIOB]) );
    If i < 8 Then
    Begin
      { toggle bit pattern to GPIOB }
      Port[I2C_BASE + _GPIOB] := not Port[I2C_BASE + _GPIOB] ;
    End
    Else
    Begin
      { toggle bit pattern to GPIOB }
      Port[I2C_BASE + _GPIOB] := 1 shl (i mod 8);
    End;
    GotoXY(1,23);
    Delay(500);
  End;

  For i:=1 To 8 Do
  Begin
    { toggle input polarity of GPIOA }
    Port[I2C_BASE + _IPOLA] := not Port[I2C_BASE + _IPOLA];
    GotoXY(10,9);
    Write( Bin(Port[I2C_BASE + _IPOLA]) );
    Write('     ');
    Write( Bin(Port[I2C_BASE + _IPOLB]) );
    GotoXY (10,4);
    Write ( Bin(Port[I2C_BASE + _GPIOA]) );
    Write ( '     ' );
    Write ( Bin(Port[I2C_BASE + _GPIOB]) );
    GotoXY(1,23);
    Delay(500);
  End;



  { --- now swap the input/output role of GPIOA and GPIOB }

  { GPIOA: all output }
  Port[I2C_BASE + _IODIRA] := $00;
  { GPIOB: all input }
  Port[I2C_BASE + _IODIRB] := $FF;

  GotoXY(10,8);
  Write( Bin(Port[I2C_BASE + _IODIRA]) );
  Write('     ');
  Write( Bin(Port[I2C_BASE + _IODIRB]) );

  { --- toggle alternating bit pattern in GPIOA and then let one bit walk right-to-left }

  Port[I2C_BASE + _GPIOA] := $0F;

  For i:=1 To 16 Do
  Begin
    GotoXY(1,4);
    Write(i);
    GotoXY (10,4);
    Write ( Bin(Port[I2C_BASE + _GPIOA]) );
    Write ( '     ' );
    Write ( Bin(Port[I2C_BASE + _GPIOB]) );
    If i < 8 Then
    Begin
      { toggle bit pattern to GPIOB }
      Port[I2C_BASE + _GPIOA] := not Port[I2C_BASE + _GPIOA] ;
    End
    Else
    Begin
      { toggle bit pattern to GPIOB }
      Port[I2C_BASE + _GPIOA] := 1 shl (i mod 8);
    End;
    GotoXY(1,23);
    Delay(500);
  End;

  GotoXY(1,23);

End.

{ ------------------------------------------------------------ }
