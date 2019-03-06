program RTCDemo;
{$C- } {Sonst funktioniert 'keypressed' nicht}

{---------------------------------------------------------------------
        AVRCPM 
        Test und Demonstration des I2C-Interface
        I2C Uhrenchip PCF8583 
        Zeit und Datum lesen und schreiben

        Moegliche Erweiterung (TODO:):
        - Control/Status-Register und Alarmregister lesen/schreiben
        - RAM lesen/schreiben

        $Id: RTCDEMO.PAS 195 2013-02-11 21:34:04Z leo $
----------------------------------------------------------------------}
  
const
  I2CCMD    = 5;        {adr of I2C Command Port (1=read, 2=write)}
  I2CMSGLEN = 6;        {Transferpuffergroesse}
  I2CADRL   = 7;        {Transferpufferadresse low/high}
  I2CADRH   = 8;

  I2C_CMD_Read  = 1;    {I2C Read Command}
  I2C_CMD_Write = 2;    {I2C Write Command}

  { TP Delay Loop }
  T100ms = 100;         {20MHz AVR + Config fuer 3 MHz Z80 (TINST)}
  {T100ms = 75;}        {20MHz AVR + Config fuer 4 MHz Z80 (TINST default)}

type
  I2CBufLen = 0..16;

  CalTime = record
    sec: 0..59;
    min: 0..59;
    hrs: 0..23;
    day: 1..31;
    month: 1..12;
    year: integer;
  end;

  DTinput = array [1..3] of integer;

var
  msgbuf : array[0..16] of Byte;        {TODO: Transferbuffer nicht global}

  Time : CalTime;
  Done: boolean;

{---------------------------------------------------------------------
        Debugging: Print 16 byte RAM ab adr 
----------------------------------------------------------------------}
procedure hexdump(adr: integer);
var
  i: integer;
  c: byte;

  function hexdigit(c: byte): char;
  begin
    if c < 10 then
      hexdigit := char(c + $30)
    else
      hexdigit := char(c - 10 + $41);
  end;

{TODO: print adr }
begin
  for i := 0 to 15 do
  begin
    c := Mem[adr+i];
    write(hexdigit(c shr 4), hexdigit(c and $f), ' ');
  end;
  writeln;
end;

{---------------------------------------------------------------------
        I2C - Routinen
----------------------------------------------------------------------}
procedure i2c_init;
var
  i: integer;
begin
  msgbuf[0] := $A0;     {I2C-Adresse des RTC-Chips}

  Port[I2CADRH] := Hi(Addr(msgbuf));
  Port[I2CADRL] := Lo(Addr(msgbuf));
end;

procedure i2c_write(len:Byte);
begin
  Port[I2CMSGLEN] := len;
  Port[I2CCMD] := I2C_CMD_Write;
end;

procedure i2c_read(len:Byte);
begin
  Port[I2CMSGLEN] := len;
  Port[I2CCMD] := I2C_CMD_Read;
end;


function BCDtoBINbyte(i: byte): byte;
begin
  BCDtoBINbyte := (i div 16) * 10 + (i and $F);
end;

function BINtoBCDbyte(i: byte): byte;
begin
  BINtoBCDbyte := (i div 10) * 16 + (i mod 10);
end;

procedure ReadRTC(var t: CalTime);
begin
  msgbuf[1] := 2;
  i2c_write(2);
  i2c_read(6);
  with t do
  begin
    sec  := BCDtoBINbyte(msgbuf[1]);
    min  := BCDtoBINbyte(msgbuf[2]);
    hrs  := BCDtoBINbyte(msgbuf[3] and $3F);
  end;
  msgbuf[1] := $10;
  i2c_write(2);
  i2c_read(3);
  with t do
  begin
    day  := BCDtoBINbyte(msgbuf[4] and $3F);
    month:= BCDtoBINbyte(msgbuf[5] and $1F);
    year := msgbuf[1] + 256*msgbuf[2];
    while Lo(year) and $3 <> (msgbuf[4] shr 6) do
      year := year + 1;
  end;
end;


procedure WriteRTC(t: CalTime);
begin
  with t do begin
    msgbuf[1] := 1;                   {register address}
    msgbuf[2] := 0;                   {hundredth of sec}
    msgbuf[3] := BINtoBCDbyte(sec);
    msgbuf[4] := BINtoBCDbyte(min);
    msgbuf[5] := BINtoBCDbyte(hrs);
    msgbuf[6] := BINtoBCDbyte(day) + (Lo(year) shl 6);
    msgbuf[7] := BINtoBCDbyte(month); {TODO: weekdays}
  end;
  i2c_write(8);

  msgbuf[1] := $10;                  {register address}
  with t do begin
    msgbuf[2] := Lo(year);
    msgbuf[3] := Hi(year);
  end;
  i2c_write(4);
end;

{---------------------------------------------------------------------
        Dialog Routinen
----------------------------------------------------------------------}

procedure PrintTime(t: CalTime);
begin
  with t do             {TODO: Fuehrende '0' statt ' '}
  begin
    write('Zeit: ', hrs:2, ':',min:2, ':',sec:2, '  ',
          'Datum: ', day, '.', month, '.', year);
  end;
end;

procedure PrintMenu;
begin
  ClrScr;
  writeln('Menu:');
  writeln(' T) Zeit setzen');
  writeln(' D) Datum setzen');
  writeln(' Q) Quit');
  writeln('> ');
end;

function GetKey: char;
var
  c: char;
begin
  Read(Kbd, c);
  GetKey := c;
end;

{Daemliche input routine ohne Fehlerpruefung (TODO:)}
procedure GetDateTime(var a: DTinput; sep: char);
var
  line: String[80];
  s: String[4];
  i,j,k: integer;
  rc: integer;

begin
  readln(line);
  line := line + sep;
  j := 1;
  for i := 1 to 3 do
  begin
    k := 1;
    while line[k+j] in ['0'..'9'] do
      k := k + 1;
    s := Copy(line, j, k);
    val(s, a[i], rc);
    j := j + k + 1;
  end;
end;

procedure SetTime;
var
  a: DTinput;

begin
  write('Zeit (hh:mm:ss): ');
  a[1] := -1;
  GetDateTime(a, ':');
{  writeln('intime:', a[1], ':', a[2], ':', a[3]);
}
  if (a[1] in [0..24]) and (a[2] in [0..59]) and (a[3] in [0..59]) then
  begin
    ReadRTC(Time);
    Time.hrs  := a[1];
    Time.min  := a[2];
    Time.sec  := a[3];
    WriteRTC(Time);
  end else
  begin
    writeln('Fehler in Eingabe.');
  end;
end;

procedure SetDate;
var
  a: DTinput;

begin
  write('Datum (TT.MM.JJJJ): ');
  a[1] := -1;
  GetDateTime(a, '.');
{  writeln('indate:', a[1], '.', a[2], '.', a[3]);
}
  if (a[1] in [1..31]) and (a[2] in [1..12]) and (a[3] > 0) and (a[3] <= 2076) then
  begin
    ReadRTC(Time);
    Time.day  := a[1];
    Time.month:= a[2];
    Time.year := a[3];
    WriteRTC(Time);
  end else
  begin
    writeln('Fehler in Eingabe.');
  end;
end;

procedure CleanConsolebuffer;
var
  c: char;
begin
  while keypressed do c := GetKey;
end;

{---------------------------------------------------------------------
        main
----------------------------------------------------------------------}

begin
  PrintMenu;
  i2c_init;

  writeln;
  Done := False;
  repeat
    Delay(T100ms);
    ReadRTC(Time);
    GotoXY(35,1); PrintTime(Time); ClrEol;
    gotoXY(2,5);  ClrEol;

    if keypressed then
    begin
      gotoXY(1,7); ClrEol;
      gotoXY(1,6); ClrEol;
      case UpCase(GetKey) of
       'T': SetTime;
       'D': SetDate;
       'Q': Done := True;
      end;
    end;
  until Done;
  CleanConsolebuffer;
end.
