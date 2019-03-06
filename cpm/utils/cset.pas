{*****************************************************************************}
{                              character set tools                            }
{                                   Version 1.0                               }
{ Program : CSET                                                              }
{ Author  : Joe G.                                                            }
{ Update  : 06.03.2019                                                        }
{*****************************************************************************}

Program CSET;

Type
 stringT_3 = string[3];

Var
 param1 : stringT_3;
 param2 : stringT_3;

{----------------------------------------------------------------------------}
Procedure get_param;                            { get command line parameter }

Var i : byte;

Begin
 i := Paramcount;
 if (i < 3) AND (i > 1) then
  begin
   param1 := paramstr(1);
   param2 := paramstr(2);
  end
  else
   begin
    writeln('ERROR: not enough parameters');
    Halt;
   end;
End;
{----------------------------------------------------------------------------}

Procedure set_g0(temp:char);
Begin
 write(char(27));   { ESC  }
 write(char(40));   { (    }
 write(temp);       { temp }
End;

Procedure set_g1(temp:char);
Begin
 write(char(27));   { ESC  }
 write(char(41));   { )    }
 write(temp);       { temp }
End;

Begin
 get_param;
 if param1 = 'UK' then set_G0('A');
 if param2 = 'UK' then set_G1('A');
 if param1 = 'US' then set_G0('B');
 if param2 = 'US' then set_G1('B');
 if param1 = 'DE' then set_G0('K');
 if param2 = 'DE' then set_G1('K');
 if param1 = 'DEC' then set_G0('0');
 if param2 = 'DEC' then set_G1('0');
 if param1 = 'SUP' then set_G0('<');
 if param2 = 'SUP' then set_G1('<');
 writeln('G0 and G1 is set');
End.