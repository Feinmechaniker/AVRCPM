{*****************************************************************************}
{                              character set tools                            }
{                                   Version 1.0                               }
{ Program : PSCS                                                              }
{ Author  : Joe G.                                                            }
{ Update  : 06.03.2019                                                        }
{*****************************************************************************}

Program PSCS;

Var i : Byte;

Begin
 clrscr;
 writeln('Print selected Charater Set'); writeln;

 write(char(15));                                                 { switch SI }
 Writeln('select G0 (SI)'); writeln;

 writeln('  0 1 2 3 4 5 6 7 8 9 A B C D E F'); Writeln;
 write('2 '); for i := 32  to 47  do write(char(i),' '); writeln;
 write('3 '); for i := 48  to 63  do write(char(i),' '); writeln;
 write('4 '); for i := 64  to 79  do write(char(i),' '); writeln;
 write('5 '); for i := 80  to 95  do write(char(i),' '); writeln;
 write('6 '); for i := 96  to 111 do write(char(i),' '); writeln;
 write('7 '); for i := 112 to 127 do write(char(i),' '); writeln;

 Writeln;
 write(char(15));                                                 { switch SI }
 Writeln('select G1 (SO)'); writeln;
 write(char(14));                                                 { switch S0 }

 writeln('  0 1 2 3 4 5 6 7 8 9 A B C D E F'); Writeln;
 write('2 '); for i := 32  to 47  do write(char(i),' '); writeln;
 write('3 '); for i := 48  to 63  do write(char(i),' '); writeln;
 write('4 '); for i := 64  to 79  do write(char(i),' '); writeln;
 write('5 '); for i := 80  to 95  do write(char(i),' '); writeln;
 write('6 '); for i := 96  to 111 do write(char(i),' '); writeln;
 write('7 '); for i := 112 to 127 do write(char(i),' '); writeln;

 write(char(15));                                                 { switch SI }
End.
