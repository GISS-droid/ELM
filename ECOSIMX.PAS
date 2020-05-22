Program Ecosystem_Emulator;
{Programador: Luis Everardo Castro Solis, 1992.
 Revisitado y modificado en ene-feb/2020 para prop¢sitos de
 investigacion en socioecologia computacional: las modificaciones
 fueron solamente en el sentido de aumentar la capacidad para observar el
 entorno computacional de datos de las simulaciones (mapeo)/ lecs}

uses crt, graph3;

const max_x = 50;
      max_y = 20;
    InputE  = 100;
    Luz     = 400;
 Maxciclos  = 30000;
 InitialE   = 500;
 Fr         = 1000;
 BondadC    = 0.5;
 AbundanciaC= 0.5;
 Rm         = 0.70;

type serT =  record
            Tipo:char;
            Cmt   :integer;
            Cmv   :integer;
            Crp   :integer;
            Edad  :integer;
            Status:real;
            end;
   medioT = array[1..max_x, 1..max_y] of serT;
   medioF = array[1..max_x, 1..max_y] of real;
   int4   = array[1..4] of integer;
   chr4   = array[1..4] of char;
   chrset = set of char;
   str20  = string[20];

var  medio
         :medioT; {elementos finitos}
   fisico
         :medioF;
   Tipos : chr4;
   Cmet,
   Cmov,
   Crep,
   EdadM
         : int4;
   a,ciclos:integer;
   estado:real;
   Sumas :int4;
   arch  :text;
   arch1 :text;
   AURA  :TEXT; {Added for SI purposes 31 ene 2020/lecs: map medioF medioT}
   POP   :TEXT; {AfSIp 31I2020/lecs: guardar datos populations}
   x1,y1 :integer;
   fase  : integer; {AfSIp 31I2020/lecs: extender ciclaje global}
   ciclox: longint; {AfSIp 31I2020/lecs: contador de ciclo extendido}


Procedure Census(medio:mediot; var sumas:int4; ciclos:integer);
var i,j,base:integer;
    total   :integer;
begin
gotoxy(60,22); write('Censando...');
for i:=1 to 4 do sumas[i]:=0;

for i:=1 to max_x do
 for j:=1 to max_y do
  begin
    case medio[i,j].tipo of
    'P': sumas[1]:=sumas[1]+1;
    'H': sumas[2]:=sumas[2]+1;
    'C': sumas[3]:=sumas[3]+1;
    'D': sumas[4]:=sumas[4]+1;
    end;
  end;

base:=10;
for i:=1 to 4 do if sumas[i] > 100 then base:=100;

 for i :=60 to 72 do
  for j:= 9 to 21 do
   begin
    gotoxy(i,j);
    write(' ');
   end;

 gotoxy(60,19); write('-------------');
 gotoxy(60,20); write('P H C D (',base,')');
 gotoxy(60,21); write('-------------');
 for i:=9 to 19 do
  begin
   gotoxy(59,i);
   write('-');
  end;

 for i:=1 to 4 do
  for j:=1 to (trunc(sumas[i]/base)) do
   begin
     case i of
     1:gotoxy(60,19-j);
     2:gotoxy(62,19-j);
     3:gotoxy(64,19-j);
     4:gotoxy(66,19-j);
     end; {case}
     write('Û');
    end;

gotoxy(60,22); write('           ');
total:=sumas[1] + sumas[2] + sumas[3] + sumas[4];
gotoxy(55,5); write('Poblaci¢n');
gotoxy(55,7); write('-          +');
for i:=55 to (55 + trunc(total/100)) do
 begin
  gotoxy(i,6);
  write('Û');
 end;
for i:= (56 + trunc(total/100)) to  66 do
 begin
  gotoxy(i,6);
  write('±');
 end;
end;


Procedure InitConstants(var tipos:chr4; var
Cmet,Cmov,Crep,EdadM:int4);
begin
tipos[1]:='P'; tipos[2]:='H'; tipos[3]:='C'; tipos[4]:='D';
Cmet[1]:=50;   Cmet[2]:=55;   Cmet[3]:=55;   Cmet[4]:=60;
Cmov[1]:=0;    Cmov[2]:=4;    Cmov[3]:=4;    Cmov[4]:=4;
Crep[1]:=2;    Crep[2]:=6;    Crep[3]:=5;    Crep[4]:=4;
EdadM[1]:=10;  EdadM[2]:=12;  EdadM[3]:=14;  EdadM[4]:=16;
end;



Procedure InitElementoFinito(var ser:serT);
begin
with ser do
 begin
 tipo:=' ';
 Cmt:=0;
 Cmv:=0;
 Crp:=0;
 Edad:=0;
 Status:=0;
 end;
end;


Procedure InitMedio(var medio:medioT);
var x,y:integer;

begin
 for x:=1 to max_x do
  for y:=1 to max_y do
   InitElementoFinito(medio[x,y]);
end;

Procedure InitSer(var ser:sert;No:integer);
begin
with ser do
 begin
 tipo:=tipos[No];
 Cmt:=Cmet[No];
 Cmv:=Cmov[No];
 Crp:=Crep[No];
 Edad:=0;
 Status:=InitialE;
 end;
end;

function Moverigt(x:integer):integer;
begin
if x < max_x then Moverigt:=x+1 else Moverigt:=max_x;
end;

function Moveleft(x:integer):integer;
begin
if X > 1 then Moveleft:=x-1 else Moveleft:=1;
end;

function Moveup(y:integer):integer;
begin
if y > 1 then MoveUp:=y-1 else MoveUp:=1;
end;

function Movedown(y:integer):integer;
begin
if y < max_y then MoveDown := y+1 else MoveDown:=max_y;
end;

Procedure idx(ser:sert; x,y:integer; fisico:real);
begin
if fisico <= BondadC then highvideo;
 with ser do
  begin
    gotoxy(x,y);
    if ((fisico = 0) and (tipo = ' ')) then write('ù') else write(tipo);
 end;
lowvideo;
end;

Procedure ConfigMedio(var medio:medioT);
var P,H,Cp,Cg:integer;
    x,y      :integer;
    key      :char;
begin
x:=1;
y:=1;
repeat
idx(medio[x,y],x,y,fisico[x,y]);
gotoxy(x,y);
 repeat
  key:=readkey;
  key:=upcase(key);
 until key in ['P','H','C','D','I','S','A','W','Z',' '];
case key of
'P':InitSer(medio[x,y],1);
'H':InitSer(medio[x,y],2);
'C':InitSer(medio[x,y],3);
'D':InitSer(medio[x,y],4);
'I':InitElementoFinito(medio[x,y]);
'S':x:=moverigt(x);
'A':x:=moveleft(x);
'W':y:=moveup(y);
'Z':y:=movedown(y);
end; {case}
until key = ' ';
end;

Procedure Seek_Coordinates(var medio:mediot; var x,y:integer;
                           ValidSet:Chrset; lim:integer;
cadena:str20);
var ni:integer;
begin
ni:=0;
repeat
  x:=random(max_X)+1;
  y:=random(max_y)+1;
  ni:=ni+1;
until ((medio[x,y].tipo in ValidSet) or (ni > lim));
if ni > lim then
             begin
             gotoxy(55,8);
             write(cadena);
             if cadena = 'No hay seres vivos'
              then
                  begin
                    census(medio,sumas,ciclos);
                    {afsip lecs31I2020 censo final antes de abortar}
                       close(aura);
                       close(pop);
                    {end addenum}
                    halt;
                  end;
             end
            else
             begin
             gotoxy(55,8);
             write('                    ');
             end;
end;

function index(ser:sert):integer;
begin
case ser.tipo of
' ':index:=0;
'P':index:=1;
'H':index:=2;
'C':index:=3;
'D':index:=4;
end;
end;




Procedure Set_Free(var medio:mediot;x,y:integer);
begin
InitElementoFinito(medio[x,y]);
idx(medio[x,y],x,y,fisico[x,y]);
end;

Procedure Procrea(var medio:mediot; x,y,indice:integer;
fisico:medioF);
var hx,hy:integer;
begin
hx:=x;
hy:=y;
if ((x>1) and (medio[x-1,y].tipo=' ') and (fisico[x-1,y]>=
BondadC))
 then hx:=x-1
 else
  if ((x<max_x) and (medio[x+1,y].tipo=' ') and
(fisico[x+1,y]>=BondadC))
  then hx:=x+1
  else
   if ((y>1) and (medio[x,y-1].tipo=' ') and
(fisico[x,y-1]>=BondadC))
   then hy:=y-1
   else
    if ((y<max_y) and (medio[x,y+1].tipo=' ') and
(fisico[x,y+1]>=BondadC))
    then hy:=y+1

               {modificacion below line on proof}
    else begin hx:=0; hy:=0;{ seek_coordinates(medio,hx,hy,[' '],500,'No hay nichos libres');}end;

if ((hx > 0) and (hy > 0))
then
 begin
  InitSer(medio[hx,hy],indice);
  idx(medio[hx,hy],hx,hy,fisico[hx,hy]);
 end;
end;

Procedure Aplication_of(var ser1,ser2:sert; mf:real);
begin
 ser1.status:=(ser1.status - ((ser1.cmt/100)*(ser1.status)) +
ser2.status)
              * mf;
ser2:=ser1;
initelementofinito(ser1);
end;

Procedure Aplication_itself(var ser1:sert; mf:real);
begin
ser1.status:=(ser1.status - ((ser1.cmt/100)*(ser1.status)))*mf;
end;

Procedure Positive_Aplication_itself(var ser1:sert; mf:real);
begin
ser1.status:=(ser1.status - ((ser1.cmt/100)*(ser1.status)) +
InputE + Luz) * mf;
end;

Function Intel_Move(var medio:mediot; x,y:integer; var fisico:mediof):integer;
var indiceA, move:integer;
begin
indiceA:=index(medio[x,y]);
if ((x+1 <= max_x) and (index(medio[x+1,y]) < indiceA )
     and (index(medio[x+1,y]) >0) and (fisico[x+1,y] >= BondadC) )
  then Intel_move:=3 {a la derecha}
  else
    if ((x-1 >= 1) and (index(medio[x-1,y]) < indiceA)
        and (index(medio[x-1,y]) > 0) and (fisico[x-1,y] >= BondadC))
      then Intel_move:=4  {a la izquierda}
      else
         if ((y-1 >= 1) and (index(medio[x,y-1]) < indiceA)
              and (index(medio[x,y-1]) > 0) and (fisico[x,y-1] >= BondadC))
           then Intel_move:=1 {arriba}
           else
              if ((y+1 <= max_y) and (index(medio[x,y+1]) < indiceA)
                  and (index(medio[x,y+1]) > 0)
                   and (fisico[x,y+1] >= BondadC))
                 then Intel_move:=2  {abajo}
                 else
                   begin
                    repeat
                         move:=random(medio[x,y].Cmv+1);
                    until move in [0..4];
                    intel_move:=move;
                   end;
end;

Procedure DinamizeMedio(var medio:mediot; var Estado:real);
var x,y   :integer;
    indice, indicep:integer;
    movdir:integer;
    px,py :integer;
    i     :integer;

begin
 seek_coordinates(medio,x,y,['P','H','C','D'],(5*max_x*max_y),
                   'No hay seres vivos');
 medio[x,y].edad:=medio[x,y].edad+1;
 estado:=estado+medio[x,y].status;

{gotoxy(60,24);write('Energ¡a:',Estado/ciclos:8:2);}
gotoxy(60,24);write('Energ¡a:',medio[x,y].status:8:2);

if ciclos mod 100{fr} = 0 then
begin
gotoxy(54,23); write('-');
for i:=55 to 75 do
 begin
  gotoxy(i,23);
  write('±');
 end;
 write('+');
{for i:= 55 to  (55 + trunc(estado/ciclos/10)) do}
for i:=55 to (55+trunc(medio[x,y].status/100)) do
 begin
  gotoxy(i,23);
  write('Û');
 end;
end;

 gotoxy(x,y);
 Indice:=index(medio[x,y]);
 {muerte}
 if medio[x,y].edad > edadM[indice] then fisico[x,y]:=fisico[x,y] +
                                                    (medio[x,y].status * Rm/InitialE);
 if fisico[x,y] > 1 then fisico[x,y]:=1; {FD overflow control}

 if ((medio[x,y].edad > edadM[indice]) or (medio[x,y].status <= 0.001))
     then Set_Free(medio,x,y)  {libera el nicho}
     else {interactua}
      begin
       if ((medio[x,y].edad mod medio[x,y].crp = 0)
          and (medio[x,y].status > AbundanciaC))
          then Procrea(medio,x,y,indice,fisico);
       if indice = 1
        then {es planta}
Positive_Aplication_itself(medio[x,y],fisico[x,y])
        else {es animal}
           begin
              {calcula inteligentemente coordenadas del ser
pasivo}
              movdir:=Intel_Move(medio,x,y,fisico);
              px:=X; py:=y;
              case movdir of
               1:py:=moveup(y);
               2:py:=movedown(y);
               3:px:=moverigt(x);
               4:px:=moveleft(x);
              end;{case}
             {determina que tipo de ser es el ser pasivo}
             indicep:=index(medio[px,py]);
             if movdir <> 0
             then
             case indice of
             2 : case indicep of
                 0,1:aplication_of(medio[x,y],medio[px,py],fisico[px,py]);
                 3,4:aplication_of(medio[px,py],medio[x,y],fisico[x,y]);
                   2:aplication_itself(medio[x,y],fisico[x,y]);
                 end;
             3 : case indicep of
                 0,2,3:aplication_of(medio[x,y],medio[px,py],fisico[px,py]);
                 4:aplication_of(medio[px,py],medio[x,y],fisico[x,y]);
                 1:aplication_itself(medio[x,y],fisico[x,y]);
                 end;
             4 : aplication_of(medio[x,y],medio[px,py],fisico[pX,py]);
             end
             else  aplication_itself(medio[x,y],fisico[x,y]);
             idx(medio[x,y],x,y,fisico[x,y]);
             idx(medio[px,py],px,py,fisico[px,py]);
           {if indice = 1 else}
         end;
      end;{interactua}
end;{dinamize}

Procedure Init_MF(var fisico:medioF);
var x,y:integer;
begin
randomize;
for x:=1 to max_x do
 for y:=1 to max_y do
  {repeat  fisico[x,y]:=random until ((fisico[x,y] >=0) and
                                                       (fisico[x,y] <=1));}
  fisico[x,y]:= {0.5} random;
end;

Procedure Retroalimenta_fisico(var fisico:mediof; medio:mediot);
var x,y:integer;
   acxy:real;
   suma1,suma2:integer;
      i:integer;

Begin
suma1:=0;
suma2:=0;
gotoxy(60,22); write('Retroalimentando...');

 for x:=1 to max_x do
  for y:=1 to max_y do
   {el medio se modificar  por la ley de promedio de promedios}
   begin
    acxy:=0;
    if x>1     then acxy:=acxy+medio[x-1,y].status;
    if x<max_x then acxy:=acxy+medio[x+1,y].status;
    if y>1     then acxy:=acxy+medio[x,y-1].status;
    if y<max_y then acxy:=acxy+medio[x,y+1].status;
    acxy:=acxy/400;
    if acxy >= abundanciaC
     then fisico[x,y]:=fisico[x,y]+acxy
     else fisico[x,y]:=fisico[x,y]-acxy;
    if fisico[x,y]<0 then fisico[x,y]:=0;
    if fisico[x,y]>1 then fisico[x,y]:=1;
    idx(medio[x,y],x,y,fisico[x,y]);
    if fisico[x,y] < bondadc then suma1:=suma1 + 1 else suma2:=suma2 +1;
   end;

gotoxy(60,22); write('                   ');
gotoxy(55,1); write('Medio F¡sico');
gotoxy(55,3); write('+          -');
for i:=55 to (55 + trunc(suma1/100)) do
 begin
  gotoxy(i,2);
  write('±');
 end;
for i:= (56 + trunc(suma1/100)) to  (56 + trunc(suma1/100) + trunc(suma2/100)) do
 begin
  gotoxy(i,2);
  write('Û');
 end;

end;

{PRocess Observation Module added by LECS on 31-I-2020
for investigation purposes map Aura and MedioT type/status}
Procedure mapAura(var Arch:text; var fisico:medioF; var medio:medioT; ciclos:integer);
var i,j:integer;
begin
writeln(Arch,'Ciclo:',ciclos);
for i:=1 to max_x do
Begin
  for j:=1 to max_y do
    Write(Arch, fisico[i,j]:10:3);
  write(Arch,' ':10);
  write(Arch,'*');
  {for j:=1 to max_y do
     write(Arch, medio[i,j].tipo);
  write(Arch,'*');}
  write(Arch,' ':10);
  for j:=1 to max_y do
     write(Arch, medio[i,j].status:10:3);
  writeln(Arch)
end
end; {mapAura}



begin {principal}

{++++ added by lecs on 31ene2020 for ip}
Assign(Aura, 'c:\tp\source\auras.txt');
Rewrite(Aura);
Assign(Pop, 'c:\tp\source\popul.txt');
Rewrite(Pop);
fase:= 1;

{end addition}


clrscr;
estado:=0;

randomize;
InitConstants(tipos,Cmet,Cmov,Crep,EdadM);
InitMedio(medio);
Init_Mf(fisico);
ConfigMedio(medio);
census(medio,sumas,ciclos);
for a:=1 to 30 do
 begin
  gotoxy(a,23);
  write('±');
 end;
     {NEXT ADD for si purpose lecs 31I2020}
         ciclox:=0;
         writeln(pop, cicloX:10,sumas[1]:10, sumas [2]:10, sumas[3]:10, sumas[4]:10);
         {retroalimenta_fisico(fisico,medio);******* ANOMALY INITIAL}
         mapAura(aura,fisico,medio,cicloX);
    for ciclos:=1 to maxciclos do
    begin
         ciclox:=ciclox + 1; {affsip on bla bla}

      gotoxy(1,22); write('Ciclos:[',ciclos,'] x',fase:3);
      gotoxy(1,24);
      write('|   |    1    |    2    |    3   X 10^3');
      if ciclos mod 1000 = 0
       then
         for a:=1 to (1 + trunc(ciclos/1000)) do
           begin
            gotoxy(a,23);
            write('Û');
           end;
      DinamizeMedio(medio,estado);
      if ciclos mod fr = 0 then
                             begin
                              census(medio,sumas,ciclos);
                              {afsip lec31I2020 censar}
                              writeln(pop, cicloX:10,sumas[1]:10, sumas [2]:10, sumas[3]:10, sumas[4]:10);

                              {++++ desactivado para evaluar efecto +++}
                              retroalimenta_fisico(fisico,medio);
                              mapAura(aura,fisico,medio,ciclos);
                             end;

     if ciclos = maxciclos
      then if fase < 5 then begin ciclos := 1; fase := fase + 1 end;
     end;
census(medio,sumas,ciclos);

{afsip lecs31I2020 censar}
writeln(pop, cicloX:10, sumas[1]:10, sumas [2]:10, sumas[3]:10, sumas[4]:10);

{assign(arch,'c:\geos\medio.dat');
assign(arch1,'c:\bp\bin\fisico.dat');
rewrite(arch);
rewrite(arch1);
for x1:=1 to max_X do for y1:=1 to max_y do
 begin
 writeln(arch,x1:4,y1:4,medio[x1,y1].status:8:2);
 writeln(arch1,x1:4,y1:4,fisico[x1,y1]:8:2);
 end;
close(arch);
close(arch1);}

    repeat until keypressed;

{afsip lecs31I2020}
close(aura);
close(pop);

end.