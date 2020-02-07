Program Ecosystem_Emulator;

{ **** Part 0. Reference ****}
{Programador: Luis Everardo Castro Sol¡s}
{Fecha      : Agosto de 1992}
{Versi¢n    : 2.0}

{Resumen    : Simulador del modelo log¡stico de un ecosistema, que
              opera dos sistemas paralelos de ecuaciones lineales
              cuyos coeficientes var¡an en el tiempo, sobre un continuo
              de probabilidades definido por operaciones de intercambio}

{ **** The Hypotalamus Zone. Do not Touch, please ****}
Uses crt;

Const Max_x = 50;
      Max_y = 20;
      Max_t = 10;
      Max_n = Maxint;
      Max_at= 2*Max_x*Max_y;
    EditSet = [chr(72),chr(80),chr(77),chr(75),chr(83),chr(79),chr(71)];
              {   ^   ,  v    ,  ->   ,  <-   , [supr], [fin]  [inicio]}

Type Entes   = Record
               Tipo  : byte;
               Bij   : real;
               Ed    : byte;
               end;

   Atributos = Record
               Symbol: char;
               Cmt   : real;
               Imv   : byte;
               Irp   : byte;
               Emax  : byte;
               end;

    MedioB   = Array [1..Max_x,1..Max_y] of Entes;
    MedioF   = Array [1..Max_x,1..Max_y] of real;
    Tipos    = Array [1..Max_t] of atributos;
    Coords   = array ['x'..'y'] of integer;
    Entorno  = Array [0..4] of Coords;
    St80     = String[80];
    ValidSet = Set of char;

Var B : MedioB;  {Matriz biol¢gica}
    F : MedioF;  {Matriz f¡sica}
    V : Entorno; {Vecindad de bij::=[(V[0,x],V[0,y]), ... ,(V[4,x],V(4,y])]}
 Tipo : Tipos;   {Definiciones de seres}
   nt : byte;    {n£mero de tipos actuantes}
 VldSt: ValidSet;{S¡mbolos de ser v lidos}
    P,           {coordenadas del nicho del ser activo (P[x],P[y])}
    A : Coords;  {coordenadas del nicho del ser pasivo (A[x],A[y])}
 Ac   : real;    {abundancia cr¡tica}
 Bc   : real;    {bondad cr¡tica}
 Fr   : integer; {frecuencia de retroalimentaci¢n}
 CRE  : real;    {Coef. de retorno energ‚tico al medio f¡sico}
 Luz  : real;    {Energ¡a luminosa por nicho}
 Nutr : real;    {Energ¡a micronutrientes ed ficos por nicho}
 Ei   : real;    {Energ¡a inicial de cada ser}
 n,              {numero de ciclos}
 m    : integer; {ciclo en curso}

{**** Part II. Configuration Facilities ****}
Procedure WriteXY(x,y:byte; cadena:st80);
begin
gotoxy(x,y);
write(cadena);
end;

Procedure Define_Tipos(var Tipo:tipos; var nt:byte; var VldSt:ValidSet);
var   i :byte;
begin
clrscr;
VldSt:=[];
for i:=1 to max_t do
 With tipo[i] do
   begin
     Symbol:=' ';
     Cmt:=0;
     Imv:=0;
     Irp:=0;
     Emax:=0;
   end;
WriteXY(5,1,'<< Definici¢n de tipos de entes interactuantes >>');
repeat
WriteXY(1,5,'N£mero de entes por definir : [  ]');
gotoxy(32,5);
read(nt);
Until nt <= max_t;
for i:=1 to nt do
 with tipo[i] do
  begin
   WriteXY(1,7,'Definici¢n del Ente tipo [  ]');
   gotoxy(27,7); write(i:2);
   WriteXY(1,9 ,'S¡mbolo                :');
   symbol:=readkey;
   write(symbol);
   vldst:= vldst + [symbol];
   WriteXY(1,10,'Coef. de metabolismo   :   ');
   repeat gotoxy(25,10); read(Cmt) until ((cmt >= 0) and (cmt <=1));
   WriteXY(1,11,'Indice de movilidad    :   ');
   repeat gotoxy(25,11); read(Imv) until Imv in [0..4];
   WriteXY(1,12,'Edad m xima            :');
   read(Emax);
   WriteXY(1,13,'Indice de reproducci¢n :   ');
   repeat gotoxy(25,13); read(Irp) until Irp in [0..Emax];
  end;
end;

Procedure define_parms(var Ac, Bc:real; var Fr:integer;
                       var CRE, Luz, Nutr, Ei: real; var n:integer);
begin
clrscr;
WriteXY(5,1,'<< Definici¢n de par metros de simulaci¢n >>');
writeXY(1,7,  'Abundancia energ‚tica cr¡tica              :');
readln(Ac);
repeat
 writeXY(1,8, 'Bondad cr¡tica                             :');
 readln(Bc);
until ((Bc >= 0) and (Bc <= 1));
repeat
 writeXY(1,9, 'N£mero de ciclos de simulaci¢n             :');
 readln(n);
until n <= max_n;
repeat
 writeXY(1,10,'Frecuencia de retroalimentaci¢n [B] -> [F] :');
 readln(Fr);
until Fr < n;
repeat
 writeXY(1,11,'Coef. de retorno energ‚tico al medio f¡sico:');
 readln(CRE);
until ((Cre >= 0) and (Cre <= 1));
writeXY(1,12, 'Energ¡a luminosa por nicho                 :');
readln(luz);
writeXY(1,13, 'Energ¡a de micronutrientes ed ficos        :');
readln(nutr);
writeXY(1,14, 'Energ¡a inicial de los seres nuevos        :');
readln(Ei);
end;

function Moveup(y:byte):byte;
begin
if y > 1 then moveup:=y-1 else moveup:=y;
end;

function Movedow(y:byte):byte;
begin
if y < Max_y then movedow:=y+1 else movedow:=y;
end;

function Movelef(x:byte):byte;
begin
if x > 1 then movelef:=x-1 else movelef:=x;
end;

function Moverig(x:byte):byte;
begin
if x < Max_x then moverig:=x+1 else moverig:=x;
end;

Procedure define_nicho(var B:medioB; A:coords; Simbolo:char; tipo:tipos);
var i:integer;
begin
i:=0;
repeat  i:=i+1 until tipo[i].Symbol = Simbolo ;
 with B[A['x'],A['y']] do
   begin
   Tipo:=i;
   Bij:=Ei;
   Ed:=0;
   end;
end;

Procedure Libera(var B:medioB; A:coords);
begin
with B[A['x'],A['y']] do
 begin
  tipo:=0;
  Bij:=0;
  Ed:=0;
 end;
end;

Procedure Idx(var B:medioB; A:coords);
begin
gotoxy(A['x'], A['y']);
if F[ A['x'],A['y']] < Bc then highvideo;
if B[ A['x'],A['y']].tipo = 0
 then write(' ')
 else write(tipo[ B[ A['x'],A['y']].tipo ].Symbol);
lowvideo;
end;

Procedure reidx(var B:medioB);
var A:coords;
begin
for A['x']:=1 to max_x do
 for A['y']:=1 to max_y do
    idx(B,A);
end;

Procedure Inicia_B(var B:medioB);
var A:coords;
begin
for A['x']:=1 to max_X do for A['y']:=1 to max_y do libera(B,A);
end;

Procedure define_B(var B:medioB; vldst:validset; tipo:tipos);
var A      :coords;
   command :char;
         i :integer;
    Arch   :text;

begin
clrscr;
writeXY(1,21,'<< Configuraci¢n del medio Biol¢gico >>');
writeXY(1,22,'<< Men£ :');
writeXY(1,23,'<< Tipos v lidos: ');
for i:=1 to nt do write(tipo[i].symbol:2);
writexy(1,24,'<< Edici¢n: [<-]  [->]  [^] [v] [Supr] [Fin]');
Inicia_B(B);
A['x']:=1;
A['y']:=1;
idx(B,A);
gotoxy(A['x'],A['y']);
repeat
 gotoxy(60,24); write('(',A['x'],',',a['y'],')');
 gotoxy(A['x'],A['y']);
 repeat command:=ReadKey until command in (Vldst + EditSet);
 Case command of
  chr(72):A['y']:=moveup(A['y']);
  chr(80):A['y']:=movedow(A['y']);
  chr(77):A['x']:=moverig(A['x']);
  chr(75):A['x']:=movelef(A['x']);
  chr(83):Libera(B,A);
  chr(79):begin {fin del procedimiento} end;
  else define_nicho(B,A,command,tipo);
  end; {case}
 Idx(B,A);
 gotoxy(A['x'],A['y']);
until command = chr(79);
end;

Procedure idxF(var F:medioF; P:coords);
var Imf : real;
    Ch  : char;
begin
Imf:=F[P['x'],P['y']];
gotoxy(P['x'], P['y']);
If Imf = 0 then ch:=' '
 else if ((Imf > 0) and (Imf < Bc)) then ch:=chr(176) {°}
  else if Imf = Bc then ch:= chr(177) {±}
   else if ((Imf > Bc) and (Imf < 1)) then ch:=chr(178) {²}
    else if Imf = 1 then ch:= chr(219); {Û}
write(ch);
end;

Procedure define_F(var F:medioF; Bc:real);
var P,P1,P2:coords;
   command    :char;
       i,j    :integer;
    VldstF    :set of char;

Procedure Fill(f1,f2:real);
var Pf:coords;
begin
    for Pf['x']:=P1['x'] to P2['x'] do
    for Pf['y']:=P1['y'] to P2['y'] do
    begin
    if f1 = f2
      then F[Pf['x'],Pf['y']]:=f1
      else
        repeat
          F[Pf['x'],Pf['y']]:=random
        until ((F[Pf['x'],Pf['y']] > f1) and (F[Pf['x'],Pf['y']] < f2));
   idxF(F,Pf);
   end;
end;

begin
clrscr;
VldStF:= ['1','2','3','4','5','Q'];
writeXY(1,21,'<< Configuraci¢n del medio F¡sico >>');
writeXY(1,22,'<< Men£ :');
writeXY(1,23,'<< Tipos v lidos: 1:[1 Û] 2:[Bc ±] 3:[<Bc °] 4:[>Bc ²] [Q]uit');
writexy(1,24,'<< Edici¢n      : [<-]  [->]  [^] [v] [Supr] [Inicio] [Fin]');
for P['x']:=1 to max_X do for P['y']:=1 to max_y do F[P['x'],P['y']]:=0;
P['x']:=1;
P['y']:=1;
idxF(F,P);
gotoxy(P['x'],P['y']);
repeat
 gotoxy(60,24); write('(',P['x'],',',P['y'],')');
 gotoxy(P['x'],P['y']);
 repeat
   command:=ReadKey;
   command:=UpCase(command)
 until command in (VldstF + EditSet);
 Case command of
  chr(72):P['y']:=moveup(P['y']);
  chr(80):P['y']:=movedow(P['y']);
  chr(77):P['x']:=moverig(P['x']);
  chr(75):P['x']:=movelef(P['x']);
  chr(71):Begin
            P1['x']:=P['x'];
            P1['y']:=P['y'];
          End;
  chr(79):Begin
            P2['x']:=P['x'];
            P2['y']:=P['y'];
          End;
  chr(83):fill(0,0);
   '1'   :fill(1,1);
   '2'   :fill(Bc,Bc);
   '3'   :fill(0,Bc);
   '4'   :fill(Bc,1);
  end; {case}
 idxF(F,P);
 gotoxy(P['x'],P['y']);
until command = 'Q';
end;

{**** Part III. An Easy way to go to hell ****}
Procedure Select(var B:medioB; var A:coords);
var atempt:integer;
begin
atempt:=1;
 repeat
   repeat A['x']:=random(max_X+1) until A['x'] > 0;
   repeat A['y']:=random(max_Y+1) until A['y'] > 0;
   atempt:=atempt + 1;
 until ((B[A['x'],A['y']].tipo > 0) or (atempt = max_At));
if atempt = max_At then halt;
end;

Procedure RetornaEnergia(var F:medioF; var B:medioB; A:coords; cre:real);
begin
F[A['x'],A['y']]:=F[A['x'],A['y']] + CRE*B[A['x'],A['y']].Bij;
if F[A['x'],A['y']] > 1 then F[A['x'],A['y']] := 1;
Libera(B,A);
end;

Procedure Compute_Entorno(var V:entorno; A:coords);
{*** condici¢n de susceptibilidad de an lisis implicita}
begin
V[0,'x']:=A['x'];
V[0,'y']:=A['y'];

V[1,'x']:= {A['x']+1;} moverig(A['x']);
V[1,'y']:=A['y'];

V[2,'x']:=A['x'];
V[2,'y']:= {A['y']+1;} movedow(A['y']);

V[3,'x']:= {A['x']-1;} movelef(A['x']);
V[3,'y']:=A['y'];

V[4,'x']:=A['x'];
V[4,'y']:= {A['y']-1;} moveup(A['y']);
end;

Procedure Retro_B_F(var B:medioB; var F:medioF);
var A    :coords;
    AcVa :real;
    pos  :integer;
    Vf   :entorno;
    S    :integer;
begin
for A['x']:=1 to max_x do
 for A['y']:=1 to max_y do
  begin
   compute_entorno(Vf,A);
   AcVa:=0;
   for pos:=1 to 4 do AcVa:=AcVa + B[Vf[pos,'x'],Vf[pos,'y']].Bij;
   AcVa:=AcVa/400;
   if AcVa >= Ac then S:=1 else S:=-1;
   {S:=1;}
   F[A['x'],A['y']]:=F[A['x'],A['y']] + S*AcVa;
   if F[A['x'],A['y']] > 1 then F[A['x'],A['y']] :=1.0;
   if F[A['x'],A['y']] < 0 then F[A['x'],A['y']] :=0.0;
   idxF(F,A);
  end;
Reidx(B);
end;

Procedure Creced_y_multiplicaos(var B:medioB; var F:medioF; A:coords;
                                tipo:tipos; V:entorno );
var pos  :integer;
    MaxF :boolean;
    H    :coords;

begin
if {*** Condici¢n de edad ***}
   ((B[A['x'],A['y']].ed mod Tipo[ B[ A['x'],A['y']].tipo].Irp = 0)
    and  {*** Condici¢n de energ¡a ***}
       (B[A['x'],A['y']].Bij >= Ac))
   then
     begin
     {*** condicion de espacio ***}
     pos:=1;
     MaxF:=false;
       repeat
       if {*** condici¢n de espacio vital ***}
         ((B[ V[pos,'x'], V[pos,'y'] ].Bij = 0)
         and {*** condici¢n de bondad ***}
         (F[ V[pos,'x'], V[pos,'y'] ] >= Bc))
           then MaxF:=true
           else pos:=pos+1;
       until ((MaxF) or (pos > 4));
     if MaxF
       then
          begin
           H['x']:=V[pos,'x'];
           H['y']:=V[pos,'y'];
           define_nicho(B,H,Tipo[B [A['x'],A['y']].tipo].symbol,tipo);
           idx(B,H);
          end;
     end;
end;

Procedure Apr(var B:medioB; var F:medioF; A:coords; tipo:tipos);
begin
    B[A['x'],A['y']].Bij:= (B[A['x'],A['y']].Bij
    -Tipo[ B[ A['x'],A['y']].tipo].Cmt * B[A['x'],A['y']].Bij
    + Luz + Nutr)*F[A['x'],A['y']]
end;

Procedure Ap(var B:medioB; var F:medioF; A,P:coords; tipo:tipos);
begin
     B[A['x'],A['y']].Bij:=(B[A['x'],A['y']].Bij
     -Tipo[ B[A['x'],A['y']].tipo].Cmt * B[A['x'],A['y']].Bij
     +B[P['x'],P['y']].Bij)*F[P['x'],P['y']];
     B[P['x'],P['y']]:=B[A['x'],A['y']];
     Libera(B,A);
     idx(B,A);
     idx(B,P);
end;

Procedure Acn(var B:medioB; var F:medioF; A,P:coords; tipo:tipos);
begin
    B[A['x'],A['y']].Bij:=(B[A['x'],A['y']].Bij
    -Tipo[ B[A['x'],A['y']].tipo].Cmt * B[A['x'],A['y']].Bij)
    *F[A['x'],A['y']];
end;

Procedure SelectAplication(var B:medioB; var F:medioF; A,P:coords;
                           tipo:tipos);
begin
If B[P['x'],P['y']].tipo = 0
 then Ap(B,F,A,p,tipo)
 else if Tipo[B[A['x'],A['y']].tipo].Imv = 4
     then Ap(B,F,A,p,tipo)
    else if Tipo[B[A['x'],A['y']].tipo].Imv > Tipo[B[P['x'],P['y']].tipo].Imv
             then Ap(B,F,A,p,tipo)
             else if Tipo[B[P['x'],P['y']].tipo].Imv
                     > Tipo[B[A['x'],A['y']].tipo].Imv
                   then Ap(B,F,P,a,tipo)
                   else if Tipo[B[A['x'],A['y']].tipo].Imv
                           = Tipo[B[P['x'],P['y']].tipo].Imv
                           then Acn(B,F,A,P,tipo);

end;

Procedure Simon_say_get_energy(var B:medioB; var F:medioF;  A:coords;
                               tipo:tipos; V:entorno);
Var pos :integer;
    MaxG:boolean;
begin
 If tipo[ B[A['x'],A['y']].tipo].Imv = 0
  then Apr(B,F,A,tipo)
  else
    begin
     pos:=1;
     MaxG:=false;
       repeat
       if {*** condici¢n de existencia energ‚tica ***}
        ((B[ V[pos,'x'], V[pos,'y'] ].Bij > 0)
         and {*** condici¢n de disponibilidad energ‚tica ***}
         (B[ V[pos,'x'], V[pos,'y'] ].tipo < B[ A['x'],A['y']].tipo)
         and {*** condici¢n de bondad ***}
         (F[ V[pos,'x'], V[pos,'y'] ] >= Bc))
           then MaxG:=true
           else pos:=pos+1;
       until ((MaxG) or (pos > 4 {Tipo[B[A['x'],A['y']].tipo].Imv} ));
     If not(MaxG) then Pos:=random(5);
     SelectAplication(B,F,A,V[pos],tipo);
    end;
end;

{**** The Final Cut. An exercise on auto-indulgence ****}
begin
randomize;
define_tipos(tipo,nt,vldst);
define_parms(Ac, Bc, Fr, CRE, Luz, Nutr, Ei, n);
define_B(B,vldst,tipo);
define_F(F,Bc);
clrscr;
reidx(B);
for m:=1 to n do
 begin
 writeXY(1,24,'Ciclo:');
 write(m:8);
 if (m mod Fr) = 0  Then Retro_B_F(B,F);
   Select(B,A);
   writeXY(1,40,'Energ¡a:');
   write('(',A['x']:4,',',A['y']:4,'):',B[A['x'],A['y']].Bij:8:2);
   B[A['x'],A['y']].Ed:=B[A['x'],A['y']].Ed+1;
   If B[A['x'],A['y']].Bij <= 0.001
   then Libera(B,A)
   else if B[A['x'],A['y']].Ed >= Tipo[B[A['x'],A['y']].tipo].Emax
         then RetornaEnergia(F,B,A,Cre)
         else
           begin
           {*** the Party begins: There is problems in the forest ***}
           Compute_Entorno(V,A);
           Creced_y_multiplicaos(B,F,A,tipo,V);
           Simon_Say_get_energy(B,F,A,tipo,V);
           end;
   idx(B,A);
 end;{for}
end.
{**** And so...}