unit u6502b_ver05alpha_ObjectOriented;// emulator for 6502 uP written by Stefan Janda (c) 2010
             // free for personal or public educational, non-commercial use
interface    // any commercial use (direct or indirect) needs special permission
const        // please contact: steve082010jay [at] arcor.de   remove brackets
             // version 0.5 alpha use on your own risk - can contain errors
             // version from April 2010, newer more stable version is available
H:array[false..true]of WORD=(0,256);
W:array[false..true]of byte=(0,1);
F:array[false..true]of byte=(1,0);
bX:array[0..7]of byte=(7,6,0,1,0,2,6,3);
k1:integer=$20000;                                 {total memory size 128 KB}
iyi:set of byte=[$d9,$db,$f9,$fb,$19,$1b,$39,$3b,
$59,$5b,$79,$7b,$96,$97,$99,$9b,$B6,$B7,$B9,$BB];
iy:set of byte=[$96,$97,$9E,$9F,$b6,$b7,$BE,$BF];  {index Y replaces index X}
ix:set of byte=[];
iy2:set of byte=[$11,$31,$51,$71,$91,$b1,$d1
                ,$f1,$13,$33,$53,$73,$93,$b3,$d3,$f3];

type
tFlr=Set of 0..7;  {'set of' enables bitwise accessing Opcodes and Status reg.}
{next , we define an array to hold the addressable RAM and ROM, its twice ..}
{the size a standard 6502 can handle, but there are some operating systems'
restrictions too, see documentation}
t7=packed array[0..$20000-1]of byte;

{the processor core emulation}
T6502b=class
yc,oC,Ad1,Ad2:integer;       {yc means cycle time consumed by emulation}
IQ,RS,NM,JM,SPO,n1:byte;      {SPO = semaphore}
p,q,hv:^byte;
rmw:WORD;
{setting alwd the proper way you can influence memory handling}
alwd:set of byte;
U:function(f1:integer; VAR f2:integer):byte;
mr: packed record case byte of
0:(AR1: array[0..$20000-1+18]of byte);
 {the order of registers is chosen deliberately with respect to their
 implicit encoding, eg. YR is addressed only when lowest two bits of Opcode =0 }
1: (mM:t7;YR,AC,XR,Mx,SP,SPH,PL,PH,fSR,O1,T_P,ADL,ADH,O2,O3:byte;due,v:integer);
{an alternative set of register names is defined; quad1 and Tmp_ only fill gaps}
2: (me2:t7;quad1:integer;yyH,PC:word;mSTa,op0,Tmp_:TFlr;AxL,AD:WORD);end;
  {meaning of some abbreviations:}
  {YR,AC,XR: Y-Register,accumulator, X-Register}
  {fSR: status register}
  {PC: 6502 program counter}
  {PL,PH: program counter low /high half}
  {mSTa: the same thing as fSR, but an implicit typecast to "set of" }
  {Mx: 'mixed' register which takes the result if accumulator and XR are
  switched on simultaneously}
  {op0: another version of the instruction byte, implicit typecast to "set of"}
constructor create;
function   myFlg(lC:byte):byte;
procedure  SYS(aes, E:WORD);
function  rT:Byte;
procedure wT(TP:byte);
property  Stack:BYTE read rT write wT;
End;

implementation
uses classes,forms;

constructor T6502b.create;      {called when you assign the processor object}
var n1:byte;
begin
 inherited create;    {clear all variable fields to zero; delete function ptr U}
 mr.v:=1;             {preset an interval flag for ProcessMessages 1=each cycle}
 mr.due:=MaxInt;      {Due time is preset to be far in the future}
 RS:=1;               {on power on, the simulated Reset line is passive}
 IQ:=1;               {Reset , if needed , must be set expressly}
 NM:=1;               {before you call T6502b.SYS(address, autoIRQ) later }
    {initialize stack pointer, this is not original 6502's behaviour but useful}
 mr.yyh:=$1FF; {can be removed for compatibilities sake, but see SP check below}
 for n1:=0to 255do                               {pre-compute some values...}
                 if(n1 and$14=20)or(n1 and$1D=1) {compute a set for XR-indexed}
                 then Include(ix,n1);ix:=ix-iy;   {and exclude special cases}
end;

{The rT / wT routines represent some special feature of Delphi / Object pascal}
{called 'property' which C/ C++ lack. rT and wT are tied to the 'stack' field}
{Each time 'stack' gets accessed, as a side effect, those methods get called}
procedure T6502b.wT(TP:byte);
begin  {WriTe sTack}
 mr.mM[mr.yyh]:=TP;  {store value to memory array}
 Dec(mr.SP);         {post-decrement stack pointer register}
 Inc(yc);            {adjust cycle time [yc] for stack access}
end;{yyh is just another flavor of stack pointer with fixed high byte =$01}

{Set machine status register flags N for negative and Z for zero results.}
{thanks to Sean Eron Anderson's Bit Twiddling Hacks, from
         http://graphics.stanford.edu/~seander/bithacks.html
 which were of tremendous help to do this in an elegant manner.}
function T6502b.myflg(lC:byte):byte; {set/clear N,Z flags in 6502's SR register}
begin  {'Merge bits from two values according to a mask'}
  mr.fSR:=mr.fSR xor((mr.fSR xor((lc and 128)+2*ord(lc=0)))and$82);
end; {fSR is just the binary bitwise flavor of mSta which is a 'set of byte'}

function  T6502b.rT:byte;
begin   {Read sTack procedure}
  Inc(mr.SP);  {pre-increment stack pointer}
  rT:= mr.mM[mr.YYH];
  Inc(yc);   {adjust cycle time [yc] by 1}
end;
{Main function}
procedure T6502b.SYS(aes,E:WORD);{first parameter: Address, second: AutoIRQClr}
var t2,y1,y2,y3,byg,iPz,rq0,mMAllg,ZerO,indirk,
A,MW,Arit,DCIC,SBTu,LK,LiN,ReC,CoL10,CoL18,CoL1a,Tr1,pCry,poV,Tri,uEven,RegZg,n2
,AiQ,DCI,Sp8A,m:byte;brCnt,brID,SpNr,brCon:Word;r0:smallInt;bT1,bT2,bT3:boolean;
begin
 WITH MR DO BEGIN                  { with MR: always work on Machine Registers}
 if SPO>0 then exit;Inc(SPO);PC:=AeS;  {Since unit can't allow reentrance,   }
   Repeat                              { we maintain a semaphore against that}
   O1:=mM[PC];               { Fetch OpCode, this has to be done always }
   Inc(JM,W[O1 and 31=18]);  { Detect 'Jam' opcodes like $12, $32, $52, ...}
   O2:=mM[PC+1];             { always fetch 1st operand like real 6502 does }
   O3:=mM[PC+2];             { 2nd operand simply gets ignored if not needed }

{Simulate a hardware wired-AND made of several low-active interrupt signals}
{RS = RESET, NM = non-maskable NMI, IQ = Interrupt request IRQ}
{AiQ equals to 'All interrupt reQuests'; IRQ additionally gets masked by its }
  AiQ:=(1-((1-IQ)*F[2 in mSta]))*Rs*Nm;  {permission flag in machine status }
                                         {AiQ is like a 'latch' for IRQ state}
  if AiQ<>1then O1:=0;{Override OpCode with ZERO== BRK if there's any interrupt}
  
  Tri:=W[(O1 and 31=25)or(O1 and $F>=$C)or(O1 and $1f>=27)]; {3 OpCode Bytes?}
  iPz:=W[O1 and$D=8];                       {detects implicit addressing mode}
   CoL18:=W[O1 and$1F=24];   {detects all 'implicit' opcodes located in row $18}
   CoL1a:=W[O1 and$1f=26];    {the same but for instructions located in row $1A}
   bT1:=O1 and 2=2;
   bT2:=O1 and 4=4;
   bT3:=O1 and 8=8;
   SpNr:=O1 shr 5*2;{isolate bits 7..5 and move into bit 3..1;note: bit 0 clear}
   ZerO:= W[O1 and$3=0];   
   CoL10:=W[O1 and$1F=16];  { detects all 'branch' opcodes located in row $10}
   IF SpNr=8              {now let's check for STA,.. and so forth instructions}
    then O1:=O1 xor(16*iPz*ZerO){if exist, make DEY and TYA change their places}
      ELSE If CoL18>0     {make second use of (SpNr=8) check}
        then              {because in general, in CoL18 reside 'Set and Clear'}
        BEGIN                {bit instructions except $98 which is in (SpNr=8)}
         m:=O1 and$20shr 5;  {determine if bit flag has to be set or cleared}
         n2:=bX[O1 shr 6+4]; {determine which bit - C,V,I,D - has to be changed}
           if Odd(m)then include(mSta,n2)else Exclude(mSta,n2); {perform change}
        END;                 {Caveat: CLV is wrong!}
                        {t2 contains lowest 5 bits of instruction code }
   t2:=O1 and$1F;       {which helps to check for all 13 addressing modes}
   Tr1:=Tri+W[O1=$20];  {adds JSR to three-byte instruction detection}
   byg:=2-iPz+Tri;      {delivers number of overall bytes the instruction needs}
   LiN:=W[O1<$80];              {all instructions from left half of table }
   ReC:=1-LiN;                  {... and the remaining from the right half}
   uEven:=W[Odd(O1)];    {'uneven' Opcodes; identifier 'odd' is already in use }
   mMAllg:=W[bT2 or(t2>=$18)];      {indicates general memory access}
   indirk:=F[bT2]*F[bT3]*W[Odd(O1)];{is 1 if indirect addressing except $6C JMP}
   RegZg:=LiN+ReC*((O1 and 3)+2*W[SpNr=$E]*ZerO);  {array index RegZg points to}
   Ad1:=RegZg+k1;       {Compute the register operand [simulated register file]}
   q:=@AR1[Ad1];                         {make register operand a real pointer}
   r0:=(O2+W[O1 in ix]*XR+W[O1 in iyi]*YR);   {compute Address I.: non-indirect}
   AD:=((r0 and(Tr1*$FF00+$FF))+H[Tr1=1]*O3); {zeropage treatment, r0 is 16 bit}
   oc:=Hi(r0);           {preserve carry for timing calculation if page changed}
   r0:=mM[AD]+W[O1 in iy2]*YR; {compute Address II.: YR indexed addresing modes}
   AD:=(1-indirk)*AD+indirk*(256*mM[AD+1]+r0);  {fetch indirect address if need}
   inc(oc,Hi(r0));    {preserve carry;note: oC is assumed not to be already set}
   Arit:=W[O1>$BF];       {Arit==Arithmetics; all codes above $C0}
   A:=LiN*W[bT1];                                {ASL and related shift instr.}
   SBTu:=W[SpNr=$E]*uEven;                       {True Subtract SBC instr.}
   LK:=LiN*uEven;      {LK == logic instructions AND,OR,EOR; note: includes ADC}

{ MW denotes a subset of read-Modify-Write instructions i.e. INC and DEC. }
{ the "in" statement enhances this by some implicit opcodes for INX, DEY etc.}
{ also the 'fake DEY' code $98 is included.   }
   MW:=W[((arit>0)and bT1 And bT2)or(O1 in[$98,$CA,$C8,$E8])];

 { DCIC means "DEC-INC-only" , thus, combined ISB / DCP codes cannot influence}
 {                                 the  flag register in an undesired way.}
   DCIC:=MW-uEven*MW;   


   Sp8A :=ReC*(1-Arit);            {LDA,LDX,LDY, STA,STX,STY etc. instructions}

   // The following lines determine which part of ALU will be switched on later

   DCI:=W[SpNr=$C]*ZerO*iPz;  {changes -1 with +1 where needed, INY instead DEY}
   y1:=W[SpNr=0]+W[SpNr=4];                 {condition for ALU: logic OR [+EOR]}
   y2:=W[SpNr=2]+ W[SpNr=4];                {condition for ALU: logic AND[+EOR]}
   y3:=W[SpNr=6];                 {turns Adder on; note: subtraction goes apart}
    m :=   mMAllg+Indirk;                {indirect case added to memory access}
    n2:=iPz+CoL10;       {if implicit or branch, we dont need to access memory}

  //compute the memory operand. Gets replaced by AD1 in a number of cases.
 Ad2:=
 AD*(1-iPz)*m+(k1+$D)*(1-n2)*(1-m)+n2*(Ad1-W[Sp8A=1]*(RegZg-W[t2<$F]-CoL1A*4));
   rmw:=(1024*A+512*W[SpNr=8]+256*MW)*(1-ipz); {Read-modify-Write flag}

// leave checking the allowed ('alwd') memory operand to an external subroutine.
// If the externally provided array ALWD determines that an address requires
// special activity, e.g. output to screen (or if address is between 0 and 1
// which means we speak to hardware processor port directly), memory access is
// detected (m>0) while a function pointer for 'U' has been provided,
// then branch there and have the work done outside. 'U' may modify the
// given memory pointer because calling convention is 'by reference'. ('VAR')
// it is possible to use this program without such a subroutine, too.

   if((Hi(AD)in Alwd)or(AD<2))and(m>0)and(@U<>nil)
      then U(rmw+0,ad2);{+0 helps U to determine from where it has been called}
   p:=@ar1[Ad2];        {finally , make a real pointer out of memory operand.}

Inc(yc,                         {do the timing calculations all in one go}
    2+(1-ipz)*((A+MW)*2+m*ord(t2<$10)*(1+Tr1+indirk*3)
                +m*W[t2>$0f]*(2+indirk+(indirk+W[t2>$18])*(oC or W[rmw>0]))));

// a few opcodes have been left for individual tests ...                
   CASE O1
      of                                  {BIT instructions $24, $2C}
   $24,$2c:fSR:=fSR and $3D or(p^and $C0)or 2*W[p^and AC=0];{first clear N,V,Z}
   $68:begin AC:=Stack;myFlg(AC);Inc(yc);end;  {PLA and adjust timing value +1}
   $48:Stack:=AC;                                            {push Accu PHA}
   $08:Stack:=fSR or $30;                                    {push Status PHP}
   $28:begin fSR:=STack;Inc(yc);end;           {PLP and adjust timing, 4 cycles}
   END;
   
  {preamble for ASL,LSR,ROL,ROR}
  {first, save old value for CARRY flag into oC [OldCarry];
   n2 ==shift direction left/right
   because combined unofficial instructions could influence carry several times}
   
   oC :=fSR And 1; n2:=W[6in Op0];    // oC saves old carry value before ALU can
                                      // compute a new one and overwrite old
{ALU stuff }
{ the following lines cover 48 instructions regarding bit shifting }
 if A=1then   {ASL and shifts, e.g. ASL,LSR,ROL,ROR}
  begin
    m:=(1-n2)*Hi(p^*2)    // n2 = 0: isolate carry from implicit high byte of..
         +n2*W[Odd(p^)];  // ..result; if n2 =1 take carry from low byte (LSR)
   p^:=(1-n2)*(Lo(p^*2)  {the W[5 in Op0] expression determines whether old ..}
      +oC*W[5in op0])    {..carry gets rotated in again. Op0 is a flavor of O1.}
      +(p^DIV 2+$80*oC*W[5in op0])*n2;     {the 'shift right' case is done here}
      myflg(p^);    // determine N and Z processor flags out of memory operand
  end;              // m saves the carry bit shifted out which is needed later.

// the following lines cover >12 instructions regarding increment and decrement.
   p^:=(1-MW)*p^     {keep old value if no INC / DEC takes place}
         +MW*        {if INC / DEC ..}
         (p^+(2*((O1 and$20)DIV$20xor DCI)-1));  // .. add or subtract 1.
                                                 // please note execution order.
// Following lines cover >30 instructions which add, subtract or compare.
   r0:=p^*(1-(y3+Arit)) {keep old value of p^ if neither ADC nor SBC take place}
       +y3*             {if ADC ..}
       (q^+p^+oC)
       +Arit*                 {if SBC or CMP or CPX and so on ..}
       (q^-p^-SBTu*(1-oC));   {invert the old carry (two's complement) first}
                              {use carry if a real subtraction took place.}
                              {intermediate result in r0 is 16 bit wide,}
   rq0:=Lo(r0);               { make 8 bit of it. }

   pCry:=Hi(r0)and 1;         {isolate carry from ADC / SBC / CMP.. result}
   poV:=1-W[(rq0 and $80)=(q^and $80)];   {do the same with overflow flag.}
                              // this could be done better - see new version
 // the following represents the very core of the ALU:
 // combinatory logic functions AND, OR, EOR
 // these 2 lines cover 24 instructions:
   rq0:=(y3+Arit)*rq0+(LK*F[SpNr=6])*((y1*(q^or rq0)
        or y2*(q^and rq0))and(NOT(y2*y1*(q^and rq0))));

   IF(LK+SBTu)>=1THEN q^:=rq0;     {store result of true SBC or ADC back into }
                                   {processor's register.}

{Condition for Carry, combines everything into C which could have influenced it}
   n2:=LK*W[SpNr=6]+(Arit*(1-iPz)*(1-CoL10)*(1-DCIC))+A;
   Dec(fSR,fSR and 1*n2);                   {make machine status clean first}
   Inc(fSR,A*m+W[(n2-A)*(pCry xor ReC)>0]); {set carry, complement if needed}

{Set or clear overflow flag only in ADC / SBC cases}
   if  (LK*W[SpNr=6]>0)          {ADC }
     or(0<(Arit*SBTu))           {or true SBC}
     then
       begin
       if poV=1then
              Include(mSta,6)
         else Exclude(mSta,6);
         end;
{the following covers approx. 62 data transportation instructions like LDA, STA}
   IF (Sp8A=1)and(CoL10=0)
    THEN
      BEGIN
       if SpNr=8 then                  // if opcode in column 8 then simply
          begin hv:=p;p:=q;q:=hv;end;  // swap source w/ destination
      q^:=p^;                        // do the real data transfer  here         
      if(O1<$90*ipz)or(O1*F[O1=$B8]>$9F)
        then  myFlg(p^);             // set N , Z flags according to data
      END;

{set or reset Negative and Zero flags according to result in rq0 except in implicit addressing cases}
   if LK+Arit*(1-iPz)*(1-CoL10)*(1-DCIC)>0   {N, Z flag out of ADC, SBC, CMP }
     then myFlg(rq0);
                                       {DEC / INC flag treatment}
   if DCIC>0then myFlg(p^);           {set N , Z flags according to memory}

   {a second call outside in order to manage write access to external memories}
   IF((Hi(AD)in Alwd)or(AD<2))and(rmw>0)and(@U<>nil)
      then  U(rmw+1,Ad2);         {the +1 serves to distinguish this call from }
                                  {first call above. this one handles WRITEs.}

 {now ALU is finished and we must check for jumps and branches }
  if(t2=16)and(bX[O1 shr 6] in mSta=(5 in op0)){check branch opcode & condition}
      then
      begin
      AxL:=2+PC+ShortInt(O2);         {AxL holds 16 bit temporary result}
      Inc(yC,1+F[ADH=PH]);            {adjust timing value in case page changed}
      PC:=AxL;                        {set Program counter to new value}
        if brID<>pc then
           begin brId:=PC;brCnt:=1;end{some magic in order to detect idle loops}
           else
           begin
             if O2 div$80>0then inc(brCnt);  {check whether the jump goes back}
             if brCnt>999then brCon:=1;  {1, if we did the same loop too often}
           end
      end
      ELSE                                  {no branch, normal PC increment}
      begin Inc(PC,byg*AiQ);end;            {but don't increment if interrupted}

{JSR, BRK, INT: Save Program counter to stack and Status when needed, too}
   if AiQ*(O1 and $df)=0then                {Hardware/software interrupt or JSR}
      begin Stack:=Hi(PC);Stack:=Lo(PC);Inc(yc,2);{push PC->stack; adjust time}
      if AiQ*O1=0then                      {Hardware/software interrupt, No JSR}
  begin Stack:=fSR and$EF or aIQ*16;Include(mSta,2);end;{push STATUS too, set I}
      end;                                             {set break flag on stack}

{JMP, JSR: Change program counter when standard JMP (non-indirect)or JSR occur}
   if O1 in[76,32]then PC:=O2+256*O3;DEC(yc,W[O1=76]);{JMP/JSR; JMP:less cycles}
   
{The following lines cover tasks related to Interrupts, BRK, RESET and NMI}
   if O1=0then
   begin
   IQ:=IQ or E;{this is my auto interrupt clear feature, not in real 6502, S.J.}
   PC:=mM[$FFF8+2*RS+4*NM]+256*mM[$FFF8+2*RS+4*NM+1]; {compute vector directly}
   NM:=1;                           {simulate edge-sensitive NMI signalling}
   RS:=1;                           {RESET cleared for convenience only}
   end;                             {in order to avoid jamming}

   if O1=$6C then                {fetch indirect address for PC if JUMP $6C}
      begin PC:=mM[AD]+256*mM[AD+1];Inc(yc);end;  {and adjust timing value 4->5}
      
{Return from subroutine RTS, Return from Interrupt RTI}
   if(AiQ*O1 in[64,96])then     {RTS takes 1 cycle more than formula above says}
      begin Inc(yc,W[O1=96]+1); {so we need to adjust}
         IF O1=64 then fSR:=Stack;
         PC:=Stack+256*Stack+W[O1=96];
      end;
{branching outside in order to keep messages running}
   if pc mod V=0 then Application.ProcessMessages;
 UNTIL(JM>0)or(Sp<2); {this line should be altered if no stack checking wanted}
 Dec(SPO);            {emulation ends, if JAM-Flag 'JM' is set, either        }
  END;  // WITH       {by an external thread or internally by a JAM instruction}
 end;         // Procedure / function "T6502b.SYS"
                {In case you wish loop detection, the '(JM>0) or (Sp<2)'      }
end.//(brCon=1) {statement above can be extended with 'or (brCond=1)'         }



