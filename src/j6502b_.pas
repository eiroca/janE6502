unit j6502b;interface const k1=$20000;bX:array[0..7]of byte=(7,6,0,1,0,2,6,3);H:
array[false..true]of WORD=(0,256);iyi:set of byte=[25,27,57,59,89,91,$79,$7b,$96
,$97,$99,$9b,$9E,$9F,$B6,$B7,$B9,$BB,$BE,$BF,$d9,$db,$f9,$fb];ix:set of byte=[20
..23,28..31,52..55,60..63,84..87,$74..$77,$94,$95,$B4,$B5,$D4..$D7,$F4..$F7,92..
95,$7C..$7F,$9C,$9D,$BC,$BD,$DC..$DF,$FC..$FF,1,33,65,97,$81,$A1,$C1,$E1,3,$23,
$43,$63,$83,$A3,$C3,$E3];iy2:set of byte=[17,49,81,$71,$91,$b1,$d1,$f1,19,51,83,
$73,$93,$b3,$d3,$f3];F:array[false..true]of byte=(1,0);type tFlr=Set of 0..7;t7=
packed array[0..k1-1]of byte;T6502b=record IQ,RS,NM,JM,SPO,n1:byte;yc,oC,Ad1,Ad2
:integer;p,q,hv:^byte;rmw:WORD;U:function(f1:WORD;VAR G:integer):byte;alwd:set
of byte;mr:packed record case byte of 0:(AR1:array[0..k1+23]of byte);1:(mM:t7;YR
,AC,XR,Mx,SP,SPH,PL,PH,fSR,O1,ADP,ADL,ADH,O2,O3:byte;due,v:integer);2:(me2:t7;qu
:integer;yyH,PC:word;mSTa,op0,p0:TFlr;AxL,AD:WORD);end;End;procedure r6502_SYS
(VAR m65:t6502b;aes,E:WORD);implementation procedure wT(var m65:t6502b;TP:byte);
begin M65.mr.mM[M65.mr.yyh]:=TP;Inc(M65.yc);Dec(M65.mr.SP);end;function myflg(
var m65:t6502b;lC:byte):byte;begin M65.mr.fSR:=M65.mr.fSR xor((M65.mr.fSR xor((
lc and$80)+2*ord(lc=0)))and$82);end;function rT(var m65:t6502b):byte;begin  Inc(
M65.mr.SP);rT:=M65.mr.mM[M65.mr.YYH];Inc(M65.yc);end;procedure r6502_SYS(var m65
:t6502b;aes,E:WORD);var t2,y1,y2,y3,byg,iPz,rq0,MemCom,ZerO,A,MW,Arit,DCIC,SBTu,
LK,LiN,ReC,CoL10,CoL18,CoL1a,Tr1,pCry,poV,Tri,uEvn,RegZg,AiQ,n2,DCI,indrk,Sp8A,m
:byte;brCnt,brID,SpNr,brCon:Word;bT1,bT2:boolean;r0:smallInt;begin with m65
do begin WITH m65.MR DO BEGIN if SPO>0then exit;Inc(SPO);PC:=AeS;brCon:=0;Repeat
O1:=mM[PC];Inc(JM,ord(O1 and 31=18));O2:=mM[PC+1];O3:=mM[PC+2];AiQ:=(1-((1-IQ)*F
[2in mSta]))*Rs*Nm;if AiQ<>1then O1:=0;Tri:=ord((O1 and 31=25)or(O1 and$F>=$C)or
(O1 and$1f>=27));iPz:=ord(O1 and$D=8);CoL18:=ord(O1 and$1F=24);CoL1a:=ord(O1 and
$1f=26);bT1:=O1 and 2=2;bT2:=O1 and 4=4;SpNr:=O1 shr 5*2;ZerO:=ord(O1 and$3=0);
CoL10:=ord(O1 and$1F=16);IF SpNr=8then O1:=O1 xor(ZerO*16*iPz)ELSE If CoL18>0
then BEGIN m:=O1 and$20shr 5;n2:=bX[O1 shr 6+4];if Odd(m xor(ord(O1=$B8)))then
include(mSta,n2)else Exclude(mSta,n2);END;t2:=31and O1;Tr1:=Tri+ord(O1=32);byg:=
2-iPz+Tri;LiN:=ord(O1<$80);ReC:=1-LiN;MemCom:=ord(bT2 or(t2>$17));uEvn:=ord(Odd(
O1));indrk:=F[bT2]*F[O1 and 8=8]*uEvn;RegZg:=LiN+ReC*((O1 and$3)-ord(SpNr>$A)*2*
ord((O1 and$3)=3)+ZerO*ord(SpNr=$E)*2);Ad1:=RegZg+k1;q:=@AR1[Ad1];r0:=(O2+ord((
O1 in ix))*XR+ord(O1 in iyi)*YR);AD:=((r0 and(Tr1*$FF00+$FF))+H[Tr1=1]*O3);oc:=
Hi(r0);r0:=mM[AD]+ord(O1 in iy2)*YR;AD:=(1-indrk)*AD+indrk*(256*mM[AD+1]+r0);inc
(oc,Hi(r0));Arit:=ord(O1>$BF);A:=LiN*ord(bT1);SBTu:=ord(SpNr=$E)*uEvn;LK:=uEvn*
LiN;MW:=ord(((arit>0)and bT1 And(bT2 or(uEvn=1)))or(O1 in[$98,$CA,$C8,$E8]));
DCIC:=MW-uEvn*MW;Sp8A:=ReC*(1-Arit);DCI:=ord(SpNr=$C)*ZerO*iPz;y1:=ord(SpNr=0)+
ord(SpNr=4);y2:=ord(2=SpNr)+ord(SpNr=4);m:=MemCom+indrk;n2:=CoL10+iPz;y3:=ord
(SpNr=6);Ad2:=AD*(1-iPz)*m+(k1+$D)*(1-n2)*(1-m)+n2*(Ad1-ord(Sp8A=1)*(RegZg-CoL1A
*4-ord(t2<$F)));rmw:=256*(2*ord(SpNr=8)+4*A+MW)*(1-ipz);if((Hi(AD)in Alwd)or(AD<
2))and(m>0)and(nil<>@U)then U(rmw,ad2);p:=@ar1[Ad2];Inc(yc,2+(1-ipz)*((A+MW)*2+m
*ord(t2<$10)*(Tr1+1+indrk*3)+m*ord(t2>$f)*(2+indrk+(indrk+ord(t2>$18))*(ord(rmw>
0)or oC))));CASE O1 of$24,44:fSR:=fSR and$3Dor(p^and$C0)or 2*ord(p^and AC=0);$68
:begin AC:=rT(m65);myFlg(m65,AC);Inc(yc);end;$48:wT(m65,AC);8:wT(m65,fSR or$30);
40:begin fSR:=rT(m65);Inc(yc);end;END;oC:=fSR And 1;n2:=ord(6in Op0);if A=1then
begin m:=(1-n2)*Hi(p^*2)+n2*ord(Odd(p^));p^:=(1-n2)*(Lo(p^*2)+oC*ord(5in op0))+(
p^DIV 2+$80*oC*ord(5in op0))*n2;myflg(m65,p^);oC:=m;end;p^:=(1-MW)*p^+MW*(p^+(2*
((O1 and$20)DIV$20xor DCI)-1));r0:=p^*(1-(y3+Arit))+y3*(q^+p^+oC)+Arit*(q^-SBTu*
(1-oC)-p^);n2:=q^xor p^;oC:=2*ord(Lo(r0)<>0);poV:=(1-(n2 shr 7)xor sbtu)*((Lo(r0
)xor q^)shr 7);n2:=(r0 xor n2)and$10or(r0 and$F);if(9-y3*6in mSta)then begin Inc
(r0,ord(n2>9)*(6-16*ord(n2>25))); oC:=r0 and$80or oC;if r0 and$1f0>$90then begin
Inc(r0,96);r0:=256or r0;end;end;if(9-SBTU*6in mSta)then begin oC:=(r0 and$80)or
oC or(Hi(r0)and 1);Dec(r0,96*ord(odd(Hi(r0)))+ord(n2>$F)*(6-16*F[n2>$15]));r0:=(
r0 and$FEFF)or(256*oC);end;Dec(oC,oC and 1);rq0:=Lo(r0); pCry:=Hi(r0)and 1;rq0:=
(y3+Arit)*rq0+(LK*F[SpNr=6])*((y1*(q^or rq0)or y2*(q^and rq0))and(NOT(y2*y1*(q^
and rq0))));IF(LK+SBTu)>0THEN q^:=rq0;if(y3+SBTU=1)and(3in msta)then rq0:=oC;y2
:=(Arit*(1-iPz)*(1-CoL10)*(1-DCIC));y3:=LK*ord(SpNr=6);n2:=y3+y2+A;Inc(fSR,A*(1-
y3)*m+ord((n2-A)>0)*(pCry xor ReC)-fSR and 1*ord((n2)>0));if(0<LK*ord(SpNr=6))or
(0<(Arit*SBTu))then begin if poV=1then Include(mSta,6)else Exclude(mSta,6);end;
IF(Sp8A=1)and(CoL10=0)THEN BEGIN if SpNr=8then begin hv:=p;p:=q;q:=hv;end;q^:=p^
;if(O1<$90*ipz)or(O1*F[O1=$B8]>$9F)then myFlg(m65,p^);END;if LK+y2>0then myFlg(
m65,rq0);if DCIC>0then myFlg(m65,p^);IF((Hi(AD)in Alwd)or(AD<2))and(rmw>0)and(@U
<>nil)then U(rmw+1,Ad2);if(t2=16)and(bX[O1 shr 6]in mSta=(5in op0))then begin
AxL:=2+PC+ShortInt(O2);Inc(yC,1+F[ADH=PH]);PC:=AxL;if brID<>pc then begin brId:=
PC;brCnt:=1;end else begin if O2 div$80>0then inc(brCnt);if 259<brCnt then brCon
:=1;end end ELSE Inc(PC,byg*AiQ);if O1 and$df=0then begin wT(m65,
Hi(PC));wT(m65,Lo(PC));Inc(yc,2);if O1=0then begin wT(m65,fSR and$CFor 16*(
2+aIQ));Include(mSta,2);end;end;if O1 in[76,32]then PC:=O2+256*O3;DEC(yc,ord(O1=
76));if O1=0then begin IQ:=IQ or E;if RS=0then begin due:=MaxInt;yyh:=511;NM:=1;
v:=1;end;PC:=mM[$FFF8+2*RS+4*NM]+256*mM[$FFF8+2*RS+4*NM+1];RS:=1;end;if O1=$6C
then begin PC:=mM[AD]+256*mM[AD+1];Inc(yc);end;if O1 in[64,96]then begin
Inc(yc,ord(O1=96)+1);IF O1=64 then fSR:=rT(m65);PC:=rT(m65)+256*rT(m65)+ord(O1=
96);end;UNTIL(JM>0){or(Sp<2)}or(brCon=1);Dec(SPO);END;ENd;end;end.{© S.JANDA'10}
