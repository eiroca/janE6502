// emulator for 6502 uP written by Stefan Janda (c) 2010
// free for personal or public educational, non-commercial use
// any commercial use (direct or indirect) needs special permission
// please contact: steve082010jay [at] arcor.de   remove brackets
unit u6502b;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  b_256: array[False..True] of word = (0, 256);
  b_96: array[False..True] of word = (0, 96);
  b_16: array[False..True] of word = (0, 16);
  b_6: array[False..True] of word = (0, 6);
  b_4: array[False..True] of word = (0, 4);
  b_2: array[False..True] of word = (0, 2);
  b_1: array[False..True] of byte = (0, 1);
  bF: array[False..True] of byte = (1, 0);

const
  bX: array[0..7] of byte = (7, 6, 0, 1, 0, 2, 6, 3);
  iyi: set of byte = [$19, $1B, $39, $3B, $59, $5B, $79, $7B, $96, $97, $99, $9B, $9E, $9F, $B6, $B7, $B9, $BB, $BE, $BF, $D9, $DB, $F9, $FB];
  iy2: set of byte = [$11, $31, $51, $71, $91, $B1, $D1, $F1, $13, $33, $53, $73, $93, $B3, $D3, $F3];
  ix: set of byte = [$01, $03, $14..$17, $1C..$1F, $21, $23, $34..$37, $3C..$3F, $41, $43, $54..$57, $5C..$5F, $61, $63, $74..$77, $7C..$7F, $81, $83, $94, $95, $9C, $9D, $A1, $A3, $B4, $B5, $BC, $BD, $C1, $C3, $D4..$D7, $DC..$DF, $E1, $E3, $F4..$F7, $FC..$FF];

type
  bitInbyte = 0..7;
  tFlr = set of bitInbyte;  // 'set of' enables bitwise accessing Opcodes and Status reg.

const
  MaxMem = $20000; // total memory size 128 KB

type
  // array to hold the addressable RAM and ROM, its twice... the size a standard 6502 can handle, but there are some operating systems' restrictions too
  TMemory = packed array[0..MaxMem - 1] of byte;

const
  I_BRK = $00;
  I_RTI = $40;
  I_RTS = $60;
  I_JMPa = $4C;
  I_JMPi = $6C;
  I_JSRa = $20;
  I_PLA = $68;
  I_PHA = $48;
  I_PHP = $08;
  I_PLP = $28;
  I_BITz = $24;
  I_BITa = $2C;
  I_CLV = $B8;
  I_TYA = $98;
  I_DEX = $CA;
  I_INY = $C8;
  I_INX = $E8;

const
  STACK_START: word = $1FF;

type

  { T6502b }

  T6502b = class
  public
    yc: int64;  // cycle time consumed by emulation
    running: boolean; // semaphore
    IRQ, RST, NMI: boolean;
    jam: boolean;
    alwd: set of byte;
    ChkMem: procedure(rmw: word; var adr: integer);
    mr: packed record // 6502 status and memory
      case byte of // Multitype access
        0: ( // flat access
          AR1: array[0..MaxMem + 23] of byte; // memory & regiters
        );
        1: (
          // the order of registers is chosen deliberately with respect to their implicit encoding,
          // eg. YR is addressed only when lowest two bits of Opcode = 0
          mem: TMemory;
          YR: byte; // Y-Register
          AC: byte; // accumulator
          XR: byte; // X-Register
          Mx: byte; // 'mixed' register which takes the result if accumulator and XR are switched on simultaneously
          SP_L, SP_H: byte;
          PC_L, PC_H: byte; // program counter low /high half
          fSR: byte; // status register
          OpCode, ADP: byte;
          ADL, ADH: byte;
          Oper1, Oper2: byte; //-
        );
        2: ( // an alternative set of register names is defined; qu and p0 only fill gaps
          me2: TMemory;
          qu: integer;
          SP16: word; // Stack Pointer
          PC16: word; // 6502 program counter
          mStatus: tFlr; // the same thing as fSR, but an implicit typecast to "set of"
          op0: tFlr; // another version of the instruction byte, implicit typecast to "set of"
          p0: tFlr;
          AxL, AD: word; //-
        );
    end;
  public
    constructor Create;
    procedure SYS(srtAddr: word; AutoIRQClr: boolean = False; runCheck: boolean = False);
  protected
    procedure myFlg(LC: byte);
    function pullB: byte;
    procedure pushB(TP: byte);
  end;

implementation

{ T6502b }

constructor T6502b.Create;
begin
  inherited Create;
  {on power on, the simulated Reset line is passive}
  {Reset , if needed , must be set expressly}
  RST := True;
  IRQ := False;
  NMI := False;
  {initialize stack pointer, this is not original 6502's behaviour but useful}
  mr.SP16 := STACK_START;
end;

// Set machine status register flags N for negative and Z for zero results.
// thanks to Sean Eron Anderson's Bit Twiddling Hacks, from http://graphics.stanford.edu/~seander/bithacks.html
// which were of tremendous help to do this in an elegant manner.
procedure T6502b.myflg(LC: byte);
begin // 'Merge bits from two values according to a mask'
  with mr do begin
    fSR := fSR xor ((fSR xor ((LC and $80) + 2 * b_1[LC = 0])) and $82);
  end;
end;

// Write stack
procedure T6502b.pushB(TP: byte);
begin
  with mr do begin
    mem[SP16] := TP;
    SP_L := (SP_L - 1) and $FF;
    Inc(yc);
  end;
end;

// Read stack procedure
function T6502b.pullB(): byte;
begin
  with mr do begin
    SP_L := (SP_L + 1) and $FF;
    Result := mem[SP16];
    Inc(yc);
  end;
end;

procedure T6502b.SYS(srtAddr: word; AutoIRQClr: boolean = False; runCheck: boolean = False);
var
  t2: byte;
  y1: boolean;
  y2: boolean;
  y3: boolean;
  imp_acc: boolean;
  DECorINC: boolean;
  byg, rq0: byte;
  MemCom: boolean;
  ZerO: boolean;
  MW: boolean;
  Arit: boolean;
  A: boolean;
  SBTu: boolean;
  LiN: boolean;
  ReC: boolean;
  LK: boolean;
  C10_bxx: boolean;
  C18_flg: boolean;
  C1A_stk: boolean;
  Tr1: boolean;
  op_len3: boolean;
  poV, pCry, RegZg: byte;
  uEvn: boolean;
  Sp8A: boolean;
  n2: boolean;
  AiQ: boolean; // All interrupt reQuests
  m: boolean;
  indrik: boolean;
  DCI: boolean;
  brCnt, brID: word;
  SpNr: word;
  brCon: boolean;
  bit2, bit3, bit4: boolean;
  rr0: word;
  _r0: smallint;
  IRQTable: word;
  tt: bitInbyte;
  hv: ^byte;
  nn2: byte;
  oC, Ad1, ad2: integer;
  pData: ^byte;
  qData: ^byte;
  rmw: word;
begin
  brID := 0;
  with mr do begin
    if running then exit; // Don't allow reentrance
    running := True;
    PC16 := srtAddr;
    brCon := False;
    repeat
      // Fetch OpCode, this has to be done always 1st operand and 2nd operand simply gets ignored if not needed
      OpCode := mem[PC16];
      Oper1 := mem[PC16 + 1];
      Oper2 := mem[PC16 + 2];
      // Detect 'jam' opcodes like $12, $32, $52, ...
      if (OpCode and $1F = $12) then jam := True;
      // Simulate an hardware interrupt signals: RST = RESET, NMI = non-maskable Interrupt, IRQ = Interrupt request
      AiQ := (IRQ and (not (2 in mStatus))) or RST or NMI;
      if AiQ then OpCode := I_BRK; // Override OpCode with BRK if there's any interrupt
      t2 := OpCode and $1F;
      // 3 OpCode Bytes?
      op_len3 := (t2 = $19) or (t2 >= $1B) or (OpCode and $0F >= $0C);
      // detects implicit addressing mode
      imp_acc := ((OpCode and $0D) = $08);
      // detects all 'implicit' opcodes located in row $18
      C18_flg := (t2 = $18);
      // the same but for instructions located in row $1A
      C1A_stk := (t2 = $1A);
      // detects all 'branch' opcodes located in row $10
      C10_bxx := (t2 = $10);
      bit2 := OpCode and $02 = $02;
      bit3 := OpCode and $04 = $04;
      bit4 := OpCode and $08 = $08;
      ZerO := OpCode and $03 = $00;
      SpNr := (OpCode shr 4) and $0E; // isolate bits 7..5 and move into bit 3..1; note: bit 0 clear
      // now let's check for STA,.. and
      if SpNr = 8 then begin
        // so forth instructions if exist, make DEY and TYA change their places
        OpCode := OpCode xor b_16[ZerO and imp_acc];
        t2 := OpCode and $1F;
      end
      else if C18_flg then begin
        // in general, in CoL18 reside 'Set and Clear' bit instructions except $98 which is in (SpNr=8)
        nn2 := bX[OpCode shr 6 + 4]; // determine which bit - C,V,I,D - has to be changed
        m := (OpCode and $20) = $20; // determine if bit flag has to be set or cleared}
        if (m xor (OpCode = I_CLV)) then Include(mStatus, nn2) else Exclude(mStatus, nn2);
      end;
      Tr1 := op_len3 or (OpCode = I_JSRa); // adds JSR to three-byte instruction detection
      byg := 2 - b_1[imp_acc] + b_1[op_len3]; // delivers number of overall bytes the instruction needs
      LiN := (OpCode < $80); // all instructions from left half of table
      ReC := not LiN; // ... and the remaining from the right half
      MemCom := bit3 or (t2 > $17); // indicates general memory access
      uEvn := Odd(OpCode); // 'uneven' Opcodes; identifier 'odd' is already in use
      indrik := (not bit3) and (not bit4) and uEvn; // is 1 if indirect addressing except $6C JMP
      RegZg := b_1[LiN] + b_1[ReC] * ((OpCode and $3) - b_2[SpNr > $A] * b_1[(OpCode and $3) = 3] + b_2[ZerO and (SpNr = $E)]); // array index RegZg points to
      Ad1 := RegZg + MaxMem; // Compute the register operand [simulated register file]
      qData := @AR1[Ad1]; // make register operand a real pointer
      rr0 := Oper1 + b_1[OpCode in ix] * XR + b_1[OpCode in iyi] * YR; // compute Address I.: non-indirect
      AD := ((rr0 and (b_1[Tr1] * $FF00 + $FF)) + b_256[Tr1] * Oper2); // zeropage treatment, rr0 is 16 bit
      oC := Hi(rr0); // preserve carry for timing calculation if page changed
      rr0 := mem[AD] + b_1[OpCode in iy2] * YR; // compute Address II.: YR indexed addresing modes
      AD := bF[indrik] * AD + b_1[indrik] * (256 * mem[AD + 1] + rr0); // fetch indirect address if need
      Inc(oC, Hi(rr0)); // preserve carry; note: oC is assumed not to be already set
      Arit := (OpCode >= $C0); {Arit==Arithmetics; all codes >=$C0}
      A := LiN and bit2; // ASL and related shift instr.
      SBTu := (SpNr = $E) and uEvn; // True Subtract SBC instr.
      LK := uEvn and LiN; // LK == logic instructions AND,OR,EOR; note: includes ADC
      // MW denotes a subset of read-Modify-Write instructions i.e. INC and DEC.
      // the "in" statement enhances this by some implicit opcodes for INX, DEY etc.
      // also the 'fake DEY' code $98 is included.
      MW := (Arit and bit2 and (bit3 or uEvn)) or (OpCode in [I_TYA, I_DEX, I_INY, I_INX]);
      // DECorINC means "DEC-INC-only", thus, combined ISB / DCP codes cannot influence the  flag register in an undesired way.
      DECorINC := MW and not uEvn;
      Sp8A := ReC and (not Arit); // LDA,LDX,LDY, STA,STX,STY etc. instructions
      // The following lines determine which part of ALU will be switched on later
      DCI := (SpNr = $C) and ZerO and imp_acc; // changes -1 with +1 where needed, INY instead DEY
      y1 := (SpNr = 0) or (SpNr = 4); // condition for ALU: logic OR [+EOR]
      y2 := (SpNr = 2) or (SpNr = 4); // condition for ALU: logic AND[+EOR]}
      y3 := (SpNr = 6); // turns Adder on; note: subtraction goes apart
      m := MemCom or indrik; // indirect case added to memory access
      n2 := C10_bxx or imp_acc; // if implicit or branch, we don't need to access memory
      // compute the memory operand. Gets replaced by AD1 in a number of cases.
      ad2 := AD * bF[imp_acc] * b_1[m];
      ad2 := ad2 + (MaxMem + $D) * bF[n2 or m];
      ad2 := ad2 + b_1[n2] * (Ad1 - b_1[Sp8A] * (RegZg - b_4[C1A_stk] - b_1[t2 < $F]));
      rmw := b_256[imp_acc] * (b_1[MW] + b_2[SpNr = 8] + b_4[A]); // Read-modify-Write flag
      // leave checking the allowed ('alwd') memory operand to an external subroutine.
      // If the externally provided array ALWD determines that an address requires
      // special activity, e.g. output to screen (or if address is between 0 and 1
      // which means we speak to hardware processor port directly), memory access is
      // detected (m>0) while a function pointer for 'U' has been provided,
      // then branch there and have the work done outside. 'U' may modify the
      // given memory pointer because calling convention is 'by reference'. ('VAR')
      // it is possible to use this program without such a subroutine, too.
      if ((Hi(AD) in alwd) or (AD < 2)) and m and (@ChkMem <> nil) then begin
        ChkMem(rmw, ad2);
      end;
      pData := @AR1[ad2]; // finally, make a real pointer out of memory operand.
      // do the timing calculations all in one go
      Inc(yc, 2);
      Inc(yc, bF[imp_acc] * ((b_1[A] + b_1[MW]) * 2 + b_1[m] * b_1[t2 < $10] * (b_1[Tr1] + 1 + b_1[indrik] * 3) + b_1[m] * b_1[t2 > $F] * (2 + b_1[indrik] + (b_1[indrik] + b_1[t2 > $18]) * (b_1[rmw > 0] or oC))));
      // a few opcodes have been left...
      case OpCode of
        I_BITz, I_BITa: begin
          fSR := fSR and $3D or (pData^ and $C0) or b_2[(pData^ and AC) = 0];
        end;
        I_PLA: begin
          AC := pullB();
          myFlg(AC);
          Inc(yc);
        end;
        I_PHA: pushB(AC);
        I_PHP: pushB(fSR or $30);
        I_PLP: begin
          fSR := pullB();
          Inc(yc);
        end;
      end;
      // preamble for ASL,LSR,ROL,ROR
      // first, save old value for CARRY flag into oC [OldCarry];
      oC := fSR and 1;
      // n2 == shift direction left/right because combined unofficial instructions could influence carry several times
      n2 := 6 in op0;
      // ALU stuff
      // the following lines cover 48 instructions regarding bit shifting
      if A then begin
        pData^ := bF[n2] * (Lo(pData^ * 2) + oC * b_1[5 in op0]) + (pData^ div 2 + $80 * oC * b_1[5 in op0]) * b_1[n2];
        myflg(pData^);
        oC := bF[n2] * Hi(pData^ * 2) + b_1[n2] * b_1[Odd(pData^)];
      end;
      // the following lines cover >12 instructions regarding increment and decrement
      if MW then  pData^ := (pData^ + (2 * (b_1[(OpCode and $20) = $20] xor b_1[DCI]) - 1)) and $FF;
      // Following lines cover >30 instructions which add, subtract or compare
      if Arit then begin
        // if SBC or CMP or CPX and so on .. invert the old carry (two's complement) first
        // use carry if a real subtraction took place.
        // intermediate result in r0 is 16 bit wide
        _r0 := qData^ - b_1[SBTu] * (1 - oC) - pData^;
      end
      else if y3 then begin
        // if ADC...
        _r0 := qData^ + pData^ + oC;
      end
      else begin
        // keep old value of p^ if neither ADC nor SBC take place
        _r0 := pData^;
      end;
      nn2 := qData^ xor pData^;
      oC := b_2[Lo(_r0) <> 0];
      poV := (1 - (nn2 shr 7) xor b_1[SBTu]) * ((Lo(_r0) xor qData^) shr 7); // overflow flag
      nn2 := (_r0 xor nn2) and $10 or (_r0 and $F);
      tt := (9 - b_6[y3]) and $07;
      if tt in mStatus then begin
        Inc(_r0, b_1[nn2 > 9] * (6 - b_16[nn2 > $19]));
        oC := _r0 and $80 or oC;
        if _r0 and $1F0 > $90 then begin
          Inc(_r0, $60);
          _r0 := $100 or _r0;
        end;
      end;
      tt := (9 - b_6[SBTu]) and $07;
      if (tt in mStatus) then begin
        oC := (_r0 and $80) or oC or (Hi(_r0) and 1);
        Dec(_r0, b_96[odd(Hi(_r0))] + b_1[nn2 > $F] * (6 - 16 * bF[nn2 > $15]));
        _r0 := smallint((word(_r0) and $FEFF) or (256 * oC));
      end;
      Dec(oC, oC and 1);
      rq0 := _r0 and $FF;
      pCry := Hi(_r0) and 1;
      // the following represents the very core of the ALU: combinatory logic functions AND, OR, EOR these 2 lines cover 24 instructions
      rq0 := b_1[y3 or Arit] * rq0 + b_1[LK and (SpNr <> 6)] * ((b_1[y1] * (qData^ or rq0) or b_1[y2] * (qData^ and rq0)) and (not (b_1[y2] * b_1[y1] * (qData^ and rq0))));
      if (LK or SBTu) then qData^ := rq0; // store result of true SBC or ADC back into processor's register
      if (y3 xor SBTu) and (3 in mStatus) then rq0 := oC;
      // Condition for Carry, combines everything into C which could have influenced it
      y2 := Arit and (not imp_acc) and (not C10_bxx) and (not DECorINC);
      y3 := LK and (SpNr = 6);
      n2 := y3 or y2 or A;
      Dec(fSR, fSR and b_1[n2]); // make machine status clean first
      Inc(fSR, b_1[A and (not y3) and m] + b_1[(b_1[n2] - b_1[A]) > 0] * (pCry xor b_1[ReC])); // set carry, complement if needed
      // Set or clear overflow flag only in ADC / SBC cases
      if (LK and (SpNr = 6)) or (Arit and SBTu) then begin
        if poV = 1 then Include(mStatus, 6) else Exclude(mStatus, 6);
      end;
      // the following covers approx. 62 data transportation instructions like LDA, STA
      if (Sp8A and (not C10_bxx)) then begin
        if SpNr = 8 then begin
          // if opcode in column 8 then simply swap source w/ destination
          hv := pData;
          pData := qData;
          qData := hv;
        end;
        qData^ := pData^; // do the real data transfer  here
        // set N , Z flags according to data
        if (OpCode < $90 * b_1[imp_acc]) or (OpCode * bF[OpCode = $B8] > $9F) then myFlg(pData^);
      end;
      // set or reset Negative and Zero flags according to result in rq0 except in implicit addressing cases
      if LK or y2 then myFlg(rq0);
      // DEC / INC flag treatment
      if DECorINC then myFlg(pData^);
      // a second call outside in order to manage write access to external memories
      if ((Hi(AD) in alwd) or (AD < 2)) and (rmw > 0) and (@ChkMem <> nil) then begin
        // the +1 serves to distinguish this call from first call above. this one handles WRITEs
        ChkMem(rmw + 1, ad2);
      end;
      // now ALU is finished and we must check for jumps and branches
      if (t2 = $10) and (bX[OpCode shr 6] in mStatus = (5 in op0)) then begin // check branch opcode & condition
        // AxL holds 16 bit temporary result
        AxL := 2 + PC16 + int8(Oper1);
        Inc(yc, 1 + bF[Hi(AxL) = Hi(PC16)]);
        PC16 := AxL; // set Program counter to new value
        if brID <> PC16 then begin
          // some magic in order to detect idle loops
          brID := PC16;
          brCnt := 1;
        end
        else begin
          // check whether the jump goes back
          if Oper1 and $80 = $80 then Inc(brCnt);
          // 1, if we did the same loop too often
          if brCnt >= 999 then begin
            brCon := True;
            brCnt := 0;
          end;
        end;
      end
      else begin
        // no branch, normal PC increment, but don't increment if interrupts
        Inc(PC16, byg * bF[AiQ]);
      end;
      // JSR, BRK, INT: Save Program counter to stack and Status when needed, too
      if OpCode and $DF = 0 then begin
        // Hardware/software interrupt or JSR
        // push PC->stack; adjust time
        pushB(Hi(PC16));
        pushB(Lo(PC16));
        Inc(yc, 2);
        if OpCode = I_BRK then begin // Hardware/software interrupt, No JSR
          // push STATUS too, set I set break flag on stack
          pushB((fSR and $CF) or $20 or $10 * bF[AiQ]);
          Include(mStatus, 2);
        end;
      end;
      // JMP, JSR: Change program counter when standard JMP (non-indirect) or JSR occur
      if OpCode in [I_JMPa, I_JSRa] then begin
        PC16 := Oper1 + 256 * Oper2;
        if (OpCode = I_JMPa) then Dec(yc);
      end
      else if OpCode = I_BRK then begin
        // The following lines cover tasks related to Interrupts, BRK, RESET and NMI
        IRQ := IRQ and not AutoIRQClr;
        if RST then begin
          SP16 := STACK_START;
          NMI := False;
        end;
        IRQTable := $FFFE - b_2[RST] - b_4[NMI];
        PC16 := mem[IRQTable] + 256 * mem[IRQTable + 1];
        RST := False;
      end
      else if OpCode = I_JMPi then begin
        // fetch indirect address for PC if JUMP $6C
        PC16 := mem[AD] + 256 * mem[AD + 1];
        Inc(yc);
      end
      else if (OpCode = I_RTS) then begin
        // Return from subroutine RTS
        Inc(yc, 2);
        PC16 := pullB() + 256 * pullB() + 1;
      end
      else if OpCode = I_RTI then begin
        // Return from Interrupt RTI
        Inc(yc, 1);
        fSR := pullB();
        PC16 := pullB() + 256 * pullB();
      end;
    until jam or (runCheck and (brCon or (SP16 < $102)));
    running := False;
  end;
end;

end.

