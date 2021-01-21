{© S.JANDA'10}
program c164j;

uses
  j6502b, miniWin;
//  windows,classes,CommDlg,MMsystem;

const
  VIA = $DC00;
  ScBs = 1024;
  ScnWidth = 40;
  ScnSize = 1000;
  CtrY = 400;
  ctrX = 640;
  CL: array[0..2] of cardinal = ($801010, $D0A0A0, $D0A0A0);
  fNam: array[0..1] of string = ('Basic', 'Kernal');
  fAd: array[0..1] of word = ($A0, $E0);
  pmKeys: string = '13579'#$BB + #$BA#8#$DC'WRYIP'#$6A#13#11 + 'ADGJL'#$C0#$27#$1b#$10'XVN' + #$BC#$bd#$28' ZCBM'#$BE + #$10#$72#$11'SFHK'#$DE#$dd + #$0'QETUO@'#$BF#$74'24680'#$DB + #36 + #$70;

var
  lpBuf: array[0..127] of char;
  KeyMatrix: array[0..7, 0..7] of byte;
  gs, KeySelct: byte;
  wClss: array[0..9] of longword;
  bRun, IDThread: DWORD;
  hthr, hdl, bsh, hDC, hMainFnt, TmId, men, KeyAxd, GlobKey, stAdd, i, j, flag, hfI: longword;
  msg: tmsg;
  m6502a: T6502b;
  ptc: pTimeCaps;
  of1: array[0..18] of longword;
  mr: trect;
  sc: string;

  procedure TiPrc(uID, uMsg: longword; dwUsr, dw1, dw2: QWord); stdcall;
  begin
    m6502a.IQ := 0;
  end;

  procedure Screen_Out(Adr_L: word; q1: byte);
  begin
    bRun := Adr_L - ScBs;
    flag := q1 shr 7;
    q1 := q1 and $7F;
    sc := char((q1 + 32 * (Ord(q1 < 32) * 2 + Ord(q1 > 63) + Ord(q1 > 95))));
    if (Adr_L < (ScBs + scnsize)) and (Adr_L >= (ScBs)) then begin
      hdc := getDC(hdl);
      SelectObject(hdc, hMainFnt);
      SetTextColor(hdc, CL[1 - flag]);
      SetBkColor(hdc, CL[flag]);
      TextOut(hdc, 1 + ((bRun) mod (ScnWidth) * CtrX) div (ScnWidth), 1 + ((Adr_L - ScBs) div (40) * CtrY) div 25, PChar(sc), 1);
      releaseDC(hdl, hDc);
    end;
  end;

  function keyread: byte;
  var
    iB, by1: byte;
    x, i, j, m: integer;
  begin
    Inc(KeyAxd);
    iB := not KeySelct;
    by1 := $FF - Ord(KeySelct = 0) * GlobKey * $FF;
    if (KeySelct > 0) and (KeySelct < $FF) and (GlobKey >= 1) then begin
      by1 := 0;
      m := -1;
      for i := 7 downto 0 do if (m < 0) and (((1 shl i) and iB) <> 0) then m := i;
      for j := 0 to 7 do by1 := by1 + keymatrix[j, m * Ord(m > -1)] * (1 shl j);
      by1 := by1 xor $FF;
    end;
    i := 3;
    j := 7;
    x := 1 and ((by1 shr i) xor (by1 shr j));
    keyread := by1 xor ((x shl i) or (x shl j));
    KeyAxd := 0;
  end;

  function memCheck(f1: word; var f2: integer): byte;
  var
    x, i, b: byte;
  begin
    bRun := f2 - ScBs;
    if (f1 = 0) and not ((f2 < (ScBs + scnsize)) and (f2 >= (ScBs))) then begin
      if (f2 = VIA + 1) then begin
        f2 := $20003;
        m6502a.MR.AR1[f2] := keyread;
      end;
      if f2 = $D012 then begin
        f2 := $20003;
        m6502a.MR.AR1[f2] := Hi(m6502a.yC shl 2);
      end;
    end;
    if (f1 = $200) and (Hi(f2) = $A0) then begin
      f2 := $20003;
    end;
    if (Hi(f1) > 0) and (Lo(f1) = 1) then begin
      if (f2 < (ScBs + ScnSize)) and (f2 >= (ScBs)) then begin
        PostMessage(hdl, 1024 + 1, f2, m6502a.MR.AR1[f2]);
      end
      else begin
        if (f2 = VIA) then  begin
          b := m6502a.MR.AR1[f2];
          i := 0;
          j := 7;
          x := 1 and ((b shr i) xor (b shr j));
          KeySelct := b xor ((x shl i) or (x shl j));
        end;
        if VIA + 5 = f2 then TmId := timeSetEvent(34, 2, @TiPrc, 0, 1);
      end;
    end;
  end;

  procedure threadRun;
  begin
    repeat
      r6502_sys(m6502a, stAdd, 1);
      stAdd := m6502a.mr.PC;
      if (m6502a.JM < 1) then Sleep(10);
    until (m6502a.JM > 0);
  end;

  procedure LOAD(t1: word; S: PChar);
  var
    S1: PChar;
    a, b: word;
  begin
    with m6502a.mr do begin
      if (S <> nil) or GetOpenFileName(OPENFILENAME(of1)) then begin
        if S <> nil then S1 := S else S1 := lpBuf;
        hfI := CreateFile(S1, $80000000, 0, nil, 3, $80, 0);
        if nil = S then ReadFile(hfI, t1, 2, bRun, nil);
        ReadFile(hfI, m6502a.mr.mm[t1], 65535, bRun, nil);
        CloseHandle(hfI);
        a := Lo(t1 + brun);
        b := Hi(brun + t1);
        mm[45] := a;
        mm[46] := b;
        mm[47] := a;
        mm[48] := b;
        mm[49] := a;
        mm[50] := b;
      end;
    end;
  end;

  function WindowProc(hwn, msg1, wpr, lpr: DWORD): DWORD; stdcall;
  var
    i: integer;
  begin
    Result := defwindowproc(hwn, msg1, wpr, lpr);
    case msg1 of
      $112: begin
        if wpr = $402 then LOAD(0, nil);
        if wpr = $403 then begin
          if GetSaveFileName(OPENFILENAME(of1)) then begin
            with m6502a.mr do begin
              j := 256 * mm[46] + mm[45];
              i := 256 * mm[44] - mm[43];
              hfI := CreateFile(lpBuf, 1 shl 30, 0, nil, 2, $80, 0);
              WriteFile(hfI, i, 2, bRun, nil);
              WriteFile(hfI, mm[i], j - i + 1, bRun, nil);
              CloseHandle(hfI);
            end;
          end;
        end;
      end;
      1025: Screen_Out(Wpr, Lpr);
      $85..$A9: begin
        hdc := GetWindowDC(hdl);
        mr.left := 0;
        mr.top := 0;
        mr.right := ctrx + gs;
        mr.bottom := getsystemmetrics(4) + gs;
        bsh := CreateSolidBrush(CL[2]);
        FillRect(hdc, mr, bsh);
        FillRect(hdc, rect(0, 0, gs, mr.bottom + ctry), bsh);
        FillRect(hdc, rect(ctrx + 1, 0, ctrx + gs, mr.bottom + ctry), bsh);
        FillRect(hdc, rect(0, mr.bottom + ctry - 0 div 2, ctrx + gs, mr.bottom + ctry + gs), bsh);
        FillRect(hdc, rect(gs, gs, mr.bottom - 2 * gs, mr.bottom - 2 * gs), CL[1]);
        ReleaseDC(hdl, hdc);
      end;
      $F: begin
        for i := 0 to scnsize do begin
          Screen_Out(i + ScBs, m6502a.mr.mM[i + ScBs]);
        end;
      end;
      256, 257: begin
        i := pos((char(lo(wpr))), pmkeys) - 1;
        GlobKey := Ord(msg1 = $100);
        if i >= 0 then KeyMatrix[(i div 8), (i mod 8)] := GlobKey;
      end;
      2: begin
        m6502a.JM := 1;
        WaitForSingleObject(hthr, $FFFFFFFF);
        timeEndPeriod(ptc^.wPeriodMin);
        Dispose(ptc);
        timekillevent(tmid);
        Halt;
      end;
    end;
  end;

begin
  gs := 2 * Getsystemmetrics(45);
  of1[0] := 76;
  of1[1] := hdl;
  of1[7] := DWORD(@lpBuf);
  of1[8] := 127;
  of1[13] := $1800;
  New(ptc);
  timeGetDevCaps(ptc, 8);
  timebeginPeriod(ptc^.wPeriodMin);
  j := GetModuleFileName(HINSTANCE, lpBuf, 127);
  repeat
    Dec(j);
  until (j = 0) or (lpBuf[j] = '\') or (lpBuf[j] = ':');
  Inc(j);
  for i := 0 to 1 do LOAD(fAd[i] * 256, PChar(concat(Copy(LpBuf, 1, J), fnam[i])));
  m6502a.U := @memCheck;
  m6502a.RS := 0;
  m6502a.alwd := [4, 5, 6, 7, $90, $9F, $A0, $B0, $D0, $DC, $DD];
  wclss[0] := $E0;
  wclss[1] := cardinal(@WindowProc);
  wclss[4] := HINSTANCE;
  wclss[6] := LoadCursor(0, PChar($7F00));
  wclss[7] := 03{16};
  wclss[9] := cardinal(PChar('M1'));
  miniwin.RegisterClass(tWndclass(wclss));
  hdl := CreateWindowEx(0, 'M1', '', $80000, 300, 110, CtrX + gs, CtrY + 2 * gs + GetSystemMetrics(4), 0, 0, HINSTANCE, nil);
  ShowWindow(hdl, 5);
  men := GetSystemMenu(hdl, False);
  ModifyMenu(men, 3, 1024, 1026, '&LOAD');
  ModifyMenu(Men, 4, 1024, 1027, 'S&AVE');
  hMainFnt := CreateFont(16, 16, 0, 0, 400, 0, 0, 0, 1, 0, 0, 0, 1, 'CBM');
  hthr := CreateThread(nil, 0, @threadRun, nil, 0, IDThread);
  while (getmessage(msg, hdl, 0, 0)) do begin
    translatemessage(msg);
    dispatchmessage(msg);
  end;
end.

