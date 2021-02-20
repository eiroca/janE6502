{(c) S.JANDA'10}
unit c164j_gui;

interface

procedure Main();

implementation

uses
  u6502b,
  Windows, CommDlg, MMsystem, Classes;

const
  VIA_00 = $DC00;
  VIA_01 = $DC01;
  VIA_05 = $DC05;
  RASTER = $D012;
  ScnBase = $0400;
  ScnWidth = 40;
  ScnHeight = 25;
  ScnSize = 1000;
  WinSizeHeight = 400;
  WinSizeWidth = 640;
  CharHeight = WinSizeHeight div ScnHeight;
  CharWidth = WinSizeWidth div ScnWidth;
  Col: array[0..2] of cardinal = ($801010, $D0A0A0, $D0A0A0);
  ROM_Names: array[0..1] of string = ('ROM\Basic', 'ROM\Kernal');
  ROM_Address: array[0..1] of word = ($A000, $E000);
  pmKeys: string = '13579'#$BB + #$BA#8#$DC'WRYIP'#$6A#13#11 + 'ADGJL'#$C0#$27#$1b#$10'XVN' + #$BC#$bd#$28' ZCBM'#$BE + #$10#$72#$11'SFHK'#$DE#$dd + #$0'QETUO@'#$BF#$74'24680'#$DB + #36 + #$70;

var
  Buf: array[0..127] of char;
  KeyMatrix: array[0..7, 0..7] of byte;
  gs: byte;
  KeySelct: byte;
  TmId, KeyAxd, GlobKey: longword;
  m6502a: T6502b;

var
  wClss: TWNDCLASS;
  IDThread: DWORD;
  hMainWin: HWND;
  hThread: THandle;
  hMainFnt: HFONT;
  ptc: PTimeCaps;
  of1: TOPENFILENAME;
  mr: TRect;

procedure TiPrc(uID, uMsg: longword; dwUsr, dw1, dw2: QWord); stdcall;
begin
  m6502a.IRQ := True;
end;

procedure Screen_Out(Adr_L: word; q1: byte);
var
  DC: HDC;
  sc: string;
  isReverse: longword;
  offset: word;
  rw, cl: word;
begin
  if ((Adr_L >= (ScnBase)) and (Adr_L < (ScnBase + ScnSize))) then begin
    offset := Adr_L - ScnBase;
    rw := (offset) div (ScnWidth);
    cl := (offset) mod (ScnWidth);
    isReverse := q1 shr 7;
    q1 := q1 and $7F;
    sc := char((q1 + 32 * (Ord(q1 < 32) * 2 + Ord(q1 > 63) + Ord(q1 > 95))));
    DC := getDC(hMainWin);
    SelectObject(DC, hMainFnt);
    SetTextColor(DC, Col[1 - isReverse]);
    SetBkColor(DC, Col[isReverse]);
    TextOut(DC, 1 + cl * CharWidth, 1 + rw * CharHeight, PChar(sc), 1);
    releaseDC(hMainWin, DC);
  end;
end;

procedure DrawScreen();
var
  i: integer;
begin
  for i := ScnBase to ScnBase + ScnSize - 1 do begin
    Screen_Out(i, m6502a.mr.mem[i]);
  end;
end;

function KeyRead: byte;
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
    for j := 0 to 7 do by1 := by1 + KeyMatrix[j, m * Ord(m > -1)] * (1 shl j);
    by1 := by1 xor $FF;
  end;
  i := 3;
  j := 7;
  x := 1 and ((by1 shr i) xor (by1 shr j));
  Result := by1 xor ((x shl i) or (x shl j));
  KeyAxd := 0;
end;

procedure memCheck(rmw: word; var adr: integer);
var
  x, i, j, b: byte;
begin
  if (rmw = 0) and not ((adr < (ScnBase + ScnSize)) and (adr >= (ScnBase))) then begin
    if (adr = VIA_01) then begin
      adr := $20003;
      m6502a.mr.AR1[adr] := KeyRead;
    end;
    if adr = RASTER then begin
      adr := $20003;
      m6502a.mr.AR1[adr] := (m6502a.yc shl 2) and $FF00 shr 8;
    end;
  end;
  if (rmw = $200) and (Hi(adr) = $A0) then begin
    adr := $20003;
  end;
  if (Hi(rmw) > 0) and (Lo(rmw) = 1) then begin
    if (adr < (ScnBase + ScnSize)) and (adr >= (ScnBase)) then begin
      PostMessage(hMainWin, 1024 + 1, adr, m6502a.mr.AR1[adr]);
    end
    else begin
      if (adr = VIA_00) then begin
        b := m6502a.mr.AR1[adr];
        i := 0;
        j := 7;
        x := 1 and ((b shr i) xor (b shr j));
        KeySelct := b xor ((x shl i) or (x shl j));
      end;
      if adr = VIA_05 then begin
        TmId := timeSetEvent(34, 2, @TiPrc, 0, 1);
      end;
    end;
  end;
end;

procedure threadRun;
var
  addr: word;
begin
  addr := $0000;
  repeat
    m6502a.sys(addr, True);
    addr := m6502a.mr.PC16;
    if (not m6502a.jam) then Sleep(10);
  until m6502a.jam;
end;

procedure Load(adr: word);
var
  S1: PChar;
  a, b: byte;
  addr: integer;
  hfI: HANDLE;
  bRun: DWORD;
begin
  bRun := 0;
  with m6502a.mr do begin
    if GetOpenFileName(@of1) then begin
      S1 := Buf;
      hfI := CreateFile(S1, $80000000, 0, nil, 3, $80, 0);
      ReadFile(hfI, adr, 2, bRun, nil);
      ReadFile(hfI, m6502a.mr.mem[adr], 65535, bRun, nil);
      CloseHandle(hfI);
      addr := (adr + bRun) and $FFFF;
      a := addr and $FF;
      b := addr shr 8;
      mem[45] := a;
      mem[46] := b;
      mem[47] := a;
      mem[48] := b;
      mem[49] := a;
      mem[50] := b;
    end;
  end;
end;

procedure LoadROM(adr: word; S1: PChar; siz: word = 8192);
var
  hfI: HANDLE;
  bRun: DWORD;
begin
  bRun := 0;
  hfI := CreateFile(S1, $80000000, 0, nil, 3, $80, 0);
  ReadFile(hfI, m6502a.mr.mem[adr], siz, bRun, nil);
  CloseHandle(hfI);
  if (bRun<>Siz) then RunError(255);
end;

procedure Save();
var
  hfI: HANDLE;
  i, j: integer;
  bRun: DWORD;
begin
  bRun := 0;
  if GetSaveFileName(@of1) then begin
    with m6502a.mr do begin
      j := 256 * mem[46] + mem[45];
      i := 256 * mem[44] + mem[43];
      hfI := CreateFile(Buf, 1 shl 30, 0, nil, 2, $80, 0);
      WriteFile(hfI, i, 2, bRun, nil);
      WriteFile(hfI, mem[i], j - i + 1, bRun, nil);
      CloseHandle(hfI);
    end;
  end;
end;

procedure Redraw();
var
  DC: HDC;
  bsh: HBRUSH;
begin
  if (hMainWin <> 0) then begin
    DC := GetWindowDC(hMainWin);
    mr.left := 0;
    mr.top := 0;
    mr.right := WinSizeWidth + gs;
    mr.bottom := GetSystemMetrics(4) + gs;
    bsh := CreateSolidBrush(Col[2]);
    FillRect(DC, mr, bsh);
    FillRect(DC, Rect(0, 0, gs, mr.bottom + WinSizeHeight), bsh);
    FillRect(DC, Rect(WinSizeWidth + 1, 0, WinSizeWidth + gs, mr.bottom + WinSizeHeight), bsh);
    FillRect(DC, Rect(0, mr.bottom + WinSizeHeight - 0 div 2, WinSizeWidth + gs, mr.bottom + WinSizeHeight + gs), bsh);
    FillRect(DC, Rect(gs, gs, mr.bottom - 2 * gs, mr.bottom - 2 * gs), Col[1]);
    ReleaseDC(hMainWin, DC);
  end;
end;

function WindowProc(hwn: Qword; msg: longword; wpr, lpr: int64): int64; stdcall;
var
  i: integer;
begin
  Result := DefWindowProc(hwn, msg, wpr, lpr);
  case msg of
    $112: begin
      if wpr = $402 then Load(0);
      if wpr = $403 then Save();
    end;
    1025: Screen_Out(Wpr, Lpr);
    $85..$A9: Redraw();
    $F: DrawScreen();
    256, 257: begin
      i := pos((char(lo(wpr))), pmKeys) - 1;
      GlobKey := Ord(msg = $100);
      if i >= 0 then KeyMatrix[(i div 8), (i mod 8)] := GlobKey;
    end;
    2: begin
      m6502a.jam := True;
      WaitForSingleObject(hThread, $FFFFFFFF);
      timeEndPeriod(ptc^.wPeriodMin);
      Dispose(ptc);
      timeKillEvent(TmId);
      Halt;
    end;
  end;
end;

procedure Main;
var
  i, j: DWORD;
  msg: TMSG;
  menu: HMENU;
begin
  hMainWin := 0;
  gs := GetSystemMetrics(45) * 2;
  of1.lStructSize := SizeOf(OPENFILENAME);
  of1.hwndOwner := hMainWin;
  of1.lpstrFile := Buf;
  of1.nMaxFile := SizeOf(Buf);
  of1.Flags := $1800;
  New(ptc);
  timeGetDevCaps(ptc, 8);
  timeBeginPeriod(ptc^.wPeriodMin);
  j := GetModuleFileName(HINSTANCE, Buf, 127);
  repeat
    Dec(j);
  until (j = 0) or (Buf[j] = '\') or (Buf[j] = ':');
  Inc(j);
  m6502a := T6502b.Create;
  for i := 0 to 1 do begin
    LoadRom(ROM_Address[i], PChar(concat(Copy(Buf, 1, J), ROM_Names[i])));
  end;
  m6502a.ChkMem := @memCheck;
  m6502a.RST := True;
  m6502a.alwd := [$04, $05, 06, $07, $90, $9F, $A0, $B0, $D0, $DC, $DD];
  wClss.style := $E0;
  wClss.lpfnWndProc := @WindowProc;
  wClss.hInstance := HINSTANCE;
  wClss.hCursor := LoadCursor(0, PChar($7F00));
  wClss.hbrBackground := 16;
  wClss.lpszClassName := PChar('M1');
  Windows.RegisterClass(wClss);
  hMainWin := CreateWindowEx(0, 'M1', '', $80000, 300, 110, WinSizeWidth + gs, WinSizeHeight + 2 * gs + GetSystemMetrics(4), 0, 0, HINSTANCE, nil);
  of1.hwndOwner := hMainWin;
  ShowWindow(hMainWin, 5);
  menu := GetSystemMenu(hMainWin, False);
  ModifyMenu(menu, 3, 1024, 1026, '&Load');
  ModifyMenu(menu, 4, 1024, 1027, 'S&ave');
  hMainFnt := CreateFont(16, 16, 0, 0, 400, 0, 0, 0, 1, 0, 0, 0, 1, 'CBM');
  hThread := CreateThread(nil, 0, @threadRun, nil, 0, IDThread);
  msg.hwnd := 0;
  while (getmessage(msg, hMainWin, 0, 0)) do begin
    translatemessage(msg);
    dispatchmessage(msg);
  end;
end;

end.

