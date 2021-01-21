unit MiniWin; {enables the emulator to access several libraries}{$A+} {$Z1}
interface
type  DWORD =Longword; OPENFILENAME = array[0..18] of LongWord;
      TRect = record case byte of 0:(A:array[0..3]of integer);1:(Left,Top,Right,
              Bottom:Integer);      end;
tMSG =      array[0..6]of Longword;
tWNDCLASS = array[0..9]of Longword;
pTimeCaps = ^tTC;tTC=record wPeriodMin,wPeriodMax:Longword end;
HDC = LongWord; HMENU = LongWord;
TSecurityAttributes=array[0..2] of LongWord;
PSecurityAttributes=^TSecurityAttributes;
pOverlapped = ^TOverlapped; TOverlapped = array[0..4] of LongWord;
function Rect(rLft,rTop,rRgt,rBtm:Integer): TRect;
//KERNEL                           
function CreateFile(lpFileName:PChar;dwDesiredAccess,dwShareMode:Longword;lpSecurityAttributes:PSecurityAttributes;dwCreationDisposition,dwFlagsAndAttributes:Longword;hTemplateFile: Longword):Longword;stdcall;external'kernel32.dll'name'CreateFileA';
function CloseHandle(hObject: Longword): BOOLean; stdcall; external 'kernel32.dll';
function ReadFile(hFile: Longword; var Buffer; nNumberOfBytesToRead: Longword;  var lpNumberOfBytesRead: Longword; lpOverlapped: POverlapped): BOOLean; stdcall;external 'kernel32.dll' ;
function WriteFile(hFile:Longword;const Buffer;nNumberOfBytesToWrite:Longword;
  var lpNumberOfBytesWritten: Longword; lpOverlapped: POverlapped): BOOLean; stdcall;external 'kernel32.dll';
function GetModuleFileName(hModule: Longword; lpFilename: PChar; nSize: Longword): Longword; stdcall;external 'kernel32.dll' name 'GetModuleFileNameA';
procedure Sleep(dwMilliseconds: Longword); stdcall;external 'kernel32.dll';
function WaitForSingleObject(hHandle: Longword; dwMilliseconds: Longword): Longword; stdcall;external 'kernel32.dll' ;
function CreateThread(lpThreadAttributes: Pointer;  dwStackSize: Longword; lpStartAddress: Pointer;
 lpParameter: Pointer; dwCreationFlags: Longword; var lpThreadId: Longword): Longword; stdcall;external 'kernel32.dll' ;
// GDI
function CreateFont(nHeight, nWidth, nEscapement, nOrientaion, fnWeight: Integer;
  fdwItalic, fdwUnderline, fdwStrikeOut, fdwCharSet, fdwOutputPrecision,
  fdwClipPrecision, fdwQuality, fdwPitchAndFamily: Longword; lpszFace: PChar): Longword; stdcall;external 'gdi32.dll' name 'CreateFontA';
// USER32
function LoadCursor(hInstance: Longword; lpCursorName: PAnsiChar):Cardinal; stdcall;external 'user32.dll' name 'LoadCursorA';
function RegisterClass(const lpWndClass{: TWndClass}): WORD; stdcall;external 'user32.dll' name 'RegisterClassA';
function CreateWindowEx(dwExStyle: Longword; lpClassName: PChar;
  lpWindowName: PChar; dwStyle: Longword; X, Y, nWidth, nHeight: Integer;
  hWndParent: Longword; hMenu: HMENU; hInstance: Longword; lpParam: Pointer): Longword; stdcall;external 'user32.dll' name 'CreateWindowExA';
function GetSystemMetrics(nIndex: Integer): Integer; stdcall;external 'user32.dll' ;
function ShowWindow(hWnd: Longword; nCmdShow: Integer): BOOLEAN; stdcall;external 'user32.dll'
function GetSystemMenu(hWnd: Longword; bRevert: BOOLean): HMENU; stdcall;external 'user32.dll'
function ModifyMenu(hMnu: HMENU; uPosition, uFlags, uIDNewItem: longword;lpNewItem: PChar): BOOLean; stdcall;external 'user32.dll' name 'ModifyMenuA';
function GetMessage(var lpMsg: TMsg; hWnd: Longword;
  wMsgFilterMin, wMsgFilterMax: longword): BOOLean; stdcall;external 'user32.dll' name 'GetMessageA';
function GetDC(hWnd: Longword): HDC; stdcall; external 'user32.dll'
function TranslateMessage(const lpMsg: TMsg): BOOLean; stdcall;external 'user32.dll'
function DispatchMessage(const lpMsg: TMsg): Integer; stdcall;external 'user32.dll' name 'DispatchMessageA';
function DefWindowProc(hWnd: Longword; Msg: longword; wParam: integer; lParam:integer): Integer; stdcall;external 'User32.dll' name 'DefWindowProcA';
function PostMessage(hWnd: Longword; Msg: longword; wParam: integer; lParam: integer): Boolean; stdcall;external 'User32.dll' name 'PostMessageA';
function MessageBox(Wndhandle: Longword; zgText, ZgHeadline:  PAnsiChar; uType: longword): Integer; stdcall;external 'User32.dll'  name 'MessageBoxA';
function FillRect(hDC: HDC; const lprc: TRect; hbr: Longword): Integer; stdcall;external 'user32.dll';
function GetWindowDC(hWnd: Longword): HDC; stdcall;external 'user32.dll' ;
function ReleaseDC(hWnd: Longword; hDC: HDC): Integer; stdcall;external 'user32.dll';
//Comdlg32             used array positions 0 1 7 8 13 in openFileName
function GetOpenFileName(var OpenFile): Boolean; stdcall;external 'comdlg32.dll' name 'GetOpenFileNameA';
function GetSaveFileName(var OpenFile): Boolean; stdcall;external 'comdlg32.dll' name 'GetSaveFileNameA';
function SelectObject(DC: HDC; p2: Longword): Longword; stdcall;external 'gdi32.dll';
function SetTextColor(DC: HDC; Color: Longword): Longword; stdcall;external 'gdi32.dll' ;
function SetBkColor(DC: HDC; Color: Longword): Longword; stdcall;external 'gdi32.dll' ;
function TextOut(DC: HDC; X, Y: Integer; Str: PChar; Count: Integer): BOOLean; stdcall;external 'gdi32.dll' name 'TextOutA';
function CreateSolidBrush(p1: Longword): Longword; stdcall;external 'gdi32.dll';
//MMSystem, too:
function timeBeginPeriod(uPeriod: longword): Longword; stdcall;external 'winMM.dll' name 'timeBeginPeriod';
function timeEndPeriod(uPeriod: longword): Longword; stdcall;external 'winMM.dll' name 'timeEndPeriod';
function timeSetEvent(uDelay, uResolution: longword;
  lpFunction: pointer; dwUser: Longword; uFlags: longword): Longword; stdcall;external 'winMM.dll' name 'timeSetEvent';
function timeKillEvent(uTimerID: longword): Longword; stdcall;external 'winMM.dll' name 'timeGetDevCaps';
function timeGetDevCaps(uPTC:pTimeCaps; bTime: longword):Longword; stdcall;external 'winMM.dll' name 'timeGetDevCaps';
implementation
function Rect(rLft,rTop,rRgt,rBtm:Integer):TRect;
begin with Result do begin A[0]:=rLft;A[1]:=rTop;A[2]:=rRgt;A[3]:=rBtm;end;end;
{$A-}
end.
