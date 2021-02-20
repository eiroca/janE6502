unit MiniWin;

{enables the emulator to access several libraries}{$A+} {$Z1}
interface

type
  DWORD = longword;
  OPENFILENAME = array[0..18] of longword;
  TRect = record case byte of 0: (A: array[0..3] of integer);1: (Left, Top, Right, Bottom: integer);  end;
  tMSG = array[0..6] of longword;
  tWNDCLASS = array[0..9] of longword;
  PTimeCaps = ^TTimeCaps;
  TTimeCaps = record
    wPeriodMin: longword;
    wPeriodMax: longword;
  end;
  HDC = longword; HMENU = longword;
  TSecurityAttributes = array[0..2] of longword;
  PSecurityAttributes = ^TSecurityAttributes;
  pOverlapped = ^TOverlapped; TOverlapped = array[0..4] of longword;

function Rect(rLft, rTop, rRgt, rBtm: integer): TRect;
//KERNEL
function CreateFile(lpFileName: PChar; dwDesiredAccess, dwShareMode: longword; lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: longword; hTemplateFile: longword): longword; stdcall; external 'kernel32.dll' Name 'CreateFileA';
function CloseHandle(hObject: longword): boolean; stdcall; external 'kernel32.dll';
function ReadFile(hFile: longword; var Buffer; nNumberOfBytesToRead: longword; var lpNumberOfBytesRead: longword; lpOverlapped: POverlapped): boolean; stdcall; external 'kernel32.dll';
function WriteFile(hFile: longword; const Buffer; nNumberOfBytesToWrite: longword; var lpNumberOfBytesWritten: longword; lpOverlapped: POverlapped): boolean; stdcall; external 'kernel32.dll';
function GetModuleFileName(hModule: longword; lpFilename: PChar; nSize: longword): longword; stdcall; external 'kernel32.dll' Name 'GetModuleFileNameA';
procedure Sleep(dwMilliseconds: longword); stdcall; external 'kernel32.dll';
function WaitForSingleObject(hHandle: longword; dwMilliseconds: longword): longword; stdcall; external 'kernel32.dll';
function CreateThread(lpThreadAttributes: Pointer; dwStackSize: longword; lpStartAddress: Pointer; lpParameter: Pointer; dwCreationFlags: longword; var lpThreadId: longword): longword; stdcall; external 'kernel32.dll';
// GDI
function CreateFont(nHeight, nWidth, nEscapement, nOrientaion, fnWeight: integer; fdwItalic, fdwUnderline, fdwStrikeOut, fdwCharSet, fdwOutputPrecision, fdwClipPrecision, fdwQuality, fdwPitchAndFamily: longword; lpszFace: PChar): longword; stdcall; external 'gdi32.dll' Name 'CreateFontA';
// USER32
function LoadCursor(hInstance: longword; lpCursorName: PAnsiChar): cardinal; stdcall; external 'user32.dll' Name 'LoadCursorA';
function RegisterClass(const lpWndClass{: TWndClass}): word; stdcall; external 'user32.dll' Name 'RegisterClassA';
function CreateWindowEx(dwExStyle: longword; lpClassName: PChar; lpWindowName: PChar; dwStyle: longword; X, Y, nWidth, nHeight: integer; hWndParent: longword; hMenu: HMENU; hInstance: longword; lpParam: Pointer): longword; stdcall; external 'user32.dll' Name 'CreateWindowExA';
function GetSystemMetrics(nIndex: integer): integer; stdcall; external 'user32.dll';
function ShowWindow(hWnd: longword; nCmdShow: integer): boolean; stdcall; external 'user32.dll';
function GetSystemMenu(hWnd: longword; bRevert: boolean): HMENU; stdcall; external 'user32.dll';
function ModifyMenu(hMnu: HMENU; uPosition, uFlags, uIDNewItem: longword; lpNewItem: PChar): boolean; stdcall; external 'user32.dll' Name 'ModifyMenuA';
function GetMessage(var lpMsg: TMsg; hWnd: longword; wMsgFilterMin, wMsgFilterMax: longword): boolean; stdcall; external 'user32.dll' Name 'GetMessageA';
function GetDC(hWnd: longword): HDC; stdcall; external 'user32.dll';
function TranslateMessage(const lpMsg: TMsg): boolean; stdcall; external 'user32.dll';
function DispatchMessage(const lpMsg: TMsg): integer; stdcall; external 'user32.dll' Name 'DispatchMessageA';
function DefWindowProc(hWnd: longword; Msg: longword; wParam: integer; lParam: integer): integer; stdcall; external 'User32.dll' Name 'DefWindowProcA';
function PostMessage(hWnd: longword; Msg: longword; wParam: integer; lParam: integer): boolean; stdcall; external 'User32.dll' Name 'PostMessageA';
function MessageBox(Wndhandle: longword; zgText, ZgHeadline: PAnsiChar; uType: longword): integer; stdcall; external 'User32.dll' Name 'MessageBoxA';
function FillRect(hDC: HDC; const lprc: TRect; hbr: longword): integer; stdcall; external 'user32.dll';
function GetWindowDC(hWnd: longword): HDC; stdcall; external 'user32.dll';
function ReleaseDC(hWnd: longword; hDC: HDC): integer; stdcall; external 'user32.dll';
//Comdlg32             used array positions 0 1 7 8 13 in openFileName
function GetOpenFileName(var OpenFile): boolean; stdcall; external 'comdlg32.dll' Name 'GetOpenFileNameA';
function GetSaveFileName(var OpenFile): boolean; stdcall; external 'comdlg32.dll' Name 'GetSaveFileNameA';
function SelectObject(DC: HDC; p2: longword): longword; stdcall; external 'gdi32.dll';
function SetTextColor(DC: HDC; Color: longword): longword; stdcall; external 'gdi32.dll';
function SetBkColor(DC: HDC; Color: longword): longword; stdcall; external 'gdi32.dll';
function TextOut(DC: HDC; X, Y: integer; Str: PChar; Count: integer): boolean; stdcall; external 'gdi32.dll' Name 'TextOutA';
function CreateSolidBrush(p1: longword): longword; stdcall; external 'gdi32.dll';
//MMSystem, too:
function timeBeginPeriod(uPeriod: longword): longword; stdcall; external 'winMM.dll' Name 'timeBeginPeriod';
function timeEndPeriod(uPeriod: longword): longword; stdcall; external 'winMM.dll' Name 'timeEndPeriod';
function timeSetEvent(uDelay, uResolution: longword; lpFunction: pointer; dwUser: longword; uFlags: longword): longword; stdcall; external 'winMM.dll' Name 'timeSetEvent';
function timeKillEvent(uTimerID: longword): longword; stdcall; external 'winMM.dll' Name 'timeGetDevCaps';
function timeGetDevCaps(uPTC: PTimeCaps; bTime: longword): longword; stdcall; external 'winMM.dll' Name 'timeGetDevCaps';
implementation

function Rect(rLft, rTop, rRgt, rBtm: integer): TRect;
begin
  with Result do begin
    A[0] := rLft;A[1] := rTop;A[2] := rRgt;A[3] := rBtm;
end;
end;

{$A-}
end.
