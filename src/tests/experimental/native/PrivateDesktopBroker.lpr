program PrivateDesktopBroker;

{ Copyright (c) 2026 Herman Schoenfeld <https://www.sphere10.com>

  Based on the PascalFramework native wizard regression broker. The broker
  never switches the input desktop; all commands target only its own QA child.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}

uses
  Windows, SysUtils, Classes;

const
  CQAExecutionContinuous = DWORD($80000000);
  CQAExecutionSystemRequired = DWORD($00000001);

// This API is absent from the older FPC Windows declarations used by this QA
// runner. Keep its standard Win32 ABI explicit without another unit dependency.
function QASetThreadExecutionState(AFlags: DWORD): DWORD; stdcall;
  external 'kernel32.dll' name 'SetThreadExecutionState';

var
  ActualPID: DWORD;
  ActualLines: TStringList;
  ActualAbout, ActualMain: HWND;
  ActualKnownMain: HWND;
  ActualConfirmation, ActualYes: HWND;
  ActualYesLeft: Integer;
  ActualSawAbout, ActualSawMain, ActualInteractiveMain: Boolean;
  ActualCloseRequested: Boolean;

function FindActualYes(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  title: array[0..127] of Char;
  messageResult: DWORD_PTR;
  bounds: TRect;
begin
  FillChar(title, SizeOf(title), 0);
  SendMessageTimeout(AWindow, WM_GETTEXT, Length(title), LPARAM(@title[0]),
    SMTO_ABORTIFHUNG or SMTO_BLOCK, 100, @messageResult);
  GetWindowRect(AWindow, bounds);
  ActualLines.Add(Format('  child=%d caption=%s enabled=%s rect=%d,%d,%d,%d style=%x id=%d',
    [AWindow, String(title), BoolToStr(IsWindowEnabled(AWindow), true), bounds.Left, bounds.Top,
     bounds.Right, bounds.Bottom, GetWindowLongPtr(AWindow, GWL_STYLE), GetDlgCtrlID(AWindow)]));
  if (StringReplace(String(title), '&', '', [rfReplaceAll]) = 'Yes') and IsWindowEnabled(AWindow) then
    ActualYes := AWindow;
  // LCL owner-drawn BitBtn captions are not exported by WM_GETTEXT in the
  // shipping build. Its known Yes/No prompt lays out Yes before No.
  if (ActualYes = 0) or (String(title) = '') then begin
    GetWindowRect(AWindow, bounds);
    if IsWindowEnabled(AWindow) and (bounds.Left < ActualYesLeft) then begin
      ActualYes := AWindow;
      ActualYesLeft := bounds.Left;
    end;
  end;
  Result := true;
end;

function SnapshotActualWindow(AWindow: HWND; AData: LPARAM): BOOL; stdcall;
var
  pid: DWORD;
  title, kind: array[0..511] of Char;
  bounds: TRect;
begin
  Result := true;
  GetWindowThreadProcessId(AWindow, @pid);
  if (pid <> ActualPID) or not IsWindowVisible(AWindow) then Exit;
  GetWindowText(AWindow, title, Length(title));
  GetClassName(AWindow, kind, Length(kind));
  GetWindowRect(AWindow, bounds);
  ActualLines.Add(Format('HWND=%d class=%s title=%s enabled=%s owner=%d rect=%d,%d,%d,%d style=%x',
    [AWindow, String(kind), String(title), BoolToStr(IsWindowEnabled(AWindow), true),
     GetWindow(AWindow, GW_OWNER), bounds.Left, bounds.Top, bounds.Right, bounds.Bottom,
     GetWindowLongPtr(AWindow, GWL_STYLE)]));
  // The shipping build uses LCL's generic native "Window" class.
  if String(title) = 'About...' then begin
    ActualAbout := AWindow;
    ActualSawAbout := true;
  end;
  // An initialization error can reuse Application.Title. Require the actual
  // ownerless wallet before recording successful startup or requesting Close.
  if (Pos('Pascal Coin Wallet', String(title)) = 1) and
    (GetWindow(AWindow, GW_OWNER) = 0) and
    ((String(kind) = 'TFRMMainForm') or (Pos('JSON-RPC', String(title)) > 0)) then begin
    ActualMain := AWindow;
    ActualKnownMain := AWindow;
    ActualSawMain := true;
    ActualInteractiveMain := ActualInteractiveMain or IsWindowEnabled(AWindow);
  end;
  // VCL uses its standard Confirm title; LCL keeps the supplied wallet title.
  // Accept either only for our own requested quit and the known main owner.
  if ActualCloseRequested and (ActualKnownMain <> 0) and
    (GetWindow(AWindow, GW_OWNER) = ActualKnownMain) and
    ((String(title) = 'Exit PascalCoin') or
     ((String(title) = 'Confirm') and (Pos('TMessageForm', String(kind)) = 1))) then begin
    ActualConfirmation := AWindow;
    ActualYes := 0;
    ActualYesLeft := High(Integer);
    EnumChildWindows(AWindow, @FindActualYes, 0);
  end;
end;

function RunActualExecutable(const AExecutable, ALogRoot, AMode: String): DWORD;
var
  desktop: HDESK;
  startup: TStartupInfo;
  child: TProcessInformation;
  desktopName, desktopPath, command, dataDirectory, logFile, previous, snapshot, windowSnapshot: String;
  started: QWord;
  closedAbout, closedMain: Boolean;
begin
  desktopName := 'PascalCoinExecutableQA_' + IntToStr(GetCurrentProcessId);
  dataDirectory := IncludeTrailingPathDelimiter(GetTempDir) + desktopName;
  if DirectoryExists(dataDirectory) then raise Exception.Create('Refusing to reuse test data');
  logFile := IncludeTrailingPathDelimiter(ALogRoot) + AMode + '.log';
  desktop := CreateDesktop(PChar(desktopName), nil, nil, 0, GENERIC_ALL, nil);
  if desktop = 0 then RaiseLastOSError;
  ActualLines := TStringList.Create;
  try
    FillChar(startup, SizeOf(startup), 0);
    FillChar(child, SizeOf(child), 0);
    startup.cb := SizeOf(startup);
    desktopPath := 'WinSta0\' + desktopName;
    startup.lpDesktop := PChar(desktopPath);
    startup.dwFlags := STARTF_USESHOWWINDOW;
    startup.wShowWindow := SW_SHOW;
    command := '"' + AExecutable + '" "--data-dir=' + dataDirectory + '" --offline';
    if AMode = 'actual-no-dialogs' then command := command + ' --no-startup-dialogs';
    if not CreateProcess(nil, PChar(command), nil, nil, false, 0, nil, nil, @startup, @child) then
      RaiseLastOSError;
    try
      ActualPID := child.dwProcessId;
      started := GetTickCount64;
      closedAbout := false;
      closedMain := false;
      ActualSawAbout := false;
      ActualSawMain := false;
      ActualInteractiveMain := false;
      ActualKnownMain := 0;
      ActualCloseRequested := false;
      previous := '';
      repeat
        ActualAbout := 0;
        ActualMain := 0;
        ActualConfirmation := 0;
        ActualLines.Clear;
        EnumDesktopWindows(desktop, @SnapshotActualWindow, 0);
        snapshot := ActualLines.Text;
        if snapshot <> previous then begin
          windowSnapshot := snapshot;
          ActualLines.Insert(0, 'elapsed=' + IntToStr(GetTickCount64 - started));
          if FileExists(logFile) then begin
            snapshot := ActualLines.Text;
            ActualLines.LoadFromFile(logFile);
            ActualLines.Add(snapshot);
          end;
          ActualLines.SaveToFile(logFile);
          previous := windowSnapshot;
        end;
        if (not closedAbout) and (GetTickCount64 - started > 3000) and (ActualAbout <> 0) then begin
          PostMessage(ActualAbout, WM_CLOSE, 0, 0);
          closedAbout := true;
        end;
        if (not closedMain) and (GetTickCount64 - started > 6000) and (ActualMain <> 0) then begin
          ActualCloseRequested := true;
          PostMessage(ActualMain, WM_CLOSE, 0, 0);
          closedMain := true;
        end;
        if ActualConfirmation <> 0 then begin
          if ActualYes <> 0 then
            PostMessage(ActualConfirmation, WM_COMMAND,
              (BN_CLICKED shl 16) or (GetDlgCtrlID(ActualYes) and $FFFF), LPARAM(ActualYes));
        end;
        if WaitForSingleObject(child.hProcess, 150) = WAIT_OBJECT_0 then break;
      until GetTickCount64 - started > 10000;
      if WaitForSingleObject(child.hProcess, 0) <> WAIT_OBJECT_0 then begin
        TerminateProcess(child.hProcess, 124);
        WaitForSingleObject(child.hProcess, 5000);
      end;
      if not GetExitCodeProcess(child.hProcess, Result) then RaiseLastOSError;
      if Result = 0 then begin
        if ActualSawAbout or (not ActualSawMain) or (not ActualInteractiveMain) then
          Result := 1;
      end;
      WriteLn('Actual EXE test PID ', child.dwProcessId, ', private desktop ', desktopName,
        ', exit ', Result, ', snapshot ', logFile);
    finally
      CloseHandle(child.hThread);
      CloseHandle(child.hProcess);
    end;
  finally
    ActualLines.Free;
    CloseDesktop(desktop);
  end;
end;

function RunProbe(const AExecutable, ALogRoot, AMode : String) : DWORD;
var
  LDesktop : HDESK;
  LStartup : TStartupInfo;
  LChild : TProcessInformation;
  LDesktopName, LDesktopPath, LCommand, LLogPath, LDataDirectory : String;
  LWaitResult : DWORD;
begin
  Result := 1;
  LDesktopName := 'PascalCoinWalletQA_' + IntToStr(GetCurrentProcessId) + '_' + AMode;
  LDesktopPath := 'WinSta0\' + LDesktopName;
  if AMode = 'units' then
    LLogPath := ALogRoot
  else
    LLogPath := IncludeTrailingPathDelimiter(ALogRoot) + AMode + '.log';
  // Every child receives a new, disposable data directory under the OS temp
  // directory. No configured user wallet path is consulted or passed through.
  LDataDirectory := IncludeTrailingPathDelimiter(GetTempDir) + LDesktopName;
  if (AMode <> 'units') and (DirectoryExists(LDataDirectory) or FileExists(LDataDirectory)) then
    raise Exception.Create('Refusing to reuse a QA data directory: ' + LDataDirectory);
  LDesktop := CreateDesktop(PChar(LDesktopName), nil, nil, 0, GENERIC_ALL, nil);
  if LDesktop = 0 then RaiseLastOSError;
  try
    FillChar(LStartup, SizeOf(LStartup), 0);
    FillChar(LChild, SizeOf(LChild), 0);
    LStartup.cb := SizeOf(LStartup);
    LStartup.lpDesktop := PChar(LDesktopPath);
    LStartup.dwFlags := STARTF_USESHOWWINDOW;
    LStartup.wShowWindow := SW_SHOW;
    if AMode = 'units' then
      LCommand := '"' + AExecutable + '" "' + LLogPath + '"'
    else
      LCommand := '"' + AExecutable + '" --probe-mode=' + AMode +
        ' "--probe-log=' + LLogPath + '" "--data-dir=' + LDataDirectory +
        '" --offline --no-startup-dialogs';
    if not CreateProcess(nil, PChar(LCommand), nil, nil, false, 0, nil, nil, @LStartup, @LChild) then
      RaiseLastOSError;
    try
      WriteLn('Started own QA child PID ', LChild.dwProcessId, ' on ', LDesktopPath,
        '; input desktop unchanged; data: ', LDataDirectory);
      // The complete suite includes real pending-save waits, large collections
      // and heap tracing; measured runs can exceed ten minutes. Individual test
      // deadlines and the short native UI probe budget remain independent.
      if AMode = 'units' then
        LWaitResult := WaitForSingleObject(LChild.hProcess, 1800000)
      else
        LWaitResult := WaitForSingleObject(LChild.hProcess, 30000);
      if LWaitResult = WAIT_TIMEOUT then begin
        TerminateProcess(LChild.hProcess, 124);
        WaitForSingleObject(LChild.hProcess, 5000);
        WriteLn('Watchdog terminated only QA child PID ', LChild.dwProcessId);
        Result := 124;
      end else begin
        if LWaitResult <> WAIT_OBJECT_0 then RaiseLastOSError;
        if not GetExitCodeProcess(LChild.hProcess, Result) then RaiseLastOSError;
        WriteLn('QA child exited ', Result, '; log: ', LLogPath);
      end;
    finally
      CloseHandle(LChild.hThread);
      CloseHandle(LChild.hProcess);
    end;
  finally
    CloseDesktop(LDesktop);
  end;
end;

var
  LExitCode : DWORD;
  LPreviousExecutionState: DWORD;
begin
  if (ParamCount <> 2) and (ParamCount <> 3) then Halt(64);
  if (ParamCount = 3) and (ParamStr(3) <> '--wallet-startup') and
    (ParamStr(3) <> '--actual-first-run') and (ParamStr(3) <> '--actual-no-dialogs') then Halt(64);
  LExitCode := 1;
  // Keep only this owned QA run awake: long real-time assertions are invalid
  // across system sleep. This thread-scoped request never changes a power plan
  // or keeps the display on, and is restored before the broker exits.
  LPreviousExecutionState := QASetThreadExecutionState(CQAExecutionContinuous or CQAExecutionSystemRequired);
  if LPreviousExecutionState=0 then RaiseLastOSError;
  try
    try
    if ParamCount = 2 then
      LExitCode := RunProbe(ExpandFileName(ParamStr(1)), ExpandFileName(ParamStr(2)), 'units')
    else if (ParamStr(3) = '--actual-first-run') or (ParamStr(3) = '--actual-no-dialogs') then begin
      LExitCode := RunActualExecutable(ExpandFileName(ParamStr(1)), ExpandFileName(ParamStr(2)), Copy(ParamStr(3), 3, MaxInt));
    end else begin
      if not DirectoryExists(ParamStr(2)) then
        raise Exception.Create('The QA log directory must already exist.');
      LExitCode := RunProbe(ExpandFileName(ParamStr(1)), ExpandFileName(ParamStr(2)), 'loaded');
      if LExitCode = 0 then
        LExitCode := RunProbe(ExpandFileName(ParamStr(1)), ExpandFileName(ParamStr(2)), 'close-loading');
      if LExitCode = 0 then
        LExitCode := RunProbe(ExpandFileName(ParamStr(1)), ExpandFileName(ParamStr(2)), 'close-before-load');
      if LExitCode = 0 then
        LExitCode := RunProbe(ExpandFileName(ParamStr(1)), ExpandFileName(ParamStr(2)), 'slow-close');
    end;
  except
    on E : Exception do begin
      WriteLn('Private desktop QA failed: ', E.Message);
      LExitCode := 1;
    end;
  end;
  finally
    QASetThreadExecutionState(LPreviousExecutionState or CQAExecutionContinuous);
  end;
  Halt(LExitCode);
end.
