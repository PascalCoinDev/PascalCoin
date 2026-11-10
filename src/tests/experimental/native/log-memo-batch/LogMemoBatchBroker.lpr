program LogMemoBatchBroker;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Runs only the requested owned probe on a separate, undisplayed desktop.
  It waits for natural exit and never terminates a thread or process. }

{$MODE Delphi}

uses Windows, SysUtils;

function QASetThreadExecutionState(AFlags: DWORD): DWORD; stdcall; external 'kernel32.dll' name 'SetThreadExecutionState';

var
  LDesktop: HDESK;
  LStartup: TStartupInfo;
  LChild: TProcessInformation;
  LName, LDesktopPath, LExe, LLog, LCommand: String;
  LExit, LPreviousExecutionState: DWORD;
begin
  if ParamCount<>2 then
    Halt(64);
  LExe := ExpandFileName(ParamStr(1));
  LLog := ExpandFileName(ParamStr(2));
  if not SameText(ExtractFilePath(LExe),ExtractFilePath(LLog)) or FileExists(LLog) then
    Halt(65);
  LPreviousExecutionState := QASetThreadExecutionState($80000001);
  if LPreviousExecutionState=0 then
    RaiseLastOSError;
  try
    LName := 'LogMemoBatchQA_'+IntToStr(GetCurrentProcessID);
    LDesktop := CreateDesktop(PChar(LName),nil,nil,0,GENERIC_ALL,nil);
    if LDesktop=0 then
      RaiseLastOSError;
    try
      FillChar(LStartup,SizeOf(LStartup),0);
      FillChar(LChild,SizeOf(LChild),0);
      LStartup.cb := SizeOf(LStartup);
      LDesktopPath := 'WinSta0\'+LName;
      LStartup.lpDesktop := PChar(LDesktopPath);
      LStartup.dwFlags := STARTF_USESHOWWINDOW;
      LStartup.wShowWindow := SW_HIDE;
      LCommand := '"'+LExe+'" "'+LLog+'"';
      if not CreateProcess(nil,PChar(LCommand),nil,nil,False,0,nil,nil,@LStartup,@LChild) then
        RaiseLastOSError;
      try
        WriteLn('OWNED PROBE PID=',LChild.dwProcessId,' desktop=',LDesktopPath,' inputDesktopUnchanged=True');
        Flush(Output);
        if WaitForSingleObject(LChild.hProcess,INFINITE)<>WAIT_OBJECT_0 then
          RaiseLastOSError;
        if not GetExitCodeProcess(LChild.hProcess,LExit) then
          RaiseLastOSError;
        WriteLn('NATURAL EXIT code=',LExit);
      finally
        CloseHandle(LChild.hThread);
        CloseHandle(LChild.hProcess);
      end;
    finally
      CloseDesktop(LDesktop);
    end;
  finally
    QASetThreadExecutionState(LPreviousExecutionState or $80000000);
  end;
  Halt(LExit);
end.
