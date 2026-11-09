program PendingCatchupProbe;
{$APPTYPE CONSOLE}
uses SysUtils, UPendingCatchupChecks;
var LCase: String; LOrdinal: Integer;
begin
  ReportMemoryLeaksOnShutdown:=True;
  if ParamCount<>1 then Halt(64);
  try
    LCase:=GetEnvironmentVariable('PASCAL_PENDING_CATCHUP_CASE');
    if LCase='' then ExitCode:=RunPendingCatchupChecks(ParamStr(1))
    else begin
      LOrdinal:=StrToInt(LCase);
      if (LOrdinal<Ord(Low(TPendingCatchupCheck))) or (LOrdinal>Ord(High(TPendingCatchupCheck))) then Halt(64);
      ExitCode:=RunPendingCatchupCheck(TPendingCatchupCheck(LOrdinal),ParamStr(1));
    end;
  except on E:Exception do begin
    WriteLn(E.ClassName,': ',E.Message);
    ExitCode:=1;
  end; end;
end.
