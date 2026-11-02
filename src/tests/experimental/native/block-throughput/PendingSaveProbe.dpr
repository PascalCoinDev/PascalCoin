program PendingSaveProbe;
{$APPTYPE CONSOLE}
uses SysUtils, UPendingSaveChecks;
begin
  if ParamCount<>1 then Halt(64);
  try ExitCode:=RunPendingSaveChecks(ExpandFileName(ParamStr(1)));
  except on E:Exception do begin
    WriteLn(E.ClassName,': ',E.Message);
    ExitCode:=1;
  end; end;
end.
