program SanitizeOperationsProbe;
{$APPTYPE CONSOLE}
uses SysUtils, UMempoolSanitizeChecks;
begin
  if ParamCount <> 1 then Halt(64);
  try ExitCode := RunMempoolSanitizeChecks(ExpandFileName(ParamStr(1)));
  except on E: Exception do begin
    WriteLn(E.ClassName, ': ', E.Message);
    ExitCode := 1;
  end; end;
end.
