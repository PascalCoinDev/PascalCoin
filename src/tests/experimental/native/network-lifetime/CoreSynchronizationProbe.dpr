program CoreSynchronizationProbe;
{$APPTYPE CONSOLE}
uses SysUtils, UCoreSynchronizationChecks;
begin
  ReportMemoryLeaksOnShutdown:=True;
  if ParamCount<>1 then Halt(64);
  ExitCode:=RunCoreSynchronizationChecks(ParamStr(1));
end.
