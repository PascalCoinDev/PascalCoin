program DownloadLifetimeProbe;
{$APPTYPE CONSOLE}
uses SysUtils, UDownloadLifetimeChecks;
begin
  ReportMemoryLeaksOnShutdown:=True;
  if ParamCount<>1 then Halt(64);
  ExitCode:=RunDownloadLifetimeChecks(ParamStr(1));
end.
