program DownloadLifetimeProbe;
{$mode delphi}
uses heaptrc, SysUtils, UDownloadLifetimeChecks;
begin
  if ParamCount<>1 then Halt(64);
  SetHeapTraceOutput(ParamStr(1)+'.heap.log');
  ExitCode:=RunDownloadLifetimeChecks(ParamStr(1));
end.
