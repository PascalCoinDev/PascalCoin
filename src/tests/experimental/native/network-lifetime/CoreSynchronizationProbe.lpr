program CoreSynchronizationProbe;
{$mode delphi}
uses heaptrc, SysUtils, UCoreSynchronizationChecks;
begin
  if ParamCount<>1 then Halt(64);
  SetHeapTraceOutput(ParamStr(1)+'.heap.log');
  ExitCode:=RunCoreSynchronizationChecks(ParamStr(1));
end.
