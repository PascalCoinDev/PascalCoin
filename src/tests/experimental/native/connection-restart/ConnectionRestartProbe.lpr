program ConnectionRestartProbe;
{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }
{$mode delphi}
uses heaptrc, SysUtils, UConnectionRestartChecks;
begin
  if ParamCount<>1 then Halt(64);
  SetHeapTraceOutput(ParamStr(1)+'.heap.log');
  ExitCode := RunConnectionRestartChecks(ParamStr(1));
end.
