program IncomingShutdownProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$mode delphi}

uses heaptrc, Interfaces, UWalletBufferedBlockChecks;

{$R BufferedBlockBacklog.Assets.rc}

begin
  if ParamCount <> 1 then Halt(64);
  SetHeapTraceOutput(ParamStr(1)+'.heap.log');
  ExitCode := RunIncomingShutdownProbe;
end.
