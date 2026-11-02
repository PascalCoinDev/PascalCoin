program ClassicWalletProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$MODE Delphi}

uses heaptrc, Interfaces, UClassicWalletChecks;

begin
  if ParamCount <> 1 then Halt(64);
  SetHeapTraceOutput(ParamStr(1) + '.heap.log');
  ExitCode := RunClassicWalletProbe;
end.
