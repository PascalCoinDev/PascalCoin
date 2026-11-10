program SafeboxRecoveryProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

uses USafeboxRecoveryChecks;

begin
  if ParamCount <> 1 then Halt(64);
  ExitCode := RunSafeboxRecoveryChecks(ParamStr(1));
end.
