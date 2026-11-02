program ClassicWalletProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

uses UClassicWalletChecks;

begin
  ReportMemoryLeaksOnShutdown := True;
  ExitCode := RunClassicWalletProbe;
end.
