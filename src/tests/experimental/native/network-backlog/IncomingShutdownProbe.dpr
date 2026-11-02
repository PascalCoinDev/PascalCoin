program IncomingShutdownProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

uses Forms, UWalletBufferedBlockChecks;

{$R 'BufferedBlockBacklog.Assets.res'}

begin
  ExitCode := RunIncomingShutdownProbe;
end.
