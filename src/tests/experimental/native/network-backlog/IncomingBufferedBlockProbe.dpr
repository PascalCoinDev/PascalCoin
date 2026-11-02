program IncomingBufferedBlockProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

uses Forms, UWalletBufferedBlockChecks;

{$R 'BufferedBlockBacklog.Assets.res'}

begin
  ExitCode := RunIncomingBufferedBlockProbe;
end.
