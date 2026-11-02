program LargeBufferedBlockBacklogProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFNDEF LARGE_BACKLOG_PROBE}
{$MESSAGE ERROR 'Build the dedicated project with LARGE_BACKLOG_PROBE defined.'}
{$ENDIF}

uses Forms, UWalletBufferedBlockChecks;

{$R 'LargeBufferedBlockBacklog.Assets.res'}

begin
  ExitCode := RunLargeBufferedBlockProbe;
end.
