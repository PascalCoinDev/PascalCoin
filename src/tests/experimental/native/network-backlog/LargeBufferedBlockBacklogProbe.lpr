program LargeBufferedBlockBacklogProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$mode delphi}
{$IFNDEF LARGE_BACKLOG_PROBE}
{$FATAL Build with the global -dLARGE_BACKLOG_PROBE compiler option.}
{$ENDIF}

// Match shipping allocation behavior for the dense public dataset. The small
// backlog scenarios and full wallet suite retain their independent heap checks.
uses Interfaces, UWalletBufferedBlockChecks;

{$R LargeBufferedBlockBacklog.Assets.rc}

begin
  if ParamCount <> 1 then Halt(64);
  ExitCode := RunLargeBufferedBlockProbe;
end.
