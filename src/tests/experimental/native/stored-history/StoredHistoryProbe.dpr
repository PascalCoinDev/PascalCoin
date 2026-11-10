program StoredHistoryProbe;
{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }
{$APPTYPE CONSOLE}
uses UStoredHistoryChecks;
begin
  ReportMemoryLeaksOnShutdown := True;
  if ParamCount <> 1 then Halt(64);
  ExitCode := RunStoredHistoryChecks(ParamStr(1));
end.
