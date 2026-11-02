program OrderedCardinalProbe;
{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }
{$APPTYPE CONSOLE}
uses UOrderedCardinalChecks;
begin
  ReportMemoryLeaksOnShutdown := True;
  if ParamCount <> 1 then Halt(64);
  ExitCode := RunOrderedCardinalChecks(ParamStr(1));
end.
