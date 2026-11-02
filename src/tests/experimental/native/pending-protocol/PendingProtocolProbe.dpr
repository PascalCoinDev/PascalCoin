program PendingProtocolProbe;
{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }
{$APPTYPE CONSOLE}
uses System.SysUtils, UPendingProtocolChecks;
begin
  if ParamCount<>1 then Halt(64);
  ExitCode:=RunPendingProtocolChecks(ParamStr(1));
end.
