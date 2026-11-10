program BufferedBlockFrameProbe;
{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }
{$APPTYPE CONSOLE}
uses System.SysUtils, UBufferedBlockFrameChecks;
{$R BufferedBlockBacklog.Assets.res}
begin
  if ParamCount <> 1 then Halt(64);
  ExitCode := RunBufferedBlockFrameChecks(ParamStr(1));
end.
