program BlockThroughputProbe;
{$IFDEF FPC}{$mode delphi}{$ENDIF}
{$APPTYPE CONSOLE}
uses SysUtils, UBlockThroughputChecks;
begin
  if ParamCount <> 1 then Halt(64);
  try ExitCode := RunBlockThroughputChecks(GetEnvironmentVariable('PASCAL_BLOCK_BENCH_DATA'),
    ExpandFileName(ParamStr(1)));
  except on E: Exception do begin
    WriteLn(E.ClassName, ': ', E.Message);
    ExitCode := 1;
  end; end;
end.