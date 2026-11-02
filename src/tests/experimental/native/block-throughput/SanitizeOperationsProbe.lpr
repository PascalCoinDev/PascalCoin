program SanitizeOperationsProbe;
{$IFDEF FPC}{$mode delphi}{$ENDIF}
{$APPTYPE CONSOLE}
uses {$IFDEF FPC}heaptrc,{$ENDIF} SysUtils, UMempoolSanitizeChecks;
begin
  if ParamCount <> 1 then Halt(64);
  {$IFDEF FPC}SetHeapTraceOutput(ParamStr(1) + '.heap.log');{$ENDIF}
  try ExitCode := RunMempoolSanitizeChecks(ExpandFileName(ParamStr(1)));
  except on E: Exception do begin WriteLn(E.ClassName, ': ', E.Message); ExitCode := 1; end; end;
end.
