program MempoolLockProbe;
{$IFDEF FPC}{$mode delphi}{$ENDIF}
{$APPTYPE CONSOLE}
uses {$IFDEF FPC}heaptrc,{$ENDIF} SysUtils, UMempoolLockChecks;
begin
  if ParamCount <> 1 then Halt(64);
  {$IFDEF FPC}SetHeapTraceOutput(ParamStr(1)+'.heap.log');{$ENDIF}
  try ExitCode := RunMempoolLockChecks(ExpandFileName(ParamStr(1)));
  except on E: Exception do begin
    WriteLn(E.ClassName, ': ', E.Message);
    ExitCode := 1;
  end; end;
end.
