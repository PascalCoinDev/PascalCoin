program LogGuiHandoffProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFDEF FPC}{$MODE Delphi}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}

uses {$IFDEF FPC}heaptrc,{$ENDIF} SysUtils, ULogGuiHandoffChecks;

begin
  {$IFDEF FPC}
  if ParamCount>0 then
    SetHeapTraceOutput(ParamStr(1)+'.heap.log');
  {$ELSE}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  if not (ParamCount in [1,2]) then
    Halt(64);
  if (ParamCount=2) and (ParamStr(2)<>'producer') then
    Halt(64);
  try
    ExitCode := RunLogGuiHandoffChecks(ExpandFileName(ParamStr(1)),ParamCount=2);
  except
    on E: Exception do begin
      WriteLn(E.ClassName+': '+E.Message);
      ExitCode := 1;
    end;
  end;
end.
