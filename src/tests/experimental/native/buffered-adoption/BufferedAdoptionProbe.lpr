program BufferedAdoptionProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFDEF FPC}{$MODE Delphi}{$ELSE}{$APPTYPE CONSOLE}{$ENDIF}

uses {$IFDEF FPC}{$IFNDEF BUFFERED_ADOPTION_PERFORMANCE}heaptrc,{$ENDIF}{$ENDIF} SysUtils, UBufferedAdoptionChecks;

begin
  {$IFDEF FPC}
  {$IFNDEF BUFFERED_ADOPTION_PERFORMANCE}
  if ParamCount>0 then SetHeapTraceOutput(ParamStr(1)+'.heap.log');
  {$ENDIF}
  {$ELSE}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  if not (ParamCount in [1,2]) then Halt(64);
  try
    if ParamCount=2 then begin
      if ParamStr(2)<>'perf' then Halt(64);
      ExitCode := RunBufferedAdoptionPerformance(ExpandFileName(ParamStr(1)));
    end else
      ExitCode := RunBufferedAdoptionChecks(ExpandFileName(ParamStr(1)));
  except
    on E: Exception do begin
      WriteLn(E.ClassName+': '+E.Message);
      ExitCode := 1;
    end;
  end;
end.
