program ClassicLogMemoBatchProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFDEF FPC}{$MODE Delphi}{$ELSE}{$APPTYPE GUI}{$ENDIF}

uses {$IFDEF FPC}Interfaces,{$ENDIF} SysUtils, UClassicLogMemoBatchChecks;

begin
  {$IFNDEF FPC}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  if ParamCount<>1 then
    Halt(64);
  ExitCode := RunClassicLogMemoBatchChecks(ExpandFileName(ParamStr(1)));
end.
