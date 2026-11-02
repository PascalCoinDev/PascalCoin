program PendingCatchupProbe;
{$mode delphi}
uses heaptrc, SysUtils, UPendingCatchupChecks;
var LCase: String; LOrdinal: Integer;
begin
  if ParamCount<>1 then Halt(64);
  SetHeapTraceOutput(ParamStr(1)+'.heap.log');
  LCase:=GetEnvironmentVariable('PASCAL_PENDING_CATCHUP_CASE');
  if LCase='' then ExitCode:=RunPendingCatchupChecks(ParamStr(1))
  else begin
    LOrdinal:=StrToInt(LCase);
    if (LOrdinal<Ord(Low(TPendingCatchupCheck))) or (LOrdinal>Ord(High(TPendingCatchupCheck))) then Halt(64);
    ExitCode:=RunPendingCatchupCheck(TPendingCatchupCheck(LOrdinal),ParamStr(1));
  end;
end.
