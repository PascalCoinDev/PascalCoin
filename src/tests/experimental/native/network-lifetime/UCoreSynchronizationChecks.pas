unit UCoreSynchronizationChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Shared native entry point: reuse the public acceptance helpers rather than
  copy algorithms or create a separate compiler graph for each core fixture. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunCoreSynchronizationChecks(const ALogFile: String): Integer;

implementation

uses Classes, SysUtils, UMempoolSanitizeChecks, UNodeSynchronizationChecks,
  UAccountCachePolicyChecks, UChangeAccountInfoReplayChecks, UDownloadLifetimeChecks,
  ULegacyConnectionLifetimeChecks, UPendingCatchupChecks;

function RunCoreSynchronizationChecks(const ALogFile: String): Integer;
var LLog: TStringList;
begin
  Result:=1;
  if FileExists(ALogFile) then raise Exception.Create('Refusing an existing core summary');
  LLog:=TStringList.Create;
  try
    try
      if RunMempoolSanitizeChecks(ALogFile+'.sanitize')<>0 then
        raise Exception.Create('Sanitizer/signature acceptance failed; see scenario logs');
      LLog.Add('PASS sanitizer/signatures: 9'); LLog.SaveToFile(ALogFile);
      CheckDefaultAccountCacheLimit;
      CheckAccountCacheLimitSurvivesMemoryReload;
      CheckAccountCacheLimitSurvivesFileReloadAndClear;
      LLog.Add('PASS account cache policy: 3'); LLog.SaveToFile(ALogFile);
      CheckChangeInfoSelfTarget;
      CheckChangeInfoDistinctTarget;
      CheckChangeInfoRepeatedAndRollback;
      CheckChangeInfoRejectsWithoutMutation;
      LLog.Add('PASS signed change-info replay: 4'); LLog.SaveToFile(ALogFile);
      if RunNodeSynchronizationChecks(ALogFile+'.node')<>0 then
        raise Exception.Create('Node status acceptance failed; see scenario logs');
      LLog.Add('PASS node status: 8'); LLog.SaveToFile(ALogFile);
      if RunDownloadLifetimeChecks(ALogFile+'.lifetime')<>0 then
        raise Exception.Create('Download/node ownership acceptance failed; see scenario logs');
      LLog.Add('PASS download/node ownership: 7'); LLog.SaveToFile(ALogFile);
      if RunLegacyConnectionLifetimeChecks(ALogFile+'.legacy')<>0 then
        raise Exception.Create('Legacy connection ownership failed; see scenario logs');
      LLog.Add('PASS legacy connection ownership: 3'); LLog.SaveToFile(ALogFile);
      if RunPendingCatchupChecks(ALogFile+'.catchup')<>0 then
        raise Exception.Create('Pending catch-up acceptance failed; see scenario logs');
      LLog.Add('PASS pending catch-up: 10'); LLog.SaveToFile(ALogFile);
      LLog.Add('PASS total: 44');
      Result:=0;
    except on E: Exception do LLog.Add('FAIL '+E.ClassName+': '+E.Message); end;
    LLog.SaveToFile(ALogFile);
  finally LLog.Free; end;
end;

end.
