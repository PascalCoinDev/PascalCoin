program CheckpointStorageProbe;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$APPTYPE CONSOLE}

uses SysUtils, UNode, UCheckpointStorageChecks;

var LCheck: Integer; LID: TGUID; LDirectory: String;

begin
  ReportMemoryLeaksOnShutdown := True;
  if not (ParamCount in [1,2]) then Halt(64);
  try
    if CreateGUID(LID)<>0 then raise Exception.Create('Cannot create owned node directory identity');
    LDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(ParamStr(1))))+'node-'+GUIDToString(LID);
    if DirectoryExists(LDirectory) or not ForceDirectories(LDirectory) then raise Exception.Create('Cannot create fresh owned node directory');
    TNode.SetPascalCoinDataFolder(LDirectory);
    try
      if ParamCount=2 then begin
        if not TryStrToInt(ParamStr(2),LCheck) or (LCheck<0) or (LCheck>Ord(High(TCheckpointStorageCheck))) then Halt(64);
        ExitCode := RunCheckpointStorageCheck(TCheckpointStorageCheck(LCheck),ExpandFileName(ParamStr(1)));
      end else ExitCode := RunCheckpointStorageChecks(ExpandFileName(ParamStr(1)));
    finally
      if TNode.NodeExists then TNode.Node.Free;
    end;
  except
    on E: Exception do begin
      WriteLn(E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
