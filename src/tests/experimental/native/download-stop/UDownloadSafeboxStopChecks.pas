unit UDownloadSafeboxStopChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunDownloadSafeboxStopProbe: Integer;

implementation

uses
  Windows, Classes, SysUtils, SyncObjs,
  {$IFDEF FPC}Generics.Collections,{$ELSE}System.Generics.Collections,{$ENDIF}
  UMemory, UThread, UNode, UNetProtocol, UChunk, UConst, UAccounts, UCrypto,
  UPCDownloadSafebox, UPCDataTypes;

type
  TDownloadStopState = class
  private
    FLogFile: String;
    FLog: TStringList;
    FLogLock: SyncObjs.TCriticalSection;
  public
    Gate: TEvent;
    Threads: TList<TPCThread>;
    StartedCount, EnteredCount, CompletedCount, DestroyedCount: Integer;
    PreparedWritesCount, CompletedWritesCount, WritesAfterCancellationCount: Integer;
    StreamsAliveAtExitCount, StreamsAliveBeforeDestroyCount, DeletedStreamCount: Integer;
    JoinCount, CompletedJoinsCount, FreedAfterAllJoinsCount, WorkerErrors: Integer;
    AllTerminatedBeforeFirstJoin: Boolean;
    constructor Create(const ALogFile: String);
    destructor Destroy; override;
    procedure Log(const AMessage: String);
    procedure StopNetwork(Sender: TObject; const AProgress: TProgressInfo);
  end;

  TDownloadAccess = class(TPCDownloadSafebox)
  public type
    TChunkThread = {$IFDEF FPC}TDownloadSBThread{$ELSE}TPCDownloadSafebox.TDownloadSBThread{$ENDIF};
  end;

  TObservedDownload = class(TDownloadAccess)
  protected type
    TBlockedChunk = class(TDownloadAccess.TChunkThread)
    private
      FState: TDownloadStopState;
      FWriteCompleted, FWorkerCompleted: Boolean;
    protected
      procedure BCExecute; override;
    public
      constructor Create(AOwner: TObservedDownload; AState: TDownloadStopState;
        ABlockStart, ACount: Cardinal); reintroduce;
      destructor Destroy; override;
    end;
  private
    FState: TDownloadStopState;
  protected
    function CreateChunkThread(ABlockStart, ACount: Cardinal): TDownloadAccess.TChunkThread; override;
    procedure WaitForChunkThread(AThread: TThread); override;
  public
    constructor Create(AState: TDownloadStopState); reintroduce;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function ReadFileBytes(const AFileName: String): AnsiString;
var LBag: TDisposables; LStream: TFileStream;
begin
  LStream := LBag.AddObject(TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone)) as TFileStream;
  SetLength(Result, LStream.Size);
  if Length(Result) > 0 then LStream.ReadBuffer(Result[1], Length(Result));
end;

procedure WriteFileBytes(const AFileName: String; const ABytes: AnsiString);
var LBag: TDisposables; LStream: TFileStream;
begin
  LStream := LBag.AddObject(TFileStream.Create(AFileName, fmCreate)) as TFileStream;
  if Length(ABytes) > 0 then LStream.WriteBuffer(ABytes[1], Length(ABytes));
  Require(FlushFileBuffers(LStream.Handle), 'Cannot flush the isolated saved-file sentinel');
end;

function DesktopName(AHandle: THandle): String;
var LBuffer: array[0..255] of Char; LRequired: DWORD;
begin
  FillChar(LBuffer, SizeOf(LBuffer), 0);
  if not GetUserObjectInformation(AHandle, UOI_NAME, @LBuffer[0], SizeOf(LBuffer), LRequired) then
    RaiseLastOSError;
  Result := StrPas(PChar(@LBuffer[0]));
end;

procedure RequirePrivateDesktop;
var LInput: THandle; LName: String;
begin
  LInput := OpenInputDesktop(0, false, DESKTOP_READOBJECTS);
  if LInput = 0 then RaiseLastOSError;
  try
    LName := DesktopName(GetThreadDesktop(GetCurrentThreadId));
    Require((Pos('PascalCoin', LName) = 1) and not SameText(LName, DesktopName(LInput)),
      'Run this probe through the existing private-desktop broker');
  finally CloseDesktop(LInput); end;
end;

constructor TDownloadStopState.Create(const ALogFile: String);
begin
  inherited Create;
  FLogFile := ALogFile;
  FLog := TStringList.Create;
  FLogLock := SyncObjs.TCriticalSection.Create;
  Gate := TEvent.Create(nil, true, false, '');
  Threads := TList<TPCThread>.Create;
end;

destructor TDownloadStopState.Destroy;
begin
  Threads.Free;
  Gate.Free;
  FLogLock.Free;
  FLog.Free;
  inherited;
end;

procedure TDownloadStopState.Log(const AMessage: String);
begin
  FLogLock.Acquire;
  try
    FLog.Add(AMessage);
    FLog.SaveToFile(FLogFile);
  finally FLogLock.Release; end;
end;

procedure TDownloadStopState.StopNetwork(Sender: TObject; const AProgress: TProgressInfo);
var LDeadline: UInt64;
begin
  Require(AProgress.Stage = psDownloadingSafeboxChunks,
    'The actual downloader must identify its stage without presentation text');
  Require((AProgress.CurrentPosition = 0) and (AProgress.TotalCount = 2),
    'Both gated chunks must remain pending in the raw progress counters');
  Require(Threads.Count = 2, 'The public downloader must create both real chunk workers');
  LDeadline := GetTickCount64 + 2000;
  while (StartedCount < Threads.Count) and (GetTickCount64 < LDeadline) do Sleep(1);
  Require(StartedCount = Threads.Count, 'Both inherited workers must reach the test gate');
  Require(PreparedWritesCount = Threads.Count, 'Both actual temporary streams must have an in-flight write');
  Require(CompletedWritesCount = 0, 'The gated writes must still be unfinished when cancellation starts');
  Require(EnteredCount = 0, 'No worker may enter its production retry before the join');
  Require(TNetData.NetData.ConnectionsCountAll = 0, 'This regression requires no peer connection');
  Log('NETWORK OFF: both real chunk workers gated before inherited retry');
  TNetData.NetData.NetConnectionsActive := false;
end;

constructor TObservedDownload.TBlockedChunk.Create(AOwner: TObservedDownload;
  AState: TDownloadStopState; ABlockStart, ACount: Cardinal);
begin
  FState := AState;
  inherited Create(AOwner, ABlockStart, ACount);
end;

procedure TObservedDownload.TBlockedChunk.BCExecute;
const
  CT_PREFIX: AnsiString = 'IN-FLIGHT CHUNK WRITE'#10;
  CT_COMPLETED: AnsiString = 'NORMAL WRITE COMPLETION AFTER CANCELLATION'#10;
var LExpected: AnsiString;
begin
  try
    // This is a test write to the real owned stream, not a valid protocol chunk.
    Stream.WriteBuffer(CT_PREFIX[1], Length(CT_PREFIX));
    InterlockedIncrement(FState.PreparedWritesCount);
    InterlockedIncrement(FState.StartedCount);
    FState.Gate.WaitFor(INFINITE);
    Require(Terminated, 'The join must request cooperative cancellation before releasing the write');
    Stream.WriteBuffer(CT_COMPLETED[1], Length(CT_COMPLETED));
    Require(FlushFileBuffers(Stream.Handle), 'The actual temporary stream must remain open through write completion');
    LExpected := CT_PREFIX + CT_COMPLETED;
    Require(ReadFileBytes(Stream.FileName) = LExpected, 'The complete flushed write must be readable before the worker exits');
    FWriteCompleted := true;
    InterlockedIncrement(FState.CompletedWritesCount);
    InterlockedIncrement(FState.WritesAfterCancellationCount);
    FState.Log('WRITE COMPLETE after cooperative cancellation; actual temporary file still open');
    InterlockedIncrement(FState.EnteredCount);
    FState.Log('WORKER ENTER inherited retry (which acquires the owner chunks lock)');
    // Keep the real retry and list acquisition; it may clear a cancelled chunk.
    inherited;
    Require(FileExists(Stream.FileName), 'The worker must retain its temporary file until normal exit');
    Require(FlushFileBuffers(Stream.Handle), 'The worker must retain its live stream handle until normal exit');
    FWorkerCompleted := true;
    InterlockedIncrement(FState.StreamsAliveAtExitCount);
    InterlockedIncrement(FState.CompletedCount);
    FState.Log('WORKER EXIT inherited retry; owned stream still alive');
  except
    on E: Exception do begin
      InterlockedIncrement(FState.WorkerErrors);
      FState.Log('WORKER FAIL ' + E.ClassName + ': ' + E.Message);
    end;
  end;
end;

destructor TObservedDownload.TBlockedChunk.Destroy;
var LFileName: String;
begin
  LFileName := Stream.FileName;
  if FWriteCompleted and FWorkerCompleted and FileExists(LFileName) and
    FlushFileBuffers(Stream.Handle) then InterlockedIncrement(FState.StreamsAliveBeforeDestroyCount);
  if FState.CompletedJoinsCount = FState.Threads.Count then
    InterlockedIncrement(FState.FreedAfterAllJoinsCount);
  inherited;
  if not FileExists(LFileName) then InterlockedIncrement(FState.DeletedStreamCount);
  InterlockedIncrement(FState.DestroyedCount);
end;

constructor TObservedDownload.Create(AState: TDownloadStopState);
begin
  FState := AState;
  inherited Create;
end;

function TObservedDownload.CreateChunkThread(ABlockStart,
  ACount: Cardinal): TDownloadAccess.TChunkThread;
begin
  Result := TBlockedChunk.Create(Self, FState, ABlockStart, ACount);
  FState.Threads.Add(Result);
end;

procedure TObservedDownload.WaitForChunkThread(AThread: TThread);
var LThread: TPCThread;
begin
  Inc(FState.JoinCount);
  if FState.JoinCount = 1 then begin
    FState.AllTerminatedBeforeFirstJoin := true;
    for LThread in FState.Threads do
      FState.AllTerminatedBeforeFirstJoin :=
        FState.AllTerminatedBeforeFirstJoin and LThread.Terminated;
    FState.Log('FIRST JOIN: all workers terminated=' + BoolToStr(FState.AllTerminatedBeforeFirstJoin, true));
    // Releasing only at the real join deterministically exposes lock-held joins.
    FState.Gate.SetEvent;
  end;
  inherited;
  Inc(FState.CompletedJoinsCount);
  FState.Log('JOIN COMPLETE ' + IntToStr(FState.JoinCount));
end;

procedure CheckTemporaryStreamsReleased(const ADirectory: String);
var LSearch: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(ADirectory) + 'Temp' + PathDelim + '*.tmp',
    faAnyFile, LSearch) = 0 then begin
    FindClose(LSearch);
    raise Exception.Create('A downloader-owned temporary chunk stream was not released');
  end;
end;

procedure CheckDownloaderStop(AState: TDownloadStopState; const ADirectory: String);
const CT_SAVED_BYTES: AnsiString = 'PREEXISTING SAVED SAFEBOX SENTINEL - MUST REMAIN UNCHANGED'#10;
var LDownload: TObservedDownload; LChunks: TPCSafeboxChunks;
  LOperationBlock: TOperationBlock; LStarted, LElapsed: UInt64; LResult: Boolean;
  LSavedFile, LUnpublishedFile: String;
begin
  LChunks := TPCSafeboxChunks.Create;
  try
    LOperationBlock := CT_OperationBlock_NUL;
    LOperationBlock.block := 2 * CT_MAX_SAFEBOXCHUNK_BLOCKS;
    Require(LOperationBlock.block mod CT_BankToDiskEveryNBlocks = 0,
      'The fixture block count must satisfy the real downloader validation');
    LSavedFile := IncludeTrailingPathDelimiter(TNode.Node.Bank.GetStorageFolder('')) + 'safebox_100.safebox';
    LUnpublishedFile := IncludeTrailingPathDelimiter(TNode.Node.Bank.GetStorageFolder('')) +
      'safebox_' + UIntToStr(LOperationBlock.block) + '.safebox';
    Require(not FileExists(LSavedFile) and not FileExists(LUnpublishedFile), 'Both archive paths must be fresh test paths');
    WriteFileBytes(LSavedFile, CT_SAVED_BYTES);
    TNode.Node.NetServer.Port := 0;
    TNode.Node.NetServer.Active := true;
    Require(TNode.Node.NetServer.Active, 'The isolated ephemeral server must start');
    TNode.Node.BeginBlockChainUpdate;
    try
      LDownload := TObservedDownload.Create(AState);
      try
        LDownload.OnProgressNotify := AState.StopNetwork;
        LResult := LDownload.DownloadSafebox(nil, LOperationBlock, LChunks);
        Require(not LResult, 'Network-off must cancel the unfinished download');
        Require(LChunks.Count = 0, 'Cancelled temporary chunks must never be transferred for publication');
        Require(not TNode.Node.NetServer.Active, 'The actual network-off path must stop the server');
      finally
        LStarted := GetTickCount64;
        LDownload.Free;
        LElapsed := GetTickCount64 - LStarted;
        AState.Log('DESTRUCTOR RETURN elapsed=' + UIntToStr(LElapsed));
      end;
    finally TNode.Node.EndBlockChainUpdate; end;
    Require(LElapsed < 3000, 'Joining cancelled no-peer workers must finish promptly');
    Require(not TNode.Node.IsUpdatingBlockChain, 'Downloader shutdown must release its owner core-work scope');
    Require(AState.AllTerminatedBeforeFirstJoin, 'Every worker must be terminated before the first join');
    Require(AState.JoinCount = 2, 'Both actual worker joins must complete');
    Require(AState.EnteredCount = 2, 'Both workers must enter the inherited retry');
    Require(AState.CompletedCount = 2, 'Both inherited production retries must return');
    Require(AState.DestroyedCount = 2, 'Both downloader-owned workers must be destroyed');
    Require(AState.WorkerErrors = 0, 'Both actual worker writes and inherited retries must succeed normally');
    Require(AState.CompletedWritesCount = 2, 'Both in-flight writes must complete before worker cleanup');
    Require(AState.WritesAfterCancellationCount = 2, 'Cancellation must be a cooperative flag, allowing the active writes to finish');
    Require(AState.StreamsAliveAtExitCount = 2, 'Both streams must remain open until normal worker exit');
    Require(AState.StreamsAliveBeforeDestroyCount = 2, 'Both completed workers must still own their streams before destruction');
    Require(AState.FreedAfterAllJoinsCount = 2, 'No worker stream may be freed until every worker has joined');
    Require(AState.DeletedStreamCount = 2, 'Owned partial stream files must be removed during worker destruction');
    Require(TNetData.NetData.ConnectionsCountAll = 0, 'No peer connection may be introduced');
    Require(ReadFileBytes(LSavedFile) = CT_SAVED_BYTES, 'Cancellation must leave the preexisting saved file byte-for-byte unchanged');
    Require(not FileExists(LUnpublishedFile), 'Cancellation must not publish a final safebox archive');
    CheckTemporaryStreamsReleased(ADirectory);
    AState.Log('PASS two inherited chunk workers acquired their owner list during shutdown; all joins, streams and core-work scope released');
    AState.Log('PASS active temporary writes completed and flushed before normal worker exit/destruction; partial files removed only after joins');
    AState.Log('PASS existing saved-file sentinel bytes unchanged; no cancelled safebox archive published (not a protocol durability test)');
  finally LChunks.Free; end;
end;

function RunDownloadSafeboxStopProbe: Integer;
var LBag: TDisposables; LState: TDownloadStopState;
  LDirectory: String; LID: TGUID; LEmpty: TPCDownloadSafebox;
  LServers: TNodeServerAddressArray;
begin
  Result := 1;
  if ParamCount <> 1 then Exit;
  LState := LBag.AddObject(TDownloadStopState.Create(ExpandFileName(ParamStr(1)))) as TDownloadStopState;
  try
    RequirePrivateDesktop;
    Require(not TNode.NodeExists and not TNetData.NetDataExists, 'The regression requires a dedicated fresh process');
    CreateGUID(LID);
    LDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'PascalCoinDownloadStop-' + GUIDToString(LID);
    Require(not DirectoryExists(LDirectory), 'The test data directory must be new');
    Require(ForceDirectories(LDirectory), 'Cannot create isolated test data');
    TNode.SetPascalCoinDataFolder(LDirectory);
    TCrypto.InitCrypto;
    TNode.Node;
    // Background discovery must stay on loopback even if this check runs slowly.
    TNode.DecodeIpStringToNodeServerAddressArray('127.0.0.1:1', LServers);
    TNetData.NetData.DiscoverFixedServersOnly(LServers);
    Require(TNetData.NetData.ConnectionsCountAll = 0, 'The fresh node must have no peers');
    LState.Log('START isolated actual downloader shutdown; data=' + LDirectory);
    LEmpty := TPCDownloadSafebox.Create;
    LEmpty.Free;
    LState.Log('PASS empty downloader destruction');
    CheckDownloaderStop(LState, LDirectory);
    Result := 0;
  except
    on E: Exception do LState.Log('FAIL ' + E.ClassName + ': ' + E.Message);
  end;
end;

end.
