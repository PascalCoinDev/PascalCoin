unit UNodeSynchronizationTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Deterministic checks of the actual node synchronization facts and notifier.
  Generated empty-node data and owned peer identities only; no sockets or keys
  from an existing wallet are used. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses Classes, SysUtils, fpcunit, testregistry, UNode, UAccounts, UBlockChain,
  UCrypto, UNetProtocol, UFileStorage;

type
  TNodeSynchronizationTests = class(TTestCase)
  private
    FNode: TNode;
    FNotify: TNodeNotifyEvents;
    FWatch: TOrderedAccountKeysList;
    FPeer, FSecondPeer: TObject;
    FKey: TECPrivateKey;
    FOperations: TOperationsHashTree;
    FTrace: TStringList;
    FLast: TNodeSynchronizationStatus;
    FCallbacks: Integer;
    FCallbackThread: UInt64;
    FStopping: Boolean;
    FExpectOperationCallback: Boolean;
    FThrowNextBlock: Boolean;
    FThrownBlockCallbacks: Integer;
    FPreviousDirectory: String;
    procedure OnSynchronization(Sender: TObject; const AStatus: TNodeSynchronizationStatus);
    procedure OnBlocks(Sender: TObject);
    procedure OnOperations(Sender: TObject);
    procedure OnKeys(Sender: TObject);
    procedure OnMessage(Sender: TNetConnection; AMessage: String);
    procedure PumpFor(AMilliseconds: Cardinal);
    procedure WaitForState(AState: TNodeSynchronizationState);
    procedure StartSynchronizing;
    procedure QueueBackendCallbacks;
    procedure WaitForCombinedCallbacks;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SelectedTargetSurvivesInterBatchSilence;
    procedure NetworkOffWaitsForBufferedWork;
    procedure DisconnectClearsTargetsBeforeReconnect;
    procedure SynchronizingCallbackPrecedesBackendInputs;
    procedure OfflineCallbackFollowsEveryBackendInput;
    procedure ActualWriteSummaryCompletesSelectedTarget;
    procedure FinalStatusSurvivesSubscriberException;
    procedure BufferedWorkSurvivesReconnectUntilEveryScopeDrains;
    procedure StorageFailureLatchesFirstCauseUntilRestart;
    procedure StorageFailureNotificationNeverWaitsForUi;
    procedure StorageFailureRetiresBufferedWorkSafely;
    procedure StorageFailurePrecedesBackendCallbacks;
    procedure NonStorageExceptionsDoNotLatchFailure;
    procedure ActualCheckpointSaveFailureLatchesCanonicalNode;
    procedure ActualFalseBlockSaveLatchesWithoutAcknowledging;
    procedure SafeboxFlushFailureLatchesBeforeUiDelivery;
    procedure FalseBlockCacheFlushLatchesBeforeUiDelivery;
  end;

implementation

uses SyncObjs, UThread, UConst, UPCDataTypes, UOpTransaction;

type
  TSynchronizationSafeBoxAccess = class(TPCSafeBox)
  public
    procedure AddGeneratedBlock(const AKey: TAccountKey);
  end;

  TSynchronizationScopeThread = class(TPCThread)
  private
    FNode: TNode;
    FPeer: TObject;
  protected
    procedure BCExecute; override;
  public
    Entered, ReleaseScope, Completed, ExerciseRetiredScope, RetiredScopeChecked: TEvent;
    ErrorMessage: String;
    constructor Create(ANode: TNode; APeer: TObject);
    destructor Destroy; override;
  end;

  TOwnedOutOfMemory = class(EOutOfMemory)
  public
    procedure FreeInstance; override;
  end;

  TFalseBlockStorage = class(TFileStorage)
  protected
    function DoSaveBlockChain(Operations: TPCOperationsComp): Boolean; override;
  public
    SaveCalls: Integer;
  end;

type
  TStorageFlushNode = class(TNode)
  protected
    procedure DoFlushSafebox; override;
    function DoFlushBlockStorage: Boolean; override;
  public
    SafeboxFault: Integer;
    BlockFlushFails: Boolean;
  end;

procedure TStorageFlushNode.DoFlushSafebox;
begin
  case SafeboxFault of
    1: raise EAbort.Create('Synthetic flush cancellation');
    2: raise EWriteError.Create('Synthetic safebox flush failure');
  else
    inherited;
  end;
end;

function TStorageFlushNode.DoFlushBlockStorage: Boolean;
begin
  if BlockFlushFails then Result := False
  else Result := inherited DoFlushBlockStorage;
end;

procedure TOwnedOutOfMemory.FreeInstance;
begin
  // RTL heap exceptions normally represent protected preallocated instances.
  // This synthetic test object is normally allocated and must own its cleanup.
  CleanupInstance;
  FreeMem(Pointer(Self));
end;

function TFalseBlockStorage.DoSaveBlockChain(Operations: TPCOperationsComp): Boolean;
begin
  Inc(SaveCalls);
  Result := False;
end;

function ReadStorageFixture(const AFileName: String): TBytes;
var LFile: TFileStream;
begin
  Result := Nil;
  LFile := TFileStream.Create(AFileName,fmOpenRead+fmShareDenyNone);
  try
    SetLength(Result,LFile.Size);
    if Length(Result)>0 then LFile.ReadBuffer(Result[0],Length(Result));
  finally
    LFile.Free;
  end;
end;

function StorageFixtureEquals(const ALeft, ARight: TBytes): Boolean;
begin
  Result := Length(ALeft)=Length(ARight);
  if Result and (Length(ALeft)>0) then Result := CompareMem(@ALeft[0],@ARight[0],Length(ALeft));
end;

type
  TStorageFailureThread = class(TPCThread)
  private
    FNode: TNode;
  protected
    procedure BCExecute; override;
  public
    Completed: TEvent;
    ErrorText: String;
    constructor Create(ANode: TNode);
    destructor Destroy; override;
  end;

constructor TStorageFailureThread.Create(ANode: TNode);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
  Completed := TEvent.Create(nil,True,False,'');
end;

destructor TStorageFailureThread.Destroy;
begin
  Terminate;
  if Suspended then Start;
  WaitFor;
  Completed.Free;
  inherited;
end;

procedure TStorageFailureThread.BCExecute;
var LError: EWriteError;
begin
  try
    try
      LError := EWriteError.Create('owned injected disk full');
      try
        FNode.ReportStorageFailure(sfsCheckpoint,'owned-checkpoint.am_safebox',LError);
      finally
        LError.Free;
      end;
    except
      on E: Exception do ErrorText := E.ClassName+': '+E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

constructor TSynchronizationScopeThread.Create(ANode: TNode; APeer: TObject);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
  FPeer := APeer;
  Entered := TEvent.Create(nil,True,False,'');
  ReleaseScope := TEvent.Create(nil,True,False,'');
  Completed := TEvent.Create(nil,True,False,'');
  ExerciseRetiredScope := TEvent.Create(nil,True,False,'');
  RetiredScopeChecked := TEvent.Create(nil,True,False,'');
end;

destructor TSynchronizationScopeThread.Destroy;
begin
  Terminate;
  ReleaseScope.SetEvent;
  if Suspended then Start;
  WaitFor;
  RetiredScopeChecked.Free;
  ExerciseRetiredScope.Free;
  Completed.Free;
  ReleaseScope.Free;
  Entered.Free;
  inherited;
end;

procedure TSynchronizationScopeThread.BCExecute;
var LProgress: TProgressInfo;
begin
  try
    try
      if Terminated then Exit;
      FNode.BeginBlockChainUpdate;
      try
        Entered.SetEvent;
        while not Terminated do begin
          if ExerciseRetiredScope.WaitFor(0) = wrSignaled then begin
            ExerciseRetiredScope.ResetEvent;
            FNode.BeginBlockChainUpdate;
            try
              FNode.SetSynchronizationTarget(FPeer,1000);
              LProgress := TProgressInfo.Create(psDownloadingBlockchain);
              LProgress.BlockNumber := 777;
              FNode.SetSynchronizationProgress(LProgress);
            finally FNode.EndBlockChainUpdate; end;
            RetiredScopeChecked.SetEvent;
          end;
          if ReleaseScope.WaitFor(10) = wrSignaled then Break;
        end;
      finally FNode.EndBlockChainUpdate; end;
    except
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally Completed.SetEvent; end;
end;

procedure TSynchronizationSafeBoxAccess.AddGeneratedBlock(const AKey: TAccountKey);
var LBlock: TOperationBlock;
begin
  LBlock := CT_OperationBlock_NUL;
  LBlock.block := BlocksCount;
  LBlock.account_key := AKey;
  LBlock.protocol_version := CT_PROTOCOL_1;
  LBlock.protocol_available := CT_PROTOCOL_6;
  LBlock.compact_target := CT_MinCompactTarget_v1;
  AddNew(LBlock);
end;

procedure TNodeSynchronizationTests.SetUp;
var LID: TGUID; LDirectory: String; LOperation: TPCOperation; I: Integer;
begin
  inherited;
  AssertFalse('Each state fixture owns a fresh node',TNode.NodeExists);
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetTempDir) + 'NodeSynchronization-' + GUIDToString(LID);
  AssertFalse(DirectoryExists(LDirectory));
  ForceDirectories(LDirectory);
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(LDirectory);
  TCrypto.InitCrypto;
  FStopping := False;
  FThrowNextBlock := False;
  FThrownBlockCallbacks := 0;
  FCallbacks := 0;
  FTrace := TStringList.Create;
  FPeer := TObject.Create;
  FSecondPeer := TObject.Create;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FOperations := TOperationsHashTree.Create;
  for I := 1 to 3 do begin
    LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_1,1000,I,1001,
      FKey,100,1,CT_TOperationPayload_NUL);
    try FOperations.AddOperationToHashTree(LOperation);
    finally LOperation.Free; end;
  end;
  FNode := TStorageFlushNode.Create(nil);
  FNode.BroadcastData := False;
  AssertFalse('The state fixture never opens a listener',FNode.NetServer.Active);
  FWatch := TOrderedAccountKeysList.Create(FNode.Bank.SafeBox,False);
  FNotify := TNodeNotifyEvents.Create(nil);
  FNotify.OnSynchronizationChanged := OnSynchronization;
  FNotify.OnBlocksChanged := OnBlocks;
  FNotify.OnOperationsChanged := OnOperations;
  FNotify.OnKeyActivity := OnKeys;
  FNotify.OnNodeMessageEvent := OnMessage;
  FNotify.WatchKeys := FWatch;
  FNotify.Node := FNode;
  FNode.SetSynchronizationNetworkActive(False);
  WaitForState(nssOffline);
end;

procedure TNodeSynchronizationTests.TearDown;
begin
  FStopping := True;
  if Assigned(FNode) then FNode.SetSynchronizationNetworkActive(False);
  if Assigned(FNotify) then FNotify.Node := nil;
  FNotify.Free;
  FWatch.Free;
  FNode.Free;
  FOperations.Free;
  FKey.Free;
  FSecondPeer.Free;
  FPeer.Free;
  FTrace.Free;
  TNode.SetPascalCoinDataFolder(FPreviousDirectory);
  inherited;
end;

procedure TNodeSynchronizationTests.OnSynchronization(Sender: TObject;
  const AStatus: TNodeSynchronizationStatus);
begin
  if FStopping then Exit;
  FLast := AStatus;
  Inc(FCallbacks);
  FCallbackThread := TThread.CurrentThread.ThreadID;
  FTrace.Add('sync');
end;

procedure TNodeSynchronizationTests.OnBlocks(Sender: TObject);
begin
  if FStopping then Exit;
  FTrace.Add('blocks');
  if FThrowNextBlock then begin
    FThrowNextBlock := False;
    Inc(FThrownBlockCallbacks);
    raise Exception.Create('Owned one-shot block subscriber failure');
  end;
end;

procedure TNodeSynchronizationTests.OnOperations(Sender: TObject);
begin
  if not FStopping then FTrace.Add('operations');
end;

procedure TNodeSynchronizationTests.OnKeys(Sender: TObject);
begin
  if not FStopping then FTrace.Add('keys');
end;

procedure TNodeSynchronizationTests.OnMessage(Sender: TNetConnection; AMessage: String);
begin
  if not FStopping then FTrace.Add('message');
end;

procedure TNodeSynchronizationTests.PumpFor(AMilliseconds: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure TNodeSynchronizationTests.WaitForState(AState: TNodeSynchronizationState);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + 3000;
  repeat
    CheckSynchronize(0);
    if (FCallbacks > 0) and (FLast.State = AState) then begin
      AssertEquals('Existing notifier keeps UI listeners on their owner thread',
        UInt64(MainThreadID),FCallbackThread);
      Exit;
    end;
    Sleep(1);
  until GetTickCount64 >= LUntil;
  Fail('The actual node notifier did not publish its current synchronization state');
end;

procedure TNodeSynchronizationTests.StartSynchronizing;
begin
  FNode.SetSynchronizationNetworkActive(True);
  FNode.SetSynchronizationPeerConnected(FPeer,True);
  FNode.SetSynchronizationTarget(FPeer,FNode.GetSynchronizationStatus.LocalBlockCount+10);
  WaitForState(nssSynchronizing);
end;

procedure TNodeSynchronizationTests.SelectedTargetSurvivesInterBatchSilence;
var LStatus: TNodeSynchronizationStatus;
begin
  StartSynchronizing;
  FNode.BeginBlockChainUpdate;
  FNode.EndBlockChainUpdate;
  PumpFor(6200);
  LStatus := FNode.GetSynchronizationStatus;
  AssertTrue('No transfer activity is needed between batches',LStatus.ActiveTransfers = 0);
  AssertTrue('The selected peer target survives a gap longer than five seconds',LStatus.HasRemoteTarget);
  AssertTrue(LStatus.TargetBlockCount > LStatus.LocalBlockCount);
  AssertTrue('Elapsed silence does not complete synchronization',LStatus.State = nssSynchronizing);
  AssertTrue(FLast.State = nssSynchronizing);
end;

procedure TNodeSynchronizationTests.NetworkOffWaitsForBufferedWork;
var LStatus: TNodeSynchronizationStatus;
begin
  StartSynchronizing;
  FNode.BeginBlockChainUpdate;
  try
    FNode.SetSynchronizationNetworkActive(False);
    LStatus := FNode.GetSynchronizationStatus;
    AssertTrue('Already buffered work is still synchronization',LStatus.State = nssSynchronizing);
    AssertFalse(LStatus.NetworkActive);
    AssertFalse('Stopping transport discards the remote download target',LStatus.HasRemoteTarget);
    AssertEquals(0,LStatus.PeerCount);
    AssertEquals('The active physical scope is not retired by network off',1,LStatus.ActiveTransfers);
    AssertTrue(FNode.IsUpdatingBlockChain);
    WaitForState(nssSynchronizing);
  finally FNode.EndBlockChainUpdate; end;
  LStatus := FNode.GetSynchronizationStatus;
  AssertEquals('Local availability resumes only after buffered work drains',0,LStatus.ActiveTransfers);
  AssertTrue(LStatus.State = nssOffline);
  WaitForState(nssOffline);
end;

procedure TNodeSynchronizationTests.DisconnectClearsTargetsBeforeReconnect;
var LStatus: TNodeSynchronizationStatus;
begin
  StartSynchronizing;
  FNode.SetSynchronizationPeerConnected(FSecondPeer,True);
  FNode.SetSynchronizationTarget(FSecondPeer,5);
  FNode.BeginBlockChainUpdate;
  try
    FNode.SetSynchronizationPeerConnected(FPeer,False);
    AssertTrue(FNode.GetSynchronizationStatus.State = nssSynchronizing);
    FNode.SetSynchronizationPeerConnected(FSecondPeer,False);
    LStatus := FNode.GetSynchronizationStatus;
    AssertTrue('No peers stops downloads but does not finish buffered work',LStatus.State = nssSynchronizing);
    AssertFalse(LStatus.HasRemoteTarget);
    AssertEquals(0,LStatus.PeerCount);
    AssertEquals(1,LStatus.ActiveTransfers);
    WaitForState(nssSynchronizing);
  finally FNode.EndBlockChainUpdate; end;
  WaitForState(nssDisconnected);
  FNode.SetSynchronizationPeerConnected(FPeer,True);
  LStatus := FNode.GetSynchronizationStatus;
  AssertTrue('Reconnect is available when no buffered work remains',LStatus.State = nssIdle);
  AssertFalse('Reconnecting has no remembered target',LStatus.HasRemoteTarget);
  WaitForState(nssIdle);
end;

procedure TNodeSynchronizationTests.QueueBackendCallbacks;
var LError: String;
begin
  FWatch.Clear;
  FNode.NotifyBlocksChanged;
  // Exercise real admission as well: catch-up now reports unavailability before
  // validating the absent senders, while the offline path rejects them normally.
  FExpectOperationCallback := FNode.PendingOperationsAvailable;
  if FExpectOperationCallback then
    AssertEquals(0,FNode.AddOperations(nil,FOperations,nil,LError))
  else
    AssertTrue(FNode.AddOperations(nil,FOperations,nil,LError)<0);
  // Unavailable admission changes no operations and must not invent a pending
  // data notification. The offline path still exercises that real callback.
  FNode.NotifyNetClientMessage(nil,'owned notification-order check');
end;

procedure TNodeSynchronizationTests.WaitForCombinedCallbacks;
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + 3000;
  repeat
    CheckSynchronize(0);
    if (FTrace.IndexOf('sync') >= 0) and (FTrace.IndexOf('blocks') >= 0) and
      ((not FExpectOperationCallback) or (FTrace.IndexOf('operations') >= 0)) and (FTrace.IndexOf('message') >= 0) and
      (FTrace.IndexOf('keys') >= 0) then Exit;
    Sleep(1);
  until GetTickCount64 >= LUntil;
  Fail('The actual combined notifier batch was incomplete: ' + FTrace.CommaText);
end;

procedure TNodeSynchronizationTests.SynchronizingCallbackPrecedesBackendInputs;
begin
  StartSynchronizing;
  PumpFor(200);
  FTrace.Clear;
  FNode.SetSynchronizationTarget(FPeer,20);
  QueueBackendCallbacks;
  WaitForCombinedCallbacks;
  AssertEquals('Pause arrives before every backend request',0,FTrace.IndexOf('sync'));
  AssertEquals('Unavailable admission must not publish an operation change',-1,FTrace.IndexOf('operations'));
end;

procedure TNodeSynchronizationTests.OfflineCallbackFollowsEveryBackendInput;
begin
  StartSynchronizing;
  PumpFor(200);
  FTrace.Clear;
  FNode.SetSynchronizationNetworkActive(False);
  QueueBackendCallbacks;
  WaitForCombinedCallbacks;
  AssertEquals('Resume follows blocks, operations, messages and key activity',
    FTrace.Count-1,FTrace.IndexOf('sync'));
  AssertTrue(FLast.State = nssOffline);
end;

procedure TNodeSynchronizationTests.ActualWriteSummaryCompletesSelectedTarget;
var LMempool: TPCOperationsComp; LStatus: TNodeSynchronizationStatus;
begin
  // Retain this empty fixture's pool before catch-up makes account scopes busy.
  // Confirmed bank mutation uses the node's ordinary bank writer scope below.
  LMempool := FNode.LockMempoolRead;
  FNode.UnlockMempoolRead;
  FNode.SetSynchronizationNetworkActive(True);
  FNode.SetSynchronizationPeerConnected(FPeer,True);
  FNode.SetSynchronizationTarget(FPeer,1);
  WaitForState(nssSynchronizing);
  AssertTrue(FNode.TryLockNode(3000));
  try
    TSynchronizationSafeBoxAccess(FNode.Bank.SafeBox).AddGeneratedBlock(FKey.PublicKey);
    LMempool.Clear(True);
  finally FNode.UnlockNode; end;
  LStatus := FNode.GetSynchronizationStatus;
  AssertTrue(LStatus.HasMempoolSummary);
  AssertEquals(1,Int64(LStatus.LocalBlockCount));
  AssertTrue('Actual write release updates the cached local tip',LStatus.State = nssIdle);
  WaitForState(nssIdle);
end;

procedure TNodeSynchronizationTests.BufferedWorkSurvivesReconnectUntilEveryScopeDrains;
var LOld, LNew: TSynchronizationScopeThread; LStatus: TNodeSynchronizationStatus;
begin
  LOld := nil;
  LNew := nil;
  try
    FNode.SetSynchronizationNetworkActive(True);
    FNode.SetSynchronizationPeerConnected(FPeer,True);
    LOld := TSynchronizationScopeThread.Create(FNode,FPeer);
    LOld.Start;
    AssertEquals(Ord(wrSignaled),Ord(LOld.Entered.WaitFor(3000)));
    AssertTrue(FNode.GetSynchronizationStatus.State = nssSynchronizing);
    FNode.SetSynchronizationPeerConnected(FPeer,False);
    FNode.SetSynchronizationPeerConnected(FPeer,True);
    LStatus := FNode.GetSynchronizationStatus;
    AssertTrue('Reconnect cannot finish already buffered processing',LStatus.State = nssSynchronizing);
    AssertEquals(1,LStatus.ActiveTransfers);
    AssertTrue(FNode.IsUpdatingBlockChain);
    LOld.ExerciseRetiredScope.SetEvent;
    AssertEquals(Ord(wrSignaled),Ord(LOld.RetiredScopeChecked.WaitFor(3000)));
    LStatus := FNode.GetSynchronizationStatus;
    AssertTrue('Nested buffered work preserves synchronization',LStatus.State = nssSynchronizing);
    AssertEquals('The completed nested scope retains its outer processing scope',1,LStatus.ActiveTransfers);
    AssertFalse('Stale producers cannot restore a peer download target',LStatus.HasRemoteTarget);
    LNew := TSynchronizationScopeThread.Create(FNode,FPeer);
    LNew.Start;
    AssertEquals(Ord(wrSignaled),Ord(LNew.Entered.WaitFor(3000)));
    AssertEquals('Both physical processing scopes remain active',2,FNode.GetSynchronizationStatus.ActiveTransfers);
    LOld.ReleaseScope.SetEvent;
    AssertEquals(Ord(wrSignaled),Ord(LOld.Completed.WaitFor(3000)));
    AssertEquals('',LOld.ErrorMessage);
    LStatus := FNode.GetSynchronizationStatus;
    AssertEquals('An old End cannot release the new processing scope',1,LStatus.ActiveTransfers);
    AssertTrue(LStatus.State = nssSynchronizing);
    LNew.ReleaseScope.SetEvent;
    AssertEquals(Ord(wrSignaled),Ord(LNew.Completed.WaitFor(3000)));
    AssertEquals('',LNew.ErrorMessage);
    AssertEquals(0,FNode.GetSynchronizationStatus.ActiveTransfers);
    AssertTrue(FNode.GetSynchronizationStatus.State = nssIdle);
    AssertFalse(FNode.IsUpdatingBlockChain);
    WaitForState(nssIdle);
  finally
    LNew.Free;
    LOld.Free;
  end;
end;

procedure TNodeSynchronizationTests.StorageFailureLatchesFirstCauseUntilRestart;
var LError: Exception; LStatus: TNodeSynchronizationStatus; LProgress: TProgressInfo;
  LReason: String; LMempool, LBlock: TPCOperationsComp; LRaised: Boolean;
begin
  LError := EWriteError.Create('owned first storage failure');
  try
    FNode.ReportStorageFailure(sfsCheckpoint,'first-checkpoint.am_safebox',LError);
  finally
    LError.Free;
  end;
  LError := EOSError.Create('owned later failure');
  try
    FNode.ReportStorageFailure(sfsBlockStream,'later.blocks',LError);
  finally
    LError.Free;
  end;
  FNode.SetSynchronizationNetworkActive(False);
  FNode.SetSynchronizationNetworkActive(True);
  FNode.SetSynchronizationPeerConnected(FPeer,True);
  FNode.SetSynchronizationTarget(FPeer,1000);
  LProgress := TProgressInfo.Create(psDownloadingBlockchain);
  LProgress.BlockNumber := 999;
  FNode.SetSynchronizationProgress(LProgress);
  FNode.BeginBlockChainUpdate;
  FNode.EndBlockChainUpdate;
  LStatus := FNode.GetSynchronizationStatus;
  AssertTrue(FNode.HasStorageFailure);
  AssertTrue(LStatus.State=nssStorageError);
  AssertTrue(LStatus.StorageFailure.Stage=sfsCheckpoint);
  AssertEquals('first-checkpoint.am_safebox',LStatus.StorageFailure.FileName);
  AssertEquals('EWriteError',LStatus.StorageFailure.ErrorClass);
  AssertEquals('owned first storage failure',LStatus.StorageFailure.ErrorMessage);
  AssertFalse(FNode.CanRequestBlockChainData);
  AssertFalse(FNode.PendingOperationsAvailable);
  AssertFalse(FNode.IsReady(LReason));
  AssertEquals('owned first storage failure',LReason);
  AssertFalse(FNode.IsBlockChainValid(LReason));
  AssertEquals('owned first storage failure',LReason);
  AssertTrue(FNode.AddOperations(nil,FOperations,nil,LReason)<0);
  AssertEquals('owned first storage failure',LReason);
  LBlock := TPCOperationsComp.Create(FNode.Bank);
  try
    AssertFalse(FNode.AddNewBlockChain(nil,LBlock,LReason));
    AssertEquals('owned first storage failure',LReason);
  finally
    LBlock.Free;
  end;
  AssertFalse(FNode.TryLockMempoolRead(0,LMempool));
  LRaised := False;
  try
    LMempool := FNode.LockMempoolWrite;
    FNode.UnlockMempoolWrite;
  except
    on E: ENodeStorageUnavailable do LRaised := True;
  end;
  AssertTrue('Storage failure prevents new operation writers',LRaised);
  AssertEquals(0,Int64(FNode.Bank.BlocksCount));
  WaitForState(nssStorageError);
end;

procedure TNodeSynchronizationTests.StorageFailureNotificationNeverWaitsForUi;
var LThread: TStorageFailureThread; LBefore: Integer;
begin
  LBefore := FCallbacks;
  LThread := TStorageFailureThread.Create(FNode);
  try
    LThread.Start;
    // No CheckSynchronize here: a producer must finish even while its actual
    // main-thread subscriber cannot run. The event record owns copied strings.
    AssertEquals(Ord(wrSignaled),Ord(LThread.Completed.WaitFor(3000)));
    AssertEquals('',LThread.ErrorText);
    AssertEquals(LBefore,FCallbacks);
    AssertTrue(FNode.GetSynchronizationStatus.State=nssStorageError);
    WaitForState(nssStorageError);
    AssertEquals('owned injected disk full',FLast.StorageFailure.ErrorMessage);
  finally
    LThread.Free;
  end;
end;

procedure TNodeSynchronizationTests.StorageFailureRetiresBufferedWorkSafely;
var LThread: TSynchronizationScopeThread; LError: Exception;
begin
  FNode.SetSynchronizationNetworkActive(True);
  FNode.SetSynchronizationPeerConnected(FPeer,True);
  LThread := TSynchronizationScopeThread.Create(FNode,FPeer);
  try
    LThread.Start;
    AssertEquals(Ord(wrSignaled),Ord(LThread.Entered.WaitFor(3000)));
    LError := EWriteError.Create('owned buffered storage failure');
    try
      FNode.ReportStorageFailure(sfsBlockStream,'owned.blocks',LError);
    finally
      LError.Free;
    end;
    AssertEquals(1,FNode.GetSynchronizationStatus.ActiveTransfers);
    AssertTrue('Failure has precedence while physical work retires',FNode.GetSynchronizationStatus.State=nssStorageError);
    AssertFalse(FNode.CanRequestBlockChainData);
    LThread.ReleaseScope.SetEvent;
    AssertEquals(Ord(wrSignaled),Ord(LThread.Completed.WaitFor(3000)));
    AssertEquals('',LThread.ErrorMessage);
    AssertEquals(0,FNode.GetSynchronizationStatus.ActiveTransfers);
    AssertTrue('Draining work never clears the storage failure',FNode.GetSynchronizationStatus.State=nssStorageError);
    WaitForState(nssStorageError);
  finally
    LThread.Free;
  end;
end;

procedure TNodeSynchronizationTests.StorageFailurePrecedesBackendCallbacks;
var LError: Exception;
begin
  FTrace.Clear;
  LError := EWriteError.Create('owned callback-order storage failure');
  try
    FNode.ReportStorageFailure(sfsCheckpoint,'owned-checkpoint',LError);
  finally
    LError.Free;
  end;
  QueueBackendCallbacks;
  WaitForCombinedCallbacks;
  AssertEquals('Storage error is delivered before backend refresh requests',0,FTrace.IndexOf('sync'));
  AssertTrue(FLast.State=nssStorageError);
  AssertEquals(-1,FTrace.IndexOf('operations'));
end;

procedure TNodeSynchronizationTests.NonStorageExceptionsDoNotLatchFailure;
var LError: Exception; I: Integer;
begin
  for I := 0 to 2 do begin
    case I of
      0: LError := TOwnedOutOfMemory.Create('owned exhaustion');
      1: LError := EAbort.Create('owned cancellation');
    else
      LError := EAccessViolation.Create('owned programmer fault');
    end;
    try
      FNode.ReportStorageFailure(sfsUnknown,'not-a-storage-failure',LError);
    finally
      LError.Free;
    end;
    AssertFalse(FNode.HasStorageFailure);
    AssertTrue(FNode.GetSynchronizationStatus.State=nssOffline);
  end;
end;

procedure TNodeSynchronizationTests.ActualCheckpointSaveFailureLatchesCanonicalNode;
var LDirectory, LDestination, LPrevious, LReason: String;
  LBefore: TBytes; LFailed: Boolean; LStatus: TNodeSynchronizationStatus;
  LIncoming: TPCOperationsComp;
begin
  FNode.Bank.StorageClass := TFileStorage;
  TSynchronizationSafeBoxAccess(FNode.Bank.SafeBox).AddGeneratedBlock(FKey.PublicKey);
  LDirectory := FNode.Bank.GetStorageFolder('');
  LPrevious := LDirectory+PathDelim+'previous.am_safebox';
  FNode.Bank.SafeBox.SaveCheckpointing(LPrevious);
  LBefore := ReadStorageFixture(LPrevious);
  LDestination := FNode.Bank.GetSafeboxCheckpointingFileName(LDirectory,FNode.Bank.BlocksCount);
  AssertTrue(ForceDirectories(LDestination));
  LFailed := False;
  try
    FNode.Bank.SaveBank(True);
  except
    on E: EOSError do LFailed := True;
  end;
  AssertTrue('Real atomic publication to a directory must fail',LFailed);
  LStatus := FNode.GetSynchronizationStatus;
  AssertTrue(LStatus.State=nssStorageError);
  AssertTrue(LStatus.StorageFailure.Stage=sfsCheckpoint);
  AssertEquals(LDestination,LStatus.StorageFailure.FileName);
  AssertTrue('The complete earlier checkpoint is preserved',StorageFixtureEquals(LBefore,ReadStorageFixture(LPrevious)));
  AssertTrue(DirectoryExists(LDestination));
  FNode.SetSynchronizationNetworkActive(False);
  FNode.SetSynchronizationNetworkActive(True);
  LIncoming := TPCOperationsComp.Create(FNode.Bank);
  try
    AssertFalse(FNode.AddNewBlockChain(nil,LIncoming,LReason));
    AssertFalse(FNode.Bank.AddNewBlockChainBlock(LIncoming,High(Cardinal),LReason));
  finally
    LIncoming.Free;
  end;
  AssertEquals(1,Int64(FNode.Bank.BlocksCount));
  AssertFalse(FNode.CanRequestBlockChainData);
  WaitForState(nssStorageError);
end;

procedure TNodeSynchronizationTests.ActualFalseBlockSaveLatchesWithoutAcknowledging;
var LPrevious, LReason: String; LBefore: TBytes; LFailed: Boolean;
  LIncoming: TPCOperationsComp; LStorage: TFalseBlockStorage;
  LCount, I: Integer; LStatus: TNodeSynchronizationStatus;
begin
  LPrevious := FNode.Bank.GetStorageFolder('')+PathDelim+'previous-empty.am_safebox';
  FNode.Bank.SafeBox.SaveCheckpointing(LPrevious);
  LBefore := ReadStorageFixture(LPrevious);
  FNode.Bank.StorageClass := TFalseBlockStorage;
  LStorage := TFalseBlockStorage(FNode.Bank.Storage);
  LIncoming := TPCOperationsComp.Create(FNode.Bank);
  try
    for I := 0 to FOperations.OperationsCount-1 do
      LIncoming.OperationsHashTree.AddOperationToHashTree(FOperations.GetOperation(I));
    LCount := LIncoming.Count;
    AssertTrue(LCount>0);
    LFailed := False;
    try
      FNode.Bank.Storage.SaveBlockChainBlock(LIncoming);
    except
      on E: EWriteError do LFailed := True;
    end;
    AssertTrue('False storage acknowledgement must become a real write error',LFailed);
    AssertEquals(1,LStorage.SaveCalls);
    AssertEquals(LCount,LIncoming.Count);
    LStatus := FNode.GetSynchronizationStatus;
    AssertTrue(LStatus.State=nssStorageError);
    AssertTrue(LStatus.StorageFailure.Stage=sfsBlockStream);
    AssertTrue(Pos('BlockChainStream.blocks',LStatus.StorageFailure.FileName)>0);
    AssertTrue('Earlier checkpoint bytes remain intact',StorageFixtureEquals(LBefore,ReadStorageFixture(LPrevious)));
    FNode.SetSynchronizationNetworkActive(False);
    FNode.SetSynchronizationNetworkActive(True);
    AssertFalse(FNode.AddNewBlockChain(nil,LIncoming,LReason));
    AssertFalse(FNode.Bank.AddNewBlockChainBlock(LIncoming,High(Cardinal),LReason));
    AssertEquals(1,LStorage.SaveCalls);
    AssertEquals(LCount,LIncoming.Count);
    AssertEquals(0,Int64(FNode.Bank.BlocksCount));
    WaitForState(nssStorageError);
  finally
    LIncoming.Free;
  end;
end;

procedure TNodeSynchronizationTests.SafeboxFlushFailureLatchesBeforeUiDelivery;
var LFileName, LReason: String; LStatus: TNodeSynchronizationStatus;
  LIncoming: TPCOperationsComp; LBefore: TBytes;
begin
  LFileName := FNode.Bank.GetStorageFolder('')+PathDelim+'flush-source.am_safebox';
  FNode.Bank.SafeBox.SetSafeboxFileName(LFileName);
  LBefore := ReadStorageFixture(LFileName);
  TStorageFlushNode(FNode).SafeboxFault := 1;
  try
    FNode.FlushSafebox;
    Fail('Synthetic cancellation must propagate');
  except
    on E: EAbort do AssertEquals('Synthetic flush cancellation',E.Message);
  end;
  AssertFalse('Cancellation cannot latch a storage error',FNode.HasStorageFailure);
  TStorageFlushNode(FNode).SafeboxFault := 2;
  try
    FNode.FlushSafebox;
    Fail('Synthetic safebox write error must propagate');
  except
    on E: EWriteError do AssertEquals('Synthetic safebox flush failure',E.Message);
  end;
  // No UI pump occurred between the actual flush boundary and these guards.
  LStatus := FNode.GetSynchronizationStatus;
  AssertTrue(LStatus.State=nssStorageError);
  AssertTrue(LStatus.StorageFailure.Stage=sfsSafeboxFlush);
  AssertEquals(LFileName,LStatus.StorageFailure.FileName);
  AssertFalse(FNode.CanRequestBlockChainData);
  AssertTrue('The pre-flush source bytes remain intact',StorageFixtureEquals(LBefore,ReadStorageFixture(LFileName)));
  LIncoming := TPCOperationsComp.Create(FNode.Bank);
  try
    AssertFalse(FNode.AddNewBlockChain(Nil,LIncoming,LReason));
    AssertFalse(FNode.Bank.AddNewBlockChainBlock(LIncoming,High(Cardinal),LReason));
  finally
    LIncoming.Free;
  end;
  TStorageFlushNode(FNode).SafeboxFault := 0;
  FNode.FlushSafebox;
  FNode.SetSynchronizationNetworkActive(True);
  AssertEquals('Synthetic safebox flush failure',FNode.GetSynchronizationStatus.StorageFailure.ErrorMessage);
  WaitForState(nssStorageError);
end;

procedure TNodeSynchronizationTests.FalseBlockCacheFlushLatchesBeforeUiDelivery;
var LStatus: TNodeSynchronizationStatus; LReason: String;
  LIncoming: TPCOperationsComp;
begin
  FNode.Bank.StorageClass := TFileStorage;
  TStorageFlushNode(FNode).BlockFlushFails := True;
  try
    FNode.FlushBlockStorage;
    Fail('False block-cache flush must become a write error');
  except
    on E: EWriteError do AssertTrue(Pos('Cannot flush blockchain storage',E.Message)>0);
  end;
  LStatus := FNode.GetSynchronizationStatus;
  AssertTrue(LStatus.State=nssStorageError);
  AssertTrue(LStatus.StorageFailure.Stage=sfsBlockStream);
  AssertEquals('EWriteError',LStatus.StorageFailure.ErrorClass);
  AssertFalse(FNode.CanRequestBlockChainData);
  LIncoming := TPCOperationsComp.Create(FNode.Bank);
  try
    AssertFalse(FNode.AddNewBlockChain(Nil,LIncoming,LReason));
    AssertFalse(FNode.Bank.AddNewBlockChainBlock(LIncoming,High(Cardinal),LReason));
  finally
    LIncoming.Free;
  end;
  TStorageFlushNode(FNode).BlockFlushFails := False;
  FNode.FlushBlockStorage;
  FNode.SetSynchronizationNetworkActive(False);
  FNode.SetSynchronizationNetworkActive(True);
  AssertEquals(LStatus.StorageFailure.ErrorMessage,FNode.GetSynchronizationStatus.StorageFailure.ErrorMessage);
  WaitForState(nssStorageError);
end;

procedure TNodeSynchronizationTests.FinalStatusSurvivesSubscriberException;
begin
  StartSynchronizing;
  PumpFor(200);
  FTrace.Clear;
  FThrowNextBlock := True;
  // Both flags are set without pumping the UI. The actual notifier must run
  // the final status callback even when the preceding block subscriber fails.
  FNode.SetSynchronizationNetworkActive(False);
  FNode.NotifyBlocksChanged;
  WaitForState(nssOffline);
  AssertEquals('The expected subscriber failure was exercised exactly once',1,FThrownBlockCallbacks);
  AssertTrue('No additional producer event was needed to deliver final status',
    (FTrace.IndexOf('blocks') >= 0) and (FTrace.IndexOf('sync') > FTrace.IndexOf('blocks')));
  AssertEquals(0,FNode.GetSynchronizationStatus.ActiveTransfers);
end;

initialization
  RegisterTest(TNodeSynchronizationTests);

end.
