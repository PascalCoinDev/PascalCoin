unit UNodeSynchronizationChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Plain native checks of the actual node synchronization facts and notifier.
  Generated empty-node data and owned peer identities only; no sockets or keys
  from an existing wallet are used. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunNodeSynchronizationChecks(const ALogPath: String): Integer;

implementation

uses {$IFNDEF FPC}Winapi.Windows,{$ELSE}Windows,{$ENDIF}
  Classes, SysUtils, UNode, UAccounts, UBlockChain,
  UCrypto, UNetProtocol, SyncObjs, UThread, UConst, UPCDataTypes, UOpTransaction, UFileStorage;

type
  TNodeSynchronizationChecks = class
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
    FDirectorySet: Boolean;
    procedure Check(AValue: Boolean); overload;
    procedure Check(const AMessage: String; AValue: Boolean); overload;
    procedure CheckFalse(AValue: Boolean); overload;
    procedure CheckFalse(const AMessage: String; AValue: Boolean); overload;
    procedure CheckEqual(AExpected, AActual: Int64); overload;
    procedure CheckEqual(const AMessage: String; AExpected, AActual: Int64); overload;
    procedure CheckEqual(const AExpected, AActual: String); overload;
    procedure Fail(const AMessage: String);
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
    procedure SetUp;
    procedure TearDown;
  public
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

procedure TNodeSynchronizationChecks.SetUp;
var LID: TGUID; LDirectory: String; LOperation: TPCOperation; I: Integer;
begin
  CheckFalse('Each state fixture owns a fresh node',TNode.NodeExists);
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'NodeSynchronization-' + GUIDToString(LID);
  CheckFalse(DirectoryExists(LDirectory));
  ForceDirectories(LDirectory);
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(LDirectory);
  FDirectorySet := True;
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
  CheckFalse('The state fixture never opens a listener',FNode.NetServer.Active);
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

procedure TNodeSynchronizationChecks.TearDown;
begin
  FStopping := True;
  if Assigned(FNode) then FNode.SetSynchronizationNetworkActive(False);
  if Assigned(FNotify) then FNotify.Node := nil;
  FreeAndNil(FNotify);
  FreeAndNil(FWatch);
  FreeAndNil(FNode);
  FreeAndNil(FOperations);
  FreeAndNil(FKey);
  FreeAndNil(FSecondPeer);
  FreeAndNil(FPeer);
  FreeAndNil(FTrace);
  if FDirectorySet then TNode.SetPascalCoinDataFolder(FPreviousDirectory);
end;

procedure TNodeSynchronizationChecks.OnSynchronization(Sender: TObject;
  const AStatus: TNodeSynchronizationStatus);
begin
  if FStopping then Exit;
  FLast := AStatus;
  Inc(FCallbacks);
  FCallbackThread := TThread.CurrentThread.ThreadID;
  FTrace.Add('sync');
end;

procedure TNodeSynchronizationChecks.OnBlocks(Sender: TObject);
begin
  if FStopping then Exit;
  FTrace.Add('blocks');
  if FThrowNextBlock then begin
    FThrowNextBlock := False;
    Inc(FThrownBlockCallbacks);
    raise Exception.Create('Owned one-shot block subscriber failure');
  end;
end;

procedure TNodeSynchronizationChecks.OnOperations(Sender: TObject);
begin
  if not FStopping then FTrace.Add('operations');
end;

procedure TNodeSynchronizationChecks.OnKeys(Sender: TObject);
begin
  if not FStopping then FTrace.Add('keys');
end;

procedure TNodeSynchronizationChecks.OnMessage(Sender: TNetConnection; AMessage: String);
begin
  if not FStopping then FTrace.Add('message');
end;

procedure TNodeSynchronizationChecks.PumpFor(AMilliseconds: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure TNodeSynchronizationChecks.WaitForState(AState: TNodeSynchronizationState);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + 3000;
  repeat
    CheckSynchronize(0);
    if (FCallbacks > 0) and (FLast.State = AState) then begin
      CheckEqual('Existing notifier keeps UI listeners on their owner thread',
        UInt64(MainThreadID),FCallbackThread);
      Exit;
    end;
    Sleep(1);
  until GetTickCount64 >= LUntil;
  Fail('The actual node notifier did not publish its current synchronization state');
end;

procedure TNodeSynchronizationChecks.StartSynchronizing;
begin
  FNode.SetSynchronizationNetworkActive(True);
  FNode.SetSynchronizationPeerConnected(FPeer,True);
  FNode.SetSynchronizationTarget(FPeer,FNode.GetSynchronizationStatus.LocalBlockCount+10);
  WaitForState(nssSynchronizing);
end;

procedure TNodeSynchronizationChecks.SelectedTargetSurvivesInterBatchSilence;
var LStatus: TNodeSynchronizationStatus;
begin
  StartSynchronizing;
  FNode.BeginBlockChainUpdate;
  FNode.EndBlockChainUpdate;
  PumpFor(6200);
  LStatus := FNode.GetSynchronizationStatus;
  Check('No transfer activity is needed between batches',LStatus.ActiveTransfers = 0);
  Check('The selected peer target survives a gap longer than five seconds',LStatus.HasRemoteTarget);
  Check(LStatus.TargetBlockCount > LStatus.LocalBlockCount);
  Check('Elapsed silence does not complete synchronization',LStatus.State = nssSynchronizing);
  Check(FLast.State = nssSynchronizing);
end;

procedure TNodeSynchronizationChecks.NetworkOffWaitsForBufferedWork;
var LStatus: TNodeSynchronizationStatus;
begin
  StartSynchronizing;
  FNode.BeginBlockChainUpdate;
  try
    FNode.SetSynchronizationNetworkActive(False);
    LStatus := FNode.GetSynchronizationStatus;
    Check('Already buffered work is still synchronization',LStatus.State = nssSynchronizing);
    CheckFalse(LStatus.NetworkActive);
    CheckFalse('Stopping transport discards the remote download target',LStatus.HasRemoteTarget);
    CheckEqual(0,LStatus.PeerCount);
    CheckEqual('The active physical scope is not retired by network off',1,LStatus.ActiveTransfers);
    Check(FNode.IsUpdatingBlockChain);
    WaitForState(nssSynchronizing);
  finally FNode.EndBlockChainUpdate; end;
  LStatus := FNode.GetSynchronizationStatus;
  CheckEqual('Local availability resumes only after buffered work drains',0,LStatus.ActiveTransfers);
  Check(LStatus.State = nssOffline);
  WaitForState(nssOffline);
end;

procedure TNodeSynchronizationChecks.DisconnectClearsTargetsBeforeReconnect;
var LStatus: TNodeSynchronizationStatus;
begin
  StartSynchronizing;
  FNode.SetSynchronizationPeerConnected(FSecondPeer,True);
  FNode.SetSynchronizationTarget(FSecondPeer,5);
  FNode.BeginBlockChainUpdate;
  try
    FNode.SetSynchronizationPeerConnected(FPeer,False);
    Check(FNode.GetSynchronizationStatus.State = nssSynchronizing);
    FNode.SetSynchronizationPeerConnected(FSecondPeer,False);
    LStatus := FNode.GetSynchronizationStatus;
    Check('No peers stops downloads but does not finish buffered work',LStatus.State = nssSynchronizing);
    CheckFalse(LStatus.HasRemoteTarget);
    CheckEqual(0,LStatus.PeerCount);
    CheckEqual(1,LStatus.ActiveTransfers);
    WaitForState(nssSynchronizing);
  finally FNode.EndBlockChainUpdate; end;
  WaitForState(nssDisconnected);
  FNode.SetSynchronizationPeerConnected(FPeer,True);
  LStatus := FNode.GetSynchronizationStatus;
  Check('Reconnect is available when no buffered work remains',LStatus.State = nssIdle);
  CheckFalse('Reconnecting has no remembered target',LStatus.HasRemoteTarget);
  WaitForState(nssIdle);
end;

procedure TNodeSynchronizationChecks.QueueBackendCallbacks;
var LError: String;
begin
  FWatch.Clear;
  FNode.NotifyBlocksChanged;
  // Exercise real admission as well: catch-up now reports unavailability before
  // validating the absent senders, while the offline path rejects them normally.
  FExpectOperationCallback := FNode.PendingOperationsAvailable;
  if FExpectOperationCallback then
    CheckEqual(0,FNode.AddOperations(nil,FOperations,nil,LError))
  else
    Check(FNode.AddOperations(nil,FOperations,nil,LError)<0);
  // Unavailable admission changes no operations and must not invent a pending
  // data notification. The offline path still exercises that real callback.
  FNode.NotifyNetClientMessage(nil,'owned notification-order check');
end;

procedure TNodeSynchronizationChecks.WaitForCombinedCallbacks;
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

procedure TNodeSynchronizationChecks.SynchronizingCallbackPrecedesBackendInputs;
begin
  StartSynchronizing;
  PumpFor(200);
  FTrace.Clear;
  FNode.SetSynchronizationTarget(FPeer,20);
  QueueBackendCallbacks;
  WaitForCombinedCallbacks;
  CheckEqual('Pause arrives before every backend request',0,FTrace.IndexOf('sync'));
  CheckEqual('Unavailable admission must not publish an operation change',-1,FTrace.IndexOf('operations'));
end;

procedure TNodeSynchronizationChecks.OfflineCallbackFollowsEveryBackendInput;
begin
  StartSynchronizing;
  PumpFor(200);
  FTrace.Clear;
  FNode.SetSynchronizationNetworkActive(False);
  QueueBackendCallbacks;
  WaitForCombinedCallbacks;
  CheckEqual('Resume follows blocks, operations, messages and key activity',
    FTrace.Count-1,FTrace.IndexOf('sync'));
  Check(FLast.State = nssOffline);
end;

procedure TNodeSynchronizationChecks.ActualWriteSummaryCompletesSelectedTarget;
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
  Check(FNode.TryLockNode(3000));
  try
    TSynchronizationSafeBoxAccess(FNode.Bank.SafeBox).AddGeneratedBlock(FKey.PublicKey);
    LMempool.Clear(True);
  finally FNode.UnlockNode; end;
  LStatus := FNode.GetSynchronizationStatus;
  Check(LStatus.HasMempoolSummary);
  CheckEqual(1,Int64(LStatus.LocalBlockCount));
  Check('Actual write release updates the cached local tip',LStatus.State = nssIdle);
  WaitForState(nssIdle);
end;

procedure TNodeSynchronizationChecks.BufferedWorkSurvivesReconnectUntilEveryScopeDrains;
var LOld, LNew: TSynchronizationScopeThread; LStatus: TNodeSynchronizationStatus;
begin
  LOld := nil;
  LNew := nil;
  try
    FNode.SetSynchronizationNetworkActive(True);
    FNode.SetSynchronizationPeerConnected(FPeer,True);
    LOld := TSynchronizationScopeThread.Create(FNode,FPeer);
    LOld.Start;
    CheckEqual(Ord(wrSignaled),Ord(LOld.Entered.WaitFor(3000)));
    Check(FNode.GetSynchronizationStatus.State = nssSynchronizing);
    FNode.SetSynchronizationPeerConnected(FPeer,False);
    FNode.SetSynchronizationPeerConnected(FPeer,True);
    LStatus := FNode.GetSynchronizationStatus;
    Check('Reconnect cannot finish already buffered processing',LStatus.State = nssSynchronizing);
    CheckEqual(1,LStatus.ActiveTransfers);
    Check(FNode.IsUpdatingBlockChain);
    LOld.ExerciseRetiredScope.SetEvent;
    CheckEqual(Ord(wrSignaled),Ord(LOld.RetiredScopeChecked.WaitFor(3000)));
    LStatus := FNode.GetSynchronizationStatus;
    Check('Nested buffered work preserves synchronization',LStatus.State = nssSynchronizing);
    CheckEqual('The completed nested scope retains its outer processing scope',1,LStatus.ActiveTransfers);
    CheckFalse('Stale producers cannot restore a peer download target',LStatus.HasRemoteTarget);
    LNew := TSynchronizationScopeThread.Create(FNode,FPeer);
    LNew.Start;
    CheckEqual(Ord(wrSignaled),Ord(LNew.Entered.WaitFor(3000)));
    CheckEqual('Both physical processing scopes remain active',2,FNode.GetSynchronizationStatus.ActiveTransfers);
    LOld.ReleaseScope.SetEvent;
    CheckEqual(Ord(wrSignaled),Ord(LOld.Completed.WaitFor(3000)));
    CheckEqual('',LOld.ErrorMessage);
    LStatus := FNode.GetSynchronizationStatus;
    CheckEqual('An old End cannot release the new processing scope',1,LStatus.ActiveTransfers);
    Check(LStatus.State = nssSynchronizing);
    LNew.ReleaseScope.SetEvent;
    CheckEqual(Ord(wrSignaled),Ord(LNew.Completed.WaitFor(3000)));
    CheckEqual('',LNew.ErrorMessage);
    CheckEqual(0,FNode.GetSynchronizationStatus.ActiveTransfers);
    Check(FNode.GetSynchronizationStatus.State = nssIdle);
    CheckFalse(FNode.IsUpdatingBlockChain);
    WaitForState(nssIdle);
  finally
    LNew.Free;
    LOld.Free;
  end;
end;

procedure TNodeSynchronizationChecks.StorageFailureLatchesFirstCauseUntilRestart;
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
  Check(FNode.HasStorageFailure);
  Check(LStatus.State=nssStorageError);
  Check(LStatus.StorageFailure.Stage=sfsCheckpoint);
  CheckEqual('first-checkpoint.am_safebox',LStatus.StorageFailure.FileName);
  CheckEqual('EWriteError',LStatus.StorageFailure.ErrorClass);
  CheckEqual('owned first storage failure',LStatus.StorageFailure.ErrorMessage);
  CheckFalse(FNode.CanRequestBlockChainData);
  CheckFalse(FNode.PendingOperationsAvailable);
  CheckFalse(FNode.IsReady(LReason));
  CheckEqual('owned first storage failure',LReason);
  CheckFalse(FNode.IsBlockChainValid(LReason));
  CheckEqual('owned first storage failure',LReason);
  Check(FNode.AddOperations(nil,FOperations,nil,LReason)<0);
  CheckEqual('owned first storage failure',LReason);
  LBlock := TPCOperationsComp.Create(FNode.Bank);
  try
    CheckFalse(FNode.AddNewBlockChain(nil,LBlock,LReason));
    CheckEqual('owned first storage failure',LReason);
  finally
    LBlock.Free;
  end;
  CheckFalse(FNode.TryLockMempoolRead(0,LMempool));
  LRaised := False;
  try
    LMempool := FNode.LockMempoolWrite;
    FNode.UnlockMempoolWrite;
  except
    on E: ENodeStorageUnavailable do LRaised := True;
  end;
  Check('Storage failure prevents new operation writers',LRaised);
  CheckEqual(0,Int64(FNode.Bank.BlocksCount));
  WaitForState(nssStorageError);
end;

procedure TNodeSynchronizationChecks.StorageFailureNotificationNeverWaitsForUi;
var LThread: TStorageFailureThread; LBefore: Integer;
begin
  LBefore := FCallbacks;
  LThread := TStorageFailureThread.Create(FNode);
  try
    LThread.Start;
    // No CheckSynchronize here: a producer must finish even while its actual
    // main-thread subscriber cannot run. The event record owns copied strings.
    CheckEqual(Ord(wrSignaled),Ord(LThread.Completed.WaitFor(3000)));
    CheckEqual('',LThread.ErrorText);
    CheckEqual(LBefore,FCallbacks);
    Check(FNode.GetSynchronizationStatus.State=nssStorageError);
    WaitForState(nssStorageError);
    CheckEqual('owned injected disk full',FLast.StorageFailure.ErrorMessage);
  finally
    LThread.Free;
  end;
end;

procedure TNodeSynchronizationChecks.StorageFailureRetiresBufferedWorkSafely;
var LThread: TSynchronizationScopeThread; LError: Exception;
begin
  FNode.SetSynchronizationNetworkActive(True);
  FNode.SetSynchronizationPeerConnected(FPeer,True);
  LThread := TSynchronizationScopeThread.Create(FNode,FPeer);
  try
    LThread.Start;
    CheckEqual(Ord(wrSignaled),Ord(LThread.Entered.WaitFor(3000)));
    LError := EWriteError.Create('owned buffered storage failure');
    try
      FNode.ReportStorageFailure(sfsBlockStream,'owned.blocks',LError);
    finally
      LError.Free;
    end;
    CheckEqual(1,FNode.GetSynchronizationStatus.ActiveTransfers);
    Check('Failure has precedence while physical work retires',FNode.GetSynchronizationStatus.State=nssStorageError);
    CheckFalse(FNode.CanRequestBlockChainData);
    LThread.ReleaseScope.SetEvent;
    CheckEqual(Ord(wrSignaled),Ord(LThread.Completed.WaitFor(3000)));
    CheckEqual('',LThread.ErrorMessage);
    CheckEqual(0,FNode.GetSynchronizationStatus.ActiveTransfers);
    Check('Draining work never clears the storage failure',FNode.GetSynchronizationStatus.State=nssStorageError);
    WaitForState(nssStorageError);
  finally
    LThread.Free;
  end;
end;

procedure TNodeSynchronizationChecks.StorageFailurePrecedesBackendCallbacks;
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
  CheckEqual('Storage error is delivered before backend refresh requests',0,FTrace.IndexOf('sync'));
  Check(FLast.State=nssStorageError);
  CheckEqual(-1,FTrace.IndexOf('operations'));
end;

procedure TNodeSynchronizationChecks.NonStorageExceptionsDoNotLatchFailure;
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
    CheckFalse(FNode.HasStorageFailure);
    Check(FNode.GetSynchronizationStatus.State=nssOffline);
  end;
end;

procedure TNodeSynchronizationChecks.ActualCheckpointSaveFailureLatchesCanonicalNode;
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
  Check(ForceDirectories(LDestination));
  LFailed := False;
  try
    FNode.Bank.SaveBank(True);
  except
    on E: EOSError do LFailed := True;
  end;
  Check('Real atomic publication to a directory must fail',LFailed);
  LStatus := FNode.GetSynchronizationStatus;
  Check(LStatus.State=nssStorageError);
  Check(LStatus.StorageFailure.Stage=sfsCheckpoint);
  CheckEqual(LDestination,LStatus.StorageFailure.FileName);
  Check('The complete earlier checkpoint is preserved',StorageFixtureEquals(LBefore,ReadStorageFixture(LPrevious)));
  Check(DirectoryExists(LDestination));
  FNode.SetSynchronizationNetworkActive(False);
  FNode.SetSynchronizationNetworkActive(True);
  LIncoming := TPCOperationsComp.Create(FNode.Bank);
  try
    CheckFalse(FNode.AddNewBlockChain(nil,LIncoming,LReason));
    CheckFalse(FNode.Bank.AddNewBlockChainBlock(LIncoming,High(Cardinal),LReason));
  finally
    LIncoming.Free;
  end;
  CheckEqual(1,Int64(FNode.Bank.BlocksCount));
  CheckFalse(FNode.CanRequestBlockChainData);
  WaitForState(nssStorageError);
end;

procedure TNodeSynchronizationChecks.ActualFalseBlockSaveLatchesWithoutAcknowledging;
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
    Check(LCount>0);
    LFailed := False;
    try
      FNode.Bank.Storage.SaveBlockChainBlock(LIncoming);
    except
      on E: EWriteError do LFailed := True;
    end;
    Check('False storage acknowledgement must become a real write error',LFailed);
    CheckEqual(1,LStorage.SaveCalls);
    CheckEqual(LCount,LIncoming.Count);
    LStatus := FNode.GetSynchronizationStatus;
    Check(LStatus.State=nssStorageError);
    Check(LStatus.StorageFailure.Stage=sfsBlockStream);
    Check(Pos('BlockChainStream.blocks',LStatus.StorageFailure.FileName)>0);
    Check('Earlier checkpoint bytes remain intact',StorageFixtureEquals(LBefore,ReadStorageFixture(LPrevious)));
    FNode.SetSynchronizationNetworkActive(False);
    FNode.SetSynchronizationNetworkActive(True);
    CheckFalse(FNode.AddNewBlockChain(nil,LIncoming,LReason));
    CheckFalse(FNode.Bank.AddNewBlockChainBlock(LIncoming,High(Cardinal),LReason));
    CheckEqual(1,LStorage.SaveCalls);
    CheckEqual(LCount,LIncoming.Count);
    CheckEqual(0,Int64(FNode.Bank.BlocksCount));
    WaitForState(nssStorageError);
  finally
    LIncoming.Free;
  end;
end;

procedure TNodeSynchronizationChecks.SafeboxFlushFailureLatchesBeforeUiDelivery;
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
    on E: EAbort do CheckEqual('Synthetic flush cancellation',E.Message);
  end;
  CheckFalse('Cancellation cannot latch a storage error',FNode.HasStorageFailure);
  TStorageFlushNode(FNode).SafeboxFault := 2;
  try
    FNode.FlushSafebox;
    Fail('Synthetic safebox write error must propagate');
  except
    on E: EWriteError do CheckEqual('Synthetic safebox flush failure',E.Message);
  end;
  // No UI pump occurred between the actual flush boundary and these guards.
  LStatus := FNode.GetSynchronizationStatus;
  Check(LStatus.State=nssStorageError);
  Check(LStatus.StorageFailure.Stage=sfsSafeboxFlush);
  CheckEqual(LFileName,LStatus.StorageFailure.FileName);
  CheckFalse(FNode.CanRequestBlockChainData);
  Check('The pre-flush source bytes remain intact',StorageFixtureEquals(LBefore,ReadStorageFixture(LFileName)));
  LIncoming := TPCOperationsComp.Create(FNode.Bank);
  try
    CheckFalse(FNode.AddNewBlockChain(Nil,LIncoming,LReason));
    CheckFalse(FNode.Bank.AddNewBlockChainBlock(LIncoming,High(Cardinal),LReason));
  finally
    LIncoming.Free;
  end;
  TStorageFlushNode(FNode).SafeboxFault := 0;
  FNode.FlushSafebox;
  FNode.SetSynchronizationNetworkActive(True);
  CheckEqual('Synthetic safebox flush failure',FNode.GetSynchronizationStatus.StorageFailure.ErrorMessage);
  WaitForState(nssStorageError);
end;

procedure TNodeSynchronizationChecks.FalseBlockCacheFlushLatchesBeforeUiDelivery;
var LStatus: TNodeSynchronizationStatus; LReason: String;
  LIncoming: TPCOperationsComp;
begin
  FNode.Bank.StorageClass := TFileStorage;
  TStorageFlushNode(FNode).BlockFlushFails := True;
  try
    FNode.FlushBlockStorage;
    Fail('False block-cache flush must become a write error');
  except
    on E: EWriteError do Check(Pos('Cannot flush blockchain storage',E.Message)>0);
  end;
  LStatus := FNode.GetSynchronizationStatus;
  Check(LStatus.State=nssStorageError);
  Check(LStatus.StorageFailure.Stage=sfsBlockStream);
  CheckEqual('EWriteError',LStatus.StorageFailure.ErrorClass);
  CheckFalse(FNode.CanRequestBlockChainData);
  LIncoming := TPCOperationsComp.Create(FNode.Bank);
  try
    CheckFalse(FNode.AddNewBlockChain(Nil,LIncoming,LReason));
    CheckFalse(FNode.Bank.AddNewBlockChainBlock(LIncoming,High(Cardinal),LReason));
  finally
    LIncoming.Free;
  end;
  TStorageFlushNode(FNode).BlockFlushFails := False;
  FNode.FlushBlockStorage;
  FNode.SetSynchronizationNetworkActive(False);
  FNode.SetSynchronizationNetworkActive(True);
  CheckEqual(LStatus.StorageFailure.ErrorMessage,FNode.GetSynchronizationStatus.StorageFailure.ErrorMessage);
  WaitForState(nssStorageError);
end;

procedure TNodeSynchronizationChecks.FinalStatusSurvivesSubscriberException;
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
  CheckEqual('The expected subscriber failure was exercised exactly once',1,FThrownBlockCallbacks);
  Check('No additional producer event was needed to deliver final status',
    (FTrace.IndexOf('blocks') >= 0) and (FTrace.IndexOf('sync') > FTrace.IndexOf('blocks')));
  CheckEqual(0,FNode.GetSynchronizationStatus.ActiveTransfers);
end;

procedure TNodeSynchronizationChecks.Check(AValue: Boolean);
begin
  Check('Synchronization assertion failed', AValue);
end;

procedure TNodeSynchronizationChecks.Check(const AMessage: String; AValue: Boolean);
begin
  if not AValue then Fail(AMessage);
end;

procedure TNodeSynchronizationChecks.CheckFalse(AValue: Boolean);
begin
  Check(not AValue);
end;

procedure TNodeSynchronizationChecks.CheckFalse(const AMessage: String; AValue: Boolean);
begin
  Check(AMessage, not AValue);
end;

procedure TNodeSynchronizationChecks.CheckEqual(AExpected, AActual: Int64);
begin
  CheckEqual('Synchronization values differ', AExpected, AActual);
end;

procedure TNodeSynchronizationChecks.CheckEqual(const AMessage: String;
  AExpected, AActual: Int64);
begin
  Check(Format('%s: expected %d, got %d', [AMessage, AExpected, AActual]), AExpected = AActual);
end;

procedure TNodeSynchronizationChecks.CheckEqual(const AExpected, AActual: String);
begin
  Check('Expected "' + AExpected + '", got "' + AActual + '"', AExpected = AActual);
end;

procedure TNodeSynchronizationChecks.Fail(const AMessage: String);
begin
  raise Exception.Create(AMessage);
end;

function RunNodeSynchronizationChecks(const ALogPath: String): Integer;
const CNames: array[0..16] of String = (
  'Selected target survives 6200 ms between batches',
  'Network off keeps synchronization until buffered work drains',
  'Last disconnect clears targets and waits for buffered work',
  'Synchronization pause precedes all backend inputs',
  'Offline resume follows blocks, operations, messages and keys',
  'Actual write summary completes the selected target',
  'Buffered work survives reconnect until every physical scope drains',
  'Final status survives a failing backend subscriber without new input',
  'Storage failure retains first cause and rejects admission until restart',
  'Storage failure producer never waits for UI delivery',
  'Storage failure lets owned buffered scopes retire without resuming',
  'Storage failure precedes every backend refresh callback',
  'Cancellation exhaustion and programmer faults are not storage errors',
  'Actual canonical checkpoint publication failure latches until restart',
  'False actual block storage acknowledgement latches without clearing operations',
  'Post-commit safebox flush failure latches before UI delivery',
  'False post-batch block-cache flush latches before UI delivery');
var LLog: TStringList; LChecks: TNodeSynchronizationChecks;
  I: Integer; LStarted: UInt64;
begin
  Result := 1;
  LLog := TStringList.Create;
  try
    try
      for I := Low(CNames) to High(CNames) do begin
        LLog.Add('START ' + CNames[I]);
        LLog.SaveToFile(ALogPath);
        LStarted := GetTickCount64;
        LChecks := TNodeSynchronizationChecks.Create;
        try
          try
            LChecks.SetUp;
            case I of
              0: LChecks.SelectedTargetSurvivesInterBatchSilence;
              1: LChecks.NetworkOffWaitsForBufferedWork;
              2: LChecks.DisconnectClearsTargetsBeforeReconnect;
              3: LChecks.SynchronizingCallbackPrecedesBackendInputs;
              4: LChecks.OfflineCallbackFollowsEveryBackendInput;
              5: LChecks.ActualWriteSummaryCompletesSelectedTarget;
              6: LChecks.BufferedWorkSurvivesReconnectUntilEveryScopeDrains;
              7: LChecks.FinalStatusSurvivesSubscriberException;
              8: LChecks.StorageFailureLatchesFirstCauseUntilRestart;
              9: LChecks.StorageFailureNotificationNeverWaitsForUi;
              10: LChecks.StorageFailureRetiresBufferedWorkSafely;
              11: LChecks.StorageFailurePrecedesBackendCallbacks;
              12: LChecks.NonStorageExceptionsDoNotLatchFailure;
              13: LChecks.ActualCheckpointSaveFailureLatchesCanonicalNode;
              14: LChecks.ActualFalseBlockSaveLatchesWithoutAcknowledging;
              15: LChecks.SafeboxFlushFailureLatchesBeforeUiDelivery;
              16: LChecks.FalseBlockCacheFlushLatchesBeforeUiDelivery;
            end;
          finally
            LChecks.TearDown;
          end;
        finally
          LChecks.Free;
        end;
        LLog.Add(Format('PASS %s (%d ms)', [CNames[I], GetTickCount64 - LStarted]));
        LLog.SaveToFile(ALogPath);
      end;
      LLog.Add('PASS all seventeen actual core synchronization lifecycle checks');
      Result := 0;
    except
      on E: Exception do LLog.Add('FAIL ' + E.ClassName + ': ' + E.Message);
    end;
    LLog.SaveToFile(ALogPath);
  finally
    LLog.Free;
  end;
end;

end.
