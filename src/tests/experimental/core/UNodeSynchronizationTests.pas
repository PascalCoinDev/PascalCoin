unit UNodeSynchronizationTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Deterministic checks of the actual node synchronization facts and notifier.
  Generated empty-node data and owned peer identities only; no sockets or keys
  from an existing wallet are used. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses Classes, SysUtils, fpcunit, testregistry, UNode, UAccounts, UBlockChain,
  UCrypto, UNetProtocol;

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
  FNode := TNode.Create(nil);
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
