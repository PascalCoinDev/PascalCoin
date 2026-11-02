unit UPendingCatchupChecks;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Real pending validation, bank subscribers and the owned maintenance worker.
  Generated commits below deliberately do not assert proof-of-work acceptance;
  the separate public five-block fixture exercises that boundary. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

type
  TPendingCatchupCheck = (pccBufferedCommits, pccInterBatchTarget,
    pccMixedRejections, pccUnavailableContracts, pccNewWorkDuringReplay,
    pccRetiredScopes, pccShutdownPackage, pccEmptyScope,
    pccReplayFailureRetry, pccReplayFailureShutdown);

function RunPendingCatchupCheck(ACheck: TPendingCatchupCheck;
  const ALogPath: String): Integer;
function RunPendingCatchupChecks(const ALogPath: String): Integer;

implementation

uses {$IFDEF FPC}Windows,{$ELSE}Winapi.Windows,{$ENDIF}
  Classes, SysUtils, SyncObjs, UNode, UAccounts, UBlockChain, UFileStorage,
  UOpTransaction, UThread, UCrypto, UConst, UBaseTypes, UPCDataTypes, UNetProtocol;

type
  TCatchupSafeBox = class(TPCSafeBox)
    procedure Build(const AKey: TAccountKey);
  end;

  TCatchupFileStorage = class(TFileStorage)
  protected
    function DoLoadBlockChain(AOperations: TPCOperationsComp; ABlock: Cardinal): Boolean; override;
    function GetFirstBlockNumber: Int64; override;
    function GetLastBlockNumber: Int64; override;
  end;

  TReplayObservation = class
  public
    Transaction: TPCSafeBoxTransaction;
    Attempts, HoldNext, ThrowOnAttempt: LongInt;
    Entered, ReleaseReplay, ThrowReached: TEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    function Count: Integer;
  end;

  TCountingTransaction = class(TOpTransaction)
    function DoOperation(APrevious: TAccountPreviousBlockInfo;
      ATransaction: TPCSafeBoxTransaction; var AErrors: String): Boolean; override;
  end;

  TCatchupScopeThread = class(TPCThread)
  private
    FNode: TNode;
    FPeer: TObject;
  protected
    procedure BCExecute; override;
  public
    Entered, ReleaseScope, Completed: TEvent;
    StaleTargetAtExit: Boolean;
    RequireUnavailableRead, ReadWasUnavailable: Boolean;
    ErrorMessage: String;
    constructor Create(ANode: TNode; APeer: TObject);
    destructor Destroy; override;
  end;

  TPendingCatchupChecks = class
  private
    FNode: TNode;
    FKey: TECPrivateKey;
    FPeer, FSecondPeer: TObject;
    FNotify: TNodeNotifyEvents;
    FObservation: TReplayObservation;
    FSeeds: TOperationsHashTree;
    FTemplateHeader: TOperationBlock;
    FComplete: TEvent;
    FLog: TStringList;
    FPath, FDirectory, FPreviousDirectory: String;
    FStopping, FDirectorySet: Boolean;
    FAvailableCallbacks: Integer;
    FCallbackThread: UInt64;
    procedure Check(AValue: Boolean; const AMessage: String);
    procedure Log(const AMessage: String);
    procedure SynchronizationChanged(Sender: TObject; const AStatus: TNodeSynchronizationStatus);
    procedure Pump(AMilliseconds: Cardinal);
    procedure WaitAvailable(ATimeout: Cardinal = 3000);
    procedure Seed;
    procedure StartSession(ATargetOffset: Cardinal);
    procedure CommitGenerated(AConfirmFirst: Boolean = False);
    procedure CheckUnavailable;
    procedure CheckPool(ACount: Integer);
    function PendingBytes: TMemoryStream;
    function OracleBytes(AExpectedCount: Integer = 2): TMemoryStream;
    procedure CheckRetainedPackage(AExpected: TMemoryStream);
    procedure CheckFinalAccounts(AExpectedCount: Integer);
    procedure BufferedCommits;
    procedure InterBatchTarget;
    procedure MixedRejections;
    procedure UnavailableContracts;
    procedure NewWorkDuringReplay;
    procedure RetiredScopes;
    procedure ShutdownPackage;
    procedure EmptyScope;
    procedure ReplayFailureRetry;
    procedure ReplayFailureShutdown;
  public
    constructor Create(const APath: String);
    destructor Destroy; override;
    procedure Run(ACheck: TPendingCatchupCheck);
  end;

var GReplayObservation: TReplayObservation;

constructor TReplayObservation.Create;
begin
  inherited Create;
  Entered := TEvent.Create(nil,True,False,'');
  ReleaseReplay := TEvent.Create(nil,True,False,'');
  ThrowReached := TEvent.Create(nil,True,False,'');
end;

destructor TReplayObservation.Destroy;
begin
  ThrowReached.Free;
  ReleaseReplay.Free;
  Entered.Free;
  inherited;
end;

procedure TReplayObservation.Reset;
begin
  InterlockedExchange(Attempts,0);
end;

function TReplayObservation.Count: Integer;
begin
  Result := InterlockedCompareExchange(Attempts,0,0);
end;

function TCountingTransaction.DoOperation(APrevious: TAccountPreviousBlockInfo;
  ATransaction: TPCSafeBoxTransaction; var AErrors: String): Boolean;
var LAttempt: LongInt;
begin
  // Count the inherited validator only for the real node's pending overlay.
  // An independent confirmed transaction/oracle must not inflate this count.
  if Assigned(GReplayObservation) and
    (ATransaction=GReplayObservation.Transaction) then begin
    LAttempt := InterlockedIncrement(GReplayObservation.Attempts);
    if InterlockedCompareExchange(GReplayObservation.HoldNext,0,1)=1 then begin
      GReplayObservation.Entered.SetEvent;
      if GReplayObservation.ReleaseReplay.WaitFor(5000)<>wrSignaled then
        raise Exception.Create('Pending replay fixture gate timed out');
    end;
    if InterlockedCompareExchange(GReplayObservation.ThrowOnAttempt,0,LAttempt)=LAttempt then begin
      GReplayObservation.ThrowReached.SetEvent;
      raise Exception.Create('Owned one-shot pending replay failure after an accepted prefix');
    end;
  end;
  Result := inherited DoOperation(APrevious,ATransaction,AErrors);
end;

constructor TCatchupScopeThread.Create(ANode: TNode; APeer: TObject);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
  FPeer := APeer;
  Entered := TEvent.Create(nil,True,False,'');
  ReleaseScope := TEvent.Create(nil,True,False,'');
  Completed := TEvent.Create(nil,True,False,'');
end;

destructor TCatchupScopeThread.Destroy;
begin
  Terminate;
  ReleaseScope.SetEvent;
  if Suspended then Start;
  WaitFor;
  Completed.Free;
  ReleaseScope.Free;
  Entered.Free;
  inherited;
end;

procedure TCatchupScopeThread.BCExecute;
var LPool: TPCOperationsComp;
begin
  try
    try
      if Terminated then Exit;
      if RequireUnavailableRead then begin
        try
          LPool := FNode.LockMempoolRead;
          FNode.UnlockMempoolRead;
        except on EPendingOperationsUnavailable do ReadWasUnavailable:=True; end;
        if not ReadWasUnavailable then
          raise Exception.Create('A busy reader obtained the pending account overlay');
      end;
      FNode.BeginBlockChainUpdate;
      try
        Entered.SetEvent;
        if ReleaseScope.WaitFor(5000)<>wrSignaled then
          raise Exception.Create('Pending physical-scope fixture timed out');
        if StaleTargetAtExit then FNode.SetSynchronizationTarget(FPeer,999999);
      finally FNode.EndBlockChainUpdate; end;
    except on E: Exception do ErrorMessage := E.ClassName+': '+E.Message; end;
  finally Completed.SetEvent; end;
end;

procedure TCatchupSafeBox.Build(const AKey: TAccountKey);
var I: Integer; LHeader: TOperationBlock;
begin
  for I := 0 to 159 do begin
    LHeader := CT_OperationBlock_NUL;
    LHeader.block := I;
    LHeader.account_key := AKey;
    LHeader.protocol_version := CT_PROTOCOL_1;
    LHeader.protocol_available := CT_PROTOCOL_6;
    LHeader.compact_target := CT_MinCompactTarget_v1;
    LHeader.timestamp := I*CT_NewLineSecondsAvg;
    // Use the normal mining reward so the first account of each generated
    // block is funded without creating an unbalanced transaction fee delta.
    LHeader.reward := TPascalCoinProtocol.GetRewardForNewLine(I);
    AddNew(LHeader);
  end;
  DoUpgradeToProtocol5;
end;

function TCatchupFileStorage.DoLoadBlockChain(AOperations: TPCOperationsComp;
  ABlock: Cardinal): Boolean;
begin
  if ABlock=Bank.BlocksCount-1 then begin
    AOperations.Clear(True);
    AOperations.SetOperationBlock(Bank.SafeBox.GetBlockInfo(ABlock));
    Result := True;
  end else Result := inherited DoLoadBlockChain(AOperations,ABlock);
end;

function TCatchupFileStorage.GetFirstBlockNumber: Int64;
begin
  Result := 0;
end;

function TCatchupFileStorage.GetLastBlockNumber: Int64;
begin
  Result := Int64(Bank.BlocksCount)-1;
end;

constructor TPendingCatchupChecks.Create(const APath: String);
var LID: TGUID; LPool: TPCOperationsComp;
begin
  inherited Create;
  FPath := APath;
  FLog := TStringList.Create;
  Check(not TNode.NodeExists,'Each catch-up scenario requires an owned fresh node');
  Check(GetEnvironmentVariable('TEMP')<>'','TEMP must name an owned test directory');
  CreateGUID(LID);
  FDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))+
    'PendingCatchup-'+GUIDToString(LID);
  ForceDirectories(FDirectory);
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(FDirectory);
  FDirectorySet := True;
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FPeer := TObject.Create;
  FSecondPeer := TObject.Create;
  FComplete := TEvent.Create(nil,True,False,'');
  FObservation := TReplayObservation.Create;
  GReplayObservation := FObservation;
  FSeeds := TOperationsHashTree.Create;
  FNode := TNode.Create(nil);
  FNode.BroadcastData := False;
  TNetData.NetData.NetConnectionsActive := False;
  FNode.SetSynchronizationNetworkActive(False);
  Check(not FNode.NetServer.Active,'The catch-up fixture must not listen');
  FNode.Bank.StorageClass := TCatchupFileStorage;
  Check(FNode.Bank.Storage.Initialize,'Generated fixture storage initialization');
  TCatchupSafeBox(FNode.Bank.SafeBox).Build(FKey.PublicKey);
  FNode.Bank.UpdateValuesFromSafebox;
  LPool := FNode.LockMempoolRead;
  try
    FObservation.Transaction := LPool.SafeBoxTransaction;
    FTemplateHeader := LPool.OperationBlock;
  finally FNode.UnlockMempoolRead; end;
  FNotify := TNodeNotifyEvents.Create(nil);
  FNotify.OnSynchronizationChanged := SynchronizationChanged;
  FNotify.Node := FNode;
  Pump(100);
  FComplete.ResetEvent;
  FAvailableCallbacks := 0;
  Log('READY generated bank; owned data='+FDirectory);
end;

destructor TPendingCatchupChecks.Destroy;
begin
  FStopping := True;
  if Assigned(FObservation) then FObservation.ReleaseReplay.SetEvent;
  if Assigned(FNotify) then FNotify.Node := nil;
  FNotify.Free;
  FNode.Free;
  FSeeds.Free;
  GReplayObservation := nil;
  FObservation.Free;
  FComplete.Free;
  FSecondPeer.Free;
  FPeer.Free;
  FKey.Free;
  if FDirectorySet then TNode.SetPascalCoinDataFolder(FPreviousDirectory);
  FLog.Free;
  inherited;
end;

procedure TPendingCatchupChecks.Check(AValue: Boolean; const AMessage: String);
begin
  if not AValue then raise Exception.Create(AMessage);
end;

procedure TPendingCatchupChecks.Log(const AMessage: String);
begin
  FLog.Add(FormatDateTime('hh:nn:ss.zzz',Now)+' '+AMessage);
  FLog.SaveToFile(FPath);
end;

procedure TPendingCatchupChecks.SynchronizationChanged(Sender: TObject;
  const AStatus: TNodeSynchronizationStatus);
begin
  if FStopping then Exit;
  FCallbackThread := TThread.CurrentThread.ThreadID;
  if AStatus.PendingOperationsAvailable then begin
    Inc(FAvailableCallbacks);
    FComplete.SetEvent;
  end;
end;

procedure TPendingCatchupChecks.Pump(AMilliseconds: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64+AMilliseconds;
  repeat CheckSynchronize(0); Sleep(1); until GetTickCount64>=LUntil;
end;

procedure TPendingCatchupChecks.WaitAvailable(ATimeout: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64+ATimeout;
  repeat
    CheckSynchronize(0);
    if FComplete.WaitFor(0)=wrSignaled then begin
      Check(FNode.PendingOperationsAvailable,'Completion event must publish available pending data');
      Check(FNode.GetSynchronizationStatus.PendingOperationsAvailable,'Cached status must agree with availability');
      Check(FCallbackThread=MainThreadID,'Typed completion must use the normal UI notifier');
      Exit;
    end;
    Sleep(1);
  until GetTickCount64>=LUntil;
  raise Exception.Create('No event-driven pending completion within the owned deadline');
end;

procedure TPendingCatchupChecks.Seed;
var LOp: TPCOperation; LError: String; I: Integer; LSigner,LNonce: Cardinal;
  LAdded: Boolean;
begin
  for I := 0 to 2 do begin
    if I<2 then begin
      LSigner:=0;
      LNonce:=I+1;
    end
    else begin
      LSigner:=5;
      LNonce:=1;
    end;
    LOp := TCountingTransaction.CreateTransaction(CT_PROTOCOL_5,LSigner,LNonce,1,
      FKey,1,1,CT_TOperationPayload_NUL);
    try
      Check(FSeeds.AddOperationToHashTree(LOp),'Register the real signed seed');
      LAdded := FNode.AddOperation(nil,FSeeds.GetOperation(I),LError);
      Check(LAdded,'Real pending admission '+IntToStr(I)+': '+LError);
    finally LOp.Free; end;
  end;
  CheckPool(3);
  FObservation.Reset;
end;

procedure TPendingCatchupChecks.StartSession(ATargetOffset: Cardinal);
begin
  FNode.SetSynchronizationNetworkActive(True);
  FNode.SetSynchronizationPeerConnected(FPeer,True);
  FNode.SetSynchronizationTarget(FPeer,FNode.Bank.BlocksCount+ATargetOffset);
  Pump(20);
  FComplete.ResetEvent;
  FAvailableCallbacks := 0;
end;

procedure TPendingCatchupChecks.CommitGenerated(AConfirmFirst: Boolean);
var LTransaction: TPCSafeBoxTransaction; LPrevious: TAccountPreviousBlockInfo;
  LSelected: TOperationsHashTree; LHeader: TOperationBlock; LError: String;
begin
  Check(FNode.TryLockNode(3000),'Confirmed bank write admission');
  try
    LTransaction := TPCSafeBoxTransaction.Create(FNode.Bank.SafeBox);
    LPrevious := TAccountPreviousBlockInfo.Create;
    LSelected := TOperationsHashTree.Create;
    try
      if AConfirmFirst then begin
        Check(FSeeds.GetOperation(0).DoOperation(LPrevious,LTransaction,LError),
          'Actual confirmed transaction execution: '+LError);
        Check(LSelected.AddOperationToHashTree(FSeeds.GetOperation(0)),'Register actual confirmed operation');
      end;
      LHeader := FTemplateHeader;
      LHeader.block := FNode.Bank.BlocksCount;
      LHeader.account_key := FKey.PublicKey;
      LHeader.timestamp := FNode.Bank.LastOperationBlock.timestamp+1;
      LHeader.initial_safe_box_hash := FNode.Bank.SafeBox.SafeBoxHash;
      LHeader.previous_proof_of_work := FNode.Bank.LastOperationBlock.proof_of_work;
      LHeader.fee := LTransaction.TotalFee;
      LHeader.operations_hash := LSelected.HashTree;
      SetLength(LHeader.proof_of_work,32);
      FillChar(LHeader.proof_of_work[0],32,0);
      Check(LTransaction.Commit(LHeader,LError),'Generated confirmed commit: '+LError);
      // This invokes the real node bank subscriber. No sanitizer/status event
      // is called by the fixture, and ordinary block validation is not replaced.
      FNode.Bank.UpdateValuesFromSafebox;
    finally LSelected.Free; LPrevious.Free; LTransaction.Free; end;
  finally FNode.UnlockNode; end;
end;

procedure TPendingCatchupChecks.CheckUnavailable;
var LStatus: TNodeSynchronizationStatus;
begin
  LStatus := FNode.GetSynchronizationStatus;
  Check(not FNode.PendingOperationsAvailable,'Pending overlay must remain unavailable');
  Check(not LStatus.PendingOperationsAvailable,'Typed status must remain unavailable');
  Check(LStatus.State=nssSynchronizing,'Unavailable overlay is synchronization work');
end;

procedure TPendingCatchupChecks.CheckPool(ACount: Integer);
var LPool: TPCOperationsComp;
begin
  LPool := FNode.LockMempoolRead;
  try
    Check(LPool.Count=ACount,'Exact pending operation count');
    Check(LPool.OperationBlock.block=FNode.Bank.BlocksCount,'Pending header uses the current confirmed tip');
    Check(LPool.SafeBoxTransaction.CheckIntegrity,'Pending overlay integrity matches the current confirmed base');
  finally FNode.UnlockMempoolRead; end;
end;

function TPendingCatchupChecks.PendingBytes: TMemoryStream;
var LPool: TPCOperationsComp;
begin
  Result := TMemoryStream.Create;
  try
    LPool := FNode.LockMempoolRead;
    try Check(LPool.OperationsHashTree.SaveOperationsHashTreeToStream(Result,True),'Serialize coherent pending package');
    finally FNode.UnlockMempoolRead; end;
  except Result.Free; raise; end;
end;

function TPendingCatchupChecks.OracleBytes(AExpectedCount: Integer): TMemoryStream;
var LPool: TPCOperationsComp; LError: String;
begin
  Result := TMemoryStream.Create;
  try
    LPool := TPCOperationsComp.Create(FNode.Bank);
    try
      Check(LPool.AddOperations(FSeeds,LError)=AExpectedCount,'Independent normal insertion accepted count');
      Check(LPool.OperationsHashTree.SaveOperationsHashTreeToStream(Result,True),'Serialize normal-insertion oracle');
    finally LPool.Free; end;
  except Result.Free; raise; end;
end;

procedure TPendingCatchupChecks.CheckRetainedPackage(AExpected: TMemoryStream);
var LPool: TPCOperationsComp; LActual: TMemoryStream; I: Integer;
begin
  Check(FNode.TryLockMempoolOperationsRead(3000,LPool),'Failed replay must release its writer');
  try
    Check(LPool.Count=FSeeds.OperationsCount,'A failed replay retains every original pending operation');
    for I := 0 to FSeeds.OperationsCount-1 do begin
      Check(LPool.Operation[I]=FSeeds.GetOperation(I),'A failed replay retains original registered order');
      Check(LPool.OperationsHashTree.IndexOfOperation(FSeeds.GetOperation(I))=I,'A failed replay preserves its SHA index');
      Check(LPool.OperationsHashTree.IndexOfOpReference(FSeeds.GetOperation(I).GetOpReference)=I,
        'A failed replay preserves its reference index');
    end;
    LActual := TMemoryStream.Create;
    try
      Check(LPool.OperationsHashTree.SaveOperationsHashTreeToStream(LActual,True),'Serialize complete retained input');
      Check((AExpected.Size=LActual.Size) and CompareMem(AExpected.Memory,LActual.Memory,AExpected.Size),
        'A failed replay must not publish the accepted prefix as a smaller pending package');
    finally LActual.Free; end;
  finally FNode.UnlockMempoolRead; end;
end;

procedure TPendingCatchupChecks.CheckFinalAccounts(AExpectedCount: Integer);
var LPool,LOracle: TPCOperationsComp; LError: String; I: Integer;
const CAccounts: array[0..2] of Cardinal = (0,1,5);
begin
  LOracle := TPCOperationsComp.Create(FNode.Bank);
  try
    Check(LOracle.AddOperations(FSeeds,LError)=AExpectedCount,'Independent complete account replay count');
    LPool := FNode.LockMempoolRead;
    try
      for I := Low(CAccounts) to High(CAccounts) do
        Check(TAccountComp.EqualAccounts(LOracle.SafeBoxTransaction.Account(CAccounts[I]),
          LPool.SafeBoxTransaction.Account(CAccounts[I])),'Final account, nonce and seal match independent normal replay');
      Check(LOracle.SafeBoxTransaction.TotalBalance=LPool.SafeBoxTransaction.TotalBalance,'Final aggregate balance');
      Check(LOracle.SafeBoxTransaction.TotalFee=LPool.SafeBoxTransaction.TotalFee,'Final aggregate fee');
    finally FNode.UnlockMempoolRead; end;
  finally LOracle.Free; end;
end;

procedure TPendingCatchupChecks.BufferedCommits;
var I: Integer; LInitial: Cardinal;
begin
  Seed;
  LInitial := FNode.Bank.BlocksCount;
  StartSession(3);
  FNode.BeginBlockChainUpdate;
  try
    for I := 1 to 3 do begin
      CommitGenerated;
      CheckUnavailable;
      Check(FNode.GetSynchronizationStatus.LocalBlockCount=LInitial+Cardinal(I),
        'Confirmed counts must advance while pending replay is deferred');
      Check(FObservation.Count=0,'A received block must not replay pending operations');
    end;
  finally FNode.EndBlockChainUpdate; end;
  WaitAvailable;
  Check(FObservation.Count=3,'The final worker must replay the three input operations once');
  CheckPool(3);
end;

procedure TPendingCatchupChecks.InterBatchTarget;
begin
  Seed;
  StartSession(2);
  FNode.BeginBlockChainUpdate;
  try CommitGenerated; finally FNode.EndBlockChainUpdate; end;
  Pump(200);
  CheckUnavailable;
  Check(FObservation.Count=0,'A target ahead must defer replay across a silent inter-batch gap');
  Check(FNode.GetSynchronizationStatus.ActiveTransfers=0,'This fixture really exercises an empty physical gap');
  FNode.BeginBlockChainUpdate;
  try CommitGenerated; finally FNode.EndBlockChainUpdate; end;
  WaitAvailable;
  Check(FObservation.Count=3,'Reaching the selected target admits one final replay');
  CheckPool(3);
end;

procedure TPendingCatchupChecks.MixedRejections;
var LExpected,LActual: TMemoryStream; LPool: TPCOperationsComp;
begin
  Seed;
  StartSession(2);
  FNode.BeginBlockChainUpdate;
  try
    CommitGenerated(True);
    CommitGenerated;
    Check(FObservation.Count=0,'Confirmed execution must not be counted as pending replay');
  finally FNode.EndBlockChainUpdate; end;
  WaitAvailable;
  Check(FObservation.Count=3,'One final full input pass includes the rejected first nonce');
  CheckPool(2);
  CheckFinalAccounts(2);
  LExpected := OracleBytes;
  LActual := PendingBytes;
  try
    Check((LExpected.Size=LActual.Size) and CompareMem(LExpected.Memory,LActual.Memory,LExpected.Size),
      'Final accepted order/hash/fees/storage bytes equal independent normal insertion');
  finally LActual.Free; LExpected.Free; end;
  LPool := FNode.LockMempoolRead;
  try
    Check(LPool.Operation[0]=FSeeds.GetOperation(1),'The second nonce remains first after confirmation');
    Check(LPool.Operation[1]=FSeeds.GetOperation(2),'The independent signer remains second');
    Check(LPool.OperationsHashTree.IndexOfOperation(FSeeds.GetOperation(0))=-1,'Confirmed SHA is removed');
    Check(LPool.OperationsHashTree.IndexOfOperation(FSeeds.GetOperation(1))=0,'Remaining SHA index is exact');
    Check(LPool.OperationsHashTree.IndexOfOpReference(FSeeds.GetOperation(2).GetOpReference)=1,'Remaining reference index is exact');
    Check(LPool.SafeBoxTransaction.TotalFee=2,'Final pending fees exclude the confirmed operation');
  finally FNode.UnlockMempoolRead; end;
end;

procedure TPendingCatchupChecks.UnavailableContracts;
var LPool: TPCOperationsComp; LOp: TPCOperation; LTree: TOperationsHashTree;
  LError: String; I: Integer; LRaised: Boolean;
begin
  Seed;
  StartSession(1);
  FNode.BeginBlockChainUpdate;
  try
    CommitGenerated;
    CheckUnavailable;
    LPool := nil;
    Check(not FNode.TryLockMempoolRead(0,LPool),'TryRead must decline unavailable balances');
    Check(LPool=nil,'A failed TryRead must not leak the old overlay');
    for I := 0 to 2 do begin
      LRaised := False;
      try
        case I of
          0: begin
            LPool:=FNode.LockMempoolRead;
            FNode.UnlockMempoolRead;
          end;
          1: begin
            LPool:=FNode.LockMempoolWrite;
            FNode.UnlockMempoolWrite;
          end;
          2: begin
            LPool:=FNode.LockMempoolUpgradeableRead;
            FNode.UnlockMempoolUpgradeableRead;
          end;
        end;
      except on EPendingOperationsUnavailable do LRaised:=True; end;
      Check(LRaised,'Each public pending scope must report typed unavailability');
    end;
    Check(FNode.TryLockMempoolOperationsRead(0,LPool),'Raw operation metadata remains accessible without account deltas');
    try Check(LPool.Count=3,'Busy reads must not erase the retained operation sequence');
    finally FNode.UnlockMempoolRead; end;
    LOp := TCountingTransaction.CreateTransaction(CT_PROTOCOL_5,0,3,1,FKey,1,1,CT_TOperationPayload_NUL);
    LTree := TOperationsHashTree.Create;
    try
      Check(LTree.AddOperationToHashTree(LOp),'Register valid new operation for admission refusal');
      Check(FNode.AddOperations(nil,LTree,nil,LError)<0,'Batch admission must return explicit unavailable result');
      Check(LError<>'','Unavailable admission supplies a reason');
      Check(not FNode.AddOperation(nil,LOp,LError),'Single operation admission must also decline');
      Check(FObservation.Count=0,'Unavailable admission must not execute pending validation');
    finally LTree.Free; LOp.Free; end;
  finally FNode.EndBlockChainUpdate; end;
  WaitAvailable;
  CheckPool(3);
  LOp := TCountingTransaction.CreateTransaction(CT_PROTOCOL_5,0,3,1,FKey,1,1,CT_TOperationPayload_NUL);
  try Check(FNode.AddOperation(nil,LOp,LError),'Normal admission resumes against the reconciled overlay: '+LError);
  finally LOp.Free; end;
  CheckPool(4);
end;

procedure TPendingCatchupChecks.NewWorkDuringReplay;
var LNew: TCatchupScopeThread; LPool: TPCOperationsComp;
begin
  Seed;
  StartSession(1);
  FNode.BeginBlockChainUpdate;
  try CommitGenerated; finally
    InterlockedExchange(FObservation.HoldNext,1);
    FNode.EndBlockChainUpdate;
  end;
  Check(FObservation.Entered.WaitFor(3000)=wrSignaled,'The actual final maintenance replay must reach the gate');
  LNew := TCatchupScopeThread.Create(FNode,FPeer);
  try
    LNew.RequireUnavailableRead := True;
    LNew.Start;
    try
      Check(LNew.Entered.WaitFor(1000)=wrSignaled,'A new physical producer must not wait for pending replay');
      Check(LNew.ReadWasUnavailable,'A busy reader must fail before waiting on the held replay writer');
      CheckUnavailable;
    finally FObservation.ReleaseReplay.SetEvent; end;
    // A raw read waits for that first complete replay, without exposing its
    // account overlay while the new physical scope is still active.
    Check(FNode.TryLockMempoolOperationsRead(3000,LPool),'The first complete replay releases its writer');
    FNode.UnlockMempoolRead;
    Pump(100);
    CheckUnavailable;
    Check(FAvailableCallbacks=0,'A new physical scope prevents a premature completion callback');
    Check(FObservation.Count=3,'Only the first final replay has run');
    CommitGenerated;
    Check(FObservation.Count=3,'New confirmed work is deferred again');
    LNew.ReleaseScope.SetEvent;
    Check(LNew.Completed.WaitFor(3000)=wrSignaled,'The new physical scope must drain');
    Check(LNew.ErrorMessage='','The new producer must complete normally: '+LNew.ErrorMessage);
    WaitAvailable;
    Check(FObservation.Count=6,'The new confirmed base receives its own single final replay');
    CheckPool(3);
  finally FObservation.ReleaseReplay.SetEvent; LNew.Free; end;
end;

procedure TPendingCatchupChecks.RetiredScopes;
var LOld,LNew: TCatchupScopeThread;
begin
  Seed;
  StartSession(2);
  LOld := TCatchupScopeThread.Create(FNode,FPeer);
  LNew := nil;
  try
    LOld.StaleTargetAtExit := True;
    LOld.Start;
    Check(LOld.Entered.WaitFor(3000)=wrSignaled,'Old physical scope entered');
    CommitGenerated;
    FNode.SetSynchronizationNetworkActive(False);
    CheckUnavailable;
    Check(FObservation.Count=0,'Network-off must retain the actual buffered scope without replaying');
    FNode.SetSynchronizationNetworkActive(True);
    FNode.SetSynchronizationPeerConnected(FSecondPeer,True);
    LNew := TCatchupScopeThread.Create(FNode,FSecondPeer);
    LNew.Start;
    Check(LNew.Entered.WaitFor(3000)=wrSignaled,'Reconnected physical scope entered');
    LOld.ReleaseScope.SetEvent;
    Check(LOld.Completed.WaitFor(3000)=wrSignaled,'Old scope drained');
    Check(LOld.ErrorMessage='','Old producer completed: '+LOld.ErrorMessage);
    Check(FNode.GetSynchronizationStatus.ActiveTransfers=1,'Old completion must not drain the new physical owner');
    Check(not FNode.GetSynchronizationStatus.HasRemoteTarget,'Stale producer cannot revive its selected target');
    CheckUnavailable;
    Check(FObservation.Count=0,'An old completion cannot trigger a replay under new physical work');
    LNew.ReleaseScope.SetEvent;
    Check(LNew.Completed.WaitFor(3000)=wrSignaled,'New scope drained');
    Check(LNew.ErrorMessage='','New producer completed: '+LNew.ErrorMessage);
    WaitAvailable;
    Check(FObservation.Count=3,'Both generations drain before one coherent replay');
    CheckPool(3);
  finally LNew.Free; LOld.Free; end;
end;

procedure TPendingCatchupChecks.ShutdownPackage;
var LBefore,LExpected,LActual: TMemoryStream; LMatchesBefore,LMatchesFinal: Boolean;
begin
  Seed;
  LBefore := PendingBytes;
  LExpected := nil;
  LActual := TMemoryStream.Create;
  try
    StartSession(10);
    FNode.BeginBlockChainUpdate;
    try CommitGenerated(True); finally FNode.EndBlockChainUpdate; end;
    CheckUnavailable;
    Check(FObservation.Count=0,'A selected target ahead must not replay before Quit');
    LExpected := OracleBytes;
    FStopping := True;
    FNotify.Node := nil;
    FreeAndNil(FNode);
    LActual.LoadFromFile(FDirectory+PathDelim+'Data'+PathDelim+'pendingbuffer.ops');
    // Shutdown may publish the retained original sequence or a fully finished
    // final reconciliation. Both restart safely; a partial package does not.
    LMatchesBefore := (LBefore.Size=LActual.Size) and CompareMem(LBefore.Memory,LActual.Memory,LBefore.Size);
    LMatchesFinal := (LExpected.Size=LActual.Size) and CompareMem(LExpected.Memory,LActual.Memory,LExpected.Size);
    Check(LMatchesBefore or LMatchesFinal,'Quit must persist one exact complete pending package');
  finally LActual.Free; LExpected.Free; LBefore.Free; end;
end;

procedure TPendingCatchupChecks.EmptyScope;
begin
  FNode.BeginBlockChainUpdate;
  try CheckUnavailable; finally FNode.EndBlockChainUpdate; end;
  WaitAvailable;
  Check(FObservation.Count=0,'An unchanged empty scope must not invent validation work');
  CheckPool(0);
end;

procedure TPendingCatchupChecks.ReplayFailureRetry;
var LOriginal,LExpected,LActual: TMemoryStream;
begin
  Seed;
  LOriginal := PendingBytes;
  LExpected := nil;
  LActual := nil;
  try
    StartSession(1);
    FNode.BeginBlockChainUpdate;
    try CommitGenerated; finally
      InterlockedExchange(FObservation.ThrowOnAttempt,2);
      FNode.EndBlockChainUpdate;
    end;
    Check(FObservation.ThrowReached.WaitFor(3000)=wrSignaled,'The second real pending validator must inject the failure');
    CheckRetainedPackage(LOriginal);
    Pump(200);
    CheckUnavailable;
    Check(FObservation.Count=2,'A failed pending replay must not retry in a hot loop');
    Check(FAvailableCallbacks=0,'A failed partial account overlay must never be published as available');
    // No new block/peer/refresh event follows the failure. The actual owned
    // maintenance backoff must wake and retry its retained complete input.
    WaitAvailable(15000);
    Check(FObservation.Count=5,'The retry validates all three original operations after two failed-pass attempts');
    CheckPool(3);
    CheckFinalAccounts(3);
    LExpected := OracleBytes(3);
    LActual := PendingBytes;
    Check((LExpected.Size=LActual.Size) and CompareMem(LExpected.Memory,LActual.Memory,LExpected.Size),
      'Successful automatic retry publishes exact independently validated bytes');
  finally LActual.Free; LExpected.Free; LOriginal.Free; end;
end;

procedure TPendingCatchupChecks.ReplayFailureShutdown;
var LOriginal,LActual: TMemoryStream;
begin
  Seed;
  LOriginal := PendingBytes;
  LActual := TMemoryStream.Create;
  try
    StartSession(1);
    FNode.BeginBlockChainUpdate;
    try CommitGenerated; finally
      InterlockedExchange(FObservation.ThrowOnAttempt,2);
      FNode.EndBlockChainUpdate;
    end;
    Check(FObservation.ThrowReached.WaitFor(3000)=wrSignaled,'Injected pending failure must precede Quit');
    CheckRetainedPackage(LOriginal);
    CheckUnavailable;
    FStopping := True;
    FNotify.Node := nil;
    FreeAndNil(FNode);
    LActual.LoadFromFile(FDirectory+PathDelim+'Data'+PathDelim+'pendingbuffer.ops');
    Check((LOriginal.Size=LActual.Size) and CompareMem(LOriginal.Memory,LActual.Memory,LOriginal.Size),
      'Quit after a failed replay must persist every original pending operation');
  finally LActual.Free; LOriginal.Free; end;
end;

procedure TPendingCatchupChecks.Run(ACheck: TPendingCatchupCheck);
begin
  case ACheck of
    pccBufferedCommits: BufferedCommits;
    pccInterBatchTarget: InterBatchTarget;
    pccMixedRejections: MixedRejections;
    pccUnavailableContracts: UnavailableContracts;
    pccNewWorkDuringReplay: NewWorkDuringReplay;
    pccRetiredScopes: RetiredScopes;
    pccShutdownPackage: ShutdownPackage;
    pccEmptyScope: EmptyScope;
    pccReplayFailureRetry: ReplayFailureRetry;
    pccReplayFailureShutdown: ReplayFailureShutdown;
  end;
  Log('PASS catch-up scenario='+IntToStr(Ord(ACheck))+' actual pending validation attempts='+IntToStr(FObservation.Count));
end;

function RunPendingCatchupCheck(ACheck: TPendingCatchupCheck;
  const ALogPath: String): Integer;
var LChecks: TPendingCatchupChecks; LLog: TStringList;
begin
  Result := 1;
  if FileExists(ALogPath) then raise Exception.Create('Refusing an existing catch-up scenario log');
  LChecks := nil;
  try
    try
      LChecks := TPendingCatchupChecks.Create(ALogPath);
      LChecks.Run(ACheck);
      Result := 0;
    except on E: Exception do begin
      LLog := TStringList.Create;
      try
        if FileExists(ALogPath) then LLog.LoadFromFile(ALogPath);
        LLog.Add('FAIL '+E.ClassName+': '+E.Message);
        LLog.SaveToFile(ALogPath);
      finally LLog.Free; end;
    end; end;
  finally LChecks.Free; end;
end;

function RunPendingCatchupChecks(const ALogPath: String): Integer;
var LCheck: TPendingCatchupCheck; LLog: TStringList;
begin
  Result := 1;
  if FileExists(ALogPath) then raise Exception.Create('Refusing an existing catch-up summary');
  LLog := TStringList.Create;
  try
    for LCheck := Low(TPendingCatchupCheck) to High(TPendingCatchupCheck) do begin
      if RunPendingCatchupCheck(LCheck,ALogPath+'.'+IntToStr(Ord(LCheck)))<>0 then begin
        LLog.Add('FAIL scenario='+IntToStr(Ord(LCheck))+'; see its owned log');
        LLog.SaveToFile(ALogPath);
        Exit;
      end;
      LLog.Add('PASS scenario='+IntToStr(Ord(LCheck)));
      LLog.SaveToFile(ALogPath);
    end;
    LLog.Add('PASS pending catch-up: 10');
    LLog.SaveToFile(ALogPath);
    Result := 0;
  finally LLog.Free; end;
end;

end.
