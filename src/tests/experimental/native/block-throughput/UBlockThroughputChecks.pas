unit UBlockThroughputChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Measures actual core work in an explicitly prepared public-data clone.
  No wallet controls, replacement operation methods or external peers. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunBlockThroughputChecks(const ADataDirectory, ALogPath: String): Integer;

implementation

uses {$IFDEF FPC}Windows,{$ELSE}Winapi.Windows,{$ENDIF}
  Classes, SysUtils, UNode, UBlockChain, UAccounts, UFileStorage, UThread,
  UBaseTypes, UCrypto, UNetProtocol, UConst, UPCDataTypes, SyncObjs,
  UOpTransaction, UOrderedList;

type
  TBlockThroughputChecks = class
  private
    FLog: TStringList;
    FLogPath: String;
    FLastProgress: TTickCount;
    FRealCompletion: TEvent;
    procedure Log(const AText: String);
    procedure Require(AValue: Boolean; const AMessage: String);
    procedure Progress(Sender: TObject; const AProgress: TProgressInfo);
    procedure ActualSynchronizationChanged(Sender: TObject;
      const AStatus: TNodeSynchronizationStatus);
    procedure MeasurePhases(ANode: TNode);
    procedure MeasureMixedRejections(ANode: TNode; AConfirmEvery: Integer);
    procedure MeasureRealBlocks(ANode: TNode; const AFixtureDirectory: String);
    procedure VerifyStoredRealBlocks(const ADataDirectory,AFixtureDirectory: String);
  public
    constructor Create(const ALogPath: String);
    destructor Destroy; override;
    procedure Run(const ADataDirectory: String);
  end;

  TDenseObservedChangeInfo = class(TOpChangeAccountInfo)
    class function CopySignedOperation(AOriginal: TOpChangeAccountInfo): TDenseObservedChangeInfo;
    function DoOperation(APrevious: TAccountPreviousBlockInfo;
      ATransaction: TPCSafeBoxTransaction; var AErrors: String): Boolean; override;
  end;

var GDenseObservedTransaction: TPCSafeBoxTransaction;
  GDenseObservedAttempts: LongInt;

class function TDenseObservedChangeInfo.CopySignedOperation(
  AOriginal: TOpChangeAccountInfo): TDenseObservedChangeInfo;
var LStream: TMemoryStream;
begin
  Result := TDenseObservedChangeInfo.Create(AOriginal.ProtocolVersion);
  try
    LStream := TMemoryStream.Create;
    try
      if not AOriginal.SaveToNettransfer(LStream) then
        raise Exception.Create('Cannot serialize the existing signed observation operation');
      LStream.Position := 0;
      if not Result.LoadFromNettransfer(LStream) then
        raise Exception.Create('Cannot decode the existing signed observation operation');
    finally LStream.Free; end;
    // Preserve the existing validated signature/cache; only inherited validator
    // entry is observed. The public wire/storage bytes remain unchanged.
    Result.FHasValidSignature := AOriginal.HasValidSignature;
    Result.FUsedPubkeyForSignature := TDenseObservedChangeInfo(AOriginal).FUsedPubkeyForSignature;
    Result.FBufferedSha256 := AOriginal.Sha256;
    Result.FBufferedRipeMD160 := AOriginal.RipeMD160;
    Result.DiscoveredOnBlock := AOriginal.DiscoveredOnBlock;
    Result.ResendOnBlock := AOriginal.ResendOnBlock;
    Result.ResendCount := AOriginal.ResendCount;
  except Result.Free; raise; end;
end;

function TDenseObservedChangeInfo.DoOperation(APrevious: TAccountPreviousBlockInfo;
  ATransaction: TPCSafeBoxTransaction; var AErrors: String): Boolean;
begin
  if ATransaction=GDenseObservedTransaction then
    InterlockedIncrement(GDenseObservedAttempts);
  Result := inherited DoOperation(APrevious,ATransaction,AErrors);
end;

constructor TBlockThroughputChecks.Create(const ALogPath: String);
begin
  inherited Create;
  FLog := TStringList.Create;
  FLogPath := ALogPath;
end;

destructor TBlockThroughputChecks.Destroy;
begin
  FLog.Free;
  inherited;
end;

procedure TBlockThroughputChecks.Log(const AText: String);
begin
  FLog.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + AText);
  FLog.SaveToFile(FLogPath);
end;

procedure TBlockThroughputChecks.Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then raise Exception.Create(AMessage);
end;

procedure TBlockThroughputChecks.Progress(Sender: TObject; const AProgress: TProgressInfo);
begin
  if TPlatform.GetElapsedMilliseconds(FLastProgress) < 1000 then Exit;
  FLastProgress := TPlatform.GetTickCount;
  Log(Format('RESTORE stage=%d current=%d total=%d',
    [Ord(AProgress.Stage), AProgress.CurrentPosition, AProgress.TotalCount]));
end;

procedure TBlockThroughputChecks.ActualSynchronizationChanged(Sender: TObject;
  const AStatus: TNodeSynchronizationStatus);
begin
  if Assigned(FRealCompletion) and AStatus.PendingOperationsAvailable then
    FRealCompletion.SetEvent;
end;

procedure TBlockThroughputChecks.MeasurePhases(ANode: TNode);
var LMempool: TPCOperationsComp; LTree: TOperationsHashTree;
  LReplay: TPCSafeBoxTransaction; LPrevious: TAccountPreviousBlockInfo;
  LStream: TMemoryStream; LStarted: TTickCount;
  I: Integer; LError: String;
begin
  // These are independent real-core experiments, not replacement sanitizer
  // logic. Exclusive ownership also excludes the background pending-file save.
  LMempool := ANode.LockMempoolWrite;
  try
    LReplay := TPCSafeBoxTransaction.Create(ANode.Bank.SafeBox);
    LPrevious := TAccountPreviousBlockInfo.Create;
    try
      LStarted := TPlatform.GetTickCount;
      for I := 0 to LMempool.Count - 1 do
        Require(LMempool.OperationsHashTree.GetOperation(I).DoOperation(LPrevious, LReplay, LError),
          'Real operation replay failed: ' + LError);
      Log('PHASE actual_DoOperation_replay_ms=' + IntToStr(TPlatform.GetElapsedMilliseconds(LStarted)));
      Require((LReplay.TotalBalance = LMempool.SafeBoxTransaction.TotalBalance) and
        (LReplay.TotalFee = LMempool.SafeBoxTransaction.TotalFee), 'Replay aggregate mismatch');
    finally
      LStarted := TPlatform.GetTickCount;
      LPrevious.Free;
      LReplay.Free;
      Log('PHASE replay_cleanup_ms=' + IntToStr(TPlatform.GetElapsedMilliseconds(LStarted)));
    end;

    LTree := TOperationsHashTree.Create;
    try
      LTree.Max0feeOperationsBySigner := LMempool.OperationsHashTree.Max0feeOperationsBySigner;
      LStarted := TPlatform.GetTickCount;
      for I := 0 to LMempool.Count - 1 do begin
        Require(LTree.CanAddOperationToHashTree(LMempool.Operation[I]), 'Eligibility mismatch');
        Require(LTree.AddOperationToHashTree(LMempool.Operation[I]), 'Index rebuild rejected an operation');
      end;
      Log('PHASE eligibility_and_actual_tree_rebuild_ms=' + IntToStr(TPlatform.GetElapsedMilliseconds(LStarted)));
      LStarted := TPlatform.GetTickCount;
      Require(TBaseType.Equals(LTree.HashTree, LMempool.OperationsHashTree.HashTree), 'Rebuilt tree hash/order mismatch');
      Log('PHASE rebuilt_hash_ms=' + IntToStr(TPlatform.GetElapsedMilliseconds(LStarted)));
    finally
      LStarted := TPlatform.GetTickCount;
      LTree.Free;
      Log('PHASE rebuilt_tree_cleanup_ms=' + IntToStr(TPlatform.GetElapsedMilliseconds(LStarted)));
    end;

    LTree := TOperationsHashTree.Create;
    try
      LStarted := TPlatform.GetTickCount;
      LTree.CopyFromHashTree(LMempool.OperationsHashTree);
      Log('PHASE existing_CopyFromHashTree_ms=' + IntToStr(TPlatform.GetElapsedMilliseconds(LStarted)));
      Require((LTree.OperationsCount = LMempool.Count) and
        TBaseType.Equals(LTree.HashTree, LMempool.OperationsHashTree.HashTree), 'Existing clone mismatch');
    finally LTree.Free; end;

    LStream := TMemoryStream.Create;
    try
      LStarted := TPlatform.GetTickCount;
      Require(LMempool.OperationsHashTree.SaveOperationsHashTreeToStream(LStream, True), 'Serialization failed');
      Log(Format('PHASE memory_serialization_ms=%d bytes=%d',
        [TPlatform.GetElapsedMilliseconds(LStarted), LStream.Size]));
    finally LStream.Free; end;
    LStarted := TPlatform.GetTickCount;
    ANode.Bank.Storage.SavePendingBufferOperations(LMempool.OperationsHashTree);
    Log('PHASE actual_buffered_pending_save_ms=' + IntToStr(TPlatform.GetElapsedMilliseconds(LStarted)));
  finally ANode.UnlockMempoolWrite; end;
end;

procedure TBlockThroughputChecks.MeasureMixedRejections(ANode: TNode; AConfirmEvery: Integer);
var LPool: TPCOperationsComp; LOriginal,LSelected,LExpected: TOperationsHashTree;
  LTransaction: TPCSafeBoxTransaction; LPrevious: TAccountPreviousBlockInfo;
  LHeader: TOperationBlock; LError: String; LStream: TMemoryStream;
  LStarted: TTickCount; I,J,LSample,LInputCount,LConfirmed,LOutputCount,LPosition: Integer;
  LOutputHash,LBytesHash,LCurrentBytesHash: TRawBytes;
  LOutputBalance,LRegistryCount: Int64;
  LCreate,LExit,LKernel,LUser: TFileTime;
  LCPUBefore,LCPUAfter: UInt64;

  function CPUTime: UInt64;
  begin
    if not GetProcessTimes(GetCurrentProcess,LCreate,LExit,LKernel,LUser) then RaiseLastOSError;
    Result := ((UInt64(LKernel.dwHighDateTime) shl 32) or LKernel.dwLowDateTime) +
      ((UInt64(LUser.dwHighDateTime) shl 32) or LUser.dwLowDateTime);
  end;

begin
  // Keep background persistence outside the experiment and retain all input
  // registrations between samples. Only SanitizeOperations is timed below.
  LPool := ANode.LockMempoolWrite;
  try
    LInputCount := LPool.Count;
    Require(LInputCount=185831,'Mixed fixture requires the complete real pending pool');
    LOriginal := TOperationsHashTree.Create;
    try
      LOriginal.CopyFromHashTree(LPool.OperationsHashTree);
      LRegistryCount := TPCOperationsStorage.PCOperationsStorage.Count;
      LTransaction := TPCSafeBoxTransaction.Create(ANode.Bank.SafeBox);
      LPrevious := TAccountPreviousBlockInfo.Create;
      LSelected := TOperationsHashTree.Create;
      try
        LConfirmed := 0;
        for I := 0 to LInputCount-1 do
          if (I mod AConfirmEvery)=0 then begin
            Require(LOriginal.GetOperation(I).DoOperation(LPrevious,LTransaction,LError),
              'Simulated confirmation uses real valid pending operations: '+LError);
            Require(LSelected.AddOperationToHashTree(LOriginal.GetOperation(I)),
              'Selected confirmation operation registration');
            Inc(LConfirmed);
          end;
        Require(LConfirmed>0,'Mixed fixture must confirm a nonempty real subset');
        LHeader := LPool.OperationBlock;
        LHeader.block := ANode.Bank.BlocksCount;
        LHeader.account_key := ANode.Bank.SafeBox.Account(0).accountInfo.accountKey;
        LHeader.timestamp := ANode.Bank.LastOperationBlock.timestamp+1;
        LHeader.nonce := 0;
        LHeader.operations_hash := LSelected.HashTree;
        LHeader.fee := LTransaction.TotalFee;
        SetLength(LHeader.proof_of_work,32);
        FillChar(LHeader.proof_of_work[0],32,0);
        Require(LTransaction.Commit(LHeader,LError),'Actual selected-operation commit failed: '+LError);
        Log(Format('SIMULATED_CONFIRMATION operations=%d every=%d new_blocks=%d no_PoW_validation=TRUE',
          [LConfirmed,AConfirmEvery,ANode.Bank.BlocksCount]));
      finally LSelected.Free; LPrevious.Free; LTransaction.Free; end;
      LOutputCount := -1;
      LOutputBalance := 0;
      for LSample := 1 to 3 do begin
        // This restores the exact original real objects/order; normal public
        // tree copying is preparation, not the optimized timed implementation.
        if LSample>1 then LPool.OperationsHashTree.CopyFromHashTree(LOriginal);
        Require(LPool.Count=LInputCount,'Every mixed sample restores the same complete input');
        LCPUBefore := CPUTime;
        LStarted := TPlatform.GetTickCount;
        LPool.SanitizeOperations;
        LCPUAfter := CPUTime;
        Log(Format('MIXED_SANITIZE sample=%d body_ms=%d cpu_ms=%d input=%d output=%d',
          [LSample,TPlatform.GetElapsedMilliseconds(LStarted),(LCPUAfter-LCPUBefore) div 10000,LInputCount,LPool.Count]));
        Require((LPool.Count>0) and (LPool.Count<LInputCount),
          'Mixed sample must actually take a nonempty rejected-operation path');
        Require(LInputCount-LPool.Count>=LConfirmed,'Every confirmed old operation must be rejected');
        LExpected := TOperationsHashTree.Create;
        LStream := TMemoryStream.Create;
        try
          LExpected.Max0feeOperationsBySigner := LPool.OperationsHashTree.Max0feeOperationsBySigner;
          LPosition := 0;
          for I := 0 to LPool.Count-1 do begin
            while (LPosition<LInputCount) and
              (LOriginal.GetOperation(LPosition)<>LPool.Operation[I]) do Inc(LPosition);
            Require(LPosition<LInputCount,'Rejected-path output must preserve original operation identity/order');
            Inc(LPosition);
            Require(LExpected.AddOperationToHashTree(LPool.Operation[I]),
              'Independent normal insertion must admit the accepted output');
          end;
          Require(TBaseType.Equals(LExpected.HashTree,LPool.OperationsHashTree.HashTree),
            'Rejected-path ordered hash differs from normal insertion');
          Require((LExpected.TotalAmount=LPool.OperationsHashTree.TotalAmount) and
            (LExpected.TotalFee=LPool.OperationsHashTree.TotalFee) and
            (LExpected.TotalFee=LPool.OperationBlock.fee) and
            (LExpected.TotalFee=LPool.SafeBoxTransaction.TotalFee),'Rejected-path aggregate mismatch');
          Require(LExpected.HasOpRecoverOperations=LPool.OperationsHashTree.HasOpRecoverOperations,
            'Rejected-path recover metadata mismatch');
          for J := 0 to LPool.Count-1 do begin
            Require(LExpected.IndexOfOperation(LPool.Operation[J])=
              LPool.OperationsHashTree.IndexOfOperation(LPool.Operation[J]),'Rejected-path SHA representative');
            Require(LExpected.IndexOfOpReference(LPool.Operation[J].GetOpReference)=
              LPool.OperationsHashTree.IndexOfOpReference(LPool.Operation[J].GetOpReference),
              'Rejected-path operation-reference representative');
            Require(LExpected.CountOperationsBySameSignerWithoutFee(LPool.Operation[J].SignerAccount)=
              LPool.OperationsHashTree.CountOperationsBySameSignerWithoutFee(LPool.Operation[J].SignerAccount),
              'Rejected-path zero-fee signer metadata');
          end;
          Require(LPool.OperationsHashTree.SaveOperationsHashTreeToStream(LStream,True),'Mixed output serialization');
          LCurrentBytesHash := TCrypto.DoSha256(PAnsiChar(LStream.Memory),LStream.Size);
          if LSample=1 then begin
            LOutputCount := LPool.Count;
            LOutputHash := Copy(LPool.OperationsHashTree.HashTree);
            LBytesHash := Copy(LCurrentBytesHash);
            LOutputBalance := LPool.SafeBoxTransaction.TotalBalance;
          end else begin
            Require(LPool.Count=LOutputCount,'Mixed sample accepted count must repeat exactly');
            Require(TBaseType.Equals(LOutputHash,LPool.OperationsHashTree.HashTree),'Mixed sample ordered hash');
            Require(TBaseType.Equals(LBytesHash,LCurrentBytesHash),'Mixed sample exact serialized output');
            Require(LPool.SafeBoxTransaction.TotalBalance=LOutputBalance,'Mixed replay balance repeats exactly');
          end;
          Log(Format('MIXED_OUTPUT sample=%d count=%d ordered_sha=%s bytes=%d bytes_sha=%s balance=%d fee=%d',
            [LSample,LPool.Count,TCrypto.ToHexaString(LPool.OperationsHashTree.HashTree),LStream.Size,
             TCrypto.ToHexaString(LCurrentBytesHash),LPool.SafeBoxTransaction.TotalBalance,LPool.SafeBoxTransaction.TotalFee]));
        finally LStream.Free; LExpected.Free; end;
        Require(TPCOperationsStorage.PCOperationsStorage.Count=LRegistryCount,
          'Repeated rejection replay must not grow the operation registry');
      end;
      Log('PASS mixed rejection: actual DoOperation/Commit with simulated confirmation; normal-insertion indexes, order/hash/bytes/fee/signer/registry exact; no PoW or network throughput claim');
    finally LOriginal.Free; end;
  finally ANode.UnlockMempoolWrite; end;
end;

procedure TBlockThroughputChecks.MeasureRealBlocks(ANode: TNode; const AFixtureDirectory: String);
var LBlock: TPCOperationsComp; LPool: TPCOperationsComp;
  LInput,LObservedTree: TOperationsHashTree; LReplay: TPCOperationsComp;
  LStream,LExpectedBytes: TMemoryStream; LAffected: TOrderedList<Cardinal>;
  LNotify: TNodeNotifyEvents; LObserved: TDenseObservedChangeInfo;
  LOriginalHash,LOriginalBytesHash: TRawBytes;
  LHeader: TOperationBlock; LFileName,LError: String;
  I,J,LObservedIndex,LInitialCount,LBlockOperations,LExpectedResend,LResent: Integer;
  LAnnouncedLast,LAccepted: Boolean;
  LInitialTip: Cardinal; LUntil: UInt64;
  LRequest,LStarted,LAdmitted,LFinished: TTickCount;
  LCreate,LExit,LKernel,LUser: TFileTime;
  LCPUBefore,LCPUAfter: UInt64;

  function CPUTime: UInt64;
  begin
    if not GetProcessTimes(GetCurrentProcess,LCreate,LExit,LKernel,LUser) then RaiseLastOSError;
    Result := ((UInt64(LKernel.dwHighDateTime) shl 32) or LKernel.dwLowDateTime) +
      ((UInt64(LUser.dwHighDateTime) shl 32) or LUser.dwLowDateTime);
  end;

begin
  Require(ANode.Bank.BlocksCount=841529,'Real fixture must start at the frozen public tip841529');
  LAnnouncedLast := GetEnvironmentVariable('PASCAL_BLOCK_BENCH_ANNOUNCED_LAST')='1';
  LBlock := TPCOperationsComp.Create(ANode.Bank);
  LInput := TOperationsHashTree.Create;
  LObservedTree := TOperationsHashTree.Create;
  LNotify := nil;
  FRealCompletion := TEvent.Create(nil,True,False,'');
  try
    LPool := ANode.LockMempoolWrite;
    try
      LInitialTip := LPool.OperationBlock.block;
      LInitialCount := LPool.Count;
      Require(LInitialCount=185831,'Real catch-up fixture requires the complete pending input');
      LInput.CopyFromHashTree(LPool.OperationsHashTree);
      LOriginalHash := Copy(LInput.HashTree);
      LStream := TMemoryStream.Create;
      try
        Require(LInput.SaveOperationsHashTreeToStream(LStream,True),'Serialize original complete pending input');
        LOriginalBytesHash := TCrypto.DoSha256(PAnsiChar(LStream.Memory),LStream.Size);
      finally LStream.Free; end;
      LObservedIndex := -1;
      for J := 0 to LInput.OperationsCount-1 do
        if LInput.GetOperation(J) is TOpChangeAccountInfo then begin
          LObservedIndex := J;
          Break;
        end;
      Require(LObservedIndex>=0,'A real signed change-info operation is required for replay observation');
      LObserved := TDenseObservedChangeInfo.CopySignedOperation(
        TOpChangeAccountInfo(LInput.GetOperation(LObservedIndex)));
      try
        LObservedTree.Max0feeOperationsBySigner := LInput.Max0feeOperationsBySigner;
        for J := 0 to LInput.OperationsCount-1 do
          if J=LObservedIndex then
            Require(LObservedTree.AddOperationToHashTree(LObserved),'Register inherited replay observation')
          else
            Require(LObservedTree.AddOperationToHashTree(LInput.GetOperation(J)),'Retain original real operation');
      finally LObserved.Free; end;
      LStream := TMemoryStream.Create;
      try
        Require(LObservedTree.SaveOperationsHashTreeToStream(LStream,True),'Serialize observed pending input');
        Require(TBaseType.Equals(LOriginalBytesHash,TCrypto.DoSha256(PAnsiChar(LStream.Memory),LStream.Size)),
          'Adding replay observation must preserve every pending storage byte');
      finally LStream.Free; end;
      LPool.OperationsHashTree.CopyFromHashTree(LObservedTree);
      LInput.CopyFromHashTree(LObservedTree);
      // Resend metadata is not part of the operation's wire/storage bytes.
      // Controlled zeros make the legacy bounded selection deterministic.
      for J := 0 to LPool.Count-1 do begin
        LPool.Operation[J].ResendCount := 0;
        LPool.Operation[J].ResendOnBlock := 0;
      end;
      Require(TBaseType.Equals(LOriginalHash,LPool.OperationsHashTree.HashTree),'Observed ordered input hash is unchanged');
      GDenseObservedTransaction := LPool.SafeBoxTransaction;
      InterlockedExchange(GDenseObservedAttempts,0);
    finally ANode.UnlockMempoolWrite; end;
    LNotify := TNodeNotifyEvents.Create(nil);
    LNotify.OnSynchronizationChanged := ActualSynchronizationChanged;
    LNotify.Node := ANode;
    LUntil := GetTickCount64+100;
    repeat CheckSynchronize(0); Sleep(1); until GetTickCount64>=LUntil;
    FRealCompletion.ResetEvent;
    ANode.BeginBlockChainUpdate;
    try
      for I := 841529 to 841533 do begin
        LFileName := IncludeTrailingPathDelimiter(AFixtureDirectory)+'block-'+IntToStr(I)+'.bin';
        LStream := TMemoryStream.Create;
        try
          LStream.LoadFromFile(LFileName);
          Require(LBlock.LoadBlockFromStream(LStream,LError),'Actual network-format block decode: '+LError);
        finally LStream.Free; end;
        LHeader := LBlock.OperationBlock;
        LBlockOperations := LBlock.Count;
        Require((LHeader.block=Cardinal(I)) and (LHeader.protocol_version=5),'Actual source block/header sequence');
        LRequest := TPlatform.GetTickCount;
        Require(ANode.TryLockNode(3000),'Actual received-block writer admission');
        LAdmitted := TPlatform.GetTickCount;
        try
          LCPUBefore := CPUTime;
          LStarted := TPlatform.GetTickCount;
          // Normal core bank validation computes PoW, executes the actual block,
          // persists it and invokes the actual node's deferred-pending subscriber.
          if LAnnouncedLast and (I=841533) then
            LAccepted := ANode.AddNewBlockChain(nil,LBlock,LError)
          else
            LAccepted := ANode.Bank.AddNewBlockChainBlock(LBlock,0,LError);
          Require(LAccepted,'Actual full block acceptance: '+LError);
          LCPUAfter := CPUTime;
          LFinished := TPlatform.GetTickCount;
          Require(ANode.Bank.BlocksCount=Cardinal(I+1),'Actual committed height');
          Require(TPCOperationsComp.EqualsOperationBlock(LHeader,ANode.Bank.LastOperationBlock),
            'Committed header must equal the extracted actual public header');
          Require(not ANode.PendingOperationsAvailable,'Pending balances remain unavailable through the received batch');
          Require(not ANode.GetSynchronizationStatus.PendingOperationsAvailable,'Cached availability agrees with the node');
          Require(ANode.GetSynchronizationStatus.LocalBlockCount=Cardinal(I+1),'Cached confirmed tip advances immediately');
          Require(LPool.OperationBlock.block=LInitialTip,'No per-block sanitizer may replace the deferred pending header');
          Require(not LPool.SafeBoxTransaction.CheckIntegrity,'The deferred overlay is not exposed as current');
          Require(InterlockedCompareExchange(GDenseObservedAttempts,0,0)=0,'Actual pending validator must not replay per block');
          Require((LPool.Count=LInitialCount) and TBaseType.Equals(LOriginalHash,LPool.OperationsHashTree.HashTree),
            'Catch-up retains the exact original operation sequence');
          for J := 0 to LPool.Count-1 do
            Require((LPool.Operation[J].ResendCount=0) and (LPool.Operation[J].ResendOnBlock=0),
              'Neither bulk blocks nor a deferred announcement may collect stale pending resends');
          Log(Format('REAL_BLOCK block=%d operations=%d admission_ms=%d body_ms=%d cpu_ms=%d retained_pending=%d observed_replays=%d',
            [I,LBlockOperations,Int64(LAdmitted-LRequest),Int64(LFinished-LStarted),
             (LCPUAfter-LCPUBefore) div 10000,LInitialCount,InterlockedCompareExchange(GDenseObservedAttempts,0,0)]));
        finally ANode.UnlockNode; end;
      end;
      LStarted := TPlatform.GetTickCount;
      ANode.Bank.SafeBox.PCAbstractMem.FlushCache;
      Log('REAL_BATCH final_safebox_flush_ms='+IntToStr(TPlatform.GetElapsedMilliseconds(LStarted)));
    finally
      LStarted := TPlatform.GetTickCount;
      LCPUBefore := CPUTime;
      ANode.EndBlockChainUpdate;
    end;
    LUntil := GetTickCount64+30000;
    repeat
      CheckSynchronize(0);
      if FRealCompletion.WaitFor(0)=wrSignaled then Break;
      Sleep(1);
    until GetTickCount64>=LUntil;
    Require(FRealCompletion.WaitFor(0)=wrSignaled,'Actual maintenance completion must arrive without a polling timer');
    Require(ANode.PendingOperationsAvailable,'The final complete pending overlay is available');
    Require(InterlockedCompareExchange(GDenseObservedAttempts,0,0)=1,'Observed real validator runs in exactly one final replay');
    Log(Format('REAL_FINAL reconcile_wait_ms=%d process_cpu_ms=%d observed_replays=%d',
      [TPlatform.GetElapsedMilliseconds(LStarted),(CPUTime-LCPUBefore) div 10000,
       InterlockedCompareExchange(GDenseObservedAttempts,0,0)]));
    LPool := ANode.LockMempoolRead;
    try
      Require((LPool.Count=LInitialCount) and (LPool.OperationBlock.block=ANode.Bank.BlocksCount),
        'Final pending count/header are coherent with the committed confirmed tip');
      Require(LPool.SafeBoxTransaction.CheckIntegrity,'Final pending overlay integrity');
      LResent := 0;
      for J := 0 to LPool.Count-1 do begin
        if LAnnouncedLast and (J<CT_MaxResendMemPoolOperations) then LExpectedResend:=1
        else LExpectedResend:=0;
        Require(LPool.Operation[J].ResendCount=LExpectedResend,
          'Only the original bounded announced-block resend selection increments once');
        if LExpectedResend=1 then begin
          Inc(LResent);
          Require(LPool.Operation[J].ResendOnBlock=Int64(LPool.OperationBlock.block),
            'Selected resend metadata uses the final coherent pending header');
        end else Require(LPool.Operation[J].ResendOnBlock=0,'Bulk catch-up does not invent a resend intent');
      end;
      Log(Format('REAL_RESENDS announced_last=%s selected=%d retained=%d',
        [BoolToStr(LAnnouncedLast,True),LResent,LPool.Count]));
      LReplay := TPCOperationsComp.Create(ANode.Bank);
      LStream := TMemoryStream.Create;
      LExpectedBytes := TMemoryStream.Create;
      LAffected := TOrderedList<Cardinal>.Create(False,TComparison_Cardinal);
      try
        // The oracle performs independent ordinary validation/insertion on the
        // final confirmed base. All of this work is outside block/replay timing.
        Require(LReplay.AddOperations(LInput,LError)=LInitialCount,'Independent final full pending replay: '+LError);
        Require(TBaseType.Equals(LReplay.OperationsHashTree.HashTree,LPool.OperationsHashTree.HashTree),
          'Final ordered hash matches normal validation/insertion');
        for J := 0 to LPool.Count-1 do begin
          Require(LPool.Operation[J]=LInput.GetOperation(J),'Every final registered operation retains its exact order');
          Require(LReplay.OperationsHashTree.IndexOfOperation(LPool.Operation[J])=
            LPool.OperationsHashTree.IndexOfOperation(LPool.Operation[J]),'Final SHA representative');
          Require(LReplay.OperationsHashTree.IndexOfOpReference(LPool.Operation[J].GetOpReference)=
            LPool.OperationsHashTree.IndexOfOpReference(LPool.Operation[J].GetOpReference),'Final reference representative');
          LPool.Operation[J].AffectedAccounts(LAffected);
        end;
        for J := 0 to LAffected.Count-1 do
          Require(TAccountComp.EqualAccounts(LReplay.SafeBoxTransaction.Account(LAffected[J]),
            LPool.SafeBoxTransaction.Account(LAffected[J])),'Final pending account/seal equals full independent replay');
        Require(LReplay.SafeBoxTransaction.TotalFee=LPool.SafeBoxTransaction.TotalFee,'Final replay fees');
        Require(LReplay.SafeBoxTransaction.TotalBalance=LPool.SafeBoxTransaction.TotalBalance,'Final replay balance');
        Require(LPool.OperationsHashTree.SaveOperationsHashTreeToStream(LStream,True),'Final pending serialization');
        Require(LReplay.OperationsHashTree.SaveOperationsHashTreeToStream(LExpectedBytes,True),'Independent oracle serialization');
        Require((LStream.Size=LExpectedBytes.Size) and CompareMem(LStream.Memory,LExpectedBytes.Memory,LStream.Size),
          'Every final pending storage byte matches independent full replay');
        Require(TBaseType.Equals(LOriginalBytesHash,TCrypto.DoSha256(PAnsiChar(LStream.Memory),LStream.Size)),
          'These five historical blocks preserve all original pending bytes');
        Require(InterlockedCompareExchange(GDenseObservedAttempts,0,0)=1,'The independent oracle must not count as node replay');
        Log(Format('REAL_PENDING_FINAL count=%d affected_accounts=%d hash=%s bytes=%d bytes_sha=%s bank_hash=%s',
          [LPool.Count,LAffected.Count,TCrypto.ToHexaString(LPool.OperationsHashTree.HashTree),LStream.Size,
           TCrypto.ToHexaString(TCrypto.DoSha256(PAnsiChar(LStream.Memory),LStream.Size)),
           TCrypto.ToHexaString(ANode.Bank.SafeBox.SafeBoxHash)]));
      finally LAffected.Free; LExpectedBytes.Free; LStream.Free; LReplay.Free; end;
    finally ANode.UnlockMempoolRead; end;
  finally
    if Assigned(LNotify) then LNotify.Node := nil;
    LNotify.Free;
    FreeAndNil(FRealCompletion);
    GDenseObservedTransaction := nil;
    LObservedTree.Free;
    LInput.Free;
    LBlock.Free;
  end;
  Log('PASS five actual PoW-validated blocks; no per-block pending replay; one final inherited-validator observation; full final account/seal/index/bytes oracle');
end;

procedure TBlockThroughputChecks.VerifyStoredRealBlocks(const ADataDirectory,AFixtureDirectory: String);
var LStorage: TFileStorage; LBlock: TPCOperationsComp;
  LActual,LOriginal: TMemoryStream; I: Integer;
begin
  LStorage := TFileStorage.Create(nil);
  LBlock := TPCOperationsComp.Create(nil);
  LActual := TMemoryStream.Create;
  LOriginal := TMemoryStream.Create;
  try
    LStorage.ReadOnly := True;
    LStorage.SetBlockChainFile(AnsiString(IncludeTrailingPathDelimiter(ADataDirectory)+'Data'+PathDelim+'BlockChainStream.blocks'));
    Require(LStorage.Initialize,'Reopen actual stored chain read-only after closing the writer');
    for I := 841529 to 841533 do begin
      Require(LStorage.LoadBlockChainBlock(LBlock,I),'Reopen each actual accepted block');
      LActual.Clear;
      Require(LBlock.SaveBlockToStream(False,LActual),'Actual stored block network serialization');
      LOriginal.LoadFromFile(IncludeTrailingPathDelimiter(AFixtureDirectory)+'block-'+IntToStr(I)+'.bin');
      Require((LActual.Size=LOriginal.Size) and CompareMem(LActual.Memory,LOriginal.Memory,LActual.Size),
        'Reopened actual block differs from its original extracted network bytes');
      Log(Format('REAL_STORAGE block=%d bytes=%d sha256=%s',
        [I,LActual.Size,TCrypto.ToHexaString(TCrypto.DoSha256(PAnsiChar(LActual.Memory),LActual.Size))]));
    end;
    Log('PASS actual persisted five-block reopen and all original network bytes/hashes');
  finally LOriginal.Free; LActual.Free; LBlock.Free; LStorage.Free; end;
end;

procedure TBlockThroughputChecks.Run(const ADataDirectory: String);
var LNode: TNode; LMempool: TPCOperationsComp;
  LStarted, LAdmitted, LFinished: TTickCount;
  LHash,LSerializedHash: TRawBytes; LBlocks: Cardinal; LCount, I, J, LSamples, LCacheBudget: Integer;
  LOriginalTree: TOperationsHashTree;
  LOriginalOperations: array of TPCOperation;
  LOriginalSHAIndexes,LOriginalReferenceIndexes: array of Integer;
  LAmount,LFee: Int64; LStream: TMemoryStream;
  LReadyEvent: TEvent; LReadyName: String;
  LRealFixture: String; LVerifyReal: Boolean;
  LCreate, LExit, LKernel, LUser: TFileTime;
  LCPUBefore, LCPUAfter: UInt64;

  function CPUTime: UInt64;
  begin
    if not GetProcessTimes(GetCurrentProcess, LCreate, LExit, LKernel, LUser) then
      RaiseLastOSError;
    Result := ((UInt64(LKernel.dwHighDateTime) shl 32) or LKernel.dwLowDateTime) +
      ((UInt64(LUser.dwHighDateTime) shl 32) or LUser.dwLowDateTime);
  end;

begin
  LVerifyReal := False;
  LRealFixture := GetEnvironmentVariable('PASCAL_BLOCK_BENCH_REAL_BLOCKS');
  Require(ADataDirectory <> '', 'PASCAL_BLOCK_BENCH_DATA must name an owned prepared clone');
  Require(FileExists(IncludeTrailingPathDelimiter(ADataDirectory) + '.block-throughput-owned'),
    'The explicit data directory is missing its owned-clone marker');
  Require(FileExists(IncludeTrailingPathDelimiter(ADataDirectory) + 'Data\safebox.am_safebox'),
    'Prepared safebox is missing');
  Require(FileExists(IncludeTrailingPathDelimiter(ADataDirectory) + 'Data\BlockChainStream.blocks'),
    'Prepared blockchain stream is missing');
  Require(FileExists(IncludeTrailingPathDelimiter(ADataDirectory) + 'Data\pendingbuffer.ops'),
    'Prepared pending operations are missing');
  Require(not TNode.NodeExists, 'The throughput probe requires its own process/node');
  Log('DATA ' + ADataDirectory);
  TCrypto.InitCrypto;
  TNode.SetPascalCoinDataFolder(ADataDirectory);
  LNode := TNode.Create(nil);
  try
    LNode.BroadcastData := False;
    LCacheBudget := StrToIntDef(GetEnvironmentVariable('PASCAL_BLOCK_BENCH_ACCOUNT_CACHE'),0);
    if LCacheBudget>0 then begin
      Require((LCacheBudget=50000) or (LCacheBudget=250000),'Use one of the reviewed account cache budgets');
      LNode.Bank.SafeBox.PCAbstractMem.AccountCache.MaxRegisters := LCacheBudget;
    end;
    TNetData.NetData.NetConnectionsActive := False;
    Require(not LNode.NetServer.Active, 'The throughput probe must not listen');
    LNode.Bank.StorageClass := TFileStorage;
    Require(LNode.Bank.Storage.Initialize, 'Storage initialization failed');
    LStarted := TPlatform.GetTickCount;
    Log('RESTORE starting actual InitSafeboxAndOperations');
    LNode.InitSafeboxAndOperations($FFFFFFFF, Progress);
    Log('RESTORE completed milliseconds=' + IntToStr(TPlatform.GetElapsedMilliseconds(LStarted)));
    Log('CACHE after_restore='+IntToStr(LNode.Bank.SafeBox.PCAbstractMem.AccountCache.MaxRegisters));
    if LCacheBudget>0 then Require(LNode.Bank.SafeBox.PCAbstractMem.AccountCache.MaxRegisters=LCacheBudget,
      'Restore must preserve the explicitly configured account cache budget');
    LReadyName := GetEnvironmentVariable('PASCAL_BLOCK_BENCH_READY_EVENT');
    if LReadyName<>'' then begin
      LReadyEvent := TEvent.Create(nil,True,False,LReadyName);
      try
        Log('READY_GATE '+LReadyName);
        Require(LReadyEvent.WaitFor(60000)=wrSignaled,'Owned performance measurement gate timed out');
      finally LReadyEvent.Free; end;
    end;
    if LRealFixture<>'' then begin
      MeasureRealBlocks(LNode,LRealFixture);
      LVerifyReal := True;
      Exit;
    end;
    I := StrToIntDef(GetEnvironmentVariable('PASCAL_BLOCK_BENCH_CONFIRM_EVERY'),0);
    if I>0 then begin
      MeasureMixedRejections(LNode,I);
      Exit;
    end;
    LMempool := LNode.LockMempoolRead;
    try
      LBlocks := LNode.Bank.BlocksCount;
      LCount := LMempool.Count;
      LHash := Copy(LMempool.OperationsHashTree.HashTree);
      LOriginalTree := LMempool.OperationsHashTree;
      LAmount := LOriginalTree.TotalAmount;
      LFee := LOriginalTree.TotalFee;
      SetLength(LOriginalOperations,LCount);
      SetLength(LOriginalSHAIndexes,LCount);
      SetLength(LOriginalReferenceIndexes,LCount);
      for J := 0 to LCount-1 do begin
        LOriginalOperations[J] := LMempool.Operation[J];
        LOriginalSHAIndexes[J] := LOriginalTree.IndexOfOperation(LOriginalOperations[J]);
        LOriginalReferenceIndexes[J] := LOriginalTree.IndexOfOpReference(LOriginalOperations[J].GetOpReference);
      end;
      LStream := TMemoryStream.Create;
      try
        Require(LOriginalTree.SaveOperationsHashTreeToStream(LStream,True),'Initial serialization failed');
        LSerializedHash := TCrypto.DoSha256(PAnsiChar(LStream.Memory),LStream.Size);
        Log(Format('OUTPUT original_serialized_bytes=%d sha256=%s amount=%d fee=%d',
          [LStream.Size,TCrypto.ToHexaString(LSerializedHash),LAmount,LFee]));
      finally LStream.Free; end;
      Log(Format('BASELINE blocks=%d pending=%d protocol=%d',
        [LBlocks, LCount, LMempool.OperationBlock.protocol_version]));
      Require(LCount = 185831, 'The public-data baseline did not restore all 185831 pending operations');
    finally LNode.UnlockMempoolRead; end;
    if SameText(GetEnvironmentVariable('PASCAL_BLOCK_BENCH_PHASES'), '1') then LSamples := 1
    else LSamples := 3;
    for I := 1 to LSamples do begin
      LStarted := TPlatform.GetTickCount;
      LMempool := LNode.LockMempoolWrite;
      LAdmitted := TPlatform.GetTickCount;
      try
        LCPUBefore := CPUTime;
        LMempool.SanitizeOperations;
        LCPUAfter := CPUTime;
        LFinished := TPlatform.GetTickCount;
        Require(LMempool.Count = LCount, 'Sanitization changed the pending-operation count');
        Require(TBaseType.Equals(LHash, LMempool.OperationsHashTree.HashTree),
          'Sanitization changed operation order or hash');
        Require(LNode.Bank.BlocksCount = LBlocks, 'The probe unexpectedly accepted a block');
        // These output checks run after the timed body. They compare the exact
        // real ordered objects, both public indexes and the stored byte format.
        if GetEnvironmentVariable('PASCAL_BLOCK_BENCH_ASSERT_RETENTION')='1' then
          Require(LMempool.OperationsHashTree=LOriginalTree,'All-valid sanitizer replaced the original tree');
        Require((LMempool.OperationsHashTree.TotalAmount=LAmount) and
          (LMempool.OperationsHashTree.TotalFee=LFee),'Sanitization changed aggregate amounts');
        for J := 0 to LCount-1 do begin
          Require(LMempool.Operation[J]=LOriginalOperations[J],'Sanitization changed registered operation order');
          Require(LMempool.OperationsHashTree.IndexOfOperation(LOriginalOperations[J])=LOriginalSHAIndexes[J],
            'Sanitization changed a SHA lookup');
          Require(LMempool.OperationsHashTree.IndexOfOpReference(LOriginalOperations[J].GetOpReference)=
            LOriginalReferenceIndexes[J],'Sanitization changed an operation-reference lookup');
        end;
        LStream := TMemoryStream.Create;
        try
          Require(LMempool.OperationsHashTree.SaveOperationsHashTreeToStream(LStream,True),'Output serialization failed');
          Require(TBaseType.Equals(LSerializedHash,TCrypto.DoSha256(PAnsiChar(LStream.Memory),LStream.Size)),
            'Sanitization changed exact pending serialization bytes');
        finally LStream.Free; end;
      finally LNode.UnlockMempoolWrite; end;
      Log(Format('SANITIZE sample=%d admission_ms=%d body_ms=%d cpu_ms=%d pending=%d',
        [I, Int64(LAdmitted - LStarted),
         Int64(LFinished - LAdmitted),
         (LCPUAfter - LCPUBefore) div 10000, LCount]));
    end;
    Log(Format('PASS %d actual sanitizations; count/identity/order/hash/both-indexes/amount/fee/serialized-bytes/local-tip preserved; no UI/network activity', [LSamples]));
    if SameText(GetEnvironmentVariable('PASCAL_BLOCK_BENCH_PHASES'), '1') then MeasurePhases(LNode);
  finally
    LNode.Free;
    if LVerifyReal then VerifyStoredRealBlocks(ADataDirectory,LRealFixture);
  end;
end;

function RunBlockThroughputChecks(const ADataDirectory, ALogPath: String): Integer;
var LChecks: TBlockThroughputChecks;
begin
  Result := 1;
  if FileExists(ALogPath) then raise Exception.Create('Refusing an existing throughput log');
  LChecks := TBlockThroughputChecks.Create(ALogPath);
  try
    try LChecks.Run(ADataDirectory); Result := 0;
    except on E: Exception do LChecks.Log('FAIL ' + E.ClassName + ': ' + E.Message); end;
  finally LChecks.Free; end;
end;

end.
