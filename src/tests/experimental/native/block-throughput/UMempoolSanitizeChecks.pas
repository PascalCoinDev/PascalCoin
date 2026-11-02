unit UMempoolSanitizeChecks;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Generated, signed operations exercise the actual sanitizer and registry. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

type
  TMempoolSanitizeCheck = (mscAllValid, mscRejectedAndDuplicates,
    mscDeletedDuplicateIndex, mscZeroFeeSignerLimit, mscReplayException,
    mscRegistryOwnership, mscRecoverRejection, mscReferenceBoundaries,
    mscProtocolSignatureCaches);

function RunMempoolSanitizeChecks(const ALogPath: String): Integer;
function RunMempoolSanitizeCheck(ACheck: TMempoolSanitizeCheck;
  const ALogPath: String): Integer;

implementation

uses Classes, SysUtils, UAccounts, UBlockChain, UOpTransaction, UCrypto,
  UBaseTypes, UConst, UPCDataTypes, UNode;

type
  TGeneratedSanitizeSafeBox = class(TPCSafeBox)
    procedure Build(const AKey: TAccountKey);
  end;

  EExpectedReplayFailure = class(Exception);
  TThrowingTransaction = class(TOpTransaction)
    function DoOperation(APrevious: TAccountPreviousBlockInfo;
      ATransaction: TPCSafeBoxTransaction; var AErrors: String): Boolean; override;
  end;

  TReferenceBoundaryTransaction = class(TOpTransaction)
  public
    function SignerAccount: Cardinal; override;
    function N_Operation: Cardinal; override;
    procedure ForceLookupCollision;
  end;

  TMempoolSanitizeChecks = class
  private
    FLog: TStringList;
    FPath: String;
    FBank: TPCBank;
    FPool: TPCOperationsComp;
    FKey: TECPrivateKey;
    FChanges: Integer;
    procedure Check(AValue: Boolean; const AMessage: String);
    procedure Log(const AMessage: String);
    procedure Changed(Sender: TObject);
    procedure Reset;
    function Add(ANonce: Cardinal; AFee: UInt64 = 1): TPCOperation;
    procedure AssertSequence(const AExpected: array of TPCOperation);
    procedure CheckAllValid;
    procedure CheckRejectedAndDuplicates;
    procedure CheckDeletedDuplicateIndex;
    procedure CheckZeroFeeSignerLimit;
    procedure CheckReplayException;
    procedure CheckRegistryOwnership;
    procedure CheckRecoverRejection;
    procedure CheckReferenceBoundaries;
    procedure CheckProtocolSignatureCaches;
  public
    constructor Create(const APath: String);
    destructor Destroy; override;
    procedure Run;
    procedure RunOne(ACheck: TMempoolSanitizeCheck);
  end;

var GThrowReplayOnce: Boolean;

procedure TGeneratedSanitizeSafeBox.Build(const AKey: TAccountKey);
var I: Integer; LBlock: TOperationBlock; LAccount: TAccount;
begin
  for I := 0 to 159 do begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := I;
    LBlock.account_key := AKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := I * CT_NewLineSecondsAvg;
    AddNew(LBlock);
  end;
  DoUpgradeToProtocol5;
  LAccount := Account(0);
  UpdateAccount(0,LAccount.accountInfo,LAccount.name,LAccount.account_type,
    1000000,0,LAccount.account_data,LAccount.account_seal,
    aus_transaction_commit,0,0,False,False);
end;

function TThrowingTransaction.DoOperation(APrevious: TAccountPreviousBlockInfo;
  ATransaction: TPCSafeBoxTransaction; var AErrors: String): Boolean;
begin
  if GThrowReplayOnce then begin
    GThrowReplayOnce := False;
    raise EExpectedReplayFailure.Create('Injected failure after an accepted prefix');
  end;
  Result := inherited DoOperation(APrevious,ATransaction,AErrors);
end;

function TReferenceBoundaryTransaction.SignerAccount: Cardinal;
begin
  case inherited N_Operation of
    2: Result := $80000000;
    3: Result := High(Cardinal);
  else Result := 0;
  end;
end;

function TReferenceBoundaryTransaction.N_Operation: Cardinal;
begin
  case inherited N_Operation of
    3,4: Result := High(Cardinal);
  else Result := 0;
  end;
end;

procedure TReferenceBoundaryTransaction.ForceLookupCollision;
begin
  // Controlled lookup-key boundaries/collision only; inherited DoOperation
  // still executes and validates the distinct, correctly signed transactions.
  SetLength(FBufferedSha256,32);
  FillChar(FBufferedSha256[0],32,$A5);
end;

constructor TMempoolSanitizeChecks.Create(const APath: String);
begin
  inherited Create;
  FPath := APath;
  FLog := TStringList.Create;
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FBank := TPCBank.Create(nil);
  TGeneratedSanitizeSafeBox(FBank.SafeBox).Build(FKey.PublicKey);
  FPool := TPCOperationsComp.Create(nil);
  FPool.Bank := FBank;
end;

destructor TMempoolSanitizeChecks.Destroy;
begin
  FPool.Free;
  FBank.Free;
  FKey.Free;
  FLog.Free;
  inherited;
end;

procedure TMempoolSanitizeChecks.Check(AValue: Boolean; const AMessage: String);
begin
  if not AValue then raise Exception.Create(AMessage);
end;

procedure TMempoolSanitizeChecks.Log(const AMessage: String);
begin
  FLog.Add(AMessage);
  FLog.SaveToFile(FPath);
end;

procedure TMempoolSanitizeChecks.Changed(Sender: TObject);
begin
  Inc(FChanges);
end;

procedure TMempoolSanitizeChecks.Reset;
begin
  GThrowReplayOnce := False;
  FPool.OperationsHashTree.OnChanged := nil;
  FPool.Clear(True);
  // Deliberately admit dirty input, as an existing pending tree can predate
  // the current protocol's limits. Sanitize must enforce the new limits.
  FPool.OperationsHashTree.Max0feeOperationsBySigner := -1;
  FChanges := 0;
end;

function TMempoolSanitizeChecks.Add(ANonce: Cardinal; AFee: UInt64): TPCOperation;
var LOperation: TPCOperation;
begin
  LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_5,0,ANonce,1,
    FKey,1,AFee,CT_TOperationPayload_NUL);
  try
    Check(FPool.OperationsHashTree.AddOperationToHashTree(LOperation),'Raw pending insertion');
    Result := FPool.OperationsHashTree.GetOperation(FPool.Count-1);
  finally LOperation.Free; end;
end;

procedure TMempoolSanitizeChecks.AssertSequence(const AExpected: array of TPCOperation);
var I: Integer; LExpected: TOperationsHashTree; LAmount,LFee: Int64;
begin
  Check(FPool.Count=Length(AExpected),'Accepted operation count');
  LExpected := TOperationsHashTree.Create;
  try
    LAmount := 0;
    LFee := 0;
    for I := 0 to High(AExpected) do begin
      Check(FPool.Operation[I]=AExpected[I],'Replay order and registered operation identity');
      Check(FPool.OperationsHashTree.IndexOfOperation(AExpected[I])=I,'SHA lookup position');
      Check(FPool.OperationsHashTree.IndexOfOpReference(AExpected[I].GetOpReference)=I,
        'Operation reference lookup position');
      Check(LExpected.AddOperationToHashTree(AExpected[I]),'Expected sequence insertion');
      Inc(LAmount,AExpected[I].OperationAmount);
      Inc(LFee,AExpected[I].OperationFee);
    end;
    Check(TBaseType.Equals(LExpected.HashTree,FPool.OperationsHashTree.HashTree),'Ordered operation hash');
    Check(TBaseType.Equals(FPool.OperationBlock.operations_hash,FPool.OperationsHashTree.HashTree),
      'Operation header hash');
    Check(FPool.OperationsHashTree.TotalAmount=LAmount,'Total amount');
    Check(FPool.OperationsHashTree.TotalFee=LFee,'Total fee');
    Check(FPool.OperationBlock.fee=LFee,'Operation header fee');
    Check(FPool.SafeBoxTransaction.TotalFee=LFee,'Replayed account transaction fee');
    Check(FBank.SafeBox.Account(0).n_operation=0,'Replay never commits the confirmed bank');
  finally LExpected.Free; end;
end;

procedure TMempoolSanitizeChecks.CheckAllValid;
var LOld: TOperationsHashTree; LOne,LTwo,LThree: TPCOperation;
begin
  Reset;
  LOne := Add(1); LTwo := Add(2); LThree := Add(3);
  LOld := FPool.OperationsHashTree;
  LOld.OnChanged := Changed;
  FPool.SanitizeOperations;
  Check(FPool.OperationsHashTree=LOld,'All-valid replay retains original tree owner');
  Check(FChanges=0,'Sanitization emits no new changed callback');
  Check(not Assigned(FPool.OperationsHashTree.OnChanged),'Legacy replacement clears changed callback');
  AssertSequence([LOne,LTwo,LThree]);
  FPool.SanitizeOperations;
  Check(FPool.OperationsHashTree=LOld,'Repeated all-valid replay retains indexes');
  AssertSequence([LOne,LTwo,LThree]);
  Log('PASS all-valid order/hash/fee/header/callback and repeated retention');
end;

procedure TMempoolSanitizeChecks.CheckRejectedAndDuplicates;
var LOld: TOperationsHashTree; LOne,LTwo: TPCOperation;
begin
  Reset;
  LOne := Add(1);
  Add(3); // Incorrect nonce; replay must continue with the valid next operation.
  LTwo := Add(2);
  LOld := FPool.OperationsHashTree;
  FPool.SanitizeOperations;
  Check(FPool.OperationsHashTree<>LOld,'Rejection publishes the materialized replacement');
  AssertSequence([LOne,LTwo]);
  Reset;
  LOne := Add(1);
  Check(FPool.OperationsHashTree.AddOperationToHashTree(LOne),'Duplicate input insertion');
  Add(1,2); // Same operation reference, different signature/hash/fee.
  LTwo := Add(2);
  FPool.SanitizeOperations;
  AssertSequence([LOne,LTwo]);
  Log('PASS invalid nonce, duplicate SHA/reference and later valid operation');
end;

procedure TMempoolSanitizeChecks.CheckDeletedDuplicateIndex;
var LOld: TOperationsHashTree; LOne: TPCOperation;
begin
  Reset;
  LOne := Add(1);
  Check(FPool.OperationsHashTree.AddOperationToHashTree(LOne),'Duplicate insertion before delete');
  FPool.OperationsHashTree.Delete(0);
  Check(FPool.OperationsHashTree.IndexOfOperation(LOne)=-1,'Actual deleted duplicate leaves absent SHA representative');
  Check(FPool.OperationsHashTree.IndexOfOpReference(LOne.GetOpReference)=-1,
    'Actual deleted duplicate leaves absent reference representative');
  LOld := FPool.OperationsHashTree;
  FPool.SanitizeOperations;
  Check(FPool.OperationsHashTree<>LOld,'Invalid lookup indexes prohibit fast retention');
  AssertSequence([LOne]);
  Log('PASS actual duplicate Delete gap repaired by legacy fallback');
end;

procedure TMempoolSanitizeChecks.CheckZeroFeeSignerLimit;
var LOld: TOperationsHashTree; LOne,LTwo,LCandidate: TPCOperation;
begin
  Reset;
  LOne := Add(1,0);
  LOld := FPool.OperationsHashTree;
  FPool.SanitizeOperations;
  Check(FPool.OperationsHashTree=LOld,'Zero-fee all-valid tree retained across limit change');
  Check(FPool.OperationsHashTree.Max0feeOperationsBySigner=1,'Current protocol zero-fee limit adopted');
  Check(FPool.OperationsHashTree.CountOperationsBySameSignerWithoutFee(0)=1,'Signer metadata adopted');
  AssertSequence([LOne]);
  LCandidate := TOpTransaction.CreateTransaction(CT_PROTOCOL_5,0,2,1,FKey,1,0,CT_TOperationPayload_NUL);
  try Check(not FPool.OperationsHashTree.CanAddOperationToHashTree(LCandidate),'Second zero-fee signer request rejected');
  finally LCandidate.Free; end;
  Reset;
  LOne := Add(1,0); Add(2,0); LTwo := Add(2,1);
  FPool.SanitizeOperations;
  AssertSequence([LOne,LTwo]);
  Check(FPool.OperationsHashTree.CountOperationsBySameSignerWithoutFee(0)=1,'Fallback zero-fee signer metadata');
  Log('PASS protocol fee limit change, signer metadata and fee-paying continuation');
end;

procedure TMempoolSanitizeChecks.CheckReplayException;
var LOne,LThrow: TPCOperation; LRaised: Boolean;
  LOriginal,LAfter: TMemoryStream; LOriginalTree: TOperationsHashTree;
begin
  Reset;
  LOne := Add(1);
  LThrow := TThrowingTransaction.CreateTransaction(CT_PROTOCOL_5,0,2,1,FKey,1,1,CT_TOperationPayload_NUL);
  try Check(FPool.OperationsHashTree.AddOperationToHashTree(LThrow),'Throwing operation inserted');
  finally LThrow.Free; end;
  LThrow := FPool.Operation[1];
  LOriginalTree := FPool.OperationsHashTree;
  LOriginal := TMemoryStream.Create;
  LAfter := TMemoryStream.Create;
  try
    Check(LOriginalTree.SaveOperationsHashTreeToStream(LOriginal,True),'Capture complete exception input');
    GThrowReplayOnce := True;
    LRaised := False;
    try FPool.SanitizeOperations;
    except on E: EExpectedReplayFailure do LRaised := True; end;
    Check(LRaised,'Replay exception propagates');
    Check(FPool.OperationsHashTree=LOriginalTree,'A failed replay retains the complete original tree owner');
    Check((FPool.Count=2) and (FPool.Operation[0]=LOne) and (FPool.Operation[1]=LThrow),
      'A failed replay retains every registered operation in its original order');
    Check(FPool.OperationsHashTree.IndexOfOperation(LThrow)=1,'Failed replay preserves its SHA representative');
    Check(FPool.OperationsHashTree.IndexOfOpReference(LThrow.GetOpReference)=1,'Failed replay preserves its reference representative');
    Check(FPool.OperationsHashTree.SaveOperationsHashTreeToStream(LAfter,True),'Serialize complete retained input');
    Check((LOriginal.Size=LAfter.Size) and CompareMem(LOriginal.Memory,LAfter.Memory,LOriginal.Size),
      'An exception must not replace the complete input with a partial accepted prefix');
    // The account overlay is not consumed after an exception. A complete retry
    // rebuilds it from the retained sequence before account reads may resume.
    FPool.SanitizeOperations;
    AssertSequence([LOne,LThrow]);
    Log('PASS replay exception preserves complete indexed input and releases locks for a successful retry');
  finally LAfter.Free; LOriginal.Free; end;
end;

procedure TMempoolSanitizeChecks.CheckRegistryOwnership;
var LClone: TOperationsHashTree; LOne: TPCOperation; LCount: Integer;
begin
  Reset;
  LCount := TPCOperationsStorage.PCOperationsStorage.Count;
  LOne := Add(1);
  LClone := TOperationsHashTree.Create;
  try
    LClone.CopyFromHashTree(FPool.OperationsHashTree);
    FPool.SanitizeOperations;
    AssertSequence([LOne]);
    Check(TPCOperationsStorage.PCOperationsStorage.Count=LCount+1,'Replay retains one registered operation');
    FPool.Clear(True);
    Check(LClone.GetOperation(0)=LOne,'External tree survives sanitizer and pool release');
    Check(LClone.IndexOfOperation(LOne)=0,'External tree lookup remains valid');
  finally LClone.Free; end;
  Check(TPCOperationsStorage.PCOperationsStorage.Count=LCount,'Final owner releases registry entry');
  LOne := Add(1);
  LClone := TOperationsHashTree.Create;
  try LClone.CopyFromHashTree(FPool.OperationsHashTree);
  finally LClone.Free; end;
  FPool.SanitizeOperations;
  AssertSequence([LOne]);
  Log('PASS external operation owners survive either release order without registry growth');
end;

procedure TMempoolSanitizeChecks.CheckRecoverRejection;
var LOne,LRecover: TPCOperation;
begin
  Reset;
  LOne := Add(1);
  LRecover := TOpRecoverFounds.Create(CT_PROTOCOL_5,0,2,1,FKey.PublicKey);
  try Check(FPool.OperationsHashTree.AddOperationToHashTree(LRecover),'Recover input inserted');
  finally LRecover.Free; end;
  Check(FPool.OperationsHashTree.HasOpRecoverOperations,'Input recover flag set');
  FPool.SanitizeOperations;
  AssertSequence([LOne]);
  Check(not FPool.OperationsHashTree.HasOpRecoverOperations,'Rejected recover does not leak metadata');
  Reset;
  FPool.SanitizeOperations;
  AssertSequence([]);
  Log('PASS rejected recover metadata and empty replay');
end;

procedure TMempoolSanitizeChecks.CheckReferenceBoundaries;
var I: Integer; LOriginal: TOperationsHashTree; LOperation: TReferenceBoundaryTransaction;
  LAccepted: array[0..4] of TPCOperation; LExpected: TOperationsHashTree;
begin
  Reset;
  Check(FPool.OperationsHashTree.IndexOfOpReference(High(UInt64))=-1,'Empty UInt64 lookup');
  for I := 1 to 5 do begin
    LOperation := TReferenceBoundaryTransaction.CreateTransaction(CT_PROTOCOL_5,0,I,1,
      FKey,1,1,CT_TOperationPayload_NUL);
    try
      LOperation.ForceLookupCollision;
      Check(FPool.OperationsHashTree.AddOperationToHashTree(LOperation),'Mixed-half reference insertion');
      LAccepted[I-1] := FPool.Operation[I-1];
    finally LOperation.Free; end;
  end;
  Add(99); // Force the bulk materialization path after a real nonce rejection.
  LOriginal := FPool.OperationsHashTree;
  FPool.SanitizeOperations;
  Check(FPool.OperationsHashTree<>LOriginal,'Rejected input materializes the bulk indexes');
  Check(FPool.Count=5,'All five inherited transactions replayed exactly');
  Check(FPool.OperationsHashTree.IndexOfOpReference(0)=0,'Duplicate zero-reference keeps first operation');
  Check(FPool.OperationsHashTree.IndexOfOpReference(UInt64(1) shl 63)=1,'High-bit boundary lookup');
  Check(FPool.OperationsHashTree.IndexOfOpReference(High(UInt64))=2,'Maximum UInt64 lookup');
  Check(FPool.OperationsHashTree.IndexOfOpReference(High(Cardinal))=3,'Maximum low-half lookup');
  Check(FPool.OperationsHashTree.IndexOfOpReference((UInt64(1) shl 63)+1)=-1,'Absent mixed-half reference');
  LExpected := TOperationsHashTree.Create;
  try
    for I := 0 to 4 do begin
      Check(FPool.Operation[I]=LAccepted[I],'Bulk sorting never reorders the operations');
      Check(FPool.OperationsHashTree.IndexOfOperation(LAccepted[I])=0,'SHA collision keeps first operation');
      Check(LExpected.AddOperationToHashTree(LAccepted[I]),'Expected boundary sequence');
    end;
    Check(TBaseType.Equals(LExpected.HashTree,FPool.OperationsHashTree.HashTree),'Bulk ordered hash retained');
    Check(FPool.SafeBoxTransaction.Account(0).n_operation=5,'Inherited replay advanced the actual nonce five times');
  finally LExpected.Free; end;
  Log('PASS UInt64 zero/high-bit/max boundaries and first duplicate SHA/reference representatives');
end;

procedure TMempoolSanitizeChecks.CheckProtocolSignatureCaches;
var LSource,LCandidate: TPCOperation;
  LSourceTree,LTargetTree: TOperationsHashTree;
  LTransaction: TPCSafeBoxTransaction; LPrevious: TAccountPreviousBlockInfo;
  LStream: TMemoryStream; LError,LPreviousDirectory,LDirectory,LTemporaryRoot: String;
  LNode: TNode; LPool,LTarget: TPCOperationsComp; LHeader: TOperationBlock; LID: TGUID;

  function UnverifiedClone(AProtocol: Word): TPCOperation;
  begin
    Result := TOpTransaction.Create(AProtocol);
    try
      LStream.Clear;
      Check(LSource.SaveToNettransfer(LStream),'Source wire serialization');
      LStream.Position := 0;
      Check(Result.LoadFromNettransfer(LStream),'Cross-version identical wire loading');
      Check(TBaseType.Equals(LSource.Sha256,Result.Sha256),'Counterexample has the same operation wire hash');
      Check(not Result.HasValidSignature,'Loaded clone starts unverified');
    except Result.Free; raise; end;
  end;

begin
  Reset;
  LSource := TOpTransaction.CreateTransaction(CT_PROTOCOL_3,0,1,1,FKey,1,1,CT_TOperationPayload_NUL);
  LSourceTree := TOperationsHashTree.Create;
  LTargetTree := TOperationsHashTree.Create;
  LStream := TMemoryStream.Create;
  try
    LTransaction := TPCSafeBoxTransaction.Create(FBank.SafeBox);
    LPrevious := TAccountPreviousBlockInfo.Create;
    try
      Check(LSource.DoOperation(LPrevious,LTransaction,LError),'Source signature verifies through actual execution: '+LError);
      Check(LSource.HasValidSignature,'Source verified cache is populated');
    finally LPrevious.Free; LTransaction.Free; end;
    Check(LSourceTree.AddOperationToHashTree(LSource),'Verified source registration');
    LCandidate := UnverifiedClone(CT_PROTOCOL_4);
    try
      Check(not TBaseType.Equals(LSource.GetDigestToSign,LCandidate.GetDigestToSign),
        'Identical wire/hash has different actual protocol signing digest');
      Check(LTargetTree.AddOperationToHashTree(LCandidate),'Cross-version target registration');
    finally LCandidate.Free; end;
    LSourceTree.MarkVerifiedECDSASignatures(LTargetTree);
    Check(not LTargetTree.GetOperation(0).HasValidSignature,'Tree must not copy cache across operation protocols');
    LTransaction := TPCSafeBoxTransaction.Create(FBank.SafeBox);
    LPrevious := TAccountPreviousBlockInfo.Create;
    try Check(not LTargetTree.GetOperation(0).DoOperation(LPrevious,LTransaction,LError),
      'Original signature is actually invalid under the different protocol digest');
    finally LPrevious.Free; LTransaction.Free; end;
    LTargetTree.ClearHastThree;
    LCandidate := UnverifiedClone(CT_PROTOCOL_3);
    try Check(LTargetTree.AddOperationToHashTree(LCandidate),'Same-protocol target registration');
    finally LCandidate.Free; end;
    LSourceTree.MarkVerifiedECDSASignatures(LTargetTree);
    Check(LTargetTree.GetOperation(0).HasValidSignature,'Same-protocol tree cache reuse remains enabled');

    Check(not TNode.NodeExists,'Node cache check requires an isolated node lifetime');
    LTemporaryRoot := GetEnvironmentVariable('TEMP');
    Check(LTemporaryRoot<>'','TEMP must identify the owned cache-check directory');
    CreateGUID(LID);
    LDirectory := IncludeTrailingPathDelimiter(LTemporaryRoot)+'SignatureCache-'+GUIDToString(LID);
    ForceDirectories(LDirectory);
    LPreviousDirectory := TNode.GetPascalCoinDataFolder;
    TNode.SetPascalCoinDataFolder(LDirectory);
    try
      LNode := TNode.Create(nil);
      try
        LNode.BroadcastData := False;
        Check(not LNode.NetServer.Active,'Signature cache check never listens');
        LPool := LNode.LockMempoolWrite;
        try
          LHeader := LPool.OperationBlock;
          LHeader.protocol_version := CT_PROTOCOL_3;
          LPool.SetOperationBlock(LHeader);
          Check(LPool.OperationsHashTree.AddOperationToHashTree(LSourceTree.GetOperation(0)),
            'Actual node verified source registration');
        finally LNode.UnlockMempoolWrite; end;
        LTarget := TPCOperationsComp.Create(nil);
        try
          LCandidate := UnverifiedClone(CT_PROTOCOL_3);
          try Check(LTarget.OperationsHashTree.AddOperationToHashTree(LCandidate),'Actual node target registration');
          finally LCandidate.Free; end;
          LHeader := LTarget.OperationBlock;
          LHeader.protocol_version := CT_PROTOCOL_4;
          LTarget.SetOperationBlock(LHeader);
          LNode.MarkVerifiedECDSASignaturesFromMemPool(LTarget);
          Check(not LTarget.Operation[0].HasValidSignature,'Node rejects differing block protocols even when operations match');
          LHeader.protocol_version := CT_PROTOCOL_3;
          LTarget.SetOperationBlock(LHeader);
          LNode.MarkVerifiedECDSASignaturesFromMemPool(LTarget);
          Check(LTarget.Operation[0].HasValidSignature,'Node same-block/same-operation protocol cache reuse remains enabled');
          LTarget.Clear(True);
          LCandidate := UnverifiedClone(CT_PROTOCOL_4);
          try Check(LTarget.OperationsHashTree.AddOperationToHashTree(LCandidate),'Mixed-operation node target');
          finally LCandidate.Free; end;
          LHeader := LTarget.OperationBlock;
          LHeader.protocol_version := CT_PROTOCOL_3;
          LTarget.SetOperationBlock(LHeader);
          LNode.MarkVerifiedECDSASignaturesFromMemPool(LTarget);
          Check(not LTarget.Operation[0].HasValidSignature,'Node also retains the per-operation protocol guard');
        finally LTarget.Free; end;
      finally LNode.Free; end;
    finally TNode.SetPascalCoinDataFolder(LPreviousDirectory); end;
    Log('PASS same-wire/different-digest cache rejection and same-protocol positives through tree and actual Node');
  finally LStream.Free; LTargetTree.Free; LSourceTree.Free; LSource.Free; end;
end;

procedure TMempoolSanitizeChecks.Run;
var LCheck: TMempoolSanitizeCheck;
begin
  for LCheck := Low(TMempoolSanitizeCheck) to High(TMempoolSanitizeCheck) do
    RunOne(LCheck);
  Log('PASS all nine sanitizer/signature-cache groups; no peers or wallet data');
end;

procedure TMempoolSanitizeChecks.RunOne(ACheck: TMempoolSanitizeCheck);
var LCount: Integer;
begin
  Reset;
  LCount := TPCOperationsStorage.PCOperationsStorage.Count;
  case ACheck of
    mscAllValid: CheckAllValid;
    mscRejectedAndDuplicates: CheckRejectedAndDuplicates;
    mscDeletedDuplicateIndex: CheckDeletedDuplicateIndex;
    mscZeroFeeSignerLimit: CheckZeroFeeSignerLimit;
    mscReplayException: CheckReplayException;
    mscRegistryOwnership: CheckRegistryOwnership;
    mscRecoverRejection: CheckRecoverRejection;
    mscReferenceBoundaries: CheckReferenceBoundaries;
    mscProtocolSignatureCaches: CheckProtocolSignatureCaches;
  end;
  Reset;
  Check(TPCOperationsStorage.PCOperationsStorage.Count=LCount,'All fixture registry references released');
end;

function RunMempoolSanitizeCheck(ACheck: TMempoolSanitizeCheck;
  const ALogPath: String): Integer;
var LChecks: TMempoolSanitizeChecks;
begin
  Result := 1;
  LChecks := TMempoolSanitizeChecks.Create(ALogPath);
  try
    try
      LChecks.RunOne(ACheck);
      Result := 0;
    except
      on E: Exception do begin
        LChecks.Log('FAIL ' + E.ClassName + ': ' + E.Message);
        raise;
      end;
    end;
  finally LChecks.Free; end;
end;

function RunMempoolSanitizeChecks(const ALogPath: String): Integer;
var LChecks: TMempoolSanitizeChecks;
begin
  Result := 1;
  LChecks := TMempoolSanitizeChecks.Create(ALogPath);
  try
    try
      LChecks.Run;
      Result := 0;
    except
      on E: Exception do begin
        LChecks.Log('FAIL ' + E.ClassName + ': ' + E.Message);
        raise;
      end;
    end;
  finally LChecks.Free; end;
end;

end.
