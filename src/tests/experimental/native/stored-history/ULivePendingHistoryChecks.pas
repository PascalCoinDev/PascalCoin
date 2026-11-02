unit ULivePendingHistoryChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Checks the live pending-history wrapper against actual node admissions. The
  generated bank and node use an owned temporary folder, without peers or wallet. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

procedure CheckLivePendingHistory;

implementation

uses
  Classes, SysUtils, Generics.Collections, UMemory, UCrypto, UAccounts,
  UBlockChain, UOpTransaction, UTxMultiOperation, UPCDataTypes, UCoreUtils,
  UNode, UConst, UBaseTypes, UOrderedList;

type
  TLivePendingHistorySafeBox = class(TPCSafeBox)
  public
    procedure BuildFunded(const AKey: TAccountKey);
  end;

  TCountingLiveHistoryTransaction = class(TOpTransaction)
  public
    class var AffectedCalls: Integer;
    procedure AffectedAccounts(AList: TOrderedList<Cardinal>); override;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create('Live pending history: ' + AMessage);
end;

procedure TLivePendingHistorySafeBox.BuildFunded(const AKey: TAccountKey);
var
  LBlock: TOperationBlock;
  I: Integer;
begin
  // Creation-block update metadata remains genuine: it is also used by the
  // previous-update index when these signed operations enter the live pool.
  for I := 0 to 159 do
  begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := I;
    LBlock.account_key := AKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := I * CT_NewLineSecondsAvg;
    if I in [20, 21, 22] then
      LBlock.reward := 1000000;
    AddNew(LBlock);
  end;
  DoUpgradeToProtocol5;
  DoUpgradeToProtocol6;
end;

procedure TCountingLiveHistoryTransaction.AffectedAccounts(
  AList: TOrderedList<Cardinal>);
begin
  Inc(AffectedCalls);
  inherited AffectedAccounts(AList);
end;

procedure RequireSameRows(const AExpected, AActual: TArray<TOperationResume>);
var
  I: Integer;
begin
  Require(Length(AExpected) = Length(AActual), 'history row count differs from the generic helper');
  for I := 0 to High(AExpected) do
  begin
    Require((AExpected[I].AffectedAccount = AActual[I].AffectedAccount) and
      (AExpected[I].NOpInsideBlock = AActual[I].NOpInsideBlock) and
      (AExpected[I].Block = AActual[I].Block) and
      (AExpected[I].OpType = AActual[I].OpType) and
      (AExpected[I].OpSubtype = AActual[I].OpSubtype) and
      (AExpected[I].SignerAccount = AActual[I].SignerAccount) and
      (AExpected[I].DestAccount = AActual[I].DestAccount) and
      (AExpected[I].Balance = AActual[I].Balance) and
      (AExpected[I].Amount = AActual[I].Amount) and
      (AExpected[I].Fee = AActual[I].Fee) and
      TBaseType.Equals(AExpected[I].OperationHash, AActual[I].OperationHash) and
      TBaseType.Equals(AExpected[I].OriginalPayload.payload_raw,
        AActual[I].OriginalPayload.payload_raw),
      'history order, duplicate account context or value changed at row ' + IntToStr(I));
  end;
end;

procedure CheckLivePendingHistory;
var
  LDisposables: TDisposables;
  LKey: TECPrivateKey;
  LNode: TNode;
  LPool: TPCOperationsComp;
  LSubmitted: TOperationsHashTree;
  LAffected: TOrderedList<Cardinal>;
  LOperation: TPCOperation;
  LRows: TArray<TOperationResume>;
  LSender: TMultiOpSender;
  LReceiver: TMultiOpReceiver;
  LChanger: TMultiOpChangeInfo;
  LSenders: TMultiOpSenders;
  LReceivers: TMultiOpReceivers;
  LChangers: TMultiOpChangesInfo;
  LPreviousDirectory, LDirectory, LError: String;
  LID, LDataID: TGUID;
  I, J, LAdded: Integer;

  procedure Add(AOperation: TPCOperation);
  begin
    try
      Require(LSubmitted.AddOperationToHashTree(AOperation), 'could not stage a generated signed operation');
    finally
      AOperation.Free;
    end;
  end;

  procedure AddTransaction(ASender, ANonce, ATarget: Cardinal; AAmount: UInt64);
  begin
    Add(TCountingLiveHistoryTransaction.CreateTransaction(CT_PROTOCOL_6,
      ASender, ANonce, ATarget, LKey, AAmount, 10, CT_TOperationPayload_NUL));
  end;

  procedure CompareQuery(const AAccounts: array of Cardinal;
    ASkip: Integer = 0; ATake: Integer = MaxInt);
  var
    LExpected, LActual: TArray<TOperationResume>;
  begin
    // This independent existing helper deliberately has no touched-index gate.
    // It is the oracle for every row, including duplicate requested accounts.
    LPool := LNode.LockMempoolRead;
    try
      LExpected := TCoreTool.GetPendingOperationsAffectingAccounts(
        LPool.OperationsHashTree, LPool.SafeBoxTransaction,
        LPool.OperationBlock.block, AAccounts, ASkip, ATake);
    finally
      LNode.UnlockMempoolRead;
    end;
    Require(Length(LExpected) > 0, 'touched-account oracle unexpectedly returned no rows');
    TCountingLiveHistoryTransaction.AffectedCalls := 0;
    LActual := LNode.GetPendingOperationsAffectingAccounts(AAccounts, ASkip, ATake);
    RequireSameRows(LExpected, LActual);
    Require(TCountingLiveHistoryTransaction.AffectedCalls = 4,
      'touched query did not retain exactly one pass over admitted transactions');
  end;

begin
  Require(not TNode.NodeExists, 'the fixture requires an isolated node lifetime');
  Require(GetEnvironmentVariable('TEMP') <> '', 'TEMP must identify the owned fixture directory');
  LPreviousDirectory := TNode.GetPascalCoinDataFolder;
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
    'LivePendingHistory-' + GUIDToString(LID);
  Require(ForceDirectories(LDirectory), 'could not create the owned fixture directory');
  TCrypto.InitCrypto;
  LKey := LDisposables.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  LSubmitted := LDisposables.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  LAffected := LDisposables.AddObject(TOrderedList<Cardinal>.Create(False,
    TComparison_Cardinal)) as TOrderedList<Cardinal>;
  LNode := nil;
  TNode.SetPascalCoinDataFolder(LDirectory);
  try
    LNode := TNode.Create(nil);
    LNode.BroadcastData := False;
    Require(not LNode.NetServer.Active, 'the fixture must not listen on the network');
    TLivePendingHistorySafeBox(LNode.Bank.SafeBox).BuildFunded(LKey.PublicKey);
    LPool := LNode.LockMempoolWrite;
    try
      // The private generated bank was built without publishing a network block.
      // Reconcile its empty pool through the same normal core replay operation.
      LPool.SanitizeOperations;
      Require(LPool.OperationBlock.block = 160, 'live pool was not advanced to the generated bank');
      Require(LPool.OperationBlock.protocol_version = CT_PROTOCOL_6, 'live pool protocol was not reconciled');
    finally
      LNode.UnlockMempoolWrite;
    end;

    AddTransaction(100, 1, 101, 100);
    AddTransaction(105, 1, 106, 200);
    AddTransaction(100, 2, 102, 300);
    AddTransaction(110, 1, 101, 400);
    Add(TOpChangeAccountInfo.CreateChangeAccountInfo(CT_PROTOCOL_6,
      100, 3, 115, LKey, False, CT_TECDSA_Public_Nul, False, nil,
      True, 99, False, nil, 10, CT_TOperationPayload_NUL));
    LDataID := Default(TGUID);
    Add(TOpData.CreateOpData(CT_PROTOCOL_6, 105, 110, 116, LKey,
      2, 1, 0, LDataID, 3, 2, CT_TOperationPayload_NUL));
    LSender := CT_TMultiOpSender_NUL;
    LSender.Account := 110;
    LSender.N_Operation := 2;
    LSender.Amount := 10;
    LReceiver := CT_TMultiOpReceiver_NUL;
    LReceiver.Account := 117;
    LReceiver.Amount := 9;
    LChanger := CT_TMultiOpChangeInfo_NUL;
    LChanger.Account := 118;
    LChanger.N_Operation := 1;
    LChanger.Changes_type := [account_type];
    LChanger.New_Type := 77;
    LSenders := TMultiOpSenders.Create(LSender);
    LReceivers := TMultiOpReceivers.Create(LReceiver);
    LChangers := TMultiOpChangesInfo.Create(LChanger);
    Add(TOpMultiOperation.CreateMultiOperation(CT_PROTOCOL_6,
      LSenders, LReceivers, LChangers, [LKey], [LKey]));

    LError := '';
    LAdded := LNode.AddOperations(nil, LSubmitted, nil, LError);
    Require(LAdded = 7, 'actual node did not admit all generated operations: ' + LError);
    LPool := LNode.LockMempoolRead;
    try
      Require(LPool.Count = 7, 'the live pool lost an admitted operation');
      for I := 0 to LPool.Count - 1 do
      begin
        LOperation := LPool.Operation[I];
        if I < 4 then
          Require(LOperation is TCountingLiveHistoryTransaction,
            'actual node admission erased the counting transaction subclass');
        LAffected.Clear;
        LOperation.AffectedAccounts(LAffected);
        for J := 0 to LAffected.Count - 1 do
          Require(LPool.PreviousUpdatedBlocks.IndexOfAccount(LAffected[J]) >= 0,
            'the live touched index omitted an admitted operation account');
      end;
      Require(LPool.PreviousUpdatedBlocks.IndexOfAccount(119) < 0,
        'the untouched fixture account was modified');
    finally
      LNode.UnlockMempoolRead;
    end;

    TCountingLiveHistoryTransaction.AffectedCalls := 0;
    LRows := LNode.GetPendingOperationsAffectingAccounts([119, 120, 119], 0, MaxInt);
    Require(Length(LRows) = 0, 'untouched accounts returned pending rows');
    Require(TCountingLiveHistoryTransaction.AffectedCalls = 0,
      'untouched account selection scanned the entire live pending pool');
    LRows := LNode.GetPendingOperationsAffectingAccounts([], 0, MaxInt);
    Require(Length(LRows) = 0, 'empty account selection returned rows');
    LRows := LNode.GetPendingOperationsAffectingAccounts([100], 0, 0);
    Require(Length(LRows) = 0, 'zero-take query returned rows');
    Require(TCountingLiveHistoryTransaction.AffectedCalls = 0,
      'empty or zero-take query scanned the live pending pool');
    CompareQuery([101]);
    CompareQuery([100]);
    CompareQuery([119, 101, 119, 100, 101]);
    CompareQuery([119, 101, 119, 100, 101], 1, 3);
    CompareQuery([115, 116, 117, 118]);
    CompareQuery([105, 106, 110]);
  finally
    // Join the node's owned workers before restoring the process data directory;
    // no worker may accidentally save into the user's normal wallet folder.
    LNode.Free;
    TNode.SetPascalCoinDataFolder(LPreviousDirectory);
  end;
end;

end.
