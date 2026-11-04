unit UStoredHistoryChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Shared actual generated stored-history fixture for FPCUnit and native Delphi.
  Core validates and commits the signed operations; this is not a PoW archive. }

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

type
  TStoredHistoryCheck = (shcWideBalance, shcReverseBoundary, shcAllHistory,
    shcActualPage, shcFiniteDepth, shcTipOnly, shcOldAccount,
    shcFinitePage, shcZeroTake, shcFiniteWithPending, shcLivePendingFastPath,
    shcActualBlockHashes, shcLayeredBeforePrimary, shcLayeredPrimaryGap,
    shcLayeredMissingBlock);

procedure CheckStoredHistory(ACheck: TStoredHistoryCheck);
function RunStoredHistoryChecks(const ALogFile: String): Integer;

implementation

uses Classes, SysUtils, Generics.Collections, UCrypto, UAccounts, UBlockChain,
  UOpTransaction, UPCDataTypes, UCoreUtils, UNode, UConst, UBaseTypes,
  UDataSources, UCommon.Data, UFileStorage, ULivePendingHistoryChecks,
  UAbstractMemBlockchainStorage;

type
  TStoredHistorySafeBox = class(TPCSafeBox)
  public
    procedure BuildFunded(const AKey: TAccountKey; AInitialBalance: Int64);
  end;

  TStoredHistoryStorage = class(TFileStorage)
  protected
    function GetFirstBlockNumber: Int64; override;
    function GetLastBlockNumber: Int64; override;
    function DoLoadBlockChain(Operations: TPCOperationsComp;
      Block: Cardinal): Boolean; override;
    function DoBlockExists(Block: Cardinal): Boolean; override;
  public
    StoredBlock: TPCOperationsComp;
    LinkedBlocks: TObjectList<TPCOperationsComp>;
    LoadedBlocks: TList<Cardinal>;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TStoredHistoryPageSource = class(TAccountsOperationsDataSource)
  public
    function ReadPage(out ATable: TDataTable): TPageFetchResult;
  end;

  TStoredHistoryFixture = class
  private
    FKey: TECPrivateKey;
    FNode: TNode;
    FSnapshot: TPCSafeBoxTransaction;
    FPrevious: TAccountPreviousBlockInfo;
    FStoredBlock: TPCOperationsComp;
    FLinkedBlocks: TObjectList<TPCOperationsComp>;
    FPreviousDirectory: String;
    procedure BuildStoredHistory(AInitialBalance: Int64; AFirstAmount,
      ASecondAmount: UInt64);
    procedure BuildLinkedHistory;
    procedure PrepareLayeredHistory(AIncludeFirstInPrimary, AIncludeMiddleInLegacy: Boolean);
    function History(ABlockDepth: Cardinal; ASkip: Integer = 0;
      ATake: Integer = MaxInt): TArray<TOperationResume>;
    procedure RequireLoaded(const AExpected: array of Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StoredBalanceAboveCardinalRetainsExactValue;
    procedure ReverseSpendCrossesCardinalBoundary;
    procedure AllHistoryDepthIncludesTheStoredBlock;
    procedure ActualHistoryPagePublishesSigned64BitBalances;
    procedure FiniteDepthStopsAtTheInclusiveBoundary;
    procedure ZeroDepthReadsTheTipOnly;
    procedure OldAccountAvoidsStoredReads;
    procedure FinitePagesStopOnceTheRequestedRowsAreFound;
    procedure ZeroTakeAvoidsStoredReads;
    procedure FiniteHistoryRetainsPendingOperations;
    procedure StoredOperationHashesUseActualBlockHeight;
    procedure MaximumReadsLegacyHistoryBeforeThePrimaryRange;
    procedure MaximumReadsLegacyHistoryInsideAPrimaryGap;
    procedure MissingLegacyBlockDoesNotPublishAnotherBlocksOperations;
  end;

procedure RequireTrue(const AMessage: String; ACondition: Boolean); overload;
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure RequireTrue(ACondition: Boolean); overload;
begin
  RequireTrue('Actual stored-history fixture assertion failed', ACondition);
end;

procedure RequireFalse(const AMessage: String; ACondition: Boolean);
begin
  RequireTrue(AMessage, not ACondition);
end;

procedure RequireEquals(const AMessage: String; AExpected, AActual: Int64); overload;
begin
  if AExpected <> AActual then raise Exception.Create(AMessage +
    ': expected ' + IntToStr(AExpected) + ', actual ' + IntToStr(AActual));
end;

procedure RequireEquals(AExpected, AActual: Int64); overload;
begin
  RequireEquals('Actual stored-history value differs', AExpected, AActual);
end;
procedure TStoredHistorySafeBox.BuildFunded(const AKey: TAccountKey;
  AInitialBalance: Int64);
var LBlock: TOperationBlock; I: Integer;
begin
  // Generated rewards fund account 100 without manually changing the safebox's
  // aggregate fee accounting; the later signed spends can use real Commit.
  for I := 0 to 159 do begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := I;
    LBlock.account_key := AKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := I * CT_NewLineSecondsAvg;
    if I = 20 then LBlock.reward := AInitialBalance;
    AddNew(LBlock);
  end;
  DoUpgradeToProtocol5;
  DoUpgradeToProtocol6;
end;

constructor TStoredHistoryStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadedBlocks := TList<Cardinal>.Create;
end;

destructor TStoredHistoryStorage.Destroy;
begin
  LoadedBlocks.Free;
  inherited Destroy;
end;

function TStoredHistoryStorage.GetFirstBlockNumber: Int64;
begin
  Result := 160;
end;

function TStoredHistoryStorage.GetLastBlockNumber: Int64;
begin
  if Assigned(LinkedBlocks) and (LinkedBlocks.Count > 0) then
    Result := LinkedBlocks[LinkedBlocks.Count - 1].OperationBlock.block
  else
    Result := 160;
end;

function TStoredHistoryStorage.DoLoadBlockChain(Operations: TPCOperationsComp;
  Block: Cardinal): Boolean;
var
  I: Integer;
begin
  LoadedBlocks.Add(Block);
  if Assigned(LinkedBlocks) then begin
    for I := 0 to LinkedBlocks.Count - 1 do
      if LinkedBlocks[I].OperationBlock.block = Block then begin
        Operations.CopyFrom(LinkedBlocks[I]);
        Exit(True);
      end;
    Exit(False);
  end;
  Result := Assigned(StoredBlock) and (Block = StoredBlock.OperationBlock.block);
  if Result then Operations.CopyFrom(StoredBlock);
end;

function TStoredHistoryStorage.DoBlockExists(Block: Cardinal): Boolean;
var I: Integer;
begin
  // Match the fixture's stored blocks without recording an actual block load.
  if Assigned(LinkedBlocks) then begin
    for I := 0 to LinkedBlocks.Count - 1 do
      if LinkedBlocks[I].OperationBlock.block = Block then Exit(True);
    Exit(False);
  end;
  Result := Assigned(StoredBlock) and (StoredBlock.OperationBlock.block = Block);
end;

function TStoredHistoryPageSource.ReadPage(out ATable: TDataTable): TPageFetchResult;
begin
  Result := FetchPage(TPageFetchParams.Create(0, 25, nil, foAnd), ATable);
end;

constructor TStoredHistoryFixture.Create;
var LID: TGUID; LDirectory: String;
begin
  inherited Create;
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  RequireFalse('Stored history owns a fresh node without peers', TNode.NodeExists);
  RequireTrue('Supply an owned TEMP directory', GetEnvironmentVariable('TEMP') <> '');
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
    'WalletStoredHistory-' + GUIDToString(LID);
  ForceDirectories(LDirectory);
  TNode.SetPascalCoinDataFolder(LDirectory);
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FNode := TNode.Create(nil);
  FNode.BroadcastData := False;
  FNode.Bank.StorageClass := TStoredHistoryStorage;
  FPrevious := TAccountPreviousBlockInfo.Create;
  FStoredBlock := TPCOperationsComp.Create(nil);
  FLinkedBlocks := TObjectList<TPCOperationsComp>.Create(True);
  TStoredHistoryStorage(FNode.Bank.Storage).StoredBlock := FStoredBlock;
end;

destructor TStoredHistoryFixture.Destroy;
begin
  // The node owns the storage adapter; its borrowed block remains alive until
  // the node and its saver have stopped. No global wallet or network is opened.
  FSnapshot.Free;
  FNode.Free;
  FLinkedBlocks.Free;
  FStoredBlock.Free;
  FPrevious.Free;
  FKey.Free;
  TNode.SetPascalCoinDataFolder(FPreviousDirectory);
  inherited;
end;

procedure TStoredHistoryFixture.BuildStoredHistory(AInitialBalance: Int64;
  AFirstAmount, ASecondAmount: UInt64);
var
  LBlock: TOperationBlock;
  LOperation: TOpTransaction;
  LError: String;
  I: Integer;
  LAmounts: array[0..1] of UInt64;
begin
  TStoredHistorySafeBox(FNode.Bank.SafeBox).BuildFunded(FKey.PublicKey,
    AInitialBalance);
  FSnapshot := TPCSafeBoxTransaction.Create(FNode.Bank.SafeBox);
  LAmounts[0] := AFirstAmount;
  LAmounts[1] := ASecondAmount;
  for I := 0 to 1 do begin
    LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_6, 100, I+1,
      101, FKey, LAmounts[I], 10, CT_TOperationPayload_NUL);
    try
      LError := '';
      RequireTrue('Stored fixture operations pass actual core validation: ' + LError,
        LOperation.DoOperation(FPrevious, FSnapshot, LError));
      RequireTrue(FStoredBlock.OperationsHashTree.AddOperationToHashTree(LOperation));
    finally LOperation.Free; end;
  end;
  FStoredBlock.PreviousUpdatedBlocks.CopyFrom(FPrevious);
  LBlock := CT_OperationBlock_NUL;
  LBlock.block := FNode.Bank.BlocksCount;
  LBlock.account_key := FKey.PublicKey;
  LBlock.protocol_version := CT_PROTOCOL_6;
  LBlock.protocol_available := CT_PROTOCOL_6;
  LBlock.compact_target := CT_MinCompactTarget_v1;
  LBlock.timestamp := LBlock.block * CT_NewLineSecondsAvg;
  LBlock.fee := 20;
  LBlock.operations_hash := FStoredBlock.OperationsHashTree.HashTree;
  FStoredBlock.SetOperationBlock(LBlock);
  // Commit the real signed-operation snapshot, then return its exact operation
  // block through storage. This tests history, not proof-of-work validation.
  LError := '';
  RequireTrue('The stored snapshot commits with consistent balances and fees: ' + LError,
    FSnapshot.Commit(LBlock, LError));
end;

procedure TStoredHistoryFixture.BuildLinkedHistory;
var
  LStored: TPCOperationsComp;
  LBlock: TOperationBlock;
  LOperation: TOpTransaction;
  LError: String;
  LBlockIndex, LOperationIndex, LOperationCount: Integer;
  LRecipient: Cardinal;
  LAmount: UInt64;
begin
  TStoredHistorySafeBox(FNode.Bank.SafeBox).BuildFunded(FKey.PublicKey, 10000);
  TStoredHistoryStorage(FNode.Bank.Storage).LinkedBlocks := FLinkedBlocks;
  for LBlockIndex := 0 to 2 do begin
    FreeAndNil(FSnapshot);
    FSnapshot := TPCSafeBoxTransaction.Create(FNode.Bank.SafeBox);
    FPrevious.Clear;
    LStored := TPCOperationsComp.Create(nil);
    FLinkedBlocks.Add(LStored);
    if LBlockIndex = 0 then
      LOperationCount := 2
    else
      LOperationCount := 1;
    for LOperationIndex := 0 to LOperationCount - 1 do begin
      if (LBlockIndex = 0) and (LOperationIndex = 1) then begin
        LRecipient := 102;
        LAmount := 100;
      end else begin
        LRecipient := 101;
        LAmount := UInt64(LBlockIndex + 1) * 10;
      end;
      LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_6, 100,
        FSnapshot.Account(100).n_operation + 1, LRecipient, FKey,
        LAmount, 1, CT_TOperationPayload_NUL);
      try
        LError := '';
        RequireTrue('Linked fixture operation validates: ' + LError,
          LOperation.DoOperation(FPrevious, FSnapshot, LError));
        RequireTrue(LStored.OperationsHashTree.AddOperationToHashTree(LOperation));
      finally
        LOperation.Free;
      end;
    end;
    LStored.PreviousUpdatedBlocks.CopyFrom(FPrevious);
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := FNode.Bank.BlocksCount;
    LBlock.account_key := FKey.PublicKey;
    LBlock.protocol_version := CT_PROTOCOL_6;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := LBlock.block * CT_NewLineSecondsAvg;
    LBlock.fee := LOperationCount;
    LBlock.operations_hash := LStored.OperationsHashTree.HashTree;
    LStored.SetOperationBlock(LBlock);
    LError := '';
    RequireTrue('Linked signed snapshot commits: ' + LError,
      FSnapshot.Commit(LBlock, LError));
  end;
  // Reuse the bank's normal post-commit notification so pending history and
  // its operation template observe the same final chain as the stored reader.
  FNode.Bank.UpdateValuesFromSafebox;
  TStoredHistoryStorage(FNode.Bank.Storage).LoadedBlocks.Clear;
end;

procedure TStoredHistoryFixture.PrepareLayeredHistory(AIncludeFirstInPrimary, AIncludeMiddleInLegacy: Boolean);
var LStorage: TAbstractMemBlockchainStorageSecondary; I: Integer;
begin
  BuildLinkedHistory;
  // Use both real file formats: older operations exist only in the legacy
  // archive, just as after restoring a checkpoint before creating its index.
  FNode.Bank.StorageClass := TAbstractMemBlockchainStorageSecondary;
  LStorage := TAbstractMemBlockchainStorageSecondary(FNode.Bank.Storage);
  RequireTrue('The layered history storage initializes in its fresh directory', LStorage.Initialize);
  LStorage.UseMultithread := False;
  LStorage.AutoFlushCache := False;
  // Fix the legacy archive's first block before saving the newer primary tip.
  RequireTrue('The legacy archive starts at the oldest generated block', LStorage.AuxStorage.SaveBlockChainBlock(FLinkedBlocks[0]));
  if AIncludeFirstInPrimary then
    RequireTrue('The primary index saves the oldest generated block', LStorage.SaveBlockChainBlock(FLinkedBlocks[0]));
  RequireTrue('The primary index saves the newest generated block', LStorage.SaveBlockChainBlock(FLinkedBlocks[2]));
  // Rewriting an older legacy block truncates its following archive. Populate
  // that archive last, in ascending order, to retain the intended middle block.
  for I := 0 to FLinkedBlocks.Count - 1 do
    if (I <> 1) or AIncludeMiddleInLegacy then
      RequireTrue('The legacy archive saves its generated signed block', LStorage.AuxStorage.SaveBlockChainBlock(FLinkedBlocks[I]));
  RequireTrue('The fixture must retain the intended legacy middle block or gap', LStorage.AuxStorage.BlockExists(161) = AIncludeMiddleInLegacy);
  RequireEquals('The primary index ends at the generated tip', 162, LStorage.LastBlock);
  if AIncludeFirstInPrimary then
    RequireEquals('The primary range surrounds its missing middle block', 160, LStorage.FirstBlock)
  else
    RequireEquals('The primary index starts after the older account update', 162, LStorage.FirstBlock);
  RequireEquals('The legacy archive begins before the primary-only tip', 160, LStorage.AuxStorage.FirstBlock);
end;

procedure TStoredHistoryFixture.RequireLoaded(const AExpected: array of Cardinal);
var
  LLoaded: TList<Cardinal>;
  I: Integer;
begin
  LLoaded := TStoredHistoryStorage(FNode.Bank.Storage).LoadedBlocks;
  RequireEquals('Stored history must make only the expected block reads',
    Length(AExpected), LLoaded.Count);
  for I := 0 to High(AExpected) do
    RequireEquals('Unexpected stored block traversal at index ' + IntToStr(I),
      AExpected[I], LLoaded[I]);
end;

function TStoredHistoryFixture.History(ABlockDepth: Cardinal; ASkip,
  ATake: Integer): TArray<TOperationResume>;
begin
  Result := FNode.GetStoredOperationsAffectingAccounts([100], ABlockDepth, ASkip, ATake);
end;

procedure TStoredHistoryFixture.FiniteDepthStopsAtTheInclusiveBoundary;
var
  LRows: TArray<TOperationResume>;
  LSource: TStoredHistoryPageSource;
  LTable: TDataTable;
  LResult: TPageFetchResult;
begin
  BuildLinkedHistory;
  LRows := History(1);
  RequireEquals('Recent and boundary operations are included', 2, Length(LRows));
  RequireEquals(162, LRows[0].Block);
  RequireEquals(161, LRows[1].Block);
  RequireLoaded([162, 161]);
  TStoredHistoryStorage(FNode.Bank.Storage).LoadedBlocks.Clear;
  LSource := TStoredHistoryPageSource.Create(nil);
  try
    LSource.Accounts := TArray<Cardinal>.Create(100);
    LSource.BlockDepth := 1;
    LResult := LSource.ReadPage(LTable);
    RequireEquals('The actual provider forwards the finite history depth', 2,
      LResult.TotalDataCount);
    RequireEquals(2, Length(LTable.Rows));
    RequireLoaded([162, 161]);
    LSource.BlockDepth := High(Cardinal);
    LResult := LSource.ReadPage(LTable);
    RequireEquals('Maximum depth includes older linked operations', 4,
      LResult.TotalDataCount);
    RequireEquals(4, Length(LTable.Rows));
    RequireTrue('Maximum must actually read the older linked block',
      TStoredHistoryStorage(FNode.Bank.Storage).LoadedBlocks.Contains(160));
    LRows := History(High(Cardinal));
    RequireEquals('The direct wrapper must accept the full unsigned depth', 4,
      Length(LRows));
    RequireEquals(160, LRows[2].Block);
  finally
    LSource.Free;
  end;
end;

procedure TStoredHistoryFixture.ZeroDepthReadsTheTipOnly;
var
  LRows: TArray<TOperationResume>;
begin
  BuildLinkedHistory;
  LRows := History(0);
  RequireEquals('Depth zero includes only the tip operation', 1, Length(LRows));
  RequireEquals(162, LRows[0].Block);
  RequireLoaded([162]);
end;

procedure TStoredHistoryFixture.OldAccountAvoidsStoredReads;
var
  LRows: TArray<TOperationResume>;
begin
  BuildLinkedHistory;
  LRows := FNode.GetStoredOperationsAffectingAccounts([102], 1, 0, MaxInt);
  RequireEquals('The selected account was last updated before the cutoff', 0,
    Length(LRows));
  RequireLoaded([]);
end;

procedure TStoredHistoryFixture.FinitePagesStopOnceTheRequestedRowsAreFound;
var
  LRows: TArray<TOperationResume>;
begin
  BuildLinkedHistory;
  LRows := History(2, 0, 1);
  RequireEquals('Taking one recent row must return it', 1, Length(LRows));
  RequireEquals(162, LRows[0].Block);
  RequireLoaded([162]);
  TStoredHistoryStorage(FNode.Bank.Storage).LoadedBlocks.Clear;
  LRows := History(2, 1, 1);
  RequireEquals('Skip/take must return the next linked operation', 1, Length(LRows));
  RequireEquals(161, LRows[0].Block);
  RequireLoaded([162, 161]);
end;

procedure TStoredHistoryFixture.ZeroTakeAvoidsStoredReads;
var
  LRows: TArray<TOperationResume>;
begin
  BuildLinkedHistory;
  LRows := History(2, 0, 0);
  RequireEquals('A zero-row query has no stored rows', 0, Length(LRows));
  RequireLoaded([]);
  LRows := FNode.GetStoredOperationsAffectingAccounts([], 2, 0, MaxInt);
  RequireEquals('An empty selection has no stored rows', 0, Length(LRows));
  RequireLoaded([]);
end;

procedure TStoredHistoryFixture.FiniteHistoryRetainsPendingOperations;
var
  LOperation: TOpTransaction;
  LSource: TStoredHistoryPageSource;
  LRows: TList<TOperationResume>;
  LError: String;
begin
  BuildLinkedHistory;
  LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_6, 100,
    FNode.Bank.SafeBox.Account(100).n_operation + 1, 101, FKey,
    7, 1, CT_TOperationPayload_NUL);
  try
    LError := '';
    RequireTrue('The real pending operation must be admitted: ' + LError,
      FNode.AddOperation(nil, LOperation, LError));
  finally
    LOperation.Free;
  end;
  TStoredHistoryStorage(FNode.Bank.Storage).LoadedBlocks.Clear;
  LSource := TStoredHistoryPageSource.Create(nil);
  LRows := TList<TOperationResume>.Create;
  try
    LSource.Accounts := TArray<Cardinal>.Create(100);
    LSource.BlockDepth := 0;
    LSource.FetchAll(LRows);
    RequireEquals('Finite stored history must retain the live pending row', 2,
      LRows.Count);
    RequireEquals(163, LRows[0].Block);
    RequireEquals(162, LRows[1].Block);
    RequireLoaded([162]);
  finally
    LRows.Free;
    LSource.Free;
  end;
end;

procedure TStoredHistoryFixture.StoredOperationHashesUseActualBlockHeight;
var
  LRows: TArray<TOperationResume>;
  LStored: TPCOperationsComp;
  LExpectedHash: TRawBytes;
  I, J: Integer;
begin
  BuildLinkedHistory;
  LRows := History(High(Cardinal));
  RequireEquals('All four linked operations must have their stored hashes',
    4, Length(LRows));
  for I := 0 to High(LRows) do
  begin
    LStored := nil;
    for J := 0 to FLinkedBlocks.Count - 1 do
      if FLinkedBlocks[J].OperationBlock.block = LRows[I].Block then
      begin
        LStored := FLinkedBlocks[J];
        Break;
      end;
    RequireTrue('The returned operation must identify a generated stored block',
      Assigned(LStored));
    RequireTrue('The returned operation index must address that stored block',
      (LRows[I].NOpInsideBlock >= 0) and (LRows[I].NOpInsideBlock < LStored.Count));
    // The block height is encoded into each hash. Checking the displayed Block
    // alone cannot detect conversion with a local loop index instead of height.
    LExpectedHash := TPCOperation.OperationHashValid(
      LStored.Operation[LRows[I].NOpInsideBlock], LStored.OperationBlock.block);
    RequireTrue('Stored operation hash must encode its actual block height',
      TBaseType.Equals(LExpectedHash, LRows[I].OperationHash));
    if (LStored.OperationBlock.block > 0) and
      (LStored.OperationBlock.block < CT_Protocol_Upgrade_v2_MinBlock) then
    begin
      LExpectedHash := TPCOperation.OperationHash_OLD(
        LStored.Operation[LRows[I].NOpInsideBlock], LStored.OperationBlock.block);
      RequireTrue('Legacy stored operation hash must use the same actual height',
        TBaseType.Equals(LExpectedHash, LRows[I].OperationHash_OLD));
    end
    else
      RequireEquals('Stored blocks outside the legacy range have no legacy hash',
        0, Length(LRows[I].OperationHash_OLD));
  end;
end;

procedure TStoredHistoryFixture.StoredBalanceAboveCardinalRetainsExactValue;
var LRows: TArray<TOperationResume>; LInitial: Int64;
begin
  LInitial := Int64(High(Cardinal)) + 1000;
  BuildStoredHistory(LInitial, 50, 200);
  LRows := History(0);
  RequireEquals(2, Length(LRows));
  RequireEquals('Latest committed balance must not narrow to 32 bits',
    LInitial - 270, LRows[0].Balance);
  RequireEquals('Previous committed balance remains exact',
    LInitial - 60, LRows[1].Balance);
  RequireEquals(1, LRows[0].NOpInsideBlock);
  RequireEquals(0, LRows[1].NOpInsideBlock);
end;

procedure TStoredHistoryFixture.ReverseSpendCrossesCardinalBoundary;
var LRows: TArray<TOperationResume>; LInitial: Int64;
begin
  LInitial := Int64(High(Cardinal)) + 150;
  BuildStoredHistory(LInitial, 50, 200);
  LRows := History(0);
  RequireEquals(2, Length(LRows));
  RequireEquals('Current balance fits 32 bits before the reverse traversal',
    Int64(High(Cardinal)) - 120, LRows[0].Balance);
  RequireEquals('Reversing the latest spend crosses the old map limit',
    Int64(High(Cardinal)) + 90, LRows[1].Balance);
  RequireEquals(Int64(-200), LRows[0].Amount);
  RequireEquals(Int64(-10), LRows[0].Fee);
end;

procedure TStoredHistoryFixture.AllHistoryDepthIncludesTheStoredBlock;
var LRows: TArray<TOperationResume>;
begin
  BuildStoredHistory(1000, 50, 200);
  LRows := History(FNode.Bank.BlocksCount);
  RequireEquals('All-history depth includes the generated stored operations',
    2, Length(LRows));
  RequireEquals(Int64(730), LRows[0].Balance);
  RequireEquals(Int64(940), LRows[1].Balance);
end;

procedure TStoredHistoryFixture.ActualHistoryPagePublishesSigned64BitBalances;
var LSource: TStoredHistoryPageSource; LTable: TDataTable;
  LResult: TPageFetchResult; LInitial: Int64;
begin
  LInitial := Int64(High(Cardinal)) + 1000;
  BuildStoredHistory(LInitial, 50, 200);
  LSource := TStoredHistoryPageSource.Create(nil);
  try
    LSource.Accounts := TArray<Cardinal>.Create(100);
    LSource.BlockDepth := FNode.Bank.BlocksCount;
    LResult := LSource.ReadPage(LTable);
    RequireEquals('The inherited provider materializes both stored operations',
      2, LResult.TotalDataCount);
    RequireEquals(2, Length(LTable.Rows));
    RequireEquals('The actual table retains the exact signed balance',
      LInitial - 270, Int64(LTable.Rows[0].Balance));
    RequireEquals(LInitial - 60, Int64(LTable.Rows[1].Balance));
  finally LSource.Free; end;
end;

procedure TStoredHistoryFixture.MaximumReadsLegacyHistoryBeforeThePrimaryRange;
var LRows: TArray<TOperationResume>; LSource: TStoredHistoryPageSource; LTable: TDataTable; LResult: TPageFetchResult;
begin
  PrepareLayeredHistory(False, True);
  LRows := FNode.GetStoredOperationsAffectingAccounts([102], High(Cardinal), 0, MaxInt);
  RequireEquals('Maximum finds an account last updated before the primary index begins', 1, Length(LRows));
  RequireEquals('The returned legacy operation retains its actual height', 160, LRows[0].Block);
  RequireEquals('The returned legacy operation belongs to the selected account', 102, LRows[0].AffectedAccount);
  LSource := TStoredHistoryPageSource.Create(nil);
  try
    LSource.Accounts := TArray<Cardinal>.Create(100);
    LSource.BlockDepth := High(Cardinal);
    LResult := LSource.ReadPage(LTable);
    RequireEquals('The actual grid source counts all primary and legacy history', 4, LResult.TotalDataCount);
    RequireEquals('The actual grid source publishes all available rows', 4, Length(LTable.Rows));
    LSource.BlockDepth := 2016;
    LResult := LSource.ReadPage(LTable);
    RequireEquals('Seven-day depth retains all rows in this short local chain', 4, LResult.TotalDataCount);
    LSource.BlockDepth := 8640;
    LResult := LSource.ReadPage(LTable);
    RequireEquals('Thirty-day depth retains all rows in this short local chain', 4, LResult.TotalDataCount);
  finally
    LSource.Free;
  end;
end;

procedure TStoredHistoryFixture.MaximumReadsLegacyHistoryInsideAPrimaryGap;
var LRows: TArray<TOperationResume>;
begin
  PrepareLayeredHistory(True, True);
  LRows := History(High(Cardinal));
  RequireEquals('Maximum includes the block held only by legacy storage inside the primary range', 4, Length(LRows));
  RequireEquals('The newest history row is the primary tip', 162, LRows[0].Block);
  RequireEquals('The missing primary block is loaded before its operations are scanned', 161, LRows[1].Block);
  RequireEquals('Both operations from the oldest linked block remain available', 160, LRows[2].Block);
  RequireEquals(160, LRows[3].Block);
  LRows := History(1);
  RequireEquals('Layered storage still honors the finite inclusive cutoff', 2, Length(LRows));
  RequireEquals(162, LRows[0].Block);
  RequireEquals(161, LRows[1].Block);
  LRows := History(0);
  RequireEquals('Layered storage still honors tip-only history', 1, Length(LRows));
  RequireEquals(162, LRows[0].Block);
end;

procedure TStoredHistoryFixture.MissingLegacyBlockDoesNotPublishAnotherBlocksOperations;
var LRows: TArray<TOperationResume>;
begin
  PrepareLayeredHistory(True, False);
  // A genuine archive gap must stop that account's linked traversal, rather
  // than reusing the previous block component and publishing duplicate rows.
  LRows := History(High(Cardinal));
  RequireEquals('A block absent from both stores does not invent history', 1, Length(LRows));
  RequireEquals('Only the actual available tip operation is returned', 162, LRows[0].Block);
end;

procedure CheckStoredHistory(ACheck: TStoredHistoryCheck);
var LFixture: TStoredHistoryFixture;
begin
  if ACheck = shcLivePendingFastPath then begin
    CheckLivePendingHistory;
    Exit;
  end;
  LFixture := TStoredHistoryFixture.Create;
  try
    case ACheck of
      shcWideBalance: LFixture.StoredBalanceAboveCardinalRetainsExactValue;
      shcReverseBoundary: LFixture.ReverseSpendCrossesCardinalBoundary;
      shcAllHistory: LFixture.AllHistoryDepthIncludesTheStoredBlock;
      shcActualPage: LFixture.ActualHistoryPagePublishesSigned64BitBalances;
      shcFiniteDepth: LFixture.FiniteDepthStopsAtTheInclusiveBoundary;
      shcTipOnly: LFixture.ZeroDepthReadsTheTipOnly;
      shcOldAccount: LFixture.OldAccountAvoidsStoredReads;
      shcFinitePage: LFixture.FinitePagesStopOnceTheRequestedRowsAreFound;
      shcZeroTake: LFixture.ZeroTakeAvoidsStoredReads;
      shcFiniteWithPending: LFixture.FiniteHistoryRetainsPendingOperations;
      shcActualBlockHashes: LFixture.StoredOperationHashesUseActualBlockHeight;
      shcLayeredBeforePrimary: LFixture.MaximumReadsLegacyHistoryBeforeThePrimaryRange;
      shcLayeredPrimaryGap: LFixture.MaximumReadsLegacyHistoryInsideAPrimaryGap;
      shcLayeredMissingBlock: LFixture.MissingLegacyBlockDoesNotPublishAnotherBlocksOperations;
    end;
  finally LFixture.Free; end;
end;

function RunStoredHistoryChecks(const ALogFile: String): Integer;
var LLog: TStringList; LCheck: TStoredHistoryCheck;
const CNames: array[TStoredHistoryCheck] of String =
  ('stored balance above Cardinal', 'reverse spend across Cardinal boundary',
   'all-history depth', 'actual inherited history page signed64 balances',
   'finite inclusive history depth and actual provider', 'depth zero reads tip only',
   'old selected account avoids stored reads', 'finite pages stop linked traversal',
   'zero take and empty selection avoid stored reads',
   'finite history retains live pending operations', 'live pending index fast path',
   'stored operation hashes encode actual block heights',
   'Maximum reads legacy history before the primary range',
   'Maximum reads legacy history inside a primary gap',
   'missing legacy block does not publish another blocks operations');
begin
  Result := 0;
  LLog := TStringList.Create;
  try
    for LCheck := Low(TStoredHistoryCheck) to High(TStoredHistoryCheck) do
      try
        CheckStoredHistory(LCheck);
        LLog.Add('PASS ' + CNames[LCheck]);
      except on E: Exception do begin
        Result := 1;
        LLog.Add('FAIL ' + CNames[LCheck] + ': ' + E.ClassName + ': ' + E.Message +
          ' address=' + IntToHex(NativeUInt(ExceptAddr), SizeOf(Pointer) * 2));
      end; end;
    LLog.Add('RESULT ' + IntToStr(Result));
    LLog.SaveToFile(ALogFile);
  finally LLog.Free; end;
end;

end.
