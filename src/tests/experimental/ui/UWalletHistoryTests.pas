unit UWalletHistoryTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Generated operations exercise pending history without a node, and stored
  history through a fresh owned node/storage fixture. No peers are contacted. }

{$mode delphi}

interface

uses
  Classes, SysUtils, SyncObjs, fpcunit, testregistry, Generics.Collections, UCrypto,
  UAccounts, UBlockChain, UOpTransaction, UPCDataTypes, UCoreUtils,
  UOperationPreparationTests, UOrderedList, UNode;

type
  TCountingHistoryTransaction = class(TOpTransaction)
  public
    class var AffectedCalls: Integer;
    class var DelayAffectedMS: Integer;
    class var AffectedEntered: TEvent;
    procedure AffectedAccounts(AList: TOrderedList<Cardinal>); override;
  end;

  TWalletHistoryTests = class(TTestCase)
  private
    FKey: TECPrivateKey;
    FSafeBox: TOperationSafeBox;
    FSnapshot: TPCSafeBoxTransaction;
    FPrevious: TAccountPreviousBlockInfo;
    FTree: TOperationsHashTree;
    procedure AddTransaction(ATree: TOperationsHashTree; ASender, ANOperation,
      AReceiver: Cardinal; AAmount: UInt64);
    function History(const AAccounts: array of Cardinal; ASkip: Integer = 0;
      ATake: Integer = MaxInt): TArray<TOperationResume>;
    procedure AssertRow(const ARow: TOperationResume; AAccount: Cardinal; AIndex: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PreservesAccountMajorOrderAndActualOperationIndexes;
    procedure UsesPendingBalanceForEachAffectedAccount;
    procedure DuplicateRequestedAccountsRetainTheirRows;
    procedure PagesAcrossAccountBoundariesWithoutOverflow;
    procedure EmptyAndZeroTakeAvoidOperationTraversal;
    procedure ManyAccountsWalkThePendingTreeOnce;
    procedure TerminatedHistoryFetchJoinsAndDoesNotCachePartialRows;
  end;

  TWalletHistorySourceTests = class(TTestCase)
  published
    procedure SettingsRemainResponsiveDuringBlockedFetch;
  end;

  TWalletStoredHistoryTests = class(TTestCase)
  published
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
    procedure LivePendingIndexAvoidsUnrelatedTraversal;
    procedure StoredOperationHashesUseActualBlockHeight;
    procedure MaximumReadsLegacyHistoryBeforeThePrimaryRange;
    procedure MaximumReadsLegacyHistoryInsideAPrimaryGap;
    procedure MissingLegacyBlockDoesNotPublishAnotherBlocksOperations;
  end;

implementation

uses UConst, UBaseTypes, UMemory, UThread, UDataSources, UCommon.Data,
  UFileStorage, UStoredHistoryChecks;

type
  TCancellableHistorySource = class(TOperationsDataSourceBase)
  private
    FTree: TOperationsHashTree;
    FSnapshot: TPCSafeBoxTransaction;
  public
    Fetches: Integer;
    constructor Create(ATree: TOperationsHashTree; ASnapshot: TPCSafeBoxTransaction);
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
    function Page: Integer;
  end;

procedure TWalletStoredHistoryTests.StoredBalanceAboveCardinalRetainsExactValue;
begin
  CheckStoredHistory(shcWideBalance);
end;

procedure TWalletStoredHistoryTests.ReverseSpendCrossesCardinalBoundary;
begin
  CheckStoredHistory(shcReverseBoundary);
end;

procedure TWalletStoredHistoryTests.AllHistoryDepthIncludesTheStoredBlock;
begin
  CheckStoredHistory(shcAllHistory);
end;

procedure TWalletStoredHistoryTests.ActualHistoryPagePublishesSigned64BitBalances;
begin
  CheckStoredHistory(shcActualPage);
end;

procedure TWalletStoredHistoryTests.FiniteDepthStopsAtTheInclusiveBoundary;
begin
  CheckStoredHistory(shcFiniteDepth);
end;

procedure TWalletStoredHistoryTests.ZeroDepthReadsTheTipOnly;
begin
  CheckStoredHistory(shcTipOnly);
end;

procedure TWalletStoredHistoryTests.OldAccountAvoidsStoredReads;
begin
  CheckStoredHistory(shcOldAccount);
end;

procedure TWalletStoredHistoryTests.FinitePagesStopOnceTheRequestedRowsAreFound;
begin
  CheckStoredHistory(shcFinitePage);
end;

procedure TWalletStoredHistoryTests.ZeroTakeAvoidsStoredReads;
begin
  CheckStoredHistory(shcZeroTake);
end;

procedure TWalletStoredHistoryTests.FiniteHistoryRetainsPendingOperations;
begin
  CheckStoredHistory(shcFiniteWithPending);
end;

procedure TWalletStoredHistoryTests.LivePendingIndexAvoidsUnrelatedTraversal;
begin
  CheckStoredHistory(shcLivePendingFastPath);
end;

procedure TWalletStoredHistoryTests.StoredOperationHashesUseActualBlockHeight;
begin
  CheckStoredHistory(shcActualBlockHashes);
end;

procedure TWalletStoredHistoryTests.MaximumReadsLegacyHistoryBeforeThePrimaryRange;
begin
  CheckStoredHistory(shcLayeredBeforePrimary);
end;

procedure TWalletStoredHistoryTests.MaximumReadsLegacyHistoryInsideAPrimaryGap;
begin
  CheckStoredHistory(shcLayeredPrimaryGap);
end;

procedure TWalletStoredHistoryTests.MissingLegacyBlockDoesNotPublishAnotherBlocksOperations;
begin
  CheckStoredHistory(shcLayeredMissingBlock);
end;

type
  TCancellableHistoryThread = class(TThread)
  private
    FSource: TCancellableHistorySource;
  protected
    procedure Execute; override;
  public
    Completed: TEvent;
    Cancelled: Boolean;
    Rows: Integer;
    ErrorMessage: String;
    constructor Create(ASource: TCancellableHistorySource);
    destructor Destroy; override;
  end;

  TBlockedHistorySource = class(TAccountsOperationsDataSource)
  public
    Entered, ReleaseFetch: TEvent;
    constructor Create;
    destructor Destroy; override;
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
    procedure FetchTestPage;
  end;

  THistoryFetchThread = class(TPCThread)
  private
    FSource: TBlockedHistorySource;
  protected
    procedure BCExecute; override;
  public
    ErrorMessage: String;
    constructor Create(ASource: TBlockedHistorySource);
  end;

  THistorySettingsThread = class(TPCThread)
  private
    FSource: TBlockedHistorySource;
  protected
    procedure BCExecute; override;
  public
    Completed: TEvent;
    ErrorMessage: String;
    constructor Create(ASource: TBlockedHistorySource);
    destructor Destroy; override;
  end;

constructor TCancellableHistorySource.Create(ATree: TOperationsHashTree;
  ASnapshot: TPCSafeBoxTransaction);
begin
  inherited Create(nil);
  FTree := ATree;
  FSnapshot := ASnapshot;
  SnapshotCacheEnabled := True;
end;

procedure TCancellableHistorySource.FetchAll(const AContainer: TList<TOperationResume>);
begin
  Inc(Fetches);
  AContainer.AddRange(TCoreTool.GetPendingOperationsAffectingAccounts(
    FTree, FSnapshot, 160, [100,101], 0, MaxInt));
end;

function TCancellableHistorySource.Page: Integer;
var LTable: TDataTable; LResult: TPageFetchResult;
begin
  LResult := FetchPage(TPageFetchParams.Create(0,25,nil,foAnd),LTable);
  Result := LResult.TotalDataCount;
end;

constructor TCancellableHistoryThread.Create(ASource: TCancellableHistorySource);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSource := ASource;
  Completed := TEvent.Create(nil,True,False,'');
  Start;
end;

destructor TCancellableHistoryThread.Destroy;
begin
  Terminate;
  WaitFor;
  Completed.Free;
  inherited Destroy;
end;

procedure TCancellableHistoryThread.Execute;
begin
  try
    try
      Rows := FSource.Page;
    except
      on E: EAbort do Cancelled := True;
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

constructor TBlockedHistorySource.Create;
begin
  inherited Create(nil);
  Entered := TEvent.Create(nil,True,False,'');
  ReleaseFetch := TEvent.Create(nil,True,False,'');
end;

destructor TBlockedHistorySource.Destroy;
begin
  Entered.Free;
  ReleaseFetch.Free;
  inherited Destroy;
end;

procedure TBlockedHistorySource.FetchAll(const AContainer: TList<TOperationResume>);
begin
  // This runs under the real base FetchPage lock, without opening a node.
  Entered.SetEvent;
  if ReleaseFetch.WaitFor(10000) <> wrSignaled then
    raise Exception.Create('History fetch gate timed out');
end;

procedure TBlockedHistorySource.FetchTestPage;
var LTable: TDataTable;
begin
  FetchPage(TPageFetchParams.Create(0,25,nil,foAnd),LTable);
end;

constructor THistoryFetchThread.Create(ASource: TBlockedHistorySource);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSource := ASource;
  Start;
end;

procedure THistoryFetchThread.BCExecute;
begin
  try
    FSource.FetchTestPage;
  except
    on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
  end;
end;

constructor THistorySettingsThread.Create(ASource: TBlockedHistorySource);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSource := ASource;
  Completed := TEvent.Create(nil,True,False,'');
  Start;
end;

destructor THistorySettingsThread.Destroy;
begin
  inherited Destroy;
  Completed.Free;
end;

procedure THistorySettingsThread.BCExecute;
var LAccounts: TArray<Cardinal>;
begin
  try
    try
      FSource.Accounts := TArray<Cardinal>.Create(100,101);
      LAccounts := FSource.Accounts;
      if (Length(LAccounts) <> 2) or (LAccounts[0] <> 100) or (LAccounts[1] <> 101) then
        raise Exception.Create('History account selection was not updated');
      LAccounts[0] := 999;
      LAccounts := FSource.Accounts;
      if LAccounts[0] <> 100 then
        raise Exception.Create('History getter exposed the stored account array');
      FSource.BlockDepth := 400;
      if FSource.BlockDepth <> 400 then
        raise Exception.Create('History block depth was not updated');
    except
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

procedure TCountingHistoryTransaction.AffectedAccounts(AList: TOrderedList<Cardinal>);
begin
  Inc(AffectedCalls);
  if Assigned(AffectedEntered) then AffectedEntered.SetEvent;
  if DelayAffectedMS > 0 then Sleep(DelayAffectedMS);
  inherited AffectedAccounts(AList);
end;

procedure TWalletHistoryTests.TerminatedHistoryFetchJoinsAndDoesNotCachePartialRows;
var
  LSource: TCancellableHistorySource;
  LWorker: TCancellableHistoryThread;
  LEntered: TEvent;
  LStarted: QWord;
begin
  LEntered := TEvent.Create(nil,True,False,'');
  LSource := TCancellableHistorySource.Create(FTree,FSnapshot);
  LWorker := nil;
  try
    TCountingHistoryTransaction.DelayAffectedMS := 250;
    TCountingHistoryTransaction.AffectedEntered := LEntered;
    LWorker := TCancellableHistoryThread.Create(LSource);
    AssertEquals('The actual pending-history query must start',Ord(wrSignaled),Ord(LEntered.WaitFor(3000)));
    LStarted := GetTickCount64;
    LWorker.Terminate;
    AssertEquals('Termination must join without releasing any test gate',Ord(wrSignaled),Ord(LWorker.Completed.WaitFor(1500)));
    LWorker.WaitFor;
    AssertTrue('History traversal must observe the owning worker termination',LWorker.Cancelled);
    AssertEquals('',LWorker.ErrorMessage);
    AssertEquals(0,LWorker.Rows);
    AssertTrue('History cancellation must stop before traversing the remaining operations',GetTickCount64-LStarted<1000);
    TCountingHistoryTransaction.AffectedEntered := nil;
    TCountingHistoryTransaction.DelayAffectedMS := 0;
    AssertEquals('A later normal query must rebuild the complete history',4,LSource.Page);
    AssertEquals('Cancelled snapshots must not enter the cache',2,LSource.Fetches);
    AssertEquals(4,LSource.Page);
    AssertEquals('The complete replacement snapshot remains reusable',2,LSource.Fetches);
  finally
    if Assigned(LWorker) then begin
      LWorker.Terminate;
      LWorker.WaitFor;
      LWorker.Free;
    end;
    TCountingHistoryTransaction.AffectedEntered := nil;
    TCountingHistoryTransaction.DelayAffectedMS := 0;
    LSource.Free;
    LEntered.Free;
  end;
end;

procedure TWalletHistoryTests.SetUp;
var
  LAccount: TAccount;
  LError: String;
  i: Integer;
begin
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FSafeBox := TOperationSafeBox.Create;
  FSafeBox.Build(FKey.PublicKey);
  for i := 100 to 115 do begin
    LAccount := FSafeBox.Account(i);
    LAccount.balance := UInt64(i - 99) * 1000000;
    LAccount.n_operation := 0;
    FSafeBox.Seed(LAccount);
  end;
  FSnapshot := TPCSafeBoxTransaction.Create(FSafeBox);
  FPrevious := TAccountPreviousBlockInfo.Create;
  FTree := TOperationsHashTree.Create;
  AddTransaction(FTree,100,1,101,100);
  AddTransaction(FTree,102,1,103,200);
  AddTransaction(FTree,100,2,102,300);
  AddTransaction(FTree,104,1,101,400);
  for i := 0 to FTree.OperationsCount - 1 do begin
    LError := '';
    AssertTrue('Fixture operation must pass actual core validation: ' + LError,
      FTree.GetOperation(i).DoOperation(FPrevious,FSnapshot,LError));
  end;
  TCountingHistoryTransaction.AffectedCalls := 0;
end;

procedure TWalletHistoryTests.TearDown;
begin
  FTree.Free;
  FPrevious.Free;
  FSnapshot.Free;
  FSafeBox.Free;
  FKey.Free;
end;

procedure TWalletHistoryTests.AddTransaction(ATree: TOperationsHashTree;
  ASender, ANOperation, AReceiver: Cardinal; AAmount: UInt64);
var LOperation: TCountingHistoryTransaction;
begin
  LOperation := TCountingHistoryTransaction.CreateTransaction(CT_PROTOCOL_6,
    ASender,ANOperation,AReceiver,FKey,AAmount,10,CT_TOperationPayload_NUL);
  try
    AssertTrue(ATree.AddOperationToHashTree(LOperation));
  finally
    LOperation.Free;
  end;
end;

function TWalletHistoryTests.History(const AAccounts: array of Cardinal;
  ASkip, ATake: Integer): TArray<TOperationResume>;
begin
  Result := TCoreTool.GetPendingOperationsAffectingAccounts(FTree,FSnapshot,
    160,AAccounts,ASkip,ATake);
end;

procedure TWalletHistoryTests.AssertRow(const ARow: TOperationResume;
  AAccount: Cardinal; AIndex: Integer);
begin
  AssertEquals('Affected account context',Int64(AAccount),Int64(ARow.AffectedAccount));
  AssertEquals('Original tree operation index',AIndex,ARow.NOpInsideBlock);
  AssertEquals('Pending block context',160,Integer(ARow.Block));
  AssertTrue('Operation hash is available',Length(ARow.OperationHash) > 0);
end;

procedure TWalletHistoryTests.PreservesAccountMajorOrderAndActualOperationIndexes;
var LRows: TArray<TOperationResume>;
begin
  LRows := History([100,101]);
  AssertEquals(4,Length(LRows));
  AssertRow(LRows[0],100,2);
  AssertRow(LRows[1],100,0);
  AssertRow(LRows[2],101,3);
  AssertRow(LRows[3],101,0);
  AssertEquals('One affected-account evaluation per pending operation',4,
    TCountingHistoryTransaction.AffectedCalls);
end;

procedure TWalletHistoryTests.UsesPendingBalanceForEachAffectedAccount;
var LRows: TArray<TOperationResume>;
begin
  LRows := History([100,101]);
  AssertEquals(Int64(999580),LRows[0].Balance);
  AssertEquals(Int64(999580),LRows[1].Balance);
  AssertEquals(Int64(2000500),LRows[2].Balance);
  AssertEquals(Int64(2000500),LRows[3].Balance);
  AssertEquals(Int64(-300),LRows[0].Amount);
  AssertEquals(Int64(-10),LRows[0].Fee);
  AssertEquals(Int64(400),LRows[2].Amount);
  AssertEquals(Int64(0),LRows[2].Fee);
end;

procedure TWalletHistoryTests.DuplicateRequestedAccountsRetainTheirRows;
var LRows: TArray<TOperationResume>;
begin
  LRows := History([101,100,101]);
  AssertEquals(6,Length(LRows));
  AssertRow(LRows[0],101,3);
  AssertRow(LRows[1],101,0);
  AssertRow(LRows[2],100,2);
  AssertRow(LRows[3],100,0);
  AssertRow(LRows[4],101,3);
  AssertRow(LRows[5],101,0);
  AssertEquals('Duplicate query accounts do not repeat tree scanning',4,
    TCountingHistoryTransaction.AffectedCalls);
end;

procedure TWalletHistoryTests.PagesAcrossAccountBoundariesWithoutOverflow;
var LRows: TArray<TOperationResume>;
begin
  LRows := History([100,101],1,2);
  AssertEquals(2,Length(LRows));
  AssertRow(LRows[0],100,0);
  AssertRow(LRows[1],101,3);
  LRows := History([100,101],3,MaxInt);
  AssertEquals(1,Length(LRows));
  AssertRow(LRows[0],101,0);
  LRows := History([100,101],MaxInt,MaxInt);
  AssertEquals('Paging arithmetic must not overflow',0,Length(LRows));
end;

procedure TWalletHistoryTests.EmptyAndZeroTakeAvoidOperationTraversal;
var
  LRows: TArray<TOperationResume>;
  LScope: TDisposables;
  LEmpty: TOperationsHashTree;
begin
  LRows := History([]);
  AssertEquals(0,Length(LRows));
  LRows := History([100],0,0);
  AssertEquals(0,Length(LRows));
  LEmpty := LScope.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  LRows := TCoreTool.GetPendingOperationsAffectingAccounts(LEmpty,nil,160,[100],0,MaxInt);
  AssertEquals(0,Length(LRows));
  AssertEquals(0,TCountingHistoryTransaction.AffectedCalls);
end;

procedure TWalletHistoryTests.ManyAccountsWalkThePendingTreeOnce;
var
  LScope: TDisposables;
  LTree: TOperationsHashTree;
  LAccounts: TArray<Cardinal>;
  LRows: TArray<TOperationResume>;
  i: Integer;
begin
  LTree := LScope.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  for i := 0 to 511 do
    AddTransaction(LTree,100 + Cardinal(i mod 16),1 + Cardinal(i div 16),
      500 + Cardinal(i mod 16),1 + UInt64(i));
  SetLength(LAccounts,32);
  for i := 0 to 15 do begin
    LAccounts[i] := 100 + Cardinal(i);
    LAccounts[i + 16] := 500 + Cardinal(i);
  end;
  TCountingHistoryTransaction.AffectedCalls := 0;
  LRows := TCoreTool.GetPendingOperationsAffectingAccounts(LTree,FSnapshot,
    160,LAccounts,0,MaxInt);
  AssertEquals('Both owned sender/receiver contexts are retained',1024,Length(LRows));
  AssertEquals('Query size must not multiply pending traversal',512,
    TCountingHistoryTransaction.AffectedCalls);
  AssertRow(LRows[0],100,496);
  AssertRow(LRows[512],500,496);
end;

procedure TWalletHistorySourceTests.SettingsRemainResponsiveDuringBlockedFetch;
var
  LSource: TBlockedHistorySource;
  LFetch: THistoryFetchThread;
  LSettings: THistorySettingsThread;
begin
  LSource := TBlockedHistorySource.Create;
  LFetch := nil;
  LSettings := nil;
  try
    LFetch := THistoryFetchThread.Create(LSource);
    AssertEquals('Fetch must hold the base data-source lock',Ord(wrSignaled),
      Ord(LSource.Entered.WaitFor(5000)));
    LSettings := THistorySettingsThread.Create(LSource);
    AssertEquals('All history settings accessors must complete while fetch is blocked',
      Ord(wrSignaled),Ord(LSettings.Completed.WaitFor(3000)));
    AssertEquals('The fetch gate must remain closed until settings have completed',
      Ord(wrTimeout),Ord(LSource.ReleaseFetch.WaitFor(0)));
    AssertEquals('',LSettings.ErrorMessage);
    LSource.ReleaseFetch.SetEvent;
    LFetch.WaitFor;
    AssertEquals('',LFetch.ErrorMessage);
  finally
    // Open the gate even after an assertion fails, then join before freeing
    // either the data source or its worker-owned events.
    LSource.ReleaseFetch.SetEvent;
    if Assigned(LSettings) then begin
      LSettings.WaitFor;
      LSettings.Free;
    end;
    if Assigned(LFetch) then begin
      LFetch.WaitFor;
      LFetch.Free;
    end;
    LSource.Free;
  end;
end;

initialization
  RegisterTest(TWalletHistoryTests);
  RegisterTest(TWalletHistorySourceTests);
  RegisterTest(TWalletStoredHistoryTests);
end.
