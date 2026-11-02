unit UWalletPagingTests;

{ Copyright © 2026 Herman Schoenfeld
  Distributed under the MIT software license, see the accompanying LICENSE.
  Synthetic key indexes count account reads without constructing a node. }

{$mode delphi}

interface

uses Classes, SysUtils, SyncObjs, fpcunit, testregistry, UCrypto, UAccounts,
  UPCDataTypes, UDataSources, UCommon.Data, UCoreObjects, UWalletStartupTask;

type
  TWalletPagingTests = class(TTestCase)
  private
    FCompletionCount: Integer;
    FCompletionThread: TThreadID;
    FCompletedSummary: TBalanceSummary;
    FCompletedSummaryReady: Boolean;
    procedure OnBalanceCompleted(Sender: TObject);
    procedure WaitForBalanceCompletion;
  published
    procedure InterfacePagingReadsOnlyTheRequestedRecords;
    procedure SmallKeyDoesNotEnumerateOrReadTheLargeKey;
    procedure SmallKeyCustomSortReadsOnlyItsOwnRecords;
    procedure PendingOwnershipChangesCorrectMembershipAndCount;
    procedure ConsecutivePagesGrowIndexPrefixesGeometrically;
    procedure FullQueryKeepsItsCapturedPendingTip;
    procedure GlobalTotalsAreIndependentOfTheSelectedKey;
    procedure ChangedCoreStateDiscardsAggregateWithoutRetryChurn;
    procedure DestroyCancelsAndJoinsTheAggregateWorker;
    procedure AggregateCompletionPublishesWithoutPolling;
    procedure AggregateCompletionCanRequestTheLatestGeneration;
    procedure DestroyRemovesAggregateCompletionCallbacks;
    procedure TerminatedCustomQueryJoinsAndDoesNotCachePartialRows;
    procedure TerminatedSortJoinsAndDoesNotCachePartialQuery;
  end;

implementation

uses UMemory, UCommon, UConst, Variants, Generics.Collections, Math;

procedure TWalletPagingTests.OnBalanceCompleted(Sender: TObject);
begin
  Inc(FCompletionCount);
  FCompletionThread := TThread.CurrentThread.ThreadID;
  FCompletedSummaryReady := TMyAccountsDataSource(Sender).TryGetBalanceSummary(FCompletedSummary);
end;

procedure TWalletPagingTests.WaitForBalanceCompletion;
var LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 5000;
  repeat
    // Deliver queued completion events; do not poll/start the aggregate here.
    CheckSynchronize(0);
    if FCompletedSummaryReady then Exit;
    if GetTickCount64 >= LDeadline then Fail('The aggregate completion event did not publish');
    Sleep(1);
  until False;
end;

type
  TCountingPagedAccounts = class(TMyAccountsDataSource)
  private
    function MakeAccount(ANumber: Cardinal): TAccount;
  protected
    procedure BeginReadSnapshot(AIncludePending: Boolean; out AContext: TAccountReadContext); override;
    function BeginBalanceSnapshot(ATask: TWalletStartupTask; AGeneration: Int64;
      AIncludePending: Boolean; out AContext: TAccountReadContext): Boolean; override;
    procedure EndReadSnapshot(const AContext: TAccountReadContext); override;
    function WalletKeys(const AContext: TAccountReadContext): TArray<TAccountKey>; override;
    function KeyAccountCount(const AKey: TAccountKey; const AContext: TAccountReadContext): Integer; override;
    function ReadKeyPrefix(const AKey: TAccountKey; ACount: Integer;
      const AContext: TAccountReadContext): TArray<Cardinal>; override;
    function OwnershipChanges(const AContext: TAccountReadContext): TArray<TAccountOwnershipChange>; override;
    function ReadPageAccount(AAccountNumber: Cardinal; const AContext: TAccountReadContext): TAccount; override;
    function ReadBalanceAccount(AAccountNumber: Cardinal; const AContext: TAccountReadContext): TAccount; override;
    function GetItemField(constref AItem: TAccount; const ABindingName: String): Variant; override;
  public
    Keys: TArray<TAccountKey>;
    Changes: TArray<TAccountOwnershipChange>;
    LargeCount, SmallCount: Integer;
    PageReads, BalanceReads, LargePrefixReads, SmallPrefixReads: Integer;
    LargestPrefixRequested, PrefixNumbersRead, FullFetches: Integer;
    DelayBalance, ChangeStateAfter: Integer;
    DelayPage, DelayField, FieldReads: Integer;
    Tip, UpdatedAt: Cardinal;
    StateID: String;
    BalanceEntered: TEvent;
    PageEntered: TEvent;
    FieldEntered: TEvent;
    constructor Create;
    destructor Destroy; override;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
    function Page(AIndex, ASize: Integer; const AFilters: TFilterCriteria; var ATable: TDataTable): TPageFetchResult;
  end;

  TCancelledPageThread = class(TThread)
  private
    FSource: TCountingPagedAccounts;
  protected
    procedure Execute; override;
  public
    Completed: TEvent;
    Cancelled: Boolean;
    ErrorMessage: String;
    constructor Create(ASource: TCountingPagedAccounts);
    destructor Destroy; override;
  end;

constructor TCountingPagedAccounts.Create;
var LKey: TECPrivateKey; i: Integer; LScope: TDisposables;
begin
  inherited Create(nil);
  TCrypto.InitCrypto;
  SetLength(Keys, 3);
  LKey := LScope.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  for i := 0 to High(Keys) do begin
    LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    Keys[i] := LKey.PublicKey;
  end;
  LargeCount := 1000;
  SmallCount := 3;
  Tip := 10;
  UpdatedAt := 11;
  StateID := 'stable';
  SnapshotCacheEnabled := True;
  BalanceEntered := TEvent.Create(nil, True, False, '');
  PageEntered := TEvent.Create(nil, True, False, '');
  FieldEntered := TEvent.Create(nil, True, False, '');
end;

destructor TCountingPagedAccounts.Destroy;
begin
  // The base owns and joins its task before fixture events are released.
  inherited Destroy;
  BalanceEntered.Free;
  PageEntered.Free;
  FieldEntered.Free;
end;

procedure TCountingPagedAccounts.BeginReadSnapshot(AIncludePending: Boolean;
  out AContext: TAccountReadContext);
begin
  AContext := Default(TAccountReadContext);
  AContext.BlockTip := Tip;
  AContext.StateID := StateID;
end;

function TCountingPagedAccounts.BeginBalanceSnapshot(ATask: TWalletStartupTask;
  AGeneration: Int64; AIncludePending: Boolean; out AContext: TAccountReadContext): Boolean;
begin
  Result := not ATask.Cancelled;
  if Result then BeginReadSnapshot(AIncludePending, AContext);
end;

procedure TCountingPagedAccounts.EndReadSnapshot(const AContext: TAccountReadContext);
begin
end;

function TCountingPagedAccounts.WalletKeys(const AContext: TAccountReadContext): TArray<TAccountKey>;
begin
  Result := Keys;
end;

function TCountingPagedAccounts.KeyAccountCount(const AKey: TAccountKey;
  const AContext: TAccountReadContext): Integer;
begin
  if TAccountComp.EqualAccountKeys(AKey, Keys[0]) then Result := LargeCount
  else if TAccountComp.EqualAccountKeys(AKey, Keys[1]) then Result := SmallCount
  else Result := 0;
end;

function TCountingPagedAccounts.ReadKeyPrefix(const AKey: TAccountKey;
  ACount: Integer; const AContext: TAccountReadContext): TArray<Cardinal>;
var i, LCount: Integer; LSmall: Boolean;
begin
  LCount := Min(ACount, KeyAccountCount(AKey, AContext));
  LSmall := TAccountComp.EqualAccountKeys(AKey, Keys[1]);
  if LSmall then Inc(SmallPrefixReads) else Inc(LargePrefixReads);
  if LCount > LargestPrefixRequested then LargestPrefixRequested := LCount;
  Inc(PrefixNumbersRead, LCount);
  SetLength(Result, LCount);
  for i := 0 to LCount - 1 do
    if LSmall then Result[i] := i * 2 + 1 else Result[i] := i * 2;
end;

function TCountingPagedAccounts.OwnershipChanges(const AContext: TAccountReadContext): TArray<TAccountOwnershipChange>;
begin
  Result := Changes;
end;

function TCountingPagedAccounts.MakeAccount(ANumber: Cardinal): TAccount;
var i: Integer;
begin
  Result := Default(TAccount);
  Result.account := ANumber;
  Result.balance := UInt64(ANumber) + 100;
  Result.updated_on_block_active_mode := UpdatedAt;
  Result.updated_on_block_passive_mode := UpdatedAt;
  if (ANumber mod 2 = 1) and (ANumber < Cardinal(SmallCount * 2)) then
    Result.accountInfo.accountKey := Keys[1]
  else
    Result.accountInfo.accountKey := Keys[0];
  for i := 0 to High(Changes) do
    if Changes[i].Account = ANumber then Result.accountInfo.accountKey := Changes[i].NewKey;
end;

function TCountingPagedAccounts.ReadPageAccount(AAccountNumber: Cardinal;
  const AContext: TAccountReadContext): TAccount;
begin
  InterlockedIncrement(PageReads);
  PageEntered.SetEvent;
  if DelayPage > 0 then Sleep(DelayPage);
  Result := MakeAccount(AAccountNumber);
end;

function TCountingPagedAccounts.ReadBalanceAccount(AAccountNumber: Cardinal;
  const AContext: TAccountReadContext): TAccount;
begin
  InterlockedIncrement(BalanceReads);
  BalanceEntered.SetEvent;
  if DelayBalance > 0 then Sleep(DelayBalance);
  if (ChangeStateAfter > 0) and (BalanceReads = ChangeStateAfter) then
    StateID := 'changed';
  Result := MakeAccount(AAccountNumber);
end;

procedure TCountingPagedAccounts.FetchAll(const AContainer: TList<TAccount>);
begin
  Inc(FullFetches);
  inherited FetchAll(AContainer);
end;

function TCountingPagedAccounts.GetItemField(constref AItem: TAccount;
  const ABindingName: String): Variant;
begin
  if ABindingName='Balance' then begin
    Inc(FieldReads);
    FieldEntered.SetEvent;
    if DelayField>0 then Sleep(DelayField);
  end;
  Result := inherited GetItemField(AItem,ABindingName);
end;

function TCountingPagedAccounts.Page(AIndex, ASize: Integer;
  const AFilters: TFilterCriteria; var ATable: TDataTable): TPageFetchResult;
var LInterface: IDataSource;
begin
  // Exercise actual interface dispatch to the new virtual specialization.
  LInterface := Self;
  Result := LInterface.FetchPage(TPageFetchParams.Create(AIndex, ASize, AFilters, foAnd), ATable);
end;

function SortBy(const AName: String; ADirection: TSortDirection): TFilterCriteria;
begin
  SetLength(Result, 1);
  Result[0] := Default(TColumnFilter);
  Result[0].ColumnName := AName;
  Result[0].Filter := vgfSortable;
  Result[0].Sort := ADirection;
end;

constructor TCancelledPageThread.Create(ASource: TCountingPagedAccounts);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSource := ASource;
  Completed := TEvent.Create(nil,True,False,'');
  Start;
end;

destructor TCancelledPageThread.Destroy;
begin
  Terminate;
  WaitFor;
  Completed.Free;
  inherited Destroy;
end;

procedure TCancelledPageThread.Execute;
var LTable: TDataTable;
begin
  try
    try
      FSource.Page(0,25,SortBy('Balance',sdAscending),LTable);
    except
      on E: EAbort do Cancelled := True;
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

procedure TWalletPagingTests.TerminatedCustomQueryJoinsAndDoesNotCachePartialRows;
var LSource: TCountingPagedAccounts; LWorker: TCancelledPageThread;
  LTable: TDataTable; LResult: TPageFetchResult; LStarted: QWord;
begin
  LSource := TCountingPagedAccounts.Create;
  LWorker := nil;
  try
    LSource.LargeCount := 5;
    LSource.SmallCount := 3;
    LSource.DelayPage := 100;
    LWorker := TCancelledPageThread.Create(LSource);
    AssertEquals(Ord(wrSignaled),Ord(LSource.PageEntered.WaitFor(3000)));
    LStarted := GetTickCount64;
    LWorker.Terminate;
    AssertEquals('The owned custom-query worker must join without releasing a test gate',
      Ord(wrSignaled),Ord(LWorker.Completed.WaitFor(1500)));
    LWorker.WaitFor;
    AssertTrue(LWorker.Cancelled);
    AssertEquals('',LWorker.ErrorMessage);
    AssertTrue(GetTickCount64-LStarted<1000);
    AssertTrue('Cancellation must stop before every account record is read',LSource.PageReads<8);
    LSource.DelayPage := 0;
    LResult := LSource.Page(0,25,SortBy('Balance',sdAscending),LTable);
    AssertEquals(8,LResult.TotalDataCount);
    AssertEquals('Cancelled custom snapshots must be rebuilt',2,LSource.FullFetches);
    LResult := LSource.Page(0,25,SortBy('Balance',sdAscending),LTable);
    AssertEquals(8,LResult.TotalDataCount);
    AssertEquals('Completed snapshots remain reusable',2,LSource.FullFetches);
  finally
    if Assigned(LWorker) then begin
      LWorker.Terminate;
      LWorker.WaitFor;
      LWorker.Free;
    end;
    LSource.Free;
  end;
end;

procedure TWalletPagingTests.TerminatedSortJoinsAndDoesNotCachePartialQuery;
var LSource: TCountingPagedAccounts; LWorker: TCancelledPageThread;
  LTable: TDataTable; LResult: TPageFetchResult; LFields: Integer;
begin
  LSource := TCountingPagedAccounts.Create;
  LWorker := nil;
  try
    LSource.LargeCount := 5;
    LSource.SmallCount := 3;
    LSource.DelayField := 50;
    LWorker := TCancelledPageThread.Create(LSource);
    AssertEquals(Ord(wrSignaled),Ord(LSource.FieldEntered.WaitFor(3000)));
    AssertEquals('Record loading must finish before this sort-phase cancellation',8,LSource.PageReads);
    LWorker.Terminate;
    AssertEquals('Sort cancellation must join without releasing a test gate',
      Ord(wrSignaled),Ord(LWorker.Completed.WaitFor(1500)));
    LWorker.WaitFor;
    AssertTrue(LWorker.Cancelled);
    AssertEquals('',LWorker.ErrorMessage);
    LFields := LSource.FieldReads;
    LSource.DelayField := 0;
    LResult := LSource.Page(0,25,SortBy('Balance',sdAscending),LTable);
    AssertEquals(8,LResult.TotalDataCount);
    AssertTrue('The cancelled query must evaluate its sort again',LSource.FieldReads-LFields>8);
    AssertEquals('The complete raw snapshot remains valid after cancelling only its query',1,LSource.FullFetches);
    LResult := LSource.Page(0,25,SortBy('Balance',sdAscending),LTable);
    AssertEquals(8,LResult.TotalDataCount);
    AssertEquals(1,LSource.FullFetches);
  finally
    if Assigned(LWorker) then begin
      LWorker.Terminate;
      LWorker.WaitFor;
      LWorker.Free;
    end;
    LSource.Free;
  end;
end;

procedure TWalletPagingTests.InterfacePagingReadsOnlyTheRequestedRecords;
var LSource: TCountingPagedAccounts; LTable: TDataTable; LResult: TPageFetchResult; LScope: TDisposables;
begin
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.LargeCount := 326387;
  LResult := LSource.Page(0, 25, nil, LTable);
  AssertEquals(326390, LResult.TotalDataCount);
  AssertEquals(25, Length(LTable.Rows));
  AssertEquals(25, LSource.PageReads);
  AssertEquals(0, LSource.BalanceReads);
  AssertEquals(0, LSource.FullFetches);
  AssertTrue('First page must not enumerate every account ID', LSource.PrefixNumbersRead <= 67);
  LSource.Page(1, 25, SortBy('AccountNumber', sdAscending), LTable);
  AssertEquals(50, LSource.PageReads);
  AssertEquals(0, LSource.FullFetches);
end;

procedure TWalletPagingTests.SmallKeyDoesNotEnumerateOrReadTheLargeKey;
var LSource: TCountingPagedAccounts; LTable: TDataTable; LResult: TPageFetchResult; LScope: TDisposables;
begin
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.LargeCount := 457633;
  LSource.FilterKeys := TArray<TAccountKey>.Create(LSource.Keys[1]);
  LResult := LSource.Page(0, 25, nil, LTable);
  AssertEquals(3, LResult.TotalDataCount);
  AssertEquals(3, LSource.PageReads);
  AssertEquals(0, LSource.LargePrefixReads);
  AssertEquals(0, LSource.BalanceReads);
  AssertEquals(1, Integer(LTable.Rows[0].AccountNumber));
end;

procedure TWalletPagingTests.SmallKeyCustomSortReadsOnlyItsOwnRecords;
var LSource: TCountingPagedAccounts; LTable: TDataTable; LResult: TPageFetchResult; LScope: TDisposables;
begin
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.LargeCount := 457633;
  LSource.FilterKeys := TArray<TAccountKey>.Create(LSource.Keys[1]);
  LResult := LSource.Page(0, 25, SortBy('Balance', sdDescending), LTable);
  AssertEquals(3, LResult.TotalDataCount);
  AssertEquals(3, LSource.PageReads);
  AssertEquals(0, LSource.LargePrefixReads);
  AssertEquals(5, Integer(LTable.Rows[0].AccountNumber));
  LSource.Page(0, 1, SortBy('Balance', sdDescending), LTable);
  AssertEquals('Warm custom-sort page must reuse its selected snapshot', 3, LSource.PageReads);
end;

procedure TWalletPagingTests.PendingOwnershipChangesCorrectMembershipAndCount;
var LSource: TCountingPagedAccounts; LTable: TDataTable; LResult: TPageFetchResult; LScope: TDisposables;
begin
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.FilterKeys := TArray<TAccountKey>.Create(LSource.Keys[1]);
  SetLength(LSource.Changes, 2);
  LSource.Changes[0].Account := 0;
  LSource.Changes[0].NewKey := LSource.Keys[1];
  LSource.Changes[1].Account := 1;
  LSource.Changes[1].NewKey := LSource.Keys[0];
  LResult := LSource.Page(0, 25, nil, LTable);
  AssertEquals(3, LResult.TotalDataCount);
  AssertEquals(3, LSource.PageReads);
  AssertEquals(0, LSource.LargePrefixReads);
  AssertEquals(0, Integer(LTable.Rows[0].AccountNumber));
  AssertEquals(3, Integer(LTable.Rows[1].AccountNumber));
  AssertEquals(5, Integer(LTable.Rows[2].AccountNumber));
end;

procedure TWalletPagingTests.ConsecutivePagesGrowIndexPrefixesGeometrically;
var LSource: TCountingPagedAccounts; LTable: TDataTable; i: Integer; LScope: TDisposables;
begin
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.LargeCount := 10000;
  LSource.FilterKeys := TArray<TAccountKey>.Create(LSource.Keys[0]);
  for i := 0 to 15 do LSource.Page(i, 25, nil, LTable);
  AssertEquals(400, LSource.PageReads);
  AssertTrue('Index prefixes must grow geometrically, not restart for every page',
    LSource.LargePrefixReads <= 5);
  AssertTrue(LSource.PrefixNumbersRead < 1100);
end;

procedure TWalletPagingTests.FullQueryKeepsItsCapturedPendingTip;
var LSource: TCountingPagedAccounts; LTable: TDataTable; LFilters: TFilterCriteria; LScope: TDisposables;
begin
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.FilterKeys := TArray<TAccountKey>.Create(LSource.Keys[1]);
  LFilters := SortBy('Balance', sdDescending);
  LSource.Page(0, 25, LFilters, LTable);
  AssertTrue(Boolean(LTable.Rows[0].IsPending));
  LSource.Tip := 12;
  LSource.Page(0, 25, nil, LTable);
  AssertFalse(Boolean(LTable.Rows[0].IsPending));
  LSource.Page(0, 25, LFilters, LTable);
  AssertTrue('Cached full view must keep its original pending-color tip', Boolean(LTable.Rows[0].IsPending));
end;

procedure TWalletPagingTests.GlobalTotalsAreIndependentOfTheSelectedKey;
var
  LSource: TCountingPagedAccounts;
  LTable: TDataTable;
  LSummary: TBalanceSummary;
  LDeadline: QWord;
  LReads: Integer;
  LScope: TDisposables;
begin
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.LargeCount := 50;
  LSource.FilterKeys := TArray<TAccountKey>.Create(LSource.Keys[1]);
  LSource.Page(0, 25, nil, LTable);
  AssertEquals(0, LSource.BalanceReads);
  LDeadline := GetTickCount64 + 5000;
  while not LSource.TryGetBalanceSummary(LSummary) do begin
    if GetTickCount64 > LDeadline then Fail('Aggregate did not finish');
    CheckSynchronize(0);
    Sleep(5);
  end;
  AssertEquals(53, Integer(LSummary.TotalPASA));
  AssertEquals(Int64(7759), Int64(LSummary.TotalPASC));
  LReads := LSource.BalanceReads;
  LSource.FilterKeys := TArray<TAccountKey>.Create(LSource.Keys[0]);
  LSource.Page(0, 25, nil, LTable);
  AssertTrue(LSource.TryGetBalanceSummary(LSummary));
  AssertEquals('Changing key must not restart global totals', LReads, LSource.BalanceReads);
end;

procedure TWalletPagingTests.ChangedCoreStateDiscardsAggregateWithoutRetryChurn;
var
  LSource: TCountingPagedAccounts;
  LSummary: TBalanceSummary;
  LDeadline: QWord;
  LReads, i: Integer;
  LScope: TDisposables;
begin
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.LargeCount := 300;
  LSource.ChangeStateAfter := 10;
  LSource.TryGetBalanceSummary(LSummary);
  AssertEquals(Ord(wrSignaled), Ord(LSource.BalanceEntered.WaitFor(5000)));
  LDeadline := GetTickCount64 + 5000;
  while LSource.BalanceReads < 128 do begin
    if GetTickCount64 > LDeadline then Fail('Aggregate did not finish its coherent chunk');
    Sleep(1);
  end;
  AssertFalse('Mixed-state sum must not publish', LSource.TryGetBalanceSummary(LSummary));
  LReads := LSource.BalanceReads;
  for i := 0 to 4 do begin
    CheckSynchronize(0);
    Sleep(10);
    AssertFalse(LSource.TryGetBalanceSummary(LSummary));
  end;
  AssertEquals('A discarded generation must wait for the next backend invalidation', LReads, LSource.BalanceReads);
  LSource.ChangeStateAfter := 0;
  LSource.InvalidateWalletData;
  LDeadline := GetTickCount64 + 5000;
  while not LSource.TryGetBalanceSummary(LSummary) do begin
    if GetTickCount64 > LDeadline then Fail('New aggregate generation did not finish');
    CheckSynchronize(0);
    Sleep(5);
  end;
  AssertEquals(303, Integer(LSummary.TotalPASA));
end;

procedure TWalletPagingTests.AggregateCompletionPublishesWithoutPolling;
var
  LSource: TCountingPagedAccounts;
  LSummary: TBalanceSummary;
  LScope: TDisposables;
begin
  FCompletionCount := 0;
  FCompletedSummaryReady := False;
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.LargeCount := 50;
  LSource.OnBalanceSummaryCompleted := OnBalanceCompleted;
  AssertFalse('The initial request starts asynchronous work', LSource.TryGetBalanceSummary(LSummary));
  WaitForBalanceCompletion;
  AssertEquals('One completed task produces one event', 1, FCompletionCount);
  AssertEquals('Completion is delivered on the control owner thread',
    Int64(MainThreadID), Int64(FCompletionThread));
  AssertEquals(53, Integer(FCompletedSummary.TotalPASA));
  AssertEquals(Int64(7759), Int64(FCompletedSummary.TotalPASC));
  AssertEquals(53, LSource.BalanceReads);
end;

procedure TWalletPagingTests.AggregateCompletionCanRequestTheLatestGeneration;
var
  LSource: TCountingPagedAccounts;
  LSummary: TBalanceSummary;
  LScope: TDisposables;
begin
  FCompletionCount := 0;
  FCompletedSummaryReady := False;
  LSource := LScope.AddObject(TCountingPagedAccounts.Create) as TCountingPagedAccounts;
  LSource.LargeCount := 300;
  LSource.DelayBalance := 1;
  LSource.OnBalanceSummaryCompleted := OnBalanceCompleted;
  AssertFalse(LSource.TryGetBalanceSummary(LSummary));
  AssertEquals(Ord(wrSignaled), Ord(LSource.BalanceEntered.WaitFor(5000)));
  LSource.InvalidateWalletData;
  WaitForBalanceCompletion;
  AssertEquals('Discarded work reports completion before the latest task starts',
    2, FCompletionCount);
  AssertEquals(303, Integer(FCompletedSummary.TotalPASA));
end;

procedure TWalletPagingTests.DestroyRemovesAggregateCompletionCallbacks;
var
  LSource: TCountingPagedAccounts;
  LSummary: TBalanceSummary;
begin
  FCompletionCount := 0;
  FCompletedSummaryReady := False;
  LSource := TCountingPagedAccounts.Create;
  try
    LSource.LargeCount := 1;
    LSource.OnBalanceSummaryCompleted := OnBalanceCompleted;
    LSource.TryGetBalanceSummary(LSummary);
    AssertEquals(Ord(wrSignaled), Ord(LSource.BalanceEntered.WaitFor(5000)));
    // Leave the UI queue undelivered while the small aggregate completes.
    Sleep(30);
  finally
    LSource.Free;
  end;
  CheckSynchronize(0);
  AssertEquals('A destroyed source cannot notify a released receiver', 0, FCompletionCount);
end;

procedure TWalletPagingTests.DestroyCancelsAndJoinsTheAggregateWorker;
var
  LSource: TCountingPagedAccounts;
  LSummary: TBalanceSummary;
  LStarted: QWord;
begin
  LSource := TCountingPagedAccounts.Create;
  try
    LSource.LargeCount := 50000;
    LSource.DelayBalance := 1;
    LSource.TryGetBalanceSummary(LSummary);
    AssertEquals(Ord(wrSignaled), Ord(LSource.BalanceEntered.WaitFor(5000)));
  finally
    LStarted := GetTickCount64;
    LSource.Free;
  end;
  AssertTrue('Aggregate cancellation must join after a bounded chunk',
    GetTickCount64 - LStarted < 1500);
end;

initialization
  RegisterTest(TWalletPagingTests);

end.
