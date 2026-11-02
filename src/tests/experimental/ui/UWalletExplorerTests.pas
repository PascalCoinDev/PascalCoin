unit UWalletExplorerTests;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE.
  Generated snapshots and gated in-memory reads never connect to peers. }

{$mode delphi}

interface

uses Classes, SysUtils, SyncObjs, Forms, Generics.Collections, fpcunit,
  testregistry, UExplorerAccountSnapshots, UPCDataTypes, UGridUtils, UNode,
  UBlockChain, UFileStorage;

type
  TExplorerSnapshotFixture = class(TExplorerAccountSnapshots)
  private
    FLock: TCriticalSection;
    FBaseBalance: Int64;
    FBlockRead, FIgnoreCancellation, FReadOnMain, FCancelled, FFailRead: Boolean;
    FCalls: Integer;
    FRequests: TArray<Cardinal>;
  protected
    function ReadSnapshot(const AAccounts: TArray<Cardinal>;
      out ARows: TArray<TAccount>; out AAccountCount, ABlockCount: Integer;
      out ABalance: Int64): Boolean; override;
  public
    Entered, ReleaseRead: TEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Configure(ABaseBalance: Int64; ABlock, AIgnoreCancellation: Boolean);
    procedure BlockNext(ABaseBalance: Int64; AIgnoreCancellation: Boolean = False);
    procedure BlockNextError;
    function Calls: Integer;
    function Requests: TArray<Cardinal>;
    function ReadOnMain: Boolean;
    function Cancelled: Boolean;
  end;

  TExplorerOperationStorage = class(TFileStorage)
  private
    FCalls, FLastAccount, FCancelled: LongInt;
  protected
    function DoGetAccountOperations(AAccount, AMaxDepth, AStartOperation,
      AMaxOperations, ASearchBackwardsStartingAtBlock: Integer;
      const AOperationsResumeList: TOperationsResumeList): Boolean; override;
  public
    Entered, ReleaseRead: TEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Calls: Integer;
    function LastAccount: Integer;
    function Cancelled: Boolean;
  end;

  TExplorerAccountsGrid = class(TAccountsGrid)
  public
    function Read(AAccountNumber: Integer; out AAccount: TAccount;
      out ABlockCount, AAccountCount: Integer): Boolean;
  end;

  TWalletExplorerTests = class(TTestCase)
  private
    FSource: TExplorerSnapshotFixture;
    FObserved: TList<Int64>;
    FWrongCallbackThread: Boolean;
    FCompletionCount: Integer;
    FRefreshStateCount: Integer;
    FRefreshStateWasBusy: Boolean;
    FNode: TNode;
    FOperations: TOperationsGrid;
    FStorage: TExplorerOperationStorage;
    FAccounts: TExplorerAccountsGrid;
    FPreviousDirectory, FDirectory: String;
    FOwnsNetData: Boolean;
    FReadReady: Boolean;
    FReadCalls, FSummaryCalls: Integer;
    FFilterEntered, FFilterRelease, FFilterExited: TEvent;
    FIndexRelease: TEvent;
    FProcessedAdds: LongInt;
    FReleasedFilterOnMain: Boolean;
    procedure FilterProgress(Sender: TObject);
    procedure ReleaseFilter;
    procedure ReleaseIndex;
    procedure SelectedBalanceUpdated(Sender: TObject);
    procedure NewSelectedAccounts(ABlocks: Integer);
    procedure PumpSelectedAccounts;
    procedure Updated(Sender: TObject);
    procedure SnapshotCompleted(Sender: TObject);
    procedure AccountRefreshStateChanged(Sender: TObject);
    procedure NewSnapshots;
    procedure PumpUntilCalls(ACount: Integer);
    procedure PumpUntilIdle;
    procedure PumpOperations(AAccount: Cardinal);
    procedure NewOperations;
    function ReadAccount(Sender: TObject; AAccountNumber: Integer;
      out AAccount: TAccount; out ABlockCount, AAccountCount: Integer): Boolean;
    function ReadSummary(Sender: TObject; out AAccountCount: Integer;
      out ABalance: Int64): Boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure BlockedReadLeavesCachedLookupsResponsive;
    procedure PreviousRowsRemainUntilReplacementIsReady;
    procedure StaleGenerationCannotPublish;
    procedure FailedSnapshotCompletesWithoutPublishingOrRetrying;
    procedure VisibleReadRequestsAreBounded;
    procedure StopCancelsBlockedReadWithoutPublishing;
    procedure LegacyOperationsCoalesceWithoutWaitingOnUI;
    procedure LegacyOperationsDestructionJoinsCancelledRead;
    procedure LegacyAccountReadersUseSnapshotCallbacks;
    procedure RetiredAccountFiltersRemainBoundedAndJoinBeforeNodeHandoff;
    procedure RetiredFilterToNodePublishesIdleAfterEarlierSnapshotCompletion;
    procedure StoredHistoryCancellationStopsAfterTheCurrentBlockRead;
    procedure SelectedTotalsReadMoreThanTheRowCacheOffTheUIThread;
    procedure SelectedTotalsRetainCompletedValueAndCancelObsoleteRequests;
    procedure CapturedKeySelectionDoesNotWaitForIndexAndJoinsCancelledResolver;
    procedure CapturedMissingKeyIsEmptyAndLegacyAllKeysRemainAvailable;
    procedure CapturedKeyCancellationStopsInsideTheRecordChunk;
  end;

implementation

uses UAccounts, UConst, UCrypto, UNetProtocol, UThread, UPCOrderedLists;

type
  TExplorerSafeBoxAccess = class(TPCSafeBox)
  public
    procedure SeedGenesis(const AKey: TAccountKey);
    procedure SeedBlocks(const AKey: TAccountKey; ACount: Integer; AReward: Int64);
  end;

  TSelectedSafeBoxHolder = class(TThread)
  private
    FSafeBox: TPCSafeBox;
  protected
    procedure Execute; override;
  public
    Entered, ReleaseRead: TEvent;
    TimedOut: Boolean;
    constructor Create(ASafeBox: TPCSafeBox);
    destructor Destroy; override;
  end;

  TAccountKeyIndexHolder = class(TThread)
  private
    FKeys: TOrderedAccountKeysList;
  protected
    procedure Execute; override;
  public
    Entered, ReleaseRead: TEvent;
    TimedOut: Boolean;
    constructor Create(AKeys: TOrderedAccountKeysList);
    destructor Destroy; override;
  end;

  TQueuedFilterRelease = class(TThread)
  private
    FAction: TThreadMethod;
  protected
    procedure Execute; override;
  public
    Entered: TEvent;
    constructor Create(AAction: TThreadMethod);
    destructor Destroy; override;
  end;

  TStoredHistoryGate = class(TFileStorage)
  protected
    function GetFirstBlockNumber: Int64; override;
    function GetLastBlockNumber: Int64; override;
    function DoLoadBlockChain(Operations: TPCOperationsComp; Block: Cardinal): Boolean; override;
  public
    Entered, ReleaseRead: TEvent;
    Loads: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TStoredHistoryReader = class(TThread)
  private
    FStorage: TStorage;
  protected
    procedure Execute; override;
  public
    Completed: TEvent;
    Rows: TOperationsResumeList;
    Cancelled: Boolean;
    ErrorMessage: String;
    constructor Create(AStorage: TStorage);
    destructor Destroy; override;
  end;

procedure TExplorerSafeBoxAccess.SeedGenesis(const AKey: TAccountKey);
begin
  SeedBlocks(AKey, 1, 0);
end;

procedure TExplorerSafeBoxAccess.SeedBlocks(const AKey: TAccountKey;
  ACount: Integer; AReward: Int64);
var LBlock: TOperationBlock;
  I: Integer;
begin
  LBlock := CT_OperationBlock_NUL;
  LBlock.account_key := AKey;
  LBlock.protocol_version := CT_PROTOCOL_1;
  LBlock.protocol_available := CT_PROTOCOL_6;
  LBlock.compact_target := CT_MinCompactTarget_v1;
  LBlock.reward := AReward;
  for I := 1 to ACount do begin
    LBlock.block := BlocksCount;
    AddNew(LBlock);
  end;
end;

constructor TSelectedSafeBoxHolder.Create(ASafeBox: TPCSafeBox);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSafeBox := ASafeBox;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, False, '');
  Start;
end;

destructor TSelectedSafeBoxHolder.Destroy;
begin
  ReleaseRead.SetEvent;
  WaitFor;
  Entered.Free;
  ReleaseRead.Free;
  inherited;
end;

procedure TSelectedSafeBoxHolder.Execute;
begin
  FSafeBox.StartThreadSafe;
  try
    Entered.SetEvent;
    TimedOut := ReleaseRead.WaitFor(10000) <> wrSignaled;
  finally FSafeBox.EndThreadSave; end;
end;

constructor TAccountKeyIndexHolder.Create(AKeys: TOrderedAccountKeysList);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FKeys := AKeys;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, False, '');
  Start;
end;

destructor TAccountKeyIndexHolder.Destroy;
begin
  ReleaseRead.SetEvent;
  WaitFor;
  Entered.Free;
  ReleaseRead.Free;
  inherited;
end;

procedure TAccountKeyIndexHolder.Execute;
begin
  FKeys.Lock;
  try
    Entered.SetEvent;
    TimedOut := ReleaseRead.WaitFor(10000) <> wrSignaled;
  finally FKeys.Unlock; end;
end;

constructor TQueuedFilterRelease.Create(AAction: TThreadMethod);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FAction := AAction;
  Entered := TEvent.Create(nil, True, False, '');
  Start;
end;

destructor TQueuedFilterRelease.Destroy;
begin
  WaitFor;
  Entered.Free;
  inherited;
end;

procedure TQueuedFilterRelease.Execute;
begin
  Entered.SetEvent;
  Synchronize(FAction);
end;

constructor TStoredHistoryGate.Create(AOwner: TComponent);
begin
  inherited;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, False, '');
end;

destructor TStoredHistoryGate.Destroy;
begin
  Entered.Free;
  ReleaseRead.Free;
  inherited;
end;

function TStoredHistoryGate.GetFirstBlockNumber: Int64;
begin
  Result := 0;
end;

function TStoredHistoryGate.GetLastBlockNumber: Int64;
begin
  Result := 0;
end;

function TStoredHistoryGate.DoLoadBlockChain(Operations: TPCOperationsComp; Block: Cardinal): Boolean;
begin
  Inc(Loads);
  Entered.SetEvent;
  if ReleaseRead.WaitFor(10000) <> wrSignaled then
    raise Exception.Create('Stored block read gate timed out');
  Result := True;
end;

constructor TStoredHistoryReader.Create(AStorage: TStorage);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FStorage := AStorage;
  Completed := TEvent.Create(nil, True, False, '');
  Rows := TOperationsResumeList.Create;
  Start;
end;

destructor TStoredHistoryReader.Destroy;
begin
  Terminate;
  WaitFor;
  Rows.Free;
  Completed.Free;
  inherited;
end;

procedure TStoredHistoryReader.Execute;
begin
  try
    try
      FStorage.GetAccountOperations(0, -1, 0, 5000, 0, Rows);
    except
      on E: EAbort do Cancelled := True;
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally Completed.SetEvent; end;
end;

constructor TExplorerSnapshotFixture.Create;
begin
  inherited Create(nil, nil);
  FLock := TCriticalSection.Create;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, True, '');
  Configure(1000, False, False);
end;

destructor TExplorerSnapshotFixture.Destroy;
begin
  // Even an intentionally uncooperative stale read must finish before its
  // fixture events and lock disappear, including when an assertion fails.
  ReleaseRead.SetEvent;
  Stop;
  Entered.Free;
  ReleaseRead.Free;
  FLock.Free;
  inherited;
end;

procedure TExplorerSnapshotFixture.Configure(ABaseBalance: Int64;
  ABlock, AIgnoreCancellation: Boolean);
begin
  FLock.Acquire;
  try
    FBaseBalance := ABaseBalance;
    FBlockRead := ABlock;
    FIgnoreCancellation := AIgnoreCancellation;
    FFailRead := False;
  finally FLock.Release; end;
end;

procedure TExplorerSnapshotFixture.BlockNext(ABaseBalance: Int64;
  AIgnoreCancellation: Boolean);
begin
  Entered.ResetEvent;
  ReleaseRead.ResetEvent;
  Configure(ABaseBalance, True, AIgnoreCancellation);
end;

procedure TExplorerSnapshotFixture.BlockNextError;
begin
  BlockNext(0, True);
  FLock.Acquire;
  try FFailRead := True; finally FLock.Release; end;
end;

function TExplorerSnapshotFixture.ReadSnapshot(const AAccounts: TArray<Cardinal>;
  out ARows: TArray<TAccount>; out AAccountCount, ABlockCount: Integer;
  out ABalance: Int64): Boolean;
var LBalance: Int64; LBlock, LIgnore, LFail: Boolean; I: Integer;
begin
  FLock.Acquire;
  try
    Inc(FCalls);
    FRequests := Copy(AAccounts);
    FReadOnMain := FReadOnMain or (TThread.CurrentThread.ThreadID = MainThreadID);
    LBalance := FBaseBalance;
    LBlock := FBlockRead;
    LIgnore := FIgnoreCancellation;
    LFail := FFailRead;
  finally FLock.Release; end;
  Entered.SetEvent;
  while LBlock and (ReleaseRead.WaitFor(10) <> wrSignaled) do
    if (not LIgnore) and TThread.CheckTerminated then begin
      FLock.Acquire;
      try FCancelled := True; finally FLock.Release; end;
      Exit(False);
    end;
  if (not LIgnore) and TThread.CheckTerminated then Exit(False);
  if LFail then raise EInvalidOperation.Create('Obsolete snapshot failure');
  AAccountCount := 4200000;
  ABlockCount := 840000;
  ABalance := LBalance * AAccountCount;
  SetLength(ARows, Length(AAccounts));
  for I := 0 to High(AAccounts) do begin
    ARows[I] := CT_Account_NUL;
    ARows[I].account := AAccounts[I];
    ARows[I].balance := UInt64(LBalance) + AAccounts[I];
  end;
  Result := True;
end;

function TExplorerSnapshotFixture.Calls: Integer;
begin
  FLock.Acquire; try Result := FCalls; finally FLock.Release; end;
end;

function TExplorerSnapshotFixture.Requests: TArray<Cardinal>;
begin
  FLock.Acquire; try Result := Copy(FRequests); finally FLock.Release; end;
end;

function TExplorerSnapshotFixture.ReadOnMain: Boolean;
begin
  FLock.Acquire; try Result := FReadOnMain; finally FLock.Release; end;
end;

function TExplorerSnapshotFixture.Cancelled: Boolean;
begin
  FLock.Acquire; try Result := FCancelled; finally FLock.Release; end;
end;

constructor TExplorerOperationStorage.Create(AOwner: TComponent);
begin
  inherited;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, False, '');
end;

destructor TExplorerOperationStorage.Destroy;
begin
  ReleaseRead.SetEvent;
  Entered.Free;
  ReleaseRead.Free;
  inherited;
end;

function TExplorerOperationStorage.DoGetAccountOperations(AAccount, AMaxDepth,
  AStartOperation, AMaxOperations, ASearchBackwardsStartingAtBlock: Integer;
  const AOperationsResumeList: TOperationsResumeList): Boolean;
var LRow: TOperationResume;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    raise Exception.Create('History was fetched on the UI thread');
  InterlockedExchange(FLastAccount, AAccount);
  InterlockedIncrement(FCalls);
  Entered.SetEvent;
  while ReleaseRead.WaitFor(10) <> wrSignaled do
    if TThread.CheckTerminated then begin
      InterlockedExchange(FCancelled, 1);
      Abort;
    end;
  if TThread.CheckTerminated then Abort;
  LRow := CT_TOperationResume_NUL;
  LRow.AffectedAccount := AAccount;
  LRow.Amount := 10000 + AAccount;
  AOperationsResumeList.Add(LRow);
  Result := True;
end;

function TExplorerOperationStorage.Calls: Integer;
begin
  Result := InterlockedCompareExchange(FCalls, 0, 0);
end;

function TExplorerOperationStorage.LastAccount: Integer;
begin
  Result := InterlockedCompareExchange(FLastAccount, 0, 0);
end;

function TExplorerOperationStorage.Cancelled: Boolean;
begin
  Result := InterlockedCompareExchange(FCancelled, 0, 0) <> 0;
end;

function TExplorerAccountsGrid.Read(AAccountNumber: Integer; out AAccount: TAccount;
  out ABlockCount, AAccountCount: Integer): Boolean;
begin
  Result := BufferGetAccount(AAccountNumber, AAccount, ABlockCount, AAccountCount);
end;

procedure TWalletExplorerTests.SetUp;
begin
  FObserved := TList<Int64>.Create;
  FCompletionCount := 0;
  FRefreshStateCount := 0;
  FRefreshStateWasBusy := False;
end;

procedure TWalletExplorerTests.TearDown;
begin
  if Assigned(FFilterRelease) then FFilterRelease.SetEvent;
  if Assigned(FStorage) then FStorage.ReleaseRead.SetEvent;
  FOperations.Free;
  FAccounts.Free;
  if FOwnsNetData and TNetData.NetDataExists then TNetData.NetData.Free;
  FNode.Free;
  FSource.Free;
  FFilterEntered.Free;
  FFilterRelease.Free;
  FFilterExited.Free;
  FObserved.Free;
  if FDirectory <> '' then begin
    TNode.SetPascalCoinDataFolder(FPreviousDirectory);
    RemoveDir(FDirectory);
  end;
end;

procedure TWalletExplorerTests.Updated(Sender: TObject);
var LCount: Integer; LBalance: Int64;
begin
  FWrongCallbackThread := FWrongCallbackThread or
    (TThread.CurrentThread.ThreadID <> MainThreadID);
  if FSource.TryGetSummary(LCount, LBalance) then FObserved.Add(LBalance);
end;

procedure TWalletExplorerTests.NewSnapshots;
begin
  FSource := TExplorerSnapshotFixture.Create;
  FSource.OnUpdated := Updated;
  FSource.OnCompleted := SnapshotCompleted;
end;

procedure TWalletExplorerTests.SnapshotCompleted(Sender: TObject);
begin
  FWrongCallbackThread := FWrongCallbackThread or
    (TThread.CurrentThread.ThreadID <> MainThreadID);
  Inc(FCompletionCount);
end;

procedure TWalletExplorerTests.AccountRefreshStateChanged(Sender: TObject);
begin
  FWrongCallbackThread := FWrongCallbackThread or
    (TThread.CurrentThread.ThreadID <> MainThreadID);
  FRefreshStateWasBusy := FRefreshStateWasBusy or FAccounts.IsUpdatingData;
  Inc(FRefreshStateCount);
end;

procedure TWalletExplorerTests.FailedSnapshotCompletesWithoutPublishingOrRetrying;
var LRow: TAccount; LBlocks, LCount: Integer; LDeadline: QWord;
begin
  NewSnapshots;
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  PumpUntilIdle;
  AssertEquals(1, FCompletionCount);
  AssertEquals(1, FObserved.Count);
  FSource.BlockNextError;
  FSource.Invalidate;
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  PumpUntilCalls(2);
  FSource.ReleaseRead.SetEvent;
  LDeadline := GetTickCount64 + 4000;
  repeat
    Application.ProcessMessages;
    Sleep(1);
  until (not FSource.Updating) or (GetTickCount64 >= LDeadline);
  AssertFalse('The failed read must retire', FSource.Updating);
  AssertTrue('A failed read must retain its diagnostic', FSource.ErrorMessage <> '');
  AssertEquals('Terminal failure must release event-driven readiness', 2, FCompletionCount);
  AssertEquals('Failure must not announce a replacement data snapshot', 1, FObserved.Count);
  AssertFalse(FWrongCallbackThread);
  LDeadline := GetTickCount64 + 300;
  repeat
    Application.ProcessMessages;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  AssertEquals('Completion alone must not start another read', 2, FSource.Calls);
end;

procedure TWalletExplorerTests.PumpUntilCalls(ACount: Integer);
var LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 4000;
  repeat
    Application.ProcessMessages;
    if FSource.Calls >= ACount then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  AssertEquals('The owned reader must enter its gate', ACount, FSource.Calls);
  AssertFalse('Snapshot reads must run off the main thread', FSource.ReadOnMain);
end;

procedure TWalletExplorerTests.PumpUntilIdle;
var LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 4000;
  while FSource.Updating and (GetTickCount64 < LDeadline) do begin
    Application.ProcessMessages;
    Sleep(1);
  end;
  AssertFalse('Snapshot publication must finish', FSource.Updating);
  AssertFalse('Snapshot callbacks must run on the main thread', FWrongCallbackThread);
  AssertEquals('', FSource.ErrorMessage);
end;

procedure TWalletExplorerTests.BlockedReadLeavesCachedLookupsResponsive;
var LRow: TAccount; LBlocks, LCount, I: Integer; LBalance: Int64; LStart: QWord;
begin
  NewSnapshots;
  FSource.BlockNext(1000);
  AssertFalse(FSource.TryGetAccount(7, LRow, LBlocks, LCount));
  PumpUntilCalls(1);
  LStart := GetTickCount64;
  for I := 1 to 1000 do begin
    AssertFalse(FSource.TryGetSummary(LCount, LBalance));
    AssertFalse(FSource.TryGetAccount(7, LRow, LBlocks, LCount));
  end;
  AssertTrue('Painting must not wait behind the gated worker', GetTickCount64 - LStart < 500);
  AssertEquals(0, FObserved.Count);
  FSource.ReleaseRead.SetEvent;
  PumpUntilIdle;
  AssertTrue(FSource.TryGetSummary(LCount, LBalance));
  AssertEquals(4200000, LCount);
  AssertEquals(Int64(4200000000), LBalance);
  AssertTrue(FSource.TryGetAccount(7, LRow, LBlocks, LCount));
  AssertEquals(Int64(1007), Int64(LRow.balance));
end;

procedure TWalletExplorerTests.PreviousRowsRemainUntilReplacementIsReady;
var LRow: TAccount; LBlocks, LCount: Integer; LBalance: Int64;
begin
  NewSnapshots;
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  PumpUntilIdle;
  FSource.BlockNext(2000);
  FSource.Invalidate;
  AssertTrue(FSource.TryGetAccount(7, LRow, LBlocks, LCount));
  PumpUntilCalls(2);
  AssertTrue(FSource.TryGetAccount(7, LRow, LBlocks, LCount));
  AssertEquals(Int64(1007), Int64(LRow.balance));
  AssertTrue(FSource.TryGetSummary(LCount, LBalance));
  AssertEquals(Int64(4200000000), LBalance);
  FSource.ReleaseRead.SetEvent;
  PumpUntilIdle;
  AssertTrue(FSource.TryGetAccount(7, LRow, LBlocks, LCount));
  AssertEquals(Int64(2007), Int64(LRow.balance));
  AssertEquals(2, FObserved.Count);
end;

procedure TWalletExplorerTests.StaleGenerationCannotPublish;
var LRow: TAccount; LBlocks, LCount: Integer;
begin
  NewSnapshots;
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  PumpUntilIdle;
  FSource.BlockNext(2000, True);
  FSource.Invalidate;
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  PumpUntilCalls(2);
  // The old read deliberately returns success even after cancellation. Only
  // the real component's generation check can prevent stale publication.
  FSource.Configure(3000, False, False);
  FSource.Invalidate;
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  FSource.ReleaseRead.SetEvent;
  PumpUntilIdle;
  AssertEquals(3, FSource.Calls);
  AssertEquals(2, FObserved.Count);
  AssertEquals(Int64(4200000000), FObserved[0]);
  AssertEquals(Int64(12600000000), FObserved[1]);
  AssertTrue(FSource.TryGetAccount(7, LRow, LBlocks, LCount));
  AssertEquals(Int64(3007), Int64(LRow.balance));
  // Obsolete failures must be rejected just like obsolete successful rows.
  FSource.BlockNextError;
  FSource.Invalidate;
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  PumpUntilCalls(4);
  FSource.Configure(4000, False, False);
  FSource.Invalidate;
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  FSource.ReleaseRead.SetEvent;
  PumpUntilIdle;
  AssertEquals('', FSource.ErrorMessage);
  AssertTrue(FSource.TryGetAccount(7, LRow, LBlocks, LCount));
  AssertEquals(Int64(4007), Int64(LRow.balance));
end;

procedure TWalletExplorerTests.VisibleReadRequestsAreBounded;
var LRow: TAccount; LBlocks, LCount, I: Integer; LRequested: TArray<Cardinal>;
begin
  NewSnapshots;
  FSource.BlockNext(1000);
  // No message pumping here: all requests precede the first real timer tick.
  for I := 0 to 9999 do FSource.TryGetAccount(I, LRow, LBlocks, LCount);
  PumpUntilCalls(1);
  LRequested := FSource.Requests;
  AssertEquals('Read only the bounded visible batch', 128, Length(LRequested));
  for I := 0 to High(LRequested) do AssertEquals(I, Integer(LRequested[I]));
  FSource.Stop;
end;

procedure TWalletExplorerTests.StopCancelsBlockedReadWithoutPublishing;
var LRow: TAccount; LBlocks, LCount: Integer; LStart: QWord;
begin
  NewSnapshots;
  FSource.BlockNext(1000);
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  PumpUntilCalls(1);
  LStart := GetTickCount64;
  FSource.Stop;
  AssertTrue('Stop must cancel the blocked reader', GetTickCount64 - LStart < 1000);
  AssertTrue(FSource.Cancelled);
  AssertFalse(FSource.Updating);
  FSource.ReleaseRead.SetEvent;
  Application.ProcessMessages;
  CheckSynchronize;
  AssertEquals('No callback may publish after Stop', 0, FObserved.Count);
  AssertFalse(FSource.TryGetAccount(7, LRow, LBlocks, LCount));
end;

procedure TWalletExplorerTests.NewOperations;
var LID: TGUID;
begin
  TCrypto.InitCrypto;
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  CreateGUID(LID);
  FDirectory := IncludeTrailingPathDelimiter(GetTempDir) + 'WalletExplorer-' + GUIDToString(LID);
  ForceDirectories(FDirectory);
  TNode.SetPascalCoinDataFolder(FDirectory);
  FOwnsNetData := not TNetData.NetDataExists;
  FNode := TNode.Create(nil);
  FNode.BroadcastData := False;
  FNode.Bank.StorageClass := TExplorerOperationStorage;
  FStorage := TExplorerOperationStorage(FNode.Bank.Storage);
  FOperations := TOperationsGrid.Create(nil);
  FOperations.AsynchronousUpdates := True;
  FOperations.AutoRefreshOnNodeEvents := False;
  FOperations.MustShowAlwaysAnAccount := True;
  FOperations.AccountNumber := -1;
  FOperations.Node := FNode;
end;

procedure TWalletExplorerTests.PumpOperations(AAccount: Cardinal);
var LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 4000;
  repeat
    Application.ProcessMessages;
    if (FOperations.OperationsResume.Count = 1) and
      (FOperations.OperationsResume[0].AffectedAccount = AAccount) then Exit;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Fail('The latest operation request was not published');
end;

procedure TWalletExplorerTests.LegacyOperationsCoalesceWithoutWaitingOnUI;
var LDeadline, LStart: QWord; LWorker: TOperationsGridUpdateThread;
begin
  NewOperations;
  FOperations.AccountNumber := 7;
  LDeadline := GetTickCount64 + 4000;
  while (FStorage.Calls = 0) and (GetTickCount64 < LDeadline) do begin
    Application.ProcessMessages; Sleep(1);
  end;
  AssertEquals(1, FStorage.Calls);
  LWorker := TOperationsGridUpdateThread(TPCThread.GetThreadByClass(TOperationsGridUpdateThread, nil));
  AssertTrue(Assigned(LWorker));
  FStorage.ReleaseRead.SetEvent;
  LWorker.WaitFor;
  AssertEquals('The initial request must complete without an error', '', LWorker.FError);
  AssertTrue('The initial request must be publishable', LWorker.FSucceeded);
  PumpOperations(7);
  FStorage.ReleaseRead.ResetEvent;
  FStorage.Entered.ResetEvent;
  FOperations.AccountNumber := 9;
  LDeadline := GetTickCount64 + 4000;
  while (FStorage.Calls < 2) and (GetTickCount64 < LDeadline) do begin
    Application.ProcessMessages; Sleep(1);
  end;
  AssertEquals(2, FStorage.Calls);
  LStart := GetTickCount64;
  FOperations.AccountNumber := 11;
  AssertTrue('Changing selection must not join the blocked request', GetTickCount64 - LStart < 500);
  AssertEquals('Keep the last displayed rows while the request is blocked',
    7, Integer(FOperations.OperationsResume[0].AffectedAccount));
  FStorage.ReleaseRead.SetEvent;
  PumpOperations(11);
  AssertEquals(1, FOperations.OperationsResume.Count);
  AssertEquals(11, FStorage.LastAccount);
end;

procedure TWalletExplorerTests.LegacyOperationsDestructionJoinsCancelledRead;
var LDeadline, LStart: QWord;
begin
  NewOperations;
  FOperations.AccountNumber := 7;
  LDeadline := GetTickCount64 + 4000;
  while (FStorage.Calls = 0) and (GetTickCount64 < LDeadline) do begin
    Application.ProcessMessages; Sleep(1);
  end;
  AssertEquals(1, FStorage.Calls);
  LStart := GetTickCount64;
  FreeAndNil(FOperations);
  AssertTrue('Destruction must cancel and join the blocked history job', GetTickCount64 - LStart < 1000);
  AssertTrue(FStorage.Cancelled);
  Application.ProcessMessages;
  CheckSynchronize;
  AssertEquals(1, FStorage.Calls);
end;

function TWalletExplorerTests.ReadAccount(Sender: TObject; AAccountNumber: Integer;
  out AAccount: TAccount; out ABlockCount, AAccountCount: Integer): Boolean;
begin
  Inc(FReadCalls);
  AAccount := CT_Account_NUL;
  AAccount.account := AAccountNumber;
  AAccount.balance := 16537985;
  ABlockCount := 840000;
  AAccountCount := 4200000;
  Result := FReadReady;
end;

function TWalletExplorerTests.ReadSummary(Sender: TObject; out AAccountCount: Integer;
  out ABalance: Int64): Boolean;
begin
  Inc(FSummaryCalls);
  AAccountCount := 4200000;
  ABalance := 5108944375;
  Result := FReadReady;
end;

procedure TWalletExplorerTests.LegacyAccountReadersUseSnapshotCallbacks;
var LRow: TAccount; LBlocks, LCount: Integer;
begin
  NewOperations;
  FAccounts := TExplorerAccountsGrid.Create(nil);
  FAccounts.OnReadAccount := ReadAccount;
  FAccounts.OnReadSummary := ReadSummary;
  FAccounts.Node := FNode;
  FReadReady := False;
  AssertFalse(FAccounts.Read(77, LRow, LBlocks, LCount));
  AssertEquals(0, FAccounts.AccountsCount);
  FReadReady := True;
  AssertTrue(FAccounts.Read(77, LRow, LBlocks, LCount));
  AssertEquals(77, Integer(LRow.account));
  AssertEquals(Int64(16537985), Int64(LRow.balance));
  AssertEquals(840000, LBlocks);
  AssertEquals(4200000, LCount);
  FAccounts.UpdateData;
  AssertEquals(4200000, FAccounts.AccountsCount);
  AssertEquals(Int64(5108944375), FAccounts.AccountsBalance);
  AssertEquals(2, FReadCalls);
  AssertTrue(FSummaryCalls > 0);
  AssertEquals('The real node is empty; counts came from the callback', 0, Integer(FNode.Bank.AccountsCount));
end;

procedure TWalletExplorerTests.FilterProgress(Sender: TObject);
begin
  InterlockedIncrement(FProcessedAdds);
  FFilterEntered.SetEvent;
  if FFilterRelease.WaitFor(10000) <> wrSignaled then
    raise Exception.Create('Account filter progress gate timed out');
  FFilterExited.SetEvent;
end;

procedure TWalletExplorerTests.ReleaseFilter;
begin
  FReleasedFilterOnMain := TThread.CurrentThread.ThreadID = MainThreadID;
  FFilterRelease.SetEvent;
end;

procedure TWalletExplorerTests.ReleaseIndex;
begin
  FReleasedFilterOnMain := TThread.CurrentThread.ThreadID = MainThreadID;
  FIndexRelease.SetEvent;
end;

procedure TWalletExplorerTests.RetiredAccountFiltersRemainBoundedAndJoinBeforeNodeHandoff;
var
  LKey: TECPrivateKey;
  LWorker: TAccountsGridUpdateThread;
  LRelease: TQueuedFilterRelease;
  LFilter: TAccountsGridFilter;
  LDeadline, LStarted: QWord;
  I: Integer;
begin
  NewOperations;
  LKey := TECPrivateKey.Create;
  try
    LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    TExplorerSafeBoxAccess(FNode.Bank.SafeBox).SeedGenesis(LKey.PublicKey);
  finally LKey.Free; end;
  FFilterEntered := TEvent.Create(nil, True, False, '');
  FFilterRelease := TEvent.Create(nil, True, False, '');
  FFilterExited := TEvent.Create(nil, True, False, '');
  FAccounts := TExplorerAccountsGrid.Create(nil);
  FAccounts.Node := FNode;
  LFilter := CT_TAccountsGridFilter_NUL;
  LFilter.MinBalance := 0;
  FAccounts.AccountsGridFilter := LFilter;
  // Pause account access before the query starts, then attach the existing
  // processed-list event. Its first Add supplies a deterministic worker gate.
  FNode.Bank.SafeBox.StartThreadSafe;
  try
    FAccounts.AccountsGridDatasource := acds_NodeFiltered;
    LDeadline := GetTickCount64 + 3000;
    repeat
      LWorker := TAccountsGridUpdateThread(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, nil));
      if Assigned(LWorker) then Break;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    AssertTrue('The real account filter worker must start', Assigned(LWorker));
    LWorker.FProcessedList.OnListChanged := FilterProgress;
  finally FNode.Bank.SafeBox.EndThreadSave; end;
  AssertTrue('The worker must reach the account-add gate',
    FFilterEntered.WaitFor(3000) = wrSignaled);
  LStarted := GetTickCount64;
  for I := 1 to 100 do begin
    LFilter.MaxBalance := I;
    FAccounts.AccountsGridFilter := LFilter;
  end;
  AssertTrue('Normal filter changes must not join the gated old request',
    GetTickCount64 - LStarted < 500);
  AssertFalse('Cancellation must not spawn a second account filter query',
    Assigned(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, LWorker)));
  LRelease := TQueuedFilterRelease.Create(ReleaseFilter);
  try
    AssertTrue(LRelease.Entered.WaitFor(3000) = wrSignaled);
    // Do not pump the queued release here. A correct Node handoff must join
    // the retired worker, servicing synchronization until that join completes.
    FAccounts.Node := nil;
    AssertTrue('The handoff must wait for the retired reader', FReleasedFilterOnMain);
    AssertTrue(FFilterExited.WaitFor(0) = wrSignaled);
    AssertFalse(Assigned(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, nil)));
    Application.ProcessMessages;
    AssertFalse('Node removal also cancels the coalesced replacement', FAccounts.IsUpdatingData);
  finally
    FFilterRelease.SetEvent;
    LRelease.Free;
  end;
end;

procedure TWalletExplorerTests.RetiredFilterToNodePublishesIdleAfterEarlierSnapshotCompletion;
var LKey: TECPrivateKey; LWorker: TAccountsGridUpdateThread;
  LFilter: TAccountsGridFilter; LDeadline: QWord;
  LRow: TAccount; LBlocks, LCount, LCalls: Integer;
begin
  NewOperations;
  LKey := TECPrivateKey.Create;
  try
    LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    TExplorerSafeBoxAccess(FNode.Bank.SafeBox).SeedGenesis(LKey.PublicKey);
  finally LKey.Free; end;
  FFilterEntered := TEvent.Create(nil, True, False, '');
  FFilterRelease := TEvent.Create(nil, True, False, '');
  FFilterExited := TEvent.Create(nil, True, False, '');
  FAccounts := TExplorerAccountsGrid.Create(nil);
  FAccounts.Node := FNode;
  FAccounts.OnReadSummary := ReadSummary;
  FAccounts.OnRefreshStateChanged := AccountRefreshStateChanged;
  LFilter := CT_TAccountsGridFilter_NUL;
  LFilter.MinBalance := 0;
  FAccounts.AccountsGridFilter := LFilter;
  FNode.Bank.SafeBox.StartThreadSafe;
  try
    FAccounts.AccountsGridDatasource := acds_NodeFiltered;
    LDeadline := GetTickCount64 + 3000;
    repeat
      LWorker := TAccountsGridUpdateThread(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, nil));
      if Assigned(LWorker) then Break;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    AssertTrue('A real filtered-account reader must start', Assigned(LWorker));
    LWorker.FProcessedList.OnListChanged := FilterProgress;
  finally FNode.Bank.SafeBox.EndThreadSave; end;
  AssertEquals('The actual reader reaches its processed-account gate',
    Ord(wrSignaled), Ord(FFilterEntered.WaitFor(3000)));

  // Switching to the unfiltered node view cancels the old worker but must not
  // wait for it on the UI thread or invent a replacement filter query.
  FAccounts.AccountsGridDatasource := acds_Node;
  AssertTrue(LWorker.Terminated);
  AssertTrue('A cancelled reader still counts until its owner retires it', FAccounts.IsUpdatingData);
  AssertEquals(0, FRefreshStateCount);

  // Reproduce the missing ordering: the independent snapshot completion arrives
  // while the cancelled filter remains held. It cannot be the later idle signal.
  NewSnapshots;
  FSource.TryGetAccount(7, LRow, LBlocks, LCount);
  PumpUntilIdle;
  AssertEquals(1, FCompletionCount);
  AssertTrue('The snapshot completed before the account reader retired', FAccounts.IsUpdatingData);
  AssertEquals(0, FRefreshStateCount);
  LCalls := FSource.Calls;
  FFilterRelease.SetEvent;
  LDeadline := GetTickCount64 + 4000;
  while FAccounts.IsUpdatingData and (GetTickCount64 < LDeadline) do begin
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  end;
  AssertFalse('The existing retirement owner must become idle', FAccounts.IsUpdatingData);
  AssertEquals('The real busy-to-idle transition notifies exactly once', 1, FRefreshStateCount);
  AssertFalse('Readiness is committed before the callback', FRefreshStateWasBusy);
  AssertFalse('The callback runs on the main thread', FWrongCallbackThread);
  AssertEquals(Ord(wrSignaled), Ord(FFilterExited.WaitFor(0)));
  AssertFalse('No cancelled/replacement filter remains',
    Assigned(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, nil)));
  LDeadline := GetTickCount64 + 150;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  AssertEquals('Idle observation cannot create another completion', 1, FRefreshStateCount);
  AssertEquals('Readiness does not invalidate/refetch snapshots', LCalls, FSource.Calls);
end;

procedure TWalletExplorerTests.StoredHistoryCancellationStopsAfterTheCurrentBlockRead;
var
  LBank: TPCBank;
  LKey: TECPrivateKey;
  LStorage: TStoredHistoryGate;
  LWorker: TStoredHistoryReader;
begin
  TCrypto.InitCrypto;
  LBank := TPCBank.Create(nil);
  LKey := TECPrivateKey.Create;
  try
    LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    TExplorerSafeBoxAccess(LBank.SafeBox).SeedGenesis(LKey.PublicKey);
    LBank.StorageClass := TStoredHistoryGate;
    LStorage := TStoredHistoryGate(LBank.Storage);
    LWorker := TStoredHistoryReader.Create(LStorage);
    try
      AssertTrue('The real stored-history traversal must reach its first block',
        LStorage.Entered.WaitFor(3000) = wrSignaled);
      LWorker.Terminate;
      LStorage.ReleaseRead.SetEvent;
      AssertTrue(LWorker.Completed.WaitFor(3000) = wrSignaled);
      LWorker.WaitFor;
      AssertTrue('Cancellation must escape immediately after the active I/O completes', LWorker.Cancelled);
      AssertEquals('', LWorker.ErrorMessage);
      AssertEquals('No later block may be read', 1, LStorage.Loads);
      AssertEquals('No partial history may be published', 0, LWorker.Rows.Count);
    finally
      LStorage.ReleaseRead.SetEvent;
      LWorker.Free;
    end;
  finally
    LKey.Free;
    LBank.Free;
  end;
end;

procedure TWalletExplorerTests.SelectedBalanceUpdated(Sender: TObject);
begin
  FWrongCallbackThread := FWrongCallbackThread or
    (TThread.CurrentThread.ThreadID <> MainThreadID);
  FObserved.Add(FAccounts.AccountsBalance);
end;

procedure TWalletExplorerTests.NewSelectedAccounts(ABlocks: Integer);
var LKey: TECPrivateKey;
begin
  NewOperations;
  LKey := TECPrivateKey.Create;
  try
    LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    TExplorerSafeBoxAccess(FNode.Bank.SafeBox).SeedBlocks(LKey.PublicKey, ABlocks, 10000);
  finally LKey.Free; end;
  FAccounts := TExplorerAccountsGrid.Create(nil);
  FAccounts.OnReadAccount := ReadAccount;
  FAccounts.AccountsGridDatasource := acds_InternalList;
  FAccounts.Node := FNode;
  FAccounts.OnUpdated := SelectedBalanceUpdated;
end;

procedure TWalletExplorerTests.PumpSelectedAccounts;
var LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 4000;
  repeat
    Application.ProcessMessages;
    CheckSynchronize;
    if not FAccounts.IsUpdatingData then Exit;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Fail('Selected-account total did not finish');
end;

procedure TWalletExplorerTests.SelectedTotalsReadMoreThanTheRowCacheOffTheUIThread;
var LIDs: TOrderedCardinalList; LHolder: TSelectedSafeBoxHolder;
  I: Integer; LStarted: QWord;
begin
  NewSelectedAccounts(120);
  LHolder := TSelectedSafeBoxHolder.Create(FNode.Bank.SafeBox);
  try
    AssertTrue(LHolder.Entered.WaitFor(3000) = wrSignaled);
    LIDs := FAccounts.LockAccountsList;
    for I := 0 to 599 do LIDs.Add(I);
    LStarted := GetTickCount64;
    FAccounts.UnlockAccountsList;
    AssertTrue('Adding 600 selected accounts must not wait for a held core lock',
      GetTickCount64 - LStarted < 500);
    AssertEquals(600, FAccounts.AccountsCount);
    AssertTrue(FAccounts.IsUpdatingData);
    AssertEquals('The cache callback is reserved for UI painting', 0, FReadCalls);
    LHolder.ReleaseRead.SetEvent;
    PumpSelectedAccounts;
    AssertEquals('All 120 rewarded accounts must contribute beyond the 512-row cache',
      Int64(1200000), FAccounts.AccountsBalance);
    AssertEquals('Publishing a total must preserve all selected IDs', 600, FAccounts.AccountsCount);
    AssertFalse(FWrongCallbackThread);
    AssertFalse(LHolder.TimedOut);
  finally LHolder.Free; end;
end;

procedure TWalletExplorerTests.SelectedTotalsRetainCompletedValueAndCancelObsoleteRequests;
var LIDs: TOrderedCardinalList; LHolder: TSelectedSafeBoxHolder;
  LWorker: TAccountsGridUpdateThread; LStarted, LDeadline: QWord; I: Integer;
begin
  NewSelectedAccounts(3);
  LIDs := FAccounts.LockAccountsList;
  LIDs.Add(0);
  FAccounts.UnlockAccountsList;
  PumpSelectedAccounts;
  AssertEquals(Int64(10000), FAccounts.AccountsBalance);
  LHolder := TSelectedSafeBoxHolder.Create(FNode.Bank.SafeBox);
  try
    AssertTrue(LHolder.Entered.WaitFor(3000) = wrSignaled);
    LIDs := FAccounts.LockAccountsList;
    LIDs.Add(5);
    FAccounts.UnlockAccountsList;
    // The previous completed worker may still be retiring until the next
    // component timer. Wait for the replacement to own the blocked read.
    LDeadline := GetTickCount64 + 3000;
    repeat
      Application.ProcessMessages;
      LWorker := TAccountsGridUpdateThread(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, nil));
      if Assigned(LWorker) then Break;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    AssertTrue('The replacement total must have an owned worker', Assigned(LWorker));
    LStarted := GetTickCount64;
    for I := 1 to 100 do begin
      LIDs := FAccounts.LockAccountsList;
      LIDs.Add(10);
      FAccounts.UnlockAccountsList;
    end;
    AssertTrue('Replacing pending selected totals must not join the old read',
      GetTickCount64 - LStarted < 500);
    AssertFalse('Only one selected total reader may be active or retiring',
      Assigned(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, LWorker)));
    AssertEquals('Keep the completed total while its replacement is blocked',
      Int64(10000), FAccounts.AccountsBalance);
    LHolder.ReleaseRead.SetEvent;
    PumpSelectedAccounts;
    AssertEquals(Int64(30000), FAccounts.AccountsBalance);
    for I := 0 to FObserved.Count - 1 do
      AssertTrue('Canceled or partial totals must never be published', FObserved[I] <> 20000);
    AssertFalse(FWrongCallbackThread);
    AssertFalse(LHolder.TimedOut);
    LIDs := FAccounts.LockAccountsList;
    LIDs.Clear;
    FAccounts.UnlockAccountsList;
    AssertEquals('An empty selection needs no core query', Int64(0), FAccounts.AccountsBalance);
    PumpSelectedAccounts;
  finally LHolder.Free; end;
end;

procedure TWalletExplorerTests.CapturedKeySelectionDoesNotWaitForIndexAndJoinsCancelledResolver;
var
  LFirst, LLast: TECPrivateKey;
  LKeys: TOrderedAccountKeysList;
  LHolder: TAccountKeyIndexHolder;
  LRelease: TQueuedFilterRelease;
  LFilter: TAccountsGridFilter;
  LWorker: TAccountsGridUpdateThread;
  LStarted, LDeadline: QWord;
  I: Integer;
begin
  NewOperations;
  LFirst := TECPrivateKey.Create;
  LLast := TECPrivateKey.Create;
  LKeys := nil; LHolder := nil; LRelease := nil;
  try
    LFirst.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    LLast.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    TExplorerSafeBoxAccess(FNode.Bank.SafeBox).SeedBlocks(LFirst.PublicKey, 3, 10000);
    TExplorerSafeBoxAccess(FNode.Bank.SafeBox).SeedBlocks(LLast.PublicKey, 1, 10000);
    LKeys := TOrderedAccountKeysList.Create(FNode.Bank.SafeBox, False);
    LKeys.AddAccountKeys([LFirst.PublicKey, LLast.PublicKey]);
    FAccounts := TExplorerAccountsGrid.Create(nil);
    FAccounts.Node := nil;
    FAccounts.AccountsGridDatasource := acds_NodeFiltered;
    LFilter := CT_TAccountsGridFilter_NUL;
    LFilter.OrderedAccountsKeyList := LKeys;
    LFilter.MinBalance := 0;
    LFilter.UseAccountKey := True;
    LFilter.AccountKey := LFirst.PublicKey;
    FAccounts.AccountsGridFilter := LFilter;
    LHolder := TAccountKeyIndexHolder.Create(LKeys);
    AssertTrue(LHolder.Entered.WaitFor(3000) = wrSignaled);
    LStarted := GetTickCount64;
    FAccounts.Node := FNode;
    for I := 1 to 100 do begin
      if Odd(I) then LFilter.AccountKey := LFirst.PublicKey.GetCopy
      else LFilter.AccountKey := LLast.PublicKey.GetCopy;
      FAccounts.AccountsGridFilter := LFilter;
    end;
    AssertTrue('Captured key changes must return while another thread owns the index',
      GetTickCount64 - LStarted < 500);
    AssertTrue(FAccounts.IsUpdatingData);
    // The pending request owns its bytes, rather than borrowing mutable arrays.
    LFilter.AccountKey.x[0] := LFilter.AccountKey.x[0] xor $FF;
    LHolder.ReleaseRead.SetEvent;
    PumpSelectedAccounts;
    AssertEquals('Only the latest selected owner may be published', 5, FAccounts.AccountsCount);
    AssertEquals(Int64(15), FAccounts.AccountNumber(1));
    AssertEquals(Int64(10000), FAccounts.AccountsBalance);
    AssertFalse(LHolder.TimedOut);
    FreeAndNil(LHolder);

    LHolder := TAccountKeyIndexHolder.Create(LKeys);
    AssertTrue(LHolder.Entered.WaitFor(3000) = wrSignaled);
    LFilter.AccountKey := LFirst.PublicKey;
    FAccounts.AccountsGridFilter := LFilter;
    LDeadline := GetTickCount64 + 3000;
    repeat
      Application.ProcessMessages;
      LWorker := TAccountsGridUpdateThread(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, nil));
      if Assigned(LWorker) and LWorker.IsProcessing then Break;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    AssertTrue('The captured-key resolver must start on the owned worker',
      Assigned(LWorker) and LWorker.IsProcessing);
    LFilter.AccountKey := LLast.PublicKey;
    FAccounts.AccountsGridFilter := LFilter;
    AssertFalse('A blocked resolver may have only one coalesced replacement',
      Assigned(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, LWorker)));
    FIndexRelease := LHolder.ReleaseRead;
    LRelease := TQueuedFilterRelease.Create(ReleaseIndex);
    AssertTrue(LRelease.Entered.WaitFor(3000) = wrSignaled);
    FAccounts.Node := nil;
    AssertTrue('Node handoff must join the canceled resolver and dispatch its release',
      FReleasedFilterOnMain);
    AssertFalse(Assigned(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, nil)));
    AssertFalse(FAccounts.IsUpdatingData);
  finally
    if Assigned(LHolder) then LHolder.ReleaseRead.SetEvent;
    LRelease.Free;
    FIndexRelease := nil;
    if Assigned(FAccounts) then FAccounts.Node := nil;
    LHolder.Free;
    LKeys.Free;
    LLast.Free;
    LFirst.Free;
  end;
end;

procedure TWalletExplorerTests.CapturedMissingKeyIsEmptyAndLegacyAllKeysRemainAvailable;
var LMissing: TECPrivateKey; LFilter: TAccountsGridFilter;
  LKeys: TOrderedAccountKeysList;
begin
  NewSelectedAccounts(1);
  FAccounts.Node := nil;
  LMissing := TECPrivateKey.Create;
  LKeys := TOrderedAccountKeysList.Create(FNode.Bank.SafeBox, True);
  try
    LMissing.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    LFilter := CT_TAccountsGridFilter_NUL;
    LFilter.OrderedAccountsKeyList := LKeys;
    LFilter.MinBalance := 0;
    LFilter.UseAccountKey := True;
    LFilter.AccountKey := LMissing.PublicKey;
    FAccounts.AccountsGridDatasource := acds_NodeFiltered;
    FAccounts.AccountsGridFilter := LFilter;
    FAccounts.Node := FNode;
    PumpSelectedAccounts;
    AssertEquals('A missing captured key must not turn into the all-keys sentinel', 0, FAccounts.AccountsCount);
    AssertEquals(Int64(0), FAccounts.AccountsBalance);
    LFilter.UseAccountKey := False;
    LFilter.indexAccountsKeyList := -1;
    FAccounts.AccountsGridFilter := LFilter;
    PumpSelectedAccounts;
    AssertEquals('Existing negative-index callers still enumerate all keys', 5, FAccounts.AccountsCount);
    AssertEquals(Int64(10000), FAccounts.AccountsBalance);
    LFilter.indexAccountsKeyList := 0;
    FAccounts.AccountsGridFilter := LFilter;
    PumpSelectedAccounts;
    AssertEquals('Existing indexed callers retain their selected owner', 5, FAccounts.AccountsCount);
  finally
    FAccounts.Node := nil;
    LKeys.Free;
    LMissing.Free;
  end;
end;

procedure TWalletExplorerTests.CapturedKeyCancellationStopsInsideTheRecordChunk;
var LKey: TECPrivateKey; LFilter: TAccountsGridFilter;
  LWorker: TAccountsGridUpdateThread; LDeadline: QWord;
  LKeys: TOrderedAccountKeysList;
begin
  NewOperations;
  LKey := TECPrivateKey.Create;
  LKeys := nil;
  try
    LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    TExplorerSafeBoxAccess(FNode.Bank.SafeBox).SeedBlocks(LKey.PublicKey, 120, 10000);
    LKeys := TOrderedAccountKeysList.Create(FNode.Bank.SafeBox, False);
    LKeys.AddAccountKey(LKey.PublicKey);
    FFilterEntered := TEvent.Create(nil, True, False, '');
    FFilterRelease := TEvent.Create(nil, True, False, '');
    FFilterExited := TEvent.Create(nil, True, False, '');
    FAccounts := TExplorerAccountsGrid.Create(nil);
    FAccounts.Node := nil;
    FAccounts.AccountsGridDatasource := acds_NodeFiltered;
    LFilter := CT_TAccountsGridFilter_NUL;
    LFilter.OrderedAccountsKeyList := LKeys;
    LFilter.MinBalance := 0;
    LFilter.UseAccountKey := True;
    LFilter.AccountKey := LKey.PublicKey;
    FAccounts.AccountsGridFilter := LFilter;
    FNode.Bank.SafeBox.StartThreadSafe;
    try
      FAccounts.Node := FNode;
      LDeadline := GetTickCount64 + 3000;
      repeat
        LWorker := TAccountsGridUpdateThread(TPCThread.GetThreadByClass(TAccountsGridUpdateThread, nil));
        if Assigned(LWorker) then Break;
        Sleep(1);
      until GetTickCount64 >= LDeadline;
      AssertTrue('The key-filter worker must start', Assigned(LWorker));
      LWorker.FProcessedList.OnListChanged := FilterProgress;
    finally FNode.Bank.SafeBox.EndThreadSave; end;
    AssertTrue(FFilterEntered.WaitFor(3000) = wrSignaled);
    LFilter.AccountKey := CT_TECDSA_Public_Nul;
    FAccounts.AccountsGridFilter := LFilter;
    FFilterRelease.SetEvent;
    PumpSelectedAccounts;
    AssertEquals('Cancellation must stop after the current record, inside its chunk',
      1, InterlockedCompareExchange(FProcessedAdds, 0, 0));
    AssertEquals('The canceled partial owner list must never be published', 0, FAccounts.AccountsCount);
  finally
    if Assigned(FFilterRelease) then FFilterRelease.SetEvent;
    if Assigned(FAccounts) then FAccounts.Node := nil;
    LKeys.Free;
    LKey.Free;
  end;
end;

initialization
  RegisterTest(TWalletExplorerTests);
end.
