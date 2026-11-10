{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Optional large-data variant of the same real main/provider/transport checks.
  Include in UWalletBufferedBlockChecks only for its dedicated large probe;
  UDataSources and UCommon.Data must be in that unit's implementation uses. }

const
  CLargeInitialHeight = 841529;
  CLargeFinalHeight = 841534;
  CLargePendingCount = 185831;
  CLargeMinimumPendingHistory = 1000;
  CLargePendingSHA256 = '53CBE8BB9E9CB01F24CA6799B715BE68101D953F8818A8235FE600E0BBA056F5';
  CLargePendingOrderedHash = 'D06C9B3339B4A91647DACE47D09027A4BCD9BC03E6989414687675C5243A2938';
  CLargeBlockFixtureSHA256 = 'BA574C8A608115157FD8B89D8F8843D723D9B9265554C5BBB7EB11B50F5E6FDC';

type
  TLargePublicOwnerSelection = class(TPCThread)
  private
    FReady: TEvent;
    FStarted: Boolean;
    FKeys: TArray<TAccountKey>;
    FCounts: TArray<Integer>;
    FTotalOwned, FSampled: Integer;
    FError: String;
  protected
    procedure BCExecute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartSelection;
    property Ready: TEvent read FReady;
    property Keys: TArray<TAccountKey> read FKeys;
    property Counts: TArray<Integer> read FCounts;
    property TotalOwned: Integer read FTotalOwned;
    property Sampled: Integer read FSampled;
    property Error: String read FError;
  end;

  TLargeCoreAccountsSource = class(TMyAccountsDataSource)
  public
    Requests, CompletedRequests, LastTotal, LastRequestedPage: LongInt;
    LastFirstAccount: Cardinal;
    LastError: String;
  protected
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
      var ADataTable: TDataTable): TPageFetchResult; override;
  end;

  TLargeCoreOperationsSource = class(TAccountsOperationsDataSource)
  public
    Requests, CompletedRequests, LastTotal, LastRequestedPage: LongInt;
    LastFirstOperationHash: String;
    LastPendingCount: Integer;
    LastPendingAtTip: Cardinal;
    LastError: String;
  protected
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
      var ADataTable: TDataTable): TPageFetchResult; override;
  end;

  TLargeCoreWallet = class(TCTRLWallet)
  private
    FObservedAccounts: TLargeCoreAccountsSource;
    FObservedOperations: TLargeCoreOperationsSource;
  protected
    function CreateAccountsDataSource: TMyAccountsDataSource; override;
    function CreateOperationsDataSource: TAccountsOperationsDataSource; override;
  public
    property AccountsGrid;
    property OperationsGrid;
    property ObservedAccounts: TLargeCoreAccountsSource read FObservedAccounts;
    property ObservedOperations: TLargeCoreOperationsSource read FObservedOperations;
  end;

  TLargeCoreMain = class(TFRMMainForm)
  protected
    function CreateWalletControl: TCTRLWallet; override;
  end;

  TLargeBufferedBlockChecks = class(TBufferedBlockChecks)
  private
    FHeaders: array[0..CHistoricCount-1] of TOperationBlock;
    FPausedAccounts, FPausedOperations: Integer;
    FPausedCompletedAccounts, FPausedCompletedOperations: Integer;
    FInitialFirstAccount: Cardinal;
    FLargestDispatch: UInt64;
    function LargeWallet: TLargeCoreWallet;
    procedure LogViewState(const APhase: String);
    procedure PumpResponsive;
    procedure WaitLargeCommit(AExpected: Integer);
    procedure AwaitInitialPages;
    procedure AwaitSparsePages;
    procedure CheckPending(ANode: TNode; const APhase: String);
    procedure CheckLargeCommit(ACommitted: Integer);
    procedure CheckFinalPages;
    procedure CheckLargeStorage;
  public
    procedure RunLarge;
  end;

function LargeProviderException(E: Exception): String;
{$IFDEF FPC}var I: Integer;{$ENDIF}
begin
  Result := E.ClassName+': '+E.Message;
  {$IFDEF FPC}
  if Assigned(BackTraceStrFunc) then begin
    Result := Result+LineEnding+BackTraceStrFunc(ExceptAddr);
    for I := 0 to ExceptFrameCount-1 do
      Result := Result+LineEnding+BackTraceStrFunc(ExceptFrames[I]);
  end;
  {$ENDIF}
end;

constructor TLargePublicOwnerSelection.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FReady := TEvent.Create(nil,True,False,'');
end;

destructor TLargePublicOwnerSelection.Destroy;
begin
  Terminate;
  if FStarted then WaitFor;
  FReady.Free;
  inherited;
end;

procedure TLargePublicOwnerSelection.StartSelection;
begin
  Start;
  FStarted := True;
end;

procedure TLargePublicOwnerSelection.BCExecute;
var
  LNode: TNode;
  LPool: TPCOperationsComp;
  LSafeBox: TPCSafeBox;
  LKeys: TList<TAccountKey>;
  LCounts: TList<Integer>;
  LAffected: TOrderedList<Cardinal>;
  LOperation: TPCOperation;
  LScope: TDisposables;
  I, J, LPosition: Integer;

  procedure AddOwner(AAccount: Cardinal; AMinimumCount, AMaximumCount: Integer);
  var LAccount: TAccount; LKey: TAccountKey; LOwned, K: Integer;
  begin
    if Terminated then raise EAbort.Create('Public-owner selection cancelled');
    LAccount := LSafeBox.Account(AAccount);
    LKey := LAccount.accountInfo.accountKey;
    LOwned := LSafeBox.OrderedAccountKeysList.GetAccountsUsingThisKey(LKey).Count;
    Inc(FSampled);
    if (LOwned < AMinimumCount) or (LOwned > AMaximumCount) then Exit;
    for K := 0 to LKeys.Count-1 do
      if TAccountComp.EqualAccountKeys(LKeys[K],LKey) then Exit;
    LKeys.Add(LKey);
    LCounts.Add(LOwned);
    Inc(FTotalOwned,LOwned);
  end;
begin
  try
    try
      LKeys := LScope.AddObject(TList<TAccountKey>.Create) as TList<TAccountKey>;
      LCounts := LScope.AddObject(TList<Integer>.Create) as TList<Integer>;
      LAffected := LScope.AddObject(TOrderedList<Cardinal>.Create(False,TComparison_Cardinal)) as TOrderedList<Cardinal>;
      LNode := TNode.Node;
      LPool := LNode.LockMempoolRead;
      try
        LSafeBox := LNode.Bank.SafeBox;
        LSafeBox.StartThreadSafe;
        try
          Require(LPool.Count = CLargePendingCount,'Wrong pool for actual public-owner selection');
          // Reuse the proven dense fixture's first/middle/last pending signer
          // owners, then affected accounts and a bounded public-chain fallback.
          for I := 0 to 2 do begin
            LPosition := (Int64(LPool.Count-1)*I) div 2;
            LOperation := LPool.OperationsHashTree.GetOperation(LPosition);
            AddOwner(LOperation.SignerAccount,1,500000);
          end;
          if FTotalOwned < 326387 then
            for I := 0 to 2 do begin
              LPosition := (Int64(LPool.Count-1)*I) div 2;
              LOperation := LPool.OperationsHashTree.GetOperation(LPosition);
              LAffected.Clear;
              LOperation.AffectedAccounts(LAffected);
              for J := 0 to LAffected.Count-1 do AddOwner(LAffected[J],1,500000);
            end;
          if FTotalOwned < 326387 then
            for I := 0 to 2047 do begin
              AddOwner((Int64(LSafeBox.AccountsCount-1)*I) div 2047,12000,500000);
              if FTotalOwned >= 326387 then Break;
            end;
          Require(FTotalOwned >= 326387,'The real public owners do not cover326387 accounts');
          FKeys := LKeys.ToArray;
          FCounts := LCounts.ToArray;
        finally LSafeBox.EndThreadSave; end;
      finally LNode.UnlockMempoolRead; end;
    except
      on E: Exception do FError := LargeProviderException(E);
    end;
  finally FReady.SetEvent; end;
end;

function TLargeCoreAccountsSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ADataTable: TDataTable): TPageFetchResult;
begin
  InterlockedIncrement(Requests);
  LastError := '';
  try
    Result := inherited FetchPage(AParams,ADataTable);
  except
    on E: Exception do begin
      LastError := LargeProviderException(E);
      raise;
    end;
  end;
  LastTotal := Result.TotalDataCount;
  LastRequestedPage := AParams.PageIndex;
  if Length(ADataTable.Rows) > 0 then LastFirstAccount := ADataTable.Rows[0].AccountNumber;
  InterlockedIncrement(CompletedRequests);
end;

function TLargeCoreOperationsSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ADataTable: TDataTable): TPageFetchResult;
begin
  InterlockedIncrement(Requests);
  LastError := '';
  try
    Result := inherited FetchPage(AParams,ADataTable);
  except
    on E: Exception do begin
      LastError := LargeProviderException(E);
      raise;
    end;
  end;
  LastTotal := Result.TotalDataCount;
  LastRequestedPage := AParams.PageIndex;
  LastFirstOperationHash := '';
  if Length(ADataTable.Rows) > 0 then LastFirstOperationHash := ADataTable.Rows[0].OPHASH;
  InterlockedIncrement(CompletedRequests);
end;

procedure TLargeCoreOperationsSource.FetchAll(const AContainer: TList<TOperationResume>);
var LTipBefore, LTipAfter: Cardinal; I, LPendingCount: Integer;
begin
  LTipBefore := TNode.Node.GetSynchronizationStatus.LocalBlockCount;
  inherited FetchAll(AContainer);
  LTipAfter := TNode.Node.GetSynchronizationStatus.LocalBlockCount;
  LastPendingAtTip := High(Cardinal);
  LastPendingCount := 0;
  if LTipBefore <> LTipAfter then Exit;
  LPendingCount := 0;
  // The real pending adapter stamps the next block number; stored operations
  // are below it. Count only an unchanged-tip inherited result, outside locks.
  for I := 0 to AContainer.Count-1 do
    if AContainer[I].Block = LTipAfter then Inc(LPendingCount);
  LastPendingCount := LPendingCount;
  LastPendingAtTip := LTipAfter;
end;

function TLargeCoreWallet.CreateAccountsDataSource: TMyAccountsDataSource;
begin
  FObservedAccounts := TLargeCoreAccountsSource.Create(Self);
  Result := FObservedAccounts;
end;

function TLargeCoreWallet.CreateOperationsDataSource: TAccountsOperationsDataSource;
begin
  FObservedOperations := TLargeCoreOperationsSource.Create(Self);
  Result := FObservedOperations;
end;

function TLargeCoreMain.CreateWalletControl: TCTRLWallet;
begin
  Result := TLargeCoreWallet.Create(Self);
end;

function TLargeBufferedBlockChecks.LargeWallet: TLargeCoreWallet;
begin
  Result := TLargeCoreWallet(TMainAccess(FMain).WalletControl);
end;

procedure TLargeBufferedBlockChecks.LogViewState(const APhase: String);
var LStatus: TNodeSynchronizationStatus;
begin
  Log(Format('LARGE VIEW %s mode=%d mainVisible=%s walletVisible=%s accountsMode=%d parents=%s/%s rows=%d/%d requests=%d/%d completed=%d/%d updating=%s/%s',
    [APhase,Ord(FMain.Mode),BoolToStr(FMain.Visible,True),BoolToStr(LargeWallet.Visible,True),
     Ord(LargeWallet.AccountsMode),BoolToStr(Assigned(LargeWallet.AccountsGrid.Parent),True),
     BoolToStr(Assigned(LargeWallet.OperationsGrid.Parent),True),
     LargeWallet.AccountsGrid.RowCount,LargeWallet.OperationsGrid.RowCount,
     LargeWallet.ObservedAccounts.Requests,LargeWallet.ObservedOperations.Requests,
     LargeWallet.ObservedAccounts.CompletedRequests,LargeWallet.ObservedOperations.CompletedRequests,
     BoolToStr(LargeWallet.AccountsGrid.IsUpdating,True),BoolToStr(LargeWallet.OperationsGrid.IsUpdating,True)]));
  LStatus := TUserInterface.Node.GetSynchronizationStatus;
  Log(Format('LARGE ADMISSION %s state=%d transfers=%d waiting=%s/%s suspended=%s/%s total=%d/%d errors=[%s]/[%s]',
    [APhase,Ord(LStatus.State),LStatus.ActiveTransfers,
     BoolToStr(LargeWallet.AccountsGrid.RefreshThrottle.Waiting,True),
     BoolToStr(LargeWallet.OperationsGrid.RefreshThrottle.Waiting,True),
     BoolToStr(LargeWallet.AccountsGrid.RefreshSuspended,True),
     BoolToStr(LargeWallet.OperationsGrid.RefreshSuspended,True),
     LargeWallet.ObservedAccounts.LastTotal,LargeWallet.ObservedOperations.LastTotal,
     LargeWallet.ObservedAccounts.LastError,LargeWallet.ObservedOperations.LastError]));
end;

procedure TLargeBufferedBlockChecks.PumpResponsive;
var LStarted, LElapsed: UInt64;
begin
  LStarted := GetTickCount64;
  Pump;
  LElapsed := GetTickCount64-LStarted;
  if LElapsed > FLargestDispatch then FLargestDispatch := LElapsed;
  if LElapsed >= 400 then Log('LARGE main dispatch='+UIntToStr(LElapsed)+'ms');
  Require(LElapsed < 400,'The actual large-data main dispatch blocked for >=400ms');
end;

procedure TLargeBufferedBlockChecks.WaitLargeCommit(AExpected: Integer);
var LDeadline: UInt64;
begin
  LDeadline := GetTickCount64+30000;
  repeat
    PumpResponsive;
    if (InterlockedCompareExchange(FObservation.CommitCount,0,0) = AExpected) and
      (FObservation.Committed.WaitFor(0) = wrSignaled) then Exit;
    if InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) <> 0 then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  raise Exception.CreateFmt('The actual large processor did not commit block%d',
    [CLargeInitialHeight+AExpected-1]);
end;

procedure TLargeBufferedBlockChecks.AwaitInitialPages;
var LDeadline: UInt64;
begin
  LDeadline := GetTickCount64+120000;
  repeat
    PumpResponsive;
    if not LargeWallet.AccountsGrid.IsUpdating and not LargeWallet.OperationsGrid.IsUpdating and
      ((LargeWallet.ObservedAccounts.LastError <> '') or (LargeWallet.ObservedOperations.LastError <> '')) then begin
      LogViewState('inherited provider exception');
      raise Exception.Create('Actual provider error: '+LargeWallet.ObservedAccounts.LastError+' / '+
        LargeWallet.ObservedOperations.LastError);
    end;
    if not LargeWallet.AccountsGrid.IsUpdating and not LargeWallet.OperationsGrid.IsUpdating and
      not LargeWallet.AccountsGrid.RefreshThrottle.Waiting and
      not LargeWallet.OperationsGrid.RefreshThrottle.Waiting and
      (LargeWallet.AccountsGrid.RowCount > 0) and (LargeWallet.OperationsGrid.RowCount > 0) then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  LogViewState('initial page await finished');
  Require((LargeWallet.AccountsGrid.RowCount > 0) and (LargeWallet.OperationsGrid.RowCount > 0) and
    not LargeWallet.AccountsGrid.IsUpdating and not LargeWallet.OperationsGrid.IsUpdating and
    not LargeWallet.AccountsGrid.RefreshThrottle.Waiting and
    not LargeWallet.OperationsGrid.RefreshThrottle.Waiting,
    'The actual large-data providers did not populate both initial main pages');
  Require(LargeWallet.ObservedAccounts.LastTotal >= 326387,
    'The sampled public owners must exercise the known large ownership set');
  Require((LargeWallet.ObservedOperations.LastPendingAtTip = CLargeInitialHeight) and
    (LargeWallet.ObservedOperations.LastPendingCount >= CLargeMinimumPendingHistory),
    'The actual history provider must materialize substantial pending history at the frozen tip');
  Require(LargeWallet.OperationsMode = womAllAccounts,
    'The large history fixture must retain actual all-wallet membership');
  Require(FMain.Visible and (FMain.Mode = wmWallet) and LargeWallet.Visible and
    Assigned(LargeWallet.AccountsGrid.Parent) and Assigned(LargeWallet.OperationsGrid.Parent),
    'The large checks must exercise the visible real main wallet and its parented grids');
  FInitialFirstAccount := LargeWallet.AccountsGrid.Rows[0].AccountNumber;
  Log(Format('LARGE initial real accounts=%d history=%d pending-history=%d rows=%d/%d fetches=%d/%d',
    [LargeWallet.ObservedAccounts.LastTotal,LargeWallet.ObservedOperations.LastTotal,
     LargeWallet.ObservedOperations.LastPendingCount,
     LargeWallet.AccountsGrid.RowCount,LargeWallet.OperationsGrid.RowCount,
     LargeWallet.ObservedAccounts.Requests,LargeWallet.ObservedOperations.Requests]));
end;

procedure TLargeBufferedBlockChecks.AwaitSparsePages;
var LDeadline: UInt64; LPool: TPCOperationsComp; LAccount: TAccount;
begin
  LDeadline := GetTickCount64+120000;
  repeat
    PumpResponsive;
    if not LargeWallet.AccountsGrid.IsUpdating and not LargeWallet.OperationsGrid.IsUpdating and
      ((LargeWallet.ObservedAccounts.LastError <> '') or (LargeWallet.ObservedOperations.LastError <> '')) then begin
      LogViewState('sparse inherited provider exception');
      raise Exception.Create('Actual sparse provider error: '+LargeWallet.ObservedAccounts.LastError+' / '+
        LargeWallet.ObservedOperations.LastError);
    end;
    if not LargeWallet.AccountsGrid.IsUpdating and not LargeWallet.OperationsGrid.IsUpdating and
      not LargeWallet.AccountsGrid.RefreshThrottle.Waiting and not LargeWallet.OperationsGrid.RefreshThrottle.Waiting and
      (LargeWallet.ObservedAccounts.Requests >= 2) and (LargeWallet.ObservedOperations.Requests >= 2) and
      (LargeWallet.ObservedAccounts.CompletedRequests = LargeWallet.ObservedAccounts.Requests) and
      (LargeWallet.ObservedOperations.CompletedRequests = LargeWallet.ObservedOperations.Requests) then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  LogViewState('sparse page await finished');
  Require(not LargeWallet.AccountsGrid.IsUpdating and not LargeWallet.OperationsGrid.IsUpdating and
    not LargeWallet.AccountsGrid.RefreshThrottle.Waiting and not LargeWallet.OperationsGrid.RefreshThrottle.Waiting and
    (LargeWallet.ObservedAccounts.Requests >= 2) and (LargeWallet.ObservedOperations.Requests >= 2) and
    (LargeWallet.ObservedAccounts.CompletedRequests = LargeWallet.ObservedAccounts.Requests) and
    (LargeWallet.ObservedOperations.CompletedRequests = LargeWallet.ObservedOperations.Requests),
    'The sparse actual providers did not complete admitted queries');
  Require((LargeWallet.AccountsGrid.RowCount = 1) and (LargeWallet.ObservedAccounts.LastTotal = 1) and
    (LargeWallet.ObservedAccounts.LastFirstAccount = 111),'The sparse account result is not the real account111');
  LPool := TUserInterface.Node.LockMempoolRead;
  try LAccount := LPool.SafeBoxTransaction.Account(111);
  finally TUserInterface.Node.UnlockMempoolRead; end;
  Require(LAccount.balance > High(Cardinal),'The sparse oracle must exercise the actual wide balance');
  Require((UInt64(LargeWallet.AccountsGrid.Rows[0].Balance) = LAccount.balance) and
    (Cardinal(LargeWallet.AccountsGrid.Rows[0].NOperation) = LAccount.n_operation) and
    (Cardinal(LargeWallet.AccountsGrid.Rows[0].LastUpdatedBlock) = LAccount.GetLastUpdatedBlock),
    'The sparse page narrowed the real balance or changed account metadata');
  Log(Format('SPARSE real account111 balance=%s history=%d successful fetches=%d/%d',
    [UIntToStr(LAccount.balance),LargeWallet.ObservedOperations.LastTotal,
     LargeWallet.ObservedAccounts.CompletedRequests,LargeWallet.ObservedOperations.CompletedRequests]));
end;

procedure TLargeBufferedBlockChecks.CheckPending(ANode: TNode; const APhase: String);
var LPool: TPCOperationsComp; LStream: TMemoryStream; LHash: String;
begin
  LStream := TMemoryStream.Create;
  try
    LPool := ANode.LockMempoolRead;
    try
      Require(LPool.Count = CLargePendingCount,APhase+': actual pending count changed');
      Require(SameText(TCrypto.ToHexaString(LPool.OperationsHashTree.HashTree),CLargePendingOrderedHash),
        APhase+': actual pending operation order changed');
      Require(LPool.OperationsHashTree.SaveOperationsHashTreeToStream(LStream,True),
        APhase+': actual pending serialization failed');
    finally ANode.UnlockMempoolRead; end;
    LHash := TCrypto.ToHexaString(TCrypto.DoSha256(PAnsiChar(LStream.Memory),LStream.Size));
    Require(SameText(LHash,CLargePendingSHA256),APhase+': serialized pending bytes changed');
    Log(APhase+' pending='+IntToStr(CLargePendingCount)+' SHA256='+LHash);
  finally LStream.Free; end;
end;

procedure TLargeBufferedBlockChecks.CheckLargeCommit(ACommitted: Integer);
var LDeadline: UInt64; LHeight: Cardinal; LStatus, LDisplayed: TNodeSynchronizationStatus;
  LFooter, LTime: String;
begin
  LHeight := CLargeInitialHeight+Cardinal(ACommitted);
  LDeadline := GetTickCount64+3000;
  repeat
    PumpResponsive;
    LStatus := TUserInterface.Node.GetSynchronizationStatus;
    LDisplayed := FMain.SyncControl.DisplayedSynchronizationStatus;
    if (LDisplayed.LocalBlockCount = LHeight) and
      (LDisplayed.State = nssSynchronizing) and not LDisplayed.NetworkActive and
      LargeWallet.AccountsGrid.RefreshSuspended and LargeWallet.OperationsGrid.RefreshSuspended then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require((LStatus.LocalBlockCount = LHeight) and (LStatus.State = nssSynchronizing) and
    (LStatus.ActiveTransfers > 0) and not LStatus.NetworkActive and
    (LStatus.PendingOperationsCount = CLargePendingCount) and
    (LStatus.LastBlockTimestamp = FHeaders[ACommitted-1].timestamp),
    'The cached real node status must describe this exact complete large-data commit');
  Require((LDisplayed.LocalBlockCount = LStatus.LocalBlockCount) and
    (LDisplayed.LastBlockTimestamp = LStatus.LastBlockTimestamp) and
    (LDisplayed.State = LStatus.State),
    'The actual large sync view rendered stale committed core metadata');
  Require(FMain.SyncControl.lblTotalBlocksValue.Caption =
    IntToStr(LHeight)+' (0..'+IntToStr(LHeight-1)+')',
    'The actual large sync block label disagrees with its cached record');
  LFooter := FMain.sbStatusBar.Panels[2].Text;
  LTime := FormatDateTime('dd/mm/yyyy hh:nn:ss',
    UnivDateTime2LocalDateTime(UnixToUnivDateTime(LStatus.LastBlockTimestamp)));
  Require((LFooter = FMain.SyncControl.ChainStatusText) and
    (Pos('Blocks: '+IntToStr(LHeight)+' |',LFooter) = 1) and
    (Pos(LTime,LFooter) > 0) and (Pos('Network off',LFooter) > 0),
    'The actual large footer must use the same current count/time/off record');
  Require(LargeWallet.AccountsGrid.RefreshSuspended and LargeWallet.OperationsGrid.RefreshSuspended and
    LargeWallet.AccountsGrid.LoadingVisible and LargeWallet.OperationsGrid.LoadingVisible and
    (FMain.Mode = wmWallet) and LargeWallet.Visible,
    'Both actual large main grids must remain paused/covered throughout received processing');
  Require(GetActiveWindow = FDialog.Handle,'A large commit stole modeless focus');
  if ACommitted = 1 then begin
    FPausedAccounts := LargeWallet.ObservedAccounts.Requests;
    FPausedOperations := LargeWallet.ObservedOperations.Requests;
    FPausedCompletedAccounts := LargeWallet.ObservedAccounts.CompletedRequests;
    FPausedCompletedOperations := LargeWallet.ObservedOperations.CompletedRequests;
    // These are real client refresh requests. Their latest parameters must be
    // retained without provider execution while the node owns its backlog.
    LargeWallet.AccountsGrid.PageIndex := 2;
    LargeWallet.AccountsGrid.PageIndex := 1;
    LargeWallet.OperationsGrid.PageIndex := 1;
    LargeWallet.OperationsGrid.PageIndex := 0;
  end;
  Require((LargeWallet.ObservedAccounts.Requests = FPausedAccounts) and
    (LargeWallet.ObservedOperations.Requests = FPausedOperations),
    'An actual large provider query started while physical backlog work remained');
  Log('LARGE COMMITTED '+IntToStr(LHeight)+' '+LFooter);
end;

procedure TLargeBufferedBlockChecks.CheckFinalPages;
var LDeadline: UInt64;
begin
  LDeadline := GetTickCount64+120000;
  repeat
    PumpResponsive;
    if not LargeWallet.AccountsGrid.IsUpdating and not LargeWallet.OperationsGrid.IsUpdating and
      not LargeWallet.AccountsGrid.RefreshSuspended and not LargeWallet.OperationsGrid.RefreshSuspended and
      not LargeWallet.AccountsGrid.LoadingVisible and not LargeWallet.OperationsGrid.LoadingVisible and
      (LargeWallet.ObservedAccounts.Requests > FPausedAccounts) and
      (LargeWallet.ObservedOperations.Requests > FPausedOperations) then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require(TUserInterface.Node.GetSynchronizationStatus.State = nssOffline,
    'The large local wallet must become available when received processing drains');
  Require(not LargeWallet.AccountsGrid.RefreshSuspended and not LargeWallet.OperationsGrid.RefreshSuspended and
    not LargeWallet.AccountsGrid.LoadingVisible and not LargeWallet.OperationsGrid.LoadingVisible and
    not LargeWallet.AccountsGrid.IsUpdating and not LargeWallet.OperationsGrid.IsUpdating,
    'The actual large main pages did not finish after backlog drain');
  Require((LargeWallet.ObservedAccounts.Requests = FPausedAccounts+1) and
    (LargeWallet.ObservedOperations.Requests = FPausedOperations+1),
    'Backlog drain must execute one retained latest real query per grid');
  Require((LargeWallet.ObservedAccounts.CompletedRequests = FPausedCompletedAccounts+1) and
    (LargeWallet.ObservedOperations.CompletedRequests = FPausedCompletedOperations+1),
    'Both final real provider queries must complete successfully, not leave retained rows after failure');
  Require((LargeWallet.ObservedAccounts.LastRequestedPage = 1) and
    (LargeWallet.ObservedOperations.LastRequestedPage = 0),
    'Backlog drain did not execute the latest real page parameters');
  Require((LargeWallet.AccountsGrid.RowCount > 0) and (LargeWallet.OperationsGrid.RowCount > 0),
    'The resumed actual large providers returned no visible rows');
  Require((LargeWallet.ObservedOperations.LastPendingAtTip = CLargeFinalHeight) and
    (LargeWallet.ObservedOperations.LastPendingCount >= CLargeMinimumPendingHistory),
    'The final real history query did not invalidate its raw snapshot and materialize current pending rows');
  Require((Cardinal(LargeWallet.AccountsGrid.Rows[0].AccountNumber) =
      LargeWallet.ObservedAccounts.LastFirstAccount) and
    (LargeWallet.ObservedAccounts.LastFirstAccount <> FInitialFirstAccount) and
    (String(LargeWallet.OperationsGrid.Rows[0].OPHASH) =
      LargeWallet.ObservedOperations.LastFirstOperationHash),
    'The visible final pages must publish the newly completed real provider rows');
  Log(Format('LARGE latest real pages published fetches=%d/%d maxUIpump=%dms',
    [LargeWallet.ObservedAccounts.Requests,LargeWallet.ObservedOperations.Requests,FLargestDispatch]));
end;

procedure TLargeBufferedBlockChecks.CheckLargeStorage;
var LBank: TPCBank; LStorage: TFileStorage; LBlock: TPCOperationsComp;
  LPending: TOperationsHashTree; LActual, LExpected: TMemoryStream;
  I: Integer; LLength: Int64; LError, LHash: String;
begin
  LBank := nil;
  LBlock := nil;
  LActual := nil;
  LExpected := nil;
  LPending := nil;
  try
    // The pending reader resolves Bank.Orphan even with an explicit chain
    // filename. Keep its real bank owner alive until every reader is freed.
    LBank := TPCBank.Create(nil);
    LBank.StorageClass := TFileStorage;
    LStorage := TFileStorage(LBank.Storage);
    Require(SameText(ExcludeTrailingPathDelimiter(ExpandFileName(TNode.GetPascalCoinDataFolder)),
      ExcludeTrailingPathDelimiter(ExpandFileName(FDataDirectory))),
      'The reopened bank must retain the owned fixture data-directory context');
    LBlock := TPCOperationsComp.Create(nil);
    LActual := TMemoryStream.Create;
    LExpected := TMemoryStream.Create;
    LPending := TOperationsHashTree.Create;
    LStorage.ReadOnly := True;
    LStorage.SetBlockChainFile(AnsiString(IncludeTrailingPathDelimiter(FDataDirectory)+
      'Data'+PathDelim+'BlockChainStream.blocks'));
    Require(LStorage.Initialize,'Cannot reopen the owned large stored chain read-only');
    // Pending decoding uses the owner's actual protocol. Restore that owner
    // through the normal bank path rather than a fresh protocol1 placeholder.
    Log('LARGE BEGIN read-only bank restore');
    LBank.DiskRestoreFromOperations(CLargeFinalHeight-1,nil);
    Log('LARGE END read-only bank restore');
    Require((LBank.BlocksCount = CLargeFinalHeight) and
      (LBank.LastOperationBlock.timestamp = FHeaders[CHistoricCount-1].timestamp),
      'The reopened large bank must retain the exact committed height and timestamp');
    FData.Position := SizeOf(Cardinal);
    for I := 0 to CHistoricCount-1 do begin
      LLength := FData.Position;
      Require(LBlock.LoadBlockFromStream(FData,LError),LError);
      LLength := FData.Position-LLength;
      LExpected.Clear;
      FData.Position := FData.Position-LLength;
      LExpected.CopyFrom(FData,LLength);
      Require(LStorage.LoadBlockChainBlock(LBlock,CLargeInitialHeight+Cardinal(I)),
        'Reopened large chain is missing one received block');
      LActual.Clear;
      Require(LBlock.SaveBlockToStream(False,LActual),'Cannot serialize the reopened received block');
      Require((LActual.Size = LExpected.Size) and CompareMem(LActual.Memory,LExpected.Memory,LActual.Size),
        'A reopened large received block differs from its original public network bytes');
      Log('LARGE reopened exact public block '+IntToStr(CLargeInitialHeight+Cardinal(I)));
    end;
    LStorage.LoadPendingBufferOperations(LPending);
    Require((LPending.OperationsCount = CLargePendingCount) and
      SameText(TCrypto.ToHexaString(LPending.HashTree),CLargePendingOrderedHash),
      'Reopened large pending storage lost operations or changed their order');
    LActual.Clear;
    Require(LPending.SaveOperationsHashTreeToStream(LActual,True),
      'Cannot serialize the reopened large pending package');
    LHash := TCrypto.ToHexaString(TCrypto.DoSha256(PAnsiChar(LActual.Memory),LActual.Size));
    Require(SameText(LHash,CLargePendingSHA256),
      'Reopened large pending storage differs from the original185831-operation package');
  finally LPending.Free; LExpected.Free; LActual.Free; LBlock.Free; LBank.Free; end;
  Log('PASS LARGE five full-PoW/bank-validated blocks and exact185831-operation package reopened');
end;

procedure TLargeBufferedBlockChecks.RunLarge;
var LOptions: TWalletStartupOptions; LParams: TAppParams; LDeadline: UInt64;
  LCount: Cardinal; LBlock: TPCOperationsComp; LSelection: TLargePublicOwnerSelection; LAccount: TAccount;
  LError: String; I: Integer; LSparseDiagnostic: Boolean;
begin
  Require(not TNode.NodeExists,'The large fixture needs its own fresh controller');
  FDataDirectory := ExcludeTrailingPathDelimiter(ExpandFileName(
    GetEnvironmentVariable('PASCALCOIN_LARGE_BACKLOG_ROOT')));
  Require((GetEnvironmentVariable('PASCALCOIN_LARGE_BACKLOG_ROOT') <> '') and
    DirectoryExists(FDataDirectory),'Provide the precreated owned large app root');
  Require(FileExists(FDataDirectory+PathDelim+'LargeBacklog.owned'),
    'The large runtime requires the explicitly leased owned-data marker');
  Require(DirectoryExists(FDataDirectory+PathDelim+'Data'),
    'The large owned app root has no leased Data directory');
  Require((GetFileAttributes(PChar(FDataDirectory+PathDelim+'Data')) and FILE_ATTRIBUTE_REPARSE_POINT) <> 0,
    'The large runtime must reuse its explicitly leased Data junction, not copy the chain');
  Require(not FileExists(FDataDirectory+PathDelim+'WalletKeys.dat') and
    not FileExists(FDataDirectory+PathDelim+'AppParams.prm'),
    'Refusing existing wallet keys or settings in the large app root');
  LParams := TAppParams.Create(nil);
  try
    LParams.FileName := FDataDirectory+PathDelim+'AppParams.prm';
    LParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].SetAsString('127.0.0.1:1');
    LParams.ParamByName[CT_PARAM_PeerCache].SetAsString('');
    LParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(0);
    LParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(False);
    LParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(False);
    LParams.ParamByName[CT_PARAM_MinerPrivateKeyType].SetAsInteger(Ord(mpk_Random));
    LParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(False);
  finally LParams.Free; end;
  LOptions := Default(TWalletStartupOptions);
  LOptions.DataDirectory := FDataDirectory;
  LOptions.NoStartupDialogs := True;
  LOptions.Offline := False;
  TUserInterface.ConfigureStartup(LOptions);
  Application.CreateForm(TLargeCoreMain,FMain);
  TUserInterface.StartApplication(FMain);
  FMain.Show;
  LDeadline := GetTickCount64+180000;
  repeat
    PumpResponsive;
    Require(TUserInterface.State <> uisError,'The owned real large-data startup failed');
    if (LargeWallet <> nil) and (TUserInterface.State <> uisLoading) then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require((LargeWallet <> nil) and (TUserInterface.State <> uisLoading),
    'The real185831-operation startup did not complete');
  Require(TUserInterface.Node.Bank.BlocksCount = CLargeInitialHeight,'Wrong owned large-chain initial tip');
  Require(TWallet.Keys.Count = 0,'The large app must begin with no wallet keys');
  TNetData.NetData.MinServersConnected := 0;
  TUserInterface.Node.BroadcastData := False;
  CheckPending(TUserInterface.Node,'LARGE BEFORE');
  LSparseDiagnostic := GetEnvironmentVariable('PASCALCOIN_LARGE_BACKLOG_SPARSE_DIAGNOSTIC') = '1';
  if LSparseDiagnostic then begin
    // Preserve the sparse-owner RED independently of the dense selection; it
    // exposes inherited provider failures before any buffered block is sent.
    LAccount := TUserInterface.Node.Bank.SafeBox.Account(111);
    TWallet.Keys.AddPublicKey('Public account111 sparse diagnostic',LAccount.accountInfo.accountKey);
    Require((TWallet.Keys.Count = 1) and not Assigned(TWallet.Keys.Key[0].PrivateKey),
      'The sparse diagnostic must contain only its public watch key');
    Log('SPARSE111 diagnostic: require successful inherited queries and exact wide balance; zero history is valid');
  end else begin
  LSelection := TLargePublicOwnerSelection.Create;
  try
    LSelection.StartSelection;
    LDeadline := GetTickCount64+30000;
    repeat PumpResponsive; Sleep(1); until
      (LSelection.Ready.WaitFor(0) = wrSignaled) or (GetTickCount64 >= LDeadline);
    Require(LSelection.Ready.WaitFor(0) = wrSignaled,'Actual public-owner selection timed out');
    LSelection.WaitFor;
    Require(LSelection.Error = '',LSelection.Error);
    Log(Format('LARGE real public owners=%d owned=%d sampled=%d',
      [Length(LSelection.Keys),LSelection.TotalOwned,LSelection.Sampled]));
    for I := 0 to Length(LSelection.Keys)-1 do begin
      Log(Format('LARGE public owner%d accounts=%d',[I,LSelection.Counts[I]]));
      TWallet.Keys.AddPublicKey('Public pending-owner fixture '+IntToStr(I),LSelection.Keys[I]);
    end;
    Require(TWallet.Keys.Count = Length(LSelection.Keys),'Wrong public-only key count');
    for I := 0 to TWallet.Keys.Count-1 do
      Require(not Assigned(TWallet.Keys.Key[I].PrivateKey),'The fixture must never load private keys');
  finally LSelection.Free; end;
  end;
  LogViewState('before public wallet entry');
  // A low-level public-key import is not a user navigation action. Enter the
  // real wallet view and its Show All account selector before awaiting pages.
  TUserInterface.ShowWallet;
  PumpResponsive;
  if LargeWallet.AccountsMode <> wamMyAccounts then begin
    LargeWallet.cbAccounts.ItemIndex := 0;
    LargeWallet.cbAccountsChange(LargeWallet.cbAccounts);
    PumpResponsive;
  end;
  LogViewState('after public wallet entry');
  if LSparseDiagnostic then begin
    AwaitSparsePages;
    CheckPending(TUserInterface.Node,'SPARSE AFTER');
    TUserInterface.ExitApplication;
    FMain := nil;
    FDialog := nil;
    Log('PASS SPARSE successful actual queries, exact64-bit balance and natural controller shutdown');
    Exit;
  end;
  AwaitInitialPages;
  LDeadline := GetTickCount64+3000;
  repeat PumpResponsive; Sleep(1); until TNetData.NetData.NetConnectionsActive or (GetTickCount64 >= LDeadline);
  Require(TNetData.NetData.NetConnectionsActive,'Owned loopback-only networking did not enable');
  // This scenario owns only an outgoing loopback client. Match the small
  // outgoing setup and retire the unused listener before receiving its batch.
  TUserInterface.Node.NetServer.Active := False;
  FData := TResourceStream.Create(HInstance,'LARGE_BUFFERED_BLOCKS',RT_RCDATA);
  Require(FData.Size = 4465,'The large embedded response must contain only count+five provenance-checked public blocks');
  Require(SameText(TCrypto.ToHexaString(TCrypto.DoSha256(PAnsiChar(FData.Memory),FData.Size)),
    CLargeBlockFixtureSHA256),'The embedded large public block fixture failed its provenance hash');
  FData.ReadBuffer(LCount,SizeOf(LCount));
  Require(LCount = CHistoricCount,'Wrong large historic response count');
  LBlock := TPCOperationsComp.Create(TUserInterface.Node.Bank);
  try
    for I := 0 to CHistoricCount-1 do begin
      Require(LBlock.LoadBlockFromStream(FData,LError),LError);
      FHeaders[I] := LBlock.OperationBlock;
      Require(FHeaders[I].block = CLargeInitialHeight+Cardinal(I),'Wrong large public block sequence');
    end;
    Require(FData.Position = FData.Size,'Unexpected bytes after the five large public blocks');
  finally LBlock.Free; end;
  FData.Position := 0;
  FObservation := TCommitObservation.Create(True);
  FListener := TTCPBlockSocket.Create;
  FListener.CreateSocket;
  FListener.Bind('127.0.0.1','0');
  FListener.Listen;
  Require(FListener.LastError = 0,'Cannot open the large fixture raw loopback listener');
  FConnection := TNetClient.Create(nil);
  FConnection.FreeNotification(FObservation);
  Require(FConnection.ConnectTo('127.0.0.1',FListener.GetLocalSinPort),
    'The real large-data transport owner did not connect to loopback');
  Require(FListener.CanRead(1000),'The raw large-data peer did not receive the connection');
  FAccepted := TTCPBlockSocket.Create;
  FAccepted.Socket := FListener.Accept;
  Require(FAccepted.LastError = 0,'Cannot accept the real large-data loopback client');
  FDialog := TForm.CreateNew(FMain);
  FDialog.Caption := 'Owned large-data modeless focus regression';
  FDialog.Show;
  SetActiveWindow(FDialog.Handle);
  ReflectBufferedResponse;
  for I := 1 to CHistoricCount do begin
    WaitLargeCommit(I);
    CheckLargeCommit(I);
    if I = 1 then begin
      LDeadline := GetTickCount64+1200;
      repeat PumpResponsive; Sleep(1); until GetTickCount64 >= LDeadline;
      Require(Assigned(FObservation.ProcessingThread) and
        (FObservation.ProcessingThread is TNetClientThread),
        'The large response did not execute on the actual outgoing client owner');
      Require(not TThreadAccess(FObservation.ProcessingThread).Terminated and
        (InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) = 0),
        'Network-off cleanup cancelled or destroyed the large buffered processor');
    end;
    FObservation.ContinueBatch.SetEvent;
  end;
  LDeadline := GetTickCount64+10000;
  repeat PumpResponsive; Sleep(1); until
    (InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) <> 0) or (GetTickCount64 >= LDeadline);
  Require(InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) <> 0,
    'The actual large-data transport owner did not retire after backlog drain');
  FConnection := nil;
  Require(TNetData.NetData.NewRequestId = FObservation.FirstOffRequestID+1,
    'The large-data drain allocated a new download request after network-off');
  Require(TUserInterface.Node.Bank.BlocksCount = CLargeFinalHeight,'The large bank did not finish all five received blocks');
  CheckFinalPages;
  CheckPending(TUserInterface.Node,'LARGE AFTER');
  FreeAndNil(FAccepted);
  FreeAndNil(FListener);
  TUserInterface.ExitApplication;
  FMain := nil;
  FDialog := nil;
  CheckLargeStorage;
end;

function RunLargeBufferedBlockProbe: Integer;
var LChecks: TLargeBufferedBlockChecks; LInput: THandle;
  LName, LInputName: array[0..255] of Char; LRequired: DWORD;
begin
  Result := 1;
  if ParamCount <> 1 then Exit;
  LInput := OpenInputDesktop(0,False,DESKTOP_READOBJECTS);
  if LInput = 0 then RaiseLastOSError;
  try
    if not GetUserObjectInformation(GetThreadDesktop(GetCurrentThreadId),UOI_NAME,
      @LName[0],SizeOf(LName),LRequired) then RaiseLastOSError;
    if not GetUserObjectInformation(LInput,UOI_NAME,@LInputName[0],SizeOf(LInputName),LRequired) then RaiseLastOSError;
    Require(not SameText(StrPas(PChar(@LName[0])),StrPas(PChar(@LInputName[0]))),
      'Use the existing owned private-desktop broker');
  finally CloseDesktop(LInput); end;
  Require(not FileExists(ParamStr(1)),'Refusing an existing large test log');
  Application.Initialize;
  LChecks := TLargeBufferedBlockChecks.Create(ExpandFileName(ParamStr(1)),True,False,False,False,False);
  try
    try LChecks.RunLarge; Result := 0;
    except on E: Exception do LChecks.Log('FAIL '+LargeProviderException(E)); end;
  finally LChecks.Free; end;
end;
