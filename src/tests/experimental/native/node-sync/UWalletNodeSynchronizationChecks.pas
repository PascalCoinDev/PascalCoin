unit UWalletNodeSynchronizationChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Actual main-form/provider wiring with generated core accounts. The only
  artificial delay is inside inherited account membership, after real core
  locks have been acquired. No row, snapshot, grid refresh or lock is mocked. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunWalletNodeSynchronizationProbe: Integer;

implementation

uses Windows, Classes, SysUtils, SyncObjs, Forms, StdCtrls, Variants,
  UNode, UNetProtocol, UThread, UAccounts, UPCDataTypes, UBlockChain, UCrypto,
  UConst, UOpTransaction, UWallet, UUserInterface, UWalletStartup,
  UFRMMainForm, UCTRLWallet, UDataSources, UCoreObjects, UCommon.Data,
  UVisualGrid, UMemory, UPCAbstractMem;

type
  TQueryKind = (qkPage, qkAggregate);

  TQueryGate = class
  public
    Armed: LongInt;
    Entered, ReleaseRead, CancelObserved, Exited: TEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Arm;
    procedure Hold;
  end;

  TCoreQueryObservations = class
  public
    Gates: array[TQueryKind] of TQueryGate;
    AccountQueries, OperationQueries, AggregateMemberships, BalanceReads: LongInt;
    LastTotal, LastPageIndex, LastRequestedPageIndex, LastPageSize: Integer;
    FirstAccount, MinimumAccount, MaximumAccount: Cardinal;
    LastRowCount: Integer;
    RealMempoolObserved: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure ReleaseAll;
  end;

  TObservedCoreAccounts = class(TMyAccountsDataSource)
  private
    FObserved: TCoreQueryObservations;
  protected
    function OwnershipChanges(const AContext: TAccountReadContext): TArray<TAccountOwnershipChange>; override;
    function ReadBalanceAccount(AAccountNumber: Cardinal; const AContext: TAccountReadContext): TAccount; override;
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
      var ADataTable: TDataTable): TPageFetchResult; override;
  public
    constructor CreateObserved(AOwner: TComponent; AObserved: TCoreQueryObservations);
  end;

  TObservedCoreOperations = class(TAccountsOperationsDataSource)
  private
    FObserved: TCoreQueryObservations;
  protected
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
      var ADataTable: TDataTable): TPageFetchResult; override;
  public
    constructor CreateObserved(AOwner: TComponent; AObserved: TCoreQueryObservations);
  end;

  TCoreSynchronizationWallet = class(TCTRLWallet)
  private
    FObserved: TCoreQueryObservations;
    FCoreAccounts: TObservedCoreAccounts;
  protected
    function CreateAccountsDataSource: TMyAccountsDataSource; override;
    function CreateOperationsDataSource: TAccountsOperationsDataSource; override;
  public
    constructor CreateObserved(AOwner: TComponent; AObserved: TCoreQueryObservations);
    property AccountsGrid;
    property OperationsGrid;
    property CoreAccounts: TObservedCoreAccounts read FCoreAccounts;
  end;

  TCoreSynchronizationMain = class(TFRMMainForm)
  private
    FObserved: TCoreQueryObservations;
    function GetCoreWallet: TCoreSynchronizationWallet;
  protected
    function CreateWalletControl: TCTRLWallet; override;
  public
    property Observed: TCoreQueryObservations read FObserved write FObserved;
    property CoreWallet: TCoreSynchronizationWallet read GetCoreWallet;
  end;

  TGeneratedSafeBoxAccess = class(TPCSafeBox)
  public
    procedure Seed(const ALargeKey, ASmallKey: TAccountKey);
  end;

  TActualNodeWriter = class(TPCThread)
  private
    FNode: TNode;
    FTree: TOperationsHashTree;
    procedure OnProgress(Sender: TObject; const AProgress: TProgressInfo);
  protected
    procedure BCExecute; override;
  public
    Prepared, Completed: TEvent;
    ErrorMessage: String;
    constructor Create(ANode: TNode; AKey: TECPrivateKey);
    destructor Destroy; override;
  end;

  TWalletNodeSynchronizationChecks = class
  private
    FMain: TCoreSynchronizationMain;
    FObserved: TCoreQueryObservations;
    FKey, FSmallKey: TECPrivateKey;
    FPeer: TObject;
    FLog: TStringList;
    FLogPath, FAsyncError, FPhase: String;
    FPreviousException: TExceptionEvent;
    procedure Log(const AText: String);
    procedure OnException(Sender: TObject; E: Exception);
    procedure Pump;
    procedure PumpFor(AMilliseconds: Cardinal);
    procedure WaitEvent(AEvent: TEvent; const ADescription: String; ATimeout: Cardinal = 5000);
    procedure WaitIdle;
    procedure WaitSuspended(AValue: Boolean);
    procedure StartSynchronization;
    procedure CheckAccountCacheOrdering;
    procedure SeedActualAccounts;
    procedure ChooseSmallKey;
    procedure CheckHeldRead(AKind: TQueryKind);
  public
    constructor Create(const ALogPath: String);
    destructor Destroy; override;
    procedure Run;
  end;

threadvar
  AccountPageSource: Pointer;
  SeedBlockIndex: Integer;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

constructor TQueryGate.Create;
begin
  inherited;
  Entered := TEvent.Create(nil,True,False,'');
  ReleaseRead := TEvent.Create(nil,True,False,'');
  CancelObserved := TEvent.Create(nil,True,False,'');
  Exited := TEvent.Create(nil,True,False,'');
end;

destructor TQueryGate.Destroy;
begin
  ReleaseRead.Free;
  CancelObserved.Free;
  Exited.Free;
  Entered.Free;
  inherited;
end;

procedure TQueryGate.Arm;
begin
  Entered.ResetEvent;
  ReleaseRead.ResetEvent;
  CancelObserved.ResetEvent;
  Exited.ResetEvent;
  InterlockedExchange(Armed,1);
end;

procedure TQueryGate.Hold;
begin
  if InterlockedCompareExchange(Armed,0,1) <> 1 then Exit;
  Entered.SetEvent;
  while ReleaseRead.WaitFor(10) <> wrSignaled do
    if TThread.CheckTerminated then begin
      CancelObserved.SetEvent;
      Break;
    end;
end;

constructor TCoreQueryObservations.Create;
var K: TQueryKind;
begin
  inherited;
  for K := Low(TQueryKind) to High(TQueryKind) do Gates[K] := TQueryGate.Create;
end;

destructor TCoreQueryObservations.Destroy;
var K: TQueryKind;
begin
  for K := Low(TQueryKind) to High(TQueryKind) do Gates[K].Free;
  inherited;
end;

procedure TCoreQueryObservations.ReleaseAll;
var K: TQueryKind;
begin
  for K := Low(TQueryKind) to High(TQueryKind) do Gates[K].ReleaseRead.SetEvent;
end;

constructor TObservedCoreAccounts.CreateObserved(AOwner: TComponent; AObserved: TCoreQueryObservations);
begin
  FObserved := AObserved;
  inherited Create(AOwner);
end;

function TObservedCoreAccounts.OwnershipChanges(const AContext: TAccountReadContext): TArray<TAccountOwnershipChange>;
var K: TQueryKind;
begin
  if AccountPageSource = Pointer(Self) then K := qkPage else begin
    K := qkAggregate;
    InterlockedIncrement(FObserved.AggregateMemberships);
  end;
  FObserved.RealMempoolObserved := Assigned(AContext.Mempool);
  // The production FetchPage/CalculateBalance try/finally is already active.
  // Cancellation returns through inherited membership and releases real locks.
  FObserved.Gates[K].Hold;
  Result := inherited OwnershipChanges(AContext);
end;

function TObservedCoreAccounts.ReadBalanceAccount(AAccountNumber: Cardinal;
  const AContext: TAccountReadContext): TAccount;
begin
  InterlockedIncrement(FObserved.BalanceReads);
  Result := inherited ReadBalanceAccount(AAccountNumber,AContext);
end;

function TObservedCoreAccounts.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ADataTable: TDataTable): TPageFetchResult;
var LPrevious: Pointer; I: Integer; LAccount: Cardinal;
begin
  InterlockedIncrement(FObserved.AccountQueries);
  LPrevious := AccountPageSource;
  AccountPageSource := Pointer(Self);
  try
    Result := inherited FetchPage(AParams,ADataTable);
    FObserved.LastTotal := Result.TotalDataCount;
    FObserved.LastPageIndex := Result.PageIndex;
    FObserved.LastRequestedPageIndex := AParams.PageIndex;
    FObserved.LastPageSize := AParams.PageSize;
    FObserved.LastRowCount := Length(ADataTable.Rows);
    FObserved.MinimumAccount := High(Cardinal);
    FObserved.MaximumAccount := 0;
    if Length(ADataTable.Rows) > 0 then
      FObserved.FirstAccount := Cardinal(ADataTable.Rows[0].AccountNumber);
    for I := 0 to High(ADataTable.Rows) do begin
      LAccount := Cardinal(ADataTable.Rows[I].AccountNumber);
      if LAccount < FObserved.MinimumAccount then FObserved.MinimumAccount := LAccount;
      if LAccount > FObserved.MaximumAccount then FObserved.MaximumAccount := LAccount;
    end;
  finally
    AccountPageSource := LPrevious;
    FObserved.Gates[qkPage].Exited.SetEvent;
  end;
end;

constructor TObservedCoreOperations.CreateObserved(AOwner: TComponent; AObserved: TCoreQueryObservations);
begin
  FObserved := AObserved;
  inherited Create(AOwner);
end;

function TObservedCoreOperations.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ADataTable: TDataTable): TPageFetchResult;
begin
  InterlockedIncrement(FObserved.OperationQueries);
  Result := inherited FetchPage(AParams,ADataTable);
end;

constructor TCoreSynchronizationWallet.CreateObserved(AOwner: TComponent; AObserved: TCoreQueryObservations);
begin
  FObserved := AObserved;
  inherited Create(AOwner);
end;

function TCoreSynchronizationWallet.CreateAccountsDataSource: TMyAccountsDataSource;
begin
  FCoreAccounts := TObservedCoreAccounts.CreateObserved(Self,FObserved);
  Result := FCoreAccounts;
end;

function TCoreSynchronizationWallet.CreateOperationsDataSource: TAccountsOperationsDataSource;
begin
  Result := TObservedCoreOperations.CreateObserved(Self,FObserved);
end;

function TCoreSynchronizationMain.CreateWalletControl: TCTRLWallet;
begin
  Result := TCoreSynchronizationWallet.CreateObserved(Self,FObserved);
end;

function TCoreSynchronizationMain.GetCoreWallet: TCoreSynchronizationWallet;
begin
  Result := TCoreSynchronizationWallet(WalletControl);
end;

procedure TGeneratedSafeBoxAccess.Seed(const ALargeKey, ASmallKey: TAccountKey);
var LBlock: TOperationBlock; I: Integer;
begin
  for I := 0 to 51 do begin
    SeedBlockIndex := I;
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := I;
    LBlock.account_key := ALargeKey;
    if I = 51 then LBlock.account_key := ASmallKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    AddNew(LBlock);
  end;
end;

constructor TActualNodeWriter.Create(ANode: TNode; AKey: TECPrivateKey);
var LOperation: TPCOperation;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
  FTree := TOperationsHashTree.Create;
  LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_1,1000000,1,1000001,
    AKey,1,1,CT_TOperationPayload_NUL);
  try FTree.AddOperationToHashTree(LOperation);
  finally LOperation.Free; end;
  Prepared := TEvent.Create(nil,True,False,'');
  Completed := TEvent.Create(nil,True,False,'');
end;

destructor TActualNodeWriter.Destroy;
begin
  Terminate;
  if Suspended then Start;
  WaitFor;
  Completed.Free;
  Prepared.Free;
  FTree.Free;
  inherited;
end;

procedure TActualNodeWriter.OnProgress(Sender: TObject; const AProgress: TProgressInfo);
begin
  if Terminated then Abort;
  if AProgress.Stage = psPreparingOperations then Prepared.SetEvent;
end;

procedure TActualNodeWriter.BCExecute;
var LError: String;
begin
  try
    try
      if not Terminated then FNode.AddOperations(nil,FTree,nil,LError,OnProgress);
    except on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message; end;
  finally Completed.SetEvent; end;
end;

constructor TWalletNodeSynchronizationChecks.Create(const ALogPath: String);
begin
  inherited Create;
  FLogPath := ALogPath;
  FLog := TStringList.Create;
  FObserved := TCoreQueryObservations.Create;
  FPeer := TObject.Create;
  FKey := TECPrivateKey.Create;
  FSmallKey := TECPrivateKey.Create;
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
end;

destructor TWalletNodeSynchronizationChecks.Destroy;
begin
  FObserved.ReleaseAll;
  if Assigned(FMain) then begin
    if TUserInterface.Started then
      TUserInterface.Node.SetSynchronizationNetworkActive(False);
    TUserInterface.ExitApplication;
    FMain := nil;
  end;
  Application.OnException := FPreviousException;
  FSmallKey.Free;
  FKey.Free;
  FPeer.Free;
  FObserved.Free;
  FLog.Free;
  inherited;
end;

procedure TWalletNodeSynchronizationChecks.Log(const AText: String);
begin
  FLog.Add(UIntToStr(GetTickCount64) + ' ' + AText);
  FLog.SaveToFile(FLogPath);
end;

procedure TWalletNodeSynchronizationChecks.OnException(Sender: TObject; E: Exception);
begin
  if FAsyncError = '' then FAsyncError := E.ClassName + ': ' + E.Message;
end;

procedure TWalletNodeSynchronizationChecks.Pump;
begin
  Application.ProcessMessages;
  CheckSynchronize(0);
  Require(FAsyncError = '',FAsyncError);
end;

procedure TWalletNodeSynchronizationChecks.PumpFor(AMilliseconds: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat Pump; Sleep(1); until GetTickCount64 >= LUntil;
end;

procedure TWalletNodeSynchronizationChecks.WaitEvent(AEvent: TEvent;
  const ADescription: String; ATimeout: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + ATimeout;
  repeat
    Pump;
    if AEvent.WaitFor(0) = wrSignaled then Exit;
    Sleep(1);
  until GetTickCount64 >= LUntil;
  raise Exception.Create(ADescription);
end;

procedure TWalletNodeSynchronizationChecks.WaitIdle;
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + 15000;
  repeat
    Pump;
    if Assigned(FMain.CoreWallet) and
      not FMain.CoreWallet.AccountsGrid.IsUpdating and
      not FMain.CoreWallet.OperationsGrid.IsUpdating and
      not FMain.CoreWallet.AccountsGrid.RefreshThrottle.Waiting and
      not FMain.CoreWallet.OperationsGrid.RefreshThrottle.Waiting then Exit;
    Sleep(1);
  until GetTickCount64 >= LUntil;
  raise Exception.Create('Actual wallet grids did not finish their admitted requests');
end;

procedure TWalletNodeSynchronizationChecks.WaitSuspended(AValue: Boolean);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + 3000;
  repeat
    Pump;
    if (FMain.CoreWallet.AccountsGrid.RefreshSuspended = AValue) and
      (FMain.CoreWallet.OperationsGrid.RefreshSuspended = AValue) then Exit;
    Sleep(1);
  until GetTickCount64 >= LUntil;
  raise Exception.Create('Actual wallet did not follow the node synchronization event');
end;

procedure TWalletNodeSynchronizationChecks.StartSynchronization;
var LNode: TNode;
begin
  LNode := TUserInterface.Node;
  LNode.SetSynchronizationNetworkActive(True);
  LNode.SetSynchronizationPeerConnected(FPeer,True);
  LNode.SetSynchronizationTarget(FPeer,LNode.GetSynchronizationStatus.LocalBlockCount+20);
  WaitSuspended(True);
end;

procedure TWalletNodeSynchronizationChecks.CheckAccountCacheOrdering;
const CNumbers: array[0..3] of Cardinal = (0, 5, $80000000, High(Cardinal));
var LMemory: TPCAbstractMem; LAccount, LFound: TAccount; I: Integer;
begin
  FPhase := 'actual-account-cache-ordering';
  LMemory := TPCAbstractMem.Create('',False);
  try
    // Use the actual cache and its private production comparison callback.
    // Reversed 0/5 and both sides of the signed boundary must remain ordered.
    for I := High(CNumbers) downto Low(CNumbers) do begin
      LAccount := CT_Account_NUL;
      LAccount.account := CNumbers[I];
      LAccount.balance := I+1;
      LMemory.AccountCache.Add(LAccount);
    end;
    for I := Low(CNumbers) to High(CNumbers) do begin
      LAccount := CT_Account_NUL;
      LAccount.account := CNumbers[I];
      Require(LMemory.AccountCache.Find(LAccount,LFound),
        'The actual account cache lost a Cardinal boundary key');
      Require((LFound.account = CNumbers[I]) and (LFound.balance = UInt64(I+1)),
        'The actual account cache comparison aliased distinct keys');
    end;
    LAccount := CT_Account_NUL;
    LAccount.account := 1;
    Require(not LMemory.AccountCache.Find(LAccount,LFound),
      'The actual cache must reject an absent account');
    LAccount.account := High(Cardinal);
    LMemory.AccountCache.Remove(LAccount);
    Require(not LMemory.AccountCache.Find(LAccount,LFound),
      'The actual cache must remove only the requested boundary account');
    LAccount.account := 0;
    Require(LMemory.AccountCache.Find(LAccount,LFound),
      'Removing a high account must preserve account zero');
  finally LMemory.Free; end;
  Log('PASS actual account-cache ordering:0,5,80000000,FFFFFFFF/reversed reads/removal');
end;

procedure TWalletNodeSynchronizationChecks.SeedActualAccounts;
var LNode: TNode; LMempool: TPCOperationsComp; LUntil: UInt64;
  LTotalLabel: TLabel;
begin
  LNode := TUserInterface.Node;
  FPhase := 'generate-large-owner';
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FPhase := 'generate-small-owner';
  FSmallKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FPhase := 'acquire-seed-writer';
  LMempool := LNode.LockMempoolWrite;
  try
    Require(LNode.Bank.SafeBox.AccountsCount = 0,'Fresh data must be empty before seeding');
    FPhase := 'seed-actual-safebox';
    TGeneratedSafeBoxAccess(LNode.Bank.SafeBox).Seed(FKey.PublicKey,FSmallKey.PublicKey);
    FPhase := 'clear-actual-mempool';
    LMempool.Clear(True);
  finally LNode.UnlockMempoolWrite; end;
  FPhase := 'assign-wallet-safebox';
  TWallet.Keys.SafeBox := LNode.Bank.SafeBox;
  FPhase := 'add-large-watch-key';
  TWallet.Keys.AddPublicKey('Large generated owner',FKey.PublicKey);
  FPhase := 'add-small-watch-key';
  TWallet.Keys.AddPublicKey('Small generated owner',FSmallKey.PublicKey);
  FPhase := 'publish-generated-records';
  TUserInterface.ShowWallet;
  LNode.NotifyBlocksChanged;
  LTotalLabel := FMain.CoreWallet.FindComponent('lblTotalPASA') as TLabel;
  LUntil := GetTickCount64+15000;
  repeat
    Pump;
    if (FObserved.LastTotal = 52*CT_AccountsPerBlock) and
      (LTotalLabel.Caption = IntToStr(52*CT_AccountsPerBlock)) then Break;
    Sleep(1);
  until GetTickCount64 >= LUntil;
  WaitIdle;
  Require(FObserved.LastTotal = 52*CT_AccountsPerBlock,'Actual provider must enumerate all generated accounts');
  Require(LTotalLabel.Caption = IntToStr(52*CT_AccountsPerBlock),
    'The actual aggregate must complete before its later cancellation test');
  Log('READY actual main/private sources:260generated core records,two watch-only keys,no transport');
end;

procedure TWalletNodeSynchronizationChecks.ChooseSmallKey;
var LCombo: TComboBox; I: Integer;
begin
  LCombo := FMain.CoreWallet.FindComponent('cbAccounts') as TComboBox;
  for I := 0 to LCombo.Items.Count-1 do
    if Pos('Small generated owner',LCombo.Items[I]) > 0 then begin
      LCombo.ItemIndex := I;
      Require(Assigned(LCombo.OnChange),'Actual key selector must retain its handler');
      LCombo.OnChange(LCombo);
      Exit;
    end;
  raise Exception.Create('The real wallet key selector did not include its generated small key');
end;

procedure TWalletNodeSynchronizationChecks.CheckHeldRead(AKind: TQueryKind);
var LWriter: TActualNodeWriter; LGate: TQueryGate; LSummary: TBalanceSummary;
  LAccounts, LOperations, LMemberships, LExpectedStart, LExpectedCount: Integer;
  LStarted: UInt64; LWriteAdmitted: Boolean;
begin
  WaitIdle;
  LGate := FObserved.Gates[AKind];
  LGate.Arm;
  if AKind = qkPage then FMain.CoreWallet.AccountsGrid.RefreshGrid
  else begin
    FMain.CoreWallet.CoreAccounts.InvalidateWalletData;
    FMain.CoreWallet.CoreAccounts.TryGetBalanceSummary(LSummary);
  end;
  WaitEvent(LGate.Entered,'The actual provider never entered its real membership scope');
  Require(FObserved.RealMempoolObserved,'The gate must retain an actual mempool read snapshot');
  LWriteAdmitted := TUserInterface.Node.TryLockNode(0);
  if LWriteAdmitted then TUserInterface.Node.UnlockNode;
  Require(not LWriteAdmitted,'Actual provider did not retain its real core reader');
  LWriter := TActualNodeWriter.Create(TUserInterface.Node,FKey);
  try
    LWriter.Start;
    WaitEvent(LWriter.Prepared,'Actual Node.AddOperations did not reach write preparation');
    Require(LWriter.Completed.WaitFor(100) = wrTimeout,'Writer unexpectedly bypassed the held actual reader');
    LStarted := GetTickCount64;
    StartSynchronization;
    WaitEvent(LGate.CancelObserved,'NodeSync did not cancel the actual source worker');
    WaitEvent(LWriter.Completed,'The canceled source did not release the actual node writer');
    Require(LWriter.ErrorMessage = '',LWriter.ErrorMessage);
    Require(LGate.ReleaseRead.WaitFor(0) = wrTimeout,'The fixture must not manually release the read gate');
    Log('WRITER PROGRESSED after real cancellation kind=' + IntToStr(Ord(AKind)) +
      ' elapsed=' + UIntToStr(GetTickCount64-LStarted));
    Require(FMain.CoreWallet.AccountsGrid.LoadingVisible and
      FMain.CoreWallet.OperationsGrid.LoadingVisible,'Both actual grids must retain the synchronization card');
    LAccounts := FObserved.AccountQueries;
    LOperations := FObserved.OperationQueries;
    LMemberships := FObserved.AggregateMemberships;
    if AKind = qkPage then begin
      ChooseSmallKey;
      FMain.CoreWallet.AccountsGrid.PageIndex := 2;
      FMain.Width := FMain.Width+80;
      FMain.CoreWallet.OperationsHistory := woh30Days;
      TUserInterface.Node.NotifyBlocksChanged;
      PumpFor(6200);
    end else PumpFor(500);
    Require(FObserved.AccountQueries = LAccounts,'No account query may execute while NodeSync is active');
    Require(FObserved.OperationQueries = LOperations,'No operations query may execute while NodeSync is active');
    Require(FObserved.AggregateMemberships = LMemberships,'No aggregate may restart while NodeSync is active');
    if AKind = qkPage then TUserInterface.Node.SetSynchronizationNetworkActive(False)
    else TUserInterface.Node.SetSynchronizationPeerConnected(FPeer,False);
    WaitSuspended(False);
    WaitIdle;
    Require(FObserved.AccountQueries = LAccounts+1,'Resume must admit exactly one latest real account query');
    Require(FObserved.OperationQueries = LOperations+1,'Resume must admit exactly one latest real operations query');
    Require(FObserved.LastTotal = CT_AccountsPerBlock,'The resumed query must use the latest selected key');
    Log('ROWS first=' + UIntToStr(FObserved.FirstAccount) +
      ' min=' + UIntToStr(FObserved.MinimumAccount) +
      ' max=' + UIntToStr(FObserved.MaximumAccount) +
      ' count=' + IntToStr(FObserved.LastRowCount) +
      ' requestedPage=' + IntToStr(FObserved.LastRequestedPageIndex) +
      ' page=' + IntToStr(FObserved.LastPageIndex) +
      ' capacity=' + IntToStr(FObserved.LastPageSize));
    // The requested page may be clipped after changing to a five-account key.
    // Verify that exact real slice rather than demanding the whole data set.
    LExpectedStart := FObserved.LastPageIndex*FObserved.LastPageSize;
    LExpectedCount := CT_AccountsPerBlock-LExpectedStart;
    if LExpectedCount > FObserved.LastPageSize then
      LExpectedCount := FObserved.LastPageSize;
    Require((LExpectedCount > 0) and
      (FObserved.LastRowCount = LExpectedCount) and
      (FObserved.MinimumAccount = Cardinal(51*CT_AccountsPerBlock+LExpectedStart)) and
      (FObserved.MaximumAccount = Cardinal(51*CT_AccountsPerBlock+LExpectedStart+LExpectedCount-1)),
      'The resumed rows must be actual small-key core accounts');
    Require(not FMain.CoreWallet.AccountsGrid.LoadingVisible and
      not FMain.CoreWallet.OperationsGrid.LoadingVisible,'Node completion must remove both manual cards');
    Log('PASS latest real query resumed once kind=' + IntToStr(Ord(AKind)) +
      ' rows=' + IntToStr(FObserved.LastTotal));
  finally
    FObserved.ReleaseAll;
    LWriter.Free;
  end;
end;

procedure TWalletNodeSynchronizationChecks.Run;
var LOptions: TWalletStartupOptions; LArguments: array of String;
  LDirectory, LError: String; LID: TGUID; LUntil: UInt64; LQueries: Integer;
begin
  FPhase := 'fresh-options';
  TCrypto.InitCrypto;
  CheckAccountCacheOrdering;
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
    'PascalCoinNodeSync-' + GUIDToString(LID);
  Require(not DirectoryExists(LDirectory),'Owned data directory must be fresh');
  ForceDirectories(LDirectory);
  SetLength(LArguments,3);
  LArguments[0] := '--data-dir='+LDirectory;
  LArguments[1] := '--offline';
  LArguments[2] := '--no-startup-dialogs';
  Require(TryParseWalletStartupOptions(LArguments,LOptions,LError),LError);
  FPhase := 'configure-actual-controller';
  TUserInterface.ConfigureStartup(LOptions);
  FPhase := 'create-actual-main';
  Application.CreateForm(TCoreSynchronizationMain,FMain);
  FMain.Observed := FObserved;
  FPhase := 'start-actual-controller';
  TUserInterface.StartApplication(FMain);
  FPhase := 'await-actual-startup';
  LUntil := GetTickCount64+10000;
  repeat
    Pump;
    if Assigned(FMain.CoreWallet) and (TUserInterface.State <> uisLoading) and
      (TUserInterface.State <> uisError) then Break;
    Sleep(1);
  until GetTickCount64 >= LUntil;
  Require(Assigned(FMain.CoreWallet) and (TUserInterface.State <> uisLoading) and
    (TUserInterface.State <> uisError),'The actual fresh controller did not load');
  SeedActualAccounts;
  FPhase := 'actual-page-cancellation';
  CheckHeldRead(qkPage);
  FPhase := 'actual-aggregate-cancellation';
  CheckHeldRead(qkAggregate);
  FPhase := 'actual-shutdown';
  StartSynchronization;
  FMain.CoreWallet.AccountsGrid.RefreshGrid;
  LQueries := FObserved.AccountQueries + FObserved.OperationQueries;
  TUserInterface.ExitApplication;
  FMain := nil;
  Require(FObserved.AccountQueries+FObserved.OperationQueries = LQueries,
    'Stopping must not admit retained source requests');
  Log('PASS stopping joined the actual UI/source workers without a new fetch');
end;

function DesktopName(AHandle: THandle): String;
var LBuffer: array[0..255] of Char; LRequired: DWORD;
begin
  if not GetUserObjectInformation(AHandle,UOI_NAME,@LBuffer[0],SizeOf(LBuffer),LRequired) then RaiseLastOSError;
  Result := StrPas(PChar(@LBuffer[0]));
end;

function RunWalletNodeSynchronizationProbe: Integer;
var LChecks: TWalletNodeSynchronizationChecks; LInput: THandle;
begin
  Result := 1;
  if ParamCount <> 1 then Exit;
  LInput := OpenInputDesktop(0,False,DESKTOP_READOBJECTS);
  if LInput = 0 then RaiseLastOSError;
  try
    Require(not SameText(DesktopName(GetThreadDesktop(GetCurrentThreadId)),DesktopName(LInput)),
      'This regression requires the existing owned private-desktop broker');
  finally CloseDesktop(LInput); end;
  Require(not FileExists(ParamStr(1)),'Refusing an existing probe log');
  Application.Initialize;
  LChecks := TWalletNodeSynchronizationChecks.Create(ExpandFileName(ParamStr(1)));
  try
    try LChecks.Run; Result := 0;
    except on E: Exception do LChecks.Log('FAIL '+E.ClassName+': '+E.Message+
      ' phase='+LChecks.FPhase+' block='+IntToStr(SeedBlockIndex)+
      ' address='+IntToHex(NativeUInt(ExceptAddr),SizeOf(Pointer)*2)+
      ' module='+IntToHex(NativeUInt(GetModuleHandle(nil)),SizeOf(Pointer)*2)); end;
  finally LChecks.Free; end;
end;

end.
