unit UWalletGridMocks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Test-only providers for the experimental wallet. The real control owns these
  typed sources through its normal factories; its scheduler, invalidation,
  caches, selection callbacks, page population and balance worker are inherited.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, UCommon, UCommon.Data,
  UDataSources, UWalletStartupTask, UCoreObjects, UPCDataTypes, UBlockChain,
  UCTRLWallet, UFRMMainForm, UVisualGrid;

type
  TMockDataKind = (mdkAccounts, mdkOperations);

  TMockFetchMetrics = record
    FetchCount, CompletedCount, RawReadCount, PageAccountReadCount: Integer;
    BalanceAccountReadCount, SnapshotReadCount, MembershipReadCount: Integer;
    ActiveFetches, LastRevision: Integer;
    LastStartedTick, LastCompletedTick: UInt64;
  end;

  TMockWalletSnapshot = record
    Revision, RowCount: Integer;
  end;

  TMockWalletData = class(TComponent)
  private
    FLock: TCriticalSection;
    FFetchGate: TEvent;
    FBalanceGate: TEvent;
    FEvents: TStringList;
    FRevision, FRowCount, FDelayMilliseconds: Integer;
    FGateFetches, FShuttingDown: Boolean;
    FMetrics: array[TMockDataKind] of TMockFetchMetrics;
    function GetRevision: Integer;
    function GetRowCount: Integer;
    procedure SetRowCount(AValue: Integer);
    function GetDelayMilliseconds: Integer;
    procedure SetDelayMilliseconds(AValue: Integer);
    function GetGateFetches: Boolean;
    procedure SetGateFetches(AValue: Boolean);
    function GetShuttingDown: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetData(ARevision, ARowCount: Integer);
    procedure Advance(ARowCount: Integer = -1);
    procedure Reset;
    procedure ReleaseFetches;
    procedure HoldBalanceSnapshots;
    procedure ReleaseBalanceSnapshots;
    function AwaitBalanceSnapshot(ATask: TWalletStartupTask): Boolean;
    procedure Shutdown;
    function Snapshot: TMockWalletSnapshot;
    function Metrics(AKind: TMockDataKind): TMockFetchMetrics;
    procedure ResetMetrics;
    procedure LogEvent(const AMessage: String);
    procedure DrainEvents(AOutput: TStrings);
    procedure BeginFetch(AKind: TMockDataKind);
    procedure FinishFetch(AKind: TMockDataKind; ARevision: Integer);
    procedure CountRawRead(AKind: TMockDataKind);
    procedure CountMembershipRead;
    procedure CountAccountRead(ABalance: Boolean);
    procedure CountSnapshotRead;
    function AccountKey: TAccountKey;
    function Account(AID: Cardinal; const ASnapshot: TMockWalletSnapshot): TAccount;
    property Revision: Integer read GetRevision;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property DelayMilliseconds: Integer read GetDelayMilliseconds write SetDelayMilliseconds;
    property GateFetches: Boolean read GetGateFetches write SetGateFetches;
    property ShuttingDown: Boolean read GetShuttingDown;
  end;

  TMockMyAccountsDataSource = class(TMyAccountsDataSource)
  private
    FModel: TMockWalletData;
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
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
      var ADataTable: TDataTable): TPageFetchResult; override;
  public
    constructor CreateForModel(AOwner: TComponent; AModel: TMockWalletData);
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

  TMockAccountsOperationsDataSource = class(TAccountsOperationsDataSource)
  private
    FModel: TMockWalletData;
    FLastFetchBlockDepth: Cardinal;
    FRawRevision: Integer;
    FOperationsPerAccount: Integer;
  protected
    function FetchWalletAccounts: TArray<Cardinal>; override;
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
      var ADataTable: TDataTable): TPageFetchResult; override;
  public
    constructor CreateForModel(AOwner: TComponent; AModel: TMockWalletData);
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
    property LastFetchBlockDepth: Cardinal read FLastFetchBlockDepth;
    property OperationsPerAccount: Integer read FOperationsPerAccount write FOperationsPerAccount;
  end;

  TMockWallet = class(TCTRLWallet)
  private
    FModel: TMockWalletData;
    FAccountsSource: TMockMyAccountsDataSource;
    FOperationsSource: TMockAccountsOperationsDataSource;
    FAccountRefreshCount, FOperationRefreshCount: Integer;
    FLastAccountRefreshTick, FLastOperationRefreshTick: UInt64;
    procedure OnAccountsRefresh(Sender: TObject);
    procedure OnOperationsRefresh(Sender: TObject);
  protected
    function CreateAccountsDataSource: TMyAccountsDataSource; override;
    function CreateOperationsDataSource: TAccountsOperationsDataSource; override;
  public
    constructor CreateForModel(AOwner: TComponent; AModel: TMockWalletData);
    procedure AfterConstruction; override;
    property Model: TMockWalletData read FModel;
    property AccountsSource: TMockMyAccountsDataSource read FAccountsSource;
    property OperationsSource: TMockAccountsOperationsDataSource read FOperationsSource;
    property AccountsGrid;
    property OperationsGrid;
    property AccountRefreshCount: Integer read FAccountRefreshCount;
    property OperationRefreshCount: Integer read FOperationRefreshCount;
    property LastAccountRefreshTick: UInt64 read FLastAccountRefreshTick;
    property LastOperationRefreshTick: UInt64 read FLastOperationRefreshTick;
  end;

  TMockMainForm = class(TFRMMainForm)
  private
    FModel: TMockWalletData;
    function GetMockWallet: TMockWallet;
  protected
    function CreateWalletControl: TCTRLWallet; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Model: TMockWalletData read FModel;
    property MockWallet: TMockWallet read GetMockWallet;
  end;

implementation

uses
  Windows, Math, DateUtils, Variants, UBaseTypes, UConst, UAccounts, UMemory;

function KindName(AKind: TMockDataKind): String;
begin
  if AKind = mdkAccounts then Result := 'accounts' else Result := 'operations';
end;

function ContextSnapshot(const AContext: TAccountReadContext): TMockWalletSnapshot;
var LSeparator: Integer;
begin
  LSeparator := Pos('/', AContext.StateID);
  Result.Revision := StrToInt(Copy(AContext.StateID, 1, LSeparator - 1));
  Result.RowCount := StrToInt(Copy(AContext.StateID, LSeparator + 1, MaxInt));
end;

function TextRevision(const AText: String): Integer;
var LEnd: Integer;
begin
  Result := -1;
  if Pos('mock revision=', AText) <> 1 then Exit;
  LEnd := Pos(' account=', AText);
  if LEnd > 0 then Result := StrToInt(Copy(AText, 15, LEnd - 15));
end;

constructor TMockWalletData.Create(AOwner: TComponent);
begin
  inherited;
  FLock := SyncObjs.TCriticalSection.Create;
  FFetchGate := TEvent.Create(nil, true, true, '');
  FBalanceGate := TEvent.Create(nil, true, true, '');
  FEvents := TStringList.Create;
  FRowCount := 200;
end;

destructor TMockWalletData.Destroy;
begin
  Shutdown;
  FEvents.Free;
  FFetchGate.Free;
  FBalanceGate.Free;
  FLock.Free;
  inherited;
end;

function TMockWalletData.Snapshot: TMockWalletSnapshot;
begin
  FLock.Acquire;
  try Result.Revision := FRevision; Result.RowCount := FRowCount;
  finally FLock.Release; end;
end;

function TMockWalletData.GetRevision: Integer;
begin
  Result := Snapshot.Revision;
end;

function TMockWalletData.GetRowCount: Integer;
begin
  Result := Snapshot.RowCount;
end;

procedure TMockWalletData.SetRowCount(AValue: Integer);
begin
  Advance(AValue);
end;

procedure TMockWalletData.SetData(ARevision, ARowCount: Integer);
begin
  if (ARevision < 0) or (ARowCount < 0) then
    raise EArgumentException.Create('Mock revision and row count must be nonnegative');
  FLock.Acquire;
  try FRevision := ARevision; FRowCount := ARowCount;
  finally FLock.Release; end;
end;

procedure TMockWalletData.Advance(ARowCount: Integer);
begin
  FLock.Acquire;
  try
    Inc(FRevision);
    if ARowCount >= 0 then FRowCount := ARowCount;
  finally FLock.Release; end;
end;

procedure TMockWalletData.Reset;
begin
  ReleaseFetches;
  DelayMilliseconds := 0;
  Advance(200);
end;

function TMockWalletData.GetDelayMilliseconds: Integer;
begin
  FLock.Acquire;
  try Result := FDelayMilliseconds; finally FLock.Release; end;
end;

procedure TMockWalletData.SetDelayMilliseconds(AValue: Integer);
begin
  if AValue < 0 then raise EArgumentException.Create('Mock delay must be nonnegative');
  FLock.Acquire;
  try FDelayMilliseconds := AValue; finally FLock.Release; end;
end;

function TMockWalletData.GetGateFetches: Boolean;
begin
  FLock.Acquire;
  try Result := FGateFetches; finally FLock.Release; end;
end;

procedure TMockWalletData.SetGateFetches(AValue: Boolean);
begin
  FLock.Acquire;
  try
    FGateFetches := AValue;
    if AValue then FFetchGate.ResetEvent else FFetchGate.SetEvent;
  finally FLock.Release; end;
end;

procedure TMockWalletData.ReleaseFetches;
begin
  SetGateFetches(false);
end;

procedure TMockWalletData.HoldBalanceSnapshots;
begin
  FBalanceGate.ResetEvent;
end;

procedure TMockWalletData.ReleaseBalanceSnapshots;
begin
  FBalanceGate.SetEvent;
end;

function TMockWalletData.AwaitBalanceSnapshot(ATask: TWalletStartupTask): Boolean;
begin
  // Pause only the fake persisted snapshot read; retain the production balance
  // worker, generation checks and queued completion notification.
  while FBalanceGate.WaitFor(10)<>wrSignaled do
    if ATask.Cancelled or ShuttingDown then Exit(False);
  Result := not ATask.Cancelled and not ShuttingDown;
end;

function TMockWalletData.GetShuttingDown: Boolean;
begin
  FLock.Acquire;
  try Result := FShuttingDown; finally FLock.Release; end;
end;

procedure TMockWalletData.Shutdown;
begin
  FLock.Acquire;
  try
    FShuttingDown := true;
    FGateFetches := false;
    FFetchGate.SetEvent;
    FBalanceGate.SetEvent;
  finally FLock.Release; end;
end;

function TMockWalletData.Metrics(AKind: TMockDataKind): TMockFetchMetrics;
begin
  FLock.Acquire;
  try Result := FMetrics[AKind]; finally FLock.Release; end;
end;

procedure TMockWalletData.ResetMetrics;
begin
  FLock.Acquire;
  try
    if (FMetrics[mdkAccounts].ActiveFetches <> 0) or (FMetrics[mdkOperations].ActiveFetches <> 0) then
      raise EInvalidOperation.Create('Wait for mock fetches before resetting metrics');
    FillChar(FMetrics, SizeOf(FMetrics), 0);
  finally FLock.Release; end;
end;

procedure TMockWalletData.LogEvent(const AMessage: String);
begin
  FLock.Acquire;
  try FEvents.Add(UIntToStr(GetTickCount64) + ' ' + AMessage);
  finally FLock.Release; end;
end;

procedure TMockWalletData.DrainEvents(AOutput: TStrings);
begin
  FLock.Acquire;
  try AOutput.AddStrings(FEvents); FEvents.Clear;
  finally FLock.Release; end;
end;

procedure TMockWalletData.BeginFetch(AKind: TMockDataKind);
var LStarted: UInt64; LDelay: Integer;
begin
  FLock.Acquire;
  try
    Inc(FMetrics[AKind].FetchCount);
    Inc(FMetrics[AKind].ActiveFetches);
    FMetrics[AKind].LastStartedTick := GetTickCount64;
    LDelay := FDelayMilliseconds;
    FEvents.Add(UIntToStr(GetTickCount64) + ' FETCH ' + KindName(AKind) +
      ' #' + IntToStr(FMetrics[AKind].FetchCount) + ' revision=' + IntToStr(FRevision));
  finally FLock.Release; end;
  LStarted := GetTickCount64;
  try
    repeat
      if ShuttingDown or TThread.CheckTerminated then
        raise EAbort.Create('Mock fetch canceled');
      if (FFetchGate.WaitFor(10) = wrSignaled) and
        (GetTickCount64 - LStarted >= UInt64(LDelay)) then Break;
      Sleep(10);
    until false;
  except
    FinishFetch(AKind, -1);
    raise;
  end;
end;

procedure TMockWalletData.FinishFetch(AKind: TMockDataKind; ARevision: Integer);
begin
  FLock.Acquire;
  try
    Dec(FMetrics[AKind].ActiveFetches);
    Inc(FMetrics[AKind].CompletedCount);
    FMetrics[AKind].LastRevision := ARevision;
    FMetrics[AKind].LastCompletedTick := GetTickCount64;
    FEvents.Add(UIntToStr(GetTickCount64) + ' DONE ' + KindName(AKind) +
      ' revision=' + IntToStr(ARevision));
  finally FLock.Release; end;
end;

procedure TMockWalletData.CountRawRead(AKind: TMockDataKind);
begin
  FLock.Acquire;
  try Inc(FMetrics[AKind].RawReadCount); finally FLock.Release; end;
end;

procedure TMockWalletData.CountMembershipRead;
begin
  FLock.Acquire;
  try Inc(FMetrics[mdkOperations].MembershipReadCount); finally FLock.Release; end;
end;

procedure TMockWalletData.CountAccountRead(ABalance: Boolean);
begin
  FLock.Acquire;
  try
    if ABalance then Inc(FMetrics[mdkAccounts].BalanceAccountReadCount)
    else Inc(FMetrics[mdkAccounts].PageAccountReadCount);
  finally FLock.Release; end;
end;

procedure TMockWalletData.CountSnapshotRead;
begin
  FLock.Acquire;
  try Inc(FMetrics[mdkAccounts].SnapshotReadCount); finally FLock.Release; end;
end;

function TMockWalletData.AccountKey: TAccountKey;
begin
  Result := Default(TAccountKey);
  Result.EC_OpenSSL_NID := CT_Default_EC_OpenSSL_NID;
  SetLength(Result.x, 32); SetLength(Result.y, 32);
  Result.x[31] := 1; Result.y[31] := 2;
end;

function TMockWalletData.Account(AID: Cardinal; const ASnapshot: TMockWalletSnapshot): TAccount;
begin
  Result := Default(TAccount);
  Result.account := AID;
  Result.accountInfo.state := as_Normal;
  Result.accountInfo.accountKey := AccountKey;
  Result.balance := 10000 + AID;
  Result.name.FromString('mock revision=' + IntToStr(ASnapshot.Revision) + ' account=' + UIntToStr(AID));
end;

constructor TMockMyAccountsDataSource.CreateForModel(AOwner: TComponent; AModel: TMockWalletData);
begin
  FModel := AModel;
  inherited Create(AOwner);
end;

procedure TMockMyAccountsDataSource.BeginReadSnapshot(AIncludePending: Boolean; out AContext: TAccountReadContext);
var LSnapshot: TMockWalletSnapshot;
begin
  FModel.CountSnapshotRead;
  LSnapshot := FModel.Snapshot;
  AContext := Default(TAccountReadContext);
  AContext.StateID := IntToStr(LSnapshot.Revision) + '/' + IntToStr(LSnapshot.RowCount);
  AContext.BlockTip := 1000000 + LSnapshot.Revision;
end;

function TMockMyAccountsDataSource.BeginBalanceSnapshot(ATask: TWalletStartupTask;
  AGeneration: Int64; AIncludePending: Boolean; out AContext: TAccountReadContext): Boolean;
begin
  Result := FModel.AwaitBalanceSnapshot(ATask);
  if Result then BeginReadSnapshot(AIncludePending, AContext);
end;

procedure TMockMyAccountsDataSource.EndReadSnapshot(const AContext: TAccountReadContext);
begin
end;

function TMockMyAccountsDataSource.WalletKeys(const AContext: TAccountReadContext): TArray<TAccountKey>;
begin
  SetLength(Result, 1);
  Result[0] := FModel.AccountKey;
end;

function TMockMyAccountsDataSource.KeyAccountCount(const AKey: TAccountKey;
  const AContext: TAccountReadContext): Integer;
begin
  Result := ContextSnapshot(AContext).RowCount;
end;

function TMockMyAccountsDataSource.ReadKeyPrefix(const AKey: TAccountKey;
  ACount: Integer; const AContext: TAccountReadContext): TArray<Cardinal>;
var I: Integer;
begin
  SetLength(Result, Min(ACount, ContextSnapshot(AContext).RowCount));
  for I := 0 to High(Result) do Result[I] := I;
end;

function TMockMyAccountsDataSource.OwnershipChanges(const AContext: TAccountReadContext): TArray<TAccountOwnershipChange>;
begin
  Result := nil;
end;

function TMockMyAccountsDataSource.ReadPageAccount(AAccountNumber: Cardinal;
  const AContext: TAccountReadContext): TAccount;
var LSnapshot: TMockWalletSnapshot;
begin
  FModel.CountAccountRead(false); LSnapshot := ContextSnapshot(AContext);
  Result := FModel.Account(AAccountNumber, LSnapshot);
end;

function TMockMyAccountsDataSource.ReadBalanceAccount(AAccountNumber: Cardinal;
  const AContext: TAccountReadContext): TAccount;
begin
  FModel.CountAccountRead(true);
  Result := FModel.Account(AAccountNumber, ContextSnapshot(AContext));
end;

function TMockMyAccountsDataSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ADataTable: TDataTable): TPageFetchResult;
var LRevision: Integer;
begin
  FModel.BeginFetch(mdkAccounts);
  LRevision := -1;
  try
    Result := inherited FetchPage(AParams, ADataTable);
    if Length(ADataTable.Rows) > 0 then LRevision := TextRevision(VarToStr(ADataTable.Rows[0].Name));
  finally FModel.FinishFetch(mdkAccounts, LRevision); end;
end;

procedure TMockMyAccountsDataSource.FetchAll(const AContainer: TList<TAccount>);
begin
  FModel.CountRawRead(mdkAccounts);
  inherited;
end;

constructor TMockAccountsOperationsDataSource.CreateForModel(AOwner: TComponent; AModel: TMockWalletData);
begin
  FModel := AModel;
  FOperationsPerAccount := 1;
  inherited Create(AOwner);
end;

function TMockAccountsOperationsDataSource.FetchWalletAccounts: TArray<Cardinal>;
var I: Integer; LSnapshot: TMockWalletSnapshot;
begin
  FModel.CountMembershipRead; LSnapshot := FModel.Snapshot;
  SetLength(Result, LSnapshot.RowCount);
  for I := 0 to High(Result) do Result[I] := I;
end;

procedure TMockAccountsOperationsDataSource.FetchAll(const AContainer: TList<TOperationResume>);
var
  LSnapshot: TMockWalletSnapshot; LAccounts: TArray<Cardinal>;
  LOperation: TOperationResume; LAccount: Cardinal; I, J, LIndex: Integer;
begin
  // Observe the actual configured query input for native history-picker checks.
  FLastFetchBlockDepth := BlockDepth;
  FModel.CountRawRead(mdkOperations);
  RefreshWalletAccounts;
  LAccounts := Accounts;
  LSnapshot := FModel.Snapshot; FRawRevision := LSnapshot.Revision;
  for I := 0 to High(LAccounts) do begin
    if TThread.CheckTerminated or FModel.ShuttingDown then raise EAbort.Create('Mock history canceled');
    LAccount := LAccounts[I];
    if LAccount >= Cardinal(LSnapshot.RowCount) then Continue;
    // Dense history remains a provider fixture; selection and paging use the
    // production adapters. Other scenarios retain one operation per account.
    for J := 0 to FOperationsPerAccount - 1 do begin
      LIndex := I * FOperationsPerAccount + J;
      LOperation := Default(TOperationResume);
      LOperation.valid := true; LOperation.Block := 1000000 + LSnapshot.Revision;
      LOperation.NOpInsideBlock := LIndex; LOperation.OpType := 1;
      LOperation.time := DateTimeToUnix(Now, false) - 60;
      LOperation.AffectedAccount := LAccount; LOperation.SignerAccount := LAccount;
      LOperation.Amount := 8; LOperation.Balance := 10000 + LAccount;
      LOperation.OperationTxt := 'mock revision=' + IntToStr(LSnapshot.Revision) + ' account=' + UIntToStr(LAccount);
      if J = 0 then
        LOperation.OperationHash.FromString('mock-' + IntToStr(LSnapshot.Revision) + '-' + UIntToStr(LAccount))
      else
        LOperation.OperationHash.FromString('mock-' + IntToStr(LSnapshot.Revision) + '-' + UIntToStr(LAccount) + '-' + IntToStr(J));
      AContainer.Add(LOperation);
    end;
  end;
end;

function TMockAccountsOperationsDataSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ADataTable: TDataTable): TPageFetchResult;
begin
  FModel.BeginFetch(mdkOperations);
  try Result := inherited FetchPage(AParams, ADataTable);
  finally FModel.FinishFetch(mdkOperations, FRawRevision); end;
end;

constructor TMockWallet.CreateForModel(AOwner: TComponent; AModel: TMockWalletData);
begin
  FModel := AModel;
  inherited Create(AOwner);
end;

procedure TMockWallet.AfterConstruction;
begin
  inherited;
  // LCL runs the form's real OnCreate from AfterConstruction. Apply mock-only
  // action restrictions after it has constructed the actual grids.
  // The data is synthetic. Preserve grid selection and configuration while
  // making monetary operations unavailable in this explicitly labelled demo.
  miSendPASC.Enabled := false;
  miBuyAccount.Enabled := false;
  miChangeKey.Enabled := false;
  miChangeAccountInfo.Enabled := false;
  miEnlistAccountsForSale.Enabled := false;
  miDelistAccountsFromSale.Enabled := false;
  // Transaction/detail popups query the real node using the selected account.
  // Those actions are outside a synthetic dataset. Keep all selection, fetch,
  // paging and completion callbacks wired to the actual wallet implementation.
  AccountsGrid.OnPreparePopupMenu := nil;
  OperationsGrid.OnPreparePopupMenu := nil;
  // Count the actual grid-owned admission events independently of raw queries.
  // Do not replace the production refresh, rendering or readiness paths.
  AccountsGrid.RefreshThrottle.Add(OnAccountsRefresh);
  OperationsGrid.RefreshThrottle.Add(OnOperationsRefresh);
end;

function TMockWallet.CreateAccountsDataSource: TMyAccountsDataSource;
begin
  FAccountsSource := TMockMyAccountsDataSource.CreateForModel(Self, FModel);
  Result := FAccountsSource;
end;

function TMockWallet.CreateOperationsDataSource: TAccountsOperationsDataSource;
begin
  FOperationsSource := TMockAccountsOperationsDataSource.CreateForModel(Self, FModel);
  Result := FOperationsSource;
end;

procedure TMockWallet.OnAccountsRefresh(Sender: TObject);
begin
  Inc(FAccountRefreshCount);
  FLastAccountRefreshTick := GetTickCount64;
  FModel.LogEvent('ACCOUNTS_ADMITTED #' + IntToStr(FAccountRefreshCount));
end;

procedure TMockWallet.OnOperationsRefresh(Sender: TObject);
begin
  Inc(FOperationRefreshCount);
  FLastOperationRefreshTick := GetTickCount64;
  FModel.LogEvent('OPERATIONS_ADMITTED #' + IntToStr(FOperationRefreshCount));
end;

constructor TMockMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FModel := TMockWalletData.Create(Self);
  if SameText(SysUtils.GetEnvironmentVariable('PASCAL_WALLET_SELECTION_ONLY'),'1') then
    FModel.HoldBalanceSnapshots;
  Caption := 'MOCK DATA - PascalCoin grid refresh test (offline, disposable wallet)';
end;

destructor TMockMainForm.Destroy;
begin
  if Assigned(FModel) then FModel.Shutdown;
  inherited;
end;

function TMockMainForm.CreateWalletControl: TCTRLWallet;
begin
  Result := TMockWallet.CreateForModel(Self, FModel);
end;

function TMockMainForm.GetMockWallet: TMockWallet;
begin
  Result := WalletControl as TMockWallet;
end;

end.
