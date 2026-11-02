unit UDataSources;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


{$IFDEF FPC}
{$modeswitch nestedprocvars}
{$ENDIF}

interface

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  Classes, SysUtils, UAccounts, UNode, UBlockchain, UCrypto, UCoreObjects,
  UCommon, UMemory, UConst, UCommon.Data, UCoreUtils, UBaseTypes,
  UCommon.Collections, Generics.Collections, Generics.Defaults, syncobjs, UPCDataTypes,
  UCache, UWalletStartupTask;

type

  {$IFDEF FPC}
  TAccountKeySet = TSortedHashSet<TAccountKey>;
  {$ELSE}
  TAccountKeySet = THashSet<TAccountKey>;
  {$ENDIF}

  TAccountReadContext = record
    Node: TNode;
    SafeBox: TPCSafeBox;
    Mempool: TPCOperationsComp;
    KeyIndex: TOrderedAccountKeysList;
    BlockTip: Cardinal;
    StateID: String;
  end;

  TAccountOwnershipChange = record
    Account: Cardinal;
    NewKey: TAccountKey;
  end;

  { TAccountsDataSourceBase }

  TAccountsDataSourceBase = class(TCustomDataSource<TAccount>)
  protected
    // Captured with the raw account snapshot; warm pages retain its chain tip.
    FSnapshotBlockTip: Cardinal;
    function GetColumns: TDataColumns; override;
  public
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: TAccount; const ABindingName: String): variant; override;
  end;

  { TAccountsDataSource }

  TAccountsDataSource = class(TAccountsDataSourceBase)
  private
    FConfigLock: TCriticalSection;
    function GetIncludePending: Boolean;
    procedure SetIncludePending(AIncludePending: Boolean);
  protected
    FIncludePending: boolean;
    FKeys: TAccountKeySet;
    function GetFilterKeys: TArray<TAccountKey>;
    procedure SetFilterKeys(const AKeys: TArray<TAccountKey>);
    procedure ReadConfiguration(out AKeys: TArray<TAccountKey>; out AIncludePending: Boolean);
  public
    property IncludePending: boolean read GetIncludePending write SetIncludePending;
    property FilterKeys: TArray<TAccountKey> read GetFilterKeys write SetFilterKeys;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

  { TMyAccountsDataSource }

  TMyAccountsDataSource = class(TAccountsDataSource)
  private type
    TAccountNumbers = TArray<Cardinal>;
    TPrefixCache = TActionCache<String, TAccountNumbers>;
  private
    FPBalanceSummary : PBalanceSummary;
    FPrefixCache: TPrefixCache;
    FPrefixStateID: String;
    FPrefixContext: TAccountReadContext;
    FPrefixKey: TAccountKey;
    FPrefixCount: Integer;
    FOwnershipStateID: String;
    FOwnershipChanges: TArray<TAccountOwnershipChange>;
    FFullSnapshotBlockTip: Cardinal;
    FBalanceTask: TWalletStartupTask;
    FBalanceLock: TCriticalSection;
    FBalanceGeneration, FBalanceTaskGeneration: Int64;
    FBalanceIncludePending: Boolean;
    FBalanceReady: Boolean;
    FBalanceSummary: TBalanceSummary;
    FBalanceCompletedNotify: TThreadNotify;
    FOnBalanceSummaryCompleted: TNotifyEvent;
    FStopping: Boolean;
    procedure BalanceTaskCompleted(Sender: TObject);
    function FetchPrefix(const AKey: String): TAccountNumbers;
    function EstimatePrefix(const AValues: TAccountNumbers): SizeInt;
    function CachedPrefix(const AKey: TAccountKey; ACount: Integer;
      const AContext: TAccountReadContext): TAccountNumbers;
    function CollectMembership(const AKeys: TArray<TAccountKey>;
      const AContext: TAccountReadContext; ARequiredPrefix: Integer;
      out ATotalCount: Integer): TArray<Cardinal>;
    procedure CalculateBalance(ATask: TWalletStartupTask; AGeneration: Int64; AIncludePending: Boolean);
  protected
    procedure BeginReadSnapshot(AIncludePending: Boolean; out AContext: TAccountReadContext); virtual;
    function BeginBalanceSnapshot(ATask: TWalletStartupTask; AGeneration: Int64;
      AIncludePending: Boolean; out AContext: TAccountReadContext): Boolean; virtual;
    procedure EndReadSnapshot(const AContext: TAccountReadContext); virtual;
    function WalletKeys(const AContext: TAccountReadContext): TArray<TAccountKey>; virtual;
    function KeyAccountCount(const AKey: TAccountKey; const AContext: TAccountReadContext): Integer; virtual;
    function ReadKeyPrefix(const AKey: TAccountKey; ACount: Integer;
      const AContext: TAccountReadContext): TArray<Cardinal>; virtual;
    function OwnershipChanges(const AContext: TAccountReadContext): TArray<TAccountOwnershipChange>; virtual;
    function ReadPageAccount(AAccountNumber: Cardinal; const AContext: TAccountReadContext): TAccount; virtual;
    function ReadBalanceAccount(AAccountNumber: Cardinal; const AContext: TAccountReadContext): TAccount; virtual;
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BalancePointer : PBalanceSummary read FPBalanceSummary write FPBalanceSummary;
    // Backend/key changes invalidate totals. A view's FilterKeys change does not.
    procedure InvalidateWalletData;
    // The owning UI can pause aggregate work without waiting for its core locks.
    // Keep the last complete summary until a later request publishes new data.
    procedure CancelBalanceSummary;
    // Request the owned aggregate from the UI; completion is delivered on that
    // same thread, retaining the last complete summary during a newer request.
    function TryGetBalanceSummary(out ASummary: TBalanceSummary): Boolean;
    property OnBalanceSummaryCompleted: TNotifyEvent read FOnBalanceSummaryCompleted
      write FOnBalanceSummaryCompleted;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
  end;

  { TOperationsDataSourceBase }

  TOperationsDataSourceBase = class(TCustomDataSource<TOperationResume>)
  protected
    function GetColumns: TDataColumns; override;
  public
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: TOperationResume; const ABindingName: String): variant; override;
  end;

  { TAccountsOperationsDataSource }

  TAccountsOperationsDataSource = class(TOperationsDataSourceBase)
  private
    FConfigLock: TCriticalSection;
    FConfigRevision: UInt64;
    FBlockDepth: Cardinal;
    FAccounts: TArray<cardinal>;
    FLastFetchID : String;
    FLastFetchBlockOperations : TArray<TOperationResume>;
    FUseWalletAccounts: Boolean;
    FWalletAccountsGeneration: Int64;
    FLastWalletAccountsGeneration: Int64;
    FWalletAccounts: TArray<Cardinal>;
    function GetAccounts: TArray<Cardinal>;
    procedure SetAccounts(const AAccounts: TArray<Cardinal>);
    function GetBlockDepth: Cardinal;
    procedure SetBlockDepth(ABlockDepth: Cardinal);
    function GetUseWalletAccounts: Boolean;
    procedure SetUseWalletAccounts(AUseWalletAccounts: Boolean);
  protected
    function FetchWalletAccounts: TArray<Cardinal>; virtual;
    procedure RefreshWalletAccounts;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Accounts: TArray<cardinal> read GetAccounts write SetAccounts;
    property BlockDepth: cardinal read GetBlockDepth write SetBlockDepth;
    // Resolve all-wallet membership on the fetch worker, not on the UI thread.
    property UseWalletAccounts: Boolean read GetUseWalletAccounts write SetUseWalletAccounts;
    procedure InvalidateWalletAccounts;
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
  end;

  { TPendingOperationsDataSource }

  TPendingOperationsDataSource = class(TOperationsDataSourceBase)
  public
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
  end;

  { TOperationsDataSource }

  TOperationsDataSource = class(TOperationsDataSourceBase)
  private
    FStartBlock, FEndBlock: Cardinal;
  public
    property StartBlock : Cardinal read FStartBlock write FStartBlock;
    property EndBlock : Cardinal read FEndBlock write FEndBlock;
    procedure FetchAll(const AContainer: TList<TOperationResume>); override;
  end;

  { TBlockSummarySourceBase }

  TBlockSummarySourceBase = class(TCustomDataSource<TBlockSummary>)
  public type
    TShowHashRateAs = (hr_Kilo, hr_Mega, hr_Giga, hr_Tera);
  private
    FStart, FEnd: cardinal;
    FHashRateAs: TShowHashRateAs;
    FHashRateAverageBlocksCount: integer;
    function GetTimeSpan: TTimeSpan;
    procedure SetTimeSpan(const ASpan: TTimeSpan);
  protected
    function GetColumns: TDataColumns; override;
  public
    constructor Create(AOwner: TComponent); override;
    property TimeSpan: TTimeSpan read GetTimeSpan write SetTimeSpan;
    property StartBlock: cardinal read FStart write FStart;
    property EndBlock: cardinal read FEnd write FEnd;
    property HashRateAs: TShowHashRateAs read FHashRateAs write FHashRateAs;
    property HashRateAverageBlocksCount: integer read FHashRateAverageBlocksCount write FHashRateAverageBlocksCount;
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: TBlockSummary; const ABindingName: String): variant; override;
  end;

  { TBlockSummarySource }

  TBlockSummarySource = class(TBlockSummarySourceBase)
  public
    procedure FetchAll(const AContainer: TList<TBlockSummary>); override;
  end;

implementation

uses
  Math, UWallet, UTime, UOpTransaction, UTxMultiOperation, UOrderedList;

type
  TWalletBalanceTask = class(TWalletStartupTask)
  private
    FSource: TMyAccountsDataSource;
    FGeneration: Int64;
    FIncludePending: Boolean;
  protected
    procedure BCExecute; override;
    procedure ExecuteLoad; override;
  public
    constructor Create(ASource: TMyAccountsDataSource; AGeneration: Int64; AIncludePending: Boolean);
  end;

procedure CheckWalletQueryCancelled; inline;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then Exit;
  if TThread.CheckTerminated then Abort;
end;

function LockQueryMempool(ANode: TNode): TPCOperationsComp;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then Exit(ANode.LockMempoolRead);
  // Cooperative read admission must not briefly acquire an exclusive node lock.
  while not ANode.TryLockMempoolRead(0, Result) do begin
    CheckWalletQueryCancelled;
    Sleep(10);
  end;
  try
    CheckWalletQueryCancelled;
  except
    ANode.UnlockMempoolRead;
    raise;
  end;
end;

constructor TWalletBalanceTask.Create(ASource: TMyAccountsDataSource;
  AGeneration: Int64; AIncludePending: Boolean);
begin
  inherited Create;
  FSource := ASource;
  FGeneration := AGeneration;
  FIncludePending := AIncludePending;
end;

procedure TWalletBalanceTask.ExecuteLoad;
begin
  FSource.CalculateBalance(Self, FGeneration, FIncludePending);
end;

procedure TWalletBalanceTask.BCExecute;
begin
  inherited BCExecute;
  // Queue the source receiver, not a callback on the task being retired. The
  // source joins its computed worker before publishing completion to controls.
  // Cancellation also retires the owned task. Otherwise its assigned reference
  // would prevent a later generation from starting after synchronization ends.
  FSource.FBalanceCompletedNotify.Invoke(FSource, FSource.BalanceTaskCompleted);
end;

{ TAccountsDataSourceBase }

function TAccountsDataSourceBase.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('AccountNumber', True),
    TDataColumn.From('Account'),
    TDataColumn.From('Name'),
    TDataColumn.From('Display'),
    TDataColumn.From('Balance'),
    TDataColumn.From('BalanceDecimal'),
    TDataColumn.From('Key'),
    TDataColumn.From('Type'),
    TDataColumn.From('State'),
    TDataColumn.From('Price'),
    TDataColumn.From('PriceDecimal'),
    TDataColumn.From('LockedUntil'),
    TDataColumn.From('NOperation'),
    TDataColumn.From('LastUpdatedBlock'),
    TDataColumn.From('IsPending')
  );
end;

function TAccountsDataSourceBase.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: TAccount; const ABindingName: String): variant;
var
  index: integer;
begin
  CheckWalletQueryCancelled;
  if ABindingName = 'AccountNumber' then
    Result := AItem.account
  else if ABindingName = 'Account' then
    Result := AItem.AccountString
  else if ABindingName = 'Name' then
    Result := AItem.Name.ToPrintable
  else if ABindingName = 'Display' then
    Result := AItem.DisplayString
  else if ABindingName = 'Balance' then
    Result := AItem.Balance
  else if ABindingName = 'BalanceDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Balance)
  else if ABindingName = 'Key' then
    Result := TAccountComp.AccountPublicKeyExport(AItem.accountInfo.accountKey)
  else if ABindingName = 'Type' then
    Result := AItem.account_type
  else if ABindingName = 'State' then
    Result := AItem.accountInfo.state
  else if ABindingName = 'Price' then
    Result := AItem.accountInfo.price
  else if ABindingName = 'PriceDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.accountInfo.price)
  else if ABindingName = 'LockedUntil' then
    Result := AItem.accountInfo.locked_until_block
  else if ABindingName = 'NOperation' then
    Result := AItem.n_operation
  else if ABindingName = 'LastUpdatedBlock' then
    Result := AItem.GetLastUpdatedBlock
  else if ABindingName = 'IsPending' then
    Result := AItem.GetLastUpdatedBlock > FSnapshotBlockTip
  else
    raise Exception.Create(Format('Field not found "%s"', [ABindingName]));
end;

function CreateAccountKeySet: TAccountKeySet;
begin
  {$IFDEF FPC}
  Result := TAccountKeySet.Create(TAccountKeyComparer.Create, TAccountKeyEqualityComparer.Create);
  {$ELSE}
  Result := TAccountKeySet.Create(TAccountKeyEqualityComparer.Create);
  {$ENDIF}
end;
{ TAccountsDataSource }

constructor TAccountsDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfigLock := TCriticalSection.Create;
  FKeys := CreateAccountKeySet;
end;

destructor TAccountsDataSource.Destroy;
begin
  FKeys.Free;
  FConfigLock.Free;
  inherited Destroy;
end;

function TAccountsDataSource.GetFilterKeys: TArray<TAccountKey>;
begin
  FConfigLock.Acquire;
  try
    Result := FKeys.ToArray;
  finally
    FConfigLock.Release;
  end;
end;

procedure TAccountsDataSource.SetFilterKeys(const AKeys: TArray<TAccountKey>);
var
  i: integer;
  LChanged: Boolean;
begin
  FConfigLock.Acquire;
  try
    LChanged := FKeys.Count <> Length(AKeys);
    if not LChanged then
      for i := Low(AKeys) to High(AKeys) do
        if not FKeys.Contains(AKeys[i]) then begin
          LChanged := True;
          Break;
        end;
    if not LChanged then Exit;
    FKeys.Clear;
    for i := Low(AKeys) to High(AKeys) do
      FKeys.Add(AKeys[i]);
    InvalidateSnapshot;
  finally
    FConfigLock.Release;
  end;
end;

function TAccountsDataSource.GetIncludePending: Boolean;
begin
  FConfigLock.Acquire;
  try
    Result := FIncludePending;
  finally
    FConfigLock.Release;
  end;
end;

procedure TAccountsDataSource.SetIncludePending(AIncludePending: Boolean);
begin
  FConfigLock.Acquire;
  try
    if FIncludePending = AIncludePending then Exit;
    FIncludePending := AIncludePending;
    InvalidateSnapshot;
  finally
    FConfigLock.Release;
  end;
end;

procedure TAccountsDataSource.ReadConfiguration(out AKeys: TArray<TAccountKey>; out AIncludePending: Boolean);
begin
  FConfigLock.Acquire;
  try
    AKeys := FKeys.ToArray;
    AIncludePending := FIncludePending;
  finally
    FConfigLock.Release;
  end;
end;

procedure TAccountsDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i: integer;
  LAcc: TAccount;
  LNode: TNode;
  LSafeBox: TPCSafeBox;
  LMemPool : TPCOperationsComp;
  LKeys: TArray<TAccountKey>;
  LIncludePending: Boolean;
  LKeySet: TAccountKeySet;
  LDisposables: TDisposables;
begin
  ReadConfiguration(LKeys, LIncludePending);
  LKeySet := LDisposables.AddObject(CreateAccountKeySet) as TAccountKeySet;
  LKeySet.AddRange(LKeys);
  LNode := TNode.Node;
  LSafeBox := LNode.Bank.SafeBox;
  LMemPool := nil;
  if LIncludePending then LMemPool := LockQueryMempool(LNode);
  try
    LSafeBox.StartThreadSafe;
    try
      if LSafeBox.BlocksCount > 0 then
        FSnapshotBlockTip := LSafeBox.BlocksCount - 1
      else FSnapshotBlockTip := 0;
      // Filter while streaming. A small key selection must not allocate a
      // second array containing every account in the entire safebox.
      for i := 0 to LSafeBox.AccountsCount - 1 do begin
        CheckWalletQueryCancelled;
        if LIncludePending then
          LAcc := LMemPool.SafeBoxTransaction.Account(i)
        else
          LAcc := LSafeBox.Account(i);
        if (LKeySet.Count = 0) or LKeySet.Contains(LAcc.accountInfo.accountKey) then
          AContainer.Add(LAcc);
      end;
    finally
      LSafeBox.EndThreadSave;
    end;
  finally
    if LIncludePending then LNode.UnlockMempoolRead;
  end;
end;

{ TMyAccountsDataSource }

constructor TMyAccountsDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBalanceLock := TCriticalSection.Create;
  FBalanceCompletedNotify := TThreadNotify.Create(TThread.CurrentThread);
  FBalanceTaskGeneration := -1;
end;

destructor TMyAccountsDataSource.Destroy;
begin
  FStopping := True;
  FOnBalanceSummaryCompleted := nil;
  if Assigned(FBalanceTask) then FBalanceTask.CancelAndWait;
  FreeAndNil(FBalanceTask);
  // Producers are joined before the owned dispatcher removes queued callbacks.
  FBalanceCompletedNotify.Free;
  FPrefixCache.Free;
  FBalanceLock.Free;
  inherited Destroy;
end;

procedure TMyAccountsDataSource.BalanceTaskCompleted(Sender: TObject);
begin
  if FStopping then Exit;
  FreeAndNil(FBalanceTask);
  if Assigned(FOnBalanceSummaryCompleted) then FOnBalanceSummaryCompleted(Self);
end;

procedure TMyAccountsDataSource.BeginReadSnapshot(AIncludePending: Boolean;
  out AContext: TAccountReadContext);
begin
  AContext := Default(TAccountReadContext);
  AContext.Node := TNode.Node;
  AContext.SafeBox := AContext.Node.Bank.SafeBox;
  CheckWalletQueryCancelled;
  if AIncludePending then AContext.Mempool := LockQueryMempool(AContext.Node);
  try
    AContext.SafeBox.StartThreadSafe;
    try
      AContext.KeyIndex := TWallet.Keys.AccountsKeyList;
      AContext.KeyIndex.Lock;
      try
        if AContext.SafeBox.BlocksCount > 0 then
          AContext.BlockTip := AContext.SafeBox.BlocksCount - 1;
        AContext.StateID := TCrypto.ToHexaString(AContext.SafeBox.SafeBoxHash);
        if Assigned(AContext.Mempool) then
          AContext.StateID := AContext.StateID + ':' +
            TCrypto.ToHexaString(AContext.Mempool.OperationsHashTree.HashTree);
      except
        AContext.KeyIndex.Unlock;
        raise;
      end;
    except
      AContext.SafeBox.EndThreadSave;
      raise;
    end;
  except
    if AIncludePending then AContext.Node.UnlockMempoolRead;
    raise;
  end;
end;

procedure TMyAccountsDataSource.EndReadSnapshot(const AContext: TAccountReadContext);
begin
  AContext.KeyIndex.Unlock;
  AContext.SafeBox.EndThreadSave;
  if Assigned(AContext.Mempool) then AContext.Node.UnlockMempoolRead;
end;

function TMyAccountsDataSource.WalletKeys(const AContext: TAccountReadContext): TArray<TAccountKey>;
begin
  Result := TArray<TAccountKey>(AContext.KeyIndex.ToArray);
end;

function TMyAccountsDataSource.KeyAccountCount(const AKey: TAccountKey;
  const AContext: TAccountReadContext): Integer;
var LNumbers: TAccountsNumbersList;
begin
  LNumbers := AContext.KeyIndex.GetAccountsUsingThisKey(AKey);
  if Assigned(LNumbers) then Result := LNumbers.Count else Result := 0;
end;

function TMyAccountsDataSource.ReadKeyPrefix(const AKey: TAccountKey;
  ACount: Integer; const AContext: TAccountReadContext): TArray<Cardinal>;
var LNumbers: TAccountsNumbersList; LList: TList<Cardinal>; LScope: TDisposables;
begin
  Result := nil;
  LNumbers := AContext.KeyIndex.GetAccountsUsingThisKey(AKey);
  if not Assigned(LNumbers) then Exit;
  LList := LScope.AddObject(TList<Cardinal>.Create) as TList<Cardinal>;
  LNumbers.FillList(0, Min(ACount, LNumbers.Count), LList);
  Result := LList.ToArray;
end;

function TMyAccountsDataSource.ReadPageAccount(AAccountNumber: Cardinal;
  const AContext: TAccountReadContext): TAccount;
begin
  if Assigned(AContext.Mempool) then
    Result := AContext.Mempool.SafeBoxTransaction.Account(AAccountNumber)
  else
    Result := AContext.SafeBox.Account(AAccountNumber);
end;

function TMyAccountsDataSource.ReadBalanceAccount(AAccountNumber: Cardinal;
  const AContext: TAccountReadContext): TAccount;
begin
  if Assigned(AContext.Mempool) then
    Result := AContext.Mempool.SafeBoxTransaction.Account(AAccountNumber)
  else
    Result := AContext.SafeBox.Account(AAccountNumber);
end;

function TMyAccountsDataSource.EstimatePrefix(const AValues: TAccountNumbers): SizeInt;
begin
  Result := SizeInt(Length(AValues)) * SizeOf(Cardinal);
end;

function TMyAccountsDataSource.FetchPrefix(const AKey: String): TAccountNumbers;
begin
  Result := ReadKeyPrefix(FPrefixKey, FPrefixCount, FPrefixContext);
end;

function TMyAccountsDataSource.CachedPrefix(const AKey: TAccountKey;
  ACount: Integer; const AContext: TAccountReadContext): TAccountNumbers;
var LKey: String; LTarget: Integer;
begin
  if not Assigned(FPrefixCache) then
    FPrefixCache := TPrefixCache.Create(nil,
      TCacheValueFetcher<String, TAccountNumbers>.FromObjectFunction(FetchPrefix),
      TCacheSizeEstimator<TAccountNumbers>.FromObjectFunction(EstimatePrefix),
      idpNone, TCacheReapPolicy.crpOldest,
      TCacheExpirationPolicy.epNone,
      TCacheNullValuePolicy.nvpCacheNormally, 64*1024*1024, TTimeSpan.MaxValue);
  if FPrefixStateID <> AContext.StateID then begin
    FPrefixCache.Flush;
    FPrefixStateID := AContext.StateID;
  end;
  LKey := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(AKey));
  if FPrefixCache.ContainsCachedItem(LKey) then begin
    Result := FPrefixCache[LKey];
    if Length(Result) >= ACount then Exit;
    FPrefixCache.Remove(LKey);
  end;
  // Grow geometrically so walking consecutive pages does not repeatedly scan
  // a B-tree prefix for every individual page. Account records are not loaded.
  LTarget := 32;
  while (LTarget < ACount) and (LTarget <= High(Integer) div 2) do LTarget := LTarget * 2;
  if LTarget < ACount then LTarget := ACount;
  FPrefixKey := AKey;
  FPrefixCount := LTarget;
  FPrefixContext := AContext;
  if SizeInt(LTarget) * SizeOf(Cardinal) > FPrefixCache.MaxCapacity then
    Result := ReadKeyPrefix(AKey, ACount, AContext)
  else
    Result := FPrefixCache[LKey];
end;

function TMyAccountsDataSource.OwnershipChanges(const AContext: TAccountReadContext): TArray<TAccountOwnershipChange>;
var
  LOwners: TDictionary<Cardinal,TAccountKey>;
  LPair: TPair<Cardinal,TAccountKey>;
  LOperation: TPCOperation;
  LChanges: TMultiOpChangesInfo;
  LScope: TDisposables;
  i, j: Integer;
begin
  Result := nil;
  if not Assigned(AContext.Mempool) then Exit;
  if FOwnershipStateID = AContext.StateID then Exit(FOwnershipChanges);
  LOwners := LScope.AddObject(TDictionary<Cardinal,TAccountKey>.Create) as TDictionary<Cardinal,TAccountKey>;
  // Read ownership metadata, not hundreds of thousands of account records.
  // Applied pending operations expose their final recipient/new-key data.
  for i := 0 to AContext.Mempool.OperationsHashTree.OperationsCount - 1 do begin
    CheckWalletQueryCancelled;
    LOperation := AContext.Mempool.OperationsHashTree.GetOperation(i);
    if LOperation is TOpChangeKey then
      LOwners.AddOrSetValue(TOpChangeKey(LOperation).Data.account_target, TOpChangeKey(LOperation).Data.new_accountkey)
    else if LOperation is TOpRecoverFounds then
      LOwners.AddOrSetValue(TOpRecoverFounds(LOperation).Data.account, TOpRecoverFounds(LOperation).Data.new_accountkey)
    else if LOperation is TOpChangeAccountInfo then begin
      if public_key in TOpChangeAccountInfo(LOperation).Data.changes_type then
        LOwners.AddOrSetValue(TOpChangeAccountInfo(LOperation).Data.account_target,
          TOpChangeAccountInfo(LOperation).Data.new_accountkey);
    end else if LOperation is TOpTransaction then begin
      if TOpTransaction(LOperation).Data.opTransactionStyle <> transaction then
        LOwners.AddOrSetValue(TOpTransaction(LOperation).Data.target,
          TOpTransaction(LOperation).Data.new_accountkey);
    end else if LOperation is TOpMultiOperation then begin
      LChanges := TOpMultiOperation(LOperation).Data.changesInfo;
      for j := 0 to High(LChanges) do begin
        CheckWalletQueryCancelled;
        if public_key in LChanges[j].Changes_type then
          LOwners.AddOrSetValue(LChanges[j].Account, LChanges[j].New_Accountkey);
      end;
    end;
  end;
  SetLength(Result, LOwners.Count);
  i := 0;
  for LPair in LOwners do begin
    CheckWalletQueryCancelled;
    Result[i].Account := LPair.Key;
    Result[i].NewKey := LPair.Value;
    // Some legacy automatic-swap/recovery encodings omit the final key. Only
    // those exceptional changed accounts need a pending account lookup.
    if Result[i].NewKey.EC_OpenSSL_NID = 0 then
      Result[i].NewKey := AContext.Mempool.SafeBoxTransaction.Account(LPair.Key).accountInfo.accountKey;
    Inc(i);
  end;
  CheckWalletQueryCancelled;
  FOwnershipChanges := Result;
  FOwnershipStateID := AContext.StateID;
end;

function TMyAccountsDataSource.CollectMembership(const AKeys: TArray<TAccountKey>;
  const AContext: TAccountReadContext; ARequiredPrefix: Integer;
  out ATotalCount: Integer): TArray<Cardinal>;
var
  LKeys: TArray<TAccountKey>;
  LKeySet: TAccountKeySet;
  LChanges: TArray<TAccountOwnershipChange>;
  LValues: TList<Cardinal>;
  LMembers: TDictionary<Cardinal,Byte>;
  LNumbers: TArray<Cardinal>;
  LKey: TAccountKey;
  LNumber: Cardinal;
  LCount, LNeeded, i: Integer;
  LScope: TDisposables;
begin
  LKeys := AKeys;
  if Length(LKeys) = 0 then LKeys := WalletKeys(AContext);
  LKeySet := LScope.AddObject(CreateAccountKeySet) as TAccountKeySet;
  LKeySet.AddRange(LKeys);
  LValues := LScope.AddObject(TList<Cardinal>.Create) as TList<Cardinal>;
  LChanges := OwnershipChanges(AContext);
  ATotalCount := 0;
  if Length(LChanges) = 0 then begin
    for LKey in LKeySet do begin
      CheckWalletQueryCancelled;
      LCount := KeyAccountCount(LKey, AContext);
      Inc(ATotalCount, LCount);
      LNeeded := LCount;
      if ARequiredPrefix >= 0 then LNeeded := Min(LNeeded, ARequiredPrefix);
      if LNeeded > 0 then begin
        if ARequiredPrefix < 0 then LNumbers := ReadKeyPrefix(LKey, LNeeded, AContext)
        else LNumbers := CachedPrefix(LKey, LNeeded, AContext);
        LValues.AddRange(LNumbers);
      end;
    end;
  end else begin
    // Ownership changes can add/remove accounts without changing the count.
    // Correct the membership using IDs only; even this conservative path never
    // reads unrelated wallet account records before populating a page.
    LMembers := LScope.AddObject(TDictionary<Cardinal,Byte>.Create) as TDictionary<Cardinal,Byte>;
    for LKey in LKeySet do begin
      CheckWalletQueryCancelled;
      LNumbers := ReadKeyPrefix(LKey, KeyAccountCount(LKey, AContext), AContext);
      for LNumber in LNumbers do begin
        CheckWalletQueryCancelled;
        LMembers.AddOrSetValue(LNumber, 0);
      end;
    end;
    for i := 0 to High(LChanges) do begin
      CheckWalletQueryCancelled;
      if LKeySet.Contains(LChanges[i].NewKey) then
        LMembers.AddOrSetValue(LChanges[i].Account, 0)
      else
        LMembers.Remove(LChanges[i].Account);
    end;
    for LNumber in LMembers.Keys do begin
      CheckWalletQueryCancelled;
      LValues.Add(LNumber);
    end;
    ATotalCount := LValues.Count;
  end;
  CheckWalletQueryCancelled;
  LValues.Sort;
  Result := LValues.ToArray;
  if (ARequiredPrefix >= 0) and (Length(Result) > ARequiredPrefix) then
    SetLength(Result, ARequiredPrefix);
end;

function TMyAccountsDataSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ADataTable: TDataTable): TPageFetchResult;
var
  LFilter: TColumnFilter;
  LContext: TAccountReadContext;
  LKeys: TArray<TAccountKey>;
  LIncludePending, LFast: Boolean;
  LNumbers: TArray<Cardinal>;
  LRows: TArray<TAccount>;
  LTotal, LStart, LTake, LRequired, i: Integer;
  LRequested: Int64;
begin
  LFast := AParams.PageSize > 0;
  for LFilter in AParams.Filter do
    if (LFilter.Filter <> vgfSortable) or
      ((LFilter.Sort <> sdNone) and
       ((LFilter.ColumnName <> 'AccountNumber') or (LFilter.Sort <> sdAscending))) then
      LFast := False;
  if not LFast then begin
    EnterLock;
    try
      // The full query cache carries a different snapshot than a later fast
      // page. Its pending-color comparison must retain its own captured tip.
      FSnapshotBlockTip := FFullSnapshotBlockTip;
      Result := inherited FetchPage(AParams, ADataTable);
    finally
      ReleaseLock;
    end;
    Exit;
  end;
  EnterLock;
  try
    ReadConfiguration(LKeys, LIncludePending);
    LRequested := (Int64(Max(AParams.PageIndex, 0)) + 1) * AParams.PageSize;
    if LRequested > High(Integer) then LRequired := High(Integer) else LRequired := LRequested;
    BeginReadSnapshot(LIncludePending, LContext);
    try
      FSnapshotBlockTip := LContext.BlockTip;
      LNumbers := CollectMembership(LKeys, LContext, LRequired, LTotal);
      Result.TotalDataCount := LTotal;
      if LTotal = 0 then begin
        Result.PageCount := 0;
        Result.PageIndex := -1;
        LTake := 0;
        LStart := 0;
      end else begin
        Result.PageCount := ((LTotal - 1) div AParams.PageSize) + 1;
        Result.PageIndex := ClipValue(AParams.PageIndex, 0, Result.PageCount - 1);
        LStart := Result.PageIndex * AParams.PageSize;
        LTake := Min(AParams.PageSize, LTotal - LStart);
      end;
      SetLength(LRows, LTake);
      for i := 0 to LTake - 1 do begin
        CheckWalletQueryCancelled;
        LRows[i] := ReadPageAccount(LNumbers[LStart + i], LContext);
      end;
    finally
      EndReadSnapshot(LContext);
    end;
    ADataTable.Columns := GetColumns;
    SetLength(ADataTable.Rows, LTake);
    for i := 0 to LTake - 1 do begin
      CheckWalletQueryCancelled;
      ADataTable.Rows[i] := TDataRow.New(PtrInt(ClassInfo), ADataTable.Columns);
      DehydrateItem(LRows[i], ADataTable.Rows[i]);
    end;
  finally
    ReleaseLock;
  end;
end;

procedure TMyAccountsDataSource.FetchAll(const AContainer: TList<TAccount>);
var
  i, LTotal: integer;
  LContext: TAccountReadContext;
  LNumbers: TArray<Cardinal>;
  LKeys: TArray<TAccountKey>;
  LIncludePending: Boolean;
begin
  ReadConfiguration(LKeys, LIncludePending);
  BeginReadSnapshot(LIncludePending, LContext);
  try
    FSnapshotBlockTip := LContext.BlockTip;
    FFullSnapshotBlockTip := LContext.BlockTip;
    LNumbers := CollectMembership(LKeys, LContext, -1, LTotal);
    for i := 0 to High(LNumbers) do begin
      CheckWalletQueryCancelled;
      AContainer.Add(ReadPageAccount(LNumbers[i], LContext));
    end;
  finally
    EndReadSnapshot(LContext);
  end;
end;

procedure TMyAccountsDataSource.InvalidateWalletData;
begin
  InterlockedIncrement64(FBalanceGeneration);
  InvalidateSnapshot;
end;

procedure TMyAccountsDataSource.CancelBalanceSummary;
begin
  InterlockedIncrement64(FBalanceGeneration);
  if Assigned(FBalanceTask) then FBalanceTask.Cancel;
end;

function TMyAccountsDataSource.TryGetBalanceSummary(out ASummary: TBalanceSummary): Boolean;
var LGeneration: Int64; LIncludePending: Boolean;
begin
  LIncludePending := IncludePending;
  if FBalanceIncludePending <> LIncludePending then begin
    FBalanceIncludePending := LIncludePending;
    InterlockedIncrement64(FBalanceGeneration);
  end;
  LGeneration := InterlockedCompareExchange64(FBalanceGeneration, 0, 0);
  if (not Assigned(FBalanceTask)) and (FBalanceTaskGeneration <> LGeneration) then begin
    FBalanceTaskGeneration := LGeneration;
    FBalanceTask := TWalletBalanceTask.Create(Self, LGeneration, LIncludePending);
    FBalanceTask.Start;
  end;
  FBalanceLock.Acquire;
  try
    Result := FBalanceReady;
    ASummary := FBalanceSummary;
  finally
    FBalanceLock.Release;
  end;
  // Keep the legacy pointer usable, but write it only from this UI request, not
  // from a worker that could outlive the form owning the pointed-to record.
  if Result and Assigned(FPBalanceSummary) then FPBalanceSummary^ := ASummary;
end;

function TMyAccountsDataSource.BeginBalanceSnapshot(ATask: TWalletStartupTask;
  AGeneration: Int64; AIncludePending: Boolean; out AContext: TAccountReadContext): Boolean;
var LNode: TNode; LMempool: TPCOperationsComp;
begin
  Result := False;
  LNode := TNode.Node;
  // A pending-buffer save can retain the mempool. Cancel promptly instead of
  // blocking a form's owned aggregate worker in an unconditional acquisition.
  while not LNode.TryLockMempoolRead(0, LMempool) do begin
    if ATask.Cancelled or
      (InterlockedCompareExchange64(FBalanceGeneration, 0, 0) <> AGeneration) then Exit;
    Sleep(10);
  end;
  try
    if ATask.Cancelled or
      (InterlockedCompareExchange64(FBalanceGeneration, 0, 0) <> AGeneration) then Exit;
    BeginReadSnapshot(AIncludePending, AContext);
    Result := True;
  finally
    LNode.UnlockMempoolRead;
  end;
end;

procedure TMyAccountsDataSource.CalculateBalance(ATask: TWalletStartupTask;
  AGeneration: Int64; AIncludePending: Boolean);
const CChunkSize = 128;
var
  LContext: TAccountReadContext;
  LNumbers: TArray<Cardinal>;
  LStateID: String;
  LSummary: TBalanceSummary;
  LAccount: TAccount;
  LTotal, i, j, LLast: Integer;
begin
  if not BeginBalanceSnapshot(ATask, AGeneration, AIncludePending, LContext) then Exit;
  try
    if ATask.Cancelled or
      (InterlockedCompareExchange64(FBalanceGeneration, 0, 0) <> AGeneration) then Exit;
    LStateID := LContext.StateID;
    LNumbers := CollectMembership(nil, LContext, -1, LTotal);
    if ATask.Cancelled or
      (InterlockedCompareExchange64(FBalanceGeneration, 0, 0) <> AGeneration) then Exit;
  finally
    EndReadSnapshot(LContext);
  end;
  LSummary := CT_BalanceSummary_Nil;
  LSummary.TotalPASA := Length(LNumbers);
  i := 0;
  while i < Length(LNumbers) do begin
    if not BeginBalanceSnapshot(ATask, AGeneration, AIncludePending, LContext) then Exit;
    try
      if LContext.StateID <> LStateID then Exit;
      LLast := Min(i + CChunkSize - 1, High(LNumbers));
      for j := i to LLast do begin
        if ATask.Cancelled or
          (InterlockedCompareExchange64(FBalanceGeneration, 0, 0) <> AGeneration) then Exit;
        LAccount := ReadBalanceAccount(LNumbers[j], LContext);
        Inc(LSummary.TotalPASC, LAccount.balance);
      end;
    finally
      EndReadSnapshot(LContext);
    end;
    i := LLast + 1;
    // Yield between short, coherent chunks instead of retaining core locks
    // while reading every account for the global Overview total.
    Sleep(0);
  end;
  // Recheck the actual core state immediately before publication too: a quiet
  // debounce can leave the UI generation unchanged while new blocks arrive.
  if not BeginBalanceSnapshot(ATask, AGeneration, AIncludePending, LContext) then Exit;
  try
    if LContext.StateID <> LStateID then Exit;
    FBalanceLock.Acquire;
    try
      if ATask.Cancelled or
        (InterlockedCompareExchange64(FBalanceGeneration, 0, 0) <> AGeneration) then Exit;
      FBalanceSummary := LSummary;
      FBalanceReady := True;
    finally
      FBalanceLock.Release;
    end;
  finally
    EndReadSnapshot(LContext);
  end;
end;

{ TOperationsDataSourceBase }

function TOperationsDataSourceBase.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(
    TDataColumn.From('OPHASH', True),
    TDataColumn.From('UnixTime'),
    TDataColumn.From('Time'),
    TDataColumn.From('Block'),
    TDataColumn.From('Index'),
    TDataColumn.From('BlockLocation'),
    TDataColumn.From('BlockLocationSortable'),
    TDataColumn.From('Account'),
    TDataColumn.From('AccountNumber'),
    TDataColumn.From('Type'),
    TDataColumn.From('SubType'),
    TDataColumn.From('Amount'),
    TDataColumn.From('AmountDecimal'),
    TDataColumn.From('Fee'),
    TDataColumn.From('FeeDecimal'),
    TDataColumn.From('Balance'),
    TDataColumn.From('BalanceDecimal'),
    TDataColumn.From('Payload'),
    TDataColumn.From('Description')
    );
end;

function TOperationsDataSourceBase.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: TOperationResume; const ABindingName: String): variant;
var
  index: integer;
begin
  CheckWalletQueryCancelled;
  if ABindingName = 'OPHASH' then
    Result := TPCOperation.OperationHashAsHexa(AItem.OperationHash)
  else if ABindingName = 'UnixTime' then
    Result := AItem.Time
  else if ABindingName = 'Time' then
    Result := UnixTimeToLocalStr(AItem.time)
  else if ABindingName = 'Block' then
    Result := AItem.Block
  else if ABindingName = 'Index' then
    Result := AItem.NOpInsideBlock
  else if ABindingName = 'BlockLocation' then
    Result := IIF(AItem.OpType <> CT_PseudoOp_Reward, IntToStr(AItem.Block) + '/' + IntToStr(AItem.NOpInsideBlock + 1), IntToStr(AItem.Block))
  else if ABindingName = 'BlockLocationSortable' then
  begin
    if AItem.OpType <> CT_PseudoOp_Reward then
      Result := (UInt64(AItem.Block) shl 32) + UInt32(AItem.NOpInsideBlock)
    else
      Result := UInt64(0);  // number pattern = [block][opindex]
  end
  else if ABindingName = 'Account' then
    Result := TAccountComp.AccountNumberToAccountTxtNumber(AItem.AffectedAccount)
  else if ABindingName = 'AccountNumber' then
    Result := AItem.AffectedAccount
  else if ABindingName = 'Type' then
    Result := AItem.OpType
  else if ABindingName = 'SubType' then
    Result := AItem.OpSubtype
  else if ABindingName = 'Amount' then
    Result := AItem.Amount
  else if ABindingName = 'AmountDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Amount)
  else if ABindingName = 'Fee' then
    Result := AItem.Fee
  else if ABindingName = 'FeeDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Fee)
  else if ABindingName = 'Balance' then
    Result := AItem.Balance
  else if ABindingName = 'BalanceDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Balance)
  else if ABindingName = 'Payload' then
    Result := AItem.PrintablePayload
  else if ABindingName = 'Description' then
    Result := AItem.OperationTxt
  else
    raise Exception.Create(Format('Field not found [%s]', [ABindingName]));
end;

{ TAccountsOperationsDataSource }

constructor TAccountsOperationsDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfigLock := TCriticalSection.Create;
  FLastWalletAccountsGeneration := -1;
end;

destructor TAccountsOperationsDataSource.Destroy;
begin
  FConfigLock.Free;
  inherited Destroy;
end;

function TAccountsOperationsDataSource.GetAccounts: TArray<Cardinal>;
begin
  FConfigLock.Acquire;
  try
    if FUseWalletAccounts then
      Result := TArrayTool<Cardinal>.Copy(FWalletAccounts)
    else
      Result := TArrayTool<Cardinal>.Copy(FAccounts);
  finally
    FConfigLock.Release;
  end;
end;

procedure TAccountsOperationsDataSource.SetAccounts(const AAccounts: TArray<Cardinal>);
type
  TAccountNumbers = TArray<Cardinal>;
begin
  FConfigLock.Acquire;
  try
    if TEqualityComparer<TAccountNumbers>.Default.Equals(FAccounts, AAccounts) then exit;
    FAccounts := TArrayTool<Cardinal>.Copy(AAccounts);
    Inc(FConfigRevision);
    // A different middle account must not collide with the old first/last key.
    FLastFetchID := '';
    InvalidateSnapshot;
  finally
    FConfigLock.Release;
  end;
end;

function TAccountsOperationsDataSource.GetBlockDepth: Cardinal;
begin
  FConfigLock.Acquire;
  try
    Result := FBlockDepth;
  finally
    FConfigLock.Release;
  end;
end;

procedure TAccountsOperationsDataSource.SetBlockDepth(ABlockDepth: Cardinal);
begin
  FConfigLock.Acquire;
  try
    if FBlockDepth = ABlockDepth then exit;
    FBlockDepth := ABlockDepth;
    Inc(FConfigRevision);
    FLastFetchID := '';
    InvalidateSnapshot;
  finally
    FConfigLock.Release;
  end;
end;

function TAccountsOperationsDataSource.GetUseWalletAccounts: Boolean;
begin
  FConfigLock.Acquire;
  try
    Result := FUseWalletAccounts;
  finally
    FConfigLock.Release;
  end;
end;

procedure TAccountsOperationsDataSource.SetUseWalletAccounts(AUseWalletAccounts: Boolean);
begin
  FConfigLock.Acquire;
  try
    if FUseWalletAccounts = AUseWalletAccounts then Exit;
    FUseWalletAccounts := AUseWalletAccounts;
    Inc(FConfigRevision);
    FLastFetchID := '';
    InvalidateSnapshot;
  finally
    FConfigLock.Release;
  end;
end;

procedure TAccountsOperationsDataSource.InvalidateWalletAccounts;
begin
  InterlockedIncrement64(FWalletAccountsGeneration);
  InvalidateSnapshot;
end;

function TAccountsOperationsDataSource.FetchWalletAccounts: TArray<Cardinal>;
begin
  Result := TCoreTool.GetUserAccountNumbers;
end;

procedure TAccountsOperationsDataSource.RefreshWalletAccounts;
type
  TAccountNumbers = TArray<Cardinal>;
var
  LConfigRevision: UInt64;
  LGeneration: Int64;
  LAccounts: TArray<Cardinal>;
begin
  FConfigLock.Acquire;
  try
    if not FUseWalletAccounts then Exit;
    LGeneration := InterlockedCompareExchange64(FWalletAccountsGeneration, 0, 0);
    if FLastWalletAccountsGeneration = LGeneration then Exit;
    LConfigRevision := FConfigRevision;
  finally
    FConfigLock.Release;
  end;
  // Enumeration can visit hundreds of thousands of accounts. Keep it on the
  // fetch worker and outside the short settings lock used by UI callbacks.
  LAccounts := FetchWalletAccounts;
  CheckWalletQueryCancelled;
  FConfigLock.Acquire;
  try
    if (not FUseWalletAccounts) or (FConfigRevision <> LConfigRevision) or
      (InterlockedCompareExchange64(FWalletAccountsGeneration, 0, 0) <> LGeneration) then Exit;
    if not (TEqualityComparer<TAccountNumbers>.Default.Equals(FWalletAccounts, LAccounts)) then
      FLastFetchID := '';
    FWalletAccounts := LAccounts;
    FLastWalletAccountsGeneration := LGeneration;
  finally
    FConfigLock.Release;
  end;
end;

procedure TAccountsOperationsDataSource.FetchAll(const AContainer: TList<TOperationResume>);
var
  LNode: TNode;
  LAccounts: TArray<Cardinal>;
  LRequestedBlockDepth: Cardinal;
  LConfigRevision: UInt64;
  LUseWalletAccounts: Boolean;
  LWalletGeneration: Int64;
  LBlockDepth: Cardinal;
  LFetchID, LCurrentFetchID, LCachedFetchID : String;
  LStoredOperations : TArray<TOperationResume>;

  function ComputeFetchID : String;
  begin
    // Snapshot only the chain identity under the safebox lock. Pending-state
    // reads and stored-history I/O must not retain this lock on a grid worker.
    LNode.Bank.SafeBox.StartThreadSafe;
    try
      // Keep depths unsigned: Maximum uses High(Cardinal), which cannot pass
      // through the signed ClipValue helper without becoming a negative depth.
      LBlockDepth := LRequestedBlockDepth;
      if LBlockDepth > LNode.Bank.BlocksCount then
        LBlockDepth := LNode.Bank.BlocksCount;
      Result := LRequestedBlockDepth.ToString + ':' + TCrypto.ToHexaString(LNode.Bank.SafeBox.SafeBoxHash);
    finally
      LNode.Bank.SafeBox.EndThreadSave;
    end;
  end;

begin
  // FetchPage retains the base data-source lock throughout filtering and I/O.
  // UI settings use a separate, short-held lock so a refresh cannot freeze them.
  CheckWalletQueryCancelled;
  RefreshWalletAccounts;
  FConfigLock.Acquire;
  try
    LUseWalletAccounts := FUseWalletAccounts;
    LWalletGeneration := FLastWalletAccountsGeneration;
    if LUseWalletAccounts then
      LAccounts := TArrayTool<Cardinal>.Copy(FWalletAccounts)
    else
      LAccounts := TArrayTool<Cardinal>.Copy(FAccounts);
    LRequestedBlockDepth := FBlockDepth;
    LConfigRevision := FConfigRevision;
    LCachedFetchID := FLastFetchID;
    LStoredOperations := FLastFetchBlockOperations;
  finally
    FConfigLock.Release;
  end;
  if Length(LAccounts) = 0 then
    exit;
  LNode := TNode.Node;
  if not Assigned(LNode) then
    exit;
  // The pending helper owns its mempool lock. Taking it while holding safebox
  // would invert the core operation-application lock order.
  AContainer.AddRange(LNode.GetPendingOperationsAffectingAccounts(LAccounts, 0, MaxInt));
  CheckWalletQueryCancelled;
  LFetchID := ComputeFetchID;
  if LFetchID <> LCachedFetchID then begin
    LStoredOperations := LNode.GetStoredOperationsAffectingAccounts(LAccounts, LBlockDepth, 0, MaxInt);
    CheckWalletQueryCancelled;
    LCurrentFetchID := ComputeFetchID;
    FConfigLock.Acquire;
    try
      // Publish only successful results for this unchanged settings snapshot
      // and chain. An older worker must not replace a newer selection's cache.
      if (FConfigRevision = LConfigRevision) and
        ((not LUseWalletAccounts) or
         (InterlockedCompareExchange64(FWalletAccountsGeneration, 0, 0) = LWalletGeneration)) then begin
        if LCurrentFetchID = LFetchID then begin
          FLastFetchBlockOperations := LStoredOperations;
          FLastFetchID := LFetchID;
        end else FLastFetchID := '';
      end;
    finally
      FConfigLock.Release;
    end;
  end;
  AContainer.AddRange(LStoredOperations);
  CheckWalletQueryCancelled;
end;

{ TPendingOperationsDataSource }

procedure TPendingOperationsDataSource.FetchAll(const AContainer: TList<TOperationResume>);
var
  i: integer;
  LNode: TNode;
  LOp: TPCOperation;
  LOPR: TOperationResume;
  LMemPool : TPCOperationsComp;
begin
  LNode := TNode.Node;
  if not Assigned(LNode) then
    exit;
  LMemPool := LockQueryMempool(LNode);
  try
    for i := LMemPool.Count - 1 downto 0 do
    begin
      CheckWalletQueryCancelled;
      LOp := LMemPool.OperationsHashTree.GetOperation(i);
      if TPCOperation.OperationToOperationResume(0, LOp, False, LOp.SignerAccount, LOPR) then
      begin
        LOPR.NOpInsideBlock := i;
        LOPR.Block := LNode.Bank.BlocksCount;
        LOPR.Balance := LMemPool.SafeBoxTransaction.Account(LOp.SignerAccount).balance;
        AContainer.Add(LOPR);
      end;
    end;
  finally
    LNode.UnlockMempoolRead;
  end;
end;

{ TOperationsDataSource }

procedure TOperationsDataSource.FetchAll(const AContainer: TList<TOperationResume>);
var
  block, i, j, keyIndex: integer;
  OPR: TOperationResume;
  blockOps: TPCOperationsComp;
  node: TNode;
  GC: TDisposables;

begin
  node := TNode.Node;
  if not Assigned(Node) then
    exit;
  blockOps := GC.AddObject(TPCOperationsComp.Create(nil)) as TPCOperationsComp;
  for block := FEndBlock downto FStartBlock do
  begin  /// iterate blocks correctly
    CheckWalletQueryCancelled;
    opr := CT_TOperationResume_NUL;
    if (Node.Bank.Storage.LoadBlockChainBlock(blockOps, block)) then begin
      AContainer.Add(blockOps.GetMinerRewardPseudoOperation);
      // if blockOps.Count = 0 then exit;
      for i := blockOps.Count - 1 downto 0 do begin
        CheckWalletQueryCancelled;
        if TPCOperation.OperationToOperationResume(block, blockOps.Operation[i], False, blockOps.Operation[i].SignerAccount, opr) then begin
          opr.NOpInsideBlock := i;
          opr.Block := block;
          opr.time := blockOps.OperationBlock.timestamp;
          AContainer.Add(opr);
        end; // reverse order
      end;
    end else break;
  end;
end;

{ TBlockSummarySourceBase }

function TBlockSummarySourceBase.GetTimeSpan: TTimeSpan;
begin
  Result := TTimeSpan.FromSeconds(CT_NewLineSecondsAvg * (FEnd - FStart + 1));
end;

procedure TBlockSummarySourceBase.SetTimeSpan(const ASpan: TTimeSpan);
var
  LNode: TNode;
begin
  LNode := TNode.Node;
  if not Assigned(LNode) then
    exit;
  FEnd := LNode.Bank.BlocksCount - 1;
  FStart := ClipValue(FEnd - (ASpan.TotalBlockCount + 1), 0, FEnd);
end;

function TBlockSummarySourceBase.GetColumns: TDataColumns;
var
  LHashType: string;
begin
  case HashRateAs of
    hr_Kilo: LHashType := 'Kh/s';
    hr_Mega: LHashType := 'Mh/s';
    hr_Giga: LHashType := 'Gh/s';
    hr_Tera: LHashType := 'Th/s';
    else LHashType := '?h/s';
  end;
  Result := TDataColumns.Create(
    TDataColumn.From('UnixTime'),
    TDataColumn.From('Time'),
    TDataColumn.From('Block'),
    TDataColumn.From('Ops'),
    TDataColumn.From(LHashType),
    TDataColumn.From('Volume'),
    TDataColumn.From('Reward'),
    TDataColumn.From('Fee'),
    TDataColumn.From('FeeDecimal'),
    TDataColumn.From('Target'),
    TDataColumn.From('MinerPayload'),
    TDataColumn.From('POW'),
    TDataColumn.From('SBH'),
    TDataColumn.From('Protocol'),
    TDataColumn.From('Deviation'),
    TDataColumn.From('TimeAverage')
  );
end;

constructor TBlockSummarySourceBase.Create(AOwner: TComponent);
var
  LNode: TNode;
begin
  inherited Create(AOwner);
  LNode := TNode.Node;
  if Assigned(LNode) then begin
    FStart := 0;
    FEnd := LNode.Bank.BlocksCount - 1;
  end else begin
    FStart := 0;
    FEnd := 0;
  end;
end;

function TBlockSummarySourceBase.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: TBlockSummary; const ABindingName: String): variant;
var
  LHR_Base: int64;
  LHashType: string;
  LDeviation: double;
begin
  CheckWalletQueryCancelled;
  case FHashRateAs of
    hr_Kilo: LHashType := 'Kh/s';
    hr_Mega: LHashType := 'Mh/s';
    hr_Giga: LHashType := 'Gh/s';
    hr_Tera: LHashType := 'Th/s';
    else LHashType := '?h/s';
  end;
  //if ABindingName = 'UnixTime' then
  //  Result := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime((AItem.Timestamp))))
  //else if ABindingName = 'Time' then
  //  Result := DateTimeToStr(UnivDateTime2LocalDateTime(UnixToUnivDateTime((AItem.Timestamp))))
  if ABindingName = 'UnixTime' then
    Result := AItem.Timestamp
  else if ABindingName = 'Time' then
    Result := UnixTimeToLocalStr(AItem.Timestamp)
  else if ABindingName = 'Block' then
    Result := AItem.Block
  else if ABindingName = 'Ops' then
    Result := AItem.OperationsCount
  else if ABindingName = 'Volume' then
    Result := TAccountComp.FormatMoney(AItem.Volume)
  else if ABindingName = 'Reward' then
    Result := TAccountComp.FormatMoney(AItem.Reward)
  else if ABindingName = 'Fee' then
    Result := AItem.Fee
  else if ABindingName = 'FeeDecimal' then
    Result := TAccountComp.FormatMoneyDecimal(AItem.Fee)
  else if ABindingName = 'Target' then
    Result := IntToHex(AItem.Target, 8)
  else if ABindingName = LHashType then begin
    case HashRateAs of
      hr_Kilo: LHR_Base := 1;
      hr_Mega: LHR_Base := 1000;
      hr_Giga: LHR_Base := 1000000;
      hr_Tera: LHR_Base := 1000000000;
      else
        Result := 1;
    end;
    Result := Format('%.2n (%.2n)', [AItem.HashRateKhs / LHR_Base, AItem.HashRateTargetKhs / LHR_Base]);
  end else if ABindingName = 'MinerPayload' then begin
    if TCrypto.IsHumanReadable(AItem.MinerPayload) then
      Result := AItem.MinerPayload
    else
      Result := TCrypto.ToHexaString(AItem.MinerPayload);
  end else if ABindingName = 'POW' then
    Result := TCrypto.ToHexaString(AItem.PoW)
  else if ABindingName = 'SBH' then
    Result := TCrypto.ToHexaString(AItem.SafeBoxHash)
  else if ABindingName = 'Protocol' then
    Result := IntToStr(AItem.BlockProtocolVersion) + '-' + IntToStr(AItem.BlockProtocolAvailable)
  else if ABindingName = 'Deviation' then begin
    LDeviation := ((CT_NewLineSecondsAvg - AItem.TimeAverage100) / CT_NewLineSecondsAvg) * 100;
    Result := Format('%.2f', [LDeviation]) + ' %';
  end else if ABindingName = 'TimeAverage' then
    Result := Format('200:%.1f 150:%.1f 100:%.1f 75:%.1f 50:%.1f 25:%.1f 10:%.1f', [AItem.TimeAverage200, AItem.TimeAverage150, AItem.TimeAverage100, AItem.TimeAverage75, AItem.TimeAverage50, AItem.TimeAverage25, AItem.TimeAverage10])
  else
    raise Exception.Create(Format('Field not found [%s]', [ABindingName]));
end;

{ TBlockSummarySource }

procedure TBlockSummarySource.FetchAll(const AContainer: TList<TBlockSummary>);
var
  LStart, LEnd, LIdx: cardinal;
  LOperationComp: TPCOperationsComp;
  LBlockChainData: TBlockSummary;
  LOperationBlock: TOperationBlock;
  LBigNum: TBigNum;
  LNode: TNode;
begin
  LNode := TNode.Node;
  if not Assigned(LNode) then
    Exit;
  if LNode.Bank.BlocksCount <= 0 then
    Exit;
  LEnd := EndBlock;
  LStart := StartBlock;
  LOperationComp := TPCOperationsComp.Create(nil);
  try
    LOperationComp.bank := LNode.Bank;

    for LIdx := LEnd downto LStart do begin
      CheckWalletQueryCancelled;
      LBlockChainData := CT_BlockSummary_NUL;
      LOperationBlock := LNode.Bank.SafeBox.GetBlock(LIdx).blockchainInfo;
      LBlockChainData.Block := LOperationBlock.block;
      LBlockChainData.Timestamp := LOperationBlock.timestamp;
      LBlockChainData.BlockProtocolVersion := LOperationBlock.protocol_version;
      LBlockChainData.BlockProtocolAvailable := LOperationBlock.protocol_available;
      LBlockChainData.Reward := LOperationBlock.reward;
      LBlockChainData.Fee := LOperationBlock.fee;
      LBlockChainData.Target := LOperationBlock.compact_target;
      LBlockChainData.HashRateKhs := LNode.Bank.SafeBox.CalcBlockHashRateInKhs(LBlockChainData.Block, HashRateAverageBlocksCount);
      LBigNum := TBigNum.TargetToHashRate(LOperationBlock.compact_target);
      try
        LBlockChainData.HashRateTargetKhs := LBigNum.Divide(1024).Divide(CT_NewLineSecondsAvg).Value;
      finally
        LBigNum.Free;
      end;
      LBlockChainData.MinerPayload := LOperationBlock.block_payload;
      LBlockChainData.PoW := LOperationBlock.proof_of_work;
      LBlockChainData.SafeBoxHash := LOperationBlock.initial_safe_box_hash;
      LBlockChainData.AccumulatedWork := LNode.Bank.SafeBox.GetBlock(LBlockChainData.Block).AccumulatedWork;
      if (LNode.Bank.LoadOperations(LOperationComp, LIdx)) then begin
        LBlockChainData.OperationsCount := LOperationComp.Count;
        LBlockChainData.Volume := LOperationComp.OperationsHashTree.TotalAmount + LOperationComp.OperationsHashTree.TotalFee;
      end;
      LBlockChainData.TimeAverage200 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 200);
      LBlockChainData.TimeAverage150 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 150);
      LBlockChainData.TimeAverage100 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 100);
      LBlockChainData.TimeAverage75 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 75);
      LBlockChainData.TimeAverage50 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 50);
      LBlockChainData.TimeAverage25 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 25);
      LBlockChainData.TimeAverage10 := LNode.Bank.GetTargetSecondsAverage(LBlockChainData.Block, 10);
      AContainer.Add(LBlockChainData);
    end;
  finally
    LOperationComp.Free;
  end;
end;

end.
