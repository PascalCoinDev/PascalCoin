unit UAccountSelectorData;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>.
  Distributed under the MIT software license; see LICENSE.
  Confirmed account queries and type statistics for the experimental selector.
  Controls supply immutable filters; only owned workers read the safebox. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

{ Queries confirmed accounts using owner, sale, balance and name filters.
  Caches matching account numbers for paging and calculates account-type
  totals on a consistent chain snapshot. }

{ Public types in this unit

  Type                            | Purpose
  --------------------------------+-------------------------------------------------------------------------
  EAccountSelectorSnapshotChanged | Reports that the queried chain snapshot changed during a read.
  TAccountSelectorChain           | Captures the safebox identity and chain totals for a query.
  TAccountSelectorDataSource      | Filters and pages confirmed accounts and calculates type statistics.
  TAccountSelectorFilter          | Carries owner, sale, balance and account-name query filters.
  TAccountSelectorTotal           | Pairs an account count with its total balance.
  TAccountSelectorTypeStats       | Groups account counts and balances by account-type digits.
}

interface

{$I ..\..\config.inc}

uses Classes, SysUtils, SyncObjs, Generics.Collections, UAccounts, UPCDataTypes,
  UCommon, UCommon.Data, UDataSources, UCache, UMemory, UBaseTypes;

type
  TAccountSelectorFilter = record
    MyAccounts, SpecificOwner: Boolean;
    OwnerKeys, WalletKeys: TArray<TAccountKey>;
    OnlyForSale, OnlyForPublicSale, OnlyForPrivateSaleToMe: Boolean;
    MinBalance, MaxBalance: Int64;
    Name: TRawBytes;
  end;

  TAccountSelectorChain = record
    SafeBox: TPCSafeBox;
    Blocks, Accounts: Integer;
    Balance: Int64;
    Hash: TRawBytes;
  end;

  TAccountSelectorTotal = record
    Accounts: Integer;
    Balance: Int64;
  end;

  TAccountSelectorTypeStats = record
    Chain: TAccountSelectorChain;
    Zero, Invalid: TAccountSelectorTotal;
    Digits: array[0..6,1..4,0..9] of TAccountSelectorTotal;
  end;

  EAccountSelectorSnapshotChanged = class(Exception);

  TAccountSelectorDataSource = class(TAccountsDataSourceBase)
  private type
    TQuerySnapshot = class
      Chain: TAccountSelectorChain;
      ImplicitAll: Boolean;
      Numbers: TArray<Cardinal>;
      Total: TAccountSelectorTotal;
    end;
    TSnapshotCache = TActionCache<String,TQuerySnapshot>;
  private
    FSafeBox: TPCSafeBox;
    FConfigLock, FQueryLock: TCriticalSection;
    FFilter, FBuildingFilter: TAccountSelectorFilter;
    FBuildingChain: TAccountSelectorChain;
    FGeneration: UInt64;
    FCache: TSnapshotCache;
    FSummary: TAccountSelectorTotal;
    FSummaryReady, FPublishedImplicitAll: Boolean;
    FPublishedNumbers: TArray<Cardinal>;
    function FetchSnapshot(const AKey: String): TQuerySnapshot;
    function CurrentSnapshot(out AGeneration: UInt64): TQuerySnapshot;
    procedure PublishSnapshot(ASnapshot: TQuerySnapshot; AGeneration: UInt64);
    function EstimateSnapshot(const AValue: TQuerySnapshot): SizeInt;
    function AcceptAccount(const AAccount: TAccount; const AFilter: TAccountSelectorFilter): Boolean;
    procedure ReadConfiguration(out AFilter: TAccountSelectorFilter; out AGeneration: UInt64; out ASafeBox: TPCSafeBox);
    procedure ReadContiguous(const AChain: TAccountSelectorChain; AStart, ACount: Integer; out ARows: TArray<TAccount>);
    procedure CheckChain(const AChain: TAccountSelectorChain);
  protected
    // Test backends replace storage only; filtering, paging and workers stay real.
    function ReadChain(ASafeBox: TPCSafeBox): TAccountSelectorChain; virtual;
    procedure ReadKeyNumbers(const AChain: TAccountSelectorChain; const AKey: TAccountKey; const ANumbers: TList<Cardinal>); virtual;
    procedure ReadAccounts(const AChain: TAccountSelectorChain; const ANumbers: TArray<Cardinal>; out ARows: TArray<TAccount>); virtual;
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Configure(ASafeBox: TPCSafeBox; const AFilter: TAccountSelectorFilter);
    procedure FetchAll(const AContainer: TList<TAccount>); override;
    procedure CalculateTypeStats(out AStats: TAccountSelectorTypeStats);
    function TryGetSummary(out ATotal: TAccountSelectorTotal): Boolean;
    function PublishedAccountIndex(AAccount: Cardinal): Integer;
  end;

procedure CheckAccountSelectorCancelled;

implementation

uses Math, UCrypto;

const
  SELECTOR_READ_CHUNK = 128;
  SELECTOR_SNAPSHOT_ATTEMPTS = 3;

procedure CheckAccountSelectorCancelled;
begin
  // Delphi does not permit CheckTerminated on an externally created UI thread.
  if TThread.CurrentThread.ThreadID = MainThreadID then Exit;
  if TThread.CheckTerminated then Abort;
end;

function ContainsKey(const AKeys: TArray<TAccountKey>; const AKey: TAccountKey): Boolean;
var I: Integer;
begin
  for I := 0 to High(AKeys) do
    if TAccountComp.EqualAccountKeys(AKeys[I],AKey) then Exit(True);
  Result := False;
end;

constructor TAccountSelectorDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConfigLock := TCriticalSection.Create;
  FQueryLock := TCriticalSection.Create;
  FGeneration := 1;
  FCache := TSnapshotCache.Create(Self,
    TCacheValueFetcher<String,TQuerySnapshot>.FromObjectFunction(FetchSnapshot),
    TCacheSizeEstimator<TQuerySnapshot>.FromObjectFunction(EstimateSnapshot),
    idpFree,TCacheReapPolicy.crpOldest,TCacheExpirationPolicy.epNone,
    TCacheNullValuePolicy.nvpCacheNormally,2,TTimeSpan.MaxValue);
end;

destructor TAccountSelectorDataSource.Destroy;
begin
  FCache.Free;
  FQueryLock.Free;
  FConfigLock.Free;
  inherited Destroy;
end;

procedure TAccountSelectorDataSource.Configure(ASafeBox: TPCSafeBox; const AFilter: TAccountSelectorFilter);
begin
  // Configuration never waits for the cache or a provider's storage/query lock.
  FConfigLock.Acquire;
  try
    FSafeBox := ASafeBox;
    FFilter := AFilter;
    FFilter.OwnerKeys := Copy(AFilter.OwnerKeys);
    FFilter.WalletKeys := Copy(AFilter.WalletKeys);
    FFilter.Name := Copy(AFilter.Name);
    Inc(FGeneration);
    FSummaryReady := False;
  finally
    FConfigLock.Release;
  end;
  // The optional inherited generic-query cache has its own logical revision.
  // Invalidating it is atomic and does not wait for an active provider/cache.
  InvalidateSnapshot;
end;

procedure TAccountSelectorDataSource.ReadConfiguration(out AFilter: TAccountSelectorFilter; out AGeneration: UInt64; out ASafeBox: TPCSafeBox);
begin
  FConfigLock.Acquire;
  try
    AFilter := FFilter;
    AGeneration := FGeneration;
    ASafeBox := FSafeBox;
  finally
    FConfigLock.Release;
  end;
end;

function TAccountSelectorDataSource.ReadChain(ASafeBox: TPCSafeBox): TAccountSelectorChain;
begin
  CheckAccountSelectorCancelled;
  Result := Default(TAccountSelectorChain);
  Result.SafeBox := ASafeBox;
  if not Assigned(Result.SafeBox) then Exit;
  Result.SafeBox.StartThreadSafe;
  try
    CheckAccountSelectorCancelled;
    Result.Blocks := Result.SafeBox.BlocksCount;
    Result.Accounts := Result.SafeBox.AccountsCount;
    Result.Balance := Result.SafeBox.TotalBalance;
    Result.Hash := Copy(Result.SafeBox.SafeBoxHash);
  finally
    Result.SafeBox.EndThreadSave;
  end;
end;

procedure TAccountSelectorDataSource.CheckChain(const AChain: TAccountSelectorChain);
begin
  if (AChain.SafeBox.BlocksCount <> AChain.Blocks) or
    (TBaseType.BinStrComp(AChain.SafeBox.SafeBoxHash,AChain.Hash) <> 0) then
    raise EAccountSelectorSnapshotChanged.Create('The confirmed account snapshot changed');
end;

procedure TAccountSelectorDataSource.ReadKeyNumbers(const AChain: TAccountSelectorChain; const AKey: TAccountKey; const ANumbers: TList<Cardinal>);
var LList: TAccountsNumbersList;
begin
  CheckAccountSelectorCancelled;
  ANumbers.Clear;
  if not Assigned(AChain.SafeBox) then Exit;
  AChain.SafeBox.StartThreadSafe;
  try
    CheckAccountSelectorCancelled;
    CheckChain(AChain);
    // Both safebox implementations expose their existing public-key index.
    // An absent key supplies no numbers; it never means the entire chain.
    LList := AChain.SafeBox.OrderedAccountKeysList.GetAccountsUsingThisKey(AKey);
    if Assigned(LList) then LList.FillList(0,LList.Count,ANumbers);
    CheckAccountSelectorCancelled;
  finally
    AChain.SafeBox.EndThreadSave;
  end;
end;

procedure TAccountSelectorDataSource.ReadAccounts(const AChain: TAccountSelectorChain; const ANumbers: TArray<Cardinal>; out ARows: TArray<TAccount>);
var I: Integer;
begin
  CheckAccountSelectorCancelled;
  SetLength(ARows,Length(ANumbers));
  if not Assigned(AChain.SafeBox) then Exit;
  AChain.SafeBox.StartThreadSafe;
  try
    CheckChain(AChain);
    for I := 0 to High(ANumbers) do begin
      CheckAccountSelectorCancelled;
      ARows[I] := AChain.SafeBox.Account(ANumbers[I]);
    end;
  finally
    AChain.SafeBox.EndThreadSave;
  end;
end;

procedure TAccountSelectorDataSource.ReadContiguous(const AChain: TAccountSelectorChain; AStart, ACount: Integer; out ARows: TArray<TAccount>);
var LNumbers: TArray<Cardinal>; I: Integer;
begin
  SetLength(LNumbers,ACount);
  for I := 0 to ACount-1 do LNumbers[I] := AStart+I;
  ReadAccounts(AChain,LNumbers,ARows);
end;

function TAccountSelectorDataSource.AcceptAccount(const AAccount: TAccount; const AFilter: TAccountSelectorFilter): Boolean;
begin
  Result := False;
  if AFilter.MyAccounts then begin
    if AFilter.SpecificOwner then begin
      if not ContainsKey(AFilter.OwnerKeys,AAccount.accountInfo.accountKey) then Exit;
    end else if not AFilter.OnlyForPrivateSaleToMe then
      if not ContainsKey(AFilter.OwnerKeys,AAccount.accountInfo.accountKey) then Exit;
  end;
  if AFilter.OnlyForSale and not TAccountComp.IsAccountForSaleOrSwap(AAccount.accountInfo) then Exit;
  if AFilter.OnlyForPublicSale and not TAccountComp.IsAccountForPublicSale(AAccount.accountInfo) then Exit;
  if AFilter.OnlyForPrivateSaleToMe then
    if not ((TAccountComp.IsAccountForPrivateSale(AAccount.accountInfo) or
      TAccountComp.IsAccountForAccountSwap(AAccount.accountInfo)) and
      ContainsKey(AFilter.WalletKeys,AAccount.accountInfo.new_publicKey)) then Exit;
  if AAccount.balance < UInt64(Max(0,AFilter.MinBalance)) then Exit;
  if (AFilter.MaxBalance >= 0) and (AAccount.balance > UInt64(AFilter.MaxBalance)) then Exit;
  // FindIn is zero-based: a match at the start of the name is valid.
  if (Length(AFilter.Name) > 0) and (TBaseType.FindIn(AFilter.Name,AAccount.name) < 0) then Exit;
  Result := True;
end;

function TAccountSelectorDataSource.EstimateSnapshot(const AValue: TQuerySnapshot): SizeInt;
begin
  Result := 1;
end;

function TAccountSelectorDataSource.CurrentSnapshot(out AGeneration: UInt64): TQuerySnapshot;
var LKey: String; LSafeBox: TPCSafeBox;
begin
  ReadConfiguration(FBuildingFilter,AGeneration,LSafeBox);
  FBuildingChain := ReadChain(LSafeBox);
  LKey := UIntToStr(AGeneration)+':'+TCrypto.ToHexaString(FBuildingChain.Hash);
  Result := FCache[LKey];
end;

procedure TAccountSelectorDataSource.PublishSnapshot(ASnapshot: TQuerySnapshot; AGeneration: UInt64);
begin
  FConfigLock.Acquire;
  try
    if AGeneration = FGeneration then begin
      FSummary := ASnapshot.Total;
      FPublishedNumbers := ASnapshot.Numbers;
      FPublishedImplicitAll := ASnapshot.ImplicitAll;
      FSummaryReady := True;
    end;
  finally
    FConfigLock.Release;
  end;
end;

function TAccountSelectorDataSource.FetchSnapshot(const AKey: String): TQuerySnapshot;
var LCandidates,LPart,LMatches: TList<Cardinal>; LRows: TArray<TAccount>;
  LNumbers: TArray<Cardinal>; LScanAll: Boolean; I,J,LStart,LTake,LUnique: Integer;
  LOwned: TDisposables;
begin
  Result := TQuerySnapshot.Create;
  try
    Result.Chain := FBuildingChain;
    LScanAll := not FBuildingFilter.MyAccounts or
      (not FBuildingFilter.SpecificOwner and FBuildingFilter.OnlyForPrivateSaleToMe);
    if FBuildingFilter.MyAccounts and (Length(FBuildingFilter.OwnerKeys) = 0) then Exit;
    if FBuildingFilter.OnlyForPrivateSaleToMe and (Length(FBuildingFilter.WalletKeys) = 0) then Exit;
    if LScanAll and not FBuildingFilter.OnlyForSale and
      not FBuildingFilter.OnlyForPublicSale and not FBuildingFilter.OnlyForPrivateSaleToMe and
      (FBuildingFilter.MinBalance <= 0) and (FBuildingFilter.MaxBalance < 0) and
      (Length(FBuildingFilter.Name) = 0) then begin
      // Unfiltered global pages use consecutive IDs and O(1) chain totals.
      Result.ImplicitAll := True;
      Result.Total.Accounts := FBuildingChain.Accounts;
      Result.Total.Balance := FBuildingChain.Balance;
      Exit;
    end;
    LCandidates := LOwned.AddObject(TList<Cardinal>.Create) as TList<Cardinal>;
    LPart := LOwned.AddObject(TList<Cardinal>.Create) as TList<Cardinal>;
    LMatches := LOwned.AddObject(TList<Cardinal>.Create) as TList<Cardinal>;
    if not LScanAll then begin
      for I := 0 to High(FBuildingFilter.OwnerKeys) do begin
        ReadKeyNumbers(FBuildingChain,FBuildingFilter.OwnerKeys[I],LPart);
        LCandidates.AddRange(LPart);
      end;
      LCandidates.Sort;
      LUnique := 0;
      for I := 0 to LCandidates.Count-1 do
        if (LUnique=0) or (LCandidates[I] <> LCandidates[LUnique-1]) then begin
          LCandidates[LUnique] := LCandidates[I];
          Inc(LUnique);
        end;
      LCandidates.Count := LUnique;
    end;
    LStart := 0;
    while (LScanAll and (LStart < FBuildingChain.Accounts)) or
      (not LScanAll and (LStart < LCandidates.Count)) do begin
      CheckAccountSelectorCancelled;
      if LScanAll then begin
        LTake := Min(SELECTOR_READ_CHUNK,FBuildingChain.Accounts-LStart);
        ReadContiguous(FBuildingChain,LStart,LTake,LRows);
      end else begin
        LTake := Min(SELECTOR_READ_CHUNK,LCandidates.Count-LStart);
        SetLength(LNumbers,LTake);
        for J := 0 to LTake-1 do LNumbers[J] := LCandidates[LStart+J];
        ReadAccounts(FBuildingChain,LNumbers,LRows);
      end;
      for I := 0 to High(LRows) do
        if AcceptAccount(LRows[I],FBuildingFilter) then begin
          LMatches.Add(LRows[I].account);
          Inc(Result.Total.Balance,LRows[I].balance);
        end;
      Inc(LStart,LTake);
    end;
    Result.Numbers := LMatches.ToArray;
    Result.Total.Accounts := Length(Result.Numbers);
  except
    Result.Free;
    raise;
  end;
end;

function TAccountSelectorDataSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
var LGeneration: UInt64; LSnapshot: TQuerySnapshot; LRows: TArray<TAccount>;
  LNumbers: TArray<Cardinal>; LStart,LTake,LRead,I,LAttempt: Integer;
  LRow: Variant; LComplete: Boolean;
begin
  // The selector's controls own its filters. Retain the base contract if a
  // separate caller explicitly supplies generic search/sort criteria.
  if Length(AParams.Filter) > 0 then Exit(inherited FetchPage(AParams,ADataTable));
  EnterLock;
  try
  FQueryLock.Acquire;
  try
    LAttempt := 0;
    repeat
      Inc(LAttempt);
      CheckAccountSelectorCancelled;
      LComplete := False;
      try
        LSnapshot := CurrentSnapshot(LGeneration);
        Result.TotalDataCount := LSnapshot.Total.Accounts;
        if AParams.PageSize <= 0 then begin
          Result.PageCount := Ord(Result.TotalDataCount > 0);
          Result.PageIndex := Result.PageCount-1;
          LStart := 0;
          LTake := Result.TotalDataCount;
        end else begin
          Result.PageCount := (Int64(Result.TotalDataCount)+AParams.PageSize-1) div AParams.PageSize;
          Result.PageIndex := Min(Max(0,AParams.PageIndex),Result.PageCount-1);
          LStart := Max(0,Result.PageIndex)*AParams.PageSize;
          LTake := Min(AParams.PageSize,Result.TotalDataCount-LStart);
        end;
        ADataTable.Columns := GetColumns;
        SetLength(ADataTable.Rows,LTake);
        FSnapshotBlockTip := Max(0,LSnapshot.Chain.Blocks-1);
        LRead := 0;
        while LRead < LTake do begin
          SetLength(LNumbers,Min(SELECTOR_READ_CHUNK,LTake-LRead));
          for I := 0 to High(LNumbers) do
            if LSnapshot.ImplicitAll then LNumbers[I] := LStart+LRead+I
            else LNumbers[I] := LSnapshot.Numbers[LStart+LRead+I];
          ReadAccounts(LSnapshot.Chain,LNumbers,LRows);
          for I := 0 to High(LRows) do begin
            LRow := TDataRow.New(PtrInt(TAccountSelectorDataSource),ADataTable.Columns);
            DehydrateItem(LRows[I],LRow);
            ADataTable.Rows[LRead+I] := LRow;
          end;
          Inc(LRead,Length(LRows));
        end;
        PublishSnapshot(LSnapshot,LGeneration);
        LComplete := True;
      except
        on E: EAccountSelectorSnapshotChanged do begin
          CheckAccountSelectorCancelled;
          // Continuous catch-up must finish with an ordinary retryable error,
          // not retain an unbounded provider loop or a forever-busy view.
          if LAttempt >= SELECTOR_SNAPSHOT_ATTEMPTS then
            raise EInvalidOperation.Create('The blockchain changed during the account search. Please search again.');
          Sleep(10);
        end;
      end;
    until LComplete;
  finally
    FQueryLock.Release;
  end;
  finally
    ReleaseLock;
  end;
end;

procedure TAccountSelectorDataSource.FetchAll(const AContainer: TList<TAccount>);
var LGeneration: UInt64; LSnapshot: TQuerySnapshot; LRows: TArray<TAccount>;
  LNumbers: TArray<Cardinal>; LStart,I,LAttempt,LOriginalCount: Integer;
  LComplete: Boolean;
begin
  // Generic external criteria are uncommon; apply them to the complete
  // selected query without first materializing and rereading Variant rows.
  EnterLock;
  try
  FQueryLock.Acquire;
  try
    LAttempt := 0;
    LOriginalCount := AContainer.Count;
    repeat
      Inc(LAttempt);
      CheckAccountSelectorCancelled;
      LComplete := False;
      try
        LSnapshot := CurrentSnapshot(LGeneration);
        FSnapshotBlockTip := Max(0,LSnapshot.Chain.Blocks-1);
        LStart := 0;
        while LStart < LSnapshot.Total.Accounts do begin
          SetLength(LNumbers,Min(SELECTOR_READ_CHUNK,LSnapshot.Total.Accounts-LStart));
          for I := 0 to High(LNumbers) do
            if LSnapshot.ImplicitAll then LNumbers[I] := LStart+I
            else LNumbers[I] := LSnapshot.Numbers[LStart+I];
          ReadAccounts(LSnapshot.Chain,LNumbers,LRows);
          AContainer.AddRange(LRows);
          Inc(LStart,Length(LRows));
        end;
        PublishSnapshot(LSnapshot,LGeneration);
        LComplete := True;
      except
        on E: EAccountSelectorSnapshotChanged do begin
          AContainer.Count := LOriginalCount;
          CheckAccountSelectorCancelled;
          if LAttempt >= SELECTOR_SNAPSHOT_ATTEMPTS then
            raise EInvalidOperation.Create('The blockchain changed during the account search. Please search again.');
          Sleep(10);
        end;
      end;
    until LComplete;
  finally
    FQueryLock.Release;
  end;
  finally
    ReleaseLock;
  end;
end;

function TAccountSelectorDataSource.TryGetSummary(out ATotal: TAccountSelectorTotal): Boolean;
begin
  FConfigLock.Acquire;
  try
    ATotal := FSummary;
    Result := FSummaryReady;
  finally
    FConfigLock.Release;
  end;
end;

function TAccountSelectorDataSource.PublishedAccountIndex(AAccount: Cardinal): Integer;
var LNumbers: TArray<Cardinal>; LImplicit: Boolean; LCount,L,H,M: Integer;
begin
  FConfigLock.Acquire;
  try
    LNumbers := FPublishedNumbers;
    LImplicit := FPublishedImplicitAll;
    LCount := FSummary.Accounts;
  finally
    FConfigLock.Release;
  end;
  Result := -1;
  if LImplicit then begin
    if AAccount < Cardinal(LCount) then Result := AAccount;
    Exit;
  end;
  L := 0;
  H := High(LNumbers);
  while L <= H do begin
    M := L+(H-L) div 2;
    if LNumbers[M] = AAccount then Exit(M);
    if LNumbers[M] < AAccount then L := M+1 else H := M-1;
  end;
end;

procedure TAccountSelectorDataSource.CalculateTypeStats(out AStats: TAccountSelectorTypeStats);
var LRows: TArray<TAccount>; LCurrent: TAccountSelectorChain;
  LSafeBox: TPCSafeBox;
  LStart,I,K,J,LType,LAttempt: Integer; LComplete: Boolean;
begin
  FConfigLock.Acquire;
  try
    LSafeBox := FSafeBox;
  finally
    FConfigLock.Release;
  end;
  LAttempt := 0;
  repeat
    Inc(LAttempt);
    CheckAccountSelectorCancelled;
    AStats := Default(TAccountSelectorTypeStats);
    AStats.Chain := ReadChain(LSafeBox);
    LComplete := False;
    try
      LStart := 0;
      while LStart < AStats.Chain.Accounts do begin
        ReadContiguous(AStats.Chain,LStart,
          Min(SELECTOR_READ_CHUNK,AStats.Chain.Accounts-LStart),LRows);
        for I := 0 to High(LRows) do begin
          CheckAccountSelectorCancelled;
          LType := LRows[I].account_type;
          if LType = 0 then begin
            Inc(AStats.Zero.Accounts);
            Inc(AStats.Zero.Balance,LRows[I].balance);
          end else if LType < 10000 then begin
            Inc(AStats.Invalid.Accounts);
            Inc(AStats.Invalid.Balance,LRows[I].balance);
          end;
          K := LType div 10000;
          for J := 4 downto 1 do begin
            Inc(AStats.Digits[K,J,LType mod 10].Accounts);
            Inc(AStats.Digits[K,J,LType mod 10].Balance,LRows[I].balance);
            LType := LType div 10;
          end;
        end;
        Inc(LStart,Length(LRows));
      end;
      LCurrent := ReadChain(LSafeBox);
      if TBaseType.BinStrComp(LCurrent.Hash,AStats.Chain.Hash) <> 0 then
        raise EAccountSelectorSnapshotChanged.Create('The account type snapshot changed');
      LComplete := True;
    except
      on E: EAccountSelectorSnapshotChanged do begin
        CheckAccountSelectorCancelled;
        if LAttempt >= SELECTOR_SNAPSHOT_ATTEMPTS then
          raise EInvalidOperation.Create('The blockchain changed during account type statistics. Please try again.');
        Sleep(10);
      end;
    end;
  until LComplete;
end;

end.
