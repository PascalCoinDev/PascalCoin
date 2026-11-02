unit UAccountSelector.Checks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Actual experimental controls and confirmed storage; only storage reads can
  be held or made obsolete. No singleton node, wallet files or peers are used. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses Classes;

procedure CheckAccountSelector(ALog: TStrings);

implementation

uses {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Types, Forms, Controls, StdCtrls, ExtCtrls, SyncObjs, Variants,
  Generics.Collections, UAccounts, UBaseTypes, UCrypto, UConst, UPCDataTypes, UWallet,
  UCommon.Data, UCommon.UI, UVisualGrid, UNode, UAccountSelectorData, UFRMAccountSelect,
  UFRMMemoText, UPCAccountsOrdenations, UAbstractMem;

type
  TSalePriceOrderAccess = class(TAccountsOrderedBySalePrice)
  public
    function CompareUsing(ARead: TCallReturnAccount; ALeft, ARight: Integer;
      ASearching: Boolean; APrice: UInt64): Integer;
  end;

  TSalePriceRecords = class
  public
    Prices: array[0..6] of UInt64;
    function ReadAccount(ANumber: Integer; var AAccount: TAccount): Boolean;
  end;

  TSelectorSafeBox = class(TPCSafeBox)
  public
    procedure Build(const AFirst, ASecond, AForeign: TAccountKey);
  end;

  TControlledSelectorSource = class(TAccountSelectorDataSource)
  protected
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
      var ATable: TDataTable): TPageFetchResult; override;
    function ReadChain(ASafeBox: TPCSafeBox): TAccountSelectorChain; override;
    procedure ReadKeyNumbers(const AChain: TAccountSelectorChain;
      const AKey: TAccountKey; const ANumbers: TList<Cardinal>); override;
    procedure ReadAccounts(const AChain: TAccountSelectorChain;
      const ANumbers: TArray<Cardinal>; out ARows: TArray<TAccount>); override;
  public
    Entered, ReleaseRead: TEvent;
    HoldReads, ChangeEveryRead: Boolean;
    ReadCalls, KeyCalls, RecordsRead, LargestRead: Integer;
    FetchCalls, FetchFinished, StoragePhase: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hold;
    procedure Release;
  end;

  TSelectorForm = class(TFRMAccountSelect)
  private
    FTestSafeBox: TPCSafeBox;
  protected
    function AccountSafeBox: TPCSafeBox; override;
    function CreateAccountSource: TAccountSelectorDataSource; override;
    procedure ShowTypeStatsReport(const AStats: TAccountSelectorTypeStats); override;
  public
    Reports, Heartbeats, Publications: Integer;
    LastStats: TAccountSelectorTypeStats;
    ReportText: String;
    ReportObserved: Boolean;
    ExpectedOwner: TAccountKey;
    WatchOwner, WrongPublication: Boolean;
    SavedCompletion: TNotifyEvent;
    constructor CreateForSafeBox(ASafeBox: TPCSafeBox);
    procedure AfterConstruction; override;
    function Source: TControlledSelectorSource;
    procedure Beat(Sender: TObject);
    procedure Completed(Sender: TObject);
    procedure Choose(const AName: String);
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure PumpFor(AMilliseconds: Cardinal);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64+AMilliseconds;
  repeat
    CheckSynchronize(0);
    Application.ProcessMessages;
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure Settle(AForm: TSelectorForm);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64+5000;
  repeat
    PumpFor(25);
    if not AForm.AccountGrid.IsUpdating and not AForm.StatsRunning then begin
      PumpFor(100);
      if not AForm.AccountGrid.IsUpdating and not AForm.StatsRunning then Exit;
    end;
  until GetTickCount64 >= LUntil;
  raise Exception.Create(Format('Account selector deadline: updating=%s suspended=%s waiting=%s manual=%s stats=%s reads=%d keys=%d records=%d fetch=%d finished=%d phase=%d beats=%d publications=%d rows=%d',
    [BoolToStr(AForm.AccountGrid.IsUpdating,True),BoolToStr(AForm.AccountGrid.RefreshSuspended,True),
     BoolToStr(AForm.AccountGrid.RefreshThrottle.Waiting,True),BoolToStr(AForm.AccountGrid.LoadingVisible,True),
     BoolToStr(AForm.StatsRunning,True),AForm.Source.ReadCalls,AForm.Source.KeyCalls,AForm.Source.RecordsRead,
     AForm.Source.FetchCalls,AForm.Source.FetchFinished,AForm.Source.StoragePhase,
     AForm.Heartbeats,AForm.Publications,AForm.AccountGrid.RowCount]));
end;

function TSalePriceOrderAccess.CompareUsing(ARead: TCallReturnAccount;
  ALeft, ARight: Integer; ASearching: Boolean; APrice: UInt64): Integer;
var LRead: TCallReturnAccount; LNumber: Integer; LInfo: TAccountInfo;
begin
  LRead := FCallReturnAccount;
  LNumber := FSearching_AccountNumber;
  LInfo := FSearching_AccountInfo;
  try
    FCallReturnAccount := ARead;
    FSearching_AccountNumber := -1;
    if ASearching then begin
      FSearching_AccountNumber := ALeft;
      FSearching_AccountInfo.price := APrice;
    end;
    Result := DoCompareData(ALeft,ARight);
  finally
    FCallReturnAccount := LRead;
    FSearching_AccountNumber := LNumber;
    FSearching_AccountInfo := LInfo;
  end;
end;

function TSalePriceRecords.ReadAccount(ANumber: Integer; var AAccount: TAccount): Boolean;
begin
  AAccount := CT_Account_NUL;
  Result := (ANumber >= Low(Prices)) and (ANumber <= High(Prices));
  if Result then AAccount.accountInfo.price := Prices[ANumber];
end;

procedure CheckSalePriceOrder(ASafeBox: TPCSafeBox; ALog: TStrings);
var LRecords: TSalePriceRecords; LOrder: TSalePriceOrderAccess;
  I,J,LExpected,LActual: Integer;
begin
  LRecords := TSalePriceRecords.Create;
  try
    LRecords.Prices[0] := 0;
    LRecords.Prices[1] := 1;
    LRecords.Prices[2] := UInt64(High(Int64));
    LRecords.Prices[3] := UInt64(High(Int64))+1;
    LRecords.Prices[4] := High(UInt64)-1;
    LRecords.Prices[5] := High(UInt64);
    LRecords.Prices[6] := High(UInt64);
    LOrder := TSalePriceOrderAccess(ASafeBox.AccountsOrderedBySalePrice);
    // Exercise the real comparer in both stored and replacement-search modes.
    for I := Low(LRecords.Prices) to High(LRecords.Prices) do
      for J := Low(LRecords.Prices) to High(LRecords.Prices) do begin
        if LRecords.Prices[I] < LRecords.Prices[J] then LExpected := -1
        else if LRecords.Prices[I] > LRecords.Prices[J] then LExpected := 1
        else if I < J then LExpected := -1
        else if I > J then LExpected := 1
        else LExpected := 0;
        LActual := LOrder.CompareUsing(LRecords.ReadAccount,I,J,False,0);
        Require(LActual = LExpected,'Stored sale-price ordering failed across the full UInt64 range');
        if LRecords.Prices[I] < LRecords.Prices[J] then LExpected := -1
        else LExpected := 1;
        LActual := LOrder.CompareUsing(LRecords.ReadAccount,100+I,J,True,LRecords.Prices[I]);
        Require(LActual = LExpected,'Replacement sale-price ordering lost the price or account tie-breaker');
        Require(LOrder.CompareUsing(LRecords.ReadAccount,I,I,True,High(UInt64)) = 0,
          'An account compared with itself must remain equal during replacement');
      end;
    ALog.Add('PASS full-range UInt64 sale-price order, account tie-breaker and replacement search');
  finally
    LRecords.Free;
  end;
end;

procedure TSelectorSafeBox.Build(const AFirst, ASecond, AForeign: TAccountKey);
var LBlock: TOperationBlock; LAccount: TAccount; I: Integer;
begin
  for I := 0 to 3 do begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := I;
    LBlock.account_key := AFirst;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := I*CT_NewLineSecondsAvg;
    AddNew(LBlock);
  end;
  for I := 0 to AccountsCount-1 do begin
    LAccount := Account(I);
    if I >= 10 then LAccount.accountInfo.accountKey := ASecond;
    if I = 19 then LAccount.accountInfo.accountKey := AForeign;
    LAccount.balance := (I+1)*10000;
    LAccount.name.FromString('selector-'+IntToStr(I));
    case I of
      1: LAccount.account_type := 42;
      2: LAccount.account_type := 9999;
      3: LAccount.account_type := 10000;
      4: LAccount.account_type := 65535;
    else
      LAccount.account_type := 0;
    end;
    if I in [1,11,19] then begin
      LAccount.accountInfo.state := as_ForSale;
      LAccount.accountInfo.new_publicKey := ASecond;
      LAccount.accountInfo.price := 10000;
    end;
    if I = 12 then begin
      LAccount.accountInfo.state := as_ForSale;
      LAccount.accountInfo.new_publicKey := CT_AccountInfo_NUL.accountKey;
      LAccount.accountInfo.price := 10000;
    end;
    UpdateAccount(I,LAccount.accountInfo,LAccount.name,LAccount.account_type,
      LAccount.balance,LAccount.n_operation,LAccount.account_data,LAccount.account_seal,
      aus_transaction_commit,0,0,False,False);
  end;
end;

constructor TControlledSelectorSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Entered := TEvent.Create(nil,True,False,'');
  ReleaseRead := TEvent.Create(nil,True,True,'');
end;

destructor TControlledSelectorSource.Destroy;
begin
  if Assigned(ReleaseRead) then ReleaseRead.SetEvent;
  Entered.Free;
  ReleaseRead.Free;
  inherited Destroy;
end;

procedure TControlledSelectorSource.Hold;
begin
  Entered.ResetEvent;
  ReleaseRead.ResetEvent;
  HoldReads := True;
end;

procedure TControlledSelectorSource.Release;
begin
  HoldReads := False;
  ReleaseRead.SetEvent;
end;

function TControlledSelectorSource.FetchPage(
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ATable: TDataTable): TPageFetchResult;
begin
  InterlockedIncrement(FetchCalls);
  InterlockedExchange(StoragePhase,1);
  try
    Result := inherited FetchPage(AParams,ATable);
  finally
    InterlockedIncrement(FetchFinished);
    InterlockedExchange(StoragePhase,9);
  end;
end;

function TControlledSelectorSource.ReadChain(ASafeBox: TPCSafeBox): TAccountSelectorChain;
begin
  InterlockedExchange(StoragePhase,2);
  Result := inherited ReadChain(ASafeBox);
  InterlockedExchange(StoragePhase,3);
end;

procedure TControlledSelectorSource.ReadKeyNumbers(const AChain: TAccountSelectorChain;
  const AKey: TAccountKey; const ANumbers: TList<Cardinal>);
begin
  InterlockedIncrement(KeyCalls);
  inherited ReadKeyNumbers(AChain,AKey,ANumbers);
end;

procedure TControlledSelectorSource.ReadAccounts(const AChain: TAccountSelectorChain;
  const ANumbers: TArray<Cardinal>; out ARows: TArray<TAccount>);
var LUntil: UInt64;
begin
  InterlockedIncrement(ReadCalls);
  InterlockedExchange(StoragePhase,4);
  if ChangeEveryRead then
    raise EAccountSelectorSnapshotChanged.Create('Controlled obsolete chain revision');
  if HoldReads then begin
    Entered.SetEvent;
    LUntil := GetTickCount64+5000;
    while ReleaseRead.WaitFor(10) <> wrSignaled do begin
      CheckAccountSelectorCancelled;
      Require(GetTickCount64 < LUntil,'Controlled selector read was never released');
    end;
  end;
  CheckAccountSelectorCancelled;
  InterlockedExchangeAdd(RecordsRead,Length(ANumbers));
  if Length(ANumbers) > LargestRead then LargestRead := Length(ANumbers);
  inherited ReadAccounts(AChain,ANumbers,ARows);
  InterlockedExchange(StoragePhase,5);
end;

constructor TSelectorForm.CreateForSafeBox(ASafeBox: TPCSafeBox);
begin
  FTestSafeBox := ASafeBox;
  inherited Create(nil);
end;

procedure TSelectorForm.AfterConstruction;
var LTimer: TTimer;
begin
  // LCL creates resource-backed controls in AfterConstruction; VCL is ready too.
  inherited AfterConstruction;
  SetBounds(80,90,900,560);
  SavedCompletion := AccountGrid.OnFinishedUpdating;
  AccountGrid.OnFinishedUpdating := Completed;
  LTimer := TTimer.Create(Self);
  LTimer.Interval := 20;
  LTimer.OnTimer := Beat;
  LTimer.Enabled := True;
end;

function TSelectorForm.AccountSafeBox: TPCSafeBox;
begin
  Result := FTestSafeBox;
end;

function TSelectorForm.CreateAccountSource: TAccountSelectorDataSource;
begin
  Result := TControlledSelectorSource.Create(Self);
end;

function TSelectorForm.Source: TControlledSelectorSource;
begin
  Result := TControlledSelectorSource(AccountSource);
end;

procedure TSelectorForm.Beat(Sender: TObject);
var I: Integer; LReport: TFRMMemoText;
begin
  Inc(Heartbeats);
  for I := 0 to ComponentCount-1 do
    if Components[I] is TFRMMemoText then begin
      LReport := TFRMMemoText(Components[I]);
      if LReport.Visible then begin
        Require(LReport.Caption = 'Account Type Stats','The original statistics report form was not presented');
        ReportText := LReport.DataText;
        ReportObserved := True;
        LReport.ModalResult := mrOk;
      end;
    end;
end;

procedure TSelectorForm.Completed(Sender: TObject);
var I: Integer; LNumber: Cardinal; LAccount: TAccount;
begin
  if Assigned(SavedCompletion) then SavedCompletion(Sender);
  Inc(Publications);
  if not WatchOwner then Exit;
  for I := 0 to AccountGrid.RowCount-1 do begin
    LNumber := Cardinal(TDataRowData(AccountGrid.Rows[I])['AccountNumber']);
    LAccount := FTestSafeBox.Account(LNumber);
    if not TAccountComp.EqualAccountKeys(LAccount.accountInfo.accountKey,ExpectedOwner) then
      WrongPublication := True;
  end;
end;

procedure TSelectorForm.Choose(const AName: String);
var LIndex: Integer;
begin
  LIndex := cbMyPrivateKeys.Items.IndexOf(AName);
  Require(LIndex >= 0,'The requested public-key choice is absent: '+AName);
  cbMyPrivateKeys.ItemIndex := LIndex;
  Require(Assigned(cbMyPrivateKeys.OnChange),'Key dropdown has no native selection-change binding');
  cbMyPrivateKeys.OnChange(cbMyPrivateKeys);
end;

procedure TSelectorForm.ShowTypeStatsReport(const AStats: TAccountSelectorTypeStats);
begin
  Require(TThread.CurrentThread.ThreadID = MainThreadID,'Report presentation ran off the UI thread');
  LastStats := AStats;
  ReportObserved := False;
  // Observe and close the actual modal report through the fixture heartbeat;
  // production still creates and displays its original TFRMMemoText.
  inherited ShowTypeStatsReport(AStats);
  Require(ReportObserved,'The actual modal type-statistics report was never visible');
  Inc(Reports);
end;

procedure AssertOwner(AForm: TSelectorForm; const AKey: TAccountKey; AExpectedCount: Integer);
var I: Integer; LNumber: Cardinal;
begin
  Require(AForm.AccountGrid.RowCount = AExpectedCount,'Selected public key produced the wrong row count');
  Require(AForm.lblAccountsCount.Caption = IntToStr(AExpectedCount),'Selected key count label is stale');
  for I := 0 to AForm.AccountGrid.RowCount-1 do begin
    LNumber := Cardinal(TDataRowData(AForm.AccountGrid.Rows[I])['AccountNumber']);
    Require(TAccountComp.EqualAccountKeys(AForm.FTestSafeBox.Account(LNumber).accountInfo.accountKey,AKey),
      'A sorted key choice displayed accounts belonging to another owner');
  end;
end;

procedure LegacyReport(ASafeBox: TPCSafeBox; AText: TStrings);
type
  TLegacyTotal = record
    Accounts: Integer;
    Balance: Int64;
  end;
var LZero,LInvalid: TLegacyTotal; LDigits: array[0..6,1..4,0..9] of TLegacyTotal;
  LAccount: TAccount; C,I,J,K: Integer;
begin
  FillChar(LZero,SizeOf(LZero),0);
  FillChar(LInvalid,SizeOf(LInvalid),0);
  FillChar(LDigits,SizeOf(LDigits),0);
  for C := 0 to ASafeBox.AccountsCount-1 do begin
    LAccount := ASafeBox.Account(C);
    if LAccount.account_type = 0 then begin
      Inc(LZero.Accounts);
      Inc(LZero.Balance,LAccount.balance);
    end else if LAccount.account_type < 10000 then begin
      Inc(LInvalid.Accounts);
      Inc(LInvalid.Balance,LAccount.balance);
    end;
    K := (LAccount.account_type mod 100000) div 10000;
    for I := 0 to 9 do begin
      J := (LAccount.account_type mod 10000) div 1000;
      if J = I then begin
        Inc(LDigits[K,1,J].Accounts);
        Inc(LDigits[K,1,J].Balance,LAccount.balance);
      end;
      J := (LAccount.account_type mod 1000) div 100;
      if J = I then begin
        Inc(LDigits[K,2,J].Accounts);
        Inc(LDigits[K,2,J].Balance,LAccount.balance);
      end;
      J := (LAccount.account_type mod 100) div 10;
      if J = I then begin
        Inc(LDigits[K,3,J].Accounts);
        Inc(LDigits[K,3,J].Balance,LAccount.balance);
      end;
      J := LAccount.account_type mod 10;
      if J = I then begin
        Inc(LDigits[K,4,J].Accounts);
        Inc(LDigits[K,4,J].Balance,LAccount.balance);
      end;
    end;
  end;
  AText.Clear;
  AText.Add('*** Account type stats ***');
  AText.Add(Format('Current block:%d accounts:%d coins:%s',
    [ASafeBox.BlocksCount,ASafeBox.AccountsCount,TAccountComp.FormatMoney(ASafeBox.TotalBalance)]));
  AText.Add('');
  AText.Add('** Basic stats **');
  AText.Add(Format('Account type=0 accounts:%d coins:%s',[LZero.Accounts,TAccountComp.FormatMoney(LZero.Balance)]));
  AText.Add(Format('Account type [1..9999] accounts:%d coins:%s',[LInvalid.Accounts,TAccountComp.FormatMoney(LInvalid.Balance)]));
  AText.Add('');
  AText.Add('*** Stats by group ***');
  for K := 0 to 6 do begin
    AText.Add('');
    AText.Add(Format('** Group [%d..%d] **',[K*10000,(K+1)*10000-1]));
    for J := 0 to 9 do AText.Add(Format('Account type like %d%d*** accounts:%d coins:%s',
      [K,J,LDigits[K,1,J].Accounts,TAccountComp.FormatMoney(LDigits[K,1,J].Balance)]));
    for J := 0 to 9 do AText.Add(Format('Account type like %d*%d** accounts:%d coins:%s',
      [K,J,LDigits[K,2,J].Accounts,TAccountComp.FormatMoney(LDigits[K,2,J].Balance)]));
    for J := 0 to 9 do AText.Add(Format('Account type like %d**%d* accounts:%d coins:%s',
      [K,J,LDigits[K,3,J].Accounts,TAccountComp.FormatMoney(LDigits[K,3,J].Balance)]));
    for J := 0 to 9 do AText.Add(Format('Account type like %d***%d accounts:%d coins:%s',
      [K,J,LDigits[K,4,J].Accounts,TAccountComp.FormatMoney(LDigits[K,4,J].Balance)]));
  end;
end;

procedure CheckGenericSnapshotInvalidation(ASafeBox: TPCSafeBox;
  const AFirst,ASecond,AEmpty: TAccountKey; ALog: TStrings);
var LSource: TControlledSelectorSource; LAdapter: IDataSource;
  LFilter: TAccountSelectorFilter; LCriteria: TFilterCriteria;
  LParams: TPageFetchParams; LResult: TPageFetchResult; LTable: TDataTable;
  LBefore: Integer;
begin
  LSource := TControlledSelectorSource.Create(nil);
  LAdapter := LSource;
  try
    LSource.SnapshotCacheEnabled := True;
    LFilter := Default(TAccountSelectorFilter);
    LFilter.MyAccounts := True;
    LFilter.SpecificOwner := True;
    LFilter.MaxBalance := -1;
    SetLength(LFilter.OwnerKeys,1);
    LFilter.OwnerKeys[0] := AFirst;
    SetLength(LCriteria,1);
    LCriteria[0] := Default(TColumnFilter);
    LCriteria[0].ColumnName := 'AccountNumber';
    LCriteria[0].Filter := vgfNumericGTE;
    LCriteria[0].Sort := sdDescending;
    SetLength(LCriteria[0].Values,1);
    LCriteria[0].Values[0] := 0;
    LParams := TPageFetchParams.Create(0,-1,LCriteria,foAnd);
    LSource.Configure(ASafeBox,LFilter);
    // These criteria deliberately enter the inherited generic snapshot cache.
    LResult := LAdapter.FetchPage(LParams,LTable);
    Require((LResult.TotalDataCount = 10) and (Length(LTable.Rows) = 10) and
      (Integer(TDataRowData(LTable.Rows[0])['AccountNumber']) = 9),
      'Inherited generic search/sort did not use the selected owner snapshot');
    LFilter.OwnerKeys[0] := ASecond;
    LSource.Configure(ASafeBox,LFilter);
    LResult := LAdapter.FetchPage(LParams,LTable);
    Require((LResult.TotalDataCount = 9) and (Length(LTable.Rows) = 9) and
      (Integer(TDataRowData(LTable.Rows[0])['AccountNumber']) = 18),
      'Configure reused an obsolete inherited generic snapshot after a key change');
    LFilter.OwnerKeys[0] := AEmpty;
    LSource.Configure(ASafeBox,LFilter);
    LResult := LAdapter.FetchPage(LParams,LTable);
    Require((LResult.TotalDataCount = 0) and (Length(LTable.Rows) = 0),
      'The inherited cache retained an old owner for an empty selected key');
    LFilter.MyAccounts := False;
    LFilter.SpecificOwner := False;
    LFilter.OwnerKeys := nil;
    LSource.Configure(ASafeBox,LFilter);
    LParams.PageSize := 3;
    LParams.PageIndex := 2;
    LResult := LAdapter.FetchPage(LParams,LTable);
    Require((LResult.TotalDataCount = 20) and (LResult.PageCount = 7) and
      (Length(LTable.Rows) = 3) and
      (Integer(TDataRowData(LTable.Rows[0])['AccountNumber']) = 13) and
      (Integer(TDataRowData(LTable.Rows[2])['AccountNumber']) = 11),
      'Generic sorting/paging did not apply to the complete selected query');
    LBefore := LSource.RecordsRead;
    LParams.PageIndex := 3;
    LResult := LAdapter.FetchPage(LParams,LTable);
    Require((LSource.RecordsRead = LBefore) and (LResult.TotalDataCount = 20),
      'A warm generic page reread storage instead of its valid snapshot');
    ALog.Add('PASS inherited snapshot cache invalidates across key/empty/All configurations');
  finally
    LAdapter := nil;
    LSource.Free;
  end;
end;

procedure CheckAccountSelector(ALog: TStrings);
var LSafeBox: TSelectorSafeBox; LKeys: TWalletKeys; LFirst,LSecond,LEmpty,LForeign: TECPrivateKey;
  LForm: TSelectorForm; LBefore,LBeats,LPublications,LWalletHandlers: Integer; LStarted: UInt64;
  LExpected: TStringList; LTask: TAccountSelectorStatsTask;
begin
  Require(not TNode.NodeExists,'The selector probe must not initialize the singleton node');
  LSafeBox := nil;
  LKeys := nil;
  LFirst := TECPrivateKey.Create;
  LSecond := TECPrivateKey.Create;
  LEmpty := TECPrivateKey.Create;
  LForeign := TECPrivateKey.Create;
  LForm := nil;
  LExpected := TStringList.Create;
  try
    LFirst.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    LSecond.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    LEmpty.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    LForeign.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    LSafeBox := TSelectorSafeBox.Create;
    LSafeBox.Build(LFirst.PublicKey,LSecond.PublicKey,LForeign.PublicKey);
    CheckSalePriceOrder(LSafeBox,ALog);
    CheckGenericSnapshotInvalidation(LSafeBox,LFirst.PublicKey,LSecond.PublicKey,LEmpty.PublicKey,ALog);
    LKeys := TWalletKeys.Create(nil);
    LKeys.AddPublicKey('Zulu owner',LFirst.PublicKey);
    LKeys.AddPublicKey('Alpha owner',LSecond.PublicKey);
    LKeys.AddPublicKey('Empty owner',LEmpty.PublicKey);
    LWalletHandlers := Length(LKeys.OnChanged.Handlers)+Length(LKeys.OnChanged.MainThreadHandlers);
    LForm := TSelectorForm.CreateForSafeBox(LSafeBox);
    LForm.WalletKeys := LKeys;
    Require(Length(LKeys.OnChanged.Handlers)+Length(LKeys.OnChanged.MainThreadHandlers) = LWalletHandlers,
      'The selector added an unsafe live-wallet UI subscription');
    LForm.AccountGrid.PageMode := vpmSingle;
    LForm.Show;
    Settle(LForm);
    Require(LForm.AccountGrid.RowCount = 20,'Global All did not display the complete confirmed fixture');
    Require(LForm.Source.RecordsRead <= 40,'Unfiltered All materialized the chain more than its requested rows');
    LForm.cbMyAccounts.Checked := True;
    LForm.cbMyAccounts.OnClick(LForm.cbMyAccounts);
    Settle(LForm);
    Require(LForm.cbMyPrivateKeys.ItemIndex = 0,'Initial wallet choice must explicitly select All');
    LBefore := LForm.Source.KeyCalls;
    LForm.Choose('Alpha owner');
    Settle(LForm);
    AssertOwner(LForm,LSecond.PublicKey,9);
    Require(LForm.Source.KeyCalls > LBefore,'My Accounts did not use the existing public-key index');
    LForm.Choose('Zulu owner');
    Settle(LForm);
    AssertOwner(LForm,LFirst.PublicKey,10);
    LBefore := LForm.Source.RecordsRead;
    LForm.Choose('Empty owner');
    Settle(LForm);
    Require((LForm.AccountGrid.RowCount = 0) and (LForm.lblAccountsCount.Caption = '0'),
      'An empty selected key must not mean all wallet or global accounts');
    Require(LForm.Source.RecordsRead = LBefore,'An absent key unexpectedly scanned account records');
    LKeys.Delete(LKeys.IndexOfAccountKey(LEmpty.PublicKey));
    LForm.WalletKeys := LKeys;
    Settle(LForm);
    Require((LForm.cbMyPrivateKeys.ItemIndex > 0) and (LForm.AccountGrid.RowCount = 0),
      'Removing the selected key silently broadened its scope to All');
    LKeys.AddPublicKey('Empty owner',LEmpty.PublicKey);
    LForm.WalletKeys := LKeys;
    Settle(LForm);
    Require(LForm.AccountGrid.RowCount = 0,'Restoring an empty key selected unrelated accounts');
    ALog.Add('PASS sorted key identity, disjoint owners, indexed membership, empty key and All');

    LForm.Choose('Alpha owner');
    LForm.cbAccountsBalance.Checked := True;
    LForm.ebMinBalance.Text := '15.0000';
    LForm.ebMaxBalance.Text := '';
    LForm.cbAccountsName.Checked := True;
    LForm.ebAccountName.Text := 'selector-1';
    LForm.bbSearch.OnClick(LForm.bbSearch);
    Settle(LForm);
    Require((LForm.AccountGrid.RowCount = 5) and
      (LForm.lblAccountsBalance.Caption = TAccountComp.FormatMoney(850000)),
      'Name and balance filters changed their matching accounts or aggregate');
    LForm.cbAccountsBalance.Checked := False;
    LForm.cbAccountsName.Checked := False;
    LForm.cbOnlyForPrivateSaleToMe.Checked := True;
    LForm.Choose('(All)');
    Settle(LForm);
    Require(LForm.AccountGrid.RowCount = 3,
      'Wallet All/private-sale destination semantics must include a nonwallet current owner');
    LForm.Choose('Zulu owner');
    Settle(LForm);
    Require(LForm.AccountGrid.RowCount = 1,'A specific owner did not restrict private-sale results');
    LForm.cbOnlyForPublicSale.Checked := True;
    LForm.bbSearch.OnClick(LForm.bbSearch);
    Settle(LForm);
    Require(LForm.AccountGrid.RowCount = 0,'Contradictory public/private sale filters must remain conjunctive');
    LForm.cbOnlyForPrivateSaleToMe.Checked := False;
    LForm.cbOnlyForSale.Checked := True;
    LForm.Choose('Alpha owner');
    Settle(LForm);
    Require((LForm.AccountGrid.RowCount = 1) and
      (Cardinal(TDataRowData(LForm.AccountGrid.Rows[0])['AccountNumber']) = 12),
      'Public sale filtering changed the confirmed result');
    LForm.cbOnlyForSale.Checked := False;
    LForm.cbOnlyForPublicSale.Checked := False;
    LForm.DefaultAccount := 11;
    LForm.AllowSelect := True;
    LForm.AccountGrid.ManualPageSize := 1;
    LForm.AccountGrid.PageMode := vpmManual;
    LForm.bbSearch.OnClick(LForm.bbSearch);
    Settle(LForm);
    Require((LForm.AccountGrid.PageIndex = 1) and (LForm.GetSelected = 11) and LForm.bbSelect.Enabled,
      'Default account was not navigated to and selected on a one-row page');
    LForm.DefaultAccount := -1;
    LForm.AccountGrid.PageMode := vpmSingle;
    LForm.Choose('Empty owner');
    Settle(LForm);
    ALog.Add('PASS name/balance/sale combinations, complete totals and default account selection');

    LForm.Source.Hold;
    LForm.Choose('Zulu owner');
    Require(LForm.Source.Entered.WaitFor(1000) = wrSignaled,'Controlled old-key provider did not enter');
    LStarted := GetTickCount64;
    LForm.Choose('Alpha owner');
    LForm.Choose('Empty owner');
    LForm.Choose('Zulu owner');
    LForm.Choose('Alpha owner');
    Require(GetTickCount64-LStarted < 400,'Key changes waited for an obsolete provider');
    LForm.ExpectedOwner := LSecond.PublicKey;
    LForm.WatchOwner := True;
    LPublications := LForm.Publications;
    LForm.Source.Release;
    Settle(LForm);
    AssertOwner(LForm,LSecond.PublicKey,9);
    Require((LForm.Publications > LPublications) and not LForm.WrongPublication,
      'A stale rapid-key result was published');
    LForm.WatchOwner := False;
    ALog.Add('PASS held provider remains nonblocking and only latest key rows publish');

    LForm.Source.ChangeEveryRead := True;
    LBefore := LForm.Source.ReadCalls;
    LForm.AccountGrid.RefreshGrid;
    Settle(LForm);
    Require((LForm.Source.ReadCalls = LBefore+3) and not LForm.AccountGrid.LoadingVisible and
      not TLoadingCircle(LForm.AccountGrid.FindComponent('LoadingCircle')).Active,
      'A continuously changing search retained unbounded work or a spinning loading card');
    Require(Pos('Please search again',
      TLabel(LForm.AccountGrid.FindComponent('LoadingStatus')).Caption) > 0,
      'Search retry exhaustion did not expose the existing retryable grid error');
    LForm.Source.ChangeEveryRead := False;
    LForm.AccountGrid.RefreshGrid;
    Settle(LForm);
    AssertOwner(LForm,LSecond.PublicKey,9);
    ALog.Add('PASS bounded search retry, nonspinning error and successful retry');

    LForm.Source.Hold;
    LBefore := LForm.Reports;
    LBeats := LForm.Heartbeats;
    LStarted := GetTickCount64;
    LForm.bbTypeStats.OnClick(LForm.bbTypeStats);
    Require(GetTickCount64-LStarted < 200,'Type statistics blocked its UI click');
    Require(LForm.Source.Entered.WaitFor(1000) = wrSignaled,'Statistics did not enter its owned worker');
    Require(LForm.StatsRunning and LForm.AccountGrid.LoadingVisible,
      'Statistics must show the existing visual-grid loading presentation');
    Require(TPanel(LForm.AccountGrid.FindComponent('LoadingOverlay')).Visible and
      TLoadingCircle(LForm.AccountGrid.FindComponent('LoadingCircle')).Active,
      'Statistics loading did not activate the actual shared loading controls');
    PumpFor(150);
    Require((LForm.Heartbeats > LBeats+2) and (LForm.Reports = LBefore),
      'The UI did not remain responsive while statistics storage was held');
    LForm.Source.Release;
    Settle(LForm);
    Require((LForm.Reports = LBefore+1) and not LForm.AccountGrid.LoadingVisible,
      'Statistics completion did not clear the loading card and present once');
    LegacyReport(LSafeBox,LExpected);
    Require(LForm.ReportText = LExpected.Text,'Global type statistics changed a legacy report line or aggregate');
    Require(LForm.LastStats.Chain.Accounts = 20,'Type statistics incorrectly used the selected-key subset');
    Require(LForm.Source.LargestRead <= 128,'Storage reads exceeded the cooperative chunk bound');
    ALog.Add('PASS background global statistics, native loading, responsive UI and exact legacy report');

    LForm.Source.ChangeEveryRead := True;
    LBefore := LForm.Source.ReadCalls;
    LTask := TAccountSelectorStatsTask.Create(LForm.Source);
    try
      LTask.Start;
      LStarted := GetTickCount64;
      while not LTask.Finished and (GetTickCount64-LStarted < 2000) do PumpFor(10);
      Require(LTask.Finished,'Continuous chain changes retained an unbounded statistics retry');
      Require((LForm.Source.ReadCalls = LBefore+3) and (Pos('Please try again',LTask.ErrorMessage) > 0),
        'Statistics snapshot retries must end after three attempts with a retryable error');
    finally
      LTask.Free;
    end;
    LForm.Source.ChangeEveryRead := False;
    ALog.Add('PASS continuous chain changes finish after bounded retry attempts');

    LForm.Source.Hold;
    LForm.bbTypeStats.OnClick(LForm.bbTypeStats);
    Require(LForm.Source.Entered.WaitFor(1000) = wrSignaled,'Close fixture statistics never entered');
    LStarted := GetTickCount64;
    FreeAndNil(LForm);
    Require(GetTickCount64-LStarted < 1000,'Closing statistics waited for a UI synchronization or busy loop');
    PumpFor(100);
    ALog.Add('PASS close cooperatively retires held statistics and queued receivers');
    LForm := TSelectorForm.CreateForSafeBox(LSafeBox);
    LForm.Source.Hold;
    LForm.WalletKeys := LKeys;
    LForm.Show;
    Require(LForm.Source.Entered.WaitFor(1000) = wrSignaled,'Close fixture page provider never entered');
    LStarted := GetTickCount64;
    FreeAndNil(LForm);
    Require(GetTickCount64-LStarted < 1000,'Closing the selector did not cancel its held grid provider');
    PumpFor(100);
    Require(Length(LKeys.OnChanged.Handlers)+Length(LKeys.OnChanged.MainThreadHandlers) = LWalletHandlers,
      'Closing selector receivers changed the wallet notification contract');
    ALog.Add('PASS close cooperatively retires held grid query without wallet subscriptions');
    Require(not TNode.NodeExists,'Account selector created a hidden node/network dependency');
  finally
    if Assigned(LForm) then LForm.Source.Release;
    LForm.Free;
    LExpected.Free;
    LKeys.Free;
    LSafeBox.Free;
    LForeign.Free;
    LEmpty.Free;
    LSecond.Free;
    LFirst.Free;
  end;
end;

end.
