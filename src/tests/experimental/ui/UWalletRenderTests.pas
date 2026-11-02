unit UWalletRenderTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Synthetic account snapshots and bitmap canvases exercise grid painting
  without opening a wallet, constructing a node or contacting peers. }

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, Variants, fpcunit, testregistry,
  Generics.Collections, UCrypto, UPCDataTypes, UDataSources, UCommon.Data;

type
  TRenderAccountSource = class(TAccountsDataSourceBase)
  public
    Items: TArray<TAccount>;
    FixtureTip: Cardinal;
    FetchCount: Integer;
    procedure SetSnapshotTip(ATip: Cardinal);
    procedure FetchAll(const AContainer: TList<TAccount>); override;
    function Page(AIndex: Integer; const AFilters: TFilterCriteria = nil): TDataTable;
  end;

  TWalletRenderTests = class(TTestCase)
  private
    FSource: TRenderAccountSource;
    FKey: TECPrivateKey;
    function Account(AActiveBlock, APassiveBlock: Cardinal): TAccount;
    function BalanceColor(const ARow: Variant; AValue: Int64): TColor;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AccountPendingUsesCapturedBoundary;
    procedure FetchedAccountsContainBooleanPendingFlag;
    procedure CachedPagesRetainTipUntilInvalidated;
    procedure PendingFlagControlsBalanceColor;
    procedure ExplicitFlagOverridesOperationTimestamp;
    procedure OperationsFallbackUsesRowTime;
    procedure AccountPaintingIgnoresLiveBlockHeight;
    procedure UnsignedBalancesUsePascFormatting;
  end;

implementation

uses
  Types, UAccounts, UConst, UCellRenderers;

procedure TRenderAccountSource.SetSnapshotTip(ATip: Cardinal);
begin
  FSnapshotBlockTip := ATip;
end;

procedure TRenderAccountSource.FetchAll(const AContainer: TList<TAccount>);
begin
  Inc(FetchCount);
  FSnapshotBlockTip := FixtureTip;
  AContainer.AddRange(Items);
end;

function TRenderAccountSource.Page(AIndex: Integer; const AFilters: TFilterCriteria): TDataTable;
begin
  FetchPage(TPageFetchParams.Create(AIndex, 1, AFilters, foAnd), Result);
end;

procedure TWalletRenderTests.SetUp;
begin
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FSource := TRenderAccountSource.Create(nil);
end;

procedure TWalletRenderTests.TearDown;
begin
  FSource.Free;
  FKey.Free;
end;

function TWalletRenderTests.Account(AActiveBlock, APassiveBlock: Cardinal): TAccount;
begin
  Result := CT_Account_NUL;
  Result.accountInfo.state := as_Normal;
  Result.accountInfo.accountKey := FKey.PublicKey;
  Result.balance := 12345;
  Result.updated_on_block_active_mode := AActiveBlock;
  Result.updated_on_block_passive_mode := APassiveBlock;
end;

function TWalletRenderTests.BalanceColor(const ARow: Variant; AValue: Int64): TColor;
var
  LBitmap: TBitmap;
  LHandled: Boolean;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(200, 40);
    LBitmap.Canvas.Font.Color := clBlack;
    LHandled := False;
    TCellRenderers.PASC_CheckPendingBalance(nil, 0, 0, LBitmap.Canvas,
      Rect(0, 0, 200, 40), [], AValue, ARow, LHandled);
    AssertTrue('Balance renderer must draw the numeric cell', LHandled);
    Result := LBitmap.Canvas.Font.Color;
  finally
    LBitmap.Free;
  end;
end;

procedure TWalletRenderTests.AccountPendingUsesCapturedBoundary;
var LAccount: TAccount;
begin
  FSource.SetSnapshotTip(10);
  LAccount := Account(9, 11);
  AssertTrue('Passive pending updates count', Boolean(FSource.GetItemField(LAccount, 'IsPending')));
  LAccount := Account(11, 9);
  AssertTrue('Active pending updates count', Boolean(FSource.GetItemField(LAccount, 'IsPending')));
  LAccount := Account(10, 9);
  AssertFalse('An update at the captured tip is confirmed', Boolean(FSource.GetItemField(LAccount, 'IsPending')));
  FSource.SetSnapshotTip(0);
  LAccount := Account(0, 0);
  AssertFalse('An empty chain boundary must not wrap', Boolean(FSource.GetItemField(LAccount, 'IsPending')));
  LAccount := Account(1, 0);
  AssertTrue(Boolean(FSource.GetItemField(LAccount, 'IsPending')));
end;

procedure TWalletRenderTests.FetchedAccountsContainBooleanPendingFlag;
var LTable: TDataTable;
begin
  FSource.FixtureTip := 10;
  FSource.Items := TArray<TAccount>.Create(Account(11, 9));
  LTable := FSource.Page(0);
  AssertEquals(1, Length(LTable.Rows));
  AssertEquals('Pending status is a row boolean', varBoolean, VarType(LTable.Rows[0].IsPending));
  AssertTrue(Boolean(LTable.Rows[0].IsPending));
  AssertEquals(11, Integer(LTable.Rows[0].LastUpdatedBlock));
end;

procedure TWalletRenderTests.CachedPagesRetainTipUntilInvalidated;
var
  LFirst, LWarm, LRefreshed: TDataTable;
  LFilters: TFilterCriteria;
begin
  FSource.SnapshotCacheEnabled := True;
  FSource.FixtureTip := 10;
  FSource.Items := TArray<TAccount>.Create(Account(11, 0), Account(11, 0));
  FSource.Items[1].account := 1;
  LFirst := FSource.Page(0);
  FSource.FixtureTip := 11;
  FSource.Items[0].updated_on_block_active_mode := 12;
  LWarm := FSource.Page(1);
  AssertEquals('A warm page must retain its raw snapshot', 1, FSource.FetchCount);
  AssertTrue('A later live tip cannot recolor a cached account', Boolean(LWarm.Rows[0].IsPending));
  SetLength(LFilters, 1);
  LFilters[0].ColumnName := 'AccountNumber';
  LFilters[0].Sort := sdDescending;
  LFilters[0].Filter := vgfSortable;
  LWarm := FSource.Page(0, LFilters);
  AssertEquals('Changing sort reuses the captured tip and accounts', 1, FSource.FetchCount);
  AssertEquals(1, Integer(LWarm.Rows[0].AccountNumber));
  AssertTrue(Boolean(LWarm.Rows[0].IsPending));
  FSource.InvalidateSnapshot;
  LRefreshed := FSource.Page(1);
  AssertEquals(2, FSource.FetchCount);
  AssertFalse('New raw values use the new tip', Boolean(LRefreshed.Rows[0].IsPending));
  LRefreshed := FSource.Page(0);
  AssertEquals(12, Integer(LRefreshed.Rows[0].LastUpdatedBlock));
  AssertTrue('New pending values and tip belong to one snapshot', Boolean(LRefreshed.Rows[0].IsPending));
  AssertTrue('Already returned rows retain their original classification', Boolean(LFirst.Rows[0].IsPending));
end;

procedure TWalletRenderTests.PendingFlagControlsBalanceColor;
var LRow: Variant;
begin
  LRow := TDataRow.New(-310001, TDataColumns.Create(TDataColumn.From('IsPending')));
  LRow.IsPending := True;
  AssertEquals(clBlue, BalanceColor(LRow, 12345));
  LRow.IsPending := False;
  AssertEquals(clGreen, BalanceColor(LRow, 12345));
  AssertEquals(clRed, BalanceColor(LRow, -12345));
  AssertEquals(clDefault, BalanceColor(LRow, 0));
end;

procedure TWalletRenderTests.ExplicitFlagOverridesOperationTimestamp;
var LRow: Variant;
begin
  LRow := TDataRow.New(-310002, TDataColumns.Create(
    TDataColumn.From('IsPending'), TDataColumn.From('UnixTime')));
  LRow.IsPending := False;
  LRow.UnixTime := 0;
  AssertEquals(clGreen, BalanceColor(LRow, 12345));
  LRow.IsPending := True;
  LRow.UnixTime := 12345;
  AssertEquals(clBlue, BalanceColor(LRow, 12345));
end;

procedure TWalletRenderTests.OperationsFallbackUsesRowTime;
var LRow: Variant;
begin
  LRow := TDataRow.New(-310003, TDataColumns.Create(TDataColumn.From('UnixTime')));
  LRow.UnixTime := 0;
  AssertEquals(clBlue, BalanceColor(LRow, 12345));
  LRow.UnixTime := 12345;
  AssertEquals(clGreen, BalanceColor(LRow, 12345));
end;

procedure TWalletRenderTests.AccountPaintingIgnoresLiveBlockHeight;
var LRow: Variant;
begin
  LRow := TDataRow.New(-310004, TDataColumns.Create(
    TDataColumn.From('IsPending'), TDataColumn.From('LastUpdatedBlock')));
  LRow.IsPending := False;
  LRow.LastUpdatedBlock := High(Cardinal);
  // No node exists in this fixture. Paint must consume the supplied row flag.
  AssertEquals(clGreen, BalanceColor(LRow, 12345));
end;

procedure TWalletRenderTests.UnsignedBalancesUsePascFormatting;
var
  LRow, LUnsigned, LSigned: Variant;
  LActual, LExpected: TBitmap;
  LHandled: Boolean;
  LValue: UInt64;
  I, X, Y: Integer;
begin
  LRow := TDataRow.New(-310005, TDataColumns.Create(TDataColumn.From('IsPending')));
  LRow.IsPending := False;
  LActual := TBitmap.Create;
  LExpected := TBitmap.Create;
  try
    LActual.SetSize(200, 40);
    LExpected.SetSize(200, 40);
    for I := 0 to 1 do begin
      if I = 0 then LValue := 16537985 else LValue := 8;
      LUnsigned := LValue;
      LSigned := Int64(LValue);
      AssertEquals(varQWord, VarType(LUnsigned));
      LActual.Canvas.Brush.Color := clWhite;
      LActual.Canvas.FillRect(Rect(0, 0, 200, 40));
      LExpected.Canvas.Brush.Color := clWhite;
      LExpected.Canvas.FillRect(Rect(0, 0, 200, 40));
      LActual.Canvas.Font.Color := clBlack;
      LExpected.Canvas.Font.Color := clBlack;
      LHandled := False;
      TCellRenderers.PASC_CheckPendingBalance(nil, 0, 0, LActual.Canvas,
        Rect(0, 0, 200, 40), [], LUnsigned, LRow, LHandled);
      AssertTrue('Unsigned account balances must use the PASC renderer', LHandled);
      AssertEquals(clGreen, LActual.Canvas.Font.Color);
      LHandled := False;
      TCellRenderers.PASC_CheckPendingBalance(nil, 0, 0, LExpected.Canvas,
        Rect(0, 0, 200, 40), [], LSigned, LRow, LHandled);
      AssertTrue(LHandled);
      for Y := 0 to 39 do
        for X := 0 to 199 do
          AssertEquals('Unsigned and signed monetary values must draw the same formatted text',
            LExpected.Canvas.Pixels[X, Y], LActual.Canvas.Pixels[X, Y]);
    end;
    AssertEquals('1,653.7985', TAccountComp.FormatMoney(16537985));
    AssertEquals('0.0008', TAccountComp.FormatMoney(8));
  finally
    LExpected.Free;
    LActual.Free;
  end;
end;

initialization
  RegisterTest(TWalletRenderTests);
end.
