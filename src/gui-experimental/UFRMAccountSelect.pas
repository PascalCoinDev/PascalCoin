unit UFRMAccountSelect;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

{$I ..\config.inc}

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  {$IFDEF FPC}LCLIntf, LCLType, LMessages,{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  UAccounts, Grids, StdCtrls, Buttons, ExtCtrls, UCommon.UI, UWallet, UNode,
  UConst, UThread, UPCOrderedLists, UBaseTypes, UPCDataTypes,
  Generics.Collections, UVisualGrid, UCommon.Data, UAccountSelectorData,
  UWalletStartupTask;

const
  CT_AS_MyAccounts = $0001;
  CT_AS_OnlyForSale = $0002;

type
  TAccountSelectorStatsTask = class(TWalletStartupTask)
  private
    FSource: TAccountSelectorDataSource;
  protected
    procedure ExecuteLoad; override;
  public
    Stats: TAccountSelectorTypeStats;
    constructor Create(ASource: TAccountSelectorDataSource); reintroduce;
  end;

  TFRMAccountSelect = class(TApplicationForm)
    bbTypeStats: TBitBtn;
    cbAccountsName: TCheckBox;
    ebMinBalance: TEdit;
    ebMaxBalance: TEdit;
    ebAccountName: TEdit;
    Label17: TLabel;
    Label19: TLabel;
    lblAccountsBalance: TLabel;
    lblAccountsCount: TLabel;
    pnlAccountsTop: TPanel;
    cbOnlyForSale: TCheckBox;
    bbSearch: TBitBtn;
    cbOnlyForPrivateSaleToMe: TCheckBox;
    pnlBottom: TPanel;
    dgAccounts: Grids.TDrawGrid;
    bbSelect: TBitBtn;
    bbCancel: TBitBtn;
    cbAccountsBalance: TCheckBox;
    cbMyAccounts: TCheckBox;
    cbMyPrivateKeys: TComboBox;
    cbOnlyForPublicSale: TCheckBox;
    procedure bbSearchClick(Sender: TObject);
    procedure bbTypeStatsClick(Sender: TObject);
    procedure cbMyAccountsChange(Sender: TObject);
    procedure cbMyAccountsClick(Sender: TObject);
    procedure cbMyPrivateKeysChange(Sender: TObject);
    procedure ebMaxBalanceExit(Sender: TObject);
    procedure ebMinBalanceExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private type
    TKeyChoice = class
      Key: TAccountKey;
      Available: Boolean;
    end;
  private
    FAllowSelect, FChangingControls, FStopping, FDefaultSelectionApplied: Boolean;
    FDefaultAccount: Int64;
    FFilters: Integer;
    FWalletKeys: TWalletKeys;
    FNode: TNode;
    FSafeBox: TPCSafeBox;
    FAccountsGrid: TVisualGrid;
    FSource: TAccountSelectorDataSource;
    FStatsTask: TAccountSelectorStatsTask;
    FKeyChoices: TObjectList<TKeyChoice>;
    procedure SetAllowSelect(AValue: Boolean);
    procedure SetDefaultAccount(AValue: Int64);
    procedure SetFilters(AValue: Integer);
    procedure SetNode(const Value: TNode);
    procedure SetWalletKeys(const Value: TWalletKeys);
    procedure SearchFiltered;
    procedure UpdateControls;
    procedure OnAccountsGridUpdated(Sender: TObject);
    procedure OnGridSelection(Sender: TObject;
      {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection);
    procedure OnStatsCompleted(Sender: TObject);
    function GetStatsRunning: Boolean;
  protected
    function AccountSafeBox: TPCSafeBox; virtual;
    function CreateAccountSource: TAccountSelectorDataSource; virtual;
    function CreateStatsTask: TAccountSelectorStatsTask; virtual;
    procedure BuildTypeStatsReport(const AStats: TAccountSelectorTypeStats; AText: TStrings);
    procedure ShowTypeStatsReport(const AStats: TAccountSelectorTypeStats); virtual;
  public
    property WalletKeys: TWalletKeys read FWalletKeys write SetWalletKeys;
    property Node: TNode read FNode write SetNode;
    property Filters: Integer read FFilters write SetFilters;
    property DefaultAccount: Int64 read FDefaultAccount write SetDefaultAccount;
    function GetSelected: Int64;
    property AllowSelect: Boolean read FAllowSelect write SetAllowSelect;
    property AccountGrid: TVisualGrid read FAccountsGrid;
    property AccountSource: TAccountSelectorDataSource read FSource;
    property StatsRunning: Boolean read GetStatsRunning;
  end;

implementation

{$IFDEF FPC}{$R *.lfm}{$ELSE}{$R *.dfm}{$ENDIF}

uses UFRMMemoText;

{$IFNDEF FPC}
type
  TSelectorNativeGridAccess = class(UVisualGrid.TDrawGrid)
  public
    procedure PublishCurrentSelection;
  end;

procedure TSelectorNativeGridAccess.PublishCurrentSelection;
begin
  // The native adapter retains its own ranges, including a one-row page whose
  // current row does not move. Reuse its normal selection publication path.
  SelectionMoved(Selection);
end;
{$ENDIF}

constructor TAccountSelectorStatsTask.Create(ASource: TAccountSelectorDataSource);
begin
  inherited Create;
  FSource := ASource;
end;

procedure TAccountSelectorStatsTask.ExecuteLoad;
begin
  CheckCancelled;
  FSource.CalculateTypeStats(Stats);
  CheckCancelled;
end;

function TFRMAccountSelect.CreateAccountSource: TAccountSelectorDataSource;
begin
  Result := TAccountSelectorDataSource.Create(Self);
end;

function TFRMAccountSelect.AccountSafeBox: TPCSafeBox;
begin
  Result := FSafeBox;
end;

function TFRMAccountSelect.CreateStatsTask: TAccountSelectorStatsTask;
begin
  Result := TAccountSelectorStatsTask.Create(FSource);
end;

procedure TFRMAccountSelect.FormCreate(Sender: TObject);

  procedure AddColumn(const ACaption, ABinding: String; AWidth: Integer);
  begin
    with FAccountsGrid.AddColumn(ACaption) do begin
      Binding := ABinding;
      Width := AWidth;
    end;
  end;

begin
  FChangingControls := True;
  FDefaultAccount := -1;
  FKeyChoices := TObjectList<TKeyChoice>.Create(True);
  FAccountsGrid := TVisualGrid.Create(Self);
  FAccountsGrid.Name := 'AccountSelectorGrid';
  FAccountsGrid.Parent := Self;
  FAccountsGrid.Align := alClient;
  FAccountsGrid.CanSearch := False;
  FAccountsGrid.SortMode := smNone;
  FAccountsGrid.PageMode := vpmAuto;
  FAccountsGrid.SelectionType := stRow;
  FAccountsGrid.DeselectionType := dtDefault;
  FAccountsGrid.Options := [vgoColSizing,vgoAutoHidePaging];
  FAccountsGrid.OnFinishedUpdating := OnAccountsGridUpdated;
  FAccountsGrid.OnSelection := OnGridSelection;
  AddColumn('Account No','Account',95);
  AddColumn('Name','Name',140);
  AddColumn('Balance','BalanceDecimal',105);
  AddColumn('N Op.','NOperation',55);
  AddColumn('Type','Type',55);
  AddColumn('Price','PriceDecimal',95);
  AddColumn('State','State',70);
  dgAccounts.Visible := False;
  FSource := CreateAccountSource;
  cbMyAccounts.OnClick := cbMyAccountsClick;
  {$IFDEF FPC}cbMyAccounts.OnChange := nil;{$ENDIF}
  // Opening a combo is not a key change; only its selection event refreshes.
  cbMyPrivateKeys.OnClick := nil;
  cbMyPrivateKeys.OnChange := cbMyPrivateKeysChange;
  cbOnlyForSale.OnClick := cbMyAccountsClick;
  cbOnlyForPrivateSaleToMe.OnClick := cbMyAccountsClick;
  cbOnlyForPublicSale.OnClick := cbMyAccountsClick;
  cbAccountsBalance.OnClick := cbMyAccountsClick;
  cbAccountsName.OnClick := cbMyAccountsClick;
  FChangingControls := False;
  UpdateControls;
  lblAccountsCount.Caption := '-';
  lblAccountsBalance.Caption := '-';
  AllowSelect := False;
end;

procedure TFRMAccountSelect.FormDestroy(Sender: TObject);
begin
  FStopping := True;
  // The existing task owns its queued completion and cooperatively joins; no
  // worker can call Synchronize while this form is releasing its receivers.
  if Assigned(FStatsTask) then begin
    FStatsTask.OnCompleted := nil;
    FStatsTask.CancelAndWait;
    FreeAndNil(FStatsTask);
  end;
  if Assigned(FAccountsGrid) then begin
    FAccountsGrid.OnFinishedUpdating := nil;
    FAccountsGrid.OnSelection := nil;
    FreeAndNil(FAccountsGrid);
  end;
  FreeAndNil(FSource);
  FKeyChoices.Free;
end;

procedure TFRMAccountSelect.SetNode(const Value: TNode);
begin
  FNode := Value;
  FSafeBox := nil;
  if Assigned(FNode) then FSafeBox := FNode.Bank.SafeBox;
  SearchFiltered;
end;

procedure TFRMAccountSelect.SetWalletKeys(const Value: TWalletKeys);
begin
  FWalletKeys := Value;
  UpdateControls;
  SearchFiltered;
end;

procedure TFRMAccountSelect.UpdateControls;
var LSelected: TAccountKey; LHasSelected: Boolean; LChoice: TKeyChoice; I: Integer;
begin
  LHasSelected := (cbMyPrivateKeys.ItemIndex > 0) and
    (cbMyPrivateKeys.ItemIndex < cbMyPrivateKeys.Items.Count);
  if LHasSelected then
    LSelected := TKeyChoice(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]).Key;
  FChangingControls := True;
  cbMyPrivateKeys.Items.BeginUpdate;
  try
    cbMyPrivateKeys.Clear;
    FKeyChoices.Clear;
    cbMyAccounts.Enabled := Assigned(FWalletKeys);
    if not Assigned(FWalletKeys) then cbMyAccounts.Checked := False;
    cbMyPrivateKeys.Sorted := True;
    if Assigned(FWalletKeys) then
      for I := 0 to FWalletKeys.Count-1 do begin
        LChoice := TKeyChoice.Create;
        LChoice.Key := FWalletKeys.Key[I].AccountKey;
        LChoice.Available := True;
        FKeyChoices.Add(LChoice);
        cbMyPrivateKeys.Items.AddObject(FWalletKeys.Key[I].Name,LChoice);
      end;
    cbMyPrivateKeys.Sorted := False;
    cbMyPrivateKeys.Items.InsertObject(0,'(All)',nil);
    cbMyPrivateKeys.ItemIndex := 0;
    if LHasSelected then
      for I := 1 to cbMyPrivateKeys.Items.Count-1 do
        if TAccountComp.EqualAccountKeys(LSelected,TKeyChoice(cbMyPrivateKeys.Items.Objects[I]).Key) then begin
          cbMyPrivateKeys.ItemIndex := I;
          Break;
        end;
    if LHasSelected and (cbMyPrivateKeys.ItemIndex = 0) then begin
      // A removed key is an unavailable selection, never an implicit All.
      LChoice := TKeyChoice.Create;
      LChoice.Key := LSelected;
      FKeyChoices.Add(LChoice);
      cbMyPrivateKeys.Items.InsertObject(1,'(Unavailable key)',LChoice);
      cbMyPrivateKeys.ItemIndex := 1;
    end;
    cbMyPrivateKeys.Enabled := cbMyAccounts.Checked and Assigned(FWalletKeys);
  finally
    cbMyPrivateKeys.Items.EndUpdate;
    FChangingControls := False;
  end;
end;

procedure TFRMAccountSelect.SearchFiltered;
var LFilter: TAccountSelectorFilter; I,LKeyCount: Integer;
begin
  if FStopping or FChangingControls or not Assigned(FSource) then Exit;
  LFilter := Default(TAccountSelectorFilter);
  LFilter.MinBalance := 0;
  LFilter.MaxBalance := -1;
  LFilter.MyAccounts := cbMyAccounts.Checked and Assigned(FWalletKeys);
  // These public identities were captured by the UI-owned WalletKeys setter.
  // Searching never follows mutable wallet positions or subscribes controls
  // to key notifications which may originate on an RPC worker.
  SetLength(LFilter.WalletKeys,FKeyChoices.Count);
  LKeyCount := 0;
  for I := 0 to FKeyChoices.Count-1 do
    if FKeyChoices[I].Available then begin
      LFilter.WalletKeys[LKeyCount] := FKeyChoices[I].Key;
      Inc(LKeyCount);
    end;
  SetLength(LFilter.WalletKeys,LKeyCount);
  if LFilter.MyAccounts then begin
    LFilter.SpecificOwner := cbMyPrivateKeys.ItemIndex > 0;
    if LFilter.SpecificOwner then begin
      if (cbMyPrivateKeys.ItemIndex < cbMyPrivateKeys.Items.Count) and
        TKeyChoice(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]).Available then begin
        SetLength(LFilter.OwnerKeys,1);
        LFilter.OwnerKeys[0] := TKeyChoice(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]).Key;
      end;
    end else LFilter.OwnerKeys := Copy(LFilter.WalletKeys);
  end;
  LFilter.OnlyForSale := cbOnlyForSale.Checked;
  LFilter.OnlyForPublicSale := cbOnlyForPublicSale.Checked;
  LFilter.OnlyForPrivateSaleToMe := cbOnlyForPrivateSaleToMe.Checked;
  if cbAccountsBalance.Checked then begin
    if not TAccountComp.TxtToMoney(ebMinBalance.Text,LFilter.MinBalance) then
      raise Exception.Create('Invalid Min Balance');
    ebMinBalance.Text := TAccountComp.FormatMoney(LFilter.MinBalance);
    if Trim(ebMaxBalance.Text) <> '' then begin
      if not TAccountComp.TxtToMoney(ebMaxBalance.Text,LFilter.MaxBalance) then
        raise Exception.Create('Invalid Max Balance');
      ebMaxBalance.Text := TAccountComp.FormatMoney(LFilter.MaxBalance);
    end;
  end;
  if cbAccountsName.Checked then LFilter.Name.FromString(ebAccountName.Text);
  cbMyPrivateKeys.Enabled := LFilter.MyAccounts;
  if LFilter.MyAccounts then cbMyPrivateKeys.ParentFont := True
  else cbMyPrivateKeys.Font.Color := clGray;
  if cbAccountsBalance.Checked then begin
    ebMinBalance.ParentFont := True;
    ebMaxBalance.ParentFont := True;
  end else begin
    ebMinBalance.Font.Color := clGray;
    ebMaxBalance.Font.Color := clGray;
  end;
  if cbAccountsName.Checked then ebAccountName.ParentFont := True
  else ebAccountName.Font.Color := clGray;
  FDefaultSelectionApplied := False;
  bbSelect.Enabled := False;
  lblAccountsCount.Caption := '-';
  lblAccountsBalance.Caption := '-';
  // Retain the latest input and cancel an obsolete provider without joining it.
  // The grid discards its stale result before admitting this generation.
  FAccountsGrid.RefreshSuspended := True;
  FSource.Configure(AccountSafeBox,LFilter);
  FAccountsGrid.PageIndex := 0;
  if not Assigned(FAccountsGrid.DataSource) then FAccountsGrid.DataSource := FSource
  else FAccountsGrid.RefreshGrid;
  if not StatsRunning then FAccountsGrid.RefreshSuspended := False;
end;

procedure TFRMAccountSelect.SetDefaultAccount(AValue: Int64);
begin
  FDefaultAccount := AValue;
  FDefaultSelectionApplied := False;
end;

procedure TFRMAccountSelect.SetAllowSelect(AValue: Boolean);
begin
  FAllowSelect := AValue;
  bbSelect.Visible := AValue;
  bbSelect.Enabled := AValue and (GetSelected >= 0) and not StatsRunning;
end;

procedure TFRMAccountSelect.SetFilters(AValue: Integer);
begin
  FFilters := AValue;
  FChangingControls := True;
  try
    cbMyAccounts.Checked := (AValue and CT_AS_MyAccounts) <> 0;
    cbOnlyForSale.Checked := (AValue and CT_AS_OnlyForSale) <> 0;
  finally
    FChangingControls := False;
  end;
  SearchFiltered;
end;

procedure TFRMAccountSelect.OnAccountsGridUpdated(Sender: TObject);
var LTotal: TAccountSelectorTotal; LIndex,LPage,I: Integer;
  LDeselection: TDeselectionType;
begin
  if FStopping then Exit;
  if FSource.TryGetSummary(LTotal) then begin
    lblAccountsCount.Caption := IntToStr(LTotal.Accounts);
    lblAccountsBalance.Caption := TAccountComp.FormatMoney(LTotal.Balance);
  end;
  if not FDefaultSelectionApplied and (FDefaultAccount >= 0) then begin
    LIndex := FSource.PublishedAccountIndex(FDefaultAccount);
    if LIndex >= 0 then begin
      LPage := 0;
      if FAccountsGrid.PageMode <> vpmSingle then LPage := LIndex div FAccountsGrid.PageSize;
      if LPage <> FAccountsGrid.PageIndex then begin
        FAccountsGrid.PageIndex := LPage;
        Exit;
      end;
      for I := 0 to FAccountsGrid.RowCount-1 do
        if Int64(TDataRowData(FAccountsGrid.Rows[I])['AccountNumber']) = FDefaultAccount then begin
          LDeselection := FAccountsGrid.DeselectionType;
          FAccountsGrid.DeselectionType := dtNone;
          FAccountsGrid.InternalDrawGrid.Row := I+1;
          {$IFNDEF FPC}
          TSelectorNativeGridAccess(FAccountsGrid.InternalDrawGrid).PublishCurrentSelection;
          {$ENDIF}
          FAccountsGrid.DeselectionType := LDeselection;
          Break;
        end;
    end;
    FDefaultSelectionApplied := True;
  end;
  bbSelect.Enabled := FAllowSelect and (GetSelected >= 0) and not StatsRunning;
end;

procedure TFRMAccountSelect.OnGridSelection(Sender: TObject;
  {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection);
begin
  bbSelect.Enabled := FAllowSelect and (GetSelected >= 0) and not StatsRunning;
end;

function TFRMAccountSelect.GetSelected: Int64;
var LRows: TArray<Variant>;
begin
  Result := -1;
  if not Assigned(FAccountsGrid) or FAccountsGrid.IsUpdating then Exit;
  LRows := FAccountsGrid.SelectedRows;
  if Length(LRows) = 1 then Result := Int64(TDataRowData(LRows[0])['AccountNumber']);
end;

function TFRMAccountSelect.GetStatsRunning: Boolean;
begin
  Result := Assigned(FStatsTask);
end;

procedure TFRMAccountSelect.bbSearchClick(Sender: TObject);
begin
  SearchFiltered;
end;

procedure TFRMAccountSelect.cbMyAccountsChange(Sender: TObject);
begin
  SearchFiltered;
end;

procedure TFRMAccountSelect.cbMyAccountsClick(Sender: TObject);
begin
  SearchFiltered;
end;

procedure TFRMAccountSelect.cbMyPrivateKeysChange(Sender: TObject);
begin
  SearchFiltered;
end;

procedure TFRMAccountSelect.ebMinBalanceExit(Sender: TObject);
begin
  if ebMinBalance.Text <> '' then begin
    cbAccountsBalance.Checked := True;
    SearchFiltered;
  end;
end;

procedure TFRMAccountSelect.ebMaxBalanceExit(Sender: TObject);
begin
  if ebMaxBalance.Text <> '' then begin
    cbAccountsBalance.Checked := True;
    SearchFiltered;
  end;
end;

procedure TFRMAccountSelect.bbTypeStatsClick(Sender: TObject);
begin
  if FStopping or StatsRunning or not Assigned(FSource) then Exit;
  FAccountsGrid.RefreshSuspended := True;
  FAccountsGrid.ShowLoading('Please Wait','Calculating account type statistics');
  bbTypeStats.Enabled := False;
  bbSelect.Enabled := False;
  try
    FStatsTask := CreateStatsTask;
    FStatsTask.OnCompleted := OnStatsCompleted;
    FStatsTask.Start;
  except
    FreeAndNil(FStatsTask);
    FAccountsGrid.HideLoading;
    FAccountsGrid.RefreshSuspended := False;
    bbTypeStats.Enabled := True;
    raise;
  end;
end;

procedure TFRMAccountSelect.OnStatsCompleted(Sender: TObject);
var LTask: TAccountSelectorStatsTask; LStats: TAccountSelectorTypeStats; LError: String;
begin
  if FStopping or (Sender <> FStatsTask) then Exit;
  LTask := FStatsTask;
  FStatsTask := nil;
  LStats := LTask.Stats;
  LError := LTask.ErrorMessage;
  LTask.Free;
  FAccountsGrid.HideLoading;
  FAccountsGrid.RefreshSuspended := False;
  bbTypeStats.Enabled := True;
  if LError <> '' then ShowMessage(LError)
  else ShowTypeStatsReport(LStats);
end;

procedure TFRMAccountSelect.BuildTypeStatsReport(const AStats: TAccountSelectorTypeStats; AText: TStrings);
var K,J: Integer;
begin
  AText.Clear;
  AText.Add('*** Account type stats ***');
  AText.Add(Format('Current block:%d accounts:%d coins:%s',
    [AStats.Chain.Blocks,AStats.Chain.Accounts,TAccountComp.FormatMoney(AStats.Chain.Balance)]));
  AText.Add('');
  AText.Add('** Basic stats **');
  AText.Add(Format('Account type=0 accounts:%d coins:%s',
    [AStats.Zero.Accounts,TAccountComp.FormatMoney(AStats.Zero.Balance)]));
  AText.Add(Format('Account type [1..9999] accounts:%d coins:%s',
    [AStats.Invalid.Accounts,TAccountComp.FormatMoney(AStats.Invalid.Balance)]));
  AText.Add('');
  AText.Add('*** Stats by group ***');
  for K := 0 to 6 do begin
    AText.Add('');
    AText.Add(Format('** Group [%d..%d] **',[K*10000,(K+1)*10000-1]));
    for J := 0 to 9 do AText.Add(Format('Account type like %d%d*** accounts:%d coins:%s',
      [K,J,AStats.Digits[K,1,J].Accounts,TAccountComp.FormatMoney(AStats.Digits[K,1,J].Balance)]));
    for J := 0 to 9 do AText.Add(Format('Account type like %d*%d** accounts:%d coins:%s',
      [K,J,AStats.Digits[K,2,J].Accounts,TAccountComp.FormatMoney(AStats.Digits[K,2,J].Balance)]));
    for J := 0 to 9 do AText.Add(Format('Account type like %d**%d* accounts:%d coins:%s',
      [K,J,AStats.Digits[K,3,J].Accounts,TAccountComp.FormatMoney(AStats.Digits[K,3,J].Balance)]));
    for J := 0 to 9 do AText.Add(Format('Account type like %d***%d accounts:%d coins:%s',
      [K,J,AStats.Digits[K,4,J].Accounts,TAccountComp.FormatMoney(AStats.Digits[K,4,J].Balance)]));
  end;
end;

procedure TFRMAccountSelect.ShowTypeStatsReport(const AStats: TAccountSelectorTypeStats);
var LText: TStringList; LForm: TFRMMemoText;
begin
  // Keep the established global report and wording; only computation moved off
  // the UI thread. Formatting this bounded report is presentation work.
  LText := TStringList.Create;
  try
    BuildTypeStatsReport(AStats,LText);
    LForm := TFRMMemoText.Create(Self);
    try
      LForm.InitData('Account Type Stats',LText.Text);
      LForm.ShowModal;
    finally
      LForm.Free;
    end;
  finally
    LText.Free;
  end;
end;

end.
