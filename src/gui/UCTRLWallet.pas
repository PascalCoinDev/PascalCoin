unit UCTRLWallet;

{$mode delphi}
{$modeswitch nestedprocvars}

{ Copyright (c) 2018 Sphere 10 Software

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Herman Schoenfeld: unit creator, implementation
}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, PairSplitter, Buttons, UVisualGrid, UCommon.UI, Generics.Collections, ULog,
  UAccounts, UDataSources, UNode, UCoreObjects, UCoreUtils, UWIZSendPASC, UWIZChangeKey, UWIZEnlistAccountForSale;

type

  { TCTRLWallet }

  TCTRLWalletAccountsMode = (wamMyAccounts, wamFirstAccount);
  TCTRLWalletOperationsMode = (womSelectedAccounts, womAllAccounts);
  TCTRLWalletOperationsHistory = (woh7Days, woh30Days, wohFullHistory);

  TCTRLWallet = class(TApplicationForm)
    cbAccounts: TComboBox;
    gpMyAccounts: TGroupBox;
    gpRecentOps: TGroupBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblTotalPASA: TLabel;
    lblTotalPASC: TLabel;
    miCopyOphash: TMenuItem;
    miOperationInfo: TMenuItem;
    miSendPASC: TMenuItem;
    miChangeKey: TMenuItem;
    miAccountsMarket: TMenuItem;
    miEnlistAccountsForSale: TMenuItem;
    miDelistAccountsFromSale: TMenuItem;
    miAccountInfo: TMenuItem;
    miSep1: TMenuItem;
    miSep2: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    paAccounts: TPanel;
    paOperations: TPanel;
    mnuAccountsPopup: TPopupMenu;
    mnuOperationsPopup: TPopupMenu;
    mnuFirstAccountPopup: TPopupMenu;
    procedure cbAccountsChange(Sender: TObject);
    procedure cmbDurationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miAccountInfoClick(Sender: TObject);
    procedure miChangeKeyClick(Sender: TObject);
    procedure miCopyOphashClick(Sender: TObject);
    procedure miOperationInfoClick(Sender: TObject);
    procedure miSendPASCClick(Sender: TObject);
    procedure miEnlistAccountsForSaleClick(Sender: TObject);
    procedure miDelistAccountsFromSaleClick(Sender: TObject);
  private
    FNodeNotifyEvents: TNodeNotifyEvents;
    FAccountsMode: TCTRLWalletAccountsMode;
    FOperationsMode: TCTRLWalletOperationsMode;
    FOperationsHistory: TCTRLWalletOperationsHistory;
    FAccountsGrid: TVisualGrid;
    FOperationsGrid: TVisualGrid;
    FBalance : TBalanceSummary;
    FAccountsDataSource: TMyAccountsDataSource;
    FOperationsDataSource: TAccountsOperationsDataSource;
    procedure SetAccountsMode(AMode: TCTRLWalletAccountsMode);
    procedure SetOperationsMode(AMode: TCTRLWalletOperationsMode);
    procedure SetOperationsHistory(AHistory: TCTRLWalletOperationsHistory);
    procedure RefreshMyAccountsCombo;
    procedure RefreshTotals;
    procedure RefreshAccountsGrid;
    procedure RefreshOperationsGrid;
    function GetSelectedAccounts : TArray<Cardinal>;
  protected
    procedure ActivateFirstTime; override;
    procedure OnPrivateKeysChanged(Sender: TObject);
    procedure OnUserKeyActivityDetected(Sender: TObject);
    procedure OnNodeBlocksChanged(Sender: TObject);
    procedure OnNodeNewOperation(Sender: TObject);
    procedure OnAccountsSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
    procedure OnAccountsGridFinishedUpdating(Sender: TObject);
    procedure OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
    procedure OnPrepareAccountPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
    procedure OnPrepareOperationsPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
  public
    property SelectedAccounts : TArray<Cardinal> read GetSelectedAccounts;
    property AccountsMode: TCTRLWalletAccountsMode read FAccountsMode write SetAccountsMode;
    property OperationsMode: TCTRLWalletOperationsMode read FOperationsMode write SetOperationsMode;
    property OperationsHistory: TCTRLWalletOperationsHistory read FOperationsHistory write SetOperationsHistory;
  end;

implementation

uses
  UUserInterface, UCellRenderers, UBlockChain, UWallet, UCrypto,
  UCommon, UMemory, Generics.Defaults, UCommon.Data, UCommon.Collections, UWIZOperation;

{$R *.lfm}

{ TCTRLWallet }

procedure TCTRLWallet.FormCreate(Sender: TObject);
var
  cmbDuration: TComboBox;
begin
  // event registrations
  FNodeNotifyEvents := TNodeNotifyEvents.Create(self);
  FNodeNotifyEvents.WatchKeys := TWallet.Keys.AccountsKeyList;
  FNodeNotifyEvents.OnKeyActivity:= OnUserKeyActivityDetected;
  FNodeNotifyEvents.OnBlocksChanged := OnNodeBlocksChanged;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;
  TWallet.Keys.OnChanged.Add(OnPrivateKeysChanged);
  TWallet.Keys.AccountsKeyList.ClearAccountKeyChanges;   // XXXXX CLEAR BUFFER on start

  // fields
  FAccountsDataSource := TMyAccountsDataSource.Create(Self);
  FAccountsDataSource.BalancePointer := @FBalance;
  FOperationsDataSource := TAccountsOperationsDataSource.Create(Self);
  FOperationsDataSource.Accounts := TCoreTool.GetUserAccountNumbers;
  FOperationsDataSource.BlockDepth:=TTimeSpan.FromDays(7).TotalBlockCount;
  FOperationsHistory := woh7Days;
  FOperationsMode:= womAllAccounts;
  FAccountsMode := wamMyAccounts;

  // grids
  FAccountsGrid := TVisualGrid.Create(Self);
  FAccountsGrid.SortMode := smSingleColumn;
  FAccountsGrid.FetchDataInThread := true;
  FAccountsGrid.AutoPageSize := True;
  FAccountsGrid.DeselectionType := dtDefault;
  FAccountsGrid.SelectionType := stMultiRow;
  FAccountsGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging, vgoAutoHideSearchPanel];
  with FAccountsGrid.AddColumn('Account') do
  begin
    Binding := 'AccountNumber';
    SortBinding := 'AccountNumber';
    DisplayBinding := 'Account';
    Width := 100;
    HeaderFontStyles := [fsBold];
    DataFontStyles := [fsBold];
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FAccountsGrid.AddColumn('Name') do
  begin
    StretchedToFill := True;
    HeaderAlignment := taCenter;
    Filters := SORTABLE_TEXT_FILTER;
  end;
  with FAccountsGrid.AddColumn('Balance') do
  begin
    Binding := 'BalanceDecimal';
    SortBinding := 'Balance';
    DisplayBinding := 'Balance';
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Renderer := TCellRenderers.PASC;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  FAccountsGRid.OnFinishedUpdating:= OnAccountsGridFinishedUpdating;
  FAccountsGrid.OnSelection := OnAccountsSelected;
  FAccountsGrid.OnPreparePopupMenu := OnPrepareAccountPopupMenu;

  FOperationsGrid := TVisualGrid.Create(Self);
  FOperationsGrid.SortMode := smMultiColumn;
  FOperationsGrid.FetchDataInThread := true;
  FOperationsGrid.AutoPageSize := True;
  FOperationsGrid.DeselectionType := dtDefault;
  FOperationsGrid.SelectionType := stRow;
  FOperationsGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging, vgoAutoHideSearchPanel];
  with FOperationsGrid.AddColumn('Time') do
  begin
    SortBinding := 'UnixTime';
    DisplayBinding := 'UnixTime';
    Renderer := TCellRenderers.OperationTime;
    Width := 130;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FOperationsGrid.AddColumn('Block') do
  begin
    Binding := 'BlockLocation';
    SortBinding := 'BlockLocationSortable';
    AutoWidth := True;
    Filters := SORTABLE_TEXT_FILTER;
  end;
  with FOperationsGrid.AddColumn('Account') do
  begin
    Binding := 'AccountNumber';
    DisplayBinding := 'Account';
    Width := 100;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FOperationsGrid.AddColumn('Type') do
  begin
    Sanitizer := TCellRenderers.OperationTypeSanitizer;
    Width := 80;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FOperationsGrid.AddColumn('Amount') do
  begin
    Binding := 'AmountDecimal';
    SortBinding := 'Amount';
    DisplayBinding := 'Amount';
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment:=taRightJustify;
    Renderer := TCellRenderers.PASC_CheckPendingBalance;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FOperationsGrid.AddColumn('Fee') do
  begin
    Binding := 'FeeDecimal';
    SortBinding := 'Fee';
    DisplayBinding := 'Fee';
    AutoWidth := True;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Renderer := TCellRenderers.PASC_CheckPendingBalance;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FOperationsGrid.AddColumn('Balance') do
  begin
    Binding := 'BalanceDecimal';
    SortBinding := 'Balance';
    DisplayBinding := 'Balance';
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Renderer := TCellRenderers.PASC_CheckPendingBalance;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FOperationsGrid.AddColumn('Payload') do
  begin
    AutoWidth := True;
    Renderer := TCellRenderers.Payload;
    Filters := SORTABLE_TEXT_FILTER;
  end;
  with FOperationsGrid.AddColumn('OPHASH') do
  begin
    Width := 80;
    Renderer := TCellRenderers.OPHASH;
    Filters := SORTABLE_TEXT_FILTER;
  end;
  with FOperationsGrid.AddColumn('Description') do
  begin
    StretchedToFill := True;
    Filters := SORTABLE_TEXT_FILTER;
  end;
  FOperationsGrid.OnSelection := OnOperationSelected;
  FOperationsGrid.OnPreparePopupMenu := OnPrepareOperationsPopupMenu;
  FOperationsGrid.Caption.Alignment := taCenter;
  FOperationsGrid.Caption.Text := 'All Account Operations';
  FOperationsGrid.Caption.Visible := True;

  // key combo
  RefreshMyAccountsCombo;

  // duration combo
  cmbDuration := TComboBox.Create(FOperationsGrid);
  cmbDuration.ReadOnly := True;
  cmbDuration.Items.BeginUpdate;
  try
    cmbDuration.AddItem('7 Days', TObject(woh7Days));
    cmbDuration.AddItem('30 Days', TObject(woh30Days));
    cmbDuration.AddItem('Maximum', TObject(wohFullHistory));
  finally
    cmbDuration.Items.EndUpdate;
    cmbDuration.ItemIndex := 0;
  end;
  cmbDuration.OnChange := cmbDurationChange;
  FOperationsGrid.WidgetControl := cmbDuration;

  // NOTE: datasources are assigned to grid in FormResize

  // Add grid to panels
  paAccounts.AddControlDockCenter(FAccountsGrid);
  paOperations.AddControlDockCenter(FOperationsGrid);
end;

procedure TCTRLWallet.FormDestroy(Sender: TObject);
begin
  TWallet.Keys.OnChanged.Add(OnPrivateKeysChanged);
end;

procedure TCTRLWallet.FormResize(Sender: TObject);
var x : integer;
begin
  // Grid data-sources are set here on "first form resize" in order to avoid
  // excessive datasource fetching during initialising sequence. Note, grid
  // refreshes on size changed, blockchain activity and when assigned to grid
  if NOT Assigned(FAccountsGrid.DataSource) then
    FAccountsGrid.DataSource := FAccountsDataSource;

  if NOT Assigned(FOperationsGrid.DataSource) then
    FOperationsGrid.DataSource := FOperationsDataSource;
end;

procedure TCTRLWallet.ActivateFirstTime;
begin
  // add first time-init here
end;

procedure TCTRLWallet.RefreshTotals;

begin
  lblTotalPASC.Caption := TAccountComp.FormatMoney(FBalance.TotalPASC);
  lblTotalPASA.Caption := Format('%d', [FBalance.TotalPASA]);
end;

procedure TCTRLWallet.RefreshMyAccountsCombo;
var
  i: integer;
  selectFirst, selectLast: boolean;
  last_key: TAccountKey;
  key: TWalletKey;
  str: ansistring;
begin
  // determine current selection
  if cbAccounts.ItemIndex >= 1 then begin
    if cbAccounts.ItemIndex < cbAccounts.Items.Count - 1 then begin
      last_key := TBox<TAccountKey>(
        cbAccounts.Items.Objects[cbAccounts.ItemIndex]).Value;
      selectFirst := False;
      selectLast := False;
    end else begin
      selectFirst := False;
      selectLast := True;
    end;
  end else begin
    selectFirst := True;
    selectLast := False;
  end;

  // update combo items
  cbAccounts.items.BeginUpdate;
  try
    // free existing items
    for i := 0 to cbAccounts.Items.Count - 1 do
      cbAccounts.Items.Objects[i].Free;
    cbAccounts.Items.Clear;
    // add new items
    for i := 0 to TWallet.Keys.Count - 1 do begin
      // get i'th key
      key := TWallet.Keys.Key[i];
      // fix name
      if (key.Name = '') then begin
        str := 'Sha256=' + TCrypto.ToHexaString(TCrypto.DoSha256(
          TAccountComp.AccountKey2RawString(key.AccountKey)));
      end else begin
        str := key.Name;
      end;
      if not Assigned(key.PrivateKey) then
        str := str + '(*)';
      cbAccounts.Items.AddObject(str, TBox<TAccountKey>.Create(key.AccountKey));
    end;
    cbAccounts.Items.InsertObject(0, 'Show All', TBox<TAccountKey>.Create);
    cbAccounts.Items.AddObject('Get An Account', TBox<TAccountKey>.Create);
  finally
    cbAccounts.Items.EndUpdate;
  end;
  // re-select previous selection
  if selectFirst then
    cbAccounts.ItemIndex := 0
  else if selectLast then
    cbAccounts.ItemIndex := cbAccounts.Items.Count - 1
  else begin
    for i := 1 to cbAccounts.Items.Count - 2 do begin
      if TAccountKeyEqualityComparer.AreEqual(TBox<TAccountKey>( cbAccounts.Items.Objects[i]).Value, last_key) then begin
        cbAccounts.ItemIndex := i;
        exit;
      end;
    end;
  end;
end;

procedure TCTRLWallet.RefreshAccountsGrid;
var
  index: integer;
  sel: TBox<TAccountKey>;
begin
  if Self.AccountsMode <> wamMyAccounts then exit; // showing getpasa
  if cbAccounts.ItemIndex = cbAccounts.Items.Count - 1 then exit; // not a key
  index := cbAccounts.ItemIndex;
  if index = 0 then begin
    gpMyAccounts.Caption := 'My Accounts';
    FAccountsDataSource.FilterKeys := TWallet.Keys.AccountsKeyList.ToArray;
  end else begin
    sel := TBox<TAccountKey>(cbAccounts.Items.Objects[cbAccounts.ItemIndex]);
    gpMyAccounts.Caption := Format('%s Accounts', [TWallet.Keys[TWallet.Keys.IndexOfAccountKey(sel.Value)].Name]);
    FAccountsDataSource.FilterKeys := TArray<TAccountKey>.Create(sel.Value);
  end;
  FAccountsGrid.RefreshGrid;
end;

procedure TCTRLWallet.RefreshOperationsGrid;
begin
  case FOperationsMode of
    womAllAccounts: begin
      FOperationsGrid.Caption.Text := '';
      FOperationsDataSource.Accounts := TCoreTool.GetUserAccountNumbers;
    end;
    womSelectedAccounts:
    begin
      FOperationsGrid.Caption.Text := 'Selected Accounts';
      FOperationsDataSource.Accounts := SelectedAccounts;
    end else
      raise ENotSupportedException.Create(Format('AMode %d not supported', [integer(FOperationsMode)]));
  end;
  FOperationsGrid.RefreshGrid;
end;

function TCTRLWallet.GetSelectedAccounts : TArray<Cardinal>;

  function GetAccNoWithoutChecksum(constref ARow: variant): cardinal;
  begin
    Result := ARow.__KEY;
  end;

begin
  Result := TListTool<Variant, Cardinal>.Transform(FAccountsGrid.SelectedRows, GetAccNoWithoutChecksum);
end;

procedure TCTRLWallet.SetAccountsMode(AMode: TCTRLWalletAccountsMode);
begin
  if FAccountsMode = AMode then exit;

  FUILock.Acquire;
  try
    FAccountsMode := AMode;
    paAccounts.RemoveAllControls(False);
    case AMode of
      wamMyAccounts:
      begin
        // reset account combo
        cbAccounts.OnChange := nil; // disable event
        cbAccounts.ItemIndex :=0;
        cbAccounts.OnChange := cbAccountsChange; // re-enable event
        // ensure on accounts panel
        if FAccountsGrid.Parent <> paAccounts then begin
          paAccounts.RemoveAllControls(False);
          paAccounts.AddControlDockCenter(FAccountsGrid);
        end;
        // Refresh grid
        FAccountsGrid.ClearSelection();
        RefreshAccountsGrid;
      end;
      wamFirstAccount: raise Exception.Create('Not implemented');
    end;
  finally
    FUILock.Release;
  end;
end;

procedure TCTRLWallet.SetOperationsMode(AMode: TCTRLWalletOperationsMode);
begin
  if FOperationsMode = AMode then exit;
  FUILock.Acquire;
  try
    FOperationsMode := AMode;
    case AMode of
     womSelectedAccounts: FOperationsDataSource.Accounts := SelectedAccounts;
     womAllAccounts: FOperationsDataSource.Accounts := TCoreTool.GetUserAccountNumbers;
    end;
    RefreshOperationsGrid;
  finally
    FUILock.Release;
  end;
end;

procedure TCTRLWallet.SetOperationsHistory(AHistory: TCTRLWalletOperationsHistory);
begin
  FOperationsHistory := AHistory;
  case FOperationsHistory of
    woh7Days: FOperationsDataSource.BlockDepth := TTimeSpan.FromDays(7).TotalBlockCount;
    woh30Days: FOperationsDataSource.BlockDepth := TTimeSpan.FromDays(30).TotalBlockCount;
    wohFullHistory: FOperationsDataSource.BlockDepth := TTimeSpan.FromDays(10 * 365).TotalBlockCount;
  end;
  FOperationsGrid.RefreshGrid;
end;

procedure TCTRLWallet.OnPrivateKeysChanged(Sender: TObject);
begin
  RefreshMyAccountsCombo;
end;

procedure TCTRLWallet.OnUserKeyActivityDetected;
begin
//  if NOT TUserInterface.Node.HasBestKnownBlockchainTip then
//    exit; // node syncing
  FAccountsGrid.RefreshGrid;
  FOperationsGrid.RefreshGrid;
end;

procedure TCTRLWallet.OnNodeBlocksChanged(Sender: TObject);
begin
  // TODO: play block sound chime
end;

procedure TCTRLWallet.OnNodeNewOperation(Sender: TObject);
begin
  // TODO: play operation sound tick
end;

procedure TCTRLWallet.OnAccountsGridFinishedUpdating(Sender: TObject);
begin
  RefreshTotals; // totals are updated by datasource, via a pointer
end;

procedure TCTRLWallet.OnAccountsSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
begin
  if ASelection.Page >= 0 then
    if FOperationsMode <> womSelectedAccounts then
      OperationsMode := womSelectedAccounts
    else
      RefreshOperationsGrid
  else
    OperationsMode := womAllAccounts;
end;

procedure TCTRLWallet.OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
var
  row: longint;
  v: variant;
  ophash: ansistring;
begin
  if ASelection.Page < 0 then exit;
  row := ASelection.Row;
  if (row >= 0) and (row < FOperationsGrid.RowCount) then begin
    v := FOperationsGrid.Rows[row];
    ophash := FOperationsGrid.Rows[row].OPHASH;
    if TPCOperation.IsValidOperationHash(ophash) then begin
      TUserInterface.ShowOperationInfoDialog(self, ophash);
      FOperationsGrid.ClearSelection;
    end;
  end;
end;

procedure TCTRLWallet.cbAccountsChange(Sender: TObject);
begin
  if cbAccounts.ItemIndex < 0 then exit;
  FAccountsGrid.ClearSelection();
  if cbAccounts.ItemIndex = cbAccounts.Items.Count - 1 then
    AccountsMode := wamFirstAccount
  else if FAccountsMode = wamFirstAccount then
    AccountsMode := wamMyAccounts
  else
    RefreshAccountsGrid;
  if FOperationsMode <> womAllAccounts then
    RefreshOperationsGrid;
end;

procedure TCTRLWallet.cmbDurationChange(Sender: TObject);
var
  cmbDuration: TComboBox;
begin
  cmbDuration := Sender as TComboBox;
  if not Assigned(cmbDuration) then
    exit;

  case cmbDuration.ItemIndex of
    0: OperationsHistory := woh7Days;
    1: OperationsHistory := woh30Days;
    2: OperationsHistory := wohFullHistory;
  end;
end;

procedure TCTRLWallet.OnPrepareAccountPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
var
  accNo: cardinal;
  account: TAccount;
begin
  miSep1.Visible := ASelection.RowCount = 1;
  miAccountInfo.Visible := ASelection.RowCount = 1;
  miSendPASC.Caption := IIF(ASelection.RowCount = 1, 'Send PASC', 'Send All PASC');
  miChangeKey.Caption := IIF(ASelection.RowCount = 1, 'Change Key', 'Change All Key');
  miEnlistAccountsForSale.Caption := IIF(ASelection.RowCount = 1, 'Enlist Account For Sale', 'Enlist All Account For Sale');
  miDelistAccountsFromSale.Caption := IIF(ASelection.RowCount = 1, 'Delist Account From Sale', 'Delist All Account From Sale');
  if ASelection.RowCount = 1 then begin
    if not TAccountComp.AccountTxtNumberToAccountNumber(FAccountsGrid.Rows[ASelection.Row].Account, accNo) then
      raise Exception.Create('Error Parsing Account Number From Grid');
    account := TNode.Node.Operations.SafeBoxTransaction.Account(accNo);
    miEnlistAccountsForSale.Visible := IIF(TAccountComp.IsAccountForSale(account.accountInfo), False, True);
    miDelistAccountsFromSale.Visible := not miEnlistAccountsForSale.Visible;
  end;
  APopupMenu := mnuAccountsPopup;
end;

procedure TCTRLWallet.miAccountInfoClick(Sender: TObject);
begin
  if FAccountsGrid.Selection.RowCount <> 1 then exit;
  TUserInterface.ShowAccountInfoDialog(Self, FAccountsGrid.SelectedRows[0].__KEY);
end;

procedure TCTRLWallet.miSendPASCClick(Sender: TObject);
var
  Scoped: TDisposables;
  wiz: TWIZSendPASCWizard;
  model: TWIZOperationsModel;
begin
  wiz := Scoped.AddObject(TWIZSendPASCWizard.Create(nil)) as TWIZSendPASCWizard;
  model := TWIZOperationsModel.Create(wiz, omtSendPasc);
  model.Account.SelectedAccounts := TNode.Node.GetAccounts(SelectedAccounts, True);
  wiz.Start(model);
end;

procedure TCTRLWallet.miChangeKeyClick(Sender: TObject);
var
  Scoped: TDisposables;
  wiz: TWIZChangeKeyWizard;
  model: TWIZOperationsModel;
begin
  wiz := Scoped.AddObject(TWIZChangeKeyWizard.Create(nil)) as TWIZChangeKeyWizard;
  model := TWIZOperationsModel.Create(wiz, omtChangeKey);
  model.Account.SelectedAccounts := TNode.Node.GetAccounts(SelectedAccounts, True);
  wiz.Start(model);
end;

procedure TCTRLWallet.miEnlistAccountsForSaleClick(Sender: TObject);
var
  Scoped: TDisposables;
  wiz: TWIZEnlistAccountForSaleWizard;
  model: TWIZOperationsModel;
begin
  wiz := Scoped.AddObject(TWIZEnlistAccountForSaleWizard.Create(nil)) as TWIZEnlistAccountForSaleWizard;
  model := TWIZOperationsModel.Create(wiz, omtEnlistAccountForSale);
  model.Account.SelectedAccounts := TNode.Node.GetAccounts(SelectedAccounts, True);
  wiz.Start(model);
end;

procedure TCTRLWallet.miDelistAccountsFromSaleClick(Sender: TObject);
begin
  raise ENotImplemented.Create('not yet implemented.');
end;

procedure TCTRLWallet.OnPrepareOperationsPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
begin
  if (ASelection.RowCount <> 1) or ((ASelection.RowCount = 1) and (FOperationsGrid.SelectedRows[0].__KEY <> variant(nil))) then begin
    miSep2.Visible := True;
    miOperationInfo.Visible := True;
    APopupMenu := mnuOperationsPopup;
  end else begin
    miSep2.Visible := False;
    miOperationInfo.Visible := False;
    APopupMenu := nil; // is empty, so dont show
  end;
end;

procedure TCTRLWallet.miCopyOphashClick(Sender: TObject);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TCTRLWallet.miOperationInfoClick(Sender: TObject);
begin
  if FOperationsGrid.Selection.RowCount = 0 then exit;
  TUserInterface.ShowOperationInfoDialog(Self, FOperationsGrid.SelectedRows[0].__KEY);
end;

end.
