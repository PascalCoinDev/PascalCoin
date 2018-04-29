unit UFRMAccountExplorer;

{$mode delphi}

{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, PairSplitter, Buttons, UVisualGrid, UCommon.UI, Generics.Collections,
  UAccounts, UDataSources, UNode, UCoreObjects, UCoreUtils, UWIZSendPASC, UWIZChangeKey, UWIZEnlistAccountForSale;

type

  { TFRMAccountExplorer }

  TFRMAccountExplorerAccountsMode = (wamMyAccounts, wamAllAccounts);
  TFRMAccountExplorerOperationsMode = (womSelectedAccounts, womAllAccounts);
  TFRMAccountExplorerOperationsHistory = (woh7Days, woh30Days, wohFullHistory);

  TFRMAccountExplorer = class(TApplicationForm)
    btnChangeKeyName: TBitBtn;
    cbAccounts: TComboBox;
    chkExploreMyAccounts: TCheckBox;
    gpAccounts: TGroupBox;
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
    procedure btnChangeKeyNameClick(Sender: TObject);
    procedure cbAccountsChange(Sender: TObject);
    procedure chkExploreMyAccountsChange(Sender: TObject);
    procedure cmbDurationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAccountInfoClick(Sender: TObject);
    procedure miChangeKeyClick(Sender: TObject);
    procedure miCopyOphashClick(Sender: TObject);
    procedure miOperationInfoClick(Sender: TObject);
    procedure miSendPASCClick(Sender: TObject);
    procedure miEnlistAccountsForSaleClick(Sender: TObject);
    procedure miDelistAccountsFromSaleClick(Sender: TObject);
  private
    FNodeNotifyEvents: TNodeNotifyEvents;
    FAccountsMode: TFRMAccountExplorerAccountsMode;
    FOperationsMode: TFRMAccountExplorerOperationsMode;
    FOperationsHistory: TFRMAccountExplorerOperationsHistory;
    FAccountsGrid: TVisualGrid;
    FOperationsGrid: TVisualGrid;
    FAllAccountsDataSource: TAccountsDataSource;
    FAccountsDataSource: TMyAccountsDataSource;
    FOperationsDataSource: TAccountsOperationsDataSource;
    procedure SetAccountsMode(AMode: TFRMAccountExplorerAccountsMode);
    procedure SetOperationsMode(AMode: TFRMAccountExplorerOperationsMode);
    procedure SetOperationsHistory(AHistory: TFRMAccountExplorerOperationsHistory);
    procedure RefreshMyAccountsCombo;
    procedure RefreshTotals;
    procedure RefreshAccountsGrid;
    procedure RefreshOperationsGrid;
    function GetAccounts(const AccountNumbers: TArray<cardinal>): TArray<TAccount>;
  protected
    procedure ActivateFirstTime; override;
    procedure OnPrivateKeysChanged(Sender: TObject);
    procedure OnUserAccountsChanged(Sender: TObject);
    procedure OnNodeBlocksChanged(Sender: TObject);
    procedure OnNodeNewOperation(Sender: TObject);
    procedure OnAccountsSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
    procedure OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
    procedure OnPrepareAccountPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
    procedure OnPrepareOperationsPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
  public
    property AccountsMode: TFRMAccountExplorerAccountsMode read FAccountsMode write SetAccountsMode;
    property OperationsMode: TFRMAccountExplorerOperationsMode read FOperationsMode write SetOperationsMode;
    property OperationsHistory: TFRMAccountExplorerOperationsHistory read FOperationsHistory write SetOperationsHistory;
  end;

implementation

uses
  UUserInterface, UCellRenderers, UBlockChain, UWallet, UCrypto,
  UCommon, UMemory, Generics.Defaults, UCommon.Data, UCommon.Collections;

{$R *.lfm}

{ TFRMAccountExplorer }

procedure TFRMAccountExplorer.FormCreate(Sender: TObject);
var
  cmbDuration: TComboBox;
begin
  // event registrations
  FNodeNotifyEvents := TNodeNotifyEvents.Create(self);
  FNodeNotifyEvents.WatchKeys := TWallet.Keys.AccountsKeyList;
  FNodeNotifyEvents.OnKeyActivity := OnUserAccountsChanged;
  FNodeNotifyEvents.OnBlocksChanged := OnNodeBlocksChanged;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;
  TWallet.Keys.OnChanged.Add(OnPrivateKeysChanged);

  // fields
  FAllAccountsDataSource := TAccountsDataSource.Create(Self);
  FAllAccountsDataSource.IncludePending := True;
  FAccountsDataSource := TMyAccountsDataSource.Create(Self);
  FOperationsDataSource := TAccountsOperationsDataSource.Create(Self);
  FOperationsDataSource.Accounts := nil;
  FOperationsHistory := woh7Days;
  FOperationsMode := womAllAccounts;
  FAccountsMode := wamAllAccounts;

  // grids
  FAccountsGrid := TVisualGrid.Create(Self);
  FAccountsGrid.SortMode := smMultiColumn;
  FAccountsGrid.FetchDataInThread := False;
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
    Width := 100;
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
  with FAccountsGrid.AddColumn('N Op.') do
  begin
    Binding := 'NumberOfOperations';
    Width := 50;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FAccountsGrid.AddColumn('Type') do
  begin
    Binding := 'Type';
    Width := 50;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FAccountsGrid.AddColumn('Price') do
  begin
    Binding := 'Price';
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Renderer := TCellRenderers.PASC_CheckPendingBalance;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;

  FAccountsGrid.OnSelection := OnAccountsSelected;
  FAccountsGrid.OnPreparePopupMenu := OnPrepareAccountPopupMenu;

  FOperationsGrid := TVisualGrid.Create(Self);
  FOperationsGrid.SortMode := smMultiColumn;
  FOperationsGrid.FetchDataInThread := True;
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
    Width := 150;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FOperationsGrid.AddColumn('Amount') do
  begin
    Binding := 'AmountDecimal';
    SortBinding := 'Amount';
    DisplayBinding := 'Amount';
    Width := 150;
    HeaderAlignment := taRightJustify;
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
  //FOperationsGrid.Caption.Text := 'All Account Operations';
  FOperationsGrid.Caption.Text := '';
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

  // Add datasources to grid
  chkExploreMyAccountsChange(Self);
  FOperationsGrid.DataSource := FOperationsDataSource;

  // Add grid to panels
  paAccounts.AddControlDockCenter(FAccountsGrid);
  paOperations.AddControlDockCenter(FOperationsGrid);
end;

procedure TFRMAccountExplorer.FormDestroy(Sender: TObject);
begin
  TWallet.Keys.OnChanged.Add(OnPrivateKeysChanged);
end;

procedure TFRMAccountExplorer.ActivateFirstTime;
begin
  RefreshTotals;
end;

procedure TFRMAccountExplorer.RefreshTotals;

  function GetAllAccountsKey(): TArray<TAccountKey>;
  var
    LAccount: TAccount;
    LAccounts: TList<TAccount>;
    LAccountsKey: TList<TAccountKey>;
    LGC: TDisposables;
  begin
    LAccounts := LGC.AddObject(TList<TAccount>.Create) as TList<TAccount>;
    FAllAccountsDataSource.FetchAll(LAccounts);
    LAccountsKey := LGC.AddObject(TList<TAccountKey>.Create) as TList<TAccountKey>;
    for LAccount in LAccounts do
      LAccountsKey.Add(LAccount.accountInfo.accountKey);
    Result := LAccountsKey.ToArray;
  end;

var
  LBalance: TBalanceSummary;
  LIdx: integer;
begin
  case FAccountsMode of
    wamMyAccounts:
    begin
      if (cbAccounts.ItemIndex = 0) then
        LBalance := TWallet.Keys.AccountsKeyList.GetBalance(True)
      else
      begin
        LIdx := TWallet.Keys.IndexOfAccountKey(TBox<TAccountKey>(
          cbAccounts.Items.Objects[cbAccounts.ItemIndex]).Value);

        if (LIdx < 0) or (LIdx >= TWallet.Keys.Count) then
        begin
          ShowMessage('You Must Select a Valid Key');
          Exit;
        end;
        LBalance := TWallet.Keys.Key[LIdx].AccountKey.GetBalance(True);
      end;

      lblTotalPASC.Caption := TAccountComp.FormatMoney(LBalance.TotalPASC);
      lblTotalPASA.Caption := Format('%d', [LBalance.TotalPASA]);
    end;
    wamAllAccounts:
    begin
      LBalance := TNode.Node.Bank.SafeBox.GetBalance(GetAllAccountsKey, True);
      lblTotalPASC.Caption := TAccountComp.FormatMoney(LBalance.TotalPASC);
      lblTotalPASA.Caption := Format('%d', [LBalance.TotalPASA]);
    end;
  end;
end;

procedure TFRMAccountExplorer.RefreshMyAccountsCombo;
var
  i: integer;
  selectFirst, selectLast: boolean;
  last_key: TAccountKey;
  key: TWalletKey;
  str: ansistring;
begin
  // determine current selection
  if cbAccounts.ItemIndex >= 1 then
  begin
    if cbAccounts.ItemIndex < cbAccounts.Items.Count - 1 then
    begin
      last_key := TBox<TAccountKey>(
        cbAccounts.Items.Objects[cbAccounts.ItemIndex]).Value;
      selectFirst := False;
      selectLast := False;
    end
    else
    begin
      selectFirst := False;
      selectLast := True;
    end;
  end
  else
  begin
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
    for i := 0 to TWallet.Keys.Count - 1 do
    begin
      // get i'th key
      key := TWallet.Keys.Key[i];
      // fix name
      if (key.Name = '') then
        str := 'Sha256=' + TCrypto.ToHexaString(TCrypto.DoSha256(
          TAccountComp.AccountKey2RawString(key.AccountKey)))
      else
        str := key.Name;
      if not Assigned(key.PrivateKey) then
        str := str + '(*)';
      cbAccounts.Items.AddObject(str, TBox<TAccountKey>.Create(key.AccountKey));
    end;
    cbAccounts.Items.InsertObject(0, 'Show All', TBox<TAccountKey>.Create);
  finally
    cbAccounts.Items.EndUpdate;
  end;
  // re-select previous selection
  if selectFirst then
    cbAccounts.ItemIndex := 0
  else if selectLast then
    cbAccounts.ItemIndex := cbAccounts.Items.Count - 1
  else
    for i := 1 to cbAccounts.Items.Count - 2 do
      if TAccountKeyEqualityComparer.AreEqual(TBox<TAccountKey>(cbAccounts.Items.Objects[i]).Value, last_key) then
      begin
        cbAccounts.ItemIndex := i;
        exit;
      end;
end;

procedure TFRMAccountExplorer.RefreshAccountsGrid;

  function GetAccountKey(constref AAccount: TAccount): TAccountKey;
  begin
    Result := AAccount.accountInfo.accountKey;
  end;

  function GetAllAccounts(): TArray<TAccount>;
  var
    LAccounts: TList<TAccount>;
    LGC: TDisposables;
  begin
    LAccounts := LGC.AddObject(TList<TAccount>.Create) as TList<TAccount>;
    FAllAccountsDataSource.FetchAll(LAccounts);
    Result := LAccounts.ToArray;
  end;

var
  index: integer;
  sel: TBox<TAccountKey>;
begin

  case Self.AccountsMode of
    wamMyAccounts:
    begin
      cbAccounts.Enabled := True;
      btnChangeKeyName.Enabled := True;
      if cbAccounts.ItemIndex = cbAccounts.Items.Count - 1 then
        exit; // not a key
      index := cbAccounts.ItemIndex;
      if index = 0 then
      begin
        gpAccounts.Caption := 'My Accounts';
        FAccountsDataSource.FilterKeys := TWallet.Keys.AccountsKeyList.ToArray;
      end
      else
      begin
        sel := TBox<TAccountKey>(cbAccounts.Items.Objects[cbAccounts.ItemIndex]);
        gpAccounts.Caption := Format('%s Accounts', [TWallet.Keys[TWallet.Keys.IndexOfAccountKey(sel.Value)].Name]);
        FAccountsDataSource.FilterKeys := TArray<TAccountKey>.Create(sel.Value);
      end;
    end;

    wamAllAccounts:
    begin
      cbAccounts.Enabled := False;
      btnChangeKeyName.Enabled := False;
      gpAccounts.Caption := 'All Accounts';
      FAllAccountsDataSource.FilterKeys := TListTool<TAccount, TAccountKey>.Transform(GetAllAccounts, GetAccountKey);
    end;
  end;
  RefreshTotals;
  FAccountsGrid.RefreshGrid;
end;

procedure TFRMAccountExplorer.RefreshOperationsGrid;

  function GetAccNo(constref AAccount: TAccount): cardinal; overload;
  begin
    Result := AAccount.account;
  end;

  function GetAccNo(constref ARow: variant): cardinal; overload;
  begin
    Result := ARow.__KEY;
  end;

  function GetAllAccounts(): TArray<TAccount>;
  var
    LAccounts: TList<TAccount>;
    LGC: TDisposables;
  begin
    LAccounts := LGC.AddObject(TList<TAccount>.Create) as TList<TAccount>;
    FAllAccountsDataSource.FetchAll(LAccounts);
    Result := LAccounts.ToArray;
  end;

begin
  case FOperationsMode of
    womAllAccounts:
    begin
      FOperationsGrid.Caption.Text := '';
      FOperationsDataSource.Accounts := nil;
    end;
    womSelectedAccounts:
    begin
      FOperationsGrid.Caption.Text := 'Selected Account Operations';
      FOperationsDataSource.Accounts := TListTool<variant, cardinal>.Transform(FAccountsGrid.SelectedRows, GetAccNo);
    end
    else
      raise ENotSupportedException.Create(Format('AMode %d not supported', [integer(FOperationsMode)]));
  end;
  FOperationsGrid.RefreshGrid;
end;

function TFRMAccountExplorer.GetAccounts(const AccountNumbers: TArray<cardinal>): TArray<TAccount>;
var
  acc: TAccount;
  safeBox: TPCSafeBox;
  keys: TOrderedAccountKeysList;
  LContainer: Generics.Collections.TList<TAccount>;
  i: integer;
begin
  LContainer := Generics.Collections.TList<TAccount>.Create();
  keys := TWallet.keys.AccountsKeyList;
  safeBox := TUserInterface.Node.Bank.safeBox;
  safeBox.StartThreadSafe;
  try
    LContainer.Clear;
    try
      // load selected user accounts
      for i := Low(AccountNumbers) to High(AccountNumbers) do
      begin
        acc := safeBox.Account(AccountNumbers[i]);
        if keys.IndexOfAccountKey(acc.accountInfo.accountKey) >= 0 then
          LContainer.Add(acc);
      end;
    finally
      safeBox.EndThreadSave;
    end;
    Result := LContainer.ToArray;
  finally
    LContainer.Free;
  end;
end;

procedure TFRMAccountExplorer.SetAccountsMode(AMode: TFRMAccountExplorerAccountsMode);
begin
  if FAccountsMode = AMode then
    exit;

  FUILock.Acquire;
  try
    FAccountsMode := AMode;
    paAccounts.RemoveAllControls(False);
    // reset account combo
    cbAccounts.OnChange := nil; // disable event
    cbAccounts.ItemIndex := 0;
    cbAccounts.OnChange := cbAccountsChange; // re-enable event
    // ensure on accounts panel
    if FAccountsGrid.Parent <> paAccounts then
    begin
      paAccounts.RemoveAllControls(False);
      paAccounts.AddControlDockCenter(FAccountsGrid);
    end;
    case AMode of

      wamMyAccounts:
        FAccountsGrid.DataSource := FAccountsDataSource;

      wamAllAccounts:
        FAccountsGrid.DataSource := FAllAccountsDataSource;
    end;
    // Refresh grid
    FAccountsGrid.ClearSelection();
    RefreshAccountsGrid;
  finally
    FUILock.Release;
  end;
end;

procedure TFRMAccountExplorer.SetOperationsMode(AMode: TFRMAccountExplorerOperationsMode);
begin
  if FOperationsMode = AMode then
    exit;
  FUILock.Acquire;
  try
    FOperationsMode := AMode;
    RefreshOperationsGrid;
  finally
    FUILock.Release;
  end;
end;

procedure TFRMAccountExplorer.SetOperationsHistory(AHistory: TFRMAccountExplorerOperationsHistory);
begin
  FOperationsHistory := AHistory;
  case FOperationsHistory of
    woh7Days: FOperationsDataSource.TimeSpan := TTimeSpan.FromDays(7);
    woh30Days: FOperationsDataSource.TimeSpan := TTimeSpan.FromDays(30);
    wohFullHistory: FOperationsDataSource.TimeSpan := TTimeSpan.FromDays(10 * 365);
  end;
  FOperationsGrid.RefreshGrid;
end;

procedure TFRMAccountExplorer.OnPrivateKeysChanged(Sender: TObject);
begin
  RefreshMyAccountsCombo;
end;

procedure TFRMAccountExplorer.OnUserAccountsChanged(Sender: TObject);
begin
  //  if NOT TUserInterface.Node.HasBestKnownBlockchainTip then
  //    exit; // node syncing

  RefreshTotals;
  FAccountsGrid.RefreshGrid;
  FOperationsGrid.RefreshGrid;
end;

procedure TFRMAccountExplorer.OnNodeBlocksChanged(Sender: TObject);
begin
  // TODO: play block sound chime
  RefreshTotals;
  FAccountsGrid.RefreshGrid;
  FOperationsGrid.RefreshGrid;
end;

procedure TFRMAccountExplorer.OnNodeNewOperation(Sender: TObject);
begin
  // TODO: play operation sound tick
  RefreshTotals;
  FAccountsGrid.RefreshGrid;
  FOperationsGrid.RefreshGrid;
end;

procedure TFRMAccountExplorer.OnAccountsSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
begin
  if ASelection.Page >= 0 then
  begin
    if OperationsMode <> womSelectedAccounts then
      OperationsMode := womSelectedAccounts
    else
      RefreshOperationsGrid; // already viewing selected accounts, add to visible set
  end
  else
    OperationsMode := womAllAccounts;
end;

procedure TFRMAccountExplorer.OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
var
  row: longint;
  v: variant;
  ophash: ansistring;
begin
  if ASelection.Page < 0 then
    exit;
  row := ASelection.Row;
  if (row >= 0) and (row < FOperationsGrid.RowCount) then
  begin
    v := FOperationsGrid.Rows[row];
    ophash := FOperationsGrid.Rows[row].OPHASH;
    if TPCOperation.IsValidOperationHash(ophash) then
    begin
      TUserInterface.ShowOperationInfoDialog(self, ophash);
      FOperationsGrid.ClearSelection(True);
    end;
  end;
end;

procedure TFRMAccountExplorer.cbAccountsChange(Sender: TObject);
begin
  if cbAccounts.ItemIndex < 0 then
    exit;
  FAccountsGrid.ClearSelection();
  RefreshAccountsGrid;
  if FOperationsMode <> womAllAccounts then
    RefreshOperationsGrid;
end;

procedure TFRMAccountExplorer.btnChangeKeyNameClick(Sender: TObject);
var
  LIdx, LCurrentIndex: integer;
  LNewName: string;
begin
  if (cbAccounts.ItemIndex <= 0) then
  begin
    ShowMessage('You Must Select a Valid Key');
    Exit;
  end;
  LIdx := TWallet.Keys.IndexOfAccountKey(TBox<TAccountKey>(
    cbAccounts.Items.Objects[cbAccounts.ItemIndex]).Value);

  if (LIdx < 0) or (LIdx >= TWallet.Keys.Count) then
  begin
    ShowMessage('You Must Select a Valid Key');
    Exit;
  end;

  LCurrentIndex := cbAccounts.ItemIndex;
  if InputQuery('Change Key Name', 'Input New Name', LNewName) then
  begin
    TWallet.Keys.SetName(LIdx, LNewName);
    cbAccounts.ItemIndex := LCurrentIndex;
    cbAccountsChange(Self);
  end;
end;

procedure TFRMAccountExplorer.chkExploreMyAccountsChange(Sender: TObject);
begin
  if chkExploreMyAccounts.Checked then
  begin
    FAccountsMode := wamMyAccounts;
    FAccountsGrid.DataSource := FAccountsDataSource;
  end
  else
  begin
    FAccountsMode := wamAllAccounts;
    FAccountsGrid.DataSource := FAllAccountsDataSource;
  end;
  RefreshAccountsGrid;
end;

procedure TFRMAccountExplorer.cmbDurationChange(Sender: TObject);
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

procedure TFRMAccountExplorer.OnPrepareAccountPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
var
  accNo: cardinal;
  account: TAccount;
begin
  miSep1.Visible := ASelection.RowCount = 1;
  miAccountInfo.Visible := ASelection.RowCount = 1;
  miSendPASC.Visible := IIF(FAccountsMode = wamMyAccounts, True, False);
  miChangeKey.Visible := IIF(FAccountsMode = wamMyAccounts, True, False);
  miAccountsMarket.Visible := IIF(FAccountsMode = wamMyAccounts, True, False);
  miSendPASC.Caption := IIF(ASelection.RowCount = 1, 'Send PASC', 'Send All PASC');
  miChangeKey.Caption := IIF(ASelection.RowCount = 1, 'Change Key', 'Change All Key');
  miEnlistAccountsForSale.Caption := IIF(ASelection.RowCount = 1, 'Enlist Account For Sale', 'Enlist All Account For Sale');
  miDelistAccountsFromSale.Caption := IIF(ASelection.RowCount = 1, 'Delist Account From Sale', 'Delist All Account From Sale');
  if ASelection.RowCount = 1 then
  begin
    if not TAccountComp.AccountTxtNumberToAccountNumber(FAccountsGrid.Rows[ASelection.Row].Account, accNo) then
      raise Exception.Create('Error Parsing Account Number From Grid');
    account := TNode.Node.Operations.SafeBoxTransaction.Account(accNo);
    miEnlistAccountsForSale.Visible := IIF(TAccountComp.IsAccountForSale(account.accountInfo), False, True);
    miDelistAccountsFromSale.Visible := not miEnlistAccountsForSale.Visible;
  end;
  APopupMenu := mnuAccountsPopup;
end;

procedure TFRMAccountExplorer.miAccountInfoClick(Sender: TObject);
begin
  if FAccountsGrid.Selection.RowCount <> 1 then
    exit;
  TUserInterface.ShowAccountInfoDialog(Self, FAccountsGrid.SelectedRows[0].__KEY);
end;

procedure TFRMAccountExplorer.miSendPASCClick(Sender: TObject);
var
  Scoped: TDisposables;
  wiz: TWIZSendPASCWizard;
  model: TExecuteOperationsModel;
  AccountNumbersWithoutChecksum: TArray<cardinal>;

  function GetAccNoWithoutChecksum(constref ARow: variant): cardinal;
  begin
    Result := ARow.__KEY;
  end;

begin
  wiz := Scoped.AddObject(TWIZSendPASCWizard.Create(nil)) as TWIZSendPASCWizard;
  model := TExecuteOperationsModel.Create(wiz, omtSendPasc);
  AccountNumbersWithoutChecksum := TListTool<variant, cardinal>.Transform(FAccountsGrid.SelectedRows, GetAccNoWithoutChecksum);
  model.Account.SelectedAccounts := GetAccounts(AccountNumbersWithoutChecksum);
  wiz.Start(model);
end;

procedure TFRMAccountExplorer.miChangeKeyClick(Sender: TObject);
var
  Scoped: TDisposables;
  wiz: TWIZChangeKeyWizard;
  model: TExecuteOperationsModel;
  AccountNumbersWithoutChecksum: TArray<cardinal>;

  function GetAccNoWithoutChecksum(constref ARow: variant): cardinal;
  begin
    Result := ARow.__KEY;
  end;

begin
  wiz := Scoped.AddObject(TWIZChangeKeyWizard.Create(nil)) as TWIZChangeKeyWizard;
  model := TExecuteOperationsModel.Create(wiz, omtChangeKey);
  AccountNumbersWithoutChecksum := TListTool<variant, cardinal>.Transform(FAccountsGrid.SelectedRows, GetAccNoWithoutChecksum);
  model.Account.SelectedAccounts := GetAccounts(AccountNumbersWithoutChecksum);
  wiz.Start(model);
end;

procedure TFRMAccountExplorer.miEnlistAccountsForSaleClick(Sender: TObject);
var
  Scoped: TDisposables;
  wiz: TWIZEnlistAccountForSaleWizard;
  model: TExecuteOperationsModel;
  AccountNumbersWithoutChecksum: TArray<cardinal>;

  function GetAccNoWithoutChecksum(constref ARow: variant): cardinal;
  begin
    Result := ARow.__KEY;
  end;

begin
  wiz := Scoped.AddObject(TWIZEnlistAccountForSaleWizard.Create(nil)) as TWIZEnlistAccountForSaleWizard;
  model := TExecuteOperationsModel.Create(wiz, omtEnlistAccountForSale);
  AccountNumbersWithoutChecksum := TListTool<variant, cardinal>.Transform(FAccountsGrid.SelectedRows, GetAccNoWithoutChecksum);
  model.Account.SelectedAccounts := GetAccounts(AccountNumbersWithoutChecksum);
  wiz.Start(model);
end;

procedure TFRMAccountExplorer.miDelistAccountsFromSaleClick(Sender: TObject);
begin
  raise ENotImplemented.Create('not yet implemented.');
end;

procedure TFRMAccountExplorer.OnPrepareOperationsPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
begin
  if (ASelection.RowCount <> 1) or ((ASelection.RowCount = 1) and (FOperationsGrid.SelectedRows[0].__KEY <> variant(nil))) then
  begin
    miSep2.Visible := True;
    miOperationInfo.Visible := True;
    APopupMenu := mnuOperationsPopup;
  end
  else
  begin
    miSep2.Visible := False;
    miOperationInfo.Visible := False;
    APopupMenu := nil; // is empty, so dont show
  end;
end;

procedure TFRMAccountExplorer.miCopyOphashClick(Sender: TObject);
begin
  raise ENotImplemented.Create('Not Implemented');
end;

procedure TFRMAccountExplorer.miOperationInfoClick(Sender: TObject);
begin
  if FOperationsGrid.Selection.RowCount = 0 then
    exit;
  TUserInterface.ShowOperationInfoDialog(Self, FOperationsGrid.SelectedRows[0].__KEY);
end;

end.
