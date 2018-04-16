unit UCTRLWallet;

{$mode delphi}

{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, PairSplitter, Buttons, UVisualGrid, UCommon.UI, Generics.Collections,
  UAccounts, UDataSources, UNode, UWIZSendPASC, UWIZChangeKey;

type

  { TCTRLWallet }

  TCTRLWalletAccountsMode = (wamMyAccounts, wamFirstAccount);
  TCTRLWalletOperationsMode = (womSelectedAccounts, womAllAccounts);
  TCTRLWalletOperationsHistory = (woh30Days, wohFullHistory);

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
    FAccountsDataSource: TAccountsDataSource;
    FOperationsDataSource: TAccountsOperationsDataSource;
    procedure SetAccountsMode(AMode: TCTRLWalletAccountsMode);
    procedure SetOperationsMode(AMode: TCTRLWalletOperationsMode);
    procedure SetOperationsHistory(AHistory: TCTRLWalletOperationsHistory);
    procedure RefreshMyAccountsCombo;
    function GetAccNoWithoutChecksum(constref ARow: variant): cardinal; inline;
    function GetAccounts(const AccountNumbers: TArray<cardinal>): TArray<TAccount>;
  protected
    procedure ActivateFirstTime; override;
    procedure OnPrivateKeysChanged(Sender: TObject);
    procedure OnNodeBlocksChanged(Sender: TObject);
    procedure OnNodeNewOperation(Sender: TObject);
    procedure OnAccountsUpdated(Sender: TObject);
    procedure OnAccountsSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
    procedure OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
    procedure OnPrepareAccountPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
    procedure OnPrepareOperationsPopupMenu(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
  public
    property AccountsMode: TCTRLWalletAccountsMode read FAccountsMode write SetAccountsMode;
    property OperationsMode: TCTRLWalletOperationsMode read FOperationsMode write SetOperationsMode;
    property OperationsHistory: TCTRLWalletOperationsHistory read FOperationsHistory write SetOperationsHistory;
  end;

implementation

uses
  UUserInterface, UCellRenderers, UBlockChain, UWallet, UCrypto, UCoreUtils,
  UCommon, UMemory, Generics.Defaults, UCommon.Data, UCommon.Collections, UWIZModels;

{$R *.lfm}

{ TCTRLWallet }

procedure TCTRLWallet.FormCreate(Sender: TObject);
var
  cmbDuration: TComboBox;
begin
  // event registrations
  FNodeNotifyEvents := TNodeNotifyEvents.Create(self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeBlocksChanged;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;
  TWallet.Keys.OnChanged.Add(OnPrivateKeysChanged);


  // fields
  FAccountsDataSource := TAccountsDataSource.Create(Self);
  FAccountsDataSource.FilterKeys := TWallet.Keys.AccountsKeyList.ToArray;
  FOperationsDataSource := TAccountsOperationsDataSource.Create(Self);

  // grids
  FAccountsGrid := TVisualGrid.Create(Self);
  FAccountsGrid.SortMode := smMultiColumn;
  FAccountsGrid.FetchDataInThread := True;
  FAccountsGrid.AutoPageSize := True;
  FAccountsGrid.SelectionType := stMultiRow;
  FAccountsGrid.DeselectionType := dtDefault;
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

  FAccountsGrid.OnSelection := OnAccountsSelected;
  FAccountsGrid.OnFinishedUpdating := OnAccountsUpdated;
  FAccountsGrid.OnPreparePopupMenu := OnPrepareAccountPopupMenu;

  FOperationsGrid := TVisualGrid.Create(Self);
  FOperationsGrid.SortMode := smMultiColumn;
  FOperationsGrid.FetchDataInThread := True;
  FOperationsGrid.AutoPageSize := True;
  FOperationsGrid.SelectionType := stRow;
  FOperationsGrid.DeselectionType := dtDefault;
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
    Renderer := TCellRenderers.PASC;
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
    Renderer := TCellRenderers.PASC;
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
    Renderer := TCellRenderers.PASC;
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
    cmbDuration.AddItem('30 Days', TObject(woh30Days));
    cmbDuration.AddItem('Maximum', TObject(wohFullHistory));
  finally
    cmbDuration.Items.EndUpdate;
    cmbDuration.ItemIndex := 0;
  end;
  cmbDuration.OnChange := cmbDurationChange;
  FOperationsGrid.WidgetControl := cmbDuration;

end;

procedure TCTRLWallet.FormDestroy(Sender: TObject);
begin
  TWallet.Keys.OnChanged.Add(OnPrivateKeysChanged);
end;

procedure TCTRLWallet.FormResize(Sender: TObject);
begin
  // Left hand panel is 50% the size up until a max size of 450

end;

procedure TCTRLWallet.ActivateFirstTime;
begin
  // Configure grid states
  AccountsMode := wamMyAccounts;
  OperationsMode := womAllAccounts;
  OperationsHistory := woh30Days;

  // Load up selected for some reasons
  FAccountsGrid.InternalDrawGrid.ClearSelections;
  FOperationsGrid.InternalDrawGrid.ClearSelections;
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

function TCTRLWallet.GetAccounts(const AccountNumbers: TArray<cardinal>): TArray<TAccount>;
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
      for i := Low(AccountNumbers) to High(AccountNumbers) do begin
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

procedure TCTRLWallet.SetAccountsMode(AMode: TCTRLWalletAccountsMode);
var
  sel1: TVisualGridSelection;
  sel2: TRect;
begin
  FAccountsMode := AMode;
  paAccounts.RemoveAllControls(False);
  case AMode of
    wamMyAccounts:
    begin
      FOperationsGrid.DataSource := FOperationsDataSource;
      FAccountsGrid.DataSource := FAccountsDataSource;
      FAccountsGrid.Caption.Text := 'My Accounts';
      paAccounts.RemoveAllControls(False);
      sel1 := FAccountsGrid.Selection;
      sel2 := FAccountsGrid.InternalDrawGrid.Selection;
      paAccounts.RemoveAllControls(False);
      paAccounts.AddControlDockCenter(FAccountsGrid);
      paOperations.RemoveAllControls(False);
      paOperations.AddControlDockCenter(FOperationsGrid);
    end;
    wamFirstAccount: raise Exception.Create('Not implemented');
  end;
end;

procedure TCTRLWallet.SetOperationsMode(AMode: TCTRLWalletOperationsMode);

  function GetAccNo(constref AAccount: TAccount): cardinal; overload;
  begin
    Result := AAccount.account;
  end;

  function GetAccNo(constref ARow: variant): cardinal; overload;
  begin
    Result := ARow.__KEY;
  end;

begin
  case AMode of
    womAllAccounts:
    begin
      FOperationsGrid.Caption.Text := '';
      FOperationsDataSource.Accounts :=
        TListTool<TAccount, cardinal>.Transform(
        FAccountsDataSource.LastFetchResult, GetAccNo);
    end;
    womSelectedAccounts:
    begin
      FOperationsGrid.Caption.Text := 'Selected Accounts';
      FOperationsDataSource.Accounts :=
        TListTool<variant, cardinal>.Transform(FAccountsGrid.SelectedRows, GetAccNo);
    end
    else
      raise ENotSupportedException.Create(Format('AMode %d not supported',
        [integer(AMode)]));
  end;
  FOperationsGrid.RefreshGrid;
  FOperationsMode := AMode;
end;

procedure TCTRLWallet.SetOperationsHistory(AHistory: TCTRLWalletOperationsHistory);
begin
  FOperationsHistory := AHistory;
  case FOperationsHistory of
    woh30Days: FOperationsDataSource.TimeSpan := TTimeSpan.FromDays(30);
    wohFullHistory: FOperationsDataSource.TimeSpan := TTimeSpan.FromDays(10 * 365);
  end;
  FOperationsGrid.RefreshGrid;
end;

procedure TCTRLWallet.OnPrivateKeysChanged(Sender: TObject);
begin
  RefreshMyAccountsCombo;
end;

procedure TCTRLWallet.OnNodeBlocksChanged(Sender: TObject);
begin
  FAccountsGrid.RefreshGrid;
  FOperationsGrid.RefreshGrid;
end;

procedure TCTRLWallet.OnNodeNewOperation(Sender: TObject);
begin
  FAccountsGrid.RefreshGrid;
  FOperationsGrid.RefreshGrid;
end;

procedure TCTRLWallet.OnAccountsUpdated(Sender: TObject);
begin
  lblTotalPASC.Caption := TAccountComp.FormatMoney(FAccountsDataSource.Overview.TotalPASC);
  lblTotalPASA.Caption := Format('%d', [FAccountsDataSource.Overview.TotalPASA]);
end;

procedure TCTRLWallet.OnAccountsSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
var
  row: longint;
  selectedAccounts: Generics.Collections.TList<cardinal>;
  acc: cardinal;
  GC: TDisposables;
begin
  selectedAccounts := GC.AddObject(TList<cardinal>.Create) as TList<cardinal>;

  if ASelection.RowCount > 0 then begin
    for row := ASelection.Row to (ASelection.Row + ASelection.RowCount - 1) do begin
      if (TAccountComp.AccountTxtNumberToAccountNumber(FAccountsGrid.Rows[row].Account, acc)) then
        selectedAccounts.Add(acc);
    end;
    FOperationsDataSource.Accounts := selectedAccounts.ToArray;
    FOperationsGrid.Caption.Text := IIF(ASelection.RowCount = 1, Format('Account: %s', [TAccountComp.AccountNumberToAccountTxtNumber(selectedAccounts[0])]), 'Selected Accounts');
    FOperationsGrid.RefreshGrid;
  end else
    OperationsMode := womAllAccounts;
end;

procedure TCTRLWallet.OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
var
  row: longint;
  v: variant;
  ophash: ansistring;
begin
  row := ASelection.Row;
  if (row >= 0) and (row < FOperationsGrid.RowCount) then begin
    v := FOperationsGrid.Rows[row];
    ophash := FOperationsGrid.Rows[row].OPHASH;
    if TPCOperation.IsValidOperationHash(ophash) then
      TUserInterface.ShowOperationInfoDialog(self, ophash);
  end;
end;

procedure TCTRLWallet.cbAccountsChange(Sender: TObject);
var
  index: integer;
  sel: TBox<TAccountKey>;
begin
  index := cbAccounts.ItemIndex;
  if cbAccounts.ItemIndex < 0 then exit;
  if index = 0 then
    FAccountsDataSource.FilterKeys := TWallet.Keys.AccountsKeyList.ToArray
  else if index = cbAccounts.Items.Count - 1 then begin
    AccountsMode := wamFirstAccount;
    exit;
  end else begin
    sel := TBox<TAccountKey>(cbAccounts.Items.Objects[cbAccounts.ItemIndex]);
    FAccountsDataSource.FilterKeys := TArray<TAccountKey>.Create(sel.Value);
  end;
  if Self.AccountsMode <> wamMyAccounts then
    AccountsMode := wamMyAccounts
  else
    FAccountsGrid.RefreshGrid;
end;

procedure TCTRLWallet.cmbDurationChange(Sender: TObject);
var
  cmbDuration: TComboBox;
begin
  cmbDuration := Sender as TComboBox;
  if not Assigned(cmbDuration) then
    exit;

  case cmbDuration.ItemIndex of
    0: OperationsHistory := woh30Days;
    1: OperationsHistory := wohFullHistory;
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
  AccountNumbersWithoutChecksum: TArray<cardinal>;
begin
  wiz := Scoped.AddObject(TWIZSendPASCWizard.Create(nil)) as TWIZSendPASCWizard;
  model := TWIZOperationsModel.Create(wiz, omtSendPasc);
  AccountNumbersWithoutChecksum := TListTool<variant, cardinal>.Transform(FAccountsGrid.SelectedRows,GetAccNoWithoutChecksum);
  model.Account.SelectedAccounts := GetAccounts(AccountNumbersWithoutChecksum);
  wiz.Start(model);
end;

procedure TCTRLWallet.miChangeKeyClick(Sender: TObject);
var
  Scoped: TDisposables;
  wiz: TWIZChangeKeyWizard;
  model: TWIZOperationsModel;
  AccountNumbersWithoutChecksum: TArray<cardinal>;
begin
  wiz := Scoped.AddObject(TWIZChangeKeyWizard.Create(nil)) as TWIZChangeKeyWizard;
  model := TWIZOperationsModel.Create(wiz, omtChangeKey);
  AccountNumbersWithoutChecksum := TListTool<variant, cardinal>.Transform(FAccountsGrid.SelectedRows, GetAccNoWithoutChecksum);
  model.Account.SelectedAccounts := GetAccounts(AccountNumbersWithoutChecksum);
  wiz.Start(model);
end;

procedure TCTRLWallet.miEnlistAccountsForSaleClick(Sender: TObject);
begin
 raise ENotImplemented.Create('not yet implemented.');
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

// Aux

function TCTRLWallet.GetAccNoWithoutChecksum(constref ARow: variant): cardinal;
begin
  Result := ARow.__KEY;
end;

end.
