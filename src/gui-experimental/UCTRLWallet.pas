unit UCTRLWallet;

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

{ Hosts wallet account and operation grids, key selection, balances and
  operation actions. Node synchronization pauses queries; completed reads
  update the grids and overview. }

{ Public types in this unit

  Type                         | Purpose
  -----------------------------+-------------------------------------------------------------------------
  TCTRLWallet                  | Hosts account/history grids, wallet totals and operation actions.
  TCTRLWalletAccountsMode      | Selects the account grid or the first-account prompt.
  TCTRLWalletOperationsHistory | Selects seven-day, thirty-day or all available stored history.
  TCTRLWalletOperationsMode    | Selects history for chosen accounts or the whole wallet.
}

interface

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  Classes, SysUtils, Types, Math, Messages, {$IFDEF FPC}FileUtil, LCLType, LCLIntf, LMessages,{$ENDIF} Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, {$IFDEF FPC}PairSplitter,{$ENDIF} Buttons, UVisualGrid, UCommon, UCommon.UI, Generics.Collections,
  ULog, UAccounts, UPCDataTypes, UDataSources, UNode, UCoreObjects, UCoreUtils, UCTRLNoAccount;

const
  CM_WalletControlKeysChanged = {$IFDEF FPC}LM_USER{$ELSE}WM_USER{$ENDIF} + 101;

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
    miBuyAccount: TMenuItem;
    miChangeAccountInfo: TMenuItem;
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
    {$IFDEF FPC}
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    {$ELSE}
    PairSplitter1: TPanel;
    PairSplitterSide1: TPanel;
    PairSplitterSide2: TPanel;
    {$ENDIF}
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
    procedure miBuyAccountClick(Sender: TObject);
    procedure miChangeAccountInfoClick(Sender: TObject);
    procedure miAccountInfoClick(Sender: TObject);
    procedure miChangeKeyClick(Sender: TObject);
    procedure miCopyOphashClick(Sender: TObject);
    procedure miOperationInfoClick(Sender: TObject);
    procedure miSendPASCClick(Sender: TObject);
    procedure miEnlistAccountsForSaleClick(Sender: TObject);
    procedure miDelistAccountsFromSaleClick(Sender: TObject);
  private
    FNodeNotifyEvents: TNodeNotifyEvents;
    FStopping: Boolean;
    FSynchronizing: Boolean;
    FAccountsMode: TCTRLWalletAccountsMode;
    FOperationsMode: TCTRLWalletOperationsMode;
    FOperationsHistory: TCTRLWalletOperationsHistory;
    FCTRLNoAccounts: TCTRLNoAccounts;
    FAccountsGrid: TVisualGrid;
    FOperationsGrid: TVisualGrid;
    FBalance : TBalanceSummary;
    FHasBalanceSummary: Boolean;
    FAccountsDataSource: TMyAccountsDataSource;
    FOperationsDataSource: TAccountsOperationsDataSource;
    function TryGetSelectedOperationHash(out AOperationHash: String): Boolean;
    procedure SetAccountsMode(AMode: TCTRLWalletAccountsMode);
    procedure SetOperationsMode(AMode: TCTRLWalletOperationsMode);
    procedure SetOperationsHistory(AHistory: TCTRLWalletOperationsHistory);
    procedure RefreshMyAccountsCombo;
    procedure RefreshTotals;
    procedure RefreshAccountsGrid;
    procedure RefreshOperationsGrid;
    procedure OnSynchronizationChanged(Sender: TObject; const AStatus: TNodeSynchronizationStatus);
    procedure OnBalanceSummaryCompleted(Sender: TObject);
    function GetSelectedAccounts : TArray<Cardinal>;
    procedure LayoutOverviewLabels;
    procedure CM_PrivateKeysChanged(var AMsg: TMessage); message CM_WalletControlKeysChanged;
  protected
    // Keep source configuration, invalidation and grid wiring in this control;
    // descendants can supply another typed backend through the same lifecycle.
    function CreateAccountsDataSource: TMyAccountsDataSource; virtual;
    function CreateOperationsDataSource: TAccountsOperationsDataSource; virtual;
    property AccountsGrid: TVisualGrid read FAccountsGrid;
    property OperationsGrid: TVisualGrid read FOperationsGrid;
    procedure RefreshBalances;
    procedure ActivateFirstTime; override;
    procedure OnPrivateKeysChanged(Sender: TObject);
    procedure OnUserKeyActivityDetected(Sender: TObject);
    procedure OnNodeBlocksChanged(Sender: TObject);
    procedure OnNodeNewOperation(Sender: TObject);
    procedure OnAccountsSelected(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection);
    procedure OnAccountsGridFinishedUpdating(Sender: TObject);
    procedure OnOperationSelected(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection);
    procedure OnPrepareAccountPopupMenu(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
    procedure OnPrepareOperationsPopupMenu(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
  public
    property SelectedAccounts : TArray<Cardinal> read GetSelectedAccounts;
    property AccountsMode: TCTRLWalletAccountsMode read FAccountsMode write SetAccountsMode;
    property OperationsMode: TCTRLWalletOperationsMode read FOperationsMode write SetOperationsMode;
    property OperationsHistory: TCTRLWalletOperationsHistory read FOperationsHistory write SetOperationsHistory;
  end;

implementation

uses
  UUserInterface, UCellRenderers, UBlockChain, UWallet, UCrypto,
  UMemory, Generics.Defaults, UCommon.Data, UCommon.Collections, UWIZOperation,
  Variants, Clipbrd;

type
  TOverviewGroupBoxAccess = class(TGroupBox);

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TCTRLWallet }

procedure TCTRLWallet.FormCreate(Sender: TObject);
var
  cmbDuration: TComboBox;
begin
  {$IFNDEF FPC}
  with TSplitter.Create(Self) do begin
    Parent := PairSplitter1;
    Left := PairSplitterSide1.Width;
    Width := 5;
    Align := alLeft;
  end;
  {$ENDIF}
  // event registrations
  FNodeNotifyEvents := TNodeNotifyEvents.Create(self);
  FNodeNotifyEvents.WatchKeys := TWallet.Keys.AccountsKeyList;
  FNodeNotifyEvents.OnKeyActivity:= OnUserKeyActivityDetected;
  FNodeNotifyEvents.OnBlocksChanged := OnNodeBlocksChanged;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;
  FNodeNotifyEvents.OnSynchronizationChanged := OnSynchronizationChanged;
  TWallet.Keys.OnChanged.Add(OnPrivateKeysChanged);
  TWallet.Keys.AccountsKeyList.ClearAccountKeyChanges;   // XXXXX CLEAR BUFFER on start

  // fields
  FAccountsDataSource := CreateAccountsDataSource;
  FAccountsDataSource.OnBalanceSummaryCompleted := OnBalanceSummaryCompleted;
  FAccountsDataSource.SnapshotCacheEnabled := true;
  FAccountsDataSource.IncludePending := true;
  FOperationsDataSource := CreateOperationsDataSource;
  FOperationsDataSource.SnapshotCacheEnabled := true;
  FOperationsDataSource.UseWalletAccounts := true;
  FOperationsDataSource.BlockDepth:=TTimeSpan.FromDays(7).TotalBlockCount;
  FOperationsHistory := woh7Days;
  FOperationsMode:= womAllAccounts;
  FAccountsMode := wamMyAccounts;
  FCTRLNoAccounts := TCTRLNoAccounts.Create(Self);
  FCTRLNoAccounts.BalanceSummary:=@FBalance;

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
    Renderer := TCellRenderers.PASC_CheckPendingBalance;
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

  // Native controls need their parent hierarchy before creating window handles.
  paAccounts.AddControlDockCenter(FAccountsGrid);
  paOperations.AddControlDockCenter(FOperationsGrid);

  // duration combo
  cmbDuration := TComboBox.Create(FOperationsGrid);
  cmbDuration.Style := csDropDownList;
  FOperationsGrid.WidgetControl := cmbDuration;
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
  // Node synchronization suspends both grids until local data is ready.
  // Outside that lifecycle, selections and updates need no quiet-time delay.
  FAccountsGrid.RefreshThrottle.Mode := temNone;
  FOperationsGrid.RefreshThrottle.Mode := temNone;
  OnSynchronizationChanged(Self, TUserInterface.SynchronizationStatus);
  // VCL does not publish the group box's resize event for form resources.
  TOverviewGroupBoxAccess(GroupBox1).OnResize := FormResize;
  // Assign each source once after the controls are configured and parented.
  FormResize(Self);
end;

procedure TCTRLWallet.FormDestroy(Sender: TObject);
var
  i: Integer;
  disposed: TDisposables;
begin
  FStopping := True;
  if Assigned(FAccountsDataSource) then
    FAccountsDataSource.OnBalanceSummaryCompleted := nil;
  if Assigned(TWallet.Keys) then
    TWallet.Keys.OnChanged.Remove(OnPrivateKeysChanged);
  if Assigned(FNodeNotifyEvents) then begin
    FNodeNotifyEvents.OnKeyActivity := nil;
    FNodeNotifyEvents.OnBlocksChanged := nil;
    FNodeNotifyEvents.OnOperationsChanged := nil;
    FNodeNotifyEvents.OnSynchronizationChanged := nil;
  end;
  if Assigned(cbAccounts) then begin
    for i := 0 to cbAccounts.Items.Count - 1 do
      disposed.AddObject(cbAccounts.Items.Objects[i]);
    cbAccounts.Items.Clear;
  end;
end;

procedure TCTRLWallet.FormResize(Sender: TObject);
begin
  if not Assigned(FAccountsGrid) or not Assigned(FOperationsGrid) then Exit;
  LayoutOverviewLabels;
  // Grid data-sources are set here on "first form resize" in order to avoid
  // excessive datasource fetching during initialising sequence. Note, grid
  // refreshes on size changed, blockchain activity and when assigned to grid
  if NOT Assigned(FAccountsGrid.DataSource) then
    FAccountsGrid.DataSource := FAccountsDataSource;

  if NOT Assigned(FOperationsGrid.DataSource) then
    FOperationsGrid.DataSource := FOperationsDataSource;
end;

procedure TCTRLWallet.LayoutOverviewLabels;
var
  LContent: Types.TRect;
  LFirstHeight, LSecondHeight, LGap, LTop: Integer;
begin
  if not HandleAllocated or not Assigned(GroupBox1) then Exit;
  // VCL reserves its caption inside ClientRect; LCL already excludes it.
  // Let the native group box define its content, then center both label pairs.
  LContent := GroupBox1.ClientRect;
  TOverviewGroupBoxAccess(GroupBox1).AdjustClientRect(LContent);
  LFirstHeight := Max(Label1.Height, lblTotalPASC.Height);
  LSecondHeight := Max(Label2.Height, lblTotalPASA.Height);
  LGap := Min(MulDiv(17, PixelsPerInch, 96),
    Max(0, LContent.Bottom - LContent.Top - LFirstHeight - LSecondHeight));
  LTop := LContent.Top + Max(0,
    (LContent.Bottom - LContent.Top - LFirstHeight - LGap - LSecondHeight) div 2);
  Label1.Top := LTop;
  lblTotalPASC.Top := LTop;
  Inc(LTop, LFirstHeight + LGap);
  Label2.Top := LTop;
  lblTotalPASA.Top := LTop;
end;

function TCTRLWallet.CreateAccountsDataSource: TMyAccountsDataSource;
begin
  Result := TMyAccountsDataSource.Create(Self);
end;

function TCTRLWallet.CreateOperationsDataSource: TAccountsOperationsDataSource;
begin
  Result := TAccountsOperationsDataSource.Create(Self);
end;

procedure TCTRLWallet.ActivateFirstTime;
begin
  // add first time-init here
end;

procedure TCTRLWallet.RefreshBalances;
begin
  if FStopping or TUserInterface.Stopping then Exit;
  // Invalidation is atomic; only the grids' admitted deliveries start queries.
  // Never defer this input behind visibility or an existing page worker.
  FAccountsDataSource.InvalidateWalletData;
  // The persisted key index does not emit every legacy key-activity event.
  // Resolve current membership once on this worker refresh as well, so newly
  // received or transferred accounts cannot remain absent from cached history.
  FOperationsDataSource.InvalidateWalletAccounts;
  FAccountsGrid.RefreshGrid;
  FOperationsGrid.RefreshGrid;
  // Page completion requests the independent aggregate; its owned worker
  // reports completion without polling or delaying the page.
end;

procedure TCTRLWallet.RefreshTotals;
begin
  // Synchronization owns the pause; completed pages and summary events can
  // update the overview immediately once that lifecycle has finished.
  if FSynchronizing then Exit;
  if FAccountsDataSource.TryGetBalanceSummary(FBalance) then begin
    FHasBalanceSummary := True;
    lblTotalPASC.Caption := TAccountComp.FormatMoney(FBalance.TotalPASC);
    lblTotalPASA.Caption := Format('%d', [FBalance.TotalPASA]);
  end else if not FHasBalanceSummary then begin
    // Unknown is not a zero balance. Retain a completed total on later
    // refreshes; only the first successful summary replaces these placeholders.
    lblTotalPASC.Caption := '-';
    lblTotalPASA.Caption := '-';
  end;
end;

procedure TCTRLWallet.RefreshMyAccountsCombo;
var
  i: integer;
  selectFirst, selectLast: boolean;
  last_key: TAccountKey;
  key: TWalletKey;
  str: String;
  disposed: TDisposables;
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
      disposed.AddObject(cbAccounts.Items.Objects[i]);
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
    // The previously selected key may have been deleted from the wallet.
    cbAccounts.ItemIndex := 0;
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
  index, keyIndex: integer;
  sel: TBox<TAccountKey>;
begin
  if Self.AccountsMode <> wamMyAccounts then exit; // showing getpasa
  if cbAccounts.ItemIndex = cbAccounts.Items.Count - 1 then exit; // not a key
  index := cbAccounts.ItemIndex;
  if (index < 0) or (index >= cbAccounts.Items.Count) then begin
    index := 0;
    cbAccounts.ItemIndex := index;
  end;
  if index > 0 then begin
    sel := TBox<TAccountKey>(cbAccounts.Items.Objects[index]);
    keyIndex := TWallet.Keys.IndexOfAccountKey(sel.Value);
    if keyIndex < 0 then begin
      index := 0;
      cbAccounts.ItemIndex := index;
    end;
  end;
  if index = 0 then begin
    gpMyAccounts.Caption := 'My Accounts';
    FAccountsDataSource.FilterKeys := TArray<TAccountKey>(TWallet.Keys.AccountsKeyList.ToArray);
  end else begin
    sel := TBox<TAccountKey>(cbAccounts.Items.Objects[cbAccounts.ItemIndex]);
    gpMyAccounts.Caption := Format('%s Accounts', [TWallet.Keys[keyIndex].Name]);
    FAccountsDataSource.FilterKeys := TArray<TAccountKey>.Create(sel.Value);
  end;
  FAccountsGrid.RefreshGrid;
end;

procedure TCTRLWallet.RefreshOperationsGrid;
begin
  // The same provider is reconfigured below. Cooperatively retire its old
  // query so obsolete history cannot delay or replace this selection's page.
  FOperationsGrid.CancelActiveFetch;
  case FOperationsMode of
    womAllAccounts: begin
      FOperationsGrid.Caption.Text := '';
      FOperationsDataSource.UseWalletAccounts := true;
    end;
    womSelectedAccounts:
    begin
      FOperationsGrid.Caption.Text := 'Selected Accounts';
      FOperationsDataSource.UseWalletAccounts := false;
      FOperationsDataSource.Accounts := SelectedAccounts;
    end else
      raise ENotSupportedException.Create(Format('AMode %d not supported', [integer(FOperationsMode)]));
  end;
  FOperationsGrid.RefreshGrid;
end;

function GetAccNoWithoutChecksum({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ARow: variant): cardinal;
begin
  Result := ARow.__KEY;
end;

function TCTRLWallet.GetSelectedAccounts : TArray<Cardinal>;
var
  LAccountTransform: TGlobalTransformFunc<Variant, Cardinal>;
begin
  LAccountTransform := GetAccNoWithoutChecksum;
  Result := TListTool<Variant, Cardinal>.Transform(FAccountsGrid.SelectedRows, LAccountTransform);
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
        if (cbAccounts.ItemIndex < 0) or (cbAccounts.ItemIndex >= cbAccounts.Items.Count - 1) then
          cbAccounts.ItemIndex := 0;
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
      wamFirstAccount: begin
        paAccounts.RemoveAllControls(False);
        paAccounts.AddControlDockCenter(FCTRLNoAccounts);
      end;
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
    RefreshOperationsGrid;
  finally
    FUILock.Release;
  end;
end;

procedure TCTRLWallet.SetOperationsHistory(AHistory: TCTRLWalletOperationsHistory);
begin
  // Changing the history window supersedes an in-flight traversal as well.
  FOperationsGrid.CancelActiveFetch;
  FOperationsHistory := AHistory;
  case FOperationsHistory of
    woh7Days: FOperationsDataSource.BlockDepth := TTimeSpan.FromDays(7).TotalBlockCount;
    woh30Days: FOperationsDataSource.BlockDepth := TTimeSpan.FromDays(30).TotalBlockCount;
    // Maximum means the whole local chain, without an arbitrary age limit.
    wohFullHistory: FOperationsDataSource.BlockDepth := High(Cardinal);
  end;
  FOperationsGrid.RefreshGrid;
end;

procedure TCTRLWallet.OnPrivateKeysChanged(Sender: TObject);
begin
  if FStopping or TUserInterface.Stopping then Exit;
  if TThread.CurrentThread.ThreadID <> MainThreadID then begin
    // RPC key imports notify on their worker. Posting keeps that producer
    // independent and binds delivery to this control's native lifetime.
    if HandleAllocated then PostMessage(Handle, CM_WalletControlKeysChanged, 0, 0);
    Exit;
  end;
  FAccountsDataSource.InvalidateWalletData;
  FOperationsDataSource.InvalidateWalletAccounts;
  RefreshMyAccountsCombo;
  RefreshAccountsGrid;
  RefreshOperationsGrid;
end;

procedure TCTRLWallet.CM_PrivateKeysChanged(var AMsg: TMessage);
begin
  OnPrivateKeysChanged(nil);
end;

procedure TCTRLWallet.OnUserKeyActivityDetected(Sender: TObject);
begin
  RefreshBalances;
end;

procedure TCTRLWallet.OnSynchronizationChanged(Sender: TObject; const AStatus: TNodeSynchronizationStatus);
var LWasSynchronizing: Boolean;
begin
  if FStopping or TUserInterface.Stopping then Exit;
  LWasSynchronizing := FSynchronizing;
  FSynchronizing := AStatus.State = nssSynchronizing;
  if not Assigned(FAccountsGrid) or not Assigned(FOperationsGrid) then Exit;
  if FSynchronizing then begin
    FAccountsGrid.RefreshSuspended := True;
    FOperationsGrid.RefreshSuspended := True;
    if not LWasSynchronizing then begin
      FAccountsDataSource.InvalidateWalletData;
      FAccountsDataSource.CancelBalanceSummary;
      FOperationsDataSource.InvalidateWalletAccounts;
    end;
    if not FAccountsGrid.LoadingVisible then
      FAccountsGrid.ShowLoading('Please Wait', 'Synchronizing');
    if not FOperationsGrid.LoadingVisible then
      FOperationsGrid.ShowLoading('Please Wait', 'Synchronizing');
  end else if LWasSynchronizing then begin
    // Resume one latest request after the node finishes its local work.
    // No quiet timer may delay the page or keep its loading card visible.
    FAccountsGrid.HideLoading;
    FOperationsGrid.HideLoading;
    FAccountsGrid.RefreshSuspended := False;
    FOperationsGrid.RefreshSuspended := False;
  end;
end;

procedure TCTRLWallet.OnBalanceSummaryCompleted(Sender: TObject);
begin
  if FStopping or TUserInterface.Stopping then Exit;
  // A newer input may have invalidated this worker while it was finishing.
  // RefreshTotals retains the last summary until the current one is ready.
  RefreshTotals;
end;

procedure TCTRLWallet.OnNodeBlocksChanged(Sender: TObject);
begin
  RefreshBalances;
end;

procedure TCTRLWallet.OnNodeNewOperation(Sender: TObject);
begin
  RefreshBalances;
end;

procedure TCTRLWallet.OnAccountsGridFinishedUpdating(Sender: TObject);
begin
  if FStopping or TUserInterface.Stopping then Exit;
  RefreshTotals;
end;

procedure TCTRLWallet.OnAccountsSelected(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection);
begin
  if ASelection.Page >= 0 then
    if FOperationsMode <> womSelectedAccounts then
      OperationsMode := womSelectedAccounts
    else
      RefreshOperationsGrid
  else
    OperationsMode := womAllAccounts;
end;

procedure TCTRLWallet.OnOperationSelected(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection);
var
  row: longint;
  v: variant;
  ophash: String;
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

procedure TCTRLWallet.OnPrepareAccountPopupMenu(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
var
  accNo: cardinal;
  account: TAccount;
begin
  miSep1.Visible := ASelection.RowCount = 1;
  miAccountInfo.Visible := ASelection.RowCount = 1;
  miBuyAccount.Visible := ASelection.RowCount = 1;
  miSendPASC.Caption := IIF(ASelection.RowCount = 1, 'Send PASC', 'Send All PASC');
  miChangeKey.Caption := IIF(ASelection.RowCount = 1, 'Change Key', 'Change All Key');
  miEnlistAccountsForSale.Caption := IIF(ASelection.RowCount = 1, 'Enlist Account For Sale', 'Enlist All Account For Sale');
  miDelistAccountsFromSale.Caption := IIF(ASelection.RowCount = 1, 'Delist Account From Sale', 'Delist All Account From Sale');
  miChangeAccountInfo.Caption := IIF(ASelection.RowCount = 1, 'Change Account Information', 'Change Accounts Information');
  if ASelection.RowCount = 1 then begin
    if not TAccountComp.AccountTxtNumberToAccountNumber(FAccountsGrid.Rows[ASelection.Row].Account, accNo) then
      raise Exception.Create('Error Parsing Account Number From Grid');
    account := TNode.Node.GetAccount(accNo);
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
begin
  TUserInterface.ShowSendDialog(SelectedAccounts);
end;

procedure TCTRLWallet.miChangeKeyClick(Sender: TObject);
begin
  TUserInterface.ShowChangeKeyDialog(SelectedAccounts);
end;

procedure TCTRLWallet.miEnlistAccountsForSaleClick(Sender: TObject);
begin
  TUserInterface.ShowSellAccountsDialog(SelectedAccounts);
end;

procedure TCTRLWallet.miDelistAccountsFromSaleClick(Sender: TObject);
begin
  TUserInterface.ShowDelistAccountsDialog(SelectedAccounts);
end;

procedure TCTRLWallet.miChangeAccountInfoClick(Sender: TObject);
begin
  TUserInterface.ShowChangeAccountInfoDialog(SelectedAccounts);
end;

procedure TCTRLWallet.miBuyAccountClick(Sender: TObject);
begin
  TUserInterface.ShowBuyAccountDialog(SelectedAccounts);
end;

procedure TCTRLWallet.OnPrepareOperationsPopupMenu(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu);
var
  operationHash: String;
begin
  if (ASelection.RowCount = 1) and TryGetSelectedOperationHash(operationHash) then begin
    miSep2.Visible := True;
    miOperationInfo.Visible := True;
    miCopyOphash.Visible := True;
    APopupMenu := mnuOperationsPopup;
  end else begin
    miSep2.Visible := False;
    miOperationInfo.Visible := False;
    miCopyOphash.Visible := False;
    APopupMenu := nil; // is empty, so dont show
  end;
end;

function TCTRLWallet.TryGetSelectedOperationHash(out AOperationHash: String): Boolean;
var
  rows: TArray<Variant>;
  value: Variant;
begin
  Result := False;
  AOperationHash := '';
  rows := FOperationsGrid.SelectedRows;
  if Length(rows) <> 1 then exit;
  value := rows[0].__KEY;
  if VarIsNull(value) or VarIsEmpty(value) then exit;
  AOperationHash := VarToStr(value);
  Result := TPCOperation.IsValidOperationHash(AOperationHash);
end;

procedure TCTRLWallet.miCopyOphashClick(Sender: TObject);
var
  operationHash: String;
begin
  if TryGetSelectedOperationHash(operationHash) then
    Clipboard.AsText := operationHash;
end;

procedure TCTRLWallet.miOperationInfoClick(Sender: TObject);
var
  operationHash: String;
begin
  if TryGetSelectedOperationHash(operationHash) then
    TUserInterface.ShowOperationInfoDialog(Self, operationHash);
end;

end.
