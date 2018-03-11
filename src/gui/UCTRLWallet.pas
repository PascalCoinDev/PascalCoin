unit UCTRLWallet;

{$mode delphi}

{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, PairSplitter, Buttons, UVisualGrid, UCommon.UI,
  UAccounts, UDataSources, UNode;

type

  { TCTRLWallet }

  TCTRLWalletAccountsMode = (wamMyAccounts, wamFirstAccount);
  TCTRLWalletOperationsMode = (womUnknown, womSelectedAccounts, womAllAccounts);
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
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    paAccounts: TPanel;
    paOperations: TPanel;
    procedure cbAccountsChange(Sender: TObject);
    procedure cmbDurationChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FNodeNotifyEvents : TNodeNotifyEvents;
    FAccountsMode : TCTRLWalletAccountsMode;
    FOperationsMode : TCTRLWalletOperationsMode;
    FOperationsHistory : TCTRLWalletOperationsHistory;
    FAccountsGrid : TVisualGrid;
    FOperationsGrid : TVisualGrid;
    FAccountsDataSource : TUserAccountsDataSource;
    FOperationsDataSource : TAccountsOperationsDataSource;
    procedure SetAccountsMode(AMode: TCTRLWalletAccountsMode);
    procedure SetOperationsMode(AMode: TCTRLWalletOperationsMode);
    procedure SetOperationsHistory(AHistory: TCTRLWalletOperationsHistory);
  protected
    procedure ActivateFirstTime; override;
    procedure OnNodeBlocksChanged(Sender: TObject);
    procedure OnNodeNewOperation(Sender: TObject);
    procedure OnAccountsUpdated(Sender: TObject);
    procedure OnAccountsSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
    procedure OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
    procedure OnAccountsGridColumnInitialize(Sender: TObject; AColIndex:Integer; AColumn: TVisualColumn);
    procedure OnOperationsGridColumnInitialize(Sender: TObject; AColIndex:Integer; AColumn: TVisualColumn);
  public
    property AccountsMode : TCTRLWalletAccountsMode read FAccountsMode write SetAccountsMode;
    property OperationsMode : TCTRLWalletOperationsMode read FOperationsMode write SetOperationsMode;
    property OperationsHistory : TCTRLWalletOperationsHistory read FOperationsHistory write SetOperationsHistory;
  end;

implementation

uses
  UUserInterface, UBlockChain, UCommon, UAutoScope, Generics.Collections, UCommon.Collections;

{$R *.lfm}

{ TCTRLWallet }

procedure TCTRLWallet.FormCreate(Sender: TObject);
var cmbDuration : TComboBox;
begin
  // event registrations
  FNodeNotifyEvents := TNodeNotifyEvents.Create (self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeBlocksChanged;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;

  // data sources
  FAccountsDataSource := TUserAccountsDataSource.Create(Self);
  FOperationsDataSource:= TAccountsOperationsDataSource.Create(Self);

  // grids
  FAccountsGrid := TVisualGrid.Create(Self);
  FAccountsGrid.SortMode := smMultiColumn;
  FAccountsGrid.FetchDataInThread:= true;
  FAccountsGrid.AutoPageSize:= true;
  FAccountsGrid.SelectionType:= stMultiRow;
  FAccountsGrid.Options := [vgoColAutoFill, vgoColSizing, vgoAllowDeselect, vgoSortDirectionAllowNone];
  FAccountsGrid.DefaultColumnWidths := TArray<Integer>.Create(
    100,                   // Account
    CT_VISUALGRID_STRETCH, // Name
    100                    // Balance
  );
  FAccountsGrid.OnColumnInitialize:= OnAccountsGridColumnInitialize;
  FAccountsGrid.OnSelection := OnAccountsSelected;
  FAccountsGrid.OnFinishedUpdating := OnAccountsUpdated;

  FOperationsGrid := TVisualGrid.Create(Self);
  FOperationsGrid.SortMode := smMultiColumn;
  FOperationsGrid.FetchDataInThread:= true;
  FOperationsGrid.AutoPageSize:= true;
  FOperationsGrid.SelectionType:= stRow;
  FOperationsGrid.Options := [vgoColAutoFill, vgoAllowDeselect, vgoColSizing, vgoSortDirectionAllowNone];
  FOperationsGrid.DefaultColumnWidths := TArray<Integer>.Create(
    130,                   // Time
    CT_VISUALGRID_DEFAULT, // Block
    100,                   // Account
    150,                   // Type
    130,                   // Amount
    CT_VISUALGRID_DEFAULT, // Fee
    100,                   // Balance
    CT_VISUALGRID_DEFAULT, // Payload
    80,                    // OPHASH
    CT_VISUALGRID_STRETCH  // Description (stretch)
  );
  FOperationsGrid.OnColumnInitialize:= OnOperationsGridColumnInitialize;
  FOperationsGrid.OnSelection := OnOperationSelected;
  FOperationsGrid.Caption.Alignment:= taCenter;
  FOperationsGrid.Caption.Text := 'All Account Operations';
  FOperationsGrid.Caption.Visible := true;
  cmbDuration := TComboBox.Create(FOperationsGrid);
  FOperationsGrid.WidgetControl := cmbDuration;
  cmbDuration.Items.BeginUpdate;
  try
    cmbDuration.AddItem('30 Days', TObject(woh30Days));
    cmbDuration.AddItem('Maximum', TObject(wohFullHistory));
  finally
    cmbDuration.Items.EndUpdate;
    cmbDuration.ItemIndex:=0;;
  end;
  cmbDuration.OnChange:=cmbDurationChange;

  // dock operations grid in panel
  paOperations.AddControlDockCenter(FOperationsGrid);

  // Configure grid states
  AccountsMode := wamMyAccounts;
  OperationsMode:= womUnknown;
  OperationsHistory := woh30Days;
end;

procedure TCTRLWallet.FormResize(Sender: TObject);
begin
  // Left hand panel is 50% the size up until a max size of 450

end;

procedure TCTRLWallet.ActivateFirstTime;
begin

end;

procedure TCTRLWallet.OnAccountsGridColumnInitialize(Sender: TObject; AColIndex:Integer; AColumn: TVisualColumn);
begin
  case AColIndex of
     2: AColumn.InternalColumn.Alignment := taRightJustify;
  end;
end;

procedure TCTRLWallet.OnOperationsGridColumnInitialize(Sender: TObject; AColIndex:Integer; AColumn: TVisualColumn);
begin
  case AColIndex of
     4, 5, 6: AColumn.InternalColumn.Alignment := taRightJustify;
  end;
end;

procedure TCTRLWallet.SetAccountsMode(AMode: TCTRLWalletAccountsMode);
begin
  paAccounts.RemoveAllControls(false);
  case AMode of
     wamMyAccounts: begin
       FOperationsGrid.DataSource := FOperationsDataSource;
       FAccountsGrid.DataSource := FAccountsDataSource;
       FAccountsGrid.Caption.Text := 'My Accounts';
       paAccounts.AddControlDockCenter(FAccountsGrid);
       FAccountsGrid.RefreshGrid;
     end;
     wamFirstAccount: raise Exception.Create('Not implemented');
  end;
end;

procedure TCTRLWallet.SetOperationsMode(AMode: TCTRLWalletOperationsMode);

  function GetAccNo (constref AAccount : TAccount) : Cardinal; overload;
  begin
    Result := AAccount.account;
  end;

  function GetAccNo(constref ARow : Variant) : Cardinal; overload;
  begin
    if NOT TAccountComp.AccountTxtNumberToAccountNumber( ARow.Account, Result ) then
      raise Exception.Create('Internal Error: Unable to parse account number from table row');
  end;

begin
  case AMode of
     womUnknown: begin
       FOperationsGrid.Caption.Text := '';
       FOperationsDataSource.Accounts := TArrayTool<Cardinal>.Empty;
     end;
     womAllAccounts: begin
       FOperationsGrid.Caption.Text := 'All Accounts';
       FOperationsDataSource.Accounts := TListTool<TAccount, Cardinal>.Transform( FAccountsDataSource.LastKnownUserAccounts, GetAccNo );
     end;
     womSelectedAccounts: begin
       FOperationsGrid.Caption.Text := 'Selected Accounts';
       FOperationsDataSource.Accounts := TListTool<Variant, Cardinal>.Transform( FAccountsGrid.SelectedRows, GetAccNo);
     end
     else raise ENotSupportedException.Create(Format('AMode %d not supported', [Integer(AMode)]));
  end;
  FOperationsGrid.RefreshGrid;
  FOperationsMode:=AMode;
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
   lblTotalPASC.Caption := TAccountComp.FormatMoney( FAccountsDataSource.Overview.TotalPASC );
   lblTotalPASA.Caption := Format('%d', [FAccountsDataSource.Overview.TotalPASA] );
end;

procedure TCTRLWallet.OnAccountsSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
var
  row : longint;
  selectedAccounts : Generics.Collections.TList<Cardinal>;
  acc : Cardinal;
  GC : TScoped;
begin
  selectedAccounts := GC.AddObject( TList<Cardinal>.Create ) as TList<Cardinal>;

  if ASelection.RowCount > 0 then begin
    for row := ASelection.Row to (ASelection.Row + ASelection.RowCount - 1) do begin
      if (TAccountComp.AccountTxtNumberToAccountNumber( FAccountsGrid.Rows[row].Account, acc)) then
        selectedAccounts.Add(acc);
    end;
    FOperationsDataSource.Accounts := selectedAccounts.ToArray;
  end else begin

    TUserInterface.ShowInfo(Self, 'Deselect', 'Deselect');
  end;
  FOperationsGrid.RefreshGrid;
end;

procedure TCTRLWallet.OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
var
  row : longint;
  v : Variant;
  ophash : AnsiString;
begin
  row := ASelection.Row;
  if (row >= 0) AND (row < FOperationsGrid.RowCount) then begin
    v := FOperationsGrid.Rows[row];
    ophash := FOperationsGrid.Rows[row].OPHASH;
    if TPCOperation.IsValidOperationHash(ophash) then
      TUserInterface.ShowOperationInfoDialog(self, ophash);
  end;
end;

procedure TCTRLWallet.cbAccountsChange(Sender: TObject);
begin
  AccountsMode := wamMyAccounts;
end;

procedure TCTRLWallet.cmbDurationChange(Sender: TObject);
var
  cmbDuration : TComboBox;
begin
  cmbDuration := Sender as TComboBox;
  if not Assigned(cmbDuration) then
    exit;

  case cmbDuration.ItemIndex of
     0: OperationsHistory := woh30Days;
     1: OperationsHistory := wohFullHistory;
  end;
end;

end.

