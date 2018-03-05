unit UCTRLWallet;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, PairSplitter, Buttons, UVisualGrid, UCommon.UI, UDataSources, UNode;

type

  { TCTRLWallet }

  TCTRLWalletAccountView = (wavAllAccounts, wavMyAccounts, wavFirstAccount);

  TCTRLWalletDuration = (wd30Days, wdFullHistory);

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
    FAccountsView : TCTRLWalletAccountView;
    FDuration : TCTRLWalletDuration;
    FAccountsGrid : TVisualGrid;
    FOperationsGrid : TVisualGrid;
    FAccountsDataSource : TUserAccountsDataSource;
    FOperationsDataSource : TAccountsOperationsDataSource;
    procedure SetAccountsView(view: TCTRLWalletAccountView);
    procedure SetDuration(const ADuration: TCTRLWalletDuration);
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
    property Duration : TCTRLWalletDuration read FDuration write SetDuration;
    property AccountsView : TCTRLWalletAccountView read FAccountsView write SetAccountsView;
  end;

implementation

uses
  UUserInterface, UAccounts, UBlockChain, UCommon, UAutoScope, Generics.Collections;

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
  FAccountsGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone];
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
  FOperationsGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone];
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
    cmbDuration.AddItem('30 Days', TObject(wd30Days));
    cmbDuration.AddItem('Maximum', TObject(wdFullHistory));
  finally
    cmbDuration.Items.EndUpdate;
    cmbDuration.ItemIndex:=0;;
  end;
  cmbDuration.OnChange:=cmbDurationChange;

  AccountsView := wavMyAccounts;
  paOperations.AddControlDockCenter(FOperationsGrid);
  Duration := wd30Days;
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

procedure TCTRLWallet.SetAccountsView(view: TCTRLWalletAccountView);
begin
  paAccounts.RemoveAllControls(false);
  case view of
     wavAllAccounts: raise Exception.Create('Not implemented');
     wavMyAccounts: begin
       FOperationsGrid.DataSource := FOperationsDataSource;
       FAccountsGrid.DataSource := FAccountsDataSource;
       FAccountsGrid.Caption.Text := 'My Accounts';
       paAccounts.AddControlDockCenter(FAccountsGrid);
       FAccountsGrid.RefreshGrid;
     end;
     wavFirstAccount: raise Exception.Create('Not implemented');
  end;
end;

procedure TCTRLWallet.SetDuration(const ADuration: TCTRLWalletDuration);
begin
  FDuration:= ADuration;
  case FDuration of
    wd30Days: FOperationsDataSource.TimeSpan := TTimeSpan.FromDays(30);
    wdFullHistory: FOperationsDataSource.TimeSpan := TTimeSpan.FromDays(10 * 365);
  end;
  FOperationsGrid.RefreshGrid;
end;

procedure TCTRLWallet.OnNodeBlocksChanged(Sender: TObject);
begin
  FAccountsGrid.RefreshGrid;
  SetDuration(FDuration);
end;

procedure TCTRLWallet.OnNodeNewOperation(Sender: TObject);
begin
  FAccountsGrid.RefreshGrid;
  SetDuration(FDuration);
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

  for row := ASelection.Row to (ASelection.Row + ASelection.RowCount - 1) do begin
    if (TAccountComp.AccountTxtNumberToAccountNumber( FAccountsGrid.Rows[row].Account, acc)) then
      selectedAccounts.Add(acc);
  end;
  FOperationsDataSource.Accounts := selectedAccounts.ToArray;
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
  case cbAccounts.ItemIndex of
     0: AccountsView := wavAllAccounts;
     1: AccountsView := wavMyAccounts;
     2: AccountsView := wavFirstAccount;
  end;
end;

procedure TCTRLWallet.cmbDurationChange(Sender: TObject);
var
  cmbDuration : TComboBox;
  newDuration : TCTRLWalletDuration;
begin
  cmbDuration := Sender as TComboBox;
  if not Assigned(cmbDuration) then
    exit;

  case cmbDuration.ItemIndex of
     0: newDuration := wd30Days;
     1: newDuration := wdFullHistory;
  end;
  if Duration <> newDuration then begin
    Duration := newDuration;
    FOperationsGrid.RefreshGrid;
  end;
end;

end.

