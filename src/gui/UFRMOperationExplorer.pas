unit UFRMOperationExplorer;

{$mode delphi}

{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, Buttons, UVisualGrid, UCommon.UI, Generics.Collections,
  UAccounts, UDataSources, UNode;

type

  { TFRMOperationExplorer }

  TFRMOperationExplorer = class(TApplicationForm)
    ebFilterOperationsEndBlock: TEdit;
    ebFilterOperationsStartBlock: TEdit;
    gpRecentOps: TGroupBox;
    gpFilter: TGroupBox;
    miFindOperationByHash: TMenuItem;
    miDecodePayload: TMenuItem;
    miTools: TMenuItem;
    OperationExplorerMenu: TMainMenu;
    paOperations: TPanel;
    procedure ebFilterOperationsAccountExit(Sender: TObject);
    procedure ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure miDecodePayloadClick(Sender: TObject);
    procedure miFindOperationByHashClick(Sender: TObject);
  private
    FMaxBlocks: integer;
    FUpdating: boolean;
    FBlockStart, FBlockEnd: int64;
    FNodeNotifyEvents: TNodeNotifyEvents;
    FOperationsGrid: TVisualGrid;
    FOperationsDataSource: TOperationsDataSource;
    procedure RefreshOperationsGrid;
    procedure SetBlocks(AStart, AEnd: int64);
    procedure SetMaxBlocks(AValue: integer);
  protected
    procedure OnNodeBlocksChanged(Sender: TObject);
    procedure OnNodeNewOperation(Sender: TObject);
    procedure OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
  public
    { public declarations }
    property MaxBlocks: integer read FMaxBlocks write SetMaxBlocks;
  end;

implementation

uses
  UUserInterface, UCellRenderers, UBlockChain, UWallet, UCrypto,
  UCommon, UMemory, Generics.Defaults, UCommon.Data, UCommon.Collections;

{$R *.lfm}

{ TFRMOperationExplorer }

procedure TFRMOperationExplorer.ebFilterOperationsAccountExit(Sender: TObject);
var
  LStart, LEnd: int64;
begin
  if not FUpdating then
    try
      FUpdating := True;// move to finally
      LStart := StrToInt64Def(ebFilterOperationsStartBlock.Text, -1);
      if LStart >= 0 then
        ebFilterOperationsStartBlock.Text := IntToStr(LStart)
      else
        ebFilterOperationsStartBlock.Text := '';
      LEnd := StrToInt64Def(ebFilterOperationsEndBlock.Text, -1);
      if LEnd >= 0 then
        ebFilterOperationsEndBlock.Text := IntToStr(LEnd)
      else
        ebFilterOperationsEndBlock.Text := '';
      SetBlocks(LStart, LEnd);
    finally
      FUpdating := False;
    end;
end;

procedure TFRMOperationExplorer.ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ebFilterOperationsAccountExit(nil);
end;

procedure TFRMOperationExplorer.FormCreate(Sender: TObject);
begin

  FUpdating := False;
  FBlockStart := -1;
  FBlockEnd := -1;
  FMaxBlocks := 300;
  // event registrations
  FNodeNotifyEvents := TNodeNotifyEvents.Create(self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeBlocksChanged;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;

  // fields
  FOperationsDataSource := TOperationsDataSource.Create(Self);

  // grids
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
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Filters := SORTABLE_TEXT_FILTER;
  end;
  with FOperationsGrid.AddColumn('Account') do
  begin
    Binding := 'AccountNumber';
    DisplayBinding := 'Account';
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
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
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Renderer := TCellRenderers.PASC_CheckPendingBalance;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FOperationsGrid.AddColumn('Fee') do
  begin
    Binding := 'FeeDecimal';
    SortBinding := 'Fee';
    DisplayBinding := 'Fee';
    Width := 100;
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

  FOperationsGrid.OnSelection := OnOperationSelected;
  FOperationsGrid.Caption.Alignment := taCenter;
  FOperationsGrid.Caption.Text := 'All Operations';
  FOperationsGrid.Caption.Text := '';
  FOperationsGrid.Caption.Visible := True;

  // Add datasources to grid
  FOperationsGrid.DataSource := FOperationsDataSource;

  // Add grid to panels
  paOperations.AddControlDockCenter(FOperationsGrid);
end;

procedure TFRMOperationExplorer.miDecodePayloadClick(Sender: TObject);
begin
  TUserInterface.ShowOperationInfoDialog(Self, '');
end;

procedure TFRMOperationExplorer.miFindOperationByHashClick(Sender: TObject);
var
  LOpHash: string;
begin
  if not InputQuery('Search operation by OpHash', 'Insert Operation Hash value (OpHash)', LOpHash) then
    Exit;

  TUserInterface.ShowOperationInfoDialog(Self, LOpHash);
end;

procedure TFRMOperationExplorer.RefreshOperationsGrid;
var
  LNode: TNode;
  LStart, LEnd: int64;
begin
  LNode := FNodeNotifyEvents.Node;
  if FBlockEnd < 0 then
  begin
    if LNode.Bank.BlocksCount > 0 then
      LEnd := LNode.Bank.BlocksCount - 1
    else
      LEnd := 0;
  end
  else
    LEnd := FBlockEnd;
  if FBlockStart < 0 then
  begin
    if (LEnd > MaxBlocks) then
      LStart := LEnd - MaxBlocks
    else
      LStart := 0;
  end
  else
    LStart := FBlockStart;
  if LStart < 0 then
    LStart := 0;
  if LEnd >= LNode.Bank.BlocksCount then
    LEnd := LNode.Bank.BlocksCount;

  FOperationsDataSource.StartBlock := LStart;
  FOperationsDataSource.EndBlock := LEnd;

  FOperationsGrid.RefreshGrid;
end;

procedure TFRMOperationExplorer.SetBlocks(AStart, AEnd: int64);
begin
  if (AStart = FBlockStart) and (AEnd = FBlockEnd) then
    Exit;
  FBlockStart := AStart;
  FBlockEnd := AEnd;
  if (FBlockEnd > 0) and (FBlockStart > FBlockEnd) then
    FBlockStart := -1;
  RefreshOperationsGrid;
end;

procedure TFRMOperationExplorer.SetMaxBlocks(AValue: integer);
begin
  if FMaxBlocks = AValue then
    Exit;
  FMaxBlocks := AValue;
  if (FMaxBlocks <= 0) or (FMaxBlocks > 500) then
    FMaxBlocks := 300;
  RefreshOperationsGrid;
end;

procedure TFRMOperationExplorer.OnNodeBlocksChanged(Sender: TObject);
begin
  // TODO: play block sound chime
  RefreshOperationsGrid;
end;

procedure TFRMOperationExplorer.OnNodeNewOperation(Sender: TObject);
begin
  // TODO: play operation sound tick
  RefreshOperationsGrid;
end;

procedure TFRMOperationExplorer.OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
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

end.
