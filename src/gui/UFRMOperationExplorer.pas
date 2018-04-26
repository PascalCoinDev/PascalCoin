unit UFRMOperationExplorer;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/master/Units/Forms/UFRMWallet.pas
}


interface

{$I ..\config.inc}

uses
  LCLIntf, LCLType, SysUtils, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Grids, Menus, Classes,
  UCommon.UI, UConst, UDataSources, UNode, UVisualGrid, UCellRenderers, UCommon.Data, UCoreUtils;

type

  { TFRMOperationExplorer }

  TFRMOperationExplorer = class(TApplicationForm)
    ebFilterOperationsEndBlock: TEdit;
    ebFilterOperationsStartBlock: TEdit;
    grpOperationExplorer: TGroupBox;
    Label2: TLabel;
    OperationsExplorerMenu: TMainMenu;
    miDecodePayload: TMenuItem;
    miFindOperationByOpHash: TMenuItem;
    miTools: TMenuItem;
    Panel1: TPanel;
    paGrid: TPanel;
    procedure ebFilterOperationsAccountExit(Sender: TObject);
    procedure ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure miDecodePayloadClick(Sender: TObject);
    procedure miFindOperationByOpHashClick(Sender: TObject);

  protected
    procedure OnNodeBlocksChanged(Sender: TObject);
    procedure OnNodeNewOperation(Sender: TObject);
    procedure OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
  private
    { private declarations }
    FUpdating: boolean;
    FBlockStart, FBlockEnd: int64;
    FNodeNotifyEvents: TNodeNotifyEvents;
    FOperationsGrid: TVisualGrid;
    FOperationsDataSource: TOperationsDataSource;
    procedure UpdateVisualGridUI();
    procedure SetBlocks(AStart, AEnd: int64);
  public
    { public declarations }
  end;

implementation

{$r *.lfm}

uses UUserInterface, UMemory, UFRMPayloadDecoder, UBlockChain, UWallet, Generics.Collections;

procedure TFRMOperationExplorer.FormCreate(Sender: TObject);
begin

  // event registrations
  FNodeNotifyEvents := TNodeNotifyEvents.Create(self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeBlocksChanged;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeNewOperation;

  FUpdating := False;
  FBlockStart := -1;
  FBlockEnd := -1;

  UpdateVisualGridUI();

end;

procedure TFRMOperationExplorer.miDecodePayloadClick(Sender: TObject);
begin
  TUserInterface.ShowOperationInfoDialog(Self, '');
end;

procedure TFRMOperationExplorer.miFindOperationByOpHashClick(Sender: TObject);
var
  ophash: string;
begin
  if not InputQuery('Search operation by OpHash', 'Insert Operation Hash value (OpHash)', ophash) then
    Exit;

  TUserInterface.ShowOperationInfoDialog(Self, ophash);
end;

procedure TFRMOperationExplorer.OnNodeBlocksChanged(Sender: TObject);
begin
  UpdateVisualGridUI(); //main
end;

procedure TFRMOperationExplorer.OnNodeNewOperation(Sender: TObject);
begin
  UpdateVisualGridUI();
end;

procedure TFRMOperationExplorer.OnOperationSelected(Sender: TObject; constref ASelection: TVisualGridSelection);
var
  LRow: longint;
  LOperationResumeList: TList<TOperationResume>;
  LGC: TDisposables;
begin
  LOperationResumeList := LGC.AddObject(TList<TOperationResume>.Create) as TList<TOperationResume>;
  LRow := ASelection.Row;
  if (LRow >= 0) and (LRow < FOperationsGrid.RowCount) then
  begin
    FOperationsDataSource.FetchAll(LOperationResumeList);
    TUserInterface.ShowOperationInfoDialog(self, LOperationResumeList[LRow]);
    FOperationsGrid.ClearSelection;
  end;
end;

procedure TFRMOperationExplorer.UpdateVisualGridUI();
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
    if (LEnd > 300) then
      LStart := LEnd - 300
    else
      LStart := 0;
  end
  else
    LStart := FBlockStart;
  if LStart < 0 then
    LStart := 0;
  if LEnd >= LNode.Bank.BlocksCount then
    LEnd := LNode.Bank.BlocksCount;
  // fields
  FOperationsDataSource := TOperationsDataSource.Create(Self);

  FOperationsDataSource.StartBlock := LStart;
  FOperationsDataSource.EndBlock := LEnd;

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

  FOperationsGrid.OnSelection := OnOperationSelected;
  FOperationsGrid.Caption.Alignment := taCenter;
  FOperationsGrid.Caption.Text := 'All Operations';
  FOperationsGrid.Caption.Visible := True;

  // Add datasources to grid
  FOperationsGrid.DataSource := FOperationsDataSource;

  // Add grid to panels
  paGrid.AddControlDockCenter(FOperationsGrid);
end;

procedure TFRMOperationExplorer.SetBlocks(AStart, AEnd: int64);
begin
  if (AStart = FBlockStart) and (AEnd = FBlockEnd) then
    Exit;
  FBlockStart := AStart;
  FBlockEnd := AEnd;
  if (FBlockEnd > 0) and (FBlockStart > FBlockEnd) then
    FBlockStart := -1;
  UpdateVisualGridUI();
end;

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

end.
