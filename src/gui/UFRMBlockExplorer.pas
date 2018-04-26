unit UFRMBlockExplorer;

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

  { TFRMBlockExplorer }

  TFRMBlockExplorer = class(TApplicationForm)
    ebBlockChainBlockEnd: TEdit;
    ebBlockChainBlockStart: TEdit;
    grpBlockExplorer: TGroupBox;
    Label2: TLabel;
    BlockExplorerMenu: TMainMenu;
    miDecodePayload: TMenuItem;
    miTools: TMenuItem;
    Panel1: TPanel;
    paGrid: TPanel;
    procedure ebBlockChainBlockExit(Sender: TObject);
    procedure ebBlockChainBlockStartKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);

  protected
    procedure OnNodeNewAccount(Sender: TObject);
  private
    FMaxBlocks: integer;
    { private declarations }
    FUpdating: boolean;
    FBlockStart, FBlockEnd: int64;
    FNodeNotifyEvents: TNodeNotifyEvents;
    FBlockChainGrid: TVisualGrid;
    FBlockChainDataSource: TBlockChainDataSource;

    FHashRateAs: TShowHashRateAs;
    FHashRateAverageBlocksCount: integer;

    procedure SetMaxBlocks(AValue: integer);
    procedure UpdateVisualGridUI();
    procedure SetBlocks(AStart, AEnd: int64);
  public
    { public declarations }
    property MaxBlocks: integer read FMaxBlocks write SetMaxBlocks;
  end;

implementation

{$r *.lfm}

uses UUserInterface, UMemory, UFRMPayloadDecoder, UBlockChain, UWallet, Generics.Collections;

procedure TFRMBlockExplorer.FormCreate(Sender: TObject);
begin

  // event registrations
  FNodeNotifyEvents := TNodeNotifyEvents.Create(self);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeNewAccount;

  FUpdating := False;
  FBlockStart := -1;
  FBlockEnd := -1;
  FMaxBlocks := 300;
  FHashRateAverageBlocksCount := 50;
  FHashRateAs:={$IFDEF PRODUCTION}hr_Giga{$ELSE}hr_Mega{$ENDIF};

  UpdateVisualGridUI();

end;

procedure TFRMBlockExplorer.OnNodeNewAccount(Sender: TObject);
begin
  UpdateVisualGridUI(); //main
end;

procedure TFRMBlockExplorer.UpdateVisualGridUI();
var
  LNode: TNode;
  LStart, LEnd: int64;
  LHashType: string;
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
  // fields
  FBlockChainDataSource := TBlockChainDataSource.Create(Self);

  FBlockChainDataSource.HashRateAs := FHashRateAs;
  FBlockChainDataSource.HashRateAverageBlocksCount := FHashRateAverageBlocksCount;

  FBlockChainDataSource.StartBlock := LStart;
  FBlockChainDataSource.EndBlock := LEnd;

  FBlockChainGrid := TVisualGrid.Create(Self);
  FBlockChainGrid.SortMode := smMultiColumn;
  FBlockChainGrid.FetchDataInThread := True;
  FBlockChainGrid.AutoPageSize := True;
  FBlockChainGrid.DeselectionType := dtDefault;
  FBlockChainGrid.SelectionType := stRow;
  FBlockChainGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone, vgoAutoHidePaging, vgoAutoHideSearchPanel];

  with FBlockChainGrid.AddColumn('Block') do
  begin
    AutoWidth := True;
    Filters := SORTABLE_TEXT_FILTER;
  end;
  with FBlockChainGrid.AddColumn('Time') do
  begin
    SortBinding := 'UnixTime';
    DisplayBinding := 'UnixTime';
    Renderer := TCellRenderers.OperationTime;
    Width := 130;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FBlockChainGrid.AddColumn('Ops') do
  begin
   // Sanitizer := TCellRenderers.OperationTypeSanitizer;
    Width := 50;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FBlockChainGrid.AddColumn('Volume') do
  begin
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
   // Renderer := TCellRenderers.PASC_CheckPendingBalance;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FBlockChainGrid.AddColumn('Reward') do
  begin
    Width := 100;
    HeaderAlignment := taRightJustify;
    DataAlignment := taRightJustify;
    Renderer := TCellRenderers.PASC_CheckPendingBalance;
    Filters := SORTABLE_NUMERIC_FILTER;
  end;
  with FBlockChainGrid.AddColumn('Fee') do
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
  with FBlockChainGrid.AddColumn('Target') do
  begin
    Width := 100;
   // Renderer := TCellRenderers.Payload;
    Filters := SORTABLE_TEXT_FILTER;
  end;
  case FBlockChainDataSource.HashRateAs of
    hr_Kilo: LHashType := 'Kh/s';
    hr_Mega: LHashType := 'Mh/s';
    hr_Giga: LHashType := 'Gh/s';
    hr_Tera: LHashType := 'Th/s';
    else
      LHashType := '?h/s';
  end;

  with FBlockChainGrid.AddColumn(LHashType) do
  begin
    Width := 100;
   // Renderer := TCellRenderers.Payload;
    Filters := SORTABLE_TEXT_FILTER;
  end;

  with FBlockChainGrid.AddColumn('Miner Payload') do
  begin
    Binding := 'MinerPayload';
    Width := 200;
    Renderer := TCellRenderers.Payload;
    Filters := SORTABLE_TEXT_FILTER;
  end;

  with FBlockChainGrid.AddColumn('Proof Of Work') do
  begin
    Binding := 'POW';
    Width := 200;
   // Renderer := TCellRenderers.Payload;
    Filters := SORTABLE_TEXT_FILTER;
  end;

  with FBlockChainGrid.AddColumn('SafeBox Hash') do
  begin
    Binding := 'SBH';
    Width := 200;
   // Renderer := TCellRenderers.Payload;
    Filters := SORTABLE_TEXT_FILTER;
  end;

  with FBlockChainGrid.AddColumn('Protocol') do
  begin
    Width := 100;
   // Renderer := TCellRenderers.Payload;
    Filters := SORTABLE_TEXT_FILTER;
  end;

  with FBlockChainGrid.AddColumn('Deviation') do
  begin
    Width := 100;
   // Renderer := TCellRenderers.Payload;
    Filters := SORTABLE_TEXT_FILTER;
  end;

  with FBlockChainGrid.AddColumn('Time Average') do
  begin
    AutoWidth := True;
    Binding := 'TimeAverage';
   // Renderer := TCellRenderers.Payload;
    Filters := SORTABLE_TEXT_FILTER;
  end;

  FBlockChainGrid.Caption.Alignment := taCenter;
  FBlockChainGrid.Caption.Text := 'All Blocks';
  FBlockChainGrid.Caption.Visible := True;

  // Add datasources to grid
  FBlockChainGrid.DataSource := FBlockChainDataSource;

  // Add grid to panels
  paGrid.AddControlDockCenter(FBlockChainGrid);
end;

procedure TFRMBlockExplorer.SetMaxBlocks(AValue: integer);
begin
  if FMaxBlocks = AValue then
    Exit;
  FMaxBlocks := AValue;
  if (FMaxBlocks <= 0) or (FMaxBlocks > 500) then
    FMaxBlocks := 300;
  UpdateVisualGridUI();
end;

procedure TFRMBlockExplorer.SetBlocks(AStart, AEnd: int64);
begin
  if (AStart = FBlockStart) and (AEnd = FBlockEnd) then
    Exit;
  FBlockStart := AStart;
  FBlockEnd := AEnd;
  if (FBlockEnd > 0) and (FBlockStart > FBlockEnd) then
    FBlockStart := -1;
  UpdateVisualGridUI();
end;

procedure TFRMBlockExplorer.ebBlockChainBlockExit(Sender: TObject);
var
  LStart, LEnd: int64;
begin
  if not FUpdating then
    try
      FUpdating := True;// move to finally
      LStart := StrToInt64Def(ebBlockChainBlockStart.Text, -1);
      if LStart >= 0 then
        ebBlockChainBlockStart.Text := IntToStr(LStart)
      else
        ebBlockChainBlockStart.Text := '';
      LEnd := StrToInt64Def(ebBlockChainBlockEnd.Text, -1);
      if LEnd >= 0 then
        ebBlockChainBlockEnd.Text := IntToStr(LEnd)
      else
        ebBlockChainBlockEnd.Text := '';
      SetBlocks(LStart, LEnd);
    finally
      FUpdating := False;
    end;
end;

procedure TFRMBlockExplorer.ebBlockChainBlockStartKeyPress(Sender: TObject; var Key: char);

begin
  if Key = #13 then
    ebBlockChainBlockExit(nil);
end;

end.
