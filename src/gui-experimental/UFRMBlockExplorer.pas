unit UFRMBlockExplorer;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/Releases/2.1.6/Units/Forms/UFRMWallet.pas

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Displays blockchain blocks with a block-range filter. Its Tools actions
  refresh the list or reset that filter. }

{ Public types in this unit

  Type              | Purpose
  ------------------+-------------------------------------------------------------------------
  TFRMBlockExplorer | Browses blocks and applies or resets a block-range filter.
}

interface

{$I ..\config.inc}

uses
  Classes, SysUtils, {$IFDEF FPC}FileUtil,{$ENDIF} Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, Menus, UCommon.UI, UGridUtils;

type

  { TFRMBlockExplorer }

  TFRMBlockExplorer = class(TApplicationForm)
    dgBlockChainExplorer: TDrawGrid;
    ebBlockChainBlockEnd: TEdit;
    ebBlockChainBlockStart: TEdit;
    ebHashRateBackBlocks: TEdit;
    cbHashRateUnits: TComboBox;
    lblHashRateBackBlocks: TLabel;
    lblHashRateUnits: TLabel;
    Label9: TLabel;
    BlockExplorerMenu: TMainMenu;
    miTools: TMenuItem;
    miRefresh: TMenuItem;
    miResetBlockFilters: TMenuItem;
    Panel2: TPanel;
    procedure ebBlockChainBlockStartExit(Sender: TObject);
    procedure ebBlockChainBlockStartKeyPress(Sender: TObject; var Key: Char);
    procedure miRefreshClick(Sender: TObject);
    procedure miResetBlockFiltersClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender:TObject);
    procedure ebHashRateBackBlocksExit(Sender: TObject);
    procedure ebHashRateBackBlocksKeyPress(Sender: TObject; var Key: Char);
    procedure cbHashRateUnitsChange(Sender: TObject);
  private
    FUpdating : boolean;
    FBlockChainGrid : TBlockChainGrid;

  public
    { public declarations }
  end;

var
  FRMBlockExplorer: TFRMBlockExplorer = nil;

implementation
uses UFRMMainForm, UUserInterface, USettings;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
procedure TFRMBlockExplorer.ebBlockChainBlockStartExit(Sender: TObject);
var bstart,bend : Int64;
begin
  If not FUpdating then
  Try
    FUpdating := True;
    bstart := StrToInt64Def(ebBlockChainBlockStart.Text,-1);
    bend := StrToInt64Def(ebBlockChainBlockEnd.Text,-1);
    FBlockChainGrid.SetBlocks(bstart,bend);
    if FBlockChainGrid.BlockStart>=0 then
      ebBlockChainBlockStart.Text := Inttostr(FBlockChainGrid.BlockStart) else ebBlockChainBlockStart.Text := '';
    if FBlockChainGrid.BlockEnd>=0 then
      ebBlockChainBlockEnd.Text := Inttostr(FBlockChainGrid.BlockEnd) else ebBlockChainBlockEnd.Text := '';
  Finally
    FUpdating := false;
  End;
end;

procedure TFRMBlockExplorer.ebBlockChainBlockStartKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then  ebBlockChainBlockStartExit(Nil);
end;

procedure TFRMBlockExplorer.miRefreshClick(Sender: TObject);
begin
  FBlockChainGrid.UpdateBlockChainGrid;
end;

procedure TFRMBlockExplorer.miResetBlockFiltersClick(Sender: TObject);
begin
  FUpdating := True;
  try
    ebBlockChainBlockStart.Clear;
    ebBlockChainBlockEnd.Clear;
    // Reset both range bounds in one request; keep the hash-rate settings.
    if (FBlockChainGrid.BlockStart < 0) and (FBlockChainGrid.BlockEnd < 0) then
      FBlockChainGrid.UpdateBlockChainGrid
    else
      FBlockChainGrid.SetBlocks(-1,-1);
  finally
    FUpdating := False;
  end;
end;

procedure TFRMBlockExplorer.FormCreate(Sender: TObject);
var
  units: Integer;
begin
  FUpdating := true;
  FBlockChainGrid := TBlockChainGrid.Create(Self);
  FBlockChainGrid.DrawGrid := dgBlockChainExplorer;
  FBlockChainGrid.Node := TUserInterface.Node;
  FBlockChainGrid.ShowTimeAverageColumns:={$IFDEF SHOW_AVERAGE_TIME_STATS}True;{$ELSE}False;{$ENDIF}
  cbHashRateUnits.Items.CommaText := 'h/s,Kh/s,Mh/s,Gh/s,Th/s,Ph/s,Eh/s';
  FBlockChainGrid.HashRateAverageBlocksCount := TSettings.HashRateAvgBlocksCount;
  units := Ord(TSettings.ShowHashRateAs);
  if (units < Ord(Low(TShowHashRateAs))) or (units > Ord(High(TShowHashRateAs))) then
    units := Ord({$IFDEF TESTNET}hr_Mega{$ELSE}hr_Tera{$ENDIF});
  FBlockChainGrid.HashRateAs := TShowHashRateAs(units);
  ebHashRateBackBlocks.Text := IntToStr(FBlockChainGrid.HashRateAverageBlocksCount);
  cbHashRateUnits.ItemIndex := units;
  FUpdating := false;
end;

procedure TFRMBlockExplorer.ebHashRateBackBlocksExit(Sender: TObject);
begin
  if FUpdating then exit;
  FUpdating := true;
  try
    FBlockChainGrid.HashRateAverageBlocksCount := StrToIntDef(ebHashRateBackBlocks.Text,
      FBlockChainGrid.HashRateAverageBlocksCount);
    ebHashRateBackBlocks.Text := IntToStr(FBlockChainGrid.HashRateAverageBlocksCount);
    TSettings.HashRateAvgBlocksCount := FBlockChainGrid.HashRateAverageBlocksCount;
  finally
    FUpdating := false;
  end;
end;

procedure TFRMBlockExplorer.ebHashRateBackBlocksKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    ebHashRateBackBlocksExit(Sender);
    Key := #0;
  end;
end;

procedure TFRMBlockExplorer.cbHashRateUnitsChange(Sender: TObject);
begin
  if FUpdating or (cbHashRateUnits.ItemIndex < Ord(Low(TShowHashRateAs))) or
    (cbHashRateUnits.ItemIndex > Ord(High(TShowHashRateAs))) then exit;
  FBlockChainGrid.HashRateAs := TShowHashRateAs(cbHashRateUnits.ItemIndex);
  TSettings.ShowHashRateAs := FBlockChainGrid.HashRateAs;
end;

procedure TFRMBlockExplorer.FormDestroy(Sender:TObject);
begin
  FreeAndNil(FBlockChainGrid);
end;

end.
