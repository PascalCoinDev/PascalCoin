unit UFRMBlockExplorer;

{$mode delphi}

interface

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/master/Units/Forms/UFRMWallet.pas
}


{$I ..\config.inc}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, Menus, UCommon.UI, UGridUtils;

type

  { TFRMBlockExplorer }

  TFRMBlockExplorer = class(TApplicationForm)
    dgBlockChainExplorer: TDrawGrid;
    ebBlockChainBlockEnd: TEdit;
    ebBlockChainBlockStart: TEdit;
    Label9: TLabel;
    BlockExplorerMenu: TMainMenu;
    miTools: TMenuItem;
    Panel2: TPanel;
    procedure ebBlockChainBlockStartExit(Sender: TObject);
    procedure ebBlockChainBlockStartKeyPress(Sender: TObject;
      var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender:TObject);
  private
    FUpdating : boolean;
    FBlockChainGrid : TBlockChainGrid;

  public
    { public declarations }
  end;

var
  FRMBlockExplorer: TFRMBlockExplorer = nil;

implementation
uses UFRMMainForm, UUserInterface;

{$R *.lfm}
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

procedure TFRMBlockExplorer.ebBlockChainBlockStartKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then  ebBlockChainBlockStartExit(Nil);
end;

procedure TFRMBlockExplorer.FormCreate(Sender: TObject);
begin
  FBlockChainGrid := TBlockChainGrid.Create(Self);
  FBlockChainGrid.DrawGrid := dgBlockChainExplorer;
  FBlockChainGrid.Node := TUserInterface.Node;
  FBlockChainGrid.ShowTimeAverageColumns:={$IFDEF SHOW_AVERAGE_TIME_STATS}True;{$ELSE}False;{$ENDIF}
  FUpdating := false;
end;

procedure TFRMBlockExplorer.FormDestroy(Sender:TObject);
begin
  FreeAndNil(FBlockChainGrid);
end;

end.
