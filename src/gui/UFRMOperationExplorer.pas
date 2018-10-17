unit UFRMOperationExplorer;

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

{$mode delphi}

interface

{$I ..\config.inc}

uses
    LCLIntf, LCLType, SysUtils, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Grids, Menus, Classes,
    UCommon.UI, UGridUtils, UConst;

type

  { TFRMOperationExplorer }

  TFRMOperationExplorer = class(TApplicationForm)
    dgOperationsExplorer: TDrawGrid;
    ebFilterOperationsEndBlock: TEdit;
    ebFilterOperationsStartBlock: TEdit;
    Label2: TLabel;
    OperationsExplorerMenu: TMainMenu;
    miDecodePayload: TMenuItem;
    miFindOperationByOpHash: TMenuItem;
    miTools: TMenuItem;
    Panel1: TPanel;
    procedure dgOperationsExplorerClick(Sender: TObject);
    procedure ebFilterOperationsAccountExit(Sender: TObject);
    procedure ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miDecodePayloadClick(Sender: TObject);
    procedure miFindOperationByOpHashClick(Sender: TObject);

  private
    { private declarations }
    FUpdating : boolean;
    FOperationsExplorerGrid : TOperationsGrid;
  public
    { public declarations }
  end;

implementation

{$r *.lfm}

uses UUserInterface, UFRMPayloadDecoder, UBlockChain;

procedure TFRMOperationExplorer.FormCreate(Sender: TObject);
begin
  FOperationsExplorerGrid := TOperationsGrid.Create(Self);
  FOperationsExplorerGrid.Node := TUserInterface.Node;
  FOperationsExplorerGrid.DrawGrid := dgOperationsExplorer;
  FOperationsExplorerGrid.AccountNumber := -1;
  FOperationsExplorerGrid.PendingOperations := False;
  FUpdating := false;
end;

procedure TFRMOperationExplorer.FormDestroy(Sender: TObject);
begin
  FOperationsExplorerGrid.Node := Nil;
  // Note: grids themselves are collected with Self (TComponent dependency)
end;

procedure TFRMOperationExplorer.miDecodePayloadClick(Sender: TObject);
begin
  TUserInterface.ShowOperationInfoDialog(Self, '');
end;

procedure TFRMOperationExplorer.miFindOperationByOpHashClick(Sender: TObject);
var
  ophash : String;
begin
  if Not InputQuery('Search operation by OpHash','Insert Operation Hash value (OpHash)',ophash)
    then exit;

  TUserInterface.ShowOperationInfoDialog(Self, ophash);
end;

procedure TFRMOperationExplorer.ebFilterOperationsAccountExit(Sender: TObject);
Var bstart,bend : Int64;
begin
  If not FUpdating then
  Try
    FUpdating := True;// move to finally
    bstart := StrToInt64Def(ebFilterOperationsStartBlock.Text,-1);
    if bstart>=0 then ebFilterOperationsStartBlock.Text := Inttostr(bstart) else ebFilterOperationsStartBlock.Text := '';
    bend := StrToInt64Def(ebFilterOperationsEndBlock.Text,-1);
    if bend>=0 then ebFilterOperationsEndBlock.Text := Inttostr(bend) else ebFilterOperationsEndBlock.Text := '';
      FOperationsExplorerGrid.SetBlocks(bstart,bend);
  Finally


    FUpdating := false;
  End;
end;

procedure TFRMOperationExplorer.dgOperationsExplorerClick(Sender: TObject);
begin
  TUserInterface.ShowOperationInfoDialog(Self, FOperationsExplorerGrid.SelectedOperation);
end;

procedure TFRMOperationExplorer.ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then  ebFilterOperationsAccountExit(Nil);
end;

end.

