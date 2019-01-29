unit UFRMPendingOperations;

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
  Classes, Forms, Grids,
  ExtCtrls, StdCtrls, Menus, UCommon.UI, UGridUtils;

type

  { TFRMPendingOperations }

  TFRMPendingOperations = class(TApplicationForm)
    dgPendingOperations: TDrawGrid;
    Label10: TLabel;
    PendingOperationsMenu: TMainMenu;
    miDecodePayLoad: TMenuItem;
    miFindOperationbyOpHash: TMenuItem;
    miTools: TMenuItem;
    pnlPendingOperations: TPanel;
    procedure dgPendingOperationsDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miDecodePayLoadClick(Sender: TObject);
    procedure miFindOperationbyOpHashClick(Sender: TObject);
  private
    { private declarations }
    FPendingOperationsGrid : TOperationsGrid;
  public
    { public declarations }
  end;

var
  FRMPendingOperations: TFRMPendingOperations = nil;

implementation

uses UUserInterface, UBlockChain;

{$R *.lfm}

{ TFRMPendingOperations }

procedure TFRMPendingOperations.FormCreate(Sender: TObject);
begin
  FPendingOperationsGrid := TOperationsGrid.Create(Self);
  FPendingOperationsGrid.Node := TUserInterface.Node;
  FPendingOperationsGrid.DrawGrid := dgPendingOperations;
  FPendingOperationsGrid.AccountNumber := -1; // all
  FPendingOperationsGrid.PendingOperations := true;
end;

procedure TFRMPendingOperations.FormDestroy(Sender: TObject);
begin
   FPendingOperationsGrid.Node := Nil;
   // Note: grids themselves are collected wisth Self (TComponent dependency)
end;

procedure TFRMPendingOperations.miDecodePayLoadClick(Sender: TObject);
begin
  TUserInterface.ShowOperationInfoDialog(self, FPendingOperationsGrid.SelectedOperation);
end;

procedure TFRMPendingOperations.miFindOperationbyOpHashClick(Sender: TObject);
var ophash:AnsiString;
begin
  if Not TUserInterface.AskEnterString(Self, 'Search operation by OpHash','Insert Operation Hash value (OpHash)',ophash)
    then exit;

  TUserInterface.ShowOperationInfoDialog(Self, ophash);
end;

procedure TFRMPendingOperations.dgPendingOperationsDblClick(Sender: TObject);
begin
  TUserInterface.ShowOperationInfoDialog(Self, TPCOperation.OperationHashAsHexa(FPendingOperationsGrid.SelectedOperation.OperationHash));
end;

end.

