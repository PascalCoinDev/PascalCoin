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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

{$I ..\config.inc}

uses
  Types,
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
    {$IFDEF FPC}LCLIntf,{$ENDIF} {$IFDEF FPC}LCLType,{$ENDIF} SysUtils, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, Grids, Menus, Classes,
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
    procedure dgOperationsExplorerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgOperationsExplorerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
    FOperationClickPending: Boolean;
    FMouseDownCell, FMouseDownPoint: TPoint;
    FMouseDownTopRow: Integer;
    function TryGetOperationCell(X, Y: Integer; out ACell: TPoint): Boolean;
  public
    { public declarations }
  end;

implementation

// Keep Lazarus-only streamed properties out of VCL form creation.
{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

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

function TFRMOperationExplorer.TryGetOperationCell(X, Y: Integer;
  out ACell: TPoint): Boolean;
begin
  Result := False;
  if (X < 0) or (Y < 0) or (X >= dgOperationsExplorer.ClientWidth) or
    (Y >= dgOperationsExplorer.ClientHeight) then
    Exit;
  dgOperationsExplorer.MouseToCell(X, Y, ACell.X, ACell.Y);
  Result := (ACell.X >= dgOperationsExplorer.FixedCols) and
    (ACell.X < dgOperationsExplorer.ColCount) and
    (ACell.Y >= dgOperationsExplorer.FixedRows) and (ACell.Y > 0) and
    (ACell.Y < dgOperationsExplorer.RowCount) and
    (ACell.Y <= FOperationsExplorerGrid.OperationsResume.Count);
end;

procedure TFRMOperationExplorer.dgOperationsExplorerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FOperationClickPending := False;
  if Button <> mbLeft then
    Exit;
  if not TryGetOperationCell(X, Y, FMouseDownCell) then
    Exit;
  FMouseDownPoint := Point(X, Y);
  FMouseDownTopRow := dgOperationsExplorer.TopRow;
  FOperationClickPending := True;
end;

procedure TFRMOperationExplorer.dgOperationsExplorerMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LWasPending: Boolean;
  LCell: TPoint;
begin
  // VCL also raises Click when wheel/keyboard navigation changes the row.
  // Only a matching left-button press/release may open a details dialog.
  LWasPending := FOperationClickPending;
  FOperationClickPending := False;
  if (Button <> mbLeft) or not LWasPending then
    Exit;
  if not TryGetOperationCell(X, Y, LCell) then
    Exit;
  if (LCell.X <> FMouseDownCell.X) or (LCell.Y <> FMouseDownCell.Y) or
    (dgOperationsExplorer.TopRow <> FMouseDownTopRow) or
    (Abs(X - FMouseDownPoint.X) > GetSystemMetrics(SM_CXDRAG)) or
    (Abs(Y - FMouseDownPoint.Y) > GetSystemMetrics(SM_CYDRAG)) then
    Exit;
  // Use the clicked data row; headers and blank space must not reuse an old selection.
  TUserInterface.ShowOperationInfoDialog(Self,
    FOperationsExplorerGrid.OperationsResume.Items[LCell.Y - 1]);
end;

procedure TFRMOperationExplorer.ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then  ebFilterOperationsAccountExit(Nil);
end;

end.

