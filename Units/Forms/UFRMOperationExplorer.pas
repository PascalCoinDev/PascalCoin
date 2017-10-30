unit UFRMOperationExplorer;

{$mode delphi}

interface

uses
    LCLIntf, LCLType,
    SysUtils, Controls, Forms,
    Dialogs, ExtCtrls, StdCtrls,
    Grids, Menus, Classes, UGridUtils, UConst;

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

var
  FRMOperationExplorer: TFRMOperationExplorer = nil;

implementation

{$R *.lfm}

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
  // TODO - move ShowModalDecoder to TUserInterface
  FOperationsExplorerGrid.ShowModalDecoder(TUserInterface.WalletKeys, TUserInterface.AppParams);
end;

procedure TFRMOperationExplorer.miFindOperationByOpHashClick(Sender: TObject);
var
  FRM : TFRMPayloadDecoder;
  oph : String;
begin
  //TODO - refactor out with TUserInterface.ShowOperationInfoDialog(oph);
  oph := '';
  if Not InputQuery('Search operation by OpHash','Insert Operation Hash value (OpHash)',oph) then exit;
  FRM := TFRMPayloadDecoder.Create(Self);
  try
    FRM.Init(CT_TOperationResume_NUL, TUserInterface.WalletKeys,TUserInterface.AppParams);
    FRM.DoFind(oph);
    FRM.ShowModal;



  finally

    FRM.Free;
  end;
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
  //with FRMWallet do
    FOperationsExplorerGrid.ShowModalDecoder(TUserInterface.WalletKeys, TUserInterface.AppParams);
end;

procedure TFRMOperationExplorer.ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: Char);
begin
  if key=#13 then  ebFilterOperationsAccountExit(Nil);
end;

end.

