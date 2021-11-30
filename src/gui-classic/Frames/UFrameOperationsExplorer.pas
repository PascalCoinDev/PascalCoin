unit UFrameOperationsExplorer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TFrameOperationsExplorer = class(TFrame)
    Panel1: TPanel;
    Label2: TLabel;
    ebFilterOperationsStartBlock: TEdit;
    ebFilterOperationsEndBlock: TEdit;
    dgOperationsExplorer: TDrawGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFrameOperationsExplorer.ebFilterAccountByBalanceMinExit(Sender: TObject);
begin
  DoUpdateAccountsFilter;
end;

procedure TFrameOperationsExplorer.ebFilterAccountByBalanceMinKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then DoUpdateAccountsFilter;
end;

procedure TFrameOperationsExplorer.ebFilterOperationsAccountExit(Sender: TObject);
Var bstart,bend : Int64;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    bstart := StrToInt64Def(ebFilterOperationsStartBlock.Text,-1);
    if bstart>=0 then ebFilterOperationsStartBlock.Text := Inttostr(bstart) else ebFilterOperationsStartBlock.Text := '';
    bend := StrToInt64Def(ebFilterOperationsEndBlock.Text,-1);
    if bend>=0 then ebFilterOperationsEndBlock.Text := Inttostr(bend) else ebFilterOperationsEndBlock.Text := '';
    FOperationsExplorerGrid.SetBlocks(bstart,bend);
  Finally
    FUpdating := false;
  End;
end;

procedure TFrameOperationsExplorer.ebFilterOperationsAccountKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then  ebFilterOperationsAccountExit(Nil);
end;


end.
