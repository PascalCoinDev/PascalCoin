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
    procedure ebFilterOperationsStartBlockExit(Sender: TObject);
    procedure ebFilterOperationsEndBlockExit(Sender: TObject);
    procedure ebFilterOperationsStartBlockKeyPress(Sender: TObject;
      var Key: Char);
    procedure ebFilterOperationsEndBlockKeyPress(Sender: TObject;
      var Key: Char);

    // shared:
    procedure ebFilterOperationsAccountExit(Sender: TObject);
    procedure ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: Char);

  private
    { Private declarations }
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  UFRMWallet;

constructor TFrameOperationsExplorer.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  ebFilterOperationsStartBlock.Text := '';
  ebFilterOperationsEndBlock.Text := '';
end;

destructor TFrameOperationsExplorer.Destroy;
begin

  inherited Destroy;
end;

procedure TFrameOperationsExplorer.ebFilterOperationsAccountExit(Sender: TObject);
Var bstart,bend : Int64;
begin
  If FRMWallet.Updating then exit;
  FRMWallet.Updating := True;
  Try
    bstart := StrToInt64Def(ebFilterOperationsStartBlock.Text,-1);
    if bstart>=0 then ebFilterOperationsStartBlock.Text := Inttostr(bstart) else ebFilterOperationsStartBlock.Text := '';
    bend := StrToInt64Def(ebFilterOperationsEndBlock.Text,-1);
    if bend>=0 then ebFilterOperationsEndBlock.Text := Inttostr(bend) else ebFilterOperationsEndBlock.Text := '';
    FRMWallet.OperationsExplorerGrid.SetBlocks(bstart,bend);
  Finally
    FRMWallet.Updating := false;
  End;
end;

procedure TFrameOperationsExplorer.ebFilterOperationsAccountKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then  ebFilterOperationsAccountExit(Nil);
end;

procedure TFrameOperationsExplorer.ebFilterOperationsStartBlockExit(
  Sender: TObject);
begin
  ebFilterOperationsAccountExit( Sender );
end;

procedure TFrameOperationsExplorer.ebFilterOperationsEndBlockExit(
  Sender: TObject);
begin
  ebFilterOperationsAccountExit( Sender );
end;

procedure TFrameOperationsExplorer.ebFilterOperationsStartBlockKeyPress(
  Sender: TObject; var Key: Char);
begin
  ebFilterOperationsAccountKeyPress( Sender, Key );
end;

procedure TFrameOperationsExplorer.ebFilterOperationsEndBlockKeyPress(
  Sender: TObject; var Key: Char);
begin
  ebFilterOperationsAccountKeyPress( Sender, Key );
end;


end.
