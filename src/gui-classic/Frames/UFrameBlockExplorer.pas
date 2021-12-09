unit UFrameBlockExplorer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls,
  Vcl.ExtCtrls,
  USettings;

type
  TFrameBlockChainExplorer = class(TFrame)
    Panel2: TPanel;
    Label9: TLabel;
    lblHashRateBackBlocks: TLabel;
    lblHashRateBackBlocks1: TLabel;
    ebBlockChainBlockStart: TEdit;
    ebBlockChainBlockEnd: TEdit;
    ebHashRateBackBlocks: TEdit;
    cbHashRateUnits: TComboBox;
    dgBlockChainExplorer: TDrawGrid;
    procedure cbHashRateUnitsClick(Sender: TObject);
    procedure ebHashRateBackBlocksExit(Sender: TObject);
    procedure ebHashRateBackBlocksKeyPress(Sender: TObject; var Key: char);
    procedure ebBlockChainBlockStartExit(Sender: TObject);
    procedure ebBlockChainBlockStartKeyPress(Sender: TObject; var Key: Char);

  private
    { Private declarations }
  public
    { Public declarations }
    procedure ChangeHashRateUnits( ParaHashRateAs : TShowHashRateAs );

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  UFRMWallet;

constructor TFrameBlockChainExplorer.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  cbHashRateUnits.Items.Clear;
  cbHashRateUnits.Items.Add('h/s');
  cbHashRateUnits.Items.Add('Kh/s');
  cbHashRateUnits.Items.Add('Mh/s');
  cbHashRateUnits.Items.Add('Gh/s');
  cbHashRateUnits.Items.Add('Th/s');
  cbHashRateUnits.Items.Add('Ph/s');
  cbHashRateUnits.Items.Add('Eh/s');

end;

destructor TFrameBlockChainExplorer.Destroy;
begin

  inherited Destroy;
end;

procedure TFrameBlockChainExplorer.ebBlockChainBlockStartExit(Sender: TObject);
var bstart,bend : Int64;
begin
  with FRMWallet do
  begin

  If Updating then exit;
  Updating := True;
  Try
    bstart := StrToInt64Def(ebBlockChainBlockStart.Text,-1);
    bend := StrToInt64Def(ebBlockChainBlockEnd.Text,-1);
    BlockChainGrid.SetBlocks(bstart,bend);
    if BlockChainGrid.BlockStart>=0 then ebBlockChainBlockStart.Text := Inttostr(BlockChainGrid.BlockStart) else ebBlockChainBlockStart.Text := '';
    if BlockChainGrid.BlockEnd>=0 then ebBlockChainBlockEnd.Text := Inttostr(BlockChainGrid.BlockEnd) else ebBlockChainBlockEnd.Text := '';
  Finally
    Updating := false;
  End;

  end;
end;

procedure TFrameBlockChainExplorer.ebBlockChainBlockStartKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then  ebBlockChainBlockStartExit(Nil);
end;

procedure TFrameBlockChainExplorer.ebHashRateBackBlocksKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then ebHashRateBackBlocksExit(Nil);
end;

procedure TFrameBlockChainExplorer.ebHashRateBackBlocksExit(Sender: TObject);
var i : Integer;
begin
  with FRMWallet do
  begin

  If Updating then exit;
  Updating := True;
  Try
    i := StrToIntDef(ebHashRateBackBlocks.Text,-1);
    BlockChainGrid.HashRateAverageBlocksCount:=i;
    TSettings.HashRateAvgBlocksCount := BlockChainGrid.HashRateAverageBlocksCount;
  Finally
    ebHashRateBackBlocks.Text := IntToStr(BlockChainGrid.HashRateAverageBlocksCount);
    Updating := false;
  End;

  end;
end;

procedure TFrameBlockChainExplorer.cbHashRateUnitsClick(Sender: TObject);
begin
  with FRMWallet do
  begin

  If Updating then Exit;
  Updating := True;
  Try
    case cbHashRateUnits.ItemIndex of
      0 : BlockChainGrid.HashRateAs := hr_Unit;
      1 : BlockChainGrid.HashRateAs := hr_Kilo;
      2 : BlockChainGrid.HashRateAs := hr_Mega;
      3 : BlockChainGrid.HashRateAs := hr_Giga;
      4 : BlockChainGrid.HashRateAs := hr_Tera;
      5 : BlockChainGrid.HashRateAs := hr_Peta;
      6 : BlockChainGrid.HashRateAs := hr_Exa;
    else BlockChainGrid.HashRateAs := hr_Mega;
    end;
    TSettings.ShowHashRateAs := BlockChainGrid.HashRateAs;
  Finally
    Updating := false;
  End;

  end;
end;

procedure TFrameBlockChainExplorer.ChangeHashRateUnits( ParaHashRateAs : TShowHashRateAs );
begin
  Case ParaHashRateAs of
    hr_Unit : cbHashRateUnits.ItemIndex:=0;
    hr_Kilo : cbHashRateUnits.ItemIndex:=1;
    hr_Mega : cbHashRateUnits.ItemIndex:=2;
    hr_Giga : cbHashRateUnits.ItemIndex:=3;
    hr_Tera : cbHashRateUnits.ItemIndex:=4;
    hr_Peta : cbHashRateUnits.ItemIndex:=5;
    hr_Exa : cbHashRateUnits.ItemIndex:=6;
  else
    cbHashRateUnits.ItemIndex:=-1;
  end;
end;


end.
