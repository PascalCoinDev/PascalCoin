unit UFrameBlockExplorer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls,
  Vcl.ExtCtrls;

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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFrameBlockChainExplorer.ebBlockChainBlockStartExit(Sender: TObject);
var bstart,bend : Int64;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    bstart := StrToInt64Def(ebBlockChainBlockStart.Text,-1);
    bend := StrToInt64Def(ebBlockChainBlockEnd.Text,-1);
    FBlockChainGrid.SetBlocks(bstart,bend);
    if FBlockChainGrid.BlockStart>=0 then ebBlockChainBlockStart.Text := Inttostr(FBlockChainGrid.BlockStart) else ebBlockChainBlockStart.Text := '';
    if FBlockChainGrid.BlockEnd>=0 then ebBlockChainBlockEnd.Text := Inttostr(FBlockChainGrid.BlockEnd) else ebBlockChainBlockEnd.Text := '';
  Finally
    FUpdating := false;
  End;
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
  If FUpdating then exit;
  FUpdating := True;
  Try
    i := StrToIntDef(ebHashRateBackBlocks.Text,-1);
    FBlockChainGrid.HashRateAverageBlocksCount:=i;
    TSettings.HashRateAvgBlocksCount := FBlockChainGrid.HashRateAverageBlocksCount;
  Finally
    ebHashRateBackBlocks.Text := IntToStr(FBlockChainGrid.HashRateAverageBlocksCount);
    FUpdating := false;
  End;
end;

procedure TFrameBlockChainExplorer.cbHashRateUnitsClick(Sender: TObject);
begin
  If FUpdating then Exit;
  FUpdating := True;
  Try
    case cbHashRateUnits.ItemIndex of
      0 : FBlockChainGrid.HashRateAs := hr_Unit;
      1 : FBlockChainGrid.HashRateAs := hr_Kilo;
      2 : FBlockChainGrid.HashRateAs := hr_Mega;
      3 : FBlockChainGrid.HashRateAs := hr_Giga;
      4 : FBlockChainGrid.HashRateAs := hr_Tera;
      5 : FBlockChainGrid.HashRateAs := hr_Peta;
      6 : FBlockChainGrid.HashRateAs := hr_Exa;
    else FBlockChainGrid.HashRateAs := hr_Mega;
    end;
    TSettings.ShowHashRateAs := FBlockChainGrid.HashRateAs;
  Finally
    FUpdating := false;
  End;
end;


end.
