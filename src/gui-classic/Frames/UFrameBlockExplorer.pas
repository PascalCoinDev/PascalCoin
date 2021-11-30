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

end.
