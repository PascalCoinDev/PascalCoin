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

    // skybuck: missing ??
 //   procedure ebFilterOperationsAccountExit(Sender: TObject);
 ///   procedure ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: Char);

  private
    { Private declarations }
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

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



end.
