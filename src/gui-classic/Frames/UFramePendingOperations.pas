unit UFramePendingOperations;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Grids;

type
  TFramePendingOperations = class(TFrame)
    dgPendingOperations: TDrawGrid;
    pnlPendingOperations: TPanel;
    Label10: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

constructor TFramePendingOperations.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

end;

destructor TFramePendingOperations.Destroy;
begin

  inherited Destroy;
end;

end.
