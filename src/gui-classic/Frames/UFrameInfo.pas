unit UFrameInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TFrameInfo = class(TFrame)
    Panel1: TPanel;
    Image1: TImage;
    Panel2: TPanel;
    lblCurrentBlockCaption: TLabel;
    lblCurrentBlock: TLabel;
    lblCurrentBlockTimeCaption: TLabel;
    lblCurrentBlockTime: TLabel;
    lblOperationsPendingCaption: TLabel;
    lblOperationsPending: TLabel;
    lblMiningStatusCaption: TLabel;
    lblMinersClients: TLabel;
    lblNodeStatus: TLabel;
    Panel3: TPanel;
    lblTimeAverage: TLabel;
    lblCurrentAccounts: TLabel;
    lblTimeAverageAux: TLabel;
    lblBlocksFound: TLabel;
    Panel4: TPanel;
    lblReceivedMessages: TLabel;
    lblBuild: TLabel;
    lblCurrentDifficultyCaption: TLabel;
    lblCurrentDifficulty: TLabel;

  private
    { Private declarations }
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

constructor TFrameInfo.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );


end;

destructor TFrameInfo.Destroy;
begin

  inherited Destroy;
end;

end.
