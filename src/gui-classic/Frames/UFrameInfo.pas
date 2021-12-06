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
    Splitter1: TSplitter;
    Panel2: TPanel;
    lblCurrentBlockCaption: TLabel;
    lblCurrentBlock: TLabel;
    lblCurrentBlockTimeCaption: TLabel;
    lblCurrentBlockTime: TLabel;
    lblOperationsPendingCaption: TLabel;
    lblOperationsPending: TLabel;
    lblMiningStatusCaption: TLabel;
    lblMinersClients: TLabel;
    lblNodeCaption: TLabel;
    lblNodeStatus: TLabel;
    Splitter2: TSplitter;
    Panel3: TPanel;
    lblTimeAverage: TLabel;
    lblTimeAverageCaption: TLabel;
    lblCurrentAccountsCaption: TLabel;
    lblCurrentAccounts: TLabel;
    lblTimeAverageAux: TLabel;
    lblBlocksFoundCaption: TLabel;
    lblBlocksFound: TLabel;
    lblCurrentDifficultyCaption: TLabel;
    lblCurrentDifficulty: TLabel;
    Splitter3: TSplitter;
    Panel4: TPanel;
    lblReceivedMessages: TLabel;
    lblBuild: TLabel;

  private
    { Private declarations }
  public
    { Public declarations }

    procedure UpdateNodeStatus;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  UFRMWallet, UNetProtocol;

constructor TFrameInfo.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );


end;

destructor TFrameInfo.Destroy;
begin

  inherited Destroy;
end;

procedure TFrameInfo.UpdateNodeStatus;
Var status : String;
begin
  If Not Assigned(FRMWallet.Node) then begin
    lblNodeStatus.Font.Color := clRed;
    lblNodeStatus.Caption := 'Initializing...';
  end else begin
    If FRMWallet.Node.IsReady(status) then begin
      if TNetData.NetData.NetStatistics.ActiveConnections>0 then begin
        lblNodeStatus.Font.Color := clGreen;
        if TNetData.NetData.IsDiscoveringServers then begin
          lblNodeStatus.Caption := 'Discovering servers';
        end else if TNetData.NetData.IsGettingNewBlockChainFromClient(status) then begin
          lblNodeStatus.Caption := 'Obtaining new blockchain '+status;
        end else begin
          lblNodeStatus.Caption := 'Running';
        end;
      end else begin
        lblNodeStatus.Font.Color := clRed;
        lblNodeStatus.Caption := 'Alone in the world...';
      end;
    end else begin
      lblNodeStatus.Font.Color := clRed;
      lblNodeStatus.Caption := status;
    end;
  end;

  If Assigned(FRMWallet.BackgroundLabel) then begin
    FRMWallet.BackgroundLabel.Font.Color:= lblNodeStatus.Font.Color;
    FRMWallet.BackgroundLabel.Caption:='Please wait until finished: '+lblNodeStatus.Caption;
  end;
end;

end.
