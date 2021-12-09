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
    FMinersBlocksFound: Integer;

    procedure SetMinersBlocksFound(const Value: Integer);

  public
    { Public declarations }

    procedure UpdateNodeStatus;
    procedure UpdateBlockChainState;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property MinersBlocksFound : Integer read FMinersBlocksFound write SetMinersBlocksFound;
  end;

implementation

{$R *.dfm}

uses
  UFRMWallet, UNetProtocol, UBlockChain, UTime, UConst;

constructor TFrameInfo.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  MinersBlocksFound := 0;

end;

destructor TFrameInfo.Destroy;
begin

  inherited Destroy;
end;

procedure TFrameInfo.SetMinersBlocksFound(const Value: Integer);
begin
  FMinersBlocksFound := Value;
  lblBlocksFound.Caption := Inttostr(Value);
  if Value>0 then lblBlocksFound.Font.Color := clGreen
  else lblBlocksFound.Font.Color := clDkGray;
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

procedure TFrameInfo.UpdateBlockChainState;
Var isMining : boolean;
  i,mc : Integer;
  s : String;
  f, favg : real;
  LLockedMempool : TPCOperationsComp;
begin
  UpdateNodeStatus;
  mc := 0;
  if Assigned(FRMWallet.Node) then begin
    if FRMWallet.Node.Bank.BlocksCount>0 then begin
      lblCurrentBlock.Caption :=  Inttostr(FRMWallet.Node.Bank.BlocksCount)+' (0..'+Inttostr(FRMWallet.Node.Bank.BlocksCount-1)+')'; ;
    end else lblCurrentBlock.Caption :=  '(none)';
    lblCurrentAccounts.Caption := Inttostr(FRMWallet.Node.Bank.AccountsCount);
    lblCurrentBlockTime.Caption := UnixTimeToLocalElapsedTime(FRMWallet.Node.Bank.LastOperationBlock.timestamp);
    LLockedMempool := FRMWallet.Node.LockMempoolRead;
    try
      lblOperationsPending.Caption := Inttostr(LLockedMempool.Count);
      lblCurrentDifficulty.Caption := InttoHex(LLockedMempool.OperationBlock.compact_target,8);
    finally
      FRMWallet.Node.UnlockMempoolRead;
    end;
    favg := FRMWallet.Node.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage);
    f := (CT_NewLineSecondsAvg - favg) / CT_NewLineSecondsAvg;
    lblTimeAverage.Caption := 'Last '+Inttostr(CT_CalcNewTargetBlocksAverage)+': '+FormatFloat('0.0',favg)+' sec. (Optimal '+Inttostr(CT_NewLineSecondsAvg)+'s) Deviation '+FormatFloat('0.00%',f*100);
    if favg>=CT_NewLineSecondsAvg then begin
      lblTimeAverage.Font.Color := clNavy;
    end else begin
      lblTimeAverage.Font.Color := clOlive;
    end;
    lblTimeAverageAux.Caption :=
    Format
    (
      'Last %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec.',
      [
        CT_CalcNewTargetBlocksAverage * 2, FormatFloat('0.0', FRMWallet.Node.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage * 2)),
        ((CT_CalcNewTargetBlocksAverage * 3) DIV 2) ,FormatFloat('0.0',FRMWallet.Node.Bank.GetActualTargetSecondsAverage((CT_CalcNewTargetBlocksAverage * 3) DIV 2)),
        ((CT_CalcNewTargetBlocksAverage DIV 4)*3),FormatFloat('0.0',FRMWallet.Node.Bank.GetActualTargetSecondsAverage(((CT_CalcNewTargetBlocksAverage DIV 4)*3))),
        CT_CalcNewTargetBlocksAverage DIV 2,FormatFloat('0.0',FRMWallet.Node.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 2)),
        CT_CalcNewTargetBlocksAverage DIV 4,FormatFloat('0.0',FRMWallet.Node.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 4))
      ]
    );
  end else begin
    isMining := false;

    lblCurrentBlock.Caption := '';
    lblCurrentAccounts.Caption := '';
    lblCurrentBlockTime.Caption := '';
    lblOperationsPending.Caption := '';
    lblCurrentDifficulty.Caption := '';
    lblTimeAverage.Caption := '';
    lblTimeAverageAux.Caption := '';
  end;
  if (Assigned(FRMWallet.PoolMiningServer)) And (FRMWallet.PoolMiningServer.Active) then begin
    If FRMWallet.PoolMiningServer.ClientsCount>0 then begin
      lblMinersClients.Caption := IntToStr(FRMWallet.PoolMiningServer.ClientsCount)+' connected JSON-RPC clients';
      lblMinersClients.Font.Color := clNavy;
    end else begin
      lblMinersClients.Caption := 'No JSON-RPC clients';
      lblMinersClients.Font.Color := clDkGray;
    end;
    MinersBlocksFound := FRMWallet.PoolMiningServer.ClientsWins;
  end else begin
    MinersBlocksFound := 0;
    lblMinersClients.Caption := 'JSON-RPC server not active';
    lblMinersClients.Font.Color := clRed;
  end;
end;

end.
