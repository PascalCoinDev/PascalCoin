unit UFRMSyncronizationDialog;

{$mode delphi}

interface

{$I ./../PascalCoin/config.inc}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, UCommonUI;

type

  { TFRMSyncronizationDialog }

  TFRMSyncronizationDialog = class(TApplicationForm)
    btnOpenWallet: TButton;
    Label16: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    lblBlocksFound: TLabel;
    lblBuild: TLabel;
    lblCurrentAccounts: TLabel;
    lblCurrentBlock: TLabel;
    lblCurrentBlockCaption: TLabel;
    lblCurrentBlockTime: TLabel;
    lblCurrentBlockTimeCaption: TLabel;
    lblCurrentDifficulty: TLabel;
    lblCurrentDifficultyCaption: TLabel;
    lblMinersClients: TLabel;
    lblMiningStatusCaption: TLabel;
    lblNodeStatus: TLabel;
    lblOperationsPending: TLabel;
    lblOperationsPendingCaption: TLabel;
    lblReceivedMessages: TLabel;
    lblTimeAverage: TLabel;
    lblTimeAverageAux: TLabel;
    pnlTop: TPanel;
    tcInfo: TTabControl;
    procedure btnOpenWalletClick(Sender:TObject);
    procedure Image1Click(Sender: TObject);
    procedure lblReceivedMessagesClick(Sender:TObject);
    procedure tcInfoChange(Sender: TObject);
  private
    { private declarations }
    FMinersBlocksFound: Integer;
  public
    { public declarations }
    procedure UpdateNodeStatus;
    procedure UpdateBlockChainState;
    procedure SetMinersBlocksFound(const Value: Integer);
    procedure OnFinishedLoadingDatabase;
    Property MinersBlocksFound : Integer read FMinersBlocksFound write SetMinersBlocksFound;
  end;

var
  //HS manage in TUserInterface
  FRMSyncronizationDialogIsFirstOpen:boolean =true;  //TODO u

implementation

{$R *.lfm}

uses UNetProtocol,UTime,UConst, UUserInterface;


{%region Methods}

procedure TFRMSyncronizationDialog.UpdateNodeStatus;
Var status : AnsiString;
begin
  if not TUserInterface.Started then exit;
  If Not Assigned(TUserInterface.Node) then begin
    lblNodeStatus.Font.Color := clRed;
    lblNodeStatus.Caption := 'Initializing...';
  end else begin
    If TUserInterface.Node.IsReady(status) then begin
      if TNetData.NetData.NetStatistics.ActiveConnections>0 then begin
        lblNodeStatus.Font.Color := clGreen;
        if TNetData.NetData.IsDiscoveringServers then begin
          lblNodeStatus.Caption := 'Discovering servers';
        end else if TNetData.NetData.IsGettingNewBlockChainFromClient then begin
          lblNodeStatus.Caption := 'Obtaining new blockchain';
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

// this rewrite   -- TODO
//  If Assigned(FBackgroundPanel) then begin
//    FBackgroundPanel.Font.Color:=lblNodeStatus.Font.Color;
//    FBackgroundPanel.Caption:='Please wait until finished: '+lblNodeStatus.Caption;
  //end;
end;

procedure TFRMSyncronizationDialog.UpdateBlockChainState;
Var
  f, favg : real;
begin
  if not TUserInterface.Started then exit;
  UpdateNodeStatus;
  if Assigned(TUserInterface.Node) then begin
    if TUserInterface.Node.Bank.BlocksCount>0 then begin
      lblCurrentBlock.Caption :=  Inttostr(TUserInterface.Node.Bank.BlocksCount)+' (0..'+Inttostr(TUserInterface.Node.Bank.BlocksCount-1)+')'; ;
    end else lblCurrentBlock.Caption :=  '(none)';
    lblCurrentAccounts.Caption := Inttostr(TUserInterface.Node.Bank.AccountsCount);
    lblCurrentBlockTime.Caption := UnixTimeToLocalElapsedTime(TUserInterface.Node.Bank.LastOperationBlock.timestamp);
    lblOperationsPending.Caption := Inttostr(TUserInterface.Node.Operations.Count);
    lblCurrentDifficulty.Caption := InttoHex(TUserInterface.Node.Operations.OperationBlock.compact_target,8);
    favg := TUserInterface.Node.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage);
    f := (CT_NewLineSecondsAvg - favg) / CT_NewLineSecondsAvg;
    lblTimeAverage.Caption := 'Last '+Inttostr(CT_CalcNewTargetBlocksAverage)+': '+FormatFloat('0.0',favg)+' sec. (Optimal '+Inttostr(CT_NewLineSecondsAvg)+'s) Deviation '+FormatFloat('0.00%',f*100);
    if favg>=CT_NewLineSecondsAvg then begin
      lblTimeAverage.Font.Color := clNavy;
    end else begin
      lblTimeAverage.Font.Color := clOlive;
    end;
    lblTimeAverageAux.Caption := Format('Last %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec.',[
        CT_CalcNewTargetBlocksAverage * 2 ,FormatFloat('0.0',TUserInterface.Node.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage * 2)),
        ((CT_CalcNewTargetBlocksAverage * 3) DIV 2) ,FormatFloat('0.0',TUserInterface.Node.Bank.GetActualTargetSecondsAverage((CT_CalcNewTargetBlocksAverage * 3) DIV 2)),
        ((CT_CalcNewTargetBlocksAverage DIV 4)*3),FormatFloat('0.0',TUserInterface.Node.Bank.GetActualTargetSecondsAverage(((CT_CalcNewTargetBlocksAverage DIV 4)*3))),
        CT_CalcNewTargetBlocksAverage DIV 2,FormatFloat('0.0',TUserInterface.Node.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 2)),
        CT_CalcNewTargetBlocksAverage DIV 4,FormatFloat('0.0',TUserInterface.Node.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 4))]);
  end else begin
    lblCurrentBlock.Caption := '';
    lblCurrentAccounts.Caption := '';
    lblCurrentBlockTime.Caption := '';
    lblOperationsPending.Caption := '';
    lblCurrentDifficulty.Caption := '';
    lblTimeAverage.Caption := '';
    lblTimeAverageAux.Caption := '';
  end;
  if (Assigned(TUserInterface.PoolMiningServer)) And (TUserInterface.PoolMiningServer.Active) then begin
    If TUserInterface.PoolMiningServer.ClientsCount>0 then begin
      lblMinersClients.Caption := IntToStr(TUserInterface.PoolMiningServer.ClientsCount)+' connected JSON-RPC clients';
      lblMinersClients.Font.Color := clNavy;
    end else begin
      lblMinersClients.Caption := 'No JSON-RPC clients';
      lblMinersClients.Font.Color := clDkGray;
    end;
    MinersBlocksFound := TUserInterface.PoolMiningServer.ClientsWins;
  end else begin
    MinersBlocksFound := 0;
    lblMinersClients.Caption := 'JSON-RPC server not active';
    lblMinersClients.Font.Color := clRed;
  end;
  lblBuild.Caption := 'Build: '+CT_ClientAppVersion;
end;

procedure TFRMSyncronizationDialog.SetMinersBlocksFound(const Value: Integer);
begin
  FMinersBlocksFound := Value;
  lblBlocksFound.Caption := Inttostr(Value);
  if Value>0 then lblBlocksFound.Font.Color := clGreen
  else lblBlocksFound.Font.Color := clDkGray;
end;

procedure TFRMSyncronizationDialog.OnFinishedLoadingDatabase;
begin
  btnOpenWallet.Enabled:=true;
end;

{%endregion}

{%region Handlers: widgets }

procedure TFRMSyncronizationDialog.lblReceivedMessagesClick(Sender:TObject);
begin
  TUserInterface.ShowMessagesForm;
end;

procedure TFRMSyncronizationDialog.tcInfoChange(Sender: TObject);
begin

end;

procedure TFRMSyncronizationDialog.btnOpenWalletClick(Sender:TObject);
begin
  TUserInterface.ShowWallet;
end;

procedure TFRMSyncronizationDialog.Image1Click(Sender: TObject);
begin

end;

{%endregion}

end.

