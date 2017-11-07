unit UFRMSyncronizationForm;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/master/Units/Forms/UFRMWallet.pas
}

interface

{$I ./../PascalCoin/config.inc}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, UCommonUI;

type

  { TFRMSyncronizationForm }

  TFRMSyncronizationForm = class(TApplicationForm)
    btnOpenWallet: TButton;
    Label16: TLabel;
    Label4: TLabel;
    lblCurrentDifficultyCaption2: TLabel;
    lblNetProtocolVersion: TLabel;
    lblTotalAccountsLabel: TLabel;
    Label8: TLabel;
    lblBlocksFound: TLabel;
    lblTotalAccountsValue: TLabel;
    lblTotalBlocksValue: TLabel;
    lblTotalBlocksLabel: TLabel;
    lblBlockAgeValue: TLabel;
    lblBlockAgeLabel: TLabel;
    lblBlockTargetValue: TLabel;
    lblProtocolVersion: TLabel;
    lblBlockTargetLabel: TLabel;
    lblCurrentDifficultyCaption1: TLabel;
    lblMinersClientsValue: TLabel;
    lblMiningStatusCaption: TLabel;
    lblNodeStatus: TLabel;
    lblPendingOperationsValue: TLabel;
    lblPendingOperationsLabel: TLabel;
    lblReceivedMessages: TLabel;
    lblTimeAverage: TLabel;
    lblTimeAverageAux: TLabel;
    procedure btnOpenWalletClick(Sender:TObject);
    procedure lblReceivedMessagesClick(Sender:TObject);
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

implementation

{$R *.lfm}

uses UNetProtocol,UTime,UConst, UUserInterface;

{%region Methods}

procedure TFRMSyncronizationForm.UpdateNodeStatus;
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
  lblProtocolVersion.Caption := Format('%d (%d)', [TUserInterface.Node.Bank.SafeBox.CurrentProtocol,CT_BlockChain_Protocol_Available]);
  lblNetProtocolVersion.Caption := Format('%d (%d)', [CT_NetProtocol_Version, CT_NetProtocol_Available]);
  if NOT btnOpenWallet.Enabled then begin
    lblNodeStatus.Caption := 'Please wait until finished - ' + lblNodeStatus.Caption;
  end;
end;

procedure TFRMSyncronizationForm.UpdateBlockChainState;
Var
  f, favg : real;
begin
  if not TUserInterface.Started then exit;
  UpdateNodeStatus;
  if Assigned(TUserInterface.Node) then begin
    if TUserInterface.Node.Bank.BlocksCount>0 then begin
      lblTotalBlocksValue.Caption :=  Inttostr(TUserInterface.Node.Bank.BlocksCount)+' (0..'+Inttostr(TUserInterface.Node.Bank.BlocksCount-1)+')'; ;
    end else lblTotalBlocksValue.Caption :=  '(none)';
    lblTotalAccountsValue.Caption := Inttostr(TUserInterface.Node.Bank.AccountsCount);
    lblBlockAgeValue.Caption := UnixTimeToLocalElapsedTime(TUserInterface.Node.Bank.LastOperationBlock.timestamp);
    lblPendingOperationsValue.Caption := Inttostr(TUserInterface.Node.Operations.Count);
    lblBlockTargetValue.Caption := InttoHex(TUserInterface.Node.Operations.OperationBlock.compact_target,8);
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
    lblTotalBlocksValue.Caption := '';
    lblTotalAccountsValue.Caption := '';
    lblBlockAgeValue.Caption := '';
    lblPendingOperationsValue.Caption := '';
    lblBlockTargetValue.Caption := '';
    lblTimeAverage.Caption := '';
    lblTimeAverageAux.Caption := '';
  end;
  if (Assigned(TUserInterface.PoolMiningServer)) And (TUserInterface.PoolMiningServer.Active) then begin
    If TUserInterface.PoolMiningServer.ClientsCount>0 then begin
      lblMinersClientsValue.Caption := IntToStr(TUserInterface.PoolMiningServer.ClientsCount)+' connected JSON-RPC clients';
      lblMinersClientsValue.Font.Color := clNavy;
    end else begin
      lblMinersClientsValue.Caption := 'No JSON-RPC clients';
      lblMinersClientsValue.Font.Color := clDkGray;
    end;
    MinersBlocksFound := TUserInterface.PoolMiningServer.ClientsWins;
  end else begin
    MinersBlocksFound := 0;
    lblMinersClientsValue.Caption := 'JSON-RPC server not active';
    lblMinersClientsValue.Font.Color := clRed;
  end;
end;

procedure TFRMSyncronizationForm.SetMinersBlocksFound(const Value: Integer);
begin
  FMinersBlocksFound := Value;
  lblBlocksFound.Caption := Inttostr(Value);
  if Value>0 then lblBlocksFound.Font.Color := clGreen
  else lblBlocksFound.Font.Color := clDkGray;
end;

procedure TFRMSyncronizationForm.OnFinishedLoadingDatabase;
begin
  btnOpenWallet.Enabled:=true;
  TUserInterface.ShowWallet;
end;

{%endregion}

{%region Handlers: widgets }

procedure TFRMSyncronizationForm.lblReceivedMessagesClick(Sender:TObject);
begin
  TUserInterface.ShowMessagesForm;
end;

procedure TFRMSyncronizationForm.btnOpenWalletClick(Sender:TObject);
begin
  TUserInterface.ShowWallet;
end;

{%endregion}

end.

