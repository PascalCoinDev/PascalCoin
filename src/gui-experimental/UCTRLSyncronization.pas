unit UCTRLSyncronization;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$mode delphi}

interface

{$I ..\config.inc}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, UCommon.UI, UNetProtocol, UBaseTypes;

type

  { TCTRLSyncronization }

  TCTRLSyncronization = class(TApplicationForm)
    btnBack: TSpeedButton;
    GroupBox1: TGroupBox;
    imgSplash: TImage;
    Label16: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    lblBlockAgeLabel: TLabel;
    lblBlockAgeValue: TLabel;
    lblBlocksFound: TLabel;
    lblBlockTargetLabel: TLabel;
    lblBlockTargetValue: TLabel;
    lblCurrentDifficultyCaption1: TLabel;
    lblCurrentDifficultyCaption2: TLabel;
    lblMinersClientsValue: TLabel;
    lblMiningStatusCaption: TLabel;
    lblNetProtocolVersion: TLabel;
    lblNodeStatus: TLabel;
    lblPendingOperationsLabel: TLabel;
    lblPendingOperationsValue: TLabel;
    lblProtocolVersion: TLabel;
    lblReceivedMessages: TLabel;
    lblTimeAverage: TLabel;
    lblTimeAverageAux: TLabel;
    lblTotalAccountsLabel: TLabel;
    lblTotalAccountsValue: TLabel;
    lblTotalBlocksLabel: TLabel;
    lblTotalBlocksValue: TLabel;
    paSplash: TPanel;
    paSync: TPanel;
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblReceivedMessagesClick(Sender:TObject);
  private
    FMinedBlocksCount: Integer;
    FShowSplash : boolean;
    FMessagesUnreadCount : Integer;
    procedure OnLoading(Sender: TObject; const message : AnsiString; curPos, totalCount : Int64);
    procedure OnLoaded(Sender: TObject);
    procedure SetMinedBlocksCount(const Value: Integer);
    procedure SetShowSplash(ABool : boolean);
    procedure SetMessagesNotificationText(const text : AnsiString);
    function GetMessagesNotificationText : AnsiString;
    procedure SetStatusText(AColour: TColor; AText: String);

    procedure UpdateNodeStatus;
    procedure UpdateBlockChainState;
    procedure OnAppStarted(Sender: TObject);
    procedure OnBlocksChanged(Sender: TObject);
    procedure OnUIRefreshTimer(Sender: TObject);
    procedure OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: String);
  protected
    procedure ActivateFirstTime; override;
  public
    property MinedBlocksCount : Integer read FMinedBlocksCount write SetMinedBlocksCount;
    property ShowSplash : boolean read FShowSplash write SetShowSplash;
    property MessagesNotificationText : AnsiString read GetMessagesNotificationText write SetMessagesNotificationText;
  end;

implementation

{$R *.lfm}

uses UCommon, UTime, UConst, UUserInterface, UAccounts, UNode;

procedure TCTRLSyncronization.FormCreate(Sender: TObject);
begin
  TUserInterface.AppStarted.Add(OnAppStarted);
  TUserInterface.Loading.Add(OnLoading);
  TUserInterface.BlocksChanged.Add(OnBlocksChanged);
  TUserInterface.UIRefreshTimer.Add(OnUIRefreshTimer);
  TUserInterface.NodeMessageEvent.Add(OnNodeMessageEvent);
  FMessagesUnreadCount := 0;
end;

procedure TCTRLSyncronization.FormDestroy(Sender: TObject);
begin
 TUserInterface.AppStarted.Remove(OnAppStarted);
 TUserInterface.Loading.Remove (OnLoading);
 TUserInterface.BlocksChanged.Remove(OnBlocksChanged);
 TUserInterface.UIRefreshTimer.Remove(OnUIRefreshTimer);
 TUserInterface.NodeMessageEvent.Remove(OnNodeMessageEvent);
end;

procedure TCTRLSyncronization.ActivateFirstTime;
begin
  ShowSplash := true;
end;

procedure TCTRLSyncronization.OnAppStarted(Sender: TObject);
begin
  UpdateBlockChainState;
end;

procedure TCTRLSyncronization.OnLoading(Sender : TObject; const message : AnsiString; curPos, totalCount : Int64);
var LPercent : String;
begin
    if (totalCount>0) then
      LPercent := Format('%.1f',[curPos*100/totalCount])+'%'
    else
      LPercent := '';

    SetStatusText(clGreen, message+' '+LPercent);
end;

procedure TCTRLSyncronization.OnLoaded(Sender: TObject);
begin
  btnBack.Enabled:=true;
  TUserInterface.ShowWallet;
end;

procedure TCTRLSyncronization.OnBlocksChanged(Sender: TObject);
begin
  UpdateBlockChainState;
end;

procedure TCTRLSyncronization.OnUIRefreshTimer(Sender: TObject);
begin
  UpdateBlockChainState;
  UpdateNodeStatus;
end;

procedure TCTRLSyncronization.OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: String);
begin
  inc(FMessagesUnreadCount);
  if FMessagesUnreadCount>1 then
    MessagesNotificationText := Format('You have received %d messages',[FMessagesUnreadCount])
  else
    MessagesNotificationText := 'You have received 1 message';
end;

procedure TCTRLSyncronization.SetShowSplash(ABool : boolean);
begin
  if ABool = FShowSplash then exit;
  FShowSplash := ABool;
  if FShowSplash then begin
    TUserInterface.Enabled := false;
    paSplash.Visible:= true;
    paSync.Visible := false;
  end else begin
    TUserInterface.Enabled := true;
    paSplash.Visible:= false;
    paSync.Visible := true;
  end;
end;

function TCTRLSyncronization.GetMessagesNotificationText : AnsiString;
begin
  Result := lblReceivedMessages.Caption;
end;

procedure TCTRLSyncronization.SetMessagesNotificationText(const text : AnsiString);
begin
  if (text = '') then lblReceivedMessages.Visible := false;
  lblReceivedMessages.Caption := text;
end;

procedure TCTRLSyncronization.SetStatusText(AColour: TColor; AText: String);
begin
  lblNodeStatus.Font.Color := AColour;
  lblNodeStatus.Caption := AText;
end;

procedure TCTRLSyncronization.UpdateNodeStatus;
Var status : AnsiString;
begin
  if not TUserInterface.Started then exit;

  // State text
  case TUserInterface.State of
     uisLoading: ShowSplash := false; // text set by OnLoading
     uisLoaded: SetStatusText(clGreen, 'Loaded');
     uisDiscoveringPeers: SetStatusText(clGreen, 'Discovering Peers');
     uisSyncronizingBlockchain: SetStatusText(clGreen, 'Syncronizing');
     uisActive: SetStatusText(clGreen, 'Active');
     uisIsolated: SetStatusText(clRed, 'Isolated');
     uisError: SetStatusText(clRed, Format('Error: %s', [TUserInterface.StateText]));
  end;

  // Protocol labels
  lblProtocolVersion.Caption := Format('%d (%d)', [TUserInterface.Node.Bank.SafeBox.CurrentProtocol,CT_BlockChain_Protocol_Available]);
  lblNetProtocolVersion.Caption := Format('%d (%d)', [CT_NetProtocol_Version, CT_NetProtocol_Available]);
end;

procedure TCTRLSyncronization.UpdateBlockChainState;
Var
  f, favg : real;
begin
  TUserInterface.Node.Operations.Lock;
  try
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
      MinedBlocksCount := TUserInterface.PoolMiningServer.ClientsWins;
    end else begin
      MinedBlocksCount := 0;
      lblMinersClientsValue.Caption := 'JSON-RPC server not active';
      lblMinersClientsValue.Font.Color := clRed;
    end;

  finally
    TUserInterface.Node.Operations.Unlock;
  end;
end;

procedure TCTRLSyncronization.SetMinedBlocksCount(const Value: Integer);
begin
  FMinedBlocksCount := Value;
  lblBlocksFound.Caption := Inttostr(Value);
  if Value>0 then lblBlocksFound.Font.Color := clGreen
  else lblBlocksFound.Font.Color := clDkGray;
end;

procedure TCTRLSyncronization.lblReceivedMessagesClick(Sender:TObject);
begin
  lblReceivedMessages.Visible := false;
  lblReceivedMessages.Caption := text;
  TUserInterface.ShowMessagesForm;
end;

procedure TCTRLSyncronization.btnBackClick(Sender: TObject);
begin
  TUserInterface.ShowWallet;
end;

end.

