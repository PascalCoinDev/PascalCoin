unit UFRMWallet;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

{$I ../config.inc}

uses
{$IFnDEF FPC}
  pngimage, Windows, AppEvnts, ShlObj,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, UWallet, StdCtrls, ULog, Grids, UAppParams, UBlockChain,
  UNode, UGridUtils, UJSONFunctions, UAccounts, Menus, ImgList, UNetProtocol,
  UCrypto, Buttons, UPoolMining, URPC, UFRMAccountSelect, UConst,
  UAccountKeyStorage, UBaseTypes, UPCDataTypes, UOrderedList,
  UFRMRPCCalls, UTxMultiOperation, USettings, UEPasa,
  UFrameAccountExplorer,
  UFramePendingOperations,
  UFrameMessages,
  UFrameNodeStats,
  UFrameLogs,
  UFrameOperationsExplorer,
  UFrameBlockExplorer,
  UFrameInfo,
  {$IFNDEF FPC}
  System.Generics.Collections
  {$ELSE}
  Generics.Collections
  {$ENDIF};

Const
  CM_PC_WalletKeysChanged = WM_USER + 1;
  CM_PC_NetConnectionUpdated = WM_USER + 2;

type

  { TFRMWallet }

  TFRMWallet = class(TForm)
    MiRPCCalls: TMenuItem;
    pnlTop: TPanel;
    StatusBar: TStatusBar;
    PageControl: TPageControl;
    tsMyAccounts: TTabSheet;
    tsOperations: TTabSheet;
    TimerUpdateStatus: TTimer;
    tsLogs: TTabSheet;
    MainMenu: TMainMenu;
    miProject: TMenuItem;
    miOptions: TMenuItem;
    miPrivatekeys: TMenuItem;
    miN1: TMenuItem;
    miAbout: TMenuItem;
    miAboutPascalCoin: TMenuItem;
    miNewOperation: TMenuItem;
    tsNodeStats: TTabSheet;
    tsBlockChain: TTabSheet;
    tsPendingOperations: TTabSheet;
    N1: TMenuItem;
    MiClose: TMenuItem;
    MiDecodePayload: TMenuItem;
    tsMessages: TTabSheet;
    IPnodes1: TMenuItem;
    MiOperations: TMenuItem;
    MiAddaccounttoSelected: TMenuItem;
    MiRemoveaccountfromselected: TMenuItem;
    N2: TMenuItem;
    MiMultiaccountoperation: TMenuItem;
    N3: TMenuItem;
    MiFindnextaccountwithhighbalance: TMenuItem;
    MiFindpreviousaccountwithhighbalance: TMenuItem;
    MiFindaccount: TMenuItem;
    MiFindOperationbyOpHash: TMenuItem;
    MiAccountInformation: TMenuItem;
    MiOperationsExplorer: TMenuItem;
    FrameAccountExplorer: TFrameAccountExplorer;
    FramePendingOperations: TFramePendingOperations;
    FrameBlockChainExplorer: TFrameBlockChainExplorer;
    FrameOperationsExplorer: TFrameOperationsExplorer;
    FrameMessages: TFrameMessages;
    FrameInfo: TFrameInfo;
    FrameNodeStats: TFrameNodeStats;
    FrameLogs: TFrameLogs;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure MiOperationsExplorerClick(Sender: TObject);
    procedure MiRPCCallsClick(Sender: TObject);
    procedure TimerUpdateStatusTimer(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miAboutPascalCoinClick(Sender: TObject);
    procedure miNewOperationClick(Sender: TObject);
    procedure miPrivatekeysClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure MiCloseClick(Sender: TObject);
    procedure MiDecodePayloadClick(Sender: TObject);
    procedure lblReceivedMessagesClick(Sender: TObject);
    procedure IPnodes1Click(Sender: TObject);
    procedure MiAddaccounttoSelectedClick(Sender: TObject);
    procedure MiRemoveaccountfromselectedClick(Sender: TObject);
    procedure MiMultiaccountoperationClick(Sender: TObject);
    procedure MiFindnextaccountwithhighbalanceClick(Sender: TObject);
    procedure MiFindpreviousaccountwithhighbalanceClick(Sender: TObject);
    procedure MiFindaccountClick(Sender: TObject);
    procedure MiFindOperationbyOpHashClick(Sender: TObject);
    procedure MiAccountInformationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miAskForAccountClick(Sender: TObject);

  private
    FLastNodesCacheUpdatedTS : TDateTime;
    FBackgroundPanel : TPanel;
    FBackgroundLabel : TLabel;
    FMinersBlocksFound: Integer;
    procedure SetMinersBlocksFound(const Value: Integer);
    Procedure FinishedLoadingApp;
    Procedure FillAccountInformation(Const Strings : TStrings; Const AccountNumber : Cardinal);
    Procedure FillOperationInformation(Const Strings : TStrings; Const OperationResume : TOperationResume);
    Procedure InitMacOSMenu;
  protected
    { Private declarations }
    FNode : TNode;
    FIsActivated : Boolean;
    FWalletKeys : TWalletKeysExt;
    FLog : TLog;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FSelectedAccountsGrid : TAccountsGrid;
    FOperationsAccountGrid : TOperationsGrid;
    FPendingOperationsGrid : TOperationsGrid;
    FOperationsExplorerGrid : TOperationsGrid;
    FBlockChainGrid : TBlockChainGrid;
    FMinerPrivateKeyType : TMinerPrivateKeyType;
    FUpdating : Boolean;
    FMessagesUnreadCount : Integer;
    FPoolMiningServer : TPoolMiningServer;
    FRPCServer : TRPCServer;
    FMustProcessWalletChanged : Boolean;
    FMustProcessNetConnectionUpdated : Boolean;
    FThreadActivate : TObject;

    Procedure OnNewAccount(Sender : TObject);
    Procedure OnReceivedHelloMessage(Sender : TObject);
    Procedure OnNetStatisticsChanged(Sender : TObject);
    procedure OnNewLog(logtype : TLogType; Time : TDateTime; ThreadID : TThreadID; Const sender, logtext : String);
    procedure OnWalletChanged(Sender : TObject);
    procedure OnNetConnectionsUpdated(Sender : TObject);
    procedure OnNetNodeServersUpdated(Sender : TObject);
    Procedure OnNodeMessageEvent(NetConnection : TNetConnection; MessageData : String);
    Procedure OnNodeKeysActivity(Sender : TObject);
    Procedure OnSelectedAccountsGridUpdated(Sender : TObject);
    Procedure OnMiningServerNewBlockFound(Sender : TObject);
    Procedure UpdateConnectionStatus;
    Procedure UpdateBlockChainState;
    Procedure UpdateConfigChanged(Sender:TObject);
    Procedure UpdateAvailableConnections;
    procedure Activate; override;
    Function ForceMining : Boolean; virtual;
    Function GetAccountKeyForMiner : TAccountKey;
    Procedure DoUpdateAccounts;
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
    procedure CM_NetConnectionUpdated(var Msg: TMessage); message CM_PC_NetConnectionUpdated;
  public
    Procedure CheckIsReady;

    Procedure UpdateOperations;

    Property BackgroundLabel : TLabel read FBackgroundLabel;

    { Public declarations }
    Property Node : TNode read FNode;

    Property WalletKeys : TWalletKeysExt read FWalletKeys;
    Property MinersBlocksFound : Integer read FMinersBlocksFound write SetMinersBlocksFound;

    Property NodeNotifyEvents : TNodeNotifyEvents read FNodeNotifyEvents;

    Property Updating : boolean read FUpdating write FUpdating;

    property OperationsExplorerGrid : TOperationsGrid read FOperationsExplorerGrid;
    Property BlockChainGrid : TBlockChainGrid read FBlockChainGrid;

    Property SelectedAccountsGrid : TAccountsGrid read FSelectedAccountsGrid;



  end;

var
  FRMWallet: TFRMWallet;

implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

Uses UFolderHelper,{$IFDEF USE_GNUGETTEXT}gnugettext,{$ENDIF}
{$IFDEF Use_OpenSSL}
  UOpenSSL,
{$ENDIF}
  UTime, UFileStorage,
  UThread, UOpTransaction, UFRMWalletConfig,
  UFRMOperationsExplorer,
  {$IFDEF TESTNET}
  UFRMRandomOperations,
  UFRMDiagnosticTool,
  {$ENDIF}
  UPCTNetDataExtraMessages,
  UFRMAskForAccount,
  UAbstractBTree, UEPasaDecoder,
  UFRMAbout, UFRMOperation, UFRMWalletKeys, UFRMPayloadDecoder, UFRMNodesIp, UFRMMemoText,
  UCommon, UPCOrderedLists;

Type

  { TThreadActivate }

  TThreadActivate = Class(TPCThread)
  private
    FLastTC : TTickCount;
    FLastMsg : String;
    procedure OnProgressNotify(sender : TObject; const mesage : String; curPos, totalCount : Int64);
    procedure ThreadSafeNotify;
  protected
    procedure BCExecute; override;
  End;

{ TThreadActivate }

procedure TThreadActivate.OnProgressNotify(sender: TObject; const mesage: String; curPos, totalCount: Int64);
var pct : String;
begin
  If TPlatform.GetElapsedMilliseconds(FLastTC)>500 then begin
    FLastTC := TPlatform.GetTickCount;
    if (totalCount>0) then pct := Format('%.2f',[curPos*100/totalCount])+'%'
    else pct := '';
    FLastMsg:='Please wait until finished: '+mesage+' '+pct;
    Synchronize(ThreadSafeNotify);
  end;
end;

procedure TThreadActivate.ThreadSafeNotify;
begin
  If (FLastMsg<>'') then begin
    if Assigned(FRMWallet.FBackgroundLabel) then begin
      FRMWallet.FBackgroundLabel.Caption:=FLastMsg;
    end;
  end else FRMWallet.FrameInfo.UpdateNodeStatus;
end;

procedure TThreadActivate.BCExecute;
Var currentProcess : String;
  LTC : TTickCount;
  LRaw : TRawBytes;
begin
  FLastTC := 0;
  FLastMsg := '';
  //
  {$IFDEF PRODUCTION}
  OnProgressNotify(Self,'Reading Hardcoded RandomHash file',0,0);
  LRaw := TCrypto.HexaToRaw(CT_Hardcoded_RandomHash_Table_HASH);
  TPascalCoinProtocol.AllowUseHardcodedRandomHashTable(
    ExtractFileDir(Application.ExeName)+PathDelim+CT_Hardcoded_RandomHash_Table_Filename,
    LRaw );
  {$ENDIF}
  FLastTC := 0;
  OnProgressNotify(Self,'Initializing databases',0,0);
  // Read Operations saved from disk
  TNode.Node.InitSafeboxAndOperations($FFFFFFFF,OnProgressNotify); // New Build 2.1.4 to load pending operations buffer
  TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  TNode.Node.NetServer.Active := true;
  FLastTC := 0;
  FLastMsg := '';
  LTC := TPlatform.GetTickCount;
  if (TNode.Node.Bank.BlocksCount<=1) then begin
    while (Not Terminated) And (Not TNode.Node.IsReady(currentProcess) Or (TNode.Node.Bank.BlocksCount<=1)) do begin
      Synchronize(ThreadSafeNotify);
      Sleep(200);
      {$IFDEF TESTNET}
      if (TPlatform.GetElapsedMilliseconds(LTC)>5000) then Break;
      {$ENDIF}
    end;
  end;
  if Not Terminated then begin
    Synchronize( FRMWallet.DoUpdateAccounts );
    Synchronize( FRMWallet.FinishedLoadingApp );
  end;
  FRMWallet.FThreadActivate := Nil;
end;

{ TFRMWallet }

procedure TFRMWallet.lblReceivedMessagesClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMessages;
end;


procedure TFRMWallet.Activate;
Var ips : AnsiString;
  nsarr : TNodeServerAddressArray;
  i : Integer;
begin
  inherited;
  if FIsActivated then exit;
  FIsActivated := true;
  try
    TCrypto.InitCrypto;
    // Read Wallet
    Try
      FWalletKeys.WalletFileName := TNode.GetPascalCoinDataFolder+PathDelim+'WalletKeys.dat';
    Except
      On E:Exception do begin
        E.Message := 'Cannot open your wallet... Perhaps another instance of Pascal Coin is active!'+#10+#10+E.Message;
        Raise;
      end;
    End;
    ips := TSettings.TryConnectOnlyWithThisFixedServers;
    TNode.DecodeIpStringToNodeServerAddressArray(ips,nsarr);
    TNetData.NetData.DiscoverFixedServersOnly(nsarr);
    setlength(nsarr,0);
    // Creating Node:
    FNode := TNode.Node;
    FNode.NetServer.Port := TSettings.InternetServerPort;
    FNode.PeerCache := TSettings.PeerCache+';'+CT_Discover_IPs;
    FNode.MaxPayToKeyPurchasePrice := TSettings.MaxPayToKeyPurchasePrice;
    // Create RPC server
    FRPCServer := TRPCServer.Create;
    FRPCServer.WalletKeys := WalletKeys;
    FRPCServer.Active := TSettings.JsonRpcPortEnabled;
    FRPCServer.ValidIPs := TSettings.JsonRpcAllowedIPs;
    WalletKeys.SafeBox := FNode.Bank.SafeBox;
    // Check Database
    FNode.Bank.StorageClass := TFileStorage;
    TFileStorage(FNode.Bank.Storage).DatabaseFolder := TNode.GetPascalCoinDataFolder+PathDelim+'Data';
    TFileStorage(FNode.Bank.Storage).Initialize;
    // Init Grid
    FSelectedAccountsGrid.Node := FNode;
    FWalletKeys.OnChanged.Add( OnWalletChanged );
    FrameAccountExplorer.AccountsGrid.Node := FNode;
    FOperationsAccountGrid.Node := FNode;
    FBlockChainGrid.HashRateAverageBlocksCount := TSettings.HashRateAvgBlocksCount;
    i := Integer(TSettings.ShowHashRateAs);
    if (i<Integer(Low(TShowHashRateAs))) Or (i>Integer(High(TShowHashRateAs))) then i := Integer({$IFDEF TESTNET}hr_Mega{$ELSE}hr_Tera{$ENDIF});
    FBlockChainGrid.HashRateAs := TShowHashRateAs(i);
    // Reading database
    FThreadActivate := TThreadActivate.Create(true);
    TThreadActivate(FThreadActivate).FreeOnTerminate := true;
    TThreadActivate(FThreadActivate).Suspended := False;
    UpdateConfigChanged(Self);
    FrameInfo.UpdateNodeStatus;
    TPCTNetDataExtraMessages.InitNetDataExtraMessages(FNode,TNetData.NetData,FWalletKeys);
  Except
    On E:Exception do begin
      E.Message := 'An error occurred during initialization. Application cannot continue:'+#10+#10+E.Message+#10+#10+'Application will close...';
      Application.MessageBox(PChar(E.Message),PChar(Application.Title),MB_ICONERROR+MB_OK);
      Halt;
    end;
  end;
  FrameAccountExplorer.UpdatePrivateKeys;
  FrameAccountExplorer.UpdateAccounts(false);
  if TSettings.FirstTime then begin
    TSettings.FirstTime := false;
    miAboutPascalCoinClick(Nil);
  end;
  PageControlChange(Nil);
end;

Procedure TFRMWallet.InitMacOSMenu;
{$IFDEF FPC}
var
  mi : TMenuItem;
  app : TMenuItem;
{$ENDIF}
begin
  {$IFDEF FPC}
  {$IFNDEF DARWIN}
  Exit;
  {$ENDIF}
  app := TMenuItem.Create(MainMenu);
  app := TMenuItem.Create(MainMenu);
  app.Caption:=#$EF#$A3#$BF;

  miAboutPascalCoin.Parent.Remove(miAboutPascalCoin);
  app.Add(miAboutPascalCoin);
  {$IFDEF TESTNET}
  // Things for testing purposes only
  miAbout.caption := 'Testing';
  {$ELSE}
  miAbout.Visible:=false;
  {$ENDIF}

  // Move "Options" to "Prefernces"
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='-';
  app.Add(mi);
  miOptions.Parent.Remove(miOptions);
  miOptions.Caption:='Preferences...';
  miOptions.ShortCut:=Menus.ShortCut(VK_OEM_COMMA, [ssMeta]);
  app.Add(miOptions);

  MiFindaccount.ShortCut:=Menus.ShortCut(VK_F, [ssMeta]);
  MiOperationsExplorer.ShortCut:=Menus.ShortCut(VK_E, [ssMeta]);
  MiFindpreviousaccountwithhighbalance.ShortCut:=Menus.Shortcut(VK_F3, [ssMeta]);
  miOptions.ShortCut:=Menus.ShortCut(VK_OEM_COMMA, [ssMeta]); // match preferences
  miPrivatekeys.ShortCut:=Menus.Shortcut(VK_P, [ssMeta]);

  N1.Visible:=false;
  MiClose.Visible:=false;

  MainMenu.Items.Insert(0, app);
  {$endif}
end;








function TFRMWallet.ForceMining: Boolean;
begin
  Result := false;
end;

procedure TFRMWallet.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FThreadActivate) then begin
    TThreadActivate(FThreadActivate).Terminate;
    FThreadActivate := Nil;
  end;
end;

procedure TFRMWallet.FormCreate(Sender: TObject);
Var i : Integer;
begin
  {$IFDEF USE_GNUGETTEXT}TranslateComponent(self);{$ENDIF}
  //
  {$IFNDEF FPC}
  {$IFDEF TESTNET}
  System.ReportMemoryLeaksOnShutdown := True; // Delphi memory leaks testing
  {$ENDIF}
  {$ENDIF}

  FLastNodesCacheUpdatedTS := Now;
  FBackgroundPanel := Nil;
  FBackgroundLabel := Nil;
  FThreadActivate := Nil;
  FMustProcessWalletChanged := false;
  FMustProcessNetConnectionUpdated := false;
  FRPCServer := Nil;
  FNode := Nil;
  FPoolMiningServer := Nil;
  FMessagesUnreadCount := 0;
  FrameInfo.lblReceivedMessages.Visible := false;

  FUpdating := false;
  TimerUpdateStatus.Enabled := false;
  FIsActivated := false;
  FWalletKeys := TWalletKeysExt.Create(Self);
  for i := 0 to StatusBar.Panels.Count - 1 do begin
    StatusBar.Panels[i].Text := '';
  end;
  FLog := TLog.Create(Self);
  FLog.OnNewLog := OnNewLog;
  FLog.SaveTypes := [];
  If Not ForceDirectories(TNode.GetPascalCoinDataFolder) then raise Exception.Create('Cannot create dir: '+TNode.GetPascalCoinDataFolder);
  TSettings.Load;
  TSettings.OnChanged.Add(UpdateConfigChanged);
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnBlocksChanged := OnNewAccount;
  FNodeNotifyEvents.OnNodeMessageEvent := OnNodeMessageEvent;
  FNodeNotifyEvents.OnKeyActivity := OnNodeKeysActivity;

  FSelectedAccountsGrid := TAccountsGrid.Create(Self);
  FSelectedAccountsGrid.AccountsGridDatasource := acds_InternalList;
  FSelectedAccountsGrid.DrawGrid := FrameAccountExplorer.dgSelectedAccounts;
  FSelectedAccountsGrid.OnUpdated := OnSelectedAccountsGridUpdated;
  FOperationsAccountGrid := TOperationsGrid.Create(Self);
  FOperationsAccountGrid.DrawGrid := FrameAccountExplorer.dgAccountOperations;
  FOperationsAccountGrid.MustShowAlwaysAnAccount := true;
  FOperationsAccountGrid.WalletKeys := FWalletKeys;
  FPendingOperationsGrid := TOperationsGrid.Create(Self);
  FPendingOperationsGrid.DrawGrid := FramePendingOperations.dgPendingOperations;
  FPendingOperationsGrid.AccountNumber := -1; // all
  FPendingOperationsGrid.PendingOperations := true;
  FPendingOperationsGrid.WalletKeys := FWalletKeys;
  FOperationsExplorerGrid := TOperationsGrid.Create(Self);
  FOperationsExplorerGrid.DrawGrid := FrameOperationsExplorer.dgOperationsExplorer;
  FOperationsExplorerGrid.AccountNumber := -1;
  FOperationsExplorerGrid.PendingOperations := False;
  FOperationsExplorerGrid.WalletKeys := FWalletKeys;
  FBlockChainGrid := TBlockChainGrid.Create(Self);
  FBlockChainGrid.DrawGrid := FrameBlockChainExplorer.dgBlockChainExplorer;
  FBlockChainGrid.ShowTimeAverageColumns:={$IFDEF SHOW_AVERAGE_TIME_STATS}True;{$ELSE}False;{$ENDIF}
  // FWalletKeys.OnChanged.Add( OnWalletChanged );
  {$IFDEF USE_GNUGETTEXT}
  // use language from the params and retranslate if needed
  // might be better to move this a bit earlier in the formcreate routine
  UseLanguage(TSettings.AppParams.ParamByName[CT_PARAM_UILanguage].GetAsString(GetCurrentLanguage));
  RetranslateComponent(self);
  {$ENDIF}
  //
  FrameAccountExplorer.UpdatePrivateKeys;
  UpdateBlockChainState;
  UpdateConnectionStatus;
  PageControl.ActivePage := tsOperations;

  FrameAccountExplorer.cbExploreMyAccountsClick(nil);

  MinersBlocksFound := 0;
  FrameInfo.lblBuild.Caption := 'Build: '+CT_ClientAppVersion;
  {$IFDEF TESTNET}
  FrameInfo.lblBuild.Font.Color := clRed;
  {$ENDIF}
  PageControl.Enabled := False;
  PageControl.Visible := False;
  FBackgroundPanel := TPanel.Create(Self);
  FBackgroundPanel.Parent:=Self;
  FBackgroundPanel.Align:=alClient;
  FBackgroundPanel.Font.Size:=15;
  FBackgroundPanel.BevelWidth := 10;
  FBackgroundLabel := TLabel.Create(Self);
  FBackgroundLabel.Parent := FBackgroundPanel;
  FBackgroundLabel.Align := alClient;
  FBackgroundLabel.Layout := tlCenter;
  FBackgroundLabel.Font.Size := 18;
  FBackgroundLabel.Alignment := taCenter;
  FBackgroundLabel.WordWrap := True;

   // set some frame properties
  with FrameAccountExplorer do
  begin
    pcAccountsOptions.ActivePage := tsAccountOperations;
    cbExploreMyAccounts.Checked:=True; // By default
  end;

  {$ifdef DARWIN}
  // this is macOS specific menu layout
  InitMacOSMenu;
  {$endif}
  PageControl.ActivePageIndex := 0;
end;

procedure TFRMWallet.FormDestroy(Sender: TObject);
Var i : Integer;
  step : String;
begin
  TLog.NewLog(ltinfo,Classname,'Destroying form - START');
  Try
    if Assigned(FThreadActivate) then begin
      TThreadActivate(FThreadActivate).Terminate;
      FThreadActivate := Nil;
    end;

  FreeAndNil(FRPCServer);
  FreeAndNil(FPoolMiningServer);
  step := 'Saving params';
  TSettings.Save;
  //
  step := 'Assigning nil events';
  FLog.OnNewLog :=Nil;
  FNodeNotifyEvents.Node := Nil;
  FOperationsAccountGrid.Node := Nil;
  FOperationsExplorerGrid.Node := Nil;
  FPendingOperationsGrid.Node := Nil;
  FrameAccountExplorer.AccountsGrid.Node := Nil;
  FSelectedAccountsGrid.Node := Nil;
  TNetData.NetData.OnReceivedHelloMessage := Nil;
  TNetData.NetData.OnStatisticsChanged := Nil;
  TNetData.NetData.OnNetConnectionsUpdated := Nil;
  TNetData.NetData.OnNodeServersUpdated := Nil;
  TNetData.NetData.OnBlackListUpdated := Nil;
  //

  step := 'Destroying NodeNotifyEvents';
  FreeAndNil(FNodeNotifyEvents);
  //
  step := 'Assigning Nil to TNetData';
  TNetData.NetData.OnReceivedHelloMessage := Nil;
  TNetData.NetData.OnStatisticsChanged := Nil;

  step := 'Destroying grids operators';
  FreeAndNil(FOperationsAccountGrid);
  FreeAndNil(FOperationsExplorerGrid);
  FreeAndNil(FBlockChainGrid);

  step := 'Desactivating Node';
  TNode.Node.NetServer.Active := false;
  FNode := Nil;

  TNetData.NetData.Free;

  step := 'Processing messages 1';
  Application.ProcessMessages;

  step := 'Destroying Node';
  TNode.Node.Free;

  step := 'Destroying Wallet';
  FreeAndNil(FWalletKeys);
  step := 'Processing messages 2';
  Application.ProcessMessages;
  step := 'Destroying stringslist';
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error destroying Form step: '+step+' Errors ('+E.ClassName+'): ' +E.Message);
    end;
  End;
  TLog.NewLog(ltinfo,Classname,'Destroying form - END');
  FreeAndNil(FLog);
  Sleep(100);
end;

procedure TFRMWallet.CheckIsReady;
Var isready : String;
begin
  if Not Assigned(FNode) then Abort;

  if Not FNode.IsReady(isready) then begin
    Raise Exception.Create('You cannot do this operation now:'+#10+#10+isready);
  end;
end;

procedure TFRMWallet.CM_NetConnectionUpdated(var Msg: TMessage);
Const CT_BooleanToString : Array[Boolean] of String = ('False','True');
Var i : integer;
 NC : TNetConnection;
 l : TList<TNetConnection>;
 sClientApp, sLastConnTime : String;
 strings, sNSC, sRS, sDisc : TStrings;
 hh,nn,ss,ms : Word;
begin
  Try
    if Not TNetData.NetData.NetConnections.TryLockList(100,l) then exit;
    try
      strings := FrameNodeStats.memoNetConnections.Lines;
      sNSC := TStringList.Create;
      sRS := TStringList.Create;
      sDisc := TStringList.Create;
      strings.BeginUpdate;
      Try
        for i := 0 to l.Count - 1 do begin
          NC := l[i];
          If NC.Client.BytesReceived>0 then begin
            sClientApp := '['+IntToStr(NC.NetProtocolVersion.protocol_version)+'-'+IntToStr(NC.NetProtocolVersion.protocol_available)+'] '+NC.ClientAppVersion;
          end else begin
            sClientApp := '(no data)';
          end;

          if NC.Connected then begin
            if NC.Client.LastCommunicationTime>1000 then begin
              DecodeTime(now - NC.Client.LastCommunicationTime,hh,nn,ss,ms);
              if (hh=0) and (nn=0) And (ss<10) then begin
                sLastConnTime := ' - Last comunication <10 sec.';
              end else begin
                sLastConnTime := Format(' - Last comunication %.2dm%.2ds',[(hh*60)+nn,ss]);
              end;
            end else begin
              sLastConnTime := '';
            end;
            if NC is TNetServerClient then begin
              sNSC.Add(Format('Client: IP:%s Block:%d Sent/Received:%d/%d Bytes - %s - Time offset %d - Active since %s %s',
                [NC.ClientRemoteAddr,NC.RemoteOperationBlock.block,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,NC.TimestampDiff,DateTimeElapsedTime(NC.CreatedTime),sLastConnTime]));
            end else begin
              if NC.IsMyselfServer then sNSC.Add(Format('MySelf IP:%s Sent/Received:%d/%d Bytes - %s - Time offset %d - Active since %s %s',
                [NC.ClientRemoteAddr,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,NC.TimestampDiff,DateTimeElapsedTime(NC.CreatedTime),sLastConnTime]))
              else begin
                sRS.Add(Format('Remote Server: IP:%s Block:%d Sent/Received:%d/%d Bytes - %s - Time offset %d - Active since %s %s',
                [NC.ClientRemoteAddr,NC.RemoteOperationBlock.block,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,NC.TimestampDiff,DateTimeElapsedTime(NC.CreatedTime),sLastConnTime]));
              end;
            end;
          end else begin
            if NC is TNetServerClient then begin
              sDisc.Add(Format('Disconnected client: IP:%s - %s',[NC.ClientRemoteAddr,sClientApp]));
            end else if NC.IsMyselfServer then begin
              sDisc.Add(Format('Disconnected MySelf IP:%s - %s',[NC.ClientRemoteAddr,sClientApp]));
            end else begin
              sDisc.Add(Format('Disconnected Remote Server: IP:%s %s - %s',[NC.ClientRemoteAddr,CT_BooleanToString[NC.Connected],sClientApp]));
            end;
          end;
        end;
        strings.Clear;
        strings.Add(Format('Connections Updated %s Clients:%d Servers:%d (valid servers:%d)',[DateTimeToStr(now),sNSC.Count,sRS.Count,TNetData.NetData.NetStatistics.ServersConnectionsWithResponse]));
        strings.AddStrings(sRS);
        strings.AddStrings(sNSC);
        if sDisc.Count>0 then begin
          strings.Add('');
          strings.Add('Disconnected connections: '+Inttostr(sDisc.Count));
          strings.AddStrings(sDisc);
        end;
      Finally
        strings.EndUpdate;
        sNSC.Free;
        sRS.Free;
        sDisc.Free;
      End;
      //CheckMining;
    finally
      TNetData.NetData.NetConnections.UnlockList;
    end;
  Finally
    FMustProcessNetConnectionUpdated := false;
  End;
end;

procedure TFRMWallet.CM_WalletChanged(var Msg: TMessage);
begin
  FrameAccountExplorer.UpdatePrivateKeys;
  FMustProcessWalletChanged := false;
end;

procedure TFRMWallet.DoUpdateAccounts;
begin
  FrameAccountExplorer.UpdateAccounts(true);
end;

procedure TFRMWallet.FinishedLoadingApp;
var LLockedMempool : TPCOperationsComp;
  LFoundAccounts, i, LOnSafebox,LOnMempool : Integer;
  Lpubkeys : TList<TAccountKey>;
begin
  FNodeNotifyEvents.Node := FNode;
  // Init
  TNetData.NetData.OnReceivedHelloMessage := OnReceivedHelloMessage;
  TNetData.NetData.OnStatisticsChanged := OnNetStatisticsChanged;
  TNetData.NetData.OnNetConnectionsUpdated := onNetConnectionsUpdated;
  TNetData.NetData.OnNodeServersUpdated := OnNetNodeServersUpdated;
//  TNetData.NetData.OnBlackListUpdated := OnNetBlackListUpdated;  try to move to FrameNodeStats

  //
  TimerUpdateStatus.Interval := 1000;
  TimerUpdateStatus.Enabled := true;
  //
  FPoolMiningServer := TPoolMiningServer.Create;
  FPoolMiningServer.Port := TSettings.JsonRpcMinerServerPort;
  FPoolMiningServer.MinerAccountKey := GetAccountKeyForMiner;
  FPoolMiningServer.MinerPayload := TEncoding.ANSI.GetBytes(TSettings.MinerName);
  LLockedMempool := FNode.LockMempoolWrite;
  try
    LLockedMempool.AccountKey := GetAccountKeyForMiner;
  finally
    FNode.UnlockMempoolWrite;
  end;
  FPoolMiningServer.Active := TSettings.JsonRpcMinerServerActive;
  FPoolMiningServer.OnMiningServerNewBlockFound := OnMiningServerNewBlockFound;
  FreeAndNil(FBackgroundLabel);
  FreeAndNil(FBackgroundPanel);

  PageControl.Visible:=True;
  PageControl.Enabled:=True;

  FrameAccountExplorer.UpdatePrivateKeys;
  //
  LFoundAccounts := 0;
  FNode.Bank.SafeBox.StartThreadSafe;
  try
    Lpubkeys := TList<TAccountKey>.Create;
    Try
      for i := 0 to FWalletKeys.Count-1 do begin
        if (FWalletKeys.Key[i].HasPrivateKey) then begin
          Lpubkeys.Add(FWalletKeys.Key[i].AccountKey);
        end;
      end;
      if (Lpubkeys.Count>0) then begin
        LFoundAccounts := FNode.GetAccountsAvailableByPublicKey(Lpubkeys,LOnSafebox,LOnMempool);
      end else LFoundAccounts := 0;
    Finally
      Lpubkeys.Free;
    End;
  finally
    FNode.Bank.SafeBox.EndThreadSave;
  end;
  if LFoundAccounts<1 then begin
    // Will only ask if no accounts
    TFRMAskForAccount.AskForAccount(Self,FNode,TNetData.NetData,FWalletKeys,GetAccountKeyForMiner);
  end;
end;

procedure TFRMWallet.FillAccountInformation(const Strings: TStrings;
  const AccountNumber: Cardinal);
Var account : TAccount;
  s : String;
  LjsonObj : TPCJSONObject;
begin
  if AccountNumber<0 then exit;
  account := FNode.GetMempoolAccount(AccountNumber);
  if Length(account.name)>0 then s:='Name: '+TEncoding.ANSI.GetString(account.name)
  else s:='';
  Strings.Add(Format('Account: %s %s Type:%d',[TAccountComp.AccountNumberToAccountTxtNumber(AccountNumber),s,account.account_type]));
  Strings.Add('');
  Strings.Add(Format('Current balance: %s',[TAccountComp.FormatMoney(account.balance)]));
  Strings.Add('');
  Strings.Add(Format('Updated on block: %d  (%d blocks ago)',[account.GetLastUpdatedBlock,FNode.Bank.BlocksCount-account.GetLastUpdatedBlock]));
  Strings.Add(Format('Updated on block as active mode: %d  (%d blocks ago)',[account.updated_on_block_active_mode,FNode.Bank.BlocksCount-account.updated_on_block_active_mode]));
  Strings.Add(Format('Public key type: %s',[TAccountComp.GetECInfoTxt(account.accountInfo.accountKey.EC_OpenSSL_NID)]));
  Strings.Add(Format('Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.accountKey)]));
  if Length(account.account_data)>0 then
    Strings.Add(Format('Account Data: %s',[account.account_data.ToHexaString]))
  else Strings.Add(Format('Account Data: (No data)',[]));
  Strings.Add(Format('Account Seal: %s',[account.account_seal.ToHexaString]));
  if TAccountComp.IsAccountForSale(account.accountInfo) then begin
    Strings.Add('');
    Strings.Add('** Account is for sale: **');
    Strings.Add(Format('Price: %s',[TAccountComp.FormatMoney(account.accountInfo.price)]));
    Strings.Add(Format('Seller account (where to pay): %s',[TAccountComp.AccountNumberToAccountTxtNumber(account.accountInfo.account_to_pay)]));
    if TAccountComp.IsAccountForPrivateSale(account.accountInfo) then begin
      Strings.Add('');
      Strings.Add('** Private sale **');
      Strings.Add(Format('New Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.new_publicKey)]));
      Strings.Add('');
      if TAccountComp.IsAccountLocked(account.accountInfo,FNode.Bank.BlocksCount) then begin
        Strings.Add(Format('PURCHASE IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [account.accountInfo.locked_until_block,FNode.Bank.BlocksCount,account.accountInfo.locked_until_block-FNode.Bank.BlocksCount]));
      end else begin
        Strings.Add(Format('PURCHASE IS NOT SECURE (Expired on block %d, current %d)',
          [account.accountInfo.locked_until_block,FNode.Bank.BlocksCount]));
      end;
    end;
  end else if TAccountComp.IsAccountForSwap(account.accountInfo) then begin
    Strings.Add('');
    if TAccountComp.IsAccountForAccountSwap(account.accountInfo) then begin
      Strings.Add('** Account is for Atomic Account Swap: **');
      Strings.Add(Format('New Base58 Public key: %s',[TAccountComp.AccountPublicKeyExport(account.accountInfo.new_publicKey)]));
    end else if TAccountComp.IsAccountForCoinSwap(account.accountInfo) then begin
      Strings.Add('** Account is for Atomic Coin Swap: **');
      Strings.Add(Format('Amount to swap: %s',[TAccountComp.FormatMoney(account.accountInfo.price)]));
      Strings.Add(Format('Counterparty account: %s',[TAccountComp.AccountNumberToAccountTxtNumber(account.accountInfo.account_to_pay)]));
    end;
    Strings.Add(Format('Public secret to find: %s',[account.accountInfo.hashed_secret.ToHexaString]));
    Strings.Add('');
    if TAccountComp.IsAccountLocked(account.accountInfo,FNode.Bank.BlocksCount) then begin
      Strings.Add(Format('SWAP IS SECURE UNTIL BLOCK %d (current %d, remains %d)',
          [account.accountInfo.locked_until_block,FNode.Bank.BlocksCount,account.accountInfo.locked_until_block-FNode.Bank.BlocksCount]));
    end else begin
        Strings.Add(Format('SWAP IS NOT SECURE (Expired on block %d, current %d)',
          [account.accountInfo.locked_until_block,FNode.Bank.BlocksCount]));
    end;
  end;
  LjsonObj := TPCJSONObject.Create;
  Try
    TPascalCoinJSONComp.FillAccountObject(account,LjsonObj);
    Strings.Add('ACCOUNT JSON:');
    Strings.Add(LjsonObj.ToJSON(False));
  Finally
    LjsonObj.Free;
  end;

end;

procedure TFRMWallet.FillOperationInformation(const Strings: TStrings;
  const OperationResume: TOperationResume);
var i : Integer;
  jsonObj : TPCJSONObject;
  LEPASA : TEPasa;
begin
  If (not OperationResume.valid) then exit;
  If OperationResume.Block<FNode.Bank.BlocksCount then
    if (OperationResume.NOpInsideBlock>=0) then begin
      Strings.Add(Format('Block: %d/%d',[OperationResume.Block,OperationResume.NOpInsideBlock]))
    end else begin
      Strings.Add(Format('Block: %d',[OperationResume.Block]))
    end
  else Strings.Add('** Pending operation not included on blockchain **');
  Strings.Add(Format('%s',[OperationResume.OperationTxt]));
  If (OperationResume.isMultiOperation) then begin
    Strings.Add('Multioperation:');
    For i := 0 to High(OperationResume.Senders) do begin
      Strings.Add(Format('  Sender (%d/%d): %s %s PASC Payload(%d):%s',[i+1,length(OperationResume.Senders),TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.Senders[i].Account),TAccountComp.FormatMoney(OperationResume.Senders[i].Amount),OperationResume.Senders[i].Payload.payload_type,OperationResume.Senders[i].Payload.payload_raw.ToHexaString]));
    end;
    For i := 0 to High(OperationResume.Receivers) do begin
      Strings.Add(Format('  Receiver (%d/%d): %s %s PASC Payload(%d):%s',[i+1,length(OperationResume.Receivers),TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.Receivers[i].Account),TAccountComp.FormatMoney(OperationResume.Receivers[i].Amount),OperationResume.Receivers[i].Payload.payload_type,OperationResume.Receivers[i].Payload.payload_raw.ToHexaString]));
    end;
    For i := 0 to High(OperationResume.Changers) do begin
      Strings.Add(Format('  Change info (%d/%d): %s [%s]',[i+1,length(OperationResume.Changers),TAccountComp.AccountNumberToAccountTxtNumber(OperationResume.Changers[i].Account),TOpMultiOperation.OpChangeAccountInfoTypesToText(OperationResume.Changers[i].Changes_type)]));
    end;

  end;
  Strings.Add(Format('OpType:%d Subtype:%d',[OperationResume.OpType,OperationResume.OpSubtype]));
  Strings.Add(Format('Operation Hash (ophash): %s',[TCrypto.ToHexaString(OperationResume.OperationHash)]));
  If (Length(OperationResume.OperationHash_OLD)>0) then begin
    Strings.Add(Format('Old Operation Hash (old_ophash): %s',[TCrypto.ToHexaString(OperationResume.OperationHash_OLD)]));
  end;
  if TEPasaDecoder.TryDecodeEPASA(OperationResume.DestAccount,OperationResume.OriginalPayload,FNode,FWalletKeys,Nil,LEPASA) then begin
    Strings.Add('EPASA: '+LEPASA.ToString);
  end else Strings.Add('No EPASA format');
  Strings.Add(Format('Payload type:%s length:%d',['0x'+IntToHex(OperationResume.OriginalPayload.payload_type,2), length(OperationResume.OriginalPayload.payload_raw)]));
  if (Length(OperationResume.OriginalPayload.payload_raw)>0) then begin
    If OperationResume.PrintablePayload<>'' then begin
      Strings.Add(Format('Payload (human): %s',[OperationResume.PrintablePayload]));
    end;
    Strings.Add(Format('Payload (Hexadecimal): %s',[TCrypto.ToHexaString(OperationResume.OriginalPayload.payload_raw)]));
  end;
  If OperationResume.Balance>=0 then begin
    Strings.Add(Format('Final balance: %s',[TAccountComp.FormatMoney(OperationResume.Balance)]));
  end;
  jsonObj := TPCJSONObject.Create;
  Try
    TPascalCoinJSONComp.FillOperationObject(OperationResume,FNode.Bank.BlocksCount,
      FNode,FWalletKeys,Nil,
      jsonObj);
    Strings.Add('OPERATION JSON:');
    Strings.Add(jsonObj.ToJSON(False));
  Finally
    jsonObj.Free;
  end;
end;





procedure TFRMWallet.MiOperationsExplorerClick(Sender: TObject);
begin
  With TFRMOperationsExplorer.Create(Self) do
  try
    SourceNode := FNode;
    SourceWalletKeys := FWalletKeys;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFRMWallet.MiRPCCallsClick(Sender: TObject);
Var FRM : TFRMRPCCalls;
begin
  FRM := TFRMRPCCalls.Create(Self);
  Try
    FRM.ServerURL:='127.0.0.1:'+IntToStr(FRPCServer.Port);
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;


function TFRMWallet.GetAccountKeyForMiner: TAccountKey;
Var PK : TECPrivateKey;
  i : Integer;
  PublicK : TECDSA_Public;
begin
  Result := CT_TECDSA_Public_Nul;
  if Not Assigned(FWalletKeys) then exit;
  case FMinerPrivateKeyType of
    mpk_NewEachTime: PublicK := CT_TECDSA_Public_Nul;
    mpk_Selected: begin
      PublicK := TAccountComp.RawString2Accountkey(TSettings.MinerSelectedPublicKey);
    end;
  else
    // Random
    PublicK := CT_TECDSA_Public_Nul;
    if FWalletKeys.Count>0 then begin
      i := Random(FWalletKeys.Count);
      if (Length(FWalletKeys.Key[i].CryptedKey)=0) then begin
        // Not valid, search for first valid:
        i:=0;
        while (i<FWalletKeys.Count) And (Length(FWalletKeys.Key[i].CryptedKey)=0) do inc(i);
        if i<FWalletKeys.Count then PublicK := FWalletKeys.Key[i].AccountKey;
      end else PublicK := FWalletKeys.Key[i].AccountKey;
    end;
  end;
  i := FWalletKeys.IndexOfAccountKey(PublicK);
  if i>=0 then begin
    if (Length(FWalletKeys.Key[i].CryptedKey)=0) then i:=-1;
  end;
  if i<0 then begin
    PK := TECPrivateKey.Create;
    try
      PK.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
      FWalletKeys.AddPrivateKey('New for miner '+DateTimeToStr(Now), PK);
      PublicK := PK.PublicKey;
      // Set to AppParams if not mpk_NewEachTime
      if (FMinerPrivateKeyType<>mpk_NewEachTime) then begin
        TSettings.MinerSelectedPublicKey := TAccountComp.AccountKey2RawString(PublicK);
        FMinerPrivateKeyType:=mpk_Selected;
        TSettings.MinerPrivateKeyType := mpk_Selected;
      end;
    finally
      PK.Free;
    end;
  end;
  Result := PublicK;
end;

procedure TFRMWallet.IPnodes1Click(Sender: TObject);
Var FRM : TFRMNodesIp;
begin
  FRM := TFRMNodesIp.Create(Self);
  Try
    FRM.AppParams := TSettings.AppParams;
    FRM.ShowModal;
  Finally
    FRM.Free;
  End;
end;


procedure TFRMWallet.miAboutPascalCoinClick(Sender: TObject);
begin
  With TFRMAbout.Create(Self) do
  try
    showmodal;
  finally
    free;
  end;
end;

procedure TFRMWallet.MiAccountInformationClick(Sender: TObject);
Var F : TFRMMemoText;
  accn : Int64;
  s,title : String;
  account : TAccount;
  strings : TStrings;
  i : Integer;
  opr : TOperationResume;
begin
  accn := -1;
  title := '';
  strings := TStringList.Create;
  try
    opr := CT_TOperationResume_NUL;
    if PageControl.ActivePage=tsOperations then begin
      i := FOperationsExplorerGrid.DrawGrid.Row;
      if (i>0) and (i<=FOperationsExplorerGrid.OperationsResume.Count) then begin
        opr := FOperationsExplorerGrid.OperationsResume.OperationResume[i-1];
      end;
    end else if PageControl.ActivePage=tsPendingOperations then begin
      i := FPendingOperationsGrid.DrawGrid.Row;
      if (i>0) and (i<=FPendingOperationsGrid.OperationsResume.Count) then begin
        opr := FPendingOperationsGrid.OperationsResume.OperationResume[i-1];
      end;
    end else if PageControl.ActivePage=tsMyAccounts then begin
      accn := FrameAccountExplorer.AccountsGrid.AccountNumber(FrameAccountExplorer.dgAccounts.Row);
      if accn<0 then raise Exception.Create('Select an account');
      FillAccountInformation(strings,accn);
      title := 'Account '+TAccountComp.AccountNumberToAccountTxtNumber(accn)+' info';
      i := FOperationsAccountGrid.DrawGrid.Row;
      if (i>0) and (i<=FOperationsAccountGrid.OperationsResume.Count) then begin
        opr := FOperationsAccountGrid.OperationsResume.OperationResume[i-1];
      end;
    end;
    If (opr.valid) then begin
      if accn>=0 then strings.Add('')
      else title := 'Operation info';
      strings.Add('Operation info:');
      FillOperationInformation(strings,opr);
    end else if accn<0 then Raise Exception.Create('No info available');
    F := TFRMMemoText.Create(Self);
    Try
      F.Caption := title;
      strings.add(Format('Agg Hashrate: %s',[FNode.Bank.SafeBox.AggregatedHashrate.ToDecimal]));
      strings.add(Format('Agg Hashrate: %s',[FNode.Bank.SafeBox.AggregatedHashrate.HexaValue]));
      F.Memo.Lines.Assign(strings);
      F.ShowModal;
    Finally
      F.Free;
    End;
  finally
    strings.free;
  end;
end;

procedure TFRMWallet.MiAddaccounttoSelectedClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  FrameAccountExplorer.pcAccountsOptions.ActivePage := FrameAccountExplorer.tsMultiSelectAccounts;
  FrameAccountExplorer.sbSelectedAccountsAddClick(Sender);
end;

procedure TFRMWallet.miAskForAccountClick(Sender: TObject);
begin
  TFRMAskForAccount.AskForAccount(Self,FNode,TNetData.NetData,FWalletKeys,GetAccountKeyForMiner);
end;

procedure TFRMWallet.MiCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFRMWallet.MiDecodePayloadClick(Sender: TObject);
begin
  if PageControl.ActivePage=tsOperations then begin
    FOperationsExplorerGrid.ShowModalDecoder(FWalletKeys, TSettings.AppParams);
  end else if PageControl.ActivePage=tsPendingOperations then begin
    FPendingOperationsGrid.ShowModalDecoder(FWalletKeys,TSettings.AppParams);
  end else if PageControl.ActivePage=tsMyAccounts then begin
    FOperationsAccountGrid.ShowModalDecoder(FWalletKeys,TSettings.AppParams);
  end;
end;



procedure TFRMWallet.MiFindaccountClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  FrameAccountExplorer.ebFindAccountNumber.SetFocus;
end;

procedure TFRMWallet.MiFindnextaccountwithhighbalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  an64 := FrameAccountExplorer.AccountsGrid.AccountNumber(FrameAccountExplorer.dgAccounts.Row);
  if an64<0 then an := 0
  else an := an64;
  If an>=FNode.Bank.SafeBox.AccountsCount then exit;
  start := FNode.Bank.SafeBox.Account(an);
  while (an<FNode.Bank.SafeBox.AccountsCount)  do begin
    if FNode.Bank.SafeBox.Account(an).balance>start.balance then break
    else inc(an);
  end;
  if (an<FNode.Bank.SafeBox.AccountsCount) then FrameAccountExplorer.AccountsGrid.MoveRowToAccount(an)
  else raise Exception.Create('Not found any account higher than '+TAccountComp.AccountNumberToAccountTxtNumber(start.account)+' with balance higher than '+
    TAccountComp.FormatMoney(start.balance));
end;

procedure TFRMWallet.MiFindOperationbyOpHashClick(Sender: TObject);
Var FRM : TFRMPayloadDecoder;
  oph : String;
begin
  oph := '';
  if Not InputQuery('Search operation by OpHash','Insert Operation Hash value (OpHash)',oph) then exit;
  //
  FRM := TFRMPayloadDecoder.Create(Self);
  try
    FRM.Init(CT_TOperationResume_NUL,WalletKeys,TSettings.AppParams);
    FRM.DoFind(oph);
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

procedure TFRMWallet.MiFindpreviousaccountwithhighbalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  an64 := FrameAccountExplorer.AccountsGrid.AccountNumber(FrameAccountExplorer.dgAccounts.Row);
  if an64<0 then an := FNode.Bank.SafeBox.AccountsCount-1
  else an := an64;
  If an>=FNode.Bank.SafeBox.AccountsCount then exit;
  start := FNode.Bank.SafeBox.Account(an);
  while (an>0)  do begin
    if FNode.Bank.SafeBox.Account(an).balance>start.balance then break
    else dec(an);
  end;
  if (FNode.Bank.SafeBox.Account(an).balance>start.balance) then FrameAccountExplorer.AccountsGrid.MoveRowToAccount(an)
  else raise Exception.Create('Not found any account lower than '+TAccountComp.AccountNumberToAccountTxtNumber(start.account)+' with balance higher than '+
    TAccountComp.FormatMoney(start.balance));
end;

procedure TFRMWallet.MiMultiaccountoperationClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  FrameAccountExplorer.pcAccountsOptions.ActivePage := FrameAccountExplorer.tsMultiSelectAccounts;
  FrameAccountExplorer.bbSelectedAccountsOperationClick(Sender);
end;

procedure TFRMWallet.miNewOperationClick(Sender: TObject);
var l : TOrderedCardinalList;
begin
  CheckIsReady;
  With TFRMOperation.Create(Self) do
  Try
    l := TOrderedCardinalList.Create;
    try
      If FrameAccountExplorer.AccountsGrid.SelectedAccounts(l)<1 then raise Exception.Create('No row selected');
      SenderAccounts.CopyFrom(l);
    finally
      l.Free;
    end;
    DefaultFee := TSettings.DefaultFee;
    WalletKeys := FWalletKeys;
    ShowModal;
  Finally
    Free;
  End;
end;

procedure TFRMWallet.miOptionsClick(Sender: TObject);
begin
  With TFRMPascalCoinWalletConfig.Create(Self) do
  try
    AppParams := TSettings.AppParams;
    WalletKeys := Self.FWalletKeys;
    if ShowModal=MrOk then begin
      TSettings.Save;
      UpdateConfigChanged(Self);
      {$IFDEF USE_GNUGETTEXT}RetranslateComponent(self);{$ENDIF}
    end;
  finally
    free;
  end;
end;

procedure TFRMWallet.miPrivatekeysClick(Sender: TObject);
Var FRM : TFRMWalletKeys;
begin
  FRM := TFRMWalletKeys.Create(Self);
  Try
    FRM.WalletKeys := FWalletKeys;
    FRM.ShowModal;
    FrameAccountExplorer.UpdatePrivateKeys;
  Finally
    FRM.Free;
  End;
end;

procedure TFRMWallet.MiRemoveaccountfromselectedClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  FrameAccountExplorer.pcAccountsOptions.ActivePage := FrameAccountExplorer.tsMultiSelectAccounts;
  FrameAccountExplorer.sbSelectedAccountsDelClick(Sender);
end;



procedure TFRMWallet.OnMiningServerNewBlockFound(Sender: TObject);
begin
  FPoolMiningServer.MinerAccountKey := GetAccountKeyForMiner;
end;



procedure TFRMWallet.OnNetConnectionsUpdated(Sender: TObject);
begin
  if FMustProcessNetConnectionUpdated then exit;
  FMustProcessNetConnectionUpdated := true;
  PostMessage(Self.Handle,CM_PC_NetConnectionUpdated,0,0);
end;

procedure TFRMWallet.OnNetNodeServersUpdated(Sender: TObject);
Var i : integer;
 P : PNodeServerAddress;
 l : TList<Pointer>;
 strings : TStrings;
 s : String;
begin
  l := TNetData.NetData.NodeServersAddresses.LockList;
  try
    strings := FrameNodeStats.memoNetServers.Lines;
    strings.BeginUpdate;
    Try
      strings.Clear;
      strings.Add('NodeServers Updated: '+DateTimeToStr(now) +' Count: '+inttostr(l.Count));
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        if Not (P^.is_blacklisted) then begin
          s := Format('Server IP:%s:%d',[P^.ip,P^.port]);
          if (P^.last_connection_by_me>0) then begin
            s := s + ' [Server] ';
          end;

          if Assigned(P.netConnection) then begin
            If P.last_connection>0 then  s := s+ ' ** ACTIVE **'
            else s := s+' ** TRYING TO CONNECT **';
          end;
          if P.its_myself then begin
            s := s+' ** NOT VALID ** '+P.BlackListText;
          end;
          if P.last_connection>0 then begin
            s := s + ' Last connection: '+DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection)));
          end;
          if P.last_connection_by_server>0 then begin
            s := s + ' Last server connection: '+DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection_by_server)));
          end;
          if (P.last_attempt_to_connect>0) then begin
            s := s + ' Last attempt to connect: '+DateTimeToStr(P^.last_attempt_to_connect);
          end;
          if (P.total_failed_attemps_to_connect>0) then begin
            s := s + ' (Attempts: '+inttostr(P^.total_failed_attemps_to_connect)+')';
          end;

          strings.Add(s);
        end;
      end;
    Finally
      strings.EndUpdate;
    End;
  finally
    TNetData.NetData.NodeServersAddresses.UnlockList;
  end;
end;

procedure TFRMWallet.OnNetStatisticsChanged(Sender: TObject);
Var NS : TNetStatistics;
begin
  //CheckMining;
  if Assigned(FNode) then begin
    If FNode.NetServer.Active then begin
      StatusBar.Panels[0].Text := 'Active (Port '+Inttostr(FNode.NetServer.Port)+')';
    end else StatusBar.Panels[0].Text := 'Server stopped';
    NS := TNetData.NetData.NetStatistics;
    StatusBar.Panels[1].Text := Format('Connections:%d Clients:%d Servers:%d - Rcvd:%d Kb Send:%d Kb',
      [NS.ActiveConnections,NS.ClientsConnections,NS.ServersConnections,NS.BytesReceived DIV 1024,NS.BytesSend DIV 1024]);
  end else begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
  end;
end;

procedure TFRMWallet.OnNewAccount(Sender: TObject);
begin
  Try
    FrameAccountExplorer.UpdateAccounts(false);
    UpdateBlockChainState;
  Except
    On E:Exception do begin
      E.Message := 'Exception at OnNewAccount '+E.ClassName+': '+E.Message;
      TLog.NewLog(lterror,ClassName,E.Message);
      Raise;
    end;
  end;
end;

procedure TFRMWallet.OnNewLog(logtype: TLogType; Time : TDateTime; ThreadID : TThreadID; const sender,logtext: String);
Var s : AnsiString;
begin
  if (logtype=ltdebug) And (Not FrameLogs.cbShowDebugLogs.Checked) then exit;
  if ThreadID=MainThreadID then s := ' MAIN:' else s:=' TID:';
  if FrameLogs.MemoLogs.Lines.Count>300 then begin
    // Limit max lines in logs...
    FrameLogs.memoLogs.Lines.BeginUpdate;
    try
      while FrameLogs.memoLogs.Lines.Count>250 do FrameLogs.memoLogs.Lines.Delete(0);
    finally
      FrameLogs.memoLogs.Lines.EndUpdate;
    end;
  end;
  FrameLogs.memoLogs.Lines.Add(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time)+s+IntToHex(PtrInt(ThreadID),8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
  //
end;

procedure TFRMWallet.OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: String);
Var s : String;
begin
  inc(FMessagesUnreadCount);
  if Assigned(NetConnection) then begin
    s := DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr;
    FrameMessages.memoMessages.Lines.Add(DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr+' Length '+inttostr(Length(MessageData))+' bytes');
    FrameMessages.memoMessages.Lines.Add('RECEIVED> '+MessageData);
    if TSettings.ShowModalMessages then begin
      s := DateTimeToStr(now)+' Message from '+NetConnection.ClientRemoteAddr+#10+
         'Length '+inttostr(length(MessageData))+' bytes'+#10+#10;
      if TCrypto.IsHumanReadable(TEncoding.ANSI.GetBytes(MessageData)) then begin
         s := s + MessageData;
      end else begin
         s := s +'Value in hexadecimal:'+#10+
              TCrypto.ToHexaString(TEncoding.ANSI.GetBytes(MessageData));
      end;
      Application.MessageBox(PChar(s),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
    end;
  end else begin
    FrameMessages.memoMessages.Lines.Add(DateTimeToStr(now)+' Internal message: '+MessageData);
  end;
  if FMessagesUnreadCount>1 then FrameInfo.lblReceivedMessages.Caption := Format('You have received %d messages',[FMessagesUnreadCount])
  else FrameInfo.lblReceivedMessages.Caption := 'You have received 1 message';
  FrameInfo.lblReceivedMessages.Visible := true;
end;

procedure TFRMWallet.OnNodeKeysActivity(Sender: TObject);
begin
  DoUpdateAccounts;
end;

procedure TFRMWallet.OnReceivedHelloMessage(Sender: TObject);
Var nsarr : TNodeServerAddressArray;
  i : Integer;
  s : AnsiString;
begin
  If (FLastNodesCacheUpdatedTS + EncodeTime(0,5,0,0) > Now) then exit; // Prevent continuous saving
  FLastNodesCacheUpdatedTS := Now;
  // Update node servers Peer Cache
  nsarr := TNetData.NetData.NodeServersAddresses.GetValidNodeServers(true,0);
  s := '';
  for i := low(nsarr) to High(nsarr) do begin
    if (s<>'') then s := s+';';
    s := s + nsarr[i].ip+':'+IntToStr( nsarr[i].port );
  end;
  TSettings.PeerCache := s;
  TNode.Node.PeerCache := s;
end;

procedure TFRMWallet.OnSelectedAccountsGridUpdated(Sender: TObject);
begin
  FrameAccountExplorer.lblSelectedAccountsCount.Caption := Inttostr(FSelectedAccountsGrid.AccountsCount);
  FrameAccountExplorer.lblSelectedAccountsBalance.Caption := TAccountComp.FormatMoney( FSelectedAccountsGrid.AccountsBalance );
end;

procedure TFRMWallet.OnWalletChanged(Sender: TObject);
begin
  if FMustProcessWalletChanged then exit;
  FMustProcessWalletChanged := true;
  PostMessage(Self.Handle,CM_PC_WalletKeysChanged,0,0);
end;

procedure TFRMWallet.PageControlChange(Sender: TObject);
begin
  MiDecodePayload.Enabled := false;
  if PageControl.ActivePage=tsMyAccounts then begin
    FrameAccountExplorer.AccountsGrid.Node := FNode;
    MiDecodePayload.Enabled := true;
    FSelectedAccountsGrid.Node := FNode;
  end else begin
    FrameAccountExplorer.AccountsGrid.Node := Nil;
    FSelectedAccountsGrid.Node := Nil;
  end;
  if PageControl.ActivePage=tsPendingOperations then begin
    FPendingOperationsGrid.Node := FNode;
    MiDecodePayload.Enabled := true;
  end else FPendingOperationsGrid.Node := Nil;
  if PageControl.ActivePage=tsBlockChain then FBlockChainGrid.Node := FNode
  else FBlockChainGrid.Node := Nil;
  if PageControl.ActivePage=tsOperations then begin
    FOperationsExplorerGrid.Node := FNode;
    MiDecodePayload.Enabled := true;
  end else FOperationsExplorerGrid.Node := Nil;
  if PageControl.ActivePage=tsMessages then begin
    UpdateAvailableConnections;
    FMessagesUnreadCount := 0;
    FrameInfo.lblReceivedMessages.Visible := false;
  end;
end;


procedure TFRMWallet.SetMinersBlocksFound(const Value: Integer);
begin
  FMinersBlocksFound := Value;
  FrameInfo.lblBlocksFound.Caption := Inttostr(Value);
  if Value>0 then FrameInfo.lblBlocksFound.Font.Color := clGreen
  else FrameInfo.lblBlocksFound.Font.Color := clDkGray;
end;

procedure TFRMWallet.TimerUpdateStatusTimer(Sender: TObject);
begin
  Try
    UpdateConnectionStatus;
    UpdateBlockChainState;
    FrameInfo.UpdateNodeStatus;
  Except
    On E:Exception do begin
      E.Message := 'Exception at TimerUpdate '+E.ClassName+': '+E.Message;
      TLog.NewLog(lterror,ClassName,E.Message);
    end;
  End;
end;


procedure TFRMWallet.UpdateAvailableConnections;
Var i : integer;
 NC : TNetConnection;
 l : TList<TNetConnection>;
begin
  if Not TNetData.NetData.NetConnections.TryLockList(100,l) then exit;
  try
    FrameMessages.lbNetConnections.Items.BeginUpdate;
    Try
      FrameMessages.lbNetConnections.Items.Clear;
      for i := 0 to l.Count - 1 do begin
        NC := l[i];
        if NC.Connected then begin
          if NC is TNetServerClient then begin
            if Not NC.IsMyselfServer then begin
              FrameMessages.lbNetConnections.Items.AddObject(Format('Client: IP:%s',[NC.ClientRemoteAddr]),NC);
            end;
          end else begin
            if Not NC.IsMyselfServer then begin
              FrameMessages.lbNetConnections.Items.AddObject(Format('Server: IP:%s',[NC.ClientRemoteAddr]),NC);
            end;
          end;
        end;
      end;
    Finally
      FrameMessages.lbNetConnections.Items.EndUpdate;
    End;
  finally
    TNetData.NetData.NetConnections.UnlockList;
  end;
end;

procedure TFRMWallet.UpdateBlockChainState;
Var isMining : boolean;
  i,mc : Integer;
  s : String;
  f, favg : real;
  LLockedMempool : TPCOperationsComp;
begin
  FrameInfo.UpdateNodeStatus;
  mc := 0;
  if Assigned(FNode) then begin
    if FNode.Bank.BlocksCount>0 then begin
      FrameInfo.lblCurrentBlock.Caption :=  Inttostr(FNode.Bank.BlocksCount)+' (0..'+Inttostr(FNode.Bank.BlocksCount-1)+')'; ;
    end else FrameInfo.lblCurrentBlock.Caption :=  '(none)';
    FrameInfo.lblCurrentAccounts.Caption := Inttostr(FNode.Bank.AccountsCount);
    FrameInfo.lblCurrentBlockTime.Caption := UnixTimeToLocalElapsedTime(FNode.Bank.LastOperationBlock.timestamp);
    LLockedMempool := FNode.LockMempoolRead;
    try
      FrameInfo.lblOperationsPending.Caption := Inttostr(LLockedMempool.Count);
      FrameInfo.lblCurrentDifficulty.Caption := InttoHex(LLockedMempool.OperationBlock.compact_target,8);
    finally
      FNode.UnlockMempoolRead;
    end;
    favg := FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage);
    f := (CT_NewLineSecondsAvg - favg) / CT_NewLineSecondsAvg;
    FrameInfo.lblTimeAverage.Caption := 'Last '+Inttostr(CT_CalcNewTargetBlocksAverage)+': '+FormatFloat('0.0',favg)+' sec. (Optimal '+Inttostr(CT_NewLineSecondsAvg)+'s) Deviation '+FormatFloat('0.00%',f*100);
    if favg>=CT_NewLineSecondsAvg then begin
      FrameInfo.lblTimeAverage.Font.Color := clNavy;
    end else begin
      FrameInfo.lblTimeAverage.Font.Color := clOlive;
    end;
    FrameInfo.lblTimeAverageAux.Caption := Format('Last %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec.',[
        CT_CalcNewTargetBlocksAverage * 2 ,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage * 2)),
        ((CT_CalcNewTargetBlocksAverage * 3) DIV 2) ,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage((CT_CalcNewTargetBlocksAverage * 3) DIV 2)),
        ((CT_CalcNewTargetBlocksAverage DIV 4)*3),FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(((CT_CalcNewTargetBlocksAverage DIV 4)*3))),
        CT_CalcNewTargetBlocksAverage DIV 2,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 2)),
        CT_CalcNewTargetBlocksAverage DIV 4,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 4))]);
  end else begin
    isMining := false;

    with FrameInfo do
    begin
      lblCurrentBlock.Caption := '';
      lblCurrentAccounts.Caption := '';
      lblCurrentBlockTime.Caption := '';
      lblOperationsPending.Caption := '';
      lblCurrentDifficulty.Caption := '';
      lblTimeAverage.Caption := '';
      lblTimeAverageAux.Caption := '';
    end;
  end;
  if (Assigned(FPoolMiningServer)) And (FPoolMiningServer.Active) then begin
    If FPoolMiningServer.ClientsCount>0 then begin
      FrameInfo.lblMinersClients.Caption := IntToStr(FPoolMiningServer.ClientsCount)+' connected JSON-RPC clients';
      FrameInfo.lblMinersClients.Font.Color := clNavy;
    end else begin
      FrameInfo.lblMinersClients.Caption := 'No JSON-RPC clients';
      FrameInfo.lblMinersClients.Font.Color := clDkGray;
    end;
    MinersBlocksFound := FPoolMiningServer.ClientsWins;
  end else begin
    MinersBlocksFound := 0;
    FrameInfo.lblMinersClients.Caption := 'JSON-RPC server not active';
    FrameInfo.lblMinersClients.Font.Color := clRed;
  end;
end;

procedure TFRMWallet.UpdateConfigChanged(Sender:TObject);
Var wa : Boolean;
  i : Integer;
  LLockedMempool : TPCOperationsComp;
begin
  tsLogs.TabVisible := TSettings.ShowLogs;
  if (Not tsLogs.TabVisible) then begin
    FLog.OnNewLog := Nil;
    if PageControl.ActivePage = tsLogs then PageControl.ActivePage := tsMyAccounts;
  end else FLog.OnNewLog := OnNewLog;
  if TSettings.SaveLogFiles then begin
    if TSettings.SaveDebugLogs then FLog.SaveTypes := CT_TLogTypes_ALL
    else FLog.SaveTypes := CT_TLogTypes_DEFAULT;
    FLog.FileName := TNode.GetPascalCoinDataFolder+PathDelim+'PascalCointWallet.log';
  end else begin
    FLog.SaveTypes := [];
    FLog.FileName := '';
  end;
  if Assigned(FNode) then begin
    wa := FNode.NetServer.Active;
    FNode.NetServer.Port := TSettings.InternetServerPort;
    FNode.NetServer.Active := wa;
    LLockedMempool := FNode.LockMempoolWrite;
    try
      LLockedMempool.BlockPayload := TEncoding.ANSI.GetBytes(TSettings.MinerName);
    finally
      FNode.UnlockMempoolWrite;
    end;
    FNode.NodeLogFilename := TNode.GetPascalCoinDataFolder+PathDelim+'blocks.log';
  end;
  if Assigned(FPoolMiningServer) then begin
    if FPoolMiningServer.Port<>TSettings.JsonRpcMinerServerPort then begin
      FPoolMiningServer.Active := false;
      FPoolMiningServer.Port := TSettings.JsonRpcMinerServerPort;
    end;
    FPoolMiningServer.Active := TSettings.JsonRpcMinerServerActive;
    FPoolMiningServer.UpdateAccountAndPayload(GetAccountKeyForMiner,TEncoding.ANSI.GetBytes(TSettings.MinerName));
  end;
  if Assigned(FRPCServer) then begin
    FRPCServer.Active := TSettings.JsonRpcPortEnabled;
    FRPCServer.ValidIPs := TSettings.JsonRpcAllowedIPs;
  end;
  i := Integer(TSettings.MinerPrivateKeyType);
  if (i>=Integer(Low(TMinerPrivateKeyType))) And (i<=Integer(High(TMinerPrivateKeyType))) then FMinerPrivateKeyType := TMinerPrivateKeyType(i)
  else FMinerPrivateKeyType := mpk_Random;
  FrameBlockChainExplorer.ebHashRateBackBlocks.Text := IntToStr(FBlockChainGrid.HashRateAverageBlocksCount);

  FrameBlockChainExplorer.ChangeHashRateUnits( FBlockChainGrid.HashRateAs );

  if TNetData.NetDataExists then begin
    if TSettings.AppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].GetAsBoolean(TNetData.NetData.MinFutureBlocksToDownloadNewSafebox>200) then begin
      TNetData.NetData.MinFutureBlocksToDownloadNewSafebox:=TSettings.AppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewSafebox].GetAsInteger(TNetData.NetData.MinFutureBlocksToDownloadNewSafebox);
    end else TNetData.NetData.MinFutureBlocksToDownloadNewSafebox:=0;
  end;
end;

procedure TFRMWallet.UpdateConnectionStatus;
var errors : String;
begin
  FrameInfo.UpdateNodeStatus;
  OnNetStatisticsChanged(Nil);
  if Assigned(FNode) then begin
    if FNode.IsBlockChainValid(errors) then begin
      StatusBar.Panels[2].Text := Format('Last account time:%s',
       [FormatDateTime('dd/mm/yyyy hh:nn:ss',UnivDateTime2LocalDateTime(UnixToUnivDateTime( FNode.Bank.LastOperationBlock.timestamp )))]);
    end else begin
      StatusBar.Panels[2].Text := 'NO BLOCKCHAIN: '+errors;
    end;
  end else begin
    StatusBar.Panels[2].Text := '';
  end;
end;



procedure TFRMWallet.UpdateOperations;
Var accn : Int64;
begin
  accn := FrameAccountExplorer.AccountsGrid.AccountNumber(FrameAccountExplorer.dgAccounts.Row);
  FOperationsAccountGrid.AccountNumber := accn;
end;





initialization
  FRMWallet := Nil;
end.
