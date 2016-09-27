unit UFRMWallet;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, ExtCtrls, ComCtrls, UWalletKeys, ShlObj, ADOInt, StdCtrls,
  ULog, DB, ADODB, Grids, DBGrids, DBCGrids, UAppParams,
  UBlockChain, UNode, DBCtrls, UGridUtils, UDBGridUtils, UMiner, UAccounts, Menus, ImgList,
  AppEvnts, UNetProtocol, UCrypto, Buttons;

Const
  CT_PARAM_GridAccountsStream = 'GridAccountsStream';
  CT_PARAM_GridAccountsPos = 'GridAccountsPos';

  CT_PARAM_DefaultFee = 'DefaultFee';
  CT_PARAM_InternetServerPort = 'InternetServerPort';
  CT_PARAM_AutomaticMineWhenConnectedToNodes = 'AutomaticMineWhenConnectedToNodes';
  CT_PARAM_MinerPrivateKeyType = 'MinerPrivateKeyType';
  CT_PARAM_MinerPrivateKeySelectedPublicKey = 'MinerPrivateKeySelectedPublicKey';
  CT_PARAM_SaveLogFiles = 'SaveLogFiles';
  CT_PARAM_SaveDebugLogs = 'SaveDebugLogs';
  CT_PARAM_ShowLogs = 'ShowLogs';
  CT_PARAM_MinerName = 'MinerName';
  CT_PARAM_FirstTime = 'FirstTime';
  CT_PARAM_ShowModalMessages = 'ShowModalMessages';
  CT_PARAM_MaxCPUs = 'MaxCPUs';
  CT_PARAM_PeerCache = 'PeerCache';
  CT_PARAM_TryToConnectOnlyWithThisFixedServers = 'TryToConnectOnlyWithFixedServers';

type
  TStringListAux = Class(TStringList)

  End;

  TMinerPrivateKey = (mpk_NewEachTime, mpk_Random, mpk_Selected);

  TFRMWallet = class(TForm)
    pnlTop: TPanel;
    Image1: TImage;
    StatusBar: TStatusBar;
    PageControl: TPageControl;
    tsAccountsExplorer: TTabSheet;
    tsOperations: TTabSheet;
    TrayIcon: TTrayIcon;
    TimerUpdateStatus: TTimer;
    tsLogs: TTabSheet;
    pnlTopLogs: TPanel;
    cbShowDebugLogs: TCheckBox;
    memoLogs: TMemo;
    pnlMyAccountsTop: TPanel;
    dgAccounts: TDrawGrid;
    cbMyPrivateKeys: TComboBox;
    Splitter1: TSplitter;
    MainMenu: TMainMenu;
    miProject: TMenuItem;
    miOptions: TMenuItem;
    miPrivatekeys: TMenuItem;
    miN1: TMenuItem;
    miAbout: TMenuItem;
    miAboutPascalCoin: TMenuItem;
    miNewOperation: TMenuItem;
    Panel1: TPanel;
    Label1: TLabel;
    dbgridOperations: TDBGrid;
    cbAllowMining: TCheckBox;
    ebFilterOperationsAccount: TEdit;
    Label2: TLabel;
    ebFilterOperationsStartBlock: TEdit;
    ebFilterOperationsEndBlock: TEdit;
    dtpFilterOperationsDateStart: TDateTimePicker;
    cbFilterOperationsByDate: TCheckBox;
    dtpFilterOperationsDateEnd: TDateTimePicker;
    tsNodeStats: TTabSheet;
    memoNetConnections: TMemo;
    memoNetServers: TMemo;
    memoNetBlackLists: TMemo;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblCurrentBlockCaption: TLabel;
    lblCurrentBlock: TLabel;
    lblCurrentBlockTimeCaption: TLabel;
    lblCurrentBlockTime: TLabel;
    lblOperationsPendingCaption: TLabel;
    lblOperationsPending: TLabel;
    lblMiningStatusCaption: TLabel;
    lblMiningStatus: TLabel;
    lblCurrentDifficultyCaption: TLabel;
    lblCurrentDifficulty: TLabel;
    lblTimeAverage: TLabel;
    Label4: TLabel;
    tsBlockChain: TTabSheet;
    Panel2: TPanel;
    Label9: TLabel;
    ebBlockChainBlockStart: TEdit;
    ebBlockChainBlockEnd: TEdit;
    dtpBlockChainDateStart: TDateTimePicker;
    cbBlockChainFilterByDate: TCheckBox;
    dtpBlockChainDateEnd: TDateTimePicker;
    dbGridBlockChain: TDBGrid;
    Label8: TLabel;
    lblNodeStatus: TLabel;
    tsPendingOperations: TTabSheet;
    dgPendingOperations: TDrawGrid;
    pnlPendingOperations: TPanel;
    Label10: TLabel;
    cbExploreMyAccounts: TCheckBox;
    N1: TMenuItem;
    MiClose: TMenuItem;
    MiDecodePayload: TMenuItem;
    ImageListIcons: TImageList;
    ApplicationEvents: TApplicationEvents;
    Label5: TLabel;
    lblCurrentAccounts: TLabel;
    lblTimeAverageAux: TLabel;
    tsMessages: TTabSheet;
    lbNetConnections: TListBox;
    bbSendAMessage: TButton;
    Label11: TLabel;
    memoMessages: TMemo;
    memoMessageToSend: TMemo;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    lblBlocksFound: TLabel;
    pnlAccounts: TPanel;
    pnlAccountsInfo: TPanel;
    Label17: TLabel;
    Label19: TLabel;
    lblAccountsCount: TLabel;
    lblAccountsBalance: TLabel;
    lblReceivedMessages: TLabel;
    lblBuild: TLabel;
    ebFindAccountNumber: TEdit;
    Label18: TLabel;
    IPnodes1: TMenuItem;
    bbChangeKeyName: TBitBtn;
    pcAccountsOptions: TPageControl;
    tsAccountOperations: TTabSheet;
    dgAccountOperations: TDrawGrid;
    tsMultiSelectAccounts: TTabSheet;
    dgSelectedAccounts: TDrawGrid;
    pnlSelectedAccountsTop: TPanel;
    pnlSelectedAccountsBottom: TPanel;
    pnlSelectedAccountsLeft: TPanel;
    sbSelectedAccountsAdd: TSpeedButton;
    sbSelectedAccountsAddAll: TSpeedButton;
    sbSelectedAccountsDel: TSpeedButton;
    sbSelectedAccountsDelAll: TSpeedButton;
    Label20: TLabel;
    lblSelectedAccountsCount: TLabel;
    Label22: TLabel;
    lblSelectedAccountsBalance: TLabel;
    bbSelectedAccountsOperation: TBitBtn;
    Label15: TLabel;
    MiOperations: TMenuItem;
    MiAddaccounttoSelected: TMenuItem;
    MiRemoveaccountfromselected: TMenuItem;
    N2: TMenuItem;
    MiMultiaccountoperation: TMenuItem;
    N3: TMenuItem;
    MiFindnextaccountwithhighbalance: TMenuItem;
    MiFindpreviousaccountwithhighbalance: TMenuItem;
    MiFindaccount: TMenuItem;
    cbFilterAccounts: TCheckBox;
    ebFilterAccountByBalanceMin: TEdit;
    ebFilterAccountByBalanceMax: TEdit;
    bbAccountsRefresh: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerUpdateStatusTimer(Sender: TObject);
    procedure cbMyPrivateKeysChange(Sender: TObject);
    procedure dgAccountsClick(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    procedure miAboutPascalCoinClick(Sender: TObject);
    procedure miNewOperationClick(Sender: TObject);
    procedure miPrivatekeysClick(Sender: TObject);
    procedure dgAccountsColumnMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure dgAccountsFixedCellClick(Sender: TObject; ACol, ARow: Integer);
    procedure PageControlChange(Sender: TObject);
    procedure ebFilterOperationsAccountExit(Sender: TObject);
    procedure ebFilterOperationsAccountKeyPress(Sender: TObject; var Key: Char);
    procedure cbFilterOperationsByDateClick(Sender: TObject);
    procedure ebBlockChainBlockStartExit(Sender: TObject);
    procedure ebBlockChainBlockStartKeyPress(Sender: TObject; var Key: Char);
    procedure cbBlockChainFilterByDateClick(Sender: TObject);
    procedure cbAllowMiningClick(Sender: TObject);
    procedure cbExploreMyAccountsClick(Sender: TObject);
    procedure MiCloseClick(Sender: TObject);
    procedure MiDecodePayloadClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure ApplicationEventsMinimize(Sender: TObject);
    procedure bbSendAMessageClick(Sender: TObject);
    procedure lblReceivedMessagesClick(Sender: TObject);
    procedure ebFindAccountNumberChange(Sender: TObject);
    procedure ebFindAccountNumberExit(Sender: TObject);
    procedure IPnodes1Click(Sender: TObject);
    procedure bbChangeKeyNameClick(Sender: TObject);
    procedure sbSelectedAccountsAddClick(Sender: TObject);
    procedure sbSelectedAccountsAddAllClick(Sender: TObject);
    procedure sbSelectedAccountsDelClick(Sender: TObject);
    procedure sbSelectedAccountsDelAllClick(Sender: TObject);
    procedure bbSelectedAccountsOperationClick(Sender: TObject);
    procedure MiAddaccounttoSelectedClick(Sender: TObject);
    procedure MiRemoveaccountfromselectedClick(Sender: TObject);
    procedure MiMultiaccountoperationClick(Sender: TObject);
    procedure MiFindnextaccountwithhighbalanceClick(Sender: TObject);
    procedure MiFindpreviousaccountwithhighbalanceClick(Sender: TObject);
    procedure MiFindaccountClick(Sender: TObject);
    procedure bbAccountsRefreshClick(Sender: TObject);
    procedure ebFilterAccountByBalanceMinExit(Sender: TObject);
    procedure ebFilterAccountByBalanceMinKeyPress(Sender: TObject;
      var Key: Char);
    procedure cbFilterAccountsClick(Sender: TObject);
  private
    FMinersBlocksFound: Integer;
    procedure SetMinersBlocksFound(const Value: Integer);
    Procedure CheckIsReady;
  protected
    { Private declarations }
    FNode : TNode;
    FIsActivated : Boolean;
    FWalletKeys : TWalletKeys;
    FLog : TLog;
    FMaxCPUs : Integer;
    FAppParams : TAppParams;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FAccountsGrid : TAccountsGrid;
    FSelectedAccountsGrid : TAccountsGrid;
    FOperationsGrid : TOperationsGrid;
    FPendingOperationsGrid : TOperationsGrid;
    FOrderedAccountsKeyList : TOrderedAccountKeysList;
    FOperationsDBGrid : TOperationsDBGrid;
    FBlockChainDBGrid : TBlockChainDBGrid;
    FMinerPrivateKeyType : TMinerPrivateKey;
    FUpdating : Boolean;
    FMessagesUnreadCount : Integer;
    FMinAccountBalance : Int64;
    FMaxAccountBalance : Int64;
    Procedure CheckMining;
    Procedure OnNewAccount(Sender : TObject);
    Procedure OnReceivedHelloMessage(Sender : TObject);
    Procedure OnNetStatisticsChanged(Sender : TObject);
    procedure OnNewLog(logtype : TLogType; Time : TDateTime; ThreadID : Cardinal; Const sender, logtext : AnsiString);
    procedure OnMinerNewBlockFound(sender : TMinerThread; Operations : TPCOperationsComp);
    procedure OnWalletChanged(Sender : TObject);
    procedure OnNetConnectionsUpdated(Sender : TObject);
    procedure OnNetNodeServersUpdated(Sender : TObject);
    procedure OnNetBlackListUpdated(Sender : TObject);
    Procedure OnNodeMessageEvent(NetConnection : TNetConnection; MessageData : TRawBytes);
    Procedure OnSelectedAccountsGridUpdated(Sender : TObject);
    Procedure UpdateConnectionStatus;
    Procedure UpdateAccounts(RefreshData : Boolean);
    Procedure UpdateBlockChainState;
    Procedure UpdatePrivateKeys;
    Procedure UpdateOperations;
    Procedure LoadAppParams;
    Procedure SaveAppParams;
    Procedure UpdateConfigChanged;
    Procedure UpdateNodeStatus;
    Procedure UpdateAvailableConnections;
    procedure Activate; override;
    Function ForceMining : Boolean; virtual;
    Function GetAccountKeyForMiner : TAccountKey;
    Procedure DoUpdateAccounts;
    Function DoUpdateAccountsFilter : Boolean;
  public
    { Public declarations }
    Property WalletKeys : TWalletKeys read FWalletKeys;
    Property MinersBlocksFound : Integer read FMinersBlocksFound write SetMinersBlocksFound;
  end;

var
  FRMWallet: TFRMWallet;

implementation

{$R *.dfm}

Uses UFolderHelper, ssl_lib, UConst, UTime,
  UDBStorage, UThread, UOpTransaction, UECIES, UFRMPascalCoinWalletConfig,
  UFRMAbout, UFRMOperation, UFRMWalletKeys, UFRMPayloadDecoder, UFRMNodesIp;

Type
  TThreadActivate = Class(TPCThread)
  protected
    procedure BCExecute; override;
  End;

{ TThreadActivate }

procedure TThreadActivate.BCExecute;
begin
  // Read Operations saved from disk
  TNode.Node.Bank.DiskRestoreFromOperations(CT_MaxBlock);
  TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  TNode.Node.NetServer.Active := true;
  Synchronize( FRMWallet.DoUpdateAccounts );
end;

{ TFRMWallet }

procedure TFRMWallet.Activate;
Var ips : AnsiString;
  nsarr : TNodeServerAddressArray;
begin
  inherited;
  if FIsActivated then exit;
  FIsActivated := true;
  try
    // Check OpenSSL dll
    if Not LoadSSLCrypt then raise Exception.Create('Cannot load '+SSL_C_LIB+#10+'To use this software make sure this file is available on you system or reinstall the application');
    TCrypto.InitCrypto;
    // Read Wallet
    Try
      FWalletKeys.WalletFileName := TFolderHelper.GetPascalCoinDataFolder+'\WalletKeys.dat';
    Except
      On E:Exception do begin
        E.Message := 'Cannot open your wallet... Perhaps another instance of Pascal Coin is active!'+#10+#10+E.Message;
        Raise;
      end;
    End;
    ips := FAppParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].GetAsString('');
    TNode.DecodeIpStringToNodeServerAddressArray(ips,nsarr);
    TNetData.NetData.DiscoverFixedServersOnly(nsarr);
    setlength(nsarr,0);
    // Creating Node:
    FNode := TNode.Node;
    FNode.NetServer.Port := FAppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
    FNode.PeerCache := FAppParams.ParamByName[CT_PARAM_PeerCache].GetAsString('')+';'+CT_Discover_IPs;
    // Check Database
    FNode.Bank.StorageClass := TDBStorage;
    TDBStorage(FNode.Bank.Storage).AccessFileName := TFolderHelper.GetPascalCoinDataFolder+'\pascalcoinB03.mdb';
    // Init Grid
    FAccountsGrid.Node := FNode;
    FSelectedAccountsGrid.Node := FNode;
    FWalletKeys.OnChanged := OnWalletChanged;
    FOperationsGrid.Node := FNode;
    FPendingOperationsGrid.Node := FNode;
    // Reading database
    TThreadActivate.Create(false).FreeOnTerminate := true;
    FNodeNotifyEvents.Node := FNode;
    FOperationsDBGrid.Node := FNode;
    FOperationsDBGrid.AdoConnection := TDBStorage(FNode.Bank.Storage).ADOConnection;
    FOperationsDBGrid.DBGrid := dbgridOperations;
    FBlockChainDBGrid.Node := FNode;
    FBlockChainDBGrid.AdoConnection := TDBStorage(FNode.Bank.Storage).ADOConnection;
    FBlockChainDBGrid.DBGrid := dbGridBlockChain;
    // Init
    TNetData.NetData.OnReceivedHelloMessage := OnReceivedHelloMessage;
    TNetData.NetData.OnStatisticsChanged := OnNetStatisticsChanged;
    TNetData.NetData.OnNetConnectionsUpdated := onNetConnectionsUpdated;
    TNetData.NetData.OnNodeServersUpdated := OnNetNodeServersUpdated;
    TNetData.NetData.OnBlackListUpdated := OnNetBlackListUpdated;
    //
    TimerUpdateStatus.Enabled := true;
    UpdateConfigChanged;
  Except
    On E:Exception do begin
      E.Message := 'An error occurred during initialization. Application cannot continue:'+#10+#10+E.Message+#10+#10+'Application will close...';
      Application.MessageBox(PChar(E.Message),PChar(Application.Title),MB_ICONERROR+MB_OK);
      Halt;
    end;
  end;
  UpdatePrivateKeys;
  UpdateAccounts(false);
  if FAppParams.ParamByName[CT_PARAM_FirstTime].GetAsBoolean(true) then begin
    FAppParams.ParamByName[CT_PARAM_FirstTime].SetAsBoolean(false);
    miAboutPascalCoinClick(Nil);
  end;

end;

procedure TFRMWallet.ApplicationEventsMinimize(Sender: TObject);
begin
  Hide();
  WindowState := wsMinimized;
  { Show the animated tray icon and also a hint balloon. }
  TrayIcon.Visible := True;
  TrayIcon.ShowBalloonHint;
end;

procedure TFRMWallet.bbAccountsRefreshClick(Sender: TObject);
begin
  UpdateAccounts(true);
end;

procedure TFRMWallet.bbChangeKeyNameClick(Sender: TObject);
var i : Integer;
  name : String;
begin
  if (cbMyPrivateKeys.ItemIndex<0) then  exit;
  i := Integer(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
  if (i<0) Or (i>=FWalletKeys.Count) then raise Exception.Create('Must select a Key');
  name := FWalletKeys.Key[i].Name;
  if InputQuery('Change Key name','Input new name',name) then begin
    FWalletKeys.SetName(i,name);
  end;
  UpdatePrivateKeys;
end;

procedure TFRMWallet.bbSelectedAccountsOperationClick(Sender: TObject);
var l : TOrderedCardinalList;
begin
  CheckIsReady;
  if FSelectedAccountsGrid.AccountsCount<=0 then raise Exception.Create('Must select at least 1 account');
  With TFRMOperation.Create(Self) do
  Try
    l := FSelectedAccountsGrid.LockAccountsList;
    try
      SenderAccounts.CopyFrom(l);
    finally
      FSelectedAccountsGrid.UnlockAccountsList;
    end;
    Fee := FAppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0);
    WalletKeys := FWalletKeys;
    ShowModal;
  Finally
    Free;
  End;
end;

procedure TFRMWallet.bbSendAMessageClick(Sender: TObject);
Var basem,m : String;
  them, errors : AnsiString;
  i,n : Integer;
  nc : TNetConnection;
begin
  CheckIsReady;
  if (lbNetConnections.SelCount<=0) Or (lbNetConnections.ItemIndex<0) then raise Exception.Create('Select at least one connection');
  if lbNetConnections.SelCount<=0 then n := 1
  else n := lbNetConnections.SelCount;

  basem := memoMessageToSend.Lines.Text;
  m := '';
  // Clear non valid characters:
  for i := 1 to length(basem) do begin
    if basem[i] in [#32..#127] then m := m + basem[i]
    else m:=m+'.';
  end;

  if trim(m)='' then raise Exception.Create('No message');

  if Application.MessageBox(PChaR('Send this message to '+inttostr(n)+' nodes?'+#10+
    'NOTE: Sending unauthorized messages will be considered spam and you will be banned'+#10+
    #10+
    'Message: '+#10+
    m),PChar(Application.Title),MB_ICONQUESTION+MB_YESNO+MB_DEFBUTTON1)<>IdYes then exit;
  them := m;
  if n>1 then begin
    for i := 0 to lbNetConnections.Items.Count - 1 do begin
      if lbNetConnections.Selected[i] then begin
        nc := TNetConnection(lbNetconnections.Items.Objects[i]);
        if TNetData.NetData.ConnectionExistsAndActive(nc) then begin
          FNode.SendNodeMessage(nc,m,errors);
          memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.ClientRemoteAddr+' > '+m);
        end;
      end;
    end;
  end else begin
    nc := TNetConnection(lbNetconnections.Items.Objects[lbNetconnections.ItemIndex]);
    if TNetData.NetData.ConnectionExistsAndActive(nc) then begin
      FNode.SendNodeMessage(nc,m,errors);
      memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.ClientRemoteAddr+' > '+m);
    end;
  end;

  Application.MessageBox(PChaR('Message sent to '+inttostr(n)+' nodes'+#10+
    'Message: '+#10+m),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
end;

procedure TFRMWallet.cbAllowMiningClick(Sender: TObject);
begin
  if Not Assigned(FNode) then exit;
  if cbAllowMining.Checked then begin
    if (TNetData.NetData.NetStatistics.ClientsConnections<=0) then begin
      Application.MessageBox(PChar(Format(
        'In order to mine is necessary that you open your external port %d from the Internet to allow other Pascal Coin nodes to connect to you.'+#10+
        '(Note: This is not mandatory... but it will work better with open ports)'+#10+
        #10+
        'To do this you must configure your Router/Firewall and enable NAT to your local machine at port: %d'+#10+#10+
        'After allowing incoming connections... you must wait until other nodes connect to you to mine'+#10+
        #10+
        'PLEASE... BE PATIENT !!!'+#10+#10+
        'Help mining Pascal Coin and win Pascal Coins!',[FNode.NetServer.Port,FNode.NetServer.Port])),
        PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
    end;
  end;
end;

procedure TFRMWallet.cbBlockChainFilterByDateClick(Sender: TObject);
begin
  dtpBlockChainDateStart.Enabled := cbBlockChainFilterByDate.Checked;
  dtpBlockChainDateEnd.Enabled := cbBlockChainFilterByDate.Checked;
  ebBlockChainBlockStartExit(Nil);
end;

procedure TFRMWallet.cbExploreMyAccountsClick(Sender: TObject);
begin
  cbMyPrivateKeys.Enabled := cbExploreMyAccounts.Checked;
  UpdateAccounts(true);
  UpdateOperations;
end;

procedure TFRMWallet.cbFilterAccountsClick(Sender: TObject);
begin
  If not DoUpdateAccountsFilter then UpdateAccounts(true);
end;

procedure TFRMWallet.cbFilterOperationsByDateClick(Sender: TObject);
begin
  dtpFilterOperationsDateStart.Enabled := cbFilterOperationsByDate.Checked;
  dtpFilterOperationsDateEnd.Enabled := cbFilterOperationsByDate.Checked;
  ebFilterOperationsAccountExit(Nil);
end;

procedure TFRMWallet.cbMyPrivateKeysChange(Sender: TObject);
begin
  UpdateAccounts(true);
end;

procedure TFRMWallet.CheckIsReady;
Var isready : AnsiString;
begin
  if Not Assigned(FNode) then Abort;

  if Not FNode.Node.IsReady(isready) then begin
    Raise Exception.Create('You cannot do this operation now:'+#10+#10+isready);
  end;
end;

procedure TFRMWallet.CheckMining;
  Procedure Stop;
  var i : Integer;
    mtl : TList;
  begin
    if ForceMining then exit;
    // Stop mining
    mtl := FNode.MinerThreads.LockList;
    try
      for i:=mtl.Count-1 downto 0 do begin
        if Not TMinerThread(mtl[i]).Paused then begin
          TLog.NewLog(ltinfo,ClassName,'Stoping miner');
        end;
        TMinerThread(mtl[i]).Paused := true;
      end;
    finally
      FNode.MinerThreads.UnlockList;
    end;
  end;
Var i, n : Integer;
  MT : TMinerThread;
  mtl : TList;
begin
  if Not Assigned(FNode) then exit;
  if (ForceMining) Or
    (
      (TNetData.NetData.NetStatistics.ActiveConnections>0) And
      // Build 1.0.2 allows mine if there was at least 1 client connection (working as a server)
      // or (new) there are 2 active connections to a servers (allowing non servers to mine too)
      ( (TNetData.NetData.NetStatistics.TotalClientsConnections>0)
        Or (TNetData.NetData.NetStatistics.ServersConnectionsWithResponse>=2) ) And
      (TNetData.NetData.MaxRemoteOperationBlock.block<=FNode.Operations.OperationBlock.block)
    ) then begin
    if (cbAllowMining.checked) Or (ForceMining) then begin
      n := 0;
      mtl := FNode.MinerThreads.LockList;
      try
        for i:=mtl.Count-1 downto 0 do begin
          if TMinerThread(mtl[i]).Paused then begin
            TMinerThread(mtl[i]).Paused := false;
            TLog.NewLog(ltinfo,ClassName,'Starting miner');
          end;
          inc(n);
        end;
        if n<FMaxCPUs then begin
          MT := FNode.AddMiner(GetAccountKeyForMiner);
          MT.OnNewAccountFound := OnMinerNewBlockFound;
          MT.Paused := false;
        end else begin
          while (mtl.Count>FMaxCPUs) do FNode.DeleteMiner(mtl.Count-1);
        end;
      Finally
        FNode.MinerThreads.UnlockList;
      End;
    end else begin
      Stop;
    end;
  end else Stop;
end;

procedure TFRMWallet.dgAccountsClick(Sender: TObject);
begin
  UpdateOperations;
end;

procedure TFRMWallet.dgAccountsColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  SaveAppParams;
end;

procedure TFRMWallet.dgAccountsFixedCellClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  SaveAppParams;
end;

procedure TFRMWallet.DoUpdateAccounts;
begin
  UpdateAccounts(true);
end;

Function TFRMWallet.DoUpdateAccountsFilter : Boolean;
Var m,bmin,bmax:Int64;
  doupd : Boolean;
begin
  if FUpdating then exit;
  FUpdating := true;
  Try
    If Not TAccountComp.TxtToMoney(ebFilterAccountByBalanceMin.Text,bmin) then bmin := 0;
    If not TAccountComp.TxtToMoney(ebFilterAccountByBalanceMax.Text,bmax) then bmax := CT_MaxWalletAmount;
    if (bmax<bmin) or (bmax=0) then bmax := CT_MaxWalletAmount;
    if bmin>bmax then bmin := 0;
    doupd := (bmin<>FMinAccountBalance) Or (bmax<>FMaxAccountBalance);
    FMinAccountBalance := bmin;
    FMaxAccountBalance := bmax;
    if bmin>0 then
      ebFilterAccountByBalanceMin.Text:=TAccountComp.FormatMoney(bmin)
    else ebFilterAccountByBalanceMin.Text := '';
    if bmax<CT_MaxWalletAmount then
      ebFilterAccountByBalanceMax.Text := TAccountComp.FormatMoney(bmax)
    else ebFilterAccountByBalanceMax.Text := '';
    if cbFilterAccounts.Checked then begin
      ebFilterAccountByBalanceMin.ParentFont := true;
      ebFilterAccountByBalanceMax.ParentFont := true;
    end else begin
      ebFilterAccountByBalanceMin.font.Color := clDkGray;
      ebFilterAccountByBalanceMax.font.Color := clDkGray;
    end;
  Finally
    FUpdating := false;
  End;
  if doupd then UpdateAccounts(true);
  Result := doupd;
end;

procedure TFRMWallet.ebBlockChainBlockStartExit(Sender: TObject);
var i64 : Int64;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    FBlockChainDBGrid.Disable;
    try
      i64 := StrToInt64Def(ebBlockChainBlockStart.Text,-1);
      FBlockChainDBGrid.BlockStart := i64;
      if i64>=0 then ebBlockChainBlockStart.Text := Inttostr(i64) else ebBlockChainBlockStart.Text := '';
      i64 := StrToInt64Def(ebBlockChainBlockEnd.Text,-1);
      FBlockChainDBGrid.BlockEnd := i64;
      if i64>=0 then ebBlockChainBlockEnd.Text := Inttostr(i64) else ebBlockChainBlockEnd.Text := '';
      if cbBlockChainFilterByDate.Checked then begin
        if dtpBlockChainDateStart.Date<encodedate(2016,01,01) then dtpBlockChainDateStart.Date := encodedate(2016,01,01);
        if dtpBlockChainDateStart.Date>dtpBlockChainDateEnd.Date then dtpBlockChainDateEnd.Date := dtpBlockChainDateStart.Date;
        FBlockChainDBGrid.SetDates(dtpBlockChainDateStart.Date,dtpBlockChainDateEnd.Date);
        dtpBlockChainDateStart.Date := FBlockChainDBGrid.DateStart;
        dtpBlockChainDateEnd.Date := FBlockChainDBGrid.DateEnd;
      end else begin
        FBlockChainDBGrid.SetDates(0,0);
      end;
    finally
      FBlockChainDBGrid.Enable;
    end;
  Finally
    FUpdating := false;
  End;
end;

procedure TFRMWallet.ebBlockChainBlockStartKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then  ebBlockChainBlockStartExit(Nil);
end;

procedure TFRMWallet.ebFilterAccountByBalanceMinExit(Sender: TObject);
begin
  DoUpdateAccountsFilter;
end;

procedure TFRMWallet.ebFilterAccountByBalanceMinKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then DoUpdateAccountsFilter;
end;

procedure TFRMWallet.ebFilterOperationsAccountExit(Sender: TObject);
Var acc : Cardinal;
  i64 : Int64;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    FOperationsDBGrid.Disable;
    try
      if TAccountComp.AccountTxtNumberToAccountNumber(ebFilterOperationsAccount.Text,acc) then begin
        FOperationsDBGrid.AccountNumber := acc;
        ebFilterOperationsAccount.Text := TAccountComp.AccountNumberToAccountTxtNumber(acc);
      end else begin
        FOperationsDBGrid.AccountNumber := -1;
        ebFilterOperationsAccount.Text := '';
      end;
      i64 := StrToInt64Def(ebFilterOperationsStartBlock.Text,-1);
      FOperationsDBGrid.BlockStart := i64;
      if i64>=0 then ebFilterOperationsStartBlock.Text := Inttostr(i64) else ebFilterOperationsStartBlock.Text := '';
      i64 := StrToInt64Def(ebFilterOperationsEndBlock.Text,-1);
      FOperationsDBGrid.BlockEnd := i64;
      if i64>=0 then ebFilterOperationsEndBlock.Text := Inttostr(i64) else ebFilterOperationsEndBlock.Text := '';
      if cbFilterOperationsByDate.Checked then begin
        if dtpFilterOperationsDateStart.Date<encodedate(2016,01,01) then dtpFilterOperationsDateStart.Date := encodedate(2016,01,01);
        if dtpFilterOperationsDateStart.Date>dtpFilterOperationsDateEnd.Date then dtpFilterOperationsDateEnd.Date := dtpFilterOperationsDateStart.Date;
        FOperationsDBGrid.SetDates(dtpFilterOperationsDateStart.Date,dtpFilterOperationsDateEnd.Date);
        dtpFilterOperationsDateStart.Date := FOperationsDBGrid.DateStart;
        dtpFilterOperationsDateEnd.Date := FOperationsDBGrid.DateEnd;
      end else begin
        FOperationsDBGrid.SetDates(0,0);
      end;
      //
    finally
      FOperationsDBGrid.Enable;
    end;
  Finally
    FUpdating := false;
  End;
end;

procedure TFRMWallet.ebFilterOperationsAccountKeyPress(Sender: TObject;
  var Key: Char);
begin
  if key=#13 then  ebFilterOperationsAccountExit(Nil);
end;

procedure TFRMWallet.ebFindAccountNumberChange(Sender: TObject);
Var an : Cardinal;
begin
  if Trim(ebFindAccountNumber.Text)='' then begin
    ebFindAccountNumber.Color := clWindow;
    ebFindAccountNumber.Font.Color := clDkGray;
  end else if TAccountComp.AccountTxtNumberToAccountNumber(ebFindAccountNumber.Text,an) then begin
    ebFindAccountNumber.Color := clWindow;
    if FAccountsGrid.MoveRowToAccount(an) then begin
      ebFindAccountNumber.Font.Color := clWindowText;
    end else begin
      ebFindAccountNumber.Font.Color := clRed;
    end;
  end else begin
    // Invalid value
    ebFindAccountNumber.Color := clRed;
    ebFindAccountNumber.Font.Color := clWindowText;
  end;
end;

procedure TFRMWallet.ebFindAccountNumberExit(Sender: TObject);
begin
  ebFindAccountNumber.Text := '';
end;

function TFRMWallet.ForceMining: Boolean;
begin
  Result := false;
end;

procedure TFRMWallet.FormCreate(Sender: TObject);
Var i : Integer;
  fvi : TFileVersionInfo;
begin
  if CPUCount>1 then FMaxCPUs := CPUCount-1
  else FMaxCPUs := 1;
  FMinAccountBalance := 0;
  FMaxAccountBalance := CT_MaxWalletAmount;
  FMessagesUnreadCount := 0;
  lblReceivedMessages.Visible := false;
  memoNetConnections.Lines.Clear;
  memoNetServers.Lines.Clear;
  memoNetBlackLists.Lines.Clear;
  memoMessages.Lines.Clear;
  memoMessageToSend.Lines.Clear;
  FNode := Nil;
  FUpdating := false;
  FOrderedAccountsKeyList := Nil;
  TimerUpdateStatus.Enabled := false;
  FIsActivated := false;
  FWalletKeys := TWalletKeys.Create(Self);
  for i := 0 to StatusBar.Panels.Count - 1 do begin
    StatusBar.Panels[i].Text := '';
  end;
  FLog := TLog.Create(Self);
  FLog.OnNewLog := OnNewLog;
  FLog.SaveTypes := [];
  If Not ForceDirectories(TFolderHelper.GetPascalCoinDataFolder) then raise Exception.Create('Cannot create dir: '+TFolderHelper.GetPascalCoinDataFolder);
  FAppParams := TAppParams.Create(self);
  FAppParams.FileName := TFolderHelper.GetPascalCoinDataFolder+'\AppParams.prm';
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Self);
  FNodeNotifyEvents.OnBlocksChanged := OnNewAccount;
  FNodeNotifyEvents.OnNodeMessageEvent := OnNodeMessageEvent;
  FAccountsGrid := TAccountsGrid.Create(Self);
  FAccountsGrid.DrawGrid := dgAccounts;
  FSelectedAccountsGrid := TAccountsGrid.Create(Self);
  FSelectedAccountsGrid.DrawGrid := dgSelectedAccounts;
  FSelectedAccountsGrid.OnUpdated := OnSelectedAccountsGridUpdated;
  FOperationsGrid := TOperationsGrid.Create(Self);
  FOperationsGrid.DrawGrid := dgAccountOperations;
  FPendingOperationsGrid := TOperationsGrid.Create(Self);
  FPendingOperationsGrid.DrawGrid := dgPendingOperations;
  FPendingOperationsGrid.AccountNumber := -1; // all
  FPendingOperationsGrid.PendingOperations := true;
  FWalletKeys.OnChanged := OnWalletChanged;
  FOperationsDBGrid := TOperationsDBGrid.Create(Self);
  FBlockChainDBGrid := TBlockChainDBGrid.Create(Self);
  LoadAppParams;
  UpdatePrivateKeys;
  UpdateBlockChainState;
  UpdateConnectionStatus;
  ebFilterOperationsAccount.Text := '';
  PageControl.ActivePage := tsAccountsExplorer;
  pcAccountsOptions.ActivePage := tsAccountOperations;
  dtpFilterOperationsDateStart.Date := Now;
  dtpFilterOperationsDateEnd.Date := Now;
  ebFilterOperationsAccount.Text := '';
  ebFilterOperationsStartBlock.Text := '';
  ebFilterOperationsEndBlock.Text := '';
  cbFilterOperationsByDate.Checked := false;
  cbFilterOperationsByDateClick(nil);
  cbExploreMyAccountsClick(nil);

  TrayIcon.Visible := true;
  TrayIcon.Hint := Self.Caption;
  TrayIcon.BalloonTitle := 'Restoring the window.';
  TrayIcon.BalloonHint :=
    'Double click the system tray icon to restore Pascal Coin';
  TrayIcon.BalloonFlags := bfInfo;
  MinersBlocksFound := 0;
  fvi := TFolderHelper.GetTFileVersionInfo(Application.ExeName);
  lblBuild.Caption := 'Build: '+fvi.FileVersion;
end;

procedure TFRMWallet.FormDestroy(Sender: TObject);
Var i : Integer;
  MT : TMinerThread;
  step : String;
begin
  TLog.NewLog(ltinfo,Classname,'Destroying form - START');
  Try
  step := 'Saving params';
  SaveAppParams;
  //
  step := 'Assigning nil events';
  FLog.OnNewLog :=Nil;
  FNodeNotifyEvents.Node := Nil;
  FOperationsGrid.Node := Nil;
  FPendingOperationsGrid.Node := Nil;
  FAccountsGrid.Node := Nil;
  FSelectedAccountsGrid.Node := Nil;
  TNetData.NetData.OnReceivedHelloMessage := Nil;
  TNetData.NetData.OnStatisticsChanged := Nil;
  TNetData.NetData.OnNetConnectionsUpdated := Nil;
  TNetData.NetData.OnNodeServersUpdated := Nil;
  TNetData.NetData.OnBlackListUpdated := Nil;
  //

  Repeat
    i := TPCThread.ThreadClassFound(TMinerThread,nil);
    if i>=0 then begin
      step := 'Terminating Miner thread '+inttostr(i);
      MT := TMinerThread( TPCThread.GetThread(i) );
      MT.Paused := false;
      MT.Terminate;
      MT.WaitFor;
    end;
  Until i<0;

  step := 'Destroying NodeNotifyEvents';
  FreeAndNil(FNodeNotifyEvents);
  //
  step := 'Assigning Nil to TNetData';
  TNetData.NetData.OnReceivedHelloMessage := Nil;
  TNetData.NetData.OnStatisticsChanged := Nil;

  step := 'Destroying grids operators';
  FreeAndNil(FOperationsDBGrid);
  FreeAndNil(FBlockChainDBGrid);

  step := 'Ordered Accounts Key list';
  FreeAndNil(FOrderedAccountsKeyList);

  step := 'Desactivating Node';
  TNode.Node.NetServer.Active := false;
  FNode := Nil;


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
end;

function TFRMWallet.GetAccountKeyForMiner: TAccountKey;
Var PK : TECPrivateKey;
  i : Integer;
  PublicK : TECDSA_Public;
begin
  Result := CT_TECDSA_Public_Nul;
  if Not Assigned(FWalletKeys) then exit;
  if Not Assigned(FAppParams) then exit;
  case FMinerPrivateKeyType of
    mpk_NewEachTime: PublicK := CT_TECDSA_Public_Nul;
    mpk_Selected: begin
      PublicK := TAccountComp.RawString2Accountkey(FAppParams.ParamByName[CT_PARAM_MinerPrivateKeySelectedPublicKey].GetAsString(''));
    end;
  else
    // Random
    PublicK := CT_TECDSA_Public_Nul;
    if FWalletKeys.Count>0 then PublicK := FWalletKeys.Key[Random(FWalletKeys.Count)].AccountKey;
  end;
  i := FWalletKeys.IndexOfAccountKey(PublicK);
  if i>=0 then begin
    if (FWalletKeys.Key[i].CryptedKey='') then i:=-1;
  end;
  if i<0 then begin
    PK := TECPrivateKey.Create;
    try
      PK.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
      FWalletKeys.AddPrivateKey('New for miner '+DateTimeToStr(Now), PK);
      PublicK := PK.PublicKey;
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
    FRM.AppParams := FAppParams;
    FRM.ShowModal;
  Finally
    FRM.Free;
  End;
end;

procedure TFRMWallet.lblReceivedMessagesClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMessages;
end;

procedure TFRMWallet.LoadAppParams;
Var ms : TMemoryStream;
  s : AnsiString;
  fvi : TFileVersionInfo;
begin
  ms := TMemoryStream.Create;
  Try
    s := FAppParams.ParamByName[CT_PARAM_GridAccountsStream].GetAsString('');
    ms.WriteBuffer(s[1],length(s));
    ms.Position := 0;
    FAccountsGrid.LoadFromStream(ms);
  Finally
    ms.Free;
  End;
  If FAppParams.FindParam(CT_PARAM_MinerName)=Nil then begin
    // New configuration... assigning a new random value
    fvi := TFolderHelper.GetTFileVersionInfo(Application.ExeName);
    FAppParams.ParamByName[CT_PARAM_MinerName].SetAsString('New Node '+DateTimeToStr(Now)+' - '+
      fvi.InternalName+' Build:'+fvi.FileVersion);
  end;
  UpdateConfigChanged;
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

procedure TFRMWallet.MiAddaccounttoSelectedClick(Sender: TObject);
begin
  PageControl.ActivePage := tsAccountsExplorer;
  PageControlChange(Nil);
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  sbSelectedAccountsAddClick(Sender);
end;

procedure TFRMWallet.MiCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFRMWallet.MiDecodePayloadClick(Sender: TObject);
begin
  if PageControl.ActivePage=tsOperations then begin
    FOperationsDBGrid.ShowModalDecoder(FWalletKeys,FAppParams);
  end else if PageControl.ActivePage=tsPendingOperations then begin
    FPendingOperationsGrid.ShowModalDecoder(FWalletKeys,FAppParams);
  end else if PageControl.ActivePage=tsAccountsExplorer then begin
    FOperationsGrid.ShowModalDecoder(FWalletKeys,FAppParams);
  end;
end;

procedure TFRMWallet.MiFindaccountClick(Sender: TObject);
begin
  PageControl.ActivePage := tsAccountsExplorer;
  PageControlChange(Nil);
  ebFindAccountNumber.SetFocus;
end;

procedure TFRMWallet.MiFindnextaccountwithhighbalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
  PageControl.ActivePage := tsAccountsExplorer;
  PageControlChange(Nil);
  an64 := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if an64<0 then an := 0
  else an := an64;
  If an>=FNode.Bank.SafeBox.AccountsCount then exit;
  start := FNode.Bank.SafeBox.Account(an);
  while (an<FNode.Bank.SafeBox.AccountsCount)  do begin
    if FNode.Bank.SafeBox.Account(an).balance>start.balance then break
    else inc(an);
  end;
  if (an<FNode.Bank.SafeBox.AccountsCount) then FAccountsGrid.MoveRowToAccount(an)
  else raise Exception.Create('Not found any account higher than '+TAccountComp.AccountNumberToAccountTxtNumber(start.account)+' with balance higher than '+
    TAccountComp.FormatMoney(start.balance));
end;

procedure TFRMWallet.MiFindpreviousaccountwithhighbalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
  PageControl.ActivePage := tsAccountsExplorer;
  PageControlChange(Nil);
  an64 := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if an64<0 then an := FNode.Bank.SafeBox.AccountsCount-1
  else an := an64;
  If an>=FNode.Bank.SafeBox.AccountsCount then exit;
  start := FNode.Bank.SafeBox.Account(an);
  while (an>0)  do begin
    if FNode.Bank.SafeBox.Account(an).balance>start.balance then break
    else dec(an);
  end;
  if (FNode.Bank.SafeBox.Account(an).balance>start.balance) then FAccountsGrid.MoveRowToAccount(an)
  else raise Exception.Create('Not found any account lower than '+TAccountComp.AccountNumberToAccountTxtNumber(start.account)+' with balance higher than '+
    TAccountComp.FormatMoney(start.balance));
end;

procedure TFRMWallet.MiMultiaccountoperationClick(Sender: TObject);
begin
  PageControl.ActivePage := tsAccountsExplorer;
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  bbSelectedAccountsOperationClick(Sender);
end;

procedure TFRMWallet.miNewOperationClick(Sender: TObject);
begin
  CheckIsReady;
  With TFRMOperation.Create(Self) do
  Try
    SenderAccounts.Add( FAccountsGrid.AccountNumber(dgAccounts.Row) );
    Fee := FAppParams.ParamByName[CT_PARAM_DefaultFee].GetAsInt64(0);
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
    AppParams := Self.FAppParams;
    WalletKeys := Self.FWalletKeys;
    if ShowModal=MrOk then begin
      SaveAppParams;
      UpdateConfigChanged;
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
    UpdatePrivateKeys;
  Finally
    FRM.Free;
  End;
end;

procedure TFRMWallet.MiRemoveaccountfromselectedClick(Sender: TObject);
begin
  PageControl.ActivePage := tsAccountsExplorer;
  PageControlChange(Nil);
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  sbSelectedAccountsDelClick(Sender);
end;

procedure TFRMWallet.OnMinerNewBlockFound(sender: TMinerThread; Operations: TPCOperationsComp);
begin
  MinersBlocksFound := MinersBlocksFound+1;
  Sender.AccountKey := GetAccountKeyForMiner;
end;

procedure TFRMWallet.OnNetBlackListUpdated(Sender: TObject);
Const CT_TRUE_FALSE : Array[Boolean] Of AnsiString = ('FALSE','TRUE');
Var i,j : integer;
 P : PNodeServerAddress;
 l : TList;
 strings : TStrings;
begin
  l := TNetData.NetData.BlackList.LockList;
  try
    strings := memoNetBlackLists.Lines;
    strings.BeginUpdate;
    Try
      strings.Clear;
      strings.Add('BlackList Updated: '+DateTimeToStr(now)+' by TID:'+IntToHex(TThread.CurrentThread.ThreadID,8));
      j := 0;
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        if Not P^.its_myself then begin
          inc(j);
          strings.Add(Format('Blacklist IP:%s:%d LastConnection:%s Reason: %s',
            [
             P^.ip,P^.port,
             DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection))),P^.BlackListText]));
        end;
      end;
      Strings.Add(Format('Total Blacklisted IPs: %d (Total %d)',[j,l.Count]));
    Finally
      strings.EndUpdate;
    End;
  finally
    TNetData.NetData.BlackList.UnlockList;
  end;
end;

procedure TFRMWallet.OnNetConnectionsUpdated(Sender: TObject);
Const CT_BooleanToString : Array[Boolean] of String = ('False','True');
Var i : integer;
 NC : TNetConnection;
 l : TList;
 sClientApp : String;
 strings, sNSC, sRS, sDisc : TStrings;
begin
  l := TNetData.NetData.NetConnections.LockList;
  try
    strings := memoNetConnections.Lines;
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
          if NC is TNetServerClient then begin
            sNSC.Add(Format('Client: IP:%s Sent/Received:%d/%d Bytes - %s - Active since %s',
              [NC.ClientRemoteAddr,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,DateTimeElapsedTime(NC.CreatedTime)]));
          end else begin
            if NC.IsMyselfServer then sNSC.Add(Format('MySelf IP:%s Sent/Received:%d/%d Bytes - %s - Active since %s',
              [NC.ClientRemoteAddr,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,DateTimeElapsedTime(NC.CreatedTime)]))
            else begin
              sRS.Add(Format('Remote Server: IP:%s Sent/Received:%d/%d Bytes - %s - Active since %s',
              [NC.ClientRemoteAddr,NC.Client.BytesSent,NC.Client.BytesReceived,sClientApp,DateTimeElapsedTime(NC.CreatedTime)]));
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
    CheckMining;
  finally
    TNetData.NetData.NetConnections.UnlockList;
  end;
end;

procedure TFRMWallet.OnNetNodeServersUpdated(Sender: TObject);
Var i : integer;
 P : PNodeServerAddress;
 l : TList;
 strings : TStrings;
 s : String;
begin
  l := TNetData.NetData.NodeServers.LockList;
  try
    strings := memoNetServers.Lines;
    strings.BeginUpdate;
    Try
      strings.Clear;
      strings.Add('NodeServers Updated: '+DateTimeToStr(now) +' Count: '+inttostr(l.Count));
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        s := Format('Server IP:%s:%d',[P^.ip,P^.port]);
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
        if (P.last_attempt_to_connect>0) then begin
          s := s + ' Last attempt to connect: '+DateTimeToStr(P^.last_attempt_to_connect)+' (Attempts: '+inttostr(P^.total_failed_attemps_to_connect)+')';
        end;
        strings.Add(s);
      end;
    Finally
      strings.EndUpdate;
    End;
  finally
    TNetData.NetData.NodeServers.UnlockList;
  end;
end;

procedure TFRMWallet.OnNetStatisticsChanged(Sender: TObject);
Var NS : TNetStatistics;
begin
  CheckMining;
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
  UpdateAccounts(false);
  UpdateBlockChainState;
end;

procedure TFRMWallet.OnNewLog(logtype: TLogType; Time : TDateTime; ThreadID : Cardinal; const sender,logtext: AnsiString);
Var s : AnsiString;
begin
  if (logtype=ltdebug) And (Not cbShowDebugLogs.Checked) then exit;
  if ThreadID=MainThreadID then s := ' MAIN:' else s:=' TID:';
  if MemoLogs.Lines.Count>300 then begin
    // Limit max lines in logs...
    memoLogs.Lines.BeginUpdate;
    try
      while memoLogs.Lines.Count>250 do memoLogs.Lines.Delete(0);
    finally
      memoLogs.Lines.EndUpdate;
    end;
  end;
  memoLogs.Lines.Add(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time)+s+IntToHex(ThreadID,8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
  //
end;

procedure TFRMWallet.OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: TRawBytes);
Var s : String;
begin
  inc(FMessagesUnreadCount);
  if Assigned(NetConnection) then begin
    s := DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr;
    memoMessages.Lines.Add(DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr+' Length '+inttostr(Length(MessageData))+' bytes');
    memoMessages.Lines.Add('RECEIVED> '+MessageData);
    if FAppParams.ParamByName[CT_PARAM_ShowModalMessages].GetAsBoolean(false) then begin
      s := DateTimeToStr(now)+' Message from '+NetConnection.ClientRemoteAddr+#10+
         'Length '+inttostr(length(MessageData))+' bytes'+#10+#10;
      if TCrypto.IsHumanReadable(MessageData) then begin
         s := s + MessageData;
      end else begin
         s := s +'Value in hexadecimal:'+#10+
              TCrypto.ToHexaString(MessageData);
      end;
      Application.MessageBox(PChar(s),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
    end;
  end else begin
    memoMessages.Lines.Add(DateTimeToStr(now)+' Internal message: '+MessageData);
  end;
  if FMessagesUnreadCount>1 then lblReceivedMessages.Caption := Format('You have received %d messages',[FMessagesUnreadCount])
  else lblReceivedMessages.Caption := 'You have received 1 message';
  lblReceivedMessages.Visible := true;
end;

procedure TFRMWallet.OnReceivedHelloMessage(Sender: TObject);
Var nsarr : TNodeServerAddressArray;
  i : Integer;
  s : AnsiString;
begin
  CheckMining;
  // Update node servers Peer Cache
  nsarr := TNetData.NetData.GetValidNodeServers;
  s := '';
  for i := low(nsarr) to High(nsarr) do begin
    if (s<>'') then s := s+';';
    s := s + nsarr[i].ip+':'+IntToStr( nsarr[i].port );
  end;
  FAppParams.ParamByName[CT_PARAM_PeerCache].SetAsString(s);
  TNode.Node.PeerCache := s;
end;

procedure TFRMWallet.OnSelectedAccountsGridUpdated(Sender: TObject);
begin
  lblSelectedAccountsCount.Caption := Inttostr(FSelectedAccountsGrid.AccountsCount);
  lblSelectedAccountsBalance.Caption := TAccountComp.FormatMoney( FSelectedAccountsGrid.AccountsBalance );
end;

procedure TFRMWallet.OnWalletChanged(Sender: TObject);
begin
  UpdatePrivateKeys;
end;

procedure TFRMWallet.PageControlChange(Sender: TObject);
begin
  MiDecodePayload.Enabled := false;
  if PageControl.ActivePage=tsAccountsExplorer then begin
    FAccountsGrid.Node := FNode;
    MiDecodePayload.Enabled := true;
    FSelectedAccountsGrid.Node := FNode;
  end else begin
    FAccountsGrid.Node := Nil;
    FSelectedAccountsGrid.Node := Nil;
  end;
  if PageControl.ActivePage=tsPendingOperations then begin
    FPendingOperationsGrid.Node := FNode;
    MiDecodePayload.Enabled := true;
  end else FPendingOperationsGrid.Node := Nil;
  if PageControl.ActivePage=tsBlockChain then FBlockChainDBGrid.Node := FNode
  else FBlockChainDBGrid.Node := Nil;
  if PageControl.ActivePage=tsOperations then begin
    FOperationsDBGrid.Node := FNode;
    MiDecodePayload.Enabled := true;
  end else FOperationsDBGrid.Node := Nil;
  if PageControl.ActivePage=tsMessages then begin
    UpdateAvailableConnections;
    FMessagesUnreadCount := 0;
    lblReceivedMessages.Visible := false;
  end;
end;

procedure TFRMWallet.SaveAppParams;
Var ms : TMemoryStream;
  s : AnsiString;
begin
  ms := TMemoryStream.Create;
  Try
    FAccountsGrid.SaveToStream(ms);
    ms.Position := 0;
    setlength(s,ms.Size);
    ms.ReadBuffer(s[1],ms.Size);
    FAppParams.ParamByName[CT_PARAM_GridAccountsStream].SetAsString(s);
  Finally
    ms.Free;
  End;
end;

procedure TFRMWallet.sbSelectedAccountsAddAllClick(Sender: TObject);
Var lsource,ltarget : TOrderedCardinalList;
  i : Integer;
begin
  lsource := FAccountsGrid.LockAccountsList;
  Try
    ltarget := FSelectedAccountsGrid.LockAccountsList;
    Try
      for i := 0 to lsource.Count-1 do begin
        if FWalletKeys.IndexOfAccountKey(FNode.Bank.SafeBox.Account(lsource.Get(i)).accountkey)<0 then raise Exception.Create(Format('You cannot operate with account %d because private key not found in your wallet',[lsource.Get(i)]));
        ltarget.Add(lsource.Get(i));
      end;
    Finally
      FSelectedAccountsGrid.UnlockAccountsList;
    End;
  Finally
    FAccountsGrid.UnlockAccountsList;
  End;
end;

procedure TFRMWallet.sbSelectedAccountsAddClick(Sender: TObject);
Var l : TOrderedCardinalList;
  an : Int64;
begin
  an := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if (an<0) then raise Exception.Create('No account selected');
  if FWalletKeys.IndexOfAccountKey(FNode.Bank.SafeBox.Account(an).accountkey)<0 then
    raise Exception.Create(Format('You cannot add %s account because private key not found in your wallet.'#10+#10+'You''re not the owner!',
      [TAccountComp.AccountNumberToAccountTxtNumber(an)]));
  // Add
  l := FSelectedAccountsGrid.LockAccountsList;
  Try
    l.Add( an );
  Finally
    FSelectedAccountsGrid.UnlockAccountsList;
  End;
end;

procedure TFRMWallet.sbSelectedAccountsDelAllClick(Sender: TObject);
Var l : TOrderedCardinalList;
begin
  l := FSelectedAccountsGrid.LockAccountsList;
  try
    l.Clear;
  finally
    FSelectedAccountsGrid.UnlockAccountsList;
  end;
end;

procedure TFRMWallet.sbSelectedAccountsDelClick(Sender: TObject);
Var an : Int64;
  l : TOrderedCardinalList;
begin
  l := FSelectedAccountsGrid.LockAccountsList;
  try
    an := FSelectedAccountsGrid.AccountNumber(dgSelectedAccounts.Row);
    if an>=0 then l.Remove(an);
  finally
    FSelectedAccountsGrid.UnlockAccountsList;
  end;
end;

procedure TFRMWallet.SetMinersBlocksFound(const Value: Integer);
begin
  FMinersBlocksFound := Value;
  lblBlocksFound.Caption := Inttostr(Value);
  if Value>0 then lblBlocksFound.Font.Color := clGreen
  else lblBlocksFound.Font.Color := clDkGray;
end;

procedure TFRMWallet.TimerUpdateStatusTimer(Sender: TObject);
begin
  Try
    UpdateConnectionStatus;
    UpdateBlockChainState;
    UpdateNodeStatus;
  Except
    On E:Exception do begin
      E.Message := 'Exception at TimerUpdate '+E.ClassName+': '+E.Message;
      TLog.NewLog(lterror,ClassName,E.Message);
    end;
  End;
end;

procedure TFRMWallet.TrayIconDblClick(Sender: TObject);
begin
  TrayIcon.Visible := False;
  Show();
  WindowState := wsNormal;
  Application.BringToFront();
end;

procedure TFRMWallet.UpdateAccounts(RefreshData : Boolean);
Var accl : TOrderedCardinalList;
  l : TList;
  i,j,k : Integer;
  c  : Cardinal;
  applyfilter : Boolean;
  acc : TAccount;
begin
  If Not Assigned(FOrderedAccountsKeyList) Then exit;
  if Not RefreshData then begin
    dgAccounts.Invalidate;
    exit;
  end;
  applyfilter := (cbFilterAccounts.Checked) and ((FMinAccountBalance>0) Or (FMaxAccountBalance<CT_MaxWalletAmount));
  FAccountsGrid.ShowAllAccounts := (Not cbExploreMyAccounts.Checked) And (not applyfilter);
  if Not FAccountsGrid.ShowAllAccounts then begin
    accl := FAccountsGrid.LockAccountsList;
    Try
      accl.Clear;
      if cbExploreMyAccounts.Checked then begin
        if cbMyPrivateKeys.ItemIndex<0 then exit;
        if cbMyPrivateKeys.ItemIndex=0 then begin
          // All keys in the wallet
          for i := 0 to FWalletKeys.Count - 1 do begin
            j := FOrderedAccountsKeyList.IndexOfAccountKey(FWalletKeys[i].AccountKey);
            if (j>=0) then begin
              l := FOrderedAccountsKeyList.AccountKeyList[j];
              for k := 0 to l.Count - 1 do begin
                if applyfilter then begin
                  acc := FNode.Bank.SafeBox.Account(Cardinal(l[k]));
                  if (acc.balance>=FMinAccountBalance) And (acc.balance<=FMaxAccountBalance) then accl.Add(acc.account);
                end else accl.Add(Cardinal(l[k]));
              end;
            end;
          end;
        end else begin
          i := Integer(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
          if (i>=0) And (i<FWalletKeys.Count) then begin
            j := FOrderedAccountsKeyList.IndexOfAccountKey(FWalletKeys[i].AccountKey);
            if (j>=0) then begin
              l := FOrderedAccountsKeyList.AccountKeyList[j];
              for k := 0 to l.Count - 1 do begin
                if applyfilter then begin
                  acc := FNode.Bank.SafeBox.Account(Cardinal(l[k]));
                  if (acc.balance>=FMinAccountBalance) And (acc.balance<=FMaxAccountBalance) then accl.Add(acc.account);
                end else accl.Add(Cardinal(l[k]));
              end;
            end;
          end;
        end;
      end else begin
        // There is a filter... check every account...
        c := 0;
        while (c<FNode.Bank.SafeBox.AccountsCount) do begin
          acc := FNode.Bank.SafeBox.Account(c);
          if (acc.balance>=FMinAccountBalance) And (acc.balance<=FMaxAccountBalance) then accl.Add(acc.account);
          inc(c);
        end;
      end;
    Finally
      FAccountsGrid.UnlockAccountsList;
    End;
    lblAccountsCount.Caption := inttostr(accl.Count);
  end else begin
    lblAccountsCount.Caption := inttostr(FNode.Bank.AccountsCount);
  end;
  bbChangeKeyName.Enabled := cbExploreMyAccounts.Checked;
  // Show Totals:
  lblAccountsBalance.Caption := TAccountComp.FormatMoney(FAccountsGrid.AccountsBalance);
  UpdateOperations;
end;

procedure TFRMWallet.UpdateAvailableConnections;
Var i : integer;
 NC : TNetConnection;
 l : TList;
begin
  l := TNetData.NetData.NetConnections.LockList;
  try
    lbNetConnections.Items.BeginUpdate;
    Try
      lbNetConnections.Items.Clear;
      for i := 0 to l.Count - 1 do begin
        NC := l[i];
        if NC.Connected then begin
          if NC is TNetServerClient then begin
            if Not NC.IsMyselfServer then begin
              lbNetConnections.Items.AddObject(Format('Client: IP:%s',[NC.ClientRemoteAddr]),NC);
            end;
          end else begin
            if Not NC.IsMyselfServer then begin
              lbNetConnections.Items.AddObject(Format('Server: IP:%s',[NC.ClientRemoteAddr]),NC);
            end;
          end;
        end;
      end;
    Finally
      lbNetConnections.Items.EndUpdate;
    End;
  finally
    TNetData.NetData.NetConnections.UnlockList;
  end;
end;

procedure TFRMWallet.UpdateBlockChainState;
Var isMining : boolean;
  hr : Int64;
  i,mc : Integer;
  s : String;
  mtl : TList;
  f, favg : real;
begin
  UpdateNodeStatus;
  if Assigned(FNode) then begin
    if FNode.Bank.BlocksCount>0 then begin
      lblCurrentBlock.Caption :=  Inttostr(FNode.Bank.BlocksCount)+' (0..'+Inttostr(FNode.Bank.BlocksCount-1)+')'; ;
    end else lblCurrentBlock.Caption :=  '(none)';
    lblCurrentAccounts.Caption := Inttostr(FNode.Bank.AccountsCount);
    lblCurrentBlockTime.Caption := UnixTimeToLocalElapsedTime(FNode.Bank.LastOperationBlock.timestamp);
    lblOperationsPending.Caption := Inttostr(FNode.Operations.Count);
    lblCurrentDifficulty.Caption := InttoHex(FNode.Operations.OperationBlock.compact_target,8);
    favg := FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage);
    f := (CT_NewLineSecondsAvg - favg) / CT_NewLineSecondsAvg;
    lblTimeAverage.Caption := 'Last '+Inttostr(CT_CalcNewTargetBlocksAverage)+': '+FormatFloat('0.0',favg)+' sec. (Optimal '+Inttostr(CT_NewLineSecondsAvg)+'s) Deviation '+FormatFloat('0.00%',f*100);
    if favg>=CT_NewLineSecondsAvg then begin
      lblTimeAverage.Font.Color := clNavy;
    end else begin
      lblTimeAverage.Font.Color := clOlive;
    end;
    lblTimeAverageAux.Caption := Format('Last %d: %s sec. - Last %d: %s sec. - Last %d: %s sec.',[
        CT_CalcNewTargetBlocksAverage * 2 ,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage * 2)),
        CT_CalcNewTargetBlocksAverage DIV 2,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 2)),
        CT_CalcNewTargetBlocksAverage DIV 4,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 4))]);
    mtl := FNode.MinerThreads.LockList;
    try
      mc := mtl.Count;
      If mc>0 then begin
        isMining := Not TMinerThread(mtl[0]).Paused;
        hr := 0;
        for i := 0 to mtl.Count - 1 do begin
          hr := hr + TMinerThread(mtl[i]).HashRate;
        end;
      end else isMining :=false;
    finally
      FNode.MinerThreads.UnlockList;
    end;
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
  if isMining then begin
    if mc>1 then s := inttostr(mc)+' Miners at '
    else s := 'Mining at ';
    lblMiningStatus.Caption := s +FormatFloat('0.0',hr / 1024)+' Kh/s (R: '+FormatFloat('0.0',TMinerThread.AllMinersPlayCount / 1000000)+'G)';
    lblMiningStatus.Font.Color := clNavy
  end else begin
    lblMiningStatus.Caption := 'Not mining';
    lblMiningStatus.Font.Color := clRed;
  end;

end;

procedure TFRMWallet.UpdateConfigChanged;
Var wa : Boolean;
  i : Integer;
begin
  tsLogs.TabVisible := FAppParams.ParamByName[CT_PARAM_ShowLogs].GetAsBoolean(false);
  if (Not tsLogs.TabVisible) then begin
    FLog.OnNewLog := Nil;
    if PageControl.ActivePage = tsLogs then PageControl.ActivePage := tsAccountsExplorer;
  end else FLog.OnNewLog := OnNewLog;
  if FAppParams.ParamByName[CT_PARAM_SaveLogFiles].GetAsBoolean(false) then begin
    if FAppParams.ParamByName[CT_PARAM_SaveDebugLogs].GetAsBoolean(false) then FLog.SaveTypes := CT_TLogTypes_ALL
    else FLog.SaveTypes := CT_TLogTypes_DEFAULT;
    FLog.FileName := TFolderHelper.GetPascalCoinDataFolder+'\PascalCointWallet.log';
  end else begin
    FLog.SaveTypes := [];
    FLog.FileName := '';
  end;
  if Assigned(FNode) then begin
    wa := FNode.NetServer.Active;
    FNode.NetServer.Port := FAppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
    FNode.NetServer.Active := wa;
    FNode.Operations.BlockPayload := FAppParams.ParamByName[CT_PARAM_MinerName].GetAsString('');
  end;
  FMaxCPUs := FAppParams.ParamByName[CT_PARAM_MaxCPUs].GetAsInteger(1);
  if FMaxCPUs>CPUCount then FMaxCPUs := CPUCount;
  if FMaxCPUs<0 then FMaxCPUs := 0;

  i := FAppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].GetAsInteger(Integer(mpk_Random));
  if (i>=Integer(Low(TMinerPrivatekey))) And (i<=Integer(High(TMinerPrivatekey))) then FMinerPrivateKeyType := TMinerPrivateKey(i)
  else FMinerPrivateKeyType := mpk_Random;

  cbAllowMining.Checked :=  (FAppParams.ParamByName[CT_PARAM_AutomaticMineWhenConnectedToNodes].GetAsBoolean(true));
end;

procedure TFRMWallet.UpdateConnectionStatus;
Var
  NS : TNetStatistics;
  errors : AnsiString;

Var i : integer;
 NC : TNetConnection;
 l : TList;
begin
  UpdateNodeStatus;
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

procedure TFRMWallet.UpdateNodeStatus;
Var status : AnsiString;
begin
  If Not Assigned(FNode) then begin
    lblNodeStatus.Font.Color := clRed;
    lblNodeStatus.Caption := 'Initializing...';
  end else begin
    If FNode.IsReady(status) then begin
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
end;

procedure TFRMWallet.UpdateOperations;
Var accn : Int64;
begin
  accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
  FOperationsGrid.AccountNumber := accn;
end;

procedure TFRMWallet.UpdatePrivateKeys;
Var i,last_i : Integer;
  wk : TWalletKey;
  s : AnsiString;
begin
  If (Not Assigned(FOrderedAccountsKeyList)) And (Assigned(FNode)) Then begin
    FOrderedAccountsKeyList := TOrderedAccountKeysList.Create(FNode.Bank.SafeBox,false);
  end;
  if (cbMyPrivateKeys.ItemIndex>=0) then last_i := Integer(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex])
  else last_i := -1;
  cbMyPrivateKeys.items.BeginUpdate;
  Try
    cbMyPrivateKeys.Items.Clear;
//    cbMyPrivateKeys.Items.AddObject('(All my private keys)',TObject(-1));
    For i:=0 to FWalletKeys.Count-1 do begin
      wk := FWalletKeys.Key[i];
      if assigned(FOrderedAccountsKeyList) then begin
        FOrderedAccountsKeyList.AddAccountKey(wk.AccountKey);
      end;
      if (wk.Name='') then begin
        s := 'Sha256='+TCrypto.ToHexaString( TCrypto.DoSha256( TAccountComp.AccountKey2RawString(wk.AccountKey) ) );
      end else begin
        s := wk.Name;
      end;
      if Not Assigned(wk.PrivateKey) then s := s + '(*)';
      cbMyPrivateKeys.Items.AddObject(s,TObject(i));
    end;
    cbMyPrivateKeys.Sorted := true;
    cbMyPrivateKeys.Sorted := false;
    cbMyPrivateKeys.Items.InsertObject(0,'(All my private keys)',TObject(-1));
  Finally
    cbMyPrivateKeys.Items.EndUpdate;
  End;
  last_i := cbMyPrivateKeys.Items.IndexOfObject(TObject(last_i));
  if last_i<0 then last_i := 0;
  if cbMyPrivateKeys.Items.Count>last_i then cbMyPrivateKeys.ItemIndex := last_i
  else if cbMyPrivateKeys.Items.Count>=0 then cbMyPrivateKeys.ItemIndex := 0;
end;

initialization
  FRMWallet := Nil;
end.
