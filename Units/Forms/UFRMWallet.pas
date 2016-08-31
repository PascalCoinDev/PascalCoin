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
  UBlockChain, UNode, DBCtrls, UGridUtils, UMiner, UAccounts, Menus, ImgList,
  AppEvnts, UNetProtocol, UCrypto;

Const
  CT_PARAM_GridAccountsStream = 'GridAccountsStream';
  CT_PARAM_GridAccountsPos = 'GridAccountsPos';

  CT_PARAM_DefaultFee = 'DefaultFee';
  CT_PARAM_InternetServerPort = 'InternetServerPort';
  CT_PARAM_AutomaticMineWhenConnectedToNodes = 'AutomaticMineWhenConnectedToNodes';
  CT_PARAM_MinerPrivateKeyType = 'MinerPrivateKeyType';
  CT_PARAM_MinerPrivateKeySelectedPublicKey = 'MinerPrivateKeySelectedPublicKey';
  CT_PARAM_SaveLogFiles = 'SaveLogFiles';
  CT_PARAM_ShowLogs = 'ShowLogs';
  CT_PARAM_MinerName = 'MinerName';
  CT_PARAM_FirstTime = 'FirstTime';
  CT_PARAM_ShowModalMessages = 'ShowModalMessages';
  CT_PARAM_MaxCPUs = 'MaxCPUs';

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
    dgAccountOperations: TDrawGrid;
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
    Label15: TLabel;
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
    FOperationsGrid : TOperationsGrid;
    FPendingOperationsGrid : TOperationsGrid;
    FOrderedAccountsKeyList : TOrderedAccountKeysList;
    FOperationsDBGrid : TOperationsDBGrid;
    FBlockChainDBGrid : TBlockChainDBGrid;
    FMinerPrivateKeyType : TMinerPrivateKey;
    FMemoNetConnections : TStringListAux;
    FMemoBlackListNodes : TStringListAux;
    FMemoAvailableNodeServers : TStringListAux;
    FUpdating : Boolean;
    FMessagesUnreadCount : Integer;
    Procedure CheckMining;
    Procedure OnNewAccount(Sender : TObject);
    Procedure OnReceivedHelloResponse(Sender : TObject);
    Procedure OnNetStatisticsChanged(Sender : TObject);
    procedure OnNewLog(logtype : TLogType; Time : TDateTime; ThreadID : Cardinal; Const sender, logtext : AnsiString);
    procedure OnMinerNewBlockFound(sender : TMinerThread; Operations : TPCOperationsComp);
    procedure OnWalletChanged(Sender : TObject);
    procedure OnNetConnectionsUpdated(Sender : TObject);
    procedure OnNetNodeServersUpdated(Sender : TObject);
    procedure OnNetBlackListUpdated(Sender : TObject);
    Procedure OnNodeMessageEvent(NetConnection : TNetConnection; MessageData : TRawBytes);
    Procedure UpdateConnectionStatus;
    Procedure UpdateAccounts;
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
  UFRMAbout, UFRMOperation, UFRMWalletKeys, UFRMPayloadDecoder;

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
  // Activating server
  TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  TNode.Node.NetServer.Active := true;
  FRMWallet.UpdateAccounts;
end;

{ TFRMWallet }

procedure TFRMWallet.Activate;
begin
  inherited;
  if FIsActivated then exit;
  FIsActivated := true;
  try
    // Check OpenSSL dll
    if Not LoadSSLCrypt then raise Exception.Create('Cannot load '+SSL_C_LIB+#10+'To use this software make sure this file is available on you system or reinstall this Application');
    TCrypto.InitCrypto;
    // Read Wallet
    Try
      FWalletKeys.WalletFileName := TFolderHelper.GetPascalCoinDataFolder+'\WalletKeys.dat';
    Except
      On E:Exception do begin
        E.Message := 'Cannot open your Wallet... Perhaps another instance of Pascal Coin is active!'+#10+#10+E.Message;
        Raise;
      end;
    End;
    // Creating Node:
    FNode := TNode.Node;
    FNode.NetServer.Port := FAppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
    // Check Database
    FNode.Bank.StorageClass := TDBStorage;
    TDBStorage(FNode.Bank.Storage).AccessFileName := TFolderHelper.GetPascalCoinDataFolder+'\pascalcoin.mdb';
    // Init Grid
    FAccountsGrid.Node := FNode;
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
    // Init TNode
    TNetData.NetData.OnReceivedHelloResponse := OnReceivedHelloResponse;
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
  UpdateAccounts;
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

procedure TFRMWallet.bbSendAMessageClick(Sender: TObject);
Var basem,m : String;
  them, errors : AnsiString;
  i,n : Integer;
  nc : TNetConnection;
begin
  CheckIsReady;
  if (lbNetConnections.SelCount<=0) Or (lbNetConnections.ItemIndex<0) then raise Exception.Create('Select at least 1 connection');
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
    'ALERT: Sending unauthorized messages will be considered spam and you will be banned'+#10+
    #10+
    'Message: '+#10+
    m),PChar(Application.Title),MB_ICONQUESTION+MB_YESNO+MB_DEFBUTTON1)<>IdYes then exit;
  them := m;
  if n>1 then begin
    for i := 0 to lbNetConnections.Items.Count - 1 do begin
      if lbNetConnections.Selected[i] then begin
        nc := TNetConnection(lbNetconnections.Items.Objects[i]);
        if TNetData.NetData.ConnectionExists(nc) then begin
          FNode.SendNodeMessage(nc,m,errors);
          memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.Client.RemoteHost+':'+nc.Client.RemotePort+' > '+m);
        end;
      end;
    end;
  end else begin
    nc := TNetConnection(lbNetconnections.Items.Objects[lbNetconnections.ItemIndex]);
    if TNetData.NetData.ConnectionExists(nc) then begin
      FNode.SendNodeMessage(nc,m,errors);
      memoMessages.Lines.Add(DateTimeToStr(now)+' Sent to '+nc.Client.RemoteHost+':'+nc.Client.RemotePort+' > '+m);
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
        'In order to Mine is necessary that you open your external port %d from the Internet to allow other nodes of Pascal Coin to connect to you.'+#10+
        '(Note: This is not mandatory... but will work better with open ports)'+#10+
        #10+
        'To do this you must configure your Router/Firewall and enable NAT to your local machine at port: %d'+#10+#10+
        'After allowing incoming connections... you must wait until other nodes connects to you to mine'+#10+
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
  UpdateAccounts;
  UpdateOperations;
end;

procedure TFRMWallet.cbFilterOperationsByDateClick(Sender: TObject);
begin
  dtpFilterOperationsDateStart.Enabled := cbFilterOperationsByDate.Checked;
  dtpFilterOperationsDateEnd.Enabled := cbFilterOperationsByDate.Checked;
  ebFilterOperationsAccountExit(Nil);
end;

procedure TFRMWallet.cbMyPrivateKeysChange(Sender: TObject);
begin
  UpdateAccounts;
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
    mtl := FNode.MinerThreads.LockList('TFRMWallet.CheckMining stop');
    try
      for i:=mtl.Count-1 downto 0 do begin
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
        Or (TNetData.NetData.NetStatistics.ServersConnections>=2) ) And
      (TNetData.NetData.MaxRemoteOperationBlock.block<=FNode.Operations.OperationBlock.block)
    ) then begin
    if (cbAllowMining.checked) Or (ForceMining) then begin
      n := 0;
      mtl := FNode.MinerThreads.LockList('TFRMWallet.CheckMining base');
      try
        for i:=mtl.Count-1 downto 0 do begin
          if TMinerThread(mtl[i]).Paused then TMinerThread(mtl[i]).Paused := false;
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

function TFRMWallet.ForceMining: Boolean;
begin
  Result := false;
end;

procedure TFRMWallet.FormCreate(Sender: TObject);
Var i : Integer;
begin
  if CPUCount>1 then FMaxCPUs := CPUCount-1
  else FMaxCPUs := 1;
  FMessagesUnreadCount := 0;
  lblReceivedMessages.Visible := false;
  FMemoNetConnections := TStringListAux.Create;
  FMemoBlackListNodes := TStringListAux.Create;
  FMemoAvailableNodeServers := TStringListAux.Create;
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
  TNetData.NetData.OnReceivedHelloResponse := Nil;
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
  FNodeNotifyEvents.Free;
  //
  step := 'Assigning Nil to TNetData';
  TNetData.NetData.OnReceivedHelloResponse := Nil;
  TNetData.NetData.OnStatisticsChanged := Nil;

  step := 'Destroying grids operators';
  FOperationsDBGrid.Free;
  FBlockChainDBGrid.Free;

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
  FWalletKeys.Free;
  step := 'Processing messages 2';
  Application.ProcessMessages;
  step := 'Destroying stringslist';
  FMemoNetConnections.Free;
  FMemoBlackListNodes.Free;
  FMemoAvailableNodeServers.Free;
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

procedure TFRMWallet.miNewOperationClick(Sender: TObject);
begin
  CheckIsReady;
  With TFRMOperation.Create(Self) do
  Try
    SenderAccount := FAccountsGrid.AccountNumber(dgAccounts.Row);
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
  l := TNetData.NetData.BlackList.LockList('TFRMWallet.OnNetBlackListUpdated');
  try
    strings := FMemoBlackListNodes;
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
 strings, sNSC, sRS, sDisc : TStrings;
begin
  l := TNetData.NetData.NetConnections.LockList('TFRMWallet.OnNetConnectionsUpdated');
  try
    strings := FMemoNetConnections;
    sNSC := TStringList.Create;
    sRS := TStringList.Create;
    sDisc := TStringList.Create;
    strings.BeginUpdate;
    Try
      for i := 0 to l.Count - 1 do begin
        NC := l[i];
        if NC.Connected then begin
          if NC is TNetServerClient then begin
            sNSC.Add(Format('Client: IP:%s:%s Sent/Received:%d/%d Bytes - Active since %s',
              [NC.Client.RemoteHost,NC.Client.RemotePort,NC.Client.BytesSent,NC.Client.BytesReceived,DateTimeElapsedTime(NC.CreatedTime)]));
          end else begin
            if NC.IsMyselfServer then sNSC.Add(Format('MySelf IP:%s:%s Sent/Received:%d/%d Bytes - Active since %s',
              [NC.Client.RemoteHost,NC.Client.RemotePort,NC.Client.BytesSent,NC.Client.BytesReceived,DateTimeElapsedTime(NC.CreatedTime)]))
            else begin
              sRS.Add(Format('Remote Server: IP:%s:%s Sent/Received:%d/%d Bytes - Active since %s',
              [NC.Client.RemoteHost,NC.Client.RemotePort,NC.Client.BytesSent,NC.Client.BytesReceived,DateTimeElapsedTime(NC.CreatedTime)]));
            end;
          end;
        end else begin
          if NC is TNetServerClient then begin
            sDisc.Add(Format('Disconnected client: IP:%s:%s',[NC.Client.RemoteHost,NC.Client.RemotePort]));
          end else if NC.IsMyselfServer then begin
            sDisc.Add(Format('Disconnected MySelf IP:%s:%s',[NC.Client.RemoteHost,NC.Client.RemotePort]));
          end else begin
            sDisc.Add(Format('Disconnected Remote Server: IP:%s:%s',[NC.Client.RemoteHost,NC.Client.RemotePort,CT_BooleanToString[NC.Connected]]));
          end;
        end;
      end;
      strings.Clear;
      strings.Add(Format('Connections Updated %s Clients:%d Servers:%d',[DateTimeToStr(now),sNSC.Count,sRS.Count]));
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
  l := TNetData.NetData.NodeServers.LockList('TFRMWallet.OnNetNodeServersUpdated');
  try
    strings := FMemoAvailableNodeServers;
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
          s := s + ' Last Connection: '+DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection)));
        end;
        if (P.last_attempt_to_connect>0) then begin
          s := s + ' Last Attempt to connect: '+DateTimeToStr(P^.last_attempt_to_connect)+' (Attempts: '+inttostr(P^.total_failed_attemps_to_connect)+')';
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
    StatusBar.Panels[1].Text := Format('Connections:%d (%d) Clients:%d Servers:%d - Rcvd:%d Bytes Send:%d Bytes',
      [NS.ActiveConnections,NS.TotalConnections,NS.ClientsConnections,NS.ServersConnections,NS.BytesReceived,NS.BytesSend]);
  end else begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
  end;
end;

procedure TFRMWallet.OnNewAccount(Sender: TObject);
begin
  UpdateAccounts;
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
  s := DateTimeToStr(now)+' Message received from '+NetConnection.Client.RemoteHost+':'+NetConnection.Client.RemotePort;
  memoMessages.Lines.Add(DateTimeToStr(now)+' Message received from '+NetConnection.Client.RemoteHost+':'+NetConnection.Client.RemotePort+' Length '+inttostr(Length(MessageData))+' bytes');
  memoMessages.Lines.Add('RECEIVED> '+MessageData);
  if FAppParams.ParamByName[CT_PARAM_ShowModalMessages].GetAsBoolean(false) then begin
    s := DateTimeToStr(now)+' Message from '+NetConnection.Client.RemoteHost+':'+NetConnection.Client.RemotePort+#10+
       'Length '+inttostr(length(MessageData))+' bytes'+#10+#10;
    if TCrypto.IsHumanReadable(MessageData) then begin
       s := s + MessageData;
    end else begin
       s := s +'Value in hexadecimal:'+#10+
            TCrypto.ToHexaString(MessageData);
    end;
    Application.MessageBox(PChar(s),PChar(Application.Title),MB_ICONINFORMATION+MB_OK);
  end;
  if FMessagesUnreadCount>1 then lblReceivedMessages.Caption := Format('You have received %d messages',[FMessagesUnreadCount])
  else lblReceivedMessages.Caption := 'You have received 1 message';
  lblReceivedMessages.Visible := true;
end;

procedure TFRMWallet.OnReceivedHelloResponse(Sender: TObject);
begin
  CheckMining;
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
  end else FAccountsGrid.Node := Nil;
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
    If (FMemoNetConnections.UpdateCount=0) And (FMemoNetConnections.Count>0) then begin
      memoNetConnections.Lines.Assign(FMemoNetConnections);
      FMemoNetConnections.Clear;
    end;
    If (FMemoAvailableNodeServers.UpdateCount=0) And (FMemoAvailableNodeServers.Count>0) then begin
      memoNetServers.Lines.Assign(FMemoAvailableNodeServers);
      FMemoAvailableNodeServers.Clear;
    end;
    If (FMemoBlackListNodes.UpdateCount=0) And (FMemoBlackListNodes.Count>0) then begin
      memoNetBlackLists.Lines.Assign(FMemoBlackListNodes);
      FMemoBlackListNodes.Clear;
    end;
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

procedure TFRMWallet.UpdateAccounts;
Var accl : TOrderedCardinalList;
  l : TList;
  i,j,k : Integer;
begin
  If Not Assigned(FOrderedAccountsKeyList) Then exit;
  FAccountsGrid.ShowAllAccounts := Not cbExploreMyAccounts.Checked;
  if cbExploreMyAccounts.Checked then begin
    accl := FAccountsGrid.LockAccountsList;
    Try
      accl.Clear;
      if cbMyPrivateKeys.ItemIndex<0 then exit;
      if cbMyPrivateKeys.ItemIndex=0 then begin
        // All keys in the wallet
        for i := 0 to FWalletKeys.Count - 1 do begin
          j := FOrderedAccountsKeyList.IndexOfAccountKey(FWalletKeys[i].AccountKey);
          if (j>=0) then begin
            l := FOrderedAccountsKeyList.AccountKeyList[j];
            for k := 0 to l.Count - 1 do begin
              accl.Add(Cardinal(l[k]));
            end;
          end;
        end;
      end else begin
        i := cbMyPrivateKeys.ItemIndex-1;
        j := FOrderedAccountsKeyList.IndexOfAccountKey(FWalletKeys[i].AccountKey);
        if (j>=0) then begin
          l := FOrderedAccountsKeyList.AccountKeyList[j];
          for k := 0 to l.Count - 1 do begin
            accl.Add(Cardinal(l[k]));
          end;
        end;
      end;
    Finally
      FAccountsGrid.UnlockAccountsList;
    End;
    lblAccountsCount.Caption := inttostr(accl.Count);
  end else begin
    lblAccountsCount.Caption := inttostr(FNode.Bank.AccountsCount);
  end;
  // Show Totals:
  lblAccountsBalance.Caption := TAccountComp.FormatMoney(FAccountsGrid.AccountsBalance);
  UpdateOperations;
end;

procedure TFRMWallet.UpdateAvailableConnections;
Var i : integer;
 NC : TNetConnection;
 l : TList;
begin
  l := TNetData.NetData.NetConnections.LockList('TFRMWallet.UpdateAvailableConnections');
  try
    lbNetConnections.Items.BeginUpdate;
    Try
      lbNetConnections.Items.Clear;
      for i := 0 to l.Count - 1 do begin
        NC := l[i];
        if NC.Connected then begin
          if NC is TNetServerClient then begin
            if Not NC.IsMyselfServer then begin
              lbNetConnections.Items.AddObject(Format('Client: IP:%s:%s',[NC.Client.RemoteHost,NC.Client.RemotePort]),NC);
            end;
          end else begin
            if Not NC.IsMyselfServer then begin
              lbNetConnections.Items.AddObject(Format('Server: IP:%s:%s',[NC.Client.RemoteHost,NC.Client.RemotePort]),NC);
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
    mtl := FNode.MinerThreads.LockList('TFRMWallet.UpdateBlockChainState');
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
    FLog.SaveTypes := CT_TLogTypes_DEFAULT;
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
      StatusBar.Panels[2].Text := Format('Last Account time:%s',
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
          lblNodeStatus.Caption := 'Obtaining new BlockChain';
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
  last_i := cbMyPrivateKeys.ItemIndex;
  cbMyPrivateKeys.items.BeginUpdate;
  Try
    cbMyPrivateKeys.Items.Clear;
    cbMyPrivateKeys.Items.AddObject('(All my private keys)',TObject(-1));
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
  Finally
    cbMyPrivateKeys.Items.EndUpdate;
  End;
  if last_i<0 then last_i := 0;
  if cbMyPrivateKeys.Items.Count>last_i then cbMyPrivateKeys.ItemIndex := last_i
  else if cbMyPrivateKeys.Items.Count>=0 then cbMyPrivateKeys.ItemIndex := 0;
end;

initialization
  FRMWallet := Nil;
end.
