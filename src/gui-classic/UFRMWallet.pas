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
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

Const
  CM_PC_WalletKeysChanged = WM_USER + 1;
  CM_PC_NetConnectionUpdated = WM_USER + 2;

type

  { TFRMWallet }

  TFRMWallet = class(TForm)
    cbHashRateUnits: TComboBox;
    ebHashRateBackBlocks: TEdit;
    lblHashRateBackBlocks: TLabel;
    lblHashRateBackBlocks1: TLabel;
    MiRPCCalls: TMenuItem;
    pnlTop: TPanel;
    Image1: TImage;
    sbSearchAccount: TSpeedButton;
    StatusBar: TStatusBar;
    PageControl: TPageControl;
    tsMyAccounts: TTabSheet;
    tsOperations: TTabSheet;
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
    Label2: TLabel;
    ebFilterOperationsStartBlock: TEdit;
    ebFilterOperationsEndBlock: TEdit;
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
    lblMinersClients: TLabel;
    lblCurrentDifficultyCaption: TLabel;
    lblCurrentDifficulty: TLabel;
    lblTimeAverage: TLabel;
    Label4: TLabel;
    tsBlockChain: TTabSheet;
    Panel2: TPanel;
    Label9: TLabel;
    ebBlockChainBlockStart: TEdit;
    ebBlockChainBlockEnd: TEdit;
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
    dgBlockChainExplorer: TDrawGrid;
    dgOperationsExplorer: TDrawGrid;
    MiFindOperationbyOpHash: TMenuItem;
    MiAccountInformation: TMenuItem;
    MiOperationsExplorer: TMenuItem;
    procedure cbHashRateUnitsClick(Sender: TObject);
    procedure ebHashRateBackBlocksExit(Sender: TObject);
    procedure ebHashRateBackBlocksKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MiOperationsExplorerClick(Sender: TObject);
    procedure MiRPCCallsClick(Sender: TObject);
    procedure sbSearchAccountClick(Sender: TObject);
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
    procedure ebBlockChainBlockStartExit(Sender: TObject);
    procedure ebBlockChainBlockStartKeyPress(Sender: TObject; var Key: Char);
    procedure cbExploreMyAccountsClick(Sender: TObject);
    procedure MiCloseClick(Sender: TObject);
    procedure MiDecodePayloadClick(Sender: TObject);
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
    procedure MiFindOperationbyOpHashClick(Sender: TObject);
    procedure MiAccountInformationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Test_ShowDiagnosticTool(Sender: TObject);
  private
    FLastNodesCacheUpdatedTS : TDateTime;
    FBackgroundPanel : TPanel;
    FBackgroundLabel : TLabel;
    FMinersBlocksFound: Integer;
    procedure SetMinersBlocksFound(const Value: Integer);
    Procedure CheckIsReady;
    Procedure FinishedLoadingApp;
    Procedure FillAccountInformation(Const Strings : TStrings; Const AccountNumber : Cardinal);
    Procedure FillOperationInformation(Const Strings : TStrings; Const OperationResume : TOperationResume);
    Procedure InitMacOSMenu;
    Procedure InitMenuForTesting;
    {$IFDEF TESTNET}
    Procedure Test_RandomOperations(Sender: TObject);
    Procedure Test_ConnectDisconnect(Sender: TObject);
    {$ENDIF}
    {$IFDEF TESTING_NO_POW_CHECK}
    Procedure Test_CreateABlock(Sender: TObject);
    {$ENDIF}
    Procedure Test_ShowPublicKeys(Sender: TObject);
    Procedure Test_ShowOperationsInMemory(Sender: TObject);
    Procedure Test_FindAccountsForPrivateBuyOrSwapToMe(Sender : TObject);
    procedure OnAccountsGridUpdatedData(Sender : TObject);
  protected
    { Private declarations }
    FNode : TNode;
    FIsActivated : Boolean;
    FWalletKeys : TWalletKeysExt;
    FLog : TLog;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FAccountsGrid : TAccountsGrid;
    FSelectedAccountsGrid : TAccountsGrid;
    FOperationsAccountGrid : TOperationsGrid;
    FPendingOperationsGrid : TOperationsGrid;
    FOperationsExplorerGrid : TOperationsGrid;
    FBlockChainGrid : TBlockChainGrid;
    FMinerPrivateKeyType : TMinerPrivateKeyType;
    FUpdating : Boolean;
    FMessagesUnreadCount : Integer;
    FMinAccountBalance : Int64;
    FMaxAccountBalance : Int64;
    FPoolMiningServer : TPoolMiningServer;
    FRPCServer : TRPCServer;
    FMustProcessWalletChanged : Boolean;
    FMustProcessNetConnectionUpdated : Boolean;
    FThreadActivate : TObject;
    FLastAccountsGridInvalidateTC : TTickCount;
    Procedure OnNewAccount(Sender : TObject);
    Procedure OnReceivedHelloMessage(Sender : TObject);
    Procedure OnNetStatisticsChanged(Sender : TObject);
    procedure OnNewLog(logtype : TLogType; Time : TDateTime; ThreadID : TThreadID; Const sender, logtext : String);
    procedure OnWalletChanged(Sender : TObject);
    procedure OnNetConnectionsUpdated(Sender : TObject);
    procedure OnNetNodeServersUpdated(Sender : TObject);
    procedure OnNetBlackListUpdated(Sender : TObject);
    Procedure OnNodeMessageEvent(NetConnection : TNetConnection; MessageData : String);
    Procedure OnNodeKeysActivity(Sender : TObject);
    Procedure OnSelectedAccountsGridUpdated(Sender : TObject);
    Procedure OnMiningServerNewBlockFound(Sender : TObject);
    Procedure UpdateConnectionStatus;
    Procedure UpdateAccounts(RefreshData : Boolean);
    Procedure UpdateBlockChainState;
    Procedure UpdatePrivateKeys;
    Procedure UpdateOperations;
    Procedure UpdateConfigChanged(Sender:TObject);
    Procedure UpdateNodeStatus;
    Procedure UpdateAvailableConnections;
    procedure Activate; override;
    Function ForceMining : Boolean; virtual;
    Function GetAccountKeyForMiner : TAccountKey;
    Procedure DoUpdateAccounts;
    Function DoUpdateAccountsFilter : Boolean;
    procedure CM_WalletChanged(var Msg: TMessage); message CM_PC_WalletKeysChanged;
    procedure CM_NetConnectionUpdated(var Msg: TMessage); message CM_PC_NetConnectionUpdated;
  public
    { Public declarations }
    Property WalletKeys : TWalletKeysExt read FWalletKeys;
    Property MinersBlocksFound : Integer read FMinersBlocksFound write SetMinersBlocksFound;
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
  {$IFDEF USE_ABSTRACTMEM_BLOCKCHAIN_STORAGE}
  UAbstractMemBlockchainStorage,
  {$ENDIF}
  UThread, UOpTransaction, UFRMPascalCoinWalletConfig,
  UFRMOperationsExplorer,
  {$IFDEF TESTNET}
  UFRMRandomOperations,
  UFRMDiagnosticTool,
  {$ENDIF}
  UPCTNetDataExtraMessages,
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
  end else FRMWallet.UpdateNodeStatus;
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
  TAbstractMemBlockchainStorage( TNode.Node.Bank.Storage ).AutoFlushCache := False;
  TNode.Node.InitSafeboxAndOperations($FFFFFFFF,OnProgressNotify); // New Build 2.1.4 to load pending operations buffer
  TAbstractMemBlockchainStorage( TNode.Node.Bank.Storage ).AutoFlushCache := True;
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

    UpdateConfigChanged(Self);

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
    {$IFDEF USE_ABSTRACTMEM_BLOCKCHAIN_STORAGE}
    FNode.Bank.StorageClass := TAbstractMemBlockchainStorageSecondary;
    {$ELSE}
    FNode.Bank.StorageClass := TFileStorage;
    {$ENDIF}
    FNode.Bank.Storage.Initialize;
    // Init Grid

    FSelectedAccountsGrid.Node := FNode;
    FWalletKeys.OnChanged.Add( OnWalletChanged );
    FAccountsGrid.Node := FNode;
    FOperationsAccountGrid.Node := FNode;
    FBlockChainGrid.HashRateAverageBlocksCount := TSettings.HashRateAvgBlocksCount;
    i := Integer(TSettings.ShowHashRateAs);
    if (i<Integer(Low(TShowHashRateAs))) Or (i>Integer(High(TShowHashRateAs))) then i := Integer({$IFDEF TESTNET}hr_Mega{$ELSE}hr_Tera{$ENDIF});
    FBlockChainGrid.HashRateAs := TShowHashRateAs(i);
    // Reading database
    FThreadActivate := TThreadActivate.Create(true);
    TThreadActivate(FThreadActivate).FreeOnTerminate := true;
    TThreadActivate(FThreadActivate).Suspended := False;
    UpdateNodeStatus;
    TPCTNetDataExtraMessages.InitNetDataExtraMessages(FNode,TNetData.NetData,FWalletKeys);
  Except
    On E:Exception do begin
      E.Message := 'An error occurred during initialization. Application cannot continue:'+#10+#10+E.Message+#10+#10+'Application will close...';
      Application.MessageBox(PChar(E.Message),PChar(Application.Title),MB_ICONERROR+MB_OK);
      Halt;
    end;
  end;
  UpdatePrivateKeys;
  UpdateAccounts(false);
  if TSettings.FirstTime then begin
    TSettings.FirstTime := false;
    miAboutPascalCoinClick(Nil);
  end;
  PageControlChange(Nil);
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
  i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
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
    DefaultFee := TSettings.DefaultFee;
    WalletKeys := FWalletKeys;
    ShowModal;
  Finally
    Free;
  End;
end;

procedure TFRMWallet.bbSendAMessageClick(Sender: TObject);
Var basem,m : String;
  them, errors : String;
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
  for i := Low(basem) to High(basem) do begin
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

procedure TFRMWallet.cbMyPrivateKeysChange(Sender: TObject);
begin
  UpdateAccounts(true);
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
  UpdatePrivateKeys;
  FMustProcessWalletChanged := false;
end;

procedure TFRMWallet.dgAccountsClick(Sender: TObject);
begin
  UpdateOperations;
end;

procedure TFRMWallet.dgAccountsColumnMoved(Sender: TObject; FromIndex, ToIndex: Integer);
begin
  TSettings.Save;
end;

procedure TFRMWallet.dgAccountsFixedCellClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  TSettings.Save;
end;

procedure TFRMWallet.DoUpdateAccounts;
begin
  UpdateAccounts(true);
end;

function TFRMWallet.DoUpdateAccountsFilter: Boolean;
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
    if bmin>0 then
      ebFilterAccountByBalanceMin.Text:=TAccountComp.FormatMoney(bmin)
    else ebFilterAccountByBalanceMin.Text := '';
    if bmax<CT_MaxWalletAmount then
      ebFilterAccountByBalanceMax.Text := TAccountComp.FormatMoney(bmax)
    else ebFilterAccountByBalanceMax.Text := '';
    if cbFilterAccounts.Checked then begin
      FMinAccountBalance := bmin;
      FMaxAccountBalance := bmax;
      ebFilterAccountByBalanceMin.ParentFont := true;
      ebFilterAccountByBalanceMax.ParentFont := true;
    end else begin
      FMinAccountBalance := 0;
      FMaxAccountBalance := CT_MaxWalletAmount;
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
var bstart,bend : Int64;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    bstart := StrToInt64Def(ebBlockChainBlockStart.Text,-1);
    bend := StrToInt64Def(ebBlockChainBlockEnd.Text,-1);
    FBlockChainGrid.SetBlocks(bstart,bend);
    if FBlockChainGrid.BlockStart>=0 then ebBlockChainBlockStart.Text := Inttostr(FBlockChainGrid.BlockStart) else ebBlockChainBlockStart.Text := '';
    if FBlockChainGrid.BlockEnd>=0 then ebBlockChainBlockEnd.Text := Inttostr(FBlockChainGrid.BlockEnd) else ebBlockChainBlockEnd.Text := '';
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
Var bstart,bend : Int64;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    bstart := StrToInt64Def(ebFilterOperationsStartBlock.Text,-1);
    if bstart>=0 then ebFilterOperationsStartBlock.Text := Inttostr(bstart) else ebFilterOperationsStartBlock.Text := '';
    bend := StrToInt64Def(ebFilterOperationsEndBlock.Text,-1);
    if bend>=0 then ebFilterOperationsEndBlock.Text := Inttostr(bend) else ebFilterOperationsEndBlock.Text := '';
    FOperationsExplorerGrid.SetBlocks(bstart,bend);
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
  LAccountNameRawValue : TRawBytes;
  LErrors : String;
  LAccNames : TOrderedRawList;
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
    LAccountNameRawValue.FromString(ebFindAccountNumber.Text);
    LAccNames := TOrderedRawList.Create;
    Try
      if FNode.Bank.SafeBox.FindAccountsStartingByName(LAccountNameRawValue,LAccNames,1)>0 then begin
        an := LAccNames.GetTag(0);
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
    Finally
      LAccNames.Free;
    End;
  end;
end;

procedure TFRMWallet.ebFindAccountNumberExit(Sender: TObject);
begin
  ebFindAccountNumber.Text := '';
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
  TNetData.NetData.OnBlackListUpdated := OnNetBlackListUpdated;
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

  UpdatePrivateKeys;
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
    // TODO: Wallet has no PASA ...
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


procedure TFRMWallet.InitMenuForTesting;
var mi : TMenuItem;
begin
{$IFDEF TESTNET}
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='-';
  miAbout.Add(mi);
  {$IFDEF TESTING_NO_POW_CHECK}
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Create a block';
  mi.OnClick:=Test_CreateABlock;
  {$IFnDEF FPC}
  mi.ShortCut := TextToShortCut('CTRL+B');
  {$ENDIF}
  miAbout.Add(mi);
  {$ENDIF}
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Connect/Disconnect';
  mi.OnClick:=Test_ConnectDisconnect;
  {$IFnDEF FPC}
  mi.ShortCut := TextToShortCut('CTRL+D');
  {$ENDIF}
  miAbout.Add(mi);

  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Create Random operations';
  mi.OnClick:=Test_RandomOperations;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Diagnostic Tool';
  mi.OnClick:=Test_ShowDiagnosticTool;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Show public keys state';
  mi.OnClick:=Test_ShowPublicKeys;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Show operations in memory';
  mi.OnClick:=Test_ShowOperationsInMemory;
  miAbout.Add(mi);
  mi := TMenuItem.Create(MainMenu);
  mi.Caption:='Search accounts for private or swap to me';
  mi.OnClick:=Test_FindAccountsForPrivateBuyOrSwapToMe;
  miAbout.Add(mi);
{$ELSE}
{$ENDIF}
end;

{$IFDEF TESTING_NO_POW_CHECK}
procedure TFRMWallet.Test_CreateABlock(Sender: TObject);
var ops, mempoolOps : TPCOperationsComp;
  nba : TBlockAccount;
  errors : String;

begin
  {$IFDEF TESTNET}
  ops := TPCOperationsComp.Create(Nil);
  Try
    ops.bank := FNode.Bank;
    mempoolOps := FNode.LockMempoolRead;
    try
      ops.CopyFrom(mempoolOps);
    finally
      FNode.UnlockMempoolRead;
    end;
    ops.BlockPayload.FromString(IntToStr(FNode.Bank.BlocksCount));
    ops.nonce := FNode.Bank.BlocksCount;
    ops.UpdateTimestamp;
    FNode.AddNewBlockChain(Nil,ops,errors);
  finally
    ops.Free;
  end;
  {$ELSE}
  Raise Exception.Create('NOT ALLOWED!');
  {$ENDIF}
end;
{$ENDIF}

{$IFDEF TESTNET}

procedure TFRMWallet.Test_ConnectDisconnect(Sender: TObject);
begin
  TNetData.NetData.NetConnectionsActive := Not TNetData.NetData.NetConnectionsActive;
  Exit;
  if FNode.NetServer.Active then begin
    FNode.NetServer.Active := False;
  end else begin
    FNode.NetServer.Active := True;
  end;
end;

procedure TFRMWallet.Test_RandomOperations(Sender: TObject);
Var FRM : TFRMRandomOperations;
begin
  FRM := TFRMRandomOperations.Create(Self);
  Try
    FRM.SourceNode := FNode;
    FRM.SourceWalletKeys := FWalletKeys;
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

{$ENDIF}

procedure TFRMWallet.Test_ShowOperationsInMemory(Sender: TObject);
var LFRM : TFRMMemoText;
  i, nOps : Integer;
  Lslist : TStrings;
begin
  Lslist := TStringList.Create;
  try
    TPCOperationsStorage.PCOperationsStorage.GetStats(Lslist);
    nOps := TPCOperationsStorage.PCOperationsStorage.Count;
    LFRM := TFRMMemoText.Create(Self);
    try
      LFRM.InitData('Operations in Memory '+IntToStr(nOps),Lslist.Text);
      LFRM.ShowModal;
    finally
      LFRM.Free;
    end;
  finally
    Lslist.Free;
  end;
end;

procedure TFRMWallet.Test_ShowPublicKeys(Sender: TObject);
var F : TFRMMemoText;
  i : Integer;
  sl : TStrings;
  ak : TAccountKey;
  nmin,nmax : Integer;
  l : TList<Pointer>;
  Pacsd : PAccountKeyStorageData;
  acc : TAccount;
begin
  sl := TStringList.Create;
  try
    for i:=0 to FNode.Bank.SafeBox.AccountsCount-1 do begin
      acc := FNode.Bank.SafeBox.Account(i);
      if acc.accountInfo.new_publicKey.EC_OpenSSL_NID<>0 then begin
        sl.Add(Format('Account %d new public key %d %s',[acc.account,
          acc.accountInfo.new_publicKey.EC_OpenSSL_NID,
          TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(acc.accountInfo.new_publicKey))]));
      end;
    end;
    {$IFnDEF USE_ABSTRACTMEM}
    l := TAccountKeyStorage.KS.LockList;
    try
      sl.Add(Format('%d public keys in TAccountKeyStorage data',[l.count]));
      for i:=0 to l.count-1 do begin
        Pacsd := l[i];
        if (Pacsd^.counter<=0) then begin
          sl.Add(Format('%d/%d public keys counter %d',[i+1,l.count,Pacsd^.counter]));
        end;
        if FNode.Bank.SafeBox.OrderedAccountKeysList.IndexOfAccountKey(Pacsd^.ptrAccountKey^)<0 then begin
          sl.Add(Format('%d/%d public keys counter %d Type %d NOT FOUND %s',[i+1,l.count,Pacsd^.counter,
          Pacsd^.ptrAccountKey^.EC_OpenSSL_NID,
          TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(Pacsd^.ptrAccountKey^))]));
        end;
      end;
    finally
      TAccountKeyStorage.KS.UnlockList;
    end;
    sl.Add(Format('%d public keys in %d accounts',[FNode.Bank.SafeBox.OrderedAccountKeysList.Count,FNode.Bank.Safebox.AccountsCount]));
    for i:=0 to FNode.Bank.SafeBox.OrderedAccountKeysList.Count-1 do begin
      ak := FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKey[i];
      if ( FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Count > 0) then begin
        nmin := FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Get(0);
        nmax := FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Get( FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Count-1 );
      end else begin
        nmin := -1; nmax := -1;
      end;
      sl.Add(Format('%d/%d %d accounts (%d to %d) for key type %d %s',[
        i+1,FNode.Bank.SafeBox.OrderedAccountKeysList.Count,
        FNode.Bank.SafeBox.OrderedAccountKeysList.AccountKeyList[i].Count,
        nmin,nmax,
        ak.EC_OpenSSL_NID,
        TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(ak)) ]));
    end;
    {$ENDIF}
    F := TFRMMemoText.Create(Self);
    try
      F.InitData('Keys in safebox',sl.Text);
      F.ShowModal;
    finally
      F.Free;
    end;
  finally
    sl.Free;
  end;
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
  FLastAccountsGridInvalidateTC := TPlatform.GetTickCount;
  FLastNodesCacheUpdatedTS := Now;
  FBackgroundPanel := Nil;
  FBackgroundLabel := Nil;
  FThreadActivate := Nil;
  FMustProcessWalletChanged := false;
  FMustProcessNetConnectionUpdated := false;
  FRPCServer := Nil;
  FNode := Nil;
  FPoolMiningServer := Nil;
  FMinAccountBalance := 0;
  FMaxAccountBalance := CT_MaxWalletAmount;
  FMessagesUnreadCount := 0;
  lblReceivedMessages.Visible := false;
  memoNetConnections.Lines.Clear;
  memoNetServers.Lines.Clear;
  memoNetBlackLists.Lines.Clear;
  memoMessages.Lines.Clear;
  memoMessageToSend.Lines.Clear;
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
  FAccountsGrid := TAccountsGrid.Create(Self);
  FAccountsGrid.DrawGrid := dgAccounts;
  FAccountsGrid.AllowMultiSelect := True;
  FAccountsGrid.OnAccountsGridUpdatedData := OnAccountsGridUpdatedData;
  FAccountsGrid.AccountsGridDatasource := acds_Node;
  FSelectedAccountsGrid := TAccountsGrid.Create(Self);
  FSelectedAccountsGrid.AccountsGridDatasource := acds_InternalList;
  FSelectedAccountsGrid.DrawGrid := dgSelectedAccounts;
  FSelectedAccountsGrid.OnUpdated := OnSelectedAccountsGridUpdated;
  FOperationsAccountGrid := TOperationsGrid.Create(Self);
  FOperationsAccountGrid.DrawGrid := dgAccountOperations;
  FOperationsAccountGrid.MustShowAlwaysAnAccount := true;
  FOperationsAccountGrid.WalletKeys := FWalletKeys;
  FPendingOperationsGrid := TOperationsGrid.Create(Self);
  FPendingOperationsGrid.DrawGrid := dgPendingOperations;
  FPendingOperationsGrid.AccountNumber := -1; // all
  FPendingOperationsGrid.PendingOperations := true;
  FPendingOperationsGrid.WalletKeys := FWalletKeys;
  FOperationsExplorerGrid := TOperationsGrid.Create(Self);
  FOperationsExplorerGrid.DrawGrid := dgOperationsExplorer;
  FOperationsExplorerGrid.AccountNumber := -1;
  FOperationsExplorerGrid.PendingOperations := False;
  FOperationsExplorerGrid.WalletKeys := FWalletKeys;
  FBlockChainGrid := TBlockChainGrid.Create(Self);
  FBlockChainGrid.DrawGrid := dgBlockChainExplorer;
  FBlockChainGrid.ShowTimeAverageColumns:={$IFDEF SHOW_AVERAGE_TIME_STATS}True;{$ELSE}False;{$ENDIF}
  // FWalletKeys.OnChanged.Add( OnWalletChanged );
  {$IFDEF USE_GNUGETTEXT}
  // use language from the params and retranslate if needed
  // might be better to move this a bit earlier in the formcreate routine
  UseLanguage(TSettings.AppParams.ParamByName[CT_PARAM_UILanguage].GetAsString(GetCurrentLanguage));
  RetranslateComponent(self);
  {$ENDIF}
  //
  UpdatePrivateKeys;
  UpdateBlockChainState;
  UpdateConnectionStatus;
  PageControl.ActivePage := tsOperations;
  pcAccountsOptions.ActivePage := tsAccountOperations;
  ebFilterOperationsStartBlock.Text := '';
  ebFilterOperationsEndBlock.Text := '';
  cbExploreMyAccounts.Checked:=True; // By default
  cbExploreMyAccountsClick(nil);

  MinersBlocksFound := 0;
  lblBuild.Caption := 'Build: '+CT_ClientAppVersion;
  {$IFDEF TESTNET}
  lblBuild.Font.Color := clRed;
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
  cbHashRateUnits.Items.Clear;
  cbHashRateUnits.Items.Add('h/s');
  cbHashRateUnits.Items.Add('Kh/s');
  cbHashRateUnits.Items.Add('Mh/s');
  cbHashRateUnits.Items.Add('Gh/s');
  cbHashRateUnits.Items.Add('Th/s');
  cbHashRateUnits.Items.Add('Ph/s');
  cbHashRateUnits.Items.Add('Eh/s');
  // Things for testing purposes only
  InitMenuForTesting;
  {$ifdef DARWIN}
  // this is macOS specific menu layout
  InitMacOSMenu;
  {$endif}
  PageControl.ActivePageIndex := 0;
  {$IFDEF DEBUG}
  System.ReportMemoryLeaksOnShutdown := True; // Delphi memory leaks testing
  {$ENDIF}
end;

procedure TFRMWallet.ebHashRateBackBlocksKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then ebHashRateBackBlocksExit(Nil);
end;

procedure TFRMWallet.ebHashRateBackBlocksExit(Sender: TObject);
var i : Integer;
begin
  If FUpdating then exit;
  FUpdating := True;
  Try
    i := StrToIntDef(ebHashRateBackBlocks.Text,-1);
    FBlockChainGrid.HashRateAverageBlocksCount:=i;
    TSettings.HashRateAvgBlocksCount := FBlockChainGrid.HashRateAverageBlocksCount;
  Finally
    ebHashRateBackBlocks.Text := IntToStr(FBlockChainGrid.HashRateAverageBlocksCount);
    FUpdating := false;
  End;
end;

procedure TFRMWallet.cbHashRateUnitsClick(Sender: TObject);
begin
  If FUpdating then Exit;
  FUpdating := True;
  Try
    case cbHashRateUnits.ItemIndex of
      0 : FBlockChainGrid.HashRateAs := hr_Unit;
      1 : FBlockChainGrid.HashRateAs := hr_Kilo;
      2 : FBlockChainGrid.HashRateAs := hr_Mega;
      3 : FBlockChainGrid.HashRateAs := hr_Giga;
      4 : FBlockChainGrid.HashRateAs := hr_Tera;
      5 : FBlockChainGrid.HashRateAs := hr_Peta;
      6 : FBlockChainGrid.HashRateAs := hr_Exa;
    else FBlockChainGrid.HashRateAs := hr_Mega;
    end;
    TSettings.ShowHashRateAs := FBlockChainGrid.HashRateAs;
  Finally
    FUpdating := false;
  End;
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
  FAccountsGrid.Node := Nil;
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

procedure TFRMWallet.sbSearchAccountClick(Sender: TObject);
Var F : TFRMAccountSelect;
begin
  F := TFRMAccountSelect.Create(Self);
  try
    F.Node := FNode;
    F.WalletKeys := FWalletKeys;
    F.ShowModal;
  finally
    F.Free;
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

procedure TFRMWallet.lblReceivedMessagesClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMessages;
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
        opr := FOperationsExplorerGrid.OperationsResume.Items[i-1];
      end;
    end else if PageControl.ActivePage=tsPendingOperations then begin
      i := FPendingOperationsGrid.DrawGrid.Row;
      if (i>0) and (i<=FPendingOperationsGrid.OperationsResume.Count) then begin
        opr := FPendingOperationsGrid.OperationsResume.Items[i-1];
      end;
    end else if PageControl.ActivePage=tsMyAccounts then begin
      accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
      if accn<0 then raise Exception.Create('Select an account');
      FillAccountInformation(strings,accn);
      title := 'Account '+TAccountComp.AccountNumberToAccountTxtNumber(accn)+' info';
      i := FOperationsAccountGrid.DrawGrid.Row;
      if (i>0) and (i<=FOperationsAccountGrid.OperationsResume.Count) then begin
        opr := FOperationsAccountGrid.OperationsResume.Items[i-1];
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
    FOperationsExplorerGrid.ShowModalDecoder(FWalletKeys, TSettings.AppParams);
  end else if PageControl.ActivePage=tsPendingOperations then begin
    FPendingOperationsGrid.ShowModalDecoder(FWalletKeys,TSettings.AppParams);
  end else if PageControl.ActivePage=tsMyAccounts then begin
    FOperationsAccountGrid.ShowModalDecoder(FWalletKeys,TSettings.AppParams);
  end;
end;

procedure TFRMWallet.Test_FindAccountsForPrivateBuyOrSwapToMe(Sender: TObject);
{ This procedure will search in Safebox all accounts in "for_private_sale" state
  or in "for_account_swap" that can be self-signed using one of my private keys
  }

  function CaptureSender0Coins(var AAccountSender0Coins : TAccount; var ANeededWalletKey : TWalletKey) : Boolean;
  var ii : Integer;
  begin
    //
    Result := False;
    for ii := 0 to WalletKeys.AccountsKeyList.Count-1 do begin
      if WalletKeys.AccountsKeyList.AccountKeyList[ii].Count>0 then begin
        if WalletKeys.TryGetKey(WalletKeys.AccountsKeyList.AccountKey[ii],ANeededWalletKey) then begin
          AAccountSender0Coins := FNode.Bank.SafeBox.Account( WalletKeys.AccountsKeyList.AccountKeyList[ii].Get(0) );
          Result := True;
        end;
      end;
    end;

  end;


var i : Integer;
  LLines : TStrings;
  LAccount, LAccountSender0Coins : TAccount;
  LAccountOpDesc : String;
  LCountAccountsFound_total, LCountAccountsFound_Operation : Integer;
  LNeededWalletKey : TWalletKey;
  s : String;
  LOpTransaction : TOpTransaction;
  LOperationsHashTree, LGlobalOperationsHashTree : TOperationsHashTree;
  LStream : TStream;
  LRaw : TRawBytes;
  LFRM : TFRMMemoText;
  LOpPayload : TOperationPayload;
begin
  if Not WalletKeys.IsValidPassword then raise Exception.Create('Your wallet keys are locked');
  LOpPayload := CT_TOperationPayload_NUL;
  if InputQuery('Search ATOMIC SWAP by SECRET','Insert SECRET value (use 0x... for Hexadecimal, otherwise will be a String)',s) then begin
    if s.StartsWith('0x') then begin
      if not UCommon.TryHex2Bytes(s,LOpPayload.payload_raw) then raise Exception.Create('SECRET value is not an Hexadecimal'+#10+s);
    end else begin
      LOpPayload.payload_raw.FromString(s);
    end;
  end;
  LCountAccountsFound_total := 0;
  LCountAccountsFound_Operation := 0;
  LLines := TStringList.Create;
  LGlobalOperationsHashTree := TOperationsHashTree.Create;
  try
    for i:=0 to FNode.Bank.SafeBox.AccountsCount-1 do begin
      LAccountOpDesc := '';
      LAccount := FNode.Bank.SafeBox.Account(i);

      LOpTransaction := Nil;
      Try
        Case LAccount.accountInfo.state of
          as_ForSale : begin
            if Not TAccountComp.IsNullAccountKey( LAccount.accountInfo.new_publicKey ) then begin
              if Not WalletKeys.TryGetKey(LAccount.accountInfo.new_publicKey,LNeededWalletKey) then Continue;
              if Not Assigned(LNeededWalletKey.PrivateKey) then Continue; // Key not available!
            end else Continue;
            // Private sale to me
            // Is in time?
            if TAccountComp.IsAccountLocked(LAccount.accountInfo,FNode.Bank.BlocksCount) then begin
              //
              if LAccount.balance>=LAccount.accountInfo.price then begin
                LAccountOpDesc := Format('Account %s is for private sale to me and with enough balance to pay %s (balance %s)',[
                  TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),
                  TAccountComp.FormatMoney(LAccount.accountInfo.price),
                  TAccountComp.FormatMoney(LAccount.balance)]);
                // No key needed... just a transaction SELF SIGNED
                LOpTransaction := TOpBuyAccount.CreateBuy(FNode.Bank.SafeBox.CurrentProtocol,
                    LAccount.account, LAccount.n_operation+1,
                    LAccount.account, LAccount.accountInfo.account_to_pay, LAccount.accountInfo.price,
                    0,0,
                    LAccount.accountInfo.new_publicKey,
                    LNeededWalletKey.PrivateKey,
                    LOpPayload);
              end else begin
                LAccountOpDesc := Format('Account %s is for private sale to me but needs a Buy operation paying %s PASC (%s pending)',[
                   TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),
                   TAccountComp.FormatMoney(LAccount.accountInfo.price),
                   TAccountComp.FormatMoney(LAccount.accountInfo.price - LAccount.balance)]);
              end;
            end else begin
              LAccountOpDesc := Format('Account %s is for private sale to me but is out-of-lock period',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account)]);
            end;
          end;
          as_ForAtomicAccountSwap : begin
            if Not WalletKeys.TryGetKey(LAccount.accountInfo.new_publicKey,LNeededWalletKey) then Continue;
            if TAccountComp.IsAccountLocked(LAccount.accountInfo,FNode.Bank.BlocksCount) then begin
              if TAccountComp.IsValidAccountInfoHashLockKey(LAccount.accountInfo,LOpPayload.payload_raw) then begin
                // Atomic Account swap using provided SECRET
                LAccountOpDesc := Format('Account %s is for Atomic Account Swap to me using SECRET %s',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
                //
                // No key needed... just a Buy Operation SELF SIGNED
                LOpTransaction := TOpBuyAccount.CreateBuy(FNode.Bank.SafeBox.CurrentProtocol,
                    LAccount.account, LAccount.n_operation+1,
                    LAccount.account, LAccount.accountInfo.account_to_pay, LAccount.accountInfo.price,
                    0,0,
                    LAccount.accountInfo.new_publicKey,
                    LNeededWalletKey.PrivateKey,
                    LOpPayload);
              end else begin
                LAccountOpDesc := Format('Account %s is for Atomic Account Swap to me but SECRET %s is not valid',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
              end;
            end else begin
              LAccountOpDesc := Format('Account %s is for Atomic Account Swap to me but is out-of-lock period',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account)]);
            end;

          end;
          as_ForAtomicCoinSwap : begin
            if Not TAccountComp.IsValidAccountInfoHashLockKey(LAccount.accountInfo,LOpPayload.payload_raw) then Continue;
            // Atomic Coin swap using provided SECRET
            if TAccountComp.IsAccountLocked(LAccount.accountInfo,FNode.Bank.BlocksCount) then begin
              // Single transaction using amount 0 from ANY sender
              if CaptureSender0Coins(LAccountSender0Coins,LNeededWalletKey) then begin
                // Atomic Account swap using provided SECRET
                LAccountOpDesc := Format('Account %s is for Atomic Coin Swap to me using SECRET %s',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
                //
                // No key needed... just a transaction SELF SIGNED
                LOpTransaction := TOpTransaction.CreateTransaction(FNode.Bank.SafeBox.CurrentProtocol,
                    LAccountSender0Coins.account, LAccountSender0Coins.n_operation+1,
                    LAccount.account,
                    LNeededWalletKey.PrivateKey,
                    0,0, // No Amount no Fee
                    LOpPayload);
              end else begin
                LAccountOpDesc := Format('Account %s is for Atomic Coin Swap using SECRET %s but I have no key to sign',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
              end;
            end else begin
              LAccountOpDesc := Format('Account %s is for Atomic Coin Swap using SECRET %s but is out-of-lock period',[TAccountComp.AccountNumberToAccountTxtNumber(LAccount.account),UCommon.Bytes2Hex(LOpPayload.payload_raw,True)]);
            end;
          end;
        else Continue;
        End;

        // Do
        Inc(LCountAccountsFound_total);
        LLines.Add(Format('%s',[LAccountOpDesc]));
        if Assigned(LOpTransaction) then begin
          Inc(LCountAccountsFound_Operation);
          LOperationsHashTree := TOperationsHashTree.Create;
          LStream := TMemoryStream.Create;
          try
            LOperationsHashTree.AddOperationToHashTree(LOpTransaction);
            LGlobalOperationsHashTree.AddOperationToHashTree(LOpTransaction);
            LLines.Add(Format('Operation: %s',[LOpTransaction.ToString]));
            LOperationsHashTree.SaveOperationsHashTreeToStream(LStream,False);
            LRaw.FromStream(LStream);
            LLines.Add(Format('rawoperations (for JSON-RPC call): %s',[LRaw.ToHexaString]));
          finally
            LOperationsHashTree.Free;
            LStream.Free;
          end;
        end;

      Finally
        FreeAndNil(LOpTransaction);
      End;
    end; // For
    LLines.Add('');
    LLines.Add(Format('Found %d of %d available account from a Safebox with %d accounts',[
      LCountAccountsFound_Operation,
      LCountAccountsFound_total,
      FNode.Bank.SafeBox.AccountsCount]));
    LStream := TMemoryStream.Create;
    try
      LGlobalOperationsHashTree.SaveOperationsHashTreeToStream(LStream,False);
      LRaw.FromStream(LStream);
      LLines.Add(Format('rawoperations (for JSON-RPC call) of %d operations: %s',[LGlobalOperationsHashTree.OperationsCount, LRaw.ToHexaString]));
    finally
      LStream.Free;
    end;
    //
    LFRM := TFRMMemoText.Create(Self);
    try
      LFRM.InitData('',LLines.Text);
      LFRM.ShowModal;
    finally
      LFRM.Free;
    end;
  finally
    LLines.Free;
    LGlobalOperationsHashTree.Free;
  end;
end;

procedure TFRMWallet.Test_ShowDiagnosticTool(Sender: TObject);
{$IFDEF TESTNET}
var
 LDialog : TFRMDiagnosticTool;
{$ENDIF}
begin
{$IFDEF TESTNET}
  LDialog := TFRMDiagnosticTool.Create(Nil);
  try
    LDialog.ShowModal;
  finally
    LDialog.Free;
  end;
{$ENDIF}
end;

procedure TFRMWallet.MiFindaccountClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  ebFindAccountNumber.SetFocus;
end;

procedure TFRMWallet.MiFindnextaccountwithhighbalanceClick(Sender: TObject);
Var an  : Cardinal;
  an64 : Int64;
  start : TAccount;
begin
  PageControl.ActivePage := tsMyAccounts;
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
  PageControl.ActivePage := tsMyAccounts;
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  bbSelectedAccountsOperationClick(Sender);
end;

procedure TFRMWallet.miNewOperationClick(Sender: TObject);
var l : TOrderedCardinalList;
begin
  CheckIsReady;
  With TFRMOperation.Create(Self) do
  Try
    l := TOrderedCardinalList.Create;
    try
      If FAccountsGrid.SelectedAccounts(l)<1 then raise Exception.Create('No row selected');
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
    UpdatePrivateKeys;
  Finally
    FRM.Free;
  End;
end;

procedure TFRMWallet.MiRemoveaccountfromselectedClick(Sender: TObject);
begin
  PageControl.ActivePage := tsMyAccounts;
  PageControlChange(Nil);
  pcAccountsOptions.ActivePage := tsMultiSelectAccounts;
  sbSelectedAccountsDelClick(Sender);
end;

procedure TFRMWallet.OnAccountsGridUpdatedData(Sender: TObject);
begin
  if FAccountsGrid.IsUpdatingData then begin
    lblAccountsCount.Caption := '(Calculating)';
    lblAccountsBalance.Caption := '(Calculating)';
  end else begin
    lblAccountsCount.Caption := IntToStr(FAccountsGrid.AccountsCount);
    lblAccountsBalance.Caption := TAccountComp.FormatMoney(FAccountsGrid.AccountsBalance);
  end;
end;

procedure TFRMWallet.OnMiningServerNewBlockFound(Sender: TObject);
begin
  FPoolMiningServer.MinerAccountKey := GetAccountKeyForMiner;
end;

procedure TFRMWallet.OnNetBlackListUpdated(Sender: TObject);
Const CT_TRUE_FALSE : Array[Boolean] Of AnsiString = ('FALSE','TRUE');
Var i,j,n : integer;
 P : PNodeServerAddress;
 l : TList<Pointer>;
 strings : TStrings;
begin
  l := TNetData.NetData.NodeServersAddresses.LockList;
  try
    strings := memoNetBlackLists.Lines;
    strings.BeginUpdate;
    Try
      strings.Clear;
      strings.Add('BlackList Updated: '+DateTimeToStr(now)+' by TID:'+IntToHex(PtrInt(TThread.CurrentThread.ThreadID),8));
      j := 0; n:=0;
      for i := 0 to l.Count - 1 do begin
        P := l[i];
        if (P^.is_blacklisted) then begin
          inc(n);
          if Not P^.its_myself then begin
            inc(j);
            strings.Add(Format('Blacklist IP:%s:%d LastConnection:%s Reason: %s',
              [
               P^.ip,P^.port,
               DateTimeToStr(UnivDateTime2LocalDateTime( UnixToUnivDateTime(P^.last_connection))),P^.BlackListText]));
          end;
        end;
      end;
      Strings.Add(Format('Total Blacklisted IPs: %d (Total %d)',[j,n]));
    Finally
      strings.EndUpdate;
    End;
  finally
    TNetData.NetData.NodeServersAddresses.UnlockList;
  end;
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
    strings := memoNetServers.Lines;
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
    UpdateAccounts(false);
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
  memoLogs.Lines.Add(formatDateTime('dd/mm/yyyy hh:nn:ss.zzz',Time)+s+IntToHex(PtrInt(ThreadID),8)+' ['+CT_LogType[Logtype]+'] <'+sender+'> '+logtext);
  //
end;

procedure TFRMWallet.OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: String);
Var s : String;
begin
  inc(FMessagesUnreadCount);
  if Assigned(NetConnection) then begin
    s := DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr;
    memoMessages.Lines.Add(DateTimeToStr(now)+' Message received from '+NetConnection.ClientRemoteAddr+' Length '+inttostr(Length(MessageData))+' bytes');
    memoMessages.Lines.Add('RECEIVED> '+MessageData);
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
    memoMessages.Lines.Add(DateTimeToStr(now)+' Internal message: '+MessageData);
  end;
  if FMessagesUnreadCount>1 then lblReceivedMessages.Caption := Format('You have received %d messages',[FMessagesUnreadCount])
  else lblReceivedMessages.Caption := 'You have received 1 message';
  lblReceivedMessages.Visible := true;
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
  lblSelectedAccountsCount.Caption := Inttostr(FSelectedAccountsGrid.AccountsCount);
  lblSelectedAccountsBalance.Caption := TAccountComp.FormatMoney( FSelectedAccountsGrid.AccountsBalance );
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
  if PageControl.ActivePage=tsBlockChain then FBlockChainGrid.Node := FNode
  else FBlockChainGrid.Node := Nil;
  if PageControl.ActivePage=tsOperations then begin
    FOperationsExplorerGrid.Node := FNode;
    MiDecodePayload.Enabled := true;
  end else FOperationsExplorerGrid.Node := Nil;
  if PageControl.ActivePage=tsMessages then begin
    UpdateAvailableConnections;
    FMessagesUnreadCount := 0;
    lblReceivedMessages.Visible := false;
  end;
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
        if FWalletKeys.IndexOfAccountKey(FNode.Bank.SafeBox.Account(lsource.Get(i)).accountInfo.accountKey)<0 then raise Exception.Create(Format('You cannot operate with account %d because private key not found in your wallet',[lsource.Get(i)]));
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
Var l, selected : TOrderedCardinalList;
  an : Int64;
  i : Integer;
begin
  an := FAccountsGrid.AccountNumber(dgAccounts.Row);
  if (an<0) then raise Exception.Create('No account selected');
  if FWalletKeys.IndexOfAccountKey(FNode.Bank.SafeBox.Account(an).accountInfo.accountkey)<0 then
    raise Exception.Create(Format('You cannot add %s account because private key not found in your wallet.'#10+#10+'You''re not the owner!',
      [TAccountComp.AccountNumberToAccountTxtNumber(an)]));
  // Add
  l := FSelectedAccountsGrid.LockAccountsList;
  selected := TOrderedCardinalList.Create;
  Try
    FAccountsGrid.SelectedAccounts(selected);
    for i := 0 to selected.Count-1 do begin
      l.Add(selected.Get(i));
    end;
  Finally
    selected.Free;
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

procedure TFRMWallet.UpdateAccounts(RefreshData : Boolean);
Var accl : TOrderedCardinalList;
  l : TOrderedCardinalList;
  i,j,k : Integer;
  c  : Cardinal;
  LApplyfilter : Boolean;
  acc : TAccount;
  LFilters : TAccountsGridFilter;
begin
  If Not Assigned(FWalletKeys) Then exit;
  if Not Assigned(FNode) then Exit;

  if Not RefreshData then begin
    if TPlatform.GetElapsedMilliseconds(FLastAccountsGridInvalidateTC)>1000 then begin
      FLastAccountsGridInvalidateTC := TPlatform.GetTickCount;
      dgAccounts.Invalidate;
    end;
    exit;
  end;
  LApplyfilter := (cbFilterAccounts.Checked) and ((FMinAccountBalance>0) Or ((FMaxAccountBalance<CT_MaxWalletAmount) and (FMaxAccountBalance>=0)));
  if (Not cbExploreMyAccounts.Checked) And (not LApplyfilter) then begin
    FAccountsGrid.AccountsGridDatasource := acds_Node;
    FAccountsGrid.UpdateData;
  end else begin
    LFilters := FAccountsGrid.AccountsGridFilter;
    LFilters.MinBalance := FMinAccountBalance;
    LFilters.MaxBalance := FMaxAccountBalance;
    if cbExploreMyAccounts.Checked then begin
      //FNode.Bank.SafeBox.StartThreadSafe;
      try
        LFilters.OrderedAccountsKeyList := FWalletKeys.AccountsKeyList;
        if cbMyPrivateKeys.ItemIndex>0 then begin
          i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex]);
          if (i>=0) And (i<FWalletKeys.Count) then begin
            LFilters.indexAccountsKeyList := FWalletKeys.AccountsKeyList.IndexOfAccountKey(FWalletKeys[i].AccountKey);
          end;
        end else LFilters.indexAccountsKeyList := -1;
      finally
        //FNode.Bank.SafeBox.EndThreadSave;
      end;
    end else begin
      LFilters.OrderedAccountsKeyList := Nil;
      LFilters.indexAccountsKeyList := -1;
    end;
    FAccountsGrid.AccountsGridFilter := LFilters;
    FAccountsGrid.AccountsGridDatasource := acds_NodeFiltered;
  end;

  bbChangeKeyName.Enabled := cbExploreMyAccounts.Checked;
  OnAccountsGridUpdatedData(Nil);
  UpdateOperations;
end;

procedure TFRMWallet.UpdateAvailableConnections;
Var i : integer;
 NC : TNetConnection;
 l : TList<TNetConnection>;
begin
  if Not TNetData.NetData.NetConnections.TryLockList(100,l) then exit;
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
  i,mc : Integer;
  s : String;
  f, favg : real;
  LLockedMempool : TPCOperationsComp;
begin
  UpdateNodeStatus;
  mc := 0;
  if Assigned(FNode) then begin
    if FNode.Bank.BlocksCount>0 then begin
      lblCurrentBlock.Caption :=  Inttostr(FNode.Bank.BlocksCount)+' (0..'+Inttostr(FNode.Bank.BlocksCount-1)+')'; ;
    end else lblCurrentBlock.Caption :=  '(none)';
    lblCurrentAccounts.Caption := Inttostr(FNode.Bank.AccountsCount);
    lblCurrentBlockTime.Caption := UnixTimeToLocalElapsedTime(FNode.Bank.LastOperationBlock.timestamp);
    LLockedMempool := FNode.LockMempoolRead;
    try
      lblOperationsPending.Caption := Inttostr(LLockedMempool.Count);
      lblCurrentDifficulty.Caption := InttoHex(LLockedMempool.OperationBlock.compact_target,8);
    finally
      FNode.UnlockMempoolRead;
    end;
    favg := FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage);
    f := (CT_NewLineSecondsAvg - favg) / CT_NewLineSecondsAvg;
    lblTimeAverage.Caption := 'Last '+Inttostr(CT_CalcNewTargetBlocksAverage)+': '+FormatFloat('0.0',favg)+' sec. (Optimal '+Inttostr(CT_NewLineSecondsAvg)+'s) Deviation '+FormatFloat('0.00%',f*100);
    if favg>=CT_NewLineSecondsAvg then begin
      lblTimeAverage.Font.Color := clNavy;
    end else begin
      lblTimeAverage.Font.Color := clOlive;
    end;
    lblTimeAverageAux.Caption := Format('Last %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec.',[
        CT_CalcNewTargetBlocksAverage * 2 ,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage * 2)),
        ((CT_CalcNewTargetBlocksAverage * 3) DIV 2) ,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage((CT_CalcNewTargetBlocksAverage * 3) DIV 2)),
        ((CT_CalcNewTargetBlocksAverage DIV 4)*3),FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(((CT_CalcNewTargetBlocksAverage DIV 4)*3))),
        CT_CalcNewTargetBlocksAverage DIV 2,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 2)),
        CT_CalcNewTargetBlocksAverage DIV 4,FormatFloat('0.0',FNode.Bank.GetActualTargetSecondsAverage(CT_CalcNewTargetBlocksAverage DIV 4))]);
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
  if (Assigned(FPoolMiningServer)) And (FPoolMiningServer.Active) then begin
    If FPoolMiningServer.ClientsCount>0 then begin
      lblMinersClients.Caption := IntToStr(FPoolMiningServer.ClientsCount)+' connected JSON-RPC clients';
      lblMinersClients.Font.Color := clNavy;
    end else begin
      lblMinersClients.Caption := 'No JSON-RPC clients';
      lblMinersClients.Font.Color := clDkGray;
    end;
    MinersBlocksFound := FPoolMiningServer.ClientsWins;
  end else begin
    MinersBlocksFound := 0;
    lblMinersClients.Caption := 'JSON-RPC server not active';
    lblMinersClients.Font.Color := clRed;
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
  ebHashRateBackBlocks.Text := IntToStr(FBlockChainGrid.HashRateAverageBlocksCount);
  Case FBlockChainGrid.HashRateAs of
    hr_Unit : cbHashRateUnits.ItemIndex:=0;
    hr_Kilo : cbHashRateUnits.ItemIndex:=1;
    hr_Mega : cbHashRateUnits.ItemIndex:=2;
    hr_Giga : cbHashRateUnits.ItemIndex:=3;
    hr_Tera : cbHashRateUnits.ItemIndex:=4;
    hr_Peta : cbHashRateUnits.ItemIndex:=5;
    hr_Exa : cbHashRateUnits.ItemIndex:=6;
  else cbHashRateUnits.ItemIndex:=-1;
  end;
  if TNetData.NetDataExists then begin
    if TSettings.AppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].GetAsBoolean(TNetData.NetData.MinFutureBlocksToDownloadNewSafebox>200) then begin
      TNetData.NetData.MinFutureBlocksToDownloadNewSafebox:=TSettings.AppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewSafebox].GetAsInteger(TNetData.NetData.MinFutureBlocksToDownloadNewSafebox);
    end else TNetData.NetData.MinFutureBlocksToDownloadNewSafebox:=0;
  end;
end;

procedure TFRMWallet.UpdateConnectionStatus;
var errors : String;
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
Var status : String;
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
  If Assigned(FBackgroundLabel) then begin
    FBackgroundLabel.Font.Color:=lblNodeStatus.Font.Color;
    FBackgroundLabel.Caption:='Please wait until finished: '+lblNodeStatus.Caption;
  end;
end;

procedure TFRMWallet.UpdateOperations;
Var accn : Int64;
begin
  accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
  FOperationsAccountGrid.AccountNumber := accn;
end;

procedure TFRMWallet.UpdatePrivateKeys;
Var i,last_i : Integer;
  wk : TWalletKey;
  s : AnsiString;
begin
  FNodeNotifyEvents.WatchKeys := FWalletKeys.AccountsKeyList;
  if (cbMyPrivateKeys.ItemIndex>=0) then last_i := PtrInt(cbMyPrivateKeys.Items.Objects[cbMyPrivateKeys.ItemIndex])
  else last_i := -1;
  cbMyPrivateKeys.items.BeginUpdate;
  Try
    cbMyPrivateKeys.Items.Clear;
    For i:=0 to FWalletKeys.Count-1 do begin
      wk := FWalletKeys.Key[i];
      if (wk.Name='') then begin
        s := 'Sha256='+TCrypto.ToHexaString( TCrypto.DoSha256( TAccountComp.AccountKey2RawString(wk.AccountKey) ) );
      end else begin
        s := wk.Name;
      end;
      if Not Assigned(wk.PrivateKey) then begin
        if Length(wk.CryptedKey)>0 then s:=s+' (**NEED PASSWORD**)'
        else s:=s+' (**PUBLIC KEY ONLY**)';
      end;
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
