unit UUserInterface;

{$mode delphi}

interface

uses
  SysUtils, Classes, Forms, Controls, Windows, ExtCtrls,
  UAccounts, UNode, UWalletKeys, UAppParams, UConst, UFolderHelper, UGridUtils, URPC, UPoolMining, ULog, UThread, UNetProtocol, UCrypto,
  UFRMWallet, UFRMSyncronizationDialog, UFRMAccountExplorer, UFRMPendingOperations, UFRMOperation,
  UFRMLogs, UFRMMessages, UFRMNodes, UFRMBlockExplorer, UFRMWalletKeys;

type
  { Forward Declarations }
  TLoadSafeBoxThread = class;

  { TMinerPrivateKey }

  TMinerPrivateKey = (mpk_NewEachTime, mpk_Random, mpk_Selected);

  { TUserInterface }

  TUserInterface = class
    private
      FUILock : TPCCriticalSection; static;
      FStarted : boolean; static;
      FAppParams : TAppParams; static;
      FWallet : TFRMWallet; static;
      FSyncronizationDialog : TFRMSyncronizationDialog; static;
      FAccountExplorer : TFRMAccountExplorer; static;
      FPendingOperationForm : TFRMPendingOperations; static;
      FOperationsExplorerForm : TFRMOperation; static;
      FBlockExplorerForm : TFRMBlockExplorer; static;
      FLogsForm : TFRMLogs; static;
      FNodesForm : TFRMNodes; static;
      FMessagesForm : TFRMMessages; static;

      FIsActivated : Boolean; static;
      FMinerPrivateKeyType : TMinerPrivateKey; static;
      FWalletKeys : TWalletKeysExt; static;

      FRPCServer : TRPCServer; static;
      FMustProcessWalletChanged : Boolean; static;
      FPoolMiningServer : TPoolMiningServer; static;

      FUpdating : Boolean; static;
      FLog : TLog; static;
      FNode : TNode; static;
      TimerUpdateStatus: TTimer; static;
      FTrayIcon: TTrayIcon; static;
      FNodeNotifyEvents : TNodeNotifyEvents; static;

      FStatusBar0Text : AnsiString; static;
      FStatusBar1Text : AnsiString; static;
      FStatusBar2Text : AnsiString; static;
      FMessagesNotificationText : AnsiString; static;

      class procedure RefreshConnectionStatusDisplay;
      class function GetAccountKeyForMiner: TAccountKey;
      class procedure SetStatusBar0Text(text : AnsiString); static;
      class procedure SetStatusBar1Text(text : AnsiString); static;
      class procedure SetStatusBar2Text(text : AnsiString); static;
      class procedure SetMessagesNotificationText(text : AnsiString); static;
      class procedure TrayIconDblClick(Sender: TObject);

      class procedure FinishedLoadingApp;
      class procedure LoadAppParams;
      class procedure SaveAppParams;
      class procedure NotifyConfigChanged;


      // Handlers
      class procedure TimerUpdateStatusTimer(Sender: TObject);

      // Blockchain event handlers. TODO: refactor this out with TNotifyEvents
      // so forms that need these messages subscribe directly
      class procedure OnAccountsChanged(Sender: TObject);
      class procedure OnNewAccount(Sender: TObject);
      class procedure OnReceivedHelloMessage(Sender: TObject);
      class procedure OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: TRawBytes);
      class procedure OnNetStatisticsChanged(Sender: TObject);
      class procedure OnNetConnectionsUpdated(Sender: TObject);
      class procedure OnNetNodeServersUpdated(Sender: TObject);
      class procedure OnNetBlackListUpdated(Sender: TObject);
      class procedure OnMiningServerNewBlockFound(Sender: TObject);
    public
      class property Started : boolean read FStarted;
      class property Node : TNode read FNode;
      class property Log : TLog read FLog;
      class property AppParams : TAppParams read FAppParams;
      class property PoolMiningServer : TPoolMiningServer read FPoolMiningServer;
      class property WalletKeys : TWalletKeysExt read FWalletKeys;
      class property StatusBar0Text : AnsiString read FStatusBar0Text write SetStatusBar0Text;
      class property StatusBar1Text : AnsiString read FStatusBar1Text write SetStatusBar1Text;
      class property StatusBar2Text : AnsiString read FStatusBar2Text write SetStatusBar2Text;
      class property MessagesNotificationText : AnsiString read FMessagesNotificationText write SetMessagesNotificationText;
      class procedure Start(mainForm : TForm);
      class procedure Quit;
      class procedure CheckNodeIsReady;
      // Show Dialogs
      class procedure ShowAboutBox(parentForm : TForm);
      class procedure ShowOptionsDialog(parentForm: TForm);
      class procedure ShowNewOperationDialog(parentForm : TForm; accounts : TOrderedCardinalList; defaultFee : Cardinal);
      class procedure ShowSyncronizationDialog(parentForm : TForm);
      class procedure ShowWalletKeysDialog(parentForm : TForm);
      class procedure ShowNodeIPDialog(parentForm : TForm);
      class procedure ShowAccountInformationDialog(parentForm : TForm; account : UInt64);
      class procedure ShowPrivateKeysDialog(parentForm: TForm);
      class procedure UnlockWallet(parentForm: TForm;  walletKeys : TWalletKeys);
      class procedure ChangeWalletPassword(parentForm: TForm; walletKeys : TWalletKeys);

      // Show sub-forms
      class procedure ShowAccountExplorer;
      class procedure ShowBlockExplorer;
      class procedure ShowOperationsExplorer;
      class procedure ShowPendingOperations;
      class procedure ShowMessagesForm;
      class procedure ShowNodesForm;
      class procedure ShowLogsForm;
  end;

  { TLoadSafeBoxThread }
  TLoadSafeBoxThread = Class(TPCThread)
  protected
    procedure BCExecute; override;
  End;

implementation

uses Dialogs, UCommon, UCommonUI, UOpenSSL, UFileStorage, UTime, UFRMAbout, UFRMNodesIp, UFRMPascalCoinWalletConfig ;

{%region UI Lifecyle}

class procedure TUserInterface.Start(mainForm : TForm);
Var ips : AnsiString;
  nsarr : TNodeServerAddressArray;
begin
  inherited;
  if FIsActivated then exit;
  FIsActivated := true;
  try
    // Create UI lock
    FUILock := TPCCriticalSection.Create('TUserInterface.UILock');

    // Initialise field defaults
    FIsActivated := false;
    FStarted := false;
    FRPCServer := Nil;
    FNode := Nil;
    FPoolMiningServer := Nil;
    FNodeNotifyEvents := nil;
    FUpdating := false;
    FStatusBar0Text := '';
    FStatusBar1Text := '';
    FStatusBar2Text := '';
    FMessagesNotificationText := '';

    // Create main form and try icon
    FWallet := mainForm as TFRMWallet;
    if (FWallet = nil)
      then raise Exception.Create('Main form is not TWallet');
    FTrayIcon := TTrayIcon.Create(TUserInterface.FWallet);
    FTrayIcon.OnDblClick := TrayIconDblClick;
    FTrayIcon.Visible := true;
    FTrayIcon.Hint := FWallet.Caption;
    FTrayIcon.BalloonTitle := 'Restoring the window.';
    FTrayIcon.BalloonHint := 'Double click the system tray icon to restore Pascal Coin';
    FTrayIcon.BalloonFlags := bfInfo;
    TimerUpdateStatus := TTimer.Create(TUserInterface.FWallet);
    TimerUpdateStatus.Enabled := false;

    // Create log
    FLog := TLog.Create(FWallet);
    FLog.SaveTypes := [];

    // Create data directories
    If Not ForceDirectories(TFolderHelper.GetPascalCoinDataFolder) then
      raise Exception.Create('Cannot create dir: '+TFolderHelper.GetPascalCoinDataFolder);

    // Open AppParams
    TUserInterface.FAppParams := TAppParams.Create(FWallet);
    TUserInterface.FAppParams.FileName := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'AppParams.prm';

    // Open Wallet
    Try
      FWalletKeys := TWalletKeysExt.Create(FWallet);  // On Activate, this will be populated
      FWalletKeys.WalletFileName := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'WalletKeys.dat';
    Except
      On E:Exception do begin
        E.Message := 'Cannot open your wallet... Perhaps another instance of Pascal Coin is active!'+#10+#10+E.Message;
        Raise;
      end;
    End;

    // Load peer list
    ips := FAppParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].GetAsString('');
    TNode.DecodeIpStringToNodeServerAddressArray(ips,nsarr);
    TNetData.NetData.DiscoverFixedServersOnly(nsarr);
    setlength(nsarr,0);

    // Start Node
    FNode := TNode.Node;
    FNode.NetServer.Port := FAppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
    FNode.PeerCache := FAppParams.ParamByName[CT_PARAM_PeerCache].GetAsString('')+';'+CT_Discover_IPs;

    // Subscribe to Node events (TODO refactor with FNotifyEvents)
    FNodeNotifyEvents := TNodeNotifyEvents.Create(FWallet);
    FNodeNotifyEvents.OnBlocksChanged := OnNewAccount;
    FNodeNotifyEvents.OnNodeMessageEvent :=  OnNodeMessageEvent;

    // Start RPC server
    FRPCServer := TRPCServer.Create;
    FRPCServer.WalletKeys := WalletKeys;
    FRPCServer.Active := FAppParams.ParamByName[CT_PARAM_JSONRPCEnabled].GetAsBoolean(false);
    FRPCServer.ValidIPs := FAppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].GetAsString('127.0.0.1');
    WalletKeys.SafeBox := FNode.Bank.SafeBox;

    // Initialise Database
    FNode.Bank.StorageClass := TFileStorage;
    TFileStorage(FNode.Bank.Storage).DatabaseFolder := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'Data';
    TFileStorage(FNode.Bank.Storage).Initialize;

    // Init Grid
    //FAccountsGrid.Node := FNode;
    //HS FSelectedAccountsGrid.Node := FNode;
    //HS FAccountsGrid.Node := FNode;
    //HS FOperationsAccountGrid.Node := FNode;

    // Reading database
    TLoadSafeBoxThread.Create(false).FreeOnTerminate := true;

    // Init
    TNetData.NetData.OnReceivedHelloMessage := OnReceivedHelloMessage;
    TNetData.NetData.OnStatisticsChanged := OnNetStatisticsChanged;
    TNetData.NetData.OnNetConnectionsUpdated := OnNetConnectionsUpdated;
    TNetData.NetData.OnNodeServersUpdated := OnNetNodeServersUpdated;
    TNetData.NetData.OnBlackListUpdated := OnNetBlackListUpdated;

    // Start refresh timer
    TimerUpdateStatus.OnTimer := TimerUpdateStatusTimer;
    TimerUpdateStatus.Interval := 1000;
    TimerUpdateStatus.Enabled := true;


    // Load app params
    LoadAppParams;

    // open the sync dialog
    FSyncronizationDialog.UpdateBlockChainState;   //TODO fix this work-flow
    RefreshConnectionStatusDisplay;

    // Setup tray icon

    // HS ???
    //FMinersBlocksFound := 0;

  // Disable wallet form
    FWallet.Enabled:=false;
    FStarted := true;
  Except
    On E:Exception do begin
      E.Message := 'An error occurred during initialization. Application cannot continue:'+#10+#10+E.Message+#10+#10+'Application will close...';
      Application.MessageBox(PChar(E.Message),PChar(Application.Title),MB_ICONERROR+MB_OK);
      Halt;
    end;
  end;


  // Notify accounts again?
  OnAccountsChanged(FWallet);

  // Refresh status bar since may not have been displayed
  SetStatusBar0Text(FStatusBar0Text);
  SetStatusBar0Text(FStatusBar1Text);
  SetStatusBar0Text(FStatusBar2Text);
  SetMessagesNotificationText(FMessagesNotificationText);

  // Show sync dialog
  ShowSyncronizationDialog(FWallet);

  // Show about box if first time load
  if FAppParams.ParamByName[CT_PARAM_FirstTime].GetAsBoolean(true) then begin
    FAppParams.ParamByName[CT_PARAM_FirstTime].SetAsBoolean(false);
    ShowAboutBox(nil);
  end;
end;

class procedure TUserInterface.Quit;
Var i : Integer;
  step : String;
begin
  TLog.NewLog(ltinfo,Classname,'Destroying form - START');
  Try
    FreeAndNil(FRPCServer);
    FreeAndNil(FPoolMiningServer);
    step := 'Saving params';
    SaveAppParams;
    FreeAndNil(FAppParams);

    step := 'Assigning nil events';
    FLog.OnNewLog :=Nil;

    TNetData.NetData.OnReceivedHelloMessage := Nil;
    TNetData.NetData.OnStatisticsChanged := Nil;
    TNetData.NetData.OnNetConnectionsUpdated := Nil;
    TNetData.NetData.OnNodeServersUpdated := Nil;
    TNetData.NetData.OnBlackListUpdated := Nil;
    //step := 'Destroying NodeNotifyEvents';
    //FreeAndNil(FNodeNotifyEvents);
    //
    step := 'Assigning Nil to TNetData';
    TNetData.NetData.OnReceivedHelloMessage := Nil;
    TNetData.NetData.OnStatisticsChanged := Nil;

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

    // TODO - refactor out to TNotifyManyEvents
    FNodeNotifyEvents.Node := Nil;
    FreeAndNil(FNodeNotifyEvents);

    step := 'Destroying UI graph';
    FWallet.Destroy; // Destroys root wallet, non-modal forms and all their attached components
    FWallet := nil;
    FMessagesForm := nil;
    FAccountExplorer := nil;
    FTrayIcon := nil;
    FreeAndNil(FUILock);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error destroying Form step: '+step+' Errors ('+E.ClassName+'): ' +E.Message);
    end;
  End;
  TLog.NewLog(ltinfo,Classname,'Destroying form - END');
  FreeAndNil(FLog);
  Sleep(100);
end;

class procedure TUserInterface.FinishedLoadingApp;
begin
  FPoolMiningServer := TPoolMiningServer.Create;
  FPoolMiningServer.Port := FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].GetAsInteger(CT_JSONRPCMinerServer_Port);
  FPoolMiningServer.MinerAccountKey := GetAccountKeyForMiner;
  FPoolMiningServer.MinerPayload := FAppParams.ParamByName[CT_PARAM_MinerName].GetAsString('');
  FNode.Operations.AccountKey := GetAccountKeyForMiner;
  FPoolMiningServer.Active := FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].GetAsBoolean(true);
  FPoolMiningServer.OnMiningServerNewBlockFound := OnMiningServerNewBlockFound;

  //HS review
//  FRMSyncronizationDialog.Hide; Herman May be, need close this window after load?
  FSyncronizationDialog.BorderIcons:=[biSystemMenu];
  FSyncronizationDialog.HideButton.Enabled:=true;
  FWallet.Enabled:=True;

  // Refresh UI
  OnAccountsChanged(FWallet);
end;

{%endregion}

{%region Show Dialogs}

class procedure TUserInterface.ShowAboutBox(parentForm : TForm);
begin
  with TFRMAbout.Create(parentForm) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

class procedure TUserInterface.ShowOptionsDialog(parentForm: TForm);
begin
  With TFRMPascalCoinWalletConfig.Create(parentForm) do
  try
    AppParams := FAppParams;
    WalletKeys := FWalletKeys;
    if ShowModal=MrOk then begin
      SaveAppParams;
      NotifyConfigChanged;
    end;
  finally
    Free;
  end;
end;

// TODO - refactor with accounts as ARRAY
class procedure TUserInterface.ShowNewOperationDialog(parentForm : TForm; accounts : TOrderedCardinalList; defaultFee : Cardinal);
begin
  If accounts.Count = 0 then raise Exception.Create('No sender accounts provided');
  CheckNodeIsReady;
  With TFRMOperation.Create(parentForm) do
  Try
    SenderAccounts.CopyFrom(accounts);
    DefaultFee := defaultFee;
    WalletKeys := FWalletKeys;
    ShowModal;
  Finally
    Free;
  End;
end;

class procedure TUserInterface.ShowSyncronizationDialog(parentForm : TForm);
begin
  try
    FUILock.Acquire;
    if FSyncronizationDialog = nil then begin
      FSyncronizationDialog:=TFRMSyncronizationDialog.Create(FWallet);
      FSyncronizationDialog.Visible := false;
      FSyncronizationDialog.Show;
      FRMSyncronizationDialogIsFirstOpen:=false;
    end;
    FWallet.SetSubFormCoordinate(FSyncronizationDialog);
    FSyncronizationDialog.Visible := true;;
  finally
    FUILock.Release;
  end;

  // SyncDialog is always created, sometimes invisible
  //
  //SetSubFormCoordinate(FRMSyncronizationDialog);
  //FRMSyncronizationDialog.Visible:=not FRMSyncronizationDialog.Visible;
  FSyncronizationDialog.Visible := true;
end;

class procedure TUserInterface.ShowWalletKeysDialog(parentForm : TForm);
var FRM : TFRMWalletKeys;
begin
  FRM := TFRMWalletKeys.Create(parentForm);
  Try
    FRM.WalletKeys := FWalletKeys;
    FRM.ShowModal;
  Finally
    FRM.Free;
  End;
end;

class procedure TUserInterface.ShowNodeIPDialog(parentForm : TForm);
Var FRM : TFRMNodesIp;
begin
  FRM := TFRMNodesIp.Create(parentForm);
  Try
    FRM.AppParams := TUserInterface.AppParams;
    FRM.ShowModal;
  Finally
    FRM.Free;
  End;
end;

class procedure TUserInterface.ShowAccountInformationDialog(parentForm : TForm; account : UInt64);
begin
  raise Exception.Create('Not Implemented');
  //HS ACCOUNT INFORMATION HERRE
  //class procedure TFRMAccountExplorer.MiAccountInformationClick(Sender: TObject);
  //Var F : TFRMMemoText;
  //  accn : Int64 =-1;
  //  title : String;
  //  //account : TAccount;
  //  strings : TStrings;
  //  i : Integer;
  //  opr : TOperationResume;
  //begin
  ////  with FRMWallet do begin
  //  accn := -1;
  //  title := '';
  //  strings := TStringList.Create;
  //  try
  //    opr := CT_TOperationResume_NUL;
  //    //AntonB if PageControl.ActivePage=tsOperations then begin
  //      if not (FOperationsExplorerGrid.DrawGrid = nil) then begin
  //      i := FOperationsExplorerGrid.DrawGrid.Row;
  //      if (i>0) and (i<=FOperationsExplorerGrid.OperationsResume.Count) then begin
  //        opr := FOperationsExplorerGrid.OperationsResume.OperationResume[i-1];
  //      end;
  //    //AntonB end else if PageControl.ActivePage=tsPendingOperations then begin
  //      end else if not (FPendingOperationsGrid.DrawGrid = nil) then begin
  //      i := FPendingOperationsGrid.DrawGrid.Row;
  //      if (i>0) and (i<=FPendingOperationsGrid.OperationsResume.Count) then begin
  //        opr := FPendingOperationsGrid.OperationsResume.OperationResume[i-1];
  //      end;
  //    end else //if PageControl.ActivePage=tsMyAccounts then
  //    begin
  //      accn := FAccountsGrid.AccountNumber(dgAccounts.Row);
  //      if accn<0 then raise Exception.Create('Select an account');
  //      FillAccountInformation(strings,accn);
  //      title := 'Account '+TAccountComp.AccountNumberToAccountTxtNumber(accn)+' info';
  //      i := FOperationsAccountGrid.DrawGrid.Row;
  //      if (i>0) and (i<=FOperationsAccountGrid.OperationsResume.Count) then begin
  //        opr := FOperationsAccountGrid.OperationsResume.OperationResume[i-1];
  //      end;
  //    end;
  //    If (opr.valid) then begin
  //      if accn>=0 then strings.Add('')
  //      else title := 'Operation info';
  //      strings.Add('Operation info:');
  //      FillOperationInformation(strings,opr);
  //    end else if accn<0 then Raise Exception.Create('No info available');
  //    F := TFRMMemoText.Create(Self);
  //    Try
  //      F.Caption := title;
  //      F.Memo.Lines.Assign(strings);
  //      F.ShowModal;
  //    Finally
  //      F.Free;
  //    End;
  //  finally
  //    strings.free;
  //  end;
  //  end;
  //end;

end;

class procedure TUserInterface.ShowPrivateKeysDialog(parentForm: TForm);
Var FRM : TFRMWalletKeys;
begin
  FRM := TFRMWalletKeys.Create(parentForm);
  Try
    FRM.WalletKeys := FWalletKeys;
    FRM.ShowModal;
    //UpdatePrivateKeys; // Should receive event from FWalletKeys.OnChanged subscription
  Finally
    FRM.Free;
  End;
end;

class procedure TUserInterface.ChangeWalletPassword(parentForm: TForm; walletKeys : TWalletKeys);
Var s,s2 : String;
begin
  if walletKeys = nil then walletKeys := FWalletKeys;
  s := ''; s2 := '';
  if Not InputQuery('Change password','Enter new password',s) then exit;
  if trim(s)<>s then raise Exception.Create('Password cannot start or end with a space character');
  if Not InputQuery('Change password','Enter new password again',s2) then exit;
  if s<>s2 then raise Exception.Create('Two passwords are different!');

  walletKeys.WalletPassword := s;
  Application.MessageBox(PChar('Password changed!'+#10+#10+
    'Please note that your new password is "'+s+'"'+#10+#10+
    '(If you lose this password, you will lose your wallet forever!)'),
    PChar(Application.Title),MB_ICONWARNING+MB_OK);
  //UpdateWalletKeys;
end;

class procedure TUserInterface.UnlockWallet(parentForm: TForm; walletKeys : TWalletKeys);
Var s : String;
begin
  if walletKeys = nil then walletKeys := FWalletKeys;
  s := '';
  Repeat
    if Not InputQuery('Wallet password','Enter wallet password',s) then exit;
    walletKeys.WalletPassword := s;
    if Not walletKeys.IsValidPassword then Application.MessageBox(PChar('Invalid password'),PChar(Application.Title),MB_ICONERROR+MB_OK);
  Until walletKeys.IsValidPassword;
  //UpdateWalletKeys;
end;

{%endregion}

{%region Show Forms}

class procedure TUserInterface.ShowAccountExplorer;
begin
  try
    FUILock.Acquire;
    if not Assigned(FAccountExplorer) then begin
       FAccountExplorer := TFRMAccountExplorer.Create(FWallet);
       FWallet.SetSubFormCoordinate(FAccountExplorer);
    end;
    FAccountExplorer.Refresh;
    FAccountExplorer.Show;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.ShowBlockExplorer;
begin
  try
    FUILock.Acquire;
    if not Assigned(FBlockExplorerForm) then begin
       FBlockExplorerForm := TFRMBlockExplorer.Create(FWallet);
       FWallet.SetSubFormCoordinate(FBlockExplorerForm);
    end;
    FBlockExplorerForm.Show;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.ShowOperationsExplorer;
begin
  try
    FUILock.Acquire;
    if not Assigned(FOperationsExplorerForm) then begin
      FOperationsExplorerForm := TFRMOperation.Create(FWallet);
      FWallet.SetSubFormCoordinate(FOperationsExplorerForm);
    end;
    FOperationsExplorerForm.Show;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.ShowPendingOperations;
begin
  try
    FUILock.Acquire;
    if not Assigned(FPendingOperationForm) then begin
      FPendingOperationForm := TFRMPendingOperations.Create(FWallet);
      FWallet.SetSubFormCoordinate(FPendingOperationForm);
    end;
    FPendingOperationForm.Show;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.ShowMessagesForm;
begin
  try
    FUILock.Acquire;
    if not Assigned(FMessagesForm) then begin
       FMessagesForm := TFRMMessages.Create(FWallet);
       FWallet.SetSubFormCoordinate(FMessagesForm);
    end;
    FMessagesForm.Show;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.ShowNodesForm;
begin
  try
    FUILock.Acquire;
    if not Assigned(FNodesForm) then begin
       FNodesForm := TFRMNodes.Create(FWallet);
       FWallet.SetSubFormCoordinate(FNodesForm);
    end;
    FNodesForm.Show;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.ShowLogsForm;
begin
  try
    FUILock.Acquire;
    if not Assigned(FLogsForm) then begin
       FLogsForm := TFRMLogs.Create(FWallet);
       FWallet.SetSubFormCoordinate(FLogsForm);
    end;
    FLogsForm.Show;
  finally
    FUILock.Release;
  end;
end;

{%endregion}

{%region Public methods}

class procedure TUserInterface.CheckNodeIsReady;
Var errorMessage : AnsiString;
begin
  //HS if Not Assigned(FNode) then Abort;

  if Not TNode.Node.IsReady(errorMessage) then begin
    Raise Exception.Create('You cannot do this operation now:'+#10+#10+errorMessage);
  end;
end;

class function TUserInterface.GetAccountKeyForMiner: TAccountKey;
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

class procedure TUserInterface.RefreshConnectionStatusDisplay;
var errors : AnsiString;
begin
  FUILock.Acquire;
  Try
    FSyncronizationDialog.UpdateNodeStatus;
    OnNetStatisticsChanged(FWallet);
    if Assigned(FNode) then begin
      if FNode.IsBlockChainValid(errors) then begin
        StatusBar2Text := Format('Last account time:%s',
         [FormatDateTime('dd/mm/yyyy hh:nn:ss',UnivDateTime2LocalDateTime(UnixToUnivDateTime( FNode.Bank.LastOperationBlock.timestamp )))]);
      end else begin
        StatusBar2Text := 'NO BLOCKCHAIN: '+errors;
      end;
    end else begin
      StatusBar2Text := '';
    end;
  finally
    FUILock.Release;
  end;
end;

{%endregion}

{%region Auxillary methods}

//HS
class procedure TUserInterface.SaveAppParams;
Var ms : TMemoryStream;
  s : AnsiString;
begin
  // Disabled in V2 (Herman)
  FAppParams.ParamByName[CT_PARAM_GridAccountsStream].SetAsString('');
  //ms := TMemoryStream.Create;
  //Try
  //  AccountExplorer.AccountsGrid.SaveToStream(ms);
  //  ms.Position := 0;
  //  setlength(s,ms.Size);
  //  ms.ReadBuffer(s[1],ms.Size);
  //  FAppParams.ParamByName[CT_PARAM_GridAccountsStream].SetAsString(s);
  //Finally
  //  ms.Free;
  //End;
end;

class procedure TUserInterface.LoadAppParams;
Var //ms : TMemoryStream;
  s : AnsiString;
  fvi : TFileVersionInfo;
begin
  // Disabled in V2 (HS)
  //ms := TMemoryStream.Create;
  //Try
  //  s := FAppParams.ParamByName[CT_PARAM_GridAccountsStream].GetAsString('');
  //  ms.WriteBuffer(s[1],length(s));
  //  ms.Position := 0;
  //  // Disabled on V2: FAccountsGrid.LoadFromStream(ms);
  //Finally
  //  ms.Free;
  //End;
  If FAppParams.FindParam(CT_PARAM_MinerName)=Nil then begin
    // New configuration... assigning a new random value
    fvi := TFolderHelper.GetTFileVersionInfo(Application.ExeName);
    FAppParams.ParamByName[CT_PARAM_MinerName].SetAsString('New Node '+DateTimeToStr(Now)+' - '+
      fvi.InternalName+' Build:'+fvi.FileVersion);
  end;
  NotifyConfigChanged;
end;

class procedure TUserInterface.NotifyConfigChanged;
Var wa : Boolean;
  i : Integer;
begin
// AntonB logs form not visible now at start and OnNewLog := Nil
//  tsLogs.TabVisible := FAppParams.ParamByName[CT_PARAM_ShowLogs].GetAsBoolean(false);
//  if (Not tsLogs.TabVisible) then begin
//    FLog.OnNewLog := Nil;
//    if PageControl.ActivePage = tsLogs then PageControl.ActivePage := tsMyAccounts;
//  end else FLog.OnNewLog := OnNewLog;
  if FAppParams.ParamByName[CT_PARAM_SaveLogFiles].GetAsBoolean(false) then begin
    if FAppParams.ParamByName[CT_PARAM_SaveDebugLogs].GetAsBoolean(false) then FLog.SaveTypes := CT_TLogTypes_ALL
    else FLog.SaveTypes := CT_TLogTypes_DEFAULT;
    FLog.FileName := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'PascalCointWallet.log';
  end else begin
    FLog.SaveTypes := [];
    FLog.FileName := '';
  end;
  if Assigned(FNode) then begin
    wa := FNode.NetServer.Active;
    FNode.NetServer.Port := FAppParams.ParamByName[CT_PARAM_InternetServerPort].GetAsInteger(CT_NetServer_Port);
    FNode.NetServer.Active := wa;
    FNode.Operations.BlockPayload := FAppParams.ParamByName[CT_PARAM_MinerName].GetAsString('');
    FNode.NodeLogFilename := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'blocks.log';
  end;
  if Assigned(FPoolMiningServer) then begin
    if FPoolMiningServer.Port<>FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].GetAsInteger(CT_JSONRPCMinerServer_Port) then begin
      FPoolMiningServer.Active := false;
      FPoolMiningServer.Port := FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].GetAsInteger(CT_JSONRPCMinerServer_Port);
    end;
    FPoolMiningServer.Active :=FAppParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].GetAsBoolean(true);
    FPoolMiningServer.UpdateAccountAndPayload(GetAccountKeyForMiner,FAppParams.ParamByName[CT_PARAM_MinerName].GetAsString(''));
  end;
  if Assigned(FRPCServer) then begin
    FRPCServer.Active := FAppParams.ParamByName[CT_PARAM_JSONRPCEnabled].GetAsBoolean(false);
    FRPCServer.ValidIPs := FAppParams.ParamByName[CT_PARAM_JSONRPCAllowedIPs].GetAsString('127.0.0.1');
  end;
  i := FAppParams.ParamByName[CT_PARAM_MinerPrivateKeyType].GetAsInteger(Integer(mpk_Random));
  if (i>=Integer(Low(TMinerPrivatekey))) And (i<=Integer(High(TMinerPrivatekey))) then FMinerPrivateKeyType := TMinerPrivateKey(i)
  else FMinerPrivateKeyType := mpk_Random;
end;

class procedure TUserInterface.SetStatusBar0Text(text : AnsiString);
begin
  FStatusBar0Text := text;
  if Assigned(FWallet) then
    FWallet.sbFooterBar.Panels[0].Text := FStatusBar0Text;
end;

class procedure TUserInterface.SetStatusBar1Text(text : AnsiString);
begin
  FStatusBar1Text := text;
  if Assigned(FWallet) then
    FWallet.sbFooterBar.Panels[1].Text := text;
end;

class procedure TUserInterface.SetStatusBar2Text(text : AnsiString);
begin
  FStatusBar2Text := text;
  if Assigned(FWallet) then
    FWallet.sbFooterBar.Panels[2].Text := text;
end;

class procedure TUserInterface.SetMessagesNotificationText(text : AnsiString); static;
begin
  FMessagesNotificationText := text;
  if Assigned(FSyncronizationDialog) then begin
    if (text = '') then
      FSyncronizationDialog.lblReceivedMessages.Visible := false;
    FSyncronizationDialog.lblReceivedMessages.Caption := text;
  end;
end;

{%endregion}

{%region Handlers -- TODO: many need to be refactored out with TNotifyManyEvent}

class procedure TUserInterface.OnAccountsChanged(Sender: TObject);
begin
  FUILock.Acquire;
  Try
    if Assigned(FAccountExplorer) then
      FAccountExplorer.UpdateAccounts(true);
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.OnNewAccount(Sender: TObject);
begin
  FUILock.Acquire;
  try
    try
      if Assigned(FAccountExplorer) then
        FAccountExplorer.UpdateAccounts(false);
      FSyncronizationDialog.UpdateBlockChainState;
    except
      On E:Exception do begin
        E.Message := 'Error at OnNewAccount '+E.Message;
        Raise;
      end;
    end;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: TRawBytes);
begin
  FUILock.Acquire;
  Try
    if Assigned(FMessagesForm) then
      FMessagesForm.OnNodeMessageEvent(NetConnection, MessageData);
  finally
      FUILock.Release;
  end;
end;

class procedure TUserInterface.OnReceivedHelloMessage(Sender: TObject);
Var nsarr : TNodeServerAddressArray;
  i : Integer;
  s : AnsiString;
begin
  // No lock required
  //CheckMining;
  // Update node servers Peer Cache
  nsarr := TNetData.NetData.GetValidNodeServers(true,0);
  s := '';
  for i := low(nsarr) to High(nsarr) do begin
    if (s<>'') then s := s+';';
    s := s + nsarr[i].ip+':'+IntToStr( nsarr[i].port );
  end;
  FAppParams.ParamByName[CT_PARAM_PeerCache].SetAsString(s);
  TNode.Node.PeerCache := s;
end;

class procedure TUserInterface.OnNetStatisticsChanged(Sender: TObject);
Var NS : TNetStatistics;
begin
  FUILock.Acquire;   // TODO - lock may not be required
  Try
    //CheckMining;
    if Assigned(FNode) then begin
      If FNode.NetServer.Active then begin
        StatusBar0Text := 'Active (Port '+Inttostr(FNode.NetServer.Port)+')';
      end else StatusBar0Text := 'Server stopped';
      NS := TNetData.NetData.NetStatistics;
      StatusBar1Text := Format('Connections:%d Clients:%d Servers:%d - Rcvd:%d Kb Send:%d Kb',
        [NS.ActiveConnections,NS.ClientsConnections,NS.ServersConnections,NS.BytesReceived DIV 1024,NS.BytesSend DIV 1024]);
    end else begin
      StatusBar0Text := '';
      StatusBar1Text := '';
    end;
  finally
      FUILock.Release;
  end;
end;

class procedure TUserInterface.OnNetConnectionsUpdated(Sender: TObject);
begin
  try
    FUILock.Acquire;
    if Assigned(FNodesForm) then
      FNodesForm.OnNetConnectionsUpdated;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.OnNetNodeServersUpdated(Sender: TObject);
begin
  try
    FUILock.Acquire;
    if Assigned(FNodesForm) then
      FNodesForm.OnNetNodeServersUpdated;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.OnNetBlackListUpdated(Sender: TObject);
begin
  try
    FUILock.Acquire;
    if Assigned(FNodesForm) then
      FNodesForm.OnNetBlackListUpdated;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.OnMiningServerNewBlockFound(Sender: TObject);
begin
  // No lock required
  FPoolMiningServer.MinerAccountKey := GetAccountKeyForMiner;
end;

class procedure TUserInterface.TimerUpdateStatusTimer(Sender: TObject);
begin
  Try
    RefreshConnectionStatusDisplay;
    FSyncronizationDialog.UpdateBlockChainState;
    // UpdateNodeStatus; AntonB in UpdateBlockChainState call UpdateNodeStatus
  Except
    On E:Exception do begin
      E.Message := 'Exception at TimerUpdate '+E.ClassName+': '+E.Message;
      TLog.NewLog(lterror,ClassName,E.Message);
    end;
  End;
end;

class procedure TUserInterface.TrayIconDblClick(Sender: TObject);
begin
  FTrayIcon.Visible := False;
  TimerUpdateStatus.Enabled := true;
  FWallet.Show();
  FWallet.WindowState := wsNormal;
  Application.BringToFront();
end;

{%endregion}


{ TUserInterfaceStartupThread }

procedure TLoadSafeBoxThread.BCExecute;
begin
  // Read Operations saved from disk
  TNode.Node.Bank.DiskRestoreFromOperations(CT_MaxBlock);
  TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  TNode.Node.NetServer.Active := true;
  Synchronize( TUserInterface.FinishedLoadingApp );
end;

finalization
  TUserInterface.FWallet.Destroy;

end.

