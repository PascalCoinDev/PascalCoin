unit UUserInterface;

{$mode delphi}

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/master/Units/Forms/UFRMWallet.pas
}

interface

{$I ..\config.inc}

uses
  SysUtils, Classes, Forms, Controls, {$IFDEF WINDOWS}Windows,{$ENDIF} ExtCtrls, Dialogs, LCLType,
  UCommon.UI, UBlockChain, UAccounts, UNode, UWallet, UConst, UFolderHelper, UGridUtils, URPC, UPoolMining,
  ULog, UThread, UNetProtocol, UCrypto,
  UFRMMainForm, UCTRLSyncronization, UFRMAccountExplorer, UFRMOperationExplorer, UFRMPendingOperations, UFRMOperation,
  UFRMLogs, UFRMMessages, UFRMNodes, UFRMBlockExplorer, UFRMWalletKeys;

type
  { Forward Declarations }

  TLoadDatabaseThread = class;

  { TUserInterface }

  TUserInterface = class
    private
      // Root-form
      FUILock : TPCCriticalSection; static;

      // Subforms
      FAccountExplorer : TFRMAccountExplorer; static;
      FPendingOperationForm : TFRMPendingOperations; static;
      FOperationsExplorerForm : TFRMOperationExplorer; static;
      FBlockExplorerForm : TFRMBlockExplorer; static;
      FLogsForm : TFRMLogs; static;
      FNodesForm : TFRMNodes; static;
      FMessagesForm : TFRMMessages; static;

      // Components
      FRPCServer : TRPCServer; static;
      FPoolMiningServer : TPoolMiningServer; static;

      // Local fields
      FStarted : boolean; static;
      FMainForm : TFRMMainForm; static;
      FIsActivated : Boolean; static;
      FUpdating : Boolean; static;
      FLog : TLog; static;
      FNode : TNode; static;
      FTimerUpdateStatus: TTimer; static;
      FTrayIcon: TTrayIcon; static;
      FNodeNotifyEvents : TNodeNotifyEvents; static;
      FStatusBar0Text : AnsiString; static;
      FStatusBar1Text : AnsiString; static;
      FStatusBar2Text : AnsiString; static;
      FMessagesNotificationText : AnsiString; static;
      FDisplayedStartupSyncDialog : boolean; static;

      // Methods
      class procedure RefreshConnectionStatusDisplay;

      // Getters/Setters
      class procedure SetStatusBar0Text(const text : AnsiString); static;
      class procedure SetStatusBar1Text(const text : AnsiString); static;
      class procedure SetStatusBar2Text(const text : AnsiString); static;
      class procedure SetMessagesNotificationText(const text : AnsiString); static;
      class procedure SetMainFormMode(AMode: TFRMMainFormMode); static;
      class function GetMainFormMode : TFRMMainFormMode; static;

      // Aux methods
      class procedure FinishedLoadingDatabase;

      // Handlers
      class procedure OnTimerUpdateStatusTimer(Sender: TObject);
      class procedure OnSubFormDestroyed(Sender: TObject);

      // Backend Handlers (TODO: refactor this out with TNotifyManyEvents)
      class procedure OnSettingsChanged(Sender: TObject);
      class procedure OnAccountsChanged(Sender: TObject);
      class procedure OnBlocksChanged(Sender: TObject);
      class procedure OnReceivedHelloMessage(Sender: TObject);
      class procedure OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: TRawBytes);
      class procedure OnNetStatisticsChanged(Sender: TObject);
      class procedure OnNetConnectionsUpdated(Sender: TObject);
      class procedure OnNetNodeServersUpdated(Sender: TObject);
      class procedure OnNetBlackListUpdated(Sender: TObject);
      class procedure OnMiningServerNewBlockFound(Sender: TObject);
      class procedure OnTrayIconDblClick(Sender: TObject);
    public
      // Properties
      class property Started : boolean read FStarted;
      class property Node : TNode read FNode;
      class property Log : TLog read FLog;
      class property PoolMiningServer : TPoolMiningServer read FPoolMiningServer;
      class property MainFormMode : TFRMMainFormMode read GetMainFormMode write SetMainFormMode;
      class property StatusBar0Text : AnsiString read FStatusBar0Text write SetStatusBar0Text;
      class property StatusBar1Text : AnsiString read FStatusBar1Text write SetStatusBar1Text;
      class property StatusBar2Text : AnsiString read FStatusBar2Text write SetStatusBar2Text;
      class property MessagesNotificationText : AnsiString read FMessagesNotificationText write SetMessagesNotificationText;

      // Methods
      class procedure StartApplication(mainForm : TForm);
      class procedure ExitApplication;
      class procedure RunInBackground;
      class procedure RunInForeground;
      class procedure CheckNodeIsReady;

      // Show Dialogs
      class procedure ShowAboutBox(parentForm : TForm);
      class procedure ShowOptionsDialog(parentForm: TForm);
      class procedure ShowAccountInfoDialog(parentForm: TForm; const account : Cardinal); overload;
      class procedure ShowAccountInfoDialog(parentForm: TForm; const account : TAccount); overload;
      class procedure ShowOperationInfoDialog(parentForm: TForm; const ophash : AnsiString); overload;
      class procedure ShowOperationInfoDialog(parentForm: TForm; const operation : TOperationResume); overload;
      class procedure ShowAccountOperationInfoDialog(parentForm: TForm; const account : TAccount; const operation : TOperationResume); overload;
      class procedure ShowNewOperationDialog(parentForm : TForm; accounts : TOrderedCardinalList; defaultFee : Cardinal);
      class procedure ShowSeedNodesDialog(parentForm : TForm);
      class procedure ShowPrivateKeysDialog(parentForm: TForm);
      class procedure ShowMemoText(parentForm: TForm; const ATitle : AnsiString; text : utf8string); overload;
      class procedure ShowMemoText(parentForm: TForm; const ATitle : AnsiString; text : TStrings); overload;
      class procedure UnlockWallet(parentForm: TForm);
      class procedure ChangeWalletPassword(parentForm: TForm);
      class procedure ShowInfo(parentForm : TForm; const ACaption, APrompt : String);
      class procedure ShowWarning(parentForm : TForm; const ACaption, APrompt : String);
      class procedure ShowError(parentForm : TForm; const ACaption, APrompt : String);
      class function AskQuestion(parentForm: TForm; AType:TMsgDlgType; const ACaption, APrompt : String; buttons: TMsgDlgButtons) : TMsgDlgBtn;
      class function AskEnterString(parentForm: TForm; const ACaption, APrompt : String; var Value : String) : Boolean;
      class function AskEnterProtectedString(parentForm: TForm; const ACaption, APrompt : String; var Value : String) : Boolean;

      // Show sub-forms
      class procedure ShowAccountExplorer;
      class procedure ShowBlockExplorer;
      class procedure ShowOperationsExplorer;
      class procedure ShowPendingOperations;
      class procedure ShowMessagesForm;
      class procedure ShowNodesForm;
      class procedure ShowLogsForm;
      class procedure ShowWallet;
      class procedure ShowSyncDialog;
  end;

  { TLoadSafeBoxThread }

  TLoadDatabaseThread = Class(TPCThread)
  protected
    procedure BCExecute; override;
  End;

  { Exceptions }

  EUserInterface = class(Exception);

implementation

uses
  UFRMAbout, UFRMNodesIp, UFRMPascalCoinWalletConfig, UFRMPayloadDecoder, UFRMMemoText,
  UOpenSSL, UFileStorage, UTime, UCommon, USettings, UCoreUtils;

{%region UI Lifecyle}

class procedure TUserInterface.StartApplication(mainForm : TForm);
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

    // Create root form and dependent components
    FMainForm := mainForm as TFRMMainForm;
    FMainForm.CloseAction := caNone;     // wallet is destroyed on ExitApplication
    if (FMainForm = nil)
      then raise Exception.Create('Main form is not TWallet');

    FTrayIcon := TTrayIcon.Create(FMainForm);
    FTrayIcon.OnDblClick := OnTrayIconDblClick;
    {$IFNDEF LCLCarbon}
    FTrayIcon.Visible := true;
    {$ENDIF}
    FTrayIcon.Hint := FMainForm.Caption;
    FTrayIcon.BalloonTitle := 'Restoring the window.';
    FTrayIcon.BalloonHint := 'Double click the system tray icon to restore Pascal Coin';
    FTrayIcon.BalloonFlags := bfInfo;
    {$IFNDEF LCLCarbon}
    FTrayIcon.Show;
    {$ENDIF}
    FTimerUpdateStatus := TTimer.Create(FMainForm);
    FTimerUpdateStatus.Enabled := false;
    FDisplayedStartupSyncDialog:=false;

    // Create log
    FLog := TLog.Create(nil); // independent component
    FLog.SaveTypes := [];

    // Create data directories
    If Not ForceDirectories(TFolderHelper.GetPascalCoinDataFolder) then
      raise Exception.Create('Cannot create dir: '+TFolderHelper.GetPascalCoinDataFolder);

    // Load settings
    TSettings.Load;
    TSettings.OnChanged.Add(OnSettingsChanged);

    // Open Wallet
    TWallet.Load;

    // Load peer list
    ips := TSettings.TryConnectOnlyWithThisFixedServers;
    TNode.DecodeIpStringToNodeServerAddressArray(ips,nsarr);
    TNetData.NetData.DiscoverFixedServersOnly(nsarr);
    setlength(nsarr,0);

    // Start Node
    FNode := TNode.Node;
    FNode.NetServer.Port := TSettings.InternetServerPort;
    FNode.PeerCache := TSettings.PeerCache+';'+CT_Discover_IPs;

    // Subscribe to Node events (TODO refactor with FNotifyEvents)
    FNodeNotifyEvents := TNodeNotifyEvents.Create(FMainForm);
    FNodeNotifyEvents.OnBlocksChanged := OnBlocksChanged;
    FNodeNotifyEvents.OnNodeMessageEvent :=  OnNodeMessageEvent;

    // Start RPC server
    FRPCServer := TRPCServer.Create;
    FRPCServer.WalletKeys := TWallet.Keys;
    FRPCServer.Active := TSettings.RpcPortEnabled;
    FRPCServer.ValidIPs := TSettings.RpcAllowedIPs;
    TWallet.Keys.SafeBox := FNode.Bank.SafeBox;

    // Initialise Database
    FNode.Bank.StorageClass := TFileStorage;
    TFileStorage(FNode.Bank.Storage).DatabaseFolder := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'Data';
    TFileStorage(FNode.Bank.Storage).Initialize;

    // Reading database
    TLoadDatabaseThread.Create(false).FreeOnTerminate := true;

    // Init
    TNetData.NetData.OnReceivedHelloMessage := OnReceivedHelloMessage;
    TNetData.NetData.OnStatisticsChanged := OnNetStatisticsChanged;
    TNetData.NetData.OnNetConnectionsUpdated := OnNetConnectionsUpdated;
    TNetData.NetData.OnNodeServersUpdated := OnNetNodeServersUpdated;
    TNetData.NetData.OnBlackListUpdated := OnNetBlackListUpdated;

    // Start refresh timer
    FTimerUpdateStatus.OnTimer := OnTimerUpdateStatusTimer;
    FTimerUpdateStatus.Interval := 1000;
    FTimerUpdateStatus.Enabled := true;

    // open the sync dialog
    FMainForm.SyncControl.UpdateBlockChainState;   //TODO fix this work-flow
    RefreshConnectionStatusDisplay;
    FStarted := true;
  Except
    On E:Exception do begin
      E.Message := 'An error occurred during initialization. Application cannot continue:'+#10+#10+E.Message+#10+#10+'Application will close...';
      Application.MessageBox(PChar(E.Message),PChar(Application.Title),MB_ICONERROR+MB_OK);
      Halt;
    end;
  end;


  // Notify accounts changed
  OnAccountsChanged(FMainForm);

  // Refresh status bar since may not have been displayed
  SetStatusBar0Text(FStatusBar0Text);
  SetStatusBar0Text(FStatusBar1Text);
  SetStatusBar0Text(FStatusBar2Text);
  SetMessagesNotificationText(FMessagesNotificationText);

  // Show sync dialog
  ShowSyncDialog;

  // Final loading sequence
  TSettings.RunCount := TSettings.RunCount + 1;
  if TSettings.RunCount = 1 then begin
    ShowAboutBox(nil);
  end;
  TSettings.Save;
end;

class procedure TUserInterface.ExitApplication;
var
  i : Integer;
  step : String;
begin
  // Exit application
  TLog.NewLog(ltinfo,Classname,'Quit Application - START');
  Try
    step := 'Saving Settings';
    TSettings.OnChanged.Remove(OnSettingsChanged);
    TSettings.Save;

    // Destroys root form, non-modal forms and all their attached components
    step := 'Destroying UI graph';
    FMainForm.Destroy;
    FMainForm := nil;  // destroyed by FWallet
    FAccountExplorer := nil;  // destroyed by FWallet
    FPendingOperationForm := nil;  // destroyed by FWallet
    FOperationsExplorerForm := nil;  // destroyed by FWallet
    FBlockExplorerForm := nil;  // destroyed by FWallet
    FLogsForm := nil;  // destroyed by FWallet
    FNodesForm := nil;  // destroyed by FWallet
    FMessagesForm := nil;  // destroyed by FWallet
    FTrayIcon := nil; // destroyed by FWallet
    FNodeNotifyEvents := nil; // destroyed by FWallet

    step := 'Destroying components';
    FreeAndNil(FRPCServer);
    FreeAndNil(FPoolMiningServer);

    step := 'Assigning nil events';
    FLog.OnNewLog :=Nil;

    TNetData.NetData.OnReceivedHelloMessage := Nil;
    TNetData.NetData.OnStatisticsChanged := Nil;
    TNetData.NetData.OnNetConnectionsUpdated := Nil;
    TNetData.NetData.OnNodeServersUpdated := Nil;
    TNetData.NetData.OnBlackListUpdated := Nil;

    step := 'Assigning Nil to TNetData';
    TNetData.NetData.OnReceivedHelloMessage := Nil;
    TNetData.NetData.OnStatisticsChanged := Nil;

    step := 'Desactivating Node';
    TNode.Node.NetServer.Active := false;
    FNode := Nil;

    // Destroy NetData
    TNetData.NetData.Free;

    step := 'Processing messages 1';
    Application.ProcessMessages;

    step := 'Destroying Node';
    TNode.Node.Free;

    step := 'Processing messages 2';
    Application.ProcessMessages;

    FreeAndNil(FUILock);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error quiting application step: '+step+' Errors ('+E.ClassName+'): ' +E.Message);
    end;
  End;
  TLog.NewLog(ltinfo,Classname,'Error quiting application - END');
  FreeAndNil(FLog);
  Application.Terminate;
end;

class procedure TUserInterface.RunInBackground;
begin
  FMainForm.Hide();
  FMainForm.WindowState := wsMinimized;
  FTimerUpdateStatus.Enabled := false;
  FTrayIcon.Visible := True;
  FTrayIcon.ShowBalloonHint;
end;

class procedure TUserInterface.RunInForeground;
begin
  FTrayIcon.Visible := False;
  FTimerUpdateStatus.Enabled := true;
  FMainForm.Show();
  FMainForm.WindowState := wsNormal;
  Application.BringToFront();
end;

class procedure TUserInterface.FinishedLoadingDatabase;
begin
  FPoolMiningServer := TPoolMiningServer.Create;
  FPoolMiningServer.Port := TSettings.MinerServerRpcPort;
  FPoolMiningServer.MinerAccountKey := TWallet.MiningKey;
  FPoolMiningServer.MinerPayload := TSettings.MinerName;
  FNode.Operations.AccountKey := TWallet.MiningKey;
  FPoolMiningServer.Active := TSettings.MinerServerRpcActive;
  FPoolMiningServer.OnMiningServerNewBlockFound := OnMiningServerNewBlockFound;
  FMainForm.SyncControl.OnFinishedLoadingDatabase;
  FMainForm.OnFinishedLoadingDatabase;
  // Refresh UI
  OnAccountsChanged(FMainForm);
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
    ShowModal
  finally
    Free;
  end;
end;

class procedure TUserInterface.ShowOperationInfoDialog(parentForm: TForm; const ophash: AnsiString);
begin
  with TFRMPayloadDecoder.Create(parentForm) do
  try
    Init(CT_TOperationResume_NUL);
    if ophash <> '' then
      DoFind(ophash);
    ShowModal;
  finally
    Free;
  end;
end;

class procedure TUserInterface.ShowOperationInfoDialog(parentForm: TForm; const operation : TOperationResume); overload;
begin
  with TFRMPayloadDecoder.Create(parentForm) do
  try
    Init(operation);
    ShowModal;
  finally
    Free;
  end;
end;

class procedure TUserInterface.ShowAccountInfoDialog(parentForm: TForm; const account: Cardinal);
begin
  if account >= TUserInterface.Node.Bank.AccountsCount then
    raise EUserInterface.Create('Account not found');
  ShowAccountInfoDialog(parentForm, TUserInterface.Node.Operations.SafeBoxTransaction.Account(account));
end;

class procedure TUserInterface.ShowAccountInfoDialog(parentForm: TForm; const account : TAccount); overload;
begin
  ShowMemoText(parentForm, Format('Account: %s', [account.GetAccountString]), account.GetInfoText(Self.Node.Bank));
end;

class procedure TUserInterface.ShowAccountOperationInfoDialog(parentForm: TForm; const account: TAccount; const operation : TOperationResume);
var text : utf8string;
begin
  text := account.GetInfoText(Self.Node.Bank) + sLineBreak + sLineBreak + operation.GetInfoText(Self.Node.Bank);
  ShowMemoText(parentForm, Format('Account/Operation: %s/%s', [account.GetAccountString, operation.GetPrintableOPHASH]), text);
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
    WalletKeys := TWallet.Keys;
    ShowModal;
  Finally
    Free;
  End;
end;

class procedure TUserInterface.ShowSeedNodesDialog(parentForm : TForm);
Var FRM : TFRMNodesIp;
begin
  FRM := TFRMNodesIp.Create(parentForm);
  Try
    FRM.ShowModal;
  Finally
    FRM.Free;
  End;
end;

class procedure TUserInterface.ShowPrivateKeysDialog(parentForm: TForm);
Var FRM : TFRMWalletKeys;
begin
  FRM := TFRMWalletKeys.Create(parentForm);
  try
    FRM.ShowModal;
  finally
    FRM.Free;
  end;
end;

class procedure TUserInterface.ShowMemoText(parentForm: TForm; const ATitle : AnsiString; text : utf8string);
begin
  with TFRMMemoText.Create(parentForm) do begin
    try
      Caption := ATitle;
      Memo.Append(text);
      ShowModal;
    finally
      Free;
    end;
  end;
end;

class procedure TUserInterface.ShowMemoText(parentForm: TForm; const ATitle : AnsiString; text : TStrings);
begin
  with TFRMMemoText.Create(parentForm) do begin
    try
      Caption := ATitle;
      Memo.Lines.Assign(text);
      ShowModal;
    finally
      Free;
    end;
  end;
end;

class procedure TUserInterface.ChangeWalletPassword(parentForm: TForm);
var
  pwd1,pwd2 : String;
  locked : boolean;
begin
  pwd1 := ''; pwd2 := '';
  locked := (NOT TWallet.Keys.HasPassword) OR (NOT TWallet.Keys.IsValidPassword);
  if Not AskEnterProtectedString(parentForm, 'Change password','Enter new password',pwd1)
    then exit;
  if trim(pwd1)<>pwd1 then
    raise Exception.Create('Password cannot start or end with a space character');
  if Not AskEnterProtectedString(parentForm, 'Change password', 'Enter new password again',pwd2)
    then exit;
  if pwd1<>pwd2 then
    raise Exception.Create('Two passwords are different!');
  TWallet.Keys.WalletPassword := pwd1;
  if locked then
    TWallet.Keys.LockWallet;

  ShowWarning(parentform,
  'Password Changed',
  'Your password has been changed.' + #10+#10 +
  'Please ensure you remember your password.'+#10+
  'If you lose your password your accounts and funds will be lost forever.');
end;

class procedure TUserInterface.UnlockWallet(parentForm: TForm);
Var pwd : String;
begin
  pwd := '';
  Repeat
    if Not AskEnterProtectedString(parentForm, 'Wallet password','Enter wallet password',pwd) then exit;
    TWallet.Keys.WalletPassword := pwd;
    if Not TWallet.Keys.IsValidPassword then
      ShowError(parentForm, 'Invalid Password', 'The password you have entered is incorrect.');
  Until TWallet.Keys.IsValidPassword;
end;

class procedure TUserInterface.ShowInfo(parentForm : TForm; const ACaption, APrompt : String);
begin
  MessageDlg(ACaption, APrompt, mtInformation, [mbOK], 0, mbOK);
end;

class procedure TUserInterface.ShowWarning(parentForm : TForm; const ACaption, APrompt : String);
begin
  MessageDlg(ACaption, APrompt, mtWarning, [mbOK], 0, mbOK);
end;

class procedure TUserInterface.ShowError(parentForm : TForm; const ACaption, APrompt : String);
begin
  MessageDlg(ACaption, APrompt, mtError, [mbOK], 0, mbOK);
end;

class function TUserInterface.AskQuestion(parentForm: TForm; AType:TMsgDlgType; const ACaption, APrompt : String; buttons: TMsgDlgButtons) : TMsgDlgBtn;
var modalResult : TModalResult;
begin
  modalResult := MessageDlg(ACaption, APrompt, AType, Buttons, 0, mbNo);
  case modalResult of
    mrYes: Result := mbYes;
    mrNo: Result := mbNo;
    mrOK: Result := mbOK;
    mrCancel: Result := mbCancel;
    mrAbort: Result := mbAbort;
    mrRetry: Result := mbRetry;
    mrIgnore:Result := mbIgnore;
    mrAll: Result := mbAll;
    mrNoToAll: Result := mbNoToAll;
    mrYesToAll: Result := mbYesToAll;
    mrClose: Result := mbClose;
    else raise Exception.Create('Internal Error: [TUserInterface.AskQuestion] unsupported dialog result');
  end;
end;

class function TUserInterface.AskEnterString(parentForm: TForm; const ACaption, APrompt : String; var Value : String) : Boolean;
begin
  Result := InputQuery(ACaption, APrompt, Value);
end;

class function TUserInterface.AskEnterProtectedString(parentForm: TForm; const ACaption, APrompt : String; var Value : String) : Boolean;
begin
  Result := InputQuery(ACaption, APrompt, true, Value);
end;

{%endregion}

{%region Show Forms}

class procedure TUserInterface.ShowAccountExplorer;
begin
  try
    FUILock.Acquire;
    if not Assigned(FAccountExplorer) then begin
       FAccountExplorer := TFRMAccountExplorer.Create(FMainForm);
       FAccountExplorer.CloseAction:= caFree;
       FAccountExplorer.OnDestroyed:= Self.OnSubFormDestroyed;
    end else
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
       FBlockExplorerForm := TFRMBlockExplorer.Create(FMainForm);
       FBlockExplorerForm.CloseAction:= caFree;
       FBlockExplorerForm.OnDestroyed:= Self.OnSubFormDestroyed;
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
      FOperationsExplorerForm := TFRMOperationExplorer.Create(FMainForm);
      FOperationsExplorerForm.CloseAction:= caFree;
      FOperationsExplorerForm.OnDestroyed:= Self.OnSubFormDestroyed;
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
      FPendingOperationForm := TFRMPendingOperations.Create(FMainForm);
      FPendingOperationForm.CloseAction:= caFree;
      FPendingOperationForm.OnDestroyed:= Self.OnSubFormDestroyed;
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
       FMessagesForm := TFRMMessages.Create(FMainForm);
       FMessagesForm.CloseAction:= caFree;
       FMessagesForm.OnDestroyed:= Self.OnSubFormDestroyed;
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
       FNodesForm := TFRMNodes.Create(FMainForm);
       FNodesForm.CloseAction:= caFree;
       FNodesForm.OnDestroyed:= Self.OnSubFormDestroyed;
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
       FLogsForm := TFRMLogs.Create(FMainForm);
       FLogsForm.CloseAction:= caFree;
       FLogsForm.OnDestroyed:= Self.OnSubFormDestroyed;
    end;
    FLogsForm.Show;
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.ShowWallet;
begin
  FMainForm.Mode := wmWallet;
end;

class procedure TUserInterface.ShowSyncDialog;
begin
  FMainForm.Mode := wmSync;
end;

{%endregion}

{%region Public methods}

class procedure TUserInterface.CheckNodeIsReady;
Var errorMessage : AnsiString;
begin
  if Not TNode.Node.IsReady(errorMessage) then begin
    Raise Exception.Create('You cannot do this operation now:'+#10+#10+errorMessage);
  end;
end;

class procedure TUserInterface.RefreshConnectionStatusDisplay;
var errors : AnsiString;
begin
  FUILock.Acquire;
  Try
    FMainForm.SyncControl.UpdateNodeStatus;
    OnNetStatisticsChanged(FMainForm);
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

class procedure TUserInterface.SetMainFormMode(AMode: TFRMMainFormMode);
begin
  if AMode <> FMainForm.Mode then
    case AMode of
      wmWallet: ShowWallet;
      wmSync: ShowSyncDialog;
      else raise Exception.Create('[Internal Error] TUserInterface.SetWalletMode - unsupported mode');
    end;
end;

class function TUserInterface.GetMainFormMode : TFRMMainFormMode;
begin
  Result := FMainForm.Mode;
end;

class procedure TUserInterface.SetStatusBar0Text(const text : AnsiString); static;
begin
  FStatusBar0Text := text;
  if Assigned(FMainForm) then
    FMainForm.sbStatusBar.Panels[0].Text := FStatusBar0Text;
end;

class procedure TUserInterface.SetStatusBar1Text(const text : AnsiString); static;
begin
  FStatusBar1Text := text;
  if Assigned(FMainForm) then
    FMainForm.sbStatusBar.Panels[1].Text := text;
end;

class procedure TUserInterface.SetStatusBar2Text(const text : AnsiString); static;
begin
  FStatusBar2Text := text;
  if Assigned(FMainForm) then
    FMainForm.sbStatusBar.Panels[2].Text := text;
end;

class procedure TUserInterface.SetMessagesNotificationText(const text : AnsiString); static;
begin
  FMessagesNotificationText := text;
  if Assigned(FMainForm.SyncControl) then begin
    if (text = '') then
      FMainForm.SyncControl.lblReceivedMessages.Visible := false;
    FMainForm.SyncControl.lblReceivedMessages.Caption := text;
  end;
end;

{%endregion}

{%region Handlers -- TODO: many need to be refactored out with TNotifyManyEvent}

class procedure TUserInterface.OnSettingsChanged(Sender: TObject);
Var wa : Boolean;
  i : Integer;
begin
  if TSettings.SaveLogFiles then begin
    if TSettings.SaveDebugLogs then
      FLog.SaveTypes := CT_TLogTypes_ALL
    else
      FLog.SaveTypes := CT_TLogTypes_DEFAULT;
    FLog.FileName := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'PascalCointWallet.log';
  end else begin
    FLog.SaveTypes := [];
    FLog.FileName := '';
  end;
  if Assigned(FNode) then begin
    wa := FNode.NetServer.Active;
    FNode.NetServer.Port := TSettings.InternetServerPort;
    FNode.NetServer.Active := wa;
    FNode.Operations.BlockPayload := TSettings.MinerName;
    FNode.NodeLogFilename := TFolderHelper.GetPascalCoinDataFolder+PathDelim+'blocks.log';
  end;
  if Assigned(FPoolMiningServer) then begin
    if FPoolMiningServer.Port <> TSettings.MinerServerRpcPort then begin
      FPoolMiningServer.Active := false;
      FPoolMiningServer.Port := TSettings.MinerServerRpcPort;
    end;
    FPoolMiningServer.Active :=TSettings.MinerServerRpcActive;
    FPoolMiningServer.UpdateAccountAndPayload(TWallet.MiningKey, TSettings.MinerName);
  end;
  if Assigned(FRPCServer) then begin
    FRPCServer.Active := TSettings.RpcPortEnabled;
    FRPCServer.ValidIPs := TSettings.RpcAllowedIPs;
  end;
end;

class procedure TUserInterface.OnAccountsChanged(Sender: TObject);
begin
  FUILock.Acquire;
  Try
    if Assigned(FAccountExplorer) then
      FAccountExplorer.RefreshAccountsGrid(true);
  finally
    FUILock.Release;
  end;
end;

class procedure TUserInterface.OnBlocksChanged(Sender: TObject);
begin
  FUILock.Acquire;
  try
    try
      if Assigned(FAccountExplorer) then
        FAccountExplorer.RefreshAccountsGrid(false);
      FMainForm.SyncControl.UpdateBlockChainState;
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
  nsarr := TNetData.NetData.NodeServersAddresses.GetValidNodeServers(true,0);
  s := '';
  for i := low(nsarr) to High(nsarr) do begin
    if (s<>'') then s := s+';';
    s := s + nsarr[i].ip+':'+IntToStr( nsarr[i].port );
  end;
  TSettings.PeerCache := s;
  TNode.Node.PeerCache := s;
end;

class procedure TUserInterface.OnNetStatisticsChanged(Sender: TObject);
Var NS : TNetStatistics;
begin
  FUILock.Acquire;   // TODO - lock may not be required
  Try
    //HS CheckMining;
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
  FPoolMiningServer.MinerAccountKey := TWallet.MiningKey;
end;

class procedure TUserInterface.OnTimerUpdateStatusTimer(Sender: TObject);
begin
  Try
    RefreshConnectionStatusDisplay;
    FMainForm.SyncControl.UpdateBlockChainState;
    FMainForm.SyncControl.UpdateNodeStatus;
  Except
    On E:Exception do begin
      E.Message := 'Exception at TimerUpdate '+E.ClassName+': '+E.Message;
      TLog.NewLog(lterror,ClassName,E.Message);
    end;
  End;
end;

class procedure TUserInterface.OnTrayIconDblClick(Sender: TObject);
begin
  RunInForeground;
end;

class procedure TUserInterface.OnSubFormDestroyed(Sender: TObject);
begin
  try
    FUILock.Acquire;
    if Sender = FAccountExplorer then
      FAccountExplorer := nil // form free's on close
    else if Sender = FPendingOperationForm then
      FPendingOperationForm := nil // form free's on close
    else if Sender = FOperationsExplorerForm then
      FOperationsExplorerForm := nil // form free's on close
    else if Sender = FBlockExplorerForm then
      FBlockExplorerForm := nil // form free's on close
    else if Sender = FLogsForm then
      FLogsForm := nil // form free's on close
    else if Sender = FNodesForm then
      FNodesForm := nil // form free's on close
    else if Sender = FMessagesForm then
      FMessagesForm := nil
    else
      raise Exception.Create('Internal Error: [NotifySubFormDestroyed] encountered an unknown sub-form instance');
  finally
    FUILock.Release;
  end;
end;

{%endregion}

{ TLoadDatabaseThread }

procedure TLoadDatabaseThread.BCExecute;
begin
  // Read Operations saved from disk
  TNode.Node.InitSafeboxAndOperations; // New Build 2.1.4 to load pending operations buffer
  TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  TNode.Node.NetServer.Active := true;
  Synchronize( TUserInterface.FinishedLoadingDatabase );
end;

initialization
// TODO - any startup code needed here?
finalization
// TODO - final cleanup here, show a modal dialog?
end.

