unit UUserInterface;

{ Copyright (c) 2018 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements:
  - Albert Molina: portions of code copied from https://github.com/PascalCoin/PascalCoin/blob/Releases/2.1.6/Units/Forms/UFRMWallet.pas

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Coordinates application startup and shutdown, node services, UI events and
  wallet dialogs. Owns background loading and serialized network/mining
  configuration changes. }

{ Public types in this unit

  Type                | Purpose
  --------------------+-------------------------------------------------------------------------
  EUserInterface      | Reports an invalid or unavailable UI operation.
  TUserInterface      | Coordinates node services, application events and wallet dialogs.
  TUserInterfaceState | Identifies loading, connectivity, synchronization and error states.
}

interface

{$I ..\config.inc}

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  SysUtils, Classes, Forms, Controls, {$IFDEF WINDOWS}Windows,{$ENDIF} ExtCtrls,
  Dialogs, {$IFDEF FPC}LCLType,{$ENDIF}
  UCommon, UCommon.UI,
  UBlockChain, UAccounts, UNode, UWallet, UConst, UFolderHelper, UGridUtils, URPC, UPoolMining,
  ULog, UThread, UNetProtocol, UCrypto, UBaseTypes, UPCDataTypes, UWalletStartup, UWalletStartupTask,
  UFRMMainForm, UCTRLSyncronization, UFRMAccountExplorer, UFRMOperationExplorer, UFRMPendingOperations, UFRMOperation,
  UFRMLogs, UFRMMessages, UFRMNodes, UFRMBlockExplorer, UFRMWalletKeys, UPCOrderedLists {$IFDEF TESTNET},UFRMRandomOperations, UAccountKeyStorage{$ENDIF};

type

  { TUserInterfaceState }

  TUserInterfaceState = (uisLoading, uisLoaded, uisDiscoveringPeers, uisSyncronizingBlockchain, uisActive, uisIsolated, uisDisconnected, uisError);

  { TUserInterface }

  TUserInterface = class
    private type

      { TLoadingThread }

      TLoadingThread = Class(TWalletStartupTask)
      private
        FMinerPayload : TBytes;
        FMinerName: String;
        procedure OnProgressNotify(sender : TObject; const AProgress : TProgressInfo);
      protected
        procedure ExecuteLoad; override;
      End;

      TMiningSettings = record
        Pool: TPoolMiningServer;
        Active, HasKey: Boolean;
        Port: Word;
        Key: TAccountKey;
        MinerName: String;
      end;

      { Listener shutdown can wait for an already received block buffer. Keep
        that join on an owned worker; its completion is queued by the existing
        startup-task lifecycle, never polled by the UI. }
      TNetworkSettingsTask = class(TWalletStartupTask)
      private
        FNode: TNode;
        FNetData: TNetData;
        FStopRPCServer: TRPCServer;
        FActive: Boolean;
        FPort: Word;
        FRequestVersion: Cardinal;
        FMinerName: String;
        FApplyMinerPayload: Boolean;
        FMining: TMiningSettings;
        FApplyMining, FApplyMiningKey: Boolean;
        FDidApplyPayload, FDidApplyMining, FDidApplyMiningKey: Boolean;
        FMiningTouched: Boolean;
        FSuperseded: LongInt;
        function RequestCurrent: Boolean;
      protected
        procedure ExecuteLoad; override;
      public
        constructor Create(ANode: TNode; ANetData: TNetData; AStopRPCServer: TRPCServer; AActive: Boolean; APort: Word; ARequestVersion: Cardinal; const AMinerName: String; AApplyMinerPayload: Boolean; const AMining: TMiningSettings; AApplyMining, AApplyMiningKey: Boolean); reintroduce;
        procedure Supersede;
      end;

    private
      // Root-form
      class var FUILock : UThread.TReadWriteLock;
      // Subforms
      class var FAccountExplorer : TFRMAccountExplorer;
      class var FPendingOperationForm : TFRMPendingOperations;
      class var FOperationsExplorerForm : TFRMOperationExplorer;
      class var FBlockExplorerForm : TFRMBlockExplorer;
      class var FLogsForm : TFRMLogs;
      class var FNodesForm : TFRMNodes;
      class var FMessagesForm : TFRMMessages;
      // Components
      class var FRPCServer : TRPCServer;
      class var FPoolMiningServer : TPoolMiningServer;
      // Local fields
      class var FStarted : boolean;
      class var FStopping : Boolean;
      class var FDispatchingAppStarted, FExitRequested : Boolean;
      class var FDispatchingLoaded: Boolean;
      class var FDispatchingNetworkSettings: Boolean;
      class var FSettingsLoaded : Boolean;
      class var FStartupOptions : TWalletStartupOptions;
      class var FNetData : TNetData;
      class var FMainForm : TFRMMainForm;
      class var FIsActivated : Boolean;
      class var FUpdating : Boolean;
      class var FLog : TLog;
      class var FNode : TNode;
      class var FEventNotify: TThreadNotify;
      class var FTrayIcon: TTrayIcon;
      class var FNodeNotifyEvents : TNodeNotifyEvents;
      class var FStatusBar0Text : String;
      class var FStatusBar1Text : String;
      class var FStatusBar2Text : String;
      class var FMessagesNotificationText : String;
      class var FDisplayedStartupSyncDialog : boolean;
      class var FAppStarted : TNotifyManyEvent;
      class var FLoading : TProgressNotifyMany;
      class var FLoaded : TNotifyManyEvent;
      class var FStateChanged : TNotifyManyEvent;
      class var FSynchronizationChanged : TNotifyManyEvent;
      class var FAccountsChanged : TNotifyManyEvent;
      class var FBlocksChanged : TNotifyManyEvent;
      class var FReceivedHelloMessage : TNotifyManyEvent;
      class var FNodeMessageEvent : TNodeMessageManyEvent;
      class var FNetStatisticsChanged : TNotifyManyEvent;
      class var FNetConnectionsUpdated : TNotifyManyEvent;
      class var FNetNodeServersUpdated : TNotifyManyEvent;
      class var FNetBlackListUpdated : TNotifyManyEvent;
      class var FMiningServerNewBlockFound : TNotifyManyEvent;
      class var FMiningStatusChanged : TNotifyManyEvent;
      class var FState : TUserInterfaceState;
      class var FStateText : String;
      class var FStorageFailureHandled: Boolean;
      class var FLastLoggedSafeboxProgress : TProgressInfo;
      class var FLoadingThread : TWalletStartupTask;
      class var FNetworkSettingsTask: TNetworkSettingsTask;
      class var FDesiredNetworkActive, FNetworkSettingsInitialized: Boolean;
      class var FDesiredNetworkPort: Word;
      class var FNetworkRequestVersion: Cardinal;
      class var FDesiredMinerName, FAppliedMinerName: String;
      class var FDesiredApplyMinerPayload, FHasAppliedMinerPayload: Boolean;
      class var FDesiredMining, FQueuedMining, FAppliedMining: TMiningSettings;
      class var FHasAppliedMining, FHasAppliedMiningKey: Boolean;
      class var FAppliedMiningKey: TAccountKey;
      // Getters/Setters
      class function GetEnabled : boolean; static;
      class procedure SetEnabled(ABool: boolean); static;
      class procedure SetState(AState : TUserInterfaceState); static;
      class function GetOffline : Boolean; static;
      class function GetNoStartupDialogs : Boolean; static;
      class function GetSynchronizationStatus : TNodeSynchronizationStatus; static;

      // Handlers
      class procedure OnSettingsChanged(Sender: TObject);
      class procedure OnLoaded(Sender: TObject);
      class procedure OnLoadingCompleted(Sender: TObject);
      class procedure RequestNetworkSettings(AActive: Boolean; APort: Word);
      class function SameMiningSettings(const A, B: TMiningSettings): Boolean;
      class function HasUnappliedMempoolSettings: Boolean;
      class procedure StartNetworkSettingsTask;
      class procedure OnNetworkSettingsCompleted(Sender: TObject);
      class procedure ApplyStorageFailure(const AFailure: TNodeStorageFailure);
      class procedure RefreshNodeState;
      class procedure OnNodeSnapshotChanged(Sender: TObject);
      class procedure OnSynchronizationChanged(Sender: TObject; const AStatus: TNodeSynchronizationStatus);
      class procedure OnConnectivityChanged(Sender: TObject);
      class procedure DispatchConnectivityChanged(Sender: TObject);
      class procedure OnMiningStatusChanged(Sender: TObject);
      class procedure DispatchMiningStatusChanged(Sender: TObject);
      class procedure OnReceivedHelloMessage(Sender: TObject);
      class procedure OnMiningServerNewBlockFound(Sender: TObject);
      class procedure UpdateMiningConfiguration(AAfterMinedBlock: Boolean = False);
      class procedure OnSubFormDestroyed(Sender: TObject);
      class procedure OnTrayIconDblClick(Sender: TObject);

      // Aux
      class procedure NotifyLoadedEvent(Sender: TObject);
      class procedure NotifyLoadingEvent(Sender: TObject; const AProgress : TProgressInfo);
      class procedure NotifyStateChanged(Sender: TObject);
      class procedure NotifyAccountsChangedEvent(Sender: TObject);
      class procedure NotifyBlocksChangedEvent(Sender: TObject);
      class procedure NotifyReceivedHelloMessageEvent(Sender: TObject);
      class procedure NotifyNodeMessageEventEvent(NetConnection: TNetConnection; MessageData: String);
      class procedure NotifyNetStatisticsChangedEvent(Sender: TObject);
      class procedure NotifyNetConnectionsUpdatedEvent(Sender: TObject);
      class procedure NotifyNetNodeServersUpdatedEvent(Sender: TObject);
      class procedure NotifyNetBlackListUpdatedEvent(Sender: TObject);
      class procedure NotifyMiningServerNewBlockFoundEvent(Sender: TObject);
    public
      // Properties
      class property Enabled : boolean read GetEnabled write SetEnabled;
      class property Started : boolean read FStarted;
      class property Stopping : Boolean read FStopping;
      class property Offline : Boolean read GetOffline;
      class property NoStartupDialogs : Boolean read GetNoStartupDialogs;
      class property State : TUserInterfaceState read FState write SetState;
      class property StateText : string read FStateText;
      class property Node : TNode read FNode;
      class property SynchronizationStatus : TNodeSynchronizationStatus read GetSynchronizationStatus;
      class property Log : TLog read FLog;
      class property PoolMiningServer : TPoolMiningServer read FPoolMiningServer;
      class property RPCServer : TRPCServer read FRPCServer;

      // Events
      class property AppStarted : TNotifyManyEvent read FAppStarted;
      class property Loading : TProgressNotifyMany read FLoading;
      class property Loaded : TNotifyManyEvent read FLoaded;
      class property StateChanged : TNotifyManyEvent read FStateChanged;
      class property SynchronizationChanged : TNotifyManyEvent read FSynchronizationChanged;
      class property AccountsChanged : TNotifyManyEvent read FAccountsChanged;
      class property BlocksChanged : TNotifyManyEvent read FBlocksChanged;
      class property ReceivedHelloMessage : TNotifyManyEvent read FReceivedHelloMessage;
      class property NodeMessageEvent : TNodeMessageManyEvent read FNodeMessageEvent;
      class property NetStatisticsChanged : TNotifyManyEvent read FNetStatisticsChanged;
      class property NetConnectionsUpdated : TNotifyManyEvent read FNetConnectionsUpdated;
      class property NetNodeServersUpdated : TNotifyManyEvent read FNetNodeServersUpdated;
      class property NetBlackListUpdated : TNotifyManyEvent read FNetBlackListUpdated;
      class property MiningServerNewBlockFound : TNotifyManyEvent read FMiningServerNewBlockFound;
      class property MiningStatusChanged : TNotifyManyEvent read FMiningStatusChanged;

      // Methods
      // Apply before constructing forms: their event hooks can initialize the node.
      class procedure ConfigureStartup(const AOptions : TWalletStartupOptions);
      // An optional task transfers ownership to the UI and uses the same joined
      // shutdown as normal disk restore. Useful for controlled startup hosts.
      class procedure StartApplication(mainForm : TForm; ALoadingTask : TWalletStartupTask = nil);
      class procedure ExitApplication;
      // Requests are serialized; repeated clicks replace the desired result
      // while an earlier listener change finishes without blocking the caller.
      class procedure ToggleConnectivity;
      class procedure RunInBackground;
      class procedure RunInForeground;
      class procedure CheckNodeIsReady;
      {$IFDEF TESTNET}
      {$IFDEF TESTING_NO_POW_CHECK}
      class procedure CreateABlock;
      {$ENDIF}
      {$ENDIF}

      // Show Dialogs
      class procedure ShowSendDialog(const AAccounts : array of Cardinal);
      class procedure ShowChangeKeyDialog(const AAccounts : array of Cardinal);
      class procedure ShowSellAccountsDialog(const AAccounts : array of Cardinal);
      class procedure ShowDelistAccountsDialog(const AAccounts : array of Cardinal);
      class procedure ShowChangeAccountInfoDialog(const AAccounts : array of Cardinal);
      class procedure ShowBuyAccountDialog(const AAccounts : array of Cardinal);
      class procedure ShowAboutBox(parentForm : TForm);
      class procedure ShowOptionsDialog(parentForm: TForm);
      class procedure ShowRPCCallsDialog(parentForm : TForm);
      class procedure ShowOperationsWorkbench(parentForm : TForm);
      class procedure ShowAccountInfoDialog(parentForm: TForm; const account : Cardinal); overload;
      class procedure ShowAccountInfoDialog(parentForm: TForm; const account : TAccount); overload;
      class procedure ShowOperationInfoDialog(parentForm: TForm; const ophash : String); overload;
      class procedure ShowOperationInfoDialog(parentForm: TForm; const operation : TOperationResume); overload;
      class procedure ShowAccountOperationInfoDialog(parentForm: TForm; const account : TAccount; const operation : TOperationResume); overload;
      class procedure ShowNewOperationDialog(parentForm : TForm; accounts : TOrderedCardinalList; defaultFee : Int64);
      class procedure ShowSeedNodesDialog(parentForm : TForm);
      class procedure ShowPrivateKeysDialog(parentForm: TForm);
      class procedure ShowMemoText(parentForm: TForm; const ATitle : String; text : String); overload;
      class procedure ShowMemoText(parentForm: TForm; const ATitle : String; text : TStrings); overload;
      class procedure UnlockWallet(parentForm: TForm);
      class procedure ChangeWalletPassword(parentForm: TForm);
      {$IFDEF TESTNET}
      class procedure ShowPublicKeysDialog(parentForm: TForm);
      class procedure ShowRandomOperationsDialog(parentForm: TForm);
      {$ENDIF}
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


  { Exceptions }

  EUserInterface = class(Exception);

implementation

uses
  {$IFNDEF FPC}System.SyncObjs,{$ENDIF}
  Generics.Collections, UFRMAbout, UFRMNodesIp, UFRMPascalCoinWalletConfig, UFRMPayloadDecoder, UFRMMemoText,
  UFileStorage, UTime, USettings, UCoreUtils, UMemory, UProgressDisplay,
  UWIZOperation, UWIZSendPASC, UWIZChangeKey, UWIZEnlistAccountForSale, UWIZDelistAccountFromSale, UWIZChangeAccountInfo, UWIZBuyAccount, UCoreObjects,
  UFRMRPCCalls, UFRMOperationsExplorer, UWalletLanguages;

const CT_ExperimentalAccountCacheRegisters = 250000;

procedure ConfigureExperimentalAccountCache(ANode: TNode);
begin
  {$IFDEF USE_ABSTRACTMEM}
  // A bounded, lazy product budget; core and classic keep their 50,000 default.
  if Assigned(ANode) and
    (ANode.Bank.SafeBox.PCAbstractMem.AccountCache.MaxRegisters <> CT_ExperimentalAccountCacheRegisters) then
    ANode.Bank.SafeBox.PCAbstractMem.AccountCache.MaxRegisters := CT_ExperimentalAccountCacheRegisters;
  {$ENDIF}
end;

{ TUserInterface.TNetworkSettingsTask }

constructor TUserInterface.TNetworkSettingsTask.Create(ANode: TNode; ANetData: TNetData; AStopRPCServer: TRPCServer; AActive: Boolean; APort: Word; ARequestVersion: Cardinal; const AMinerName: String; AApplyMinerPayload: Boolean; const AMining: TMiningSettings; AApplyMining, AApplyMiningKey: Boolean);
begin
  inherited Create;
  FNode := ANode;
  FNetData := ANetData;
  FStopRPCServer := AStopRPCServer;
  FActive := AActive;
  FPort := APort;
  FRequestVersion := ARequestVersion;
  FMinerName := AMinerName;
  FApplyMinerPayload := AApplyMinerPayload;
  FMining := AMining;
  FApplyMining := AApplyMining;
  FApplyMiningKey := AApplyMiningKey;
end;

procedure TUserInterface.TNetworkSettingsTask.Supersede;
begin
  {$IFDEF FPC}InterlockedExchange(FSuperseded,1);
  {$ELSE}TInterlocked.Exchange(FSuperseded,1);{$ENDIF}
end;

function TUserInterface.TNetworkSettingsTask.RequestCurrent: Boolean;
begin
  CheckCancelled;
  {$IFDEF FPC}Result := InterlockedCompareExchange(FSuperseded,0,0) = 0;
  {$ELSE}Result := TInterlocked.CompareExchange(FSuperseded,0,0) = 0;{$ENDIF}
end;

procedure TUserInterface.TNetworkSettingsTask.ExecuteLoad;
var LLockedMempool: TPCOperationsComp;
begin
  if not RequestCurrent then Exit;
  // A storage failure is restart-only. Retire services on this owned worker,
  // never by joining RPC, peer or miner listeners from a UI state callback.
  if FNode.HasStorageFailure then begin
    FActive := False;
    FApplyMinerPayload := False;
    FApplyMiningKey := False;
    FMining.Active := False;
    FMining.HasKey := False;
  end;
  if Assigned(FStopRPCServer) then FStopRPCServer.Active := False;
  if not RequestCurrent then Exit;
  // Publish OFF before a potentially long listener join. The core preserves
  // physical buffered work and denies new acquisition independently of the UI.
  if not FActive and FNetData.NetConnectionsActive then
    FNetData.NetConnectionsActive := false;
  if not RequestCurrent then Exit;
  if FNode.NetServer.Port <> FPort then begin
    FNode.NetServer.Active := false;
    if not RequestCurrent then Exit;
    FNode.NetServer.Port := FPort;
  end;
  if not RequestCurrent then Exit;
  if FNetData.NetConnectionsActive <> FActive then
    FNetData.NetConnectionsActive := FActive
  else if FNode.NetServer.Active <> FActive then
    FNode.NetServer.Active := FActive;
  if (FApplyMinerPayload or FApplyMiningKey) and FNode.PendingOperationsAvailable then begin
    try
    // This is a cancellable lock wait on the worker, not a UI settings read.
    // An unchanged payload/key snapshot never enters the mempool gate at all.
    while not FNode.TryLockNode(50) do
      if not RequestCurrent then Exit;
    try
      if not RequestCurrent then Exit;
      LLockedMempool := FNode.LockMempoolWrite;
      try
        if FApplyMinerPayload then begin
          LLockedMempool.BlockPayload := TEncoding.ANSI.GetBytes(FMinerName);
          FDidApplyPayload := true;
        end;
        if FApplyMiningKey then begin
          LLockedMempool.AccountKey := FMining.Key;
          FDidApplyMiningKey := true;
        end;
      finally
        FNode.UnlockMempoolWrite;
      end;
    finally
      FNode.UnlockNode;
    end;
    except
      // A catch-up scope can begin after the cached availability check. Keep
      // the settings intent; its final availability event retries this part.
      on EPendingOperationsUnavailable do ;
    end;
  end;
  if not RequestCurrent then Exit;
  // Pool callbacks may acquire their own locks or join listeners. Never retain
  // the node write scope across any of these operations.
  if FApplyMining and Assigned(FMining.Pool) then begin
    FMiningTouched := true;
    if (not FMining.Active) or (FMining.Pool.Port <> FMining.Port) then
      FMining.Pool.Active := false;
    if not RequestCurrent then Exit;
    FMining.Pool.Port := FMining.Port;
    if FMining.HasKey then
      FMining.Pool.UpdateAccountAndPayload(FMining.Key, TEncoding.ANSI.GetBytes(FMining.MinerName))
    else
      FMining.Pool.MinerPayload := TEncoding.ANSI.GetBytes(FMining.MinerName);
    if not RequestCurrent then Exit;
    if FMining.Pool.Active <> FMining.Active then
      FMining.Pool.Active := FMining.Active;
    FDidApplyMining := true;
  end;
end;

class function TUserInterface.SameMiningSettings(const A, B: TMiningSettings): Boolean;
begin
  Result := (A.Pool = B.Pool) and (A.Active = B.Active) and
    (A.Port = B.Port) and (A.MinerName = B.MinerName) and (A.HasKey = B.HasKey);
  if Result and A.HasKey then Result := TAccountComp.EqualAccountKeys(A.Key, B.Key);
end;

class function TUserInterface.HasUnappliedMempoolSettings: Boolean;
begin
  Result := FNetworkSettingsInitialized and
    ((FDesiredApplyMinerPayload and ((not FHasAppliedMinerPayload) or
      (FAppliedMinerName <> FDesiredMinerName))) or
     (FDesiredMining.HasKey and ((not FHasAppliedMiningKey) or
      not TAccountComp.EqualAccountKeys(FDesiredMining.Key,FAppliedMiningKey))));
end;

class procedure TUserInterface.RequestNetworkSettings(AActive: Boolean; APort: Word);
var LMinerName: String; LApplyPayload: Boolean;
begin
  if FStopping or not Assigned(FNetData) or not Assigned(FNode) then Exit;
  AActive := AActive and not Offline and not FNode.HasStorageFailure;
  LMinerName := TSettings.MinerName;
  LApplyPayload := not (FState in [uisLoading, uisError]) and not FNode.HasStorageFailure;
  if FNetworkSettingsInitialized and (FDesiredNetworkActive = AActive) and
    (FDesiredNetworkPort = APort) and (FDesiredMinerName = LMinerName) and
    (FDesiredApplyMinerPayload = LApplyPayload) and
    SameMiningSettings(FDesiredMining, FQueuedMining) then Exit;
  FNetworkSettingsInitialized := true;
  FDesiredNetworkActive := AActive;
  FDesiredNetworkPort := APort;
  FDesiredMinerName := LMinerName;
  FDesiredApplyMinerPayload := LApplyPayload;
  FQueuedMining := FDesiredMining;
  Inc(FNetworkRequestVersion);
  // Superseded tasks exit at a complete mutation boundary and still deliver
  // normal queued completion; cancellation is reserved for application quit.
  if Assigned(FNetworkSettingsTask) then FNetworkSettingsTask.Supersede;
  StartNetworkSettingsTask;
end;

class procedure TUserInterface.StartNetworkSettingsTask;
var LStopRPCServer: TRPCServer;
begin
  if FStopping or FDispatchingNetworkSettings or Assigned(FNetworkSettingsTask) or
    not FNetworkSettingsInitialized then Exit;
  LStopRPCServer := Nil;
  if Assigned(FNode) and FNode.HasStorageFailure then LStopRPCServer := FRPCServer;
  FNetworkSettingsTask := TNetworkSettingsTask.Create(FNode, FNetData, LStopRPCServer,
    FDesiredNetworkActive, FDesiredNetworkPort, FNetworkRequestVersion,
    FDesiredMinerName, FDesiredApplyMinerPayload and
      ((not FHasAppliedMinerPayload) or (FAppliedMinerName <> FDesiredMinerName)),
    FDesiredMining, Assigned(FDesiredMining.Pool) and
      ((not FHasAppliedMining) or not SameMiningSettings(FDesiredMining, FAppliedMining)),
    FDesiredMining.HasKey and ((not FHasAppliedMiningKey) or
      not TAccountComp.EqualAccountKeys(FDesiredMining.Key, FAppliedMiningKey)));
  FNetworkSettingsTask.OnCompleted := OnNetworkSettingsCompleted;
  FNetworkSettingsTask.Start;
end;

class procedure TUserInterface.OnNetworkSettingsCompleted(Sender: TObject);
var
  LTask: TNetworkSettingsTask;
  LVersion: Cardinal;
  LError: String;
begin
  if FStopping or (Sender <> FNetworkSettingsTask) then Exit;
  LTask := FNetworkSettingsTask;
  LVersion := LTask.FRequestVersion;
  LError := LTask.ErrorMessage;
  FDispatchingNetworkSettings := true;
  try
    LTask.OnCompleted := nil;
    // Keep the field assigned through the join: reentrant settings callbacks may
    // replace the desired request, but cannot start a second listener mutation.
    LTask.CancelAndWait;
    FNetworkSettingsTask := nil;
    if LTask.FDidApplyPayload then begin
      FAppliedMinerName := LTask.FMinerName;
      FHasAppliedMinerPayload := true;
    end;
    if LTask.FDidApplyMiningKey then begin
      FAppliedMiningKey := LTask.FMining.Key;
      FHasAppliedMiningKey := true;
    end;
    if LTask.FDidApplyMining then begin
      FAppliedMining := LTask.FMining;
      FHasAppliedMining := true;
    end else if LTask.FMiningTouched then
      // A superseding request may have interrupted between pool mutations.
      // The next snapshot must restore the complete desired configuration.
      FHasAppliedMining := false;
    LTask.Free;
    if LError <> '' then
      TLog.NewLog(lterror, ClassName, 'Unable to apply networking settings: ' + LError);
    // A port-only change need not produce connectivity traffic. Publish its
    // completed primitive state explicitly so the footer never waits for I/O.
    NotifyNetStatisticsChangedEvent(FNetData);
    FNetConnectionsUpdated.Invoke(FNetData);
  finally
    FDispatchingNetworkSettings := false;
    if FExitRequested then begin
      FExitRequested := false;
      ExitApplication;
    end else
      // A failed request is not retried forever. A newer explicit request still
      // wins after full retirement, even if a UI subscriber raised above.
      if (FNetworkRequestVersion <> LVersion) or
        ((LError='') and Assigned(FNode) and FNode.PendingOperationsAvailable and
         HasUnappliedMempoolSettings) then StartNetworkSettingsTask;
  end;
end;

class procedure TUserInterface.ToggleConnectivity;
begin
  if FStopping or Offline or (FState in [uisLoading, uisError]) then Exit;
  if FNetworkSettingsInitialized then
    RequestNetworkSettings(not FDesiredNetworkActive, TSettings.InternetServerPort)
  else
    RequestNetworkSettings(not FNetData.NetConnectionsActive, TSettings.InternetServerPort);
end;

{ TUserInterface.TLoadingThread }

procedure TUserInterface.TLoadingThread.OnProgressNotify(sender: TObject; const AProgress: TProgressInfo);
begin
  ReportProgress(AProgress);
end;

procedure TUserInterface.TLoadingThread.ExecuteLoad;
var
  LLockedMempool : TPCOperationsComp;
begin
  // Loading local files is independent of network readiness. An empty chain is a
  // valid first run; discovery and synchronization are reported after loading.
  TUserInterface.FNode.InitSafeboxAndOperations($FFFFFFFF, OnProgressNotify);
  CheckCancelled;
  // Final mempool work can wait for the pending-buffer saver. Keep that wait
  // on the owned loader, rather than blocking the UI in the Loaded callback.
  ReportProgress(TProgressInfo.Create(psFinalizingRestore));
  LLockedMempool := TUserInterface.FNode.LockMempoolWrite;
  try
    LLockedMempool.BlockPayload := FMinerPayload;
  finally
    TUserInterface.FNode.UnlockMempoolWrite;
  end;
  CheckCancelled;
end;

{%region UI Lifecyle}

class procedure TUserInterface.ConfigureStartup(const AOptions : TWalletStartupOptions);
begin
  if FIsActivated then
    raise EUserInterface.Create('Startup options must be applied before starting the wallet.');
  FStartupOptions := AOptions;
  if FStartupOptions.DataDirectory <> '' then
    TNode.SetPascalCoinDataFolder(FStartupOptions.DataDirectory);
  // The networking singleton starts its maintenance threads in its constructor.
  // Disable it before any persisted peers can be supplied or forms constructed.
  if Offline then begin
    TCrypto.InitCrypto;
    FNetData := TNetData.NetData;
    FNetData.NetConnectionsActive := false;
    FNode := TNode.Node;
  end;
end;

class function TUserInterface.GetOffline : Boolean;
begin
  Result := FStartupOptions.Offline;
end;

class function TUserInterface.GetNoStartupDialogs : Boolean;
begin
  Result := FStartupOptions.NoStartupDialogs;
end;

class procedure TUserInterface.StartApplication(mainForm : TForm; ALoadingTask : TWalletStartupTask);
Var ips : String;
  nsarr : TNodeServerAddressArray;
begin
  inherited;
  if FIsActivated then exit;
  FIsActivated := true;
  FLoadingThread := ALoadingTask;
  try
    // Initialize crypto module
    TCrypto.InitCrypto;

    // Create UI lock
    FUILock := UThread.TReadWriteLock.Create('TUserInterface.UILock',UThread.lrpSupportsRecursion);

    // Initialise field defaults
    FStarted := false;
    FStopping := false;
    FNetworkSettingsTask := nil;
    FDispatchingNetworkSettings := false;
    FNetworkSettingsInitialized := false;
    FNetworkRequestVersion := 0;
    FHasAppliedMinerPayload := false;
    FDesiredApplyMinerPayload := false;
    FHasAppliedMining := false;
    FHasAppliedMiningKey := false;
    FDesiredMining := Default(TMiningSettings);
    FQueuedMining := Default(TMiningSettings);
    FAppliedMining := Default(TMiningSettings);
    FRPCServer := Nil;
    FPoolMiningServer := Nil;
    FNodeNotifyEvents := nil;
    FUpdating := false;
    FStatusBar0Text := '';
    FStatusBar1Text := '';
    FStatusBar2Text := '';
    FStorageFailureHandled := False;
    FMessagesNotificationText := '';
    FLastLoggedSafeboxProgress := Default(TProgressInfo);

    // Create root form and dependent components
    FMainForm := mainForm as TFRMMainForm;
    FMainForm.CloseAction := caNone;     // wallet is destroyed on ExitApplication
    if (FMainForm = nil)
      then raise Exception.Create('Main form is not TWallet');

    FTrayIcon := TTrayIcon.Create(FMainForm);
    FTrayIcon.Icon.Assign(Application.Icon);
    FTrayIcon.OnDblClick := OnTrayIconDblClick;
    {$IFNDEF LCLCarbon}
    FTrayIcon.Visible := true;
    {$ENDIF}
    FTrayIcon.Hint := FMainForm.Caption;
    FTrayIcon.BalloonTitle := 'Restoring the window.';
    FTrayIcon.BalloonHint := 'Double click the system tray icon to restore Pascal Coin';
    FTrayIcon.BalloonFlags := bfInfo;
    {$IF Defined(FPC) and not Defined(LCLCarbon)}
    FTrayIcon.Show;
    {$ENDIF}

    FDisplayedStartupSyncDialog:=false;

    // Core producers publish real changes; this owned dispatcher marshals their
    // callbacks without a periodic UI refresh. It outlives producer shutdown.
    FEventNotify := TThreadNotify.Create(TThread.CurrentThread);
    FMainForm.SyncControl.SnapshotChanged.Add(OnNodeSnapshotChanged);

    // Create log
    FLog := TLog.Create(nil); // independent component
//    FLog.OnNewLog:= OnNewLog;
    FLog.SaveTypes := [];

    // Create data directories
    If Not ForceDirectories(TNode.GetPascalCoinDataFolder) then
      raise Exception.Create('Cannot create dir: '+TNode.GetPascalCoinDataFolder);

    // Load settings
    TSettings.Load;
    FSettingsLoaded := true;
    TWalletLanguages.Apply(TWalletLanguages.ReadPreference);
    TSettings.OnChanged.Add(OnSettingsChanged);

    // Open Wallet
    TWallet.Load;

    // Keep the node disconnected until its local database has finished loading.
    // In offline mode no peer list is ever supplied to the networking threads.
    FNetData := TNetData.NetData;
    FNetData.NetConnectionsActive := false;
    FNode := TNode.Node;
    ConfigureExperimentalAccountCache(FNode);
    // Load peer list
    ips := TSettings.TryConnectOnlyWithThisFixedServers;
    if not Offline then begin
      TNode.DecodeIpStringToNodeServerAddressArray(ips,nsarr);
      FNetData.DiscoverFixedServersOnly(nsarr);
    end;
    setlength(nsarr,0);

    // Start Node
    FNode.NetServer.Port := TSettings.InternetServerPort;
    FDesiredNetworkActive := false;
    FDesiredNetworkPort := TSettings.InternetServerPort;
    FDesiredMinerName := TSettings.MinerName;
    FNetworkSettingsInitialized := true;
    if not Offline then
      FNode.PeerCache := TSettings.PeerCache+';'+CT_Discover_IPs;
    FReceivedHelloMessage.Add(OnReceivedHelloMessage);
    FMiningServerNewBlockFound.Add(OnMiningServerNewBlockFound, True);

    // Subscribe to Node events (TODO refactor with FNotifyEvents)
    FNodeNotifyEvents := TNodeNotifyEvents.Create(FMainForm);
    FNodeNotifyEvents.OnBlocksChanged := NotifyBlocksChangedEvent;
    FNodeNotifyEvents.OnOperationsChanged := NotifyAccountsChangedEvent;
    FNodeNotifyEvents.OnSynchronizationChanged := OnSynchronizationChanged;
    FNodeNotifyEvents.OnNodeMessageEvent :=  NotifyNodeMessageEventEvent;

    // Keep the message history subscribed even before its window is opened.
    FMessagesForm := TFRMMessages.Create(FMainForm);
    FMessagesForm.CloseAction := caHide;

    // Start RPC server
    FRPCServer := TRPCServer.Create;
    FRPCServer.WalletKeys := TWallet.Keys;
    FRPCServer.Active := false;
    FRPCServer.ValidIPs := TSettings.JsonRpcAllowedIPs;
    TWallet.Keys.SafeBox := FNode.Bank.SafeBox;

    // Initialise Database
    FNode.Bank.StorageClass := TFileStorage;
    // TPCBank supplies the configured Data folder to the file storage backend.
    TFileStorage(FNode.Bank.Storage).Initialize;

    // Reading database
    State := uisLoading;
    FLoaded.Add(OnLoaded);
    if not Assigned(FLoadingThread) then
      FLoadingThread := TLoadingThread.Create;
    FLoadingThread.OnProgress := NotifyLoadingEvent;
    FLoadingThread.OnCompleted := OnLoadingCompleted;

    // Init
    FNetData.OnReceivedHelloMessage := NotifyReceivedHelloMessageEvent;
    FNetData.OnStatisticsChanged := NotifyNetStatisticsChangedEvent;
    FNetData.OnNetConnectionsUpdated := NotifyNetConnectionsUpdatedEvent;
    FNetData.OnNodeServersUpdated := NotifyNetNodeServersUpdatedEvent;
    FNetData.OnBlackListUpdated := NotifyNetBlackListUpdatedEvent;
    FNetData.OnConnectivityChanged.Add(OnConnectivityChanged);

    // open the sync dialog
    FStarted := true;
    FMainForm.SubscribeApplicationEvents;
    FDispatchingAppStarted := true;
    try
      FAppStarted.Invoke(nil);
    finally
      FDispatchingAppStarted := false;
    end;
    if FExitRequested then begin
      ExitApplication;
      Exit;
    end;
    if FStopping then Exit;
    // Snapshot mutable UI settings before the worker starts. TAppParams lookup
    // must not race the RunCount update below or settings callbacks.
    if FLoadingThread is TLoadingThread then begin
      TLoadingThread(FLoadingThread).FMinerName := TSettings.MinerName;
      TLoadingThread(FLoadingThread).FMinerPayload := TEncoding.ANSI.GetBytes(TSettings.MinerName);
    end;
    FLoadingThread.Start;
  Except
    On E:Exception do begin
      FStateText := 'An error occurred during initialization: ' + E.Message;
      State := uisError;
      TLog.NewLog(lterror, ClassName, FStateText);
      if not NoStartupDialogs then
        Application.MessageBox(PChar(FStateText), PChar(Application.Title), MB_ICONERROR + MB_OK);
      ExitCode := 1;
      ExitApplication;
      Exit;
    end;
  end;

  // Notify accounts changed
  NotifyAccountsChangedEvent(FMainForm);

  // Show sync dialog
  ShowSyncDialog;

  // Final loading sequence
  TSettings.RunCount := TSettings.RunCount + 1;
  // First run must not enter a modal loop before the main window is shown.
  // About remains available from its explicitly owned menu command.
  TSettings.Save;
end;

class procedure TUserInterface.ExitApplication;
var
  step : String;
begin
  if FStopping then Exit;
  // Event receivers may request shutdown during AppStarted. Finish dispatching
  // before destroying receivers or mutating their subscription arrays.
  if FDispatchingAppStarted or FDispatchingNetworkSettings then begin
    FExitRequested := true;
    Exit;
  end;
  FStopping := true;
  FStarted := false;
  // Publish application cancellation before closing listeners can join workers
  // that are finishing a received buffer; network-off alone must drain it.
  if Assigned(FNetData) then FNetData.BeginShutdown;
  TLog.NewLog(ltinfo,Classname,'Quit Application - START');
  Try
    // A main-thread WaitFor may dispatch synchronized core notifications. Keep
    // the receiver graph alive, but silence forwarding before joining.
    step := 'Stopping networking settings task';
    if Assigned(FNetworkSettingsTask) then begin
      FNetworkSettingsTask.OnCompleted := nil;
      FNetworkSettingsTask.CancelAndWait;
      FreeAndNil(FNetworkSettingsTask);
    end;

    step := 'Stopping database loader';
    if Assigned(FLoadingThread) then begin
      FLoadingThread.CancelAndWait;
      FreeAndNil(FLoadingThread);
    end;

    step := 'Deregistering events';
    if Assigned(FMainForm) and Assigned(FMainForm.SyncControl) then
      FMainForm.SyncControl.SnapshotChanged.Remove(OnNodeSnapshotChanged);
    if FSettingsLoaded then
      TSettings.OnChanged.Remove(OnSettingsChanged);
    FReceivedHelloMessage.Remove(OnReceivedHelloMessage);
    FMiningServerNewBlockFound.Remove(OnMiningServerNewBlockFound);
    FLoaded.Remove(OnLoaded);

    step := 'Saving Settings';
    if FSettingsLoaded then
      TSettings.Save;

    step := 'Disconnecting event sources';
    if Assigned(FNetData) then begin
      FNetData.OnReceivedHelloMessage := Nil;
      FNetData.OnStatisticsChanged := Nil;
      FNetData.OnNetConnectionsUpdated := Nil;
      FNetData.OnNodeServersUpdated := Nil;
      FNetData.OnBlackListUpdated := Nil;
      FNetData.OnConnectivityChanged.Remove(OnConnectivityChanged);
    end;
    if Assigned(FPoolMiningServer) then begin
      FPoolMiningServer.OnStatusChanged := nil;
      FPoolMiningServer.OnMiningServerNewBlockFound := nil;
    end;
    if Assigned(FNodeNotifyEvents) then begin
      FNodeNotifyEvents.OnBlocksChanged := nil;
      FNodeNotifyEvents.OnOperationsChanged := nil;
      FNodeNotifyEvents.OnSynchronizationChanged := nil;
      FNodeNotifyEvents.OnNodeMessageEvent := nil;
    end;

    // Destroys root form, non-modal forms and all their attached components
    step := 'Destroying UI graph';
    FreeAndNil(FMainForm);
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

    // The wallet owns a safebox key index, so release it while the node is alive.
    step := 'Closing wallet';
    TWallet.Unload;

    step := 'Assigning nil events';
    if Assigned(FLog) then
      FLog.OnNewLog := Nil;

    step := 'Desactivating Node';
    if Assigned(FNode) then
      FNode.NetServer.Active := false;

    // Destroy NetData
    FreeAndNil(FNetData);

    step := 'Destroying Node';
    FreeAndNil(FNode);

    // All core producers are joined before discarding queued UI notifications.
    FreeAndNil(FEventNotify);

    FreeAndNil(FUILock);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,Classname,'Error quiting application step: '+step+' Errors ('+E.ClassName+'): ' +E.Message);
    end;
  End;
  TLog.NewLog(ltinfo,Classname,'Quit Application - END');
  FreeAndNil(FLog);
  Application.Terminate;
end;

class procedure TUserInterface.RunInBackground;
begin
  FMainForm.Hide();
  FMainForm.WindowState := wsMinimized;
  FTrayIcon.Visible := True;
  FTrayIcon.ShowBalloonHint;
end;

class procedure TUserInterface.RunInForeground;
begin
  FTrayIcon.Visible := False;
  FMainForm.Show();
  FMainForm.WindowState := wsNormal;
  RefreshNodeState;
  Application.BringToFront();
end;

{%endregion}

{%region UI Handlers }

class procedure TUserInterface.OnTrayIconDblClick(Sender: TObject);
begin
  RunInForeground;
end;

class procedure TUserInterface.OnSubFormDestroyed(Sender: TObject);
begin
  // Form creation/destruction callbacks may re-enter this recursive writer.
  FUILock.AcquireWriteLock;
  try
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
    FUILock.ReleaseWriteLock;
  end;
end;

class procedure TUserInterface.OnLoaded(Sender: TObject);
begin
  if FStopping then Exit;
  // The normal loader applied its copied payload before completion. Custom
  // loaders and settings changed during restore use the same owned worker below.
  if FLoadingThread is TLoadingThread then begin
    FAppliedMinerName := TLoadingThread(FLoadingThread).FMinerName;
    FHasAppliedMinerPayload := true;
  end;

  FPoolMiningServer := TPoolMiningServer.Create;
  FPoolMiningServer.OnStatusChanged := OnMiningStatusChanged;
  FDesiredNetworkActive := not Offline;
  UpdateMiningConfiguration;
  FPoolMiningServer.OnMiningServerNewBlockFound := NotifyMiningServerNewBlockFoundEvent;
  FRPCServer.Active := (not Offline) and (not FNode.HasStorageFailure) and TSettings.JsonRpcPortEnabled;
  RequestNetworkSettings(not Offline, TSettings.InternetServerPort);
  if not Offline then begin
    FNode.AutoDiscoverNodes(CT_Discover_IPs);
  end;
  State := uisLoaded;
  RefreshNodeState;
  ShowWallet;
end;

class procedure TUserInterface.OnLoadingCompleted(Sender: TObject);
var LStatus: TNodeSynchronizationStatus;
begin
  if FStopping or (Sender <> FLoadingThread) then Exit;
  try
    if FLoadingThread.ErrorMessage <> '' then
      raise EUserInterface.Create(FLoadingThread.ErrorMessage);
    State := uisLoaded;
  except
    on E : Exception do begin
      if FNode.HasStorageFailure then begin
        LStatus := FNode.GetSynchronizationStatus;
        ApplyStorageFailure(LStatus.StorageFailure);
      end else
        FStateText := 'Unable to load the local blockchain: ' + E.Message;
      TLog.NewLog(lterror, ClassName, FStateText);
      if not FNode.HasStorageFailure then FRPCServer.Active := false;
      FDesiredMining.Active := false;
      FDesiredMining.HasKey := false;
      State := uisError;
      RequestNetworkSettings(false, TSettings.InternetServerPort);
      // Keep the error visible and permit closing the wallet after a load error.
      FMainForm.SyncControl.ShowSplash := false;
      ShowSyncDialog;
    end;
  end;
end;

class function TUserInterface.GetSynchronizationStatus: TNodeSynchronizationStatus;
begin
  if Assigned(FNode) then Result := FNode.GetSynchronizationStatus
  else Result := Default(TNodeSynchronizationStatus);
end;

class procedure TUserInterface.ApplyStorageFailure(const AFailure: TNodeStorageFailure);
var LWasError: Boolean;
begin
  if not AFailure.HasFailure or FStorageFailureHandled then Exit;
  LWasError := FState=uisError;
  FStateText := FormatStorageFailure(AFailure);
  FDesiredMining.Active := False;
  FDesiredMining.HasKey := False;
  // Force one stop snapshot even if networking was already off. This also
  // retires an active RPC listener without changing persisted user settings.
  FNetworkSettingsInitialized := False;
  RequestNetworkSettings(False,TSettings.InternetServerPort);
  State := uisError;
  // A deferred loader event may have selected Error before this queued record
  // supplied its diagnostic text. Publish that first text even if state is equal.
  if LWasError then NotifyStateChanged(Nil);
  if Assigned(FMainForm) then begin
    FMainForm.SyncControl.ShowSplash := False;
    ShowSyncDialog;
  end;
  FStorageFailureHandled := True;
end;

class procedure TUserInterface.RefreshNodeState;
var LStatus: TNodeSynchronizationStatus;
begin
  if FStopping or FDispatchingLoaded or (not FStarted) then Exit;
  LStatus := GetSynchronizationStatus;
  if LStatus.StorageFailure.HasFailure then begin
    ApplyStorageFailure(LStatus.StorageFailure);
    Exit;
  end;
  if FState in [uisLoading, uisError] then Exit;
  // Physical work and final pending replay remain synchronization even when
  // acquisition is disabled. Let the node end that state before showing the
  // configured offline caption; the CLI flag still prevents all networking.
  case LStatus.State of
    nssOffline: State := uisDisconnected;
    nssDisconnected: State := uisIsolated;
    nssSynchronizing: State := uisSyncronizingBlockchain;
    nssIdle: begin
      if TNetData.NetData.IsDiscoveringServers then State := uisDiscoveringPeers
      else State := uisActive;
    end;
  end;
end;

class procedure TUserInterface.OnSynchronizationChanged(Sender: TObject; const AStatus: TNodeSynchronizationStatus);
begin
  if FStopping then Exit;
  if (AStatus.State = nssSynchronizing) and
    (AStatus.Progress.Stage = psDownloadingSafeboxChunks) then begin
    // The producer repeats progress while waiting for chunks. Log only changed
    // counters on this existing UI event, outside the downloading thread.
    if (FLastLoggedSafeboxProgress.Stage <> AStatus.Progress.Stage) or
      (FLastLoggedSafeboxProgress.CurrentPosition <> AStatus.Progress.CurrentPosition) or
      (FLastLoggedSafeboxProgress.TotalCount <> AStatus.Progress.TotalCount) or
      (FLastLoggedSafeboxProgress.ChunksCount <> AStatus.Progress.ChunksCount) or
      (FLastLoggedSafeboxProgress.BlockCount <> AStatus.Progress.BlockCount) or
      (FLastLoggedSafeboxProgress.Context <> AStatus.Progress.Context) then begin
      FLastLoggedSafeboxProgress := AStatus.Progress;
      TLog.NewLog(ltInfo, TNetData.ClassName, FormatProgressInfo(AStatus.Progress));
    end;
  end else
    FLastLoggedSafeboxProgress := Default(TProgressInfo);
  RefreshNodeState;
  FSynchronizationChanged.Invoke(Sender);
  // Connectivity changes never wait for catch-up's pending overlay. Reapply
  // only its deferred mining fields when the final coherent state is ready.
  if AStatus.PendingOperationsAvailable and HasUnappliedMempoolSettings then
    StartNetworkSettingsTask;
end;

class procedure TUserInterface.OnNodeSnapshotChanged(Sender: TObject);
begin
  RefreshNodeState;
end;

class procedure TUserInterface.OnConnectivityChanged(Sender: TObject);
begin
  if FStopping or not Assigned(FEventNotify) then Exit;
  FEventNotify.Invoke(Sender, DispatchConnectivityChanged);
end;

class procedure TUserInterface.DispatchConnectivityChanged(Sender: TObject);
begin
  if FStopping then Exit;
  RefreshNodeState;
  FNetConnectionsUpdated.Invoke(Sender);
end;

class procedure TUserInterface.OnMiningStatusChanged(Sender: TObject);
begin
  if FStopping or not Assigned(FEventNotify) then Exit;
  FEventNotify.Invoke(Sender, DispatchMiningStatusChanged);
end;

class procedure TUserInterface.DispatchMiningStatusChanged(Sender: TObject);
begin
  if FStopping then Exit;
  FMiningStatusChanged.Invoke(Sender);
end;

class procedure TUserInterface.OnSettingsChanged(Sender: TObject);
var
  i : Integer;
begin
  if FStopping then Exit;
  FMainForm.miLogs.Visible := TSettings.ShowLogs;
  if TSettings.ShowLogs then begin
    if not Assigned(FLogsForm) then begin
      FLogsForm := TFRMLogs.Create(FMainForm);
      FLogsForm.CloseAction := caHide;
    end;
    FLog.OnNewLog := FLogsForm.OnNewLog;
  end else begin
    FLog.OnNewLog := nil;
    if Assigned(FLogsForm) then
      FLogsForm.Hide;
  end;
  if TSettings.SaveLogFiles then begin
    if TSettings.SaveDebugLogs then
      FLog.SaveTypes := CT_TLogTypes_ALL
    else
      FLog.SaveTypes := CT_TLogTypes_DEFAULT;
    FLog.FileName := TNode.GetPascalCoinDataFolder+PathDelim+'PascalCointWallet.log';
  end else begin
    FLog.SaveTypes := [];
    FLog.FileName := '';
  end;
  if Assigned(FNode) then begin
    // Port changes and toolbar connectivity share one owner. In particular,
    // settings cannot reopen a listener while an earlier OFF request drains it.
    FNode.NodeLogFilename := TNode.GetPascalCoinDataFolder+PathDelim+'blocks.log';
  end;
  if Assigned(FPoolMiningServer) then begin
    UpdateMiningConfiguration;
  end;
  if Assigned(FRPCServer) then begin
    // The storage-error stop snapshot owns listener retirement on its worker.
    // A later settings event must not join that listener on the UI thread.
    if not FNode.HasStorageFailure then
      FRPCServer.Active := (not Offline) and (not (FState in [uisLoading, uisError])) and TSettings.JsonRpcPortEnabled;
    FRPCServer.ValidIPs := TSettings.JsonRpcAllowedIPs;
  end;
  if Assigned(FNetData) then begin
    // Match the classic wallet's persisted keys and its existing runtime
    // default when no checkpoint preference has yet been stored.
    if TSettings.AppParams.ParamByName[CT_PARAM_AllowDownloadNewCheckpointIfOlderThan].GetAsBoolean(
      FNetData.MinFutureBlocksToDownloadNewSafebox > 200) then
      FNetData.MinFutureBlocksToDownloadNewSafebox :=
        TSettings.AppParams.ParamByName[CT_PARAM_MinFutureBlocksToDownloadNewSafebox].GetAsInteger(
          FNetData.MinFutureBlocksToDownloadNewSafebox)
    else
      FNetData.MinFutureBlocksToDownloadNewSafebox := 0;
  end;
  RequestNetworkSettings(FDesiredNetworkActive, TSettings.InternetServerPort);
  RefreshNodeState;
end;

class procedure TUserInterface.OnReceivedHelloMessage(Sender: TObject);
Var nsarr : TNodeServerAddressArray;
  i : Integer;
  s : String;
begin
  // Internal handler
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

end;

class procedure TUserInterface.OnMiningServerNewBlockFound(Sender: TObject);
begin
  if FStopping then Exit;
  UpdateMiningConfiguration(True);
end;

class procedure TUserInterface.UpdateMiningConfiguration(AAfterMinedBlock: Boolean);
var
  LMiningKey : TAccountKey;
begin
  if not Assigned(FPoolMiningServer) then Exit;
  FDesiredMining.Pool := FPoolMiningServer;
  FDesiredMining.Port := TSettings.JsonRpcMinerServerPort;
  FDesiredMining.MinerName := TSettings.MinerName;
  FDesiredMining.Active := false;
  FDesiredMining.HasKey := false;
  if (not Offline) and (not FNode.HasStorageFailure) and TSettings.JsonRpcMinerServerActive then begin
    // Wallet selection/creation/rotation stays on its UI owner with the exact
    // existing policy. Only this copied public key crosses to the worker.
    if TWallet.Keys.TryGetMiningKey(TSettings.MinerPrivateKeyType,
      TSettings.MinerSelectedPublicKey, LMiningKey, AAfterMinedBlock) then begin
      FDesiredMining.Key := LMiningKey;
      FDesiredMining.HasKey := true;
      FDesiredMining.Active := true;
    end else
      TLog.NewLog(ltinfo, ClassName, 'Mining is inactive: select or add a wallet private key for rewards.');
  end;
  RequestNetworkSettings(FDesiredNetworkActive, TSettings.InternetServerPort);
end;

{%endregion}

{%region Show Dialogs}

class procedure TUserInterface.ShowSendDialog(const AAccounts : array of Cardinal);
var
  Scoped: TDisposables;
  wiz: TWIZSendPASCWizard;
  model: TWIZOperationsModel;
begin
  wiz := Scoped.AddObject(TWIZSendPASCWizard.Create(nil)) as TWIZSendPASCWizard;
  model := TWIZOperationsModel.Create(wiz, omtSendPasc);
  model.Account.SelectedAccounts := TNode.Node.GetAccounts(AAccounts, True);
  model.Account.Count := Length(model.Account.SelectedAccounts);
  wiz.Start(model);
end;

class procedure TUserInterface.ShowChangeKeyDialog(const AAccounts : array of Cardinal);
var
  Scoped: TDisposables;
  wiz: TWIZChangeKeyWizard;
  model: TWIZOperationsModel;
begin
  wiz := Scoped.AddObject(TWIZChangeKeyWizard.Create(nil)) as TWIZChangeKeyWizard;
  model := TWIZOperationsModel.Create(wiz, omtChangeKey);
  model.Account.SelectedAccounts := TNode.Node.GetAccounts(AAccounts, True);
  model.Account.Count := Length(model.Account.SelectedAccounts);
  wiz.Start(model);
end;

class procedure TUserInterface.ShowSellAccountsDialog(const AAccounts : array of Cardinal);
var
  Scoped: TDisposables;
  wiz: TWIZEnlistAccountForSaleWizard;
  model: TWIZOperationsModel;
begin
  wiz := Scoped.AddObject(TWIZEnlistAccountForSaleWizard.Create(nil)) as TWIZEnlistAccountForSaleWizard;
  model := TWIZOperationsModel.Create(wiz, omtEnlistAccountForSale);
  model.Account.SelectedAccounts := TNode.Node.GetAccounts(AAccounts, True);
  model.Account.Count := Length(model.Account.SelectedAccounts);
  wiz.Start(model);
end;

class procedure TUserInterface.ShowDelistAccountsDialog(const AAccounts : array of Cardinal);
var
  Scoped: TDisposables;
  wiz: TWIZDelistAccountFromSaleWizard;
  model: TWIZOperationsModel;
begin
  wiz := Scoped.AddObject(TWIZDelistAccountFromSaleWizard.Create(nil)) as TWIZDelistAccountFromSaleWizard;
  model := TWIZOperationsModel.Create(wiz, omtDelistAccountFromSale);
  model.Account.SelectedAccounts := TNode.Node.GetAccounts(AAccounts, True);
  model.Account.Count := Length(model.Account.SelectedAccounts);
  wiz.Start(model);
end;

class procedure TUserInterface.ShowChangeAccountInfoDialog( const AAccounts: array of Cardinal);
var
  Scoped: TDisposables;
  wiz: TWIZChangeAccountInfoWizard;
  model: TWIZOperationsModel;
begin
  wiz := Scoped.AddObject(TWIZChangeAccountInfoWizard.Create(nil)) as TWIZChangeAccountInfoWizard;
  model := TWIZOperationsModel.Create(wiz, omtChangeInfo);
  model.Account.SelectedAccounts := TNode.Node.GetAccounts(AAccounts, True);
  model.Account.Count := Length(model.Account.SelectedAccounts);
  wiz.Start(model);
end;

class procedure TUserInterface.ShowBuyAccountDialog( const AAccounts: array of Cardinal);
var
  Scoped: TDisposables;
  wiz: TWIZBuyAccountWizard;
  model: TWIZOperationsModel;
begin
  wiz := Scoped.AddObject(TWIZBuyAccountWizard.Create(nil)) as TWIZBuyAccountWizard;
  model := TWIZOperationsModel.Create(wiz, omtBuyAccount);
  model.Account.SelectedAccounts := TNode.Node.GetAccounts(AAccounts, True);
  model.Account.Count := Length(model.Account.SelectedAccounts);
  wiz.Start(model);
end;

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

class procedure TUserInterface.ShowRPCCallsDialog(parentForm : TForm);
var
  LScoped : TDisposables;
  LForm : TFRMRPCCalls;
begin
  if Offline then
    raise EUserInterface.Create('RPC calls are unavailable in offline mode.');
  LForm := LScoped.AddObject(TFRMRPCCalls.Create(nil)) as TFRMRPCCalls;
  LForm.PopupParent := parentForm;
  LForm.Position := poOwnerFormCenter;
  LForm.ServerURL := '127.0.0.1:' + IntToStr(FRPCServer.Port);
  LForm.ShowModal;
end;

class procedure TUserInterface.ShowOperationsWorkbench(parentForm : TForm);
var
  LScoped : TDisposables;
  LForm : TFRMOperationsExplorer;
begin
  LForm := LScoped.AddObject(TFRMOperationsExplorer.Create(nil)) as TFRMOperationsExplorer;
  LForm.PopupParent := parentForm;
  LForm.Position := poOwnerFormCenter;
  LForm.Offline := Offline;
  LForm.SourceNode := FNode;
  LForm.SourceWalletKeys := TWallet.Keys;
  LForm.ShowModal;
end;

class procedure TUserInterface.ShowOperationInfoDialog(parentForm: TForm; const ophash: String);
begin
  with TFRMPayloadDecoder.Create(parentForm) do
  try
    Init(CT_TOperationResume_NUL);
    if ophash <> '' then
      DoFind(ophash);
    Position := poMainFormCenter;
    ShowModal;
  finally
    Free;
  end;
end;

class procedure TUserInterface.ShowOperationInfoDialog(parentForm: TForm; const operation : TOperationResume);
begin
  with TFRMPayloadDecoder.Create(parentForm) do
  try
    Init(operation);
    Position := poMainFormCenter;
    ShowModal;
  finally
    Free;
  end;
end;

class procedure TUserInterface.ShowAccountInfoDialog(parentForm: TForm; const account: Cardinal);
begin
  if account >= TUserInterface.Node.Bank.AccountsCount then
    raise EUserInterface.Create('Account not found');
  ShowAccountInfoDialog(parentForm, TUserInterface.Node.GetAccount(account));
end;

class procedure TUserInterface.ShowAccountInfoDialog(parentForm: TForm; const account : TAccount);
begin
  ShowMemoText(parentForm, Format('Account: %s', [account.GetAccountString]), account.GetInfoText(Self.Node.Bank));
end;

class procedure TUserInterface.ShowAccountOperationInfoDialog(parentForm: TForm; const account: TAccount; const operation : TOperationResume);
var text : String;
begin
  text := account.GetInfoText(Self.Node.Bank) + sLineBreak + sLineBreak +
    operation.GetInfoText(Self.Node.Bank.BlocksCount, Self.Node, TWallet.Keys);
  ShowMemoText(parentForm, Format('Account/Operation: %s/%s', [account.GetAccountString, operation.GetPrintableOPHASH]), text);
end;

// TODO - refactor with accounts as ARRAY
class procedure TUserInterface.ShowNewOperationDialog(parentForm : TForm; accounts : TOrderedCardinalList; defaultFee : Int64);
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

class procedure TUserInterface.ShowMemoText(parentForm: TForm; const ATitle : String; text : String);
begin
  with TFRMMemoText.Create(parentForm) do begin
    try
      Caption := ATitle;
      Memo.Lines.Add(text);
      Position := poMainFormCenter;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

class procedure TUserInterface.ShowMemoText(parentForm: TForm; const ATitle : String; text : TStrings);
begin
  with TFRMMemoText.Create(parentForm) do begin
    try
      Caption := ATitle;
      Memo.Lines.Assign(text);
      Position := poMainFormCenter;
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

{$IFDEF TESTNET}
class procedure TUserInterface.ShowRandomOperationsDialog(parentForm: TForm);
begin
  with TFRMRandomOperations.Create(parentForm) do begin
    try
      SourceNode := TUserInterface.Node;
      SourceWalletKeys := TWallet.Keys;
      ShowModal;
    finally
      Free;
    end;
  end;
end;

class procedure TUserInterface.ShowPublicKeysDialog(parentForm: TForm);
var
  sl : TStrings;
  ak : TAccountKey;
  i, nmin,nmax : Integer;
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
    with TFRMMemoText.Create(parentForm) do begin
    try
      InitData('Keys in safebox',sl.Text);
      ShowModal;
    finally
      Free;
    end;

    end;
  finally
    sl.Free;
  end;
end;

{$IFDEF TESTING_NO_POW_CHECK}
class procedure TUserInterface.CreateABlock;
var
  ops : TPCOperationsComp;
  nba : TBlockAccount;
  errors : String;
begin
  ops := TPCOperationsComp.Create(Nil);
  Try
    ops.bank := FNode.Bank;
    ops.CopyFrom(FNode.Operations);
    ops.BlockPayload:= IntToStr(FNode.Bank.BlocksCount);
    ops.nonce := FNode.Bank.BlocksCount;
    ops.UpdateTimestamp;
    FNode.AddNewBlockChain(Nil,ops,nba,errors);
  finally
    ops.Free;
  end;
end;
{$ENDIF}
{$ENDIF}

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
  UpdateMiningConfiguration;
end;

class procedure TUserInterface.ShowInfo(parentForm : TForm; const ACaption, APrompt : String);
begin
  {$IFDEF FPC}MessageDlg{$ELSE}TaskMessageDlg{$ENDIF}(ACaption, APrompt, mtInformation, [mbOK], 0, mbOK);
end;

class procedure TUserInterface.ShowWarning(parentForm : TForm; const ACaption, APrompt : String);
begin
  {$IFDEF FPC}MessageDlg{$ELSE}TaskMessageDlg{$ENDIF}(ACaption, APrompt, mtWarning, [mbOK], 0, mbOK);
end;

class procedure TUserInterface.ShowError(parentForm : TForm; const ACaption, APrompt : String);
begin
  {$IFDEF FPC}MessageDlg{$ELSE}TaskMessageDlg{$ENDIF}(ACaption, APrompt, mtError, [mbOK], 0, mbOK);
end;

class function TUserInterface.AskQuestion(parentForm: TForm; AType:TMsgDlgType; const ACaption, APrompt : String; buttons: TMsgDlgButtons) : TMsgDlgBtn;
var modalResult : TModalResult;
begin
  modalResult := {$IFDEF FPC}MessageDlg{$ELSE}TaskMessageDlg{$ENDIF}(ACaption, APrompt, AType, Buttons, 0, mbNo);
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
{$IFNDEF FPC}
var
  LValues: TArray<String>;
{$ENDIF}
begin
  {$IFDEF FPC}
  Result := InputQuery(ACaption, APrompt, true, Value);
  {$ELSE}
  LValues := TArray<String>.Create(Value);
  Result := InputQuery(ACaption, [#31 + APrompt], LValues);
  if Result then
    Value := LValues[0];
  {$ENDIF}
end;

{%endregion}

{%region Show Forms}

class procedure TUserInterface.ShowAccountExplorer;
begin
  FUILock.AcquireWriteLock;
  try
    if not Assigned(FAccountExplorer) then begin
       FAccountExplorer := TFRMAccountExplorer.Create(FMainForm);
       FAccountExplorer.CloseAction:= caFree;
       FAccountExplorer.OnDestroyed:= Self.OnSubFormDestroyed;
    end else
      FAccountExplorer.Refresh;
    FAccountExplorer.Show;
  finally
    FUILock.ReleaseWriteLock;
  end;
end;

class procedure TUserInterface.ShowBlockExplorer;
begin
  FUILock.AcquireWriteLock;
  try
    if not Assigned(FBlockExplorerForm) then begin
       FBlockExplorerForm := TFRMBlockExplorer.Create(FMainForm);
       FBlockExplorerForm.CloseAction:= caFree;
       FBlockExplorerForm.OnDestroyed:= Self.OnSubFormDestroyed;
    end;
    FBlockExplorerForm.Show;
  finally
    FUILock.ReleaseWriteLock;
  end;
end;

class procedure TUserInterface.ShowOperationsExplorer;
begin
  FUILock.AcquireWriteLock;
  try
    if not Assigned(FOperationsExplorerForm) then begin
      FOperationsExplorerForm := TFRMOperationExplorer.Create(FMainForm);
      FOperationsExplorerForm.CloseAction:= caFree;
      FOperationsExplorerForm.OnDestroyed:= Self.OnSubFormDestroyed;
    end;
    FOperationsExplorerForm.Show;
  finally
    FUILock.ReleaseWriteLock;
  end;
end;

class procedure TUserInterface.ShowPendingOperations;
begin
  FUILock.AcquireWriteLock;
  try
    if not Assigned(FPendingOperationForm) then begin
      FPendingOperationForm := TFRMPendingOperations.Create(FMainForm);
      FPendingOperationForm.CloseAction:= caFree;
      FPendingOperationForm.OnDestroyed:= Self.OnSubFormDestroyed;
    end;
    FPendingOperationForm.Show;
  finally
    FUILock.ReleaseWriteLock;
  end;
end;

class procedure TUserInterface.ShowMessagesForm;
begin
  FUILock.AcquireWriteLock;
  try
    if not Assigned(FMessagesForm) then begin
       FMessagesForm := TFRMMessages.Create(FMainForm);
       FMessagesForm.CloseAction:= caHide;
    end;
    FMessagesForm.MarkRead;
    FMessagesForm.Show;
  finally
    FUILock.ReleaseWriteLock;
  end;
end;

class procedure TUserInterface.ShowNodesForm;
begin
  FUILock.AcquireWriteLock;
  try
    if not Assigned(FNodesForm) then begin
       FNodesForm := TFRMNodes.Create(FMainForm);
       FNodesForm.CloseAction:= caFree;
       FNodesForm.OnDestroyed:= Self.OnSubFormDestroyed;
    end;
    FNodesForm.Show;
  finally
    FUILock.ReleaseWriteLock;
  end;
end;

class procedure TUserInterface.ShowLogsForm;
begin
  if not TSettings.ShowLogs then Exit;
  FUILock.AcquireWriteLock;
  try
    if not Assigned(FLogsForm) then begin
       FLogsForm := TFRMLogs.Create(FMainForm);
       FLogsForm.CloseAction:= caHide;
    end;
    FLogsForm.Show;
  finally
    FUILock.ReleaseWriteLock;
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
Var errorMessage, LBankDiagnostic : String;
  LNode : TNode;
  LProgress : TProgressInfo;
begin
  LNode := TNode.Node;
  if Not LNode.IsReady(errorMessage) then begin
    // Recreate download text here; bank/server failures keep their own reason.
    if LNode.Bank.IsReady(LBankDiagnostic) then
      if LNode.NetServer.Active then
        if TNetData.NetData.IsGettingNewBlockChainFromClient(LProgress) then
          errorMessage := FormatProgressInfo(LProgress);
    Raise Exception.Create('You cannot do this operation now:'+#10+#10+errorMessage);
  end;
end;

{%endregion}

{%region Auxillary methods}

class function TUserInterface.GetEnabled : boolean;
begin
  Result := Assigned(FMainForm) and FMainForm.Enabled;
end;

class procedure TUserInterface.SetEnabled(ABool: boolean);
begin
  if Assigned(FMainForm) then
    FMainForm.Enabled:=ABool;
end;

class procedure TUserInterface.SetState(AState : TUserInterfaceState);
begin
  // Connectivity, progress and deferred loader events cannot clear a latched
  // storage error; only constructing a fresh node after restart can do that.
  if Assigned(FNode) and FNode.HasStorageFailure then AState := uisError;
  if AState = FState then
    exit;
  FState := AState;
  if FState = uisLoaded then begin
    // Loading listeners may publish a snapshot synchronously. Finish this
    // lifecycle dispatch before those events can choose the connected state.
    FDispatchingLoaded := True;
    try
      NotifyLoadedEvent(nil);
    finally
      FDispatchingLoaded := False;
    end;
  end;
  NotifyStateChanged(nil);
  if FState = uisLoaded then RefreshNodeState;
end;

class procedure TUserInterface.NotifyLoadedEvent(Sender: TObject);
begin
  TUserInterface.FLoaded.Invoke(Sender);
end;

class procedure TUserInterface.NotifyLoadingEvent(Sender: TObject; const AProgress : TProgressInfo);
begin
  if FStopping then Exit;
  TUserInterface.FLoading.Invoke(Sender, AProgress);
end;

class procedure TUserInterface.NotifyStateChanged(Sender: TObject);
begin
  TUserInterface.FStateChanged.Invoke(Sender);
end;

class procedure TUserInterface.NotifyAccountsChangedEvent(Sender: TObject);
begin
  if FStopping then Exit;
  TUserInterface.FAccountsChanged.Invoke(Sender);
end;

class procedure TUserInterface.NotifyBlocksChangedEvent(Sender: TObject);
begin
  if FStopping then Exit;
  RefreshNodeState;
  TUserInterface.FBlocksChanged.Invoke(Sender);
end;

class procedure TUserInterface.NotifyNodeMessageEventEvent(NetConnection: TNetConnection; MessageData: String);
begin
  if FStopping then Exit;
  TUserInterface.FNodeMessageEvent.Invoke(NetConnection, MessageData);
end;

class procedure TUserInterface.NotifyReceivedHelloMessageEvent(Sender: TObject);
begin
  if FStopping then Exit;
  RefreshNodeState;
  TUserInterface.FReceivedHelloMessage.Invoke(Sender);;
end;

class procedure TUserInterface.NotifyNetStatisticsChangedEvent(Sender: TObject);
begin
  if FStopping then Exit;
  RefreshNodeState;
  TUserInterface.FNetStatisticsChanged.Invoke(Sender);
end;

class procedure TUserInterface.NotifyNetConnectionsUpdatedEvent(Sender: TObject);
begin
  if FStopping then Exit;
  RefreshNodeState;
  TUserInterface.FNetConnectionsUpdated.Invoke(Sender);
end;

class procedure TUserInterface.NotifyNetNodeServersUpdatedEvent(Sender: TObject);
begin
  if FStopping then Exit;
  RefreshNodeState;
  TUserInterface.NetNodeServersUpdated.Invoke(Sender);
end;

class procedure TUserInterface.NotifyNetBlackListUpdatedEvent(Sender: TObject);
begin
  if FStopping then Exit;
  TUserInterface.FNetBlackListUpdated.Invoke(Sender);
end;

class procedure TUserInterface.NotifyMiningServerNewBlockFoundEvent(Sender: TObject);
begin
  if FStopping then Exit;
  TUserInterface.FMiningServerNewBlockFound.Invoke(Sender);
end;

{%endregion}

initialization
// TODO - any startup code needed here?
finalization
// TODO - final cleanup here, show a modal dialog?
end.
