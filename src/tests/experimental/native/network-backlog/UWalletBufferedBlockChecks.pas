unit UWalletBufferedBlockChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Receives an immutable historic batch through the actual network processor.
  Only the notification after each completed bank commit is gated for observation. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunBufferedBlockBacklogProbe: Integer;
function RunOutgoingBufferedBlockProbe: Integer;
function RunIncomingBufferedBlockProbe: Integer;
function RunOutgoingShutdownProbe: Integer;
function RunIncomingShutdownProbe: Integer;
function RunIncomingSettingsProbe: Integer;
function RunConnectedMinerProbe: Integer;
{$IFDEF LARGE_BACKLOG_PROBE}
function RunLargeBufferedBlockProbe: Integer;
{$ENDIF}

implementation

uses Windows, Classes, SysUtils, SyncObjs, Forms, StdCtrls, Controls, ExtCtrls, Types, Generics.Collections,
  blcksock, UNode, UNetProtocol, UThread, UBlockChain, UAccounts, UPCDataTypes,
  UFileStorage, UCrypto, UConst, UWallet, UUserInterface, UWalletStartup,
  UFRMMainForm, UCTRLWallet, UCTRLSyncronization, UAppParams, UCommon,
  USettings, UMemory, UVisualGrid, UCommon.UI, UTime, UPoolMining, UJSONFunctions, ULog
  {$IFDEF LARGE_BACKLOG_PROBE}, UDataSources, UCommon.Data, UOrderedList, UBaseTypes{$ENDIF};

const
  CHistoricCount = 5;
  CHistoricTimes: array[0..CHistoricCount-1] of Cardinal =
    (1470934813,1470935312,1470935390,1470935433,1470935652);

type
  TWalletAccess = class(TCTRLWallet)
  public
    property AccountsGrid;
    property OperationsGrid;
  end;

  TMainAccess = class(TFRMMainForm)
  public
    property WalletControl;
  end;

  TBufferedResponseConnection = class(TNetConnection)
  public
    procedure ProcessReceivedBatch(AData: TStream);
    function CallerProcessingTerminated: Boolean;
  end;

  TThreadAccess = class(TThread)
  public
    property Terminated;
  end;

  TCommitObservation = class(TComponent)
  private
    FBankNotify: TPCBankNotify;
    FReleasing: LongInt;
    FOfflineAtCommit: Boolean;
    procedure OnCommitted(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    CommitCount, ConnectionDestroyed: LongInt;
    FirstOffRequestID: Cardinal;
    ProcessingThread: TThread;
    CheckDeferredOperationsAtCommit: Boolean;
    FirstCommitTick: UInt64;
    Committed, ContinueBatch: TEvent;
    constructor Create(AOfflineAtCommit: Boolean);
    destructor Destroy; override;
    procedure ReleaseAll;
  end;

  TDelayedGateRelease = class(TPCThread)
  private
    FObservation: TCommitObservation;
    FWaitForCommit: Boolean;
  protected
    procedure BCExecute; override;
  public
    constructor Create(AObservation: TCommitObservation; AWaitForCommit: Boolean = False);
  end;

  TReceivedBatchTask = class(TPCThread)
  private
    FConnection: TBufferedResponseConnection;
    FData: TStream;
    FStarted: Boolean;
  protected
    procedure BCExecute; override;
  public
    ErrorMessage: String;
    Completed: TEvent;
    constructor Create(AConnection: TBufferedResponseConnection; AData: TStream);
    destructor Destroy; override;
    procedure StartOwned;
  end;

  TBufferedBlockChecks = class
  private
    FMain: TFRMMainForm;
    FDialog: TForm;
    FConnection: TNetConnection;
    FOutgoingOwner: Boolean;
    FIncomingOwner: Boolean;
    FShutdownAfterFirst: Boolean;
    FExerciseSettings: Boolean;
    FSynchronizationLifecycle: Boolean;
    FConnectedMiner, FMonitorMinerNotifications: Boolean;
    FMiner: TPoolMinerClient;
    FMinerPort: Word;
    FMinerObserver: TNodeNotifyEvents;
    FExternalRelease: TDelayedGateRelease;
    FBlockNotificationTick, FOperationNotificationTick, FMaximumPump: UInt64;
    FBlockWhileWriterHeld, FOperationsWhileWriterHeld: Boolean;
    FExpectedMiningKey: TAccountKey;
    FMiningWalletKeyCount: Integer;
    FObservation: TCommitObservation;
    FListener, FAccepted: TTCPBlockSocket;
    FTask: TReceivedBatchTask;
    FData: TResourceStream;
    FLog: TStringList;
    FLogPath, FError, FDataDirectory: String;
    FPreviousException: TExceptionEvent;
    FDiagnosticLog: TLog;
    procedure Log(const AText: String);
    procedure OnException(Sender: TObject; E: Exception);
    procedure Pump;
    procedure WaitCommit(AExpected: Integer);
    procedure CheckOnlineCallerRequest;
    procedure ReflectBufferedResponse;
    procedure CheckOwnerWhileGated;
    procedure OpenIncomingConnection;
    procedure CheckIncomingNetworkOff;
    procedure ConfigureMiningFixture;
    procedure ConnectControlledMiner;
    procedure QueueMinerOperationsNotification;
    procedure AwaitMinerBlockJob(AExpectedBlock: Cardinal);
    procedure OnMinerBlockNotification(Sender: TObject);
    procedure OnMinerOperationsNotification(Sender: TObject);
    function ActualWriterHeld: Boolean;
    procedure CheckExplicitShutdown;
    procedure CheckCommittedDisplay(AExpected: Integer);
    procedure CheckNativeSynchronizationCards(const ACheckpoint: String);
    procedure CheckSynchronizationResponses;
    procedure CheckNoNewRequests;
    procedure CheckReopenedStorage(AExpected: Integer);
    function Wallet: TWalletAccess;
  public
    constructor Create(const ALogPath: String; AOutgoingOwner, AIncomingOwner,
      AShutdownAfterFirst, AExerciseSettings, AConnectedMiner: Boolean);
    destructor Destroy; override;
    procedure Run;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure TBufferedResponseConnection.ProcessReceivedBatch(AData: TStream);
var LHeader: TNetHeaderData;
begin
  LHeader := Default(TNetHeaderData);
  LHeader.header_type := ntp_response;
  LHeader.operation := CT_NetOp_GetBlocks;
  AData.Position := 0;
  // Inherited decoding, PoW/signature validation, atomic commits and cleanup.
  DoProcess_GetBlocks_Response(LHeader,AData);
end;

function TBufferedResponseConnection.CallerProcessingTerminated: Boolean;
begin
  Result := BlockProcessingTerminated;
end;

constructor TCommitObservation.Create(AOfflineAtCommit: Boolean);
begin
  inherited Create(nil);
  FOfflineAtCommit := AOfflineAtCommit;
  Committed := TEvent.Create(nil,True,False,'');
  ContinueBatch := TEvent.Create(nil,False,False,'');
  FBankNotify := TPCBankNotify.Create(Self);
  FBankNotify.Bank := TNode.Node.Bank;
  FBankNotify.OnNewBlock := OnCommitted;
end;

destructor TCommitObservation.Destroy;
begin
  ReleaseAll;
  FBankNotify.Free;
  ContinueBatch.Free;
  Committed.Free;
  inherited;
end;

procedure TCommitObservation.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TNetConnection) then
    InterlockedExchange(ConnectionDestroyed,1);
end;

procedure TCommitObservation.ReleaseAll;
begin
  InterlockedExchange(FReleasing,1);
  ContinueBatch.SetEvent;
end;

procedure TCommitObservation.OnCommitted(Sender: TObject);
var LCount: Integer; LDeadline: UInt64; LOperations: TOperationsHashTree; LError: String;
begin
  ProcessingThread := TThread.CurrentThread;
  LCount := InterlockedIncrement(CommitCount);
  if LCount = 1 then begin
    FirstCommitTick := GetTickCount64;
    if CheckDeferredOperationsAtCommit then begin
      // Catch-up intentionally rejects even empty pending admission. Its real
      // operations notification was queued before the received batch started.
      LOperations := TOperationsHashTree.Create;
      try
        Require(TNode.Node.AddOperations(nil,LOperations,nil,LError) = -1,
          'Pending admission must remain unavailable during received-block processing');
        Require(LError = 'Pending operations are unavailable during blockchain synchronization',
          'Pending admission must report the deferred-state reason');
      finally LOperations.Free; end;
    end;
    // Exercise the real switch from its actual bank-commit callback. The
    // outgoing destroy worker remains free to run; this fixture holds no net lock.
    if FOfflineAtCommit then TNetData.NetData.NetConnectionsActive := False;
    FirstOffRequestID := TNetData.NetData.NewRequestId;
  end;
  if InterlockedCompareExchange(FReleasing,0,0) <> 0 then Exit;
  Committed.SetEvent;
  LDeadline := GetTickCount64+10000;
  while ContinueBatch.WaitFor(10) <> wrSignaled do begin
    if InterlockedCompareExchange(FReleasing,0,0) <> 0 then Exit;
    if GetTickCount64 >= LDeadline then
      raise Exception.Create('The main-thread committed-snapshot check did not release its gate');
  end;
  Committed.ResetEvent;
end;

constructor TDelayedGateRelease.Create(AObservation: TCommitObservation; AWaitForCommit: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FObservation := AObservation;
  FWaitForCommit := AWaitForCommit;
end;

procedure TDelayedGateRelease.BCExecute;
begin
  // Bound even the old synchronous listener join without forcing any thread.
  if FWaitForCommit then
    while FObservation.Committed.WaitFor(10) <> wrSignaled do
      if Terminated then Exit;
  if Terminated then Exit;
  Sleep(1200);
  FObservation.ReleaseAll;
end;

constructor TReceivedBatchTask.Create(AConnection: TBufferedResponseConnection; AData: TStream);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FConnection := AConnection;
  FData := AData;
  Completed := TEvent.Create(nil,True,False,'');
end;

destructor TReceivedBatchTask.Destroy;
begin
  Terminate;
  if not FStarted then StartOwned;
  WaitFor;
  Completed.Free;
  inherited;
end;

procedure TReceivedBatchTask.StartOwned;
begin
  FStarted := True;
  Start;
end;

procedure TReceivedBatchTask.BCExecute;
begin
  try
    try
      if not Terminated then FConnection.ProcessReceivedBatch(FData);
    except
      on E: Exception do ErrorMessage := E.ClassName+': '+E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

constructor TBufferedBlockChecks.Create(const ALogPath: String; AOutgoingOwner, AIncomingOwner,
  AShutdownAfterFirst, AExerciseSettings, AConnectedMiner: Boolean);
begin
  inherited Create;
  FLogPath := ALogPath;
  FOutgoingOwner := AOutgoingOwner;
  FIncomingOwner := AIncomingOwner;
  FShutdownAfterFirst := AShutdownAfterFirst;
  FExerciseSettings := AExerciseSettings;
  FConnectedMiner := AConnectedMiner;
  FSynchronizationLifecycle := SameText(GetEnvironmentVariable('PASCALCOIN_BACKLOG_CASE'),'synchronization');
  FLog := TStringList.Create;
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
end;

destructor TBufferedBlockChecks.Destroy;
begin
  // Join the actual processor before its connection, data, node or forms die.
  FMonitorMinerNotifications := False;
  if Assigned(FObservation) then FObservation.ReleaseAll;
  if Assigned(FExternalRelease) then begin
    FExternalRelease.Terminate;
    FExternalRelease.WaitFor;
    FExternalRelease.Free;
  end;
  FMinerObserver.Free;
  FMiner.Free;
  FTask.Free;
  if not (FOutgoingOwner or FIncomingOwner) then FConnection.Free;
  FAccepted.Free;
  FListener.Free;
  FData.Free;
  if Assigned(FMain) then TUserInterface.ExitApplication;
  FObservation.Free;
  FDiagnosticLog.Free;
  Application.OnException := FPreviousException;
  FLog.Free;
  inherited;
end;

procedure TBufferedBlockChecks.Log(const AText: String);
begin
  FLog.Add(UIntToStr(GetTickCount64)+' '+AText);
  FLog.SaveToFile(FLogPath);
end;

procedure TBufferedBlockChecks.OnException(Sender: TObject; E: Exception);
begin
  if FError = '' then FError := E.ClassName+': '+E.Message;
end;

procedure TBufferedBlockChecks.Pump;
var LStarted, LElapsed: UInt64;
begin
  LStarted := GetTickCount64;
  Application.ProcessMessages;
  CheckSynchronize(0);
  LElapsed := GetTickCount64-LStarted;
  if FMonitorMinerNotifications then begin
    if LElapsed > FMaximumPump then FMaximumPump := LElapsed;
    if LElapsed >= 400 then begin
      Log('MINER actual UI dispatch duration='+UIntToStr(LElapsed)+'ms');
      Require(False,'An actual connected-miner node callback blocked the UI for >=400ms');
    end;
  end;
  Require(FError = '',FError);
end;

function TBufferedBlockChecks.Wallet: TWalletAccess;
begin
  Result := TWalletAccess(TMainAccess(FMain).WalletControl);
end;

procedure TBufferedBlockChecks.WaitCommit(AExpected: Integer);
var LDeadline: UInt64;
begin
  LDeadline := GetTickCount64+10000;
  repeat
    Pump;
    if Assigned(FTask) then Require(FTask.ErrorMessage = '',FTask.ErrorMessage);
    if (InterlockedCompareExchange(FObservation.CommitCount,0,0) = AExpected) and
      (FObservation.Committed.WaitFor(0) = wrSignaled) then Exit;
    if Assigned(FTask) and (FTask.Completed.WaitFor(0) = wrSignaled) then Break;
    if InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) <> 0 then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  raise Exception.CreateFmt('Buffered processor did not commit historic block %d',[AExpected-1]);
end;

procedure TBufferedBlockChecks.CheckCommittedDisplay(AExpected: Integer);
var LDeadline: UInt64; S, LDisplayed: TNodeSynchronizationStatus; LFooter, LTime: String;
begin
  LDeadline := GetTickCount64+3000;
  repeat
    Pump;
    S := TUserInterface.Node.GetSynchronizationStatus;
    LDisplayed := FMain.SyncControl.DisplayedSynchronizationStatus;
    if (LDisplayed.LocalBlockCount = Cardinal(AExpected)) and
      (LDisplayed.State = nssSynchronizing) and (LDisplayed.NetworkActive=FSynchronizationLifecycle) and
      (Pos('Blocks: '+IntToStr(AExpected)+' |',FMain.sbStatusBar.Panels[2].Text) = 1) and
      Wallet.AccountsGrid.RefreshSuspended and Wallet.OperationsGrid.RefreshSuspended and
      Wallet.AccountsGrid.LoadingVisible and Wallet.OperationsGrid.LoadingVisible then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require(TUserInterface.Node.Bank.BlocksCount = Cardinal(AExpected),'Actual bank count differs from completed commits');
  Require((S.State = nssSynchronizing) and (S.ActiveTransfers > 0) and
    (S.NetworkActive=FSynchronizationLifecycle),'Received work did not retain the expected node synchronization state');
  Require((S.LocalBlockCount = Cardinal(AExpected)) and
    (S.LastBlockTimestamp = CHistoricTimes[AExpected-1]),
    'The node cached metadata must describe the completed bank commit');
  Require((LDisplayed.State = nssSynchronizing) and
    (LDisplayed.LocalBlockCount = S.LocalBlockCount) and
    (LDisplayed.LastBlockTimestamp = S.LastBlockTimestamp),
    'The actual sync view did not render the current cached committed record');
  Require(FMain.SyncControl.lblTotalBlocksValue.Caption =
    IntToStr(AExpected)+' (0..'+IntToStr(AExpected-1)+')',
    'The basic sync block label disagrees with its displayed record');
  LFooter := FMain.sbStatusBar.Panels[2].Text;
  LTime := FormatDateTime('dd/mm/yyyy hh:nn:ss',
    UnivDateTime2LocalDateTime(UnixToUnivDateTime(CHistoricTimes[AExpected-1])));
  Require((LFooter = FMain.SyncControl.ChainStatusText) and
    (Pos('Blocks: '+IntToStr(AExpected)+' |',LFooter) = 1) and
    (Pos(LTime,LFooter) > 0) and (FSynchronizationLifecycle or (Pos('Network off',LFooter) > 0)),
    'Footer count/time/network policy must use the same committed record as sync labels');
  Require(Wallet.AccountsGrid.RefreshSuspended and Wallet.OperationsGrid.RefreshSuspended and
    Wallet.AccountsGrid.LoadingVisible and Wallet.OperationsGrid.LoadingVisible,
    Format('Both actual main grids must remain suspended/covered until backlog drains: paused=%s/%s cards=%s/%s',
      [BoolToStr(Wallet.AccountsGrid.RefreshSuspended,True),BoolToStr(Wallet.OperationsGrid.RefreshSuspended,True),
       BoolToStr(Wallet.AccountsGrid.LoadingVisible,True),BoolToStr(Wallet.OperationsGrid.LoadingVisible,True)]));
  CheckNativeSynchronizationCards('committed-'+IntToStr(AExpected));
  Require(GetActiveWindow = FDialog.Handle,'A committed-block update raised the main form over its modeless dialog');
  Log('COMMITTED '+IntToStr(AExpected)+' synchronizing networkActive='+BoolToStr(S.NetworkActive,True)+'; '+LFooter);
end;

procedure TBufferedBlockChecks.CheckNativeSynchronizationCards(const ACheckpoint: String);
var LGrids: array[0..1] of TVisualGrid; LPanel: TPanel; LCircle: TLoadingCircle;
  LTitle, LSubtitle: TLabel; LBounds, LClient: TRect; I: Integer;
begin
  LGrids[0] := Wallet.AccountsGrid;
  LGrids[1] := Wallet.OperationsGrid;
  for I := Low(LGrids) to High(LGrids) do begin
    LPanel := LGrids[I].FindComponent('LoadingOverlay') as TPanel;
    LCircle := LGrids[I].FindComponent('LoadingCircle') as TLoadingCircle;
    LTitle := LGrids[I].FindComponent('LoadingStatus') as TLabel;
    LSubtitle := LGrids[I].FindComponent('LoadingSubtitle') as TLabel;
    Require(LGrids[I].RefreshSuspended and LGrids[I].LoadingVisible,
      ACheckpoint+': real GET_BLOCKS lost its grid suspension/card');
    Require(LPanel.Visible and LPanel.HandleAllocated and IsWindowVisible(LPanel.Handle),
      ACheckpoint+': manual flag did not produce a visible native overlay');
    Require(LCircle.Active and LCircle.Visible and LCircle.HandleAllocated and IsWindowVisible(LCircle.Handle),
      ACheckpoint+': the native synchronization spinner is hidden');
    Require(LTitle.Visible and (LTitle.Caption='Please Wait') and LSubtitle.Visible and
      (LSubtitle.Caption='Synchronizing'),ACheckpoint+': synchronization text was replaced by ordinary loading');
    GetWindowRect(LPanel.Handle,LBounds);
    GetClientRect(LPanel.Parent.Handle,LClient);
    MapWindowPoints(LPanel.Parent.Handle,0,LClient,2);
    Require((LBounds.Right>LBounds.Left) and (LBounds.Bottom>LBounds.Top) and
      (LBounds.Left>=LClient.Left) and (LBounds.Top>=LClient.Top) and
      (LBounds.Right<=LClient.Right) and (LBounds.Bottom<=LClient.Bottom),
      ACheckpoint+': native synchronization overlay lies outside its grid client');
  end;
end;

procedure TBufferedBlockChecks.CheckSynchronizationResponses;
var LFirst, LSecond: TMemoryStream; LBlock: TPCOperationsComp;
  LSplit: Int64; LCount: Cardinal; LDeadline: UInt64; LStatus: TNodeSynchronizationStatus;
  LBag: TDisposables; LError: String; I: Integer;

  procedure AwaitBatch;
  var LUntil: UInt64;
  begin
    LUntil := GetTickCount64+5000;
    repeat
      Pump;
      Require(FTask.ErrorMessage='',FTask.ErrorMessage);
      if FTask.Completed.WaitFor(0)=wrSignaled then Break;
      Sleep(1);
    until GetTickCount64>=LUntil;
    Require(FTask.Completed.WaitFor(0)=wrSignaled,'The actual GET_BLOCKS response did not finish');
    FreeAndNil(FTask);
  end;

begin
  // Split the immutable resource at a decoder-confirmed block boundary. Both
  // responses retain the exact historic block bytes and production validation.
  LFirst := LBag.AddObject(TMemoryStream.Create) as TMemoryStream;
  LSecond := LBag.AddObject(TMemoryStream.Create) as TMemoryStream;
  LBlock := LBag.AddObject(TPCOperationsComp.Create(TUserInterface.Node.Bank)) as TPCOperationsComp;
  FData.Position := SizeOf(Cardinal);
  for I := 0 to 1 do
    Require(LBlock.LoadBlockFromStream(FData,LError),LError);
  LSplit := FData.Position;
  LCount := 2;
  LFirst.WriteBuffer(LCount,SizeOf(LCount));
  FData.Position := SizeOf(Cardinal);
  LFirst.CopyFrom(FData,LSplit-FData.Position);
  LCount := CHistoricCount-2;
  LSecond.WriteBuffer(LCount,SizeOf(LCount));
  FData.Position := LSplit;
  LSecond.CopyFrom(FData,FData.Size-FData.Position);
  LBag.RemoveObject(LBlock);
  FreeAndNil(LBlock);
  // Select this actual connected loopback peer through the same public node
  // API used by Send_GetBlocks; never synthesize a synchronization state.
  TUserInterface.Node.SetSynchronizationTarget(FConnection,CHistoricCount);
  try
    FTask := TReceivedBatchTask.Create(TBufferedResponseConnection(FConnection),LFirst);
    FTask.StartOwned;
    for I := 1 to 2 do begin
      WaitCommit(I);
      CheckCommittedDisplay(I);
      FObservation.ContinueBatch.SetEvent;
    end;
    AwaitBatch;
    LStatus := TUserInterface.Node.GetSynchronizationStatus;
    Require((LStatus.State=nssSynchronizing) and LStatus.NetworkActive and
      (LStatus.ActiveTransfers=0) and (LStatus.LocalBlockCount=2) and
      (LStatus.TargetBlockCount=CHistoricCount),'The selected real peer target did not retain synchronization between responses');
    LDeadline := GetTickCount64+6200;
    repeat
      Pump;
      CheckNativeSynchronizationCards('real-response-gap');
      Sleep(20);
    until GetTickCount64>=LDeadline;
    Log('PASS actual GET_BLOCKS gap: local2/target5, zero active transfers, both native Synchronizing cards retained for6.2s');
    FTask := TReceivedBatchTask.Create(TBufferedResponseConnection(FConnection),LSecond);
    FTask.StartOwned;
    for I := 3 to CHistoricCount do begin
      WaitCommit(I);
      CheckCommittedDisplay(I);
      FObservation.ContinueBatch.SetEvent;
    end;
    AwaitBatch;
    LDeadline := GetTickCount64+5000;
    repeat
      Pump;
      LStatus := TUserInterface.Node.GetSynchronizationStatus;
      if (LStatus.State=nssIdle) and not Wallet.AccountsGrid.RefreshSuspended and
        not Wallet.OperationsGrid.RefreshSuspended and not Wallet.AccountsGrid.IsUpdating and
        not Wallet.OperationsGrid.IsUpdating and not Wallet.OperationsGrid.RefreshThrottle.Waiting then Break;
      Sleep(1);
    until GetTickCount64>=LDeadline;
    Require((LStatus.State=nssIdle) and (LStatus.LocalBlockCount=CHistoricCount),
      'The actual final GET_BLOCKS response did not complete the selected target');
    Require(not Wallet.AccountsGrid.RefreshSuspended and not Wallet.OperationsGrid.RefreshSuspended and
      not (Wallet.AccountsGrid.FindComponent('LoadingOverlay') as TPanel).Visible and
      not (Wallet.OperationsGrid.FindComponent('LoadingOverlay') as TPanel).Visible,
      'Completed real GET_BLOCKS synchronization retained a suspended grid or overlay');
    Log('PASS actual GET_BLOCKS target completion: local5/target5, idle, both grids resumed and native cards removed');
  finally
    FObservation.ReleaseAll;
    FreeAndNil(FTask);
  end;
  FreeAndNil(FConnection);
  FreeAndNil(FAccepted);
  FreeAndNil(FListener);
  TUserInterface.ExitApplication;
  FMain := nil;
  FDialog := nil;
  CheckReopenedStorage(CHistoricCount);
end;

procedure TBufferedBlockChecks.CheckNoNewRequests;
var LRequestID, LBefore, LAfter: Cardinal; LHeader: TNetHeaderData;
  LSend, LReceive: TMemoryStream; LDeadline: UInt64;
begin
  Require(not TBufferedResponseConnection(FConnection).CallerProcessingTerminated,
    'An external/main caller must not be treated as a terminated processing thread');
  LAfter := TNetData.NetData.NewRequestId;
  Require(LAfter = FObservation.FirstOffRequestID+1,'A new network request was allocated after network-off while the backlog drained');
  LBefore := TNetData.NetData.NewRequestId;
  Require(not FConnection.ConnectTo('127.0.0.1',FListener.GetLocalSinPort),
    'Network-off must reject a new connection attempt');
  LRequestID := 0;
  Require(not FConnection.Send_GetBlocks(CHistoricCount,5,LRequestID),
    'Network-off must reject a new block request');
  LSend := TMemoryStream.Create;
  LReceive := TMemoryStream.Create;
  try
    LHeader := Default(TNetHeaderData);
    LDeadline := GetTickCount64;
    Require(not FConnection.DoSendAndWaitForResponse(CT_NetOp_GetBlocks,0,
      LSend,LReceive,1000,LHeader),'Network-off must reject a nested request/wait');
    Require(GetTickCount64-LDeadline < 500,'A denied request must not wait for a network timeout');
  finally
    LReceive.Free;
    LSend.Free;
  end;
  LAfter := TNetData.NetData.NewRequestId;
  Require(LAfter = LBefore+1,'Denied requests must not allocate fresh request IDs');
  Log('PASS already received blocks finished; new connect/GetBlocks/nested wait requests rejected');
end;

procedure TBufferedBlockChecks.CheckOnlineCallerRequest;
var LSend, LReceive, LWire: TMemoryStream; LHeader: TNetHeaderData;
  LStart, LEnd, LRequestID: Cardinal; LDeadline: UInt64; LBytesBefore, LReadAt: Int64;
  LPacket: RawByteString; LNeedMore, LFound: Boolean;
begin
  {$IFNDEF FPC}
  Require(TThread.CurrentThread.ExternalThread,
    'The native main-thread regression must exercise the RTL external-thread path');
  {$ENDIF}
  Require(TNetData.NetData.NetConnectionsActive and
    not TBufferedResponseConnection(FConnection).CallerProcessingTerminated,
    'An online external/main caller must not raise or report processing cancellation');
  LDeadline := GetTickCount64+1000;
  repeat Pump; Sleep(1); until (FConnection.Client.BytesSent > 0) or (GetTickCount64 >= LDeadline);
  Require(FConnection.Client.BytesSent > 0,'The initial real loopback HELLO was not sent');
  LBytesBefore := FConnection.Client.BytesSent;
  LSend := TMemoryStream.Create;
  LReceive := TMemoryStream.Create;
  LWire := TMemoryStream.Create;
  try
    LStart := 0;
    LEnd := CHistoricCount-1;
    LSend.WriteBuffer(LStart,SizeOf(LStart));
    LSend.WriteBuffer(LEnd,SizeOf(LEnd));
    LRequestID := TNetData.NetData.NewRequestId;
    LHeader := Default(TNetHeaderData);
    // The raw owned peer intentionally has no response yet. A zero-timeout
    // public request still exercises actual online admission and transmission.
    Require(not FConnection.DoSendAndWaitForResponse(CT_NetOp_GetBlocks,
      LRequestID,LSend,LReceive,0,LHeader),'The raw peer unexpectedly replied');
    LFound := False;
    LDeadline := GetTickCount64+1000;
    repeat
      Pump;
      if FAccepted.CanRead(10) then begin
        LPacket := FAccepted.RecvPacket(10);
        if Length(LPacket) > 0 then begin
          LReadAt := LWire.Position;
          LWire.Position := LWire.Size;
          LWire.WriteBuffer(LPacket[1],Length(LPacket));
          LWire.Position := LReadAt;
          // Decode the real loopback bytes with the production framing parser.
          // This skips the initial HELLO and identifies our exact request ID.
          while TNetData.ExtractHeaderInfo(LWire,LHeader,LReceive,LNeedMore) do
            if (LHeader.header_type = ntp_request) and
              (LHeader.operation = CT_NetOp_GetBlocks) and
              (LHeader.request_id = LRequestID) then begin
              Require(LReceive.Size = 2*SizeOf(Cardinal),'The real GetBlocks payload has the wrong size');
              LReceive.ReadBuffer(LStart,SizeOf(LStart));
              LReceive.ReadBuffer(LEnd,SizeOf(LEnd));
              Require((LStart = 0) and (LEnd = CHistoricCount-1),
                'The real loopback request did not carry our block range');
              LFound := True;
              Break;
            end;
        end;
      end;
    until LFound or (GetTickCount64 >= LDeadline);
    Require(LFound,'The owned peer did not receive the exact online GetBlocks frame/request ID');
  finally
    LWire.Free;
    LReceive.Free;
    LSend.Free;
  end;
  LDeadline := GetTickCount64+1000;
  repeat Pump; Sleep(1); until (FConnection.Client.BytesSent > LBytesBefore) or (GetTickCount64 >= LDeadline);
  Require(FConnection.Client.BytesSent > LBytesBefore,
    'The actual online public request was not transmitted to its owned loopback peer');
  Log('PASS main/external caller transmitted a real loopback block request while online');
end;

procedure TBufferedBlockChecks.ReflectBufferedResponse;
var LWire, LPayload: TMemoryStream; LHeader: TNetHeaderData;
  LPacket: RawByteString; LDeadline: UInt64; LAt: Int64;
  LNeedMore, LFound: Boolean;
const CResponseID = 7654321;
begin
  LWire := TMemoryStream.Create;
  LPayload := TMemoryStream.Create;
  try
    FData.Position := 0;
    // Serialize with the production sender, then reflect the exact frame over
    // the real socket. Only the client's owned thread decodes/processes it.
    FConnection.DoSend(ntp_response,CT_NetOp_GetBlocks,0,CResponseID,FData);
    LFound := False;
    LDeadline := GetTickCount64+3000;
    repeat
      Pump;
      if FAccepted.CanRead(10) then begin
        LPacket := FAccepted.RecvPacket(10);
        if Length(LPacket) > 0 then begin
          LAt := LWire.Position;
          LWire.Position := LWire.Size;
          LWire.WriteBuffer(LPacket[1],Length(LPacket));
          LWire.Position := LAt;
          repeat
            LAt := LWire.Position;
            if not TNetData.ExtractHeaderInfo(LWire,LHeader,LPayload,LNeedMore) then Break;
            if (LHeader.header_type = ntp_response) and
              (LHeader.operation = CT_NetOp_GetBlocks) and
              (LHeader.request_id = CResponseID) then begin
              Require(LPayload.Size = FData.Size,'The real response changed the historic payload');
              if FConnectedMiner then QueueMinerOperationsNotification;
              Require(FAccepted.SendBuffer(PByte(LWire.Memory)+LAt,
                Integer(LWire.Position-LAt)) = Integer(LWire.Position-LAt),
                'The raw loopback peer could not send the complete real response frame');
              if FConnectedMiner then
                // Do not pump the queued notification until the real bank
                // callback holds its writer; no producer waits for the UI.
                Require(FObservation.Committed.WaitFor(3000) = wrSignaled,
                  'The connected-miner processor did not reach its committed writer gate');
              LFound := True;
              Break;
            end;
          until False;
        end;
      end;
    until LFound or (GetTickCount64 >= LDeadline);
    Require(LFound,'The actual client did not serialize the historic response');
  finally
    LPayload.Free;
    LWire.Free;
  end;
  Log('PASS reflected exact production GetBlocks response into actual socket-owned processor');
end;

procedure TBufferedBlockChecks.CheckOwnerWhileGated;
var LDeadline: UInt64; LTerminated: Boolean;
begin
  Require(Assigned(FObservation.ProcessingThread),'The actual processor thread was not captured');
  Require(FObservation.ProcessingThread is TNetClientThread,
    'The received batch must execute on the actual client-owned thread');
  LDeadline := GetTickCount64+1200;
  repeat Pump; Sleep(1); until GetTickCount64 >= LDeadline;
  // The processor is alive inside OnCommitted throughout this read. A real
  // destroyer may be joining it, but cannot finish freeing it before release.
  LTerminated := TThreadAccess(FObservation.ProcessingThread).Terminated;
  Log('OWNER gate after1200ms terminated='+BoolToStr(LTerminated,True)+
    ' destroyed='+IntToStr(InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0))+
    ' committed='+IntToStr(InterlockedCompareExchange(FObservation.CommitCount,0,0)));
  Require(not LTerminated,
    'Real outgoing connection cleanup terminated its owned processor during a received batch');
  Require(InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) = 0,
    'The real outgoing connection was destroyed before its buffered work drained');
end;

procedure TBufferedBlockChecks.OpenIncomingConnection;
var LPort: Word; LDeadline: UInt64; LConnections: TList<TNetConnection>;
  LConnection: TNetConnection;
begin
  // Ask the OS for a currently unused loopback port; never use the user's port.
  FListener := TTCPBlockSocket.Create;
  FListener.CreateSocket;
  FListener.Bind('127.0.0.1','0');
  Require(FListener.LastError = 0,'Cannot reserve an owned listener port');
  LPort := FListener.GetLocalSinPort;
  FListener.CloseSocket;
  // Use the real settings event so the UI's desired port and the actual
  // serialized listener owner agree before a later toolbar request.
  TSettings.InternetServerPort := LPort;
  TSettings.Save;
  LDeadline := GetTickCount64+3000;
  repeat
    Pump;
    if TUserInterface.Node.NetServer.Active and
      (TUserInterface.Node.NetServer.Port = LPort) then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require(TUserInterface.Node.NetServer.Active and
    (TUserInterface.Node.NetServer.Port = LPort),'Owned listener settings were not applied');
  FAccepted := TTCPBlockSocket.Create;
  FAccepted.Connect('127.0.0.1',IntToStr(LPort));
  Require(FAccepted.LastError = 0,'Cannot connect the raw peer to the actual listener');
  LDeadline := GetTickCount64+3000;
  repeat
    Pump;
    LConnections := TNetData.NetData.NetConnections.LockList;
    try
      for LConnection in LConnections do
        if LConnection is TNetServerClient then begin
          FConnection := LConnection;
          FConnection.FreeNotification(FObservation);
          Break;
        end;
    finally
      TNetData.NetData.NetConnections.UnlockList;
    end;
    if Assigned(FConnection) then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require(Assigned(FConnection),'The actual listener did not create its server connection');
end;

procedure TBufferedBlockChecks.ConfigureMiningFixture;
var LDeadline: UInt64; LOperations: TPCOperationsComp; LApplied: Boolean;
  LPortSocket: TTCPBlockSocket;
begin
  // This secret belongs only to the fresh disposable wallet. Ordinary settings
  // must reuse its eligible reward key even in NewEachTime mode; no mined-block
  // callback is being simulated by these port/name changes.
  TWallet.GenerateNewKey('Disposable buffered-settings mining key',CT_Default_EC_OpenSSL_NID);
  TSettings.MinerPrivateKeyType := mpk_NewEachTime;
  LPortSocket := TTCPBlockSocket.Create;
  try
    LPortSocket.CreateSocket;
    LPortSocket.Bind('127.0.0.1','0');
    Require(LPortSocket.LastError = 0,'Cannot reserve the owned mining-listener port');
    FMinerPort := LPortSocket.GetLocalSinPort;
  finally LPortSocket.Free; end;
  TSettings.JsonRpcMinerServerPort := FMinerPort;
  TSettings.JsonRpcMinerServerActive := True;
  TSettings.MinerName := 'Initial buffered mining payload';
  FExpectedMiningKey := TWallet.MiningKey;
  FMiningWalletKeyCount := TWallet.Keys.Count;
  TSettings.Save;
  LApplied := False;
  LDeadline := GetTickCount64+3000;
  repeat
    Pump;
    if TUserInterface.PoolMiningServer.Active and
      TUserInterface.Node.TryLockMempoolRead(0,LOperations) then begin
      try
        LApplied := TAccountComp.EqualAccountKeys(LOperations.AccountKey,FExpectedMiningKey);
      finally
        TUserInterface.Node.UnlockMempoolRead;
      end;
    end;
    if LApplied then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require(LApplied,'The actual configured mining reward key was not applied');
  Require(TWallet.Keys.Count = FMiningWalletKeyCount,
    'Ordinary mining configuration unexpectedly generated another wallet key');
  Log('MINING configured ON with one disposable eligible key; ordinary NewEachTime rotation forbidden');
end;

procedure TBufferedBlockChecks.ConnectControlledMiner;
var LDeadline: UInt64; LJSON: TPCJSONObject; LMethod: String; LFound: Boolean;
begin
  FMiner := TPoolMinerClient.Create(nil);
  FMiner.RemoteHost := '127.0.0.1';
  FMiner.RemotePort := FMinerPort;
  Require(FMiner.Connect,'The controlled miner could not connect to its actual pool listener');
  LJSON := nil;
  try
    LFound := False;
    LDeadline := GetTickCount64+3000;
    repeat
      Pump;
      LMethod := '';
      // Match the production miner loop: Assign appends array items, so every
      // received message needs its own object rather than previous job data.
      FreeAndNil(LJSON);
      LJSON := TPCJSONObject.Create;
      if FMiner.DoProcessBuffer(nil,10,False,LMethod,LJSON) and
        (LJSON.AsString('method','') = CT_PoolMining_Method_MINER_NOTIFY) then begin
        FMiner.DoProcessJSONObject(LJSON,LMethod);
        LFound := Length(FMiner.MinerValuesForWork.part1) > 0;
      end;
      if LFound then Break;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    Require(LFound and (TUserInterface.PoolMiningServer.ClientsCount = 1),
      'The actual connected miner did not consume its production initial job');
  finally LJSON.Free; end;
  FMinerObserver := TNodeNotifyEvents.Create(nil);
  FMinerObserver.OnBlocksChanged := OnMinerBlockNotification;
  FMinerObserver.OnOperationsChanged := OnMinerOperationsNotification;
  FMinerObserver.Node := TUserInterface.Node;
  Log('MINER actual connected client consumed production miner-notify job block='+
    IntToStr(FMiner.MinerValuesForWork.block));
end;

procedure TBufferedBlockChecks.QueueMinerOperationsNotification;
var LOperations: TOperationsHashTree; LError: String;
begin
  // Queue through the public producer while pending state is coherent. The UI
  // must not dispatch this notification until the received block holds WRITE.
  Require(TUserInterface.Node.PendingOperationsAvailable,
    'The controlled operations producer must run before catch-up starts');
  LOperations := TOperationsHashTree.Create;
  try
    Require(TUserInterface.Node.AddOperations(nil,LOperations,nil,LError) = 0,LError);
  finally LOperations.Free; end;
  Log('MINER queued actual operations notification before received-block processing');
end;

function TBufferedBlockChecks.ActualWriterHeld: Boolean;
var LUnexpectedWrite: Boolean;
begin
  LUnexpectedWrite := TUserInterface.Node.TryLockNode(0);
  if LUnexpectedWrite then TUserInterface.Node.UnlockNode;
  Result := not LUnexpectedWrite;
end;

procedure TBufferedBlockChecks.AwaitMinerBlockJob(AExpectedBlock: Cardinal);
var LDeadline: UInt64; LJSON: TPCJSONObject; LMethod: String; LFound: Boolean;
begin
  LJSON := nil;
  try
    LFound := False;
    Log('MINER awaiting latest block='+IntToStr(AExpectedBlock)+
      ' previous='+IntToStr(FMiner.MinerValuesForWork.block)+
      ' connected='+BoolToStr(FMiner.Connected,True)+
      ' clients='+IntToStr(TUserInterface.PoolMiningServer.ClientsCount)+
      ' active='+BoolToStr(TUserInterface.PoolMiningServer.Active,True));
    LDeadline := GetTickCount64+3000;
    repeat
      Pump;
      LMethod := '';
      FreeAndNil(LJSON);
      LJSON := TPCJSONObject.Create;
      if FMiner.DoProcessBuffer(nil,10,False,LMethod,LJSON) and
        (LJSON.AsString('method','') = CT_PoolMining_Method_MINER_NOTIFY) then begin
        FMiner.DoProcessJSONObject(LJSON,LMethod);
        Log('MINER received production job block='+IntToStr(FMiner.MinerValuesForWork.block));
        LFound := (FMiner.MinerValuesForWork.block = AExpectedBlock) and
          (Length(FMiner.MinerValuesForWork.part1) > 0);
      end;
      if LFound then Break;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    // Real committed-block events clear the old job cache. An unchanged empty
    // operations notification alone is not expected to bypass the 10/30s rules.
    if not LFound then Log('MINER timeout last='+IntToStr(FMiner.MinerValuesForWork.block)+
      ' connected='+BoolToStr(FMiner.Connected,True)+
      ' clients='+IntToStr(TUserInterface.PoolMiningServer.ClientsCount)+
      ' active='+BoolToStr(TUserInterface.PoolMiningServer.Active,True));
    Require(LFound,'The actual miner did not receive a job for the latest committed block');
    Log('MINER consumed latest production job block='+IntToStr(AExpectedBlock));
  finally LJSON.Free; end;
end;

procedure TBufferedBlockChecks.OnMinerBlockNotification(Sender: TObject);
begin
  if not FMonitorMinerNotifications then Exit;
  if not Assigned(FObservation) then Exit;
  if FObservation.FirstCommitTick = 0 then Exit;
  if FBlockNotificationTick = 0 then begin
    FBlockNotificationTick := GetTickCount64;
    FBlockWhileWriterHeld := ActualWriterHeld;
  end;
end;

procedure TBufferedBlockChecks.OnMinerOperationsNotification(Sender: TObject);
begin
  if not FMonitorMinerNotifications then Exit;
  if not Assigned(FObservation) then Exit;
  if FObservation.FirstCommitTick = 0 then Exit;
  if FOperationNotificationTick = 0 then begin
    FOperationNotificationTick := GetTickCount64;
    FOperationsWhileWriterHeld := ActualWriterHeld;
  end;
end;

procedure TBufferedBlockChecks.CheckIncomingNetworkOff;
var LRelease: TDelayedGateRelease; LStarted, LElapsed, LDeadline, LSettingsElapsed: UInt64;
  LCount: Integer; LPort: Word; LPortSocket: TTCPBlockSocket;
  LOperations: TPCOperationsComp;
  LConfigurationApplied: Boolean;
const CChangedMinerName = 'Buffered ownership latest payload';
begin
  WaitCommit(1);
  Require(TNetData.NetData.NetConnectionsActive,'Incoming fixture must begin online');
  if FConnectedMiner then begin
    LDeadline := GetTickCount64+400;
    repeat
      Pump;
      if (FBlockNotificationTick <> 0) and (FOperationNotificationTick <> 0) then Break;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    Require((FBlockNotificationTick <> 0) and (FOperationNotificationTick <> 0),
      'Actual block/operations notifications did not reach the UI within400ms');
    Require(FBlockWhileWriterHeld and FOperationsWhileWriterHeld,
      'The connected-miner notification check did not execute while the actual bank writer was held');
    Require((FBlockNotificationTick-FObservation.FirstCommitTick < 400) and
      (FOperationNotificationTick-FObservation.FirstCommitTick < 400),
      'Actual node notifications waited for a connected miner/core lock');
    Log('MINER actual block/operations UI latency='+
      UIntToStr(FBlockNotificationTick-FObservation.FirstCommitTick)+'/'+
      UIntToStr(FOperationNotificationTick-FObservation.FirstCommitTick)+
      'ms maxUIpump='+UIntToStr(FMaximumPump)+'ms while bankWRITE held');
  end;
  Require(TUserInterface.Node.GetSynchronizationStatus.State = nssSynchronizing,
    'The real incoming processor did not enter its physical backlog scope');
  LRelease := TDelayedGateRelease.Create(FObservation);
  LSettingsElapsed := 0;
  LPort := TUserInterface.Node.NetServer.Port;
  try
    LRelease.Start;
    LStarted := GetTickCount64;
    // Invoke the actual published toolbar handler on the real UI thread.
    // The independent gate actor bounds a blocking old listener shutdown.
    FMain.tbtnConnectivityClick(FMain.tbtnConnectivity);
    if FExerciseSettings then begin
      FMain.tbtnConnectivityClick(FMain.tbtnConnectivity);
      FMain.tbtnConnectivityClick(FMain.tbtnConnectivity);
    end;
    LElapsed := GetTickCount64-LStarted;
    if FExerciseSettings then begin
      LPortSocket := TTCPBlockSocket.Create;
      try
        LPortSocket.CreateSocket;
        LPortSocket.Bind('127.0.0.1','0');
        Require(LPortSocket.LastError = 0,'Cannot reserve the next owned settings port');
        LPort := LPortSocket.GetLocalSinPort;
      finally LPortSocket.Free; end;
      LStarted := GetTickCount64;
      TSettings.InternetServerPort := LPort;
      TSettings.MinerName := CChangedMinerName;
      TSettings.Save;
      LSettingsElapsed := GetTickCount64-LStarted;
    end;
    LRelease.WaitFor;
  finally
    LRelease.Free;
  end;
  LDeadline := GetTickCount64+5000;
  repeat
    Pump;
    if (InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) <> 0) and
      not TUserInterface.Node.IsUpdatingBlockChain and
      (TUserInterface.Node.NetServer.Port = LPort) then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  LCount := InterlockedCompareExchange(FObservation.CommitCount,0,0);
  Log('INCOMING actual toolbar OFF duration='+UIntToStr(LElapsed)+
    'ms settings='+UIntToStr(LSettingsElapsed)+'ms committed='+IntToStr(LCount)+' destroyed='+
    IntToStr(InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0)));
  Require(not TNetData.NetData.NetConnectionsActive,'Actual toolbar did not switch networking off');
  Require(LCount = CHistoricCount,'Listener shutdown discarded an already received block tail');
  Require(LElapsed < 400,'Actual UI network-off waited for buffered listener work');
  Require(LSettingsElapsed < 400,'Actual port settings event waited for a buffered core writer');
  Require(TUserInterface.Node.NetServer.Port = LPort,'The latest queued port did not win');
  if FExerciseSettings then begin
    // A port is applied before the same worker finishes the copied mining
    // snapshot. Await its observable result, not a mid-task field transition.
    LDeadline := GetTickCount64+3000;
    repeat
      Pump;
      LConfigurationApplied := False;
      if TUserInterface.Node.TryLockMempoolRead(0,LOperations) then begin
        try
          LConfigurationApplied :=
            (TEncoding.ANSI.GetString(LOperations.BlockPayload) = CChangedMinerName) and
            TAccountComp.EqualAccountKeys(LOperations.AccountKey,FExpectedMiningKey);
        finally
          TUserInterface.Node.UnlockMempoolRead;
        end;
      end;
      LConfigurationApplied := LConfigurationApplied and
        TUserInterface.PoolMiningServer.Active and
        TAccountComp.EqualAccountKeys(TUserInterface.PoolMiningServer.MinerAccountKey,FExpectedMiningKey) and
        (TEncoding.ANSI.GetString(TUserInterface.PoolMiningServer.MinerPayload) = CChangedMinerName);
      if LConfigurationApplied then Break;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    Require(LConfigurationApplied,'The latest copied node/pool mining snapshot was not applied');
    LOperations := TUserInterface.Node.LockMempoolRead;
    try
      Require(TEncoding.ANSI.GetString(LOperations.BlockPayload) = CChangedMinerName,
        'The owned settings worker did not apply the copied latest miner payload');
      Require(TAccountComp.EqualAccountKeys(LOperations.AccountKey,FExpectedMiningKey),
        'The copied mining reward key was not retained through the settings updates');
    finally
      TUserInterface.Node.UnlockMempoolRead;
    end;
    Require(TSettings.JsonRpcMinerServerActive and TUserInterface.PoolMiningServer.Active,
      'The settings test must exercise the actual configured-on mining branch');
    Require(TWallet.Keys.Count = FMiningWalletKeyCount,
      'Ordinary port/name settings rotated or added a NewEachTime wallet key');
  end;
  Require(InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) <> 0,
    'The drained incoming connection was not retired');
  FConnection := nil;
  if FExerciseSettings then begin
    FMain.tbtnConnectivityClick(FMain.tbtnConnectivity);
    LDeadline := GetTickCount64+3000;
    repeat
      Pump;
      if TNetData.NetData.NetConnectionsActive and TUserInterface.Node.NetServer.Active and
        (FMain.sbStatusBar.Panels[0].Text = 'Active (port: '+IntToStr(LPort)+')') then Break;
      Sleep(1);
    until GetTickCount64 >= LDeadline;
    Require(FMain.sbStatusBar.Panels[0].Text = 'Active (port: '+IntToStr(LPort)+')',
      'The port-only completion did not publish the actual listener/footer state');
    FMain.tbtnConnectivityClick(FMain.tbtnConnectivity);
    LDeadline := GetTickCount64+3000;
    repeat Pump; Sleep(1); until
      (not TNetData.NetData.NetConnectionsActive and not TUserInterface.Node.NetServer.Active) or
      (GetTickCount64 >= LDeadline);
    Require(not TNetData.NetData.NetConnectionsActive,'Final independent OFF was not applied');
    Log('PASS rapid OFF/ON/OFF latest port and subsequent real listener/footer ON/OFF');
  end;
  if FConnectedMiner then AwaitMinerBlockJob(CHistoricCount);
end;

procedure TBufferedBlockChecks.CheckExplicitShutdown;
var LRelease: TDelayedGateRelease; LStarted, LElapsed: UInt64;
begin
  WaitCommit(1);
  if not FIncomingOwner then begin
    CheckCommittedDisplay(1);
    CheckOwnerWhileGated;
  end;
  LRelease := TDelayedGateRelease.Create(FObservation);
  try
    LRelease.Start;
    if FIncomingOwner then begin
      FMain.tbtnConnectivityClick(FMain.tbtnConnectivity);
      LStarted := GetTickCount64+500;
      repeat Pump; Sleep(1); until not TNetData.NetData.NetConnectionsActive or
        (GetTickCount64 >= LStarted);
      Require(not TNetData.NetData.NetConnectionsActive,
        'The real pending listener-stop task did not publish OFF before shutdown');
    end;
    LStarted := GetTickCount64;
    // Exercise the production application shutdown, not a direct thread kill.
    // Its shutdown latch is published before listener/client joins begin.
    TUserInterface.ExitApplication;
    LElapsed := GetTickCount64-LStarted;
    FMain := nil;
    FDialog := nil;
    FConnection := nil;
    LRelease.WaitFor;
  finally
    LRelease.Free;
  end;
  Log('SHUTDOWN actual ExitApplication duration='+UIntToStr(LElapsed)+
    'ms committed='+IntToStr(InterlockedCompareExchange(FObservation.CommitCount,0,0))+
    ' destroyed='+IntToStr(InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0)));
  Require(LElapsed < 5000,'Explicit shutdown did not join the buffered processor promptly');
  Require(InterlockedCompareExchange(FObservation.CommitCount,0,0) = 1,
    'Explicit application shutdown must cancel between complete atomic blocks');
  Require(InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) <> 0,
    'Explicit shutdown returned before its transport-owned connection was retired');
  Log('PASS actual ExitApplication joined in '+UIntToStr(LElapsed)+
    'ms and stopped between blocks; one complete block persisted');
  CheckReopenedStorage(1);
end;

procedure TBufferedBlockChecks.CheckReopenedStorage(AExpected: Integer);
var LBank: TPCBank;
begin
  LBank := TPCBank.Create(nil);
  try
    LBank.StorageClass := TFileStorage;
    // Match startup's complete restore entrypoint, not RestoreBank's safebox-
    // selection phase alone, which does not populate the cached bank header.
    LBank.DiskRestoreFromOperations(AExpected-1,nil);
    Require(LBank.BlocksCount = Cardinal(AExpected),'Reopened storage lost or added a received block');
    Require(LBank.LastOperationBlock.timestamp = CHistoricTimes[AExpected-1],
      'Reopened storage has the wrong committed tip');
  finally LBank.Free; end;
  Log('PASS persisted/reopened '+IntToStr(AExpected)+' fully validated historical blocks');
end;

procedure TBufferedBlockChecks.Run;
var LID: TGUID; LOptions: TWalletStartupOptions; LParams: TAppParams; LDeadline: UInt64;
  I: Integer; LBlock: TPCOperationsComp; LCount: Cardinal; LError: String;
begin
  Require(not TNode.NodeExists,'The backlog fixture needs its own fresh controller');
  if FConnectedMiner then begin
    // Use an independent instance of the existing logger so real settings
    // changes cannot disable the diagnostic sink. Its lifetime spans startup
    // through joined application shutdown; the normal logger is unchanged.
    FDiagnosticLog := TLog.Create(nil);
    FDiagnosticLog.SaveTypes := CT_TLogTypes_ALL;
    FDiagnosticLog.FileName := FLogPath+'.core.log';
  end;
  CreateGUID(LID);
  FDataDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))+'BufferedBlocks-'+GUIDToString(LID);
  Require(not DirectoryExists(FDataDirectory),'Refusing existing runtime data');
  Require(ForceDirectories(FDataDirectory),'Cannot create owned runtime data');
  LParams := TAppParams.Create(nil);
  try
    LParams.FileName := IncludeTrailingPathDelimiter(FDataDirectory)+'AppParams.prm';
    LParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].SetAsString('127.0.0.1:1');
    LParams.ParamByName[CT_PARAM_PeerCache].SetAsString('');
    LParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(0);
    LParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(False);
    LParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(False);
    LParams.ParamByName[CT_PARAM_MinerPrivateKeyType].SetAsInteger(Ord(mpk_Random));
    LParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(False);
  finally LParams.Free; end;
  LOptions := Default(TWalletStartupOptions);
  LOptions.DataDirectory := FDataDirectory;
  LOptions.NoStartupDialogs := True;
  LOptions.Offline := False;
  TUserInterface.ConfigureStartup(LOptions);
  Application.CreateForm(TFRMMainForm,FMain);
  TUserInterface.StartApplication(FMain);
  LDeadline := GetTickCount64+10000;
  repeat
    Pump;
    Require(TUserInterface.State <> uisError,'Fresh loopback-only startup failed');
    if (Wallet <> nil) and (TUserInterface.State <> uisLoading) then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require(Wallet <> nil,'Actual main wallet did not load');
  LDeadline := GetTickCount64+3000;
  repeat Pump; Sleep(1); until TNetData.NetData.NetConnectionsActive or (GetTickCount64 >= LDeadline);
  Require(TNetData.NetData.NetConnectionsActive,'The owned online startup task did not complete');
  TUserInterface.Node.NetServer.Active := False;
  TNetData.NetData.MinServersConnected := 0;
  TUserInterface.Node.BroadcastData := False;
  FData := TResourceStream.Create(HInstance,'HISTORIC_BUFFERED_BLOCKS',RT_RCDATA);
  FData.ReadBuffer(LCount,SizeOf(LCount));
  Require(LCount = CHistoricCount,'Wrong historical batch resource');
  LBlock := TPCOperationsComp.Create(TUserInterface.Node.Bank);
  try
    for I := 0 to CHistoricCount-1 do begin
      Require(LBlock.LoadBlockFromStream(FData,LError),LError);
      Require(LBlock.OperationBlock.block = Cardinal(I),'Historical fixture height mismatch');
      TWallet.Keys.AddPublicKey('Historic public miner '+IntToStr(I),LBlock.OperationBlock.account_key);
    end;
  finally LBlock.Free; end;
  FData.Position := 0;
  if FExerciseSettings then ConfigureMiningFixture;
  if FConnectedMiner then ConnectControlledMiner;
  FObservation := TCommitObservation.Create(not FIncomingOwner and not FSynchronizationLifecycle);
  FObservation.CheckDeferredOperationsAtCommit := FConnectedMiner;
  if FIncomingOwner then OpenIncomingConnection
  else begin
    FListener := TTCPBlockSocket.Create;
    FListener.CreateSocket;
    FListener.Bind('127.0.0.1','0');
    FListener.Listen;
    Require(FListener.LastError = 0,'Cannot open the owned raw loopback listener');
    if FOutgoingOwner then FConnection := TNetClient.Create(nil)
    else FConnection := TBufferedResponseConnection.Create(nil);
    if FOutgoingOwner then FConnection.FreeNotification(FObservation);
    Require(FConnection.ConnectTo('127.0.0.1',FListener.GetLocalSinPort),'The real core client did not connect to loopback');
    Require(FListener.CanRead(1000),'The owned raw loopback server did not receive the connection');
    FAccepted := TTCPBlockSocket.Create;
    FAccepted.Socket := FListener.Accept;
    Require(FAccepted.LastError = 0,'Cannot accept the owned loopback peer');
  end;
  // The separate direct-entry case covers synchronous external callers. Here
  // the client's real receiver owns that socket and may be reading already.
  if not (FOutgoingOwner or FIncomingOwner or FSynchronizationLifecycle) then CheckOnlineCallerRequest;
  FMain.Show;
  FDialog := TForm.CreateNew(FMain);
  FDialog.Caption := 'Owned modeless focus regression';
  FDialog.Show;
  SetActiveWindow(FDialog.Handle);
  if FSynchronizationLifecycle then begin
    CheckSynchronizationResponses;
    Exit;
  end;
  if FConnectedMiner then begin
    FExternalRelease := TDelayedGateRelease.Create(FObservation,True);
    FExternalRelease.Start;
    FMonitorMinerNotifications := True;
  end;
  if FOutgoingOwner or FIncomingOwner then ReflectBufferedResponse
  else begin
    FTask := TReceivedBatchTask.Create(TBufferedResponseConnection(FConnection),FData);
    FTask.StartOwned;
  end;
  if FShutdownAfterFirst then begin
    CheckExplicitShutdown;
    Exit;
  end;
  if FIncomingOwner then CheckIncomingNetworkOff
  else
    for I := 1 to CHistoricCount do begin
      WaitCommit(I);
      CheckCommittedDisplay(I);
      if FOutgoingOwner and (I = 1) then CheckOwnerWhileGated;
      FObservation.ContinueBatch.SetEvent;
    end;
  LDeadline := GetTickCount64+5000;
  if FOutgoingOwner then begin
    repeat Pump; Sleep(1); until
      (InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) <> 0) or
      (GetTickCount64 >= LDeadline);
    Require(InterlockedCompareExchange(FObservation.ConnectionDestroyed,0,0) <> 0,
      'The actual destroy worker did not retire the drained outgoing connection');
    FConnection := nil;
  end else if not FIncomingOwner then begin
    repeat Pump; Sleep(1); until (FTask.Completed.WaitFor(0)=wrSignaled) or (GetTickCount64>=LDeadline);
    Require(FTask.Completed.WaitFor(0)=wrSignaled,'Received batch did not finish');
    Require(FTask.ErrorMessage = '',FTask.ErrorMessage);
  end;
  LDeadline := GetTickCount64+3000;
  repeat
    Pump;
    if (TUserInterface.Node.GetSynchronizationStatus.State = nssOffline) and
      not Wallet.AccountsGrid.RefreshSuspended and not Wallet.OperationsGrid.RefreshSuspended then Break;
    Sleep(1);
  until GetTickCount64>=LDeadline;
  Require(TUserInterface.Node.GetSynchronizationStatus.State = nssOffline,'Network-off must become available once the received batch drains');
  Require(not Wallet.AccountsGrid.LoadingVisible and not Wallet.OperationsGrid.LoadingVisible,
    'The actual main cards must clear after the received batch drains');
  if not (FOutgoingOwner or FIncomingOwner) then CheckNoNewRequests
  else if FOutgoingOwner then Require(TNetData.NetData.NewRequestId = FObservation.FirstOffRequestID+1,
    'The outgoing drain allocated a new request after network-off');
  FreeAndNil(FTask);
  if not (FOutgoingOwner or FIncomingOwner) then FreeAndNil(FConnection);
  FreeAndNil(FAccepted);
  FreeAndNil(FListener);
  FMonitorMinerNotifications := False;
  if Assigned(FMinerObserver) then begin
    FMinerObserver.OnBlocksChanged := nil;
    FMinerObserver.OnOperationsChanged := nil;
    FMinerObserver.Node := nil;
    FreeAndNil(FMinerObserver);
  end;
  // Keep the controlled miner connected through actual application shutdown.
  // Its accepted server worker must retire before the pool's jobs/template die.
  TUserInterface.ExitApplication;
  FMain := nil;
  FDialog := nil;
  CheckReopenedStorage(CHistoricCount);
end;

{$IFDEF LARGE_BACKLOG_PROBE}
{$I UWalletLargeBufferedBlockChecks.inc}
{$ENDIF}

function RunChecks(AOutgoingOwner, AIncomingOwner, AShutdownAfterFirst,
  AExerciseSettings, AConnectedMiner: Boolean): Integer;
var LChecks: TBufferedBlockChecks; LInput: THandle; LName, LInputName: array[0..255] of Char; LRequired: DWORD;
begin
  Result := 1;
  if ParamCount <> 1 then Exit;
  LInput := OpenInputDesktop(0,False,DESKTOP_READOBJECTS);
  if LInput = 0 then RaiseLastOSError;
  try
    if not GetUserObjectInformation(GetThreadDesktop(GetCurrentThreadId),UOI_NAME,@LName[0],SizeOf(LName),LRequired) then RaiseLastOSError;
    if not GetUserObjectInformation(LInput,UOI_NAME,@LInputName[0],SizeOf(LInputName),LRequired) then RaiseLastOSError;
    Require(not SameText(StrPas(PChar(@LName[0])),StrPas(PChar(@LInputName[0]))),
      'Use the existing owned private-desktop broker');
  finally CloseDesktop(LInput); end;
  Require(not FileExists(ParamStr(1)),'Refusing an existing test log');
  Application.Initialize;
  LChecks := TBufferedBlockChecks.Create(ExpandFileName(ParamStr(1)),AOutgoingOwner,
    AIncomingOwner,AShutdownAfterFirst,AExerciseSettings,AConnectedMiner);
  try
    try LChecks.Run; Result := 0;
    except on E: Exception do LChecks.Log('FAIL '+E.ClassName+': '+E.Message); end;
  finally LChecks.Free; end;
end;

function RunBufferedBlockBacklogProbe: Integer;
var LScenario: String;
begin
  // The public runner compiles once, then gives each scenario a fresh process
  // and controller through the existing one-logfile private-desktop broker.
  LScenario := LowerCase(GetEnvironmentVariable('PASCALCOIN_BACKLOG_CASE'));
  if (LScenario = '') or (LScenario = 'direct') or (LScenario = 'synchronization') then
    Result := RunChecks(False,False,False,False,False)
  else if LScenario = 'outgoing' then Result := RunOutgoingBufferedBlockProbe
  else if LScenario = 'outgoing-shutdown' then Result := RunOutgoingShutdownProbe
  else if LScenario = 'incoming' then Result := RunIncomingBufferedBlockProbe
  else if LScenario = 'incoming-settings' then Result := RunIncomingSettingsProbe
  else if LScenario = 'incoming-shutdown' then Result := RunIncomingShutdownProbe
  else if LScenario = 'connected-miner' then Result := RunConnectedMinerProbe
  else Result := 64;
end;

function RunOutgoingBufferedBlockProbe: Integer;
begin
  Result := RunChecks(True,False,False,False,False);
end;

function RunIncomingBufferedBlockProbe: Integer;
begin
  Result := RunChecks(False,True,False,False,False);
end;

function RunOutgoingShutdownProbe: Integer;
begin
  Result := RunChecks(True,False,True,False,False);
end;

function RunIncomingShutdownProbe: Integer;
begin
  Result := RunChecks(False,True,True,False,False);
end;

function RunIncomingSettingsProbe: Integer;
begin
  Result := RunChecks(False,True,False,True,False);
end;

function RunConnectedMinerProbe: Integer;
begin
  Result := RunChecks(False,True,False,True,True);
end;

end.
