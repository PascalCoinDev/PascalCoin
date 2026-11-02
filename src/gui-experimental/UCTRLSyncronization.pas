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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

{$I ..\config.inc}

uses
  Classes, SysUtils, Types, {$IFDEF FPC}FileUtil,{$ENDIF} Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, UCommon, UCommon.UI, UNetProtocol, UBaseTypes, UAccounts, UNode;

type

  TSyncNodeSnapshot = record
    BlockCount: Cardinal;
    PeerCount: Integer;
    TotalBlocks, TotalAccounts, BlockAge, PendingOperations, BlockTarget: String;
    TimeAverage, TimeAverageAux, ProtocolVersion, ChainStatus: String;
    TimeAverageColour: TColor;
  end;
  TSyncNodeSnapshotReader = procedure(ANode: TNode; out ASnapshot: TSyncNodeSnapshot) of object;

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
    procedure FormShow(Sender: TObject);
    procedure lblReceivedMessagesClick(Sender:TObject);
  private
    FMinedBlocksCount: Integer;
    FShowSplash : boolean;
    FMessagesUnreadCount : Integer;
    FLoadingText : TLabel;
    FCancelLoading : TButton;
    FSnapshotWorker : TThread;
    FSnapshotNotify : TThreadNotify;
    FSnapshotPending, FSnapshotStopping : Boolean;
    FHasSecondarySnapshot: Boolean;
    FSecondaryBlockCount: Cardinal;
    FSecondaryRevision: Int64;
    FWasSynchronizing: Boolean;
    FLastActiveConnections : Integer;
    FChainStatusText : String;
    FHasNodeSnapshot : Boolean;
    FSnapshotBlockCount : Cardinal;
    FNodeSnapshot : TSyncNodeSnapshot;
    FSnapshotChanged : TNotifyManyEvent;
    FDisplayedSynchronizationStatus: TNodeSynchronizationStatus;
    procedure StartNodeSnapshot;
    procedure OnSnapshotCompleted(Sender: TObject);
    procedure ApplyNodeSnapshot(const ASnapshot: TSyncNodeSnapshot);
    procedure ApplySynchronizationStatus;
    procedure OnSynchronizationChanged(Sender: TObject);
    procedure UpdateMiningStatus;
    {$IFNDEF FPC}
    function Scale96ToForm(AValue: Integer): Integer;
    {$ENDIF}
    procedure LayoutLoadingControls(Sender : TObject);
    procedure OnCancelLoading(Sender : TObject);
    procedure OnLoading(Sender: TObject; const AProgress : TProgressInfo);
    procedure OnLoaded(Sender: TObject);
    procedure OnStateChanged(Sender: TObject);
    procedure SetMinedBlocksCount(const Value: Integer);
    procedure SetShowSplash(ABool : boolean);
    procedure SetMessagesNotificationText(const text : String);
    function GetMessagesNotificationText : String;
    procedure SetStatusText(AColour: TColor; AText: String);

    procedure UpdateNodeStatus;
    procedure UpdateBlockChainState;
    procedure OnAppStarted(Sender: TObject);
    procedure OnBlocksChanged(Sender: TObject);
    procedure OnNetworkStatusChanged(Sender: TObject);
    procedure OnMiningStatusChanged(Sender: TObject);
    procedure OnNodeMessageEvent(NetConnection: TNetConnection; MessageData: String);
  protected
    procedure ActivateFirstTime; override;
    // Invoked by the owned worker. Overrides must not touch UI controls.
    procedure ReadSecondarySnapshot(ANode: TNode; out ASnapshot: TSyncNodeSnapshot); virtual;
  public
    class function CurrentNodeStatusText: String; static;
    // Hosted forms may become visible through their parent panel without a
    // native activation. Refresh immediately, retaining the last snapshot if
    // the node is busy; its owned task notifies completion without UI polling.
    procedure RefreshNodeState;
    procedure ClearMessagesNotification;
    property ChainStatusText : String read FChainStatusText;
    property HasNodeSnapshot : Boolean read FHasNodeSnapshot;
    property SnapshotBlockCount : Cardinal read FSnapshotBlockCount;
    property SnapshotChanged : TNotifyManyEvent read FSnapshotChanged;
    property DisplayedSynchronizationStatus: TNodeSynchronizationStatus read FDisplayedSynchronizationStatus;
    property MinedBlocksCount : Integer read FMinedBlocksCount write SetMinedBlocksCount;
    property ShowSplash : boolean read FShowSplash write SetShowSplash;
    property MessagesNotificationText : String read GetMessagesNotificationText write SetMessagesNotificationText;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  UTime, UConst, UBlockChain, UProgressDisplay,
  UUserInterface, UThread, ULog;

type
  TSyncNodeSnapshotThread = class(TPCThread)
  private
    FNode: TNode;
    FNotify: TThreadNotify;
    FOnCompleted: TNotifyEvent;
    FReadSnapshot: TSyncNodeSnapshotReader;
    FCancelRequested: Boolean;
  protected
    procedure BCExecute; override;
  public
    Snapshot: TSyncNodeSnapshot;
    Succeeded: Boolean;
    Revision: Int64;
    ErrorMessage: String;
    constructor Create(ANode: TNode; ANotify: TThreadNotify;
      AOnCompleted: TNotifyEvent; AReadSnapshot: TSyncNodeSnapshotReader; ARevision: Int64);
    function WasCanceled: Boolean;
    procedure Cancel;
  end;

{$IFNDEF FPC}
type
  TImageAccess = class(TImage);

function TCTRLSyncronization.Scale96ToForm(AValue: Integer): Integer;
begin
  Result := ScaleValue(AValue);
end;
{$ENDIF}

procedure ReadNodeSnapshot(ANode: TNode; out ASnapshot: TSyncNodeSnapshot);
var
  LStatus: TNodeSynchronizationStatus;
  LDeviation, LAverage: Double;
  LAverages: array[0..5] of Double;
  LWindows: array[0..5] of Cardinal;
  I: Integer;
begin
  LStatus := ANode.GetSynchronizationStatus;
  ASnapshot.BlockCount := LStatus.LocalBlockCount;
  ASnapshot.PeerCount := LStatus.PeerCount;
  if ASnapshot.BlockCount > 0 then begin
    ASnapshot.TotalBlocks := IntToStr(ASnapshot.BlockCount) + ' (0..' +
      IntToStr(ASnapshot.BlockCount - 1) + ')';
    ASnapshot.BlockAge := UnixTimeToLocalElapsedTime(LStatus.LastBlockTimestamp);
  end else begin
    ASnapshot.TotalBlocks := '(none)';
    ASnapshot.BlockAge := '(none)';
  end;
  ASnapshot.TotalAccounts := UIntToStr(UInt64(LStatus.LocalBlockCount) * CT_AccountsPerBlock);
  ASnapshot.PendingOperations := IntToStr(LStatus.PendingOperationsCount);
  ASnapshot.BlockTarget := IntToHex(LStatus.CompactTarget, 8);
  ASnapshot.ProtocolVersion := Format('%d (%d)',
    [LStatus.ProtocolVersion, CT_BlockChain_Protocol_Available]);
  LWindows[0] := CT_CalcNewTargetBlocksAverage;
  LWindows[1] := CT_CalcNewTargetBlocksAverage * 2;
  LWindows[2] := (CT_CalcNewTargetBlocksAverage * 3) div 2;
  LWindows[3] := (CT_CalcNewTargetBlocksAverage div 4) * 3;
  LWindows[4] := CT_CalcNewTargetBlocksAverage div 2;
  LWindows[5] := CT_CalcNewTargetBlocksAverage div 4;
  for I := 0 to High(LWindows) do begin
    if TThread.CheckTerminated then Abort;
    LAverages[I] := ANode.Bank.GetActualTargetSecondsAverage(LWindows[I]);
  end;
  if TThread.CheckTerminated then Abort;
  LAverage := LAverages[0];
  LDeviation := (CT_NewLineSecondsAvg - LAverage) / CT_NewLineSecondsAvg;
  ASnapshot.TimeAverage := 'Last ' + IntToStr(CT_CalcNewTargetBlocksAverage) + ': ' +
    FormatFloat('0.0', LAverage) + ' sec. (Optimal ' + IntToStr(CT_NewLineSecondsAvg) +
    's) Deviation ' + FormatFloat('0.00%', LDeviation * 100);
  if LAverage >= CT_NewLineSecondsAvg then ASnapshot.TimeAverageColour := clNavy
  else ASnapshot.TimeAverageColour := clOlive;
  ASnapshot.TimeAverageAux := Format('Last %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec. - %d: %s sec.', [
    CT_CalcNewTargetBlocksAverage * 2, FormatFloat('0.0', LAverages[1]),
    (CT_CalcNewTargetBlocksAverage * 3) div 2, FormatFloat('0.0', LAverages[2]),
    (CT_CalcNewTargetBlocksAverage div 4) * 3, FormatFloat('0.0', LAverages[3]),
    CT_CalcNewTargetBlocksAverage div 2, FormatFloat('0.0', LAverages[4]),
    CT_CalcNewTargetBlocksAverage div 4, FormatFloat('0.0', LAverages[5])]);
  // Secondary averages never supply committed-tip or connection presentation.
  ASnapshot.ChainStatus := '';
end;

constructor TSyncNodeSnapshotThread.Create(ANode: TNode; ANotify: TThreadNotify;
  AOnCompleted: TNotifyEvent; AReadSnapshot: TSyncNodeSnapshotReader; ARevision: Int64);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
  FNotify := ANotify;
  FOnCompleted := AOnCompleted;
  FReadSnapshot := AReadSnapshot;
  Revision := ARevision;
end;

procedure TSyncNodeSnapshotThread.BCExecute;
begin
  try
    if not Terminated then begin
      // No mempool lock may span a bank/safebox read or its disk I/O.
      FReadSnapshot(FNode, Snapshot);
      Succeeded := not Terminated;
    end;
  except
    on E: Exception do
      if not Terminated then ErrorMessage := E.ClassName + ': ' + E.Message;
  end;
  // Canceled workers still retire their owned slot, without publishing values.
  FNotify.Invoke(Self, FOnCompleted);
end;

function TSyncNodeSnapshotThread.WasCanceled: Boolean;
begin
  Result := FCancelRequested;
end;

procedure TSyncNodeSnapshotThread.Cancel;
begin
  // TPCThread marks even normal completion Terminated in its finalizer.
  // Only this owner-issued flag distinguishes a canceled result at publication.
  FCancelRequested := True;
  Terminate;
end;

procedure TCTRLSyncronization.ReadSecondarySnapshot(ANode: TNode;
  out ASnapshot: TSyncNodeSnapshot);
begin
  ReadNodeSnapshot(ANode, ASnapshot);
end;

procedure TCTRLSyncronization.FormCreate(Sender: TObject);
begin
  {$IFNDEF FPC}
  // Keep the previous frame visible until VCL has painted all status children.
  // The themed parent-background path bypasses a panel's own buffering, so
  // these solid-background details containers must paint their own background.
  DoubleBuffered := true;
  paSync.DoubleBuffered := true;
  paSync.ParentBackground := false;
  GroupBox1.DoubleBuffered := true;
  GroupBox1.ParentBackground := false;
  // Paint the transparent progress label and artwork in the same VCL buffer.
  paSplash.DoubleBuffered := true;
  {$ENDIF}
  TUserInterface.AppStarted.Add(OnAppStarted);
  TUserInterface.Loading.Add(OnLoading);
  TUserInterface.Loaded.Add(OnLoaded);
  TUserInterface.StateChanged.Add(OnStateChanged);
  TUserInterface.BlocksChanged.Add(OnBlocksChanged);
  TUserInterface.SynchronizationChanged.Add(OnSynchronizationChanged);
  TUserInterface.NetStatisticsChanged.Add(OnNetworkStatusChanged);
  TUserInterface.NetConnectionsUpdated.Add(OnNetworkStatusChanged);
  TUserInterface.MiningStatusChanged.Add(OnMiningStatusChanged);
  TUserInterface.NodeMessageEvent.Add(OnNodeMessageEvent);

  FSnapshotNotify := TThreadNotify.Create(TThread.CurrentThread);
  FLastActiveConnections := -1;
  FMessagesUnreadCount := 0;
  FLoadingText := TLabel.Create(Self);
  FLoadingText.Name := 'lblLoadingStatus';
  FLoadingText.Parent := paSplash;
  FLoadingText.AutoSize := false;
  FLoadingText.ParentFont := false;
  FLoadingText.Font.Color := clWhite;
  FLoadingText.Font.Size := 16;
  FLoadingText.Font.Style := [fsBold];
  FLoadingText.Alignment := taCenter;
  FLoadingText.Layout := tlCenter;
  FLoadingText.WordWrap := true;
  FLoadingText.Transparent := true;
  FLoadingText.Anchors := [akLeft, akRight];
  FLoadingText.Caption := 'Loading local blockchain...';
  FCancelLoading := TButton.Create(Self);
  FCancelLoading.Name := 'btnCancelLoading';
  FCancelLoading.Parent := paSplash;
  FCancelLoading.SetBounds(paSplash.ClientWidth - 128, paSplash.ClientHeight - 48, 112, 32);
  FCancelLoading.Anchors := [akRight, akBottom];
  FCancelLoading.Caption := 'Cancel startup';
  FCancelLoading.OnClick := OnCancelLoading;
  paSplash.OnResize := LayoutLoadingControls;
  {$IFDEF FPC}
  imgSplash.OnResize := LayoutLoadingControls;
  {$ELSE}
  TImageAccess(imgSplash).OnResize := LayoutLoadingControls;
  {$ENDIF}
  LayoutLoadingControls(paSplash);
end;

procedure TCTRLSyncronization.LayoutLoadingControls(Sender : TObject);
var
  LImageRect : TRect;
  LMargin, LTextHeight, LTop, LMaximumTop : Integer;
begin
  if (not Assigned(FLoadingText)) or (not Assigned(FCancelLoading)) then Exit;
  LMargin := Scale96ToForm(16);
  FCancelLoading.SetBounds(paSplash.ClientWidth - Scale96ToForm(112) - LMargin,
    paSplash.ClientHeight - Scale96ToForm(32) - LMargin,
    Scale96ToForm(112), Scale96ToForm(32));
  // The existing artwork's logo ends at y=353 in its 630-pixel source image.
  // Use the actual image rectangle so the text follows the centered logo.
  {$IFDEF FPC}
  LImageRect := imgSplash.DestRect;
  {$ELSE}
  LImageRect := TImageAccess(imgSplash).DestRect;
  {$ENDIF}
  LTextHeight := Scale96ToForm(56);
  LTop := imgSplash.Top + LImageRect.Top +
    ((LImageRect.Bottom - LImageRect.Top) * 353 div 630) + LMargin;
  LMaximumTop := FCancelLoading.Top - LTextHeight - Scale96ToForm(12);
  if LTop > LMaximumTop then LTop := LMaximumTop;
  if LTop < 0 then LTop := 0;
  FLoadingText.SetBounds(LMargin, LTop, paSplash.ClientWidth - LMargin * 2, LTextHeight);
end;

procedure TCTRLSyncronization.OnCancelLoading(Sender : TObject);
begin
  if Assigned(Application.MainForm) then
    Application.MainForm.Close;
end;

procedure TCTRLSyncronization.FormDestroy(Sender: TObject);
begin
 FSnapshotStopping := True;
 TUserInterface.AppStarted.Remove(OnAppStarted);
 TUserInterface.Loading.Remove (OnLoading);
 TUserInterface.Loaded.Remove(OnLoaded);
 TUserInterface.StateChanged.Remove(OnStateChanged);
 TUserInterface.BlocksChanged.Remove(OnBlocksChanged);
 TUserInterface.SynchronizationChanged.Remove(OnSynchronizationChanged);
 TUserInterface.NetStatisticsChanged.Remove(OnNetworkStatusChanged);
 TUserInterface.NetConnectionsUpdated.Remove(OnNetworkStatusChanged);
 TUserInterface.MiningStatusChanged.Remove(OnMiningStatusChanged);
 TUserInterface.NodeMessageEvent.Remove(OnNodeMessageEvent);
 // Producers must stop before their owned main-thread dispatcher is disposed.
 if Assigned(FSnapshotWorker) then begin
   TSyncNodeSnapshotThread(FSnapshotWorker).Cancel;
   FSnapshotWorker.WaitFor;
   FreeAndNil(FSnapshotWorker);
 end;
 FreeAndNil(FSnapshotNotify);
end;

procedure TCTRLSyncronization.ActivateFirstTime;
begin
  ShowSplash := TUserInterface.State = uisLoading;
  RefreshNodeState;
end;

procedure TCTRLSyncronization.FormShow(Sender: TObject);
begin
  RefreshNodeState;
end;

procedure TCTRLSyncronization.RefreshNodeState;
begin
  // Hosted view entry renders the last completed values immediately. All new
  // bank/safebox reads belong to the worker, even when the mempool is free.
  if FNodeSnapshot.TimeAverage <> '' then begin
    lblTimeAverage.Caption := FNodeSnapshot.TimeAverage;
    lblTimeAverage.Font.Color := FNodeSnapshot.TimeAverageColour;
    lblTimeAverageAux.Caption := FNodeSnapshot.TimeAverageAux;
  end else begin
    lblTimeAverage.Caption := 'Calculating...';
    lblTimeAverageAux.Caption := '';
  end;
  ApplySynchronizationStatus;
  UpdateBlockChainState;
end;

procedure TCTRLSyncronization.OnAppStarted(Sender: TObject);
begin
  ShowSplash := TUserInterface.State = uisLoading;
  btnBack.Enabled := TUserInterface.State <> uisLoading;
  UpdateBlockChainState;
end;

procedure TCTRLSyncronization.OnLoading(Sender: TObject; const AProgress : TProgressInfo);
var pct : String;
begin
  pct := FormatProgressInfo(AProgress);
  if FLoadingText.Caption <> pct then
    FLoadingText.Caption := pct;
  SetStatusText(clGreen, pct);
end;

procedure TCTRLSyncronization.OnLoaded(Sender: TObject);
begin
  ShowSplash := false;
  btnBack.Enabled:=true;
  // Capture the restored values before the normal loaded handler enables
  // networking. A first visit during a busy block batch can then retain these
  // values instead of exposing the designer's placeholder text.
  RefreshNodeState;
  TUserInterface.ShowWallet;
end;

procedure TCTRLSyncronization.OnStateChanged(Sender: TObject);
begin
  ShowSplash := TUserInterface.State = uisLoading;
  btnBack.Enabled := not (TUserInterface.State in [uisLoading, uisError]);
  UpdateBlockChainState;
end;

procedure TCTRLSyncronization.OnBlocksChanged(Sender: TObject);
begin
  // A committed replacement can change a tip without changing its height.
  FHasSecondarySnapshot := False;
  Inc(FSecondaryRevision);
  UpdateBlockChainState;
end;

procedure TCTRLSyncronization.OnNetworkStatusChanged(Sender: TObject);
var
  LConnections: Integer;
begin
  if FSnapshotStopping or TUserInterface.Stopping then Exit;
  UpdateNodeStatus;
  // Byte counters are frequent events. Only a connection-count change needs
  // another blockchain-validity snapshot; traffic does not reread the bank.
  LConnections := TNetData.NetData.NetStatistics.ActiveConnections;
  if LConnections <> FLastActiveConnections then begin
    UpdateBlockChainState;
  end;
end;

procedure TCTRLSyncronization.OnMiningStatusChanged(Sender: TObject);
begin
  if FSnapshotStopping or TUserInterface.Stopping then Exit;
  UpdateMiningStatus;
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
  FShowSplash := ABool;
  FCancelLoading.Enabled := FShowSplash;
  if FShowSplash then begin
    paSplash.Visible:= true;
    paSync.Visible := false;
  end else begin
    paSplash.Visible:= false;
    paSync.Visible := true;
  end;
end;

function TCTRLSyncronization.GetMessagesNotificationText : String;
begin
  Result := lblReceivedMessages.Caption;
end;

procedure TCTRLSyncronization.SetMessagesNotificationText(const text : String);
begin
  lblReceivedMessages.Visible := text <> '';
  lblReceivedMessages.Caption := text;
end;

procedure TCTRLSyncronization.SetStatusText(AColour: TColor; AText: String);
begin
  lblNodeStatus.Font.Color := AColour;
  lblNodeStatus.Caption := AText;
end;

procedure TCTRLSyncronization.UpdateNodeStatus;
begin
  if (not TUserInterface.Started) or (not IsVisible) then exit;

  // State text
  case TUserInterface.State of
     uisLoading: ShowSplash := true; // text set by OnLoading
     uisLoaded: SetStatusText(clGreen, 'Loaded');
     uisDiscoveringPeers: SetStatusText(clGreen, 'Discovering Peers');
     // ApplySynchronizationStatus owns detailed progress. Byte-stat updates
     // must not replace it with a generic caption and invalidate it twice.
     uisSyncronizingBlockchain: ;
     uisActive: SetStatusText(clGreen, 'Active');
     uisIsolated: SetStatusText(clRed, 'Isolated');
     uisDisconnected: begin
       if TUserInterface.Offline then
         SetStatusText(clNavy, 'Offline mode')
       else
         SetStatusText(clRed, 'Disconnected');
     end;
     uisError: SetStatusText(clRed, Format('Error: %s', [TUserInterface.StateText]));
  end;

  // A disk restore owns the bank/mempool locks and may deliver UI progress.
  // Do not wait for those locks from the UI or query partially restored state.
  if TUserInterface.State in [uisLoading, uisError] then Exit;

  // Protocol labels
  lblNetProtocolVersion.Caption := Format('%d (%d)', [CT_NetProtocol_Version, CT_NetProtocol_Available]);
end;

class function TCTRLSyncronization.CurrentNodeStatusText: String;
begin
  // Banner and details consume the same event-owned application state. This
  // formatter never reads the bank or acquires a pending-operation lock.
  case TUserInterface.State of
    uisLoading: Result := 'Initializing';
    uisLoaded: Result := 'Loaded';
    uisDiscoveringPeers: Result := 'Discovering Peers';
    uisSyncronizingBlockchain: Result := 'Synchronizing';
    uisActive: Result := 'Active';
    uisIsolated: Result := 'Isolated';
    uisDisconnected:
      if TUserInterface.Offline then Result := 'Offline mode'
      else Result := 'Disconnected';
    uisError: Result := Format('Error: %s', [TUserInterface.StateText]);
  else
    Result := '';
  end;
end;

procedure TCTRLSyncronization.ApplySynchronizationStatus;
var S: TNodeSynchronizationStatus; P: String;
begin
  if not Assigned(TUserInterface.Node) then Exit;
  S := TUserInterface.SynchronizationStatus;
  FDisplayedSynchronizationStatus := S;
  FHasNodeSnapshot := True;
  FSnapshotBlockCount := S.LocalBlockCount;
  if S.LocalBlockCount > 0 then begin
    lblTotalBlocksValue.Caption := IntToStr(S.LocalBlockCount) + ' (0..' + IntToStr(S.LocalBlockCount - 1) + ')';
    lblBlockAgeValue.Caption := UnixTimeToLocalElapsedTime(S.LastBlockTimestamp);
  end else begin
    lblTotalBlocksValue.Caption := '(none)';
    lblBlockAgeValue.Caption := '(none)';
  end;
  lblTotalAccountsValue.Caption := UIntToStr(UInt64(S.LocalBlockCount) * CT_AccountsPerBlock);
  if S.HasMempoolSummary then begin
    lblPendingOperationsValue.Caption := IntToStr(S.PendingOperationsCount);
    lblBlockTargetValue.Caption := IntToHex(S.CompactTarget, 8);
  end;
  lblProtocolVersion.Caption := Format('%d (%d)', [S.ProtocolVersion, CT_BlockChain_Protocol_Available]);
  if S.State = nssSynchronizing then begin
    P := FormatProgressInfo(S.Progress);
    if P = '' then P := 'Synchronizing';
    SetStatusText(clGreen, P);
  end;
  // Footer and basic labels are one projection of this exact cached commit.
  // A slow/paused secondary reader cannot leave the footer on an older tip.
  if S.LocalBlockCount = 0 then
    FChainStatusText := 'Blocks: 0 | No local blockchain'
  else
    FChainStatusText := Format('Blocks: %d | Last account time: %s',
      [S.LocalBlockCount, FormatDateTime('dd/mm/yyyy hh:nn:ss',
      UnivDateTime2LocalDateTime(UnixToUnivDateTime(S.LastBlockTimestamp)))]);
  if not S.NetworkActive then FChainStatusText := FChainStatusText + ' | Network off'
  else if S.PeerCount = 0 then FChainStatusText := FChainStatusText + ' | No connections';
end;

procedure TCTRLSyncronization.OnSynchronizationChanged(Sender: TObject);
var LSync: Boolean;
begin
  if FSnapshotStopping or TUserInterface.Stopping then Exit;
  LSync := TUserInterface.SynchronizationStatus.State = nssSynchronizing;
  if FWasSynchronizing and not LSync then begin
    FHasSecondarySnapshot := False;
    Inc(FSecondaryRevision);
  end;
  FWasSynchronizing := LSync;
  UpdateBlockChainState;
end;

procedure TCTRLSyncronization.UpdateBlockChainState;
var S: TNodeSynchronizationStatus;
begin
  if FSnapshotStopping or (not TUserInterface.Started) or TUserInterface.Stopping then Exit;
  UpdateNodeStatus;
  UpdateMiningStatus;
  if TUserInterface.State in [uisLoading, uisError] then Exit;
  ApplySynchronizationStatus;
  FSnapshotChanged.Invoke(Self);
  S := TUserInterface.SynchronizationStatus;
  if S.State = nssSynchronizing then begin
    FSnapshotPending := False;
    if Assigned(FSnapshotWorker) then TSyncNodeSnapshotThread(FSnapshotWorker).Cancel;
    Exit;
  end;
  if FLastActiveConnections <> S.PeerCount then begin
    FHasSecondarySnapshot := False;
    Inc(FSecondaryRevision);
  end;
  FLastActiveConnections := S.PeerCount;
  if (not FHasSecondarySnapshot) or (FSecondaryBlockCount <> S.LocalBlockCount) then begin
    FSnapshotPending := True;
    StartNodeSnapshot;
  end;
end;

procedure TCTRLSyncronization.StartNodeSnapshot;
var
  LNode: TNode;
begin
  if FSnapshotStopping or TUserInterface.Stopping or
    Assigned(FSnapshotWorker) or (not FSnapshotPending) then Exit;
  if TUserInterface.SynchronizationStatus.State = nssSynchronizing then Exit;
  if FHasSecondarySnapshot and
    (FSecondaryBlockCount = TUserInterface.SynchronizationStatus.LocalBlockCount) then begin
    FSnapshotPending := False;
    Exit;
  end;
  FSnapshotPending := False;
  LNode := TUserInterface.Node;
  if not Assigned(LNode) then Exit;
  // SafeBox has a separate mutex with no public try-lock. Acquiring the node
  // lock on the UI would not make its subsequent bank getters nonblocking.
  FSnapshotWorker := TSyncNodeSnapshotThread.Create(LNode, FSnapshotNotify,
    OnSnapshotCompleted, ReadSecondarySnapshot, FSecondaryRevision);
  FSnapshotWorker.Start;
end;

procedure TCTRLSyncronization.OnSnapshotCompleted(Sender: TObject);
var
  LWorker: TSyncNodeSnapshotThread;
begin
  if FSnapshotStopping or TUserInterface.Stopping or (Sender <> FSnapshotWorker) then Exit;
  LWorker := TSyncNodeSnapshotThread(FSnapshotWorker);
  FSnapshotWorker := nil;
  // Completion is queued without Synchronize. Joining here cannot wait for a
  // UI callback on the worker; it only finishes the producer's final teardown.
  LWorker.WaitFor;
  try
    if LWorker.Succeeded and (not LWorker.WasCanceled) and
      (LWorker.Revision = FSecondaryRevision) and
      (TUserInterface.SynchronizationStatus.State <> nssSynchronizing) and
      (LWorker.Snapshot.PeerCount = TUserInterface.SynchronizationStatus.PeerCount) and
      (LWorker.Snapshot.BlockCount = TUserInterface.SynchronizationStatus.LocalBlockCount) then
      ApplyNodeSnapshot(LWorker.Snapshot)
    else if LWorker.ErrorMessage <> '' then
      TLog.NewLog(lterror, ClassName, LWorker.ErrorMessage);
  finally
    LWorker.Free;
  end;
  // Requests received during this read coalesce; completion itself is not a
  // new status request and cannot create a self-sustaining refresh loop.
  StartNodeSnapshot;
end;

procedure TCTRLSyncronization.ApplyNodeSnapshot(const ASnapshot: TSyncNodeSnapshot);
begin
  FHasSecondarySnapshot := True;
  FSecondaryBlockCount := ASnapshot.BlockCount;
  FNodeSnapshot := ASnapshot;
  // This worker owns averages only. Basic values and the footer are always
  // rendered together from the current cached synchronization record below.
  lblTimeAverage.Caption := ASnapshot.TimeAverage;
  lblTimeAverage.Font.Color := ASnapshot.TimeAverageColour;
  lblTimeAverageAux.Caption := ASnapshot.TimeAverageAux;
  ApplySynchronizationStatus;
  FSnapshotChanged.Invoke(Self);
end;

procedure TCTRLSyncronization.UpdateMiningStatus;
begin
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
  ClearMessagesNotification;
  TUserInterface.ShowMessagesForm;
end;

procedure TCTRLSyncronization.ClearMessagesNotification;
begin
  lblReceivedMessages.Visible := false;
  lblReceivedMessages.Caption := '';
  FMessagesUnreadCount := 0;
end;

procedure TCTRLSyncronization.btnBackClick(Sender: TObject);
begin
  TUserInterface.ShowWallet;
end;

end.

