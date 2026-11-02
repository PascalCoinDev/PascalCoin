unit UWalletExplorerSchedulingTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license, see the accompanying LICENSE. }

{$mode delphi}

interface

uses Classes, SysUtils, fpcunit, testregistry;

type
  TWalletExplorerSchedulingTests = class(TTestCase)
  private
    procedure CheckProducerBurst(ADeferredLeading: Boolean);
    procedure CheckIndependentDeadline(AQueuedNodeInput: Boolean);
  published
    procedure DeferredLeadingSurvivesBusyViewAndQuietTimer;
    procedure DeferredLeadingResumesOnShowWithoutPolling;
    procedure ContinuousRequestsResetTheOwnedQuietTimer;
    procedure UnrelatedNodeWorkCannotPostponeQuietRefreshRequests;
    procedure QueuedNodeCallbackStartsANewBurstAfterOwnedDeadline;
  end;

implementation

uses Windows, Forms, ExtCtrls, UThread, UNode, UCrypto, UCommon, UCommon.UI,
  UFRMAccountExplorer;

type
  TExplorerScheduler = class(TFRMAccountExplorer)
  private
    FNotifier: TNodeNotifyEvents;
    procedure OnNodeChanged(Sender: TObject);
  protected
    function CanRefreshBackend: Boolean; override;
    procedure RefreshBackend; override;
  public
    Ready, AcceptNodeRequests: Boolean;
    RefreshCount, NodeCallbackCount: Integer;
    LastRefreshTick, LastRequestTick: QWord;
    constructor Create(ANode: TNode);
    destructor Destroy; override;
    procedure ReadyChanged;
    procedure Request;
    function ThrottleActive: Boolean;
    procedure PumpFor(AMilliseconds: Cardinal);
    procedure DeliverTimerBeforeQueuedNodeCallback;
  end;

  TExplorerProducer = class(TPCThread)
  private
    FNode: TNode;
    FStop: LongInt;
  protected
    procedure BCExecute; override;
  public
    LastProducedTick: QWord;
    constructor Create(ANode: TNode);
    procedure Stop;
  end;

const CT_INTERVAL = 200;

constructor TExplorerScheduler.Create(ANode: TNode);
begin
  // Control view readiness and rendering, keeping the actual Explorer scheduler
  // and real core notifications. No wallet keys or chain records are loaded.
  inherited CreateNew(nil);
  Ready := True;
  AcceptNodeRequests := True;
  FNotifier := TNodeNotifyEvents.Create(Self);
  FNotifier.Node := ANode;
  FNotifier.OnBlocksChanged := OnNodeChanged;
  InitializeBackendRefresh;
  BackendRefreshEvent.Interval := TTimeSpan.FromMilliseconds(CT_INTERVAL);
end;

destructor TExplorerScheduler.Destroy;
begin
  Ready := False;
  FNotifier.OnBlocksChanged := nil;
  FreeAndNil(FNotifier);
  inherited;
end;

function TExplorerScheduler.CanRefreshBackend: Boolean;
begin
  Result := Ready;
end;

procedure TExplorerScheduler.RefreshBackend;
begin
  Inc(RefreshCount);
  LastRefreshTick := GetTickCount64;
end;

procedure TExplorerScheduler.OnNodeChanged(Sender: TObject);
begin
  Inc(NodeCallbackCount);
  if AcceptNodeRequests then Request;
end;

procedure TExplorerScheduler.Request;
begin
  LastRequestTick := GetTickCount64;
  ScheduleBackendRefresh;
end;

function TExplorerScheduler.ThrottleActive: Boolean;
begin
  Result := BackendRefreshEvent.BurstActive;
end;

procedure TExplorerScheduler.ReadyChanged;
begin
  OnViewReady(Self);
end;

procedure TExplorerScheduler.PumpFor(AMilliseconds: Cardinal);
var LDeadline: QWord;
begin
  LDeadline := GetTickCount64+AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64>=LDeadline;
end;

procedure TExplorerScheduler.DeliverTimerBeforeQueuedNodeCallback;
var I: Integer; LTimer: TTimer;
begin
  for I := 0 to BackendRefreshEvent.ComponentCount-1 do
    if BackendRefreshEvent.Components[I] is TTimer then begin
      LTimer := TTimer(BackendRefreshEvent.Components[I]);
      if not LTimer.Enabled then raise Exception.Create('Expected an armed Explorer timer');
      LTimer.OnTimer(LTimer);
      Exit;
    end;
  raise Exception.Create('Explorer throttle has no owned timer');
end;

constructor TExplorerProducer.Create(ANode: TNode);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
end;

procedure TExplorerProducer.BCExecute;
begin
  while InterlockedCompareExchange(FStop,0,0)=0 do begin
    FNode.NotifyBlocksChanged;
    LastProducedTick := GetTickCount64;
    Sleep(10);
  end;
end;

procedure TExplorerProducer.Stop;
begin
  InterlockedExchange(FStop,1);
  WaitFor;
end;

procedure TWalletExplorerSchedulingTests.CheckProducerBurst(ADeferredLeading: Boolean);
var
  LNode: TNode;
  LExplorer: TExplorerScheduler;
  LProducer: TExplorerProducer;
  LPreviousDirectory, LDirectory: String;
  LID: TGUID;
  LDeadline: QWord;
begin
  AssertFalse('The fixture must not reuse a global node',TNode.NodeExists);
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetTempDir)+'ExplorerSchedule-'+GUIDToString(LID);
  ForceDirectories(LDirectory);
  LPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(LDirectory);
  LNode := nil;
  LExplorer := nil;
  LProducer := nil;
  try
    TCrypto.InitCrypto;
    LNode := TNode.Create(nil);
    LNode.BroadcastData := False;
    LExplorer := TExplorerScheduler.Create(LNode);
    LExplorer.Ready := not ADeferredLeading;
    LNode.NotifyBlocksChanged;
    LDeadline := GetTickCount64+2500;
    while (LExplorer.NodeCallbackCount=0) and (GetTickCount64<LDeadline) do
      LExplorer.PumpFor(2);
    AssertEquals('The real node must deliver the initial input',1,LExplorer.NodeCallbackCount);
    if ADeferredLeading then AssertEquals(0,LExplorer.RefreshCount)
    else AssertEquals('An eligible Explorer leading request is immediate',1,LExplorer.RefreshCount);

    LProducer := TExplorerProducer.Create(LNode);
    LProducer.Start;
    LDeadline := GetTickCount64+2500;
    while (LExplorer.NodeCallbackCount<2) and (GetTickCount64<LDeadline) do
      LExplorer.PumpFor(2);
    AssertTrue('Producer callbacks reached the actual Explorer scheduler',LExplorer.NodeCallbackCount>=2);
    if ADeferredLeading then begin
      LExplorer.Ready := True;
      LExplorer.ReadyChanged;
      AssertEquals('A busy view retains its one ready leading request',1,LExplorer.RefreshCount);
    end;

    LExplorer.PumpFor(500);
    AssertEquals('Continuing actual refresh inputs must not cause periodic publications',1,LExplorer.RefreshCount);
    LProducer.Stop;
    LDeadline := GetTickCount64+2500;
    while (LExplorer.RefreshCount<2) and (GetTickCount64<LDeadline) do
      LExplorer.PumpFor(2);
    AssertEquals('The settled burst delivers its one final refresh',2,LExplorer.RefreshCount);
    AssertTrue('The final Explorer refresh follows the last input to its own throttle',
      LExplorer.LastRefreshTick-LExplorer.LastRequestTick>=CT_INTERVAL-20);
    LExplorer.PumpFor(CT_INTERVAL+50);
    AssertEquals('There is no duplicate settled refresh',2,LExplorer.RefreshCount);
  finally
    if Assigned(LProducer) then begin
      LProducer.Stop;
      LProducer.Free;
    end;
    LExplorer.Free;
    LNode.Free;
    TNode.SetPascalCoinDataFolder(LPreviousDirectory);
    RemoveDir(LDirectory);
  end;
end;

procedure TWalletExplorerSchedulingTests.DeferredLeadingSurvivesBusyViewAndQuietTimer;
begin
  CheckProducerBurst(True);
end;

procedure TWalletExplorerSchedulingTests.DeferredLeadingResumesOnShowWithoutPolling;
var LExplorer: TExplorerScheduler;
begin
  LExplorer := TExplorerScheduler.Create(nil);
  try
    LExplorer.Ready := False;
    LExplorer.Request;
    AssertEquals('The hidden busy view retains its admitted input', 0, LExplorer.RefreshCount);
    LExplorer.Ready := True;
    LExplorer.Show;
    AssertEquals('Actual DoShow releases admitted work without a poll', 1, LExplorer.RefreshCount);
    LExplorer.PumpFor(CT_INTERVAL + 50);
    AssertEquals('Visibility did not manufacture another input', 1, LExplorer.RefreshCount);
  finally
    LExplorer.Free;
  end;
end;

procedure TWalletExplorerSchedulingTests.ContinuousRequestsResetTheOwnedQuietTimer;
begin
  CheckProducerBurst(False);
end;

procedure TWalletExplorerSchedulingTests.CheckIndependentDeadline(AQueuedNodeInput: Boolean);
var LNode: TNode; LExplorer: TExplorerScheduler; LProducer: TExplorerProducer;
  LPreviousDirectory, LDirectory: String; LID: TGUID;
begin
  AssertFalse('The fixture must not reuse a global node',TNode.NodeExists);
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetTempDir)+'ExplorerOwnDeadline-'+GUIDToString(LID);
  ForceDirectories(LDirectory);
  LPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(LDirectory);
  LNode := nil;
  LExplorer := nil;
  LProducer := nil;
  try
    TCrypto.InitCrypto;
    LNode := TNode.Create(nil);
    LNode.BroadcastData := False;
    LExplorer := TExplorerScheduler.Create(LNode);
    LExplorer.AcceptNodeRequests := AQueuedNodeInput;
    LNode.BeginBlockChainUpdate;
    try
      LExplorer.Request;
      LExplorer.Request;
      AssertEquals('The first own input delivers immediately',1,LExplorer.RefreshCount);
      LProducer := TExplorerProducer.Create(LNode);
      LProducer.Start;
      if AQueuedNodeInput then begin
        // An external notification that has not reached Request is not an
        // input to the throttle. A due native timer may run before it.
        Sleep(CT_INTERVAL+100);
        AssertEquals('Core callbacks are queued until the UI pumps',0,LExplorer.NodeCallbackCount);
        LExplorer.DeliverTimerBeforeQueuedNodeCallback;
      end else
        // Observe real producer timestamps without translating those
        // unrelated notifications into refresh requests for this view.
        LExplorer.PumpFor(CT_INTERVAL+200);
      AssertTrue('Core work remains active independently',LNode.IsUpdatingBlockChain);
      AssertEquals('Core state cannot postpone an eligible own quiet refresh',2,LExplorer.RefreshCount);
      AssertFalse('Only the component input deadline controls completion',LExplorer.ThrottleActive);
      LProducer.Stop;
      if AQueuedNodeInput then begin
        LExplorer.PumpFor(2);
        AssertTrue('The queued real callback was delivered',LExplorer.NodeCallbackCount>0);
        AssertEquals('Its later request begins a new immediate burst',3,LExplorer.RefreshCount);
      end else
        AssertTrue('Unrelated real callbacks were delivered',LExplorer.NodeCallbackCount>0);
      LExplorer.PumpFor(CT_INTERVAL+100);
      AssertEquals('No duplicate final request is manufactured',
        2+Ord(AQueuedNodeInput),LExplorer.RefreshCount);
    finally
      if Assigned(LProducer) then begin
        LProducer.Stop;
        FreeAndNil(LProducer);
      end;
      LNode.EndBlockChainUpdate;
    end;
  finally
    LExplorer.Free;
    LNode.Free;
    TNode.SetPascalCoinDataFolder(LPreviousDirectory);
    RemoveDir(LDirectory);
  end;
end;

procedure TWalletExplorerSchedulingTests.UnrelatedNodeWorkCannotPostponeQuietRefreshRequests;
begin
  CheckIndependentDeadline(False);
end;

procedure TWalletExplorerSchedulingTests.QueuedNodeCallbackStartsANewBurstAfterOwnedDeadline;
begin
  CheckIndependentDeadline(True);
end;

initialization
  RegisterTest(TWalletExplorerSchedulingTests);
end.
