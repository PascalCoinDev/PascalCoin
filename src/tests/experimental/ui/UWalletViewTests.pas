unit UWalletViewTests;

{ Copyright (c) 2026 by Herman Schoenfeld
  Distributed under the MIT software license; see the accompanying LICENSE.
  Physical synchronization scopes own node busy state. Grid admission and optional
  per-grid quiet timers are tested separately from that work accounting. }

{$mode delphi}

interface

uses Classes, SysUtils, Forms, fpcunit, testregistry;

type
  TWalletViewTests = class(TTestCase)
  published
    procedure NestedBlockWorkEndsAfterLastScope;
    procedure ConcurrentBlockWorkRemainsBusyUntilBothWorkersFinish;
    procedure FailedBlockWorkReleasesItsPhysicalScope;
  end;

implementation

uses Windows, SyncObjs, UThread, UNode, UCrypto;

type
  TCoreNodeObserver = class(TComponent)
  private
    FNotifier: TNodeNotifyEvents;
    procedure SynchronizationChanged(Sender: TObject;
      const AStatus: TNodeSynchronizationStatus);
  public
    SynchronizationCallbackCount: Integer;
    LastStatus: TNodeSynchronizationStatus;
    constructor Create(ANode: TNode); reintroduce;
    destructor Destroy; override;
    procedure PumpFor(AMilliseconds: Cardinal);
  end;

  TBlockWorkThread = class(TPCThread)
  private
    FNode: TNode;
    FEntered, FReleaseWork: TEvent;
  protected
    procedure BCExecute; override;
  public
    WatchdogReleased: Boolean;
    constructor Create(ANode: TNode);
    destructor Destroy; override;
    procedure WaitForEntry;
    procedure Stop;
  end;

procedure OpenBackendTest(out ANode: TNode; out APreviousDirectory,
  ADirectory: String);
var LID: TGUID;
begin
  if TNode.NodeExists then
    raise Exception.Create('The core-work fixture must not reuse a global node');
  CreateGUID(LID);
  ADirectory := IncludeTrailingPathDelimiter(GetTempDir)+'WalletBlockWork-'+GUIDToString(LID);
  ForceDirectories(ADirectory);
  APreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(ADirectory);
  try
    TCrypto.InitCrypto;
    ANode := TNode.Create(nil);
    ANode.BroadcastData := False;
  except
    TNode.SetPascalCoinDataFolder(APreviousDirectory);
    RemoveDir(ADirectory);
    raise;
  end;
end;

procedure CloseBackendTest(var ANode: TNode; const APreviousDirectory,
  ADirectory: String);
begin
  FreeAndNil(ANode);
  TNode.SetPascalCoinDataFolder(APreviousDirectory);
  RemoveDir(ADirectory);
end;

constructor TBlockWorkThread.Create(ANode: TNode);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
  FEntered := TEvent.Create(nil,True,False,'');
  FReleaseWork := TEvent.Create(nil,True,False,'');
end;

destructor TBlockWorkThread.Destroy;
begin
  Stop;
  FEntered.Free;
  FReleaseWork.Free;
  inherited;
end;

procedure TBlockWorkThread.BCExecute;
begin
  FNode.BeginBlockChainUpdate;
  try
    FEntered.SetEvent;
    WatchdogReleased := FReleaseWork.WaitFor(20000) <> wrSignaled;
  finally
    FNode.EndBlockChainUpdate;
  end;
end;

procedure TBlockWorkThread.WaitForEntry;
begin
  if FEntered.WaitFor(2500) <> wrSignaled then
    raise Exception.Create('The block-work fixture did not enter its update scope');
end;

procedure TBlockWorkThread.Stop;
begin
  FReleaseWork.SetEvent;
  WaitFor;
end;

constructor TCoreNodeObserver.Create(ANode: TNode);
begin
  inherited Create(nil);
  FNotifier := TNodeNotifyEvents.Create(Self);
  FNotifier.Node := ANode;
  FNotifier.OnSynchronizationChanged := SynchronizationChanged;
end;

destructor TCoreNodeObserver.Destroy;
begin
  FNotifier.OnSynchronizationChanged := nil;
  inherited;
end;

procedure TCoreNodeObserver.SynchronizationChanged(Sender: TObject;
  const AStatus: TNodeSynchronizationStatus);
begin
  Inc(SynchronizationCallbackCount);
  LastStatus := AStatus;
end;

procedure TCoreNodeObserver.PumpFor(AMilliseconds: Cardinal);
var LDeadline: QWord;
begin
  LDeadline := GetTickCount64+AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64>=LDeadline;
end;

procedure TWalletViewTests.NestedBlockWorkEndsAfterLastScope;
var
  LNode: TNode;
  LObserver: TCoreNodeObserver;
  LDirectory, LPreviousDirectory: String;
begin
  OpenBackendTest(LNode,LPreviousDirectory,LDirectory);
  LObserver := nil;
  try
    LObserver := TCoreNodeObserver.Create(LNode);
    LNode.BeginBlockChainUpdate;
    try
      LNode.BeginBlockChainUpdate;
      try
        AssertEquals('Each nested scope owns one physical reservation',2,
          LNode.GetSynchronizationStatus.ActiveTransfers);
        AssertTrue(LNode.IsUpdatingBlockChain);
        LObserver.PumpFor(150);
        AssertEquals('The status event reports the same physical reservations',2,
          LObserver.LastStatus.ActiveTransfers);
        AssertEquals(Ord(nssSynchronizing),Ord(LObserver.LastStatus.State));
      finally
        LNode.EndBlockChainUpdate;
      end;
      LObserver.PumpFor(150);
      AssertTrue('Ending an inner scope must leave outer work active',LNode.IsUpdatingBlockChain);
      AssertEquals(1,LNode.GetSynchronizationStatus.ActiveTransfers);
      AssertEquals(1,LObserver.LastStatus.ActiveTransfers);
    finally
      LNode.EndBlockChainUpdate;
    end;
    AssertFalse(LNode.IsUpdatingBlockChain);
    AssertEquals('The final exit releases physical work immediately',0,
      LNode.GetSynchronizationStatus.ActiveTransfers);
    LObserver.PumpFor(150);
    AssertTrue('Actual synchronization callbacks must have been delivered',
      LObserver.SynchronizationCallbackCount>0);
    AssertEquals(0,LObserver.LastStatus.ActiveTransfers);
    AssertTrue(LObserver.LastStatus.State<>nssSynchronizing);
  finally
    LObserver.Free;
    CloseBackendTest(LNode,LPreviousDirectory,LDirectory);
  end;
end;

procedure TWalletViewTests.ConcurrentBlockWorkRemainsBusyUntilBothWorkersFinish;
var
  LNode: TNode;
  LObserver: TCoreNodeObserver;
  LFirst, LSecond: TBlockWorkThread;
  LDirectory, LPreviousDirectory: String;
begin
  OpenBackendTest(LNode,LPreviousDirectory,LDirectory);
  LObserver := nil;
  LFirst := nil;
  LSecond := nil;
  try
    LObserver := TCoreNodeObserver.Create(LNode);
    LFirst := TBlockWorkThread.Create(LNode);
    LFirst.Start;
    LFirst.WaitForEntry;
    LSecond := TBlockWorkThread.Create(LNode);
    LSecond.Start;
    LSecond.WaitForEntry;
    AssertEquals('Both producers own a physical reservation',2,
      LNode.GetSynchronizationStatus.ActiveTransfers);
    LFirst.Stop;
    LObserver.PumpFor(150);
    AssertTrue('One completed producer must not clear another producer',LNode.IsUpdatingBlockChain);
    AssertEquals(1,LNode.GetSynchronizationStatus.ActiveTransfers);
    AssertEquals(1,LObserver.LastStatus.ActiveTransfers);
    LSecond.Stop;
    AssertFalse(LNode.IsUpdatingBlockChain);
    AssertEquals(0,LNode.GetSynchronizationStatus.ActiveTransfers);
    LObserver.PumpFor(150);
    AssertEquals(0,LObserver.LastStatus.ActiveTransfers);
    AssertTrue(LObserver.LastStatus.State<>nssSynchronizing);
    AssertFalse(LFirst.WatchdogReleased);
    AssertFalse(LSecond.WatchdogReleased);
  finally
    LSecond.Free;
    LFirst.Free;
    LObserver.Free;
    CloseBackendTest(LNode,LPreviousDirectory,LDirectory);
  end;
end;

procedure TWalletViewTests.FailedBlockWorkReleasesItsPhysicalScope;
var
  LNode: TNode;
  LObserver: TCoreNodeObserver;
  LDirectory, LPreviousDirectory: String;
  LFailed: Boolean;
begin
  OpenBackendTest(LNode,LPreviousDirectory,LDirectory);
  LObserver := nil;
  try
    LObserver := TCoreNodeObserver.Create(LNode);
    LFailed := False;
    try
      LNode.BeginBlockChainUpdate;
      try
        LObserver.PumpFor(150);
        raise EAbort.Create('Controlled invalid-block validation failure');
      finally
        LNode.EndBlockChainUpdate;
      end;
    except
      on E: EAbort do LFailed := True;
    end;
    AssertTrue('The controlled validation failure must execute',LFailed);
    AssertFalse('Failure cleanup must balance the active-work scope',LNode.IsUpdatingBlockChain);
    AssertEquals(0,LNode.GetSynchronizationStatus.ActiveTransfers);
    LObserver.PumpFor(150);
    AssertEquals('The completion event must release the physical busy state',0,
      LObserver.LastStatus.ActiveTransfers);
    AssertTrue(LObserver.LastStatus.State<>nssSynchronizing);
  finally
    LObserver.Free;
    CloseBackendTest(LNode,LPreviousDirectory,LDirectory);
  end;
end;

initialization
  RegisterTest(TWalletViewTests);
end.
