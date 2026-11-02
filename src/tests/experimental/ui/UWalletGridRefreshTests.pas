unit UWalletGridRefreshTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE.
  The shared fixture covers grid behavior; this unit adds the real node producer. }

{$mode delphi}

interface

uses Classes, SysUtils, Forms, fpcunit, testregistry, UNode, UCrypto,
  UNetProtocol, UVisualGrid.Refresh.Tests;

implementation

type
  TWalletGridRefreshTests = class(TVisualGridRefreshFixture)
  private
    FNodeCallbacks: Integer;
    procedure NodeChanged(Sender: TObject);
  published
    procedure SettledGridDoesNotFetchOnBackendNotificationTimers;
  end;

  TGridNotificationProducer = class(TThread)
  private
    FNode: TNode;
    FStop: LongInt;
  protected
    procedure Execute; override;
  public
    constructor Create(ANode: TNode);
    procedure Stop;
  end;

constructor TGridNotificationProducer.Create(ANode: TNode);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
end;

procedure TGridNotificationProducer.Execute;
begin
  while InterlockedCompareExchange(FStop, 0, 0) = 0 do begin
    FNode.NotifyBlocksChanged;
    Sleep(10);
  end;
end;

procedure TGridNotificationProducer.Stop;
begin
  InterlockedExchange(FStop, 1);
  WaitFor;
end;

procedure TWalletGridRefreshTests.NodeChanged(Sender: TObject);
begin
  Inc(FNodeCallbacks);
end;

procedure TWalletGridRefreshTests.SettledGridDoesNotFetchOnBackendNotificationTimers;
var LNotifier: TNodeNotifyEvents; LProducer: TGridNotificationProducer;
  LFetches, LCompleted: Integer;
  LNode: TNode; LOwnsNetData: Boolean; LPreviousDirectory, LDirectory: String;
  LID: TGUID;
begin
  AssertFalse('The fixture must not reuse a global node', TNode.NodeExists);
  TCrypto.InitCrypto;
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetTempDir) + 'GridRefresh-' + GUIDToString(LID);
  ForceDirectories(LDirectory);
  LPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(LDirectory);
  LOwnsNetData := not TNetData.NetDataExists;
  LNode := TNode.Create(nil);
  LNode.BroadcastData := False;
  LNotifier := TNodeNotifyEvents.Create(nil);
  LNotifier.Node := LNode;
  LNotifier.OnBlocksChanged := NodeChanged;
  LNotifier.OnOperationsChanged := NodeChanged;
  LProducer := TGridNotificationProducer.Create(LNode);
  try
    LFetches := FetchCount;
    LCompleted := FCompletions;
    LProducer.Start;
    PumpFor(1300);
    LProducer.Stop;
    PumpFor(150);
    AssertTrue('Real queued producer notifications must reach the UI', FNodeCallbacks > 5);
    AssertEquals('Producer and grid timers alone must not request another page', LFetches, FetchCount);
    AssertEquals(LCompleted, FCompletions);
    AssertFalse(FGrid.IsUpdating);
  finally
    LProducer.Stop;
    LProducer.Free;
    LNotifier.OnBlocksChanged := nil;
    LNotifier.OnOperationsChanged := nil;
    LNotifier.Free;
    if LOwnsNetData and TNetData.NetDataExists then TNetData.NetData.Free;
    LNode.Free;
    TNode.SetPascalCoinDataFolder(LPreviousDirectory);
    RemoveDir(LDirectory);
  end;
end;

initialization
  RegisterTest(TWalletGridRefreshTests);
end.
