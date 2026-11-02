unit UMempoolLockChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Deterministic integration checks for the actual node mempool lock API.
  Distributed under the MIT software license; see the accompanying LICENSE. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

// Run in an owned isolated process. Actor gates verify actual Node ownership,
// including balanced upgradeable-read and nested-write release on failure.
function RunMempoolLockChecks(const ALogPath: String): Integer;

implementation

uses {$IFNDEF FPC}Winapi.Windows,{$ELSE}Windows,{$ENDIF}
  Classes, SysUtils, SyncObjs, Generics.Collections, UNode, UThread, UBlockChain,
  UCrypto, UNetProtocol, UOperationReadHashChecks, UMinerTemplateUpgradeChecks;

type
  TLockAction = (laRead, laWrite, laNestedWrite, laCancelableRead,
    laUpgradeable, laPromotingUpgradeable);

  EExpectedUpgradeFailure = class(Exception);

  TLockCounters = class
  public
    Readers, Writers, Violations: LongInt;
  end;

  TLockActor = class(TPCThread)
  private
    FNode: TNode;
    FAction: TLockAction;
    FCounters: TLockCounters;
    FStarted: Boolean;
    procedure Hold;
    procedure CountRead(AEnter: Boolean);
    procedure CountWrite(AEnter: Boolean);
  protected
    procedure BCExecute; override;
  public
    Entered, ReleaseLock, Completed, Attempted: TEvent;
    Promote, Writing, LeaveWrite, AfterWrite: TEvent;
    Mempool: TPCOperationsComp;
    ErrorMessage: String;
    constructor Create(ANode: TNode; AAction: TLockAction; ACounters: TLockCounters);
    destructor Destroy; override;
    procedure Start; reintroduce;
  end;

  TLockChecks = class
  private
    FNode: TNode;
    FLogPath: String;
    FCounters: TLockCounters;
    FActors: TList<TLockActor>;
    function Actor(AAction: TLockAction): TLockActor;
    procedure StopActors;
    procedure Require(AValue: Boolean; const AMessage: String);
    procedure Log(const AText: String);
    procedure Entered(AActor: TLockActor; const AMessage: String);
    procedure CheckReaders;
    procedure CheckExclusion;
    procedure CheckNestedWriter;
    procedure CheckTryAcquisition;
    procedure CheckCancellation;
    procedure CheckUpgradeableSharing;
    procedure CheckUpgradeablePromotion;
    procedure CheckUpgradeableException;
    procedure CheckUpgradeableCancellation;
  public
    constructor Create(ANode: TNode; const ALogPath: String);
    destructor Destroy; override;
    procedure Run;
  end;

constructor TLockActor.Create(ANode: TNode; AAction: TLockAction; ACounters: TLockCounters);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FNode := ANode;
  FAction := AAction;
  FCounters := ACounters;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseLock := TEvent.Create(nil, True, False, '');
  Completed := TEvent.Create(nil, True, False, '');
  Attempted := TEvent.Create(nil, True, False, '');
  Promote := TEvent.Create(nil, True, False, '');
  Writing := TEvent.Create(nil, True, False, '');
  LeaveWrite := TEvent.Create(nil, True, False, '');
  AfterWrite := TEvent.Create(nil, True, False, '');
end;

procedure TLockActor.Start;
begin
  if FStarted then raise EThread.Create('Lock actor already started');
  FStarted := True;
  inherited Start;
end;

destructor TLockActor.Destroy;
begin
  Terminate;
  ReleaseLock.SetEvent;
  Promote.SetEvent;
  LeaveWrite.SetEvent;
  if not FStarted then Start;
  WaitFor;
  AfterWrite.Free;
  LeaveWrite.Free;
  Writing.Free;
  Promote.Free;
  Attempted.Free;
  Completed.Free;
  ReleaseLock.Free;
  Entered.Free;
  inherited;
end;

procedure TLockActor.CountRead(AEnter: Boolean);
begin
  if AEnter then begin
    InterlockedIncrement(FCounters.Readers);
    if InterlockedCompareExchange(FCounters.Writers, 0, 0) <> 0 then
      InterlockedIncrement(FCounters.Violations);
  end else InterlockedDecrement(FCounters.Readers);
end;

procedure TLockActor.CountWrite(AEnter: Boolean);
begin
  if AEnter then begin
    if (InterlockedIncrement(FCounters.Writers) <> 1) or
      (InterlockedCompareExchange(FCounters.Readers, 0, 0) <> 0) then
      InterlockedIncrement(FCounters.Violations);
  end else InterlockedDecrement(FCounters.Writers);
end;

procedure TLockActor.Hold;
begin
  Entered.SetEvent;
  while not Terminated do
    if ReleaseLock.WaitFor(10) = wrSignaled then Break;
end;

procedure TLockActor.BCExecute;
var LNested: TPCOperationsComp; LCountedRead: Boolean;
begin
  try
    try
      if Terminated then Exit;
      case FAction of
        laRead: begin
          DebugStep := 'acquire reader';
          Mempool := FNode.LockMempoolRead;
          CountRead(True);
          try Hold;
          finally CountRead(False); FNode.UnlockMempoolRead; end;
        end;
        laWrite: begin
          DebugStep := 'acquire writer';
          Mempool := FNode.LockMempoolWrite;
          CountWrite(True);
          try Hold;
          finally CountWrite(False); FNode.UnlockMempoolWrite; end;
        end;
        laNestedWrite: begin
          DebugStep := 'outer writer';
          Mempool := FNode.LockMempoolWrite;
          CountWrite(True);
          try
            DebugStep := 'recursive writer';
            LNested := FNode.LockMempoolWrite;
            try
              if LNested <> Mempool then raise Exception.Create('Nested writer changed mempool');
              DebugStep := 'writer-owned reader';
              LNested := FNode.LockMempoolRead;
              try
                if LNested <> Mempool then raise Exception.Create('Writer-owned reader changed mempool');
                DebugStep := 'writer-owned try writer';
                if not FNode.TryLockNode(0) then raise Exception.Create('Nested exclusive try failed');
                FNode.UnlockNode;
                DebugStep := 'writer-owned try reader';
                if not FNode.TryLockMempoolRead(0, LNested) then
                  raise Exception.Create('Writer-owned shared try failed');
                FNode.UnlockMempoolRead;
              finally FNode.UnlockMempoolRead; end;
            finally FNode.UnlockMempoolWrite; end;
            DebugStep := 'nested sequence completed';
            Hold;
          finally CountWrite(False); FNode.UnlockMempoolWrite; end;
        end;
        laCancelableRead: begin
          DebugStep := 'cooperative timed acquisition';
          Entered.SetEvent;
          while not Terminated do begin
            if FNode.TryLockMempoolRead(20, LNested) then begin
              FNode.UnlockMempoolRead;
              Break;
            end;
            Attempted.SetEvent;
          end;
        end;
        laUpgradeable: begin
          DebugStep := 'acquire upgradeable reader';
          Mempool := FNode.LockMempoolUpgradeableRead;
          CountRead(True);
          try Hold;
          finally CountRead(False); FNode.UnlockMempoolUpgradeableRead; end;
        end;
        laPromotingUpgradeable: begin
          DebugStep := 'acquire promotable reader';
          Mempool := FNode.LockMempoolUpgradeableRead;
          CountRead(True);
          LCountedRead := True;
          try
            Entered.SetEvent;
            while not Terminated do
              if Promote.WaitFor(10) = wrSignaled then Break;
            if Terminated then Exit;
            // Ownership is still upgradeable while waiting, but its own read
            // must not be counted as a foreign reader during exclusive work.
            CountRead(False);
            LCountedRead := False;
            DebugStep := 'promote to nested writer';
            LNested := FNode.LockMempoolWrite;
            CountWrite(True);
            try
              if LNested <> Mempool then raise Exception.Create('Promotion changed mempool');
              Writing.SetEvent;
              while not Terminated do
                if LeaveWrite.WaitFor(10) = wrSignaled then Break;
            finally CountWrite(False); FNode.UnlockMempoolWrite; end;
            CountRead(True);
            LCountedRead := True;
            DebugStep := 'outer upgradeable retained after writer release';
            AfterWrite.SetEvent;
            Hold;
          finally
            if LCountedRead then CountRead(False);
            FNode.UnlockMempoolUpgradeableRead;
          end;
        end;
      end;
    except on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message; end;
  finally Completed.SetEvent; end;
end;

constructor TLockChecks.Create(ANode: TNode; const ALogPath: String);
begin
  inherited Create;
  FNode := ANode;
  FLogPath := ALogPath;
  FCounters := TLockCounters.Create;
  FActors := TList<TLockActor>.Create;
end;

destructor TLockChecks.Destroy;
begin
  StopActors;
  FActors.Free;
  FCounters.Free;
  inherited;
end;

function TLockChecks.Actor(AAction: TLockAction): TLockActor;
begin
  Result := TLockActor.Create(FNode, AAction, FCounters);
  FActors.Add(Result);
  Result.Start;
end;

procedure TLockChecks.StopActors;
var A: TLockActor; LError: String;
begin
  // Release ALL holders before joining ANY waiter, including failed checks.
  for A in FActors do begin
    A.Terminate;
    A.ReleaseLock.SetEvent;
    A.Promote.SetEvent;
    A.LeaveWrite.SetEvent;
  end;
  LError := '';
  for A in FActors do begin
    A.WaitFor;
    if (LError = '') and (A.ErrorMessage <> '') then LError := A.ErrorMessage;
  end;
  for A in FActors do A.Free;
  FActors.Clear;
  if LError <> '' then raise Exception.Create('Actual lock actor failed: ' + LError);
end;

procedure TLockChecks.Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then raise Exception.Create(AMessage);
end;

procedure TLockChecks.Log(const AText: String);
var F: TextFile;
begin
  AssignFile(F, FLogPath);
  if FileExists(FLogPath) then Append(F) else Rewrite(F);
  try WriteLn(F, GetTickCount64, ' ', AText); finally CloseFile(F); end;
end;

procedure TLockChecks.Entered(AActor: TLockActor; const AMessage: String);
begin
  Require(AActor.Entered.WaitFor(1500) = wrSignaled,
    AMessage + '; step=' + AActor.DebugStep + '; error=' + AActor.ErrorMessage);
end;

procedure TLockChecks.CheckReaders;
var A, B: TLockActor;
begin
  A := Actor(laRead); Entered(A, 'First reader did not enter');
  B := Actor(laRead);
  Require(B.Entered.WaitFor(350) = wrSignaled,
    'Second actual Node reader was serialized behind the first reader');
  Require(A.ReleaseLock.WaitFor(0) <> wrSignaled, 'First reader gate was released early');
  Require(A.Mempool = B.Mempool, 'Readers received different mempools');
end;

procedure TLockChecks.CheckExclusion;
var A, B, W, R, W2: TLockActor;
begin
  A := Actor(laRead); Entered(A, 'First exclusion reader did not enter');
  B := Actor(laRead); Entered(B, 'Second exclusion reader did not enter');
  W := Actor(laWrite);
  Require(W.Entered.WaitFor(100) = wrTimeout, 'Writer overlapped two readers');
  A.ReleaseLock.SetEvent;
  Require(A.Completed.WaitFor(1000) = wrSignaled, 'First reader did not leave');
  Require(W.Entered.WaitFor(100) = wrTimeout, 'Writer entered before the last reader left');
  B.ReleaseLock.SetEvent; Entered(W, 'Writer did not progress after all readers left');
  R := Actor(laRead); W2 := Actor(laWrite);
  Require(R.Entered.WaitFor(100) = wrTimeout, 'Reader overlapped exclusive writer');
  Require(W2.Entered.WaitFor(100) = wrTimeout, 'Two writers overlapped');
  // Do not impose a reader/writer preference policy on the remaining race.
  R.ReleaseLock.SetEvent; W2.ReleaseLock.SetEvent; W.ReleaseLock.SetEvent;
  Require(R.Completed.WaitFor(1500) = wrSignaled, 'Reader did not progress after writer release');
  Require(W2.Completed.WaitFor(1500) = wrSignaled, 'Next writer did not progress after writer release');
end;

procedure TLockChecks.CheckNestedWriter;
var W, R: TLockActor;
begin
  W := Actor(laNestedWrite); Entered(W, 'Existing recursive writer/read/write path deadlocked');
  R := Actor(laRead);
  Require(R.Entered.WaitFor(100) = wrTimeout, 'Nested reader prematurely released outer writer exclusion');
  W.ReleaseLock.SetEvent; Entered(R, 'Outer writer remained held after balanced nested releases');
end;

procedure TLockChecks.CheckTryAcquisition;
var W, R: TLockActor; L: TPCOperationsComp; T: UInt64; Acquired: Boolean;
begin
  W := Actor(laWrite); Entered(W, 'Try-lock writer did not enter');
  T := GetTickCount64; Acquired := FNode.TryLockNode(0);
  if Acquired then FNode.UnlockNode;
  Require(not Acquired, 'Exclusive zero-wait try overlapped a writer');
  Require(GetTickCount64-T < 150, 'Exclusive zero-wait try blocked');
  T := GetTickCount64; Acquired := FNode.TryLockNode(60);
  if Acquired then FNode.UnlockNode;
  Require(not Acquired, 'Timed exclusive try overlapped a writer');
  Require((GetTickCount64-T >= 45) and (GetTickCount64-T < 1000), 'Exclusive timeout was not finite');
  L := TPCOperationsComp(Pointer(1)); T := GetTickCount64; Acquired := FNode.TryLockMempoolRead(0, L);
  if Acquired then FNode.UnlockMempoolRead;
  Require(not Acquired and not Assigned(L), 'Failed shared try must return nil without acquiring');
  Require(GetTickCount64-T < 150, 'Shared zero-wait try blocked');
  T := GetTickCount64; Acquired := FNode.TryLockMempoolRead(60, L);
  if Acquired then FNode.UnlockMempoolRead;
  Require(not Acquired and not Assigned(L), 'Timed shared try overlapped a writer');
  Require((GetTickCount64-T >= 45) and (GetTickCount64-T < 1000), 'Shared timeout was not finite');
  W.ReleaseLock.SetEvent; Require(W.Completed.WaitFor(1000) = wrSignaled, 'Try-lock writer did not leave');
  R := Actor(laRead); Entered(R, 'Shared try fixture reader did not enter');
  Require(FNode.TryLockMempoolRead(0, L), 'Shared nonblocking try was serialized behind another reader');
  try Require(L = R.Mempool, 'Shared try returned another mempool');
  finally FNode.UnlockMempoolRead; end;
  Acquired := FNode.TryLockNode(0);
  if Acquired then FNode.UnlockNode;
  Require(not Acquired, 'Legacy TryLockNode lost exclusive semantics');
end;

procedure TLockChecks.CheckCancellation;
var W, C: TLockActor; T: UInt64;
begin
  W := Actor(laWrite); Entered(W, 'Cancellation writer did not enter');
  C := Actor(laCancelableRead); Entered(C, 'Cancelable acquisition did not start');
  Require(C.Attempted.WaitFor(500) = wrSignaled, 'Cancelable actor never completed a bounded failed acquisition');
  T := GetTickCount64; C.Terminate;
  Require(C.Completed.WaitFor(500) = wrSignaled, 'Canceled reader waited for owner release');
  Require(GetTickCount64-T < 500, 'Cancellation exceeded its bounded try interval');
  Require(W.ReleaseLock.WaitFor(0) <> wrSignaled, 'Cancellation fixture released its writer early');
  W.ReleaseLock.SetEvent;
end;

procedure TLockChecks.CheckUpgradeableSharing;
var U, R, U2: TLockActor;
begin
  U := Actor(laUpgradeable); Entered(U, 'First upgradeable reader did not enter');
  R := Actor(laRead); Entered(R, 'Ordinary reader was serialized behind upgradeable reader');
  U2 := Actor(laUpgradeable);
  Require(U2.Entered.WaitFor(100) = wrTimeout, 'Two upgradeable readers overlapped');
  Require(U.Mempool = R.Mempool, 'Upgradeable and ordinary reader saw different mempools');
  U.ReleaseLock.SetEvent;
  Entered(U2, 'Second upgradeable reader did not progress after first release');
  Require(R.ReleaseLock.WaitFor(0) <> wrSignaled, 'Reader sharing gate was released early');
end;

procedure TLockChecks.CheckUpgradeablePromotion;
var U, R, R2, U2: TLockActor;
begin
  U := Actor(laPromotingUpgradeable); Entered(U, 'Promotable reader did not enter');
  R := Actor(laRead); Entered(R, 'Reader did not coexist before promotion');
  U.Promote.SetEvent;
  Require(U.Writing.WaitFor(100) = wrTimeout, 'Upgrade entered writer while foreign reader remained');
  R.ReleaseLock.SetEvent;
  Require(U.Writing.WaitFor(1500) = wrSignaled, 'Upgrade did not become writer after reader release');
  R2 := Actor(laRead); U2 := Actor(laUpgradeable);
  Require(R2.Entered.WaitFor(100) = wrTimeout, 'Foreign reader overlapped upgraded writer');
  Require(U2.Entered.WaitFor(100) = wrTimeout, 'Foreign upgrader overlapped upgraded writer');
  U.LeaveWrite.SetEvent;
  Require(U.AfterWrite.WaitFor(1500) = wrSignaled, 'Nested writer release did not complete');
  Entered(R2, 'Ordinary reader did not resume after nested writer release');
  Require(U2.Entered.WaitFor(100) = wrTimeout, 'ExitWrite also released the outer upgradeable lock');
  U.ReleaseLock.SetEvent;
  Entered(U2, 'Balanced outer upgradeable release did not admit next upgrader');
end;

procedure TLockChecks.CheckUpgradeableException;
var U2, R: TLockActor; L, LWrite: TPCOperationsComp; LRaised: Boolean;
begin
  L := FNode.LockMempoolUpgradeableRead;
  try
    LRaised := False;
    try
      LWrite := FNode.LockMempoolWrite;
      try
        Require(LWrite = L, 'Exception fixture changed mempool during promotion');
        raise EExpectedUpgradeFailure.Create('Intentional work failure');
      finally FNode.UnlockMempoolWrite; end;
    except on E: EExpectedUpgradeFailure do LRaised := True; end;
    Require(LRaised, 'Exception cleanup fixture did not execute failing work');
    R := Actor(laRead); Entered(R, 'Exception cleanup left nested writer held');
    U2 := Actor(laUpgradeable);
    Require(U2.Entered.WaitFor(100) = wrTimeout, 'Exception cleanup discarded outer upgradeable ownership');
  finally FNode.UnlockMempoolUpgradeableRead; end;
  Entered(U2, 'Outer upgradeable release did not recover after work exception');
end;

procedure TLockChecks.CheckUpgradeableCancellation;
var U, W: TLockActor;
begin
  U := Actor(laUpgradeable); Entered(U, 'Cancelable upgradeable holder did not enter');
  W := Actor(laWrite);
  Require(W.Entered.WaitFor(100) = wrTimeout, 'Writer overlapped upgradeable holder');
  U.Terminate;
  Require(U.Completed.WaitFor(500) = wrSignaled, 'Upgradeable holder ignored cooperative termination');
  Require(U.ReleaseLock.WaitFor(0) <> wrSignaled, 'Cancellation test released upgradeable gate early');
  Entered(W, 'Canceled upgradeable holder leaked ownership and blocked writer');
end;

procedure TLockChecks.Run;
var I: Integer; Names: array[0..8] of String;
begin
  Names[0] := 'concurrent readers'; Names[1] := 'writer exclusion/progress';
  Names[2] := 'nested writer/read/write'; Names[3] := 'timed/nonblocking acquisition';
  Names[4] := 'cooperative cancellation';
  Names[5] := 'upgradeable and ordinary read sharing';
  Names[6] := 'upgradeable promotion and balanced nested release';
  Names[7] := 'upgraded work exception cleanup';
  Names[8] := 'upgradeable cooperative cancellation';
  for I := 0 to 8 do begin
    Log('START ' + Names[I]);
    try
      case I of
        0: CheckReaders;
        1: CheckExclusion;
        2: CheckNestedWriter;
        3: CheckTryAcquisition;
        4: CheckCancellation;
        5: CheckUpgradeableSharing;
        6: CheckUpgradeablePromotion;
        7: CheckUpgradeableException;
        8: CheckUpgradeableCancellation;
      end;
    finally StopActors; end;
    Require(FCounters.Violations = 0, 'Actual lock allowed forbidden reader/writer overlap');
    Require((FCounters.Readers = 0) and (FCounters.Writers = 0), 'Unbalanced actor lock release');
    Log('PASS ' + Names[I]);
  end;
end;

function RunMempoolLockChecks(const ALogPath: String): Integer;
var Node: TNode; NetData: TNetData; Checks: TLockChecks; DirectoryName, PreviousDirectory: String;
  ID: TGUID; BeforeThreads: Integer;
begin
  Result := 1; Node := nil; NetData := nil; Checks := nil;
  if FileExists(ALogPath) then raise Exception.Create('Refusing to append an earlier probe result');
  if TNode.NodeExists or TNetData.NetDataExists then
    raise Exception.Create('Probe requires fresh node/network singletons');
  PreviousDirectory := TNode.GetPascalCoinDataFolder;
  CreateGUID(ID);
  DirectoryName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'PascalCoinMempoolLock-' + GUIDToString(ID);
  if DirectoryExists(DirectoryName) then raise Exception.Create('Refusing existing test data');
  ForceDirectories(DirectoryName);
  TNode.SetPascalCoinDataFolder(DirectoryName);
  BeforeThreads := TPCThread.ThreadCount;
  try
    TCrypto.InitCrypto;
    Checks := TLockChecks.Create(nil, ALogPath);
    Checks.Log('START published operation hash readiness');
    CheckPublishedOperationReadHashes;
    Checks.Log('PASS published operation hash readiness');
    Checks.Log('START cached hash-tree lock synchronization');
    CheckCachedTreeReadTakesExistingLock;
    Checks.Log('PASS cached hash-tree lock synchronization');
    Checks.Log('START parallel operation/hash-tree read consistency');
    CheckParallelOperationReadHashes;
    Checks.Log('PASS parallel operation/hash-tree read consistency');
    FreeAndNil(Checks);
    Node := TNode.Create(nil);
    Node.BroadcastData := False;
    NetData := TNetData.NetData;
    NetData.NetConnectionsActive := False;
    if Node.NetServer.Active or (NetData.NetStatistics.ActiveConnections <> 0) then
      raise Exception.Create('Fresh lock checks must not enable listeners or peers');
    Checks := TLockChecks.Create(Node, ALogPath);
    Checks.Log('Actual Node lock checks; generated empty data; no listeners, peers or broadcasts');
    if SameText(GetEnvironmentVariable('PASCAL_MEMPOOL_COMPARISON'), 'exclusive') then
      Checks.Log('Reconstructed exclusive-lock comparison, not a historical source snapshot');
    try
      Checks.Run;
      Checks.Log('START actual miner-template conditional upgrade');
      CheckMinerTemplateUpgrades(Node);
      Checks.Log('PASS actual miner-template conditional upgrade');
      Result := 0;
    except on E: Exception do Checks.Log('FAIL ' + E.ClassName + ': ' + E.Message); end;
  finally
    Checks.Free;
    NetData.Free;
    Node.Free;
    TNode.SetPascalCoinDataFolder(PreviousDirectory);
  end;
  if Result = 0 then begin
    Checks := TLockChecks.Create(nil, ALogPath);
    try
      Checks.Require(TPCThread.ThreadCount = BeforeThreads, 'Owned node/actors remained after joined shutdown');
      Checks.Log('PASS joined node shutdown; all actual lock actors and core node worker released');
    finally Checks.Free; end;
  end;
end;

end.
