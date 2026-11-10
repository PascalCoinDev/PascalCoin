unit UConcurrency;

{ Copyright © 2026 Herman Schoenfeld <https://www.sphere10.com>
  Distributed under the MIT software license, see the accompanying LICENSE.

  Protects shared memory with multiple concurrent readers or one exclusive
  writer. One upgradeable reader can reserve a later write; timed acquisition
  and optional recursion control contention. Waiters sleep on RTL events.
  Ownership semantics follow System.Threading.ReaderWriterLockSlim.
}

{ Public types in this unit

  Type                          | Purpose
  ------------------------------+-------------------------------------------------------------------------
  ELockDisposedException        | Reports access to a disposed lock.
  ELockRecursionException       | Reports prohibited recursive acquisition or promotion.
  ESynchronizationLockException | Reports release without the required thread ownership.
  TLockRecursionPolicy          | Allows or rejects recursive lock acquisition.
  TReadWriteLock                | Grants concurrent reads or one write, with upgrades and timeouts.
}

{$IFDEF FPC}{$MODE DELPHI}{$H+}{$ENDIF}

interface

uses Classes, SysUtils, SyncObjs, Generics.Collections, UCommon;

type
  TLockRecursionPolicy = (lrpNoRecursion, lrpSupportsRecursion);
  ELockRecursionException = class(Exception);
  ESynchronizationLockException = class(Exception);
  ELockDisposedException = class(Exception);

  { All acquisitions and releases are thread-affine. Ordinary read ownership
    cannot be promoted: acquire upgradeable read before a possible write.
    Stop/join consumers before Free. Dispose rejects outstanding ownership or
    waiters; it never releases another thread's lock or terminates a thread. }
  TReadWriteLock = class
  private type
    TLockKind = (lkRead, lkWrite, lkUpgradeable);
    TThreadCounts = record
      Reads, Writes, Upgrades: Integer;
    end;
    TLockWaiter = class
      Kind: TLockKind;
      Signal: TEvent;
      constructor Create(AKind: TLockKind);
      destructor Destroy; override;
    end;
  private
    // This gate protects ownership/admission bookkeeping only. The caller's
    // resource stays protected by the granted ownership after the gate is left.
    FGate: TCriticalSection;
    FThreads: TDictionary<TThreadID, TThreadCounts>;
    FWaiters: TList<TLockWaiter>;
    FPolicy: TLockRecursionPolicy;
    // Reserving upgradeable ownership for one thread prevents competing readers
    // from each waiting to promote while preventing the other from completing.
    FWriter, FUpgrader: TThreadID;
    // Readers counts distinct explicit read owners, not recursive acquisitions.
    // Upgradeable-only access is tracked separately by FUpgrader.
    FReaders, FWaitingReaders, FWaitingWriters, FWaitingUpgraders: Integer;
    FDisposed: Boolean;
    function ThreadCounts(AThreadID: TThreadID): TThreadCounts;
    procedure CheckAcquisition(AKind: TLockKind; const ACounts: TThreadCounts);
    function CanAcquire(AKind: TLockKind; AThreadID: TThreadID; const ACounts: TThreadCounts): Boolean;
    procedure Grant(AKind: TLockKind; AThreadID: TThreadID; var ACounts: TThreadCounts);
    function TryAcquire(AKind: TLockKind; AMillisecondsTimeout: Integer): Boolean;
    procedure ReleaseLock(AKind: TLockKind);
    procedure ChangeWaitCount(AKind: TLockKind; ADelta: Integer);
    procedure WakeWaiters;
    function GetHeld(AKind: TLockKind): Boolean;
    function GetRecursiveCount(AKind: TLockKind): Integer;
    function GetCount(AIndex: Integer): Integer;
    function GetIsReadLockHeld: Boolean;
    function GetIsWriteLockHeld: Boolean;
    function GetIsUpgradeableReadLockHeld: Boolean;
    function GetRecursiveReadCount: Integer;
    function GetRecursiveWriteCount: Integer;
    function GetRecursiveUpgradeCount: Integer;
    function GetCurrentReadCount: Integer;
    function GetWaitingReadCount: Integer;
    function GetWaitingWriteCount: Integer;
    function GetWaitingUpgradeCount: Integer;
  public
    constructor Create(ARecursionPolicy: TLockRecursionPolicy = lrpNoRecursion);
    destructor Destroy; override;
    procedure Dispose;
    // Keep the returned managed record in a local variable. Each scope owns
    // one acquisition; copies retain it until the last copy is finalized.
    // The lock must outlive every scope, all on the acquiring thread.
    function EnterReadScope: TScope;
    function EnterWriteScope: TScope;
    function EnterUpgradeableReadScope: TScope;
    // Pair every successful Acquire/TryAcquire with the matching Release on
    // the same thread. Promotion adds a write acquisition to upgradeable read.
    procedure AcquireReadLock;
    function TryAcquireReadLock(AMillisecondsTimeout: Integer): Boolean;
    procedure ReleaseReadLock;
    procedure AcquireWriteLock;
    function TryAcquireWriteLock(AMillisecondsTimeout: Integer): Boolean;
    procedure ReleaseWriteLock;
    procedure AcquireUpgradeableReadLock;
    function TryAcquireUpgradeableReadLock(AMillisecondsTimeout: Integer): Boolean;
    procedure ReleaseUpgradeableReadLock;
    property RecursionPolicy: TLockRecursionPolicy read FPolicy;
    property IsReadLockHeld: Boolean read GetIsReadLockHeld;
    property IsWriteLockHeld: Boolean read GetIsWriteLockHeld;
    property IsUpgradeableReadLockHeld: Boolean read GetIsUpgradeableReadLockHeld;
    property RecursiveReadCount: Integer read GetRecursiveReadCount;
    property RecursiveWriteCount: Integer read GetRecursiveWriteCount;
    property RecursiveUpgradeCount: Integer read GetRecursiveUpgradeCount;
    // Counts distinct explicit read owners, excluding upgradeable-only access.
    property CurrentReadCount: Integer read GetCurrentReadCount;
    property WaitingReadCount: Integer read GetWaitingReadCount;
    property WaitingWriteCount: Integer read GetWaitingWriteCount;
    property WaitingUpgradeCount: Integer read GetWaitingUpgradeCount;
  end;

implementation

constructor TReadWriteLock.Create(ARecursionPolicy: TLockRecursionPolicy);
begin
  inherited Create;
  FPolicy := ARecursionPolicy;
  FGate := TCriticalSection.Create;
  FThreads := TDictionary<TThreadID, TThreadCounts>.Create;
  FWaiters := TList<TLockWaiter>.Create;
end;

destructor TReadWriteLock.Destroy;
begin
  if Assigned(FGate) then Dispose;
  FWaiters.Free;
  FThreads.Free;
  FGate.Free;
  inherited;
end;

procedure TReadWriteLock.Dispose;
begin
  FGate.Acquire;
  try
    if FDisposed then Exit;
    // Consumers must be stopped/joined by their owner before disposal. Reject
    // active use without changing it; never unlock another thread or join it here.
    if (Assigned(FThreads) and (FThreads.Count <> 0)) or
      (Assigned(FWaiters) and (FWaiters.Count <> 0)) then
      raise ESynchronizationLockException.Create('Cannot dispose a lock with owners or waiters.');
    FDisposed := True;
  finally
    FGate.Release;
  end;
end;

function TReadWriteLock.EnterReadScope: TScope;
var Scope: TScope;
begin
  AcquireReadLock;
  try
    Scope := TScope.Create(ReleaseReadLock);
  except
    // Scope creation did not adopt this acquisition. Unwind it exactly once.
    ReleaseReadLock;
    raise;
  end;
  // Assignment to the caller can itself release a previous scope. Keep it
  // outside the construction handler: Scope now owns this acquisition.
  Result := Scope;
end;

function TReadWriteLock.EnterWriteScope: TScope;
var Scope: TScope;
begin
  AcquireWriteLock;
  try
    Scope := TScope.Create(ReleaseWriteLock);
  except
    ReleaseWriteLock;
    raise;
  end;
  Result := Scope;
end;

function TReadWriteLock.EnterUpgradeableReadScope: TScope;
var Scope: TScope;
begin
  AcquireUpgradeableReadLock;
  try
    Scope := TScope.Create(ReleaseUpgradeableReadLock);
  except
    ReleaseUpgradeableReadLock;
    raise;
  end;
  Result := Scope;
end;

procedure TReadWriteLock.AcquireReadLock;
begin
  TryAcquireReadLock(-1);
end;

function TReadWriteLock.TryAcquireReadLock(AMillisecondsTimeout: Integer): Boolean;
begin
  Result := TryAcquire(lkRead, AMillisecondsTimeout);
end;

procedure TReadWriteLock.ReleaseReadLock;
begin
  ReleaseLock(lkRead);
end;

procedure TReadWriteLock.AcquireWriteLock;
begin
  TryAcquireWriteLock(-1);
end;

function TReadWriteLock.TryAcquireWriteLock(AMillisecondsTimeout: Integer): Boolean;
begin
  Result := TryAcquire(lkWrite, AMillisecondsTimeout);
end;

procedure TReadWriteLock.ReleaseWriteLock;
begin
  ReleaseLock(lkWrite);
end;

procedure TReadWriteLock.AcquireUpgradeableReadLock;
begin
  TryAcquireUpgradeableReadLock(-1);
end;

function TReadWriteLock.TryAcquireUpgradeableReadLock(AMillisecondsTimeout: Integer): Boolean;
begin
  Result := TryAcquire(lkUpgradeable, AMillisecondsTimeout);
end;

procedure TReadWriteLock.ReleaseUpgradeableReadLock;
begin
  ReleaseLock(lkUpgradeable);
end;

function MonotonicMilliseconds: UInt64;
begin
  {$IFDEF FPC}Result := GetTickCount64;{$ELSE}Result := TThread.GetTickCount64;{$ENDIF}
end;

constructor TReadWriteLock.TLockWaiter.Create(AKind: TLockKind);
begin
  inherited Create;
  Kind := AKind;
  Signal := TEvent.Create(nil, False, False, '');
end;

destructor TReadWriteLock.TLockWaiter.Destroy;
begin
  Signal.Free;
  inherited;
end;

function TReadWriteLock.ThreadCounts(AThreadID: TThreadID): TThreadCounts;
begin
  if not FThreads.TryGetValue(AThreadID, Result) then
    Result := Default(TThreadCounts);
end;

procedure TReadWriteLock.CheckAcquisition(AKind: TLockKind; const ACounts: TThreadCounts);
var LInvalid: Boolean;
begin
  if FDisposed then raise ELockDisposedException.Create('The lock is disposed.');
  // An ordinary reader cannot promote, even with recursion enabled: two such
  // readers would each wait for the other. Choose upgradeable mode in advance.
  if FPolicy = lrpNoRecursion then
    case AKind of
      lkRead: LInvalid := (ACounts.Reads > 0) or (ACounts.Writes > 0);
      lkWrite: LInvalid := (ACounts.Writes > 0) or (ACounts.Reads > 0);
      lkUpgradeable: LInvalid := (ACounts.Reads > 0) or (ACounts.Writes > 0) or
        (ACounts.Upgrades > 0);
    end
  else
    LInvalid := (AKind <> lkRead) and (ACounts.Reads > 0) and
      (ACounts.Writes = 0) and (ACounts.Upgrades = 0);
  if LInvalid then
    raise ELockRecursionException.Create('This acquisition violates the lock recursion policy.');
end;

function TReadWriteLock.CanAcquire(AKind: TLockKind; AThreadID: TThreadID; const ACounts: TThreadCounts): Boolean;
begin
  // RTL thread IDs may be integers or pointers; use their typed empty value.
  // Existing owners can recurse without queuing behind a writer that needs
  // them to finish. New readers/upgraders wait once any writer has queued.
  case AKind of
    lkRead:
      Result := (ACounts.Reads > 0) or (ACounts.Writes > 0) or
        (ACounts.Upgrades > 0) or ((FWriter = Default(TThreadID)) and (FWaitingWriters = 0));
    lkWrite:
      // Only the reserved upgrader may discount its own explicit read ownership;
      // all other readers must leave before that thread can acquire write mode.
      Result := (ACounts.Writes > 0) or ((FWriter = Default(TThreadID)) and
        ((FUpgrader = Default(TThreadID)) or (FUpgrader = AThreadID)) and
        (FReaders = Ord(ACounts.Reads > 0)));
    lkUpgradeable:
      Result := (ACounts.Upgrades > 0) or (ACounts.Writes > 0) or
        ((FUpgrader = Default(TThreadID)) and (FWriter = Default(TThreadID)) and (FWaitingWriters = 0));
  else
    Result := False;
  end;
end;

procedure TReadWriteLock.Grant(AKind: TLockKind; AThreadID: TThreadID; var ACounts: TThreadCounts);
var LFirst: Boolean;
begin
  LFirst := False;
  case AKind of
    lkRead: begin
      LFirst := ACounts.Reads = 0;
      Inc(ACounts.Reads);
    end;
    lkWrite: Inc(ACounts.Writes);
    lkUpgradeable: Inc(ACounts.Upgrades);
  end;
  // Publish counters only after the dictionary allocation has succeeded.
  FThreads.AddOrSetValue(AThreadID, ACounts);
  case AKind of
    lkRead: if LFirst then Inc(FReaders);
    lkWrite: FWriter := AThreadID;
    lkUpgradeable: FUpgrader := AThreadID;
  end;
end;

procedure TReadWriteLock.ChangeWaitCount(AKind: TLockKind; ADelta: Integer);
begin
  case AKind of
    lkRead: Inc(FWaitingReaders, ADelta);
    lkWrite: Inc(FWaitingWriters, ADelta);
    lkUpgradeable: Inc(FWaitingUpgraders, ADelta);
  end;
end;

procedure TReadWriteLock.WakeWaiters;
var LWaiter: TLockWaiter;
begin
  // Per-waiter auto-reset events avoid lost wakes and repeated wakes without a
  // state change. Admission is rechecked under the gate, with writer priority.
  for LWaiter in FWaiters do LWaiter.Signal.SetEvent;
end;

function TReadWriteLock.TryAcquire(AKind: TLockKind; AMillisecondsTimeout: Integer): Boolean;
var
  LThreadID: TThreadID;
  LCounts: TThreadCounts;
  LWaiter: TLockWaiter;
  LStarted, LElapsed: UInt64;
  LRemaining: Cardinal;
  LWaitResult: TWaitResult;
begin
  if AMillisecondsTimeout < -1 then
    raise EArgumentOutOfRangeException.Create('Timeout must be -1 or a nonnegative number of milliseconds.');
  // Keep the original deadline across state-change wakes; contention must not
  // restart a caller's finite timeout. This clock is independent of wall time.
  LStarted := MonotonicMilliseconds;
  LThreadID := TThread.CurrentThread.ThreadID;
  LWaiter := nil;
  FGate.Acquire;
  try
    LCounts := ThreadCounts(LThreadID);
    CheckAcquisition(AKind, LCounts);
    while True do begin
      if CanAcquire(AKind, LThreadID, LCounts) then begin
        Grant(AKind, LThreadID, LCounts);
        Exit(True);
      end;
      if AMillisecondsTimeout = 0 then Exit(False);
      if AMillisecondsTimeout = -1 then LRemaining := High(Cardinal)
      else begin
        LElapsed := MonotonicMilliseconds - LStarted;
        if LElapsed >= UInt64(AMillisecondsTimeout) then Exit(False);
        LRemaining := Cardinal(UInt64(AMillisecondsTimeout) - LElapsed);
      end;
      if not Assigned(LWaiter) then begin
        LWaiter := TLockWaiter.Create(AKind);
        try
          FWaiters.Add(LWaiter);
        except
          FreeAndNil(LWaiter);
          raise;
        end;
        ChangeWaitCount(AKind, 1);
      end;
      // No protected resource or admission lock is held during an event wait.
      FGate.Release;
      try
        LWaitResult := LWaiter.Signal.WaitFor(LRemaining);
      finally
        FGate.Acquire;
      end;
      if LWaitResult = wrTimeout then Exit(False);
      if LWaitResult <> wrSignaled then
        raise ESynchronizationLockException.Create('Waiting for lock ownership failed.');
      LCounts := ThreadCounts(LThreadID);
    end;
  finally
    if Assigned(LWaiter) then begin
      // Remove registration before freeing its event, while the gate prevents
      // a releaser from signalling that event. Failure never grants ownership.
      FWaiters.Remove(LWaiter);
      ChangeWaitCount(AKind, -1);
      // A timed-out writer must not keep new readers asleep indefinitely.
      WakeWaiters;
    end;
    FGate.Release;
    LWaiter.Free;
  end;
end;

procedure TReadWriteLock.ReleaseLock(AKind: TLockKind);
var LThreadID: TThreadID; LCounts: TThreadCounts; LCount: Integer;
begin
  // Every typed Release balances ONE same-thread acquisition of that mode. In
  // particular, ReleaseWriteLock after promotion retains the surrounding upgrader.
  LThreadID := TThread.CurrentThread.ThreadID;
  FGate.Acquire;
  try
    LCounts := ThreadCounts(LThreadID);
    case AKind of
      lkRead: LCount := LCounts.Reads;
      lkWrite: LCount := LCounts.Writes;
      lkUpgradeable: LCount := LCounts.Upgrades;
    else LCount := 0;
    end;
    if LCount = 0 then
      raise ESynchronizationLockException.Create('This thread does not hold the requested lock.');
    case AKind of
      lkRead: begin
        Dec(LCounts.Reads);
        if LCounts.Reads = 0 then Dec(FReaders);
      end;
      lkWrite: begin
        Dec(LCounts.Writes);
        if LCounts.Writes = 0 then FWriter := Default(TThreadID);
      end;
      lkUpgradeable: begin
        Dec(LCounts.Upgrades);
        if LCounts.Upgrades = 0 then FUpgrader := Default(TThreadID);
      end;
    end;
    if (LCounts.Reads = 0) and (LCounts.Writes = 0) and (LCounts.Upgrades = 0) then
      FThreads.Remove(LThreadID)
    else
      FThreads.AddOrSetValue(LThreadID, LCounts);
    WakeWaiters;
  finally
    FGate.Release;
  end;
end;

function TReadWriteLock.GetRecursiveCount(AKind: TLockKind): Integer;
var LCounts: TThreadCounts;
begin
  FGate.Acquire;
  try
    LCounts := ThreadCounts(TThread.CurrentThread.ThreadID);
    case AKind of
      lkRead: Result := LCounts.Reads;
      lkWrite: Result := LCounts.Writes;
      lkUpgradeable: Result := LCounts.Upgrades;
    else Result := 0;
    end;
  finally
    FGate.Release;
  end;
end;

function TReadWriteLock.GetHeld(AKind: TLockKind): Boolean;
begin
  Result := GetRecursiveCount(AKind) > 0;
end;

function TReadWriteLock.GetCount(AIndex: Integer): Integer;
begin
  FGate.Acquire;
  try
    case AIndex of
      0: Result := FReaders;
      1: Result := FWaitingReaders;
      2: Result := FWaitingWriters;
      3: Result := FWaitingUpgraders;
    else Result := 0;
    end;
  finally
    FGate.Release;
  end;
end;

function TReadWriteLock.GetIsReadLockHeld: Boolean;
begin
  Result := GetHeld(lkRead);
end;

function TReadWriteLock.GetIsWriteLockHeld: Boolean;
begin
  Result := GetHeld(lkWrite);
end;

function TReadWriteLock.GetIsUpgradeableReadLockHeld: Boolean;
begin
  Result := GetHeld(lkUpgradeable);
end;

function TReadWriteLock.GetRecursiveReadCount: Integer;
begin
  Result := GetRecursiveCount(lkRead);
end;

function TReadWriteLock.GetRecursiveWriteCount: Integer;
begin
  Result := GetRecursiveCount(lkWrite);
end;

function TReadWriteLock.GetRecursiveUpgradeCount: Integer;
begin
  Result := GetRecursiveCount(lkUpgradeable);
end;

function TReadWriteLock.GetCurrentReadCount: Integer;
begin
  Result := GetCount(0);
end;

function TReadWriteLock.GetWaitingReadCount: Integer;
begin
  Result := GetCount(1);
end;

function TReadWriteLock.GetWaitingWriteCount: Integer;
begin
  Result := GetCount(2);
end;

function TReadWriteLock.GetWaitingUpgradeCount: Integer;
begin
  Result := GetCount(3);
end;

end.
