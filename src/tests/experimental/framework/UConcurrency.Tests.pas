unit UConcurrency.Tests;

{ Copyright © 2026 Herman Schoenfeld <https://www.sphere10.com>
  Distributed under the MIT software license, see the accompanying LICENSE. }

{$MODE DELPHI}{$H+}

interface

uses Classes, SysUtils, SyncObjs, Generics.Collections, fpcunit, testregistry,
  UConcurrency;

type
  TLockAction = (laRead, laWrite, laUpgrade, laReleaseRead, laReleaseWrite,
    laReleaseUpgrade, laCancellableRead, laUpgradeThenWrite);

  TLockWorker = class(TThread)
  private
    FLock: TReadWriteLock;
    FAction: TLockAction;
    FTimeout: Integer;
  protected
    procedure Execute; override;
  public
    Entered, ReleaseGate, Done: TEvent;
    Acquired: Boolean;
    ErrorClass, ErrorMessage: String;
    Elapsed: UInt64;
    OwnReadCount, OwnWriteCount, OwnUpgradeCount: Integer;
    constructor Create(ALock: TReadWriteLock; AAction: TLockAction; ATimeout: Integer);
    destructor Destroy; override;
    procedure RequestStop;
  end;

  TReadWriteLockTests = class(TTestCase)
  private
    FLock: TReadWriteLock;
    FWorkers: TList<TLockWorker>;
    function Worker(AAction: TLockAction; ATimeout: Integer = 2500): TLockWorker;
    procedure Await(AEvent: TEvent; const ADescription: String);
    procedure WaitCount(AKind, ACount: Integer);
    procedure Completed(AWorker: TLockWorker);
    procedure Release(AWorker: TLockWorker);
    procedure ExpectRecursion(AAction: TLockAction);
    procedure RecursivePolicy;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DefaultPolicyAndEmptyCounts;
    procedure DifferentThreadsReadConcurrently;
    procedure WriterExcludesOtherReadersWritersAndUpgraders;
    procedure WaitingWriterPrecedesNewReader;
    procedure WaitingWriterPrecedesNewUpgrader;
    procedure RecursiveReadsCountOneReader;
    procedure RecursiveWriterCanReadWriteAndUpgrade;
    procedure NoRecursionRejectsRepeatedAcquisition;
    procedure OrdinaryReadCannotPromoteUnderEitherPolicy;
    procedure NoRecursionUpgradeableReadThenWriteIsRejected;
    procedure UpgradeableReadPromotesWithoutOtherReaders;
    procedure RecursiveUpgradeableWithExplicitReadCanPromote;
    procedure UpgradeWaitsForOtherReaderAndRetainsOwnershipOnTimeout;
    procedure WaitingUpgradePromotesWhenOtherReaderReleases;
    procedure OnlyOneUpgradeableOwnerAndReadersCanShare;
    procedure WriteToReadDowngradeRetainsReader;
    procedure UpgradeableToReadDowngradeRetainsReader;
    procedure FiniteTimeoutIsBoundedAndRemovesWaiter;
    procedure ZeroTimeoutDoesNotRegisterWaiter;
    procedure InfiniteTimeoutWakesOnRelease;
    procedure TimedOutWriterReleasesWaitingReaders;
    procedure WrongThreadCannotReleaseAnyMode;
    procedure UnmatchedReleaseDoesNotChangeOwnership;
    procedure InvalidTimeoutDoesNotAcquire;
    procedure DisposeRejectsActiveOwnershipAndCanBeRetried;
    procedure DisposeRejectsWaiterAndWaiterLaterCompletes;
    procedure DisposedLockRejectsAcquisitions;
    procedure ExceptionCleanupReleasesOnlyOwnedAcquisitions;
    procedure PromotedWriteExceptionRetainsSurroundingUpgradeableLock;
    procedure HeldAndRecursivePropertiesAreThreadSpecific;
    procedure FiniteWaitPermitsCooperativeCancellation;
  end;

implementation

function Tick: UInt64;
begin
  Result := GetTickCount64;
end;

constructor TLockWorker.Create(ALock: TReadWriteLock; AAction: TLockAction; ATimeout: Integer);
begin
  inherited Create(True);
  FLock := ALock;
  FAction := AAction;
  FTimeout := ATimeout;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseGate := TEvent.Create(nil, True, False, '');
  Done := TEvent.Create(nil, True, False, '');
  Start;
end;

destructor TLockWorker.Destroy;
begin
  RequestStop;
  WaitFor;
  Done.Free;
  ReleaseGate.Free;
  Entered.Free;
  inherited;
end;

procedure TLockWorker.RequestStop;
begin
  Terminate;
  ReleaseGate.SetEvent;
end;

procedure TLockWorker.Execute;
var LStarted: UInt64;
begin
  LStarted := Tick;
  try
    try
      case FAction of
        laRead: Acquired := FLock.TryAcquireReadLock(FTimeout);
        laWrite: Acquired := FLock.TryAcquireWriteLock(FTimeout);
        laUpgrade: Acquired := FLock.TryAcquireUpgradeableReadLock(FTimeout);
        laReleaseRead: FLock.ReleaseReadLock;
        laReleaseWrite: FLock.ReleaseWriteLock;
        laReleaseUpgrade: FLock.ReleaseUpgradeableReadLock;
        laCancellableRead:
          while not Terminated do
            if FLock.TryAcquireReadLock(FTimeout) then begin
              FLock.ReleaseReadLock;
              Break;
            end;
        laUpgradeThenWrite: begin
          FLock.AcquireUpgradeableReadLock;
          try
            Entered.SetEvent;
            if ReleaseGate.WaitFor(4000) <> wrSignaled then
              raise Exception.Create('Promotion start gate timed out.');
            Acquired := FLock.TryAcquireWriteLock(FTimeout);
            if Acquired then FLock.ReleaseWriteLock;
          finally FLock.ReleaseUpgradeableReadLock; end;
        end;
      end;
      if Acquired and (FAction in [laRead, laWrite, laUpgrade]) then begin
        try
          OwnReadCount := FLock.RecursiveReadCount;
          OwnWriteCount := FLock.RecursiveWriteCount;
          OwnUpgradeCount := FLock.RecursiveUpgradeCount;
          Entered.SetEvent;
          if ReleaseGate.WaitFor(4000) <> wrSignaled then
            raise Exception.Create('Test release gate timed out.');
        finally
          case FAction of
            laRead: FLock.ReleaseReadLock;
            laWrite: FLock.ReleaseWriteLock;
            laUpgrade: FLock.ReleaseUpgradeableReadLock;
          end;
        end;
      end;
    except
      on E: Exception do begin
        ErrorClass := E.ClassName;
        ErrorMessage := E.Message;
      end;
    end;
  finally
    Elapsed := Tick - LStarted;
    Done.SetEvent;
  end;
end;

procedure TReadWriteLockTests.SetUp;
begin
  inherited;
  FLock := TReadWriteLock.Create;
  FWorkers := TList<TLockWorker>.Create;
end;

procedure TReadWriteLockTests.TearDown;
var LWorker: TLockWorker;
begin
  for LWorker in FWorkers do LWorker.RequestStop;
  // Release test-thread ownership before joining blocked workers after a failure.
  while FLock.IsWriteLockHeld do FLock.ReleaseWriteLock;
  while FLock.IsUpgradeableReadLockHeld do FLock.ReleaseUpgradeableReadLock;
  while FLock.IsReadLockHeld do FLock.ReleaseReadLock;
  for LWorker in FWorkers do LWorker.Free;
  FWorkers.Free;
  FLock.Free;
  inherited;
end;

procedure TReadWriteLockTests.RecursivePolicy;
begin
  FLock.Free;
  FLock := TReadWriteLock.Create(lrpSupportsRecursion);
end;

function TReadWriteLockTests.Worker(AAction: TLockAction; ATimeout: Integer): TLockWorker;
begin
  Result := TLockWorker.Create(FLock, AAction, ATimeout);
  FWorkers.Add(Result);
end;

procedure TReadWriteLockTests.Await(AEvent: TEvent; const ADescription: String);
begin
  AssertEquals(ADescription, Ord(wrSignaled), Ord(AEvent.WaitFor(3000)));
end;

procedure TReadWriteLockTests.WaitCount(AKind, ACount: Integer);
var LStarted: UInt64; LCount: Integer;
begin
  LStarted := Tick;
  repeat
    case AKind of
      0: LCount := FLock.WaitingReadCount;
      1: LCount := FLock.WaitingWriteCount;
    else LCount := FLock.WaitingUpgradeCount;
    end;
    if LCount = ACount then Exit;
    Sleep(1);
  until Tick - LStarted >= 2000;
  AssertEquals('Actual registered waiter count', ACount, LCount);
end;

procedure TReadWriteLockTests.Completed(AWorker: TLockWorker);
begin
  Await(AWorker.Done, 'Worker completed');
  AssertEquals(AWorker.ErrorMessage, '', AWorker.ErrorClass);
end;

procedure TReadWriteLockTests.Release(AWorker: TLockWorker);
begin
  AWorker.ReleaseGate.SetEvent;
  Completed(AWorker);
end;

procedure TReadWriteLockTests.ExpectRecursion(AAction: TLockAction);
begin
  try
    case AAction of
      laRead: FLock.TryAcquireReadLock(0);
      laWrite: FLock.TryAcquireWriteLock(0);
      laUpgrade: FLock.TryAcquireUpgradeableReadLock(0);
    end;
    Fail('This recursion/promotion should be rejected.');
  except on E: ELockRecursionException do ; end;
end;

procedure TReadWriteLockTests.DefaultPolicyAndEmptyCounts;
begin
  AssertEquals(Ord(lrpNoRecursion), Ord(FLock.RecursionPolicy));
  AssertFalse(FLock.IsReadLockHeld or FLock.IsWriteLockHeld or FLock.IsUpgradeableReadLockHeld);
  AssertEquals(0, FLock.CurrentReadCount);
  AssertEquals(0, FLock.WaitingReadCount + FLock.WaitingWriteCount + FLock.WaitingUpgradeCount);
end;

procedure TReadWriteLockTests.DifferentThreadsReadConcurrently;
var LReader: TLockWorker;
begin
  FLock.AcquireReadLock;
  LReader := Worker(laRead);
  Await(LReader.Entered, 'Second reader acquires while first still owns');
  AssertEquals(2, FLock.CurrentReadCount);
  AssertEquals(1, FLock.RecursiveReadCount);
  FLock.ReleaseReadLock;
  Release(LReader);
  AssertEquals(0, FLock.CurrentReadCount);
end;

procedure TReadWriteLockTests.WriterExcludesOtherReadersWritersAndUpgraders;
var LAction: TLockAction; LWorker: TLockWorker;
begin
  FLock.AcquireWriteLock;
  for LAction := laRead to laUpgrade do begin
    LWorker := Worker(LAction, 0);
    Completed(LWorker);
    AssertFalse('Writer excludes other owners', LWorker.Acquired);
  end;
  AssertTrue(FLock.IsWriteLockHeld);
  AssertEquals(0, FLock.WaitingReadCount + FLock.WaitingWriteCount + FLock.WaitingUpgradeCount);
  FLock.ReleaseWriteLock;
end;

procedure TReadWriteLockTests.WaitingWriterPrecedesNewReader;
var LWriter, LReader: TLockWorker;
begin
  FLock.AcquireReadLock;
  LWriter := Worker(laWrite);
  WaitCount(1, 1);
  LReader := Worker(laRead);
  WaitCount(0, 1);
  FLock.ReleaseReadLock;
  Await(LWriter.Entered, 'Waiting writer wins admission');
  AssertEquals(Ord(wrTimeout), Ord(LReader.Entered.WaitFor(0)));
  Release(LWriter);
  Await(LReader.Entered, 'Reader follows completed writer');
  Release(LReader);
end;

procedure TReadWriteLockTests.WaitingWriterPrecedesNewUpgrader;
var LWriter, LUpgrade: TLockWorker;
begin
  FLock.AcquireReadLock;
  LWriter := Worker(laWrite);
  WaitCount(1, 1);
  LUpgrade := Worker(laUpgrade);
  WaitCount(2, 1);
  FLock.ReleaseReadLock;
  Await(LWriter.Entered, 'Writer precedes new upgrader');
  AssertEquals(Ord(wrTimeout), Ord(LUpgrade.Entered.WaitFor(0)));
  Release(LWriter);
  Await(LUpgrade.Entered, 'Upgrader follows writer');
  Release(LUpgrade);
end;

procedure TReadWriteLockTests.RecursiveReadsCountOneReader;
begin
  RecursivePolicy;
  FLock.AcquireReadLock;
  FLock.AcquireReadLock;
  AssertEquals(2, FLock.RecursiveReadCount);
  AssertEquals(1, FLock.CurrentReadCount);
  FLock.ReleaseReadLock;
  AssertTrue(FLock.IsReadLockHeld);
  FLock.ReleaseReadLock;
  AssertEquals(0, FLock.CurrentReadCount);
end;

procedure TReadWriteLockTests.RecursiveWriterCanReadWriteAndUpgrade;
begin
  RecursivePolicy;
  FLock.AcquireWriteLock;
  FLock.AcquireReadLock;
  FLock.AcquireWriteLock;
  FLock.AcquireUpgradeableReadLock;
  FLock.AcquireUpgradeableReadLock;
  AssertEquals(2, FLock.RecursiveWriteCount);
  AssertEquals(1, FLock.RecursiveReadCount);
  AssertEquals(2, FLock.RecursiveUpgradeCount);
  FLock.ReleaseWriteLock;
  AssertTrue(FLock.IsWriteLockHeld);
  FLock.ReleaseReadLock;
  FLock.ReleaseUpgradeableReadLock;
  FLock.ReleaseWriteLock;
  AssertTrue(FLock.IsUpgradeableReadLockHeld);
  FLock.ReleaseUpgradeableReadLock;
end;

procedure TReadWriteLockTests.NoRecursionRejectsRepeatedAcquisition;
begin
  FLock.AcquireReadLock;
  ExpectRecursion(laRead);
  AssertEquals(1, FLock.RecursiveReadCount);
  FLock.ReleaseReadLock;
  FLock.AcquireWriteLock;
  ExpectRecursion(laWrite);
  ExpectRecursion(laRead);
  ExpectRecursion(laUpgrade);
  AssertEquals(1, FLock.RecursiveWriteCount);
  FLock.ReleaseWriteLock;
  FLock.AcquireUpgradeableReadLock;
  ExpectRecursion(laUpgrade);
  AssertEquals(1, FLock.RecursiveUpgradeCount);
  FLock.ReleaseUpgradeableReadLock;
end;

procedure TReadWriteLockTests.OrdinaryReadCannotPromoteUnderEitherPolicy;
var LPolicy: TLockRecursionPolicy;
begin
  for LPolicy := lrpNoRecursion to lrpSupportsRecursion do begin
    FLock.Free;
    FLock := TReadWriteLock.Create(LPolicy);
    FLock.AcquireReadLock;
    ExpectRecursion(laWrite);
    ExpectRecursion(laUpgrade);
    AssertEquals(1, FLock.RecursiveReadCount);
    FLock.ReleaseReadLock;
  end;
end;

procedure TReadWriteLockTests.NoRecursionUpgradeableReadThenWriteIsRejected;
begin
  FLock.AcquireUpgradeableReadLock;
  FLock.AcquireReadLock;
  ExpectRecursion(laWrite);
  FLock.ReleaseReadLock;
  AssertTrue(FLock.TryAcquireWriteLock(0));
  FLock.ReleaseWriteLock;
  FLock.ReleaseUpgradeableReadLock;
end;

procedure TReadWriteLockTests.UpgradeableReadPromotesWithoutOtherReaders;
begin
  FLock.AcquireUpgradeableReadLock;
  AssertEquals(0, FLock.CurrentReadCount);
  AssertTrue(FLock.TryAcquireWriteLock(0));
  AssertTrue(FLock.IsWriteLockHeld and FLock.IsUpgradeableReadLockHeld);
  FLock.ReleaseWriteLock;
  AssertTrue(FLock.IsUpgradeableReadLockHeld);
  FLock.ReleaseUpgradeableReadLock;
end;

procedure TReadWriteLockTests.RecursiveUpgradeableWithExplicitReadCanPromote;
begin
  RecursivePolicy;
  FLock.AcquireUpgradeableReadLock;
  FLock.AcquireReadLock;
  AssertTrue(FLock.TryAcquireWriteLock(0));
  FLock.ReleaseUpgradeableReadLock;
  AssertTrue(FLock.IsWriteLockHeld and FLock.IsReadLockHeld);
  FLock.ReleaseReadLock;
  FLock.ReleaseWriteLock;
end;

procedure TReadWriteLockTests.UpgradeWaitsForOtherReaderAndRetainsOwnershipOnTimeout;
var LReader: TLockWorker;
begin
  FLock.AcquireUpgradeableReadLock;
  LReader := Worker(laRead);
  Await(LReader.Entered, 'Reader shares upgradeable ownership');
  AssertFalse(FLock.TryAcquireWriteLock(70));
  AssertTrue(FLock.IsUpgradeableReadLockHeld);
  AssertFalse(FLock.IsWriteLockHeld);
  AssertEquals(0, FLock.WaitingWriteCount);
  Release(LReader);
  AssertTrue(FLock.TryAcquireWriteLock(2000));
  FLock.ReleaseWriteLock;
  FLock.ReleaseUpgradeableReadLock;
end;

procedure TReadWriteLockTests.OnlyOneUpgradeableOwnerAndReadersCanShare;
var LUpgrade, LReader: TLockWorker;
begin
  FLock.AcquireUpgradeableReadLock;
  LUpgrade := Worker(laUpgrade, 60);
  LReader := Worker(laRead);
  Await(LReader.Entered, 'Reader shares first upgrader');
  Completed(LUpgrade);
  AssertFalse(LUpgrade.Acquired);
  Release(LReader);
  FLock.ReleaseUpgradeableReadLock;
end;

procedure TReadWriteLockTests.WaitingUpgradePromotesWhenOtherReaderReleases;
var LUpgrade, LReader: TLockWorker;
begin
  FLock.AcquireReadLock;
  LUpgrade := Worker(laUpgradeThenWrite);
  Await(LUpgrade.Entered, 'Upgradeable read shares existing reader');
  LUpgrade.ReleaseGate.SetEvent;
  WaitCount(1, 1);
  LReader := Worker(laRead);
  WaitCount(0, 1);
  AssertEquals(Ord(wrTimeout), Ord(LUpgrade.Done.WaitFor(0)));
  FLock.ReleaseReadLock;
  Completed(LUpgrade);
  AssertTrue('Pending promotion succeeds after last other reader releases its lock', LUpgrade.Acquired);
  Await(LReader.Entered, 'New reader follows completed promotion');
  Release(LReader);
end;

procedure TReadWriteLockTests.WriteToReadDowngradeRetainsReader;
var LWriter, LReader: TLockWorker;
begin
  RecursivePolicy;
  FLock.AcquireWriteLock;
  FLock.AcquireReadLock;
  FLock.ReleaseWriteLock;
  AssertTrue(FLock.IsReadLockHeld);
  LWriter := Worker(laWrite, 0);
  Completed(LWriter);
  AssertFalse(LWriter.Acquired);
  LReader := Worker(laRead);
  Await(LReader.Entered, 'Downgrade permits another reader');
  Release(LReader);
  FLock.ReleaseReadLock;
end;

procedure TReadWriteLockTests.UpgradeableToReadDowngradeRetainsReader;
begin
  FLock.AcquireUpgradeableReadLock;
  FLock.AcquireReadLock;
  FLock.ReleaseUpgradeableReadLock;
  AssertTrue(FLock.IsReadLockHeld);
  ExpectRecursion(laUpgrade);
  FLock.ReleaseReadLock;
end;

procedure TReadWriteLockTests.FiniteTimeoutIsBoundedAndRemovesWaiter;
var LReader: TLockWorker;
begin
  FLock.AcquireWriteLock;
  LReader := Worker(laRead, 90);
  Completed(LReader);
  AssertFalse(LReader.Acquired);
  AssertTrue('Actually waited', LReader.Elapsed >= 75);
  AssertTrue('Bounded timeout', LReader.Elapsed < 1000);
  AssertEquals(0, FLock.WaitingReadCount);
  FLock.ReleaseWriteLock;
end;

procedure TReadWriteLockTests.ZeroTimeoutDoesNotRegisterWaiter;
var LWorker: TLockWorker;
begin
  FLock.AcquireReadLock;
  LWorker := Worker(laWrite, 0);
  Completed(LWorker);
  AssertFalse(LWorker.Acquired);
  AssertTrue(LWorker.Elapsed < 500);
  AssertEquals(0, FLock.WaitingWriteCount);
  FLock.ReleaseReadLock;
end;

procedure TReadWriteLockTests.InfiniteTimeoutWakesOnRelease;
var LReader: TLockWorker;
begin
  FLock.AcquireWriteLock;
  LReader := Worker(laRead, -1);
  WaitCount(0, 1);
  AssertEquals(Ord(wrTimeout), Ord(LReader.Done.WaitFor(0)));
  FLock.ReleaseWriteLock;
  Await(LReader.Entered, 'Infinite waiter wakes on release');
  Release(LReader);
end;

procedure TReadWriteLockTests.TimedOutWriterReleasesWaitingReaders;
var LWriter, LReader: TLockWorker;
begin
  FLock.AcquireReadLock;
  LWriter := Worker(laWrite, 160);
  WaitCount(1, 1);
  LReader := Worker(laRead);
  WaitCount(0, 1);
  Completed(LWriter);
  AssertFalse(LWriter.Acquired);
  Await(LReader.Entered, 'Writer timeout releases reader while original read remains held');
  AssertTrue(FLock.IsReadLockHeld);
  Release(LReader);
  FLock.ReleaseReadLock;
end;

procedure TReadWriteLockTests.WrongThreadCannotReleaseAnyMode;
var LAction: TLockAction; LWorker: TLockWorker;
begin
  for LAction := laReleaseRead to laReleaseUpgrade do begin
    case LAction of
      laReleaseRead: FLock.AcquireReadLock;
      laReleaseWrite: FLock.AcquireWriteLock;
      laReleaseUpgrade: FLock.AcquireUpgradeableReadLock;
    end;
    LWorker := Worker(LAction);
    Await(LWorker.Done, 'Wrong-thread release completed');
    AssertEquals('ESynchronizationLockException', LWorker.ErrorClass);
    case LAction of
      laReleaseRead: begin
        AssertTrue(FLock.IsReadLockHeld);
        FLock.ReleaseReadLock;
      end;
      laReleaseWrite: begin
        AssertTrue(FLock.IsWriteLockHeld);
        FLock.ReleaseWriteLock;
      end;
      laReleaseUpgrade: begin
        AssertTrue(FLock.IsUpgradeableReadLockHeld);
        FLock.ReleaseUpgradeableReadLock;
      end;
    end;
  end;
end;

procedure TReadWriteLockTests.UnmatchedReleaseDoesNotChangeOwnership;
begin
  FLock.AcquireReadLock;
  try FLock.ReleaseWriteLock; Fail('Unmatched release');
  except on E: ESynchronizationLockException do ; end;
  AssertTrue(FLock.IsReadLockHeld);
  FLock.ReleaseReadLock;
  try FLock.ReleaseReadLock; Fail('Extra release');
  except on E: ESynchronizationLockException do ; end;
  AssertEquals(0, FLock.CurrentReadCount);
end;

procedure TReadWriteLockTests.InvalidTimeoutDoesNotAcquire;
begin
  try FLock.TryAcquireReadLock(-2); Fail('Invalid read timeout');
  except on E: EArgumentOutOfRangeException do ; end;
  try FLock.TryAcquireWriteLock(-2); Fail('Invalid write timeout');
  except on E: EArgumentOutOfRangeException do ; end;
  try FLock.TryAcquireUpgradeableReadLock(-2); Fail('Invalid upgrade timeout');
  except on E: EArgumentOutOfRangeException do ; end;
  AssertEquals(0, FLock.CurrentReadCount);
end;

procedure TReadWriteLockTests.DisposeRejectsActiveOwnershipAndCanBeRetried;
begin
  FLock.AcquireReadLock;
  try FLock.Dispose; Fail('Dispose with owner');
  except on E: ESynchronizationLockException do ; end;
  AssertTrue(FLock.IsReadLockHeld);
  FLock.ReleaseReadLock;
  FLock.Dispose;
  FLock.Dispose;
  try FLock.AcquireReadLock; Fail('Disposed acquisition');
  except on E: ELockDisposedException do ; end;
end;

procedure TReadWriteLockTests.DisposeRejectsWaiterAndWaiterLaterCompletes;
var LWriter: TLockWorker;
begin
  FLock.AcquireReadLock;
  LWriter := Worker(laWrite);
  WaitCount(1, 1);
  try FLock.Dispose; Fail('Dispose with waiter');
  except on E: ESynchronizationLockException do ; end;
  AssertEquals(1, FLock.WaitingWriteCount);
  FLock.ReleaseReadLock;
  Await(LWriter.Entered, 'Rejected dispose leaves waiter usable');
  Release(LWriter);
  FLock.Dispose;
end;

procedure TReadWriteLockTests.DisposedLockRejectsAcquisitions;
begin
  FLock.Dispose;
  try FLock.AcquireReadLock; Fail('Disposed read');
  except on E: ELockDisposedException do ; end;
  try FLock.AcquireWriteLock; Fail('Disposed write');
  except on E: ELockDisposedException do ; end;
  try FLock.AcquireUpgradeableReadLock; Fail('Disposed upgrade');
  except on E: ELockDisposedException do ; end;
end;

procedure TReadWriteLockTests.ExceptionCleanupReleasesOnlyOwnedAcquisitions;
var LWriter: TLockWorker;
begin
  RecursivePolicy;
  FLock.AcquireWriteLock;
  try
    try
      FLock.AcquireWriteLock;
      try
        FLock.AcquireReadLock;
        try raise EAbort.Create('Expected protected work failure');
        finally FLock.ReleaseReadLock; end;
      finally FLock.ReleaseWriteLock; end;
    except on E: EAbort do ; end;
    AssertTrue(FLock.IsWriteLockHeld);
    AssertEquals(1, FLock.RecursiveWriteCount);
  finally FLock.ReleaseWriteLock; end;
  LWriter := Worker(laWrite);
  Await(LWriter.Entered, 'Exception cleanup released ownership');
  Release(LWriter);
end;

procedure TReadWriteLockTests.HeldAndRecursivePropertiesAreThreadSpecific;
var LReader: TLockWorker;
begin
  RecursivePolicy;
  FLock.AcquireUpgradeableReadLock;
  FLock.AcquireUpgradeableReadLock;
  LReader := Worker(laRead);
  Await(LReader.Entered, 'Independent reader');
  AssertEquals(1, LReader.OwnReadCount);
  AssertEquals(0, LReader.OwnWriteCount);
  AssertEquals(0, LReader.OwnUpgradeCount);
  AssertFalse(FLock.IsReadLockHeld);
  AssertEquals(2, FLock.RecursiveUpgradeCount);
  Release(LReader);
  FLock.ReleaseUpgradeableReadLock;
  FLock.ReleaseUpgradeableReadLock;
end;

procedure TReadWriteLockTests.FiniteWaitPermitsCooperativeCancellation;
var LReader: TLockWorker; LStarted: UInt64;
begin
  FLock.AcquireWriteLock;
  LReader := Worker(laCancellableRead, 35);
  WaitCount(0, 1);
  LStarted := Tick;
  LReader.RequestStop;
  Completed(LReader);
  AssertTrue('Cooperative stop completes after finite wait', Tick - LStarted < 700);
  AssertTrue('Cancellation retains owner', FLock.IsWriteLockHeld);
  FLock.ReleaseWriteLock;
end;

procedure TReadWriteLockTests.PromotedWriteExceptionRetainsSurroundingUpgradeableLock;
var LWriter, LReader: TLockWorker;
begin
  FLock.AcquireUpgradeableReadLock;
  try
    try
      FLock.AcquireWriteLock;
      try
        raise EAbort.Create('Expected promoted-write failure');
      finally
        FLock.ReleaseWriteLock;
      end;
    except on E: EAbort do ; end;
    AssertFalse('Only write ownership was released', FLock.IsWriteLockHeld);
    AssertTrue('Surrounding upgradeable ownership survives', FLock.IsUpgradeableReadLockHeld);
    AssertEquals(1, FLock.RecursiveUpgradeCount);
    LWriter := Worker(laWrite, 0);
    Completed(LWriter);
    AssertFalse('The remaining upgrader excludes another writer', LWriter.Acquired);
    LReader := Worker(laRead);
    Await(LReader.Entered, 'Readers can share the remaining upgradeable lock');
    Release(LReader);
  finally
    FLock.ReleaseUpgradeableReadLock;
  end;
  LWriter := Worker(laWrite);
  Await(LWriter.Entered, 'Final typed release frees the surrounding ownership');
  Release(LWriter);
end;

initialization
  RegisterTest(TReadWriteLockTests);
end.
