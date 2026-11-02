unit UReadWriteScope.Checks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  These assertions run unchanged under FPCUnit and native Delphi. }

{$IFDEF FPC}{$mode Delphi}{$H+}{$ENDIF}

interface

const ReadWriteScopeCheckCount = 26;

function ReadWriteScopeCheckName(AIndex: Integer): String;
procedure RunReadWriteScopeCheck(AIndex: Integer);

implementation

uses Classes, SysUtils, SyncObjs, UCommon, UConcurrency;

type
  TMode = (smRead, smWrite, smUpgradeable);
  TScopeRecord = record
    Scope: TScope;
  end;
  TScopeOwner = class
    Scope: TScope;
    constructor Create(ALock: TReadWriteLock);
  end;
  TScopeWorker = class(TThread)
  private
    FLock: TReadWriteLock;
    FMode: TMode;
    procedure RunOwned;
  protected
    procedure Execute; override;
  public
    Started, Acquired, ReleaseGate, Done: TEvent;
    Error: String;
    constructor Create(ALock: TReadWriteLock; AMode: TMode);
    destructor Destroy; override;
  end;
  TWrongThreadWorker = class(TThread)
  private
    FScope: TScope;
  protected
    procedure Execute; override;
  public
    ErrorClass: String;
    constructor Create(const AScope: TScope);
  end;

const Names: array[1..ReadWriteScopeCheckCount] of String = (
  'ReadScopeNormalReturn', 'WriteScopeNormalReturn', 'UpgradeableScopeNormalReturn',
  'AllModesEarlyExit', 'AllModesExceptionUnwind', 'CaughtExceptionKeepsScope',
  'CopiesRetainSingleAcquisition', 'OverwriteReleasesPreviousLock',
  'ReturnedScopeRetainsAcquisition', 'RecordFieldFinalizesScope',
  'ClassFieldFinalizesScope', 'RecursiveReadScopesBalance',
  'RecursiveWriteScopesBalance', 'NonrecursiveAcquisitionPreservesOuter',
  'OrdinaryReadCannotPromote', 'DisposedLockRejectsAllScopes',
  'PromotionRetainsOuterUpgradeable', 'PromotionExceptionRetainsOuter',
  'WriteToReadDowngrade', 'UpgradeableToReadDowngrade',
  'ConcurrentReadersShareScopes', 'WriterWaitsForLastCopy',
  'ReaderWaitsForWriterScope', 'FailedAssignmentPreservesExistingScope',
  'ManualAndScopedAcquisitionsBalance', 'WrongThreadReleaseIsRejected');

procedure Check(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function Enter(ALock: TReadWriteLock; AMode: TMode): TScope;
begin
  case AMode of
    smRead: Result := ALock.EnterReadScope;
    smWrite: Result := ALock.EnterWriteScope;
    smUpgradeable: Result := ALock.EnterUpgradeableReadScope;
  end;
end;

procedure SetScope(var AScope: TScope; ALock: TReadWriteLock; AMode: TMode);
begin
  // Function-result records can introduce compiler-owned temporary copies.
  // End that routine before tests deliberately release the final named owner.
  AScope := Enter(ALock, AMode);
end;

constructor TScopeOwner.Create(ALock: TReadWriteLock);
begin
  inherited Create;
  Scope := ALock.EnterWriteScope;
end;

procedure CheckHeld(ALock: TReadWriteLock; AMode: TMode);
begin
  case AMode of
    smRead: Check(ALock.IsReadLockHeld, 'Read scope was released before protected work');
    smWrite: Check(ALock.IsWriteLockHeld, 'Write scope was released before protected work');
    smUpgradeable: Check(ALock.IsUpgradeableReadLockHeld, 'Upgradeable scope released early');
  end;
end;

procedure CheckReleased(ALock: TReadWriteLock);
begin
  Check(not ALock.IsReadLockHeld and not ALock.IsWriteLockHeld and
    not ALock.IsUpgradeableReadLockHeld, 'Scope left same-thread ownership');
  Check(ALock.CurrentReadCount = 0, 'Scope left a reader registered');
end;

procedure RunLocal(ALock: TReadWriteLock; AMode: TMode; AExitKind: Integer);
var Scope: TScope;
begin
  Scope := Enter(ALock, AMode);
  // The local is deliberately never read: only external lock state is checked.
  CheckHeld(ALock, AMode);
  case AExitKind of
    1: Exit;
    2: raise EAbort.Create('protected-work failure');
    3: begin
      try raise EAbort.Create('caught inside protected work');
      except on E: EAbort do CheckHeld(ALock, AMode); end;
      CheckHeld(ALock, AMode);
    end;
  end;
end;

function ReturnScope(ALock: TReadWriteLock): TScope;
var Scope: TScope;
begin
  Scope := ALock.EnterReadScope;
  Result := Scope;
end;

procedure ReceiveReturnedScope(var AScope: TScope; ALock: TReadWriteLock);
begin
  AScope := ReturnScope(ALock);
end;

procedure RunRecordField(ALock: TReadWriteLock);
var Owner: TScopeRecord;
begin
  Owner.Scope := ALock.EnterWriteScope;
  CheckHeld(ALock, smWrite);
end;

procedure RunPromoted(ALock: TReadWriteLock; ARaise: Boolean);
var Scope: TScope;
begin
  Scope := ALock.EnterWriteScope;
  Check(ALock.IsWriteLockHeld and ALock.IsUpgradeableReadLockHeld,
    'Promoted scope lost its outer upgradeable ownership');
  if ARaise then raise EAbort.Create('promoted-work failure');
end;

constructor TScopeWorker.Create(ALock: TReadWriteLock; AMode: TMode);
begin
  inherited Create(True);
  FLock := ALock;
  FMode := AMode;
  Started := TEvent.Create(nil, True, False, '');
  Acquired := TEvent.Create(nil, True, False, '');
  ReleaseGate := TEvent.Create(nil, True, False, '');
  Done := TEvent.Create(nil, True, False, '');
end;

destructor TScopeWorker.Destroy;
begin
  ReleaseGate.SetEvent;
  WaitFor;
  Started.Free; Acquired.Free; ReleaseGate.Free; Done.Free;
  inherited;
end;

procedure TScopeWorker.RunOwned;
var Scope: TScope;
begin
  Scope := Enter(FLock, FMode);
  CheckHeld(FLock, FMode);
  Acquired.SetEvent;
  Check(ReleaseGate.WaitFor(4000) = wrSignaled, 'Worker release gate timed out');
end;

procedure TScopeWorker.Execute;
begin
  Started.SetEvent;
  try
    try
      RunOwned;
    except on E: Exception do Error := E.ClassName + ': ' + E.Message; end;
  finally
    // RunOwned's managed local has already finalized before completion is sent.
    Done.SetEvent;
  end;
end;

constructor TWrongThreadWorker.Create(const AScope: TScope);
begin
  inherited Create(True);
  FScope := AScope;
end;

procedure TWrongThreadWorker.Execute;
begin
  try FScope.Reset;
  except on E: Exception do ErrorClass := E.ClassName; end;
end;

function ReadWriteScopeCheckName(AIndex: Integer): String;
begin
  if (AIndex < 1) or (AIndex > ReadWriteScopeCheckCount) then
    raise EArgumentOutOfRangeException.Create('AIndex');
  Result := Names[AIndex];
end;

procedure RunReadWriteScopeCheck(AIndex: Integer);
var
  Lock, OtherLock: TReadWriteLock;
  Scope, Other: TScope;
  Owner: TScopeOwner;
  Worker: TScopeWorker;
  WrongWorker: TWrongThreadWorker;
  Mode: TMode;
  Policy: TLockRecursionPolicy;
  Rejected: Boolean;
begin
  ReadWriteScopeCheckName(AIndex);
  Lock := TReadWriteLock.Create(lrpSupportsRecursion);
  OtherLock := nil; Worker := nil; WrongWorker := nil;
  try
    case AIndex of
      1..3: begin
        RunLocal(Lock, TMode(AIndex - 1), 0);
        CheckReleased(Lock);
      end;
      4..6: for Mode := Low(TMode) to High(TMode) do begin
        try RunLocal(Lock, Mode, AIndex - 3);
        except on E: EAbort do Check(AIndex = 5, 'Unexpected protected-work exception'); end;
        CheckReleased(Lock);
      end;
      7: for Mode := Low(TMode) to High(TMode) do begin
        SetScope(Scope, Lock, Mode);
        Other := Scope;
        Check((Lock.RecursiveReadCount + Lock.RecursiveWriteCount +
          Lock.RecursiveUpgradeCount) = 1, 'Copy acquired the lock again');
        Scope.Reset;
        CheckHeld(Lock, Mode);
        Other.Reset;
        CheckReleased(Lock);
      end;
      8: begin
        OtherLock := TReadWriteLock.Create;
        SetScope(Scope, Lock, smRead);
        SetScope(Scope, OtherLock, smWrite);
        CheckReleased(Lock);
        CheckHeld(OtherLock, smWrite);
        Scope.Reset;
        CheckReleased(OtherLock);
      end;
      9: begin
        ReceiveReturnedScope(Scope, Lock);
        CheckHeld(Lock, smRead);
        Scope.Reset;
        CheckReleased(Lock);
      end;
      10: begin
        RunRecordField(Lock);
        CheckReleased(Lock);
      end;
      11: begin
        Owner := TScopeOwner.Create(Lock);
        try CheckHeld(Lock, smWrite);
        finally Owner.Free; end;
        CheckReleased(Lock);
      end;
      12, 13: begin
        if AIndex = 12 then Mode := smRead else Mode := smWrite;
        SetScope(Scope, Lock, Mode);
        SetScope(Other, Lock, Mode);
        if Mode = smRead then Check(Lock.RecursiveReadCount = 2, 'Missing nested read')
        else Check(Lock.RecursiveWriteCount = 2, 'Missing nested write');
        Other.Reset;
        CheckHeld(Lock, Mode);
        Scope.Reset;
        CheckReleased(Lock);
      end;
      14: begin
        OtherLock := TReadWriteLock.Create(lrpNoRecursion);
        for Mode := Low(TMode) to High(TMode) do begin
          SetScope(Scope, OtherLock, Mode);
          Rejected := False;
          try Other := Enter(OtherLock, Mode);
          except on E: ELockRecursionException do Rejected := True; end;
          Check(Rejected and Other.IsEmpty, 'Recursive scope was not rejected cleanly');
          CheckHeld(OtherLock, Mode);
          Scope.Reset;
          CheckReleased(OtherLock);
        end;
      end;
      15: begin
        SetScope(Scope, Lock, smRead);
        Rejected := False;
        try Other := Lock.EnterWriteScope;
        except on E: ELockRecursionException do Rejected := True; end;
        Check(Rejected and Other.IsEmpty, 'Ordinary read scope promoted');
        CheckHeld(Lock, smRead);
        Scope.Reset;
        CheckReleased(Lock);
      end;
      16: begin
        Lock.Dispose;
        for Mode := Low(TMode) to High(TMode) do begin
          Rejected := False;
          try Scope := Enter(Lock, Mode);
          except on E: ELockDisposedException do Rejected := True; end;
          Check(Rejected and Scope.IsEmpty, 'Disposed lock returned an owning scope');
        end;
      end;
      17, 18: for Policy := Low(TLockRecursionPolicy) to High(TLockRecursionPolicy) do begin
        OtherLock := TReadWriteLock.Create(Policy);
        try
          SetScope(Scope, OtherLock, smUpgradeable);
          try RunPromoted(OtherLock, AIndex = 18);
          except on E: EAbort do Check(AIndex = 18, 'Unexpected promotion exception'); end;
          CheckHeld(OtherLock, smUpgradeable);
          Check(not OtherLock.IsWriteLockHeld, 'Inner write acquisition survived its routine');
          Scope.Reset;
          CheckReleased(OtherLock);
        finally
          Scope.Reset;
          FreeAndNil(OtherLock);
        end;
      end;
      19, 20: begin
        if AIndex = 19 then SetScope(Scope, Lock, smWrite)
        else SetScope(Scope, Lock, smUpgradeable);
        SetScope(Other, Lock, smRead);
        Scope.Reset;
        CheckHeld(Lock, smRead);
        Check(not Lock.IsWriteLockHeld and not Lock.IsUpgradeableReadLockHeld,
          'Downgrade retained the released mode');
        Other.Reset;
        CheckReleased(Lock);
      end;
      21: begin
        SetScope(Scope, Lock, smRead);
        Worker := TScopeWorker.Create(Lock, smRead);
        Worker.Start;
        Check(Worker.Acquired.WaitFor(2000) = wrSignaled, 'Readers did not coexist');
        CheckHeld(Lock, smRead);
        Worker.ReleaseGate.SetEvent;
        Check(Worker.Done.WaitFor(2000) = wrSignaled, 'Reader scope did not finish');
        Check(Worker.Error = '', Worker.Error);
        Check(Lock.CurrentReadCount = 1, 'Worker did not release its own read scope');
        Scope.Reset;
        CheckReleased(Lock);
      end;
      22, 23: begin
        if AIndex = 22 then begin
          SetScope(Scope, Lock, smRead); Other := Scope;
          Worker := TScopeWorker.Create(Lock, smWrite);
        end else begin
          SetScope(Scope, Lock, smWrite);
          Worker := TScopeWorker.Create(Lock, smRead);
        end;
        Worker.Start;
        Check(Worker.Started.WaitFor(2000) = wrSignaled, 'Blocked worker did not start');
        Check(Worker.Acquired.WaitFor(40) = wrTimeout, 'Worker bypassed owning scope');
        Scope.Reset;
        if AIndex = 22 then begin
          Check(Worker.Acquired.WaitFor(40) = wrTimeout, 'Writer bypassed last scope copy');
          Other.Reset;
        end;
        Check(Worker.Acquired.WaitFor(2000) = wrSignaled, 'Worker did not acquire after scope release');
        Worker.ReleaseGate.SetEvent;
        Check(Worker.Done.WaitFor(2000) = wrSignaled, 'Worker did not finish');
        Check(Worker.Error = '', Worker.Error);
        CheckReleased(Lock);
      end;
      24: begin
        OtherLock := TReadWriteLock.Create;
        OtherLock.Dispose;
        SetScope(Scope, Lock, smRead);
        Rejected := False;
        try Scope := OtherLock.EnterWriteScope;
        except on E: ELockDisposedException do Rejected := True; end;
        Check(Rejected and not Scope.IsEmpty, 'Failed RHS discarded existing scope');
        CheckHeld(Lock, smRead);
        Scope.Reset;
        CheckReleased(Lock);
      end;
      25: begin
        Lock.AcquireReadLock;
        try
          SetScope(Scope, Lock, smRead);
          Check(Lock.RecursiveReadCount = 2, 'Scoped/manual acquisitions not independent');
          Scope.Reset;
          Check(Lock.RecursiveReadCount = 1, 'Scope released manual read acquisition');
        finally Lock.ReleaseReadLock; end;
        CheckReleased(Lock);
      end;
      26: begin
        SetScope(Scope, Lock, smWrite);
        WrongWorker := TWrongThreadWorker.Create(Scope);
        Scope.Reset;
        WrongWorker.Start;
        WrongWorker.WaitFor;
        Check(WrongWorker.ErrorClass = 'ESynchronizationLockException',
          'Cross-thread finalization failed to reject release');
        CheckHeld(Lock, smWrite);
        // Invalid cross-thread ownership transfer is rejected. The original
        // thread must still release the actual lock acquisition it owns.
        Lock.ReleaseWriteLock;
        CheckReleased(Lock);
      end;
    end;
  finally
    Scope.Reset;
    Other.Reset;
    Worker.Free;
    WrongWorker.Free;
    OtherLock.Free;
    Lock.Free;
  end;
end;

end.
