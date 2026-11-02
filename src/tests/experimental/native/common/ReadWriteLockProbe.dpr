program ReadWriteLockProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Actual native Delphi checks for the public generic lock contract.
  Distributed under the MIT software license; see the accompanying LICENSE. }
{$APPTYPE CONSOLE}

uses Winapi.Windows, System.SysUtils, System.Classes, System.SyncObjs,
  System.Generics.Collections, UConcurrency;

type
  TAction = (acRead, acWrite, acUpgrade, acPromote, acReleaseRead, acReleaseWrite, acReleaseUpgrade);
  TActor = class(TThread)
  private FLock:TReadWriteLock; FAction:TAction; FTimeout:Integer;
  protected procedure Execute; override;
  public Entered, ReleaseGate, Promote, Promoted, Done:TEvent;
    Acquired:Boolean; ErrorClass, ExpectedError:String; OwnR,OwnW,OwnU:Integer;
    Elapsed:UInt64;
    constructor Create(ALock:TReadWriteLock; AAction:TAction; ATimeout:Integer);
    destructor Destroy; override;
  end;
  TProbe = class
  private L:TReadWriteLock; Actors:TList<TActor>; LogPath:String;
    procedure Require(AValue:Boolean; const AMessage:String);
    procedure Log(const AText:String);
    function Worker(AAction:TAction; ATimeout:Integer=1500; const AExpectedError:String=''):TActor;
    procedure Await(AEvent:TEvent; const AMessage:String);
    procedure WaitCount(AKind,AValue:Integer);
    procedure Clean;
    procedure Reset(APolicy:TLockRecursionPolicy=lrpNoRecursion);
    procedure ExpectRecursion(AAction:TAction);
    procedure ConcurrentReaders;
    procedure ExclusiveWriter;
    procedure RecursionPolicies;
    procedure UpgradeAndDowngrade;
    procedure PromotionQueue;
    procedure TimeoutsAndDispose;
    procedure WrongThreadAndInvalidTimeout;
  public constructor Create(const ALogPath:String);
    destructor Destroy; override;
    procedure Run;
  end;
constructor TActor.Create(ALock:TReadWriteLock; AAction:TAction; ATimeout:Integer);
begin
  inherited Create(True); FreeOnTerminate:=False;
  FLock:=ALock; FAction:=AAction; FTimeout:=ATimeout;
  Entered:=TEvent.Create(nil,True,False,''); ReleaseGate:=TEvent.Create(nil,True,False,'');
  Promote:=TEvent.Create(nil,True,False,''); Promoted:=TEvent.Create(nil,True,False,'');
  Done:=TEvent.Create(nil,True,False,'');
end;
destructor TActor.Destroy;
begin
  Terminate; ReleaseGate.SetEvent; Promote.SetEvent; WaitFor;
  Entered.Free; ReleaseGate.Free; Promote.Free; Promoted.Free; Done.Free; inherited;
end;
procedure TActor.Execute;
var T:UInt64;
begin
  T:=GetTickCount64;
  try
    try
      case FAction of
        acRead: Acquired:=FLock.TryAcquireReadLock(FTimeout);
        acWrite: Acquired:=FLock.TryAcquireWriteLock(FTimeout);
        acUpgrade,acPromote: Acquired:=FLock.TryAcquireUpgradeableReadLock(FTimeout);
        acReleaseRead: FLock.ReleaseReadLock;
        acReleaseWrite: FLock.ReleaseWriteLock;
        acReleaseUpgrade: FLock.ReleaseUpgradeableReadLock;
      end;
      Elapsed:=GetTickCount64-T;
      if Acquired then begin
        try
          OwnR:=FLock.RecursiveReadCount; OwnW:=FLock.RecursiveWriteCount; OwnU:=FLock.RecursiveUpgradeCount;
          Entered.SetEvent;
          if FAction=acPromote then begin
            if Promote.WaitFor(1500)<>wrSignaled then raise Exception.Create('Promotion request gate timed out');
            FLock.AcquireWriteLock;
            try
              Promoted.SetEvent;
              while not Terminated do if ReleaseGate.WaitFor(10)=wrSignaled then Break;
            finally FLock.ReleaseWriteLock; end;
          end else
            while not Terminated do if ReleaseGate.WaitFor(10)=wrSignaled then Break;
        finally
          case FAction of
            acRead: FLock.ReleaseReadLock;
            acWrite: FLock.ReleaseWriteLock;
            acUpgrade,acPromote: FLock.ReleaseUpgradeableReadLock;
          end;
        end;
      end;
    except on E:Exception do ErrorClass:=E.ClassName; end;
  finally Done.SetEvent; end;
end;
constructor TProbe.Create(const ALogPath:String);
begin
  inherited Create;
  LogPath:=ALogPath;
  Actors:=TList<TActor>.Create;
end;
destructor TProbe.Destroy;
begin
  Clean;
  Actors.Free;
  inherited;
end;
procedure TProbe.Require(AValue:Boolean; const AMessage:String);
begin
  if not AValue then
    raise Exception.Create(AMessage);
end;
procedure TProbe.Log(const AText:String);
var F:TextFile;
begin
  AssignFile(F,LogPath); if FileExists(LogPath) then Append(F) else Rewrite(F);
  try WriteLn(F,GetTickCount64,' ',AText); finally CloseFile(F); end;
end;
function TProbe.Worker(AAction:TAction; ATimeout:Integer; const AExpectedError:String):TActor;
begin
  Result:=TActor.Create(L,AAction,ATimeout); Result.ExpectedError:=AExpectedError;
  Actors.Add(Result); Result.Start;
end;
procedure TProbe.Await(AEvent:TEvent; const AMessage:String);
begin
  Require(AEvent.WaitFor(2000)=wrSignaled,AMessage);
end;
procedure TProbe.WaitCount(AKind,AValue:Integer);
var T:UInt64; N:Integer;
begin
  T:=GetTickCount64+1500;
  repeat
    case AKind of
      0: N:=L.WaitingReadCount;
      1: N:=L.WaitingWriteCount;else N:=L.WaitingUpgradeCount;
    end;
    if N=AValue then Exit;
    Sleep(1);
  until GetTickCount64>=T;
  raise Exception.Create('Actual waiter count did not reach '+IntToStr(AValue));
end;
procedure TProbe.Clean;
var A:TActor; Err:String;
begin
  if Assigned(L) then begin
    while L.RecursiveWriteCount>0 do L.ReleaseWriteLock;
    while L.RecursiveReadCount>0 do L.ReleaseReadLock;
    while L.RecursiveUpgradeCount>0 do L.ReleaseUpgradeableReadLock;
  end;
  for A in Actors do begin
    A.Terminate;
    A.ReleaseGate.SetEvent;
    A.Promote.SetEvent;
  end;
  Err:='';
  for A in Actors do begin
    A.WaitFor;
    if (A.ErrorClass<>A.ExpectedError) and (Err='') then
      Err:='Actor exception '+A.ErrorClass+' expected '+A.ExpectedError;
  end;
  for A in Actors do A.Free;
  Actors.Clear;
  FreeAndNil(L);
  if Err<>'' then raise Exception.Create(Err);
end;
procedure TProbe.Reset(APolicy:TLockRecursionPolicy);
begin
  Clean;
  L:=TReadWriteLock.Create(APolicy);
end;
procedure TProbe.ExpectRecursion(AAction:TAction);
var Failed:Boolean;
begin
  Failed:=False;
  try
    case AAction of
      acRead: L.TryAcquireReadLock(0);
      acWrite: L.TryAcquireWriteLock(0);
      acUpgrade: L.TryAcquireUpgradeableReadLock(0);
    end;
  except on E:ELockRecursionException do Failed:=True;end;
  Require(Failed,'Forbidden recursion/promotion did not raise ELockRecursionException');
end;
procedure TProbe.ConcurrentReaders;
var A,B:TActor;
begin
  Reset;
  A:=Worker(acRead); Await(A.Entered,'First native reader');
  B:=Worker(acRead); Await(B.Entered,'Concurrent native reader');
  Require((L.CurrentReadCount=2) and not L.IsReadLockHeld,'Reader count/ownership is not thread-specific');
  Require((A.OwnR=1) and (B.OwnR=1),'Each reader must own one recursion');
  Log('PASS concurrent native readers and thread-specific ownership/counts');
end;
procedure TProbe.ExclusiveWriter;
var W,R,W2,U:TActor;
begin
  Reset; W:=Worker(acWrite); Await(W.Entered,'Native writer');
  R:=Worker(acRead,0);W2:=Worker(acWrite,0);U:=Worker(acUpgrade,0);
  Await(R.Done,'Read zero-timeout');Await(W2.Done,'Write zero-timeout');Await(U.Done,'Upgrade zero-timeout');
  Require(not R.Acquired and not W2.Acquired and not U.Acquired,'Writer allowed concurrent foreign ownership');
  Require((L.WaitingReadCount=0) and (L.WaitingWriteCount=0) and (L.WaitingUpgradeCount=0),'Zero-timeout registered a waiter');
  Log('PASS writer exclusion across all modes and zero-timeout cleanup');
end;
procedure TProbe.RecursionPolicies;
begin
  Reset;
  L.AcquireReadLock;ExpectRecursion(acRead);ExpectRecursion(acWrite);ExpectRecursion(acUpgrade);L.ReleaseReadLock;
  L.AcquireWriteLock;ExpectRecursion(acWrite);ExpectRecursion(acRead);ExpectRecursion(acUpgrade);L.ReleaseWriteLock;
  L.AcquireUpgradeableReadLock;L.AcquireReadLock;ExpectRecursion(acWrite);L.ReleaseReadLock;
  Require(L.TryAcquireWriteLock(0),'Default upgrader could not promote without an explicit reader');L.ReleaseWriteLock;L.ReleaseUpgradeableReadLock;
  Reset(lrpSupportsRecursion);
  L.AcquireWriteLock;L.AcquireWriteLock;L.AcquireReadLock;L.AcquireUpgradeableReadLock;
  Require((L.RecursiveWriteCount=2) and (L.RecursiveReadCount=1) and (L.RecursiveUpgradeCount=1),'Supported recursion counts differ');
  L.ReleaseUpgradeableReadLock;L.ReleaseReadLock;L.ReleaseWriteLock;L.ReleaseWriteLock;
  L.AcquireReadLock;L.AcquireReadLock;Require(L.CurrentReadCount=1,'Recursive read counted as multiple owners');
  ExpectRecursion(acWrite);ExpectRecursion(acUpgrade);L.ReleaseReadLock;L.ReleaseReadLock;
  Log('PASS default/supported recursion and forbidden ordinary-read promotion');
end;
procedure TProbe.UpgradeAndDowngrade;
var R,W:TActor;
begin
  Reset(lrpSupportsRecursion);
  L.AcquireUpgradeableReadLock;L.AcquireReadLock;
  R:=Worker(acRead);Await(R.Entered,'Foreign reader shares upgradeable');
  Require(not L.TryAcquireWriteLock(60),'Promotion ignored foreign reader');
  Require(L.IsUpgradeableReadLockHeld and not L.IsWriteLockHeld,'Timed-out promotion lost ownership');
  R.ReleaseGate.SetEvent;Await(R.Done,'Foreign reader release');
  Require(L.TryAcquireWriteLock(500),'Promotion failed after last foreign reader left');
  L.ReleaseUpgradeableReadLock;L.ReleaseWriteLock;
  Require(L.IsReadLockHeld and not L.IsWriteLockHeld,'Write-to-read downgrade lost reader');
  W:=Worker(acWrite,0);Await(W.Done,'Downgrade writer test');Require(not W.Acquired,'Downgrade lost reader exclusion');
  L.ReleaseReadLock;
  Log('PASS upgrade timeout/retry and write-to-read downgrade retains exclusion');
end;
procedure TProbe.PromotionQueue;
var U,R:TActor;
begin
  Reset;
  L.AcquireReadLock;U:=Worker(acPromote);Await(U.Entered,'Upgradeable reader');
  U.Promote.SetEvent;WaitCount(1,1);
  R:=Worker(acRead);WaitCount(0,1);
  Require(U.Promoted.WaitFor(0)=wrTimeout,'Promotion overlapped foreign reader');
  L.ReleaseReadLock;Await(U.Promoted,'Queued upgrade promotion');
  Require(R.Entered.WaitFor(0)=wrTimeout,'New reader bypassed queued promoted writer');
  U.ReleaseGate.SetEvent;Await(U.Done,'Promoted writer release');Await(R.Entered,'Queued reader recovery');
  Log('PASS queued promotion and reader recovery without ownership loss');
end;
procedure TProbe.TimeoutsAndDispose;
var R:TActor; Rejected:Boolean;
begin
  Reset;L.AcquireWriteLock;
  R:=Worker(acRead,90);Await(R.Done,'Timed reader');
  Require(not R.Acquired and (R.Elapsed>=75) and (R.Elapsed<1000),'Finite reader timeout was not bounded');
  Require(L.WaitingReadCount=0,'Timed waiter remained registered');
  R:=Worker(acRead,-1);WaitCount(0,1);
  Rejected:=False;try L.Dispose;except on E:ESynchronizationLockException do Rejected:=True;end;
  Require(Rejected and L.IsWriteLockHeld and (L.WaitingReadCount=1),'Rejected Dispose changed owners/waiters');
  L.ReleaseWriteLock;Await(R.Entered,'Waiter recovery after rejected Dispose');
  R.ReleaseGate.SetEvent;Await(R.Done,'Infinite reader release');
  L.Dispose;Rejected:=False;
  try L.TryAcquireReadLock(0);except on E:ELockDisposedException do Rejected:=True;end;
  Require(Rejected,'Disposed lock accepted an acquisition');
  Log('PASS finite/infinite timeout, rejected Dispose, waiter recovery and disposed rejection');
end;
procedure TProbe.WrongThreadAndInvalidTimeout;
var A:TAction; W:TActor; Rejected:Boolean;
begin
  for A:=acReleaseRead to acReleaseUpgrade do begin
    Reset;
    case A of
      acReleaseRead: L.AcquireReadLock;
      acReleaseWrite: L.AcquireWriteLock;
      acReleaseUpgrade: L.AcquireUpgradeableReadLock;
    end;
    W:=Worker(A,0,'ESynchronizationLockException');Await(W.Done,'Wrong-thread release');
    Require(W.ErrorClass='ESynchronizationLockException','Wrong-thread release was accepted');
    case A of
      acReleaseRead: Require(L.IsReadLockHeld,'Wrong-thread lost read');
      acReleaseWrite: Require(L.IsWriteLockHeld,'Wrong-thread lost write');
      acReleaseUpgrade: Require(L.IsUpgradeableReadLockHeld,'Wrong-thread lost upgrade');
    end;
  end;
  Reset;
  Rejected:=False;try L.TryAcquireReadLock(-2);except on E:EArgumentOutOfRangeException do Rejected:=True;end;
  Require(Rejected and (L.CurrentReadCount=0),'Invalid timeout altered ownership');
  Log('PASS wrong-thread release protection and invalid timeout rejection');
end;
procedure TProbe.Run;
begin
  ConcurrentReaders;ExclusiveWriter;RecursionPolicies;UpgradeAndDowngrade;
  PromotionQueue;TimeoutsAndDispose;WrongThreadAndInvalidTimeout;Clean;
  Log('PASS all seven native read/write lock contract groups');
end;
var Probe:TProbe;
begin
  if ParamCount<>1 then Halt(64);
  Probe:=nil;
  try
    ReportMemoryLeaksOnShutdown:=True;
    Probe:=TProbe.Create(ExpandFileName(ParamStr(1)));
    try Probe.Run;finally FreeAndNil(Probe);end;
    ExitCode:=0;
  except on E:Exception do begin
    with TStringList.Create do
    try if FileExists(ParamStr(1)) then LoadFromFile(ParamStr(1));Add('FAIL '+E.ClassName+': '+E.Message);SaveToFile(ParamStr(1));finally Free;end;
    ExitCode:=1;
  end;end;
  Probe.Free;
end.
