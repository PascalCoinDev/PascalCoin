unit UCoreLockMigrationChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Exercises the migrated core owners and their public leases, without replacing
  their collections, admission helpers or operation-component implementation. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

type
  TCoreLockMigrationCheck = (clmNamedDefault, clmNamedRecursive, clmThreadListRecursive, clmThreadListTryExclusion, clmThreadListCountExclusion, clmThreadListTimedWake, clmProtectedWrite, clmTimedWriteExclusion, clmTimedWriteWake, clmUnsignedTimeouts, clmOperationsRecursive, clmOperationsDestruction);

function CoreLockMigrationCheckCount: Integer;
function RunCoreLockMigrationCheck(ACheck: TCoreLockMigrationCheck; const ALogFile: String): Integer;
function RunCoreLockMigrationChecks(const ALogFile: String): Integer;

implementation

uses Classes, SysUtils, SyncObjs, UThread, UBaseTypes, UBlockChain, UCrypto,
  {$IFDEF FPC}Generics.Collections{$ELSE}System.Generics.Collections{$ENDIF};

type
  TMigrationWorkerAction = (mwaProtectedWrite, mwaTimedWrite, mwaTimedList, mwaListCount, mwaOperations);

  TMigrationWorker = class(TThread)
  private
    FAction: TMigrationWorkerAction;
    FLock: TReadWriteLock;
    FList: TPCThreadList<Integer>;
    FOperations: TPCOperationsComp;
    FTimeout: Cardinal;
    FStarted: Boolean;
  protected
    procedure Execute; override;
  public
    Attempting, Entered, ReleaseGate, Done: TEvent;
    Acquired: Boolean;
    ObservedCount: Integer;
    ErrorText: String;
    constructor Create(AAction: TMigrationWorkerAction; ALock: TReadWriteLock; AList: TPCThreadList<Integer>; AOperations: TPCOperationsComp; ATimeout: Cardinal);
    destructor Destroy; override;
    procedure StartOwned;
  end;

procedure Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then raise Exception.Create(AMessage);
end;

procedure Await(AEvent: TEvent; const AMessage: String);
begin
  Require(AEvent.WaitFor(5000)=wrSignaled,AMessage);
end;

constructor TMigrationWorker.Create(AAction: TMigrationWorkerAction; ALock: TReadWriteLock; AList: TPCThreadList<Integer>; AOperations: TPCOperationsComp; ATimeout: Cardinal);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FAction := AAction;
  FLock := ALock;
  FList := AList;
  FOperations := AOperations;
  FTimeout := ATimeout;
  Attempting := TEvent.Create(nil,True,False,'');
  Entered := TEvent.Create(nil,True,False,'');
  ReleaseGate := TEvent.Create(nil,True,False,'');
  Done := TEvent.Create(nil,True,False,'');
  // Keep the native thread suspended until Delphi's AfterConstruction returns.
  // Starting here changes its creation flag and makes Delphi start it twice.
end;

procedure TMigrationWorker.StartOwned;
begin
  Start;
  FStarted := True;
end;

destructor TMigrationWorker.Destroy;
begin
  // Callers first release every owner lease and observe Done. This is a natural
  // join; no test terminates a thread that might still own a production lock.
  Terminate;
  if Assigned(ReleaseGate) then ReleaseGate.SetEvent;
  if FStarted then WaitFor;
  // RTL destruction handles a never-started thread. Keep any constructed gates
  // alive until that native retirement finishes, including constructor failure.
  inherited;
  Done.Free;
  ReleaseGate.Free;
  Entered.Free;
  Attempting.Free;
end;

procedure TMigrationWorker.Execute;
var LMutable: TList<Integer>;
begin
  // A constructor failure can retire a suspended thread through RTL cleanup.
  // That path must not touch incomplete event state or borrowed test owners.
  if Terminated then Exit;
  try
    try
      Attempting.SetEvent;
      case FAction of
        mwaProtectedWrite: begin
          TPCThread.ProtectAcquireWriteLock(Self,FLock);
          Acquired := True;
        end;
        mwaTimedWrite: Acquired := TPCThread.TryProtectAcquireWriteLock(Self,FTimeout,FLock);
        mwaTimedList: Acquired := FList.TryLockList(FTimeout,LMutable);
        mwaListCount: ObservedCount := FList.Count;
        mwaOperations: begin
          FOperations.Lock;
          Acquired := True;
        end;
      end;
      if Acquired then begin
        try
          if FAction=mwaTimedList then LMutable.Add(71);
          Entered.SetEvent;
          Await(ReleaseGate,'Worker release gate was not opened');
        finally
          case FAction of
            mwaProtectedWrite, mwaTimedWrite: FLock.ReleaseWriteLock;
            mwaTimedList: FList.UnlockList;
            mwaOperations: FOperations.Unlock;
          end;
        end;
      end;
    except
      on E: Exception do ErrorText := E.ClassName+': '+E.Message;
    end;
  finally
    Done.SetEvent;
  end;
end;

procedure FinishWorker(var AWorker: TMigrationWorker);
var LError: String;
begin
  if not Assigned(AWorker) then Exit;
  if not AWorker.FStarted then begin
    FreeAndNil(AWorker);
    Exit;
  end;
  AWorker.ReleaseGate.SetEvent;
  Await(AWorker.Done,'Worker did not finish after owner release');
  AWorker.WaitFor;
  LError := AWorker.ErrorText;
  FreeAndNil(AWorker);
  Require(LError='',LError);
end;

procedure WaitForWriter(ALock: TReadWriteLock; AWorker: TMigrationWorker);
var LStarted: TTickCount;
begin
  Await(AWorker.Attempting,'Writer did not reach admission');
  LStarted := TPlatform.GetTickCount;
  // Observe the real admission counter, rather than copy the lock's timeout
  // algorithm or guess when the worker should have started waiting.
  while (ALock.WaitingWriteCount=0) and (AWorker.Done.WaitFor(0)<>wrSignaled) and
    (TPlatform.GetElapsedMilliseconds(LStarted)<3000) do
    AWorker.Done.WaitFor(1);
  if AWorker.Done.WaitFor(0)=wrSignaled then
    Require(AWorker.ErrorText='',AWorker.ErrorText);
  Require(ALock.WaitingWriteCount=1,'Writer was not queued behind the held owner');
end;

procedure CheckNamedDefault;
var LLock, LUnnamed: TReadWriteLock;
  LRaised, LUnexpectedAcquisition: Boolean;
begin
  LLock := TReadWriteLock.Create('migration.default');
  LUnnamed := TReadWriteLock.Create;
  try
    Require(LLock.Name='migration.default','Named constructor lost its diagnostic name');
    Require(LLock.RecursionPolicy=lrpNoRecursion,'Named constructor changed the default recursion policy');
    Require(LUnnamed.Name='','Unnamed constructor did not preserve its empty name');
    Require(LUnnamed.RecursionPolicy=lrpNoRecursion,'Unnamed constructor changed the default recursion policy');
    LLock.AcquireWriteLock;
    try
      LRaised := False;
      LUnexpectedAcquisition := False;
      try
        LUnexpectedAcquisition := LLock.TryAcquireWriteLock(0);
      except
        on E: ELockRecursionException do LRaised := True;
      end;
      if LUnexpectedAcquisition then LLock.ReleaseWriteLock;
      Require(LRaised,'Named default lock allowed a recursive write');
      Require(LLock.RecursiveWriteCount=1,'Rejected recursion changed the existing owner');
    finally
      LLock.ReleaseWriteLock;
    end;
  finally
    LUnnamed.Free;
    LLock.Free;
  end;
end;

procedure CheckNamedRecursive;
var LLock: TReadWriteLock;
begin
  LLock := TReadWriteLock.Create('migration.recursive',lrpSupportsRecursion);
  try
    Require(LLock.Name='migration.recursive','Recursive named constructor lost its name');
    Require(LLock.RecursionPolicy=lrpSupportsRecursion,'Named constructor lost recursive policy');
    LLock.AcquireWriteLock;
    try
      LLock.AcquireWriteLock;
      try
        LLock.AcquireReadLock;
        try
          Require(LLock.RecursiveWriteCount=2,'Recursive writer depth was not preserved');
          Require(LLock.RecursiveReadCount=1,'Writer could not make a nested read');
        finally
          LLock.ReleaseReadLock;
        end;
      finally
        LLock.ReleaseWriteLock;
      end;
    finally
      LLock.ReleaseWriteLock;
    end;
    Require(not LLock.IsWriteLockHeld,'Recursive writes were not fully released');
    Require(LLock.CurrentReadCount=0,'Nested read ownership leaked');
  finally
    LLock.Free;
  end;
end;

procedure CheckThreadListRecursive;
var LOwner: TPCThreadList<Integer>;
  LFirst, LSecond, LTimed: TList<Integer>;
begin
  LOwner := TPCThreadList<Integer>.Create('migration.list.recursive');
  try
    LFirst := LOwner.LockList;
    try
      LFirst.Add(11);
      LSecond := LOwner.LockList;
      try
        Require(LFirst=LSecond,'Nested mutable lease returned another list');
        Require(LOwner.Add(22)=1,'Add did not preserve recursive owner admission');
        Require(LOwner.Count=2,'Count could not read inside a mutable write lease');
        LOwner.Remove(11);
        Require(LOwner.TryLockList(0,LTimed),'Same-thread nonblocking recursive lease failed');
        try
          Require(LTimed=LFirst,'Timed lease did not return the owned list');
          LTimed.Add(33);
        finally
          LOwner.UnlockList;
        end;
        Require(LOwner.Count=2,'Nested mutation/count result differs');
        LOwner.Clear;
        Require(LOwner.Count=0,'Clear could not reenter the mutable owner');
      finally
        LOwner.UnlockList;
      end;
    finally
      LOwner.UnlockList;
    end;
    Require(LOwner.Count=0,'List owner was not usable after releasing all leases');
  finally
    LOwner.Free;
  end;
end;

procedure CheckThreadListTryExclusion;
var LOwner: TPCThreadList<Integer>;
  LWorker: TMigrationWorker;
  LHeld: Boolean;
begin
  LOwner := TPCThreadList<Integer>.Create('migration.list.try');
  LWorker := nil;
  LHeld := False;
  try
    LOwner.LockList;
    LHeld := True;
    LWorker := TMigrationWorker.Create(mwaTimedList,nil,LOwner,nil,0);
    LWorker.StartOwned;
    Await(LWorker.Done,'Nonblocking list admission did not return');
    Require(LWorker.ErrorText='',LWorker.ErrorText);
    Require(not LWorker.Acquired,'Another thread obtained the mutable list while owned');
    Require(LOwner.Count=0,'Failed list admission changed the protected list');
    FinishWorker(LWorker);
    LOwner.UnlockList;
    LHeld := False;
    LWorker := TMigrationWorker.Create(mwaTimedList,nil,LOwner,nil,0);
    LWorker.StartOwned;
    Await(LWorker.Entered,'List remained locked after its owner released');
    FinishWorker(LWorker);
    Require(LOwner.Count=1,'Successful mutable admission did not apply its mutation');
  finally
    if LHeld then LOwner.UnlockList;
    try
      FinishWorker(LWorker);
    finally
      LOwner.Free;
    end;
  end;
end;

procedure CheckThreadListCountExclusion;
var LOwner: TPCThreadList<Integer>;
  LWorker: TMigrationWorker;
  LMutable: TList<Integer>;
  LHeld: Boolean;
begin
  LOwner := TPCThreadList<Integer>.Create('migration.list.count');
  LWorker := nil;
  LHeld := False;
  try
    LMutable := LOwner.LockList;
    LHeld := True;
    LMutable.Add(19);
    LWorker := TMigrationWorker.Create(mwaListCount,nil,LOwner,nil,0);
    LWorker.StartOwned;
    Await(LWorker.Attempting,'Count reader did not start');
    // Count must not observe a mutable lease owned by another thread. This is
    // a bounded negative event assertion, not a production debounce delay.
    Require(LWorker.Done.WaitFor(150)=wrTimeout,'Count bypassed another thread''s mutable lease');
    LOwner.UnlockList;
    LHeld := False;
    Await(LWorker.Done,'Count did not wake after the mutable lease ended');
    Require(LWorker.ObservedCount=1,'Count did not observe the completed mutation');
  finally
    if LHeld then LOwner.UnlockList;
    try
      FinishWorker(LWorker);
    finally
      LOwner.Free;
    end;
  end;
end;

procedure CheckThreadListTimedWake;
var LOwner: TPCThreadList<Integer>;
  LWorker: TMigrationWorker;
  LHeld: Integer;
begin
  LOwner := TPCThreadList<Integer>.Create('migration.list.wait');
  LWorker := nil;
  LHeld := 0;
  try
    LOwner.LockList;
    Inc(LHeld);
    LOwner.LockList;
    Inc(LHeld);
    LWorker := TMigrationWorker.Create(mwaTimedList,nil,LOwner,nil,2500);
    LWorker.StartOwned;
    Await(LWorker.Attempting,'Timed list writer did not start');
    Require(LWorker.Entered.WaitFor(150)=wrTimeout,'Timed writer bypassed nested mutable leases');
    LOwner.UnlockList;
    Dec(LHeld);
    Require(LWorker.Entered.WaitFor(150)=wrTimeout,'Releasing one nested lease released another owner''s protection');
    LOwner.UnlockList;
    Dec(LHeld);
    Await(LWorker.Entered,'Timed list writer did not wake after the final owner release');
    FinishWorker(LWorker);
    Require(LOwner.Count=1,'Woken writer did not mutate the actual list');
  finally
    while LHeld>0 do begin
      LOwner.UnlockList;
      Dec(LHeld);
    end;
    try
      FinishWorker(LWorker);
    finally
      LOwner.Free;
    end;
  end;
end;

procedure CheckProtectedWrite;
var LLock: TReadWriteLock;
  LWorker: TMigrationWorker;
  LHeld: Integer;
begin
  LLock := TReadWriteLock.Create('migration.protect',lrpSupportsRecursion);
  LWorker := nil;
  LHeld := 0;
  try
    TPCThread.ProtectAcquireWriteLock(nil,LLock);
    Inc(LHeld);
    TPCThread.ProtectAcquireWriteLock(nil,LLock);
    Inc(LHeld);
    Require(LLock.RecursiveWriteCount=2,'Protected admission lost recursive write ownership');
    LWorker := TMigrationWorker.Create(mwaProtectedWrite,LLock,nil,nil,0);
    LWorker.StartOwned;
    WaitForWriter(LLock,LWorker);
    LLock.ReleaseWriteLock;
    Dec(LHeld);
    Require(LLock.WaitingWriteCount=1,'Partial recursive release woke the competing writer');
    LLock.ReleaseWriteLock;
    Dec(LHeld);
    Await(LWorker.Entered,'Protected writer did not wake after final release');
  finally
    while LHeld>0 do begin
      LLock.ReleaseWriteLock;
      Dec(LHeld);
    end;
    try
      FinishWorker(LWorker);
    finally
      LLock.Free;
    end;
  end;
end;

procedure CheckTimedWriteExclusion;
var LLock: TReadWriteLock;
  LWorker: TMigrationWorker;
  LHeld: Boolean;
begin
  LLock := TReadWriteLock.Create('migration.try',lrpSupportsRecursion);
  LWorker := nil;
  LHeld := False;
  try
    LLock.AcquireReadLock;
    LHeld := True;
    LWorker := TMigrationWorker.Create(mwaTimedWrite,LLock,nil,nil,0);
    LWorker.StartOwned;
    Await(LWorker.Done,'Nonblocking helper did not return');
    Require(LWorker.ErrorText='',LWorker.ErrorText);
    Require(not LWorker.Acquired,'Write helper bypassed another thread''s read ownership');
    Require(LLock.CurrentReadCount=1,'Failed helper admission changed the reader owner');
    Require(LLock.WaitingWriteCount=0,'Nonblocking helper left a queued writer');
  finally
    if LHeld then LLock.ReleaseReadLock;
    try
      FinishWorker(LWorker);
    finally
      LLock.Free;
    end;
  end;
end;

procedure CheckTimedWriteWake(ATimeout: Cardinal);
var LLock: TReadWriteLock;
  LWorker: TMigrationWorker;
  LHeld, LUnexpectedRead: Boolean;
begin
  LLock := TReadWriteLock.Create('migration.timed',lrpSupportsRecursion);
  LWorker := nil;
  LHeld := False;
  try
    LLock.AcquireWriteLock;
    LHeld := True;
    LWorker := TMigrationWorker.Create(mwaTimedWrite,LLock,nil,nil,ATimeout);
    LWorker.StartOwned;
    WaitForWriter(LLock,LWorker);
    Require(not LWorker.Acquired,'Timed helper acquired before the owner released');
    LLock.ReleaseWriteLock;
    LHeld := False;
    Await(LWorker.Entered,'Timed helper did not wake after owner release');
    Require(LWorker.Acquired,'Timed helper failed after a prompt owner release');
    LUnexpectedRead := LLock.TryAcquireReadLock(0);
    if LUnexpectedRead then LLock.ReleaseReadLock;
    Require(not LUnexpectedRead,'Acquired helper did not hold exclusive ownership');
  finally
    if LHeld then LLock.ReleaseWriteLock;
    try
      FinishWorker(LWorker);
    finally
      LLock.Free;
    end;
  end;
end;

procedure CheckUnsignedTimeouts;
begin
  // Values beyond signed Integer range exercise the helper's unsigned clamp.
  // Observe queued admission then release promptly instead of waiting a minute.
  CheckTimedWriteWake(60001);
  CheckTimedWriteWake(High(Cardinal)-1);
  CheckTimedWriteWake(High(Cardinal));
end;

procedure CheckOperationsRecursive;
var LOperations: TPCOperationsComp;
  LWorker: TMigrationWorker;
  LHeld: Integer;
begin
  // Operation construction hashes the empty tree; initialize its real backend
  // here so this case does not depend on another fixture having run first.
  TCrypto.InitCrypto;
  LOperations := TPCOperationsComp.Create(nil);
  LWorker := nil;
  LHeld := 0;
  try
    LOperations.Lock;
    Inc(LHeld);
    LOperations.Lock;
    Inc(LHeld);
    LOperations.Clear(True);
    Require(LOperations.Count=0,'Clear did not reenter the operation component');
    LWorker := TMigrationWorker.Create(mwaOperations,nil,nil,LOperations,0);
    LWorker.StartOwned;
    Await(LWorker.Attempting,'Operation-component writer did not start');
    Require(LWorker.Entered.WaitFor(150)=wrTimeout,'Operation writer bypassed nested ownership');
    LOperations.Unlock;
    Dec(LHeld);
    Require(LWorker.Entered.WaitFor(150)=wrTimeout,'Partial operation release lost the outer write');
    LOperations.Unlock;
    Dec(LHeld);
    Await(LWorker.Entered,'Operation writer did not wake after the outer lease ended');
  finally
    while LHeld>0 do begin
      LOperations.Unlock;
      Dec(LHeld);
    end;
    try
      FinishWorker(LWorker);
    finally
      LOperations.Free;
    end;
  end;
end;

procedure CheckOperationsDestruction;
var I: Integer;
  LOperations: TPCOperationsComp;
begin
  TCrypto.InitCrypto;
  for I := 1 to 8 do begin
    LOperations := TPCOperationsComp.Create(nil);
    try
      LOperations.Lock;
      try
        LOperations.Clear(True);
      finally
        LOperations.Unlock;
      end;
    finally
      // The real destructor calls Clear while holding its own recursive write;
      // it must balance that outer write before disposing the migrated lock.
      LOperations.Free;
    end;
  end;
end;

function CoreLockMigrationCheckCount: Integer;
begin
  Result := Ord(High(TCoreLockMigrationCheck))+1;
end;

function RunCoreLockMigrationCheck(ACheck: TCoreLockMigrationCheck; const ALogFile: String): Integer;
var LLog: TStringList;
begin
  Result := 1;
  Require(not FileExists(ALogFile),'Refusing an existing core-lock migration log');
  LLog := TStringList.Create;
  try
    try
      case ACheck of
        clmNamedDefault: CheckNamedDefault;
        clmNamedRecursive: CheckNamedRecursive;
        clmThreadListRecursive: CheckThreadListRecursive;
        clmThreadListTryExclusion: CheckThreadListTryExclusion;
        clmThreadListCountExclusion: CheckThreadListCountExclusion;
        clmThreadListTimedWake: CheckThreadListTimedWake;
        clmProtectedWrite: CheckProtectedWrite;
        clmTimedWriteExclusion: CheckTimedWriteExclusion;
        clmTimedWriteWake: CheckTimedWriteWake(2500);
        clmUnsignedTimeouts: CheckUnsignedTimeouts;
        clmOperationsRecursive: CheckOperationsRecursive;
        clmOperationsDestruction: CheckOperationsDestruction;
      end;
      LLog.Add(Format('PASS core-lock migration %d',[Ord(ACheck)+1]));
      Result := 0;
    except
      on E: Exception do LLog.Add('FAIL '+E.ClassName+': '+E.Message);
    end;
    LLog.SaveToFile(ALogFile);
  finally
    LLog.Free;
  end;
end;

function RunCoreLockMigrationChecks(const ALogFile: String): Integer;
var LCheck: TCoreLockMigrationCheck;
  LLog: TStringList;
begin
  Result := 1;
  Require(not FileExists(ALogFile),'Refusing an existing core-lock migration summary');
  LLog := TStringList.Create;
  try
    for LCheck := Low(TCoreLockMigrationCheck) to High(TCoreLockMigrationCheck) do begin
      if RunCoreLockMigrationCheck(LCheck,ALogFile+'.'+IntToStr(Ord(LCheck)+1)+'.log')<>0 then begin
        LLog.Add(Format('FAIL core-lock migration %d; see scenario log',[Ord(LCheck)+1]));
        LLog.SaveToFile(ALogFile);
        Exit;
      end;
      LLog.Add(Format('PASS core-lock migration %d',[Ord(LCheck)+1]));
      LLog.SaveToFile(ALogFile);
    end;
    LLog.Add(Format('PASS total: %d',[CoreLockMigrationCheckCount]));
    LLog.SaveToFile(ALogFile);
    Result := 0;
  finally
    LLog.Free;
  end;
end;

end.
