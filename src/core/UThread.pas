unit UThread;

{ Copyright (c) 2016 by Albert Molina

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com> author of TPipelineQueue (2019)
    Herman Schoenfeld <https://sphere10.com> author of TReadWriteLock (2026)

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFnDEF FPC}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
{$ELSE}
  {$IFDEF LINUX}cthreads,{$ENDIF}
{$ENDIF}
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  Classes, SyncObjs, SysUtils, UBaseTypes, UConst;

{$I ./../config.inc}

Type
  TPCCriticalSection = Class(TCriticalSection)
  private
    FCounterLock : TCriticalSection;
    FWaitingForCounter : Integer;
    FCurrentThread : Cardinal;
    FStartedTickCount : TTickCount;
    FName : String;
  public
    Constructor Create(const AName : String);
    Destructor Destroy; override;
    {$IFDEF HIGHLOG}
    procedure Acquire; override;
    procedure Release; override;
    function TryEnter: Boolean; { HS - had 'override' in development }
    {$ENDIF}
    Property CurrentThread : Cardinal read FCurrentThread;
    Property WaitingForCounter : Integer read FWaitingForCounter;
    Property StartedTickCount : TTickCount read FStartedTickCount;  // Renamed from StartedTimestamp to StartedTickCount to avoid confusion
    Property Name : String read FName;
  end;

  { Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
    TReadWriteLock is ported from PascalFramework's MIT-licensed UConcurrency.
    Its core implementation uses existing PascalCoin thread identity, admission
    diagnostics and monotonic timing, and imports no framework unit. }
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
    FName: String;
    // UConst makes the existing core thread-ID type visible in both interface
    // and implementation; an implementation-only import cannot change it here.
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
    function CanAcquire(AKind: TLockKind; AThreadID: TThreadID;
      const ACounts: TThreadCounts): Boolean;
    procedure Grant(AKind: TLockKind; AThreadID: TThreadID;
      var ACounts: TThreadCounts);
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
    constructor Create(ARecursionPolicy: TLockRecursionPolicy = lrpNoRecursion); overload;
    constructor Create(const AName: String; ARecursionPolicy: TLockRecursionPolicy = lrpNoRecursion); overload;
    destructor Destroy; override;
    procedure Dispose;
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
    property Name: String read FName;
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

  TPCThread = Class;
  TPCThreadClass = Class of TPCThread;
  TPCThread = Class(TThread)
  private
    FDebugStep: String;
    FStartTickCount : TTickCount;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure BCExecute; virtual; abstract;
  public
    Class function ThreadClassFound(tclass : TPCThreadClass; Exclude : TObject) : Integer;
    Class function ThreadCount : Integer;
    Class function GetThread(index : Integer) : TPCThread;
    Class function GetThreadByClass(tclass : TPCThreadClass; Exclude : TObject) : TPCThread;
    Class Procedure ProtectEnterCriticalSection(Const Sender : TObject; var Lock : TPCCriticalSection);
    Class Function TryProtectEnterCriticalSection(Const Sender : TObject; MaxWaitMilliseconds : Cardinal; var Lock : TPCCriticalSection) : Boolean;
    Class Procedure ProtectAcquireWriteLock(const Sender: TObject; const Lock: TReadWriteLock);
    Class Function TryProtectAcquireWriteLock(const Sender: TObject; MaxWaitMilliseconds: Cardinal; const Lock: TReadWriteLock): Boolean;
    Class Procedure ThreadsListInfo(list: TStrings);
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    Property DebugStep : String read FDebugStep write FDebugStep;
    Property StartTickCount : TTickCount read FStartTickCount;
    property Terminated;
  End;

  TPCThreadList<T> = class
  private
    FList: TList<T>;
    FLock: TReadWriteLock;
  public
    constructor Create(const AName : String);
    destructor Destroy; override;
    function Add(Item: T) : Integer;
    function Count : Integer;
    procedure Clear;
    procedure Remove(Item: T); inline;
    function LockList: TList<T>;
    function TryLockList(MaxWaitMilliseconds : Cardinal; var lockedList : TList<T>) : Boolean;
    procedure UnlockList; inline;
  end;

  { TPipelineQueue }

  TPipelineQueue<T> = class(TComponent)
  private type

    { TStageQueue }

    TStageQueue = class
    private
      FDirty : Boolean;
      FLock : TReadWriteLock;
      FItems : TList<T>;
      function GetDirty : Boolean;
      procedure SetDirty (AValue : Boolean);
      function GetItems : TArray<T>;
    public
      constructor Create; overload;
      destructor Destroy; override;
      property Dirty : Boolean read GetDirty write SetDirty;
      property Lock : TReadWriteLock read FLock;
      property Items : TArray<T> read GetItems;
    end;

    { TErrorResult }

    TErrorResult = record
     Item : T;
     ErrorMessage : String;
    end;

    { TPipelineWorkerThread}

    TPipelineWorkerThread = class(TPCThread)
    private
      FPipeline : TPipelineQueue<T>;
      FStage : Integer;
    protected
      procedure BCExecute; override;
    public
      constructor Create(const APipelineQueue : TPipelineQueue<T>; AStage : Integer); overload;
    end;

  private
    FQueues : TList<TStageQueue>;
    FMaxWorkerThreads : Integer;
    FActiveWorkerThreads : Integer;
    {$IFDEF UNITTESTS}
    FHistoricalMaxActiveWorkerThreads : Integer;
    {$ENDIF}
    procedure Initialize(AStageCount : Integer; AMaxWorkerThreadCount : Integer);
    procedure Enqueue(AStage : Integer; const AItem : T); overload;
    procedure EnqueueRange(AStage : Integer; const AItems : array of T); overload;
    procedure NotifyPipelineAppended(AStage : Integer);
    function GetStageCount : Integer; inline;
    function GetHasCompleted : Boolean;
  protected
    function ProcessStage(AStageNum : Integer; const AItems : TArray<T>; out AErrors : TArray<TErrorResult>) : TArray<T>; virtual; abstract;
    procedure HandleErrorItems(const AErrorItems : array of TErrorResult); virtual; abstract;
    procedure HandleFinishedItems(const AItems : array of T); virtual; abstract;
  public
    property StageCount : Integer read GetStageCount;
    property HasCompleted : Boolean read GetHasCompleted;
    {$IFDEF UNITTESTS}
    property HistoricalMaxActiveWorkerThreads : Integer read FHistoricalMaxActiveWorkerThreads;
    {$ENDIF}
    constructor Create(AOwner : TComponent; AStageCount, AMaxWorkerThreads : Integer); overload;
    destructor Destroy; override;
    procedure Enqueue(const AItem : T); overload;
    procedure EnqueueRange(const AItems : array of T); overload;
  end;

implementation

uses
  ULog;

{ TPCThread }

Var _threads : TPCThreadList<TPCThread>;

constructor TPCThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Created Thread '+IntToHex(PtrInt(Self),16));{$ENDIF}
end;

destructor TPCThread.Destroy;
begin
  inherited;
end;

procedure TPCThread.DoTerminate;
begin
  inherited;
end;

procedure TPCThread.Execute;
Var l : TList<TPCThread>;
  i : Integer;
begin
  FStartTickCount := TPlatform.GetTickCount;
  FDebugStep := '';
  i := _threads.Add(Self);
  try
    {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Starting Thread '+IntToHex(PtrInt(Self),16)+' in pos '+inttostr(i+1));{$ENDIF}
    Try
      Try
        BCExecute;
        FDebugStep := 'Finalized BCExecute';
      Finally
        Terminate;
      End;
    Except
      On E:Exception do begin
        TLog.NewLog(lterror,Classname,'Exception inside a Thread at step: '+FDebugStep+' ('+E.ClassName+'): '+E.Message);
        Raise;
      end;
    End;
  finally
    if Assigned(_threads) then begin
      l := _threads.LockList;
      Try
        i := l.Remove(Self);
        {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Finalizing Thread in pos '+inttostr(i+1)+'/'+inttostr(l.Count+1)+' working time: '+FormatFloat('0.000',TPlatform.GetElapsedMilliseconds(FStartTickCount) / 1000)+' sec');{$ENDIF}
      Finally
        _threads.UnlockList;
      End;
    end;
  end;
end;

class function TPCThread.GetThread(index: Integer): TPCThread;
Var l : TList<TPCThread>;
begin
  Result := Nil;
  l := _threads.LockList;
  try
    if (index<0) or (index>=l.Count) then exit;
    Result := TPCThread(l[index]);
  finally
    _threads.UnlockList;
  end;
end;

class function TPCThread.GetThreadByClass(tclass: TPCThreadClass; Exclude: TObject): TPCThread;
Var l : TList<TPCThread>;
  i : Integer;
begin
  Result := Nil;
  if Not Assigned(_threads) then exit;
  l := _threads.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      if (TPCThread(l[i]) is tclass) And ((l[i])<>Exclude) then begin
        Result := TPCThread(l[i]);
        exit;
      end;
    end;
  finally
    _threads.UnlockList;
  end;
end;

class procedure TPCThread.ProtectEnterCriticalSection(Const Sender : TObject; var Lock: TPCCriticalSection);
begin
  {$IFDEF HIGHLOG}
  if Not Lock.TryEnter then begin
    Lock.Acquire;
  end;
  {$ELSE}
  Lock.Acquire;
  {$ENDIF}
end;

class function TPCThread.ThreadClassFound(tclass: TPCThreadClass; Exclude : TObject): Integer;
Var l : TList<TPCThread>;
begin
  Result := -1;
  if Not Assigned(_threads) then exit;
  l := _threads.LockList;
  try
    for Result := 0 to l.Count - 1 do begin
      if (TPCThread(l[Result]) is tclass) And ((l[Result])<>Exclude) then exit;
    end;
    Result := -1;
  finally
    _threads.UnlockList;
  end;
end;

class function TPCThread.ThreadCount: Integer;
Var l : TList<TPCThread>;
begin
  l := _threads.LockList;
  try
    Result := l.Count;
  finally
    _threads.UnlockList;
  end;
end;

class procedure TPCThread.ThreadsListInfo(list: TStrings);
Var l : TList<TPCThread>;
  i : Integer;
begin
  l := _threads.LockList;
  try
    list.BeginUpdate;
    list.Clear;
    for i := 0 to l.Count - 1 do begin
      list.Add(Format('%.2d/%.2d <%s> Time:%s sec - Step: %s',[i+1,l.Count,TPCThread(l[i]).ClassName,FormatFloat('0.000',(TPlatform.GetElapsedMilliseconds(TPCThread(l[i]).FStartTickCount) / 1000)),TPCThread(l[i]).DebugStep] ));
    end;
    list.EndUpdate;
  finally
    _threads.UnlockList;
  end;
end;

class function TPCThread.TryProtectEnterCriticalSection(const Sender: TObject;
  MaxWaitMilliseconds: Cardinal; var Lock: TPCCriticalSection): Boolean;
Var tc : TTickCount;
  {$IFDEF HIGHLOG}
  tc2,tc3,lockStartedTimestamp : TTickCount;
  lockCurrThread : TThreadID;
  lockWatingForCounter : Cardinal;
  s : String;
  {$ENDIF}
begin
  tc := TPlatform.GetTickCount;
  if MaxWaitMilliseconds>60000 then MaxWaitMilliseconds := 60000;
  {$IFDEF HIGHLOG}
  lockWatingForCounter := Lock.WaitingForCounter;
  lockStartedTimestamp := Lock.StartedTickCount;
  lockCurrThread := Lock.CurrentThread;
  {$ENDIF}
  Repeat
    Result := Lock.TryEnter;
    if Not Result then sleep(1);
  Until (Result) Or (TPlatform.GetElapsedMilliseconds(tc)>MaxWaitMilliseconds);
  {$IFDEF HIGHLOG}
  if Not Result then begin
    tc2 := TPlatform.GetTickCount;
    if lockStartedTimestamp=0 then lockStartedTimestamp := Lock.StartedTickCount;
    if lockStartedTimestamp=0 then tc3 := 0
    else tc3 := tc2-lockStartedTimestamp;
    s := Format('Cannot Protect a critical section %s %s class %s after %d milis locked by %s waiting %d-%d elapsed milis: %d',
      [IntToHex(PtrInt(Lock),16),Lock.Name,
      Sender.ClassName,tc2-tc,
      IntToHex(lockCurrThread,16)+'-'+IntToHex(Lock.CurrentThread,16),
      lockWatingForCounter,Lock.WaitingForCounter,
      tc3
      ]);
    TLog.NewLog(ltdebug,Classname,s);
  end;
  {$ENDIF}
end;

class procedure TPCThread.ProtectAcquireWriteLock(const Sender: TObject; const Lock: TReadWriteLock);
begin
  Lock.AcquireWriteLock;
end;

class function TPCThread.TryProtectAcquireWriteLock(const Sender: TObject; MaxWaitMilliseconds: Cardinal; const Lock: TReadWriteLock): Boolean;
{$IFDEF HIGHLOG}
var LSenderName: String;
{$ENDIF}
begin
  // Retain the legacy helper's cap before converting the unsigned timeout.
  // The read/write lock waits on its event instead of polling every millisecond.
  if MaxWaitMilliseconds > 60000 then MaxWaitMilliseconds := 60000;
  Result := Lock.TryAcquireWriteLock(Integer(MaxWaitMilliseconds));
  {$IFDEF HIGHLOG}
  if not Result then begin
    if Assigned(Sender) then LSenderName := Sender.ClassName else LSenderName := '';
    TLog.NewLog(ltdebug,ClassName,Format('Cannot acquire write lock %s for %s after %d milliseconds; waiting writers: %d',
      [Lock.Name,LSenderName,MaxWaitMilliseconds,Lock.WaitingWriteCount]));
  end;
  {$ENDIF}
end;

{ TPCThreadList }

function TPCThreadList<T>.Add(Item: T) : Integer;
begin
  LockList;
  Try
    Result := FList.Add(Item);
  Finally
    UnlockList;
  End;
end;

procedure TPCThreadList<T>.Clear;
begin
  LockList;
  Try
    FList.Clear;
  Finally
    UnlockList;
  End;
end;

constructor TPCThreadList<T>.Create(const AName : String);
begin
  FLock := TReadWriteLock.Create(AName,lrpSupportsRecursion);
  FList := TList<T>.Create;
end;

function TPCThreadList<T>.Count : Integer;
begin
  FLock.AcquireReadLock;
  try
    Result := FList.Count;
  finally
    FLock.ReleaseReadLock;
  end;
end;

destructor TPCThreadList<T>.Destroy;
begin
  LockList;
  try
    FreeAndNil(FList);
    inherited Destroy;
  finally
    UnlockList;
    FreeAndNil(FLock);
  end;
end;

function TPCThreadList<T>.LockList: TList<T>;
begin
  // This API returns a mutable list, so even callers inspecting it own WRITE.
  TPCThread.ProtectAcquireWriteLock(Self,FLock);
  Result := FList;
end;

procedure TPCThreadList<T>.Remove(Item: T);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

function TPCThreadList<T>.TryLockList(MaxWaitMilliseconds: Cardinal; var lockedList: TList<T>): Boolean;
begin
  lockedList := FList;
  Result := TPCThread.TryProtectAcquireWriteLock(Self,MaxWaitMilliseconds,FLock);
end;

procedure TPCThreadList<T>.UnlockList;
begin
  FLock.ReleaseWriteLock;
end;

{ TPCCriticalSection }

{$IFDEF HIGHLOG}
procedure TPCCriticalSection.Acquire;
Var continue, logged : Boolean;
  startTC, LLastTC : TTickCount;
  LWaitMillis : Int64;
begin
  startTC := TPlatform.GetTickCount;
  LLastTC := startTC;
  FCounterLock.Acquire;
  try
    FWaitingForCounter := FWaitingForCounter + 1;
  finally
    FCounterLock.Release;
  end;
  logged := false;
  LWaitMillis := 1000;
  Repeat
    continue := inherited TryEnter;
    if (Not continue) then begin
      If (TPlatform.GetElapsedMilliseconds(LLastTC)>LWaitMillis) then begin
        LLastTC := TPlatform.GetTickCount;
        inc(LWaitMillis,LWaitMillis);
        logged := true;
        TLog.NewLog(ltdebug,ClassName,'ALERT Critical section '+IntToHex(PtrInt(Self),16)+' '+Name+
          ' locked by '+IntToHex(FCurrentThread,16)+' waiting '+
          IntToStr(FWaitingForCounter)+' elapsed milis: '+IntToStr(TPlatform.GetElapsedMilliseconds(startTC)));
      end else sleep(1);
      sleep(1);
    end;
  Until continue;
  if (logged) then begin
    TLog.NewLog(ltdebug,Classname,'ENTER Critical section '+IntToHex(PtrInt(Self),16)+' '+Name+' elapsed milis: '+IntToStr(TPlatform.GetElapsedMilliseconds(startTC)) );
  end;
  FCounterLock.Acquire;
  try
    FWaitingForCounter := FWaitingForCounter - 1;
  finally
    FCounterLock.Release;
  end;
  FCurrentThread := TThread.CurrentThread.ThreadID;
  FStartedTickCount := TPlatform.GetTickCount;
end;
{$ENDIF}

constructor TPCCriticalSection.Create(const AName : String);
begin
  FCounterLock := TCriticalSection.Create;
  FWaitingForCounter := 0;
  FCurrentThread := 0;
  FStartedTickCount := 0;
  FName := AName;
  inherited Create;
  {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,ClassName,'Created critical section '+IntToHex(PtrInt(Self),16)+' '+AName );{$ENDIF}
end;

destructor TPCCriticalSection.Destroy;
begin
  FCounterLock.Free;
  inherited;
end;

{$IFDEF HIGHLOG}
procedure TPCCriticalSection.Release;
begin
  FCurrentThread := 0;
  FStartedTickCount := 0;
  inherited;
end;

function TPCCriticalSection.TryEnter: Boolean;
begin
  FCounterLock.Acquire;
  try
    FWaitingForCounter := FWaitingForCounter + 1;
  finally
    FCounterLock.Release;
  end;
  If inherited TryEnter then begin
    FCurrentThread := TThread.CurrentThread.ThreadID;
    FStartedTickCount := TPlatform.GetTickCount;
    Result := true;
  end else Result := false;
  FCounterLock.Acquire;
  try
    FWaitingForCounter := FWaitingForCounter - 1;
  finally
    FCounterLock.Release;
  end;
end;
{$ENDIF}

{ TPipelineQueue }

constructor TPipelineQueue<T>.Create(AOwner : TComponent; AStageCount, AMaxWorkerThreads : Integer);
begin
  inherited Create(AOwner);
  Initialize(AStageCount, AMaxWorkerThreads);
end;

destructor TPipelineQueue<T>.Destroy;
var i : Integer;
begin
  inherited;
  for i := 0 to FQueues.Count - 1 do
    FQueues[i].Destroy;
  FreeAndNil(FQueues);
end;

procedure TPipelineQueue<T>.Initialize(AStageCount : Integer; AMaxWorkerThreadCount : Integer);
var i : integer;
begin
  if AStageCount <= 0 then raise EArgumentException.Create('AStageCount must be greater than 0');
  if AMaxWorkerThreadCount <= 0 then raise EArgumentException.Create('AMaxWorkerThreadCount must be greater than 0');
  FMaxWorkerThreads := AMaxWorkerThreadCount;
  FActiveWorkerThreads := 0;
  FQueues := TList<TStageQueue>.Create;
  for i := 0 to AStageCount - 1 do begin
    FQueues.Add(  TStageQueue.Create );
  end;
end;

procedure TPipelineQueue<T>.Enqueue(AStage : Integer; const AItem : T);
begin
  EnqueueRange(AStage, [AItem]);
end;

procedure TPipelineQueue<T>.EnqueueRange(AStage : Integer; const AItems : array of T);
begin
  FQueues[AStage].Lock.AcquireWriteLock;
  try
    FQueues[AStage].FDirty := True;
    FQueues[AStage].FItems.AddRange(AItems);
  finally
    FQueues[AStage].Lock.ReleaseWriteLock;
  end;
  NotifyPipelineAppended(AStage);
end;

procedure TPipelineQueue<T>.Enqueue(const AItem : T);
begin
  Enqueue(0, AItem);
end;

procedure TPipelineQueue<T>.EnqueueRange(const AItems : array of T);
begin
  EnqueueRange(0, AItems);
end;

procedure TPipelineQueue<T>.NotifyPipelineAppended(AStage : Integer);
begin
  if (FActiveWorkerThreads = 0) OR (FActiveWorkerThreads < FMaxWorkerThreads) then begin
    // Start a new worker thread to process
    TPipelineWorkerThread.Create(Self, AStage);
    {$IFDEF UNITTESTS}
    if (FActiveWorkerThreads > FHistoricalMaxActiveWorkerThreads) then
      FHistoricalMaxActiveWorkerThreads := FActiveWorkerThreads;
    {$ENDIF}
  end;
end;

function TPipelineQueue<T>.GetStageCount : Integer;
begin
  Result := FQueues.Count;
end;

function TPipelineQueue<T>.GetHasCompleted : Boolean;
var i : Integer;
begin
  if FActiveWorkerThreads > 0 then Exit(False);
  for i := 0 to FQueues.Count - 1 do
    if FQueues[i].Dirty then Exit(False);
  Result := true;
end;

{ TPipelineQueue<T>.TStageQueue }

constructor TPipelineQueue<T>.TStageQueue.Create;
begin
  inherited;
  FDirty := False;
  FLock := TReadWriteLock.Create('Pipeline.Stage',lrpSupportsRecursion);
  FItems := TList<T>.Create;
end;

destructor TPipelineQueue<T>.TStageQueue.Destroy;
begin
  inherited;
  FreeAndNil(FLock);
  FreeAndNil(FItems);
end;

function TPipelineQueue<T>.TStageQueue.GetDirty : Boolean;
begin
  FLock.AcquireReadLock;
  try
    Result := FDirty;
  finally
    FLock.ReleaseReadLock;
  end;
end;

procedure TPipelineQueue<T>.TStageQueue.SetDirty ( AValue : Boolean );
begin
  FLock.AcquireWriteLock;
  try
    FDirty := AValue;
  finally
    FLock.ReleaseWriteLock;
  end;
end;

function TPipelineQueue<T>.TStageQueue.GetItems : TArray<T>;
begin
  begin
    FLock.AcquireReadLock;
    try
      Result := FItems.ToArray;
    finally
      FLock.ReleaseReadLock;
    end;
  end;
end;

{ TPipelineQueue<T>.TPipelineWorkerThread }

constructor TPipelineQueue<T>.TPipelineWorkerThread.Create(const APipelineQueue : TPipelineQueue<T>; AStage : Integer);
begin
 inherited Create(True);
 Self.FreeOnTerminate := true;
 FPipeline := APipelineQueue;
 FStage := AStage;
 Inc(FPipeline.FActiveWorkerThreads);
 Self.Start;
end;

procedure TPipelineQueue<T>.TPipelineWorkerThread.BCExecute;
var
  i, j : Integer;
  LHasMore : Boolean;
  LIn : TArray<T>;
  LOut : TArray<T>;
  LErrorOut : TArray<TErrorResult>;
begin
  try
    repeat
      // protect against excessive worker threads
      if FPipeline.FActiveWorkerThreads > FPipeline.FMaxWorkerThreads then exit;

      // double-check ensure still dirty
      if not FPipeline.FQueues[FStage].FDirty then exit;

      // Extract items from pipeline stage
      FPipeline.FQueues[FStage].Lock.AcquireWriteLock;
      try
        LIn := FPipeline.FQueues[FStage].FItems.ToArray;
        FPipeline.FQueues[FStage].FItems.Clear;
        FPipeline.FQueues[FStage].FDirty := False;
      finally
        FPipeline.FQueues[FStage].Lock.ReleaseWriteLock;
      end;

      // process items
      LOut := FPipeline.ProcessStage(FStage, LIn, LErrorOut);

      // process errors
      if Length(LErrorOut) > 0 then
        FPipeline.HandleErrorItems(LErrorOut);

      // send output to next queue (or finish)
      if FStage < FPipeline.StageCount - 1 then
        FPipeline.EnqueueRange(FStage + 1, LOut)
      else
        FPipeline.HandleFinishedItems(LOut);

      // keep working until all stages are completed
      LHasMore := False;
      for i := 0 to FPipeline.FQueues.Count - 1 do begin
        if FPipeline.FQueues[i].Dirty then begin
          FStage := i;
          LHasMore := True;
          break;
        end;
      end;
    until not LHasMore;
  finally
    Dec(FPipeline.FActiveWorkerThreads);
  end;
end;

{ TReadWriteLock }

constructor TReadWriteLock.Create(ARecursionPolicy: TLockRecursionPolicy);
begin
  inherited Create;
  FPolicy := ARecursionPolicy;
  // Admission must use the lower-level primitive. Its contention path must not
  // log: the logger itself uses this lock, including in HIGHLOG builds.
  FGate := TCriticalSection.Create;
  FThreads := TDictionary<TThreadID, TThreadCounts>.Create;
  FWaiters := TList<TLockWaiter>.Create;
end;

constructor TReadWriteLock.Create(const AName: String; ARecursionPolicy: TLockRecursionPolicy);
begin
  Create(ARecursionPolicy);
  FName := AName;
end;

destructor TReadWriteLock.Destroy;
begin
  if Assigned(FGate) then
    Dispose;
  FWaiters.Free;
  FThreads.Free;
  FGate.Free;
  inherited;
end;

procedure TReadWriteLock.Dispose;
begin
  FGate.Acquire;
  try
    if FDisposed then
      Exit;
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

procedure TReadWriteLock.CheckAcquisition(AKind: TLockKind;
  const ACounts: TThreadCounts);
var
  LInvalid: Boolean;
begin
  if FDisposed then
    raise ELockDisposedException.Create('The lock is disposed.');
  // An ordinary reader cannot promote, even with recursion enabled: two such
  // readers would each wait for the other. Choose upgradeable mode in advance.
  if FPolicy = lrpNoRecursion then
    case AKind of
      lkRead: LInvalid := (ACounts.Reads > 0) or (ACounts.Writes > 0);
      lkWrite: LInvalid := (ACounts.Writes > 0) or (ACounts.Reads > 0);
      lkUpgradeable:
        LInvalid := (ACounts.Reads > 0) or (ACounts.Writes > 0) or
        (ACounts.Upgrades > 0);
    end
  else
    LInvalid := (AKind <> lkRead) and (ACounts.Reads > 0) and
      (ACounts.Writes = 0) and (ACounts.Upgrades = 0);
  if LInvalid then
    raise ELockRecursionException.Create('This acquisition violates the lock recursion policy.');
end;

function TReadWriteLock.CanAcquire(AKind: TLockKind; AThreadID: TThreadID;
  const ACounts: TThreadCounts): Boolean;
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

procedure TReadWriteLock.Grant(AKind: TLockKind; AThreadID: TThreadID;
  var ACounts: TThreadCounts);
var
  LFirst: Boolean;
begin
  LFirst := False;
  case AKind of
    lkRead:
      begin
        LFirst := ACounts.Reads = 0;
        Inc(ACounts.Reads);
      end;
    lkWrite: Inc(ACounts.Writes);
    lkUpgradeable: Inc(ACounts.Upgrades);
  end;
  // Publish counters only after the dictionary allocation has succeeded.
  FThreads.AddOrSetValue(AThreadID, ACounts);
  case AKind of
    lkRead:
      if LFirst then
        Inc(FReaders);
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
var
  LWaiter: TLockWaiter;
begin
  // Per-waiter auto-reset events avoid lost wakes and repeated wakes without a
  // state change. Admission is rechecked under the gate, with writer priority.
  for LWaiter in FWaiters do
    LWaiter.Signal.SetEvent;
end;

function TReadWriteLock.TryAcquire(AKind: TLockKind;
  AMillisecondsTimeout: Integer): Boolean;
var
  LThreadID: TThreadID;
  LCounts: TThreadCounts;
  LWaiter: TLockWaiter;
  LStarted: TTickCount;
  LElapsed: Int64;
  LRemaining: Cardinal;
  LWaitResult: TWaitResult;
begin
  if AMillisecondsTimeout < -1 then
    raise EArgumentOutOfRangeException.Create('Timeout must be -1 or a nonnegative number of milliseconds.');
  // Keep the original deadline across state-change wakes; contention must not
  // restart a caller's finite timeout. Use the core's monotonic tick helpers.
  LStarted := TPlatform.GetTickCount;
  LThreadID := TThread.CurrentThread.ThreadID;
  LWaiter := nil;
  FGate.Acquire;
  try
    LCounts := ThreadCounts(LThreadID);
    CheckAcquisition(AKind, LCounts);
    while True do
    begin
      if CanAcquire(AKind, LThreadID, LCounts) then
      begin
        Grant(AKind, LThreadID, LCounts);
        Exit(True);
      end;
      if AMillisecondsTimeout = 0 then
        Exit(False);
      if AMillisecondsTimeout = -1 then
        LRemaining := High(Cardinal)
      else
      begin
        LElapsed := TPlatform.GetElapsedMilliseconds(LStarted);
        if LElapsed >= AMillisecondsTimeout then
          Exit(False);
        LRemaining := Cardinal(Int64(AMillisecondsTimeout) - LElapsed);
      end;
      if not Assigned(LWaiter) then
      begin
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
      if LWaitResult = wrTimeout then
        Exit(False);
      if LWaitResult <> wrSignaled then
        raise ESynchronizationLockException.Create('Waiting for lock ownership failed.');
      LCounts := ThreadCounts(LThreadID);
    end;
  finally
    if Assigned(LWaiter) then
    begin
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
var
  LThreadID: TThreadID;
  LCounts: TThreadCounts;
  LCount: Integer;
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
    else
      LCount := 0;
    end;
    if LCount = 0 then
      raise ESynchronizationLockException.Create('This thread does not hold the requested lock.');
    case AKind of
      lkRead:
        begin
          Dec(LCounts.Reads);
          if LCounts.Reads = 0 then
            Dec(FReaders);
        end;
      lkWrite:
        begin
          Dec(LCounts.Writes);
          if LCounts.Writes = 0 then
            FWriter := Default(TThreadID);
        end;
      lkUpgradeable:
        begin
          Dec(LCounts.Upgrades);
          if LCounts.Upgrades = 0 then
            FUpgrader := Default(TThreadID);
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
var
  LCounts: TThreadCounts;
begin
  FGate.Acquire;
  try
    LCounts := ThreadCounts(TThread.CurrentThread.ThreadID);
    case AKind of
      lkRead: Result := LCounts.Reads;
      lkWrite: Result := LCounts.Writes;
      lkUpgradeable: Result := LCounts.Upgrades;
    else
      Result := 0;
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
    else
      Result := 0;
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

initialization
  _threads := TPCThreadList<TPCThread>.Create('GLOBAL_THREADS');
finalization
  FreeAndNil(_threads);
end.

