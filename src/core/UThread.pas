unit UThread;

{ Copyright (c) 2016 by Albert Molina

  Acknowledgements:
    Herman Schoenfeld <herman@sphere10.com> author of TPipelineQueue (2019)

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
  Classes, SyncObjs, SysUtils, UBaseTypes;

{$I config.inc}

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
    Class Procedure ThreadsListInfo(list: TStrings);
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    Property DebugStep : String read FDebugStep write FDebugStep;
    property Terminated;
  End;

  TPCThreadList<T> = class
  private
    FList: TList<T>;
    FLock: TPCCriticalSection;
  public
    constructor Create(const AName : String);
    destructor Destroy; override;
    function Add(Item: T) : Integer;
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
      FLock : TMultiReadExclusiveWriteSynchronizer;
      FItems : TList<T>;
      function GetDirty : Boolean;
      procedure SetDirty (AValue : Boolean);
      function GetItems : TArray<T>;
    public
      constructor Create; overload;
      destructor Destroy; override;
      property Dirty : Boolean read GetDirty write SetDirty;
      property Lock : TMultiReadExclusiveWriteSynchronizer read FLock;
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
  ULog, UConst;

{ TPCThread }

Var _threads : TPCThreadList<TPCThread>;

constructor TPCThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Created Thread '+IntToHex(PtrInt(Self),8));{$ENDIF}
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
    {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Starting Thread '+IntToHex(PtrInt(Self),8)+' in pos '+inttostr(i+1));{$ENDIF}
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
      [IntToHex(PtrInt(Lock),8),Lock.Name,
      Sender.ClassName,tc2-tc,
      IntToHex(lockCurrThread,8)+'-'+IntToHex(Lock.CurrentThread,8),
      lockWatingForCounter,Lock.WaitingForCounter,
      tc3
      ]);
    TLog.NewLog(ltdebug,Classname,s);
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
  FLock := TPCCriticalSection.Create(AName);
  FList := TList<T>.Create;
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
  TPCThread.ProtectEnterCriticalSection(Self,FLock);
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
  Result := TPCThread.TryProtectEnterCriticalSection(Self,MaxWaitMilliseconds,FLock);
end;

procedure TPCThreadList<T>.UnlockList;
begin
  FLock.Release;
end;

{ TPCCriticalSection }

{$IFDEF HIGHLOG}
procedure TPCCriticalSection.Acquire;
Var continue, logged : Boolean;
  startTC : TTickCount;
begin
  startTC := TPlatform.GetTickCount;
  FCounterLock.Acquire;
  try
    FWaitingForCounter := FWaitingForCounter + 1;
  finally
    FCounterLock.Release;
  end;
  logged := false;
  Repeat
    continue := inherited TryEnter;
    if (Not continue) then begin
      If (not logged) And (TPlatform.GetElapsedMilliseconds(startTC)>1000) then begin
        logged := true;
        TLog.NewLog(ltdebug,ClassName,'ALERT Critical section '+IntToHex(PtrInt(Self),8)+' '+Name+
          ' locked by '+IntToHex(FCurrentThread,8)+' waiting '+
          IntToStr(FWaitingForCounter)+' elapsed milis: '+IntToStr(TPlatform.GetElapsedMilliseconds(startTC)));
        continue := true;
        inherited;
      end else sleep(1);
    end;
  Until continue;
  if (logged) then begin
    TLog.NewLog(ltdebug,Classname,'ENTER Critical section '+IntToHex(PtrInt(Self),8)+' '+Name+' elapsed milis: '+IntToStr(TPlatform.GetElapsedMilliseconds(startTC)) );
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
  {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,ClassName,'Created critical section '+IntToHex(PtrInt(Self),8)+' '+AName );{$ENDIF}
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
  FQueues[AStage].Lock.BeginWrite;
  try
    FQueues[AStage].FDirty := True;
    FQueues[AStage].FItems.AddRange(AItems);
  finally
    FQueues[AStage].Lock.EndWrite;
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
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
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
  FLock.BeginRead;
  try
    Result := FDirty;
  finally
    FLock.EndRead;
  end;
end;

procedure TPipelineQueue<T>.TStageQueue.SetDirty ( AValue : Boolean );
begin
  FLock.BeginWrite;
  try
    FDirty := AValue;
  finally
    FLock.EndWrite;
  end;
end;

function TPipelineQueue<T>.TStageQueue.GetItems : TArray<T>;
begin
  begin
    FLock.BeginRead;
    try
      Result := FItems.ToArray;
    finally
      FLock.EndRead;
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
      FPipeline.FQueues[FStage].Lock.BeginWrite;
      try
        LIn := FPipeline.FQueues[FStage].FItems.ToArray;
        FPipeline.FQueues[FStage].FItems.Clear;
        FPipeline.FQueues[FStage].FDirty := False;
      finally
        FPipeline.FQueues[FStage].Lock.EndWrite;
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

initialization
  _threads := TPCThreadList<TPCThread>.Create('GLOBAL_THREADS');
finalization
  FreeAndNil(_threads);
end.

