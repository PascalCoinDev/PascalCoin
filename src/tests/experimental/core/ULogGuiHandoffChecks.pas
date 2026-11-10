unit ULogGuiHandoffChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Checks the actual logger's producer-to-GUI handoff without starting a node. }

{$IFDEF FPC}{$MODE Delphi}{$H+}{$ENDIF}

interface

const LogGuiHandoffCheckCount = 6;
function RunLogGuiHandoffChecks(const ALogPath: String; AProducerOnly: Boolean = False): Integer;

implementation

uses Classes, SysUtils, SyncObjs, ULog, UConst;

type
  TLogProducer = class(TThread)
  private
    FLog: TLog;
    FStarted: Boolean;
  protected
    procedure Execute; override;
  public
    Entered, Attempted, Done: TEvent;
    ErrorText: String;
    constructor Create(ALog: TLog);
    destructor Destroy; override;
    procedure StartOwned;
  end;

  TLogReceiver = class
  public
    Log: TLog;
    Producer: TLogProducer;
    Messages, Journal: TStringList;
    BeginCount, EndCount, SynchronousCount: Integer;
    ProducerAttempted, ProducerCompleted: Boolean;
    Reenter, RaiseRecord, RaiseBegin, RaiseEnd, RemoveHandler: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Receive(AType: TLogType; ATime: TDateTime; AThreadID: TThreadID; const ASender, AText: String);
    procedure ReceiveSynchronous(AType: TLogType; ATime: TDateTime; AThreadID: TThreadID; const ASender, AText: String);
    procedure BeginBatch(Sender: TObject);
    procedure EndBatch(Sender: TObject);
  end;

  TLogFixture = class
  public
    Log: TLog;
    Dispatch: TThreadSafeLogEvent;
    Receiver: TLogReceiver;
    constructor Create;
    destructor Destroy; override;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

constructor TLogProducer.Create(ALog: TLog);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FLog := ALog;
  Entered := TEvent.Create(nil,True,False,'');
  Attempted := TEvent.Create(nil,True,False,'');
  Done := TEvent.Create(nil,True,False,'');
end;

procedure TLogProducer.StartOwned;
begin
  Start;
  FStarted := True;
end;

destructor TLogProducer.Destroy;
begin
  Terminate;
  if Assigned(Entered) then
    Entered.SetEvent;
  if FStarted then
    WaitFor;
  inherited Destroy;
  Done.Free;
  Attempted.Free;
  Entered.Free;
end;

procedure TLogProducer.Execute;
begin
  try
    try
      Require(Entered.WaitFor(5000)=wrSignaled,'GUI callback did not release the producer barrier');
      if Terminated then
        Exit;
      Attempted.SetEvent;
      FLog.NotifyNewLog(ltupdate,'producer','late-one');
      FLog.NotifyNewLog(lterror,'producer','late-two');
    except
      on E: Exception do
        ErrorText := E.ClassName+': '+E.Message;
    end;
  finally
    Done.SetEvent;
  end;
end;

constructor TLogReceiver.Create;
begin
  inherited Create;
  Messages := TStringList.Create;
  Journal := TStringList.Create;
end;

destructor TLogReceiver.Destroy;
begin
  Journal.Free;
  Messages.Free;
  inherited Destroy;
end;

procedure TLogReceiver.Receive(AType: TLogType; ATime: TDateTime; AThreadID: TThreadID; const ASender, AText: String);
begin
  Require(TThread.CurrentThread.ThreadID=MainThreadID,'GUI callback left the main thread');
  Messages.Add(AText);
  Journal.Add(AText);
  if (Messages.Count=1) and Assigned(Producer) then begin
    Producer.Entered.SetEvent;
    ProducerAttempted := Producer.Attempted.WaitFor(3000)=wrSignaled;
    // A bounded wait exposes the old queue lock without leaving RED deadlocked:
    // returning from this callback lets that old producer finish normally.
    ProducerCompleted := Producer.Done.WaitFor(1000)=wrSignaled;
  end;
  if (Messages.Count=1) and Reenter then
    Log.NotifyNewLog(ltdebug,'reentrant','next-batch');
  if RemoveHandler then
    Log.OnNewLog := nil;
  if RaiseRecord then
    raise Exception.Create('expected record callback failure');
end;

procedure TLogReceiver.ReceiveSynchronous(AType: TLogType; ATime: TDateTime; AThreadID: TThreadID; const ASender, AText: String);
begin
  Require(AThreadID=TThread.CurrentThread.ThreadID,'synchronous callback changed producer thread');
  Inc(SynchronousCount);
end;

procedure TLogReceiver.BeginBatch(Sender: TObject);
begin
  Inc(BeginCount);
  Journal.Add('begin');
  if RaiseBegin then
    raise Exception.Create('expected begin callback failure');
end;

procedure TLogReceiver.EndBatch(Sender: TObject);
begin
  Inc(EndCount);
  Journal.Add('end');
  if RaiseEnd then
    raise Exception.Create('expected end callback failure');
end;

constructor TLogFixture.Create;
begin
  inherited Create;
  Receiver := TLogReceiver.Create;
  Log := TLog.Create(nil);
  Log.ProcessGlobalLogs := False;
  Receiver.Log := Log;
  Log.OnNewLog := Receiver.Receive;
  {$IFNDEF LOG_GUI_HANDOFF_OLD_API}
  Log.OnNewLogBatchBegin := Receiver.BeginBatch;
  Log.OnNewLogBatchEnd := Receiver.EndBatch;
  {$ENDIF}
  // Invoke the public delivery helper on the real queue. Its background sibling
  // cannot dispatch until this console's main thread pumps synchronization.
  Dispatch := TThreadSafeLogEvent.Create(True);
  Dispatch.FLog := Log;
end;

destructor TLogFixture.Destroy;
begin
  Dispatch.Free;
  Log.Free;
  Receiver.Free;
  inherited Destroy;
end;

procedure CheckProducerHandoff;
var LFixture: TLogFixture; LProducer: TLogProducer;
begin
  LFixture := TLogFixture.Create;
  LProducer := nil;
  try
    LProducer := TLogProducer.Create(LFixture.Log);
    LFixture.Receiver.Producer := LProducer;
    LProducer.StartOwned;
    LFixture.Log.NotifyNewLog(ltinfo,'main','initial');
    LFixture.Dispatch.SynchronizedProcess;
    Require(LProducer.Done.WaitFor(5000)=wrSignaled,'producer did not finish after GUI callback returned');
    LProducer.WaitFor;
    Require(LProducer.ErrorText='',LProducer.ErrorText);
    Require(LFixture.Receiver.ProducerAttempted,'producer never attempted admission during the GUI callback');
    Require(LFixture.Receiver.ProducerCompleted,'producer waited for the GUI callback to return');
    Require(LFixture.Receiver.Messages.Count=1,'new producer records entered the detached batch');
    LFixture.Dispatch.SynchronizedProcess;
    Require(LFixture.Receiver.Messages.CommaText='initial,late-one,late-two','producer FIFO or next-batch delivery changed');
  finally
    LProducer.Free;
    LFixture.Free;
  end;
end;

procedure CheckReentrantFIFO;
var LFixture: TLogFixture;
begin
  LFixture := TLogFixture.Create;
  try
    LFixture.Receiver.Reenter := True;
    LFixture.Log.NotifyNewLog(ltinfo,'main','first');
    LFixture.Log.NotifyNewLog(ltinfo,'main','second');
    LFixture.Dispatch.SynchronizedProcess;
    Require(LFixture.Receiver.Messages.CommaText='first,second','current batch changed order or swallowed reentrant input');
    LFixture.Dispatch.SynchronizedProcess;
    Require(LFixture.Receiver.Messages.CommaText='first,second,next-batch','reentrant message did not remain for the next batch');
    Require(LFixture.Receiver.Journal.CommaText='begin,first,second,end,begin,next-batch,end','batch brackets do not surround FIFO delivery');
  finally
    LFixture.Free;
  end;
end;

procedure CheckRecordException;
var LFixture: TLogFixture; LRaised: Boolean;
begin
  LFixture := TLogFixture.Create;
  try
    LFixture.Receiver.RaiseRecord := True;
    LFixture.Log.NotifyNewLog(ltinfo,'main',StringOfChar('a',257));
    LFixture.Log.NotifyNewLog(ltinfo,'main',StringOfChar('b',263));
    LRaised := False;
    try
      LFixture.Dispatch.SynchronizedProcess;
    except
      on E: Exception do begin
        LRaised := True;
        Require(E.Message='expected record callback failure','record exception changed');
      end;
    end;
    Require(LRaised,'record callback exception was swallowed');
    Require((LFixture.Receiver.BeginCount=1) and (LFixture.Receiver.EndCount=1),'batch end was skipped after a record exception');
    LFixture.Receiver.RaiseRecord := False;
    LFixture.Dispatch.SynchronizedProcess;
    Require(LFixture.Receiver.Messages.Count=1,'retired exceptional batch was delivered again');
    LFixture.Log.NotifyNewLog(ltinfo,'main','after-error');
    LFixture.Dispatch.SynchronizedProcess;
    Require(LFixture.Receiver.Messages.Count=2,'queue was unusable after an exceptional batch');
  finally
    LFixture.Free;
  end;
end;

procedure CheckHookExceptions;
var LFixture: TLogFixture; LRaised: Boolean; I: Integer;
begin
  for I := 0 to 1 do begin
    LFixture := TLogFixture.Create;
    try
      LFixture.Receiver.RaiseBegin := I=0;
      LFixture.Receiver.RaiseEnd := I=1;
      LFixture.Log.NotifyNewLog(ltinfo,'main',StringOfChar('c',269));
      LRaised := False;
      try
        LFixture.Dispatch.SynchronizedProcess;
      except
        on E: Exception do begin
          LRaised := True;
          if I=0 then
            Require(E.Message='expected begin callback failure','begin exception changed')
          else
            Require(E.Message='expected end callback failure','end exception changed');
        end;
      end;
      Require(LRaised,'hook exception was swallowed');
      Require(LFixture.Receiver.BeginCount=1,'batch begin count changed');
      Require(LFixture.Receiver.EndCount=I,'end ran after failed begin or failed to run after successful begin');
      LFixture.Receiver.RaiseBegin := False;
      LFixture.Receiver.RaiseEnd := False;
      LFixture.Dispatch.SynchronizedProcess;
      Require(LFixture.Receiver.BeginCount=1,'exceptional detached data remained queued');
      LFixture.Log.NotifyNewLog(ltinfo,'main','after-hook-error');
      LFixture.Dispatch.SynchronizedProcess;
      Require(LFixture.Receiver.BeginCount=2,'queue was unusable after a hook exception');
    finally
      LFixture.Free;
    end;
  end;
end;

procedure CheckEmptyAndUnsubscribe;
var LFixture: TLogFixture;
begin
  LFixture := TLogFixture.Create;
  try
    LFixture.Dispatch.SynchronizedProcess;
    Require((LFixture.Receiver.BeginCount=0) and (LFixture.Receiver.EndCount=0),'empty batch emitted presentation hooks');
    LFixture.Receiver.RemoveHandler := True;
    LFixture.Log.NotifyNewLog(ltinfo,'main','first');
    LFixture.Log.NotifyNewLog(ltinfo,'main',StringOfChar('d',277));
    LFixture.Dispatch.SynchronizedProcess;
    Require((LFixture.Receiver.Messages.Count=1) and (LFixture.Receiver.EndCount=1),'handler removal changed batch cleanup');
    LFixture.Receiver.RemoveHandler := False;
    LFixture.Log.OnNewLog := LFixture.Receiver.Receive;
    LFixture.Dispatch.SynchronizedProcess;
    Require(LFixture.Receiver.Messages.Count=1,'unsubscribed records were delivered again');
  finally
    LFixture.Free;
  end;
end;

procedure CheckLegacyAndFile(const ALogPath: String);
var LFixture: TLogFixture; LContents: TStringList; LPath: String;
begin
  LFixture := TLogFixture.Create;
  LContents := TStringList.Create;
  try
    LPath := ALogPath+'.file.log';
    Require(not FileExists(LPath),'file compatibility sample already exists');
    LFixture.Log.OnNewLog := nil;
    LFixture.Log.FileName := LPath;
    LFixture.Log.SaveTypes := [ltinfo];
    LFixture.Log.OnNewLog := LFixture.Receiver.Receive;
    LFixture.Log.OnInThreadNewLog := LFixture.Receiver.ReceiveSynchronous;
    {$IFNDEF LOG_GUI_HANDOFF_OLD_API}
    LFixture.Log.OnNewLogBatchBegin := nil;
    LFixture.Log.OnNewLogBatchEnd := nil;
    {$ENDIF}
    LFixture.Log.NotifyNewLog(ltinfo,'compatibility','saved-info');
    Require(LFixture.Receiver.SynchronousCount=1,'in-thread callback no longer completes before NotifyNewLog returns');
    LFixture.Log.NotifyNewLog(ltdebug,'compatibility','filtered-debug');
    Require(LFixture.Receiver.SynchronousCount=2,'filtered file record lost its synchronous callback');
    LFixture.Dispatch.SynchronizedProcess;
    Require(LFixture.Receiver.Messages.CommaText='saved-info,filtered-debug','legacy single-record receiver changed');
    Require((LFixture.Receiver.BeginCount=0) and (LFixture.Receiver.EndCount=0),'optional hooks became mandatory');
    LFixture.Log.FileName := '';
    LContents.LoadFromFile(LPath);
    Require(Pos('[Info] <compatibility> saved-info',LContents.Text)>0,'existing file format or accepted record changed');
    Require(Pos('filtered-debug',LContents.Text)=0,'file SaveTypes filtering changed');
  finally
    LContents.Free;
    LFixture.Free;
  end;
end;

function RunLogGuiHandoffChecks(const ALogPath: String; AProducerOnly: Boolean): Integer;
const Names: array[1..LogGuiHandoffCheckCount] of String = ('ProducerCompletesDuringGuiCallback','DetachedFifoAndReentrantNextBatch','RecordExceptionEndsBatchAndDisposes','HookExceptionsDisposeAndPreserveQueue','EmptyBatchAndUnsubscribe','LegacyHandlerAndFileCompatibility');
var LLines: TStringList; I, LCount: Integer;
begin
  Result := 0;
  LCount := LogGuiHandoffCheckCount;
  if AProducerOnly then
    LCount := 1;
  LLines := TStringList.Create;
  try
    for I := 1 to LCount do begin
      try
        case I of
          1: CheckProducerHandoff;
          2: CheckReentrantFIFO;
          3: CheckRecordException;
          4: CheckHookExceptions;
          5: CheckEmptyAndUnsubscribe;
          6: CheckLegacyAndFile(ALogPath);
        end;
        LLines.Add('PASS '+Names[I]);
      except
        on E: Exception do begin
          Inc(Result);
          LLines.Add('FAIL '+Names[I]+' '+E.ClassName+': '+E.Message);
        end;
      end;
    end;
    if Result=0 then
      LLines.Add('PASS '+IntToStr(LCount));
    LLines.SaveToFile(ALogPath);
  finally
    LLines.Free;
  end;
end;

end.
