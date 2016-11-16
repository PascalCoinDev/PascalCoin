unit ULog;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Classes, UThread, SyncObjs;

type
  TLogType = (ltinfo, ltupdate, lterror, ltdebug);
  TLogTypes = set of TLogType;

  TNewLogEvent = procedure(logtype : TLogType; Time : TDateTime; ThreadID : Cardinal; Const sender, logtext : AnsiString) of object;

  TLog = Class;

  { TThreadSafeLogEvent }

  TThreadSafeLogEvent = Class(TPCThread)
    FLog : TLog;
    Procedure SynchronizedProcess;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(Suspended : Boolean);
  End;

  TLogData = Record
    Logtype : TLogType;
    Time : TDateTime;
    ThreadID : Cardinal;
    Sender, Logtext : AnsiString
  End;

  TLog = Class(TComponent)
  private
    FLogDataList : TThreadList;
    FOnNewLog: TNewLogEvent;
    FOnInThreadNewLog : TNewLogEvent;
    FFileStream : TFileStream;
    FFileName: AnsiString;
    FSaveTypes: TLogTypes;
    FThreadSafeLogEvent : TThreadSafeLogEvent;
    FProcessGlobalLogs: Boolean;
    FLock : TCriticalSection;
    procedure SetFileName(const Value: AnsiString);
  protected
    Procedure DoLog(logtype : TLogType; sender, logtext : AnsiString); virtual;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Class Procedure NewLog(logtype : TLogType; Const sender, logtext : String);
    Property OnInThreadNewLog : TNewLogEvent read FOnInThreadNewLog write FOnInThreadNewLog;
    Property OnNewLog : TNewLogEvent read FOnNewLog write FOnNewLog;
    Property FileName : AnsiString read FFileName write SetFileName;
    Property SaveTypes : TLogTypes read FSaveTypes write FSaveTypes;
    Property ProcessGlobalLogs : Boolean read FProcessGlobalLogs write FProcessGlobalLogs;
    Procedure NotifyNewLog(logtype : TLogType; Const sender, logtext : String);
  End;

Const
  CT_LogType : Array[TLogType] of AnsiString = ('Info','Update','Error','Debug');
  CT_TLogTypes_ALL : TLogTypes = [ltinfo, ltupdate, lterror, ltdebug];
  CT_TLogTypes_DEFAULT : TLogTypes = [ltinfo, ltupdate, lterror];


implementation

uses SysUtils;

var _logs : TList;
Type PLogData = ^TLogData;

{ TLog }

constructor TLog.Create(AOwner: TComponent);
Var l : TList;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FProcessGlobalLogs := true;
  FLogDataList := TThreadList.Create;
  FFileStream := Nil;
  FFileName := '';
  FSaveTypes := CT_TLogTypes_DEFAULT;
  if (Not assigned(_logs)) then _logs := TList.Create;
  _logs.Add(self);
  FThreadSafeLogEvent := TThreadSafeLogEvent.Create(true);
  FThreadSafeLogEvent.FLog := Self;
  FThreadSafeLogEvent.Suspended := false;
end;

destructor TLog.Destroy;
var
  l : TList;
  i : Integer;
  P : PLogData;
begin
  FThreadSafeLogEvent.Terminate;
  FThreadSafeLogEvent.WaitFor;
  FreeAndNil(FThreadSafeLogEvent);
  _logs.Remove(Self);
  FreeAndNil(FFileStream);
  l := FLogDataList.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      P := PLogData(l[i]);
      Dispose(P);
    end;
    l.Clear;
  finally
    FLogDataList.UnlockList;
  end;
  FreeAndNil(FLogDataList);
  FreeAndNil(FLock);
  inherited;
end;

procedure TLog.DoLog(logtype: TLogType; sender, logtext: AnsiString);
begin
//
end;

class procedure TLog.NewLog(logtype: TLogType; Const sender, logtext: String);
var i : Integer;
begin
  if (Not Assigned(_logs)) then exit;
  for i := 0 to _logs.Count - 1 do begin
    if (TLog(_logs[i]).FProcessGlobalLogs) then begin
      TLog(_logs[i]).NotifyNewLog(logtype,sender,logtext);
    end;
  end;
end;

procedure TLog.NotifyNewLog(logtype: TLogType; Const sender, logtext: String);
Var s,tid : AnsiString;
  P : PLogData;
begin
  FLock.Acquire;
  try
    if assigned(FFileStream) And (logType in FSaveTypes) then begin
      if TThread.CurrentThread.ThreadID=MainThreadID then tid := ' MAIN:' else tid:=' TID:';
      s := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',now)+tid+IntToHex(TThread.CurrentThread.ThreadID,8)+' ['+CT_LogType[logtype]+'] <'+sender+'> '+logtext+#13#10;
      FFileStream.Write(s[1],length(s));
    end;
    if Assigned(FOnInThreadNewLog) then begin
      FOnInThreadNewLog(logtype,now,TThread.CurrentThread.ThreadID,sender,logtext);
    end;
    if Assigned(FOnNewLog) then begin
      // Add to a thread safe list
      New(P);
      P^.Logtype := logtype;
      P^.Time := now;
      P^.ThreadID :=TThread.CurrentThread.ThreadID;
      P^.Sender := sender;
      P^.Logtext := logtext;
      FLogDataList.Add(P);
    end;
  finally
    FLock.Release;
  end;
  DoLog(logtype,sender,logtext);
end;

procedure TLog.SetFileName(const Value: AnsiString);
var fm : Word;
begin
  if FFileName = Value then exit;
  if assigned(FFileStream) then Begin
    FreeAndNil(FFileStream);
  End;
  FFileName := Value;
  if (FFileName<>'') then begin
    If Not ForceDirectories(ExtractFileDir(FFileName)) then exit;
    if FileExists(FFileName) then fm := fmOpenWrite + fmShareDenyWrite
    else fm := fmCreate + fmShareDenyWrite;
    FFileStream := TFileStream.Create(FFileName,fm);
    FFileStream.Position := FFileStream.size; // To the end!
    NotifyNewLog(ltinfo,Classname,'Log file start: '+FFileName);
  end;
end;

{ TThreadSafeLogEvent }

procedure TThreadSafeLogEvent.BCExecute;
begin
  while (not Terminated) do begin
    sleep(100);
    If (Not Terminated) And (Assigned(FLog.OnNewLog)) then begin
      Synchronize(SynchronizedProcess);
    end;
  end;
end;

constructor TThreadSafeLogEvent.Create(Suspended: Boolean);
begin
  inherited Create(Suspended);
end;

procedure TThreadSafeLogEvent.SynchronizedProcess;
Var l : TList;
  i : Integer;
  P : PLogData;
begin
  // This event is thread safe and will do OnNewLog on main thread
  l := FLog.FLogDataList.LockList;
  try
    try
      for i := 0 to l.Count - 1 do begin
        P := PLogData(l[i]);
        if Assigned(FLog.FOnNewLog) then begin
          FLog.OnNewLog( P^.Logtype,P^.Time,P^.ThreadID,P^.Sender,P^.Logtext );
        end;
        Dispose(P);
      end;
    finally
      // Protection for possible raise
      l.Clear;
    end;
  finally
    FLog.FLogDataList.UnlockList;
  end;
end;

initialization
  _logs := Nil;
finalization
  FreeAndNil(_logs);
end.
