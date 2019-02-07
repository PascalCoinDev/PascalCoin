unit ULog;

{ Copyright (c) 2016 by Albert Molina

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
  Classes, UThread, SyncObjs, UConst,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};


type
  TLogType = (ltinfo, ltupdate, lterror, ltdebug);
  TLogTypes = set of TLogType;

  TNewLogEvent = procedure(logtype : TLogType; Time : TDateTime; ThreadID : TThreadID; Const sender, logtext : String) of object;

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
    ThreadID : TThreadID;
    Sender, Logtext : String
  End;

  TLog = Class(TComponent)
  private
    FLogDataList : TThreadList<Pointer>;
    FOnNewLog: TNewLogEvent;
    FOnInThreadNewLog : TNewLogEvent;
    FFileStream : TFileStream;
    FFileName: String;
    FSaveTypes: TLogTypes;
    FThreadSafeLogEvent : TThreadSafeLogEvent;
    FProcessGlobalLogs: Boolean;
    FLock : TCriticalSection;
    procedure SetFileName(const Value: String);
  protected
    Procedure DoLog(logtype : TLogType; const sender, logtext : String); virtual;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Class Procedure NewLog(logtype : TLogType; Const sender, logtext : String);
    Property OnInThreadNewLog : TNewLogEvent read FOnInThreadNewLog write FOnInThreadNewLog;
    Property OnNewLog : TNewLogEvent read FOnNewLog write FOnNewLog;
    Property FileName : String read FFileName write SetFileName;
    Property SaveTypes : TLogTypes read FSaveTypes write FSaveTypes;
    Property ProcessGlobalLogs : Boolean read FProcessGlobalLogs write FProcessGlobalLogs;
    Procedure NotifyNewLog(logtype : TLogType; Const sender, logtext : String);
  End;

Const
  CT_LogType : Array[TLogType] of String = ('Info','Update','Error','Debug');
  CT_TLogTypes_ALL : TLogTypes = [ltinfo, ltupdate, lterror, ltdebug];
  CT_TLogTypes_DEFAULT : TLogTypes = [ltinfo, ltupdate, lterror];


implementation

uses SysUtils;

var _logs : TList<TLog>;
Type PLogData = ^TLogData;

{ TLog }

constructor TLog.Create(AOwner: TComponent);
begin
  FLock := TCriticalSection.Create;
  FProcessGlobalLogs := true;
  FLogDataList := TThreadList<Pointer>.Create;
  FFileStream := Nil;
  FFileName := '';
  FSaveTypes := CT_TLogTypes_DEFAULT;
  FOnInThreadNewLog:=Nil;
  FOnNewLog:=Nil;
  if (Not assigned(_logs)) then _logs := TList<TLog>.Create;
  _logs.Add(self);
  FThreadSafeLogEvent := TThreadSafeLogEvent.Create(true);
  FThreadSafeLogEvent.FLog := Self;
  FThreadSafeLogEvent.Suspended := false;
  inherited;
end;

destructor TLog.Destroy;
var
  l : TList<Pointer>;
  i : Integer;
  P : PLogData;
begin
  FOnNewLog:=Nil;
  FOnInThreadNewLog:=Nil;
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

procedure TLog.DoLog(logtype: TLogType; const sender, logtext: String);
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
Var s,tid : RawByteString;
  P : PLogData;
begin
  FLock.Acquire;
  try
    if assigned(FFileStream) And (logType in FSaveTypes) then begin
      if TThread.CurrentThread.ThreadID=MainThreadID then tid := ' MAIN:' else tid:=' TID:';
      s := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',now)+tid+IntToHex(PtrInt(TThread.CurrentThread.ThreadID),8)+' ['+CT_LogType[logtype]+'] <'+sender+'> '+logtext+#13#10;
      FFileStream.Write(s[Low(s)],Length(s));
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

procedure TLog.SetFileName(const Value: String);
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
Var l : TList<Pointer>;
  i : Integer;
  P : PLogData;
begin
  If Not Assigned(FLog) then Exit;
  If Not Assigned(FLog.FOnNewLog) then Exit;
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
