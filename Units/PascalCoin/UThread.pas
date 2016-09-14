unit UThread;

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
  Classes, SyncObjs, Windows;

Type
  TPCThread = Class;
  TPCThreadClass = Class of TPCThread;
  TPCThread = Class(TThread)
  private
    FDebugStep: String;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
    procedure BCExecute; virtual; abstract;
  public
    Class function ThreadClassFound(tclass : TPCThreadClass; Exclude : TObject) : Integer;
    Class function ThreadCount : Integer;
    Class function GetThread(index : Integer) : TPCThread;
    Class function TerminateAllThreads : Integer;
    Class Procedure ProtectEnterCriticalSection(Const Sender : TObject; var Lock : TRTLCriticalSection);
    Property DebugStep : String read FDebugStep write FDebugStep;
  End;

  TPCThreadList = class
  private
    FList: TList;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: Pointer; Const Subject : String);
    procedure Clear;
    procedure Remove(Item: Pointer); inline;
    function LockList: TList;
    procedure UnlockList; inline;
  end;


implementation

uses
  SysUtils, ULog;

{ TPCThread }

Var _threads : TPCThreadList;

procedure TPCThread.DoTerminate;
begin
  inherited;
end;

procedure TPCThread.Execute;
Var l : TList;
begin
  FDebugStep := '';
  _threads.Add(Self,'TPCThread.Execute');
  try
//    TLog.NewLog(ltdebug,Classname,'Starting Thread');
    Try
      Try
        BCExecute;
      Except
        On E:Exception do begin
          TLog.NewLog(lterror,Classname,'Exception inside a Thread at step: '+FDebugStep+' ('+E.ClassName+'): '+E.Message);
          Raise;
        end;
      End;
    Finally
//      TLog.NewLog(ltdebug,Classname,'Finalizing Thread');
    End;
  finally
    if (Assigned(_threads)) then begin
      l := _threads.LockList;
      Try
        l.Remove(Self);
      Finally
        _threads.UnlockList;
      End;
    end;
  end;
end;

class function TPCThread.GetThread(index: Integer): TPCThread;
Var l : TList;
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

class procedure TPCThread.ProtectEnterCriticalSection(Const Sender : TObject; var Lock: TRTLCriticalSection);
begin
  if Not TryEnterCriticalSection(Lock) then begin
//    TLog.NewLog(ltdebug,Sender.Classname,Format('Locked critical section (WAIT): LockCount:%d RecursionCount:%d Semaphore:%d LockOwnerThread:%s',[
//      Lock.LockCount,Lock.RecursionCount,Lock.LockSemaphore,IntToHex(Lock.OwningThread,8) ]));
    EnterCriticalSection(Lock);
//    TLog.NewLog(ltdebug,Sender.Classname,Format('UnLocked critical section (ENTER): LockCount:%d RecursionCount:%d Semaphore:%d LockOwnerThread:%s',[
//      Lock.LockCount,Lock.RecursionCount,Lock.LockSemaphore,IntToHex(Lock.OwningThread,8) ]));
  end;
end;

class function TPCThread.TerminateAllThreads: Integer;
Var l : TList;
  i : Integer;
begin
  Result := -1;
  if Not Assigned(_threads) then exit;
  l := _threads.LockList;
  try
    for i :=l.Count - 1 downto 0 do begin
      TPCThread(l[i]).Terminate;
      if TPCThread(l[i]).Suspended then TPCThread(l[i]).Suspended := false;
      TPCThread(l[i]).WaitFor;
    end;
    Result := l.Count;
  finally
    _threads.UnlockList;
  end;
end;

class function TPCThread.ThreadClassFound(tclass: TPCThreadClass; Exclude : TObject): Integer;
Var l : TList;
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
Var l : TList;
begin
  l := _threads.LockList;
  try
    Result := l.Count;
  finally
    _threads.UnlockList;
  end;
end;

{ TPCThreadList }

procedure TPCThreadList.Add(Item: Pointer; Const Subject : String);
begin
  LockList;
  Try
    FList.Add(Item);
  Finally
    UnlockList;
  End;
end;

procedure TPCThreadList.Clear;
begin
  LockList;
  Try
    FList.Clear;
  Finally
    UnlockList;
  End;
end;

constructor TPCThreadList.Create;
begin
  InitializeCriticalSection(FLock);
  FList := TList.Create;
end;

destructor TPCThreadList.Destroy;
begin
  LockList;
  try
    FreeAndNil(FList);
    inherited Destroy;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;
end;

function TPCThreadList.LockList: TList;
begin
  TPCThread.ProtectEnterCriticalSection(Self,FLock);
  Result := FList;
end;

procedure TPCThreadList.Remove(Item: Pointer);
begin
  LockList;
  try
    FList.Remove(Item);
  finally
    UnlockList;
  end;
end;

procedure TPCThreadList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

initialization
  _threads := TPCThreadList.Create;
finalization
  FreeAndNil(_threads);
end.
