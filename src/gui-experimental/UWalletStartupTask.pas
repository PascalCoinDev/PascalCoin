unit UWalletStartupTask;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

{ Runs an owned, cancellable node task with queued progress and completion
  notifications. Cancellation joins the worker and removes callbacks before
  its receivers are destroyed. }

{ Public types in this unit

  Type               | Purpose
  -------------------+-------------------------------------------------------------------------
  TWalletStartupTask | Runs cancellable node work and delivers progress/completion events.
}

interface

uses
  Classes, SysUtils, {$IFNDEF FPC}System.SyncObjs,{$ENDIF} UThread, UAccounts;

type

  { An owned, single-use node worker, using the core's thread tracking, lock and
    progress event type. Call CancelAndWait before destroying its receivers.
    The extra lifecycle is needed because queued methods must not outlive either
    the worker or UI graph. The owner retains errors until the task is freed. }

  TWalletStartupTask = class(TPCThread)
  private
    FCancelled : LongInt;
    FHasStarted : Boolean;
    FProgressLock : TReadWriteLock;
    FProgressQueued : Boolean;
    FProgress : TProgressInfo;
    FErrorMessage : String;
    FOnProgress : TProgressNotify;
    FOnCompleted : TNotifyEvent;
    function GetCancelled : Boolean;
    procedure DispatchProgress;
    procedure DispatchCompleted;
  protected
    procedure BCExecute; override;
    procedure ExecuteLoad; virtual; abstract;
    procedure CheckCancelled;
    procedure ReportProgress(const AProgress : TProgressInfo);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Cancel;
    procedure CancelAndWait;
    property Cancelled : Boolean read GetCancelled;
    property ErrorMessage : String read FErrorMessage;
    property OnProgress : TProgressNotify read FOnProgress write FOnProgress;
    property OnCompleted : TNotifyEvent read FOnCompleted write FOnCompleted;
  end;

implementation

constructor TWalletStartupTask.Create;
begin
  FProgressLock := TReadWriteLock.Create('WalletStartup.Progress',lrpNoRecursion);
  inherited Create(true);
  FreeOnTerminate := false;
end;

destructor TWalletStartupTask.Destroy;
begin
  CancelAndWait;
  inherited;
  FProgressLock.Free;
end;

procedure TWalletStartupTask.Start;
begin
  if FHasStarted then
    raise EThread.Create('The wallet startup task has already started.');
  FHasStarted := true;
  inherited Start;
end;

function TWalletStartupTask.GetCancelled : Boolean;
begin
  {$IFDEF FPC}
  Result := InterlockedCompareExchange(FCancelled, 0, 0) <> 0;
  {$ELSE}
  Result := TInterlocked.CompareExchange(FCancelled, 0, 0) <> 0;
  {$ENDIF}
end;

procedure TWalletStartupTask.CheckCancelled;
begin
  if Cancelled then Abort;
end;

procedure TWalletStartupTask.Cancel;
begin
  {$IFDEF FPC}
  InterlockedExchange(FCancelled, 1);
  {$ELSE}
  TInterlocked.Exchange(FCancelled, 1);
  {$ENDIF}
  Terminate;
end;

procedure TWalletStartupTask.CancelAndWait;
begin
  Cancel;
  if not FHasStarted then Start;
  WaitFor;
  TThread.RemoveQueuedEvents(Self);
end;

procedure TWalletStartupTask.BCExecute;
begin
  try
    CheckCancelled;
    ExecuteLoad;
  except
    on E : Exception do
      if not Cancelled then
        FErrorMessage := E.ClassName + ': ' + E.Message;
  end;
  if not Cancelled then Queue(DispatchCompleted);
end;

procedure TWalletStartupTask.ReportProgress(const AProgress : TProgressInfo);
var
  LQueue : Boolean;
begin
  CheckCancelled;
  FProgressLock.AcquireWriteLock;
  try
    FProgress := AProgress;
    LQueue := not FProgressQueued;
    FProgressQueued := true;
  finally
    FProgressLock.ReleaseWriteLock;
  end;
  if LQueue then Queue(DispatchProgress);
end;

procedure TWalletStartupTask.DispatchProgress;
var
  LProgress : TProgressInfo;
begin
  // Dispatch also clears the queued flag, so it retains write ownership.
  FProgressLock.AcquireWriteLock;
  try
    // Copy one complete measurement, including managed filename/peer metadata,
    // before dropping the lock. Formatting never runs on the producer thread.
    LProgress := FProgress;
    FProgressQueued := false;
  finally
    FProgressLock.ReleaseWriteLock;
  end;
  if (not Cancelled) and Assigned(FOnProgress) then
    FOnProgress(Self, LProgress);
end;

procedure TWalletStartupTask.DispatchCompleted;
begin
  if (not Cancelled) and Assigned(FOnCompleted) then
    FOnCompleted(Self);
end;

end.
