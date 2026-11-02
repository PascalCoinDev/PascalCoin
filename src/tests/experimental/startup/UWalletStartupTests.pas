unit UWalletStartupTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$mode delphi}

interface

uses
  Classes, SysUtils, SyncObjs, fpcunit, testregistry, UWalletStartup, UWalletStartupTask, UThread, UAccounts;

type

  TWalletStartupOptionsTests = class(TTestCase)
  published
    procedure DefaultsPreserveNormalStartup;
    procedure ParsesIsolatedOfflineStartup;
    procedure ResolvesRelativeDataDirectory;
    procedure RejectsEmptyDataDirectory;
    procedure RejectsDuplicateDataDirectory;
    procedure RejectsUnknownArguments;
    procedure HelpDoesNotCreateDataDirectory;
  end;

  TWalletStartupTaskTests = class(TTestCase)
  private
    FProgressCount, FCompletedCount : Integer;
    FCallbackThread : TThreadID;
    FProgress : TProgressInfo;
    procedure OnProgress(Sender : TObject; const AProgress : TProgressInfo);
    procedure OnCompleted(Sender : TObject);
    procedure WaitUntilFinished(ATask : TWalletStartupTask);
  protected
    procedure SetUp; override;
  published
    procedure SuccessfulLoadDispatchesOnMainThread;
    procedure FailureIsReportedByCompletion;
    procedure CancellationDiscardsQueuedNotifications;
    procedure CancellationWaitsForRunningLoad;
    procedure CancellationBeforeStartSkipsLoad;
    procedure DestructionDiscardsQueuedNotifications;
    procedure TaskCannotStartTwice;
  end;

implementation

type

  TTestStartupTask = class(TWalletStartupTask)
  private
    FEntered : TEvent;
    FShouldBlock, FShouldFail : Boolean;
    FLoadCount : Integer;
    FWorkerThread : TThreadID;
  protected
    procedure ExecuteLoad; override;
  public
    constructor Create(AShouldBlock : Boolean = false; AShouldFail : Boolean = false);
    destructor Destroy; override;
  end;

{ TWalletStartupOptionsTests }

procedure TWalletStartupOptionsTests.DefaultsPreserveNormalStartup;
var
  LOptions : TWalletStartupOptions;
  LError : String;
begin
  AssertTrue(TryParseWalletStartupOptions([], LOptions, LError));
  AssertEquals('', LError);
  AssertEquals('', LOptions.DataDirectory);
  AssertFalse(LOptions.Offline);
  AssertFalse(LOptions.NoStartupDialogs);
  AssertFalse(LOptions.ShowHelp);
end;

procedure TWalletStartupOptionsTests.ParsesIsolatedOfflineStartup;
var
  LOptions : TWalletStartupOptions;
  LError, LDirectory : String;
begin
  LDirectory := IncludeTrailingPathDelimiter(GetTempDir) + 'PascalCoin test directory';
  AssertTrue(TryParseWalletStartupOptions(['--data-dir=' + LDirectory, '--offline',
    '--no-startup-dialogs'], LOptions, LError));
  AssertEquals(ExpandFileName(LDirectory), LOptions.DataDirectory);
  AssertTrue(LOptions.Offline);
  AssertTrue(LOptions.NoStartupDialogs);
  AssertFalse(LOptions.ShowHelp);
end;

procedure TWalletStartupOptionsTests.ResolvesRelativeDataDirectory;
var
  LOptions : TWalletStartupOptions;
  LError : String;
begin
  AssertTrue(TryParseWalletStartupOptions(['--data-dir=wallet-test-data'], LOptions, LError));
  AssertEquals(ExpandFileName('wallet-test-data'), LOptions.DataDirectory);
end;

procedure TWalletStartupOptionsTests.RejectsEmptyDataDirectory;
var
  LOptions : TWalletStartupOptions;
  LError : String;
begin
  AssertFalse(TryParseWalletStartupOptions(['--data-dir='], LOptions, LError));
  AssertTrue(Pos('non-empty', LError) > 0);
  AssertFalse(TryParseWalletStartupOptions(['--data-dir=   '], LOptions, LError));
end;

procedure TWalletStartupOptionsTests.RejectsDuplicateDataDirectory;
var
  LOptions : TWalletStartupOptions;
  LError : String;
begin
  AssertFalse(TryParseWalletStartupOptions(['--data-dir=first', '--data-dir=second'], LOptions, LError));
  AssertTrue(Pos('only once', LError) > 0);
end;

procedure TWalletStartupOptionsTests.RejectsUnknownArguments;
var
  LOptions : TWalletStartupOptions;
  LError : String;
begin
  AssertFalse(TryParseWalletStartupOptions(['--offine'], LOptions, LError));
  AssertTrue(Pos('--offine', LError) > 0);
  AssertFalse(TryParseWalletStartupOptions(['--offline=true'], LOptions, LError));
  AssertFalse(TryParseWalletStartupOptions(['unexpected.dat'], LOptions, LError));
end;

procedure TWalletStartupOptionsTests.HelpDoesNotCreateDataDirectory;
var
  LOptions : TWalletStartupOptions;
  LError, LDirectory : String;
  LId : TGUID;
begin
  CreateGUID(LId);
  LDirectory := IncludeTrailingPathDelimiter(GetTempDir) + GUIDToString(LId);
  AssertFalse(DirectoryExists(LDirectory));
  AssertTrue(TryParseWalletStartupOptions(['--help', '--data-dir=' + LDirectory], LOptions, LError));
  AssertTrue(LOptions.ShowHelp);
  AssertFalse(DirectoryExists(LDirectory));
  AssertTrue(Pos('--offline', WalletStartupHelp) > 0);
  AssertTrue(TryParseWalletStartupOptions(['-h'], LOptions, LError));
  AssertTrue(LOptions.ShowHelp);
end;

{ TTestStartupTask }

constructor TTestStartupTask.Create(AShouldBlock : Boolean; AShouldFail : Boolean);
begin
  inherited Create;
  FEntered := TEvent.Create(nil, true, false, '');
  FShouldBlock := AShouldBlock;
  FShouldFail := AShouldFail;
end;

destructor TTestStartupTask.Destroy;
begin
  CancelAndWait;
  FEntered.Free;
  inherited;
end;

procedure TTestStartupTask.ExecuteLoad;
var LProgress : TProgressInfo;
begin
  FWorkerThread := GetCurrentThreadID;
  Inc(FLoadCount);
  FEntered.SetEvent;
  while FShouldBlock and (not Cancelled) do
    Sleep(1);
  CheckCancelled;
  ReportProgress(TProgressInfo.Create(psReadingBlockchain,5,10));
  LProgress := TProgressInfo.Create(psFinalizingRestore,10,10);
  LProgress.Context := pcCheckingNewSafebox;
  LProgress.BlockNumber := 42;
  LProgress.BlockCount := 84;
  LProgress.StartBlock := 7;
  LProgress.EndBlock := 13;
  LProgress.OperationsCount := 17;
  LProgress.ChunksCount := 2;
  LProgress.FileName := 'generated-checkpoint';
  LProgress.PeerAddress := '127.0.0.1';
  ReportProgress(LProgress);
  if FShouldFail then
    raise Exception.Create('test restore failure');
end;

{ TWalletStartupTaskTests }

procedure TWalletStartupTaskTests.SetUp;
begin
  inherited;
  FProgressCount := 0;
  FCompletedCount := 0;
  FCallbackThread := 0;
  FProgress := TProgressInfo.Create(psNone);
end;

procedure TWalletStartupTaskTests.OnProgress(Sender : TObject; const AProgress : TProgressInfo);
begin
  Inc(FProgressCount);
  FCallbackThread := GetCurrentThreadID;
  FProgress := AProgress;
end;

procedure TWalletStartupTaskTests.OnCompleted(Sender : TObject);
begin
  Inc(FCompletedCount);
  FCallbackThread := GetCurrentThreadID;
end;

procedure TWalletStartupTaskTests.WaitUntilFinished(ATask : TWalletStartupTask);
var
  LDeadline : QWord;
begin
  LDeadline := GetTickCount64 + 3000;
  while (not ATask.Finished) and (GetTickCount64 < LDeadline) do
    Sleep(1);
  AssertTrue('Startup task did not finish', ATask.Finished);
end;

procedure TWalletStartupTaskTests.SuccessfulLoadDispatchesOnMainThread;
var
  LTask : TTestStartupTask;
begin
  LTask := TTestStartupTask.Create;
  try
    LTask.OnProgress := OnProgress;
    LTask.OnCompleted := OnCompleted;
    LTask.Start;
    WaitUntilFinished(LTask);
    AssertEquals(0, FCompletedCount);
    AssertEquals(0, FProgressCount);
    CheckSynchronize;
    AssertEquals(1, FCompletedCount);
    AssertEquals('Repeated progress is coalesced', 1, FProgressCount);
    AssertTrue('The latest typed stage is retained',FProgress.Stage = psFinalizingRestore);
    AssertTrue('The raw context is retained',FProgress.Context = pcCheckingNewSafebox);
    AssertEquals(Int64(10), FProgress.CurrentPosition);
    AssertEquals(Int64(10), FProgress.TotalCount);
    AssertEquals(Int64(42), FProgress.BlockNumber);
    AssertEquals(Int64(84), FProgress.BlockCount);
    AssertEquals(Int64(7), FProgress.StartBlock);
    AssertEquals(Int64(13), FProgress.EndBlock);
    AssertEquals(Int64(17), FProgress.OperationsCount);
    AssertEquals(Int64(2), FProgress.ChunksCount);
    AssertEquals('generated-checkpoint',FProgress.FileName);
    AssertEquals('127.0.0.1',FProgress.PeerAddress);
    AssertEquals(QWord(GetCurrentThreadID), QWord(FCallbackThread));
    AssertTrue(LTask.FWorkerThread <> GetCurrentThreadID);
    AssertEquals('', LTask.ErrorMessage);
    AssertEquals(1, LTask.FLoadCount);
  finally
    LTask.Free;
  end;
end;

procedure TWalletStartupTaskTests.FailureIsReportedByCompletion;
var
  LTask : TTestStartupTask;
begin
  LTask := TTestStartupTask.Create(false, true);
  try
    LTask.OnCompleted := OnCompleted;
    LTask.Start;
    WaitUntilFinished(LTask);
    CheckSynchronize;
    AssertEquals(1, FCompletedCount);
    AssertTrue(Pos('test restore failure', LTask.ErrorMessage) > 0);
    AssertEquals(QWord(GetCurrentThreadID), QWord(FCallbackThread));
  finally
    LTask.Free;
  end;
end;

procedure TWalletStartupTaskTests.CancellationDiscardsQueuedNotifications;
var
  LTask : TTestStartupTask;
begin
  LTask := TTestStartupTask.Create;
  try
    LTask.OnProgress := OnProgress;
    LTask.OnCompleted := OnCompleted;
    LTask.Start;
    WaitUntilFinished(LTask);
    LTask.CancelAndWait;
    CheckSynchronize;
    AssertEquals(0, FProgressCount);
    AssertEquals(0, FCompletedCount);
    AssertTrue(LTask.Cancelled);
  finally
    LTask.Free;
  end;
end;

procedure TWalletStartupTaskTests.CancellationWaitsForRunningLoad;
var
  LTask : TTestStartupTask;
begin
  LTask := TTestStartupTask.Create(true);
  try
    LTask.OnCompleted := OnCompleted;
    LTask.Start;
    AssertEquals(Ord(wrSignaled), Ord(LTask.FEntered.WaitFor(3000)));
    AssertTrue('Startup work is tracked by the node thread infrastructure',
      TPCThread.GetThreadByClass(TTestStartupTask, nil) = LTask);
    LTask.CancelAndWait;
    AssertTrue(LTask.Finished);
    AssertNull(TPCThread.GetThreadByClass(TTestStartupTask, nil));
    CheckSynchronize;
    AssertEquals(1, LTask.FLoadCount);
    AssertEquals(0, FCompletedCount);
    AssertEquals('', LTask.ErrorMessage);
  finally
    LTask.Free;
  end;
end;

procedure TWalletStartupTaskTests.CancellationBeforeStartSkipsLoad;
var
  LTask : TTestStartupTask;
begin
  LTask := TTestStartupTask.Create;
  try
    LTask.CancelAndWait;
    AssertTrue(LTask.Finished);
    AssertEquals(0, LTask.FLoadCount);
    AssertEquals('', LTask.ErrorMessage);
  finally
    LTask.Free;
  end;
end;

procedure TWalletStartupTaskTests.DestructionDiscardsQueuedNotifications;
var
  LTask : TTestStartupTask;
begin
  LTask := TTestStartupTask.Create;
  LTask.OnProgress := OnProgress;
  LTask.OnCompleted := OnCompleted;
  LTask.Start;
  WaitUntilFinished(LTask);
  LTask.Free;
  CheckSynchronize;
  AssertEquals(0, FProgressCount);
  AssertEquals(0, FCompletedCount);
end;

procedure TWalletStartupTaskTests.TaskCannotStartTwice;
var
  LTask : TTestStartupTask;
  LRaised : Boolean;
begin
  LTask := TTestStartupTask.Create;
  try
    LTask.Start;
    LRaised := false;
    try
      LTask.Start;
    except
      on E : EThread do LRaised := true;
    end;
    AssertTrue(LRaised);
  finally
    LTask.Free;
  end;
end;

initialization
  RegisterTest(TWalletStartupOptionsTests);
  RegisterTest(TWalletStartupTaskTests);

end.
