unit UPendingRestoreTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php. }

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, UCrypto, UAccounts, UBlockChain;

type
  TPendingRestoreTests = class(TTestCase)
  private
    FKey : TECPrivateKey;
    FOriginal, FLoaded : TOperationsHashTree;
    FStream : TMemoryStream;
    FOriginalBytes : TBytes;
    FProgressCount, FChangedCount : Integer;
    FFirstCurrent, FLastCurrent, FLastTotal : Int64;
    FCancelAt : Int64;
    FDelayFirstProgress : Boolean;
    FExpectedProgressThread : TThreadID;
    procedure OnProgress(Sender : TObject; const AProgress : TProgressInfo);
    procedure OnChanged(Sender : TObject);
    procedure SnapshotSource;
    procedure AssertSourceUnchanged;
    procedure AssertSameOperations;
    procedure AssertNotificationsRestored;
    procedure LoadAndExpectCancellation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure LegacyStoredRoundTripWithoutProgress;
    procedure ReportsDecodeStartAndCompletion;
    procedure CancellationBeforeDecodeRestoresNotifications;
    procedure CancellationDuringDecodePreservesSource;
    procedure CanceledDecodeCanBeRetried;
    procedure EmptySavedTreeReportsCompletion;
    procedure TruncatedStreamDoesNotReportSuccessfulCompletion;
    procedure LegacyStorageOverrideIsPreserved;
  end;

implementation

uses
  UConst, UOpTransaction, UPCDataTypes, UBaseTypes, UFileStorage, UMemory;

type
  // An existing storage implementation can retain its original override.
  TPendingStorageOverride = class(TFileStorage)
  protected
    procedure DoLoadPendingBufferOperations(OperationsHashTree : TOperationsHashTree); override;
  public
    Calls : Integer;
    RaiseOnLoad : Boolean;
  end;

procedure TPendingStorageOverride.DoLoadPendingBufferOperations(OperationsHashTree : TOperationsHashTree);
begin
  Inc(Calls);
  if RaiseOnLoad then Abort;
end;

procedure TPendingRestoreTests.SetUp;
var
  LOperation : TPCOperation;
  i : Integer;
begin
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FOriginal := TOperationsHashTree.Create;
  FLoaded := TOperationsHashTree.Create;
  FStream := TMemoryStream.Create;
  FProgressCount := 0;
  FChangedCount := 0;
  FCancelAt := -1;
  FDelayFirstProgress := False;
  FExpectedProgressThread := GetCurrentThreadID;
  FFirstCurrent := -1;
  FLastCurrent := -1;
  FLastTotal := -1;
  FLoaded.OnChanged := OnChanged;
  for i := 1 to 8 do begin
    LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_6,100,i,101,
      FKey,100,1,CT_TOperationPayload_NUL);
    try
      AssertTrue(FOriginal.AddOperationToHashTree(LOperation));
    finally
      LOperation.Free;
    end;
  end;
  AssertTrue(FOriginal.SaveOperationsHashTreeToStream(FStream,True));
  FStream.Position := 0;
  SnapshotSource;
end;

procedure TPendingRestoreTests.TearDown;
begin
  FStream.Free;
  FLoaded.Free;
  FOriginal.Free;
  FKey.Free;
end;

procedure TPendingRestoreTests.OnProgress(Sender : TObject; const AProgress : TProgressInfo);
begin
  AssertTrue('Progress is attributed to its operation tree',Sender = FLoaded);
  AssertTrue('The core identifies decoding without a formatted message',
    AProgress.Stage = psReadingOperations);
  AssertEquals('Core callbacks stay synchronous on the invoking thread',
    QWord(FExpectedProgressThread),QWord(GetCurrentThreadID));
  AssertTrue('A progress count stays within its total',
    (AProgress.CurrentPosition >= 0) and
    (AProgress.CurrentPosition <= AProgress.TotalCount));
  if FProgressCount = 0 then FFirstCurrent := AProgress.CurrentPosition;
  Inc(FProgressCount);
  FLastCurrent := AProgress.CurrentPosition;
  FLastTotal := AProgress.TotalCount;
  // Model a slow first notification so a mid-stream checkpoint is exercised
  // deterministically even with this small, generated in-memory fixture.
  if (FProgressCount = 1) and FDelayFirstProgress then Sleep(250);
  if (FCancelAt >= 0) and (AProgress.CurrentPosition >= FCancelAt) then Abort;
end;

procedure TPendingRestoreTests.OnChanged(Sender : TObject);
begin
  Inc(FChangedCount);
end;

procedure TPendingRestoreTests.SnapshotSource;
begin
  SetLength(FOriginalBytes,FStream.Size);
  if FStream.Size > 0 then Move(FStream.Memory^,FOriginalBytes[0],FStream.Size);
end;

procedure TPendingRestoreTests.AssertSourceUnchanged;
begin
  AssertEquals('Restore must not alter the saved package',Length(FOriginalBytes),FStream.Size);
  if Length(FOriginalBytes) > 0 then
    AssertTrue('Saved operation bytes changed',CompareMem(@FOriginalBytes[0],FStream.Memory,Length(FOriginalBytes)));
end;

procedure TPendingRestoreTests.AssertSameOperations;
var
  i : Integer;
begin
  AssertEquals(FOriginal.OperationsCount,FLoaded.OperationsCount);
  for i := 0 to FOriginal.OperationsCount-1 do begin
    AssertTrue('Signed operation digest changed on restore',
      TBaseType.Equals(FOriginal.GetOperation(i).Sha256,FLoaded.GetOperation(i).Sha256));
    AssertEquals(FOriginal.GetOperation(i).N_Operation,FLoaded.GetOperation(i).N_Operation);
  end;
end;

procedure TPendingRestoreTests.AssertNotificationsRestored;
var
  LExpected, LActual : TNotifyEvent;
begin
  LExpected := OnChanged;
  LActual := FLoaded.OnChanged;
  AssertTrue('Decode lost the original change callback',
    (TMethod(LExpected).Code = TMethod(LActual).Code) and
    (TMethod(LExpected).Data = TMethod(LActual).Data));
end;

procedure TPendingRestoreTests.LoadAndExpectCancellation;
var
  LError : String;
  LCanceled : Boolean;
begin
  LCanceled := False;
  try
    FLoaded.LoadOperationsHashTreeFromStream(FStream,True,CT_PROTOCOL_6,
      CT_PROTOCOL_6,Nil,LError,OnProgress);
  except
    on E : EAbort do LCanceled := True;
  end;
  AssertTrue('A progress cancellation must reach the startup owner',LCanceled);
  AssertEquals('Canceled decoding must not announce successful loading',0,FChangedCount);
  AssertNotificationsRestored;
  AssertSourceUnchanged;
end;

procedure TPendingRestoreTests.LegacyStoredRoundTripWithoutProgress;
var
  LError : String;
begin
  // Existing callers need neither a callback nor a new serialization format.
  AssertTrue(FLoaded.LoadOperationsHashTreeFromStream(FStream,True,
    CT_PROTOCOL_6,CT_PROTOCOL_6,Nil,LError));
  AssertEquals('',LError);
  AssertEquals(1,FChangedCount);
  AssertSameOperations;
  AssertSourceUnchanged;
end;

procedure TPendingRestoreTests.ReportsDecodeStartAndCompletion;
var
  LError : String;
  LOperationsCount, LProcessedCount : Integer;
begin
  AssertTrue(FLoaded.LoadOperationsHashTreeFromStream(FStream,True,
    CT_PROTOCOL_6,CT_PROTOCOL_6,Nil,True,LOperationsCount,LProcessedCount,
    LError,OnProgress));
  AssertTrue('A fast restore still reports both endpoints',FProgressCount >= 2);
  AssertEquals(Int64(0),FFirstCurrent);
  AssertEquals(Int64(8),FLastCurrent);
  AssertEquals(Int64(8),FLastTotal);
  AssertEquals(8,LOperationsCount);
  AssertEquals(8,LProcessedCount);
  AssertEquals(1,FChangedCount);
  AssertSameOperations;
  AssertNotificationsRestored;
  AssertSourceUnchanged;
end;

procedure TPendingRestoreTests.CancellationBeforeDecodeRestoresNotifications;
begin
  FCancelAt := 0;
  LoadAndExpectCancellation;
  AssertEquals(0,FLoaded.OperationsCount);
  AssertTrue('Change callbacks remain usable after cancellation',
    FLoaded.AddOperationToHashTree(FOriginal.GetOperation(0)));
  AssertEquals(1,FChangedCount);
end;

procedure TPendingRestoreTests.CancellationDuringDecodePreservesSource;
begin
  FCancelAt := 1;
  FDelayFirstProgress := True;
  LoadAndExpectCancellation;
  AssertTrue('Cancellation should occur after decoding starts',FLoaded.OperationsCount > 0);
  AssertTrue('Cancellation should not wait for the whole package',
    FLoaded.OperationsCount < FOriginal.OperationsCount);
end;

procedure TPendingRestoreTests.CanceledDecodeCanBeRetried;
var
  LError : String;
begin
  FCancelAt := 1;
  FDelayFirstProgress := True;
  LoadAndExpectCancellation;
  FLoaded.ClearHastThree;
  FStream.Position := 0;
  FCancelAt := -1;
  FDelayFirstProgress := False;
  FChangedCount := 0;
  AssertTrue(FLoaded.LoadOperationsHashTreeFromStream(FStream,True,
    CT_PROTOCOL_6,CT_PROTOCOL_6,Nil,LError,OnProgress));
  AssertEquals(1,FChangedCount);
  AssertSameOperations;
  AssertSourceUnchanged;
end;

procedure TPendingRestoreTests.EmptySavedTreeReportsCompletion;
var
  LError : String;
begin
  FOriginal.ClearHastThree;
  FStream.Clear;
  AssertTrue(FOriginal.SaveOperationsHashTreeToStream(FStream,True));
  AssertEquals(Int64(SizeOf(Cardinal)),FStream.Size);
  FStream.Position := 0;
  SnapshotSource;
  AssertTrue(FLoaded.LoadOperationsHashTreeFromStream(FStream,True,
    CT_PROTOCOL_6,CT_PROTOCOL_6,Nil,LError,OnProgress));
  AssertTrue(FProgressCount >= 2);
  AssertEquals(Int64(0),FFirstCurrent);
  AssertEquals(Int64(0),FLastCurrent);
  AssertEquals(Int64(0),FLastTotal);
  AssertEquals(0,FLoaded.OperationsCount);
  AssertSourceUnchanged;
end;

procedure TPendingRestoreTests.TruncatedStreamDoesNotReportSuccessfulCompletion;
var
  LError : String;
begin
  FStream.Size := FStream.Size-1;
  FStream.Position := 0;
  SnapshotSource;
  AssertFalse(FLoaded.LoadOperationsHashTreeFromStream(FStream,True,
    CT_PROTOCOL_6,CT_PROTOCOL_6,Nil,LError,OnProgress));
  AssertTrue(LError <> '');
  AssertEquals(Int64(0),FFirstCurrent);
  AssertTrue('Invalid data must not report successful completion',FLastCurrent < FLastTotal);
  AssertEquals(0,FChangedCount);
  AssertNotificationsRestored;
  AssertSourceUnchanged;
end;

procedure TPendingRestoreTests.LegacyStorageOverrideIsPreserved;
var
  LDisposables : TDisposables;
  LStorage : TPendingStorageOverride;
  LCanceled : Boolean;
begin
  // No bank or files are needed by the legacy override. The public wrapper
  // must dispatch it on callback, exception and ordinary no-callback paths.
  LStorage := LDisposables.AddObject(TPendingStorageOverride.Create(nil)) as TPendingStorageOverride;
  LStorage.RaiseOnLoad := True;
  LCanceled := False;
  try
    LStorage.LoadPendingBufferOperations(FLoaded,OnProgress);
  except
    on E : EAbort do LCanceled := True;
  end;
  AssertTrue(LCanceled);
  AssertEquals(1,LStorage.Calls);
  AssertEquals('Only the storage override owns its progress reporting',0,FProgressCount);
  LStorage.RaiseOnLoad := False;
  LStorage.LoadPendingBufferOperations(FLoaded);
  AssertEquals(2,LStorage.Calls);
end;

initialization
  RegisterTest(TPendingRestoreTests);
end.
