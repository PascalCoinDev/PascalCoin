unit UAbstractMemStorageFailureTests;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Registers the real optional-storage worker and shutdown regressions. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses SysUtils, fpcunit, testregistry, UAbstractMemStorageFailureChecks;

type
  TAbstractMemStorageFailureTests = class(TTestCase)
  private
    procedure RunCheck(ACheck: TAbstractMemStorageFailureCheck);
  published
    procedure FalseCompletionFlushPausesStorage;
    procedure BackgroundWriteFailurePausesStorage;
    procedure FailedQueueDrainReturnsWithoutDeadlock;
    procedure FailedCleanupPreservesUnstableMarker;
    procedure SuccessfulShutdownDrainsQueuedWrites;
    procedure ShutdownWaitsForInFlightWrite;
  end;

implementation

procedure TAbstractMemStorageFailureTests.RunCheck(ACheck: TAbstractMemStorageFailureCheck);
var LID: TGUID;
begin
  AssertEquals('Create a unique owned test log',0,CreateGUID(LID));
  AssertEquals(0,RunAbstractMemStorageFailureCheck(ACheck,IncludeTrailingPathDelimiter(GetTempDir)+'AbstractMemStorage-'+GUIDToString(LID)+'.log'));
end;

procedure TAbstractMemStorageFailureTests.FalseCompletionFlushPausesStorage;
begin
  RunCheck(amscFalseCompletionFlush);
end;

procedure TAbstractMemStorageFailureTests.BackgroundWriteFailurePausesStorage;
begin
  RunCheck(amscBackgroundWriteFailure);
end;

procedure TAbstractMemStorageFailureTests.FailedQueueDrainReturnsWithoutDeadlock;
begin
  RunCheck(amscFailedQueueDrain);
end;

procedure TAbstractMemStorageFailureTests.FailedCleanupPreservesUnstableMarker;
begin
  RunCheck(amscFailedCleanupPreservesMarker);
end;

procedure TAbstractMemStorageFailureTests.SuccessfulShutdownDrainsQueuedWrites;
begin
  RunCheck(amscSuccessfulDrain);
end;

procedure TAbstractMemStorageFailureTests.ShutdownWaitsForInFlightWrite;
begin
  RunCheck(amscInFlightShutdown);
end;

initialization
  RegisterTest(TAbstractMemStorageFailureTests);
end.
