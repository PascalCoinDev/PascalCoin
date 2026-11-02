unit UCheckpointStorageTests;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Registers the same real-storage regressions used by the FPC/Delphi probe. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses SysUtils, fpcunit, testregistry, UCheckpointStorageChecks;

type
  TCheckpointStorageTests = class(TTestCase)
  private
    procedure RunCheck(ACheck: TCheckpointStorageCheck);
  published
    procedure TemporaryCreateFailurePreservesPreviousCheckpoint;
    procedure ShortWritePreservesPreviousCheckpoint;
    procedure FlushFailurePreservesPreviousCheckpoint;
    procedure InvalidTemporaryPreservesPreviousCheckpoint;
    procedure AtomicReplaceFailurePreservesPreviousCheckpoint;
    procedure FailedFirstCheckpointDoesNotPublishPartialFiles;
    procedure FailedCheckpointCanRetryAndReopen;
    procedure MemoryCheckpointPublishesCompleteState;
    procedure EmptyCheckpointCanReopen;
    procedure MissingCandidateDoesNotCreateFiles;
    procedure TruncatedCandidateIsRejectedWithoutMutation;
    procedure MalformedCandidateIsRejectedWithoutMutation;
    procedure HealthyMainSurvivesCorruptCheckpoint;
    procedure CorruptMainRecoversFromValidCheckpoint;
    procedure AllCorruptCandidatesFailWithoutOverwritingEvidence;
    procedure ValidCheckpointReplaysLaterLocalBlocks;
    procedure FailedLiveFlushExceptionKeepsUnstableMarker;
    procedure FailedLiveShortWriteKeepsUnstableMarker;
    procedure FailedUncachedWriteExceptionKeepsUnstableMarker;
    procedure FailedUncachedShortWriteKeepsUnstableMarker;
    procedure FailedUncachedCloseDoesNotPublishStableState;
    procedure UncachedPartialRetriesKeepAllFailedRegions;
    procedure FailedAutomaticCachedCloseLogsAndReleases;
    procedure HealthyMainSurvivesNewerLegacyHeaderWithInvalidBody;
    procedure OlderLegacySurvivesNewerHeaderWithInvalidBody;
    procedure FailedUnstableHeaderMustRetryBeforeLaterPayload;
    {$IFDEF MSWINDOWS}
    procedure RealSharingFailurePreservesPreviousCheckpoint;
    {$ENDIF}
  end;

implementation

procedure TCheckpointStorageTests.RunCheck(ACheck: TCheckpointStorageCheck);
var LID: TGUID;
begin
  CreateGUID(LID);
  AssertEquals(0,RunCheckpointStorageCheck(ACheck,IncludeTrailingPathDelimiter(GetTempDir)+'CheckpointStorage-'+GUIDToString(LID)+'.log'));
end;

procedure TCheckpointStorageTests.TemporaryCreateFailurePreservesPreviousCheckpoint;
begin
  RunCheck(cscFactoryFailure);
end;

procedure TCheckpointStorageTests.ShortWritePreservesPreviousCheckpoint;
begin
  RunCheck(cscPartialWrite);
end;

procedure TCheckpointStorageTests.FlushFailurePreservesPreviousCheckpoint;
begin
  RunCheck(cscFlushFailure);
end;

procedure TCheckpointStorageTests.InvalidTemporaryPreservesPreviousCheckpoint;
begin
  RunCheck(cscValidationFailure);
end;

procedure TCheckpointStorageTests.AtomicReplaceFailurePreservesPreviousCheckpoint;
begin
  RunCheck(cscPublicationFailure);
end;

procedure TCheckpointStorageTests.FailedFirstCheckpointDoesNotPublishPartialFiles;
begin
  RunCheck(cscAbsentTargetFailures);
end;

procedure TCheckpointStorageTests.FailedCheckpointCanRetryAndReopen;
begin
  RunCheck(cscSuccessfulRetry);
end;

procedure TCheckpointStorageTests.MemoryCheckpointPublishesCompleteState;
begin
  RunCheck(cscMemoryPublication);
end;

procedure TCheckpointStorageTests.EmptyCheckpointCanReopen;
begin
  RunCheck(cscEmptyPublication);
end;

procedure TCheckpointStorageTests.MissingCandidateDoesNotCreateFiles;
begin
  RunCheck(cscAnalyzeMissing);
end;

procedure TCheckpointStorageTests.TruncatedCandidateIsRejectedWithoutMutation;
begin
  RunCheck(cscAnalyzeTruncated);
end;

procedure TCheckpointStorageTests.MalformedCandidateIsRejectedWithoutMutation;
begin
  RunCheck(cscAnalyzeMalformed);
end;

procedure TCheckpointStorageTests.HealthyMainSurvivesCorruptCheckpoint;
begin
  RunCheck(cscHealthyMainBadCheckpoint);
end;

procedure TCheckpointStorageTests.CorruptMainRecoversFromValidCheckpoint;
begin
  RunCheck(cscCorruptMainGoodCheckpoint);
end;

procedure TCheckpointStorageTests.AllCorruptCandidatesFailWithoutOverwritingEvidence;
begin
  RunCheck(cscAllCorruptCandidates);
end;

procedure TCheckpointStorageTests.ValidCheckpointReplaysLaterLocalBlocks;
begin
  RunCheck(cscCheckpointThenLocalReplay);
end;

procedure TCheckpointStorageTests.FailedLiveFlushExceptionKeepsUnstableMarker;
begin
  RunCheck(cscFailedFlushException);
end;

procedure TCheckpointStorageTests.FailedLiveShortWriteKeepsUnstableMarker;
begin
  RunCheck(cscFailedFlushShortWrite);
end;

procedure TCheckpointStorageTests.FailedUncachedWriteExceptionKeepsUnstableMarker;
begin
  RunCheck(cscUncachedWriteException);
end;

procedure TCheckpointStorageTests.FailedUncachedShortWriteKeepsUnstableMarker;
begin
  RunCheck(cscUncachedShortWrite);
end;

procedure TCheckpointStorageTests.FailedUncachedCloseDoesNotPublishStableState;
begin
  RunCheck(cscUncachedFailedClose);
end;

procedure TCheckpointStorageTests.UncachedPartialRetriesKeepAllFailedRegions;
begin
  RunCheck(cscUncachedPartialRetry);
end;

procedure TCheckpointStorageTests.FailedAutomaticCachedCloseLogsAndReleases;
begin
  RunCheck(cscCachedFailedClose);
end;

procedure TCheckpointStorageTests.HealthyMainSurvivesNewerLegacyHeaderWithInvalidBody;
begin
  RunCheck(cscHealthyMainBadLegacyBody);
end;

procedure TCheckpointStorageTests.OlderLegacySurvivesNewerHeaderWithInvalidBody;
begin
  RunCheck(cscOlderLegacySurvivesBadNewerBody);
end;

procedure TCheckpointStorageTests.FailedUnstableHeaderMustRetryBeforeLaterPayload;
begin
  RunCheck(cscUnstableHeaderRetryBeforeLaterPayload);
end;

{$IFDEF MSWINDOWS}
procedure TCheckpointStorageTests.RealSharingFailurePreservesPreviousCheckpoint;
begin
  RunCheck(cscSharingDenied);
end;
{$ENDIF}

initialization
  RegisterTest(TCheckpointStorageTests);
end.
