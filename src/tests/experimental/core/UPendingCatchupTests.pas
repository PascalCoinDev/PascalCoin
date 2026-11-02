unit UPendingCatchupTests;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  FPCUnit and the native Delphi probe execute the same pending catch-up cases. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses Classes, SysUtils, fpcunit, testregistry, UPendingCatchupChecks;

type
  TPendingCatchupTests = class(TTestCase)
  private
    procedure RunCheck(ACheck: TPendingCatchupCheck);
  published
    procedure BufferedCommitsDeferOneFinalPendingReplay;
    procedure RemoteTargetHoldsPendingAcrossBatchGaps;
    procedure FinalReplayRejectsConfirmedOperationsExactly;
    procedure PendingReadsAndNewAdmissionReportUnavailable;
    procedure NewPhysicalWorkPreventsPrematureCompletion;
    procedure RetiredAndReconnectedOwnersDrainBeforeReplay;
    procedure QuitPersistsOneCompletePendingPackage;
    procedure UnchangedEmptyScopeNeedsNoReplay;
    procedure FailedReplayKeepsCompleteInputAndRetriesWithoutNewEvents;
    procedure QuitAfterFailedReplayPersistsTheCompleteOriginalPackage;
  end;

implementation

procedure TPendingCatchupTests.RunCheck(ACheck: TPendingCatchupCheck);
var LID: TGUID; LPath: String;
begin
  CreateGUID(LID);
  LPath:=IncludeTrailingPathDelimiter(GetTempDir)+'PendingCatchup-'+GUIDToString(LID)+'.log';
  AssertEquals('The shared actual-node catch-up scenario must pass',0,
    RunPendingCatchupCheck(ACheck,LPath));
end;

procedure TPendingCatchupTests.BufferedCommitsDeferOneFinalPendingReplay;
begin
  RunCheck(pccBufferedCommits);
end;

procedure TPendingCatchupTests.RemoteTargetHoldsPendingAcrossBatchGaps;
begin
  RunCheck(pccInterBatchTarget);
end;

procedure TPendingCatchupTests.FinalReplayRejectsConfirmedOperationsExactly;
begin
  RunCheck(pccMixedRejections);
end;

procedure TPendingCatchupTests.PendingReadsAndNewAdmissionReportUnavailable;
begin
  RunCheck(pccUnavailableContracts);
end;

procedure TPendingCatchupTests.NewPhysicalWorkPreventsPrematureCompletion;
begin
  RunCheck(pccNewWorkDuringReplay);
end;

procedure TPendingCatchupTests.RetiredAndReconnectedOwnersDrainBeforeReplay;
begin
  RunCheck(pccRetiredScopes);
end;

procedure TPendingCatchupTests.QuitPersistsOneCompletePendingPackage;
begin
  RunCheck(pccShutdownPackage);
end;

procedure TPendingCatchupTests.UnchangedEmptyScopeNeedsNoReplay;
begin
  RunCheck(pccEmptyScope);
end;

procedure TPendingCatchupTests.FailedReplayKeepsCompleteInputAndRetriesWithoutNewEvents;
begin
  RunCheck(pccReplayFailureRetry);
end;

procedure TPendingCatchupTests.QuitAfterFailedReplayPersistsTheCompleteOriginalPackage;
begin
  RunCheck(pccReplayFailureShutdown);
end;

initialization
  RegisterTest(TPendingCatchupTests);
end.
