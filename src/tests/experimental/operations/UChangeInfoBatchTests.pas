unit UChangeInfoBatchTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$mode delphi}

interface

uses fpcunit, testregistry;

type
  TChangeInfoBatchTests = class(TTestCase)
  published
    procedure MixedTargetsKeepOrderAndBoundaryTypes;
    procedure CommonSignerPaysOnlyChangedTargets;
    procedure SecondarySignerUsesTargetBeforeFallback;
    procedure SkippedTargetsNeedNoFundsKeyOrCounter;
    procedure ChangedTargetsMustBeFunded;
    procedure CallerSnapshotsAndNamesArePreserved;
    procedure TargetSelectionKeepsOrderAndOwnArray;
    procedure ApprovalRejectsAChangedTargetSet;
    procedure AllUnchangedAndSingleNoOpAreRejected;
    procedure MalformedSelectionsAreRejected;
  end;

implementation

uses UChangeInfoBatchChecks;

procedure TChangeInfoBatchTests.MixedTargetsKeepOrderAndBoundaryTypes;
begin
  CheckChangeInfoMixedTargets;
end;

procedure TChangeInfoBatchTests.CommonSignerPaysOnlyChangedTargets;
begin
  CheckChangeInfoCommonSigner;
end;

procedure TChangeInfoBatchTests.SecondarySignerUsesTargetBeforeFallback;
begin
  CheckChangeInfoSecondarySigner;
end;

procedure TChangeInfoBatchTests.SkippedTargetsNeedNoFundsKeyOrCounter;
begin
  CheckChangeInfoSkippedUnusableAccount;
end;

procedure TChangeInfoBatchTests.ChangedTargetsMustBeFunded;
begin
  CheckChangeInfoChangedFunding;
end;

procedure TChangeInfoBatchTests.CallerSnapshotsAndNamesArePreserved;
begin
  CheckChangeInfoPreservedSnapshots;
end;

procedure TChangeInfoBatchTests.TargetSelectionKeepsOrderAndOwnArray;
begin
  CheckChangeInfoTargetSelection;
end;

procedure TChangeInfoBatchTests.ApprovalRejectsAChangedTargetSet;
begin
  CheckChangeInfoApprovedTargets;
end;

procedure TChangeInfoBatchTests.AllUnchangedAndSingleNoOpAreRejected;
begin
  CheckChangeInfoNoChanges;
end;

procedure TChangeInfoBatchTests.MalformedSelectionsAreRejected;
begin
  CheckChangeInfoMalformedRequests;
end;

initialization
  RegisterTest(TChangeInfoBatchTests);

end.
