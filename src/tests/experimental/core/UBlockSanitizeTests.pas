unit UBlockSanitizeTests;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  FPCUnit registration reuses the same actual-operation checks as both native
  compiler probes; operation, replay and index implementations are not copied. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses Classes, SysUtils, fpcunit, testregistry, UMempoolSanitizeChecks;

type
  TBlockSanitizeTests = class(TTestCase)
  private
    procedure RunCheck(ACheck: TMempoolSanitizeCheck);
  published
    procedure AllValidPreservesOrderHashFeesAndTreeOwnership;
    procedure RejectedAndDuplicateOperationsRetainLegacySequence;
    procedure DeletedDuplicateIndexForcesMaterializedFallback;
    procedure CurrentProtocolFeeLimitAndSignerMetadataAreAdopted;
    procedure ReplayExceptionPreservesOriginalSequenceAndReleasesLocks;
    procedure OperationRegistryRetainsIndependentTreeOwners;
    procedure RejectedRecoverMetadataAndEmptyReplayRemainCorrect;
    procedure BulkIndexesPreserveFirstOccurrenceAndUnsignedReferenceBoundaries;
    procedure SignatureCachesRespectBlockAndOperationProtocols;
  end;

implementation

procedure TBlockSanitizeTests.RunCheck(ACheck: TMempoolSanitizeCheck);
var LID: TGUID; LPath: String;
begin
  CreateGUID(LID);
  LPath := IncludeTrailingPathDelimiter(GetTempDir) +
    'BlockSanitize-' + GUIDToString(LID) + '.log';
  AssertEquals('The shared actual-core scenario must succeed',0,
    RunMempoolSanitizeCheck(ACheck,LPath));
end;

procedure TBlockSanitizeTests.AllValidPreservesOrderHashFeesAndTreeOwnership;
begin
  RunCheck(mscAllValid);
end;

procedure TBlockSanitizeTests.RejectedAndDuplicateOperationsRetainLegacySequence;
begin
  RunCheck(mscRejectedAndDuplicates);
end;

procedure TBlockSanitizeTests.DeletedDuplicateIndexForcesMaterializedFallback;
begin
  RunCheck(mscDeletedDuplicateIndex);
end;

procedure TBlockSanitizeTests.CurrentProtocolFeeLimitAndSignerMetadataAreAdopted;
begin
  RunCheck(mscZeroFeeSignerLimit);
end;

procedure TBlockSanitizeTests.ReplayExceptionPreservesOriginalSequenceAndReleasesLocks;
begin
  RunCheck(mscReplayException);
end;

procedure TBlockSanitizeTests.OperationRegistryRetainsIndependentTreeOwners;
begin
  RunCheck(mscRegistryOwnership);
end;

procedure TBlockSanitizeTests.RejectedRecoverMetadataAndEmptyReplayRemainCorrect;
begin
  RunCheck(mscRecoverRejection);
end;

procedure TBlockSanitizeTests.BulkIndexesPreserveFirstOccurrenceAndUnsignedReferenceBoundaries;
begin
  RunCheck(mscReferenceBoundaries);
end;

procedure TBlockSanitizeTests.SignatureCachesRespectBlockAndOperationProtocols;
begin
  RunCheck(mscProtocolSignatureCaches);
end;

initialization
  RegisterTest(TBlockSanitizeTests);
end.
