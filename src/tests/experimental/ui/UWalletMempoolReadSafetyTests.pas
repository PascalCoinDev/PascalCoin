unit UWalletMempoolReadSafetyTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE. }

{$mode delphi}

interface

uses fpcunit, testregistry;

type
  TWalletMempoolReadSafetyTests = class(TTestCase)
  published
    procedure PublishedOperationsHaveCompleteReadHashes;
    procedure CachedTreeReadsRespectActiveWriters;
    procedure ParallelReadersSeeCompleteStableDigests;
  end;

implementation

uses UOperationReadHashChecks;

procedure TWalletMempoolReadSafetyTests.PublishedOperationsHaveCompleteReadHashes;
begin
  CheckPublishedOperationReadHashes;
end;

procedure TWalletMempoolReadSafetyTests.CachedTreeReadsRespectActiveWriters;
begin
  CheckCachedTreeReadTakesExistingLock;
end;

procedure TWalletMempoolReadSafetyTests.ParallelReadersSeeCompleteStableDigests;
begin
  CheckParallelOperationReadHashes;
end;

initialization
  RegisterTest(TWalletMempoolReadSafetyTests);

end.
