unit UWalletProgressDisplayTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}

interface

uses Classes, SysUtils, fpcunit, testregistry, UAccounts, UNode, UProgressDisplay;

type
  TWalletProgressDisplayTests = class(TTestCase)
  private
    FSavedFormatSettings: TFormatSettings;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MeasurementsAreFormattedOnce;
    procedure DisplayBlockMetadataPreservesRawPosition;
    procedure UnknownTotalsDoNotClaimCompletion;
    procedure LargeCountersDoNotOverflowPercentage;
    procedure DownloadMetadataIsFormattedAtTheBoundary;
    procedure CreationClearsMetadataAndEveryStageHasPresentation;
    procedure OriginalCoreTemplatesAreExact;
    procedure OriginalGuiDecoratorsAreExact;
    procedure OriginalNetworkDecoratorsAreExact;
    procedure OriginalDaemonLogPrefixIsExact;
    procedure UnknownTotalsKeepOriginalSeparator;
    procedure OriginalPercentageIsNotClipped;
    procedure OriginalPercentageRoundingAndLocaleArePreserved;
    procedure ZeroBlockMetadataIsNotDiscarded;
    procedure EmptyStorageFailureHasNoPresentation;
    procedure StorageFailurePreservesOriginalDetails;
    procedure EveryStorageFailureStageHasPresentation;
    procedure StorageFailureFormattingDoesNotMutateData;
    procedure StorageFailureWithoutOptionalDetailsRemainsClear;
  end;

implementation

procedure TWalletProgressDisplayTests.SetUp;
begin
  inherited;
  FSavedFormatSettings := DefaultFormatSettings;
  DefaultFormatSettings.DecimalSeparator := '.';
end;

procedure TWalletProgressDisplayTests.TearDown;
begin
  DefaultFormatSettings := FSavedFormatSettings;
  inherited;
end;

procedure TWalletProgressDisplayTests.MeasurementsAreFormattedOnce;
var L: TProgressInfo;
begin
  L := TProgressInfo.Create(psPreparingOperations, 2, 4);
  AssertEquals('Preparing operations 2/4', FormatProgressMessage(L));
  AssertEquals('Preparing operations 2/4 50.00%', FormatProgressInfo(L));
  AssertEquals(Int64(2), L.CurrentPosition);
  AssertEquals(Int64(4), L.TotalCount);
end;

procedure TWalletProgressDisplayTests.DisplayBlockMetadataPreservesRawPosition;
var L: TProgressInfo; LText: String;
begin
  // The safebox decoder historically reports an index with an extra sentinel
  // in its numeric total. Display metadata must not silently alter that data.
  L := TProgressInfo.Create(psReadingSafebox, 5, 11);
  L.BlockNumber := 6;
  L.BlockCount := 10;
  LText := FormatProgressInfo(L);
  AssertEquals('Reading Safebox block 6/10 45.45%', LText);
  AssertEquals('Raw counters were duplicated after display metadata', 0, Pos('5/11', LText));
  AssertEquals(Int64(5), L.CurrentPosition);
  AssertEquals(Int64(11), L.TotalCount);
end;

procedure TWalletProgressDisplayTests.UnknownTotalsDoNotClaimCompletion;
var L: TProgressInfo;
begin
  L := TProgressInfo.Create(psFinalizingRestore);
  AssertEquals('Finalizing local restore... ', FormatProgressInfo(L));
  L.TotalCount := -1;
  AssertEquals('Finalizing local restore... ', FormatProgressInfo(L));
  L := TProgressInfo.Create(psNone, 1, 1);
  AssertEquals('', FormatProgressInfo(L));
  AssertEquals('', FormatProgressMessage(L));
  AssertEquals('', FormatProgressLogInfo(L));
  L := TProgressInfo.Create(psApplyingOperations, 10, 10);
  AssertEquals('Applying operations 10/10 100.00%', FormatProgressInfo(L));
  AssertEquals('A phase percentage must not invent an overall completion stage',
    Ord(psApplyingOperations), Ord(L.Stage));
end;

procedure TWalletProgressDisplayTests.LargeCountersDoNotOverflowPercentage;
var L: TProgressInfo; LText: String;
begin
  L := TProgressInfo.Create(psReadingOperations, High(Int64) div 2, High(Int64));
  LText := FormatProgressInfo(L);
  AssertEquals('Reading operations 4611686018427387903/9223372036854775807 50.00%', LText);
  AssertEquals('Formatting modified the raw position', High(Int64) div 2, L.CurrentPosition);
end;

procedure TWalletProgressDisplayTests.DownloadMetadataIsFormattedAtTheBoundary;
var L: TProgressInfo; LFileName: String;
begin
  L := TProgressInfo.Create(psSavingSafebox);
  L.ChunksCount := 12;
  LFileName := PathDelim + 'owned' + PathDelim + 'generated' + PathDelim + 'safebox_100.safebox';
  L.FileName := LFileName;
  AssertEquals('Saving Safebox 12 chunks to safebox_100.safebox', FormatProgressMessage(L));
  AssertEquals('Saving Safebox 12 chunks to safebox_100.safebox ', FormatProgressInfo(L));
  AssertEquals(LFileName, L.FileName);
  L := TProgressInfo.Create(psReceivedSafebox);
  L.BlockCount := 100;
  L.PeerAddress := '127.0.0.1:4004';
  AssertEquals('Received new safebox with 100 blocks from 127.0.0.1:4004', FormatProgressInfo(L));
  L := TProgressInfo.Create(psReadingOperations, 1, 2);
  L.Context := pcCheckingNewSafebox;
  AssertEquals('Checking new safebox: Reading operations 1/2 50.00%', FormatProgressInfo(L));
  L := TProgressInfo.Create(psReadingBlockBatch);
  L.StartBlock := 7;
  L.EndBlock := 9;
  L.OperationsCount := 12;
  AssertEquals('Reading blocks from 7 to 9 with 12 operations ', FormatProgressInfo(L));
end;

procedure TWalletProgressDisplayTests.CreationClearsMetadataAndEveryStageHasPresentation;
var L: TProgressInfo; S: TProgressStage;
begin
  L := TProgressInfo.Create(psSavingSafebox, 2, 3);
  L.FileName := 'old filename';
  L.PeerAddress := 'old peer';
  L.Context := pcCheckingNewSafebox;
  L.ChunksCount := 9;
  L := TProgressInfo.Create(psFinalizingRestore);
  AssertEquals('', L.FileName);
  AssertEquals('', L.PeerAddress);
  AssertEquals(Ord(pcNone), Ord(L.Context));
  AssertEquals(Int64(0), L.CurrentPosition);
  AssertEquals(Int64(0), L.TotalCount);
  AssertEquals(Int64(0), L.BlockNumber);
  AssertEquals(Int64(0), L.BlockCount);
  AssertEquals(Int64(0), L.StartBlock);
  AssertEquals(Int64(0), L.EndBlock);
  AssertEquals(Int64(0), L.OperationsCount);
  AssertEquals(Int64(0), L.ChunksCount);
  for S := Succ(Low(TProgressStage)) to High(TProgressStage) do
    AssertTrue('No presentation for typed progress stage ' + IntToStr(Ord(S)),
      FormatProgressInfo(TProgressInfo.Create(S)) <> '');
end;

procedure TWalletProgressDisplayTests.OriginalCoreTemplatesAreExact;
  procedure Expect(AStage: TProgressStage; const AExpected: String);
  var L: TProgressInfo;
  begin
    L := TProgressInfo.Create(AStage, 2, 7);
    L.BlockNumber := 6; L.BlockCount := 10;
    L.StartBlock := 7; L.EndBlock := 9; L.OperationsCount := 12;
    L.ChunksCount := 8;
    L.FileName := 'owned' + PathDelim + 'safebox_100.safebox';
    L.PeerAddress := '127.0.0.1:4004';
    AssertEquals(AExpected, FormatProgressMessage(L));
    AssertEquals(Int64(2), L.CurrentPosition);
    AssertEquals(Int64(7), L.TotalCount);
  end;
begin
  // Literal oracles come from the removed producers, not this formatter.
  Expect(psNone, '');
  Expect(psReadingSafebox, 'Reading Safebox block 6/10');
  Expect(psValidatingOperationBlocks, 'Validating OperationBlock info 2/7');
  Expect(psCheckingSafeboxIntegrity, 'Checking Safebox integrity');
  Expect(psReadingBlockBatch, 'Reading blocks from 7 to 9 with 12 operations');
  Expect(psReadingBlockchain, 'Reading blockchain block 6/10');
  Expect(psLoadingPendingOperations, 'Loading saved pending operations');
  Expect(psReadingOperations, 'Reading operations 2/7');
  Expect(psPreparingOperations, 'Preparing operations 2/7');
  Expect(psValidatingSignatures, 'Validating signatures 2/7');
  Expect(psApplyingOperations, 'Applying operations 2/7');
  Expect(psFinalizingOperations, 'Finalizing operations 2/7');
  Expect(psFinalizingRestore, 'Finalizing local restore...');
  Expect(psDownloadingSafeboxChunks, 'Downloading Safebox chunks 2/8');
  Expect(psSavingSafebox, 'Saving Safebox 8 chunks to safebox_100.safebox');
  Expect(psReceivedSafebox, 'Received new safebox with 10 blocks from 127.0.0.1:4004');
  Expect(psDownloadingBlockchain, 'Downloading block 6 from 127.0.0.1:4004');
end;

procedure TWalletProgressDisplayTests.OriginalGuiDecoratorsAreExact;
var L: TProgressInfo;
begin
  L := TProgressInfo.Create(psValidatingOperationBlocks, 2, 7);
  AssertEquals('Validating OperationBlock info 2/7 28.57%', FormatProgressInfo(L));
  L := TProgressInfo.Create(psReadingOperations, 1, 3);
  AssertEquals('Reading operations 1/3 33.33%', FormatProgressInfo(L));
  L := TProgressInfo.Create(psPreparingOperations, 2, 3);
  AssertEquals('Preparing operations 2/3 66.67%', FormatProgressInfo(L));
  L.Stage := psValidatingSignatures;
  AssertEquals('Validating signatures 2/3 66.67%', FormatProgressInfo(L));
  L.Stage := psApplyingOperations;
  AssertEquals('Applying operations 2/3 66.67%', FormatProgressInfo(L));
  L := TProgressInfo.Create(psFinalizingOperations, 0, 3);
  AssertEquals('Finalizing operations 0/3 0.00%', FormatProgressInfo(L));
  L := TProgressInfo.Create(psCheckingSafeboxIntegrity, 9, 9);
  AssertEquals('Checking Safebox integrity 100.00%', FormatProgressInfo(L));
end;

procedure TWalletProgressDisplayTests.OriginalNetworkDecoratorsAreExact;
var L: TProgressInfo;
begin
  L := TProgressInfo.Create(psDownloadingSafeboxChunks, 2, 8);
  L.ChunksCount := 10;
  AssertEquals('Downloading Safebox chunks 2/10 25.00%', FormatProgressInfo(L));
  L := TProgressInfo.Create(psReadingSafebox, 5, 11);
  L.BlockNumber := 6; L.BlockCount := 10;
  L.Context := pcCheckingNewSafebox;
  AssertEquals('Checking new safebox: Reading Safebox block 6/10 45.45%', FormatProgressInfo(L));
  L := TProgressInfo.Create(psReceivedSafebox, 1, 2);
  L.BlockCount := 100; L.PeerAddress := '127.0.0.1:4004';
  AssertEquals('Received new safebox with 100 blocks from 127.0.0.1:4004', FormatProgressInfo(L));
  L := TProgressInfo.Create(psDownloadingBlockchain);
  L.BlockNumber := 99; L.PeerAddress := '127.0.0.1:4004';
  AssertEquals('Downloading block 99 from 127.0.0.1:4004', FormatProgressInfo(L));
end;

procedure TWalletProgressDisplayTests.OriginalDaemonLogPrefixIsExact;
var L: TProgressInfo;
begin
  L := TProgressInfo.Create(psReadingSafebox, 5, 11);
  L.BlockNumber := 6; L.BlockCount := 10;
  AssertEquals('Progress (5/11): Reading Safebox block 6/10', FormatProgressLogInfo(L));
  L := TProgressInfo.Create(psPreparingOperations, 2, 4);
  AssertEquals('Progress (2/4): Preparing operations 2/4', FormatProgressLogInfo(L));
  L := TProgressInfo.Create(psLoadingPendingOperations);
  AssertEquals('Progress (0/0): Loading saved pending operations', FormatProgressLogInfo(L));
  L := TProgressInfo.Create(psNone, 1, 1);
  AssertEquals('', FormatProgressLogInfo(L));
end;

procedure TWalletProgressDisplayTests.UnknownTotalsKeepOriginalSeparator;
var L: TProgressInfo;
begin
  L := TProgressInfo.Create(psLoadingPendingOperations);
  AssertEquals('Loading saved pending operations ', FormatProgressInfo(L));
  L := TProgressInfo.Create(psCheckingSafeboxIntegrity);
  L.Context := pcCheckingNewSafebox;
  AssertEquals('Checking new safebox: Checking Safebox integrity ', FormatProgressInfo(L));
  L := TProgressInfo.Create(psFinalizingOperations, 0, 0);
  AssertEquals('Finalizing operations 0/0 ', FormatProgressInfo(L));
  L.TotalCount := -1;
  AssertEquals('Finalizing operations 0/-1 ', FormatProgressInfo(L));
end;

procedure TWalletProgressDisplayTests.OriginalPercentageIsNotClipped;
var L: TProgressInfo;
begin
  L := TProgressInfo.Create(psPreparingOperations, 15, 10);
  AssertEquals('Preparing operations 15/10 150.00%', FormatProgressInfo(L));
  L := TProgressInfo.Create(psReadingOperations, -1, 10);
  AssertEquals('Reading operations -1/10 -10.00%', FormatProgressInfo(L));
  AssertEquals(Int64(-1), L.CurrentPosition);
  AssertEquals(Int64(10), L.TotalCount);
end;

procedure TWalletProgressDisplayTests.OriginalPercentageRoundingAndLocaleArePreserved;
var L: TProgressInfo;
begin
  L := TProgressInfo.Create(psValidatingOperationBlocks, 2, 3);
  AssertEquals('Validating OperationBlock info 2/3 66.67%', FormatProgressInfo(L));
  DefaultFormatSettings.DecimalSeparator := ',';
  AssertEquals('Validating OperationBlock info 2/3 66,67%', FormatProgressInfo(L));
  L := TProgressInfo.Create(psDownloadingSafeboxChunks, 1, 3);
  L.ChunksCount := 3;
  AssertEquals('Downloading Safebox chunks 1/3 33,33%', FormatProgressInfo(L));
end;

procedure TWalletProgressDisplayTests.ZeroBlockMetadataIsNotDiscarded;
var L: TProgressInfo;
begin
  L := TProgressInfo.Create(psReadingBlockchain);
  AssertEquals('Reading blockchain block 0/0', FormatProgressMessage(L));
  AssertEquals('Reading blockchain block 0/0 ', FormatProgressInfo(L));
  L := TProgressInfo.Create(psReadingSafebox);
  AssertEquals('Reading Safebox block 0/0 ', FormatProgressInfo(L));
end;

procedure TWalletProgressDisplayTests.EmptyStorageFailureHasNoPresentation;
var LFailure: TNodeStorageFailure;
begin
  LFailure := Default(TNodeStorageFailure);
  AssertEquals('',FormatStorageFailure(LFailure));
  LFailure.ErrorMessage := 'Unused stale detail';
  LFailure.FileName := 'old-checkpoint';
  AssertEquals('',FormatStorageFailure(LFailure));
end;

procedure TWalletProgressDisplayTests.StorageFailurePreservesOriginalDetails;
var LFailure: TNodeStorageFailure;
begin
  LFailure := Default(TNodeStorageFailure);
  LFailure.HasFailure := True;
  LFailure.Stage := sfsCheckpoint;
  LFailure.FileName := 'owned'+PathDelim+'checkpoint6.am_safebox';
  LFailure.ErrorClass := 'EWriteError';
  LFailure.ErrorMessage := 'Stream write error';
  AssertEquals('Storage error (restart required) during checkpoint save: EWriteError: Stream write error ['+LFailure.FileName+']',FormatStorageFailure(LFailure));
end;

procedure TWalletProgressDisplayTests.EveryStorageFailureStageHasPresentation;
const LExpected: array[TNodeStorageFailureStage] of String = ('local storage','safebox flush','checkpoint save','block storage','pending operations save','local blockchain restore');
var LFailure: TNodeStorageFailure; LStage: TNodeStorageFailureStage;
begin
  LFailure := Default(TNodeStorageFailure);
  LFailure.HasFailure := True;
  for LStage := Low(TNodeStorageFailureStage) to High(TNodeStorageFailureStage) do begin
    LFailure.Stage := LStage;
    AssertEquals('Storage error (restart required) during '+LExpected[LStage],FormatStorageFailure(LFailure));
  end;
end;

procedure TWalletProgressDisplayTests.StorageFailureFormattingDoesNotMutateData;
var LFailure: TNodeStorageFailure;
begin
  LFailure := Default(TNodeStorageFailure);
  LFailure.HasFailure := True;
  LFailure.Stage := sfsBlockStream;
  LFailure.FileName := 'owned'+PathDelim+'BlockChainStream.blocks';
  LFailure.ErrorClass := 'EOSError';
  LFailure.ErrorMessage := 'Original operating system detail';
  FormatStorageFailure(LFailure);
  AssertTrue(LFailure.HasFailure);
  AssertEquals(Ord(sfsBlockStream),Ord(LFailure.Stage));
  AssertEquals('owned'+PathDelim+'BlockChainStream.blocks',LFailure.FileName);
  AssertEquals('EOSError',LFailure.ErrorClass);
  AssertEquals('Original operating system detail',LFailure.ErrorMessage);
end;

procedure TWalletProgressDisplayTests.StorageFailureWithoutOptionalDetailsRemainsClear;
var LFailure: TNodeStorageFailure;
begin
  LFailure := Default(TNodeStorageFailure);
  LFailure.HasFailure := True;
  LFailure.Stage := sfsUnknown;
  AssertEquals('Storage error (restart required) during local storage',FormatStorageFailure(LFailure));
end;

initialization
  RegisterTest(TWalletProgressDisplayTests);

end.
