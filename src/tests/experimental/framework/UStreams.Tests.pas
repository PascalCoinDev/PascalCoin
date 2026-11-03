unit UStreams.Tests;

{ Copyright © 2026 Herman Schoenfeld <https://www.sphere10.com>
  Distributed under the MIT software license; see LICENSE. }

{$MODE Delphi}{$H+}
interface
uses fpcunit, testregistry;
type
  TBufferedStreamTests = class(TTestCase)
  published
    procedure NilSourceRejected;
    procedure NonpositiveBufferRejectedWithoutOwnership;
    procedure BorrowedSourceRemainsAlive;
    procedure OwnedSourceDisposedOnce;
    procedure NonzeroInitialPosition;
    procedure LogicalAndPhysicalCursors;
    procedure SingleByteReadsCollapseSourceCalls;
    procedure AllBufferBoundariesAndEOF;
    procedure EmptyStreamEOF;
    procedure ZeroCountsPerformNoIO;
    procedure NegativeCountsRejectedBeforeIO;
    procedure ReadBufferCrossesBoundaries;
    procedure ShortReadsDoNotLoseData;
    procedure AllSeekOrigins;
    procedure SeekWithinRetainedWindow;
    procedure SeekOutsideWindowAndBack;
    procedure SeekBeyondEOFDoesNotResize;
    procedure WriteAfterReadUsesLogicalCursor;
    procedure ReadAfterWriteRejectsStaleBuffer;
    procedure ShortWriteReturnsActualCount;
    procedure SourceReadFailurePreserved;
    procedure SourceWriteFailurePreserved;
    procedure TruncatePreservesCursorSemantics;
    procedure ExpandThenWritePreservesData;
    procedure ResizeFailurePreserved;
    procedure SizeFailurePreserved;
    procedure ReadOnlySourceStillReadable;
    procedure Sparse64BitOffsetsAndBoundedReads;
    procedure DeterministicDifferentialOperations;
    procedure OwnedSourceDisposedAfterReadError;
    procedure WriteThroughDoesNotRead;
    procedure ForwardOnlyReadsPreserveCapabilities;
    procedure BorrowedDestroyDoesNotSeek;
    procedure ExplicitSynchronizationAlignsCursor;
    procedure UnsupportedSourceCapabilitiesPropagate;
    procedure ForwardReadThenWriteFailsBeforeMutation;
    procedure ConsumedForwardBufferCanWrite;
    procedure ForwardDirectWriteNeedsNoSeek;
    procedure FailedForwardSynchronizationRetainsBuffer;
    procedure FailedExplicitSynchronizationRetainsBuffer;
    procedure ForwardResizeFailsBeforeMutation;
    procedure PartialReadFailureInvalidatesCursor;
    procedure PartialWriteFailureInvalidatesCursor;
    procedure PartialSeekFailureInvalidatesCursor;
    procedure NegativePositionIsNotAdjustedOrCached;
    procedure NegativeSeekAndSizeRetainUnreadData;
    procedure NegativeSynchronizationPreventsMutation;
    procedure NegativeReadResultInvalidatesCursor;
    procedure NegativeWriteResultInvalidatesCursor;
    procedure PositiveShortReadMakesOneSourceAttempt;
    procedure UncachedSizeQueriesRemainDynamic;
    procedure CachedSizeQueriesSourceOnce;
    procedure CachedZeroLengthQueriesSourceOnce;
    procedure CachedConstructionDoesNotProbe;
    procedure CachedSizeExceptionIsRetried;
    procedure CachedNegativeSizeIsRetried;
    procedure CachedSizeAfterOverwriteAndAppend;
    procedure CachedSizeAfterShortWrite;
    procedure CachedSizeAfterNegativePartialWrite;
    procedure CachedSizeAfterThrowingPartialWrite;
    procedure CachedSizeAfterTruncateAndExpand;
    procedure CachedSizeAfterFailedAndIgnoredResize;
    procedure CachedSizeAfterFailedSynchronization;
    procedure CachedSparseLengthAndZeroWrite;
    procedure DefaultBufferingEnabled;
    procedure DisabledConstructorDirectReads;
    procedure DisabledShortReadsAndEOF;
    procedure DisabledForwardOnlyReads;
    procedure DisableUnreadRejectedWithoutIO;
    procedure FlushUnreadDataAllowsDisabling;
    procedure EmptyAndRepeatedTogglesDoNoIO;
    procedure ReenablePreservesDirectCursor;
    procedure DisabledSparseSeekOrigins;
    procedure DisabledWriteAndResize;
    procedure DisabledReadExceptionIdentity;
    procedure DisabledNegativePartialRead;
    procedure FailedFlushPreservesUnread;
    procedure ForwardOnlyFlushPreservesUnread;
    procedure NegativeFlushPreservesUnread;
    procedure DisabledReadBufferingCachesLength;
    procedure ReenableInvalidatesSizeAndPosition;
    procedure ToggledOwnership;
    procedure AlternatingModesMatchMemoryStream;
    procedure DisabledArgumentValidation;
    procedure LengthCachingTogglesWithUnreadBytes;
    procedure ConvenienceModesAreAtomic;
    procedure DirectReadsUncachedLength;
    procedure DirectReadsCachedLength;
    procedure BufferedReadsUncachedLength;
    procedure BufferedReadsCachedLength;
    procedure DirectCachedLengthAfterOverwriteAndAppend;
    procedure DirectCachedLengthAfterShortWrite;
    procedure DirectCachedLengthAfterNegativePartialWrite;
    procedure DirectCachedLengthAfterThrowingPartialWrite;
    procedure DirectCachedLengthAfterTruncateAndExpand;
    procedure DirectCachedLengthAfterFailedAndIgnoredResize;
    procedure DirectCachedNegativeSizeIsRetried;
    procedure DirectCachedSizeExceptionIsRetried;
    procedure DirectCachedSparseLengthAndZeroWrite;
    procedure LengthCachingTogglesWithoutReadBuffer;
    procedure DirectCachedLengthSurvivesRejectedMutations;
  end;

implementation
uses UStreams.Checks;

procedure TBufferedStreamTests.NilSourceRejected;
begin
  RunBufferedStreamCheck(1);
end;

procedure TBufferedStreamTests.NonpositiveBufferRejectedWithoutOwnership;
begin
  RunBufferedStreamCheck(2);
end;

procedure TBufferedStreamTests.BorrowedSourceRemainsAlive;
begin
  RunBufferedStreamCheck(3);
end;

procedure TBufferedStreamTests.OwnedSourceDisposedOnce;
begin
  RunBufferedStreamCheck(4);
end;

procedure TBufferedStreamTests.NonzeroInitialPosition;
begin
  RunBufferedStreamCheck(5);
end;

procedure TBufferedStreamTests.LogicalAndPhysicalCursors;
begin
  RunBufferedStreamCheck(6);
end;

procedure TBufferedStreamTests.SingleByteReadsCollapseSourceCalls;
begin
  RunBufferedStreamCheck(7);
end;

procedure TBufferedStreamTests.AllBufferBoundariesAndEOF;
begin
  RunBufferedStreamCheck(8);
end;

procedure TBufferedStreamTests.EmptyStreamEOF;
begin
  RunBufferedStreamCheck(9);
end;

procedure TBufferedStreamTests.ZeroCountsPerformNoIO;
begin
  RunBufferedStreamCheck(10);
end;

procedure TBufferedStreamTests.NegativeCountsRejectedBeforeIO;
begin
  RunBufferedStreamCheck(11);
end;

procedure TBufferedStreamTests.ReadBufferCrossesBoundaries;
begin
  RunBufferedStreamCheck(12);
end;

procedure TBufferedStreamTests.ShortReadsDoNotLoseData;
begin
  RunBufferedStreamCheck(13);
end;

procedure TBufferedStreamTests.AllSeekOrigins;
begin
  RunBufferedStreamCheck(14);
end;

procedure TBufferedStreamTests.SeekWithinRetainedWindow;
begin
  RunBufferedStreamCheck(15);
end;

procedure TBufferedStreamTests.SeekOutsideWindowAndBack;
begin
  RunBufferedStreamCheck(16);
end;

procedure TBufferedStreamTests.SeekBeyondEOFDoesNotResize;
begin
  RunBufferedStreamCheck(17);
end;

procedure TBufferedStreamTests.WriteAfterReadUsesLogicalCursor;
begin
  RunBufferedStreamCheck(18);
end;

procedure TBufferedStreamTests.ReadAfterWriteRejectsStaleBuffer;
begin
  RunBufferedStreamCheck(19);
end;

procedure TBufferedStreamTests.ShortWriteReturnsActualCount;
begin
  RunBufferedStreamCheck(20);
end;

procedure TBufferedStreamTests.SourceReadFailurePreserved;
begin
  RunBufferedStreamCheck(21);
end;

procedure TBufferedStreamTests.SourceWriteFailurePreserved;
begin
  RunBufferedStreamCheck(22);
end;

procedure TBufferedStreamTests.TruncatePreservesCursorSemantics;
begin
  RunBufferedStreamCheck(23);
end;

procedure TBufferedStreamTests.ExpandThenWritePreservesData;
begin
  RunBufferedStreamCheck(24);
end;

procedure TBufferedStreamTests.ResizeFailurePreserved;
begin
  RunBufferedStreamCheck(25);
end;

procedure TBufferedStreamTests.SizeFailurePreserved;
begin
  RunBufferedStreamCheck(26);
end;

procedure TBufferedStreamTests.ReadOnlySourceStillReadable;
begin
  RunBufferedStreamCheck(27);
end;

procedure TBufferedStreamTests.Sparse64BitOffsetsAndBoundedReads;
begin
  RunBufferedStreamCheck(28);
end;

procedure TBufferedStreamTests.DeterministicDifferentialOperations;
begin
  RunBufferedStreamCheck(29);
end;

procedure TBufferedStreamTests.OwnedSourceDisposedAfterReadError;
begin
  RunBufferedStreamCheck(30);
end;

procedure TBufferedStreamTests.WriteThroughDoesNotRead;
begin
  RunBufferedStreamCheck(31);
end;

procedure TBufferedStreamTests.ForwardOnlyReadsPreserveCapabilities;
begin
  RunBufferedStreamCheck(32);
end;

procedure TBufferedStreamTests.BorrowedDestroyDoesNotSeek;
begin
  RunBufferedStreamCheck(33);
end;

procedure TBufferedStreamTests.ExplicitSynchronizationAlignsCursor;
begin
  RunBufferedStreamCheck(34);
end;

procedure TBufferedStreamTests.UnsupportedSourceCapabilitiesPropagate;
begin
  RunBufferedStreamCheck(35);
end;

procedure TBufferedStreamTests.ForwardReadThenWriteFailsBeforeMutation;
begin
  RunBufferedStreamCheck(36);
end;

procedure TBufferedStreamTests.ConsumedForwardBufferCanWrite;
begin
  RunBufferedStreamCheck(37);
end;

procedure TBufferedStreamTests.ForwardDirectWriteNeedsNoSeek;
begin
  RunBufferedStreamCheck(38);
end;

procedure TBufferedStreamTests.FailedForwardSynchronizationRetainsBuffer;
begin
  RunBufferedStreamCheck(39);
end;

procedure TBufferedStreamTests.FailedExplicitSynchronizationRetainsBuffer;
begin
  RunBufferedStreamCheck(40);
end;

procedure TBufferedStreamTests.ForwardResizeFailsBeforeMutation;
begin
  RunBufferedStreamCheck(41);
end;

procedure TBufferedStreamTests.PartialReadFailureInvalidatesCursor;
begin
  RunBufferedStreamCheck(42);
end;

procedure TBufferedStreamTests.PartialWriteFailureInvalidatesCursor;
begin
  RunBufferedStreamCheck(43);
end;

procedure TBufferedStreamTests.PartialSeekFailureInvalidatesCursor;
begin
  RunBufferedStreamCheck(44);
end;

procedure TBufferedStreamTests.NegativePositionIsNotAdjustedOrCached;
begin
  RunBufferedStreamCheck(45);
end;

procedure TBufferedStreamTests.NegativeSeekAndSizeRetainUnreadData;
begin
  RunBufferedStreamCheck(46);
end;

procedure TBufferedStreamTests.NegativeSynchronizationPreventsMutation;
begin
  RunBufferedStreamCheck(47);
end;

procedure TBufferedStreamTests.NegativeReadResultInvalidatesCursor;
begin
  RunBufferedStreamCheck(48);
end;

procedure TBufferedStreamTests.NegativeWriteResultInvalidatesCursor;
begin
  RunBufferedStreamCheck(49);
end;

procedure TBufferedStreamTests.PositiveShortReadMakesOneSourceAttempt;
begin
  RunBufferedStreamCheck(50);
end;

procedure TBufferedStreamTests.UncachedSizeQueriesRemainDynamic;
begin
  RunBufferedStreamCheck(51);
end;

procedure TBufferedStreamTests.CachedSizeQueriesSourceOnce;
begin
  RunBufferedStreamCheck(52);
end;

procedure TBufferedStreamTests.CachedZeroLengthQueriesSourceOnce;
begin
  RunBufferedStreamCheck(53);
end;

procedure TBufferedStreamTests.CachedConstructionDoesNotProbe;
begin
  RunBufferedStreamCheck(54);
end;

procedure TBufferedStreamTests.CachedSizeExceptionIsRetried;
begin
  RunBufferedStreamCheck(55);
end;

procedure TBufferedStreamTests.CachedNegativeSizeIsRetried;
begin
  RunBufferedStreamCheck(56);
end;

procedure TBufferedStreamTests.CachedSizeAfterOverwriteAndAppend;
begin
  RunBufferedStreamCheck(57);
end;

procedure TBufferedStreamTests.CachedSizeAfterShortWrite;
begin
  RunBufferedStreamCheck(58);
end;

procedure TBufferedStreamTests.CachedSizeAfterNegativePartialWrite;
begin
  RunBufferedStreamCheck(59);
end;

procedure TBufferedStreamTests.CachedSizeAfterThrowingPartialWrite;
begin
  RunBufferedStreamCheck(60);
end;

procedure TBufferedStreamTests.CachedSizeAfterTruncateAndExpand;
begin
  RunBufferedStreamCheck(61);
end;

procedure TBufferedStreamTests.CachedSizeAfterFailedAndIgnoredResize;
begin
  RunBufferedStreamCheck(62);
end;

procedure TBufferedStreamTests.CachedSizeAfterFailedSynchronization;
begin
  RunBufferedStreamCheck(63);
end;

procedure TBufferedStreamTests.CachedSparseLengthAndZeroWrite;
begin
  RunBufferedStreamCheck(64);
end;

procedure TBufferedStreamTests.DefaultBufferingEnabled;
begin
  RunBufferedStreamCheck(65);
end;

procedure TBufferedStreamTests.DisabledConstructorDirectReads;
begin
  RunBufferedStreamCheck(66);
end;

procedure TBufferedStreamTests.DisabledShortReadsAndEOF;
begin
  RunBufferedStreamCheck(67);
end;

procedure TBufferedStreamTests.DisabledForwardOnlyReads;
begin
  RunBufferedStreamCheck(68);
end;

procedure TBufferedStreamTests.DisableUnreadRejectedWithoutIO;
begin
  RunBufferedStreamCheck(69);
end;

procedure TBufferedStreamTests.FlushUnreadDataAllowsDisabling;
begin
  RunBufferedStreamCheck(70);
end;

procedure TBufferedStreamTests.EmptyAndRepeatedTogglesDoNoIO;
begin
  RunBufferedStreamCheck(71);
end;

procedure TBufferedStreamTests.ReenablePreservesDirectCursor;
begin
  RunBufferedStreamCheck(72);
end;

procedure TBufferedStreamTests.DisabledSparseSeekOrigins;
begin
  RunBufferedStreamCheck(73);
end;

procedure TBufferedStreamTests.DisabledWriteAndResize;
begin
  RunBufferedStreamCheck(74);
end;

procedure TBufferedStreamTests.DisabledReadExceptionIdentity;
begin
  RunBufferedStreamCheck(75);
end;

procedure TBufferedStreamTests.DisabledNegativePartialRead;
begin
  RunBufferedStreamCheck(76);
end;

procedure TBufferedStreamTests.FailedFlushPreservesUnread;
begin
  RunBufferedStreamCheck(77);
end;

procedure TBufferedStreamTests.ForwardOnlyFlushPreservesUnread;
begin
  RunBufferedStreamCheck(78);
end;

procedure TBufferedStreamTests.NegativeFlushPreservesUnread;
begin
  RunBufferedStreamCheck(79);
end;

procedure TBufferedStreamTests.DisabledReadBufferingCachesLength;
begin
  RunBufferedStreamCheck(80);
end;

procedure TBufferedStreamTests.ReenableInvalidatesSizeAndPosition;
begin
  RunBufferedStreamCheck(81);
end;

procedure TBufferedStreamTests.ToggledOwnership;
begin
  RunBufferedStreamCheck(82);
end;

procedure TBufferedStreamTests.AlternatingModesMatchMemoryStream;
begin
  RunBufferedStreamCheck(83);
end;

procedure TBufferedStreamTests.DisabledArgumentValidation;
begin
  RunBufferedStreamCheck(84);
end;

procedure TBufferedStreamTests.LengthCachingTogglesWithUnreadBytes;
begin
  RunBufferedStreamCheck(85);
end;

procedure TBufferedStreamTests.ConvenienceModesAreAtomic;
begin
  RunBufferedStreamCheck(86);
end;

procedure TBufferedStreamTests.DirectReadsUncachedLength;
begin
  RunBufferedStreamCheck(87);
end;

procedure TBufferedStreamTests.DirectReadsCachedLength;
begin
  RunBufferedStreamCheck(88);
end;

procedure TBufferedStreamTests.BufferedReadsUncachedLength;
begin
  RunBufferedStreamCheck(89);
end;

procedure TBufferedStreamTests.BufferedReadsCachedLength;
begin
  RunBufferedStreamCheck(90);
end;

procedure TBufferedStreamTests.DirectCachedLengthAfterOverwriteAndAppend;
begin
  RunBufferedStreamCheck(91);
end;

procedure TBufferedStreamTests.DirectCachedLengthAfterShortWrite;
begin
  RunBufferedStreamCheck(92);
end;

procedure TBufferedStreamTests.DirectCachedLengthAfterNegativePartialWrite;
begin
  RunBufferedStreamCheck(93);
end;

procedure TBufferedStreamTests.DirectCachedLengthAfterThrowingPartialWrite;
begin
  RunBufferedStreamCheck(94);
end;

procedure TBufferedStreamTests.DirectCachedLengthAfterTruncateAndExpand;
begin
  RunBufferedStreamCheck(95);
end;

procedure TBufferedStreamTests.DirectCachedLengthAfterFailedAndIgnoredResize;
begin
  RunBufferedStreamCheck(96);
end;

procedure TBufferedStreamTests.DirectCachedNegativeSizeIsRetried;
begin
  RunBufferedStreamCheck(97);
end;

procedure TBufferedStreamTests.DirectCachedSizeExceptionIsRetried;
begin
  RunBufferedStreamCheck(98);
end;

procedure TBufferedStreamTests.DirectCachedSparseLengthAndZeroWrite;
begin
  RunBufferedStreamCheck(99);
end;

procedure TBufferedStreamTests.LengthCachingTogglesWithoutReadBuffer;
begin
  RunBufferedStreamCheck(100);
end;

procedure TBufferedStreamTests.DirectCachedLengthSurvivesRejectedMutations;
begin
  RunBufferedStreamCheck(101);
end;

initialization
  RegisterTest(TBufferedStreamTests);
end.
