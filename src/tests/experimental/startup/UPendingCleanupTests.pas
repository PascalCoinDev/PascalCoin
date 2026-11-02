unit UPendingCleanupTests;

{ Copyright (c) 2026 by Herman Schoenfeld
  Distributed under the MIT software license, see the accompanying LICENSE.
  Generated operations, in-memory lists and disposable files only. }

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, UBaseTypes, UPCOrderedLists,
  UNode, UBlockChain, UCrypto, UAccounts;

type
  TOrderedRawListCleanupTests = class(TTestCase)
  private
    FList : TOrderedRawList;
    function Raw(AValue : Integer) : TRawBytes;
    procedure Fill(ACount : Integer);
    procedure AssertOriginal;
    procedure Reject(const AIndexes : array of Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EmptyRequestLeavesValuesAndTags;
    procedure DeleteAllReleasesOwnedEntries;
    procedure InterleavedBatchesRetainValuesAndTags;
    procedure InvalidRequestsAreAtomic;
    procedure EndRangesRetainSearchableMiddle;
    procedure LargeBatchPreservesUnrelatedReservations;
  end;

  TPendingFinalizationTests = class(TTestCase)
  private
    FDirectory, FPreviousDirectory : String;
    FKey : TECPrivateKey;
    FTree, FNestedTree : TOperationsHashTree;
    FNode : TNode;
    FCancelStage : TProgressStage;
    FObservedStages : set of TProgressStage;
    FExpectedProgressThread : TThreadID;
    FReservedTotal, FNestedCount : Integer;
    FRunNested : Boolean;
    procedure OnProgress(Sender : TObject; const AProgress : TProgressInfo);
    procedure ExpectCancellation(ATree : TOperationsHashTree);
    procedure CreateNode;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CanceledBatchCanReserveAgain;
    procedure NestedCanceledBatchesPreserveOuterReservations;
    procedure FinalizingCancellationStillReleasesReservations;
    procedure ActualNodeReportsTypedStagesSynchronously;
    procedure BufferedSavePreservesExactBytesAndTruncatesOldTail;
    procedure BufferedSaveUsesBoundedFileWrites;
    procedure EmptyAndPopulatedProofDigestRetainsBytes;
    procedure ZeroAndNonzeroBigNumbersRetainByteEncoding;
  end;

implementation

uses
  Windows, UConst, UOpTransaction, UFileStorage, UPCDataTypes;

type
  TProcessIOCounts = record
    ReadOperationCount, WriteOperationCount, OtherOperationCount : UInt64;
    ReadTransferCount, WriteTransferCount, OtherTransferCount : UInt64;
  end;

function ReadProcessIOCounts(AProcess : THandle; out ACounts : TProcessIOCounts) : BOOL;
  stdcall; external 'kernel32.dll' name 'GetProcessIoCounters';

function TOrderedRawListCleanupTests.Raw(AValue : Integer) : TRawBytes;
begin
  SetLength(Result,4);
  Result[0] := Byte(AValue shr 24);
  Result[1] := Byte(AValue shr 16);
  Result[2] := Byte(AValue shr 8);
  Result[3] := Byte(AValue);
end;

procedure TOrderedRawListCleanupTests.Fill(ACount : Integer);
var i : Integer;
begin
  for i := 0 to ACount-1 do FList.Add(Raw(i),1000+i);
end;

procedure TOrderedRawListCleanupTests.SetUp;
begin
  inherited;
  FList := TOrderedRawList.Create;
end;

procedure TOrderedRawListCleanupTests.TearDown;
begin
  FList.Free;
  inherited;
end;

procedure TOrderedRawListCleanupTests.AssertOriginal;
var i : Integer;
begin
  AssertEquals(8,FList.Count);
  for i := 0 to 7 do begin
    AssertTrue(TBaseType.Equals(Raw(i),FList.Get(i)));
    AssertEquals(1000+i,FList.GetTag(i));
  end;
end;

procedure TOrderedRawListCleanupTests.Reject(const AIndexes : array of Integer);
var LRejected : Boolean;
begin
  LRejected := False;
  try
    FList.DeleteIndexes(AIndexes);
  except
    on E : EOrderedList do LRejected := True;
  end;
  AssertTrue('Invalid bulk requests must be rejected',LRejected);
  AssertOriginal;
end;

procedure TOrderedRawListCleanupTests.EmptyRequestLeavesValuesAndTags;
begin
  FList.DeleteIndexes([]);
  AssertEquals(0,FList.Count);
  Fill(8);
  FList.DeleteIndexes([]);
  AssertOriginal;
end;

procedure TOrderedRawListCleanupTests.DeleteAllReleasesOwnedEntries;
begin
  Fill(8);
  FList.DeleteIndexes([0,1,2,3,4,5,6,7]);
  AssertEquals(0,FList.Count);
  Fill(8);
  AssertOriginal;
end;

procedure TOrderedRawListCleanupTests.InterleavedBatchesRetainValuesAndTags;
var i : Integer;
begin
  Fill(8);
  FList.DeleteIndexes([0,2,4,6]);
  AssertEquals(4,FList.Count);
  for i := 0 to 3 do begin
    AssertTrue(TBaseType.Equals(Raw(i*2+1),FList.Get(i)));
    AssertEquals(1000+i*2+1,FList.GetTag(i));
    AssertEquals(i,FList.IndexOf(Raw(i*2+1)));
    AssertEquals(-1,FList.IndexOf(Raw(i*2)));
  end;
end;

procedure TOrderedRawListCleanupTests.InvalidRequestsAreAtomic;
begin
  Fill(8);
  Reject([-1]);
  Reject([0,8]);
  Reject([1,1]);
  Reject([4,2]);
end;

procedure TOrderedRawListCleanupTests.EndRangesRetainSearchableMiddle;
begin
  Fill(8);
  FList.DeleteIndexes([0,1,6,7]);
  AssertEquals(4,FList.Count);
  AssertTrue(TBaseType.Equals(Raw(2),FList.Get(0)));
  AssertTrue(TBaseType.Equals(Raw(5),FList.Get(3)));
  FList.Add(Raw(0),-1);
  FList.Remove(Raw(3));
  AssertEquals(4,FList.Count);
  AssertEquals(-1,FList.GetTag(0));
  AssertEquals(-1,FList.IndexOf(Raw(3)));
end;

procedure TOrderedRawListCleanupTests.LargeBatchPreservesUnrelatedReservations;
const CT_COUNT = 185831;
var LIndexes : array of Integer;
  i : Integer;
begin
  Fill(CT_COUNT*2);
  SetLength(LIndexes,CT_COUNT);
  for i := 0 to High(LIndexes) do LIndexes[i] := i*2;
  FList.DeleteIndexes(LIndexes);
  AssertEquals(CT_COUNT,FList.Count);
  for i := 0 to FList.Count-1 do begin
    AssertTrue(TBaseType.Equals(Raw(i*2+1),FList.Get(i)));
    AssertEquals(1000+i*2+1,FList.GetTag(i));
  end;
end;

procedure TPendingFinalizationTests.SetUp;
var LID : TGUID;
  LOperation : TPCOperation;
  i : Integer;
begin
  inherited;
  CreateGUID(LID);
  FDirectory := IncludeTrailingPathDelimiter(GetTempDir)+'PendingCleanup-'+GUIDToString(LID);
  ForceDirectories(FDirectory);
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(FDirectory);
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FTree := TOperationsHashTree.Create;
  FNestedTree := TOperationsHashTree.Create;
  for i := 1 to 3 do begin
    LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_6,100,i,101,FKey,100,1,CT_TOperationPayload_NUL);
    try
      AssertTrue(FTree.AddOperationToHashTree(LOperation));
      if i>=2 then AssertTrue(FNestedTree.AddOperationToHashTree(LOperation));
    finally
      LOperation.Free;
    end;
  end;
  FNode := nil;
  FCancelStage := psValidatingSignatures;
  FObservedStages := [];
  FExpectedProgressThread := GetCurrentThreadID;
  FReservedTotal := -1;
  FNestedCount := 0;
  FRunNested := False;
end;

procedure TPendingFinalizationTests.TearDown;
begin
  FNode.Free;
  FNestedTree.Free;
  FTree.Free;
  FKey.Free;
  TNode.SetPascalCoinDataFolder(FPreviousDirectory);
  SysUtils.DeleteFile(FDirectory+PathDelim+'Data'+PathDelim+'pendingbuffer.ops');
  RemoveDir(FDirectory+PathDelim+'Data');
  RemoveDir(FDirectory);
  inherited;
end;

procedure TPendingFinalizationTests.CreateNode;
begin
  AssertFalse('Tests must not reuse a global node',TNode.NodeExists);
  FNode := TNode.Create(nil);
  FNode.BroadcastData := False;
end;

procedure TPendingFinalizationTests.ExpectCancellation(ATree : TOperationsHashTree);
var LError : String;
  LCanceled : Boolean;
begin
  LCanceled := False;
  try
    FNode.AddOperations(nil,ATree,nil,LError,OnProgress);
  except
    on E : EAbort do LCanceled := True;
  end;
  AssertTrue('The requested progress cancellation must propagate',LCanceled);
end;

procedure TPendingFinalizationTests.OnProgress(Sender : TObject; const AProgress : TProgressInfo);
begin
  AssertTrue('Actual node owns its progress callback',Sender = FNode);
  AssertEquals('Core callback and cancellation remain on the invoking thread',
    QWord(FExpectedProgressThread),QWord(GetCurrentThreadID));
  Include(FObservedStages,AProgress.Stage);
  AssertTrue('The raw operation counts remain bounded',
    (AProgress.CurrentPosition >= 0) and
    (AProgress.CurrentPosition <= AProgress.TotalCount));
  if (AProgress.Stage=FCancelStage) and (AProgress.CurrentPosition=0) then begin
    FReservedTotal := AProgress.TotalCount;
    if FRunNested then begin
      FRunNested := False;
      // The outer batch owns hashes1..3. Both overlapping nested batches must
      // reserve nothing, and their cleanup must leave the outer owner intact.
      ExpectCancellation(FNestedTree);
      AssertEquals(0,FReservedTotal);
      Inc(FNestedCount);
      ExpectCancellation(FNestedTree);
      AssertEquals(0,FReservedTotal);
      Inc(FNestedCount);
    end;
    Abort;
  end;
end;

procedure TPendingFinalizationTests.CanceledBatchCanReserveAgain;
begin
  CreateNode;
  ExpectCancellation(FTree);
  AssertEquals(3,FReservedTotal);
  ExpectCancellation(FTree);
  AssertEquals('Canceled reservations must not suppress the retry',3,FReservedTotal);
end;

procedure TPendingFinalizationTests.NestedCanceledBatchesPreserveOuterReservations;
begin
  CreateNode;
  FRunNested := True;
  ExpectCancellation(FTree);
  AssertEquals(2,FNestedCount);
  ExpectCancellation(FTree);
  AssertEquals('The outer batch must release its own reservations',3,FReservedTotal);
end;

procedure TPendingFinalizationTests.FinalizingCancellationStillReleasesReservations;
begin
  CreateNode;
  // Invalid senders in an empty safebox reach finalization without accepting
  // an operation; no persistent wallet, chain or broadcast is involved.
  FCancelStage := psFinalizingOperations;
  ExpectCancellation(FTree);
  AssertEquals(3,FReservedTotal);
  FCancelStage := psValidatingSignatures;
  ExpectCancellation(FTree);
  AssertEquals('Finalization cancellation must still release reservations',3,FReservedTotal);
  AssertFalse(FileExists(FDirectory+PathDelim+'Data'+PathDelim+'pendingbuffer.ops'));
end;

procedure TPendingFinalizationTests.ActualNodeReportsTypedStagesSynchronously;
var LError : String;
begin
  CreateNode;
  FCancelStage := psNone;
  // These generated senders are absent from the empty safebox. The real core
  // still prepares, validates, applies and finalizes without accepting them.
  AssertEquals(0,FNode.AddOperations(nil,FTree,nil,LError,OnProgress));
  AssertTrue('Preparation is a typed stage',psPreparingOperations in FObservedStages);
  AssertTrue('Signature validation is a typed stage',psValidatingSignatures in FObservedStages);
  AssertTrue('Application is a typed stage',psApplyingOperations in FObservedStages);
  AssertTrue('Finalization is a typed stage',psFinalizingOperations in FObservedStages);
end;

procedure TPendingFinalizationTests.BufferedSavePreservesExactBytesAndTruncatesOldTail;
var LBank : TPCBank;
  LStorage : TFileStorage;
  LExpected, LActual : TMemoryStream;
  LFileName : String;
begin
  LBank := TPCBank.Create(nil);
  LStorage := TFileStorage.Create(nil);
  LExpected := TMemoryStream.Create;
  LActual := TMemoryStream.Create;
  try
    LStorage.Bank := LBank;
    AssertTrue(FTree.SaveOperationsHashTreeToStream(LExpected,True));
    LStorage.SavePendingBufferOperations(FTree);
    FreeAndNil(LStorage); // Release the exclusive file before inspecting bytes.
    LFileName := FDirectory+PathDelim+'Data'+PathDelim+'pendingbuffer.ops';
    LActual.LoadFromFile(LFileName);
    AssertEquals(LExpected.Size,LActual.Size);
    AssertTrue(CompareMem(LExpected.Memory,LActual.Memory,LExpected.Size));
    FTree.ClearHastThree;
    LExpected.Clear;
    AssertTrue(FTree.SaveOperationsHashTreeToStream(LExpected,True));
    LStorage := TFileStorage.Create(nil);
    LStorage.Bank := LBank;
    LStorage.SavePendingBufferOperations(FTree);
    FreeAndNil(LStorage);
    LActual.LoadFromFile(LFileName);
    AssertEquals('A shorter package must truncate the old tail',LExpected.Size,LActual.Size);
    AssertTrue(CompareMem(LExpected.Memory,LActual.Memory,LExpected.Size));
  finally
    LActual.Free;
    LExpected.Free;
    LStorage.Free;
    LBank.Free;
  end;
end;

procedure TPendingFinalizationTests.BufferedSaveUsesBoundedFileWrites;
var LBank : TPCBank;
  LStorage : TFileStorage;
  LBefore, LAfter : TProcessIOCounts;
begin
  LBank := TPCBank.Create(nil);
  LStorage := TFileStorage.Create(nil);
  try
    LStorage.Bank := LBank;
    AssertTrue(ReadProcessIOCounts(GetCurrentProcess,LBefore));
    LStorage.SavePendingBufferOperations(FTree);
    AssertTrue(ReadProcessIOCounts(GetCurrentProcess,LAfter));
    AssertTrue('Saving a small package must use bulk file writes, not each serialized field',
      LAfter.WriteOperationCount-LBefore.WriteOperationCount<8);
  finally
    LStorage.Free;
    LBank.Free;
  end;
end;

procedure TPendingFinalizationTests.EmptyAndPopulatedProofDigestRetainsBytes;
const
  CT_EXPECTED : array[1..2,0..1] of String = (
    ('44332211','01020304AABBCC44332211'),
    ('4433221100000000','01020304AABBCC44332211000000001020304050'));
var LBlock : TOperationBlock;
  LActual : TRawBytes;
  LProtocol, LPopulated : Integer;
begin
  for LProtocol := 1 to 2 do begin
    for LPopulated := 0 to 1 do begin
      LBlock := CT_OperationBlock_NUL;
      if LProtocol=1 then LBlock.protocol_version := CT_PROTOCOL_1
      else LBlock.protocol_version := CT_PROTOCOL_6;
      LBlock.fee := $11223344;
      if LPopulated=1 then begin
        LBlock.initial_safe_box_hash := TCrypto.HexaToRaw('01020304');
        LBlock.operations_hash := TCrypto.HexaToRaw('AABBCC');
        LBlock.previous_proof_of_work := TCrypto.HexaToRaw('1020304050');
      end;
      TPascalCoinProtocol.CalcProofOfWork_Part3(LBlock,LActual);
      AssertTrue('The existing proof digest input bytes must remain identical',
        TBaseType.Equals(TCrypto.HexaToRaw(CT_EXPECTED[LProtocol,LPopulated]),LActual));
    end;
  end;
end;

procedure TPendingFinalizationTests.ZeroAndNonzeroBigNumbersRetainByteEncoding;
var LNumber : TBigNum;
begin
  LNumber := TBigNum.Create(0);
  try
    AssertEquals('Zero retains the existing empty unsigned encoding',0,Length(LNumber.RawValue));
    LNumber.Value := $01020304;
    AssertTrue('Nonzero BIGNUM byte order must remain unchanged',
      TBaseType.Equals(TCrypto.HexaToRaw('01020304'),LNumber.RawValue));
  finally
    LNumber.Free;
  end;
end;

initialization
  RegisterTest(TOrderedRawListCleanupTests);
  RegisterTest(TPendingFinalizationTests);
end.
