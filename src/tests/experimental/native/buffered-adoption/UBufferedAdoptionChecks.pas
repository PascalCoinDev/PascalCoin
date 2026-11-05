unit UBufferedAdoptionChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Small actual-core checks for buffered pending, history and table input.
  The header-boundary records test storage serialization, not consensus admission. }

{$IFDEF FPC}{$MODE Delphi}{$H+}{$ENDIF}
{$I ../../../../config.inc}

interface

const BufferedAdoptionCheckCount = 11;
procedure CheckBufferedAdoption(AIndex: Integer);
function RunBufferedAdoptionChecks(const ALogPath: String): Integer;
function RunBufferedAdoptionPerformance(const ALogPath: String): Integer;

implementation

uses Classes, SysUtils, {$IFDEF MSWINDOWS}Windows,{$ENDIF} UCrypto, UAccounts, UBlockChain, UFileStorage,
  UNode, UOpTransaction, UConst, UPCDataTypes, UBaseTypes, UPCHardcodedRandomHashTable;

type
  TStoredBlocks = array of TBytes;

  TAdoptionSafeBox = class(TPCSafeBox)
  public
    procedure UseStorageProtocol(AProtocol: Word);
  end;

  TBufferedAdoptionFixture = class
  private
    FDirectory, FPreviousDirectory: String;
    FChangedDirectory: Boolean;
    FKey: TECPrivateKey;
    FCancelPending: Boolean;
    procedure PendingProgress(Sender: TObject; const AProgress: TProgressInfo);
    procedure FillPending(ATree: TOperationsHashTree; AProtocol: Word; ACount: Integer);
    procedure SeedPendingBank(ABank: TPCBank; AProtocol: Word);
    procedure Pending(AProtocol: Word; ACancel: Boolean);
    procedure PopulateHistory(AStorage: TFileStorage; out AExpected: TStoredBlocks);
    procedure History;
    procedure HeaderBoundary;
    procedure MalformedHistory;
    procedure RandomTable(AKind: Integer);
    {$IFDEF MSWINDOWS}
    procedure Performance(ALines: TStrings);
    procedure PendingPerformance(ALines: TStrings);
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create('Buffered adoption: '+AMessage);
end;

function AdoptionCheckName(AIndex: Integer): String;
const Names: array[1..BufferedAdoptionCheckCount] of String = (
  'PendingProtocol5RoundTripAndReplacement','PendingProtocol5CancellationAndRetry',
  'HistoryReadSaveDeleteAndReopen','HistoryHeaderGroupBoundary',
  'RandomHashCanonicalTable','RandomHashInvalidMagic','RandomHashTruncatedSeal','RandomHashMismatchedSeal',
  'MalformedHistoryReadOnlyPreservesBytes','PendingProtocol6RoundTripAndReplacement','PendingProtocol6CancellationAndRetry');
begin
  Result := Names[AIndex];
end;

procedure TAdoptionSafeBox.UseStorageProtocol(AProtocol: Word);
begin
  // This fixture exercises the current saved-operation format, without replaying
  // years of blocks merely to reach its protocol. It does not admit a new block.
  Require(AProtocol in [CT_PROTOCOL_5,CT_PROTOCOL_6],'unsupported fixture protocol');
  Require(DoUpgradeToProtocol5,'cannot select protocol5 storage');
  if AProtocol=CT_PROTOCOL_6 then Require(DoUpgradeToProtocol6,'cannot select protocol6 storage');
end;

function FileBytes(const APath: String): TBytes;
var LFile: TFileStream; LSize: Int64;
begin
  LFile := TFileStream.Create(APath,fmOpenRead or fmShareDenyNone);
  try
    LSize := LFile.Size;
    Require((LSize>=0) and (LSize<=4*1024*1024),'fixture exceeded its 4 MiB bound');
    SetLength(Result,Integer(LSize));
    if LSize>0 then LFile.ReadBuffer(Result[0],Longint(LSize));
  finally
    LFile.Free;
  end;
end;

procedure WriteBytes(const APath: String; const ABytes: TBytes);
var LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath,fmCreate);
  try
    if Length(ABytes)>0 then LFile.WriteBuffer(ABytes[0],Longint(Length(ABytes)));
  finally
    LFile.Free;
  end;
end;

function SameBytes(const ALeft, ARight: TBytes): Boolean;
begin
  Result := Length(ALeft)=Length(ARight);
  if Result and (Length(ALeft)>0) then Result := CompareMem(@ALeft[0],@ARight[0],Length(ALeft));
end;

function StoredBytes(AOperations: TPCOperationsComp): TBytes;
var LMemory: TMemoryStream;
begin
  LMemory := TMemoryStream.Create;
  try
    Require(AOperations.SaveBlockToStorage(LMemory),'cannot serialize expected stored block');
    SetLength(Result,Integer(LMemory.Size));
    if Length(Result)>0 then Move(LMemory.Memory^,Result[0],Length(Result));
  finally
    LMemory.Free;
  end;
end;

constructor TBufferedAdoptionFixture.Create;
var LId: TGUID;
begin
  inherited Create;
  Require(not TNode.NodeExists,'fixture requires an independent node directory');
  Require(SysUtils.GetEnvironmentVariable('TEMP')<>'','runner must supply its owned TEMP directory');
  Require(CreateGUID(LId)=0,'cannot create fixture identity');
  FDirectory := IncludeTrailingPathDelimiter(SysUtils.GetEnvironmentVariable('TEMP'))+'BufferedAdoption-'+GUIDToString(LId);
  Require(not DirectoryExists(FDirectory) and ForceDirectories(FDirectory),'cannot create fresh fixture directory');
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(FDirectory);
  FChangedDirectory := True;
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
end;

destructor TBufferedAdoptionFixture.Destroy;
begin
  FKey.Free;
  if FChangedDirectory then TNode.SetPascalCoinDataFolder(FPreviousDirectory);
  // Retain these small owned files beside the test logs for before/after review.
  inherited Destroy;
end;

procedure TBufferedAdoptionFixture.PendingProgress(Sender: TObject; const AProgress: TProgressInfo);
begin
  Require(AProgress.Stage=psReadingOperations,'pending stage changed');
  if FCancelPending then Abort;
end;

procedure TBufferedAdoptionFixture.FillPending(ATree: TOperationsHashTree; AProtocol: Word; ACount: Integer);
var I: Integer; LOperation: TPCOperation;
begin
  for I := 1 to ACount do begin
    LOperation := TOpTransaction.CreateTransaction(AProtocol,100,I,101,FKey,100,1,CT_TOperationPayload_NUL);
    try
      Require(ATree.AddOperationToHashTree(LOperation),'cannot create signed pending fixture');
    finally
      LOperation.Free;
    end;
  end;
end;

procedure TBufferedAdoptionFixture.Pending(AProtocol: Word; ACancel: Boolean);
var LBank: TPCBank; LStorage: TFileStorage; LOriginal, LLoaded, LRaw: TOperationsHashTree; LPath, LError: String; LBefore: TBytes; LFile: TFileStream; I: Integer; LRaised: Boolean;
begin
  LBank := TPCBank.Create(nil);
  LStorage := nil;
  LOriginal := TOperationsHashTree.Create;
  LLoaded := TOperationsHashTree.Create;
  LRaw := TOperationsHashTree.Create;
  try
    SeedPendingBank(LBank,AProtocol);
    LStorage := TFileStorage.Create(nil);
    LStorage.Bank := LBank;
    FillPending(LOriginal,LBank.SafeBox.CurrentProtocol,8);
    LStorage.SavePendingBufferOperations(LOriginal);
    LPath := IncludeTrailingPathDelimiter(TPCBank.GetStorageFolder(LBank.Orphan))+'pendingbuffer.ops';
    LBefore := FileBytes(LPath);
    Require(Length(LBefore)>0,'actual pending save produced no package');
    LFile := TFileStream.Create(LPath,fmOpenRead or fmShareDenyWrite);
    try
      Require(LRaw.LoadOperationsHashTreeFromStream(LFile,True,LBank.SafeBox.CurrentProtocol,LBank.SafeBox.CurrentProtocol,nil,LError),'raw pending fixture decode failed at protocol '+IntToStr(LBank.SafeBox.CurrentProtocol)+': '+LError);
      Require(LRaw.OperationsCount=8,'raw pending fixture decoded a different operation count');
    finally
      LFile.Free;
    end;
    if ACancel then begin
      FCancelPending := True;
      LRaised := False;
      try
        LStorage.LoadPendingBufferOperations(LLoaded,PendingProgress);
      except
        on E: EAbort do LRaised := True;
      end;
      Require(LRaised,'actual pending load did not propagate cancellation');
      FCancelPending := False;
    end;
    LStorage.LoadPendingBufferOperations(LLoaded,PendingProgress);
    Require(LLoaded.OperationsCount=LOriginal.OperationsCount,'actual pending restore lost operations: expected '+IntToStr(LOriginal.OperationsCount)+' actual '+IntToStr(LLoaded.OperationsCount));
    for I := 0 to LOriginal.OperationsCount-1 do
      Require(TBaseType.Equals(LOriginal.GetOperation(I).Sha256,LLoaded.GetOperation(I).Sha256),'pending operation digest changed');
    // The existing pending reader intentionally holds an exclusive file lease.
    // Close it before independently comparing the persisted package bytes.
    FreeAndNil(LStorage);
    Require(SameBytes(LBefore,FileBytes(LPath)),'pending cancellation/restore altered package bytes');
    LStorage := TFileStorage.Create(nil);
    LStorage.Bank := LBank;
    LLoaded.ClearHastThree;
    LStorage.LoadPendingBufferOperations(LLoaded);
    // Replacement retires the retained reader before the next atomic package.
    LOriginal.ClearHastThree;
    FillPending(LOriginal,LBank.SafeBox.CurrentProtocol,3);
    LStorage.SavePendingBufferOperations(LOriginal);
    LLoaded.ClearHastThree;
    LStorage.LoadPendingBufferOperations(LLoaded);
    Require(LLoaded.OperationsCount=3,'pending replacement reused stale buffered bytes');
    for I := 0 to 2 do
      Require(TBaseType.Equals(LOriginal.GetOperation(I).Sha256,LLoaded.GetOperation(I).Sha256),'replacement pending digest differs');
  finally
    LRaw.Free;
    LStorage.Free;
    LLoaded.Free;
    LOriginal.Free;
    LBank.Free;
  end;
end;

procedure TBufferedAdoptionFixture.SeedPendingBank(ABank: TPCBank; AProtocol: Word);
var LFile: TFileStream; LOperations: TPCOperationsComp; LCount: Cardinal; LError: String;
begin
  ABank.StorageClass := TFileStorage;
  LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'mainnet-blocks-0-4.bin',fmOpenRead or fmShareDenyWrite);
  LOperations := TPCOperationsComp.Create(ABank);
  try
    LFile.ReadBuffer(LCount,SizeOf(LCount));
    Require((LCount=5) and LOperations.LoadBlockFromStream(LFile,LError),'cannot decode the actual genesis fixture');
    Require(ABank.AddNewBlockChainBlock(LOperations,0,LError),'cannot seed pending bank: '+LError);
    TAdoptionSafeBox(ABank.SafeBox).UseStorageProtocol(AProtocol);
    Require(ABank.SafeBox.CurrentProtocol=AProtocol,'pending fixture did not reach the selected storage protocol');
  finally
    LOperations.Free;
    LFile.Free;
  end;
end;

procedure TBufferedAdoptionFixture.PopulateHistory(AStorage: TFileStorage; out AExpected: TStoredBlocks);
var LFixture: TFileStream; LOperations: TPCOperationsComp; LCount: Cardinal; LError: String; I: Integer;
begin
  LFixture := TFileStream.Create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'mainnet-blocks-0-4.bin',fmOpenRead or fmShareDenyWrite);
  LOperations := TPCOperationsComp.Create(nil);
  try
    LFixture.ReadBuffer(LCount,SizeOf(LCount));
    Require(LCount=5,'committed public block fixture count changed');
    SetLength(AExpected,5);
    for I := 0 to 4 do begin
      LError := '';
      Require(LOperations.LoadBlockFromStream(LFixture,LError),'public block decode failed: '+LError);
      Require(LOperations.OperationBlock.block=Cardinal(I),'public fixture order changed');
      AExpected[I] := StoredBytes(LOperations);
      Require(AStorage.SaveBlockChainBlock(LOperations),'actual file storage save failed');
    end;
  finally
    LOperations.Free;
    LFixture.Free;
  end;
end;

procedure TBufferedAdoptionFixture.History;
const Order: array[0..4] of Integer = (4,0,3,1,2);
var LStorage: TFileStorage; LOperations: TPCOperationsComp; LExpected: TStoredBlocks; LInitial: TBytes; LPath: String; I, LIndex: Integer;
begin
  LPath := IncludeTrailingPathDelimiter(FDirectory)+'history.blocks';
  LStorage := TFileStorage.Create(nil);
  LOperations := TPCOperationsComp.Create(nil);
  try
    LStorage.SetBlockChainFile(AnsiString(LPath));
    PopulateHistory(LStorage,LExpected);
    LStorage.SetBlockChainFile(AnsiString(LPath));
    Require(LStorage.Initialize,'history reopen initialization failed');
    LInitial := FileBytes(LPath);
    Require((LStorage.FirstBlock=0) and (LStorage.LastBlock=4),'history reopen lost its range');
    for I := Low(Order) to High(Order) do begin
      LIndex := Order[I];
      Require(LStorage.BlockExists(LIndex),'history index lost a stored block');
      Require(LStorage.LoadBlockChainBlock(LOperations,LIndex),'actual out-of-order history load failed');
      Require(SameBytes(LExpected[LIndex],StoredBytes(LOperations)),'history payload bytes changed');
    end;
    LStorage.SetBlockChainFile(AnsiString(LPath));
    Require(SameBytes(LInitial,FileBytes(LPath)),'history reads rewrote the file');
    LStorage.DeleteBlockChainBlocks(3);
    Require((LStorage.LastBlock=2) and not LStorage.BlockExists(3),'history delete retained removed index rows');
    for I := 0 to 2 do begin
      Require(LStorage.LoadBlockChainBlock(LOperations,I),'history delete lost retained blocks');
      Require(SameBytes(LExpected[I],StoredBytes(LOperations)),'history delete changed retained payload');
    end;
    // Recreate all rows through the same production writer and compare the file.
    LStorage.SetBlockChainFile(AnsiString(LPath));
    LStorage.EraseStorage;
    PopulateHistory(LStorage,LExpected);
    LStorage.SetBlockChainFile(AnsiString(LPath));
    Require(SameBytes(LInitial,FileBytes(LPath)),'history save/reopen changed canonical file bytes');
    LStorage.ReadOnly := True;
    Require(LStorage.Initialize,'read-only history initialization failed');
    Require(LStorage.LoadBlockChainBlock(LOperations,4),'read-only history load failed');
    Require(SameBytes(LExpected[4],StoredBytes(LOperations)),'read-only history payload differs');
  finally
    LOperations.Free;
    LStorage.Free;
  end;
end;

procedure TBufferedAdoptionFixture.HeaderBoundary;
var LStorage: TFileStorage; LOperations, LLoaded: TPCOperationsComp; LBlock: TOperationBlock; I: Integer; LPath: String;
begin
  LPath := IncludeTrailingPathDelimiter(FDirectory)+'header-boundary.blocks';
  LStorage := TFileStorage.Create(nil);
  LOperations := TPCOperationsComp.Create(nil);
  LLoaded := TPCOperationsComp.Create(nil);
  try
    LStorage.SetBlockChainFile(AnsiString(LPath));
    LBlock := TPCOperationsComp.GetFirstBlock;
    for I := 0 to 1001 do begin
      LBlock.block := I;
      // The existing testing setter changes only this generated record, not
      // production consensus checks; storage parsing is the subject here.
      LOperations.SetOperationBlock(LBlock);
      Require(LStorage.SaveBlockChainBlock(LOperations),'cannot build the second header group');
    end;
    LStorage.SetBlockChainFile(AnsiString(LPath));
    Require(LStorage.Initialize,'boundary reopen initialization failed');
    Require((LStorage.FirstBlock=0) and (LStorage.LastBlock=1001),'header-group reopen lost its range');
    Require(LStorage.LoadBlockChainBlock(LLoaded,1000),'second header group cannot be read');
    Require(LLoaded.OperationBlock.block=1000,'second header group points to the wrong record');
    Require(LStorage.LoadBlockChainBlock(LLoaded,999),'previous header group cannot be revisited');
    Require(LLoaded.OperationBlock.block=999,'previous header group points to the wrong record');
    LStorage.DeleteBlockChainBlocks(1000);
    LStorage.SetBlockChainFile(AnsiString(LPath));
    Require(LStorage.Initialize,'boundary delete/reopen initialization failed');
    Require((LStorage.LastBlock=999) and not LStorage.BlockExists(1000),'boundary delete/reopen retained removed rows');
  finally
    LLoaded.Free;
    LOperations.Free;
    LStorage.Free;
  end;
end;

procedure TBufferedAdoptionFixture.RandomTable(AKind: Integer);
var LTable, LRaw: TPCHardcodedRandomHashTable; LFile: TFileStream; LBytes: TBytes; LHash, LDigest, LExpected, LActual: TRawBytes; LPath: String; I, J: Integer; LRawAccepted, LPublicAccepted, LExpectedAccepted: Boolean;
begin
  LPath := IncludeTrailingPathDelimiter(FDirectory)+'random-table.bin';
  LTable := TPCHardcodedRandomHashTable.Create;
  LRaw := TPCHardcodedRandomHashTable.Create;
  try
    for I := 0 to 2 do begin
      SetLength(LHash,32);
      for J := 0 to 31 do LHash[J] := Byte(I*32+J);
      LDigest.FromString('buffered-adoption-'+IntToStr(I));
      LTable.AddRandomHash(LHash,LDigest,False);
    end;
    LExpected := LTable.GetHardcodedSha256;
    LFile := TFileStream.Create(LPath,fmCreate);
    try
      LTable.SaveToStream(LFile);
    finally
      LFile.Free;
    end;
    LBytes := FileBytes(LPath);
    case AKind of
      1: LBytes[2] := LBytes[2] xor $01;
      2: SetLength(LBytes,Length(LBytes)-8);
      3: LBytes[Length(LBytes)-1] := LBytes[Length(LBytes)-1] xor $01;
    end;
    WriteBytes(LPath,LBytes);
    LFile := TFileStream.Create(LPath,fmOpenRead or fmShareDenyWrite);
    try
      LRawAccepted := LRaw.LoadFromStream(LFile,LActual);
    finally
      LFile.Free;
    end;
    Require(LRawAccepted=(AKind=0),'raw canonical-table fixture has an unexpected outcome');
    LPublicAccepted := TPascalCoinProtocol.AllowUseHardcodedRandomHashTable(LPath,LExpected);
    {$IFDEF ASSUME_VALID_POW_OLD_PROTOCOLS}
    // The shipping configuration returns before opening the table file.
    LExpectedAccepted := False;
    {$ELSE}
    LExpectedAccepted := LRawAccepted;
    {$ENDIF}
    Require(LPublicAccepted=LExpectedAccepted,'actual RandomHash entry changed raw-loader acceptance');
    Require(SameBytes(LBytes,FileBytes(LPath)),'RandomHash load altered the input file');
    if AKind=0 then begin
      LExpected[0] := LExpected[0] xor $01;
      Require(not TPascalCoinProtocol.AllowUseHardcodedRandomHashTable(LPath,LExpected),'wrong caller-supplied table hash was accepted');
      Require(not TPascalCoinProtocol.AllowUseHardcodedRandomHashTable(LPath+'.missing',LExpected),'missing table was accepted');
    end;
  finally
    LRaw.Free;
    LTable.Free;
  end;
end;

procedure TBufferedAdoptionFixture.MalformedHistory;
var LPath: String; LStorage: TFileStorage; LExpected: TStoredBlocks; LBytes: TBytes; LRaised, LAccepted: Boolean; LLease: TFileStream; LOperations: TPCOperationsComp; I: Integer;
begin
  for I := 0 to 1 do begin
    LPath := IncludeTrailingPathDelimiter(FDirectory)+'malformed-'+IntToStr(I)+'.blocks';
    LStorage := TFileStorage.Create(nil);
    try
      LStorage.SetBlockChainFile(AnsiString(LPath));
      PopulateHistory(LStorage,LExpected);
    finally
      LStorage.Free;
    end;
    LBytes := FileBytes(LPath);
    if I=0 then SetLength(LBytes,63)
    else SetLength(LBytes,Length(LBytes)-8);
    WriteBytes(LPath,LBytes);
    LStorage := TFileStorage.Create(nil);
    try
      LStorage.SetBlockChainFile(AnsiString(LPath));
      LStorage.ReadOnly := True;
      LRaised := False;
      LAccepted := False;
      try
        LAccepted := LStorage.Initialize;
      except
        on E: Exception do LRaised := True;
      end;
      if I=0 then
        Require(LRaised or not LAccepted,'read-only malformed header was accepted')
      else if LAccepted then begin
        // Index initialization need not decode the final body; requesting that
        // truncated payload must retain the existing False result.
        LOperations := TPCOperationsComp.Create(nil);
        try
          Require(not LStorage.LoadBlockChainBlock(LOperations,4),'truncated history payload was accepted');
        finally
          LOperations.Free;
        end;
      end;
    finally
      LStorage.Free;
    end;
    Require(SameBytes(LBytes,FileBytes(LPath)),'read-only malformed history was repaired or rewritten');
    // The normal owner must release its raw lease even after initialization fails.
    LLease := TFileStream.Create(LPath,fmOpenReadWrite or fmShareExclusive);
    LLease.Free;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TBufferedAdoptionFixture.Performance(ALines: TStrings);
const Iterations = 1000;
var LStorage: TFileStorage; LOperations: TPCOperationsComp; LExpected: TStoredBlocks; LPath: String; LInput: TBytes; LHash: TRawBytes; LFrequency, LStart, LFinish, LChecksum: Int64; LSeconds: Double; I, J, LRepeat: Integer;
begin
  LPath := IncludeTrailingPathDelimiter(FDirectory)+'performance.blocks';
  LStorage := TFileStorage.Create(nil);
  LOperations := TPCOperationsComp.Create(nil);
  try
    LStorage.SetBlockChainFile(AnsiString(LPath));
    PopulateHistory(LStorage,LExpected);
    LStorage.SetBlockChainFile(AnsiString(LPath));
    Require(LStorage.Initialize,'cannot open the history sample');
    LInput := FileBytes(LPath);
    LHash := TCrypto.DoSha256(LInput);
    ALines.Add('INPUT_BYTES='+IntToStr(Length(LInput)));
    ALines.Add('INPUT_SHA256='+LHash.ToHexaString);
    Require(QueryPerformanceFrequency(LFrequency),'QPC frequency unavailable');
    for LRepeat := 1 to 3 do begin
      LChecksum := 0;
      QueryPerformanceCounter(LStart);
      for I := 1 to Iterations do
        for J := 0 to 4 do begin
          Require(LStorage.LoadBlockChainBlock(LOperations,J),'timed actual history load failed');
          Inc(LChecksum,LOperations.OperationBlock.block);
        end;
      QueryPerformanceCounter(LFinish);
      LSeconds := LFinish-LStart;
      LSeconds := LSeconds/LFrequency;
      Require(LChecksum=Iterations*10,'timed history result checksum differs');
      ALines.Add('HISTORY_REPEAT='+IntToStr(LRepeat)+';LOADS='+IntToStr(Iterations*5)+';SECONDS='+FloatToStr(LSeconds)+';CHECKSUM='+IntToStr(LChecksum));
    end;
    for J := 0 to 4 do begin
      Require(LStorage.LoadBlockChainBlock(LOperations,J),'post-timing history validation failed');
      Require(SameBytes(LExpected[J],StoredBytes(LOperations)),'post-timing payload bytes differ');
    end;
    Require(SameBytes(LInput,FileBytes(LPath)),'timed history reads changed input bytes');
  finally
    LOperations.Free;
    LStorage.Free;
  end;
end;

procedure TBufferedAdoptionFixture.PendingPerformance(ALines: TStrings);
const Iterations = 25; OperationCount = 128;
var LBank: TPCBank; LStorage: TFileStorage; LOriginal, LLoaded: TOperationsHashTree; LPath, LSharedSample, LError: String; LSample: TFileStream; LInput: TBytes; LHash: TRawBytes; LFrequency, LStart, LFinish, LChecksum: Int64; LSeconds: Double; I, LRepeat: Integer;
begin
  LBank := TPCBank.Create(nil);
  LStorage := nil;
  LOriginal := TOperationsHashTree.Create;
  LLoaded := TOperationsHashTree.Create;
  try
    SeedPendingBank(LBank,CT_PROTOCOL_5);
    LStorage := TFileStorage.Create(nil);
    LStorage.Bank := LBank;
    LPath := IncludeTrailingPathDelimiter(TPCBank.GetStorageFolder(LBank.Orphan))+'pendingbuffer.ops';
    LSharedSample := SysUtils.GetEnvironmentVariable('BUFFERED_ADOPTION_PENDING_SAMPLE');
    Require(LSharedSample<>'','performance runner must provide one shared owned pending sample path');
    if FileExists(LSharedSample) then begin
      LSample := TFileStream.Create(LSharedSample,fmOpenRead or fmShareDenyWrite);
      try
        Require(LOriginal.LoadOperationsHashTreeFromStream(LSample,True,LBank.SafeBox.CurrentProtocol,LBank.SafeBox.CurrentProtocol,nil,LError),'cannot decode the shared pending sample: '+LError);
      finally
        LSample.Free;
      end;
      Require(LOriginal.OperationsCount=OperationCount,'shared pending sample count changed');
      WriteBytes(LPath,FileBytes(LSharedSample));
    end else begin
      // Generate once, then reuse these exact signed bytes for both compilers
      // and both source versions; ECDSA signatures are otherwise nondeterministic.
      FillPending(LOriginal,LBank.SafeBox.CurrentProtocol,OperationCount);
      LStorage.SavePendingBufferOperations(LOriginal);
      WriteBytes(LSharedSample,FileBytes(LPath));
    end;
    LInput := FileBytes(LPath);
    LHash := TCrypto.DoSha256(LInput);
    ALines.Add('PENDING_PROTOCOL='+IntToStr(LBank.SafeBox.CurrentProtocol));
    ALines.Add('PENDING_INPUT_BYTES='+IntToStr(Length(LInput)));
    ALines.Add('PENDING_INPUT_SHA256='+LHash.ToHexaString);
    Require(QueryPerformanceFrequency(LFrequency),'QPC frequency unavailable');
    for LRepeat := 1 to 3 do begin
      LChecksum := 0;
      QueryPerformanceCounter(LStart);
      for I := 1 to Iterations do begin
        LLoaded.ClearHastThree;
        LStorage.LoadPendingBufferOperations(LLoaded);
        Inc(LChecksum,LLoaded.OperationsCount);
      end;
      QueryPerformanceCounter(LFinish);
      LSeconds := LFinish-LStart;
      LSeconds := LSeconds/LFrequency;
      Require(LChecksum=Iterations*OperationCount,'timed pending operation count differs');
      ALines.Add('PENDING_REPEAT='+IntToStr(LRepeat)+';LOADS='+IntToStr(Iterations)+';OPERATIONS='+IntToStr(OperationCount)+';SECONDS='+FloatToStr(LSeconds)+';CHECKSUM='+IntToStr(LChecksum));
    end;
    for I := 0 to OperationCount-1 do
      Require(TBaseType.Equals(LOriginal.GetOperation(I).Sha256,LLoaded.GetOperation(I).Sha256),'timed pending operation digest differs');
    FreeAndNil(LStorage);
    Require(SameBytes(LInput,FileBytes(LPath)),'timed pending loads changed saved bytes');
  finally
    LLoaded.Free;
    LOriginal.Free;
    LStorage.Free;
    LBank.Free;
  end;
end;

{$ENDIF}

procedure CheckBufferedAdoption(AIndex: Integer);
var LFixture: TBufferedAdoptionFixture;
begin
  LFixture := TBufferedAdoptionFixture.Create;
  try
    case AIndex of
      1: LFixture.Pending(CT_PROTOCOL_5,False);
      2: LFixture.Pending(CT_PROTOCOL_5,True);
      3: LFixture.History;
      4: LFixture.HeaderBoundary;
      5: LFixture.RandomTable(0);
      6: LFixture.RandomTable(1);
      7: LFixture.RandomTable(2);
      8: LFixture.RandomTable(3);
      9: LFixture.MalformedHistory;
      10: LFixture.Pending(CT_PROTOCOL_6,False);
      11: LFixture.Pending(CT_PROTOCOL_6,True);
    else
      raise EArgumentOutOfRangeException.Create('Invalid buffered-adoption check');
    end;
  finally
    LFixture.Free;
  end;
end;

function RunBufferedAdoptionChecks(const ALogPath: String): Integer;
var LLines: TStringList; I: Integer;
begin
  Result := 0;
  LLines := TStringList.Create;
  try
    for I := 1 to BufferedAdoptionCheckCount do begin
      try
        CheckBufferedAdoption(I);
        LLines.Add('PASS '+IntToStr(I)+' '+AdoptionCheckName(I));
      except
        on E: Exception do begin
          Inc(Result);
          LLines.Add('FAIL '+IntToStr(I)+' '+AdoptionCheckName(I)+' '+E.ClassName+': '+E.Message);
        end;
      end;
    end;
    {$IFDEF ASSUME_VALID_POW_OLD_PROTOCOLS}
    LLines.Add('RANDOMHASH_ENTRY=shipping-early-return');
    {$ELSE}
    LLines.Add('RANDOMHASH_ENTRY=table-entry-exercised');
    {$ENDIF}
    if Result=0 then LLines.Add('PASS '+IntToStr(BufferedAdoptionCheckCount));
    LLines.SaveToFile(ALogPath);
  finally
    LLines.Free;
  end;
end;

function RunBufferedAdoptionPerformance(const ALogPath: String): Integer;
{$IFDEF MSWINDOWS}
var LLines: TStringList; LFixture: TBufferedAdoptionFixture;
begin
  Result := 0;
  LLines := TStringList.Create;
  LFixture := TBufferedAdoptionFixture.Create;
  try
    try
      LFixture.Performance(LLines);
      LFixture.PendingPerformance(LLines);
      LLines.Add('PASS actual history performance');
    except
      on E: Exception do begin
        Result := 1;
        LLines.Add('FAIL '+E.ClassName+': '+E.Message);
      end;
    end;
    LLines.SaveToFile(ALogPath);
  finally
    LFixture.Free;
    LLines.Free;
  end;
end;

{$ELSE}
begin
  raise Exception.Create('The optional QPC performance probe requires Windows.');
end;
{$ENDIF}
end.
