unit UCheckpointStorageChecks;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Exercises checkpoint publication, malformed-candidate recovery and the live
  file's failed-flush marker through real core storage. Every file belongs to a
  newly generated test directory; no existing wallet data is opened.

  Public types in this unit
  TCheckpointStorageCheck | Selects one independently repeatable storage check. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}
{$I ../../../../config.inc}

interface

type
  TCheckpointStorageCheck = (cscFactoryFailure, cscPartialWrite, cscFlushFailure,
    cscValidationFailure, cscPublicationFailure, cscAbsentTargetFailures,
    cscSuccessfulRetry, cscMemoryPublication, cscEmptyPublication,
    cscAnalyzeMissing, cscAnalyzeTruncated, cscAnalyzeMalformed,
    cscHealthyMainBadCheckpoint, cscCorruptMainGoodCheckpoint,
    cscAllCorruptCandidates, cscCheckpointThenLocalReplay,
    cscFailedFlushException, cscFailedFlushShortWrite, cscSharingDenied,
    cscUncachedWriteException, cscUncachedShortWrite, cscUncachedFailedClose,
    cscUncachedPartialRetry, cscCachedFailedClose,
    cscHealthyMainBadLegacyBody, cscOlderLegacySurvivesBadNewerBody,
    cscUnstableHeaderRetryBeforeLaterPayload);

function RunCheckpointStorageCheck(ACheck: TCheckpointStorageCheck; const ALogPath: String): Integer;
function RunCheckpointStorageChecks(const ALogPath: String): Integer;

implementation

uses Classes, SysUtils, TypInfo, UAccounts, UBlockChain, UNode, UFileStorage,
  UPCAbstractMem, UPCDataTypes, UAbstractMem, UFileMem, UCacheMem, UCrypto, UConst, UBaseTypes;

type
  TCheckpointFailure = (cfNone, cfCreate, cfPartialWrite, cfFlush, cfValidation, cfPublish);

  TShortCheckpointStream = class(TFileStream)
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TFailureCheckpointMem = class(TPCAbstractMem)
  protected
    function CreateCheckpointStream(const AFileName: String): TFileStream; override;
    procedure FlushCheckpointStream(AStream: TFileStream); override;
    procedure PublishCheckpointFile(const ATemporaryFileName, ADestinationFileName: String); override;
  public
    Failure: TCheckpointFailure;
    FailureHits: Integer;
  end;

  TCheckpointSafeBox = class(TPCSafeBox)
  public
    procedure Build(const AKey: TAccountKey; AType: Word);
  end;

  TFailureFileMem = class(TFileMem)
  protected
    function AbsoluteWrite(const AAbsolutePosition: Int64; const ABuffer; ASize: Integer): Integer; override;
  public
    RaiseOnWrite, ShortOnWrite, PartialBeforeException: Boolean;
    HeaderFailuresRemaining: Integer;
    FailureHits: Integer;
    function Stable: Boolean;
  end;

  TCheckpointFixture = class
  private
    FDirectory, FSourceName, FTargetName, FForeignTemporary, FPreviousDirectory: String;
    FOwnDirectory, FOwnNodeDirectory: Boolean;
    FCloseLogs: Integer;
    FCloseLogText: String;
    FKey: TECPrivateKey;
    FSource: TFailureCheckpointMem;
    procedure SeedFile(const AFileName: String; AType: Word);
    procedure PrepareSource;
    procedure CheckFailure(AFailure: TCheckpointFailure; AExistingTarget: Boolean);
    procedure CheckAnalyze(AEmpty: Boolean);
    procedure CheckRestore(AMode: Integer);
    procedure CheckReplay;
    procedure CheckLegacyBodyRecovery(AHealthyMain: Boolean);
    procedure CheckFailedFlush(AShortWrite: Boolean);
    procedure CheckUncachedWrite(AShortWrite: Boolean);
    procedure CheckUncachedClose;
    procedure CheckUncachedPartialRetry;
    procedure CheckCachedClose;
    procedure CheckUnstableHeaderRetry;
    procedure ObserveStorageLog(ASender: TObject; const AText: String);
    procedure CheckNoOwnedTemporary;
    procedure CheckSavedType(const AFileName: String; AType: Word);
  public
    constructor Create(const ALogPath: String);
    destructor Destroy; override;
    procedure Run(ACheck: TCheckpointStorageCheck);
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create('Checkpoint storage: '+AMessage);
end;

function ReadBytes(const AFileName: String): TBytes;
var LFile: TFileStream;
begin
  LFile := TFileStream.Create(AFileName,fmOpenRead+fmShareDenyNone);
  try
    Require(LFile.Size<=32*1024*1024,'fixture unexpectedly exceeds 32 MiB');
    SetLength(Result,Integer(LFile.Size));
    if Length(Result)>0 then LFile.ReadBuffer(Result[0],Length(Result));
  finally
    LFile.Free;
  end;
end;

procedure WriteBytes(const AFileName: String; const ABytes: TBytes);
var LFile: TFileStream;
begin
  LFile := TFileStream.Create(AFileName,fmCreate);
  try
    if Length(ABytes)>0 then LFile.WriteBuffer(ABytes[0],Length(ABytes));
  finally
    LFile.Free;
  end;
end;

function SameBytes(const ALeft, ARight: TBytes): Boolean;
begin
  Result := Length(ALeft)=Length(ARight);
  if Result and (Length(ALeft)>0) then Result := CompareMem(@ALeft[0],@ARight[0],Length(ALeft));
end;

procedure RemoveOwnedDirectory(const ADirectory: String);
var LSearch: TSearchRec; LName: String;
begin
  // This helper receives only a fresh GUID fixture directory created below.
  // Never enumerate or delete outside that owned directory.
  if FindFirst(IncludeTrailingPathDelimiter(ADirectory)+'*',faAnyFile,LSearch)=0 then begin
    try
      repeat
        if (LSearch.Name<>'.') and (LSearch.Name<>'..') then begin
          LName := IncludeTrailingPathDelimiter(ADirectory)+LSearch.Name;
          if (LSearch.Attr and faDirectory)<>0 then RemoveOwnedDirectory(LName)
          else SysUtils.DeleteFile(LName);
        end;
      until FindNext(LSearch)<>0;
    finally
      FindClose(LSearch);
    end;
  end;
  RemoveDir(ADirectory);
end;

function TShortCheckpointStream.Write(const Buffer; Count: Longint): Longint;
var LShort: Longint;
begin
  LShort := Count div 2;
  if LShort=Count then LShort := 0;
  Result := inherited Write(Buffer,LShort);
end;

function TFailureCheckpointMem.CreateCheckpointStream(const AFileName: String): TFileStream;
begin
  case Failure of
    cfCreate: begin
      Inc(FailureHits);
      raise EWriteError.Create('Injected checkpoint temporary creation failure');
    end;
    cfPartialWrite: begin
      Inc(FailureHits);
      Result := TShortCheckpointStream.Create(AFileName,fmCreate+fmShareExclusive);
    end;
  else
    Result := inherited CreateCheckpointStream(AFileName);
  end;
end;

procedure TFailureCheckpointMem.FlushCheckpointStream(AStream: TFileStream);
var LWrongHeader: array[0..63] of Byte;
begin
  case Failure of
    cfFlush: begin
      Inc(FailureHits);
      raise EWriteError.Create('Injected checkpoint flush failure');
    end;
    cfValidation: begin
      Inc(FailureHits);
      FillChar(LWrongHeader,SizeOf(LWrongHeader),0);
      AStream.Position := 0;
      AStream.WriteBuffer(LWrongHeader,SizeOf(LWrongHeader));
    end;
  end;
  inherited FlushCheckpointStream(AStream);
end;

procedure TFailureCheckpointMem.PublishCheckpointFile(const ATemporaryFileName, ADestinationFileName: String);
begin
  if Failure=cfPublish then begin
    Inc(FailureHits);
    raise EWriteError.Create('Injected atomic checkpoint replacement failure');
  end;
  inherited PublishCheckpointFile(ATemporaryFileName,ADestinationFileName);
end;

procedure TCheckpointSafeBox.Build(const AKey: TAccountKey; AType: Word);
var LBlock: TOperationBlock; LAccount: TAccount;
begin
  LBlock := CT_OperationBlock_NUL;
  LBlock.account_key := AKey;
  LBlock.protocol_version := CT_PROTOCOL_1;
  LBlock.protocol_available := CT_PROTOCOL_6;
  LBlock.compact_target := CT_MinCompactTarget_v1;
  LBlock.reward := TPascalCoinProtocol.GetRewardForNewLine(0);
  AddNew(LBlock);
  LAccount := Account(0);
  UpdateAccount(0,LAccount.accountInfo,LAccount.name,AType,LAccount.balance,LAccount.n_operation,LAccount.account_data,LAccount.account_seal,aus_transaction_commit,LAccount.updated_on_block_passive_mode,LAccount.updated_on_block_active_mode,False,False);
end;

function TFailureFileMem.AbsoluteWrite(const AAbsolutePosition: Int64; const ABuffer; ASize: Integer): Integer;
begin
  if (AAbsolutePosition=0) and (ASize<=64) and (HeaderFailuresRemaining>0) then begin
    Dec(HeaderFailuresRemaining);
    Inc(FailureHits);
    raise EWriteError.Create('Injected initial unstable-header write failure');
  end;
  // Permit the small header marker write, but fail the actual dirty cache cell.
  if ((AAbsolutePosition>0) or (ASize>64)) and (RaiseOnWrite or ShortOnWrite) then begin
    Inc(FailureHits);
    if RaiseOnWrite then begin
      if PartialBeforeException then inherited AbsoluteWrite(AAbsolutePosition,ABuffer,ASize div 2);
      raise EWriteError.Create('Injected live cache flush failure');
    end;
    Exit(inherited AbsoluteWrite(AAbsolutePosition,ABuffer,ASize div 2));
  end;
  Result := inherited AbsoluteWrite(AAbsolutePosition,ABuffer,ASize);
end;

function TFailureFileMem.Stable: Boolean;
begin
  Result := IsAbstractMemInfoStable;
end;

constructor TCheckpointFixture.Create(const ALogPath: String);
var LID: TGUID;
begin
  inherited Create;
  Require(GetEnvironmentVariable('TEMP')<>'','the caller must supply an owned TEMP directory');
  Require(CreateGUID(LID)=0,'cannot allocate fixture identity');
  FDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))+'CheckpointStorage-'+GUIDToString(LID);
  Require(not DirectoryExists(FDirectory),'refusing an existing fixture directory');
  Require(ForceDirectories(FDirectory),'cannot create owned fixture directory');
  FOwnDirectory := True;
  FSourceName := IncludeTrailingPathDelimiter(FDirectory)+'source.am_safebox';
  FTargetName := IncludeTrailingPathDelimiter(FDirectory)+'checkpoint6.am_safebox';
  FForeignTemporary := FTargetName+'.unrelated.tmp';
  Require(not TNode.NodeExists,'the storage fixture requires its own node lifetime');
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(FDirectory);
  FOwnNodeDirectory := True;
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
end;

destructor TCheckpointFixture.Destroy;
begin
  FSource.Free;
  // A core helper must not create a hidden singleton. If it does, retire it
  // before restoring the directory so even its saver remains inside the fixture.
  if FOwnNodeDirectory and TNode.NodeExists then TNode.Node.Free;
  FKey.Free;
  if FOwnNodeDirectory then TNode.SetPascalCoinDataFolder(FPreviousDirectory);
  if FOwnDirectory then RemoveOwnedDirectory(FDirectory);
  inherited;
end;

procedure TCheckpointFixture.SeedFile(const AFileName: String; AType: Word);
var LSafeBox: TCheckpointSafeBox;
begin
  Require(not FileExists(AFileName),'refusing an existing fixture seed');
  LSafeBox := TCheckpointSafeBox.Create;
  try
    LSafeBox.Build(FKey.PublicKey,AType);
    LSafeBox.SaveCheckpointing(AFileName);
  finally
    LSafeBox.Free;
  end;
  Require(not TNode.NodeExists,'bank/storage check unexpectedly created the global node');
end;

procedure TCheckpointFixture.PrepareSource;
begin
  SeedFile(FSourceName,111);
  FSource := TFailureCheckpointMem.Create(FSourceName,False);
  FSource.FlushCache;
end;

procedure TCheckpointFixture.CheckNoOwnedTemporary;
var LSearch: TSearchRec;
begin
  if FindFirst(FTargetName+'.*.tmp',faAnyFile,LSearch)=0 then begin
    try
      repeat
        Require(SameFileName(IncludeTrailingPathDelimiter(FDirectory)+LSearch.Name,FForeignTemporary),'temporary checkpoint leaked after publication/failure: '+LSearch.Name);
      until FindNext(LSearch)<>0;
    finally
      FindClose(LSearch);
    end;
  end;
end;

procedure TCheckpointFixture.CheckSavedType(const AFileName: String; AType: Word);
var LSaved: TPCAbstractMem;
begin
  LSaved := TPCAbstractMem.Create(AFileName,True);
  try
    Require((LSaved.BlocksCount=1) and (LSaved.AccountsCount=5),'published checkpoint has wrong counts');
    Require(LSaved.GetAccount(0).account_type=AType,'published checkpoint contains stale account data');
    Require(LSaved.BufferBlocksHash.Length=32,'published checkpoint has wrong block-hash buffer length');
  finally
    LSaved.Free;
  end;
end;

procedure TCheckpointFixture.CheckFailure(AFailure: TCheckpointFailure; AExistingTarget: Boolean);
var LSourceBefore, LTargetBefore, LForeign: TBytes; LRaised: Boolean;
begin
  if FSource=nil then PrepareSource;
  if AExistingTarget and not FileExists(FTargetName) then SeedFile(FTargetName,222);
  if not AExistingTarget then Require(not FileExists(FTargetName),'absent-target case already has a checkpoint');
  LSourceBefore := ReadBytes(FSourceName);
  if AExistingTarget then LTargetBefore := ReadBytes(FTargetName);
  SetLength(LForeign,4);
  LForeign[0] := 91;
  LForeign[1] := 92;
  LForeign[2] := 93;
  LForeign[3] := 94;
  WriteBytes(FForeignTemporary,LForeign);
  FSource.Failure := AFailure;
  FSource.FailureHits := 0;
  LRaised := False;
  try
    try
      FSource.SaveToFile(FTargetName);
    except
      on E: EStreamError do LRaised := True;
      on E: EPCAbstractMem do LRaised := True;
      on E: EOSError do LRaised := True;
    end;
  finally
    FSource.Failure := cfNone;
  end;
  Require(LRaised,'injected failed checkpoint was reported as success');
  Require(FSource.FailureHits>0,'the intended failure boundary was not reached');
  Require(SameBytes(LSourceBefore,ReadBytes(FSourceName)),'failed checkpoint modified the live source');
  if AExistingTarget then begin
    Require(SameBytes(LTargetBefore,ReadBytes(FTargetName)),'failed checkpoint replaced/truncated the previous good file');
    CheckSavedType(FTargetName,222);
  end else Require(not FileExists(FTargetName),'failed first checkpoint left a published partial file');
  Require(SameBytes(LForeign,ReadBytes(FForeignTemporary)),'cleanup modified an unrelated temporary file');
  CheckNoOwnedTemporary;
end;

procedure TCheckpointFixture.CheckAnalyze(AEmpty: Boolean);
var LBytes, LBefore, LHeader: TBytes; LFile: TFileStream;
  LCount, LBlocks, LAccounts, LVariant: Integer;
  LReader: TPCAbstractMem; LHeaderZone: TAMZone; LBlocksZone, LAccountsZone: UInt32;

  procedure RejectReadOnly(const AExpected: TBytes);
  var LRepeat: Integer;
  begin
    for LRepeat := 1 to 3 do begin
      LCount := 123456;
      Require(not TPCAbstractMem.AnalyzeFile(FTargetName,LCount),'malformed/truncated candidate was accepted');
      Require(LCount=0,'rejected candidate retained a stale block count');
    end;
    Require(SameBytes(AExpected,ReadBytes(FTargetName)),'read-only candidate analysis modified invalid evidence');
  end;

begin
  if AEmpty then begin
    SetLength(LBytes,64);
    FillChar(LBytes[0],Length(LBytes),0);
    WriteBytes(FTargetName,LBytes);
  end else begin
    SeedFile(FTargetName,111);
    LFile := TFileStream.Create(FTargetName,fmOpenReadWrite);
    try
      LFile.Size := LFile.Size div 2;
    finally
      LFile.Free;
    end;
  end;
  LBefore := ReadBytes(FTargetName);
  // Repeat malformed construction to expose cleanup leaks in heap tracing.
  RejectReadOnly(LBefore);
  if AEmpty then begin
    SeedFile(FSourceName,111);
    LBefore := ReadBytes(FSourceName);
    LReader := TPCAbstractMem.Create(FSourceName,True);
    try
      Require(LReader.AbstractMem.ReadFirstData(LHeaderZone,LHeader),'cannot locate the valid fixture header');
      Require(Length(LHeader)>=24,'fixture has a short core header');
      Move(LHeader[16],LBlocksZone,4);
      Move(LHeader[20],LAccountsZone,4);
    finally
      LReader.Free;
    end;
    Require((Int64(LBlocksZone)+12<=Length(LBefore)) and (Int64(LAccountsZone)+12<=Length(LBefore)),'fixture list headers are outside the file');
    // Preserve the AbstractMem header while corrupting only persisted counts.
    // Reject these before count arithmetic or a block-hash allocation can run.
    for LVariant := 0 to 3 do begin
      case LVariant of
        0: begin
          LBlocks := -1;
          LAccounts := 5;
        end;
        1: begin
          LBlocks := High(Integer);
          LAccounts := 5;
        end;
        2: begin
          LBlocks := 1;
          LAccounts := 4;
        end;
      else
        LBlocks := 100000000;
        LAccounts := 500000000;
      end;
      LBytes := Copy(LBefore,0,Length(LBefore));
      Move(LBlocks,LBytes[LBlocksZone+8],4);
      Move(LAccounts,LBytes[LAccountsZone+8],4);
      WriteBytes(FTargetName,LBytes);
      RejectReadOnly(LBytes);
    end;
  end;
end;

procedure TCheckpointFixture.CheckRestore(AMode: Integer);
var LPreviousDirectory, LMain, LCheckpoint, LError: String; LBank: TPCBank;
  LBad, LMainBefore, LCheckpointBefore: TBytes; LRaised: Boolean; LCount: Integer;
begin
  LPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(FDirectory);
  try
    Require(ForceDirectories(IncludeTrailingPathDelimiter(FDirectory)+'Data'),'cannot create owned data directory');
    LMain := IncludeTrailingPathDelimiter(FDirectory)+'Data'+PathDelim+'safebox.am_safebox';
    LCheckpoint := IncludeTrailingPathDelimiter(FDirectory)+'Data'+PathDelim+'checkpoint6.am_safebox';
    SetLength(LBad,64);
    FillChar(LBad[0],Length(LBad),0);
    if AMode=0 then begin
      SeedFile(LMain,111);
      WriteBytes(LCheckpoint,LBad);
    end else if AMode=1 then begin
      WriteBytes(LMain,LBad);
      SeedFile(LCheckpoint,111);
    end else begin
      WriteBytes(LMain,LBad);
      WriteBytes(LCheckpoint,LBad);
    end;
    LMainBefore := ReadBytes(LMain);
    LCheckpointBefore := ReadBytes(LCheckpoint);
    LBank := TPCBank.Create(nil);
    try
      LBank.StorageClass := TFileStorage;
      LRaised := False;
      LError := '';
      try
        LBank.RestoreBank(-1,'',nil);
      except
        on E: Exception do begin
          Require(not (E is EAccessViolation),'candidate failure became a cleanup access violation');
          LRaised := True;
          LError := E.ClassName+': '+E.Message;
        end;
      end;
      if AMode=2 then begin
        Require(LRaised,'all-invalid existing storage was silently replaced with an empty chain');
        Require(Pos('No readable safebox or checkpoint',LError)>0,'all-invalid storage did not report the recovery failure clearly: '+LError);
      end
      else begin
        Require(not LRaised,'a rejected candidate prevented valid safebox recovery: '+LError);
        Require((LBank.BlocksCount=1) and (LBank.SafeBox.Account(0).account_type=111),'recovery chose the wrong safebox');
      end;
    finally
      LBank.Free;
    end;
    Require(SameBytes(LCheckpointBefore,ReadBytes(LCheckpoint)),'recovery modified its source/rejected checkpoint');
    if AMode=2 then Require(SameBytes(LMainBefore,ReadBytes(LMain)),'failed recovery overwrote the original invalid live file');
    if AMode=0 then Require(SameBytes(LMainBefore,ReadBytes(LMain)),'bad checkpoint changed the healthy main safebox');
    if AMode=1 then begin
      Require(TPCAbstractMem.AnalyzeFile(LMain,LCount) and (LCount=1),'recovered live file did not reopen');
    end;
  finally
    TNode.SetPascalCoinDataFolder(LPreviousDirectory);
  end;
end;

procedure TCheckpointFixture.CheckReplay;
var LPreviousDirectory, LCheckpoint, LMain, LFixtureName: String; LBank: TPCBank;
  LBlocks: TFileStream; LOperations: TPCOperationsComp; LCount: Cardinal;
  LError: String; I: Integer; LCheckpointBefore, LBad: TBytes;
begin
  LPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(FDirectory);
  try
    LFixtureName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'mainnet-blocks-0-4.bin';
    Require(FileExists(LFixtureName),'committed public block fixture is missing beside the test executable');
    LCheckpoint := IncludeTrailingPathDelimiter(FDirectory)+'Data'+PathDelim+'checkpoint0.am_safebox';
    LMain := IncludeTrailingPathDelimiter(FDirectory)+'Data'+PathDelim+'safebox.am_safebox';
    LBank := TPCBank.Create(nil);
    try
      LBank.StorageClass := TFileStorage;
      LBlocks := TFileStream.Create(LFixtureName,fmOpenRead+fmShareDenyWrite);
      LOperations := TPCOperationsComp.Create(LBank);
      try
        LBlocks.ReadBuffer(LCount,SizeOf(LCount));
        Require(LCount=5,'public block fixture count changed');
        for I := 0 to 4 do begin
          LError := '';
          Require(LOperations.LoadBlockFromStream(LBlocks,LError),'public block decode failed: '+LError);
          Require(LOperations.OperationBlock.block=Cardinal(I),'public block fixture order changed');
          Require(LBank.AddNewBlockChainBlock(LOperations,0,LError),'public block did not validate: '+LError);
          if I=2 then LBank.SafeBox.SaveCheckpointing(LCheckpoint);
        end;
      finally
        LOperations.Free;
        LBlocks.Free;
      end;
      Require(LBank.BlocksCount=5,'fixture did not persist all five blocks');
    finally
      LBank.Free;
    end;
    LCheckpointBefore := ReadBytes(LCheckpoint);
    SetLength(LBad,64);
    FillChar(LBad[0],Length(LBad),0);
    WriteBytes(LMain,LBad);
    LBank := TPCBank.Create(nil);
    try
      LBank.StorageClass := TFileStorage;
      LBank.DiskRestoreFromOperations(4,nil);
      Require(LBank.BlocksCount=5,'recovery did not replay local blocks after the valid checkpoint');
      Require(LBank.LastOperationBlock.block=4,'replayed bank has the wrong tip');
    finally
      LBank.Free;
    end;
    Require(SameBytes(LCheckpointBefore,ReadBytes(LCheckpoint)),'local replay modified the last good recovery checkpoint');
  finally
    TNode.SetPascalCoinDataFolder(LPreviousDirectory);
  end;
end;

procedure TCheckpointFixture.CheckLegacyBodyRecovery(AHealthyMain: Boolean);
var LPreviousDirectory, LMain, LOlder, LNewer, LFixtureName, LError: String;
  LBank: TPCBank;
  LBlocks: TFileStream;
  LOperations: TPCOperationsComp;
  LCount: Cardinal;
  I: Integer;
  LExpectedHash: TRawBytes;
  LMainBefore, LOlderBefore, LNewerBefore: TBytes;
  LHeader: TPCSafeBoxHeader;

  procedure SaveLegacy(const AFileName: String; AWithoutBody: Boolean);
  var LFull: TMemoryStream;
    LFile: TFileStream;
    LStreamHeader: TPCSafeBoxHeader;
    LHeaderSize: Int64;
    LEndOffset: Cardinal;
    J: Integer;
  begin
    LFull := TMemoryStream.Create;
    try
      LBank.SafeBox.SaveSafeBoxToAStream(LFull,0,LBank.BlocksCount-1);
      LFull.Position := 0;
      Require(TPCSafeBox.LoadSafeBoxStreamHeader(LFull,LStreamHeader),
        'cannot read the real legacy fixture header');
      LHeaderSize := LFull.Position;
      LFile := TFileStream.Create(AFileName,fmCreate);
      try
        LFull.Position := 0;
        if not AWithoutBody then LFile.CopyFrom(LFull,LFull.Size)
        else begin
          // Retain a genuine newer header and its final hash, but remove the
          // block bodies. Header-only inspection still accepts this candidate;
          // full loading must reject it before it can outrank intact storage.
          LFile.CopyFrom(LFull,LHeaderSize);
          LEndOffset := (LStreamHeader.blocksCount+1)*SizeOf(Cardinal);
          for J := 0 to Integer(LStreamHeader.blocksCount) do
            LFile.WriteBuffer(LEndOffset,SizeOf(LEndOffset));
          TStreamOp.WriteAnsiString(LFile,LStreamHeader.safeBoxHash);
        end;
      finally
        LFile.Free;
      end;
    finally
      LFull.Free;
    end;
  end;

begin
  LPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(FDirectory);
  try
    Require(ForceDirectories(IncludeTrailingPathDelimiter(FDirectory)+'Data'),
      'cannot create the owned legacy recovery directory');
    LMain := IncludeTrailingPathDelimiter(FDirectory)+'Data'+PathDelim+'safebox.am_safebox';
    LOlder := IncludeTrailingPathDelimiter(FDirectory)+'Data'+PathDelim+'recovery-old.safebox';
    LNewer := IncludeTrailingPathDelimiter(FDirectory)+'Data'+PathDelim+'recovery-new.safebox';
    LFixtureName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'mainnet-blocks-0-4.bin';
    Require(FileExists(LFixtureName),'the public five-block fixture is missing');
    LBank := TPCBank.Create(nil);
    try
      LBank.StorageClass := TFileStorage;
      LBlocks := TFileStream.Create(LFixtureName,fmOpenRead+fmShareDenyWrite);
      LOperations := TPCOperationsComp.Create(LBank);
      try
        LBlocks.ReadBuffer(LCount,SizeOf(LCount));
        Require(LCount=5,'the public block fixture count changed');
        for I := 0 to 4 do begin
          LError := '';
          Require(LOperations.LoadBlockFromStream(LBlocks,LError),'public legacy block decode failed: '+LError);
          Require(LBank.AddNewBlockChainBlock(LOperations,0,LError),'public legacy block did not validate: '+LError);
          if I=2 then begin
            LExpectedHash := Copy(LBank.SafeBox.SafeBoxHash);
            if AHealthyMain then LBank.SafeBox.SaveCheckpointing(LMain)
            else SaveLegacy(LOlder,False);
          end;
        end;
        SaveLegacy(LNewer,True);
        Require(LBank.LoadBankFileInfo(LNewer,LHeader) and (LHeader.blocksCount=5),
          'the bad-body fixture did not pass the real header-only candidate check');
      finally
        LOperations.Free;
        LBlocks.Free;
      end;
    finally
      LBank.Free;
    end;
    LNewerBefore := ReadBytes(LNewer);
    if AHealthyMain then LMainBefore := ReadBytes(LMain)
    else begin
      Require(not FileExists(LMain),'the legacy-only fixture unexpectedly has a live safebox');
      LOlderBefore := ReadBytes(LOlder);
    end;
    LBank := TPCBank.Create(nil);
    try
      LBank.StorageClass := TFileStorage;
      Require(LBank.RestoreBank(-1,'',nil),'recovery did not accept the valid older storage');
      Require((LBank.BlocksCount=3) and (LBank.SafeBox.AccountsCount=15),
        'the corrupt newer legacy body displaced the valid three-block state');
      Require(TBaseType.Equals(LExpectedHash,LBank.SafeBox.SafeBoxHash),
        'legacy fallback restored a different confirmed safebox hash');
    finally
      LBank.Free;
    end;
    Require(SameBytes(LNewerBefore,ReadBytes(LNewer)),
      'recovery modified the rejected legacy candidate');
    if AHealthyMain then
      Require(SameBytes(LMainBefore,ReadBytes(LMain)),
        'a bad newer legacy body changed the healthy live safebox')
    else
      Require(SameBytes(LOlderBefore,ReadBytes(LOlder)),
        'legacy fallback modified its valid recovery source');
  finally
    TNode.SetPascalCoinDataFolder(LPreviousDirectory);
  end;
end;

procedure TCheckpointFixture.CheckFailedFlush(AShortWrite: Boolean);
var LMem: TFailureFileMem; LReopened: TFileMem; LZone: TAMZone;
  LExpected, LRead: array[0..127] of Byte; LPhysical: TBytes; LRaised, LResult: Boolean;
begin
  LMem := TFailureFileMem.Create(FSourceName,False);
  try
    LZone := LMem.New(SizeOf(LExpected));
    FillChar(LExpected,SizeOf(LExpected),17);
    LMem.Write(LZone.position,LExpected,SizeOf(LExpected));
    Require(LMem.FlushCache,'initial live-file fixture flush failed');
    FillChar(LExpected,SizeOf(LExpected),83);
    LMem.Write(LZone.position,LExpected,SizeOf(LExpected));
    LMem.ShortOnWrite := AShortWrite;
    LMem.RaiseOnWrite := not AShortWrite;
    LRaised := False;
    LResult := True;
    try
      LResult := LMem.FlushCache;
    except
      on E: EWriteError do LRaised := True;
      on E: EAbstractMem do LRaised := True;
    end;
    Require(LMem.FailureHits>0,'the dirty cache write was not exercised');
    if AShortWrite then Require(LRaised or not LResult,'a short cache write was reported successful')
    else Require(LRaised,'a cache write exception was swallowed');
    Require(not LMem.Stable,'failed cache flush was marked stable in memory');
    LPhysical := ReadBytes(FSourceName);
    Require((Length(LPhysical)>6) and (LPhysical[6]=0),'failed cache flush published a stable on-disk header');
    LMem.ShortOnWrite := False;
    LMem.RaiseOnWrite := False;
    Require(LMem.FlushCache,'retry did not flush the retained dirty cache');
    Require(LMem.Stable,'successful retry did not restore the stable marker');
  finally
    LMem.ShortOnWrite := False;
    LMem.RaiseOnWrite := False;
    LMem.Free;
  end;
  LPhysical := ReadBytes(FSourceName);
  Require((Length(LPhysical)>6) and (LPhysical[6]=1),'successful retry did not persist a stable header');
  LReopened := TFileMem.Create(FSourceName,True);
  try
    Require(LReopened.Read(LZone.position,LRead,SizeOf(LRead))=SizeOf(LRead),'retried live file could not be read');
    Require(CompareMem(@LExpected[0],@LRead[0],SizeOf(LRead)),'retried live data was lost');
  finally
    LReopened.Free;
  end;
end;

procedure TCheckpointFixture.CheckUncachedWrite(AShortWrite: Boolean);
var LMem: TFailureFileMem; LReopened: TFileMem; LZone: TAMZone;
  LExpected, LRead: array[0..127] of Byte; LPhysical: TBytes; LRaised: Boolean;
begin
  LMem := TFailureFileMem.Create(FSourceName,False);
  try
    LZone := LMem.New(SizeOf(LExpected));
    FillChar(LExpected,SizeOf(LExpected),17);
    LMem.Write(LZone.position,LExpected,SizeOf(LExpected));
    Require(LMem.FlushCache,'initial uncached-write fixture flush failed');
    LMem.UseCache := False;
    FillChar(LExpected,SizeOf(LExpected),83);
    LMem.PartialBeforeException := True;
    LMem.ShortOnWrite := AShortWrite;
    LMem.RaiseOnWrite := not AShortWrite;
    LRaised := False;
    try
      LMem.Write(LZone.position,LExpected,SizeOf(LExpected));
    except
      on E: EWriteError do LRaised := True;
      on E: EAbstractMem do LRaised := True;
    end;
    Require(LRaised and (LMem.FailureHits>0),'uncached partial write was reported successful or not exercised');
    Require(not LMem.Stable,'failed uncached write was marked stable in memory');
    LPhysical := ReadBytes(FSourceName);
    Require((Length(LPhysical)>LZone.position+127) and (LPhysical[6]=0),'uncached failure retained a stable physical header');
    Require((LPhysical[LZone.position]=83) and (LPhysical[LZone.position+127]=17),'uncached injection did not write only part of the actual payload');
    LMem.ShortOnWrite := False;
    LMem.RaiseOnWrite := False;
    LMem.Write(LZone.position,LExpected,SizeOf(LExpected));
    Require(LMem.FlushCache,'uncached retry did not flush the corrected payload');
    Require(LMem.Stable,'successful uncached retry did not restore the stable marker');
  finally
    LMem.ShortOnWrite := False;
    LMem.RaiseOnWrite := False;
    LMem.Free;
  end;
  LPhysical := ReadBytes(FSourceName);
  Require((Length(LPhysical)>6) and (LPhysical[6]=1),'uncached retry did not persist the stable marker');
  LReopened := TFileMem.Create(FSourceName,True);
  try
    Require(LReopened.Read(LZone.position,LRead,SizeOf(LRead))=SizeOf(LRead),'uncached retry file could not be read');
    Require(CompareMem(@LExpected[0],@LRead[0],SizeOf(LRead)),'uncached retry did not persist the complete payload');
  finally
    LReopened.Free;
  end;
end;

procedure TCheckpointFixture.CheckUncachedClose;
var LMem: TFailureFileMem; LReadOnly: TFileMem; LExclusive: TFileStream;
  LZone: TAMZone; LBuffer: array[0..127] of Byte;
  LFileName: String; LPhysical: TBytes; LRaised: Boolean; LMode: Integer;
begin
  for LMode := 0 to 1 do begin
    LFileName := FSourceName+'.failed-close-'+IntToStr(LMode);
    LMem := TFailureFileMem.Create(LFileName,False);
    try
      LZone := LMem.New(SizeOf(LBuffer));
      FillChar(LBuffer,SizeOf(LBuffer),17);
      LMem.Write(LZone.position,LBuffer,SizeOf(LBuffer));
      Require(LMem.FlushCache,'failed-close fixture did not persist its initial state');
      LMem.UseCache := False;
      LMem.PartialBeforeException := True;
      LMem.RaiseOnWrite := LMode=0;
      LMem.ShortOnWrite := LMode=1;
      FillChar(LBuffer,SizeOf(LBuffer),83);
      LRaised := False;
      try
        LMem.Write(LZone.position,LBuffer,SizeOf(LBuffer));
      except
        on E: EWriteError do LRaised := True;
        on E: EAbstractMem do LRaised := True;
      end;
      Require(LRaised and (LMem.FailureHits>0),'failed-close case did not exercise the partial payload write');
      LMem.RaiseOnWrite := False;
      LMem.ShortOnWrite := False;
      Require(not LMem.FlushCache,'flush committed an uncached failure without a payload retry');
      LPhysical := ReadBytes(LFileName);
      Require(LPhysical[6]=0,'no-retry flush published a stable header');
      // The write and explicit flush already reported failure. Destruction must
      // release normally; throwing again can leak the instance on FPC.
      FreeAndNil(LMem);
    finally
      LMem.Free;
    end;
    // Successful exclusive open proves that the exceptional destructor released
    // its real native handle rather than leaking it to keep the test passing.
    LExclusive := TFileStream.Create(LFileName,fmOpenRead+fmShareExclusive);
    LExclusive.Free;
    Require(SameBytes(LPhysical,ReadBytes(LFileName)),'exceptional close changed the incomplete-file evidence');
    LReadOnly := TFileMem.Create(LFileName,True);
    try
      Require(not LReadOnly.HeaderInitialized,'read-only reopen accepted the failed stable marker');
    finally
      LReadOnly.Free;
    end;
    Require(SameBytes(LPhysical,ReadBytes(LFileName)),'read-only inspection changed the failed file');
  end;
end;

procedure TCheckpointFixture.ObserveStorageLog(ASender: TObject; const AText: String);
begin
  Inc(FCloseLogs);
  FCloseLogText := FCloseLogText+AText+sLineBreak;
end;

procedure TCheckpointFixture.CheckCachedClose;
var LMem: TFailureFileMem; LCache: TCacheMem; LExclusive: TFileStream;
  LZone: TAMZone; LBuffer: array[0..127] of Byte; LPhysical: TBytes;
begin
  LMem := TFailureFileMem.Create(FSourceName,False);
  try
    LZone := LMem.New(SizeOf(LBuffer));
    FillChar(LBuffer,SizeOf(LBuffer),17);
    LMem.Write(LZone.position,LBuffer,SizeOf(LBuffer));
    Require(LMem.FlushCache,'cached-close fixture initial flush failed');
    FillChar(LBuffer,SizeOf(LBuffer),83);
    LMem.Write(LZone.position,LBuffer,SizeOf(LBuffer));
    LCache := LMem.LockCache;
    try
      LCache.OnLog := ObserveStorageLog;
    finally
      LMem.UnlockCache;
    end;
    LMem.RaiseOnWrite := True;
    LMem.PartialBeforeException := True;
    // Here the first I/O failure occurs during automatic close. It must be
    // diagnosed through the existing cache log hook, while releasing normally.
    FreeAndNil(LMem);
  finally
    LMem.Free;
  end;
  Require((FCloseLogs>0) and (Pos(FSourceName,FCloseLogText)>0),'automatic close lost the storage failure filename/diagnostic');
  LExclusive := TFileStream.Create(FSourceName,fmOpenRead+fmShareExclusive);
  LExclusive.Free;
  LPhysical := ReadBytes(FSourceName);
  Require((Length(LPhysical)>6) and (LPhysical[6]=0),'failed cached close certified incomplete data as stable');
end;

procedure TCheckpointFixture.CheckUncachedPartialRetry;
var LMem: TFailureFileMem; LReadOnly: TFileMem; LFirst, LSecond: TAMZone;
  LBuffer: array[0..127] of Byte; LBytes, LComplete, LRead: TBytes;
  I, LSpan: Integer; LRaised: Boolean; LPosition: Int64;
begin
  LMem := TFailureFileMem.Create(FSourceName,False);
  try
    LFirst := LMem.New(SizeOf(LBuffer));
    LSecond := LMem.New(SizeOf(LBuffer));
    FillChar(LBuffer,SizeOf(LBuffer),17);
    LMem.Write(LFirst.position,LBuffer,SizeOf(LBuffer));
    LMem.Write(LSecond.position,LBuffer,SizeOf(LBuffer));
    Require(LMem.FlushCache,'partial-retry fixture initial flush failed');
    LMem.UseCache := False;
    FillChar(LBuffer,SizeOf(LBuffer),83);
    LMem.ShortOnWrite := True;
    for I := 0 to 1 do begin
      if I=0 then LPosition := LFirst.position else LPosition := LSecond.position;
      LRaised := False;
      try
        LMem.Write(LPosition,LBuffer,SizeOf(LBuffer));
      except
        on E: EWriteError do LRaised := True;
        on E: EAbstractMem do LRaised := True;
      end;
      Require(LRaised,'a disjoint short-write failure was swallowed');
    end;
    Require(LMem.FailureHits>=2,'both failed payload regions were not exercised');
    LMem.ShortOnWrite := False;
    LMem.Write(LFirst.position,LBuffer,SizeOf(LBuffer));
    Require(not LMem.FlushCache,'retry of the first region forgot the second failed region');
    LMem.Write(LSecond.position,LBuffer,SizeOf(LBuffer));
    Require(not LMem.FlushCache,'separate retries incorrectly cleared the conservative union');
    LBytes := ReadBytes(FSourceName);
    Require(LBytes[6]=0,'partial union retry persisted a stable marker');
    LSpan := Integer(LSecond.position+SizeOf(LBuffer)-LFirst.position);
    LComplete := Copy(LBytes,Integer(LFirst.position),LSpan);
    // Include the unchanged allocation metadata between the two payloads. One
    // complete covering retry retires the conservative failed-write union.
    LMem.Write(LFirst.position,LComplete[0],Length(LComplete));
    Require(LMem.FlushCache and LMem.Stable,'complete covering retry did not restore stable state');
  finally
    LMem.ShortOnWrite := False;
    LMem.RaiseOnWrite := False;
    LMem.Free;
  end;
  LBytes := ReadBytes(FSourceName);
  Require(LBytes[6]=1,'covering retry did not persist the final stable marker');
  LReadOnly := TFileMem.Create(FSourceName,True);
  try
    SetLength(LRead,Length(LComplete));
    Require(LReadOnly.Read(LFirst.position,LRead[0],Length(LRead))=Length(LRead),'covering retry could not reopen');
    Require(SameBytes(LComplete,LRead),'covering retry lost payload/allocation bytes');
  finally
    LReadOnly.Free;
  end;
end;

procedure TCheckpointFixture.CheckUnstableHeaderRetry;
var LMode: Integer;
  LMem, LReopened: TFailureFileMem;
  LCache: TCacheMem;
  LFirst, LSecond: TAMZone;
  LFileName: String;
  LBytes: TBytes;
  LOldValue, LFirstValue, LSecondValue, LReadValue: Cardinal;
  LRaised: Boolean;
begin
  for LMode := 0 to 1 do begin
    LFileName := IncludeTrailingPathDelimiter(FDirectory)+'header-retry-'+IntToStr(LMode)+'.am';
    LOldValue := 11;
    LFirstValue := 1234;
    LSecondValue := 5678;
    LMem := TFailureFileMem.Create(LFileName,False);
    try
      LMem.UseCache := LMode=0;
      LFirst := LMem.New(128);
      LSecond := LMem.New(128);
      LMem.Write(LFirst.position,LOldValue,SizeOf(LOldValue));
      LMem.Write(LSecond.position,LOldValue,SizeOf(LOldValue));
      Require(LMem.FlushCache,'cannot establish the stable header retry fixture');
      LBytes := ReadBytes(LFileName);
      Require((Length(LBytes)>6) and (LBytes[6]=1),'header retry fixture was not physically stable');
      LMem.HeaderFailuresRemaining := 1;
      LRaised := False;
      try
        LMem.Write(LFirst.position,LFirstValue,SizeOf(LFirstValue));
        if LMode=0 then begin
          // Exercise actual cache eviction's callback, not FileMem.FlushCache's
          // separate explicit header write. The first invalidation must fail.
          LCache := LMem.LockCache;
          try
            LCache.FlushCache;
          finally
            LMem.UnlockCache;
          end;
        end;
      except
        on E: EWriteError do LRaised := True;
      end;
      Require(LRaised and (LMem.HeaderFailuresRemaining=0) and (LMem.FailureHits=1),
        'the initial unstable-header write did not fail at the intended boundary');
      Require(LMem.Write(LSecond.position,LSecondValue,SizeOf(LSecondValue))=SizeOf(LSecondValue),
        'the different payload retry did not complete');
      if LMode=0 then begin
        LCache := LMem.LockCache;
        try
          Require(LCache.FlushCache,'the later cached payload could not retry');
        finally
          LMem.UnlockCache;
        end;
      end;
      LBytes := ReadBytes(LFileName);
      Require((Length(LBytes)>6) and (LBytes[6]=0),
        'a later payload write skipped durable invalidation after the first header failure');
      if LMode=1 then
        Require(LMem.Write(LFirst.position,LFirstValue,SizeOf(LFirstValue))=SizeOf(LFirstValue),
          'the originally rejected uncached write could not be repaired');
      // Normal close now owns all complete bytes; it must finish the pending
      // marker protocol before read-only reopening can accept the repaired data.
    finally
      LMem.Free;
    end;
    LReopened := TFailureFileMem.Create(LFileName,True);
    try
      Require(LReopened.HeaderInitialized,'repaired header retry did not close as a valid file');
      Require(LReopened.Read(LFirst.position,LReadValue,SizeOf(LReadValue))=SizeOf(LReadValue),
        'cannot reopen the first repaired payload');
      Require(LReadValue=LFirstValue,'the first repaired payload changed');
      Require(LReopened.Read(LSecond.position,LReadValue,SizeOf(LReadValue))=SizeOf(LReadValue),
        'cannot reopen the later payload');
      Require(LReadValue=LSecondValue,'the later payload changed');
    finally
      LReopened.Free;
    end;
  end;
end;

procedure TCheckpointFixture.Run(ACheck: TCheckpointStorageCheck);
var LFailure: TCheckpointFailure; LMem: TPCAbstractMem; LCount: Integer;
  LBefore: TBytes; LReader: TFileStream; LRaised: Boolean;
begin
  case ACheck of
    cscFactoryFailure: CheckFailure(cfCreate,True);
    cscPartialWrite: CheckFailure(cfPartialWrite,True);
    cscFlushFailure: CheckFailure(cfFlush,True);
    cscValidationFailure: CheckFailure(cfValidation,True);
    cscPublicationFailure: CheckFailure(cfPublish,True);
    cscAbsentTargetFailures: begin
      for LFailure := cfCreate to cfPublish do CheckFailure(LFailure,False);
    end;
    cscSuccessfulRetry: begin
      CheckFailure(cfPartialWrite,True);
      FSource.SaveToFile(FTargetName);
      CheckSavedType(FTargetName,111);
      CheckNoOwnedTemporary;
    end;
    cscMemoryPublication: begin
      SeedFile(FTargetName,111);
      CheckSavedType(FTargetName,111);
      CheckNoOwnedTemporary;
    end;
    cscEmptyPublication: begin
      LMem := TPCAbstractMem.Create('',False);
      try
        LMem.SaveToFile(FTargetName);
      finally
        LMem.Free;
      end;
      Require(TPCAbstractMem.AnalyzeFile(FTargetName,LCount) and (LCount=0),'valid empty checkpoint was rejected');
      CheckNoOwnedTemporary;
    end;
    cscAnalyzeMissing: begin
      LCount := 42;
      Require(not TPCAbstractMem.AnalyzeFile(FTargetName,LCount),'missing checkpoint was accepted');
      Require((LCount=0) and not FileExists(FTargetName),'missing-file analysis mutated filesystem/count');
    end;
    cscAnalyzeTruncated: CheckAnalyze(False);
    cscAnalyzeMalformed: CheckAnalyze(True);
    cscHealthyMainBadCheckpoint: CheckRestore(0);
    cscCorruptMainGoodCheckpoint: CheckRestore(1);
    cscAllCorruptCandidates: CheckRestore(2);
    cscCheckpointThenLocalReplay: CheckReplay;
    cscFailedFlushException: CheckFailedFlush(False);
    cscFailedFlushShortWrite: CheckFailedFlush(True);
    cscUncachedWriteException: CheckUncachedWrite(False);
    cscUncachedShortWrite: CheckUncachedWrite(True);
    cscUncachedFailedClose: CheckUncachedClose;
    cscUncachedPartialRetry: CheckUncachedPartialRetry;
    cscCachedFailedClose: CheckCachedClose;
    cscHealthyMainBadLegacyBody: CheckLegacyBodyRecovery(True);
    cscOlderLegacySurvivesBadNewerBody: CheckLegacyBodyRecovery(False);
    cscUnstableHeaderRetryBeforeLaterPayload: CheckUnstableHeaderRetry;
    cscSharingDenied: begin
      {$IFDEF MSWINDOWS}
      PrepareSource;
      SeedFile(FTargetName,222);
      LBefore := ReadBytes(FTargetName);
      LReader := TFileStream.Create(FTargetName,fmOpenRead+fmShareDenyNone);
      try
        LRaised := False;
        try
          FSource.SaveToFile(FTargetName);
        except
          on E: EOSError do begin
            Require(E.ErrorCode in [5,32],'publication failed outside the Windows sharing boundary');
            LRaised := True;
          end;
        end;
        Require(LRaised,'an existing handle denying deletion did not prevent replacement');
        Require(SameBytes(LBefore,ReadBytes(FTargetName)),'sharing failure modified the previous checkpoint');
      finally
        LReader.Free;
      end;
      CheckNoOwnedTemporary;
      FSource.SaveToFile(FTargetName);
      CheckSavedType(FTargetName,111);
      {$ENDIF}
    end;
  end;
  Require(not TNode.NodeExists,'bank/storage check unexpectedly created the global node');
end;

function RunCheckpointStorageCheck(ACheck: TCheckpointStorageCheck; const ALogPath: String): Integer;
var LFixture: TCheckpointFixture; LLog: TStringList;
begin
  Result := 1;
  LLog := TStringList.Create;
  try
    LLog.Add('CHECK '+GetEnumName(TypeInfo(TCheckpointStorageCheck),Ord(ACheck)));
    try
      LFixture := TCheckpointFixture.Create(ALogPath);
      try
        LFixture.Run(ACheck);
      finally
        LFixture.Free;
      end;
      LLog.Add('PASS');
      Result := 0;
    except
      on E: Exception do begin
        LLog.Add('FAIL '+E.ClassName+': '+E.Message);
      end;
    end;
    LLog.SaveToFile(ALogPath);
  finally
    LLog.Free;
  end;
end;

function RunCheckpointStorageChecks(const ALogPath: String): Integer;
var LCheck: TCheckpointStorageCheck; LLog: TStringList; LCasePath: String;
begin
  Result := 0;
  LLog := TStringList.Create;
  try
    for LCheck := Low(TCheckpointStorageCheck) to High(TCheckpointStorageCheck) do begin
      LCasePath := ChangeFileExt(ALogPath,'.case-'+IntToStr(Ord(LCheck))+'.log');
      if RunCheckpointStorageCheck(LCheck,LCasePath)<>0 then Result := 1;
      LLog.Add(GetEnumName(TypeInfo(TCheckpointStorageCheck),Ord(LCheck))+' '+LCasePath);
      LLog.SaveToFile(ALogPath);
    end;
    if Result=0 then LLog.Add('PASS '+IntToStr(Ord(High(TCheckpointStorageCheck))+1))
    else LLog.Add('FAIL');
    LLog.SaveToFile(ALogPath);
  finally
    LLog.Free;
  end;
end;

end.
