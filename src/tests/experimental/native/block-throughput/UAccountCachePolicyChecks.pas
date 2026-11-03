unit UAccountCachePolicyChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Existing cache configuration must survive storage replacement. Fixtures use
  generated metadata and new TEMP files; they do not test block consensus. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}
{$I ..\..\..\..\config.inc}

interface

procedure CheckDefaultAccountCacheLimit;
procedure CheckAccountCacheLimitSurvivesMemoryReload;
procedure CheckAccountCacheLimitSurvivesFileReloadAndClear;
procedure CheckFailedSafeboxReplacementPreservesFinallyException;
procedure CheckFailedSafeboxReplacementPreservesDisposalException;
procedure CheckFailedSafeboxConstructionPreservesException;
procedure CheckNormalSafeboxClearRetainsItsExistingSemantics;

implementation

uses Classes, SysUtils, UAccounts, UPCDataTypes, UCrypto, UConst, UMemory;

type
  TCachePolicySafeBox = class(TPCSafeBox)
    procedure Build(const AKey: TAccountKey; AType: Word);
  end;

  TFailedReplacementSafeBox = class(TPCSafeBox)
    constructor Create(const ALockedFile: String); reintroduce;
  end;

  TFailureOwnerMode = (fomFinally, fomDisposables, fomConstructor);

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create('Account cache policy: '+AMessage);
end;

procedure TCachePolicySafeBox.Build(const AKey: TAccountKey; AType: Word);
var B: TOperationBlock; A: TAccount;
begin
  B := CT_OperationBlock_NUL;
  B.account_key := AKey;
  B.protocol_version := CT_PROTOCOL_1;
  B.protocol_available := CT_PROTOCOL_6;
  B.compact_target := CT_MinCompactTarget_v1;
  B.reward := TPascalCoinProtocol.GetRewardForNewLine(0);
  AddNew(B);
  A := Account(0);
  A.account_type := AType;
  UpdateAccount(A.account,A.accountInfo,A.name,A.account_type,A.balance,
    A.n_operation,A.account_data,A.account_seal,aus_transaction_commit,
    A.updated_on_block_passive_mode,A.updated_on_block_active_mode,False,False);
end;

constructor TFailedReplacementSafeBox.Create(const ALockedFile: String);
begin
  inherited Create;
  // A failed descendant constructor invokes the same inherited destructor as
  // a caller disposing an already-created safebox after a failed replacement.
  SetSafeboxFileName(ALockedFile);
end;

procedure FailReplacementWithFinally(const ALockedFile: String);
var S: TPCSafeBox;
begin
  S := TPCSafeBox.Create;
  try
    S.SetSafeboxFileName(ALockedFile);
  finally
    S.Free;
  end;
end;

procedure FailReplacementWithDisposables(const ALockedFile: String);
var D: TDisposables;
  S: TPCSafeBox;
begin
  S := D.AddObject(TPCSafeBox.Create) as TPCSafeBox;
  S.SetSafeboxFileName(ALockedFile);
end;

procedure FailReplacementDuringConstruction(const ALockedFile: String);
var S: TPCSafeBox;
begin
  S := TFailedReplacementSafeBox.Create(ALockedFile);
  S.Free;
end;

procedure CheckFailedReplacement(AOwnerMode: TFailureOwnerMode);
const CMarker: array[0..7] of Byte = ($51,$41,$2D,$4F,$57,$4E,$45,$44);
var ID: TGUID;
  Directory, FileName, ExpectedClass, ExpectedMessage, ActualClass, ActualMessage: String;
  Blocker, Unexpected: TFileStream;
  LReadBack: array[0..7] of Byte;
begin
  TCrypto.InitCrypto;
  Require(CreateGUID(ID)=0,'cannot create replacement-failure identity');
  Directory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))+'SafeboxCleanup-'+GUIDToString(ID);
  Require(not DirectoryExists(Directory),'refusing an existing cleanup fixture');
  Require(ForceDirectories(Directory),'cannot create cleanup fixture');
  FileName := IncludeTrailingPathDelimiter(Directory)+'locked.safebox';
  Blocker := TFileStream.Create(FileName,fmCreate or fmShareExclusive);
  try
    Blocker.WriteBuffer(CMarker,SizeOf(CMarker));
    ExpectedClass := '';
    ExpectedMessage := '';
    try
      // Match the production file-open mode and retain its actual localized OS
      // error. The fixture never exhausts the disk or touches a user data file.
      Unexpected := TFileStream.Create(FileName,fmOpenReadWrite+fmShareDenyWrite);
      Unexpected.Free;
    except
      on E: EFOpenError do begin
        ExpectedClass := E.ClassName;
        ExpectedMessage := E.Message;
      end;
    end;
    Require(ExpectedClass<>'','the exclusive fixture did not reject another writer');
    ActualClass := '';
    ActualMessage := '';
    try
      case AOwnerMode of
        fomFinally: FailReplacementWithFinally(FileName);
        fomDisposables: FailReplacementWithDisposables(FileName);
        fomConstructor: FailReplacementDuringConstruction(FileName);
      end;
    except
      on E: Exception do begin
        ActualClass := E.ClassName;
        ActualMessage := E.Message;
      end;
    end;
    Require(ActualClass=ExpectedClass,'cleanup replaced '+ExpectedClass+' with '+ActualClass+': '+ActualMessage);
    Require(ActualMessage=ExpectedMessage,'cleanup replaced the original file-open diagnostic');
    Require(Blocker.Size=SizeOf(CMarker),'failed replacement resized the locked file');
    Blocker.Position := 0;
    Blocker.ReadBuffer(LReadBack,SizeOf(LReadBack));
    Require(CompareMem(@LReadBack[0],@CMarker[0],SizeOf(CMarker)),'failed replacement changed locked-file bytes');
  finally
    Blocker.Free;
  end;
end;

procedure CheckFailedSafeboxReplacementPreservesFinallyException;
begin
  CheckFailedReplacement(fomFinally);
end;

procedure CheckFailedSafeboxReplacementPreservesDisposalException;
begin
  CheckFailedReplacement(fomDisposables);
end;

procedure CheckFailedSafeboxConstructionPreservesException;
begin
  CheckFailedReplacement(fomConstructor);
end;

procedure CheckNormalSafeboxClearRetainsItsExistingSemantics;
var S: TCachePolicySafeBox;
  Key: TECPrivateKey;
begin
  TCrypto.InitCrypto;
  Key := TECPrivateKey.Create;
  try
    Key.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    S := TCachePolicySafeBox.Create;
    try
      S.PCAbstractMem.AccountCache.MaxRegisters := 250000;
      S.Build(Key.PublicKey,321);
      Require(S.GetMinimumAvailableSnapshotBlock>=0,'normal fixture did not create owned snapshot records');
      S.Clear;
      Require(S.GetMinimumAvailableSnapshotBlock=-1,'Clear retained owned snapshot records');
      // Existing AbstractMem Clear resets metadata/snapshots; it does not erase
      // the current backend's records. Preserve that public behavior exactly.
      Require((S.BlocksCount=1) and (S.Account(0).account_type=321),'Clear changed the existing AbstractMem record semantics');
      Require((S.TotalBalance=0) and (S.WorkSum=0) and (S.CurrentProtocol=CT_PROTOCOL_1),'Clear no longer resets public summary metadata');
      Require(S.PCAbstractMem.AccountCache.MaxRegisters=250000,'Clear changed the configured cache policy');
      S.Clear;
      Require(S.GetMinimumAvailableSnapshotBlock=-1,'repeated Clear recreated snapshots');
    finally
      S.Free;
    end;
  finally
    Key.Free;
  end;
end;

procedure CheckDefaultAccountCacheLimit;
var S: TPCSafeBox;
begin
  TCrypto.InitCrypto;
  S := TPCSafeBox.Create;
  try
    Require(S.PCAbstractMem.AccountCache.MaxRegisters=50000,
      'core/classic default changed');
  finally S.Free; end;
end;

procedure CheckAccountCacheLimitSurvivesMemoryReload;
var S: TPCSafeBox;
begin
  TCrypto.InitCrypto;
  S := TPCSafeBox.Create;
  try
    S.PCAbstractMem.AccountCache.MaxRegisters := 250000;
    S.SetSafeboxFileName('');
    Require(S.PCAbstractMem.AccountCache.MaxRegisters=250000,
      'memory replacement lost the configured limit');
    S.Clear;
    Require(S.PCAbstractMem.AccountCache.MaxRegisters=250000,
      'safebox clear lost the configured limit');
    S.PCAbstractMem.AccountCache.MaxRegisters := 0;
    S.SetSafeboxFileName('');
    Require(S.PCAbstractMem.AccountCache.MaxRegisters=0,
      'the existing zero-limit configuration was replaced by a default');
  finally S.Free; end;
end;

procedure CheckAccountCacheLimitSurvivesFileReloadAndClear;
var
  D: TDisposables;
  Key: TECPrivateKey;
  Source,Target: TCachePolicySafeBox;
  ID: TGUID;
  Directory,FirstFile,SecondFile: String;

  procedure SaveFixture(const AFile: String; AType: Word);
  begin
    Require(not FileExists(AFile),'refusing an existing fixture file');
    Source := TCachePolicySafeBox.Create;
    try
      Source.Build(Key.PublicKey,AType);
      Source.SaveCheckpointing(AFile);
    finally Source.Free; end;
  end;

begin
  TCrypto.InitCrypto;
  CreateGUID(ID);
  Directory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))+
    'AccountCachePolicy-'+GUIDToString(ID);
  Require(not DirectoryExists(Directory),'refusing an existing fixture directory');
  Require(ForceDirectories(Directory),'cannot create fixture directory');
  FirstFile := IncludeTrailingPathDelimiter(Directory)+'first.safebox';
  SecondFile := IncludeTrailingPathDelimiter(Directory)+'second.safebox';
  Key := D.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  Key.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  SaveFixture(FirstFile,111);
  SaveFixture(SecondFile,222);
  Target := D.AddObject(TCachePolicySafeBox.Create) as TCachePolicySafeBox;
  Target.PCAbstractMem.AccountCache.MaxRegisters := 250000;
  Target.SetSafeboxFileName(FirstFile);
  Require((Target.BlocksCount=1) and (Target.Account(0).account_type=111),
    'first saved fixture did not reopen');
  Require(Target.PCAbstractMem.AccountCache.MaxRegisters=250000,
    'file replacement lost the configured limit');
  Target.SetSafeboxFileName(SecondFile);
  Require((Target.BlocksCount=1) and (Target.Account(0).account_type=222),
    'old cache entries leaked into a different storage file');
  Require(Target.PCAbstractMem.AccountCache.MaxRegisters=250000,
    'second file replacement lost the configured limit');
  // The core clear API deletes/recreates only this newly generated fixture.
  Target.ClearSafeboxfile;
  Require((Target.BlocksCount=0) and
    (Target.PCAbstractMem.AccountCache.MaxRegisters=250000),
    'file clear failed to retain the bounded cache policy');
  Require(FileExists(FirstFile),'clearing the second fixture touched the first');
end;

end.
