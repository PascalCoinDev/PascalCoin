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

implementation

uses Classes, SysUtils, UAccounts, UPCDataTypes, UCrypto, UConst, UMemory;

type
  TCachePolicySafeBox = class(TPCSafeBox)
    procedure Build(const AKey: TAccountKey; AType: Word);
  end;

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
