unit UChangeAccountInfoReplayChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Actual signed operations on generated in-memory accounts. No node, listener,
  existing keys or replacement operation implementation is used. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

procedure CheckChangeInfoSelfTarget;
procedure CheckChangeInfoDistinctTarget;
procedure CheckChangeInfoRepeatedAndRollback;
procedure CheckChangeInfoRejectsWithoutMutation;

implementation

uses Classes, SysUtils, UMemory, UAccounts, UBlockChain, UOpTransaction,
  UPCDataTypes, UCrypto, UBaseTypes, UConst;

type
  TChangeInfoSafeBox = class(TPCSafeBox)
  public
    procedure Build(const AKey: TAccountKey);
    procedure Seed(const AAccount: TAccount);
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create('Change-info replay: '+AMessage);
end;

procedure TChangeInfoSafeBox.Build(const AKey: TAccountKey);
var LBlock: TOperationBlock; I: Integer;
begin
  for I := 0 to 159 do begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := I;
    LBlock.account_key := AKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := I*CT_NewLineSecondsAvg;
    AddNew(LBlock);
  end;
  DoUpgradeToProtocol5;
  DoUpgradeToProtocol6;
end;

procedure TChangeInfoSafeBox.Seed(const AAccount: TAccount);
begin
  UpdateAccount(AAccount.account,AAccount.accountInfo,AAccount.name,
    AAccount.account_type,AAccount.balance,AAccount.n_operation,
    AAccount.account_data,AAccount.account_seal,aus_transaction_commit,
    0,0,False,False);
end;

function MakeAccount(ANumber: Cardinal; const AKey: TAccountKey;
  const AName: String): TAccount;
begin
  Result := CT_Account_NUL;
  Result.account := ANumber;
  Result.accountInfo.state := as_Normal;
  Result.accountInfo.accountKey := AKey.GetCopy;
  Result.balance := 100000;
  Result.n_operation := 7;
  Result.account_type := 42;
  Result.name := TEncoding.ASCII.GetBytes(AName);
  Result.account_data := TEncoding.ASCII.GetBytes('original data');
end;

function ExpectedSeal(const ABefore: TAccount; AOperation: TPCOperation;
  const ASafeboxHash: TRawBytes): TRawBytes;
var LStream: TMemoryStream; LAccount: TAccount; LOpID: TRawBytes;
begin
  // Independent byte-level oracle for the protocol's pre-change account seal.
  LStream := TMemoryStream.Create;
  try
    LAccount := ABefore.GetCopy;
    LAccount.SerializeAccount(LStream,CT_PROTOCOL_6);
    LOpID := AOperation.GetOpID;
    if Length(LOpID)>0 then LStream.WriteBuffer(LOpID[0],Length(LOpID));
    if Length(ASafeboxHash)>0 then LStream.WriteBuffer(ASafeboxHash[0],Length(ASafeboxHash));
    Result := TCrypto.DoRipeMD160AsRaw(TCrypto.DoSha256(LStream.Memory,LStream.Size));
  finally LStream.Free; end;
end;

procedure CheckCase(AKind: Integer);
var
  LDisposables: TDisposables;
  LOwner,LNextOwner: TECPrivateKey;
  LSafeBox: TChangeInfoSafeBox;
  LTransaction: TPCSafeBoxTransaction;
  LPrevious: TAccountPreviousBlockInfo;
  LOperation,LSecond: TPCOperation;
  LBaseSigner,LBaseTarget,LSigner,LTarget,LFirstResult,LBeforeSecond: TAccount;
  LNewName,LNewData,LSafeboxHash: TRawBytes;
  LTargetNumber,LOperationNumber: Cardinal;
  LError: String;
begin
  TCrypto.InitCrypto;
  LOwner := LDisposables.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  LNextOwner := LDisposables.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  LOwner.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  LNextOwner.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  LSafeBox := LDisposables.AddObject(TChangeInfoSafeBox.Create) as TChangeInfoSafeBox;
  LSafeBox.Build(LOwner.PublicKey);
  LSafeBox.Seed(MakeAccount(100,LOwner.PublicKey,'original_signer'));
  LSafeBox.Seed(MakeAccount(101,LOwner.PublicKey,'original_target'));
  LBaseSigner := LSafeBox.Account(100);
  LTargetNumber := 100;
  if AKind=1 then LTargetNumber := 101;
  LBaseTarget := LSafeBox.Account(LTargetNumber);
  LSafeboxHash := Copy(LSafeBox.SafeBoxHash);
  LTransaction := LDisposables.AddObject(TPCSafeBoxTransaction.Create(LSafeBox)) as TPCSafeBoxTransaction;
  LPrevious := LDisposables.AddObject(TAccountPreviousBlockInfo.Create) as TAccountPreviousBlockInfo;
  LNewName := TEncoding.ASCII.GetBytes('replayed_account');
  LNewData := TEncoding.ASCII.GetBytes('new account data');
  LOperationNumber := 8;
  if AKind=3 then LOperationNumber := 99;
  LOperation := LDisposables.AddObject(TOpChangeAccountInfo.CreateChangeAccountInfo(
    CT_PROTOCOL_6,100,LOperationNumber,LTargetNumber,LOwner,
    True,LNextOwner.PublicKey,True,LNewName,True,99,True,LNewData,
    100,CT_TOperationPayload_NUL)) as TPCOperation;
  if AKind=3 then begin
    Require(not LOperation.DoOperation(LPrevious,LTransaction,LError),
      'an invalid operation number must be rejected');
    Require(LError='Invalid n_operation','wrong rejection reason: '+LError);
    Require(LTransaction.TotalFee=0,'rejection changed transaction fees');
    Require(TAccountComp.EqualAccounts(LBaseSigner,LTransaction.Account(100)),
      'a rejected self-change altered its transaction account');
  end else begin
    Require(LOperation.DoOperation(LPrevious,LTransaction,LError),LError);
    LSigner := LTransaction.Account(100);
    LTarget := LTransaction.Account(LTargetNumber);
    Require((LSigner.n_operation=8) and (LSigner.balance=99900),
      'signer nonce or fee changed more than once');
    Require(LTransaction.TotalFee=100,'the operation fee must be applied exactly once');
    Require(TAccountComp.EqualAccountKeys(LNextOwner.PublicKey,LTarget.accountInfo.accountKey),
      'target owner was not changed');
    Require(TBaseType.Equals(LNewName,LTarget.name) and
      TBaseType.Equals(LNewData,LTarget.account_data) and (LTarget.account_type=99),
      'target name/data/type were not applied');
    Require(TBaseType.Equals(ExpectedSeal(LBaseSigner,LOperation,LSafeboxHash),LSigner.account_seal),
      'signer seal differs from its original account bytes');
    Require(TBaseType.Equals(ExpectedSeal(LBaseTarget,LOperation,LSafeboxHash),LTarget.account_seal),
      'target seal differs from its original account bytes');
    if AKind=1 then begin
      Require((LTarget.balance=LBaseTarget.balance) and (LTarget.n_operation=LBaseTarget.n_operation),
        'a distinct target was charged the signer fee or nonce');
      Require(TAccountComp.EqualAccountKeys(LOwner.PublicKey,LSigner.accountInfo.accountKey) and
        TBaseType.Equals(LBaseSigner.name,LSigner.name) and
        TBaseType.Equals(LBaseSigner.account_data,LSigner.account_data) and
        (LSigner.account_type=LBaseSigner.account_type),
        'a distinct target change leaked into its signer');
    end;
    if AKind=2 then begin
      LFirstResult := LTarget.GetCopy;
      LBeforeSecond := LTarget.GetCopy;
      LSecond := LDisposables.AddObject(TOpChangeAccountInfo.CreateChangeAccountInfo(
        CT_PROTOCOL_6,100,9,100,LNextOwner,False,CT_TECDSA_Public_Nul,
        True,TEncoding.ASCII.GetBytes('second_edit'),True,100,False,nil,
        200,CT_TOperationPayload_NUL)) as TPCOperation;
      Require(LSecond.DoOperation(LPrevious,LTransaction,LError),LError);
      LTarget := LTransaction.Account(100);
      Require((LTarget.n_operation=9) and (LTarget.balance=99700) and
        (LTransaction.TotalFee=300),'a repeated edit ignored the pending account state');
      Require(TBaseType.Equals(ExpectedSeal(LBeforeSecond,LSecond,LSafeboxHash),LTarget.account_seal),
        'a repeated edit sealed stale base state');
      LTransaction.Rollback;
      LPrevious.Clear;
      Require(TAccountComp.EqualAccounts(LBaseSigner,LTransaction.Account(100)) and
        (LTransaction.TotalFee=0),'rollback did not restore the original account');
      Require(LOperation.DoOperation(LPrevious,LTransaction,LError),LError);
      Require(TAccountComp.EqualAccounts(LFirstResult,LTransaction.Account(100)),
        'replay after rollback changed the accepted account result');
    end;
  end;
  Require(TAccountComp.EqualAccounts(LBaseSigner,LSafeBox.Account(100)) and
    TAccountComp.EqualAccounts(LBaseTarget,LSafeBox.Account(LTargetNumber)) and
    TBaseType.Equals(LSafeboxHash,LSafeBox.SafeBoxHash),
    'transactional replay mutated the underlying safebox or hash');
end;

procedure CheckChangeInfoSelfTarget;
begin
  CheckCase(0);
end;

procedure CheckChangeInfoDistinctTarget;
begin
  CheckCase(1);
end;

procedure CheckChangeInfoRepeatedAndRollback;
begin
  CheckCase(2);
end;

procedure CheckChangeInfoRejectsWithoutMutation;
begin
  CheckCase(3);
end;

end.
