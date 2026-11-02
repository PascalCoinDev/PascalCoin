unit UChangeInfoBatchChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Portable change-type batch checks with generated keys and an in-memory
  safebox. The real preparation, signature and core operation paths are used. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

procedure CheckChangeInfoMixedTargets;
procedure CheckChangeInfoCommonSigner;
procedure CheckChangeInfoSecondarySigner;
procedure CheckChangeInfoSkippedUnusableAccount;
procedure CheckChangeInfoChangedFunding;
procedure CheckChangeInfoPreservedSnapshots;
procedure CheckChangeInfoTargetSelection;
procedure CheckChangeInfoApprovedTargets;
procedure CheckChangeInfoNoChanges;
procedure CheckChangeInfoMalformedRequests;
function RunChangeInfoBatchChecks(const ALogFile: String): Integer;

implementation

uses
  Classes, SysUtils, UMemory, UAccounts, UBlockChain, UOpTransaction,
  UPCDataTypes, UCrypto, UBaseTypes, UConst, UWallet, UCoreObjects, UCoreUtils;

type
  TChangeInfoCheck = procedure;

  TChangeInfoBatchSafeBox = class(TPCSafeBox)
  public
    procedure Build(const AKey: TAccountKey);
    procedure Seed(const AAccount: TAccount);
  end;

  TChangeInfoBatchFixture = class
  private
    FDisposables: TDisposables;
    function CopyAccounts(const AAccounts: TArray<TAccount>): TArray<TAccount>;
    procedure CheckRequestUnchanged(const AAccounts, ASigners: TArray<TAccount>);
  public
    Owner, Other: TECPrivateKey;
    Keys: TWalletKeys;
    Request: TWalletOperationRequest;
    Tree: TOperationsHashTree;
    Error: String;
    constructor Create;
    destructor Destroy; override;
    function Account(ANumber: Cardinal; AType: Word; ABalance: UInt64;
      const AKey: TAccountKey): TAccount;
    procedure Prepare;
    procedure Reject;
    procedure Validate(const ATargets, ASigners, ANonces: array of Cardinal;
      ATypeOnly: Boolean = True);
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create('Change-info batch: ' + AMessage);
end;

procedure TChangeInfoBatchSafeBox.Build(const AKey: TAccountKey);
var
  LBlock: TOperationBlock;
  I: Integer;
begin
  for I := 0 to 159 do
  begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := I;
    LBlock.account_key := AKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := I * CT_NewLineSecondsAvg;
    AddNew(LBlock);
  end;
  DoUpgradeToProtocol5;
  DoUpgradeToProtocol6;
end;

procedure TChangeInfoBatchSafeBox.Seed(const AAccount: TAccount);
begin
  UpdateAccount(AAccount.account, AAccount.accountInfo, AAccount.name,
    AAccount.account_type, AAccount.balance, AAccount.n_operation,
    AAccount.account_data, AAccount.account_seal, aus_transaction_commit,
    0, 0, False, False);
end;

constructor TChangeInfoBatchFixture.Create;
begin
  inherited Create;
  TCrypto.InitCrypto;
  Owner := FDisposables.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  Other := FDisposables.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  Owner.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  Other.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  Keys := FDisposables.AddObject(TWalletKeys.Create(nil)) as TWalletKeys;
  Keys.AddPrivateKey('Generated batch test owner', Owner);
  Request := Default(TWalletOperationRequest);
  Request.OperationType := omtChangeInfo;
  Request.Accounts := TArray<TAccount>.Create(
    Account(100, 7, 1000, Owner.PublicKey),
    Account(101, 7, 1000, Owner.PublicKey),
    Account(102, 7, 1000, Owner.PublicKey));
  Request.SignerAccounts := CopyAccounts(Request.Accounts);
  Request.Fee := 100;
  Request.NewType := 9;
  Request.NewName := TEncoding.ASCII.GetBytes('unused_batch_name');
  Request.SigningMode := akaPrimary;
  Request.PayloadMode := pemNotEncrypt;
  Request.PayloadContent := 'type change';
end;

destructor TChangeInfoBatchFixture.Destroy;
begin
  Tree.Free;
  inherited Destroy;
end;

function TChangeInfoBatchFixture.Account(ANumber: Cardinal; AType: Word;
  ABalance: UInt64; const AKey: TAccountKey): TAccount;
begin
  Result := CT_Account_NUL;
  Result.account := ANumber;
  Result.accountInfo.state := as_Normal;
  Result.accountInfo.accountKey := AKey.GetCopy;
  Result.balance := ABalance;
  Result.n_operation := 7;
  Result.account_type := AType;
  Result.name := TEncoding.ASCII.GetBytes('batch_account_' + IntToStr(ANumber));
  Result.account_data := TEncoding.ASCII.GetBytes('preserved data ' + IntToStr(ANumber));
end;

function TChangeInfoBatchFixture.CopyAccounts(
  const AAccounts: TArray<TAccount>): TArray<TAccount>;
var
  I: Integer;
begin
  SetLength(Result, Length(AAccounts));
  for I := 0 to High(AAccounts) do
    Result[I] := AAccounts[I].GetCopy;
end;

procedure TChangeInfoBatchFixture.CheckRequestUnchanged(
  const AAccounts, ASigners: TArray<TAccount>);
var
  I: Integer;
begin
  Require(Length(AAccounts) = Length(Request.Accounts), 'caller selection length changed');
  Require(Length(ASigners) = Length(Request.SignerAccounts), 'caller signer length changed');
  for I := 0 to High(AAccounts) do
    Require(TAccountComp.EqualAccounts(AAccounts[I], Request.Accounts[I]),
      'caller account snapshot/order changed at ' + IntToStr(I));
  for I := 0 to High(ASigners) do
    Require(TAccountComp.EqualAccounts(ASigners[I], Request.SignerAccounts[I]),
      'caller signer snapshot/order changed at ' + IntToStr(I));
end;

procedure TChangeInfoBatchFixture.Prepare;
var
  LAccounts, LSigners: TArray<TAccount>;
  LResult: Boolean;
begin
  FreeAndNil(Tree);
  LAccounts := CopyAccounts(Request.Accounts);
  LSigners := CopyAccounts(Request.SignerAccounts);
  LResult := TWIZOperationsHelper.PrepareOperations(Request, CT_PROTOCOL_6,
    160, Keys, Tree, Error);
  CheckRequestUnchanged(LAccounts, LSigners);
  Require(LResult, 'preparation rejected a valid request: ' + Error);
  Require(Assigned(Tree), 'successful preparation returned no operations');
  Require(Error = '', 'successful preparation retained an error: ' + Error);
end;

procedure TChangeInfoBatchFixture.Reject;
var
  LAccounts, LSigners: TArray<TAccount>;
  LResult: Boolean;
begin
  FreeAndNil(Tree);
  LAccounts := CopyAccounts(Request.Accounts);
  LSigners := CopyAccounts(Request.SignerAccounts);
  LResult := TWIZOperationsHelper.PrepareOperations(Request, CT_PROTOCOL_6,
    160, Keys, Tree, Error);
  CheckRequestUnchanged(LAccounts, LSigners);
  Require(not LResult, 'invalid request was accepted');
  Require(not Assigned(Tree), 'rejection leaked a partial operation tree');
  Require(Error <> '', 'rejection supplied no reason');
end;

procedure TChangeInfoBatchFixture.Validate(
  const ATargets, ASigners, ANonces: array of Cardinal; ATypeOnly: Boolean);
var
  LDisposables: TDisposables;
  LSafeBox: TChangeInfoBatchSafeBox;
  LTransaction: TPCSafeBoxTransaction;
  LPrevious: TAccountPreviousBlockInfo;
  LSnapshots: TArray<TAccount>;
  LOperation: TOpChangeAccountInfo;
  LBefore, LAfter: TAccount;
  LError: String;
  I, J, LSignerUses: Integer;
  LChanged: Boolean;

  procedure AddSnapshot(const AAccount: TAccount);
  var
    K: Integer;
  begin
    for K := 0 to High(LSnapshots) do
      if LSnapshots[K].account = AAccount.account then
        Exit;
    LSafeBox.Seed(AAccount);
    SetLength(LSnapshots, Length(LSnapshots) + 1);
    LSnapshots[High(LSnapshots)] := LSafeBox.Account(AAccount.account).GetCopy;
  end;

begin
  Require(Length(ATargets) = Length(ASigners), 'invalid expected signer list');
  Require(Length(ATargets) = Length(ANonces), 'invalid expected counter list');
  Require(Tree.OperationsCount = Length(ATargets), 'wrong number of changed targets');
  Require(Tree.TotalFee = Int64(Length(ATargets)) * Request.Fee, 'skipped targets were charged fees');
  LSafeBox := LDisposables.AddObject(TChangeInfoBatchSafeBox.Create) as TChangeInfoBatchSafeBox;
  LSafeBox.Build(Owner.PublicKey);
  for I := 0 to High(Request.Accounts) do
    AddSnapshot(Request.Accounts[I]);
  for I := 0 to High(Request.SignerAccounts) do
    AddSnapshot(Request.SignerAccounts[I]);
  LTransaction := LDisposables.AddObject(TPCSafeBoxTransaction.Create(LSafeBox)) as TPCSafeBoxTransaction;
  LPrevious := LDisposables.AddObject(TAccountPreviousBlockInfo.Create) as TAccountPreviousBlockInfo;
  for I := 0 to Tree.OperationsCount - 1 do
  begin
    Require(Tree.GetOperation(I) is TOpChangeAccountInfo, 'unexpected prepared operation class');
    LOperation := TOpChangeAccountInfo(Tree.GetOperation(I));
    Require(LOperation.DestinationAccount = ATargets[I], 'changed target order differs from selection');
    Require(LOperation.SignerAccount = ASigners[I], 'wrong fee payer');
    Require(LOperation.N_Operation = ANonces[I], 'skipped target consumed an operation counter');
    Require(LOperation.OperationFee = Request.Fee, 'wrong individual fee');
    if ATypeOnly then
    begin
      Require(LOperation.Data.changes_type = [account_type], 'batch changed a name, owner or data');
      Require(LOperation.Data.new_type = Request.NewType, 'wrong requested account type');
    end
    else
      Require(LOperation.Data.changes_type = [account_name], 'name-only change altered another field');
    Require(TCrypto.ECDSAVerify(Owner.PublicKey, LOperation.GetDigestToSign,
      LOperation.Data.sign), 'signature did not verify with the generated owner key');
    Require(not TCrypto.ECDSAVerify(Other.PublicKey, LOperation.GetDigestToSign,
      LOperation.Data.sign), 'unrelated key verified a prepared signature');
    LError := '';
    Require(LOperation.DoOperation(LPrevious, LTransaction, LError),
      'core rejected the signed operation: ' + LError);
  end;
  Require(LTransaction.TotalFee = Tree.TotalFee, 'core charged a different total fee');
  for I := 0 to High(LSnapshots) do
  begin
    LBefore := LSnapshots[I];
    LAfter := LTransaction.Account(LBefore.account);
    LChanged := False;
    LSignerUses := 0;
    for J := 0 to High(ATargets) do
    begin
      LChanged := LChanged or (ATargets[J] = LBefore.account);
      if ASigners[J] = LBefore.account then
        Inc(LSignerUses);
    end;
    if LChanged and ATypeOnly then
      Require(LAfter.account_type = Request.NewType, 'core did not apply the new type')
    else
      Require(LAfter.account_type = LBefore.account_type, 'skipped account type changed');
    if LChanged and not ATypeOnly then
      Require(TBaseType.Equals(Request.NewName, LAfter.name), 'single account name was not changed')
    else
      Require(TBaseType.Equals(LBefore.name, LAfter.name), 'account name changed');
    Require(TBaseType.Equals(LBefore.account_data, LAfter.account_data), 'account data changed');
    Require(TAccountComp.EqualAccountKeys(LBefore.accountInfo.accountKey,
      LAfter.accountInfo.accountKey), 'account owner changed');
    Require(LAfter.balance = LBefore.balance - UInt64(Int64(LSignerUses) * Request.Fee),
      'fee charged to a skipped/non-signing account');
    Require(LAfter.n_operation = LBefore.n_operation + Cardinal(LSignerUses),
      'counter changed outside actual signer use');
    Require(TAccountComp.EqualAccounts(LBefore, LSafeBox.Account(LBefore.account)),
      'replay mutated the committed safebox');
  end;
end;

procedure CheckChangeInfoMixedTargets;
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
  LTypes: array[0..1] of Word;
  LTargets: TArray<Cardinal>;
  LNonces: TArray<Cardinal>;
  I, J, K: Integer;
begin
  LTypes[0] := 0;
  LTypes[1] := High(Word);
  // Keep the already-matching account first in the first case: this is the
  // historical whole-batch rejection, before later changed targets were read.
  for I := 0 to High(LTypes) do
    for J := 0 to 2 do
    begin
      LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
      LFixture.Request.NewType := LTypes[I];
      for K := 0 to 2 do
        LFixture.Request.Accounts[K].account_type := 1;
      LFixture.Request.Accounts[J].account_type := LTypes[I];
      LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
      SetLength(LTargets, 0);
      for K := 0 to 2 do
        if K <> J then
        begin
          SetLength(LTargets, Length(LTargets) + 1);
          LTargets[High(LTargets)] := LFixture.Request.Accounts[K].account;
        end;
      LNonces := TArray<Cardinal>.Create(8, 8);
      LFixture.Prepare;
      LFixture.Validate(LTargets, LTargets, LNonces);
    end;
end;

procedure CheckChangeInfoCommonSigner;
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
begin
  LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
  LFixture.Request.Accounts[0].account_type := LFixture.Request.NewType;
  LFixture.Request.Accounts[0].balance := 200;
  LFixture.Request.Accounts[1].balance := 0;
  LFixture.Request.Accounts[2].balance := 0;
  LFixture.Request.SignerAccounts := TArray<TAccount>.Create(LFixture.Request.Accounts[0]);
  LFixture.Prepare;
  // A matching account may still be the explicitly selected common fee payer.
  LFixture.Validate([101, 102], [100, 100], [8, 9]);
end;

procedure CheckChangeInfoSecondarySigner;
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
begin
  LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
  LFixture.Request.Accounts[0].account_type := LFixture.Request.NewType;
  LFixture.Request.Accounts[0].balance := 0;
  LFixture.Request.Accounts[1].balance := 100;
  LFixture.Request.Accounts[2].balance := 0;
  LFixture.Request.SignerAccounts := TArray<TAccount>.Create(
    LFixture.Account(103, 7, 100, LFixture.Owner.PublicKey));
  LFixture.Request.SigningMode := akaSecondary;
  LFixture.Prepare;
  LFixture.Validate([101, 102], [101, 103], [8, 8]);
end;

procedure CheckChangeInfoSkippedUnusableAccount;
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
begin
  LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
  LFixture.Request.Accounts[0] := LFixture.Account(100, 9, 0, LFixture.Other.PublicKey);
  LFixture.Request.Accounts[0].n_operation := High(Cardinal);
  LFixture.Request.Accounts[0].accountInfo.state := as_ForSale;
  LFixture.Request.Accounts[0].accountInfo.new_publicKey := LFixture.Owner.PublicKey;
  LFixture.Request.Accounts[0].accountInfo.locked_until_block := 200;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Prepare;
  LFixture.Validate([101, 102], [101, 102], [8, 8]);
end;

procedure CheckChangeInfoChangedFunding;
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
begin
  LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
  LFixture.Request.Accounts[0].account_type := LFixture.Request.NewType;
  LFixture.Request.Accounts[1].balance := 100;
  LFixture.Request.Accounts[2].balance := 99;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Reject;
  LFixture.Request.Accounts[2].balance := 100;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Prepare;
  LFixture.Validate([101, 102], [101, 102], [8, 8]);
  LFixture.Request.Accounts[0].balance := 199;
  LFixture.Request.SignerAccounts := TArray<TAccount>.Create(LFixture.Request.Accounts[0]);
  LFixture.Reject;
  LFixture.Request.Accounts[0].balance := 200;
  LFixture.Request.SignerAccounts := TArray<TAccount>.Create(LFixture.Request.Accounts[0]);
  LFixture.Prepare;
  LFixture.Validate([101, 102], [100, 100], [8, 9]);
end;

procedure CheckChangeInfoPreservedSnapshots;
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
  LCallerSelection: TArray<TAccount>;
begin
  LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
  LFixture.Request.Accounts[0].account_type := LFixture.Request.NewType;
  LFixture.Request.Accounts[1].account_type := LFixture.Request.NewType;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LCallerSelection := Copy(LFixture.Request.Accounts);
  LFixture.Prepare;
  // One remaining target is still a batch: a form's name edit must not rename it.
  LFixture.Validate([102], [102], [8]);
  Require(TAccountComp.EqualAccounts(LCallerSelection[0], LFixture.Request.Accounts[0]) and
    TAccountComp.EqualAccounts(LCallerSelection[1], LFixture.Request.Accounts[1]) and
    TAccountComp.EqualAccounts(LCallerSelection[2], LFixture.Request.Accounts[2]),
    'caller-owned selection was changed by filtering');
end;

procedure CheckChangeInfoTargetSelection;
{$IFNDEF CHANGEINFO_BATCH_BASELINE}
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
  LTargets: TArray<TAccount>;
{$ENDIF}
begin
{$IFNDEF CHANGEINFO_BATCH_BASELINE}
  LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
  LFixture.Request.Accounts[1].account_type := LFixture.Request.NewType;
  LTargets := TWIZOperationsHelper.GetChangeAccountInfoTargets(
    LFixture.Request.Accounts, LFixture.Request.NewType);
  Require((Length(LTargets) = 2) and (LTargets[0].account = 100) and
    (LTargets[1].account = 102), 'target selection did not preserve filtered order');
  LTargets[0].account := 999;
  Require(LFixture.Request.Accounts[0].account = 100, 'target selection shared the outer array');
  LFixture.Request.Accounts := TArray<TAccount>.Create(LFixture.Request.Accounts[1]);
  LTargets := TWIZOperationsHelper.GetChangeAccountInfoTargets(
    LFixture.Request.Accounts, LFixture.Request.NewType);
  Require((Length(LTargets) = 1) and (LTargets[0].account = 101),
    'single-account name changes lost their target');
  LTargets[0].account := 999;
  Require(LFixture.Request.Accounts[0].account = 101, 'single target shared the outer array');
  LFixture.Request.Accounts := TArray<TAccount>.Create(
    LFixture.Request.Accounts[0], LFixture.Request.Accounts[0]);
  LTargets := TWIZOperationsHelper.GetChangeAccountInfoTargets(
    LFixture.Request.Accounts, LFixture.Request.NewType);
  Require(Length(LTargets) = 0, 'an all-matching batch retained a target');
  LTargets := TWIZOperationsHelper.GetChangeAccountInfoTargets(nil, 0);
  Require(Length(LTargets) = 0, 'empty selection acquired a target');
{$ENDIF}
end;

procedure CheckChangeInfoApprovedTargets;
{$IFNDEF CHANGEINFO_BATCH_BASELINE}
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
  LTemporary: TAccount;
{$ENDIF}
begin
{$IFNDEF CHANGEINFO_BATCH_BASELINE}
  LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
  LFixture.Request.Accounts[0].account_type := LFixture.Request.NewType;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Request.ApprovedChangeInfoTargets := TWIZOperationsHelper.GetChangeAccountInfoTargets(
    LFixture.Request.Accounts, LFixture.Request.NewType);
  // The approval records identity, not stale balances or operation counters.
  LFixture.Request.ApprovedChangeInfoTargets[0].balance := 0;
  LFixture.Request.ApprovedChangeInfoTargets[0].n_operation := 1;
  LFixture.Prepare;
  LFixture.Validate([101, 102], [101, 102], [8, 8]);
  LTemporary := LFixture.Request.ApprovedChangeInfoTargets[0];
  LFixture.Request.ApprovedChangeInfoTargets[0] := LFixture.Request.ApprovedChangeInfoTargets[1];
  LFixture.Request.ApprovedChangeInfoTargets[1] := LTemporary;
  LFixture.Reject;
  LFixture.Request.ApprovedChangeInfoTargets := TWIZOperationsHelper.GetChangeAccountInfoTargets(
    LFixture.Request.Accounts, LFixture.Request.NewType);
  LFixture.Request.Accounts[1].account_type := LFixture.Request.NewType;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Reject;
  LFixture.Request.Accounts[0].account_type := 7;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Keys.Clear;
  LFixture.Reject;
  Require(Pos('review', LowerCase(LFixture.Error)) > 0,
    'stale approval was checked after private-key lookup');
{$ENDIF}
end;

procedure CheckChangeInfoNoChanges;
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
  I: Integer;
begin
  LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
  for I := 0 to High(LFixture.Request.Accounts) do
    LFixture.Request.Accounts[I].account_type := LFixture.Request.NewType;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Reject;
  LFixture.Request.Accounts := TArray<TAccount>.Create(LFixture.Request.Accounts[0]);
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Request.NewName := Copy(LFixture.Request.Accounts[0].name);
  LFixture.Reject;
  // The single-account editor must still allow a real name-only change.
  LFixture.Request.NewName := TEncoding.ASCII.GetBytes('renamed_single_account');
  LFixture.Prepare;
  Require(LFixture.Tree.OperationsCount = 1, 'single name change was lost');
  Require(TOpChangeAccountInfo(LFixture.Tree.GetOperation(0)).Data.changes_type = [account_name],
    'single name change acquired an unintended type change');
  LFixture.Validate([100], [100], [8], False);
end;

procedure CheckChangeInfoMalformedRequests;
var
  LDisposables: TDisposables;
  LFixture: TChangeInfoBatchFixture;
  LOriginalAccounts, LOriginalSigners: TArray<TAccount>;
begin
  LFixture := LDisposables.AddObject(TChangeInfoBatchFixture.Create) as TChangeInfoBatchFixture;
  LFixture.Request.Accounts[0].account_type := LFixture.Request.NewType;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LOriginalAccounts := Copy(LFixture.Request.Accounts);
  LOriginalSigners := Copy(LFixture.Request.SignerAccounts);
  LFixture.Request.Accounts := TArray<TAccount>.Create(LOriginalAccounts[0], LOriginalAccounts[0]);
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Reject;
  LFixture.Request.Accounts := TArray<TAccount>.Create(LOriginalAccounts[1], LOriginalAccounts[1]);
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Reject;
  LFixture.Request.Accounts := Copy(LOriginalAccounts);
  LFixture.Request.SignerAccounts := nil;
  LFixture.Reject;
  LFixture.Request.SignerAccounts := TArray<TAccount>.Create(LOriginalSigners[0], LOriginalSigners[1]);
  LFixture.Reject;
  LFixture.Request.SignerAccounts := TArray<TAccount>.Create(
    LFixture.Account(103, 7, 1000, LFixture.Other.PublicKey));
  LFixture.Reject;
  LFixture.Request.SignerAccounts := Copy(LOriginalSigners);
  LFixture.Request.Accounts[1].n_operation := High(Cardinal);
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Reject;
  LFixture.Request.Accounts := Copy(LOriginalAccounts);
  LFixture.Request.Accounts[1].accountInfo.state := as_ForSale;
  LFixture.Request.Accounts[1].accountInfo.new_publicKey := LFixture.Other.PublicKey;
  LFixture.Request.Accounts[1].accountInfo.locked_until_block := 200;
  LFixture.Request.SignerAccounts := Copy(LFixture.Request.Accounts);
  LFixture.Reject;
  LFixture.Request.Accounts := Copy(LOriginalAccounts);
  LFixture.Request.SignerAccounts := Copy(LOriginalSigners);
  LFixture.Keys.Clear;
  LFixture.Keys.AddPublicKey('Generated watch-only key', LFixture.Owner.PublicKey);
  LFixture.Reject;
end;

function RunChangeInfoBatchChecks(const ALogFile: String): Integer;
var
  LDisposables: TDisposables;
  LLog: TStringList;

  procedure Run(const AName: String; ACheck: TChangeInfoCheck);
  begin
    ACheck;
    LLog.Add('PASS ' + AName);
    LLog.SaveToFile(ALogFile);
  end;

begin
  LLog := LDisposables.AddObject(TStringList.Create) as TStringList;
  Result := 1;
  try
    Run('mixed-target-order-and-type-boundaries', CheckChangeInfoMixedTargets);
    Run('common-signer-skipped-target-fees-and-counters', CheckChangeInfoCommonSigner);
    Run('secondary-signer-own-funds-before-fallback', CheckChangeInfoSecondarySigner);
    Run('skipped-watch-only-locked-exhausted-target', CheckChangeInfoSkippedUnusableAccount);
    Run('only-changed-targets-require-funding', CheckChangeInfoChangedFunding);
    Run('caller-snapshots-and-batch-names-preserved', CheckChangeInfoPreservedSnapshots);
{$IFNDEF CHANGEINFO_BATCH_BASELINE}
    Run('target-selection-order-and-independent-array', CheckChangeInfoTargetSelection);
    Run('approved-target-identities-and-stale-set', CheckChangeInfoApprovedTargets);
{$ENDIF}
    Run('all-unchanged-and-single-no-op-rejected', CheckChangeInfoNoChanges);
    Run('duplicates-missing-signers-and-invalid-changed-targets', CheckChangeInfoMalformedRequests);
    LLog.Add('PASS complete change-info batch preparation checks');
    Result := 0;
  except
    on E: Exception do
      LLog.Add('FAIL ' + E.ClassName + ': ' + E.Message);
  end;
  LLog.SaveToFile(ALogFile);
end;

end.
