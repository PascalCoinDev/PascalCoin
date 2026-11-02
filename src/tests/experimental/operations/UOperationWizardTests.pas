unit UOperationWizardTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Operation wizard tests use generated, memory-only wallet keys. They never
  load the global wallet, initialize a node, or submit operations. }

{$mode delphi}
{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, UCrypto, UWallet, UPCDataTypes,
  UWIZOperation, UCoreObjects;

type
  TOperationWizardTests = class(TTestCase)
  private
    FKeys: TWalletKeys;
    FKey: TECPrivateKey;
    FOtherKey: TECPrivateKey;
    FUnlockCalls: Integer;
    procedure Unlock(Sender: TObject);
    procedure CancelUnlock(Sender: TObject);
    procedure WrongPassword(Sender: TObject);
    procedure UnlockThenRaise(Sender: TObject);
    procedure ProtectWallet;
    function Account(ANumber: Cardinal; ABalance: UInt64; const AKey: TAccountKey): TAccount;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure PasswordlessWalletDoesNotAcquireUnlock;
    procedure CanceledUnlockStaysLocked;
    procedure WrongPasswordStaysLocked;
    procedure AcquiredUnlockRelocksOnce;
    procedure ExistingUnlockedWalletKeepsItsState;
    procedure SeparateModelsKeepIndependentOwnership;
    procedure ModelDisposalRelocksOwnedUnlock;
    procedure WalletDisposalInvalidatesBorrowedReference;
    procedure CallbackFailureRetainsUnlockOwnership;
    procedure CommonSignerMustOwnEveryTarget;
    procedure CommonSignerMustCoverCumulativeFees;
    procedure LockedSignerIsExcluded;
    procedure SecondaryModeOnlyFundsPoorTargets;
    procedure SecondaryModeIncludesPayersOwnFee;
    procedure NegativeFeeHasNoSignerCandidates;
    procedure FeeAboveProtocolLimitHasNoSignerCandidates;
    procedure LockedTargetCannotUseSecondaryMode;
    procedure ImmatureSignerIsExcluded;
    procedure ImmatureTargetCannotUseSecondaryMode;
  end;

implementation

uses UConst;

procedure TOperationWizardTests.SetUp;
begin
  inherited SetUp;
  FKey := nil;
  FOtherKey := nil;
  FKeys := nil;
  try
    TCrypto.InitCrypto;
    FKey := TECPrivateKey.Create;
    FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    FOtherKey := TECPrivateKey.Create;
    FOtherKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    FKeys := TWalletKeys.Create(nil);
    FKeys.AddPrivateKey('Generated test key', FKey);
    // Establish password validity through the same decryption path as loading
    // a wallet stream. An empty password remains valid and does not lock it.
    FKeys.LockWallet;
    FUnlockCalls := 0;
  except
    FreeAndNil(FKeys);
    FreeAndNil(FOtherKey);
    FreeAndNil(FKey);
    raise;
  end;
end;

procedure TOperationWizardTests.TearDown;
begin
  FKeys.Free;
  FOtherKey.Free;
  FKey.Free;
  inherited TearDown;
end;

procedure TOperationWizardTests.ProtectWallet;
begin
  FKeys.WalletPassword := 'operation-test-password';
  AssertTrue('Generated wallet locks', FKeys.LockWallet);
end;

procedure TOperationWizardTests.Unlock(Sender: TObject);
begin
  Inc(FUnlockCalls);
  FKeys.WalletPassword := 'operation-test-password';
end;

procedure TOperationWizardTests.CancelUnlock(Sender: TObject);
begin
  Inc(FUnlockCalls);
end;

procedure TOperationWizardTests.WrongPassword(Sender: TObject);
begin
  Inc(FUnlockCalls);
  FKeys.WalletPassword := 'incorrect-password';
end;

procedure TOperationWizardTests.UnlockThenRaise(Sender: TObject);
begin
  Unlock(Sender);
  raise Exception.Create('Simulated dialog failure after unlocking');
end;

function TOperationWizardTests.Account(ANumber: Cardinal; ABalance: UInt64; const AKey: TAccountKey): TAccount;
begin
  Result := CT_Account_NUL;
  Result.account := ANumber;
  Result.balance := ABalance;
  Result.accountInfo.state := as_Normal;
  Result.accountInfo.accountKey := AKey;
end;

procedure TOperationWizardTests.PasswordlessWalletDoesNotAcquireUnlock;
var LScope: TWalletUnlockScope; LMessage: String;
begin
  LScope := TWalletUnlockScope.Create(nil);
  try
    AssertTrue(LScope.TryAcquire(FKeys, Unlock, LMessage));
    AssertFalse(LScope.OwnsUnlock);
    LScope.Release;
    AssertTrue(FKeys.IsValidPassword);
    AssertEquals(0, FUnlockCalls);
    AssertTrue(Assigned(FKeys.Key[0].PrivateKey));
  finally
    LScope.Free;
  end;
end;

procedure TOperationWizardTests.CanceledUnlockStaysLocked;
var LScope: TWalletUnlockScope; LMessage: String;
begin
  ProtectWallet;
  LScope := TWalletUnlockScope.Create(nil);
  try
    AssertFalse(LScope.TryAcquire(FKeys, CancelUnlock, LMessage));
    AssertTrue('Rejection explains the locked wallet', LMessage <> '');
    AssertFalse(LScope.OwnsUnlock);
    AssertFalse(FKeys.IsValidPassword);
    AssertEquals(1, FUnlockCalls);
  finally
    LScope.Free;
  end;
end;

procedure TOperationWizardTests.WrongPasswordStaysLocked;
var LScope: TWalletUnlockScope; LMessage: String;
begin
  ProtectWallet;
  LScope := TWalletUnlockScope.Create(nil);
  try
    AssertFalse(LScope.TryAcquire(FKeys, WrongPassword, LMessage));
    AssertFalse(LScope.OwnsUnlock);
    AssertFalse(Assigned(FKeys.Key[0].PrivateKey));
  finally
    LScope.Free;
  end;
end;

procedure TOperationWizardTests.AcquiredUnlockRelocksOnce;
var LScope: TWalletUnlockScope; LMessage: String;
begin
  ProtectWallet;
  LScope := TWalletUnlockScope.Create(nil);
  try
    AssertTrue(LScope.TryAcquire(FKeys, Unlock, LMessage));
    AssertTrue(LScope.OwnsUnlock);
    AssertTrue(Assigned(FKeys.Key[0].PrivateKey));
    AssertTrue('Retry does not reprompt', LScope.TryAcquire(FKeys, Unlock, LMessage));
    AssertEquals(1, FUnlockCalls);
    LScope.Release;
    AssertFalse(FKeys.IsValidPassword);
    FKeys.WalletPassword := 'operation-test-password';
    LScope.Release;
    AssertTrue('Repeated release does not lock another unlock', FKeys.IsValidPassword);
  finally
    LScope.Free;
  end;
end;

procedure TOperationWizardTests.ExistingUnlockedWalletKeepsItsState;
var LModel: TWIZOperationsModel; LMessage: String;
begin
  ProtectWallet;
  Unlock(nil);
  LModel := TWIZOperationsModel.Create(nil, omtSendPasc);
  try
    AssertTrue(LModel.TryUnlockWallet(FKeys, CancelUnlock, LMessage));
    AssertFalse(LModel.UnlockScope.OwnsUnlock);
  finally
    LModel.Free;
  end;
  AssertTrue(FKeys.IsValidPassword);
  AssertEquals(1, FUnlockCalls);
end;

procedure TOperationWizardTests.SeparateModelsKeepIndependentOwnership;
var LFirst, LSecond: TWIZOperationsModel; LMessage: String;
begin
  ProtectWallet;
  LFirst := TWIZOperationsModel.Create(nil, omtSendPasc);
  try
    AssertTrue(LFirst.TryUnlockWallet(FKeys, Unlock, LMessage));
    LSecond := TWIZOperationsModel.Create(nil, omtChangeKey);
    try
      AssertTrue(LSecond.TryUnlockWallet(FKeys, CancelUnlock, LMessage));
      AssertFalse(LSecond.UnlockScope.OwnsUnlock);
      AssertTrue('Creating another wizard preserves ownership', LFirst.UnlockScope.OwnsUnlock);
    finally
      LSecond.Free;
    end;
    AssertTrue(FKeys.IsValidPassword);
    LFirst.RelockWallet;
    AssertFalse(FKeys.IsValidPassword);
  finally
    LFirst.Free;
  end;
end;

procedure TOperationWizardTests.ModelDisposalRelocksOwnedUnlock;
var LModel: TWIZOperationsModel; LMessage: String;
begin
  ProtectWallet;
  LModel := TWIZOperationsModel.Create(nil, omtSendPasc);
  try
    AssertTrue(LModel.TryUnlockWallet(FKeys, Unlock, LMessage));
  finally
    LModel.Free;
  end;
  AssertFalse(FKeys.IsValidPassword);
  AssertFalse(Assigned(FKeys.Key[0].PrivateKey));
end;

procedure TOperationWizardTests.WalletDisposalInvalidatesBorrowedReference;
var LScope: TWalletUnlockScope; LMessage: String;
begin
  ProtectWallet;
  LScope := TWalletUnlockScope.Create(nil);
  try
    AssertTrue(LScope.TryAcquire(FKeys, Unlock, LMessage));
    FreeAndNil(FKeys);
    AssertFalse(LScope.OwnsUnlock);
    LScope.Release;
  finally
    LScope.Free;
  end;
end;

procedure TOperationWizardTests.CallbackFailureRetainsUnlockOwnership;
var LScope: TWalletUnlockScope; LMessage: String; LRaised: Boolean;
begin
  ProtectWallet;
  LScope := TWalletUnlockScope.Create(nil);
  try
    LRaised := False;
    try
      LScope.TryAcquire(FKeys, UnlockThenRaise, LMessage);
    except
      on E: Exception do LRaised := True;
    end;
    AssertTrue(LRaised);
    AssertTrue(LScope.OwnsUnlock);
  finally
    LScope.Free;
  end;
  AssertFalse(FKeys.IsValidPassword);
end;

procedure TOperationWizardTests.CommonSignerMustOwnEveryTarget;
var LCandidates, LTargets, LResult: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(1, 100, FKey.PublicKey), Account(2, 100, FOtherKey.PublicKey));
  LTargets := TArray<TAccount>.Create(Account(3, 0, FKey.PublicKey));
  LResult := TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaPrimary);
  AssertEquals(1, Length(LResult));
  AssertEquals(1, Integer(LResult[0].account));
  LTargets := TArray<TAccount>.Create(Account(3, 0, FKey.PublicKey), Account(4, 0, FOtherKey.PublicKey));
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaPrimary)));
end;

procedure TOperationWizardTests.CommonSignerMustCoverCumulativeFees;
var LCandidates, LTargets: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(1, 19, FKey.PublicKey));
  LTargets := TArray<TAccount>.Create(Account(3, 0, FKey.PublicKey), Account(4, 0, FKey.PublicKey));
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaPrimary)));
  LCandidates[0].balance := 20;
  AssertEquals(1, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaPrimary)));
end;

procedure TOperationWizardTests.LockedSignerIsExcluded;
var LCandidates, LTargets: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(1, 100, FKey.PublicKey));
  LCandidates[0].accountInfo.state := as_ForSale;
  LCandidates[0].accountInfo.locked_until_block := 1000;
  LTargets := TArray<TAccount>.Create(Account(3, 0, FKey.PublicKey));
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaPrimary)));
  AssertEquals(1, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1001, 10, akaPrimary)));
end;

procedure TOperationWizardTests.SecondaryModeOnlyFundsPoorTargets;
var LCandidates, LTargets: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(1, 10, FKey.PublicKey));
  LTargets := TArray<TAccount>.Create(Account(3, 0, FKey.PublicKey), Account(4, 10, FOtherKey.PublicKey));
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaPrimary)));
  AssertEquals(1, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaSecondary)));
end;

procedure TOperationWizardTests.SecondaryModeIncludesPayersOwnFee;
var LCandidates, LTargets: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(1, 10, FKey.PublicKey));
  LTargets := TArray<TAccount>.Create(LCandidates[0], Account(4, 0, FKey.PublicKey));
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaSecondary)));
  LCandidates[0].balance := 20;
  LTargets[0] := LCandidates[0];
  AssertEquals(1, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaSecondary)));
end;

procedure TOperationWizardTests.NegativeFeeHasNoSignerCandidates;
var LCandidates, LTargets: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(1, 100, FKey.PublicKey));
  LTargets := TArray<TAccount>.Create(Account(3, 0, FKey.PublicKey));
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, -1, akaPrimary)));
end;

procedure TOperationWizardTests.FeeAboveProtocolLimitHasNoSignerCandidates;
var LCandidates, LTargets: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(1, UInt64(CT_MaxTransactionFee) + 1, FKey.PublicKey));
  LTargets := TArray<TAccount>.Create(Account(3, 0, FKey.PublicKey));
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, CT_MaxTransactionFee + 1, akaPrimary)));
end;

procedure TOperationWizardTests.LockedTargetCannotUseSecondaryMode;
var LCandidates, LTargets: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(1, 100, FKey.PublicKey));
  LTargets := TArray<TAccount>.Create(Account(3, 10, FKey.PublicKey));
  LTargets[0].accountInfo.state := as_ForSale;
  LTargets[0].accountInfo.locked_until_block := 1000;
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaSecondary)));
  AssertEquals(1, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1001, 10, akaSecondary)));
end;

procedure TOperationWizardTests.ImmatureSignerIsExcluded;
var LCandidates, LTargets: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(5000, 100, FKey.PublicKey));
  LTargets := TArray<TAccount>.Create(Account(3, 0, FKey.PublicKey));
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaPrimary)));
  LCandidates[0].account := 1;
  AssertEquals(1, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaPrimary)));
end;

procedure TOperationWizardTests.ImmatureTargetCannotUseSecondaryMode;
var LCandidates, LTargets: TArray<TAccount>;
begin
  LCandidates := TArray<TAccount>.Create(Account(1, 100, FKey.PublicKey));
  LTargets := TArray<TAccount>.Create(Account(5000, 10, FKey.PublicKey));
  AssertEquals(0, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaSecondary)));
  LTargets[0].account := 3;
  AssertEquals(1, Length(TWIZOperationsModel.FilterSignerCandidates(LCandidates, LTargets, 1000, 10, akaSecondary)));
end;

initialization
  RegisterTest(TOperationWizardTests);

end.
