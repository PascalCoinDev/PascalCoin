unit UOperationPreparationTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, UCoreUtils, UCoreObjects, UWallet,
  UAccounts, UBlockChain, UOpTransaction, UCrypto, UPCDataTypes, UBaseTypes;

type
  // Shared in-memory fixture for preparation and RAW/offline signing tests.
  TOperationSafeBox = class(TPCSafeBox)
  public
    procedure Build(const AKey: TAccountKey);
    procedure Seed(const AAccount: TAccount);
  end;

  // Every test uses generated in-memory keys and explicit account snapshots.
  // Preparation never loads TWallet.Keys, creates TNode.Node or broadcasts.
  TOperationPreparationTests = class(TTestCase)
  private
    FKeys: TWalletKeys;
    FOwner, FRecipient: TECPrivateKey;
    FRequest: TWalletOperationRequest;
    FTree: TOperationsHashTree;
    FError: string;
    function Account(ANumber: Cardinal; ABalance: UInt64; AKey: TECPrivateKey): TAccount;
    procedure Prepare;
    procedure Reject;
    procedure AssertSigned(AOperation: TPCOperation);
    procedure SetOperation(AType: TExecuteOperationType);
    procedure ValidatePreparedBatch;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure SendSpecifiedAmountAndFeeAreSigned;
    procedure SendAllBalancePreservesFee;
    procedure SendRejectsAmountPlusFeeOverBalance;
    procedure RejectsNegativeAmountAndFee;
    procedure RejectsDuplicateAccounts;
    procedure RejectsWatchOnlySigner;
    procedure RejectsLockedSigner;
    procedure RejectsDifferentOwnerSigner;
    procedure EmptyPayloadClearsPreviousPayload;
    procedure RejectsOversizedPayload;
    procedure PasswordEncryptionRequiresPassword;
    procedure ChangeKeyUsesCommonSignerLastWithoutReorderingInput;
    procedure ChangeKeyProtocolOneInitializesFee;
    procedure CommonSignerPaysCumulativeFeesAndCounters;
    procedure CommonSignerInsufficientForBatchFailsAtomically;
    procedure SecondarySignerPaysOnlyForUnderfundedTargets;
    procedure PublicSaleClearsBuyerKeyAndLock;
    procedure PrivateSaleUsesBuyerKeyAndFutureLock;
    procedure PrivateSaleRejectsExpiredLock;
    procedure DelistSignsSaleAccount;
    procedure ChangeInfoEncodesPayloadWithoutChangingAccountData;
    procedure ChangeInfoRejectsMissingSigner;
    procedure ChangeInfoRejectsNoChanges;
    procedure BuySignsWithFundingKeyAndSupportsPrefundedTarget;
    procedure BuyRejectsInsufficientPriceFunding;
    procedure BuyRejectsWrongPrivateSaleRecipient;
    procedure BuyRejectsExpiredPrivateSale;
    procedure AllSixPreparedOperationsPassCoreValidation;
    procedure EmptyRawBytesSerializationRetainsLengthPrefix;
    procedure EmptyAccountSealConvertsToZeroPaddedBytes;
    procedure AbstractMemNullAndPopulatedKeysRoundTrip;
    procedure PayloadEncryptionModesRoundTrip;
    procedure PurchaseRecipientPayloadUsesNewOwnerKey;
    procedure CorePreflightLeavesPendingSnapshotUnchanged;
    procedure AdvancedSwapsClaimsAndAccountDataPassCoreValidation;
  end;

implementation

uses UConst, UMemory, UAbstractMem, UPCAbstractMemAccountKeys, UPCEncryption;

procedure TOperationSafeBox.Build(const AKey: TAccountKey);
var
  LBlock: TOperationBlock;
  i: Integer;
begin
  for i := 0 to 159 do
  begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := i;
    LBlock.account_key := AKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := i * CT_NewLineSecondsAvg;
    AddNew(LBlock);
  end;
  DoUpgradeToProtocol5;
  DoUpgradeToProtocol6;
end;

procedure TOperationSafeBox.Seed(const AAccount: TAccount);
begin
  UpdateAccount(AAccount.account, AAccount.accountInfo, AAccount.name,
    AAccount.account_type, AAccount.balance, AAccount.n_operation,
    AAccount.account_data, AAccount.account_seal, aus_transaction_commit,
    0, 0, False, False);
end;

function TOperationPreparationTests.Account(ANumber: Cardinal; ABalance: UInt64; AKey: TECPrivateKey): TAccount;
begin
  Result := CT_Account_NUL;
  Result.account := ANumber;
  Result.accountInfo.state := as_Normal;
  Result.accountInfo.accountKey := AKey.PublicKey;
  Result.balance := ABalance;
  Result.n_operation := 7;
end;

procedure TOperationPreparationTests.SetUp;
begin
  TCrypto.InitCrypto;
  FOwner := TECPrivateKey.Create;
  FRecipient := TECPrivateKey.Create;
  FOwner.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FRecipient.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FKeys := TWalletKeys.Create(nil);
  FKeys.AddPrivateKey('Ephemeral test key', FOwner);
  FTree := nil;
  FRequest := Default(TWalletOperationRequest);
  FRequest.OperationType := omtSendPasc;
  FRequest.Accounts := TArray<TAccount>.Create(Account(100, 100000, FOwner));
  FRequest.SignerAccounts := Copy(FRequest.Accounts);
  FRequest.DestinationAccount := Account(101, 0, FRecipient);
  FRequest.SellerAccount := Account(102, 0, FRecipient);
  FRequest.Amount := 25000;
  FRequest.Fee := 100;
  FRequest.SendMode := akaSpecifiedAmount;
  FRequest.SigningMode := akaPrimary;
  FRequest.PayloadMode := pemNotEncrypt;
  FRequest.PublicKey := FRecipient.PublicKey;
  FRequest.SalePrice := 50000;
  FRequest.NewType := 1;
end;

procedure TOperationPreparationTests.TearDown;
begin
  FTree.Free;
  FKeys.Free;
  FRecipient.Free;
  FOwner.Free;
end;

procedure TOperationPreparationTests.Prepare;
begin
  FreeAndNil(FTree);
  AssertTrue(FError, TWIZOperationsHelper.PrepareOperations(FRequest, CT_PROTOCOL_6, 1000, FKeys, FTree, FError));
  AssertNotNull(FTree);
  AssertEquals('', FError);
end;

procedure TOperationPreparationTests.Reject;
begin
  FreeAndNil(FTree);
  AssertFalse(TWIZOperationsHelper.PrepareOperations(FRequest, CT_PROTOCOL_6, 1000, FKeys, FTree, FError));
  AssertNull('No partially prepared batch may escape on failure', FTree);
  AssertTrue('A rejected request must explain the error', FError <> '');
end;

procedure TOperationPreparationTests.SetOperation(AType: TExecuteOperationType);
begin
  FRequest.OperationType := AType;
  if AType = omtDelistAccountFromSale then
  begin
    FRequest.Accounts[0].accountInfo.state := as_ForSale;
    FRequest.Accounts[0].accountInfo.price := FRequest.SalePrice;
  end;
  if AType = omtBuyAccount then
  begin
    FRequest.DestinationAccount.accountInfo.state := as_ForSale;
    FRequest.DestinationAccount.accountInfo.price := FRequest.SalePrice;
    FRequest.DestinationAccount.accountInfo.account_to_pay := 102;
  end;
end;

procedure TOperationPreparationTests.ValidatePreparedBatch;
var
  LDisposables: TDisposables;
  LSafeBox: TOperationSafeBox;
  LTransaction: TPCSafeBoxTransaction;
  LPrevious: TAccountPreviousBlockInfo;
  LError: string;
  i, j: Integer;
  LSelected: Boolean;
begin
  LSafeBox := LDisposables.AddObject(TOperationSafeBox.Create) as TOperationSafeBox;
  LSafeBox.Build(FOwner.PublicKey);
  for i := 0 to High(FRequest.Accounts) do
    LSafeBox.Seed(FRequest.Accounts[i]);
  for i := 0 to High(FRequest.SignerAccounts) do
  begin
    LSelected := False;
    for j := 0 to High(FRequest.Accounts) do
      LSelected := LSelected or (FRequest.SignerAccounts[i].account = FRequest.Accounts[j].account);
    if not LSelected then
      LSafeBox.Seed(FRequest.SignerAccounts[i]);
  end;
  LSafeBox.Seed(FRequest.SellerAccount);
  LTransaction := LDisposables.AddObject(TPCSafeBoxTransaction.Create(LSafeBox)) as TPCSafeBoxTransaction;
  LPrevious := LDisposables.AddObject(TAccountPreviousBlockInfo.Create) as TAccountPreviousBlockInfo;
  LError := '';
  for i := 0 to FTree.OperationsCount - 1 do
    AssertTrue('Core rejected prepared batch: ' + LError,
      FTree.GetOperation(i).DoOperation(LPrevious, LTransaction, LError));
  AssertEquals(FTree.TotalFee, LTransaction.TotalFee);
end;

procedure TOperationPreparationTests.AssertSigned(AOperation: TPCOperation);
var
  LSignature: TECDSA_SIG;
begin
  if AOperation is TOpTransaction then
    LSignature := TOpTransaction(AOperation).Data.sign
  else if AOperation is TOpChangeKey then
    LSignature := TOpChangeKey(AOperation).Data.sign
  else if AOperation is TOpListAccount then
    LSignature := TOpListAccount(AOperation).Data.sign
  else if AOperation is TOpChangeAccountInfo then
    LSignature := TOpChangeAccountInfo(AOperation).Data.sign
  else
    Fail('Unexpected operation type');
  AssertTrue('The operation must verify against the funding/owner key',
    TCrypto.ECDSAVerify(FOwner.PublicKey, AOperation.GetDigestToSign, LSignature));
  AssertFalse('An unrelated key must not verify the signature',
    TCrypto.ECDSAVerify(FRecipient.PublicKey, AOperation.GetDigestToSign, LSignature));
end;

procedure TOperationPreparationTests.SendSpecifiedAmountAndFeeAreSigned;
begin
  Prepare;
  AssertEquals(Int64(25000), FTree.GetOperation(0).OperationAmount);
  AssertEquals(Int64(100), FTree.GetOperation(0).OperationFee);
  AssertEquals(8, Integer(FTree.GetOperation(0).N_Operation));
  AssertSigned(FTree.GetOperation(0));
end;

procedure TOperationPreparationTests.SendAllBalancePreservesFee;
begin
  FRequest.SendMode := akaAllBalance;
  Prepare;
  AssertEquals(Int64(99900), FTree.GetOperation(0).OperationAmount);
  AssertEquals(Int64(100), FTree.GetOperation(0).OperationFee);
  AssertSigned(FTree.GetOperation(0));
end;

procedure TOperationPreparationTests.SendRejectsAmountPlusFeeOverBalance;
begin
  FRequest.Amount := 99950;
  Reject;
end;

procedure TOperationPreparationTests.RejectsNegativeAmountAndFee;
begin
  FRequest.Amount := -1;
  Reject;
  FRequest.Amount := 100;
  FRequest.Fee := -1;
  Reject;
  FRequest.Fee := CT_MaxTransactionFee + 1;
  Reject;
end;

procedure TOperationPreparationTests.RejectsDuplicateAccounts;
begin
  FRequest.Accounts := TArray<TAccount>.Create(FRequest.Accounts[0], FRequest.Accounts[0]);
  Reject;
end;

procedure TOperationPreparationTests.RejectsWatchOnlySigner;
begin
  FKeys.Clear;
  FKeys.AddPublicKey('Watch only', FOwner.PublicKey);
  Reject;
end;

procedure TOperationPreparationTests.RejectsLockedSigner;
begin
  FRequest.Accounts[0].accountInfo.state := as_ForSale;
  FRequest.Accounts[0].accountInfo.new_publicKey := FRecipient.PublicKey;
  FRequest.Accounts[0].accountInfo.locked_until_block := 1050;
  Reject;
end;

procedure TOperationPreparationTests.RejectsDifferentOwnerSigner;
begin
  SetOperation(omtChangeKey);
  FRequest.SignerAccounts[0] := Account(103, 100000, FRecipient);
  Reject;
end;

procedure TOperationPreparationTests.EmptyPayloadClearsPreviousPayload;
begin
  FRequest.PayloadContent := 'previous payload';
  Prepare;
  AssertTrue(Length(FTree.GetOperation(0).OperationPayload.payload_raw) > 0);
  FRequest.PayloadContent := '';
  Prepare;
  AssertEquals(0, Integer(FTree.GetOperation(0).OperationPayload.payload_type));
  AssertEquals(0, Length(FTree.GetOperation(0).OperationPayload.payload_raw));
end;

procedure TOperationPreparationTests.RejectsOversizedPayload;
begin
  FRequest.PayloadContent := StringOfChar('x', CT_MaxPayloadSize + 1);
  Reject;
end;

procedure TOperationPreparationTests.PasswordEncryptionRequiresPassword;
begin
  FRequest.PayloadMode := pemEncryptWithPassword;
  FRequest.PayloadContent := 'secret';
  Reject;
end;

procedure TOperationPreparationTests.ChangeKeyUsesCommonSignerLastWithoutReorderingInput;
begin
  SetOperation(omtChangeKey);
  FRequest.Accounts := TArray<TAccount>.Create(Account(100, 100000, FOwner), Account(103, 1000, FOwner));
  Prepare;
  AssertEquals(100, Integer(FRequest.Accounts[0].account));
  AssertEquals(103, Integer(FTree.GetOperation(0).DestinationAccount));
  AssertEquals(100, Integer(FTree.GetOperation(1).DestinationAccount));
  AssertEquals(8, Integer(FTree.GetOperation(0).N_Operation));
  AssertEquals(9, Integer(FTree.GetOperation(1).N_Operation));
  AssertSigned(FTree.GetOperation(0));
  AssertSigned(FTree.GetOperation(1));
  ValidatePreparedBatch;
end;

procedure TOperationPreparationTests.ChangeKeyProtocolOneInitializesFee;
begin
  SetOperation(omtChangeKey);
  AssertTrue(FError, TWIZOperationsHelper.PrepareOperations(FRequest, CT_PROTOCOL_1, 1000, FKeys, FTree, FError));
  AssertEquals(Int64(100), FTree.GetOperation(0).OperationFee);
  AssertSigned(FTree.GetOperation(0));
end;

procedure TOperationPreparationTests.CommonSignerPaysCumulativeFeesAndCounters;
begin
  SetOperation(omtEnlistAccountForSale);
  FRequest.Accounts := TArray<TAccount>.Create(Account(103, 0, FOwner), Account(104, 0, FOwner));
  FRequest.SignerAccounts := TArray<TAccount>.Create(Account(100, 200, FOwner));
  Prepare;
  AssertEquals(Int64(200), FTree.TotalFee);
  AssertEquals(8, Integer(FTree.GetOperation(0).N_Operation));
  AssertEquals(9, Integer(FTree.GetOperation(1).N_Operation));
  AssertSigned(FTree.GetOperation(1));
  ValidatePreparedBatch;
end;

procedure TOperationPreparationTests.CommonSignerInsufficientForBatchFailsAtomically;
begin
  SetOperation(omtEnlistAccountForSale);
  FRequest.Accounts := TArray<TAccount>.Create(Account(103, 0, FOwner), Account(104, 0, FOwner));
  FRequest.SignerAccounts := TArray<TAccount>.Create(Account(100, 199, FOwner));
  Reject;
end;

procedure TOperationPreparationTests.SecondarySignerPaysOnlyForUnderfundedTargets;
begin
  SetOperation(omtChangeKey);
  FRequest.SigningMode := akaSecondary;
  FRequest.Accounts := TArray<TAccount>.Create(Account(103, 100, FOwner), Account(104, 0, FOwner));
  FRequest.SignerAccounts := TArray<TAccount>.Create(Account(100, 100, FOwner));
  Prepare;
  AssertEquals(103, Integer(FTree.GetOperation(0).SignerAccount));
  AssertEquals(100, Integer(FTree.GetOperation(1).SignerAccount));
  AssertEquals(Int64(200), FTree.TotalFee);
  AssertSigned(FTree.GetOperation(0));
  AssertSigned(FTree.GetOperation(1));
  ValidatePreparedBatch;
end;

procedure TOperationPreparationTests.PublicSaleClearsBuyerKeyAndLock;
var LData: TOpListAccountData;
begin
  SetOperation(omtEnlistAccountForSale);
  FRequest.SaleMode := akaPublicSale;
  FRequest.LockedUntilBlock := 1500;
  Prepare;
  LData := TOpListAccount(FTree.GetOperation(0)).Data;
  AssertEquals(Ord(as_ForSale), Ord(LData.account_state));
  AssertTrue(TAccountComp.IsNullAccountKey(LData.new_public_key));
  AssertEquals(0, Integer(LData.locked_until_block));
  AssertEquals(102, Integer(LData.account_to_pay));
  AssertSigned(FTree.GetOperation(0));
end;

procedure TOperationPreparationTests.PrivateSaleUsesBuyerKeyAndFutureLock;
var LData: TOpListAccountData;
begin
  SetOperation(omtEnlistAccountForSale);
  FRequest.SaleMode := akaPrivateSale;
  FRequest.LockedUntilBlock := 1050;
  Prepare;
  LData := TOpListAccount(FTree.GetOperation(0)).Data;
  AssertTrue(TAccountComp.EqualAccountKeys(FRecipient.PublicKey, LData.new_public_key));
  AssertEquals(1050, Integer(LData.locked_until_block));
  AssertSigned(FTree.GetOperation(0));
end;

procedure TOperationPreparationTests.PrivateSaleRejectsExpiredLock;
begin
  SetOperation(omtEnlistAccountForSale);
  FRequest.SaleMode := akaPrivateSale;
  FRequest.LockedUntilBlock := 1000;
  Reject;
end;

procedure TOperationPreparationTests.DelistSignsSaleAccount;
begin
  SetOperation(omtDelistAccountFromSale);
  Prepare;
  AssertTrue(FTree.GetOperation(0) is TOpDelistAccountForSale);
  AssertSigned(FTree.GetOperation(0));
end;

procedure TOperationPreparationTests.ChangeInfoEncodesPayloadWithoutChangingAccountData;
var LData: TOpChangeAccountInfoData;
begin
  SetOperation(omtChangeInfo);
  FRequest.NewName := TEncoding.ASCII.GetBytes('test-account');
  FRequest.PayloadContent := 'change-info payload';
  Prepare;
  LData := TOpChangeAccountInfo(FTree.GetOperation(0)).Data;
  AssertTrue(TBaseType.Equals(FRequest.NewName, LData.new_name));
  AssertTrue(TBaseType.Equals(TEncoding.ASCII.GetBytes(FRequest.PayloadContent), LData.payload.payload_raw));
  AssertEquals(0, Integer(LData.payload.payload_type));
  AssertFalse('The wizard does not change account data', account_data in LData.changes_type);
  AssertSigned(FTree.GetOperation(0));
end;

procedure TOperationPreparationTests.ChangeInfoRejectsMissingSigner;
begin
  SetOperation(omtChangeInfo);
  FRequest.SignerAccounts := nil;
  Reject;
end;

procedure TOperationPreparationTests.ChangeInfoRejectsNoChanges;
begin
  SetOperation(omtChangeInfo);
  FRequest.NewType := 0;
  Reject;
end;

procedure TOperationPreparationTests.BuySignsWithFundingKeyAndSupportsPrefundedTarget;
var LData: TOpTransactionData;
begin
  SetOperation(omtBuyAccount);
  FRequest.DestinationAccount.balance := 30000;
  Prepare;
  LData := TOpTransaction(FTree.GetOperation(0)).Data;
  AssertEquals(Ord(buy_account), Ord(LData.opTransactionStyle));
  AssertEquals(Int64(25000), Int64(LData.amount));
  AssertEquals(Int64(50000), Int64(LData.AccountPrice));
  AssertSigned(FTree.GetOperation(0));
end;

procedure TOperationPreparationTests.BuyRejectsInsufficientPriceFunding;
begin
  SetOperation(omtBuyAccount);
  Reject;
end;

procedure TOperationPreparationTests.BuyRejectsWrongPrivateSaleRecipient;
begin
  SetOperation(omtBuyAccount);
  FRequest.DestinationAccount.balance := 30000;
  FRequest.DestinationAccount.accountInfo.new_publicKey := FOwner.PublicKey;
  FRequest.DestinationAccount.accountInfo.locked_until_block := 1050;
  Reject;
end;

procedure TOperationPreparationTests.BuyRejectsExpiredPrivateSale;
begin
  SetOperation(omtBuyAccount);
  FRequest.DestinationAccount.balance := 30000;
  FRequest.DestinationAccount.accountInfo.new_publicKey := FRecipient.PublicKey;
  FRequest.DestinationAccount.accountInfo.locked_until_block := 999;
  Reject;
end;

procedure TOperationPreparationTests.AllSixPreparedOperationsPassCoreValidation;
const
  LTypes: array[0..5] of TExecuteOperationType = (omtSendPasc, omtChangeKey,
    omtEnlistAccountForSale, omtDelistAccountFromSale, omtChangeInfo, omtBuyAccount);
var
  LType: TExecuteOperationType;

  procedure CheckOperation(AType: TExecuteOperationType);
  var
    LDisposables: TDisposables;
    LSafeBox: TOperationSafeBox;
    LTransaction: TPCSafeBoxTransaction;
    LPrevious: TAccountPreviousBlockInfo;
    LAccount: TAccount;
    LError: string;
    i: Integer;
  begin
    FRequest.Accounts := TArray<TAccount>.Create(Account(100, 100000, FOwner));
    FRequest.SignerAccounts := Copy(FRequest.Accounts);
    FRequest.DestinationAccount := Account(101, 30000, FRecipient);
    SetOperation(AType);
    Prepare;
    LSafeBox := LDisposables.AddObject(TOperationSafeBox.Create) as TOperationSafeBox;
    LSafeBox.Build(FOwner.PublicKey);
    LSafeBox.Seed(FRequest.Accounts[0]);
    LSafeBox.Seed(FRequest.DestinationAccount);
    LSafeBox.Seed(FRequest.SellerAccount);
    LTransaction := LDisposables.AddObject(TPCSafeBoxTransaction.Create(LSafeBox)) as TPCSafeBoxTransaction;
    LPrevious := LDisposables.AddObject(TAccountPreviousBlockInfo.Create) as TAccountPreviousBlockInfo;
    LError := '';
    for i := 0 to FTree.OperationsCount - 1 do
      AssertTrue('Core rejected ' + FTree.GetOperation(i).ClassName + ': ' + LError,
        FTree.GetOperation(i).DoOperation(LPrevious, LTransaction, LError));
    LAccount := LTransaction.Account(100);
    AssertEquals(8, Integer(LAccount.n_operation));
    AssertEquals(Int64(100), LTransaction.TotalFee);
    if AType = omtSendPasc then
      AssertEquals(Int64(55000), Int64(LTransaction.Account(101).balance));
    if AType = omtEnlistAccountForSale then
      AssertTrue(TAccountComp.IsAccountForPublicSale(LAccount.accountInfo));
    if AType = omtDelistAccountFromSale then
      AssertEquals(Ord(as_Normal), Ord(LAccount.accountInfo.state));
    if AType = omtChangeKey then
      AssertTrue(TAccountComp.EqualAccountKeys(FRecipient.PublicKey, LAccount.accountInfo.accountKey));
    if AType = omtChangeInfo then
      AssertEquals(1, LAccount.account_type);
    if AType = omtBuyAccount then
    begin
      AssertTrue(TAccountComp.EqualAccountKeys(FRecipient.PublicKey, LTransaction.Account(101).accountInfo.accountKey));
      AssertEquals(Int64(50000), Int64(LTransaction.Account(102).balance));
      AssertEquals(Int64(5000), Int64(LTransaction.Account(101).balance));
    end;

  end;

begin
  for LType in LTypes do
    CheckOperation(LType);
end;

procedure TOperationPreparationTests.EmptyRawBytesSerializationRetainsLengthPrefix;
var
  LDisposables: TDisposables;
  LRaw, LEncoded: TRawBytes;
  LStream: TMemoryStream;
  LPosition: Integer;
begin
  LRaw := nil;
  LEncoded := LRaw.ToSerialized;
  AssertEquals(2, Length(LEncoded));
  AssertEquals(0, Integer(LEncoded[0]));
  AssertEquals(0, Integer(LEncoded[1]));
  LPosition := 0;
  AssertTrue(LRaw.LoadFromTBytes(LEncoded, LPosition));
  AssertEquals(2, LPosition);
  AssertEquals(0, Length(LRaw));
  LEncoded := nil;
  LPosition := 0;
  LRaw.SaveInsideTBytes(LEncoded, LPosition);
  AssertEquals(2, Length(LEncoded));
  AssertEquals(2, LPosition);
  LStream := LDisposables.AddObject(TMemoryStream.Create) as TMemoryStream;
  LRaw.ToSerialized(LStream);
  AssertEquals(Int64(2), LStream.Size);
  LRaw.FromStream(LStream, 2, 0);
  AssertEquals(0, Length(LRaw));
  LStream.Position := 0;
  SetLength(LEncoded, 2);
  LStream.ReadBuffer(LEncoded[0], Length(LEncoded));
  AssertEquals(0, Integer(LEncoded[0]));
  AssertEquals(0, Integer(LEncoded[1]));
  LRaw := TEncoding.ASCII.GetBytes('abc');
  LEncoded := LRaw.ToSerialized;
  AssertEquals(5, Length(LEncoded));
  AssertEquals(3, Integer(LEncoded[0]));
  AssertEquals(Ord('a'), Integer(LEncoded[2]));

end;

procedure TOperationPreparationTests.EmptyAccountSealConvertsToZeroPaddedBytes;
var
  LRaw: TRawBytes;
  L20: T20Bytes;
  L32: T32Bytes;
  i: Integer;
begin
  LRaw := nil;
  L20 := TBaseType.To20Bytes(LRaw);
  L32 := TBaseType.To32Bytes(LRaw);
  for i := Low(L20) to High(L20) do
    AssertEquals(0, Integer(L20[i]));
  for i := Low(L32) to High(L32) do
    AssertEquals(0, Integer(L32[i]));
  LRaw := TEncoding.ASCII.GetBytes('x');
  L20 := TBaseType.To20Bytes(LRaw);
  AssertEquals(Ord('x'), Integer(L20[0]));
  AssertEquals(0, Integer(L20[1]));
end;

procedure TOperationPreparationTests.AbstractMemNullAndPopulatedKeysRoundTrip;
var
  LDisposables: TDisposables;
  LMemory: TMem;
  LWritten, LRead: TAbstractMemAccountKeyNode;
  LZone: TAMZone;
  i: Integer;
begin
  LMemory := LDisposables.AddObject(TMem.Create(0, False)) as TMem;
  for i := 0 to 1 do
  begin
    LWritten.Clear;
    if i = 1 then
      LWritten.accountKey := FOwner.PublicKey;
    LZone := LMemory.New(LWritten.GetSize(LMemory));
    LWritten.myPosition := LZone.position;
    LWritten.WriteToMem(LMemory);
    LRead.ReadFromMem(LWritten.myPosition, LMemory);
    AssertTrue(TAccountComp.EqualAccountKeys(LWritten.accountKey, LRead.accountKey));
    AssertEquals(Int64(LWritten.myPosition), Int64(LRead.myPosition));
  end;
end;

procedure TOperationPreparationTests.PayloadEncryptionModesRoundTrip;
var
  LMode: TPayloadEncryptionMode;
  LDecrypted: TRawBytes;
begin
  FRequest.PayloadContent := 'private operation payload';
  FRequest.PayloadPassword := 'ephemeral test password';
  for LMode := pemEncryptWithSender to pemEncryptWithPassword do
  begin
    FRequest.PayloadMode := LMode;
    Prepare;
    case LMode of
      pemEncryptWithSender: AssertTrue(TPCEncryption.DoPascalCoinECIESDecrypt(FOwner.PrivateKey, FTree.GetOperation(0).OperationPayload.payload_raw, LDecrypted));
      pemEncryptWithRecipient: AssertTrue(TPCEncryption.DoPascalCoinECIESDecrypt(FRecipient.PrivateKey, FTree.GetOperation(0).OperationPayload.payload_raw, LDecrypted));
      pemEncryptWithPassword:
        AssertTrue(TPCEncryption.DoPascalCoinAESDecrypt(FTree.GetOperation(0).OperationPayload.payload_raw,
          TEncoding.ANSI.GetBytes(FRequest.PayloadPassword), LDecrypted));
    end;
    AssertTrue(TBaseType.Equals(TEncoding.ANSI.GetBytes(FRequest.PayloadContent), LDecrypted));
    AssertSigned(FTree.GetOperation(0));
  end;
end;

procedure TOperationPreparationTests.PurchaseRecipientPayloadUsesNewOwnerKey;
var
  LDecrypted: TRawBytes;
begin
  SetOperation(omtBuyAccount);
  FRequest.DestinationAccount.balance := 30000;
  FRequest.DestinationAccount.accountInfo.accountKey := FOwner.PublicKey;
  FRequest.PayloadMode := pemEncryptWithRecipient;
  FRequest.PayloadContent := 'new owner private payload';
  Prepare;
  AssertTrue(TPCEncryption.DoPascalCoinECIESDecrypt(FRecipient.PrivateKey,
    FTree.GetOperation(0).OperationPayload.payload_raw, LDecrypted));
  AssertTrue(TBaseType.Equals(TEncoding.ANSI.GetBytes(FRequest.PayloadContent), LDecrypted));
  AssertFalse('The former owner must not decrypt the new owner payload',
    TPCEncryption.DoPascalCoinECIESDecrypt(FOwner.PrivateKey, FTree.GetOperation(0).OperationPayload.payload_raw, LDecrypted));
  AssertSigned(FTree.GetOperation(0));
end;

procedure TOperationPreparationTests.CorePreflightLeavesPendingSnapshotUnchanged;
var
  LDisposables: TDisposables;
  LSafeBox: TOperationSafeBox;
  LSnapshot: TPCSafeBoxTransaction;
begin
  Prepare;
  LSafeBox := LDisposables.AddObject(TOperationSafeBox.Create) as TOperationSafeBox;
  LSafeBox.Build(FOwner.PublicKey);
  LSafeBox.Seed(FRequest.Accounts[0]);
  LSafeBox.Seed(FRequest.DestinationAccount);
  LSnapshot := LDisposables.AddObject(TPCSafeBoxTransaction.Create(LSafeBox)) as TPCSafeBoxTransaction;
  AssertTrue(TWIZOperationsHelper.ValidatePreparedOperations(FTree, LSnapshot, FError));
  AssertEquals(7, Integer(LSnapshot.Account(100).n_operation));
  AssertEquals(Int64(100000), Int64(LSnapshot.Account(100).balance));
  AssertEquals(Int64(0), LSnapshot.TotalFee);
  AssertFalse(TWIZOperationsHelper.ValidatePreparedOperations(nil, LSnapshot, FError));
end;

procedure TOperationPreparationTests.AdvancedSwapsClaimsAndAccountDataPassCoreValidation;

  procedure CheckOperation(AKind: Integer);
  var
    LDisposables: TDisposables;
    LSafeBox: TOperationSafeBox;
    LSnapshot: TPCSafeBoxTransaction;
    LPrevious: TAccountPreviousBlockInfo;
    LOperation: TPCOperation;
    LPayload: TOperationPayload;
    LSecret, LData: TRawBytes;
    LHashLock: T32Bytes;
    LState: TAccountState;
    LNewKey: TAccountKey;
    LPrice: UInt64;
    LPayee: Cardinal;
    LError: String;
  begin
    LSafeBox := LDisposables.AddObject(TOperationSafeBox.Create) as TOperationSafeBox;
    LSafeBox.Build(FOwner.PublicKey);
    LSafeBox.Seed(Account(100, 100000, FOwner));
    LSafeBox.Seed(Account(102, 0, FRecipient));
    LSafeBox.Seed(Account(103, 1000, FOwner));
    LSnapshot := LDisposables.AddObject(TPCSafeBoxTransaction.Create(LSafeBox)) as TPCSafeBoxTransaction;
    LPrevious := LDisposables.AddObject(TAccountPreviousBlockInfo.Create) as TAccountPreviousBlockInfo;
    LPayload := CT_TOperationPayload_NUL;
    if AKind = 2 then
    begin
      LData := TEncoding.ASCII.GetBytes('account data');
      LOperation := LDisposables.AddObject(TOpChangeAccountInfo.CreateChangeAccountInfo(CT_PROTOCOL_6,
        100, 8, 100, FOwner, False, CT_TECDSA_Public_Nul, False, nil,
        False, 0, True, LData, 100, LPayload)) as TPCOperation;
      AssertTrue(LOperation.DoOperation(LPrevious, LSnapshot, LError));
      AssertTrue(TBaseType.Equals(LData, LSnapshot.Account(100).account_data));
      Exit;
    end;
    LSecret := TEncoding.ASCII.GetBytes('ephemeral atomic swap secret');
    LHashLock := TAccountComp.CalculateHashLock(LSecret);
    if AKind = 0 then
    begin
      LState := as_ForAtomicAccountSwap;
      LNewKey := FRecipient.PublicKey;
      LPrice := 0;
      LPayee := 100;
    end
    else
    begin
      LState := as_ForAtomicCoinSwap;
      LNewKey := CT_TECDSA_Public_Nul;
      LPrice := 50000;
      LPayee := 102;
    end;
    LOperation := LDisposables.AddObject(TOpListAccountForSaleOrSwap.CreateListAccountForSaleOrSwap(
      CT_PROTOCOL_6, LState, 100, 8, 100, LPrice, 100, LPayee, LNewKey,
      200, FOwner, LHashLock, LPayload)) as TPCOperation;
    AssertTrue(LOperation.DoOperation(LPrevious, LSnapshot, LError));
    AssertEquals(Ord(LState), Ord(LSnapshot.Account(100).accountInfo.state));
    LPayload.payload_raw := TEncoding.ASCII.GetBytes('wrong secret');
    if AKind = 0 then
      LOperation := LDisposables.AddObject(TOpBuyAccount.CreateBuy(CT_PROTOCOL_6,
        100, 9, 100, 100, 0, 0, 100, FRecipient.PublicKey, FRecipient, LPayload)) as TPCOperation
    else
      LOperation := LDisposables.AddObject(TOpTransaction.CreateTransaction(CT_PROTOCOL_6,
        103, 8, 100, FOwner, 0, 100, LPayload)) as TPCOperation;
    AssertFalse('A wrong swap secret must not claim the account/funds', LOperation.DoOperation(LPrevious, LSnapshot, LError));
    LPayload.payload_raw := LSecret;
    if AKind = 0 then
      LOperation := LDisposables.AddObject(TOpBuyAccount.CreateBuy(CT_PROTOCOL_6,
        100, 9, 100, 100, 0, 0, 100, FRecipient.PublicKey, FRecipient, LPayload)) as TPCOperation
    else
      LOperation := LDisposables.AddObject(TOpTransaction.CreateTransaction(CT_PROTOCOL_6,
        103, 8, 100, FOwner, 0, 100, LPayload)) as TPCOperation;
    AssertTrue(LOperation.DoOperation(LPrevious, LSnapshot, LError));
    AssertEquals(Ord(as_Normal), Ord(LSnapshot.Account(100).accountInfo.state));
    if AKind = 0 then
      AssertTrue(TAccountComp.EqualAccountKeys(FRecipient.PublicKey, LSnapshot.Account(100).accountInfo.accountKey))
    else
      AssertEquals(Int64(50000), Int64(LSnapshot.Account(102).balance));
  end;

begin
  CheckOperation(0);
  CheckOperation(1);
  CheckOperation(2);
end;

initialization
  RegisterTest(TOperationPreparationTests);

end.
