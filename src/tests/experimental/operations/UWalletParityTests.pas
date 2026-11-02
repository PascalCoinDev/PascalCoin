unit UWalletParityTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Keys and RAW packages are generated in memory. These tests never load the
  global wallet/node, contact RPC or submit operations. Native forms run on the
  regression runner's private Windows desktop. }

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, fpcunit, testregistry, UCrypto, UWallet,
  UPCDataTypes, UBlockChain, UTxMultiOperation, UWizard, UWIZAddKey, UFRMOperationsExplorer;

type
  TParityKeyWizard = class(TWIZAddKeyWizard)
  public
    Run: TNotifyEvent;
  protected
    procedure ShowHost; override;
  end;

  TWalletParityTests = class(TTestCase)
  private
    FKey: TECPrivateKey;
    procedure PublicKeyNavigation(Sender: TObject);
    procedure PrivateKeyNavigation(Sender: TObject);
    procedure CurveNavigation(Sender: TObject);
    procedure RejectPackagePresentation(Sender: TObject);
    procedure RunKeyWizard(ACallback: TNotifyEvent);
    function RawTransaction(AAmount: UInt64): String;
    function RawOperation(AOperation: TPCOperation): String;
    function Multioperation(AReceived: UInt64 = 1900): TOpMultiOperation;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure UnencryptedExportsRoundTripForEveryCurve;
    procedure ProtectedAndLegacyEmptyPasswordExportsRoundTrip;
    procedure BareSecretImportRemainsCompatible;
    procedure PrivateKeyRejectsWrongPasswordAndMalformedInput;
    procedure PublicKeyAcceptsExportedAndRawFormats;
    procedure PublicKeyRejectsMalformedInput;
    procedure PublicKeyWizardRetainsInputAndNameOnBack;
    procedure PrivateKeyWizardRetainsInputPasswordAndNameOnBack;
    procedure KeyWizardRetainsCurveOnBack;
    procedure AdvancedFormContainsCurrentClassicControlsWithoutANode;
    procedure OfflineWorkbenchBlocksExecutionAndRpcHandlers;
    procedure RawWorkbenchImportsExportsAndDeduplicatesPackages;
    procedure RawWorkbenchRejectsConflictsWithoutChangingItsPackage;
    procedure MemoEditorSupportsEditableRawInput;
    procedure MarketFiltersIncludeAtomicSwapsAndIncomingKeys;
    procedure OfflinePartialSignaturesMergeAndPassCoreValidation;
    procedure MergedSignatureEncodingStillRequiresCoreVerification;
    procedure RawImportPresentationFailureIsSeparateFromCommit;
    procedure LateRawConflictDoesNotLeakStagedSignatures;
  end;

implementation

uses
  UConst, UBaseTypes, UAccounts, UOpTransaction, UPCEncryption,
  UMemory, UNode, UFRMOperation, UFRMMemoText, UAccountSelectorData,
  Generics.Collections, UOperationPreparationTests, UWIZAddKey_Start,
  UWIZAddKey_GenerateOrImport, UWIZAddKey_SelectEncryption,
  UWIZAddKey_ImportPubKey, UWIZAddKey_ImportPrivKey, UWIZAddKey_EnterName;

type
  TParitySelectorSource = class(TAccountSelectorDataSource)
  protected
    function ReadChain(ASafeBox: TPCSafeBox): TAccountSelectorChain; override;
    procedure ReadAccounts(const AChain: TAccountSelectorChain;
      const ANumbers: TArray<Cardinal>; out ARows: TArray<TAccount>); override;
  public
    Account: TAccount;
  end;

function TParitySelectorSource.ReadChain(ASafeBox: TPCSafeBox): TAccountSelectorChain;
begin
  Result := Default(TAccountSelectorChain);
  Result.Accounts := 1;
end;

procedure TParitySelectorSource.ReadAccounts(const AChain: TAccountSelectorChain;
  const ANumbers: TArray<Cardinal>; out ARows: TArray<TAccount>);
var
  I: Integer;
begin
  SetLength(ARows, Length(ANumbers));
  for I := 0 to High(ARows) do
    ARows[I] := Account;
end;

procedure TParityKeyWizard.ShowHost;
var
  LMessage: String;
begin
  Host.Show;
  if Assigned(Run) then Run(Self);
  Cancel(LMessage);
end;

procedure TWalletParityTests.SetUp;
begin
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
end;

procedure TWalletParityTests.TearDown;
begin
  FKey.Free;
end;

procedure TWalletParityTests.UnencryptedExportsRoundTripForEveryCurve;
const
  LCurves: array[0..3] of Word = (CT_NID_secp256k1, CT_NID_secp384r1,
    CT_NID_sect283k1, CT_NID_secp521r1);
var
  LDisposables: TDisposables;
  LKey, LImported: TECPrivateKey;
  LWalletKey: TWalletKey;
  LText, LError: String;
  LCurve: Word;
begin
  for LCurve in LCurves do
  begin
    LKey := LDisposables.AddObject(TECPrivateKey.Create) as TECPrivateKey;
    LKey.GenerateRandomPrivateKey(LCurve);
    LWalletKey := CT_TWalletKey_NUL;
    LWalletKey.PrivateKey := LKey;
    LText := TWallet.ExportPrivateKey(LWalletKey, '');
    AssertEquals(LKey.ExportToRaw.ToHexaString, LText);
    AssertTrue(TWallet.TryDecryptPrivateKey('  ' + LText + LineEnding, '', LImported, LError));
    LDisposables.AddObject(LImported);
    AssertTrue(TAccountComp.EqualAccountKeys(LKey.PublicKey, LImported.PublicKey));
  end;
end;

procedure TWalletParityTests.ProtectedAndLegacyEmptyPasswordExportsRoundTrip;
var
  LDisposables: TDisposables;
  LWalletKey: TWalletKey;
  LImported: TECPrivateKey;
  LText, LError: String;
begin
  LWalletKey := CT_TWalletKey_NUL;
  LWalletKey.PrivateKey := FKey;
  LText := TWallet.ExportPrivateKey(LWalletKey, 'export password');
  AssertTrue(TWallet.TryDecryptPrivateKey(LText, 'export password', LImported, LError));
  LDisposables.AddObject(LImported);
  AssertTrue(TAccountComp.EqualAccountKeys(FKey.PublicKey, LImported.PublicKey));
  LText := TPCEncryption.DoPascalCoinAESEncrypt(FKey.ExportToRaw, nil).ToHexaString;
  AssertTrue(TWallet.TryDecryptPrivateKey(LText, '', LImported, LError));
  LDisposables.AddObject(LImported);
  AssertTrue(TAccountComp.EqualAccountKeys(FKey.PublicKey, LImported.PublicKey));
end;

procedure TWalletParityTests.BareSecretImportRemainsCompatible;
var
  LDisposables: TDisposables;
  LImported: TECPrivateKey;
  LText, LError: String;
begin
  LText := FKey.PrivateKeyAsRaw.ToHexaString;
  while Length(LText) < 64 do LText := '0' + LText;
  AssertTrue(TWallet.TryDecryptPrivateKey(LText, '', LImported, LError));
  LDisposables.AddObject(LImported);
  AssertTrue(TAccountComp.EqualAccountKeys(FKey.PublicKey, LImported.PublicKey));
end;

procedure TWalletParityTests.PrivateKeyRejectsWrongPasswordAndMalformedInput;
var
  LWalletKey: TWalletKey;
  LImported: TECPrivateKey;
  LText, LError: String;
begin
  LWalletKey := CT_TWalletKey_NUL;
  LWalletKey.PrivateKey := FKey;
  LText := TWallet.ExportPrivateKey(LWalletKey, 'correct password');
  AssertFalse(TWallet.TryDecryptPrivateKey(LText, 'wrong password', LImported, LError));
  AssertNull(LImported);
  AssertTrue(LError <> '');
  AssertFalse(TWallet.TryDecryptPrivateKey('not hexadecimal', '', LImported, LError));
  AssertNull(LImported);
  LText := FKey.ExportToRaw.ToHexaString;
  AssertFalse(TWallet.TryDecryptPrivateKey(Copy(LText, 1, Length(LText) - 2), '', LImported, LError));
  AssertNull(LImported);
end;

procedure TWalletParityTests.PublicKeyAcceptsExportedAndRawFormats;
var
  LImported: TAccountKey;
  LError: String;
begin
  AssertTrue(TWallet.TryParsePublicKey(TAccountComp.AccountPublicKeyExport(FKey.PublicKey), LImported, LError));
  AssertTrue(TAccountComp.EqualAccountKeys(FKey.PublicKey, LImported));
  AssertTrue(TWallet.TryParsePublicKey(TAccountComp.AccountKey2RawString(FKey.PublicKey).ToHexaString + LineEnding, LImported, LError));
  AssertTrue(TAccountComp.EqualAccountKeys(FKey.PublicKey, LImported));
end;

procedure TWalletParityTests.PublicKeyRejectsMalformedInput;
var
  LImported: TAccountKey;
  LError: String;
begin
  AssertFalse(TWallet.TryParsePublicKey('', LImported, LError));
  AssertFalse(TWallet.TryParsePublicKey('not a key', LImported, LError));
  AssertFalse(TWallet.TryParsePublicKey('CA0200000000', LImported, LError));
  AssertTrue(TAccountComp.IsNullAccountKey(LImported));
end;

procedure TWalletParityTests.RunKeyWizard(ACallback: TNotifyEvent);
var
  LDisposables: TDisposables;
  LWizard: TParityKeyWizard;
  LModel: TWIZAddKeyModel;
begin
  LWizard := LDisposables.AddObject(TParityKeyWizard.Create(nil)) as TParityKeyWizard;
  LModel := LDisposables.AddObject(TWIZAddKeyModel.Create(nil)) as TWIZAddKeyModel;
  LWizard.Run := ACallback;
  LWizard.Start(LModel);
end;

procedure TWalletParityTests.PublicKeyNavigation(Sender: TObject);
var
  LWizard: TParityKeyWizard;
  LMessage, LText: String;
begin
  LWizard := TParityKeyWizard(Sender);
  TWIZAddKey_Start(LWizard.CurrentScreen).rbPublicKey.Checked := True;
  AssertTrue(LWizard.TryNext(LMessage));
  LText := TAccountComp.AccountKey2RawString(FKey.PublicKey).ToHexaString;
  TWIZAddKey_ImportPubKey(LWizard.CurrentScreen).txtPublicKey.Text := LText;
  AssertTrue(LWizard.TryNext(LMessage));
  TWIZAddKey_EnterName(LWizard.CurrentScreen).txtName.Text := 'Retained name';
  AssertTrue(LWizard.TryPrevious(LMessage));
  AssertEquals(LText, Trim(TWIZAddKey_ImportPubKey(LWizard.CurrentScreen).txtPublicKey.Text));
  AssertTrue(LWizard.TryNext(LMessage));
  AssertEquals('Retained name', TWIZAddKey_EnterName(LWizard.CurrentScreen).txtName.Text);
end;

procedure TWalletParityTests.PrivateKeyNavigation(Sender: TObject);
var
  LWizard: TParityKeyWizard;
  LMessage, LText: String;
begin
  LWizard := TParityKeyWizard(Sender);
  AssertTrue(LWizard.TryNext(LMessage));
  TWIZAddKey_GenerateOrImport(LWizard.CurrentScreen).rbPublicKey.Checked := True;
  AssertTrue(LWizard.TryNext(LMessage));
  LText := FKey.ExportToRaw.ToHexaString;
  TWIZAddKey_ImportPrivKey(LWizard.CurrentScreen).txtPrivateKey.Text := LText;
  TWIZAddKey_ImportPrivKey(LWizard.CurrentScreen).txtPassword.Text := 'Retained password';
  AssertTrue(LWizard.TryNext(LMessage));
  TWIZAddKey_EnterName(LWizard.CurrentScreen).txtName.Text := 'Private name';
  AssertTrue(LWizard.TryPrevious(LMessage));
  AssertEquals(LText, Trim(TWIZAddKey_ImportPrivKey(LWizard.CurrentScreen).txtPrivateKey.Text));
  AssertEquals('Retained password', TWIZAddKey_ImportPrivKey(LWizard.CurrentScreen).txtPassword.Text);
  AssertTrue(LWizard.TryNext(LMessage));
  AssertEquals('Private name', TWIZAddKey_EnterName(LWizard.CurrentScreen).txtName.Text);
end;

procedure TWalletParityTests.CurveNavigation(Sender: TObject);
var
  LWizard: TParityKeyWizard;
  LCurve: TWIZAddKey_SelectEncryption;
  LMessage: String;
  LIndex: Integer;
begin
  LWizard := TParityKeyWizard(Sender);
  AssertTrue(LWizard.TryNext(LMessage));
  AssertTrue(LWizard.TryNext(LMessage));
  LCurve := TWIZAddKey_SelectEncryption(LWizard.CurrentScreen);
  LIndex := LCurve.rgKeyType.Items.IndexOfObject(TObject(CT_NID_secp384r1));
  AssertTrue(LIndex >= 0);
  LCurve.rgKeyType.ItemIndex := LIndex;
  AssertTrue(LWizard.TryNext(LMessage));
  AssertTrue(LWizard.TryPrevious(LMessage));
  AssertEquals(LIndex, TWIZAddKey_SelectEncryption(LWizard.CurrentScreen).rgKeyType.ItemIndex);
end;

procedure TWalletParityTests.PublicKeyWizardRetainsInputAndNameOnBack;
begin
  RunKeyWizard(PublicKeyNavigation);
end;

procedure TWalletParityTests.PrivateKeyWizardRetainsInputPasswordAndNameOnBack;
begin
  RunKeyWizard(PrivateKeyNavigation);
end;

procedure TWalletParityTests.KeyWizardRetainsCurveOnBack;
begin
  RunKeyWizard(CurveNavigation);
end;

procedure TWalletParityTests.AdvancedFormContainsCurrentClassicControlsWithoutANode;
var
  LDisposables: TDisposables;
  LForm: TFRMOperation;
  LHadNode: Boolean;
begin
  LHadNode := TNode.NodeExists;
  LForm := LDisposables.AddObject(TFRMOperation.Create(nil, nil)) as TFRMOperation;
  AssertNotNull(LForm.rbListAccountForAccountSwap);
  AssertNotNull(LForm.rbListAccountForCoinSwap);
  AssertNotNull(LForm.ebHashLock);
  AssertNotNull(LForm.btnHashLock);
  AssertNotNull(LForm.ebChangeAccountData);
  AssertNotNull(LForm.memoEPASA);
  AssertEquals(3, LForm.cbPayloadDataInputType.Items.Count);
  AssertEquals(6, LForm.PageControlOpType.PageCount);
  AssertEquals(LHadNode, TNode.NodeExists);
end;

procedure TWalletParityTests.OfflineWorkbenchBlocksExecutionAndRpcHandlers;
var
  LDisposables: TDisposables;
  LForm: TFRMOperationsExplorer;
begin
  LForm := LDisposables.AddObject(TFRMOperationsExplorer.Create(nil)) as TFRMOperationsExplorer;
  AssertFalse(LForm.Offline);
  LForm.Offline := True;
  AssertFalse(LForm.MiRPCCalls.Enabled);
  AssertFalse(LForm.ActExecuteOperation.Enabled);
  AssertFalse(LForm.bbExecuteOperation.Enabled);
  // These would otherwise enter RPC or fail on the intentionally absent node.
  LForm.MiRPCCallsClick(nil);
  LForm.ActExecuteOperationExecute(nil);
  LForm.ActAddTxToMultioperationUpdate(LForm.ActAddTx);
  LForm.AddOpChangeToMultioperationUpdate(LForm.ActAddOpChange);
  AssertTrue(LForm.ActAddTx.Enabled);
  AssertTrue(LForm.ActAddOpChange.Enabled);
end;

function TWalletParityTests.RawTransaction(AAmount: UInt64): String;
var
  LDisposables: TDisposables;
  LOperation: TPCOperation;
begin
  LOperation := LDisposables.AddObject(TOpTransaction.CreateTransaction(CT_PROTOCOL_6,
    100, 8, 101, FKey, AAmount, 100, CT_TOperationPayload_NUL)) as TPCOperation;
  Result := RawOperation(LOperation);
end;

function TWalletParityTests.RawOperation(AOperation: TPCOperation): String;
var
  LDisposables: TDisposables;
  LTree: TOperationsHashTree;
  LStream: TMemoryStream;
  LRaw: TRawBytes;
begin
  LTree := LDisposables.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  AssertTrue(LTree.AddOperationToHashTree(AOperation));
  LStream := LDisposables.AddObject(TMemoryStream.Create) as TMemoryStream;
  LTree.SaveOperationsHashTreeToStream(LStream, False);
  LRaw.FromStream(LStream);
  Result := LRaw.ToHexaString;
end;

function TWalletParityTests.Multioperation(AReceived: UInt64): TOpMultiOperation;
var
  LSenders: TMultiOpSenders;
  LReceivers: TMultiOpReceivers;
begin
  SetLength(LSenders, 2);
  LSenders[0] := CT_TMultiOpSender_NUL;
  LSenders[0].Account := 100;
  LSenders[0].Amount := 1000;
  LSenders[0].N_Operation := 8;
  LSenders[1] := LSenders[0];
  LSenders[1].Account := 103;
  SetLength(LReceivers, 1);
  LReceivers[0] := CT_TMultiOpReceiver_NUL;
  LReceivers[0].Account := 101;
  LReceivers[0].Amount := AReceived;
  Result := TOpMultiOperation.Create(CT_PROTOCOL_6);
  AssertTrue(Result.AddTx(LSenders, LReceivers, False));
end;

procedure TWalletParityTests.RawWorkbenchImportsExportsAndDeduplicatesPackages;
var
  LDisposables: TDisposables;
  LForm: TFRMOperationsExplorer;
  LRaw, LError: String;
begin
  LForm := LDisposables.AddObject(TFRMOperationsExplorer.Create(nil)) as TFRMOperationsExplorer;
  LForm.Offline := True;
  LRaw := RawTransaction(1000);
  if not LForm.TryImportRawOperations(LRaw, LError) then Fail('RAW import failed: ' + LError);
  AssertEquals(1, LForm.Operations.OperationsCount);
  AssertEquals('RAW export must retain its serialized bytes', LRaw, Trim(LForm.mOperationExport.Text));
  if not LForm.TryImportRawOperations(LRaw, LError) then Fail('Repeated RAW import failed: ' + LError);
  AssertEquals(1, LForm.Operations.OperationsCount);
end;

procedure TWalletParityTests.RawWorkbenchRejectsConflictsWithoutChangingItsPackage;
var
  LDisposables: TDisposables;
  LForm: TFRMOperationsExplorer;
  LRaw, LError: String;
begin
  LForm := LDisposables.AddObject(TFRMOperationsExplorer.Create(nil)) as TFRMOperationsExplorer;
  LRaw := RawTransaction(1000);
  if not LForm.TryImportRawOperations(LRaw, LError) then Fail('Initial RAW import failed: ' + LError);
  AssertFalse(LForm.TryImportRawOperations(RawTransaction(2000), LError));
  AssertTrue(LError <> '');
  AssertEquals(1, LForm.Operations.OperationsCount);
  AssertEquals(LRaw, Trim(LForm.mOperationExport.Text));
  AssertFalse(LForm.TryImportRawOperations('not hexadecimal', LError));
  AssertFalse(LForm.TryImportRawOperations(LRaw + 'FF', LError));
  AssertEquals(1, LForm.Operations.OperationsCount);
end;

procedure TWalletParityTests.MemoEditorSupportsEditableRawInput;
var
  LDisposables: TDisposables;
  LForm: TFRMMemoText;
begin
  LForm := LDisposables.AddObject(TFRMMemoText.Create(nil)) as TFRMMemoText;
  LForm.InitData('RAW input', '1234');
  LForm.AllowInput := True;
  AssertFalse(LForm.Memo.ReadOnly);
  LForm.DataText := '5678';
  AssertEquals('5678', Trim(LForm.DataText));
  LForm.AllowInput := False;
  AssertTrue(LForm.Memo.ReadOnly);
end;

procedure TWalletParityTests.MarketFiltersIncludeAtomicSwapsAndIncomingKeys;
var
  LDisposables: TDisposables;
  LSource: TParitySelectorSource;
  LSearch: TAccountSelectorFilter;
  LRows: TList<TAccount>;

  procedure CheckMatch(AExpected: Boolean);
  begin
    // Exercise the provider actually used by the selector, rather than a
    // second copy of the sale predicates retained only for this regression.
    LRows.Clear;
    LSource.Configure(nil, LSearch);
    LSource.FetchAll(LRows);
    AssertEquals(Ord(AExpected), LRows.Count);
  end;

begin
  LSource := LDisposables.AddObject(TParitySelectorSource.Create(nil)) as TParitySelectorSource;
  LRows := LDisposables.AddObject(TList<TAccount>.Create) as TList<TAccount>;
  LSearch := Default(TAccountSelectorFilter);
  LSearch.MaxBalance := -1;
  LSearch.OnlyForSale := True;
  LSource.Account := CT_Account_NUL;
  LSource.Account.accountInfo.state := as_ForAtomicCoinSwap;
  CheckMatch(True);
  LSource.Account.accountInfo.state := as_ForAtomicAccountSwap;
  CheckMatch(True);
  LSearch.OnlyForPrivateSaleToMe := True;
  LSearch.WalletKeys := TArray<TAccountKey>.Create(FKey.PublicKey);
  LSource.Account.accountInfo.new_publicKey := FKey.PublicKey;
  CheckMatch(True);
  LSearch.OnlyForPublicSale := True;
  CheckMatch(False);
  LSearch.OnlyForPrivateSaleToMe := False;
  LSource.Account.accountInfo.state := as_ForSale;
  LSource.Account.accountInfo.new_publicKey := CT_TECDSA_Public_Nul;
  CheckMatch(True);
end;

procedure TWalletParityTests.OfflinePartialSignaturesMergeAndPassCoreValidation;
var
  LDisposables: TDisposables;
  LOtherKey: TECPrivateKey;
  LFirst, LSecond, LMerged, LConflict: TOpMultiOperation;
  LForm: TFRMOperationsExplorer;
  LSafeBox: TOperationSafeBox;
  LSnapshot: TPCSafeBoxTransaction;
  LPrevious: TAccountPreviousBlockInfo;
  LAccount: TAccount;
  LRaw, LError: String;
begin
  LOtherKey := LDisposables.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  LOtherKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  LFirst := LDisposables.AddObject(Multioperation) as TOpMultiOperation;
  LSecond := LDisposables.AddObject(Multioperation) as TOpMultiOperation;
  AssertEquals(1, LFirst.DoSignMultiOperationSigner(CT_PROTOCOL_6, 100, FKey));
  AssertEquals(1, LSecond.DoSignMultiOperationSigner(CT_PROTOCOL_6, 103, LOtherKey));
  LForm := LDisposables.AddObject(TFRMOperationsExplorer.Create(nil)) as TFRMOperationsExplorer;
  LForm.Offline := True;
  if not LForm.TryImportRawOperations(RawOperation(LFirst), LError) then Fail(LError);
  if not LForm.TryImportRawOperations(RawOperation(LSecond), LError) then Fail(LError);
  AssertEquals(1, LForm.Operations.OperationsCount);
  LMerged := LForm.Operations.GetOperation(0) as TOpMultiOperation;
  AssertTrue(TCrypto.ECDSAVerify(FKey.PublicKey, LMerged.GetDigestToSign, LMerged.Data.txSenders[0].Signature));
  AssertTrue(TCrypto.ECDSAVerify(LOtherKey.PublicKey, LMerged.GetDigestToSign, LMerged.Data.txSenders[1].Signature));
  LRaw := Trim(LForm.mOperationExport.Text);
  if not LForm.TryImportRawOperations(RawOperation(LSecond), LError) then Fail(LError);
  AssertEquals('Repeated partial signatures must be idempotent', LRaw, Trim(LForm.mOperationExport.Text));
  LMerged := LForm.Operations.GetOperation(0) as TOpMultiOperation;
  LConflict := LDisposables.AddObject(Multioperation(1800)) as TOpMultiOperation;
  AssertEquals(-1, LMerged.MergeMissingSignaturesFrom(LConflict));
  AssertFalse(LForm.TryImportRawOperations(RawOperation(LConflict), LError));
  AssertEquals(LRaw, Trim(LForm.mOperationExport.Text));

  LSafeBox := LDisposables.AddObject(TOperationSafeBox.Create) as TOperationSafeBox;
  LSafeBox.Build(FKey.PublicKey);
  LAccount := CT_Account_NUL;
  LAccount.accountInfo.state := as_Normal;
  LAccount.accountInfo.accountKey := FKey.PublicKey;
  LAccount.account := 100;
  LAccount.balance := 100000;
  LAccount.n_operation := 7;
  LSafeBox.Seed(LAccount);
  LAccount.account := 101;
  LAccount.balance := 0;
  LSafeBox.Seed(LAccount);
  LAccount.account := 103;
  LAccount.balance := 100000;
  LAccount.accountInfo.accountKey := LOtherKey.PublicKey;
  LSafeBox.Seed(LAccount);
  LSnapshot := LDisposables.AddObject(TPCSafeBoxTransaction.Create(LSafeBox)) as TPCSafeBoxTransaction;
  LPrevious := LDisposables.AddObject(TAccountPreviousBlockInfo.Create) as TAccountPreviousBlockInfo;
  AssertTrue(LMerged.CheckSignatures(LSnapshot, LError));
  AssertTrue(LMerged.DoOperation(LPrevious, LSnapshot, LError));
  AssertEquals(Int64(1900), Int64(LSnapshot.Account(101).balance));
  AssertEquals(Int64(100), LSnapshot.TotalFee);
end;

procedure TWalletParityTests.MergedSignatureEncodingStillRequiresCoreVerification;
var
  LDisposables: TDisposables;
  LFirst, LWrongSigner: TOpMultiOperation;
  LSafeBox: TOperationSafeBox;
  LSnapshot: TPCSafeBoxTransaction;
  LOtherKey: TECPrivateKey;
  LAccount: TAccount;
  LError: String;
begin
  LOtherKey := LDisposables.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  LOtherKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  LFirst := LDisposables.AddObject(Multioperation) as TOpMultiOperation;
  LWrongSigner := LDisposables.AddObject(Multioperation) as TOpMultiOperation;
  LFirst.DoSignMultiOperationSigner(CT_PROTOCOL_6, 100, FKey);
  LWrongSigner.DoSignMultiOperationSigner(CT_PROTOCOL_6, 103, FKey);
  AssertEquals(1, LFirst.MergeMissingSignaturesFrom(LWrongSigner));
  AssertEquals(0, LFirst.MergeMissingSignaturesFrom(LWrongSigner));
  LSafeBox := LDisposables.AddObject(TOperationSafeBox.Create) as TOperationSafeBox;
  LSafeBox.Build(FKey.PublicKey);
  LAccount := CT_Account_NUL;
  LAccount.account := 100;
  LAccount.balance := 100000;
  LAccount.n_operation := 7;
  LAccount.accountInfo.state := as_Normal;
  LAccount.accountInfo.accountKey := FKey.PublicKey;
  LSafeBox.Seed(LAccount);
  LAccount.account := 103;
  LAccount.accountInfo.accountKey := LOtherKey.PublicKey;
  LSafeBox.Seed(LAccount);
  LSnapshot := LDisposables.AddObject(TPCSafeBoxTransaction.Create(LSafeBox)) as TPCSafeBoxTransaction;
  AssertFalse('A well-sized signature from a different owner must fail core verification', LFirst.CheckSignatures(LSnapshot, LError));
end;

procedure TWalletParityTests.RejectPackagePresentation(Sender: TObject);
begin
  raise Exception.Create('Fixture presentation failure');
end;

procedure TWalletParityTests.RawImportPresentationFailureIsSeparateFromCommit;
var
  LDisposables: TDisposables;
  LForm: TFRMOperationsExplorer;
  LError: String;
begin
  LForm := LDisposables.AddObject(TFRMOperationsExplorer.Create(nil)) as TFRMOperationsExplorer;
  LForm.Operations.OnChanged := RejectPackagePresentation;
  AssertTrue(LForm.TryImportRawOperations(RawTransaction(1000), LError));
  AssertEquals(1, LForm.Operations.OperationsCount);
  AssertTrue(Pos('imported', LError) > 0);
  AssertTrue(Pos('Fixture presentation failure', LError) > 0);
  AssertFalse(LForm.TryImportRawOperations('malformed', LError));
  AssertEquals(1, LForm.Operations.OperationsCount);
end;

procedure TWalletParityTests.LateRawConflictDoesNotLeakStagedSignatures;
var
  LDisposables: TDisposables;
  LFirst, LSecond: TOpMultiOperation;
  LConflict: TPCOperation;
  LPackage: TOperationsHashTree;
  LForm: TFRMOperationsExplorer;
  LStream: TMemoryStream;
  LBytes: TRawBytes;
  LRaw, LError: String;
begin
  LFirst := LDisposables.AddObject(Multioperation) as TOpMultiOperation;
  LSecond := LDisposables.AddObject(Multioperation) as TOpMultiOperation;
  LFirst.DoSignMultiOperationSigner(CT_PROTOCOL_6, 100, FKey);
  LSecond.DoSignMultiOperationSigner(CT_PROTOCOL_6, 103, FKey);
  LForm := LDisposables.AddObject(TFRMOperationsExplorer.Create(nil)) as TFRMOperationsExplorer;
  AssertTrue(LForm.TryImportRawOperations(RawOperation(LFirst), LError));
  LRaw := Trim(LForm.mOperationExport.Text);
  LPackage := LDisposables.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  LPackage.AddOperationToHashTree(LSecond);
  LConflict := LDisposables.AddObject(TOpTransaction.CreateTransaction(CT_PROTOCOL_6,
    100, 8, 101, FKey, 5000, 100, CT_TOperationPayload_NUL)) as TPCOperation;
  LPackage.AddOperationToHashTree(LConflict);
  LStream := LDisposables.AddObject(TMemoryStream.Create) as TMemoryStream;
  LPackage.SaveOperationsHashTreeToStream(LStream, False);
  LBytes.FromStream(LStream);
  AssertFalse(LForm.TryImportRawOperations(LBytes.ToHexaString, LError));
  AssertEquals(LRaw, Trim(LForm.mOperationExport.Text));
  AssertEquals(0, Length((LForm.Operations.GetOperation(0) as TOpMultiOperation).Data.txSenders[1].Signature.r));
end;

initialization
  RegisterTest(TWalletParityTests);

end.
