unit UWalletRestoreTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php. }

{$mode delphi}

interface

uses Classes, SysUtils, fpcunit, testregistry, UWallet, UCrypto;

type
  TWalletRestoreTests = class(TTestCase)
  private
    FDirectory, FBackup: String;
    FKey: TECPrivateKey;
    FKeyEventCount: Integer;
    FKeyEventSawIndex: Boolean;
    procedure SaveBackup(const APassword: String);
    procedure OnKeyAdded(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure WrongPasswordReturnsEmptyCounts;
    procedure RestoreAndRepeatCountDuplicates;
    procedure PrivateBackupUpgradesWatchOnlyKey;
    procedure LockedWalletRejectsRestoreAndGeneration;
    procedure UnloadReleasesWalletFile;
    procedure PasswordlessKeySurvivesSaveAndReload;
    procedure KeyAddedEventSeesUpdatedAccountIndex;
  end;

implementation

uses UNode, UConst, UAccounts, UBlockChain, UPCDataTypes, UCommon;

type
  TKeyEventSafeBox = class(TPCSafeBox)
    procedure Seed(const AKey: TAccountKey);
  end;

procedure TKeyEventSafeBox.Seed(const AKey: TAccountKey);
var
  block: TOperationBlock;
begin
  block := CT_OperationBlock_NUL;
  block.account_key := AKey;
  block.protocol_version := CT_PROTOCOL_1;
  block.protocol_available := CT_PROTOCOL_6;
  block.compact_target := CT_MinCompactTarget_v1;
  AddNew(block);
end;

procedure TWalletRestoreTests.SetUp;
var
  id: TGUID;
begin
  CreateGUID(id);
  FDirectory := IncludeTrailingPathDelimiter(GetTempDir) + 'PascalCoinRestoreTest-' + GUIDToString(id);
  ForceDirectories(FDirectory);
  FBackup := FDirectory + PathDelim + 'generated-backup.wallet';
  // No node is constructed. Every file belongs to this generated fixture.
  TNode.SetPascalCoinDataFolder(FDirectory);
  TWallet.Load;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
end;

procedure TWalletRestoreTests.TearDown;
begin
  TWallet.Unload;
  FreeAndNil(FKey);
  DeleteFile(FBackup);
  DeleteFile(FDirectory + PathDelim + 'WalletKeys.dat');
  RemoveDir(FDirectory);
  TNode.SetPascalCoinDataFolder('');
end;

procedure TWalletRestoreTests.SaveBackup(const APassword: String);
var
  keys: TWalletKeys;
  stream: TFileStream;
begin
  keys := TWalletKeys.Create(nil);
  try
    keys.WalletPassword := APassword;
    keys.AddPrivateKey('Generated fixture key', FKey);
    stream := TFileStream.Create(FBackup, fmCreate);
    try
      keys.SaveToStream(stream);
    finally
      stream.Free;
    end;
  finally
    keys.Free;
  end;
end;

procedure TWalletRestoreTests.WrongPasswordReturnsEmptyCounts;
var
  restored: TRestoreWalletResult;
begin
  SaveBackup('fixture password');
  restored := TWallet.RestoreWallet(FBackup, 'wrong');
  AssertFalse(restored.Success);
  AssertEquals(0, restored.TotalKeysFound);
  AssertEquals(0, restored.ImportedPrivateKeys);
  AssertEquals(0, restored.ImportedPublicKeys);
  AssertEquals(0, restored.Duplicates);
  AssertEquals(0, TWallet.Keys.Count);
end;

procedure TWalletRestoreTests.RestoreAndRepeatCountDuplicates;
var
  restored: TRestoreWalletResult;
begin
  SaveBackup('fixture password');
  restored := TWallet.RestoreWallet(FBackup, 'fixture password');
  AssertTrue(restored.Success);
  AssertEquals(1, restored.TotalKeysFound);
  AssertEquals(1, restored.ImportedPrivateKeys);
  AssertEquals(0, restored.Duplicates);
  restored := TWallet.RestoreWallet(FBackup, 'fixture password');
  AssertTrue(restored.Success);
  AssertEquals(1, restored.Duplicates);
  AssertEquals(0, restored.ImportedPrivateKeys);
  AssertEquals(1, TWallet.Keys.Count);
end;

procedure TWalletRestoreTests.PrivateBackupUpgradesWatchOnlyKey;
var
  restored: TRestoreWalletResult;
begin
  TWallet.Keys.AddPublicKey('Watch-only fixture', FKey.PublicKey);
  SaveBackup('');
  restored := TWallet.RestoreWallet(FBackup, '');
  AssertTrue(restored.Success);
  AssertEquals(1, restored.ImportedPrivateKeys);
  AssertEquals(0, restored.Duplicates);
  AssertEquals(1, TWallet.Keys.Count);
  AssertTrue(TWallet.Keys[0].HasPrivateKey);
  AssertTrue(Assigned(TWallet.Keys[0].PrivateKey));
end;

procedure TWalletRestoreTests.LockedWalletRejectsRestoreAndGeneration;
var
  rejected: Boolean;
begin
  SaveBackup('');
  TWallet.Keys.AddPrivateKey('Existing fixture', FKey);
  TWallet.Keys.WalletPassword := 'target password';
  TWallet.Keys.LockWallet;
  rejected := false;
  try
    TWallet.RestoreWallet(FBackup, '');
  except
    on E: Exception do rejected := Pos('locked', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('Restore must not encrypt new keys with an unknown target password', rejected);
  rejected := false;
  try
    TWallet.GenerateNewKey('Should not be created', CT_Default_EC_OpenSSL_NID);
  except
    on E: Exception do rejected := Pos('locked', LowerCase(E.Message)) > 0;
  end;
  AssertTrue('Key generation must require the current wallet password', rejected);
  TWallet.Keys.WalletPassword := 'target password';
  AssertTrue(TWallet.Keys.IsValidPassword);
  AssertEquals(1, TWallet.Keys.Count);
end;

procedure TWalletRestoreTests.UnloadReleasesWalletFile;
begin
  TWallet.Keys.AddPrivateKey('Fixture', FKey);
  TWallet.Unload;
  AssertTrue(DeleteFile(FDirectory + PathDelim + 'WalletKeys.dat'));
  // Repeated shutdown is safe, and the same directory can be reopened.
  TWallet.Unload;
  TWallet.Load;
  AssertEquals(0, TWallet.Keys.Count);
end;

procedure TWalletRestoreTests.PasswordlessKeySurvivesSaveAndReload;
begin
  TWallet.Keys.AddPrivateKey('Fixture', FKey);
  TWallet.Unload;
  TWallet.Load;
  AssertEquals(1, TWallet.Keys.Count);
  AssertTrue(TWallet.Keys.IsValidPassword);
  AssertTrue(Assigned(TWallet.Keys[0].PrivateKey));
  AssertTrue(TAccountComp.EqualAccountKeys(FKey.PublicKey, TWallet.Keys[0].AccountKey));
end;

procedure TWalletRestoreTests.OnKeyAdded(Sender: TObject);
begin
  Inc(FKeyEventCount);
  FKeyEventSawIndex := TWallet.Keys.AccountsKeyList.IndexOfAccountKey(FKey.PublicKey) >= 0;
end;

procedure TWalletRestoreTests.KeyAddedEventSeesUpdatedAccountIndex;
var
  safebox: TKeyEventSafeBox;
begin
  safebox := TKeyEventSafeBox.Create;
  try
    safebox.Seed(FKey.PublicKey);
    TWallet.Keys.SafeBox := safebox;
    FKeyEventCount := 0;
    FKeyEventSawIndex := false;
    TWallet.Keys.OnChanged.Add(OnKeyAdded);
    try
      TWallet.Keys.AddPrivateKey('Account-owning generated fixture', FKey);
      AssertEquals('One completed key mutation raises one notification', 1, FKeyEventCount);
      AssertTrue('UI listeners must see the completed account-key index', FKeyEventSawIndex);
    finally
      TWallet.Keys.OnChanged.Remove(OnKeyAdded);
    end;
  finally
    TWallet.Keys.SafeBox := nil;
    safebox.Free;
  end;
end;

initialization
  RegisterTest(TWalletRestoreTests);
end.
