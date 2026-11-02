unit UWalletMiningTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Only generated keys and a disposable wallet file are used. No global wallet,
  settings file, node, network connection or mined transaction is required. }

{$mode delphi}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, UWallet, USettings, UCrypto,
  UPCDataTypes, UBaseTypes;

type
  TWalletMiningTests = class(TTestCase)
  private
    FKeys: TWalletKeys;
    FPrivate, FOtherPrivate, FWatch: TECPrivateKey;
    FDirectory, FFileName: String;
    function WalletBytes: TBytes;
    procedure AssertKey(const AExpected, AActual: TAccountKey);
    procedure AssertUnchanged(const ABefore: TBytes; ACount: Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MissingSelectedUsesExistingPrivateKey;
    procedure RandomNeverSelectsWatchOnly;
    procedure RandomReselectsAfterMinedBlocksWithoutCreatingKeys;
    procedure LockedPrivateKeyRemainsUsable;
    procedure WatchOnlyWalletDoesNotGenerate;
    procedure EmptyWalletDoesNotGenerate;
    procedure NewEachTimeOnlyRotatesAfterMinedBlock;
    procedure SelectionAndDeletionRevalidateCachedKey;
    procedure ReadOnlyWalletDoesNotRotate;
    procedure OrdinaryRefreshAndReloadLeaveFileUntouched;
    procedure ExplicitRotationUsesWalletEncryption;
  end;

implementation

uses UAccounts, UConst, UMemory;

procedure TWalletMiningTests.SetUp;
var LID: TGUID;
begin
  TCrypto.InitCrypto;
  CreateGUID(LID);
  FDirectory := IncludeTrailingPathDelimiter(GetTempDir) + 'PascalCoinMiningTest-' + GUIDToString(LID);
  ForceDirectories(FDirectory);
  FFileName := FDirectory + PathDelim + 'generated-wallet.dat';
  FKeys := TWalletKeys.Create(nil);
  FKeys.WalletFileName := FFileName;
  FPrivate := TECPrivateKey.Create;
  FPrivate.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FOtherPrivate := TECPrivateKey.Create;
  FOtherPrivate.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FWatch := TECPrivateKey.Create;
  FWatch.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
end;

procedure TWalletMiningTests.TearDown;
begin
  FKeys.Free;
  FWatch.Free;
  FOtherPrivate.Free;
  FPrivate.Free;
  DeleteFile(FFileName);
  RemoveDir(FDirectory);
end;

function TWalletMiningTests.WalletBytes: TBytes;
var
  LFile: TFileStream;
  LDisposables: TDisposables;
begin
  LFile := LDisposables.AddObject(TFileStream.Create(FFileName,
    fmOpenRead or fmShareDenyNone)) as TFileStream;
  SetLength(Result, LFile.Size);
  if Length(Result) > 0 then LFile.ReadBuffer(Result[0], Length(Result));
end;

procedure TWalletMiningTests.AssertKey(const AExpected, AActual: TAccountKey);
begin
  AssertTrue('Expected the existing reward public key', TAccountComp.EqualAccountKeys(AExpected, AActual));
end;

procedure TWalletMiningTests.AssertUnchanged(const ABefore: TBytes; ACount: Integer);
begin
  AssertEquals('Normal selection must not add wallet keys', ACount, FKeys.Count);
  AssertTrue('Normal selection must not write the wallet file', TBaseType.Equals(ABefore, WalletBytes));
end;

procedure TWalletMiningTests.MissingSelectedUsesExistingPrivateKey;
var
  LKey: TAccountKey;
  LBefore: TBytes;
begin
  FKeys.AddPrivateKey('Generated existing private key', FPrivate);
  LBefore := WalletBytes;
  AssertTrue(FKeys.TryGetMiningKey(mpk_Selected,
    TAccountComp.AccountKey2RawString(FOtherPrivate.PublicKey), LKey));
  AssertKey(FPrivate.PublicKey, LKey);
  AssertTrue('Malformed saved bytes must fall back without parsing or generation',
    FKeys.TryGetMiningKey(mpk_Selected, TBytes.Create($FF, 1, 2), LKey));
  AssertKey(FPrivate.PublicKey, LKey);
  AssertUnchanged(LBefore, 1);
end;

procedure TWalletMiningTests.RandomNeverSelectsWatchOnly;
var
  LKey: TAccountKey;
  LBefore: TBytes;
  i: Integer;
begin
  FKeys.AddPublicKey('Generated watch-only key', FWatch.PublicKey);
  FKeys.AddPublicKey('Another watch-only key', FOtherPrivate.PublicKey);
  FKeys.AddPrivateKey('Generated eligible key', FPrivate);
  LBefore := WalletBytes;
  for i := 1 to 100 do begin
    AssertTrue(FKeys.TryGetMiningKey(mpk_Random, nil, LKey));
    AssertKey(FPrivate.PublicKey, LKey);
  end;
  AssertTrue('A selected watch-only key also falls back to the eligible private key',
    FKeys.TryGetMiningKey(mpk_Selected, TAccountComp.AccountKey2RawString(FWatch.PublicKey), LKey));
  AssertKey(FPrivate.PublicKey, LKey);
  AssertUnchanged(LBefore, 3);
end;

procedure TWalletMiningTests.RandomReselectsAfterMinedBlocksWithoutCreatingKeys;
var
  LKey, LCurrent: TAccountKey;
  LBefore: TBytes;
  // FPC's RTL seed is unsigned; preserve its high bit in checked test builds.
  LSavedSeed: Cardinal;
  LSeenFirst, LSeenSecond: Boolean;
  i: Integer;
begin
  FKeys.AddPrivateKey('First generated reward key', FPrivate);
  FKeys.AddPrivateKey('Second generated reward key', FOtherPrivate);
  LBefore := WalletBytes;
  LSavedSeed := RandSeed;
  try
    RandSeed := 12345;
    LSeenFirst := False;
    LSeenSecond := False;
    for i := 1 to 100 do begin
      AssertTrue(FKeys.TryGetMiningKey(mpk_Random, nil, LCurrent, True));
      LSeenFirst := LSeenFirst or TAccountComp.EqualAccountKeys(FPrivate.PublicKey, LCurrent);
      LSeenSecond := LSeenSecond or TAccountComp.EqualAccountKeys(FOtherPrivate.PublicKey, LCurrent);
      AssertTrue(FKeys.TryGetMiningKey(mpk_Random, nil, LKey));
      AssertKey(LCurrent, LKey);
      AssertTrue(FKeys.TryGetMiningKey(mpk_Random, nil, LKey));
      AssertKey(LCurrent, LKey);
    end;
    AssertTrue('Actual blocks must be able to select either existing private key', LSeenFirst and LSeenSecond);
    AssertUnchanged(LBefore, 2);
  finally
    RandSeed := LSavedSeed;
  end;
end;

procedure TWalletMiningTests.LockedPrivateKeyRemainsUsable;
var
  LKey: TAccountKey;
  LBefore: TBytes;
begin
  FKeys.AddPrivateKey('Generated encrypted key', FPrivate);
  FKeys.WalletPassword := 'disposable QA password';
  AssertTrue(FKeys.LockWallet);
  AssertFalse(FKeys.IsValidPassword);
  AssertFalse(Assigned(FKeys[0].PrivateKey));
  LBefore := WalletBytes;
  AssertTrue(FKeys.TryGetMiningKey(mpk_Selected,
    TAccountComp.AccountKey2RawString(FPrivate.PublicKey), LKey));
  AssertKey(FPrivate.PublicKey, LKey);
  AssertTrue('Locked NewEachTime retains a public reward key without generating a secret',
    FKeys.TryGetMiningKey(mpk_NewEachTime, nil, LKey, True));
  AssertKey(FPrivate.PublicKey, LKey);
  AssertUnchanged(LBefore, 1);
end;

procedure TWalletMiningTests.WatchOnlyWalletDoesNotGenerate;
var
  LMode: TMinerPrivateKeyType;
  LKey: TAccountKey;
  LBefore: TBytes;
begin
  FKeys.AddPublicKey('Generated public-only key', FWatch.PublicKey);
  LBefore := WalletBytes;
  for LMode := Low(TMinerPrivateKeyType) to High(TMinerPrivateKeyType) do begin
    AssertFalse(FKeys.TryGetMiningKey(LMode,
      TAccountComp.AccountKey2RawString(FWatch.PublicKey), LKey));
    AssertKey(CT_TECDSA_Public_Nul, LKey);
  end;
  AssertUnchanged(LBefore, 1);
end;

procedure TWalletMiningTests.EmptyWalletDoesNotGenerate;
var
  LMode: TMinerPrivateKeyType;
  LKey: TAccountKey;
  LBefore: TBytes;
begin
  LBefore := WalletBytes;
  for LMode := Low(TMinerPrivateKeyType) to High(TMinerPrivateKeyType) do begin
    AssertFalse(FKeys.TryGetMiningKey(LMode, nil, LKey));
    AssertKey(CT_TECDSA_Public_Nul, LKey);
  end;
  AssertUnchanged(LBefore, 0);
end;

procedure TWalletMiningTests.NewEachTimeOnlyRotatesAfterMinedBlock;
var
  LKey, LRotated: TAccountKey;
  LBefore: TBytes;
  i: Integer;
begin
  FKeys.AddPrivateKey('Generated original reward key', FPrivate);
  LBefore := WalletBytes;
  for i := 1 to 20 do begin
    AssertTrue(FKeys.TryGetMiningKey(mpk_NewEachTime, nil, LKey));
    AssertKey(FPrivate.PublicKey, LKey);
  end;
  AssertUnchanged(LBefore, 1);
  AssertTrue(FKeys.TryGetMiningKey(mpk_NewEachTime, nil, LRotated, True));
  AssertEquals('One explicit rotation adds exactly one key', 2, FKeys.Count);
  AssertFalse(TAccountComp.EqualAccountKeys(FPrivate.PublicKey, LRotated));
  LBefore := WalletBytes;
  for i := 1 to 20 do begin
    AssertTrue(FKeys.TryGetMiningKey(mpk_NewEachTime, nil, LKey));
    AssertKey(LRotated, LKey);
  end;
  AssertUnchanged(LBefore, 2);
end;

procedure TWalletMiningTests.SelectionAndDeletionRevalidateCachedKey;
var LKey: TAccountKey;
begin
  FKeys.AddPrivateKey('First generated key', FPrivate);
  FKeys.AddPrivateKey('Second generated key', FOtherPrivate);
  AssertTrue(FKeys.TryGetMiningKey(mpk_Selected,
    TAccountComp.AccountKey2RawString(FPrivate.PublicKey), LKey));
  AssertKey(FPrivate.PublicKey, LKey);
  AssertTrue(FKeys.TryGetMiningKey(mpk_Selected,
    TAccountComp.AccountKey2RawString(FOtherPrivate.PublicKey), LKey));
  AssertKey(FOtherPrivate.PublicKey, LKey);
  FKeys.Delete(FKeys.IndexOfAccountKey(FOtherPrivate.PublicKey));
  AssertTrue(FKeys.TryGetMiningKey(mpk_Selected,
    TAccountComp.AccountKey2RawString(FOtherPrivate.PublicKey), LKey));
  AssertKey(FPrivate.PublicKey, LKey);
  FKeys.Delete(0);
  AssertFalse(FKeys.TryGetMiningKey(mpk_Random, nil, LKey));
  AssertKey(CT_TECDSA_Public_Nul, LKey);
end;

procedure TWalletMiningTests.ReadOnlyWalletDoesNotRotate;
var
  LKey: TAccountKey;
  LBefore: TBytes;
begin
  FKeys.AddPrivateKey('Generated read-only reward key', FPrivate);
  FKeys.IsReadOnly := True;
  LBefore := WalletBytes;
  AssertTrue(FKeys.TryGetMiningKey(mpk_NewEachTime, nil, LKey, True));
  AssertKey(FPrivate.PublicKey, LKey);
  AssertUnchanged(LBefore, 1);
end;

procedure TWalletMiningTests.OrdinaryRefreshAndReloadLeaveFileUntouched;
var
  LKey: TAccountKey;
  LBefore: TBytes;
  LMode: TMinerPrivateKeyType;
  i: Integer;
begin
  FKeys.AddPrivateKey('Generated existing reward key', FPrivate);
  FKeys.AddPublicKey('Generated watch-only key', FWatch.PublicKey);
  LBefore := WalletBytes;
  for i := 1 to 10 do
    for LMode := Low(TMinerPrivateKeyType) to High(TMinerPrivateKeyType) do
      AssertTrue(FKeys.TryGetMiningKey(LMode, nil, LKey));
  AssertUnchanged(LBefore, 2);
  FreeAndNil(FKeys);
  FKeys := TWalletKeys.Create(nil);
  FKeys.WalletFileName := FFileName;
  AssertTrue(FKeys.TryGetMiningKey(mpk_NewEachTime, nil, LKey));
  AssertKey(FPrivate.PublicKey, LKey);
  AssertUnchanged(LBefore, 2);
end;

procedure TWalletMiningTests.ExplicitRotationUsesWalletEncryption;
var
  LKey: TAccountKey;
  LBefore: TBytes;
  i: Integer;
begin
  FKeys.AddPrivateKey('Generated encrypted reward key', FPrivate);
  FKeys.WalletPassword := 'disposable mining password';
  AssertTrue(FKeys.TryGetMiningKey(mpk_NewEachTime, nil, LKey, True));
  AssertEquals(2, FKeys.Count);
  LBefore := WalletBytes;
  FreeAndNil(FKeys);
  FKeys := TWalletKeys.Create(nil);
  FKeys.WalletFileName := FFileName;
  AssertFalse('A rotated secret must not be encrypted with an empty password', FKeys.IsValidPassword);
  for i := 0 to FKeys.Count - 1 do AssertFalse(Assigned(FKeys[i].PrivateKey));
  AssertUnchanged(LBefore, 2);
  FKeys.WalletPassword := 'disposable mining password';
  AssertTrue(FKeys.IsValidPassword);
  AssertTrue(Assigned(FKeys[FKeys.IndexOfAccountKey(LKey)].PrivateKey));
end;

initialization
  RegisterTest(TWalletMiningTests);
end.
