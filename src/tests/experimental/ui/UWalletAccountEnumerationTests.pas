unit UWalletAccountEnumerationTests;

{ Copyright (c) 2026 by Herman Schoenfeld

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php. }

{$mode delphi}

interface

uses Classes, SysUtils, Generics.Collections, fpcunit, testregistry,
  UAbstractMem, UAbstractBTree, UAbstractMemBTree, UFileMem,
  UPCAbstractMemAccountKeys;

type
  TCountingAccounts = class(TAccountsUsingThisKey)
  public
    NodeReads : Integer;
    function GetNode(AIdentify : TAbstractMemPosition) :
      TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; override;
  end;

  TWalletAccountEnumerationTests = class(TTestCase)
  private
    FDirectory, FFileName : String;
    FMemory : TFileMem;
    FTree : TCountingAccounts;
    FZone : TAMZone;
    FValues : TList<Cardinal>;
    procedure AddRange(ACount : Integer);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EmptyAndZeroCountClearDestination;
    procedure BoundedRangeReturnsExactlyRequestedCount;
    procedure CardinalValuesRetainUnsignedRange;
    procedure RejectsInvalidAccountNumber;
    procedure LargePersistedSnapshotUsesLinearTraversal;
    procedure WalletAccountNumbersAreSortedAndUnique;
  end;

implementation

uses UCrypto, UAccounts, UBlockChain, UWallet, UNode, UConst, UPCDataTypes,
  UCoreUtils;

type
  TEnumerationSafeBox = class(TPCSafeBox)
  public
    procedure Seed(const AFirstKey, ASecondKey : TAccountKey; ABlocks : Integer);
  end;

function TCountingAccounts.GetNode(AIdentify : TAbstractMemPosition) :
  TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode;
begin
  Inc(NodeReads);
  Result := inherited GetNode(AIdentify);
end;

procedure TEnumerationSafeBox.Seed(const AFirstKey, ASecondKey : TAccountKey; ABlocks : Integer);
var LBlock : TOperationBlock;
  i : Integer;
begin
  for i := 0 to ABlocks-1 do begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := i;
    if i mod 2=0 then LBlock.account_key := AFirstKey
    else LBlock.account_key := ASecondKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    AddNew(LBlock);
  end;
end;

procedure TWalletAccountEnumerationTests.SetUp;
var LID : TGUID;
begin
  CreateGUID(LID);
  FDirectory := IncludeTrailingPathDelimiter(GetTempDir) + 'WalletEnumeration-' + GUIDToString(LID);
  ForceDirectories(FDirectory);
  FFileName := IncludeTrailingPathDelimiter(FDirectory) + 'accounts.am';
  FMemory := TFileMem.Create(FFileName,False);
  FMemory.Initialize(True,4);
  FZone := FMemory.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(FMemory));
  FTree := TCountingAccounts.Create(FMemory,FZone,False);
  FValues := TList<Cardinal>.Create;
end;

procedure TWalletAccountEnumerationTests.TearDown;
begin
  FValues.Free;
  FTree.Free;
  FMemory.Free;
  DeleteFile(FFileName);
  DeleteFile(IncludeTrailingPathDelimiter(FDirectory) + 'WalletKeys.dat');
  RemoveDir(FDirectory);
end;

procedure TWalletAccountEnumerationTests.AddRange(ACount : Integer);
var i : Integer;
begin
  for i := ACount-1 downto 0 do
    AssertTrue(FTree.Add(i*3+100));
end;

procedure TWalletAccountEnumerationTests.EmptyAndZeroCountClearDestination;
begin
  FValues.Add(99);
  AssertEquals(0,FTree.FillList(0,10,FValues));
  AssertEquals(0,FValues.Count);
  AddRange(5);
  FValues.Add(99);
  AssertEquals(0,FTree.FillList(0,0,FValues));
  AssertEquals(0,FValues.Count);
end;

procedure TWalletAccountEnumerationTests.BoundedRangeReturnsExactlyRequestedCount;
begin
  AddRange(12);
  FValues.Add(99);
  AssertEquals(1,FTree.FillList(3,1,FValues));
  AssertEquals(109,Integer(FValues[0]));
  AssertEquals(4,FTree.FillList(2,4,FValues));
  AssertEquals(4,FValues.Count);
  AssertEquals(106,Integer(FValues[0]));
  AssertEquals(115,Integer(FValues[3]));
  AssertEquals(2,FTree.FillList(10,9,FValues));
  AssertEquals(130,Integer(FValues[0]));
  AssertEquals(133,Integer(FValues[1]));
  AssertEquals(0,FTree.FillList(12,1,FValues));
  AssertEquals(0,FValues.Count);
end;

procedure TWalletAccountEnumerationTests.CardinalValuesRetainUnsignedRange;
begin
  AssertTrue(FTree.Add(Int64(High(Cardinal))-2));
  AssertTrue(FTree.Add(Int64(High(Cardinal))));
  AssertEquals(2,FTree.FillList(0,FTree.Count,FValues));
  AssertEquals(Int64(High(Cardinal))-2,Int64(FValues[0]));
  AssertEquals(Int64(High(Cardinal)),Int64(FValues[1]));
end;

procedure TWalletAccountEnumerationTests.RejectsInvalidAccountNumber;
var LRaised : Boolean;
begin
  AssertTrue(FTree.Add(Int64(High(Cardinal))+1));
  LRaised := False;
  try
    FTree.FillList(0,1,FValues);
  except
    on EPCAbstractMemAccountKeys do LRaised := True;
  end;
  AssertTrue('An invalid persisted account number must not truncate',LRaised);
end;

procedure TWalletAccountEnumerationTests.LargePersistedSnapshotUsesLinearTraversal;
const LCount = 8192;
var i : Integer;
begin
  AddRange(LCount);
  FreeAndNil(FTree);
  AssertTrue(FMemory.FlushCache);
  FreeAndNil(FMemory);
  FMemory := TFileMem.Create(FFileName,False);
  FTree := TCountingAccounts.Create(FMemory,FZone,False);
  AssertEquals(LCount,FTree.Count);
  FTree.NodeReads := 0;
  AssertEquals(LCount,FTree.FillList(0,FTree.Count,FValues));
  AssertEquals(LCount,FValues.Count);
  for i := 0 to LCount-1 do
    AssertEquals(i*3+100,Integer(FValues[i]));
  AssertTrue('A complete snapshot must not restart at the B-tree root for every index',
    FTree.NodeReads < LCount*4);
end;

procedure TWalletAccountEnumerationTests.WalletAccountNumbersAreSortedAndUnique;
var LFirst, LSecond : TECPrivateKey;
  LSafeBox : TEnumerationSafeBox;
  LNumbers : TArray<Cardinal>;
  LCountedFirst, LCountedSecond : TCountingAccounts;
  i : Integer;

  function CountTree(const AKey : TAccountKey) : TCountingAccounts;
  var LKeys : TPCAbstractMemAccountKeys;
    LSearch, LEntry : TAccountKeyByPosition;
    LInitialZone : TAMZone;
  begin
    LKeys := LSafeBox.PCAbstractMem.AccountKeys;
    LKeys.GetAccountsUsingThisKey(AKey);
    LSearch.Clear;
    LSearch.position := LKeys.GetPositionOfKey(AKey,False);
    AssertTrue(LKeys.AccountKeyByPositionCache.Find(LSearch,LEntry));
    LInitialZone := LEntry.accountsUsingThisKey.InitialZone;
    LKeys.AccountKeyByPositionCache.Remove(LEntry);
    Result := TCountingAccounts.Create(LSafeBox.PCAbstractMem.AbstractMem,LInitialZone,False);
    LEntry.accountsUsingThisKey := Result;
    LKeys.AccountKeyByPositionCache.Add(LEntry); // Existing cache owns the wrapper.
    Result.NodeReads := 0;
  end;

begin
  TCrypto.InitCrypto;
  LFirst := TECPrivateKey.Create;
  LSecond := TECPrivateKey.Create;
  LSafeBox := nil;
  TNode.SetPascalCoinDataFolder(FDirectory);
  TWallet.Load;
  try
    LFirst.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    LSecond.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    LSafeBox := TEnumerationSafeBox.Create;
    LSafeBox.Seed(LFirst.PublicKey,LSecond.PublicKey,128);
    TWallet.Keys.SafeBox := LSafeBox;
    TWallet.Keys.AddPublicKey('Second generated key',LSecond.PublicKey);
    TWallet.Keys.AddPublicKey('First generated key',LFirst.PublicKey);
    TWallet.Keys.AddPublicKey('Repeated generated key',LFirst.PublicKey);
    LCountedFirst := CountTree(LFirst.PublicKey);
    LCountedSecond := CountTree(LSecond.PublicKey);
    LNumbers := TCoreTool.GetUserAccountNumbers;
    AssertEquals(128*CT_AccountsPerBlock,Length(LNumbers));
    for i := 0 to High(LNumbers) do AssertEquals(i,Integer(LNumbers[i]));
    AssertTrue('Wallet enumeration must use the persistent bulk cursor, not indexed Get',
      LCountedFirst.NodeReads + LCountedSecond.NodeReads < Length(LNumbers)*4);
  finally
    TWallet.Unload;
    LSafeBox.Free;
    LSecond.Free;
    LFirst.Free;
    TNode.SetPascalCoinDataFolder('');
  end;
end;

initialization
  RegisterTest(TWalletAccountEnumerationTests);
end.
