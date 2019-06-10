unit UPCSafeBoxRootHashTests;

interface

uses
  Classes, SysUtils, {$IFDEF FPC}fpcunit,testregistry {$ELSE}TestFramework {$ENDIF FPC},
  UUnitTests;

type

  { TRandomHashTest }

  TPCSafeBoxRootHashTest = class(TPascalCoinUnitTest)
  private
    procedure InitCrypto;
    procedure TestSafeboxRootHash_N_items(ANItems : Integer);
  published
    procedure TestSafeboxRootHash_1_item;
    procedure TestSafeboxRootHash_11_items;
    procedure TestSafeboxRootHash_101_items;
    procedure TestSafeboxRootHash_1000_items;
  end;

  TBytesBuffer32MerkleRootTest = class(TPascalCoinUnitTest)
  private
    procedure InitCrypto;
    procedure TestMerkleRoot_N_items(ANItems : Integer);
  published
    procedure TestMerkleRoot_1_item;
    procedure TestMerkleRoot_11_items;
    procedure TestMerkleRoot_101_items;
    procedure TestMerkleRoot_1000_items;
    procedure TestMerkleRoot_10000_items;
    procedure TestMerkleRoot_100000_items;
//    procedure TestSpeed_100000_items;
  end;


implementation

uses variants, UCommon, UMemory, URandomHash, HlpHashFactory, HlpBitConverter, strutils,
  UCrypto, UPCSafeBoxRootHash, UBaseTypes;



{ TPCSafeBoxRootHashTest }

procedure TPCSafeBoxRootHashTest.InitCrypto;
begin
  TCrypto.InitCrypto;
end;

procedure TPCSafeBoxRootHashTest.TestSafeboxRootHash_1000_items;
begin
  TestSafeboxRootHash_N_items(1000);
end;

procedure TPCSafeBoxRootHashTest.TestSafeboxRootHash_101_items;
begin
  TestSafeboxRootHash_N_items(101);
end;

procedure TPCSafeBoxRootHashTest.TestSafeboxRootHash_11_items;
begin
  TestSafeboxRootHash_N_items(11);
end;

procedure TPCSafeBoxRootHashTest.TestSafeboxRootHash_1_item;
begin
  TestSafeboxRootHash_N_items(1);
end;

procedure TPCSafeBoxRootHashTest.TestSafeboxRootHash_N_items(ANItems: Integer);
var LBuffer : TBytesBuffer;
  LRawData, LSafeBoxRoot : TRawBytes;
  i : Integer;
  LProof : TProofLevels;
begin
  InitCrypto;
  LBuffer := TBytesBuffer.Create(ANItems * 32);
  try
    SetLength(LRawData,32);
    for i := 0 to Length(LRawData)-1 do begin
      LRawData[i] := Random(256);
    end;
    for i := 1 to ANItems do begin
      LBuffer.Add(LRawData);
    end;

    LSafeBoxRoot := TPCSafeboxRootHash.CalcSafeBoxRootHash(LBuffer);

    // Do the tests: FOR EACH ITEM
    for i := 0 to ANItems-1 do begin
      // Get the Proof of each item
      Assert(TPCSafeboxRootHash.GetProof(LBuffer,i,LProof),Format('Cannot GetProof(%d bytes, pos %d) for %d Items',[LBuffer.Length,i,ANItems]));
      // Check the Proof of this item
      Assert(TPCSafeboxRootHash.CheckProof(i,ANItems,LProof),Format('Error on CheckProof(pos %d of %d items)',[i,ANItems]));

      // Check that last array level contains same SafeBoxRootHash obtained
      Assert(TBaseType.Equals(LProof.Levels[High(LProof.Levels)],LSafeBoxRoot),'Not equal safeboxroot values');
    end;
  finally
    LBuffer.Free;
  end;
end;

{ TBytesBuffer32MerkleRootTest }

procedure TBytesBuffer32MerkleRootTest.InitCrypto;
begin
  TCrypto.InitCrypto;
end;

procedure TBytesBuffer32MerkleRootTest.TestMerkleRoot_100000_items;
begin
  TestMerkleRoot_N_items(100000);
end;

procedure TBytesBuffer32MerkleRootTest.TestMerkleRoot_10000_items;
begin
  TestMerkleRoot_N_items(10000);
end;

procedure TBytesBuffer32MerkleRootTest.TestMerkleRoot_1000_items;
begin
  TestMerkleRoot_N_items(1000);
end;

procedure TBytesBuffer32MerkleRootTest.TestMerkleRoot_101_items;
begin
  TestMerkleRoot_N_items(101);
end;

procedure TBytesBuffer32MerkleRootTest.TestMerkleRoot_11_items;
begin
  TestMerkleRoot_N_items(11);
end;

procedure TBytesBuffer32MerkleRootTest.TestMerkleRoot_1_item;
begin
  TestMerkleRoot_N_items(1);
end;

procedure TBytesBuffer32MerkleRootTest.TestMerkleRoot_N_items(ANItems: Integer);
var LBuffer : TBytesBuffer32Safebox;
  LRawData, LSafeBoxRoot_1, LSafeBoxRoot_2 : TRawBytes;
  i, iItem : Integer;
  LPByte : PByte;
begin
  InitCrypto;
  LBuffer := TBytesBuffer32Safebox.Create(ANItems * 32);
  try
    LBuffer.SafeBoxHashCalcType := sbh_Merkle_Root_Hash;
    SetLength(LRawData,32);
    for i := 0 to Length(LRawData)-1 do begin
      LRawData[i] := Random(256);
    end;
    for i := 1 to ANItems do begin
      LBuffer.Add(LRawData);
    end;


    LSafeBoxRoot_1 := TPCSafeboxRootHash.CalcSafeBoxRootHash(LBuffer);
    LSafeBoxRoot_2 := LBuffer.GetSafeBoxHash;

    Assert(TBaseType.Equals(LSafeBoxRoot_1,LSafeBoxRoot_2),'Not equal safeboxroot values');

    LSafeBoxRoot_1 := TPCSafeboxRootHash.CalcSafeBoxRootHash(LBuffer);

    for iItem := 0 to ANItems-1 do begin
      SetLength(LRawData,32);
      LPByte := LBuffer.Memory;
      Inc(LPByte,iItem*32);
      Move(LPByte^,LRawData[0],32);

      LBuffer.Replace(iItem*32,LRawData);

      LSafeBoxRoot_2 := LBuffer.GetSafeBoxHash;

      Assert(TBaseType.Equals(LSafeBoxRoot_1,LSafeBoxRoot_2),Format('Not equal safeboxroot values updating item %d/%d',[iItem+1,ANItems]));
    end;
  finally
    LBuffer.Free;
  end;
end;

initialization
  Randomize;
  RegisterTest(TPCSafeBoxRootHashTest.Suite);
  RegisterTest(TBytesBuffer32MerkleRootTest.Suite);

end.
