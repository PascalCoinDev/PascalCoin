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

initialization
  Randomize;
  RegisterTest(TPCSafeBoxRootHashTest.Suite);

end.
