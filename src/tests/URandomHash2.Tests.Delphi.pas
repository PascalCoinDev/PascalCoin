unit URandomHash2.Tests.Delphi;

interface

uses
  Classes, SysUtils, {$IFDEF FPC}fpcunit,testregistry {$ELSE}TestFramework {$ENDIF FPC},
  UUnitTests, HlpIHash;

type

  { TRandomHash2Test }

  TRandomHash2Test = class(TPascalCoinUnitTest)
  published
    procedure TestRandomHash2_Standard;
  end;

  { TRandomHash2FastTest }

  TRandomHash2FastTest = class(TPascalCoinUnitTest)
  published
    procedure TestRandomHash2_Standard;
    procedure ReferenceConsistency_RandomHash;
    procedure CacheConsistency;
    procedure ReferenceConsistency_Expand;
    procedure ReferenceConsistency_Compress;
    procedure ReferenceConsistency_MemTransform1;
  end;

implementation

uses variants, UCommon, UMemory, URandomHash, URandomHash2, HlpHashFactory, HlpBitConverter, strutils;

const

  { General purpose byte array for testing }

  DATA_BYTES : String =
  '0x4f550200ca022000bb718b4b00d6f74478c332f5fb310507e55a9ef9b38551f63858e3f7c86db'+
  'd00200006f69afae8a6b0735b6acfcc58b7865fc8418897c530211f19140c9f95f2453210270000'+
  '0000000003000300a297fd17506f6c796d696e65722e506f6c796d696e65722e506f6c796d69393'+
  '03030303030302184d63666eb166619e925cef2a306549bbc4d6f4da3bdf28b4393d5c1856f0ee3'+
  'b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855000000006d68295b0'+
  '0000000';


  { RandomHash Official Values }

  DATA_RANDOMHASH2_STANDARD_INPUT : array[1..3] of String = (
    '0x0',
    'The quick brown fox jumps over the lazy dog',
    '0x000102030405060708090a0b0c0d0e0f'
  );

  DATA_RANDOMHASH2_STANDARD_EXPECTED : array[1..3] of String = (
    '0x1340164c73924ecdcff3866bd1a5c75c5c9f517c480336fa8189b2a4c2c745c3',
    '0xe2ba36ce31ed3b01a5f254dfafb392f29b8eafef24c52fd0480c4db5b2d2e999',
    '0x88624077976ac8327b38092fff01428125d914b98d76b5c9270177ec105a6976'
  );

{ TRandomHash2Test }

procedure TRandomHash2Test.TestRandomHash2_Standard;
var
  i : integer;
  LStr, LStr2 : String;
  LB, LB2 : TBytes;
begin
  for i := Low(DATA_RANDOMHASH2_STANDARD_INPUT) to High(DATA_RANDOMHASH2_STANDARD_INPUT) do begin
    AssertEquals(ParseBytes(DATA_RANDOMHASH2_STANDARD_EXPECTED[i]), TRandomHash2.Compute(ParseBytes(DATA_RANDOMHASH2_STANDARD_INPUT[i])));
   //WriteLn(Format('%s', [Bytes2Hex(TRandomHash.Compute(ParseBytes(LCase.Input)), True)]));
  end;
end;

{ TRandomHash2FastTest }

procedure TRandomHash2FastTest.TestRandomHash2_Standard;
var
  i : integer;
  LStr, LStr2 : String;
begin
  for i := Low(DATA_RANDOMHASH2_STANDARD_INPUT) to High(DATA_RANDOMHASH2_STANDARD_INPUT) do
    AssertEquals(ParseBytes(DATA_RANDOMHASH2_STANDARD_EXPECTED[i]), TRandomHash2Fast.Compute(ParseBytes(DATA_RANDOMHASH2_STANDARD_INPUT[i])));
   //WriteLn(Format('%s', [Bytes2Hex(TRandomHash.Compute(ParseBytes(LCase.Input)), True)]));
end;

procedure TRandomHash2FastTest.ReferenceConsistency_RandomHash;
var
  LInput, LBuff1, LBuff2 : TBytes;
  LHasher1 : TRandomHash2;
  LHasher2 : TRandomHash2Fast;
  LDisposables : TDisposables;
  i : Integer;
begin
  LInput := ParseBytes(DATA_BYTES);
  LHasher1 := LDisposables.AddObject( TRandomHash2.Create ) as TRandomHash2;
  LHasher2 := LDisposables.AddObject( TRandomHash2Fast.Create ) as TRandomHash2Fast;

  for i := 1 to 100 do begin
    LInput := TRandomHash.Compute( LInput );
    LBuff1 := LHasher1.Hash(LInput);
    LBuff2 := LHasher2.Hash(LInput);
    AssertEquals(LBuff1, LBuff2);
  end;
end;

procedure TRandomHash2FastTest.CacheConsistency;
var
  LBuff : TBytes;
  LHasher : TRandomHash2Fast;
  LCachedHash : TRandomHash2Fast.TCachedHash;
  i, j : Integer;
  LDisposables : TDisposables;
begin
  LBuff := ParseBytes(DATA_BYTES);
  LHasher := LDisposables.AddObject( TRandomHash2Fast.Create ) as TRandomHash2Fast;

  for i := 1 to 100 do begin
    LBuff := LHasher.Hash(LBuff);
    while LHasher.HasCachedHash do begin
      LCachedHash := LHasher.PopCachedHash;
      AssertEquals(TRandomHash2Fast.Compute(LCachedHash.Header), LCachedHash.Hash);
    end;
  end;
end;

procedure TRandomHash2FastTest.ReferenceConsistency_Expand;
var
  LInput, LBuff1, LBuff2 : TBytes;
  LHasher1 : TRandomHash2;
  LHasher2 : TRandomHash2Fast;
  LDisposables : TDisposables;
  i : Integer;
begin
  LInput := ParseBytes(DATA_BYTES);
  LHasher1 := LDisposables.AddObject( TRandomHash2.Create ) as TRandomHash2;
  LHasher2 := LDisposables.AddObject( TRandomHash2Fast.Create ) as TRandomHash2Fast;

  for i := 1 to 100 do begin
    //LInput := TRandomHash.Compute( LInput );
    LInput := TBytes.Create(0,1,2,3);
    LBuff1 := LHasher1.Expand(LInput, 1, i);
    LBuff2 := LHasher2.Expand(LInput, 1, i);
    AssertEquals(LBuff1, LBuff2);
  end;
end;

procedure TRandomHash2FastTest.ReferenceConsistency_Compress;
var
  LInput : TArray<TBytes>;
  LBuff1, LBuff2 : TBytes;
  LHasher1 : TRandomHash2;
  LHasher2 : TRandomHash2Fast;
  LDisposables : TDisposables;
begin
  LInput := TArray<TBytes>.Create( ParseBytes(DATA_BYTES), ParseBytes(DATA_BYTES), ParseBytes(DATA_BYTES)) ;
  LHasher1 := LDisposables.AddObject( TRandomHash2.Create ) as TRandomHash2;
  LHasher2 := LDisposables.AddObject( TRandomHash2Fast.Create ) as TRandomHash2Fast;
  LBuff1 := LHasher1.Compress(LInput, 1234);
  LBuff2 := LHasher2.Compress(LInput, 1234);
  AssertEquals(LBuff1, LBuff2);
end;

procedure TRandomHash2FastTest.ReferenceConsistency_MemTransform1;
var
  LInput, LBuff1, LBuff2, LTmp : TBytes;
  LHasher1 : TRandomHash2;
  LHasher2 : TRandomHash2Fast;
  LDisposables : TDisposables;
  LStartPad, LEndPad, LTransformAmount : Integer;
begin
  LInput := ParseBytes(DATA_BYTES);
  LHasher1 := LDisposables.AddObject( TRandomHash2.Create ) as TRandomHash2;
  LHasher2 := LDisposables.AddObject( TRandomHash2Fast.Create ) as TRandomHash2Fast;


  for LStartPad := 0 to 5 do
    for LEndPad := 0 to 5 do begin
      for LTransformAmount := 4 to 10 do begin
        // Ref
        LInput := TRandomHash.Compute( LInput );
        SetLength(LInput, LTransformAmount);
        LBuff1 := LHasher1.MemTransform1(LInput);
        SetLength(LBuff1, LTransformAmount);

        // Opt
        SetLength(LTmp, Length(LInput)+ LStartPad + LTransformAmount + LEndPad);
        Move(LInput[0], LTmp[LStartPad], Length(LInput));
        LHasher2.MemTransform1(LTmp, LStartPad, LStartPad + Length(LInput), LTransformAmount);
        SetLength(LBuff2, LTransformAmount);
        Move(LTmp[Length(LInput)+LStartPad], LBuff2[0], LTransformAmount);

        // Check
        AssertEquals(LBuff1, LBuff2);
      end;
    end;
end;

initialization
  RegisterTest(TRandomHash2Test.Suite);
  RegisterTest(TRandomHash2FastTest.Suite);
end.
