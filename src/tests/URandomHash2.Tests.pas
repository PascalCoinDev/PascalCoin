unit URandomHash2.Tests;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$MODESWITCH NESTEDPROCVARS}
{$ENDIF}


interface

uses
  Classes, SysUtils, {$IFDEF FPC}fpcunit,testregistry {$ELSE}DUnitX.TestFramework, DUnitX.DUnitCompatibility{$ENDIF FPC},
  UUnitTests, HlpIHash;

type

  { TRandomHash2Test }

  TRandomHash2Test = class(TPascalCoinUnitTest)
  published
    procedure TestRandomHash2_Standard;
    procedure TestRandomHash2;
    procedure GetSetLastDWordConsistency;
  end;

  { TRandomHash2FastTest }

  TRandomHash2FastTest = class(TPascalCoinUnitTest)
  published
    procedure CacheConsistency;
    procedure TestRandomHash2_Standard;
  end;


  { TRandomHash2StressTest }

  TRandomHash2StressTest = class(TPascalCoinUnitTest)
  published
    procedure Reference1000;
  end;

implementation

uses
  variants, UCommon, UCommon.Collections, UMemory, URandomHash2, HlpHashFactory,
  HlpBitConverter, strutils, Generics.Collections;

const

  { General purpose byte array for testing }

  DATA_BYTES : String = '0x4f550200ca022000bb718b4b00d6f74478c332f5fb310507e55a9ef9b38551f63858e3f7c86dbd00200006f69afae8a6b0735b6acfcc58b7865fc8418897c530211f19140c9f95f24532102700000000000003000300a297fd17506f6c796d696e65722e506f6c796d696e65722e506f6c796d6939303030303030302184d63666eb166619e925cef2a306549bbc4d6f4da3bdf28b4393d5c1856f0ee3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855000000006d68295b00000000';

  { RandomHash Official Values }

  DATA_RANDOMHASH_STANDARD : array[1..3] of TTestItem<String, String> = (
    (Input: '0x0';                                         Expected: '0x76821dd68e384bdc2a69fea66a70191c1d7df5799bddc70f2dfaaeda2393899b'),
    (Input: 'The quick brown fox jumps over the lazy dog'; Expected: '0x65ec5370f913497abc57621e9b6703b51d541a320ac2422a51c16e48d0d1fc05'),
    (Input: '0x000102030405060708090a0b0c0d0e0f';          Expected: '0xfabbfcd96c9ef4734bd82d59bfa4bc9c0ff389a53c7c247abb1c1c1794e6dea8')
  );

  {  Hash Test Data }

  DATA_RANDOMHASH : array[1..16] of TTestItem<Integer, String> = (
    { NOTE: Input denotes the number of bytes to take from DATA_BYTES when executing test }
    (Input: 17;  Expected: '0x5f66d30d5872652d6f7c88ade147e5a2dfb1082850c48ce3c4391a354c27f6ef'),
    (Input: 31;  Expected: '0x36a8da1e7af559dd77bd9588c78b0f4a6a5424d049d9dec3379f5246bad26733'),
    (Input: 32;  Expected: '0xf9ec7c6a442296c92352b4b8e74186bc12318f0f6c33cdbea45cf235ab4ba36c'),
    (Input: 33;  Expected: '0xbaca33a265173ec828378e1842d50183d72fc74713e5814db8fd11258b139fc8'),
    (Input: 34;  Expected: '0xeed8e703ab3bb2e525e5f753a401bf506316963c3721bbb006f1f14d0c7e77ed'),
    (Input: 63;  Expected: '0x938256c0a20e8466829b0147215f37bbc21612b097f488680231830ccb6e8073'),
    (Input: 64;  Expected: '0x22efd0c3a556b3b604ddff123c229ba28c37990a4bb2440419689ecc721aea51'),
    (Input: 65;  Expected: '0x79c61dfa3f1c2851feddf768708eb9905f7b074912f1b67bc92c059996c0bb10'),
    (Input: 100; Expected: '0x6a1eb54ce86e5737d03cf3818f4fac142ffb6c345fb7bf536a14700d650b6ce5'),
    (Input: 117; Expected: '0xae9993eadbd78e61f5ef4058e8bf034d66bfd44a7ee4b8e95447f774c13b51f3'),
    (Input: 127; Expected: '0xc2ac91fdd2cffa19c840a53ded2236aed5dccdff8566be4e4a6c95d2b9788f05'),
    (Input: 128; Expected: '0x83c06103d03564515e17929e5083453c9ad7a35f92baa821a9758dbb7e6819a0'),
    (Input: 129; Expected: '0xb8556216f6c2256faee05176bb1b429ff6aa3d514e9f49c2526ded4374ff8881'),
    (Input: 178; Expected: '0xf4abd91b9392c636cf8b22ae4e54f72c5734bc05a80f8a430c6f41e1f7bd5fd1'),
    (Input: 199; Expected: '0x3c392fc666bf0d1127e10989234c8f2f5d4d9cc1c4eba41c1d4736924988a8ac'),
    (Input: 200; Expected: '0xbe8880a61f1039adca78c5d4d073044f142a033d1a31fb4f2cdb73d9501b424a')
  );

{ TRandomHash2Test }

procedure TRandomHash2Test.TestRandomHash2_Standard;
var
  LCase : TTestItem<String, String>;
begin
  for LCase in DATA_RANDOMHASH_STANDARD do
    AssertEquals(ParseBytes(LCase.Expected), TRandomHash2.Compute(ParseBytes(LCase.Input)));
    //WriteLn(Format('%s', [Bytes2Hex(TRandomHash2.Compute(ParseBytes(LCase.Input)), True)]));
end;

procedure TRandomHash2Test.TestRandomHash2;
var
  LInput : TBytes;
  LCase : TTestItem<Integer, String>;
begin
  for LCase in DATA_RANDOMHASH do begin
    LInput := TArrayTool<byte>.Copy(ParseBytes(DATA_BYTES), 0, LCase.Input);
    AssertEquals(ParseBytes(LCase.Expected), TRandomHash2.Compute(LInput));
    //WriteLn(Format('%s', [Bytes2Hex(TRandomHash2.Compute(LInput), True)]));
  end;
end;

procedure TRandomHash2Test.GetSetLastDWordConsistency;
var
  LBuff, LBuff2 : TBytes;
  LHasher : TRandomHash2;
  LCachedHash : TRandomHash2Fast.TCachedHash;
  i  : UInt32;
  LDisposables : TDisposables;
begin
  LBuff := ParseBytes(DATA_BYTES);
  LHasher := LDisposables.AddObject( TRandomHash2.Create ) as TRandomHash2;
  for i := 1 to 100 do begin
    LBuff := LHasher.Hash(LBuff);
    AssertEquals(32768 + i, GetLastDWordLE(SetLastDWordLE(LBuff, 32768 + i)));
  end;
end;

{ TRandomHash2FastTest }

procedure TRandomHash2FastTest.TestRandomHash2_Standard;
var
  LCase : TTestItem<String, String>;
begin
  for LCase in DATA_RANDOMHASH_STANDARD do
    AssertEquals(ParseBytes(LCase.Expected), TRandomHash2Fast.Compute(ParseBytes(LCase.Input)));
    //WriteLn(Format('%s', [Bytes2Hex(TRandomHash2.Compute(ParseBytes(LCase.Input)), True)]));
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
    while LHasher.Cache.HasComputedHash do begin
      LCachedHash := LHasher.Cache.PopComputedHash;
      AssertEquals(TRandomHash2Fast.Compute(LCachedHash.Header), LCachedHash.RoundOutputs[0]);
    end;
  end;
end;

{ TRandomHash2StressTest }

procedure TRandomHash2StressTest.Reference1000;
const
  NUM_ITER = 1000;
var
  i : Integer;
  LBuff : TBytes;
  LHasher : TRandomHash2;
  LDisposables : TDisposables;
begin
  LBuff := ParseBytes(DATA_BYTES);
  LHasher := LDisposables.AddObject( TRandomHash2.Create ) as TRandomHash2;
  for i := 1 to NUM_ITER do
    LBuff := LHasher.Hash(LBuff);
  // no exceptions should occur
end;

initialization

{$IFDEF FPC}
  RegisterTest(TRandomHash2Test);
  RegisterTest(TRandomHash2FastTest);
  RegisterTest(TRandomHash2StressTest);
{$ELSE}
  //TDUnitX.RegisterTextFixture(TRandomHashTest);
  //TDUnitX.RegisterTextFixture(TRandomHashFastTest);
  //TDUnitX.RegisterTextFixture(TRandomHashFast_TChecksummedByteCollectionTest);
  //TDUnitX.RegisterTextFixture(TRandomHashStressTest);
{$ENDIF FPC}

end.
