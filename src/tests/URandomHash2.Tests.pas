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
    procedure CacheConsistency;
    procedure GetSetLastDWordConsistency;
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
    (Input: '0x0';                                         Expected: '0x78b7b1a58fd073a47b02279080ff5b0d1fb673f0477c18a652801ba8fd0cbac8'),
    (Input: 'The quick brown fox jumps over the lazy dog'; Expected: '0xf6ad93cb45b8749a1d64cd74cf446975cf782990587f9ddd08dfeee2087e7487'),
    (Input: '0x000102030405060708090a0b0c0d0e0f';          Expected: '0xa37a261ff74ccb03bbb64bd1c3da5928f580052adcf7ff48596f4297110f20c2')
  );

  {  Hash Test Data }

  DATA_RANDOMHASH : array[1..16] of TTestItem<Integer, String> = (
    { NOTE: Input denotes the number of bytes to take from DATA_BYTES when executing test }
    (Input: 17;  Expected: '0x28e348e6a865e5333e4528e0743b4965248cac41904e11d9011a50cb19513fb6'),
    (Input: 31;  Expected: '0x656a42062355319bd643a9eff87cc04d14ea442384e7dd0932113911f2823024'),
    (Input: 32;  Expected: '0xd666b74b91c2ddfb54d663bafb369c53055a875ababbdf1f510db39dd73d86fa'),
    (Input: 33;  Expected: '0x163b3758fb5dfb896f469f9914df1f43d966a8ff3d1e710ed51fdcfc7e425308'),
    (Input: 34;  Expected: '0x97a3618f57f9477cd4ba91397cd7856ecd05f8d8206b077828cd52e8ecdcd4c5'),
    (Input: 63;  Expected: '0x93f998f385413219f4ca9b764e3f69f90100fdf43d4434d0f8fe4bcf4ed98ae6'),
    (Input: 64;  Expected: '0x709a93634fe1927f4b94570049ebf33ac4c1bd750392df070e44d59261d763e9'),
    (Input: 65;  Expected: '0x2ef12aab3156b3e25551ed7c1dfbf51c6dfc84510cb6cc46b405bae02578f1b0'),
    (Input: 100; Expected: '0x6cf7f41cb8ec80ae512e68322f67bf381821599d39a2007cb6d1857b3458947a'),
    (Input: 117; Expected: '0xa3ce45f763b30dc74181b4ad4a02631318f93cdf3cc4f4054b01561df29c53fd'),
    (Input: 127; Expected: '0x1c042113abb5c41265916be7092acf2000a148668b0bca9671e9cdbdf13dc99a'),
    (Input: 128; Expected: '0xda552e27ca43bcfc8c636a4c58041027f9f0a06b0d40031c889fe3a673b89701'),
    (Input: 129; Expected: '0x4b1b1d161ff80435ea4a6839530258d33d3105b5812def2b1b65bf543df06e62'),
    (Input: 178; Expected: '0x66cbb61b4b927ca60c4ca04d999b0d8398b9fa9cf1db2866956b246f11cbc692'),
    (Input: 199; Expected: '0x3705f63d6242acd77fbc262010b34101165c7cc6c0c9bbd607bbc118d1626a5f'),
    (Input: 200; Expected: '0xcde9da66925e5b363aaca02dac5c473e81ab6a0f1b7a6b6dbac9d0ebec383e22')
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

procedure TRandomHash2Test.CacheConsistency;
var
  LBuff : TBytes;
  LHasher : TRandomHash2;
  LCachedHash : TRandomHash2.TCachedHash;
  i, j : Integer;
  LDisposables : TDisposables;
begin
  LBuff := ParseBytes(DATA_BYTES);
  LHasher := LDisposables.AddObject( TRandomHash2.Create ) as TRandomHash2;

  for i := 1 to 100 do begin
    LBuff := LHasher.Hash(LBuff);
    while LHasher.HasCachedHash do begin
      LCachedHash := LHasher.PopCachedHash;
      AssertEquals(TRandomHash2.Compute(LCachedHash.Header), LCachedHash.Hash);
    end;
  end;
end;

procedure TRandomHash2Test.GetSetLastDWordConsistency;
var
  LBuff, LBuff2 : TBytes;
  LHasher : TRandomHash2;
  LCachedHash : TRandomHash2.TCachedHash;
  i  : UInt32;
  LDisposables : TDisposables;
begin
  LBuff := ParseBytes(DATA_BYTES);
  LHasher := LDisposables.AddObject( TRandomHash2.Create ) as TRandomHash2;
  for i := 1 to 100 do begin
    LBuff := LHasher.Hash(LBuff);
    AssertEquals(32768 + i, LHasher.GetLastDWordLE(LHasher.SetLastDWordLE(LBuff, 32768 + i)));
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
  RegisterTest(TRandomHash2StressTest);
{$ELSE}
  //TDUnitX.RegisterTextFixture(TRandomHashTest);
  //TDUnitX.RegisterTextFixture(TRandomHashFastTest);
  //TDUnitX.RegisterTextFixture(TRandomHashFast_TChecksummedByteCollectionTest);
  //TDUnitX.RegisterTextFixture(TRandomHashStressTest);
{$ENDIF FPC}

end.
