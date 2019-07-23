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
    (Input: '0x0';                                         Expected: '0x2710e884255b3be9635a4e1fb2b5ea828eb74b8b8d610d09bc16453c0cf28f80'),
    (Input: 'The quick brown fox jumps over the lazy dog'; Expected: '0x7846c582ea5283d23b7afedb4424605eb73318717de223c71d3548f18ac68c9a'),
    (Input: '0x000102030405060708090a0b0c0d0e0f';          Expected: '0xd82578f3fdd6d9856cfb306f25ed52e4b75efb2a2d2e17362acc3fe522d73d81')
  );

  {  Hash Test Data }

  DATA_RANDOMHASH : array[1..16] of TTestItem<Integer, String> = (
    { NOTE: Input denotes the number of bytes to take from DATA_BYTES when executing test }
    (Input: 17;  Expected: '0xc32ec411cbfad5b9b3c19a1cf0e8acdb2c4b0990879a7eac5ed8561d3ab93887'),
    (Input: 31;  Expected: '0x51fffe6ecf05ae4302b06320ed13a183a4be09c7268dab8303b442fceb9a63cc'),
    (Input: 32;  Expected: '0x8b9baf1aa33dc651dcedcded4e6ad2dfe592df050cba5aae7ba397e3502a7baf'),
    (Input: 33;  Expected: '0xfbd0c2fc3f8207b9bb1adb587ea2ed4a6de3fc9d3454e375fe577277f0a4f179'),
    (Input: 34;  Expected: '0x13b0016075a7bb970c7f71bf78b475f3f871dbe85b9d4835ffd48b446d4cbd14'),
    (Input: 63;  Expected: '0x54f3f0c4cd6ee11ab1d682669f4c012ab8deb8275d5e6219e4aeae8be34ecc8a'),
    (Input: 64;  Expected: '0x7f7e0095d3185364189a9534b2d9f6f0ef6cf0757027b6af125cbeb92fdcb69c'),
    (Input: 65;  Expected: '0x102bc1c8860c09b9be241f879fa505290c7b5cc4f083257628c920899f05c305'),
    (Input: 100; Expected: '0xc3a152da079b8053a41bc784684dae75f10e5d8b8a940f01766dd2d88783e964'),
    (Input: 117; Expected: '0x59569881237f0cd269dcc8cc59ab3fd9e650ebe8ff737e368150415a9f40b2c4'),
    (Input: 127; Expected: '0x2a7576d9c7982b12d0f01e66636f8d17600343df150a0c46ab1d1ed78bb7265f'),
    (Input: 128; Expected: '0x7f7a3ac6924ea521928447e7b96dcc78fbe917c647523c2e77e6065088b23823'),
    (Input: 129; Expected: '0x8f06802a97a3b0eb5a069c6bc66e2b7660a5b4dd439c64e9af08e32d58f1d771'),
    (Input: 178; Expected: '0xef4b5a65fd27e6869d1d5d746a6705aa68b0f6e327208b837f9e849241103f00'),
    (Input: 199; Expected: '0x867bb39399d57cfba6b3deb9988348dc924da41cdff26b40e81773cdb87d39f5'),
    (Input: 200; Expected: '0x933e6497270d5b0101262d30a9e9bfb1bd47eeddf9eee4b87aba38933bdbb3b5')
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
