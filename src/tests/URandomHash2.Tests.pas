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
    (Input: '0x0';                                         Expected: '0x1340164c73924ecdcff3866bd1a5c75c5c9f517c480336fa8189b2a4c2c745c3'),
    (Input: 'The quick brown fox jumps over the lazy dog'; Expected: '0xe2ba36ce31ed3b01a5f254dfafb392f29b8eafef24c52fd0480c4db5b2d2e999'),
    (Input: '0x000102030405060708090a0b0c0d0e0f';          Expected: '0x88624077976ac8327b38092fff01428125d914b98d76b5c9270177ec105a6976')
  );

  {  Hash Test Data }

  DATA_RANDOMHASH : array[1..16] of TTestItem<Integer, String> = (
    { NOTE: Input denotes the number of bytes to take from DATA_BYTES when executing test }
    (Input: 17;  Expected: '0x4cc3fb94697855e5d124823331516e5a18623d87d735e67ca3f2f7b87fcdabee'),
    (Input: 31;  Expected: '0xb4655c784cc21d0da5a19420c66ed275c8881952b9119d0b215882880069813c'),
    (Input: 32;  Expected: '0x2e2de78bb4db374779108541ac1eeb36a1f1144b2767e7e1aa8dac19c5430958'),
    (Input: 33;  Expected: '0xb096070aec5b4f6146cc5b70c85ab4e0b94efb5aa3ca41e7fd671fd3b98c8588'),
    (Input: 34;  Expected: '0x015b89e6c6b2b4580702555adda7b445e1064f481442475061a4768e65879eb0'),
    (Input: 63;  Expected: '0x868b30a0091d6562fa3dffe9c5ec5df2214383f2c6f48bbf2b314f8791e089e9'),
    (Input: 64;  Expected: '0xfe90fe080b0dca79aec5e0b269e58d368d9a83ce73171d155f475c42642034d0'),
    (Input: 65;  Expected: '0xec04a9b78b4f4ecf1e48d5c11b89f87f461ea5ae23eaff9d2baa0c703e14d01b'),
    (Input: 100; Expected: '0x42e42defa9c03dd9ff74053a6bffbc966321aa14df446bb36a270844fdb8e838'),
    (Input: 117; Expected: '0xaf4257f5d406ab3e3d0453129fefdf6633016676bb3aa45a40fdac2f5bca4951'),
    (Input: 127; Expected: '0xa998117cd3f3505b4ed863601abc175a393653ce3eec3b375235ad530845045f'),
    (Input: 128; Expected: '0xb4a76d37c5f390986ace3e35b7306c65a62d92f3f449a5f7b523982a5e5496a4'),
    (Input: 129; Expected: '0x432d52101c90b268a7127d84c080f8d5c60d0223620e5c463883b15ed81db390'),
    (Input: 178; Expected: '0x4ab88684c0ef33bc8dad4a4aa1a9badd025bc7afe98657d325e3bf1e4f3706a2'),
    (Input: 199; Expected: '0x25aa461b64f96f7b62b46e79273554a7b5866f26278929ae3f78203a714cca2a'),
    (Input: 200; Expected: '0x3bd8f64cc0eeb0c8dedba03a1f142763c3e6f9aed80dcd6534eac28eba2f7510')
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
    while LHasher.HasCachedHash do begin
      LCachedHash := LHasher.PopCachedHash;
      AssertEquals(TRandomHash2Fast.Compute(LCachedHash.Header), LCachedHash.Hash);
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
