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

{ TRandomHash2Test }

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

  for i := 1 to 1000 do begin
    LBuff := LHasher.Hash(LBuff);
    //TFileTool.AppendText('d:/temp/log.txt', Bytes2Hex(LBuff));
    while LHasher.HasCachedHash do begin
      LCachedHash := LHasher.PopCachedHash;
      AssertEquals(TRandomHash2.Compute(LCachedHash.Header), LCachedHash.Hash);
      //TFileTool.AppendText('d:/temp/log.txt', '     ' +Bytes2Hex(LCachedHash.Hash));
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
  for i := 1 to 1000 do begin
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
  //RegisterTest(TRandomHashFastTest);
  //RegisterTest(TRandomHashFast_TChecksummedByteCollectionTest);
//  RegisterTest(TRandomHash2StressTest);
{$ELSE}
  //TDUnitX.RegisterTextFixture(TRandomHashTest);
  //TDUnitX.RegisterTextFixture(TRandomHashFastTest);
  //TDUnitX.RegisterTextFixture(TRandomHashFast_TChecksummedByteCollectionTest);
  //TDUnitX.RegisterTextFixture(TRandomHashStressTest);
{$ENDIF FPC}

end.
