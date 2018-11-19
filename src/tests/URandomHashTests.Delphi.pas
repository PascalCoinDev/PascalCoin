unit URandomHashTests.Delphi;

interface

uses
  Classes, SysUtils, {$IFDEF FPC}fpcunit,testregistry {$ELSE}TestFramework {$ENDIF FPC},
  UUnitTests, HlpIHash;

type

  { TRandomHashTest }

  TRandomHashTest = class(TPascalCoinUnitTest)
  published
    procedure TestRandomHash_Standard;
  end;

implementation

uses variants, UCommon, UMemory, URandomHash, HlpHashFactory, HlpBitConverter, strutils;

const

  { RandomHash Official Values }

  DATA_RANDOMHASH_STANDARD_INPUT : array[1..3] of String = (
    '0x0',
    'The quick brown fox jumps over the lazy dog',
    '0x000102030405060708090a0b0c0d0e0f'
  );

  DATA_RANDOMHASH_STANDARD_EXPECTED : array[1..3] of String = (
    '0x675c62c74c313647e95e820bbf540c6d4453482b745e62016404424323b69e09',
    '0xaa5b8597f00bdb1c1953e668e6fdd6b5b0df3731b09a7777893d7fc5554b1e3a',
    '0xff4f832020dc4eac07868e9f180f256c9b1d5513b35cd24db5af7da6526bb50f'
  );

{ TRandomHashTest }

procedure TRandomHashTest.TestRandomHash_Standard;
var
  i : integer;
begin
  for i := Low(DATA_RANDOMHASH_STANDARD_INPUT) to High(DATA_RANDOMHASH_STANDARD_INPUT) do
    AssertEquals(ParseBytes(DATA_RANDOMHASH_STANDARD_EXPECTED[i]), TRandomHash.Compute(ParseBytes(DATA_RANDOMHASH_STANDARD_INPUT[i])));
    //WriteLn(Format('%s', [Bytes2Hex(TRandomHash.Compute(ParseBytes(LCase.Input)), True)]));
end;


initialization

  RegisterTest(TRandomHashTest.Suite);

end.
