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

implementation

uses variants, UCommon, UMemory, URandomHash2, HlpHashFactory, HlpBitConverter, strutils;

const

  { RandomHash Official Values }

  DATA_RANDOMHASH2_STANDARD_INPUT : array[1..3] of String = (
    '0x0',
    'The quick brown fox jumps over the lazy dog',
    '0x000102030405060708090a0b0c0d0e0f'
  );

  DATA_RANDOMHASH2_STANDARD_EXPECTED : array[1..3] of String = (
    '0x78b7b1a58fd073a47b02279080ff5b0d1fb673f0477c18a652801ba8fd0cbac8',
    '0xf6ad93cb45b8749a1d64cd74cf446975cf782990587f9ddd08dfeee2087e7487',
    '0xa37a261ff74ccb03bbb64bd1c3da5928f580052adcf7ff48596f4297110f20c2'
  );

{ TRandomHash2Test }

procedure TRandomHash2Test.TestRandomHash2_Standard;
var
  i : integer;
  LStr, LStr2 : String;
  LB, LB2 : TBytes;
begin
  for i := Low(DATA_RANDOMHASH2_STANDARD_INPUT) to High(DATA_RANDOMHASH2_STANDARD_INPUT) do
    AssertEquals(ParseBytes(DATA_RANDOMHASH2_STANDARD_EXPECTED[i]), TRandomHash2.Compute(ParseBytes(DATA_RANDOMHASH2_STANDARD_INPUT[i])));
   //WriteLn(Format('%s', [Bytes2Hex(TRandomHash.Compute(ParseBytes(LCase.Input)), True)]));
end;


initialization

  RegisterTest(TRandomHash2Test.Suite);

end.
