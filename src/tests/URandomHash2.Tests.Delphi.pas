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
    '0x2710e884255b3be9635a4e1fb2b5ea828eb74b8b8d610d09bc16453c0cf28f80',
    '0x7846c582ea5283d23b7afedb4424605eb73318717de223c71d3548f18ac68c9a',
    '0xd82578f3fdd6d9856cfb306f25ed52e4b75efb2a2d2e17362acc3fe522d73d81'
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
  end;
    //WriteLn(Format('%s', [Bytes2Hex(TRandomHash.Compute(ParseBytes(LCase.Input)), True)]));
end;


initialization

  RegisterTest(TRandomHash2Test.Suite);

end.
