unit RandomHashTests;

interface

uses
  Classes, SysUtils, {$IFDEF FPC}fpcunit,testregistry,{$ELSE}TestFramework,{$ENDIF FPC}
  URandomHash;

type
  { TTestRandomHash }

  TTestRandomHash = class(TTestCase)
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure TestEmpty;
      procedure TestQuickBrownFox;
  end;

implementation

uses UCommon;

{ TTestRandomHash }

procedure TTestRandomHash.SetUp;
begin
  inherited;
end;

procedure TTestRandomHash.TearDown;
begin
  inherited;
end;

procedure TTestRandomHash.TestEmpty;
var
  LDigest, LHash : TBytes;
  LHashHex : AnsiString;
begin
  LDigest := TEncoding.ASCII.GetBytes('');
  LHash := TRandomHash.Compute(LDigest);
  LHashHex := Buffer2Hex(LHash);
  // TODO: need to tweak RandomHash with Polyminer to establish correctness
end;

procedure TTestRandomHash.TestQuickBrownFox;
var
  LDigest, LHash : TBytes;
  LHashHex : AnsiString;
begin
  LDigest := TEncoding.ASCII.GetBytes('The quick brown fox jumps over the lazy dog');
  LHash := TRandomHash.Compute(LDigest);
  LHashHex := Buffer2Hex(LHash);
  // TODO: need to tweak RandomHash with Polyminer to establish correctnessend;
end;

initialization


{$IFDEF FPC}
  RegisterTest(TTestRandomHash);
{$ELSE}
  RegisterTest(TTestRandomHash.Suite);
{$ENDIF FPC}

end.
