unit UUnitTests;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}fpcunit,testregistry {$ELSE}TestFramework{$ENDIF FPC}, variants;

type

  { TestItem }

  TTestItem<TInput, TExpected> = record
    Input : TInput;
    Expected : TExpected;
  end;


  { PascalCoin Unit Test }

  TPascalCoinUnitTest = class(TTestCase)
  protected
    class function ParseBytes(const AStr : String) : TBytes;   // if stars with 0x parses as input hexstring else ascii
    class procedure AssertEquals(const AMessage: string; const Expected, Actual: TBytes); overload;
    class procedure AssertEquals(const Expected, Actual: TBytes); overload;
  end;

implementation

uses UCommon;

class function TPascalCoinUnitTest.ParseBytes(const AStr : String) : TBytes;
begin
  if AStr.StartsWith('0x') then
    Result := Hex2Bytes(AStr)
  else
    Result := TEncoding.ASCII.GetBytes(AStr);
end;

class procedure TPascalCoinUnitTest.AssertEquals(const AMessage: string; const Expected, Actual: TBytes); overload;
begin
  AssertTrue(AMessage, BytesCompare(Expected, Actual) = 0, CallerAddr);
end;

class procedure TPascalCoinUnitTest.AssertEquals(const Expected, Actual: TBytes); overload;
begin
  AssertEquals(ComparisonMsg(Bytes2Hex(Expected, True), Bytes2Hex(Actual, True)), Expected, Actual);
end;

end.
