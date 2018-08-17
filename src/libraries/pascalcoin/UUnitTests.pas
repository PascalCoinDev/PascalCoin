unit UUnitTests;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}fpcunit,testregistry {$ELSE}TestFramework {$ENDIF FPC}, variants;

type

  { TestItem }

  TTestItem<TInput, TExpected> = record
    Input : TInput;
    Expected : TExpected;
  end;


  TTestItem<TInput1, TInput2, TExpected> = record
    Input1 : TInput1;
    Input2 : TInput2;
    Expected : TExpected;
  end;

  { PascalCoin Unit Test }

  TPascalCoinUnitTest = class(TTestCase)
  protected
     function ParseBytes(const AStr : String) : TBytes;   // if stars with 0x parses as input hexstring else ascii
     procedure AssertEquals(const AMessage: string; const Expected, Actual: TBytes); overload;
     procedure AssertEquals(const Expected, Actual: TBytes); overload;
  end;

implementation

uses UCommon;

function TPascalCoinUnitTest.ParseBytes(const AStr : String) : TBytes;
begin
  if AStr.StartsWith('0x') then
    Result := Hex2Bytes(AStr)
  else
    Result := TEncoding.ASCII.GetBytes(AStr);
end;

procedure TPascalCoinUnitTest.AssertEquals(const AMessage: string; const Expected, Actual: TBytes);
begin
{$IFDEF FPC}
  AssertTrue(AMessage, BytesCompare(Expected, Actual) = 0, CallerAddr);
{$ELSE}
  CheckTrue(BytesCompare(Expected, Actual) = 0, AMessage);
{$ENDIF}
end;

procedure TPascalCoinUnitTest.AssertEquals(const Expected, Actual: TBytes);
begin
{$IFDEF FPC}
  AssertEquals(ComparisonMsg(Bytes2Hex(Expected, True), Bytes2Hex(Actual, True)), Expected, Actual);
{$ELSE}
  AssertEquals('TODO', Expected, Actual);
{$ENDIF}
end;

end.
