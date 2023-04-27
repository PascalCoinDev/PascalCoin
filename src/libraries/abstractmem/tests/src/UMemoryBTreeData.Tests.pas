unit UMemoryBTreeData.Tests;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
   SysUtils, Classes,
   {$IFDEF FPC}
   fpcunit, testutils, testregistry,
   {$ELSE}
   TestFramework,
   {$ENDIF}
   {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults,{$ELSE}Generics.Collections,Generics.Defaults,{$ENDIF}
   UMemoryBTreeData,
   UOrderedList;

type
   TestTMemoryBTreeData = class(TTestCase)
   strict private
   public
     Type TTestData = record
       a : Integer;
       b : String;
     end;
   published
     procedure Test_Integer;
     procedure Test_Record;
   end;

implementation


{ TestTMemoryBTreeData }

function CompareTTestData(const ALeft, ARight: TestTMemoryBTreeData.TTestData): Integer;
begin
  Result := ALeft.a - ARight.a;
  if Result=0 then Result := TComparison_String(ALeft.b,ARight.b);
end;

procedure TestTMemoryBTreeData.Test_Integer;
var Lbt : TMemoryBTreeData<Integer>;
  i, intValue : Integer;
begin
  Lbt := TMemoryBTreeData<Integer>.Create(TComparison_Integer,True,7);
  try
    for i:=1 to Lbt.Order do Lbt.AddIndex(TComparison_Integer,False);
    i := 1;
    repeat
      intValue := (i DIV 2);
      inc(i);
      Lbt.AddData(intValue);
      if (i MOD Lbt.Order)=0 then Lbt.DeleteData(intValue);
    until Lbt.Height>6;
    Lbt.CheckConsistency;
  finally
    Lbt.Free;
  end;
end;

procedure TestTMemoryBTreeData.Test_Record;
var Lbt : TMemoryBTreeData<TTestData>;
  i : Integer;
  d : TTestData;
begin
  Lbt := TMemoryBTreeData<TTestData>.Create(CompareTTestData,True,7);
  try
    for i:=1 to Lbt.Order do Lbt.AddIndex(CompareTTestData,False);
    repeat
      d.a := (i DIV 2);
      d.b := IntToStr(d.a);
      inc(i);
      Lbt.AddData(d);
      if (i MOD Lbt.Order)=0 then Lbt.DeleteData(d);
    until Lbt.Height>6;
    Lbt.CheckConsistency;
  finally
    Lbt.Free;
  end;
end;

initialization
  RegisterTest(TestTMemoryBTreeData{$IFNDEF FPC}.Suite{$ENDIF});
end.
