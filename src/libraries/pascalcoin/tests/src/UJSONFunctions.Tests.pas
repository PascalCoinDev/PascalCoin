unit UJSONFunctions.Tests;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
   SysUtils,
   {$IFDEF FPC}
   fpcunit, testutils, testregistry,
   {$ELSE}
   TestFramework,
   {$ENDIF}
   UJSONFunctions,
   UAbstractBTree, UOrderedList;

type
   TestJSONFunctions = class(TTestCase)
   strict private
   public
     procedure SetUp; override;
     procedure TearDown; override;
   published
     procedure Test_JSON;
   end;

implementation




{ TestJSONFunctions }

procedure TestJSONFunctions.SetUp;
begin
  inherited;

end;

procedure TestJSONFunctions.TearDown;
begin
  inherited;

end;

procedure TestJSONFunctions.Test_JSON;
var j : TPCJSONObject;
begin
  j := TPCJSONObject.ParseJSONValue('{"a":[1,2,3,4,7],"d":null,"b":null,"C":null,"m":null,"J":"j"}') as TPCJSONObject;
  try
    j.AsInteger('j',1);
    j.AsInteger('J',2);
    assert(j.HasName('a'),'Not found');
    assert(j.HasName('b'),'Not found');
    assert(j.HasName('C'),'Not found');
    assert(j.HasName('d'),'Not found');
    assert(j.HasName('m'),'Not found');
    assert(j.HasName('j'),'Not found');
    assert(j.HasName('J'),'Not found');
    j.CheckConsistency;
    while j.Count>0 do begin
      j.DeleteName(j.GetNameValue(j.Count-1).Name);
      j.CheckConsistency;
    end;


  finally
    j.Free;
  end;
end;

initialization
  RegisterTest(TestJSONFunctions{$IFNDEF FPC}.Suite{$ENDIF});
end.
