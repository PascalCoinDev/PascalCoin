unit UMemory.Tests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  UCommon;

type
  TDisposablesTest = class(TTestCase)
    published
      procedure TestNestedScope;
      procedure TestRecordScope;
  end;

implementation

uses Generics.Defaults, Generics.Collections, UCommon.Collections, UMemory, LazLogger;

type
  TTestObject = class(TObject)
    public
      Instances: Integer; static;
      Val : AnsiString;
      class constructor Create;
      constructor Create; overload;
      destructor Destroy; override;
  end;

  TTestRecord = record
    public
      Disposables : TDisposables;
  end;


procedure TDisposablesTest.TestNestedScope;
   procedure RunTest;
   var auto : TDisposables;
   begin
     auto.AddObject( TTestObject.Create );
     auto.AddObject( TTestObject.Create );
     auto.AddObject( TTestObject.Create );
     Self.AssertEquals('Premature collection', 3, TTestObject.Instances);
   end;
begin
  RunTest;
  Self.AssertEquals('Memory leak', 0, TTestObject.Instances);
end;

procedure TDisposablesTest.TestRecordScope;
   procedure RunTest;
   var autoRec : TTestRecord; dummy : TTestObject;

   begin
     autoRec.Disposables.AddObject(TTestObject.Create);
     autoRec.Disposables.AddObject(TTestObject.Create);
     autoRec.Disposables.AddObject(TTestObject.Create);
     Self.AssertEquals('Premature collection', 3, TTestObject.Instances);
   end;

begin
  RunTest;
  Self.AssertEquals('Memory leak', 0, TTestObject.Instances);
end;

class constructor TTestObject.Create;
begin
  Instances := 0;
end;

constructor TTestObject.Create;
begin
  Inherited;
  Inc(Instances);
end;

destructor TTestObject.Destroy;
begin
  Inherited;
  Dec(Instances);
end;

initialization
  RegisterTest(TDisposablesTest);
end.


