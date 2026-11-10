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
      procedure TestBagGrowsPastInitialCapacity;
      procedure TestRemovedObjectTransfersOwnership;
      procedure TestExceptionDisposesScope;
  end;

  TDisposablesScopeTests = class(TTestCase)
  published
    procedure NormalReturn;
    procedure EarlyExit;
    procedure ExceptionUnwind;
    procedure InnerCatchRetainsBag;
    procedure EmptyBags;
    procedure GrowthReverseOrder;
    procedure RemoveObjectOwnership;
    procedure ClassFieldAutomatic;
    procedure NestedRecordAutomatic;
    procedure MixedResources;
    procedure ReallocMemPreservesData;
    procedure RemoveMemOwnership;
    procedure ExplicitFreeMem;
    procedure UnregisteredReallocDoesNotTakeOwnership;
    procedure CleanupFailureContinues;
  end;

implementation

uses UDisposables.Scope.Checks, Generics.Defaults, Generics.Collections, UCommon.Collections, UMemory, LazLogger;

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

procedure TDisposablesTest.TestBagGrowsPastInitialCapacity;
  procedure RunTest;
  var
    scoped: TDisposables;
    i: Integer;
  begin
    for i := 1 to 65 do
      scoped.AddObject(TTestObject.Create);
    AssertEquals(65, TTestObject.Instances);
  end;
begin
  RunTest;
  AssertEquals('Every registered object must be disposed after bag growth', 0, TTestObject.Instances);
end;

procedure TDisposablesTest.TestRemovedObjectTransfersOwnership;
  function CreateTransferred: TTestObject;
  var scoped: TDisposables;
  begin
    Result := scoped.AddObject(TTestObject.Create) as TTestObject;
    scoped.RemoveObject(Result);
  end;
var
  transferred: TTestObject;
begin
  transferred := CreateTransferred;
  try
    AssertEquals(1, TTestObject.Instances);
  finally
    transferred.Free;
  end;
  AssertEquals(0, TTestObject.Instances);
end;

procedure TDisposablesTest.TestExceptionDisposesScope;
  procedure RunTest;
  var scoped: TDisposables;
  begin
    scoped.AddObject(TTestObject.Create);
    raise EAbort.Create('Fixture exception');
  end;
begin
  try
    RunTest;
  except
    on E: EAbort do ;
  end;
  AssertEquals('Exception unwinding must dispose registered objects', 0, TTestObject.Instances);
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

procedure TDisposablesScopeTests.NormalReturn;
begin
  CheckNormalReturn;
end;

procedure TDisposablesScopeTests.EarlyExit;
begin
  CheckEarlyExit;
end;

procedure TDisposablesScopeTests.ExceptionUnwind;
begin
  CheckExceptionUnwind;
end;

procedure TDisposablesScopeTests.InnerCatchRetainsBag;
begin
  CheckInnerCatchRetainsBag;
end;

procedure TDisposablesScopeTests.EmptyBags;
begin
  CheckEmptyBags;
end;

procedure TDisposablesScopeTests.GrowthReverseOrder;
begin
  CheckGrowthReverseOrder;
end;

procedure TDisposablesScopeTests.RemoveObjectOwnership;
begin
  CheckRemoveObjectOwnership;
end;

procedure TDisposablesScopeTests.ClassFieldAutomatic;
begin
  CheckClassFieldAutomatic;
end;

procedure TDisposablesScopeTests.NestedRecordAutomatic;
begin
  CheckNestedRecordAutomatic;
end;

procedure TDisposablesScopeTests.MixedResources;
begin
  CheckMixedResources;
end;

procedure TDisposablesScopeTests.ReallocMemPreservesData;
begin
  CheckReallocMemPreservesData;
end;

procedure TDisposablesScopeTests.RemoveMemOwnership;
begin
  CheckRemoveMemOwnership;
end;

procedure TDisposablesScopeTests.ExplicitFreeMem;
begin
  CheckExplicitFreeMem;
end;

procedure TDisposablesScopeTests.UnregisteredReallocDoesNotTakeOwnership;
begin
  CheckUnregisteredReallocDoesNotTakeOwnership;
end;

procedure TDisposablesScopeTests.CleanupFailureContinues;
begin
  CheckCleanupFailureContinues;
end;

initialization
  RegisterTest(TDisposablesTest);
  RegisterTest(TDisposablesScopeTests);
end.


