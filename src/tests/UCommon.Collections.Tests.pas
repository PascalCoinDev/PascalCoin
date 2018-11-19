unit UCommon.Collections.Tests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TCompareTests = class(TTestCase)
    private
      function ObjectCompareFunc(constref Left, Right : Integer ) : Integer;
    published
      procedure NestedCompare;
      procedure ObjectCompare;
      procedure GlobalCompare;
      procedure ManyTest_LE;
      procedure ManyTest_EQ;
      procedure ManyTest_GE;
      procedure MemTest;
  end;

  TFilterTests = class(TTestCase)
    private
      function ObjectFilterFunc(constref AItem : Integer ) : boolean;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure NestedFilter;
      procedure ObjectFilter;
      procedure GlobalFilter;
      procedure AndManyFilter;
      procedure OrManyFilter;
  end;

implementation

uses Generics.Defaults, Generics.Collections, UCommon, UCommon.Collections, UMemory, LazLogger;

{ TCompareTests }

procedure TCompareTests.NestedCompare;
var
  cmp : IComparer<Integer>;
  res : Integer;

  function NestedCompareFunc(constref Left, Right : Integer ) : Integer;
  begin
    Result := TCompare.Integer(Left, Right);
  end;

begin
  cmp := TComparerTool<Integer>.FromFunc( NestedCompareFunc );
  res := cmp.Compare(2,3);
  self.AssertEquals(-1, res);
end;

function TCompareTests.ObjectCompareFunc(constref Left, Right : Integer ) : Integer;
begin
  Result := TCompare.Integer(Left, Right);
end;

procedure TCompareTests.ObjectCompare;
var
  cmp : IComparer<Integer>;
  res : Integer;

begin
  cmp := TComparerTool<Integer>.FromFunc( ObjectCompareFunc );
  res := cmp.Compare(2,3);
  self.AssertEquals(-1, res);
end;

function GlobalCompareFunc(constref Left, Right : Integer ) : Integer;
begin
  Result := TCompare.Integer(Left, Right);
end;

procedure TCompareTests.GlobalCompare;
var
  cmp : IComparer<Integer>;
  res : Integer;

begin
  cmp := TComparerTool<Integer>.FromFunc( GlobalCompareFunc );
  res := cmp.Compare(2,3);
  self.AssertEquals(-1, res);
end;

procedure TCompareTests.MemTest;

  // Manually debug and confirm destructor called on comparers
  procedure Test1;
  var
    cmp : IComparer<Integer>;
  begin
       // 1 globalcomparer destroy
      cmp := TComparerTool<Integer>.FromFunc(GlobalCompareFunc);
  end;

  procedure Test2;
  var
    cmp : IComparer<Integer>;
  begin
      // 1 andmanycomparer, 2 globalcomparers detroys
      cmp := TComparerTool<Integer>.Many([GlobalCompareFunc, GlobalCompareFunc]);
  end;

begin
   Test1;
   Test2;
end;

procedure TCompareTests.ManyTest_LE;
var
  cmp : IComparer<Integer>;
  count,res : integer;

  function NestedCompareFunc(constref Left, Right : Integer ) : Integer;
  begin
    Inc(count);
    Result := TCompare.Integer(Left, Right);
  end;

begin
  count := 0;
  cmp := TComparerTool<Integer>.Many([NestedCompareFunc, NestedCompareFunc]);
  res := cmp.Compare(1,2);
  AssertEquals(1, count);
  AssertEquals(-1, res);
end;

procedure TCompareTests.ManyTest_EQ;
var
  cmp : IComparer<Integer>;
  count,res : integer;

  function NestedCompareFunc(constref Left, Right : Integer ) : Integer;
  begin
    Inc(count);
    Result := TCompare.Integer(Left, Right);
  end;

begin
  count := 0;
  cmp := TComparerTool<Integer>.Many([NestedCompareFunc, NestedCompareFunc]);
  res := cmp.Compare(1,1);
  AssertEquals(2, count);
  AssertEquals(0, res);
end;

procedure TCompareTests.ManyTest_GE;
var
  cmp : IComparer<Integer>;
  count,res : integer;

  function NestedCompareFunc(constref Left, Right : Integer ) : Integer;
  begin
    Inc(count);
    Result := TCompare.Integer(Left, Right);
  end;

begin
  count := 0;
  cmp := TComparerTool<Integer>.Many([NestedCompareFunc, NestedCompareFunc]);
  res := cmp.Compare(2,1);
  AssertEquals(1, count);
  AssertEquals(1, res);
end;

{ TFilterTests }

procedure TFilterTests.NestedFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;
  i : integer;

  function NestedFilterFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) = 0;
  end;

begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.FromFunc( NestedFilterFunc );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(5, list.Count);
  self.AssertEquals(2, list[0]);
  self.AssertEquals(4, list[1]);
  self.AssertEquals(6, list[2]);
  self.AssertEquals(8, list[3]);
  self.AssertEquals(10, list[4]);
end;

function TFilterTests.ObjectFilterFunc(constref AItem : Integer ) : boolean;
begin
  Result := (AItem mod 2) = 0;
end;

procedure TFilterTests.ObjectFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;
begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.FromFunc( ObjectFilterFunc );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(5, list.Count);
  self.AssertEquals(2, list[0]);
  self.AssertEquals(4, list[1]);
  self.AssertEquals(6, list[2]);
  self.AssertEquals(8, list[3]);
  self.AssertEquals(10, list[4]);
end;

function GlobalFilterFunc(constref AItem : Integer ) : boolean;
begin
  Result := (AItem mod 2) = 0;
end;

procedure TFilterTests.GlobalFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;
begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.FromFunc( GlobalFilterFunc );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(5, list.Count);
  self.AssertEquals(2, list[0]);
  self.AssertEquals(4, list[1]);
  self.AssertEquals(6, list[2]);
  self.AssertEquals(8, list[3]);
  self.AssertEquals(10, list[4]);
end;

procedure TFilterTests.AndManyFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;

  function NestedFilterOddFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) = 0;
  end;

    function NestedFilterEvenFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) <> 0;
  end;

begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.AndMany( [NestedFilterOddFunc, NestedFilterEvenFunc] );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(0, list.Count);
end;

procedure TFilterTests.OrManyFilter;
var
  list : TList<Integer>;
  pred : IPredicate<Integer>;
  res : Integer;
  GC : TDisposables;

  function NestedFilterOddFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) = 0;
  end;

    function NestedFilterEvenFunc(constref AItem : Integer ) : boolean;
  begin
    Result := (AItem mod 2) <> 0;
  end;

begin
  list := GC.AddObject( TList<Integer>.Create ) as TList<Integer>;
  list.AddRange([1,2,3,4,5,6,7,8,9,10]);
  pred := TPredicateTool<Integer>.OrMany( [NestedFilterOddFunc, NestedFilterEvenFunc] );
  TListTool<Integer>.FilterBy(list, pred);
  self.AssertEquals(10, list.Count);
end;

procedure TFilterTests.SetUp;
begin

end;

procedure TFilterTests.TearDown;
begin

end;

initialization
  RegisterTest(TFilterTests);
end.

