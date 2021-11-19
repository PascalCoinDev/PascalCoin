unit UAbstractBTree.Tests;

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
   UAbstractBTree, UOrderedList, UAbstractMem;

type

  TIntegerBTree = Class( TMemoryBTree<Integer> )
  private
  protected
  public
    constructor Create(AAllowDuplicates : Boolean; AOrder: Integer);
    function NodeDataToString(const AData : Integer) : String; override;
  End;


   TestTAbstractBTree = class(TTestCase)
   strict private
   public
     procedure SetUp; override;
     procedure TearDown; override;
     procedure TestInfinite(AOrder : Integer);
   published
     procedure Test_duplicate;
     procedure TestInsert;
     procedure TestDelete;
     procedure TestInfiniteOrder_3;
     procedure TestInfiniteOrder_4;
     procedure TestInfiniteOrder_5;
     procedure TestInfiniteOrder_6;
     procedure TestInfiniteOrder_7;
     procedure TestPrecessorSuccessor;
     procedure TestPrecessorSuccessor_Duplicates;
   end;

implementation

{ TIntegerBTree }

constructor TIntegerBTree.Create(AAllowDuplicates: Boolean; AOrder: Integer);
begin
  inherited Create(TComparison_Integer,AAllowDuplicates,AOrder);
end;

function TIntegerBTree.NodeDataToString(const AData: Integer): String;
begin
  Result := AData.ToString;
end;

{ TestTAbstractBTree }

procedure TestTAbstractBTree.SetUp;
begin
end;

procedure TestTAbstractBTree.TearDown;
begin
end;

procedure TestTAbstractBTree.TestInfinite(AOrder : Integer);
var Lbt : TIntegerBTree;
  intValue, nRounds, nAdds, nDeletes, i : Integer;
  Lnode : TIntegerBTree.TAbstractBTreeNode;
begin
  {$IFDEF FPC}
  Randomize;
  {$ELSE}
  RandomizeProc(0);
  {$ENDIF}
  nRounds := 0;
  nAdds := 0;
  nDeletes := 0;
  Lbt := TIntegerBTree.Create(True,AOrder);
  try
    Lbt.CircularProtection := (AOrder MOD 2)=0;
    repeat
      inc(nRounds);
      intValue := Random(AOrder * 100);
      if Random(2)=0 then begin
        if (Lbt.Add(intValue)) then begin
          inc(nAdds);
        end;
      end else begin
        if Lbt.Delete(intValue) then begin
          inc(nDeletes);
        end;
      end;
      if Random(100)=0 then begin
        Lbt.CheckConsistency;
      end;
    until (nRounds>=AOrder * 10000);
    Lbt.CheckConsistency;
    // Delete mode
    while Lbt.Count>0 do begin
      Lnode := Lbt.Root;
      while (Not Lnode.IsLeaf) and (Random(5)>0) do begin
        Lnode := Lbt.GetNode(Lnode.childs[Random(Lnode.Count)+1]);
      end;
      Lbt.Delete(Lnode.data[Random(Lnode.Count)]);
      if Random(100)=0 then begin
        Lbt.CheckConsistency;
      end;
    end;
    Lbt.CheckConsistency;
    // Try to re-use
    for i := 1 to AOrder do begin
      intValue := Random(AOrder * 100);
      Assert(Lbt.Add(intValue),Format('Cannot re-use %d/%d and add %d',[i,AOrder,intValue]));
      Lbt.CheckConsistency;
    end;
  finally
    Lbt.Free;
  end;

end;

procedure TestTAbstractBTree.TestInfiniteOrder_3;
begin
  TestInfinite(3);
end;

procedure TestTAbstractBTree.TestInfiniteOrder_4;
begin
  TestInfinite(4);
end;

procedure TestTAbstractBTree.TestInfiniteOrder_5;
begin
  TestInfinite(5);
end;

procedure TestTAbstractBTree.TestInfiniteOrder_6;
begin
  TestInfinite(6);
end;

procedure TestTAbstractBTree.TestInfiniteOrder_7;
begin
  TestInfinite(7);
end;

procedure TestTAbstractBTree.TestInsert;
var Lbt : TIntegerBTree;
  Lorder, i, intValue : Integer;
begin
  for Lorder := 3 to 5 do begin
    Lbt := TIntegerBTree.Create(False,Lorder);
    try
      i := 1;
      repeat
        intValue := i;
        inc(i);
        Lbt.Add(intValue);
        Lbt.CheckConsistency;
      until Lbt.Height>6;
    finally
      Lbt.Free;
    end;
  end;
  for Lorder := 3 to 5 do begin
    Lbt := TIntegerBTree.Create(False,Lorder);
    try
      i := 10000;
      repeat
        intValue := i;
        dec(i);
        Lbt.Add(intValue);
        Lbt.CheckConsistency;
      until Lbt.Height>6;
    finally
      Lbt.Free;
    end;
  end;
  for Lorder := 3 to 5 do begin
    Lbt := TIntegerBTree.Create(False,Lorder);
    try
      repeat
        intValue := Random(50000);
        Lbt.Add(intValue);
        Lbt.CheckConsistency;
      until Lbt.Height>6;
    finally
      Lbt.Free;
    end;
  end;
end;

procedure TestTAbstractBTree.TestPrecessorSuccessor;
var Lbt : TIntegerBTree;
  Lorder : Integer;
  i, intValue, valMin, valMax, Lregs : Integer;

begin
  for Lorder := 3 to 7 do begin
    Lbt := TIntegerBTree.Create(False,Lorder);
    try
      Lbt.CircularProtection := (Lorder MOD 2)=0;
      valMin := 1;
      intValue :=valMin;
      Lregs := 0;
      while Lbt.Height<Lorder+1 do begin
        Lbt.Add(intValue);
        valMax := intValue;
        inc(intValue);
        inc(Lregs);
      end;
      Assert(Lbt.FindLowest(i),'Find lowest');
      Assert(i=valMin,Format('Lowest <> %d',[valMin]));
      Assert(Lbt.FindHighest(i),'Find highest');
      Assert(i=valMax,Format('Highest <> %d',[valMax]));
      Lbt.FindLowest(intValue);
      i := 1;
      while (Lbt.FindSuccessor(intValue,intValue)) do begin
        inc(i);
      end;
      Assert(intValue=valMax,Format('Successor %d<>%d',[intValue,valMax]));
      Assert(i=Lregs,Format('Succcessor count %d %d',[i,Lregs]));
      Lbt.FindHighest(intValue);
      i := 1;
      while (Lbt.FindPrecessor(intValue,intValue)) do begin
        inc(i);
      end;
      Assert(intValue=valMin,Format('Precessor %d<>%d',[intValue,valMin]));
      Assert(i=Lregs,Format('Precessor count %d %d',[i,Lregs]));

    finally
      Lbt.Free;
    end;

  end;
end;

procedure TestTAbstractBTree.TestPrecessorSuccessor_Duplicates;
var Lbt : TIntegerBTree;
  Lorder,
  i, intValue, valMin, valMax, Lregs : Integer;
begin
  for Lorder := 3 to 7 do begin
    Lbt := TIntegerBTree.Create(True,Lorder);
    try
      Lbt.CircularProtection := (Lorder MOD 2)=0;
      valMin := 1;
      intValue :=valMin;
      Lregs := 0;
      while Lbt.Height<Lorder+1 do begin
        Lbt.Add(intValue);
        valMax := intValue;
        if (Lregs MOD Lorder)=0 then inc(intValue);
        inc(Lregs);
      end;
      Assert(Lbt.FindLowest(i),'Find lowest');
      Assert(i=valMin,Format('Lowest <> %d',[valMin]));
      Assert(Lbt.FindHighest(i),'Find highest');
      Assert(i=valMax,Format('Highest <> %d',[valMax]));
      Lbt.FindLowest(intValue);
      i := 1;
      while (Lbt.FindSuccessor(intValue,intValue)) do begin
        inc(i);
      end;
      Assert(intValue=valMax,Format('Successor %d<>%d',[intValue,valMax]));
      Lbt.FindHighest(intValue);
      i := 1;
      while (Lbt.FindPrecessor(intValue,intValue)) do begin
        inc(i);
      end;
      Assert(intValue=valMin,Format('Precessor %d<>%d',[intValue,valMin]));
    finally
      Lbt.Free;
    end;

  end;
end;

procedure TestTAbstractBTree.Test_duplicate;
var Lbt : TIntegerBTree;
  Lorder, i, intValue : Integer;
  LLastTree,LCurrentTree : String;

  procedure DoInsert(AValue : Integer);
  begin
    Lbt.Add(AValue);
  end;

  procedure DoDelete(AValue : Integer);
  begin
    Lbt.Delete(AValue);
  end;

begin
  {$IFDEF FPC}
  Randomize;
  {$ELSE}
  RandomizeProc(0);
  {$ENDIF}
  for Lorder := 3 to 7 do begin
    Lbt := TIntegerBTree.Create(True,Lorder);
    try
      LLastTree := '';
      LCurrentTree := '';
      i :=1;
      while Lbt.Height<Lorder+1 do begin
        intValue := Random(100);
        DoInsert(intValue);
        inc(i);
      end;

      LCurrentTree := Lbt.BTreeToString;
      Lbt.CheckConsistency;

      i := 0;

      // Tree is ready to delete
      while (Lbt.Count>0) do begin
        Lbt.FindHighest(i);
        intValue := Random(i+1);
        DoDelete(intValue);
      end;
      LCurrentTree := Lbt.BTreeToString;
      Lbt.CheckConsistency;
    finally
      Lbt.Free;
    end;

  end;
end;

procedure TestTAbstractBTree.TestDelete;
var Lbt : TIntegerBTree;
  Lorder, i, intValue : Integer;
  LLastTree, LCurrentTree : String;

  procedure DoDelete(AValue : Integer);
  begin
    Lbt.Delete(AValue);
    LCurrentTree := Lbt.BTreeToString;
    Lbt.CheckConsistency;
    LLastTree := LCurrentTree;
  end;

begin
  for Lorder := 3 to 6 do begin
    Lbt := TIntegerBTree.Create(False,Lorder);
    try
      LLastTree := '';
      LCurrentTree := '';
      i :=1;
      while Lbt.Height<Lorder+1 do begin
        intValue := i;
        Lbt.Add(intValue);
        inc(i);
      end;

      LCurrentTree := Lbt.BTreeToString;
      Lbt.CheckConsistency;
      i := 0;

      DoDelete(1);
      DoDelete(13);
      DoDelete(8);
      DoDelete(4);
      DoDelete(6);
      DoDelete(5);
      DoDelete(12);
      DoDelete(14);
      DoDelete(9);

      // Tree is ready to delete
      while (Lbt.Count>0) do begin
        inc(i);
        Lbt.FindHighest(intValue);
        intValue := Random(intValue)+1;
        DoDelete(intValue);
      end;
    finally
      Lbt.Free;
    end;

  end;
end;


initialization
  RegisterTest(TestTAbstractBTree{$IFNDEF FPC}.Suite{$ENDIF});
end.
