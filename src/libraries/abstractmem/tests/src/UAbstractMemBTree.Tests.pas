unit UAbstractMemBTree.Tests;

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
   {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults,{$ELSE}Generics.Collections,Generics.Defaults,{$ENDIF}
   UAbstractMem, UAbstractBTree.Tests,
   UAbstractBTree, UOrderedList, UAbstractMemBTree;

type
   TAbstractMemBTreeExampleInteger = Class(TAbstractMemBTree)
   protected
     procedure DisposeData(var AData : TAbstractMemPosition); override;
     function DoCompareData(const ALeftData, ARightData: TAbstractMemPosition): Integer; override;
   public
     function NodeDataToString(const AData : TAbstractMemPosition) : String; override;
   End;

   TAbstractMemBTreeExampleString = Class(TAbstractMemBTreeData<String>)
   protected
     function LoadData(const APosition : TAbstractMemPosition) : String; override;
     function SaveData(const AData : String) : TAMZone; override;
   public
     function NodeDataToString(const AData : TAbstractMemPosition) : String; override;
   End;

   TestTAbstractMemBTree = class(TTestCase)
   strict private
   public
     procedure SetUp; override;
     procedure TearDown; override;
     procedure TestInfinite_Integer(AOrder : Integer; AAllowDuplicates : Boolean);
     procedure TestInfinite_String(AOrder : Integer; AAllowDuplicates : Boolean);
     procedure TestInfinite(AOrder : Integer);
     procedure DoCheckAbstractMem(AAbstractMem : TAbstractMem; AUsedBytes : Integer);
   published
     procedure TestInfiniteOrder_3;
     procedure TestInfiniteOrder_4;
     procedure TestInfiniteOrder_5;
     procedure TestInfiniteOrder_6;
     procedure TestInfiniteOrder_7;
   end;

implementation

{ TAbstractMemBTreeExampleInteger }

procedure TAbstractMemBTreeExampleInteger.DisposeData(var AData: TAbstractMemPosition);
begin
  // NOTE: Nothing to do NEITHER to inherit from ancestor
end;

function TAbstractMemBTreeExampleInteger.DoCompareData(const ALeftData, ARightData: TAbstractMemPosition): Integer;
begin
  Result := ALeftData - ARightData;
end;

function TAbstractMemBTreeExampleInteger.NodeDataToString(const AData: TAbstractMemPosition): String;
begin
  Result := IntToStr(AData);
end;

{ TAbstractMemBTreeExampleString }

function TAbstractMemBTreeExampleString.LoadData(const APosition: TAbstractMemPosition): String;
var i : Integer;
  wLength : Word;
  Lbuff : TBytes;
begin
  Result := '';
  wLength := 0;
  FAbstractMem.Read(APosition,wLength,2);
  if wLength<=0 then Exit;
  SetLength(Lbuff,wLength);
  FAbstractMem.Read(APosition+2,LBuff[0],wLength);
  for i:=0 to wLength-1 do begin
    Result := Result + Char(LBuff[i]);
  end;
end;

function TAbstractMemBTreeExampleString.NodeDataToString(const AData: TAbstractMemPosition): String;
begin
  Result := LoadData(AData);
end;

function TAbstractMemBTreeExampleString.SaveData(const AData: String): TAMZone;
var i : Integer;
  wLength : Word;
  Lbuff : TBytes;
begin
  wLength := Length(AData);
  Result := FAbstractMem.New( wLength+2 );
  SetLength(Lbuff,wLength+2);
  Move(wLength,Lbuff[0],2);
  for i:=0 to AData.Length-1 do begin
    Lbuff[2 + i] := Byte(Char(AData.Chars[i]));
  end;
  FAbstractMem.Write(Result.position,Lbuff[0],Length(Lbuff));
end;

{ TestTAbstractMemBTree }

procedure TestTAbstractMemBTree.DoCheckAbstractMem(AAbstractMem: TAbstractMem; AUsedBytes: Integer);
var
  LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount : Integer;
begin
  Assert(AAbstractMem.CheckConsistency(Nil,LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount));
  Assert(LTotalUsedSize=AUsedBytes,Format('Total used %d bytes (%d blocks) different from expected %d bytes - Total free %d bytes (%d blocks)',[LTotalUsedSize, AUsedBytes, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount]));
end;

procedure TestTAbstractMemBTree.SetUp;
begin
end;

procedure TestTAbstractMemBTree.TearDown;
begin
end;

procedure TestTAbstractMemBTree.TestInfinite(AOrder: Integer);
begin
  TestInfinite_Integer(AOrder,(AOrder MOD 2)=0);
  TestInfinite_String(AOrder,(AOrder MOD 2)=0);
end;

procedure TestTAbstractMemBTree.TestInfinite_Integer(AOrder : Integer; AAllowDuplicates : Boolean);
var Lbt : TAbstractMemBTreeExampleInteger;
  Lbts : TAbstractMemBTreeExampleString;
  Lzone : TAMZone;
  intValue, nRounds, nAdds, nDeletes, i, j : Integer;
  Lnode : TIntegerBTree.TAbstractBTreeNode;
  Lmem : TAbstractMem;
  LCurr : String;
begin
  Lmem := TMem.Create(0,False);
  Try
    {$IFDEF FPC}
    Randomize;
    {$ELSE}
    RandomizeProc(0);
    {$ENDIF}
    nRounds := 0;
    nAdds := 0;
    nDeletes := 0;
    Lzone := Lmem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize);
    try
    Lbt := TAbstractMemBTreeExampleInteger.Create(Lmem,Lzone,AAllowDuplicates,AOrder);
    try
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
        If Not Lbt.Delete(Lnode.data[Random(Lnode.Count)]) then raise Exception.Create('Not Found to delete!');
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
        Assert(Lbt.FindIndex(i-1,j),Format('Cannot find %d on index %d on order %d',[intValue,i-1,AOrder]));
        Assert(Not Lbt.FindIndex(i,j),Format('Found %d on index %d on order %d',[j,i-1,AOrder]));
      end;
    finally
      Lbt.Free;
    end;
    Lbt := TAbstractMemBTreeExampleInteger.Create(Lmem,Lzone,AAllowDuplicates,AOrder);
    try
      Lbt.CheckConsistency;
      Lbt.EraseTree;
      Lbt.CheckConsistency;
    finally
      Lbt.Free;
    end;
    finally
      Lmem.Dispose(Lzone);
    end;
    DoCheckAbstractMem(Lmem,0);
  Finally
    Lmem.Free;
  End;
end;

procedure TestTAbstractMemBTree.TestInfinite_String(AOrder: Integer; AAllowDuplicates : Boolean);
var Lbt : TAbstractMemBTreeExampleString;
  Lzone : TAMZone;
  intValue, nRounds, nAdds, nDeletes, i : Integer;
  Lnode : TIntegerBTree.TAbstractBTreeNode;
  Lmem : TAbstractMem;
  LCurr : String;
  LCurrData : String;
begin
  Lmem := TMem.Create(0,False);
  Try
    {$IFDEF FPC}
    Randomize;
    {$ELSE}
    RandomizeProc(0);
    {$ENDIF}
    nRounds := 0;
    nAdds := 0;
    nDeletes := 0;
    Lzone := Lmem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize);
    try
    Lbt := TAbstractMemBTreeExampleString.Create(Lmem,Lzone,AAllowDuplicates,AOrder,TComparison_String);
    try
      repeat
        inc(nRounds);
        intValue := Random(AOrder * 100);
        if Random(2)=0 then begin
          if (Lbt.AddData(intValue.ToString)) then begin
            inc(nAdds);
          end;
        end else begin
          if Lbt.DeleteData(intValue.ToString) then begin
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
        LCurrData := Lbt.LoadData(Lnode.data[Random(Lnode.Count)]);
        if Not Lbt.DeleteData(LCurrData) then raise EAbstractMemBTree.Create('Not found to delete!');
        if Random(100)=0 then begin
          Lbt.CheckConsistency;
        end;
      end;
      Lbt.CheckConsistency;
      // Try to re-use
      for i := 1 to AOrder do begin
        intValue := i;
        Assert(Lbt.AddData(intValue.ToString),Format('Cannot re-use %d/%d and add %d',[i,AOrder,intValue]));
        Lbt.CheckConsistency;
      end;
    finally
      Lbt.Free;
    end;
    Lbt := TAbstractMemBTreeExampleString.Create(Lmem,Lzone,AAllowDuplicates,AOrder,TComparison_String);
    try
      Lbt.CheckConsistency;
      LCurr := Lbt.BTreeToString;
      // SUCCESSOR
      Assert(Lbt.FindDataLowest(LCurrData),'Not found Lowest');
      Assert(LcurrData='1','Not valid lowest');
      for i := 1 to AOrder do begin
        Assert(i.ToString=LcurrData,Format('Not valid successor %d %s',[i,LcurrData]));
        if i<AOrder then begin
          Assert(Lbt.FindDataSuccessor(LcurrData,LCurrData),Format('Not found successor %d %s',[i,LcurrData]));
        end else begin
          Assert(Not Lbt.FindDataSuccessor(LCurrData,LCurrData),Format('Not valid last successor %s',[LCurrData]));
        end;
      end;
      // PRECESSOR
      Assert(Lbt.FindDataHighest(LCurrData),'Not found Highest');
      Assert(LcurrData=IntToStr(AOrder),'Not valid highest');
      for i := AOrder downto 1 do begin
        Assert(i.ToString=LcurrData,Format('Not valid precessor %d %s',[i,LcurrData]));
        if i>1 then begin
          Assert(Lbt.FindDataPrecessor(LcurrData,LCurrData),Format('Not found precessor %d %s',[i,LcurrData]));
        end else begin
          Assert(Not Lbt.FindDataPrecessor(LCurrData,LCurrData),Format('Not valid last precessor %s',[LCurrData]));
        end;
      end;
      Lbt.EraseTree;
      Assert(Lbt.Count=0,'Not erased tree count 0');
      Lbt.CheckConsistency;
      Lbt.EraseTree;
    finally
      Lbt.Free;
    end;
    finally
      Lmem.Dispose(Lzone);
    end;
    DoCheckAbstractMem(Lmem,0);
  Finally
    Lmem.Free;
  End;
end;

procedure TestTAbstractMemBTree.TestInfiniteOrder_3;
begin
  TestInfinite(3);
end;

procedure TestTAbstractMemBTree.TestInfiniteOrder_4;
begin
  TestInfinite(4);
end;

procedure TestTAbstractMemBTree.TestInfiniteOrder_5;
begin
  TestInfinite(5);
end;

procedure TestTAbstractMemBTree.TestInfiniteOrder_6;
begin
  TestInfinite(6);
end;

procedure TestTAbstractMemBTree.TestInfiniteOrder_7;
begin
  TestInfinite(7);
end;

initialization
  RegisterTest(TestTAbstractMemBTree{$IFNDEF FPC}.Suite{$ENDIF});
end.
