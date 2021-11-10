unit UAbstractMemBTree.Tests;

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
     procedure TestInfiniteExt(AMemUnitsSize, AOrder : Integer; AAllowDuplicates : Boolean; A64Bits : Boolean);
     procedure TestInfinite_Integer(AMemUnitsSize, AOrder: Integer; AAllowDuplicates : Boolean; A64Bits : Boolean);
     procedure DoCheckAbstractMem(AAbstractMem : TAbstractMem; AUsedBytes : Integer);
   published
     procedure TestInfinite_TAbstractMemBTree;
     procedure TestInfinite_TAbstractMemBTreeData;
   end;

implementation

{ TAbstractMemBTreeExampleInteger }

procedure TAbstractMemBTreeExampleInteger.DisposeData(var AData: TAbstractMemPosition);
begin
  // NOTE: Nothing to do NEITHER to inherit from ancestor
end;

function TAbstractMemBTreeExampleInteger.DoCompareData(const ALeftData, ARightData: TAbstractMemPosition): Integer;
begin
  Result := Integer( ALeftData - ARightData );
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
  LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount : TAbstractMemPosition;
begin
  Assert(AAbstractMem.CheckConsistency(Nil,Nil,LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount));
  Assert(LTotalUsedSize=AUsedBytes,Format('Total used %d bytes (%d blocks) different from expected %d bytes - Total free %d bytes (%d blocks)',
    [LTotalUsedSize, LTotalUsedBlocksCount, AUsedBytes, LTotalLeaksSize, LTotalLeaksBlocksCount]));
end;

procedure TestTAbstractMemBTree.SetUp;
begin
end;

procedure TestTAbstractMemBTree.TearDown;
begin
end;

function TComparison_SumChars(const ALeft, ARight: String): Integer;
  function SumChars(const AString : string) : Integer;
  var i : Integer;
  begin
    Result := 0;
    for i := 0 to AString.Length-1 do inc(Result,Ord(AString.Chars[i]));
  end;
begin
  Result := SumChars(ALeft) - SumChars(ARight);
  ALeft.GetHashCode
end;

function TComparison_HashCode(const ALeft, ARight: String): Integer;
begin
  Result := ALeft.GetHashCode - ARight.GetHashCode;
end;

procedure TestTAbstractMemBTree.TestInfinite_TAbstractMemBTree;
var LOrder, LMemUnitsSize, LInitialRandSeed : Integer;
  L64Bits, LAllowDuplicates : Boolean;
  s64Bits, sAllowDuplicates : String;
begin
  LInitialRandSeed := RandSeed;
  LOrder := 3;
  LMemUnitsSize := 4;
  L64Bits := False;
  LAllowDuplicates := False;
  try
    repeat
      LMemUnitsSize := ((Random(255) DIV 4)*4)+4;
      LAllowDuplicates := Random(2)=0;
      L64Bits := Random(2)=0;
      TestInfinite_Integer(LMemUnitsSize,LOrder,LAllowDuplicates,L64Bits);
      inc(LOrder);
    until (LOrder>11);
  Except
    On E:Exception do begin
      if L64Bits then s64Bits := '64bits' else s64Bits := '32bits';
      if LAllowDuplicates then sAllowDuplicates := 'Duplicates' else sAllowDuplicates := 'Unique';

      E.Message := Format('Seed:%d Order:%d MUS:%d %s %s Error(%s):%s',[LInitialRandSeed,LOrder,LMemUnitsSize,s64Bits,sAllowDuplicates,E.ClassName,E.Message]);
      Raise;
    end;
  end;
end;

procedure TestTAbstractMemBTree.TestInfinite_TAbstractMemBTreeData;
var LOrder, LMemUnitsSize, LInitialRandSeed : Integer;
  L64Bits, LAllowDuplicates : Boolean;
  s64Bits, sAllowDuplicates : String;
begin
  LInitialRandSeed := RandSeed;
  LOrder := 3;
  LMemUnitsSize := 4;
  L64Bits := False;
  LAllowDuplicates := False;
  try
    repeat
      LMemUnitsSize := ((Random(255) DIV 4)*4)+4;
      LAllowDuplicates := Random(2)=0;
      L64Bits := Random(2)=0;
      TestInfiniteExt(LMemUnitsSize,LOrder,LAllowDuplicates,L64Bits);
      inc(LOrder);
    until (LOrder>11);
  Except
    On E:Exception do begin
      if L64Bits then s64Bits := '64bits' else s64Bits := '32bits';
      if LAllowDuplicates then sAllowDuplicates := 'Duplicates' else sAllowDuplicates := 'Unique';

      E.Message := Format('Seed:%d Order:%d MUS:%d %s %s Error(%s):%s',[LInitialRandSeed,LOrder,LMemUnitsSize,s64Bits,sAllowDuplicates,E.ClassName,E.Message]);
      Raise;
    end;
  end;
end;

procedure TestTAbstractMemBTree.TestInfiniteExt(AMemUnitsSize, AOrder: Integer; AAllowDuplicates, A64Bits: Boolean);
var
  Lbt : TAbstractMemBTreeExampleString;

  procedure ProcessTree(ATotalRounds : Integer);
  var LzoneIndex : TAMZone;
  j : TAbstractMemPosition;
  intValue, nRounds, nAdds, nDeletes, i, intAux : Integer;
  LCurr, LnextCurr : String;
  begin
    repeat
      inc(nRounds);
      intValue := Random(AOrder * 100);
      if Random(5)>0 then begin
        if (Lbt.AddData(intValue.ToString)) then begin
          inc(nAdds);
        end;
      end else begin
        if Lbt.DeleteData(intValue.ToString) then begin
          inc(nDeletes);
        end;
      end;
    until (nRounds>=ATotalRounds);
    Lbt.CheckConsistency;
    // Delete mode
    while Lbt.Count>0 do begin
      if not Lbt.FindDataLowest(LCurr) then raise Exception.Create('Cannot fint lowest but Count>0');
      if not Lbt.FindData(LCurr,LzoneIndex.position) then raise Exception.Create(Format('"%s" Not Found %d',[LCurr,Lbt.Count]));
      while (Random(50)>0) do begin
        if Random(3)=0 then begin
          if not Lbt.FindDataPrecessor(Lcurr,LnextCurr) then begin
            break;
          end;
          LCurr := LnextCurr;
        end else if Random(2)=0 then begin
          if not Lbt.FindDataSuccessor(LCurr,LnextCurr) then begin
            break;
          end;
          LCurr := LnextCurr;
        end;
      end;
      If Not Lbt.DeleteData(LCurr) then raise Exception.Create(Format('"%s" Not Found to delete! %d',[LCurr,Lbt.Count]));
      Lbt.CheckConsistency;
    end;
    Lbt.CheckConsistency;
    // Try to re-use
    i := 0;
    intValue := 10;
    repeat
      inc(intValue);
      if (Lbt.CanAddData(intValue.ToString)) then begin
        inc(i);
        Assert(Lbt.AddData(intValue.ToString),Format('Cannot re-use (round %d on order %d) and add %d',[i,AOrder,intValue]));
        Assert(Lbt.FindIndex(i-1,j),Format('Cannot find %d on index %d on order %d',[intValue,i-1,AOrder]));
        Assert(Not Lbt.FindIndex(i,j),Format('Found %d on index %d on order %d',[j,i-1,AOrder]));
      end;
    until Lbt.Count>(AOrder * 10);
  end;

  procedure ProcessSaveToStream(AAbstractMem : TAbstractMem);
  var LStream : TStream;
    LStreamMem : TStreamMem;
  begin
    LStream := TMemoryStream.Create;
    Try
      AAbstractMem.SaveToStream(LStream);
      //
      LStreamMem := TStreamMem.Create(LStream,0,True);
      Try
        Assert( LStreamMem.HeaderInitialized , 'No valid Stream');
        LStreamMem.CheckConsistency;
      Finally
        LStreamMem.Free;
      End;
    Finally
      LStream.Free;
    End;
  end;


var
  LzoneData,
  LzoneIndex : TAMZone;
  Lmem : TAbstractMem;
  i : Integer;
begin
  Lmem := TMem.Create(0,False);
  Try
    LMem.Initialize(A64Bits,AMemUnitsSize);
    LzoneData := Lmem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(Lmem));
    try
      Lbt := TAbstractMemBTreeExampleString.Create(Lmem,LzoneData,AAllowDuplicates,AOrder,TComparison_String);
      try
        TAbstractMemBTreeDataIndex<String>.Create(Lbt,
          Lmem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(Lmem)),False,
          AOrder+1,TComparison_SumChars);
        TAbstractMemBTreeDataIndex<String>.Create(Lbt,
          Lmem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(Lmem)),True,
          AOrder+1,TComparison_HashCode);
        ProcessTree(AOrder * 1000);
      finally
        // Dispose indexes
        for i := Lbt.Indexes.Count-1 downto 0 do begin
          LzoneIndex := Lbt.Indexes.Items[i].InitialZone;
          Lbt.Indexes.Items[i].EraseTree;
          Lbt.Indexes.Items[i].Free;
          Lmem.Dispose( LzoneIndex );
        end;
        Lbt.Free;
      end;
      Lbt := TAbstractMemBTreeExampleString.Create(Lmem,LzoneData,AAllowDuplicates,AOrder,TComparison_String);
      try
        Lbt.CheckConsistency;
        Lbt.EraseTree;
        Lbt.CheckConsistency;
      finally
        Lbt.Free;
      end;
    finally
      Lmem.Dispose(LzoneData);
    end;
    //
    DoCheckAbstractMem(Lmem,0);
    //
    ProcessSaveToStream(Lmem);
  Finally
    Lmem.Free;
  End;
end;

procedure TestTAbstractMemBTree.TestInfinite_Integer(AMemUnitsSize, AOrder: Integer; AAllowDuplicates : Boolean; A64Bits : Boolean);
var Lbt : TAbstractMemBTreeExampleInteger;
  Lzone : TAMZone;
  intValue, nRounds, nAdds, nDeletes, i, intAux : Integer;
  j : TAbstractMemPosition;
  Lnode : TAbstractMemBTreeExampleInteger.TAbstractBTreeNode;
  Lmem : TAbstractMem;
  LCurr : String;
begin
  Lmem := TMem.Create(0,False);
  Try
    LMem.Initialize(A64Bits,AMemUnitsSize);
    nRounds := 0;
    nAdds := 0;
    nDeletes := 0;
    Lzone := Lmem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(Lmem));
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
      until (nRounds>=AOrder * 1000);
      Lbt.CheckConsistency;
      // Delete mode
      while Lbt.Count>0 do begin
        Lnode := Lbt.Root;
        while (Not Lnode.IsLeaf) and (Random(5)>0) do begin
          Lnode := Lbt.GetNode(Lnode.childs[Random(Lnode.Count)+1]);
        end;
        If Not Lbt.Delete(Lnode.data[Random(Lnode.Count)]) then raise Exception.Create('Not Found to delete!');
      end;
      Lbt.CheckConsistency;
      // Try to re-use
      i := 0;
      repeat
        intValue := Random(AOrder * 100);
        if (not Lbt.Find(intValue,Lnode,intAux)) or (AAllowDuplicates) then begin
          inc(i);
          Assert(Lbt.Add(intValue),Format('Cannot re-use %d/%d and add %d',[i,AOrder,intValue]));
          Assert(Lbt.FindIndex(i-1,j),Format('Cannot find %d on index %d on order %d',[intValue,i-1,AOrder]));
          Assert(Not Lbt.FindIndex(i,j),Format('Found %d on index %d on order %d',[j,i-1,AOrder]));
        end;
      until Lbt.Count>(AOrder * 10);
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


initialization
  RegisterTest(TestTAbstractMemBTree{$IFNDEF FPC}.Suite{$ENDIF});
end.
