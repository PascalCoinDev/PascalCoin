unit UAbstractStorage.Tests;

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
   System.IOUtils,
   {$ENDIF}
   {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults,{$ELSE}Generics.Collections,Generics.Defaults,{$ENDIF}
   UAbstractStorage, UAbstractMem, UAbstractMemBTree, UAbstractMemTList;

{$I ./../../ConfigAbstractMem.inc }

type
   TestTAbstractStorage = class(TTestCase)
   strict private
   private
     procedure InitializeStorage(ASender : TAbstractStorage; ACurrentVersion, AUpdateToVersion : Integer);
     procedure AddData(ASender: TAbstractStorage);
     procedure CheckAddedData(ASender: TAbstractStorage);
   public
     procedure SetUp; override;
     procedure TearDown; override;
   published
     procedure Test_TAbstractStorage;
   end;

implementation

uses UOrderedList, UAbstractMemTList.Tests, UAbstractMemBTree.Tests;

{ TestTAbstractStorage }

procedure TestTAbstractStorage.AddData(ASender: TAbstractStorage);
var  i : Integer;
  aml_1 : TAbstractMemTList;
  aml_int : TAbstractMemTList_TTestRecord;
  aml_int_ord : TAbstractMemOrderedTList_TTestRecord;
  ambt_1 : TAbstractMemBTree;
  ambt_int : TAbstractMemBTreeDataExampleInteger;
  ambt_index : TAbstractMemBTreeDataIndex<Integer>;
begin
  aml_1 := TAbstractMemTList.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemTList'),5,True);
  aml_int := TAbstractMemTList_TTestRecord.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemTList<>'),5,True);
  aml_int_ord := TAbstractMemOrderedTList_TTestRecord.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemOrderedTList<>'),5,True,True);
  ambt_1 := TAbstractMemBTree.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemBTree'),False,5);
  ambt_int := TAbstractMemBTreeDataExampleInteger.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemBTreeData<Integer>'),False,5,TComparison_Integer);
  ambt_index := TAbstractMemBTreeDataIndex<Integer>.Create(ambt_int,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemBTreeDataAbstract<Integer>'),False,5,TComparison_Integer);
  try
    for i := 1 to 100 do begin
      aml_1.Add(ASender.AbstractMem.New(50).position);
      aml_int.AddRandom;
      aml_int_ord.AddRandom;
      ambt_1.Add(ASender.AbstractMem.New(50).position);
      ambt_int.AddData(Random(5000));
    end;
  finally
    aml_1.Free;
    aml_int.Free;
    aml_int_ord.Free;
    ambt_1.Free;
    ambt_int.Free;
    ambt_index.Free;
  end;
end;

procedure TestTAbstractStorage.CheckAddedData(ASender: TAbstractStorage);
var  i : Integer;
  aml_1 : TAbstractMemTList;
  aml_int : TAbstractMemTList_TTestRecord;
  aml_int_ord : TAbstractMemOrderedTList_TTestRecord;
  ambt_1 : TAbstractMemBTree;
  ambt_int : TAbstractMemBTreeDataExampleInteger;
  ambt_index : TAbstractMemBTreeDataIndex<Integer>;
begin
  aml_1 := TAbstractMemTList.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemTList'),5,True);
  aml_int := TAbstractMemTList_TTestRecord.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemTList<>'),5,True);
  aml_int_ord := TAbstractMemOrderedTList_TTestRecord.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemOrderedTList<>'),5,True,True);
  ambt_1 := TAbstractMemBTree.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemBTree'),False,5);
  ambt_int := TAbstractMemBTreeDataExampleInteger.Create(ASender.AbstractMem,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemBTreeData<Integer>'),False,5,TComparison_Integer);
  ambt_index := TAbstractMemBTreeDataIndex<Integer>.Create(ambt_int,ASender.StorageStructInformation.GetStorageStructAMZone('TAbstractMemBTreeDataAbstract<Integer>'),False,5,TComparison_Integer);
  try
    Assert(aml_1.Count=100);
    Assert(aml_int.Count=100);
    Assert(aml_int_ord.Count=100);
    Assert(ambt_1.Count=100);
    Assert(ambt_int.Count=100);
    Assert(ambt_index.Count=100);
  finally
    aml_1.Free;
    aml_int.Free;
    aml_int_ord.Free;
    ambt_1.Free;
    ambt_int.Free;
    ambt_index.Free;
  end;
end;

procedure TestTAbstractStorage.InitializeStorage(ASender: TAbstractStorage; ACurrentVersion, AUpdateToVersion: Integer);
var amz : TAMZone;
  aml_1 : TAbstractMemTList;
  aml_int : TAbstractMemTList_TTestRecord;
  aml_int_ord : TAbstractMemOrderedTList_TTestRecord;
  ambt_1 : TAbstractMemBTree;
  ambt_int : TAbstractMemBTreeDataExampleInteger;
  ambt_index : TAbstractMemBTreeDataIndex<Integer>;
  i : Integer;
begin
  if ACurrentVersion>=AUpdateToVersion then raise EAbstractStorage.Create('Invalid update version');
  aml_1 := Nil;
  aml_int := Nil;
  aml_int_ord := Nil;
  ambt_1 := Nil;
  ambt_int := Nil;
  ambt_index := Nil;
  try
    if ACurrentVersion<1 then begin
      amz := ASender.AbstractMem.New(TAbstractMemTList.MinAbstractMemTListHeaderSize(ASender.AbstractMem));
      aml_1 := TAbstractMemTList.Create(ASender.AbstractMem,amz,5,True);
      ASender.StorageStructInformation.AddStorageStruct('TAbstractMemTList',amz.position,ss_TAbstractMemTList,aml_1);
      //
      amz := ASender.AbstractMem.New(TAbstractMemTList.MinAbstractMemTListHeaderSize(ASender.AbstractMem));
      aml_int := TAbstractMemTList_TTestRecord.Create(ASender.AbstractMem,amz,5,True);
      ASender.StorageStructInformation.AddStorageStruct('TAbstractMemTList<>',amz.position,ss_TAbstractMemTListT,aml_int);
      //
      amz := ASender.AbstractMem.New(TAbstractMemTList.MinAbstractMemTListHeaderSize(ASender.AbstractMem));
      aml_int_ord := TAbstractMemOrderedTList_TTestRecord.Create(ASender.AbstractMem,amz,5,True,True);
      ASender.StorageStructInformation.AddStorageStruct('TAbstractMemOrderedTList<>',amz.position,ss_TAbstractMemOrderedTListT,aml_int_ord);
      //
      amz := ASender.AbstractMem.New(TAbstractMemBTree.MinAbstractMemInitialPositionSize(ASender.AbstractMem));
      ambt_1 := TAbstractMemBTree.Create(ASender.AbstractMem,amz,False,5);
      ASender.StorageStructInformation.AddStorageStruct('TAbstractMemBTree',amz.position,ss_TAbstractMemBTree,ambt_1);
      //
      amz := ASender.AbstractMem.New(TAbstractMemBTreeDataAbstract<Integer>.MinAbstractMemInitialPositionSize(ASender.AbstractMem));
      ambt_int := TAbstractMemBTreeDataExampleInteger.Create(ASender.AbstractMem,amz,False,5,TComparison_Integer);
      ASender.StorageStructInformation.AddStorageStruct('TAbstractMemBTreeData<Integer>',amz.position,ss_TAbstractMemBTree,ambt_int);
      //
      amz := ASender.AbstractMem.New(TAbstractMemBTreeDataAbstract<Integer>.MinAbstractMemInitialPositionSize(ASender.AbstractMem));
      ambt_index := TAbstractMemBTreeDataIndex<Integer>.Create(ambt_int,amz,False,5,TComparison_Integer);
      ASender.StorageStructInformation.AddStorageStruct('TAbstractMemBTreeDataAbstract<Integer>',amz.position,ss_TAbstractMemBTree,ambt_index);
    end;
  finally
    aml_1.Free;
    aml_int.Free;
    aml_int_ord.Free;
    ambt_1.Free;
    ambt_int.Free;
    ambt_index.Free;
  end;

end;

procedure TestTAbstractStorage.SetUp;
begin
  inherited;

end;

procedure TestTAbstractStorage.TearDown;
begin
  inherited;

end;

procedure TestTAbstractStorage.Test_TAbstractStorage;
var Las : TAbstractStorage;
 lines: TStrings;
 s : String;
begin
  Las := TAbstractStorage.Create('',False,'TEST',1,InitializeStorage);
  try
    Las.Init('TEST',2,Las.AbstractMem.Is64Bits,Las.AbstractMem.MemUnitsSize,InitializeStorage);
    lines := TStringList.Create;
    try
      AddData(Las);
      Las.Analize(lines);
      CheckAddedData(Las);
      s := lines.Text;
      if s='' then Abort;

    finally
      lines.Free;
    end;
  finally
    Las.Free;
  end;
end;

initialization
  RegisterTest(TestTAbstractStorage{$IFNDEF FPC}.Suite{$ENDIF});
end.
