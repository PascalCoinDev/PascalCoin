unit UAbstractMemTList.Tests;

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
   UAbstractMem,
   UAbstractMemTList;

type
  TTestRecord = record
    i : Integer;
    s : String;
    procedure Clear;
    procedure RandomData;
  end;

  TAbstractMemTList_TTestRecord = class(TAbstractMemTList<TTestRecord>)
  protected
    procedure LoadFrom(const ABytes : TBytes; var AItem : TTestRecord); override;
    procedure SaveTo(const AItem : TTestRecord; AIsAddingItem : Boolean; var ABytes : TBytes); override;
  public
    procedure AddRandom;
    procedure AddValues(i : Integer; s : String);
  end;

  TAbstractMemOrderedTList_TTestRecord = class(TAbstractMemOrderedTList<TTestRecord>)
  protected
    procedure LoadFrom(const ABytes : TBytes; var AItem : TTestRecord); override;
    procedure SaveTo(const AItem : TTestRecord; AIsAddingItem : Boolean; var ABytes : TBytes); override;
    function Compare(const ALeft, ARight : TTestRecord) : Integer; override;
  public
    procedure AddRandom;
    procedure AddValues(i : Integer; s : String);
  end;

   TestTAbstractMemTList = class(TTestCase)
   strict private
   public
     procedure SetUp; override;
     procedure TearDown; override;
     procedure TestInfinite(A64Bytes : Boolean; AUseCache, AUseCacheAuto : Boolean; AElementsPerBlock : Integer);
   published
     procedure Test_32b_NoCache;
     procedure Test_32b_Cache;
     procedure Test_64b_NoCache;
     procedure Test_64b_Cache;
     procedure Test_TTestRecord;
   end;

implementation

uses UAbstractStorage;

{ TestTAbstractMemTList }

procedure TestTAbstractMemTList.SetUp;
begin
  inherited;
end;

procedure TestTAbstractMemTList.TearDown;
begin
  inherited;
end;

procedure TestTAbstractMemTList.TestInfinite(A64Bytes, AUseCache,
  AUseCacheAuto: Boolean; AElementsPerBlock: Integer);
var LMem : TMem;
  LAMList : TAbstractMemTList;
  LAMList_TR : TAbstractMemTList_TTestRecord;
  LAMZone : TAMZone;
  i : Integer;
  LTR : TTestRecord;
begin
  RandSeed:=0;
  LMem := TMem.Create(0,False);
  Try
    LMem.Initialize(A64Bytes,4);
    LAMZone := LMem.New(TAbstractMemTList.MinAbstractMemTListHeaderSize(LMem));
    LAMList := TAbstractMemTList.Create(LMem,LAMZone,AElementsPerBlock,AUseCache);
    LAMList_TR := TAbstractMemTList_TTestRecord.Create(LMem,LMem.New(TAbstractMemTList.MinAbstractMemTListHeaderSize(LMem)),5,True);
    Try
      LAMList.UseCacheAuto := AUseCacheAuto;
      // Start process
      repeat
        LTR.RandomData;
        LAMList_TR.Add(LTR);
        //
        LAMList.Add(LMem.New((Random(50)+1)*4).position);
        if (Random(5)=0) and (LAMList.Count>0) then begin
          i := Random(LAMList.Count);
          LAMZone.position := LAMList.Position[i];
          LAMList.Delete(i);
          LMem.Dispose(LAMZone.position);
        end;
        if Random(100)=0 then
          LAMList.FlushCache;
      until LAMList.Count>(AElementsPerBlock*200);
      //
      LAMList.FlushCache;
      //
      while (LAMList.Count>0) do begin
        i := Random(LAMList.Count);
        LAMZone.position := LAMList.Position[i];
        LAMList.Delete(i);
        LMem.Dispose(LAMZone.position);
        if Random(100)=0 then
          LAMList.FlushCache;
      end;
      LAMList.FlushCache;
      LMem.CheckConsistency();
      //
    Finally
      LAMList.Free;
      LAMList_TR.Free;
    End;
  Finally
    LMem.Free;
  End;
end;

procedure TestTAbstractMemTList.Test_32b_Cache;
begin
  TestInfinite(False,True,True,10);
end;

procedure TestTAbstractMemTList.Test_32b_NoCache;
begin
  TestInfinite(False,False,False,10);
end;

procedure TestTAbstractMemTList.Test_64b_Cache;
begin
  TestInfinite(True,True,True,10);
end;

procedure TestTAbstractMemTList.Test_64b_NoCache;
begin
  TestInfinite(True,False,False,10);
end;

procedure TestTAbstractMemTList.Test_TTestRecord;
begin

end;

{ TTestRecord }

procedure TTestRecord.Clear;
begin
  Self.i := 0;
  Self.s := '';
end;

procedure TTestRecord.RandomData;
var i,j : Integer;
begin
  Self.s := '';
  Self.i := Random(100000);
  j := Random(25)+5;
  for i := 1 to j do begin
    Self.s := Self.s + Char(Random(ord('Z')-ord('A'))+ord('A'));
  end;
end;

{ TAbstractMemTList_TTestRecord }

procedure TAbstractMemTList_TTestRecord.AddRandom;
var tr : TTestRecord;
begin
  tr.Clear;
  tr.RandomData;
  Add(tr);
end;

procedure TAbstractMemTList_TTestRecord.AddValues(i: Integer; s: String);
var tr : TTestRecord;
begin
  tr.Clear;
  tr.i := i;
  tr.s := s;
  Add(tr);
end;

procedure TAbstractMemTList_TTestRecord.LoadFrom(const ABytes: TBytes;
  var AItem: TTestRecord);
var bs : TBytesStorage;
begin
  bs := TBytesStorage.Create(ABytes);
  try
    AItem.Clear;
    AItem.i := bs.ReadUInt32;
    AItem.s := bs.ReadString;
  finally
    bs.Free;
  end;
end;

procedure TAbstractMemTList_TTestRecord.SaveTo(const AItem: TTestRecord;
  AIsAddingItem: Boolean; var ABytes: TBytes);
var bs : TBytesStorage;
begin
  bs := TBytesStorage.Create(0);
  try
    bs.WriteUInt32(AItem.i);
    bs.WriteString(AItem.s);
    ABytes := Copy(bs.Bytes,0,bs.Size);
  finally
    bs.Free;
  end;
end;

{ TAbstractMemOrderedTList_TTestRecord }

procedure TAbstractMemOrderedTList_TTestRecord.AddRandom;
var tr : TTestRecord;
begin
  tr.Clear;
  tr.RandomData;
  Add(tr);
end;

procedure TAbstractMemOrderedTList_TTestRecord.AddValues(i: Integer;
  s: String);
var tr : TTestRecord;
begin
  tr.Clear;
  tr.i := i;
  tr.s := s;
  Add(tr);
end;

function TAbstractMemOrderedTList_TTestRecord.Compare(const ALeft,
  ARight: TTestRecord): Integer;
begin
  Result := ALeft.i - ARight.i;
  if (Result=0) then begin
    Result := AnsiCompareStr(ALeft.s,ARight.s);
  end;
end;

procedure TAbstractMemOrderedTList_TTestRecord.LoadFrom(const ABytes: TBytes;
  var AItem: TTestRecord);
var bs : TBytesStorage;
begin
  bs := TBytesStorage.Create(ABytes);
  try
    AItem.Clear;
    AItem.i := bs.ReadUInt32;
    AItem.s := bs.ReadString;
  finally
    bs.Free;
  end;
end;

procedure TAbstractMemOrderedTList_TTestRecord.SaveTo(const AItem: TTestRecord;
  AIsAddingItem: Boolean; var ABytes: TBytes);
var bs : TBytesStorage;
begin
  bs := TBytesStorage.Create(0);
  try
    bs.WriteUInt32(AItem.i);
    bs.WriteString(AItem.s);
    ABytes := Copy(bs.Bytes,0,bs.Size);
  finally
    bs.Free;
  end;
end;

initialization
  RegisterTest(TestTAbstractMemTList{$IFNDEF FPC}.Suite{$ENDIF});
end.
