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
   end;

implementation


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
  LAMZone : TAMZone;
  i : Integer;
begin
  RandSeed:=0;
  LMem := TMem.Create(0,False);
  Try
    LMem.Initialize(A64Bytes,4);
    LAMZone := LMem.New(TAbstractMemTList.MinAbstractMemTListHeaderSize(LMem));
    LAMList := TAbstractMemTList.Create(LMem,LAMZone,AElementsPerBlock,AUseCache);
    Try
      LAMList.UseCacheAuto := AUseCacheAuto;
      // Start process
      repeat
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

initialization
  RegisterTest(TestTAbstractMemTList{$IFNDEF FPC}.Suite{$ENDIF});
end.
