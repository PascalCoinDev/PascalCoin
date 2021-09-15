unit UAbstractMem.Tests;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

 uses
   SysUtils, classes,
   {$IFDEF FPC}
   fpcunit, testutils, testregistry,
   {$ELSE}
   TestFramework,
   {$ENDIF}
   UCacheMem, UFileMem, UAbstractMem, UAbstractBTree, UAbstractMemTList,
   UOrderedList
   {$IFNDEF FPC},System.Generics.Collections,System.Generics.Defaults{$ELSE},Generics.Collections,Generics.Defaults{$ENDIF};
 type
   // Test methods for class TCalc
   TestTAbstractMem = class(TTestCase)
   strict private
   public
     procedure SetUp; override;
     procedure TearDown; override;
   published
     procedure Test_ClearContent;
     procedure Test_MemLeaksReuse;
   end;

implementation

procedure TestTAbstractMem.SetUp;
begin
end;

procedure TestTAbstractMem.TearDown;
begin
end;

procedure TestTAbstractMem.Test_ClearContent;
var Lfm : TFileMem;
begin
  Lfm := TFileMem.Create(ExtractFileDir(ParamStr(0))+PathDelim+'test1.am',False);
  try
    Lfm.ClearContent; // Init
  finally
    Lfm.Free;
  end;
end;


procedure TestTAbstractMem.Test_MemLeaksReuse;
var LAM : TAbstractMem;
  LAMs : TList<TAMZone>;
  i,j, loops : Integer;
  LStrings : TStrings;
  LAbstractMemZoneInfoList : TList<TAbstractMemZoneInfo>;
  LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount : TAbstractMemPosition;
begin
  LAM := TMem.Create(0,False);
  try
    LAMs := TList<TAMZone>.Create;
    Try
      for loops := 1 to 2 do begin

      LAMs.Clear;

      for j := 1 to 10000 do begin
        LAMs.Add( LAM.New(Random(1000)+10) );
      end;

      //
      for i := 0 to LAMs.Count-1 do begin
        LAM.Dispose( LAMs.Items[i] );
      end;

      end;

      LStrings := TStringList.Create;
      LAbstractMemZoneInfoList := TList<TAbstractMemZoneInfo>.Create;
      try
        if Not LAM.CheckConsistency(LStrings,LAbstractMemZoneInfoList,LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount) then raise Exception.Create(LStrings.Text);
      finally
        LAbstractMemZoneInfoList.Free;
        LStrings.Free;
      end;
    Finally
      LAMs.Free;
    End;
  finally
    LAM.Free;
  end;
end;

initialization
  RegisterTest(TestTAbstractMem{$IFNDEF FPC}.Suite{$ENDIF});
end.
