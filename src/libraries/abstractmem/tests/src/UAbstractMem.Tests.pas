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
     procedure Test_MemLeaks(AAbstractMem: TAbstractMem);
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
var LAM : TAbstractMem;
begin
  LAM := TMem.Create(0,False);
  try
    LAM.ClearContent(False,4); // Init
    LAM.ClearContent(True,16); // Init
    LAM.CheckConsistency;
  finally
    LAM.Free;
  end;
end;


procedure TestTAbstractMem.Test_MemLeaks(AAbstractMem: TAbstractMem);
var
  LAMs : TList<TAMZone>;
  i,j, loops : Integer;
begin
  LAMs := TList<TAMZone>.Create;
  Try
    for loops := 1 to 2 do begin

      LAMs.Clear;

      for j := 1 to 10000 do begin
        LAMs.Add( AAbstractMem.New(Random(1000)+10) );
      end;

      //
      for i := 0 to LAMs.Count-1 do begin
        AAbstractMem.Dispose( LAMs.Items[i] );
      end;

      AAbstractMem.CheckConsistency;
    end;

  Finally
    LAMs.Free;
  End;
end;

procedure TestTAbstractMem.Test_MemLeaksReuse;
var LAM : TAbstractMem;
begin
  RandSeed := 0;
  LAM := TMem.Create(0,False);
  try
    LAM.Initialize(False,4);
    Test_MemLeaks(LAM);
    LAM.Initialize(True,4);
    Test_MemLeaks(LAM);
    LAM.Initialize(True,160);
    Test_MemLeaks(LAM);
    LAM.Initialize(True,256);
  finally
    LAM.Free;
  end;
end;

initialization
  RegisterTest(TestTAbstractMem{$IFNDEF FPC}.Suite{$ENDIF});
end.
