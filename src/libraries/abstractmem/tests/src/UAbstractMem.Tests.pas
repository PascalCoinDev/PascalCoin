unit UAbstractMem.Tests;

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
   UCacheMem, UFileMem, UAbstractMem, UAbstractBTree, UAbstractMemTList;
 type
   // Test methods for class TCalc
   TestTAbstractMem = class(TTestCase)
   strict private
   public
     procedure SetUp; override;
     procedure TearDown; override;
   published
     procedure Test1;
   end;

implementation

procedure TestTAbstractMem.SetUp;
begin
end;

procedure TestTAbstractMem.TearDown;
begin
end;

procedure TestTAbstractMem.Test1;
var Lfm : TFileMem;
begin
  Lfm := TFileMem.Create(ExtractFileDir(ParamStr(0))+PathDelim+'test1.am',False);
  try
    Lfm.ClearContent; // Init
  finally
    Lfm.Free;
  end;
end;


initialization
//  RegisterTest(TestTAbstractMem{$IFNDEF FPC}.Suite{$ENDIF});
end.
