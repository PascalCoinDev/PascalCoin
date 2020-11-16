program AbstractMem.Tests;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

// Enable for Console tests
{.$DEFINE CONSOLE_TESTRUNNER}

{$IFDEF CONSOLE_TESTRUNNER}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF FPC}
  {$IFDEF CONSOLE_TESTRUNNER}
  Classes,
  {$ELSE}
  Interfaces,
  Forms,
  GUITestRunner,
  {$ENDIF }
  {$ELSE}
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  {$ENDIF }
  UAbstractBTree in '..\UAbstractBTree.pas',
  UAbstractMem in '..\UAbstractMem.pas',
  UAbstractMemTList in '..\UAbstractMemTList.pas',
  UAVLCache in '..\UAVLCache.pas',
  UCacheMem in '..\UCacheMem.pas',
  UFileMem in '..\UFileMem.pas',
  UOrderedList in '..\UOrderedList.pas',
  UCacheMem.Tests in 'src\UCacheMem.Tests.pas';

{$IF Defined(FPC) and (Defined(CONSOLE_TESTRUNNER))}
type
  TFreePascalConsoleRunner = class(TTestRunner)
  protected
  end;
var
  Application : TFreePascalConsoleRunner;
{$ENDIF}

begin
  {$IF Defined(FPC) and (Defined(CONSOLE_TESTRUNNER))}
  Application := TFreePascalConsoleRunner.Create(nil);
  {$ENDIF}

  Application.Initialize;
  {$IFDEF FPC}
  {$IF Not Defined(CONSOLE_TESTRUNNER)}
  Application.CreateForm(TGuiTestRunner, TestRunner);
  {$ENDIF}
  Application.Run;
  {$ELSE}
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
  {$ENDIF}
end.


