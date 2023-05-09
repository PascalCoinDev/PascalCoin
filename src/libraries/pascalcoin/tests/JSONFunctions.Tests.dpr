program JSONFunctions.Tests;

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
  Forms, GuiTestRunner,
  {$ENDIF }
  {$ELSE}
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  {$ENDIF }
  UAbstractBTree in '..\..\abstractmem\UAbstractBTree.pas',
  UOrderedList in '..\..\abstractmem\UOrderedList.pas',
  UJSONFunctions in '..\UJSONFunctions.pas',
  UJSONFunctions.Tests in 'src\UJSONFunctions.Tests.pas';

{$IF Defined(FPC) and (Defined(CONSOLE_TESTRUNNER))}
type
  TFreePascalConsoleRunner = class(TTestRunner)
  protected
  end;
var
  Application : TFreePascalConsoleRunner;
{$ENDIF}

begin
  {$IFNDEF FPC}
  System.ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  {$IF Defined(FPC) and (Defined(CONSOLE_TESTRUNNER))}
  Application := TFreePascalConsoleRunner.Create(nil);
  {$ENDIF}

  Application.Title:='Test';
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


