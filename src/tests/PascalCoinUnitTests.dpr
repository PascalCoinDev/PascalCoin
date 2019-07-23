program RandomHash.Tests;

{$WARN DUPLICATE_CTOR_DTOR OFF}
{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  UPCSafeBoxRootHashTests in 'UPCSafeBoxRootHashTests.pas',
  URandomHash.Tests.Delphi in 'URandomHash.Tests.Delphi.pas',
  URandomHash2.Tests.Delphi in 'URandomHash2.Tests.Delphi.pas',
  URandomHash in '..\core\URandomHash.pas',
  URandomHash2 in '..\core\URandomHash2.pas';

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.
