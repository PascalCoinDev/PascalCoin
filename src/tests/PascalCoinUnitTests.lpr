program UPascalCoinUnitTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UCommon.Collections, URandomHashTests,
  UCommon.Tests, UCommon.Collections.Tests, UMemory.Tests, UPipeline.Tests,
  UPipeline;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

