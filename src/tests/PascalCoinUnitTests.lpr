program UPascalCoinUnitTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, UCommon.Collections, UCommon.Tests,
  UCommon.Collections.Tests, UMemory.Tests, UThread.Tests, URandomHash.Tests,
  URandomHash2.Tests, URandomHash, ubasetypes.tests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

