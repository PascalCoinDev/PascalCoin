unit ULogGuiHandoffTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Registers the logger handoff contracts shared with the native Delphi probe. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses SysUtils, fpcunit, testregistry, ULogGuiHandoffChecks;

type
  TLogGuiHandoffTests = class(TTestCase)
  published
    procedure ProducersDoNotWaitForGuiRendering;
  end;

implementation

procedure TLogGuiHandoffTests.ProducersDoNotWaitForGuiRendering;
var LID: TGUID; LPath: String;
begin
  AssertEquals('Create an owned logger test path',0,CreateGUID(LID));
  LPath := IncludeTrailingPathDelimiter(GetTempDir)+'LogGuiHandoff-'+GUIDToString(LID)+'.log';
  AssertEquals('Logger handoff contracts; details: '+LPath,0,RunLogGuiHandoffChecks(LPath));
end;

initialization
  RegisterTest(TLogGuiHandoffTests);

end.
