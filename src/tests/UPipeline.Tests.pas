unit UPipeline.Tests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  UPipeline;

type

  TPipelineTests = class(TTestCase)
    published
      procedure Test1;
  end;

implementation

procedure TPipelineTests.Test1;
begin

end;

initialization
  RegisterTest(TPipelineTests);

end.

