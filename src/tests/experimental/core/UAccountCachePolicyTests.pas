unit UAccountCachePolicyTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses fpcunit, testregistry;

type
  TAccountCachePolicyTests = class(TTestCase)
  published
    procedure CoreDefaultRemains50000;
    procedure ConfiguredLimitSurvivesMemoryReplacement;
    procedure FileReplacementAndClearRetainPolicyWithoutStaleRecords;
  end;

implementation

uses UAccountCachePolicyChecks;

procedure TAccountCachePolicyTests.CoreDefaultRemains50000;
begin
  CheckDefaultAccountCacheLimit;
end;

procedure TAccountCachePolicyTests.ConfiguredLimitSurvivesMemoryReplacement;
begin
  CheckAccountCacheLimitSurvivesMemoryReload;
end;

procedure TAccountCachePolicyTests.FileReplacementAndClearRetainPolicyWithoutStaleRecords;
begin
  CheckAccountCacheLimitSurvivesFileReloadAndClear;
end;

initialization
  RegisterTest(TAccountCachePolicyTests);

end.
