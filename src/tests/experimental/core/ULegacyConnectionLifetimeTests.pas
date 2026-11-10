unit ULegacyConnectionLifetimeTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Reuse the actual native acceptance helper in the public FPCUnit suite. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses SysUtils, fpcunit, testregistry, ULegacyConnectionLifetimeChecks;

type
  TLegacyConnectionLifetimeTests = class(TTestCase)
  private
    procedure RunCheck(ACheck: TLegacyConnectionLifetimeCheck);
  published
    procedure RetirementWaitsForHeldConnectionLock;
    procedure NestedConnectionLocksRetainLifetimeUntilBothExits;
    procedure TimedAndNonblockingFailuresReleaseLifetimePins;
  end;

implementation

procedure TLegacyConnectionLifetimeTests.RunCheck(ACheck: TLegacyConnectionLifetimeCheck);
var LID: TGUID; LLog: String;
begin
  CreateGUID(LID);
  LLog:=IncludeTrailingPathDelimiter(GetTempDir)+'LegacyConnection-'+GUIDToString(LID)+'.log';
  AssertEquals('See actual connection ownership scenario log '+LLog,0,
    RunLegacyConnectionLifetimeCheck(ACheck,LLog));
end;

procedure TLegacyConnectionLifetimeTests.RetirementWaitsForHeldConnectionLock;
begin
  RunCheck(llcHeldLock);
end;

procedure TLegacyConnectionLifetimeTests.NestedConnectionLocksRetainLifetimeUntilBothExits;
begin
  RunCheck(llcRecursiveLock);
end;

procedure TLegacyConnectionLifetimeTests.TimedAndNonblockingFailuresReleaseLifetimePins;
begin
  RunCheck(llcTimedAdmission);
end;

initialization
  RegisterTest(TLegacyConnectionLifetimeTests);
end.
