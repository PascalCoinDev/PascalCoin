unit UCoreLockMigrationTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Registers actual core owner/lease checks shared with native Delphi probes. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses SysUtils, fpcunit, testregistry, UCoreLockMigrationChecks;

type
  TCoreApplicationLockMigrationTests = class(TTestCase)
  private
    procedure RunCheck(ACheck: TCoreLockMigrationCheck);
  published
    procedure NamedConstructorPreservesDefaultPolicy;
    procedure NamedConstructorPreservesRecursiveOwnership;
    procedure MutableThreadListLeaseRetainsRecursion;
    procedure ThreadListTryLeaseExcludesOtherOwners;
    procedure ThreadListCountWaitsForMutableOwner;
    procedure ThreadListWaiterWakesAfterFinalNestedRelease;
    procedure ProtectedWriteRetainsRecursiveOwnership;
    procedure TimedWriteExcludesExistingReaders;
    procedure TimedWriteWakesAfterOwnerRelease;
    procedure UnsignedTimeoutsAreClampedBeforeSignedConversion;
    procedure OperationsComponentRetainsRecursiveLease;
    procedure OperationsComponentDestructionBalancesOwnership;
  end;

implementation

procedure TCoreApplicationLockMigrationTests.RunCheck(ACheck: TCoreLockMigrationCheck);
var LID: TGUID;
begin
  AssertEquals('Create a unique owned core-lock log',0,CreateGUID(LID));
  AssertEquals(0,RunCoreLockMigrationCheck(ACheck,IncludeTrailingPathDelimiter(GetTempDir)+'CoreLockMigration-'+GUIDToString(LID)+'.log'));
end;

procedure TCoreApplicationLockMigrationTests.NamedConstructorPreservesDefaultPolicy;
begin
  RunCheck(clmNamedDefault);
end;

procedure TCoreApplicationLockMigrationTests.NamedConstructorPreservesRecursiveOwnership;
begin
  RunCheck(clmNamedRecursive);
end;

procedure TCoreApplicationLockMigrationTests.MutableThreadListLeaseRetainsRecursion;
begin
  RunCheck(clmThreadListRecursive);
end;

procedure TCoreApplicationLockMigrationTests.ThreadListTryLeaseExcludesOtherOwners;
begin
  RunCheck(clmThreadListTryExclusion);
end;

procedure TCoreApplicationLockMigrationTests.ThreadListCountWaitsForMutableOwner;
begin
  RunCheck(clmThreadListCountExclusion);
end;

procedure TCoreApplicationLockMigrationTests.ThreadListWaiterWakesAfterFinalNestedRelease;
begin
  RunCheck(clmThreadListTimedWake);
end;

procedure TCoreApplicationLockMigrationTests.ProtectedWriteRetainsRecursiveOwnership;
begin
  RunCheck(clmProtectedWrite);
end;

procedure TCoreApplicationLockMigrationTests.TimedWriteExcludesExistingReaders;
begin
  RunCheck(clmTimedWriteExclusion);
end;

procedure TCoreApplicationLockMigrationTests.TimedWriteWakesAfterOwnerRelease;
begin
  RunCheck(clmTimedWriteWake);
end;

procedure TCoreApplicationLockMigrationTests.UnsignedTimeoutsAreClampedBeforeSignedConversion;
begin
  RunCheck(clmUnsignedTimeouts);
end;

procedure TCoreApplicationLockMigrationTests.OperationsComponentRetainsRecursiveLease;
begin
  RunCheck(clmOperationsRecursive);
end;

procedure TCoreApplicationLockMigrationTests.OperationsComponentDestructionBalancesOwnership;
begin
  RunCheck(clmOperationsDestruction);
end;

initialization
  RegisterTest(TCoreApplicationLockMigrationTests);
end.
