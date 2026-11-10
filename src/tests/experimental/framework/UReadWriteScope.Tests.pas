unit UReadWriteScope.Tests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }
{$mode Delphi}{$H+}

interface

uses fpcunit, testregistry;

type
  TReadWriteScopeTests = class(TTestCase)
  published
    procedure ReadScopeNormalReturn;
    procedure WriteScopeNormalReturn;
    procedure UpgradeableScopeNormalReturn;
    procedure AllModesEarlyExit;
    procedure AllModesExceptionUnwind;
    procedure CaughtExceptionKeepsScope;
    procedure CopiesRetainSingleAcquisition;
    procedure OverwriteReleasesPreviousLock;
    procedure ReturnedScopeRetainsAcquisition;
    procedure RecordFieldFinalizesScope;
    procedure ClassFieldFinalizesScope;
    procedure RecursiveReadScopesBalance;
    procedure RecursiveWriteScopesBalance;
    procedure NonrecursiveAcquisitionPreservesOuter;
    procedure OrdinaryReadCannotPromote;
    procedure DisposedLockRejectsAllScopes;
    procedure PromotionRetainsOuterUpgradeable;
    procedure PromotionExceptionRetainsOuter;
    procedure WriteToReadDowngrade;
    procedure UpgradeableToReadDowngrade;
    procedure ConcurrentReadersShareScopes;
    procedure WriterWaitsForLastCopy;
    procedure ReaderWaitsForWriterScope;
    procedure FailedAssignmentPreservesExistingScope;
    procedure ManualAndScopedAcquisitionsBalance;
    procedure WrongThreadReleaseIsRejected;
  end;

implementation

uses UReadWriteScope.Checks;

procedure TReadWriteScopeTests.ReadScopeNormalReturn;
begin
  RunReadWriteScopeCheck(1);
end;

procedure TReadWriteScopeTests.WriteScopeNormalReturn;
begin
  RunReadWriteScopeCheck(2);
end;

procedure TReadWriteScopeTests.UpgradeableScopeNormalReturn;
begin
  RunReadWriteScopeCheck(3);
end;

procedure TReadWriteScopeTests.AllModesEarlyExit;
begin
  RunReadWriteScopeCheck(4);
end;

procedure TReadWriteScopeTests.AllModesExceptionUnwind;
begin
  RunReadWriteScopeCheck(5);
end;

procedure TReadWriteScopeTests.CaughtExceptionKeepsScope;
begin
  RunReadWriteScopeCheck(6);
end;

procedure TReadWriteScopeTests.CopiesRetainSingleAcquisition;
begin
  RunReadWriteScopeCheck(7);
end;

procedure TReadWriteScopeTests.OverwriteReleasesPreviousLock;
begin
  RunReadWriteScopeCheck(8);
end;

procedure TReadWriteScopeTests.ReturnedScopeRetainsAcquisition;
begin
  RunReadWriteScopeCheck(9);
end;

procedure TReadWriteScopeTests.RecordFieldFinalizesScope;
begin
  RunReadWriteScopeCheck(10);
end;

procedure TReadWriteScopeTests.ClassFieldFinalizesScope;
begin
  RunReadWriteScopeCheck(11);
end;

procedure TReadWriteScopeTests.RecursiveReadScopesBalance;
begin
  RunReadWriteScopeCheck(12);
end;

procedure TReadWriteScopeTests.RecursiveWriteScopesBalance;
begin
  RunReadWriteScopeCheck(13);
end;

procedure TReadWriteScopeTests.NonrecursiveAcquisitionPreservesOuter;
begin
  RunReadWriteScopeCheck(14);
end;

procedure TReadWriteScopeTests.OrdinaryReadCannotPromote;
begin
  RunReadWriteScopeCheck(15);
end;

procedure TReadWriteScopeTests.DisposedLockRejectsAllScopes;
begin
  RunReadWriteScopeCheck(16);
end;

procedure TReadWriteScopeTests.PromotionRetainsOuterUpgradeable;
begin
  RunReadWriteScopeCheck(17);
end;

procedure TReadWriteScopeTests.PromotionExceptionRetainsOuter;
begin
  RunReadWriteScopeCheck(18);
end;

procedure TReadWriteScopeTests.WriteToReadDowngrade;
begin
  RunReadWriteScopeCheck(19);
end;

procedure TReadWriteScopeTests.UpgradeableToReadDowngrade;
begin
  RunReadWriteScopeCheck(20);
end;

procedure TReadWriteScopeTests.ConcurrentReadersShareScopes;
begin
  RunReadWriteScopeCheck(21);
end;

procedure TReadWriteScopeTests.WriterWaitsForLastCopy;
begin
  RunReadWriteScopeCheck(22);
end;

procedure TReadWriteScopeTests.ReaderWaitsForWriterScope;
begin
  RunReadWriteScopeCheck(23);
end;

procedure TReadWriteScopeTests.FailedAssignmentPreservesExistingScope;
begin
  RunReadWriteScopeCheck(24);
end;

procedure TReadWriteScopeTests.ManualAndScopedAcquisitionsBalance;
begin
  RunReadWriteScopeCheck(25);
end;

procedure TReadWriteScopeTests.WrongThreadReleaseIsRejected;
begin
  RunReadWriteScopeCheck(26);
end;

initialization
  RegisterTest(TReadWriteScopeTests);
end.
