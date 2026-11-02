unit UScope.Tests;

{ Copyright © 2026 Herman Schoenfeld <https://www.sphere10.com>
  Distributed under the MIT software license, see LICENSE. }

{$MODE Delphi}{$H+}
interface
uses fpcunit, testregistry;
type
  TScopeTests = class(TTestCase)
  published
    procedure DefaultScopeIsEmpty;
    procedure NilCallbackRejected;
    procedure CreationDefersCallback;
    procedure LocalScopeReleasesOnReturn;
    procedure LocalScopeReleasesOnExit;
    procedure LocalScopeReleasesOnException;
    procedure InnerHandlerRetainsLocalScope;
    procedure ReturnedRecordRetainsCallback;
    procedure CopiesInvokeOnlyLastOwner;
    procedure OverwriteInvokesPreviousCallback;
    procedure OverwriteKeepsAliasedPreviousCallback;
    procedure ClassFieldFinalizesCallback;
    procedure ArrayCopiesShareCallback;
    procedure CallbackExceptionEmptiesOwner;
    procedure CallbackExceptionDuringOverwrite;
    procedure FailedCopyAssignmentPreservesSource;
    procedure SelfAssignmentPreservesOwner;
    procedure FreeMethodDisposesOwnedTarget;
    procedure BorrowedTargetRemainsCallerOwned;
    procedure FailedCallbackGetterKeepsPreviousOwner;
    procedure IndependentLocalsOwnIndependentCallbacks;
    procedure RecordFieldFinalizesCallback;
  end;
implementation
uses UScope.Checks;
procedure TScopeTests.DefaultScopeIsEmpty;
begin
  RunScopeCheck(1);
end;
procedure TScopeTests.NilCallbackRejected;
begin
  RunScopeCheck(2);
end;
procedure TScopeTests.CreationDefersCallback;
begin
  RunScopeCheck(3);
end;
procedure TScopeTests.LocalScopeReleasesOnReturn;
begin
  RunScopeCheck(4);
end;
procedure TScopeTests.LocalScopeReleasesOnExit;
begin
  RunScopeCheck(5);
end;
procedure TScopeTests.LocalScopeReleasesOnException;
begin
  RunScopeCheck(6);
end;
procedure TScopeTests.InnerHandlerRetainsLocalScope;
begin
  RunScopeCheck(7);
end;
procedure TScopeTests.ReturnedRecordRetainsCallback;
begin
  RunScopeCheck(8);
end;
procedure TScopeTests.CopiesInvokeOnlyLastOwner;
begin
  RunScopeCheck(9);
end;
procedure TScopeTests.OverwriteInvokesPreviousCallback;
begin
  RunScopeCheck(10);
end;
procedure TScopeTests.OverwriteKeepsAliasedPreviousCallback;
begin
  RunScopeCheck(11);
end;
procedure TScopeTests.ClassFieldFinalizesCallback;
begin
  RunScopeCheck(12);
end;
procedure TScopeTests.ArrayCopiesShareCallback;
begin
  RunScopeCheck(13);
end;
procedure TScopeTests.CallbackExceptionEmptiesOwner;
begin
  RunScopeCheck(14);
end;
procedure TScopeTests.CallbackExceptionDuringOverwrite;
begin
  RunScopeCheck(15);
end;
procedure TScopeTests.FailedCopyAssignmentPreservesSource;
begin
  RunScopeCheck(16);
end;
procedure TScopeTests.SelfAssignmentPreservesOwner;
begin
  RunScopeCheck(17);
end;
procedure TScopeTests.FreeMethodDisposesOwnedTarget;
begin
  RunScopeCheck(18);
end;
procedure TScopeTests.BorrowedTargetRemainsCallerOwned;
begin
  RunScopeCheck(19);
end;
procedure TScopeTests.FailedCallbackGetterKeepsPreviousOwner;
begin
  RunScopeCheck(20);
end;
procedure TScopeTests.IndependentLocalsOwnIndependentCallbacks;
begin
  RunScopeCheck(21);
end;
procedure TScopeTests.RecordFieldFinalizesCallback;
begin
  RunScopeCheck(22);
end;
initialization
  RegisterTest(TScopeTests);
end.
