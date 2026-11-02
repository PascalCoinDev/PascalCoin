unit UPendingSaveLifecycleTests;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  FPCUnit delegates to the same actual node/storage checks as the native probe. }
{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses SysUtils, fpcunit, testregistry, UPendingSaveChecks;

type
  TPendingSaveLifecycleTests = class(TTestCase)
  private
    procedure RunCheck(ACheck: TPendingSaveCheck);
  published
    procedure FinalDirtyOperationIsSavedDuringQuietWaitShutdown;
    procedure FailedSaveIsReportedAndRetried;
    procedure BankSanitizationRemovalSchedulesSave;
    procedure AtomicFileFailuresPreserveCompletePackageAndReopen;
    {$IFDEF MSWINDOWS}
    procedure FailedFinalPublicationReportsErrorAndExitsWithPreviousPackage;
    {$ENDIF}
  end;

implementation

procedure TPendingSaveLifecycleTests.RunCheck(ACheck: TPendingSaveCheck);
var LID: TGUID;
begin
  CreateGUID(LID);
  AssertEquals(0,RunPendingSaveCheck(ACheck,IncludeTrailingPathDelimiter(GetTempDir)+
    'PendingSaveCheck-'+GUIDToString(LID)+'.log'));
end;

procedure TPendingSaveLifecycleTests.FinalDirtyOperationIsSavedDuringQuietWaitShutdown;
begin
  RunCheck(pscFinalDirty);
end;

procedure TPendingSaveLifecycleTests.FailedSaveIsReportedAndRetried;
begin
  RunCheck(pscRetryAfterFailure);
end;

procedure TPendingSaveLifecycleTests.BankSanitizationRemovalSchedulesSave;
begin
  RunCheck(pscSanitizeRemoval);
end;

procedure TPendingSaveLifecycleTests.AtomicFileFailuresPreserveCompletePackageAndReopen;
begin
  RunCheck(pscAtomicStorage);
end;

{$IFDEF MSWINDOWS}
procedure TPendingSaveLifecycleTests.FailedFinalPublicationReportsErrorAndExitsWithPreviousPackage;
begin
  RunCheck(pscFinalPublicationFailure);
end;
{$ENDIF}

initialization
  RegisterTest(TPendingSaveLifecycleTests);
end.
