unit UDownloadWorkerLifetimeTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  The same socket/owner checks run in the native Delphi and FPC probes. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses SysUtils, fpcunit, testregistry, UDownloadLifetimeChecks;

type
  TDownloadWorkerLifetimeTests = class(TTestCase)
  private
    procedure RunCheck(ACheck: TDownloadLifetimeCheck);
  published
    procedure SelectedConnectionOutlivesDownloadAndFactoryFailureDoesNotPoisonAdmission;
    procedure UnselectedAndIncomingTransportsRetireAtTheirOwnBoundaries;
    procedure ShutdownDeniesNewWorkersAndJoinsBeforeOwnerDestruction;
    procedure OwnedServerCallbacksRetainNodeResourcesDuringDestruction;
    procedure ShutdownOwnsDiscoveryBeforeTheWorkerStarts;
    procedure ShutdownJoinsDiscoveryAndDeniesLateClientCreation;
    procedure DrainRechecksRegistryAfterAnEarlierEmptyScan;
  end;

implementation

procedure TDownloadWorkerLifetimeTests.RunCheck(ACheck: TDownloadLifetimeCheck);
var LID: TGUID; LLog: String;
begin
  CreateGUID(LID);
  LLog:=IncludeTrailingPathDelimiter(GetTempDir)+'DownloadLifetime-'+GUIDToString(LID)+'.log';
  AssertEquals('See actual connection/owner scenario log '+LLog,0,
    RunDownloadLifetimeCheck(ACheck,LLog));
end;

procedure TDownloadWorkerLifetimeTests.SelectedConnectionOutlivesDownloadAndFactoryFailureDoesNotPoisonAdmission;
begin
  RunCheck(dlcOutgoing);
end;

procedure TDownloadWorkerLifetimeTests.UnselectedAndIncomingTransportsRetireAtTheirOwnBoundaries;
begin
  RunCheck(dlcIncoming);
end;

procedure TDownloadWorkerLifetimeTests.ShutdownDeniesNewWorkersAndJoinsBeforeOwnerDestruction;
begin
  RunCheck(dlcShutdown);
end;

procedure TDownloadWorkerLifetimeTests.OwnedServerCallbacksRetainNodeResourcesDuringDestruction;
begin
  RunCheck(dlcNodeCallbacks);
end;

procedure TDownloadWorkerLifetimeTests.ShutdownOwnsDiscoveryBeforeTheWorkerStarts;
begin
  RunCheck(dlcPendingDiscovery);
end;

procedure TDownloadWorkerLifetimeTests.ShutdownJoinsDiscoveryAndDeniesLateClientCreation;
begin
  RunCheck(dlcActiveDiscovery);
end;

procedure TDownloadWorkerLifetimeTests.DrainRechecksRegistryAfterAnEarlierEmptyScan;
begin
  RunCheck(dlcLateConnection);
end;

initialization
  RegisterTest(TDownloadWorkerLifetimeTests);
end.
