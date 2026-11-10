unit UWalletSnapshotTests;

{ Copyright © 2026 Herman Schoenfeld
  Distributed under the MIT software license, see the accompanying LICENSE.
  These fixtures use synthetic memberships and generated keys, without a node. }

{$mode delphi}

interface

uses Classes, SysUtils, SyncObjs, fpcunit, testregistry, Generics.Collections,
  UCrypto, UAccounts, UPCDataTypes, UBaseTypes, UDataSources, UCommon.Data;

type
  TMembershipHistorySource = class(TAccountsOperationsDataSource)
  protected
    function FetchWalletAccounts: TArray<Cardinal>; override;
  public
    FixtureAccounts: TArray<Cardinal>;
    ResolveCount: Integer;
    BlockResolution: Boolean;
    Entered, ReleaseResolution: TEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Resolve;
  end;

  TBlockedAccountsSource = class(TAccountsDataSource)
  public
    Entered, ReleaseFetch: TEvent;
    constructor Create;
    destructor Destroy; override;
    procedure FetchAll(const AContainer: TList<TAccount>); override;
    procedure Fetch;
  end;

  TWalletSnapshotTests = class(TTestCase)
  private
    FHistory: TMembershipHistorySource;
    FAccounts: TBlockedAccountsSource;
    FSettingsKey: TAccountKey;
    procedure ChangeHistorySettings;
    procedure ChangeAccountSettings;
  published
    procedure WalletMembershipResolvesOnlyWhenMarkedDirty;
    procedure MembershipSettingsDoNotWaitForResolution;
    procedure AccountSettingsDoNotWaitForFetch;
  end;

implementation

uses UCommon, UConst, UMemory, UThread;

type
  TSnapshotActionThread = class(TPCThread)
  private
    FAction: TThreadMethod;
  protected
    procedure BCExecute; override;
  public
    Completed: TEvent;
    ErrorMessage: String;
    constructor Create(AAction: TThreadMethod);
    destructor Destroy; override;
  end;

constructor TSnapshotActionThread.Create(AAction: TThreadMethod);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FAction := AAction;
  Completed := TEvent.Create(nil, True, False, '');
  Start;
end;

destructor TSnapshotActionThread.Destroy;
begin
  Completed.Free;
  inherited Destroy;
end;

procedure TSnapshotActionThread.BCExecute;
begin
  try
    try
      FAction;
    except
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

constructor TMembershipHistorySource.Create;
begin
  inherited Create(nil);
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseResolution := TEvent.Create(nil, True, False, '');
end;

destructor TMembershipHistorySource.Destroy;
begin
  Entered.Free;
  ReleaseResolution.Free;
  inherited Destroy;
end;

function TMembershipHistorySource.FetchWalletAccounts: TArray<Cardinal>;
begin
  Inc(ResolveCount);
  Result := TArrayTool<Cardinal>.Copy(FixtureAccounts);
  if BlockResolution then begin
    Entered.SetEvent;
    if ReleaseResolution.WaitFor(10000) <> wrSignaled then
      raise Exception.Create('Membership resolution gate timed out');
  end;
end;

procedure TMembershipHistorySource.Resolve;
begin
  RefreshWalletAccounts;
end;

constructor TBlockedAccountsSource.Create;
begin
  inherited Create(nil);
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseFetch := TEvent.Create(nil, True, False, '');
end;

destructor TBlockedAccountsSource.Destroy;
begin
  Entered.Free;
  ReleaseFetch.Free;
  inherited Destroy;
end;

procedure TBlockedAccountsSource.FetchAll(const AContainer: TList<TAccount>);
begin
  Entered.SetEvent;
  if ReleaseFetch.WaitFor(10000) <> wrSignaled then
    raise Exception.Create('Account fetch gate timed out');
end;

procedure TBlockedAccountsSource.Fetch;
var LTable: TDataTable;
begin
  FetchPage(TPageFetchParams.Create(0, 25, nil, foAnd), LTable);
end;

procedure TWalletSnapshotTests.ChangeHistorySettings;
begin
  FHistory.InvalidateWalletAccounts;
  FHistory.UseWalletAccounts := False;
  FHistory.Accounts := TArray<Cardinal>.Create(77);
  if FHistory.Accounts[0] <> 77 then
    raise Exception.Create('Explicit membership was not retained');
  FHistory.BlockDepth := 300;
  if FHistory.BlockDepth <> 300 then
    raise Exception.Create('History depth was not retained');
  FHistory.UseWalletAccounts := True;
end;

procedure TWalletSnapshotTests.ChangeAccountSettings;
var LKeys: TArray<TAccountKey>;
begin
  FAccounts.FilterKeys := TArray<TAccountKey>.Create(FSettingsKey);
  LKeys := FAccounts.FilterKeys;
  if Length(LKeys) <> 1 then
    raise Exception.Create('Account filter was not retained');
  LKeys := nil;
  FAccounts.IncludePending := True;
  if not FAccounts.IncludePending then
    raise Exception.Create('Pending preference was not retained');
  FAccounts.InvalidateSnapshot;
end;

procedure TWalletSnapshotTests.WalletMembershipResolvesOnlyWhenMarkedDirty;
var
  LSource: TMembershipHistorySource;
  LNumbers: TArray<Cardinal>;
  LDisposables: TDisposables;
begin
  LSource := LDisposables.AddObject(TMembershipHistorySource.Create) as TMembershipHistorySource;
  LSource.FixtureAccounts := TArray<Cardinal>.Create(1, 2, 3);
  LSource.UseWalletAccounts := True;
  AssertEquals(0, LSource.ResolveCount); // The UI setter must not enumerate.
  LSource.Resolve;
  LSource.Resolve;
  AssertEquals(1, LSource.ResolveCount);
  LNumbers := LSource.Accounts;
  AssertEquals(1, Integer(LNumbers[0]));
  LNumbers[0] := 999;
  LNumbers := LSource.Accounts;
  AssertEquals('Getter returns an independent array', 1, Integer(LNumbers[0]));
  LSource.FixtureAccounts := TArray<Cardinal>.Create(4, 5, 6); // Same cardinality.
  LSource.InvalidateSnapshot;
  LSource.Resolve;
  AssertEquals('Ordinary data changes do not re-enumerate membership', 1, LSource.ResolveCount);
  LSource.InvalidateWalletAccounts;
  LSource.Resolve;
  AssertEquals(2, LSource.ResolveCount);
  LNumbers := LSource.Accounts;
  AssertEquals('Same-count replacements must be refreshed', 4, Integer(LNumbers[0]));
  LSource.UseWalletAccounts := False;
  LSource.Accounts := TArray<Cardinal>.Create(77);
  LSource.Resolve;
  AssertEquals(2, LSource.ResolveCount);
  LNumbers := LSource.Accounts;
  AssertEquals(77, Integer(LNumbers[0]));
end;

procedure TWalletSnapshotTests.MembershipSettingsDoNotWaitForResolution;
var
  LResolve, LSettings: TSnapshotActionThread;
begin
  FHistory := TMembershipHistorySource.Create;
  try
    FHistory.FixtureAccounts := TArray<Cardinal>.Create(1, 2, 3);
    FHistory.UseWalletAccounts := True;
    FHistory.BlockResolution := True;
    LResolve := nil;
    LSettings := nil;
    try
      LResolve := TSnapshotActionThread.Create(FHistory.Resolve);
      AssertEquals(Ord(wrSignaled), Ord(FHistory.Entered.WaitFor(5000)));
      LSettings := TSnapshotActionThread.Create(ChangeHistorySettings);
      AssertEquals('Settings waited for membership resolution', Ord(wrSignaled),
        Ord(LSettings.Completed.WaitFor(500)));
      AssertEquals('', LSettings.ErrorMessage);
      AssertEquals(Ord(wrTimeout), Ord(FHistory.ReleaseResolution.WaitFor(0)));
    finally
      FHistory.ReleaseResolution.SetEvent;
      if Assigned(LResolve) then LResolve.WaitFor;
      if Assigned(LSettings) then LSettings.WaitFor;
      LSettings.Free;
      LResolve.Free;
    end;
    AssertEquals('Stale resolution must not publish into the new settings', 0, Length(FHistory.Accounts));
    FHistory.BlockResolution := False;
    FHistory.FixtureAccounts := TArray<Cardinal>.Create(41, 42, 43);
    FHistory.Resolve;
    AssertEquals(2, FHistory.ResolveCount);
    AssertEquals(41, Integer(FHistory.Accounts[0]));
  finally
    FHistory.Free;
    FHistory := nil;
  end;
end;

procedure TWalletSnapshotTests.AccountSettingsDoNotWaitForFetch;
var
  LFetch, LSettings: TSnapshotActionThread;
  LKey: TECPrivateKey;
  LDisposables: TDisposables;
begin
  TCrypto.InitCrypto;
  LKey := LDisposables.AddObject(TECPrivateKey.Create) as TECPrivateKey;
  LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FSettingsKey := LKey.PublicKey;
  FAccounts := TBlockedAccountsSource.Create;
  try
    FAccounts.SnapshotCacheEnabled := True;
    LFetch := nil;
    LSettings := nil;
    try
      LFetch := TSnapshotActionThread.Create(FAccounts.Fetch);
      AssertEquals(Ord(wrSignaled), Ord(FAccounts.Entered.WaitFor(5000)));
      LSettings := TSnapshotActionThread.Create(ChangeAccountSettings);
      AssertEquals('Account settings waited for FetchPage', Ord(wrSignaled),
        Ord(LSettings.Completed.WaitFor(500)));
      AssertEquals('', LSettings.ErrorMessage);
      AssertEquals(Ord(wrTimeout), Ord(FAccounts.ReleaseFetch.WaitFor(0)));
    finally
      FAccounts.ReleaseFetch.SetEvent;
      if Assigned(LFetch) then LFetch.WaitFor;
      if Assigned(LSettings) then LSettings.WaitFor;
      LSettings.Free;
      LFetch.Free;
    end;
  finally
    FAccounts.Free;
    FAccounts := nil;
  end;
end;

initialization
  RegisterTest(TWalletSnapshotTests);

end.
