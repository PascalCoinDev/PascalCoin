unit UWalletNetworkOffChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Exercises the real connectivity action and wallet refresh lifecycle with
  controlled data. Only loopback discovery is permitted in this fresh wallet.
  Distributed under the MIT software license; see the accompanying LICENSE.
}

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunWalletNetworkOffProbe: Integer;

implementation

uses
  Windows, Classes, SysUtils, Forms, Generics.Collections, Variants,
  UWalletGridMocks, UUserInterface, UWalletStartup, UNode, UNetProtocol,
  UAppParams, USettings, UMemory, UCommon, UCommon.UI, UVisualGrid;

type
  TGridGeometryAccess = class(TVisualGrid)
  public
    function HasPendingLayout: Boolean;
  end;

  TNetworkOffChecks = class
  private
    FMain: TMockMainForm;
    FModel: TMockWalletData;
    FLog: TStringList;
    FLogPath, FAsyncError: String;
    FPreviousException: TExceptionEvent;
    FWorkActive: Boolean;
    procedure Log(const AText: String);
    procedure OnException(Sender: TObject; E: Exception);
    procedure Pump(AMilliseconds: Cardinal);
    procedure WaitForFetches;
    procedure Settle;
    procedure WaitForRefresh(AAccounts, AOperations: Integer; ATimeout: Cardinal = 8000);
    function Grid(AKind: TMockDataKind): TVisualGrid;
    procedure CheckGridState(AKind: TMockDataKind);
    procedure CheckNodePresentation;
    procedure CheckRows(ARevision: Integer);
    procedure CheckConnections;
    function Notify(ARevision: Integer): UInt64;
    function Burst(AFirstRevision: Integer): UInt64;
    procedure ClickConnectivity(AExpectedActive: Boolean);
    procedure BusyPagesThenNetworkOff;
    procedure ActiveWorkThenNetworkOff;
  public
    constructor Create(const ALogPath: String);
    destructor Destroy; override;
    procedure Run;
  end;

function TGridGeometryAccess.HasPendingLayout: Boolean;
begin
  Result := Assigned(FDelayedBoundsChangeTimer) and FDelayedBoundsChangeTimer.Enabled;
end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function DesktopName(AHandle: THandle): String;
var LBuffer: array[0..255] of Char; LRequired: DWORD;
begin
  FillChar(LBuffer, SizeOf(LBuffer), 0);
  if not GetUserObjectInformation(AHandle, UOI_NAME, @LBuffer[0], SizeOf(LBuffer), LRequired) then
    RaiseLastOSError;
  Result := StrPas(PChar(@LBuffer[0]));
end;

procedure RequirePrivateDesktop;
var LInput: THandle; LName: String;
begin
  LInput := OpenInputDesktop(0, false, DESKTOP_READOBJECTS);
  if LInput = 0 then RaiseLastOSError;
  try
    LName := DesktopName(GetThreadDesktop(GetCurrentThreadId));
    Require((Pos('PascalCoin', LName) = 1) and not SameText(LName, DesktopName(LInput)),
      'Run this probe through the existing private-desktop broker');
  finally CloseDesktop(LInput); end;
end;

procedure SeedLoopbackSettings(const ADirectory: String);
var LBag: TDisposables; LParams: TAppParams;
begin
  LParams := LBag.AddObject(TAppParams.Create(nil)) as TAppParams;
  LParams.FileName := IncludeTrailingPathDelimiter(ADirectory) + 'AppParams.prm';
  LParams.ParamByName[CT_PARAM_TryToConnectOnlyWithThisFixedServers].SetAsString('127.0.0.1:1');
  LParams.ParamByName[CT_PARAM_PeerCache].SetAsString('');
  LParams.ParamByName[CT_PARAM_InternetServerPort].SetAsInteger(0);
  LParams.ParamByName[CT_PARAM_JSONRPCEnabled].SetAsBoolean(false);
  LParams.ParamByName[CT_PARAM_JSONRPCMinerServerActive].SetAsBoolean(false);
  LParams.ParamByName[CT_PARAM_JSONRPCMinerServerPort].SetAsInteger(0);
  LParams.ParamByName[CT_PARAM_MinerPrivateKeyType].SetAsInteger(Ord(mpk_Random));
  LParams.ParamByName[CT_PARAM_ShowLogs].SetAsBoolean(false);
  LParams.ParamByName[CT_PARAM_SaveLogFiles].SetAsBoolean(false);
end;

constructor TNetworkOffChecks.Create(const ALogPath: String);
begin
  inherited Create;
  FLogPath := ALogPath;
  FLog := TStringList.Create;
  FPreviousException := Application.OnException;
  Application.OnException := OnException;
end;

destructor TNetworkOffChecks.Destroy;
begin
  try
    if FWorkActive then begin
      TUserInterface.Node.EndBlockChainUpdate;
      FWorkActive := false;
    end;
    if Assigned(FModel) then FModel.ReleaseFetches;
    if Assigned(FMain) then begin
      TUserInterface.ExitApplication;
      FMain := nil;
      FModel := nil;
    end;
  finally
    Application.OnException := FPreviousException;
    FLog.Free;
    inherited;
  end;
end;

procedure TNetworkOffChecks.Log(const AText: String);
begin
  if Assigned(FModel) then FModel.DrainEvents(FLog);
  FLog.Add(UIntToStr(GetTickCount64) + ' ' + AText);
  FLog.SaveToFile(FLogPath);
end;

procedure TNetworkOffChecks.OnException(Sender: TObject; E: Exception);
begin
  if FAsyncError <> '' then Exit;
  FAsyncError := E.ClassName + ': ' + E.Message;
  Log('ASYNC ' + FAsyncError);
end;

procedure TNetworkOffChecks.Pump(AMilliseconds: Cardinal);
var LDeadline: UInt64;
begin
  LDeadline := GetTickCount64 + AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    if FAsyncError <> '' then raise Exception.Create(FAsyncError);
    Sleep(1);
  until GetTickCount64 >= LDeadline;
end;

procedure TNetworkOffChecks.WaitForFetches;
var LDeadline: UInt64;
begin
  LDeadline := GetTickCount64 + 3000;
  while ((FModel.Metrics(mdkAccounts).ActiveFetches <> 1) or
    (FModel.Metrics(mdkOperations).ActiveFetches <> 1)) and
    (GetTickCount64 < LDeadline) do Pump(10);
  Require((FModel.Metrics(mdkAccounts).ActiveFetches = 1) and
    (FModel.Metrics(mdkOperations).ActiveFetches = 1), 'Both real grid workers must enter their fetch gates');
end;

procedure TNetworkOffChecks.Settle;
var
  LDeadline, LStable: UInt64;
  LLastAccounts, LLastOperations, LLastAccountPageSize, LLastOperationPageSize: Integer;
begin
  LDeadline := GetTickCount64 + 8000;
  LStable := GetTickCount64;
  LLastAccounts := FModel.Metrics(mdkAccounts).FetchCount;
  LLastOperations := FModel.Metrics(mdkOperations).FetchCount;
  LLastAccountPageSize := Grid(mdkAccounts).PageSize;
  LLastOperationPageSize := Grid(mdkOperations).PageSize;
  repeat
    Pump(10);
    if Grid(mdkAccounts).IsUpdating or Grid(mdkOperations).IsUpdating or
      (FModel.Metrics(mdkAccounts).ActiveFetches <> 0) or
      (FModel.Metrics(mdkOperations).ActiveFetches <> 0) or
      (FModel.Metrics(mdkAccounts).FetchCount <> LLastAccounts) or
      (FModel.Metrics(mdkOperations).FetchCount <> LLastOperations) or
      TGridGeometryAccess(Grid(mdkAccounts)).HasPendingLayout or
      TGridGeometryAccess(Grid(mdkOperations)).HasPendingLayout or
      (Grid(mdkAccounts).PageSize <> LLastAccountPageSize) or
      (Grid(mdkOperations).PageSize <> LLastOperationPageSize) then
      LStable := GetTickCount64;
    LLastAccounts := FModel.Metrics(mdkAccounts).FetchCount;
    LLastOperations := FModel.Metrics(mdkOperations).FetchCount;
    LLastAccountPageSize := Grid(mdkAccounts).PageSize;
    LLastOperationPageSize := Grid(mdkOperations).PageSize;
    if GetTickCount64 - LStable >= 400 then Exit;
  until GetTickCount64 >= LDeadline;
  raise Exception.Create('The real wallet grid workers and populated geometry did not settle');
end;

procedure TNetworkOffChecks.WaitForRefresh(AAccounts, AOperations: Integer; ATimeout: Cardinal);
var LDeadline: UInt64;
begin
  LDeadline := GetTickCount64 + ATimeout;
  while ((FMain.MockWallet.AccountRefreshCount < AAccounts) or
    (FMain.MockWallet.OperationRefreshCount < AOperations)) and
    (GetTickCount64 < LDeadline) do Pump(10);
  Require((FMain.MockWallet.AccountRefreshCount = AAccounts) and
    (FMain.MockWallet.OperationRefreshCount = AOperations),
    Format('Expected per-grid admissions %d/%d, observed %d/%d',
    [AAccounts, AOperations, FMain.MockWallet.AccountRefreshCount,
      FMain.MockWallet.OperationRefreshCount]));
end;

function TNetworkOffChecks.Grid(AKind: TMockDataKind): TVisualGrid;
begin
  if AKind = mdkAccounts then Result := FMain.MockWallet.AccountsGrid
  else Result := FMain.MockWallet.OperationsGrid;
end;

procedure TNetworkOffChecks.CheckGridState(AKind: TMockDataKind);
begin
  Require(Grid(AKind).RefreshThrottle.Mode = temNone,
    'The actual wallet must not restore its removed quiet-time policy');
  Require(not Grid(AKind).RefreshThrottle.Waiting,
    'Wallet input must not create a throttle waiting state');
  Require(Grid(AKind).LoadingVisible =
    (TUserInterface.SynchronizationStatus.State=nssSynchronizing),
    'The manual card must follow actual node synchronization');
  Require(Grid(AKind).RefreshSuspended =
    (TUserInterface.SynchronizationStatus.State=nssSynchronizing),
    'Query suspension must follow actual node synchronization');
end;

procedure TNetworkOffChecks.CheckNodePresentation;
begin
  CheckGridState(mdkAccounts);
  CheckGridState(mdkOperations);
end;

procedure TNetworkOffChecks.CheckRows(ARevision: Integer);
var LRow: Variant; LPrefix: String;
begin
  LPrefix := 'mock revision=' + IntToStr(ARevision) + ' ';
  Require((FMain.MockWallet.AccountsGrid.RowCount > 0) and
    (FMain.MockWallet.OperationsGrid.RowCount > 0), 'Both real grids must publish rows');
  LRow := FMain.MockWallet.AccountsGrid.Rows[0];
  Require(Pos(LPrefix, String(LRow.Name)) = 1, 'Accounts did not publish the latest revision');
  LRow := FMain.MockWallet.OperationsGrid.Rows[0];
  Require(Pos(LPrefix, String(LRow.Description)) = 1, 'Operations did not publish the latest revision');
end;

procedure TNetworkOffChecks.CheckConnections;
var LConnections: TList<TNetConnection>; LConnection: TNetConnection; LAddress: String;
begin
  Require(TSettings.TryConnectOnlyWithThisFixedServers = '127.0.0.1:1', 'Discovery must remain restricted to the fixed loopback peer');
  Require(not TSettings.JsonRpcPortEnabled and not TSettings.JsonRpcMinerServerActive,
    'RPC and mining must remain disabled');
  LConnections := TNetData.NetData.NetConnections.LockList;
  try
    for LConnection in LConnections do begin
      LAddress := LConnection.ClientRemoteAddr;
      Require((LAddress = '') or (LAddress = 'NIL') or (LAddress = '127.0.0.1') or
        (Pos('127.0.0.1:', LAddress) = 1),
        'A connection escaped loopback: ' + LAddress);
      Require(not LConnection.Connected, 'The unavailable loopback peer must not establish a connection');
    end;
  finally TNetData.NetData.NetConnections.UnlockList; end;
  Require(TNetData.NetData.NetStatistics.ActiveConnections = 0, 'This probe must have no connected peers');
end;

function TNetworkOffChecks.Notify(ARevision: Integer): UInt64;
begin
  FModel.SetData(ARevision, 200);
  Result := GetTickCount64;
  TUserInterface.Node.NotifyBlocksChanged;
end;

function TNetworkOffChecks.Burst(AFirstRevision: Integer): UInt64;
var I: Integer;
begin
  Result := 0;
  for I := 0 to 6 do begin
    Result := Notify(AFirstRevision + I);
    Pump(50);
  end;
end;

procedure TNetworkOffChecks.ClickConnectivity(AExpectedActive: Boolean);
var LStarted, LReturned: UInt64;
begin
  Require(not TUserInterface.Offline, 'The actual connectivity handler must not take its offline early return');
  Require(TNetData.NetData.NetConnectionsActive <> AExpectedActive, 'Connectivity must actually change on this click');
  LStarted := GetTickCount64;
  FMain.tbtnConnectivityClick(FMain.tbtnConnectivity);
  LReturned := GetTickCount64;
  Require(LReturned - LStarted < 400, 'The actual toolbar action blocked the main thread');
  // The controller publishes intent to its owned worker. Wait for the actual
  // core transition, without repeatedly invoking any notification or handler.
  while (TNetData.NetData.NetConnectionsActive <> AExpectedActive) and
    (GetTickCount64 - LStarted < 3000) do Pump(10);
  Require(TNetData.NetData.NetConnectionsActive = AExpectedActive, 'The actual toolbar action did not toggle networking');
  Log('ACTUAL connectivity action active=' + BoolToStr(AExpectedActive, true) +
    ' handler_ms=' + UIntToStr(LReturned - LStarted));
end;

procedure TNetworkOffChecks.BusyPagesThenNetworkOff;
var
  LLastInput, LIndependentInput: UInt64;
  LAccounts, LOperations, LAccountAdmissions, LOperationAdmissions: Integer;
begin
  // Initial result totals and native scrollbars can change page capacity.
  // Settle that observed layout before isolating backend coalescing counts.
  Settle;
  CheckNodePresentation;
  CheckRows(FModel.Revision);
  FModel.GateFetches := true;
  Notify(900);
  WaitForFetches;
  CheckNodePresentation;
  LAccountAdmissions := FMain.MockWallet.AccountRefreshCount;
  LOperationAdmissions := FMain.MockWallet.OperationRefreshCount;
  LAccounts := FModel.Metrics(mdkAccounts).FetchCount;
  LOperations := FModel.Metrics(mdkOperations).FetchCount;
  LLastInput := Burst(1000);
  CheckNodePresentation;
  Require((FMain.MockWallet.AccountRefreshCount > LAccountAdmissions) and
    (FMain.MockWallet.OperationRefreshCount > LOperationAdmissions),
    'Busy grids must record new backend inputs without a quiet-time wait');
  ClickConnectivity(false);
  Pump(250);
  Require((FModel.Metrics(mdkAccounts).FetchCount = LAccounts) and
    (FModel.Metrics(mdkOperations).FetchCount = LOperations),
    'New inputs cannot start parallel queries while initial workers are held');
  FModel.ReleaseFetches;
  Settle;
  Require((FModel.Metrics(mdkAccounts).FetchCount = LAccounts + 1) and
    (FModel.Metrics(mdkOperations).FetchCount = LOperations + 1),
    'Worker retirement must issue one latest coalesced page per grid');
  Require(GetTickCount64 - LLastInput < 2500,
    'Busy-worker retirement imposed a quiet-time delay on the latest pages');
  CheckNodePresentation;
  CheckRows(1006);
  CheckConnections;

  LAccounts := FModel.Metrics(mdkAccounts).FetchCount;
  LOperations := FModel.Metrics(mdkOperations).FetchCount;
  LAccountAdmissions := FMain.MockWallet.AccountRefreshCount;
  LOperationAdmissions := FMain.MockWallet.OperationRefreshCount;
  LIndependentInput := Notify(1500);
  WaitForRefresh(LAccountAdmissions + 1, LOperationAdmissions + 1, 1800);
  Require((FMain.MockWallet.LastAccountRefreshTick - LIndependentInput < 1800) and
    (FMain.MockWallet.LastOperationRefreshTick - LIndependentInput < 1800),
    'An independent input was delayed after network-off');
  Settle;
  CheckRows(1500);
  Pump(250);
  Require((FModel.Metrics(mdkAccounts).FetchCount = LAccounts + 1) and
    (FModel.Metrics(mdkOperations).FetchCount = LOperations + 1),
    'A completed independent input must not manufacture another page query');
  CheckNodePresentation;
  Log('PASS actual network-off: busy-worker coalescing, immediate latest pages and independent inputs');
end;

procedure TNetworkOffChecks.ActiveWorkThenNetworkOff;
var
  LEndTick, LDeadline: UInt64;
  LAccountAdmissions, LOperationAdmissions, LAccounts, LOperations: Integer;
begin
  ClickConnectivity(true);
  LAccounts := FModel.Metrics(mdkAccounts).FetchCount;
  LOperations := FModel.Metrics(mdkOperations).FetchCount;
  FModel.GateFetches := true;
  TUserInterface.Node.BeginBlockChainUpdate;
  FWorkActive := true;
  try
    LDeadline := GetTickCount64 + 3000;
    repeat
      Pump(10);
      if Grid(mdkAccounts).RefreshSuspended and Grid(mdkOperations).RefreshSuspended then Break;
    until GetTickCount64 >= LDeadline;
    Require(Grid(mdkAccounts).LoadingVisible and Grid(mdkOperations).LoadingVisible,
      'Physical buffered work must show both synchronization cards');
    Burst(2000);
    ClickConnectivity(false);
    // Received work outlives transport activity and any former five-second
    // presentation timeout. Only the actual node lifecycle may release it.
    Pump(5500);
    Require(TUserInterface.Node.IsUpdatingBlockChain and
      (TUserInterface.SynchronizationStatus.State = nssSynchronizing),
      'Transport off cannot finish actual buffered processing');
    CheckNodePresentation;
    Require(not Grid(mdkAccounts).IsUpdating and not Grid(mdkOperations).IsUpdating,
      'Suspended requests must not start page workers');
    Require((FModel.Metrics(mdkAccounts).FetchCount = LAccounts) and
      (FModel.Metrics(mdkOperations).FetchCount = LOperations),
      'Silence cannot query while buffered processing remains');
    Require(not Grid(mdkAccounts).InternalDrawGrid.Enabled and
      not Grid(mdkOperations).InternalDrawGrid.Enabled,
      'The node-owned synchronization card must block inputs');
    LAccountAdmissions := FMain.MockWallet.AccountRefreshCount;
    LOperationAdmissions := FMain.MockWallet.OperationRefreshCount;
  finally
    LEndTick := GetTickCount64;
    TUserInterface.Node.EndBlockChainUpdate;
    FWorkActive := false;
  end;
  WaitForFetches;
  Require(GetTickCount64 - LEndTick < 1800,
    'Backlog completion must resume the retained pages immediately');
  CheckNodePresentation;
  Require(not Grid(mdkAccounts).LoadingVisible and not Grid(mdkOperations).LoadingVisible,
    'Both manual cards must clear when physical processing finishes');
  Require(Grid(mdkAccounts).IsUpdating and Grid(mdkOperations).IsUpdating,
    'The resumed pages must remain in their controlled fetch gate');
  Require((FModel.Metrics(mdkAccounts).FetchCount = LAccounts + 1) and
    (FModel.Metrics(mdkOperations).FetchCount = LOperations + 1),
    'Ending the backlog must execute one latest retained page per grid');
  // EndBlockChainUpdate emits one real BlocksChanged event when pending
  // state is available. The notifier delivers it before the final resume.
  Require((FMain.MockWallet.AccountRefreshCount = LAccountAdmissions + 1) and
    (FMain.MockWallet.OperationRefreshCount = LOperationAdmissions + 1),
    'The real node completion event must add one input per grid before resume');
  FModel.ReleaseFetches;
  Settle;
  CheckRows(2006);
  CheckConnections;
  Log('PASS actual network-off preserves synchronization until backlog end, then one latest page resumes');
end;

procedure TNetworkOffChecks.Run;
var LDirectory: String; LID: TGUID; LOptions: TWalletStartupOptions; LDeadline: UInt64;
begin
  RequirePrivateDesktop;
  Require(not TNode.NodeExists and not TNetData.NetDataExists, 'A dedicated process must start without node/network singletons');
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) + 'PascalCoinNetworkOff-' + GUIDToString(LID);
  Require(not DirectoryExists(LDirectory), 'The test data directory must be new');
  Require(ForceDirectories(LDirectory), 'Cannot create the test data directory');
  SeedLoopbackSettings(LDirectory);
  LOptions := Default(TWalletStartupOptions);
  LOptions.DataDirectory := LDirectory;
  LOptions.Offline := false;
  LOptions.NoStartupDialogs := true;
  TUserInterface.ConfigureStartup(LOptions);
  Application.CreateForm(TMockMainForm, FMain);
  FModel := FMain.Model;
  FMain.Caption := 'MOCK DATA - loopback-only connectivity regression';
  TUserInterface.StartApplication(FMain);
  if not TUserInterface.Started then begin
    FMain := nil; FModel := nil;
    raise Exception.Create('The actual controller did not start');
  end;
  FMain.Show;
  LDeadline := GetTickCount64 + 12000;
  repeat
    Pump(10);
    Require(TUserInterface.State <> uisError, 'Fresh loopback-only startup failed');
    if Assigned(FMain.MockWallet) then Break;
  until GetTickCount64 >= LDeadline;
  Require(Assigned(FMain.MockWallet), 'The actual main wallet was not constructed');
  Require(TSettings.TryConnectOnlyWithThisFixedServers = '127.0.0.1:1', 'The controller did not load the fixed loopback restriction');
  while (not TNetData.NetData.NetConnectionsActive) and
    (GetTickCount64 < LDeadline) do Pump(10);
  Require(TNetData.NetData.NetConnectionsActive, 'The actual runtime must start with connectivity enabled');
  CheckConnections;
  Log('START fresh loopback-only online-policy controller; RPC/mining off; real startup geometry settles before gated backend queries');
  BusyPagesThenNetworkOff;
  ActiveWorkThenNetworkOff;
  Log('PASS all real network-off regressions');
end;

function RunWalletNetworkOffProbe: Integer;
var LBag: TDisposables; LChecks: TNetworkOffChecks;
begin
  Result := 1;
  if ParamCount <> 1 then Exit;
  Application.Initialize;
  LChecks := LBag.AddObject(TNetworkOffChecks.Create(ExpandFileName(ParamStr(1)))) as TNetworkOffChecks;
  try
    LChecks.Run;
    Result := 0;
  except
    on E: Exception do LChecks.Log('FAIL ' + E.ClassName + ': ' + E.Message);
  end;
end;

end.
