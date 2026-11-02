unit UWalletGridIntegrationTests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Exercises the real main form, wallet control, cached data sources and both
  rendered grids. Only the stored account/history records are mocked.

  Distributed under the MIT software license; see the accompanying LICENSE.
}

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, fpcunit, testregistry, UWalletGridMocks;

type
  TWalletGridIntegrationTests = class(TTestCase)
  private
    FMain: TMockMainForm;
    FWallet: TMockWallet;
    FModel: TMockWalletData;
    FProducer: TThread;
    FDirectory, FPreviousDirectory, FAsyncError: String;
    FEvents: TStringList;
    FPreviousExceptionHandler: TExceptionEvent;
    procedure OnException(Sender: TObject; E: Exception);
    procedure PumpFor(AMilliseconds: Cardinal);
    procedure Settle;
    procedure WaitForRefresh(ACount: Integer; ATimeout: Cardinal = 8000);
    procedure CheckRenderedRevision(ARevision: Integer);
    procedure CheckRefreshState(const ACheckpoint: String;
      AExpectIdle: Boolean = false; AAutomaticUpdating: Boolean = false);
    procedure CheckFetchCounts(const ACheckpoint: String; AAccounts, AOperations: Integer);
    procedure Log(const AMessage: String);
    function NotifyChange(ARevision: Integer): QWord;
    procedure CheckProductionWiringAndCachedPaging;
    procedure CheckEventDrivenOverview;
    procedure CheckSingleInputLoading;
    procedure CheckContinuousInputs;
    procedure CheckSilentProcessingGap;
    procedure CheckSlowFetchDuringContinuousInputs;
    procedure CheckImmediateInputsAndIndependentGrids;
    procedure CheckHiddenAccountsRequests;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // The controller has a process lifetime. Keep these scenarios together in
    // the dedicated runner rather than changing globals in the general suite.
    procedure MainGridsRefreshImmediatelyAndPauseForNodeWork;
  end;

implementation

uses
  Windows, Variants, Types, Controls, StdCtrls, ExtCtrls, Buttons, UThread, UNode,
  UUserInterface, UWalletStartup, UVisualGrid, UCommon, UCommon.UI, UCommon.Data,
  UCTRLWallet, UAccounts, UCoreUtils;

type
  TGridInputAccess = class(TVisualGrid)
  public
    procedure Search(const AExpression: String);
    function HasPendingLayout: Boolean;
  end;

  TGridUpdateProducer = class(TPCThread)
  private
    FNode: TNode;
    FModel: TMockWalletData;
    FStop: LongInt;
  protected
    procedure BCExecute; override;
  public
    InputCount: Integer;
    constructor Create(ANode: TNode; AModel: TMockWalletData);
    destructor Destroy; override;
    procedure Stop;
  end;

procedure TGridInputAccess.Search(const AExpression: String);
begin
  FSearchEdit.Text := AExpression;
  SearchButtonClick(Self);
end;

function TGridInputAccess.HasPendingLayout: Boolean;
begin
  Result := Assigned(FDelayedBoundsChangeTimer) and FDelayedBoundsChangeTimer.Enabled;
end;

function DesktopName(ADesktop: THandle): String;
var
  LName: array[0..255] of Char;
  LNeeded: DWORD;
begin
  FillChar(LName, SizeOf(LName), 0);
  if not GetUserObjectInformation(ADesktop, UOI_NAME, @LName[0], SizeOf(LName), LNeeded) then
    RaiseLastOSError;
  Result := LName;
end;

procedure RequirePrivateDesktop;
var
  LInput: THandle;
  LCurrentName, LInputName: String;
begin
  LInput := OpenInputDesktop(0, false, DESKTOP_READOBJECTS);
  if LInput = 0 then RaiseLastOSError;
  try
    LCurrentName := DesktopName(GetThreadDesktop(GetCurrentThreadId));
    LInputName := DesktopName(LInput);
    if SameText(LCurrentName, LInputName) or (Pos('PascalCoin', LCurrentName) <> 1) then
      raise Exception.Create('Run the grid integration test through the existing private-desktop broker');
  finally
    CloseDesktop(LInput);
  end;
end;

constructor TGridUpdateProducer.Create(ANode: TNode; AModel: TMockWalletData);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FNode := ANode;
  FModel := AModel;
end;

destructor TGridUpdateProducer.Destroy;
begin
  Stop;
  inherited;
end;

procedure TGridUpdateProducer.BCExecute;
begin
  while InterlockedCompareExchange(FStop, 0, 0) = 0 do begin
    FModel.Advance;
    FNode.NotifyBlocksChanged;
    Inc(InputCount);
    Sleep(100);
  end;
end;

procedure TGridUpdateProducer.Stop;
begin
  InterlockedExchange(FStop, 1);
  WaitFor;
end;

procedure TWalletGridIntegrationTests.OnException(Sender: TObject; E: Exception);
var
  I: Integer;
begin
  if FAsyncError <> '' then Exit;
  FAsyncError := E.ClassName + ': ' + E.Message;
  // Preserve the original GUI exception location before FPCUnit records the
  // assertion in PumpFor. This also makes asynchronous startup failures useful.
  if Assigned(FEvents) then begin
    FEvents.Add('ASYNC ' + FAsyncError);
    if Assigned(BackTraceStrFunc) then begin
      FEvents.Add(BackTraceStrFunc(ExceptAddr));
      for I := 0 to ExceptFrameCount - 1 do
        FEvents.Add(BackTraceStrFunc(ExceptFrames[I]));
    end;
    FEvents.SaveToFile(ParamStr(1) + '.async.log');
  end;
end;

procedure TWalletGridIntegrationTests.Log(const AMessage: String);
begin
  FEvents.Add(UIntToStr(GetTickCount64) + ' CHECK ' + AMessage);
end;

procedure TWalletGridIntegrationTests.PumpFor(AMilliseconds: Cardinal);
var
  LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    if Assigned(FModel) then FModel.DrainEvents(FEvents);
    if FAsyncError <> '' then Fail('Asynchronous main-wallet error: ' + FAsyncError);
    Sleep(1);
  until GetTickCount64 >= LDeadline;
end;

procedure TWalletGridIntegrationTests.Settle;
var
  LDeadline, LStableSince: QWord;
  LAccounts, LOperations, LLastAccounts, LLastOperations: TMockFetchMetrics;
  LLastAccountPageSize, LLastOperationPageSize: Integer;
begin
  LDeadline := GetTickCount64 + 10000;
  LStableSince := GetTickCount64;
  LLastAccounts := FModel.Metrics(mdkAccounts);
  LLastOperations := FModel.Metrics(mdkOperations);
  LLastAccountPageSize := FWallet.AccountsGrid.PageSize;
  LLastOperationPageSize := FWallet.OperationsGrid.PageSize;
  repeat
    PumpFor(10);
    LAccounts := FModel.Metrics(mdkAccounts);
    LOperations := FModel.Metrics(mdkOperations);
    if FWallet.AccountsGrid.IsUpdating or FWallet.OperationsGrid.IsUpdating or
      (LAccounts.ActiveFetches <> 0) or (LOperations.ActiveFetches <> 0) or
      (LAccounts.FetchCount <> LLastAccounts.FetchCount) or
      (LOperations.FetchCount <> LLastOperations.FetchCount) or
      TGridInputAccess(FWallet.AccountsGrid).HasPendingLayout or
      TGridInputAccess(FWallet.OperationsGrid).HasPendingLayout or
      (FWallet.AccountsGrid.PageSize <> LLastAccountPageSize) or
      (FWallet.OperationsGrid.PageSize <> LLastOperationPageSize) then
      LStableSince := GetTickCount64;
    LLastAccounts := LAccounts;
    LLastOperations := LOperations;
    // First population can change footer/scrollbar capacity. Observe that
    // geometry settling before taking exact-query-count baselines.
    LLastAccountPageSize := FWallet.AccountsGrid.PageSize;
    LLastOperationPageSize := FWallet.OperationsGrid.PageSize;
    if GetTickCount64 - LStableSince >= 400 then Exit;
  until GetTickCount64 >= LDeadline;
  Fail('The real wallet grids did not settle within ten seconds');
end;

procedure TWalletGridIntegrationTests.WaitForRefresh(ACount: Integer; ATimeout: Cardinal);
var
  LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + ATimeout;
  while (FWallet.AccountRefreshCount < ACount) and (GetTickCount64 < LDeadline) do PumpFor(10);
  AssertEquals('The actual account-grid admission must have the expected count', ACount, FWallet.AccountRefreshCount);
end;

procedure TWalletGridIntegrationTests.CheckFetchCounts(const ACheckpoint: String;
  AAccounts, AOperations: Integer);
begin
  AssertEquals(ACheckpoint + ': account queries', AAccounts, FModel.Metrics(mdkAccounts).FetchCount);
  AssertEquals(ACheckpoint + ': operation queries', AOperations, FModel.Metrics(mdkOperations).FetchCount);
end;

procedure TWalletGridIntegrationTests.CheckRenderedRevision(ARevision: Integer);
var
  LRow: Variant;
  LExpected: String;
begin
  AssertTrue('Account grid must display a fetched row', FWallet.AccountsGrid.RowCount > 0);
  AssertTrue('Operation grid must display a fetched row', FWallet.OperationsGrid.RowCount > 0);
  LExpected := 'mock revision=' + IntToStr(ARevision) + ' ';
  LRow := FWallet.AccountsGrid.Rows[0];
  AssertTrue('Account rendering must use the final invalidated snapshot: ' + String(LRow.Name),
    Pos(LExpected, String(LRow.Name)) = 1);
  LRow := FWallet.OperationsGrid.Rows[0];
  AssertTrue('Operation rendering must use the final invalidated snapshot: ' + String(LRow.Description),
    Pos(LExpected, String(LRow.Description)) = 1);
  AssertEquals('Account fetch must observe the final revision', ARevision,
    FModel.Metrics(mdkAccounts).LastRevision);
  AssertEquals('Operation fetch must observe the final revision', ARevision,
    FModel.Metrics(mdkOperations).LastRevision);
  Log('Both real grids display revision ' + IntToStr(ARevision));
end;

procedure TWalletGridIntegrationTests.CheckRefreshState(const ACheckpoint: String;
  AExpectIdle: Boolean; AAutomaticUpdating: Boolean);

  procedure CheckGrid(AGrid: TVisualGrid; const AName: String);
  var
    I, LEditCount, LButtonCount: Integer;
    LInputAllowed, LNodeSynchronizing: Boolean;
    LComponent: TComponent;
    LPanel: TPanel;
    LTitle, LSubtitle: TLabel;

    function InputEnabled(AControl: TControl): Boolean;
    begin
      Result := true;
      while Assigned(AControl) do begin
        if not AControl.Enabled then Exit(false);
        if AControl = AGrid then Exit;
        AControl := AControl.Parent;
      end;
    end;

  begin
    LNodeSynchronizing := TUserInterface.SynchronizationStatus.State=nssSynchronizing;
    AssertEquals(ACheckpoint+': '+AName+' no wallet throttle',Ord(temNone),Ord(AGrid.RefreshThrottle.Mode));
    AssertFalse(ACheckpoint+': '+AName+' no quiet-time wait',AGrid.RefreshThrottle.Waiting);
    AssertEquals(ACheckpoint+': '+AName+' node manual card',LNodeSynchronizing,AGrid.LoadingVisible);
    AssertEquals(ACheckpoint+': '+AName+' node query suspension',LNodeSynchronizing,AGrid.RefreshSuspended);
    AssertTrue('The loading animation retains an enabled grid owner', AGrid.Enabled);
    LInputAllowed := not (LNodeSynchronizing or AAutomaticUpdating or AGrid.IsUpdating);
    AssertEquals(ACheckpoint + ': ' + AName + ' row input', LInputAllowed,
      AGrid.InternalDrawGrid.Enabled);
    if AExpectIdle then
      AssertFalse('A held loading presentation is independent of an active query', AGrid.IsUpdating);
    LPanel := AGrid.FindComponent('LoadingOverlay') as TPanel;
    AssertTrue('The actual grid owns its loading card', Assigned(LPanel));
    // Automatic page loading retains its own delayed card after a manual hold
    // ends. Its visibility is independent of the synchronization presentation.
    if not AAutomaticUpdating and not AGrid.IsUpdating then
      AssertEquals(ACheckpoint + ': ' + AName + ' visible loading card', LNodeSynchronizing, LPanel.Visible);
    if LNodeSynchronizing then begin
      LTitle := AGrid.FindComponent('LoadingStatus') as TLabel;
      LSubtitle := AGrid.FindComponent('LoadingSubtitle') as TLabel;
      AssertTrue(Assigned(LTitle) and Assigned(LSubtitle));
      AssertEquals('The actual title reflects the requested synchronization state', 'Please Wait', LTitle.Caption);
      AssertEquals('The actual subtitle reflects the requested synchronization state', 'Synchronizing', LSubtitle.Caption);
      AssertTrue('The synchronization subtitle is visible', LSubtitle.Visible);
    end;

    // The grid owns its existing search, page-size and page-index controls.
    // Walk their real parent chain to check the complete input-region gates;
    // a disabled draw grid alone would leave search and paging interactive.
    LEditCount := 0;
    LButtonCount := 0;
    for I := 0 to AGrid.ComponentCount - 1 do begin
      LComponent := AGrid.Components[I];
      if LComponent is TEdit then begin
        Inc(LEditCount);
        // Ordinary fetches make edits read-only; only the manual hold
        // disables entire input regions. Navigation has its own disabled parent.
        if LNodeSynchronizing then
          AssertFalse(ACheckpoint + ': ' + AName + ' held search/paging input',
            InputEnabled(TEdit(LComponent)))
        else if LInputAllowed then
          AssertTrue(ACheckpoint + ': ' + AName + ' ready search/paging input',
            InputEnabled(TEdit(LComponent)));
        AssertEquals(ACheckpoint + ': ' + AName + ' search/paging read-only', not LInputAllowed,
          TEdit(LComponent).ReadOnly);
      end else if (LComponent is TButton) or (LComponent is TSpeedButton) then begin
        Inc(LButtonCount);
        if LNodeSynchronizing then
          AssertFalse(ACheckpoint + ': ' + AName + ' search/paging action', InputEnabled(TControl(LComponent)));
      end;
    end;
    AssertTrue('Check search, page-size and page-index input', LEditCount >= 3);
    AssertTrue('Check the native page navigation and search buttons', LButtonCount >= 5);
  end;

begin
  CheckGrid(FWallet.AccountsGrid, 'accounts');
  CheckGrid(FWallet.OperationsGrid, 'operations');
  Log(ACheckpoint + ': wallet inputs are immediate; node owns synchronization');
end;

function TWalletGridIntegrationTests.NotifyChange(ARevision: Integer): QWord;
begin
  FModel.SetData(ARevision, 200);
  Result := GetTickCount64;
  TUserInterface.Node.NotifyBlocksChanged;
  Log('Node input revision=' + IntToStr(ARevision) + ' input_tick=' + UIntToStr(Result));
end;

procedure TWalletGridIntegrationTests.SetUp;
var
  LID: TGUID;
  LOptions: TWalletStartupOptions;
  LDeadline: QWord;
begin
  inherited;
  RequirePrivateDesktop;
  AssertFalse('The dedicated fixture must start without an existing node', TNode.NodeExists);
  FEvents := TStringList.Create;
  FPreviousExceptionHandler := Application.OnException;
  Application.OnException := OnException;
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  try
    CreateGUID(LID);
    FDirectory := IncludeTrailingPathDelimiter(GetTempDir) + 'WalletGridIntegration-' + GUIDToString(LID);
    AssertFalse('Fresh test data must not already exist', DirectoryExists(FDirectory));
    LOptions := Default(TWalletStartupOptions);
    LOptions.DataDirectory := FDirectory;
    LOptions.Offline := true;
    LOptions.NoStartupDialogs := true;
    TUserInterface.ConfigureStartup(LOptions);
    Application.CreateForm(TMockMainForm, FMain);
    FModel := FMain.Model;
    TUserInterface.StartApplication(FMain);
    if not TUserInterface.Started then begin
      // Failed controller startup already releases its form graph.
      FMain := nil;
      FModel := nil;
      Fail('The actual application controller did not start: ' + TUserInterface.StateText);
    end;
    FMain.Show;
    LDeadline := GetTickCount64 + 10000;
    repeat
      PumpFor(10);
      if TUserInterface.State = uisError then Fail('Mock wallet startup failed: ' + TUserInterface.StateText);
      if Assigned(FMain.MockWallet) then Break;
    until GetTickCount64 >= LDeadline;
    FWallet := FMain.MockWallet;
    AssertTrue('The main form must create the actual mock-backed wallet through its factory', Assigned(FWallet));
    Settle;
    Log('Actual main form/controller started offline with typed mock providers');
  except
    // FPCUnit does not call TearDown when SetUp itself raises.
    TearDown;
    raise;
  end;
end;

procedure TWalletGridIntegrationTests.TearDown;
begin
  try
    if Assigned(FProducer) then begin
      TGridUpdateProducer(FProducer).Stop;
      FreeAndNil(FProducer);
    end;
    if Assigned(FModel) then begin
      FModel.ReleaseFetches;
      FModel.ReleaseBalanceSnapshots;
      FModel.DrainEvents(FEvents);
    end;
    if Assigned(FMain) then begin
      TUserInterface.ExitApplication;
      FMain := nil;
      FWallet := nil;
      FModel := nil;
    end;
    if Assigned(FEvents) then FEvents.SaveToFile(ParamStr(1) + '.events.log');
  finally
    Application.OnException := FPreviousExceptionHandler;
    FEvents.Free;
    FEvents := nil;
    TNode.SetPascalCoinDataFolder(FPreviousDirectory);
    inherited;
  end;
end;

procedure TWalletGridIntegrationTests.CheckProductionWiringAndCachedPaging;
var
  LBefore, LAfter: TMockFetchMetrics;
  LAccountsSource, LOperationsSource: IDataSource;
  LCell: TRect;
  LPoint: LPARAM;
  LRow: Variant;
begin
  LAccountsSource := FWallet.AccountsSource;
  LOperationsSource := FWallet.OperationsSource;
  AssertTrue('Accounts must use the same typed source held by the wallet',
    FWallet.AccountsGrid.DataSource = LAccountsSource);
  AssertTrue('Operations must use the same typed source held by the wallet',
    FWallet.OperationsGrid.DataSource = LOperationsSource);
  AssertTrue('Account snapshot cache must remain enabled', FWallet.AccountsSource.SnapshotCacheEnabled);
  AssertTrue('History snapshot cache must remain enabled', FWallet.OperationsSource.SnapshotCacheEnabled);
  AssertTrue(FWallet.AccountsGrid.FetchDataInThread and FWallet.OperationsGrid.FetchDataInThread);
  AssertTrue(FWallet.AccountsGrid.AutoPageSize and FWallet.OperationsGrid.AutoPageSize);
  AssertTrue(Assigned(FWallet.AccountsGrid.OnFinishedUpdating) and Assigned(FWallet.AccountsGrid.OnSelection));
  AssertTrue(Assigned(FWallet.OperationsGrid.OnSelection));
  AssertEquals('The real wallet account grid admits inputs immediately',Ord(temNone),
    Ord(FWallet.AccountsGrid.RefreshThrottle.Mode));
  AssertEquals('The real wallet operations grid admits inputs immediately',Ord(temNone),
    Ord(FWallet.OperationsGrid.RefreshThrottle.Mode));
  CheckRenderedRevision(FModel.Revision);
  CheckRefreshState('Initial loaded grids', true);

  LBefore := FModel.Metrics(mdkAccounts);
  FWallet.AccountsGrid.PageIndex := 1;
  Settle;
  LAfter := FModel.Metrics(mdkAccounts);
  AssertEquals('Changing an account page requests one page', LBefore.FetchCount + 1, LAfter.FetchCount);
  AssertEquals('Account paging reads only the displayed records', FWallet.AccountsGrid.RowCount,
    LAfter.PageAccountReadCount - LBefore.PageAccountReadCount);
  AssertTrue('One visible page must not read all two hundred accounts', FWallet.AccountsGrid.RowCount < 200);
  FWallet.AccountsGrid.PageIndex := 0;
  Settle;

  LBefore := FModel.Metrics(mdkOperations);
  FWallet.OperationsGrid.PageIndex := 1;
  Settle;
  LAfter := FModel.Metrics(mdkOperations);
  AssertEquals('Changing a history page requests one page', LBefore.FetchCount + 1, LAfter.FetchCount);
  AssertEquals('Warm history paging retains the raw snapshot', LBefore.RawReadCount, LAfter.RawReadCount);
  AssertEquals('Warm history paging retains resolved membership', LBefore.MembershipReadCount, LAfter.MembershipReadCount);
  FWallet.OperationsGrid.PageIndex := 0;
  Settle;

  // Click the actual draw grid: the production selection handler must switch
  // history membership through the same typed source used by node refreshes.
  LCell := FWallet.AccountsGrid.InternalDrawGrid.CellRect(0, 1);
  LPoint := LPARAM(((LCell.Top + LCell.Bottom) div 2) shl 16) or
    LPARAM((LCell.Left + LCell.Right) div 2);
  Windows.SendMessage(FWallet.AccountsGrid.InternalDrawGrid.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPoint);
  Windows.SendMessage(FWallet.AccountsGrid.InternalDrawGrid.Handle, WM_LBUTTONUP, 0, LPoint);
  Settle;
  AssertFalse('Real account selection must switch off all-wallet history membership',
    FWallet.OperationsSource.UseWalletAccounts);
  AssertEquals('Only the clicked account is selected', 1, Length(FWallet.OperationsSource.Accounts));
  AssertEquals('The real history grid displays only the selected account', 1, FWallet.OperationsGrid.RowCount);
  LRow := FWallet.OperationsGrid.Rows[0];
  AssertEquals('The visible selected operation belongs to the clicked first account', 0, Integer(LRow.AccountNumber));
  FWallet.AccountsGrid.ClearSelection(true);
  Settle;
  AssertTrue('Clearing selection restores all-wallet history membership', FWallet.OperationsSource.UseWalletAccounts);
  AssertTrue('All-wallet history returns more than one operation', FWallet.OperationsGrid.RowCount > 1);
  PumpFor(1100);
  Log('Production wiring, actual selection callbacks, account page-only reads and history snapshot reuse passed');
end;

procedure TWalletGridIntegrationTests.CheckEventDrivenOverview;
var
  LReads, LAdmissions: Integer;
  LDeadline: QWord;
begin
  AssertEquals('Initial totals publish from the aggregate completion event',
    '200', FWallet.lblTotalPASA.Caption);
  LReads := FModel.Metrics(mdkAccounts).BalanceAccountReadCount;
  LAdmissions := FWallet.AccountRefreshCount;
  // Hold storage, not grid admission. Pages finish while the independently
  // owned aggregate retains the last complete overview.
  FModel.HoldBalanceSnapshots;
  try
    FModel.SetData(4800, 37);
    TUserInterface.Node.NotifyBlocksChanged;
    WaitForRefresh(LAdmissions + 1, 1800);
    Settle;
    CheckRefreshState('Completed page with an unfinished overview', true);
    AssertEquals('A held aggregate cannot read records', LReads,
      FModel.Metrics(mdkAccounts).BalanceAccountReadCount);
    AssertEquals('Retain the last completed overview until the worker finishes',
      '200', FWallet.lblTotalPASA.Caption);
  finally
    FModel.ReleaseBalanceSnapshots;
  end;
  LDeadline := GetTickCount64 + 3000;
  while (FWallet.lblTotalPASA.Caption <> '37') and
    (GetTickCount64 < LDeadline) do PumpFor(10);
  AssertEquals('Worker completion publishes the aggregate without polling',
    '37', FWallet.lblTotalPASA.Caption);
  AssertEquals('The worker completion paints the complete balance',
    TAccountComp.FormatMoney(370666), FWallet.lblTotalPASC.Caption);
  AssertEquals('One aggregate visits exactly the latest wallet membership',
    LReads + 37, FModel.Metrics(mdkAccounts).BalanceAccountReadCount);
  LAdmissions := FWallet.AccountRefreshCount;
  FModel.SetData(4801, 200);
  TUserInterface.Node.NotifyBlocksChanged;
  WaitForRefresh(LAdmissions + 1, 1800);
  Settle;
  LDeadline := GetTickCount64 + 3000;
  while (FWallet.lblTotalPASA.Caption <> '200') and
    (GetTickCount64 < LDeadline) do PumpFor(10);
  AssertEquals('The next completed generation replaces the overview',
    '200', FWallet.lblTotalPASA.Caption);
  Log('Overview retains complete totals while storage is held and publishes through worker completion');
end;

procedure TWalletGridIntegrationTests.CheckSingleInputLoading;
var
  LBaseRefresh, LAccounts, LOperations: Integer;
  LInputTick, LDeadline: QWord;
  LOverlay: TPanel;
begin
  LBaseRefresh := FWallet.AccountRefreshCount;
  FModel.GateFetches := true;
  LInputTick := NotifyChange(500);
  WaitForRefresh(LBaseRefresh + 1, 1800);
  LDeadline := GetTickCount64 + 1800;
  while ((FModel.Metrics(mdkAccounts).ActiveFetches <> 1) or
    (FModel.Metrics(mdkOperations).ActiveFetches <> 1)) and
    (GetTickCount64 < LDeadline) do PumpFor(10);
  AssertEquals('One held account provider',1,FModel.Metrics(mdkAccounts).ActiveFetches);
  AssertEquals('One held operations provider',1,FModel.Metrics(mdkOperations).ActiveFetches);
  PumpFor(250);
  CheckRefreshState('Single input has real providers in flight');
  LOverlay := FWallet.OperationsGrid.FindComponent('LoadingOverlay') as TPanel;
  AssertTrue('A held ordinary query shows its actual loading card',LOverlay.Visible);
  FModel.ReleaseFetches;
  Settle;
  AssertTrue('Publication does not wait for a quiet interval',GetTickCount64-LInputTick < 2500);
  LAccounts := FModel.Metrics(mdkAccounts).FetchCount;
  LOperations := FModel.Metrics(mdkOperations).FetchCount;
  CheckRenderedRevision(500);
  CheckRefreshState('Accepted data clears loading and enables inputs',true);
  PumpFor(250);
  AssertEquals('A completed input has one admission',LBaseRefresh+1,FWallet.AccountRefreshCount);
  CheckFetchCounts('Completion does not manufacture a trailing page',LAccounts,LOperations);
  Log('Single input starts immediately, displays loading only while active and clears it on publication');
end;

procedure TWalletGridIntegrationTests.CheckContinuousInputs;
var
  LBaseRefresh, LAccounts, LOperations: Integer;
  LLastInput: QWord;
begin
  LBaseRefresh := FWallet.AccountRefreshCount;
  NotifyChange(1000);
  WaitForRefresh(LBaseRefresh+1,1800);
  Settle;
  LAccounts := FModel.Metrics(mdkAccounts).FetchCount;
  LOperations := FModel.Metrics(mdkOperations).FetchCount;
  FProducer := TGridUpdateProducer.Create(TUserInterface.Node,FModel);
  FProducer.Start;
  PumpFor(700);
  TGridUpdateProducer(FProducer).Stop;
  AssertTrue('The real background producer delivers repeated inputs',
    TGridUpdateProducer(FProducer).InputCount >= 4);
  FreeAndNil(FProducer);
  AssertTrue('Inputs continue entering account admission during the burst',
    FWallet.AccountRefreshCount > LBaseRefresh+1);
  AssertTrue('Idle account providers are not delayed until a quiet deadline',
    FModel.Metrics(mdkAccounts).FetchCount > LAccounts);
  AssertTrue('Idle operations providers are not delayed until a quiet deadline',
    FModel.Metrics(mdkOperations).FetchCount > LOperations);
  AssertTrue('Only one account provider can run at a time',
    FModel.Metrics(mdkAccounts).ActiveFetches <= 1);
  AssertTrue('Only one operations provider can run at a time',
    FModel.Metrics(mdkOperations).ActiveFetches <= 1);
  LLastInput := NotifyChange(1999);
  Settle;
  AssertTrue('The latest burst input does not wait for silence',GetTickCount64-LLastInput < 2500);
  CheckRenderedRevision(1999);
  CheckRefreshState('Latest continuous-input pages are ready',true);
  Log('Continuous node inputs admit immediately; busy requests coalesce and final rows publish without a quiet wait');
end;

procedure TWalletGridIntegrationTests.CheckSilentProcessingGap;
var
  LBaseRefresh,LAccounts,LOperations,LAccountAdmissionsBeforeEnd,LOperationAdmissionsBeforeEnd: Integer;
  LEndTick,LDeadline: QWord;
begin
  LBaseRefresh := FWallet.AccountRefreshCount;
  NotifyChange(2000);
  WaitForRefresh(LBaseRefresh+1,1800);
  Settle;
  CheckRefreshState('Loaded data before physical work',true);
  LAccounts := FModel.Metrics(mdkAccounts).FetchCount;
  LOperations := FModel.Metrics(mdkOperations).FetchCount;
  TUserInterface.Node.BeginBlockChainUpdate;
  try
    LDeadline := GetTickCount64+3000;
    repeat
      PumpFor(10);
      if FWallet.AccountsGrid.RefreshSuspended and FWallet.OperationsGrid.RefreshSuspended then Break;
    until GetTickCount64 >= LDeadline;
    AssertTrue('Actual physical work pauses both grids',
      FWallet.AccountsGrid.RefreshSuspended and FWallet.OperationsGrid.RefreshSuspended);
    NotifyChange(2999);
    PumpFor(5500);
    AssertTrue('Suspended inputs are still admitted immediately',FWallet.AccountRefreshCount > LBaseRefresh+1);
    AssertTrue('Physical processing remains active through notification silence',TUserInterface.Node.IsUpdatingBlockChain);
    CheckFetchCounts('A silent backlog cannot start a suspended provider',LAccounts,LOperations);
    CheckRenderedRevision(2000);
    CheckRefreshState('Only the node lifecycle holds synchronization through silence',true);
    LAccountAdmissionsBeforeEnd := FWallet.AccountRefreshCount;
    LOperationAdmissionsBeforeEnd := FWallet.OperationRefreshCount;
  finally
    LEndTick := GetTickCount64;
    TUserInterface.Node.EndBlockChainUpdate;
  end;
  LDeadline := GetTickCount64+1800;
  repeat
    PumpFor(10);
    if (FModel.Metrics(mdkAccounts).FetchCount=LAccounts+1) and
      (FModel.Metrics(mdkOperations).FetchCount=LOperations+1) then Break;
  until GetTickCount64>=LDeadline;
  CheckFetchCounts('Backlog end starts one latest page per grid',LAccounts+1,LOperations+1);
  AssertTrue('Backlog completion starts retained pages immediately',GetTickCount64-LEndTick < 1800);
  Settle;
  // EndBlockChainUpdate emits a real BlocksChanged event while pending state
  // is available. It is admitted before node resume and coalesces to one page.
  AssertEquals('The real completion event admits one account input',
    LAccountAdmissionsBeforeEnd+1,FWallet.AccountRefreshCount);
  AssertEquals('The real completion event admits one operations input',
    LOperationAdmissionsBeforeEnd+1,FWallet.OperationRefreshCount);
  CheckRenderedRevision(2999);
  CheckRefreshState('Actual processing end clears the node card',true);
  Log('Actual node backlog alone holds queries/cards; completion resumes one latest page without a timer');
end;

procedure TWalletGridIntegrationTests.CheckSlowFetchDuringContinuousInputs;
var
  LBaseRefresh,LAccounts,LOperations,LAccountAdmissions,LOperationAdmissions: Integer;
  LDeadline,LLastInput: QWord;
begin
  LBaseRefresh := FWallet.AccountRefreshCount;
  FModel.GateFetches := true;
  NotifyChange(3000);
  WaitForRefresh(LBaseRefresh+1,1800);
  LDeadline := GetTickCount64+2500;
  while ((FModel.Metrics(mdkAccounts).ActiveFetches <> 1) or
    (FModel.Metrics(mdkOperations).ActiveFetches <> 1)) and
    (GetTickCount64<LDeadline) do PumpFor(10);
  AssertEquals('The account provider is held',1,FModel.Metrics(mdkAccounts).ActiveFetches);
  AssertEquals('The operations provider is held',1,FModel.Metrics(mdkOperations).ActiveFetches);
  LAccounts := FModel.Metrics(mdkAccounts).FetchCount;
  LOperations := FModel.Metrics(mdkOperations).FetchCount;
  FProducer := TGridUpdateProducer.Create(TUserInterface.Node,FModel);
  FProducer.Start;
  PumpFor(700);
  TGridUpdateProducer(FProducer).Stop;
  AssertTrue('Repeated inputs reach the held actual grids',TGridUpdateProducer(FProducer).InputCount >= 4);
  FreeAndNil(FProducer);
  AssertTrue('A busy grid records inputs immediately',FWallet.AccountRefreshCount > LBaseRefresh+1);
  CheckFetchCounts('Held providers do not start parallel queries',LAccounts,LOperations);
  CheckRefreshState('Ordinary loading covers held providers');
  LAccountAdmissions := FWallet.AccountRefreshCount;
  LOperationAdmissions := FWallet.OperationRefreshCount;
  LLastInput := NotifyChange(3999);
  // Node delivery is asynchronous. Observe a new admission after the final
  // input; an earlier callback can share its tick and is not a delivery fence.
  // Keep both providers held until the final input has reached both grids.
  LDeadline := GetTickCount64+1800;
  while ((FWallet.AccountRefreshCount<=LAccountAdmissions) or
    (FWallet.OperationRefreshCount<=LOperationAdmissions)) and
    (GetTickCount64<LDeadline) do PumpFor(5);
  AssertTrue('The final input reaches account admission before releasing its provider',
    FWallet.AccountRefreshCount>LAccountAdmissions);
  AssertTrue('The final input reaches operations admission before releasing its provider',
    FWallet.OperationRefreshCount>LOperationAdmissions);
  CheckFetchCounts('The final input coalesces behind the held workers',LAccounts,LOperations);
  FModel.ReleaseFetches;
  Settle;
  AssertTrue('Retiring the worker does not add a quiet wait',GetTickCount64-LLastInput < 2500);
  CheckFetchCounts('Only one latest query follows each held provider',LAccounts+1,LOperations+1);
  CheckRenderedRevision(3999);
  CheckRefreshState('Latest held-provider pages clear ordinary loading',true);
  Log('Busy providers retain one latest request, run no parallel fetch and publish immediately after retirement');
end;

procedure TWalletGridIntegrationTests.CheckImmediateInputsAndIndependentGrids;
var
  LAccountAdmissions,LOperationAdmissions,LLatestPage: Integer;
  LInputTick: QWord;
begin
  NotifyChange(4500);
  Settle;
  LAccountAdmissions := FWallet.AccountRefreshCount;
  LOperationAdmissions := FWallet.OperationRefreshCount;
  FModel.GateFetches := true;
  // Exercise real page/filter/resize entry points while the first provider is
  // held. Visual geometry changes immediately; only the latest query is kept.
  LInputTick := GetTickCount64;
  FWallet.AccountsGrid.PageIndex := 1;
  TGridInputAccess(FWallet.AccountsGrid).Search('mock%');
  FMain.SetBounds(FMain.Left,FMain.Top,FMain.Width+24,FMain.Height+48);
  LLatestPage := FWallet.AccountsGrid.PageIndex;
  PumpFor(250);
  AssertTrue('Account page/filter/resize inputs enter admission immediately',
    FWallet.AccountRefreshCount > LAccountAdmissions);
  AssertTrue('A resized operations viewport enters admission immediately',
    FWallet.OperationRefreshCount > LOperationAdmissions);
  AssertTrue('Account client changes cannot create parallel providers',
    FModel.Metrics(mdkAccounts).ActiveFetches <= 1);
  AssertTrue('Operations resize cannot create parallel providers',
    FModel.Metrics(mdkOperations).ActiveFetches <= 1);
  CheckRefreshState('Mixed actual client inputs have no quiet wait');
  FModel.ReleaseFetches;
  Settle;
  AssertTrue('Latest client parameters do not wait for silence',GetTickCount64-LInputTick < 2500);
  AssertEquals('The latest requested account page is retained',LLatestPage,FWallet.AccountsGrid.PageIndex);
  CheckRenderedRevision(4500);
  LAccountAdmissions := FWallet.AccountRefreshCount;
  LOperationAdmissions := FWallet.OperationRefreshCount;
  FWallet.OperationsHistory := woh30Days;
  AssertFalse('A history-only request cannot show a node synchronization card',FWallet.OperationsGrid.LoadingVisible);
  Settle;
  AssertEquals('Independent history input does not admit account work',LAccountAdmissions,FWallet.AccountRefreshCount);
  AssertEquals('The independent operations input is delivered once',LOperationAdmissions+1,FWallet.OperationRefreshCount);
  AssertEquals('The provider receives the selected thirty-day depth',
    Integer(TTimeSpan.FromDays(30).TotalBlockCount),Integer(FWallet.OperationsSource.LastFetchBlockDepth));
  CheckRefreshState('Independent history input completes without a timer',true);
  Log('Actual page/filter/resize inputs and independent history-depth changes passed');
end;

procedure TWalletGridIntegrationTests.CheckHiddenAccountsRequests;
var
  LReturnedTick: QWord;
  LAccountSource: IDataSource;
begin
  LAccountSource := FWallet.AccountsGrid.DataSource;
  FWallet.AccountsMode := wamFirstAccount;
  AssertFalse('The get-account view detaches the actual account grid',Assigned(FWallet.AccountsGrid.Parent));
  NotifyChange(4600);
  PumpFor(250);
  AssertTrue('Hidden input retains the same source',LAccountSource=FWallet.AccountsGrid.DataSource);
  AssertFalse('A hidden ordinary refresh cannot show a node card',FWallet.AccountsGrid.LoadingVisible);
  AssertFalse('A hidden ordinary refresh has no quiet-time wait',FWallet.AccountsGrid.RefreshThrottle.Waiting);
  LReturnedTick := GetTickCount64;
  FWallet.AccountsMode := wamMyAccounts;
  AssertTrue('Returning reparents the same account grid',Assigned(FWallet.AccountsGrid.Parent));
  Settle;
  AssertTrue('Returning publishes without a quiet-time delay',GetTickCount64-LReturnedTick < 2500);
  CheckRenderedRevision(4600);
  CheckRefreshState('Hidden input publishes the latest rows after reparenting',true);
  Log('Actual get-account view, hidden node input and immediate returning view passed');
end;

procedure TWalletGridIntegrationTests.MainGridsRefreshImmediatelyAndPauseForNodeWork;
begin
  CheckProductionWiringAndCachedPaging;
  CheckEventDrivenOverview;
  CheckSingleInputLoading;
  CheckContinuousInputs;
  CheckSilentProcessingGap;
  CheckSlowFetchDuringContinuousInputs;
  CheckImmediateInputsAndIndependentGrids;
  CheckHiddenAccountsRequests;
end;

initialization
  RegisterTest(TWalletGridIntegrationTests);

end.
