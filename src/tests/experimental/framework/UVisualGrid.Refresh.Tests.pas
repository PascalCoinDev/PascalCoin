unit UVisualGrid.Refresh.Tests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE.
  Generated rows exercise native layout and worker callbacks without product dependencies. }

{$mode delphi}

interface

uses Classes, SysUtils, Types, Forms, Controls, StdCtrls, ExtCtrls, Graphics, SyncObjs, Variants, Generics.Collections,
  fpcunit, testregistry, UCommon, UCommon.UI, UCommon.Data, UVisualGrid,
  UVisualGrid.Viewport.Checks, UVisualGrid.Paging.Checks, UVisualGrid.Worker.Checks,
  UVisualGrid.Loading.Checks, UVisualGrid.Selection.Checks;

type
  TVisualGridSelectionEventTests = class(TTestCase)
  published
    procedure OneNativeClickPublishesOneSelectedEvent;
    procedure RepeatedNativeClickPreservesToggleDeselection;
    procedure ConsumerClearPublishesOneActualEmptyFollowUp;
    procedure ProgrammaticAndNoHandlerSelectionRemainCoherent;
  end;

  TVisualGridLoadingLifecycleTests = class(TTestCase)
  published
    procedure InternalCapacityRefitDoesNotRetainUpdating;
    procedure GenuineQueuedClientRetainsItsLoadingCard;
    procedure ManualImageSurvivesInternalPublication;
    procedure ManualAndErrorLayersRemainIndependent;
    procedure SynchronousFailureRetainsRowsAndCanRetry;
  end;

  TVisualGridPagingTests = class(TTestCase)
  published
    procedure LegacyShowAllDoesNotDependOnCachedTotal;
    procedure SynchronousSourceCanBeAssignedBeforeParenting;
    procedure SingleLoadsTheWholeUnknownAndGrowingResult;
    procedure DropdownModesRememberManualSizeAndFitAutomaticRows;
    procedure LargeTotalsCompactBeforeWrapping;
    procedure SingleSearchAndSortUseTheWholeResult;
    procedure SelectorRemainsReachableForEmptyAndOnePageResults;
    procedure SuspendedModesResumeOnlyTheLatestIntent;
    procedure SwitchingModesRejectsAnOldFiniteWorker;
    procedure DisablingPagingQueriesTheWholeResultInEveryMode;
    procedure EveryModeChangeRestoresNativeScrollOrigin;
    procedure SuspendedModeChangesRestoreScrollAfterPublication;
    procedure HeldWorkersCannotRestoreAnOldModeScrollPosition;
  end;

  TVisualGridViewportTests = class(TTestCase)
  published
    procedure UniformRowsFitAtNativePixelBoundaries;
    procedure HeaderAndVariableRowsUseTheirNativeHeights;
    procedure DpiScaledRowsFitAtNativePixelBoundaries;
    procedure HorizontalScrollbarReducesTheNativeViewport;
    procedure UnfetchedRowsStillContributePageCapacity;
    procedure AutomaticPageSizeTracksNativeResizing;
  end;

  { Reused by product tests which add their own notification source. }
  TVisualGridRefreshFixture = class(TTestCase)
  protected
    FForm: TForm;
    FGrid: TVisualGrid;
    FSource: TComponent;
    FCompletions, FSelections: Integer;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure PumpFor(AMilliseconds: Cardinal);
    procedure Settle;
    procedure Completed(Sender: TObject);
    procedure Selected(Sender: TObject; constref ASelection: TVisualGridSelection);
    function FetchCount: Integer;
  end;

  TVisualGridRefreshTests = class(TVisualGridRefreshFixture)
  published
    procedure SamePageRepopulationDoesNotRaiseSelectionRefresh;
    procedure QueuedRefreshesCoalesceOnceThenStayIdle;
    procedure AutoPageLayoutTransitionsSettleWithoutRepeatedFetching;
    procedure NarrowAndWideColumnsSettleAfterHeldResize;
    procedure CompleteResultsReuseRowsWhenPageCapacityChanges;
    procedure SmallResultAutoHideCompletesExactlyOnce;
    procedure NormalizedUnchangedPageSizeDoesNotFetch;
    procedure TinyClientAreaUsesOneRowWithoutRepeatedFetching;
    procedure ManualPageSizeSurvivesHiddenSearchPanel;
    procedure ManualLoadingCopiesArtworkAndDoesNotFetch;
    procedure ManualLoadingBlocksInputButAllowsProgrammaticRefresh;
    procedure ManualLoadingSurvivesFetchCompletionAndError;
    procedure HidingManualLoadingRetainsActiveFetchRestrictions;
    procedure ManualLoadingPreservesCallerDisabledGrid;
    procedure NonblockingManualLoadingRespectsFetchRestrictions;
    procedure ManualLoadingCanBeRequestedBeforeParenting;
  end;

  TVisualGridQueuedCompletionTests = class(TVisualGridRefreshFixture)
  private
    FRequestNextPage: Boolean;
    procedure DisableTimers(AOwner: TComponent);
    procedure PumpCompletionOnly(AMilliseconds: Cardinal);
    procedure AwaitCompletionOnly;
    procedure CompleteAndRequestNext(Sender: TObject);
  published
    {$IF defined(MSWINDOWS) and defined(VISUALGRID_TEST_WORKER_START)}
    procedure WorkerCancelledBeforeExecutionRetiresAndPreservesOtherReceivers;
    procedure PublicCancellationBeforeStartKeepsTheLatestRequest;
    {$ENDIF}
    procedure CancelActiveFetchNeverJoinsOrPublishesAHeldQuery;
    procedure CancelledFetchRetainsTheLatestFiveSecondAdmission;
    procedure CancelledProviderExceptionCannotPublishAnErrorCard;
    procedure FinishedWorkerPublishesWithAllTimersDisabled;
    procedure FailedWorkerPublishesAndRetriesWithoutTimers;
    procedure DisposingQueuedReceiverPreservesOtherGridCompletion;
    procedure QueuedCompletionAcceptsANewClientRequest;
    procedure SourceReplacementDoesNotJoinANewHeldWorkerFromAStaleWake;
  end;

  TVisualGridRequestThrottleTests = class(TVisualGridRefreshFixture)
  private
    FWaitStarts, FWaitEnds: Integer;
    FRequestOnCompletion: Boolean;
    procedure WaitingChanged(Sender: TObject);
    procedure RequestCompleted(Sender: TObject);
    procedure EnableThrottle(AInterval: Integer = 200);
    procedure AwaitQuiet;
    function PageCalls: Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ContinuousClientRequestsAdmitOnlyLeadingAndFinalPages;
    procedure SingleBusyLeadingRequestWaitsForItsOwnQuietFinish;
    procedure BusyInputsRetainTheLatestPageParameters;
    procedure PageSearchAndSortRequestsShareOneAdmission;
    procedure SourceReplacementUsesOnlyTheLatestAdmittedSource;
    procedure ResizeDuringWaitingIsVisualAndRetainsLatestCapacity;
    procedure CompletedResultLayoutCannotStartAnotherBurst;
    procedure FinishedUpdatingClientRequestIsNewInput;
    procedure ANewBurstAfterQuietLeadsImmediately;
    procedure CancelCannotReleaseABusyWaitingRequest;
    procedure ModeChangeCancelsPendingPageWork;
    procedure OwnerDestructionCannotReleaseCancelledWork;
    procedure PartialResultLayoutRetainsLatestAdmittedCapacity;
    procedure ClearingSourcePublishesEmptyRowsWithoutLayoutReplay;
    procedure SuspendedInputsRetainLatestParametersUntilImmediateResume;
    procedure SuspendedResizeChangesGeometryWithoutFetching;
    procedure SuspensionCancelsWorkerWithoutJoiningOrPublishingError;
    procedure ResumeBeforeCancelledWorkerRetiresFetchesOnlyLatestPage;
    procedure ResuspensionCannotReviveACancelledWorkerResult;
    procedure SuspendedQuietCompletionCannotAdmitAQuery;
    procedure SuspendedSourceReplacementUsesOnlyLatestSource;
    procedure SuspensionCooperativelyReleasesTheProviderBeforeResume;
    procedure QueuedRequestShowsLoadingBeforeFetchAdmission;
    procedure CancelledQueuedRequestHidesLoading;
    procedure ManualLoadingOverridesQueuedRefreshPresentation;
    procedure DeferredSingleInputRetainsLoadingThroughQuietAdmission;
  end;

implementation

procedure TVisualGridSelectionEventTests.OneNativeClickPublishesOneSelectedEvent;
begin
  CheckVisualGridSelection(ssNativeClickEvents);
end;

procedure TVisualGridSelectionEventTests.RepeatedNativeClickPreservesToggleDeselection;
begin
  CheckVisualGridSelection(ssRepeatedClickToggle);
end;

procedure TVisualGridSelectionEventTests.ConsumerClearPublishesOneActualEmptyFollowUp;
begin
  CheckVisualGridSelection(ssConsumerClearSelection);
end;

procedure TVisualGridSelectionEventTests.ProgrammaticAndNoHandlerSelectionRemainCoherent;
begin
  CheckVisualGridSelection(ssProgrammaticAndNoHandler);
end;

procedure TVisualGridLoadingLifecycleTests.InternalCapacityRefitDoesNotRetainUpdating;
begin
  CheckVisualGridLoading(lsInternalLayoutFinishesLoading);
end;

procedure TVisualGridLoadingLifecycleTests.GenuineQueuedClientRetainsItsLoadingCard;
begin
  CheckVisualGridLoading(lsNewClientKeepsLoading);
end;

procedure TVisualGridLoadingLifecycleTests.ManualImageSurvivesInternalPublication;
begin
  CheckVisualGridLoading(lsManualCardSurvivesInternalLayout);
end;

procedure TVisualGridLoadingLifecycleTests.ManualAndErrorLayersRemainIndependent;
begin
  CheckVisualGridLoading(lsErrorAndManualLayers);
end;

procedure TVisualGridLoadingLifecycleTests.SynchronousFailureRetainsRowsAndCanRetry;
begin
  CheckVisualGridLoading(lsSynchronousErrorAndRecovery);
end;

procedure TVisualGridPagingTests.LegacyShowAllDoesNotDependOnCachedTotal;
begin
  CheckVisualGridShowAllCompatibility;
end;

procedure TVisualGridPagingTests.SynchronousSourceCanBeAssignedBeforeParenting;
begin
  CheckVisualGridDeferredParenting;
end;

procedure TVisualGridPagingTests.SingleLoadsTheWholeUnknownAndGrowingResult;
begin
  CheckVisualGridPaging(psSingleFromUnknownTotal);
end;

procedure TVisualGridPagingTests.DropdownModesRememberManualSizeAndFitAutomaticRows;
begin
  CheckVisualGridPaging(psModeTransitions);
end;

procedure TVisualGridPagingTests.LargeTotalsCompactBeforeWrapping;
begin
  CheckVisualGridFooterCompaction;
end;

procedure TVisualGridPagingTests.SingleSearchAndSortUseTheWholeResult;
begin
  CheckVisualGridPaging(psCompleteSearchAndSort);
end;

procedure TVisualGridPagingTests.SelectorRemainsReachableForEmptyAndOnePageResults;
begin
  CheckVisualGridPaging(psEmptyAndOnePage);
end;

procedure TVisualGridPagingTests.SuspendedModesResumeOnlyTheLatestIntent;
begin
  CheckVisualGridPaging(psSuspendedLatestMode);
end;

procedure TVisualGridPagingTests.SwitchingModesRejectsAnOldFiniteWorker;
begin
  CheckVisualGridPaging(psDiscardOldWorker);
end;

procedure TVisualGridPagingTests.EveryModeChangeRestoresNativeScrollOrigin;
begin
  CheckVisualGridPaging(psModeChangeScrollOrigin);
end;

procedure TVisualGridPagingTests.SuspendedModeChangesRestoreScrollAfterPublication;
begin
  CheckVisualGridPaging(psSuspendedModeScrollOrigin);
end;

procedure TVisualGridPagingTests.HeldWorkersCannotRestoreAnOldModeScrollPosition;
begin
  CheckVisualGridPaging(psHeldModeScrollOrigin);
end;

procedure TVisualGridPagingTests.DisablingPagingQueriesTheWholeResultInEveryMode;
begin
  CheckVisualGridPaging(psDisabledPaging);
end;

procedure TVisualGridViewportTests.UniformRowsFitAtNativePixelBoundaries;
begin
  CheckVisualGridViewport(vsUniformRows);
end;

procedure TVisualGridViewportTests.HeaderAndVariableRowsUseTheirNativeHeights;
begin
  CheckVisualGridViewport(vsVariableRows);
end;

procedure TVisualGridViewportTests.DpiScaledRowsFitAtNativePixelBoundaries;
begin
  CheckVisualGridViewport(vsDpiScaling);
end;

procedure TVisualGridViewportTests.HorizontalScrollbarReducesTheNativeViewport;
begin
  CheckVisualGridViewport(vsHorizontalScrollbar);
end;

procedure TVisualGridViewportTests.UnfetchedRowsStillContributePageCapacity;
begin
  CheckVisualGridViewport(vsUnfetchedRows);
end;

procedure TVisualGridViewportTests.AutomaticPageSizeTracksNativeResizing;
begin
  CheckVisualGridViewport(vsAutomaticResize);
end;

type
  TGridRefreshSource = class(TCustomDataSource<Integer>)
  public
    FetchCount, ItemCount, PageCallCount: LongInt;
    LastParams: TPageFetchParams;
    Entered, ReleaseRead, Returned, CancelObserved: TEvent;
    BlockRead, FailRead, CooperativeRead: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColumns: TDataColumns; override;
    function GetItemField(constref AItem: Integer; const AName: String): Variant; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
    function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult; override;
  end;

  TRequestGridAccess = class(TVisualGrid)
  public
    procedure Search(const AText: String);
    function PublishedTotalCount: Integer;
  end;

  TLoadingGridAccess = class(TVisualGrid)
    function ManualRegionsEnabled: Boolean;
    function FetchFieldsEnabled: Boolean;
  end;

function TRequestGridAccess.PublishedTotalCount: Integer;
begin
  Result := FTotalDataCount;
end;

function TLoadingGridAccess.ManualRegionsEnabled: Boolean;
begin
  Result := FTopPanel.Enabled and FTopPanelMultiSearch.Enabled and FBottomPanel.Enabled;
end;

function TLoadingGridAccess.FetchFieldsEnabled: Boolean;
begin
  Result := FDrawGrid.Enabled and FMultiSearchCheckComboBox.Enabled and
    FBottomRightPanel.Enabled and not FSearchEdit.ReadOnly and
    not FPageIndexEdit.ReadOnly and not FPageSizeEdit.ReadOnly;
end;

constructor TGridRefreshSource.Create(AOwner: TComponent);
begin
  inherited;
  ItemCount := 1000;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, False, '');
  Returned := TEvent.Create(nil, True, False, '');
  CancelObserved := TEvent.Create(nil, True, False, '');
end;

destructor TGridRefreshSource.Destroy;
begin
  Entered.Free;
  ReleaseRead.Free;
  Returned.Free;
  CancelObserved.Free;
  inherited;
end;

function TGridRefreshSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Id', True),
    TDataColumn.From('Name'), TDataColumn.From('Balance'),
    TDataColumn.From('Block'), TDataColumn.From('Type'), TDataColumn.From('Payload'));
end;

function TGridRefreshSource.GetItemField(constref AItem: Integer; const AName: String): Variant;
begin
  Result := AItem;
end;

procedure TGridRefreshSource.FetchAll(const AContainer: TList<Integer>);
var I, LCount: Integer;
begin
  InterlockedIncrement(FetchCount);
  Entered.SetEvent;
  if BlockRead then begin
    if CooperativeRead then begin
      while ReleaseRead.WaitFor(10) <> wrSignaled do
        if TThread.CheckTerminated then begin
          CancelObserved.SetEvent;
          Abort;
        end;
    end else if ReleaseRead.WaitFor(5000) <> wrSignaled then
      raise Exception.Create('Grid refresh fixture release timed out');
  end;
  if FailRead then raise Exception.Create('Controlled data-source failure');
  LCount := InterlockedCompareExchange(ItemCount, 0, 0);
  for I := 0 to LCount - 1 do AContainer.Add(I);
end;

function TGridRefreshSource.FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
begin
  LastParams := AParams;
  InterlockedIncrement(PageCallCount);
  try Result := inherited;
  finally Returned.SetEvent; end;
end;

procedure TRequestGridAccess.Search(const AText: String);
begin
  FSearchEdit.Text := AText;
  SearchButtonClick(FSearchButton);
end;

procedure TVisualGridRefreshFixture.SetUp;
begin
  FForm := TForm.CreateNew(nil);
  FForm.SetBounds(80, 90, 640, 440);
  FSource := TGridRefreshSource.Create(FForm);
  FGrid := TRequestGridAccess.Create(FForm);
  FGrid.Parent := FForm;
  FGrid.Align := alClient;
  FGrid.FetchDataInThread := True;
  FGrid.AutoPageSize := True;
  FGrid.Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone,
    vgoAutoHidePaging, vgoAutoHideSearchPanel];
  FGrid.SelectionType := stMultiRow;
  FGrid.DeselectionType := dtDefault;
  FGrid.AddColumn('Id').Binding := 'Id';
  FGrid.OnFinishedUpdating := Completed;
  FGrid.OnSelection := Selected;
  FForm.Show;
  FGrid.DataSource := TGridRefreshSource(FSource);
  Settle;
end;

procedure TVisualGridRefreshFixture.TearDown;
begin
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  if Assigned(FGrid) then FGrid.DataSource := nil;
  FForm.Free;
end;

procedure TVisualGridRefreshFixture.PumpFor(AMilliseconds: Cardinal);
var LUntil: QWord;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure TVisualGridRefreshFixture.Settle;
var LUntil, LStableSince: QWord; LFetches: Integer;
begin
  LUntil := GetTickCount64 + 5000;
  LStableSince := GetTickCount64;
  LFetches := InterlockedCompareExchange(TGridRefreshSource(FSource).FetchCount, 0, 0);
  repeat
    PumpFor(10);
    if FGrid.IsUpdating or
      (InterlockedCompareExchange(TGridRefreshSource(FSource).FetchCount, 0, 0) <> LFetches) then begin
      LStableSince := GetTickCount64;
      LFetches := InterlockedCompareExchange(TGridRefreshSource(FSource).FetchCount, 0, 0);
    end;
    if (not FGrid.IsUpdating) and (GetTickCount64 - LStableSince >= 350) then Exit;
  until GetTickCount64 >= LUntil;
  Fail('Auto-page grid did not settle; fetches=' + IntToStr(TGridRefreshSource(FSource).FetchCount));
end;

procedure TVisualGridRefreshFixture.Completed(Sender: TObject);
begin
  Inc(FCompletions);
end;

procedure TVisualGridRefreshFixture.Selected(Sender: TObject; constref ASelection: TVisualGridSelection);
begin
  Inc(FSelections);
end;

function TVisualGridRefreshFixture.FetchCount: Integer;
begin
  Result := InterlockedCompareExchange(TGridRefreshSource(FSource).FetchCount, 0, 0);
end;

procedure TVisualGridRefreshTests.SamePageRepopulationDoesNotRaiseSelectionRefresh;
var LSelections: Integer;
begin
  // First population deliberately suppresses selection. Re-enable a real row
  // mode after that population, as a user interaction would do.
  FGrid.SelectionType := stRow;
  FGrid.InternalDrawGrid.Row := 2;
  PumpFor(20);
  AssertTrue('The real grid must have a selected data row', Length(FGrid.SelectedRows) > 0);
  LSelections := FSelections;
  FGrid.RefreshGrid;
  Settle;
  AssertEquals('Repopulating a page must not masquerade as another user selection',
    LSelections, FSelections);
end;

procedure TVisualGridRefreshTests.QueuedRefreshesCoalesceOnceThenStayIdle;
var I, LFetches: Integer;
begin
  LFetches := TGridRefreshSource(FSource).FetchCount;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  FGrid.RefreshGrid;
  AssertTrue(TGridRefreshSource(FSource).Entered.WaitFor(2000) = wrSignaled);
  for I := 1 to 50 do FGrid.RefreshGrid;
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  Settle;
  AssertEquals('Many explicit requests during one fetch must retain only one follow-up',
    LFetches + 2, TGridRefreshSource(FSource).FetchCount);
  LFetches := TGridRefreshSource(FSource).FetchCount;
  PumpFor(400);
  AssertEquals('A drained pending request must not turn into periodic fetching', LFetches, TGridRefreshSource(FSource).FetchCount);
end;

procedure TVisualGridRefreshTests.AutoPageLayoutTransitionsSettleWithoutRepeatedFetching;
const LCounts: array[0..4] of Integer = (0, 1, 17, 28, 1000);
var I, LFetches: Integer;
begin
  for I := Low(LCounts) to High(LCounts) do begin
    InterlockedExchange(TGridRefreshSource(FSource).ItemCount, LCounts[I]);
    LFetches := TGridRefreshSource(FSource).FetchCount;
    FGrid.RefreshGrid;
    Settle;
    AssertTrue('A layout transition may resize its page but must reach a stable layout',
      TGridRefreshSource(FSource).FetchCount - LFetches <= 4);
    LFetches := TGridRefreshSource(FSource).FetchCount;
    PumpFor(250);
    AssertEquals('Auto-hidden paging/search panels must not sustain a fetch loop',
      LFetches, TGridRefreshSource(FSource).FetchCount);
  end;
end;

procedure TVisualGridRefreshTests.NarrowAndWideColumnsSettleAfterHeldResize;
var I, LFetches, LPageSize: Integer;
begin
  FGrid.Columns[0].Width := 100;
  FGrid.AddColumn('Name').Binding := 'Name';
  FGrid.Columns[1].Width := 120;
  FGrid.AddColumn('Balance').Binding := 'Balance';
  FGrid.Columns[2].Width := 100;
  FForm.SetBounds(80, 90, 340, 480);
  Settle;
  for I := 0 to 1 do begin
    if I = 1 then begin
      FGrid.AddColumn('Block').Binding := 'Block';
      FGrid.AddColumn('Type').Binding := 'Type';
      FGrid.AddColumn('Payload').Binding := 'Payload';
      FGrid.Columns[3].Width := 150;
      FGrid.Columns[4].Width := 150;
      FGrid.Columns[5].Width := 300;
      Settle;
    end;
    TGridRefreshSource(FSource).BlockRead := True;
    TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
    TGridRefreshSource(FSource).Entered.ResetEvent;
    FGrid.RefreshGrid;
    AssertTrue(TGridRefreshSource(FSource).Entered.WaitFor(2000) = wrSignaled);
    FForm.Height := 490;
    PumpFor(60);
    FForm.Height := 470;
    PumpFor(60);
    FForm.Height := 480;
    PumpFor(60);
    TGridRefreshSource(FSource).ReleaseRead.SetEvent;
    Settle;
    LFetches := TGridRefreshSource(FSource).FetchCount;
    LPageSize := FGrid.PageSize;
    PumpFor(700);
    AssertEquals('Scrollbar changes must not alternate page-size fetches after a held resize',
      LFetches, TGridRefreshSource(FSource).FetchCount);
    AssertEquals('The final auto page size must remain stable', LPageSize, FGrid.PageSize);
  end;
end;

procedure TVisualGridRefreshTests.CompleteResultsReuseRowsWhenPageCapacityChanges;
var I, LFetches, LCompleted: Integer;
begin
  FGrid.AutoPageSize := False;
  Settle;
  for I := 0 to 1 do begin
    InterlockedExchange(TGridRefreshSource(FSource).ItemCount, I);
    FGrid.RefreshGrid;
    Settle;
    AssertEquals('The complete result is already present', I, FGrid.RowCount);
    LFetches := FetchCount;
    LCompleted := FCompletions;
    FGrid.PageSize := FGrid.PageSize + 5;
    Settle;
    AssertEquals('Capacity growth must reuse a complete empty or small result',
      LFetches, FetchCount);
    AssertEquals('A layout-only capacity change must not signal fetch completion',
      LCompleted, FCompletions);
    AssertEquals(I, FGrid.RowCount);
  end;
  InterlockedExchange(TGridRefreshSource(FSource).ItemCount, 1000);
  FGrid.PageSize := 10;
  Settle;
  FGrid.RefreshGrid;
  Settle;
  LFetches := FetchCount;
  FGrid.PageSize := 11;
  Settle;
  AssertEquals('A partially populated page still fetches its newly exposed row',
    LFetches + 1, FetchCount);
  AssertEquals(11, FGrid.RowCount);
end;

procedure TVisualGridRefreshTests.SmallResultAutoHideCompletesExactlyOnce;
var LFetches, LCompleted: Integer;
begin
  LFetches := FetchCount;
  LCompleted := FCompletions;
  InterlockedExchange(TGridRefreshSource(FSource).ItemCount, 0);
  FGrid.RefreshGrid;
  Settle;
  AssertEquals('Hiding paging/search panels must not refetch an empty completed result',
    LFetches + 1, FetchCount);
  AssertEquals('One completed fetch signals exactly once after layout settles',
    LCompleted + 1, FCompletions);
  AssertEquals(0, FGrid.RowCount);
end;

procedure TVisualGridRefreshTests.NormalizedUnchangedPageSizeDoesNotFetch;
var LFetches, LPageSize, LCompleted: Integer;
begin
  LFetches := FetchCount;
  LPageSize := FGrid.PageSize;
  LCompleted := FCompletions;
  FGrid.PageSize := -1;
  FGrid.PageSize := 0;
  Settle;
  AssertEquals('Invalid sizes normalize to the existing page without another query',
    LFetches, FetchCount);
  AssertEquals(LPageSize, FGrid.PageSize);
  AssertEquals(LCompleted, FCompletions);
end;

procedure TVisualGridRefreshTests.TinyClientAreaUsesOneRowWithoutRepeatedFetching;
var LFetches, LSmallFetches, LSmallPageSize: Integer;
begin
  FGrid.CanSearch := False;
  Settle;
  LFetches := FetchCount;
  try
    // A hidden/partially laid-out host can temporarily expose no drawable rows.
    FGrid.Align := alNone;
    FGrid.SetBounds(0, 0, FForm.ClientWidth, 1);
    PumpFor(900);
    LSmallFetches := FetchCount - LFetches;
    LSmallPageSize := FGrid.PageSize;
  finally
    FGrid.Align := alClient;
    FGrid.CanSearch := True;
    Settle;
  end;
  AssertEquals('A tiny viewport retains one valid data-row capacity', 1, LSmallPageSize);
  AssertTrue('A tiny viewport must not sustain queries for the same effective size',
    LSmallFetches <= 2);
end;

procedure TVisualGridRefreshTests.ManualPageSizeSurvivesHiddenSearchPanel;
var LFetches: Integer;
begin
  FGrid.AutoPageSize := False;
  FGrid.PageSize := 7;
  Settle;
  FGrid.CanSearch := False;
  Settle;
  LFetches := FetchCount;
  FGrid.RefreshGrid;
  Settle;
  AssertEquals('A hidden search panel must not be mistaken for changed paging visibility',
    7, FGrid.PageSize);
  AssertEquals('An unchanged manual page requests only its explicit refresh',
    LFetches + 1, FetchCount);
end;

procedure TVisualGridRefreshTests.ManualLoadingCopiesArtworkAndDoesNotFetch;
var LBitmap: TBitmap; LImage: TImage; LFetches, LCompletions: Integer;
begin
  LFetches := FetchCount;
  LCompletions := FCompletions;
  LBitmap := TBitmap.Create;
  try
    LBitmap.SetSize(8, 8);
    LBitmap.Canvas.Brush.Color := clRed;
    LBitmap.Canvas.FillRect(Rect(0, 0, 8, 8));
    FGrid.ShowLoading('Held title', 'A separate task is running', LBitmap);
    LBitmap.Canvas.Brush.Color := clBlue;
    LBitmap.Canvas.FillRect(Rect(0, 0, 8, 8));
  finally
    LBitmap.Free;
  end;
  PumpFor(300);
  AssertTrue(FGrid.LoadingVisible);
  AssertFalse('A manual hold is not a fetch worker', FGrid.IsUpdating);
  AssertEquals('Held title', TLabel(FGrid.FindComponent('LoadingStatus')).Caption);
  AssertEquals('A separate task is running', TLabel(FGrid.FindComponent('LoadingSubtitle')).Caption);
  AssertTrue(fsBold in TLabel(FGrid.FindComponent('LoadingStatus')).Font.Style);
  AssertFalse(fsBold in TLabel(FGrid.FindComponent('LoadingSubtitle')).Font.Style);
  LImage := TImage(FGrid.FindComponent('LoadingImage'));
  AssertTrue(LImage.Visible);
  AssertEquals(8, LImage.Picture.Width);
  AssertEquals('The caller can mutate/free its original graphic',
    ColorToRGB(clRed), ColorToRGB(LImage.Picture.Bitmap.Canvas.Pixels[1, 1]));
  AssertFalse(TLoadingCircle(FGrid.FindComponent('LoadingCircle')).Visible);
  FGrid.ShowLoading('Replacement title');
  AssertEquals('Replacement title', TLabel(FGrid.FindComponent('LoadingStatus')).Caption);
  AssertFalse(LImage.Visible);
  AssertTrue(TLoadingCircle(FGrid.FindComponent('LoadingCircle')).Active);
  AssertFalse(TLabel(FGrid.FindComponent('LoadingSubtitle')).Visible);
  FGrid.HideLoading;
  FGrid.HideLoading;
  AssertFalse(FGrid.LoadingVisible);
  AssertFalse(TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
  AssertEquals('Presentation must not query the source', LFetches, FetchCount);
  AssertEquals(LCompletions, FCompletions);
end;

procedure TVisualGridRefreshTests.ManualLoadingBlocksInputButAllowsProgrammaticRefresh;
var LFetches: Integer;
begin
  LFetches := FetchCount;
  FGrid.ShowLoading('Waiting', 'Inputs are unavailable');
  AssertTrue('The grid root stays enabled', FGrid.Enabled);
  AssertFalse(TLoadingGridAccess(FGrid).ManualRegionsEnabled);
  AssertFalse(TLoadingGridAccess(FGrid).FetchFieldsEnabled);
  AssertFalse(FGrid.InternalDrawGrid.Enabled);
  AssertEquals(LFetches, FetchCount);
  FGrid.RefreshGrid;
  Settle;
  AssertEquals('An explicit programmatic refresh remains possible', LFetches + 1, FetchCount);
  AssertTrue('Completion does not release the caller hold', FGrid.LoadingVisible);
  AssertFalse(TLoadingGridAccess(FGrid).ManualRegionsEnabled);
  AssertFalse(FGrid.InternalDrawGrid.Enabled);
  FGrid.HideLoading;
  AssertTrue(TLoadingGridAccess(FGrid).ManualRegionsEnabled);
  AssertTrue(TLoadingGridAccess(FGrid).FetchFieldsEnabled);
end;

procedure TVisualGridRefreshTests.ManualLoadingSurvivesFetchCompletionAndError;
begin
  FGrid.ShowLoading('Independent operation', 'Keep this message');
  FGrid.RefreshGrid;
  Settle;
  AssertTrue(FGrid.LoadingVisible);
  AssertEquals('Independent operation', TLabel(FGrid.FindComponent('LoadingStatus')).Caption);
  TGridRefreshSource(FSource).FailRead := True;
  FGrid.RefreshGrid;
  Settle;
  AssertTrue('An automatic fetch error must not clear manual state', FGrid.LoadingVisible);
  AssertEquals('Keep this message', TLabel(FGrid.FindComponent('LoadingSubtitle')).Caption);
  FGrid.HideLoading;
  AssertFalse(FGrid.LoadingVisible);
  AssertTrue('Releasing the hold exposes the actual fetch error', TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
  AssertEquals('Controlled data-source failure', TLabel(FGrid.FindComponent('LoadingStatus')).Caption);
  AssertEquals(ColorToRGB(clRed), ColorToRGB(TLabel(FGrid.FindComponent('LoadingStatus')).Font.Color));
  TGridRefreshSource(FSource).FailRead := False;
  FGrid.RefreshGrid;
  Settle;
  AssertFalse(TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
end;

procedure TVisualGridRefreshTests.HidingManualLoadingRetainsActiveFetchRestrictions;
begin
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  FGrid.ShowLoading('Manual title');
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled), Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  FGrid.HideLoading;
  AssertFalse(FGrid.LoadingVisible);
  AssertTrue('The real worker remains active', FGrid.IsUpdating);
  AssertFalse('Hide cannot enable an active fetch', FGrid.InternalDrawGrid.Enabled);
  PumpFor(350);
  AssertTrue('The delayed automatic card still appears', TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
  AssertTrue(TLoadingCircle(FGrid.FindComponent('LoadingCircle')).Active);
  AssertTrue(TLabel(FGrid.FindComponent('LoadingStatus')).Caption <> 'Manual title');
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  Settle;
  AssertFalse(TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
  AssertTrue(TLoadingGridAccess(FGrid).FetchFieldsEnabled);
end;

procedure TVisualGridRefreshTests.ManualLoadingPreservesCallerDisabledGrid;
var LFetches: Integer;
begin
  LFetches := FetchCount;
  FGrid.Enabled := False;
  FGrid.ShowLoading('Caller disabled');
  AssertFalse(FGrid.Enabled);
  FGrid.HideLoading;
  AssertFalse('Hide does not enable a caller-disabled root', FGrid.Enabled);
  FGrid.ShowLoading('Nonblocking', '', nil, False);
  FGrid.HideLoading;
  AssertFalse(FGrid.Enabled);
  AssertEquals(LFetches, FetchCount);
  FGrid.Enabled := True;
end;

procedure TVisualGridRefreshTests.NonblockingManualLoadingRespectsFetchRestrictions;
begin
  FGrid.ShowLoading('Background information', '', nil, False);
  AssertTrue(TLoadingGridAccess(FGrid).ManualRegionsEnabled);
  AssertTrue(TLoadingGridAccess(FGrid).FetchFieldsEnabled);
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled), Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  AssertFalse(FGrid.InternalDrawGrid.Enabled);
  FGrid.ShowLoading('Updated information', '', nil, False);
  AssertFalse('A nonblocking manual state cannot override fetch restrictions', FGrid.InternalDrawGrid.Enabled);
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  Settle;
  AssertTrue(FGrid.LoadingVisible);
  AssertTrue(TLoadingGridAccess(FGrid).FetchFieldsEnabled);
  FGrid.HideLoading;
end;

procedure TVisualGridRefreshTests.ManualLoadingCanBeRequestedBeforeParenting;
var LUnparented: TVisualGrid; LFetches: Integer;
begin
  LFetches := FetchCount;
  LUnparented := TVisualGrid.Create(FForm);
  try
    LUnparented.ShowLoading('Before parenting', 'State is retained');
    AssertTrue(LUnparented.LoadingVisible);
    AssertFalse('Presentation must not request an unparented native window', LUnparented.HandleAllocated);
    LUnparented.SetBounds(0, 0, 320, 180);
    LUnparented.Parent := FForm;
    PumpFor(50);
    AssertTrue(TPanel(LUnparented.FindComponent('LoadingOverlay')).Visible);
    AssertEquals('Before parenting', TLabel(LUnparented.FindComponent('LoadingStatus')).Caption);
    LUnparented.HideLoading;
    AssertFalse(LUnparented.LoadingVisible);
  finally
    LUnparented.Free;
  end;
  AssertEquals(LFetches, FetchCount);
end;

procedure TVisualGridQueuedCompletionTests.DisableTimers(AOwner: TComponent);
var I: Integer;
begin
  for I := 0 to AOwner.ComponentCount - 1 do begin
    if AOwner.Components[I] is TTimer then
      TTimer(AOwner.Components[I]).Enabled := False;
    DisableTimers(AOwner.Components[I]);
  end;
end;

procedure TVisualGridQueuedCompletionTests.PumpCompletionOnly(AMilliseconds: Cardinal);
var LUntil: QWord;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    // Deliberately do not pump window messages or any TTimer. The provider's
    // queued main-thread completion must be sufficient to publish its page.
    CheckSynchronize(0);
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

procedure TVisualGridQueuedCompletionTests.AwaitCompletionOnly;
var LUntil: QWord;
begin
  LUntil := GetTickCount64 + 1500;
  while FGrid.IsUpdating and (GetTickCount64 < LUntil) do
    PumpCompletionOnly(1);
  AssertFalse('A worker must publish through its queued notification, without a timer',
    FGrid.IsUpdating);
end;

{$IF defined(MSWINDOWS) and defined(VISUALGRID_TEST_WORKER_START)}
procedure TVisualGridQueuedCompletionTests.WorkerCancelledBeforeExecutionRetiresAndPreservesOtherReceivers;
begin
  CheckVisualGridEarlyWorkerCancellation;
end;
{$ENDIF}

procedure TVisualGridQueuedCompletionTests.CancelActiveFetchNeverJoinsOrPublishesAHeldQuery;
begin
  CheckVisualGridActiveCancellation(acsHeldProvider);
end;

procedure TVisualGridQueuedCompletionTests.CancelledFetchRetainsTheLatestFiveSecondAdmission;
begin
  CheckVisualGridActiveCancellation(acsQuietAdmission);
end;

procedure TVisualGridQueuedCompletionTests.CancelledProviderExceptionCannotPublishAnErrorCard;
begin
  CheckVisualGridActiveCancellation(acsCancelledError);
end;

{$IF defined(MSWINDOWS) and defined(VISUALGRID_TEST_WORKER_START)}
procedure TVisualGridQueuedCompletionTests.PublicCancellationBeforeStartKeepsTheLatestRequest;
begin
  CheckVisualGridEarlyActiveCancellation;
end;
{$ENDIF}

procedure TVisualGridQueuedCompletionTests.FinishedWorkerPublishesWithAllTimersDisabled;
var LBefore: Integer;
begin
  FGrid.AutoPageSize := False;
  Settle;
  LBefore := FCompletions;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled), Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  DisableTimers(FGrid);
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  AwaitCompletionOnly;
  AssertEquals('The actual worker publishes exactly once', LBefore + 1, FCompletions);
  AssertTrue('Finished rows are readable', FGrid.RowCount > 0);
  PumpCompletionOnly(50);
  AssertEquals('A queued wake cannot publish the same worker twice', LBefore + 1, FCompletions);
end;

procedure TVisualGridQueuedCompletionTests.FailedWorkerPublishesAndRetriesWithoutTimers;
var LBefore: Integer;
begin
  FGrid.AutoPageSize := False;
  Settle;
  LBefore := FCompletions;
  TGridRefreshSource(FSource).FailRead := True;
  FGrid.RefreshGrid;
  DisableTimers(FGrid);
  AwaitCompletionOnly;
  AssertEquals('A failed fetch is not a successful page completion', LBefore, FCompletions);
  AssertTrue('The prior rows survive failure', FGrid.RowCount > 0);
  AssertEquals('Controlled data-source failure', TLabel(FGrid.FindComponent('LoadingStatus')).Caption);
  TGridRefreshSource(FSource).FailRead := False;
  FGrid.RefreshGrid;
  DisableTimers(FGrid);
  AwaitCompletionOnly;
  AssertEquals('The new actual worker can recover without polling', LBefore + 1, FCompletions);
end;

procedure TVisualGridQueuedCompletionTests.DisposingQueuedReceiverPreservesOtherGridCompletion;
var LOtherSource: TGridRefreshSource; LOther: TVisualGrid; LBefore: Integer;
  LUntil: QWord;
begin
  FGrid.AutoPageSize := False;
  Settle;
  LOtherSource := TGridRefreshSource.Create(FForm);
  LOther := TVisualGrid.Create(FForm);
  LOther.Parent := FForm;
  LOther.AutoPageSize := False;
  LOther.PageSize := 5;
  LOther.AddColumn('Id').Binding := 'Id';
  LOther.FetchDataInThread := True;
  LOther.OnFinishedUpdating := Completed;
  LOtherSource.BlockRead := True;
  try
    TGridRefreshSource(FSource).BlockRead := True;
    TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
    TGridRefreshSource(FSource).Returned.ResetEvent;
    FGrid.RefreshGrid;
    LOther.DataSource := LOtherSource;
    AssertEquals(Ord(wrSignaled), Ord(LOtherSource.Entered.WaitFor(1000)));
    DisableTimers(FGrid);
    DisableTimers(LOther);
    LBefore := FCompletions;
    TGridRefreshSource(FSource).ReleaseRead.SetEvent;
    LOtherSource.ReleaseRead.SetEvent;
    AssertEquals(Ord(wrSignaled), Ord(TGridRefreshSource(FSource).Returned.WaitFor(1000)));
    AssertEquals(Ord(wrSignaled), Ord(LOtherSource.Returned.WaitFor(1000)));
    Sleep(20); // Producers can queue; the main thread has not pumped them.
    FreeAndNil(FGrid);
    LUntil := GetTickCount64 + 1500;
    while LOther.IsUpdating and (GetTickCount64 < LUntil) do PumpCompletionOnly(1);
    AssertFalse('Disposing one receiver cannot remove another grid completion', LOther.IsUpdating);
    AssertEquals('Only the surviving grid publishes', LBefore + 1, FCompletions);
    PumpCompletionOnly(30);
    AssertEquals('Disposed queued work stays cancelled', LBefore + 1, FCompletions);
  finally
    TGridRefreshSource(FSource).ReleaseRead.SetEvent;
    LOtherSource.ReleaseRead.SetEvent;
    LOther.Free;
  end;
end;

procedure TVisualGridQueuedCompletionTests.CompleteAndRequestNext(Sender: TObject);
begin
  Completed(Sender);
  if FRequestNextPage then begin
    FRequestNextPage := False;
    FGrid.PageIndex := 2;
  end;
end;

procedure TVisualGridQueuedCompletionTests.QueuedCompletionAcceptsANewClientRequest;
var LBefore: Integer;
begin
  FGrid.AutoPageSize := False;
  FGrid.PageSize := 10;
  Settle;
  LBefore := FCompletions;
  FRequestNextPage := True;
  FGrid.OnFinishedUpdating := CompleteAndRequestNext;
  FGrid.RefreshGrid;
  DisableTimers(FGrid);
  AwaitCompletionOnly;
  AssertEquals('A user completion refresh is new client work', LBefore + 2, FCompletions);
  AssertEquals('The latest requested page is published', 2, FGrid.PageIndex);
  AssertEquals(20, Integer(FGrid.Cells[0, 0]));
end;

procedure TVisualGridQueuedCompletionTests.SourceReplacementDoesNotJoinANewHeldWorkerFromAStaleWake;
var LOtherSource: TGridRefreshSource; LStarted: QWord;
begin
  FGrid.AutoPageSize := False;
  FGrid.PageSize := 30;
  Settle;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled), Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  LOtherSource := TGridRefreshSource.Create(FForm);
  LOtherSource.BlockRead := True;
  LOtherSource.ItemCount := 23;
  try
    TGridRefreshSource(FSource).ReleaseRead.SetEvent;
    FGrid.DataSource := LOtherSource;
    AssertEquals(Ord(wrSignaled), Ord(LOtherSource.Entered.WaitFor(1000)));
    DisableTimers(FGrid);
    LStarted := GetTickCount64;
    PumpCompletionOnly(80);
    AssertTrue('An obsolete wake must not join the replacement provider',
      GetTickCount64 - LStarted < 400);
    AssertTrue('The new held worker remains independently active', FGrid.IsUpdating);
    LOtherSource.ReleaseRead.SetEvent;
    AwaitCompletionOnly;
    AssertEquals(23, TRequestGridAccess(FGrid).PublishedTotalCount);
    AssertTrue((FGrid.RowCount > 0) and (FGrid.RowCount <= FGrid.PageSize));
  finally
    LOtherSource.ReleaseRead.SetEvent;
    FGrid.DataSource := TGridRefreshSource(FSource);
  end;
end;

procedure TVisualGridRequestThrottleTests.SetUp;
begin
  inherited;
  FWaitStarts := 0;
  FWaitEnds := 0;
  FRequestOnCompletion := False;
  FGrid.AutoPageSize := False;
  FGrid.PageSize := 10;
  Settle;
  FGrid.FetchDataInThread := False;
  FGrid.OnFinishedUpdating := RequestCompleted;
  FGrid.RefreshThrottle.OnWaitingChanged.Add(WaitingChanged);
end;

procedure TVisualGridRequestThrottleTests.TearDown;
begin
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  FGrid.RefreshThrottle.OnWaitingChanged.Remove(WaitingChanged);
  FGrid.RefreshThrottle.Mode := temNone;
  inherited;
end;

procedure TVisualGridRequestThrottleTests.WaitingChanged(Sender: TObject);
begin
  AssertTrue(Sender = FGrid.RefreshThrottle);
  if FGrid.RefreshThrottle.Waiting then Inc(FWaitStarts) else Inc(FWaitEnds);
end;

procedure TVisualGridRequestThrottleTests.RequestCompleted(Sender: TObject);
begin
  Completed(Sender);
  if FRequestOnCompletion then begin
    FRequestOnCompletion := False;
    FGrid.RefreshGrid;
  end;
end;

procedure TVisualGridRequestThrottleTests.EnableThrottle(AInterval: Integer);
begin
  FGrid.RefreshThrottle.Interval := TTimeSpan.FromMilliseconds(AInterval);
  FGrid.RefreshThrottle.Mode := temNotifyOnEventBurstStartAndFinished;
end;

function TVisualGridRequestThrottleTests.PageCalls: Integer;
begin
  Result := InterlockedCompareExchange(TGridRefreshSource(FSource).PageCallCount,0,0);
end;

procedure TVisualGridRequestThrottleTests.AwaitQuiet;
var LDeadline: QWord;
begin
  LDeadline := GetTickCount64 + 5000;
  repeat
    PumpFor(10);
    if not FGrid.RefreshThrottle.Waiting and not FGrid.IsUpdating then begin
      PumpFor(60);
      if not FGrid.RefreshThrottle.Waiting and not FGrid.IsUpdating then Exit;
    end;
  until GetTickCount64 >= LDeadline;
  Fail('The actual grid did not finish its own quiet request and page fetch');
end;

procedure TVisualGridRequestThrottleTests.ContinuousClientRequestsAdmitOnlyLeadingAndFinalPages;
var LBefore,I: Integer;
begin
  EnableThrottle;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  AssertEquals(LBefore+1,PageCalls);
  for I := 1 to 12 do begin
    PumpFor(50);
    FGrid.RefreshGrid;
    AssertTrue(FGrid.RefreshThrottle.Waiting);
    AssertEquals('Continuous inputs must not produce a middle page query',LBefore+1,PageCalls);
    AssertEquals(0,FWaitEnds);
  end;
  PumpFor(100);
  AssertEquals(LBefore+1,PageCalls);
  AwaitQuiet;
  AssertEquals(LBefore+2,PageCalls);
  AssertEquals(1,FWaitStarts);
  AssertEquals(1,FWaitEnds);
end;

procedure TVisualGridRequestThrottleTests.SingleBusyLeadingRequestWaitsForItsOwnQuietFinish;
var LBefore: Integer;
begin
  FGrid.FetchDataInThread := True;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled),Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  EnableThrottle(1000);
  FGrid.RefreshGrid;
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  PumpFor(400);
  AssertFalse('The older worker must have completed',FGrid.IsUpdating);
  AssertTrue(FGrid.RefreshThrottle.Waiting);
  AssertEquals('Worker completion cannot bypass a waiting single leading input',LBefore+1,PageCalls);
  AwaitQuiet;
  AssertEquals(LBefore+2,PageCalls);
  AssertEquals(1,FWaitStarts);
  AssertEquals(1,FWaitEnds);
end;

procedure TVisualGridRequestThrottleTests.BusyInputsRetainTheLatestPageParameters;
var LBefore: Integer;
begin
  FGrid.FetchDataInThread := True;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled),Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  EnableThrottle(800);
  FGrid.RefreshGrid;
  FGrid.Visible := False;
  FGrid.PageSize := 17;
  FGrid.PageIndex := 3;
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  PumpFor(400);
  AssertTrue('Hidden inputs still reset the same owned throttle',FGrid.RefreshThrottle.Waiting);
  AssertEquals(LBefore+1,PageCalls);
  FGrid.Visible := True;
  AwaitQuiet;
  AssertEquals(LBefore+2,PageCalls);
  AssertEquals(17,TGridRefreshSource(FSource).LastParams.PageSize);
  AssertEquals(3,TGridRefreshSource(FSource).LastParams.PageIndex);
  AssertEquals(51,Integer(FGrid.Cells[0,0]));
end;

procedure TVisualGridRequestThrottleTests.PageSearchAndSortRequestsShareOneAdmission;
var LBefore: Integer;
begin
  FGrid.Columns[0].Filters := [vgfNumericGT,vgfSortable];
  FGrid.SortMode := smSingleColumn;
  EnableThrottle;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  FGrid.PageSize := 25;
  FGrid.PageIndex := 2;
  TRequestGridAccess(FGrid).Search('>400');
  FGrid.Columns[0].SortDirection := sdDescending;
  AssertEquals(LBefore+1,PageCalls);
  PumpFor(100);
  AssertEquals(LBefore+1,PageCalls);
  AwaitQuiet;
  AssertEquals('Paging, search and sort must produce one latest query',LBefore+2,PageCalls);
  AssertEquals(25,TGridRefreshSource(FSource).LastParams.PageSize);
  AssertEquals(2,TGridRefreshSource(FSource).LastParams.PageIndex);
  AssertEquals(949,Integer(FGrid.Cells[0,0]));
end;

procedure TVisualGridRequestThrottleTests.SourceReplacementUsesOnlyTheLatestAdmittedSource;
var LBefore: Integer; LNewSource: TGridRefreshSource;
begin
  EnableThrottle;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  LNewSource := TGridRefreshSource.Create(FForm);
  LNewSource.ItemCount := 30;
  FGrid.DataSource := LNewSource;
  FGrid.PageSize := 7;
  AssertEquals(LBefore+1,PageCalls);
  AssertEquals(0,LNewSource.PageCallCount);
  AwaitQuiet;
  AssertEquals(1,LNewSource.PageCallCount);
  AssertEquals(7,LNewSource.LastParams.PageSize);
  AssertEquals(7,FGrid.RowCount);
end;

procedure TVisualGridRequestThrottleTests.ResizeDuringWaitingIsVisualAndRetainsLatestCapacity;
var LBefore,LSize: Integer;
begin
  FGrid.AutoPageSize := True;
  Settle;
  EnableThrottle;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  FForm.Height := FForm.Height+40;
  PumpFor(60);
  FForm.Height := FForm.Height+50;
  PumpFor(60);
  LSize := FGrid.PageSize;
  AssertTrue(FGrid.RefreshThrottle.Waiting);
  AssertEquals('Resize while Waiting must update geometry without querying data',LBefore+1,PageCalls);
  AssertEquals(0,FWaitEnds);
  AwaitQuiet;
  AssertEquals(LBefore+2,PageCalls);
  AssertEquals(LSize,TGridRefreshSource(FSource).LastParams.PageSize);
  AssertEquals(LSize,FGrid.RowCount);
  PumpFor(400);
  AssertEquals('Settled geometry cannot restart another burst/query',LBefore+2,PageCalls);
  AssertEquals(1,FWaitStarts);
end;

procedure TVisualGridRequestThrottleTests.CompletedResultLayoutCannotStartAnotherBurst;
var LBefore: Integer;
begin
  FGrid.AutoPageSize := True;
  Settle;
  FGrid.FetchDataInThread := True;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).ItemCount := 0;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  EnableThrottle;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled),Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  PumpFor(400);
  AssertFalse('Own quiet finish is independent of the held page fetch',FGrid.RefreshThrottle.Waiting);
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  AwaitQuiet;
  PumpFor(500);
  AssertEquals('Applying empty-page chrome must not notify or refetch itself',LBefore+1,PageCalls);
  AssertEquals(1,FWaitStarts);
  AssertEquals(1,FWaitEnds);
  AssertEquals(0,FGrid.RowCount);
end;

procedure TVisualGridRequestThrottleTests.FinishedUpdatingClientRequestIsNewInput;
var LBefore: Integer;
begin
  EnableThrottle;
  LBefore := PageCalls;
  FRequestOnCompletion := True;
  FGrid.RefreshGrid;
  AssertEquals(LBefore+1,PageCalls);
  AssertTrue(FGrid.RefreshThrottle.Waiting);
  AwaitQuiet;
  AssertEquals('A consumer completion callback remains a new client request',LBefore+2,PageCalls);
  AssertEquals(1,FWaitStarts);
  AssertEquals(1,FWaitEnds);
end;

procedure TVisualGridRequestThrottleTests.ANewBurstAfterQuietLeadsImmediately;
var LBefore: Integer;
begin
  EnableThrottle;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  FGrid.RefreshGrid;
  AwaitQuiet;
  AssertEquals(LBefore+2,PageCalls);
  FGrid.RefreshGrid;
  AssertEquals('A new independent request must lead before returning',LBefore+3,PageCalls);
  AssertEquals(2,FWaitStarts);
end;

procedure TVisualGridRequestThrottleTests.CancelCannotReleaseABusyWaitingRequest;
var LBefore: Integer;
begin
  FGrid.FetchDataInThread := True;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled),Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  EnableThrottle(800);
  FGrid.RefreshGrid;
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  PumpFor(400);
  AssertTrue(FGrid.RefreshThrottle.Waiting);
  AssertFalse(FGrid.IsUpdating);
  FGrid.RefreshThrottle.Cancel;
  PumpFor(1000);
  AssertFalse(FGrid.RefreshThrottle.Waiting);
  AssertEquals('Cancel must not release a deferred leading page',LBefore+1,PageCalls);
  FGrid.RefreshGrid;
  PumpFor(100);
  AssertEquals('A new request after cancellation still leads immediately',LBefore+2,PageCalls);
  AwaitQuiet;
end;

procedure TVisualGridRequestThrottleTests.ModeChangeCancelsPendingPageWork;
var LBefore: Integer;
begin
  EnableThrottle;
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  FGrid.PageSize := 17;
  FGrid.RefreshThrottle.Mode := temNone;
  PumpFor(400);
  AssertEquals('A mode change cancels old work rather than admitting it',LBefore+1,PageCalls);
  FGrid.RefreshGrid;
  AssertEquals(LBefore+2,PageCalls);
  AssertEquals(17,TGridRefreshSource(FSource).LastParams.PageSize);
end;

procedure TVisualGridRequestThrottleTests.OwnerDestructionCannotReleaseCancelledWork;
var LGrid: TVisualGrid; LSource: TGridRefreshSource;
begin
  LSource := TGridRefreshSource.Create(FForm);
  LGrid := TVisualGrid.Create(FForm);
  try
    LGrid.Parent := FForm;
    LGrid.SetBounds(0,0,300,200);
    LGrid.PageSize := 10;
    LGrid.AddColumn('Id').Binding := 'Id';
    LSource.BlockRead := True;
    LGrid.DataSource := LSource;
    AssertEquals(Ord(wrSignaled),Ord(LSource.Entered.WaitFor(1000)));
    LGrid.RefreshThrottle.Interval := TTimeSpan.FromMilliseconds(800);
    LGrid.RefreshThrottle.Mode := temNotifyOnEventBurstStartAndFinished;
    LGrid.RefreshGrid;
    LSource.ReleaseRead.SetEvent;
    PumpFor(400);
    AssertTrue(LGrid.RefreshThrottle.Waiting);
    LGrid.RefreshThrottle.Cancel;
  finally
    LSource.ReleaseRead.SetEvent;
    LGrid.Free;
  end;
  PumpFor(1000);
  AssertEquals('Cancelling and destroying the owner must not start another page',1,LSource.PageCallCount);
end;

procedure TVisualGridRequestThrottleTests.PartialResultLayoutRetainsLatestAdmittedCapacity;
var LBefore,LCount,LInitialCapacity,LNativeCapacity,LPitch: Integer;
  LFirst,LSecond: TRect;
begin
  FGrid.AutoPageSize := True;
  Settle;
  LInitialCapacity := FGrid.PageSize;
  AssertTrue('The geometry fixture needs at least three initial data rows',LInitialCapacity > 2);
  // The mode selector retains its footer. A one-page result instead hides
  // search chrome, exposing actual extra rows before the final admission.
  LCount := LInitialCapacity-1;
  TGridRefreshSource(FSource).ItemCount := LCount;
  FGrid.FetchDataInThread := True;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  EnableThrottle(1000);
  LBefore := PageCalls;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled),Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  FGrid.RefreshGrid;
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  PumpFor(400);
  AssertTrue(FGrid.RefreshThrottle.Waiting);
  AssertEquals('Applying partial-page chrome cannot query during Waiting',LBefore+1,PageCalls);
  AssertEquals(0,FWaitEnds);
  // Derive capacity from native cell geometry rather than the grid's sizing
  // implementation or an assumption that the entire footer disappears.
  LFirst := FGrid.InternalDrawGrid.CellRect(0,FGrid.InternalDrawGrid.FixedRows);
  LSecond := FGrid.InternalDrawGrid.CellRect(0,FGrid.InternalDrawGrid.FixedRows+1);
  LPitch := LSecond.Top-LFirst.Top;
  AssertTrue('Native rows need a positive pitch',LPitch > 0);
  AssertTrue('The first native data row must fit',LFirst.Bottom <= FGrid.InternalDrawGrid.ClientHeight);
  LNativeCapacity := 1+(FGrid.InternalDrawGrid.ClientHeight-LFirst.Bottom) div LPitch;
  AssertTrue('Internal search layout must expose additional complete rows',LNativeCapacity > LInitialCapacity);
  AwaitQuiet;
  AssertEquals('The final admitted page contains every matching row',LCount,FGrid.RowCount);
  AssertEquals('The final query uses independently measured settled capacity',
    LNativeCapacity,TGridRefreshSource(FSource).LastParams.PageSize);
  AssertEquals('Own layout must not turn into another request/burst',1,FWaitStarts);
  AssertEquals(1,FWaitEnds);
  AssertEquals('The one latest final query must already include settled capacity',LBefore+2,PageCalls);
end;

procedure TVisualGridRequestThrottleTests.ClearingSourcePublishesEmptyRowsWithoutLayoutReplay;
var LBefore: Integer;
begin
  LBefore := PageCalls;
  FGrid.DataSource := nil;
  AssertEquals('A cleared source must publish an empty page',0,FGrid.RowCount);
  PumpFor(300);
  AssertEquals('Clearing a source has no provider query to replay',LBefore,PageCalls);
  FGrid.FetchDataInThread := True;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  FGrid.DataSource := TGridRefreshSource(FSource);
  AssertEquals(Ord(wrSignaled),Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  EnableThrottle(500);
  FGrid.RefreshGrid;
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  FGrid.DataSource := nil;
  AssertTrue(FGrid.RefreshThrottle.Waiting);
  AwaitQuiet;
  AssertEquals('A waiting clear request must retain its latest nil source',0,FGrid.RowCount);
  AssertEquals(LBefore+1,PageCalls);
end;

procedure TVisualGridRequestThrottleTests.SuspendedInputsRetainLatestParametersUntilImmediateResume;
var LBefore: Integer;
begin
  FGrid.Columns[0].Filters := [vgfNumericGT, vgfSortable];
  FGrid.SortMode := smSingleColumn;
  EnableThrottle(5000);
  LBefore := PageCalls;
  FGrid.RefreshSuspended := True;
  FGrid.RefreshGrid;
  FGrid.PageSize := 25;
  FGrid.PageIndex := 2;
  TRequestGridAccess(FGrid).Search('>400');
  FGrid.Columns[0].SortDirection := sdDescending;
  AssertTrue('Client inputs still reach the independent throttle', FGrid.RefreshThrottle.Waiting);
  AssertEquals('Suspension must reject even the leading fetch', LBefore, PageCalls);
  FGrid.RefreshThrottle.Cancel;
  AssertEquals('Cancelling the quiet interval while suspended cannot query', LBefore, PageCalls);
  FGrid.RefreshSuspended := False;
  AssertEquals('Resume admits one page immediately, without a five-second wait', LBefore + 1, PageCalls);
  AssertEquals(25, TGridRefreshSource(FSource).LastParams.PageSize);
  AssertEquals(2, TGridRefreshSource(FSource).LastParams.PageIndex);
  AssertEquals('Latest search and sort survive suspension', 949, Integer(FGrid.Cells[0, 0]));
  AssertFalse('Internal resume does not notify the throttle again', FGrid.RefreshThrottle.Waiting);
  PumpFor(300);
  AssertEquals('The retained request is delivered once', LBefore + 1, PageCalls);
end;

procedure TVisualGridRequestThrottleTests.SuspendedResizeChangesGeometryWithoutFetching;
var LBefore, LSize: Integer;
begin
  FGrid.AutoPageSize := True;
  Settle;
  EnableThrottle(5000);
  LBefore := PageCalls;
  FGrid.RefreshSuspended := True;
  FGrid.RefreshGrid;
  FForm.Height := FForm.Height + 80;
  PumpFor(60);
  FForm.Height := FForm.Height + 60;
  PumpFor(60);
  LSize := FGrid.PageSize;
  AssertEquals('Paused resizing is visual only', LBefore, PageCalls);
  FGrid.RefreshThrottle.Cancel;
  FGrid.RefreshSuspended := False;
  AssertEquals(LBefore + 1, PageCalls);
  AssertEquals(LSize, TGridRefreshSource(FSource).LastParams.PageSize);
  AssertEquals(LSize, FGrid.RowCount);
  PumpFor(300);
  AssertEquals('The result layout cannot replay an older capacity', LBefore + 1, PageCalls);
end;

procedure TVisualGridRequestThrottleTests.SuspensionCancelsWorkerWithoutJoiningOrPublishingError;
var LBefore, LCompleted, LRow: Integer; LStarted, LDeadline: QWord;
begin
  FGrid.FetchDataInThread := True;
  LRow := Integer(FGrid.Cells[0, 0]);
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).FailRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  LBefore := PageCalls;
  LCompleted := FCompletions;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled), Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  LStarted := GetTickCount64;
  FGrid.RefreshSuspended := True;
  AssertTrue('Suspension cannot join the held provider on the UI thread', GetTickCount64 - LStarted < 150);
  FGrid.ShowLoading('Please Wait', 'Synchronizing');
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  LDeadline := GetTickCount64 + 1500;
  while FGrid.IsUpdating and (GetTickCount64 < LDeadline) do PumpFor(1);
  AssertFalse('Cancelled work is retired by its completion notification', FGrid.IsUpdating);
  AssertEquals('A cancelled error cannot publish a page', LCompleted, FCompletions);
  AssertEquals('Old visible data survives cancellation', LRow, Integer(FGrid.Cells[0, 0]));
  AssertTrue('Worker retirement cannot clear caller-owned sync presentation', FGrid.LoadingVisible);
  AssertEquals('Cancelled errors cannot replace the held title', 'Please Wait',
    TLabel(FGrid.FindComponent('LoadingStatus')).Caption);
  AssertEquals('Retiring the worker cannot fetch while suspended', LBefore + 1, PageCalls);
  TGridRefreshSource(FSource).FailRead := False;
  TGridRefreshSource(FSource).BlockRead := False;
  FGrid.RefreshThrottle.Cancel;
  FGrid.RefreshSuspended := False;
  Settle;
  AssertEquals(LBefore + 2, PageCalls);
  AssertEquals(LCompleted + 1, FCompletions);
  FGrid.HideLoading;
end;

procedure TVisualGridRequestThrottleTests.ResumeBeforeCancelledWorkerRetiresFetchesOnlyLatestPage;
var LBefore, LCompleted: Integer;
begin
  FGrid.FetchDataInThread := True;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  LBefore := PageCalls;
  LCompleted := FCompletions;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled), Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  FGrid.RefreshSuspended := True;
  EnableThrottle(5000);
  FGrid.PageSize := 17;
  FGrid.PageIndex := 3;
  FGrid.RefreshThrottle.Cancel;
  FGrid.RefreshSuspended := False;
  AssertEquals('Resume must not start a second concurrent worker', LBefore + 1, PageCalls);
  TGridRefreshSource(FSource).BlockRead := False;
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  Settle;
  AssertEquals('Old cancellation is followed by one latest query', LBefore + 2, PageCalls);
  AssertEquals('Only the replacement page is published', LCompleted + 1, FCompletions);
  AssertEquals(17, TGridRefreshSource(FSource).LastParams.PageSize);
  AssertEquals(3, TGridRefreshSource(FSource).LastParams.PageIndex);
  AssertEquals(51, Integer(FGrid.Cells[0, 0]));
  AssertFalse(FGrid.RefreshThrottle.Waiting);
end;

procedure TVisualGridRequestThrottleTests.ResuspensionCannotReviveACancelledWorkerResult;
var LBefore, LCompleted, LRow: Integer; LDeadline: QWord;
begin
  FGrid.FetchDataInThread := True;
  LRow := Integer(FGrid.Cells[0, 0]);
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  LBefore := PageCalls;
  LCompleted := FCompletions;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled), Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  FGrid.RefreshSuspended := True;
  FGrid.PageIndex := 2;
  FGrid.RefreshThrottle.Cancel;
  FGrid.RefreshSuspended := False;
  FGrid.RefreshSuspended := True;
  FGrid.ShowLoading('Please Wait', 'Synchronizing');
  TGridRefreshSource(FSource).ReleaseRead.SetEvent;
  LDeadline := GetTickCount64 + 1500;
  while FGrid.IsUpdating and (GetTickCount64 < LDeadline) do PumpFor(1);
  AssertFalse(FGrid.IsUpdating);
  AssertEquals('A true/false/true flip cannot publish the cancelled result', LCompleted, FCompletions);
  AssertEquals(LRow, Integer(FGrid.Cells[0, 0]));
  AssertEquals('Resuspension prevents the deferred fetch too', LBefore + 1, PageCalls);
  AssertTrue(FGrid.LoadingVisible);
  TGridRefreshSource(FSource).BlockRead := False;
  FGrid.PageIndex := 4;
  FGrid.RefreshThrottle.Cancel;
  FGrid.RefreshSuspended := False;
  Settle;
  AssertEquals(LBefore + 2, PageCalls);
  AssertEquals(LCompleted + 1, FCompletions);
  AssertEquals(40, Integer(FGrid.Cells[0, 0]));
  FGrid.HideLoading;
end;

procedure TVisualGridRequestThrottleTests.SuspendedQuietCompletionCannotAdmitAQuery;
var LBefore: Integer;
begin
  EnableThrottle;
  LBefore := PageCalls;
  FGrid.RefreshSuspended := True;
  FGrid.RefreshGrid;
  FGrid.PageIndex := 3;
  PumpFor(500);
  AssertFalse('The generic throttle still owns and finishes its timer', FGrid.RefreshThrottle.Waiting);
  AssertEquals('Quiet completion cannot bypass suspension', LBefore, PageCalls);
  FGrid.RefreshSuspended := False;
  AssertEquals(LBefore + 1, PageCalls);
  AssertEquals(30, Integer(FGrid.Cells[0, 0]));
  PumpFor(300);
  AssertEquals(LBefore + 1, PageCalls);
end;

procedure TVisualGridRequestThrottleTests.SuspendedSourceReplacementUsesOnlyLatestSource;
var LBefore: Integer; LNewSource: TGridRefreshSource;
begin
  EnableThrottle(5000);
  LBefore := PageCalls;
  FGrid.RefreshSuspended := True;
  LNewSource := TGridRefreshSource.Create(FForm);
  LNewSource.ItemCount := 30;
  FGrid.DataSource := LNewSource;
  FGrid.PageSize := 7;
  FGrid.PageIndex := 2;
  AssertEquals(LBefore, PageCalls);
  AssertEquals(0, LNewSource.PageCallCount);
  FGrid.RefreshThrottle.Cancel;
  FGrid.RefreshSuspended := False;
  AssertEquals('Only the newest source receives the resumed query', 1, LNewSource.PageCallCount);
  AssertEquals(LBefore, PageCalls);
  AssertEquals(14, Integer(FGrid.Cells[0, 0]));
  FGrid.RefreshThrottle.Mode := temNone;
  FGrid.DataSource := TGridRefreshSource(FSource);
end;

procedure TVisualGridRequestThrottleTests.SuspensionCooperativelyReleasesTheProviderBeforeResume;
var LBefore, LCompleted: Integer; LDeadline: QWord;
begin
  FGrid.FetchDataInThread := True;
  TGridRefreshSource(FSource).BlockRead := True;
  TGridRefreshSource(FSource).CooperativeRead := True;
  TGridRefreshSource(FSource).Entered.ResetEvent;
  TGridRefreshSource(FSource).ReleaseRead.ResetEvent;
  LBefore := PageCalls;
  LCompleted := FCompletions;
  FGrid.RefreshGrid;
  AssertEquals(Ord(wrSignaled), Ord(TGridRefreshSource(FSource).Entered.WaitFor(1000)));
  FGrid.RefreshSuspended := True;
  AssertEquals('The provider observes the worker cancellation without manual gate release',
    Ord(wrSignaled), Ord(TGridRefreshSource(FSource).CancelObserved.WaitFor(1000)));
  LDeadline := GetTickCount64 + 1500;
  while FGrid.IsUpdating and (GetTickCount64 < LDeadline) do PumpFor(1);
  AssertFalse(FGrid.IsUpdating);
  AssertEquals('Abort is not a completed page or a visible error', LCompleted, FCompletions);
  TGridRefreshSource(FSource).BlockRead := False;
  FGrid.PageIndex := 5;
  FGrid.RefreshThrottle.Cancel;
  FGrid.RefreshSuspended := False;
  Settle;
  AssertEquals('The next worker acquires the same real data-source lock after Abort finally',
    LBefore + 2, PageCalls);
  AssertEquals(LCompleted + 1, FCompletions);
  AssertEquals(50, Integer(FGrid.Cells[0, 0]));
end;

procedure TVisualGridRequestThrottleTests.QueuedRequestShowsLoadingBeforeFetchAdmission;
var LCalls: Integer;
begin
  EnableThrottle(900);
  FGrid.RefreshGrid;
  LCalls := TGridRefreshSource(FSource).PageCallCount;
  FGrid.PageIndex := 1;
  PumpFor(250);
  AssertTrue(FGrid.RefreshThrottle.Waiting);
  AssertEquals('Waiting must not fetch the new selection',LCalls,
    TGridRefreshSource(FSource).PageCallCount);
  AssertTrue('Queued work needs visible feedback before the fetch starts',
    TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
  AssertTrue(TLoadingCircle(FGrid.FindComponent('LoadingCircle')).Active);
  PumpFor(800);
  AssertEquals(LCalls+1,TGridRefreshSource(FSource).PageCallCount);
  AssertFalse('A completed synchronous fetch also clears its indicator',
    TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
end;

procedure TVisualGridRequestThrottleTests.CancelledQueuedRequestHidesLoading;
begin
  EnableThrottle(900);
  FGrid.RefreshGrid;
  FGrid.PageIndex := 1;
  PumpFor(250);
  AssertTrue(TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
  FGrid.RefreshThrottle.Cancel;
  AssertFalse('Cancel retires waiting feedback without starting a provider',
    TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
end;

procedure TVisualGridRequestThrottleTests.ManualLoadingOverridesQueuedRefreshPresentation;
begin
  EnableThrottle(900);
  FGrid.RefreshGrid;
  FGrid.PageIndex := 1;
  FGrid.ShowLoading('Please Wait','Synchronizing');
  PumpFor(250);
  AssertEquals('Please Wait',TLabel(FGrid.FindComponent('LoadingStatus')).Caption);
  FGrid.RefreshThrottle.Cancel;
  AssertTrue('Ordinary cancellation must not dismiss caller-owned artwork',
    TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
  FGrid.HideLoading;
  AssertFalse(TPanel(FGrid.FindComponent('LoadingOverlay')).Visible);
end;

procedure TVisualGridRequestThrottleTests.DeferredSingleInputRetainsLoadingThroughQuietAdmission;
var LUntil: QWord; LSource: TGridRefreshSource; LPanel: TPanel;
begin
  LSource := TGridRefreshSource(FSource);
  FGrid.FetchDataInThread := True;
  LSource.BlockRead := True;
  LSource.ReleaseRead.ResetEvent;
  LSource.Entered.ResetEvent;
  FGrid.RefreshGrid;
  try
    LUntil := GetTickCount64+2000;
    while (LSource.Entered.WaitFor(0)<>wrSignaled) and (GetTickCount64<LUntil) do PumpFor(5);
    AssertTrue('Original provider entered its actual read',LSource.Entered.WaitFor(0)=wrSignaled);
    EnableThrottle(900);
    FGrid.PageIndex := 1;
    LSource.ReleaseRead.SetEvent;
    LUntil := GetTickCount64+2000;
    while FGrid.IsUpdating and (GetTickCount64<LUntil) do PumpFor(5);
    AssertFalse(FGrid.IsUpdating);
    LSource.ReleaseRead.ResetEvent;
    PumpFor(250);
    LPanel := TPanel(FGrid.FindComponent('LoadingOverlay'));
    AssertTrue('Deferred single input already has visible feedback',LPanel.Visible);
    LUntil := GetTickCount64+2000;
    while FGrid.RefreshThrottle.Waiting and (GetTickCount64<LUntil) do PumpFor(5);
    AssertFalse(FGrid.RefreshThrottle.Waiting);
    AssertTrue('Genuine quiet finish admitted the retained held provider',FGrid.IsUpdating);
    AssertTrue('Admission must retain an already visible card without another delay',LPanel.Visible);
  finally LSource.ReleaseRead.SetEvent; end;
end;

initialization
  RegisterTest(TVisualGridSelectionEventTests);
  RegisterTest(TVisualGridLoadingLifecycleTests);
  RegisterTest(TVisualGridPagingTests);
  RegisterTest(TVisualGridViewportTests);
  RegisterTest(TVisualGridRefreshTests);
  RegisterTest(TVisualGridRequestThrottleTests);
  RegisterTest(TVisualGridQueuedCompletionTests);
end.
