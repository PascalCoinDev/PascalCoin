unit UVisualGrid.Worker.Checks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Native worker lifecycle checks use generated rows and an owned test process.
  The dedicated test-only constructor hook is restored on every exit path. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

type
  TGridActiveCancellationScenario = (acsHeldProvider, acsQuietAdmission, acsCancelledError);

procedure CheckVisualGridEarlyWorkerCancellation;
procedure CheckVisualGridEarlyActiveCancellation;
procedure CheckVisualGridActiveCancellation(AScenario: TGridActiveCancellationScenario);

implementation

uses Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Variants, SyncObjs, Generics.Collections,
  UCommon, UCommon.UI, UCommon.Data, UVisualGrid
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

{$IF defined(MSWINDOWS) and defined(VISUALGRID_TEST_WORKER_START)}
type
  TWorkerSource = class(TCustomDataSource<Integer>)
  public
    Calls, LastPage: LongInt;
    function GetColumns: TDataColumns; override;
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer;
      const AName: String): Variant; override;
    procedure FetchAll(const AItems: TList<Integer>); override;
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
      var ATable: TDataTable): TPageFetchResult; override;
  end;

  TWorkerCheck = class(TComponent)
  private
    FForm: TForm;
    FGrids: array[0..1] of TVisualGrid;
    FSources: array[0..1] of TWorkerSource;
    FCompletions: array[0..1] of Integer;
    FCancelNextWorker: Boolean;
    FForcedCancellations: Integer;
    FCancelNextThroughApi: Boolean;
    FApiPreStartCalls: Integer;
    procedure BeforeWorkerStart(Sender: TObject);
    procedure BeforeActiveCancelStart(Sender: TObject);
    procedure Completed(Sender: TObject);
    procedure DisableTimers(AOwner: TComponent);
    procedure Pump;
    procedure AwaitCompletion(AIndex, AExpected: Integer);
    procedure CheckResume;
    procedure CheckClose;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Run;
    procedure CheckActiveCancelBeforeStart;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function TWorkerSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Id',True));
end;

function TWorkerSource.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer;
  const AName: String): Variant;
begin
  Result := AItem;
end;

procedure TWorkerSource.FetchAll(const AItems: TList<Integer>);
var I: Integer;
begin
  for I := 0 to 99 do AItems.Add(I);
end;

function TWorkerSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ATable: TDataTable): TPageFetchResult;
begin
  LastPage := AParams.PageIndex;
  InterlockedIncrement(Calls);
  Result := inherited FetchPage(AParams,ATable);
end;

constructor TWorkerCheck.Create;
var I: Integer;
begin
  inherited Create(nil);
  FForm := TForm.CreateNew(nil);
  FForm.SetBounds(40,50,1000,500);
  for I := 0 to High(FGrids) do begin
    FSources[I] := TWorkerSource.Create(Self);
    FGrids[I] := TVisualGrid.Create(nil);
    FGrids[I].Parent := FForm;
    FGrids[I].SetBounds(8+I*480,8,470,420);
    FGrids[I].FetchDataInThread := False;
    FGrids[I].PageSize := 10;
    FGrids[I].CanSearch := False;
    FGrids[I].AddColumn('Id').Binding := 'Id';
    FGrids[I].OnFinishedUpdating := Completed;
    FGrids[I].DataSource := FSources[I];
  end;
  FForm.Show;
  Pump;
end;

destructor TWorkerCheck.Destroy;
var I: Integer;
begin
  for I := 0 to High(FGrids) do FreeAndNil(FGrids[I]);
  FForm.Free;
  inherited Destroy;
end;

procedure TWorkerCheck.BeforeWorkerStart(Sender: TObject);
begin
  if not FCancelNextWorker then Exit;
  Require(Sender is TThread,'The constructor hook did not receive its actual worker');
  FCancelNextWorker := False;
  Inc(FForcedCancellations);
  TThread(Sender).Terminate;
end;

procedure TWorkerCheck.BeforeActiveCancelStart(Sender: TObject);
begin
  if not FCancelNextThroughApi then
    Exit;
  FCancelNextThroughApi := False;
  Inc(FApiPreStartCalls);
  Require(Sender is TThread, 'The API hook did not receive its actual worker');
  {$IFNDEF VISUALGRID_ACTIVE_CANCEL_BASELINE}
  FGrids[0].CancelActiveFetch;
  {$ENDIF}
  // Constructor-time cancellation must preserve the next ordinary request;
  // no suspension, scheduler priority or message dispatch is involved.
  FGrids[0].PageIndex := 6;
  Require(not FGrids[0].RefreshSuspended, 'Early API cancellation changed suspension');
end;

procedure TWorkerCheck.Completed(Sender: TObject);
var I: Integer;
begin
  for I := 0 to High(FGrids) do
    if Sender = FGrids[I] then begin
      Inc(FCompletions[I]);
      Exit;
    end;
  raise Exception.Create('A disposed grid receiver was invoked');
end;

procedure TWorkerCheck.DisableTimers(AOwner: TComponent);
var I: Integer;
begin
  for I := 0 to AOwner.ComponentCount-1 do begin
    if AOwner.Components[I] is TTimer then TTimer(AOwner.Components[I]).Enabled := False;
    DisableTimers(AOwner.Components[I]);
  end;
end;

procedure TWorkerCheck.Pump;
var I: Integer;
begin
  for I := 0 to High(FGrids) do
    if Assigned(FGrids[I]) then DisableTimers(FGrids[I]);
  CheckSynchronize(0);
  Application.ProcessMessages;
  Sleep(1);
end;

procedure TWorkerCheck.AwaitCompletion(AIndex, AExpected: Integer);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64+2000;
  repeat
    Pump;
    if not FGrids[AIndex].IsUpdating and (FCompletions[AIndex] = AExpected) then Exit;
  until GetTickCount64 >= LUntil;
  raise Exception.Create(Format('Early cancellation stranded grid: busy=%s calls=%d completions=%d expected=%d',
    [BoolToStr(FGrids[AIndex].IsUpdating,True),FSources[AIndex].Calls,FCompletions[AIndex],AExpected]));
end;

procedure TWorkerCheck.CheckResume;
var LPrevious: TNotifyEvent; LCalls,LCompleted,LForced: Integer;
begin
  LCalls := FSources[0].Calls;
  LCompleted := FCompletions[0];
  LForced := FForcedCancellations;
  FGrids[0].FetchDataInThread := True;
  LPrevious := VisualGridBeforeWorkerStart;
  VisualGridBeforeWorkerStart := BeforeWorkerStart;
  FCancelNextWorker := True;
  try
    FGrids[0].RefreshGrid;
    Require(FForcedCancellations = LForced+1,'The constructor did not force the pre-start cancellation');
    FGrids[0].RefreshSuspended := True;
    FGrids[0].PageIndex := 4;
    FGrids[0].RefreshSuspended := False;
    Require(FSources[0].Calls = LCalls,'The pre-start-cancelled worker entered its provider');
  finally
    FCancelNextWorker := False;
    VisualGridBeforeWorkerStart := LPrevious;
  end;
  AwaitCompletion(0,LCompleted+1);
  Require((FSources[0].Calls = LCalls+1) and (FSources[0].LastPage = 4) and
    (FGrids[0].PageIndex = 4) and (FGrids[0].RowCount = 10),
    'Early cancellation did not publish exactly one latest admitted page');
  Pump;
  Require((FSources[0].Calls = LCalls+1) and (FCompletions[0] = LCompleted+1),
    'Retiring an early-cancelled worker published an extra query or completion');
end;

procedure TWorkerCheck.CheckClose;
var LPrevious: TNotifyEvent; LCalls,LSecondCalls,LSecondCompleted,LForced: Integer;
begin
  LCalls := FSources[0].Calls;
  LSecondCalls := FSources[1].Calls;
  LSecondCompleted := FCompletions[1];
  LForced := FForcedCancellations;
  FGrids[1].FetchDataInThread := True;
  LPrevious := VisualGridBeforeWorkerStart;
  VisualGridBeforeWorkerStart := BeforeWorkerStart;
  try
    FGrids[1].RefreshGrid;
    FCancelNextWorker := True;
    FGrids[0].RefreshGrid;
    Require(FForcedCancellations = LForced+1,'Receiver disposal did not force the pre-start cancellation');
    FGrids[0].RefreshSuspended := True;
    // The independent provider may finish; its UI completion remains queued.
    Require((FSources[0].Calls = LCalls) and (FCompletions[1] = LSecondCompleted),
      'The cancelled provider ran or the independent receiver was pumped early');
    FreeAndNil(FGrids[0]);
  finally
    FCancelNextWorker := False;
    VisualGridBeforeWorkerStart := LPrevious;
  end;
  AwaitCompletion(1,LSecondCompleted+1);
  Require((FSources[0].Calls = LCalls) and (FSources[1].Calls = LSecondCalls+1),
    'Early-cancelled disposal fetched stale data or lost another receiver');
  Pump;
end;

procedure TWorkerCheck.CheckActiveCancelBeforeStart;
var
  LPrevious: TNotifyEvent;
  LCalls, LCompleted: Integer;
begin
  LCalls := FSources[0].Calls;
  LCompleted := FCompletions[0];
  FGrids[0].FetchDataInThread := True;
  LPrevious := VisualGridBeforeWorkerStart;
  VisualGridBeforeWorkerStart := BeforeActiveCancelStart;
  FCancelNextThroughApi := True;
  try
    FGrids[0].RefreshGrid;
    Require(FApiPreStartCalls = 1, 'The public API was not invoked before Start');
    Require(FGrids[0].IsUpdating and not FGrids[0].RefreshSuspended,
      'Early cancellation synchronously retired or suspended its worker');
  finally
    FCancelNextThroughApi := False;
    VisualGridBeforeWorkerStart := LPrevious;
  end;
  AwaitCompletion(0, LCompleted + 1);
  Require((FSources[0].Calls = LCalls + 1) and (FSources[0].LastPage = 6) and
    (FGrids[0].PageIndex = 6) and (FGrids[0].RowCount = 10) and
    (Integer(TDataRowData(FGrids[0].Rows[0])['Id']) = 60),
    'Pre-start API cancellation entered its old provider or lost the latest page');
  Pump;
  Require((FSources[0].Calls = LCalls + 1) and
    (FCompletions[0] = LCompleted + 1), 'Pre-start cancellation published extra data');
end;

procedure TWorkerCheck.Run;
begin
  CheckResume;
  CheckClose;
end;
{$ENDIF}

procedure CheckVisualGridEarlyWorkerCancellation;
{$IF defined(MSWINDOWS) and defined(VISUALGRID_TEST_WORKER_START)}
var LCheck: TWorkerCheck;
begin
  LCheck := TWorkerCheck.Create;
  try
    LCheck.Run;
  finally
    LCheck.Free;
  end;
end;
{$ELSE}
begin
  raise ENotSupportedException.Create('This native worker regression requires Windows and VISUALGRID_TEST_WORKER_START');
end;
{$ENDIF}

procedure CheckVisualGridEarlyActiveCancellation;
{$IF defined(MSWINDOWS) and defined(VISUALGRID_TEST_WORKER_START)}
var
  LCheck: TWorkerCheck;
begin
  LCheck := TWorkerCheck.Create;
  try
    LCheck.CheckActiveCancelBeforeStart;
  finally
    LCheck.Free;
  end;
end;
{$ELSE}
begin
  raise ENotSupportedException.Create('The direct pre-start API check requires Windows and VISUALGRID_TEST_WORKER_START');
end;
{$ENDIF}

type
  TActiveCancelSource = class(TCustomDataSource<Integer>)
  private
    FReadPrefix: Integer;
  protected
    function GetColumns: TDataColumns; override;
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer;
      const AName: String): Variant; override;
    procedure FetchAll(const AItems: TList<Integer>); override;
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
      var ATable: TDataTable): TPageFetchResult; override;
  public
    Prefix, Calls, HoldNext: LongInt;
    Cooperative, RaiseObsoleteError: Boolean;
    Entered, ReleaseRead, CancelObserved: TEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TActiveCancelObserver = class(TComponent)
  public
    Grid: TVisualGrid;
    Completions: Integer;
    PublishedOldQuery: Boolean;
    procedure Completed(Sender: TObject);
  end;

procedure RequireActiveCancellation(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create('Active fetch cancellation: ' + AMessage);
end;

procedure PumpActiveCancellation(AMilliseconds: Cardinal);
var
  LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    CheckSynchronize(0);
    Application.ProcessMessages;
    Sleep(1);
  until GetTickCount64 >= LUntil;
end;

constructor TActiveCancelSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, True, '');
  CancelObserved := TEvent.Create(nil, True, False, '');
end;

destructor TActiveCancelSource.Destroy;
begin
  CancelObserved.Free;
  ReleaseRead.Free;
  Entered.Free;
  inherited Destroy;
end;

function TActiveCancelSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Id', True));
end;

function TActiveCancelSource.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer;
  const AName: String): Variant;
begin
  Result := AItem;
end;

procedure TActiveCancelSource.FetchAll(const AItems: TList<Integer>);
var
  I: Integer;
begin
  for I := 0 to 59 do
    AItems.Add(FReadPrefix + I);
end;

function TActiveCancelSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams;
  var ATable: TDataTable): TPageFetchResult;
begin
  FReadPrefix := InterlockedCompareExchange(Prefix, 0, 0);
  InterlockedIncrement(Calls);
  if InterlockedExchange(HoldNext, 0) <> 0 then
  begin
    Entered.SetEvent;
    if Cooperative then
    begin
      while ReleaseRead.WaitFor(10) <> wrSignaled do
        if TThread.CheckTerminated then
        begin
          CancelObserved.SetEvent;
          Abort;
        end;
    end
    else
      RequireActiveCancellation(ReleaseRead.WaitFor(5000) = wrSignaled,
        'held provider release timed out');
    if RaiseObsoleteError then
      raise Exception.Create('Obsolete cancelled provider sentinel error');
  end;
  Result := inherited FetchPage(AParams, ATable);
end;

procedure TActiveCancelObserver.Completed(Sender: TObject);
begin
  Inc(Completions);
  if (Grid.RowCount > 0) and
    (Integer(TDataRowData(Grid.Rows[0])['Id']) >= 1000) and
    (Integer(TDataRowData(Grid.Rows[0])['Id']) < 2000) then
    PublishedOldQuery := True;
end;

procedure CheckVisualGridActiveCancellation(AScenario: TGridActiveCancellationScenario);
var
  LForm: TForm;
  LGrid: TVisualGrid;
  LSource: TActiveCancelSource;
  LObserver: TActiveCancelObserver;
  LCalls: Integer;
  LUntil, LStarted, LNewestInput: UInt64;
  LNotify: TDateTime;

  procedure CancelActive;
  begin
    {$IFNDEF VISUALGRID_ACTIVE_CANCEL_BASELINE}
    LGrid.CancelActiveFetch;
    {$ENDIF}
  end;

  procedure RequireOriginalRows(const AContext: String);
  begin
    RequireActiveCancellation((LGrid.RowCount = 10) and
      (Integer(TDataRowData(LGrid.Rows[0])['Id']) = 0),
      AContext + ': accepted rows were lost or replaced by the cancelled query');
  end;

begin
  LForm := TForm.CreateNew(nil);
  LSource := nil;
  try
    LForm.SetBounds(40, 50, 640, 440);
    LSource := TActiveCancelSource.Create(LForm);
    LGrid := TVisualGrid.Create(LForm);
    LGrid.Parent := LForm;
    LGrid.Align := alClient;
    LGrid.FetchDataInThread := False;
    LGrid.PageMode := vpmManual;
    LGrid.ManualPageSize := 10;
    LGrid.CanSearch := False;
    LGrid.AddColumn('Id').Binding := 'Id';
    LGrid.DataSource := LSource;
    LObserver := TActiveCancelObserver.Create(LForm);
    LObserver.Grid := LGrid;
    LGrid.OnFinishedUpdating := LObserver.Completed;
    LForm.Show;
    PumpActiveCancellation(50);
    RequireOriginalRows('initial page');
    LCalls := LSource.Calls;
    LNotify := LGrid.RefreshThrottle.LastClientNotify;
    CancelActive;
    RequireActiveCancellation((LSource.Calls = LCalls) and not LGrid.IsUpdating and
      not LGrid.RefreshSuspended and not LGrid.RefreshThrottle.Waiting and
      (LGrid.RefreshThrottle.LastClientNotify = LNotify),
      'an idle cancellation changed refresh state');

    LSource.Cooperative := AScenario = acsQuietAdmission;
    LSource.RaiseObsoleteError := AScenario = acsCancelledError;
    if AScenario = acsQuietAdmission then
    begin
      LGrid.RefreshThrottle.Interval := TTimeSpan.FromSeconds(5);
      LGrid.RefreshThrottle.Mode := temNotifyOnEventBurstStartAndFinished;
    end;
    InterlockedExchange(LSource.Prefix, 1000);
    LSource.ReleaseRead.ResetEvent;
    InterlockedExchange(LSource.HoldNext, 1);
    LGrid.FetchDataInThread := True;
    LGrid.RefreshGrid;
    RequireActiveCancellation(LSource.Entered.WaitFor(1000) = wrSignaled,
      'the real worker never entered its held provider');
    if AScenario = acsHeldProvider then
      LGrid.ShowLoading('Manual owner', 'Retain this card', nil, True);
    LNotify := LGrid.RefreshThrottle.LastClientNotify;
    LStarted := GetTickCount64;
    CancelActive;
    RequireActiveCancellation(GetTickCount64 - LStarted < 250,
      'CancelActiveFetch waited for the provider');
    RequireActiveCancellation(LGrid.IsUpdating and not LGrid.RefreshSuspended and
      (LGrid.RefreshThrottle.LastClientNotify = LNotify),
      'cancellation retired synchronously or notified/changed refresh policy');
    RequireActiveCancellation(LGrid.RefreshThrottle.Waiting = (AScenario = acsQuietAdmission),
      'cancellation changed the existing Waiting state');
    RequireOriginalRows('while cancellation is pending');
    if AScenario = acsHeldProvider then
      RequireActiveCancellation(LGrid.LoadingVisible and
        (TLabel(LGrid.FindComponent('LoadingStatus')).Caption = 'Manual owner'),
        'cancellation changed the caller-owned loading card');

    if AScenario = acsCancelledError then
    begin
      // Inspect the cancelled error before scheduling a replacement which could
      // otherwise hide a leaked error card immediately in BeginLoading.
      LSource.ReleaseRead.SetEvent;
      LUntil := GetTickCount64 + 2000;
      while LGrid.IsUpdating and (GetTickCount64 < LUntil) do
        PumpActiveCancellation(10);
      RequireActiveCancellation(not LGrid.IsUpdating, 'cancelled failing worker did not retire');
      RequireOriginalRows('after cancelled provider error');
      RequireActiveCancellation((LObserver.Completions = 0) and not LObserver.PublishedOldQuery,
        'cancelled failing worker published a completion or old rows');
      RequireActiveCancellation(not TPanel(LGrid.FindComponent('LoadingOverlay')).Visible and
        (Pos('Obsolete cancelled provider sentinel error',
         TLabel(LGrid.FindComponent('LoadingStatus')).Caption) = 0),
        'cancelled ordinary exception leaked its obsolete error card');
    end;

    InterlockedExchange(LSource.Prefix, 2000);
    LNewestInput := GetTickCount64;
    LGrid.PageIndex := 2;
    if AScenario in [acsHeldProvider, acsCancelledError] then
      LSource.ReleaseRead.SetEvent
    else
    begin
      RequireActiveCancellation(LSource.CancelObserved.WaitFor(1000) = wrSignaled,
        'the cooperative provider did not receive cancellation');
      LUntil := GetTickCount64 + 2000;
      while LGrid.IsUpdating and (GetTickCount64 < LUntil) do
        PumpActiveCancellation(10);
      RequireActiveCancellation(not LGrid.IsUpdating and LGrid.RefreshThrottle.Waiting,
        'cancelled worker did not retire independently of its pending quiet interval');
      RequireOriginalRows('after cooperative retirement');
      RequireActiveCancellation((LObserver.Completions = 0) and
        (LSource.Calls = LCalls + 1), 'cancelled query published or latest input fetched early');
      LNotify := LGrid.RefreshThrottle.LastClientNotify;
      CancelActive;
      RequireActiveCancellation((LGrid.RefreshThrottle.LastClientNotify = LNotify) and
        LGrid.RefreshThrottle.Waiting, 'idle cancellation reset or ended the pending wait');
      while GetTickCount64 - LNewestInput < 4700 do
      begin
        PumpActiveCancellation(10);
        RequireActiveCancellation((LObserver.Completions = 0) and
          (LSource.Calls = LCalls + 1), 'newest query bypassed five quiet seconds');
      end;
    end;
    LUntil := GetTickCount64 + 2500;
    while ((LObserver.Completions <> 1) or LGrid.IsUpdating or
      LGrid.RefreshThrottle.Waiting) and (GetTickCount64 < LUntil) do
      PumpActiveCancellation(10);
    RequireActiveCancellation((LObserver.Completions = 1) and not LGrid.IsUpdating and
      not LGrid.RefreshThrottle.Waiting and not LObserver.PublishedOldQuery,
      'cancelled result published or latest query did not complete exactly once');
    RequireActiveCancellation((LSource.Calls = LCalls + 2) and (LGrid.PageIndex = 2) and
      (LGrid.RowCount = 10) and
      (Integer(TDataRowData(LGrid.Rows[0])['Id']) = 2020),
      'latest pending page/source configuration was lost');
    if AScenario = acsQuietAdmission then
      RequireActiveCancellation(GetTickCount64 - LNewestInput >= 4980,
        'latest publication was earlier than the five-second quiet contract');
    if AScenario = acsHeldProvider then
    begin
      RequireActiveCancellation(LGrid.LoadingVisible and
        (TLabel(LGrid.FindComponent('LoadingStatus')).Caption = 'Manual owner'),
        'worker retirement removed the manual card');
      LGrid.HideLoading;
      RequireActiveCancellation(not TPanel(LGrid.FindComponent('LoadingOverlay')).Visible,
        'completed cancellation left an obsolete automatic/error card');
    end;
    PumpActiveCancellation(50);
    RequireActiveCancellation((LSource.Calls = LCalls + 2) and (LObserver.Completions = 1),
      'retiring the cancelled worker caused another query/publication');
  finally
    if Assigned(LSource) then
      LSource.ReleaseRead.SetEvent;
    LForm.Free;
  end;
end;

end.
