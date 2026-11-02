program GridThrottleProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE.
  Runs real VCL grid controls and generated data on the caller's desktop. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Variants,
  System.SyncObjs, System.Generics.Collections, Vcl.Forms, Vcl.Controls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics,
  UCommon, UCommon.UI, UCommon.Data, UVisualGrid, UVisualGrid.Worker.Checks;

type
  TProbeSource = class(TCustomDataSource<Integer>)
  public
    PageCalls: LongInt;
    ItemCount: Integer;
    LastParams: TPageFetchParams;
    Entered, ReleaseRead, CancelObserved: TEvent;
    BlockRead, FailRead, CooperativeRead: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColumns: TDataColumns; override;
    function GetItemField(const AItem: Integer; const AName: String): Variant; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
    function FetchPage(const AParams: TPageFetchParams;
      var ADataTable: TDataTable): TPageFetchResult; override;
  end;

  TProbeGrid = class(TVisualGrid)
  public
    procedure Search(const AText: String);
  end;

  TGridProbe = class(TComponent)
  private
    FForm: TForm;
    FGrids: array[0..1] of TProbeGrid;
    FSources: array[0..1] of TProbeSource;
    FStarts, FEnds, FAdmissions, FCompletions: array[0..1] of Integer;
    FRequestOnCompletion: Boolean;
    FFailure: String;
    FLogPath: String;
    procedure Log(const AText: String);
    procedure Require(AValue: Boolean; const AMessage: String);
    procedure OnException(Sender: TObject; E: Exception);
    function GridIndex(Sender: TObject; AThrottle: Boolean): Integer;
    procedure WaitingChanged(Sender: TObject);
    procedure Admitted0(Sender: TObject);
    procedure Admitted1(Sender: TObject);
    procedure Completed(Sender: TObject);
    procedure Pump(AMilliseconds: Cardinal);
    procedure AwaitIdle(AIndex: Integer; AQuiet: Boolean = True);
    procedure AwaitEntered(AIndex: Integer);
    procedure Reset;
    procedure EnableThrottle(AIndex: Integer);
    function Calls(AIndex: Integer): Integer;
    procedure CheckMixedRequestsAndIndependentTimers;
    procedure CheckBusySingleAndLatestParameters;
    procedure CheckHeldResultAndCompletionInput;
    procedure CheckCancelModeAndDisposal;
    procedure CheckParentlessAndHiddenRequests;
    procedure CheckWorkerNotificationWithoutTimers;
    procedure CheckRefreshSuspension;
    procedure CheckQueuedLoading;
  public
    constructor Create(AForm: TForm; const ALogPath: String); reintroduce;
    destructor Destroy; override;
    procedure Run;
  end;

constructor TProbeSource.Create(AOwner: TComponent);
begin
  inherited;
  ItemCount := 1000;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, False, '');
  CancelObserved := TEvent.Create(nil, True, False, '');
end;

destructor TProbeSource.Destroy;
begin
  Entered.Free;
  ReleaseRead.Free;
  CancelObserved.Free;
  inherited;
end;

function TProbeSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Id', True));
end;

function TProbeSource.GetItemField(const AItem: Integer; const AName: String): Variant;
begin
  Result := AItem;
end;

procedure TProbeSource.FetchAll(const AContainer: TList<Integer>);
var I: Integer;
begin
  // Inherited FetchPage owns the real provider lock around this callback.
  if BlockRead and CooperativeRead then
    while ReleaseRead.WaitFor(10) <> wrSignaled do
      if TThread.CheckTerminated then begin
        CancelObserved.SetEvent;
        Abort;
      end;
  for I := 0 to ItemCount - 1 do AContainer.Add(I);
end;

function TProbeSource.FetchPage(const AParams: TPageFetchParams;
  var ADataTable: TDataTable): TPageFetchResult;
begin
  LastParams := AParams;
  InterlockedIncrement(PageCalls);
  Entered.SetEvent;
  if BlockRead and not CooperativeRead and (ReleaseRead.WaitFor(15000) <> wrSignaled) then
    raise Exception.Create('Owned native source gate timed out');
  if FailRead then raise Exception.Create('Owned cancelled provider failure');
  Result := inherited;
end;

procedure TProbeGrid.Search(const AText: String);
begin
  FSearchEdit.Text := AText;
  SearchButtonClick(FSearchButton);
end;

constructor TGridProbe.Create(AForm: TForm; const ALogPath: String);
var I: Integer;
begin
  inherited Create(nil);
  FForm := AForm;
  FLogPath := ALogPath;
  with TFileStream.Create(FLogPath, fmCreate) do Free;
  Application.OnException := OnException;
  for I := 0 to 1 do begin
    FSources[I] := TProbeSource.Create(Self);
    FGrids[I] := TProbeGrid.Create(nil);
    FGrids[I].Parent := FForm;
    FGrids[I].SetBounds(8 + I * 530, 8, 520, 600);
    FGrids[I].FetchDataInThread := False;
    FGrids[I].PageSize := 10;
    FGrids[I].Options := [vgoColAutoFill, vgoColSizing, vgoSortDirectionAllowNone,
      vgoAutoHidePaging, vgoAutoHideSearchPanel];
    FGrids[I].AddColumn('Id').Binding := 'Id';
    FGrids[I].Columns[0].Filters := [vgfNumericGT, vgfSortable];
    FGrids[I].SortMode := smSingleColumn;
    FGrids[I].OnFinishedUpdating := Completed;
    FGrids[I].RefreshThrottle.OnWaitingChanged.Add(WaitingChanged);
    FGrids[I].DataSource := FSources[I];
  end;
  FGrids[0].RefreshThrottle.Add(Admitted0);
  FGrids[1].RefreshThrottle.Add(Admitted1);
end;

destructor TGridProbe.Destroy;
var I: Integer;
begin
  Application.OnException := nil;
  for I := 0 to 1 do if Assigned(FSources[I]) then FSources[I].ReleaseRead.SetEvent;
  for I := 0 to 1 do begin
    FGrids[I].Free;
    FGrids[I] := nil;
  end;
  inherited;
end;

procedure TGridProbe.Log(const AText: String);
var F: TFileStream; B: TBytes;
begin
  F := TFileStream.Create(FLogPath, fmOpenReadWrite or fmShareDenyNone);
  try
    F.Position := F.Size;
    B := TEncoding.UTF8.GetBytes(UIntToStr(GetTickCount64) + ' ' + AText + #13#10);
    F.WriteBuffer(B[0], Length(B));
  finally F.Free; end;
end;

procedure TGridProbe.Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then raise Exception.Create(AMessage);
end;

procedure TGridProbe.OnException(Sender: TObject; E: Exception);
begin
  FFailure := E.ClassName + ': ' + E.Message;
  Log('ASYNC ' + FFailure + ' ExceptAddr=' + IntToHex(NativeUInt(ExceptAddr),16) +
    ' ImageBase=' + IntToHex(NativeUInt(HInstance),16) + ' grid0 NativeRow=' + IntToStr(FGrids[0].InternalDrawGrid.Row) +
    ' NativeRowCount=' + IntToStr(FGrids[0].InternalDrawGrid.RowCount) +
    ' DataRows=' + IntToStr(FGrids[0].RowCount) +
    ' selectionType=' + IntToStr(Ord(FGrids[0].SelectionType)));
end;

function TGridProbe.GridIndex(Sender: TObject; AThrottle: Boolean): Integer;
var I: Integer;
begin
  for I := 0 to 1 do
    if Assigned(FGrids[I]) and (((not AThrottle) and (Sender = FGrids[I])) or
      (AThrottle and (Sender = FGrids[I].RefreshThrottle))) then Exit(I);
  raise Exception.Create('Unexpected native grid event sender');
end;

procedure TGridProbe.WaitingChanged(Sender: TObject);
var I: Integer;
begin
  I := GridIndex(Sender, True);
  if FGrids[I].RefreshThrottle.Waiting then begin
    Inc(FStarts[I]);
    FGrids[I].ShowLoading('Please wait', 'Grid ' + IntToStr(I));
  end else begin
    Inc(FEnds[I]);
    FGrids[I].HideLoading;
  end;
  Require(FGrids[I].LoadingVisible = FGrids[I].RefreshThrottle.Waiting,
    'The sender grid card differs from its own Waiting state');
end;

procedure TGridProbe.Admitted0(Sender: TObject);
begin
  Inc(FAdmissions[0]);
end;

procedure TGridProbe.Admitted1(Sender: TObject);
begin
  Inc(FAdmissions[1]);
end;

procedure TGridProbe.Completed(Sender: TObject);
var I: Integer;
begin
  I := GridIndex(Sender, False);
  Inc(FCompletions[I]);
  if (I = 1) and FRequestOnCompletion then begin
    FRequestOnCompletion := False;
    FGrids[I].RefreshGrid;
  end;
end;

procedure TGridProbe.Pump(AMilliseconds: Cardinal);
var UntilTick: UInt64;
begin
  UntilTick := GetTickCount64 + AMilliseconds;
  repeat
    Application.ProcessMessages;
    CheckSynchronize(0);
    if FFailure <> '' then raise Exception.Create('Asynchronous native failure: ' + FFailure);
    Sleep(1);
  until GetTickCount64 >= UntilTick;
end;

procedure TGridProbe.AwaitIdle(AIndex: Integer; AQuiet: Boolean);
var UntilTick: UInt64;
begin
  UntilTick := GetTickCount64 + 8000;
  repeat
    Pump(10);
    if not FGrids[AIndex].IsUpdating and
      ((not AQuiet) or not FGrids[AIndex].RefreshThrottle.Waiting) then begin
      Pump(60);
      if not FGrids[AIndex].IsUpdating and
        ((not AQuiet) or not FGrids[AIndex].RefreshThrottle.Waiting) then Exit;
    end;
  until GetTickCount64 >= UntilTick;
  raise Exception.Create('The actual grid did not settle: ' + IntToStr(AIndex));
end;

procedure TGridProbe.AwaitEntered(AIndex: Integer);
var UntilTick: UInt64;
begin
  UntilTick := GetTickCount64 + 1500;
  while FSources[AIndex].Entered.WaitFor(0) <> wrSignaled do begin
    Require(GetTickCount64 < UntilTick, 'Actual native fetch did not enter');
    Pump(5);
  end;
end;

procedure TGridProbe.Reset;
var I: Integer;
begin
  FRequestOnCompletion := False;
  for I := 0 to 1 do begin
    FSources[I].ReleaseRead.SetEvent;
    FGrids[I].RefreshThrottle.Mode := temNone;
    AwaitIdle(I);
    FSources[I].BlockRead := False;
    FSources[I].FailRead := False;
    FSources[I].CooperativeRead := False;
    FGrids[I].RefreshSuspended := False;
    FSources[I].ItemCount := 1000;
    FSources[I].InvalidateSnapshot;
    FGrids[I].FetchDataInThread := False;
    FGrids[I].Visible := True;
    FGrids[I].AutoPageSize := False;
    FGrids[I].Columns[0].SortDirection := sdNone;
    FGrids[I].Search('');
    FGrids[I].PageSize := 10;
    FGrids[I].PageIndex := 0;
    FGrids[I].RefreshGrid;
    AwaitIdle(I);
    FStarts[I] := 0; FEnds[I] := 0; FAdmissions[I] := 0; FCompletions[I] := 0;
  end;
end;

procedure TGridProbe.EnableThrottle(AIndex: Integer);
begin
  FGrids[AIndex].RefreshThrottle.Interval := TTimeSpan.FromSeconds(5);
  FGrids[AIndex].RefreshThrottle.Mode := temNotifyOnEventBurstStartAndFinished;
end;

function TGridProbe.Calls(AIndex: Integer): Integer;
begin
  Result := InterlockedCompareExchange(FSources[AIndex].PageCalls, 0, 0);
end;

procedure TGridProbe.CheckMixedRequestsAndIndependentTimers;
var Before0, Before1, Capacity, I: Integer;
    Last0, Last1, UntilTick: UInt64;
    B: TBitmap;
begin
  Reset;
  FGrids[1].AutoPageSize := True;
  AwaitIdle(1);
  EnableThrottle(0); EnableThrottle(1);
  Before0 := Calls(0); Before1 := Calls(1);
  FGrids[0].RefreshGrid; FGrids[1].RefreshGrid;
  Require((Calls(0) = Before0 + 1) and (Calls(1) = Before1 + 1), 'Each free grid must lead immediately');
  FGrids[0].PageSize := 25; FGrids[0].PageIndex := 2;
  FGrids[0].Search('>400'); FGrids[0].Columns[0].SortDirection := sdDescending;
  FGrids[1].Height := FGrids[1].Height + 40;
  Pump(50);
  Capacity := FGrids[1].PageSize;
  Last1 := GetTickCount64;
  for I := 0 to 9 do begin
    Pump(100);
    FGrids[0].RefreshGrid;
  end;
  Last0 := GetTickCount64;
  Require((Calls(0) = Before0 + 1) and (Calls(1) = Before1 + 1),
    'Mixed refresh/page/filter/sort/resize inputs bypassed the owned quiet timer');
  Require(FGrids[0].LoadingVisible and FGrids[1].LoadingVisible, 'Independent custom cards are missing');
  Require(TLabel(FGrids[0].FindComponent('LoadingSubtitle')).Caption = 'Grid 0', 'Wrong custom card subtitle');
  B := FForm.GetFormImage;
  try B.SaveToFile(ChangeFileExt(FLogPath, '-waiting.bmp')); finally B.Free; end;
  UntilTick := Last1 + 6500;
  while FGrids[1].RefreshThrottle.Waiting do begin
    Require(GetTickCount64 < UntilTick, 'The independent grid timer did not finish');
    Pump(10);
  end;
  Require(GetTickCount64 - Last0 < 5000, 'Independent timer assertion missed its observation window');
  Require(FGrids[0].RefreshThrottle.Waiting and not FGrids[1].LoadingVisible,
    'One grid finishing ended the other grid waiting state');
  Require((Calls(0) = Before0 + 1) and (Calls(1) = Before1 + 2), 'Wrong independent leading/final query counts');
  AwaitIdle(0); AwaitIdle(1);
  Require(Calls(0) = Before0 + 2, 'Mixed inputs must issue exactly one final query');
  Require((FSources[0].LastParams.PageSize = 25) and (FSources[0].LastParams.PageIndex = 2), 'Latest page parameters were lost');
  Require(Integer(FGrids[0].Cells[0,0]) = 949, 'Latest search and descending sort were not applied');
  Require((FSources[1].LastParams.PageSize = Capacity) and (FGrids[1].RowCount = Capacity), 'Resize did not retain the latest capacity');
  Require((FStarts[0] = 1) and (FEnds[0] = 1) and (FStarts[1] = 1) and (FEnds[1] = 1), 'Waiting edges restarted internally');
  Pump(300);
  Require((Calls(0) = Before0 + 2) and (Calls(1) = Before1 + 2), 'Settled result chrome restarted a query');
  Log('PASS mixed RefreshGrid/page/filter/sort/resize; latest rows/params; independent five-second timers/cards');
end;

procedure TGridProbe.CheckBusySingleAndLatestParameters;
var Before0, Before1, I: Integer;
begin
  Reset;
  Before0 := Calls(0); Before1 := Calls(1);
  for I := 0 to 1 do begin
    FGrids[I].FetchDataInThread := True;
    FSources[I].Entered.ResetEvent; FSources[I].ReleaseRead.ResetEvent; FSources[I].BlockRead := True;
    FGrids[I].RefreshGrid; AwaitEntered(I);
    FAdmissions[I] := 0; // Exclude the deliberately started temNone worker.
    EnableThrottle(I); FGrids[I].RefreshGrid;
  end;
  FGrids[1].Visible := False;
  FGrids[1].PageSize := 17; FGrids[1].PageIndex := 3;
  Pump(200);
  for I := 0 to 1 do FSources[I].ReleaseRead.SetEvent;
  AwaitIdle(0, False); AwaitIdle(1, False);
  Require(FGrids[0].RefreshThrottle.Waiting and FGrids[1].RefreshThrottle.Waiting, 'Busy inputs lost their quiet wait');
  Require((Calls(0) = Before0 + 1) and (Calls(1) = Before1 + 1), 'Older worker completion admitted a waiting request');
  FGrids[1].Visible := True;
  AwaitIdle(0); AwaitIdle(1);
  Require((Calls(0) = Before0 + 2) and (Calls(1) = Before1 + 2), 'Quiet finish failed to release one latest busy request');
  Require((FAdmissions[0] = 1) and (FStarts[0] = 1) and (FEnds[0] = 1), 'Single busy leading input delivered twice');
  Require((FSources[1].LastParams.PageSize = 17) and (FSources[1].LastParams.PageIndex = 3) and
    (Integer(FGrids[1].Cells[0,0]) = 51), 'Hidden busy inputs lost the latest page');
  Log('PASS single busy leading request; no replay before quiet; hidden latest page parameters');
end;

procedure TGridProbe.CheckHeldResultAndCompletionInput;
var Before0, Before1, CompletionsBefore0: Integer; UntilTick: UInt64;
begin
  Reset;
  FGrids[0].AutoPageSize := True;
  AwaitIdle(0);
  // Empty publication must safely clamp a previously selected native row.
  FGrids[0].SelectionType := stNone;
  FGrids[0].SelectionType := stRow;
  FGrids[0].InternalDrawGrid.Row := FGrids[0].InternalDrawGrid.FixedRows + 2;
  Require(Length(FGrids[0].SelectedRows) = 1, 'Empty-result case needs a valid prior selection');
  FSources[0].ItemCount := 0; FSources[0].InvalidateSnapshot;
  CompletionsBefore0 := FCompletions[0];
  EnableThrottle(0); EnableThrottle(1);
  Before0 := Calls(0); Before1 := Calls(1);
  FGrids[0].FetchDataInThread := True;
  FSources[0].Entered.ResetEvent; FSources[0].ReleaseRead.ResetEvent; FSources[0].BlockRead := True;
  FGrids[0].RefreshGrid; AwaitEntered(0);
  FGrids[0].RefreshGrid;
  FRequestOnCompletion := True;
  FGrids[1].RefreshGrid;
  UntilTick := GetTickCount64 + 6500;
  while FGrids[0].RefreshThrottle.Waiting or FGrids[1].RefreshThrottle.Waiting do begin
    Require(GetTickCount64 < UntilTick, 'Held query prevented its own quiet lifecycle finish');
    Pump(10);
  end;
  Require(FGrids[0].IsUpdating and not FGrids[0].LoadingVisible and
    not FGrids[0].InternalDrawGrid.Enabled, 'Own timer must clear manual card while automatic fetch remains gated');
  Require(Calls(0) = Before0 + 1, 'Final admission launched a second query while the old worker was active');
  Require(Calls(1) = Before1 + 2, 'A client request inside OnFinishedUpdating was not admitted as input');
  FSources[0].ReleaseRead.SetEvent;
  AwaitIdle(0); AwaitIdle(1);
  Require((Calls(0) = Before0 + 2) and (FGrids[0].RowCount = 0), 'Held final page replay did not publish the empty result');
  Log(Format('Empty publication starts=%d ends=%d completions=%d baseline=%d nativeRow=%d nativeCount=%d dataRows=%d',
    [FStarts[0],FEnds[0],FCompletions[0],CompletionsBefore0,FGrids[0].InternalDrawGrid.Row,
     FGrids[0].InternalDrawGrid.RowCount,FGrids[0].RowCount]));
  Require((FStarts[0] = 1) and (FEnds[0] = 1) and (FCompletions[0] = CompletionsBefore0 + 2),
    'Worker replay/result chrome became another input');
  Require((FGrids[0].InternalDrawGrid.Row < FGrids[0].InternalDrawGrid.RowCount) and
    (Length(FGrids[0].SelectedRows) = 0), 'Empty result retained an invalid native current row/selection');
  Pump(350);
  Require((Calls(0) = Before0 + 2) and (Calls(1) = Before1 + 2), 'Empty result chrome repeatedly fetched');
  FGrids[0].RefreshThrottle.Mode := temNone;
  FSources[0].BlockRead := False; FSources[0].ItemCount := 1; FSources[0].InvalidateSnapshot;
  FGrids[0].RefreshGrid; AwaitIdle(0);
  Require((FGrids[0].RowCount = 1) and (Integer(FGrids[0].Cells[0,0]) = 0) and
    (FGrids[0].InternalDrawGrid.Row < FGrids[0].InternalDrawGrid.RowCount),
    'A one-row result after the empty page retained invalid native coordinates');
  Log('PASS held worker finishes Waiting independently; latest replay; selected-to-empty-to-one native rows; result chrome; client completion input');
end;

procedure TGridProbe.CheckCancelModeAndDisposal;
var Before0, Before1, BeforeDisposed, I: Integer;
    DisposedGrid: TProbeGrid; DisposedSource: TProbeSource;
begin
  Reset;
  Before0 := Calls(0); Before1 := Calls(1);
  for I := 0 to 1 do begin
    FGrids[I].FetchDataInThread := True;
    FSources[I].Entered.ResetEvent; FSources[I].ReleaseRead.ResetEvent; FSources[I].BlockRead := True;
    FGrids[I].RefreshGrid; AwaitEntered(I);
    FAdmissions[I] := 0; // Exclude the deliberately started temNone worker.
    EnableThrottle(I); FGrids[I].RefreshGrid;
  end;
  FGrids[0].RefreshThrottle.Cancel;
  FGrids[1].RefreshThrottle.Mode := temNone;
  for I := 0 to 1 do FSources[I].ReleaseRead.SetEvent;
  AwaitIdle(0); AwaitIdle(1);
  DisposedSource := TProbeSource.Create(Self);
  DisposedGrid := TProbeGrid.Create(nil);
  try
    DisposedGrid.Parent := FForm; DisposedGrid.SetBounds(0,0,200,120);
    DisposedGrid.PageSize := 10; DisposedGrid.FetchDataInThread := True;
    DisposedGrid.AddColumn('Id').Binding := 'Id';
    DisposedSource.BlockRead := True; DisposedGrid.DataSource := DisposedSource;
    Require(DisposedSource.Entered.WaitFor(1000) = wrSignaled, 'Owned disposal source did not enter');
    DisposedGrid.RefreshThrottle.Interval := TTimeSpan.FromSeconds(5);
    DisposedGrid.RefreshThrottle.Mode := temNotifyOnEventBurstStartAndFinished;
    DisposedGrid.RefreshGrid; DisposedGrid.RefreshThrottle.Cancel;
    BeforeDisposed := DisposedSource.PageCalls;
  finally
    DisposedSource.ReleaseRead.SetEvent;
    DisposedGrid.Free;
  end;
  Pump(5200);
  Require((Calls(0) = Before0 + 1) and (Calls(1) = Before1 + 1), 'Cancel/mode change released a pending request or result-layout replay');
  Require(DisposedSource.PageCalls = BeforeDisposed, 'Disposal admitted cancelled data work');
  Require(not FGrids[0].LoadingVisible and not FGrids[1].LoadingVisible, 'Cancel/mode change left a manual card');
  FGrids[0].RefreshGrid; AwaitIdle(0, False);
  Require(Calls(0) = Before0 + 2, 'Fresh input after Cancel did not lead immediately');
  FGrids[0].RefreshThrottle.Cancel;
  FGrids[1].RefreshGrid; AwaitIdle(1);
  Require(Calls(1) = Before1 + 2, 'Fresh non-throttled input after mode change failed');
  Log('PASS Cancel/mode change/disposal admit no cancelled page; fresh input still leads');
end;

procedure TGridProbe.CheckParentlessAndHiddenRequests;
var Before0, Before1: Integer;
begin
  Reset;
  FGrids[0].FetchDataInThread := True;
  FGrids[1].FetchDataInThread := True;
  EnableThrottle(0); EnableThrottle(1);
  Before0 := Calls(0); Before1 := Calls(1);
  // The wallet may detach an existing grid while showing a first-account view.
  // Admission and its owned timer must still accept requests in that state.
  FGrids[0].SelectionType := stNone;
  FGrids[0].SelectionType := stRow;
  FGrids[0].InternalDrawGrid.Row := FGrids[0].InternalDrawGrid.FixedRows + 2;
  Require(Length(FGrids[0].SelectedRows) = 1, 'The pre-detach grid must have a valid selected row');
  FGrids[0].Parent := nil;
  FGrids[1].Visible := False;
  FGrids[0].RefreshGrid; FGrids[1].RefreshGrid;
  Pump(300);
  Require(FGrids[0].RefreshThrottle.Waiting and FGrids[1].RefreshThrottle.Waiting,
    'Parentless/hidden refresh inputs were not admitted');
  Require((FAdmissions[0] = 1) and (FAdmissions[1] = 1), 'Parentless/hidden leading admission was vetoed');
  FGrids[0].RefreshGrid; FGrids[1].RefreshGrid;
  Pump(350);
  Require((Calls(0) <= Before0 + 1) and (Calls(1) <= Before1 + 1),
    'A detached/hidden waiting request queried before quiet');
  FGrids[0].Parent := FForm;
  FGrids[1].Visible := True;
  Require(FGrids[0].RefreshThrottle.Waiting and FGrids[1].RefreshThrottle.Waiting,
    'Reparenting or showing a grid prematurely ended its wait');
  AwaitIdle(0); AwaitIdle(1);
  Require((Calls(0) >= Before0 + 1) and (Calls(0) <= Before0 + 2) and
    (Calls(1) >= Before1 + 1) and (Calls(1) <= Before1 + 2),
    'Parentless/hidden requests were lost or duplicated on reattachment');
  Require((FGrids[0].RowCount = 10) and (FGrids[1].RowCount = 10) and
    (Integer(FGrids[0].Cells[0,0]) = 0) and (Integer(FGrids[1].Cells[0,0]) = 0),
    'Reattached grids did not render their admitted rows');
  Require((FStarts[0] = 1) and (FEnds[0] = 1) and (FStarts[1] = 1) and (FEnds[1] = 1),
    'Detaching a grid restarted its logical burst');
  Log('PASS parentless/hidden refresh admission/timer; no native parent exception; latest rows on reattachment');
end;
procedure TGridProbe.CheckWorkerNotificationWithoutTimers;
var LBefore, LCompleted: Integer; LDeadline: UInt64;
begin
  Reset;
  FGrids[0].FetchDataInThread := True;
  FSources[0].Entered.ResetEvent;
  FSources[0].ReleaseRead.ResetEvent;
  FSources[0].BlockRead := True;
  LBefore := Calls(0);
  LCompleted := FCompletions[0];
  FGrids[0].RefreshGrid;
  Require(FSources[0].Entered.WaitFor(1500) = wrSignaled, 'Notification-only fetch did not enter');
  Require(FGrids[0].IsUpdating, 'Notification-only fetch was not held');
  FSources[0].ReleaseRead.SetEvent;
  LDeadline := GetTickCount64 + 2000;
  repeat
    // Deliberately do not dispatch the Windows message queue: a WM_TIMER
    // completion poll cannot satisfy this assertion. Only queued worker
    // notification via the existing main-thread dispatcher may publish.
    CheckSynchronize(10);
    if not FGrids[0].IsUpdating then Break;
    Sleep(1);
  until GetTickCount64 >= LDeadline;
  Require(not FGrids[0].IsUpdating, 'Worker completion still requires a native timer');
  Require((Calls(0) = LBefore + 1) and (FCompletions[0] = LCompleted + 1),
    'Notification-only completion did not publish exactly one actual result');
  Require(Integer(FGrids[0].Cells[0,0]) = 0, 'Notification-only result rows were not published');
  Log('PASS actual worker result/rows/completion via CheckSynchronize only; no Windows message or WM_TIMER dispatch');
end;

procedure TGridProbe.CheckRefreshSuspension;
var LBefore, LCompleted: Integer; LStarted, LDeadline: UInt64;
begin
  Reset;
  EnableThrottle(0);
  LBefore := Calls(0);
  FGrids[0].RefreshSuspended := True;
  FGrids[0].RefreshGrid;
  FGrids[0].PageSize := 25;
  FGrids[0].PageIndex := 2;
  FGrids[0].Search('>400');
  FGrids[0].Columns[0].SortDirection := sdDescending;
  FGrids[0].SetBounds(10, 15, 490, 580);
  Pump(80);
  Require(Calls(0) = LBefore, 'Suspension admitted a leading or resized provider query');
  Require(FGrids[0].RefreshThrottle.Waiting, 'Suspension swallowed an independent throttle input');
  FGrids[1].RefreshGrid;
  Require(not FGrids[1].RefreshSuspended, 'Suspension leaked into the other grid');
  FGrids[0].RefreshThrottle.Cancel;
  Require(Calls(0) = LBefore, 'Throttle cancellation bypassed the suspension guard');
  LStarted := GetTickCount64;
  FGrids[0].RefreshSuspended := False;
  Require(GetTickCount64 - LStarted < 1000, 'Resume waited for the old five-second quiet interval');
  Require(Calls(0) = LBefore + 1, 'Resume did not admit exactly one latest page');
  Require((FSources[0].LastParams.PageSize = 25) and (FSources[0].LastParams.PageIndex = 2),
    'Resume lost the latest page intent');
  Require(Integer(FGrids[0].Cells[0,0]) = 949, 'Resume lost the latest search/sort intent');
  Require(not FGrids[0].RefreshThrottle.Waiting, 'Internal resume became a new throttle input');
  Log('PASS suspended latest page/search/sort/resize; immediate one-query resume; other grid independent');

  Reset;
  FGrids[0].FetchDataInThread := True;
  FSources[0].BlockRead := True;
  FSources[0].Entered.ResetEvent;
  FSources[0].ReleaseRead.ResetEvent;
  LBefore := Calls(0);
  LCompleted := FCompletions[0];
  FGrids[0].RefreshGrid;
  AwaitEntered(0);
  LStarted := GetTickCount64;
  FGrids[0].RefreshSuspended := True;
  Require(GetTickCount64 - LStarted < 150, 'Suspension joined an unfinished provider on the UI thread');
  FGrids[0].PageSize := 17;
  FGrids[0].PageIndex := 3;
  FGrids[0].RefreshThrottle.Cancel;
  FGrids[0].RefreshSuspended := False;
  FGrids[0].RefreshSuspended := True;
  FGrids[0].ShowLoading('Please Wait', 'Synchronizing');
  FSources[0].FailRead := True;
  FSources[0].ReleaseRead.SetEvent;
  AwaitIdle(0);
  Require((Calls(0) = LBefore + 1) and (FCompletions[0] = LCompleted),
    'True/false/true revived cancelled results or released deferred work');
  Require(FGrids[0].LoadingVisible and
    (TLabel(FGrids[0].FindComponent('LoadingStatus')).Caption = 'Please Wait'),
    'Cancelled failure replaced caller-owned presentation');
  Require(Integer(FGrids[0].Cells[0,0]) = 0, 'Cancelled failure replaced retained rows');
  FSources[0].FailRead := False;
  FSources[0].BlockRead := False;
  FGrids[0].RefreshThrottle.Cancel;
  FGrids[0].RefreshSuspended := False;
  AwaitIdle(0);
  Require((Calls(0) = LBefore + 2) and (FCompletions[0] = LCompleted + 1),
    'Retired cancellation was not followed by exactly one latest page');
  Require(Integer(FGrids[0].Cells[0,0]) = 51, 'Retirement lost the latest deferred page');
  FGrids[0].HideLoading;
  Log('PASS active cancellation without UI join; false/true completion race; ignored errors; latest deferred page');

  Reset;
  FGrids[0].FetchDataInThread := True;
  FSources[0].BlockRead := True;
  FSources[0].CooperativeRead := True;
  FSources[0].Entered.ResetEvent;
  FSources[0].ReleaseRead.ResetEvent;
  FGrids[0].RefreshGrid;
  AwaitEntered(0);
  FGrids[0].RefreshSuspended := True;
  Require(FSources[0].CancelObserved.WaitFor(1500) = wrSignaled,
    'Actual provider did not observe cooperative worker termination');
  LDeadline := GetTickCount64 + 1500;
  while FGrids[0].IsUpdating and (GetTickCount64 < LDeadline) do CheckSynchronize(10);
  Require(not FGrids[0].IsUpdating, 'Cancelled worker retirement depended on a polling timer');
  FSources[0].BlockRead := False;
  FGrids[0].RefreshThrottle.Cancel;
  FGrids[0].RefreshSuspended := False;
  AwaitIdle(0);
  Log('PASS cooperative provider Abort/finally and queued retirement; next query reacquires the real source lock');
end;

procedure TGridProbe.CheckQueuedLoading;
var LBefore: Integer; LPanel: TPanel; LCircle: TLoadingCircle; LUntil: UInt64;
begin
  Reset;
  // Other groups intentionally bind a manual card to Waiting. This group
  // verifies the component's automatic presentation without that test policy.
  FGrids[0].RefreshThrottle.OnWaitingChanged.Remove(WaitingChanged);
  try
  FGrids[0].RefreshThrottle.Interval := TTimeSpan.FromMilliseconds(900);
  FGrids[0].RefreshThrottle.Mode := temNotifyOnEventBurstStartAndFinished;
  FGrids[0].RefreshGrid;
  LBefore := Calls(0);
  FGrids[0].PageIndex := 1;
  Pump(250);
  LPanel := TPanel(FGrids[0].FindComponent('LoadingOverlay'));
  LCircle := TLoadingCircle(FGrids[0].FindComponent('LoadingCircle'));
  Require(FGrids[0].RefreshThrottle.Waiting and (Calls(0)=LBefore),
    'Queued input fetched before its quiet interval');
  Require(LPanel.Visible and LCircle.Active,'Queued input has no loading feedback');
  Pump(800);
  Require((Calls(0)=LBefore+1) and not LPanel.Visible,
    'Completed queued input did not retire its loading feedback');
  Log('PASS waiting input shows an animated card before admission; completed synchronous fetch hides it');

  FGrids[0].RefreshGrid;
  FGrids[0].PageIndex := 2;
  Pump(250);
  Require(LPanel.Visible,'Second queued input has no loading feedback');
  FGrids[0].RefreshThrottle.Cancel;
  Require(not LPanel.Visible,'Cancelled queued input left a loading card');
  Log('PASS cancelling queued input hides its ordinary loading card');

  FGrids[0].RefreshGrid;
  FGrids[0].PageIndex := 3;
  FGrids[0].ShowLoading('Please Wait','Synchronizing');
  Pump(250);
  Require(TLabel(FGrids[0].FindComponent('LoadingStatus')).Caption='Please Wait',
    'Queued input replaced caller-owned artwork');
  FGrids[0].RefreshThrottle.Cancel;
  Require(LPanel.Visible,'Cancellation dismissed caller-owned artwork');
  FGrids[0].HideLoading;
  Require(not LPanel.Visible,'Manual card did not hide');
  Log('PASS caller-owned synchronization card takes precedence and survives ordinary cancellation');

  Reset;
  FGrids[0].FetchDataInThread := True;
  FSources[0].BlockRead := True;
  FSources[0].ReleaseRead.ResetEvent;
  FSources[0].Entered.ResetEvent;
  FGrids[0].RefreshGrid;
  try
    AwaitEntered(0);
    FGrids[0].RefreshThrottle.Interval := TTimeSpan.FromMilliseconds(900);
    FGrids[0].RefreshThrottle.Mode := temNotifyOnEventBurstStartAndFinished;
    FGrids[0].PageIndex := 1;
    FSources[0].ReleaseRead.SetEvent;
    AwaitIdle(0,False);
    FSources[0].ReleaseRead.ResetEvent;
    Pump(250);
    Require(LPanel.Visible,'Deferred single request has no feedback');
    LUntil := GetTickCount64+2000;
    while FGrids[0].RefreshThrottle.Waiting and (GetTickCount64<LUntil) do Pump(5);
    Require(not FGrids[0].RefreshThrottle.Waiting and FGrids[0].IsUpdating,
      'Quiet finish did not admit the deferred held provider');
    Require(LPanel.Visible and LCircle.Active,
      'Quiet admission restarted the delay on an already visible loading card');
  finally FSources[0].ReleaseRead.SetEvent; end;
  AwaitIdle(0);
  Require(not LPanel.Visible,'Finished deferred request retained its loading card');
  Log('PASS deferred single input retains its visible loading card through quiet admission into a held worker');
  finally
    FGrids[0].RefreshThrottle.OnWaitingChanged.Add(WaitingChanged);
  end;
end;

procedure TGridProbe.Run;
begin
  if SameText(GetEnvironmentVariable('PASCAL_GRID_EARLY_CANCEL_ONLY'), '1') then begin
    CheckVisualGridEarlyWorkerCancellation;
    Log('PASS cancelled-before-execution worker retires and preserves other grid receivers');
    Exit;
  end;
  if SameText(GetEnvironmentVariable('PASCAL_GRID_LOADING_ONLY'), '1') then begin
    CheckQueuedLoading;
    Exit;
  end;
  if SameText(GetEnvironmentVariable('PASCAL_GRID_SUSPENSION_ONLY'), '1') then begin
    CheckRefreshSuspension;
    Exit;
  end;
  if SameText(GetEnvironmentVariable('PASCAL_GRID_COMPLETION_ONLY'), '1') then begin
    CheckWorkerNotificationWithoutTimers;
    Exit;
  end;
  CheckVisualGridEarlyWorkerCancellation;
  Log('PASS cancelled-before-execution worker retires and preserves other grid receivers');
  CheckRefreshSuspension;
  CheckWorkerNotificationWithoutTimers;
  CheckParentlessAndHiddenRequests;
  CheckMixedRequestsAndIndependentTimers;
  CheckBusySingleAndLatestParameters;
  CheckHeldResultAndCompletionInput;
  CheckCancelModeAndDisposal;
  Log('PASS all actual VCL grid admission checks');
end;

var Form: TForm; Probe: TGridProbe; LogPath: String;
begin
  if ParamCount > 1 then Halt(64);
  LogPath := ParamStr(1);
  if LogPath = '' then LogPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
    'GridThrottleProbe-' + IntToStr(GetCurrentProcessId) + '.log';
  Form := nil; Probe := nil;
  try
    ReportMemoryLeaksOnShutdown := True;
    Application.Initialize;
    Form := TForm.CreateNew(nil);
    Form.SetBounds(60,70,1080,690);
    Form.Caption := 'Generic grid request throttle regression';
    Form.Show;
    Probe := TGridProbe.Create(Form,LogPath);
    try Probe.Run; ExitCode := 0;
    finally FreeAndNil(Probe); end;
  except
    on E: Exception do begin
      with TStringList.Create do
      try
        if FileExists(LogPath) then LoadFromFile(LogPath);
        Add('FAIL ' + E.ClassName + ': ' + E.Message); SaveToFile(LogPath);
      finally Free; end;
      ExitCode := 1;
    end;
  end;
  Probe.Free; Form.Free;
end.
