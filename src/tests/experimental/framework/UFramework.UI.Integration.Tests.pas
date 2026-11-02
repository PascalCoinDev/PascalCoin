unit UFramework.UI.Integration.Tests;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see LICENSE.
  Shared UI regressions mirrored in the embedded PascalCoin framework suite. }

{$mode delphi}

interface

uses Classes, SysUtils, SyncObjs, Forms, Controls, ExtCtrls, Generics.Collections,
  fpcunit, testregistry, UCommon, UCommon.UI, UCommon.Data, UVisualGrid;

type
  TFrameworkUIIntegrationTests = class(TTestCase)
  private
    FCompleted: Integer;
    FCallbackThread: TThreadID;
    procedure OnCompleted(Sender: TObject);
    procedure PumpUntil(AValue: Integer);
  published
    procedure MillisecondDurationIsNumeric;
    procedure EmbeddedFormsHideBeforeDetaching;
    procedure GridCompletesOnMainThread;
    procedure GridDestructionJoinsBlockedFetch;
    procedure GridCoalescesRefreshDuringFetch;
    procedure GridSourceReplacementWaitsForFetch;
    procedure GridRecoversAfterFetchError;
    procedure ConcurrentDataRowSchemasRemainConsistent;
  end;

implementation

uses Windows, Types;

type
  TTestSource = class(TCustomDataSource<Integer>)
  public
    Entered, ReleaseFetch: TEvent;
    FetchCount: Integer;
    BlockFetch: Boolean;
    FailFetch: Boolean;
    Value: Integer;
    constructor Create;
    destructor Destroy; override;
    function GetColumns: TDataColumns; override;
    function GetItemField(constref AItem: Integer; const ABindingName: String): Variant; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
  end;

  TReleaseThread = class(TThread)
  private
    FSource: TTestSource;
  protected
    procedure Execute; override;
  public
    constructor Create(ASource: TTestSource);
  end;

  TSchemaThread = class(TThread)
  private
    FStart: TEvent;
    FNumber: Integer;
  protected
    procedure Execute; override;
  public
    ErrorMessage: String;
    constructor Create(AStart: TEvent; ANumber: Integer);
  end;

constructor TTestSource.Create;
begin
  inherited Create(nil);
  Value := 42;
  Entered := TEvent.Create(nil, true, false, '');
  ReleaseFetch := TEvent.Create(nil, true, false, '');
end;

destructor TTestSource.Destroy;
begin
  Entered.Free;
  ReleaseFetch.Free;
  inherited Destroy;
end;

function TTestSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Value', true));
end;

function TTestSource.GetItemField(constref AItem: Integer; const ABindingName: String): Variant;
begin
  Result := AItem;
end;

procedure TTestSource.FetchAll(const AContainer: TList<Integer>);
begin
  InterlockedIncrement(FetchCount);
  Entered.SetEvent;
  if BlockFetch and (ReleaseFetch.WaitFor(5000) <> wrSignaled) then
    raise Exception.Create('Fixture release timed out');
  if FailFetch then raise Exception.Create('Fixture fetch error');
  AContainer.Add(Value);
end;

constructor TReleaseThread.Create(ASource: TTestSource);
begin
  inherited Create(true);
  FSource := ASource;
  FreeOnTerminate := false;
end;

procedure TReleaseThread.Execute;
begin
  Sleep(75);
  FSource.ReleaseFetch.SetEvent;
end;

constructor TSchemaThread.Create(AStart: TEvent; ANumber: Integer);
begin
  inherited Create(true);
  FreeOnTerminate := false;
  FStart := AStart;
  FNumber := ANumber;
end;

procedure TSchemaThread.Execute;
var
  row: Variant;
  i: Integer;
begin
  try
    if FStart.WaitFor(5000) <> wrSignaled then raise Exception.Create('Schema gate timed out');
    for i := 0 to 63 do begin
      row := TDataRow.New(-2000000 - FNumber * 1000 - i,
        TDataColumns.Create(TDataColumn.From('Value', true)));
      row.Value := i;
      row.__KEY := FNumber;
      if (Integer(row.Value) <> i) or (Integer(row.__KEY) <> FNumber) then
        raise Exception.Create('Concurrent schema returned an inconsistent row');
    end;
  except
    on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
  end;
end;

procedure TFrameworkUIIntegrationTests.OnCompleted(Sender: TObject);
begin
  Inc(FCompleted);
  FCallbackThread := GetCurrentThreadID;
end;

procedure TFrameworkUIIntegrationTests.PumpUntil(AValue: Integer);
var
  deadline: QWord;
begin
  deadline := GetTickCount64 + 5000;
  while (FCompleted < AValue) and (GetTickCount64 < deadline) do begin
    Application.ProcessMessages;
    CheckSynchronize(0);
    Sleep(5);
  end;
  AssertTrue('Fetch completion timed out', FCompleted >= AValue);
end;

procedure TFrameworkUIIntegrationTests.MillisecondDurationIsNumeric;
begin
  AssertEquals(10000.0, TTimeSpan.FromSeconds(10).TotalMilliseconds, 0.0);
  AssertEquals(-250.0, TTimeSpan.FromMilliseconds(-250).TotalMilliseconds, 0.0);
end;

procedure TFrameworkUIIntegrationTests.EmbeddedFormsHideBeforeDetaching;
var
  host, first, second: TForm;
  panel: TPanel;
  origin: TPoint;
begin
  host := TForm.CreateNew(nil);
  first := TForm.CreateNew(nil);
  second := TForm.CreateNew(nil);
  try
    host.SetBounds(137, 83, 640, 420);
    host.Position := poDesigned;
    panel := TPanel.Create(host);
    panel.BevelOuter := bvNone;
    panel.Parent := host;
    panel.Align := alClient;
    host.Show;
    panel.AddControlDockCenter(first);
    Application.ProcessMessages;
    AssertEquals(PtrInt(panel.Handle), PtrInt(GetParent(first.Handle)));
    panel.RemoveAllControls(false);
    AssertFalse('Outgoing form must be hidden before native reparenting', first.Visible);
    AssertFalse(IsWindowVisible(first.Handle));
    panel.AddControlDockCenter(second);
    Application.ProcessMessages;
    AssertEquals(PtrInt(panel.Handle), PtrInt(GetParent(second.Handle)));
    AssertTrue((GetWindowLongPtr(second.Handle, GWL_STYLE) and WS_CHILD) <> 0);
    origin := second.ClientToScreen(Types.Point(0, 0));
    AssertEquals(panel.ClientToScreen(Types.Point(0, 0)).X, origin.X);
    AssertEquals(panel.ClientToScreen(Types.Point(0, 0)).Y, origin.Y);
    AssertEquals(137, host.Left);
    AssertEquals(83, host.Top);
    panel.RemoveAllControls(false);
  finally
    second.Free;
    first.Free;
    host.Free;
  end;
end;

procedure TFrameworkUIIntegrationTests.GridCompletesOnMainThread;
var
  source: TTestSource;
  grid: TVisualGrid;
begin
  source := TTestSource.Create;
  grid := TVisualGrid.Create(nil);
  try
    FCompleted := 0;
    grid.OnFinishedUpdating := OnCompleted;
    grid.DataSource := source;
    PumpUntil(1);
    AssertEquals(1, grid.RowCount);
    AssertEquals(PtrInt(GetCurrentThreadID), PtrInt(FCallbackThread));
    AssertEquals(42, Integer(grid.Rows[0].Value));
  finally
    grid.Free;
    source.Free;
  end;
end;

procedure TFrameworkUIIntegrationTests.GridDestructionJoinsBlockedFetch;
var
  source: TTestSource;
  grid: TVisualGrid;
  release: TReleaseThread;
begin
  source := TTestSource.Create;
  grid := TVisualGrid.Create(nil);
  release := nil;
  try
    source.BlockFetch := true;
    FCompleted := 0;
    grid.OnFinishedUpdating := OnCompleted;
    grid.DataSource := source;
    AssertEquals(Ord(wrSignaled), Ord(source.Entered.WaitFor(1000)));
    release := TReleaseThread.Create(source);
    release.Start;
    FreeAndNil(grid);
    AssertEquals('Destruction must not dispatch a stale completion', 0, FCompleted);
    AssertEquals(Ord(wrSignaled), Ord(source.ReleaseFetch.WaitFor(0)));
    Application.ProcessMessages;
    AssertEquals(0, FCompleted);
  finally
    if Assigned(release) then begin
      release.WaitFor;
      release.Free;
    end;
    grid.Free;
    source.Free;
  end;
end;

procedure TFrameworkUIIntegrationTests.GridCoalescesRefreshDuringFetch;
var
  source: TTestSource;
  grid: TVisualGrid;
begin
  source := TTestSource.Create;
  grid := TVisualGrid.Create(nil);
  try
    source.BlockFetch := true;
    // Keep layout-driven page-size refreshes outside this scheduling assertion.
    grid.Options := [];
    grid.CanPage := false;
    grid.CanSearch := false;
    FCompleted := 0;
    grid.OnFinishedUpdating := OnCompleted;
    grid.DataSource := source;
    AssertEquals(Ord(wrSignaled), Ord(source.Entered.WaitFor(1000)));
    grid.RefreshGrid;
    grid.RefreshGrid;
    grid.RefreshGrid;
    source.ReleaseFetch.SetEvent;
    PumpUntil(2);
    AssertEquals('Refresh requests during one fetch collapse to one follow-up', 2, source.FetchCount);
    AssertEquals(1, grid.RowCount);
  finally
    grid.Free;
    source.Free;
  end;
end;

procedure TFrameworkUIIntegrationTests.GridSourceReplacementWaitsForFetch;
var
  first, second: TTestSource;
  grid: TVisualGrid;
  release: TReleaseThread;
begin
  first := TTestSource.Create;
  second := TTestSource.Create;
  grid := TVisualGrid.Create(nil);
  release := nil;
  try
    first.BlockFetch := true;
    second.Value := 73;
    grid.DataSource := first;
    AssertEquals(Ord(wrSignaled), Ord(first.Entered.WaitFor(1000)));
    release := TReleaseThread.Create(first);
    release.Start;
    grid.DataSource := second;
    AssertEquals('Replacement must join the previous source', Ord(wrSignaled), Ord(first.ReleaseFetch.WaitFor(0)));
    FCompleted := 0;
    grid.OnFinishedUpdating := OnCompleted;
    PumpUntil(1);
    AssertEquals(73, Integer(grid.Rows[0].Value));
  finally
    if Assigned(release) then begin
      release.WaitFor;
      release.Free;
    end;
    grid.Free;
    second.Free;
    first.Free;
  end;
end;

procedure TFrameworkUIIntegrationTests.GridRecoversAfterFetchError;
var
  failed, recovered: TTestSource;
  grid: TVisualGrid;
begin
  failed := TTestSource.Create;
  recovered := TTestSource.Create;
  grid := TVisualGrid.Create(nil);
  try
    failed.FailFetch := true;
    recovered.Value := 91;
    grid.DataSource := failed;
    AssertEquals(Ord(wrSignaled), Ord(failed.Entered.WaitFor(1000)));
    // Source replacement collects the failed worker before starting a new one.
    grid.DataSource := recovered;
    FCompleted := 0;
    grid.OnFinishedUpdating := OnCompleted;
    PumpUntil(1);
    AssertEquals(91, Integer(grid.Rows[0].Value));
  finally
    grid.Free;
    recovered.Free;
    failed.Free;
  end;
end;

procedure TFrameworkUIIntegrationTests.ConcurrentDataRowSchemasRemainConsistent;
var
  gate: TEvent;
  threads: array[0..3] of TSchemaThread;
  i: Integer;
begin
  gate := TEvent.Create(nil, true, false, '');
  FillChar(threads, SizeOf(threads), 0);
  try
    for i := Low(threads) to High(threads) do begin
      threads[i] := TSchemaThread.Create(gate, i);
      threads[i].Start;
    end;
    gate.SetEvent;
    for i := Low(threads) to High(threads) do begin
      threads[i].WaitFor;
      AssertEquals('', threads[i].ErrorMessage);
    end;
  finally
    gate.SetEvent;
    for i := Low(threads) to High(threads) do
      if Assigned(threads[i]) then begin
        threads[i].WaitFor;
        threads[i].Free;
      end;
    gate.Free;
  end;
end;


initialization
  RegisterTest(TFrameworkUIIntegrationTests);
end.
