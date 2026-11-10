unit UVisualGrid.Loading.Checks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Real native loading checks around asynchronous publication and the grid's
  own five-second throttle. Only the integer provider and its gate are fixtures. }

{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}

interface

uses Classes;

type
  TGridLoadingScenario = (lsInternalLayoutFinishesLoading, lsNewClientKeepsLoading,
    lsManualCardSurvivesInternalLayout, lsErrorAndManualLayers,
    lsSynchronousErrorAndRecovery);

procedure CheckVisualGridLoading(AScenario: TGridLoadingScenario; ALog: TStrings = nil);

implementation

uses
  {$IFNDEF FPC}Winapi.Windows,{$ENDIF}
  SysUtils, Types, Variants, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  SyncObjs, Generics.Collections, UCommon, UCommon.UI, UCommon.Data, UVisualGrid;

type
  TLoadingSource = class(TCustomDataSource<Integer>)
  protected
    function GetColumns: TDataColumns; override;
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer; const ABindingName: String): Variant; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult; override;
  public
    Count, Calls: Integer;
    UnknownTotal, HoldNext, FailRead, ReplaceBeforeFailure: Boolean;
    LastParams: TPageFetchParams;
    Entered, ReleaseRead: TEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TLoadingObserver = class(TComponent)
  public
    Grid: TVisualGrid;
    Completions: Integer;
    FirstWaiting, FirstUpdating, FirstOverlay: Boolean;
    FirstRows, FirstPageSize: Integer;
    FirstCaption: String;
    procedure Completed(Sender: TObject);
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create('Grid loading: ' + AMessage);
end;

procedure PumpFor(AMilliseconds: Cardinal);
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

procedure Settle(AGrid: TVisualGrid; ATimeout: Cardinal = 2000);
var
  LUntil: UInt64;
begin
  LUntil := GetTickCount64 + ATimeout;
  repeat
    PumpFor(10);
    if not AGrid.IsUpdating and not AGrid.RefreshThrottle.Waiting then
    begin
      PumpFor(100);
      if not AGrid.IsUpdating and not AGrid.RefreshThrottle.Waiting then
        Exit;
    end;
  until GetTickCount64 >= LUntil;
  raise Exception.Create('Grid loading fixture did not settle');
end;

constructor TLoadingSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Count := 240;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseRead := TEvent.Create(nil, True, True, '');
end;

destructor TLoadingSource.Destroy;
begin
  ReleaseRead.Free;
  Entered.Free;
  inherited Destroy;
end;

function TLoadingSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Id', True));
end;

function TLoadingSource.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer; const ABindingName: String): Variant;
begin
  Result := AItem;
end;

procedure TLoadingSource.FetchAll(const AContainer: TList<Integer>);
var
  I: Integer;
begin
  if FailRead then
    raise Exception.Create('Loading fixture provider error');
  for I := 0 to Count - 1 do
    AContainer.Add(I);
end;

function TLoadingSource.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
begin
  Inc(Calls);
  LastParams := AParams;
  if FailRead and ReplaceBeforeFailure then
  begin
    ADataTable.Columns := GetColumns;
    SetLength(ADataTable.Rows, 1);
    ADataTable.Rows[0] := TDataRow.New(PtrInt(ClassInfo), ADataTable.Columns);
    TDataRowData(ADataTable.Rows[0])['Id'] := -999;
    raise Exception.Create('Loading fixture provider error');
  end;
  if HoldNext then
  begin
    HoldNext := False;
    Entered.SetEvent;
    Require(ReleaseRead.WaitFor(5000) = wrSignaled, 'held provider was not released');
  end;
  Result := inherited FetchPage(AParams, ADataTable);
  if UnknownTotal then
    Result.TotalDataCount := -1;
end;

procedure TLoadingObserver.Completed(Sender: TObject);
begin
  Inc(Completions);
  if Completions <> 1 then
    Exit;
  FirstWaiting := Grid.RefreshThrottle.Waiting;
  FirstUpdating := Grid.IsUpdating;
  FirstOverlay := TPanel(Grid.FindComponent('LoadingOverlay')).Visible;
  FirstCaption := TLabel(Grid.FindComponent('LoadingStatus')).Caption;
  FirstRows := Grid.RowCount;
  FirstPageSize := Grid.PageSize;
end;

procedure CheckVisualGridLoading(AScenario: TGridLoadingScenario; ALog: TStrings);
var
  LForm: TForm;
  LGrid: TVisualGrid;
  LSource: TLoadingSource;
  LObserver: TLoadingObserver;
  LPanel: TPanel;
  LImage: Graphics.TBitmap;
  LInitialCapacity, LBeforeCalls, LBeforeRows, LPass: Integer;
  LUntil: UInt64;
  LError: String;

  procedure AwaitFirstCompletion;
  begin
    LUntil := GetTickCount64 + 2000;
    while (LObserver.Completions = 0) and (GetTickCount64 < LUntil) do
      PumpFor(10);
    Require(LObserver.Completions = 1, 'accepted rows did not publish before the quiet interval');
  end;

  procedure BeginHeldRead(AThrottle: Boolean);
  begin
    LGrid.FetchDataInThread := True;
    LSource.Entered.ResetEvent;
    LSource.ReleaseRead.ResetEvent;
    LSource.HoldNext := True;
    LObserver.Completions := 0;
    LBeforeCalls := LSource.Calls;
    if AThrottle then
    begin
      LGrid.RefreshThrottle.Interval := TTimeSpan.FromSeconds(5);
      LGrid.RefreshThrottle.Mode := temNotifyOnEventBurstStartAndFinished;
    end;
    LGrid.RefreshGrid;
    Require(LSource.Entered.WaitFor(1000) = wrSignaled, 'actual worker never entered the provider');
    PumpFor(250);
    Require(LPanel.Visible, 'held fetch did not show its delayed automatic card');
  end;

begin
  LForm := TForm.CreateNew(nil);
  LSource := nil;
  LImage := nil;
  try
    LForm.SetBounds(80, 90, 640, 440);
    LSource := TLoadingSource.Create(LForm);
    LGrid := TVisualGrid.Create(LForm);
    LGrid.Parent := LForm;
    LGrid.Align := alClient;
    LGrid.FetchDataInThread := False;
    LGrid.AutoPageSize := True;
    LGrid.Options := [vgoAutoHidePaging, vgoAutoHideSearchPanel];
    LGrid.AddColumn('Id').Binding := 'Id';
    LObserver := TLoadingObserver.Create(LForm);
    LObserver.Grid := LGrid;
    LGrid.OnFinishedUpdating := LObserver.Completed;
    LPanel := LGrid.FindComponent('LoadingOverlay') as TPanel;
    LForm.Show;
    PumpFor(50);
    LGrid.DataSource := LSource;
    Settle(LGrid);
    LInitialCapacity := LGrid.PageSize;
    Require(LInitialCapacity > 2, 'native fixture needs multiple visible rows');

    if AScenario = lsSynchronousErrorAndRecovery then
    begin
      LBeforeRows := LGrid.RowCount;
      Require(Integer(LGrid.Rows[0].Id) = 0, 'initial synchronous page has the wrong identity');
      for LPass := 0 to 1 do
      begin
        LSource.FailRead := True;
        LSource.ReplaceBeforeFailure := LPass = 1;
        LObserver.Completions := 0;
        LError := '';
        try
          LGrid.RefreshGrid;
        except
          on E: Exception do
            LError := E.Message;
        end;
        Require(LError = 'Loading fixture provider error',
          'synchronous failure did not preserve the original exception');
        Require(LObserver.Completions = 0, 'failed synchronous fetch published completion');
        Require(not LGrid.IsUpdating, 'failed synchronous fetch retained worker state');
        Require((LGrid.RowCount = LBeforeRows) and (Integer(LGrid.Rows[0].Id) = 0),
          'failed synchronous output replaced the last accepted rows');
        PumpFor(250);
        Require(LPanel.Visible and
          (TLabel(LGrid.FindComponent('LoadingStatus')).Caption = LError),
          'failed synchronous fetch retained its loading card instead of its error');
        Require(not TLoadingCircle(LGrid.FindComponent('LoadingCircle')).Active,
          'failed synchronous fetch retained animation');
        LGrid.ShowLoading('Manual owner', 'Manual subtitle', nil, True);
        LGrid.HideLoading;
        Require(LPanel.Visible and
          (TLabel(LGrid.FindComponent('LoadingStatus')).Caption = LError),
          'manual presentation erased the synchronous error');
        LSource.FailRead := False;
        LGrid.RefreshGrid;
        Settle(LGrid);
        Require((LObserver.Completions = 1) and not LPanel.Visible and
          (LGrid.RowCount = LBeforeRows) and (Integer(LGrid.Rows[0].Id) = 0),
          'successful synchronous retry did not publish and clear the error');
      end;
    end
    else if AScenario = lsErrorAndManualLayers then
    begin
      LSource.FailRead := True;
      BeginHeldRead(False);
      LSource.ReleaseRead.SetEvent;
      LUntil := GetTickCount64 + 2000;
      while LGrid.IsUpdating and (GetTickCount64 < LUntil) do
        PumpFor(10);
      Require(not LGrid.IsUpdating and LPanel.Visible, 'provider error did not finish into its error card');
      Require(Pos('Loading fixture provider error', TLabel(LGrid.FindComponent('LoadingStatus')).Caption) > 0,
        'provider error text was not retained');
      LGrid.ShowLoading('Manual owner', 'Manual subtitle', nil, True);
      Require(TLabel(LGrid.FindComponent('LoadingStatus')).Caption = 'Manual owner',
        'manual card did not override an error');
      LGrid.HideLoading;
      Require(LPanel.Visible and
        (Pos('Loading fixture provider error', TLabel(LGrid.FindComponent('LoadingStatus')).Caption) > 0),
        'hiding manual presentation erased the provider error');
      LSource.FailRead := False;
      LGrid.RefreshGrid;
      Settle(LGrid);
      Require(not LPanel.Visible, 'successful recovery retained an automatic/error card');
    end
    else
    begin
      // Unknown totals are a supported provider contract. A partial final page
      // hides search chrome; that native capacity change requires an internal
      // refit rather than the known-complete-result no-query optimization.
      LSource.Count := LInitialCapacity - 1;
      LSource.UnknownTotal := True;
      BeginHeldRead(True);
      if AScenario = lsNewClientKeepsLoading then
        LGrid.PageIndex := 1;
      if AScenario = lsManualCardSurvivesInternalLayout then
      begin
        LImage := Graphics.TBitmap.Create;
        LImage.SetSize(8, 8);
        LImage.Canvas.Brush.Color := clRed;
        LImage.Canvas.FillRect(Types.Rect(0, 0, 8, 8));
        LGrid.ShowLoading('Please Wait', 'Manual synchronization', LImage, True);
      end;
      LSource.ReleaseRead.SetEvent;
      AwaitFirstCompletion;
      Require(LObserver.FirstWaiting and not LObserver.FirstUpdating,
        'completion must retain the throttle wait while finishing its worker');
      Require(LObserver.FirstRows = LSource.Count, 'accepted partial rows were not published');
      Require(LObserver.FirstPageSize > LInitialCapacity,
        'the actual result did not expose extra native rows for an internal refit');
      Require(LSource.Calls = LBeforeCalls + 1, 'internal refit queried before quiet finish');
      case AScenario of
        lsInternalLayoutFinishesLoading:
          begin
            Require(not LObserver.FirstOverlay and not LPanel.Visible,
              'completed data retained Updating solely for an internal layout request');
            Settle(LGrid, 6500);
            Require(LSource.Calls = LBeforeCalls + 2, 'quiet finish did not admit exactly one internal refit');
            Require(not LPanel.Visible and (LGrid.RowCount = LSource.Count),
              'final internal refit changed accepted rows or left its card visible');
          end;
        lsNewClientKeepsLoading:
          begin
            Require(LObserver.FirstOverlay and LPanel.Visible,
              'a genuinely newer client request lost its waiting feedback');
            LGrid.RefreshThrottle.Cancel;
            Require(not LPanel.Visible, 'cancelling pending client work left its card visible');
          end;
        lsManualCardSurvivesInternalLayout:
          begin
            Require(LObserver.FirstOverlay and LPanel.Visible and
              (LObserver.FirstCaption = 'Please Wait'), 'internal completion dismissed caller-owned loading');
            Require(TImage(LGrid.FindComponent('LoadingImage')).Visible,
              'caller-owned custom loading image was lost');
            LGrid.HideLoading;
            Require(LGrid.RefreshThrottle.Waiting and not LPanel.Visible,
              'hiding manual loading revealed an obsolete automatic card');
            LGrid.RefreshThrottle.Cancel;
          end;
      end;
    end;
    if ALog <> nil then
      ALog.Add('PASS native loading scenario=' + IntToStr(Ord(AScenario)));
  finally
    if Assigned(LSource) then
      LSource.ReleaseRead.SetEvent;
    LForm.Free;
    LImage.Free;
  end;
end;

end.
