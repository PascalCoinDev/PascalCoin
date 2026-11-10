unit UVisualGrid.Viewport.Checks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see LICENSE.
  The oracle is the native CellRect, not a second copy of the capacity formula. }

{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}

interface

uses Classes;

type
  TGridViewportScenario = (vsUniformRows, vsVariableRows, vsDpiScaling,
    vsHorizontalScrollbar, vsUnfetchedRows, vsAutomaticResize);

procedure CheckVisualGridViewport(AScenario: TGridViewportScenario; ALog: TStrings = nil);
procedure CheckVisualGridShowAllCompatibility(ALog: TStrings = nil);
procedure CheckVisualGridDeferredParenting(ALog: TStrings = nil);

implementation

uses SysUtils, Types, Math, Variants, Forms, Controls, StdCtrls, Grids,
  Generics.Collections, UCommon.Data, UVisualGrid;

type
  TViewportGrid = class(TVisualGrid)
  public
    function NativeGrid: {$IFDEF FPC}Grids.{$ELSE}UVisualGrid.{$ENDIF}TDrawGrid;
    function Capacity: Integer;
    procedure ApplyLayout;
  end;

  TShowAllSource = class(TCustomDataSource<Integer>)
  protected
    function GetColumns: TDataColumns; override;
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer; const ABindingName: String): Variant; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
  public
    Count: Integer;
  end;

function TShowAllSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Id', True));
end;

function TShowAllSource.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer; const ABindingName: String): Variant;
begin
  Result := AItem;
end;

procedure TShowAllSource.FetchAll(const AContainer: TList<Integer>);
var I: Integer;
begin
  for I := 0 to Count - 1 do AContainer.Add(I);
end;

function TViewportGrid.NativeGrid: {$IFDEF FPC}Grids.{$ELSE}UVisualGrid.{$ENDIF}TDrawGrid;
begin
  Result := FDrawGrid;
end;

function TViewportGrid.Capacity: Integer;
begin
  Result := ClientRowCount;
end;

procedure TViewportGrid.ApplyLayout;
begin
  LayoutChanged;
end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure CheckVisualGridShowAllCompatibility(ALog: TStrings);
var LForm: TForm; LGrid: TVisualGrid; LSource: TShowAllSource;
begin
  LForm := TForm.CreateNew(nil);
  try
    LForm.SetBounds(80, 90, 640, 480);
    LSource := TShowAllSource.Create(LForm);
    LSource.Count := 240;
    LGrid := TVisualGrid.Create(LForm);
    LGrid.Parent := LForm;
    LGrid.Align := alClient;
    LGrid.FetchDataInThread := False;
    LGrid.AddColumn('Id').Binding := 'Id';
    LForm.Show;
    Application.ProcessMessages;
    // This existing API must not turn an unknown total into a finite page.
    LGrid.ShowAllData := True;
    LGrid.DataSource := LSource;
    Application.ProcessMessages;
    Require(LGrid.RowCount = 240, 'ShowAllData before source assignment did not load all 240 records');
    LSource.Count := 370;
    LGrid.RefreshGrid;
    Application.ProcessMessages;
    Require(LGrid.RowCount = 370, 'ShowAllData truncated growth to the previous known total');
    if Assigned(ALog) then ALog.Add('PASS legacy ShowAllData unknown total and growing complete result');
  finally
    LForm.Free;
  end;
end;

procedure CheckVisualGridDeferredParenting(ALog: TStrings);
var LForm: TForm; LGrid: TViewportGrid; LSource: TShowAllSource;
begin
  LForm := TForm.CreateNew(nil);
  try
    LForm.SetBounds(80, 90, 640, 480);
    LSource := TShowAllSource.Create(LForm);
    LSource.Count := 3;
    LGrid := TViewportGrid.Create(LForm);
    LGrid.FetchDataInThread := False;
    LGrid.CanSearch := True;
    LGrid.SearchMode := smMulti;
    with LGrid.AddColumn('Id') do begin
      Binding := 'Id';
      Width := 180;
      Filters := SORTABLE_NUMERIC_FILTER;
    end;
    // Component ownership is deliberately not window parenting. The public
    // data API must retain data/column intent without asking VCL for a HWND.
    LGrid.DataSource := LSource;
    Require(not Assigned(LGrid.Parent), 'Deferred-parent fixture acquired a parent before source assignment');
    LGrid.Parent := LForm;
    LGrid.Align := alClient;
    LForm.Show;
    Application.ProcessMessages;
    LGrid.ApplyLayout;
    Application.ProcessMessages;
    Require((LGrid.RowCount = 3) and (LGrid.ColCount = 1),
      'Docking did not retain the source data and columns assigned before parenting');
    Require(LGrid.NativeGrid.HandleAllocated and
      (LGrid.NativeGrid.RowCount = 4) and (LGrid.NativeGrid.ClientWidth > 0),
      'Docking did not replay native row/column geometry after its parent existed');
    Require(LGrid.Columns[0].Width > 0, 'Deferred column geometry was lost');
    if Assigned(ALog) then ALog.Add('PASS synchronous data assignment before window parenting and deferred native layout');
  finally
    LForm.Free;
  end;
end;

procedure CheckVisualGridViewport(AScenario: TGridViewportScenario; ALog: TStrings);
var
  LForm: TForm;
  LGrid: TViewportGrid;
  LNative: {$IFDEF FPC}Grids.{$ELSE}UVisualGrid.{$ENDIF}TDrawGrid;
  LPpi, LPass, LLastPass, LRow, LFixed, LBottom, LDelta, LExpected, LActual: Integer;
  LLabel: String;
  LRect: TRect;
  LRowRects: array[0..127] of TRect;

  function FullyVisibleRows: Integer;
  var I: Integer; R: TRect;
  begin
    Result := 0;
    for I := LFixed to High(LRowRects) do begin
      R := LRowRects[I];
      if (R.Bottom > LNative.ClientHeight) then Break;
      if (R.Bottom > R.Top) and (R.Top >= 0) then Inc(Result);
    end;
    Result := Max(1,Result);
  end;

begin
  LLastPass := 0;
  if AScenario in [vsDpiScaling,vsUniformRows] then LLastPass := 2;
  for LPass := 0 to LLastPass do begin
    LPpi := 96;
    if AScenario = vsDpiScaling then Inc(LPpi,48*LPass);
    LForm := TForm.CreateNew(nil);
    try
      LForm.SetBounds(80,90,720,560);
      LGrid := TViewportGrid.Create(LForm);
      LGrid.Parent := LForm;
      LGrid.SetBounds(0,0,640,480);
      LGrid.AddColumn('Id').Binding := 'Id';
      LForm.Show;
      Application.ProcessMessages;
      if LPpi <> 96 then begin
        {$IFDEF FPC}LForm.ScaleBy(LPpi,96);{$ELSE}LForm.ScaleForPPI(LPpi);{$ENDIF}
        Application.ProcessMessages;
      end;
      LNative := LGrid.NativeGrid;
      LNative.Align := alNone;
      LNative.SetBounds(0,0,500,900);
      LNative.ScrollBars := ssNone;
      LNative.RowCount := 128;
      if AScenario = vsUniformRows then LNative.GridLineWidth := 1+LPass;
      LNative.FixedRows := 1;
      LNative.TopRow := LNative.FixedRows;
      if AScenario = vsVariableRows then LNative.FixedRows := 2;
      LFixed := LNative.FixedRows;
      for LRow := 0 to LNative.RowCount - 1 do
        LNative.RowHeights[LRow] := LNative.DefaultRowHeight;
      if AScenario = vsVariableRows then begin
        LNative.RowHeights[0] := 37;
        LNative.RowHeights[1] := 19;
        LNative.RowHeights[LFixed] := 31;
        LNative.RowHeights[LFixed+1] := 47;
        LNative.RowHeights[LFixed+3] := 13;
      end;
      if AScenario = vsHorizontalScrollbar then begin
        LNative.ColWidths[0] := 1400;
        LNative.ScrollBars := ssHorizontal;
        Application.ProcessMessages;
        Require(LNative.ClientHeight < LNative.Height,
          'Horizontal scrollbar fixture did not reduce the native client viewport');
      end;

      // Measure the fifth complete data row at a tall native viewport first.
      // Then test its exact bottom and both adjacent pixel boundaries.
      // VCL CellRect clips a partially visible last cell, so keep the original
      // uncut native rectangles as the oracle when shrinking the viewport.
      for LRow := 0 to High(LRowRects) do LRowRects[LRow] := LNative.CellRect(0,LRow);
      LBottom := LRowRects[LFixed+4].Bottom;
      for LDelta := -1 to 1 do begin
        LNative.ClientHeight := LBottom + LDelta;
        Application.ProcessMessages;
        LExpected := FullyVisibleRows;
        LActual := LGrid.Capacity;
        LLabel := Format('scenario=%d PPI=%d delta=%d client=%d row=%d line=%d',
          [Ord(AScenario),LPpi,LDelta,LNative.ClientHeight,
           LNative.DefaultRowHeight,LNative.GridLineWidth]);
        Require(LActual = LExpected,Format('%s: expected %d complete rows, got %d',
          [LLabel,LExpected,LActual]));
        Require(LExpected = 5 + Ord(LDelta < 0) * -1,
          LLabel + ': native boundary oracle did not describe 4/5/5 rows');
        LRect := LRowRects[LFixed+LActual-1];
        Require(LRect.Bottom <= LNative.ClientHeight,LLabel + ': last admitted row is clipped');
        if AScenario = vsUnfetchedRows then begin
          LNative.RowCount := LFixed+1;
          Require(LGrid.Capacity = LExpected,LLabel + ': capacity depends on fetched row count');
          LNative.RowCount := 128;
        end;
        if ALog <> nil then ALog.Add('PASS viewport ' + LLabel);
      end;

      if AScenario = vsAutomaticResize then begin
        LNative.Align := alClient;
        LGrid.AutoPageSize := True;
        for LDelta := -1 to 1 do begin
          LGrid.Height := 300 + 37 * LDelta;
          Application.ProcessMessages;
          LGrid.ApplyLayout;
          Require(LGrid.PageSize = FullyVisibleRows,
            Format('AutoPageSize after resize: page=%d native=%d capacity=%d rows=%d client=%d',
              [LGrid.PageSize,FullyVisibleRows,LGrid.Capacity,LNative.RowCount,LNative.ClientHeight]));
          if ALog <> nil then ALog.Add('PASS automatic resized page ' + IntToStr(LGrid.PageSize));
        end;
      end;
    finally
      LForm.Free;
    end;
  end;
end;

end.
