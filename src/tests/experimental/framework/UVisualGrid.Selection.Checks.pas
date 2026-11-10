unit UVisualGrid.Selection.Checks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Shared selection checks use native mouse messages and the actual grid event
  path. The integer source is the only data fixture; no wallet is involved. }

{$IFDEF FPC}{$mode delphi}{$H+}{$ENDIF}

interface

uses Classes;

type
  TGridSelectionScenario = (ssNativeClickEvents, ssRepeatedClickToggle,
    ssConsumerClearSelection, ssProgrammaticAndNoHandler);

procedure CheckVisualGridSelection(AScenario: TGridSelectionScenario; ALog: TStrings = nil);

implementation

uses
  SysUtils, Types, Variants, Forms, Controls, Generics.Collections,
  UCommon.Data, UVisualGrid
  {$IFDEF MSWINDOWS}, Windows, Messages{$ENDIF};

type
  TSelectionSource = class(TCustomDataSource<Integer>)
  protected
    function GetColumns: TDataColumns; override;
    function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer; const AColumnName: String): Variant; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
  end;

  TSelectionObserver = class(TComponent)
  public
    Grid: TVisualGrid;
    Pages, Rows: TArray<Integer>;
    ClearOnNextSelection: Boolean;
    procedure Selected(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection);
    procedure Reset;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then
    raise Exception.Create('Grid selection: ' + AMessage);
end;

procedure PumpMessages;
begin
  CheckSynchronize(0);
  Application.ProcessMessages;
end;

function TSelectionSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Id', True));
end;

function TSelectionSource.GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: Integer; const AColumnName: String): Variant;
begin
  Result := AItem;
end;

procedure TSelectionSource.FetchAll(const AContainer: TList<Integer>);
var
  I: Integer;
begin
  for I := 0 to 5 do
    AContainer.Add(I);
end;

procedure TSelectionObserver.Reset;
begin
  Pages := nil;
  Rows := nil;
end;

procedure TSelectionObserver.Selected(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection);
var
  LIndex: Integer;
begin
  LIndex := Length(Pages);
  SetLength(Pages, LIndex + 1);
  SetLength(Rows, LIndex + 1);
  Pages[LIndex] := ASelection.Page;
  if ASelection.Page < 0 then
    Rows[LIndex] := -1
  else
    Rows[LIndex] := ASelection.Row;
  if ClearOnNextSelection and (ASelection.Page >= 0) then
  begin
    ClearOnNextSelection := False;
    Grid.ClearSelection(True);
  end;
end;

procedure NativeClickRow(AGrid: TVisualGrid; ARow: Integer);
{$IFDEF MSWINDOWS}
var
  LRect: TRect;
  LX, LY: Integer;
  LPosition: LPARAM;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  LRect := AGrid.InternalDrawGrid.CellRect(0, ARow);
  LX := LRect.Left + (LRect.Right - LRect.Left) div 2;
  LY := LRect.Top + (LRect.Bottom - LRect.Top) div 2;
  Require((LX >= 0) and (LY >= 0) and
    (LX < AGrid.InternalDrawGrid.ClientWidth) and
    (LY < AGrid.InternalDrawGrid.ClientHeight), 'clicked row is outside the native client');
  LPosition := LPARAM((Cardinal(LY) shl 16) or (Cardinal(LX) and $FFFF));
  // Deliver both native button messages to this owned grid window. No global
  // cursor/input state or direct selection-event invocation is used.
  SendMessage(AGrid.InternalDrawGrid.Handle, WM_LBUTTONDOWN, MK_LBUTTON, LPosition);
  SendMessage(AGrid.InternalDrawGrid.Handle, WM_LBUTTONUP, 0, LPosition);
  PumpMessages;
  {$ELSE}
  raise Exception.Create('Native mouse selection checks require the Windows widgetset');
  {$ENDIF}
end;

procedure CheckVisualGridSelection(AScenario: TGridSelectionScenario; ALog: TStrings);
var
  LForm: TForm;
  LGrid: TVisualGrid;
  LSource: TSelectionSource;
  LObserver: TSelectionObserver;

  procedure RequireEvent(ACount, AIndex, APage, ARow: Integer; const AContext: String);
  begin
    Require(Length(LObserver.Pages) = ACount,
      AContext + ': expected ' + IntToStr(ACount) + ' events, received ' +
      IntToStr(Length(LObserver.Pages)));
    Require((LObserver.Pages[AIndex] = APage) and
      (LObserver.Rows[AIndex] = ARow), AContext + ': wrong selection payload');
  end;

begin
  {$IFNDEF MSWINDOWS}
  if AScenario in [ssNativeClickEvents, ssRepeatedClickToggle] then
  begin
    if Assigned(ALog) then
      ALog.Add('SKIP native mouse selection requires the Windows widgetset');
    Exit;
  end;
  {$ENDIF}
  LForm := TForm.CreateNew(nil);
  try
    LForm.SetBounds(80, 90, 640, 440);
    LSource := TSelectionSource.Create(LForm);
    LGrid := TVisualGrid.Create(LForm);
    LGrid.Parent := LForm;
    LGrid.Align := alClient;
    LGrid.FetchDataInThread := False;
    LGrid.PageMode := vpmManual;
    LGrid.ManualPageSize := 20;
    LGrid.SelectionType := stRow;
    LGrid.DeselectionType := dtDefault;
    LGrid.AddColumn('Id').Binding := 'Id';
    LGrid.DataSource := LSource;
    LForm.Show;
    PumpMessages;
    Require(not LGrid.IsUpdating and (LGrid.RowCount = 6), 'fixture did not publish its six rows');
    LGrid.ClearSelection(True);
    LObserver := TSelectionObserver.Create(LForm);
    LObserver.Grid := LGrid;
    LGrid.OnSelection := LObserver.Selected;

    case AScenario of
      ssNativeClickEvents:
        begin
          NativeClickRow(LGrid, 1);
          RequireEvent(1, 0, 0, 0, 'first native click');
          Require(Length(LGrid.SelectedRows) = 1, 'first click did not retain one selected row');
          NativeClickRow(LGrid, 2);
          RequireEvent(2, 1, 0, 1, 'second-row native click');
          Require(Integer(TDataRowData(LGrid.SelectedRows[0])['Id']) = 1,
            'second native click retained the wrong record');
        end;
      ssRepeatedClickToggle:
        begin
          NativeClickRow(LGrid, 2);
          RequireEvent(1, 0, 0, 1, 'initial toggle selection');
          NativeClickRow(LGrid, 2);
          RequireEvent(2, 1, -1, -1, 'repeated native click');
          Require(Length(LGrid.SelectedRows) = 0, 'repeated click did not clear selection');
          NativeClickRow(LGrid, 2);
          RequireEvent(3, 2, 0, 1, 'native click after deselection');
        end;
      ssConsumerClearSelection:
        begin
          LGrid.OnSelection := nil;
          LGrid.DeselectionType := dtNone;
          LGrid.InternalDrawGrid.Row := 1;
          LObserver.Reset;
          LObserver.ClearOnNextSelection := True;
          LGrid.OnSelection := LObserver.Selected;
          // A real native selection change enters the same event path. The
          // handler's explicit clear must publish one genuine empty follow-up.
          LGrid.InternalDrawGrid.Row := 3;
          PumpMessages;
          RequireEvent(2, 0, 0, 2, 'callback selected event');
          RequireEvent(2, 1, -1, -1, 'callback clear follow-up');
          Require(Length(LGrid.SelectedRows) = 0, 'callback clear was not retained');
          LGrid.OnSelection := nil;
          LGrid.SelectionType := stNone;
          LGrid.SelectionType := stRow;
          LObserver.Reset;
          LGrid.OnSelection := LObserver.Selected;
          LGrid.InternalDrawGrid.Row := 4;
          RequireEvent(1, 0, 0, 3, 'event guard after consumer clear');
        end;
      ssProgrammaticAndNoHandler:
        begin
          LGrid.OnSelection := nil;
          LGrid.DeselectionType := dtNone;
          LGrid.InternalDrawGrid.Row := 1;
          LObserver.Reset;
          LGrid.OnSelection := LObserver.Selected;
          LGrid.InternalDrawGrid.Row := 2;
          RequireEvent(1, 0, 0, 1, 'programmatic native selection');
          LGrid.ClearSelection(True);
          RequireEvent(2, 1, -1, -1, 'programmatic explicit clear');
          Require(Length(LGrid.SelectedRows) = 0, 'explicit clear retained rows');
          LGrid.OnSelection := nil;
          LGrid.SelectionType := stNone;
          LGrid.SelectionType := stRow;
          LGrid.InternalDrawGrid.Row := 3;
          Require(Length(LGrid.SelectedRows) = 1, 'no-handler selection did not retain its row');
          LGrid.ClearSelection(True);
          Require(Length(LGrid.SelectedRows) = 0, 'no-handler clear retained its native cursor as selection');
        end;
    end;
    if Assigned(ALog) then
      ALog.Add('PASS native grid selection scenario ' + IntToStr(Ord(AScenario)));
  finally
    LForm.Free;
  end;
end;

end.
