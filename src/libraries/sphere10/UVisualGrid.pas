{
  Copyright (c) 2017 - 2018 Sphere 10 Software

  An enterprise-class grid component with datasource, paging, searching capability.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Credits:
     Herman Schoenfeld (designer)
     Maciej Izak (hnb) (developer)
}

unit UVisualGrid;

{$MODE DELPHI}

{.$DEFINE VISUALGRID_DEBUG}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, Controls, Grids, Types, Graphics,
  UCommon, UCommon.Data, UCommon.Collections, Generics.Collections, Generics.Defaults, Menus, ComboEx, Buttons, Math,
  LResources, syncobjs;

const
  CT_VISUALGRID_DEFAULT = Integer(-1);   { Column is not sized on start }
  CT_VISUALGRID_STRETCH = Integer(-2);   { Column is stretched to fit }

type

  { TSelectionType }

  TSelectionType = (stNone, stCell, stRow, stMultiRow);

  { TVisualGridSelection }

  TVisualGridSelection = record
  private
    function GetCol: longint;
    function GetColCount: longint;
    function GetRow: longint;
    function GetRowCount: longint;
  public
    Selections: array of TRect;
    property Col: longint read GetCol;
    property Row: longint read GetRow;
    property RowCount: longint read GetRowCount;
    property ColCount: longint read GetColCount;
  end;

  TCustomVisualGrid = class;

  { TVisualColumn }

  TVisualColumn = class
  private
    function GetStretchedToFill: boolean; inline;
    function GetWidth: Integer; inline;
    procedure SetSortDirection(AValue: TSortDirection);
    procedure SetStretchedToFill(AValue: boolean); inline;
    procedure SetWidth(AValue: Integer); inline;
  protected
    FColumn: TGridColumn;
    FGrid: TCustomVisualGrid;
    FSortDirection: TSortDirection;
    FIgnoreRefresh: Boolean;
  public
    property InternalColumn : TGridColumn read FColumn;
    constructor Create(AGrid: TCustomVisualGrid; AColumn: TGridColumn);
    property StretchedToFill: boolean read GetStretchedToFill write SetStretchedToFill;
    property Width: Integer read GetWidth write SetWidth;
    property SortDirection: TSortDirection read FSortDirection write SetSortDirection;
  end;

  { TVisualGrid Events }

  TPreparePopupMenuEvent = procedure(Sender: TObject; constref ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu) of object;
  TSelectionEvent = procedure(Sender: TObject; constref ASelection: TVisualGridSelection) of object;
  TDrawVisualCellEvent = procedure(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const RowData: Variant; var Handled: boolean) of object;
  TColumnInitializeEvent = procedure(Sender: TObject; AColIndex:Integer; AColumn : TVisualColumn) of object;

  { TVisualGrid Exceptions }

  EVisualGridError = class(Exception);

  { TVisualGridOptions }

  TVisualGridOptions = set of (vgoColAutoFill, vgoColSizing, vgoAllowDeselect,
    vgoMultiSearchCheckComboBox, vgoSortDirectionAllowNone);

  { TSortMode }

  TSortMode = (smNone, smSingleColumn, smMultiColumn);

  { TSearchMode }

  TSearchMode = (smSingle, smMulti);

  { TVisualGridCaption }

  TVisualGridCaption = class(TPersistent)
  private
    FOwner: TCustomVisualGrid;
    FLabel: TLabel;

    function GetText: TCaption;
    function GetAlignment: TAlignment;
    function GetFont: TFont;
    function GetVisible: boolean;
    procedure SetText(AValue: TCaption);
    procedure SetAlignment(AValue: TAlignment);
    procedure SetFont(AValue: TFont);
    procedure SetVisible(AValue: boolean);
  public
    constructor Create(AOwner: TCustomVisualGrid);
  published
    property Text: TCaption read GetText write SetText;
    property Visible: boolean read GetVisible write SetVisible;
    property Font: TFont read GetFont write SetFont;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
  end;

  { TCustomVisualGrid }

  TCustomVisualGrid = class(TCustomControl)
  protected type
    TUpdateOfVisualGridGUI = set of (updPageIndex, updPageSize);

    TLastFetchDataResult = record
      FromThread: boolean;
      RefreshColumns: boolean;
      FetchResult: TPageFetchResult;
    end;

    { TSearchEdit }

    TSearchEdit = class
    private
      FGrid: TCustomVisualGrid;
      FPanel: TPanel;
      FEdit: TEdit;
      FButton: TButton;

      function GetEditVisible: boolean;
      function GetVisible: boolean;
      procedure SetEditVisible(AValue: boolean);
      procedure SetVisible(AValue: boolean);
    public
      SearchCapability: PSearchCapability;
      constructor Create(AParent: TWinControl; AGrid: TCustomVisualGrid);
      destructor Destroy; override;
      procedure Clear;

      property EditVisible: boolean read GetEditVisible write SetEditVisible;
      property Visible: boolean read GetVisible write SetVisible;
    end;

  protected const
    PAGE_NAVIGATION_FIRST    = 1;
    PAGE_NAVIGATION_PREVIOUS = 2;
    PAGE_NAVIGATION_NEXT     = 3;
    PAGE_NAVIGATION_LAST     = 4;
  protected { component interface part }
    FMainPanel: TPanel;
    FSearchLabel: TLabel;
    FSearchEdit: TEdit;
    FSearchButton: TSpeedButton;
    FMultiSearchCheckComboBox: TCheckComboBox;
    FTopPanel: TPanel;
    FTopPanelMultiSearch: TPanel;
    FTopPanelMultiSearchFixed: TPanel;
    FTopPanelMultiSearchClient: TPanel;
    FTopPanelMultiSearchRight: TPanel;
    FTopPanelLeft: TPanel;
    FTopPanelRight: TPanel;
    FClientPanel: TPanel;
    FLoadDataPanel: TPanel;
    FLoadDataLabel: TLabel;
    FLoadDataProgressLabel: TLabel;
    FBottomPanel: TPanel;
    FBottomCenterPanel: TPanel;
    FBottomRightPanel: TPanel;

    FButtonFirst: TButton;
    FButtonLast: TButton;
    FButtonNext: TButton;
    FButtonPrevious: TButton;

    FPageIndexEdit: TEdit;
    FPageCountLabel: TLabel;

    FPageSizeEdit: TEdit;
    FPageSizeLabel: TLabel;
    FAllRecordsCountLabel: TLabel;

    FDrawGrid: TDrawGrid;
    FDelayedBoundsChangeTimer: TTimer;
    FFetchDataThreadTimer: TTimer;
    FEditingDoneTimer: TTimer;
    FSearchKindPopupMenu: TPopupMenu;
    FSingleSearchMenuItem: TMenuItem;
    FMultiSearchMenuItem: TMenuItem;

    FMultiSearchEdits: TObjectList<TSearchEdit>;
    FColumns: TObjectList<TVisualColumn>;
  protected { events for UI }
    procedure StandardDrawCell(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SearchKindPopupMenuClick(Sender: TObject);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure GridHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure PageIndexEditingDone(Sender: TObject);
    procedure PageSizeEditingDone(Sender: TObject);
    procedure PageNavigationClick(Sender: TObject);
    procedure MultiSearchCheckComboBoxChange(Sender: TObject; AIndex: Integer);
    procedure DelayedBoundsChange(Sender: TObject);
    procedure FetchDataThreadProgress(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
  private
    FCaption: TVisualGridCaption;
    FFetchDataInThread: boolean;
    FActiveThread: TThread;
    FOnPreparePopupMenu: TPreparePopupMenuEvent;
    FOnSelection: TSelectionEvent;
    FFinishedUpdating: TNotifyEvent;
    FOptions: TVisualGridOptions;
    FSortMode: TSortMode;
    FSearchMode : TSearchMode;
    FShowAllData: boolean;
    FAutoPageSize: boolean;
    FCanPage: boolean;
    FCanSearch: boolean;
    FSelectionType: TSelectionType;
    FCurrentSelectionType: TSelectionType;
    FLastSelection: TVisualGridSelection;
    FIgnoreSelectionEvent: boolean;
    FDefaultStretchedColumn : Integer;
    FDefaultColumnWidths : TArray<Integer>;
    FWidgetControl: TControl;
    FWidgetControlParent: TWinControl;
    function GetCells(ACol, ARow: Integer): Variant;
    function GetColCount: Integer; inline;
    function GetColumns(Index: Integer): TVisualColumn;
    function GetActiveDataTable: PDataTable;
    function GetRowCount: Integer; inline;
    function GetRows(ARow: Integer): Variant;
    function GetSelection: TVisualGridSelection;
    function GetSelectedRows : TArray<Variant>;
    procedure ControlsEnable(AEnable: boolean);
    function GetCanvas: TCanvas;
    procedure SetCells(ACol, ARow: Integer; AValue: Variant);
    procedure SetFetchDataInThread(AValue: boolean);
    procedure SetSortMode(AValue: TSortMode);
    procedure SetSearchMode(AValue: TSearchMode);
    procedure SetOptions(AValue: TVisualGridOptions);
    procedure SetRows(ARow: Integer; AValue: Variant);
    procedure SetShowAllData(AValue: boolean);
    procedure SetAutoPageSize(AValue: boolean);
    procedure SetCanPage(AValue: boolean);
    procedure SetCanSearch(AValue: boolean);
{$IFDEF VISUALGRID_DEBUG}
    procedure ClickTest(Sender: TObject);
{$ENDIF}
    procedure SetPageIndex(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetSelectionType(AValue: TSelectionType);
    procedure SetWidgetControl(AValue: TControl);
  protected { TComponent }
    procedure Loaded; override;
  protected { TControl }
    procedure BoundsChanged; override;
  protected
    FGUIUpdates: TUpdateOfVisualGridGUI;
    FDataTable: TDataTable;
    FCachedDataTable: PDataTable;
    FDataSource: IDataSource;
    FStrFilter: UTF8String;
    FFilters: TList<TColumnFilter>;
    FSearchCapabilities: TSearchCapabilities;
    FPageSize: Integer;
    FPageIndex: Integer;
    FPageCount: Integer;
    FDefaultDrawGridOptions: TGridOptions;
    FTotalDataCount: Integer;
    FLastFetchDataResult: TLastFetchDataResult;
    FSortColumn: TVisualColumn;

    FOnDrawVisualCell: TDrawVisualCellEvent;
    FOnColumnInitialize : TColumnInitializeEvent;

    procedure SortDirectionGlyphRefresh;
    procedure ReloadColumns;
    procedure LayoutChanged;
    function ClientRowCount: Integer;
    procedure HidePageSizeControls(AVisible: boolean);
    procedure HidePageNavigationControls(AVisible: boolean);
    // return true if range is correct
    function CheckRangeForPageSize(var APageSize: Integer): boolean;
    procedure SetDataSource(ADataSource: IDataSource);
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState; const RowData: Variant);
    procedure RefreshPageIndexAndGridInterface;
    procedure RefreshPageIndexData(ARefreshColumns: boolean);
    procedure ResizeSearchEdit(ACol: Integer);
    procedure SetPageIndexEditText(const AStr: utf8string);
    procedure SetPageSizeEditText(const AStr: utf8string);
    procedure BeforeFetchPage;
    procedure FetchPage(out AResult: TPageFetchResult);
    procedure AfterFetchPage;
    property ActiveDataTable: PDataTable read GetActiveDataTable;
    procedure UpdateSelection(ASelectionType: TSelectionType);
    function SelectionsEquals(constref A, B: TVisualGridSelection): boolean;
    procedure ResetLastSelection; inline;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property DataSource: IDataSource read FDataSource write SetDataSource;
    property PageSize: Integer read FPageSize write SetPageSize default 100;
    property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
    property AutoPageSize: boolean read FAutoPageSize write SetAutoPageSize default false;
    property ShowAllData: boolean read FShowAllData write SetShowAllData default false;
    property FetchDataInThread: boolean read FFetchDataInThread write SetFetchDataInThread;
    property DefaultStretchedColumn: Integer read FDefaultStretchedColumn write FDefaultStretchedColumn;
    property DefaultColumnWidths : TArray<Integer> read FDefaultColumnWidths write FDefaultColumnWidths;

    property CanPage: boolean read FCanPage write SetCanPage default true;
    property CanSearch: boolean read FCanSearch write SetCanSearch default true;
    property Options: TVisualGridOptions read FOptions write SetOptions;
    property Canvas: TCanvas read GetCanvas;
    property SelectionType: TSelectionType read FSelectionType write SetSelectionType;
    property Selection: TVisualGridSelection read GetSelection;
    property SelectedRows : TArray<Variant> read GetSelectedRows;
    property SortMode: TSortMode read FSortMode write SetSortMode;
    property SearchMode : TSearchMode read FSearchMode write SetSearchMode;

    property Caption: TVisualGridCaption read FCaption write FCaption;
    property ColCount: Integer read GetColCount;
    property Columns[Index: Integer]: TVisualColumn read GetColumns;
    property Cells[ACol, ARow: Integer]: Variant read GetCells write SetCells;
    property RowCount: Integer read GetRowCount;
    property Rows[ARow: Integer]: Variant read GetRows write SetRows;

    property OnColumnInitialize : TColumnInitializeEvent read FOnColumnInitialize write FOnColumnInitialize;
    property OnDrawVisualCell: TDrawVisualCellEvent read FOnDrawVisualCell write FOnDrawVisualCell;
    property OnSelection: TSelectionEvent read FOnSelection write FOnSelection;
    property OnPreparePopupMenu: TPreparePopupMenuEvent read FOnPreparePopupMenu write FOnPreparePopupMenu;
    property OnFinishedUpdating : TNotifyEvent read FFinishedUpdating write FFinishedUpdating;

    property WidgetControl: TControl read FWidgetControl write SetWidgetControl;

    procedure RefreshGrid;
  end;

  { TVisualGrid }

  TVisualGrid = class(TCustomVisualGrid)
  published
    property Caption;

    property Align;
    property PageSize;
    property AutoPageSize;
    property ShowAllData;
    property CanPage;
    property CanSearch;
    property Options;
    property SelectionType;
    property SortMode;
    property SearchMode;
    property FetchDataInThread;
    property WidgetControl;

    property OnDrawVisualCell;
    property OnSelection;
    property OnPreparePopupMenu;
  end;

  { TVisualGridSearchExpressionService }

  TVisualGridSearchExpressionService = class
  end;

procedure Register;

implementation

uses Variants, UAutoScope, Dialogs;

resourcestring
  sTotal = 'Total: %d';
  sStandardSearch = 'Standard Search';
  sMultiColumnSearch = 'Multi-Column Search';
  sPageSize = 'Page size:';
  sSearchExpression = 'Search expression';
  sDataLoading = 'DATA LOADING';
  sExpression = 'Expression';
  sImproperColumnIndex = 'Improper column index. Max expected is %d but %d found.';

type
  TDrawGridAccess = class(TDrawGrid);
  //TScrollBarAccess = class(TScrollBar);

  { TFetchDataThread }

  TFetchDataThread = class(TThread)
  protected
    FGrid: TCustomVisualGrid;
    FLastFetchDataResult: TCustomVisualGrid.TLastFetchDataResult;
    procedure Execute; override;
  public
    constructor Create(AGrid: TCustomVisualGrid; ARefreshColumns: boolean);
    destructor Destroy; override;
  end;

procedure Register;
begin
  RegisterComponents('Pascal Framework', [TVisualGrid]);
end;

{ TVisualGridCaption }

function TVisualGridCaption.GetText: TCaption;
begin
  Result := FLabel.Caption;
end;

function TVisualGridCaption.GetAlignment: TAlignment;
begin
  Result := FLabel.Alignment;
end;

function TVisualGridCaption.GetFont: TFont;
begin
  Result := FLabel.Font;
end;

function TVisualGridCaption.GetVisible: boolean;
begin
  Result := FLabel.Visible;
end;

procedure TVisualGridCaption.SetText(AValue: TCaption);
begin
  if FLabel.Caption = AValue then Exit;
  FLabel.Caption:=AValue;
  FOwner.LayoutChanged;
end;

procedure TVisualGridCaption.SetAlignment(AValue: TAlignment);
begin
  if FLabel.Alignment = AValue then Exit;
  FLabel.Alignment:=AValue;
end;

procedure TVisualGridCaption.SetFont(AValue: TFont);
begin
  if FLabel.Font.IsEqual(AValue) then Exit;
  FLabel.Font.Assign(AValue);
  FOwner.LayoutChanged;
end;

procedure TVisualGridCaption.SetVisible(AValue: boolean);
begin
  if FLabel.Visible = AValue then Exit;
  FLabel.Visible:=AValue;
  FOwner.LayoutChanged;
end;

constructor TVisualGridCaption.Create(AOwner: TCustomVisualGrid);
begin
  FOwner := AOwner;

  FLabel := TLabel.Create(AOwner);
  FLabel.Parent := AOwner.FTopPanel;

  with FLabel do
  begin
    AnchorSideLeft.Control := AOwner.FTopPanelLeft;
    AnchorSideLeft.Side := asrBottom;
    AnchorSideTop.Control := AOwner.FTopPanelRight;
    AnchorSideTop.Side := asrCenter;
    AnchorSideRight.Control := AOwner.FTopPanelRight;
    Anchors := [akTop, akLeft, akRight];
    Visible := false;
  end;
end;

{ TVisualColumn }

function TVisualColumn.GetStretchedToFill: boolean;
begin
  Result := FColumn.SizePriority > 0;
end;

function TVisualColumn.GetWidth: Integer;
begin
  Result := FColumn.Width;
end;

procedure TVisualColumn.SetSortDirection(AValue: TSortDirection);
begin
  if FSortDirection=AValue then Exit;
  FSortDirection:=AValue;

  TDrawGridAccess(FGrid.FDrawGrid).InvalidateCell(FColumn.Index, 0, true);
  if not FIgnoreRefresh then
    FGrid.RefreshPageIndexData(false);
end;

procedure TVisualColumn.SetStretchedToFill(AValue: boolean);
begin
  FColumn.SizePriority := ifthen(AValue, 1);
end;

procedure TVisualColumn.SetWidth(AValue: Integer);
begin
  FColumn.Width := AValue;
end;

constructor TVisualColumn.Create(AGrid: TCustomVisualGrid; AColumn: TGridColumn );
begin
  FGrid := AGrid;
  FColumn := AColumn;
end;

{ TVisualGridSelection }

function TVisualGridSelection.GetCol: longint;
begin
  if Length(Selections) = 0 then
    Exit(-1);
  Result := Selections[0].Left;
end;

function TVisualGridSelection.GetColCount: longint;
begin
  if Length(Selections) = 0 then
    Exit(0);
  Result := Selections[0].Width + 1;
end;

function TVisualGridSelection.GetRow: longint;
begin
  if Length(Selections) = 0 then
    Exit(-1);
  Result := Selections[0].Top;
end;

function TVisualGridSelection.GetRowCount: longint;
begin
  if Length(Selections) = 0 then
    Exit(0);
  Result := Selections[0].Height + 1;
end;

{ TFetchDataThread }

procedure TFetchDataThread.Execute;
begin
  FGrid.FetchPage(FLastFetchDataResult.FetchResult);
end;

constructor TFetchDataThread.Create(AGrid: TCustomVisualGrid;
  ARefreshColumns: boolean);
begin
  AGrid.FActiveThread := Self;
  FGrid := AGrid;
  FGrid.ControlsEnable(false);
  FGrid.FFetchDataThreadTimer.Enabled:=true;
  FGrid.FLoadDataPanel.Visible:=True;
  FGrid.FLoadDataPanel.BringToFront;
  FGrid.BeforeFetchPage;
  FreeOnTerminate:=true;
  FLastFetchDataResult.RefreshColumns:=ARefreshColumns;

  // fast copy of data (we need to draw old data for a while)
  New(FGrid.FCachedDataTable);
  Move(FGrid.FDataTable, FGrid.FCachedDataTable^, SizeOf(TDataTable));
  FillChar(FGrid.FDataTable, SizeOf(TDataTable), #0);
  inherited Create(false);
end;

destructor TFetchDataThread.Destroy;
begin
  FLastFetchDataResult.FromThread:=true;
  FGrid.FLastFetchDataResult := FLastFetchDataResult;
  Synchronize(FGrid.AfterFetchPage);
  inherited Destroy;
end;

{ TCustomVisualGrid.TSearchEdit }

function TCustomVisualGrid.TSearchEdit.GetEditVisible: boolean;
begin
  Result := FEdit.Visible;
end;

function TCustomVisualGrid.TSearchEdit.GetVisible: boolean;
begin
  Result := FPanel.Visible;
end;

procedure TCustomVisualGrid.TSearchEdit.SetEditVisible(AValue: boolean);
begin
  FEdit.Visible:=AValue;
  //FButton.Visible:=AValue;
end;

procedure TCustomVisualGrid.TSearchEdit.SetVisible(AValue: boolean);
begin
  FPanel.Visible := AValue;
end;

constructor TCustomVisualGrid.TSearchEdit.Create(AParent: TWinControl;
  AGrid: TCustomVisualGrid);
begin
  FGrid := AGrid;
  FPanel := TPanel.Create(nil);
  FPanel.Parent := AParent;
  FPanel.BevelOuter := bvNone;
  FEdit := TEdit.Create(FPanel);
  FPanel.Height:=FEdit.Height;
  FEdit.Parent := FPanel;
  FEdit.PopupMenu := FGrid.FSearchKindPopupMenu;
  FEdit.Align:=alClient;
  {FButton := TButton.Create(FPanel);
  FButton.Width:=25;
  FButton.Parent := FPanel;
  FButton.Align:=alRight;}
end;

destructor TCustomVisualGrid.TSearchEdit.Destroy;
begin
  FPanel.Free;
  inherited Destroy;
end;

procedure TCustomVisualGrid.TSearchEdit.Clear;
begin
  FEdit.Clear;
end;

{ TCustomVisualGrid }

{$IFDEF VISUALGRID_DEBUG}
procedure TCustomVisualGrid.ClickTest(Sender: TOBject);
begin
  TButton(Sender).Caption := Format('%dx%d', [FSearchEdit.Left,FSearchEdit.Top]);
end;
{$ENDIF}

constructor TCustomVisualGrid.Create(Owner: TComponent);
begin
  inherited;

  FMultiSearchEdits := TObjectList<TSearchEdit>.Create;
  FColumns := TObjectList<TVisualColumn>.Create;
  FFilters := TList<TColumnFilter>.Create;
  ResetLastSelection;

  { component layout }

  ControlStyle := ControlStyle - [csAcceptsControls] + [csOwnedChildrenNotSelectable];

  FSearchKindPopupMenu := TPopupMenu.Create(Self);
  FSingleSearchMenuItem := TMenuItem.Create(Self);
  FSingleSearchMenuItem.RadioItem:=True;
  FSingleSearchMenuItem.Caption:=sStandardSearch;
  FSingleSearchMenuItem.OnClick:=SearchKindPopupMenuClick;
  FMultiSearchMenuItem := TMenuItem.Create(Self);
  FMultiSearchMenuItem.RadioItem:=True;
  FMultiSearchMenuItem.Caption:=sMultiColumnSearch;
  FMultiSearchMenuItem.OnClick:=SearchKindPopupMenuClick;

  FSearchKindPopupMenu.Items.Add([FSingleSearchMenuItem, FMultiSearchMenuItem]);

  FMainPanel := TPanel.Create(Self);
  FMainPanel.Parent := Self;
  with FMainPanel do
  begin
    Align:=alClient;
    BevelOuter := bvNone;
  end;

  FBottomPanel := TPanel.Create(Self);
  FBottomPanel.Parent := FMainPanel;
  with FBottomPanel do
  begin
    Align := alBottom;
    BevelOuter := bvNone;
    Height := 40;

    FBottomRightPanel := TPanel.Create(Self);
    FBottomRightPanel.Parent := FBottomPanel;
    with FBottomRightPanel do
    begin
      Width := 217;
      Height := 40;
      Align := alRight;
      BevelOuter := bvNone;
      FPageCountLabel := TLabel.Create(Self);
      FPageCountLabel.Parent := FBottomRightPanel;
      with FPageCountLabel do
      begin
        Left := 118;
        Top := 13;
        Width := 36;
        Height := 13;
        Caption := '/';
      end;
      FButtonFirst := TButton.Create(Self);
      FButtonFirst.Parent := FBottomRightPanel;
      with FButtonFirst do
      begin
        Left := 8;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := '|<';
        OnClick := PageNavigationClick;
        Tag := PAGE_NAVIGATION_FIRST;
      end;
      FButtonPrevious := TButton.Create(Self);
      FButtonPrevious.Parent := FBottomRightPanel;
      with FButtonPrevious do
      begin
        Left := 32;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := '<';
        OnClick := PageNavigationClick;
        Tag := PAGE_NAVIGATION_PREVIOUS;
      end;
      FButtonNext := TButton.Create(Self);
      FButtonNext.Parent := FBottomRightPanel;
      with FButtonNext do
      begin
        Left := 160;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := '>';
        OnClick := PageNavigationClick;
        Tag := PAGE_NAVIGATION_NEXT;
      end;
      FButtonLast := TButton.Create(Self);
      FButtonLast.Parent := FBottomRightPanel;
      with FButtonLast do
      begin
        Left := 184;
        Top := 8;
        Width := 25;
        Height := 25;
        Caption := '>|';
        OnClick := PageNavigationClick;
        Tag := PAGE_NAVIGATION_LAST;
      end;

      FPageIndexEdit := TEdit.Create(Self);
      FPageIndexEdit.Parent := FBottomRightPanel;
      with FPageIndexEdit do
      begin
        Left := 61;
        Top := 10;
        Width := 56;
        Height := 21;

        AnchorSideLeft.Control := FButtonPrevious;
        AnchorSideLeft.Side := asrBottom;
        AnchorSideTop.Control := FBottomRightPanel;
        AnchorSideRight.Control := FPageCountLabel;
        AnchorSideBottom.Control := FBottomRightPanel;
        AnchorSideBottom.Side := asrBottom;
        Anchors := [akLeft, akTop, akRight, akBottom];
        BorderSpacing.Top := 10;
        BorderSpacing.Right := 2;
        BorderSpacing.Left := 2;
        BorderSpacing.Bottom := 8;

        OnEditingDone := PageIndexEditingDone;
      end;

    end;

    FBottomCenterPanel := TPanel.Create(Self);
    FBottomCenterPanel.Parent := FBottomPanel;
    with FBottomCenterPanel do
    begin
      Align := alClient;
      BevelOuter := bvNone;
      FAllRecordsCountLabel := TLabel.Create(Self);
      FAllRecordsCountLabel.Parent := FBottomCenterPanel;
      with FAllRecordsCountLabel do
      begin
        Left := 7;
        Top := 13;
        Width := 31;
        Height := 13;
        Caption := Format(sTotal, [0]);
      end;
      FPageSizeLabel := TLabel.Create(Self);
      FPageSizeLabel.Parent := FBottomCenterPanel;
      with FPageSizeLabel do
      begin
        Left := 116;
        Top := 13;
        Width := 31;
        Height := 13;
        Caption := sPageSize;
      end;
      FPageSizeEdit := TEdit.Create(Self);
      FPageSizeEdit.Parent := FBottomCenterPanel;
      with FPageSizeEdit do
      begin
        Left := 181;
        Top := 10;
        Width := 52;
        Height := 21;

        AnchorSideLeft.Control := FPageSizeLabel;
        AnchorSideLeft.Side := asrBottom;
        AnchorSideTop.Control := FBottomCenterPanel;
        AnchorSideBottom.Control := FBottomCenterPanel;
        AnchorSideBottom.Side := asrBottom;
        Anchors := [akLeft, akTop, akBottom];
        BorderSpacing.Top := 10;
        BorderSpacing.Left := 2;
        BorderSpacing.Bottom := 8;

        OnEditingDone:=PageSizeEditingDone;
      end;
    end;
  end;

  FTopPanel := TPanel.Create(Self);
  FTopPanel.Parent := FMainPanel;
  with FTopPanel do
  begin
    Align := alTop;
    BevelOuter := bvNone;
    Height := 36;

    FMultiSearchCheckComboBox := TCheckComboBox.Create(Self);
    FMultiSearchCheckComboBox.Parent := FTopPanel;
    with FMultiSearchCheckComboBox do
    begin
      AnchorSideTop.Control := FTopPanel;
      AnchorSideLeft.Control := FTopPanel;
      AnchorSideBottom.Control := FTopPanel;
      AnchorSideBottom.Side := asrBottom;
      Anchors := [akLeft, akTop, akBottom];

      BorderSpacing.Top := 6;
      BorderSpacing.Left := 4;
      BorderSpacing.Bottom := 6;
      Width := 120;
      OnItemChange:=MultiSearchCheckComboBoxChange;
      PopupMenu:=FSearchKindPopupMenu;
      Visible:=False;
    end;

    FTopPanelLeft := TPanel.Create(Self);
    FTopPanelLeft.Parent := FTopPanel;
    with FTopPanelLeft do
    begin
      BevelOuter := bvNone;
      Align := alLeft;
      Height := 40;
      Width := 0;
    end;

    FTopPanelRight := TPanel.Create(Self);
    FTopPanelRight.Parent := FTopPanel;
    with FTopPanelRight do
    begin
      BevelOuter := bvNone;
      Align := alRight;
      Height := 40;
      Width := 154;

      FSearchButton := TSpeedButton.Create(Self);
      FSearchButton.Parent := FTopPanelRight;
      {$IFDEF WINDOWS}
      FSearchButton.LoadGlyphFromResourceName(HINSTANCE, 'VISUALGRID_SEARCH');
      {$ELSE}
      FSearchButton.LoadGlyphFromLazarusResource('VISUALGRID_SEARCH');
      {$ENDIF}
      with FSearchButton do
      begin
        AnchorSideTop.Control := FTopPanelRight;
        AnchorSideRight.Control := FTopPanelRight;
        AnchorSideRight.Side := asrBottom;
        AnchorSideBottom.Control := FTopPanelRight;
        AnchorSideBottom.Side := asrBottom;
        Anchors := [akTop, akRight, akBottom];
        BorderSpacing.Top := 6;
        BorderSpacing.Right := 4;
        BorderSpacing.Bottom := 6;
        Width := 23;
        PopupMenu:=FSearchKindPopupMenu;
        OnClick:=SearchButtonClick;
      end;

      FSearchEdit := TEdit.Create(Self);
      FSearchEdit.Parent := FTopPanelRight;
      with FSearchEdit do
      begin
        AnchorSideTop.Control := FTopPanelRight;
        AnchorSideRight.Control := FSearchButton;
        AnchorSideBottom.Control := FTopPanelRight;
        AnchorSideBottom.Side := asrBottom;
        Anchors := [akTop, akRight, akBottom];
        BorderSpacing.Top := 6;
        BorderSpacing.Right := 2;
        BorderSpacing.Bottom := 6;
        Width := 121;
        TextHint := sSearchExpression;
        PopupMenu := FSearchKindPopupMenu;
      end;
    end;
  end;

  FCaption := TVisualGridCaption.Create(Self);

  FClientPanel := TPanel.Create(Self);
  FClientPanel.Parent := FMainPanel;
  with FClientPanel do
  begin
    Align := alClient;
    BevelOuter := bvNone;

    FTopPanelMultiSearch := TPanel.Create(Self);
    FTopPanelMultiSearch.Parent := FClientPanel;
    with FTopPanelMultiSearch do
    begin
      Align:=alTop;
      BevelOuter := bvNone;
      Height := 40;

      FTopPanelMultiSearchFixed := TPanel.Create(Self);
      FTopPanelMultiSearchFixed.Parent := FTopPanelMultiSearch;
      with FTopPanelMultiSearchFixed do
      begin
        Align:=alLeft;
        BevelOuter := bvNone;
        Height := 40;
        Width := 0; // may be usefull for fixed columns
      end;

      FTopPanelMultiSearchClient := TPanel.Create(Self);
      FTopPanelMultiSearchClient.Parent := FTopPanelMultiSearch;
      with FTopPanelMultiSearchClient do
      begin
        Align:=alClient;
        BevelOuter := bvNone;
        Height := 40;
      end;

      {FTopPanelMultiSearchRight := TPanel.Create(Self);
      FTopPanelMultiSearchRight.Parent := FTopPanelMultiSearch;
      with FTopPanelMultiSearchRight do
      begin
        Align:=alRight;
        BevelOuter := bvNone;
        Height := 40;
        Width:=TScrollBarAccess.GetControlClassDefaultSize.cy;
      end;}
    end;

    FDrawGrid := TDrawGrid.Create(Self);
    FDrawGrid.Parent := FClientPanel;
    with FDrawGrid do
    begin
      Align := alClient;
      BorderStyle := bsNone;
      OnDrawCell := StandardDrawCell;
      OnMouseUp := GridMouseUp;
      OnSelection := GridSelection;
      OnHeaderClick := GridHeaderClick;
      Options := (Options - [goRangeSelect]);
      FixedCols := 0;
      ColCount:=0;
      RowCount:=0;
    end;
    FDefaultDrawGridOptions := FDrawGrid.Options;
  end;

  FLoadDataPanel := TPanel.Create(Self);
  FLoadDataPanel.Parent := Self;
  with FLoadDataPanel do
  begin
    //BevelOuter := bvNone;
    Width := 300;
    Height := 150;
    //Align:=alClient;
    Visible:=false;
    AnchorSideLeft.Control := Self;
    AnchorSideLeft.Side := asrCenter;
    AnchorSideTop.Control := Self;
    AnchorSideTop.Side := asrCenter;

    FLoadDataLabel := TLabel.Create(Self);
    FLoadDataLabel.Parent := FLoadDataPanel;
    with FLoadDataLabel do
    begin
      AnchorSideLeft.Control := FLoadDataPanel;
      AnchorSideLeft.Side := asrCenter;
      AnchorSideTop.Control := FLoadDataPanel;
      AnchorSideTop.Side := asrCenter;
      Caption := sDataLoading;
    end;
    FLoadDataProgressLabel := TLabel.Create(Self);
    FLoadDataProgressLabel.Parent := FLoadDataPanel;
    with FLoadDataProgressLabel do
    begin
      AnchorSideLeft.Control := FLoadDataLabel;
      AnchorSideLeft.Side := asrCenter;
      AnchorSideTop.Control := FLoadDataLabel;
      AnchorSideTop.Side := asrBottom;
      Caption := '-';
    end
  end;

  FDelayedBoundsChangeTimer := TTimer.Create(Self);
  with FDelayedBoundsChangeTimer do
  begin
    Enabled:=false;
    Interval:=20;
    OnTimer:=DelayedBoundsChange;
  end;

  FFetchDataThreadTimer := TTimer.Create(Self);
  with FFetchDataThreadTimer do
  begin
    Enabled:=false;
    Interval:=250;
    OnTimer:=FetchDataThreadProgress;
  end;

  FEditingDoneTimer := TTimer.Create(Self);
  with FEditingDoneTimer do
  begin
    Enabled:=false;
    Interval:=2000;
  end;

  { default values for properties }
  DefaultStretchedColumn := -1;
  FSearchMode := smSingle;
  PageSize := 100;
  PageIndex := -1;
  FCanPage := true;
  FCanSearch := true;
  FTotalDataCount := -1;

  {$IFDEF VISUALGRID_DEBUG}
  with TButton.Create(Self) do
  begin
    Left := 0;
    Top := 0;
    Parent := Self;
    OnClick := ClickTest;
    Caption := 'Test';
  end;
  {$ENDIF}

  FetchDataInThread := true;

  { set single search mode as default }
  FTopPanelMultiSearch.Visible := False;
  FMultiSearchCheckComboBox.AddItem(sExpression, cbChecked);
  SearchKindPopupMenuClick(FSingleSearchMenuItem);
end;

destructor TCustomVisualGrid.Destroy;
begin
  FFilters.Free;
  FColumns.Free;
  FMultiSearchEdits.Free;
  FCaption.Free;
  inherited;
end;

procedure TCustomVisualGrid.DoDrawCell(Sender: TObject; ACol, ARow: Longint;
  Rect: TRect; State: TGridDrawState; const RowData: Variant);
const
  {
    Direction triangle points schema

                 (P1)                x
              *   x                *--->
                 / \               |
                /   \             y|
               /     \             |
         (P3) x-------x (P2)       v

    where * is (0,0) point
  }

  DIRECTION_RECT_HEIGHT = 3;
  DIRECTION_RECT_WIDTH = 5;

  P1_X = (Pred(DIRECTION_RECT_WIDTH) div 2 + Pred(DIRECTION_RECT_WIDTH) mod 2);
  P1_Y = 0;
  P2_X = Pred(DIRECTION_RECT_WIDTH);
  P2_Y = Pred(DIRECTION_RECT_HEIGHT);
  P3_X = 0;
  P3_Y = Pred(DIRECTION_RECT_HEIGHT);
  P1_Y_CENTER_DELTA = -(Pred(DIRECTION_RECT_HEIGHT) div 2 + Pred(DIRECTION_RECT_HEIGHT) mod 2);
  P2_Y_CENTER_DELTA = -(Pred(DIRECTION_RECT_HEIGHT) div 2);
  P3_Y_CENTER_DELTA = -(Pred(DIRECTION_RECT_HEIGHT) div 2);

  RIGHT_MARIGN = 4;

var
  LText: utf8string;
  LColumn: TVisualColumn;
  LHalfOfHeight: longint;
  LP1,LP2,LP3: TPoint;
begin
  if ARow = 0 then
  begin
    if ACol < Length(ActiveDataTable.Columns) then
      LText := ActiveDataTable.Columns[ACol]
    else
      raise EVisualGridError.CreateFmt(sImproperColumnIndex, [Length(ActiveDataTable.Columns)-1,ACol]);
    LColumn := GetColumns(ACol);
    FDrawGrid.Canvas.TextRect(Rect, Rect.Left+8, Rect.Top+2, LText);

    FDrawGrid.Canvas.Pen.Width:=1;
    FDrawGrid.Canvas.Pen.Color:=clBlack;
    FDrawGrid.Canvas.Brush.Color:=clBlack;

    // draw triangle
    LHalfOfHeight := Rect.Height div 2 - Rect.Height mod 2;
    LP1 := TPoint.Create(
      Rect.Right + P1_X -(DIRECTION_RECT_WIDTH + RIGHT_MARIGN),
      Rect.Top + LHalfOfHeight + P1_Y + P1_Y_CENTER_DELTA);
    LP2 := TPoint.Create(
      Rect.Right + P2_X -(DIRECTION_RECT_WIDTH + RIGHT_MARIGN),
      Rect.Top + LHalfOfHeight + P2_Y + P2_Y_CENTER_DELTA);
    LP3 := TPoint.Create(
      Rect.Right + P3_X -(DIRECTION_RECT_WIDTH + RIGHT_MARIGN),
      Rect.Top + LHalfOfHeight + P3_Y + P3_Y_CENTER_DELTA);
    case LColumn.SortDirection of
      sdNone: ;
      sdAscending:
        FDrawGrid.Canvas.Polygon([LP1, LP2, LP3]);
      sdDescending:
        begin
          // swap y for points
          LP3.y := LP1.y;
          LP1.y := LP2.y;
          LP2.y := LP3.y;
          FDrawGrid.Canvas.Polygon([LP1, LP2, LP3]);
        end;
    end;
  end
  else
    FDrawGrid.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, RowData);
end;

procedure TCustomVisualGrid.Loaded;
begin
  inherited;
  ReloadColumns;
end;

procedure TCustomVisualGrid.BoundsChanged;
begin
  inherited BoundsChanged;
  // fix maximize form problem for AutoPageSize (new size of grid is not yet fully propagated)
  FDelayedBoundsChangeTimer.Enabled:=true;
end;

procedure TCustomVisualGrid.SortDirectionGlyphRefresh;

  procedure VisualColumnsSortDirection(AStopOnFirstSortable, AAllNone: boolean);
  var
    i: integer;
    p: PSearchCapability;
    LStop: boolean = false;
    LColumn: TVisualColumn;
  begin
    for i := 0 to FColumns.Count - 1 do
    try
      LColumn := FColumns[i];
      // lock RefreshPageIndexData
      LColumn.FIgnoreRefresh:=true;
      if AAllNone then
      begin
        LColumn.SortDirection := sdNone;
        Continue;
      end;
      p := FMultiSearchEdits[i].SearchCapability;
      if Assigned(p) and (vgfSortable in p^.SupportedFilters) then
      begin
        if not LStop then
          if vgoSortDirectionAllowNone in Options then
            LColumn.SortDirection := sdNone
          else
            LColumn.SortDirection := sdAscending
        else
          LColumn.SortDirection := sdNone;

        if AStopOnFirstSortable and not LStop then
        begin
          FSortColumn := LColumn;
          LStop := true;
        end;
      end else
        LColumn.SortDirection := sdNone;
    finally
      LColumn.FIgnoreRefresh:=false;
    end;
  end;

begin
  case FSortMode of
    smNone:
      begin
        FDrawGrid.Options := FDrawGrid.Options - [goHeaderHotTracking, goHeaderPushedLook];
        VisualColumnsSortDirection(false, true)
      end;
    smSingleColumn, smMultiColumn:
      begin
        FDrawGrid.Options := FDrawGrid.Options + [goHeaderHotTracking, goHeaderPushedLook];
        VisualColumnsSortDirection(FSortMode=smSingleColumn, false);
      end;
  end;
  RefreshPageIndexData(false);
end;

procedure TCustomVisualGrid.DelayedBoundsChange(Sender: TObject);
begin
  // check data loading. try in next cycle
  if FFetchDataThreadTimer.Enabled then
    Exit;
  FDelayedBoundsChangeTimer.Enabled:=false;
  if AutoPageSize then
    PageSize := ClientRowCount;
end;

procedure TCustomVisualGrid.FetchDataThreadProgress(Sender: TObject);
{$J+}
const
  PROGRESS: Integer = 0;
{$J-}
  PROGRESS_CHARS: array[0..3] of string = ('-', '\', '|', '/');
begin
  FLoadDataProgressLabel.Caption := PROGRESS_CHARS[PROGRESS];
  Inc(PROGRESS);
  if PROGRESS > High(PROGRESS_CHARS) then
    PROGRESS := 0;
end;

procedure TCustomVisualGrid.SearchButtonClick(Sender: TObject);
var
  LFormat: TFormatSettings;
  LNewStrFilter: utf8string;
  LException: boolean = false;
  e: TSearchEdit;

  procedure AddExpression(const AExpression: utf8string;
    ASearchCapability: PSearchCapability);
  var
    LSearchCapability: TSearchCapability;
    LAccepted: TVisualGridFilters;
    LColumnFilter: TColumnFilter;
    LCandidates: TVisualGridFilters = [];
    LExpressionRecord: TExpressionRecord;

    procedure AddFilter(ASearchCapability: PSearchCapability);
    var
      LFilter: TVisualGridFilter;
    begin
      LAccepted := ASearchCapability.SupportedFilters * LCandidates;
      if LAccepted = [] then
        Exit;
      for LFilter in LAccepted do
      begin
        LColumnFilter:=Default(TColumnFilter);
        LColumnFilter.ColumnName := ASearchCapability.ColumnName;
        LColumnFilter.Filter := LFilter;

        if LFilter in TEXT_FILTER then
        begin
          SetLength(LColumnFilter.Values, 1);
          LColumnFilter.Values[0]:=LExpressionRecord.Values[0];
        end
        else if LFilter in (NUMERIC_FILTER - [vgfNumericBetweenInclusive, vgfNumericBetweenExclusive]) then
        begin
          SetLength(LColumnFilter.Values, 1);
          if LExpressionRecord.HasDecimals then
            LColumnFilter.Values[0]:=StrToFloat(LExpressionRecord.Values[0], LFormat)
          else
            LColumnFilter.Values[0]:=StrToInt(LExpressionRecord.Values[0]);
        end
        else if LFilter in [vgfNumericBetweenInclusive, vgfNumericBetweenExclusive] then
        begin
          SetLength(LColumnFilter.Values, 2);
          if LExpressionRecord.HasDecimals then
          begin
            LColumnFilter.Values[0]:=StrToFloat(LExpressionRecord.Values[0], LFormat);
            LColumnFilter.Values[1]:=StrToFloat(LExpressionRecord.Values[1], LFormat);
          end else
          begin
            LColumnFilter.Values[0]:=StrToInt(LExpressionRecord.Values[0]);
            LColumnFilter.Values[1]:=StrToInt(LExpressionRecord.Values[1]);
          end;
        end;

        FFilters.Add(LColumnFilter);
      end;
    end;

  begin
    try
      LExpressionRecord := TSearchExpressionService.Parse(AExpression);
    except
      LException:=true;
      raise;
    end;

    with LExpressionRecord do
    begin
      case Kind of
        ekUnknown:
          Exit;
        ekNum:
          case NumericComparisionKind of
            // nckUnknown is special case - pure number can be text and num
            nckUnknown: LCandidates := [vgfNumericEQ, vgfMatchTextExact];
            nckNumericEQ: LCandidates := [vgfNumericEQ];
            nckNumericLT: LCandidates := [vgfNumericLT];
            nckNumericLTE: LCandidates := [vgfNumericLTE];
            nckNumericGT: LCandidates := [vgfNumericGT];
            nckNumericGTE: LCandidates := [vgfNumericGTE];
          else
            Assert(false);
          end;
        ekText:
          case TextMatchKind of
            tmkMatchTextExact: LCandidates := [vgfMatchTextExact];
            tmkMatchTextBeginning: LCandidates := [vgfMatchTextBeginning];
            tmkMatchTextEnd: LCandidates := [vgfMatchTextEnd];
            tmkMatchTextAnywhere: LCandidates := [vgfMatchTextAnywhere]
          else
            Assert(false);
          end;
        ekSet:
          case SetKind of
            skNumericBetweenInclusive: LCandidates := [vgfNumericBetweenInclusive];
            skNumericBetweenExclusive: LCandidates := [vgfNumericBetweenExclusive];
          else
            Assert(false);
          end;
      else
        Assert(false);
      end;
      Assert(LCandidates<>[]);
    end;

    if Assigned(ASearchCapability) then
      AddFilter(ASearchCapability)
    else
      for LSearchCapability in FSearchCapabilities do
        AddFilter(@LSearchCapability);

    LNewStrFilter := LNewStrFilter + QuotedStr(AExpression) + ',';
  end;

begin
  if not Assigned(FDataSource) then
    exit;

  FFilters.Clear;
  LFormat := DefaultFormatSettings;
  LFormat.DecimalSeparator:='.';
  try
    AddExpression(FSearchEdit.Text, nil);
    // multi column search
    for e in FMultiSearchEdits do
      if Assigned(e.SearchCapability) then
        AddExpression(e.FEdit.Text, e.SearchCapability);
  finally
    // delete last comma
    SetLength(LNewStrFilter, Length(LNewStrFilter)-1);
    if not LException and (FStrFilter <> LNewStrFilter) then
    begin
      FStrFilter:=LNewStrFilter;
      RefreshPageIndexData(false);
    end;
  end;
end;

procedure TCustomVisualGrid.ControlsEnable(AEnable: boolean);
var
  e: TSearchEdit;
  LReadOnly: boolean;
begin
  LReadOnly:=not AEnable;
  FMultiSearchCheckComboBox.Enabled:=AEnable;
  FSearchEdit.ReadOnly:=LReadOnly;
  FPageSizeEdit.ReadOnly:=LReadOnly;
  FPageIndexEdit.ReadOnly:=LReadOnly;
  FDrawGrid.Enabled:=AEnable;
  FBottomRightPanel.Enabled:=AEnable;
  for e in FMultiSearchEdits do
    e.FEdit.ReadOnly:=LReadOnly;
end;

function TCustomVisualGrid.GetCells(ACol, ARow: Integer): Variant;
begin
  Result := ActiveDataTable.Rows[ARow]._(ACol);
end;

function TCustomVisualGrid.GetColCount: Integer;
begin
  Result := FColumns.Count;
end;

function TCustomVisualGrid.GetColumns(Index: Integer): TVisualColumn;
begin
  Result := FColumns[Index];
end;

function TCustomVisualGrid.GetActiveDataTable: PDataTable;
begin
  if FFetchDataThreadTimer.Enabled then
    Result := FCachedDataTable
  else
    Result := @FDataTable;
end;

function TCustomVisualGrid.GetRowCount: Integer;
begin
  Result := Length(ActiveDataTable.Rows);
end;

function TCustomVisualGrid.GetRows(ARow: Integer): Variant;
begin
  Result := ActiveDataTable.Rows[ARow];
end;

function TCustomVisualGrid.GetSelection: TVisualGridSelection;
var
  i: Integer;
begin
  SetLength(Result.Selections, FDrawGrid.SelectedRangeCount);
  for i := 0 to High(Result.Selections) do
  begin
    Result.Selections[i] := FDrawGrid.SelectedRange[i];
    Result.Selections[i].Top:=Result.Selections[i].Top-1; // - fixed row
    Result.Selections[i].Bottom:=Result.Selections[i].Bottom-1; // - fixed row
  end;
end;

function TCustomVisualGrid.GetSelectedRows : TArray<Variant>;
var
  sel : TVisualGridSelection;
  selectedRows : TList<Variant>;
  GC: TScoped;
  row : Integer;
begin
  sel := GetSelection;
  selectedRows := GC.AddObject( TList<Variant>.Create ) as TList<Variant>;
  if sel.RowCount > 0 then
    for row := sel.Row to (sel.Row + sel.RowCount - 1) do
       selectedRows.Add(Self.Rows[row]);
  result := selectedRows.ToArray;
end;

procedure TCustomVisualGrid.PageIndexEditingDone(Sender: TObject);
var
  LPageIndex: Integer;
begin
  if updPageIndex in FGUIUpdates then
    Exit;
  // value in edit has increased value by 1 (more readable for end user)
  LPageIndex := Pred(StrToIntDef(FPageIndexEdit.Text, Succ(FPageIndex)));
  if (LPageIndex < 0) then
  begin
    LPageIndex := 0;
    SetPageIndexEditText('1');
  end;
  if (LPageIndex > FPageCount-1) then
  begin
    LPageIndex := FPageCount-1;
    SetPageIndexEditText(IntToStr(FPageCount));
  end;

  PageIndex := LPageIndex;
end;

procedure TCustomVisualGrid.PageSizeEditingDone(Sender: TObject);
var
  LPageSize: Integer;
begin
  if updPageSize in FGUIUpdates then
    Exit;
  LPageSize:=StrToIntDef(FPageSizeEdit.Text, FPageSize);
  if not CheckRangeForPageSize(LPageSize) then
    SetPageSizeEditText(IntToStr(LPageSize));
  PageSize:=LPageSize;
end;

procedure TCustomVisualGrid.PageNavigationClick(Sender: TObject);
begin
  if FPageCount = 0 then
    Exit;
  case TButton(Sender).Tag of
    PAGE_NAVIGATION_FIRST: PageIndex := 0;
    PAGE_NAVIGATION_PREVIOUS: PageIndex := PageIndex - 1;
    PAGE_NAVIGATION_NEXT: PageIndex := PageIndex + 1;
    PAGE_NAVIGATION_LAST: PageIndex := FPageCount - 1;
  end;
end;

procedure TCustomVisualGrid.MultiSearchCheckComboBoxChange(Sender: TObject; AIndex: Integer);
var
  LState: TCheckComboItemState;
  LOldHasEdit, LNewHasEdit: boolean;

  function HasOneOrMoreEdit: boolean;
  var
    LEdit: TSearchEdit;
  begin
    for LEdit in FMultiSearchEdits do
      if LEdit.EditVisible then
        exit(true);
    Result := false
  end;

begin
  LOldHasEdit := HasOneOrMoreEdit;
  LState := TCheckComboItemState(FMultiSearchCheckComboBox.Items.Objects[AIndex]);
  if Assigned(LState.Data) then
    TSearchEdit(LState.Data).EditVisible:=LState.State=cbChecked
  else
    FSearchEdit.Visible:=LState.State=cbChecked;

  LNewHasEdit := HasOneOrMoreEdit;
  FTopPanelMultiSearch.Visible := LNewHasEdit;

  FSearchButton.Visible:=FSearchEdit.Visible or LNewHasEdit;

  if LOldHasEdit <> LNewHasEdit then
    LayoutChanged;
end;

function TCustomVisualGrid.GetCanvas: TCanvas;
begin
  Result := FDrawGrid.Canvas;
end;

procedure TCustomVisualGrid.SetCells(ACol, ARow: Integer; AValue: Variant);
begin
  TTableRowData(FDataTable.Rows[ARow]).vvalues[ACol] := AValue;
  TDrawGridAccess(FDrawGrid).InvalidateCell(ACol, ARow, true);
end;

procedure TCustomVisualGrid.SetFetchDataInThread(AValue: boolean);
begin
  if FFetchDataInThread=AValue then Exit;
  FFetchDataInThread:=AValue;
end;

procedure TCustomVisualGrid.SetSortMode(AValue: TSortMode);
begin
  if FSortMode=AValue then Exit;
  FSortMode:=AValue;

  SortDirectionGlyphRefresh;
end;

procedure TCustomVisualGrid.SetSearchMode(AValue: TSearchMode);
var
  i: Integer;
  LIsMultiSearch: boolean;
begin
  if AValue = FSearchMode then
    Exit;

  LIsMultiSearch := AValue = smMulti;

  // LIsMultiSearch
  //   true: check almost all (except expression)
  //   false: check only expression
  for i := 0 to FMultiSearchCheckComboBox.Items.Count - 2 do
    FMultiSearchCheckComboBox.Checked[i] := LIsMultiSearch;
  FMultiSearchCheckComboBox.Checked[FMultiSearchCheckComboBox.Items.Count-1] := not LIsMultiSearch;

  // Clear all filters
  for i := 0 to FMultiSearchEdits.Count - 1 do
    FMultiSearchEdits[i].Clear;
  FSearchEdit.Clear;

  FSearchMode := AValue;

  // reload data and don't use any filter for new mode
  FStrFilter := '';
  FFilters.Clear;
  RefreshPageIndexData(false);
end;

procedure TCustomVisualGrid.SetOptions(AValue: TVisualGridOptions);
var
  LSortDirectionAllowNone: boolean;
  LRefreshSelection: boolean;
begin
  if FOptions=AValue then Exit;
  LSortDirectionAllowNone := vgoSortDirectionAllowNone in FOptions;
  LRefreshSelection := vgoAllowDeselect in FOptions;

  FOptions:=AValue;
  if vgoColSizing in FOptions then
    FDrawGrid.Options := FDrawGrid.Options + [goColSizing]
  else
    FDrawGrid.Options := FDrawGrid.Options - [goColSizing];

  FDrawGrid.AutoFillColumns:=vgoColAutoFill in FOptions;
  FMultiSearchCheckComboBox.Visible:=vgoMultiSearchCheckComboBox in FOptions;

  // refresh for sort direction graphic
  if LSortDirectionAllowNone <> (vgoSortDirectionAllowNone in AValue) then
    SortDirectionGlyphRefresh;
  // refresh selection
  if LRefreshSelection <> (vgoAllowDeselect in AValue) then
  begin
    ResetLastSelection;
    UpdateSelection(SelectionType);
  end;
end;

procedure TCustomVisualGrid.SetRows(ARow: Integer; AValue: Variant);
begin
  FDataTable.Rows[ARow] := AValue;
  FDrawGrid.InvalidateRow(ARow + 1); // + fixed row
end;

procedure TCustomVisualGrid.SetShowAllData(AValue: boolean);
begin
  if FShowAllData=AValue then
    Exit;

  FShowAllData:=AValue;
  if FShowAllData then
    AutoPageSize:=false;

  HidePageSizeControls(not AValue);
  HidePageNavigationControls(not AValue);
  PageSize:=FTotalDataCount;
end;

procedure TCustomVisualGrid.SetAutoPageSize(AValue: boolean);

begin
  if FAutoPageSize=AValue then
    Exit;

  FAutoPageSize:=AValue;
  if FAutoPageSize then
    ShowAllData:=false;

  HidePageSizeControls(not FAutoPageSize);

  if FAutoPageSize then
    FDrawGrid.ScrollBars:=ssNone
  else
    FDrawGrid.ScrollBars:=ssAutoBoth;

  PageSize := ClientRowCount;
end;

procedure TCustomVisualGrid.SetCanPage(AValue: boolean);
begin
  if FCanPage=AValue then
    Exit;

  FCanPage:=AValue;
  FBottomPanel.Visible:=FCanPage;
  if csDesigning in ComponentState then
    if FBottomPanel.Visible then
      FBottomPanel.ControlStyle := FBottomPanel.ControlStyle - [csNoDesignVisible]
    else
      FBottomPanel.ControlStyle := FBottomPanel.ControlStyle + [csNoDesignVisible];

  LayoutChanged;
end;

procedure TCustomVisualGrid.SetCanSearch(AValue: boolean);
begin
  if FCanSearch=AValue then
    Exit;

  FCanSearch:=AValue;
  FTopPanel.Visible:=FCanSearch;
  if csDesigning in ComponentState then
    if FTopPanel.Visible then
      FTopPanel.ControlStyle := FTopPanel.ControlStyle - [csNoDesignVisible]
    else
      FTopPanel.ControlStyle := FTopPanel.ControlStyle + [csNoDesignVisible];

  LayoutChanged;
end;

procedure TCustomVisualGrid.RefreshGrid;
begin
  RefreshPageIndexData( False );
end;

procedure TCustomVisualGrid.RefreshPageIndexData(ARefreshColumns: boolean);
begin
  if Assigned(FDataSource) and FetchDataInThread then
  begin
    if not Assigned(FActiveThread) then
      TFetchDataThread.Create(Self, ARefreshColumns)
  end
  else
  begin
    BeforeFetchPage;
    FLastFetchDataResult.FromThread:=false;
    FLastFetchDataResult.RefreshColumns:=ARefreshColumns;
    FetchPage(FLastFetchDataResult.FetchResult);
    AfterFetchPage;
  end;
end;

procedure TCustomVisualGrid.ResizeSearchEdit(ACol: Integer);
var
  LEdit: TSearchEdit;
  LEditOnLeft, LEditOnRight: TSearchEdit;
  //LFixedRect: TRect;
  LRect: TRect;
  xxx : SizeInt;
begin
  xxx := FMultiSearchEdits.Count;
  LEdit := FMultiSearchEdits[ACol];
  LEdit.Visible := FDrawGrid.IsFixedCellVisible(aCol, 0);
  if ACol > 0 then
    LEditOnLeft := FMultiSearchEdits[ACol-1]
  else
    LEditOnLeft := nil;

  if ACol < FMultiSearchEdits.Count-1 then
    LEditOnRight := FMultiSearchEdits[ACol+1]
  else
    LEditOnRight := nil;

  if ACol = TDrawGridAccess(FDrawGrid).GCache.VisibleGrid.Right then
    if Assigned(LEditOnRight) then
      LEditOnRight.Visible:=false;
  if ACol = TDrawGridAccess(FDrawGrid).GCache.VisibleGrid.Left then
    begin
      if Assigned(LEditOnLeft) then
        if (ACol > FDrawGrid.FixedCols) or (not FDrawGrid.IsFixedCellVisible(ACol-1,0)) then
          LEditOnLeft.Visible:=false
    end;
  // TODO : next column after fixed column
  {if (ACol > 0) and (ACol = FDrawGrid.FixedCols) then
  begin
    fr := FDrawGrid.CellRect(aCol-1, aRow);

    e.SetBounds(FDrawGrid.Left + fr.Right + 2, FDrawGrid.Top - e.Height, (aRect.Right - (fr.Right)) -1, e.Height);
  end
  else}
  LRect := FDrawGrid.CellRect(ACol,0);
  LEdit.FPanel.SetBounds(LRect.Left + 1, 0, LRect.Width - 2, LEdit.FEdit.Height);
end;

procedure TCustomVisualGrid.RefreshPageIndexAndGridInterface;
begin
  SetPageIndexEditText(IntToStr(Succ(FPageIndex)));
  FPageCountLabel.Caption := Format('/%d',[FPageCount]);
  FDrawGrid.Refresh;
  if Assigned(FFinishedUpdating) then
    FFinishedUpdating(Self);
end;

procedure TCustomVisualGrid.ReloadColumns;
var
  i: Integer;
  LEdit: TSearchEdit;
  p: PSearchCapability;
  LColumn: TVisualColumn;

  function SearchCapability: PSearchCapability;
  var
    j: Integer;
  begin
    for j := 0 to High(FSearchCapabilities) do
      if FSearchCapabilities[j].ColumnName = FDataTable.Columns[i] then
        Exit(@FSearchCapabilities[j]);
    Result := nil;
  end;

begin
  FSortColumn := nil;
  FDrawGrid.Columns.Clear;
  FDrawGrid.Columns.BeginUpdate;
  FColumns.Clear;
  for i := 0 to High(FDataTable.Columns) do
  begin
    LColumn := TVisualColumn.Create(Self, FDrawGrid.Columns.Add);
    FColumns.Add(LColumn);
    if i <= High(FDefaultColumnWidths) then
      case FDefaultColumnWidths[i] of
        CT_VISUALGRID_DEFAULT: LColumn.StretchedToFill:=False;
        CT_VISUALGRID_STRETCH: LColumn.StretchedToFill:=True;
        else begin
          LColumn.StretchedToFill := False;
          LColumn.SetWidth(FDefaultColumnWidths[i]);
        end;
    end else if i = FDefaultStretchedColumn then LColumn.StretchedToFill:=True;
    LColumn.FColumn.Title.Caption:=''; //FDataTable.Columns[i]; already painted in default drawing event

    // invoke client initialization
    if Assigned(FOnColumnInitialize) then
      FOnColumnInitialize(Self, i, LColumn);
  end;
  FDrawGrid.Columns.EndUpdate;
  // TODO: may be optimized
  FMultiSearchEdits.Clear;
  FSearchButton.Visible := true;
  FMultiSearchCheckComboBox.Clear;
  FSearchCapabilities := Copy(FDataSource.SearchCapabilities);
  for i := 0 to High(FDataTable.Columns) do
  begin
    LEdit := TSearchEdit.Create(FTopPanelMultiSearchClient, Self);
    FMultiSearchEdits.Add(LEdit);
    p := SearchCapability;
    LEdit.EditVisible:=Assigned(p) and FMultiSearchMenuItem.Checked and (p^.SupportedFilters * NON_SORTABLE_FILTER <> []);
    LEdit.SearchCapability := p;
    ResizeSearchEdit(i);
    if Assigned(p) then
    begin
      if p^.SupportedFilters * NON_SORTABLE_FILTER <> [] then
      begin
      FMultiSearchCheckComboBox.AddItem(p^.ColumnName, cbChecked);
      FMultiSearchCheckComboBox.Objects[FMultiSearchCheckComboBox.Items.Count-1] := LEdit;
      FMultiSearchCheckComboBox.Checked[FMultiSearchCheckComboBox.Items.Count-1] := FMultiSearchMenuItem.Checked;
    end;
      if (vgfSortable in p^.SupportedFilters)
       and not (vgoSortDirectionAllowNone in Options)
       and (SortMode <> smNone) then
        FColumns[i].SortDirection := sdAscending;
    end;
  end;
  if FDrawGrid.Columns.Count > 0 then
    FTopPanelMultiSearch.Height:=FMultiSearchEdits.Last.FPanel.Height;

  // last item doesn't need to store object
  FMultiSearchCheckComboBox.AddItem(sExpression, cbChecked);
end;

procedure TCustomVisualGrid.LayoutChanged;
begin
  // layout has changed, maybe more space is available
  if AutoPageSize then
    PageSize := ClientRowCount;
end;

function TCustomVisualGrid.ClientRowCount: Integer;
begin
  Result := ((FDrawGrid.ClientHeight - FDrawGrid.GridLineWidth) div FDrawGrid.DefaultRowHeight) - FDrawGrid.FixedRows;
  if Result = 0 then
    Result := 1;
  FDrawGrid.VisibleRowCount;
end;

procedure TCustomVisualGrid.HidePageSizeControls(AVisible: boolean);
begin
  FPageSizeEdit.Visible:=AVisible;
  FPageSizeLabel.Visible:=AVisible;
end;

procedure TCustomVisualGrid.HidePageNavigationControls(AVisible: boolean);
begin
  FBottomRightPanel.Visible:=AVisible;
end;

function TCustomVisualGrid.CheckRangeForPageSize(var APageSize: Integer
  ): boolean;
begin
  if APageSize <= 0 then
  begin
    APageSize:=FPageSize;
    Exit(False);
  end
  else if APageSize > 1000000 then
  begin
    APageSize:=1000000;
    Exit(False);
  end;
  Result := True;
end;

procedure TCustomVisualGrid.SetDataSource(ADataSource: IDataSource);
begin
  if FDataSource = ADataSource then
    Exit;

  FDataSource := ADataSource;

  RefreshPageIndexData(true);
end;

procedure TCustomVisualGrid.SetPageIndex(Value: Integer);
begin
  if Value >= FPageCount then
    Value := FPageCount - 1;
  if Value < 0 then
    Value := 0;

  if FPageIndex = Value then
    Exit;

  FPageIndex := Value;
  RefreshPageIndexData(false)
end;

procedure TCustomVisualGrid.SetPageIndexEditText(const AStr: utf8string);
begin
  Include(FGUIUpdates, updPageIndex);
  FPageIndexEdit.Text := AStr;
  Exclude(FGUIUpdates, updPageIndex);
end;

procedure TCustomVisualGrid.SetPageSizeEditText(const AStr: utf8string);
begin
  Include(FGUIUpdates, updPageSize);
  FPageSizeEdit.Text := AStr;
  Exclude(FGUIUpdates, updPageSize);
end;

procedure TCustomVisualGrid.BeforeFetchPage;
begin
  if Assigned(FDataSource) then
    if FPageIndex >= FPageCount then
      FPageIndex := FPageCount - 1;
end;

procedure TCustomVisualGrid.FetchPage(out AResult: TPageFetchResult);

  // additionall fill with sort direction
  procedure FillFilter;
  var
    LColumnsToAdd: TList<Integer>;
    i, j, idx: Integer;
    LFilter: TColumnFilter;
    LData: PDataTable;

    function UpdateFilterSortDirection(const AColumnName: utf8string; ASortDirection: TSortDirection): boolean;
    var
      i: Integer;
    begin
      for i := 0 to FFilters.Count-1 do
        if FFilters[i].ColumnName = AColumnName then
        begin
          LFilter := FFilters[i];
          LFilter.Sort := ASortDirection;
          FFilters[i] := LFilter;
          Exit(True);
        end;
      Result := False;
    end;

  begin
    LColumnsToAdd := TList<Integer>.Create;

    for i := FFilters.Count-1 downto 0 do
      if FFilters[i].Filter=vgfSortable then
        FFilters.Delete(i);
    LData := GetActiveDataTable;
    try
      for i := 0 to FColumns.Count - 1 do
        if FColumns[i].SortDirection <> sdNone then
        begin
          // try to find column in existing filters
          // if filter not found we need to create it later
          if not UpdateFilterSortDirection(LData.Columns[i], FColumns[i].SortDirection) then
            LColumnsToAdd.Add(i);
        end else
          UpdateFilterSortDirection(LData.Columns[i], sdNone);

      // add missing filters
      FFilters.Count:=FFilters.Count + LColumnsToAdd.Count;
      j := 0;
      for i := FFilters.Count - LColumnsToAdd.Count to FFilters.Count-1 do
      begin
        // real column index
        idx := LColumnsToAdd[j];
        inc(j);
        // create new filter
        LFilter := Default(TColumnFilter);
        LFilter.Filter:=vgfSortable;
        LFilter.ColumnName:=LData.Columns[idx];
        LFilter.Sort := FColumns[idx].SortDirection;
        FFilters[i] := LFilter;
      end;
    finally
      LColumnsToAdd.Free;
    end;
  end;

begin
  if Assigned(FDataSource) then
  begin
    FillFilter;
    AResult := FDataSource.FetchPage(TPageFetchParams.Create(FPageIndex, FPageSize, FFilters.ToArray, IIF(FSearchMode = smMulti, foAnd, foOr)), FDataTable)
  end
  else
    FillChar(AResult, SizeOf(AResult), #0);
end;

procedure TCustomVisualGrid.AfterFetchPage;
begin
  with FLastFetchDataResult do
  if Assigned(FDataSource) then
  begin
    if FromThread then
    begin
      FActiveThread := nil;
      Dispose(FCachedDataTable);
      FCachedDataTable := nil;
      FFetchDataThreadTimer.Enabled:=false;
      FLoadDataPanel.Visible:=False;
      ControlsEnable(true);
    end;

    FPageCount:=FetchResult.PageCount;

    if FetchResult.TotalDataCount >= 0 then
      FTotalDataCount := FetchResult.TotalDataCount
    else
      FTotalDataCount := -1;

    FAllRecordsCountLabel.Visible := FTotalDataCount<>-1;
    FAllRecordsCountLabel.Caption:=Format(sTotal, [FTotalDataCount]);

    FPageIndex := FetchResult.PageIndex;

    if RefreshColumns then
      ReloadColumns;
    FDrawGrid.RowCount := Length(FDataTable.Rows) + 1;
  end;
  RefreshPageIndexAndGridInterface;
end;

procedure TCustomVisualGrid.UpdateSelection(ASelectionType: TSelectionType);
var
  LSelectionEvent: boolean;
begin
  if FCurrentSelectionType = ASelectionType then
    Exit;

  FCurrentSelectionType:=ASelectionType;
  LSelectionEvent := FCurrentSelectionType=stNone;
  case FCurrentSelectionType of
    stNone: FDrawGrid.Options:=FDefaultDrawGridOptions;
    stCell: FDrawGrid.Options:=FDefaultDrawGridOptions+[goDrawFocusSelected];
    stRow: FDrawGrid.Options:=FDefaultDrawGridOptions+[goRowSelect];
    stMultiRow: FDrawGrid.Options:=FDefaultDrawGridOptions+[goRowSelect,goRangeSelect];
  end;
  if LSelectionEvent and Assigned(FOnSelection) then
      FOnSelection(Self, FLastSelection);
end;

function TCustomVisualGrid.SelectionsEquals(constref A, B: TVisualGridSelection
  ): boolean;
begin
  case FSelectionType of
    stNone: Result := false;
    stCell: Result := (A.Col = B.Col) and (A.Row = B.Row);
    stRow: Result := (A.Row = B.Row);
    stMultiRow: Result := (A.Row = B.Row) and (A.RowCount = B.RowCount);
  end;
end;

procedure TCustomVisualGrid.ResetLastSelection;
begin
  FLastSelection.Selections := nil;
end;

procedure TCustomVisualGrid.SetPageSize(Value: Integer);
begin
  if FPageSize = Value then
    Exit;

  CheckRangeForPageSize(Value);
  FPageSize := Value;
  SetPageSizeEditText(IntToStr(FPageSize));
  RefreshPageIndexData(false);
end;

procedure TCustomVisualGrid.SetSelectionType(AValue: TSelectionType);
begin
  if FSelectionType=AValue then
    Exit;

  FSelectionType:=AValue;
  UpdateSelection(FSelectionType);

  GridSelection(Self, 0, 0);
end;

procedure TCustomVisualGrid.SetWidgetControl(AValue: TControl);
begin
  if FWidgetControl=AValue then Exit;

  if Assigned(FWidgetControl) then
    FWidgetControl.Parent := FWidgetControlParent;

  FWidgetControl:=AValue;

  if Assigned(AValue) then
  begin
    FTopPanelLeft.Width:=FWidgetControl.Width + 4 + 4;
    if FWidgetControl.Height > FTopPanelLeft.Height then
      FWidgetControl.Height := FTopPanelLeft.Height;

    FWidgetControlParent := FWidgetControl.Parent;
    FWidgetControl.Parent := FTopPanelLeft;
    with FWidgetControl do
    begin
      AnchorSideLeft.Control := FTopPanelLeft;
      AnchorSideLeft.Side := asrCenter;
      AnchorSideTop.Control := FTopPanelLeft;
      AnchorSideTop.Side := asrCenter;
    end;
  end else
    FTopPanelLeft.Width:=0;
end;

procedure TCustomVisualGrid.StandardDrawCell(Sender: TObject; ACol,
  ARow: Longint; Rect: TRect; State: TGridDrawState);
var
  LHandled: boolean;
  LCellData: Variant;
begin
  LHandled := False;

  if ColCount = 0 then
    Exit;

  if ARow = 0 then
    ResizeSearchEdit(ACol);
  if (ARow > 0) and Assigned(FDataSource) then
    LCellData := ActiveDataTable^.Rows[ARow-1]._(ACol);

  if Assigned(FOnDrawVisualCell) then
    FOnDrawVisualCell(Self, ACol, ARow, Canvas, Rect, State, LCellData, LHandled);
  if not LHandled then
    DoDrawCell(Self, ACol, ARow, Rect, State, LCellData);
end;

procedure TCustomVisualGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LPopup: TPopupMenu;
  LSelection: TVisualGridSelection;
  i: integer;
  LCol, LRow: longint;
  LContains: boolean = false;
begin
  if ColCount = 0 then
    Exit;
  case Button of
    mbRight:
      if (SelectionType <> stNone) and Assigned(FOnPreparePopupMenu) and
         (FDrawGrid.MouseToGridZone(X, Y) = gzNormal) then
      begin
        FDrawGrid.MouseToCell(X, Y, LCol, LRow);
        // fixed rows
        Dec(LRow);
        LSelection := Selection;
        for i := 0 to High(LSelection.Selections) do
          if not LContains then
          begin
            with LSelection.Selections[i] do
              LContains := (LCol >= Left) and (LRow >= Top) and (LCol <= Right) and (LRow <= Bottom);
            Break;
          end;
        if not LContains then
          Exit;
        FOnPreparePopupMenu(Self, LSelection, LPopup);
        if Assigned(LPopup) then
          with FDrawGrid.ClientToScreen(TPoint.Create(X, Y)) do
            LPopup.PopUp(X, Y);
      end;
    mbLeft:
      if (SelectionType <> stNone) and (vgoAllowDeselect in FOptions) and
       (FDrawGrid.MouseToGridZone(X, Y) = gzNormal) then
      begin
        LSelection := GetSelection;
        if (FCurrentSelectionType <> stNone) and SelectionsEquals(LSelection, FLastSelection) then
        begin
          ResetLastSelection;
          UpdateSelection(stNone)
        end
        else begin
          // lock for select event, otherwise we get false information about selection
          // (highly visible problem for deselection option)
          FIgnoreSelectionEvent := true;
          UpdateSelection(FSelectionType);
          FIgnoreSelectionEvent := false;
          FLastSelection := LSelection;
          GridSelection(nil, 0, 0);
        end;
      end;
  end;
end;

procedure TCustomVisualGrid.SearchKindPopupMenuClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:=true;
  SearchMode := IIF(Sender = FMultiSearchMenuItem, smMulti, smSingle);
end;

procedure TCustomVisualGrid.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if (FCurrentSelectionType <> stNone) and Assigned(FOnSelection) and (not FIgnoreSelectionEvent) then
    FOnSelection(Self, Selection);
end;

procedure TCustomVisualGrid.GridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  LColumn: TVisualColumn;
begin
  if ColCount = 0 then
    Exit;
  if Index >= Length(ActiveDataTable.Columns) then
    raise EVisualGridError.CreateFmt(sImproperColumnIndex, [Length(ActiveDataTable.Columns)-1,Index]);

  if FSortMode = smNone then
    Exit;

  LColumn := GetColumns(Index);
  case LColumn.SortDirection of
    sdNone: LColumn.SortDirection := sdAscending;
    sdAscending: LColumn.SortDirection := sdDescending;
    sdDescending:
      if vgoSortDirectionAllowNone in Options then
        LColumn.SortDirection := sdNone
      else
        LColumn.SortDirection := sdAscending;
  end;
  if Assigned(FSortColumn) and (FSortColumn<>LColumn) then
    if SortMode=smSingleColumn then
    begin
      FSortColumn.FIgnoreRefresh:=true;
      FSortColumn.SortDirection:=sdNone;
      FSortColumn.FIgnoreRefresh:=false;
    end;
  FSortColumn := LColumn;
end;

initialization
  {$I *.inc}

end.

