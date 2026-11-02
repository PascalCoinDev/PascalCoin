unit UVisualGrid;

{ Copyright (c) 2017 - 2018 Herman Schoenfeld <https://www.sphere10.com>

  Data-bound grid with filtering, sorting and paging for Delphi/VCL and Lazarus/LCL.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Herman Schoenfeld: main author

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{ Public types in this unit

  Type                                   | Purpose
  ---------------------------------------+-----------------------------------------------------------------------
  EVisualGridError                       | Reports invalid grid configuration or operations.
  TCheckComboBox                         | Supplies the VCL column-picker control used by the grid.
  TCheckComboItemChange                  | Notifies a VCL column-picker checkbox change.
  TColumnInitializeEvent                 | Configures a column when its native counterpart is created.
  TCustomVisualGrid                      | Coordinates rendering, paging, filtering, loading and refreshes.
  TDeselectionType                       | Selects how existing grid selections may be cleared.
  TDrawGrid                              | Adapts VCL drawing and geometry for the exposed native grid.
  TDrawVisualCellEvent                   | Customizes cell painting using its value and row data.
  TGridColumn                            | Aliases the VCL header-section column metadata.
  TGridZone                              | Identifies fixed, ordinary or invalid VCL grid cells.
  TPreparePopupMenuEvent                 | Supplies a context menu for the current selection.
  TSearchMode                            | Selects a single expression or per-column search.
  TSelectionEvent                        | Notifies changes to selected cells or rows.
  TSelectionType                         | Selects cell, row or multiple-row selection.
  TSortMode                              | Selects no sorting, one column or multiple columns.
  TVisualCellRenderers                   | Supplies reusable cell-drawing callbacks.
  TVisualColumn                          | Configures a column's binding, layout, filters and appearance.
  TVisualColumnDataSanitizer             | Converts source values before rendering a cell.
  TVisualColumnRenderer                  | Renders a cell using its value, row and draw state.
  TVisualGrid                            | Publishes the reusable grid's design-time properties and events.
  TVisualGridCaption                     | Exposes the grid caption's text, font, visibility and alignment.
  TVisualGridCellSelected                | Notifies native VCL cell selection.
  TVisualGridHeaderClick                 | Notifies native VCL column or row-header clicks.
  TVisualGridOptions                     | Configures column sizing, searching and paging presentation.
  TVisualGridPageMode                    | Selects all rows, a viewport-sized page or a manual page size.
  TVisualGridSearchExpressionService     | Provides the grid search-service extension point.
  TVisualGridSelection                   | Describes selected cells, rows and their associated data.
}


{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

{$IFDEF FPC}{$modeswitch nestedprocvars}{$ENDIF}

{.$DEFINE VISUALGRID_DEBUG}

interface

uses
  Classes, SysUtils, Types, Math, Generics.Collections, Generics.Defaults, SyncObjs,
  {$IFDEF FPC}
  StdCtrls, ExtCtrls, Controls, Grids, Graphics, Menus, ComboEx, Buttons, LResources,
  {$ELSE}
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, Vcl.Grids, Vcl.Graphics, Vcl.Menus,
  Vcl.Buttons, Vcl.ComCtrls, Vcl.CheckLst, Vcl.Forms, Winapi.Windows, Winapi.Messages,
  {$ENDIF}
  UCommon, UCommon.UI, UCommon.Data, UCommon.Collections;

{$IFNDEF FPC}
  {$DEFINE VISUALGRID_VCL_DECLARATIONS}
  {$I UVisualGrid.VCL.inc}
  {$UNDEF VISUALGRID_VCL_DECLARATIONS}
{$ENDIF}

 type

   { Forward Decls }

   TCustomVisualGrid = class;
   TVisualColumn = class;


  { TSelectionType }

  TSelectionType = (stNone, stCell, stRow, stMultiRow);

  { TDeselectionType }

  TDeselectionType = (
    dtNone,    { deselection is disallowed }
    dtDefault, { click on selection means deselect (except multirow which is special case), click outside selection means new selection }
    dtClick    { each click anywhere if anything is selected means deselection }
  );

  { TVisualGridSelection }

  TVisualGridSelection = record
  private
    function GetCol: longint;
    function GetColCount: longint;
    function GetRow: longint;
    function GetRowCount: longint;
  public
    Page: Integer;
    Selections: array of TRect;
    property Col: longint read GetCol;
    property Row: longint read GetRow;
    property RowCount: longint read GetRowCount;
    property ColCount: longint read GetColCount;
  end;

  TVisualColumnRenderer = procedure(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean) of object;
  TVisualColumnDataSanitizer = function(const CellData, RowData : Variant) : Variant of object;

  { TVisualColumn }

  TVisualColumn = class
  private
    function GetStretchedToFill: boolean; inline;
    function GetWidth: Integer; inline;
    procedure SetSortDirection(AValue: TSortDirection);
    procedure SetStretchedToFill(AValue: boolean); inline;
    procedure SetAutoWidth(AValue: boolean); inline;
    procedure SetWidth(AValue: Integer); inline;
    procedure SetName(const AValue: String);
    procedure SetBinding(const AValue : String);
    procedure SetHeaderAlignment(const AAlignment : TAlignment);
    procedure SetHeaderFontStyles(const AStyles : TFontStyles);
    procedure SetDataAlignment(const AAlignment : TAlignment);
    procedure SetDataFontStyles(const AStyles : TFontStyles);
  protected
    FColumn: TGridColumn;
    FGrid: TCustomVisualGrid;
    FSortDirection: TSortDirection;
    FIgnoreRefresh: Boolean;

    // Functional
    FIndex : Integer;
    FName : String;
    FFilters : TDataFilters;
    FBinding : String;
    FSortBinding: String;
    FDisplayBinding: String;
    FSanitizer : TVisualColumnDataSanitizer;
    FWidth : Integer;
    FStretchColumn : Boolean;
    FAutoWidth : boolean;
    FRenderer : TVisualColumnRenderer;
    FVisible : boolean; // TODO: implement this functionality
    FSortSequence : Integer; // TODO: implement this functionality

    // Style
    FHasHeaderAlignment : boolean;
    FHasHeaderFontStyles : boolean;
    FHeaderAlignment : TAlignment;
    FHeaderFontStyles : TFontStyles;
    FHasDataAlignment : boolean;
    FHasDataFontStyles : boolean;
    FDataAlignment : TAlignment;
    FDataFontStyles : TFontStyles;

  public
    constructor Create(AGrid: TCustomVisualGrid);
    property Index : Integer read FIndex write FIndex;
    property Name : String read FName write SetName;
    property Filters : TDataFilters read FFilters write FFilters;
    property Binding : String read FBinding write SetBinding;
    property SortBinding : String read FSortBinding write FSortBinding;
    property DisplayBinding : String read FDisplayBinding write FDisplayBinding;
    property Sanitizer: TVisualColumnDataSanitizer read FSanitizer write FSanitizer;
    property AutoWidth : boolean read FAutoWidth write SetAutoWidth;
    property Visible : boolean read FVisible write FVisible;
    property SortSequence : Integer read FSortSequence write FSortSequence;
    property InternalColumn : TGridColumn read FColumn;
    property StretchedToFill: boolean read GetStretchedToFill write SetStretchedToFill;
    property Width: Integer read GetWidth write SetWidth;
    property SortDirection: TSortDirection read FSortDirection write SetSortDirection;
    property Renderer : TVisualColumnRenderer read FRenderer write FRenderer;
    property HasHeaderAlignment : boolean read FHasHeaderAlignment;
    property HasHeaderFontStyles : boolean read FHasHeaderFontStyles;
    property HeaderAlignment : TAlignment read FHeaderAlignment write SetHeaderAlignment;
    property HeaderFontStyles : TFontStyles read FHeaderFontStyles write SetHeaderFontStyles;
    property HasDataAlignment : boolean read FHasDataAlignment;
    property HasDataFontStyles : boolean read FHasDataFontStyles;
    property DataAlignment : TAlignment read FDataAlignment write SetDataAlignment;
    property DataFontStyles : TFontStyles read FDataFontStyles write SetDataFontStyles;

    procedure AttachGridColumn(AGridColumn : TGridColumn);
  end;

  { TVisualGrid Events }

  TPreparePopupMenuEvent = procedure(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection; out APopupMenu: TPopupMenu) of object;
  TSelectionEvent = procedure(Sender: TObject; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} ASelection: TVisualGridSelection) of object;
  TDrawVisualCellEvent = procedure(Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean) of object;
  TColumnInitializeEvent = procedure(Sender: TObject; AColumn : TVisualColumn) of object;

  { TVisualGrid Exceptions }

  EVisualGridError = class(Exception);

  { TVisualGridOptions }

  TVisualGridOptions = set of (vgoColAutoFill, vgoColSizing, vgoAutoHidePaging, vgoAutoHideSearchPanel, vgoMultiSearchCheckComboBox, vgoSortDirectionAllowNone);

  { TSortMode }

  TSortMode = (smNone, smSingleColumn, smMultiColumn);

  { TSearchMode }

  TSearchMode = (smSingle, smMulti);

  { TVisualGridPageMode }

  TVisualGridPageMode = (vpmSingle, vpmAuto, vpmManual);

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
      Column: TVisualColumn;
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
    FLoadingSubtitle: TLabel;
    FLoadingImage: TImage;
    FLoadingCircle : TLoadingCircle;
    FLoadingStartedAt : QWord;
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
    FPageModeButton: TBitBtn;
    FPageModeMenu: TPopupMenu;

    FDrawGrid: TDrawGrid;
    FDelayedBoundsChangeTimer: TTimer;
    FLoadingDelayTimer: TTimer;
    FEditingDoneTimer: TTimer;
    FSearchKindPopupMenu: TPopupMenu;
    FSingleSearchMenuItem: TMenuItem;
    FMultiSearchMenuItem: TMenuItem;

    FMultiSearchEdits: TObjectList<TSearchEdit>;
    FColumns: TObjectList<TVisualColumn>;
  protected { events for UI }
    procedure StandardDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SearchKindPopupMenuClick(Sender: TObject);
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure GridHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure PageIndexEditingDone(Sender: TObject);
    procedure PageSizeEditingDone(Sender: TObject);
    procedure PageModeChanged(Sender: TObject);
    procedure PageModeButtonClick(Sender: TObject);
    procedure PageNavigationClick(Sender: TObject);
    procedure MultiSearchCheckComboBoxChange(Sender: TObject; AIndex: Integer);
    procedure GridGeometryChanged(Sender: TObject);
    procedure ScheduleBoundsChange;
    procedure DelayedBoundsChange(Sender: TObject);
    procedure FetchDataThreadCompleted(Sender: TObject);
    procedure ShowDelayedLoading(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    {$IFNDEF FPC}procedure LoadSearchGlyph;{$ENDIF}
  private
    FCaption: TVisualGridCaption;
    FFetchDataInThread: boolean;
    FActiveThread: TThread;
    FFetchCompletion: TThreadNotify;
    FFetchInProgress, FJoiningFetch: Boolean;
    FShuttingDown: Boolean;
    FManualLoading, FManualLoadingDisablesInput: Boolean;
    FFetchControlsEnabled, FAutomaticLoading: Boolean;
    FManualLoadingTitle, FManualLoadingSubtitle: String;
    FAutomaticLoadingTitle, FLoadingError: String;
    FRefreshPending, FRefreshColumnsPending: Boolean;
    FRefreshThrottle: TThrottledEvent;
    FRefreshReady, FRefreshClientPending, FExecutingRefresh, FRefreshCancelled: Boolean;
    FRefreshSuspended, FActiveRefreshCancelled: Boolean;
    FInternalLayoutDepth: Integer;
    FPendingRefreshParams, FActiveRefreshParams: TPageFetchParams;
    FPendingRefreshSource, FActiveRefreshSource: IDataSource;
    FBoundsClientPending, FBoundsAlreadyNotified: Boolean;
    FOnPreparePopupMenu: TPreparePopupMenuEvent;
    FOnSelection: TSelectionEvent;
    FOnFinishedUpdating: TNotifyEvent;
    FOptions: TVisualGridOptions;
    FSortMode: TSortMode;
    FSearchMode : TSearchMode;
    FShowAllData: boolean;
    FAutoPageSize: boolean;
    FPageMode: TVisualGridPageMode;
    FModeScrollResetPending: Boolean;
    FManualPageSize: Integer;
    FCanPage: boolean;
    FCanSearch: boolean;
    FSelectionType: TSelectionType;
    FDeselectionType: TDeselectionType;
    FCurrentSelectionType: TSelectionType;
    FLastSelection: TVisualGridSelection;
    FLastSelectionEvent: TVisualGridSelection;
    FIgnoreSelectionEvent: boolean;
    FIgnoreRecalcPageCount: boolean;
    FCellPadding : TRect;
    FWidgetControl: TControl;
    FWidgetControlParent: TWinControl;
    function GetCells(ACol, ARow: Integer): Variant;
    function GetColCount: Integer; inline;
    function GetColumn(Index: Integer): TVisualColumn;
    function GetActiveDataTable: PDataTable;
    function GetRowCount: Integer; inline;
    function GetIsUpdating : Boolean; inline;
    function GetRows(ARow: Integer): Variant;
    function GetSelection: TVisualGridSelection;
    function GetSelectedRows : TArray<Variant>;
    procedure ControlsEnable(AEnable: boolean);
    function CaptureRefreshParams: TPageFetchParams;
    procedure QueueRefresh(ARefreshColumns, AClientInput, ANotify: Boolean);
    procedure ExecutePendingRefresh(AOrdinaryAdmission: Boolean = False);
    procedure RefreshThrottleDelivery(Sender: TObject);
    procedure RefreshThrottleWaitingChanged(Sender: TObject);
    procedure RefreshThrottleFinished(Sender: TObject);
    procedure ChangePageSize(Value: Integer; AClientInput, ANotify: Boolean);
    procedure UpdatePagingControls;
    procedure ResetModeScrollPosition(AClearPending: Boolean);
    procedure UpdatePageModeGlyph;
    function CanChangePageMode: Boolean;
    procedure ApplyControlsState;
    procedure RefreshLoadingPresentation;
    function GetCanvas: TCanvas;
    procedure SetCells(ACol, ARow: Integer; AValue: Variant);
    procedure SetFetchDataInThread(AValue: boolean);
    procedure SetRefreshSuspended(AValue: Boolean);
    procedure SetSortMode(AValue: TSortMode);
    procedure SetSearchMode(AValue: TSearchMode);
    procedure SetOptions(AValue: TVisualGridOptions);
    procedure SetRows(ARow: Integer; AValue: Variant);
    procedure SetShowAllData(AValue: boolean);
    procedure SetAutoPageSize(AValue: boolean);
    procedure SetPageMode(AValue: TVisualGridPageMode);
    procedure SetManualPageSize(AValue: Integer);
    procedure SetCanPage(AValue: boolean);
    procedure SetCanSearch(AValue: boolean);
{$IFDEF VISUALGRID_DEBUG}
    procedure ClickTest(Sender: TObject);
{$ENDIF}
    procedure SetPageIndex(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetSelectionType(AValue: TSelectionType);
    procedure SetDeselectionType(AValue: TDeselectionType);
    procedure SetWidgetControl(AValue: TControl);
    function CalculateCellContentRect(const ARect : TRect) : TRect;
  protected { TComponent }
    procedure Loaded; override;
  protected { TControl }
    procedure LayoutLoadingPanel(Sender : TObject);
    procedure BringLoadingPanelToFront;
    procedure BeginLoading;
    procedure UpdateLoading;
    procedure EndLoading;
    procedure ShowLoadingError(const AMessage : String);
    {$IFDEF FPC}procedure BoundsChanged; override;{$ELSE}procedure Resize; override;
    function Scale96ToFont(AValue:Integer):Integer;
    {$ENDIF}
    procedure SetParent(AParent:TWinControl); override;
  protected
    FGUIUpdates: TUpdateOfVisualGridGUI;
    FDataTable: TDataTable;
    FCachedDataTable: PDataTable;
    FDataSource: IDataSource;
    FStrFilter: UTF8String;
    FFilters: TList<TColumnFilter>;
    FPageSize: Integer;
    FPageIndex: Integer;
    FPageCount: Integer;
    FDefaultDrawGridOptions: TGridOptions;
    FTotalDataCount: Integer;
    FLastFetchDataResult: TLastFetchDataResult;
    FSortColumn: TVisualColumn;
    FDefaultStretchedColumn : Integer;

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
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState; const RowData: Variant);
    procedure RefreshPageIndexAndGridInterface;
    procedure RefreshPageIndexData(ARefreshColumns: boolean);
    procedure ResizeSearchEdit(ACol: Integer);
    procedure SetPageIndexEditText(const AStr: String);
    procedure SetPageSizeEditText(const AStr: String);
    procedure BeforeFetchPage;
    procedure FetchPage(out AResult: TPageFetchResult);
    procedure AfterFetchPage;
    property ActiveDataTable: PDataTable read GetActiveDataTable;
    procedure UpdateSelection(ASelectionType: TSelectionType; AResetSelection: boolean);
    function SelectionsEquals({$IFDEF FPC}constref{$ELSE}const{$ENDIF} A, B: TVisualGridSelection): boolean;
    procedure ResetSelection(out ASelection: TVisualGridSelection); inline;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    property DataSource: IDataSource read FDataSource write SetDataSource;
    property PageSize: Integer read FPageSize write SetPageSize default 100;
    // Single returns all matching rows; Auto fits the viewport; Manual uses
    // ManualPageSize. Mode changes start at page zero and preserve manual size.
    property PageMode: TVisualGridPageMode read FPageMode write SetPageMode default vpmManual;
    property ManualPageSize: Integer read FManualPageSize write SetManualPageSize default 100;
    property PageIndex: Integer read FPageIndex write SetPageIndex default -1;
    property AutoPageSize: boolean read FAutoPageSize write SetAutoPageSize default false;
    property ShowAllData: boolean read FShowAllData write SetShowAllData default false;
    property FetchDataInThread: boolean read FFetchDataInThread write SetFetchDataInThread;
    // True while an asynchronous page fetch is running or awaiting UI completion.
    property IsUpdating : Boolean read GetIsUpdating;
    // UI-thread refresh admission for every client request. temNone preserves
    // immediate behavior; burst modes retain only the latest pending parameters.
    // The grid owns this component. Internal worker/layout replay never notifies it.
    property RefreshThrottle: TThrottledEvent read FRefreshThrottle;
    // UI-thread policy boundary. Requests and visual layout continue while
    // suspended, but no provider query is admitted. Active work is cancelled
    // cooperatively and its result is ignored; this setter never joins it.
    // Cancel RefreshThrottle while suspended to resume immediately, otherwise
    // the retained request still respects its current quiet interval.
    property RefreshSuspended: Boolean read FRefreshSuspended write SetRefreshSuspended;
    // UI-thread cooperative cancellation of the active asynchronous fetch only.
    // Never joins, queues input or changes throttle, suspension or manual cards.
    // Existing rows remain; the provider must outlive its normal retirement.
    procedure CancelActiveFetch;
    // UI-thread presentation only. The graphic is copied; its owner may free it
    // immediately. A nil/empty graphic uses the existing loading circle.
    // Repeated calls replace this manual state without fetching any data.
    procedure ShowLoading(const ATitle: String; const ASubtitle: String = '';
      AImage: TGraphic = nil; ADisableInput: Boolean = True);
    // Releases only manual presentation, retaining a current fetch/error card
    // and its input restrictions. Neither method changes this grid's Enabled.
    procedure HideLoading;
    // True for a held manual overlay, independently of worker-only IsUpdating.
    property LoadingVisible: Boolean read FManualLoading;
    property DefaultStretchedColumn: Integer read FDefaultStretchedColumn write FDefaultStretchedColumn;

    property CanPage: boolean read FCanPage write SetCanPage default true;
    property CanSearch: boolean read FCanSearch write SetCanSearch default true;
    property Options: TVisualGridOptions read FOptions write SetOptions;
    property Canvas: TCanvas read GetCanvas;
    property SelectionType: TSelectionType read FSelectionType write SetSelectionType;
    property DeselectionType: TDeselectionType read FDeselectionType write SetDeselectionType;
    property Selection: TVisualGridSelection read GetSelection;
    property SelectedRows : TArray<Variant> read GetSelectedRows;
    property SortMode: TSortMode read FSortMode write SetSortMode;
    property SearchMode : TSearchMode read FSearchMode write SetSearchMode;

    property Caption: TVisualGridCaption read FCaption write FCaption;
    property CellPadding : TRect read FCellPadding write FCellPadding;
    property ColCount: Integer read GetColCount;
    property Columns[Index: Integer]: TVisualColumn read GetColumn;
    property Cells[ACol, ARow: Integer]: Variant read GetCells write SetCells;
    property RowCount: Integer read GetRowCount;
    property Rows[ARow: Integer]: Variant read GetRows write SetRows;

    property OnColumnInitialize : TColumnInitializeEvent read FOnColumnInitialize write FOnColumnInitialize;
    property OnDrawVisualCell: TDrawVisualCellEvent read FOnDrawVisualCell write FOnDrawVisualCell;
    property OnSelection: TSelectionEvent read FOnSelection write FOnSelection;
    property OnPreparePopupMenu: TPreparePopupMenuEvent read FOnPreparePopupMenu write FOnPreparePopupMenu;
    // Raised once after page population, layout and selection restoration.
    // Resizing an already populated page does not raise a fetch completion.
    property OnFinishedUpdating : TNotifyEvent read FOnFinishedUpdating write FOnFinishedUpdating;

    property WidgetControl: TControl read FWidgetControl write SetWidgetControl;
    property InternalDrawGrid : TDrawGrid read FDrawGrid;
    function AddColumn(const AName: String) : TVisualColumn;
    procedure RefreshGrid;
    procedure ClearSelection(AIgnoreDeselectionType: boolean = false);
  end;

  { TVisualGrid }

  TVisualGrid = class(TCustomVisualGrid)
  published
    property Caption;

    property Align;
    property PageSize;
    property PageMode;
    property ManualPageSize;
    property AutoPageSize;
    property ShowAllData;
    property CanPage;
    property CanSearch;
    property Options;
    property SelectionType;
    property DeselectionType;
    property SortMode;
    property SearchMode;
    property FetchDataInThread;
    property WidgetControl;

    property OnDrawVisualCell;
    property OnSelection;
    property OnPreparePopupMenu;
  end;

  { TVisualCellRenderers }

  TVisualCellRenderers = class
    public
      class procedure DollarValue (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
  end;

  { TVisualGridSearchExpressionService }

  TVisualGridSearchExpressionService = class
  end;

{$IFDEF VISUALGRID_TEST_WORKER_START}
var
  // Dedicated test builds can force cancellation before the RTL schedules Execute.
  VisualGridBeforeWorkerStart: TNotifyEvent;
{$ENDIF}

procedure Register;

implementation

uses Variants, UMemory,
  {$IFDEF FPC}Dialogs;{$ELSE}Vcl.Dialogs, Vcl.Imaging.pngimage;{$ENDIF}

{$IFNDEF FPC}
{$R UVisualGrid.VCL.res}
{$I UVisualGrid.VCL.inc}

procedure DrawGridText(ACanvas:TCanvas; ARect:TRect; const AText:string; AAlignment:TAlignment);
var LFlags:Cardinal;
begin
  LFlags:=DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX;
  case AAlignment of
    taRightJustify: LFlags:=LFlags or DT_RIGHT;
    taCenter: LFlags:=LFlags or DT_CENTER;
  end;
  Winapi.Windows.DrawText(ACanvas.Handle,PChar(AText),Length(AText),ARect,LFlags);
end;
{$ENDIF}

resourcestring
  sTotal = 'Total: %d';
  sStandardSearch = 'Standard Search';
  sMultiColumnSearch = 'Multi-Column Search';
  sPageSize = 'Page size:';
  sSearchExpression = 'Search expression';
  sDataLoading = 'Loading...';
  sDataUpdating = 'Updating...';
  sExpression = 'Expression';
  sImproperColumnIndex = 'Improper column index. Max expected is %d but %d found.';

const
  CT_DEFAULT_CELL_PADDING_LEFT = 0;
  CT_DEFAULT_CELL_PADDING_TOP = 0;
  CT_DEFAULT_CELL_PADDING_RIGHT = 4;
  CT_DEFAULT_CELL_PADDING_BOTTOM = 0;

type
  TDrawGridAccess = class(TDrawGrid);
  //TScrollBarAccess = class(TScrollBar);

  { TFetchDataThread }

  TFetchDataThread = class(TThread)
  protected
    FGrid: TCustomVisualGrid;
    FLastFetchDataResult: TCustomVisualGrid.TLastFetchDataResult;
    FIgnoreRecalcPageCount: boolean;
    FError: String;
    FResultReady: LongInt;
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(AGrid: TCustomVisualGrid; ARefreshColumns: boolean);
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
  if FLabel.Font.{$IFDEF FPC}IsEqual{$ELSE}Equals{$ENDIF}(AValue) then Exit;
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
      {$IFDEF FPC}
    AnchorSideLeft.Control := AOwner.FTopPanelLeft;
    AnchorSideLeft.Side := asrBottom;
    AnchorSideTop.Control := AOwner.FTopPanelRight;
    AnchorSideTop.Side := asrCenter;
    AnchorSideRight.Control := AOwner.FTopPanelRight;
      {$ENDIF}
    Anchors := [akTop, akLeft, akRight];
    Visible := false;
    {$IFNDEF FPC}Align:=alClient; Layout:=tlCenter;{$ENDIF}
  end;
end;

{ TVisualColumn }

constructor TVisualColumn.Create(AGrid: TCustomVisualGrid);
begin
  FGrid := AGrid;
  FColumn := nil;
  FHasHeaderAlignment:=false;
  FHasHeaderFontStyles:=false;
  FHasDataAlignment:=false;
  FHasDataFontStyles:=false;
  FIndex := -1;
  FName := '';
  FBinding := '';
  FSortBinding := '';
  FDisplayBinding := '';
  FStretchColumn:=false;
  FAutoWidth:=false;
  FWidth :=-1;
  FRenderer := nil;
  FSanitizer:= nil;
  FVisible := true;
  FSortSequence := -1;
  FFilters :=[];
end;

function TVisualColumn.GetStretchedToFill: boolean;
begin
  {$IFDEF FPC}Result := FColumn.SizePriority > 0;{$ELSE}Result := FStretchColumn;{$ENDIF}
end;

function TVisualColumn.GetWidth: Integer;
begin
  {$IFDEF FPC}Result := FColumn.Width;{$ELSE}
  if FGrid.FDrawGrid.HandleAllocated then Result:=FGrid.FDrawGrid.ColWidths[FColumn.Index]
  else Result:=FColumn.Width;
  {$ENDIF}
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
  FStretchColumn:=AValue;
  if Assigned(FColumn) then
    AttachGridColumn(FColumn)
end;

procedure TVisualColumn.SetAutoWidth(AValue: boolean);
begin
  FAutoWidth:=AValue;
  if Assigned(FColumn) then
    AttachGridColumn(FColumn)
end;

procedure TVisualColumn.SetWidth(AValue: Integer);
begin
  FWidth := AValue;
  if Assigned(FColumn) then
    AttachGridColumn(FColumn)
end;

procedure TVisualColumn.SetName(const AValue: String);
begin
  FName := AValue;
  Binding := UTF8Decode(AValue);
end;

procedure TVisualColumn.SetBinding(const AValue: String);
begin
  FBinding := AValue;
  FSortBinding := AValue;
  FDisplayBinding := AValue;
end;

procedure TVisualColumn.SetHeaderAlignment(const AAlignment : TAlignment);
begin
  FHasHeaderAlignment := true;
  FHeaderAlignment := AAlignment;
end;

procedure TVisualColumn.SetHeaderFontStyles(const AStyles : TFontStyles);
begin
  FHasHeaderFontStyles := true;
  FHeaderFontStyles := AStyles;
end;

procedure TVisualColumn.SetDataAlignment(const AAlignment : TAlignment);
begin
  FHasDataAlignment := true;
  FDataAlignment := AAlignment;
end;

procedure TVisualColumn.SetDataFontStyles(const AStyles : TFontStyles);
begin
  FHasDataFontStyles := true;
  FDataFontStyles := AStyles;
end;

procedure TVisualColumn.AttachGridColumn(AGridColumn : TGridColumn);
begin
  if NOT Assigned(AGridColumn) then raise EArgumentNilException.Create('AGridColumn');

  // Set TDrawGrid column
  Self.FColumn := AGridColumn;

  // Width
  if FAutoWidth then
    FStretchColumn := false
  else if NOT FStretchColumn then begin
    {$IFDEF FPC}FColumn.Width := FWidth;{$ELSE}
    FColumn.Width := Max(1,IfThen(FWidth>0,FWidth,FGrid.FDrawGrid.DefaultColWidth));
    {$ENDIF}
  end;

  // StretchedToFill
  {$IFDEF FPC}FColumn.SizePriority := ifthen(FStretchColumn, 1);{$ELSE}FColumn.AutoSize:=FStretchColumn;{$ENDIF}

  // Caption - already painted in default drawing event
  {$IFDEF FPC}
  if Self.FColumn.Title.Caption <> '' then Self.FColumn.Title.Caption:='';
  {$ELSE}
  FColumn.Text:='';
  // Text metrics require a native window; retain AutoWidth until docking.
  if FAutoWidth and Assigned(FGrid.Parent) then FColumn.Width:=FGrid.Canvas.TextWidth(string(FName))+16;
  FGrid.FDrawGrid.FitColumns;
  {$ENDIF}
  FGrid.GridGeometryChanged(FGrid.FDrawGrid);
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

{ TVisualCellRenderers }

class procedure TVisualCellRenderers.DollarValue (Sender: TObject; ACol, ARow: Longint; Canvas: TCanvas; Rect: TRect; State: TGridDrawState; const CellData, RowData: Variant; var Handled: boolean);
var
  LReal : Real;
  {$IFDEF FPC}LTextStyle: TTextStyle;{$ENDIF}
begin
  if NOT VarIsNumeric(CellData) then
    exit;

  LReal := CellData;
  Canvas.Font.Color:= IIF (LReal < 0, clRed, clGreen);
  Canvas.Font.Style:=[fsBold];
  {$IFDEF FPC}
  LTextStyle := Canvas.TextStyle;
  LTextStyle.Alignment:=taRightJustify;
  Canvas.TextStyle:=LTextStyle;
  Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, Format('$%.2F', [LReal]), LTextStyle);
  {$ELSE}
  DrawGridText(Canvas,Rect,Format('$%.2F',[LReal]),taRightJustify);
  {$ENDIF}
  Handled := true;
end;

{ TFetchDataThread }

procedure TFetchDataThread.Execute;
begin
  if Terminated then Exit;
  try
    FGrid.FetchPage(FLastFetchDataResult.FetchResult);
  except
    on E: Exception do FError := E.Message;
  end;
end;

procedure TFetchDataThread.DoTerminate;
begin
  // FPC skips Execute if cancellation wins before scheduling, but still calls
  // DoTerminate. Always retire that request through the owned async dispatcher;
  // inherited OnTerminate would synchronously wait for the UI during shutdown.
  // Publish readiness first so an old queued wake cannot join a newer worker.
  InterlockedExchange(FResultReady, 1);
  FGrid.FFetchCompletion.Invoke(FGrid, FGrid.FetchDataThreadCompleted);
end;

constructor TFetchDataThread.Create(AGrid: TCustomVisualGrid;
  ARefreshColumns: boolean);
var LTransferred:Boolean;
begin
  // Delphi starts an unsuspended thread after the complete constructor returns.
  inherited Create({$IFDEF FPC}True{$ELSE}False{$ENDIF});
  FreeOnTerminate := false;
  AGrid.FActiveThread := Self;
  FGrid := AGrid;
  LTransferred:=False;
  try
  FGrid.ControlsEnable(false);
  FGrid.FFetchInProgress:=True;
  FGrid.BeginLoading;
  FGrid.BeforeFetchPage;
  FLastFetchDataResult.RefreshColumns:=ARefreshColumns;
  FIgnoreRecalcPageCount:=FGrid.FIgnoreRecalcPageCount;

  // fast copy of data (we need to draw old data for a while)
  New(FGrid.FCachedDataTable);
  Move(FGrid.FDataTable, FGrid.FCachedDataTable^, SizeOf(TDataTable));
  FillChar(FGrid.FDataTable, SizeOf(TDataTable), #0);
  LTransferred:=True;
  {$IFDEF VISUALGRID_TEST_WORKER_START}
  if Assigned(VisualGridBeforeWorkerStart) then VisualGridBeforeWorkerStart(Self);
  {$ENDIF}
  {$IFDEF FPC}Start;{$ENDIF}
  except
    // Failed native control preparation must not leave a pointer to the thread
    // whose constructor is unwinding. Its inherited destructor joins it.
    FGrid.FActiveThread:=nil;
    FGrid.FFetchInProgress:=False;
    if LTransferred then begin
      FGrid.FDataTable:=FGrid.FCachedDataTable^;
      Dispose(FGrid.FCachedDataTable);
      FGrid.FCachedDataTable:=nil;
    end;
    FGrid.EndLoading;
    FGrid.ControlsEnable(True);
    raise;
  end;
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
  FEdit.Text := '';
end;

{ TCustomVisualGrid }

{$IFDEF VISUALGRID_DEBUG}
procedure TCustomVisualGrid.ClickTest(Sender: TOBject);
begin
  TButton(Sender).Caption := Format('%dx%d', [FSearchEdit.Left,FSearchEdit.Top]);
end;
{$ENDIF}

constructor TCustomVisualGrid.Create(Owner: TComponent);
var LMode: Integer; LItem: TMenuItem;
begin
  inherited;

  FFetchControlsEnabled := True;
  FPageMode := vpmManual;
  FManualPageSize := 100;

  FMultiSearchEdits := TObjectList<TSearchEdit>.Create;
  FColumns := TObjectList<TVisualColumn>.Create;
  FFilters := TList<TColumnFilter>.Create;
  ResetSelection(FLastSelection);
  ResetSelection(FLastSelectionEvent);

  { component layout }

  ControlStyle := ControlStyle - [csAcceptsControls]{$IFDEF FPC} + [csOwnedChildrenNotSelectable]{$ENDIF};

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
      Align := alNone;
      Anchors := [akTop, akRight];
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

      {$IFDEF FPC}
        AnchorSideLeft.Control := FButtonPrevious;
        AnchorSideLeft.Side := asrBottom;
        AnchorSideTop.Control := FBottomRightPanel;
        AnchorSideRight.Control := FPageCountLabel;
        AnchorSideBottom.Control := FBottomRightPanel;
        AnchorSideBottom.Side := asrBottom;
      {$ENDIF}
        Anchors := [akLeft, akTop];
      {$IFDEF FPC}
        BorderSpacing.Top := 10;
        BorderSpacing.Right := 2;
        BorderSpacing.Left := 2;
        BorderSpacing.Bottom := 8;
      {$ENDIF}

        {$IFDEF FPC}OnEditingDone{$ELSE}OnExit{$ENDIF} := PageIndexEditingDone;
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

      {$IFDEF FPC}
        AnchorSideLeft.Control := FPageSizeLabel;
        AnchorSideLeft.Side := asrBottom;
        AnchorSideTop.Control := FBottomCenterPanel;
      {$ENDIF}
        Anchors := [akLeft, akTop];
      {$IFDEF FPC}
        BorderSpacing.Top := 10;
        BorderSpacing.Left := 2;
        BorderSpacing.Bottom := 8;
      {$ENDIF}

        {$IFDEF FPC}OnEditingDone{$ELSE}OnExit{$ENDIF}:=PageSizeEditingDone;
      end;
      FPageModeMenu := TPopupMenu.Create(Self);
      FPageModeMenu.Name := 'PageModeMenu';
      FPageModeMenu.AutoPopup := False;
      {$IFNDEF FPC}FPageModeMenu.AutoHotkeys := maManual;{$ENDIF}
      for LMode := 0 to 2 do begin
        LItem := TMenuItem.Create(FPageModeMenu);
        case LMode of
          0: LItem.Caption := 'Single';
          1: LItem.Caption := 'Auto';
          2: LItem.Caption := 'Manual';
        end;
        LItem.Name := 'PageMode'+LItem.Caption;
        LItem.Tag := LMode;
        LItem.RadioItem := True;
        LItem.GroupIndex := 1;
        LItem.OnClick := PageModeChanged;
        FPageModeMenu.Items.Add(LItem);
      end;
      FPageModeButton := TBitBtn.Create(Self);
      FPageModeButton.Name := 'PageModeButton';
      FPageModeButton.Parent := FBottomRightPanel;
      FPageModeButton.Caption := '';
      FPageModeButton.Spacing := 0;
      FPageModeButton.Margin := -1;
      FPageModeButton.ShowHint := True;
      FPageModeButton.OnClick := PageModeButtonClick;
      FPageModeButton.PopupMenu := FPageModeMenu;
      {$IFDEF FPC}FPageModeButton.AutoSize := False;{$ENDIF}
      FPageModeButton.TabOrder := 0;
      FButtonFirst.TabOrder := 1;
      FButtonPrevious.TabOrder := 2;
      FPageIndexEdit.TabOrder := 3;
      FButtonNext.TabOrder := 4;
      FButtonLast.TabOrder := 5;
      {$IFDEF FPC}
      // The shared footer layout positions these controls on both widgetsets.
      FPageSizeEdit.AnchorSideLeft.Control := nil;
      FPageSizeEdit.AnchorSideTop.Control := nil;
      FPageIndexEdit.AnchorSideLeft.Control := nil;
      FPageIndexEdit.AnchorSideRight.Control := nil;
      FPageIndexEdit.AnchorSideTop.Control := nil;
      FPageIndexEdit.AnchorSideBottom.Control := nil;
      {$ENDIF}
    end;
    FBottomRightPanel.BringToFront;
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
      {$IFDEF FPC}
      AnchorSideTop.Control := FTopPanel;
      AnchorSideLeft.Control := FTopPanel;
      AnchorSideBottom.Control := FTopPanel;
      AnchorSideBottom.Side := asrBottom;
      {$ENDIF}
      Anchors := [akLeft, akTop, akBottom];

      {$IFDEF FPC}
      BorderSpacing.Top := 6;
      BorderSpacing.Left := 4;
      BorderSpacing.Bottom := 6;
      {$ENDIF}
      Width := 120;
      {$IFNDEF FPC}SetBounds(4,6,120,24);{$ENDIF}
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
      // remove windres usage due to "directory with spaces" bug during compilation.
      //{$IFDEF WINDOWS}
      //FSearchButton.LoadGlyphFromResourceName(HINSTANCE, 'VISUALGRID_SEARCH');
      //{$ELSE}
      {$IFDEF FPC}
      FSearchButton.LoadGlyphFromLazarusResource('VISUALGRID_SEARCH');
      {$ELSE}
      LoadSearchGlyph;
      {$ENDIF}
      //{$ENDIF}
      with FSearchButton do
      begin
      {$IFDEF FPC}
        AnchorSideTop.Control := FTopPanelRight;
        AnchorSideRight.Control := FTopPanelRight;
        AnchorSideRight.Side := asrBottom;
        AnchorSideBottom.Control := FTopPanelRight;
        AnchorSideBottom.Side := asrBottom;
      {$ENDIF}
        Anchors := [akTop, akRight, akBottom];
      {$IFDEF FPC}
        BorderSpacing.Top := 6;
        BorderSpacing.Right := 4;
        BorderSpacing.Bottom := 6;
      {$ENDIF}
        Width := 23;
        {$IFNDEF FPC}SetBounds(127,6,23,24);{$ENDIF}
        PopupMenu:=FSearchKindPopupMenu;
        OnClick:=SearchButtonClick;
      end;

      FSearchEdit := TEdit.Create(Self);
      FSearchEdit.Parent := FTopPanelRight;
      with FSearchEdit do
      begin
      {$IFDEF FPC}
        AnchorSideTop.Control := FTopPanelRight;
        AnchorSideRight.Control := FSearchButton;
        AnchorSideBottom.Control := FTopPanelRight;
        AnchorSideBottom.Side := asrBottom;
      {$ENDIF}
        Anchors := [akTop, akRight, akBottom];
      {$IFDEF FPC}
        BorderSpacing.Top := 6;
        BorderSpacing.Right := 2;
        BorderSpacing.Bottom := 6;
      {$ENDIF}
        Width := 121;
        {$IFNDEF FPC}SetBounds(2,6,121,24);{$ENDIF}
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
      OnMouseDown := GridMouseDown;
      OnMouseUp := GridMouseUp;
      OnSelection := GridSelection;
      OnHeaderClick := GridHeaderClick;
      OnResize := GridGeometryChanged;
      Options := (Options - [goRangeSelect]);
      FixedCols := 0;
      {$IFDEF FPC}ColCount:=0;{$ELSE}ColCount:=1;{$ENDIF}
      {$IFDEF FPC}RowCount:=0;{$ELSE}RowCount:=2;{$ENDIF}
    end;
    FDefaultDrawGridOptions := FDrawGrid.Options;
  end;

  FLoadDataPanel := TPanel.Create(Self);
  FLoadDataPanel.Name := 'LoadingOverlay';
  FLoadDataPanel.Caption := '';
  FLoadDataPanel.Parent := FClientPanel;
  FLoadDataPanel.BevelOuter := bvNone;
  FLoadDataPanel.BorderStyle := bsSingle;
  FLoadDataPanel.Color := clWindow;
  FLoadDataPanel.DoubleBuffered := True;
  FLoadDataPanel.Visible := False;
  FLoadDataPanel.TabStop := False;

  FLoadingCircle := TLoadingCircle.Create(Self);
  FLoadingCircle.Name := 'LoadingCircle';
  FLoadingCircle.Parent := FLoadDataPanel;
  FLoadingCircle.Color := clWindow;

  FLoadDataLabel := TLabel.Create(Self);
  FLoadDataLabel.Name := 'LoadingStatus';
  FLoadDataLabel.Parent := FLoadDataPanel;
  FLoadDataLabel.AutoSize := False;
  FLoadDataLabel.Font.Style := [fsBold];
  FLoadDataLabel.Caption := sDataLoading;
  FLoadDataLabel.WordWrap := True;
  FLoadDataLabel.Layout := tlCenter;
  FLoadingSubtitle := TLabel.Create(Self);
  FLoadingSubtitle.Name := 'LoadingSubtitle';
  FLoadingSubtitle.Parent := FLoadDataPanel;
  FLoadingSubtitle.AutoSize := False;
  FLoadingSubtitle.Font.Style := [];
  FLoadingSubtitle.WordWrap := True;
  FLoadingSubtitle.Visible := False;
  FLoadingImage := TImage.Create(Self);
  FLoadingImage.Name := 'LoadingImage';
  FLoadingImage.Parent := FLoadDataPanel;
  FLoadingImage.Center := True;
  FLoadingImage.Proportional := True;
  FLoadingImage.Stretch := True;
  FLoadingImage.Visible := False;
  FLoadDataPanel.OnResize := LayoutLoadingPanel;
  FClientPanel.OnResize := LayoutLoadingPanel;
  LayoutLoadingPanel(FClientPanel);

  FDelayedBoundsChangeTimer := TTimer.Create(Self);
  with FDelayedBoundsChangeTimer do
  begin
    Enabled:=false;
    Interval:=20;
    OnTimer:=DelayedBoundsChange;
  end;

  FFetchCompletion := TThreadNotify.Create(TThread.CurrentThread);
  // A one-shot presentation delay preserves the nonflashing loading card.
  // Fetch completion itself is queued by the worker, never polled by a timer.
  FLoadingDelayTimer := TTimer.Create(Self);
  with FLoadingDelayTimer do
  begin
    Enabled:=false;
    Interval:=200;
    OnTimer:=ShowDelayedLoading;
  end;

  FEditingDoneTimer := TTimer.Create(Self);
  with FEditingDoneTimer do
  begin
    Enabled:=false;
    Interval:=2000;
  end;

  { default values for properties }
  FRefreshThrottle := TThrottledEvent.Create(Self);
  FRefreshThrottle.Add(RefreshThrottleDelivery);
  FRefreshThrottle.OnWaitingChanged.Add(RefreshThrottleWaitingChanged);
  FRefreshThrottle.OnBurstFinished.Add(RefreshThrottleFinished);
  FCellPadding := TRect.Create(CT_DEFAULT_CELL_PADDING_LEFT, CT_DEFAULT_CELL_PADDING_TOP, CT_DEFAULT_CELL_PADDING_RIGHT, CT_DEFAULT_CELL_PADDING_BOTTOM);
  DefaultStretchedColumn := -1;
  FSearchMode := smSingle;
  PageSize := 100;
  PageIndex := -1;
  FCanPage := true;
  FCanSearch := true;
  FTotalDataCount := -1;
  UpdatePagingControls;

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
  // Join the producer before disposing its queued UI dispatcher or borrowed data.
  FShuttingDown := true;
  FreeAndNil(FRefreshThrottle);
  if Assigned(FLoadingDelayTimer) then FLoadingDelayTimer.Enabled := false;
  if Assigned(FDelayedBoundsChangeTimer) then FDelayedBoundsChangeTimer.Enabled := false;
  if Assigned(FActiveThread) then begin
    FActiveThread.Terminate;
    FJoiningFetch := True;
    try FActiveThread.WaitFor;
    finally FJoiningFetch := False; end;
    FreeAndNil(FActiveThread);
  end;
  FreeAndNil(FFetchCompletion); // Cancels this receiver's queued adapters.
  FFetchInProgress := False;
  if Assigned(FCachedDataTable) then begin
    Dispose(FCachedDataTable);
    FCachedDataTable := nil;
  end;
  FDataSource := nil;
  FFilters.Free;
  FColumns.Free;
  FMultiSearchEdits.Free;
  FCaption.Free;
  inherited;
end;

procedure TCustomVisualGrid.DoDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState; const RowData: Variant);
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
  LText: String;
  LColumn: TVisualColumn;
  LHalfOfHeight: longint;
  LP1,LP2,LP3: TPoint;
begin
  if ARow = 0 then
  begin
    LColumn := GetColumn(ACol);
    LText := LColumn.Name;
    Rect := CalculateCellContentRect(Rect);

    {$IFDEF FPC}FDrawGrid.Canvas.TextRect(Rect, Rect.Left, Rect.Top, LText);{$ELSE}
    if LColumn.HasHeaderAlignment then DrawGridText(Canvas,Rect,string(LText),LColumn.HeaderAlignment)
    else DrawGridText(Canvas,Rect,string(LText),taLeftJustify);{$ENDIF}

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
      sdAscending: FDrawGrid.Canvas.Polygon([LP1, LP2, LP3]);
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
  else begin
    {$IFDEF FPC}FDrawGrid.Canvas.TextRect(Rect, Rect.Left, Rect.Top, RowData);{$ELSE}
    LColumn:=GetColumn(ACol);
    if LColumn.HasDataAlignment then DrawGridText(Canvas,Rect,VarToStr(RowData),LColumn.DataAlignment)
    else DrawGridText(Canvas,Rect,VarToStr(RowData),taLeftJustify);{$ENDIF}
  end;
end;

procedure TCustomVisualGrid.Loaded;
begin
  inherited;
  ReloadColumns;
end;

{$IFDEF FPC}procedure TCustomVisualGrid.BoundsChanged;{$ELSE}procedure TCustomVisualGrid.Resize;{$ENDIF}
begin
  {$IFDEF FPC}inherited BoundsChanged;{$ELSE}inherited Resize;{$ENDIF}
  ScheduleBoundsChange;
end;

procedure TCustomVisualGrid.GridGeometryChanged(Sender: TObject);
begin
  if not FAutoPageSize or not FCanPage or not Assigned(Parent) then Exit;
  // Horizontal scrollbars alter the native viewport without changing this
  // composite's bounds. Refit through the same deferred layout admission.
  ScheduleBoundsChange;
end;

procedure TCustomVisualGrid.ScheduleBoundsChange;
begin
  if FShuttingDown then Exit;
  if Assigned(FRefreshThrottle) and (FInternalLayoutDepth = 0) then begin
    FBoundsClientPending := True;
    // A real resize is input now, even when page-capacity layout is deferred.
    // The deferred geometry pass must not notify a second time for this input.
    if FRefreshThrottle.Waiting then begin
      FBoundsAlreadyNotified := True;
      FRefreshReady := False;
      FRefreshThrottle.Notify;
    end;
  end;
  // fix maximize form problem for AutoPageSize (new size of grid is not yet fully propagated)
  if Assigned(FDelayedBoundsChangeTimer) then FDelayedBoundsChangeTimer.Enabled:=true;
end;

procedure TCustomVisualGrid.SortDirectionGlyphRefresh;

  procedure VisualColumnsSortDirection(AStopOnFirstSortable, AAllNone: boolean);
  var
    i: integer;
    LStop: boolean;
    LColumn: TVisualColumn;
  begin
    LStop:=False;
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
    finally
      LColumn.FIgnoreRefresh:=false;
    end;
  end;

begin
  case FSortMode of
    smNone:
      begin
        FDrawGrid.Options := FDrawGrid.Options - [{$IFDEF FPC}goHeaderHotTracking,goHeaderPushedLook{$ELSE}goFixedHotTrack{$ENDIF}];
        VisualColumnsSortDirection(false, true)
      end;
    smSingleColumn, smMultiColumn:
      begin
        FDrawGrid.Options := FDrawGrid.Options + [{$IFDEF FPC}goHeaderHotTracking,goHeaderPushedLook{$ELSE}goFixedHotTrack{$ENDIF}];
        VisualColumnsSortDirection(FSortMode=smSingleColumn, false);
      end;
  end;
  RefreshPageIndexData(false);
end;

{$IFNDEF FPC}
function TCustomVisualGrid.Scale96ToFont(AValue:Integer):Integer;
begin
  Result:=ScaleValue(AValue);
end;
{$ENDIF}

procedure TCustomVisualGrid.SetParent(AParent:TWinControl);
var i:Integer;
begin
  inherited;
  if Assigned(AParent) and Assigned(FFetchCompletion) and not FShuttingDown then begin
    {$IFNDEF FPC}
    // Apply column metrics deferred while the composite control had no parent.
    for i:=0 to FColumns.Count-1 do FColumns[i].AttachGridColumn(FColumns[i].FColumn);
    {$ENDIF}
    RefreshPageIndexAndGridInterface;
    ResetModeScrollPosition(not FRefreshPending and not FFetchInProgress);
    for i:=0 to FMultiSearchEdits.Count-1 do ResizeSearchEdit(i);
    LayoutLoadingPanel(Self);
    RefreshLoadingPresentation;
  end;
end;

function TCustomVisualGrid.GetIsUpdating : Boolean;
begin
  Result := FFetchInProgress;
end;

procedure TCustomVisualGrid.LayoutLoadingPanel(Sender : TObject);
var
  LMargin, LWidth, LHeight, LClientWidth, LClientHeight,
    LCircleSize, LTextLeft : Integer;
begin
  if not Assigned(FLoadDataPanel) then Exit;
  {$IFNDEF FPC}
  // VCL ClientRect creates the native window. A composite control's children
  // must wait until its caller assigns a parent before requesting those HWNDs.
  if not Assigned(Parent) then Exit;
  {$ENDIF}
  LMargin := Scale96ToFont(8);
  if FManualLoading then LWidth := Scale96ToFont(320) else LWidth := Scale96ToFont(240);
  if FLoadingSubtitle.Visible then LHeight := Scale96ToFont(80) else LHeight := Scale96ToFont(64);
  LWidth := Min(LWidth,Max(0,FClientPanel.ClientWidth-2*LMargin));
  LHeight := Min(LHeight,Max(0,FClientPanel.ClientHeight-2*LMargin));
  FLoadDataPanel.SetBounds((FClientPanel.ClientWidth-LWidth) div 2,
    (FClientPanel.ClientHeight-LHeight) div 2,LWidth,LHeight);
  LClientWidth := FLoadDataPanel.ClientWidth;
  LClientHeight := FLoadDataPanel.ClientHeight;
  LCircleSize := Scale96ToFont(36);
  FLoadingCircle.SetBounds(Scale96ToFont(10),(LClientHeight-LCircleSize) div 2,
    LCircleSize,LCircleSize);
  FLoadingImage.SetBounds(Scale96ToFont(10),(LClientHeight-LCircleSize) div 2,
    LCircleSize,LCircleSize);
  if FLoadingCircle.Visible or FLoadingImage.Visible then begin
    LTextLeft := Scale96ToFont(56);
    if FLoadingSubtitle.Visible then begin
      FLoadDataLabel.SetBounds(LTextLeft,Scale96ToFont(8),
        Max(0,LClientWidth-LTextLeft-Scale96ToFont(10)),Scale96ToFont(22));
      FLoadingSubtitle.SetBounds(LTextLeft,Scale96ToFont(32),
        Max(0,LClientWidth-LTextLeft-Scale96ToFont(10)),Max(0,LClientHeight-Scale96ToFont(40)));
    end else
      FLoadDataLabel.SetBounds(LTextLeft,(LClientHeight-Scale96ToFont(22)) div 2,
        Max(0,LClientWidth-LTextLeft-Scale96ToFont(10)),Scale96ToFont(22));
  end else
    FLoadDataLabel.SetBounds(Scale96ToFont(12),Scale96ToFont(10),
      Max(0,LClientWidth-Scale96ToFont(24)),Max(0,LClientHeight-Scale96ToFont(20)));
end;

procedure TCustomVisualGrid.BringLoadingPanelToFront;
begin
  {$IFDEF FPC}
  FLoadDataPanel.BringToFront;
  {$ELSE}
  // VCL BringToFront activates a child window's top-level parent. Background
  // refreshes must only raise the card within this grid, preserving the user's
  // active modeless window and keyboard focus.
  if FLoadDataPanel.HandleAllocated then
    SetWindowPos(FLoadDataPanel.Handle, HWND_TOP, 0, 0, 0, 0,
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE)
  else
    FLoadDataPanel.BringToFront;
  {$ENDIF}
end;

procedure TCustomVisualGrid.BeginLoading;
begin
  // Queued throttle input is already outstanding work. Keep one presentation
  // interval through admission and completion instead of hiding it while waiting.
  if not FAutomaticLoading then FLoadingStartedAt := GetTickCount64;
  FAutomaticLoading := True;
  FLoadingError := '';
  FLoadingDelayTimer.Enabled := False;
  FLoadingDelayTimer.Interval := 200;
  FLoadingDelayTimer.Enabled := True;
  if Length(FDataTable.Rows) > 0 then
    FAutomaticLoadingTitle := sDataUpdating
  else
    FAutomaticLoadingTitle := sDataLoading;
  RefreshLoadingPresentation;
end;

procedure TCustomVisualGrid.UpdateLoading;
begin
  RefreshLoadingPresentation;
end;

procedure TCustomVisualGrid.ShowDelayedLoading(Sender: TObject);
begin
  FLoadingDelayTimer.Enabled := False;
  if not FShuttingDown and FAutomaticLoading then UpdateLoading;
end;

procedure TCustomVisualGrid.EndLoading;
begin
  FLoadingDelayTimer.Enabled := False;
  // Accepted rows end their automatic card. Only newer client input retains
  // it; a deferred internal capacity refit already has usable published data.
  FAutomaticLoading := FRefreshPending and FRefreshClientPending and
    not FRefreshSuspended and not FRefreshCancelled and not FShuttingDown;
  if FAutomaticLoading and (GetTickCount64-FLoadingStartedAt < 200) then
    FLoadingDelayTimer.Enabled := True;
  FLoadingError := '';
  RefreshLoadingPresentation;
end;

procedure TCustomVisualGrid.ShowLoadingError(const AMessage : String);
begin
  FLoadingDelayTimer.Enabled := False;
  FAutomaticLoading := False;
  FLoadingError := AMessage;
  RefreshLoadingPresentation;
end;

procedure TCustomVisualGrid.ShowLoading(const ATitle: String;
  const ASubtitle: String; AImage: TGraphic; ADisableInput: Boolean);
begin
  FLoadingImage.Picture.Assign(AImage);
  FManualLoadingTitle := ATitle;
  FManualLoadingSubtitle := ASubtitle;
  FManualLoadingDisablesInput := ADisableInput;
  FManualLoading := True;
  ApplyControlsState;
  RefreshLoadingPresentation;
end;

procedure TCustomVisualGrid.HideLoading;
begin
  if not FManualLoading then Exit;
  FManualLoading := False;
  FManualLoadingDisablesInput := False;
  FLoadingImage.Picture.Assign(nil);
  ApplyControlsState;
  RefreshLoadingPresentation;
end;

procedure TCustomVisualGrid.RefreshLoadingPresentation;
var
  LShow, LImage, LCircle: Boolean;
begin
  if not Assigned(FLoadDataPanel) then Exit;
  // A manual request may precede parenting. Keep its state without causing
  // VCL to create an HWND whose ancestor has no parent window yet.
  if not Assigned(Parent) then begin
    FLoadingCircle.Active := False;
    Exit;
  end;
  LImage := FManualLoading and Assigned(FLoadingImage.Picture.Graphic) and
    (not FLoadingImage.Picture.Graphic.Empty);
  LCircle := FManualLoading or (FAutomaticLoading and (FLoadingError = ''));
  LShow := FManualLoading or (FLoadingError <> '') or
    (FAutomaticLoading and (GetTickCount64-FLoadingStartedAt >= 200) and
      (FClientPanel.ClientWidth >= Scale96ToFont(100)) and
      (FClientPanel.ClientHeight >= Scale96ToFont(40)));
  FLoadingImage.Visible := LImage;
  FLoadingCircle.Visible := LCircle and not LImage;
  FLoadingCircle.Active := LShow and FLoadingCircle.Visible;
  FLoadingSubtitle.Visible := FManualLoading and (FManualLoadingSubtitle <> '');
  FLoadingSubtitle.Caption := FManualLoadingSubtitle;
  FLoadDataPanel.ShowHint := (not FManualLoading) and (FLoadingError <> '');
  FLoadDataPanel.Hint := FLoadingError;
  if FManualLoading then begin
    FLoadDataLabel.Caption := FManualLoadingTitle;
    FLoadDataLabel.Font.Color := clWindowText;
  end else if FLoadingError <> '' then begin
    FLoadDataLabel.Caption := FLoadingError;
    FLoadDataLabel.Font.Color := clRed;
  end else begin
    FLoadDataLabel.Caption := FAutomaticLoadingTitle;
    FLoadDataLabel.Font.Color := clWindowText;
  end;
  LayoutLoadingPanel(Self);
  FLoadDataPanel.Visible := LShow;
  if LShow then BringLoadingPanelToFront;
end;

procedure TCustomVisualGrid.DelayedBoundsChange(Sender: TObject);
var LClientInput, LAlreadyNotified: Boolean;
begin
  {$IFNDEF FPC}
  if not Assigned(Parent) then begin FDelayedBoundsChangeTimer.Enabled:=False; Exit; end;
  {$ENDIF}
  // check data loading. try in next cycle
  if FFetchInProgress and (FRefreshThrottle.Mode = temNone) and not FRefreshSuspended then
    Exit;
  FDelayedBoundsChangeTimer.Enabled:=false;
  LClientInput := FBoundsClientPending;
  LAlreadyNotified := FBoundsAlreadyNotified;
  FBoundsClientPending := False;
  FBoundsAlreadyNotified := False;
  Inc(FInternalLayoutDepth);
  try
    UpdatePagingControls;
  finally
    Dec(FInternalLayoutDepth);
  end;
  if AutoPageSize and FCanPage then
  begin
    // for vgoAutoHidePaging and AutoPageSize more space may be available
    // and paging panel may be hidden
    if (PageSize = ClientRowCount) and (vgoAutoHidePaging in FOptions) and FAutoPageSize then
      begin
        Inc(FInternalLayoutDepth);
        try RefreshPageIndexAndGridInterface;
        finally Dec(FInternalLayoutDepth); end;
      end
    else
      ChangePageSize(ClientRowCount, LClientInput, LClientInput and not LAlreadyNotified);
  end;
  ExecutePendingRefresh;
end;

procedure TCustomVisualGrid.FetchDataThreadCompleted(Sender: TObject);
var
  worker: TFetchDataThread;
  disposed: TDisposables;
begin
  if FShuttingDown or FJoiningFetch or not Assigned(FActiveThread) then Exit;
  worker := TFetchDataThread(FActiveThread);
  if InterlockedCompareExchange(worker.FResultReady, 0, 0) = 0 then Exit;
  // Provider work has ended or was skipped before scheduling. There is no
  // OnTerminate/Synchronize callback for WaitFor to wait on. Guard other queued
  // wakes while WaitFor services the main-thread queue during a source change.
  FJoiningFetch := True;
  try worker.WaitFor;
  finally FJoiningFetch := False; end;
  disposed.AddObject(worker);
  FActiveThread := nil;
  if FActiveRefreshCancelled then begin
    // A provider may finish normally after cancellation, or raise Abort while
    // releasing its own locks. Neither outcome can replace the retained rows.
    FDataTable := FCachedDataTable^;
    Dispose(FCachedDataTable);
    FCachedDataTable := nil;
    FFetchInProgress := False;
    FActiveRefreshSource := nil;
    FActiveRefreshCancelled := False;
    EndLoading;
    ControlsEnable(True);
    ExecutePendingRefresh;
    Exit;
  end;
  FLastFetchDataResult := worker.FLastFetchDataResult;
  FLastFetchDataResult.FromThread := true;
  FIgnoreRecalcPageCount := worker.FIgnoreRecalcPageCount;
  if worker.FError <> '' then begin
    FDataTable := FCachedDataTable^;
    FLastFetchDataResult.FromThread := false;
    Dispose(FCachedDataTable);
    FCachedDataTable := nil;
    FFetchInProgress := false;
    FActiveRefreshSource := nil;
    ShowLoadingError(worker.FError);
    ControlsEnable(true);
  end else
    AfterFetchPage;
  if FRefreshPending then ExecutePendingRefresh;
end;

{$IFNDEF FPC}
procedure TCustomVisualGrid.LoadSearchGlyph;
var LStream:TResourceStream; LPng:TPngImage;
begin
  LStream:=TResourceStream.Create(HInstance,'VISUALGRID_SEARCH',RT_RCDATA);
  try
    LPng:=TPngImage.Create;
    try LPng.LoadFromStream(LStream); FSearchButton.Glyph.Assign(LPng);
    finally LPng.Free; end;
  finally LStream.Free; end;
end;
{$ENDIF}

procedure TCustomVisualGrid.SearchButtonClick(Sender: TObject);
var
  LFormat: TFormatSettings;
  LNewStrFilter: utf8string;
  LException: boolean;
  LNum : SizeInt;
  LDataFilter : TDataFilter;
  e: TSearchEdit;

  procedure AddExpression(const AExpression: utf8string; AColumn : TVisualColumn);
  var
    LColumn: TVisualColumn;
    LAccepted: TDataFilters;
    LColumnFilter: TColumnFilter;
    LCandidates: TDataFilters;
    LExpressionRecord: TExpressionRecord;

    procedure AddFilter( const ABinding : String; const AFilters: TDataFilters);
    var
      LFilter: TDataFilter;
    begin
      LAccepted := AFilters * LCandidates;
      if LAccepted = [] then
        Exit;
      for LFilter in LAccepted do
      begin
        LColumnFilter:=Default(TColumnFilter);
        LColumnFilter.ColumnName := ABinding;
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
    LCandidates:=[];
    try
      LExpressionRecord := TSearchExpressionService.Parse(AExpression);
    except
      LException:=true;
      raise;
    end;

    with LExpressionRecord do
    begin
      case Kind of
        ekUnknown: Exit;
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

    if Assigned(AColumn) then
      AddFilter(AColumn.SortBinding, AColumn.Filters)    // add filter for column only
    else
      for LColumn in FColumns do
        AddFilter(LColumn.SortBinding, LColumn.Filters); // add filter for all columns

    LNewStrFilter := LNewStrFilter + QuotedStr(AExpression) + ',';
  end;

begin
  if not Assigned(FDataSource) then
    exit;

  LException:=False;
  FFilters.Clear;
  LFormat := {$IFDEF FPC}DefaultFormatSettings{$ELSE}TFormatSettings.Create{$ENDIF};
  LFormat.DecimalSeparator:='.';
  try
    AddExpression(FSearchEdit.Text, nil);
    // multi column search
    for e in FMultiSearchEdits do begin
      // Count how many filters
      LNum := 0;
      for LDataFilter in e.Column.Filters do inc(LNum);
      if LNum > 0 then
        AddExpression(e.FEdit.Text, e.Column);
    end;
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
begin
  FFetchControlsEnabled := AEnable;
  ApplyControlsState;
end;

procedure TCustomVisualGrid.ApplyControlsState;
var
  e: TSearchEdit;
  I: Integer;
  LReadOnly, LManualInputAllowed, LInputAllowed: boolean;
begin
  LManualInputAllowed := not (FManualLoading and FManualLoadingDisablesInput);
  LInputAllowed := FFetchControlsEnabled and LManualInputAllowed;
  LReadOnly:=not LInputAllowed;
  // The card's parent remains enabled. Gate complete input regions for a
  // manual hold without changing ordinary automatic-fetch interaction.
  FTopPanel.Enabled := LManualInputAllowed;
  FTopPanelMultiSearch.Enabled := LManualInputAllowed;
  FBottomPanel.Enabled := LManualInputAllowed;
  FMultiSearchCheckComboBox.Enabled:=LInputAllowed;
  FSearchEdit.ReadOnly:=LReadOnly;
  FPageSizeEdit.ReadOnly:=LReadOnly;
  FPageIndexEdit.ReadOnly:=LReadOnly;
  FDrawGrid.Enabled:=LInputAllowed;
  FBottomRightPanel.Enabled:=LInputAllowed;
  FPageModeButton.Enabled:=LInputAllowed;
  for I := 0 to FPageModeMenu.Items.Count-1 do FPageModeMenu.Items[I].Enabled := LInputAllowed;
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

function TCustomVisualGrid.GetColumn(Index: Integer): TVisualColumn;
begin
  Result := FColumns[Index];
end;

function TCustomVisualGrid.GetActiveDataTable: PDataTable;
begin
  if FFetchInProgress and Assigned(FCachedDataTable) then
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
  Result.Page := PageIndex;
end;

function TCustomVisualGrid.GetSelectedRows : TArray<Variant>;
var
  sel : TVisualGridSelection;
  selectedRows : TList<Variant>;
  GC: TDisposables;
  row, rangeIndex: Integer;
  selected: TArray<Boolean>;
begin
  if FCurrentSelectionType = stNone then Exit(nil);
  sel := GetSelection;
  selectedRows := GC.AddObject( TList<Variant>.Create ) as TList<Variant>;
  // Multi-selection can contain disjoint or overlapping grid rectangles.
  // Return each selected data row once, in its displayed order.
  SetLength(selected, RowCount);
  for rangeIndex := 0 to High(sel.Selections) do
    for row := Max(0,sel.Selections[rangeIndex].Top) to
      Min(High(selected),sel.Selections[rangeIndex].Bottom) do
      selected[row] := True;
  for row := 0 to High(selected) do
    if selected[row] then selectedRows.Add(Self.Rows[row]);
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

  FIgnoreRecalcPageCount := true;
  PageSize:=LPageSize;
  FIgnoreRecalcPageCount := false;
end;

function TCustomVisualGrid.CanChangePageMode: Boolean;
var LControl: TControl;
begin
  Result := False;
  if not FCanPage or not Assigned(Parent) then Exit;
  LControl := FPageModeButton;
  while Assigned(LControl) do begin
    if not LControl.Enabled then Exit;
    LControl := LControl.Parent;
  end;
  Result := True;
end;

procedure TCustomVisualGrid.PageModeButtonClick(Sender: TObject);
var LPoint: TPoint;
begin
  if not CanChangePageMode then Exit;
  LPoint := FPageModeButton.ClientToScreen(Point(0,FPageModeButton.Height));
  FPageModeMenu.PopupComponent := FPageModeButton;
  FPageModeMenu.PopUp(LPoint.X,LPoint.Y);
end;

procedure TCustomVisualGrid.PageModeChanged(Sender: TObject);
begin
  // A menu can outlive the input state in which it opened (fetch/manual hold).
  // Keep its command subject to the same enabled parent chain as the button.
  if not CanChangePageMode or not (Sender is TMenuItem) then Exit;
  if TMenuItem(Sender).Tag in [Ord(Low(TVisualGridPageMode))..Ord(High(TVisualGridPageMode))] then
    PageMode := TVisualGridPageMode(TMenuItem(Sender).Tag);
end;

procedure TCustomVisualGrid.UpdatePageModeGlyph;
var LBitmap: {$IFDEF FPC}Graphics.TBitmap{$ELSE}Vcl.Graphics.TBitmap{$ENDIF}; LSize: Integer;

  function P(AValue: Integer): Integer;
  begin
    Result := (AValue*LSize+8) div 16;
  end;

begin
  LSize := Max(12,Min(FPageModeButton.Width,FPageModeButton.Height)-Scale96ToFont(8));
  if (FPageModeButton.Glyph.Width = LSize) and (FPageModeButton.Glyph.Height = LSize) then Exit;
  LBitmap := {$IFDEF FPC}Graphics.TBitmap{$ELSE}Vcl.Graphics.TBitmap{$ENDIF}.Create;
  try
    LBitmap.PixelFormat := pf24bit;
    LBitmap.Width := LSize;
    LBitmap.Height := LSize;
    LBitmap.Canvas.Brush.Color := clFuchsia;
    LBitmap.Canvas.FillRect(Types.Rect(0,0,LSize,LSize));
    LBitmap.Canvas.Brush.Style := bsClear;
    LBitmap.Canvas.Pen.Color := clBtnText;
    LBitmap.Canvas.Rectangle(P(1),P(1),P(12),P(12));
    LBitmap.Canvas.MoveTo(P(1),P(5));
    LBitmap.Canvas.LineTo(P(12),P(5));
    LBitmap.Canvas.MoveTo(P(1),P(8));
    LBitmap.Canvas.LineTo(P(12),P(8));
    LBitmap.Canvas.MoveTo(P(5),P(1));
    LBitmap.Canvas.LineTo(P(5),P(12));
    LBitmap.Canvas.Brush.Style := bsSolid;
    LBitmap.Canvas.Brush.Color := clBtnText;
    LBitmap.Canvas.Polygon([Point(P(9),P(12)),Point(P(15),P(12)),Point(P(12),P(15))]);
    LBitmap.TransparentColor := clFuchsia;
    LBitmap.Transparent := True;
    FPageModeButton.Glyph.Assign(LBitmap);
    FPageModeButton.NumGlyphs := 1;
  finally
    LBitmap.Free;
  end;
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
  {$IFDEF FPC}LState: TCheckComboItemState;{$ENDIF}
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
  {$IFDEF FPC}
  LState := TCheckComboItemState(FMultiSearchCheckComboBox.Items.Objects[AIndex]);
  if Assigned(LState.Data) then TSearchEdit(LState.Data).EditVisible:=LState.State=cbChecked
  else FSearchEdit.Visible:=LState.State=cbChecked;
  {$ELSE}
  if Assigned(FMultiSearchCheckComboBox.Objects[AIndex]) then
    TSearchEdit(FMultiSearchCheckComboBox.Objects[AIndex]).EditVisible:=FMultiSearchCheckComboBox.Checked[AIndex]
  else FSearchEdit.Visible:=FMultiSearchCheckComboBox.Checked[AIndex];
  {$ENDIF}

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
var LBinding : String; LDataColIndex : Integer; LRow : TDataRowData;
begin
  LRow := TDataRowData(FDataTable.Rows[ARow]);
  LBinding := FColumns[ACol].Binding;
  LRow[LBinding] := AValue;
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
  // VCL Clear forces a HWND; Text also retains the intended empty value while
  // a composite grid is configured before its native parent is assigned.
  FSearchEdit.Text := '';

  FSearchMode := AValue;

  // reload data and don't use any filter for new mode
  FStrFilter := '';
  FFilters.Clear;
  RefreshPageIndexData(false);
end;

procedure TCustomVisualGrid.SetOptions(AValue: TVisualGridOptions);
var
  LSortDirectionAllowNone: boolean;
  LAutoHidePaging: boolean;
  LAutoHideSearchPanel: boolean;
begin
  if FOptions=AValue then Exit;
  LSortDirectionAllowNone := vgoSortDirectionAllowNone in FOptions;
  LAutoHidePaging := vgoAutoHidePaging in FOptions;
  LAutoHideSearchPanel := vgoAutoHideSearchPanel in FOptions;

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
  // try to hide paging panel or search panel
  if (LAutoHidePaging <> (vgoAutoHidePaging in AValue)) or (LAutoHideSearchPanel <> (vgoAutoHideSearchPanel in AValue)) then
    RefreshPageIndexAndGridInterface;
end;

procedure TCustomVisualGrid.SetRows(ARow: Integer; AValue: Variant);
begin
  FDataTable.Rows[ARow] := AValue;
  FDrawGrid.InvalidateRow(ARow + 1); // + fixed row
end;

procedure TCustomVisualGrid.SetShowAllData(AValue: boolean);
begin
  if AValue then PageMode := vpmSingle
  else if FPageMode = vpmSingle then PageMode := vpmManual;
end;

procedure TCustomVisualGrid.SetAutoPageSize(AValue: boolean);
begin
  if AValue then PageMode := vpmAuto
  else if FPageMode = vpmAuto then PageMode := vpmManual;
end;

procedure TCustomVisualGrid.ResetModeScrollPosition(AClearPending: Boolean);
begin
  if not FModeScrollResetPending or not Assigned(Parent) then
    Exit;
  // A paging mode starts at a new viewport origin. Reapply after publication:
  // native row clamping and retained selection can move the scrollbars again.
  if FDrawGrid.RowCount > FDrawGrid.FixedRows then
    FDrawGrid.TopRow := FDrawGrid.FixedRows;
  if FDrawGrid.ColCount > FDrawGrid.FixedCols then
    FDrawGrid.LeftCol := FDrawGrid.FixedCols;
  if AClearPending and not FRefreshPending then
    FModeScrollResetPending := False;
end;

procedure TCustomVisualGrid.SetPageMode(AValue: TVisualGridPageMode);
begin
  if FPageMode = AValue then Exit;
  FModeScrollResetPending := True;
  FPageMode := AValue;
  FShowAllData := AValue = vpmSingle;
  FAutoPageSize := AValue = vpmAuto;
  // A mode change supersedes an in-flight slice without joining the provider
  // on the UI thread. Completion restores the previous rows until the latest
  // retained request is admitted by this grid's existing refresh policy.
  CancelActiveFetch;
  FPageIndex := 0;
  if not FAutoPageSize then FPageSize := FManualPageSize;
  Inc(FInternalLayoutDepth);
  try
    UpdatePagingControls;
    if FAutoPageSize and FCanPage and Assigned(Parent) then
      FPageSize := ClientRowCount;
  finally
    Dec(FInternalLayoutDepth);
  end;
  SetPageSizeEditText(IntToStr(FPageSize));
  ResetModeScrollPosition(False);
  RefreshPageIndexData(False);
end;

procedure TCustomVisualGrid.SetManualPageSize(AValue: Integer);
begin
  if AValue <= 0 then Exit;
  AValue := Min(AValue,1000000);
  if FManualPageSize = AValue then Exit;
  FManualPageSize := AValue;
  if FPageMode = vpmManual then ChangePageSize(AValue,True,True)
  else if FPageMode = vpmSingle then begin
    FPageSize := AValue;
    SetPageSizeEditText(IntToStr(AValue));
  end;
end;

procedure TCustomVisualGrid.SetCanPage(AValue: boolean);
begin
  if FCanPage=AValue then
    Exit;

  FCanPage:=AValue;
  CancelActiveFetch;
  FPageIndex := 0;
  Inc(FInternalLayoutDepth);
  try
    UpdatePagingControls;
    if FAutoPageSize and FCanPage and Assigned(Parent) then
      FPageSize := ClientRowCount;
  finally
    Dec(FInternalLayoutDepth);
  end;
  FBottomPanel.Visible:=FCanPage;
  if csDesigning in ComponentState then
    if FBottomPanel.Visible then
      FBottomPanel.ControlStyle := FBottomPanel.ControlStyle - [csNoDesignVisible]
    else
      FBottomPanel.ControlStyle := FBottomPanel.ControlStyle + [csNoDesignVisible];

  LayoutChanged;
  RefreshPageIndexData(False);
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

procedure TCustomVisualGrid.CancelActiveFetch;
begin
  if not Assigned(FActiveThread) then
    Exit;
  // Terminate is a cooperative request. Completion owns joining and restoring
  // the retained page, including when a provider ignores cancellation briefly.
  FActiveRefreshCancelled := True;
  FActiveThread.Terminate;
end;

procedure TCustomVisualGrid.SetRefreshSuspended(AValue: Boolean);
begin
  if FRefreshSuspended = AValue then Exit;
  if FShuttingDown then Exit;
  if AValue then begin
    FRefreshSuspended := True;
    // The caller owns synchronization artwork while ordinary refreshes pause.
    EndLoading;
    // Keep newer queued input intact. With no queued input, retain the active
    // page intent so resuming also refreshes a view which received no events.
    if not FRefreshPending then begin
      FPendingRefreshParams := CaptureRefreshParams;
      FPendingRefreshSource := FDataSource;
      FRefreshPending := True;
      FRefreshClientPending := True;
    end;
    if Assigned(FActiveThread) then begin
      FRefreshColumnsPending := FRefreshColumnsPending or
        TFetchDataThread(FActiveThread).FLastFetchDataResult.RefreshColumns;
      // This flag belongs to the active request, not to the pause state: an
      // intervening resume must never make its cancelled result valid again.
      CancelActiveFetch;
    end;
  end else begin
    // Apply a queued real resize before admitting the one latest page. Layout
    // remains visual while the suspension guard is still in force.
    if FDelayedBoundsChangeTimer.Enabled then DelayedBoundsChange(Self);
    FRefreshSuspended := False;
    FRefreshCancelled := False;
    if FRefreshPending then FRefreshReady := True;
    ExecutePendingRefresh;
  end;
end;

procedure TCustomVisualGrid.RefreshPageIndexData(ARefreshColumns: boolean);
begin
  QueueRefresh(ARefreshColumns, True, True);
end;

procedure TCustomVisualGrid.QueueRefresh(ARefreshColumns, AClientInput,
  ANotify: Boolean);
begin
  if FShuttingDown then exit;
  // Old result layout cannot replace newer client page/filter/source intent.
  if not AClientInput and FRefreshCancelled then Exit;
  if not AClientInput and FRefreshClientPending then Exit;
  if AClientInput then FRefreshCancelled := False;
  FRefreshPending := True;
  FRefreshColumnsPending := FRefreshColumnsPending or ARefreshColumns;
  FRefreshClientPending := AClientInput;
  FPendingRefreshParams := CaptureRefreshParams;
  FPendingRefreshSource := FDataSource;
  FRefreshReady := not AClientInput;
  // A throttled selection/page change must show that its retained rows are
  // awaiting replacement even before a provider worker may start.
  if AClientInput and not FRefreshSuspended and Assigned(FDataSource) then
    BeginLoading;
  if ANotify then FRefreshThrottle.Notify;
end;

procedure TCustomVisualGrid.RefreshThrottleDelivery(Sender: TObject);
begin
  if FShuttingDown then Exit;
  FRefreshReady := True;
  ExecutePendingRefresh(True);
end;

procedure TCustomVisualGrid.RefreshThrottleWaitingChanged(Sender: TObject);
begin
  if FShuttingDown then Exit;
  // Cancel and mode changes also end Waiting. They cannot admit page work or
  // allow an older worker's result layout to restart a cancelled request.
  if not FRefreshThrottle.Waiting then begin
    FRefreshReady := False;
    FRefreshCancelled := True;
    if not FFetchInProgress then EndLoading;
  end;
end;

procedure TCustomVisualGrid.RefreshThrottleFinished(Sender: TObject);
begin
  if FShuttingDown then Exit;
  // Only the owned timer's genuine quiet finish releases deferred single-input
  // work. Explicit Cancel/mode changes do not publish this lifecycle event.
  FRefreshCancelled := False;
  if FRefreshPending then begin
    FRefreshReady := True;
    if FRefreshClientPending and not FRefreshSuspended and
      Assigned(FPendingRefreshSource) then begin
      // WaitingChanged must hide cancelled work immediately. A genuine quiet
      // finish restores this request's existing presentation interval before
      // admission, so a deferred leading request does not flash another delay.
      FAutomaticLoading := True;
      RefreshLoadingPresentation;
    end;
  end;
  ExecutePendingRefresh;
end;

procedure TCustomVisualGrid.ExecutePendingRefresh(AOrdinaryAdmission: Boolean);
var
  LRefreshColumns: Boolean;
  LPreviousDataTable: TDataTable;
begin
  if FShuttingDown or FRefreshSuspended or FExecutingRefresh or (FInternalLayoutDepth > 0) or
    Assigned(FActiveThread) or not FRefreshPending or not FRefreshReady then Exit;
  // Only the ordinary admission itself can lead while Waiting. Worker and
  // layout replay cannot bypass the latest request's quiet interval.
  if not AOrdinaryAdmission and FRefreshThrottle.Waiting then Exit;
  FExecutingRefresh := True;
  try
    FActiveRefreshParams := FPendingRefreshParams;
    FActiveRefreshSource := FPendingRefreshSource;
    FActiveRefreshCancelled := False;
    LRefreshColumns := FRefreshColumnsPending;
    FRefreshPending := False;
    FRefreshColumnsPending := False;
    FRefreshClientPending := False;
    FRefreshReady := False;
    FPendingRefreshSource := nil;
    FPageIndex := FActiveRefreshParams.PageIndex;
    if FActiveRefreshParams.PageSize > 0 then
      FPageSize := FActiveRefreshParams.PageSize;
    SetPageSizeEditText(IntToStr(FPageSize));
    if Assigned(FActiveRefreshSource) and FetchDataInThread then
      TFetchDataThread.Create(Self, LRefreshColumns)
    else begin
      LPreviousDataTable := FDataTable;
      try
        BeforeFetchPage;
        FDataTable := Default(TDataTable);
        FLastFetchDataResult.FromThread := False;
        FLastFetchDataResult.RefreshColumns := LRefreshColumns;
        FetchPage(FLastFetchDataResult.FetchResult);
      except
        on E: Exception do begin
          // A synchronous provider can fail after replacing its output table.
          // Retain the accepted page and end loading before preserving the error.
          FDataTable := LPreviousDataTable;
          ShowLoadingError(E.Message);
          raise;
        end;
      end;
      AfterFetchPage;
    end;
  finally
    FExecutingRefresh := False;
    if not Assigned(FActiveThread) then FActiveRefreshSource := nil;
  end;
  if not Assigned(FActiveThread) then ExecutePendingRefresh;
end;

procedure TCustomVisualGrid.ResizeSearchEdit(ACol: Integer);
var
  LEdit: TSearchEdit;
  LEditOnLeft, LEditOnRight: TSearchEdit;
  //LFixedRect: TRect;
  LRect: TRect;
begin
  // Data may be assigned before a native parent exists. CellRect creates VCL
  // handles, so replay this geometry from SetParent instead of forcing them.
  if not Assigned(Parent) then Exit;
  if ACol > FMultiSearchEdits.Count - 1 then
    exit;
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

  if ACol = {$IFDEF FPC}TDrawGridAccess(FDrawGrid).GCache.VisibleGrid.Right{$ELSE}(FDrawGrid.LeftCol+FDrawGrid.VisibleColCount-1){$ENDIF} then
    if Assigned(LEditOnRight) then
      LEditOnRight.Visible:=false;
  if ACol = {$IFDEF FPC}TDrawGridAccess(FDrawGrid).GCache.VisibleGrid.Left{$ELSE}FDrawGrid.LeftCol{$ENDIF} then
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
var
  LPreviousFooterHeight: Integer;
  LWasVisible: boolean;
  LHasFilter: boolean;

  function RecalcPageCount: boolean;
  var
    LCount: Integer;
  begin
    if FIgnoreRecalcPageCount or not FAutoPageSize or not FCanPage then
      Exit(false);
    LCount := ClientRowCount;
    Result := LCount <> PageSize;
    if Result then
      ChangePageSize(LCount, FInternalLayoutDepth = 0, FInternalLayoutDepth = 0);
  end;

  function FindProperFilter: boolean;
  var
    f: TColumnFilter;
  begin
    for f in FFilters do
      if f.Filter <> vgfSortable then
        Exit(true);
    Result := false;
  end;

begin
  {$IFNDEF FPC}if not Assigned(Parent) then Exit;{$ENDIF}
  // if filter is active then vgoAutoHideSearchPanel should be not considered.
  // Note: vgfSortable is not considered as 'real' filter
  LHasFilter := (FFilters.Count > 0) and FindProperFilter;

  SetPageIndexEditText(IntToStr(Succ(FPageIndex)));
  FPageCountLabel.Caption := Format('/%d',[FPageCount]);

  LWasVisible := FBottomPanel.Visible;
  LPreviousFooterHeight := FBottomPanel.Height;
  UpdatePagingControls;
  if ((FBottomPanel.Visible <> LWasVisible) or
    (FBottomPanel.Height <> LPreviousFooterHeight)) and RecalcPageCount then Exit;

  // show or hide Search Panel (related to option vgoAutoHideSearchPanel)
  if (vgoAutoHideSearchPanel in FOptions) and (FPageCount in [0,1]) and not LHasFilter then
  begin
    if FCanSearch then
    begin
      // if widget control is set, only hide search controls
      LWasVisible := FTopPanel.Visible;
      FTopPanel.Visible := Assigned(FWidgetControl);
      FTopPanelRight.Visible := not Assigned(FWidgetControl);
      FTopPanelMultiSearch.Visible := false;
      if (FTopPanel.Visible <> LWasVisible) and RecalcPageCount then
        Exit;
    end;
  end
  else
  begin
    LWasVisible := FTopPanel.Visible;
    FTopPanel.Visible := FCanSearch;
    FTopPanelRight.Visible := true;
    FTopPanelMultiSearch.Visible := smMulti = FSearchMode;
    if (FTopPanel.Visible <> LWasVisible) and RecalcPageCount then
      Exit;
  end;

  FDrawGrid.Refresh;
end;

function TCustomVisualGrid.AddColumn(const AName: String) : TVisualColumn;
begin
  Result := TVisualColumn.Create(Self);
  Result.Index := FColumns.Count;
  Result.Name := AName;
  if Result.Index = FDefaultStretchedColumn then
    Result.StretchedToFill := true;
  Result.AttachGridColumn(FDrawGrid.Columns.Add);
  FColumns.Add(Result);
end;

procedure TCustomVisualGrid.ReloadColumns;
var
  i: Integer;
  LEdit: TSearchEdit;
  LColumn: TVisualColumn;
  LWasIgnoringRefresh: Boolean;
begin
  FSortColumn := nil;
  FDrawGrid.Columns.Clear; // clear TDrawGrid columns
  FDrawGrid.Columns.BeginUpdate;
  for i := 0 to FColumns.Count - 1 do begin
    LColumn := FColumns[i];
    LColumn.AttachGridColumn(FDrawGrid.Columns.Add);  // recreate TDrawGrid column
    // invoke client initialization
    if Assigned(FOnColumnInitialize) then
      FOnColumnInitialize(Self, LColumn);
  end;
  FDrawGrid.Columns.EndUpdate;
  {$IFNDEF FPC}FDrawGrid.FitColumns;{$ENDIF}
  // TODO: may be optimized
  FMultiSearchEdits.Clear;
  FSearchButton.Visible := true;
  FMultiSearchCheckComboBox.Clear;
  for i := 0 to FColumns.Count - 1 do begin
    LEdit := TSearchEdit.Create(FTopPanelMultiSearchClient, Self);
    FMultiSearchEdits.Add(LEdit);
    LEdit.Column := FColumns[i];
    LEdit.EditVisible := FMultiSearchMenuItem.Checked AND (FColumns[i].Filters * NON_SORTABLE_FILTER <> []);
    ResizeSearchEdit(i);
    if FColumns[i].Filters * NON_SORTABLE_FILTER <> [] then
    begin
      FMultiSearchCheckComboBox.AddItem(FColumns[i].Name, cbChecked);
      FMultiSearchCheckComboBox.Objects[FMultiSearchCheckComboBox.Items.Count-1] := LEdit;
      FMultiSearchCheckComboBox.Checked[FMultiSearchCheckComboBox.Items.Count-1] := FMultiSearchMenuItem.Checked;
    end;
    if (vgfSortable in FColumns[i].Filters) and not (vgoSortDirectionAllowNone in Options) and
      (SortMode <> smNone) and (FColumns[i].SortDirection = sdNone) then begin
      // This default belongs to the admitted column population, not a user
      // sort request. Explicit consumer callbacks above retain normal admission.
      LWasIgnoringRefresh := FColumns[i].FIgnoreRefresh;
      FColumns[i].FIgnoreRefresh := True;
      try FColumns[i].SortDirection := sdAscending;
      finally FColumns[i].FIgnoreRefresh := LWasIgnoringRefresh; end;
    end;
  end;
  if FDrawGrid.Columns.Count > 0 then
    FTopPanelMultiSearch.Height:=FMultiSearchEdits.Last.FPanel.Height;

  // last item doesn't need to store object
  FMultiSearchCheckComboBox.AddItem(sExpression, cbChecked);
end;

procedure TCustomVisualGrid.LayoutChanged;
begin
  {$IFNDEF FPC}if not Assigned(Parent) then Exit;{$ENDIF}
  // layout has changed, maybe more space is available
  if AutoPageSize and FCanPage then
    ChangePageSize(ClientRowCount,True,True);
end;

function TCustomVisualGrid.ClientRowCount: Integer;
var
  LAvailable: Int64;
  LRow, LRowHeight, LLineWidth: Integer;
begin
  // ClientHeight already excludes native borders/scrollbars. LCL includes the
  // separator in RowHeights; VCL adds its DPI-scaled separator to each row.
  LAvailable := FDrawGrid.ClientHeight;
  LLineWidth := 0;
  {$IFDEF FPC}
  if (FDrawGrid.FixedRows > 0) and (FDrawGrid.RowCount > 0) and
    (FDrawGrid.ColCount > 0) then
    Dec(LAvailable, FDrawGrid.CellRect(0,0).Top);
  {$ELSE}
  if FDrawGrid.Options * [goFixedHorzLine,goHorzLine] <> [] then
    LLineWidth := TDrawGridAccess(FDrawGrid).ActualGridLineWidth;
  // VCL's final separator is outside CellRect: the row is complete when its
  // content reaches ClientHeight, even if that following separator is clipped.
  Inc(LAvailable,LLineWidth);
  {$ENDIF}
  for LRow := 0 to FDrawGrid.FixedRows - 1 do
    Dec(LAvailable, Max(0,FDrawGrid.RowHeights[LRow]) + LLineWidth);

  // Honor explicit row heights on the current page, then use the default for
  // rows not fetched yet. Never admit a row whose bottom lies off the viewport.
  Result := 0;
  LRow := FDrawGrid.FixedRows;
  while (LRow < FDrawGrid.RowCount) and (LAvailable >= 0) do begin
    LRowHeight := Max(0,FDrawGrid.RowHeights[LRow]) + LLineWidth;
    if LRowHeight > LAvailable then Break;
    Dec(LAvailable,LRowHeight);
    Inc(Result);
    Inc(LRow);
  end;
  if LRow >= FDrawGrid.RowCount then begin
    LRowHeight := Max(1,FDrawGrid.DefaultRowHeight + LLineWidth);
    if LAvailable > 0 then Inc(Result,LAvailable div LRowHeight);
  end;
  // PageSize has a one-row minimum even before the first usable native layout.
  Result := Max(1,Result);
end;

procedure TCustomVisualGrid.HidePageSizeControls(AVisible: boolean);
begin
  FPageSizeEdit.Visible:=AVisible;
  FPageSizeLabel.Visible:=AVisible;
end;

procedure TCustomVisualGrid.HidePageNavigationControls(AVisible: boolean);
begin
  FButtonFirst.Visible := AVisible;
  FButtonPrevious.Visible := AVisible;
  FPageIndexEdit.Visible := AVisible;
  FPageCountLabel.Visible := AVisible;
  FButtonNext.Visible := AVisible;
  FButtonLast.Visible := AVisible;
end;

procedure TCustomVisualGrid.UpdatePagingControls;
var
  LNavigation, LManual, LInline, LManualBelow: Boolean;
  LWidth, LAvailable, LMargin, LGap, LModeGap, LModeWidth,
    LTotalWidth, LManualWidth, LLabelWidth, LEditWidth, LEditHeight,
    LButtonWidth, LButtonMinimum, LButtonHeight, LPageWidth,
    LRequiredPageWidth, LCountWidth, LRowHeight,
    LGroupWidth, LGroupHeight, LGroupRowHeight, LGroupLeft, LGroupTop,
    LLeftWidth, LTopHeight, LHeight, LManualLeft, LManualTop, LX, LY, I: Integer;

  function ControlTextWidth(AControl: TControl; AFont: TFont;
    const AText: String): Integer;
  var LCanvas: TControlCanvas;
  begin
    // Native child fonts can differ from the composite canvas after scaling
    // or custom painting; measure the control which will display this text.
    LCanvas := TControlCanvas.Create;
    try
      LCanvas.Control := AControl;
      LCanvas.Font.Assign(AFont);
      Result := LCanvas.TextWidth(AText);
    finally
      LCanvas.Free;
    end;
  end;

  procedure PlaceNavigation(AControl: TControl; AWidth, AHeight: Integer);
  begin
    // Only extreme widths wrap the tail. The selector and first-page button
    // are placed together before this flow and always remain adjacent.
    if LX+AWidth > LGroupWidth then begin
      LX := 0;
      Inc(LY,LGroupRowHeight+LGap);
    end;
    AControl.SetBounds(LX,LY+(LGroupRowHeight-AHeight) div 2,AWidth,AHeight);
    Inc(LX,AWidth+LGap);
  end;
begin
  if not Assigned(FPageModeButton) then Exit;
  for I := 0 to FPageModeMenu.Items.Count-1 do
    FPageModeMenu.Items[I].Checked := I = Ord(FPageMode);
  FPageModeButton.Hint := 'Paging mode: '+FPageModeMenu.Items[Ord(FPageMode)].Caption+
    '. Choose Single, Auto or Manual.';
  if not Assigned(Parent) then Exit;
  FBottomPanel.Visible := FCanPage;
  FBottomRightPanel.Visible := FCanPage;
  FPageModeButton.Visible := FCanPage;
  LManual := FCanPage and (FPageMode = vpmManual);
  LNavigation := FCanPage and (FPageMode <> vpmSingle);
  if (vgoAutoHidePaging in FOptions) and (FTotalDataCount >= 0) and
    (FTotalDataCount <= FPageSize) then LNavigation := False;
  HidePageSizeControls(LManual);
  HidePageNavigationControls(LNavigation);
  if FAutoPageSize and FCanPage then
    FDrawGrid.ScrollBars := {$IFDEF FPC}ssAutoHorizontal{$ELSE}ssHorizontal{$ENDIF}
  else FDrawGrid.ScrollBars := {$IFDEF FPC}ssAutoBoth{$ELSE}ssBoth{$ENDIF};

  LMargin := Scale96ToFont(7);
  LGap := Scale96ToFont(4);
  LModeGap := Scale96ToFont(4);
  LWidth := FBottomPanel.ClientWidth;
  LAvailable := Max(1,LWidth-2*LMargin);
  LTotalWidth := 0;
  if FAllRecordsCountLabel.Visible then LTotalWidth := FAllRecordsCountLabel.Width;
  LLabelWidth := Max(FPageSizeLabel.Width,Scale96ToFont(60));
  LEditWidth := Max(Scale96ToFont(52),4*Abs(Font.Height)+Scale96ToFont(8));
  LManualWidth := LLabelWidth+LMargin+LEditWidth;
  LEditHeight := Max(Scale96ToFont(21),Abs(Font.Height)+Scale96ToFont(8));
  // LCL edits can auto-size taller than the requested height. Reserve their
  // actual native height before placing the separate manual-size band.
  FPageSizeEdit.Height := LEditHeight;
  FPageIndexEdit.Height := LEditHeight;
  LEditHeight := Max(LEditHeight,Max(FPageSizeEdit.Height,FPageIndexEdit.Height));
  LButtonMinimum := ControlTextWidth(FButtonFirst,FButtonFirst.Font,'|<')+Scale96ToFont(6);
  LButtonWidth := Max(Scale96ToFont(25),LButtonMinimum);
  LButtonHeight := Max(Scale96ToFont(25),LEditHeight);
  LModeWidth := LButtonHeight;
  // Required text space applies in both the normal and compact layout. LCL
  // can scale a child font independently of these nominal control widths.
  LRequiredPageWidth :=
    ControlTextWidth(FPageIndexEdit,FPageIndexEdit.Font,IntToStr(Max(1,FPageCount)))+
    Max(0,FPageIndexEdit.Width-FPageIndexEdit.ClientWidth)+Scale96ToFont(8);
  LPageWidth := Max(Scale96ToFont(56),LRequiredPageWidth);
  LCountWidth := Max(FPageCountLabel.Width,Scale96ToFont(18));
  LGroupWidth := LModeWidth;
  if LNavigation then
    Inc(LGroupWidth,LModeGap+4*LButtonWidth+LPageWidth+LCountWidth+5*LGap);
  if LNavigation and (LGroupWidth > LAvailable) then begin
    // Compact the navigation before moving other footer content to another
    // row. The icon button belongs to this right-anchored native control group.
    LGap := Scale96ToFont(2);
    LButtonWidth := Max(Scale96ToFont(21),LButtonMinimum);
    // Compact spacing must still display the largest valid page number,
    // including the native edit border and room for its text margins/caret.
    LPageWidth := Max(Scale96ToFont(36),LRequiredPageWidth);
    LGroupWidth := LModeWidth+LModeGap+4*LButtonWidth+LPageWidth+LCountWidth+5*LGap;
  end;
  LGroupWidth := Min(LGroupWidth,LAvailable);
  if LNavigation then LGroupWidth := Max(LGroupWidth,LModeWidth+LModeGap+LButtonWidth)
  else LGroupWidth := Max(LGroupWidth,LModeWidth);
  LGroupRowHeight := Max(LButtonHeight,LEditHeight);
  FPageModeButton.SetBounds(0,(LGroupRowHeight-LButtonHeight) div 2,LModeWidth,LButtonHeight);
  LGroupRowHeight := Max(LGroupRowHeight,FPageModeButton.Height);
  FPageModeButton.Top := (LGroupRowHeight-FPageModeButton.Height) div 2;
  UpdatePageModeGlyph;
  LX := LModeWidth+LModeGap;
  LY := 0;
  if LNavigation then begin
    FButtonFirst.SetBounds(LX,(LGroupRowHeight-LButtonHeight) div 2,LButtonWidth,LButtonHeight);
    Inc(LX,LButtonWidth+LGap);
    PlaceNavigation(FButtonPrevious,LButtonWidth,LButtonHeight);
    PlaceNavigation(FPageIndexEdit,LPageWidth,LEditHeight);
    PlaceNavigation(FPageCountLabel,LCountWidth,FPageCountLabel.Height);
    PlaceNavigation(FButtonNext,LButtonWidth,LButtonHeight);
    PlaceNavigation(FButtonLast,LButtonWidth,LButtonHeight);
  end;
  LGroupHeight := LY+LGroupRowHeight;
  LRowHeight := Max(Scale96ToFont(40),LGroupRowHeight+Scale96ToFont(14));
  LLeftWidth := LTotalWidth;
  if LManual then begin
    if LLeftWidth > 0 then Inc(LLeftWidth,2*LMargin);
    Inc(LLeftWidth,LManualWidth);
  end;
  LInline := LLeftWidth+LGroupWidth+3*LMargin <= LWidth;
  LManualBelow := LManual and (LLeftWidth > LAvailable);
  LTopHeight := FAllRecordsCountLabel.Height;
  if LManual then LTopHeight := Max(LTopHeight,LEditHeight);
  if LManualBelow then LTopHeight := FAllRecordsCountLabel.Height+LGap+LEditHeight;
  if LInline then begin
    LHeight := Max(LRowHeight,LGroupHeight+Scale96ToFont(4));
    LGroupTop := (LHeight-LGroupHeight) div 2;
    LManualTop := (LHeight-LEditHeight) div 2;
    FAllRecordsCountLabel.Top := (LHeight-FAllRecordsCountLabel.Height) div 2;
  end else begin
    LHeight := Max(LRowHeight,LTopHeight+LGroupHeight+Scale96ToFont(2));
    LGroupTop := LHeight-LGroupHeight-Scale96ToFont(2);
    LManualTop := (LTopHeight-LEditHeight) div 2;
    FAllRecordsCountLabel.Top := (LTopHeight-FAllRecordsCountLabel.Height) div 2;
    if LManualBelow then begin
      FAllRecordsCountLabel.Top := 0;
      LManualTop := FAllRecordsCountLabel.Height+LGap;
    end;
  end;
  LManualLeft := LMargin;
  if (LTotalWidth > 0) and not LManualBelow then Inc(LManualLeft,LTotalWidth+2*LMargin);
  FAllRecordsCountLabel.Left := LMargin;
  FPageSizeLabel.SetBounds(LManualLeft,LManualTop+
    (LEditHeight-FPageSizeLabel.Height) div 2,LLabelWidth,FPageSizeLabel.Height);
  FPageSizeEdit.SetBounds(LManualLeft+LLabelWidth+LMargin,LManualTop,LEditWidth,LEditHeight);
  FBottomPanel.Height := LHeight;
  LGroupLeft := Max(0,LWidth-LMargin-LGroupWidth);
  FBottomRightPanel.SetBounds(LGroupLeft,LGroupTop,LGroupWidth,LGroupHeight);
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

  if Assigned(FActiveThread) and (FRefreshThrottle.Mode <> temNone) then begin
    // Component-backed sources are not necessarily reference counted. Record
    // the new input before joining, but retain the existing safe replacement
    // lifetime: a caller may release its old source after this setter returns.
    FDataSource := ADataSource;
    RefreshPageIndexData(True);
    FJoiningFetch := True;
    try FActiveThread.WaitFor;
    finally FJoiningFetch := False; end;
    FetchDataThreadCompleted(Self);
    Exit;
  end;

  // FetchPage reads this interface on the worker; never replace it mid-fetch.
  if Assigned(FActiveThread) then begin
    FJoiningFetch := True;
    try FActiveThread.WaitFor;
    finally FJoiningFetch := False; end;
    FetchDataThreadCompleted(Self);
    // A pending refresh may have started another fetch of the old source.
    while Assigned(FActiveThread) do begin
      FRefreshPending := false;
      FRefreshColumnsPending := false;
      FRefreshClientPending := False;
      FRefreshReady := False;
      FPendingRefreshSource := nil;
      FJoiningFetch := True;
      try FActiveThread.WaitFor;
      finally FJoiningFetch := False; end;
      FetchDataThreadCompleted(Self);
    end;
  end;
  FDataSource := ADataSource;

  RefreshPageIndexData(true);
end;

procedure TCustomVisualGrid.SetPageIndex(Value: Integer);
begin
  if (FPageMode = vpmSingle) or not FCanPage then Value := 0;
  if Value >= FPageCount then
    Value := FPageCount - 1;
  if Value < 0 then
    Value := 0;

  if FPageIndex = Value then
    Exit;

  FPageIndex := Value;
  RefreshPageIndexData(false);
end;

procedure TCustomVisualGrid.SetPageIndexEditText(const AStr: String);
begin
  Include(FGUIUpdates, updPageIndex);
  FPageIndexEdit.Text := AStr;
  Exclude(FGUIUpdates, updPageIndex);
end;

procedure TCustomVisualGrid.SetPageSizeEditText(const AStr: String);
begin
  Include(FGUIUpdates, updPageSize);
  FPageSizeEdit.Text := AStr;
  Exclude(FGUIUpdates, updPageSize);
end;

procedure TCustomVisualGrid.BeforeFetchPage;
begin
  if FActiveRefreshParams.PageSize <= 0 then FPageIndex := 0
  else if Assigned(FActiveRefreshSource) then
    if FPageIndex >= FPageCount then
      FPageIndex := FPageCount - 1;
  FActiveRefreshParams.PageIndex := FPageIndex;
end;

procedure TCustomVisualGrid.FetchPage(out AResult: TPageFetchResult);
begin
  if Assigned(FActiveRefreshSource) then
    AResult := FActiveRefreshSource.FetchPage(FActiveRefreshParams, FDataTable)
  else begin
    // Clearing a source publishes an empty page too. Stale counts/rows would
    // otherwise keep automatic layout replaying the removed page indefinitely.
    FDataTable := Default(TDataTable);
    FillChar(AResult, SizeOf(AResult), #0);
    AResult.PageIndex := -1;
  end;
end;

function TCustomVisualGrid.CaptureRefreshParams: TPageFetchParams;

  // additionall fill with sort direction
  procedure FillFilter;
  var
    LColumnsToAdd: TList<Integer>;
    i, j, idx: Integer;
    LFilter: TColumnFilter;

    function UpdateFilterSortDirection(const AColumnName: String; ASortDirection: TSortDirection): boolean;
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
    try
      for i := 0 to FColumns.Count - 1 do
        if FColumns[i].SortDirection <> sdNone then
        begin
          // try to find column in existing filters
          // if filter not found we need to create it later
          if not UpdateFilterSortDirection(FColumns[i].SortBinding, FColumns[i].SortDirection) then
            LColumnsToAdd.Add(i);
        end else
          UpdateFilterSortDirection(FColumns[i].SortBinding, sdNone);

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
        LFilter.ColumnName:= FColumns[idx].SortBinding;
        LFilter.SortSequence := FColumns[idx].SortSequence;
        LFilter.Sort := FColumns[idx].SortDirection;
        FFilters[i] := LFilter;
      end;
    finally
      LColumnsToAdd.Free;
    end;
  end;

begin
  FillFilter;
  Result := TPageFetchParams.Create(FPageIndex, FPageSize, FFilters.ToArray,
    IIF(FSearchMode = smMulti, foAnd, foOr));
  // The source's existing nonpositive-size contract means all matching rows,
  // including rows added after an earlier result or an initially unknown total.
  if (FPageMode = vpmSingle) or not FCanPage then begin
    Result.PageIndex := 0;
    Result.PageSize := -1;
  end;
end;

procedure TCustomVisualGrid.AfterFetchPage;

  procedure UpdateDrawGridRect;
  var
    LNewRect: TGridRect;
    LOptions: TGridOptions;
  begin
    {$IFDEF FPC}
    LNewRect := TGridRect.Create(FLastSelectionEvent.Col,FLastSelectionEvent.Row+1,
      FLastSelectionEvent.Col+FLastSelectionEvent.ColCount,FLastSelectionEvent.Row+FLastSelectionEvent.RowCount);
    {$ELSE}
    // An empty page and an absent selection both use -1. That sentinel is
    // not a native grid coordinate; retain no selection until rows exist.
    if (Length(FDataTable.Rows)=0) or (FLastSelectionEvent.Col<0) or
      (FLastSelectionEvent.Row<0) or (FLastSelectionEvent.ColCount<=0) or
      (FLastSelectionEvent.RowCount<=0) then begin
      UpdateSelection(stNone,False);
      Exit;
    end;
    LNewRect.Left:=FLastSelectionEvent.Col;
    LNewRect.Top:=FLastSelectionEvent.Row+1;
    LNewRect.Right:=Min(FDrawGrid.ColCount-1,
      FLastSelectionEvent.Col+FLastSelectionEvent.ColCount-1);
    LNewRect.Bottom:=Min(Length(FDataTable.Rows),
      FLastSelectionEvent.Row+FLastSelectionEvent.RowCount);
    if (LNewRect.Left>LNewRect.Right) or (LNewRect.Top>LNewRect.Bottom) then begin
      UpdateSelection(stNone,False);
      Exit;
    end;
    {$ENDIF}

    FIgnoreSelectionEvent:=true;
    TDrawGridAccess(FDrawGrid).MoveExtend(false, LNewRect.Left, LNewRect.Bottom);
    FIgnoreSelectionEvent:=false;

    if goRangeSelect in FDrawGrid.Options then
      FDrawGrid.Selection := LNewRect
    else
      FDrawGrid.Row := FLastSelectionEvent.Row + 1;
  end;

begin
  Inc(FInternalLayoutDepth);
  try
  with FLastFetchDataResult do
  begin
    if FromThread then
    begin
      Dispose(FCachedDataTable);
      FCachedDataTable := nil;
      FFetchInProgress:=false;
      EndLoading;
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

    if RefreshColumns and Assigned(FActiveRefreshSource) then
      ReloadColumns;
    // when last page has different count than all other pages
    // we need to handle events on own way
    FIgnoreSelectionEvent:=true;
    {$IFDEF FPC}
    FDrawGrid.RowCount := Length(FDataTable.Rows) + 1;
    {$ELSE}
    // VCL must be able to clamp its previous current row during a shrink.
    // Its structural placeholder is selectable only for that update; a final
    // empty result still rejects all data-row selection.
    FDrawGrid.DataRowCount:=Max(1,Length(FDataTable.Rows));
    try
      FDrawGrid.RowCount:=Max(2,Length(FDataTable.Rows)+1);
    finally
      FDrawGrid.DataRowCount:=Length(FDataTable.Rows);
      FIgnoreSelectionEvent:=false;
    end;
    {$ENDIF}
    FIgnoreSelectionEvent:=false;
  end;
  RefreshPageIndexAndGridInterface;
  FIgnoreRecalcPageCount := false;

  // each page means different records so selection should be not moved to
  // new page nor deselected
  if FLastSelectionEvent.Page = FPageIndex then
  begin
    UpdateSelection(FSelectionType, false);
    UpdateDrawGridRect;
  end
  else
    UpdateSelection(stNone, false);
  ResetModeScrollPosition(not FRefreshClientPending);
  finally
    Dec(FInternalLayoutDepth);
  end;
  // Publishing an older page must not overwrite a newer queued client intent.
  if FRefreshClientPending then begin
    FPageIndex := FPendingRefreshParams.PageIndex;
    if FPendingRefreshParams.PageSize > 0 then
      FPageSize := FPendingRefreshParams.PageSize;
    SetPageSizeEditText(IntToStr(FPageSize));
    FFilters.Clear;
    FFilters.AddRange(FPendingRefreshParams.Filter);
  end;
  // An interface does not retain a component-backed provider. Result handling
  // is finished before consumers can replace and release their old source.
  FActiveRefreshSource := nil;
  // Synchronous cached fetches also finish the queued loading presentation.
  // EndLoading retains it when a newer client request is still waiting.
  EndLoading;
  // A consumer requesting another refresh here supplies a new client input.
  if Assigned(FOnFinishedUpdating) then
    FOnFinishedUpdating(Self);
end;

procedure TCustomVisualGrid.UpdateSelection(ASelectionType: TSelectionType; AResetSelection: boolean);
begin
  if FCurrentSelectionType = ASelectionType then
    Exit;

  FCurrentSelectionType:=ASelectionType;
  case FCurrentSelectionType of
    stNone: FDrawGrid.Options:=FDefaultDrawGridOptions;
    stCell: FDrawGrid.Options:=FDefaultDrawGridOptions+[goDrawFocusSelected];
    stRow: FDrawGrid.Options:=FDefaultDrawGridOptions+[goRowSelect];
    stMultiRow: FDrawGrid.Options:=FDefaultDrawGridOptions+[goRowSelect,goRangeSelect];
  end;

  if AResetSelection then
    GridSelection(nil, 0, 0);
end;

function TCustomVisualGrid.SelectionsEquals({$IFDEF FPC}constref{$ELSE}const{$ENDIF} A, B: TVisualGridSelection
  ): boolean;
begin
  Result := A.Page = B.Page;
  if Result then
    case FSelectionType of
      stNone: Result := (A.Selections = nil) and (B.Selections = nil);
      stCell: Result := (A.Col = B.Col) and (A.Row = B.Row) and (A.ColCount = B.ColCount);
      stRow: Result := (A.Row = B.Row)  and (A.ColCount = B.ColCount);
      stMultiRow: Result := (A.Row = B.Row) and (A.RowCount = B.RowCount) and (A.ColCount = B.ColCount);
    end;
end;

procedure TCustomVisualGrid.ResetSelection(out ASelection: TVisualGridSelection);
begin
  ASelection.Selections := nil;
  ASelection.Page := -1;
end;

procedure TCustomVisualGrid.SetPageSize(Value: Integer);
begin
  if Value <= 0 then Exit;
  CheckRangeForPageSize(Value);
  FManualPageSize := Value;
  if FPageMode = vpmSingle then begin
    FPageSize := Value;
    SetPageSizeEditText(IntToStr(Value));
    Exit;
  end;
  ChangePageSize(Value, True, True);
end;

procedure TCustomVisualGrid.ChangePageSize(Value: Integer;
  AClientInput, ANotify: Boolean);
begin
  if FPageSize = Value then
    Exit;

  CheckRangeForPageSize(Value);
  // Invalid inputs normalize to the current capacity. A transient tiny client
  // rectangle must not keep refetching that unchanged effective page size.
  if FPageSize = Value then
    Exit;
  FPageSize := Value;
  SetPageSizeEditText(IntToStr(FPageSize));
  if not AClientInput and FAutoPageSize and FRefreshClientPending then begin
    // Result chrome may reveal the final capacity while newer client work is
    // waiting. Keep its page/filter/source intent and merge geometry only.
    FPendingRefreshParams.PageSize := Value;
    Exit;
  end;
  // Hiding paging/search controls can expose extra rows after a fetch. If the
  // entire result already fits, changing capacity needs layout only; fetching
  // again would bypass the consumer's refresh policy for the same data.
  if (not Assigned(FActiveThread)) and not FRefreshPending and (FPageIndex <= 0) and
    (FTotalDataCount >= 0) and (Length(FDataTable.Rows) = FTotalDataCount) and
    (Value >= FTotalDataCount) then begin
    if FTotalDataCount > 0 then begin
      FPageIndex := 0;
      FPageCount := 1;
    end else begin
      FPageIndex := -1;
      FPageCount := 0;
    end;
    Inc(FInternalLayoutDepth);
    try RefreshPageIndexAndGridInterface;
    finally Dec(FInternalLayoutDepth); end;
    if AClientInput and ANotify then begin
      FRefreshReady := False;
      FRefreshThrottle.Notify;
    end;
    Exit;
  end;
  QueueRefresh(False, AClientInput, ANotify);
end;

procedure TCustomVisualGrid.SetSelectionType(AValue: TSelectionType);
begin
  if FSelectionType=AValue then
    Exit;
  FSelectionType:=AValue;
  // reset internal selection variable for stNone (and call event)
  if FSelectionType = stNone then
    ResetSelection(FLastSelection);
  UpdateSelection(FSelectionType, true);
  GridSelection(Self, 0, 0)
end;

procedure TCustomVisualGrid.SetDeselectionType(AValue: TDeselectionType);
begin
  if FDeselectionType=AValue then
    Exit;

  FDeselectionType:=AValue;

  if FDeselectionType = dtNone then
  begin
    ResetSelection(FLastSelection);
    UpdateSelection(SelectionType, true);
  end else
    FLastSelection := FLastSelectionEvent;
end;

procedure TCustomVisualGrid.ClearSelection(AIgnoreDeselectionType: boolean);
begin
  if (FDeselectionType <> dtNone) or AIgnoreDeselectionType then
  begin
    ResetSelection(FLastSelection);
    // Physical selection must also clear when no selection event is registered.
    UpdateSelection(stNone, true);
    if FLastSelectionEvent.Page <> FPageIndex then
      GridSelection(nil, 0, 0);
  end;
end;

procedure TCustomVisualGrid.SetWidgetControl(AValue: TControl);
var
  LRefreshPageIndexAndGridInterface: boolean;
begin
  if FWidgetControl=AValue then Exit;

  if Assigned(FWidgetControl) then
    FWidgetControl.Parent := FWidgetControlParent;

  LRefreshPageIndexAndGridInterface := (not Assigned(FWidgetControl) and Assigned(AValue)) or (not Assigned(AValue) and Assigned(FWidgetControl));
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
      {$IFDEF FPC}
      AnchorSideLeft.Control := FTopPanelLeft;
      AnchorSideLeft.Side := asrCenter;
      AnchorSideTop.Control := FTopPanelLeft;
      AnchorSideTop.Side := asrCenter;
      {$ENDIF}
    end;
  end else
    FTopPanelLeft.Width:=0;

  if LRefreshPageIndexAndGridInterface then
    RefreshPageIndexAndGridInterface;
end;

procedure TCustomVisualGrid.StandardDrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
var
  LHandled: boolean;
  LCellData, LRow: Variant;
  LColumn : TVisualColumn;
  {$IFDEF FPC}LStyle : TTextStyle;{$ENDIF}
  LDataTable: PDataTable;

  procedure DrawHeaderCell;
  begin
    // Pre-set user defined styles
    if LColumn.HasHeaderAlignment then begin
      {$IFDEF FPC}
      LStyle := Canvas.TextStyle;
      LStyle.Alignment := LColumn.HeaderAlignment;
      Canvas.TextStyle := LStyle;
      {$ENDIF}
    end;
    if LColumn.HasHeaderFontStyles then Canvas.Font.Style := LColumn.HeaderFontStyles;

    // Standard draw cell
    DoDrawCell(Self, ACol, ARow, Rect, State, LColumn.Name);
  end;

  procedure DrawDataCell;
  begin
    // Pre-set user defined styles
    if LColumn.HasDataAlignment then begin
      {$IFDEF FPC}
      LStyle := Canvas.TextStyle;
      LStyle.Alignment := LColumn.DataAlignment;
      Canvas.TextStyle := LStyle;
      {$ENDIF}
    end;
    if LColumn.HasDataFontStyles then Canvas.Font.Style := LColumn.DataFontStyles;

    // Get row/cell data
    LRow := LDataTable^.Rows[ARow-1];
    LCellData := TDataRowData(LRow)[LColumn.DisplayBinding];

    // Clean data if necessary
    if Assigned(LColumn.Sanitizer) then
      LCellData := LColumn.Sanitizer(LCellData, LRow);

    // Try global cell renderer
    if Assigned(FOnDrawVisualCell) then
      FOnDrawVisualCell(Self, ACol, ARow, Canvas, Rect, State, LCellData, LRow, LHandled);

    // Try column renderer
    if (NOT LHandled) AND Assigned(LColumn.Renderer) then
      LColumn.Renderer(Self, ACol, ARow, Canvas, Rect, State, LCellData, LRow, LHandled);

    // Use default renderer
    if NOT LHandled then
      DoDrawCell(Self, ACol, ARow, Rect, State, LCellData);
  end;

begin
  LHandled := False;
  if ColCount = 0 then Exit;
  LDataTable := ActiveDataTable;
  if (ARow > 0) and (ARow > Length(LDataTable^.Rows)) then
    Exit;
  if ARow = 0 then ResizeSearchEdit(ACol);
  LColumn := FColumns[ACol];
  Rect := CalculateCellContentRect(Rect);
  if ARow = 0 then
    DrawHeaderCell
  else if Assigned(FDataSource) then
    DrawDataCell;
end;

procedure TCustomVisualGrid.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LCol, LRow: Integer;
begin
  if ColCount = 0 then
    Exit;

  // handle special case: when was selected row in fist page (for example row 20)
  // and the last page has 10 rows, when we click to row 5 implicitly is called
  // event for row 10 and next for row 5 (we have one extra event with row 10)
  if (FLastSelectionEvent.Page <> FPageIndex) and (Y <> FDrawGrid.Row) then
  begin
    FDrawGrid.MouseToCell(X, Y, LCol, LRow);
    FIgnoreSelectionEvent:=true;
    TDrawGridAccess(FDrawGrid).MoveExtend(false, LCol, LRow);
    FIgnoreSelectionEvent:=false;
  end;

  case FDeselectionType of
    dtClick:
      case Button of
        mbLeft:
          if (SelectionType <> stNone) and (FDrawGrid.MouseToGridZone(X, Y) = gzNormal) then
            if Assigned(FLastSelectionEvent.Selections) and (FLastSelectionEvent.Page = FPageIndex) then
            begin
              ResetSelection(FLastSelection);
              UpdateSelection(stNone, true);
              FIgnoreSelectionEvent := true;
            end else
              UpdateSelection(FSelectionType, true);
      end;
    dtDefault, dtNone:
      if FDrawGrid.MouseToGridZone(X, Y) = gzNormal then
      case Button of
        mbLeft: UpdateSelection(FSelectionType, true);
      end;
  end;
end;

procedure TCustomVisualGrid.GridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LPopup: TPopupMenu;
  LSelection: TVisualGridSelection;
  i: integer;
  LCol, LRow: longint;
  LContains: boolean;
begin
  LContains:=False;
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
      if (SelectionType <> stNone) and (FDeselectionType <> dtNone) and
       (FDrawGrid.MouseToGridZone(X, Y) = gzNormal) then
      begin
        LSelection := GetSelection;
        if FIgnoreSelectionEvent then
          FIgnoreSelectionEvent:=false
        else
        if (FCurrentSelectionType <> stNone) and SelectionsEquals(LSelection, FLastSelection) then
        begin
          ResetSelection(FLastSelection);
          UpdateSelection(stNone, true)
        end
        else begin
          // lock for select event, otherwise we get false information about selection
          // (highly visible problem for deselection option)
          FIgnoreSelectionEvent := true;
          UpdateSelection(FSelectionType, true);
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
var
  LSelection, LAfterSelection: TVisualGridSelection;
begin
  if Assigned(FOnSelection) and (not FIgnoreSelectionEvent) then
  begin
    if FCurrentSelectionType = stNone then
      ResetSelection(LSelection)
    else
      LSelection := Selection;

    if not SelectionsEquals(FLastSelectionEvent, LSelection) then
    begin
      FIgnoreSelectionEvent := True;
      try
        FLastSelectionEvent := LSelection;
        FOnSelection(Self, LSelection);
        // Only an actual callback change, such as ClearSelection, is another
        // event. FLastSelection remembers mouse-up state and can be older.
        if FCurrentSelectionType = stNone then
          ResetSelection(LAfterSelection)
        else
          LAfterSelection := Selection;
        if not SelectionsEquals(LAfterSelection, LSelection) then
        begin
          FLastSelectionEvent := LAfterSelection;
          FOnSelection(Self, LAfterSelection);
        end;
      finally
        FIgnoreSelectionEvent := False;
      end;
    end;
  end;
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

  LColumn := GetColumn(Index);
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

function TCustomVisualGrid.CalculateCellContentRect(const ARect : TRect) : TRect;
const
{$IFDEF Windows}
  PLATFORM_CELL_RECT_X_OFFSET = 2;
  PLATFORM_CELL_RECT_Y_OFFSET = 2;
{$ELSE}
  {$IFDEF DARWIN}
  PLATFORM_CELL_RECT_X_OFFSET = 2;
  PLATFORM_CELL_RECT_Y_OFFSET = 2;
  {$ELSE} // Linux
  PLATFORM_CELL_RECT_X_OFFSET = 2;
  PLATFORM_CELL_RECT_Y_OFFSET = 2;
  {$ENDIF}
{$ENDIF}
begin
  Result.Left := ClipValue(ARect.Left + FCellPadding.Left + PLATFORM_CELL_RECT_X_OFFSET, 2, 10000);
  Result.Top := ClipValue(ARect.Top + FCellPadding.Top + PLATFORM_CELL_RECT_Y_OFFSET, 2, 100000);
  Result.Right := ClipValue(ARect.Right - FCellPadding.Right - PLATFORM_CELL_RECT_X_OFFSET, 2, 1000000);
  Result.Bottom := ClipValue(ARect.Bottom - FCellPadding.Bottom - PLATFORM_CELL_RECT_Y_OFFSET, 2, 100000);
end;

initialization
  {$IFDEF FPC}{$I UVisualGrid.inc}{$ENDIF}

end.
