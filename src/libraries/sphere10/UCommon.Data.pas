unit UCommon.Data;

{ Copyright (c) 2017 - 2018 Herman Schoenfeld <https://www.sphere10.com>

  Common data tools.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Herman Schoenfeld: main author

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{ Public types in this unit

  Type                             | Purpose
  ---------------------------------+-------------------------------------------------------------------------
  EDataRow                         | Reports invalid data-row access or operations.
  EDataTable                       | Reports invalid table configuration or operations.
  ESearchExpressionParserException | Reports an invalid search expression.
  IDataSource                      | Supplies filtered and sorted pages of named Variant rows.
  PDataTable                       | Points to a data table.
  TApplyFilterDelegate<T>          | Tests one item against a column filter.
  TApplySortDelegate<T>            | Compares two items using a column sort definition.
  TColumnFilter                    | Holds a column's search values and sorting configuration.
  TCustomDataSource<T>             | Provides generic paging, filtering, sorting and snapshot caching.
  TDataColumn                      | Describes a named column and whether it identifies a row.
  TDataColumns                     | Holds an ordered array of column definitions.
  TDataFilter                      | Selects an allowed text, numeric or sorting operation.
  TDataFilters                     | Combines allowed column filter operations.
  TDataRow                         | Implements named-field Variant rows and dynamic field access.
  TDataRowData                     | Provides named-field access to a row's Variant data.
  TDataSourceTool<T>               | Builds row comparers and predicates from column filters.
  TDataTable                       | Pairs column metadata with an array of Variant rows.
  TExpressionKind                  | Identifies text, numeric or range search expressions.
  TExpressionRecord                | Stores a parsed text, numeric or range search expression.
  TFilterCriteria                  | Holds an array of column filters.
  TFilterOperand                   | Combines filters using AND or OR.
  TNumericComparisionKind          | Selects the numeric comparison in a parsed expression.
  TPageFetchParams                 | Describes a page request, its filters and filter-combination rule.
  TPageFetchResult                 | Reports the returned page index, page count and total row count.
  TSearchExpressionService         | Parses text, numeric and range search expressions.
  TSetKind                         | Selects inclusive or exclusive numeric range matching.
  TSortDirection                   | Selects unsorted, ascending or descending order.
  TSortNullPolicy                  | Controls placement of null values during sorting.
  TTextMatchKind                   | Selects exact, prefix, suffix or substring text matching.
}

{$IFDEF FPC}
  {$MODE Delphi}
  {$MODESWITCH NESTEDPROCVARS}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils, Generics.Collections, Generics.Defaults, Variants, LazUTF8,
  Math, TypInfo, SyncObjs,
  {$ELSE}
  System.Classes, System.SysUtils, System.Generics.Collections, System.Generics.Defaults,
  System.Variants, System.Math, System.TypInfo, System.SyncObjs, System.TimeSpan,
  {$ENDIF}
  UMemory, UCommon, UCommon.Collections, UCache;

type
  TSortDirection = (sdNone, sdAscending, sdDescending);
  TFilterOperand = (foAnd, foOr);
  TSortNullPolicy = (snpNone, snpNullFirst, snpNullLast);
  TDataFilter = (vgfMatchTextExact, vgfMatchTextBeginning, vgfMatchTextEnd,
     vgfMatchTextAnywhere, vgfNumericEQ, vgfNumericLT, vgfNumericLTE, vgfNumericGT,
     vgfNumericGTE, vgfNumericBetweenInclusive, vgfNumericBetweenExclusive, vgfSortable);
   TDataFilters = set of TDataFilter;

const
   TEXT_FILTER = [vgfMatchTextExact, vgfMatchTextBeginning, vgfMatchTextEnd, vgfMatchTextAnywhere];
   NUMERIC_FILTER = [vgfNumericEQ, vgfNumericLT, vgfNumericLTE, vgfNumericGT, vgfNumericGTE, vgfNumericBetweenInclusive, vgfNumericBetweenExclusive];
   SORTABLE_TEXT_FILTER = TEXT_FILTER + [vgfSortable];
   SORTABLE_NUMERIC_FILTER = NUMERIC_FILTER + [vgfSortable];
   SORTABLE_FILTER = [vgfSortable];
   NON_SORTABLE_FILTER = [vgfMatchTextExact, vgfMatchTextBeginning, vgfMatchTextEnd,
     vgfMatchTextAnywhere, vgfNumericEQ, vgfNumericLT, vgfNumericLTE, vgfNumericGT,
     vgfNumericGTE, vgfNumericBetweenInclusive, vgfNumericBetweenExclusive];

 type
  { TDataColumn }

  TDataColumn = record
    private
      FName : String;
      FKey : boolean;
    public
      property Name : String read FName;
      property IsKey : boolean read FKey;
      class function From(const AName: String; IsKey : boolean = False) : TDataColumn; overload; static;
  end;

  TDataColumns = TArray<TDataColumn>;

  { TDataRow }

  TDataRow = class(TInvokeableVariantType)
  private
    class constructor Create;
    class destructor Destroy;
  protected type
    TColumnMapToIndex = TDictionary<String, Integer>;
    TColumnsDictionary = TObjectDictionary<PtrInt, TColumnMapToIndex>;
  protected class var
    FColumns: TColumnsDictionary;
    FColumnsLock: TCriticalSection;
  protected
    {$IFNDEF FPC}
    function FixupIdent(const AText : String) : String; override; {$ENDIF}
    class function MapColumns(AClassID : PtrInt; const AColumns: TDataColumns): TColumnMapToIndex;
  public
    function GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean; override;
    function SetProperty({$IFDEF FPC}var{$ELSE}const{$ENDIF} V: TVarData; const Name: string; const Value: TVarData): Boolean; override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
    class function New(AClassID : PtrInt; const AColumns: TDataColumns): Variant;
  end;

  EDataRow = class(Exception);

  TDataRowData = packed record
  public
    vtype: tvartype;
  private
    vfiller1 : word; // Variant filler
    vfiller2: int32; // Variant filler
    vcolumnmap: TDataRow.TColumnMapToIndex;
    vvalues: TArray<Variant>;
    function GetData(const ABinding : String): Variant;
    procedure SetData(const ABinding : String; const AValue: Variant);
  public
    function HasData(const ABinding : String): boolean;
    property Data[const ABinding : String] : Variant read GetData write SetData; default;
  end;

  { TColumnFilter }

  TColumnFilter = record
    ColumnName: String;
    Sort: TSortDirection;
    Filter: TDataFilter;
    Values: array of Variant;
    SortSequence : Integer;
  end;

  { TFilterCriteria }

  TFilterCriteria = TArray<TColumnFilter>;

  { TApplyFilterDelegate }

  TApplyFilterDelegate<T> = function ({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem : T; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} AColumnFilter: TColumnFilter) : boolean of object;

  { TApplySortDelegate }

  TApplySortDelegate<T> = function ({$IFDEF FPC}constref{$ELSE}const{$ENDIF} Left, Right : T; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} AColumnFilter: TColumnFilter) : Integer of object;

  { TDataTable }

  TDataTable = record
    Columns: TDataColumns;
    Rows : TArray<Variant>;
  end;
  PDataTable = ^TDataTable;
  EDataTable = class(Exception);

  { TColumnFilterPredicate -- should be implementation only }

  TColumnFilterPredicate<T> = class (TInterfacedObject, IPredicate<T>)
     private
       FFilter : TColumnFilter;
       FDelegate : TApplyFilterDelegate<T>;
     public
       constructor Create(const AFilter : TColumnFilter; const ADelegate : TApplyFilterDelegate<T>); overload;
       function Evaluate ({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: T) : boolean;
   end;

  {TColumnFieldComparer }

  TColumnFieldComparer<T> = class (TInterfacedObject, IComparer<T>)
    private
      FFilter : TColumnFilter;
      FDelegate: TApplySortDelegate<T>;
    public
      constructor Create(const AFilter : TColumnFilter; const ADelegate: TApplySortDelegate<T>);
      function Compare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: T): Integer; virtual;
  end;

  { TPageFetchParams }

  TPageFetchParams = record
    PageIndex: Integer;
    PageSize: Integer;
    Filter: TFilterCriteria;
    Operand : TFilterOperand;
    constructor Create(AIndex: Integer; ASize: Integer; AFilter: TFilterCriteria; AOperand: TFilterOperand);
    function GetSortFilters : TArray<TColumnFilter>;
    function GetSearchFilters : TArray<TColumnFilter>;
  end;

  { TPageFetchResult }

  TPageFetchResult = record
    PageIndex: Integer;
    PageCount: Integer;
    TotalDataCount: Integer;
  end;

  { IDataSource }

  IDataSource = interface
    function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
  end;

  { TCustomDataSource }

  TCustomDataSource<T> = class(TComponent, IDataSource)
    private type
      TSnapshotItems = TArray<T>;
      TSnapshotCache = TActionCache<Integer, TSnapshotItems>;
    private
      FLock : TCriticalSection;
      FClassID : PtrInt;
      FSnapshotCache : TSnapshotCache;
      FSnapshotGeneration : Int64;
      FCachedGeneration : Int64;
      FSnapshotCacheEnabled : LongInt;
      FSnapshotParams : TPageFetchParams;
      FHasSnapshotParams : Boolean;
      procedure EnsureSnapshotCache;
      function GetSnapshotCacheEnabled : Boolean;
      procedure SetSnapshotCacheEnabled(AEnabled : Boolean);
      function FetchSnapshot(const AKey : Integer) : TSnapshotItems;
      function EstimateSnapshot(const AItems : TSnapshotItems) : SizeInt;
      function SnapshotQueryMatches({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams : TPageFetchParams) : Boolean;
      procedure CopySnapshotQuery({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams : TPageFetchParams);
    protected
      function GetNullPolicy(const AFilter : TColumnFilter) : TSortNullPolicy; virtual;
      function GetItemDisposePolicy : TDisposePolicy; virtual;
      function GetColumns : TDataColumns; virtual; abstract;
      function ApplyColumnSort({$IFDEF FPC}constref{$ELSE}const{$ENDIF} Left, Right : T; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} AFilter: TColumnFilter) : Integer; virtual;
      function ApplyColumnFilter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: T; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} AFilter: TColumnFilter) : boolean; virtual;
      function GetItemField({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: T; const ABindingName : String) : Variant; virtual; abstract;
      procedure DehydrateItem({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: T; var ADataRow: Variant);
      function FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult; virtual;
      procedure OnBeforeFetchAll({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams); virtual;
      procedure FetchAll(const AContainer : TList<T>); virtual; abstract;
      procedure OnAfterFetchAll({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams); virtual;
      procedure EnterLock; inline;
      procedure ReleaseLock; inline;
    public
      constructor Create(AOwner: TComponent); override; {$IFDEF FPC}overload;{$ENDIF}
      destructor Destroy; override;
      // Opt in for immutable value snapshots. The cache retains one raw dataset
      // and one filtered/sorted view; page navigation only converts visible rows.
      // Object items remain borrowed and must outlive reuse of the cached
      // snapshot and all active fetches. Invalidation does not join a worker or
      // transfer ownership; it only makes the next fetch rebuild backing values.
      property SnapshotCacheEnabled : Boolean read GetSnapshotCacheEnabled write SetSnapshotCacheEnabled;
      // Logical invalidation never waits for a fetch/cache lock. An in-flight
      // worker may finish its old snapshot; the next fetch rebuilds the dataset.
      procedure InvalidateSnapshot;
  end;

  { Expression API }

  TExpressionKind = (ekUnknown, ekText, ekNum, ekSet);
  TTextMatchKind = (tmkUnknown, tmkMatchTextExact, tmkMatchTextBeginning, tmkMatchTextEnd, tmkMatchTextAnywhere);
  TNumericComparisionKind = (nckUnknown, nckNumericEQ, nckNumericLT, nckNumericLTE, nckNumericGT, nckNumericGTE);
  TSetKind = (skUnknown, skNumericBetweenInclusive, skNumericBetweenExclusive);

  TExpressionRecord = record
    Values: array of utf8string;
    HasDecimals: boolean; // for ekSet and ekNum only
  case Kind: TExpressionKind of
    ekUnknown: ();
    ekText: (TextMatchKind: TTextMatchKind);
    ekNum: (NumericComparisionKind: TNumericComparisionKind);
    ekSet: (SetKind: TSetKind);
  end;

  ESearchExpressionParserException = class(Exception);

  { TSearchExpressionService }

  TSearchExpressionService = class
  public
    class procedure Parse(const AExpression: utf8string; const AExpressionKind: TExpressionKind; out AExpressionRecord: TExpressionRecord); overload;
    class function Parse(const AExpression: utf8string): TExpressionRecord; overload;
  end;

  { TDataSourceTool }

  TDataSourceTool<T> = class
    protected type
      __IPredicate_T = IPredicate<T>;
      __TList_IPredicate_T = TList<__IPredicate_T>;
    public
     class function ToArrayWorkAround( ParaList : __TList_IPredicate_T ) : TArray<IPredicate<T>>; // Skybuck: work around for buggy TList.ToArray
     class function ConstructRowComparer(const AFilters : TArray<TColumnFilter>; const ADelegate : TApplySortDelegate<T>) : IComparer<T>;
     class function ConstructRowPredicate(const AFilters : TArray<TColumnFilter>; const ADelegate : TApplyFilterDelegate<T>; const AOperand : TFilterOperand) : IPredicate<T>;
  end;

  { Resources }

 resourcestring
  sAColumnsCantBeNil = 'AColumns can''t be nil!';
  sTooManyValues = 'Too many values';
  sInvalidUTF8String = 'Invalid UTF8 string';
  sBadNumericExpression = 'Bad numeric expression';
  sUnexpectedNumberFormat = 'Unexpected number format';
  sBadSyntaxForEscapeCharacter = 'Bad syntax for escape character "\"';
  sUnexpectedCharInExpression = 'Unexpected char in expression';
  sInvaildExpression_CharDetectedAfterClosingBracket = 'Invaild expression (char detected after closing bracket)';
  sUnexpectedTokenFound = 'Unexpected token found : "%s"';
  sUnexpectedStringLiteralInExpression = 'Unexpected string literal in expression';
  sBadlyClosedBetweenExpression = 'Badly closed "between" expression';
  sMissingNumberInExpression = 'Missing number in expression';
  sUnexpectedOccurrenceOf_Found = 'Unexpected occurrence of "%s" found';
  sBadBetweenExpression_UnexpectedToken = 'Bad "between" expression. Unexpected "%s"';
  sExpressionError_NoValue = 'Expression error (no value)';

implementation

uses {$IFDEF FPC}DateUtils{$ELSE}System.DateUtils{$ENDIF};

{ Variables }

var
  DataRowType: TDataRow = nil;

{ TDataColumn }

class function TDataColumn.From(const AName: String; IsKey : boolean = False) : TDataColumn;
begin
  Result.FName := AName;
  Result.FKey:= IsKey;
end;

{ TDataRow }

class constructor TDataRow.Create;
begin
  FColumnsLock := TCriticalSection.Create;
  FColumns := TColumnsDictionary.Create([doOwnsValues]);
end;

class destructor TDataRow.Destroy;
begin
  FColumns.Free;
  FColumnsLock.Free;
end;

class function TDataRow.MapColumns(AClassID : PtrInt; const AColumns: TDataColumns): TColumnMapToIndex;
var
  i: Integer;
  disposed: TDisposables;
begin
  Result := disposed.AddObject(TColumnMapToIndex.Create) as TColumnMapToIndex;
  for i := Low(AColumns) to High(AColumns) do
    Result.Add(AColumns[i].Name, i);
  // A for-loop's final counter value is undefined in Delphi.
  Result.Add('__KEY', Length(AColumns));
  FColumns.Add(AClassID, Result);
  disposed.RemoveObject(Result);
end;

{$IFNDEF FPC}
function TDataRow.FixupIdent(const AText : String) : String;
begin
  // Delphi's default uppercases identifiers; column maps retain their names.
  Result := AText;
end;
{$ENDIF}

function TDataRow.GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean;
var
  LRow: TDataRowData absolute V;
begin
  Variant(Dest) := LRow[Name];
  Result := true;
end;

function TDataRow.SetProperty({$IFDEF FPC}var{$ELSE}const{$ENDIF} V: TVarData; const Name: string; const Value: TVarData): Boolean;
var
  LRow: TDataRowData absolute V;
begin
  LRow[Name] := Variant(Value);
  Result := true;
end;

procedure TDataRow.Clear(var V: TVarData);
begin
  Finalize(TDataRowData(V));
  FillChar(V, SizeOf(V), #0);
end;

procedure TDataRow.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
var
  LDestRow: TDataRowData absolute Dest;
  LSourceRow: TDataRowData absolute Source;
begin
  if Indirect then
    SimplisticCopy(Dest,Source,true)
  else
  begin
    VarClear(variant(Dest));
    FillChar(LDestRow, SizeOf(LDestRow), #0);
    LDestRow.vtype := LSourceRow.vtype;
    LDestRow.vcolumnmap := LSourceRow.vcolumnmap;
    LDestRow.vvalues := system.copy(TDataRowData(LSourceRow).vvalues);
  end;
end;

function TDataRow.DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean;
var
  LRow: TDataRowData absolute V;
begin
  // FPC 3.2.2 emits an empty default-member name for Row._(index).
  Result := ((Name = '_') {$IFDEF FPC}or (Name = ''){$ENDIF}) and (Length(Arguments)=1);
  if Result then
    Variant(Dest) := LRow.vvalues[{$IFNDEF FPC}Integer({$ENDIF}Variant(Arguments[0]){$IFNDEF FPC}){$ENDIF}];
end;

class function TDataRow.New(AClassID:PtrInt; const AColumns: TDataColumns): Variant;

var
  LColumnMap: TColumnMapToIndex;
  AKey : String;
  i : Integer;
begin
  if not Assigned(AColumns) then
    raise EDataRow.Create(sAColumnsCantBeNil);

  VarClear(Result);
  FillChar(Result, SizeOf(Result), #0);
  TDataRowData(Result).vtype:=DataRowType.VarType;
  // Account and operation grids construct their first rows on different workers.
  FColumnsLock.Acquire;
  try
    if not FColumns.TryGetValue(AClassID, LColumnMap) then
      LColumnMap := MapColumns(AClassID, AColumns)
    else begin
      // Ensure all instances have consistent columns for AClassID
      if Length(AColumns) <> (LColumnMap.Count - 1) then
        raise EDataRow.Create('Internal Error: Inconsistent column counts for given AClassID. Ensure AClassID values are unique for each class of constructed TDataRow');
      for i := Low(AColumns) to High(AColumns) do begin
        if NOT LColumnMap.ContainsKey(AColumns[i].Name) then
          raise EDataRow.Create(Format('Internal Error: Inconsistent columns for given AClassID. Unknown column "%s".', [AColumns[i].Name]));
      end;
    end;
  finally
    FColumnsLock.Release;
  end;
  TDataRowData(Result).vcolumnmap:=LColumnMap;
  SetLength(TDataRowData(Result).vvalues, LColumnMap.Count);
end;

{ TDataRowData }

function TDataRowData.HasData(const ABinding : String): boolean;
begin
  Result := Self.vcolumnmap.ContainsKey(ABinding)
end;

function TDataRowData.GetData(const ABinding: String): Variant;
var LIndex : Integer;
begin
  if NOT Self.vcolumnmap.TryGetValue(ABinding, LIndex) then
    raise EDataRow.Create(Format('TDataRowData does not contain column "%s"', [ABinding]));
  Result := Self.vvalues[LIndex];
end;

procedure TDataRowData.SetData(const ABinding: String; const AValue: Variant);
var LIndex : Integer;
begin
  if NOT Self.vcolumnmap.TryGetValue(ABinding, LIndex) then
    raise EDataRow.Create(Format('TDataRowData does not contain column "%s"', [ABinding]));
  Self.vvalues[LIndex] := AValue;
end;

{ TCustomDataSource }

constructor TCustomDataSource<T>.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FClassID := PtrInt(ClassInfo); // Use pointer to ClassInfo as unique identifier for DataSource
  FCachedGeneration := -1;
end;

procedure TCustomDataSource<T>.EnsureSnapshotCache;
begin
  if Assigned(FSnapshotCache) then Exit;
  FSnapshotCache := TSnapshotCache.Create(nil,
    TCacheValueFetcher<Integer, TSnapshotItems>.FromObjectFunction(FetchSnapshot),
    TCacheSizeEstimator<TSnapshotItems>.FromObjectFunction(EstimateSnapshot),
    idpNone, TCacheReapPolicy.crpOldest,
    TCacheExpirationPolicy.epNone,
    TCacheNullValuePolicy.nvpCacheNormally, 2, TTimeSpan.MaxValue);
end;

destructor TCustomDataSource<T>.Destroy;
var
  i : integer;
  policy : TDisposePolicy;
begin
  FreeAndNil(FSnapshotCache);
  inherited;
  FreeAndNil(FLock);
end;

function TCustomDataSource<T>.GetNullPolicy(const AFilter : TColumnFilter) : TSortNullPolicy;
begin
  // Rule is as DBMS. When ascending NULLS show last (are greater), when descending, NULLS show first
  case AFilter.Sort of
    sdNone: Result := snpNone;
    sdAscending: Result := snpNullLast;
    sdDescending: Result := snpNullFirst;
  end;
end;

function TCustomDataSource<T>.GetSnapshotCacheEnabled : Boolean;
begin
  {$IFDEF FPC}
  Result := InterlockedCompareExchange(FSnapshotCacheEnabled, 0, 0) <> 0;
  {$ELSE}
  Result := TInterlocked.CompareExchange(FSnapshotCacheEnabled, 0, 0) <> 0;
  {$ENDIF}
end;

procedure TCustomDataSource<T>.SetSnapshotCacheEnabled(AEnabled : Boolean);
begin
  {$IFDEF FPC}
  if InterlockedExchange(FSnapshotCacheEnabled, Ord(AEnabled)) <> Ord(AEnabled) then
  {$ELSE}
  if TInterlocked.Exchange(FSnapshotCacheEnabled, Ord(AEnabled)) <> Ord(AEnabled) then
  {$ENDIF}
    InvalidateSnapshot;
end;

procedure TCustomDataSource<T>.InvalidateSnapshot;
begin
  {$IFDEF FPC}
  InterlockedIncrement64(FSnapshotGeneration);
  {$ELSE}
  TInterlocked.Increment(FSnapshotGeneration);
  {$ENDIF}
end;

function TCustomDataSource<T>.EstimateSnapshot(const AItems : TSnapshotItems) : SizeInt;
begin
  // Capacity counts snapshots, not individual rows: at most raw + current view.
  Result := 1;
end;

function TCustomDataSource<T>.SnapshotQueryMatches({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams : TPageFetchParams) : Boolean;
var
  i, j : Integer;
  LLeft, LRight : TColumnFilter;
begin
  Result := False;
  if not FHasSnapshotParams then Exit;
  if (FSnapshotParams.Operand <> AParams.Operand) or
    (Length(FSnapshotParams.Filter) <> Length(AParams.Filter)) then Exit;
  for i := 0 to High(AParams.Filter) do begin
    LLeft := FSnapshotParams.Filter[i];
    LRight := AParams.Filter[i];
    if (LLeft.ColumnName <> LRight.ColumnName) or (LLeft.Sort <> LRight.Sort) or
      (LLeft.Filter <> LRight.Filter) or (LLeft.SortSequence <> LRight.SortSequence) or
      (Length(LLeft.Values) <> Length(LRight.Values)) then Exit;
    for j := 0 to High(LLeft.Values) do begin
      if VarType(LLeft.Values[j]) <> VarType(LRight.Values[j]) then Exit;
      // Search criteria use scalar values. Unusual array/interface criteria are
      // rebuilt instead of accepting an uncertain equality comparison.
      if VarIsArray(LLeft.Values[j]) or VarIsByRef(LLeft.Values[j]) then Exit;
      if not VarSameValue(LLeft.Values[j], LRight.Values[j]) then Exit;
    end;
  end;
  Result := True;
end;

procedure TCustomDataSource<T>.CopySnapshotQuery({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams : TPageFetchParams);
var
  i, j : Integer;
begin
  FSnapshotParams := AParams;
  FSnapshotParams.Filter := nil;
  SetLength(FSnapshotParams.Filter, Length(AParams.Filter));
  for i := 0 to High(AParams.Filter) do begin
    FSnapshotParams.Filter[i] := AParams.Filter[i];
    FSnapshotParams.Filter[i].Values := nil;
    SetLength(FSnapshotParams.Filter[i].Values, Length(AParams.Filter[i].Values));
    for j := 0 to High(AParams.Filter[i].Values) do
      FSnapshotParams.Filter[i].Values[j] := AParams.Filter[i].Values[j];
  end;
  FHasSnapshotParams := True;
end;

function TCustomDataSource<T>.FetchSnapshot(const AKey : Integer) : TSnapshotItems;
var
  LData : TList<T>;
  LRaw : TSnapshotItems;
  LFilters : TArray<TColumnFilter>;
  LDisposables : TDisposables;
begin
  LData := LDisposables.AddObject(TList<T>.Create) as TList<T>;
  if AKey = 0 then begin
    FetchAll(LData);
  end else begin
    LRaw := FSnapshotCache[0];
    LFilters := FSnapshotParams.GetSearchFilters;
    if (Length(LFilters) = 0) and (Length(FSnapshotParams.GetSortFilters) = 0) then
      Exit(LRaw);
    LData.AddRange(LRaw);
    if Length(LFilters) > 0 then
      TListTool<T>.FilterBy(LData,
        TDataSourceTool<T>.ConstructRowPredicate(LFilters, ApplyColumnFilter, FSnapshotParams.Operand));
    LFilters := FSnapshotParams.GetSortFilters;
    if Length(LFilters) > 0 then
      LData.Sort(TDataSourceTool<T>.ConstructRowComparer(LFilters, ApplyColumnSort));
  end;
  Result := LData.ToArray;
end;

function TCustomDataSource<T>.GetItemDisposePolicy : TDisposePolicy;
begin
  Result := idpNone;
end;

function TCustomDataSource<T>.ApplyColumnSort({$IFDEF FPC}constref{$ELSE}const{$ENDIF} Left, Right : T; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} AFilter: TColumnFilter) : Integer;
var
  leftField, rightField : Variant;
  leftType, rightType : Integer;
  nullPolicy : TSortNullPolicy;
begin
  leftField := GetItemField(Left, AFilter.ColumnName);
  rightField := GetItemField(Right, AFilter.ColumnName);
  leftType := VarType(leftField);
  rightType := VarType(rightField);
  nullPolicy := GetNullPolicy(AFilter);

  // Handle null-based corner cases
  if (leftType = varNull) AND (rightType = varNull) then begin
    Result := 0;
    exit;
  end else if (leftType = varNull) AND (rightType <> varNull) then begin
    Result := IIF(nullPolicy = snpNullFirst , -1, 1);
    exit;
  end else if (leftType <> varNull) AND (rightType = varNull) then begin
    Result := IIF(nullPolicy = snpNullFirst, 1, -1);
    exit;
  end;

  // Compare left/right values
  Result := TVariantTool.CompareVariant(leftField, rightField);

  // Invert result for descending
  if AFilter.Sort = sdDescending then
    Result := Result * -1;
end;

function TCustomDataSource<T>.ApplyColumnFilter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: T; {$IFDEF FPC}constref{$ELSE}const{$ENDIF} AFilter: TColumnFilter) : boolean;
var
  value : Variant;
begin
  if Length(AFilter.Values) = 0 then
    raise EArgumentException.Create('AFilter.Values does not contain any parameters');

  value := GetItemField(AItem, AFilter.ColumnName);
  if AFilter.Filter in TEXT_FILTER then begin
    case AFilter.Filter of
      vgfMatchTextExact: Result := TVariantTool.MatchTextExact(value, AFilter.Values[0]);
      vgfMatchTextBeginning: Result := TVariantTool.MatchTextBeginning(value, AFilter.Values[0]);
      vgfMatchTextEnd: Result := TVariantTool.MatchTextEnd(value, AFilter.Values[0]);
      vgfMatchTextAnywhere: Result := TVariantTool.MatchTextAnywhere(value, AFilter.Values[0]);
    end;
  end else if AFilter.Filter in NUMERIC_FILTER then begin
    case AFilter.Filter of
      vgfNumericEQ: Result := TVariantTool.NumericEQ(value, AFilter.Values[0]);
      vgfNumericLT: Result := TVariantTool.NumericLT(value, AFilter.Values[0]);
      vgfNumericLTE: Result := TVariantTool.NumericLTE(value, AFilter.Values[0]);
      vgfNumericGT: Result := TVariantTool.NumericGT(value, AFilter.Values[0]);
      vgfNumericGTE: Result := TVariantTool.NumericGTE(value, AFilter.Values[0]);
      vgfNumericBetweenInclusive: begin
        if Length(AFilter.Values) < 2 then
          raise EArgumentException.Create('AFilter.Values does not contain at least 2 parameters');
        Result := TVariantTool.NumericBetweenInclusive(value, AFilter.Values[0], AFilter.Values[1]);
      end;
      vgfNumericBetweenExclusive: begin
        if Length(AFilter.Values) < 2 then
          raise EArgumentException.Create('AFilter.Values does not contain at least 2 parameters');
        Result := TVariantTool.NumericBetweenExclusive(value, AFilter.Values[0], AFilter.Values[1]);
      end;
    end;
  end else Result := false;
end;

function TCustomDataSource<T>.FetchPage({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
var
  i, j : SizeInt;
  data : TList<T>;
  GC : TDisposables;
  pageStart, pageEnd : SizeInt;
  LItems : TSnapshotItems;
  LCount : SizeInt;
  LGeneration : Int64;
  LUseSnapshot : Boolean;
  filters : TArray<TColumnFilter>;
begin
  OnBeforeFetchAll(AParams);
  FLock.Acquire;
  try
     LUseSnapshot := GetSnapshotCacheEnabled;
     if LUseSnapshot then begin
       EnsureSnapshotCache;
       {$IFDEF FPC}
       LGeneration := InterlockedCompareExchange64(FSnapshotGeneration, 0, 0);
       {$ELSE}
       LGeneration := TInterlocked.CompareExchange(FSnapshotGeneration, 0, 0);
       {$ENDIF}
       if LGeneration <> FCachedGeneration then begin
         FSnapshotCache.Flush;
         FHasSnapshotParams := False;
         FCachedGeneration := LGeneration;
       end;
       if not SnapshotQueryMatches(AParams) then begin
         FSnapshotCache.Remove(1);
         CopySnapshotQuery(AParams);
       end;
       LItems := FSnapshotCache[1];
       LCount := Length(LItems);
     end else begin
       if Assigned(FSnapshotCache) then FSnapshotCache.Flush;
       FHasSnapshotParams := False;
       data := GC.AddObject(TList<T>.Create) as TList<T>;
       FetchAll(data);
       filters := AParams.GetSearchFilters;
       if Length(filters) > 0 then
         TListTool<T>.FilterBy(data,
           TDataSourceTool<T>.ConstructRowPredicate(filters, ApplyColumnFilter, AParams.Operand));
       filters := AParams.GetSortFilters;
       if Length(filters) > 0 then
         data.Sort(TDataSourceTool<T>.ConstructRowComparer(filters, ApplyColumnSort));
       LCount := data.Count;
     end;

     Result.TotalDataCount := LCount;
     if LCount = 0 then begin
       Result.PageCount := 0;
       Result.PageIndex := -1;
       pageStart := 0;
       pageEnd := -1;
     end else if AParams.PageSize > 0 then begin
       Result.PageCount := ((LCount - 1) div AParams.PageSize) + 1;
       Result.PageIndex := ClipValue(AParams.PageIndex, 0, Result.PageCount - 1);
       pageStart := Result.PageIndex * AParams.PageSize;
       pageEnd := ClipValue(pageStart + (AParams.PageSize - 1), pageStart, LCount - 1);
     end else begin
       Result.PageCount := 1;
       Result.PageIndex := 0;
       pageStart := 0;
       pageEnd := LCount - 1;
     end;
     ADataTable.Columns := GetColumns;
     // A reused table must also lose its previous rows when the result is empty.
     SetLength(ADataTable.Rows, pageEnd - pageStart + 1);
     if pageEnd >= pageStart then begin
       j := 0;
       for i := pageStart to pageEnd do begin
         ADataTable.Rows[j] := TDataRow.New(FClassID, ADataTable.Columns);
         if LUseSnapshot then
           DehydrateItem(LItems[i], ADataTable.Rows[j])
         else
           DehydrateItem(data[i], ADataTable.Rows[j]);
         inc(j);
       end;
     end;
  finally
    FLock.Release;
  end;
  OnAfterFetchAll(AParams);
end;

procedure TCustomDataSource<T>.DehydrateItem({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AItem: T; var ADataRow: Variant);
var
  i : integer;
  LCols : TArray<TDataColumn>;
  LData : TDataRowData;
  LField : Variant;
begin
  LCols := GetColumns;
  LData := TDataRowData(ADataRow);
  for i := Low(LCols) to High(LCols) do begin
    LField := GetItemField(AItem, LCols[i].Name);
    LData[LCols[i].Name] := LField;
    if LCols[i].IsKey then
      LData['__KEY'] := LField;
  end;
end;

procedure TCustomDataSource<T>.OnBeforeFetchAll({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams);
begin
end;

procedure TCustomDataSource<T>.OnAfterFetchAll({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AParams: TPageFetchParams);
begin
end;

procedure TCustomDataSource<T>.EnterLock;
begin
  FLock.Acquire;
end;

procedure TCustomDataSource<T>.ReleaseLock; {$IFDEF FPC}inline;{$ENDIF}
begin
  FLock.Release;
end;

{ TColumnFilterPredicate }

constructor TColumnFilterPredicate<T>.Create(const AFilter : TColumnFilter; const ADelegate : TApplyFilterDelegate<T>);
begin
  FFilter := AFilter;
  FDelegate := ADelegate;
end;

function TColumnFilterPredicate<T>.Evaluate ({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AValue: T) : boolean;
begin
  Result := FDelegate(AValue, FFilter);
end;

{ TColumnFieldComparer }

constructor TColumnFieldComparer<T>.Create(const AFilter : TColumnFilter; const ADelegate: TApplySortDelegate<T>);
begin
  FFilter := AFilter;
  FDelegate := ADelegate;
end;

function TColumnFieldComparer<T>.Compare({$IFDEF FPC}constref{$ELSE}const{$ENDIF} ALeft, ARight: T): Integer;
begin
  Result := FDelegate(ALeft, ARight, FFilter);
end;

{ TDataSourceTool }

class function TDataSourceTool<T>.ConstructRowComparer(const AFilters : TArray<TColumnFilter>; const ADelegate : TApplySortDelegate<T>) : IComparer<T>;
type
  __IComparer_T = IComparer<T>;
var
  i : integer;
  comparers : TList<__IComparer_T>;
  filter : TColumnFilter;
  GC : TDisposables;
begin
  comparers := GC.AddObject(  TList<__IComparer_T>.Create ) as TList<__IComparer_T>;
  for i := Low(AFilters) to High(AFilters) do begin
    filter := AFilters[i];
    if filter.Sort <> sdNone then
      comparers.Add( TColumnFieldComparer<T>.Create( filter, ADelegate ) );
  end;

  case comparers.Count of
    0: Result := nil;
    1: Result := comparers[0];
    else Result := TComparerTool<T>.Many(comparers);
  end;
end;

// Skybuck: ToArrayWorkAround implementation until TList.ToArray is fixed by FPC compiler and/or generics team ?
class function TDataSourceTool<T>.ToArrayWorkAround( ParaList : __TList_IPredicate_T ) : TArray<IPredicate<T>>;
begin
  result := ParaList.ToArray;
end;

class function TDataSourceTool<T>.ConstructRowPredicate(const AFilters : TArray<TColumnFilter>; const ADelegate : TApplyFilterDelegate<T>; const AOperand : TFilterOperand) : IPredicate<T>;
type
  __TColumnFilterPredicate_T = TColumnFilterPredicate<T>;
  __TPredicateTool_T = TPredicateTool<T>;
var
  i : integer;
  filters : __TList_IPredicate_T;
  GC : TDisposables;
begin
  filters := GC.AddObject( __TList_IPredicate_T.Create ) as __TList_IPredicate_T;
  for i := Low(AFilters) to High(AFilters) do begin
    if AFilters[i].Filter <> vgfSortable then begin
      filters.Add( __TColumnFilterPredicate_T.Create(AFilters[i], ADelegate));
    end;
  end;

  // Skybuck: Free Pascal Compiler Bug/Internal Error 2015052501 caused by buggy TList.ToArray
  case filters.Count of
    0: Result := nil;
    1: Result := filters[0];
    else case AOperand of
//      foAnd: Result := __TPredicateTool_T.AndMany(filters.ToArray); // Skybuck: TList.ToArray causes FPC compiler to crash here.
//      foOr: Result := __TPredicateTool_T.OrMany(filters.ToArray);

      foAnd: Result := __TPredicateTool_T.AndMany(ToArrayWorkAround(filters));
      foOr: Result := __TPredicateTool_T.OrMany(ToArrayWorkAround(filters));
    end;
  end;
end;

{ TPageFetchParams }

constructor TPageFetchParams.Create(AIndex: Integer; ASize: Integer; AFilter: TFilterCriteria; AOperand : TFilterOperand);
begin
  PageIndex:= AIndex;
  PageSize:=ASize;
  Filter:=AFilter;
  Operand := AOperand;
end;

function TPageFetchParams.GetSortFilters : TArray<TColumnFilter>;
var
  sortFilters : TList<TColumnFilter>;
  GC : TDisposables;
  {$IFNDEF FPC}
  LPredicate : TNestedPredicateFunc<TColumnFilter>;
  {$ENDIF}

  function IsSortFilter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AColFilter : TColumnFilter) : boolean;
  begin
    Result := AColFilter.Sort <> sdNone;
  end;

begin
  sortFilters := GC.AddObject( TList<TColumnFilter>.Create ) as TList<TColumnFilter>;
  sortFilters.AddRange( Filter);
  {$IFDEF FPC}
  TListTool<TColumnFilter>.FilterBy( sortFilters, TPredicateTool<TColumnFilter>.FromFunc( IsSortFilter ));
  {$ELSE}
  LPredicate := function(const AColFilter : TColumnFilter) : Boolean
    begin
      Result := AColFilter.Sort <> sdNone;
    end;
  TListTool<TColumnFilter>.FilterBy(sortFilters, TPredicateTool<TColumnFilter>.FromFunc(LPredicate));
  {$ENDIF}
  SetLength(Result, 0);
  if sortFilters.Count > 0 then
    Result := sortFilters.ToArray;
end;

function TPageFetchParams.GetSearchFilters : TArray<TColumnFilter>;
var
  searchFilters : TList<TColumnFilter>;
  GC : TDisposables;
  {$IFNDEF FPC}
  LPredicate : TNestedPredicateFunc<TColumnFilter>;
  {$ENDIF}

  function IsSearchFilter({$IFDEF FPC}constref{$ELSE}const{$ENDIF} AColFilter : TColumnFilter) : boolean;
  begin
    Result := AColFilter.Filter <> vgfSortable;
  end;

begin
  searchFilters := GC.AddObject( TList<TColumnFilter>.Create ) as TList<TColumnFilter>;
  searchFilters.AddRange(Filter);
  {$IFDEF FPC}
  TListTool<TColumnFilter>.FilterBy( searchFilters, TPredicateTool<TColumnFilter>.FromFunc( IsSearchFilter ));
  {$ELSE}
  LPredicate := function(const AColFilter : TColumnFilter) : Boolean
    begin
      Result := AColFilter.Filter <> vgfSortable;
    end;
  TListTool<TColumnFilter>.FilterBy(searchFilters, TPredicateTool<TColumnFilter>.FromFunc(LPredicate));
  {$ENDIF}
  SetLength(Result, 0);
  if searchFilters.Count > 0 then
    Result := searchFilters.ToArray;
end;

{ TSearchExpressionService }

class procedure TSearchExpressionService.Parse(const AExpression: utf8string; const AExpressionKind: TExpressionKind; out AExpressionRecord: TExpressionRecord);
const
  MAX_VALUES = 2;
type
  TToken = (tkNone, tkPercent, tkLess, tkGreater, tkEqual, tkLessOrEqual,
    tkGreaterOrEqual, tkOpeningParenthesis, tkClosingParenthesis,
    tkOpeningBracket, tkClosingBracket, tkText, tkNum, tkComma);

  TUTF8Char = record
    Length: byte;
    Char: array [0..3] of AnsiChar;
  end;
  {$IFNDEF FPC}
  PUTF8String = ^UTF8String;
  {$ENDIF}

const
  CONVERTABLE_TOKENS_TO_STR = [tkLess..tkClosingBracket, tkComma];
  NUM_OPERATORS = [tkLess..tkGreaterOrEqual];

  procedure GetChar(APos: PAnsiChar; out AChar: TUTF8Char);
  begin
    {$IFDEF FPC}
    AChar.Length := UTF8CharacterLength(APos);
    {$ELSE}
    // The expression was validated before scanning; retain UTF8 byte offsets.
    if Byte(APos[0])<$80 then AChar.Length := 1
    else if Byte(APos[0])<$E0 then AChar.Length := 2
    else if Byte(APos[0])<$F0 then AChar.Length := 3
    else AChar.Length := 4;
    {$ENDIF}

    if AChar.Length >= 1 then AChar.Char[0] := APos[0];
    if AChar.Length >= 2 then AChar.Char[1] := APos[1];
    if AChar.Length >= 3 then AChar.Char[2] := APos[2];
    if AChar.Length = 4 then AChar.Char[3] := APos[3];
  end;

  function TokenToStr(AToken: TToken): utf8string;
  const
    CONVERTER: array[TToken] of utf8string = (
      'NONE', '%', '<', '>', '=', '<=', '>=', '(', ')', '[', ']', 'TEXT',
      'NUMBER', ','
    );
  begin
    Result := CONVERTER[AToken];
  end;

var
  c, nc: PAnsiChar;
  i: Integer;
  LDot: boolean;
  LChar: TUTF8Char;
  LValueIdx: Integer;
  LValues: array[0..MAX_VALUES-1] of utf8string; // for now only 2 values for set "between"
  LValue: PUTF8String;
  LToken: TToken;
  LPrevToken: TToken;
  LExpression: utf8string;
  LLastPercent: boolean;
  LExpressionKind: TExpressionKind;
  LWasUnknow: boolean;
  LHasDecimals: boolean;

  procedure NextValue;
  begin
    Inc(LValueIdx);
    if LValueIdx > MAX_VALUES - 1 then
      raise ESearchExpressionParserException.Create(sTooManyValues);
    LValue := @LValues[LValueIdx];
  end;

  procedure EscapeSequence(AChar: AnsiChar);
  begin
    LToken := tkText;
    LValue^ := LValue^ + AChar;
    Inc(c);
  end;

begin
  LDot := False;
  LValueIdx := -1;
  LToken := tkNone;
  LPrevToken := tkNone;
  LLastPercent := False;
  LHasDecimals := False;
  AExpressionRecord := Default(TExpressionRecord);
  if AExpression = '' then
    Exit;

  LWasUnknow := AExpressionKind = ekUnknown;
  LExpressionKind := AExpressionKind;
  // more simple parsing loop
  if AExpressionKind in [ekSet, ekNum] then
    LExpression:=Trim(AExpression)
  else
    LExpression:=AExpression;

  c := @LExpression[1];
  {$IFDEF FPC}
  if FindInvalidUTF8Character(c, Length(LExpression)) <> -1 then
  {$ELSE}
  if not TEncoding.UTF8.IsBufferValid(Pointer(c), Length(LExpression)) then
  {$ENDIF}
    raise ESearchExpressionParserException.Create(sInvalidUTF8String);

  NextValue;
  repeat
    // simple scanner
    GetChar(c, LChar);
    if LChar.Length = 1 then
      case LChar.Char[0] of
        #0: Break;
        #1..#32:
          case LExpressionKind of
            ekSet:
              begin
                while c^ in [#1..#32] do Inc(c);
                Continue;
              end;
            ekText, ekUnknown:
              begin
                LValue^:=LValue^+LChar.Char[0];
                LToken:=tkText;
              end;
            ekNum:
              if not (LPrevToken in NUM_OPERATORS) then
                raise ESearchExpressionParserException.Create(sBadNumericExpression)
              else
              begin
                while c^ in [#1..#32] do Inc(c);
                continue;
              end;
          end;
        '0'..'9':
          begin
            repeat
              if c^ = '.' then
                if LDot then
                  raise ESearchExpressionParserException.Create(sUnexpectedNumberFormat)
                else
                  LDot:=true;
              LValue^:=LValue^+c^;
              Inc(c);
            until not (c^ in ['0'..'9', '.']);
            Dec(c);
            case LExpressionKind of
              ekUnknown, ekSet, ekNum: LToken := tkNum;
              ekText: LToken := tkText;
            end;
            if LWasUnknow and LDot then
              LHasDecimals := true;
            LDot := false;
          end;
        '%':
          begin
            if not (LExpressionKind in [ekText,ekUnknown]) then
              ESearchExpressionParserException.Create(sBadNumericExpression);

            LToken := tkPercent;
          end;
        '\':
          begin
            if not (LExpressionKind in [ekText,ekUnknown]) then
              ESearchExpressionParserException.Create(sBadNumericExpression);
            case (c+1)^ of
              '%': EscapeSequence('%');
              '\': EscapeSequence('\');
              '[': EscapeSequence('[');
              '(': EscapeSequence('(');
              ']': EscapeSequence(']');
              ')': EscapeSequence(')');
              '<': EscapeSequence('<');
              '>': EscapeSequence('>');
              '=': EscapeSequence('=');
            else
              raise ESearchExpressionParserException.Create(sBadSyntaxForEscapeCharacter);
            end
          end;
        '<':
          if (c+1)^ = '=' then
          begin
            LToken := tkLessOrEqual;
            Inc(c);
          end
          else
            LToken := tkLess;
        '>':
          if (c+1)^ = '=' then
          begin
            LToken := tkGreaterOrEqual;
            Inc(c);
          end
          else
            LToken := tkGreater;
        '=': LToken := tkEqual;
        '(': LToken := tkOpeningParenthesis;
        ')': LToken := tkClosingParenthesis;
        '[': LToken := tkOpeningBracket;
        ']': LToken := tkClosingBracket;
        ',': LToken := tkComma;
      else
        LValue^ := LValue^ + LChar.Char[0];
        LToken:=tkText;
      end
    else
    begin
      if not (LExpressionKind in [ekUnknown, ekText]) then
        raise ESearchExpressionParserException.Create(sUnexpectedCharInExpression);
      SetLength(LValue^, Length(LValue^) + LChar.Length);
      Move(LChar.Char[0], LValue^[Succ(Length(LValue^) - LChar.Length)], LChar.Length);
      LToken:=tkText;
    end;

    // parser is able to deduce expression kind (if needed)
    if LExpressionKind = ekUnknown then
    case LToken of
      tkPercent, tkText, tkComma: LExpressionKind:=ekText;
      tkOpeningBracket, tkOpeningParenthesis: LExpressionKind:=ekSet;
      tkLess..tkGreaterOrEqual, tkNum: LExpressionKind:=ekNum;
    else
      raise ESearchExpressionParserException.Create(sUnexpectedCharInExpression);
    end;

    // text mode has precedence (parsing the expressions like: 123abs)
    if (LExpressionKind = ekNum) and (AExpressionKind = ekUnknown)
      and (LToken in [tkText, tkPercent]) and (AExpressionRecord.NumericComparisionKind = nckUnknown) then
    begin
      LExpressionKind := ekText;
    end;

    // text mode is special so part of tokens are used as normal characters
    if (LExpressionKind = ekText) and (LToken in CONVERTABLE_TOKENS_TO_STR) then
      LValue^:=LValue^+TokenToStr(LToken);

    if LPrevToken in [tkClosingBracket, tkClosingParenthesis] then
      raise ESearchExpressionParserException.Create(sInvaildExpression_CharDetectedAfterClosingBracket);

    // rules
    case LToken of
      tkNum:
        if LExpressionKind = ekSet then
          if not (LPrevToken in [tkOpeningBracket, tkOpeningParenthesis, tkComma]) then
            raise ESearchExpressionParserException.CreateFmt(sUnexpectedTokenFound, [TokenToStr(LToken)]);
      tkText:
        if LExpressionKind in [ekSet, ekNum] then
          raise ESearchExpressionParserException.Create(sUnexpectedStringLiteralInExpression);
      tkClosingBracket:
        if (LExpressionKind = ekSet) then
          if (AExpressionRecord.SetKind<>skNumericBetweenInclusive) then
            raise ESearchExpressionParserException.Create(sBadlyClosedBetweenExpression)
          else if LPrevToken <> tkNum then
            raise ESearchExpressionParserException.Create(sMissingNumberInExpression);
      tkClosingParenthesis:
        if (LExpressionKind = ekSet) then
          if (AExpressionRecord.SetKind<>skNumericBetweenExclusive) then
            raise ESearchExpressionParserException.Create(sBadlyClosedBetweenExpression)
          else if LPrevToken <> tkNum then
            raise ESearchExpressionParserException.Create(sMissingNumberInExpression);
      tkComma:
        if LExpressionKind = ekSet then
          if not (LPrevToken = tkNum) then
            raise ESearchExpressionParserException.CreateFmt(sUnexpectedOccurrenceOf_Found, [','])
          else
            NextValue;
      tkPercent:
        if LExpressionKind = ekText then
        begin
          if LLastPercent then
            raise ESearchExpressionParserException.CreateFmt(sUnexpectedOccurrenceOf_Found, ['%']);
          case LPrevToken of
            tkText, tkNum: // tkNum is here because is possible to parse: 123%
              begin
                if (AExpressionRecord.TextMatchKind = tmkUnknown) then
                  AExpressionRecord.TextMatchKind:=tmkMatchTextBeginning
                else
                  AExpressionRecord.TextMatchKind:=tmkMatchTextAnywhere;
                LLastPercent:=true;
              end;
            tkNone: AExpressionRecord.TextMatchKind:=tmkMatchTextEnd;
            tkPercent: raise ESearchExpressionParserException.CreateFmt(sUnexpectedOccurrenceOf_Found, ['%']);
          end;
        end
        else
          raise ESearchExpressionParserException.CreateFmt(sUnexpectedOccurrenceOf_Found, ['%']);
      tkLess..tkGreaterOrEqual:
        case LExpressionKind of
          ekNum:
            if LPrevToken <> tkNone then
              raise ESearchExpressionParserException.Create(sBadNumericExpression)
            else
              with AExpressionRecord do
              case LToken of
                tkLess: NumericComparisionKind:=nckNumericLT;
                tkGreater: NumericComparisionKind:=nckNumericGT;
                tkEqual: NumericComparisionKind:=nckNumericEQ;
                tkLessOrEqual: NumericComparisionKind:=nckNumericLTE;
                tkGreaterOrEqual: NumericComparisionKind:=nckNumericGTE;
              end;
          ekSet: raise ESearchExpressionParserException.CreateFmt(sUnexpectedTokenFound, [TokenToStr(LToken)]);
        end;
      tkOpeningParenthesis, tkOpeningBracket:
        if LExpressionKind = ekSet then
          if LPrevToken <> tkNone then
            raise ESearchExpressionParserException.CreateFmt(sBadBetweenExpression_UnexpectedToken, [TokenToStr(LToken)])
          else
          with AExpressionRecord do
          case LToken of
            tkOpeningParenthesis: SetKind:=skNumericBetweenExclusive;
            tkOpeningBracket: SetKind:=skNumericBetweenInclusive;
          end;
    end;
    LPrevToken := LToken;
    Inc(c, LChar.Length);
  until (LChar.Length=0) or (c^ = #0);

  case LExpressionKind of
    ekText:
      if AExpressionRecord.TextMatchKind = tmkUnknown then
        AExpressionRecord.TextMatchKind:=tmkMatchTextExact;
    ekSet:
      case AExpressionRecord.SetKind of
        skNumericBetweenInclusive:
          if LPrevToken <> tkClosingBracket then
            raise ESearchExpressionParserException.Create(sBadlyClosedBetweenExpression);
        skNumericBetweenExclusive:
          if LPrevToken <> tkClosingParenthesis then
            raise ESearchExpressionParserException.Create(sBadlyClosedBetweenExpression);
      end;
  end;

  if (LValueIdx = 0) and (LValue^='') then
    raise ESearchExpressionParserException.Create(sExpressionError_NoValue);

  SetLength(AExpressionRecord.Values, LValueIdx + 1);
  for i := 0 to LValueIdx do
    AExpressionRecord.Values[i] := LValues[i];

  AExpressionRecord.Kind := LExpressionKind;
  AExpressionRecord.HasDecimals := LHasDecimals;
end;

class function TSearchExpressionService.Parse(const AExpression: utf8string): TExpressionRecord;
begin
  Result.Kind := ekUnknown;
  Parse(AExpression, Result.Kind, Result);
end;

initialization
  DataRowType := TDataRow.Create;

finalization
  DataRowType.Free;

end.
