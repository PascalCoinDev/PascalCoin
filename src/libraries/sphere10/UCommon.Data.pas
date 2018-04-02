{
  Copyright (c) 2017 - 2018 Sphere 10 Software

  Common data-oriented classes usable across all tiers.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld
    Maciej Izak (hnb)
}

unit UCommon.Data;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, Generics.Collections, UMemory, UCommon, UCommon.Collections, Generics.Defaults,
  Variants, LazUTF8, math, typinfo, syncobjs;

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
    Name : AnsiString;
    // FType: typecode??
    class function From(AName: AnsiString) : TDataColumn; overload; static;
  end;

  TDataColumns = TArray<TDataColumn>;

  { TDataRow }

  TDataRow = class(TInvokeableVariantType)
  private
    class constructor Create;
    class destructor Destroy;
  protected type
    TColumnMapToIndex = TDictionary<AnsiString, Integer>;
    TColumnsDictionary = TObjectDictionary<AnsiString, TColumnMapToIndex>;
  protected class var
    FColumns: TColumnsDictionary;
  protected
    class function MapColumns(const ADataSourceClassName : AnsiString; const AColumns: TDataColumns): TColumnMapToIndex;
  public
    function GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean; override;
    function SetProperty(var V: TVarData; const Name: string; const Value: TVarData): Boolean; override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
    class function New(const ADataSourceClassName : AnsiString; const AColumns: TDataColumns): Variant;
  end;

  ETableRow = class(Exception);

  TDataRowData = packed record
  public
    vtype: tvartype;
  private
    vfiller1 : word;
    vfiller2: int32;
  public
    vcolumnmap: TDataRow.TColumnMapToIndex;
    vvalues: TArray<Variant>;
  end;

  { TColumnFilter }

  TColumnFilter = record
    ColumnName: utf8string;
    Sort: TSortDirection;
    Filter: TDataFilter;
    Values: array of Variant;
    SortSequence : Integer;
  end;

  { TFilterCriteria }

  TFilterCriteria = TArray<TColumnFilter>;

  { TApplyFilterDelegate }

  TApplyFilterDelegate<T> = function (constref AItem : T; constref AColumnFilter: TColumnFilter) : boolean of object;

  { TApplySortDelegate }

  TApplySortDelegate<T> = function (constref Left, Right : T; constref AColumnFilter: TColumnFilter) : Integer of object;

  { TDataTable }

  PDataTable = ^TDataTable;
  TDataTable = record
  public
    Columns: TDataColumns;
    Rows : TArray<Variant>;
  end;

  { TColumnFilterPredicate -- should be implementation only }

  TColumnFilterPredicate<T> = class (TInterfacedObject, IPredicate<T>)
     private
       FFilter : TColumnFilter;
       FDelegate : TApplyFilterDelegate<T>;
     public
       constructor Create(const AFilter : TColumnFilter; const ADelegate : TApplyFilterDelegate<T>); overload;
       function Evaluate (constref AValue: T) : boolean;
   end;

  {TColumnFieldComparer }

  TColumnFieldComparer<T> = class (TInterfacedObject, IComparer<T>)
    private
      FFilter : TColumnFilter;
      FDelegate: TApplySortDelegate<T>;
    public
      constructor Create(const AFilter : TColumnFilter; const ADelegate: TApplySortDelegate<T>);
      function Compare(constref ALeft, ARight: T): Integer; virtual;
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
    function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
  end;

  { TCustomDataSource }

  TCustomDataSource<T> = class(TComponent, IDataSource)
    private
      FLock : TCriticalSection;
      FClassName : AnsiString;
    protected
      function GetNullPolicy(const AFilter : TColumnFilter) : TSortNullPolicy; virtual;
      function GetItemDisposePolicy : TDisposePolicy; virtual; abstract;
      function GetColumns : TDataColumns; virtual; abstract;
      function ApplyColumnSort(constref Left, Right : T; constref AFilter: TColumnFilter) : Integer; virtual;
      function ApplyColumnFilter(constref AItem: T; constref AFilter: TColumnFilter) : boolean; virtual;
      function GetItemField(constref AItem: T; const ABindingName : AnsiString) : Variant; virtual; abstract;
      procedure DehydrateItem(constref AItem: T; var ADataRow: Variant); virtual; abstract;
      function FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
      function GetEntityKey(constref AItem: T) : Variant; virtual;
      procedure OnBeforeFetchAll(constref AParams: TPageFetchParams); virtual;
      procedure FetchAll(const AContainer : TList<T>); virtual; abstract;
      procedure OnAfterFetchAll(constref AParams: TPageFetchParams); virtual;
    public
      constructor Create(AOwner: TComponent); override; overload;
      destructor Destroy; override;
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
     class function ConstructRowComparer(const AFilters : TArray<TColumnFilter>; const ADelegate : TApplySortDelegate<T>) : IComparer<T>;
     class function ConstructRowPredicate(const AFilters : TArray<TColumnFilter>; const ADelegate : TApplyFilterDelegate<T>; const AOperand : TFilterOperand) : IPredicate<T>;
  end;


{ RESOURCES }
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

uses dateutils;

{ VARIABLES }

var
  DataRowType: TDataRow = nil;

{ TDataColumn }

class function TDataColumn.From(AName: AnsiString) : TDataColumn;
begin
  Result.Name := AName;
end;

{ TDataRow }

class constructor TDataRow.Create;
begin
  FColumns := TColumnsDictionary.Create([doOwnsValues]);
end;

class destructor TDataRow.Destroy;
begin
  FColumns.Free;
end;

class function TDataRow.MapColumns(const ADataSourceClassName : AnsiString; const AColumns: TDataColumns): TColumnMapToIndex;
var
  i: Integer;
begin
  Result := TColumnMapToIndex.Create;
  for i := Low(AColumns) to High(AColumns) do
    Result.Add(AColumns[i].Name, i);
  Result.Add('__KEY', i + 1);
  FColumns.Add(ADataSourceClassName, Result);
end;

function TDataRow.GetProperty(var Dest: TVarData;
  const V: TVarData; const Name: string): Boolean;
var
  LRow: TDataRowData absolute V;
begin
  Variant(Dest) := LRow.vvalues[LRow.vcolumnmap[Name]];
  Result := true;
end;

function TDataRow.SetProperty(var V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
var
  LRow: TDataRowData absolute V;
begin
  if NOT LRow.vcolumnmap.ContainsKey(Name) then
    Exit(true); // TODO: Re-enable this when TVisualColumn added -- ETableRow.Create(Format('TableRow did not have column "%s"', [Name]));

  LRow.vvalues[LRow.vcolumnmap[Name]] := Variant(Value);
  Result := true;
end;

procedure TDataRow.Clear(var V: TVarData);
begin
  Finalize(TDataRowData(V));
  FillChar(V, SizeOf(V), #0);
end;

procedure TDataRow.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
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

function TDataRow.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  LRow: TDataRowData absolute V;
begin
  Result := (Name = '_') and (Length(Arguments)=1);
  if Result then
    Variant(Dest) := LRow.vvalues[Variant(Arguments[0])];
end;

class function TDataRow.New(const ADataSourceClassName:AnsiString; const AColumns: TDataColumns): Variant;

var
  LColumnMap: TColumnMapToIndex;
begin
  if not Assigned(AColumns) then
    raise ETableRow.Create(sAColumnsCantBeNil);

  VarClear(Result);
  FillChar(Result, SizeOf(Result), #0);
  TDataRowData(Result).vtype:=DataRowType.VarType;

  if not FColumns.TryGetValue(ADataSourceClassName, LColumnMap) then
    LColumnMap := MapColumns(ADataSourceClassName, AColumns);

  TDataRowData(Result).vcolumnmap:=LColumnMap;
  SetLength(TDataRowData(Result).vvalues, LColumnMap.Count);
end;

{ TCustomDataSource }

constructor TCustomDataSource<T>.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FClassName := Self.ClassName;
end;

{constructor TCustomDataSource<T>.Create(AOwner: TComponent; const ADataSourceClassName : AnsiString);
begin
  inherited Create(AOwner);
  FLock := TCriticalSection.Create;
  FClassName := ADataSourceClassName;
end;}

destructor TCustomDataSource<T>.Destroy;
var
  i : integer;
  policy : TDisposePolicy;
begin
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

function TCustomDataSource<T>.ApplyColumnSort(constref Left, Right : T; constref AFilter: TColumnFilter) : Integer;
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
  Result := TCompare.Variant(@leftField, @rightField);

  // Invert result for descending
  if AFilter.Sort = sdDescending then
    Result := Result * -1;
end;

function TCustomDataSource<T>.ApplyColumnFilter(constref AItem: T; constref AFilter: TColumnFilter) : boolean;
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

function TCustomDataSource<T>.GetEntityKey(constref AItem: T) : Variant;
begin
  Result := nil;
end;

function TCustomDataSource<T>.FetchPage(constref AParams: TPageFetchParams; var ADataTable: TDataTable): TPageFetchResult;
var
  i, j : SizeInt;
  data : TList<T>;
  GC : TDisposables;
  pageStart, pageEnd : SizeInt;
  entity : T;
  comparer : IComparer<T>;
  filters : TArray<TColumnFilter>;
  filter : IPredicate<T>;
begin
  OnBeforeFetchAll(AParams);
  FLock.Acquire;
  try
     // Fetch underlying data if stale
     data := GC.AddObject( TList<T>.Create ) as TList<T>;
     FetchAll(data);

     // Filter the data
     filters := AParams.GetSearchFilters;
     if Length(filters) > 0 then
       TListTool<T>.FilterBy( data, TDataSourceTool<T>.ConstructRowPredicate( filters, ApplyColumnFilter, AParams.Operand ) );

     // Sort the data
     filters := AParams.GetSortFilters;
     if Length(filters) > 0 then
       data.Sort( TDataSourceTool<T>.ConstructRowComparer( filters, ApplyColumnSort ) );

     // Setup result
     Result.TotalDataCount := data.Count;
     if (Result.TotalDataCount = 0) then begin
         Result.PageCount := 0;
         Result.PageIndex := -1;
         pageStart := 0;
         pageEnd := -1;
     end;
     Result.PageCount := Ceil (data.Count / (1.0 *AParams.PageSize));
     if (AParams.PageSize >= 0) and (data.Count > 0) then begin
       Result.PageIndex := ClipValue(AParams.PageIndex, 0, Result.PageCount - 1);
       pageStart := Result.PageIndex * AParams.PageSize;
       pageEnd := ClipValue(pageStart + (AParams.PageSize - 1), pageStart, data.Count - 1);
     end else begin
       Result.PageIndex := 0;
       pageStart := 0;
       pageEnd := data.Count - 1;
     end;

     // Dehydrate the page of data only

     // Set columns
     ADataTable.Columns := GetColumns;

     if pageEnd >= pageStart then begin
       j := 0;
       SetLength(ADataTable.Rows, pageEnd - pageStart + 1);
       for i := pageStart to pageEnd do begin
         ADataTable.Rows[j] := TDataRow.New(FClassName, ADataTable.Columns);
         DehydrateItem( data[i], ADataTable.Rows[j]);
         ADataTable.Rows[j].__KEY := GetEntityKey(data[i]);
         inc(j)
       end;
     end;
  finally
    FLock.Release;
  end;
  OnAfterFetchAll(AParams);
end;

procedure TCustomDataSource<T>.OnBeforeFetchAll(constref AParams: TPageFetchParams);
begin
end;

procedure TCustomDataSource<T>.OnAfterFetchAll(constref AParams: TPageFetchParams);
begin
end;

{ TColumnFilterPredicate }

constructor TColumnFilterPredicate<T>.Create(const AFilter : TColumnFilter; const ADelegate : TApplyFilterDelegate<T>);
begin
  FFilter := AFilter;
  FDelegate := ADelegate;
end;

function TColumnFilterPredicate<T>.Evaluate (constref AValue: T) : boolean;
begin
  Result := FDelegate(AValue, FFilter);
end;

{ TColumnFieldComparer }

constructor TColumnFieldComparer<T>.Create(const AFilter : TColumnFilter; const ADelegate: TApplySortDelegate<T>);
begin
  FFilter := AFilter;
  FDelegate := ADelegate;
end;

function TColumnFieldComparer<T>.Compare(constref ALeft, ARight: T): Integer;
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

  case filters.Count of
    0: Result := nil;
    1: Result := filters[0];
    else case AOperand of
      foAnd: Result := __TPredicateTool_T.AndMany(filters.ToArray);
      foOr: Result := __TPredicateTool_T.OrMany(filters.ToArray);
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

  function IsSortFilter(constref AColFilter : TColumnFilter) : boolean;
  begin
    Result := AColFilter.Sort <> sdNone;
  end;

begin
  sortFilters := GC.AddObject( TList<TColumnFilter>.Create ) as TList<TColumnFilter>;
  sortFilters.AddRange( Filter);
  TListTool<TColumnFilter>.FilterBy( sortFilters, TPredicateTool<TColumnFilter>.FromFunc( IsSortFilter ));
  SetLength(Result, 0);
  if sortFilters.Count > 0 then
    Result := sortFilters.ToArray;
end;

function TPageFetchParams.GetSearchFilters : TArray<TColumnFilter>;
var
  searchFilters : TList<TColumnFilter>;
  GC : TDisposables;

  function IsSearchFilter(constref AColFilter : TColumnFilter) : boolean;
  begin
    Result := AColFilter.Filter <> vgfSortable;
  end;

begin
  searchFilters := GC.AddObject( TList<TColumnFilter>.Create ) as TList<TColumnFilter>;
  searchFilters.AddRange(Filter);
  TListTool<TColumnFilter>.FilterBy( searchFilters, TPredicateTool<TColumnFilter>.FromFunc( IsSearchFilter ));
  SetLength(Result, 0);
  if searchFilters.Count > 0 then
    Result := searchFilters.ToArray;
end;

{ TSearchExpressionService }

class procedure TSearchExpressionService.Parse(const AExpression: utf8string;
  const AExpressionKind: TExpressionKind; out
  AExpressionRecord: TExpressionRecord);
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

const
  CONVERTABLE_TOKENS_TO_STR = [tkLess..tkClosingBracket, tkComma];
  NUM_OPERATORS = [tkLess..tkGreaterOrEqual];

  procedure GetChar(APos: PAnsiChar; out AChar: TUTF8Char);
  begin
    AChar.Length := UTF8CharacterLength(APos);

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
  LDot: boolean = false;
  LChar: TUTF8Char;
  LValueIdx: Integer = -1;
  LValues: array[0..MAX_VALUES-1] of utf8string; // for now only 2 values for set "between"
  LValue: PUTF8String;
  LToken: TToken = tkNone;
  LPrevToken: TToken = tkNone;
  LExpression: utf8string;
  LLastPercent: boolean = false;
  LExpressionKind: TExpressionKind;
  LWasUnknow: boolean;
  LHasDecimals: boolean = false;

  procedure NextValue;
  begin
    Inc(LValueIdx);
    if LValueIdx > MAX_VALUES - 1 then
      raise ESearchExpressionParserException.Create(sTooManyValues);
    LValue := @LValues[LValueIdx];
  end;

  procedure EscapeSequence(AChar: Char);
  begin
    LToken := tkText;
    LValue^ := LValue^ + AChar;
    Inc(c);
  end;

begin
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
  if FindInvalidUTF8Character(c, Length(LExpression)) <> -1 then
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
            tkNone:
              AExpressionRecord.TextMatchKind:=tmkMatchTextEnd;
            tkPercent:
              raise ESearchExpressionParserException.CreateFmt(sUnexpectedOccurrenceOf_Found, ['%']);
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
          ekSet:
            raise ESearchExpressionParserException.CreateFmt(sUnexpectedTokenFound, [TokenToStr(LToken)]);
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

class function TSearchExpressionService.Parse(const AExpression: utf8string
  ): TExpressionRecord;
begin
  Result.Kind := ekUnknown;
  Parse(AExpression, Result.Kind, Result);
end;

initialization
  DataRowType := TDataRow.Create;

finalization
  DataRowType.Free;
end.

