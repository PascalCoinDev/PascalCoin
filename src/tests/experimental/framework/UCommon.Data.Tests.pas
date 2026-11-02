unit UCommon.Data.Tests;

{ Copyright © 2026 Herman Schoenfeld
  Distributed under the MIT software license, see the accompanying LICENSE. }

{$mode delphi}

interface

uses Classes, SysUtils, SyncObjs, Variants, fpcunit, testregistry,
  Generics.Collections, UCommon.Data;

type
  TDataSourceSnapshotTests = class(TTestCase)
  published
    procedure PagingReusesFetchedAndSortedSnapshot;
    procedure SearchAndSortReuseRawSnapshot;
    procedure CriteriaValuesAndOperandArePartOfTheQuery;
    procedure InvalidationsCoalesceAndClearEmptyRows;
    procedure DefaultSourceStillFetchesEveryTime;
    procedure InvalidatingDoesNotWaitForBlockedFetch;
    procedure FailedFetchDoesNotCachePartialData;
    procedure FilteringDoesNotModifyTheRawSnapshot;
    procedure PageSizesAndClampingUseTheCachedCount;
    procedure EmptyColumnsAreRejected;
    procedure KeyAndColumnValuesRoundTripIndependently;
    procedure PositionalCallsWithRuntimeIndexPreserveColumnsAndKey;
    procedure PositionalDispatcherRejectsUnknownNamesAndWrongArity;
    procedure UnicodeColumnNamesBindingsAndValuesRoundTrip;
  end;

implementation

uses UMemory;

type
  TUnicodeBindingSource = class(TCustomDataSource<Integer>)
  protected
    function GetColumns: TDataColumns; override;
    function GetItemField(constref AItem: Integer; const ABindingName: String): Variant; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
  end;

  TCountingSnapshotSource = class(TCustomDataSource<Integer>)
  protected
    function GetColumns: TDataColumns; override;
    function GetItemField(constref AItem: Integer; const ABindingName: String): Variant; override;
    function ApplyColumnSort(constref ALeft, ARight: Integer; constref AFilter: TColumnFilter): Integer; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
  public
    ItemCount, FetchCount, FieldCalls, SortCalls: Integer;
    BlockFetch, FailNext: Boolean;
    Entered, ReleaseFetch: TEvent;
    constructor Create;
    destructor Destroy; override;
    function Fetch(AIndex, ASize: Integer; const AFilter: TFilterCriteria;
      AOperand: TFilterOperand; var ATable: TDataTable): TPageFetchResult;
  end;

  TSnapshotTestThread = class(TThread)
  private
    FSource: TCountingSnapshotSource;
    FInvalidate: Boolean;
  protected
    procedure Execute; override;
  public
    Completed: TEvent;
    ErrorMessage: String;
    constructor Create(ASource: TCountingSnapshotSource; AInvalidate: Boolean);
    destructor Destroy; override;
  end;

function UnicodeBinding(ASecond: Boolean): String;
var LText: UnicodeString;
begin
  if ASecond then LText := #$65E5#$672C#$D83D#$DE80
  else LText := #$4E2D#$6587#$D83D#$DE80;
  Result := UTF8Encode(LText);
end;

function UnicodePayload: UnicodeString;
begin
  Result := #$03A9#$0416#$6F22#$D83D#$DE80;
end;

function TUnicodeBindingSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From(UnicodeBinding(False), True),
    TDataColumn.From(UnicodeBinding(True)));
end;

function TUnicodeBindingSource.GetItemField(constref AItem: Integer;
  const ABindingName: String): Variant;
begin
  if ABindingName = UnicodeBinding(False) then Result := AItem
  else if ABindingName = UnicodeBinding(True) then
    Result := UnicodePayload + UnicodeString(IntToStr(AItem))
  else raise Exception.Create('Unicode binding was changed before field lookup.');
end;

procedure TUnicodeBindingSource.FetchAll(const AContainer: TList<Integer>);
begin
  AContainer.Add(2);
  AContainer.Add(1);
end;

constructor TCountingSnapshotSource.Create;
begin
  inherited Create(nil);
  ItemCount := 100;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseFetch := TEvent.Create(nil, True, False, '');
end;

destructor TCountingSnapshotSource.Destroy;
begin
  Entered.Free;
  ReleaseFetch.Free;
  inherited Destroy;
end;

function TCountingSnapshotSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Value', True));
end;

function TCountingSnapshotSource.GetItemField(constref AItem: Integer;
  const ABindingName: String): Variant;
begin
  Inc(FieldCalls);
  Result := AItem;
end;

function TCountingSnapshotSource.ApplyColumnSort(constref ALeft, ARight: Integer;
  constref AFilter: TColumnFilter): Integer;
begin
  Inc(SortCalls);
  Result := inherited ApplyColumnSort(ALeft, ARight, AFilter);
end;

procedure TCountingSnapshotSource.FetchAll(const AContainer: TList<Integer>);
var i, LCount: Integer;
begin
  Inc(FetchCount);
  LCount := ItemCount;
  if BlockFetch then begin
    Entered.SetEvent;
    if ReleaseFetch.WaitFor(10000) <> wrSignaled then
      raise Exception.Create('Snapshot fetch gate timed out');
  end;
  for i := LCount - 1 downto 0 do begin
    AContainer.Add(i);
    if FailNext then begin
      FailNext := False;
      raise Exception.Create('Expected snapshot fetch failure');
    end;
  end;
end;

function TCountingSnapshotSource.Fetch(AIndex, ASize: Integer;
  const AFilter: TFilterCriteria; AOperand: TFilterOperand; var ATable: TDataTable): TPageFetchResult;
begin
  Result := FetchPage(TPageFetchParams.Create(AIndex, ASize, AFilter, AOperand), ATable);
end;

constructor TSnapshotTestThread.Create(ASource: TCountingSnapshotSource; AInvalidate: Boolean);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSource := ASource;
  FInvalidate := AInvalidate;
  Completed := TEvent.Create(nil, True, False, '');
  Start;
end;

destructor TSnapshotTestThread.Destroy;
begin
  Completed.Free;
  inherited Destroy;
end;

procedure TSnapshotTestThread.Execute;
var i: Integer; LTable: TDataTable;
begin
  try
    if FInvalidate then
      for i := 1 to 1000 do FSource.InvalidateSnapshot
    else
      FSource.Fetch(0, 10, nil, foAnd, LTable);
  except
    on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
  end;
  Completed.SetEvent;
end;

function SortByValue(ADirection: TSortDirection): TFilterCriteria;
begin
  SetLength(Result, 1);
  Result[0] := Default(TColumnFilter);
  Result[0].ColumnName := 'Value';
  Result[0].Filter := vgfSortable;
  Result[0].Sort := ADirection;
end;

function AtLeast(AValue: Integer): TFilterCriteria;
begin
  SetLength(Result, 1);
  Result[0] := Default(TColumnFilter);
  Result[0].ColumnName := 'Value';
  Result[0].Filter := vgfNumericGTE;
  SetLength(Result[0].Values, 1);
  Result[0].Values[0] := AValue;
end;

procedure TDataSourceSnapshotTests.PagingReusesFetchedAndSortedSnapshot;
var
  LSource: TCountingSnapshotSource;
  LTable: TDataTable;
  LResult: TPageFetchResult;
  LFilters: TFilterCriteria;
  LFieldCalls, LSortCalls: Integer;
  LDisposables: TDisposables;
begin
  LSource := LDisposables.AddObject(TCountingSnapshotSource.Create) as TCountingSnapshotSource;
  LSource.ItemCount := 50000;
  LSource.SnapshotCacheEnabled := True;
  LFilters := SortByValue(sdAscending);
  LResult := LSource.Fetch(0, 25, LFilters, foAnd, LTable);
  AssertEquals(50000, LResult.TotalDataCount);
  AssertEquals(0, Integer(LTable.Rows[0].Value));
  LFieldCalls := LSource.FieldCalls;
  LSortCalls := LSource.SortCalls;
  AssertTrue(LSortCalls > 0);
  LResult := LSource.Fetch(1, 25, LFilters, foAnd, LTable);
  AssertEquals(25, Integer(LTable.Rows[0].Value));
  AssertEquals(1, LSource.FetchCount);
  AssertEquals(LSortCalls, LSource.SortCalls);
  AssertEquals('Only the requested page should be converted', 25, LSource.FieldCalls - LFieldCalls);
  AssertEquals(2000, LResult.PageCount);
end;

procedure TDataSourceSnapshotTests.SearchAndSortReuseRawSnapshot;
var
  LSource: TCountingSnapshotSource;
  LTable: TDataTable;
  LFilters: TFilterCriteria;
  LResult: TPageFetchResult;
  LDisposables: TDisposables;
begin
  LSource := LDisposables.AddObject(TCountingSnapshotSource.Create) as TCountingSnapshotSource;
  LSource.SnapshotCacheEnabled := True;
  LFilters := AtLeast(50);
  LFilters[0].Sort := sdDescending;
  LResult := LSource.Fetch(0, 10, LFilters, foAnd, LTable);
  AssertEquals(50, LResult.TotalDataCount);
  AssertEquals(99, Integer(LTable.Rows[0].Value));
  LFilters[0].Sort := sdAscending;
  LSource.Fetch(0, 10, LFilters, foAnd, LTable);
  AssertEquals(50, Integer(LTable.Rows[0].Value));
  AssertEquals(1, LSource.FetchCount);
  LFilters[0].Values[0] := 95;
  LResult := LSource.Fetch(0, 10, LFilters, foAnd, LTable);
  AssertEquals(5, LResult.TotalDataCount);
  AssertEquals(95, Integer(LTable.Rows[0].Value));
  AssertEquals(1, LSource.FetchCount);
end;

procedure TDataSourceSnapshotTests.CriteriaValuesAndOperandArePartOfTheQuery;
var
  LSource: TCountingSnapshotSource;
  LTable: TDataTable;
  LFilters: TFilterCriteria;
  LResult: TPageFetchResult;
  LDisposables: TDisposables;
begin
  LSource := LDisposables.AddObject(TCountingSnapshotSource.Create) as TCountingSnapshotSource;
  LSource.SnapshotCacheEnabled := True;
  LFilters := AtLeast(90);
  LSource.Fetch(0, 25, LFilters, foAnd, LTable);
  LFilters[0].Values[0] := 95; // Same caller array must not mutate the cached query.
  LResult := LSource.Fetch(0, 25, LFilters, foAnd, LTable);
  AssertEquals(5, LResult.TotalDataCount);
  SetLength(LFilters, 2);
  LFilters[0].Values[0] := 90;
  LFilters[1] := Default(TColumnFilter);
  LFilters[1].ColumnName := 'Value';
  LFilters[1].Filter := vgfNumericLT;
  SetLength(LFilters[1].Values, 1);
  LFilters[1].Values[0] := 10;
  LResult := LSource.Fetch(0, 25, LFilters, foOr, LTable);
  AssertEquals(20, LResult.TotalDataCount);
  LResult := LSource.Fetch(0, 25, LFilters, foAnd, LTable);
  AssertEquals(0, LResult.TotalDataCount);
  AssertEquals(0, Length(LTable.Rows));
  AssertEquals(1, LSource.FetchCount);
end;

procedure TDataSourceSnapshotTests.InvalidationsCoalesceAndClearEmptyRows;
var
  LSource: TCountingSnapshotSource;
  LTable: TDataTable;
  LResult: TPageFetchResult;
  i: Integer;
  LDisposables: TDisposables;
begin
  LSource := LDisposables.AddObject(TCountingSnapshotSource.Create) as TCountingSnapshotSource;
  LSource.SnapshotCacheEnabled := True;
  LSource.Fetch(0, 10, nil, foAnd, LTable);
  AssertEquals(10, Length(LTable.Rows));
  LSource.ItemCount := 0;
  for i := 1 to 1000 do LSource.InvalidateSnapshot;
  LResult := LSource.Fetch(0, 10, nil, foAnd, LTable);
  AssertEquals(2, LSource.FetchCount);
  AssertEquals(0, Length(LTable.Rows));
  AssertEquals(0, LResult.TotalDataCount);
  AssertEquals(0, LResult.PageCount);
  AssertEquals(-1, LResult.PageIndex);
  LSource.Fetch(0, 10, nil, foAnd, LTable);
  AssertEquals(2, LSource.FetchCount);
end;

procedure TDataSourceSnapshotTests.DefaultSourceStillFetchesEveryTime;
var
  LSource: TCountingSnapshotSource;
  LTable: TDataTable;
  LDisposables: TDisposables;
begin
  LSource := LDisposables.AddObject(TCountingSnapshotSource.Create) as TCountingSnapshotSource;
  AssertFalse(LSource.SnapshotCacheEnabled);
  LSource.Fetch(0, 10, nil, foAnd, LTable);
  LSource.Fetch(1, 10, nil, foAnd, LTable);
  AssertEquals(2, LSource.FetchCount);
end;

procedure TDataSourceSnapshotTests.InvalidatingDoesNotWaitForBlockedFetch;
var
  LSource: TCountingSnapshotSource;
  LFetch, LInvalidate: TSnapshotTestThread;
  LCompleted: Boolean;
  LTable: TDataTable;
begin
  LSource := TCountingSnapshotSource.Create;
  try
    LFetch := nil;
    LInvalidate := nil;
    try
      LSource.SnapshotCacheEnabled := True;
      LSource.BlockFetch := True;
      LFetch := TSnapshotTestThread.Create(LSource, False);
      AssertEquals('Fetch did not enter its gate', Ord(wrSignaled), Ord(LSource.Entered.WaitFor(5000)));
      LInvalidate := TSnapshotTestThread.Create(LSource, True);
      LCompleted := LInvalidate.Completed.WaitFor(500) = wrSignaled;
      AssertTrue('Logical invalidation waited for the fetch lock', LCompleted);
      AssertEquals('', LInvalidate.ErrorMessage);
      AssertEquals('The fetch gate must remain closed during invalidation', Ord(wrTimeout),
        Ord(LSource.ReleaseFetch.WaitFor(0)));
    finally
      LSource.ReleaseFetch.SetEvent;
      if Assigned(LFetch) then LFetch.WaitFor;
      if Assigned(LInvalidate) then LInvalidate.WaitFor;
      LInvalidate.Free;
      LFetch.Free;
    end;
    // The in-flight snapshot used an old generation and must not be reused.
    LSource.Fetch(0, 10, nil, foAnd, LTable);
    AssertEquals(2, LSource.FetchCount);
  finally
    LSource.Free;
  end;
end;

procedure TDataSourceSnapshotTests.FailedFetchDoesNotCachePartialData;
var
  LSource: TCountingSnapshotSource;
  LTable: TDataTable;
  LResult: TPageFetchResult;
  LFailed: Boolean;
  LDisposables: TDisposables;
begin
  LSource := LDisposables.AddObject(TCountingSnapshotSource.Create) as TCountingSnapshotSource;
  LSource.SnapshotCacheEnabled := True;
  LSource.FailNext := True;
  LFailed := False;
  try
    LSource.Fetch(0, 10, nil, foAnd, LTable);
  except
    on E: Exception do LFailed := E.Message = 'Expected snapshot fetch failure';
  end;
  AssertTrue(LFailed);
  LResult := LSource.Fetch(0, 10, nil, foAnd, LTable);
  AssertEquals(2, LSource.FetchCount);
  AssertEquals(100, LResult.TotalDataCount);
  AssertEquals(10, Length(LTable.Rows));
end;

procedure TDataSourceSnapshotTests.FilteringDoesNotModifyTheRawSnapshot;
var
  LSource: TCountingSnapshotSource;
  LTable: TDataTable;
  LResult: TPageFetchResult;
  LDisposables: TDisposables;
begin
  LSource := LDisposables.AddObject(TCountingSnapshotSource.Create) as TCountingSnapshotSource;
  LSource.SnapshotCacheEnabled := True;
  LResult := LSource.Fetch(0, 10, AtLeast(95), foAnd, LTable);
  AssertEquals(5, LResult.TotalDataCount);
  LResult := LSource.Fetch(0, 10, nil, foAnd, LTable);
  AssertEquals(100, LResult.TotalDataCount);
  AssertEquals(99, Integer(LTable.Rows[0].Value));
  AssertEquals(1, LSource.FetchCount);
end;

procedure TDataSourceSnapshotTests.PageSizesAndClampingUseTheCachedCount;
var
  LSource: TCountingSnapshotSource;
  LTable: TDataTable;
  LResult: TPageFetchResult;
  LDisposables: TDisposables;
begin
  LSource := LDisposables.AddObject(TCountingSnapshotSource.Create) as TCountingSnapshotSource;
  LSource.ItemCount := 3;
  LSource.SnapshotCacheEnabled := True;
  LResult := LSource.Fetch(99, 2, nil, foAnd, LTable);
  AssertEquals(1, LResult.PageIndex);
  AssertEquals(2, LResult.PageCount);
  AssertEquals(1, Length(LTable.Rows));
  LResult := LSource.Fetch(0, -1, nil, foAnd, LTable);
  AssertEquals(0, LResult.PageIndex);
  AssertEquals(1, LResult.PageCount);
  AssertEquals(3, Length(LTable.Rows));
  AssertEquals(1, LSource.FetchCount);
end;

procedure TDataSourceSnapshotTests.EmptyColumnsAreRejected;
var LColumns : TDataColumns;
  LRow : Variant;
  LRejected : Boolean;
begin
  LColumns := nil;
  LRejected := False;
  try
    LRow := TDataRow.New($5343484D,LColumns);
  except
    on E : EDataRow do LRejected := True;
  end;
  AssertTrue('A row must reject an empty schema before allocating values',LRejected);
end;

procedure TDataSourceSnapshotTests.KeyAndColumnValuesRoundTripIndependently;
var LColumns : TDataColumns;
  LRow, LCopy : Variant;
  LData : TDataRowData;
begin
  SetLength(LColumns,2);
  LColumns[0] := TDataColumn.From('Value',True);
  LColumns[1] := TDataColumn.From('Title');
  LRow := TDataRow.New($5343484D,LColumns);
  LData := TDataRowData(LRow);
  LData['Value'] := 17;
  LData['Title'] := 'saved';
  LData['__KEY'] := 29;
  AssertEquals(17,Integer(LRow.Value));
  AssertEquals('saved',String(LRow.Title));
  AssertEquals('The key follows every declared column',29,Integer(LRow.__KEY));
  AssertEquals('Positional reads retain the original first column',17,Integer(LRow._(0)));
  LCopy := LRow;
  LCopy.__KEY := 31;
  AssertEquals('A copied row owns an independent value array',29,Integer(LRow.__KEY));
  AssertEquals(31,Integer(LCopy.__KEY));
end;

procedure TDataSourceSnapshotTests.PositionalCallsWithRuntimeIndexPreserveColumnsAndKey;
var LColumns: TDataColumns;
  LRow, LValue: Variant;
  LData: TDataRowData;
  LIndex, LNumber: Integer;
begin
  SetLength(LColumns,2);
  LColumns[0] := TDataColumn.From('Value',True);
  LColumns[1] := TDataColumn.From('Title');
  LRow := TDataRow.New($53434958,LColumns);
  LData := TDataRowData(LRow);
  LData['Value'] := 17;
  LData['Title'] := 'saved';
  LData['__KEY'] := 29;
  LIndex := 0;
  LValue := LRow._(LIndex);
  AssertEquals(17,Integer(LValue));
  LNumber := LRow._(LIndex);
  AssertEquals(17,LNumber);
  LIndex := 1;
  LValue := LRow._(LIndex);
  AssertEquals('saved',String(LValue));
  LIndex := 2;
  AssertEquals(29,Integer(LRow._(LIndex)));
  AssertEquals(17,Integer(LRow.Value));
  AssertEquals(29,Integer(LRow.__KEY));
end;

procedure TDataSourceSnapshotTests.PositionalDispatcherRejectsUnknownNamesAndWrongArity;
var LColumns: TDataColumns;
  LRow, LValue: Variant;
  LData: TDataRowData;
  LHandler: TCustomVariantType;
  LArguments: TVarDataArray;
begin
  SetLength(LColumns,1);
  LColumns[0] := TDataColumn.From('Value',True);
  LRow := TDataRow.New($53434446,LColumns);
  LData := TDataRowData(LRow);
  LData['Value'] := 17;
  AssertTrue(FindCustomVariantType(VarType(LRow),LHandler));
  LValue := 93;
  AssertFalse(TDataRow(LHandler).DoFunction(TVarData(LValue),TVarData(LRow),'_',nil));
  AssertFalse(TDataRow(LHandler).DoFunction(TVarData(LValue),TVarData(LRow),'',nil));
  SetLength(LArguments,2);
  Variant(LArguments[0]) := 0;
  Variant(LArguments[1]) := 0;
  AssertFalse(TDataRow(LHandler).DoFunction(TVarData(LValue),TVarData(LRow),'_',LArguments));
  AssertFalse(TDataRow(LHandler).DoFunction(TVarData(LValue),TVarData(LRow),'',LArguments));
  SetLength(LArguments,1);
  AssertFalse(TDataRow(LHandler).DoFunction(TVarData(LValue),TVarData(LRow),'UnknownMethod',LArguments));
  AssertEquals('Rejected dispatch must leave its destination unchanged',93,Integer(LValue));
  AssertTrue(TDataRow(LHandler).DoFunction(TVarData(LValue),TVarData(LRow),'_',LArguments));
  AssertEquals(17,Integer(LValue));
  LValue := 93;
  AssertTrue(TDataRow(LHandler).DoFunction(TVarData(LValue),TVarData(LRow),'',LArguments));
  AssertEquals(17,Integer(LValue));
end;

procedure TDataSourceSnapshotTests.UnicodeColumnNamesBindingsAndValuesRoundTrip;
var
  LSource: TUnicodeBindingSource;
  LTable: TDataTable;
  LFilter: TFilterCriteria;
  LData, LCopiedData: TDataRowData;
  LCopy: Variant;
  LResult: TPageFetchResult;
begin
  LSource := TUnicodeBindingSource.Create(nil);
  try
    SetLength(LFilter, 1);
    LFilter[0] := Default(TColumnFilter);
    LFilter[0].ColumnName := UnicodeBinding(False);
    LFilter[0].Filter := vgfSortable;
    LFilter[0].Sort := sdAscending;
    LResult := LSource.FetchPage(TPageFetchParams.Create(0, 1, LFilter, foAnd), LTable);
    AssertEquals(2, LResult.TotalDataCount);
    AssertTrue('First Unicode column name', LTable.Columns[0].Name = UnicodeBinding(False));
    AssertTrue('Distinct Unicode column name', LTable.Columns[1].Name = UnicodeBinding(True));
    LData := TDataRowData(LTable.Rows[0]);
    AssertTrue(LData.HasData(UnicodeBinding(False)) and LData.HasData(UnicodeBinding(True)));
    AssertEquals('Sort by Unicode binding selects first item', 1, Integer(LData[UnicodeBinding(False)]));
    AssertTrue('Unicode field value survives dehydration',
      VarToWideStr(LData[UnicodeBinding(True)]) = UnicodePayload + UnicodeString('1'));
    AssertEquals(1, Integer(LData['__KEY']));

    LCopy := LTable.Rows[0];
    LCopiedData := TDataRowData(LCopy);
    LCopiedData[UnicodeBinding(True)] := UnicodePayload + UnicodeString(' copy');
    AssertTrue('Copied Unicode field remains independent',
      VarToWideStr(LData[UnicodeBinding(True)]) = UnicodePayload + UnicodeString('1'));
    AssertTrue(VarToWideStr(LCopiedData[UnicodeBinding(True)]) =
      UnicodePayload + UnicodeString(' copy'));
  finally
    LSource.Free;
  end;
end;

initialization
  RegisterTest(TDataSourceSnapshotTests);

end.
