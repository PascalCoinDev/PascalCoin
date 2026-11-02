unit UVisualGrid.Tests;

{ Copyright © 2026 Herman Schoenfeld
  Distributed under the MIT software license; see LICENSE. }

{$mode delphi}

interface

uses
  Classes, SysUtils, Types, Variants, Forms, Grids, Generics.Collections,
  fpcunit, testregistry, UCommon.Data, UVisualGrid;

type
  TVisualGridSelectionTests = class(TTestCase)
  private
    FForm: TForm;
    FGrid: TVisualGrid;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DisjointAndOverlappingRangesReturnUniqueRows;
    procedure ClearSelectionWithoutHandlerReturnsNoRows;
  end;

implementation

type
  TSelectionSource = class(TCustomDataSource<Integer>)
  protected
    function GetColumns: TDataColumns; override;
    function GetItemField(constref AItem: Integer;
      const AColumnName: String): Variant; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
  end;

  TGridSelectionAccess = class(Grids.TDrawGrid)
  public
    procedure RetainRange;
  end;

function TSelectionSource.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Id',True));
end;

function TSelectionSource.GetItemField(constref AItem: Integer;
  const AColumnName: String): Variant;
begin
  Result := AItem;
end;

procedure TSelectionSource.FetchAll(const AContainer: TList<Integer>);
var i: Integer;
begin
  for i := 0 to 4 do AContainer.Add(i);
end;

procedure TGridSelectionAccess.RetainRange;
begin
  AddSelectedRange;
end;

procedure TVisualGridSelectionTests.SetUp;
var LSource: TSelectionSource;
begin
  inherited SetUp;
  FForm := TForm.CreateNew(nil);
  LSource := TSelectionSource.Create(FForm);
  FGrid := TVisualGrid.Create(FForm);
  FGrid.Parent := FForm;
  FGrid.FetchDataInThread := False;
  FGrid.AutoPageSize := False;
  FGrid.PageSize := 5;
  FGrid.AddColumn('Id').Binding := 'Id';
  FGrid.DataSource := LSource;
  // The first source assignment establishes the paging and search layout.
  FGrid.PageSize := 5;
  FGrid.SelectionType := stMultiRow;
  FGrid.DeselectionType := dtNone;
  FGrid.InternalDrawGrid.RangeSelectMode := rsmMulti;
end;

procedure TVisualGridSelectionTests.TearDown;
begin
  FForm.Free;
  inherited TearDown;
end;

procedure TVisualGridSelectionTests.DisjointAndOverlappingRangesReturnUniqueRows;
var LRows: TArray<Variant>;
begin
  FGrid.InternalDrawGrid.Selection := Rect(0,1,0,2);
  TGridSelectionAccess(FGrid.InternalDrawGrid).RetainRange;
  FGrid.InternalDrawGrid.Selection := Rect(0,2,0,3);
  TGridSelectionAccess(FGrid.InternalDrawGrid).RetainRange;
  FGrid.InternalDrawGrid.Selection := Rect(0,5,0,5);
  AssertEquals('Fetched five rows',5,FGrid.RowCount);
  AssertEquals('Native grid has a header and five rows',6,FGrid.InternalDrawGrid.RowCount);
  AssertEquals('Current range reaches fifth data row',5,FGrid.InternalDrawGrid.Selection.Bottom);
  LRows := FGrid.SelectedRows;
  AssertEquals('Each selected row appears once',4,Length(LRows));
  AssertEquals(0,Integer(TDataRowData(LRows[0])['Id']));
  AssertEquals(1,Integer(TDataRowData(LRows[1])['Id']));
  AssertEquals(2,Integer(TDataRowData(LRows[2])['Id']));
  AssertEquals(4,Integer(TDataRowData(LRows[3])['Id']));
end;

procedure TVisualGridSelectionTests.ClearSelectionWithoutHandlerReturnsNoRows;
begin
  AssertFalse('No client selection callback is installed',Assigned(FGrid.OnSelection));
  FGrid.SelectionType := stRow;
  FGrid.InternalDrawGrid.Row := 2;
  AssertEquals('A row is selected before clearing',1,Length(FGrid.SelectedRows));
  FGrid.ClearSelection(True);
  AssertEquals('The retained native cursor is not a selected row',0,Length(FGrid.SelectedRows));
end;

initialization
  RegisterTest(TVisualGridSelectionTests);

end.
