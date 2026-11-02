program DataRowVariantProbe;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE. }

{$APPTYPE CONSOLE}

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Variants,
  System.Generics.Collections, Vcl.Forms, Vcl.Controls,
  UCommon, UCommon.Data, UVisualGrid, UWizardStringChecks;

type
  TRowSchema = class end;

  TNativeRows = class(TCustomDataSource<Integer>)
  protected
    function GetColumns: TDataColumns; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
    function GetItemField(const AItem: Integer;
      const ABindingName: String): Variant; override;
  end;

  TUnicodeRows = class(TCustomDataSource<Integer>)
  protected
    function GetColumns: TDataColumns; override;
    procedure FetchAll(const AContainer: TList<Integer>); override;
    function GetItemField(const AItem: Integer;
      const ABindingName: String): Variant; override;
  end;

function UnicodeBinding(ASecond: Boolean): String;
begin
  if ASecond then Result := #$65E5#$672C#$D83D#$DE80
  else Result := #$4E2D#$6587#$D83D#$DE80;
end;

function UnicodePayload: String;
begin
  Result := #$03A9#$0416#$6F22#$D83D#$DE80;
end;

function TUnicodeRows.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From(UnicodeBinding(False), True),
    TDataColumn.From(UnicodeBinding(True)));
end;

procedure TUnicodeRows.FetchAll(const AContainer: TList<Integer>);
begin
  AContainer.Add(2);
  AContainer.Add(1);
end;

function TUnicodeRows.GetItemField(const AItem: Integer;
  const ABindingName: String): Variant;
begin
  if ABindingName = UnicodeBinding(False) then Result := AItem
  else if ABindingName = UnicodeBinding(True) then Result := UnicodePayload + IntToStr(AItem)
  else raise Exception.Create('Unicode binding changed before native field lookup');
end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function TNativeRows.GetColumns: TDataColumns;
begin
  Result := TDataColumns.Create(TDataColumn.From('Value', True),
    TDataColumn.From('Title'));
end;

procedure TNativeRows.FetchAll(const AContainer: TList<Integer>);
begin
  AContainer.Add(17);
  AContainer.Add(29);
end;

function TNativeRows.GetItemField(const AItem: Integer;
  const ABindingName: String): Variant;
begin
  if ABindingName = 'Value' then Result := AItem
  else if ABindingName = 'Title' then Result := 'row-' + IntToStr(AItem)
  else raise Exception.Create('Unexpected native probe column');
end;

procedure CheckPositionalRows(ALog: TStrings);
var LRow, LCopy, LTemporary: Variant; LIndex, LImplicitInteger: Integer;
begin
  LRow := TDataRow.New(PtrInt(TRowSchema), TDataColumns.Create(
    TDataColumn.From('Value', True), TDataColumn.From('Title')));
  LRow.Value := 17;
  LRow.Title := 'saved';
  LRow.__KEY := 29;

  Require(Integer(LRow._(0)) = 17, 'Explicit Integer positional conversion failed');
  ALog.Add('PASS explicit Integer(Row._(0))');
  LTemporary := LRow._(0);
  Require(VarIsNumeric(LTemporary) and (Integer(LTemporary) = 17),
    'Positional Variant temporary failed');
  ALog.Add('PASS Variant temporary := Row._(0)');
  LImplicitInteger := LRow._(0);
  Require(LImplicitInteger = 17, 'Implicit positional Integer assignment failed');
  ALog.Add('PASS implicit Integer assignment := Row._(0)');
  LIndex := 0;
  Require(Integer(LRow._(LIndex)) = 17, 'Runtime Integer positional index failed');
  LIndex := 1;
  Require(String(LRow._(LIndex)) = 'saved', 'Runtime index selected the wrong column');
  ALog.Add('PASS runtime Integer index and text positional value');

  Require((Integer(LRow.Value) = 17) and (String(LRow.Title) = 'saved'),
    'Named columns changed during positional access');
  Require((Integer(LRow.__KEY) = 29) and (Integer(LRow._(2)) = 29),
    'Independent key slot must follow declared columns');
  LRow.__KEY := 31;
  Require((Integer(LRow.Value) = 17) and (Integer(LRow._(0)) = 17),
    'Changing the key changed the first declared column');
  ALog.Add('PASS named columns and independent positional key slot');

  LCopy := LRow;
  LCopy.Value := 71;
  LCopy.Title := 'copied';
  LCopy.__KEY := 97;
  Require((Integer(LRow._(0)) = 17) and (String(LRow._(1)) = 'saved') and
    (Integer(LRow.__KEY) = 31), 'Variant copy modified the original row');
  Require((Integer(LCopy._(0)) = 71) and (String(LCopy._(1)) = 'copied') and
    (Integer(LCopy.__KEY) = 97), 'Copied row values did not round trip');
  ALog.Add('PASS independent copied row values and key');
end;

procedure CheckNativeGridCells(ALog: TStrings);
var LForm: TForm; LSource: TNativeRows; LGrid: TVisualGrid;
begin
  LForm := TForm.CreateNew(nil);
  try
    LForm.SetBounds(80, 90, 640, 440);
    LSource := TNativeRows.Create(LForm);
    LGrid := TVisualGrid.Create(LForm);
    LGrid.Parent := LForm;
    LGrid.Align := alClient;
    LGrid.FetchDataInThread := False;
    LGrid.AutoPageSize := False;
    LGrid.PageSize := 2;
    LGrid.AddColumn('Value').Binding := 'Value';
    LGrid.AddColumn('Title').Binding := 'Title';
    LGrid.DataSource := LSource;
    Require(LGrid.RowCount = 2, 'Native grid did not fetch its two fixture rows');
    Require((Integer(LGrid.Cells[0, 0]) = 17) and
      (String(LGrid.Cells[1, 0]) = 'row-17') and
      (Integer(LGrid.Cells[0, 1]) = 29) and
      (String(LGrid.Cells[1, 1]) = 'row-29'),
      'Actual TVisualGrid.Cells positional getter failed');
    Require((Integer(LGrid.Rows[0].__KEY) = 17) and
      (Integer(LGrid.Rows[1].__KEY) = 29), 'Native grid source keys changed');
    ALog.Add('PASS actual synchronous VCL TVisualGrid.Cells columns/rows and keys');
  finally
    LForm.Free;
  end;
end;

procedure CheckUnicodeBindingsAndCells(ALog: TStrings);
var
  LForm: TForm;
  LSource: TUnicodeRows;
  LGrid: TVisualGrid;
  LColumn: TVisualColumn;
  LRow, LCopy: Variant;
  LData, LCopyData: TDataRowData;
begin
  LForm := TForm.CreateNew(nil);
  try
    LForm.SetBounds(80, 90, 640, 440);
    LSource := TUnicodeRows.Create(LForm);
    LGrid := TVisualGrid.Create(LForm);
    LGrid.Parent := LForm;
    LGrid.Align := alClient;
    LGrid.FetchDataInThread := False;
    LGrid.AutoPageSize := False;
    LGrid.PageSize := 2;
    LGrid.SortMode := smNone;
    LColumn := LGrid.AddColumn(UnicodeBinding(False));
    LColumn.Binding := UnicodeBinding(False);
    LColumn.SortBinding := UnicodeBinding(False);
    LColumn.DisplayBinding := UnicodeBinding(False);
    LGrid.AddColumn(UnicodeBinding(True)).Binding := UnicodeBinding(True);
    Require((LColumn.Name = UnicodeBinding(False)) and
      (LColumn.Binding = UnicodeBinding(False)) and
      (LColumn.SortBinding = UnicodeBinding(False)) and
      (LColumn.DisplayBinding = UnicodeBinding(False)),
      'Native String column name or bindings lost Unicode characters');
    LGrid.DataSource := LSource;
    Require((LGrid.RowCount = 2) and (Integer(LGrid.Cells[0, 0]) = 2) and
      (String(LGrid.Cells[1, 0]) = UnicodePayload + '2'),
      'Actual Unicode-bound native grid values failed');
    LRow := LGrid.Rows[0];
    LData := TDataRowData(LRow);
    Require(LData.HasData(UnicodeBinding(False)) and LData.HasData(UnicodeBinding(True)),
      'Unicode column map lost distinct names');
    Require((Integer(LData[UnicodeBinding(False)]) = 2) and
      (String(LData[UnicodeBinding(True)]) = UnicodePayload + '2') and
      (Integer(LData['__KEY']) = 2), 'Unicode-bound row fields/key failed');
    LCopy := LRow;
    LCopyData := TDataRowData(LCopy);
    LCopyData[UnicodeBinding(True)] := UnicodePayload + ' copy';
    Require((String(LData[UnicodeBinding(True)]) = UnicodePayload + '2') and
      (String(LCopyData[UnicodeBinding(True)]) = UnicodePayload + ' copy'),
      'Copied Unicode-bound row changed the original');
    ALog.Add('PASS native String Unicode column names/bindings and actual VCL grid cells');
    ALog.Add('PASS Unicode row map/value/key and independent copied row');
  finally
    LForm.Free;
  end;
end;

var LLog: TStringList; LLogPath: String;
begin
  if ParamCount > 1 then Halt(64);
  LLogPath := ParamStr(1);
  if LLogPath = '' then
    LLogPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
      'DataRowVariantProbe-' + IntToStr(GetCurrentProcessId) + '.log';
  ReportMemoryLeaksOnShutdown := True;
  LLog := TStringList.Create;
  try
    try
      Application.Initialize;
      CheckPositionalRows(LLog);
      CheckNativeGridCells(LLog);
      CheckUnicodeBindingsAndCells(LLog);
      CheckNativeWizardStrings(LLog);
      LLog.Add('PASS all native positional Variant and grid compatibility checks');
      ExitCode := 0;
    except
      on E: Exception do begin
        LLog.Add('FAIL ' + E.ClassName + ': ' + E.Message);
        ExitCode := 1;
      end;
    end;
    LLog.SaveToFile(LLogPath);
    WriteLn(LLog.Text);
  finally
    LLog.Free;
  end;
end.
