{
  Copyright (c) 2017 Sphere 10 Software

  Common unit usable across all tiers.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld
    Maciej Izak (hnb)
}

unit UCommon;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults,
  Variants, LazUTF8;

{ GLOBAL FUNCTIONS }

{ Converts a string to hexidecimal format }
function String2Hex(const Buffer: AnsiString): AnsiString;

{ Binary-safe StrComp replacement. StrComp will return 0 for when str1 and str2 both start with NUL character. }
function BinStrComp(const Str1, Str2 : AnsiString): Integer;

{ Ternary operator equivalent of predicate ? (true-val) : (false-value) }
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Cardinal): Cardinal; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Integer): Integer; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Int64): Int64; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: UInt64): UInt64; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Double): Double; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: string): string; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: TObject): TObject; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: variant): variant; overload;

{ Clip Value }
function ClipValue( AValue, MinValue, MaxValue: Integer) : Integer;

{ DateTime functions }
function TimeStamp : AnsiString;
function UtcTimeStamp : AnsiString;

type
  { TBox - a generic wrapper class for wrappying any type, mainly strings and primtives }
  TBox<T> = class(TObject)
    type
      TDestroyItemDelegate = procedure (constref val : T) of object;
    strict private
      FValue: T;
      FDestroyFunc : TDestroyItemDelegate;
      class procedure NoOpDestroyItem(constref val : T);
    public
      constructor Create(Value: T); overload;
      constructor Create(Value: T; destroyItemFunc: TDestroyItemDelegate); overload;
      destructor Destroy; override;
      property Value: T read FValue;
  end;

  { A TObject-wrapped string }
  TStringObject = TBox<AnsiString>;

  { TArrayTool }
  TArrayTool<T> = class
    public
      class function Contains(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>; out ItemIndex: SizeInt): Boolean; overload; static;
      class function Contains(const Values: TArray<T>; const Item: T; out ItemIndex: SizeInt): Boolean; overload; static;
      class function Contains(const Values: TArray<T>; const Item: T) : Boolean; overload; static;
      class function IndexOf(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>): SizeInt; overload; static;
      class function IndexOf(const Values: TArray<T>; const Item: T): SizeInt; overload; static;
      class procedure Add(var Values: TArray<T>; const AValue : T); static;
      class procedure Remove(var Values : TArray<T>; const Item : T; const Comparer : IEqualityComparer<T>); overload; static;
      class procedure Remove(var Values : TArray<T>; const Item : T); overload; static;
      class procedure RemoveAt(var Values : TArray<T>; ItemIndex : SizeInt); static;
      class procedure Append(var Arr: TArray<T>; Value: T);
      class procedure Prepend(var Arr: TArray<T>; Value: T);
      class procedure InsertAt(var Values : TArray<T>; ItemIndex : SizeInt; const Item : T);
      class procedure Swap(var Values : array of T; Item1Index, Item2Index : SizeInt);
      class procedure MoveItem(var Values : array of T; FromIndex, ToIndex : SizeInt);
      class function Concat(const Arrays: array of TArray<T>): TArray<T>; static;
      class function Create(const a : T; const b : T) : TArray<T>; static;
      class function ToArray(Enumerable: TEnumerable<T>; Count: SizeInt): TArray<T>; static;
    end;

  { TNotifyManyEvent - support for multiple listeners }
  TNotifyManyEvent = TArray<TNotifyEvent>;

  { Helper for TNotifyManyEvent }
  TNotifyManyEventHelper = record helper for TNotifyManyEvent
    procedure Add(listener : TNotifyEvent);
    procedure Remove(listener : TNotifyEvent);
    procedure Invoke(sender : TObject);
  end;

  { TTable types }

  TTableColumns = TArray<utf8string>;
  PTableColumns = ^TTableColumns;
  ETableRow = class(Exception);

  { TTableRow }

  TTableRow = class(TInvokeableVariantType)
  private
    class constructor Create;
    class destructor Destroy;
  protected type
    TColumnMapToIndex = TDictionary<utf8string, Integer>;
    TColumnsDictionary = TObjectDictionary<PTableColumns, TColumnMapToIndex>;
  protected class var
    FColumns: TColumnsDictionary;
  protected
    class function MapColumns(AColumns: PTableColumns): TColumnMapToIndex;
  public
    function GetProperty(var Dest: TVarData; const V: TVarData; const Name: string): Boolean; override;
    function SetProperty(var V: TVarData; const Name: string; const Value: TVarData): Boolean; override;
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData; const Name: string; const Arguments: TVarDataArray): Boolean; override;
    class function New(AColumns: PTableColumns): Variant;
  end;

  TTableRowData = packed record
  public
    vtype: tvartype;
  private
    vfiller1 : word;
    vfiller2: int32;
  public
    vcolumnmap: TTableRow.TColumnMapToIndex;
    vvalues: TArray<Variant>;
  end;

  TExpressionKind = (ekUnknown, ekText, ekNum, ekSet);
  TTextMatchKind = (tmkUnknown, tmkMatchTextExact, tmkMatchTextBeginning, tmkMatchTextEnd, tmkMatchTextAnywhere);
  TNumericComparisionKind = (nckUnknown, nckNumericEQ, nckNumericLT, nckNumericLTE, nckNumericGT, nckNumericGTE);
  TSetKind = (skUnknown, skNumericBetweenInclusive, skNumericBetweenExclusive);

  TExpressionRecord = record
    Values: array of utf8string;
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

resourcestring
  sNotImplemented = 'Not implemented';
  sInvalidParameter_OutOfBounds = 'Invalid Parameter: %s out of bounds';
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

var
  TableRowType: TTableRow = nil;

{%region Global functions %}

function String2Hex(const Buffer: AnsiString): AnsiString;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Buffer) do
    Result := LowerCase(Result + IntToHex(Ord(Buffer[n]), 2));
end;


function BinStrComp(const Str1, Str2: AnsiString): integer;
var Str1Len, Str2Len, i : Integer;
begin
   Str1Len := Length(Str1);
   Str2Len := Length(Str2);
   if (Str1Len < Str2Len) then
     Result := -1
   else if (Str1Len > Str2Len) then
     Result := 1
   else begin
     Result := 0;
     For i:= 1 to Str1Len do begin
       if Str1[i] < Str2[i] then begin
         Result := -1;
         break;
       end else if Str1[i] > Str2[i] then begin
         Result := 1;
         break;
       end
     end;
   end;
End;

{%endregion}

{%region Language-level tools }
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Cardinal): Cardinal;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Integer): Integer;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Int64): Int64;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: UInt64): UInt64;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Double): Double;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: string): string;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: TObject): TObject;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: variant): variant;
begin
  if ACondition then
    Result := ATrueResult
  else
    Result := AFalseResult;
end;

{ Clip Value }

function ClipValue( AValue, MinValue, MaxValue: Integer) : Integer;
begin
  if AValue < MinValue then
    Result := MinValue
  else if AValue > MaxValue then
    Result := MaxValue
  else
    Result := AValue
end;


{ DateTime functions }
function TimeStamp : AnsiString;
begin
  Result := FormatDateTime('yyy-mm-dd hh:nn:ss', Now);
end;

function UtcTimeStamp : AnsiString;
begin
  raise Exception.Create(sNotImplemented);
end;

{%endregion}

{%region TBox }

constructor TBox<T>.Create(Value: T);
begin
  Create(Value, NoOpDestroyItem);
end;

constructor TBox<T>.Create(Value: T; destroyItemFunc: TDestroyItemDelegate);
begin
  inherited Create;
  FValue := Value;
  FDestroyFunc := destroyItemFunc;
end;

destructor TBox<T>.Destroy;
begin
  FDestroyFunc(FValue);
  inherited;
end;

class procedure TBox<T>.NoOpDestroyItem(constref val : T);
begin
  // No op
end;

{%endregion}

{%region TArrayTool }

class function TArrayTool<T>.Contains(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>; out ItemIndex: SizeInt): Boolean;
var
  Index: SizeInt;
begin
  for Index := 0 to high(Values) do begin
    if Comparer.Equals(Values[Index], Item) then begin
      ItemIndex := Index;
      Result := True;
      exit;
    end;
  end;
  ItemIndex := -1;
  Result := False;
end;

class function TArrayTool<T>.Contains(const Values: TArray<T>; const Item: T; out ItemIndex: SizeInt): Boolean;
begin
  Result := TArrayTool<T>.Contains(Values, Item, TEqualityComparer<T>.Default, ItemIndex);
end;

class function TArrayTool<T>.Contains(const Values: TArray<T>; const Item: T): Boolean;
var
  ItemIndex: SizeInt;
begin
  Result := TArrayTool<T>.Contains(Values, Item, ItemIndex);
end;

class function TArrayTool<T>.IndexOf(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>): SizeInt;
begin
  TArrayTool<T>.Contains(Values, Item, Comparer, Result);
end;

class function TArrayTool<T>.IndexOf(const Values: TArray<T>; const Item: T): SizeInt;
begin
  TArrayTool<T>.Contains(Values, Item, Result);
end;

class procedure TArrayTool<T>.Add(var Values: TArray<T>; const AValue : T);
begin
  SetLength(Values, SizeInt(Length(Values)) + 1);
  Values[High(Values)] := AValue;
end;

class procedure TArrayTool<T>.Remove(var Values : TArray<T>; const Item : T; const Comparer : IEqualityComparer<T>);
var index : SizeInt;
begin
  while TArrayTool<T>.Contains(Values, item, Comparer, index) do begin
    TArrayTool<T>.RemoveAt(Values, index);
  end;
end;

class procedure TArrayTool<T>.Remove(var Values : TArray<T>; const Item : T);
begin
  TArrayTool<T>.Remove(Values, Item, TEqualityComparer<T>.Default);
end;

class procedure TArrayTool<T>.RemoveAt(var Values : TArray<T>; ItemIndex : SizeInt);
var i : Integer;
begin
  for i := ItemIndex + 1 to High(Values) do
    Values[i - 1] := Values[i];
  SetLength(Values, Length(Values) - 1);
end;

class procedure TArrayTool<T>.Append(var Arr: TArray<T>; Value: T);
begin
  SetLength(Arr, Length(Arr)+1);
  Arr[High(Arr)] := Value;
end;

class procedure TArrayTool<T>.Prepend(var Arr: TArray<T>; Value: T);
var i : Integer;
begin
  SetLength(Arr, Length(Arr)+1);
  for i := High(Arr)-1 downto Low(Arr) do
    Arr[i+1] := Arr[i];
  Arr[Low(Arr)] := Value;
end;

class procedure TArrayTool<T>.InsertAt(var Values : TArray<T>; ItemIndex : SizeInt; const Item : T);
var i : Integer;
begin
  if (ItemIndex < Low(Values)) OR (ItemIndex > High(Values)) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['ItemIndex']);
  SetLength(Values, Length(Values)+1);
  for i := High(Values)-1 downto ItemIndex do
    Values[i+1] := Values[i];
  Values[ItemIndex] := Item;
end;

class procedure TArrayTool<T>.Swap(var Values : array of T; Item1Index, Item2Index : SizeInt);
var temp : T; len, recSize : SizeInt; itemSize : SizeInt;
begin
  len := Length(Values);
  recSize := SizeOf(T);
  if (Item1Index < 0) OR (Item1Index > len) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['Item1Index']);
  if (Item2Index < 0) OR (Item2Index > len) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['Item2Index']);
  temp := Values[Item1Index];
  Values[Item1Index] := Values[Item2Index];
  Values[Item2Index] := temp;
end;

class procedure TArrayTool<T>.MoveItem(var Values : array of T; FromIndex, ToIndex : SizeInt);
var i : Integer; item : T;
begin
  if (FromIndex < Low(Values)) OR (FromIndex > High(Values)) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['FromIndex']);
  if (ToIndex < Low(Values)) OR (ToIndex > High(Values)) then Raise Exception.CreateFmt(sInvalidParameter_OutOfBounds, ['ToIndex']);

  item := Values[FromIndex];
  if FromIndex < ToIndex then begin
    for i := FromIndex + 1 to ToIndex do
      Values[i - 1] := Values[i];
    Values[ToIndex] := item;
  end else if FromIndex > ToIndex then begin
    for i := FromIndex - 1 downto ToIndex do
      Values[i + 1] := Values[i];
    Values[ToIndex] := item;
  end;
end;

class function TArrayTool<T>.Concat(const Arrays: array of TArray<T>): TArray<T>;
var
  i, k, LIndex, LLength: Integer;
begin
  LLength := 0;
  for i := 0 to High(Arrays) do
    Inc(LLength, Length(Arrays[i]));
  SetLength(Result, LLength);
  LIndex := 0;
  for i := 0 to High(Arrays) do
  begin
    for k := 0 to High(Arrays[i]) do
    begin
      Result[LIndex] := Arrays[i][k];
      Inc(LIndex);
    end;
  end;
end;

class function TArrayTool<T>.Create(const a, b: T): TArray<T>;
begin
  SetLength(result,2);
  result[0] := a;
  result[1] := b;
end;

class function TArrayTool<T>.ToArray(Enumerable: TEnumerable<T>; Count: SizeInt): TArray<T>;
var
  LItem: T;
begin
  SetLength(Result, Count);
  Count := 0;
  for LItem in Enumerable do
  begin
    Result[Count] := LItem;
    Inc(Count);
  end;
end;

{%endregion}

{ TTableRow }

class constructor TTableRow.Create;
begin
  FColumns := TColumnsDictionary.Create([doOwnsValues]);
end;

class destructor TTableRow.Destroy;
begin
  FColumns.Free;
end;

class function TTableRow.MapColumns(AColumns: PTableColumns): TColumnMapToIndex;
var
  i: Integer;
begin
  Result := TColumnMapToIndex.Create;
  for i := 0 to High(AColumns^) do
    Result.Add(AColumns^[i], i);
  FColumns.Add(AColumns, Result);
end;

function TTableRow.GetProperty(var Dest: TVarData;
  const V: TVarData; const Name: string): Boolean;
var
  LRow: TTableRowData absolute V;
begin
  Variant(Dest) := LRow.vvalues[LRow.vcolumnmap[Name]];
  Result := true;
end;

function TTableRow.SetProperty(var V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
var
  LRow: TTableRowData absolute V;
begin
  LRow.vvalues[LRow.vcolumnmap[Name]] := Variant(Value);
  Result := true;
end;

procedure TTableRow.Clear(var V: TVarData);
begin
  Finalize(TTableRowData(V));
  FillChar(V, SizeOf(V), #0);
end;

procedure TTableRow.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
var
  LDestRow: TTableRowData absolute Dest;
  LSourceRow: TTableRowData absolute Source;
begin
  if Indirect then
    SimplisticCopy(Dest,Source,true)
  else
  begin
    VarClear(variant(Dest));
    FillChar(LDestRow, SizeOf(LDestRow), #0);
    LDestRow.vtype := LSourceRow.vtype;
    LDestRow.vcolumnmap := LSourceRow.vcolumnmap;
    LDestRow.vvalues := system.copy(TTableRowData(LSourceRow).vvalues);
  end;
end;

function TTableRow.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  LRow: TTableRowData absolute V;
begin
  Result := (Name = '_') and (Length(Arguments)=1);
  if Result then
    Variant(Dest) := LRow.vvalues[Variant(Arguments[0])];
end;

class function TTableRow.New(AColumns: PTableColumns): Variant;
var
  LColumnMap: TColumnMapToIndex;
begin
  if not Assigned(AColumns) then
    raise ETableRow.Create(sAColumnsCantBeNil);

  VarClear(Result);
  FillChar(Result, SizeOf(Result), #0);
  TTableRowData(Result).vtype:=TableRowType.VarType;

  if not FColumns.TryGetValue(AColumns, LColumnMap) then
    LColumnMap := MapColumns(AColumns);

  TTableRowData(Result).vcolumnmap:=LColumnMap;
  SetLength(TTableRowData(Result).vvalues, Length(AColumns^));
end;

{%region TNotifyManyEventHelper}

procedure TNotifyManyEventHelper.Add(listener : TNotifyEvent);
begin
  if TArrayTool<TNotifyEvent>.IndexOf(self, listener) = -1 then begin
    TArrayTool<TNotifyEvent>.Add(self, listener);
  end;
end;

procedure TNotifyManyEventHelper.Remove(listener : TNotifyEvent);
begin
  TArrayTool<TNotifyEvent>.Remove(self, listener);
end;

procedure TNotifyManyEventHelper.Invoke(sender : TObject);
var i : Integer;
begin
  for i := 0 to high(self) do
    self[i](sender);
end;

{%endregion}

{ TSearchExpressionService }

class procedure TSearchExpressionService.Parse(const AExpression: utf8string;
  const AExpressionKind: TExpressionKind; out
  AExpressionRecord: TExpressionRecord);
const
  MAX_VALUES = 2;
type
  TToken = (tkNone, tkPercent, tkLess, tkGreater, tkEqual, tkLessOrEqual,
    tkGreaterOrEqual, tkOpeningParenthesis, tkClosingParenthesis,
    tkOpeningBracket, tkClosingBracket,  tkText, tkNum, tkComma);

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
              ekUnknown, ekSet, ekNum: LToken:=tkNum;
              ekText: LToken:=tkText;
            end;
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
                  AExpressionRecord.TextMatchKind:=tmkMatchTextEnd
                else
                  AExpressionRecord.TextMatchKind:=tmkMatchTextAnywhere;
                LLastPercent:=true;
              end;
            tkNone:
              AExpressionRecord.TextMatchKind:=tmkMatchTextBeginning;
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
end;

class function TSearchExpressionService.Parse(const AExpression: utf8string
  ): TExpressionRecord;
begin
  Result.Kind := ekUnknown;
  Parse(AExpression, Result.Kind, Result);
end;


initialization
  TableRowType := TTableRow.Create;
finalization
  TableRowType.Free;
end.

