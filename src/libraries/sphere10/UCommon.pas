{
  Copyright (c) 2017 - 2018 Sphere 10 Software

  Common unit usable across all tiers.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld
}

unit UCommon;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults,
  Variants, LazUTF8, math, typinfo, UAutoScope;

{ CONSTANTS }

const
  MillisPerSecond = 1000;
  MillisPerMinute = 60 * MillisPerSecond;
  MillisPerHour = 60 * MillisPerMinute;
  MillisPerDay = 24 * MillisPerHour;
  MaxMilliseconds = High(Int64);
  MinMilliseconds = Low(Int64);
  MaxSeconds = MaxMilliseconds div 60;
  MinSeconds = MinMilliseconds div 60;


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

function GetSetName(const aSet:PTypeInfo; Value: Integer):string;
function GetSetValue(const aSet:PTypeInfo; Name: String): Integer;

{ Clip/Min/Max Value }
function ClipValue( AValue, MinValue, MaxValue: Integer) : Integer;
function MinValue(const AArray : array of Cardinal) : Cardinal;

{ DateTime functions }
function TimeStamp : AnsiString;
function UtcTimeStamp : AnsiString;

type

  {$IFDEF FPC}
  { TTimeSpan }

  TTimeSpan = record
    // Naive implementation - only accurate to millisecond scale, needs updating to tick scale.
    private
      FMillis: Int64;
    strict private
      class constructor Create;
      function GetDays: Integer;
      function GetHours: Integer;
      function GetMinutes: Integer;
      function GetSeconds: Integer;
      function GetMilliseconds: Integer;
      function GetTotalDays: Double;
      function GetTotalHours: Double;
      function GetTotalMinutes: Double;
      function GetTotalSeconds: Double;
      function GetTotalMilliseconds: Double;
      class function Normalize(const ADateTime : TDateTime) : Int64; inline; static;
    public
      constructor Create(Hours, Minutes, Seconds: Integer); overload;
      constructor Create(Days, Hours, Minutes, Seconds: Integer); overload;
      constructor Create(Days, Hours, Minutes, Seconds, Milliseconds: Integer); overload;
      function Add(const TS: TTimeSpan): TTimeSpan; overload;
      function Duration: TTimeSpan;
      function Negate: TTimeSpan;
      function Subtract(const TS: TTimeSpan): TTimeSpan; overload;
      function ToString: string;
      class function FromDays(Value: Double): TTimeSpan; static;
      class function FromHours(Value: Double): TTimeSpan; static;
      class function FromMinutes(Value: Double): TTimeSpan; static;
      class function FromSeconds(Value: Double): TTimeSpan; static;
      class function FromMilliseconds(Value: Double): TTimeSpan; static;
      class function Subtract(const D1, D2: TDateTime): TTimeSpan; overload; static;
      class function Parse(const S: string): TTimeSpan; static;
      class function TryParse(const S: string; out Value: TTimeSpan): Boolean; static;
      class operator +(const Left, Right: TTimeSpan) ATimeSpan : TTimeSpan;
      class operator +(const Left: TTimeSpan; Right: TDateTime) ADateTime: TDateTime;
      class operator +(const Left: TDateTime; Right: TTimeSpan) ADateTime: TDateTime;
      class operator -(const Left, Right: TTimeSpan) ATimeSpan : TTimeSpan;
      class operator -(const Left: TDateTime; Right: TTimeSpan) ADateTime: TDateTime;
      class operator =(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator <>(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator >(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator >=(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator <(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator <=(const Left, Right: TTimeSpan) ABool : Boolean;
      class operator Negative(const Value: TTimeSpan) ATimeSpan: TTimeSpan;
      class operator Positive(const Value: TTimeSpan) ATimeSpan : TTimeSpan;
      class operator Implicit(const Value: TTimeSpan) AString : string;
      class operator Explicit(const Value: TTimeSpan) AString : string;
      property Days: Integer read GetDays;
      property Hours: Integer read GetHours;
      property Minutes: Integer read GetMinutes;
      property Seconds: Integer read GetSeconds;
      property Milliseconds: Integer read GetMilliseconds;
      property TotalDays: Double read GetTotalDays;
      property TotalHours: Double read GetTotalHours;
      property TotalMinutes: Double read GetTotalMinutes;
      property TotalSeconds: Double read GetTotalSeconds;
      property TotalMilliseconds: Double read GetTotalMilliseconds;
    end;
  {$ENDIF}

  { TAuto }

  TAuto<T> = record
    private
      FGC : TScoped;
      function GetItem : T;
      procedure SetItem(const AItem: T);
    public
      constructor Create(const AItem: T);
      property Item : T read GetItem write SetItem;
  end;

  { TDateTimeHelper }

  TDateTimeHelper = record helper for TDateTime
   private
     function GetDay: Word; inline;
     function GetDate: TDateTime; inline;
     function GetDayOfWeek: Word; inline;
     function GetDayOfYear: Word; inline;
     function GetHour: Word; inline;
     function GetMillisecond: Word; inline;
     function GetMinute: Word; inline;
     function GetMonth: Word; inline;
     function GetSecond: Word; inline;
     function GetTime: TDateTime; inline;
     function GetYear: Word; inline;
     class function GetNow: TDateTime; static; inline;
     class function GetToday: TDateTime; static; inline;
     class function GetTomorrow: TDateTime; static; inline;
     class function GetYesterDay: TDateTime; static; inline;
   public
     class function Create(const aYear, aMonth, aDay: Word): TDateTime; overload; static; inline;
     class function Create(const aYear, aMonth, aDay, aHour, aMinute, aSecond, aMillisecond: Word): TDateTime; overload; static; inline;

     class property Now: TDateTime read GetNow;
     class property Today: TDateTime read GetToday;
     class property Yesterday: TDateTime read GetYesterDay;
     class property Tomorrow: TDateTime read GetTomorrow;

     property Date: TDateTime read GetDate;
     property Time: TDateTime read GetTime;

     property DayOfWeek: Word read GetDayOfWeek;
     property DayOfYear: Word read GetDayOfYear;

     property Year: Word read GetYear;
     property Month: Word read GetMonth;
     property Day: Word read GetDay;
     property Hour: Word read GetHour;
     property Minute: Word read GetMinute;
     property Second: Word read GetSecond;
     property Millisecond: Word read GetMillisecond;

     function ToString(const aFormatStr: string = ''): string; inline;

     function StartOfYear: TDateTime; inline;
     function EndOfYear: TDateTime; inline;
     function StartOfMonth: TDateTime; inline;
     function EndOfMonth: TDateTime; inline;
     function StartOfWeek: TDateTime; inline;
     function EndOfWeek: TDateTime; inline;
     function StartOfDay: TDateTime; inline;
     function EndOfDay: TDateTime; inline;

     function AddYears(const aNumberOfYears: Integer = 1): TDateTime; inline;
     function AddMonths(const aNumberOfMonths: Integer = 1): TDateTime; inline;
     function AddDays(const aNumberOfDays: Integer = 1): TDateTime; inline;
     function AddHours(const aNumberOfHours: Int64 = 1): TDateTime; inline;
     function AddMinutes(const aNumberOfMinutes: Int64 = 1): TDateTime; inline;
     function AddSeconds(const aNumberOfSeconds: Int64 = 1): TDateTime; inline;
     function AddMilliseconds(const aNumberOfMilliseconds: Int64 = 1): TDateTime; inline;

     function CompareTo(const aDateTime: TDateTime): TValueRelationship; inline;
     function Equals(const aDateTime: TDateTime): Boolean; inline;
     function IsSameDay(const aDateTime: TDateTime): Boolean; inline;
     function InRange(const aStartDateTime, aEndDateTime: TDateTime; const aInclusive: Boolean = True): Boolean; inline;
     function IsInLeapYear: Boolean; inline;
     function IsToday: Boolean; inline;
     function IsAM: Boolean; inline;
     function IsPM: Boolean; inline;

     function YearsBetween(const aDateTime: TDateTime): Integer; inline;
     function MonthsBetween(const aDateTime: TDateTime): Integer; inline;
     function WeeksBetween(const aDateTime: TDateTime): Integer; inline;
     function DaysBetween(const aDateTime: TDateTime): Integer; inline;
     function HoursBetween(const aDateTime: TDateTime): Int64; inline;
     function MinutesBetween(const aDateTime: TDateTime): Int64; inline;
     function SecondsBetween(const aDateTime: TDateTime): Int64; inline;
     function MilliSecondsBetween(const aDateTime: TDateTime): Int64; inline;

     function WithinYears(const aDateTime: TDateTime; const aYears: Integer): Boolean; inline;
     function WithinMonths(const aDateTime: TDateTime; const aMonths: Integer): Boolean; inline;
     function WithinWeeks(const aDateTime: TDateTime; const aWeeks: Integer): Boolean; inline;
     function WithinDays(const aDateTime: TDateTime; const aDays: Integer): Boolean; inline;
     function WithinHours(const aDateTime: TDateTime; const aHours: Int64): Boolean; inline;
     function WithinMinutes(const aDateTime: TDateTime; const aMinutes: Int64): Boolean; inline;
     function WithinSeconds(const aDateTime: TDateTime; const aSeconds: Int64): Boolean; inline;
     function WithinMilliseconds(const aDateTime: TDateTime; const AMilliseconds: Int64): Boolean; inline;
   end;


  { TItemDisposePolicy }

  TItemDisposePolicy = (idpNone, idpNil, idpFreeAndNil);

  { Event Support}

  TNotifyEventEx = procedure (sender : TObject; const args: array of Pointer) of object;
  TNotifyManyEvent = TArray<TNotifyEvent>;
  TNotifyManyEventEx = TArray<TNotifyEventEx>;
  TNotifyManyEventHelper = record helper for TNotifyManyEvent
    procedure Add(listener : TNotifyEvent);
    procedure Remove(listener : TNotifyEvent);
    procedure Invoke(sender : TObject);
  end;
  TNotifyManyEventExHelper = record helper for TNotifyManyEventEx
    procedure Add(listener : TNotifyEventEx);
    procedure Remove(listener : TNotifyEventEx);
    procedure Invoke(sender : TObject; const args: array of Pointer);
  end;

  { TArrayTool }

  TArrayTool<T> = class
    public
      class function Empty : TArray<T>;
      class function Contains(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>; out ItemIndex: SizeInt): Boolean; overload; static;
      class function Contains(const Values: TArray<T>; const Item: T; out ItemIndex: SizeInt): Boolean; overload; static;
      class function Contains(const Values: TArray<T>; const Item: T) : Boolean; overload; static;
      class function IndexOf(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>): SizeInt; overload; static;
      class function IndexOf(const Values: TArray<T>; const Item: T): SizeInt; overload; static;
      class function Copy(const AArray: array of T): TArray<T>; overload;
      class function Copy(const AArray: array of T; FromIndex, Count: SizeInt ): TArray<T>; overload;
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
      class function Create(const item0 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T; const item2 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T; const item4 : T) : TArray<T>; overload; static;
      class function Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T; const item4 : T; const item5 : T) : TArray<T>; overload; static;
      class function _Length(const Values: array of T) : SizeInt; static; inline;
      class function ToArray(Enumerable: TEnumerable<T>; Count: SizeInt): TArray<T>; static;
  end;

  { TVariantTool }

  TVariantTool = class
    public
      class function IsNumeric(const AValue : Variant) : boolean;
      class function TryParseBool(const AValue : Variant; out ABoolean : boolean) : boolean;
      class function VarToInt(const AVariant: Variant): integer;
      class function MatchTextExact(const AValue, AMatch : Variant) : boolean;
      class function MatchTextBeginning(const AValue, AMatch : Variant) : boolean;
      class function MatchTextEnd(const AValue, AMatch : Variant) : boolean;
      class function MatchTextAnywhere(const AValue, AMatch : Variant) : boolean;
      class function NumericEQ(const AValue, AMatch : Variant) : boolean;
      class function NumericLT(const AValue, AMatch : Variant) : boolean;
      class function NumericLTE(const AValue, AMatch : Variant) : boolean;
      class function NumericGT(const AValue, AMatch : Variant) : boolean;
      class function NumericGTE(const AValue, AMatch : Variant) : boolean;
      class function NumericBetweenInclusive(const AValue, Lower, Upper : Variant) : boolean;
      class function NumericBetweenExclusive(const AValue, Lower, Upper : Variant) : boolean;
  end;

  { TFileTool }

  TFileTool = class
    class procedure AppendText(const AFileName: string; const AText: string);
  end;

{ COMPLEX CONSTANTS }

const
    MinTimeSpan : TTimeSpan = (FMillis: Low(Int64));
    MaxTimeSpan: TTimeSpan = (FMillis: High(Int64));
    ZeroTimeSpan: TTimeSpan = (FMillis: 0);

resourcestring
  sNotImplemented = 'Not implemented';
  sInvalidParameter_OutOfBounds = 'Invalid Parameter: %s out of bounds';

implementation

uses dateutils;

{ CONSTANTS }
const
  IntlDateTimeFormat : TFormatSettings = (
    DateSeparator : '-';
    TimeSeparator : ':';
    ShortDateFormat : 'yyyy/mm/dd';
    LongDateFormat : ' yyyy/mm/dd';
    ShortTimeFormat : 'hh:nn:zzz';
    LongTimeFormat : 'hh:nn:zzz'
  );

{ VARIABLES }

var
  MinTimeStampDateTime : TDateTime = 0;
  VarTrue : Variant;
  VarFalse : Variant;


{%region Global functions }

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
     for i:= Low(Str1) to High(Str1) do begin
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

{$IFDEF FPC}
{%region TTimeSpan }

class constructor TTimeSpan.Create;
begin
end;

function TTimeSpan.GetDays: Integer;
begin
  Result :=  FMillis div MillisPerDay;
end;

function TTimeSpan.GetHours: Integer;
begin
  Result := (FMillis div MillisPerHour) mod HoursPerDay;
end;

function TTimeSpan.GetMinutes: Integer;
begin
  Result := (FMillis div MillisPerMinute) mod MinsPerHour;
end;

function TTimeSpan.GetSeconds: Integer;
begin
  Result := (FMillis div MillisPerSecond) mod SecsPerMin;
end;

function TTimeSpan.GetMilliseconds: Integer;
begin
  Result := FMillis mod MillisPerSecond;
end;

function TTimeSpan.GetTotalDays: Double;
begin
  Result := FMillis / MillisPerDay;
end;

function TTimeSpan.GetTotalHours: Double;
begin
  Result := FMillis / MillisPerHour;
end;

function TTimeSpan.GetTotalMinutes: Double;
begin
  Result := FMillis / MillisPerMinute;
end;

function TTimeSpan.GetTotalSeconds: Double;
begin
  Result := FMillis / MillisPerSecond;
end;

function TTimeSpan.GetTotalMilliseconds: Double;
begin
  Result := Double(FMillis);
end;

class function TTimeSpan.Normalize(const ADateTime : TDateTime) : Int64; static;
begin
  Result := MilliSecondsBetween(ADateTime, MinDateTime);
end;

constructor TTimeSpan.Create(Hours, Minutes, Seconds: Integer);
begin
  Self.FMillis := (Hours*MillisPerHour) + (Minutes*MillisPerMinute) + (Seconds*MillisPerSecond);
end;

constructor TTimeSpan.Create(Days, Hours, Minutes, Seconds: Integer); overload;
begin
  Self.FMillis := Days*MillisPerDay + Hours*MillisPerHour + Minutes*MillisPerMinute + Seconds*MillisPerSecond;
end;

constructor TTimeSpan.Create(Days, Hours, Minutes, Seconds, Milliseconds: Integer); overload;
begin
  Self.FMillis := Days*MillisPerDay + Hours*MillisPerHour + Minutes*MillisPerMinute + Seconds*MillisPerSecond + Milliseconds;
end;

function TTimeSpan.Add(const TS: TTimeSpan): TTimeSpan;
begin
  Result.FMillis := FMillis + TS.FMillis;
end;

function TTimeSpan.Duration: TTimeSpan;
begin
  Result.FMillis := FMillis;
end;

function TTimeSpan.Negate: TTimeSpan;
begin
  Result.FMillis := -FMillis;
end;

function TTimeSpan.Subtract(const TS: TTimeSpan): TTimeSpan;
begin
  Result.FMillis := FMillis - TS.FMillis;
end;

function TTimeSpan.ToString: string;
begin
  Result := Format('[%d]:[%.2d]:[%.2d]:[%.2d].[%.4d]', [GetDays, GetHours, GetMinutes, GetSeconds, GetMilliseconds]);
end;

class function TTimeSpan.FromDays(Value: Double): TTimeSpan;
var xxx : double;
begin
  xxx := MillisPerDay;
  Result.FMillis := Round(Value * MillisPerDay);
end;

class function TTimeSpan.FromHours(Value: Double): TTimeSpan; static;
begin
  Result.FMillis := Round(Value * MillisPerHour);
end;

class function TTimeSpan.FromMinutes(Value: Double): TTimeSpan; static;
begin
  Result.FMillis := Round(Value * MillisPerMinute);
end;

class function TTimeSpan.FromSeconds(Value: Double): TTimeSpan; static;
begin
  Result.FMillis := Round(Value * MillisPerSecond);
end;

class function TTimeSpan.FromMilliseconds(Value: Double): TTimeSpan; static;
begin
  Result.FMillis := Round(Value);
end;

class function TTimeSpan.Subtract(const D1, D2: TDateTime): TTimeSpan; static;
begin
  Result.FMillis := Normalize(D1) - Normalize(D2);
end;

class function TTimeSpan.Parse(const S: string): TTimeSpan; static;
begin
  raise ENotImplemented.Create('');
end;

class function TTimeSpan.TryParse(const S: string; out Value: TTimeSpan): Boolean; static;
begin
  raise ENotImplemented.Create('');
end;

class operator TTimeSpan.+(const Left, Right: TTimeSpan) ATimeSpan : TTimeSpan;
begin
  ATimeSpan.FMillis := Left.FMillis + Right.FMillis;
end;

class operator TTimeSpan.+(const Left: TTimeSpan; Right: TDateTime) ADateTime: TDateTime;
begin
  ADateTime := IncMilliSecond(Right, Left.FMillis);
end;

class operator TTimeSpan.+(const Left: TDateTime; Right: TTimeSpan) ADateTime: TDateTime;
begin
  ADateTime := IncMilliSecond(Left, Right.FMillis);
end;

class operator TTimeSpan.-(const Left, Right: TTimeSpan) ATimeSpan : TTimeSpan;
begin
  ATimeSpan.FMillis := Left.FMillis - Right.FMillis;
end;

class operator TTimeSpan.-(const Left: TDateTime; Right: TTimeSpan) ADateTime: TDateTime;
begin
  ADateTime := IncMilliSecond(Left, -Right.FMillis);
end;

class operator TTimeSpan.=(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis = Right.FMillis;
end;

class operator TTimeSpan.<>(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis <> Right.FMillis;
end;

class operator TTimeSpan.>(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis > Right.FMillis;
end;

class operator TTimeSpan.>=(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis >= Right.FMillis;
end;

class operator TTimeSpan.<(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis < Right.FMillis;
end;

class operator TTimeSpan.<=(const Left, Right: TTimeSpan) ABool : Boolean;
begin
  ABool := Left.FMillis <= Right.FMillis;
end;

class operator TTimeSpan.Negative(const Value: TTimeSpan) ATimeSpan: TTimeSpan;
begin
  ATimeSpan := Value.Negate;
end;

class operator TTimeSpan.Positive(const Value: TTimeSpan) ATimeSpan : TTimeSpan;
begin
  ATimeSpan := Value;
end;

class operator TTimeSpan.Implicit(const Value: TTimeSpan) AString : string;
begin
  AString := Value.ToString;
end;

class operator TTimeSpan.Explicit(const Value: TTimeSpan) AString : string;
begin
  AString := Value.ToString;
end;

{%endregion}

{$ENDIF}

{%region TAuto }

constructor TAuto<T>.Create(const AItem: T);
begin
  FGC.InitCapacity(1);
  FGC.AddObject(AItem);
end;

function TAuto<T>.GetItem : T;
begin
  if FGC.Count = 1 then
    Result := T(FGC.ItemAt(0))
  else
    Result := Default(T)
end;

procedure TAuto<T>.SetItem(const AItem: T);
var
  oldsp : TScopedPtr;
  old : TObject;
begin
  while FGC.Count > 0 do begin
    oldsp := FGC.ScopedPtrAt(0);
    old := FGC.ItemAt(0);
    FGC.RemoveObject(old);
    if (oldsp.IsObject) then
      FreeAndNil(Pointer(old));
  end;
  FGC.AddObject(AItem);
end;

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

{ Enums }

function GetSetName(const aSet:PTypeInfo; Value: Integer):string;
var
  vData1 : PTypeData;
  vData2 : PTypeData;
  vCntr  : Integer;
  v: Integer;
begin
  Result := '';
  if aSet^.Kind = tkSet then begin
    vData1 := GetTypeData(aSet);
    vData2 := GetTypeData(vData1^.CompType);
    for vCntr := vData2^.MinValue to vData2^.MaxValue do
      if (Value shr vCntr) and 1 <> 0 then
        Result := Result+ GetEnumName(vData1^.CompType,vCntr)+',';
    if Result <> '' then Delete(Result, Length(Result), 1);
  end;
end;

function GetSetValue(const aSet:PTypeInfo; Name: String): Integer;
var
  vData1 : PTypeData;
  vData2 : PTypeData;
  vCntr  : Integer;
  p      : Integer;
begin
  Result := 0;
  if aSet^.Kind = tkSet then begin
    vData1 := GetTypeData(aSet);
    vData2 := GetTypeData(vData1^.CompType);
    for vCntr := vData2^.MinValue to vData2^.MaxValue do begin
      p := pos(GetEnumName(vData1^.CompType, vCntr), Name);
      if p = 0 then
        Continue;
      if (p = 1) or (Name[p-1] = ',') then
        Result := Result or (1 shl vCntr);
    end;
  end;
end;

{ Clip/Min/Max Value }

function ClipValue( AValue, MinValue, MaxValue: Integer) : Integer;
begin
  if AValue < MinValue then
    Result := MinValue
  else if AValue > MaxValue then
    Result := MaxValue
  else
    Result := AValue
end;

function MinValue(const AArray : array of Cardinal) : Cardinal;
var i : SizeInt;
begin
  Result := -1;
  for i := Low(AArray) to High(AArray) do begin
    if Result > AArray[i] then
      Result := AArray[i];
  end;
end;

{%endregion}

{%region Date/Time Support }

function TimeStamp : AnsiString;
begin
  Result := FormatDateTime('yyy-mm-dd hh:nn:ss', Now);
end;

function UtcTimeStamp : AnsiString;
begin
  raise Exception.Create(sNotImplemented);
end;

function TDateTimeHelper.AddDays(const aNumberOfDays: Integer): TDateTime;
begin
  Result := IncDay(Self, aNumberOfDays);
end;

function TDateTimeHelper.AddHours(const aNumberOfHours: Int64): TDateTime;
begin
  Result := IncHour(Self, aNumberOfHours);
end;

function TDateTimeHelper.AddMilliseconds(const aNumberOfMilliseconds: Int64): TDateTime;
begin
  Result := IncMilliSecond(Self, aNumberOfMilliseconds);
end;

function TDateTimeHelper.AddMinutes(const aNumberOfMinutes: Int64): TDateTime;
begin
  Result := IncMinute(Self, aNumberOfMinutes);
end;

function TDateTimeHelper.AddMonths(const aNumberOfMonths: Integer): TDateTime;
begin
  Result := IncMonth(Self, aNumberOfMonths);
end;

function TDateTimeHelper.AddSeconds(const aNumberOfSeconds: Int64): TDateTime;
begin
  Result := IncSecond(Self, aNumberOfSeconds);
end;

function TDateTimeHelper.AddYears(const aNumberOfYears: Integer): TDateTime;
begin
  Result := IncYear(Self, aNumberOfYears);
end;

function TDateTimeHelper.CompareTo(const aDateTime: TDateTime): TValueRelationship;
begin
  Result := CompareDateTime(Self, aDateTime);
end;

class function TDateTimeHelper.Create(const aYear, aMonth,
  aDay: Word): TDateTime;
begin
  Result := EncodeDate(aYear, aMonth, aDay);
end;

class function TDateTimeHelper.Create(const aYear, aMonth, aDay, aHour, aMinute,
  aSecond, aMillisecond: Word): TDateTime;
begin
  Result := EncodeDateTime(aYear, aMonth, aDay, aHour, aMinute, aSecond, aMillisecond);
end;

function TDateTimeHelper.DaysBetween(const aDateTime: TDateTime): Integer;
begin
  Result := dateutils.DaysBetween(Self, aDateTime);
end;

function TDateTimeHelper.EndOfDay: TDateTime;
begin
  Result := EndOfTheDay(Self);
end;

function TDateTimeHelper.EndOfMonth: TDateTime;
begin
  Result := EndOfTheMonth(Self);
end;

function TDateTimeHelper.EndOfWeek: TDateTime;
begin
  Result := EndOfTheWeek(Self);
end;

function TDateTimeHelper.EndOfYear: TDateTime;
begin
  Result := EndOfTheYear(Self);
end;

function TDateTimeHelper.Equals(const aDateTime: TDateTime): Boolean;
begin
  Result := SameDateTime(Self, aDateTime);
end;

function TDateTimeHelper.GetDate: TDateTime;
begin
  Result := DateOf(Self);
end;

function TDateTimeHelper.GetDay: Word;
begin
  Result := DayOf(Self);
end;

function TDateTimeHelper.GetDayOfWeek: Word;
begin
  Result := DayOfTheWeek(Self);
end;

function TDateTimeHelper.GetDayOfYear: Word;
begin
  Result := DayOfTheYear(Self);
end;

function TDateTimeHelper.GetHour: Word;
begin
  Result := HourOf(Self);
end;

function TDateTimeHelper.GetMillisecond: Word;
begin
  Result := MilliSecondOf(Self);
end;

function TDateTimeHelper.GetMinute: Word;
begin
  Result := MinuteOf(Self);
end;

function TDateTimeHelper.GetMonth: Word;
begin
  Result := MonthOf(Self);
end;

class function TDateTimeHelper.GetNow: TDateTime;
begin
  Result := SysUtils.Now;
end;

function TDateTimeHelper.GetSecond: Word;
begin
  Result := SecondOf(Self);
end;

function TDateTimeHelper.GetTime: TDateTime;
begin
  Result := TimeOf(Self);
end;

class function TDateTimeHelper.GetToday: TDateTime;
begin
  Result := SysUtils.Date;
end;

class function TDateTimeHelper.GetTomorrow: TDateTime;
begin
  Result := SysUtils.Date + 1;
end;

function TDateTimeHelper.GetYear: Word;
begin
  Result := YearOf(Self);
end;

class function TDateTimeHelper.GetYesterDay: TDateTime;
begin
  Result := SysUtils.Date - 1;
end;

function TDateTimeHelper.HoursBetween(const aDateTime: TDateTime): Int64;
begin
  Result := DateUtils.HoursBetween(Self, aDateTime);
end;

function TDateTimeHelper.InRange(const aStartDateTime, aEndDateTime: TDateTime; const aInclusive: Boolean): Boolean;
begin
  if aInclusive then
    Result := (aStartDateTime <= self) AND (self <= aEndDateTime)
  else
    Result := (aStartDateTime < self) AND (self < aEndDateTime);
end;

function TDateTimeHelper.IsAM: Boolean;
begin
  Result := NOT dateutils.IsPM(self);
end;

function TDateTimeHelper.IsInLeapYear: Boolean;
begin
  Result := dateutils.IsInLeapYear(Self);
end;

function TDateTimeHelper.IsPM: Boolean;
begin
  Result := dateutils.IsPM(Self);
end;

function TDateTimeHelper.IsSameDay(const aDateTime: TDateTime): Boolean;
begin
  Result := DateUtils.IsSameDay(Self, aDateTime);
end;

function TDateTimeHelper.IsToday: Boolean;
begin
  Result := DateUtils.IsToday(Self);
end;

function TDateTimeHelper.MilliSecondsBetween(const aDateTime: TDateTime): Int64;
begin
  Result := DateUtils.MilliSecondsBetween(Self, aDateTime);
end;

function TDateTimeHelper.MinutesBetween(const aDateTime: TDateTime): Int64;
begin
  Result := DateUtils.MinutesBetween(Self, aDateTime);
end;

function TDateTimeHelper.MonthsBetween(const aDateTime: TDateTime): Integer;
begin
  Result := DateUtils.MonthsBetween(Self, aDateTime);
end;

function TDateTimeHelper.SecondsBetween(const aDateTime: TDateTime): Int64;
begin
  Result := DateUtils.SecondsBetween(Self, aDateTime);
end;

function TDateTimeHelper.StartOfDay: TDateTime;
begin
  Result := StartOfTheDay(Self);
end;

function TDateTimeHelper.StartOfMonth: TDateTime;
begin
  Result := StartOfTheMonth(Self);
end;

function TDateTimeHelper.StartOfWeek: TDateTime;
begin
  Result := StartOfTheWeek(Self);
end;

function TDateTimeHelper.StartOfYear: TDateTime;
begin
  Result := StartOfTheYear(Self);
end;

function TDateTimeHelper.ToString(const aFormatStr: string): string;
begin
  if aFormatStr = '' then
    Result := DateToStr(Self)
  else
    Result := FormatDateTime(aFormatStr, Self);
end;

function TDateTimeHelper.WeeksBetween(const aDateTime: TDateTime): Integer;
begin
  Result := DateUtils.WeeksBetween(Self, aDateTime);
end;

function TDateTimeHelper.WithinDays(const aDateTime: TDateTime;
  const aDays: Integer): Boolean;
begin
  Result := DateUtils.WithinPastDays(Self, aDateTime, aDays);
end;

function TDateTimeHelper.WithinHours(const aDateTime: TDateTime;
  const aHours: Int64): Boolean;
begin
  Result := DateUtils.WithinPastHours(Self, aDateTime, aHours);
end;

function TDateTimeHelper.WithinMilliseconds(const aDateTime: TDateTime;
  const AMilliseconds: Int64): Boolean;
begin
  Result := DateUtils.WithinPastMilliSeconds(Self, aDateTime, AMilliseconds);
end;

function TDateTimeHelper.WithinMinutes(const aDateTime: TDateTime;
  const aMinutes: Int64): Boolean;
begin
  Result := DateUtils.WithinPastMinutes(Self, aDateTime, aMinutes);
end;

function TDateTimeHelper.WithinMonths(const aDateTime: TDateTime;
  const aMonths: Integer): Boolean;
begin
  Result := DateUtils.WithinPastMonths(Self, aDateTime, aMonths);
end;

function TDateTimeHelper.WithinSeconds(const aDateTime: TDateTime;
  const aSeconds: Int64): Boolean;
begin
  Result := DateUtils.WithinPastSeconds(Self, aDateTime, aSeconds);
end;

function TDateTimeHelper.WithinWeeks(const aDateTime: TDateTime;
  const aWeeks: Integer): Boolean;
begin
  Result := DateUtils.WithinPastWeeks(Self, aDateTime, aWeeks);
end;

function TDateTimeHelper.WithinYears(const aDateTime: TDateTime;
  const aYears: Integer): Boolean;
begin
  Result := DateUtils.WithinPastYears(Self, aDateTime, aYears);
end;

function TDateTimeHelper.YearsBetween(const aDateTime: TDateTime): Integer;
begin
  Result := DateUtils.YearsBetween(Self, aDateTime);
end;

{%endregion}

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
  for i := low(self) to high(self) do
    self[i](sender);
end;

{%endregion}

{%region TNotifyManyEventHelperEx}

procedure TNotifyManyEventExHelper.Add(listener : TNotifyEventEx);
begin
  if TArrayTool<TNotifyEventEx>.IndexOf(self, listener) = -1 then begin
    TArrayTool<TNotifyEventEx>.Add(self, listener);
  end;
end;

procedure TNotifyManyEventExHelper.Remove(listener : TNotifyEventEx);
begin
  TArrayTool<TNotifyEventEx>.Remove(self, listener);
end;

procedure TNotifyManyEventExHelper.Invoke(sender : TObject; const args: array of Pointer);
var i : Integer;
begin
  for i := Low(Self) to high(Self) do
    self[i](sender, args);
end;

{%endregion}

{%region TArrayTool}

class function TArrayTool<T>.Empty : TArray<T>;
begin
  SetLength(Result, 0);
end;

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
begin
  Result := IndexOf(Values, Item) >= 0;
end;

class function TArrayTool<T>.IndexOf(const Values: TArray<T>; const Item: T; const Comparer: IEqualityComparer<T>): SizeInt;
var
  i : SizeInt;
begin
  Result := -1;
  for i := Low(Values) to High(Values) do
    if Comparer.Equals(Values[i], Item) then begin
      Result := i;
      exit;
    end;
end;

class function TArrayTool<T>.IndexOf(const Values: TArray<T>; const Item: T): SizeInt;
begin
  Result := IndexOf(Values, Item, TEqualityComparer<T>.Default);
end;

class function TArrayTool<T>.Copy(const AArray: array of T): TArray<T>;
begin
  Result := Copy(AArray, 0, Length(AArray));
end;

class function TArrayTool<T>.Copy(const AArray: array of T; FromIndex, Count: SizeInt ): TArray<T>;
var
  i : SizeInt;
begin
  if Count < 0 then raise EArgumentOutOfRangeException.Create('Count was less than 0');
  if (FromIndex + Count - 1) > High(AArray) then raise EArgumentOutOfRangeException.Create('FromIndex + Count was greater than High(AArray)');
  SetLength(Result, Count);
  for i:= FromIndex to FromIndex + Count - 1 do
    Result[i - FromIndex] := AArray[i];
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

class function TArrayTool<T>.Create(const item0 : T) : TArray<T>;
begin
  SetLength(result,1);
  result[0] := item0;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T) : TArray<T>;
begin
  SetLength(result,2);
  result[0] := item0;
  result[1] := item1;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T; const item2 : T) : TArray<T>;
begin
  SetLength(result,3);
  result[0] := item0;
  result[1] := item1;
  result[2] := item2;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T) : TArray<T>;
begin
  SetLength(result,4);
  result[0] := item0;
  result[1] := item1;
  result[2] := item2;
  result[3] := item3;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T; const item4 : T) : TArray<T>;begin
  SetLength(result,5);
  result[0] := item0;
  result[1] := item1;
  result[2] := item2;
  result[3] := item3;
  result[4] := item4;
end;

class function TArrayTool<T>.Create(const item0 : T; const item1 : T; const item2 : T; const item3 : T; const item4 : T; const item5 : T) : TArray<T>;
begin
  SetLength(result,6);
  result[0] := item0;
  result[1] := item1;
  result[2] := item2;
  result[3] := item3;
  result[4] := item4;
  result[5] := item5;
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

class function TArrayTool<T>._Length(const Values: array of T) : SizeInt;
begin
   Result := Length(Values);
end;

{%endregion}

{%region TVariantTool}

class function TVariantTool.IsNumeric(const AValue : Variant) : boolean;
begin
  // VarIsNumeric seems to be broken
  case VarType(AValue) of
    varsmallint, varinteger, varsingle,
    vardouble, varcurrency, varboolean, vardecimal,
    varshortint, varbyte, varword, varlongword, varint64, varqword : Result := true;
    else Result := false;
  end;
end;

class function TVariantTool.TryParseBool(const AValue : Variant; out ABoolean : boolean) : boolean;
var
  AValueStr : string;
begin
  ABoolean := false;
  Result := false;
  if VarIsBool(AValue) then begin
    ABoolean := Boolean(AValue);
    Result := true;
  end else if VarIsNumeric(AValue) then
    case VarToInt(AValue) of
      0: begin
          ABoolean := False;
          Result := True;
         end;
      1: begin
           ABoolean := True;
           Result := True;
         end;
    end
  else if VarIsStr(AValue) then begin
    AValueStr := VarToStr(AValue);
    Result := (AValueStr = VarToStr(VarTrue)) or (AValueStr = VarToStr(VarFalse));
    ABoolean := AValueStr = VarToStr(VarTrue);
  end;
end;

class function TVariantTool.VarToInt(const AVariant: Variant): integer;
begin
  Result := StrToIntDef(Trim(VarToStr(AVariant)), 0);
end;

class function TVariantTool.MatchTextExact(const AValue, AMatch : Variant) : boolean;
begin
  if VarIsNumeric(AValue) then
    Result := false
  else
    Result := VarToStr(AValue) = VarToStr(AMatch);
end;

class function TVariantTool.MatchTextBeginning(const AValue, AMatch : Variant) : boolean;
begin
  Result := VarToStr(AValue).StartsWith(VarToStr(AMatch));
end;

class function TVariantTool.MatchTextEnd(const AValue, AMatch : Variant) : boolean;
begin
  Result := VarToStr(AValue).EndsWith(VarToStr(AMatch));
end;

class function TVariantTool.MatchTextAnywhere(const AValue, AMatch : Variant) : boolean;
begin
  Result := VarToStr(AValue).Contains(VarToStr(AMatch));
end;

class function TVariantTool.NumericEQ(const AValue, AMatch : Variant) : boolean;
var
  bmatch : boolean;
begin
  if NOT IsNumeric(AValue) then
    Exit(false);

  IF VarIsBool(AValue) then begin
    if TryParseBool(AMatch, bmatch) then begin
      Result := (Boolean(AValue) = bmatch);
    end else begin
      Result := false;
      end
  end else begin
    Result := TCompare.Variant(@AValue, @AMatch) = 0;
  end;
end;

class function TVariantTool.NumericLT(const AValue, AMatch : Variant) : boolean;
begin
  if (NOT IsNumeric(AValue)) OR (VarIsBool(AValue)) then
    Exit(false);
  Result := TCompare.Variant(@AValue, @AMatch) = -1;
end;

class function TVariantTool.NumericLTE(const AValue, AMatch : Variant) : boolean;
var
  cmp : Integer;
begin
  if (NOT IsNumeric(AValue)) OR (VarIsBool(AValue)) then
    Exit(false);
  cmp := TCompare.Variant(@AValue, @AMatch);
  Result := (cmp = -1) OR (cmp = 0);
end;

class function TVariantTool.NumericGT(const AValue, AMatch : Variant) : boolean;
var
  cmp : Integer;
begin
  if (NOT IsNumeric(AValue)) OR (VarIsBool(AValue)) then
    Exit(false);
  cmp := TCompare.Variant(@AValue, @AMatch);
  Result := (cmp = 1);
end;

class function TVariantTool.NumericGTE(const AValue, AMatch : Variant) : boolean;
var
  cmp : Integer;
begin
  if (NOT IsNumeric(AValue)) OR (VarIsBool(AValue)) then
    Exit(false);
  cmp := TCompare.Variant(@AValue, @AMatch);
  Result := (cmp = 1) OR (cmp = 0);
end;

class function TVariantTool.NumericBetweenInclusive(const AValue, Lower, Upper : Variant) : boolean;
var
  lowercmp, uppercmp : Integer;
begin
  if (NOT IsNumeric(AValue)) OR (VarIsBool(AValue)) then
    Exit(false);
  lowercmp := TCompare.Variant(@AValue, @Lower);
  uppercmp := TCompare.Variant(@AValue, @Upper);
  Result := ((lowercmp = 1) OR (lowercmp = 0)) AND ((uppercmp = -1) OR (uppercmp = 0));
end;

class function TVariantTool.NumericBetweenExclusive(const AValue, Lower, Upper : Variant) : boolean;
var
  lowercmp, uppercmp : Integer;
begin
  if NOT IsNumeric(AValue) then
    Exit(false);
  lowercmp := TCompare.Variant(@AValue, @Lower);
  uppercmp := TCompare.Variant(@AValue, @Upper);
  Result := (lowercmp = 1) AND (uppercmp = -1);
end;

{%endregion}

{%region TFileTool }

class procedure TFileTool.AppendText(const AFileName: string; const AText: string);
var
  fstream: TFileStream;
begin
  if (FileExists(AFileName)) then begin
    fstream := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyNone);
    fstream.Seek(0, soFromEnd);
  end else begin
    fstream := TFileStream.Create(AFileName, fmCreate or fmShareDenyNone);
    fstream.Seek(0, soFromEnd);
  end;
  try
    fstream.WriteAnsiString(AText+#13#10);
  finally
    fstream.Free;
  end;
end;

{%endregion}

initialization
  MinTimeStampDateTime:= StrToDateTime('1980-01-01 00:00:000', IntlDateTimeFormat);
  VarTrue := True;
  VarFalse := False;

finalization

end.

