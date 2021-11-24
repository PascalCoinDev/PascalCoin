{ Copyright (c) 2017 - 2018 Sphere 10 Software <https://www.sphere10.com>

  Common tools.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Herman Schoenfeld: main author
  - Ugochukwu Mmaduekwe: Add "TLogicalCPUCount" Class

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

unit UCommon;

{$IFDEF FPC}
  {$MODE Delphi}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}


interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults,
  {$IFNDEF FPC}System.Types, System.TimeSpan,
  {$ELSE}{$IFDEF LINUX} {$linklib c} ctypes, {$ENDIF LINUX}
  {$IFDEF WINDOWS} Windows, {$ENDIF WINDOWS}
  {$IF DEFINED(DARWIN) OR DEFINED(FREEBSD)} ctypes, sysctl, {$ENDIF}
  {$ENDIF} Variants, math, typinfo, UMemory, syncobjs;

{ CONSTANTS }

const
  EPSILON : Double = 0.00001;
  MillisPerSecond = 1000;
  MillisPerMinute = 60 * MillisPerSecond;
  MillisPerHour = 60 * MillisPerMinute;
  MillisPerDay = 24 * MillisPerHour;
  MaxMilliseconds = High(Int64);
  MinMilliseconds = Low(Int64);
  MaxSeconds = MaxMilliseconds div 60;
  MinSeconds = MinMilliseconds div 60;

  BYTE_BIT_0 = byte(1);
  BYTE_BIT_1 = byte(2);
  BYTE_BIT_2 = byte(4);
  BYTE_BIT_3 = byte(8);
  BYTE_BIT_4 = byte(16);
  BYTE_BIT_5 = byte(32);
  BYTE_BIT_6 = byte(64);
  BYTE_BIT_7 = byte(128);

{ GLOBAL HELPER FUNCTIONS }


function String2Hex(const Buffer: String): String;
function Hex2Bytes(const AHexString: String): TBytes; overload;
function TryHex2Bytes(const AHexString: String; out ABytes : TBytes): boolean; overload;
function Bytes2Hex(const ABytes: TBytes; AUsePrefix : boolean = false) : String;
function BinStrComp(const Str1, Str2 : String): Integer; // Binary-safe StrComp replacement. StrComp will return 0 for when str1 and str2 both start with NUL character.
function BytesCompare(const ABytes1, ABytes2: TBytes): integer;
function BytesEqual(const ABytes1, ABytes2 : TBytes) : boolean; overload; inline;
function BytesEqual(const ABytes1, ABytes2 : TBytes; AFrom, ALength : UInt32) : boolean; overload; inline;
function ContencateBytes(const AChunk1, AChunk2: TBytes): TBytes; inline;
function SetLastDWordLE(const ABytes: TBytes; AValue: UInt32): TBytes;
function GetLastDWordLE(const ABytes: TBytes) : UInt32;
function GetDWordLE(const ABytes: TBytes; AOffset : Integer) : UInt32;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Cardinal): Cardinal; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Integer): Integer; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Int64): Int64; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: UInt64): UInt64; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: Double): Double; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: string): string; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: TObject): TObject; overload;
function IIF(const ACondition: Boolean; const ATrueResult, AFalseResult: variant): variant; overload;
function ClipValue( AValue, MinValue, MaxValue: Integer) : Integer;
function MinValue(const AArray : array of Cardinal) : Cardinal;
function MaxValue(const AArray : array of Cardinal) : Cardinal;
function RoundEx(const AInput: Single; APlaces: Integer): Single; overload;
function RoundEx(const AInput: Double; APlaces: Integer): Double; overload;
function RoundEx(const AInput: Currency; APlaces: integer): Currency; overload;
{$IFDEF FPC}
function GetSetName(const aSet:PTypeInfo; Value: Integer):string;
function GetSetValue(const aSet:PTypeInfo; Name: String): Integer;
{$ENDIF}

type

  {$IFNDEF FPC}
  // Delphi compatibility
  SizeInt = NativeInt;
  {$ENDIF}

  { TNullable }

  TNullable<T> = record
  private
    FHasValue: Boolean;
    FValue: T;
    function GetValue: T;
    procedure SetValue(AValue: T);
  public
    procedure Clear;
    property HasValue: Boolean read FHasValue;
    property Value: T read GetValue write SetValue;
    class operator Implicit(A: T): TNullable<T>;
    class operator Implicit(A: Pointer): TNullable<T>;
  end;

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
      class function GetMinValue : TTimeSpan; static; inline; // cannot be var due to FPC bug
      class function GetMaxValue : TTimeSpan; static; inline; // cannot be var due to FPC bug
      class function GetZeroValue : TTimeSpan; static; inline; // cannot be var due to FPC bug
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
      class property MinValue: TTimeSpan read GetMinValue;
      class property MaxValue: TTimeSpan read GetMaxValue;
      class property ZeroValue: TTimeSpan read GetZeroValue;
    end;

  {$ENDIF}

  { TBox }

  TBox<T> = class(TObject)
    private
      FValue : T;
    public
      class var Instances: Integer;
    public
      property Value : T read FValue write FValue;
      class constructor Create;
      constructor Create(const AValue : T); overload;
      destructor Destroy; override;
  end;

  { TLogSeverity }

  TLogSeverity = (lsDebug, lsInfo, lsWarning, lsError);

  { TLogMessage }

  TLogMessage = record
  private
    FTimestamp : TDateTime;
    FSeverity : TLogSeverity;
    FText : String;
  public
    property TimeStamp : TDateTime read FTimestamp;
    property Severity : TLogSeverity read FSeverity;
    property Text : String read FText;
    class function From(const AText : String; ASeverity : TLogSeverity; ATimeStamp : TDateTime) : TLogMessage; overload; static;
    class function From(const AText : String; ASeverity : TLogSeverity = lsError) : TLogMessage; overload; static;
  end;

  { TResult }

  TResult = record
  private
    FMessages : TArray<TLogMessage>;
    FValue : Variant;
    FHasError : boolean;
    function GetIsSuccess : boolean; inline;
  public
    property Messages : TArray<TLogMessage> read FMessages;
    property Value : Variant read FValue write FValue;  // used to carry an optional return value
    property IsFailure : boolean read FHasError;
    property IsSuccess : boolean read GetIsSuccess;
    procedure Add(const ALogMessage : TLogMessage); overload;
    procedure Add(ASeverity : TLogSeverity; const AString : String); overload;
    procedure AddDebug(const AString : String);
    procedure AddInfo(const AString : String);
    procedure AddWarning(const AString : String);
    procedure AddError(const AString : String);
    function ToString(AIncludeTimeStamp : boolean = false) : String; overload;
    function ToString(AIncludeTimeStamp, AIncludeSeverity : boolean) : String; overload;
    class function Success : TResult; overload; static;
    class function Success(const AText : String) : TResult; overload; static;
    class function Failure : TResult; overload; static;
    class function Failure(const AText : String) : TResult; overload; static;
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
     class function GetNowUtc: TDateTime; static; inline;
     class function GetToday: TDateTime; static; inline;
     class function GetTomorrow: TDateTime; static; inline;
     class function GetYesterDay: TDateTime; static; inline;
   public
     class function Create(const aYear, aMonth, aDay: Word): TDateTime; overload; static; inline;
     class function Create(const aYear, aMonth, aDay, aHour, aMinute, aSecond, aMillisecond: Word): TDateTime; overload; static; inline;

     class property NowUtc: TDateTime read GetNowUtc;
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

     function ToIntlString : String; inline;
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

  { Event Support}

  TNotifyManyEvent = record
    Handlers: TArray<TNotifyEvent>;
    MainThreadHandlers : TArray<TNotifyEvent>;
  end;

  TNotifyManyEventHelper = record helper for TNotifyManyEvent
    procedure Add(AHandler : TNotifyEvent; ExecuteMainThread : Boolean = False);
    procedure Remove(AHandler : TNotifyEvent);
    procedure Invoke(sender : TObject);
  end;

  { TThreadNotify }

  TThreadNotify = class
  type
    TPendingNotifyManyEvent = record
      Sender : TObject;
      Handlers : TArray<TNotifyEvent>;
    end;
  private
    FTargetThread : TThread;
    FLock : TCriticalSection;
    FPendingNotifications : TList<TPendingNotifyManyEvent>;
    procedure InvokePendingOnTargetThread;
  public
    constructor Create(ATargetThread : TThread);
    destructor Destroy; override;
    procedure Invoke(Sender: TObject; Handler : TNotifyEvent); overload;
    procedure Invoke(Sender: TObject; const Handlers: TArray<TNotifyEvent>); overload;
    class procedure InvokeMainThread(Sender: TObject; Handler : TNotifyEvent); overload;
    class procedure InvokeMainThread(Sender: TObject; const Handlers: TArray<TNotifyEvent>); overload;
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
      class function _Length(const Values: array of T) : SizeInt; static; {$IFDEF FPC}inline;{$endif}
      class function ToArray(Enumerable: TEnumerable<T>; Count: SizeInt): TArray<T>; static;
  end;

  { TVariantTool }

  TVariantTool = class
    public
      class function IsBool(const AValue : Variant) : boolean; inline;
      class function IsNumeric(const AValue : Variant) : boolean; inline;
      class function CompareVariant(const ALeft, ARight : Variant) : Integer; inline;
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

  { TStreamHelper }

  TStreamHelper = class helper for TStream
    {$IFNDEF FPC}
    procedure WriteString(const AString : String);
    {$ENDIF}
    function ReadBytes(ACount : Int32) : TBytes; inline;
  end;

  { TMemoryStreamHelper }

   TMemoryStreamHelper = class helper for TMemoryStream
     function ToBytes(ASize : Integer = -1) : TBytes; inline;
   end;


  { TFileTool }

  TFileTool = class
    class procedure AppendText(const AFileName: string; const AText: string);
  end;

  { TCPUTool }

  TCPUTool = class
    //returns number of cores: a computer with two hyperthreaded cores will report 4
    class function GetLogicalCPUCount(): Int32; static;
  end;


  { TStatistics }

  { NOTE: this is a running stats keeper that does not keep item set, thus
    uses estimations which can diverge from real values }
  TStatistics = record
  private
    FCount : UInt32; // Number of items in the analysis
    FTotal : Double; // Total of data
    FTotal2 : Double; // Sum of sqaures of data
//    FProduct : Double; // Product of data
    FRecip : Double; // Sum of reciprocals of data
    FMin : Double;  // Min datum
    FMax : Double;  // Min datum
  public
    property SampleCount : UInt32 read FCount;
    property Sum : Double read FTotal;
    property SquaredSum : Double read FTotal2;
//    property Product : Double read FProduct;
    property ReciprocalSum : Double read FRecip;
    property Minimum : Double read FMin;
    property Maximum : Double read FMax;
    procedure Reset;
    function Mean : Double; inline;
    function PopulationVariance : Double; inline;
    function PopulationStandardDeviation : Double; inline;
    function PopulationVariationCoefficient : Double; inline;
 //   function GeometricMean : Double; inline;
    function HarmonicMean : Double; inline;
    function MinimumError : Double; inline;
    function MaximumError : Double; inline;
    function SampleVariance : Double; inline;
    function SampleStandardDeviation : Double; inline;
    function SampleVariationCoefficient : Double; inline;
    procedure AddDatum(ADatum : Double); overload; inline;
    procedure AddDatum(ADatum : Double; ANumTimes : UInt32); overload;
    procedure RemoveDatum(ADatum : Double);
end;

resourcestring
  sNotImplemented = 'Not implemented';
  sInvalidParameter_OutOfBounds = 'Invalid Parameter: %s out of bounds';
  sLogDebug = 'DEBUG';
  sLogInfo = 'INFO';
  sLogWarn = 'WARNING';
  sLogError = 'ERROR';

implementation

uses dateutils, StrUtils;

const
  IntlDateTimeFormat : TFormatSettings = (
    DateSeparator : '-';
    TimeSeparator : ':';
    ShortDateFormat : 'yyyy/mm/dd';
    LongDateFormat : ' yyyy/mm/dd';
    ShortTimeFormat : 'hh:nn:zzz';
    LongTimeFormat : 'hh:nn:zzz'
  );

  {$IFDEF FPC}
  MinTimeSpan : TTimeSpan = (FMillis: Low(Int64));
  MaxTimeSpan: TTimeSpan = (FMillis: High(Int64));
  ZeroTimeSpan: TTimeSpan = (FMillis: 0);

  {$IF DEFINED(LINUX)}
  _SC_NPROCESSORS_ONLN = 83;

  function sysconf(i: cint): clong; cdecl; external Name 'sysconf';
  {$ENDIF LINUX}
  {$ENDIF}


var
  MinTimeStampDateTime : TDateTime = 0;
  VarTrue : Variant;
  VarFalse : Variant;
  GMainThreadNotify : TThreadNotify;

{ Global helper functions }

function String2Hex(const Buffer: String): String;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Buffer) do
    Result := AnsiLowerCase(Result + IntToHex(Ord(Buffer[n]), 2));
end;

function Hex2Bytes(const AHexString: String): TBytes;
begin
  if NOT TryHex2Bytes(AHexString, Result) then
    raise EArgumentOutOfRangeException.Create('Invalidly formatted hexadecimal string.');
end;

function TryHex2Bytes(const AHexString: String; out ABytes : TBytes): boolean; overload;
var
  P : PAnsiChar;
  LHexString : String;
  LHexIndex, LHexLength, LHexStart : Integer;
begin
  SetLength(ABytes, 0);
  LHexLength := System.Length(AHexString);
  LHexStart := 1;
  if {$IFDEF FPC}AnsiStartsText{$ELSE}StartsText{$ENDIF}('0x', AHexString) then begin

    // Special case: 0x0 = empty byte array
    if (LHexLength = 3) AND (AHexString[3] = '0') then
      Exit(true);
    dec(LHexLength, 2);
    inc(LHexStart, 2);
  end;

  if (LHexLength MOD 2) <> 0 then
    Exit(false);

  if LHexLength = 0 then
    Exit(true);

  SetLength(ABytes, LHexLength DIV 2);

  LHexString := LowerCase(AHexString);
  {$IFDEF FPC}
  P := @ABytes[Low(ABytes)];
  LHexIndex := HexToBin(PAnsiChar(@LHexString[LHexStart]), P, System.Length(ABytes));
  {$ELSE}
  LHexIndex := HexToBin(@LHexString[LHexStart],0,ABytes,0,Length(ABytes));
  {$ENDIF}
  Result := (LHexIndex = (LHexLength DIV 2));
end;


function Bytes2Hex(const ABytes: TBytes; AUsePrefix : boolean = false) : String;
var
  i, LStart, LLen : Integer;
  s : String;
  b : Byte;
begin
  LLen := System.Length(ABytes)*2;
  if LLen = 0 then
    Exit(IIF(AUsePrefix, '0x0', ''));

  if AUsePrefix then
    inc(LLen, 2);

  System.SetLength(Result, LLen);
  i := 0;
  LStart := 1;
  if AUsePrefix then
    inc(LStart, 2);

  if AUsePrefix then begin
    Result[1] := '0';
    Result[2] := 'x';
  end;

  for b in ABytes do begin
    s := IntToHex(b,2);
    Result[(i*2)+ LStart] := s[1];
    Result[(i*2)+ LStart + 1] := s[2];
    Inc(i);
  end;
end;

function BinStrComp(const Str1, Str2: String): integer;
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

function BytesCompare(const ABytes1, ABytes2: TBytes): integer;
var ABytes1Len, ABytes2Len, i : Integer;
begin
   ABytes1Len := Length(ABytes1);
   ABytes2Len := Length(ABytes2);
   if (ABytes1Len < ABytes2Len) then
     Result := -1
   else if (ABytes1Len > ABytes2Len) then
     Result := 1
   else begin
     Result := 0;
     for i:= Low(ABytes1) to High(ABytes1) do begin
       if ABytes1[i] < ABytes2[i] then begin
         Result := -1;
         break;
       end else if ABytes1[i] > ABytes2[i] then begin
         Result := 1;
         break;
       end
     end;
   end;
end;

function BytesEqual(const ABytes1, ABytes2 : TBytes) : boolean;
begin
  Result := BytesEqual(ABytes1, ABytes2, 0, Length(ABytes1));
end;

function BytesEqual(const ABytes1, ABytes2 : TBytes; AFrom, ALength : UInt32) : boolean;
var ABytes1Len, ABytes2Len : Integer;
begin
  if ALength = 0 then
    Exit(False);
  ABytes1Len := Length(ABytes1);
  ABytes2Len := Length(ABytes2);
  if ((ABytes1Len - AFrom) < ALength) OR ((ABytes2Len - AFrom) < ALength ) then
    Exit(False);
  Result := CompareMem(@ABytes1[AFrom], @ABytes2[AFrom], ALength);
end;

function ContencateBytes(const AChunk1, AChunk2: TBytes): TBytes;
begin
  SetLength(Result, Length(AChunk1) + Length(AChunk2));
  Move(AChunk1[0], Result[0], Length(AChunk1));
  Move(AChunk2[0], Result[Length(AChunk1)], Length(AChunk2));
end;

function SetLastDWordLE(const ABytes: TBytes; AValue: UInt32): TBytes;
var
  ABytesLength : Integer;
begin
  // Clone the original header
  Result := Copy(ABytes);

  // If digest not big enough to contain a nonce, just return the clone
  ABytesLength := Length(ABytes);
  if ABytesLength < 4 then
    exit;

  // Overwrite the nonce in little-endian
  Result[ABytesLength - 4] := Byte(AValue);
  Result[ABytesLength - 3] := (AValue SHR 8) AND 255;
  Result[ABytesLength - 2] := (AValue SHR 16) AND 255;
  Result[ABytesLength - 1] := (AValue SHR 24) AND 255;
end;

function GetLastDWordLE(const ABytes: TBytes) : UInt32;
var LLen : Integer;
begin
  LLen := Length(ABytes);
  if LLen < 4 then
   raise EArgumentException.Create('ABytes needs to be at least 4 bytes');

  // Last 4 bytes are nonce (LE)
  Result := ABytes[LLen - 4] OR
           (ABytes[LLen - 3] SHL 8) OR
           (ABytes[LLen - 2] SHL 16) OR
           (ABytes[LLen - 1] SHL 24);
end;

function GetDWordLE(const ABytes: TBytes; AOffset : Integer) : UInt32;
var LLen : Integer;
begin
  LLen := Length(ABytes);
  if LLen < AOffset+3 then
   raise EArgumentException.Create('ABytes[AOffset] needs at least 4 more bytes');

  // Last 4 bytes are nonce (LE)
  Result := ABytes[AOffset + 0] OR
           (ABytes[AOffset + 1] SHL 8) OR
           (ABytes[AOffset + 2] SHL 16) OR
           (ABytes[AOffset + 3] SHL 24);
end;


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
  if Length(AArray) = 0 then raise EArgumentException.Create('AArray is empty');
  Result := AArray[Low(AArray)];
  for i := Low(AArray) to High(AArray) do begin
    if Result > AArray[i] then
      Result := AArray[i];
  end;
end;

function MaxValue(const AArray : array of Cardinal) : Cardinal;
var i : SizeInt;
begin
  if Length(AArray) = 0 then raise EArgumentException.Create('AArray is empty');
  Result := AArray[Low(AArray)];
  for i := Low(AArray) to High(AArray) do begin
    if Result < AArray[i] then
      Result := AArray[i];
  end;
end;

function RoundEx(const AInput: Single; APlaces: Integer): Single;
var
  k: Single;
begin
  if APlaces = 0 then begin
    Result := Round(AInput);
  end else begin
    if APlaces > 0 then begin
      k := Power(10, APlaces);
      Result := Round(AInput * k) / k;
    end else begin
      k := Power(10, (APlaces*-1));
      Result := Round(AInput / k) * k;
    end;
  end;
end;

function RoundEx(const AInput: Double; APlaces: Integer): Double;
var
  k: Double;
begin
  if APlaces = 0 then begin
    Result := Round(AInput);
  end else begin
    if APlaces > 0 then begin
      k := Power(10, APlaces);
      Result := Round(AInput * k) / k;
    end else begin
      k := Power(10, (APlaces*-1));
      Result := Round(AInput / k) * k;
    end;
  end;
end;

function RoundEx(const AInput: Currency; APlaces: integer): Currency;
var
  k: Currency;
begin
  if APlaces = 0 then begin
    Result := Round(AInput);
  end else begin
    if APlaces > 0 then begin
      k := Power(10, APlaces);
      Result := Round(AInput * k) / k;
    end else begin
      k := Power(10, (APlaces*-1));
      Result := Round(AInput / k) * k;
    end;
  end;
end;



{$IFDEF FPC}

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

{$ENDIF}

{ TNullable }

function TNullable<T>.GetValue: T;
begin
  if FHasValue then
    Result := FValue
  else
    raise Exception.Create('Variable has no value');
end;

procedure TNullable<T>.SetValue(AValue: T);
begin
  FValue := AValue;
  FHasValue := True;
end;

procedure TNullable<T>.Clear;
begin
  FHasValue := False;
end;

class operator TNullable<T>.Implicit(A: T): TNullable<T>;
begin
  Result.Value := A;
end;

class operator TNullable<T>.Implicit(A: Pointer): TNullable<T>;
begin
  if A = nil then
    Result.Clear
  else
    raise Exception.Create('Pointer value not allowed');
end;


{$IFDEF FPC}

{ TTimeSpan }

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

class function TTimeSpan.GetMinValue : TTimeSpan;
begin
  Result := MinTimeSpan;
end;

class function TTimeSpan.GetMaxValue : TTimeSpan; static; inline;
begin
  Result := MaxTimeSpan;
end;

class function TTimeSpan.GetZeroValue : TTimeSpan; static; inline;
begin
  Result := ZeroValue;
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

{$ENDIF}

{ TBox }

class constructor TBox<T>.Create;
begin
  Instances := 0;
end;

constructor TBox<T>.Create(const AValue: T);
begin
  Inherited Create;
  Inc(Instances);
  FValue := AValue;
end;

destructor TBox<T>.Destroy;
begin
  Inherited;
  Dec(Instances);
end;

{ TLogMessage }

class function TLogMessage.From(const AText : String; ASeverity : TLogSeverity; ATimeStamp : TDateTime) : TLogMessage;
begin
  Result := From(AText, lsError);
end;

class function TLogMessage.From(const AText : String; ASeverity : TLogSeverity = lsError) : TLogMessage;
begin
  Result.FTimeStamp := TDateTime.NowUtc;
  Result.FSeverity := ASeverity;
  Result.FText := AText;
end;

{ TResult }

function TResult.GetIsSuccess : boolean;
begin
  Result := NOT FHasError;
end;

procedure TResult.Add(const ALogMessage : TLogMessage);
begin
  TArrayTool<TLogMessage>.Add(FMessages, ALogMessage);
  if ALogMessage.Severity = lsError then
    Self.FHasError := true;
end;

procedure TResult.Add(ASeverity : TLogSeverity; const AString : String);
begin
  Add(TLogMessage.From(AString, ASeverity));
end;

procedure TResult.AddDebug(const AString : String);
begin
  Add(lsDebug, AString);
end;

procedure TResult.AddInfo(const AString : String);
begin
  Add(lsInfo, AString);
end;

procedure TResult.AddWarning(const AString : String);
begin
  Add(lsWarning, AString);
end;

procedure TResult.AddError(const AString : String);
begin
  Add(lsError, AString);
end;

function TResult.ToString(AIncludeTimeStamp : boolean = false) : String;
begin
  Result := ToString(AIncludeTimeStamp, false);
end;

function TResult.ToString(AIncludeTimeStamp, AIncludeSeverity : boolean) : String;
var
  i : integer;
  LLine : String;
begin
  Result := '';
  for i := Low(Messages) to High(Messages) do begin
    LLine := '';
    if AIncludeTimeStamp then
      LLine := LLine + '[' + Messages[i].TimeStamp.ToIntlString + '] ';
    if AIncludeSeverity then
      case Messages[i].Severity of
        lsDebug: LLine := LLine + ' ' + sLogDebug + ': ';
        lsInfo: LLine := LLine + ' ' + sLogInfo + ': ';
        lsWarning: LLine := LLine + ' ' + sLogWarn + ': ';
        lsError: LLine := LLine + ' ' + sLogError + ': ';
      end;
    LLine := LLine + Messages[i].Text;
    if i < High(Messages) then
      LLine := LLine + sLineBreak;
    Result := Result + LLine;
  end;
end;

class function TResult.Success : TResult;
begin
  SetLength(Result.FMessages, 0);
  Result.FValue := Variants.Null;
  Result.FHasError := false;
end;

class function TResult.Success(const AText : String) : TResult;
begin
  Result := Success;
  Result.AddInfo(AText);
end;

class function TResult.Failure : TResult;
begin
  SetLength(Result.FMessages, 0);
  Result.FValue := Variants.Null;
  Result.FHasError := true;
end;

class function TResult.Failure(const AText : String) : TResult;
begin
  Result := Failure;
  Result.AddError(AText);
end;

{ TDateTimeHelper }

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

class function TDateTimeHelper.GetNowUtc: TDateTime;
begin
{$IFDEF FPC}
  Result := LocalTimeToUniversal(SysUtils.Now);
{$ELSE}
  Result := TTimeZone.Local.ToUniversalTime(SysUtils.Now);
{$ENDIF}
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

function TDateTimeHelper.ToIntlString : String;
begin
  Result := ToString('yyy-mm-dd hh:nn:ss');
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

{ TNotifyManyEventHelper }

procedure TNotifyManyEventHelper.Add(AHandler : TNotifyEvent; ExecuteMainThread : Boolean = False);
begin
  if NOT ExecuteMainThread then begin
    if TArrayTool<TNotifyEvent>.IndexOf(self.Handlers, AHandler) = -1 then
      TArrayTool<TNotifyEvent>.Add(self.Handlers, AHandler)
  end else begin
    if TArrayTool<TNotifyEvent>.IndexOf(self.MainThreadHandlers, AHandler) = -1 then
      TArrayTool<TNotifyEvent>.Add(self.MainThreadHandlers, AHandler);
  end;
end;

procedure TNotifyManyEventHelper.Remove(AHandler : TNotifyEvent);
begin
  TArrayTool<TNotifyEvent>.Remove(self.Handlers, AHandler);
  TArrayTool<TNotifyEvent>.Remove(self.MainThreadHandlers, AHandler);
end;

procedure TNotifyManyEventHelper.Invoke(sender : TObject);
var i : Integer;
begin
  for i := low(self.Handlers) to high(self.Handlers) do
    self.Handlers[i](sender);

  if Length(self.MainThreadHandlers) > 0 then
    TThreadNotify.InvokeMainThread(sender, self.MainThreadHandlers);
end;

{ TThreadNotify }

constructor TThreadNotify.Create(ATargetThread : TThread);
begin
  FTargetThread := ATargetThread;
  FLock := TCriticalSection.Create;
  FPendingNotifications := TList<TPendingNotifyManyEvent>.Create;
end;

destructor TThreadNotify.Destroy;
begin
  FTargetThread := nil;
  FLock.Acquire;
  try
    FPendingNotifications.Destroy;
  finally
    FLock.Release;
    FLock.Destroy;
  end;
end;

procedure TThreadNotify.InvokePendingOnTargetThread;
var
  LPendings : TArray<TPendingNotifyManyEvent>;
  LPending : TPendingNotifyManyEvent;
  LNotify : TNotifyEvent;
begin
  if (NOT Assigned(FTargetThread)) OR (NOT Assigned(FLock)) OR (NOT Assigned(FPendingNotifications)) then
    exit;

  if TThread.CurrentThread.ThreadID = FTargetThread.ThreadID then begin
    FLock.Acquire;
    try
      LPendings := FPendingNotifications.ToArray;
      FPendingNotifications.Clear;
    finally
      FLock.Release;
    end;
    for LPending in LPendings do
      for LNotify in LPending.Handlers do
        LNotify(LPending.Sender);
  end else TThread.Queue(FTargetThread, InvokePendingOnTargetThread);
end;

procedure TThreadNotify.Invoke(Sender: TObject; Handler : TNotifyEvent);
begin
  Invoke(Sender, TArrayTool<TNotifyEvent>.Create(Handler));
end;

procedure TThreadNotify.Invoke(Sender: TObject; const Handlers: TArray<TNotifyEvent>);
var
  LPending : TPendingNotifyManyEvent;
begin
  FLock.Acquire;
  try
    LPending.Sender := Sender;
    LPending.Handlers := Handlers;
    FPendingNotifications.Add(LPending);
  finally
    FLock.Release;
  end;
  InvokePendingOnTargetThread;
end;

class procedure TThreadNotify.InvokeMainThread(Sender: TObject; Handler : TNotifyEvent);
begin
  InvokeMainThread(Sender, TArrayTool<TNotifyEvent>.Create(Handler));
end;

class procedure TThreadNotify.InvokeMainThread(Sender: TObject; const Handlers: TArray<TNotifyEvent>);
begin
  GMainThreadNotify.Invoke(Sender, Handlers);
end;

{ TArrayTool }

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

{ TVariantTool }

class function TVariantTool.IsBool(const AValue : Variant) : boolean;
begin
{$IFDEF FPC}
  Result := VarIsBool(AValue);
{$ELSE}
  Result := VarIsType(AValue, VarBoolean);
{$ENDIF}
end;

class function TVariantTool.IsNumeric(const AValue : Variant) : boolean;
begin
  // VarIsNumeric seems to be broken
  case VarType(AValue) of
    varsmallint, varinteger, varsingle,
    vardouble, varcurrency, varboolean, {$IFDEF FPC}vardecimal,{$ENDIF}
    varshortint, varbyte, varword, varlongword, varint64 {$IFDEF FPC},varqword {$ENDIF} : Result := true;
    else Result := false;
  end;
end;

class function TVariantTool.CompareVariant(const ALeft, ARight : Variant) : Integer;
begin
{$IFDEF FPC}
  Result := TCompare.Variant(@ALeft, @ARight);
{$ELSE}
  Result := Integer(VarCompareValue(ALeft, ARight));
{$ENDIF}
end;

class function TVariantTool.TryParseBool(const AValue : Variant; out ABoolean : boolean) : boolean;
var
  AValueStr : string;
begin
  ABoolean := false;
  Result := false;
  if IsBool(AValue) then begin
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

  IF IsBool(AValue) then begin
    if TryParseBool(AMatch, bmatch) then begin
      Result := (Boolean(AValue) = bmatch);
    end else begin
      Result := false;
      end
  end else begin
    Result := CompareVariant(AValue, AMatch) = 0;
  end;
end;

class function TVariantTool.NumericLT(const AValue, AMatch : Variant) : boolean;
begin
  if (NOT IsNumeric(AValue)) OR (IsBool(AValue)) then
    Exit(false);
  Result := CompareVariant(AValue, AMatch) = -1;
end;

class function TVariantTool.NumericLTE(const AValue, AMatch : Variant) : boolean;
var
  cmp : Integer;
begin
  if (NOT IsNumeric(AValue)) OR (IsBool(AValue)) then
    Exit(false);
  cmp := CompareVariant(AValue, AMatch);
  Result := (cmp = -1) OR (cmp = 0);
end;

class function TVariantTool.NumericGT(const AValue, AMatch : Variant) : boolean;
var
  cmp : Integer;
begin
  if (NOT IsNumeric(AValue)) OR (IsBool(AValue)) then
    Exit(false);
  cmp := CompareVariant(AValue, AMatch);
  Result := (cmp = 1);
end;

class function TVariantTool.NumericGTE(const AValue, AMatch : Variant) : boolean;
var
  cmp : Integer;
begin
  if (NOT IsNumeric(AValue)) OR (IsBool(AValue)) then
    Exit(false);
  cmp := CompareVariant(AValue, AMatch);
  Result := (cmp = 1) OR (cmp = 0);
end;

class function TVariantTool.NumericBetweenInclusive(const AValue, Lower, Upper : Variant) : boolean;
var
  lowercmp, uppercmp : Integer;
begin
  if (NOT IsNumeric(AValue)) OR (IsBool(AValue)) then
    Exit(false);
  lowercmp := CompareVariant(AValue, Lower);
  uppercmp := CompareVariant(AValue, Upper);
  Result := ((lowercmp = 1) OR (lowercmp = 0)) AND ((uppercmp = -1) OR (uppercmp = 0));
end;

class function TVariantTool.NumericBetweenExclusive(const AValue, Lower, Upper : Variant) : boolean;
var
  lowercmp, uppercmp : Integer;
begin
  if NOT IsNumeric(AValue) then
    Exit(false);
  lowercmp := CompareVariant(AValue, Lower);
  uppercmp := CompareVariant(AValue, Upper);
  Result := (lowercmp = 1) AND (uppercmp = -1);
end;

{ TStreamHelper }

{$IFNDEF FPC}
procedure TStreamHelper.WriteString(const AString : String);
begin
   Self.WriteBuffer(Pointer(AString)^, Length(AString));
end;
{$ENDIF}

function TStreamHelper.ReadBytes(ACount : Int32) : TBytes;
begin
  SetLength(Result, ACount);
  Read(Result, ACount);
end;

{ TMemoryStreamHelper }

function TMemoryStreamHelper.ToBytes(ASize : Integer = -1) : TBytes;
var
  LTakeAmount : Integer;
begin
  if ASize < 0 then
    LTakeAmount := Self.Size
  else
    LTakeAmount := ASize;
  SetLength(Result, LTakeAmount);
  Move(Self.Memory^, Result[0], LTakeAmount);
end;

{ TFileTool }

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
    fstream.{$IFDEF FPC}WriteAnsiString{$ELSE}WriteString{$ENDIF}(AText+#13#10);
  finally
    fstream.Free;
  end;
end;

{ TCPUTool }

class function TCPUTool.GetLogicalCPUCount(): Int32;
{$IFDEF FPC}
{$IFDEF WINDOWS}
var
  LIdx: Int32;
  LProcessAffinityMask, LSystemAffinityMask: DWORD_PTR;
  LMask: DWORD;
  LSystemInfo: SYSTEM_INFO;
{$ENDIF WINDOWS}
{$IF DEFINED(DARWIN) OR DEFINED(FREEBSD)}
var
  LMib: array[0..1] of cint;
  Llen, Lt: cint;
{$ENDIF}
{$ENDIF FPC}

begin
{$IFNDEF FPC}
  // For Delphi
  Result := System.CPUCount;
{$ELSE}
{$IF DEFINED(WINDOWS)}
  //returns total number of processors available to system including logical hyperthreaded processors
  if GetProcessAffinityMask(GetCurrentProcess, LProcessAffinityMask,
    LSystemAffinityMask) then
  begin
    Result := 0;
    for LIdx := 0 to 31 do
    begin
      LMask := DWORD(1) shl LIdx;
      if (LProcessAffinityMask and LMask) <> 0 then
      begin
        System.Inc(Result);
      end;
    end;
  end
  else
  begin
    // can't get the affinity mask so we just report the total number of processors
    GetSystemInfo(LSystemInfo);
    Result := LSystemInfo.dwNumberOfProcessors;
  end;
  {$ELSEIF DEFINED(DARWIN) OR DEFINED(FREEBSD)}

  LMib[0] := CTL_HW;
  LMib[1] := HW_NCPU;
  Llen := System.SizeOf(Lt);
  {$IF DEFINED(VER3_0_0) OR DEFINED(VER3_0_2)}
  fpsysctl(PChar(@LMib), 2, @Lt, @Llen, nil, 0);
  {$ELSE}
  fpsysctl(@LMib, 2, @Lt, @Llen, nil, 0);
  {$ENDIF}
  Result := Lt;

  {$ELSEIF DEFINED(LINUX)}
  Result := sysconf(_SC_NPROCESSORS_ONLN);
  {$ELSE}
  // Fallback for other platforms
  Result := 1;
{$ENDIF WINDOWS}
{$ENDIF FPC}
end;

{ TStatistics }

function TStatistics.Mean : Double;
begin
  Result := NaN;
  if SampleCount > 0 then
    Result := Sum / SampleCount;
end;

function TStatistics.PopulationStandardDeviation : Double;
begin
  Result := Sqrt(PopulationVariance);
end;

function TStatistics.PopulationVariance : Double;
var LSum : Double;
begin
  LSum := Sum;
  if SampleCount > 2 then
    Result := ((SampleCount * SquaredSum) - (LSum * LSum)) / (SampleCount * SampleCount)
  else
    Result := Nan;
end;

function TStatistics.PopulationVariationCoefficient : Double;
begin
  if SampleCount > 0 then
    Result :=  (PopulationVariance / Mean) * 100.0
  else
    Result := Nan;
end;

(*function TStatistics.GeometricMean : Double;
begin
  if SampleCount > 0 then
    Result :=  Power(Product, 1.0 / SampleCount)
  else
    Result := Nan;
end;*)

function TStatistics.HarmonicMean : Double;
begin
  if SampleCount > 0 then
    Result := SampleCount / ReciprocalSum
  else
    Result := Nan;
end;

function TStatistics.MinimumError : Double;
begin
  if (Mean * Mean) > (EPSILON * EPSILON) then
    Result := 100.0 * (Minimum - Mean) / Mean
  else
    Result := Nan;
end;

function TStatistics.MaximumError : Double;
begin
  if (Mean * Mean) > (EPSILON * EPSILON) then
    Result := 100.0 * (Maximum - Mean) / Mean
  else
    Result := Nan;
end;

function TStatistics.SampleStandardDeviation : Double;
begin
  if SampleCount >= 2 then
    Result := Sqrt(SampleVariance)
  else
    Result := Nan;
end;

function TStatistics.SampleVariance : Double;
var LSum : Double;
begin
  LSum := Sum;
  if SampleCount > 2 then
    Result := ((SampleCount * SquaredSum) - (LSum * LSum)) / ((SampleCount - 1) * (SampleCount - 1))
  else
    Result := Nan;
end;

function TStatistics.SampleVariationCoefficient : Double;
begin
  if SampleCount >= 2 then
    Result := 100 * (SampleStandardDeviation / Mean)
  else
    Result := Nan;
end;

procedure TStatistics.Reset;
begin
  FCount := 0;
  FMin := 0.0;
  FMax := 0.0;
  FTotal := 0.0;
  FTotal2 := 0.0;
  FRecip := 0.0;
  //FProduct := 1.0;
end;

procedure TStatistics.AddDatum(ADatum : Double);
begin
  if FCount = 0 then
    Reset;
  Inc(FCount);
  FTotal := FTotal + ADatum;
  FTotal2 := FTotal2 + ADatum * ADatum;
  if IsNaN(FRecip) OR ((ADatum * ADatum) < (EPSILON * EPSILON)) then
    FRecip := double.NaN
  else
    FRecip := FRecip + (1.0 / ADatum);
  //FProduct := FProduct * ADatum;
  if (FCount = 1) then begin
    // first data so set _min/_max
    FMin := ADatum;
    FMax := ADatum;
  end else begin
    // adjust _min/_max boundaries if necessary
    if (ADatum < FMin) then
      FMin := ADatum;
    if (ADatum > FMax) then
      FMax := ADatum;
  end;
end;

procedure TStatistics.AddDatum(ADatum : Double; ANumTimes : UInt32);
begin
  if FCount = 0 then
    Reset;
  FCount := FCount + ANumTimes;
  FTotal := FTotal + ADatum * ANumTimes;
  FTotal2 := FTotal2 + ADatum * ADatum * ANumTimes;
  if IsNaN(FRecip) OR ((ADatum * ADatum) < (EPSILON * EPSILON)) then
    FRecip := NaN
  else
    FRecip := FRecip + (1.0 / ADatum) * ANumTimes;
  //FProduct := FProduct * Power(ADatum, ANumTimes);
  if (FCount = 1) then begin
    // first data so set _min/_max
    FMin := ADatum;
    FMax := ADatum;
  end else begin
    // adjust _min/_max boundaries if necessary
    if ADatum < FMin then
        FMin := ADatum;
    if ADatum > FMax then
        FMax := ADatum;
  end;
end;

procedure TStatistics.RemoveDatum(ADatum : Double);
begin
  if FCount = 0 then
    Exit;
  Dec(FCount);
  FTotal := FTotal - ADatum;
  FTotal2 := FTotal2 - ADatum * ADatum;
  FRecip := FRecip - (1.0 / ADatum);
  if ABS(ADatum) > EPSILON then
   //FProduct := FProduct / ADatum;
end;

initialization
  MinTimeStampDateTime:= StrToDateTime('1980-01-01 00:00:000', IntlDateTimeFormat);
  VarTrue := True;
  VarFalse := False;
  GMainThreadNotify := TThreadNotify.Create ( TThread.CurrentThread ); // unit initialization runs in main thread

finalization
  FreeAndNil(GMainThreadNotify);

end.
