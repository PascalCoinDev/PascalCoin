unit UCommon.Tests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TSortedHashTests = class(TTestCase)
    published
      procedure SortedHashSet_Large;
      procedure SortedHashSet_Clear;
      procedure SortedHashSet_AddRange;
  end;

  THexStringTests = class(TTestCase)
    published
      procedure Bytes2Hex_1;
      procedure Bytes2Hex_2;
      procedure Bytes2Hex_3;
      procedure Bytes2Hex_4;
      procedure Hex2Bytes_1;
      procedure Hex2Bytes_2;
      procedure Hex2Bytes_3;
      procedure Hex2Bytes_4;
  end;

  TLogMessageTests = class(TTestCase)
    published
      procedure ExplicitMetadataPreservesEverySeverity;
      procedure ExplicitInfoMessageKeepsResultSuccessful;
      procedure DefaultTimestampPreservesRequestedSeverity;
  end;

  TResultTests = class(TTestCase)
    published
      procedure Success;
      procedure Success_WithMessage;
      procedure Error;
      procedure Error_WithMessage;
      procedure IntValue;
      procedure StringValue;
      procedure Complex;
  end;

  TVariantToolTests = class(TTestCase)
    published
      procedure IsNumeric64BitIntegers;
      procedure IsNumericRejectsNonNumericValues;
      procedure NumericEQ;
      procedure NumericLT;
      procedure TVariantCompare_1;
      procedure TVariantCompare_2;
      procedure TVariantCompare_3;
      procedure TVariantCompare_4;
  end;

  TBoxTests = class(TTestCase)
    published
      procedure TestStringList;
      procedure TestStringListReleaseOnly;
  end;

  TFileToolTests = class(TTestCase)
    private
      procedure CheckAppends(const ATexts: array of String; const AExpected: array of Byte);
    published
      procedure EmptyAppendWritesOnlyCRLF;
      procedure ASCIIAppendHasNoBOMOrLengthPrefix;
      procedure UnicodeAppendIncludesSupplementaryCharacter;
      procedure RepeatedAppendsPreserveExactUTF8Lines;
  end;
  TStatsTests = class(TTestCase)
    published
      procedure TestBasic;
      procedure Test100kData;
      procedure VarianceOfThreeKnownValues;
      procedure VarianceSmallSampleDomains;
      procedure VarianceWithLargeCountDoesNotOverflow;
  end;

implementation

uses
    Variants, Math, Generics.Defaults, Generics.Collections, UCommon, UMemory,
    UCommon.Collections;

{ TLogMessageTests }

procedure TLogMessageTests.ExplicitMetadataPreservesEverySeverity;
var
  LSeverity: TLogSeverity;
  LTimeStamp: TDateTime;
  LMessage: TLogMessage;
begin
  LTimeStamp := EncodeDate(2001, 2, 3) + EncodeTime(4, 5, 6, 7);
  for LSeverity := Low(TLogSeverity) to High(TLogSeverity) do begin
    LMessage := TLogMessage.From('Retained text', LSeverity, LTimeStamp);
    AssertEquals('Retained text', LMessage.Text);
    AssertEquals(Ord(LSeverity), Ord(LMessage.Severity));
    AssertTrue('The explicit timestamp must not be replaced with the current time',
      LMessage.TimeStamp = LTimeStamp);
  end;
end;

procedure TLogMessageTests.ExplicitInfoMessageKeepsResultSuccessful;
var
  LResult: TResult;
  LTimeStamp: TDateTime;
begin
  LTimeStamp := EncodeDate(2001, 2, 3);
  LResult := TResult.Success;
  LResult.Add(TLogMessage.From('Imported three rows', lsInfo, LTimeStamp));
  AssertTrue('An informational message is not a failure', LResult.IsSuccess);
  AssertEquals(1, Length(LResult.Messages));
  AssertEquals(Ord(lsInfo), Ord(LResult.Messages[0].Severity));
  AssertTrue(LResult.Messages[0].TimeStamp = LTimeStamp);
end;

procedure TLogMessageTests.DefaultTimestampPreservesRequestedSeverity;
var
  LSeverity: TLogSeverity;
  LBefore, LAfter: TDateTime;
  LMessage: TLogMessage;
begin
  for LSeverity := Low(TLogSeverity) to High(TLogSeverity) do begin
    LBefore := TDateTime.NowUtc;
    LMessage := TLogMessage.From('Current message', LSeverity);
    LAfter := TDateTime.NowUtc;
    AssertEquals(Ord(LSeverity), Ord(LMessage.Severity));
    AssertTrue((LMessage.TimeStamp >= LBefore) and (LMessage.TimeStamp <= LAfter));
  end;
  LMessage := TLogMessage.From('Default severity');
  AssertEquals(Ord(lsError), Ord(LMessage.Severity));
end;

{ TSortedHashTests }

procedure TSortedHashTests.SortedHashSet_Large;
var
  i : integer;
  c : Cardinal;
  LSet : TSortedHashSet<Cardinal>;
  GC : TDisposables;
begin
  LSet := GC.AddObject( TSortedHashSet<Cardinal>.Create ) as TSortedHashSet<Cardinal>;

  // Add 1 to 1000000 in non-ordered manner
  for i := 1 to 1000000 do begin
    if i mod 2 = 0 then LSet.Add(i);
  end;

  for i := 1000000 downto 1 do begin
    if i mod 2 = 1 then LSet.Add(i);
  end;

  // Add 1 to 1000000 again, should not do anything (since set)
  for i := 1 to 1000000 do begin
    LSet.Add(i);
  end;

  // Ensure enumerates ordered manner
  i := 1;
  for c in LSet do begin
    AssertEquals(i, c);
    Inc(i);
  end;
end;

procedure TSortedHashTests.SortedHashSet_Clear;
var
  i : integer;
  c : Cardinal;
  LSet : TSortedHashSet<Cardinal>;
  GC : TDisposables;
begin
  LSet := GC.AddObject( TSortedHashSet<Cardinal>.Create ) as TSortedHashSet<Cardinal>;

  for i := 1 to 1000 do  LSet.Add(i);
  LSet.Clear;
  for i := 1001 to 2000 do  LSet.Add(i);

  // Ensure enumerates ordered manner
  i := 1001;
  for c in LSet do begin
    AssertEquals(i, c);
    Inc(i);
  end;
end;

procedure TSortedHashTests.SortedHashSet_AddRange;
var
  i, j : integer;
  c : Cardinal;
  LVals : TArray<Cardinal>;
  LSet : TSortedHashSet<Cardinal>;
  GC : TDisposables;
begin
  LSet := GC.AddObject( TSortedHashSet<Cardinal>.Create ) as TSortedHashSet<Cardinal>;

  // Prepare addrange input
  SetLength(LVals, 1000);
  for i := 0 to 999 do
    LVals[i] := i;
  // shuffle randomly
  for i := 0 to 999 do begin
    TArrayTool<Cardinal>.Swap(LVals, i, Random(999));
  end;

  LSet.AddRange(LVals);
  LSet.Clear;
  LSet.AddRange(LVals);

  // Ensure enumerates ordered manner
  i := 0;
  for c in LSet do begin
    AssertEquals(i, c);
    Inc(i);
  end;
end;

{ THexStringTests }

procedure THexStringTests.Bytes2Hex_1;
var B : TBytes;
begin
  SetLength(B, 0);
  AssertEquals('', Bytes2Hex(B));
end;

procedure THexStringTests.Bytes2Hex_2;
begin
  AssertEquals('00FF', Bytes2Hex(TBytes.Create(0, 255)));
end;

procedure THexStringTests.Bytes2Hex_3;
begin
  AssertEquals('00FFFE', Bytes2Hex(TBytes.Create(0, 255, 254)));
end;

procedure THexStringTests.Bytes2Hex_4;
begin
  AssertEquals('0x00FFFE', Bytes2Hex(TBytes.Create(0, 255, 254), True));
end;

procedure THexStringTests.Hex2Bytes_1;
var B : TBytes;
begin
  SetLength(B, 0);
  AssertEquals(0, BytesCompare (B, Hex2Bytes('')));
end;

procedure THexStringTests.Hex2Bytes_2;
begin
  AssertEquals(0, BytesCompare (TBytes.Create(0, 255), Hex2Bytes('00FF')));
end;

procedure THexStringTests.Hex2Bytes_3;
begin
  AssertEquals(0, BytesCompare( TBytes.Create(0, 255, 254), Hex2Bytes('00FFFE')));
end;

procedure THexStringTests.Hex2Bytes_4;
begin
  AssertEquals(0, BytesCompare( Hex2Bytes('0x00FFFE'), Hex2Bytes('00FFFE')));
end;

{ TResultTests }

procedure TResultTests.Success;
  function Test : TResult;
  begin
    Result := TResult.Success;
  end;

var LResult : TResult;
begin
  LResult := Test;
  AssertTrue(LResult.IsSuccess);
  AssertTrue(VarIsNull(LResult.Value));
  AssertEquals(0, Length(LResult.Messages));
end;

procedure TResultTests.Success_WithMessage;
const C_Message = 'ALPHA BETA GAMMA';

  function Test : TResult;
  begin
    Result := TResult.Success(C_Message);
  end;

var LResult : TResult;
begin
  LResult := Test;
  AssertTrue(LResult.IsSuccess);
  AssertTrue(VarIsNull(LResult.Value));
  AssertEquals(1, Length(LResult.Messages));
  AssertEquals(LResult.Messages[0].Text, C_Message);
end;

procedure TResultTests.Error;
  function Test : TResult;
  begin
    Result := TResult.Failure;
  end;

var LResult : TResult;
begin
  LResult := Test;
  AssertTrue(LResult.IsFailure);
  AssertTrue(VarIsNull(LResult.Value));
  AssertEquals(0, Length(LResult.Messages));
end;

procedure TResultTests.Error_WithMessage;
const C_Message = 'DELTA EPSILON ZETA';

  function Test : TResult;
  begin
    Result := TResult.Failure(C_Message);
  end;

var LResult : TResult;
begin
  LResult := Test;
  AssertTrue(LResult.IsFailure);
  AssertTrue(VarIsNull(LResult.Value));
  AssertEquals(1, Length(LResult.Messages));
  AssertEquals(LResult.Messages[0].Text, C_Message);
end;

procedure TResultTests.IntValue;
const C_TestValue = -3;
  function Test : TResult;
  begin
    Result := TResult.Failure;
    Result.Value := Integer(C_TestValue);
  end;

var LResult : TResult;
begin
  LResult := Test;
  AssertEquals(Integer(C_TestValue), Integer(LResult.Value));
end;

procedure TResultTests.StringValue;
const C_TestValue = 'A string return value';
  function Test : TResult;
  begin
    Result := TResult.Failure;
    Result.Value := String(C_TestValue);
  end;

var LResult : TResult;
begin
  LResult := Test;
  AssertEquals(String(C_TestValue), String(LResult.Value));
end;

procedure TResultTests.Complex;
const C_TestValue = 'A string return value';

  function Test1 : TResult;
  begin
    Result := TResult.Success;
    Result.AddDebug('DEBUGMSG');
  end;

  function Test2(var AResult : TResult) : TResult;
  begin
    AResult.Value := String(C_TestValue);
    AResult.AddInfo('INFOMSG');
    AResult.AddWarning('WARNMSG');
    Result := AResult;
  end;

var LResult : TResult; LString : String;
begin
  LResult := Test1;
  LResult := Test2(LResult);
  LResult.AddError('ERRORMSG');
  AssertTrue(LResult.IsFailure);
  AssertEquals(String(C_TestValue), String(LResult.Value));
  LString := LResult.ToString(true, true);
  AssertTrue(LString.Contains('DEBUGMSG'));
  AssertTrue(LString.Contains('INFOMSG'));
  AssertTrue(LString.Contains('WARNMSG'));
  AssertTrue(LString.Contains('ERRORMSG'));
end;

{ TVariantToolTests }

procedure TVariantToolTests.IsNumeric64BitIntegers;
var
  LValue: Variant;
  LUnsigned: UInt64;
begin
  LValue := Low(Int64);
  AssertEquals(varInt64, VarType(LValue));
  AssertTrue(TVariantTool.IsNumeric(LValue));
  LValue := High(Int64);
  AssertTrue(TVariantTool.IsNumeric(LValue));

  // Recognize the type without narrowing a value which cannot fit in Int64.
  LUnsigned := UInt64(High(Int64)) + 1;
  LValue := LUnsigned;
  AssertEquals({$IFDEF FPC}varQWord{$ELSE}varUInt64{$ENDIF}, VarType(LValue));
  AssertTrue(TVariantTool.IsNumeric(LValue));
  LValue := High(UInt64);
  AssertTrue(TVariantTool.IsNumeric(LValue));
  AssertTrue(UInt64(LValue) = High(UInt64));
end;

procedure TVariantToolTests.IsNumericRejectsNonNumericValues;
begin
  AssertFalse(TVariantTool.IsNumeric(Null));
  AssertFalse(TVariantTool.IsNumeric(Unassigned));
  AssertFalse(TVariantTool.IsNumeric('16537985'));
  AssertFalse(TVariantTool.IsNumeric(VarArrayOf([1, 2])));
end;

procedure TVariantToolTests.NumericEQ;
var
  lbyte : byte;
  luint16 : uint16;
  lint : integer;
  lqword : qword;
  left, right : variant;
  cmp : boolean;
begin
  lbyte := 111;
  luint16 := 111;
  lint := 111;
  lqword := 111;

  left := Variant(lbyte);
  right := Variant(luint16);
  cmp := TVariantTool.NumericEq(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lbyte);
  right := Variant(lint);
  cmp := TVariantTool.NumericEq(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lbyte);
  right := Variant(lqword);
  cmp := TVariantTool.NumericEq(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lint);
  right := Variant(lqword);
  cmp := TVariantTool.NumericEq(left, right);
  self.AssertEquals(true, cmp);

end;

procedure TVariantToolTests.NumericLT;
var
  lbyte : byte;
  lint : integer;
  lqword : qword;
  left, right : variant;
  cmp : boolean;
begin
  lbyte := 111;
  lint := 11111;
  lqword := 1111111111111111111;

  left := Variant(lbyte);
  right := Variant(lint);
  cmp := TVariantTool.NumericLT(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lbyte);
  right := Variant(lqword);
  cmp := TVariantTool.NumericLT(left, right);
  self.AssertEquals(true, cmp);

  left := Variant(lint);
  right := Variant(lqword);
  cmp := TVariantTool.NumericLT(left, right);
  self.AssertEquals(true, cmp);

end;

procedure TVariantToolTests.TVariantCompare_1;
var
  lval : byte;
  rval : integer;
  left, right : variant;
  cmp : integer;
begin
  lval := 111;
  rval := 111;
  left := lval;
  right := rval;
  cmp := TCompare.Variant(@left, @right);
  self.AssertEquals(0, cmp);
  cmp := TCompare.Variant(@Right, @Left);
  self.AssertEquals(0, cmp);
end;

procedure TVariantToolTests.TVariantCompare_2;
var
  lval : integer;
  rval : qword;
  left, right : variant;
  cmp : integer;
begin
  lval := 111;
  rval := 111;
  left := lval;
  right := rval;
  cmp := TCompare.Variant(@left, @right);
  self.AssertEquals(0, cmp);
  cmp := TCompare.Variant(@Right, @Left);
  self.AssertEquals(0, cmp);
end;

procedure TVariantToolTests.TVariantCompare_3;
var
  lval : real;
  rval : qword;
  left, right : variant;
  cmp : integer;
begin
  lval := 111.0;
  rval := 111;
  left := lval;
  right := rval;
  cmp := TCompare.Variant(@left, @right);
  self.AssertEquals(0, cmp);
  cmp := TCompare.Variant(@Right, @Left);
  self.AssertEquals(0, cmp);
end;

procedure TVariantToolTests.TVariantCompare_4;
var
  lval : uint16;
  rval : qword;
  left, right : variant;
  cmp : integer;
begin
  lval := 10001;
  rval := 10001;
  left := lval;
  right := rval;
  cmp := TCompare.Variant(@left, @right);
  self.AssertEquals(0, cmp);
  cmp := TCompare.Variant(@Right, @Left);
  self.AssertEquals(0, cmp);
end;

{ TBoxTests }

procedure TBoxTests.TestStringList;

  type
    TTestRecord = record
      AInt : Integer;
      AString : String;
    end;

var rec : TTestRecord; box : TBox<TTestRecord>; strList : TStringList;
begin
  strList := TStringList.Create;
  strList.OwnsObjects:=true;
  rec.AInt := 11;
  rec.AString := 'ABC';
  box := TBox<TTestRecord>.Create(rec);
  strList.AddObject('first', box);
  Self.AssertEquals(1, TBox<TTestRecord>.Instances);
  strList.Clear;
  Self.AssertEquals(0, TBox<TTestRecord>.Instances);
  strList.Free;
end;

procedure TBoxTests.TestStringListReleaseOnly;

  type
    TTestRecord = record
      AInt : Integer;
      AString : String;
    end;

var rec : TTestRecord; box : TBox<TTestRecord>;  ref : TBox<TTestRecord>; strList : TStringList;
begin
  strList := TStringList.Create;
  strList.OwnsObjects:=true;
  rec.AInt := 11;
  rec.AString := 'ABC';
  box := TBox<TTestRecord>.Create(rec);
  strList.AddObject('first', box);
  Self.AssertEquals(1, TBox<TTestRecord>.Instances);
  ref := TBox<TTestRecord>( strList.Objects[0] );
  strList.Clear;
  Self.AssertEquals(0, TBox<TTestRecord>.Instances);  // owned object collected anyway
  ref := nil;
  Self.AssertEquals(0, TBox<TTestRecord>.Instances);
  strList.Free;
end;

{ TFileToolTests }

function FileExampleUnicodeText: String;
var LText: UnicodeString;
  {$IFDEF FPC}LUTF8: UTF8String;{$ENDIF}
begin
  LText := #$0041#$03A9#$6F22#$D83D#$DE80;
  {$IFDEF FPC}
  // Emulate Lazarus UTF-8 String bytes even in a console using Windows ANSI.
  // An ordinary assignment would transcode the fixture before AppendText runs.
  LUTF8 := UTF8Encode(LText);
  SetLength(Result, Length(LUTF8));
  Move(LUTF8[1], Result[1], Length(LUTF8));
  {$ELSE}
  Result := LText;
  {$ENDIF}
end;

procedure TFileToolTests.CheckAppends(const ATexts: array of String;
  const AExpected: array of Byte);
var
  LGuid: TGUID;
  LFileName: String;
  LFile: TFileStream;
  LActual: TBytes;
  I: Integer;
begin
  CreateGUID(LGuid);
  LFileName := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
    'Sphere10UTF8-' + GUIDToString(LGuid) + '.tmp';
  try
    for I := 0 to High(ATexts) do TFileTool.AppendText(LFileName, ATexts[I]);
    LFile := TFileStream.Create(LFileName, fmOpenRead or fmShareDenyWrite);
    try
      AssertEquals('Exact byte length, without a BOM or length prefix',
        Int64(Length(AExpected)), LFile.Size);
      SetLength(LActual, Length(AExpected));
      if Length(LActual) > 0 then LFile.ReadBuffer(LActual[0], Length(LActual));
    finally
      LFile.Free;
    end;
    for I := 0 to High(AExpected) do
      AssertEquals('UTF-8 byte ' + IntToStr(I), Integer(AExpected[I]), Integer(LActual[I]));
  finally
    if FileExists(LFileName) then DeleteFile(LFileName);
  end;
end;

procedure TFileToolTests.EmptyAppendWritesOnlyCRLF;
begin
  CheckAppends([''], [$0D, $0A]);
end;

procedure TFileToolTests.ASCIIAppendHasNoBOMOrLengthPrefix;
begin
  CheckAppends(['ABC'], [$41, $42, $43, $0D, $0A]);
end;

procedure TFileToolTests.UnicodeAppendIncludesSupplementaryCharacter;
begin
  CheckAppends([FileExampleUnicodeText],
    [$41, $CE, $A9, $E6, $BC, $A2, $F0, $9F, $9A, $80, $0D, $0A]);
end;

procedure TFileToolTests.RepeatedAppendsPreserveExactUTF8Lines;
begin
  CheckAppends([FileExampleUnicodeText, '', 'END'],
    [$41, $CE, $A9, $E6, $BC, $A2, $F0, $9F, $9A, $80, $0D, $0A,
     $0D, $0A, $45, $4E, $44, $0D, $0A]);
end;
{ TStatsTests }

procedure TStatsTests.TestBasic;
var
  LStats : TStatistics;
begin
  LStats.Reset;
  LStats.AddDatum(1);
  LStats.AddDatum(2);
  LStats.AddDatum(4);
  LStats.AddDatum(5);
  LStats.AddDatum(8);

  AssertEquals(5, LStats.SampleCount);
  AssertEquals(1, LStats.Minimum);
  AssertEquals(8, LStats.Maximum);
  AssertEquals(4, LStats.Mean);
  AssertEquals(6, LStats.PopulationVariance);
  AssertEquals(RoundEx(2.4495, 4), RoundEx(LStats.PopulationStandardDeviation, 4));
  AssertEquals(7.5, LStats.SampleVariance);
  AssertEquals(2.7386, RoundEx(LStats.SampleStandardDeviation, 4));
end;


procedure TStatsTests.Test100kData;
var
  LStats : TStatistics;
  i : integer;
begin
  LStats.Reset;
  for i := 1 to 100000 do
    LStats.AddDatum(i);
  AssertEquals(1, LStats.Minimum);
  AssertEquals(100000, LStats.Maximum);
end;

procedure TStatsTests.VarianceOfThreeKnownValues;
var LStats: TStatistics;
begin
  LStats.Reset;
  LStats.AddDatum(1);
  LStats.AddDatum(2);
  LStats.AddDatum(3);
  AssertEquals(2.0 / 3.0, LStats.PopulationVariance, EPSILON);
  AssertEquals(1.0, LStats.SampleVariance, EPSILON);
end;

procedure TStatsTests.VarianceSmallSampleDomains;
var LStats: TStatistics;
begin
  LStats.Reset;
  AssertTrue('Empty population variance is undefined', IsNaN(LStats.PopulationVariance));
  AssertTrue('Empty sample variance is undefined', IsNaN(LStats.SampleVariance));
  LStats.AddDatum(1);
  AssertEquals(0.0, LStats.PopulationVariance);
  AssertTrue('One value cannot estimate sample variance', IsNaN(LStats.SampleVariance));
  LStats.AddDatum(3);
  AssertEquals(1.0, LStats.PopulationVariance);
  AssertEquals(2.0, LStats.SampleVariance);
end;

procedure TStatsTests.VarianceWithLargeCountDoesNotOverflow;
var LStats: TStatistics;
begin
  LStats.Reset;
  LStats.AddDatum(1, 100000);
  AssertEquals(0.0, LStats.PopulationVariance);
  AssertEquals(0.0, LStats.SampleVariance);
end;

initialization
  RegisterTest(TSortedHashTests);
  RegisterTest(THexStringTests);
  RegisterTest(TLogMessageTests);
  RegisterTest(TResultTests);
  RegisterTest(TVariantToolTests);
  RegisterTest(TFileToolTests);
  RegisterTest(TStatsTests);
end.

