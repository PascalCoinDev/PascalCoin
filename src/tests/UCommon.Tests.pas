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

implementation

uses
    Variants, Generics.Defaults, Generics.Collections, UCommon, UMemory,
    UCommon.Collections;

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

initialization
  RegisterTest(TSortedHashTests);
  RegisterTest(THexStringTests);
  RegisterTest(TResultTests);
  RegisterTest(TVariantToolTests);
end.

