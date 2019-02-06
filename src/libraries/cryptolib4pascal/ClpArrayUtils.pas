{ *********************************************************************************** }
{ *                              CryptoLib Library                                  * }
{ *                Copyright (c) 2018 - 20XX Ugochukwu Mmaduekwe                    * }
{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpArrayUtils;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  Math,
  ClpCryptoLibTypes;

resourcestring
  SInvalidLength = '%d " > " %d';

type
  TArrayUtils = class sealed(TObject)

  strict private
    class function GetLength(from, &to: Int32): Int32; static; inline;

  public

    class function Concatenate(const A, B: TCryptoLibStringArray)
      : TCryptoLibStringArray; overload; static;

    class function Concatenate(const A, B: TCryptoLibByteArray)
      : TCryptoLibByteArray; overload; static; inline;

    class function Concatenate(const A: TCryptoLibByteArray;
      const Others: TCryptoLibMatrixByteArray): TCryptoLibByteArray;
      overload; static;

    class function AreEqual(const A, B: TCryptoLibByteArray): Boolean;
      overload; static;

    class function AreEqual(const A, B: TCryptoLibInt32Array): Boolean;
      overload; static;

    class function AreAllZeroes(const buf: TCryptoLibByteArray; off, len: Int32)
      : Boolean; static;

    class function GetArrayHashCode(const data: TCryptoLibByteArray): Int32;
      overload; static;

    class function GetArrayHashCode(const data: TCryptoLibInt32Array): Int32;
      overload; static;

    class function GetArrayHashCode(const data: TCryptoLibUInt32Array): Int32;
      overload; static;

    class function GetArrayHashCode(const data: TCryptoLibUInt32Array;
      off, len: Int32): Int32; overload; static;

    class function GetArrayHashCode(const data: TCryptoLibUInt64Array;
      off, len: Int32): Int32; overload; static;

    class function Prepend(const A: TCryptoLibByteArray; B: Byte)
      : TCryptoLibByteArray; static;

    class function CopyOf(const data: TCryptoLibByteArray; newLength: Int32)
      : TCryptoLibByteArray; static;

    class function CopyOfRange(const data: TCryptoLibByteArray;
      from, &to: Int32): TCryptoLibByteArray; static;

    class function ConstantTimeAreEqual(const a_ar1, a_ar2: TCryptoLibByteArray)
      : Boolean; static;

    class procedure Fill(const buf: TCryptoLibByteArray; from, &to: Int32;
      filler: Byte); overload; static;

    class procedure Fill(const buf: TCryptoLibInt32Array; from, &to: Int32;
      filler: Int32); overload; static;

    class procedure Fill(const buf: TCryptoLibUInt32Array; from, &to: Int32;
      filler: UInt32); overload; static;

  end;

implementation

{ TArrayUtils }

class function TArrayUtils.GetLength(from, &to: Int32): Int32;
var
  newLength: Int32;
begin
  newLength := &to - from;
  if (newLength < 0) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidLength,
      [from, &to]);
  end;
  Result := newLength;
end;

class function TArrayUtils.Concatenate(const A, B: TCryptoLibByteArray)
  : TCryptoLibByteArray;
var
  l: Int32;
begin
  l := System.Length(A);
  System.SetLength(Result, l + System.Length(B));
  System.Move(A[0], Result[0], l * System.SizeOf(Byte));
  System.Move(B[0], Result[l], System.Length(B) * System.SizeOf(Byte));
end;

class function TArrayUtils.Concatenate(const A: TCryptoLibByteArray;
  const Others: TCryptoLibMatrixByteArray): TCryptoLibByteArray;
var
  len, Idx, Pos: Int32;
  temp: TCryptoLibByteArray;
begin
  len := 0;
  for Idx := System.Low(Others) to System.High(Others) do
  begin
    len := len + System.Length(Others[Idx]);
  end;
  len := len + System.Length(A);
  System.SetLength(Result, len);
  System.Move(A[0], Result[0], System.Length(A) * System.SizeOf(Byte));
  Pos := System.Length(A);
  for Idx := System.Low(Others) to System.High(Others) do
  begin
    temp := Others[Idx];
    System.Move(temp[0], Result[Pos], System.Length(temp) *
      System.SizeOf(Byte));
    Pos := Pos + System.Length(temp);
  end;
end;

class function TArrayUtils.Concatenate(const A, B: TCryptoLibStringArray)
  : TCryptoLibStringArray;
var
  i, l: Int32;
begin
  l := System.Length(A);
  System.SetLength(Result, l + System.Length(B));
  for i := System.Low(A) to System.High(A) do
  begin
    Result[i] := A[i];
  end;

  for i := System.Low(B) to System.High(B) do
  begin
    Result[l + i] := B[i];
  end;
end;

class function TArrayUtils.AreEqual(const A, B: TCryptoLibByteArray): Boolean;
begin
  if System.Length(A) <> System.Length(B) then
  begin
    Result := false;
    Exit;
  end;

  Result := CompareMem(A, B, System.Length(A) * System.SizeOf(Byte));
end;

class function TArrayUtils.AreAllZeroes(const buf: TCryptoLibByteArray;
  off, len: Int32): Boolean;
var
  bits: UInt32;
  i: Int32;
begin
  bits := 0;
  for i := 0 to System.Pred(len) do
  begin
    bits := bits or (buf[off + i]);
  end;
  Result := bits = 0;
end;

class function TArrayUtils.AreEqual(const A, B: TCryptoLibInt32Array): Boolean;
begin
  if System.Length(A) <> System.Length(B) then
  begin
    Result := false;
    Exit;
  end;

  Result := CompareMem(A, B, System.Length(A) * System.SizeOf(Int32));
end;

{$B+}

class function TArrayUtils.ConstantTimeAreEqual(const a_ar1,
  a_ar2: TCryptoLibByteArray): Boolean;
var
  i: Int32;
  diff: UInt32;

begin
  diff := UInt32(System.Length(a_ar1)) xor UInt32(System.Length(a_ar2));

  i := 0;

  while (i <= System.High(a_ar1)) and (i <= System.High(a_ar2)) do
  begin
    diff := diff or (UInt32(a_ar1[i] xor a_ar2[i]));
    System.Inc(i);
  end;

  Result := diff = 0;
end;

{$B-}

class function TArrayUtils.CopyOf(const data: TCryptoLibByteArray;
  newLength: Int32): TCryptoLibByteArray;
begin
  System.SetLength(Result, newLength);
  if (newLength < System.Length(data)) then
  begin
    System.Move(data[0], Result[0], newLength * System.SizeOf(Byte));
  end
  else
  begin
    System.Move(data[0], Result[0], System.Length(data) * System.SizeOf(Byte));
  end;
end;

class function TArrayUtils.CopyOfRange(const data: TCryptoLibByteArray;
  from, &to: Int32): TCryptoLibByteArray;
var
  newLength: Int32;
begin
  newLength := GetLength(from, &to);
  System.SetLength(Result, newLength);
  System.Move(data[from], Result[0], Min(newLength, System.Length(data) - from)
    * System.SizeOf(Byte));
end;

class procedure TArrayUtils.Fill(const buf: TCryptoLibByteArray;
  from, &to: Int32; filler: Byte);
begin
  System.FillChar(buf[from], (&to - from) * System.SizeOf(Byte), filler);
end;

class procedure TArrayUtils.Fill(const buf: TCryptoLibInt32Array;
  from, &to: Int32; filler: Int32);
begin
  while from < &to do
  begin
    buf[from] := filler;
    System.Inc(from);
  end;
end;

class procedure TArrayUtils.Fill(const buf: TCryptoLibUInt32Array;
  from, &to: Int32; filler: UInt32);
begin
{$IFDEF FPC}
  System.FillDWord(buf[from], (&to - from), filler);
{$ELSE}
  while from < &to do
  begin
    buf[from] := filler;
    System.Inc(from);
  end;
{$ENDIF}
end;

class function TArrayUtils.GetArrayHashCode(const data
  : TCryptoLibByteArray): Int32;
var
  i, hc: Int32;
begin
  if data = Nil then
  begin
    Result := 0;
    Exit;
  end;

  i := System.Length(data);
  hc := i + 1;

  System.Dec(i);
  while (i >= 0) do
  begin
    hc := hc * 257;
    hc := hc xor data[i];
    System.Dec(i);
  end;
  Result := hc;
end;

class function TArrayUtils.GetArrayHashCode(const data
  : TCryptoLibInt32Array): Int32;
var
  i, hc: Int32;
begin
  if data = Nil then
  begin
    Result := 0;
    Exit;
  end;

  i := System.Length(data);
  hc := i + 1;

  System.Dec(i);
  while (i >= 0) do
  begin
    hc := hc * 257;
    hc := hc xor data[i];
    System.Dec(i);
  end;
  Result := hc;
end;

class function TArrayUtils.GetArrayHashCode(const data
  : TCryptoLibUInt32Array): Int32;
var
  i, hc: Int32;
begin
  if data = Nil then
  begin
    Result := 0;
    Exit;
  end;

  i := System.Length(data);
  hc := i + 1;

  System.Dec(i);
  while (i >= 0) do
  begin
    hc := hc * 257;
    hc := hc xor Int32(data[i]);
    System.Dec(i);
  end;
  Result := hc;
end;

class function TArrayUtils.GetArrayHashCode(const data: TCryptoLibUInt32Array;
  off, len: Int32): Int32;
var
  i, hc: Int32;
begin
  if data = Nil then
  begin
    Result := 0;
    Exit;
  end;

  i := len;
  hc := i + 1;

  System.Dec(i);
  while (i >= 0) do
  begin
    hc := hc * 257;
    hc := hc xor Int32(data[off + i]);
    System.Dec(i);
  end;
  Result := hc;
end;

class function TArrayUtils.GetArrayHashCode(const data: TCryptoLibUInt64Array;
  off, len: Int32): Int32;
var
  i, hc: Int32;
  di: UInt64;
begin
  if data = Nil then
  begin
    Result := 0;
    Exit;
  end;

  i := len;
  hc := i + 1;

  System.Dec(i);
  while (i >= 0) do
  begin
    di := data[off + i];
    hc := hc * 257;
    hc := hc xor Int32(di);
    hc := hc * 257;
    hc := hc xor Int32(di shr 32);
    System.Dec(i);
  end;
  Result := hc;
end;

class function TArrayUtils.Prepend(const A: TCryptoLibByteArray; B: Byte)
  : TCryptoLibByteArray;
var
  &length: Int32;
begin
  if (A = Nil) then
  begin
    Result := TCryptoLibByteArray.Create(B);
    Exit;
  end;

  Length := System.Length(A);
  System.SetLength(Result, Length + 1);
  System.Move(A[0], Result[1], Length * System.SizeOf(Byte));
  Result[0] := B;
end;

end.
