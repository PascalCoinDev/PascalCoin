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

unit ClpNat256;

{$I CryptoLib.inc}

interface

uses
  ClpNat,
  ClpConverters,
  ClpBits,
  ClpBigInteger,
  ClpArrayUtils,
  ClpCryptoLibTypes;

type
  TNat256 = class abstract(TObject)

  strict private
  const
    M = UInt64($FFFFFFFF);

  public
    class function Add(const x, y, z: TCryptoLibUInt32Array): UInt32;
      overload; static;
    class function Add(const x: TCryptoLibUInt32Array; xOff: Int32;
      const y: TCryptoLibUInt32Array; yOff: Int32;
      const z: TCryptoLibUInt32Array; zOff: Int32): UInt32; overload; static;

    class function AddBothTo(const x, y, z: TCryptoLibUInt32Array): UInt32;
      overload; static;
    class function AddBothTo(const x: TCryptoLibUInt32Array; xOff: Int32;
      const y: TCryptoLibUInt32Array; yOff: Int32;
      const z: TCryptoLibUInt32Array; zOff: Int32): UInt32; overload; static;

    class function AddTo(const x, z: TCryptoLibUInt32Array): UInt32;
      overload; static;
    class function AddTo(const x: TCryptoLibUInt32Array; xOff: Int32;
      const z: TCryptoLibUInt32Array; zOff: Int32; cIn: UInt32): UInt32;
      overload; static;

    class function AddToEachOther(const u: TCryptoLibUInt32Array; uOff: Int32;
      const v: TCryptoLibUInt32Array; vOff: Int32): UInt32; static;

    class procedure Copy(const x, z: TCryptoLibUInt32Array); overload;
      static; inline;
    class procedure Copy(const x: TCryptoLibUInt32Array; xOff: Int32;
      const z: TCryptoLibUInt32Array; zOff: Int32); overload; static; inline;

    class procedure Copy64(const x, z: TCryptoLibUInt64Array); overload;
      static; inline;

    class procedure Copy64(const x: TCryptoLibUInt64Array; xOff: Int32;
      const z: TCryptoLibUInt64Array; zOff: Int32); overload; static; inline;

    class function Create(): TCryptoLibUInt32Array; static; inline;

    class function Create64(): TCryptoLibUInt64Array; static; inline;

    class function CreateExt(): TCryptoLibUInt32Array; static; inline;

    class function CreateExt64(): TCryptoLibUInt64Array; static; inline;

    class function Diff(const x: TCryptoLibUInt32Array; xOff: Int32;
      const y: TCryptoLibUInt32Array; yOff: Int32;
      const z: TCryptoLibUInt32Array; zOff: Int32): Boolean; static; inline;

    class function Eq(const x, y: TCryptoLibUInt32Array): Boolean; static;

    class function Eq64(const x, y: TCryptoLibUInt64Array): Boolean; static;

    class function FromBigInteger(const x: TBigInteger)
      : TCryptoLibUInt32Array; static;

    class function FromBigInteger64(const x: TBigInteger)
      : TCryptoLibUInt64Array; static;

    class function GetBit(const x: TCryptoLibUInt32Array; bit: Int32): UInt32;
      static; inline;

    class function Gte(const x, y: TCryptoLibUInt32Array): Boolean;
      overload; static;

    class function Gte(const x: TCryptoLibUInt32Array; xOff: Int32;
      const y: TCryptoLibUInt32Array; yOff: Int32): Boolean; overload; static;

    class function IsOne(const x: TCryptoLibUInt32Array): Boolean; static;

    class function IsOne64(const x: TCryptoLibUInt64Array): Boolean; static;

    class function IsZero(const x: TCryptoLibUInt32Array): Boolean; static;

    class function IsZero64(const x: TCryptoLibUInt64Array): Boolean; static;

    class procedure Mul(const x, y, zz: TCryptoLibUInt32Array);
      overload; static;

    class procedure Mul(const x: TCryptoLibUInt32Array; xOff: Int32;
      const y: TCryptoLibUInt32Array; yOff: Int32;
      const zz: TCryptoLibUInt32Array; zzOff: Int32); overload; static;

    class function MulAddTo(const x, y, zz: TCryptoLibUInt32Array): UInt32;
      overload; static;

    class function MulAddTo(const x: TCryptoLibUInt32Array; xOff: Int32;
      const y: TCryptoLibUInt32Array; yOff: Int32;
      const zz: TCryptoLibUInt32Array; zzOff: Int32): UInt32; overload; static;

    class function Mul33Add(w: UInt32; const x: TCryptoLibUInt32Array;
      xOff: Int32; const y: TCryptoLibUInt32Array; yOff: Int32;
      const z: TCryptoLibUInt32Array; zOff: Int32): UInt64; static;

    class function MulByWord(x: UInt32; const z: TCryptoLibUInt32Array)
      : UInt32; static;

    class function MulByWordAddTo(x: UInt32; const y, z: TCryptoLibUInt32Array)
      : UInt32; static;

    class function MulWordAddTo(x: UInt32; const y: TCryptoLibUInt32Array;
      yOff: Int32; const z: TCryptoLibUInt32Array; zOff: Int32): UInt32; static;

    class function Mul33DWordAdd(x: UInt32; y: UInt64;
      const z: TCryptoLibUInt32Array; zOff: Int32): UInt32; static;

    class function Mul33WordAdd(x, y: UInt32; const z: TCryptoLibUInt32Array;
      zOff: Int32): UInt32; static;

    class function MulWordDwordAdd(x: UInt32; y: UInt64;
      const z: TCryptoLibUInt32Array; zOff: Int32): UInt32; static;

    class function MulWord(x: UInt32; const y, z: TCryptoLibUInt32Array;
      zOff: Int32): UInt32; static;

    class procedure Square(const x, zz: TCryptoLibUInt32Array);
      overload; static;

    class procedure Square(const x: TCryptoLibUInt32Array; xOff: Int32;
      const zz: TCryptoLibUInt32Array; zzOff: Int32); overload; static;

    class function Sub(const x, y, z: TCryptoLibUInt32Array): Int32;
      overload; static;

    class function Sub(const x: TCryptoLibUInt32Array; xOff: Int32;
      const y: TCryptoLibUInt32Array; yOff: Int32;
      const z: TCryptoLibUInt32Array; zOff: Int32): Int32; overload; static;

    class function SubBothFrom(const x, y, z: TCryptoLibUInt32Array)
      : Int32; static;

    class function SubFrom(const x, z: TCryptoLibUInt32Array): Int32;
      overload; static;

    class function SubFrom(const x: TCryptoLibUInt32Array; xOff: Int32;
      const z: TCryptoLibUInt32Array; zOff: Int32): Int32; overload; static;

    class function ToBigInteger(const x: TCryptoLibUInt32Array)
      : TBigInteger; static;

    class function ToBigInteger64(const x: TCryptoLibUInt64Array)
      : TBigInteger; static;

    class procedure Zero(const z: TCryptoLibUInt32Array); static; inline;

  end;

implementation

{ TNat256 }

class function TNat256.Add(const x, y, z: TCryptoLibUInt32Array): UInt32;
var
  c: UInt64;
begin
  c := 0;
  c := c + (UInt64(x[0]) + y[0]);
  z[0] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[1]) + y[1]);
  z[1] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[2]) + y[2]);
  z[2] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[3]) + y[3]);
  z[3] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[4]) + y[4]);
  z[4] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[5]) + y[5]);
  z[5] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[6]) + y[6]);
  z[6] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[7]) + y[7]);
  z[7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TNat256.Add(const x: TCryptoLibUInt32Array; xOff: Int32;
  const y: TCryptoLibUInt32Array; yOff: Int32; const z: TCryptoLibUInt32Array;
  zOff: Int32): UInt32;
var
  c: UInt64;
begin
  c := 0;
  c := c + (UInt64(x[xOff + 0]) + y[yOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 1]) + y[yOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 2]) + y[yOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 3]) + y[yOff + 3]);
  z[zOff + 3] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 4]) + y[yOff + 4]);
  z[zOff + 4] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 5]) + y[yOff + 5]);
  z[zOff + 5] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 6]) + y[yOff + 6]);
  z[zOff + 6] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 7]) + y[yOff + 7]);
  z[zOff + 7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TNat256.AddBothTo(const x, y, z: TCryptoLibUInt32Array): UInt32;
var
  c: UInt64;
begin
  c := 0;
  c := c + (UInt64(x[0]) + y[0] + z[0]);
  z[0] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[1]) + y[1] + z[1]);
  z[1] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[2]) + y[2] + z[2]);
  z[2] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[3]) + y[3] + z[3]);
  z[3] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[4]) + y[4] + z[4]);
  z[4] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[5]) + y[5] + z[5]);
  z[5] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[6]) + y[6] + z[6]);
  z[6] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[7]) + y[7] + z[7]);
  z[7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TNat256.AddBothTo(const x: TCryptoLibUInt32Array; xOff: Int32;
  const y: TCryptoLibUInt32Array; yOff: Int32; const z: TCryptoLibUInt32Array;
  zOff: Int32): UInt32;
var
  c: UInt64;
begin
  c := 0;
  c := c + (UInt64(x[xOff + 0]) + y[yOff + 0] + z[zOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 1]) + y[yOff + 1] + z[zOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 2]) + y[yOff + 2] + z[zOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 3]) + y[yOff + 3] + z[zOff + 3]);
  z[zOff + 3] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 4]) + y[yOff + 4] + z[zOff + 4]);
  z[zOff + 4] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 5]) + y[yOff + 5] + z[zOff + 5]);
  z[zOff + 5] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 6]) + y[yOff + 6] + z[zOff + 6]);
  z[zOff + 6] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 7]) + y[yOff + 7] + z[zOff + 7]);
  z[zOff + 7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TNat256.AddTo(const x, z: TCryptoLibUInt32Array): UInt32;
var
  c: UInt64;
begin
  c := 0;
  c := c + (UInt64(x[0]) + z[0]);
  z[0] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[1]) + z[1]);
  z[1] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[2]) + z[2]);
  z[2] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[3]) + z[3]);
  z[3] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[4]) + z[4]);
  z[4] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[5]) + z[5]);
  z[5] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[6]) + z[6]);
  z[6] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[7]) + z[7]);
  z[7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TNat256.AddTo(const x: TCryptoLibUInt32Array; xOff: Int32;
  const z: TCryptoLibUInt32Array; zOff: Int32; cIn: UInt32): UInt32;
var
  c: UInt64;
begin
  c := cIn;
  c := c + (UInt64(x[xOff + 0]) + z[zOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 1]) + z[zOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 2]) + z[zOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 3]) + z[zOff + 3]);
  z[zOff + 3] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 4]) + z[zOff + 4]);
  z[zOff + 4] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 5]) + z[zOff + 5]);
  z[zOff + 5] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 6]) + z[zOff + 6]);
  z[zOff + 6] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(x[xOff + 7]) + z[zOff + 7]);
  z[zOff + 7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TNat256.AddToEachOther(const u: TCryptoLibUInt32Array;
  uOff: Int32; const v: TCryptoLibUInt32Array; vOff: Int32): UInt32;
var
  c: UInt64;
begin
  c := 0;
  c := c + (UInt64(u[uOff + 0]) + v[vOff + 0]);
  u[uOff + 0] := UInt32(c);
  v[vOff + 0] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(u[uOff + 1]) + v[vOff + 1]);
  u[uOff + 1] := UInt32(c);
  v[vOff + 1] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(u[uOff + 2]) + v[vOff + 2]);
  u[uOff + 2] := UInt32(c);
  v[vOff + 2] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(u[uOff + 3]) + v[vOff + 3]);
  u[uOff + 3] := UInt32(c);
  v[vOff + 3] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(u[uOff + 4]) + v[vOff + 4]);
  u[uOff + 4] := UInt32(c);
  v[vOff + 4] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(u[uOff + 5]) + v[vOff + 5]);
  u[uOff + 5] := UInt32(c);
  v[vOff + 5] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(u[uOff + 6]) + v[vOff + 6]);
  u[uOff + 6] := UInt32(c);
  v[vOff + 6] := UInt32(c);
  c := c shr 32;
  c := c + (UInt64(u[uOff + 7]) + v[vOff + 7]);
  u[uOff + 7] := UInt32(c);
  v[vOff + 7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class procedure TNat256.Copy(const x, z: TCryptoLibUInt32Array);
begin
  System.Move(x[0], z[0], 8 * System.SizeOf(UInt32));
end;

class procedure TNat256.Copy(const x: TCryptoLibUInt32Array; xOff: Int32;
  const z: TCryptoLibUInt32Array; zOff: Int32);
begin
  System.Move(x[xOff], z[zOff], 8 * System.SizeOf(UInt32));
end;

class procedure TNat256.Copy64(const x, z: TCryptoLibUInt64Array);
begin
  System.Move(x[0], z[0], 4 * System.SizeOf(UInt64));
end;

class procedure TNat256.Copy64(const x: TCryptoLibUInt64Array; xOff: Int32;
  const z: TCryptoLibUInt64Array; zOff: Int32);
begin
  System.Move(x[xOff], z[zOff], 4 * System.SizeOf(UInt64));
end;

class function TNat256.Create: TCryptoLibUInt32Array;
begin
  System.SetLength(result, 8);
end;

class function TNat256.Create64: TCryptoLibUInt64Array;
begin
  System.SetLength(result, 4);
end;

class function TNat256.CreateExt: TCryptoLibUInt32Array;
begin
  System.SetLength(result, 16);
end;

class function TNat256.CreateExt64: TCryptoLibUInt64Array;
begin
  System.SetLength(result, 8);
end;

class function TNat256.Diff(const x: TCryptoLibUInt32Array; xOff: Int32;
  const y: TCryptoLibUInt32Array; yOff: Int32; const z: TCryptoLibUInt32Array;
  zOff: Int32): Boolean;
var
  pos: Boolean;
begin
  pos := Gte(x, xOff, y, yOff);
  if (pos) then
  begin
    Sub(x, xOff, y, yOff, z, zOff);
  end
  else
  begin
    Sub(y, yOff, x, xOff, z, zOff);
  end;
  result := pos;
end;

class function TNat256.Eq(const x, y: TCryptoLibUInt32Array): Boolean;
var
  i: Int32;
begin
  i := 7;
  while i >= 0 do
  begin
    if (x[i] <> y[i]) then
    begin
      result := false;
      Exit;
    end;
    System.Dec(i);
  end;
  result := true;
end;

class function TNat256.Eq64(const x, y: TCryptoLibUInt64Array): Boolean;
var
  i: Int32;
begin
  i := 3;
  while i >= 0 do
  begin
    if (x[i] <> y[i]) then
    begin
      result := false;
      Exit;
    end;
    System.Dec(i);
  end;
  result := true;
end;

class function TNat256.FromBigInteger(const x: TBigInteger)
  : TCryptoLibUInt32Array;
var
  i: Int32;
  Lx: TBigInteger;
begin
  Lx := x;
  if ((Lx.SignValue < 0) or (Lx.BitLength > 256)) then
  begin
    raise EArgumentCryptoLibException.Create('');
  end;

  result := Create();
  i := 0;
  while (Lx.SignValue <> 0) do
  begin
    result[i] := UInt32(Lx.Int32Value);
    System.Inc(i);
    Lx := Lx.ShiftRight(32);
  end;
end;

class function TNat256.FromBigInteger64(const x: TBigInteger)
  : TCryptoLibUInt64Array;
var
  i: Int32;
  Lx: TBigInteger;
begin
  Lx := x;
  if ((Lx.SignValue < 0) or (Lx.BitLength > 256)) then
  begin
    raise EArgumentCryptoLibException.Create('');
  end;

  result := Create64();
  i := 0;
  while (Lx.SignValue <> 0) do
  begin
    result[i] := UInt64(Lx.Int64Value);
    System.Inc(i);
    Lx := Lx.ShiftRight(64);
  end;
end;

class function TNat256.GetBit(const x: TCryptoLibUInt32Array;
  bit: Int32): UInt32;
var
  w, b: Int32;
begin
  if (bit = 0) then
  begin
    result := x[0] and 1;
    Exit;
  end;
  if ((bit and 255) <> bit) then
  begin
    result := 0;
    Exit;
  end;
  w := TBits.Asr32(bit, 5);
  b := bit and 31;
  result := (x[w] shr b) and 1;
end;

class function TNat256.Gte(const x, y: TCryptoLibUInt32Array): Boolean;
var
  i: Int32;
  x_i, y_i: UInt32;
begin
  i := 7;
  while i >= 0 do
  begin
    x_i := x[i];
    y_i := y[i];

    if (x_i < y_i) then
    begin
      result := false;
      Exit;
    end;

    if (x_i > y_i) then
    begin
      result := true;
      Exit;
    end;
    System.Dec(i);
  end;
  result := true;
end;

class function TNat256.Gte(const x: TCryptoLibUInt32Array; xOff: Int32;
  const y: TCryptoLibUInt32Array; yOff: Int32): Boolean;
var
  i: Int32;
  x_i, y_i: UInt32;
begin
  i := 7;
  while i >= 0 do
  begin
    x_i := x[xOff + i];
    y_i := y[yOff + i];

    if (x_i < y_i) then
    begin
      result := false;
      Exit;
    end;

    if (x_i > y_i) then
    begin
      result := true;
      Exit;
    end;
    System.Dec(i);
  end;
  result := true;
end;

class function TNat256.IsOne(const x: TCryptoLibUInt32Array): Boolean;
var
  i: Int32;
begin
  if (x[0] <> 1) then
  begin
    result := false;
    Exit;
  end;

  i := 1;
  while i < 8 do
  begin
    if (x[i] <> 0) then
    begin
      result := false;
      Exit;
    end;
    System.Inc(i);
  end;
  result := true;
end;

class function TNat256.IsOne64(const x: TCryptoLibUInt64Array): Boolean;
var
  i: Int32;
begin
  if (x[0] <> UInt64(1)) then
  begin
    result := false;
    Exit;
  end;

  i := 1;
  while i < 4 do
  begin
    if (x[i] <> UInt64(0)) then
    begin
      result := false;
      Exit;
    end;
    System.Inc(i);
  end;
  result := true;
end;

class function TNat256.IsZero(const x: TCryptoLibUInt32Array): Boolean;
var
  i: Int32;
begin
  i := 0;
  while i < 8 do
  begin
    if (x[i] <> 0) then
    begin
      result := false;
      Exit;
    end;
    System.Inc(i);
  end;
  result := true;
end;

class function TNat256.IsZero64(const x: TCryptoLibUInt64Array): Boolean;
var
  i: Int32;
begin
  i := 0;
  while i < 4 do
  begin
    if (x[i] <> UInt64(0)) then
    begin
      result := false;
      Exit;
    end;
    System.Inc(i);
  end;
  result := true;
end;

class procedure TNat256.Mul(const x, y, zz: TCryptoLibUInt32Array);
var
  c, x_0, x_i, y_0, y_1, y_2, y_3, y_4, y_5, y_6, y_7: UInt64;
  i: Int32;
begin
  y_0 := y[0];
  y_1 := y[1];
  y_2 := y[2];
  y_3 := y[3];
  y_4 := y[4];
  y_5 := y[5];
  y_6 := y[6];
  y_7 := y[7];

  c := 0;
  x_0 := x[0];
  c := c + (x_0 * y_0);
  zz[0] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_1);
  zz[1] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_2);
  zz[2] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_3);
  zz[3] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_4);
  zz[4] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_5);
  zz[5] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_6);
  zz[6] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_7);
  zz[7] := UInt32(c);
  c := c shr 32;
  zz[8] := UInt32(c);

  for i := 1 to System.Pred(8) do
  begin
    c := 0;
    x_i := x[i];
    c := c + (x_i * y_0 + zz[i + 0]);
    zz[i + 0] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_1 + zz[i + 1]);
    zz[i + 1] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_2 + zz[i + 2]);
    zz[i + 2] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_3 + zz[i + 3]);
    zz[i + 3] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_4 + zz[i + 4]);
    zz[i + 4] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_5 + zz[i + 5]);
    zz[i + 5] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_6 + zz[i + 6]);
    zz[i + 6] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_7 + zz[i + 7]);
    zz[i + 7] := UInt32(c);
    c := c shr 32;
    zz[i + 8] := UInt32(c);
  end;
end;

class procedure TNat256.Mul(const x: TCryptoLibUInt32Array; xOff: Int32;
  const y: TCryptoLibUInt32Array; yOff: Int32; const zz: TCryptoLibUInt32Array;
  zzOff: Int32);
var
  c, x_0, x_i, y_0, y_1, y_2, y_3, y_4, y_5, y_6, y_7: UInt64;
  i: Int32;
begin
  y_0 := y[yOff + 0];
  y_1 := y[yOff + 1];
  y_2 := y[yOff + 2];
  y_3 := y[yOff + 3];
  y_4 := y[yOff + 4];
  y_5 := y[yOff + 5];
  y_6 := y[yOff + 6];
  y_7 := y[yOff + 7];

  c := 0;
  x_0 := x[xOff + 0];
  c := c + (x_0 * y_0);
  zz[zzOff + 0] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_1);
  zz[zzOff + 1] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_2);
  zz[zzOff + 2] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_3);
  zz[zzOff + 3] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_4);
  zz[zzOff + 4] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_5);
  zz[zzOff + 5] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_6);
  zz[zzOff + 6] := UInt32(c);
  c := c shr 32;
  c := c + (x_0 * y_7);
  zz[zzOff + 7] := UInt32(c);
  c := c shr 32;
  zz[zzOff + 8] := UInt32(c);

  for i := 1 to System.Pred(8) do
  begin
    System.Inc(zzOff);
    c := 0;
    x_i := x[xOff + i];
    c := c + (x_i * y_0 + zz[zzOff + 0]);
    zz[zzOff + 0] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_1 + zz[zzOff + 1]);
    zz[zzOff + 1] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_2 + zz[zzOff + 2]);
    zz[zzOff + 2] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_3 + zz[zzOff + 3]);
    zz[zzOff + 3] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_4 + zz[zzOff + 4]);
    zz[zzOff + 4] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_5 + zz[zzOff + 5]);
    zz[zzOff + 5] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_6 + zz[zzOff + 6]);
    zz[zzOff + 6] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_7 + zz[zzOff + 7]);
    zz[zzOff + 7] := UInt32(c);
    c := c shr 32;
    zz[zzOff + 8] := UInt32(c);
  end;
end;

class function TNat256.MulAddTo(const x, y, zz: TCryptoLibUInt32Array): UInt32;
var
  c, x_i, y_0, y_1, y_2, y_3, y_4, y_5, y_6, y_7, zc: UInt64;
  i: Int32;
begin
  y_0 := y[0];
  y_1 := y[1];
  y_2 := y[2];
  y_3 := y[3];
  y_4 := y[4];
  y_5 := y[5];
  y_6 := y[6];
  y_7 := y[7];

  zc := 0;
  for i := 0 to System.Pred(8) do
  begin
    c := 0;
    x_i := x[i];
    c := c + (x_i * y_0 + zz[i + 0]);
    zz[i + 0] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_1 + zz[i + 1]);
    zz[i + 1] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_2 + zz[i + 2]);
    zz[i + 2] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_3 + zz[i + 3]);
    zz[i + 3] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_4 + zz[i + 4]);
    zz[i + 4] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_5 + zz[i + 5]);
    zz[i + 5] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_6 + zz[i + 6]);
    zz[i + 6] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_7 + zz[i + 7]);
    zz[i + 7] := UInt32(c);
    c := c shr 32;
    c := c + (zc + zz[i + 8]);
    zz[i + 8] := UInt32(c);
    zc := c shr 32;
  end;
  result := UInt32(zc);
end;

class function TNat256.MulAddTo(const x: TCryptoLibUInt32Array; xOff: Int32;
  const y: TCryptoLibUInt32Array; yOff: Int32; const zz: TCryptoLibUInt32Array;
  zzOff: Int32): UInt32;
var
  c, x_i, y_0, y_1, y_2, y_3, y_4, y_5, y_6, y_7, zc: UInt64;
  i: Int32;
begin
  y_0 := y[yOff + 0];
  y_1 := y[yOff + 1];
  y_2 := y[yOff + 2];
  y_3 := y[yOff + 3];
  y_4 := y[yOff + 4];
  y_5 := y[yOff + 5];
  y_6 := y[yOff + 6];
  y_7 := y[yOff + 7];

  zc := 0;
  for i := 0 to System.Pred(8) do
  begin
    c := 0;
    x_i := x[xOff + i];
    c := c + (x_i * y_0 + zz[zzOff + 0]);
    zz[zzOff + 0] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_1 + zz[zzOff + 1]);
    zz[zzOff + 1] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_2 + zz[zzOff + 2]);
    zz[zzOff + 2] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_3 + zz[zzOff + 3]);
    zz[zzOff + 3] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_4 + zz[zzOff + 4]);
    zz[zzOff + 4] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_5 + zz[zzOff + 5]);
    zz[zzOff + 5] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_6 + zz[zzOff + 6]);
    zz[zzOff + 6] := UInt32(c);
    c := c shr 32;
    c := c + (x_i * y_7 + zz[zzOff + 7]);
    zz[zzOff + 7] := UInt32(c);
    c := c shr 32;
    c := c + (zc + zz[zzOff + 8]);
    zz[zzOff + 8] := UInt32(c);
    zc := c shr 32;
    System.Inc(zzOff);
  end;
  result := UInt32(zc);
end;

class function TNat256.Mul33Add(w: UInt32; const x: TCryptoLibUInt32Array;
  xOff: Int32; const y: TCryptoLibUInt32Array; yOff: Int32;
  const z: TCryptoLibUInt32Array; zOff: Int32): UInt64;
var
  c, wVal, x0, x1, x2, x3, x4, x5, x6, x7: UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(w shr 31 = 0);
{$ENDIF DEBUG}
  c := 0;
  wVal := w;
  x0 := x[xOff + 0];
  c := c + (wVal * x0 + y[yOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := c shr 32;
  x1 := x[xOff + 1];
  c := c + (wVal * x1 + x0 + y[yOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := c shr 32;
  x2 := x[xOff + 2];
  c := c + (wVal * x2 + x1 + y[yOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := c shr 32;
  x3 := x[xOff + 3];
  c := c + (wVal * x3 + x2 + y[yOff + 3]);
  z[zOff + 3] := UInt32(c);
  c := c shr 32;
  x4 := x[xOff + 4];
  c := c + (wVal * x4 + x3 + y[yOff + 4]);
  z[zOff + 4] := UInt32(c);
  c := c shr 32;
  x5 := x[xOff + 5];
  c := c + (wVal * x5 + x4 + y[yOff + 5]);
  z[zOff + 5] := UInt32(c);
  c := c shr 32;
  x6 := x[xOff + 6];
  c := c + (wVal * x6 + x5 + y[yOff + 6]);
  z[zOff + 6] := UInt32(c);
  c := c shr 32;
  x7 := x[xOff + 7];
  c := c + (wVal * x7 + x6 + y[yOff + 7]);
  z[zOff + 7] := UInt32(c);
  c := c shr 32;
  c := c + x7;
  result := c;
end;

class function TNat256.MulByWord(x: UInt32;
  const z: TCryptoLibUInt32Array): UInt32;
var
  c, xVal: UInt64;
begin
  c := 0;
  xVal := x;
  c := c + (xVal * UInt64(z[0]));
  z[0] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[1]));
  z[1] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[2]));
  z[2] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[3]));
  z[3] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[4]));
  z[4] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[5]));
  z[5] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[6]));
  z[6] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[7]));
  z[7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TNat256.MulByWordAddTo(x: UInt32;
  const y, z: TCryptoLibUInt32Array): UInt32;
var
  c, xVal: UInt64;
begin
  c := 0;
  xVal := x;
  c := c + (xVal * UInt64(z[0]) + y[0]);
  z[0] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[1]) + y[1]);
  z[1] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[2]) + y[2]);
  z[2] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[3]) + y[3]);
  z[3] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[4]) + y[4]);
  z[4] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[5]) + y[5]);
  z[5] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[6]) + y[6]);
  z[6] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * UInt64(z[7]) + y[7]);
  z[7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TNat256.MulWordAddTo(x: UInt32; const y: TCryptoLibUInt32Array;
  yOff: Int32; const z: TCryptoLibUInt32Array; zOff: Int32): UInt32;
var
  c, xVal: UInt64;
begin
  c := 0;
  xVal := x;
  c := c + (xVal * y[yOff + 0] + z[zOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * y[yOff + 1] + z[zOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * y[yOff + 2] + z[zOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * y[yOff + 3] + z[zOff + 3]);
  z[zOff + 3] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * y[yOff + 4] + z[zOff + 4]);
  z[zOff + 4] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * y[yOff + 5] + z[zOff + 5]);
  z[zOff + 5] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * y[yOff + 6] + z[zOff + 6]);
  z[zOff + 6] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * y[yOff + 7] + z[zOff + 7]);
  z[zOff + 7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TNat256.Mul33DWordAdd(x: UInt32; y: UInt64;
  const z: TCryptoLibUInt32Array; zOff: Int32): UInt32;
var
  c, xVal, y00, y01: UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(x shr 31 = 0);
  System.Assert(zOff <= 4);
{$ENDIF DEBUG}
  c := 0;
  xVal := x;
  y00 := y and M;
  c := c + (xVal * y00 + z[zOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := c shr 32;
  y01 := y shr 32;
  c := c + (xVal * y01 + y00 + z[zOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := c shr 32;
  c := c + (y01 + z[zOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := c shr 32;
  c := c + (z[zOff + 3]);
  z[zOff + 3] := UInt32(c);
  c := c shr 32;
  if c = 0 then
  begin
    result := 0;
  end
  else
  begin
    result := TNat.IncAt(8, z, zOff, 4);
  end;
end;

class function TNat256.Mul33WordAdd(x, y: UInt32;
  const z: TCryptoLibUInt32Array; zOff: Int32): UInt32;
var
  c, yVal: UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(x shr 31 = 0);
  System.Assert(zOff <= 5);
{$ENDIF DEBUG}
  c := 0;
  yVal := y;
  c := c + (yVal * x + z[zOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := c shr 32;
  c := c + (yVal + z[zOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := c shr 32;
  c := c + (z[zOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := c shr 32;
  if c = 0 then
  begin
    result := 0;
  end
  else
  begin
    result := TNat.IncAt(8, z, zOff, 3);
  end;
end;

class function TNat256.MulWordDwordAdd(x: UInt32; y: UInt64;
  const z: TCryptoLibUInt32Array; zOff: Int32): UInt32;
var
  c, xVal: UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(zOff <= 5);
{$ENDIF DEBUG}
  c := 0;
  xVal := x;
  c := c + (xVal * y + z[zOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := c shr 32;
  c := c + (xVal * (y shr 32) + z[zOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := c shr 32;
  c := c + (z[zOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := c shr 32;
  if c = 0 then
  begin
    result := 0;
  end
  else
  begin
    result := TNat.IncAt(8, z, zOff, 3);
  end;
end;

class function TNat256.MulWord(x: UInt32; const y, z: TCryptoLibUInt32Array;
  zOff: Int32): UInt32;
var
  c, xVal: UInt64;
  i: Int32;
begin
  c := 0;
  xVal := x;
  i := 0;
  repeat
    c := c + (xVal * y[i]);
    z[zOff + i] := UInt32(c);
    c := c shr 32;
    System.Inc(i);
  until not(i < 8);
  result := UInt32(c);
end;

class procedure TNat256.Square(const x, zz: TCryptoLibUInt32Array);
var
  x_0, zz_1, xVal, p, x_1, zz_2, x_2, zz_3, zz_4, x_3, zz_5, zz_6, x_4, zz_7,
    zz_8, x_5, zz_9, zz_10, x_6, zz_11, zz_12, x_7, zz_13, zz_14: UInt64;
  c, w: UInt32;
  i, j: Int32;
begin
  x_0 := x[0];
  c := 0;
  i := 7;
  j := 16;

  repeat
    xVal := x[i];
    System.Dec(i);
    p := xVal * xVal;
    System.Dec(j);
    zz[j] := (c shl 31) or UInt32(p shr 33);
    System.Dec(j);
    zz[j] := UInt32(p shr 1);
    c := UInt32(p);
  until not(i > 0);

  p := x_0 * x_0;
  zz_1 := UInt64(c shl 31) or (p shr 33);
  zz[0] := UInt32(p);
  c := UInt32(p shr 32) and 1;

  x_1 := x[1];
  zz_2 := zz[2];

  zz_1 := zz_1 + (x_1 * x_0);
  w := UInt32(zz_1);
  zz[1] := (w shl 1) or c;
  c := w shr 31;
  zz_2 := zz_2 + (zz_1 shr 32);

  x_2 := x[2];
  zz_3 := zz[3];
  zz_4 := zz[4];

  zz_2 := zz_2 + (x_2 * x_0);
  w := UInt32(zz_2);
  zz[2] := (w shl 1) or c;
  c := w shr 31;
  zz_3 := zz_3 + ((zz_2 shr 32) + x_2 * x_1);
  zz_4 := zz_4 + (zz_3 shr 32);
  zz_3 := zz_3 and M;

  x_3 := x[3];
  zz_5 := zz[5] + (zz_4 shr 32);
  zz_4 := zz_4 and M;
  zz_6 := zz[6] + (zz_5 shr 32);
  zz_5 := zz_5 and M;

  zz_3 := zz_3 + (x_3 * x_0);
  w := UInt32(zz_3);
  zz[3] := (w shl 1) or c;
  c := w shr 31;
  zz_4 := zz_4 + ((zz_3 shr 32) + x_3 * x_1);
  zz_5 := zz_5 + ((zz_4 shr 32) + x_3 * x_2);
  zz_4 := zz_4 and M;
  zz_6 := zz_6 + (zz_5 shr 32);
  zz_5 := zz_5 and M;

  x_4 := x[4];
  zz_7 := zz[7] + (zz_6 shr 32);
  zz_6 := zz_6 and M;
  zz_8 := zz[8] + (zz_7 shr 32);
  zz_7 := zz_7 and M;

  zz_4 := zz_4 + (x_4 * x_0);
  w := UInt32(zz_4);
  zz[4] := (w shl 1) or c;
  c := w shr 31;
  zz_5 := zz_5 + ((zz_4 shr 32) + x_4 * x_1);
  zz_6 := zz_6 + ((zz_5 shr 32) + x_4 * x_2);
  zz_5 := zz_5 and M;
  zz_7 := zz_7 + ((zz_6 shr 32) + x_4 * x_3);
  zz_6 := zz_6 and M;
  zz_8 := zz_8 + (zz_7 shr 32);
  zz_7 := zz_7 and M;

  x_5 := x[5];
  zz_9 := zz[9] + (zz_8 shr 32);
  zz_8 := zz_8 and M;
  zz_10 := zz[10] + (zz_9 shr 32);
  zz_9 := zz_9 and M;

  zz_5 := zz_5 + (x_5 * x_0);
  w := UInt32(zz_5);
  zz[5] := (w shl 1) or c;
  c := w shr 31;
  zz_6 := zz_6 + ((zz_5 shr 32) + x_5 * x_1);
  zz_7 := zz_7 + ((zz_6 shr 32) + x_5 * x_2);
  zz_6 := zz_6 and M;
  zz_8 := zz_8 + ((zz_7 shr 32) + x_5 * x_3);
  zz_7 := zz_7 and M;
  zz_9 := zz_9 + ((zz_8 shr 32) + x_5 * x_4);
  zz_8 := zz_8 and M;
  zz_10 := zz_10 + (zz_9 shr 32);
  zz_9 := zz_9 and M;

  x_6 := x[6];
  zz_11 := zz[11] + (zz_10 shr 32);
  zz_10 := zz_10 and M;
  zz_12 := zz[12] + (zz_11 shr 32);
  zz_11 := zz_11 and M;

  zz_6 := zz_6 + (x_6 * x_0);
  w := UInt32(zz_6);
  zz[6] := (w shl 1) or c;
  c := w shr 31;
  zz_7 := zz_7 + ((zz_6 shr 32) + x_6 * x_1);
  zz_8 := zz_8 + ((zz_7 shr 32) + x_6 * x_2);
  zz_7 := zz_7 and M;
  zz_9 := zz_9 + ((zz_8 shr 32) + x_6 * x_3);
  zz_8 := zz_8 and M;
  zz_10 := zz_10 + ((zz_9 shr 32) + x_6 * x_4);
  zz_9 := zz_9 and M;
  zz_11 := zz_11 + ((zz_10 shr 32) + x_6 * x_5);
  zz_10 := zz_10 and M;
  zz_12 := zz_12 + (zz_11 shr 32);
  zz_11 := zz_11 and M;

  x_7 := x[7];
  zz_13 := zz[13] + (zz_12 shr 32);
  zz_12 := zz_12 and M;
  zz_14 := zz[14] + (zz_13 shr 32);
  zz_13 := zz_13 and M;

  zz_7 := zz_7 + (x_7 * x_0);
  w := UInt32(zz_7);
  zz[7] := (w shl 1) or c;
  c := w shr 31;
  zz_8 := zz_8 + ((zz_7 shr 32) + x_7 * x_1);
  zz_9 := zz_9 + ((zz_8 shr 32) + x_7 * x_2);
  zz_10 := zz_10 + ((zz_9 shr 32) + x_7 * x_3);
  zz_11 := zz_11 + ((zz_10 shr 32) + x_7 * x_4);
  zz_12 := zz_12 + ((zz_11 shr 32) + x_7 * x_5);
  zz_13 := zz_13 + ((zz_12 shr 32) + x_7 * x_6);
  zz_14 := zz_14 + (zz_13 shr 32);

  w := UInt32(zz_8);
  zz[8] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_9);
  zz[9] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_10);
  zz[10] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_11);
  zz[11] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_12);
  zz[12] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_13);
  zz[13] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_14);
  zz[14] := (w shl 1) or c;
  c := w shr 31;
  w := zz[15] + UInt32(zz_14 shr 32);
  zz[15] := (w shl 1) or c;
end;

class procedure TNat256.Square(const x: TCryptoLibUInt32Array; xOff: Int32;
  const zz: TCryptoLibUInt32Array; zzOff: Int32);
var
  x_0, zz_1, xVal, p, x_1, zz_2, x_2, zz_3, zz_4, x_3, zz_5, zz_6, x_4, zz_7,
    zz_8, x_5, zz_9, zz_10, x_6, zz_11, zz_12, x_7, zz_13, zz_14: UInt64;
  c, w: UInt32;
  i, j: Int32;
begin
  x_0 := x[xOff + 0];
  c := 0;
  i := 7;
  j := 16;

  repeat
    xVal := x[xOff + i];
    System.Dec(i);
    p := xVal * xVal;
    System.Dec(j);
    zz[zzOff + j] := (c shl 31) or UInt32(p shr 33);
    System.Dec(j);
    zz[zzOff + j] := UInt32(p shr 1);
    c := UInt32(p);
  until not(i > 0);

  p := x_0 * x_0;
  zz_1 := UInt64(c shl 31) or (p shr 33);
  zz[zzOff + 0] := UInt32(p);
  c := UInt32(p shr 32) and 1;

  x_1 := x[xOff + 1];
  zz_2 := zz[zzOff + 2];

  zz_1 := zz_1 + (x_1 * x_0);
  w := UInt32(zz_1);
  zz[zzOff + 1] := (w shl 1) or c;
  c := w shr 31;
  zz_2 := zz_2 + (zz_1 shr 32);

  x_2 := x[xOff + 2];
  zz_3 := zz[zzOff + 3];
  zz_4 := zz[zzOff + 4];

  zz_2 := zz_2 + (x_2 * x_0);
  w := UInt32(zz_2);
  zz[zzOff + 2] := (w shl 1) or c;
  c := w shr 31;
  zz_3 := zz_3 + ((zz_2 shr 32) + x_2 * x_1);
  zz_4 := zz_4 + (zz_3 shr 32);
  zz_3 := zz_3 and M;

  x_3 := x[xOff + 3];
  zz_5 := zz[zzOff + 5] + (zz_4 shr 32);
  zz_4 := zz_4 and M;
  zz_6 := zz[zzOff + 6] + (zz_5 shr 32);
  zz_5 := zz_5 and M;

  zz_3 := zz_3 + (x_3 * x_0);
  w := UInt32(zz_3);
  zz[zzOff + 3] := (w shl 1) or c;
  c := w shr 31;
  zz_4 := zz_4 + ((zz_3 shr 32) + x_3 * x_1);
  zz_5 := zz_5 + ((zz_4 shr 32) + x_3 * x_2);
  zz_4 := zz_4 and M;
  zz_6 := zz_6 + (zz_5 shr 32);
  zz_5 := zz_5 and M;

  x_4 := x[xOff + 4];
  zz_7 := zz[zzOff + 7] + (zz_6 shr 32);
  zz_6 := zz_6 and M;
  zz_8 := zz[zzOff + 8] + (zz_7 shr 32);
  zz_7 := zz_7 and M;

  zz_4 := zz_4 + (x_4 * x_0);
  w := UInt32(zz_4);
  zz[zzOff + 4] := (w shl 1) or c;
  c := w shr 31;
  zz_5 := zz_5 + ((zz_4 shr 32) + x_4 * x_1);
  zz_6 := zz_6 + ((zz_5 shr 32) + x_4 * x_2);
  zz_5 := zz_5 and M;
  zz_7 := zz_7 + ((zz_6 shr 32) + x_4 * x_3);
  zz_6 := zz_6 and M;
  zz_8 := zz_8 + (zz_7 shr 32);
  zz_7 := zz_7 and M;

  x_5 := x[xOff + 5];
  zz_9 := zz[zzOff + 9] + (zz_8 shr 32);
  zz_8 := zz_8 and M;
  zz_10 := zz[zzOff + 10] + (zz_9 shr 32);
  zz_9 := zz_9 and M;

  zz_5 := zz_5 + (x_5 * x_0);
  w := UInt32(zz_5);
  zz[zzOff + 5] := (w shl 1) or c;
  c := w shr 31;
  zz_6 := zz_6 + ((zz_5 shr 32) + x_5 * x_1);
  zz_7 := zz_7 + ((zz_6 shr 32) + x_5 * x_2);
  zz_6 := zz_6 and M;
  zz_8 := zz_8 + ((zz_7 shr 32) + x_5 * x_3);
  zz_7 := zz_7 and M;
  zz_9 := zz_9 + ((zz_8 shr 32) + x_5 * x_4);
  zz_8 := zz_8 and M;
  zz_10 := zz_10 + (zz_9 shr 32);
  zz_9 := zz_9 and M;

  x_6 := x[xOff + 6];
  zz_11 := zz[zzOff + 11] + (zz_10 shr 32);
  zz_10 := zz_10 and M;
  zz_12 := zz[zzOff + 12] + (zz_11 shr 32);
  zz_11 := zz_11 and M;

  zz_6 := zz_6 + (x_6 * x_0);
  w := UInt32(zz_6);
  zz[zzOff + 6] := (w shl 1) or c;
  c := w shr 31;
  zz_7 := zz_7 + ((zz_6 shr 32) + x_6 * x_1);
  zz_8 := zz_8 + ((zz_7 shr 32) + x_6 * x_2);
  zz_7 := zz_7 and M;
  zz_9 := zz_9 + ((zz_8 shr 32) + x_6 * x_3);
  zz_8 := zz_8 and M;
  zz_10 := zz_10 + ((zz_9 shr 32) + x_6 * x_4);
  zz_9 := zz_9 and M;
  zz_11 := zz_11 + ((zz_10 shr 32) + x_6 * x_5);
  zz_10 := zz_10 and M;
  zz_12 := zz_12 + (zz_11 shr 32);
  zz_11 := zz_11 and M;

  x_7 := x[xOff + 7];
  zz_13 := zz[zzOff + 13] + (zz_12 shr 32);
  zz_12 := zz_12 and M;
  zz_14 := zz[zzOff + 14] + (zz_13 shr 32);
  zz_13 := zz_13 and M;

  zz_7 := zz_7 + (x_7 * x_0);
  w := UInt32(zz_7);
  zz[zzOff + 7] := (w shl 1) or c;
  c := w shr 31;
  zz_8 := zz_8 + ((zz_7 shr 32) + x_7 * x_1);
  zz_9 := zz_9 + ((zz_8 shr 32) + x_7 * x_2);
  zz_10 := zz_10 + ((zz_9 shr 32) + x_7 * x_3);
  zz_11 := zz_11 + ((zz_10 shr 32) + x_7 * x_4);
  zz_12 := zz_12 + ((zz_11 shr 32) + x_7 * x_5);
  zz_13 := zz_13 + ((zz_12 shr 32) + x_7 * x_6);
  zz_14 := zz_14 + (zz_13 shr 32);

  w := UInt32(zz_8);
  zz[zzOff + 8] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_9);
  zz[zzOff + 9] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_10);
  zz[zzOff + 10] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_11);
  zz[zzOff + 11] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_12);
  zz[zzOff + 12] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_13);
  zz[zzOff + 13] := (w shl 1) or c;
  c := w shr 31;
  w := UInt32(zz_14);
  zz[zzOff + 14] := (w shl 1) or c;
  c := w shr 31;
  w := zz[zzOff + 15] + UInt32(zz_14 shr 32);
  zz[zzOff + 15] := (w shl 1) or c;
end;

class function TNat256.Sub(const x, y, z: TCryptoLibUInt32Array): Int32;
var
  c: Int64;
begin
  c := 0;
  c := c + (Int64(x[0]) - y[0]);
  z[0] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[1]) - y[1]);
  z[1] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[2]) - y[2]);
  z[2] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[3]) - y[3]);
  z[3] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[4]) - y[4]);
  z[4] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[5]) - y[5]);
  z[5] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[6]) - y[6]);
  z[6] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[7]) - y[7]);
  z[7] := UInt32(c);
  c := TBits.Asr64(c, 32);
  result := Int32(c);
end;

class function TNat256.Sub(const x: TCryptoLibUInt32Array; xOff: Int32;
  const y: TCryptoLibUInt32Array; yOff: Int32; const z: TCryptoLibUInt32Array;
  zOff: Int32): Int32;
var
  c: Int64;
begin
  c := 0;
  c := c + (Int64(x[xOff + 0]) - y[yOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[xOff + 1]) - y[yOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[xOff + 2]) - y[yOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[xOff + 3]) - y[yOff + 3]);
  z[zOff + 3] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[xOff + 4]) - y[yOff + 4]);
  z[zOff + 4] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[xOff + 5]) - y[yOff + 5]);
  z[zOff + 5] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[xOff + 6]) - y[yOff + 6]);
  z[zOff + 6] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(x[xOff + 7]) - y[yOff + 7]);
  z[zOff + 7] := UInt32(c);
  c := TBits.Asr64(c, 32);
  result := Int32(c);
end;

class function TNat256.SubBothFrom(const x, y, z: TCryptoLibUInt32Array): Int32;
var
  c: Int64;
begin
  c := 0;
  c := c + (Int64(z[0]) - x[0] - y[0]);
  z[0] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[1]) - x[1] - y[1]);
  z[1] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[2]) - x[2] - y[2]);
  z[2] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[3]) - x[3] - y[3]);
  z[3] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[4]) - x[4] - y[4]);
  z[4] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[5]) - x[5] - y[5]);
  z[5] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[6]) - x[6] - y[6]);
  z[6] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[7]) - x[7] - y[7]);
  z[7] := UInt32(c);
  c := TBits.Asr64(c, 32);
  result := Int32(c);
end;

class function TNat256.SubFrom(const x, z: TCryptoLibUInt32Array): Int32;
var
  c: Int64;
begin
  c := 0;
  c := c + (Int64(z[0]) - x[0]);
  z[0] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[1]) - x[1]);
  z[1] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[2]) - x[2]);
  z[2] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[3]) - x[3]);
  z[3] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[4]) - x[4]);
  z[4] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[5]) - x[5]);
  z[5] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[6]) - x[6]);
  z[6] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[7]) - x[7]);
  z[7] := UInt32(c);
  c := TBits.Asr64(c, 32);
  result := Int32(c);
end;

class function TNat256.SubFrom(const x: TCryptoLibUInt32Array; xOff: Int32;
  const z: TCryptoLibUInt32Array; zOff: Int32): Int32;
var
  c: Int64;
begin
  c := 0;
  c := c + (Int64(z[zOff + 0]) - x[xOff + 0]);
  z[zOff + 0] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[zOff + 1]) - x[xOff + 1]);
  z[zOff + 1] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[zOff + 2]) - x[xOff + 2]);
  z[zOff + 2] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[zOff + 3]) - x[xOff + 3]);
  z[zOff + 3] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[zOff + 4]) - x[xOff + 4]);
  z[zOff + 4] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[zOff + 5]) - x[xOff + 5]);
  z[zOff + 5] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[zOff + 6]) - x[xOff + 6]);
  z[zOff + 6] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[zOff + 7]) - x[xOff + 7]);
  z[zOff + 7] := UInt32(c);
  c := TBits.Asr64(c, 32);
  result := Int32(c);
end;

class function TNat256.ToBigInteger(const x: TCryptoLibUInt32Array)
  : TBigInteger;
var
  bs, temp: TCryptoLibByteArray;
  i: Int32;
  x_i: UInt32;
begin
  System.SetLength(bs, 32);
  for i := 0 to System.Pred(8) do

  begin
    x_i := x[i];
    if (x_i <> 0) then
    begin
      temp := TConverters.ReadUInt32AsBytesBE(x_i);
      System.Move(temp[0], bs[(7 - i) shl 2], System.Length(temp) *
        System.SizeOf(Byte))
    end;
  end;
  result := TBigInteger.Create(1, bs);
end;

class function TNat256.ToBigInteger64(const x: TCryptoLibUInt64Array)
  : TBigInteger;
var
  bs, temp: TCryptoLibByteArray;
  i: Int32;
  x_i: UInt64;
begin
  System.SetLength(bs, 32);
  for i := 0 to System.Pred(4) do

  begin
    x_i := x[i];
    if (x_i <> Int64(0)) then
    begin
      temp := TConverters.ReadUInt64AsBytesBE(x_i);
      System.Move(temp[0], bs[(3 - i) shl 3], System.Length(temp) *
        System.SizeOf(Byte))
    end;
  end;
  result := TBigInteger.Create(1, bs);
end;

class procedure TNat256.Zero(const z: TCryptoLibUInt32Array);
begin
  TArrayUtils.Fill(z, 0, 8, UInt32(0));
end;

end.
