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

unit ClpX25519Field;

{$I CryptoLib.inc}

interface

uses
  ClpBits,
  ClpConverters,
  ClpArrayUtils,
  ClpCryptoLibTypes;

type
  TX25519Field = class sealed(TObject)

  strict private

  const
    M24 = Int32($00FFFFFF);
    M25 = Int32($01FFFFFF);
    M26 = Int32($03FFFFFF);

    class var

      FRootNegOne: TCryptoLibInt32Array;

    class procedure Decode128(const bs: TCryptoLibByteArray; off: Int32;
      const z: TCryptoLibInt32Array; zOff: Int32); static; inline;

    class function Decode32(const bs: TCryptoLibByteArray; off: Int32): UInt32;
      static; inline;

    class procedure Encode32(n: UInt32; const bs: TCryptoLibByteArray;
      off: Int32); static; inline;

    class procedure Encode128(const x: TCryptoLibInt32Array; xOff: Int32;
      const bs: TCryptoLibByteArray; off: Int32); static; inline;

    class procedure PowPm5d8(const x, rx2, rz: TCryptoLibInt32Array); static;

    class procedure Reduce(const z: TCryptoLibInt32Array; c: Int32); static;

    class procedure Boot(); static;
    class constructor X25519Field();

  public

    const
    Size = Int32(10);

    class procedure Add(const x, y, z: TCryptoLibInt32Array); static; inline;

    class procedure AddOne(const z: TCryptoLibInt32Array); overload;
      static; inline;

    class procedure AddOne(const z: TCryptoLibInt32Array; zOff: Int32);
      overload; static; inline;

    class procedure Apm(const x, y, zp, zm: TCryptoLibInt32Array);
      static; inline;

    class procedure Carry(const z: TCryptoLibInt32Array); static;

    class procedure CMov(cond: Int32; const x: TCryptoLibInt32Array;
      xOff: Int32; const z: TCryptoLibInt32Array; zOff: Int32); static;

    class procedure CNegate(ANegate: Int32; const z: TCryptoLibInt32Array);
      static; inline;

    class procedure Copy(const x: TCryptoLibInt32Array; xOff: Int32;
      const z: TCryptoLibInt32Array; zOff: Int32); static; inline;

    class function Create(): TCryptoLibInt32Array; static; inline;

    class function CreateTable(n: Int32): TCryptoLibInt32Array; static; inline;

    class procedure CSwap(swap: Int32;
      const a, b: TCryptoLibInt32Array); static;

    class procedure Decode(const x: TCryptoLibByteArray; xOff: Int32;
      const z: TCryptoLibInt32Array); static; inline;

    class procedure Encode(const x: TCryptoLibInt32Array;
      const z: TCryptoLibByteArray; zOff: Int32); static; inline;

    class procedure Inv(const x, z: TCryptoLibInt32Array); static; inline;

    class function IsZero(const x: TCryptoLibInt32Array): Int32; static;

    class function IsZeroVar(const x: TCryptoLibInt32Array): Boolean;
      static; inline;

    class procedure Mul(const x: TCryptoLibInt32Array; y: Int32;
      const z: TCryptoLibInt32Array); overload; static;

    class procedure Mul(const x, y, z: TCryptoLibInt32Array); overload; static;

    class procedure Negate(const x, z: TCryptoLibInt32Array); static; inline;

    class procedure Normalize(const z: TCryptoLibInt32Array); static; inline;

    class procedure One(const z: TCryptoLibInt32Array); static; inline;

    class procedure Sqr(const x, z: TCryptoLibInt32Array); overload; static;

    class procedure Sqr(const x: TCryptoLibInt32Array; n: Int32;
      const z: TCryptoLibInt32Array); overload; static;

    class function SqrtRatioVar(const u, v, z: TCryptoLibInt32Array)
      : Boolean; static;

    class procedure Sub(const x, y, z: TCryptoLibInt32Array); static; inline;

    class procedure SubOne(const z: TCryptoLibInt32Array); static; inline;

    class procedure Zero(const z: TCryptoLibInt32Array); static; inline;

  end;

implementation

{ TX25519Field }

class procedure TX25519Field.Boot;
begin
  FRootNegOne := TCryptoLibInt32Array.Create($020EA0B0, $0386C9D2, $00478C4E,
    $0035697F, $005E8630, $01FBD7A7, $0340264F, $01F0B2B4, $00027E0E,
    $00570649);
end;

class constructor TX25519Field.X25519Field;
begin
  TX25519Field.Boot();
end;

class procedure TX25519Field.Add(const x, y, z: TCryptoLibInt32Array);
var
  i: Int32;
begin
  for i := 0 to System.Pred(Size) do
  begin
    z[i] := x[i] + y[i];
  end;
end;

class procedure TX25519Field.AddOne(const z: TCryptoLibInt32Array);
begin
  z[0] := z[0] + 1;
end;

class procedure TX25519Field.AddOne(const z: TCryptoLibInt32Array; zOff: Int32);
begin
  z[zOff] := z[zOff] + 1;
end;

class procedure TX25519Field.Apm(const x, y, zp, zm: TCryptoLibInt32Array);
var
  i, xi, yi: Int32;
begin
  for i := 0 to System.Pred(Size) do
  begin
    xi := x[i];
    yi := y[i];
    zp[i] := xi + yi;
    zm[i] := xi - yi;
  end;
end;

class procedure TX25519Field.Carry(const z: TCryptoLibInt32Array);
var
  z0, z1, z2, z3, z4, z5, z6, z7, z8, z9: Int32;
begin
  z0 := z[0];
  z1 := z[1];
  z2 := z[2];
  z3 := z[3];
  z4 := z[4];
  z5 := z[5];
  z6 := z[6];
  z7 := z[7];
  z8 := z[8];
  z9 := z[9];

  z3 := z3 + TBits.Asr32(z2, 25);
  z2 := z2 and M25;
  z5 := z5 + TBits.Asr32(z4, 25);
  z4 := z4 and M25;
  z8 := z8 + TBits.Asr32(z7, 25);
  z7 := z7 and M25;
  // z0 := z0 + (z9 shr 24) * 19; z9 := z9 and M24;
  z0 := z0 + (TBits.Asr32(z9, 25) * 38);
  z9 := z9 and M25;

  z1 := z1 + TBits.Asr32(z0, 26);
  z0 := z0 and M26;
  z6 := z6 + TBits.Asr32(z5, 26);
  z5 := z5 and M26;

  z2 := z2 + TBits.Asr32(z1, 26);
  z1 := z1 and M26;
  z4 := z4 + TBits.Asr32(z3, 26);
  z3 := z3 and M26;
  z7 := z7 + TBits.Asr32(z6, 26);
  z6 := z6 and M26;
  z9 := z9 + TBits.Asr32(z8, 26);
  z8 := z8 and M26;

  z[0] := z0;
  z[1] := z1;
  z[2] := z2;
  z[3] := z3;
  z[4] := z4;
  z[5] := z5;
  z[6] := z6;
  z[7] := z7;
  z[8] := z8;
  z[9] := z9;
end;

class procedure TX25519Field.CMov(cond: Int32; const x: TCryptoLibInt32Array;
  xOff: Int32; const z: TCryptoLibInt32Array; zOff: Int32);
var
  i, z_i, diff: Int32;
begin
{$IFDEF DEBUG}
  System.Assert((cond = 0) or (cond = -1));
{$ENDIF DEBUG}
  for i := 0 to System.Pred(Size) do
  begin
    z_i := z[zOff + i];
    diff := z_i xor x[xOff + i];
    z_i := z_i xor (diff and cond);
    z[zOff + i] := z_i;
  end;
end;

class procedure TX25519Field.CNegate(ANegate: Int32;
  const z: TCryptoLibInt32Array);
var
  mask, i: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(TBits.Asr32(ANegate, 1) = 0);
{$ENDIF DEBUG}
  mask := 0 - ANegate;
  for i := 0 to System.Pred(Size) do
  begin
    z[i] := (z[i] xor mask) - mask;
  end;
end;

class procedure TX25519Field.Copy(const x: TCryptoLibInt32Array; xOff: Int32;
  const z: TCryptoLibInt32Array; zOff: Int32);
var
  i: Int32;
begin
  for i := 0 to System.Pred(Size) do
  begin
    z[zOff + i] := x[xOff + i];
  end;
end;

class function TX25519Field.Create: TCryptoLibInt32Array;
begin
  System.SetLength(Result, Size);
end;

class function TX25519Field.CreateTable(n: Int32): TCryptoLibInt32Array;
begin
  System.SetLength(Result, Size * n);
end;

class procedure TX25519Field.CSwap(swap: Int32;
  const a, b: TCryptoLibInt32Array);
var
  mask, i, ai, bi, dummy: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(TBits.Asr32(swap, 1) = 0);
  System.Assert(a <> b);
{$ENDIF DEBUG}
  mask := 0 - swap;
  for i := 0 to System.Pred(Size) do
  begin
    ai := a[i];
    bi := b[i];
    dummy := mask and (ai xor bi);
    a[i] := ai xor dummy;
    b[i] := bi xor dummy;
  end;
end;

class function TX25519Field.Decode32(const bs: TCryptoLibByteArray;
  off: Int32): UInt32;
begin
  // UInt32 n := bs[off];
  // System.Inc(off);
  // n := n or (UInt32(bs[off]) shl 8);
  // System.Inc(off);
  // n := n or (UInt32(bs[off]) shl 16);
  // System.Inc(off);
  // n := n or (UInt32(bs[off]) shl 24);
  // result := n;
  Result := TConverters.ReadBytesAsUInt32LE(PByte(bs), off);
end;

class procedure TX25519Field.Decode128(const bs: TCryptoLibByteArray;
  off: Int32; const z: TCryptoLibInt32Array; zOff: Int32);
var
  t0, t1, t2, t3: UInt32;
begin
  t0 := Decode32(bs, off + 0);
  t1 := Decode32(bs, off + 4);
  t2 := Decode32(bs, off + 8);
  t3 := Decode32(bs, off + 12);

  z[zOff + 0] := Int32(t0 and M26);
  z[zOff + 1] := Int32((t1 shl 6) or (t0 shr 26)) and M26;
  z[zOff + 2] := Int32((t2 shl 12) or (t1 shr 20)) and M25;
  z[zOff + 3] := Int32((t3 shl 19) or (t2 shr 13)) and M26;
  z[zOff + 4] := Int32(t3 shr 7);
end;

class procedure TX25519Field.Decode(const x: TCryptoLibByteArray; xOff: Int32;
  const z: TCryptoLibInt32Array);
begin
  Decode128(x, xOff, z, 0);
  Decode128(x, xOff + 16, z, 5);
  z[9] := z[9] and M24;
end;

class procedure TX25519Field.Encode32(n: UInt32; const bs: TCryptoLibByteArray;
  off: Int32);
begin
  // bs[  off] := Byte(n      );
  // System.Inc(off);
  // bs[off] := Byte(n shr  8);
  // System.Inc(off);
  // bs[off] := Byte(n shr  16);
  // System.Inc(off);
  // bs[off] := Byte(n shr  24);
  TConverters.ReadUInt32AsBytesLE(n, bs, off);
end;

class procedure TX25519Field.Encode128(const x: TCryptoLibInt32Array;
  xOff: Int32; const bs: TCryptoLibByteArray; off: Int32);
var
  x0, x1, x2, x3, x4, t0, t1, t2, t3: UInt32;
begin
  x0 := UInt32(x[xOff + 0]);
  x1 := UInt32(x[xOff + 1]);
  x2 := UInt32(x[xOff + 2]);
  x3 := UInt32(x[xOff + 3]);
  x4 := UInt32(x[xOff + 4]);

  t0 := x0 or (x1 shl 26);
  Encode32(t0, bs, off + 0);
  t1 := (x1 shr 6) or (x2 shl 20);
  Encode32(t1, bs, off + 4);
  t2 := (x2 shr 12) or (x3 shl 13);
  Encode32(t2, bs, off + 8);
  t3 := (x3 shr 19) or (x4 shl 7);
  Encode32(t3, bs, off + 12);
end;

class procedure TX25519Field.Encode(const x: TCryptoLibInt32Array;
  const z: TCryptoLibByteArray; zOff: Int32);
begin
  Encode128(x, 0, z, zOff);
  Encode128(x, 5, z, zOff + 16);
end;

class procedure TX25519Field.Inv(const x, z: TCryptoLibInt32Array);
var
  x2, t: TCryptoLibInt32Array;
begin
  // z = x^(p-2) = x^7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEB
  // (250 1s) (1 0s) (1 1s) (1 0s) (2 1s)
  // Addition chain: [1] [2] 3 5 10 15 25 50 75 125 [250]

  x2 := Create();
  t := Create();
  PowPm5d8(x, x2, t);
  Sqr(t, 3, t);
  Mul(t, x2, z);
end;

class function TX25519Field.IsZero(const x: TCryptoLibInt32Array): Int32;
var
  d, i: Int32;
begin
  d := 0;
  for i := 0 to System.Pred(Size) do
  begin
    d := d or x[i];
  end;
  d := (TBits.Asr32(d, 1)) or (d and 1);
  Result := (d - 1) shr 31;
end;

class function TX25519Field.IsZeroVar(const x: TCryptoLibInt32Array): Boolean;
begin
  Result := IsZero(x) <> 0;
end;

class procedure TX25519Field.Mul(const x: TCryptoLibInt32Array; y: Int32;
  const z: TCryptoLibInt32Array);
var
  x0, x1, x2, x3, x4, x5, x6, x7, x8, x9: Int32;
  c0, c1, c2, c3: Int64;
begin
  x0 := x[0];
  x1 := x[1];
  x2 := x[2];
  x3 := x[3];
  x4 := x[4];
  x5 := x[5];
  x6 := x[6];
  x7 := x[7];
  x8 := x[8];
  x9 := x[9];

  c0 := Int64(x2) * y;
  x2 := Int32(c0) and M25;
  c0 := TBits.Asr64(c0, 25);
  c1 := Int64(x4) * y;
  x4 := Int32(c1) and M25;
  c1 := TBits.Asr64(c1, 25);
  c2 := Int64(x7) * y;
  x7 := Int32(c2) and M25;
  c2 := TBits.Asr64(c2, 25);
  // c3 := Int64(x9) * y; x9 := Int32(c3) and M24; c3 := TBits.Asr64(c3 , 24);
  // c3 := c3 * 19;
  c3 := Int64(x9) * y;
  x9 := Int32(c3) and M25;
  c3 := TBits.Asr64(c3, 25);
  c3 := c3 * 38;

  c3 := c3 + (Int64(x0) * y);
  z[0] := Int32(c3) and M26;
  c3 := TBits.Asr64(c3, 26);
  c1 := c1 + (Int64(x5) * y);
  z[5] := Int32(c1) and M26;
  c1 := TBits.Asr64(c1, 26);

  c3 := c3 + (Int64(x1) * y);
  z[1] := Int32(c3) and M26;
  c3 := TBits.Asr64(c3, 26);
  c0 := c0 + (Int64(x3) * y);
  z[3] := Int32(c0) and M26;
  c0 := TBits.Asr64(c0, 26);
  c1 := c1 + (Int64(x6) * y);
  z[6] := Int32(c1) and M26;
  c1 := TBits.Asr64(c1, 26);
  c2 := c2 + (Int64(x8) * y);
  z[8] := Int32(c2) and M26;
  c2 := TBits.Asr64(c2, 26);

  z[2] := x2 + Int32(c3);
  z[4] := x4 + Int32(c0);
  z[7] := x7 + Int32(c1);
  z[9] := x9 + Int32(c2);
end;

class procedure TX25519Field.Mul(const x, y, z: TCryptoLibInt32Array);
var
  x0, x1, x2, x3, x4, y0, y1, y2, y3, y4, u0, u1, u2, u3, u4, z8, z9: Int32;
  a0, a1, a2, a3, a4, a5, a6, a7, a8, b0, b1, b2, b3, b4, b5, b6, b7, b8, c0,
    c1, c2, c3, c4, c5, c6, c7, c8, v0, v1, v2, v3, v4, t: Int64;
begin
  x0 := x[0];
  y0 := y[0];
  x1 := x[1];
  y1 := y[1];
  x2 := x[2];
  y2 := y[2];
  x3 := x[3];
  y3 := y[3];
  x4 := x[4];
  y4 := y[4];

  u0 := x[5];
  v0 := y[5];
  u1 := x[6];
  v1 := y[6];
  u2 := x[7];
  v2 := y[7];
  u3 := x[8];
  v3 := y[8];
  u4 := x[9];
  v4 := y[9];

  a0 := Int64(x0) * y0;
  a1 := Int64(x0) * y1 + Int64(x1) * y0;
  a2 := Int64(x0) * y2 + Int64(x1) * y1 + Int64(x2) * y0;
  a3 := Int64(x1) * y2 + Int64(x2) * y1;
  a3 := a3 shl 1;
  a3 := a3 + (Int64(x0) * y3 + Int64(x3) * y0);
  a4 := Int64(x2) * y2;
  a4 := a4 shl 1;
  a4 := a4 + (Int64(x0) * y4 + Int64(x1) * y3 + Int64(x3) * y1 +
    Int64(x4) * y0);
  a5 := Int64(x1) * y4 + Int64(x2) * y3 + Int64(x3) * y2 + Int64(x4) * y1;
  a5 := a5 shl 1;
  a6 := Int64(x2) * y4 + Int64(x4) * y2;
  a6 := a6 shl 1;
  a6 := a6 + (Int64(x3) * y3);
  a7 := Int64(x3) * y4 + Int64(x4) * y3;
  a8 := Int64(x4) * y4;
  a8 := a8 shl 1;

  b0 := Int64(u0) * v0;
  b1 := Int64(u0) * v1 + Int64(u1) * v0;
  b2 := Int64(u0) * v2 + Int64(u1) * v1 + Int64(u2) * v0;
  b3 := Int64(u1) * v2 + Int64(u2) * v1;
  b3 := b3 shl 1;
  b3 := b3 + (Int64(u0) * v3 + Int64(u3) * v0);
  b4 := Int64(u2) * v2;
  b4 := b4 shl 1;
  b4 := b4 + (Int64(u0) * v4 + Int64(u1) * v3 + Int64(u3) * v1 +
    Int64(u4) * v0);
  b5 := Int64(u1) * v4 + Int64(u2) * v3 + Int64(u3) * v2 + Int64(u4) * v1;
  // b5     := b5 shl 1;
  b6 := Int64(u2) * v4 + Int64(u4) * v2;
  b6 := b6 shl 1;
  b6 := b6 + (Int64(u3) * v3);
  b7 := Int64(u3) * v4 + Int64(u4) * v3;
  b8 := Int64(u4) * v4;
  // b8     := b8 shl 1;

  a0 := a0 - (b5 * 76);
  a1 := a1 - (b6 * 38);
  a2 := a2 - (b7 * 38);
  a3 := a3 - (b8 * 76);

  a5 := a5 - b0;
  a6 := a6 - b1;
  a7 := a7 - b2;
  a8 := a8 - b3;
  // a9 := -b4;

  x0 := x0 + u0;
  y0 := y0 + v0;
  x1 := x1 + u1;
  y1 := y1 + v1;
  x2 := x2 + u2;
  y2 := y2 + v2;
  x3 := x3 + u3;
  y3 := y3 + v3;
  x4 := x4 + u4;
  y4 := y4 + v4;

  c0 := Int64(x0) * y0;
  c1 := Int64(x0) * y1 + Int64(x1) * y0;
  c2 := Int64(x0) * y2 + Int64(x1) * y1 + Int64(x2) * y0;
  c3 := Int64(x1) * y2 + Int64(x2) * y1;
  c3 := c3 shl 1;
  c3 := c3 + (Int64(x0) * y3 + Int64(x3) * y0);
  c4 := Int64(x2) * y2;
  c4 := c4 shl 1;
  c4 := c4 + (Int64(x0) * y4 + Int64(x1) * y3 + Int64(x3) * y1 +
    Int64(x4) * y0);
  c5 := Int64(x1) * y4 + Int64(x2) * y3 + Int64(x3) * y2 + Int64(x4) * y1;
  c5 := c5 shl 1;
  c6 := Int64(x2) * y4 + Int64(x4) * y2;
  c6 := c6 shl 1;
  c6 := c6 + (Int64(x3) * y3);
  c7 := Int64(x3) * y4 + Int64(x4) * y3;
  c8 := Int64(x4) * y4;
  c8 := c8 shl 1;

  t := a8 + (c3 - a3);
  z8 := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  // t       := t + (a9 + (c4 - a4));
  t := t + ((c4 - a4) - b4);
  // z9       := Int32(t) and M24; t := TBits.Asr64(t , 24);
  // t        := a0 + (t + ((c5 - a5) shl 1)) * 19;
  z9 := Int32(t) and M25;
  t := TBits.Asr64(t, 25);
  t := a0 + (t + c5 - a5) * 38;
  z[0] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  t := t + (a1 + (c6 - a6) * 38);
  z[1] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  t := t + (a2 + (c7 - a7) * 38);
  z[2] := Int32(t) and M25;
  t := TBits.Asr64(t, 25);
  t := t + (a3 + (c8 - a8) * 38);
  z[3] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  // t       := t + (a4 - (a9 * 38));
  t := t + (a4 + b4 * 38);
  z[4] := Int32(t) and M25;
  t := TBits.Asr64(t, 25);
  t := t + (a5 + (c0 - a0));
  z[5] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  t := t + (a6 + (c1 - a1));
  z[6] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  t := t + (a7 + (c2 - a2));
  z[7] := Int32(t) and M25;
  t := TBits.Asr64(t, 25);
  t := t + z8;
  z[8] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  z[9] := z9 + Int32(t);
end;

class procedure TX25519Field.Negate(const x, z: TCryptoLibInt32Array);
var
  i: Int32;
begin
  for i := 0 to System.Pred(Size) do
  begin
    z[i] := -x[i];
  end;
end;

class procedure TX25519Field.Normalize(const z: TCryptoLibInt32Array);
var
  x: Int32;
begin
  x := TBits.Asr32(z[9], 23) and 1;
  Reduce(z, x);
  Reduce(z, -x);
{$IFDEF DEBUG}
  System.Assert(TBits.Asr32(z[9], 24) = 0);
{$ENDIF DEBUG}
end;

class procedure TX25519Field.One(const z: TCryptoLibInt32Array);
var
  i: Int32;
begin
  z[0] := 1;
  for i := 1 to System.Pred(Size) do
  begin
    z[i] := 0;
  end;
end;

class procedure TX25519Field.PowPm5d8(const x, rx2, rz: TCryptoLibInt32Array);
var
  x2, x3, x5, x10, x15, x25, x50, x75, x125, x250, t: TCryptoLibInt32Array;
begin
  // z = x^((p-5)/8) = x^FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD
  // (250 1s) (1 0s) (1 1s)
  // Addition chain: [1] 2 3 5 10 15 25 50 75 125 [250]

  x2 := rx2;
  Sqr(x, x2);
  Mul(x, x2, x2);
  x3 := Create();
  Sqr(x2, x3);
  Mul(x, x3, x3);
  x5 := x3;
  Sqr(x3, 2, x5);
  Mul(x2, x5, x5);
  x10 := Create();
  Sqr(x5, 5, x10);
  Mul(x5, x10, x10);
  x15 := Create();
  Sqr(x10, 5, x15);
  Mul(x5, x15, x15);
  x25 := x5;
  Sqr(x15, 10, x25);
  Mul(x10, x25, x25);
  x50 := x10;
  Sqr(x25, 25, x50);
  Mul(x25, x50, x50);
  x75 := x15;
  Sqr(x50, 25, x75);
  Mul(x25, x75, x75);
  x125 := x25;
  Sqr(x75, 50, x125);
  Mul(x50, x125, x125);
  x250 := x50;
  Sqr(x125, 125, x250);
  Mul(x125, x250, x250);

  t := x125;
  Sqr(x250, 2, t);
  Mul(t, x, rz);
end;

class procedure TX25519Field.Reduce(const z: TCryptoLibInt32Array; c: Int32);
var
  z9, t: Int32;
begin
  z9 := z[9];
  t := z9;
  z9 := t and M24;
  t := TBits.Asr32(t, 24);
  t := t + c;
  t := t * 19;
  t := t + z[0];
  z[0] := t and M26;
  t := TBits.Asr32(t, 26);
  t := t + z[1];
  z[1] := t and M26;
  t := TBits.Asr32(t, 26);
  t := t + z[2];
  z[2] := t and M25;
  t := TBits.Asr32(t, 25);
  t := t + z[3];
  z[3] := t and M26;
  t := TBits.Asr32(t, 26);
  t := t + z[4];
  z[4] := t and M25;
  t := TBits.Asr32(t, 25);
  t := t + z[5];
  z[5] := t and M26;
  t := TBits.Asr32(t, 26);
  t := t + z[6];
  z[6] := t and M26;
  t := TBits.Asr32(t, 26);
  t := t + z[7];
  z[7] := t and M25;
  t := TBits.Asr32(t, 25);
  t := t + z[8];
  z[8] := t and M26;
  t := TBits.Asr32(t, 26);
  t := t + z9;
  z[9] := t;
end;

class procedure TX25519Field.Sqr(const x, z: TCryptoLibInt32Array);
var
  x0, x1, x2, x3, x4, u0, u1, u2, u3, u4, x1_2, x2_2, x3_2, x4_2, u1_2, u2_2,
    u3_2, u4_2, z8, z9: Int32;
  a0, a1, a2, a3, a4, a5, a6, a7, a8, b0, b1, b2, b3, b4, b5, b6, b7, b8, c0,
    c1, c2, c3, c4, c5, c6, c7, c8, t: Int64;
begin
  x0 := x[0];
  x1 := x[1];
  x2 := x[2];
  x3 := x[3];
  x4 := x[4];

  u0 := x[5];
  u1 := x[6];
  u2 := x[7];
  u3 := x[8];
  u4 := x[9];

  x1_2 := x1 * 2;
  x2_2 := x2 * 2;
  x3_2 := x3 * 2;
  x4_2 := x4 * 2;

  a0 := Int64(x0) * x0;
  a1 := Int64(x0) * x1_2;
  a2 := Int64(x0) * x2_2 + Int64(x1) * x1;
  a3 := Int64(x1_2) * x2_2 + Int64(x0) * x3_2;
  a4 := Int64(x2) * x2_2 + Int64(x0) * x4_2 + Int64(x1) * x3_2;
  a5 := Int64(x1_2) * x4_2 + Int64(x2_2) * x3_2;
  a6 := Int64(x2_2) * x4_2 + Int64(x3) * x3;
  a7 := Int64(x3) * x4_2;
  a8 := Int64(x4) * x4_2;

  u1_2 := u1 * 2;
  u2_2 := u2 * 2;
  u3_2 := u3 * 2;
  u4_2 := u4 * 2;

  b0 := Int64(u0) * u0;
  b1 := Int64(u0) * u1_2;
  b2 := Int64(u0) * u2_2 + Int64(u1) * u1;
  b3 := Int64(u1_2) * u2_2 + Int64(u0) * u3_2;
  b4 := Int64(u2) * u2_2 + Int64(u0) * u4_2 + Int64(u1) * u3_2;
  b5 := Int64(u1_2) * u4_2 + Int64(u2_2) * u3_2;
  b6 := Int64(u2_2) * u4_2 + Int64(u3) * u3;
  b7 := Int64(u3) * u4_2;
  b8 := Int64(u4) * u4_2;

  a0 := a0 - (b5 * 38);
  a1 := a1 - (b6 * 38);
  a2 := a2 - (b7 * 38);
  a3 := a3 - (b8 * 38);

  a5 := a5 - b0;
  a6 := a6 - b1;
  a7 := a7 - b2;
  a8 := a8 - b3;
  // Int64 a9 = -b4;

  x0 := x0 + u0;
  x1 := x1 + u1;
  x2 := x2 + u2;
  x3 := x3 + u3;
  x4 := x4 + u4;

  x1_2 := x1 * 2;
  x2_2 := x2 * 2;
  x3_2 := x3 * 2;
  x4_2 := x4 * 2;

  c0 := Int64(x0) * x0;
  c1 := Int64(x0) * x1_2;
  c2 := Int64(x0) * x2_2 + Int64(x1) * x1;
  c3 := Int64(x1_2) * x2_2 + Int64(x0) * x3_2;
  c4 := Int64(x2) * x2_2 + Int64(x0) * x4_2 + Int64(x1) * x3_2;
  c5 := Int64(x1_2) * x4_2 + Int64(x2_2) * x3_2;
  c6 := Int64(x2_2) * x4_2 + Int64(x3) * x3;
  c7 := Int64(x3) * x4_2;
  c8 := Int64(x4) * x4_2;

  t := a8 + (c3 - a3);
  z8 := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  // t  = t + (a9 + (c4 - a4));
  t := t + ((c4 - a4) - b4);
  // z9  := Int32(t) and M24; t := TBits.Asr64(t , 24);
  // t  := a0 + (t + ((c5 - a5) shl 1)) * 19;
  z9 := Int32(t) and M25;
  t := TBits.Asr64(t, 25);
  t := a0 + (t + c5 - a5) * 38;
  z[0] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  t := t + (a1 + (c6 - a6) * 38);
  z[1] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  t := t + (a2 + (c7 - a7) * 38);
  z[2] := Int32(t) and M25;
  t := TBits.Asr64(t, 25);
  t := t + (a3 + (c8 - a8) * 38);
  z[3] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  // t       := t + (a4 - a9 * 38);
  t := t + (a4 + b4 * 38);
  z[4] := Int32(t) and M25;
  t := TBits.Asr64(t, 25);
  t := t + (a5 + (c0 - a0));
  z[5] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  t := t + (a6 + (c1 - a1));
  z[6] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  t := t + (a7 + (c2 - a2));
  z[7] := Int32(t) and M25;
  t := TBits.Asr64(t, 25);
  t := t + z8;
  z[8] := Int32(t) and M26;
  t := TBits.Asr64(t, 26);
  z[9] := z9 + Int32(t);
end;

class procedure TX25519Field.Sqr(const x: TCryptoLibInt32Array; n: Int32;
  const z: TCryptoLibInt32Array);
begin
{$IFDEF DEBUG}
  System.Assert(n > 0);
{$ENDIF DEBUG}
  Sqr(x, z);
  System.Dec(n);

  while (n > 0) do
  begin
    Sqr(z, z);
    System.Dec(n);
  end;
end;

class procedure TX25519Field.Sub(const x, y, z: TCryptoLibInt32Array);
var
  i: Int32;
begin
  for i := 0 to System.Pred(Size) do
  begin
    z[i] := x[i] - y[i];
  end;
end;

class procedure TX25519Field.SubOne(const z: TCryptoLibInt32Array);
begin
  z[0] := z[0] - 1;
end;

class function TX25519Field.SqrtRatioVar(const u, v,
  z: TCryptoLibInt32Array): Boolean;
var
  uv3, uv7, t, x, vx2: TCryptoLibInt32Array;
begin
  uv3 := Create();
  uv7 := Create();

  Mul(u, v, uv3);
  Sqr(v, uv7);
  Mul(uv3, uv7, uv3);
  Sqr(uv7, uv7);
  Mul(uv7, uv3, uv7);

  t := Create();
  x := Create();
  PowPm5d8(uv7, t, x);
  Mul(x, uv3, x);

  vx2 := Create();
  Sqr(x, vx2);
  Mul(vx2, v, vx2);

  Sub(vx2, u, t);
  Normalize(t);
  if (IsZeroVar(t)) then
  begin
    Copy(x, 0, z, 0);
    Result := true;
    Exit;
  end;

  Add(vx2, u, t);
  Normalize(t);
  if (IsZeroVar(t)) then
  begin
    Mul(x, FRootNegOne, z);
    Result := true;
    Exit;
  end;

  Result := false;
end;

class procedure TX25519Field.Zero(const z: TCryptoLibInt32Array);
begin
  TArrayUtils.Fill(z, 0, Size, Int32(0));
end;

end.
