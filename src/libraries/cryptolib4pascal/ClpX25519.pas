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

unit ClpX25519;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpX25519Field,
  ClpEd25519,
  ClpArrayUtils,
  ClpConverters,
  ClpCryptoLibTypes;

type
  TX25519 = class sealed(TObject)
  strict private
  const
    C_A = Int32(486662);
    C_A24 = Int32((C_A + 2) div 4);

  public

    const
    PointSize = Int32(32);
    ScalarSize = Int32(32);

    class function CalculateAgreement(const k: TCryptoLibByteArray; kOff: Int32;
      const u: TCryptoLibByteArray; uOff: Int32; const r: TCryptoLibByteArray;
      rOff: Int32): Boolean; static; inline;

    class function Decode32(const bs: TCryptoLibByteArray; off: Int32): UInt32;
      static; inline;

    class procedure DecodeScalar(const k: TCryptoLibByteArray; kOff: Int32;
      const n: TCryptoLibUInt32Array); static; inline;

    class procedure GeneratePrivateKey(const random: ISecureRandom;
      const k: TCryptoLibByteArray); static; inline;

    class procedure GeneratePublicKey(const k: TCryptoLibByteArray; kOff: Int32;
      r: TCryptoLibByteArray; rOff: Int32); static; inline;

    class procedure PointDouble(const x, z: TCryptoLibInt32Array); static;

    class procedure Precompute(); static;

    class procedure ScalarMult(const k: TCryptoLibByteArray; kOff: Int32;
      const u: TCryptoLibByteArray; uOff: Int32; const r: TCryptoLibByteArray;
      rOff: Int32); static;

    class procedure ScalarMultBase(const k: TCryptoLibByteArray; kOff: Int32;
      const r: TCryptoLibByteArray; rOff: Int32); static;

  end;

implementation

{ TX25519 }

class function TX25519.CalculateAgreement(const k: TCryptoLibByteArray;
  kOff: Int32; const u: TCryptoLibByteArray; uOff: Int32;
  const r: TCryptoLibByteArray; rOff: Int32): Boolean;
begin
  ScalarMult(k, kOff, u, uOff, r, rOff);
  result := not TArrayUtils.AreAllZeroes(r, rOff, PointSize);
end;

class function TX25519.Decode32(const bs: TCryptoLibByteArray;
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
  result := TConverters.ReadBytesAsUInt32LE(PByte(bs), off);
end;

class procedure TX25519.DecodeScalar(const k: TCryptoLibByteArray; kOff: Int32;
  const n: TCryptoLibUInt32Array);
var
  i: Int32;
begin
  for i := 0 to System.Pred(8) do
  begin
    n[i] := Decode32(k, kOff + i * 4);
  end;

  n[0] := n[0] and UInt32($FFFFFFF8);
  n[7] := n[7] and UInt32($7FFFFFFF);
  n[7] := n[7] or UInt32($40000000);
end;

class procedure TX25519.GeneratePrivateKey(const random: ISecureRandom;
  const k: TCryptoLibByteArray);
begin
  random.NextBytes(k);

  k[0] := k[0] and $F8;
  k[ScalarSize - 1] := k[ScalarSize - 1] and $7F;
  k[ScalarSize - 1] := k[ScalarSize - 1] or $40;
end;

class procedure TX25519.GeneratePublicKey(const k: TCryptoLibByteArray;
  kOff: Int32; r: TCryptoLibByteArray; rOff: Int32);
begin
  ScalarMultBase(k, kOff, r, rOff);
end;

class procedure TX25519.PointDouble(const x, z: TCryptoLibInt32Array);
var
  A, B: TCryptoLibInt32Array;
begin
  A := TX25519Field.Create();
  B := TX25519Field.Create();

  TX25519Field.Apm(x, z, A, B);
  TX25519Field.Sqr(A, A);
  TX25519Field.Sqr(B, B);
  TX25519Field.Mul(A, B, x);
  TX25519Field.Sub(A, B, A);
  TX25519Field.Mul(A, C_A24, z);
  TX25519Field.Add(z, B, z);
  TX25519Field.Mul(z, A, z);
end;

class procedure TX25519.Precompute;
begin
  TEd25519.Precompute();
end;

class procedure TX25519.ScalarMult(const k: TCryptoLibByteArray; kOff: Int32;
  const u: TCryptoLibByteArray; uOff: Int32; const r: TCryptoLibByteArray;
  rOff: Int32);
var
  n: TCryptoLibUInt32Array;
  x1, x2, z2, x3, z3, t1, t2: TCryptoLibInt32Array;
  bit, swap, word, shift, kt, i: Int32;
begin
  System.SetLength(n, 8);
  DecodeScalar(k, kOff, n);

  x1 := TX25519Field.Create();
  TX25519Field.Decode(u, uOff, x1);
  x2 := TX25519Field.Create();
  TX25519Field.Copy(x1, 0, x2, 0);
  z2 := TX25519Field.Create();
  z2[0] := 1;
  x3 := TX25519Field.Create();
  x3[0] := 1;
  z3 := TX25519Field.Create();

  t1 := TX25519Field.Create();
  t2 := TX25519Field.Create();

{$IFDEF DEBUG}
  System.Assert((n[7] shr 30) = UInt32(1));
{$ENDIF DEBUG}
  bit := 254;
  swap := 1;
  repeat
    TX25519Field.Apm(x3, z3, t1, x3);
    TX25519Field.Apm(x2, z2, z3, x2);
    TX25519Field.Mul(t1, x2, t1);
    TX25519Field.Mul(x3, z3, x3);
    TX25519Field.Sqr(z3, z3);
    TX25519Field.Sqr(x2, x2);

    TX25519Field.Sub(z3, x2, t2);
    TX25519Field.Mul(t2, C_A24, z2);
    TX25519Field.Add(z2, x2, z2);
    TX25519Field.Mul(z2, t2, z2);
    TX25519Field.Mul(x2, z3, x2);

    TX25519Field.Apm(t1, x3, x3, z3);
    TX25519Field.Sqr(x3, x3);
    TX25519Field.Sqr(z3, z3);
    TX25519Field.Mul(z3, x1, z3);

    System.Dec(bit);

    word := bit shr 5;
    shift := bit and $1F;
    kt := Int32(n[word] shr shift) and 1;
    swap := swap xor kt;
    TX25519Field.CSwap(swap, x2, x3);
    TX25519Field.CSwap(swap, z2, z3);
    swap := kt;

  until (bit < 3);

{$IFDEF DEBUG}
  System.Assert(swap = 0);
{$ENDIF DEBUG}
  i := 0;
  while i < 3 do
  begin
    PointDouble(x2, z2);
    System.Inc(i);
  end;

  TX25519Field.Inv(z2, z2);
  TX25519Field.Mul(x2, z2, x2);

  TX25519Field.Normalize(x2);
  TX25519Field.Encode(x2, r, rOff);
end;

class procedure TX25519.ScalarMultBase(const k: TCryptoLibByteArray;
  kOff: Int32; const r: TCryptoLibByteArray; rOff: Int32);
var
  y, z: TCryptoLibInt32Array;
begin
  y := TX25519Field.Create();
  z := TX25519Field.Create();

  TEd25519.ScalarMultBaseYZ(k, kOff, y, z);

  TX25519Field.Apm(z, y, y, z);

  TX25519Field.Inv(z, z);
  TX25519Field.Mul(y, z, y);

  TX25519Field.Normalize(y);
  TX25519Field.Encode(y, r, rOff);
end;

end.
