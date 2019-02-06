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

unit ClpSecT283Custom;

{$I CryptoLib.inc}

interface

uses
  ClpBits,
  ClpEncoders,
  ClpNat,
  ClpNat320,
  ClpECC,
  ClpInterleave,
  ClpBigInteger,
  ClpArrayUtils,
  ClpIECC,
  ClpWTauNafMultiplier,
  ClpCryptoLibTypes,
  ClpECCurveConstants,
  ClpISecT283Custom;

resourcestring
  SInvalidValueForSecT283FieldElement =
    'Value Invalid for SecT283FieldElement "%s"';
  SOneOfECFieldElementIsNil = 'Exactly One of the Field Elements is Nil';

type
  TSecT283Field = class sealed(TObject)

  strict private
  const
    M27 = UInt64(System.High(UInt64) shr 37);
    M57 = UInt64(System.High(UInt64) shr 7);

    class var

      FROOT_Z: TCryptoLibUInt64Array;

    class procedure ImplCompactExt(const zz: TCryptoLibUInt64Array); static;
    class procedure ImplExpand(const x, z: TCryptoLibUInt64Array);
      static; inline;
    class procedure ImplMultiply(const x, y, zz: TCryptoLibUInt64Array); static;
    class procedure ImplMulw(x, y: UInt64; const z: TCryptoLibUInt64Array;
      zOff: Int32); static;

    class procedure ImplSquare(const x, zz: TCryptoLibUInt64Array);
      static; inline;

    class procedure Boot(); static;
    class constructor SecT283Field();

  public
    class procedure Add(const x, y, z: TCryptoLibUInt64Array); static; inline;
    class procedure AddExt(const xx, yy, zz: TCryptoLibUInt64Array);
      static; inline;
    class procedure AddOne(const x, z: TCryptoLibUInt64Array); static; inline;
    class function FromBigInteger(const x: TBigInteger): TCryptoLibUInt64Array;
      static; inline;

    class procedure Invert(const x, z: TCryptoLibUInt64Array); static;
    class procedure Multiply(const x, y, z: TCryptoLibUInt64Array);
      static; inline;
    class procedure MultiplyAddToExt(const x, y, zz: TCryptoLibUInt64Array);
      static; inline;
    class procedure Reduce(const xx, z: TCryptoLibUInt64Array); static;
    class procedure Reduce37(const z: TCryptoLibUInt64Array; zOff: Int32);
      static; inline;
    class procedure Sqrt(const x, z: TCryptoLibUInt64Array); static;

    class procedure Square(const x, z: TCryptoLibUInt64Array); static; inline;
    class procedure SquareAddToExt(const x, zz: TCryptoLibUInt64Array);
      static; inline;
    class procedure SquareN(const x: TCryptoLibUInt64Array; n: Int32;
      const z: TCryptoLibUInt64Array); static; inline;

    class function Trace(const x: TCryptoLibUInt64Array): UInt32;
      static; inline;

  end;

type
  TSecT283FieldElement = class(TAbstractF2mFieldElement, ISecT283FieldElement)

  strict private

    function GetM: Int32; inline;

    function GetRepresentation: Int32; inline;

    function GetK1: Int32; inline;
    function GetK2: Int32; inline;
    function GetK3: Int32; inline;

    function Equals(const other: ISecT283FieldElement): Boolean;
      reintroduce; overload;

  strict protected
  var
    Fx: TCryptoLibUInt64Array;

    function GetFieldName: string; override;
    function GetFieldSize: Int32; override;
    function GetIsOne: Boolean; override;
    function GetIsZero: Boolean; override;

    function GetX: TCryptoLibUInt64Array; inline;
    property x: TCryptoLibUInt64Array read GetX;

  public
    constructor Create(); overload;
    constructor Create(const x: TBigInteger); overload;
    constructor Create(const x: TCryptoLibUInt64Array); overload;

    function TestBitZero: Boolean; override;
    function ToBigInteger(): TBigInteger; override;

    function Add(const b: IECFieldElement): IECFieldElement; override;
    function AddOne(): IECFieldElement; override;
    function Subtract(const b: IECFieldElement): IECFieldElement; override;

    function Multiply(const b: IECFieldElement): IECFieldElement; override;
    function MultiplyMinusProduct(const b, x, y: IECFieldElement)
      : IECFieldElement; override;
    function MultiplyPlusProduct(const b, x, y: IECFieldElement)
      : IECFieldElement; override;
    function Divide(const b: IECFieldElement): IECFieldElement; override;
    function Negate(): IECFieldElement; override;
    function Square(): IECFieldElement; override;
    function SquareMinusProduct(const x, y: IECFieldElement)
      : IECFieldElement; override;
    function SquarePlusProduct(const x, y: IECFieldElement)
      : IECFieldElement; override;

    function SquarePow(pow: Int32): IECFieldElement; override;

    function Trace(): Int32; override;

    function Invert(): IECFieldElement; override;

    /// <summary>
    /// return a sqrt root - the routine verifies that the calculation
    /// returns the right value - if <br />none exists it returns null.
    /// </summary>
    function Sqrt(): IECFieldElement; override;

    function Equals(const other: IECFieldElement): Boolean; overload; override;

    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

    property IsZero: Boolean read GetIsZero;
    property IsOne: Boolean read GetIsOne;
    property FieldName: string read GetFieldName;
    property FieldSize: Int32 read GetFieldSize;

    property Representation: Int32 read GetRepresentation;

    property M: Int32 read GetM;

    property k1: Int32 read GetK1;

    property k2: Int32 read GetK2;

    property k3: Int32 read GetK3;

  end;

type
  TSecT283K1Point = class sealed(TAbstractF2mPoint, ISecT283K1Point)

  strict protected
    function Detach(): IECPoint; override;

    function GetCompressionYTilde: Boolean; override;
    property CompressionYTilde: Boolean read GetCompressionYTilde;

  public

    /// <summary>
    /// Create a point which encodes without point compression.
    /// </summary>
    /// <param name="curve">
    /// the curve to use
    /// </param>
    /// <param name="x">
    /// affine x co-ordinate
    /// </param>
    /// <param name="y">
    /// affine y co-ordinate
    /// </param>
    constructor Create(const curve: IECCurve; const x, y: IECFieldElement);
      overload; deprecated 'Use ECCurve.createPoint to construct points';

    /// <summary>
    /// Create a point that encodes with or without point compresion.
    /// </summary>
    /// <param name="curve">
    /// the curve to use
    /// </param>
    /// <param name="x">
    /// affine x co-ordinate
    /// </param>
    /// <param name="y">
    /// affine y co-ordinate
    /// </param>
    /// <param name="withCompression">
    /// if true encode with point compression
    /// </param>
    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      withCompression: Boolean); overload;
      deprecated
      'Per-point compression property will be removed, see GetEncoded(boolean)';

    constructor Create(const curve: IECCurve; const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean); overload;

    function Add(const b: IECPoint): IECPoint; override;
    function Negate(): IECPoint; override;

    function Twice(): IECPoint; override;
    function TwicePlus(const b: IECPoint): IECPoint; override;

    function GetYCoord: IECFieldElement; override;
    property YCoord: IECFieldElement read GetYCoord;

  end;

type
  TSecT283K1Curve = class sealed(TAbstractF2mCurve, ISecT283K1Curve)

  strict private

  type
    TSecT283K1LookupTable = class sealed(TInterfacedObject,
      ISecT283K1LookupTable, IECLookupTable)

    strict private
    var
      Fm_outer: ISecT283K1Curve;
      Fm_table: TCryptoLibUInt64Array;
      Fm_size: Int32;

      function GetSize: Int32; virtual;

    public

      constructor Create(const outer: ISecT283K1Curve;
        const table: TCryptoLibUInt64Array; size: Int32);

      function Lookup(index: Int32): IECPoint; virtual;

      property size: Int32 read GetSize;

    end;

  const
    SECT283K1_DEFAULT_COORDS = Int32(TECCurveConstants.COORD_LAMBDA_PROJECTIVE);
    SECT283K1_FE_LONGS = Int32(5);

    function GetM: Int32; inline;
    function GetK1: Int32; inline;
    function GetK2: Int32; inline;
    function GetK3: Int32; inline;
    function GetIsTrinomial: Boolean; inline;

  strict protected
  var
    Fm_infinity: ISecT283K1Point;

    function GetFieldSize: Int32; override;
    function GetInfinity: IECPoint; override;
    function GetIsKoblitz: Boolean; override;

    function CloneCurve(): IECCurve; override;

    function CreateDefaultMultiplier(): IECMultiplier; override;

    function CreateRawPoint(const x, y: IECFieldElement;
      withCompression: Boolean): IECPoint; overload; override;

    function CreateRawPoint(const x, y: IECFieldElement;
      const zs: TCryptoLibGenericArray<IECFieldElement>;
      withCompression: Boolean): IECPoint; overload; override;

  public
    constructor Create();
    function FromBigInteger(const x: TBigInteger): IECFieldElement; override;

    function SupportsCoordinateSystem(coord: Int32): Boolean; override;

    function CreateCacheSafeLookupTable(const points
      : TCryptoLibGenericArray<IECPoint>; off, len: Int32)
      : IECLookupTable; override;

    property Infinity: IECPoint read GetInfinity;
    property FieldSize: Int32 read GetFieldSize;
    property IsKoblitz: Boolean read GetIsKoblitz;

    property M: Int32 read GetM;
    property k1: Int32 read GetK1;
    property k2: Int32 read GetK2;
    property k3: Int32 read GetK3;
    property IsTrinomial: Boolean read GetIsTrinomial;

  end;

implementation

{ TSecT283Field }

class constructor TSecT283Field.SecT283Field;
begin
  TSecT283Field.Boot;
end;

class procedure TSecT283Field.Reduce37(const z: TCryptoLibUInt64Array;
  zOff: Int32);
var
  z4, t: UInt64;
begin
  z4 := z[zOff + 4];
  t := z4 shr 27;
  z[zOff] := z[zOff] xor (t xor (t shl 5) xor (t shl 7) xor (t shl 12));
  z[zOff + 4] := z4 and M27;
end;

class procedure TSecT283Field.Add(const x, y, z: TCryptoLibUInt64Array);
begin
  z[0] := x[0] xor y[0];
  z[1] := x[1] xor y[1];
  z[2] := x[2] xor y[2];
  z[3] := x[3] xor y[3];
  z[4] := x[4] xor y[4];
end;

class procedure TSecT283Field.AddExt(const xx, yy, zz: TCryptoLibUInt64Array);
begin
  zz[0] := xx[0] xor yy[0];
  zz[1] := xx[1] xor yy[1];
  zz[2] := xx[2] xor yy[2];
  zz[3] := xx[3] xor yy[3];
  zz[4] := xx[4] xor yy[4];
  zz[5] := xx[5] xor yy[5];
  zz[6] := xx[6] xor yy[6];
  zz[7] := xx[7] xor yy[7];
  zz[8] := xx[8] xor yy[8];
end;

class procedure TSecT283Field.AddOne(const x, z: TCryptoLibUInt64Array);
begin
  z[0] := x[0] xor UInt64(1);
  z[1] := x[1];
  z[2] := x[2];
  z[3] := x[3];
  z[4] := x[4];
end;

class procedure TSecT283Field.Boot;
begin
  FROOT_Z := TCryptoLibUInt64Array.Create(UInt64($0C30C30C30C30808),
    UInt64($30C30C30C30C30C3), UInt64($820820820820830C),
    UInt64($0820820820820820), UInt64($2082082));
end;

class function TSecT283Field.FromBigInteger(const x: TBigInteger)
  : TCryptoLibUInt64Array;
var
  z: TCryptoLibUInt64Array;
begin
  z := TNat320.FromBigInteger64(x);
  Reduce37(z, 0);
  result := z;
end;

class procedure TSecT283Field.Multiply(const x, y, z: TCryptoLibUInt64Array);
var
  tt: TCryptoLibUInt64Array;
begin
  tt := TNat320.CreateExt64();
  ImplMultiply(x, y, tt);
  Reduce(tt, z);
end;

class procedure TSecT283Field.ImplSquare(const x, zz: TCryptoLibUInt64Array);
var
  i: Int32;
begin
  for i := 0 to System.Pred(4) do
  begin
    TInterleave.Expand64To128(x[i], zz, i shl 1);
  end;

  zz[8] := TInterleave.Expand32to64(UInt32(x[4]));
end;

class procedure TSecT283Field.Square(const x, z: TCryptoLibUInt64Array);
var
  tt: TCryptoLibUInt64Array;
begin
  tt := TNat.Create64(9);
  ImplSquare(x, tt);
  Reduce(tt, z);
end;

class procedure TSecT283Field.SquareN(const x: TCryptoLibUInt64Array; n: Int32;
  const z: TCryptoLibUInt64Array);
var
  tt: TCryptoLibUInt64Array;
begin
{$IFDEF DEBUG}
  System.Assert(n > 0);
{$ENDIF DEBUG}
  tt := TNat.Create64(9);
  ImplSquare(x, tt);
  Reduce(tt, z);

  System.Dec(n);
  while (n > 0) do
  begin
    ImplSquare(z, tt);
    Reduce(tt, z);
    System.Dec(n);
  end;
end;

class procedure TSecT283Field.Invert(const x, z: TCryptoLibUInt64Array);
var
  t0, t1: TCryptoLibUInt64Array;
begin
  if TNat320.IsZero64(x) then
  begin
    raise EInvalidOperationCryptoLibException.Create('');
  end;

  // Itoh-Tsujii inversion

  t0 := TNat320.Create64();
  t1 := TNat320.Create64();

  Square(x, t0);
  Multiply(t0, x, t0);
  SquareN(t0, 2, t1);
  Multiply(t1, t0, t1);
  SquareN(t1, 4, t0);
  Multiply(t0, t1, t0);
  SquareN(t0, 8, t1);
  Multiply(t1, t0, t1);
  Square(t1, t1);
  Multiply(t1, x, t1);
  SquareN(t1, 17, t0);
  Multiply(t0, t1, t0);
  Square(t0, t0);
  Multiply(t0, x, t0);
  SquareN(t0, 35, t1);
  Multiply(t1, t0, t1);
  SquareN(t1, 70, t0);
  Multiply(t0, t1, t0);
  Square(t0, t0);
  Multiply(t0, x, t0);
  SquareN(t0, 141, t1);
  Multiply(t1, t0, t1);
  Square(t1, z);
end;

class procedure TSecT283Field.ImplCompactExt(const zz: TCryptoLibUInt64Array);
var
  z0, z1, z2, z3, z4, z5, z6, z7, z8, z9: UInt64;
begin
  z0 := zz[0];
  z1 := zz[1];
  z2 := zz[2];
  z3 := zz[3];
  z4 := zz[4];
  z5 := zz[5];
  z6 := zz[6];
  z7 := zz[7];
  z8 := zz[8];
  z9 := zz[9];
  zz[0] := z0 xor (z1 shl 57);
  zz[1] := (z1 shr 7) xor (z2 shl 50);
  zz[2] := (z2 shr 14) xor (z3 shl 43);
  zz[3] := (z3 shr 21) xor (z4 shl 36);
  zz[4] := (z4 shr 28) xor (z5 shl 29);
  zz[5] := (z5 shr 35) xor (z6 shl 22);
  zz[6] := (z6 shr 42) xor (z7 shl 15);
  zz[7] := (z7 shr 49) xor (z8 shl 8);
  zz[8] := (z8 shr 56) xor (z9 shl 1);
  zz[9] := (z9 shr 63); // Zero!
end;

class procedure TSecT283Field.ImplExpand(const x, z: TCryptoLibUInt64Array);
var
  x0, x1, x2, x3, x4: UInt64;
begin
  x0 := x[0];
  x1 := x[1];
  x2 := x[2];
  x3 := x[3];
  x4 := x[4];
  z[0] := x0 and M57;
  z[1] := ((x0 shr 57) xor (x1 shl 7)) and M57;
  z[2] := ((x1 shr 50) xor (x2 shl 14)) and M57;
  z[3] := ((x2 shr 43) xor (x3 shl 21)) and M57;
  z[4] := ((x3 shr 36) xor (x4 shl 28));
end;

class procedure TSecT283Field.ImplMultiply(const x, y,
  zz: TCryptoLibUInt64Array);
var
  a, b, p: TCryptoLibUInt64Array;
  u0, u1, u2, u3, v0, v1, v2, v3, A4, A5, B4, B5, t1, t2, t3, t4, t5, t6, t7,
    t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22,
    t23, t24, t25, t26, t27, t28, t29, t30, t31, t32, t33, t34, t35, t36, t37,
    t38, t39: UInt64;
begin
  // /*
  // * Formula (17) from "Some New Results on Binary Polynomial Multiplication",
  // * Murat Cenk and M. Anwar Hasan.
  // *
  // * The formula as given contained an error in the term t25, as noted below
  // */
  System.SetLength(a, 5);
  System.SetLength(b, 5);
  ImplExpand(x, a);
  ImplExpand(y, b);

  System.SetLength(p, 26);

  ImplMulw(a[0], b[0], p, 0); // m1
  ImplMulw(a[1], b[1], p, 2); // m2
  ImplMulw(a[2], b[2], p, 4); // m3
  ImplMulw(a[3], b[3], p, 6); // m4
  ImplMulw(a[4], b[4], p, 8); // m5

  u0 := a[0] xor a[1];
  v0 := b[0] xor b[1];
  u1 := a[0] xor a[2];
  v1 := b[0] xor b[2];
  u2 := a[2] xor a[4];
  v2 := b[2] xor b[4];
  u3 := a[3] xor a[4];
  v3 := b[3] xor b[4];

  ImplMulw(u1 xor a[3], v1 xor b[3], p, 18); // m10
  ImplMulw(u2 xor a[1], v2 xor b[1], p, 20); // m11

  A4 := u0 xor u3;
  B4 := v0 xor v3;
  A5 := A4 xor a[2];
  B5 := B4 xor b[2];

  ImplMulw(A4, B4, p, 22); // m12
  ImplMulw(A5, B5, p, 24); // m13

  ImplMulw(u0, v0, p, 10); // m6
  ImplMulw(u1, v1, p, 12); // m7
  ImplMulw(u2, v2, p, 14); // m8
  ImplMulw(u3, v3, p, 16); // m9


  // Improved method factors out common single-word terms
  // NOTE: p1,...,p26 in the paper maps to p[0],...,p[25] here

  zz[0] := p[0];
  zz[9] := p[9];

  t1 := p[0] xor p[1];
  t2 := t1 xor p[2];
  t3 := t2 xor p[10];

  zz[1] := t3;

  t4 := p[3] xor p[4];
  t5 := p[11] xor p[12];
  t6 := t4 xor t5;
  t7 := t2 xor t6;

  zz[2] := t7;

  t8 := t1 xor t4;
  t9 := p[5] xor p[6];
  t10 := t8 xor t9;
  t11 := t10 xor p[8];
  t12 := p[13] xor p[14];
  t13 := t11 xor t12;
  t14 := p[18] xor p[22];
  t15 := t14 xor p[24];
  t16 := t13 xor t15;

  zz[3] := t16;

  t17 := p[7] xor p[8];
  t18 := t17 xor p[9];
  t19 := t18 xor p[17];

  zz[8] := t19;

  t20 := t18 xor t9;
  t21 := p[15] xor p[16];
  t22 := t20 xor t21;

  zz[7] := t22;

  t23 := t22 xor t3;
  t24 := p[19] xor p[20];
  // t25 := p[23] xor  p[24];
  t25 := p[25] xor p[24]; // Fixes an error in the paper: p[23] -> p{25]
  t26 := p[18] xor p[23];
  t27 := t24 xor t25;
  t28 := t27 xor t26;
  t29 := t28 xor t23;

  zz[4] := t29;

  t30 := t7 xor t19;
  t31 := t27 xor t30;
  t32 := p[21] xor p[22];
  t33 := t31 xor t32;

  zz[5] := t33;

  t34 := t11 xor p[0];
  t35 := t34 xor p[9];
  t36 := t35 xor t12;
  t37 := t36 xor p[21];
  t38 := t37 xor p[23];
  t39 := t38 xor p[25];

  zz[6] := t39;

  ImplCompactExt(zz);
end;

class procedure TSecT283Field.ImplMulw(x, y: UInt64;
  const z: TCryptoLibUInt64Array; zOff: Int32);
var
  u: TCryptoLibUInt64Array;
  j: UInt32;
  g, h, l: UInt64;
  k: Int32;
begin
{$IFDEF DEBUG}
  System.Assert((x shr 57) = 0);
  System.Assert((y shr 57) = 0);
{$ENDIF DEBUG}
  System.SetLength(u, 8);
  // u[0] := 0;
  u[1] := y;
  u[2] := u[1] shl 1;
  u[3] := u[2] xor y;
  u[4] := u[2] shl 1;
  u[5] := u[4] xor y;
  u[6] := u[3] shl 1;
  u[7] := u[6] xor y;

  j := UInt32(x);
  h := 0;
  l := u[j and 7];
  k := 48;

  repeat

    j := UInt32(x shr k);
    g := u[j and 7] xor u[(j shr 3) and 7] shl 3 xor u[(j shr 6) and 7] shl 6;
    l := l xor ((g shl k));
    h := h xor TBits.NegativeRightShift64(g, -k);

    System.Dec(k, 9);
  until not(k > 0);

  h := h xor (((x and Int64($0100804020100800)) and
    UInt64(TBits.Asr64(Int64(y) shl 7, 63))) shr 8);

{$IFDEF DEBUG}
  System.Assert((h shr 49) = 0);
{$ENDIF DEBUG}
  z[zOff] := l and M57;
  z[zOff + 1] := (l shr 57) xor (h shl 7);
end;

class procedure TSecT283Field.MultiplyAddToExt(const x, y,
  zz: TCryptoLibUInt64Array);
var
  tt: TCryptoLibUInt64Array;
begin
  tt := TNat320.CreateExt64();
  ImplMultiply(x, y, tt);
  AddExt(zz, tt, zz);
end;

class procedure TSecT283Field.Reduce(const xx, z: TCryptoLibUInt64Array);
var
  x0, x1, x2, x3, x4, x5, x6, x7, x8, t: UInt64;
begin
  x0 := xx[0];
  x1 := xx[1];
  x2 := xx[2];
  x3 := xx[3];
  x4 := xx[4];
  x5 := xx[5];
  x6 := xx[6];
  x7 := xx[7];
  x8 := xx[8];

  x3 := x3 xor ((x8 shl 37) xor (x8 shl 42) xor (x8 shl 44) xor (x8 shl 49));
  x4 := x4 xor ((x8 shr 27) xor (x8 shr 22) xor (x8 shr 20) xor (x8 shr 15));

  x2 := x2 xor ((x7 shl 37) xor (x7 shl 42) xor (x7 shl 44) xor (x7 shl 49));
  x3 := x3 xor ((x7 shr 27) xor (x7 shr 22) xor (x7 shr 20) xor (x7 shr 15));

  x1 := x1 xor ((x6 shl 37) xor (x6 shl 42) xor (x6 shl 44) xor (x6 shl 49));
  x2 := x2 xor ((x6 shr 27) xor (x6 shr 22) xor (x6 shr 20) xor (x6 shr 15));

  x0 := x0 xor ((x5 shl 37) xor (x5 shl 42) xor (x5 shl 44) xor (x5 shl 49));
  x1 := x1 xor ((x5 shr 27) xor (x5 shr 22) xor (x5 shr 20) xor (x5 shr 15));

  t := x4 shr 27;
  z[0] := x0 xor t xor (t shl 5) xor (t shl 7) xor (t shl 12);
  z[1] := x1;
  z[2] := x2;
  z[3] := x3;
  z[4] := x4 and M27;
end;

class procedure TSecT283Field.Sqrt(const x, z: TCryptoLibUInt64Array);
var
  u0, u1, e0, e1, e2: UInt64;
  odd: TCryptoLibUInt64Array;
begin
  odd := TNat320.Create64();

  u0 := TInterleave.Unshuffle(x[0]);
  u1 := TInterleave.Unshuffle(x[1]);
  e0 := (u0 and UInt64($00000000FFFFFFFF)) or (u1 shl 32);
  odd[0] := (u0 shr 32) or (u1 and UInt64($FFFFFFFF00000000));

  u0 := TInterleave.Unshuffle(x[2]);
  u1 := TInterleave.Unshuffle(x[3]);
  e1 := (u0 and UInt64($00000000FFFFFFFF)) or (u1 shl 32);
  odd[1] := (u0 shr 32) or (u1 and UInt64($FFFFFFFF00000000));

  u0 := TInterleave.Unshuffle(x[4]);
  e2 := (u0 and UInt64($00000000FFFFFFFF));
  odd[2] := (u0 shr 32);

  Multiply(odd, FROOT_Z, z);

  z[0] := z[0] xor e0;
  z[1] := z[1] xor e1;
  z[2] := z[2] xor e2;
end;

class procedure TSecT283Field.SquareAddToExt(const x,
  zz: TCryptoLibUInt64Array);
var
  tt: TCryptoLibUInt64Array;
begin
  tt := TNat.Create64(9);
  ImplSquare(x, tt);
  AddExt(zz, tt, zz);
end;

class function TSecT283Field.Trace(const x: TCryptoLibUInt64Array): UInt32;
begin
  // Non-zero-trace bits: 0, 271
  result := UInt32(x[0] xor (x[4] shr 15)) and UInt32(1);
end;

{ TSecT283FieldElement }

function TSecT283FieldElement.Add(const b: IECFieldElement): IECFieldElement;
var
  z: TCryptoLibUInt64Array;
begin
  z := TNat320.Create64();
  TSecT283Field.Add(Fx, (b as ISecT283FieldElement).x, z);
  result := TSecT283FieldElement.Create(z);
end;

function TSecT283FieldElement.AddOne: IECFieldElement;
var
  z: TCryptoLibUInt64Array;
begin
  z := TNat320.Create64();
  TSecT283Field.AddOne(Fx, z);
  result := TSecT283FieldElement.Create(z);
end;

constructor TSecT283FieldElement.Create(const x: TBigInteger);
begin
  if ((not(x.IsInitialized)) or (x.SignValue < 0) or (x.BitLength > 283)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt
      (@SInvalidValueForSecT283FieldElement, ['x']);
  end;
  Inherited Create();
  Fx := TSecT283Field.FromBigInteger(x);
end;

constructor TSecT283FieldElement.Create;
begin
  Inherited Create();
  Fx := TNat320.Create64();
end;

constructor TSecT283FieldElement.Create(const x: TCryptoLibUInt64Array);
begin
  Inherited Create();
  Fx := x;
end;

function TSecT283FieldElement.Divide(const b: IECFieldElement): IECFieldElement;
begin
  result := Multiply(b.Invert());
end;

function TSecT283FieldElement.Equals(const other: ISecT283FieldElement)
  : Boolean;
begin
  if ((Self as ISecT283FieldElement) = other) then
  begin
    result := true;
    Exit;
  end;
  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := TNat320.Eq64(Fx, other.x);
end;

function TSecT283FieldElement.Equals(const other: IECFieldElement): Boolean;
begin
  result := Equals(other as ISecT283FieldElement);
end;

function TSecT283FieldElement.GetFieldName: string;
begin
  result := 'SecT283Field';
end;

function TSecT283FieldElement.GetFieldSize: Int32;
begin
  result := 283;
end;

function TSecT283FieldElement.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := 2831275 xor TArrayUtils.GetArrayHashCode(Fx, 0, 5);
end;

function TSecT283FieldElement.GetIsOne: Boolean;
begin
  result := TNat320.IsOne64(Fx);
end;

function TSecT283FieldElement.GetIsZero: Boolean;
begin
  result := TNat320.IsZero64(Fx);
end;

function TSecT283FieldElement.GetK1: Int32;
begin
  result := 5;
end;

function TSecT283FieldElement.GetK2: Int32;
begin
  result := 7;
end;

function TSecT283FieldElement.GetK3: Int32;
begin
  result := 12;
end;

function TSecT283FieldElement.GetM: Int32;
begin
  result := 283;
end;

function TSecT283FieldElement.GetRepresentation: Int32;
begin
  result := TF2mFieldElement.Ppb;
end;

function TSecT283FieldElement.GetX: TCryptoLibUInt64Array;
begin
  result := Fx;
end;

function TSecT283FieldElement.Invert: IECFieldElement;
var
  z: TCryptoLibUInt64Array;
begin
  z := TNat320.Create64();
  TSecT283Field.Invert(Fx, z);
  result := TSecT283FieldElement.Create(z);
end;

function TSecT283FieldElement.Multiply(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt64Array;
begin
  z := TNat320.Create64();
  TSecT283Field.Multiply(Fx, (b as ISecT283FieldElement).x, z);
  result := TSecT283FieldElement.Create(z);
end;

function TSecT283FieldElement.MultiplyMinusProduct(const b, x,
  y: IECFieldElement): IECFieldElement;
begin
  result := MultiplyPlusProduct(b, x, y);
end;

function TSecT283FieldElement.MultiplyPlusProduct(const b, x,
  y: IECFieldElement): IECFieldElement;
var
  ax, bx, xx, yx, tt, z: TCryptoLibUInt64Array;
begin
  ax := Fx;
  bx := (b as ISecT283FieldElement).x;
  xx := (x as ISecT283FieldElement).x;
  yx := (y as ISecT283FieldElement).x;

  tt := TNat.Create64(9);
  TSecT283Field.MultiplyAddToExt(ax, bx, tt);
  TSecT283Field.MultiplyAddToExt(xx, yx, tt);

  z := TNat320.Create64();
  TSecT283Field.Reduce(tt, z);
  result := TSecT283FieldElement.Create(z);
end;

function TSecT283FieldElement.Negate: IECFieldElement;
begin
  result := Self as IECFieldElement;
end;

function TSecT283FieldElement.Sqrt: IECFieldElement;
var
  z: TCryptoLibUInt64Array;
begin
  z := TNat320.Create64();
  TSecT283Field.Sqrt(Fx, z);
  result := TSecT283FieldElement.Create(z);
end;

function TSecT283FieldElement.Square: IECFieldElement;
var
  z: TCryptoLibUInt64Array;
begin
  z := TNat320.Create64();
  TSecT283Field.Square(Fx, z);
  result := TSecT283FieldElement.Create(z);
end;

function TSecT283FieldElement.SquareMinusProduct(const x, y: IECFieldElement)
  : IECFieldElement;
begin
  result := SquarePlusProduct(x, y);
end;

function TSecT283FieldElement.SquarePlusProduct(const x, y: IECFieldElement)
  : IECFieldElement;
var
  ax, xx, yx, tt, z: TCryptoLibUInt64Array;
begin
  ax := Fx;
  xx := (x as ISecT283FieldElement).x;
  yx := (y as ISecT283FieldElement).x;

  tt := TNat.Create64(9);
  TSecT283Field.SquareAddToExt(ax, tt);
  TSecT283Field.MultiplyAddToExt(xx, yx, tt);

  z := TNat320.Create64();
  TSecT283Field.Reduce(tt, z);
  result := TSecT283FieldElement.Create(z);
end;

function TSecT283FieldElement.SquarePow(pow: Int32): IECFieldElement;
var
  z: TCryptoLibUInt64Array;
begin
  if (pow < 1) then
  begin
    result := Self as IECFieldElement;
    Exit;
  end;

  z := TNat320.Create64();
  TSecT283Field.SquareN(Fx, pow, z);
  result := TSecT283FieldElement.Create(z);
end;

function TSecT283FieldElement.Subtract(const b: IECFieldElement)
  : IECFieldElement;
begin
  // Addition and subtraction are the same in F2m
  result := Add(b);
end;

function TSecT283FieldElement.TestBitZero: Boolean;
begin
  result := (Fx[0] and UInt64(1)) <> UInt64(0);
end;

function TSecT283FieldElement.ToBigInteger: TBigInteger;
begin
  result := TNat320.ToBigInteger64(Fx);
end;

function TSecT283FieldElement.Trace: Int32;
begin
  result := Int32(TSecT283Field.Trace(Fx));
end;

{ TSecT283K1Point }

function TSecT283K1Point.Add(const b: IECPoint): IECPoint;
var
  LCurve: IECCurve;
  x1, x2, L1, L2, z1, z2, u2, S2, u1, S1, a, LB, x3, L3, z3, Y1, Y2, l, Y3, AU1,
    AU2, ABZ2: IECFieldElement;
  Z1IsOne, Z2IsOne: Boolean;
  p: IECPoint;
begin
  if ((IsInfinity)) then
  begin
    result := b;
    Exit;
  end;
  if ((b.IsInfinity)) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  LCurve := curve;

  x1 := RawXCoord;
  x2 := b.RawXCoord;

  if (x1.IsZero) then
  begin
    if (x2.IsZero) then
    begin
      result := LCurve.Infinity;
      Exit;
    end;

    result := b.Add(Self as IECPoint);
    Exit;
  end;

  L1 := RawYCoord;
  z1 := RawZCoords[0];
  L2 := b.RawYCoord;
  z2 := b.RawZCoords[0];

  Z1IsOne := z1.IsOne;
  u2 := x2;
  S2 := L2;
  if (not(Z1IsOne)) then
  begin
    u2 := u2.Multiply(z1);
    S2 := S2.Multiply(z1);
  end;

  Z2IsOne := z2.IsOne;
  u1 := x1;
  S1 := L1;
  if (not(Z2IsOne)) then
  begin
    u1 := u1.Multiply(z2);
    S1 := S1.Multiply(z2);
  end;

  a := S1.Add(S2);
  LB := u1.Add(u2);

  if (LB.IsZero) then
  begin
    if (a.IsZero) then
    begin
      result := Twice();
      Exit;
    end;

    result := LCurve.Infinity;
    Exit;
  end;

  if (x2.IsZero) then
  begin
    // TODO This can probably be optimized quite a bit
    p := Self.Normalize();
    x1 := p.XCoord;
    Y1 := p.YCoord;

    Y2 := L2;
    l := Y1.Add(Y2).Divide(x1);

    x3 := l.Square().Add(l).Add(x1);
    if (x3.IsZero) then
    begin
      result := TSecT283K1Point.Create(LCurve, x3, LCurve.b, IsCompressed);
      Exit;
    end;

    Y3 := l.Multiply(x1.Add(x3)).Add(x3).Add(Y1);
    L3 := Y3.Divide(x3).Add(x3);
    z3 := LCurve.FromBigInteger(TBigInteger.One);
  end
  else
  begin
    LB := LB.Square();

    AU1 := a.Multiply(u1);
    AU2 := a.Multiply(u2);

    x3 := AU1.Multiply(AU2);
    if (x3.IsZero) then
    begin
      result := TSecT283K1Point.Create(curve, x3, curve.b, IsCompressed);
      Exit;
    end;

    ABZ2 := a.Multiply(LB);
    if (not(Z2IsOne)) then
    begin
      ABZ2 := ABZ2.Multiply(z2);
    end;

    L3 := AU2.Add(LB).SquarePlusProduct(ABZ2, L1.Add(z1));

    z3 := ABZ2;
    if (not(Z1IsOne)) then
    begin
      z3 := z3.Multiply(z1);
    end;
  end;

  result := TSecT283K1Point.Create(LCurve, x3, L3,
    TCryptoLibGenericArray<IECFieldElement>.Create(z3), IsCompressed);
end;

constructor TSecT283K1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement);
begin
  Create(curve, x, y, false);
end;

constructor TSecT283K1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

constructor TSecT283K1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
  if ((x = Nil) <> (y = Nil)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SOneOfECFieldElementIsNil);
  end;
end;

function TSecT283K1Point.Detach: IECPoint;
begin
  result := TSecT283K1Point.Create(Nil, AffineXCoord, AffineYCoord);
end;

function TSecT283K1Point.GetCompressionYTilde: Boolean;
var
  x, y: IECFieldElement;
begin
  x := RawXCoord;
  if (x.IsZero) then
  begin
    result := false;
    Exit;
  end;

  y := RawYCoord;

  // Y is actually Lambda (X + Y/X) here
  result := y.TestBitZero() <> x.TestBitZero();
end;

function TSecT283K1Point.GetYCoord: IECFieldElement;
var
  x, l, y, z: IECFieldElement;
begin
  x := RawXCoord;
  l := RawYCoord;

  if ((IsInfinity) or (x.IsZero)) then
  begin
    result := l;
    Exit;
  end;

  // Y is actually Lambda (X + Y/X) here; convert to affine value on the fly
  y := l.Add(x).Multiply(x);

  z := RawZCoords[0];
  if (not(z.IsOne)) then
  begin
    y := y.Divide(z);
  end;

  result := y;
end;

function TSecT283K1Point.Negate: IECPoint;
var
  x, l, z: IECFieldElement;
begin
  if (IsInfinity) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  x := RawXCoord;
  if (x.IsZero) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  // L is actually Lambda (X + Y/X) here
  l := RawYCoord;
  z := RawZCoords[0];
  result := TSecT283K1Point.Create(curve, x, l.Add(z),
    TCryptoLibGenericArray<IECFieldElement>.Create(z), IsCompressed);
end;

function TSecT283K1Point.Twice: IECPoint;
var
  LCurve: IECCurve;
  x1, L1, z1, Z1Sq, t, x3, z3, t1, t2, L3: IECFieldElement;
  Z1IsOne: Boolean;
begin
  if ((IsInfinity)) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  LCurve := Self.curve;

  x1 := RawXCoord;
  if (x1.IsZero) then
  begin
    // A point with X == 0 is it's own Additive inverse
    result := LCurve.Infinity;
    Exit;
  end;

  L1 := RawYCoord;
  z1 := RawZCoords[0];

  Z1IsOne := z1.IsOne;
  if Z1IsOne then
  begin
    Z1Sq := z1;
  end
  else
  begin
    Z1Sq := z1.Square();
  end;

  if (Z1IsOne) then
  begin
    t := L1.Square().Add(L1);
  end
  else
  begin
    t := L1.Add(z1).Multiply(L1);
  end;

  if (t.IsZero) then
  begin
    result := TSecT283K1Point.Create(LCurve, t, LCurve.b, IsCompressed);
    Exit;
  end;

  x3 := t.Square();
  if Z1IsOne then
  begin
    z3 := t;
  end
  else
  begin
    z3 := t.Multiply(Z1Sq);
  end;

  t1 := L1.Add(x1).Square();

  if Z1IsOne then
  begin
    t2 := z1;
  end
  else
  begin
    t2 := Z1Sq.Square();
  end;

  L3 := t1.Add(t).Add(Z1Sq).Multiply(t1).Add(t2).Add(x3).Add(z3);

  result := TSecT283K1Point.Create(LCurve, x3, L3,
    TCryptoLibGenericArray<IECFieldElement>.Create(z3), IsCompressed);
end;

function TSecT283K1Point.TwicePlus(const b: IECPoint): IECPoint;
var
  LCurve: IECCurve;
  x1, x2, z2, L1, z1, L2, X1Sq, L1Sq, Z1Sq, L1Z1, t, L2plus1, a, X2Z1Sq, LB, x3,
    z3, L3: IECFieldElement;
begin
  if ((IsInfinity)) then
  begin
    result := b;
    Exit;
  end;
  if (b.IsInfinity) then
  begin
    result := Twice();
    Exit;
  end;

  LCurve := Self.curve;

  x1 := RawXCoord;
  if (x1.IsZero) then
  begin
    // A point with X == 0 is it's own Additive inverse
    result := b;
    Exit;
  end;

  // NOTE: TwicePlus() only optimized for lambda-affine argument
  x2 := b.RawXCoord;
  z2 := b.RawZCoords[0];
  if ((x2.IsZero) or (not(z2.IsOne))) then
  begin
    result := Twice().Add(b);
    Exit;
  end;

  L1 := RawYCoord;
  z1 := RawZCoords[0];
  L2 := b.RawYCoord;

  X1Sq := x1.Square();
  L1Sq := L1.Square();
  Z1Sq := z1.Square();
  L1Z1 := L1.Multiply(z1);

  t := L1Sq.Add(L1Z1);
  L2plus1 := L2.AddOne();
  a := L2plus1.Multiply(Z1Sq).Add(L1Sq).MultiplyPlusProduct(t, X1Sq, Z1Sq);
  X2Z1Sq := x2.Multiply(Z1Sq);
  LB := X2Z1Sq.Add(t).Square();

  if (LB.IsZero) then
  begin
    if (a.IsZero) then
    begin
      result := b.Twice();
      Exit;
    end;

    result := LCurve.Infinity;
    Exit;
  end;

  if (a.IsZero) then
  begin
    result := TSecT283K1Point.Create(LCurve, a, LCurve.b, IsCompressed);
    Exit;
  end;

  x3 := a.Square().Multiply(X2Z1Sq);
  z3 := a.Multiply(LB).Multiply(Z1Sq);
  L3 := a.Add(LB).Square().MultiplyPlusProduct(t, L2plus1, z3);

  result := TSecT283K1Point.Create(LCurve, x3, L3,
    TCryptoLibGenericArray<IECFieldElement>.Create(z3), IsCompressed);
end;

{ TSecT283K1Curve }

constructor TSecT283K1Curve.Create;
begin
  Inherited Create(283, 5, 7, 12);
  Fm_infinity := TSecT283K1Point.Create(Self as IECCurve, Nil, Nil);

  Fm_a := FromBigInteger(TBigInteger.Zero);
  Fm_b := FromBigInteger(TBigInteger.One);
  Fm_order := TBigInteger.Create(1,
    THex.Decode
    ('01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9AE2ED07577265DFF7F94451E061E163C61')
    );
  Fm_cofactor := TBigInteger.ValueOf(4);

  Fm_coord := SECT283K1_DEFAULT_COORDS;
end;

function TSecT283K1Curve.CloneCurve: IECCurve;
begin
  result := TSecT283K1Curve.Create() as ISecT283K1Curve;
end;

function TSecT283K1Curve.CreateCacheSafeLookupTable(const points
  : TCryptoLibGenericArray<IECPoint>; off, len: Int32): IECLookupTable;
var
  table: TCryptoLibUInt64Array;
  pos, i: Int32;
  p: IECPoint;
begin
  System.SetLength(table, len * SECT283K1_FE_LONGS * 2);

  pos := 0;
  for i := 0 to System.Pred(len) do
  begin
    p := points[off + i];
    TNat320.Copy64((p.RawXCoord as ISecT283FieldElement).x, 0, table, pos);
    pos := pos + SECT283K1_FE_LONGS;
    TNat320.Copy64((p.RawYCoord as ISecT283FieldElement).x, 0, table, pos);
    pos := pos + SECT283K1_FE_LONGS;
  end;

  result := TSecT283K1LookupTable.Create(Self as ISecT283K1Curve, table, len);
end;

function TSecT283K1Curve.CreateRawPoint(const x, y: IECFieldElement;
  withCompression: Boolean): IECPoint;
begin
  result := TSecT283K1Point.Create(Self as IECCurve, x, y, withCompression);
end;

function TSecT283K1Curve.CreateRawPoint(const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean)
  : IECPoint;
begin
  result := TSecT283K1Point.Create(Self as IECCurve, x, y, zs, withCompression);
end;

function TSecT283K1Curve.FromBigInteger(const x: TBigInteger): IECFieldElement;
begin
  result := TSecT283FieldElement.Create(x);
end;

function TSecT283K1Curve.GetFieldSize: Int32;
begin
  result := 283;
end;

function TSecT283K1Curve.GetInfinity: IECPoint;
begin
  result := Fm_infinity;
end;

function TSecT283K1Curve.GetIsKoblitz: Boolean;
begin
  result := true;
end;

function TSecT283K1Curve.GetIsTrinomial: Boolean;
begin
  result := false;
end;

function TSecT283K1Curve.GetK1: Int32;
begin
  result := 5;
end;

function TSecT283K1Curve.GetK2: Int32;
begin
  result := 7;
end;

function TSecT283K1Curve.GetK3: Int32;
begin
  result := 12;
end;

function TSecT283K1Curve.GetM: Int32;
begin
  result := 283;
end;

function TSecT283K1Curve.SupportsCoordinateSystem(coord: Int32): Boolean;
begin
  case coord of
    TECCurveConstants.COORD_LAMBDA_PROJECTIVE:
      result := true
  else
    result := false;
  end;
end;

function TSecT283K1Curve.CreateDefaultMultiplier(): IECMultiplier;
begin
  result := TWTauNafMultiplier.Create() as IECMultiplier;
end;

{ TSecT283K1Curve.TSecT283K1LookupTable }

constructor TSecT283K1Curve.TSecT283K1LookupTable.Create
  (const outer: ISecT283K1Curve; const table: TCryptoLibUInt64Array;
  size: Int32);
begin
  Inherited Create();
  Fm_outer := outer;
  Fm_table := table;
  Fm_size := size;
end;

function TSecT283K1Curve.TSecT283K1LookupTable.GetSize: Int32;
begin
  result := Fm_size;
end;

function TSecT283K1Curve.TSecT283K1LookupTable.Lookup(index: Int32): IECPoint;
var
  x, y: TCryptoLibUInt64Array;
  pos, i, j: Int32;
  MASK: UInt64;
begin
  x := TNat320.Create64();
  y := TNat320.Create64();
  pos := 0;

  for i := 0 to System.Pred(Fm_size) do
  begin
    MASK := UInt64(Int64(TBits.Asr32((i xor index) - 1, 31)));

    for j := 0 to System.Pred(SECT283K1_FE_LONGS) do
    begin
      x[j] := x[j] xor (Fm_table[pos + j] and MASK);
      y[j] := y[j] xor (Fm_table[pos + SECT283K1_FE_LONGS + j] and MASK);
    end;

    pos := pos + (SECT283K1_FE_LONGS * 2);
  end;

  result := Fm_outer.CreateRawPoint(TSecT283FieldElement.Create(x)
    as ISecT283FieldElement, TSecT283FieldElement.Create(y)
    as ISecT283FieldElement, false);
end;

end.
