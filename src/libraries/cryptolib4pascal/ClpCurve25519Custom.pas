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

unit ClpCurve25519Custom;

{$I CryptoLib.inc}

interface

uses
  ClpECC,
  ClpIECC,
  ClpNat,
  ClpMod,
  ClpNat256,
  ClpBigInteger,
  ClpICurve25519Custom,
  ClpECCurveConstants,
  ClpBits,
  ClpEncoders,
  ClpArrayUtils,
  ClpCryptoLibTypes;

resourcestring
  SInvalidValueForCurve25519FieldElement =
    'Value Invalid for Curve25519FieldElement "%s"';
  SOneOfECFieldElementIsNil = 'Exactly One of the Field Elements is Nil';

type
  // 2^255 - 2^4 - 2^1 - 1
  TCurve25519Field = class sealed(TObject)

  strict private
  const
    P7 = UInt32($7FFFFFFF);
    PInv = UInt32($13);

    class var

      FP, FPExt: TCryptoLibUInt32Array;

    class function AddPTo(const z: TCryptoLibUInt32Array): UInt32;
      static; inline;

    class function AddPExtTo(const zz: TCryptoLibUInt32Array): UInt32;
      static; inline;

    class function SubPFrom(const z: TCryptoLibUInt32Array): Int32;
      static; inline;

    class function SubPExtFrom(const zz: TCryptoLibUInt32Array): Int32;
      static; inline;

    class function GetP: TCryptoLibUInt32Array; static; inline;

    class procedure Boot(); static;
    class constructor Curve25519Field();

  public
    class procedure Add(const x, y, z: TCryptoLibUInt32Array); static; inline;
    class procedure AddExt(const xx, yy, zz: TCryptoLibUInt32Array);
      static; inline;
    class procedure AddOne(const x, z: TCryptoLibUInt32Array); static; inline;
    class function FromBigInteger(const x: TBigInteger): TCryptoLibUInt32Array;
      static; inline;
    class procedure Half(const x, z: TCryptoLibUInt32Array); static; inline;
    class procedure Multiply(const x, y, z: TCryptoLibUInt32Array);
      static; inline;
    class procedure MultiplyAddToExt(const x, y, zz: TCryptoLibUInt32Array);
      static; inline;
    class procedure Negate(const x, z: TCryptoLibUInt32Array); static; inline;
    class procedure Reduce(const xx, z: TCryptoLibUInt32Array); static; inline;
    class procedure Reduce27(x: UInt32; const z: TCryptoLibUInt32Array);
      static; inline;
    class procedure Square(const x, z: TCryptoLibUInt32Array); static; inline;
    class procedure SquareN(const x: TCryptoLibUInt32Array; n: Int32;
      const z: TCryptoLibUInt32Array); static; inline;
    class procedure Subtract(const x, y, z: TCryptoLibUInt32Array);
      static; inline;
    class procedure SubtractExt(const xx, yy, zz: TCryptoLibUInt32Array);
      static; inline;
    class procedure Twice(const x, z: TCryptoLibUInt32Array); static; inline;

    class property P: TCryptoLibUInt32Array read GetP;

  end;

type
  TCurve25519FieldElement = class(TAbstractFpFieldElement,
    ICurve25519FieldElement)

  strict private
    class var

      FPRECOMP_POW2: TCryptoLibUInt32Array;

    function Equals(const other: ICurve25519FieldElement): Boolean;
      reintroduce; overload;

    class function GetQ: TBigInteger; static; inline;

    class procedure Boot(); static;
    class constructor Curve25519FieldElement();

  strict protected
  var
    Fx: TCryptoLibUInt32Array;

    function GetFieldName: string; override;
    function GetFieldSize: Int32; override;
    function GetIsOne: Boolean; override;
    function GetIsZero: Boolean; override;

    function GetX: TCryptoLibUInt32Array; inline;
    property x: TCryptoLibUInt32Array read GetX;

  public
    constructor Create(); overload;
    constructor Create(const x: TBigInteger); overload;
    constructor Create(const x: TCryptoLibUInt32Array); overload;

    function TestBitZero: Boolean; override;
    function ToBigInteger(): TBigInteger; override;

    function Add(const b: IECFieldElement): IECFieldElement; override;
    function AddOne(): IECFieldElement; override;
    function Subtract(const b: IECFieldElement): IECFieldElement; override;

    function Multiply(const b: IECFieldElement): IECFieldElement; override;
    function Divide(const b: IECFieldElement): IECFieldElement; override;
    function Negate(): IECFieldElement; override;
    function Square(): IECFieldElement; override;

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

    class property Q: TBigInteger read GetQ;

  end;

type
  TCurve25519Point = class sealed(TAbstractFpPoint, ICurve25519Point)

  strict protected
    function Detach(): IECPoint; override;
    function CalculateJacobianModifiedW(const z: ICurve25519FieldElement;
      const ZSquared: TCryptoLibUInt32Array): ICurve25519FieldElement; virtual;
    function GetJacobianModifiedW(): ICurve25519FieldElement; virtual;
    function TwiceJacobianModified(calculateW: Boolean)
      : ICurve25519Point; virtual;

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

    function ThreeTimes(): IECPoint; override;

    function GetZCoord(index: Int32): IECFieldElement; override;

  end;

type
  TCurve25519 = class sealed(TAbstractFpCurve, ICurve25519)

  strict private

  type
    TCurve25519LookupTable = class sealed(TAbstractECLookupTable,
      ICurve25519LookupTable)

    strict private
    var
      Fm_outer: ICurve25519;
      Fm_table: TCryptoLibUInt32Array;
      Fm_size: Int32;

      function CreatePoint(const x, y: TCryptoLibUInt32Array): IECPoint;

    strict protected

      function GetSize: Int32; override;

    public

      constructor Create(const outer: ICurve25519;
        const table: TCryptoLibUInt32Array; size: Int32);

      function Lookup(index: Int32): IECPoint; override;
      function LookupVar(index: Int32): IECPoint; override;

    end;

  const
    Curve25519_DEFAULT_COORDS = Int32
      (TECCurveConstants.COORD_JACOBIAN_MODIFIED);
    CURVE25519_FE_INTS = Int32(8);

  var
    Fq: TBigInteger;

  strict protected
  var
    Fm_infinity: ICurve25519Point;

    function GetQ: TBigInteger; virtual;
    function GetFieldSize: Int32; override;
    function GetInfinity: IECPoint; override;

    function CloneCurve(): IECCurve; override;

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

    property Q: TBigInteger read GetQ;
    property Infinity: IECPoint read GetInfinity;
    property FieldSize: Int32 read GetFieldSize;

  end;

implementation

{ TCurve25519Field }

class function TCurve25519Field.AddPTo(const z: TCryptoLibUInt32Array): UInt32;
var
  c: Int64;
begin
  c := Int64(z[0]) - PInv;
  z[0] := UInt32(c);
  c := c shr 32;
  if (c <> 0) then
  begin
    c := TNat.DecAt(7, z, 1);
  end;
  c := c + (Int64(z[7]) + (P7 + 1));
  z[7] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TCurve25519Field.AddPExtTo
  (const zz: TCryptoLibUInt32Array): UInt32;
var
  c: Int64;
begin
  c := Int64(zz[0]) + FPExt[0];
  zz[0] := UInt32(c);
  c := c shr 32;
  if (c <> 0) then
  begin
    c := TNat.IncAt(8, zz, 1);
  end;
  c := c + (Int64(zz[8]) - PInv);
  zz[8] := UInt32(c);
  c := c shr 32;
  if (c <> 0) then
  begin
    c := TNat.DecAt(15, zz, 9);
  end;
  c := c + (Int64(zz[15]) + (FPExt[15] + 1));
  zz[15] := UInt32(c);
  c := c shr 32;
  result := UInt32(c);
end;

class function TCurve25519Field.SubPFrom(const z: TCryptoLibUInt32Array): Int32;
var
  c: Int64;
begin
  c := Int64(z[0]) + PInv;
  z[0] := UInt32(c);
  c := c shr 32;
  if (c <> 0) then
  begin
    c := TNat.IncAt(7, z, 1);
  end;
  c := c + (Int64(z[7]) - (P7 + 1));
  z[7] := UInt32(c);
  c := c shr 32;
  result := Int32(c);
end;

class function TCurve25519Field.SubPExtFrom
  (const zz: TCryptoLibUInt32Array): Int32;
var
  c: Int64;
begin
  c := Int64(zz[0]) - FPExt[0];
  zz[0] := UInt32(c);
  c := c shr 32;
  if (c <> 0) then
  begin
    c := TNat.DecAt(8, zz, 1);
  end;
  c := c + (Int64(zz[8]) + PInv);
  zz[8] := UInt32(c);
  c := c shr 32;
  if (c <> 0) then
  begin
    c := TNat.IncAt(15, zz, 9);
  end;
  c := c + (Int64(zz[15]) - (FPExt[15] + 1));
  zz[15] := UInt32(c);
  c := c shr 32;
  result := Int32(c);
end;

class constructor TCurve25519Field.Curve25519Field;
begin
  TCurve25519Field.Boot;
end;

class function TCurve25519Field.GetP: TCryptoLibUInt32Array;
begin
  result := FP;
end;

class procedure TCurve25519Field.Add(const x, y, z: TCryptoLibUInt32Array);
begin
  TNat256.Add(x, y, z);
  if (TNat256.Gte(z, P)) then
  begin
    SubPFrom(z);
  end;
end;

class procedure TCurve25519Field.AddExt(const xx, yy,
  zz: TCryptoLibUInt32Array);
begin
  TNat.Add(16, xx, yy, zz);
  if (TNat.Gte(16, zz, FPExt)) then
  begin
    SubPExtFrom(zz);
  end;
end;

class procedure TCurve25519Field.AddOne(const x, z: TCryptoLibUInt32Array);
begin
  TNat.Inc(8, x, z);
  if (TNat256.Gte(z, P)) then
  begin
    SubPFrom(z);
  end;
end;

class procedure TCurve25519Field.Boot;
begin
  FP := TCryptoLibUInt32Array.Create($FFFFFFED, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
    $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $7FFFFFFF);
  FPExt := TCryptoLibUInt32Array.Create($00000169, $00000000, $00000000,
    $00000000, $00000000, $00000000, $00000000, $00000000, $FFFFFFED, $FFFFFFFF,
    $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $3FFFFFFF);
end;

class function TCurve25519Field.FromBigInteger(const x: TBigInteger)
  : TCryptoLibUInt32Array;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.FromBigInteger(x);
  while (TNat256.Gte(z, P)) do
  begin
    TNat256.SubFrom(P, z);
  end;
  result := z;
end;

class procedure TCurve25519Field.Half(const x, z: TCryptoLibUInt32Array);
begin
  if ((x[0] and 1) = 0) then
  begin
    TNat.ShiftDownBit(8, x, 0, z);
  end
  else
  begin
    TNat256.Add(x, P, z);
    TNat.ShiftDownBit(8, z, 0);
  end;
end;

class procedure TCurve25519Field.Reduce(const xx, z: TCryptoLibUInt32Array);
var
  xx07, c, z7: UInt32;
begin
{$IFDEF DEBUG}
  System.Assert((xx[15] shr 30) = 0);
{$ENDIF DEBUG}
  xx07 := xx[7];
  TNat.ShiftUpBit(8, xx, 8, xx07, z, 0);
  c := TNat256.MulByWordAddTo(PInv, xx, z) shl 1;
  z7 := z[7];
  c := c + ((z7 shr 31) - (xx07 shr 31));
  z7 := z7 and P7;
  z7 := z7 + (TNat.AddWordTo(7, c * PInv, z));
  z[7] := z7;
  if ((z7 >= P7) and (TNat256.Gte(z, P))) then
  begin
    SubPFrom(z);
  end;
end;

class procedure TCurve25519Field.Multiply(const x, y, z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
  tt := TNat256.CreateExt();
  TNat256.Mul(x, y, tt);
  Reduce(tt, z);
end;

class procedure TCurve25519Field.MultiplyAddToExt(const x, y,
  zz: TCryptoLibUInt32Array);
begin
  TNat256.MulAddTo(x, y, zz);
  if (TNat.Gte(16, zz, FPExt)) then
  begin
    SubPExtFrom(zz);
  end;
end;

class procedure TCurve25519Field.Negate(const x, z: TCryptoLibUInt32Array);
begin
  if (TNat256.IsZero(x)) then
  begin
    TNat256.Zero(z);
  end
  else
  begin
    TNat256.Sub(P, x, z);
  end;
end;

class procedure TCurve25519Field.Reduce27(x: UInt32;
  const z: TCryptoLibUInt32Array);
var
  z7, c: UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(((x shr 26) = 0));
{$ENDIF DEBUG}
  z7 := z[7];
  c := ((x shl 1) or (z7 shr 31));
  z7 := z7 and P7;
  z7 := z7 + (TNat.AddWordTo(7, c * PInv, z));
  z[7] := z7;
  if ((z7 >= P7) and (TNat256.Gte(z, P))) then
  begin
    SubPFrom(z);
  end;
end;

class procedure TCurve25519Field.Square(const x, z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
  tt := TNat256.CreateExt();
  TNat256.Square(x, tt);
  Reduce(tt, z);
end;

class procedure TCurve25519Field.SquareN(const x: TCryptoLibUInt32Array;
  n: Int32; const z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
{$IFDEF DEBUG}
  System.Assert(n > 0);
{$ENDIF DEBUG}
  tt := TNat256.CreateExt();
  TNat256.Square(x, tt);
  Reduce(tt, z);
  System.Dec(n);
  while (n > 0) do
  begin
    TNat256.Square(z, tt);
    Reduce(tt, z);
    System.Dec(n);
  end;
end;

class procedure TCurve25519Field.Subtract(const x, y, z: TCryptoLibUInt32Array);
var
  c: Int32;
begin
  c := TNat256.Sub(x, y, z);
  if (c <> 0) then
  begin
    AddPTo(z);
  end;
end;

class procedure TCurve25519Field.SubtractExt(const xx, yy,
  zz: TCryptoLibUInt32Array);
var
  c: Int32;
begin
  c := TNat.Sub(16, xx, yy, zz);
  if (c <> 0) then
  begin
    AddPExtTo(zz);
  end;
end;

class procedure TCurve25519Field.Twice(const x, z: TCryptoLibUInt32Array);
begin
  TNat.ShiftUpBit(8, x, 0, z);
  if (TNat256.Gte(z, P)) then
  begin
    SubPFrom(z);
  end;
end;

{ TCurve25519FieldElement }

class function TCurve25519FieldElement.GetQ: TBigInteger;
begin
  result := TNat256.ToBigInteger(TCurve25519Field.P);
end;

class procedure TCurve25519FieldElement.Boot;
begin
  // Calculated as TBigInteger.ValueOf(2).modPow(Q.shiftRight(2), Q)
  FPRECOMP_POW2 := TCryptoLibUInt32Array.Create($4A0EA0B0, $C4EE1B27, $AD2FE478,
    $2F431806, $3DFBD7A7, $2B4D0099, $4FC1DF0B, $2B832480);
end;

class constructor TCurve25519FieldElement.Curve25519FieldElement;
begin
  TCurve25519FieldElement.Boot();
end;

function TCurve25519FieldElement.GetX: TCryptoLibUInt32Array;
begin
  result := Fx;
end;

constructor TCurve25519FieldElement.Create;
begin
  Inherited Create();
  Fx := TNat256.Create();
end;

constructor TCurve25519FieldElement.Create(const x: TBigInteger);
begin
  if ((not(x.IsInitialized)) or (x.SignValue < 0) or (x.CompareTo(Q) >= 0)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt
      (@SInvalidValueForCurve25519FieldElement, ['x']);
  end;
  Inherited Create();
  Fx := TCurve25519Field.FromBigInteger(x);
end;

constructor TCurve25519FieldElement.Create(const x: TCryptoLibUInt32Array);
begin
  Inherited Create();
  Fx := x;
end;

function TCurve25519FieldElement.GetFieldName: string;
begin
  result := 'Curve25519Field';
end;

function TCurve25519FieldElement.GetFieldSize: Int32;
begin
  result := Q.BitLength;
end;

function TCurve25519FieldElement.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := Q.GetHashCode() xor TArrayUtils.GetArrayHashCode(Fx, 0, 8);
end;

function TCurve25519FieldElement.GetIsOne: Boolean;
begin
  result := TNat256.IsOne(Fx);
end;

function TCurve25519FieldElement.GetIsZero: Boolean;
begin
  result := TNat256.IsZero(Fx);
end;

function TCurve25519FieldElement.Invert: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TMod.Invert(TCurve25519Field.P, Fx, z);
  result := TCurve25519FieldElement.Create(z);
end;

function TCurve25519FieldElement.Multiply(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TCurve25519Field.Multiply(Fx, (b as ICurve25519FieldElement).x, z);
  result := TCurve25519FieldElement.Create(z);
end;

function TCurve25519FieldElement.Negate: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TCurve25519Field.Negate(Fx, z);
  result := TCurve25519FieldElement.Create(z);
end;

function TCurve25519FieldElement.Sqrt: IECFieldElement;
var
  x1, x2, x3, x4, x7, x11, x15, x30, x60, x120, x131, x251, t1,
    t2: TCryptoLibUInt32Array;
begin

  (*
    * Q == 8m + 5, so we use Pocklington's method for this case.
    *
    * First, raise this element to the exponent 2^252 - 2^1 (i.e. m + 1)
    *
    * Breaking up the exponent's binary representation into "repunits", we get:
    * { 251 1s } { 1 0s }
    *
    * Therefore we need an addition chain containing 251 (the lengths of the repunits)
    * We use: 1, 2, 3, 4, 7, 11, 15, 30, 60, 120, 131, [251]
  *)

  x1 := Fx;
  if ((TNat256.IsZero(x1)) or (TNat256.IsOne(x1))) then
  begin
    result := Self as IECFieldElement;
    Exit;
  end;

  x2 := TNat256.Create();
  TCurve25519Field.Square(x1, x2);
  TCurve25519Field.Multiply(x2, x1, x2);
  x3 := x2;
  TCurve25519Field.Square(x2, x3);
  TCurve25519Field.Multiply(x3, x1, x3);
  x4 := TNat256.Create();
  TCurve25519Field.Square(x3, x4);
  TCurve25519Field.Multiply(x4, x1, x4);
  x7 := TNat256.Create();
  TCurve25519Field.SquareN(x4, 3, x7);
  TCurve25519Field.Multiply(x7, x3, x7);
  x11 := x3;
  TCurve25519Field.SquareN(x7, 4, x11);
  TCurve25519Field.Multiply(x11, x4, x11);
  x15 := x7;
  TCurve25519Field.SquareN(x11, 4, x15);
  TCurve25519Field.Multiply(x15, x4, x15);
  x30 := x4;
  TCurve25519Field.SquareN(x15, 15, x30);
  TCurve25519Field.Multiply(x30, x15, x30);
  x60 := x15;
  TCurve25519Field.SquareN(x30, 30, x60);
  TCurve25519Field.Multiply(x60, x30, x60);
  x120 := x30;
  TCurve25519Field.SquareN(x60, 60, x120);
  TCurve25519Field.Multiply(x120, x60, x120);
  x131 := x60;
  TCurve25519Field.SquareN(x120, 11, x131);
  TCurve25519Field.Multiply(x131, x11, x131);
  x251 := x11;
  TCurve25519Field.SquareN(x131, 120, x251);
  TCurve25519Field.Multiply(x251, x120, x251);

  t1 := x251;
  TCurve25519Field.Square(t1, t1);

  t2 := x120;
  TCurve25519Field.Square(t1, t2);

  if (TNat256.Eq(x1, t2)) then
  begin
    result := TCurve25519FieldElement.Create(t1);
    Exit;
  end;

  (*
    * If the first guess is incorrect, we multiply by a precomputed power of 2 to get the second guess,
    * which is ((4x)^(m + 1))/2 mod Q
  *)
  TCurve25519Field.Multiply(t1, FPRECOMP_POW2, t1);

  TCurve25519Field.Square(t1, t2);

  if (TNat256.Eq(x1, t2)) then
  begin
    result := TCurve25519FieldElement.Create(t1);
    Exit;
  end;

  result := Nil;

end;

function TCurve25519FieldElement.Square: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TCurve25519Field.Square(Fx, z);
  result := TCurve25519FieldElement.Create(z);
end;

function TCurve25519FieldElement.Subtract(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TCurve25519Field.Subtract(Fx, (b as ICurve25519FieldElement).x, z);
  result := TCurve25519FieldElement.Create(z);
end;

function TCurve25519FieldElement.TestBitZero: Boolean;
begin
  result := TNat256.GetBit(Fx, 0) = 1;
end;

function TCurve25519FieldElement.ToBigInteger: TBigInteger;
begin
  result := TNat256.ToBigInteger(Fx);
end;

function TCurve25519FieldElement.Add(const b: IECFieldElement): IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TCurve25519Field.Add(x, (b as ICurve25519FieldElement).x, z);
  result := TCurve25519FieldElement.Create(z);
end;

function TCurve25519FieldElement.AddOne: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TCurve25519Field.AddOne(Fx, z);
  result := TCurve25519FieldElement.Create(z);
end;

function TCurve25519FieldElement.Divide(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TMod.Invert(TCurve25519Field.P, (b as ICurve25519FieldElement).x, z);
  TCurve25519Field.Multiply(z, Fx, z);
  result := TCurve25519FieldElement.Create(z);
end;

function TCurve25519FieldElement.Equals(const other
  : ICurve25519FieldElement): Boolean;
begin
  if ((Self as ICurve25519FieldElement) = other) then
  begin
    result := true;
    Exit;
  end;
  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := TNat256.Eq(Fx, other.x);
end;

function TCurve25519FieldElement.Equals(const other: IECFieldElement): Boolean;
begin
  result := Equals(other as ICurve25519FieldElement);
end;

{ TCurve25519Point }

function TCurve25519Point.Detach: IECPoint;
begin
  result := TCurve25519Point.Create(Nil, AffineXCoord, AffineYCoord);
end;

function TCurve25519Point.CalculateJacobianModifiedW
  (const z: ICurve25519FieldElement; const ZSquared: TCryptoLibUInt32Array)
  : ICurve25519FieldElement;
var
  a4, W: ICurve25519FieldElement;
  LZSquared: TCryptoLibUInt32Array;
begin
  a4 := curve.A as ICurve25519FieldElement;
  if (z.IsOne) then
  begin
    result := a4;
    Exit;
  end;

  W := TCurve25519FieldElement.Create();
  LZSquared := ZSquared;
  if (LZSquared = Nil) then
  begin
    LZSquared := W.x;
    TCurve25519Field.Square(z.x, LZSquared);
  end;
  TCurve25519Field.Square(LZSquared, W.x);
  TCurve25519Field.Multiply(W.x, a4.x, W.x);
  result := W;
end;

function TCurve25519Point.GetJacobianModifiedW: ICurve25519FieldElement;
var
  zz: TCryptoLibGenericArray<IECFieldElement>;
  W: ICurve25519FieldElement;
begin
  zz := RawZCoords;
  W := zz[1] as ICurve25519FieldElement;
  if (W = Nil) then
  begin
    // NOTE: Rarely, TwicePlus will result in the need for a lazy W1 calculation here
    W := CalculateJacobianModifiedW(zz[0] as ICurve25519FieldElement, Nil);
    zz[1] := W;
  end;
  result := W;
end;

function TCurve25519Point.TwiceJacobianModified(calculateW: Boolean)
  : ICurve25519Point;
var
  x1, Y1, Z1, W1, x3, Y3, Z3, W3: ICurve25519FieldElement;
  c: UInt32;
  M, _2Y1, _2Y1Squared, S, _8T: TCryptoLibUInt32Array;
begin
  x1 := RawXCoord as ICurve25519FieldElement;
  Y1 := RawYCoord as ICurve25519FieldElement;
  Z1 := RawZCoords[0] as ICurve25519FieldElement;
  W1 := GetJacobianModifiedW();

  M := TNat256.Create();
  TCurve25519Field.Square(x1.x, M);
  c := TNat256.AddBothTo(M, M, M);
  c := c + TNat256.AddTo(W1.x, M);
  TCurve25519Field.Reduce27(c, M);

  _2Y1 := TNat256.Create();
  TCurve25519Field.Twice(Y1.x, _2Y1);

  _2Y1Squared := TNat256.Create();
  TCurve25519Field.Multiply(_2Y1, Y1.x, _2Y1Squared);

  S := TNat256.Create();
  TCurve25519Field.Multiply(_2Y1Squared, x1.x, S);
  TCurve25519Field.Twice(S, S);

  _8T := TNat256.Create();
  TCurve25519Field.Square(_2Y1Squared, _8T);
  TCurve25519Field.Twice(_8T, _8T);

  x3 := TCurve25519FieldElement.Create(_2Y1Squared);
  TCurve25519Field.Square(M, x3.x);
  TCurve25519Field.Subtract(x3.x, S, x3.x);
  TCurve25519Field.Subtract(x3.x, S, x3.x);

  Y3 := TCurve25519FieldElement.Create(S);
  TCurve25519Field.Subtract(S, x3.x, Y3.x);
  TCurve25519Field.Multiply(Y3.x, M, Y3.x);
  TCurve25519Field.Subtract(Y3.x, _8T, Y3.x);

  Z3 := TCurve25519FieldElement.Create(_2Y1);
  if (not(TNat256.IsOne(Z1.x))) then
  begin
    TCurve25519Field.Multiply(Z3.x, Z1.x, Z3.x);
  end;

  W3 := Nil;
  if (calculateW) then
  begin
    W3 := TCurve25519FieldElement.Create(_8T);
    TCurve25519Field.Multiply(W3.x, W1.x, W3.x);
    TCurve25519Field.Twice(W3.x, W3.x);
  end;

  result := TCurve25519Point.Create(curve, x3, Y3,
    TCryptoLibGenericArray<IECFieldElement>.Create(Z3, W3), IsCompressed);
end;

function TCurve25519Point.GetZCoord(index: Int32): IECFieldElement;
begin
  if (index = 1) then
  begin
    result := GetJacobianModifiedW();
    Exit;
  end;

  result := Inherited GetZCoord(index);
end;

function TCurve25519Point.Add(const b: IECPoint): IECPoint;
var
  LCurve: IECCurve;
  x1, Y1, Z1, x2, Y2, Z2, x3, Y3, Z3, W3: ICurve25519FieldElement;
  c: UInt32;
  tt1, t2, t3, t4, U2, S2, U1, S1, H, R, G, V, HSquared,
    Z3Squared: TCryptoLibUInt32Array;
  zs: TCryptoLibGenericArray<IECFieldElement>;
  Z1IsOne, Z2IsOne: Boolean;
begin
  if (IsInfinity) then
  begin
    result := b;
    Exit;
  end;
  if (b.IsInfinity) then
  begin
    result := Self;
    Exit;
  end;
  if ((Self as IECPoint) = b) then
  begin
    result := Twice();
    Exit;
  end;

  LCurve := curve;

  x1 := RawXCoord as ICurve25519FieldElement;
  Y1 := RawYCoord as ICurve25519FieldElement;
  Z1 := RawZCoords[0] as ICurve25519FieldElement;
  x2 := b.RawXCoord as ICurve25519FieldElement;
  Y2 := b.RawYCoord as ICurve25519FieldElement;
  Z2 := b.RawZCoords[0] as ICurve25519FieldElement;

  tt1 := TNat256.CreateExt();
  t2 := TNat256.Create();
  t3 := TNat256.Create();
  t4 := TNat256.Create();

  Z1IsOne := Z1.IsOne;
  if (Z1IsOne) then
  begin
    U2 := x2.x;
    S2 := Y2.x;
  end
  else
  begin
    S2 := t3;
    TCurve25519Field.Square(Z1.x, S2);

    U2 := t2;
    TCurve25519Field.Multiply(S2, x2.x, U2);

    TCurve25519Field.Multiply(S2, Z1.x, S2);
    TCurve25519Field.Multiply(S2, Y2.x, S2);
  end;

  Z2IsOne := Z2.IsOne;

  if (Z2IsOne) then
  begin
    U1 := x1.x;
    S1 := Y1.x;
  end
  else
  begin
    S1 := t4;
    TCurve25519Field.Square(Z2.x, S1);

    U1 := tt1;
    TCurve25519Field.Multiply(S1, x1.x, U1);

    TCurve25519Field.Multiply(S1, Z2.x, S1);
    TCurve25519Field.Multiply(S1, Y1.x, S1);
  end;

  H := TNat256.Create();
  TCurve25519Field.Subtract(U1, U2, H);

  R := t2;
  TCurve25519Field.Subtract(S1, S2, R);

  // Check if b = Self or b = -Self
  if (TNat256.IsZero(H)) then
  begin
    if (TNat256.IsZero(R)) then
    begin
      // this == b, i.e. this must be doubled
      result := Twice();
      Exit;
    end;

    // Self = -b, i.e. the result is the point at infinity
    result := LCurve.Infinity;
    Exit;
  end;

  HSquared := TNat256.Create();
  TCurve25519Field.Square(H, HSquared);

  G := TNat256.Create();
  TCurve25519Field.Multiply(HSquared, H, G);

  V := t3;
  TCurve25519Field.Multiply(HSquared, U1, V);

  TCurve25519Field.Negate(G, G);
  TNat256.Mul(S1, G, tt1);

  c := TNat256.AddBothTo(V, V, G);
  TCurve25519Field.Reduce27(c, G);

  x3 := TCurve25519FieldElement.Create(t4);
  TCurve25519Field.Square(R, x3.x);
  TCurve25519Field.Subtract(x3.x, G, x3.x);

  Y3 := TCurve25519FieldElement.Create(G);
  TCurve25519Field.Subtract(V, x3.x, Y3.x);
  TCurve25519Field.MultiplyAddToExt(Y3.x, R, tt1);
  TCurve25519Field.Reduce(tt1, Y3.x);

  Z3 := TCurve25519FieldElement.Create(H);
  if (not(Z1IsOne)) then
  begin
    TCurve25519Field.Multiply(Z3.x, Z1.x, Z3.x);
  end;
  if (not(Z2IsOne)) then
  begin
    TCurve25519Field.Multiply(Z3.x, Z2.x, Z3.x);
  end;

  if ((Z1IsOne) and (Z2IsOne)) then
  begin
    Z3Squared := HSquared;
  end
  else
  begin
    Z3Squared := Nil;
  end;

  // TODO If the result will only be used in a subsequent addition, we don't need W3
  W3 := CalculateJacobianModifiedW(Z3, Z3Squared);

  zs := TCryptoLibGenericArray<IECFieldElement>.Create(Z3, W3);

  result := TCurve25519Point.Create(LCurve, x3, Y3, zs, IsCompressed);
end;

constructor TCurve25519Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

constructor TCurve25519Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
  if ((x = Nil) <> (y = Nil)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SOneOfECFieldElementIsNil);
  end;
end;

constructor TCurve25519Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement);
begin
  Create(curve, x, y, false);
end;

function TCurve25519Point.Negate: IECPoint;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  result := TCurve25519Point.Create(curve, RawXCoord, RawYCoord.Negate(),
    RawZCoords, IsCompressed);
end;

function TCurve25519Point.ThreeTimes: IECPoint;
begin
  if ((IsInfinity) or (RawYCoord.IsZero)) then
  begin
    result := Self;
    Exit;
  end;

  result := TwiceJacobianModified(false).Add(Self as ICurve25519Point);
end;

function TCurve25519Point.Twice: IECPoint;
var
  LCurve: IECCurve;
  Y1: IECFieldElement;
begin
  if (IsInfinity) then
  begin
    result := Self;
    Exit;
  end;

  LCurve := curve;

  Y1 := RawYCoord;
  if (Y1.IsZero) then
  begin
    result := LCurve.Infinity;
    Exit;
  end;

  result := TwiceJacobianModified(true);
end;

function TCurve25519Point.TwicePlus(const b: IECPoint): IECPoint;
var
  Y1: IECFieldElement;
begin
  if ((Self as IECPoint) = b) then
  begin
    result := ThreeTimes();
    Exit;
  end;
  if (IsInfinity) then
  begin
    result := b;
    Exit;
  end;
  if (b.IsInfinity) then
  begin
    result := Twice();
    Exit;
  end;

  Y1 := RawYCoord;
  if (Y1.IsZero) then
  begin
    result := b;
    Exit;
  end;

  result := TwiceJacobianModified(false).Add(b);
end;

{ TCurve25519 }

constructor TCurve25519.Create;
begin
  Fq := TCurve25519FieldElement.Q;
  Inherited Create(Fq);
  Fm_infinity := TCurve25519Point.Create(Self as IECCurve, Nil, Nil);

  Fm_a := FromBigInteger(TBigInteger.Create(1,
    THex.decode
    ('2AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA984914A144')));
  Fm_b := FromBigInteger(TBigInteger.Create(1,
    THex.decode
    ('7B425ED097B425ED097B425ED097B425ED097B425ED097B4260B5E9C7710C864')));
  Fm_order := TBigInteger.Create(1,
    THex.decode
    ('1000000000000000000000000000000014DEF9DEA2F79CD65812631A5CF5D3ED'));
  Fm_cofactor := TBigInteger.ValueOf(8);
  Fm_coord := Curve25519_DEFAULT_COORDS;
end;

function TCurve25519.CloneCurve: IECCurve;
begin
  result := TCurve25519.Create();
end;

function TCurve25519.CreateCacheSafeLookupTable(const points
  : TCryptoLibGenericArray<IECPoint>; off, len: Int32): IECLookupTable;
var
  table: TCryptoLibUInt32Array;
  pos, i: Int32;
  P: IECPoint;
begin
  System.SetLength(table, len * CURVE25519_FE_INTS * 2);

  pos := 0;
  for i := 0 to System.Pred(len) do
  begin
    P := points[off + i];
    TNat256.Copy((P.RawXCoord as ICurve25519FieldElement).x, 0, table, pos);
    pos := pos + CURVE25519_FE_INTS;
    TNat256.Copy((P.RawYCoord as ICurve25519FieldElement).x, 0, table, pos);
    pos := pos + CURVE25519_FE_INTS;
  end;

  result := TCurve25519LookupTable.Create(Self as ICurve25519, table, len);
end;

function TCurve25519.CreateRawPoint(const x, y: IECFieldElement;
  withCompression: Boolean): IECPoint;
begin
  result := TCurve25519Point.Create(Self as IECCurve, x, y, withCompression);
end;

function TCurve25519.CreateRawPoint(const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean)
  : IECPoint;
begin
  result := TCurve25519Point.Create(Self as IECCurve, x, y, zs,
    withCompression);
end;

function TCurve25519.FromBigInteger(const x: TBigInteger): IECFieldElement;
begin
  result := TCurve25519FieldElement.Create(x);
end;

function TCurve25519.GetFieldSize: Int32;
begin
  result := Fq.BitLength;
end;

function TCurve25519.GetInfinity: IECPoint;
begin
  result := Fm_infinity;
end;

function TCurve25519.GetQ: TBigInteger;
begin
  result := Fq;
end;

function TCurve25519.SupportsCoordinateSystem(coord: Int32): Boolean;
begin
  case coord of
    TECCurveConstants.COORD_JACOBIAN_MODIFIED:
      result := true
  else
    result := false;
  end;
end;

{ TCurve25519.TCurve25519LookupTable }

constructor TCurve25519.TCurve25519LookupTable.Create(const outer: ICurve25519;
  const table: TCryptoLibUInt32Array; size: Int32);
begin
  Inherited Create();
  Fm_outer := outer;
  Fm_table := table;
  Fm_size := size;
end;

function TCurve25519.TCurve25519LookupTable.CreatePoint(const x,
  y: TCryptoLibUInt32Array): IECPoint;
var
  XFieldElement, YFieldElement: ICurve25519FieldElement;
  CURVE25519_AFFINE_ZS: TCryptoLibGenericArray<IECFieldElement>;
  C_a: TBigInteger;
begin
  C_a := TBigInteger.Create(1,
    THex.decode
    ('2AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA984914A144'));
  CURVE25519_AFFINE_ZS := TCryptoLibGenericArray<IECFieldElement>.Create
    (TCurve25519FieldElement.Create(TBigInteger.One) as ICurve25519FieldElement,
    TCurve25519FieldElement.Create(C_a) as ICurve25519FieldElement);

  XFieldElement := TCurve25519FieldElement.Create(x);
  YFieldElement := TCurve25519FieldElement.Create(y);
  result := Fm_outer.CreateRawPoint(XFieldElement, YFieldElement,
    CURVE25519_AFFINE_ZS, false);
end;

function TCurve25519.TCurve25519LookupTable.GetSize: Int32;
begin
  result := Fm_size;
end;

function TCurve25519.TCurve25519LookupTable.Lookup(index: Int32): IECPoint;
var
  x, y: TCryptoLibUInt32Array;
  pos, i, J: Int32;
  MASK: UInt32;
begin
  x := TNat256.Create();
  y := TNat256.Create();
  pos := 0;

  for i := 0 to System.Pred(Fm_size) do
  begin
    MASK := UInt32(TBits.Asr32((i xor index) - 1, 31));

    for J := 0 to System.Pred(CURVE25519_FE_INTS) do
    begin
      x[J] := x[J] xor (Fm_table[pos + J] and MASK);
      y[J] := y[J] xor (Fm_table[pos + CURVE25519_FE_INTS + J] and MASK);
    end;

    pos := pos + (CURVE25519_FE_INTS * 2);
  end;

  result := CreatePoint(x, y)
end;

function TCurve25519.TCurve25519LookupTable.LookupVar(index: Int32): IECPoint;
var
  x, y: TCryptoLibUInt32Array;
  pos, J: Int32;
begin
  x := TNat256.Create();
  y := TNat256.Create();
  pos := index * CURVE25519_FE_INTS * 2;

  for J := 0 to System.Pred(CURVE25519_FE_INTS) do
  begin
    x[J] := Fm_table[pos + J];
    y[J] := Fm_table[pos + CURVE25519_FE_INTS + J];
  end;

  result := CreatePoint(x, y)
end;

end.
