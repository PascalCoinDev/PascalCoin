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

unit ClpSecP521R1Custom;

{$I CryptoLib.inc}

interface

uses
  ClpNat,
  ClpMod,
  ClpEncoders,
  ClpBits,
  ClpNat512,
  ClpECC,
  ClpBigInteger,
  ClpArrayUtils,
  ClpIECC,
  ClpCryptoLibTypes,
  ClpECCurveConstants,
  ClpISecP521R1Custom;

resourcestring
  SInvalidValueForSecP521R1FieldElement =
    'Value Invalid for SecP521R1FieldElement "%s"';
  SOneOfECFieldElementIsNil = 'Exactly One of the Field Elements is Nil';

type
  // 2^521 - 1
  TSecP521R1Field = class sealed(TObject)

  strict private
  const
    P16 = UInt32($1FF);

    class var

      FP: TCryptoLibUInt32Array;

    class function GetP: TCryptoLibUInt32Array; static; inline;

    class procedure ImplMultiply(const x, y, zz: TCryptoLibUInt32Array);
      static; inline;
    class procedure ImplSquare(const x, zz: TCryptoLibUInt32Array);
      static; inline;

    class procedure Boot(); static;
    class constructor SecP521R1Field();

  public
    class procedure Add(const x, y, z: TCryptoLibUInt32Array); static; inline;
    class procedure AddOne(const x, z: TCryptoLibUInt32Array); static; inline;
    class function FromBigInteger(const x: TBigInteger): TCryptoLibUInt32Array;
      static; inline;
    class procedure Half(const x, z: TCryptoLibUInt32Array); static; inline;
    class procedure Multiply(const x, y, z: TCryptoLibUInt32Array);
      static; inline;
    class procedure Negate(const x, z: TCryptoLibUInt32Array); static; inline;
    class procedure Reduce(const xx, z: TCryptoLibUInt32Array); static; inline;
    class procedure Reduce23(const z: TCryptoLibUInt32Array); static; inline;
    class procedure Square(const x, z: TCryptoLibUInt32Array); static; inline;
    class procedure SquareN(const x: TCryptoLibUInt32Array; n: Int32;
      const z: TCryptoLibUInt32Array); static; inline;
    class procedure Subtract(const x, y, z: TCryptoLibUInt32Array);
      static; inline;
    class procedure Twice(const x, z: TCryptoLibUInt32Array); static; inline;

    class property P: TCryptoLibUInt32Array read GetP;
  end;

type
  TSecP521R1FieldElement = class(TAbstractFpFieldElement,
    ISecP521R1FieldElement)

  strict private

    function Equals(const other: ISecP521R1FieldElement): Boolean;
      reintroduce; overload;

    class function GetQ: TBigInteger; static; inline;

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
  TSecP521R1Point = class sealed(TAbstractFpPoint, ISecP521R1Point)

  strict protected
    function Detach(): IECPoint; override;

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

  end;

type
  TSecP521R1Curve = class sealed(TAbstractFpCurve, ISecP521R1Curve)

  strict private

  type
    TSecP521R1LookupTable = class sealed(TInterfacedObject,
      ISecP521R1LookupTable, IECLookupTable)

    strict private
    var
      Fm_outer: ISecP521R1Curve;
      Fm_table: TCryptoLibUInt32Array;
      Fm_size: Int32;

      function GetSize: Int32; virtual;

    public

      constructor Create(const outer: ISecP521R1Curve;
        const table: TCryptoLibUInt32Array; size: Int32);

      function Lookup(index: Int32): IECPoint; virtual;

      property size: Int32 read GetSize;

    end;

  const
    SECP521R1_DEFAULT_COORDS = Int32(TECCurveConstants.COORD_JACOBIAN);
    SECP521R1_FE_INTS = Int32(17);

  var
    Fq: TBigInteger;

    class function GetSecP521R1Curve_Q: TBigInteger; static; inline;

  strict protected
  var
    Fm_infinity: ISecP521R1Point;

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

    class property SecP521R1Curve_Q: TBigInteger read GetSecP521R1Curve_Q;

  end;

implementation

{ TSecP521R1Field }

class constructor TSecP521R1Field.SecP521R1Field;
begin
  TSecP521R1Field.Boot;
end;

class function TSecP521R1Field.GetP: TCryptoLibUInt32Array;
begin
  result := FP;
end;

class procedure TSecP521R1Field.Add(const x, y, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat.Add(16, x, y, z) + x[16] + y[16];
  if ((c > P16) or ((c = P16) and (TNat.Eq(16, z, FP)))) then
  begin
    c := c + (TNat.Inc(16, z));
    c := c and P16;
  end;
  z[16] := c;
end;

class procedure TSecP521R1Field.AddOne(const x, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat.Inc(16, x, z) + x[16];
  if ((c > P16) or ((c = P16) and (TNat.Eq(16, z, FP)))) then
  begin
    c := c + TNat.Inc(16, z);
    c := c and P16;
  end;
  z[16] := c;
end;

class procedure TSecP521R1Field.Boot;
begin
  FP := TCryptoLibUInt32Array.Create($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
    $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
    $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $1FF);
end;

class function TSecP521R1Field.FromBigInteger(const x: TBigInteger)
  : TCryptoLibUInt32Array;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.FromBigInteger(521, x);
  if (TNat.Eq(17, z, FP)) then
  begin
    TNat.Zero(17, z);
  end;
  result := z;
end;

class procedure TSecP521R1Field.Half(const x, z: TCryptoLibUInt32Array);
var
  x16, c: UInt32;
begin
  x16 := x[16];
  c := TNat.ShiftDownBit(16, x, x16, z);
  z[16] := (x16 shr 1) or (c shr 23);
end;

class procedure TSecP521R1Field.ImplMultiply(const x, y,
  zz: TCryptoLibUInt32Array);
var
  x16, y16: UInt32;
begin
  TNat512.Mul(x, y, zz);
  x16 := x[16];
  y16 := y[16];
  zz[32] := TNat.Mul31BothAdd(16, x16, y, y16, x, zz, 16) + (x16 * y16);
end;

class procedure TSecP521R1Field.ImplSquare(const x, zz: TCryptoLibUInt32Array);
var
  x16: UInt32;
begin
  TNat512.Square(x, zz);
  x16 := x[16];
  zz[32] := TNat.MulWordAddTo(16, (x16 shl 1), x, 0, zz, 16) + (x16 * x16);
end;

class procedure TSecP521R1Field.Reduce23(const z: TCryptoLibUInt32Array);
var
  z16, c: UInt32;
begin
  z16 := z[16];
  c := TNat.AddWordTo(16, (z16 shr 9), z) + (z16 and P16);
  if ((c > P16) or ((c = P16) and (TNat.Eq(16, z, FP)))) then
  begin
    c := c + (TNat.Inc(16, z));
    c := c and P16;
  end;
  z[16] := c;
end;

class procedure TSecP521R1Field.Reduce(const xx, z: TCryptoLibUInt32Array);
var
  xx32, c: UInt32;
begin
{$IFDEF DEBUG}
  System.Assert((xx[32] shr 18) = 0);
{$ENDIF DEBUG}
  xx32 := xx[32];
  c := TNat.ShiftDownBits(16, xx, 16, 9, xx32, z, 0) shr 23;
  c := c + (xx32 shr 9);
  c := c + (TNat.AddTo(16, xx, z));
  if ((c > P16) or ((c = P16) and (TNat.Eq(16, z, FP)))) then
  begin
    c := c + (TNat.Inc(16, z));
    c := c and P16;
  end;
  z[16] := c;
end;

class procedure TSecP521R1Field.Multiply(const x, y, z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
  tt := TNat.Create(33);
  ImplMultiply(x, y, tt);
  Reduce(tt, z);
end;

class procedure TSecP521R1Field.Negate(const x, z: TCryptoLibUInt32Array);
begin
  if (TNat.IsZero(17, x)) then
  begin
    TNat.Zero(17, z);
  end
  else
  begin
    TNat.Sub(17, FP, x, z);
  end;
end;

class procedure TSecP521R1Field.Square(const x, z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
  tt := TNat.Create(33);
  ImplSquare(x, tt);
  Reduce(tt, z);
end;

class procedure TSecP521R1Field.SquareN(const x: TCryptoLibUInt32Array;
  n: Int32; const z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
{$IFDEF DEBUG}
  System.Assert(n > 0);
{$ENDIF DEBUG}
  tt := TNat.Create(33);
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

class procedure TSecP521R1Field.Subtract(const x, y, z: TCryptoLibUInt32Array);
var
  c: Int32;
begin
  c := TNat.Sub(16, x, y, z) + Int32(x[16] - y[16]);
  if (c < 0) then
  begin
    c := c + TNat.Dec(16, z);
    c := c and P16;
  end;
  z[16] := UInt32(c);
end;

class procedure TSecP521R1Field.Twice(const x, z: TCryptoLibUInt32Array);
var
  x16, c: UInt32;
begin
  x16 := x[16];
  c := TNat.ShiftUpBit(16, x, x16 shl 23, z) or (x16 shl 1);
  z[16] := c and P16;
end;

{ TSecP521R1FieldElement }

class function TSecP521R1FieldElement.GetQ: TBigInteger;
begin
  result := TSecP521R1Curve.SecP521R1Curve_Q;
end;

function TSecP521R1FieldElement.GetX: TCryptoLibUInt32Array;
begin
  result := Fx;
end;

constructor TSecP521R1FieldElement.Create;
begin
  Inherited Create();
  Fx := TNat.Create(17);
end;

constructor TSecP521R1FieldElement.Create(const x: TBigInteger);
begin
  if ((not(x.IsInitialized)) or (x.SignValue < 0) or (x.CompareTo(Q) >= 0)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt
      (@SInvalidValueForSecP521R1FieldElement, ['x']);
  end;
  Inherited Create();
  Fx := TSecP521R1Field.FromBigInteger(x);
end;

constructor TSecP521R1FieldElement.Create(const x: TCryptoLibUInt32Array);
begin
  Inherited Create();
  Fx := x;
end;

function TSecP521R1FieldElement.GetFieldName: string;
begin
  result := 'SecP521R1Field';
end;

function TSecP521R1FieldElement.GetFieldSize: Int32;
begin
  result := Q.BitLength;
end;

function TSecP521R1FieldElement.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := Q.GetHashCode() xor TArrayUtils.GetArrayHashCode(Fx, 0, 17);
end;

function TSecP521R1FieldElement.GetIsOne: Boolean;
begin
  result := TNat.IsOne(17, Fx);
end;

function TSecP521R1FieldElement.GetIsZero: Boolean;
begin
  result := TNat.IsZero(17, Fx);
end;

function TSecP521R1FieldElement.Sqrt: IECFieldElement;
var
  x1, t1, t2: TCryptoLibUInt32Array;
begin
  // Raise this element to the exponent 2^519
  x1 := Fx;
  if ((TNat.IsZero(17, x1)) or (TNat.IsOne(17, x1))) then
  begin
    result := Self as IECFieldElement;
    Exit;
  end;

  t1 := TNat.Create(17);
  t2 := TNat.Create(17);

  TSecP521R1Field.SquareN(x1, 519, t1);
  TSecP521R1Field.Square(t1, t2);

  if TNat.Eq(17, x1, t2) then
  begin
    result := TSecP521R1FieldElement.Create(t1);
  end
  else
  begin
    result := Nil;
  end;
end;

function TSecP521R1FieldElement.TestBitZero: Boolean;
begin
  result := TNat.GetBit(Fx, 0) = 1;
end;

function TSecP521R1FieldElement.ToBigInteger: TBigInteger;
begin
  result := TNat.ToBigInteger(17, Fx);
end;

function TSecP521R1FieldElement.Add(const b: IECFieldElement): IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(17);
  TSecP521R1Field.Add(Fx, (b as ISecP521R1FieldElement).x, z);
  result := TSecP521R1FieldElement.Create(z);
end;

function TSecP521R1FieldElement.AddOne: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(17);
  TSecP521R1Field.AddOne(Fx, z);
  result := TSecP521R1FieldElement.Create(z);
end;

function TSecP521R1FieldElement.Subtract(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(17);
  TSecP521R1Field.Subtract(Fx, (b as ISecP521R1FieldElement).x, z);
  result := TSecP521R1FieldElement.Create(z);
end;

function TSecP521R1FieldElement.Multiply(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(17);
  TSecP521R1Field.Multiply(Fx, (b as ISecP521R1FieldElement).x, z);
  result := TSecP521R1FieldElement.Create(z);
end;

function TSecP521R1FieldElement.Divide(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(17);
  TMod.Invert(TSecP521R1Field.P, (b as ISecP521R1FieldElement).x, z);
  TSecP521R1Field.Multiply(z, Fx, z);
  result := TSecP521R1FieldElement.Create(z);
end;

function TSecP521R1FieldElement.Negate: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(17);
  TSecP521R1Field.Negate(Fx, z);
  result := TSecP521R1FieldElement.Create(z);
end;

function TSecP521R1FieldElement.Square: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(17);
  TSecP521R1Field.Square(Fx, z);
  result := TSecP521R1FieldElement.Create(z);
end;

function TSecP521R1FieldElement.Invert: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(17);
  TMod.Invert(TSecP521R1Field.P, Fx, z);
  result := TSecP521R1FieldElement.Create(z);
end;

function TSecP521R1FieldElement.Equals(const other
  : ISecP521R1FieldElement): Boolean;
begin
  if ((Self as ISecP521R1FieldElement) = other) then
  begin
    result := true;
    Exit;
  end;
  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := TNat.Eq(17, Fx, other.x);
end;

function TSecP521R1FieldElement.Equals(const other: IECFieldElement): Boolean;
begin
  result := Equals(other as ISecP521R1FieldElement);
end;

{ TSecP521R1Point }

constructor TSecP521R1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement);
begin
  Create(curve, x, y, false);
end;

constructor TSecP521R1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
  if ((x = Nil) <> (y = Nil)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SOneOfECFieldElementIsNil);
  end;
end;

constructor TSecP521R1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

function TSecP521R1Point.Add(const b: IECPoint): IECPoint;
var
  Lcurve: IECCurve;
  x1, Y1, X2, Y2, Z1, Z2, X3, Y3, Z3: ISecP521R1FieldElement;
  t1, t2, t3, t4, U2, S2, U1, S1, H, R, HSquared, G, V: TCryptoLibUInt32Array;
  Z1IsOne, Z2IsOne: Boolean;
  zs: TCryptoLibGenericArray<IECFieldElement>;
begin
  if (IsInfinity) then
  begin
    result := b;
    Exit;
  end;
  if (b.IsInfinity) then
  begin
    result := Self as IECPoint;
    Exit;
  end;
  if ((Self as IECPoint) = b) then
  begin
    result := Twice();
    Exit;
  end;

  Lcurve := curve;

  x1 := RawXCoord as ISecP521R1FieldElement;
  Y1 := RawYCoord as ISecP521R1FieldElement;
  X2 := b.RawXCoord as ISecP521R1FieldElement;
  Y2 := b.RawYCoord as ISecP521R1FieldElement;

  Z1 := RawZCoords[0] as ISecP521R1FieldElement;
  Z2 := b.RawZCoords[0] as ISecP521R1FieldElement;

  t1 := TNat.Create(17);
  t2 := TNat.Create(17);
  t3 := TNat.Create(17);
  t4 := TNat.Create(17);

  Z1IsOne := Z1.IsOne;

  if (Z1IsOne) then
  begin
    U2 := X2.x;
    S2 := Y2.x;
  end
  else
  begin
    S2 := t3;
    TSecP521R1Field.Square(Z1.x, S2);

    U2 := t2;
    TSecP521R1Field.Multiply(S2, X2.x, U2);

    TSecP521R1Field.Multiply(S2, Z1.x, S2);
    TSecP521R1Field.Multiply(S2, Y2.x, S2);
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
    TSecP521R1Field.Square(Z2.x, S1);

    U1 := t1;
    TSecP521R1Field.Multiply(S1, x1.x, U1);

    TSecP521R1Field.Multiply(S1, Z2.x, S1);
    TSecP521R1Field.Multiply(S1, Y1.x, S1);
  end;

  H := TNat.Create(17);
  TSecP521R1Field.Subtract(U1, U2, H);

  R := t2;
  TSecP521R1Field.Subtract(S1, S2, R);

  // Check if b = Self or b = -Self
  if (TNat.IsZero(17, H)) then
  begin
    if (TNat.IsZero(17, R)) then
    begin
      // Self = b, i.e. Self must be doubled
      result := Twice();
      Exit;
    end;

    // Self = -b, i.e. the result is the point at infinity
    result := Lcurve.Infinity;
    Exit;
  end;

  HSquared := t3;
  TSecP521R1Field.Square(H, HSquared);

  G := TNat.Create(17);
  TSecP521R1Field.Multiply(HSquared, H, G);

  V := t3;
  TSecP521R1Field.Multiply(HSquared, U1, V);

  TSecP521R1Field.Multiply(S1, G, t1);

  X3 := TSecP521R1FieldElement.Create(t4);
  TSecP521R1Field.Square(R, X3.x);
  TSecP521R1Field.Add(X3.x, G, X3.x);
  TSecP521R1Field.Subtract(X3.x, V, X3.x);
  TSecP521R1Field.Subtract(X3.x, V, X3.x);

  Y3 := TSecP521R1FieldElement.Create(G);
  TSecP521R1Field.Subtract(V, X3.x, Y3.x);
  TSecP521R1Field.Multiply(Y3.x, R, t2);
  TSecP521R1Field.Subtract(t2, t1, Y3.x);

  Z3 := TSecP521R1FieldElement.Create(H);
  if (not(Z1IsOne)) then
  begin
    TSecP521R1Field.Multiply(Z3.x, Z1.x, Z3.x);
  end;
  if (not(Z2IsOne)) then
  begin
    TSecP521R1Field.Multiply(Z3.x, Z2.x, Z3.x);
  end;

  zs := TCryptoLibGenericArray<IECFieldElement>.Create(Z3);

  result := TSecP521R1Point.Create(Lcurve, X3, Y3, zs, IsCompressed)
    as IECPoint;
end;

function TSecP521R1Point.Detach: IECPoint;
begin
  result := TSecP521R1Point.Create(Nil, AffineXCoord, AffineYCoord) as IECPoint;
end;

function TSecP521R1Point.Negate: IECPoint;
begin
  if (IsInfinity) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  result := TSecP521R1Point.Create(curve, RawXCoord, RawYCoord.Negate(),
    RawZCoords, IsCompressed) as IECPoint;
end;

function TSecP521R1Point.ThreeTimes: IECPoint;
begin
  if ((IsInfinity) or (RawYCoord.IsZero)) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  // NOTE: Be careful about recursions between TwicePlus and ThreeTimes
  result := Twice().Add(Self as IECPoint);
end;

function TSecP521R1Point.Twice: IECPoint;
var
  Lcurve: IECCurve;
  Y1, x1, Z1, X3, Y3, Z3: ISecP521R1FieldElement;
  Y1Squared, Z1Squared, T, M, S, t1, t2: TCryptoLibUInt32Array;
  Z1IsOne: Boolean;
begin

  if (IsInfinity) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  Lcurve := curve;

  Y1 := RawYCoord as ISecP521R1FieldElement;
  if (Y1.IsZero) then
  begin
    result := Lcurve.Infinity;
    Exit;
  end;

  x1 := RawXCoord as ISecP521R1FieldElement;
  Z1 := RawZCoords[0] as ISecP521R1FieldElement;

  t1 := TNat.Create(17);
  t2 := TNat.Create(17);
  Y1Squared := TNat.Create(17);
  TSecP521R1Field.Square(Y1.x, Y1Squared);

  T := TNat.Create(17);
  TSecP521R1Field.Square(Y1Squared, T);

  Z1IsOne := Z1.IsOne;

  Z1Squared := Z1.x;
  if (not(Z1IsOne)) then
  begin
    Z1Squared := t2;
    TSecP521R1Field.Square(Z1.x, Z1Squared);
  end;

  TSecP521R1Field.Subtract(x1.x, Z1Squared, t1);

  M := t2;
  TSecP521R1Field.Add(x1.x, Z1Squared, M);
  TSecP521R1Field.Multiply(M, t1, M);
  TNat.AddBothTo(17, M, M, M);
  TSecP521R1Field.Reduce23(M);

  S := Y1Squared;
  TSecP521R1Field.Multiply(Y1Squared, x1.x, S);
  TNat.ShiftUpBits(17, S, 2, 0);
  TSecP521R1Field.Reduce23(S);

  TNat.ShiftUpBits(17, T, 3, 0, t1);
  TSecP521R1Field.Reduce23(t1);

  X3 := TSecP521R1FieldElement.Create(T);
  TSecP521R1Field.Square(M, X3.x);
  TSecP521R1Field.Subtract(X3.x, S, X3.x);
  TSecP521R1Field.Subtract(X3.x, S, X3.x);

  Y3 := TSecP521R1FieldElement.Create(S);
  TSecP521R1Field.Subtract(S, X3.x, Y3.x);
  TSecP521R1Field.Multiply(Y3.x, M, Y3.x);
  TSecP521R1Field.Subtract(Y3.x, t1, Y3.x);

  Z3 := TSecP521R1FieldElement.Create(M);
  TSecP521R1Field.Twice(Y1.x, Z3.x);
  if (not(Z1IsOne)) then
  begin
    TSecP521R1Field.Multiply(Z3.x, Z1.x, Z3.x);
  end;

  result := TSecP521R1Point.Create(Lcurve, X3, Y3,
    TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed)
    as IECPoint;
end;

function TSecP521R1Point.TwicePlus(const b: IECPoint): IECPoint;
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

  result := Twice().Add(b);
end;

{ TSecP521R1Curve }

class function TSecP521R1Curve.GetSecP521R1Curve_Q: TBigInteger;
begin
  result := TBigInteger.Create(1,
    THex.Decode
    ('01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF')
    );
end;

constructor TSecP521R1Curve.Create;
begin
  Fq := SecP521R1Curve_Q;
  Inherited Create(Fq);
  Fm_infinity := TSecP521R1Point.Create(Self as IECCurve, Nil, Nil);
  Fm_a := FromBigInteger(TBigInteger.Create(1,
    THex.Decode
    ('01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC'))
    );
  Fm_b := FromBigInteger(TBigInteger.Create(1,
    THex.Decode
    ('0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00'))
    );
  Fm_order := TBigInteger.Create(1,
    THex.Decode
    ('01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E91386409')
    );
  Fm_cofactor := TBigInteger.One;
  Fm_coord := SECP521R1_DEFAULT_COORDS;
end;

function TSecP521R1Curve.CloneCurve: IECCurve;
begin
  result := TSecP521R1Curve.Create();
end;

function TSecP521R1Curve.CreateCacheSafeLookupTable(const points
  : TCryptoLibGenericArray<IECPoint>; off, len: Int32): IECLookupTable;
var
  table: TCryptoLibUInt32Array;
  pos, i: Int32;
  P: IECPoint;
begin
  System.SetLength(table, len * SECP521R1_FE_INTS * 2);

  pos := 0;
  for i := 0 to System.Pred(len) do
  begin
    P := points[off + i];
    TNat.Copy(SECP521R1_FE_INTS, (P.RawXCoord as ISecP521R1FieldElement).x, 0,
      table, pos);
    pos := pos + SECP521R1_FE_INTS;
    TNat.Copy(SECP521R1_FE_INTS, (P.RawYCoord as ISecP521R1FieldElement).x, 0,
      table, pos);
    pos := pos + SECP521R1_FE_INTS;
  end;

  result := TSecP521R1LookupTable.Create(Self as ISecP521R1Curve, table, len);
end;

function TSecP521R1Curve.CreateRawPoint(const x, y: IECFieldElement;
  withCompression: Boolean): IECPoint;
begin
  result := TSecP521R1Point.Create(Self as IECCurve, x, y, withCompression);
end;

function TSecP521R1Curve.CreateRawPoint(const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean)
  : IECPoint;
begin
  result := TSecP521R1Point.Create(Self as IECCurve, x, y, zs, withCompression);
end;

function TSecP521R1Curve.FromBigInteger(const x: TBigInteger): IECFieldElement;
begin
  result := TSecP521R1FieldElement.Create(x);
end;

function TSecP521R1Curve.GetFieldSize: Int32;
begin
  result := Fq.BitLength;
end;

function TSecP521R1Curve.GetInfinity: IECPoint;
begin
  result := Fm_infinity;
end;

function TSecP521R1Curve.GetQ: TBigInteger;
begin
  result := Fq;
end;

function TSecP521R1Curve.SupportsCoordinateSystem(coord: Int32): Boolean;
begin
  case coord of
    TECCurveConstants.COORD_JACOBIAN:
      result := true
  else
    result := false;
  end;
end;

{ TSecP521R1Curve.TSecP521R1LookupTable }

constructor TSecP521R1Curve.TSecP521R1LookupTable.Create
  (const outer: ISecP521R1Curve; const table: TCryptoLibUInt32Array;
  size: Int32);
begin
  Inherited Create();
  Fm_outer := outer;
  Fm_table := table;
  Fm_size := size;
end;

function TSecP521R1Curve.TSecP521R1LookupTable.GetSize: Int32;
begin
  result := Fm_size;
end;

function TSecP521R1Curve.TSecP521R1LookupTable.Lookup(index: Int32): IECPoint;
var
  x, y: TCryptoLibUInt32Array;
  pos, i, J: Int32;
  MASK: UInt32;
begin
  x := TNat.Create(SECP521R1_FE_INTS);
  y := TNat.Create(SECP521R1_FE_INTS);
  pos := 0;

  for i := 0 to System.Pred(Fm_size) do
  begin
    MASK := UInt32(TBits.Asr32((i xor index) - 1, 31));

    for J := 0 to System.Pred(SECP521R1_FE_INTS) do
    begin
      x[J] := x[J] xor (Fm_table[pos + J] and MASK);
      y[J] := y[J] xor (Fm_table[pos + SECP521R1_FE_INTS + J] and MASK);
    end;

    pos := pos + (SECP521R1_FE_INTS * 2);
  end;

  result := Fm_outer.CreateRawPoint(TSecP521R1FieldElement.Create(x)
    as ISecP521R1FieldElement, TSecP521R1FieldElement.Create(y)
    as ISecP521R1FieldElement, false);
end;

end.
