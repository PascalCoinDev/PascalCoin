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

unit ClpSecP256K1Custom;

{$I CryptoLib.inc}

interface

uses
  ClpMod,
  ClpEncoders,
  ClpNat,
  ClpBits,
  ClpNat256,
  ClpECC,
  ClpBigInteger,
  ClpArrayUtils,
  ClpCryptoLibTypes,
  ClpECCurveConstants,
  ClpIECC,
  ClpISecP256K1Custom;

resourcestring
  SInvalidValueForSecP256K1FieldElement =
    'Value Invalid for SecP256K1FieldElement "%s"';
  SOneOfECFieldElementIsNil = 'Exactly One of the Field Elements is Nil';

type
  // 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1
  TSecP256K1Field = class sealed(TObject)

  strict private
  const
    P7 = UInt32($FFFFFFFF);
    PExt15 = UInt32($FFFFFFFF);
    PInv33 = UInt32($3D1);

    class var

      FP, FPExt, FPExtInv: TCryptoLibUInt32Array;

    class function GetP: TCryptoLibUInt32Array; static; inline;

    class procedure Boot(); static;
    class constructor SecP256K1Field();

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
    class procedure Reduce32(x: UInt32; const z: TCryptoLibUInt32Array);
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
  TSecP256K1FieldElement = class(TAbstractFpFieldElement,
    ISecP256K1FieldElement)

  strict private

    function Equals(const other: ISecP256K1FieldElement): Boolean;
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
  TSecP256K1Point = class sealed(TAbstractFpPoint, ISecP256K1Point)

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
  TSecP256K1Curve = class sealed(TAbstractFpCurve, ISecP256K1Curve)

  strict private

  type
    TSecP256K1LookupTable = class sealed(TInterfacedObject,
      ISecP256K1LookupTable, IECLookupTable)

    strict private
    var
      Fm_outer: ISecP256K1Curve;
      Fm_table: TCryptoLibUInt32Array;
      Fm_size: Int32;

      function GetSize: Int32; virtual;

    public

      constructor Create(const outer: ISecP256K1Curve;
        const table: TCryptoLibUInt32Array; size: Int32);

      function Lookup(index: Int32): IECPoint; virtual;

      property size: Int32 read GetSize;

    end;

  const
    SECP256K1_DEFAULT_COORDS = Int32(TECCurveConstants.COORD_JACOBIAN);
    SECP256K1_FE_INTS = Int32(8);

  var
    Fq: TBigInteger;

    class function GetSecP256K1Curve_Q: TBigInteger; static; inline;

  strict protected
  var
    Fm_infinity: ISecP256K1Point;

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

    class property SecP256K1Curve_Q: TBigInteger read GetSecP256K1Curve_Q;

  end;

implementation

{ TSecP256K1Field }

class constructor TSecP256K1Field.SecP256K1Field;
begin
  TSecP256K1Field.Boot;
end;

class function TSecP256K1Field.GetP: TCryptoLibUInt32Array;
begin
  result := FP;
end;

class procedure TSecP256K1Field.Add(const x, y, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat256.Add(x, y, z);
  if ((c <> 0) or ((z[7] = P7) and (TNat256.Gte(z, FP)))) then
  begin
    TNat.Add33To(8, PInv33, z);
  end;
end;

class procedure TSecP256K1Field.AddExt(const xx, yy, zz: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat.Add(16, xx, yy, zz);
  if ((c <> 0) or ((zz[15] = PExt15) and (TNat.Gte(16, zz, FPExt)))) then
  begin
    if (TNat.AddTo(System.Length(FPExtInv), FPExtInv, zz) <> 0) then
    begin
      TNat.IncAt(16, zz, System.Length(FPExtInv));
    end;
  end;
end;

class procedure TSecP256K1Field.AddOne(const x, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat.Inc(8, x, z);
  if ((c <> 0) or ((z[7] = P7) and (TNat256.Gte(z, FP)))) then
  begin
    TNat.Add33To(8, PInv33, z);
  end;
end;

class procedure TSecP256K1Field.Boot;
begin
  FP := TCryptoLibUInt32Array.Create($FFFFFC2F, $FFFFFFFE, $FFFFFFFF, $FFFFFFFF,
    $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF);
  FPExt := TCryptoLibUInt32Array.Create($000E90A1, $000007A2, $00000001,
    $00000000, $00000000, $00000000, $00000000, $00000000, $FFFFF85E, $FFFFFFFD,
    $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF);
  FPExtInv := TCryptoLibUInt32Array.Create($FFF16F5F, $FFFFF85D, $FFFFFFFE,
    $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $000007A1,
    $00000002);
end;

class function TSecP256K1Field.FromBigInteger(const x: TBigInteger)
  : TCryptoLibUInt32Array;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.FromBigInteger(x);
  if ((z[7] = P7) and (TNat256.Gte(z, FP))) then
  begin
    TNat256.SubFrom(FP, z);
  end;
  result := z;
end;

class procedure TSecP256K1Field.Half(const x, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  if ((x[0] and 1) = 0) then
  begin
    TNat.ShiftDownBit(8, x, 0, z);
  end
  else
  begin
    c := TNat256.Add(x, FP, z);
    TNat.ShiftDownBit(8, z, c);
  end;
end;

class procedure TSecP256K1Field.Reduce(const xx, z: TCryptoLibUInt32Array);
var
  cc: UInt64;
  c: UInt32;
begin
  cc := TNat256.Mul33Add(PInv33, xx, 8, xx, 0, z, 0);
  c := TNat256.Mul33DWordAdd(PInv33, cc, z, 0);
{$IFDEF DEBUG}
  System.Assert((c = 0) or (c = 1));
{$ENDIF DEBUG}
  if ((c <> 0) or ((z[7] = P7) and (TNat256.Gte(z, FP)))) then
  begin
    TNat.Add33To(8, PInv33, z);
  end;
end;

class procedure TSecP256K1Field.Multiply(const x, y, z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
  tt := TNat256.CreateExt();
  TNat256.Mul(x, y, tt);
  Reduce(tt, z);
end;

class procedure TSecP256K1Field.MultiplyAddToExt(const x, y,
  zz: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat256.MulAddTo(x, y, zz);
  if ((c <> 0) or ((zz[15] = PExt15) and (TNat.Gte(16, zz, FPExt)))) then
  begin
    if (TNat.AddTo(System.Length(FPExtInv), FPExtInv, zz) <> 0) then
    begin
      TNat.IncAt(16, zz, System.Length(FPExtInv));
    end;
  end;
end;

class procedure TSecP256K1Field.Negate(const x, z: TCryptoLibUInt32Array);
begin
  if (TNat256.IsZero(x)) then
  begin
    TNat256.Zero(z);
  end
  else
  begin
    TNat256.Sub(FP, x, z);
  end;
end;

class procedure TSecP256K1Field.Reduce32(x: UInt32;
  const z: TCryptoLibUInt32Array);
begin
  if (((x <> 0) and (TNat256.Mul33WordAdd(PInv33, x, z, 0) <> 0)) or
    ((z[7] = P7) and (TNat256.Gte(z, FP)))) then
  begin
    TNat.Add33To(8, PInv33, z);
  end;
end;

class procedure TSecP256K1Field.Square(const x, z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
  tt := TNat256.CreateExt();
  TNat256.Square(x, tt);
  Reduce(tt, z);
end;

class procedure TSecP256K1Field.SquareN(const x: TCryptoLibUInt32Array;
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

class procedure TSecP256K1Field.Subtract(const x, y, z: TCryptoLibUInt32Array);
var
  c: Int32;
begin
  c := TNat256.Sub(x, y, z);
  if (c <> 0) then
  begin
    TNat.Sub33From(8, PInv33, z);
  end;
end;

class procedure TSecP256K1Field.SubtractExt(const xx, yy,
  zz: TCryptoLibUInt32Array);
var
  c: Int32;
begin
  c := TNat.Sub(16, xx, yy, zz);
  if (c <> 0) then
  begin
    if (TNat.SubFrom(System.Length(FPExtInv), FPExtInv, zz) <> 0) then
    begin
      TNat.DecAt(16, zz, System.Length(FPExtInv));
    end;
  end;
end;

class procedure TSecP256K1Field.Twice(const x, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat.ShiftUpBit(8, x, 0, z);
  if ((c <> 0) or ((z[7] = P7) and (TNat256.Gte(z, FP)))) then
  begin
    TNat.Add33To(8, PInv33, z);
  end;
end;

{ TSecP256K1FieldElement }

class function TSecP256K1FieldElement.GetQ: TBigInteger;
begin
  result := TSecP256K1Curve.SecP256K1Curve_Q;
end;

function TSecP256K1FieldElement.GetX: TCryptoLibUInt32Array;
begin
  result := Fx;
end;

constructor TSecP256K1FieldElement.Create;
begin
  Inherited Create();
  Fx := TNat256.Create();
end;

constructor TSecP256K1FieldElement.Create(const x: TBigInteger);
begin
  if ((not(x.IsInitialized)) or (x.SignValue < 0) or (x.CompareTo(Q) >= 0)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt
      (@SInvalidValueForSecP256K1FieldElement, ['x']);
  end;
  Inherited Create();
  Fx := TSecP256K1Field.FromBigInteger(x);
end;

constructor TSecP256K1FieldElement.Create(const x: TCryptoLibUInt32Array);
begin
  Inherited Create();
  Fx := x;
end;

function TSecP256K1FieldElement.GetFieldName: string;
begin
  result := 'SecP256K1Field';
end;

function TSecP256K1FieldElement.GetFieldSize: Int32;
begin
  result := Q.BitLength;
end;

function TSecP256K1FieldElement.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := Q.GetHashCode() xor TArrayUtils.GetArrayHashCode(Fx, 0, 8);
end;

function TSecP256K1FieldElement.GetIsOne: Boolean;
begin
  result := TNat256.IsOne(Fx);
end;

function TSecP256K1FieldElement.GetIsZero: Boolean;
begin
  result := TNat256.IsZero(Fx);
end;

function TSecP256K1FieldElement.Invert: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TMod.Invert(TSecP256K1Field.P, Fx, z);
  result := TSecP256K1FieldElement.Create(z);
end;

function TSecP256K1FieldElement.Multiply(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TSecP256K1Field.Multiply(Fx, (b as ISecP256K1FieldElement).x, z);
  result := TSecP256K1FieldElement.Create(z);
end;

function TSecP256K1FieldElement.Negate: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TSecP256K1Field.Negate(Fx, z);
  result := TSecP256K1FieldElement.Create(z);
end;

function TSecP256K1FieldElement.Sqrt: IECFieldElement;
var
  x1, x2, x3, x6, x9, x11, x22, x44, x88, x176, x220, x223, t1,
    t2: TCryptoLibUInt32Array;
begin
  { *
    * Raise this element to the exponent 2^254 - 2^30 - 2^7 - 2^6 - 2^5 - 2^4 - 2^2
    *
    * Breaking up the exponent's binary representation into "repunits", we get:
    * ( 223 1s ) ( 1 0s ) ( 22 1s ) ( 4 0s ) ( 2 1s ) ( 2 0s)
    *
    * Therefore we need an addition chain containing 2, 22, 223 (the lengths of the repunits)
    * We use: 1, [2], 3, 6, 9, 11, [22], 44, 88, 176, 220, [223]
    * }

  x1 := Fx;
  if ((TNat256.IsZero(x1)) or (TNat256.IsOne(x1))) then
  begin
    result := Self as IECFieldElement;
    Exit;
  end;

  x2 := TNat256.Create();
  TSecP256K1Field.Square(x1, x2);
  TSecP256K1Field.Multiply(x2, x1, x2);
  x3 := TNat256.Create();
  TSecP256K1Field.Square(x2, x3);
  TSecP256K1Field.Multiply(x3, x1, x3);
  x6 := TNat256.Create();
  TSecP256K1Field.SquareN(x3, 3, x6);
  TSecP256K1Field.Multiply(x6, x3, x6);
  x9 := x6;
  TSecP256K1Field.SquareN(x6, 3, x9);
  TSecP256K1Field.Multiply(x9, x3, x9);
  x11 := x9;
  TSecP256K1Field.SquareN(x9, 2, x11);
  TSecP256K1Field.Multiply(x11, x2, x11);
  x22 := TNat256.Create();
  TSecP256K1Field.SquareN(x11, 11, x22);
  TSecP256K1Field.Multiply(x22, x11, x22);
  x44 := x11;
  TSecP256K1Field.SquareN(x22, 22, x44);
  TSecP256K1Field.Multiply(x44, x22, x44);
  x88 := TNat256.Create();
  TSecP256K1Field.SquareN(x44, 44, x88);
  TSecP256K1Field.Multiply(x88, x44, x88);
  x176 := TNat256.Create();
  TSecP256K1Field.SquareN(x88, 88, x176);
  TSecP256K1Field.Multiply(x176, x88, x176);
  x220 := x88;
  TSecP256K1Field.SquareN(x176, 44, x220);
  TSecP256K1Field.Multiply(x220, x44, x220);
  x223 := x44;
  TSecP256K1Field.SquareN(x220, 3, x223);
  TSecP256K1Field.Multiply(x223, x3, x223);

  t1 := x223;
  TSecP256K1Field.SquareN(t1, 23, t1);
  TSecP256K1Field.Multiply(t1, x22, t1);
  TSecP256K1Field.SquareN(t1, 6, t1);
  TSecP256K1Field.Multiply(t1, x2, t1);
  TSecP256K1Field.SquareN(t1, 2, t1);

  t2 := x2;
  TSecP256K1Field.Square(t1, t2);

  if TNat256.Eq(x1, t2) then
  begin
    result := TSecP256K1FieldElement.Create(t1);
  end
  else
  begin
    result := Nil;
  end;
end;

function TSecP256K1FieldElement.Square: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TSecP256K1Field.Square(Fx, z);
  result := TSecP256K1FieldElement.Create(z);
end;

function TSecP256K1FieldElement.Subtract(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TSecP256K1Field.Subtract(Fx, (b as ISecP256K1FieldElement).x, z);
  result := TSecP256K1FieldElement.Create(z);
end;

function TSecP256K1FieldElement.TestBitZero: Boolean;
begin
  result := TNat256.GetBit(Fx, 0) = 1;
end;

function TSecP256K1FieldElement.ToBigInteger: TBigInteger;
begin
  result := TNat256.ToBigInteger(Fx);
end;

function TSecP256K1FieldElement.Add(const b: IECFieldElement): IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TSecP256K1Field.Add(Fx, (b as ISecP256K1FieldElement).x, z);
  result := TSecP256K1FieldElement.Create(z);
end;

function TSecP256K1FieldElement.AddOne: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TSecP256K1Field.AddOne(Fx, z);
  result := TSecP256K1FieldElement.Create(z);
end;

function TSecP256K1FieldElement.Divide(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat256.Create();
  TMod.Invert(TSecP256K1Field.P, (b as ISecP256K1FieldElement).x, z);
  TSecP256K1Field.Multiply(z, Fx, z);
  result := TSecP256K1FieldElement.Create(z);
end;

function TSecP256K1FieldElement.Equals(const other
  : ISecP256K1FieldElement): Boolean;
begin
  if ((Self as ISecP256K1FieldElement) = other) then
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

function TSecP256K1FieldElement.Equals(const other: IECFieldElement): Boolean;
begin
  result := Equals(other as ISecP256K1FieldElement);
end;

{ TSecP256K1Point }

constructor TSecP256K1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement);
begin
  Create(curve, x, y, false);
end;

constructor TSecP256K1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
  if ((x = Nil) <> (y = Nil)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SOneOfECFieldElementIsNil);
  end;
end;

constructor TSecP256K1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

function TSecP256K1Point.Add(const b: IECPoint): IECPoint;
var
  Lcurve: IECCurve;
  x1, Y1, x2, Y2, Z1, Z2, x3, Y3, Z3: ISecP256K1FieldElement;
  c: UInt32;
  tt1, t2, t3, t4, U2, S2, U1, S1, H, R, HSquared, G, V: TCryptoLibUInt32Array;
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

  x1 := RawXCoord as ISecP256K1FieldElement;
  Y1 := RawYCoord as ISecP256K1FieldElement;
  x2 := b.RawXCoord as ISecP256K1FieldElement;
  Y2 := b.RawYCoord as ISecP256K1FieldElement;

  Z1 := RawZCoords[0] as ISecP256K1FieldElement;
  Z2 := b.RawZCoords[0] as ISecP256K1FieldElement;

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
    TSecP256K1Field.Square(Z1.x, S2);

    U2 := t2;
    TSecP256K1Field.Multiply(S2, x2.x, U2);

    TSecP256K1Field.Multiply(S2, Z1.x, S2);
    TSecP256K1Field.Multiply(S2, Y2.x, S2);
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
    TSecP256K1Field.Square(Z2.x, S1);

    U1 := tt1;
    TSecP256K1Field.Multiply(S1, x1.x, U1);

    TSecP256K1Field.Multiply(S1, Z2.x, S1);
    TSecP256K1Field.Multiply(S1, Y1.x, S1);
  end;

  H := TNat256.Create();
  TSecP256K1Field.Subtract(U1, U2, H);

  R := t2;
  TSecP256K1Field.Subtract(S1, S2, R);

  // Check if b = Self or b = -Self
  if (TNat256.IsZero(H)) then
  begin
    if (TNat256.IsZero(R)) then
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
  TSecP256K1Field.Square(H, HSquared);

  G := TNat256.Create();
  TSecP256K1Field.Multiply(HSquared, H, G);

  V := t3;
  TSecP256K1Field.Multiply(HSquared, U1, V);

  TSecP256K1Field.Negate(G, G);
  TNat256.Mul(S1, G, tt1);

  c := TNat256.AddBothTo(V, V, G);
  TSecP256K1Field.Reduce32(c, G);

  x3 := TSecP256K1FieldElement.Create(t4);
  TSecP256K1Field.Square(R, x3.x);
  TSecP256K1Field.Subtract(x3.x, G, x3.x);

  Y3 := TSecP256K1FieldElement.Create(G);
  TSecP256K1Field.Subtract(V, x3.x, Y3.x);
  TSecP256K1Field.MultiplyAddToExt(Y3.x, R, tt1);
  TSecP256K1Field.Reduce(tt1, Y3.x);

  Z3 := TSecP256K1FieldElement.Create(H);
  if (not(Z1IsOne)) then
  begin
    TSecP256K1Field.Multiply(Z3.x, Z1.x, Z3.x);
  end;
  if (not(Z2IsOne)) then
  begin
    TSecP256K1Field.Multiply(Z3.x, Z2.x, Z3.x);
  end;

  zs := TCryptoLibGenericArray<IECFieldElement>.Create(Z3);

  result := TSecP256K1Point.Create(Lcurve, x3, Y3, zs, IsCompressed)
    as IECPoint;
end;

function TSecP256K1Point.Detach: IECPoint;
begin
  result := TSecP256K1Point.Create(Nil, AffineXCoord, AffineYCoord) as IECPoint;
end;

function TSecP256K1Point.Negate: IECPoint;
begin
  if (IsInfinity) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  result := TSecP256K1Point.Create(curve, RawXCoord, RawYCoord.Negate(),
    RawZCoords, IsCompressed) as IECPoint;
end;

function TSecP256K1Point.ThreeTimes: IECPoint;
begin
  if ((IsInfinity) or (RawYCoord.IsZero)) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  // NOTE: Be careful about recursions between TwicePlus and ThreeTimes
  result := Twice().Add(Self as IECPoint);
end;

function TSecP256K1Point.Twice: IECPoint;
var
  Lcurve: IECCurve;
  Y1, x1, Z1, x3, Y3, Z3: ISecP256K1FieldElement;
  c: UInt32;
  Y1Squared, T, M, S, t1: TCryptoLibUInt32Array;
begin

  if (IsInfinity) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  Lcurve := curve;

  Y1 := RawYCoord as ISecP256K1FieldElement;
  if (Y1.IsZero) then
  begin
    result := Lcurve.Infinity;
    Exit;
  end;

  x1 := RawXCoord as ISecP256K1FieldElement;
  Z1 := RawZCoords[0] as ISecP256K1FieldElement;

  Y1Squared := TNat256.Create();
  TSecP256K1Field.Square(Y1.x, Y1Squared);

  T := TNat256.Create();
  TSecP256K1Field.Square(Y1Squared, T);

  M := TNat256.Create();
  TSecP256K1Field.Square(x1.x, M);
  c := TNat256.AddBothTo(M, M, M);
  TSecP256K1Field.Reduce32(c, M);

  S := Y1Squared;
  TSecP256K1Field.Multiply(Y1Squared, x1.x, S);
  c := TNat.ShiftUpBits(8, S, 2, 0);
  TSecP256K1Field.Reduce32(c, S);

  t1 := TNat256.Create();
  c := TNat.ShiftUpBits(8, T, 3, 0, t1);
  TSecP256K1Field.Reduce32(c, t1);

  x3 := TSecP256K1FieldElement.Create(T);
  TSecP256K1Field.Square(M, x3.x);
  TSecP256K1Field.Subtract(x3.x, S, x3.x);
  TSecP256K1Field.Subtract(x3.x, S, x3.x);

  Y3 := TSecP256K1FieldElement.Create(S);
  TSecP256K1Field.Subtract(S, x3.x, Y3.x);
  TSecP256K1Field.Multiply(Y3.x, M, Y3.x);
  TSecP256K1Field.Subtract(Y3.x, t1, Y3.x);

  Z3 := TSecP256K1FieldElement.Create(M);
  TSecP256K1Field.Twice(Y1.x, Z3.x);
  if (not(Z1.IsOne)) then
  begin
    TSecP256K1Field.Multiply(Z3.x, Z1.x, Z3.x);
  end;

  result := TSecP256K1Point.Create(Lcurve, x3, Y3,
    TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed)
    as IECPoint;
end;

function TSecP256K1Point.TwicePlus(const b: IECPoint): IECPoint;
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

{ TSecP256K1Curve }

class function TSecP256K1Curve.GetSecP256K1Curve_Q: TBigInteger;
begin
  result := TBigInteger.Create(1,
    THex.Decode
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F'));
end;

constructor TSecP256K1Curve.Create;
begin
  Fq := SecP256K1Curve_Q;
  Inherited Create(Fq);
  Fm_infinity := TSecP256K1Point.Create(Self as IECCurve, Nil, Nil);
  Fm_a := FromBigInteger(TBigInteger.Zero);
  Fm_b := FromBigInteger(TBigInteger.ValueOf(7));
  Fm_order := TBigInteger.Create(1,
    THex.Decode
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141'));
  Fm_cofactor := TBigInteger.One;
  Fm_coord := SECP256K1_DEFAULT_COORDS;
end;

function TSecP256K1Curve.CloneCurve: IECCurve;
begin
  result := TSecP256K1Curve.Create();
end;

function TSecP256K1Curve.CreateCacheSafeLookupTable(const points
  : TCryptoLibGenericArray<IECPoint>; off, len: Int32): IECLookupTable;
var
  table: TCryptoLibUInt32Array;
  pos, i: Int32;
  P: IECPoint;
begin
  System.SetLength(table, len * SECP256K1_FE_INTS * 2);

  pos := 0;
  for i := 0 to System.Pred(len) do
  begin
    P := points[off + i];
    TNat256.Copy((P.RawXCoord as ISecP256K1FieldElement).x, 0, table, pos);
    pos := pos + SECP256K1_FE_INTS;
    TNat256.Copy((P.RawYCoord as ISecP256K1FieldElement).x, 0, table, pos);
    pos := pos + SECP256K1_FE_INTS;
  end;

  result := TSecP256K1LookupTable.Create(Self as ISecP256K1Curve, table, len);
end;

function TSecP256K1Curve.CreateRawPoint(const x, y: IECFieldElement;
  withCompression: Boolean): IECPoint;
begin
  result := TSecP256K1Point.Create(Self as IECCurve, x, y, withCompression);
end;

function TSecP256K1Curve.CreateRawPoint(const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean)
  : IECPoint;
begin
  result := TSecP256K1Point.Create(Self as IECCurve, x, y, zs, withCompression);
end;

function TSecP256K1Curve.FromBigInteger(const x: TBigInteger): IECFieldElement;
begin
  result := TSecP256K1FieldElement.Create(x);
end;

function TSecP256K1Curve.GetFieldSize: Int32;
begin
  result := Fq.BitLength;
end;

function TSecP256K1Curve.GetInfinity: IECPoint;
begin
  result := Fm_infinity;
end;

function TSecP256K1Curve.GetQ: TBigInteger;
begin
  result := Fq;
end;

function TSecP256K1Curve.SupportsCoordinateSystem(coord: Int32): Boolean;
begin
  case coord of
    TECCurveConstants.COORD_JACOBIAN:
      result := true
  else
    result := false;
  end;
end;

{ TSecP256K1Curve.TSecP256K1LookupTable }

constructor TSecP256K1Curve.TSecP256K1LookupTable.Create
  (const outer: ISecP256K1Curve; const table: TCryptoLibUInt32Array;
  size: Int32);
begin
  Inherited Create();
  Fm_outer := outer;
  Fm_table := table;
  Fm_size := size;
end;

function TSecP256K1Curve.TSecP256K1LookupTable.GetSize: Int32;
begin
  result := Fm_size;
end;

function TSecP256K1Curve.TSecP256K1LookupTable.Lookup(index: Int32): IECPoint;
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

    for J := 0 to System.Pred(SECP256K1_FE_INTS) do
    begin
      x[J] := x[J] xor (Fm_table[pos + J] and MASK);
      y[J] := y[J] xor (Fm_table[pos + SECP256K1_FE_INTS + J] and MASK);
    end;

    pos := pos + (SECP256K1_FE_INTS * 2);
  end;

  result := Fm_outer.CreateRawPoint(TSecP256K1FieldElement.Create(x)
    as ISecP256K1FieldElement, TSecP256K1FieldElement.Create(y)
    as ISecP256K1FieldElement, false);
end;

end.
