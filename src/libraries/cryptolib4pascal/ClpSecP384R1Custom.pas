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

unit ClpSecP384R1Custom;

{$I CryptoLib.inc}

interface

uses
  ClpMod,
  ClpEncoders,
  ClpNat,
  ClpBits,
  ClpNat384,
  ClpECC,
  ClpBigInteger,
  ClpArrayUtils,
  ClpCryptoLibTypes,
  ClpECCurveConstants,
  ClpIECC,
  ClpISecP384R1Custom;

resourcestring
  SInvalidValueForSecP384R1FieldElement =
    'Value Invalid for SecP384R1FieldElement "%s"';
  SOneOfECFieldElementIsNil = 'Exactly One of the Field Elements is Nil';

type
  // 2^384 - 2^128 - 2^96 + 2^32 - 1
  TSecP384R1Field = class sealed(TObject)

  strict private
  const
    P11 = UInt32($FFFFFFFF);
    PExt23 = UInt32($FFFFFFFF);

    class var

      FP, FPExt, FPExtInv: TCryptoLibUInt32Array;

    class function GetP: TCryptoLibUInt32Array; static; inline;

    class procedure AddPInvTo(const z: TCryptoLibUInt32Array); static;
    class procedure SubPInvFrom(const z: TCryptoLibUInt32Array); static;

    class procedure Boot(); static;
    class constructor SecP384R1Field();

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
    class procedure Negate(const x, z: TCryptoLibUInt32Array); static; inline;
    class procedure Reduce(const xx, z: TCryptoLibUInt32Array); static;
    class procedure Reduce32(x: UInt32; const z: TCryptoLibUInt32Array); static;
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
  TSecP384R1FieldElement = class(TAbstractFpFieldElement,
    ISecP384R1FieldElement)

  strict private

    function Equals(const other: ISecP384R1FieldElement): Boolean;
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
  TSecP384R1Point = class sealed(TAbstractFpPoint, ISecP384R1Point)

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
  TSecP384R1Curve = class sealed(TAbstractFpCurve, ISecP384R1Curve)

  strict private

  type
    TSecP384R1LookupTable = class sealed(TInterfacedObject,
      ISecP384R1LookupTable, IECLookupTable)

    strict private
    var
      Fm_outer: ISecP384R1Curve;
      Fm_table: TCryptoLibUInt32Array;
      Fm_size: Int32;

      function GetSize: Int32; virtual;

    public

      constructor Create(const outer: ISecP384R1Curve;
        const table: TCryptoLibUInt32Array; size: Int32);

      function Lookup(index: Int32): IECPoint; virtual;

      property size: Int32 read GetSize;

    end;

  const
    SECP384R1_DEFAULT_COORDS = Int32(TECCurveConstants.COORD_JACOBIAN);
    SECP384R1_FE_INTS = Int32(12);

  var
    Fq: TBigInteger;

    class function GetSecP384R1Curve_Q: TBigInteger; static; inline;

  strict protected
  var
    Fm_infinity: ISecP384R1Point;

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

    class property SecP384R1Curve_Q: TBigInteger read GetSecP384R1Curve_Q;

  end;

implementation

{ TSecP384R1Field }

class constructor TSecP384R1Field.SecP384R1Field;
begin
  TSecP384R1Field.Boot;
end;

class function TSecP384R1Field.GetP: TCryptoLibUInt32Array;
begin
  result := FP;
end;

class procedure TSecP384R1Field.AddPInvTo(const z: TCryptoLibUInt32Array);
var
  c: Int64;
begin
  c := Int64(z[0]) + 1;
  z[0] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[1]) - 1);
  z[1] := UInt32(c);
  c := TBits.Asr64(c, 32);
  if (c <> 0) then
  begin
    c := c + Int64(z[2]);
    z[2] := UInt32(c);
    c := TBits.Asr64(c, 32);
  end;
  c := c + (Int64(z[3]) + 1);
  z[3] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[4]) + 1);
  z[4] := UInt32(c);
  c := TBits.Asr64(c, 32);
  if (c <> 0) then
  begin
    TNat.IncAt(12, z, 5);
  end;
end;

class procedure TSecP384R1Field.Boot;
begin
  FP := TCryptoLibUInt32Array.Create($FFFFFFFF, $00000000, $00000000, $FFFFFFFF,
    $FFFFFFFE, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
    $FFFFFFFF);
  FPExt := TCryptoLibUInt32Array.Create($00000001, $FFFFFFFE, $00000000,
    $00000002, $00000000, $FFFFFFFE, $00000000, $00000002, $00000001, $00000000,
    $00000000, $00000000, $FFFFFFFE, $00000001, $00000000, $FFFFFFFE, $FFFFFFFD,
    $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
    $FFFFFFFF);
  FPExtInv := TCryptoLibUInt32Array.Create($FFFFFFFF, $00000001, $FFFFFFFF,
    $FFFFFFFD, $FFFFFFFF, $00000001, $FFFFFFFF, $FFFFFFFD, $FFFFFFFE, $FFFFFFFF,
    $FFFFFFFF, $FFFFFFFF, $00000001, $FFFFFFFE, $FFFFFFFF, $00000001,
    $00000002);
end;

class procedure TSecP384R1Field.SubPInvFrom(const z: TCryptoLibUInt32Array);
var
  c: Int64;
begin
  c := Int64(z[0]) - 1;
  z[0] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[1]) + 1);
  z[1] := UInt32(c);
  c := TBits.Asr64(c, 32);
  if (c <> 0) then
  begin
    c := c + Int64(z[2]);
    z[2] := UInt32(c);
    c := TBits.Asr64(c, 32);
  end;
  c := c + (Int64(z[3]) - 1);
  z[3] := UInt32(c);
  c := TBits.Asr64(c, 32);
  c := c + (Int64(z[4]) - 1);
  z[4] := UInt32(c);
  c := TBits.Asr64(c, 32);
  if (c <> 0) then
  begin
    TNat.DecAt(12, z, 5);
  end;
end;

class procedure TSecP384R1Field.Add(const x, y, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat.Add(12, x, y, z);
  if ((c <> 0) or ((z[11] = P11) and (TNat.Gte(12, z, FP)))) then
  begin
    AddPInvTo(z);
  end;
end;

class procedure TSecP384R1Field.AddExt(const xx, yy, zz: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat.Add(24, xx, yy, zz);
  if ((c <> 0) or ((zz[23] = PExt23) and (TNat.Gte(24, zz, FPExt)))) then
  begin
    if (TNat.AddTo(System.Length(FPExtInv), FPExtInv, zz) <> 0) then
    begin
      TNat.IncAt(24, zz, System.Length(FPExtInv));
    end;
  end;
end;

class procedure TSecP384R1Field.AddOne(const x, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat.Inc(12, x, z);
  if ((c <> 0) or ((z[11] = P11) and (TNat.Gte(12, z, FP)))) then
  begin
    AddPInvTo(z);
  end;
end;

class function TSecP384R1Field.FromBigInteger(const x: TBigInteger)
  : TCryptoLibUInt32Array;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.FromBigInteger(384, x);
  if ((z[11] = P11) and (TNat.Gte(12, z, FP))) then
  begin
    TNat.SubFrom(12, FP, z);
  end;
  result := z;
end;

class procedure TSecP384R1Field.Half(const x, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  if ((x[0] and 1) = 0) then
  begin
    TNat.ShiftDownBit(12, x, 0, z);
  end
  else
  begin
    c := TNat.Add(12, x, FP, z);
    TNat.ShiftDownBit(12, z, c);
  end;
end;

class procedure TSecP384R1Field.Reduce32(x: UInt32;
  const z: TCryptoLibUInt32Array);
var
  cc, xx12: Int64;
begin
  cc := 0;

  if (x <> 0) then
  begin
    xx12 := x;

    cc := cc + (Int64(z[0]) + xx12);
    z[0] := UInt32(cc);
    cc := TBits.Asr64(cc, 32);
    cc := cc + (Int64(z[1]) - xx12);
    z[1] := UInt32(cc);
    cc := TBits.Asr64(cc, 32);
    if (cc <> 0) then
    begin
      cc := cc + Int64(z[2]);
      z[2] := UInt32(cc);
      cc := TBits.Asr64(cc, 32);
    end;
    cc := cc + (Int64(z[3]) + xx12);
    z[3] := UInt32(cc);
    cc := TBits.Asr64(cc, 32);
    cc := cc + (Int64(z[4]) + xx12);
    z[4] := UInt32(cc);
    cc := TBits.Asr64(cc, 32);

{$IFDEF DEBUG}
    System.Assert((cc = 0) or (cc = 1));
{$ENDIF DEBUG}
  end;

  if (((cc <> 0) and (TNat.IncAt(12, z, 5) <> 0)) or
    ((z[11] = P11) and (TNat.Gte(12, z, FP)))) then
  begin
    AddPInvTo(z);
  end;
end;

class procedure TSecP384R1Field.Reduce(const xx, z: TCryptoLibUInt32Array);
const
  n: Int64 = 1;
var
  cc, xx16, xx17, xx18, xx19, xx20, xx21, xx22, xx23, t0, t1, t2, t3, t4, t5,
    t6, t7: Int64;

begin
  xx16 := xx[16];
  xx17 := xx[17];
  xx18 := xx[18];
  xx19 := xx[19];
  xx20 := xx[20];
  xx21 := xx[21];
  xx22 := xx[22];
  xx23 := xx[23];

  t0 := Int64(xx[12]) + xx20 - n;
  t1 := Int64(xx[13]) + xx22;
  t2 := Int64(xx[14]) + xx22 + xx23;
  t3 := Int64(xx[15]) + xx23;
  t4 := xx17 + xx21;
  t5 := xx21 - xx23;
  t6 := xx22 - xx23;
  t7 := t0 + t5;

  cc := 0;
  cc := cc + (Int64(xx[0]) + t7);
  z[0] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[1]) + xx23 - t0 + t1);
  z[1] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[2]) - xx21 - t1 + t2);
  z[2] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[3]) - t2 + t3 + t7);
  z[3] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[4]) + xx16 + xx21 + t1 - t3 + t7);
  z[4] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[5]) - xx16 + t1 + t2 + t4);
  z[5] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[6]) + xx18 - xx17 + t2 + t3);
  z[6] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[7]) + xx16 + xx19 - xx18 + t3);
  z[7] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[8]) + xx16 + xx17 + xx20 - xx19);
  z[8] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[9]) + xx18 - xx20 + t4);
  z[9] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[10]) + xx18 + xx19 - t5 + t6);
  z[10] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (Int64(xx[11]) + xx19 + xx20 - t6);
  z[11] := UInt32(cc);
  cc := TBits.Asr64(cc, 32);
  cc := cc + (n);

{$IFDEF DEBUG}
  System.Assert(cc >= 0);
{$ENDIF DEBUG}
  Reduce32(UInt32(cc), z);
end;

class procedure TSecP384R1Field.Multiply(const x, y, z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
  tt := TNat.Create(24);
  TNat384.Mul(x, y, tt);
  Reduce(tt, z);
end;

class procedure TSecP384R1Field.Negate(const x, z: TCryptoLibUInt32Array);
begin
  if (TNat.IsZero(12, x)) then
  begin
    TNat.Zero(12, z);
  end
  else
  begin
    TNat.Sub(12, FP, x, z);
  end;
end;

class procedure TSecP384R1Field.Square(const x, z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
  tt := TNat.Create(24);
  TNat384.Square(x, tt);
  Reduce(tt, z);
end;

class procedure TSecP384R1Field.SquareN(const x: TCryptoLibUInt32Array;
  n: Int32; const z: TCryptoLibUInt32Array);
var
  tt: TCryptoLibUInt32Array;
begin
{$IFDEF DEBUG}
  System.Assert(n > 0);
{$ENDIF DEBUG}
  tt := TNat.Create(24);
  TNat384.Square(x, tt);
  Reduce(tt, z);

  System.Dec(n);
  while (n > 0) do
  begin
    TNat384.Square(z, tt);
    Reduce(tt, z);
    System.Dec(n);
  end;
end;

class procedure TSecP384R1Field.Subtract(const x, y, z: TCryptoLibUInt32Array);
var
  c: Int32;
begin
  c := TNat.Sub(12, x, y, z);
  if (c <> 0) then
  begin
    SubPInvFrom(z);
  end;
end;

class procedure TSecP384R1Field.SubtractExt(const xx, yy,
  zz: TCryptoLibUInt32Array);
var
  c: Int32;
begin
  c := TNat.Sub(24, xx, yy, zz);
  if (c <> 0) then
  begin
    if (TNat.SubFrom(System.Length(FPExtInv), FPExtInv, zz) <> 0) then
    begin
      TNat.DecAt(24, zz, System.Length(FPExtInv));
    end;
  end;
end;

class procedure TSecP384R1Field.Twice(const x, z: TCryptoLibUInt32Array);
var
  c: UInt32;
begin
  c := TNat.ShiftUpBit(12, x, 0, z);
  if ((c <> 0) or ((z[11] = P11) and (TNat.Gte(12, z, FP)))) then
  begin
    AddPInvTo(z);
  end;
end;

{ TSecP384R1FieldElement }

class function TSecP384R1FieldElement.GetQ: TBigInteger;
begin
  result := TSecP384R1Curve.SecP384R1Curve_Q;
end;

function TSecP384R1FieldElement.GetX: TCryptoLibUInt32Array;
begin
  result := Fx;
end;

constructor TSecP384R1FieldElement.Create;
begin
  Inherited Create();
  Fx := TNat.Create(12);
end;

constructor TSecP384R1FieldElement.Create(const x: TBigInteger);
begin
  if ((not(x.IsInitialized)) or (x.SignValue < 0) or (x.CompareTo(Q) >= 0)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt
      (@SInvalidValueForSecP384R1FieldElement, ['x']);
  end;
  Inherited Create();
  Fx := TSecP384R1Field.FromBigInteger(x);
end;

constructor TSecP384R1FieldElement.Create(const x: TCryptoLibUInt32Array);
begin
  Inherited Create();
  Fx := x;
end;

function TSecP384R1FieldElement.GetFieldName: string;
begin
  result := 'SecP384R1Field';
end;

function TSecP384R1FieldElement.GetFieldSize: Int32;
begin
  result := Q.BitLength;
end;

function TSecP384R1FieldElement.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := Q.GetHashCode() xor TArrayUtils.GetArrayHashCode(Fx, 0, 12);
end;

function TSecP384R1FieldElement.GetIsOne: Boolean;
begin
  result := TNat.IsOne(12, Fx);
end;

function TSecP384R1FieldElement.GetIsZero: Boolean;
begin
  result := TNat.IsZero(12, Fx);
end;

function TSecP384R1FieldElement.Sqrt: IECFieldElement;
var
  x1, t1, t2, t3, t4, r: TCryptoLibUInt32Array;
begin
  // Raise this element to the exponent 2^382 - 2^126 - 2^94 + 2^30
  x1 := Fx;
  if ((TNat.IsZero(12, x1)) or (TNat.IsOne(12, x1))) then
  begin
    result := Self as IECFieldElement;
    Exit;
  end;

  t1 := TNat.Create(12);
  t2 := TNat.Create(12);
  t3 := TNat.Create(12);
  t4 := TNat.Create(12);

  TSecP384R1Field.Square(x1, t1);
  TSecP384R1Field.Multiply(t1, x1, t1);

  TSecP384R1Field.SquareN(t1, 2, t2);
  TSecP384R1Field.Multiply(t2, t1, t2);

  TSecP384R1Field.Square(t2, t2);
  TSecP384R1Field.Multiply(t2, x1, t2);

  TSecP384R1Field.SquareN(t2, 5, t3);
  TSecP384R1Field.Multiply(t3, t2, t3);

  TSecP384R1Field.SquareN(t3, 5, t4);
  TSecP384R1Field.Multiply(t4, t2, t4);

  TSecP384R1Field.SquareN(t4, 15, t2);
  TSecP384R1Field.Multiply(t2, t4, t2);

  TSecP384R1Field.SquareN(t2, 2, t3);
  TSecP384R1Field.Multiply(t1, t3, t1);

  TSecP384R1Field.SquareN(t3, 28, t3);
  TSecP384R1Field.Multiply(t2, t3, t2);

  TSecP384R1Field.SquareN(t2, 60, t3);
  TSecP384R1Field.Multiply(t3, t2, t3);

  r := t2;

  TSecP384R1Field.SquareN(t3, 120, r);
  TSecP384R1Field.Multiply(r, t3, r);

  TSecP384R1Field.SquareN(r, 15, r);
  TSecP384R1Field.Multiply(r, t4, r);

  TSecP384R1Field.SquareN(r, 33, r);
  TSecP384R1Field.Multiply(r, t1, r);

  TSecP384R1Field.SquareN(r, 64, r);
  TSecP384R1Field.Multiply(r, x1, r);

  TSecP384R1Field.SquareN(r, 30, t1);
  TSecP384R1Field.Square(t1, t2);

  if TNat.Eq(12, x1, t2) then
  begin
    result := TSecP384R1FieldElement.Create(t1);
  end
  else
  begin
    result := Nil;
  end;
end;

function TSecP384R1FieldElement.TestBitZero: Boolean;
begin
  result := TNat.GetBit(Fx, 0) = 1;
end;

function TSecP384R1FieldElement.ToBigInteger: TBigInteger;
begin
  result := TNat.ToBigInteger(12, Fx);
end;

function TSecP384R1FieldElement.Add(const b: IECFieldElement): IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(12);
  TSecP384R1Field.Add(Fx, (b as ISecP384R1FieldElement).x, z);
  result := TSecP384R1FieldElement.Create(z);
end;

function TSecP384R1FieldElement.AddOne: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(12);
  TSecP384R1Field.AddOne(Fx, z);
  result := TSecP384R1FieldElement.Create(z);
end;

function TSecP384R1FieldElement.Subtract(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(12);
  TSecP384R1Field.Subtract(Fx, (b as ISecP384R1FieldElement).x, z);
  result := TSecP384R1FieldElement.Create(z);
end;

function TSecP384R1FieldElement.Multiply(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(12);
  TSecP384R1Field.Multiply(Fx, (b as ISecP384R1FieldElement).x, z);
  result := TSecP384R1FieldElement.Create(z);
end;

function TSecP384R1FieldElement.Divide(const b: IECFieldElement)
  : IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(12);
  TMod.Invert(TSecP384R1Field.P, (b as ISecP384R1FieldElement).x, z);
  TSecP384R1Field.Multiply(z, Fx, z);
  result := TSecP384R1FieldElement.Create(z);
end;

function TSecP384R1FieldElement.Negate: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(12);
  TSecP384R1Field.Negate(Fx, z);
  result := TSecP384R1FieldElement.Create(z);
end;

function TSecP384R1FieldElement.Square: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(12);
  TSecP384R1Field.Square(Fx, z);
  result := TSecP384R1FieldElement.Create(z);
end;

function TSecP384R1FieldElement.Invert: IECFieldElement;
var
  z: TCryptoLibUInt32Array;
begin
  z := TNat.Create(12);
  TMod.Invert(TSecP384R1Field.P, Fx, z);
  result := TSecP384R1FieldElement.Create(z);
end;

function TSecP384R1FieldElement.Equals(const other
  : ISecP384R1FieldElement): Boolean;
begin
  if ((Self as ISecP384R1FieldElement) = other) then
  begin
    result := true;
    Exit;
  end;
  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := TNat.Eq(12, Fx, other.x);
end;

function TSecP384R1FieldElement.Equals(const other: IECFieldElement): Boolean;
begin
  result := Equals(other as ISecP384R1FieldElement);
end;

{ TSecP384R1Point }

constructor TSecP384R1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement);
begin
  Create(curve, x, y, false);
end;

constructor TSecP384R1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, withCompression);
  if ((x = Nil) <> (y = Nil)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SOneOfECFieldElementIsNil);
  end;
end;

constructor TSecP384R1Point.Create(const curve: IECCurve;
  const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean);
begin
  Inherited Create(curve, x, y, zs, withCompression);
end;

function TSecP384R1Point.Add(const b: IECPoint): IECPoint;
var
  Lcurve: IECCurve;
  x1, Y1, X2, Y2, Z1, Z2, X3, Y3, Z3: ISecP384R1FieldElement;
  c: UInt32;
  tt1, tt2, t3, t4, U2, S2, U1, S1, H, r, HSquared, G, V: TCryptoLibUInt32Array;
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

  x1 := RawXCoord as ISecP384R1FieldElement;
  Y1 := RawYCoord as ISecP384R1FieldElement;
  X2 := b.RawXCoord as ISecP384R1FieldElement;
  Y2 := b.RawYCoord as ISecP384R1FieldElement;

  Z1 := RawZCoords[0] as ISecP384R1FieldElement;
  Z2 := b.RawZCoords[0] as ISecP384R1FieldElement;

  tt1 := TNat.Create(24);
  tt2 := TNat.Create(24);
  t3 := TNat.Create(12);
  t4 := TNat.Create(12);

  Z1IsOne := Z1.IsOne;

  if (Z1IsOne) then
  begin
    U2 := X2.x;
    S2 := Y2.x;
  end
  else
  begin
    S2 := t3;
    TSecP384R1Field.Square(Z1.x, S2);

    U2 := tt2;
    TSecP384R1Field.Multiply(S2, X2.x, U2);

    TSecP384R1Field.Multiply(S2, Z1.x, S2);
    TSecP384R1Field.Multiply(S2, Y2.x, S2);
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
    TSecP384R1Field.Square(Z2.x, S1);

    U1 := tt1;
    TSecP384R1Field.Multiply(S1, x1.x, U1);

    TSecP384R1Field.Multiply(S1, Z2.x, S1);
    TSecP384R1Field.Multiply(S1, Y1.x, S1);
  end;

  H := TNat.Create(12);
  TSecP384R1Field.Subtract(U1, U2, H);

  r := TNat.Create(12);
  TSecP384R1Field.Subtract(S1, S2, r);

  // Check if b = Self or b = -Self
  if (TNat.IsZero(12, H)) then
  begin
    if (TNat.IsZero(12, r)) then
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
  TSecP384R1Field.Square(H, HSquared);

  G := TNat.Create(12);
  TSecP384R1Field.Multiply(HSquared, H, G);

  V := t3;
  TSecP384R1Field.Multiply(HSquared, U1, V);

  TSecP384R1Field.Negate(G, G);
  TNat384.Mul(S1, G, tt1);

  c := TNat.AddBothTo(12, V, V, G);
  TSecP384R1Field.Reduce32(c, G);

  X3 := TSecP384R1FieldElement.Create(t4);
  TSecP384R1Field.Square(r, X3.x);
  TSecP384R1Field.Subtract(X3.x, G, X3.x);

  Y3 := TSecP384R1FieldElement.Create(G);
  TSecP384R1Field.Subtract(V, X3.x, Y3.x);
  TNat384.Mul(Y3.x, r, tt2);
  TSecP384R1Field.AddExt(tt1, tt2, tt1);
  TSecP384R1Field.Reduce(tt1, Y3.x);

  Z3 := TSecP384R1FieldElement.Create(H);
  if (not(Z1IsOne)) then
  begin
    TSecP384R1Field.Multiply(Z3.x, Z1.x, Z3.x);
  end;
  if (not(Z2IsOne)) then
  begin
    TSecP384R1Field.Multiply(Z3.x, Z2.x, Z3.x);
  end;

  zs := TCryptoLibGenericArray<IECFieldElement>.Create(Z3);

  result := TSecP384R1Point.Create(Lcurve, X3, Y3, zs, IsCompressed)
    as IECPoint;
end;

function TSecP384R1Point.Detach: IECPoint;
begin
  result := TSecP384R1Point.Create(Nil, AffineXCoord, AffineYCoord) as IECPoint;
end;

function TSecP384R1Point.Negate: IECPoint;
begin
  if (IsInfinity) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  result := TSecP384R1Point.Create(curve, RawXCoord, RawYCoord.Negate(),
    RawZCoords, IsCompressed) as IECPoint;
end;

function TSecP384R1Point.ThreeTimes: IECPoint;
begin
  if ((IsInfinity) or (RawYCoord.IsZero)) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  // NOTE: Be careful about recursions between TwicePlus and ThreeTimes
  result := Twice().Add(Self as IECPoint);
end;

function TSecP384R1Point.Twice: IECPoint;
var
  Lcurve: IECCurve;
  Y1, x1, Z1, X3, Y3, Z3: ISecP384R1FieldElement;
  c: UInt32;
  Y1Squared, Z1Squared, T, M, S, t1, t2: TCryptoLibUInt32Array;
  Z1IsOne: Boolean;
begin

  if (IsInfinity) then
  begin
    result := Self as IECPoint;
    Exit;
  end;

  Lcurve := curve;

  Y1 := RawYCoord as ISecP384R1FieldElement;
  if (Y1.IsZero) then
  begin
    result := Lcurve.Infinity;
    Exit;
  end;

  x1 := RawXCoord as ISecP384R1FieldElement;
  Z1 := RawZCoords[0] as ISecP384R1FieldElement;

  t1 := TNat.Create(12);
  t2 := TNat.Create(12);
  Y1Squared := TNat.Create(12);
  TSecP384R1Field.Square(Y1.x, Y1Squared);

  T := TNat.Create(12);
  TSecP384R1Field.Square(Y1Squared, T);

  Z1IsOne := Z1.IsOne;

  Z1Squared := Z1.x;
  if (not(Z1IsOne)) then
  begin
    Z1Squared := t2;
    TSecP384R1Field.Square(Z1.x, Z1Squared);
  end;

  TSecP384R1Field.Subtract(x1.x, Z1Squared, t1);

  M := t2;
  TSecP384R1Field.Add(x1.x, Z1Squared, M);
  TSecP384R1Field.Multiply(M, t1, M);
  c := TNat.AddBothTo(12, M, M, M);
  TSecP384R1Field.Reduce32(c, M);

  S := Y1Squared;
  TSecP384R1Field.Multiply(Y1Squared, x1.x, S);
  c := TNat.ShiftUpBits(12, S, 2, 0);
  TSecP384R1Field.Reduce32(c, S);

  c := TNat.ShiftUpBits(12, T, 3, 0, t1);
  TSecP384R1Field.Reduce32(c, t1);

  X3 := TSecP384R1FieldElement.Create(T);
  TSecP384R1Field.Square(M, X3.x);
  TSecP384R1Field.Subtract(X3.x, S, X3.x);
  TSecP384R1Field.Subtract(X3.x, S, X3.x);

  Y3 := TSecP384R1FieldElement.Create(S);
  TSecP384R1Field.Subtract(S, X3.x, Y3.x);
  TSecP384R1Field.Multiply(Y3.x, M, Y3.x);
  TSecP384R1Field.Subtract(Y3.x, t1, Y3.x);

  Z3 := TSecP384R1FieldElement.Create(M);
  TSecP384R1Field.Twice(Y1.x, Z3.x);
  if (not(Z1IsOne)) then
  begin
    TSecP384R1Field.Multiply(Z3.x, Z1.x, Z3.x);
  end;

  result := TSecP384R1Point.Create(Lcurve, X3, Y3,
    TCryptoLibGenericArray<IECFieldElement>.Create(Z3), IsCompressed)
    as IECPoint;
end;

function TSecP384R1Point.TwicePlus(const b: IECPoint): IECPoint;
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

{ TSecP384R1Curve }

class function TSecP384R1Curve.GetSecP384R1Curve_Q: TBigInteger;
begin
  result := TBigInteger.Create(1,
    THex.Decode
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF')
    );
end;

constructor TSecP384R1Curve.Create;
begin
  Fq := SecP384R1Curve_Q;
  Inherited Create(Fq);
  Fm_infinity := TSecP384R1Point.Create(Self as IECCurve, Nil, Nil);
  Fm_a := FromBigInteger(TBigInteger.Create(1,
    THex.Decode
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFC'))
    );
  Fm_b := FromBigInteger(TBigInteger.Create(1,
    THex.Decode
    ('B3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF'))
    );
  Fm_order := TBigInteger.Create(1,
    THex.Decode
    ('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF581A0DB248B0A77AECEC196ACCC52973')
    );
  Fm_cofactor := TBigInteger.One;
  Fm_coord := SECP384R1_DEFAULT_COORDS;
end;

function TSecP384R1Curve.CloneCurve: IECCurve;
begin
  result := TSecP384R1Curve.Create();
end;

function TSecP384R1Curve.CreateCacheSafeLookupTable(const points
  : TCryptoLibGenericArray<IECPoint>; off, len: Int32): IECLookupTable;
var
  table: TCryptoLibUInt32Array;
  pos, i: Int32;
  P: IECPoint;
begin
  System.SetLength(table, len * SECP384R1_FE_INTS * 2);

  pos := 0;
  for i := 0 to System.Pred(len) do
  begin
    P := points[off + i];
    TNat.Copy(SECP384R1_FE_INTS, (P.RawXCoord as ISecP384R1FieldElement).x, 0,
      table, pos);
    pos := pos + SECP384R1_FE_INTS;
    TNat.Copy(SECP384R1_FE_INTS, (P.RawYCoord as ISecP384R1FieldElement).x, 0,
      table, pos);
    pos := pos + SECP384R1_FE_INTS;
  end;

  result := TSecP384R1LookupTable.Create(Self as ISecP384R1Curve, table, len);
end;

function TSecP384R1Curve.CreateRawPoint(const x, y: IECFieldElement;
  withCompression: Boolean): IECPoint;
begin
  result := TSecP384R1Point.Create(Self as IECCurve, x, y, withCompression);
end;

function TSecP384R1Curve.CreateRawPoint(const x, y: IECFieldElement;
  const zs: TCryptoLibGenericArray<IECFieldElement>; withCompression: Boolean)
  : IECPoint;
begin
  result := TSecP384R1Point.Create(Self as IECCurve, x, y, zs, withCompression);
end;

function TSecP384R1Curve.FromBigInteger(const x: TBigInteger): IECFieldElement;
begin
  result := TSecP384R1FieldElement.Create(x);
end;

function TSecP384R1Curve.GetFieldSize: Int32;
begin
  result := Fq.BitLength;
end;

function TSecP384R1Curve.GetInfinity: IECPoint;
begin
  result := Fm_infinity;
end;

function TSecP384R1Curve.GetQ: TBigInteger;
begin
  result := Fq;
end;

function TSecP384R1Curve.SupportsCoordinateSystem(coord: Int32): Boolean;
begin
  case coord of
    TECCurveConstants.COORD_JACOBIAN:
      result := true
  else
    result := false;
  end;
end;

{ TSecP384R1Curve.TSecP384R1LookupTable }

constructor TSecP384R1Curve.TSecP384R1LookupTable.Create
  (const outer: ISecP384R1Curve; const table: TCryptoLibUInt32Array;
  size: Int32);
begin
  Inherited Create();
  Fm_outer := outer;
  Fm_table := table;
  Fm_size := size;
end;

function TSecP384R1Curve.TSecP384R1LookupTable.GetSize: Int32;
begin
  result := Fm_size;
end;

function TSecP384R1Curve.TSecP384R1LookupTable.Lookup(index: Int32): IECPoint;
var
  x, y: TCryptoLibUInt32Array;
  pos, i, J: Int32;
  MASK: UInt32;
begin
  x := TNat.Create(SECP384R1_FE_INTS);
  y := TNat.Create(SECP384R1_FE_INTS);
  pos := 0;

  for i := 0 to System.Pred(Fm_size) do
  begin
    MASK := UInt32(TBits.Asr32((i xor index) - 1, 31));

    for J := 0 to System.Pred(SECP384R1_FE_INTS) do
    begin
      x[J] := x[J] xor (Fm_table[pos + J] and MASK);
      y[J] := y[J] xor (Fm_table[pos + SECP384R1_FE_INTS + J] and MASK);
    end;

    pos := pos + (SECP384R1_FE_INTS * 2);
  end;

  result := Fm_outer.CreateRawPoint(TSecP384R1FieldElement.Create(x)
    as ISecP384R1FieldElement, TSecP384R1FieldElement.Create(y)
    as ISecP384R1FieldElement, false);
end;

end.
