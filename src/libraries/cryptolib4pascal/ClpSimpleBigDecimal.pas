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

unit ClpSimpleBigDecimal;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpBigInteger;

resourcestring
  SInvalidScale = 'Scale may not be Negative';
  SUnEqualScale =
    'Only SimpleBigDecimal of Same Scale Allowed in Arithmetic Operations';
  SNegativeScale = 'Scale may not be Negative';

type
  /// **
  // * Class representing a simple version of a big decimal. A
  // * <code>SimpleBigDecimal</code> is basically a
  // * {@link java.math.BigInteger BigInteger} with a few digits on the right of
  // * the decimal point. The number of (binary) digits on the right of the decimal
  // * point is called the <code>scale</code> of the <code>SimpleBigDecimal</code>.
  // * Unlike in {@link java.math.BigDecimal BigDecimal}, the scale is not adjusted
  // * automatically, but must be set manually. All <code>SimpleBigDecimal</code>s
  // * taking part in the same arithmetic operation must have equal scale. The
  // * result of a multiplication of two <code>SimpleBigDecimal</code>s returns a
  // * <code>SimpleBigDecimal</code> with double scale.
  // */
  TSimpleBigDecimal = record

  strict private
  var
    FbigInt: TBigInteger;
    Fscale: Int32;

  strict private

    function GetInt32Value: Int32; inline;
    function GetInt64Value: Int64; inline;
    function GetScale: Int32; inline;

    // constructor Create(const limBigDec: TSimpleBigDecimal); overload;

    procedure CheckScale(const b: TSimpleBigDecimal); inline;

  public
    // /**
    // * Constructor for <code>SimpleBigDecimal</code>. The value of the
    // * constructed <code>SimpleBigDecimal</code> Equals <code>bigInt /
    // * 2<sup>scale</sup></code>.
    // * @param bigInt The <code>bigInt</code> value parameter.
    // * @param scale The scale of the constructed <code>SimpleBigDecimal</code>.
    // */
    constructor Create(const bigInt: TBigInteger; scale: Int32); overload;

    function AdjustScale(newScale: Int32): TSimpleBigDecimal;

    function Add(const b: TSimpleBigDecimal): TSimpleBigDecimal; overload;

    function Add(const b: TBigInteger): TSimpleBigDecimal; overload;

    function Negate(): TSimpleBigDecimal;

    function Subtract(const b: TSimpleBigDecimal): TSimpleBigDecimal; overload;

    function Subtract(const b: TBigInteger): TSimpleBigDecimal; overload;

    function Multiply(const b: TSimpleBigDecimal): TSimpleBigDecimal; overload;

    function Multiply(const b: TBigInteger): TSimpleBigDecimal; overload;

    function Divide(const b: TSimpleBigDecimal): TSimpleBigDecimal; overload;

    function Divide(const b: TBigInteger): TSimpleBigDecimal; overload;

    function ShiftLeft(n: Int32): TSimpleBigDecimal;

    function CompareTo(const val: TSimpleBigDecimal): Int32; overload;

    function CompareTo(const val: TBigInteger): Int32; overload;

    function Floor(): TBigInteger;

    function Round(): TBigInteger;

    function ToString(): String; inline;

    function Equals(const other: TSimpleBigDecimal): Boolean; inline;

    function GetHashCode(): Int32; inline;

    property Int32Value: Int32 read GetInt32Value;

    property Int64Value: Int64 read GetInt64Value;

    property scale: Int32 read GetScale;

    // /**
    // * Returns a <code>SimpleBigDecimal</code> representing the same numerical
    // * value as <code>value</code>.
    // * @param value The value of the <code>SimpleBigDecimal</code> to be
    // * created.
    // * @param scale The scale of the <code>SimpleBigDecimal</code> to be
    // * created.
    // * @return The such created <code>SimpleBigDecimal</code>.
    // */
    class function GetInstance(const val: TBigInteger; scale: Int32)
      : TSimpleBigDecimal; static; inline;

  end;

implementation

{ TSimpleBigDecimal }

procedure TSimpleBigDecimal.CheckScale(const b: TSimpleBigDecimal);
begin
  if (Fscale <> b.Fscale) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SUnEqualScale);
  end;
end;

function TSimpleBigDecimal.Add(const b: TSimpleBigDecimal): TSimpleBigDecimal;
begin
  CheckScale(b);
  Result := TSimpleBigDecimal.Create(FbigInt.Add(b.FbigInt), Fscale);
end;

function TSimpleBigDecimal.Add(const b: TBigInteger): TSimpleBigDecimal;
begin
  Result := TSimpleBigDecimal.Create(FbigInt.Add(b.ShiftLeft(Fscale)), Fscale);
end;

function TSimpleBigDecimal.AdjustScale(newScale: Int32): TSimpleBigDecimal;
begin
  if (newScale < 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNegativeScale);
  end;

  if (newScale = Fscale) then
  begin
    Result := Self;
    Exit;
  end;

  Result := TSimpleBigDecimal.Create(FbigInt.ShiftLeft(newScale - Fscale),
    newScale);
end;

function TSimpleBigDecimal.CompareTo(const val: TBigInteger): Int32;
begin
  Result := FbigInt.CompareTo(val.ShiftLeft(Fscale));
end;

function TSimpleBigDecimal.CompareTo(const val: TSimpleBigDecimal): Int32;
begin
  CheckScale(val);
  Result := FbigInt.CompareTo(val.FbigInt);
end;

// constructor TSimpleBigDecimal.Create(const limBigDec: TSimpleBigDecimal);
// begin
// FbigInt := limBigDec.FbigInt;
// Fscale := limBigDec.Fscale;
// end;

constructor TSimpleBigDecimal.Create(const bigInt: TBigInteger; scale: Int32);
begin
  if (scale < 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidScale);
  end;

  FbigInt := bigInt;
  Fscale := scale;
end;

function TSimpleBigDecimal.Divide(const b: TSimpleBigDecimal)
  : TSimpleBigDecimal;
var
  dividend: TBigInteger;
begin
  CheckScale(b);
  dividend := FbigInt.ShiftLeft(Fscale);
  Result := TSimpleBigDecimal.Create(dividend.Divide(b.FbigInt), Fscale);
end;

function TSimpleBigDecimal.Divide(const b: TBigInteger): TSimpleBigDecimal;
begin
  Result := TSimpleBigDecimal.Create(FbigInt.Divide(b), Fscale);
end;

function TSimpleBigDecimal.Equals(const other: TSimpleBigDecimal): Boolean;
begin
  Result := ((FbigInt.Equals(other.FbigInt)) and (Fscale = other.Fscale));
end;

function TSimpleBigDecimal.Floor: TBigInteger;
begin
  Result := FbigInt.ShiftRight(Fscale);
end;

function TSimpleBigDecimal.GetHashCode: Int32;
begin
  Result := FbigInt.GetHashCode() xor Fscale;
end;

class function TSimpleBigDecimal.GetInstance(const val: TBigInteger;
  scale: Int32): TSimpleBigDecimal;
begin
  Result := TSimpleBigDecimal.Create(val.ShiftLeft(scale), scale);
end;

function TSimpleBigDecimal.GetInt32Value: Int32;
begin
  Result := Floor().Int32Value;
end;

function TSimpleBigDecimal.GetInt64Value: Int64;
begin
  Result := Floor().Int64Value;
end;

function TSimpleBigDecimal.GetScale: Int32;
begin
  Result := Fscale;
end;

{$IFNDEF _FIXINSIGHT_}

function TSimpleBigDecimal.Multiply(const b: TSimpleBigDecimal)
  : TSimpleBigDecimal;
begin
  CheckScale(b);
  Result := TSimpleBigDecimal.Create(FbigInt.Multiply(b.FbigInt),
    Fscale + Fscale);
end;
{$ENDIF}

function TSimpleBigDecimal.Multiply(const b: TBigInteger): TSimpleBigDecimal;
begin
  Result := TSimpleBigDecimal.Create(FbigInt.Multiply(b), Fscale);
end;

function TSimpleBigDecimal.Negate: TSimpleBigDecimal;
begin
  Result := TSimpleBigDecimal.Create(FbigInt.Negate(), Fscale);
end;

function TSimpleBigDecimal.Round: TBigInteger;
var
  oneHalf: TSimpleBigDecimal;
begin
  oneHalf := TSimpleBigDecimal.Create(TBigInteger.One, 1);
  Result := Add(oneHalf.AdjustScale(Fscale)).Floor();
end;

function TSimpleBigDecimal.ShiftLeft(n: Int32): TSimpleBigDecimal;
begin
  Result := TSimpleBigDecimal.Create(FbigInt.ShiftLeft(n), Fscale);
end;

function TSimpleBigDecimal.Subtract(const b: TSimpleBigDecimal)
  : TSimpleBigDecimal;
begin
  Result := Add(b.Negate());
end;

function TSimpleBigDecimal.Subtract(const b: TBigInteger): TSimpleBigDecimal;
begin
  Result := TSimpleBigDecimal.Create(FbigInt.Subtract(b.ShiftLeft(Fscale)
    ), Fscale);
end;

function TSimpleBigDecimal.ToString: String;
var
  floorBigInt, fract: TBigInteger;
  leftOfPoint, fractStr, rightOfPoint: String;
  fractCharArr: TCryptoLibCharArray;
  fractLen, zeroes, I, j: Int32;
begin
  if (Fscale = 0) then
  begin
    Result := FbigInt.ToString();
    Exit;
  end;

  floorBigInt := Floor();

  fract := FbigInt.Subtract(floorBigInt.ShiftLeft(Fscale));
  if (FbigInt.SignValue < 0) then
  begin
    fract := TBigInteger.One.ShiftLeft(Fscale).Subtract(fract);
  end;

  if ((floorBigInt.SignValue = -1) and (not(fract.Equals(TBigInteger.Zero))))
  then
  begin
    floorBigInt := floorBigInt.Add(TBigInteger.One);
  end;
  leftOfPoint := floorBigInt.ToString();

  System.SetLength(fractCharArr, Fscale);
  fractStr := fract.ToString(2);
  fractLen := System.Length(fractStr);
  zeroes := Fscale - fractLen;
  for I := 0 to System.Pred(zeroes) do
  begin
    fractCharArr[I] := '0';
  end;
  for j := 0 to System.Pred(fractLen) do
  begin
    fractCharArr[zeroes + j] := fractStr[j + 1];
  end;

  System.SetString(rightOfPoint, PChar(@fractCharArr[0]),
    System.Length(fractCharArr));

  Result := leftOfPoint + '.' + rightOfPoint;
end;

end.
