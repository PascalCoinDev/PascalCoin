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

unit ClpBigInteger;

{$I CryptoLib.inc}

interface

uses
  Classes,
  Math,
  SysUtils,
  StrUtils,
  Generics.Collections,
  ClpISecureRandom,
  ClpIRandom,
  ClpArrayUtils,
  ClpCryptoLibTypes;

resourcestring
  SDivisionByZero = 'Division by Zero Error';
  SModulusPositive = 'Modulus must be Positive';
  SNotRelativelyPrime = 'Numbers not Relatively Prime.';
  SNegativeValue = 'Cannot be Called on Value < 0';
  SNegativeExponent = 'Negative Exponent';
  SResultTooLarge = 'Result too Large';
  SNegativeBitPosition = 'Bit Position must not be Negative';
  SInvalidBitAddress = 'Bit Address less than Zero';
  SZeroLengthBigInteger = 'Zero length BigInteger';
  SInvalidSign = 'Invalid Sign Value';
  SNegativeSizeInBits = 'sizeInBits must be non-negative';
  SInvalidBitLength = 'bitLength < 2';
  SInvalidBase = 'Only bases 2, 8, 10, or 16 allowed';
  SBadCharacterRadix8 = 'Bad Character in radix 8 string: %s';
  SBadCharacterRadix2 = 'Bad Character in radix 2 string: %s';
  SUnSupportedBase = 'Only bases 2, 8, 10, 16 are allowed';

type
{$SCOPEDENUMS ON}
  TNumberStyles = (None = 0, AllowLeadingWhite = 1, AllowTrailingWhite = 2,
    AllowLeadingSign = 4, Integer = 4 or 2 or 1, AllowTrailingSign = 8,
    AllowParentheses = 16, AllowDecimalPoint = 32, AllowThousands = 64,
    AllowExponent = 128, AllowCurrencySymbol = 256, AllowHexSpecifier = 512);
{$SCOPEDENUMS OFF}

type
  TBigInteger = record

  strict private

  const

    IMASK = Int64($FFFFFFFF);
    UIMASK = UInt64($FFFFFFFF);
    BitLengthTable: array [0 .. 255] of Byte = (0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4,
      4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6,
      6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
      6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
      7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
      8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8);

    /// <summary>
    /// These are the threshold bit-lengths (of an exponent) where we
    /// increase the window size. <br />They are calculated according to the
    /// expected savings in multiplications. <br />Some squares will also be
    /// saved on average, but we offset these against the extra storage
    /// costs. <br />
    /// </summary>
    ExpWindowThresholds: array [0 .. 7] of Int32 = (7, 25, 81, 241, 673, 1793,
      4609, Int32($7FFFFFFF));

    // TODO Parse radix-2 64 bits at a time and radix-8 63 bits at a time
    chunk2 = Int32(1);
    chunk8 = Int32(1);
    chunk10 = Int32(19);
    chunk16 = Int32(16);

    BitsPerByte = Int32(8);
    BitsPerInt = Int32(32);
    BytesPerInt = Int32(4);

  var
    // array of ints with [0] being the most significant
    Fmagnitude: TCryptoLibInt32Array;
    Fsign: Int32; // -1 means -ve; +1 means +ve; 0 means 0;
    FnBits: Int32; // cache BitCount() value
    FnBitLength: Int32; // cache BitLength() value
    // -m^(-1) mod b, b = 2^32 (see Montgomery mult.), 0 when uninitialised
    FmQuote: Int32;
    FIsInitialized: Boolean;

  class var

    FZero, FOne, FTwo, FThree, FFour, FTen: TBigInteger;
    // Each list has a product < 2^31
    FprimeLists: TCryptoLibMatrixInt32Array;
    FprimeProducts, FZeroMagnitude: TCryptoLibInt32Array;
    FZeroEncoding: TCryptoLibByteArray;
    FSMALL_CONSTANTS: TCryptoLibGenericArray<TBigInteger>;
    Fradix2, Fradix2E, Fradix8, Fradix8E, Fradix10, Fradix10E, Fradix16,
      Fradix16E: TBigInteger;
    FRandomSource: ISecureRandom;

    function GetBitLength: Int32; inline;
    function GetBitCount: Int32; inline;
    function GetInt32Value: Int32; inline;
    function GetInt64Value: Int64; inline;
    function GetIsInitialized: Boolean; inline;
    function GetSignValue: Int32; inline;

    function AddToMagnitude(const magToAdd: TCryptoLibInt32Array): TBigInteger;
    function QuickPow2Check(): Boolean; inline;
    /// <summary>
    /// return z = x / y - done in place (z value preserved, x contains the *
    /// remainder)
    /// </summary>
    function Divide(const x, y: TCryptoLibInt32Array)
      : TCryptoLibInt32Array; overload;
    function IsEqualMagnitude(const x: TBigInteger): Boolean;

    function CheckProbablePrime(certainty: Int32; const random: IRandom;
      randomlySelected: Boolean): Boolean;

    function ModInversePow2(const m: TBigInteger): TBigInteger;

    /// <summary>
    /// Calculate mQuote = -m^(-1) mod b with b = 2^32 (32 = word size)
    /// </summary>
    function GetMQuote(): Int32; inline;

    function Remainder(m: Int32): Int32; overload; inline;

    /// <summary>
    /// return x = x mod y - done in place (y value preserved)
    /// </summary>
    function Remainder(const x, y: TCryptoLibInt32Array)
      : TCryptoLibInt32Array; overload;

    function LastNBits(n: Int32): TCryptoLibInt32Array; inline;

    function DivideWords(w: Int32): TBigInteger; inline;

    function RemainderWords(w: Int32): TBigInteger; inline;

    function ToByteArray(unsigned: Boolean): TCryptoLibByteArray; overload;

    function FlipExistingBit(n: Int32): TBigInteger; inline;

    function GetLowestSetBitMaskFirst(firstWordMask: Int32): Int32;

    procedure ParseString(const str: String; radix: Int32);
    procedure ParseBytes(const bytes: TCryptoLibByteArray;
      offset, length: Int32);
    procedure ParseBytesWithSign(sign: Int32; const bytes: TCryptoLibByteArray;
      offset, length: Int32);

    class function GetZero: TBigInteger; static; inline;
    class function GetOne: TBigInteger; static; inline;
    class function GetTwo: TBigInteger; static; inline;
    class function GetThree: TBigInteger; static; inline;
    class function GetFour: TBigInteger; static; inline;
    class function GetTen: TBigInteger; static; inline;
    class function GetprimeLists: TCryptoLibMatrixInt32Array; static; inline;
    class function GetprimeProducts: TCryptoLibInt32Array; static; inline;
    class function GetRandomSource: ISecureRandom; static; inline;

    class function GetByteLength(nBits: Int32): Int32; static; inline;

    class function MakeMagnitude(const bytes: TCryptoLibByteArray;
      offset, length: Int32): TCryptoLibInt32Array; static;

    /// <summary>
    /// a = a + b - b preserved.
    /// </summary>
    class function AddMagnitudes(const a, b: TCryptoLibInt32Array)
      : TCryptoLibInt32Array; static; inline;

    class function CalcBitLength(sign, indx: Int32;
      const mag: TCryptoLibInt32Array): Int32; static;

    /// <summary>
    /// unsigned comparison on two arrays - note the arrays may start with
    /// leading zeros.
    /// </summary>
    class function CompareTo(xIndx: Int32; const x: TCryptoLibInt32Array;
      yIndx: Int32; const y: TCryptoLibInt32Array): Int32; overload; static;

    class function CompareNoLeadingZeroes(xIndx: Int32;
      const x: TCryptoLibInt32Array; yIndx: Int32;
      const y: TCryptoLibInt32Array): Int32; static;

    class function ModInverse32(d: Int32): Int32; static; inline;

    class function ModInverse64(d: Int64): Int64; static; inline;

    /// <summary>
    /// Calculate the numbers u1, u2, and u3 such that: <br />u1 * a + u2 * b
    /// = u3 <br />where u3 is the greatest common divider of a and b. <br />
    /// a and b using the extended Euclid algorithm (refer p. 323 of The Art
    /// of Computer Programming vol 2, 2nd ed). <br />This also seems to have
    /// the side effect of calculating some form of multiplicative inverse.
    /// </summary>
    /// <param name="a">
    /// First number to calculate gcd for
    /// </param>
    /// <param name="b">
    /// Second number to calculate gcd for
    /// </param>
    /// <param name="u1Out">
    /// the return object for the u1 value
    /// </param>
    /// <returns>
    /// The greatest common divisor of a and b
    /// </returns>
    class function ExtEuclid(const a, b: TBigInteger; out u1Out: TBigInteger)
      : TBigInteger; static; inline;

    class function ModPowBarrett(const b, e, m: TBigInteger)
      : TBigInteger; static;

    class function ReduceBarrett(const x: TBigInteger;
      const m, mr, yu: TBigInteger): TBigInteger; static;

    class function ModPowMonty(const b: TBigInteger; const e, m: TBigInteger;
      convert: Boolean): TBigInteger; static;

    class function GetWindowList(const mag: TCryptoLibInt32Array;
      extraBits: Int32): TCryptoLibInt32Array; static;

    class function CreateWindowEntry(mult, zeroes: Int32): Int32;
      static; inline;

    /// <returns>
    /// w with w = x * x - w is assumed to have enough space.
    /// </returns>
    class function Square(const w, x: TCryptoLibInt32Array)
      : TCryptoLibInt32Array; overload; static;

    /// <returns>
    /// x with x = y * z - x is assumed to have enough space.
    /// </returns>
    class function Multiply(const x, y, z: TCryptoLibInt32Array)
      : TCryptoLibInt32Array; overload; static;

    // mDash = -m^(-1) mod b
    class procedure MontgomeryReduce(const x, m: TCryptoLibInt32Array;
      mDash: UInt32); static;

    // mDash = -m^(-1) mod b

    /// <summary>
    /// Montgomery multiplication: a = x * y * R^(-1) mod m <br />Based
    /// algorithm 14.36 of Handbook of Applied Cryptography. <br />&lt;li&gt;
    /// m, x, y should have length n &lt;/li&gt; <br />&lt;li&gt; a should
    /// have length (n + 1) &lt;/li&gt; <br />&lt;li&gt; b = 2^32, R = b^n
    /// &lt;/li&gt; <br />&lt;br/&gt; <br />The result is put in x <br />
    /// &lt;br/&gt; <br />NOTE: the indices of x, y, m, a different in HAC
    /// and in Java <br />
    /// </summary>
    class procedure MultiplyMonty(const a, x, y, m: TCryptoLibInt32Array;
      mDash: UInt32; smallMontyModulus: Boolean); static;

    // mDash = -m^(-1) mod b
    class procedure SquareMonty(const a, x, m: TCryptoLibInt32Array;
      mDash: UInt32; smallMontyModulus: Boolean); static;

    class function MultiplyMontyNIsOne(x, y, m, mDash: UInt32): UInt32;
      static; inline;

    /// <summary>
    /// do a left shift - this returns a new array.
    /// </summary>
    class function ShiftLeft(const mag: TCryptoLibInt32Array; n: Int32)
      : TCryptoLibInt32Array; overload; static;

    /// <summary>
    /// do a right shift - this does it in place.
    /// </summary>
    class procedure ShiftRightInPlace(start: Int32;
      const mag: TCryptoLibInt32Array; n: Int32); static;

    /// <summary>
    /// do a right shift by one - this does it in place.
    /// </summary>
    class procedure ShiftRightOneInPlace(start: Int32;
      const mag: TCryptoLibInt32Array); static;

    /// <summary>
    /// returns x = x - y - we assume x is &gt;= y
    /// </summary>
    class function Subtract(xStart: Int32; const x: TCryptoLibInt32Array;
      yStart: Int32; const y: TCryptoLibInt32Array): TCryptoLibInt32Array;
      overload; static;

    class function doSubBigLil(const bigMag, lilMag: TCryptoLibInt32Array)
      : TCryptoLibInt32Array; static; inline;

    class procedure AppendZeroExtendedString(var sl: TStringList;
      const s: String; minLength: Int32); static; inline;

    class procedure ToString(var sl: TStringList; radix: Int32;
      var moduli: TList<TBigInteger>; scale: Int32; const pos: TBigInteger);
      overload; static;

    class function CreateUValueOf(value: UInt64): TBigInteger; static;
    class function CreateValueOf(value: Int64): TBigInteger; static;

    class function IntToBin(input: Int32): string; static;

    class function IntToOctal(input: Int32): string; static;

    constructor Create(signum: Int32; const mag: TCryptoLibInt32Array;
      checkMag: Boolean); overload;

    class procedure Boot(); static;
    class constructor BigInteger();

  public
    property BitLength: Int32 read GetBitLength;
    property BitCount: Int32 read GetBitCount;
    property IsInitialized: Boolean read GetIsInitialized;
    property Int32Value: Int32 read GetInt32Value;
    property Int64Value: Int64 read GetInt64Value;
    property SignValue: Int32 read GetSignValue;

    class property Zero: TBigInteger read GetZero;
    class property One: TBigInteger read GetOne;
    class property Two: TBigInteger read GetTwo;
    class property Three: TBigInteger read GetThree;
    class property Four: TBigInteger read GetFour;
    class property Ten: TBigInteger read GetTen;
    class property primeLists: TCryptoLibMatrixInt32Array read GetprimeLists;
    class property primeProducts: TCryptoLibInt32Array read GetprimeProducts;

    class property RandomSource: ISecureRandom read GetRandomSource;

    constructor Create(const value: String); overload;
    constructor Create(const str: String; radix: Int32); overload;
    constructor Create(const bytes: TCryptoLibByteArray); overload;
    constructor Create(const bytes: TCryptoLibByteArray;
      offset, length: Int32); overload;
    constructor Create(sign: Int32; const bytes: TCryptoLibByteArray); overload;
    constructor Create(sign: Int32; const bytes: TCryptoLibByteArray;
      offset, length: Int32); overload;
    constructor Create(sizeInBits: Int32; const random: IRandom); overload;
    constructor Create(BitLength, certainty: Int32;
      const random: IRandom); overload;

    function Abs(): TBigInteger;
    function Add(const value: TBigInteger): TBigInteger;
    function Subtract(const n: TBigInteger): TBigInteger; overload;
    function &And(const value: TBigInteger): TBigInteger;
    function &Not(): TBigInteger;
    function AndNot(const val: TBigInteger): TBigInteger;
    function &Or(const value: TBigInteger): TBigInteger;
    function &Xor(const value: TBigInteger): TBigInteger;

    function CompareTo(const value: TBigInteger): Int32; overload;
    function Divide(const val: TBigInteger): TBigInteger; overload;
    function DivideAndRemainder(const val: TBigInteger)
      : TCryptoLibGenericArray<TBigInteger>;
    function Gcd(const value: TBigInteger): TBigInteger;
    function Inc(): TBigInteger;

    function RabinMillerTest(certainty: Int32; const random: IRandom)
      : Boolean; overload;

    function RabinMillerTest(certainty: Int32; const random: IRandom;
      randomlySelected: Boolean): Boolean; overload;

    /// <summary>
    /// return whether or not a BigInteger is probably prime with a
    /// probability of 1 - (1/2)**certainty. <br />&lt;p&gt;From Knuth Vol 2,
    /// pg 395.&lt;/p&gt;
    /// </summary>
    function IsProbablePrime(certainty: Int32): Boolean; overload;
    function IsProbablePrime(certainty: Int32; randomlySelected: Boolean)
      : Boolean; overload;

    function Max(const value: TBigInteger): TBigInteger;
    function Min(const value: TBigInteger): TBigInteger;
    function &Mod(const m: TBigInteger): TBigInteger;
    function ModInverse(const m: TBigInteger): TBigInteger;
    function ModPow(const e, m: TBigInteger): TBigInteger;

    function Multiply(const val: TBigInteger): TBigInteger; overload;
    function Square(): TBigInteger; overload;
    function Negate(): TBigInteger;

    function NextProbablePrime(): TBigInteger;

    function Pow(exp: Int32): TBigInteger;

    function Remainder(const n: TBigInteger): TBigInteger; overload;

    function ShiftLeft(n: Int32): TBigInteger; overload;
    function ShiftRight(n: Int32): TBigInteger;

    function ToByteArray(): TCryptoLibByteArray; overload;
    function ToByteArrayUnsigned(): TCryptoLibByteArray;

    function TestBit(n: Int32): Boolean;
    function SetBit(n: Int32): TBigInteger;
    function ClearBit(n: Int32): TBigInteger;
    function FlipBit(n: Int32): TBigInteger;

    function GetLowestSetBit(): Int32;

    function ToString(): String; overload;
    function ToString(radix: Int32): String; overload;

    function Equals(const other: TBigInteger): Boolean; inline;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
    inline;
{$ENDIF DELPHI}
    class function BitCnt(i: Int32): Int32; static;

    /// <summary>
    /// BitLen(value) is the number of bits in value.
    /// </summary>
    class function BitLen(w: Int32): Int32; static;
    class function ProbablePrime(BitLength: Int32; const random: IRandom)
      : TBigInteger; static;

    class function ValueOf(value: Int64): TBigInteger; static;

    class function Arbitrary(sizeInBits: Int32): TBigInteger; static;

    class function Jacobi(const a, b: TBigInteger): Int32; static;

  end;

implementation

uses
  ClpSecureRandom; // included here to avoid circular dependency :)

{ TBigInteger }

class function TBigInteger.BitLen(w: Int32): Int32;
var
  v, t: UInt32;
begin
  v := UInt32(w);
  t := v shr 24;
  if (t <> 0) then
  begin
    Result := 24 + BitLengthTable[t];
    Exit;
  end;
  t := v shr 16;
  if (t <> 0) then
  begin
    Result := 16 + BitLengthTable[t];
    Exit;
  end;
  t := v shr 8;
  if (t <> 0) then
  begin
    Result := 8 + BitLengthTable[t];
    Exit;
  end;
  Result := BitLengthTable[v];
end;

class function TBigInteger.CalcBitLength(sign, indx: Int32;
  const mag: TCryptoLibInt32Array): Int32;
var
  BitLength, firstMag: Int32;
begin
  while True do

  begin
    if (indx >= System.length(mag)) then
    begin
      Result := 0;
      Exit;
    end;

    if (mag[indx] <> 0) then
    begin
      break;
    end;

    System.Inc(indx);
  end;

  // bit length for everything after the first int
  BitLength := 32 * ((System.length(mag) - indx) - 1);

  // and determine bitlength of first int
  firstMag := mag[indx];
  BitLength := BitLength + BitLen(firstMag);

  // Check for negative powers of two
  if ((sign < 0) and ((firstMag and Int32(-firstMag)) = firstMag)) then
  begin
    repeat
      System.Inc(indx);
      if (indx >= System.length(mag)) then
      begin
        System.Dec(BitLength);
        break;
      end;
    until (not(mag[indx] = 0));
  end;

  Result := BitLength;
end;

class function TBigInteger.GetZero: TBigInteger;
begin
  Result := FZero;
end;

class function TBigInteger.GetOne: TBigInteger;
begin
  Result := FOne;
end;

class function TBigInteger.GetTwo: TBigInteger;
begin
  Result := FTwo;
end;

class function TBigInteger.GetThree: TBigInteger;
begin
  Result := FThree;
end;

class function TBigInteger.GetFour: TBigInteger;
begin
  Result := FFour;
end;

class function TBigInteger.GetTen: TBigInteger;
begin
  Result := FTen;
end;

class function TBigInteger.GetprimeLists: TCryptoLibMatrixInt32Array;
begin
  Result := FprimeLists;
end;

class function TBigInteger.GetprimeProducts: TCryptoLibInt32Array;
begin
  Result := FprimeProducts;
end;

class function TBigInteger.GetRandomSource: ISecureRandom;
begin
  Result := FRandomSource;
end;

function TBigInteger.GetSignValue: Int32;
begin
  Result := Fsign;
end;

function TBigInteger.GetBitLength: Int32;
begin
  if (FnBitLength = -1) then
  begin
    if Fsign = 0 then
    begin
      FnBitLength := 0;
    end
    else
    begin
      FnBitLength := CalcBitLength(Fsign, 0, Fmagnitude);
    end;

  end;
  Result := FnBitLength;
end;

function TBigInteger.GetInt32Value: Int32;
var
  n, v: Int32;
begin
  if (Fsign = 0) then
  begin
    Result := 0;
    Exit;
  end;

  n := System.length(Fmagnitude);

  v := Fmagnitude[n - 1];

  if Fsign < 0 then
  begin
    Result := -v;
  end
  else
  begin
    Result := v;
  end;
end;

function TBigInteger.GetInt64Value: Int64;
var
  n: Int32;
  v: Int64;
begin
  if (Fsign = 0) then
  begin
    Result := 0;
    Exit;
  end;

  n := System.length(Fmagnitude);

  v := Fmagnitude[n - 1] and IMASK;
  if (n > 1) then
  begin
    v := v or ((Fmagnitude[n - 2] and IMASK) shl 32);
  end;

  if Fsign < 0 then

  begin
    Result := -v;
    Exit;
  end
  else
  begin
    Result := v;
    Exit;
  end;

end;

function TBigInteger.GetIsInitialized: Boolean;
begin
  Result := FIsInitialized;
end;

class function TBigInteger.BitCnt(i: Int32): Int32;
var
  u: UInt32;
begin
  u := UInt32(i);
{$IFDEF FPC}
  Result := Int32(PopCnt(u));
{$ELSE}
  u := u - ((u shr 1) and $55555555);
  u := (u and $33333333) + ((u shr 2) and $33333333);
  u := (u + (u shr 4)) and $0F0F0F0F;
  u := u + (u shr 8);
  u := u + (u shr 16);
  u := u and $3F;
  Result := Int32(u);
{$ENDIF FPC}
end;

class function TBigInteger.CreateWindowEntry(mult, zeroes: Int32): Int32;
begin
  while ((mult and 1) = 0) do
  begin
    mult := mult shr 1;
    System.Inc(zeroes);
  end;

  Result := mult or (zeroes shl 8);
end;

class function TBigInteger.GetByteLength(nBits: Int32): Int32;
begin
  Result := (nBits + BitsPerByte - 1) div BitsPerByte;
end;

function TBigInteger.LastNBits(n: Int32): TCryptoLibInt32Array;
var
  numWords, excessBits: Int32;
begin
  if (n < 1) then
  begin
    Result := FZeroMagnitude;
    Exit;
  end;

  numWords := (n + BitsPerInt - 1) div BitsPerInt;
  numWords := Math.Min(numWords, System.length(Fmagnitude));
  System.SetLength(Result, numWords);
  System.Move(Fmagnitude[System.length(Fmagnitude) - numWords], Result[0],
    numWords * System.SizeOf(Int32));

  excessBits := (numWords shl 5) - n;
  if (excessBits > 0) then
  begin
    Result[0] := Result[0] and (Int32(System.High(UInt32) shr excessBits));
  end;

end;

function TBigInteger.CompareTo(const value: TBigInteger): Int32;
begin

  if Fsign < value.Fsign then
  begin
    Result := -1;
  end
  else
  begin
    if Fsign > value.Fsign then
    begin
      Result := 1;
    end
    else
    begin
      if Fsign = 0 then
      begin
        Result := 0
      end
      else
      begin
        Result := Fsign * CompareNoLeadingZeroes(0, Fmagnitude, 0,
          value.Fmagnitude);
      end;
    end;

  end;

end;

class function TBigInteger.CreateValueOf(value: Int64): TBigInteger;
begin
  if (value < 0) then
  begin
    if (value = System.Low(Int64)) then
    begin
      Result := CreateValueOf(not value).&Not();
      Exit;
    end;

    Result := CreateValueOf(-value).Negate();
    Exit;
  end;

  Result := CreateUValueOf(UInt64(value));
end;

class function TBigInteger.CreateUValueOf(value: UInt64): TBigInteger;
var
  msw, lsw: Int32;
begin
  msw := Int32(value shr 32);
  lsw := Int32(value);

  if (msw <> 0) then
  begin
    Result := TBigInteger.Create(1, TCryptoLibInt32Array.Create(msw,
      lsw), false);
    Exit;
  end;

  if (lsw <> 0) then
  begin
    Result := TBigInteger.Create(1, TCryptoLibInt32Array.Create(lsw), false);
    // Check for a power of two

    if ((lsw and -lsw) = lsw) then
    begin
      Result.FnBits := 1;
    end;
    Exit;
  end;

  Result := Zero;
end;

class function TBigInteger.ValueOf(value: Int64): TBigInteger;
begin
  if ((value >= 0) and (value < System.length(FSMALL_CONSTANTS))) then
  begin
    Result := FSMALL_CONSTANTS[value];
    Exit;
  end;

  Result := CreateValueOf(value);
end;

class procedure TBigInteger.Boot;
var
  i: UInt32;
  primeList: TCryptoLibInt32Array;
  product, j: Int32;
begin

  System.SetLength(FZeroEncoding, 0);
  System.SetLength(FZeroMagnitude, 0);
  FprimeLists := TCryptoLibMatrixInt32Array.Create
    (TCryptoLibInt32Array.Create(3, 5, 7, 11, 13, 17, 19, 23),
    TCryptoLibInt32Array.Create(29, 31, 37, 41, 43),
    TCryptoLibInt32Array.Create(47, 53, 59, 61, 67),
    TCryptoLibInt32Array.Create(71, 73, 79, 83), TCryptoLibInt32Array.Create(89,
    97, 101, 103),

    TCryptoLibInt32Array.Create(107, 109, 113, 127),
    TCryptoLibInt32Array.Create(131, 137, 139, 149),
    TCryptoLibInt32Array.Create(151, 157, 163, 167),
    TCryptoLibInt32Array.Create(173, 179, 181, 191),
    TCryptoLibInt32Array.Create(193, 197, 199, 211),

    TCryptoLibInt32Array.Create(223, 227, 229), TCryptoLibInt32Array.Create(233,
    239, 241), TCryptoLibInt32Array.Create(251, 257, 263),
    TCryptoLibInt32Array.Create(269, 271, 277), TCryptoLibInt32Array.Create(281,
    283, 293),

    TCryptoLibInt32Array.Create(307, 311, 313), TCryptoLibInt32Array.Create(317,
    331, 337), TCryptoLibInt32Array.Create(347, 349, 353),
    TCryptoLibInt32Array.Create(359, 367, 373), TCryptoLibInt32Array.Create(379,
    383, 389),

    TCryptoLibInt32Array.Create(397, 401, 409), TCryptoLibInt32Array.Create(419,
    421, 431), TCryptoLibInt32Array.Create(433, 439, 443),
    TCryptoLibInt32Array.Create(449, 457, 461), TCryptoLibInt32Array.Create(463,
    467, 479),

    TCryptoLibInt32Array.Create(487, 491, 499), TCryptoLibInt32Array.Create(503,
    509, 521), TCryptoLibInt32Array.Create(523, 541, 547),
    TCryptoLibInt32Array.Create(557, 563, 569), TCryptoLibInt32Array.Create(571,
    577, 587),

    TCryptoLibInt32Array.Create(593, 599, 601), TCryptoLibInt32Array.Create(607,
    613, 617), TCryptoLibInt32Array.Create(619, 631, 641),
    TCryptoLibInt32Array.Create(643, 647, 653), TCryptoLibInt32Array.Create(659,
    661, 673),

    TCryptoLibInt32Array.Create(677, 683, 691), TCryptoLibInt32Array.Create(701,
    709, 719), TCryptoLibInt32Array.Create(727, 733, 739),
    TCryptoLibInt32Array.Create(743, 751, 757), TCryptoLibInt32Array.Create(761,
    769, 773),

    TCryptoLibInt32Array.Create(787, 797, 809), TCryptoLibInt32Array.Create(811,
    821, 823), TCryptoLibInt32Array.Create(827, 829, 839),
    TCryptoLibInt32Array.Create(853, 857, 859), TCryptoLibInt32Array.Create(863,
    877, 881),

    TCryptoLibInt32Array.Create(883, 887, 907), TCryptoLibInt32Array.Create(911,
    919, 929), TCryptoLibInt32Array.Create(937, 941, 947),
    TCryptoLibInt32Array.Create(953, 967, 971), TCryptoLibInt32Array.Create(977,
    983, 991),

    TCryptoLibInt32Array.Create(997, 1009, 1013),
    TCryptoLibInt32Array.Create(1019, 1021, 1031),
    TCryptoLibInt32Array.Create(1033, 1039, 1049),
    TCryptoLibInt32Array.Create(1051, 1061, 1063),
    TCryptoLibInt32Array.Create(1069, 1087, 1091),

    TCryptoLibInt32Array.Create(1093, 1097, 1103),
    TCryptoLibInt32Array.Create(1109, 1117, 1123),
    TCryptoLibInt32Array.Create(1129, 1151, 1153),
    TCryptoLibInt32Array.Create(1163, 1171, 1181),
    TCryptoLibInt32Array.Create(1187, 1193, 1201),

    TCryptoLibInt32Array.Create(1213, 1217, 1223),
    TCryptoLibInt32Array.Create(1229, 1231, 1237),
    TCryptoLibInt32Array.Create(1249, 1259, 1277),
    TCryptoLibInt32Array.Create(1279, 1283, 1289));

  // !!! Only Remove when we are able to move "ClpSecureRandom" to the
  // interface uses section of this unit. !!!
  TSecureRandom.Boot;

  FRandomSource := TSecureRandom.Create();

  FZero := TBigInteger.Create(0, FZeroMagnitude, false);
  FZero.FnBits := 0;
  FZero.FnBitLength := 0;

  System.SetLength(FSMALL_CONSTANTS, 17);

  FSMALL_CONSTANTS[0] := FZero;

  i := 1;

  while i < UInt32(System.length(FSMALL_CONSTANTS)) do
  begin
    FSMALL_CONSTANTS[i] := CreateUValueOf(i);
    System.Inc(i);
  end;

  FOne := FSMALL_CONSTANTS[1];
  FTwo := FSMALL_CONSTANTS[2];
  FThree := FSMALL_CONSTANTS[3];
  FFour := FSMALL_CONSTANTS[4];
  FTen := FSMALL_CONSTANTS[10];

  Fradix2 := ValueOf(2);
  Fradix2E := Fradix2.Pow(chunk2);

  Fradix8 := ValueOf(8);
  Fradix8E := Fradix8.Pow(chunk8);

  Fradix10 := ValueOf(10);

  Fradix10E := Fradix10.Pow(chunk10);

  Fradix16 := ValueOf(16);
  Fradix16E := Fradix16.Pow(chunk16);

  System.SetLength(FprimeProducts, System.length(primeLists));

  for i := 0 to System.Pred(System.length(primeLists)) do
  begin
    primeList := primeLists[i];
    product := primeList[0];
    for j := 1 to System.Pred(System.length(primeList)) do
    begin
      product := product * primeList[j];
    end;

    FprimeProducts[i] := product;
  end;

end;

function TBigInteger.QuickPow2Check: Boolean;
begin
  Result := (Fsign > 0) and (FnBits = 1);
end;

function TBigInteger.Negate: TBigInteger;
begin
  if (Fsign = 0) then
  begin
    Result := Self;
    Exit;
  end;

  Result := TBigInteger.Create(-Fsign, Fmagnitude, false);
end;

class function TBigInteger.doSubBigLil(const bigMag,
  lilMag: TCryptoLibInt32Array): TCryptoLibInt32Array;
var
  res: TCryptoLibInt32Array;
begin
  res := System.Copy(bigMag);

  Result := Subtract(0, res, 0, lilMag);
end;

function TBigInteger.Inc: TBigInteger;
begin
  if (Fsign = 0) then
  begin
    Result := One;
    Exit;
  end;

  if (Fsign < 0) then
  begin
    Result := TBigInteger.Create(-1, doSubBigLil(Fmagnitude,
      One.Fmagnitude), True);
    Exit;
  end;

  Result := AddToMagnitude(One.Fmagnitude);
end;

class function TBigInteger.IntToBin(input: Int32): string;
var
  bits: TCryptoLibCharArray;
  i: Int32;
begin

  Result := '';

  System.SetLength(bits, System.SizeOf(Int32) * 8);

  i := 0;

  while (input <> 0) do
  begin
    if (input and 1) = 1 then
    begin
      bits[i] := '1'
    end
    else
    begin
      bits[i] := '0';
    end;
    System.Inc(i);
    input := input shr 1;
  end;
  System.SetString(Result, PChar(@bits[0]), i);

  Result := ReverseString(Result);

end;

class function TBigInteger.IntToOctal(input: Int32): string;
var
  bits: TCryptoLibCharArray;
  i: Int32;
begin

  Result := '';

  System.SetLength(bits, System.SizeOf(Int32) * 8);

  i := 0;

  while (input <> 0) do
  begin
    case (input and 7) of
      0:
        bits[i] := '0';
      1:
        bits[i] := '1';
      2:
        bits[i] := '2';
      3:
        bits[i] := '3';
      4:
        bits[i] := '4';
      5:
        bits[i] := '5';
      6:
        bits[i] := '6';
      7:
        bits[i] := '7';
    end;
    System.Inc(i);
    input := input shr 3;
  end;

  System.SetString(Result, PChar(@bits[0]), i);

  Result := ReverseString(Result);

end;

function TBigInteger.&Not: TBigInteger;
begin
  Result := Inc().Negate();
end;

function TBigInteger.TestBit(n: Int32): Boolean;
var
  wordNum, word: Int32;
begin
  if (n < 0) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SNegativeBitPosition);
  end;

  if (Fsign < 0) then
  begin
    Result := (not &Not().TestBit(n));
    Exit;
  end;

  wordNum := n div 32;
  if (wordNum >= System.length(Fmagnitude)) then
  begin
    Result := false;
    Exit;
  end;

  word := Fmagnitude[System.length(Fmagnitude) - 1 - wordNum];
  Result := ((word shr (n and 31)) and 1) > 0;
end;

function TBigInteger.Abs: TBigInteger;
begin
  if Fsign >= 0 then
  begin
    Result := Self;
  end
  else
  begin
    Result := Negate();
  end;
end;

function TBigInteger.Square: TBigInteger;
var
  resLength: Int32;
  res: TCryptoLibInt32Array;
begin
  if (Fsign = 0) then
  begin
    Result := Zero;
    Exit;
  end;
  if (QuickPow2Check()) then
  begin
    Result := ShiftLeft(Abs().BitLength - 1);
    Exit;
  end;
  resLength := System.length(Fmagnitude) shl 1;

  if (UInt32(Fmagnitude[0]) shr 16 = 0) then
  begin
    System.Dec(resLength);
  end;
  System.SetLength(res, resLength);
  Square(res, Fmagnitude);
  Result := TBigInteger.Create(1, res, false);
end;

function TBigInteger.Add(const value: TBigInteger): TBigInteger;
begin
  if (Fsign = 0) then
  begin
    Result := value;
    Exit;
  end;

  if (Fsign <> value.Fsign) then
  begin
    if (value.Fsign = 0) then
    begin
      Result := Self;
      Exit;
    end;

    if (value.Fsign < 0) then
    begin
      Result := Subtract(value.Negate());
      Exit;
    end;

    Result := value.Subtract(Negate());
    Exit;
  end;

  Result := AddToMagnitude(value.Fmagnitude);
end;

class function TBigInteger.AddMagnitudes(const a, b: TCryptoLibInt32Array)
  : TCryptoLibInt32Array;
var
  tI, vI: Int32;
  m: Int64;
begin
  tI := System.length(a) - 1;
  vI := System.length(b) - 1;
  m := 0;

  while (vI >= 0) do
  begin
    m := m + (Int64(UInt32(a[tI])) + Int64(UInt32(b[vI])));
    System.Dec(vI);
    a[tI] := Int32(m);
    System.Dec(tI);
    m := Int64(UInt64(m shr 32));
  end;

  if (m <> 0) then
  begin
    while (tI >= 0) do
    begin

      a[tI] := a[tI] + 1;

      if (a[tI] <> 0) then
      begin
        break;
      end;
      System.Dec(tI);
    end;
  end;

  Result := a;
end;

function TBigInteger.AddToMagnitude(const magToAdd: TCryptoLibInt32Array)
  : TBigInteger;
var
  big, small, bigCopy: TCryptoLibInt32Array;
  limit: UInt32;
  possibleOverflow: Boolean;
begin
  if (System.length(Fmagnitude) < System.length(magToAdd)) then
  begin
    big := magToAdd;
    small := Fmagnitude;
  end
  else
  begin
    big := Fmagnitude;
    small := magToAdd;
  end;

  // Conservatively avoid over-allocation when no overflow possible
  limit := System.High(UInt32);
  if (System.length(big) = System.length(small)) then
  begin
    limit := limit - UInt32(small[0]);
  end;

  possibleOverflow := UInt32(big[0]) >= limit;

  if (possibleOverflow) then
  begin
    System.SetLength(bigCopy, System.length(big) + 1);
    System.Move(big[0], bigCopy[1], System.length(big) * System.SizeOf(Int32));
  end
  else
  begin
    bigCopy := System.Copy(big);
  end;

  bigCopy := AddMagnitudes(bigCopy, small);

  Result := TBigInteger.Create(Fsign, bigCopy, possibleOverflow);
end;

function TBigInteger.&And(const value: TBigInteger): TBigInteger;
var
  aMag, bMag, resultMag: TCryptoLibInt32Array;
  resultNeg: Boolean;
  resultLength, aStart, bStart, i, aWord, bWord: Int32;
begin
  if ((Fsign = 0) or (value.Fsign = 0)) then
  begin
    Result := Zero;
    Exit;
  end;

  if Fsign > 0 then
  begin
    aMag := Fmagnitude;
  end
  else
  begin
    aMag := Add(One).Fmagnitude;
  end;

  if value.Fsign > 0 then
  begin
    bMag := value.Fmagnitude;
  end
  else
  begin
    bMag := value.Add(One).Fmagnitude;
  end;

  resultNeg := (Fsign < 0) and (value.Fsign < 0);
  resultLength := Math.Max(System.length(aMag), System.length(bMag));

  System.SetLength(resultMag, resultLength);

  aStart := System.length(resultMag) - System.length(aMag);
  bStart := System.length(resultMag) - System.length(bMag);

  for i := 0 to System.Pred(System.length(resultMag)) do

  begin

    if i >= aStart then
    begin
      aWord := aMag[i - aStart];
    end
    else
    begin
      aWord := 0;
    end;

    if i >= bStart then
    begin
      bWord := bMag[i - bStart];
    end
    else
    begin
      bWord := 0;
    end;

    if (Fsign < 0) then
    begin
      aWord := not aWord;
    end;

    if (value.Fsign < 0) then
    begin
      bWord := not bWord;
    end;

    resultMag[i] := aWord and bWord;

    if (resultNeg) then
    begin
      resultMag[i] := not resultMag[i];
    end;
  end;

  Result := TBigInteger.Create(1, resultMag, True);

  // TODO Optimise this case
  if (resultNeg) then
  begin
    Result := Result.&Not();
  end;

end;

function TBigInteger.AndNot(const val: TBigInteger): TBigInteger;
begin
  Result := &And(val.&Not());
end;

class procedure TBigInteger.AppendZeroExtendedString(var sl: TStringList;
  const s: String; minLength: Int32);
var
  len: Int32;
begin
  len := System.length(s);
  while len < minLength do
  begin
    sl.Add('0');
    System.Inc(len);
  end;
  sl.Add(s);
end;

class procedure TBigInteger.ToString(var sl: TStringList; radix: Int32;
  var moduli: TList<TBigInteger>; scale: Int32; const pos: TBigInteger);
var
  s: String;
  qr: TCryptoLibGenericArray<TBigInteger>;
begin
  if (pos.BitLength < 64) then
  begin
    s := IntToStr(pos.Int64Value);
    if ((sl.Count > 1) or ((sl.Count = 1) and (sl[0] <> '-'))) then
    begin
      AppendZeroExtendedString(sl, s, 1 shl scale);
    end
    else if (pos.SignValue <> 0) then
    begin
      sl.Append(s);
    end;
    Exit;
  end;

  System.Dec(scale);
  qr := pos.DivideAndRemainder(moduli[scale]);

  ToString(sl, radix, moduli, scale, qr[0]);
  ToString(sl, radix, moduli, scale, qr[1]);
end;

class function TBigInteger.Arbitrary(sizeInBits: Int32): TBigInteger;
begin
  Result := TBigInteger.Create(sizeInBits, RandomSource);
end;

function TBigInteger.Remainder(m: Int32): Int32;
var
  acc, posVal: Int64;
  &pos: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(m > 0);
{$ENDIF DEBUG}
  acc := 0;
  for pos := 0 to System.Pred(System.length(Fmagnitude)) do
  begin
    posVal := UInt32(Fmagnitude[pos]);
    acc := ((acc shl 32) or posVal) mod m;
  end;

  Result := Int32(acc);
end;

function TBigInteger.Remainder(const n: TBigInteger): TBigInteger;
var
  val, rem: Int32;
  tempRes: TCryptoLibInt32Array;
begin
  if (n.Fsign = 0) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SDivisionByZero);
  end;

  if (Fsign = 0) then
  begin
    Result := Zero;
    Exit;
  end;

  // For small values, use fast remainder method
  if (System.length(n.Fmagnitude) = 1) then
  begin
    val := n.Fmagnitude[0];

    if (val > 0) then
    begin
      if (val = 1) then
      begin
        Result := Zero;
        Exit;
      end;

      // TODO Make this func work on uint, and handle val == 1?
      rem := Remainder(val);

      if rem = 0 then
      begin
        Result := Zero;
        Exit;
      end
      else
      begin
        Result := TBigInteger.Create(Fsign,
          TCryptoLibInt32Array.Create(rem), false);
        Exit;
      end;

    end;
  end;

  if (CompareNoLeadingZeroes(0, Fmagnitude, 0, n.Fmagnitude) < 0) then
  begin
    Result := Self;
    Exit;
  end;

  if (n.QuickPow2Check()) then // n is power of two
  begin
    // TODO Move before small values branch above?
    tempRes := LastNBits(n.Abs().BitLength - 1);
  end
  else
  begin
    tempRes := System.Copy(Fmagnitude);
    tempRes := Remainder(tempRes, n.Fmagnitude);
  end;

  Result := TBigInteger.Create(Fsign, tempRes, True);
end;

function TBigInteger.&Mod(const m: TBigInteger): TBigInteger;
var
  biggie: TBigInteger;
begin
  if (m.Fsign < 1) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SModulusPositive);
  end;

  biggie := Remainder(m);

  if biggie.Fsign >= 0 then
  begin
    Result := biggie;
  end
  else
  begin
    Result := biggie.Add(m);
  end;
end;

function TBigInteger.&Or(const value: TBigInteger): TBigInteger;
var
  aMag, bMag, resultMag: TCryptoLibInt32Array;
  resultNeg: Boolean;
  resultLength, aStart, bStart, i, aWord, bWord: Int32;
begin
  if (Fsign = 0) then
  begin
    Result := value;
    Exit;
  end;

  if (value.Fsign = 0) then
  begin
    Result := Self;
    Exit;
  end;

  if Fsign > 0 then
  begin
    aMag := Fmagnitude;
  end
  else
  begin
    aMag := Add(One).Fmagnitude;
  end;

  if value.Fsign > 0 then
  begin
    bMag := value.Fmagnitude;
  end
  else
  begin
    bMag := value.Add(One).Fmagnitude;
  end;

  resultNeg := (Fsign < 0) or (value.Fsign < 0);
  resultLength := Math.Max(System.length(aMag), System.length(bMag));

  System.SetLength(resultMag, resultLength);

  aStart := System.length(resultMag) - System.length(aMag);
  bStart := System.length(resultMag) - System.length(bMag);

  for i := 0 to System.Pred(System.length(resultMag)) do

  begin

    if i >= aStart then
    begin
      aWord := aMag[i - aStart];
    end
    else
    begin
      aWord := 0;
    end;

    if i >= bStart then
    begin
      bWord := bMag[i - bStart];
    end
    else
    begin
      bWord := 0;
    end;

    if (Fsign < 0) then
    begin
      aWord := not aWord;
    end;

    if (value.Fsign < 0) then
    begin
      bWord := not bWord;
    end;

    resultMag[i] := aWord or bWord;

    if (resultNeg) then
    begin
      resultMag[i] := not resultMag[i];
    end;
  end;

  Result := TBigInteger.Create(1, resultMag, True);

  // TODO Optimise this case
  if (resultNeg) then
  begin
    Result := Result.&Not();
  end;

end;

function TBigInteger.&Xor(const value: TBigInteger): TBigInteger;
var
  aMag, bMag, resultMag: TCryptoLibInt32Array;
  resultNeg: Boolean;
  resultLength, aStart, bStart, i, aWord, bWord: Int32;
begin
  if (Fsign = 0) then
  begin
    Result := value;
    Exit;
  end;

  if (value.Fsign = 0) then
  begin
    Result := Self;
    Exit;
  end;

  if Fsign > 0 then
  begin
    aMag := Fmagnitude;
  end
  else
  begin
    aMag := Add(One).Fmagnitude;
  end;

  if value.Fsign > 0 then
  begin
    bMag := value.Fmagnitude;
  end
  else
  begin
    bMag := value.Add(One).Fmagnitude;
  end;
  // TODO Can just replace with sign != value.sign?
  resultNeg := ((Fsign < 0) and (value.Fsign >= 0)) or
    ((Fsign >= 0) and (value.Fsign < 0));
  resultLength := Math.Max(System.length(aMag), System.length(bMag));

  System.SetLength(resultMag, resultLength);

  aStart := System.length(resultMag) - System.length(aMag);
  bStart := System.length(resultMag) - System.length(bMag);

  for i := 0 to System.Pred(System.length(resultMag)) do

  begin

    if i >= aStart then
    begin
      aWord := aMag[i - aStart];
    end
    else
    begin
      aWord := 0;
    end;

    if i >= bStart then
    begin
      bWord := bMag[i - bStart];
    end
    else
    begin
      bWord := 0;
    end;

    if (Fsign < 0) then
    begin
      aWord := not aWord;
    end;

    if (value.Fsign < 0) then
    begin
      bWord := not bWord;
    end;

    resultMag[i] := aWord xor bWord;

    if (resultNeg) then
    begin
      resultMag[i] := not resultMag[i];
    end;
  end;

  Result := TBigInteger.Create(1, resultMag, True);

  // TODO Optimise this case
  if (resultNeg) then
  begin
    Result := Result.&Not();
  end;

end;

class constructor TBigInteger.BigInteger;
begin
  TBigInteger.Boot;
end;

constructor TBigInteger.Create(const value: String);
begin
  ParseString(value, 10);
end;

constructor TBigInteger.Create(const str: String; radix: Int32);
begin
  ParseString(str, radix);
end;

constructor TBigInteger.Create(const bytes: TCryptoLibByteArray);
begin
  ParseBytes(bytes, 0, System.length(bytes));
end;

constructor TBigInteger.Create(sign: Int32; const bytes: TCryptoLibByteArray);
begin
  ParseBytesWithSign(sign, bytes, 0, System.length(bytes));
end;

constructor TBigInteger.Create(const bytes: TCryptoLibByteArray;
  offset, length: Int32);
begin
  ParseBytes(bytes, offset, length);
end;

constructor TBigInteger.Create(sign: Int32; const bytes: TCryptoLibByteArray;
  offset, length: Int32);
begin
  ParseBytesWithSign(sign, bytes, offset, length);
end;

constructor TBigInteger.Create(BitLength, certainty: Int32;
  const random: IRandom);
var
  nBytes, xBits, j: Int32;
  mask, lead: Byte;
  b: TCryptoLibByteArray;
begin
  if (BitLength < 2) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SInvalidBitLength);
  end;

  Fsign := 1;
  FnBits := -1;
  FnBitLength := BitLength;
  FmQuote := 0;
  FIsInitialized := True;

  if (BitLength = 2) then
  begin
    if (random.Next(2) = 0) then
    begin
      Fmagnitude := Two.Fmagnitude
    end
    else
    begin
      Fmagnitude := Three.Fmagnitude
    end;
    Exit;

  end;

  nBytes := GetByteLength(BitLength);
  System.SetLength(b, nBytes);

  xBits := (BitsPerByte * nBytes) - BitLength;
  mask := Byte(UInt32(255) shr xBits);
  lead := Byte(1 shl (7 - xBits));

  while True do
  begin
    random.NextBytes(b);

    // strip off any excess bits in the MSB
    b[0] := b[0] and mask;

    // ensure the leading bit is 1 (to meet the strength requirement)
    b[0] := b[0] or lead;

    // ensure the trailing bit is 1 (i.e. must be odd)
    b[nBytes - 1] := b[nBytes - 1] or 1;

    Fmagnitude := MakeMagnitude(b, 0, System.length(b));
    FnBits := -1;
    FmQuote := 0;

    if (certainty < 1) then
    begin
      break;
    end;

    if (CheckProbablePrime(certainty, random, True)) then
    begin
      break;
    end;

    j := 1;

    while j < (System.length(Fmagnitude) - 1) do

    begin
      Fmagnitude[j] := Fmagnitude[j] xor random.Next();

      if (CheckProbablePrime(certainty, random, True)) then
      begin
        Exit;
      end;
      System.Inc(j);
    end;

  end;

end;

constructor TBigInteger.Create(sizeInBits: Int32; const random: IRandom);
var
  nBytes, xBits: Int32;
  b: TCryptoLibByteArray;
begin
  if (sizeInBits < 0) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SNegativeSizeInBits);
  end;

  Fsign := -1;
  FnBits := -1;
  FnBitLength := -1;
  FmQuote := 0;
  FIsInitialized := True;

  if (sizeInBits = 0) then
  begin
    Fsign := 0;
    Fmagnitude := FZeroMagnitude;
    Exit;
  end;

  nBytes := GetByteLength(sizeInBits);
  System.SetLength(b, nBytes);
  random.NextBytes(b);

  // strip off any excess bits in the MSB
  xBits := (BitsPerByte * nBytes) - sizeInBits;
  b[0] := b[0] and Byte(UInt32(255) shr xBits);

  Fmagnitude := MakeMagnitude(b, 0, System.length(b));

  if System.length(Fmagnitude) < 1 then
  begin
    Fsign := 0;
  end
  else
  begin
    Fsign := 1;
  end;

end;

constructor TBigInteger.Create(signum: Int32; const mag: TCryptoLibInt32Array;
  checkMag: Boolean);
var
  i: Int32;
begin

  Fsign := -1;
  FnBits := -1;
  FnBitLength := -1;
  FmQuote := 0;
  FIsInitialized := True;

  if (checkMag) then
  begin
    i := 0;
    while ((i < System.length(mag)) and (mag[i] = 0)) do
    begin
      System.Inc(i);
    end;

    if (i = System.length(mag)) then
    begin
      Fsign := 0;
      Fmagnitude := FZeroMagnitude;
    end
    else
    begin
      Fsign := signum;

      if (i = 0) then
      begin
        Fmagnitude := mag;
      end
      else
      begin
        // strip leading 0 words
        System.SetLength(Fmagnitude, System.length(mag) - i);
        System.Move(mag[i], Fmagnitude[0], System.length(Fmagnitude) *
          System.SizeOf(Int32));
      end
    end;
  end
  else
  begin
    Fsign := signum;
    Fmagnitude := mag;
  end;

end;

function TBigInteger.Equals(const other: TBigInteger): Boolean;
begin
  Result := (Fsign = other.Fsign) and IsEqualMagnitude(other);
end;

class function TBigInteger.ExtEuclid(const a, b: TBigInteger;
  out u1Out: TBigInteger): TBigInteger;
var
  u1, v1, u3, v3, oldU1: TBigInteger;
  q: TCryptoLibGenericArray<TBigInteger>;
begin
  u1 := One;
  v1 := Zero;
  u3 := a;
  v3 := b;

  if (v3.Fsign > 0) then
  begin
    while True do

    begin
      q := u3.DivideAndRemainder(v3);
      u3 := v3;
      v3 := q[1];

      oldU1 := u1;
      u1 := v1;

      if (v3.Fsign <= 0) then
      begin
        break;
      end;

      v1 := oldU1.Subtract(v1.Multiply(q[0]));
    end;
  end;

  u1Out := u1;

  Result := u3;
end;

function TBigInteger.FlipExistingBit(n: Int32): TBigInteger;
var
  mag: TCryptoLibInt32Array;
begin
{$IFDEF DEBUG}
  System.Assert(Fsign > 0);
  System.Assert(n >= 0);
  System.Assert(n < BitLength - 1);
{$ENDIF DEBUG}
  mag := System.Copy(Fmagnitude);
  mag[System.length(mag) - 1 - (n shr 5)] :=
    mag[System.length(mag) - 1 - (n shr 5)] xor (1 shl (n and 31));
  // Flip bit
  // mag[mag.Length - 1 - (n / 32)] ^= (1 << (n % 32));
  Result := TBigInteger.Create(Fsign, mag, false);
end;

function TBigInteger.FlipBit(n: Int32): TBigInteger;
begin
  if (n < 0) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SInvalidBitAddress);
  end;

  // TODO Handle negative values and zero
  if ((Fsign > 0) and (n < (BitLength - 1))) then
  begin
    Result := FlipExistingBit(n);
    Exit;
  end;

  Result := &Xor(One.ShiftLeft(n));
end;

function TBigInteger.Gcd(const value: TBigInteger): TBigInteger;
var
  r, u, v: TBigInteger;
begin
  if (value.Fsign = 0) then
  begin
    Result := Abs();
    Exit;
  end;

  if (Fsign = 0) then
  begin
    Result := value.Abs();
    Exit;
  end;

  u := Self;
  v := value;

  while (v.Fsign <> 0) do
  begin
    r := u.&Mod(v);
    u := v;
    v := r;
  end;

  Result := u;
end;

function TBigInteger.GetBitCount: Int32;
var
  sum, i: Int32;
begin
  if (FnBits = -1) then
  begin
    if (Fsign < 0) then
    begin
      // TODO Optimise this case
      FnBits := &Not().BitCount;
    end
    else
    begin
      sum := 0;
      for i := 0 to System.Pred(System.length(Fmagnitude)) do
      begin
        sum := sum + BitCnt(Fmagnitude[i]);
      end;
      FnBits := sum;
    end;
  end;

  Result := FnBits;
end;

function TBigInteger.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}

var
  hc: Int32;
begin
  hc := System.length(Fmagnitude);
  if (System.length(Fmagnitude) > 0) then
  begin
    hc := hc xor Fmagnitude[0];

    if (System.length(Fmagnitude) > 1) then
    begin
      hc := hc xor Fmagnitude[System.length(Fmagnitude) - 1];
    end;
  end;

  if Fsign < 0 then
  begin
    Result := not hc;
  end
  else
  begin
    Result := hc;
  end;

end;

function TBigInteger.GetLowestSetBit: Int32;
begin
  if (Fsign = 0) then
  begin
    Result := -1;
    Exit;
  end;

  Result := GetLowestSetBitMaskFirst(-1);
end;

function TBigInteger.GetLowestSetBitMaskFirst(firstWordMask: Int32): Int32;
var
  w, offset: Int32;
  word: UInt32;
begin
  w := System.length(Fmagnitude);
  offset := 0;

  System.Dec(w);
  word := UInt32(Fmagnitude[w] and firstWordMask);
{$IFDEF DEBUG}
  System.Assert(Fmagnitude[0] <> 0);
{$ENDIF DEBUG}
  while (word = 0) do
  begin
    System.Dec(w);
    word := UInt32(Fmagnitude[w]);
    offset := offset + 32;
  end;

  while ((word and $FF) = 0) do
  begin
    word := word shr 8;
    offset := offset + 8;
  end;

  while ((word and 1) = 0) do
  begin
    word := word shr 1;
    System.Inc(offset);
  end;

  Result := offset;
end;

class function TBigInteger.ModInverse32(d: Int32): Int32;
var
  x: Int32;
begin
  // Newton's method with initial estimate "correct to 4 bits"
{$IFDEF DEBUG}
  System.Assert((d and 1) <> 0);
{$ENDIF DEBUG}
  x := d + (((d + 1) and 4) shl 1); // d.x == 1 mod 2**4
{$IFDEF DEBUG}
  System.Assert(((d * x) and 15) = 1);
{$ENDIF DEBUG}
  x := x * (2 - (d * x)); // d.x == 1 mod 2**8
  x := x * (2 - (d * x)); // d.x == 1 mod 2**16
  x := x * (2 - (d * x)); // d.x == 1 mod 2**32
{$IFDEF DEBUG}
  System.Assert(d * x = 1);
{$ENDIF DEBUG}
  Result := x;
end;

function TBigInteger.GetMQuote: Int32;
var
  d: Int32;
begin
  if (FmQuote <> 0) then
  begin
    Result := FmQuote; // already calculated
    Exit;
  end;

{$IFDEF DEBUG}
  System.Assert(Fsign > 0);
{$ENDIF DEBUG}
  d := Int32(Fmagnitude[System.length(Fmagnitude) - 1]);
  d := -d;

{$IFDEF DEBUG}
  System.Assert((d and 1) <> 0);
{$ENDIF DEBUG}
  FmQuote := ModInverse32(d);
  Result := FmQuote;
end;

function TBigInteger.IsEqualMagnitude(const x: TBigInteger): Boolean;
var
  i: Integer;
  xMag: TCryptoLibInt32Array;
begin
  xMag := x.Fmagnitude;
  if (System.length(Fmagnitude) <> System.length(xMag)) then
  begin
    Result := false;
    Exit;
  end;
  for i := 0 to System.Pred(System.length(Fmagnitude)) do
  begin
    if (Fmagnitude[i] <> xMag[i]) then
    begin
      Result := false;
      Exit;
    end;
  end;
  Result := True;
end;

function TBigInteger.IsProbablePrime(certainty: Int32;
  randomlySelected: Boolean): Boolean;
var
  n: TBigInteger;
begin
  if (certainty <= 0) then
  begin
    Result := True;
    Exit;
  end;

  n := Abs();

  if (not n.TestBit(0)) then
  begin
    Result := n.Equals(Two);
    Exit;
  end;

  if (n.Equals(One)) then
  begin
    Result := false;
    Exit;
  end;

  Result := n.CheckProbablePrime(certainty, RandomSource, randomlySelected);
end;

class function TBigInteger.Jacobi(const a, b: TBigInteger): Int32;
var
  totalS, e, bLsw, a1Lsw: Int32;
  a1, La, Lb: TBigInteger;
begin
  La := a;
  Lb := b;
{$IFDEF DEBUG}
  System.Assert(La.SignValue >= 0);
  System.Assert(Lb.SignValue > 0);
  System.Assert(Lb.TestBit(0));
  System.Assert(La.CompareTo(Lb) < 0);
{$ENDIF DEBUG}
  totalS := 1;
  while True do
  begin
    if (La.SignValue = 0) then
    begin
      Result := 0;
      Exit;
    end;

    if (La.Equals(One)) then
    begin
      break;
    end;

    e := La.GetLowestSetBit();

    bLsw := Lb.Fmagnitude[System.length(Lb.Fmagnitude) - 1];
    if (((e and 1) <> 0) and (((bLsw and 7) = 3) or ((bLsw and 7) = 5))) then
    begin
      totalS := -totalS;
    end;

    if (La.BitLength = e + 1) then
    begin
      break;
    end;
    a1 := La.ShiftRight(e);

    a1Lsw := a1.Fmagnitude[System.length(a1.Fmagnitude) - 1];
    if (((bLsw and 3) = 3) and ((a1Lsw and 3) = 3)) then
    begin
      totalS := -totalS;
    end;

    La := Lb.Remainder(a1);
    Lb := a1;
  end;
  Result := totalS;
end;

function TBigInteger.IsProbablePrime(certainty: Int32): Boolean;
begin
  Result := IsProbablePrime(certainty, false);
end;

class function TBigInteger.MakeMagnitude(const bytes: TCryptoLibByteArray;
  offset, length: Int32): TCryptoLibInt32Array;
var
  endPoint, firstSignificant, nInts, bCount, v, magnitudeIndex, i: Int32;
  mag: TCryptoLibInt32Array;
begin
  endPoint := offset + length;

  // strip leading zeros
  firstSignificant := offset;
  while ((firstSignificant < endPoint) and (bytes[firstSignificant] = 0)) do
  begin

    System.Inc(firstSignificant);
  end;

  if (firstSignificant >= endPoint) then
  begin
    Result := FZeroMagnitude;
    Exit;
  end;

  nInts := (endPoint - firstSignificant + 3) div BytesPerInt;
  bCount := (endPoint - firstSignificant) mod BytesPerInt;
  if (bCount = 0) then
  begin
    bCount := BytesPerInt;
  end;

  if (nInts < 1) then
  begin
    Result := FZeroMagnitude;
    Exit;
  end;

  System.SetLength(mag, nInts);

  v := 0;
  magnitudeIndex := 0;

  i := firstSignificant;
  while i < endPoint do

  begin
    v := v shl 8;
    v := v or (bytes[i] and $FF);
    System.Dec(bCount);
    if (bCount <= 0) then
    begin
      mag[magnitudeIndex] := v;
      System.Inc(magnitudeIndex);
      bCount := BytesPerInt;
      v := 0;
    end;
    System.Inc(i);
  end;

  if (magnitudeIndex < System.length(mag)) then
  begin
    mag[magnitudeIndex] := v;
  end;

  Result := mag;
end;

function TBigInteger.Max(const value: TBigInteger): TBigInteger;
begin
  if CompareTo(value) > 0 then
  begin
    Result := Self;
  end
  else
  begin
    Result := value;
  end;
end;

function TBigInteger.Min(const value: TBigInteger): TBigInteger;
begin
  if CompareTo(value) < 0 then
  begin
    Result := Self;
  end
  else
  begin
    Result := value;
  end;
end;

function TBigInteger.ModInverse(const m: TBigInteger): TBigInteger;
var
  d, x, Gcd: TBigInteger;
begin
  if (m.Fsign < 1) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SModulusPositive);
  end;

  // TODO Too slow at the moment
  // // "Fast Key Exchange with Elliptic Curve Systems" R.Schoeppel
  // if (m.TestBit(0))
  // {
  // //The Almost Inverse Algorithm
  // int k = 0;
  // BigInteger B = One, C = Zero, F = this, G = m, tmp;
  //
  // for (;;)
  // {
  // // While F is even, do F=F/u, C=C*u, k=k+1.
  // int zeroes = F.GetLowestSetBit();
  // if (zeroes > 0)
  // {
  // F = F.ShiftRight(zeroes);
  // C = C.ShiftLeft(zeroes);
  // k += zeroes;
  // }
  //
  // // If F = 1, then return B,k.
  // if (F.Equals(One))
  // {
  // BigInteger half = m.Add(One).ShiftRight(1);
  // BigInteger halfK = half.ModPow(BigInteger.ValueOf(k), m);
  // return B.Multiply(halfK).Mod(m);
  // }
  //
  // if (F.CompareTo(G) < 0)
  // {
  // tmp = G; G = F; F = tmp;
  // tmp = B; B = C; C = tmp;
  // }
  //
  // F = F.Add(G);
  // B = B.Add(C);
  // }
  // }

  if (m.QuickPow2Check()) then
  begin
    Result := ModInversePow2(m);
    Exit;
  end;

  d := Remainder(m);
  Gcd := ExtEuclid(d, m, x);

  if (not Gcd.Equals(One)) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SNotRelativelyPrime);
  end;

  if (x.Fsign < 0) then
  begin
    x := x.Add(m);
  end;

  Result := x;
end;

class function TBigInteger.ModInverse64(d: Int64): Int64;
var
  x: Int64;
begin
  // Newton's method with initial estimate "correct to 4 bits"
{$IFDEF DEBUG}
  System.Assert((d and Int64(1)) <> 0);
{$ENDIF DEBUG}
  x := d + (((d + Int64(1)) and Int64(4)) shl 1); // d.x == 1 mod 2**4
{$IFDEF DEBUG}
  System.Assert(((d * x) and Int64(15)) = Int64(1));
{$ENDIF DEBUG}
  x := x * (2 - (d * x)); // d.x == 1 mod 2**8
  x := x * (2 - (d * x)); // d.x == 1 mod 2**16
  x := x * (2 - (d * x)); // d.x == 1 mod 2**32
  x := x * (2 - (d * x)); // d.x == 1 mod 2**64
{$IFDEF DEBUG}
  System.Assert(d * x = Int64(1));
{$ENDIF DEBUG}
  Result := x;
end;

function TBigInteger.ModInversePow2(const m: TBigInteger): TBigInteger;
var
  Pow, bitsCorrect: Int32;
  inv64: Int64;
  x, d, t: TBigInteger;
begin
{$IFDEF DEBUG}
  System.Assert(m.SignValue > 0);
  System.Assert(m.BitCount = 1);
{$ENDIF DEBUG}
  if (not TestBit(0)) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SNotRelativelyPrime);
  end;

  Pow := m.BitLength - 1;

  inv64 := ModInverse64(Int64Value);
  if (Pow < 64) then
  begin
    inv64 := inv64 and ((Int64(1) shl Pow) - 1);
  end;

  x := TBigInteger.ValueOf(inv64);

  if (Pow > 64) then
  begin
    d := Remainder(m);
    bitsCorrect := 64;

    repeat
      t := x.Multiply(d).Remainder(m);
      x := x.Multiply(Two.Subtract(t)).Remainder(m);
      bitsCorrect := bitsCorrect shl 1;
    until (not(bitsCorrect < Pow));
  end;

  if (x.Fsign < 0) then
  begin
    x := x.Add(m);
  end;

  Result := x;
end;

function TBigInteger.ModPow(const e, m: TBigInteger): TBigInteger;
var
  negExp: Boolean;
  le: TBigInteger;
begin
  le := e;
  if (m.Fsign < 1) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SModulusPositive);
  end;

  if (m.Equals(One)) then
  begin
    Result := Zero;
    Exit;
  end;

  if (le.Fsign = 0) then
  begin
    Result := One;
    Exit;
  end;

  if (Fsign = 0) then
  begin
    Result := Zero;
    Exit;
  end;

  negExp := le.Fsign < 0;
  if (negExp) then
  begin
    le := le.Negate();
  end;

  Result := &Mod(m);

  if (not le.Equals(One)) then
  begin
    if ((m.Fmagnitude[System.length(m.Fmagnitude) - 1] and 1) = 0) then
    begin
      Result := ModPowBarrett(Result, le, m);
    end
    else
    begin
      Result := ModPowMonty(Result, le, m, True);

    end;
  end;

  if (negExp) then
  begin
    Result := Result.ModInverse(m);
  end;

end;

class function TBigInteger.ModPowBarrett(const b, e, m: TBigInteger)
  : TBigInteger;
var
  k, extraBits, expLength, numPowers, i, window, mult, lastZeroes, windowPos,
    bits, j: Int32;
  mr, yu, b2, y: TBigInteger;
  oddPowers: TCryptoLibGenericArray<TBigInteger>;
  windowList: TCryptoLibInt32Array;
begin
  k := System.length(m.Fmagnitude);
  mr := One.ShiftLeft((k + 1) shl 5);
  yu := One.ShiftLeft(k shl 6).Divide(m);

  // Sliding window from MSW to LSW
  extraBits := 0;
  expLength := e.BitLength;
  while (expLength > ExpWindowThresholds[extraBits]) do
  begin
    System.Inc(extraBits);
  end;

  numPowers := 1 shl extraBits;
  System.SetLength(oddPowers, numPowers);
  oddPowers[0] := b;

  b2 := ReduceBarrett(b.Square(), m, mr, yu);

  for i := 1 to System.Pred(numPowers) do

  begin
    oddPowers[i] := ReduceBarrett(oddPowers[i - 1].Multiply(b2), m, mr, yu);
  end;

  windowList := GetWindowList(e.Fmagnitude, extraBits);
{$IFDEF DEBUG}
  System.Assert(System.length(windowList) > 0);
{$ENDIF DEBUG}
  window := windowList[0];
  mult := window and $FF;
  lastZeroes := window shr 8;

  if (mult = 1) then
  begin
    y := b2;
    System.Dec(lastZeroes);
  end
  else
  begin
    y := oddPowers[mult shr 1];
  end;

  windowPos := 1;
  window := windowList[windowPos];
  System.Inc(windowPos);
  while (window <> -1) do
  begin

    mult := window and $FF;

    bits := lastZeroes + BitLengthTable[mult];

    j := 0;
    while j < bits do
    begin
      y := ReduceBarrett(y.Square(), m, mr, yu);
      System.Inc(j);
    end;

    y := ReduceBarrett(y.Multiply(oddPowers[mult shr 1]), m, mr, yu);

    lastZeroes := window shr 8;

    window := windowList[windowPos];
    System.Inc(windowPos);
  end;

  i := 0;
  while i < lastZeroes do
  begin
    y := ReduceBarrett(y.Square(), m, mr, yu);
    System.Inc(i);
  end;

  Result := y;
end;

class function TBigInteger.ModPowMonty(const b: TBigInteger;
  const e, m: TBigInteger; convert: Boolean): TBigInteger;
var
  n, powR, extraBits, expLength, numPowers, i, window, mult, lastZeroes,
    windowPos, bits, j: Int32;
  smallMontyModulus: Boolean;
  mDash: UInt32;
  yAccum, zVal, tmp, zSquared, windowList, yVal: TCryptoLibInt32Array;
  oddPowers: TCryptoLibMatrixInt32Array;
  Lb: TBigInteger;
begin
  Lb := b;
  n := System.length(m.Fmagnitude);
  powR := 32 * n;
  smallMontyModulus := (m.BitLength + 2) <= powR;
  mDash := UInt32(m.GetMQuote());

  // tmp = this * R mod m
  if (convert) then
  begin
    Lb := Lb.ShiftLeft(powR).Remainder(m);
  end;

  System.SetLength(yAccum, n + 1);

  zVal := Lb.Fmagnitude;
{$IFDEF DEBUG}
  System.Assert(System.length(zVal) <= n);
{$ENDIF DEBUG}
  if (System.length(zVal) < n) then
  begin
    System.SetLength(tmp, n);
    System.Move(zVal[0], tmp[n - System.length(zVal)],
      System.length(zVal) * System.SizeOf(Int32));
    zVal := tmp;
  end;

  // Sliding window from MSW to LSW

  extraBits := 0;

  // Filter the common case of small RSA exponents with few bits set
  if ((System.length(e.Fmagnitude) > 1) or (e.BitCount > 2)) then
  begin
    expLength := e.BitLength;
    while (expLength > ExpWindowThresholds[extraBits]) do
    begin
      System.Inc(extraBits);
    end;
  end;

  numPowers := 1 shl extraBits;

  System.SetLength(oddPowers, numPowers);
  oddPowers[0] := zVal;

  zSquared := System.Copy(zVal, 0, System.length(zVal));

  SquareMonty(yAccum, zSquared, m.Fmagnitude, mDash, smallMontyModulus);

  for i := 1 to System.Pred(numPowers) do

  begin
    oddPowers[i] := System.Copy(oddPowers[i - 1]);
    MultiplyMonty(yAccum, oddPowers[i], zSquared, m.Fmagnitude, mDash,
      smallMontyModulus);

  end;

  windowList := GetWindowList(e.Fmagnitude, extraBits);
{$IFDEF DEBUG}
  System.Assert(System.length(windowList) > 1);
{$ENDIF DEBUG}
  window := windowList[0];
  mult := window and $FF;
  lastZeroes := window shr 8;

  if (mult = 1) then
  begin
    yVal := zSquared;
    System.Dec(lastZeroes);
  end
  else
  begin
    yVal := System.Copy(oddPowers[mult shr 1]);
  end;

  windowPos := 1;
  window := windowList[windowPos];
  System.Inc(windowPos);
  while (Int32(window) <> Int32(-1)) do
  begin
    mult := window and $FF;

    bits := lastZeroes + BitLengthTable[mult];

    j := 0;
    while j < bits do
    begin
      SquareMonty(yAccum, yVal, m.Fmagnitude, mDash, smallMontyModulus);
      System.Inc(j);
    end;

    MultiplyMonty(yAccum, yVal, oddPowers[mult shr 1], m.Fmagnitude, mDash,
      smallMontyModulus);

    lastZeroes := window shr 8;
    window := windowList[windowPos];
    System.Inc(windowPos);
  end;

  i := 0;
  while i < lastZeroes do
  begin
    SquareMonty(yAccum, yVal, m.Fmagnitude, mDash, smallMontyModulus);
    System.Inc(i);
  end;

  if (convert) then
  begin
    // Return y * R^(-1) mod m

    MontgomeryReduce(yVal, m.Fmagnitude, mDash);

  end
  else if ((smallMontyModulus) and (CompareTo(0, yVal, 0, m.Fmagnitude) >= 0))
  then
  begin
    Subtract(0, yVal, 0, m.Fmagnitude);
  end;

  Result := TBigInteger.Create(1, yVal, True);
end;

class function TBigInteger.Subtract(xStart: Int32;
  const x: TCryptoLibInt32Array; yStart: Int32; const y: TCryptoLibInt32Array)
  : TCryptoLibInt32Array;
var
  iT, iV, borrow: Int32;
  m: Int64;
begin
{$IFDEF DEBUG}
  System.Assert(yStart < System.length(y));
  System.Assert(System.length(x) - xStart >= System.length(y) - yStart);
{$ENDIF DEBUG}
  iT := System.length(x);
  iV := System.length(y);

  borrow := 0;

  repeat
    System.Dec(iT);
    System.Dec(iV);
    m := (x[iT] and IMASK) - ((y[iV] and IMASK) + borrow);
    // fixed precedence bug :)
    x[iT] := Int32(m);

    // borrow = (m < 0) ? -1 : 0;
    borrow := Int32(m shr 63);
  until (not(iV > yStart));

  if (borrow <> 0) then
  begin
    System.Dec(iT);
    x[iT] := x[iT] - 1;
    while (x[iT] = -1) do
    begin
      System.Dec(iT);
      x[iT] := x[iT] - 1;
    end;

  end;

  Result := x;
end;

class procedure TBigInteger.MontgomeryReduce(const x, m: TCryptoLibInt32Array;
  mDash: UInt32);
var
  n, i, j: Int32;
  x0: UInt32;
  t, carry: UInt64;
begin
  // NOTE: Not a general purpose reduction (which would allow x up to twice the bitlength of m)
{$IFDEF DEBUG}
  System.Assert(System.length(x) = System.length(m));
{$ENDIF DEBUG}
  n := System.length(m);

  i := n - 1;

  while i >= 0 do
  begin

    x0 := UInt32(x[n - 1]);
    t := UInt32(UInt64(x0) * mDash);

    carry := t * UInt32(m[n - 1]) + x0;

{$IFDEF DEBUG}
    System.Assert(UInt32(carry) = 0);
{$ENDIF DEBUG}
    carry := carry shr 32;

    j := n - 2;

    while j >= 0 do
    begin
      carry := carry + (t * UInt32(m[j]) + UInt32(x[j]));
      x[j + 1] := Int32(carry);
      carry := carry shr 32;
      System.Dec(j);
    end;

    x[0] := Int32(carry);
{$IFDEF DEBUG}
    System.Assert(carry shr 32 = 0);
{$ENDIF DEBUG}
    System.Dec(i);
  end;

  if (CompareTo(0, x, 0, m) >= 0) then
  begin
    Subtract(0, x, 0, m);
  end;
end;

class function TBigInteger.Multiply(const x, y, z: TCryptoLibInt32Array)
  : TCryptoLibInt32Array;
var
  i, xBase, j: Int32;
  a, val: Int64;
begin
  i := System.length(z);

  if (i < 1) then
  begin
    Result := x;
    Exit;
  end;

  xBase := System.length(x) - System.length(y);

  repeat
    System.Dec(i);
    a := z[i] and IMASK;
    val := 0;

    if (a <> 0) then
    begin
      j := System.length(y) - 1;
      while j >= 0 do
      begin
        val := val + (a * (y[j] and IMASK) + (x[xBase + j] and IMASK));

        x[xBase + j] := Int32(val);

        val := Int64(UInt64(val) shr 32);
        System.Dec(j);
      end;
    end;

    System.Dec(xBase);

    if (xBase >= 0) then
    begin
      x[xBase] := Int32(val);
    end
    else
    begin
{$IFDEF DEBUG}
      System.Assert(val = 0);
{$ENDIF DEBUG}
    end;

  until (not(i > 0));

  Result := x;
end;

function TBigInteger.Multiply(const val: TBigInteger): TBigInteger;
var
  resLength, resSign: Int32;
  res: TCryptoLibInt32Array;
begin
  if (val.Equals(Self)) then
  begin
    Result := Square();
    Exit;
  end;

  if ((Fsign and val.Fsign) = 0) then
  begin
    Result := Zero;
    Exit;
  end;

  if (val.QuickPow2Check()) then // val is power of two
  begin
    Result := ShiftLeft(val.Abs().BitLength - 1);
    if val.Fsign > 0 then
    begin
      Exit;
    end
    else
    begin
      Result := Result.Negate();
      Exit;
    end;

  end;

  if (QuickPow2Check()) then // this is power of two
  begin

    Result := val.ShiftLeft(Abs().BitLength - 1);
    if Fsign > 0 then
    begin
      Exit;
    end
    else
    begin
      Result := Result.Negate();
      Exit;
    end;

  end;

  resLength := System.length(Fmagnitude) + System.length(val.Fmagnitude);
  System.SetLength(res, resLength);

  Multiply(res, Fmagnitude, val.Fmagnitude);

  resSign := Fsign xor val.Fsign xor 1;
  Result := TBigInteger.Create(resSign, res, True);
end;

class function TBigInteger.MultiplyMontyNIsOne(x, y, m, mDash: UInt32): UInt32;
var
  carry, um, prod2: UInt64;
  t: UInt32;
begin
  carry := UInt64(UInt64(x) * y);
  t := UInt32(UInt32(carry) * mDash);
  um := m;
  prod2 := um * t;
  carry := carry + UInt32(prod2);
{$IFDEF DEBUG}
  System.Assert(UInt32(carry) = 0);
{$ENDIF DEBUG}
  carry := (carry shr 32) + (prod2 shr 32);
  if (carry > um) then
  begin
    carry := carry - um;
  end;
{$IFDEF DEBUG}
  System.Assert(carry < um);
{$ENDIF DEBUG}
  Result := UInt32(carry);
end;

class procedure TBigInteger.MultiplyMonty(const a, x, y,
  m: TCryptoLibInt32Array; mDash: UInt32; smallMontyModulus: Boolean);
var
  n, aMax, j, i: Int32;
  a0, y0: UInt32;
  carry, t, prod1, prod2, xi: UInt64;
begin
  // mDash = -m^(-1) mod b

  n := System.length(m);

  if (n = 1) then
  begin
    x[0] := Int32(MultiplyMontyNIsOne(UInt32(x[0]), UInt32(y[0]),
      UInt32(m[0]), mDash));
    Exit;
  end;
  y0 := UInt32(y[n - 1]);

  xi := UInt32(x[n - 1]);

  carry := xi * y0;

  t := UInt32(UInt64(UInt32(carry)) * mDash);

  prod2 := t * UInt32(m[n - 1]);
  carry := carry + UInt32(prod2);
{$IFDEF DEBUG}
  System.Assert(UInt32(carry) = 0);
{$ENDIF DEBUG}
  carry := (carry shr 32) + (prod2 shr 32);

  j := n - 2;
  while j >= 0 do
  begin
    prod1 := xi * UInt32(y[j]);
    prod2 := t * UInt32(m[j]);

    carry := carry + ((prod1 and UIMASK) + UInt32(prod2));
    a[j + 2] := Int32(carry);
    carry := (carry shr 32) + (prod1 shr 32) + (prod2 shr 32);
    System.Dec(j);
  end;

  a[1] := Int32(carry);
  aMax := Int32(carry shr 32);

  i := n - 2;
  while i >= 0 do
  begin
    a0 := UInt32(a[n]);
    xi := UInt32(x[i]);

    prod1 := xi * y0;
    carry := (prod1 and UIMASK) + a0;
    t := UInt32(UInt64(UInt32(carry)) * mDash);

    prod2 := t * UInt32(m[n - 1]);
    carry := carry + UInt32(prod2);
{$IFDEF DEBUG}
    System.Assert(UInt32(carry) = 0);
{$ENDIF DEBUG}
    carry := (carry shr 32) + (prod1 shr 32) + (prod2 shr 32);

    j := n - 2;
    while j >= 0 do
    begin
      prod1 := xi * UInt32(y[j]);
      prod2 := t * UInt32(m[j]);

      carry := carry + ((prod1 and UIMASK) + UInt32(prod2) + UInt32(a[j + 1]));
      a[j + 2] := Int32(carry);
      carry := (carry shr 32) + (prod1 shr 32) + (prod2 shr 32);
      System.Dec(j);
    end;

    carry := carry + UInt32(aMax);
    a[1] := Int32(carry);
    aMax := Int32(carry shr 32);
    System.Dec(i);
  end;

  a[0] := aMax;

  if ((not smallMontyModulus) and (CompareTo(0, a, 0, m) >= 0)) then
  begin
    Subtract(0, a, 0, m);
  end;

  System.Move(a[1], x[0], n * System.SizeOf(Int32));

end;

function TBigInteger.NextProbablePrime: TBigInteger;
var
  n: TBigInteger;
begin
  if (Fsign < 0) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SNegativeValue);
  end;

  if (CompareTo(Two) < 0) then
  begin
    Result := Two;
    Exit;
  end;

  n := Inc().SetBit(0);

  while (not n.CheckProbablePrime(100, RandomSource, false)) do
  begin
    n := n.Add(Two);
  end;

  Result := n;
end;

procedure TBigInteger.ParseBytes(const bytes: TCryptoLibByteArray;
  offset, length: Int32);
var
  endPoint, iBval, numBytes, index: Int32;
  inverse: TCryptoLibByteArray;
begin
  if (length = 0) then
  begin
    raise EFormatCryptoLibException.CreateRes(@SZeroLengthBigInteger);
  end;

  Fsign := -1;
  FnBits := -1;
  FnBitLength := -1;
  FmQuote := 0;
  FIsInitialized := True;

  // TODO Move this processing into MakeMagnitude (provide sign argument)
  if (ShortInt(bytes[offset]) < 0) then
  begin
    Fsign := -1;

    endPoint := offset + length;

    iBval := offset;

    // strip leading sign bytes
    while (iBval < endPoint) and (ShortInt(bytes[iBval]) = -1) do
    begin
      System.Inc(iBval);
    end;

    if (iBval >= endPoint) then
    begin
      Fmagnitude := One.Fmagnitude;
    end
    else
    begin
      numBytes := endPoint - iBval;
      System.SetLength(inverse, numBytes);

      index := 0;
      while (index < numBytes) do
      begin
        inverse[index] := Byte(not bytes[iBval]);
        System.Inc(index);
        System.Inc(iBval);
      end;
{$IFDEF DEBUG}
      System.Assert(iBval = endPoint);
{$ENDIF DEBUG}
      System.Dec(index);
      while (inverse[index] = System.High(Byte)) do
      begin
        inverse[index] := System.Low(Byte);
        System.Dec(index);
      end;

      inverse[index] := Byte(inverse[index] + 1);

      Fmagnitude := MakeMagnitude(inverse, 0, System.length(inverse));
    end
  end
  else
  begin
    // strip leading zero bytes and return magnitude bytes
    Fmagnitude := MakeMagnitude(bytes, offset, length);

    if System.length(Fmagnitude) > 0 then
    begin
      Fsign := 1;
    end
    else
    begin
      Fsign := 0;
    end;
  end;

end;

procedure TBigInteger.ParseBytesWithSign(sign: Int32;
  const bytes: TCryptoLibByteArray; offset, length: Int32);
begin
  begin
    if ((sign < -1) or (sign > 1)) then
    begin
      raise EFormatCryptoLibException.CreateRes(@SInvalidSign);
    end;

    Fsign := -1;
    FnBits := -1;
    FnBitLength := -1;
    FmQuote := 0;
    FIsInitialized := True;

    if (sign = 0) then
    begin
      Fsign := 0;
      Fmagnitude := FZeroMagnitude;
    end
    else
    begin
      // copy bytes
      Fmagnitude := MakeMagnitude(bytes, offset, length);
      if System.length(Fmagnitude) < 1 then
      begin
        Fsign := 0;
      end
      else
      begin
        Fsign := sign;
      end;

    end;

  end;
end;

procedure TBigInteger.ParseString(const str: String; radix: Int32);
var
  style: TNumberStyles;
  chunk, index, Next, LowPoint, HighPoint: Int32;
  r, rE, b, bi: TBigInteger;
  dVal, s, temp: String;
  i: UInt64;
begin

  if (System.length(str) = 0) then
  begin
    raise EFormatCryptoLibException.CreateRes(@SZeroLengthBigInteger);
  end;

  Fsign := -1;
  FnBits := -1;
  FnBitLength := -1;
  FmQuote := 0;
  FIsInitialized := True;

  case radix of
    2:
      begin
        // Is there anyway to restrict to binary digits?
        style := TNumberStyles.Integer;
        chunk := chunk2;
        r := Fradix2;
        rE := Fradix2E;
      end;

    8:
      begin
        // Is there anyway to restrict to octal digits?
        style := TNumberStyles.Integer;
        chunk := chunk8;
        r := Fradix8;
        rE := Fradix8E;
      end;

    10:
      begin
        // This style seems to handle spaces and minus sign already (our processing redundant?)
        style := TNumberStyles.Integer;
        chunk := chunk10;
        r := Fradix10;
        rE := Fradix10E;

      end;

    16:
      begin
        // TODO Should this be HexNumber?
        style := TNumberStyles.AllowHexSpecifier;
        chunk := chunk16;
        r := Fradix16;
        rE := Fradix16E;
      end
  else
    begin
      raise EFormatCryptoLibException.CreateRes(@SInvalidBase);
    end;

  end;

{$IFDEF DELPHIXE3_UP}
  LowPoint := System.Low(str);
  HighPoint := System.High(str);
{$ELSE}
  LowPoint := 1;
  HighPoint := System.length(str);
{$ENDIF DELPHIXE3_UP}
  index := LowPoint;
  Fsign := 1;

  if (str[LowPoint] = '-') then
  begin
    if (HighPoint = 1) then
    begin
      raise EFormatCryptoLibException.CreateRes(@SZeroLengthBigInteger);
    end;

    Fsign := -1;
    index := LowPoint + 1;
  end;

  // strip leading zeros from the string str
  while (index < (HighPoint + 1)) do
  begin

    dVal := str[index];

    if (style = TNumberStyles.AllowHexSpecifier) then
    begin
      temp := '$' + dVal;
    end
    else
    begin
      temp := dVal;
    end;

    if (StrToInt(temp) = 0) then
    begin
      System.Inc(index);
    end
    else
    begin
      break;
    end;
  end;

  if (index >= (HighPoint + 1)) then
  begin
    // zero value - we're done
    Fsign := 0;
    Fmagnitude := FZeroMagnitude;
    Exit;
  end;

  /// ///
  // could we work out the max number of ints required to store
  // str.Length digits in the given base, then allocate that
  // storage in one hit?, then Generate the magnitude in one hit too?
  /// ///

  b := Zero;

  Next := index + chunk;

  while (Next <= (HighPoint + 1)) do
  begin
    s := System.Copy(str, index, chunk);
    if (style = TNumberStyles.AllowHexSpecifier) then
    begin
      temp := '$' + s;
    end
    else
    begin
      temp := s;
    end;

{$IFDEF FPC}
    i := StrToQWord(temp);
{$ELSE}
    i := StrToUInt64(temp);
{$ENDIF FPC}
    bi := CreateUValueOf(i);

    case (radix) of

      2:
        begin
          // TODO Need this because we are parsing in radix 10 above
          if (i >= 2) then
          begin
            raise EFormatCryptoLibException.CreateResFmt
              (@SBadCharacterRadix2, [s]);
          end;

          // TODO Parse 64 bits at a time
          b := b.ShiftLeft(1);

        end;
      8:
        begin
          // TODO Need this because we are parsing in radix 10 above
          if (i >= 8) then
          begin
            raise EFormatCryptoLibException.CreateResFmt
              (@SBadCharacterRadix8, [s]);
          end;

          // TODO Parse 63 bits at a time
          b := b.ShiftLeft(3);

        end;

      16:
        begin
          b := b.ShiftLeft(64);

        end
    else
      begin
        b := b.Multiply(rE);
      end;

    end;

    b := b.Add(bi);

    index := Next;
    Next := Next + chunk;

  end;

  if (index < System.length(str) + 1) then
  begin
    s := System.Copy(str, index, System.length(str) - (index - 1));

    if (style = TNumberStyles.AllowHexSpecifier) then
    begin
      temp := '$' + s;
    end
    else
    begin
      temp := s;
    end;

{$IFDEF FPC}
    i := StrToQWord(temp);
{$ELSE}
    i := StrToUInt64(temp);
{$ENDIF FPC}
    bi := CreateUValueOf(i);

    if (b.Fsign > 0) then
    begin
      if (radix = 2) then
      begin
        // NB: Can't reach here since we are parsing one char at a time
{$IFDEF DEBUG}
        System.Assert(false);
{$ENDIF DEBUG}
        // TODO Parse all bits at once
        // b = b.ShiftLeft(s.Length);
      end
      else if (radix = 8) then
      begin
        // NB: Can't reach here since we are parsing one char at a time
{$IFDEF DEBUG}
        System.Assert(false);
{$ENDIF DEBUG}
        // TODO Parse all bits at once
        // b = b.ShiftLeft(s.Length * 3);
      end
      else if (radix = 16) then
      begin
        b := b.ShiftLeft(System.length(s) shl 2);
      end
      else
      begin
        b := b.Multiply(r.Pow(System.length(s)));
      end;

      b := b.Add(bi);
    end
    else
    begin
      b := bi;
    end;
  end;

  Fmagnitude := b.Fmagnitude;

end;

function TBigInteger.Pow(exp: Int32): TBigInteger;
var
  powOf2: Int64;
  y, z: TBigInteger;
begin
  if (exp <= 0) then
  begin
    if (exp < 0) then
    begin
      raise EArithmeticCryptoLibException.CreateRes(@SNegativeExponent);
    end;

    Result := One;
    Exit;
  end;

  if (Fsign = 0) then
  begin
    Result := Self;
    Exit;
  end;

  if (QuickPow2Check()) then
  begin
    powOf2 := Int64(exp) * (Int64(BitLength) - 1);
    if (powOf2 > System.High(Int32)) then
    begin
      raise EArithmeticCryptoLibException.CreateRes(@SResultTooLarge);
    end;
    Result := One.ShiftLeft(Int32(powOf2));
    Exit;
  end;

  y := One;
  z := Self;

  while True do
  begin
    if ((exp and $1) = 1) then
    begin
      y := y.Multiply(z);
    end;
    exp := exp shr 1;
    if (exp = 0) then
    begin
      break;
    end;
    z := z.Multiply(z);
  end;

  Result := y;
end;

function TBigInteger.DivideWords(w: Int32): TBigInteger;
var
  n: Int32;
  mag: TCryptoLibInt32Array;
begin
{$IFDEF DEBUG}
  System.Assert(w >= 0);
{$ENDIF DEBUG}
  n := System.length(Fmagnitude);
  if (w >= n) then
  begin
    Result := Zero;
    Exit;
  end;

  System.SetLength(mag, n - w);
  System.Move(Fmagnitude[0], mag[0], (n - w) * System.SizeOf(Int32));
  Result := TBigInteger.Create(Fsign, mag, false);
end;

function TBigInteger.RemainderWords(w: Int32): TBigInteger;
var
  n: Int32;
  mag: TCryptoLibInt32Array;
begin
{$IFDEF DEBUG}
  System.Assert(w >= 0);
{$ENDIF DEBUG}
  n := System.length(Fmagnitude);
  if (w >= n) then
  begin
    Result := Self;
    Exit;
  end;

  System.SetLength(mag, w);
  System.Move(Fmagnitude[n - w], mag[0], w * System.SizeOf(Int32));
  Result := TBigInteger.Create(Fsign, mag, false);
end;

class function TBigInteger.ProbablePrime(BitLength: Int32;
  const random: IRandom): TBigInteger;
begin
  Result := TBigInteger.Create(BitLength, 100, random);
end;

function TBigInteger.RabinMillerTest(certainty: Int32; const random: IRandom;
  randomlySelected: Boolean): Boolean;
var
  bits, iterations, itersFor100Cert, s, j, shiftval: Int32;
  n, r, montRadix, minusMontRadix, a, y: TBigInteger;
begin
  bits := BitLength;

{$IFDEF DEBUG}
  System.Assert(certainty > 0);
  System.Assert(bits > 2);
  System.Assert(TestBit(0));
{$ENDIF DEBUG}
  iterations := ((certainty - 1) shr 1) + 1;
  if (randomlySelected) then
  begin
    if bits >= 1024 then
    begin
      itersFor100Cert := 4
    end
    else if bits >= 512 then
    begin
      itersFor100Cert := 8
    end
    else if bits >= 256 then
    begin
      itersFor100Cert := 16
    end
    else
    begin
      itersFor100Cert := 50
    end;

    if (certainty < 100) then
    begin
      iterations := Math.Min(itersFor100Cert, iterations);
    end
    else
    begin
      iterations := iterations - 50;
      iterations := iterations + itersFor100Cert;
    end;
  end;

  // let n = 1 + d . 2^s
  n := Self;
  shiftval := Int32(-1) shl 1; // -2
  s := n.GetLowestSetBitMaskFirst(shiftval);
{$IFDEF DEBUG}
  System.Assert(s >= 1);
{$ENDIF DEBUG}
  r := n.ShiftRight(s);

  // NOTE: Avoid conversion to/from Montgomery form and check for R/-R as result instead

  montRadix := One.ShiftLeft(32 * System.length(n.Fmagnitude)).Remainder(n);
  minusMontRadix := n.Subtract(montRadix);

  repeat
    repeat
      a := TBigInteger.Create(n.BitLength, random);

    until (not((a.Fsign = 0) or (a.CompareTo(n) >= 0) or
      (a.IsEqualMagnitude(montRadix)) or (a.IsEqualMagnitude(minusMontRadix))));

    y := ModPowMonty(a, r, n, false);

    if (not y.Equals(montRadix)) then
    begin
      j := 0;
      while (not y.Equals(minusMontRadix)) do
      begin
        System.Inc(j);
        if (j = s) then
        begin
          Result := false;
          Exit;
        end;

        y := ModPowMonty(y, Two, n, false);

        if (y.Equals(montRadix)) then
        begin
          Result := false;
          Exit;
        end;
      end;

    end;
    System.Dec(iterations);
  until (not(iterations > 0));

  Result := True;
end;

function TBigInteger.RabinMillerTest(certainty: Int32;
  const random: IRandom): Boolean;
begin
  Result := RabinMillerTest(certainty, random, false);
end;

class function TBigInteger.ReduceBarrett(const x: TBigInteger;
  const m, mr, yu: TBigInteger): TBigInteger;
var
  xLen, mLen, k: Int32;
  q1, q2, q3, r1, r2, r3, lx: TBigInteger;
begin
  lx := x;
  xLen := lx.BitLength;
  mLen := m.BitLength;
  if (xLen < mLen) then
  begin
    Result := lx;
    Exit;
  end;

  if ((xLen - mLen) > 1) then
  begin
    k := System.length(m.Fmagnitude);

    q1 := lx.DivideWords(k - 1);
    q2 := q1.Multiply(yu); // TODO Only need partial multiplication here
    q3 := q2.DivideWords(k + 1);

    r1 := lx.RemainderWords(k + 1);
    r2 := q3.Multiply(m); // TODO Only need partial multiplication here
    r3 := r2.RemainderWords(k + 1);

    lx := r1.Subtract(r3);
    if (lx.Fsign < 0) then
    begin
      lx := lx.Add(mr);
    end;
  end;

  while (lx.CompareTo(m) >= 0) do
  begin
    lx := lx.Subtract(m);
  end;

  Result := lx;
end;

function TBigInteger.Remainder(const x, y: TCryptoLibInt32Array)
  : TCryptoLibInt32Array;
var
  xStart, yStart, xyCmp, yBitLength, xBitLength, shift, cBitLength, cStart,
    len: Int32;
  c: TCryptoLibInt32Array;
  firstC, firstX: UInt32;
begin
  xStart := 0;
  while ((xStart < System.length(x)) and (x[xStart] = 0)) do
  begin
    System.Inc(xStart);
  end;

  yStart := 0;

  while ((yStart < System.length(y)) and (y[yStart] = 0)) do
  begin
    System.Inc(yStart);
  end;

{$IFDEF DEBUG}
  System.Assert(yStart < System.length(y));
{$ENDIF DEBUG}
  xyCmp := CompareNoLeadingZeroes(xStart, x, yStart, y);

  if (xyCmp > 0) then
  begin
    yBitLength := CalcBitLength(1, yStart, y);
    xBitLength := CalcBitLength(1, xStart, x);
    shift := xBitLength - yBitLength;

    cStart := 0;
    cBitLength := yBitLength;
    if (shift > 0) then
    begin

      c := ShiftLeft(y, shift);
      cBitLength := cBitLength + shift;
{$IFDEF DEBUG}
      System.Assert(c[0] <> 0);
{$ENDIF DEBUG}
    end
    else
    begin

      len := System.length(y) - yStart;
      System.SetLength(c, len);
      System.Move(y[yStart], c[0], len * System.SizeOf(Int32));
    end;

    while True do
    begin
      if ((cBitLength < xBitLength) or (CompareNoLeadingZeroes(xStart, x,
        cStart, c) >= 0)) then
      begin
        Subtract(xStart, x, cStart, c);

        while (x[xStart] = 0) do
        begin
          System.Inc(xStart);
          if (xStart = System.length(x)) then
          begin
            Result := x;
            Exit;
          end;
        end;

        // xBitLength = CalcBitLength(xStart, x);
        xBitLength := 32 * (System.length(x) - xStart - 1) + BitLen(x[xStart]);

        if (xBitLength <= yBitLength) then
        begin
          if (xBitLength < yBitLength) then
          begin
            Result := x;
            Exit;
          end;

          xyCmp := CompareNoLeadingZeroes(xStart, x, yStart, y);

          if (xyCmp <= 0) then
          begin
            break;
          end;
        end;
      end;

      shift := cBitLength - xBitLength;

      // NB: The case where c[cStart] is 1-bit is harmless
      if (shift = 1) then
      begin
        firstC := UInt32(c[cStart] shr 1);
        firstX := UInt32(x[xStart]);
        if (firstC > firstX) then
        begin
          System.Inc(shift);
        end;
      end;

      if (shift < 2) then
      begin
        ShiftRightOneInPlace(cStart, c);
        System.Dec(cBitLength);
      end
      else
      begin
        ShiftRightInPlace(cStart, c, shift);
        cBitLength := cBitLength - shift;
      end;

      // cStart = c.Length - ((cBitLength + 31) / 32);
      while (c[cStart] = 0) do
      begin
        System.Inc(cStart);
      end;

    end;
  end;

  if (xyCmp = 0) then
  begin
    TArrayUtils.Fill(x, xStart, System.length(x), Int32(0));
  end;

  Result := x;
end;

function TBigInteger.SetBit(n: Int32): TBigInteger;
begin
  if (n < 0) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SInvalidBitAddress);
  end;

  if (TestBit(n)) then
  begin
    Result := Self;
    Exit;
  end;

  // TODO Handle negative values and zero
  if ((Fsign > 0) and (n < (BitLength - 1))) then
  begin
    Result := FlipExistingBit(n);
    Exit;
  end;

  Result := &Or(One.ShiftLeft(n));
end;

function TBigInteger.ShiftLeft(n: Int32): TBigInteger;
begin
  if ((Fsign = 0) or (System.length(Fmagnitude) = 0)) then
  begin
    Result := Zero;
    Exit;
  end;

  if (n = 0) then
  begin
    Result := Self;
    Exit;
  end;

  if (n < 0) then
  begin
    Result := ShiftRight(-n);
    Exit;
  end;

  Result := TBigInteger.Create(Fsign, ShiftLeft(Fmagnitude, n), True);

  // if (FnBits <> -1) then
  if (BitCount <> -1) then
  begin
    if Fsign > 0 then
    begin
      Result.FnBits := FnBits;
    end
    else
    begin
      Result.FnBits := FnBits + n;
    end;

  end;

  if (FnBitLength <> -1) then
  begin
    Result.FnBitLength := FnBitLength + n;
  end;

end;

class function TBigInteger.ShiftLeft(const mag: TCryptoLibInt32Array; n: Int32)
  : TCryptoLibInt32Array;
var
  nInts, nBits, magLen, i, nBits2, highBits, m, j, Next: Int32;
  newMag: TCryptoLibInt32Array;

begin
  nInts := Int32(UInt32(n) shr 5);
  nBits := n and $1F;
  magLen := System.length(mag);

  if (nBits = 0) then
  begin
    System.SetLength(newMag, magLen + nInts);
    System.Move(mag[0], newMag[0], System.length(mag) * System.SizeOf(Int32));
  end
  else
  begin
    i := 0;
    nBits2 := 32 - nBits;
    highBits := Int32(UInt32(mag[0]) shr nBits2);

    if (highBits <> 0) then
    begin
      System.SetLength(newMag, magLen + nInts + 1);

      newMag[i] := highBits;
      System.Inc(i);
    end
    else
    begin
      System.SetLength(newMag, magLen + nInts);

    end;

    m := mag[0];

    j := 0;

    while (j < (magLen - 1)) do
    begin
      Next := mag[j + 1];

      newMag[i] := (m shl nBits) or Int32(UInt32(Next) shr nBits2);
      System.Inc(i);
      m := Next;
      System.Inc(j);
    end;

    newMag[i] := mag[magLen - 1] shl nBits;
  end;

  Result := newMag;
end;

function TBigInteger.ShiftRight(n: Int32): TBigInteger;
var
  resultLength, numInts, numBits, numBits2, magPos, i: Int32;
  res: TCryptoLibInt32Array;
begin
  if (n = 0) then
  begin
    Result := Self;
    Exit;
  end;

  if (n < 0) then
  begin
    Result := ShiftLeft(-n);
    Exit;
  end;

  if (n >= BitLength) then
  begin
    if Fsign < 0 then
    begin
      Result := One.Negate();
      Exit;
    end
    else
    begin
      Result := Zero;
      Exit;
    end;

  end;

  // int[] res = (int[]) this.magnitude.Clone();
  //
  // ShiftRightInPlace(0, res, n);
  //
  // return new BigInteger(this.sign, res, true);

  resultLength := (BitLength - n + 31) shr 5;
  System.SetLength(res, resultLength);

  numInts := n shr 5;
  numBits := n and 31;

  if (numBits = 0) then
  begin
    System.Move(Fmagnitude[0], res[0], System.length(res) *
      System.SizeOf(Int32));
  end
  else
  begin
    numBits2 := 32 - numBits;

    magPos := System.length(Fmagnitude) - 1 - numInts;

    i := resultLength - 1;
    while i >= 0 do
    begin
      res[i] := Int32(UInt32(Fmagnitude[magPos]) shr numBits);
      System.Dec(magPos);

      if (magPos >= 0) then
      begin
        res[i] := res[i] or (Fmagnitude[magPos] shl numBits2);
      end;
      System.Dec(i);
    end;

  end;

{$IFDEF DEBUG}
  System.Assert(res[0] <> 0);
{$ENDIF DEBUG}
  Result := TBigInteger.Create(Fsign, res, false);
end;

class procedure TBigInteger.ShiftRightInPlace(start: Int32;
  const mag: TCryptoLibInt32Array; n: Int32);
var
  nInts, nBits, magEnd, delta, i, nBits2, m, Next: Int32;
begin
  nInts := Int32(UInt32(n) shr 5) + start;
  nBits := n and $1F;
  magEnd := System.length(mag) - 1;

  if (nInts <> start) then
  begin
    delta := (nInts - start);

    i := magEnd;
    while i >= nInts do
    begin
      mag[i] := mag[i - delta];
      System.Dec(i);
    end;

    i := nInts - 1;
    while i >= start do
    begin
      mag[i] := 0;
      System.Dec(i);
    end;

  end;

  if (nBits <> 0) then
  begin
    nBits2 := 32 - nBits;
    m := mag[magEnd];

    i := magEnd;
    while i > nInts do
    begin
      Next := mag[i - 1];

      mag[i] := Int32(UInt32(m) shr nBits) or (Next shl nBits2);
      m := Next;
      System.Dec(i);
    end;

    mag[nInts] := Int32(UInt32(mag[nInts]) shr nBits);
  end;
end;

class procedure TBigInteger.ShiftRightOneInPlace(start: Int32;
  const mag: TCryptoLibInt32Array);
var
  i, m, Next: Int32;
begin
  i := System.length(mag);
  m := mag[i - 1];

  System.Dec(i);
  while (i > start) do
  begin
    Next := mag[i - 1];
    mag[i] := (Int32(UInt32(m) shr 1)) or (Next shl 31);
    m := Next;
    System.Dec(i);
  end;

  mag[start] := Int32(UInt32(mag[start]) shr 1);
end;

class function TBigInteger.Square(const w, x: TCryptoLibInt32Array)
  : TCryptoLibInt32Array;
var
  c, v, prod: UInt64;
  wBase, i, j: Int32;
begin
  // Note: this method allows w to be only (2 * x.Length - 1) words if result will fit
  // if (w.Length != 2 * x.Length)
  // throw new ArgumentException("no I don't think so...");

  wBase := System.length(w) - 1;

  i := System.length(x) - 1;

  while i > 0 do

  begin
    v := UInt32(x[i]);

    c := v * v + UInt32(w[wBase]);
    w[wBase] := Int32(c);
    c := c shr 32;

    j := i - 1;
    while j >= 0 do
    begin
      prod := v * UInt32(x[j]);

      System.Dec(wBase);
      c := c + ((UInt32(w[wBase]) and UIMASK) + (UInt32(prod) shl 1));
      w[wBase] := Int32(c);
      c := (c shr 32) + (prod shr 31);
      System.Dec(j);
    end;

    System.Dec(wBase);
    c := c + UInt32(w[wBase]);
    w[wBase] := Int32(c);

    System.Dec(wBase);
    if (wBase >= 0) then
    begin
      w[wBase] := Int32(c shr 32);
    end
    else
    begin
{$IFDEF DEBUG}
      System.Assert((c shr 32) = 0);
{$ENDIF DEBUG}
    end;

    wBase := wBase + i;
    System.Dec(i);
  end;

  c := UInt32(x[0]);

  c := (c * c) + UInt32(w[wBase]);
  w[wBase] := Int32(c);

  System.Dec(wBase);
  if (wBase >= 0) then
  begin
    w[wBase] := w[wBase] + Int32(c shr 32);
  end
  else
  begin
{$IFDEF DEBUG}
    System.Assert((c shr 32) = 0);
{$ENDIF DEBUG}
  end;

  Result := w;
end;

class procedure TBigInteger.SquareMonty(const a, x, m: TCryptoLibInt32Array;
  mDash: UInt32; smallMontyModulus: Boolean);
var
  n, aMax, j, i: Int32;
  xVal, a0: UInt32;
  x0, carry, t, prod1, prod2, xi: UInt64;
begin
  // mDash = -m^(-1) mod b

  n := System.length(m);

  if (n = 1) then
  begin
    xVal := UInt32(x[0]);
    x[0] := Int32(MultiplyMontyNIsOne(xVal, xVal, UInt32(m[0]), mDash));
    Exit;
  end;

  x0 := UInt32(x[n - 1]);

  carry := x0 * x0;

  t := UInt32(UInt64(UInt32(carry)) * mDash);

  prod2 := t * UInt32(m[n - 1]);
  carry := carry + UInt32(prod2);

{$IFDEF DEBUG}
  System.Assert(UInt32(carry) = 0);
{$ENDIF DEBUG}
  carry := (carry shr 32) + (prod2 shr 32);

  j := n - 2;
  while j >= 0 do
  begin
    prod1 := x0 * UInt32(x[j]);
    prod2 := t * UInt32(m[j]);

    carry := carry + ((prod2 and UIMASK) + (UInt32(prod1) shl 1));
    a[j + 2] := Int32(carry);
    carry := (carry shr 32) + (prod1 shr 31) + (prod2 shr 32);
    System.Dec(j);
  end;

  a[1] := Int32(carry);
  aMax := Int32(carry shr 32);

  i := n - 2;
  while i >= 0 do
  begin
    a0 := UInt32(a[n]);
    t := UInt32(UInt64(a0) * mDash);

    carry := t * UInt32(m[n - 1]) + a0;

{$IFDEF DEBUG}
    System.Assert(UInt32(carry) = 0);
{$ENDIF DEBUG}
    carry := carry shr 32;

    j := n - 2;
    while j > i do
    begin
      carry := carry + (t * UInt32(m[j]) + UInt32(a[j + 1]));
      a[j + 2] := Int32(carry);
      carry := carry shr 32;
      System.Dec(j);
    end;

    xi := UInt32(x[i]);

    prod1 := xi * xi;
    prod2 := t * UInt32(m[i]);

    carry := carry + ((prod1 and UIMASK) + UInt32(prod2) + UInt32(a[i + 1]));
    a[i + 2] := Int32(carry);
    carry := (carry shr 32) + (prod1 shr 32) + (prod2 shr 32);

    j := i - 1;
    while j >= 0 do
    begin
      prod1 := xi * UInt32(x[j]);
      prod2 := t * UInt32(m[j]);

      carry := carry + ((prod2 and UIMASK) + (UInt32(prod1) shl 1) +
        UInt32(a[j + 1]));
      a[j + 2] := Int32(carry);
      carry := (carry shr 32) + (prod1 shr 31) + (prod2 shr 32);
      System.Dec(j);
    end;

    carry := carry + UInt32(aMax);
    a[1] := Int32(carry);
    aMax := Int32(carry shr 32);
    System.Dec(i);
  end;

  a[0] := aMax;

  if ((not smallMontyModulus) and (CompareTo(0, a, 0, m) >= 0)) then
  begin
    Subtract(0, a, 0, m);
  end;

  System.Move(a[1], x[0], n * System.SizeOf(Int32));

end;

function TBigInteger.Subtract(const n: TBigInteger): TBigInteger;
var
  compare: Int32;
  bigun, lilun: TBigInteger;
begin
  if (n.Fsign = 0) then
  begin
    Result := Self;
    Exit;
  end;

  if (Fsign = 0) then
  begin
    Result := n.Negate();
    Exit;
  end;

  if (Fsign <> n.Fsign) then
  begin
    Result := Add(n.Negate());
    Exit;
  end;

  compare := CompareNoLeadingZeroes(0, Fmagnitude, 0, n.Fmagnitude);
  if (compare = 0) then
  begin
    Result := Zero;
    Exit;
  end;

  if (compare < 0) then
  begin
    bigun := n;
    lilun := Self;
  end
  else
  begin
    bigun := Self;
    lilun := n;
  end;

  Result := TBigInteger.Create(Fsign * compare, doSubBigLil(bigun.Fmagnitude,
    lilun.Fmagnitude), True);
end;

function TBigInteger.ToString(radix: Int32): String;
var
  firstNonZero, pos, mask, bits, i, scale: Int32;
  sl: TStringList;
  s: TList<String>;
  moduli: TList<TBigInteger>;
  u, q, r: TBigInteger;
begin
  // TODO Make this method work for other radices (ideally 2 <= radix <= 36 as in Java)
  case (radix) of
    2, 8, 10, 16:
      begin
        // do nothing because it is in valid supported range
      end

  else
    begin
      raise EFormatCryptoLibException.CreateRes(@SUnSupportedBase);
    end;

  end;

  // NB: Can only happen to internally managed instances
  if ((not FIsInitialized) and (Fmagnitude = Nil)) then
  begin
    Result := 'Nil';
    Exit;
  end;

  if (Fsign = 0) then
  begin
    Result := '0';
    Exit;
  end;

  // NOTE: This *should* be unnecessary, since the magnitude *should* never have leading zero digits
  firstNonZero := 0;
  while (firstNonZero < System.length(Fmagnitude)) do
  begin
    if (Fmagnitude[firstNonZero] <> 0) then
    begin
      break;
    end;
    System.Inc(firstNonZero);
  end;

  if (firstNonZero = System.length(Fmagnitude)) then
  begin
    Result := '0';
    Exit;
  end;

  sl := TStringList.Create();
  sl.LineBreak := '';
  try

    if (Fsign = -1) then
    begin
      sl.Add('-');
    end;

    case radix of
      2:
        begin
          pos := firstNonZero;

          sl.Add(TBigInteger.IntToBin(Fmagnitude[pos]));
          System.Inc(pos);
          while (pos < System.length(Fmagnitude)) do
          begin
            AppendZeroExtendedString(sl,
              TBigInteger.IntToBin(Fmagnitude[pos]), 32);

            System.Inc(pos);
          end;

        end;

      8:
        begin
          mask := (1 shl 30) - 1;
          u := Abs();
          bits := u.BitLength;
          s := TList<string>.Create();
          try
            while (bits > 30) do
            begin
              s.Add(TBigInteger.IntToOctal(u.Int32Value and mask));
              u := u.ShiftRight(30);
              bits := bits - 30;
            end;
            sl.Add(TBigInteger.IntToOctal(u.Int32Value));
            i := s.Count - 1;
            while i >= 0 do
            begin
              AppendZeroExtendedString(sl, s[i], 10);
              System.Dec(i);
            end;
          finally
            s.Free;
          end;

        end;

      16:
        begin
          pos := firstNonZero;
          sl.Add(IntToHex(Fmagnitude[pos], 2));
          System.Inc(pos);
          while (pos < System.length(Fmagnitude)) do
          begin
            AppendZeroExtendedString(sl, IntToHex(Fmagnitude[pos], 2), 8);
            System.Inc(pos);
          end;

        end;
      // TODO This could work for other radices if there is an alternative to Convert.ToString method
      // default:
      10:
        begin
          q := Abs();
          if (q.BitLength < 64) then
          begin
            sl.Add(IntToStr(q.Int64Value));
            Result := sl.Text;
            Exit;
          end;

          // TODO Could cache the moduli for each radix (soft reference?)
          moduli := TList<TBigInteger>.Create();
          try
            r := TBigInteger.ValueOf(radix);
            while (r.CompareTo(q) <= 0) do
            begin
              moduli.Add(r);
              r := r.Square();
            end;

            scale := moduli.Count;
            sl.Capacity := sl.Capacity + (1 shl scale);

            ToString(sl, radix, moduli, scale, q);
          finally
            moduli.Free;
          end;

        end;

    end;

    Result := LowerCase(sl.Text);

  finally
    sl.Free;
  end;

end;

function TBigInteger.ToString: String;
begin
  Result := ToString(10);
end;

class function TBigInteger.GetWindowList(const mag: TCryptoLibInt32Array;
  extraBits: Int32): TCryptoLibInt32Array;
var
  i, v, leadingBits, resultSize, resultPos, bitPos, mult, multLimit,
    zeroes: Int32;
begin

  v := mag[0];
{$IFDEF DEBUG}
  System.Assert(v <> 0);
{$ENDIF DEBUG}
  leadingBits := BitLen(v);

  resultSize := (((System.length(mag) - 1) shl 5) + leadingBits)
    div (1 + extraBits) + 2;
  System.SetLength(Result, resultSize);
  resultPos := 0;

  bitPos := 33 - leadingBits;
  v := v shl bitPos;

  mult := 1;
  multLimit := 1 shl extraBits;
  zeroes := 0;

  i := 0;
  while True do

  begin
    while bitPos < 32 do

    begin
      if (mult < multLimit) then
      begin
        mult := (mult shl 1) or Int32((UInt32(v) shr 31));
      end
      else if (v < 0) then
      begin
        Result[resultPos] := CreateWindowEntry(mult, zeroes);
        System.Inc(resultPos);
        mult := 1;
        zeroes := 0;
      end
      else
      begin
        System.Inc(zeroes);
      end;

      v := v shl 1;
      System.Inc(bitPos);
    end;

    System.Inc(i);
    if (i = System.length(mag)) then
    begin
      Result[resultPos] := CreateWindowEntry(mult, zeroes);
      System.Inc(resultPos);
      break;
    end;

    v := mag[i];
    bitPos := 0;
  end;

  Result[resultPos] := -1;

end;

function TBigInteger.CheckProbablePrime(certainty: Int32; const random: IRandom;
  randomlySelected: Boolean): Boolean;
var
  numLists, i, j, prime, qRem, test: Int32;
  primeList: TCryptoLibInt32Array;
begin
{$IFDEF DEBUG}
  System.Assert(certainty > 0);
  System.Assert(CompareTo(Two) > 0);
  System.Assert(TestBit(0));
{$ENDIF DEBUG}
  // Try to reduce the penalty for really small numbers
  numLists := Math.Min(BitLength - 1, System.length(primeLists));

  for i := 0 to System.Pred(numLists) do
  begin
    test := Remainder(primeProducts[i]);

    primeList := primeLists[i];

    for j := 0 to System.Pred(System.length(primeList)) do

    begin
      prime := primeList[j];
      qRem := test mod prime;
      if (qRem = 0) then
      begin
        // We may find small numbers in the list
        Result := (BitLength < 16) and (Int32Value = prime);
        Exit;
      end;
    end;
  end;

  // TODO Special case for < 10^16 (RabinMiller fixed list)
  // if (BitLength < 30)
  // {
  // RabinMiller against 2, 3, 5, 7, 11, 13, 23 is sufficient
  // }

  // TODO Is it worth trying to create a hybrid of these two?
  Result := RabinMillerTest(certainty, random, randomlySelected);
  // return SolovayStrassenTest(certainty, random);

  // bool rbTest = RabinMillerTest(certainty, random);
  // bool ssTest = SolovayStrassenTest(certainty, random);
  //
  // Debug.Assert(rbTest == ssTest);
  //
  // return rbTest;
end;

function TBigInteger.ClearBit(n: Int32): TBigInteger;
begin
  if (n < 0) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SInvalidBitAddress);
  end;

  if (not TestBit(n)) then
  begin
    Result := Self;
    Exit;
  end;

  // TODO Handle negative values
  if ((Fsign > 0) and (n < (BitLength - 1))) then
  begin
    Result := FlipExistingBit(n);
    Exit;
  end;

  Result := AndNot(One.ShiftLeft(n));
end;

class function TBigInteger.CompareNoLeadingZeroes(xIndx: Int32;
  const x: TCryptoLibInt32Array; yIndx: Int32;
  const y: TCryptoLibInt32Array): Int32;
var
  diff: Int32;
  v1, v2: UInt32;
begin
  diff := (System.length(x) - System.length(y)) - (xIndx - yIndx);

  if (diff <> 0) then
  begin
    if diff < 0 then
    begin
      Result := -1;
      Exit;
    end
    else
    begin
      Result := 1;
      Exit;
    end;

  end;

  // lengths of magnitudes the same, test the magnitude values

  while (xIndx < System.length(x)) do
  begin
    v1 := UInt32(x[xIndx]);
    System.Inc(xIndx);
    v2 := UInt32(y[yIndx]);
    System.Inc(yIndx);

    if (v1 <> v2) then
    begin

      if v1 < v2 then
      begin
        Result := -1;
        Exit;
      end
      else
      begin
        Result := 1;
        Exit;
      end;
    end;
  end;

  Result := 0;
end;

class function TBigInteger.CompareTo(xIndx: Int32;
  const x: TCryptoLibInt32Array; yIndx: Int32;
  const y: TCryptoLibInt32Array): Int32;
begin
  while ((xIndx <> System.length(x)) and (x[xIndx] = 0)) do
  begin
    System.Inc(xIndx);
  end;

  while ((yIndx <> System.length(y)) and (y[yIndx] = 0)) do
  begin
    System.Inc(yIndx);
  end;

  Result := CompareNoLeadingZeroes(xIndx, x, yIndx, y);
end;

function TBigInteger.Divide(const x, y: TCryptoLibInt32Array)
  : TCryptoLibInt32Array;
var
  xStart, yStart, xyCmp, yBitLength, xBitLength, shift, iCountStart, cBitLength,
    cStart, len: Int32;
  Count, iCount, c: TCryptoLibInt32Array;
  firstC, firstX: UInt32;
begin
  xStart := 0;
  while ((xStart < System.length(x)) and (x[xStart] = 0)) do
  begin
    System.Inc(xStart);
  end;

  yStart := 0;

  while ((yStart < System.length(y)) and (y[yStart] = 0)) do
  begin
    System.Inc(yStart);
  end;

{$IFDEF DEBUG}
  System.Assert(yStart < System.length(y));
{$ENDIF DEBUG}
  xyCmp := CompareNoLeadingZeroes(xStart, x, yStart, y);

  if (xyCmp > 0) then
  begin
    yBitLength := CalcBitLength(1, yStart, y);
    xBitLength := CalcBitLength(1, xStart, x);
    shift := xBitLength - yBitLength;

    iCountStart := 0;

    cStart := 0;
    cBitLength := yBitLength;
    if (shift > 0) then
    begin
      // iCount = ShiftLeft(One.magnitude, shift);
      System.SetLength(iCount, (shift shr 5) + 1);

      iCount[0] := 1 shl (shift and 31);

      c := ShiftLeft(y, shift);
      cBitLength := cBitLength + shift;
    end
    else
    begin
      iCount := TCryptoLibInt32Array.Create(1);

      len := System.length(y) - yStart;
      System.SetLength(c, len);
      System.Move(y[yStart], c[0], len * System.SizeOf(Int32));
    end;

    System.SetLength(Count, System.length(iCount));

    while True do
    begin
      if ((cBitLength < xBitLength) or (CompareNoLeadingZeroes(xStart, x,
        cStart, c) >= 0)) then
      begin
        Subtract(xStart, x, cStart, c);
        AddMagnitudes(Count, iCount);

        while (x[xStart] = 0) do
        begin
          System.Inc(xStart);
          if (xStart = System.length(x)) then
          begin
            Result := Count;
            Exit;
          end;
        end;

        // xBitLength = CalcBitLength(xStart, x);
        xBitLength := 32 * (System.length(x) - xStart - 1) + BitLen(x[xStart]);

        if (xBitLength <= yBitLength) then
        begin
          if (xBitLength < yBitLength) then
          begin
            Result := Count;
            Exit;
          end;

          xyCmp := CompareNoLeadingZeroes(xStart, x, yStart, y);

          if (xyCmp <= 0) then
          begin
            break;
          end;
        end;
      end;

      shift := cBitLength - xBitLength;

      // NB: The case where c[cStart] is 1-bit is harmless
      if (shift = 1) then
      begin
        firstC := UInt32(c[cStart] shr 1);
        firstX := UInt32(x[xStart]);
        if (firstC > firstX) then
        begin
          System.Inc(shift);
        end;
      end;

      if (shift < 2) then
      begin
        ShiftRightOneInPlace(cStart, c);
        System.Dec(cBitLength);
        ShiftRightOneInPlace(iCountStart, iCount);
      end
      else
      begin
        ShiftRightInPlace(cStart, c, shift);
        cBitLength := cBitLength - shift;
        ShiftRightInPlace(iCountStart, iCount, shift);
      end;

      // cStart = c.Length - ((cBitLength + 31) / 32);
      while (c[cStart] = 0) do
      begin
        System.Inc(cStart);
      end;

      while (iCount[iCountStart] = 0) do
      begin
        System.Inc(iCountStart);
      end;
    end;
  end
  else
  begin
    System.SetLength(Count, 1);
  end;

  if (xyCmp = 0) then
  begin
    AddMagnitudes(Count, One.Fmagnitude);
    TArrayUtils.Fill(x, xStart, System.length(x), Int32(0));
  end;

  Result := Count;
end;

function TBigInteger.Divide(const val: TBigInteger): TBigInteger;
var
  tempRes: TBigInteger;
  mag: TCryptoLibInt32Array;
begin
  if (val.Fsign = 0) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SDivisionByZero);
  end;

  if (Fsign = 0) then
  begin
    Result := Zero;
    Exit;
  end;

  if (val.QuickPow2Check()) then // val is power of two
  begin
    tempRes := Abs().ShiftRight(val.Abs().BitLength - 1);
    if val.Fsign = Fsign then
    begin
      Result := tempRes;
      Exit;
    end
    else
    begin
      Result := tempRes.Negate();
      Exit;
    end;

  end;

  mag := System.Copy(Fmagnitude);

  Result := TBigInteger.Create(Fsign * val.Fsign,
    Divide(mag, val.Fmagnitude), True);
end;

function TBigInteger.DivideAndRemainder(const val: TBigInteger)
  : TCryptoLibGenericArray<TBigInteger>;
var
  biggies: TCryptoLibGenericArray<TBigInteger>;
  e: Int32;
  Quotient: TBigInteger;
  Remainder, quotient_array: TCryptoLibInt32Array;
begin
  if (val.Fsign = 0) then
  begin
    raise EArithmeticCryptoLibException.CreateRes(@SDivisionByZero);
  end;

  System.SetLength(biggies, 2);

  if (Fsign = 0) then
  begin
    biggies[0] := Zero;
    biggies[1] := Zero;
  end
  else if (val.QuickPow2Check()) then // val is power of two
  begin
    e := val.Abs().BitLength - 1;
    Quotient := Abs().ShiftRight(e);
    Remainder := LastNBits(e);

    if val.Fsign = Fsign then
    begin
      biggies[0] := Quotient
    end
    else
    begin
      biggies[0] := Quotient.Negate();
    end;

    biggies[1] := TBigInteger.Create(Fsign, Remainder, True);
  end
  else
  begin
    Remainder := System.Copy(Fmagnitude);
    quotient_array := Divide(Remainder, val.Fmagnitude);

    biggies[0] := TBigInteger.Create(Fsign * val.Fsign, quotient_array, True);
    biggies[1] := TBigInteger.Create(Fsign, Remainder, True);
  end;

  Result := biggies;
end;

function TBigInteger.ToByteArray(unsigned: Boolean): TCryptoLibByteArray;
var
  nBits, nBytes, magIndex, bytesIndex: Int32;
  mag, lastMag: UInt32;
  bytes: TCryptoLibByteArray;
  carry: Boolean;
begin
  if (Fsign = 0) then
  begin
    if unsigned then
    begin
      Result := FZeroEncoding;
      Exit;
    end
    else
    begin
      System.SetLength(Result, 1);
      Exit;
    end;

  end;

  if ((unsigned) and (Fsign > 0)) then
  begin
    nBits := BitLength;
  end
  else
  begin
    nBits := BitLength + 1;
  end;

  nBytes := GetByteLength(nBits);
  System.SetLength(bytes, nBytes);

  magIndex := System.length(Fmagnitude);
  bytesIndex := System.length(bytes);

  if (Fsign > 0) then
  begin
    while (magIndex > 1) do
    begin
      System.Dec(magIndex);
      mag := UInt32(Fmagnitude[magIndex]);
      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(mag);
      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(mag shr 8);
      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(mag shr 16);
      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(mag shr 24);
    end;

    lastMag := UInt32(Fmagnitude[0]);
    while (lastMag > System.High(Byte)) do
    begin
      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(lastMag);
      lastMag := lastMag shr 8;
    end;

    System.Dec(bytesIndex);
    bytes[bytesIndex] := Byte(lastMag);
  end
  else // sign < 0
  begin
    carry := True;

    while (magIndex > 1) do
    begin
      System.Dec(magIndex);
      mag := not(UInt32(Fmagnitude[magIndex]));

      if (carry) then
      begin
        System.Inc(mag);
        carry := (mag = System.Low(UInt32));
      end;

      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(mag);
      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(mag shr 8);
      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(mag shr 16);
      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(mag shr 24);

    end;

    lastMag := UInt32(Fmagnitude[0]);

    if (carry) then
    begin
      // Never wraps because magnitude[0] != 0
      System.Dec(lastMag);
    end;

    while (lastMag > System.High(Byte)) do
    begin
      System.Dec(bytesIndex);
      bytes[bytesIndex] := Byte(not lastMag);
      lastMag := lastMag shr 8;
    end;

    System.Dec(bytesIndex);
    bytes[bytesIndex] := Byte(not lastMag);

    if (bytesIndex > 0) then
    begin
      System.Dec(bytesIndex);
      bytes[bytesIndex] := System.High(Byte);
    end;
  end;

  Result := bytes;
end;

function TBigInteger.ToByteArray: TCryptoLibByteArray;
begin
  Result := ToByteArray(false);
end;

function TBigInteger.ToByteArrayUnsigned: TCryptoLibByteArray;
begin
  Result := ToByteArray(True);
end;

end.
