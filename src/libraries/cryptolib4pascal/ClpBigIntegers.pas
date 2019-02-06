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

unit ClpBigIntegers;

{$I CryptoLib.inc}

interface

uses
  Math,
  ClpBigInteger,
  ClpCryptoLibTypes,
  ClpISecureRandom;

resourcestring
  SInvalidLength = 'Standard Length Exceeded, "n"';
  SInvalidMinValue = '"min" may not be greater than "max""';

type

  /// <summary>
  /// BigInteger utilities.
  /// </summary>
  TBigIntegers = class abstract(TObject)

  strict private
  const
    MaxIterations = Int32(1000);

  public

    /// <summary>
    /// Return the passed in value as an unsigned byte array.
    /// </summary>
    /// <param name="n">
    /// value to be converted.
    /// </param>
    /// <returns>
    /// a byte array without a leading zero byte if present in the signed
    /// encoding.
    /// </returns>
    class function AsUnsignedByteArray(const n: TBigInteger)
      : TCryptoLibByteArray; overload; static;

    /// <summary>
    /// the passed in value as an unsigned byte array of specified length,
    /// zero-extended as necessary.
    /// </summary>
    /// <param name="length">
    /// length desired length of result array.
    /// </param>
    /// <param name="n">
    /// value to be converted.
    /// </param>
    /// <returns>
    /// a byte array of specified length, with leading zeroes as necessary
    /// given the size of n.
    /// </returns>
    class function AsUnsignedByteArray(length: Int32; const n: TBigInteger)
      : TCryptoLibByteArray; overload; static;

    /// <summary>
    /// Return a random BigInteger not less than 'min' and not greater than
    /// 'max'
    /// </summary>
    /// <param name="min">
    /// the least value that may be generated
    /// </param>
    /// <param name="max">
    /// the greatest value that may be generated
    /// </param>
    /// <param name="random">
    /// the source of randomness
    /// </param>
    /// <returns>
    /// a random BigInteger value in the range [min,max]
    /// </returns>
    class function CreateRandomInRange(const min, max: TBigInteger;
      // TODO Should have been just Random class
      const random: ISecureRandom): TBigInteger; static;

    /// <summary>
    /// <para>
    /// The regular <b>BigInteger.toByteArray()</b> includes the sign bit
    /// of the number and <br />might result in an extra byte addition.
    /// This method removes this extra byte.
    /// </para>
    /// </summary>
    /// <param name="b">
    /// the integer to format into a byte array
    /// </param>
    /// <param name="numBytes">
    /// the desired size of the resulting byte array
    /// </param>
    /// <returns>
    /// numBytes byte long array.
    /// </returns>
    class function BigIntegerToBytes(const b: TBigInteger; numBytes: Int32)
      : TCryptoLibByteArray; static; inline;

    class function GetUnsignedByteLength(const n: TBigInteger): Int32;
      static; inline;

  end;

implementation

{ TBigIntegers }

class function TBigIntegers.AsUnsignedByteArray(const n: TBigInteger)
  : TCryptoLibByteArray;
begin
  Result := n.ToByteArrayUnsigned();
end;

class function TBigIntegers.AsUnsignedByteArray(length: Int32;
  const n: TBigInteger): TCryptoLibByteArray;
var
  bytes: TCryptoLibByteArray;
begin
  bytes := n.ToByteArrayUnsigned();

  if (System.length(bytes) > length) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidLength);
  end;

  if (System.length(bytes) = length) then
  begin
    Result := bytes;
    Exit;
  end;

  System.SetLength(Result, length);
  System.Move(bytes[0], Result[System.length(Result) - System.length(bytes)],
    System.length(bytes) * System.SizeOf(Byte));

end;

class function TBigIntegers.BigIntegerToBytes(const b: TBigInteger;
  numBytes: Int32): TCryptoLibByteArray;
var
  biBytes: TCryptoLibByteArray;
  start, length: Int32;
begin
  System.SetLength(Result, numBytes);
  biBytes := b.ToByteArray();
  if System.length(biBytes) = (numBytes + 1) then
  begin
    start := 1
  end
  else
  begin
    start := 0
  end;

  length := min(System.length(biBytes), numBytes);
  System.Move(biBytes[start], Result[numBytes - length],
    length * System.SizeOf(Byte));
end;

class function TBigIntegers.CreateRandomInRange(const min, max: TBigInteger;
  // TODO Should have been just Random class
  const random: ISecureRandom): TBigInteger;
var
  cmp, I: Int32;
  x: TBigInteger;
begin
  cmp := min.CompareTo(max);
  if (cmp >= 0) then
  begin
    if (cmp > 0) then
    begin
      raise EArgumentCryptoLibException.CreateRes(@SInvalidMinValue);
    end;

    Result := min;
    Exit;
  end;

  if (min.BitLength > (max.BitLength shr 1)) then
  begin
    Result := CreateRandomInRange(TBigInteger.Zero, max.Subtract(min),
      random).Add(min);
    Exit;
  end;

  I := 0;
  while I < MaxIterations do
  begin
    x := TBigInteger.Create(max.BitLength, random);
    if ((x.CompareTo(min) >= 0) and (x.CompareTo(max) <= 0)) then
    begin
      Result := x;
      Exit;
    end;
    System.Inc(I);
  end;

  // fall back to a faster (restricted) method
  Result := TBigInteger.Create(max.Subtract(min).BitLength - 1, random)
    .Add(min);
end;

class function TBigIntegers.GetUnsignedByteLength(const n: TBigInteger): Int32;
begin
  Result := (n.BitLength + 7) shr 3;
end;

end.
