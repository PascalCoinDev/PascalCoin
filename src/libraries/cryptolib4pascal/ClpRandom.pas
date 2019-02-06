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

unit ClpRandom;

{$I CryptoLib.inc}

interface

uses
  Classes,
  ClpCryptoLibTypes,
  ClpIRandom;

resourcestring
  SBufferNil = 'Buffer Cannot be Nil';
  SMaxValueNegative = 'maxValue Must be Positive';
  SInvalidMinValue = 'minValue Cannot be Greater Than maxValue';

type
  TRandom = class(TInterfacedObject, IRandom)

  strict private
  const
    FMSEED = Int32(161803398);

  var
    FSeedArray: array [0 .. 55] of Int32;
    Finext, Finextp: Int32;

    function InternalSample(): Int32; inline;
    function GetSampleForLargeRange(): Double;

  strict protected
    /// <summary>Returns a random floating-point number between 0.0 and 1.0.</summary>
    /// <returns>A double-precision floating point number that is greater than or equal to 0.0, and less than 1.0.</returns>
    function Sample(): Double; virtual;

  public
    /// <summary>Initializes a new instance of the <see cref="T:System.Random" /> class, using a time-dependent default seed value.</summary>
    constructor Create(); overload;
    constructor Create(Seed: Int32); overload;

    /// <summary>Returns a non-negative random integer.</summary>
    /// <returns>A 32-bit signed integer that is greater than or equal to 0 and less than <see cref="F:System.Int32.MaxValue" />.</returns>
    /// <filterpriority>1</filterpriority>
    function Next(): Int32; overload; virtual;

    /// <summary>Returns a non-negative random integer that is less than the specified maximum.</summary>
    /// <returns>A 32-bit signed integer that is greater than or equal to 0, and less than <paramref name="maxValue" />; that is, the range of return values ordinarily includes 0 but not <paramref name="maxValue" />. However, if <paramref name="maxValue" /> equals 0, <paramref name="maxValue" /> is returned.</returns>
    /// <param name="maxValue">The exclusive upper bound of the random number to be generated. <paramref name="maxValue" /> must be greater than or equal to 0. </param>
    /// <exception cref="EArgumentOutOfRangeCryptoLibException">
    /// <paramref name="maxValue" /> is less than 0. </exception>
    /// <filterpriority>1</filterpriority>
    function Next(maxValue: Int32): Int32; overload; virtual;

    /// <summary>Returns a random integer that is within a specified range.</summary>
    /// <returns>A 32-bit signed integer greater than or equal to <paramref name="minValue" /> and less than <paramref name="maxValue" />; that is, the range of return values includes <paramref name="minValue" /> but not <paramref name="maxValue" />. If <paramref name="minValue" /> equals <paramref name="maxValue" />, <paramref name="minValue" /> is returned.</returns>
    /// <param name="minValue">The inclusive lower bound of the random number returned. </param>
    /// <param name="maxValue">The exclusive upper bound of the random number returned. <paramref name="maxValue" /> must be greater than or equal to <paramref name="minValue" />. </param>
    /// <exception cref="EArgumentOutOfRangeCryptoLibException">
    /// <paramref name="minValue" /> is greater than <paramref name="maxValue" />. </exception>
    /// <filterpriority>1</filterpriority>
    function Next(minValue, maxValue: Int32): Int32; overload; virtual;

    /// <summary>Returns a random floating-point number that is greater than or equal to 0.0, and less than 1.0.</summary>
    /// <returns>A double-precision floating point number that is greater than or equal to 0.0, and less than 1.0.</returns>
    /// <filterpriority>1</filterpriority>
    function NextDouble(): Double; virtual;

    /// <summary>
    /// Fills the elements of a specified array of bytes with random numbers.
    /// </summary>
    /// <param name="buffer">
    /// An array of bytes to contain random numbers.
    /// </param>
    /// <exception cref="EArgumentNilCryptoLibException">
    /// <paramref name="buffer" /> is nil.
    /// </exception>
    /// <filterpriority>1</filterpriority>
    procedure NextBytes(const buf: TCryptoLibByteArray); overload; virtual;

  end;

implementation

{ TRandom }

constructor TRandom.Create;
begin
{$IFDEF FPC}
  Create(Int32(TThread.GetTickCount64));
{$ELSE}
  Create(Int32(TThread.GetTickCount));
{$ENDIF FPC}
end;

constructor TRandom.Create(Seed: Int32);
var
  num1, num2, index1, index2: Int32;
begin
  num1 := FMSEED - Abs(Seed);
  FSeedArray[55] := num1;
  num2 := 1;
  for index1 := 1 to System.Pred(55) do
  begin
    index2 := 21 * index1 mod 55;
    FSeedArray[index2] := num2;
    num2 := num1 - num2;
    if (num2 < 0) then
      num2 := num2 + System.High(Int32);
    num1 := FSeedArray[index2];
  end;

  index1 := 1;
  while index1 < 5 do
  begin
    for index2 := 1 to System.Pred(56) do
    begin
      FSeedArray[index2] := FSeedArray[index2] - FSeedArray
        [1 + (index2 + 30) mod 55];
      if (FSeedArray[index2] < 0) then
        FSeedArray[index2] := FSeedArray[index2] + System.High(Int32);
    end;
    System.Inc(index1);
  end;

  Finext := 0;
  Finextp := 21;
end;

function TRandom.InternalSample: Int32;
var
  inext, inextp, index1, index2, num: Int32;
begin
  inext := Finext;
  inextp := Finextp;
  index1 := inext + 1;
  if ((index1) >= 56) then
    index1 := 1;

  index2 := inextp + 1;
  if ((index2) >= 56) then
    index2 := 1;
  num := FSeedArray[index1] - FSeedArray[index2];
  if (num < 0) then
    num := num + System.High(Int32);
  FSeedArray[index1] := num;
  Finext := index1;
  Finextp := index2;
  Result := num;
end;

function TRandom.GetSampleForLargeRange: Double;
var
  num: Int32;
begin
  num := InternalSample();
  if (InternalSample() mod 2 = 0) then
    num := -num;
  Result := (num + 2147483646.0) / 4294967293.0;
end;

function TRandom.Next(minValue, maxValue: Int32): Int32;
var
  num: Int64;
begin
  if (minValue > maxValue) then
  begin
    raise EArgumentOutOfRangeCryptoLibException.CreateRes(@SInvalidMinValue);
  end;
  num := Int64(maxValue) - Int64(minValue);
  if (num <= Int64(System.High(Int32))) then
  begin
    Result := Int32(Trunc(Sample()) * num) + minValue;
    Exit;
  end;
  Result := Int32(Int64(Trunc(GetSampleForLargeRange()) * num) +
    Int64(minValue));
end;

function TRandom.Next(maxValue: Int32): Int32;
begin
  if (maxValue < 0) then
  begin
    raise EArgumentOutOfRangeCryptoLibException.CreateRes(@SMaxValueNegative);
  end;
  Result := Int32(Trunc(Sample() * maxValue));
end;

function TRandom.Next: Int32;
begin
  Result := InternalSample();
end;

procedure TRandom.NextBytes(const buf: TCryptoLibByteArray);
var
  i: Int32;
begin
  if (buf = Nil) then
    raise EArgumentNilCryptoLibException.CreateRes(@SBufferNil);

  for i := System.Low(buf) to System.High(buf) do
  begin
    buf[i] := Byte(InternalSample() mod (255 + 1));
  end;

end;

function TRandom.NextDouble: Double;
begin
  Result := Sample();
end;

function TRandom.Sample: Double;
begin
  Result := InternalSample() * 4.6566128752458E-10;
end;

end.
