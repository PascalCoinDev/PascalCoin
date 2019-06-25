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

unit ClpDHParametersHelper;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpBigInteger,
  ClpBigIntegers,
  ClpECAlgorithms,
  ClpBits,
  ClpCryptoLibTypes;

type
  TDHParametersHelper = class sealed(TObject)

  strict private
  class var

    FSix: TBigInteger;
    FPrimeProducts: TCryptoLibInt32Array;
    FPrimeLists: TCryptoLibMatrixInt32Array;
    FBigPrimeProducts: TCryptoLibGenericArray<TBigInteger>;
    FIsBooted: Boolean;

    class function ConstructBigPrimeProducts(const primeProducts
      : TCryptoLibInt32Array): TCryptoLibGenericArray<TBigInteger>; static;

    class procedure Boot(); static;

    class constructor DHParametersHelper();

  public

    /// <summary>
    /// <para>
    /// Finds a pair of prime BigInteger's {p, q: p = 2q + 1}
    /// </para>
    /// <para>
    /// (see: Handbook of Applied Cryptography 4.86)
    /// </para>
    /// </summary>
    class function GenerateSafePrimes(size, certainty: Int32;
      const random: ISecureRandom): TCryptoLibGenericArray<TBigInteger>; static;

{$IFNDEF _FIXINSIGHT_}
    /// <summary>
    /// <para>
    /// Select a high order element of the multiplicative group Zp*
    /// </para>
    /// <para>
    /// p and q must be s.t. p = 2*q + 1, where p and q are prime (see
    /// generateSafePrimes)
    /// </para>
    /// </summary>
    class function SelectGenerator(const p, q: TBigInteger;
      const random: ISecureRandom): TBigInteger; static;
{$ENDIF}
  end;

implementation

{ TDHParametersHelper }

class procedure TDHParametersHelper.Boot;
begin
  if not FIsBooted then
  begin
    FSix := TBigInteger.ValueOf(6);

    FPrimeLists := TBigInteger.primeLists;
    FPrimeProducts := TBigInteger.primeProducts;
    FBigPrimeProducts := ConstructBigPrimeProducts(FPrimeProducts);

    FIsBooted := True;
  end;
end;

class constructor TDHParametersHelper.DHParametersHelper;
begin
  TDHParametersHelper.Boot;
end;

class function TDHParametersHelper.ConstructBigPrimeProducts(const primeProducts
  : TCryptoLibInt32Array): TCryptoLibGenericArray<TBigInteger>;
var
  bpp: TCryptoLibGenericArray<TBigInteger>;
  i: Int32;
begin
  System.SetLength(bpp, System.Length(FPrimeProducts));

  for i := 0 to System.Pred(System.Length(bpp)) do
  begin
    bpp[i] := TBigInteger.ValueOf(primeProducts[i]);
  end;

  result := bpp;
end;

class function TDHParametersHelper.GenerateSafePrimes(size, certainty: Int32;
  const random: ISecureRandom): TCryptoLibGenericArray<TBigInteger>;
var
  p, q: TBigInteger;
  qLength, minWeight, i, test, rem3, diff, j, prime, qRem: Int32;
  retryFlag: Boolean;
  LPrimeList: TCryptoLibInt32Array;
begin
  retryFlag := False;
  qLength := size - 1;
  minWeight := TBits.Asr32(size, 2);

  if (size <= 32) then
  begin
    while True do

    begin
      q := TBigInteger.Create(qLength, 2, random);

      p := q.ShiftLeft(1).Add(TBigInteger.One);

      if (not p.IsProbablePrime(certainty, True)) then
      begin
        continue;
      end;

      if ((certainty > 2) and (not(q.IsProbablePrime(certainty, True)))) then
      begin
        continue;
      end;

      break;
    end;
  end
  else
  begin
    while True do
    begin
      q := TBigInteger.Create(qLength, 0, random);

      i := 0;
      while i < System.Length(FPrimeLists) do
      begin
        test := q.Remainder(FBigPrimeProducts[i]).Int32Value;

        if (i = 0) then
        begin
          rem3 := test mod 3;
          if (rem3 <> 2) then
          begin
            diff := (2 * rem3) + 2;
            q := q.Add(TBigInteger.ValueOf(diff));
            test := (test + diff) mod FPrimeProducts[i];
          end;
        end;

        LPrimeList := FPrimeLists[i];
        for j := 0 to System.Pred(System.Length(LPrimeList)) do
        begin
          prime := LPrimeList[j];
          qRem := test mod prime;
          if ((qRem = 0) or (qRem = TBits.Asr32(prime, 1))) then
          begin
            q := q.Add(FSix);
            retryFlag := True;
            break;
          end;
        end;

        if retryFlag then
        begin
          i := 0;
          retryFlag := False;
        end
        else
        begin
          System.Inc(i);
        end;

      end;

      if (q.BitLength <> qLength) then
      begin
        continue;
      end;

      if (not(q.RabinMillerTest(2, random, True))) then
      begin
        continue;
      end;

      p := q.ShiftLeft(1).Add(TBigInteger.One);

      if (not(p.RabinMillerTest(certainty, random, True))) then
      begin
        continue;
      end;

      if ((certainty > 2) and (not q.RabinMillerTest(certainty - 2, random,
        True))) then
      begin
        continue;
      end;

      (*
        * Require a minimum weight of the NAF representation, since low-weight primes may be
        * weak against a version of the number-field-sieve for the discrete-logarithm-problem.
        *
        * See "The number field sieve for integers of low weight", Oliver Schirokauer.
      *)
      if (TWNafUtilities.GetNafWeight(p) < minWeight) then
      begin
        continue;
      end;

      break;
    end;
  end;

  result := TCryptoLibGenericArray<TBigInteger>.Create(p, q);
end;

{$IFNDEF _FIXINSIGHT_}

class function TDHParametersHelper.SelectGenerator(const p, q: TBigInteger;
  const random: ISecureRandom): TBigInteger;
var
  g, h, pMinusTwo: TBigInteger;
  // CompareResOne, CompareResTwo: Boolean;
begin
  pMinusTwo := p.Subtract(TBigInteger.Two);


  // (see: Handbook of Applied Cryptography 4.80)
  //
  // repeat
  // g := TBigIntegers.CreateRandomInRange(TBigInteger.Two, pMinusTwo, random);
  // CompareResOne := g.ModPow(TBigInteger.Two, p).Equals(TBigInteger.One);
  // CompareResTwo := g.ModPow(q, p).Equals(TBigInteger.One);
  // until ((not CompareResOne) and (not CompareResTwo));

  // RFC 2631 2.2.1.2 (and see: Handbook of Applied Cryptography 4.81)

  repeat
    h := TBigIntegers.CreateRandomInRange(TBigInteger.Two, pMinusTwo, random);
    g := h.ModPow(TBigInteger.Two, p);
  until ((not g.Equals(TBigInteger.One)));

  result := g;
end;
{$ENDIF}

end.
