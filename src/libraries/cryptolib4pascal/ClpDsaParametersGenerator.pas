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

unit ClpDsaParametersGenerator;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  Math,
  SysUtils,
  HlpSHA1,
  ClpIDigest,
  ClpISecureRandom,
  ClpDsaParameters,
  ClpIDsaParameters,
  ClpDsaValidationParameters,
  ClpIDsaValidationParameters,
  ClpIDsaParametersGenerator,
  ClpIDsaParameterGenerationParameters,
  ClpEncoders,
  ClpDigestUtilities,
  ClpBigInteger,
  ClpBigIntegers,
  ClpCryptoLibTypes;

resourcestring
  SInvalidLValue =
    'L Values Must be Between 1024 and 3072 and a Multiple of 1024';
  SInvalidNValueForSpecifiedL = 'N Must be " %d " for L = " %d "';
  SInvalidNValueForSpecifiedL_Two = 'N Must be " %d " or " %d " for L = " %d "';
  SDigestOutputSizeTooSmallForN =
    'Digest Output Size Too Small for Value of N Which is " %d "';
  SUnsupportedDigest =
    'Can Only Use SHA-1 For Generating FIPS 186-2 Parameters';
  SInvalidDsaKeyStrength =
    'Size Must Be From %d - %d and a multiple of %d, "%d"';

type

  /// <summary>
  /// Generate suitable parameters for DSA, in line with FIPS 186-2, or FIPS
  /// 186-3.
  /// </summary>
  TDsaParametersGenerator = class(TInterfacedObject, IDsaParametersGenerator)

  strict private
    Fdigest: IDigest;
    FL, FN, Fcertainty, Fiterations, FusageIndex: Int32;
    Frandom: ISecureRandom;
    Fuse186_3: Boolean;

    function IsProbablePrime(const x: TBigInteger): Boolean; inline;

    function GenerateParameters_FIPS186_2(): IDsaParameters;

    /// <summary>
    /// generate suitable parameters for DSA, in line with <i>FIPS 186-3 A.1
    /// Generation of the FFC Primes p and q</i>
    /// </summary>
    function GenerateParameters_FIPS186_3(): IDsaParameters;

    class function IsValidDsaStrength(strength: Int32): Boolean; static; inline;

    class function GetDefaultN(L: Int32): Int32; static; inline;
    class function GetMinimumIterations(L: Int32): Int32; static; inline;
    class procedure Hash(const d: IDigest;
      const input, output: TCryptoLibByteArray; outputPos: Int32);
      static; inline;

    class procedure Inc(const buf: TCryptoLibByteArray); static; inline;

    class function CalculateGenerator_FIPS186_2(const p, q: TBigInteger;
      const r: ISecureRandom): TBigInteger; static; inline;

    class function CalculateGenerator_FIPS186_3_Unverifiable(const p,
      q: TBigInteger; const r: ISecureRandom): TBigInteger; static; inline;

    class function CalculateGenerator_FIPS186_3_Verifiable(const d: IDigest;
      const p, q: TBigInteger; const seed: TCryptoLibByteArray; index: Int32)
      : TBigInteger; static; inline;

  public
    constructor Create(); overload;
    constructor Create(const digest: IDigest); overload;

    /// <summary>
    /// initialise the key generator.
    /// </summary>
    /// <param name="size">
    /// size of the key (range 2^512 -&amp;gt; 2^1024 - 64 bit increments)
    /// </param>
    /// <param name="certainty">
    /// measure of robustness of prime (for FIPS 186-2 compliance this should
    /// be at least 80).
    /// </param>
    /// <param name="random">
    /// random byte source.
    /// </param>
    procedure Init(size, certainty: Int32;
      const random: ISecureRandom); overload;

    /// <summary>
    /// initialise the key generator.
    /// </summary>
    /// <param name="size">
    /// size of the key (range 2^512 -&amp;gt; 2^1024 - 64 bit increments)
    /// </param>
    /// <param name="certainty">
    /// measure of robustness of prime (for FIPS 186-2 compliance this should
    /// be at least 80).
    /// </param>
    /// <param name="iterations">
    /// iterations
    /// </param>
    /// <param name="random">
    /// random byte source.
    /// </param>
    procedure Init(size, certainty, iterations: Int32;
      const random: ISecureRandom); overload;

    /// <summary>
    /// <para>
    /// Initialise the key generator for DSA 2.
    /// </para>
    /// <para>
    /// Use this init method if you need to generate parameters for DSA 2
    /// keys.
    /// </para>
    /// </summary>
    /// <param name="params">
    /// DSA 2 key generation parameters.
    /// </param>
    procedure Init(const params: IDsaParameterGenerationParameters); overload;

    /// <summary>
    /// <para>
    /// which generates the p and g values from the given parameters,
    /// returning the DSAParameters object.
    /// </para>
    /// <para>
    /// Note: can take a while...
    /// </para>
    /// </summary>
    /// <returns>
    /// a generated DSA parameters object.
    /// </returns>
    function GenerateParameters(): IDsaParameters; inline;

  end;

implementation

{ TDsaParametersGenerator }

function TDsaParametersGenerator.IsProbablePrime(const x: TBigInteger): Boolean;
begin
  result := x.IsProbablePrime(Fcertainty);
end;

class function TDsaParametersGenerator.IsValidDsaStrength
  (strength: Int32): Boolean;
begin
  // result := (strength >= 512) and (strength <= 1024) and ((strength mod 64) = 0);
  result := (strength >= 512) and (strength <= 1024) and
    ((strength and 63) = 0);
end;

class function TDsaParametersGenerator.GetDefaultN(L: Int32): Int32;
begin
  if L > 1024 then
  begin
    result := 256
  end
  else
  begin
    result := 160;
  end;
end;

class function TDsaParametersGenerator.GetMinimumIterations(L: Int32): Int32;
begin
  // Values based on FIPS 186-4 C.3 Table C.1
  if L <= 1024 then
  begin
    result := 40;
  end
  else
  begin
    result := (48 + 8 * ((L - 1) div 1024))
  end;
end;

class procedure TDsaParametersGenerator.Hash(const d: IDigest;
  const input, output: TCryptoLibByteArray; outputPos: Int32);
begin
  d.BlockUpdate(input, 0, System.Length(input));
  d.DoFinal(output, outputPos);
end;

class procedure TDsaParametersGenerator.Inc(const buf: TCryptoLibByteArray);
var
  i: Int32;
  b: Byte;
begin
  i := System.Length(buf) - 1;
  while i >= 0 do
  begin
    b := Byte((buf[i] + 1) and $FF);
    buf[i] := b;

    if (b <> 0) then
    begin
      break;
    end;
    System.Dec(i);
  end;
end;

constructor TDsaParametersGenerator.Create;
begin
  Create(TDigestUtilities.GetDigest('SHA-1'));
end;

class function TDsaParametersGenerator.CalculateGenerator_FIPS186_2(const p,
  q: TBigInteger; const r: ISecureRandom): TBigInteger;
var
  e, pSub2, h, g: TBigInteger;
begin
  result := Default (TBigInteger);
  e := p.subtract(TBigInteger.One).Divide(q);
  pSub2 := p.subtract(TBigInteger.Two);

  while True do
  begin
    h := TBigIntegers.CreateRandomInRange(TBigInteger.Two, pSub2, r);
    g := h.ModPow(e, p);
    if (g.BitLength > 1) then
    begin
      result := g;
      Exit;
    end;
  end;
end;

class function TDsaParametersGenerator.CalculateGenerator_FIPS186_3_Unverifiable
  (const p, q: TBigInteger; const r: ISecureRandom): TBigInteger;
begin
  result := CalculateGenerator_FIPS186_2(p, q, r);
end;

class function TDsaParametersGenerator.CalculateGenerator_FIPS186_3_Verifiable
  (const d: IDigest; const p, q: TBigInteger; const seed: TCryptoLibByteArray;
  index: Int32): TBigInteger;
var
  e, W, g: TBigInteger;
  ggen, U, smallw: TCryptoLibByteArray;
  count: Int32;
begin
  // A.2.3 Verifiable Canonical Generation of the Generator g
  e := p.subtract(TBigInteger.One).Divide(q);
  ggen := THex.Decode('6767656E');

  // 7. U = domain_parameter_seed || "ggen" || index || count.
  System.SetLength(U, System.Length(seed) + System.Length(ggen) + 1 + 2);
  System.Move(seed[0], U[0], System.Length(seed) * System.SizeOf(Byte));
  System.Move(ggen[0], U[System.Length(seed)], System.Length(ggen) *
    System.SizeOf(Byte));
  U[System.Length(U) - 3] := Byte(index);

  System.SetLength(smallw, d.GetDigestSize());

  count := 1;
  while count < (1 shl 16) do
  begin
    Inc(U);
    Hash(d, U, smallw, 0);
    W := TBigInteger.Create(1, smallw);
    g := W.ModPow(e, p);
    if (g.CompareTo(TBigInteger.Two) >= 0) then
    begin
      result := g;
      Exit;
    end;
    System.Inc(count);
  end;

  result := Default (TBigInteger);
end;

function TDsaParametersGenerator.GenerateParameters_FIPS186_2: IDsaParameters;
var
  seed, part1, part2, U, W, offset: TCryptoLibByteArray;
  n, i, counter, k, remaining: Int32;
  q, x, c, p, g: TBigInteger;
begin
  result := Nil;
  System.SetLength(seed, 20);
  System.SetLength(part1, 20);
  System.SetLength(part2, 20);
  System.SetLength(U, 20);
  n := (FL - 1) div 160;
  System.SetLength(W, FL div 8);

  if (not {$IFDEF FPC} (Supports(Fdigest.GetUnderlyingIHash, TSHA1))
{$ELSE} (Fdigest.GetUnderlyingIHash is TSHA1) {$ENDIF FPC}) then
  begin
    raise EInvalidParameterCryptoLibException.CreateRes(@SUnsupportedDigest);
  end;

  while True do
  begin
    Frandom.NextBytes(seed);

    Hash(Fdigest, seed, part1, 0);
    System.Move(seed[0], part2[0], System.Length(seed) * System.SizeOf(Byte));
    Inc(part2);
    Hash(Fdigest, part2, part2, 0);

    i := 0;
    while i <> System.Length(U) do
    begin
      U[i] := Byte(part1[i] xor part2[i]);
      System.Inc(i);
    end;

    U[0] := U[0] or Byte($80);
    U[19] := U[19] or Byte($01);

    q := TBigInteger.Create(1, U);

    if (not IsProbablePrime(q)) then
    begin
      continue;
    end;

    offset := System.Copy(seed);
    Inc(offset);
    counter := 0;
    while counter < 4096 do
    begin

      k := 1;
      while k <= n do
      begin
        Inc(offset);
        Hash(Fdigest, offset, W, System.Length(W) - (k * System.Length(part1)));
        System.Inc(k);
      end;

      remaining := System.Length(W) - (n * System.Length(part1));
      Inc(offset);
      Hash(Fdigest, offset, part1, 0);
      System.Move(part1[System.Length(part1) - remaining], W[0], remaining);

      W[0] := W[0] or Byte($80);

      x := TBigInteger.Create(1, W);

      c := x.&Mod(q.ShiftLeft(1));

      p := x.subtract(c.subtract(TBigInteger.One));

      if (p.BitLength <> FL) then
      begin
        System.Inc(counter);
        continue;
      end;

      if (IsProbablePrime(p)) then
      begin
        g := CalculateGenerator_FIPS186_2(p, q, Frandom);

        result := TDsaParameters.Create(p, q, g,
          TDsaValidationParameters.Create(seed, counter)
          as IDsaValidationParameters);
        Exit;
      end;

      System.Inc(counter);
    end;
  end;
end;

function TDsaParametersGenerator.GenerateParameters_FIPS186_3: IDsaParameters;
var
  d: IDigest;
  outlen, seedlen, n, { b, } counterLimit, counter, j, remaining: Int32;
  seed, W, output, offset: TCryptoLibByteArray;
  U, q, x, c, p, g: TBigInteger;
begin
  result := Nil;
  // A.1.1.2 Generation of the Probable Primes p and q Using an Approved Hash Function
  // TODO FIXME This should be configurable (digest size in bits must be >= N)
  d := Fdigest;
  outlen := d.GetDigestSize() * 8;

  // 1. Check that the (L, N) pair is in the list of acceptable (L, N pairs) (see Section 4.2). If
  // the pair is not in the list, then return INVALID.
  // Note: checked at initialisation

  // 2. If (seedlen < N), then return INVALID.
  // TODO FIXME This should be configurable (must be >= N)
  seedlen := FN;
  System.SetLength(seed, seedlen div 8);

  // 3. n = ceiling(L / outlen) - 1.
  n := (FL - 1) div outlen;

  // 4. b = L - 1 - (n * outlen).
  // b := (FL - 1) mod outlen;

  System.SetLength(W, FL div 8);

  System.SetLength(output, d.GetDigestSize());

  while True do
  begin
    // 5. Get an arbitrary sequence of seedlen bits as the domain_parameter_seed.
    Frandom.NextBytes(seed);

    // 6. U = Hash (domain_parameter_seed) mod 2^(N–1).
    Hash(d, seed, output, 0);

    U := TBigInteger.Create(1, output).&Mod(TBigInteger.One.ShiftLeft(FN - 1));

    // 7. q = 2^(N–1) + U + 1 – ( U mod 2).
    q := U.SetBit(0).SetBit(FN - 1);

    // 8. Test whether or not q is prime as specified in Appendix C.3.
    if (not IsProbablePrime(q)) then
    begin
      // 9. If q is not a prime, then go to step 5.
      continue;
    end;

    // 10. offset = 1.
    // Note: 'offset' value managed incrementally
    offset := System.Copy(seed);

    // 11. For counter = 0 to (4L – 1) do
    counterLimit := 4 * FL;
    counter := 0;
    while counter < counterLimit do
    begin
      // 11.1 For j = 0 to n do
      // Vj = Hash ((domain_parameter_seed + offset + j) mod 2^seedlen).
      // 11.2 W = V0 + (V1 * 2^outlen) + ... + (V^(n–1) * 2^((n–1) * outlen)) + ((Vn mod 2^b) * 2^(n * outlen)).

      j := 1;
      while j <= n do
      begin
        Inc(offset);
        Hash(d, offset, W, System.Length(W) - (j * System.Length(output)));
        System.Inc(j);
      end;

      remaining := System.Length(W) - (n * System.Length(output));
      Inc(offset);
      Hash(d, offset, output, 0);
      System.Move(output[System.Length(output) - remaining], W[0], remaining);

      // 11.3 X = W + 2^(L–1). Comment: 0 ≤ W < 2^(L–1); hence, 2^(L–1) ≤ X < 2^L.
      W[0] := W[0] or Byte($80);

      x := TBigInteger.Create(1, W);

      // 11.4 c = X mod 2q.
      c := x.&Mod(q.ShiftLeft(1));

      // 11.5 p = X - (c - 1). Comment: p ≡ 1 (mod 2q).
      p := x.subtract(c.subtract(TBigInteger.One));

      // 11.6 If (p < 2^(L-1)), then go to step 11.9
      if (p.BitLength <> FL) then
      begin
        System.Inc(counter);
        continue;
      end;

      // 11.7 Test whether or not p is prime as specified in Appendix C.3.
      if (IsProbablePrime(p)) then
      begin
        // 11.8 If p is determined to be prime, then return VALID and the values of p, q and
        // (optionally) the values of domain_parameter_seed and counter.
        if (FusageIndex >= 0) then
        begin
          g := CalculateGenerator_FIPS186_3_Verifiable(d, p, q, seed,
            FusageIndex);
          if (g.IsInitialized) then
          begin
            result := TDsaParameters.Create(p, q, g,
              TDsaValidationParameters.Create(seed, counter, FusageIndex)
              as IDsaValidationParameters);
            Exit;
          end;
        end;

        g := CalculateGenerator_FIPS186_3_Unverifiable(p, q, Frandom);

        result := TDsaParameters.Create(p, q, g,
          TDsaValidationParameters.Create(seed, counter)
          as IDsaValidationParameters);
        Exit;
      end;

      // 11.9 offset = offset + n + 1.      Comment: Increment offset; then, as part of
      // the loop in step 11, increment counter; if
      // counter < 4L, repeat steps 11.1 through 11.8.
      // Note: 'offset' value already incremented in inner loop
      System.Inc(counter);
    end;
    // 12. Go to step 5.
  end;

end;

function TDsaParametersGenerator.GenerateParameters: IDsaParameters;
begin
  if Fuse186_3 then
  begin
    result := GenerateParameters_FIPS186_3()
  end
  else
  begin
    result := GenerateParameters_FIPS186_2();
  end;
end;

constructor TDsaParametersGenerator.Create(const digest: IDigest);
begin
  Inherited Create();
  Fdigest := digest;
end;

procedure TDsaParametersGenerator.Init(size, certainty: Int32;
  const random: ISecureRandom);
begin
  Init(size, certainty, Max(GetMinimumIterations(size),
    (certainty + 1) div 2), random);
end;

procedure TDsaParametersGenerator.Init(size, certainty, iterations: Int32;
  const random: ISecureRandom);
begin
  if (not IsValidDsaStrength(size)) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SInvalidDsaKeyStrength,
      [512, 1024, 64, size]);
  end;
  FL := size;
  FN := GetDefaultN(size);
  Fcertainty := certainty;
  Fiterations := iterations;
  Frandom := random;
  Fuse186_3 := false;
  FusageIndex := -1;
end;

procedure TDsaParametersGenerator.Init(const params
  : IDsaParameterGenerationParameters);
var
  L, n: Int32;
begin
  L := params.L;
  n := params.n;

  // if (((L < 1024) or (L > 3072)) or ((L mod 1024) <> 0))
  if (((L < 1024) or (L > 3072)) or ((L and 1023) <> 0)) then
  begin
    raise EInvalidParameterCryptoLibException.CreateRes(@SInvalidLValue);
  end
  else if ((L = 1024) and (n <> 160)) then
  begin
    raise EInvalidParameterCryptoLibException.CreateResFmt
      (@SInvalidNValueForSpecifiedL, [160, 1024]);
  end
  else if ((L = 2048) and ((n <> 224) and (n <> 256))) then
  begin
    raise EInvalidParameterCryptoLibException.CreateResFmt
      (@SInvalidNValueForSpecifiedL_Two, [224, 256, 2048]);
  end
  else if ((L = 3072) and (n <> 256)) then
  begin
    raise EInvalidParameterCryptoLibException.CreateResFmt
      (@SInvalidNValueForSpecifiedL, [256, 3072]);
  end;

  if ((Fdigest.GetDigestSize * 8) < n) then
  begin
    raise EInvalidParameterCryptoLibException.CreateResFmt
      (@SDigestOutputSizeTooSmallForN, [n]);
  end;

  FL := L;
  FN := n;
  Fcertainty := params.certainty;
  Fiterations := Max(GetMinimumIterations(L), (Fcertainty + 1) div 2);
  Frandom := params.random;
  Fuse186_3 := True;
  FusageIndex := params.UsageIndex;
end;

end.
