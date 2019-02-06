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

unit ClpDsaKeyPairGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpIDsaParameters,
  ClpIDsaKeyPairGenerator,
  ClpDsaPublicKeyParameters,
  ClpIDsaPublicKeyParameters,
  ClpDsaPrivateKeyParameters,
  ClpIDsaPrivateKeyParameters,
  ClpAsymmetricCipherKeyPair,
  ClpIAsymmetricCipherKeyPair,
  ClpIKeyGenerationParameters,
  ClpIDsaKeyGenerationParameters,
  ClpIAsymmetricCipherKeyPairGenerator,
  ClpBits,
  ClpBigInteger,
  ClpBigIntegers,
  ClpECAlgorithms,
  ClpCryptoLibTypes;

resourcestring
  SParametersCannotBeNil = '"parameters" Cannot Be Nil';

type

  /// <summary>
  /// <para>
  /// a DSA key pair generator.
  /// </para>
  /// <para>
  /// This Generates DSA keys in line with the method described in <i>
  /// FIPS 186-3 B.1 FFC Key Pair Generation</i>
  /// </para>
  /// </summary>
  TDsaKeyPairGenerator = class sealed(TInterfacedObject,
    IAsymmetricCipherKeyPairGenerator, IDsaKeyPairGenerator)

  strict private

  var
    Fparam: IDsaKeyGenerationParameters;

    class function GeneratePrivateKey(const q: TBigInteger;
      const random: ISecureRandom): TBigInteger; static;

    class function CalculatePublicKey(const p, g, x: TBigInteger): TBigInteger;
      static; inline;

  public

    procedure Init(const parameters: IKeyGenerationParameters);

    function GenerateKeyPair(): IAsymmetricCipherKeyPair;

  end;

implementation

{ TDsaKeyPairGenerator }

class function TDsaKeyPairGenerator.CalculatePublicKey(const p, g,
  x: TBigInteger): TBigInteger;
begin
  result := g.ModPow(x, p);
end;

function TDsaKeyPairGenerator.GenerateKeyPair: IAsymmetricCipherKeyPair;
var
  dsaParams: IDsaParameters;
  x, y: TBigInteger;
begin
  dsaParams := Fparam.parameters;

  x := GeneratePrivateKey(dsaParams.q, Fparam.random);
  y := CalculatePublicKey(dsaParams.p, dsaParams.g, x);

  result := TAsymmetricCipherKeyPair.Create(TDsaPublicKeyParameters.Create(y,
    dsaParams) as IDsaPublicKeyParameters, TDsaPrivateKeyParameters.Create(x,
    dsaParams) as IDsaPrivateKeyParameters);
end;

class function TDsaKeyPairGenerator.GeneratePrivateKey(const q: TBigInteger;
  const random: ISecureRandom): TBigInteger;
var
  minWeight: Int32;
  x, One: TBigInteger;
begin
  One := TBigInteger.One;
  result := Default (TBigInteger);
  // B.1.2 Key Pair Generation by Testing Candidates
  minWeight := TBits.Asr32(q.BitLength, 2);
  while (True) do
  begin
    // TODO Prefer this method? (change test cases that used fixed random)
    // B.1.1 Key Pair Generation Using Extra Random Bits
    // x := TBigInteger.Create(q.BitLength + 64, random).&Mod(q.Subtract(One)).Add(One);

    x := TBigIntegers.CreateRandomInRange(One, q.Subtract(One), random);
    if (TWNafUtilities.GetNafWeight(x) >= minWeight) then
    begin
      result := x;
      Exit;
    end;
  end;
end;

procedure TDsaKeyPairGenerator.Init(const parameters: IKeyGenerationParameters);
begin
  if (parameters = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SParametersCannotBeNil);
  end;

  // Note: If we start accepting instances of KeyGenerationParameters,
  // must apply constraint checking on strength (see DsaParametersGenerator.Init)

  Fparam := parameters as IDsaKeyGenerationParameters;
end;

end.
