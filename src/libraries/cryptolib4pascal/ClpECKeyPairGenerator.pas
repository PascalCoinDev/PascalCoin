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

unit ClpECKeyPairGenerator;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  ClpBigInteger,
  ClpBits,
  ClpCryptoLibTypes,
  ClpECKeyParameters,
  ClpECCompUtilities,
  ClpIECKeyPairGenerator,
  ClpIAsn1Objects,
  ClpIKeyGenerationParameters,
  ClpIECKeyGenerationParameters,
  ClpECDomainParameters,
  ClpIECDomainParameters,
  ClpIECC,
  ClpMultipliers,
  ClpIMultipliers,
  ClpSecObjectIdentifiers,
  ClpCustomNamedCurves,
  ClpECNamedCurveTable,
  ClpX9ObjectIdentifiers,
  ClpIX9ECParameters,
  ClpAsymmetricCipherKeyPair,
  ClpIAsymmetricCipherKeyPair,
  ClpECPublicKeyParameters,
  ClpIECPublicKeyParameters,
  ClpECPrivateKeyParameters,
  ClpIECPrivateKeyParameters,
  ClpSecureRandom,
  ClpISecureRandom,
  ClpIAsymmetricCipherKeyPairGenerator;

resourcestring
  SAlgorithmNil = 'Algorithm Cannot be Empty';
  SInvalidKeySize = 'Unknown Key Size "%d"';

type
  TECKeyPairGenerator = class sealed(TInterfacedObject, IECKeyPairGenerator,
    IAsymmetricCipherKeyPairGenerator)

  strict private
  var
    Falgorithm: String;
    Fparameters: IECDomainParameters;
    Frandom: ISecureRandom;

  strict protected

    function CreateBasePointMultiplier(): IECMultiplier; virtual;

  public
    constructor Create(); overload;
    constructor Create(const algorithm: String); overload;

    procedure Init(const parameters: IKeyGenerationParameters);
    // /**
    // * Given the domain parameters this routine generates an EC key
    // * pair in accordance with X9.62 section 5.2.1 pages 26, 27.
    // */
    function GenerateKeyPair(): IAsymmetricCipherKeyPair;

    class function FindECCurveByOid(const oid: IDerObjectIdentifier)
      : IX9ECParameters; static;

    class function GetCorrespondingPublicKey(const privKey
      : IECPrivateKeyParameters): IECPublicKeyParameters; static;

  end;

implementation

{ TECKeyPairGenerator }

constructor TECKeyPairGenerator.Create;
begin
  Create('EC');
end;

constructor TECKeyPairGenerator.Create(const algorithm: String);
begin
  if (algorithm = '') then
    raise EArgumentNilCryptoLibException.CreateRes(@SAlgorithmNil);

  Falgorithm := TECKeyParameters.VerifyAlgorithmName(algorithm);
end;

function TECKeyPairGenerator.CreateBasePointMultiplier: IECMultiplier;
begin
  result := TFixedPointCombMultiplier.Create();
end;

class function TECKeyPairGenerator.FindECCurveByOid
  (const oid: IDerObjectIdentifier): IX9ECParameters;
var
  ecP: IX9ECParameters;
begin
  // TODO ECGost3410NamedCurves support (returns ECDomainParameters though)

  ecP := TCustomNamedCurves.GetByOid(oid);
  if (ecP = Nil) then
  begin
    ecP := TECNamedCurveTable.GetByOid(oid);
  end;

  result := ecP;
end;

function TECKeyPairGenerator.GenerateKeyPair: IAsymmetricCipherKeyPair;
var
  n, d: TBigInteger;
  minWeight: Int32;
  q: IECPoint;
begin
  n := Fparameters.n;
  minWeight := TBits.Asr32(n.BitLength, 2);

  while (true) do
  begin
    d := TBigInteger.Create(n.BitLength, Frandom);

    if ((d.CompareTo(TBigInteger.One) < 0) or (d.CompareTo(n) >= 0)) then
      continue;

    if (TWNafUtilities.GetNafWeight(d) < minWeight) then
    begin
      continue;
    end;

    break;
  end;

  q := CreateBasePointMultiplier().Multiply(Fparameters.G, d);

  result := TAsymmetricCipherKeyPair.Create
    (TECPublicKeyParameters.Create(Falgorithm, q, Fparameters)
    as IECPublicKeyParameters, TECPrivateKeyParameters.Create(Falgorithm, d,
    Fparameters) as IECPrivateKeyParameters);
end;

class function TECKeyPairGenerator.GetCorrespondingPublicKey
  (const privKey: IECPrivateKeyParameters): IECPublicKeyParameters;
var
  ec: IECDomainParameters;
  q: IECPoint;
begin
  ec := privKey.parameters;
  q := (TFixedPointCombMultiplier.Create() as IFixedPointCombMultiplier)
    .Multiply(ec.G, privKey.d);

  result := TECPublicKeyParameters.Create(privKey.AlgorithmName, q, ec);
end;

procedure TECKeyPairGenerator.Init(const parameters: IKeyGenerationParameters);
var
  ecP: IECKeyGenerationParameters;
  ecps: IX9ECParameters;
  oid: IDerObjectIdentifier;
begin
  if (Supports(parameters, IECKeyGenerationParameters, ecP)) then
  begin
    Fparameters := ecP.DomainParameters;
  end
  else
  begin
    case parameters.Strength of
      192:
        oid := TX9ObjectIdentifiers.Prime192v1;
      224:
        oid := TSecObjectIdentifiers.SecP224r1;
      239:
        oid := TX9ObjectIdentifiers.Prime239v1;
      256:
        oid := TX9ObjectIdentifiers.Prime256v1;
      384:
        oid := TSecObjectIdentifiers.SecP384r1;
      521:
        oid := TSecObjectIdentifiers.SecP521r1;
    else
      raise EInvalidParameterCryptoLibException.CreateResFmt(@SInvalidKeySize,
        [parameters.Strength]);

    end;

    ecps := FindECCurveByOid(oid);

    Fparameters := TECDomainParameters.Create(ecps.Curve, ecps.G, ecps.n,
      ecps.H, ecps.GetSeed());
  end;

  Frandom := parameters.random;

  if (Frandom = Nil) then
  begin
    Frandom := TSecureRandom.Create();
  end;

end;

end.
