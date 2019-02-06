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

unit ClpECKeyParameters;

{$I CryptoLib.inc}

interface

uses

  SysUtils,
  ClpECGost3410NamedCurves,
  ClpCryptoLibTypes,
  ClpECDomainParameters,
  ClpIECKeyParameters,
  ClpIX9ECParameters,
  ClpIECDomainParameters,
  ClpIAsn1Objects,
  ClpIECKeyGenerationParameters,
  ClpAsymmetricKeyParameter,
  ClpECKeyGenerationParameters,
  ClpISecureRandom;

resourcestring
  SAlgorithmNil = 'Algorithm Cannot be Empty';
  SParameterNil = 'Parameter Cannot be Nil';
  SPublicKeyParamSetNil = 'PublicKeyParamSet Cannot be Nil';
  SInvalidPublicKeyParamSetNil =
    'OID Is Not a Valid Public Key Parameter Set  "PublicKeyParamSet"';
  SUnRecognizedAlgorithm = 'Unrecognised Algorithm: " %s, "Algorithm';

type
  TECKeyParameters = class abstract(TAsymmetricKeyParameter, IECKeyParameters)

  strict private

  const

    Falgorithms: array [0 .. 6] of String = ('EC', 'ECDSA', 'ECDH', 'ECDHC',
      'ECGOST3410', 'ECMQV', 'ECSCHNORR');

  var
    Falgorithm: String;
    Fparameters: IECDomainParameters;
    FpublicKeyParamSet: IDerObjectIdentifier;

  strict protected

    constructor Create(const algorithm: String; isPrivate: Boolean;
      const parameters: IECDomainParameters); overload;

    constructor Create(const algorithm: String; isPrivate: Boolean;
      const publicKeyParamSet: IDerObjectIdentifier); overload;

    function CreateKeyGenerationParameters(const random: ISecureRandom)
      : IECKeyGenerationParameters; inline;

    function GetAlgorithmName: String; inline;
    function GetPublicKeyParamSet: IDerObjectIdentifier; inline;
    function GetParameters: IECDomainParameters; inline;

    function Equals(const other: IECKeyParameters): Boolean; reintroduce; overload;

  public
    class function VerifyAlgorithmName(const algorithm: String): String; static;
    class function LookupParameters(const publicKeyParamSet
      : IDerObjectIdentifier): IECDomainParameters; static;
    property AlgorithmName: String read GetAlgorithmName;
    property parameters: IECDomainParameters read GetParameters;
    property publicKeyParamSet: IDerObjectIdentifier read GetPublicKeyParamSet;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}override;

  end;

implementation

uses
  ClpECKeyPairGenerator; // included here to avoid circular dependency :)

{ TECKeyParameters }

function TECKeyParameters.GetPublicKeyParamSet: IDerObjectIdentifier;
begin
  result := FpublicKeyParamSet;
end;

function TECKeyParameters.GetParameters: IECDomainParameters;
begin
  result := Fparameters;
end;

class function TECKeyParameters.LookupParameters(const publicKeyParamSet
  : IDerObjectIdentifier): IECDomainParameters;
var
  p: IECDomainParameters;
  x9: IX9ECParameters;

begin
  if publicKeyParamSet = Nil then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SPublicKeyParamSetNil);
  end;

  p := TECGost3410NamedCurves.GetByOid(publicKeyParamSet);

  if (p = Nil) then
  begin
    x9 := TECKeyPairGenerator.FindECCurveByOid(publicKeyParamSet);

    if (x9 = Nil) then
    begin
      raise EArgumentCryptoLibException.CreateRes
        (@SInvalidPublicKeyParamSetNil);

    end;

    p := TECDomainParameters.Create(x9.Curve, x9.G, x9.N, x9.H, x9.GetSeed());
  end;

  result := p;

end;

class function TECKeyParameters.VerifyAlgorithmName(const algorithm
  : String): String;
var
  upper: String;
  i: Int32;
  found: Boolean;
begin
  upper := UpperCase(algorithm);
  found := false;
  for i := System.Low(Falgorithms) to System.High(Falgorithms) do
  begin
    if (Falgorithms[i] = algorithm) then
    begin
      found := true;
      break;
    end;
  end;

  if (not found) then
  begin
    raise EArgumentCryptoLibException.CreateResFmt(@SUnRecognizedAlgorithm,
      [algorithm]);
  end;
  result := upper;
end;

constructor TECKeyParameters.Create(const algorithm: String; isPrivate: Boolean;
  const parameters: IECDomainParameters);
begin
  Inherited Create(isPrivate);
  if (algorithm = '') then
    raise EArgumentNilCryptoLibException.CreateRes(@SAlgorithmNil);

  if (parameters = Nil) then
    raise EArgumentNilCryptoLibException.CreateRes(@SParameterNil);

  Falgorithm := VerifyAlgorithmName(algorithm);
  Fparameters := parameters;
end;

constructor TECKeyParameters.Create(const algorithm: String; isPrivate: Boolean;
  const publicKeyParamSet: IDerObjectIdentifier);
begin
  Inherited Create(isPrivate);

  if (algorithm = '') then
    raise EArgumentNilCryptoLibException.CreateRes(@SAlgorithmNil);

  if (publicKeyParamSet = Nil) then
    raise EArgumentNilCryptoLibException.CreateRes(@SPublicKeyParamSetNil);

  Falgorithm := VerifyAlgorithmName(algorithm);
  Fparameters := LookupParameters(publicKeyParamSet);
  FpublicKeyParamSet := publicKeyParamSet;
end;

function TECKeyParameters.CreateKeyGenerationParameters
  (const random: ISecureRandom): IECKeyGenerationParameters;
begin
  if (publicKeyParamSet <> Nil) then
  begin
    result := TECKeyGenerationParameters.Create(publicKeyParamSet, random);
  end;

  result := TECKeyGenerationParameters.Create(parameters, random);
end;

function TECKeyParameters.Equals(const other: IECKeyParameters): Boolean;
begin
  if (other = Self as IECKeyParameters) then
  begin
    result := true;
    Exit;
  end;

  if (other = Nil) then
  begin
    result := false;
    Exit;
  end;
  result := Fparameters.Equals(other.parameters) and (inherited Equals(other));
end;

function TECKeyParameters.GetAlgorithmName: String;
begin
  result := Falgorithm;
end;

function TECKeyParameters.GetHashCode: {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
begin
  result := Fparameters.GetHashCode() xor (inherited GetHashCode());
end;

end.
