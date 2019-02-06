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

unit ClpECKeyGenerationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpISecureRandom,
  ClpIECKeyGenerationParameters,
  ClpIECDomainParameters,
  ClpIAsn1Objects,
  ClpKeyGenerationParameters;

type
  TECKeyGenerationParameters = class sealed(TKeyGenerationParameters,
    IECKeyGenerationParameters)

  strict private
  var
    FdomainParams: IECDomainParameters;
    FpublicKeyParamSet: IDerObjectIdentifier;

    function GetDomainParameters: IECDomainParameters;
    function GetPublicKeyParamSet: IDerObjectIdentifier;

  public
    constructor Create(const domainParameters: IECDomainParameters;
      const random: ISecureRandom); overload;
    constructor Create(const publicKeyParamSet: IDerObjectIdentifier;
      const random: ISecureRandom); overload;
    property domainParameters: IECDomainParameters read GetDomainParameters;
    property publicKeyParamSet: IDerObjectIdentifier read GetPublicKeyParamSet;

  end;

implementation

uses
  ClpECKeyParameters; // included here to avoid circular dependency :)

{ TECKeyGenerationParameters }

constructor TECKeyGenerationParameters.Create(const domainParameters
  : IECDomainParameters; const random: ISecureRandom);
begin
  Inherited Create(random, domainParameters.N.BitLength);
  FdomainParams := domainParameters;
end;

constructor TECKeyGenerationParameters.Create(const publicKeyParamSet
  : IDerObjectIdentifier; const random: ISecureRandom);
begin
  Create(TECKeyParameters.LookupParameters(publicKeyParamSet), random);
  FpublicKeyParamSet := publicKeyParamSet;
end;

function TECKeyGenerationParameters.GetDomainParameters: IECDomainParameters;
begin
  Result := FdomainParams;
end;

function TECKeyGenerationParameters.GetPublicKeyParamSet: IDerObjectIdentifier;
begin
  Result := FpublicKeyParamSet;
end;

end.
