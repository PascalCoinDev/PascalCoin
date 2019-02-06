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

unit ClpAsymmetricCipherKeyPair;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpIAsymmetricKeyParameter,
  ClpIAsymmetricCipherKeyPair;

resourcestring
  SExpectedPublicKey = 'Expected a Public Key "publicParameter"';
  SExpectedPrivateKey = 'Expected a Private Key "privateParameter"';

type
  /// <summary>
  /// a holding class for public/private parameter pairs <br />
  /// </summary>
  TAsymmetricCipherKeyPair = class sealed(TInterfacedObject,
    IAsymmetricCipherKeyPair)

  strict private
  var
    FpublicParameter, FprivateParameter: IAsymmetricKeyParameter;

    function GetPrivate: IAsymmetricKeyParameter; inline;
    function GetPublic: IAsymmetricKeyParameter; inline;

  public

    /// <summary>
    /// basic constructor.
    /// </summary>
    /// <param name="publicParameter">
    /// publicParam a public key parameters object.
    /// </param>
    /// <param name="privateParameter">
    /// privateParam the corresponding private key parameters.
    /// </param>
    constructor Create(const publicParameter, privateParameter
      : IAsymmetricKeyParameter);

    /// <summary>
    /// return the public key parameters.
    /// </summary>
    property &Public: IAsymmetricKeyParameter read GetPublic;

    /// <summary>
    /// return the private key parameters.
    /// </summary>
    property &Private: IAsymmetricKeyParameter read GetPrivate;

  end;

implementation

{ TAsymmetricCipherKeyPair }

constructor TAsymmetricCipherKeyPair.Create(const publicParameter,
  privateParameter: IAsymmetricKeyParameter);
begin
  if (publicParameter.IsPrivate) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SExpectedPublicKey);
  end;
  if (not(privateParameter.IsPrivate)) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SExpectedPrivateKey);
  end;

  FpublicParameter := publicParameter;
  FprivateParameter := privateParameter;
end;

function TAsymmetricCipherKeyPair.GetPrivate: IAsymmetricKeyParameter;
begin
  Result := FprivateParameter;
end;

function TAsymmetricCipherKeyPair.GetPublic: IAsymmetricKeyParameter;
begin
  Result := FpublicParameter;
end;

end.
