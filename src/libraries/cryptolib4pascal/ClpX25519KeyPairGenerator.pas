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

unit ClpX25519KeyPairGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpAsymmetricCipherKeyPair,
  ClpIX25519KeyPairGenerator,
  ClpIX25519PrivateKeyParameters,
  ClpX25519PrivateKeyParameters,
  ClpIX25519PublicKeyParameters,
  ClpISecureRandom,
  ClpIKeyGenerationParameters,
  ClpIAsymmetricCipherKeyPair,
  ClpIAsymmetricCipherKeyPairGenerator;

type
  TX25519KeyPairGenerator = class sealed(TInterfacedObject,
    IX25519KeyPairGenerator, IAsymmetricCipherKeyPairGenerator)

  strict private
  var
    FRandom: ISecureRandom;

  public
    procedure Init(const parameters: IKeyGenerationParameters);

    function GenerateKeyPair(): IAsymmetricCipherKeyPair;

  end;

implementation

{ TX25519KeyPairGenerator }

function TX25519KeyPairGenerator.GenerateKeyPair: IAsymmetricCipherKeyPair;
var
  privateKey: IX25519PrivateKeyParameters;
  publicKey: IX25519PublicKeyParameters;
begin
  privateKey := TX25519PrivateKeyParameters.Create(FRandom);
  publicKey := privateKey.GeneratePublicKey();
  result := TAsymmetricCipherKeyPair.Create(publicKey, privateKey);
end;

procedure TX25519KeyPairGenerator.Init(const parameters
  : IKeyGenerationParameters);
begin
  FRandom := parameters.random;
end;

end.
