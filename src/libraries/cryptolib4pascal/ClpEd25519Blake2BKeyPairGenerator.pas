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

unit ClpEd25519Blake2BKeyPairGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpAsymmetricCipherKeyPair,
  ClpIEd25519Blake2BKeyPairGenerator,
  ClpIEd25519Blake2BPrivateKeyParameters,
  ClpEd25519Blake2BPrivateKeyParameters,
  ClpIEd25519Blake2BPublicKeyParameters,
  ClpISecureRandom,
  ClpIKeyGenerationParameters,
  ClpIAsymmetricCipherKeyPair,
  ClpIAsymmetricCipherKeyPairGenerator;

type
  TEd25519Blake2BKeyPairGenerator = class(TInterfacedObject,
    IEd25519Blake2BKeyPairGenerator, IAsymmetricCipherKeyPairGenerator)

  strict private
  var
    FRandom: ISecureRandom;

  public
    procedure Init(const parameters: IKeyGenerationParameters);

    function GenerateKeyPair(): IAsymmetricCipherKeyPair;

  end;

implementation

{ TEd25519Blake2BKeyPairGenerator }

function TEd25519Blake2BKeyPairGenerator.GenerateKeyPair
  : IAsymmetricCipherKeyPair;
var
  privateKey: IEd25519Blake2BPrivateKeyParameters;
  publicKey: IEd25519Blake2BPublicKeyParameters;
begin
  privateKey := TEd25519Blake2BPrivateKeyParameters.Create(FRandom);
  publicKey := privateKey.GeneratePublicKey();
  result := TAsymmetricCipherKeyPair.Create(publicKey, privateKey);
end;

procedure TEd25519Blake2BKeyPairGenerator.Init(const parameters
  : IKeyGenerationParameters);
begin
  FRandom := parameters.random;
end;

end.
