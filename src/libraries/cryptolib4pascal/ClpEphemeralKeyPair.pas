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

unit ClpEphemeralKeyPair;

{$I CryptoLib.inc}

interface

uses
  ClpIEphemeralKeyPair,
  ClpIAsymmetricCipherKeyPair,
  ClpIKeyEncoder,
  ClpCryptoLibTypes;

type
  TEphemeralKeyPair = class sealed(TInterfacedObject, IEphemeralKeyPair)

  strict private
  var
    FkeyPair: IAsymmetricCipherKeyPair;
    FpublicKeyEncoder: IKeyEncoder;

  public

    function GetKeyPair(): IAsymmetricCipherKeyPair; inline;

    function GetEncodedPublicKey: TCryptoLibByteArray; inline;

    constructor Create(const keyPair: IAsymmetricCipherKeyPair;
      const publicKeyEncoder: IKeyEncoder);

  end;

implementation

{ TEphemeralKeyPair }

constructor TEphemeralKeyPair.Create(const keyPair: IAsymmetricCipherKeyPair;
  const publicKeyEncoder: IKeyEncoder);
begin
  Inherited Create();
  FkeyPair := keyPair;
  FpublicKeyEncoder := publicKeyEncoder;
end;

function TEphemeralKeyPair.GetEncodedPublicKey: TCryptoLibByteArray;
begin
  result := FpublicKeyEncoder.GetEncoded(FkeyPair.Public);
end;

function TEphemeralKeyPair.GetKeyPair: IAsymmetricCipherKeyPair;
begin
  result := FkeyPair;
end;

end.
