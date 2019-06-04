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

unit ClpIX25519PrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIX25519PublicKeyParameters,
  ClpIAsymmetricKeyParameter,
  ClpCryptoLibTypes;

type
  IX25519PrivateKeyParameters = interface(IAsymmetricKeyParameter)
    ['{6C7D2CD5-33A1-4153-A84C-70455CA69729}']

    procedure Encode(const buf: TCryptoLibByteArray; off: Int32);
    function GetEncoded(): TCryptoLibByteArray;
    function GeneratePublicKey(): IX25519PublicKeyParameters;
    procedure GenerateSecret(const publicKey: IX25519PublicKeyParameters;
      const buf: TCryptoLibByteArray; off: Int32);

    function Equals(const other: IX25519PrivateKeyParameters): Boolean;
      overload;
  end;

implementation

end.
