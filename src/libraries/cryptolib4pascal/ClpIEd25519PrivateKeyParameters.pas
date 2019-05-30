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

unit ClpIEd25519PrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpEd25519,
  ClpIEd25519PublicKeyParameters,
  ClpIAsymmetricKeyParameter,
  ClpCryptoLibTypes;

type
  IEd25519PrivateKeyParameters = interface(IAsymmetricKeyParameter)
    ['{03CF8E9D-F462-4C77-B954-2519E31E625F}']

    procedure Encode(const buf: TCryptoLibByteArray; off: Int32);
    function GetEncoded(): TCryptoLibByteArray;
    function GeneratePublicKey(): IEd25519PublicKeyParameters;

    procedure Sign(algorithm: TEd25519.TEd25519Algorithm;
      const publicKey: IEd25519PublicKeyParameters;
      const ctx, msg: TCryptoLibByteArray; msgOff, msgLen: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32);

    function Equals(const other: IEd25519PrivateKeyParameters)
      : Boolean; overload;
  end;

implementation

end.
