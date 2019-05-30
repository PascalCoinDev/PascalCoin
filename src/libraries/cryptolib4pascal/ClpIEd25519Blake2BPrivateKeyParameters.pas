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

unit ClpIEd25519Blake2BPrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpEd25519,
  ClpIEd25519Blake2BPublicKeyParameters,
  ClpIAsymmetricKeyParameter,
  ClpCryptoLibTypes;

type
  IEd25519Blake2BPrivateKeyParameters = interface(IAsymmetricKeyParameter)
    ['{AA0786F9-11D8-4B36-B9AE-A108FE678774}']

    procedure Encode(const buf: TCryptoLibByteArray; off: Int32);
    function GetEncoded(): TCryptoLibByteArray;
    function GeneratePublicKey(): IEd25519Blake2BPublicKeyParameters;

    procedure Sign(algorithm: TEd25519.TEd25519Algorithm;
      const publicKey: IEd25519Blake2BPublicKeyParameters;
      const ctx, msg: TCryptoLibByteArray; msgOff, msgLen: Int32;
      const sig: TCryptoLibByteArray; sigOff: Int32);

    function Equals(const other: IEd25519Blake2BPrivateKeyParameters)
      : Boolean; overload;
  end;

implementation

end.
