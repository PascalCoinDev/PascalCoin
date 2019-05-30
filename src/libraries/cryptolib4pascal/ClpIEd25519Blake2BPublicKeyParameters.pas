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

unit ClpIEd25519Blake2BPublicKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIAsymmetricKeyParameter,
  ClpCryptoLibTypes;

type
  IEd25519Blake2BPublicKeyParameters = interface(IAsymmetricKeyParameter)
    ['{54BC984B-F087-463C-AB06-BCB8C61D9770}']

    procedure Encode(const buf: TCryptoLibByteArray; off: Int32);
    function GetEncoded(): TCryptoLibByteArray;

    function Equals(const other: IEd25519Blake2BPublicKeyParameters)
      : Boolean; overload;
  end;

implementation

end.
