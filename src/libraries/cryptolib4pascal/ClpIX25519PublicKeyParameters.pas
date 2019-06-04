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

unit ClpIX25519PublicKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIAsymmetricKeyParameter,
  ClpCryptoLibTypes;

type
  IX25519PublicKeyParameters = interface(IAsymmetricKeyParameter)
    ['{52D136C4-4DD1-4AF1-9AB8-0783136EF04A}']

    procedure Encode(const buf: TCryptoLibByteArray; off: Int32);
    function GetEncoded(): TCryptoLibByteArray;

    function Equals(const other: IX25519PublicKeyParameters): Boolean; overload;
  end;

implementation

end.
