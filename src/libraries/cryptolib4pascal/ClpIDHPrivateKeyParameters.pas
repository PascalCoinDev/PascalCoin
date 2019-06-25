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

unit ClpIDHPrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHKeyParameters,
  ClpBigInteger;

type
  IDHPrivateKeyParameters = interface(IDHKeyParameters)
    ['{946AD4C3-6B77-46F5-871C-C8958DD371E0}']

    function GetX: TBigInteger;

    function Equals(const other: IDHPrivateKeyParameters): Boolean; overload;
    property X: TBigInteger read GetX;

  end;

implementation

end.
