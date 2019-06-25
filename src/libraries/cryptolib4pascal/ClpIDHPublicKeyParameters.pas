{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpIDHPublicKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDHKeyParameters,
  ClpBigInteger;

type
  IDHPublicKeyParameters = interface(IDHKeyParameters)
    ['{F78EC20B-B591-42AB-87F3-22011F1DE05E}']

    function GetY: TBigInteger;

    function Equals(const other: IDHPublicKeyParameters): Boolean; overload;
    property y: TBigInteger read GetY;

  end;

implementation

end.
