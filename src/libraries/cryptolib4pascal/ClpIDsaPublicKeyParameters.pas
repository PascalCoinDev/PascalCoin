{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpIDsaPublicKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDsaKeyParameters,
  ClpBigInteger;

type
  IDsaPublicKeyParameters = interface(IDsaKeyParameters)
    ['{B3F14490-AF3D-437C-9E42-B6940B2ECADE}']

    function GetY: TBigInteger;

    function Equals(const other: IDsaPublicKeyParameters): Boolean;
    property y: TBigInteger read GetY;

  end;

implementation

end.
