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

unit ClpIDsaPrivateKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDsaKeyParameters,
  ClpBigInteger;

type
  IDsaPrivateKeyParameters = interface(IDsaKeyParameters)
    ['{A956E21D-0A60-4073-8F17-5EA8B4615B68}']

    function GetX: TBigInteger;

    function Equals(const other: IDsaPrivateKeyParameters): Boolean;
    property X: TBigInteger read GetX;

  end;

implementation

end.
