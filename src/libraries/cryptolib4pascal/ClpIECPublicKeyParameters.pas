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

unit ClpIECPublicKeyParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIECC,
  ClpIECKeyParameters;

type

  IECPublicKeyParameters = interface(IECKeyParameters)
    ['{4BABC163-847A-4FE2-AA16-5CD100F76124}']

    function GetQ: IECPoint;

    property Q: IECPoint read GetQ;
  end;

implementation

end.
