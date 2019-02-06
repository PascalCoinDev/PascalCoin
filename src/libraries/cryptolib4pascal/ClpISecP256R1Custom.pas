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

unit ClpISecP256R1Custom;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIECC,
  ClpCryptoLibTypes;

type
  ISecP256R1FieldElement = Interface(IAbstractFpFieldElement)
    ['{963E19D3-B86F-4F78-8C10-54BF82030F09}']

    function GetX: TCryptoLibUInt32Array;
    property X: TCryptoLibUInt32Array read GetX;
  end;

type
  ISecP256R1Point = Interface(IAbstractFpPoint)
    ['{EDD8CEBA-B86F-41F0-96B3-958525EF3272}']

  end;

type
  ISecP256R1Curve = Interface(IAbstractFpCurve)
    ['{D6B64687-91B2-4281-B099-3B3DCFB330DB}']

    function GetQ: TBigInteger;
    property Q: TBigInteger read GetQ;

  end;

type
  ISecP256R1LookupTable = Interface(IECLookupTable)
    ['{87BF97BA-18D2-4248-ABEB-8E429998E9D9}']
  end;

implementation

end.
