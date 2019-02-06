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

unit ClpISecP521R1Custom;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIECC,
  ClpCryptoLibTypes;

type
  ISecP521R1FieldElement = Interface(IAbstractFpFieldElement)
    ['{30C8C42B-5099-4387-BEC9-66D6D8901BB4}']

    function GetX: TCryptoLibUInt32Array;
    property X: TCryptoLibUInt32Array read GetX;
  end;

type
  ISecP521R1Point = Interface(IAbstractFpPoint)
    ['{BBE6F8EB-1C56-4B69-B4DE-93EF3079939A}']

  end;

type
  ISecP521R1Curve = Interface(IAbstractFpCurve)
    ['{B2AACD7E-6EF2-45E2-8126-FB87D6DB65B1}']

    function GetQ: TBigInteger;
    property Q: TBigInteger read GetQ;

  end;

type
  ISecP521R1LookupTable = Interface(IECLookupTable)
    ['{3A647191-94A9-483D-9AC5-57FEFDBA3060}']
  end;

implementation

end.
