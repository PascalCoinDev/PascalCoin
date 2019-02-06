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

unit ClpISecP384R1Custom;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIECC,
  ClpCryptoLibTypes;

type
  ISecP384R1FieldElement = Interface(IAbstractFpFieldElement)
    ['{EE28D1BA-2409-4915-99E9-EACD18C420DA}']

    function GetX: TCryptoLibUInt32Array;
    property X: TCryptoLibUInt32Array read GetX;
  end;

type
  ISecP384R1Point = Interface(IAbstractFpPoint)
    ['{2F1900E8-0B35-414A-B2EE-EDCA9763E2E8}']

  end;

type
  ISecP384R1Curve = Interface(IAbstractFpCurve)
    ['{50639F3D-E15C-4C3C-A7AA-7A8ACA243341}']

    function GetQ: TBigInteger;
    property Q: TBigInteger read GetQ;

  end;

type
  ISecP384R1LookupTable = Interface(IECLookupTable)
    ['{F1354F0B-577F-402C-A363-7761CF82DA43}']
  end;

implementation

end.
