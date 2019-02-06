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

unit ClpISecP256K1Custom;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIECC,
  ClpCryptoLibTypes;

type
  ISecP256K1FieldElement = Interface(IAbstractFpFieldElement)
    ['{F283DDDA-CB65-45E0-9239-2A202F2F1462}']

    function GetX: TCryptoLibUInt32Array;
    property X: TCryptoLibUInt32Array read GetX;
  end;

type
  ISecP256K1Point = Interface(IAbstractFpPoint)
    ['{E3B94BCB-F6A0-4140-A04E-7C26B1D81B7F}']

  end;

type
  ISecP256K1Curve = Interface(IAbstractFpCurve)
    ['{BBE4D704-8562-4C17-9149-CA33CFE7611F}']

    function GetQ: TBigInteger;
    property Q: TBigInteger read GetQ;

  end;

type
  ISecP256K1LookupTable = Interface(IECLookupTable)
    ['{0E204483-F303-49FD-AF66-0F30CF855CA9}']
  end;

implementation

end.
