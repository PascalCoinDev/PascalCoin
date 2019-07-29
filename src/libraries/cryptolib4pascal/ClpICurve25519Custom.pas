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

unit ClpICurve25519Custom;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIECC,
  ClpCryptoLibTypes;

type
  ICurve25519FieldElement = Interface(IAbstractFpFieldElement)
    ['{50046C65-BACE-4E68-9AEF-09AAD33DFD62}']

    function GetX: TCryptoLibUInt32Array;
    property X: TCryptoLibUInt32Array read GetX;
  end;

type
  ICurve25519Point = Interface(IAbstractFpPoint)
    ['{49280930-32AC-4F84-BBCE-C9A9DF18E71E}']

    function CalculateJacobianModifiedW(const z: ICurve25519FieldElement;
      const ZSquared: TCryptoLibUInt32Array): ICurve25519FieldElement;
    function GetJacobianModifiedW(): ICurve25519FieldElement;
    function TwiceJacobianModified(calculateW: Boolean): ICurve25519Point;

  end;

type
  ICurve25519 = Interface(IAbstractFpCurve)
    ['{56BB2C20-454C-4C42-A603-AD7429362D82}']

    function GetQ: TBigInteger;
    property Q: TBigInteger read GetQ;

  end;

type
  ICurve25519LookupTable = Interface(IAbstractECLookupTable)
    ['{79FE1276-3D22-4A20-A4F1-58F0C0532BAC}']
  end;

implementation

end.
