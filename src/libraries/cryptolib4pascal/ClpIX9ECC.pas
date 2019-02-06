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

unit ClpIX9ECC;

{$I CryptoLib.inc}

interface

uses
  ClpIECC,
  ClpIAsn1Objects,
  ClpCryptoLibTypes;

type
  IX9FieldID = interface(IAsn1Encodable)

    ['{12A8969E-8050-4BB2-87F7-F4E155A35DCE}']

    function GetIdentifier: IDerObjectIdentifier;
    function GetParameters: IAsn1Object;

    property Identifier: IDerObjectIdentifier read GetIdentifier;

    property Parameters: IAsn1Object read GetParameters;

  end;

type
  IX9FieldElement = interface(IAsn1Encodable)
    ['{7B055B2C-04BB-438B-B590-9A157F6412C0}']

    function GetValue: IECFieldElement;

    property Value: IECFieldElement read GetValue;

  end;

type

  IX9ECPoint = interface(IAsn1Encodable)
    ['{B91190B8-A56A-4231-9687-24E4BB1397C7}']

    function GetPointEncoding(): TCryptoLibByteArray;
    function GetIsPointCompressed: Boolean;
    function GetPoint: IECPoint;
    property Point: IECPoint read GetPoint;
    property IsPointCompressed: Boolean read GetIsPointCompressed;

  end;

type
  IX9Curve = interface(IAsn1Encodable)
    ['{BD78E2A1-C079-461C-8962-C4834DFA1478}']

    function GetCurve: IECCurve;

    function GetSeed(): TCryptoLibByteArray;

    property curve: IECCurve read GetCurve;

  end;

implementation

end.
