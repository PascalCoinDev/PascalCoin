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

unit ClpIX9ECParameters;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpIAsn1Objects,
  ClpBigInteger,
  ClpIX9ECC,
  ClpIECC;

type

  IX9ECParameters = interface(IAsn1Encodable)
    ['{9D037623-C439-46E8-960B-92E81386D51D}']

    function GetCurve: IECCurve;
    function GetG: IECPoint;
    function GetH: TBigInteger;
    function GetN: TBigInteger;
    function GetBaseEntry: IX9ECPoint;
    function GetCurveEntry: IX9Curve;
    function GetFieldIDEntry: IX9FieldID;

    property Curve: IECCurve read GetCurve;
    property G: IECPoint read GetG;
    property N: TBigInteger read GetN;
    property H: TBigInteger read GetH;
    property CurveEntry: IX9Curve read GetCurveEntry;
    property FieldIDEntry: IX9FieldID read GetFieldIDEntry;
    property BaseEntry: IX9ECPoint read GetBaseEntry;

    function GetSeed(): TCryptoLibByteArray;

  end;

implementation

end.
