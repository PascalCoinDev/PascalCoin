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

unit ClpIECDomainParameters;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIECC,
  ClpCryptoLibTypes;

type
  IECDomainParameters = interface(IInterface)

    ['{FFF479CD-D7FD-455D-B70C-00D37F8E22A8}']

    function GetCurve: IECCurve;
    function GetG: IECPoint;
    function GetN: TBigInteger;
    function GetH: TBigInteger;

    function GetHInv: TBigInteger;
    function GetSeed: TCryptoLibByteArray;

    property Curve: IECCurve read GetCurve;
    property G: IECPoint read GetG;
    property N: TBigInteger read GetN;
    property H: TBigInteger read GetH;
    property HInv: TBigInteger read GetHInv;
    property Seed: TCryptoLibByteArray read GetSeed;
    function Equals(const other: IECDomainParameters): Boolean;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
  end;

implementation

end.
