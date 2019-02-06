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

unit ClpIWNafPreCompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpIECC,
  ClpIPreCompInfo;

type
  IWNafPreCompInfo = interface(IPreCompInfo)
    ['{DB29636B-A1EB-40C3-96C2-0B7830A55DB9}']

    function GetPreComp: TCryptoLibGenericArray<IECPoint>;
    procedure SetPreComp(const Value: TCryptoLibGenericArray<IECPoint>);
    function GetPreCompNeg: TCryptoLibGenericArray<IECPoint>;
    procedure SetPreCompNeg(const Value: TCryptoLibGenericArray<IECPoint>);
    function GetTwice: IECPoint;
    procedure SetTwice(const Value: IECPoint);

    property PreComp: TCryptoLibGenericArray<IECPoint> read GetPreComp
      write SetPreComp;
    property PreCompNeg: TCryptoLibGenericArray<IECPoint> read GetPreCompNeg
      write SetPreCompNeg;
    property Twice: IECPoint read GetTwice write SetTwice;

  end;

implementation

end.
