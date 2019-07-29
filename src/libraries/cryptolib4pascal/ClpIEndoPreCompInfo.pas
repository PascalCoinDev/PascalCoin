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

unit ClpIEndoPreCompInfo;

{$I CryptoLib.inc}

interface

uses
  ClpIECC,
  ClpIPreCompInfo;

type
  IEndoPreCompInfo = interface(IPreCompInfo)
    ['{84C79A80-8162-4079-8146-AA1D46A739ED}']

    function GetEndomorphism: IECEndomorphism;
    procedure SetEndomorphism(const value: IECEndomorphism);

    property Endomorphism: IECEndomorphism read GetEndomorphism
      write SetEndomorphism;

    function GetMappedPoint: IECPoint;
    procedure SetMappedPoint(const value: IECPoint);

    property MappedPoint: IECPoint read GetMappedPoint write SetMappedPoint;

  end;

implementation

end.
