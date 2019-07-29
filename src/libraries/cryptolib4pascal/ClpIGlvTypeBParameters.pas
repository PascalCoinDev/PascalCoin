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

unit ClpIGlvTypeBParameters;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIScalarSplitParameters,
  ClpCryptoLibTypes;

type

  IGlvTypeBParameters = interface(IInterface)
    ['{089AC2AB-15A1-47F5-BED0-C09EA77BECB9}']

    function GetLambda: TBigInteger;
    function GetBeta: TBigInteger;
    function GetSplitParams: IScalarSplitParameters;

    property lambda: TBigInteger read GetLambda;
    property beta: TBigInteger read GetBeta;
    property splitParams: IScalarSplitParameters read GetSplitParams;

  end;

implementation

end.
