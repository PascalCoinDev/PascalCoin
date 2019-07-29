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

unit ClpIGlvTypeAParameters;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIScalarSplitParameters,
  ClpCryptoLibTypes;

type

  IGlvTypeAParameters = interface(IInterface)
    ['{B5DDABB5-B51C-41F4-B2FD-6C8733300502}']

    function GetI: TBigInteger;
    function GetLambda: TBigInteger;
    function GetSplitParams: IScalarSplitParameters;

    property I: TBigInteger read GetI;
    property lambda: TBigInteger read GetLambda;
    property splitParams: IScalarSplitParameters read GetSplitParams;

  end;

implementation

end.
