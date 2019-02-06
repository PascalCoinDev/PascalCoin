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
  ClpCryptoLibTypes;

type

  IGlvTypeBParameters = interface(IInterface)
    ['{089AC2AB-15A1-47F5-BED0-C09EA77BECB9}']

    function GetBeta: TBigInteger;
    function GetBits: Int32;
    function GetG1: TBigInteger;
    function GetG2: TBigInteger;
    function GetLambda: TBigInteger;
    function GetV1: TCryptoLibGenericArray<TBigInteger>;
    function GetV2: TCryptoLibGenericArray<TBigInteger>;

    property beta: TBigInteger read GetBeta;
    property lambda: TBigInteger read GetLambda;
    property v1: TCryptoLibGenericArray<TBigInteger> read GetV1;
    property v2: TCryptoLibGenericArray<TBigInteger> read GetV2;
    property g1: TBigInteger read GetG1;
    property g2: TBigInteger read GetG2;
    property bits: Int32 read GetBits;

  end;

implementation

end.
