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

unit ClpIScalarSplitParameters;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpIGlvEndomorphism;

type
  IScalarSplitParameters = interface(IInterface)
    ['{C36FF223-C4F3-4483-B280-A50EF95497AF}']

    function GetG1: TBigInteger;
    function GetG2: TBigInteger;
    function GetV1A: TBigInteger;
    function GetV1B: TBigInteger;
    function GetV2A: TBigInteger;
    function GetV2B: TBigInteger;
    function GetBits: Int32;

    property g1: TBigInteger read GetG1;
    property g2: TBigInteger read GetG2;
    property V1A: TBigInteger read GetV1A;
    property V1B: TBigInteger read GetV1B;
    property V2A: TBigInteger read GetV2A;
    property V2B: TBigInteger read GetV2B;
    property bits: Int32 read GetBits;

  end;

implementation

end.
