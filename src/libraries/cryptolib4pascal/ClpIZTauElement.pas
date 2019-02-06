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

unit ClpIZTauElement;

{$I CryptoLib.inc}

interface

uses
  ClpBigInteger;

type
  IZTauElement = interface(IInterface)
    ['{607ABBF3-AE3E-45AC-B772-92423508528A}']

    function GetU: TBigInteger;
    function GetV: TBigInteger;

    // /**
    // * The &quot;real&quot; part of <code>&#955;</code>.
    // */
    property U: TBigInteger read GetU;
    // /**
    // * The &quot;<code>&#964;</code>-adic&quot; part of <code>&#955;</code>.
    // */
    property V: TBigInteger read GetV;

  end;

implementation

end.
