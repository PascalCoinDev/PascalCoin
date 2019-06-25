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

unit ClpIDHValidationParams;

{$I CryptoLib.inc}

interface

uses
  ClpIAsn1Objects;

type
  IDHValidationParams = interface(IAsn1Encodable)
    ['{A75D3486-080A-43F5-9296-9C74B7DEE7DC}']

    function GetSeed: IDerBitString;
    property Seed: IDerBitString read GetSeed;

    function GetPGenCounter: IDerInteger;
    property PGenCounter: IDerInteger read GetPGenCounter;

  end;

implementation

end.
