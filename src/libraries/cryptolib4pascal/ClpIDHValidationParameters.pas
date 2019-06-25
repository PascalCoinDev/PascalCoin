{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpIDHValidationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes;

type
  IDHValidationParameters = interface(IInterface)
    ['{6F7404A7-0588-4154-8955-8C1A5C757B17}']

    function GetCounter: Int32;
    function GetSeed: TCryptoLibByteArray;

    function Equals(const other: IDHValidationParameters): Boolean;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
    property counter: Int32 read GetCounter;
    property seed: TCryptoLibByteArray read GetSeed;
  end;

implementation

end.
