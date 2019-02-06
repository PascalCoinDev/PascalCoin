{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpIDsaValidationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes;

type
  IDsaValidationParameters = interface(IInterface)
    ['{F7C394CB-BDC3-47B5-835F-6216FBBF90F9}']

    function GetCounter: Int32;
    function GetUsageIndex: Int32;
    function GetSeed: TCryptoLibByteArray;

    function Equals(const other: IDsaValidationParameters): Boolean;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
    property counter: Int32 read GetCounter;
    property usageIndex: Int32 read GetUsageIndex;
    property seed: TCryptoLibByteArray read GetSeed;
  end;

implementation

end.
