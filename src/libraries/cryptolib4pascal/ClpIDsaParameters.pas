{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpIDsaParameters;

{$I CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpIDsaValidationParameters,
  ClpBigInteger;

type
  IDsaParameters = interface(ICipherParameters)
    ['{6A088962-AF58-4699-83B9-ADDABFC65A7E}']

    function GetG: TBigInteger;
    function GetP: TBigInteger;
    function GetQ: TBigInteger;
    function GetValidationParameters: IDsaValidationParameters;

    function Equals(const other: IDsaParameters): Boolean;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
    property p: TBigInteger read GetP;
    property q: TBigInteger read GetQ;
    property g: TBigInteger read GetG;
    property ValidationParameters: IDsaValidationParameters
      read GetValidationParameters;

  end;

implementation

end.
