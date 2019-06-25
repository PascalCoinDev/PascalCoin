{ *                 Github Repository <https://github.com/Xor-el>                   * }

{ *  Distributed under the MIT software license, see the accompanying file LICENSE  * }
{ *          or visit http://www.opensource.org/licenses/mit-license.php.           * }

{ *                              Acknowledgements:                                  * }
{ *                                                                                 * }
{ *      Thanks to Sphere 10 Software (http://www.sphere10.com/) for sponsoring     * }
{ *                           development of this library                           * }

{ * ******************************************************************************* * }

(* &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& *)

unit ClpIDHParameters;

{$I CryptoLib.inc}

interface

uses
  ClpICipherParameters,
  ClpIDHValidationParameters,
  ClpBigInteger;

type
  IDHParameters = interface(ICipherParameters)
    ['{6609D678-F9FB-48FD-A22F-52AFAE9EA5F8}']

    function GetG: TBigInteger;
    property g: TBigInteger read GetG;

    function GetP: TBigInteger;
    property p: TBigInteger read GetP;

    function GetQ: TBigInteger;
    property q: TBigInteger read GetQ;

    function GetJ: TBigInteger;
    property J: TBigInteger read GetJ;

    function GetM: Int32;
    /// <summary>The minimum bitlength of the private value.</summary>
    property m: Int32 read GetM;

    function GetL: Int32;
    /// <summary>The bitlength of the private value.</summary>
    property l: Int32 read GetL;

    function GetValidationParameters: IDHValidationParameters;
    property ValidationParameters: IDHValidationParameters
      read GetValidationParameters;

    function Equals(const other: IDHParameters): Boolean;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
  end;

implementation

end.
