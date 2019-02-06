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

unit ClpIGF2Polynomial;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpIPolynomial;

type
  IGF2Polynomial = interface(IPolynomial)
    ['{B60318B7-B459-4C09-9D0A-67C84DF794B3}']

    function GetExponents: TCryptoLibInt32Array;

    function Equals(const other: IGF2Polynomial): Boolean;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
    property exponents: TCryptoLibInt32Array read GetExponents;

  end;

implementation

end.
