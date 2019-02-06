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

unit ClpIAsymmetricKeyParameter;

{$I CryptoLib.inc}

interface

uses
  ClpICipherParameters;

type

  IAsymmetricKeyParameter = interface(ICipherParameters)
    ['{306A2860-9D12-46BF-9994-BCDCEF63214F}']

    function GetIsPrivate: Boolean;
    function GetPrivateKey: Boolean;
    property IsPrivate: Boolean read GetIsPrivate;
    property PrivateKey: Boolean read GetPrivateKey;
    function Equals(const other: IAsymmetricKeyParameter): Boolean;
    function GetHashCode(): {$IFDEF DELPHI}Int32; {$ELSE}PtrInt;
{$ENDIF DELPHI}
  end;

implementation

end.
