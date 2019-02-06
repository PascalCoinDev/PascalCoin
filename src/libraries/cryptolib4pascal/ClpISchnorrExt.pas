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

unit ClpISchnorrExt;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpISchnorr,
  ClpBigInteger;

type
  /// <summary>
  /// An "extended" interface for classes implementing Schnorr-style algorithms, that provides access
  /// to the group order.
  /// </summary>
  ISchnorrExt = interface(ISchnorr)
    ['{0BCED764-F352-4C6C-B30C-9E0D7A2B042B}']

    function GetOrder: TBigInteger;
    /// <summary>The order of the group that the r, s values in signatures belong to.</summary>
    property Order: TBigInteger read GetOrder;

  end;

implementation

end.
