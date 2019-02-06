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

unit ClpIDsaExt;

{$I CryptoLib.inc}

interface

uses
  ClpIDsa,
  ClpBigInteger;

type
  /// <summary>
  /// An "extended" interface for classes implementing DSA-style algorithms, that provides access
  /// to the group order.
  /// </summary>
  IDsaExt = interface(IDsa)
    ['{FF9421DB-97F1-4409-AC00-B0000EE5EAFB}']

    function GetOrder: TBigInteger;
    /// <summary>The order of the group that the r, s values in signatures belong to.</summary>
    property Order: TBigInteger read GetOrder;

  end;

implementation

end.
