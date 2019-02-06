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

unit ClpIPreCompInfo;

{$I CryptoLib.inc}

interface

type

  /// <summary>
  /// Interface for classes storing precomputation data for multiplication
  /// algorithms. Used as a Memento (see GOF patterns) for
  /// &lt;code&gt;WNafMultiplier&lt;/code&gt;.
  /// </summary>
  IPreCompInfo = interface(IInterface)
    ['{8274B14C-C784-4964-9412-AAB3F7A36AB2}']
  end;

implementation

end.
