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

unit ClpIHMacDsaKCalculator;

{$I ..\Include\CryptoLib.inc}

interface

uses
  ClpIDsaKCalculator;

type
  /// <summary>
  /// A deterministic K calculator based on the algorithm in section 3.2 of
  /// RFC 6979.
  /// </summary>
  IHMacDsaKCalculator = interface(IDsaKCalculator)
    ['{A075E2C3-2EE8-4CAC-BDF8-977408617B98}']

  end;

implementation

end.
