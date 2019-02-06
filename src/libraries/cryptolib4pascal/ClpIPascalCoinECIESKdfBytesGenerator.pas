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

unit ClpIPascalCoinECIESKdfBytesGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpIBaseKdfBytesGenerator,
  ClpCryptoLibTypes;

type

  IPascalCoinECIESKdfBytesGenerator = interface(IBaseKdfBytesGenerator)
    ['{F6C7D34B-BA6A-45DB-B2A2-088F36557396}']

  end;

implementation

end.
