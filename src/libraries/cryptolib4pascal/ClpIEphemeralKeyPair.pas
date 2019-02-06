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

unit ClpIEphemeralKeyPair;

{$I CryptoLib.inc}

interface

uses
  ClpIAsymmetricCipherKeyPair,
  ClpCryptoLibTypes;

type
  IEphemeralKeyPair = interface(IInterface)
    ['{E3CEA842-F26D-445C-8DDE-BAB041018DA0}']

    function GetKeyPair(): IAsymmetricCipherKeyPair;

    function GetEncodedPublicKey(): TCryptoLibByteArray;

  end;

implementation

end.
