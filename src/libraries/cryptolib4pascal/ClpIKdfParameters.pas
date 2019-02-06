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

unit ClpIKdfParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDerivationParameters,
  ClpCryptoLibTypes;

type

  IKdfParameters = interface(IDerivationParameters)
    ['{2DCF1BDD-90A5-4501-8F7A-F22E896A0219}']

    function GetSharedSecret(): TCryptoLibByteArray;
    function GetIV(): TCryptoLibByteArray;

  end;

implementation

end.
