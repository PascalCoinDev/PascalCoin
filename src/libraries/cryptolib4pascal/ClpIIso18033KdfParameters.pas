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

unit ClpIIso18033KdfParameters;

{$I CryptoLib.inc}

interface

uses
  ClpIDerivationParameters,
  ClpCryptoLibTypes;

type

  IIso18033KdfParameters = interface(IDerivationParameters)
    ['{FFECB534-B0D3-47C1-83EE-B91B5760F07D}']

    function GetSeed(): TCryptoLibByteArray;

  end;

implementation

end.
