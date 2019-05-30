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

unit ClpX25519KeyGenerationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpIX25519KeyGenerationParameters,
  ClpKeyGenerationParameters;

type
  TX25519KeyGenerationParameters = class sealed(TKeyGenerationParameters,
    IX25519KeyGenerationParameters)

  public
    constructor Create(const random: ISecureRandom);

  end;

implementation

{ TX25519KeyGenerationParameters }

constructor TX25519KeyGenerationParameters.Create(const random: ISecureRandom);
begin
  Inherited Create(random, 255);
end;

end.
