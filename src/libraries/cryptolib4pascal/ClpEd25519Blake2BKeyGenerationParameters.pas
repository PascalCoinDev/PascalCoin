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

unit ClpEd25519Blake2BKeyGenerationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpIEd25519Blake2BKeyGenerationParameters,
  ClpKeyGenerationParameters;

type
  TEd25519Blake2BKeyGenerationParameters = class sealed
    (TKeyGenerationParameters, IEd25519Blake2BKeyGenerationParameters)

  public
    constructor Create(const random: ISecureRandom);

  end;

implementation

{ TEd25519Blake2BKeyGenerationParameters }

constructor TEd25519Blake2BKeyGenerationParameters.Create
  (const random: ISecureRandom);
begin
  Inherited Create(random, 256);
end;

end.
