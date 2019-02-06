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

unit ClpDsaKeyGenerationParameters;

{$I ..\..\Include\CryptoLib.inc}

interface

uses
  ClpBigInteger,
  ClpISecureRandom,
  ClpIDsaParameters,
  ClpIDsaKeyGenerationParameters,
  ClpKeyGenerationParameters;

type
  TDsaKeyGenerationParameters = class sealed(TKeyGenerationParameters,
    IDsaKeyGenerationParameters)
  strict private
  var
    Fparameters: IDsaParameters;

    function GetParameters: IDsaParameters; inline;

  public
    constructor Create(const random: ISecureRandom;
      const parameters: IDsaParameters);

    property parameters: IDsaParameters read GetParameters;
  end;

implementation

{ TDsaKeyGenerationParameters }

constructor TDsaKeyGenerationParameters.Create(const random: ISecureRandom;
  const parameters: IDsaParameters);
var
  P: TBigInteger;
begin
  P := parameters.P;
  Inherited Create(random, P.BitLength - 1);
  Fparameters := parameters;
end;

function TDsaKeyGenerationParameters.GetParameters: IDsaParameters;
begin
  result := Fparameters;
end;

end.
