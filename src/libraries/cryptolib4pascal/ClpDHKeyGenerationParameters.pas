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

unit ClpDHKeyGenerationParameters;

{$I CryptoLib.inc}

interface

uses
  ClpISecureRandom,
  ClpIDHParameters,
  ClpIDHKeyGenerationParameters,
  ClpKeyGenerationParameters;

type
  TDHKeyGenerationParameters = class sealed(TKeyGenerationParameters,
    IDHKeyGenerationParameters)
  strict private
  var
    Fparameters: IDHParameters;

    function GetParameters: IDHParameters; inline;

    class function GetStrengthLocal(const parameters: IDHParameters): Int32;
      static; inline;

  public
    constructor Create(const random: ISecureRandom;
      const parameters: IDHParameters);

    property parameters: IDHParameters read GetParameters;
  end;

implementation

{ TDHKeyGenerationParameters }

function TDHKeyGenerationParameters.GetParameters: IDHParameters;
begin
  result := Fparameters;
end;

class function TDHKeyGenerationParameters.GetStrengthLocal(const parameters
  : IDHParameters): Int32;
begin
  if parameters.L <> 0 then
  begin
    result := parameters.L;
  end
  else
  begin
    result := parameters.P.BitLength;
  end;
end;

constructor TDHKeyGenerationParameters.Create(const random: ISecureRandom;
  const parameters: IDHParameters);
begin
  Inherited Create(random, GetStrengthLocal(parameters));
  Fparameters := parameters;
end;

end.
