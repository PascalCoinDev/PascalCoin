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

unit ClpParametersWithRandom;

{$I CryptoLib.inc}

interface

uses
  ClpCryptoLibTypes,
  ClpIParametersWithRandom,
  ClpISecureRandom,
  ClpSecureRandom,
  ClpICipherParameters;

resourcestring
  SParameters = 'Parameters';
  SRandom = 'Random';

type
  TParametersWithRandom = class(TInterfacedObject, ICipherParameters,
    IParametersWithRandom)

  strict private
  var
    Fparameters: ICipherParameters;
    Frandom: ISecureRandom;
    function GetRandom: ISecureRandom; inline;
    function GetParameters: ICipherParameters; inline;

  public

    constructor Create(const parameters: ICipherParameters); overload;

    constructor Create(const parameters: ICipherParameters;
      const random: ISecureRandom); overload;

    property random: ISecureRandom read GetRandom;

    property parameters: ICipherParameters read GetParameters;

  end;

implementation

{ TParametersWithRandom }

constructor TParametersWithRandom.Create(const parameters: ICipherParameters);
begin
  Create(parameters, TSecureRandom.Create() as ISecureRandom);
end;

constructor TParametersWithRandom.Create(const parameters: ICipherParameters;
  const random: ISecureRandom);
begin
  inherited Create();
  if (parameters = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SParameters);
  end;

  if (random = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SRandom);
  end;

  Fparameters := parameters;
  Frandom := random;
end;

function TParametersWithRandom.GetParameters: ICipherParameters;
begin
  Result := Fparameters;
end;

function TParametersWithRandom.GetRandom: ISecureRandom;
begin
  Result := Frandom;
end;

end.
