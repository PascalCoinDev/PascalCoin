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

unit ClpDHKeyPairGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpIDHParameters,
  ClpIDHKeyPairGenerator,
  ClpDHPublicKeyParameters,
  ClpIDHPublicKeyParameters,
  ClpDHPrivateKeyParameters,
  ClpIDHPrivateKeyParameters,
  ClpAsymmetricCipherKeyPair,
  ClpIAsymmetricCipherKeyPair,
  ClpIKeyGenerationParameters,
  ClpIDHKeyGenerationParameters,
  ClpDHKeyGeneratorHelper,
  ClpIDHKeyGeneratorHelper,
  ClpIAsymmetricCipherKeyPairGenerator,
  ClpBigInteger,
  ClpCryptoLibTypes;

resourcestring
  SParametersCannotBeNil = '"parameters" Cannot Be Nil';

type

  /// <summary>
  /// <para>
  /// a Diffie-Hellman key pair generator.
  /// </para>
  /// <para>
  /// This generates keys consistent for use in the MTI/A0 key agreement
  /// protocol as described in "Handbook of Applied Cryptography", Pages
  /// 516-519.
  /// </para>
  /// </summary>
  TDHKeyPairGenerator = class sealed(TInterfacedObject,
    IAsymmetricCipherKeyPairGenerator, IDHKeyPairGenerator)

  strict private

  var
    Fparam: IDHKeyGenerationParameters;

  public

    procedure Init(const parameters: IKeyGenerationParameters);

    function GenerateKeyPair(): IAsymmetricCipherKeyPair;

  end;

implementation

{ TDHKeyPairGenerator }

function TDHKeyPairGenerator.GenerateKeyPair: IAsymmetricCipherKeyPair;
var
  dhp: IDHParameters;
  helper: IDHKeyGeneratorHelper;
  x, y: TBigInteger;
begin

  helper := TDHKeyGeneratorHelper.Instance;
  dhp := Fparam.parameters;

  x := helper.CalculatePrivate(dhp, Fparam.Random);
  y := helper.CalculatePublic(dhp, x);

  result := TAsymmetricCipherKeyPair.Create(TDHPublicKeyParameters.Create(y,
    dhp) as IDHPublicKeyParameters, TDHPrivateKeyParameters.Create(x, dhp)
    as IDHPrivateKeyParameters);
end;

procedure TDHKeyPairGenerator.Init(const parameters: IKeyGenerationParameters);
begin
  if (parameters = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SParametersCannotBeNil);
  end;

  Fparam := parameters as IDHKeyGenerationParameters;
end;

end.
