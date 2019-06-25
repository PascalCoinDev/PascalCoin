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

unit ClpDHBasicKeyPairGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpIDHParameters,
  ClpIDHBasicKeyPairGenerator,
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
  /// a basic Diffie-Hellman key pair generator.
  /// </para>
  /// <para>
  /// This generates keys consistent for use with the basic algorithm for
  /// Diffie-Hellman.
  /// </para>
  /// </summary>
  TDHBasicKeyPairGenerator = class sealed(TInterfacedObject,
    IAsymmetricCipherKeyPairGenerator, IDHBasicKeyPairGenerator)

  strict private

  var
    Fparam: IDHKeyGenerationParameters;

  public

    procedure Init(const parameters: IKeyGenerationParameters);

    function GenerateKeyPair(): IAsymmetricCipherKeyPair;

  end;

implementation

{ TDHBasicKeyPairGenerator }

function TDHBasicKeyPairGenerator.GenerateKeyPair: IAsymmetricCipherKeyPair;
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

procedure TDHBasicKeyPairGenerator.Init(const parameters
  : IKeyGenerationParameters);
begin
  if (parameters = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SParametersCannotBeNil);
  end;

  Fparam := parameters as IDHKeyGenerationParameters;
end;

end.
