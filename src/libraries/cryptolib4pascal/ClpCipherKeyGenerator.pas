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

unit ClpCipherKeyGenerator;

{$I CryptoLib.inc}

interface

uses
  ClpSecureRandom,
  ClpISecureRandom,
  ClpICipherKeyGenerator,
  ClpKeyGenerationParameters,
  ClpIKeyGenerationParameters,
  ClpCryptoLibTypes;

resourcestring
  SInvalidStrengthValue =
    'Strength must be a Positive Value, "defaultStrength"';
  SParametersNil = 'Parameters Cannot be Nil';
  SGeneratorNotInitialized = 'Generator has not been Initialised';

type

  /// <summary>
  /// The base class for symmetric, or secret, cipher key generators.
  /// </summary>
  TCipherKeyGenerator = class(TInterfacedObject, ICipherKeyGenerator)

  strict private
  var
    Funinitialised: Boolean;
    FdefaultStrength: Int32;

    function GetdefaultStrength: Int32; inline;

  strict protected
  var
    Frandom: ISecureRandom;
    Fstrength: Int32;

    procedure EngineInit(const parameters: IKeyGenerationParameters); virtual;
    function EngineGenerateKey: TCryptoLibByteArray; virtual;

  public

    constructor Create(); overload;
    constructor Create(defaultStrength: Int32); overload;

    /// <summary>
    /// initialise the key generator.
    /// </summary>
    /// <param name="parameters">
    /// the parameters to be used for key generation
    /// </param>
    procedure Init(const parameters: IKeyGenerationParameters);

    /// <summary>
    /// Generate a secret key.
    /// </summary>
    /// <returns>
    /// a byte array containing the key value.
    /// </returns>
    function GenerateKey: TCryptoLibByteArray;

    property defaultStrength: Int32 read GetdefaultStrength;
  end;

implementation

{ TCipherKeyGenerator }

constructor TCipherKeyGenerator.Create;
begin
  Inherited Create();
  Funinitialised := true;
end;

constructor TCipherKeyGenerator.Create(defaultStrength: Int32);
begin
  Inherited Create();
  Funinitialised := true;
  if (defaultStrength < 1) then
  begin
    raise EArgumentCryptoLibException.CreateRes(@SInvalidStrengthValue);
  end;

  FdefaultStrength := defaultStrength;
end;

function TCipherKeyGenerator.EngineGenerateKey: TCryptoLibByteArray;
begin
  result := TSecureRandom.GetNextBytes(Frandom, Fstrength);
end;

procedure TCipherKeyGenerator.EngineInit(const parameters
  : IKeyGenerationParameters);
begin
  Frandom := parameters.Random;
  Fstrength := (parameters.Strength + 7) div 8;
end;

function TCipherKeyGenerator.GenerateKey: TCryptoLibByteArray;
begin
  if (Funinitialised) then
  begin
    if (FdefaultStrength < 1) then
    begin
      raise EInvalidOperationCryptoLibException.CreateRes
        (@SGeneratorNotInitialized);
    end;

    Funinitialised := false;

    EngineInit(TKeyGenerationParameters.Create(TSecureRandom.Create()
      as ISecureRandom, FdefaultStrength) as IKeyGenerationParameters);
  end;

  result := EngineGenerateKey();
end;

function TCipherKeyGenerator.GetdefaultStrength: Int32;
begin
  result := FdefaultStrength;
end;

procedure TCipherKeyGenerator.Init(const parameters: IKeyGenerationParameters);
begin
  if (parameters = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SParametersNil);
  end;

  Funinitialised := false;

  EngineInit(parameters);
end;

end.
