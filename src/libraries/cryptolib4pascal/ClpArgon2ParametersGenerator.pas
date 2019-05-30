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

unit ClpArgon2ParametersGenerator;

{$I CryptoLib.inc}

interface

uses

  HlpIHashInfo,
  HlpHashFactory,
  HlpPBKDF_Argon2NotBuildInAdapter,
  ClpICipherParameters,
  ClpIArgon2ParametersGenerator,
  ClpKeyParameter,
  ClpIKeyParameter,
  ClpParametersWithIV,
  ClpParameterUtilities,
  ClpCryptoLibTypes;

resourcestring
  SArgon2TypeInvalid = 'Selected Argon2Type is Invalid';
  SArgon2MemoryCostTypeInvalid = 'Selected Argon2MemoryCostType is Invalid';

type

  /// <summary>
  /// <see href="https://github.com/P-H-C/phc-winner-argon2/blob/master/argon2-specs.pdf">
  /// Argon2 Specification</see>, <see href="https://tools.ietf.org/html/draft-irtf-cfrg-argon2-04">
  /// ietf specs</see>
  /// </summary>
  TArgon2ParametersGenerator = class sealed(TInterfacedObject,
    IArgon2ParametersGenerator)

  strict private
  var
    FPassword: TCryptoLibByteArray;
    FPBKDF_Argon2: IPBKDF_Argon2;
    FArgon2ParametersBuilder: HlpIHashInfo.IArgon2ParametersBuilder;

    /// <returns>
    /// the password byte array.
    /// </returns>
    function GetPassword: TCryptoLibByteArray; inline;

    /// <returns>
    /// the Argon2 Parameter Builder Instance
    /// </returns>
    function GetArgon2ParametersBuilder
      : HlpIHashInfo.IArgon2ParametersBuilder; inline;

    function GenerateDerivedKey(dkLen: Int32): TCryptoLibByteArray; inline;

  public

    /// <summary>
    /// construct an Argon2 Parameters generator.
    /// </summary>
    /// <param name="digest">
    /// digest to use for constructing hmac
    /// </param>
    constructor Create();

    procedure Init(argon2Type: TArgon2Type; argon2Version: TArgon2Version;
      const password, salt, secret, additional: TCryptoLibByteArray;
      iterations, memory, parallelism: Int32;
      memoryCostType: TArgon2MemoryCostType);

    /// <summary>
    /// Generate a key parameter derived from the password, salt, and
    /// iteration count we are currently initialised with.
    /// </summary>
    /// <param name="algorithm">
    /// a parameters object representing a key.
    /// </param>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key.
    /// </returns>
    function GenerateDerivedParameters(const algorithm: String; keySize: Int32)
      : ICipherParameters; overload;

    /// <summary>
    /// Generate a key with initialisation vector parameter derived from <br />
    /// the password, salt, and iteration count we are currently initialised
    /// with.
    /// </summary>
    /// <param name="algorithm">
    /// a parameters object representing a key.
    /// </param>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <param name="ivSize">
    /// the length, in bits, of the iv required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key and an iv.
    /// </returns>
    function GenerateDerivedParameters(const algorithm: String;
      keySize, ivSize: Int32): ICipherParameters; overload;

    /// <summary>
    /// Generate a key parameter for use with a MAC derived from the
    /// password, salt, and iteration count we are currently initialised
    /// with.
    /// </summary>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key.
    /// </returns>
    function GenerateDerivedMacParameters(keySize: Int32)
      : ICipherParameters; overload;

    /// <value>
    /// the password byte array.
    /// </value>
    property password: TCryptoLibByteArray read GetPassword;

    /// <returns>
    /// the Argon2 Parameter Builder Instance
    /// </returns>
    property Argon2ParametersBuilder: HlpIHashInfo.IArgon2ParametersBuilder
      read GetArgon2ParametersBuilder;

  end;

implementation

{ TArgon2ParametersGenerator }

function TArgon2ParametersGenerator.GetPassword: TCryptoLibByteArray;
begin
  result := System.Copy(FPassword);
end;

function TArgon2ParametersGenerator.GetArgon2ParametersBuilder
  : HlpIHashInfo.IArgon2ParametersBuilder;
begin
  result := FArgon2ParametersBuilder;
end;

constructor TArgon2ParametersGenerator.Create();
begin
  Inherited Create();
end;

function TArgon2ParametersGenerator.GenerateDerivedKey(dkLen: Int32)
  : TCryptoLibByteArray;
begin
  result := FPBKDF_Argon2.GetBytes(dkLen);
end;

function TArgon2ParametersGenerator.GenerateDerivedMacParameters(keySize: Int32)
  : ICipherParameters;
var
  dKey: TCryptoLibByteArray;
begin
  keySize := keySize div 8;

  dKey := GenerateDerivedKey(keySize);

  result := TKeyParameter.Create(dKey, 0, keySize);
end;

function TArgon2ParametersGenerator.GenerateDerivedParameters(const algorithm
  : String; keySize: Int32): ICipherParameters;
var
  dKey: TCryptoLibByteArray;
begin
  keySize := keySize div 8;

  dKey := GenerateDerivedKey(keySize);

  result := TParameterUtilities.CreateKeyParameter(algorithm, dKey, 0, keySize);
end;

function TArgon2ParametersGenerator.GenerateDerivedParameters(const algorithm
  : String; keySize, ivSize: Int32): ICipherParameters;
var
  dKey: TCryptoLibByteArray;
  key: IKeyParameter;
begin
  keySize := keySize div 8;
  ivSize := ivSize div 8;

  dKey := GenerateDerivedKey(keySize + ivSize);
  key := TParameterUtilities.CreateKeyParameter(algorithm, dKey, 0, keySize);

  result := TParametersWithIV.Create(key, dKey, keySize, ivSize);
end;

procedure TArgon2ParametersGenerator.Init(argon2Type: TArgon2Type;
  argon2Version: TArgon2Version; const password, salt, secret,
  additional: TCryptoLibByteArray; iterations, memory, parallelism: Int32;
  memoryCostType: TArgon2MemoryCostType);
var
  LArgon2ParametersBuilder: IArgon2ParametersBuilder;
begin
  FPassword := password;

  case argon2Type of
    TArgon2Type.a2tARGON2_d:
      begin
        LArgon2ParametersBuilder := TArgon2dParametersBuilder.Builder();
      end;

    TArgon2Type.a2tARGON2_i:
      begin
        LArgon2ParametersBuilder := TArgon2iParametersBuilder.Builder();
      end;
    TArgon2Type.a2tARGON2_id:
      begin
        LArgon2ParametersBuilder := TArgon2idParametersBuilder.Builder();
      end
  else
    begin
      raise EArgumentCryptoLibException.CreateRes(@SArgon2TypeInvalid);
    end;
  end;

  case memoryCostType of
    TArgon2MemoryCostType.a2mctMemoryAsKB:
      begin
        LArgon2ParametersBuilder.WithVersion(argon2Version).WithSalt(salt)
          .WithSecret(secret).WithAdditional(additional)
          .WithIterations(iterations).WithMemoryAsKB(memory)
          .WithParallelism(parallelism);
      end;

    TArgon2MemoryCostType.a2mctMemoryPowOfTwo:
      begin
        LArgon2ParametersBuilder.WithVersion(argon2Version).WithSalt(salt)
          .WithSecret(secret).WithAdditional(additional)
          .WithIterations(iterations).WithMemoryPowOfTwo(memory)
          .WithParallelism(parallelism);
      end
  else
    begin
      raise EArgumentCryptoLibException.CreateRes
        (@SArgon2MemoryCostTypeInvalid);
    end;
  end;

  FPBKDF_Argon2 := TKDF.TPBKDF_Argon2.CreatePBKDF_Argon2(FPassword,
    LArgon2ParametersBuilder.Build());
end;

end.
