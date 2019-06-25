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

unit ClpScryptParametersGenerator;

{$I CryptoLib.inc}

interface

uses

  HlpIHashInfo,
  HlpHashFactory,
  ClpICipherParameters,
  ClpPbeParametersGenerator,
  ClpIScryptParametersGenerator,
  ClpKeyParameter,
  ClpIKeyParameter,
  ClpParametersWithIV,
  ClpParameterUtilities,
  ClpArrayUtils,
  ClpCryptoLibTypes;

type

  /// <summary>
  /// <a href="http://tools.ietf.org/html/draft-josefsson-scrypt-kdf-01">draft-josefsson-scrypt-kd</a>
  /// Scrypt Specification</see>
  /// </summary>
  TScryptParametersGenerator = class sealed(TPbeParametersGenerator,
    IScryptParametersGenerator)

  strict private
  var
    FPassword, FSalt: TCryptoLibByteArray;
    FPBKDF_Scrypt: HlpIHashInfo.IPBKDF_Scrypt;

    function GenerateDerivedKey(dkLen: Int32): TCryptoLibByteArray; inline;

  public

    procedure Clear(); override;
    /// <summary>
    /// construct an Scrypt Parameters generator.
    /// </summary>
    constructor Create();

    destructor Destroy; override;

    procedure Init(const password, salt: TCryptoLibByteArray;
      cost, blockSize, parallelism: Int32);

    /// <summary>
    /// Generate a key parameter derived from the password, salt,
    /// cost, blockSize, parallelism we are currently initialised with.
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
      : ICipherParameters; overload; override;

    /// <summary>
    /// Generate a key with initialisation vector parameter derived from <br />
    /// the password, salt, cost, blockSize, parallelism we are currently initialised with.
    /// </summary>
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
      keySize, ivSize: Int32): ICipherParameters; overload; override;

    /// <summary>
    /// Generate a key parameter for use with a MAC derived from the
    /// the password, salt, cost, blockSize, parallelism we are currently initialised with.
    /// with.
    /// </summary>
    /// <param name="keySize">
    /// the length, in bits, of the key required.
    /// </param>
    /// <returns>
    /// a parameters object representing a key.
    /// </returns>
    function GenerateDerivedMacParameters(keySize: Int32)
      : ICipherParameters; override;

  end;

implementation

{ TPkcs5S2ParametersGenerator }

procedure TScryptParametersGenerator.Clear();
begin
  TArrayUtils.ZeroFill(FPassword);
  TArrayUtils.ZeroFill(FSalt);

  if FPBKDF_Scrypt <> Nil then
  begin
    FPBKDF_Scrypt.Clear();
  end;
end;

constructor TScryptParametersGenerator.Create();
begin
  Inherited Create();
end;

destructor TScryptParametersGenerator.Destroy;
begin
  Clear();
  inherited Destroy;
end;

function TScryptParametersGenerator.GenerateDerivedKey(dkLen: Int32)
  : TCryptoLibByteArray;
begin
  result := FPBKDF_Scrypt.GetBytes(dkLen);
end;

function TScryptParametersGenerator.GenerateDerivedMacParameters(keySize: Int32)
  : ICipherParameters;
var
  dKey: TCryptoLibByteArray;
begin
  keySize := keySize div 8;

  dKey := GenerateDerivedKey(keySize);

  result := TKeyParameter.Create(dKey, 0, keySize);
end;

function TScryptParametersGenerator.GenerateDerivedParameters(const algorithm
  : String; keySize: Int32): ICipherParameters;
var
  dKey: TCryptoLibByteArray;
begin
  keySize := keySize div 8;

  dKey := GenerateDerivedKey(keySize);

  result := TParameterUtilities.CreateKeyParameter(algorithm, dKey, 0, keySize);
end;

function TScryptParametersGenerator.GenerateDerivedParameters(const algorithm
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

procedure TScryptParametersGenerator.Init(const password,
  salt: TCryptoLibByteArray; cost, blockSize, parallelism: Int32);
begin
  FPassword := System.Copy(password);
  FSalt := System.Copy(salt);
  FPBKDF_Scrypt := TKDF.TPBKDF_Scrypt.CreatePBKDF_Scrypt(FPassword, FSalt, cost,
    blockSize, parallelism);
end;

end.
