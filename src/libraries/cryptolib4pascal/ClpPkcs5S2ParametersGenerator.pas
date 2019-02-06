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

unit ClpPkcs5S2ParametersGenerator;

{$I ..\..\Include\CryptoLib.inc}

interface

uses

  HlpIHashInfo,
  HlpHashFactory,
  ClpIDigest,
  ClpICipherParameters,
  ClpIPkcs5S2ParametersGenerator,
  ClpKeyParameter,
  ClpIKeyParameter,
  ClpParametersWithIV,
  ClpParameterUtilities,
  ClpPbeParametersGenerator,
  ClpCryptoLibTypes;

type

  /// <summary>
  /// <see href="https://tools.ietf.org/html/rfc2898#section-5.2">
  /// Pkcs5S2 Specification</see>
  /// </summary>
  TPkcs5S2ParametersGenerator = class sealed(TPbeParametersGenerator,
    IPkcs5S2ParametersGenerator)

  strict private
  var
    Fdigest: IDigest;
    FPBKDF2_HMAC: HlpIHashInfo.IPBKDF2_HMAC;

    /// <returns>
    /// the underlying digest.
    /// </returns>
    function GetDigest: IDigest; inline;

    function GenerateDerivedKey(dkLen: Int32): TCryptoLibByteArray; inline;

  public

    /// <summary>
    /// construct a Pkcs5 Scheme 2 Parameters generator.
    /// </summary>
    /// <param name="digest">
    /// digest to use for constructing hmac
    /// </param>
    constructor Create(const digest: IDigest);

    procedure Init(const password, salt: TCryptoLibByteArray;
      iterationCount: Int32); override;

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
      : ICipherParameters; overload; override;

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
      keySize, ivSize: Int32): ICipherParameters; overload; override;

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
    function GenerateDerivedMacParameters(keySize: Int32): ICipherParameters;
      overload; override;

    /// <value>
    /// the underlying digest.
    /// </value>
    property digest: IDigest read GetDigest;
  end;

implementation

{ TPkcs5S2ParametersGenerator }

constructor TPkcs5S2ParametersGenerator.Create(const digest: IDigest);
begin
  Inherited Create();
  Fdigest := digest;
end;

function TPkcs5S2ParametersGenerator.GenerateDerivedKey(dkLen: Int32)
  : TCryptoLibByteArray;
begin
  result := FPBKDF2_HMAC.GetBytes(dkLen);
end;

function TPkcs5S2ParametersGenerator.GenerateDerivedMacParameters
  (keySize: Int32): ICipherParameters;
var
  dKey: TCryptoLibByteArray;
begin
  keySize := keySize div 8;

  dKey := GenerateDerivedKey(keySize);

  result := TKeyParameter.Create(dKey, 0, keySize);
end;

function TPkcs5S2ParametersGenerator.GenerateDerivedParameters(const algorithm
  : String; keySize: Int32): ICipherParameters;
var
  dKey: TCryptoLibByteArray;
begin
  keySize := keySize div 8;

  dKey := GenerateDerivedKey(keySize);

  result := TParameterUtilities.CreateKeyParameter(algorithm, dKey, 0, keySize);
end;

function TPkcs5S2ParametersGenerator.GenerateDerivedParameters(const algorithm
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

function TPkcs5S2ParametersGenerator.GetDigest: IDigest;
begin
  result := Fdigest;
end;

procedure TPkcs5S2ParametersGenerator.Init(const password,
  salt: TCryptoLibByteArray; iterationCount: Int32);
begin
  inherited Init(password, salt, iterationCount);
  FPBKDF2_HMAC := TKDF.TPBKDF2_HMAC.CreatePBKDF2_HMAC
    (Fdigest.GetUnderlyingIHash, password, salt, iterationCount);
end;

end.
