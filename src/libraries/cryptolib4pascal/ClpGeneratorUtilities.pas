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

unit ClpGeneratorUtilities;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  Generics.Collections,
  ClpECKeyPairGenerator,
  ClpIECKeyPairGenerator,
  ClpCipherKeyGenerator,
  ClpICipherKeyGenerator,
  ClpIAsn1Objects,
  ClpDsaKeyPairGenerator,
  ClpIDsaKeyPairGenerator,
  ClpIAsymmetricCipherKeyPairGenerator,
  ClpNistObjectIdentifiers,
  ClpIanaObjectIdentifiers,
  ClpPkcsObjectIdentifiers,
  ClpRosstandartObjectIdentifiers,
  ClpStringUtils,
  ClpCryptoLibTypes;

resourcestring
  SKeyGeneratorAlgorithmNotRecognised = 'KeyGenerator "%s" not Recognised.';
  SKeyGeneratorAlgorithmNotSupported =
    'KeyGenerator "%s" ( "%s" ) not Supported.';
  SKeyPairGeneratorAlgorithmNotRecognised =
    'KeyPairGenerator "%s" not Recognised.';
  SKeyPairGeneratorAlgorithmNotSupported =
    'KeyPairGenerator "%s" ( "%s" ) not Supported.';

type

  TGeneratorUtilities = class sealed(TObject)

  strict private
  class var

    FkgAlgorithms: TDictionary<String, String>;
    FkpgAlgorithms: TDictionary<String, String>;
    FdefaultKeySizes: TDictionary<String, Int32>;

    class function FindDefaultKeySize(const canonicalName: String): Int32;
      static; inline;

    class procedure AddDefaultKeySizeEntries(size: Int32;
      const algorithms: array of String); static;

    class procedure AddKgAlgorithm(const canonicalName: String;
      const aliases: array of String); static;

    class procedure AddKpgAlgorithm(const canonicalName: String;
      const aliases: array of String); static;

    class procedure AddHMacKeyGenerator(const algorithm: String;
      const aliases: array of String); static;

    class procedure Boot(); static;

    class constructor CreateGeneratorUtilities();
    class destructor DestroyGeneratorUtilities();

  public

    class function GetCanonicalKeyGeneratorAlgorithm(const algorithm: String)
      : String; static; inline;

    class function GetCanonicalKeyPairGeneratorAlgorithm(const algorithm
      : String): String; static; inline;

    class function GetKeyPairGenerator(const oid: IDerObjectIdentifier)
      : IAsymmetricCipherKeyPairGenerator; overload; static; inline;

    class function GetKeyPairGenerator(const algorithm: String)
      : IAsymmetricCipherKeyPairGenerator; overload; static;

    class function GetKeyGenerator(const algorithm: String)
      : ICipherKeyGenerator; static;

    class function GetDefaultKeySize(const oid: IDerObjectIdentifier): Int32;
      overload; static; inline;

    class function GetDefaultKeySize(const algorithm: String): Int32;
      overload; static;

  end;

implementation

{ TGeneratorUtilities }

class procedure TGeneratorUtilities.AddDefaultKeySizeEntries(size: Int32;
  const algorithms: array of String);
var
  algorithm: string;
begin
  for algorithm in algorithms do
  begin
    FdefaultKeySizes.Add(algorithm, size);
  end;

end;

class procedure TGeneratorUtilities.AddKgAlgorithm(const canonicalName: String;
  const aliases: array of String);
var
  alias: string;
begin
  FkgAlgorithms.Add(canonicalName, canonicalName);
  for alias in aliases do
  begin
    FkgAlgorithms.Add(alias, canonicalName);
  end;

end;

class procedure TGeneratorUtilities.AddKpgAlgorithm(const canonicalName: String;
  const aliases: array of String);
var
  alias: string;
begin
  FkpgAlgorithms.Add(canonicalName, canonicalName);
  for alias in aliases do
  begin
    FkpgAlgorithms.Add(alias, canonicalName);
  end;

end;

class procedure TGeneratorUtilities.AddHMacKeyGenerator(const algorithm: String;
  const aliases: array of String);
var
  alias, mainName: string;
begin
  mainName := 'HMAC' + algorithm;

  FkgAlgorithms.Add(mainName, mainName);
  FkgAlgorithms.Add('HMAC-' + algorithm, mainName);
  FkgAlgorithms.Add('HMAC/' + algorithm, mainName);

  for alias in aliases do
  begin
    FkgAlgorithms.Add(alias, mainName);
  end;

end;

class procedure TGeneratorUtilities.Boot;
begin
  FkgAlgorithms := TDictionary<String, String>.Create();
  FkpgAlgorithms := TDictionary<String, String>.Create();
  FdefaultKeySizes := TDictionary<String, Int32>.Create();

  TNistObjectIdentifiers.Boot;

  //
  // key generators.
  //

  AddKgAlgorithm('AES128', ['2.16.840.1.101.3.4.2',
    TNistObjectIdentifiers.IdAes128Cbc.ID,
    TNistObjectIdentifiers.IdAes128Cfb.ID,
    TNistObjectIdentifiers.IdAes128Ecb.ID,
    TNistObjectIdentifiers.IdAes128Ofb.ID]);

  AddKgAlgorithm('AES192', ['2.16.840.1.101.3.4.22',
    TNistObjectIdentifiers.IdAes192Cbc.ID,
    TNistObjectIdentifiers.IdAes192Cfb.ID,
    TNistObjectIdentifiers.IdAes192Ecb.ID,
    TNistObjectIdentifiers.IdAes192Ofb.ID]);

  AddKgAlgorithm('AES256', ['2.16.840.1.101.3.4.42',
    TNistObjectIdentifiers.IdAes256Cbc.ID,
    TNistObjectIdentifiers.IdAes256Cfb.ID,
    TNistObjectIdentifiers.IdAes256Ecb.ID,
    TNistObjectIdentifiers.IdAes256Ofb.ID]);

  AddKgAlgorithm('BLOWFISH', ['1.3.6.1.4.1.3029.1.2']);

  //
  // HMac key generators
  //
  TIanaObjectIdentifiers.Boot;

  AddHMacKeyGenerator('MD2', []);
  AddHMacKeyGenerator('MD4', []);
  AddHMacKeyGenerator('MD5', [TIanaObjectIdentifiers.HmacMD5.ID]);

  TPkcsObjectIdentifiers.Boot;

  AddHMacKeyGenerator('SHA1', [TPkcsObjectIdentifiers.IdHmacWithSha1.ID,
    TIanaObjectIdentifiers.HmacSha1.ID]);
  AddHMacKeyGenerator('SHA224', [TPkcsObjectIdentifiers.IdHmacWithSha224.ID]);
  AddHMacKeyGenerator('SHA256', [TPkcsObjectIdentifiers.IdHmacWithSha256.ID]);
  AddHMacKeyGenerator('SHA384', [TPkcsObjectIdentifiers.IdHmacWithSha384.ID]);
  AddHMacKeyGenerator('SHA512', [TPkcsObjectIdentifiers.IdHmacWithSha512.ID]);
  AddHMacKeyGenerator('SHA512/224', []);
  AddHMacKeyGenerator('SHA512/256', []);

  AddHMacKeyGenerator('SHA3-224',
    [TNistObjectIdentifiers.IdHMacWithSha3_224.ID]);
  AddHMacKeyGenerator('SHA3-256',
    [TNistObjectIdentifiers.IdHMacWithSha3_256.ID]);
  AddHMacKeyGenerator('SHA3-384',
    [TNistObjectIdentifiers.IdHMacWithSha3_384.ID]);
  AddHMacKeyGenerator('SHA3-512',
    [TNistObjectIdentifiers.IdHMacWithSha3_512.ID]);
  AddHMacKeyGenerator('RIPEMD128', []);
  AddHMacKeyGenerator('RIPEMD160', [TIanaObjectIdentifiers.HmacRipeMD160.ID]);
  AddHMacKeyGenerator('TIGER', [TIanaObjectIdentifiers.HmacTiger.ID]);

  TRosstandartObjectIdentifiers.Boot;

  AddHMacKeyGenerator('GOST3411-2012-256',
    [TRosstandartObjectIdentifiers.id_tc26_hmac_gost_3411_12_256.ID]);
  AddHMacKeyGenerator('GOST3411-2012-512',
    [TRosstandartObjectIdentifiers.id_tc26_hmac_gost_3411_12_512.ID]);

  //
  // key pair generators.
  //

  AddKpgAlgorithm('DSA', []);
  AddKpgAlgorithm('ECDH', ['ECIES']);
  AddKpgAlgorithm('ECDSA', []);

  AddDefaultKeySizeEntries(128, ['AES128', 'BLOWFISH', 'HMACMD2', 'HMACMD4',
    'HMACMD5', 'HMACRIPEMD128']);
  AddDefaultKeySizeEntries(160, ['HMACRIPEMD160', 'HMACSHA1']);
  AddDefaultKeySizeEntries(192, ['AES', 'AES192', 'HMACTIGER']);
  AddDefaultKeySizeEntries(224, ['HMACSHA3-224', 'HMACSHA224',
    'HMACSHA512/224']);
  AddDefaultKeySizeEntries(256, ['AES256', 'HMACGOST3411-2012-256',
    'HMACSHA3-256', 'HMACSHA256', 'HMACSHA512/256']);
  AddDefaultKeySizeEntries(384, ['HMACSHA3-384', 'HMACSHA384']);
  AddDefaultKeySizeEntries(512, ['HMACGOST3411-2012-512', 'HMACSHA3-512',
    'HMACSHA512']);
end;

class constructor TGeneratorUtilities.CreateGeneratorUtilities;
begin
  TGeneratorUtilities.Boot;
end;

class destructor TGeneratorUtilities.DestroyGeneratorUtilities;
begin
  FkgAlgorithms.Free;
  FkpgAlgorithms.Free;
  FdefaultKeySizes.Free;
end;

class function TGeneratorUtilities.FindDefaultKeySize(const canonicalName
  : String): Int32;
begin
  if (not FdefaultKeySizes.ContainsKey(canonicalName)) then
  begin
    result := -1;
    Exit;
  end;

  FdefaultKeySizes.TryGetValue(canonicalName, result);
end;

class function TGeneratorUtilities.GetCanonicalKeyGeneratorAlgorithm
  (const algorithm: String): String;
begin
  FkgAlgorithms.TryGetValue(UpperCase(algorithm), result);
end;

class function TGeneratorUtilities.GetCanonicalKeyPairGeneratorAlgorithm
  (const algorithm: String): String;
begin
  FkpgAlgorithms.TryGetValue(UpperCase(algorithm), result);
end;

class function TGeneratorUtilities.GetDefaultKeySize(const algorithm
  : String): Int32;
var
  canonicalName: string;
  defaultKeySize: Int32;
begin
  canonicalName := GetCanonicalKeyGeneratorAlgorithm(algorithm);

  if (canonicalName = '') then
  begin
    raise ESecurityUtilityCryptoLibException.CreateResFmt
      (@SKeyGeneratorAlgorithmNotRecognised, [algorithm]);
  end;

  defaultKeySize := FindDefaultKeySize(canonicalName);
  if (defaultKeySize = -1) then
  begin

    raise ESecurityUtilityCryptoLibException.CreateResFmt
      (@SKeyGeneratorAlgorithmNotSupported, [algorithm, canonicalName]);
  end;

  result := defaultKeySize;
end;

class function TGeneratorUtilities.GetDefaultKeySize
  (const oid: IDerObjectIdentifier): Int32;
begin
  result := GetDefaultKeySize(oid.ID);
end;

class function TGeneratorUtilities.GetKeyGenerator(const algorithm: String)
  : ICipherKeyGenerator;
var
  canonicalName: string;
  defaultKeySize: Int32;
begin

  canonicalName := GetCanonicalKeyGeneratorAlgorithm(algorithm);
  if (canonicalName = '') then
  begin
    raise ESecurityUtilityCryptoLibException.CreateResFmt
      (@SKeyGeneratorAlgorithmNotRecognised, [algorithm]);
  end;

  defaultKeySize := FindDefaultKeySize(canonicalName);
  if (defaultKeySize = -1) then
  begin
    raise ESecurityUtilityCryptoLibException.CreateResFmt
      (@SKeyGeneratorAlgorithmNotSupported, [algorithm, canonicalName]);
  end;

  result := TCipherKeyGenerator.Create(defaultKeySize);
end;

class function TGeneratorUtilities.GetKeyPairGenerator(const algorithm: String)
  : IAsymmetricCipherKeyPairGenerator;
var
  canonicalName: string;
begin
  canonicalName := GetCanonicalKeyPairGeneratorAlgorithm(algorithm);

  if (canonicalName = '') then
  begin
    raise ESecurityUtilityCryptoLibException.CreateResFmt
      (@SKeyPairGeneratorAlgorithmNotRecognised, [algorithm]);
  end;

  if (canonicalName = 'DSA') then
  begin
    result := TDsaKeyPairGenerator.Create() as IDsaKeyPairGenerator;
    Exit;
  end;

  // "EC", "ECDH", "ECDHC", "ECDSA", "ECGOST3410", "ECMQV"
  if TStringUtils.BeginsWith(canonicalName, 'EC', True) then
  begin
    result := TECKeyPairGenerator.Create(canonicalName) as IECKeyPairGenerator;
    Exit;
  end;

  raise ESecurityUtilityCryptoLibException.CreateResFmt
    (@SKeyPairGeneratorAlgorithmNotSupported, [algorithm, canonicalName]);

end;

class function TGeneratorUtilities.GetKeyPairGenerator
  (const oid: IDerObjectIdentifier): IAsymmetricCipherKeyPairGenerator;
begin
  result := GetKeyPairGenerator(oid.ID);
end;

end.
