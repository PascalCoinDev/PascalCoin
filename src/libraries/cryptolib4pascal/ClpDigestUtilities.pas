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

unit ClpDigestUtilities;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  TypInfo,
  Generics.Collections,
  HlpHashFactory,
  ClpIDigest,
  ClpDigest,
  ClpPkcsObjectIdentifiers,
  ClpRosstandartObjectIdentifiers,
  ClpOiwObjectIdentifiers,
  ClpNistObjectIdentifiers,
  ClpMiscObjectIdentifiers,
  ClpTeleTrusTObjectIdentifiers,
  ClpCryptoProObjectIdentifiers,
  ClpCryptoLibTypes,
  ClpIAsn1Objects;

resourcestring
  SMechanismNil = 'Mechanism Cannot be Nil';
  SUnRecognizedDigest = '"Digest " %s not recognised.';

type
  TDigestUtilities = class sealed(TObject)

  strict private

  class var
    Falgorithms: TDictionary<String, String>;
    Foids: TDictionary<String, IDerObjectIdentifier>;

  type
{$SCOPEDENUMS ON}
    TDigestAlgorithm = (BLAKE2B_160, BLAKE2B_256, BLAKE2B_384, BLAKE2B_512,
      BLAKE2S_128, BLAKE2S_160, BLAKE2S_224, BLAKE2S_256, GOST3411,
      GOST3411_2012_256, GOST3411_2012_512, MD2, MD4, MD5, NONE, RIPEMD128,
      RIPEMD160, RIPEMD256, RIPEMD320, SHA_1, SHA_224, SHA_256, SHA_384,
      SHA_512, SHA_512_224, SHA_512_256, SHA3_224, SHA3_256, SHA3_384, SHA3_512,
      TIGER, WHIRLPOOL);
{$SCOPEDENUMS OFF}
  class procedure Boot(); static;
  class constructor CreateDigestUtilities();
  class destructor DestroyDigestUtilities();

  public
    /// <summary>
    /// Returns a ObjectIdentifier for a given digest mechanism.
    /// </summary>
    /// <param name="mechanism">A string representation of the digest mechanism.</param>
    /// <returns>A DerObjectIdentifier, null if the Oid is not available.</returns>
    class function GetObjectIdentifier(mechanism: String)
      : IDerObjectIdentifier; static;
    class function GetDigest(const id: IDerObjectIdentifier): IDigest; overload;
      static; inline;
    class function GetDigest(const algorithm: String): IDigest;
      overload; static;

    class function GetAlgorithmName(const oid: IDerObjectIdentifier): String;
      static; inline;

    class function DoFinal(const digest: IDigest): TCryptoLibByteArray;
      overload; static; inline;
    class function DoFinal(const digest: IDigest;
      const input: TCryptoLibByteArray): TCryptoLibByteArray; overload;
      static; inline;

    class function CalculateDigest(const algorithm: String;
      const input: TCryptoLibByteArray): TCryptoLibByteArray; static; inline;

  end;

implementation

{ TDigestUtilities }

class function TDigestUtilities.GetDigest
  (const id: IDerObjectIdentifier): IDigest;
begin
  result := GetDigest(id.id);
end;

class function TDigestUtilities.GetDigest(const algorithm: String): IDigest;
var
  upper, mechanism, temp: String;
  digestAlgorithm: TDigestAlgorithm;
begin

  if (Falgorithms = Nil) or (Foids = Nil) then
  begin
    TDigestUtilities.Boot;
  end;

  upper := UpperCase(algorithm);
  Falgorithms.TryGetValue(upper, mechanism);

  if (mechanism = '') then
  begin
    mechanism := upper;
  end;

  temp := StringReplace(mechanism, '-', '_', [rfReplaceAll, rfIgnoreCase]);

  temp := StringReplace(temp, '/', '_', [rfReplaceAll, rfIgnoreCase]);

  digestAlgorithm := TDigestAlgorithm
    (GetEnumValue(TypeInfo(TDigestAlgorithm), temp));

  case digestAlgorithm of

    TDigestAlgorithm.BLAKE2B_160:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateBlake2B_160);

        Exit;
      end;

    TDigestAlgorithm.BLAKE2B_256:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateBlake2B_256);

        Exit;
      end;

    TDigestAlgorithm.BLAKE2B_384:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateBlake2B_384);

        Exit;
      end;

    TDigestAlgorithm.BLAKE2B_512:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateBlake2B_512);

        Exit;
      end;

    TDigestAlgorithm.BLAKE2S_128:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateBlake2S_128);

        Exit;
      end;

    TDigestAlgorithm.BLAKE2S_160:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateBlake2S_160);

        Exit;
      end;

    TDigestAlgorithm.BLAKE2S_224:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateBlake2S_224);

        Exit;
      end;

    TDigestAlgorithm.BLAKE2S_256:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateBlake2S_256);

        Exit;
      end;

    TDigestAlgorithm.GOST3411:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateGost());

        Exit;
      end;

    TDigestAlgorithm.GOST3411_2012_256:
      begin
        result := TDigest.Create
          (THashFactory.TCrypto.CreateGOST3411_2012_256());

        Exit;
      end;

    TDigestAlgorithm.GOST3411_2012_512:
      begin
        result := TDigest.Create
          (THashFactory.TCrypto.CreateGOST3411_2012_512());

        Exit;
      end;

    TDigestAlgorithm.MD2:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateMD2());

        Exit;
      end;

    TDigestAlgorithm.MD4:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateMD4());

        Exit;
      end;

    TDigestAlgorithm.MD5:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateMD5());

        Exit;
      end;

    TDigestAlgorithm.NONE:
      begin
        result := TDigest.Create
          (THashFactory.TNullDigestFactory.CreateNullDigest());

        Exit;
      end;

    TDigestAlgorithm.RIPEMD128:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateRIPEMD128());

        Exit;
      end;

    TDigestAlgorithm.RIPEMD160:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateRIPEMD160());

        Exit;
      end;

    TDigestAlgorithm.RIPEMD256:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateRIPEMD256());

        Exit;
      end;

    TDigestAlgorithm.RIPEMD320:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateRIPEMD320());

        Exit;
      end;

    TDigestAlgorithm.SHA_1:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA1());

        Exit;
      end;

    TDigestAlgorithm.SHA_224:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA2_224());

        Exit;
      end;

    TDigestAlgorithm.SHA_256:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA2_256());

        Exit;
      end;

    TDigestAlgorithm.SHA_384:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA2_384());

        Exit;
      end;

    TDigestAlgorithm.SHA_512:
      begin

        result := TDigest.Create(THashFactory.TCrypto.CreateSHA2_512());

        Exit;
      end;

    TDigestAlgorithm.SHA_512_224:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA2_512_224());

        Exit;
      end;

    TDigestAlgorithm.SHA_512_256:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA2_512_256());

        Exit;
      end;

    TDigestAlgorithm.SHA3_224:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA3_224());

        Exit;
      end;

    TDigestAlgorithm.SHA3_256:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA3_256());

        Exit;
      end;

    TDigestAlgorithm.SHA3_384:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA3_384());

        Exit;
      end;

    TDigestAlgorithm.SHA3_512:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateSHA3_512());

        Exit;
      end;

    TDigestAlgorithm.TIGER:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateTiger_3_192);

        Exit;
      end;

    TDigestAlgorithm.WHIRLPOOL:
      begin
        result := TDigest.Create(THashFactory.TCrypto.CreateWhirlPool);

        Exit;
      end
  else
    begin
      raise ESecurityUtilityCryptoLibException.CreateResFmt
        (@SUnRecognizedDigest, [mechanism]);
    end;

  end;

end;

class procedure TDigestUtilities.Boot;
begin
  if (Falgorithms = Nil) or (Foids = Nil) then
  begin
    Falgorithms := TDictionary<string, string>.Create();
    Foids := TDictionary<string, IDerObjectIdentifier>.Create();

    Falgorithms.Add('NONE', 'NONE'); // Null Digest

    TPkcsObjectIdentifiers.Boot;

    Falgorithms.Add(TPkcsObjectIdentifiers.MD2.id, 'MD2');
    Falgorithms.Add(TPkcsObjectIdentifiers.MD4.id, 'MD4');
    Falgorithms.Add(TPkcsObjectIdentifiers.MD5.id, 'MD5');

    TOiwObjectIdentifiers.Boot;

    Falgorithms.Add('SHA1', 'SHA-1');
    Falgorithms.Add(TOiwObjectIdentifiers.IdSha1.id, 'SHA-1');

    TNistObjectIdentifiers.Boot;

    Falgorithms.Add('SHA224', 'SHA-224');
    Falgorithms.Add(TNistObjectIdentifiers.IdSha224.id, 'SHA-224');
    Falgorithms.Add('SHA256', 'SHA-256');
    Falgorithms.Add(TNistObjectIdentifiers.IdSha256.id, 'SHA-256');
    Falgorithms.Add('SHA384', 'SHA-384');
    Falgorithms.Add(TNistObjectIdentifiers.IdSha384.id, 'SHA-384');
    Falgorithms.Add('SHA512', 'SHA-512');
    Falgorithms.Add(TNistObjectIdentifiers.IdSha512.id, 'SHA-512');
    Falgorithms.Add('SHA512/224', 'SHA-512/224');
    Falgorithms.Add(TNistObjectIdentifiers.IdSha512_224.id, 'SHA-512/224');
    Falgorithms.Add('SHA512/256', 'SHA-512/256');
    Falgorithms.Add(TNistObjectIdentifiers.IdSha512_256.id, 'SHA-512/256');

    TTeleTrusTObjectIdentifiers.Boot;

    Falgorithms.Add('RIPEMD-128', 'RIPEMD128');
    Falgorithms.Add(TTeleTrusTObjectIdentifiers.RIPEMD128.id, 'RIPEMD128');
    Falgorithms.Add('RIPEMD-160', 'RIPEMD160');
    Falgorithms.Add(TTeleTrusTObjectIdentifiers.RIPEMD160.id, 'RIPEMD160');
    Falgorithms.Add('RIPEMD-256', 'RIPEMD256');
    Falgorithms.Add(TTeleTrusTObjectIdentifiers.RIPEMD256.id, 'RIPEMD256');
    Falgorithms.Add('RIPEMD-320', 'RIPEMD320');
    // Falgorithms.Add(TTeleTrusTObjectIdentifiers.RipeMD320.Id,'RIPEMD320');

    TCryptoProObjectIdentifiers.Boot;

    Falgorithms.Add(TCryptoProObjectIdentifiers.GostR3411.id, 'GOST3411');

    Falgorithms.Add(TNistObjectIdentifiers.IdSha3_224.id, 'SHA3-224');
    Falgorithms.Add(TNistObjectIdentifiers.IdSha3_256.id, 'SHA3-256');
    Falgorithms.Add(TNistObjectIdentifiers.IdSha3_384.id, 'SHA3-384');
    Falgorithms.Add(TNistObjectIdentifiers.IdSha3_512.id, 'SHA3-512');

    TMiscObjectIdentifiers.Boot;

    Falgorithms.Add(TMiscObjectIdentifiers.id_blake2b160.id, 'BLAKE2B-160');
    Falgorithms.Add(TMiscObjectIdentifiers.id_blake2b256.id, 'BLAKE2B-256');
    Falgorithms.Add(TMiscObjectIdentifiers.id_blake2b384.id, 'BLAKE2B-384');
    Falgorithms.Add(TMiscObjectIdentifiers.id_blake2b512.id, 'BLAKE2B-512');
    Falgorithms.Add(TMiscObjectIdentifiers.id_blake2s128.id, 'BLAKE2S-128');
    Falgorithms.Add(TMiscObjectIdentifiers.id_blake2s160.id, 'BLAKE2S-160');
    Falgorithms.Add(TMiscObjectIdentifiers.id_blake2s224.id, 'BLAKE2S-224');
    Falgorithms.Add(TMiscObjectIdentifiers.id_blake2s256.id, 'BLAKE2S-256');

    TRosstandartObjectIdentifiers.Boot;

    Falgorithms.Add(TRosstandartObjectIdentifiers.id_tc26_hmac_gost_3411_12_256.
      id, 'HMAC-GOST3411-2012-256');
    Falgorithms.Add(TRosstandartObjectIdentifiers.id_tc26_hmac_gost_3411_12_512.
      id, 'HMAC-GOST3411-2012-512');

    Foids.Add('MD2', TPkcsObjectIdentifiers.MD2);
    Foids.Add('MD4', TPkcsObjectIdentifiers.MD4);
    Foids.Add('MD5', TPkcsObjectIdentifiers.MD5);
    Foids.Add('SHA-1', TOiwObjectIdentifiers.IdSha1);
    Foids.Add('SHA-224', TNistObjectIdentifiers.IdSha224);
    Foids.Add('SHA-256', TNistObjectIdentifiers.IdSha256);
    Foids.Add('SHA-384', TNistObjectIdentifiers.IdSha384);
    Foids.Add('SHA-512', TNistObjectIdentifiers.IdSha512);
    Foids.Add('SHA-512/224', TNistObjectIdentifiers.IdSha512_224);
    Foids.Add('SHA-512/256', TNistObjectIdentifiers.IdSha512_256);
    Foids.Add('SHA3-224', TNistObjectIdentifiers.IdSha3_224);
    Foids.Add('SHA3-256', TNistObjectIdentifiers.IdSha3_256);
    Foids.Add('SHA3-384', TNistObjectIdentifiers.IdSha3_384);
    Foids.Add('SHA3-512', TNistObjectIdentifiers.IdSha3_512);
    Foids.Add('RIPEMD128', TTeleTrusTObjectIdentifiers.RIPEMD128);
    Foids.Add('RIPEMD160', TTeleTrusTObjectIdentifiers.RIPEMD160);
    Foids.Add('RIPEMD256', TTeleTrusTObjectIdentifiers.RIPEMD256);
    Foids.Add('GOST3411', TCryptoProObjectIdentifiers.GostR3411);
    Foids.Add('BLAKE2B-160', TMiscObjectIdentifiers.id_blake2b160);
    Foids.Add('BLAKE2B-256', TMiscObjectIdentifiers.id_blake2b256);
    Foids.Add('BLAKE2B-384', TMiscObjectIdentifiers.id_blake2b384);
    Foids.Add('BLAKE2B-512', TMiscObjectIdentifiers.id_blake2b512);
    Foids.Add('BLAKE2S-128', TMiscObjectIdentifiers.id_blake2s128);
    Foids.Add('BLAKE2S-160', TMiscObjectIdentifiers.id_blake2s160);
    Foids.Add('BLAKE2S-224', TMiscObjectIdentifiers.id_blake2s224);
    Foids.Add('BLAKE2S-256', TMiscObjectIdentifiers.id_blake2s256);
    Foids.Add('GOST3411-2012-256',
      TRosstandartObjectIdentifiers.id_tc26_gost_3411_12_256);
    Foids.Add('GOST3411-2012-512',
      TRosstandartObjectIdentifiers.id_tc26_gost_3411_12_512);
  end;
end;

class function TDigestUtilities.DoFinal(const digest: IDigest)
  : TCryptoLibByteArray;
begin
  System.SetLength(result, digest.GetDigestSize());
  digest.DoFinal(result, 0);
end;

class function TDigestUtilities.DoFinal(const digest: IDigest;
  const input: TCryptoLibByteArray): TCryptoLibByteArray;
begin
  digest.BlockUpdate(input, 0, System.Length(input));
  result := DoFinal(digest);
end;

class function TDigestUtilities.CalculateDigest(const algorithm: String;
  const input: TCryptoLibByteArray): TCryptoLibByteArray;
var
  digest: IDigest;
begin
  digest := GetDigest(algorithm);
  digest.BlockUpdate(input, 0, System.Length(input));
  result := DoFinal(digest);
end;

class constructor TDigestUtilities.CreateDigestUtilities;
begin
  TDigestUtilities.Boot;
end;

class destructor TDigestUtilities.DestroyDigestUtilities;
begin
  Falgorithms.Free;
  Foids.Free;
end;

class function TDigestUtilities.GetAlgorithmName
  (const oid: IDerObjectIdentifier): String;
begin
  Falgorithms.TryGetValue(oid.id, result);
end;

class function TDigestUtilities.GetObjectIdentifier(mechanism: String)
  : IDerObjectIdentifier;
var
  aliased: String;
begin
  if (mechanism = '') then
    raise EArgumentNilCryptoLibException.CreateRes(@SMechanismNil);

  mechanism := UpperCase(mechanism);
  if Falgorithms.TryGetValue(mechanism, aliased) then
  begin
    mechanism := aliased;
  end;

  Foids.TryGetValue(mechanism, result);

end;

end.
