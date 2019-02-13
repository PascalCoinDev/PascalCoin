unit UPCCryptoLib4Pascal;

{ Copyright (c) 2019 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  This unit contains code made by Ugochukwu Mmaduekwe (aka Xor-el at GitHub)
  https://github.com/PascalCoin/PascalCoinTools/tree/master/Tools/PascalCoinKeyTool
  Available under MIT License

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{
  This unit is a bridge between PascalCoin and CryptoLib4Pascal in order to
  do not use OpenSSL library to do some cryptographic functions.

  Specially will define:
  - Use of BigIntegers
  - ECDSA keys generation
  - ECDSA Sign
  - ECDSA Verify
  - PascalCoin ECDSA encryption
  - PascalCoin AES encryption
}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


Uses SysUtils, UBaseTypes, UPCDataTypes,
  ClpBigInteger,
  ClpSecureRandom,
  ClpISecureRandom,
  ClpIECDomainParameters,
  ClpECDomainParameters,
  ClpIX9ECParameters,
  ClpIIESEngine,
  ClpIBaseKdfBytesGenerator,
  ClpIIESWithCipherParameters,
  ClpIPascalCoinECIESKdfBytesGenerator,
  ClpIPascalCoinIESEngine
  ;

Type
  TPCCryptoLib4Pascal = Class
  strict private
  const
    PKCS5_SALT_LEN = Int32(8);
    SALT_MAGIC_LEN = Int32(8);
    SALT_SIZE = Int32(8);
    SALT_MAGIC: string = 'Salted__';
  strict private
    class var FRandom: ISecureRandom;
    class var FDomain_SECP256K1 : IECDomainParameters;
    class var FDomain_SECP384R1 : IECDomainParameters;
    class var FDomain_SECT283K1 : IECDomainParameters;
    class var FDomain_SECP521R1 : IECDomainParameters;
    class var FCurve_SECP256K1 : IX9ECParameters;
    class var FCurve_SECP384R1 : IX9ECParameters;
    class var FCurve_SECT283K1 : IX9ECParameters;
    class var FCurve_SECP521R1 : IX9ECParameters;
    class var FPascalCoinIESEngine : IPascalCoinIESEngine;
    class var FPascalCoinIESWithCipherParameters : IIESWithCipherParameters;
    class constructor TPCCryptoLib4Pascal();
    class function GetCurveAndDomainParameters(const AEC_OpenSSL_NID : Word; var OCurve : IX9ECParameters; var ODomain : IECDomainParameters; ARaiseIfNotForPascal : Boolean = True ) : Boolean;
    class function GetDomainParameters(const AEC_OpenSSL_NID : Word) : IECDomainParameters;
    class function EVP_GetSalt(): TBytes; static; inline;
    class function EVP_GetKeyIV(const APasswordBytes, ASaltBytes: TBytes; out AKeyBytes, AIVBytes: TBytes): boolean; static;
  protected
  public
    class function DoECDSASign(const AEC_OpenSSL_NID : Word; const APrivateKey, AMessage: TBytes; var ASignature : TECDSA_SIG) : Boolean;
    class function DoECDSAVerify(const APublicKey : TECDSA_Public; const AMessage: TBytes; const ASignature : TECDSA_SIG) : Boolean;
    class function DoPascalCoinECIESEncrypt(const APublicKey : TECDSA_Public; const AMessage : TBytes; var AEncryptedMessage : TBytes) : Boolean;
    class function DoPascalCoinECIESDecrypt(const AEC_OpenSSL_NID : Word; const APrivateKey, AEncryptedMessage : TBytes; var ADecryptedMessage : TBytes) : Boolean;
    class function DoPascalCoinAESEncrypt(const AMessage, APassword: TBytes): TBytes; static;
    class function DoPascalCoinAESDecrypt(const AEncryptedMessage, APassword: TBytes; out ADecryptedMessage: TBytes): boolean; static;
    class function DoGetRandomPrivateKey(const AEC_OpenSSL_NID : Word) : TBytes;
    class function DoGetPublicKey(const AEC_OpenSSL_NID : Word; const APrivateKey: TBytes) : TECDSA_Public;
    class procedure DoSHA256(const AInput : TBytes; var AOutput : TBytes);
    class procedure DoRIPEMD160(const AInput : TBytes; var AOutput : TBytes);
  End;

implementation

Uses
  ClpCustomNamedCurves,
  ClpIECPrivateKeyParameters,
  ClpECPrivateKeyParameters,
  ClpIParametersWithRandom,
  ClpParametersWithRandom,
  ClpIECDsaSigner,
  ClpECDsaSigner,
  ClpCryptoLibTypes,
  //
  ClpIECPublicKeyParameters,
  ClpECPublicKeyParameters,
  ClpIECC,
  //
  ClpIIESCipher,
  ClpIESCipher,
  ClpIBufferedBlockCipher,
  ClpIAesEngine,
  ClpIBlockCipherModes,
  ClpIBasicAgreement,
  ClpIESWithCipherParameters,
  ClpIECDHBasicAgreement,
  ClpIMac,
  ClpECDHBasicAgreement,
  ClpPascalCoinECIESKdfBytesGenerator,
  ClpDigestUtilities,
  ClpMacUtilities,
  ClpAesEngine,
  ClpBlockCipherModes,
  ClpPaddedBufferedBlockCipher,
  ClpPaddingModes,
  ClpIPaddingModes,
  ClpPascalCoinIESEngine,
  //
  ClpIParametersWithIV,
  ClpIBufferedCipher,
  ClpArrayUtils,
  ClpCipherUtilities,
  ClpParametersWithIV,
  ClpParameterUtilities,
  ClpIDigest,
  //
  ClpIAsymmetricCipherKeyPairGenerator,
  ClpGeneratorUtilities,
  ClpECKeyGenerationParameters,
  ClpIECKeyGenerationParameters,
  ClpIAsymmetricCipherKeyPair,
  ClpECKeyPairGenerator,
  //
  HlpSHA2_256,
  HlpRIPEMD160,
  //
  UAccounts,
  UConst,
  ULog;

{ TPCCryptoLib4Pascal }

class function TPCCryptoLib4Pascal.DoECDSASign(const AEC_OpenSSL_NID : Word;
  Const APrivateKey, AMessage: TBytes; var ASignature: TECDSA_SIG): Boolean;
var
  LDomain: IECDomainParameters;
  LPrivD: TBigInteger;
  LPrivKeyParams : IECPrivateKeyParameters;
  LParam: IParametersWithRandom;
  LSigner : IECDsaSigner;
  LSignerResult : TCryptoLibGenericArray<TBigInteger>;
begin
  LDomain := GetDomainParameters(AEC_OpenSSL_NID);
  LPrivD := TBigInteger.Create(1, APrivateKey);
  LPrivKeyParams := TECPrivateKeyParameters.Create('ECDSA',LPrivD,LDomain);
  //
  LParam := TParametersWithRandom.Create(LPrivKeyParams, FRandom);
  LSigner := TECDsaSigner.Create();
  LSigner.Init(True, LParam);
  LSignerResult := LSigner.GenerateSignature(AMessage);
  ASignature.r := LSignerResult[0].ToByteArray();
  ASignature.s := LSignerResult[1].ToByteArray();
  Result := True;
end;

class function TPCCryptoLib4Pascal.DoECDSAVerify(const APublicKey: TECDSA_Public; const AMessage: TBytes; const ASignature: TECDSA_SIG): Boolean;
var
  LDomain: IECDomainParameters;
  LCurve: IX9ECParameters;
  LSigner: IECDsaSigner;
  LPubKeyParams : IECPublicKeyParameters;
  LPoint: IECPoint;
  LBigXCoord, LBigYCoord,
  LSigR, LSigS: TBigInteger;
begin
  GetCurveAndDomainParameters(APublicKey.EC_OpenSSL_NID,LCurve,LDomain);
  LBigXCoord := TBigInteger.Create(1, APublicKey.x);
  LBigYCoord := TBigInteger.Create(1, APublicKey.y);
  LPoint := LCurve.Curve.CreatePoint(LBigXCoord, LBigYCoord);
  LPubKeyParams := TECPublicKeyParameters.Create('ECDSA', LPoint, LDomain);

  LSigR := TBigInteger.Create(1, ASignature.r);
  LSigS := TBigInteger.Create(1, ASignature.s);

  LSigner := TECDsaSigner.Create();
  LSigner.Init(False, LPubKeyParams);
  Result := LSigner.VerifySignature(AMessage, LSigR, LSigS);
end;

class function TPCCryptoLib4Pascal.DoGetPublicKey(const AEC_OpenSSL_NID: Word; const APrivateKey: TBytes): TECDSA_Public;
var
  LDomain: IECDomainParameters;
  LPrivD : TBigInteger;
  LECPrivateKeyParameters : IECPrivateKeyParameters;
  LPublicKey: IECPublicKeyParameters;
begin
  LDomain := GetDomainParameters(AEC_OpenSSL_NID);

  LPrivD := TBigInteger.Create(1,APrivateKey); // Obtain a big num based on private key
  LECPrivateKeyParameters := TECPrivateKeyParameters.Create('ECDSA',LPrivD,LDomain);

  LPublicKey := TECKeyPairGenerator.GetCorrespondingPublicKey(LECPrivateKeyParameters);

  Result.EC_OpenSSL_NID := AEC_OpenSSL_NID;
  Result.X := LPublicKey.Q.AffineXCoord.ToBigInteger().ToByteArrayUnsigned();
  Result.Y := LPublicKey.Q.AffineYCoord.ToBigInteger().ToByteArrayUnsigned();
 end;

class function TPCCryptoLib4Pascal.DoGetRandomPrivateKey(const AEC_OpenSSL_NID: Word): TBytes;
var
  LDomain: IECDomainParameters;
  LCurve: IX9ECParameters;
  KeyPairGeneratorInstance: IAsymmetricCipherKeyPairGenerator;
  LKeyPair: IAsymmetricCipherKeyPair;
  LPrivateKey: IECPrivateKeyParameters;
begin
  GetCurveAndDomainParameters(AEC_OpenSSL_NID,LCurve,LDomain);
  KeyPairGeneratorInstance := TGeneratorUtilities.GetKeyPairGenerator('ECDSA');
  KeyPairGeneratorInstance.Init(TECKeyGenerationParameters.Create(LDomain, FRandom) as IECKeyGenerationParameters);
  LKeyPair := KeyPairGeneratorInstance.GenerateKeyPair();
  LPrivateKey := LKeyPair.&Private as IECPrivateKeyParameters;
  Result := LPrivateKey.D.ToByteArray();
end;

class function TPCCryptoLib4Pascal.DoPascalCoinAESDecrypt(
  const AEncryptedMessage, APassword: TBytes; out ADecryptedMessage: TBytes): boolean;
var
  SaltBytes, KeyBytes, IVBytes, Buf, Chopped: TRawBytes;
  KeyParametersWithIV: IParametersWithIV;
  cipher: IBufferedCipher;
  LBufStart, LSrcStart, LCount: Int32;
begin
  try
    System.SetLength(SaltBytes, SALT_SIZE);
    // First read the magic text and the salt - if any
    Chopped := System.Copy(AEncryptedMessage, 0, SALT_MAGIC_LEN);
    if (System.Length(AEncryptedMessage) >= SALT_MAGIC_LEN) and
      (Chopped.ToString = SALT_MAGIC) then
    begin
      System.Move(AEncryptedMessage[SALT_MAGIC_LEN], SaltBytes[0], SALT_SIZE);
      if not EVP_GetKeyIV(APassword, SaltBytes, KeyBytes, IVBytes) then
      begin
        Result := False;
        Exit;
      end;
      LSrcStart := SALT_MAGIC_LEN + SALT_SIZE;
    end
    else
    begin
      if not EVP_GetKeyIV(APassword, nil, KeyBytes, IVBytes) then
      begin
        Result := False;
        Exit;
      end;
      LSrcStart := 0;
    end;

    cipher := TCipherUtilities.GetCipher('AES/CBC/PKCS7PADDING');
    KeyParametersWithIV := TParametersWithIV.Create(TParameterUtilities.CreateKeyParameter('AES', KeyBytes), IVBytes);

    cipher.Init(False, KeyParametersWithIV); // init decryption cipher

    System.SetLength(Buf, System.Length(AEncryptedMessage));

    LBufStart := 0;

    LCount := cipher.ProcessBytes(AEncryptedMessage, LSrcStart, System.Length(AEncryptedMessage) - LSrcStart, Buf, LBufStart);
    System.Inc(LBufStart, LCount);
    LCount := cipher.DoFinal(Buf, LBufStart);
    System.Inc(LBufStart, LCount);

    System.SetLength(Buf, LBufStart);

    ADecryptedMessage := System.Copy(Buf);

    Result := True;
  except
    Result := False;
  end;
end;

class function TPCCryptoLib4Pascal.DoPascalCoinAESEncrypt(const AMessage, APassword: TBytes): TBytes;
var
  SaltBytes, KeyBytes, IVBytes, Buf, LAuxBuf: TRawBytes;
  KeyParametersWithIV: IParametersWithIV;
  cipher: IBufferedCipher;
  LBlockSize, LBufStart, Count: Int32;
begin
  SaltBytes := EVP_GetSalt();
  EVP_GetKeyIV(APassword, SaltBytes, KeyBytes, IVBytes);
  cipher := TCipherUtilities.GetCipher('AES/CBC/PKCS7PADDING');
  KeyParametersWithIV := TParametersWithIV.Create
    (TParameterUtilities.CreateKeyParameter('AES', KeyBytes), IVBytes);

  cipher.Init(True, KeyParametersWithIV); // init encryption cipher
  LBlockSize := cipher.GetBlockSize;

  System.SetLength(Buf, System.Length(AMessage) + LBlockSize + SALT_MAGIC_LEN + PKCS5_SALT_LEN);

  LBufStart := 0;

  LAuxBuf.FromString(SALT_MAGIC);
  System.Move(LAuxBuf[0], Buf[LBufStart], SALT_MAGIC_LEN * System.SizeOf(byte));
  System.Inc(LBufStart, SALT_MAGIC_LEN);
  System.Move(SaltBytes[0], Buf[LBufStart], PKCS5_SALT_LEN * System.SizeOf(byte));
  System.Inc(LBufStart, PKCS5_SALT_LEN);

  Count := cipher.ProcessBytes(AMessage, 0, System.Length(AMessage), Buf, LBufStart);
  System.Inc(LBufStart, Count);
  Count := cipher.DoFinal(Buf, LBufStart);
  System.Inc(LBufStart, Count);

  System.SetLength(Buf, LBufStart);
  Result := Buf;
end;

class function TPCCryptoLib4Pascal.DoPascalCoinECIESDecrypt(
  const AEC_OpenSSL_NID: Word; const APrivateKey, AEncryptedMessage: TBytes;
  var ADecryptedMessage: TBytes): Boolean;
var
  LDomain: IECDomainParameters;
  LPrivD: TBigInteger;
  LPrivKeyParams : IECPrivateKeyParameters;

  LCurve: IX9ECParameters;
  LBigXCoord, LBigYCoord : TBigInteger;
  LPoint: IECPoint;
  LPubKeyParams : IECPublicKeyParameters;
  //
  LCipherDecrypt: IIESCipher;
begin
  try
    LDomain := GetDomainParameters(AEC_OpenSSL_NID);
    LPrivD := TBigInteger.Create(1, APrivateKey);
    LPrivKeyParams := TECPrivateKeyParameters.Create('ECDSA',LPrivD,LDomain);

    // Decryption
    LCipherDecrypt := TIESCipher.Create(FPascalCoinIESEngine);
    LCipherDecrypt.Init(False, LPrivKeyParams, FPascalCoinIESWithCipherParameters, FRandom);
    ADecryptedMessage := System.Copy(LCipherDecrypt.DoFinal(AEncryptedMessage));
    Result := True;
  except
    Result := False;
  end;
end;

class function TPCCryptoLib4Pascal.DoPascalCoinECIESEncrypt(
  const APublicKey: TECDSA_Public; const AMessage: TBytes;
  var AEncryptedMessage: TBytes): Boolean;
var
  LDomain: IECDomainParameters;
  LCurve: IX9ECParameters;
  LBigXCoord, LBigYCoord : TBigInteger;
  LPoint: IECPoint;
  LPubKeyParams : IECPublicKeyParameters;
  //
  LCipherEncrypt: IIESCipher;
begin
  GetCurveAndDomainParameters(APublicKey.EC_OpenSSL_NID,LCurve,LDomain);
  LBigXCoord := TBigInteger.Create(1, APublicKey.x);
  LBigYCoord := TBigInteger.Create(1, APublicKey.y);
  LPoint := LCurve.Curve.CreatePoint(LBigXCoord, LBigYCoord);
  LPubKeyParams := TECPublicKeyParameters.Create('ECDSA', LPoint, LDomain);
  // Encryption
  LCipherEncrypt := TIESCipher.Create(FPascalCoinIESEngine);
  LCipherEncrypt.Init(True, LPubKeyParams, FPascalCoinIESWithCipherParameters, FRandom);
  AEncryptedMessage := LCipherEncrypt.DoFinal(AMessage);
  Result := True;
end;

class procedure TPCCryptoLib4Pascal.DoRIPEMD160(const AInput: TBytes; var AOutput: TBytes);
Var Lrmd160 : TRIPEMD160;
begin
  Lrmd160 := TRIPEMD160.Create;
  try
    AOutput := Lrmd160.ComputeBytes(AInput).GetBytes;
  finally
    Lrmd160.Free;
  end;
end;

class procedure TPCCryptoLib4Pascal.DoSHA256(const AInput: TBytes; var AOutput: TBytes);
var Lsha : TSHA2_256;
begin
  Lsha := TSHA2_256.Create;
  try
    AOutput := Lsha.ComputeBytes(AInput).GetBytes;
  finally
    Lsha.Free;
  end;
end;

class function TPCCryptoLib4Pascal.EVP_GetKeyIV(const APasswordBytes,
  ASaltBytes: TBytes; out AKeyBytes, AIVBytes: TBytes): boolean;
var
  LKey, LIV: Int32;
  LDigest: IDigest;
begin
  LKey := 32; // AES256 CBC Key Length
  LIV := 16; // AES256 CBC IV Length
  System.SetLength(AKeyBytes, LKey);
  System.SetLength(AIVBytes, LKey);
  // Max size to start then reduce it at the end
  LDigest := TDigestUtilities.GetDigest('SHA-256'); // SHA2_256
  System.Assert(LDigest.GetDigestSize >= LKey);
  System.Assert(LDigest.GetDigestSize >= LIV);
  // Derive Key First
  LDigest.BlockUpdate(APasswordBytes, 0, System.Length(APasswordBytes));
  if ASaltBytes <> Nil then
  begin
    LDigest.BlockUpdate(ASaltBytes, 0, System.Length(ASaltBytes));
  end;
  LDigest.DoFinal(AKeyBytes, 0);
  // Derive IV Next
  LDigest.Reset();
  LDigest.BlockUpdate(AKeyBytes, 0, System.Length(AKeyBytes));
  LDigest.BlockUpdate(APasswordBytes, 0, System.Length(APasswordBytes));
  if ASaltBytes <> Nil then
  begin
    LDigest.BlockUpdate(ASaltBytes, 0, System.Length(ASaltBytes));
  end;
  LDigest.DoFinal(AIVBytes, 0);

  System.SetLength(AIVBytes, LIV);
  Result := True;
end;

class function TPCCryptoLib4Pascal.EVP_GetSalt: TBytes;
begin
  System.SetLength(Result, PKCS5_SALT_LEN);
  FRandom.NextBytes(Result);

end;

class function TPCCryptoLib4Pascal.GetCurveAndDomainParameters(
  const AEC_OpenSSL_NID: Word; var OCurve: IX9ECParameters;
  var ODomain: IECDomainParameters; ARaiseIfNotForPascal: Boolean): Boolean;
begin
  Result := True;
  case AEC_OpenSSL_NID of
    CT_NID_secp256k1 : begin
      OCurve := FCurve_SECP256K1;
      ODomain := FDomain_SECP256K1;
    end;
    CT_NID_secp384r1 : begin
      OCurve := FCurve_SECP384R1;
      ODomain := FDomain_SECP384R1;
    end;
    CT_NID_secp521r1 : begin
      OCurve := FCurve_SECP521R1;
      ODomain := FDomain_SECP521R1;
    end;
    CT_NID_sect283k1 : begin
      OCurve := FCurve_SECT283K1;
      ODomain := FDomain_SECT283K1;
    end;
  else
    if ARaiseIfNotForPascal then raise Exception.Create(Format('Invalid Curve type:%d',[AEC_OpenSSL_NID]))
    else Result := False;
  end;
end;

class function TPCCryptoLib4Pascal.GetDomainParameters(const AEC_OpenSSL_NID: Word): IECDomainParameters;
begin
  case AEC_OpenSSL_NID of
    CT_NID_secp256k1 : Result := FDomain_SECP256K1;
    CT_NID_secp384r1 : Result := FDomain_SECP384R1;
    CT_NID_secp521r1 : Result := FDomain_SECP521R1;
    CT_NID_sect283k1 : Result := FDomain_SECT283K1;
  else raise Exception.Create(Format('Invalid Curve type:%d',[AEC_OpenSSL_NID]));
  end;
end;

class constructor TPCCryptoLib4Pascal.TPCCryptoLib4Pascal;
    function GetIESCipherParameters: IIESWithCipherParameters;
    var
      Derivation, Encoding, IVBytes: TBytes;
      MacKeySizeInBits, CipherKeySizeInBits: Int32;
      UsePointCompression: boolean;
    begin
      // Set up IES Cipher Parameters For Compatibility With PascalCoin Current Implementation

      // The derivation and encoding vectors are used when initialising the KDF and MAC.
      // They're optional but if used then they need to be known by the other user so that
      // they can decrypt the ciphertext and verify the MAC correctly. The security is based
      // on the shared secret coming from the (static-ephemeral) ECDH key agreement.
      Derivation := nil;

      Encoding := nil;

      System.SetLength(IVBytes, 16); // using Zero Initialized IV for compatibility

      MacKeySizeInBits := 32 * 8;

      // Since we are using AES256_CBC for compatibility
      CipherKeySizeInBits := 32 * 8;

      // whether to use point compression when deriving the octets string
      // from a point or not in the EphemeralKeyPairGenerator
      UsePointCompression := True; // for compatibility

      Result := TIESWithCipherParameters.Create(Derivation, Encoding,
        MacKeySizeInBits, CipherKeySizeInBits, IVBytes, UsePointCompression);
    end;
    function GetECIESPascalCoinCompatibilityEngine(): IPascalCoinIESEngine;
    var
      cipher: IBufferedBlockCipher;
      AesEngine: IAesEngine;
      blockCipher: ICbcBlockCipher;
      ECDHBasicAgreementInstance: IECDHBasicAgreement;
      KDFInstance: IPascalCoinECIESKdfBytesGenerator;
      DigestMACInstance: IMac;

    begin
      // Set up IES Cipher Engine For Compatibility With PascalCoin

      ECDHBasicAgreementInstance := TECDHBasicAgreement.Create();

      KDFInstance := TPascalCoinECIESKdfBytesGenerator.Create
        (TDigestUtilities.GetDigest('SHA-512'));

      DigestMACInstance := TMacUtilities.GetMac('HMAC-MD5');

      // Set Up Block Cipher
      AesEngine := TAesEngine.Create(); // AES Engine

      blockCipher := TCbcBlockCipher.Create(AesEngine); // CBC

      cipher := TPaddedBufferedBlockCipher.Create(blockCipher,
        TZeroBytePadding.Create() as IZeroBytePadding); // ZeroBytePadding

      Result := TPascalCoinIESEngine.Create(ECDHBasicAgreementInstance, KDFInstance,
        DigestMACInstance, cipher);
    end;
var  LCipher: IBufferedBlockCipher;
  LAesEngine: IAesEngine;
  blockCipher: ICbcBlockCipher;
  ECDHBasicAgreementInstance: IECDHBasicAgreement;
  KDFInstance: IPascalCoinECIESKdfBytesGenerator;
  DigestMACInstance: IMac;
begin
  FRandom := TSecureRandom.Create();
  // Init Curves and Domains for quick usage
  FCurve_SECP256K1 := TCustomNamedCurves.GetByName('SECP256K1');
  FDomain_SECP256K1 := TECDomainParameters.Create(FCurve_SECP256K1.Curve, FCurve_SECP256K1.G, FCurve_SECP256K1.N, FCurve_SECP256K1.H, FCurve_SECP256K1.GetSeed);
  FCurve_SECP384R1 := TCustomNamedCurves.GetByName('SECP384R1');
  FDomain_SECP384R1 := TECDomainParameters.Create(FCurve_SECP384R1.Curve, FCurve_SECP384R1.G, FCurve_SECP384R1.N, FCurve_SECP384R1.H, FCurve_SECP384R1.GetSeed);
  FCurve_SECT283K1 := TCustomNamedCurves.GetByName('SECT283K1');
  FDomain_SECT283K1 := TECDomainParameters.Create(FCurve_SECT283K1.Curve, FCurve_SECT283K1.G, FCurve_SECT283K1.N, FCurve_SECT283K1.H, FCurve_SECT283K1.GetSeed);
  FCurve_SECP521R1 := TCustomNamedCurves.GetByName('SECP521R1');
  FDomain_SECP521R1 := TECDomainParameters.Create(FCurve_SECP521R1.Curve, FCurve_SECP521R1.G, FCurve_SECP521R1.N, FCurve_SECP521R1.H, FCurve_SECP521R1.GetSeed);
  // Init ECIES
  FPascalCoinIESEngine := GetECIESPascalCoinCompatibilityEngine;
  FPascalCoinIESWithCipherParameters := GetIESCipherParameters;

end;

end.
