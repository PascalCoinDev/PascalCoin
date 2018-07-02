unit HlpHashFactory;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF HAS_UNITSCOPE}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF HAS_UNITSCOPE}
  HlpHashRounds,
  HlpHashSize,
  HlpIHash,
  HlpIHashInfo,
  HlpHashLibTypes,
  // NullDigest Unit //
  HlpNullDigest,
  // Checksum Units //
  HlpAdler32,
  HlpCRC,
  HlpCRC16,
  HlpCRC32,
  HlpCRC64,
  // Hash32 Units //
  HlpAP,
  HlpBernstein,
  HlpBernstein1,
  HlpBKDR,
  HlpDEK,
  HlpDJB,
  HlpELF,
  HlpFNV,
  HlpFNV1a,
  HlpJenkins3,
  HlpJS,
  HlpMurmur2,
  HlpMurmurHash3_x86_32,
  HlpOneAtTime,
  HlpPJW,
  HlpRotating,
  HlpRS,
  HlpSDBM,
  HlpShiftAndXor,
  HlpSuperFast,
  HlpXXHash32,
  // Hash64 Units //
  HlpFNV64,
  HlpFNV1a64,
  HlpMurmur2_64,
  HlpSipHash,
  HlpXXHash64,
  // Hash128 Units //
  HlpMurmurHash3_x86_128,
  HlpMurmurHash3_x64_128,
  // Crypto Units
  HlpTiger,
  HlpTiger2,
  HlpMD2,
  HlpMD4,
  HlpMD5,
  HlpSHA0,
  HlpSHA1,
  HlpSHA2_224,
  HlpSHA2_256,
  HlpSHA2_384,
  HlpSHA2_512,
  HlpSHA2_512_224,
  HlpSHA2_512_256,
  HlpGrindahl256,
  HlpGrindahl512,
  HlpPanama,
  HlpWhirlPool,
  HlpRadioGatun32,
  HlpRadioGatun64,
  HlpSnefru,
  HlpHaval,
  HlpGost,
  HlpGOST3411_2012,
  HlpHAS160,
  HlpRIPEMD,
  HlpRIPEMD128,
  HlpRIPEMD160,
  HlpRIPEMD256,
  HlpRIPEMD320,
  HlpSHA3,
  HlpBlake2B,
  HlpIBlake2BConfig,
  HlpBlake2BConfig,
  HlpBlake2S,
  HlpBlake2SConfig,
  HlpIBlake2SConfig,
  // HMAC Unit
  HlpHMACNotBuildInAdapter,
  // PBKDF2_HMAC Unit
  HlpPBKDF2_HMACNotBuildInAdapter;

type
  THashFactory = class sealed(TObject)

    // ====================== TNullDigestFactory ====================== //

  type
    TNullDigestFactory = class sealed(TObject)

    public
      class function CreateNullDigest(): IHash; static;

    end;

    // ====================== TChecksum ====================== //

  type
    TChecksum = class sealed(TObject)

    public

      class function CreateCRC(_Width: Int32; _poly, _Init: UInt64;
        _refIn, _refOut: Boolean; _XorOut, _check: UInt64;
        _Names: THashLibStringArray): IHash; overload; static;

      class function CreateCRC(_value: TCRCStandard): IHash; overload; static;

      class function CreateCRC16(_poly, _Init: UInt64; _refIn, _refOut: Boolean;
        _XorOut, _check: UInt64; _Names: THashLibStringArray): IHash; static;

      class function CreateCRC32(_poly, _Init: UInt64; _refIn, _refOut: Boolean;
        _XorOut, _check: UInt64; _Names: THashLibStringArray): IHash; static;

      class function CreateCRC64(_poly, _Init: UInt64; _refIn, _refOut: Boolean;
        _XorOut, _check: UInt64; _Names: THashLibStringArray): IHash; static;

      /// <summary>
      /// BUYPASS, polynomial = $8005
      /// </summary>
      /// <returns></returns>
      class function CreateCRC16_BUYPASS(): IHash; static;

      /// <summary>
      /// PKZIP, polynomial = $04C11DB7
      /// </summary>
      /// <returns></returns>
      class function CreateCRC32_PKZIP(): IHash; static;
      /// <summary>
      /// Castagnoli, polynomial = $1EDC6F41
      /// </summary>
      /// <returns></returns>
      class function CreateCRC32_CASTAGNOLI(): IHash; static;
      /// <summary>
      /// ECMA-182, polynomial = $42F0E1EBA9EA3693
      /// </summary>
      /// <returns></returns>
      class function CreateCRC64_ECMA(): IHash; static;

      class function CreateAdler32: IHash; static;
    end;

    // ====================== THash32 ====================== //

  type
    THash32 = class sealed(TObject)

    public
      class function CreateAP(): IHash; static;

      class function CreateBernstein(): IHash; static;
      class function CreateBernstein1(): IHash; static;

      class function CreateBKDR(): IHash; static;

      class function CreateDEK(): IHash; static;

      class function CreateDJB(): IHash; static;

      class function CreateELF(): IHash; static;

      class function CreateFNV(): IHash; static;
      class function CreateFNV1a(): IHash; static;

      class function CreateJenkins3(): IHash; static;

      class function CreateJS(): IHash; static;

      class function CreateMurmur2(): IHashWithKey; static;

      class function CreateMurmurHash3_x86_32(): IHashWithKey; static;

      class function CreateOneAtTime(): IHash; static;

      class function CreatePJW(): IHash; static;

      class function CreateRotating(): IHash; static;

      class function CreateRS(): IHash; static;

      class function CreateSDBM(): IHash; static;

      class function CreateShiftAndXor(): IHash; static;

      class function CreateSuperFast(): IHash; static;

      class function CreateXXHash32(): IHashWithKey; static;

    end;

    // ====================== THash64 ====================== //

  type
    THash64 = class sealed(TObject)

    public

      class function CreateFNV(): IHash; static;
      class function CreateFNV1a(): IHash; static;

      class function CreateMurmur2(): IHashWithKey; static;

      class function CreateSipHash2_4(): IHashWithKey; static;

      class function CreateXXHash64(): IHashWithKey; static;

    end;

    // ====================== THash128 ====================== //

  type
    THash128 = class sealed(TObject)

    public

      class function CreateMurmurHash3_x86_128(): IHashWithKey; static;
      class function CreateMurmurHash3_x64_128(): IHashWithKey; static;

    end;

    // ====================== TCrypto ====================== //

  type
    TCrypto = class sealed(TObject)

    public

      /// <summary>
      ///
      /// </summary>
      /// <param name="a_hash_size">16, 20 or 24 bytes. </param>
      /// <param name="a_rounds">no of rounds (standard rounds are 3, 4 and 5)</param>
      /// <returns></returns>
      class function CreateTiger(a_hash_size: Int32; a_rounds: THashRounds)
        : IHash; static;
      class function CreateTiger_3_128(): IHash; static;
      class function CreateTiger_3_160(): IHash; static;
      class function CreateTiger_3_192(): IHash; static;

      class function CreateTiger_4_128(): IHash; static;
      class function CreateTiger_4_160(): IHash; static;
      class function CreateTiger_4_192(): IHash; static;

      class function CreateTiger_5_128(): IHash; static;
      class function CreateTiger_5_160(): IHash; static;
      class function CreateTiger_5_192(): IHash; static;

      /// <summary>
      ///
      /// </summary>
      /// <param name="a_hash_size">16, 20 or 24 bytes. </param>
      /// <param name="a_rounds">no of rounds (standard rounds are 3, 4 and 5)</param>
      /// <returns></returns>
      class function CreateTiger2(a_hash_size: Int32; a_rounds: THashRounds)
        : IHash; static;

      class function CreateTiger2_3_128(): IHash; static;
      class function CreateTiger2_3_160(): IHash; static;
      class function CreateTiger2_3_192(): IHash; static;

      class function CreateTiger2_4_128(): IHash; static;
      class function CreateTiger2_4_160(): IHash; static;
      class function CreateTiger2_4_192(): IHash; static;

      class function CreateTiger2_5_128(): IHash; static;
      class function CreateTiger2_5_160(): IHash; static;
      class function CreateTiger2_5_192(): IHash; static;

      class function CreateMD2(): IHash; static;
      class function CreateMD4(): IHash; static;
      class function CreateMD5(): IHash; static;

      class function CreateSHA0(): IHash; static;
      class function CreateSHA1(): IHash; static;

      class function CreateSHA2_224(): IHash; static;
      class function CreateSHA2_256(): IHash; static;
      class function CreateSHA2_384(): IHash; static;
      class function CreateSHA2_512(): IHash; static;
      class function CreateSHA2_512_224(): IHash; static;
      class function CreateSHA2_512_256(): IHash; static;

      class function CreateGrindahl256(): IHash; static;
      class function CreateGrindahl512(): IHash; static;

      class function CreatePanama(): IHash; static;

      class function CreateWhirlPool(): IHash; static;

      class function CreateRadioGatun32(): IHash; static;
      class function CreateRadioGatun64(): IHash; static;

      /// <summary>
      ///
      /// </summary>
      /// <param name="a_security_level">any Integer value greater than 0. Standard is 8. </param>
      /// <param name="a_hash_size">128bit, 256bit</param>
      /// <returns></returns>
      class function CreateSnefru(a_security_level: Int32;
        a_hash_size: THashSize): IHash; static;
      class function CreateSnefru_8_128(): IHash; static;
      class function CreateSnefru_8_256(): IHash; static;

      /// <summary>
      ///
      /// </summary>
      /// <param name="a_rounds">3, 4, 5</param>
      /// <param name="a_hash_size">128, 160, 192, 224, 256</param>
      /// <returns></returns>
      class function CreateHaval(a_rounds: THashRounds; a_hash_size: THashSize)
        : IHash; static;

      class function CreateHaval_3_128(): IHash; static;
      class function CreateHaval_4_128(): IHash; static;
      class function CreateHaval_5_128(): IHash; static;

      class function CreateHaval_3_160(): IHash; static;
      class function CreateHaval_4_160(): IHash; static;
      class function CreateHaval_5_160(): IHash; static;

      class function CreateHaval_3_192(): IHash; static;
      class function CreateHaval_4_192(): IHash; static;
      class function CreateHaval_5_192(): IHash; static;

      class function CreateHaval_3_224(): IHash; static;
      class function CreateHaval_4_224(): IHash; static;
      class function CreateHaval_5_224(): IHash; static;

      class function CreateHaval_3_256(): IHash; static;
      class function CreateHaval_4_256(): IHash; static;
      class function CreateHaval_5_256(): IHash; static;

      class function CreateGost(): IHash; static;

      // Streebog 256
      class function CreateGOST3411_2012_256(): IHash; static;

      // Streebog 512
      class function CreateGOST3411_2012_512(): IHash; static;

      class function CreateHAS160(): IHash; static;

      class function CreateRIPEMD(): IHash; static;
      class function CreateRIPEMD128(): IHash; static;
      class function CreateRIPEMD160(): IHash; static;
      class function CreateRIPEMD256(): IHash; static;
      class function CreateRIPEMD320(): IHash; static;

      class function CreateSHA3_224(): IHash; static;
      class function CreateSHA3_256(): IHash; static;
      class function CreateSHA3_384(): IHash; static;
      class function CreateSHA3_512(): IHash; static;

      class function CreateBlake2B(config: IBlake2BConfig = Nil): IHash; static;

      class function CreateBlake2B_160(): IHash; static;
      class function CreateBlake2B_256(): IHash; static;
      class function CreateBlake2B_384(): IHash; static;
      class function CreateBlake2B_512(): IHash; static;

      class function CreateBlake2S(config: IBlake2SConfig = Nil): IHash; static;

      class function CreateBlake2S_128(): IHash; static;
      class function CreateBlake2S_160(): IHash; static;
      class function CreateBlake2S_224(): IHash; static;
      class function CreateBlake2S_256(): IHash; static;

    end;

    // ====================== THMAC ====================== //

  type
    THMAC = class sealed(TObject)

    public

      class function CreateHMAC(a_hash: IHash): IHMAC; static;

    end;

  end;

type
  TKDF = class sealed(TObject)

    // ====================== TPBKDF2_HMAC ====================== //

  type
    TPBKDF2_HMAC = class sealed(TObject)

    public

      /// <summary>
      /// Initializes a new interface instance of the TPBKDF2_HMAC class using a password, a salt, a number of iterations and an Instance of an "IHash" to be used as an "IHMAC" hashing implementation to derive the key.
      /// </summary>
      /// <param name="a_hash">The name of the "IHash" implementation to be transformed to an "IHMAC" Instance so it can be used to derive the key.</param>
      /// <param name="password">The password to derive the key for.</param>
      /// <param name="salt">The salt to use to derive the key.</param>
      /// <param name="iterations">The number of iterations to use to derive the key.</param>
      /// <exception cref="EArgumentNilHashLibException">The password, salt or algorithm is Nil.</exception>
      /// <exception cref="EArgumentHashLibException">The iteration is less than 1.</exception>

      class function CreatePBKDF2_HMAC(a_hash: IHash;
        a_password, a_salt: THashLibByteArray; a_iterations: UInt32)
        : IPBKDF2_HMAC; static;

    end;

  end;

implementation

{ THashFactory.TNullDigestFactory }

class function THashFactory.TNullDigestFactory.CreateNullDigest: IHash;
begin
  Result := TNullDigest.Create();
end;

{ THashFactory.TChecksum }

class function THashFactory.TChecksum.CreateCRC(_Width: Int32;
  _poly, _Init: UInt64; _refIn, _refOut: Boolean; _XorOut, _check: UInt64;
  _Names: THashLibStringArray): IHash;
begin
  Result := TCRC.Create(_Width, _poly, _Init, _refIn, _refOut, _XorOut,
    _check, _Names);
end;

class function THashFactory.TChecksum.CreateCRC(_value: TCRCStandard): IHash;
begin
  Result := TCRC.CreateCRCObject(_value);
end;

class function THashFactory.TChecksum.CreateCRC16(_poly, _Init: UInt64;
  _refIn, _refOut: Boolean; _XorOut, _check: UInt64;
  _Names: THashLibStringArray): IHash;
begin
  Result := TCRC16.Create(_poly, _Init, _refIn, _refOut, _XorOut,
    _check, _Names);
end;

class function THashFactory.TChecksum.CreateCRC16_BUYPASS: IHash;
begin
  Result := TCRC16_BUYPASS.Create();
end;

class function THashFactory.TChecksum.CreateCRC32(_poly, _Init: UInt64;
  _refIn, _refOut: Boolean; _XorOut, _check: UInt64;
  _Names: THashLibStringArray): IHash;
begin
  Result := TCRC32.Create(_poly, _Init, _refIn, _refOut, _XorOut,
    _check, _Names);
end;

class function THashFactory.TChecksum.CreateCRC32_CASTAGNOLI: IHash;
begin
  Result := TCRC32_CASTAGNOLI.Create();
end;

class function THashFactory.TChecksum.CreateCRC32_PKZIP: IHash;
begin
  Result := TCRC32_PKZIP.Create();
end;

class function THashFactory.TChecksum.CreateCRC64(_poly, _Init: UInt64;
  _refIn, _refOut: Boolean; _XorOut, _check: UInt64;
  _Names: THashLibStringArray): IHash;
begin
  Result := TCRC64.Create(_poly, _Init, _refIn, _refOut, _XorOut,
    _check, _Names);
end;

class function THashFactory.TChecksum.CreateCRC64_ECMA: IHash;
begin
  Result := TCRC64_ECMA.Create();
end;

class function THashFactory.TChecksum.CreateAdler32: IHash;
begin
  Result := TAdler32.Create();
end;

{ THashFactory.THash32 }

class function THashFactory.THash32.CreateAP: IHash;
begin
  Result := TAP.Create();
end;

class function THashFactory.THash32.CreateBernstein: IHash;
begin
  Result := TBernstein.Create();
end;

class function THashFactory.THash32.CreateBernstein1: IHash;
begin
  Result := TBernstein1.Create();
end;

class function THashFactory.THash32.CreateBKDR: IHash;
begin
  Result := TBKDR.Create();
end;

class function THashFactory.THash32.CreateDEK: IHash;
begin
  Result := TDEK.Create();
end;

class function THashFactory.THash32.CreateDJB: IHash;
begin
  Result := TDJB.Create();
end;

class function THashFactory.THash32.CreateELF: IHash;
begin
  Result := TELF.Create();
end;

class function THashFactory.THash32.CreateFNV: IHash;
begin
  Result := TFNV.Create();
end;

class function THashFactory.THash32.CreateFNV1a: IHash;
begin
  Result := TFNV1a.Create();
end;

class function THashFactory.THash32.CreateJenkins3: IHash;
begin
  Result := TJenkins3.Create();
end;

class function THashFactory.THash32.CreateJS: IHash;
begin
  Result := TJS.Create();
end;

class function THashFactory.THash32.CreateMurmur2: IHashWithKey;
begin
  Result := TMurmur2.Create();
end;

class function THashFactory.THash32.CreateMurmurHash3_x86_32: IHashWithKey;
begin
  Result := TMurmurHash3_x86_32.Create();
end;

class function THashFactory.THash32.CreateOneAtTime: IHash;
begin
  Result := TOneAtTime.Create();
end;

class function THashFactory.THash32.CreatePJW: IHash;
begin
  Result := TPJW.Create();
end;

class function THashFactory.THash32.CreateRotating: IHash;
begin
  Result := TRotating.Create();
end;

class function THashFactory.THash32.CreateRS: IHash;
begin
  Result := TRS.Create();
end;

class function THashFactory.THash32.CreateSDBM: IHash;
begin
  Result := TSDBM.Create();
end;

class function THashFactory.THash32.CreateShiftAndXor: IHash;
begin
  Result := TShiftAndXor.Create();
end;

class function THashFactory.THash32.CreateSuperFast: IHash;
begin
  Result := TSuperFast.Create();
end;

class function THashFactory.THash32.CreateXXHash32: IHashWithKey;
begin
  Result := TXXHash32.Create();
end;

{ THashFactory.THash64 }

class function THashFactory.THash64.CreateFNV: IHash;
begin
  Result := TFNV64.Create();
end;

class function THashFactory.THash64.CreateFNV1a: IHash;
begin
  Result := TFNV1a64.Create();
end;

class function THashFactory.THash64.CreateMurmur2: IHashWithKey;
begin
  Result := TMurmur2_64.Create();
end;

class function THashFactory.THash64.CreateSipHash2_4: IHashWithKey;
begin
  Result := TSipHash2_4.Create();
end;

class function THashFactory.THash64.CreateXXHash64: IHashWithKey;
begin
  Result := TXXHash64.Create();
end;

{ THashFactory.THash128 }

class function THashFactory.THash128.CreateMurmurHash3_x86_128: IHashWithKey;
begin
  Result := TMurmurHash3_x86_128.Create();
end;

class function THashFactory.THash128.CreateMurmurHash3_x64_128: IHashWithKey;
begin
  Result := TMurmurHash3_x64_128.Create();
end;

{ THashFactory.TCrypto }

class function THashFactory.TCrypto.CreateGost: IHash;
begin
  Result := TGost.Create();
end;

class function THashFactory.TCrypto.CreateGOST3411_2012_256: IHash;
begin
  Result := TGOST3411_2012_256.Create();
end;

class function THashFactory.TCrypto.CreateGOST3411_2012_512: IHash;
begin
  Result := TGOST3411_2012_512.Create();
end;

class function THashFactory.TCrypto.CreateGrindahl256: IHash;
begin
  Result := TGrindahl256.Create();
end;

class function THashFactory.TCrypto.CreateGrindahl512: IHash;
begin
  Result := TGrindahl512.Create();
end;

class function THashFactory.TCrypto.CreateHAS160: IHash;
begin
  Result := THAS160.Create();
end;

class function THashFactory.TCrypto.CreateHaval(a_rounds: THashRounds;
  a_hash_size: THashSize): IHash;
begin
  case a_rounds of
    hrRounds3:
      case a_hash_size of
        hsHashSize128:
          Result := CreateHaval_3_128();
        hsHashSize160:
          Result := CreateHaval_3_160();
        hsHashSize192:
          Result := CreateHaval_3_192();
        hsHashSize224:
          Result := CreateHaval_3_224();
        hsHashSize256:
          Result := CreateHaval_3_256();
      else
        raise EArgumentHashLibException.CreateRes(@SInvalidHavalHashSize);
      end;

    hrRounds4:
      case a_hash_size of
        hsHashSize128:
          Result := CreateHaval_4_128();
        hsHashSize160:
          Result := CreateHaval_4_160();
        hsHashSize192:
          Result := CreateHaval_4_192();
        hsHashSize224:
          Result := CreateHaval_4_224();
        hsHashSize256:
          Result := CreateHaval_4_256();
      else
        raise EArgumentHashLibException.CreateRes(@SInvalidHavalHashSize);
      end;

    hrRounds5:
      case a_hash_size of
        hsHashSize128:
          Result := CreateHaval_5_128();
        hsHashSize160:
          Result := CreateHaval_5_160();
        hsHashSize192:
          Result := CreateHaval_5_192();
        hsHashSize224:
          Result := CreateHaval_5_224();
        hsHashSize256:
          Result := CreateHaval_5_256();
      else
        raise EArgumentHashLibException.CreateRes(@SInvalidHavalHashSize);
      end;

  else
    raise EArgumentHashLibException.CreateRes(@SInvalidHavalRound);
  end;
end;

class function THashFactory.TCrypto.CreateHaval_3_128: IHash;
begin
  Result := THaval_3_128.Create();
end;

class function THashFactory.TCrypto.CreateHaval_3_160: IHash;
begin
  Result := THaval_3_160.Create();
end;

class function THashFactory.TCrypto.CreateHaval_3_192: IHash;
begin
  Result := THaval_3_192.Create();
end;

class function THashFactory.TCrypto.CreateHaval_3_224: IHash;
begin
  Result := THaval_3_224.Create();
end;

class function THashFactory.TCrypto.CreateHaval_3_256: IHash;
begin
  Result := THaval_3_256.Create();
end;

class function THashFactory.TCrypto.CreateHaval_4_128: IHash;
begin
  Result := THaval_4_128.Create();
end;

class function THashFactory.TCrypto.CreateHaval_4_160: IHash;
begin
  Result := THaval_4_160.Create();
end;

class function THashFactory.TCrypto.CreateHaval_4_192: IHash;
begin
  Result := THaval_4_192.Create();
end;

class function THashFactory.TCrypto.CreateHaval_4_224: IHash;
begin
  Result := THaval_4_224.Create();
end;

class function THashFactory.TCrypto.CreateHaval_4_256: IHash;
begin
  Result := THaval_4_256.Create();
end;

class function THashFactory.TCrypto.CreateHaval_5_128: IHash;
begin
  Result := THaval_5_128.Create();
end;

class function THashFactory.TCrypto.CreateHaval_5_160: IHash;
begin
  Result := THaval_5_160.Create();
end;

class function THashFactory.TCrypto.CreateHaval_5_192: IHash;
begin
  Result := THaval_5_192.Create();
end;

class function THashFactory.TCrypto.CreateHaval_5_224: IHash;
begin
  Result := THaval_5_224.Create();
end;

class function THashFactory.TCrypto.CreateHaval_5_256: IHash;
begin
  Result := THaval_5_256.Create();
end;

class function THashFactory.TCrypto.CreateMD2: IHash;
begin
  Result := TMD2.Create();
end;

class function THashFactory.TCrypto.CreateMD4: IHash;
begin
  Result := TMD4.Create();
end;

class function THashFactory.TCrypto.CreateMD5: IHash;
begin
  Result := TMD5.Create();
end;

class function THashFactory.TCrypto.CreatePanama: IHash;
begin
  Result := TPanama.Create();
end;

class function THashFactory.TCrypto.CreateRadioGatun32: IHash;
begin
  Result := TRadioGatun32.Create();
end;

class function THashFactory.TCrypto.CreateRadioGatun64: IHash;
begin
  Result := TRadioGatun64.Create();
end;

class function THashFactory.TCrypto.CreateRIPEMD: IHash;
begin
  Result := TRIPEMD.Create();
end;

class function THashFactory.TCrypto.CreateRIPEMD128: IHash;
begin
  Result := TRIPEMD128.Create();
end;

class function THashFactory.TCrypto.CreateRIPEMD160: IHash;
begin
  Result := TRIPEMD160.Create();
end;

class function THashFactory.TCrypto.CreateRIPEMD256: IHash;
begin
  Result := TRIPEMD256.Create();
end;

class function THashFactory.TCrypto.CreateRIPEMD320: IHash;
begin
  Result := TRIPEMD320.Create();
end;

class function THashFactory.TCrypto.CreateSHA0: IHash;
begin
  Result := TSHA0.Create();
end;

class function THashFactory.TCrypto.CreateSHA1: IHash;
begin
  Result := TSHA1.Create();
end;

class function THashFactory.TCrypto.CreateSHA2_224: IHash;
begin
  Result := TSHA2_224.Create();
end;

class function THashFactory.TCrypto.CreateSHA2_256: IHash;
begin
  Result := TSHA2_256.Create();
end;

class function THashFactory.TCrypto.CreateSHA2_384: IHash;
begin
  Result := TSHA2_384.Create();
end;

class function THashFactory.TCrypto.CreateSHA2_512: IHash;
begin
  Result := TSHA2_512.Create();
end;

class function THashFactory.TCrypto.CreateSHA2_512_224: IHash;
begin
  Result := TSHA2_512_224.Create();
end;

class function THashFactory.TCrypto.CreateSHA2_512_256: IHash;
begin
  Result := TSHA2_512_256.Create();
end;

class function THashFactory.TCrypto.CreateSHA3_224: IHash;
begin
  Result := TSHA3_224.Create();
end;

class function THashFactory.TCrypto.CreateSHA3_256: IHash;
begin
  Result := TSHA3_256.Create();
end;

class function THashFactory.TCrypto.CreateSHA3_384: IHash;
begin
  Result := TSHA3_384.Create();
end;

class function THashFactory.TCrypto.CreateSHA3_512: IHash;
begin
  Result := TSHA3_512.Create();
end;

class function THashFactory.TCrypto.CreateBlake2B
  (config: IBlake2BConfig): IHash;
begin
  if config = Nil then
  begin
    Result := TBlake2B.Create()
  end
  else
  begin
    Result := TBlake2B.Create(config);
  end;
end;

class function THashFactory.TCrypto.CreateBlake2B_160: IHash;
begin
  Result := THashFactory.TCrypto.CreateBlake2B
    (TBlake2BConfig.Create(THashSize.hsHashSize160));
end;

class function THashFactory.TCrypto.CreateBlake2B_256: IHash;
begin
  Result := THashFactory.TCrypto.CreateBlake2B
    (TBlake2BConfig.Create(THashSize.hsHashSize256));
end;

class function THashFactory.TCrypto.CreateBlake2B_384: IHash;
begin
  Result := THashFactory.TCrypto.CreateBlake2B
    (TBlake2BConfig.Create(THashSize.hsHashSize384));
end;

class function THashFactory.TCrypto.CreateBlake2B_512: IHash;
begin
  Result := THashFactory.TCrypto.CreateBlake2B
    (TBlake2BConfig.Create(THashSize.hsHashSize512));
end;

class function THashFactory.TCrypto.CreateBlake2S
  (config: IBlake2SConfig): IHash;
begin
  if config = Nil then
  begin
    Result := TBlake2S.Create()
  end
  else
  begin
    Result := TBlake2S.Create(config);
  end;
end;

class function THashFactory.TCrypto.CreateBlake2S_128: IHash;
begin
  Result := THashFactory.TCrypto.CreateBlake2S
    (TBlake2SConfig.Create(THashSize.hsHashSize128));
end;

class function THashFactory.TCrypto.CreateBlake2S_160: IHash;
begin
  Result := THashFactory.TCrypto.CreateBlake2S
    (TBlake2SConfig.Create(THashSize.hsHashSize160));
end;

class function THashFactory.TCrypto.CreateBlake2S_224: IHash;
begin
  Result := THashFactory.TCrypto.CreateBlake2S
    (TBlake2SConfig.Create(THashSize.hsHashSize224));
end;

class function THashFactory.TCrypto.CreateBlake2S_256: IHash;
begin
  Result := THashFactory.TCrypto.CreateBlake2S
    (TBlake2SConfig.Create(THashSize.hsHashSize256));
end;

class function THashFactory.TCrypto.CreateSnefru(a_security_level: Int32;
  a_hash_size: THashSize): IHash;
begin
  if a_security_level < Int32(1) then
    raise EArgumentHashLibException.CreateRes(@SInvalidSnefruLevel);

  if ((a_hash_size = THashSize.hsHashSize128) or
    (a_hash_size = THashSize.hsHashSize256)) then
  begin
    Result := TSnefru.Create(a_security_level, a_hash_size);
  end
  else
  begin
    raise EArgumentHashLibException.CreateRes(@SInvalidSnefruHashSize);
  end

end;

class function THashFactory.TCrypto.CreateSnefru_8_128: IHash;
begin
  Result := CreateSnefru(8, THashSize.hsHashSize128);
end;

class function THashFactory.TCrypto.CreateSnefru_8_256: IHash;
begin
  Result := CreateSnefru(8, THashSize.hsHashSize256);
end;

class function THashFactory.TCrypto.CreateTiger_3_128: IHash;
begin
  Result := TTiger_128.CreateRound3();
end;

class function THashFactory.TCrypto.CreateTiger_3_160: IHash;
begin
  Result := TTiger_160.CreateRound3();
end;

class function THashFactory.TCrypto.CreateTiger_3_192: IHash;
begin
  Result := TTiger_192.CreateRound3();
end;

class function THashFactory.TCrypto.CreateTiger_4_128: IHash;
begin
  Result := TTiger_128.CreateRound4();
end;

class function THashFactory.TCrypto.CreateTiger_4_160: IHash;
begin
  Result := TTiger_160.CreateRound4();
end;

class function THashFactory.TCrypto.CreateTiger_4_192: IHash;
begin
  Result := TTiger_192.CreateRound4();
end;

class function THashFactory.TCrypto.CreateTiger_5_128: IHash;
begin
  Result := TTiger_128.CreateRound5();
end;

class function THashFactory.TCrypto.CreateTiger_5_160: IHash;
begin
  Result := TTiger_160.CreateRound5();
end;

class function THashFactory.TCrypto.CreateTiger_5_192: IHash;
begin
  Result := TTiger_192.CreateRound5();
end;

class function THashFactory.TCrypto.CreateWhirlPool: IHash;
begin
  Result := TWhirlPool.Create();
end;

class function THashFactory.TCrypto.CreateTiger(a_hash_size: Int32;
  a_rounds: THashRounds): IHash;
begin
  if ((a_hash_size <> 16) and (a_hash_size <> 20) and (a_hash_size <> 24)) then
    raise EArgumentHashLibException.CreateRes(@SInvalidTigerHashSize);

  Result := TTiger_Base.Create(a_hash_size, a_rounds);
end;

class function THashFactory.TCrypto.CreateTiger2(a_hash_size: Int32;
  a_rounds: THashRounds): IHash;
begin
  if ((a_hash_size <> 16) and (a_hash_size <> 20) and (a_hash_size <> 24)) then
    raise EArgumentHashLibException.CreateRes(@SInvalidTiger2HashSize);

  Result := TTiger2_Base.Create(a_hash_size, a_rounds);
end;

class function THashFactory.TCrypto.CreateTiger2_3_128: IHash;
begin
  Result := TTiger2_128.CreateRound3();
end;

class function THashFactory.TCrypto.CreateTiger2_3_160: IHash;
begin
  Result := TTiger2_160.CreateRound3();
end;

class function THashFactory.TCrypto.CreateTiger2_3_192: IHash;
begin
  Result := TTiger2_192.CreateRound3();
end;

class function THashFactory.TCrypto.CreateTiger2_4_128: IHash;
begin
  Result := TTiger2_128.CreateRound4();
end;

class function THashFactory.TCrypto.CreateTiger2_4_160: IHash;
begin
  Result := TTiger2_160.CreateRound4();
end;

class function THashFactory.TCrypto.CreateTiger2_4_192: IHash;
begin
  Result := TTiger2_192.CreateRound4();
end;

class function THashFactory.TCrypto.CreateTiger2_5_128: IHash;
begin
  Result := TTiger2_128.CreateRound5();
end;

class function THashFactory.TCrypto.CreateTiger2_5_160: IHash;
begin
  Result := TTiger2_160.CreateRound5();
end;

class function THashFactory.TCrypto.CreateTiger2_5_192: IHash;
begin
  Result := TTiger2_192.CreateRound5();
end;

{ THashFactory.THMAC }

class function THashFactory.THMAC.CreateHMAC(a_hash: IHash): IHMAC;
begin

  if Supports(a_hash, IHMAC) then
  begin
    Result := (a_hash) as IHMAC;
    Exit;
  end
  else
  begin
    Result := THMACNotBuildInAdapter.Create(a_hash);
    Exit;
  end;

end;

{ TKDF.TPBKDF2_HMAC }

class function TKDF.TPBKDF2_HMAC.CreatePBKDF2_HMAC(a_hash: IHash;
  a_password, a_salt: THashLibByteArray; a_iterations: UInt32): IPBKDF2_HMAC;
begin

  if not(System.Assigned(a_hash)) then
    raise EArgumentNilHashLibException.CreateRes(@SUninitializedInstance);

  if (a_password = Nil) then
    raise EArgumentNilHashLibException.CreateRes(@SEmptyPassword);

  if (a_salt = Nil) then
    raise EArgumentNilHashLibException.CreateRes(@SEmptySalt);

  if (a_iterations < 1) then
    raise EArgumentHashLibException.CreateRes(@SIterationtooSmall);

  Result := TPBKDF2_HMACNotBuildInAdapter.Create(a_hash, a_password, a_salt,
    a_iterations);
end;

end.
