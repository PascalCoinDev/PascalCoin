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

unit ClpIESEngine;

{$I CryptoLib.inc}

interface

uses
  Classes,
  SysUtils,
  ClpIMac,
  ClpIIESEngine,
  ClpIBasicAgreement,
  ClpIDerivationFunction,
  ClpIBufferedBlockCipher,
  ClpICipherParameters,
  ClpIIESParameters,
  ClpIEphemeralKeyPairGenerator,
  ClpIAsymmetricKeyParameter,
  ClpKeyParameter,
  ClpIKeyParameter,
  ClpParametersWithIV,
  ClpIParametersWithIV,
  ClpIKeyParser,
  ClpIEphemeralKeyPair,
  ClpKDFParameters,
  ClpIKdfParameters,
  ClpIIESWithCipherParameters,
  ClpConverters,
  ClpArrayUtils,
  ClpBigInteger,
  ClpBigIntegers,
  ClpCryptoLibTypes;

resourcestring
  SErrorRecoveringEphemeralPublicKey =
    'Unable to Recover Ephemeral Public Key: "%s"';
  SInvalidCipherTextLength =
    'Length of Input Must be Greater than the MAC and V Combined';
  SInvalidMAC = 'Invalid MAC';

type

  /// <summary>
  /// Support class for constructing integrated encryption ciphers for doing
  /// basic message exchanges on top of key agreement ciphers. <br />Follows
  /// the description given in IEEE Std 1363a.
  /// </summary>
  TIESEngine = class(TInterfacedObject, IIESEngine)

  strict private

    // as described in Shroup's paper( ch 10, pg 20) and P1363a
    function GetLengthTag(const p2: TCryptoLibByteArray)
      : TCryptoLibByteArray; inline;

    procedure ExtractParams(const params: ICipherParameters); inline;

  strict protected

  var
    Fagree: IBasicAgreement;
    Fkdf: IDerivationFunction;
    Fmac: IMac;
    Fcipher: IBufferedBlockCipher;
    FmacBuf, FV, FIV: TCryptoLibByteArray;
    FforEncryption: Boolean;
    FprivParam, FpubParam: ICipherParameters;
    Fparam: IIESParameters;
    FkeyPairGenerator: IEphemeralKeyPairGenerator;
    FkeyParser: IKeyParser;

    function GetCipher: IBufferedBlockCipher; inline;
    function GetMac: IMac; inline;
    function EncryptBlock(const &in: TCryptoLibByteArray; inOff, inLen: Int32)
      : TCryptoLibByteArray; virtual;

    function DecryptBlock(const in_enc: TCryptoLibByteArray;
      inOff, inLen: Int32): TCryptoLibByteArray; virtual;

  public

    /// <summary>
    /// Set up for use with stream mode, where the key derivation function is
    /// used to provide a stream of bytes to xor with the message.
    /// </summary>
    /// <param name="agree">
    /// the key agreement used as the basis for the encryption
    /// </param>
    /// <param name="kdf">
    /// the key derivation function used for byte generation
    /// </param>
    /// <param name="mac">
    /// the message authentication code generator for the message
    /// </param>
    constructor Create(const agree: IBasicAgreement;
      const kdf: IDerivationFunction; const mac: IMac); overload;

    /// <summary>
    /// Set up for use in conjunction with a block cipher to handle the <br />
    /// message. It is <b>strongly</b> recommended that the cipher is not
    /// in ECB mode.
    /// </summary>
    /// <param name="agree">
    /// the key agreement used as the basis for the encryption
    /// </param>
    /// <param name="kdf">
    /// the key derivation function used for byte generation
    /// </param>
    /// <param name="mac">
    /// the message authentication code generator for the message
    /// </param>
    /// <param name="cipher">
    /// the cipher to used for encrypting the message
    /// </param>
    constructor Create(const agree: IBasicAgreement;
      const kdf: IDerivationFunction; const mac: IMac;
      const cipher: IBufferedBlockCipher); overload;

    /// <summary>
    /// Initialise the encryptor/decryptor.
    /// </summary>
    /// <param name="forEncryption">
    /// whether or not this is encryption/decryption.
    /// </param>
    /// <param name="privParam">
    /// our private key parameters
    /// </param>
    /// <param name="pubParam">
    /// the recipient's/sender's public key parameters
    /// </param>
    /// <param name="params">
    /// encoding and derivation parameters, may be wrapped to include an IV
    /// for an underlying block cipher.
    /// </param>
    procedure Init(forEncryption: Boolean; const privParam, pubParam,
      params: ICipherParameters); overload;

    /// <summary>
    /// Initialise the encryptor.
    /// </summary>
    /// <param name="publicKey">
    /// the recipient's/sender's public key parameters
    /// </param>
    /// <param name="params">
    /// encoding and derivation parameters, may be wrapped to include an IV
    /// for an underlying block cipher.
    /// </param>
    /// <param name="ephemeralKeyPairGenerator">
    /// the ephemeral key pair generator to use.
    /// </param>
    procedure Init(const publicKey: IAsymmetricKeyParameter;
      const params: ICipherParameters;
      const ephemeralKeyPairGenerator: IEphemeralKeyPairGenerator); overload;

    /// <summary>
    /// Initialise the decryptor.
    /// </summary>
    /// <param name="privateKey">
    /// the recipient's private key.
    /// </param>
    /// <param name="params">
    /// encoding and derivation parameters, may be wrapped to include an IV
    /// for an underlying block cipher.
    /// </param>
    /// <param name="publicKeyParser">
    /// the parser for reading the ephemeral public key.
    /// </param>
    procedure Init(const privateKey: IAsymmetricKeyParameter;
      const params: ICipherParameters;
      const publicKeyParser: IKeyParser); overload;

    function ProcessBlock(const &in: TCryptoLibByteArray; inOff, inLen: Int32)
      : TCryptoLibByteArray; virtual;

    property cipher: IBufferedBlockCipher read GetCipher;
    property mac: IMac read GetMac;

  end;

implementation

{ TIESEngine }

function TIESEngine.GetLengthTag(const p2: TCryptoLibByteArray)
  : TCryptoLibByteArray;
begin
  System.SetLength(Result, 8);
  if (p2 <> Nil) then
  begin
    TConverters.ReadUInt64AsBytesBE(System.Length(p2) * 8, Result, 0);
  end;
end;

constructor TIESEngine.Create(const agree: IBasicAgreement;
  const kdf: IDerivationFunction; const mac: IMac);
begin
  Inherited Create();
  Fagree := agree;
  Fkdf := kdf;
  Fmac := mac;
  System.SetLength(FmacBuf, mac.GetMacSize);
  Fcipher := Nil;
end;

constructor TIESEngine.Create(const agree: IBasicAgreement;
  const kdf: IDerivationFunction; const mac: IMac;
  const cipher: IBufferedBlockCipher);
begin
  Inherited Create();
  Fagree := agree;
  Fkdf := kdf;
  Fmac := mac;
  System.SetLength(FmacBuf, mac.GetMacSize);
  Fcipher := cipher;
end;

function TIESEngine.DecryptBlock(const in_enc: TCryptoLibByteArray;
  inOff, inLen: Int32): TCryptoLibByteArray;
var
  M, K, K1, K2, p2, L2, T1, T2: TCryptoLibByteArray;
  len, i, endPoint: Int32;
  cp: ICipherParameters;
begin
  len := 0;
  // Ensure that the length of the input is greater than the MAC in bytes
  if (inLen < (System.Length(FV) + Fmac.GetMacSize)) then
  begin
    raise EInvalidCipherTextCryptoLibException.CreateRes
      (@SInvalidCipherTextLength);
  end;
  // note order is important: set up keys, do simple encryptions, check mac, do final encryption.
  if (Fcipher = Nil) then
  begin

    // Streaming mode.
    System.SetLength(K1, inLen - System.Length(FV) - Fmac.GetMacSize);
    System.SetLength(K2, Fparam.MacKeySize div 8);
    System.SetLength(K, System.Length(K1) + System.Length(K2));

    Fkdf.GenerateBytes(K, 0, System.Length(K));

    if (System.Length(FV) <> 0) then
    begin
      System.Move(K[0], K2[0], System.Length(K2) * System.SizeOf(Byte));
      if K1 <> Nil then
      begin
        System.Move(K[System.Length(K2)], K1[0],
          System.Length(K1) * System.SizeOf(Byte));
      end;
    end
    else
    begin
      if K1 <> Nil then
      begin
        System.Move(K[0], K1[0], System.Length(K1) * System.SizeOf(Byte));
      end;
      System.Move(K[System.Length(K1)], K2[0], System.Length(K2) *
        System.SizeOf(Byte));
    end;

    // process the message
    System.SetLength(M, System.Length(K1));

    i := 0;

    while i <> System.Length(K1) do
    begin
      M[i] := Byte(in_enc[inOff + System.Length(FV) + i] xor K1[i]);
      System.Inc(i);
    end;

  end
  else
  begin
    // Block cipher mode.

    System.SetLength(K1, (Fparam as IIESWithCipherParameters)
      .CipherKeySize div 8);
    System.SetLength(K2, Fparam.MacKeySize div 8);
    System.SetLength(K, System.Length(K1) + System.Length(K2));

    Fkdf.GenerateBytes(K, 0, System.Length(K));

    System.Move(K[0], K1[0], System.Length(K1) * System.SizeOf(Byte));
    System.Move(K[System.Length(K1)], K2[0], System.Length(K2) *
      System.SizeOf(Byte));

    cp := TKeyParameter.Create(K1);

    // If iv is provided use it to initialise the cipher
    if (FIV <> Nil) then
    begin
      cp := TParametersWithIV.Create(cp, FIV);
    end;

    Fcipher.Init(False, cp);

    System.SetLength(M, Fcipher.GetOutputSize(inLen - System.Length(FV) -
      Fmac.GetMacSize));

    // do initial processing
    len := Fcipher.ProcessBytes(in_enc, inOff + System.Length(FV),
      inLen - System.Length(FV) - Fmac.GetMacSize, M, 0);

  end;

  // Convert the length of the encoding vector into a byte array.
  p2 := Fparam.GetEncodingV();
  L2 := Nil;
  if (System.Length(FV) <> 0) then
  begin
    L2 := GetLengthTag(p2);
  end;

  // Verify the MAC.
  endPoint := inOff + inLen;
  T1 := TArrayUtils.CopyOfRange(in_enc, endPoint - Fmac.GetMacSize, endPoint);
  System.SetLength(T2, System.Length(T1));

  Fmac.Init((TKeyParameter.Create(K2) as IKeyParameter) as ICipherParameters);

  Fmac.BlockUpdate(in_enc, inOff + System.Length(FV), inLen - System.Length(FV)
    - System.Length(T2));
  if (p2 <> Nil) then
  begin
    Fmac.BlockUpdate(p2, 0, System.Length(p2));
  end;
  if (System.Length(FV) <> 0) then
  begin
    Fmac.BlockUpdate(L2, 0, System.Length(L2));
  end;
  T2 := Fmac.DoFinal();

  if (not TArrayUtils.ConstantTimeAreEqual(T1, T2)) then
  begin
    raise EInvalidCipherTextCryptoLibException.CreateRes(@SInvalidMAC);
  end;

  if (Fcipher = Nil) then
  begin
    Result := M;
    Exit;
  end
  else
  begin
    len := len + Fcipher.DoFinal(M, len);

    Result := TArrayUtils.CopyOfRange(M, 0, len);
    Exit;
  end;
end;

function TIESEngine.EncryptBlock(const &in: TCryptoLibByteArray;
  inOff, inLen: Int32): TCryptoLibByteArray;
var
  C, K, K1, K2, p2, L2, T: TCryptoLibByteArray;
  len, i, lenCount: Int32;
begin
  if (Fcipher = Nil) then
  begin
    // Streaming mode.
    System.SetLength(K1, inLen);
    System.SetLength(K2, Fparam.MacKeySize div 8);
    System.SetLength(K, System.Length(K1) + System.Length(K2));

    Fkdf.GenerateBytes(K, 0, System.Length(K));

    if (System.Length(FV) <> 0) then
    begin
      System.Move(K[0], K2[0], System.Length(K2) * System.SizeOf(Byte));
      if K1 <> Nil then
      begin
        System.Move(K[System.Length(K2)], K1[0],
          System.Length(K1) * System.SizeOf(Byte));
      end;
    end
    else
    begin
      if K1 <> Nil then
      begin
        System.Move(K[0], K1[0], System.Length(K1) * System.SizeOf(Byte));
      end;
      System.Move(K[inLen], K2[0], System.Length(K2) * System.SizeOf(Byte));
    end;

    System.SetLength(C, inLen);

    i := 0;

    while i <> inLen do
    begin
      C[i] := Byte(&in[inOff + i] xor K1[i]);
      System.Inc(i);
    end;

    len := inLen;
  end
  else
  begin
    // Block cipher mode.

    System.SetLength(K1, (Fparam as IIESWithCipherParameters)
      .CipherKeySize div 8);
    System.SetLength(K2, Fparam.MacKeySize div 8);
    System.SetLength(K, System.Length(K1) + System.Length(K2));

    Fkdf.GenerateBytes(K, 0, System.Length(K));

    System.Move(K[0], K1[0], System.Length(K1) * System.SizeOf(Byte));
    System.Move(K[System.Length(K1)], K2[0], System.Length(K2) *
      System.SizeOf(Byte));

    // If iv is provided use it to initialise the cipher
    if (FIV <> Nil) then
    begin
      Fcipher.Init(true, TParametersWithIV.Create(TKeyParameter.Create(K1)
        as IKeyParameter, FIV));
    end
    else
    begin
      Fcipher.Init(true, TKeyParameter.Create(K1) as IKeyParameter);
    end;

    System.SetLength(C, Fcipher.GetOutputSize(inLen));

    len := Fcipher.ProcessBytes(&in, inOff, inLen, C, 0);
    len := len + Fcipher.DoFinal(C, len);
  end;

  // Convert the length of the encoding vector into a byte array.
  p2 := Fparam.GetEncodingV();
  L2 := Nil;
  if (System.Length(FV) <> 0) then
  begin
    L2 := GetLengthTag(p2);
  end;

  // Apply the MAC.
  System.SetLength(T, Fmac.GetMacSize);

  Fmac.Init((TKeyParameter.Create(K2) as IKeyParameter) as ICipherParameters);
  Fmac.BlockUpdate(C, 0, System.Length(C));
  if (p2 <> Nil) then
  begin
    Fmac.BlockUpdate(p2, 0, System.Length(p2));
  end;
  if (System.Length(FV) <> 0) then
  begin
    Fmac.BlockUpdate(L2, 0, System.Length(L2));
  end;
  T := Fmac.DoFinal;

  // Output the triple (V,C,T).
  // V := Ephermeral Public Key
  // C := Encrypted Payload
  // T := Authentication Message (MAC)
  System.SetLength(Result, System.Length(FV) + len + System.Length(T));
  if FV <> Nil then
  begin
    System.Move(FV[0], Result[0], System.Length(FV) * System.SizeOf(Byte));
  end;
  lenCount := len * System.SizeOf(Byte);
  if lenCount > 0 then
  begin
    System.Move(C[0], Result[System.Length(FV)], lenCount);
  end;
  System.Move(T[0], Result[System.Length(FV) + len],
    System.Length(T) * System.SizeOf(Byte));

end;

procedure TIESEngine.ExtractParams(const params: ICipherParameters);
begin
  if Supports(params, IParametersWithIV) then
  begin
    FIV := (params as IParametersWithIV).GetIV;
    Fparam := ((params as IParametersWithIV).Parameters) as IIESParameters;
  end
  else
  begin
    FIV := Nil;
    Fparam := (params as IIESParameters);
  end;
end;

function TIESEngine.GetCipher: IBufferedBlockCipher;
begin
  Result := Fcipher;
end;

function TIESEngine.GetMac: IMac;
begin
  Result := Fmac;
end;

procedure TIESEngine.Init(const privateKey: IAsymmetricKeyParameter;
  const params: ICipherParameters; const publicKeyParser: IKeyParser);
begin
  FforEncryption := False;
  FprivParam := privateKey;
  FkeyParser := publicKeyParser;
  ExtractParams(params);
end;

procedure TIESEngine.Init(const publicKey: IAsymmetricKeyParameter;
  const params: ICipherParameters;
  const ephemeralKeyPairGenerator: IEphemeralKeyPairGenerator);
begin
  FforEncryption := true;
  FpubParam := publicKey;
  FkeyPairGenerator := ephemeralKeyPairGenerator;
  ExtractParams(params);
end;

procedure TIESEngine.Init(forEncryption: Boolean;
  const privParam, pubParam, params: ICipherParameters);
begin
  FforEncryption := forEncryption;
  FprivParam := privParam;
  FpubParam := pubParam;
  System.SetLength(FV, 0);
  ExtractParams(params);
end;

function TIESEngine.ProcessBlock(const &in: TCryptoLibByteArray;
  inOff, inLen: Int32): TCryptoLibByteArray;
var
  ephKeyPair: IEphemeralKeyPair;
  bIn: TBytesStream;
  encLength: Int32;
  z: TBigInteger;
  BigZ, VZ: TCryptoLibByteArray;
  kdfParam: IKDFParameters;
begin
  if (FforEncryption) then
  begin
    if (FkeyPairGenerator <> Nil) then
    begin
      ephKeyPair := FkeyPairGenerator.Generate;

      FprivParam := ephKeyPair.GetKeyPair.Private;
      FV := ephKeyPair.GetEncodedPublicKey;
    end
  end
  else
  begin
    if (FkeyParser <> Nil) then
    begin
      // used TBytesStream here for one pass creation and population with byte array :)
      bIn := TBytesStream.Create(System.Copy(&in, inOff, inLen));

      try
        bIn.Position := 0;

        try
          FpubParam := FkeyParser.ReadKey(bIn);
        except
          on e: EIOCryptoLibException do
          begin
            raise EInvalidCipherTextCryptoLibException.CreateResFmt
              (@SErrorRecoveringEphemeralPublicKey, [e.Message]);
          end;

          on e: EArgumentCryptoLibException do
          begin
            raise EInvalidCipherTextCryptoLibException.CreateResFmt
              (@SErrorRecoveringEphemeralPublicKey, [e.Message]);
          end;

        end;

        encLength := (inLen - (bIn.Size - bIn.Position));
        FV := TArrayUtils.CopyOfRange(&in, inOff, inOff + encLength);

      finally
        bIn.Free;
      end;
    end;
  end;

  // Compute the common value and convert to byte array.
  Fagree.Init(FprivParam);
  z := Fagree.CalculateAgreement(FpubParam);
  BigZ := TBigIntegers.AsUnsignedByteArray(Fagree.GetFieldSize, z);

  // Create input to KDF.
  if (System.Length(FV) <> 0) then
  begin
    VZ := TArrayUtils.Concatenate(FV, BigZ);
    TArrayUtils.Fill(BigZ, 0, System.Length(BigZ), Byte(0));
    BigZ := VZ;
  end;

  try
    // Initialise the KDF.
    kdfParam := TKDFParameters.Create(BigZ, Fparam.GetDerivationV);
    Fkdf.Init(kdfParam);

    if FforEncryption then
    begin
      Result := EncryptBlock(&in, inOff, inLen);
      Exit;
    end
    else
    begin
      Result := DecryptBlock(&in, inOff, inLen);
      Exit;
    end;

  finally
    TArrayUtils.Fill(BigZ, 0, System.Length(BigZ), Byte(0));
  end;
end;

end.
