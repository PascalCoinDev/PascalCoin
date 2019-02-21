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

unit ClpPascalCoinIESEngine;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  Classes,
  ClpIMac,
  ClpIPascalCoinIESEngine,
  ClpICipherParameters,
  ClpKeyParameter,
  ClpIKeyParameter,
  ClpParametersWithIV,
  ClpIKeyParser,
  ClpIEphemeralKeyPair,
  ClpKdfParameters,
  ClpIKdfParameters,
  ClpIIESWithCipherParameters,
  ClpIESEngine,
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
  SCipherCannotbeNilInThisMode = 'Cipher Cannot be Nil in This Mode.';

type

  /// <summary>
  /// Compatibility Class for PascalCoin IESEngine
  /// </summary>
  TPascalCoinIESEngine = class(TIESEngine, IPascalCoinIESEngine)

  strict private
  type
    /// <summary>
    /// Structure for Compatibility with PascalCoin Original
    /// Implementation.
    /// </summary>
    TSecureHead = record
      Key: Byte;
      Mac: Byte;
      Orig: UInt16;
      Body: UInt16;
    end;

  const
    /// <summary>
    /// <b>SizeOf <paramref name="TSecureHead" /></b>. <br />
    /// </summary>
    // SECURE_HEAD_SIZE = Int32(6);
    SECURE_HEAD_SIZE = System.SizeOf(TSecureHead);

  strict protected

    function EncryptBlock(const &in: TCryptoLibByteArray; inOff, inLen: Int32)
      : TCryptoLibByteArray; override;

    function DecryptBlock(const in_enc: TCryptoLibByteArray;
      inOff, inLen: Int32): TCryptoLibByteArray; override;

  public

    function ProcessBlock(const &in: TCryptoLibByteArray; inOff, inLen: Int32)
      : TCryptoLibByteArray; override;

  end;

implementation

{ TPascalCoinIESEngine }

function TPascalCoinIESEngine.DecryptBlock(const in_enc: TCryptoLibByteArray;
  inOff, inLen: Int32): TCryptoLibByteArray;
var
  K, K1, K2, T1, T2: TCryptoLibByteArray;
  cp: ICipherParameters;
begin
  // Ensure that the length of the input is greater than the MAC in bytes
  if (inLen < (System.Length(FV) + Fmac.GetMacSize)) then
  begin
    raise EInvalidCipherTextCryptoLibException.CreateRes
      (@SInvalidCipherTextLength);
  end;
  // note order is important: set up keys, do simple encryptions, check mac, do final encryption.
  if (Fcipher = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes
      (@SCipherCannotbeNilInThisMode);
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

  end;

  // Verify the MAC.
  T1 := System.Copy(in_enc, System.Length(FV), Fmac.GetMacSize);
  System.SetLength(T2, System.Length(T1));

  Fmac.Init((TKeyParameter.Create(K2) as IKeyParameter) as ICipherParameters);

  Fmac.BlockUpdate(in_enc, inOff + System.Length(FV) + System.Length(T2),
    inLen - System.Length(FV) - System.Length(T2));

  T2 := Fmac.DoFinal();

  if (not TArrayUtils.ConstantTimeAreEqual(T1, T2)) then
  begin
    raise EInvalidCipherTextCryptoLibException.CreateRes(@SInvalidMAC);
  end;

  Result := Fcipher.DoFinal(in_enc, inOff + System.Length(FV) + Fmac.GetMacSize,
    inLen - System.Length(FV) - System.Length(T2));
  Exit;
end;

function TPascalCoinIESEngine.EncryptBlock(const &in: TCryptoLibByteArray;
  inOff, inLen: Int32): TCryptoLibByteArray;
var
  C, K, K1, K2, T: TCryptoLibByteArray;
  MessageToEncryptPadSize, CipherBlockSize, MessageToEncryptSize: Int32;
begin
  if (Fcipher = Nil) then
  begin
    raise EArgumentNilCryptoLibException.CreateRes
      (@SCipherCannotbeNilInThisMode);
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

    C := Fcipher.DoFinal(&in, inOff, inLen);
  end;

  // Apply the MAC.
  System.SetLength(T, Fmac.GetMacSize);

  Fmac.Init((TKeyParameter.Create(K2) as IKeyParameter) as ICipherParameters);

  Fmac.BlockUpdate(C, 0, System.Length(C));

  T := Fmac.DoFinal();
  CipherBlockSize := Fcipher.GetBlockSize;
  MessageToEncryptSize := inLen - inOff;

  if (MessageToEncryptSize mod CipherBlockSize) = 0 then
  begin
    MessageToEncryptPadSize := 0
  end
  else
  begin
    MessageToEncryptPadSize := CipherBlockSize -
      (MessageToEncryptSize mod CipherBlockSize);
  end;
  // Output the quadruple (SECURE_HEAD_DETAILS,V,T,C).
  // SECURE_HEAD_DETAILS :=
  // [0] := Convert Byte(Length(V)) to a ByteArray,
  // [1] := Convert Byte(Length(T)) to a ByteArray,
  // [2] and [3] := Convert UInt16(MessageToEncryptSize) to a ByteArray,
  // [4] and [5] := Convert UInt16(MessageToEncryptSize + MessageToEncryptPadSize) to a ByteArray,
  // V := Ephermeral Public Key
  // T := Authentication Message (MAC)
  // C := Encrypted Payload

  System.SetLength(Result, SECURE_HEAD_SIZE + System.Length(FV) +
    System.Length(T) + System.Length(C));

  PByte(Result)^ := Byte(System.Length(FV));
  (PByte(Result) + 1)^ := Byte(System.Length(T));
  (PWord(Result) + 1)^ := UInt16(MessageToEncryptSize);
  (PWord(Result) + 2)^ :=
    UInt16(MessageToEncryptSize + MessageToEncryptPadSize);

  System.Move(FV[0], Result[SECURE_HEAD_SIZE], System.Length(FV) *
    System.SizeOf(Byte));

  System.Move(T[0], Result[SECURE_HEAD_SIZE + System.Length(FV)],
    System.Length(T) * System.SizeOf(Byte));

  System.Move(C[0], Result[SECURE_HEAD_SIZE + System.Length(FV) +
    System.Length(T)], System.Length(C) * System.SizeOf(Byte));

end;

function TPascalCoinIESEngine.ProcessBlock(const &in: TCryptoLibByteArray;
  inOff, inLen: Int32): TCryptoLibByteArray;
var
  ephKeyPair: IEphemeralKeyPair;
  bIn: TBytesStream;
  encLength: Int32;
  z: TBigInteger;
  BigZ: TCryptoLibByteArray;
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
        // for existing PascalCoin compatiblity purposes
        bIn.Position := SECURE_HEAD_SIZE;

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
        FV := TArrayUtils.CopyOfRange(&in, inOff + SECURE_HEAD_SIZE,
          inOff + encLength);

      finally
        bIn.Free;
      end;
    end;
  end;

  // Compute the common value and convert to byte array.
  Fagree.Init(FprivParam);
  z := Fagree.CalculateAgreement(FpubParam);
  BigZ := TBigIntegers.AsUnsignedByteArray(Fagree.GetFieldSize, z);

  try
    // Initialise the KDF.
    kdfParam := TKDFParameters.Create(BigZ, Nil);
    Fkdf.Init(kdfParam);

    if FforEncryption then
    begin
      Result := EncryptBlock(&in, inOff, inLen);
      Exit;
    end
    else
    begin
      Result := DecryptBlock(System.Copy(&in, inOff + SECURE_HEAD_SIZE,
        inLen - SECURE_HEAD_SIZE), inOff, inLen - SECURE_HEAD_SIZE);
      Exit;
    end;

  finally
    TArrayUtils.Fill(BigZ, 0, System.Length(BigZ), Byte(0));
  end;
end;

end.
