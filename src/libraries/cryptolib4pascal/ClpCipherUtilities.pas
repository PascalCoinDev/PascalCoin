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

unit ClpCipherUtilities;

{$I CryptoLib.inc}

interface

uses
  SysUtils,
  TypInfo,
  Generics.Collections,
  ClpCryptoLibTypes,
  ClpStringUtils,
  ClpPaddingModes,
  ClpIPaddingModes,
  ClpBlockCipherModes,
  ClpIBlockCipherModes,
  ClpBufferedBlockCipher,
  ClpIBufferedBlockCipher,
  ClpPaddedBufferedBlockCipher,
  ClpIPaddedBufferedBlockCipher,
  ClpNistObjectIdentifiers,
  ClpIAsn1Objects,
  ClpIBufferedCipher,
  ClpIBlockCipher,
  ClpAesEngine,
  ClpIAesEngine,
  ClpBlowfishEngine,
  ClpIBlowfishEngine,
  ClpIBlockCipherPadding;

resourcestring
  SMechanismNil = 'Mechanism Cannot be Nil';
  SAlgorithmNil = 'Algorithm Cannot be Nil';
  SUnRecognizedCipher = '"Cipher " %s Not Recognised.';
  SSICModeWarning =
    'Warning: SIC-Mode Can Become a TwoTime-Pad if the Blocksize of the Cipher is Too Small. Use a Cipher With a Block Size of at Least 128 bits (e.g. AES)';

type

  /// <remarks>
  /// Cipher Utility class contains methods that can not be specifically grouped into other classes.
  /// </remarks>
  TCipherUtilities = class sealed(TObject)

  strict private

  type
{$SCOPEDENUMS ON}
    TCipherAlgorithm = (AES, BLOWFISH);
    TCipherMode = (NONE, CBC, CFB, CTR, ECB, OFB, SIC);
    TCipherPadding = (NOPADDING, ISO10126PADDING, ISO10126D2PADDING,
      ISO10126_2PADDING, ISO7816_4PADDING, ISO9797_1PADDING, PKCS5,
      PKCS5PADDING, PKCS7, PKCS7PADDING, TBCPADDING, X923PADDING,
      ZEROBYTEPADDING);
{$SCOPEDENUMS OFF}

  class var

    Falgorithms: TDictionary<String, String>;
    Foids: TDictionary<String, IDerObjectIdentifier>;

    class function GetAlgorithms: TCryptoLibStringArray; static; inline;
    class function GetDigitIndex(const s: String): Int32; static; inline;

    class procedure Boot(); static;
    class constructor CreateCipherUtilities();
    class destructor DestroyCipherUtilities();

  public
    /// <summary>
    /// Returns a ObjectIdentifier for a give encoding.
    /// </summary>
    /// <param name="mechanism">A string representation of the encoding.</param>
    /// <returns>A DerObjectIdentifier, null if the Oid is not available.</returns>
    // TODO Don't really want to support this
    class function GetObjectIdentifier(mechanism: String)
      : IDerObjectIdentifier; static;
    class function GetCipher(algorithm: String): IBufferedCipher;
      overload; static;
    class function GetCipher(const oid: IDerObjectIdentifier): IBufferedCipher;
      overload; static; inline;

    class property Algorithms: TCryptoLibStringArray read GetAlgorithms;
  end;

implementation

{ TCipherUtilities }

class procedure TCipherUtilities.Boot;
begin
  Falgorithms := TDictionary<string, string>.Create();
  Foids := TDictionary<string, IDerObjectIdentifier>.Create();

  TNistObjectIdentifiers.Boot;

  // TODO Flesh out the list of aliases

  Falgorithms.Add(TNistObjectIdentifiers.IdAes128Ecb.Id,
    'AES/ECB/PKCS7PADDING');
  Falgorithms.Add(TNistObjectIdentifiers.IdAes192Ecb.Id,
    'AES/ECB/PKCS7PADDING');
  Falgorithms.Add(TNistObjectIdentifiers.IdAes256Ecb.Id,
    'AES/ECB/PKCS7PADDING');
  Falgorithms.Add('AES//PKCS7', 'AES/ECB/PKCS7PADDING');
  Falgorithms.Add('AES//PKCS7PADDING', 'AES/ECB/PKCS7PADDING');
  Falgorithms.Add('AES//PKCS5', 'AES/ECB/PKCS7PADDING');
  Falgorithms.Add('AES//PKCS5PADDING', 'AES/ECB/PKCS7PADDING');

  Falgorithms.Add(TNistObjectIdentifiers.IdAes128Cbc.Id,
    'AES/CBC/PKCS7PADDING');
  Falgorithms.Add(TNistObjectIdentifiers.IdAes192Cbc.Id,
    'AES/CBC/PKCS7PADDING');
  Falgorithms.Add(TNistObjectIdentifiers.IdAes256Cbc.Id,
    'AES/CBC/PKCS7PADDING');

  Falgorithms.Add(TNistObjectIdentifiers.IdAes128Ofb.Id, 'AES/OFB/NOPADDING');
  Falgorithms.Add(TNistObjectIdentifiers.IdAes192Ofb.Id, 'AES/OFB/NOPADDING');
  Falgorithms.Add(TNistObjectIdentifiers.IdAes256Ofb.Id, 'AES/OFB/NOPADDING');

  Falgorithms.Add(TNistObjectIdentifiers.IdAes128Cfb.Id, 'AES/CFB/NOPADDING');
  Falgorithms.Add(TNistObjectIdentifiers.IdAes192Cfb.Id, 'AES/CFB/NOPADDING');
  Falgorithms.Add(TNistObjectIdentifiers.IdAes256Cfb.Id, 'AES/CFB/NOPADDING');

  Falgorithms.Add('1.3.6.1.4.1.3029.1.2', 'BLOWFISH/CBC');

end;

class constructor TCipherUtilities.CreateCipherUtilities;
begin
  TCipherUtilities.Boot;
end;

class destructor TCipherUtilities.DestroyCipherUtilities;
begin
  Falgorithms.Free;
  Foids.Free;
end;

class function TCipherUtilities.GetAlgorithms: TCryptoLibStringArray;
begin
  Result := Foids.Keys.ToArray;
end;

class function TCipherUtilities.GetDigitIndex(const s: String): Int32;
var
  i, LowPoint, HighPoint: Int32;
begin
{$IFDEF DELPHIXE3_UP}
  LowPoint := System.Low(s);
  HighPoint := System.High(s);
{$ELSE}
  LowPoint := 1;
  HighPoint := System.Length(s);
{$ENDIF DELPHIXE3_UP}
  For i := LowPoint to HighPoint do
  begin
    if (CharInSet(s[i], ['0' .. '9'])) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

class function TCipherUtilities.GetCipher(algorithm: String): IBufferedCipher;
var
  aliased, algorithmName, temp, paddingName, mode, modeName: string;
  di, LowPoint, bits, HighPoint: Int32;
  padded: Boolean;
  parts: TCryptoLibStringArray;
  cipherAlgorithm: TCipherAlgorithm;
  cipherPadding: TCipherPadding;
  cipherMode: TCipherMode;
  blockCipher: IBlockCipher;
  padding: IBlockCipherPadding;
begin
  if (algorithm = '') then
  begin
    raise EArgumentNilCryptoLibException.CreateRes(@SAlgorithmNil);
  end;
  algorithm := UpperCase(algorithm);

  if (Falgorithms.TryGetValue(algorithm, aliased)) then
  begin
    algorithm := aliased;
  end;

  parts := TStringUtils.SplitString(algorithm, '/');

  blockCipher := Nil;

  algorithmName := parts[0];

  if (Falgorithms.TryGetValue(algorithmName, aliased)) then
  begin
    algorithmName := aliased;
  end;

  temp := StringReplace(algorithmName, '-', '_', [rfReplaceAll, rfIgnoreCase]);

  temp := StringReplace(temp, '/', '_', [rfReplaceAll, rfIgnoreCase]);

  cipherAlgorithm := TCipherAlgorithm
    (GetEnumValue(TypeInfo(TCipherAlgorithm), temp));

  case cipherAlgorithm of
    TCipherAlgorithm.AES:
      begin
        blockCipher := TAesEngine.Create() as IAesEngine;
      end;
    TCipherAlgorithm.BLOWFISH:
      begin
        blockCipher := TBlowfishEngine.Create() as IBlowfishEngine;
      end
  else
    begin
      raise ESecurityUtilityCryptoLibException.CreateResFmt
        (@SUnRecognizedCipher, [algorithm]);
    end;
  end;

  padded := true;
  padding := Nil;

  if System.Length(parts) > 2 then
  begin
    paddingName := parts[2];

    temp := StringReplace(paddingName, '-', '_', [rfReplaceAll, rfIgnoreCase]);

    temp := StringReplace(temp, '/', '_', [rfReplaceAll, rfIgnoreCase]);

    cipherPadding := TCipherPadding
      (GetEnumValue(TypeInfo(TCipherPadding), temp));

    case cipherPadding of
      TCipherPadding.NOPADDING:
        begin
          padded := false;
        end;

      TCipherPadding.ISO10126PADDING, TCipherPadding.ISO10126D2PADDING,
        TCipherPadding.ISO10126_2PADDING:
        begin
          padding := TISO10126d2Padding.Create() as IISO10126d2Padding;
        end;

      TCipherPadding.ISO7816_4PADDING, TCipherPadding.ISO9797_1PADDING:
        begin
          padding := TISO7816d4Padding.Create() as IISO7816d4Padding;
        end;

      TCipherPadding.PKCS5, TCipherPadding.PKCS5PADDING, TCipherPadding.PKCS7,
        TCipherPadding.PKCS7PADDING:
        begin
          padding := TPkcs7Padding.Create() as IPkcs7Padding;
        end;

      TCipherPadding.TBCPADDING:
        begin
          padding := TTBCPadding.Create() as ITBCPadding;
        end;

      TCipherPadding.X923PADDING:
        begin
          padding := TX923Padding.Create() as IX923Padding;
        end;

      TCipherPadding.ZEROBYTEPADDING:
        begin
          padding := TZeroBytePadding.Create() as IZeroBytePadding;
        end

    else
      begin
        raise ESecurityUtilityCryptoLibException.CreateResFmt
          (@SUnRecognizedCipher, [algorithm]);
      end;
    end;

  end;

  mode := '';
  if (System.Length(parts) > 1) then
  begin
    mode := parts[1];

    di := GetDigitIndex(mode);
    if di >= 0 then
    begin
{$IFDEF DELPHIXE3_UP}
      LowPoint := System.Low(mode);
{$ELSE}
      LowPoint := 1;
{$ENDIF DELPHIXE3_UP}
      modeName := System.Copy(mode, LowPoint, di);
    end
    else
    begin
      modeName := mode;
    end;

    if modeName = '' then
    begin
      cipherMode := TCipherMode.NONE;
    end
    else
    begin
      temp := StringReplace(modeName, '-', '_', [rfReplaceAll, rfIgnoreCase]);

      temp := StringReplace(temp, '/', '_', [rfReplaceAll, rfIgnoreCase]);

      cipherMode := TCipherMode(GetEnumValue(TypeInfo(TCipherMode), temp));
    end;

    case cipherMode of
      TCipherMode.ECB, TCipherMode.NONE:
        begin
          // do nothing
        end;

      TCipherMode.CBC:
        begin
          blockCipher := TCbcBlockCipher.Create(blockCipher) as ICbcBlockCipher;
        end;

      TCipherMode.CFB:
        begin
          if (di < 0) then
          begin
            bits := 8 * blockCipher.GetBlockSize();
          end
          else
          begin
{$IFDEF DELPHIXE3_UP}
            HighPoint := System.High(mode);
{$ELSE}
            HighPoint := System.Length(mode);
{$ENDIF DELPHIXE3_UP}
            bits := StrToInt(System.Copy(mode, di, HighPoint - di));
          end;

          blockCipher := TCfbBlockCipher.Create(blockCipher, bits)
            as ICfbBlockCipher;
        end;

      TCipherMode.CTR:
        begin
          blockCipher := TSicBlockCipher.Create(blockCipher) as ISicBlockCipher;
        end;

      TCipherMode.OFB:
        begin
          if (di < 0) then
          begin
            bits := 8 * blockCipher.GetBlockSize();
          end
          else
          begin
{$IFDEF DELPHIXE3_UP}
            HighPoint := System.High(mode);
{$ELSE}
            HighPoint := System.Length(mode);
{$ENDIF DELPHIXE3_UP}
            bits := StrToInt(System.Copy(mode, di, HighPoint - di));
          end;

          blockCipher := TOfbBlockCipher.Create(blockCipher, bits)
            as IOfbBlockCipher;
        end;

      TCipherMode.SIC:
        begin
          if (blockCipher.GetBlockSize() < 16) then
          begin
            raise EArgumentCryptoLibException.CreateRes(@SSICModeWarning);
          end;
          blockCipher := TSicBlockCipher.Create(blockCipher) as ISicBlockCipher;
        end

    else
      begin
        raise ESecurityUtilityCryptoLibException.CreateResFmt
          (@SUnRecognizedCipher, [algorithm]);
      end;
    end;
  end;

  if (blockCipher <> Nil) then
  begin

    if (padding <> Nil) then
    begin
      Result := TPaddedBufferedBlockCipher.Create(blockCipher, padding)
        as IPaddedBufferedBlockCipher;
      Exit;
    end;

    if ((not padded) or (blockCipher.IsPartialBlockOkay)) then
    begin
      Result := TBufferedBlockCipher.Create(blockCipher)
        as IBufferedBlockCipher;
      Exit;
    end;

    Result := TPaddedBufferedBlockCipher.Create(blockCipher)
      as IPaddedBufferedBlockCipher;
    Exit;
  end;

  raise ESecurityUtilityCryptoLibException.CreateResFmt(@SUnRecognizedCipher,
    [algorithm]);
end;

class function TCipherUtilities.GetCipher(const oid: IDerObjectIdentifier)
  : IBufferedCipher;
begin
  Result := GetCipher(oid.Id);
end;

class function TCipherUtilities.GetObjectIdentifier(mechanism: String)
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

  Foids.TryGetValue(mechanism, Result);

end;

end.
