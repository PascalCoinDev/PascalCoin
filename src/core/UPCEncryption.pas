unit UPCEncryption;

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
  This unit is used to call Encryption/Decryption routines used by PascalCoin
  - PascalCoin ECDSA encryption
  - PascalCoin AES encryption

  It will use OpenSSL library or native CryptoLib4Pascal based on config.inc file
}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I config.inc}

{$IF (not Defined(Use_OpenSSL)) and (not Defined(Use_CryptoLib4Pascal))}
  {$Message Fatal 'ERROR: Use_OpenSSL or Use_CryptoLib4Pascal are not defined, you need to at least define one!'}
  ERROR: Use_OpenSSL or Use_CryptoLib4Pascal are not defined, you need to at least define one!
{$ENDIF}

Uses SysUtils, UBaseTypes,
  {$IFDEF Use_OpenSSL}
  UAES, UECIES,
  {$ENDIF}
  {$IFDEF Use_CryptoLib4Pascal}
  UPCCryptoLib4Pascal,
  {$ENDIF}
  UPCDataTypes;

type
  TPCEncryption = Class
  public
    class function DoPascalCoinECIESEncrypt(const APublicKey : TECDSA_Public; const AMessage : TBytes; var AEncryptedMessage : TBytes) : Boolean;
    class function DoPascalCoinECIESDecrypt(const APrivateKeyInfo : TECPrivateKeyInfo; const AEncryptedMessage : TBytes; var ADecryptedMessage : TBytes) : Boolean;
    class function DoPascalCoinAESEncrypt(const AMessage, APassword: TBytes): TBytes; static;
    class function DoPascalCoinAESDecrypt(const AEncryptedMessage, APassword: TBytes; out ADecryptedMessage: TBytes): boolean; static;
  End;

implementation


{ TPCEncryption }

class function TPCEncryption.DoPascalCoinAESDecrypt(const AEncryptedMessage, APassword: TBytes; out ADecryptedMessage: TBytes): boolean;
begin
  {$IFDEF Use_OpenSSL}
  Result := TAESComp.EVP_Decrypt_AES256(AEncryptedMessage,APassword,ADecryptedMessage);
  {$ELSE}
  Result := TPCCryptoLib4Pascal.DoPascalCoinAESDecrypt(AEncryptedMessage,APassword,ADecryptedMessage);
  {$ENDIF}
end;

class function TPCEncryption.DoPascalCoinAESEncrypt(const AMessage, APassword: TBytes): TBytes;
begin
  {$IFDEF Use_OpenSSL}
  Result := TAESComp.EVP_Encrypt_AES256(AMessage,APassword);
  {$ELSE}
  Result := TPCCryptoLib4Pascal.DoPascalCoinAESEncrypt(AMessage,APassword);
  {$ENDIF}
end;

class function TPCEncryption.DoPascalCoinECIESDecrypt(const APrivateKeyInfo : TECPrivateKeyInfo; const AEncryptedMessage: TBytes; var ADecryptedMessage: TBytes): Boolean;
begin
  {$IFDEF Use_OpenSSL}
  Result := ECIESDecrypt(APrivateKeyInfo.EC_OpenSSL_NID,APrivateKeyInfo.RAW_PrivKey,False,AEncryptedMessage,ADecryptedMessage);
  {$ELSE}
  Result := TPCCryptoLib4Pascal.DoPascalCoinECIESDecrypt(APrivateKeyInfo.EC_OpenSSL_NID,APrivateKeyInfo.RAW_PrivKey,AEncryptedMessage,ADecryptedMessage);
  {$ENDIF}
end;

class function TPCEncryption.DoPascalCoinECIESEncrypt(const APublicKey: TECDSA_Public; const AMessage: TBytes; var AEncryptedMessage: TBytes): Boolean;
begin
  {$IFDEF Use_OpenSSL}
  AEncryptedMessage := ECIESEncrypt(APublicKey,AMessage);
  Result := True;
  {$ELSE}
  Result := TPCCryptoLib4Pascal.DoPascalCoinECIESEncrypt(APublicKey,AMessage,AEncryptedMessage);
  {$ENDIF}
end;

end.
