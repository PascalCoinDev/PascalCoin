unit UAES;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{ This unit is used to encrypt/decrypt using AES256
  Basic source code found at internet:
  http://stackoverflow.com/questions/9723963/delphi-pascal-example-for-calling-openssl-evp-functions

  Original source code probably copyright of Marco Ferrante (ferrante@disi.unige.it) and "shunty" user:
  http://stackoverflow.com/users/197962/shunty
}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I config.inc}

{$IF not Defined(Use_OpenSSL)}
  {$Message Warn 'ERROR: Use_OpenSSL is not defined, you should not use this UNIT!'}
{$ENDIF}

interface

uses
  SysUtils, UCrypto, UBaseTypes;

Type
  TAESComp = Class
  private
  public
    Class function EVP_Encrypt_AES256(const TheMessage, APassword: TRawBytes): TRawBytes; overload;
    Class function EVP_Encrypt_AES256(const TheMessage: TRawBytes; const APassword : String): TRawBytes; overload;
    Class function EVP_Decrypt_AES256(const EncryptedMessage, APassword: TRawBytes; var Decrypted: TRawBytes) : Boolean; overload;
    Class function EVP_Decrypt_AES256(const EncryptedMessage: TRawBytes; const APassword: String; var Decrypted : TRawBytes) : Boolean; overload;
  End;

implementation

uses UOpenSSL;

CONST SALT_MAGIC: RawByteString = 'Salted__'; SALT_MAGIC_LEN: integer = 8; SALT_SIZE = 8;

function EVP_GetSalt: TBytes;
begin
  SetLength(result, PKCS5_SALT_LEN);
  RAND_pseudo_bytes(@result[0], PKCS5_SALT_LEN);
end;

Function EVP_GetKeyIV(APassword: TRawBytes; ACipher: PEVP_CIPHER; const ASalt: TRawBytes; out Key, IV: TRawBytes) : Boolean;
var
  pctx: PEVP_MD_CTX;
  {$IFDEF OpenSSL10}
  ctx: EVP_MD_CTX;
  {$ENDIF}
  hash: PEVP_MD;
  mdbuff: TRawBytes;
  mds: integer;
  nkey, niv: integer;
begin
  Result := false;
  hash := EVP_sha256;
  mds := 0;
  SetLength(mdbuff, EVP_MAX_MD_SIZE);

  nkey := ACipher.key_len;
  niv := ACipher.iv_len;
  SetLength(Key, nkey);
  SetLength(IV, nkey);  // Max size to start then reduce it at the end

  Assert(hash.md_size >= nkey);
  Assert(hash.md_size >= niv);

  // This is pretty much the same way that EVP_BytesToKey works. But that
  // allows multiple passes through the hashing loop and also allows to
  // choose different hashing methods. We have no need for this. The
  // OpenSSL docs say it is out of date and internet sources suggest using
  // something like PKCS5_v2_PBE_keyivgen and/or PKCS5_PBKDF2_HMAC_SHA1
  // but this method is easy to port to the DEC and DCP routines and easy to
  // use in other environments. Ultimately the Key and IV rely on the password
  // and the salt and can be easily reformed.

  // This method relies on the fact that the hashing method produces a key of
  // the correct size. EVP_BytesToKey goes through muptiple hashing passes if
  // necessary to make the key big enough when using smaller hashes.
  {$IFDEF OpenSSL10}
  EVP_MD_CTX_init(@ctx);
  pctx := @ctx;
  {$ELSE}
  pctx := EVP_MD_CTX_new;
  {$ENDIF}
  try
    // Key first
    If EVP_DigestInit_ex(pctx, hash, nil)<>1 then exit;
    If EVP_DigestUpdate(pctx, @APassword[0], Length(APassword))<>1 then exit;
    if (ASalt <> nil) then begin
      if EVP_DigestUpdate(pctx, @ASalt[0], Length(ASalt))<>1 then exit;
    end;
    if (EVP_DigestFinal_ex(pctx, @Key[0], mds)<>1) then exit;

    // Derive IV next
    If EVP_DigestInit_ex(pctx, hash, nil)<>1 then exit;
    If EVP_DigestUpdate(pctx, @Key[0], mds)<>1 then exit;
    If EVP_DigestUpdate(pctx, @APassword[0], Length(APassword))<>1 then exit;
    if (ASalt <> nil) then begin
      if EVP_DigestUpdate(pctx, @ASalt[0], Length(ASalt))<>1 then exit;
    end;
    If EVP_DigestFinal_ex(pctx, @IV[0], mds)<>1 then exit;

    SetLength(IV, niv);
    Result := true;
  finally
    {$IFDEF OpenSSL10}
    EVP_MD_CTX_cleanup(pctx);
    {$ELSE}
    EVP_MD_CTX_free(pctx);
    {$ENDIF}
  end;
end;

{ TAESComp }

class function TAESComp.EVP_Decrypt_AES256(const EncryptedMessage: TRawBytes; const APassword: String; var Decrypted : TRawBytes) : Boolean;
Var bytes_password : TRawBytes;
begin
  bytes_password.FromString(APassword);
  Result := EVP_Decrypt_AES256(EncryptedMessage,bytes_password,Decrypted);
end;

class function TAESComp.EVP_Decrypt_AES256(const EncryptedMessage, APassword: TRawBytes; var Decrypted: TRawBytes) : Boolean;
var
  cipher: PEVP_CIPHER;
  pctx: PEVP_CIPHER_CTX;
  {$IFDEF OpenSSL10}
  ctx: EVP_CIPHER_CTX;
  {$ENDIF}
  salt, key, iv, buf: TBytes;
  src_start, buf_start, out_len: integer;
begin
  Result := false;
  cipher := EVP_aes_256_cbc;
  SetLength(salt, SALT_SIZE);
  // First read the magic text and the salt - if any
  if (length(EncryptedMessage)>=SALT_MAGIC_LEN) AND (TEncoding.ASCII.GetString(EncryptedMessage, 0, SALT_MAGIC_LEN) = SALT_MAGIC) then
  begin
    Move(EncryptedMessage[SALT_MAGIC_LEN], salt[0], SALT_SIZE);
    If Not EVP_GetKeyIV(APassword, cipher, salt, key, iv) then exit;
    src_start := SALT_MAGIC_LEN + SALT_SIZE;
  end
  else
  begin
    If Not EVP_GetKeyIV(APassword, cipher, nil, key, iv) then exit;
    src_start := 0;
  end;
  {$IFDEF OpenSSL10}
  EVP_CIPHER_CTX_init(@ctx);
  pctx := @ctx;
  {$ELSE}
  pctx := EVP_CIPHER_CTX_new;
  {$ENDIF}
  try
    If EVP_DecryptInit(pctx, cipher, @key[0], @iv[0])<>1 then exit;
    SetLength(buf, Length(EncryptedMessage));
    buf_start := 0;
    If EVP_DecryptUpdate(pctx, @buf[buf_start], out_len, @EncryptedMessage[src_start], Length(EncryptedMessage) - src_start)<>1 then exit;
    Inc(buf_start, out_len);
    If EVP_DecryptFinal(pctx, @buf[buf_start], out_len)<>1 then exit;
    Inc(buf_start, out_len);
    SetLength(buf, buf_start);
    Decrypted := buf;
    Result := true;
  finally
    {$IFDEF OpenSSL10}
    EVP_CIPHER_CTX_cleanup(pctx);
    {$ELSE}
    EVP_CIPHER_CTX_free(pctx);
    {$ENDIF}
  end;
end;

class function TAESComp.EVP_Encrypt_AES256(const TheMessage: TRawBytes; const APassword : String): TRawBytes;
Var bytes_password : TRawBytes;
begin
  bytes_password.FromString(APassword);
  Result := EVP_Encrypt_AES256(TheMessage,bytes_password);
end;

class function TAESComp.EVP_Encrypt_AES256(const TheMessage, APassword: TRawBytes): TRawBytes;
var
  cipher: PEVP_CIPHER;
  pctx: PEVP_CIPHER_CTX;
  {$IFDEF OpenSSL10}
  ctx: EVP_CIPHER_CTX;
  {$ENDIF}
  salt, key, iv, buf: TBytes;
  block_size: integer;
  buf_start, out_len: integer;
begin
  cipher := EVP_aes_256_cbc;
  salt := EVP_GetSalt;
  EVP_GetKeyIV(APassword, cipher, salt, key, iv);

  {$IFDEF OpenSSL10}
  EVP_CIPHER_CTX_init(@ctx);
  pctx := @ctx;
  {$ELSE}
  pctx := EVP_CIPHER_CTX_new;
  {$ENDIF}
  try
    EVP_EncryptInit(pctx, cipher, @key[0], @iv[0]);
    block_size := EVP_CIPHER_CTX_block_size(pctx);
    SetLength(buf, Length(TheMessage) + block_size + SALT_MAGIC_LEN + PKCS5_SALT_LEN);
    buf_start := 0;
    Move(PChar(SALT_MAGIC)^, buf[buf_start], SALT_MAGIC_LEN);
    Inc(buf_start, SALT_MAGIC_LEN);
    Move(salt[0], buf[buf_start], PKCS5_SALT_LEN);
    Inc(buf_start, PKCS5_SALT_LEN);
    EVP_EncryptUpdate(pctx, @buf[buf_start], out_len, @TheMessage[0], Length(TheMessage));
    Inc(buf_start, out_len);
    EVP_EncryptFinal(pctx, @buf[buf_start], out_len);
    Inc(buf_start, out_len);
    SetLength(buf, buf_start);
    result := buf;
  finally
    {$IFDEF OpenSSL10}
    EVP_CIPHER_CTX_cleanup(pctx);
    {$ELSE}
    EVP_CIPHER_CTX_free(pctx);
    {$ENDIF}
  end;
end;

end.
