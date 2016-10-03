unit UECIES;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

{ Pascal implementation of ECIES functions to encrypt/decrypt data using AES
  and EC keys.

  Original C Source code by Ladar Levison
  http://www.mail-archive.com/openssl-dev@openssl.org/msg28042.html

  Adapted to use different EC types and with a md5 hash (because no need strong verify
  due it will be included in block chain, so no change will be made)
  and a head with lower bytes because all the data is small

  This unit works with all EC types defined in OpenSLL, can encrypt using the public
  EC key and decrypt using the private EC key.

  When encrypting uses a MD5, so less space (original source code used SHA512), but I don't need
  to use strong anti-corruption in encrypted data because I will include it
  in a blockchain, so corruption is not possible. (for now ;-). Using MD5 safe space
  for me.

  }

interface

Uses ssl_ecdh,ssl_types,ssl_evp,ssl_const,ssl_ec, UCrypto, ULog, ssl_hmac, ssl_err, UConst;

Const CT_Max_Bytes_To_Encrypt = 32000;

function ECIESEncrypt(const ECDSAPubKey: TECDSA_Public; const MessageToEncrypt: AnsiString): TRawBytes; overload;
function ECIESEncrypt(EC_OpenSSL_NID : Word; PubKey: EC_POINT; const MessageToEncrypt: AnsiString): TRawBytes; overload;
function ECIESDecrypt(EC_OpenSSL_NID : Word; PrivateKey: PEC_KEY; logErrors : Boolean; const MessageToDecrypt: TRawBytes; Var Decrypted : AnsiString): Boolean;

implementation

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages, Windows,
{$ENDIF}
  ssl_sha, SysUtils, ssl_bn;

Type
  Psecure_t = Pointer;
  secure_head_t = Record
    key : byte;
    mac : byte;
    orig : word;
    body : word;
  End;
  Psecure_head_t = ^secure_head_t;

function secure_key_length(cryptex : Psecure_t): UInt64;
begin
  Result := Psecure_head_t(cryptex)^.key;
end;

function secure_mac_length(cryptex : Psecure_t): UInt64;
begin
  Result := Psecure_head_t(cryptex)^.mac;
end;

function secure_body_length(cryptex : Psecure_t): UInt64;
begin
  Result := Psecure_head_t(cryptex)^.body;
end;

function secure_orig_length(cryptex : Psecure_t): UInt64;
begin
  Result := Psecure_head_t(cryptex)^.orig;
end;

function secure_total_length(cryptex : Psecure_t): UInt64;
begin
  Result := Sizeof(secure_head_t) + Psecure_head_t(cryptex)^.key +
    Psecure_head_t(cryptex)^.mac + Psecure_head_t(cryptex)^.body;
end;

function secure_key_data(cryptex : Psecure_t): Pointer;
begin
  Result := Pointer(Integer(cryptex) + Sizeof(secure_head_t));
end;

function secure_mac_data(cryptex : Psecure_t): Pointer;
begin
  Result := Pointer(Integer(cryptex) + Sizeof(secure_head_t) + Psecure_head_t(cryptex)^.key);
end;

function secure_body_data(cryptex : Psecure_t): Pointer;
begin
  Result := Pointer(Integer(cryptex) + Sizeof(secure_head_t) + Psecure_head_t(cryptex)^.key + Psecure_head_t(cryptex)^.mac);
end;

function secure_alloc(key, mac, orig, body : UInt64) : Psecure_t;
Var psh : Psecure_head_t;
begin
  Result := AllocMem(Sizeof(secure_head_t) + key + mac + body);
  psh := Result;
  psh^.key := key;
  psh^.mac := mac;
  psh^.orig := orig;
  psh^.body := body;
end;

procedure secure_free(cryptex : Psecure_t);
begin
  FreeMemory(cryptex);
end;

function ecies_key_derivation_512(const _in: Pointer; _inlen: TC_SIZE_T; _out: Pointer; var _outlen: TC_SIZE_T): pointer; cdecl;
begin
  if ( _outlen < SHA512_DIGEST_LENGTH) then begin
     Result := Nil;
  end;
  _outlen := SHA512_DIGEST_LENGTH;
  Result := SHA512(_in,_inlen,_out);
end;

function ECIES_HASHER : PEVP_MD;
begin
  // NOTE: Original C source code uses EVP_sha512() as Hash function to check
  // corrupted data... But I need to save space, so I use EVP_md5
  Result := EVP_md5;
end;

function ECIESEncrypt(const ECDSAPubKey: TECDSA_Public; const MessageToEncrypt: AnsiString): TRawBytes;
Var BNx,BNy : PBIGNUM;
  ECG : PEC_GROUP;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
  s : String;
begin
  Result := '';
  BNx := BN_bin2bn(PAnsiChar(ECDSAPubKey.x),length(ECDSAPubKey.x),nil);
  BNy := BN_bin2bn(PAnsiChar(ECDSAPubKey.y),length(ECDSAPubKey.y),nil);
  Try

    ECG := EC_GROUP_new_by_curve_name(ECDSAPubKey.EC_OpenSSL_NID);
    if ECG=Nil then begin
      s := Format('An error occurred while trying to generate curve group {error = %s}',
         [ERR_error_string(ERR_get_error(),nil)]);
      TLog.NewLog(lterror,'ECIES',s);
      exit;
    end;
    pub_key := EC_POINT_new(ECG);
    ctx := BN_CTX_new;
    if EC_POINT_set_affine_coordinates_GFp(ECG,pub_key,BNx,BNy,ctx)=1 then begin
      Result := ECIESEncrypt(ECDSAPubKey.EC_OpenSSL_NID,pub_key^,MessageToEncrypt);
    end else begin
      s := Format('An error occurred while trying to convert public key to public point {error = %s}',
         [ERR_error_string(ERR_get_error(),nil)]);
      TLog.NewLog(lterror,'ECIES',s);
    end;
    BN_CTX_free(ctx);
    EC_POINT_free(pub_key);
    EC_GROUP_free(ECG);
  Finally
    BN_free(BNx);
    BN_free(BNy);
  End;
End;

function ECIESEncrypt(EC_OpenSSL_NID : Word; PubKey: EC_POINT; const MessageToEncrypt: AnsiString): TRawBytes;
Var PK,PEphemeral : PEC_KEY;
  i,key_length,block_length,envelope_length,body_length : Integer;
  mac_length : Cardinal;
  envelope_key : Array[1..SHA512_DIGEST_LENGTH] of byte;
  iv: Array[1..EVP_MAX_IV_LENGTH] of byte;
  block:Array[1..EVP_MAX_BLOCK_LENGTH] of byte;
  cryptex : Psecure_t;
  cipher : EVP_CIPHER_CTX;
  body,aux : Pointer;
  hmac : HMAC_CTX;
begin
  Result := '';
  if length(MessageToEncrypt)>CT_Max_Bytes_To_Encrypt then begin
    TLog.NewLog(lterror,'ECIES','Max bytes to encrypt: '+inttostr(length(MessageToEncrypt))+'>'+Inttostr(CT_Max_Bytes_To_Encrypt));
    exit;
  end;
  // Make sure we are generating enough key material for the symmetric ciphers.
  key_length := (EVP_CIPHER_key_length(EVP_aes_256_cbc));
  if (key_length*2)>SHA512_DIGEST_LENGTH then begin
    TLog.NewLog(lterror,'ECIES',Format('The key derivation method will not produce enough envelope key material for the chosen ciphers. {envelope = %i / required = %zu}',
      [SHA512_DIGEST_LENGTH DIV 8,(key_length * 2) DIV 8]));
    exit;
  end;
  // Convert the user's public key from hex into a full EC_KEY structure.
  PK := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
  PEphemeral := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
  try
    if (EC_KEY_set_public_key(PK,@PubKey)<>1) then begin
      TLog.NewLog(lterror,'ECIES','Invalid public key provided');
      exit;
    end;
    if (EC_KEY_generate_key(PEphemeral)<>1) then begin
      TLog.NewLog(lterror,'ECIES','An error occurred while trying to generate the ephemeral key');
      exit;
    end;
    // Use the intersection of the provided keys to generate the envelope data used by the ciphers below. The ecies_key_derivation_512() function uses
    // SHA 512 to ensure we have a sufficient amount of envelope key material and that the material created is sufficiently secure.
    if (ECDH_compute_key(@envelope_key,SHA512_DIGEST_LENGTH,EC_KEY_get0_public_key(PK),PEphemeral,ecies_key_derivation_512)<>SHA512_DIGEST_LENGTH) then begin
      TLog.NewLog(lterror,'ECIES',Format('An error occurred while trying to compute the envelope key {error = %s}',[ERR_error_string(ERR_get_error(), nil)]));
      exit;
    end;
    // Determine the envelope and block lengths so we can allocate a buffer for the result.
    block_length := EVP_CIPHER_block_size(EVP_aes_256_cbc);
    if (block_length=0) or (block_length>EVP_MAX_BLOCK_LENGTH) then begin
      TLog.NewLog(lterror,'ECIES',Format('Invalid block length {block = %zu}',[block_length]));
      exit;
    end;
    envelope_length := EC_POINT_point2oct(EC_KEY_get0_group(PEphemeral),EC_KEY_get0_public_key(PEphemeral),POINT_CONVERSION_COMPRESSED,nil,0,nil);
    if (envelope_length=0) then begin
      TLog.NewLog(lterror,'ECIES',Format('Invalid envelope length {envelope = %zu}',[envelope_length]));
      exit;
    end;
    // We use a conditional to pad the length if the input buffer is not evenly divisible by the block size.
    if (Length(MessageToEncrypt) MOD block_length)=0 then i := 0
    else i := block_length - (Length(MessageToEncrypt) MOD block_length);
    cryptex := secure_alloc(envelope_length,EVP_MD_size(ECIES_HASHER),Length(MessageToEncrypt), Length(MessageToEncrypt) + i);
    try
      // Store the public key portion of the ephemeral key.
      If EC_POINT_point2oct(EC_KEY_get0_group(PEphemeral),EC_KEY_get0_public_key(PEphemeral),
           POINT_CONVERSION_COMPRESSED,secure_key_data(cryptex),envelope_length,Nil)<>envelope_length then begin
        TLog.NewLog(lterror,'ECIES',Format('An error occurred while trying to record the public portion of the envelope key {error = %s}',
          [ERR_error_string(ERR_get_error(),nil)]));
        exit;
      end;
      // For now we use an empty initialization vector.
      {$IFDEF FPC}
      FillByte(iv,EVP_MAX_IV_LENGTH,0);
      {$ELSE}
      FillMemory(@iv,EVP_MAX_IV_LENGTH,0);
      {$ENDIF}
      // Setup the cipher context, the body length, and store a pointer to the body buffer location.
      EVP_CIPHER_CTX_init(@cipher);
      try
        body := secure_body_data(cryptex);
        body_length := secure_body_length(cryptex);
        // Initialize the cipher with the envelope key.
        if (EVP_EncryptInit_ex(@cipher,EVP_aes_256_cbc,nil,@envelope_key,@iv)<>1) or
          (EVP_CIPHER_CTX_set_padding(@cipher,0)<>1) or
          (EVP_EncryptUpdate(@cipher,body,body_length,@MessageToEncrypt[1],
            Length(MessageToEncrypt) - (Length(MessageToEncrypt) MOD block_length))<>1) then begin
              TLog.NewLog(lterror,'ECIES',Format('An error occurred while trying to secure the data using the chosen symmetric cipher. {error = %s}',
              [ERR_error_string(ERR_get_error(),nil)]));
              exit;
            end;
        // Check whether all of the data was encrypted. If they don't match up, we either have a partial block remaining, or an error occurred.
        if (body_length<>Length(MessageToEncrypt)) then begin
          // Make sure all that remains is a partial block, and their wasn't an error
          if (Length(MessageToEncrypt) - body_length >= block_length) then begin
            TLog.NewLog(lterror,'ECIES',Format('Unable to secure the data using the chosen symmetric cipher. {error = %s}',
            [ERR_error_string(ERR_get_error(),nil)]));
            exit;
          end;
          // Copy the remaining data into our partial block buffer. The memset() call ensures any extra bytes will be zero'ed out.
          //SetLength(block,EVP_MAX_BLOCK_LENGTH);
          {$IFDEF FPC}
          FillByte(block,length(block),0);
          {$ELSE}
          FillMemory(@block,length(block),0);
          {$ENDIF}
          CopyMemory(@block,Pointer(Integer(@MessageToEncrypt[1])+body_length),Length(MessageToEncrypt)-body_length);
          // Advance the body pointer to the location of the remaining space, and calculate just how much room is still available.
          body := Pointer(integer(body)+body_length);
          body_length := secure_body_length(cryptex) - body_length;
          if (body_length <0) then begin
             TLog.NewLog(lterror,'ECIES','The symmetric cipher overflowed!');
             exit;
          end;
          // Pass the final partially filled data block into the cipher as a complete block.
          // The padding will be removed during the decryption process.
          if (EVP_EncryptUpdate(@cipher, body, body_length, @block, block_length)<>1) then begin
            TLog.NewLog(lterror,'ECIES',Format('Unable to secure the data using the chosen symmetric cipher. {error = %s}',
            [ERR_error_string(ERR_get_error(),nil)]));
            exit;
          end;
        end;
        // Advance the pointer, then use pointer arithmetic to calculate how much of the body buffer has been used. The complex logic is needed so that we get
        // the correct status regardless of whether there was a partial data block.
        body := Pointer(integer(body)+body_length);
        body_length := secure_body_length(cryptex) - (Integer(body)-Integer(secure_body_data(cryptex)));
        if (body_length < 0) then begin
          TLog.NewLog(lterror,'ECIES','The symmetric cipher overflowed!');
          exit;
        end;
        if (EVP_EncryptFinal_ex(@cipher, body, body_length)<>1) then begin
          TLog.NewLog(lterror,'ECIES',Format('Unable to secure the data using the chosen symmetric cipher. {error = %s}',
          [ERR_error_string(ERR_get_error(),nil)]));
          exit;
        end;
      finally
        EVP_CIPHER_CTX_cleanup(@cipher);
      end;
      // Generate an authenticated hash which can be used to validate the data during decryption.
      HMAC_CTX_init(@hmac);
      Try
        mac_length := secure_mac_length(cryptex);
        // At the moment we are generating the hash using encrypted data. At some point we may want to validate the original text instead.
        aux := Pointer(Integer(@envelope_key) + key_length);
        if (HMAC_Init_ex(@hmac, aux, key_length, ECIES_HASHER, nil)<>1)
          OR (HMAC_Update(@hmac, secure_body_data(cryptex), secure_body_length(cryptex))<>1)
          OR (HMAC_Final(@hmac, secure_mac_data(cryptex),mac_length)<>1) then begin
          TLog.NewLog(lterror,'ECIES',Format('Unable to generate a data authentication code. {error = %s}',
          [ERR_error_string(ERR_get_error(),nil)]));
          exit;
        end;
      Finally
        HMAC_CTX_cleanup(@hmac);
      End;
      SetLength(Result,secure_total_length(cryptex));
      CopyMemory(@Result[1],cryptex,length(Result));
    finally
      secure_free(cryptex);
    end;
  finally
    EC_KEY_free(PK);
    EC_KEY_free(PEphemeral);
  end;
end;

function ecies_key_create_public_octets(EC_OpenSSL_NID : Word; octets : PAnsiChar; length : size_t) : PEC_KEY;
Var group : PEC_GROUP;
  key : PEC_KEY;
  point : PEC_POINT;
Begin
  Result := Nil;
  key := EC_KEY_new();
  group := EC_GROUP_new_by_curve_name(EC_OpenSSL_NID);
  try
    if (EC_KEY_set_group(key, group)<>1) then exit;
    point := EC_POINT_new(group);
    if (point = Nil) then exit;
    try
      if (EC_POINT_oct2point(group, point, octets, length, nil) <> 1) then exit;
      if (EC_KEY_set_public_key(key, point)<>1) then exit;
    finally
      EC_POINT_free(point);
    end;
    if (EC_KEY_check_key(key)<>1) then exit;
    Result := key;
  finally
    EC_GROUP_free(group);
    if (Result=Nil) then EC_KEY_free(key);
  end;
End;


function ECIESDecrypt(EC_OpenSSL_NID : Word; PrivateKey: PEC_KEY; logErrors : Boolean; const MessageToDecrypt: TRawBytes; Var Decrypted : AnsiString): Boolean;
var
  cryptex : Psecure_t;
  hmac : HMAC_CTX;
  key_length : size_t;
  ephemeral : PEC_KEY;
  envelope_key : Array[1..SHA512_DIGEST_LENGTH] of byte;
  iv : Array[1..EVP_MAX_IV_LENGTH] of byte;
  md : Array[1..EVP_MAX_MD_SIZE] of byte;
  block, output : Pointer;
  mac_length : Cardinal;
  aux : Pointer;
  output_length : Integer;
  cipher : EVP_CIPHER_CTX;
Begin
  Result := false;
  Decrypted := '';
  cryptex := Psecure_t(@MessageToDecrypt[1]);
  // Make sure we are generating enough key material for the symmetric ciphers.
  key_length := EVP_CIPHER_key_length(EVP_aes_256_cbc);
  if (key_length*2>SHA512_DIGEST_LENGTH) then begin
    if logErrors then TLog.NewLog(lterror,'ECIES',Format('The key derivation method will not produce enough envelope key material for the chosen ciphers. {envelope = %d / required = %d)',
      [SHA512_DIGEST_LENGTH DIV 8,(key_length * 2) DIV 8]));
    exit;
  end;
  // Create the ephemeral key used specifically for this block of data.
  ephemeral := ecies_key_create_public_octets(EC_OpenSSL_NID,secure_key_data(cryptex),secure_key_length(cryptex));
  if (ephemeral=Nil) then begin
    if logErrors then TLog.NewLog(lterror,'ECIES','An error occurred while trying to recreate the ephemeral key.');
    exit;
  end;
  try
    // Use the intersection of the provided keys to generate the envelope data used by the ciphers below.
    // The ecies_key_derivation() function uses SHA 512 to ensure we have a sufficient amount of envelope key
    // material and that the material created is sufficiently secure.
    FillMemory(@envelope_key,length(envelope_key),0);
    if (ECDH_compute_key(@envelope_key,SHA512_DIGEST_LENGTH,EC_KEY_get0_public_key(ephemeral),
      PrivateKey, ecies_key_derivation_512)<>SHA512_DIGEST_LENGTH) then begin
      if logErrors then TLog.NewLog(lterror,'ECIES',Format('An error occurred while trying to compute the envelope key. {error = %s}',[ERR_error_string(ERR_get_error, nil)]));
      exit;
    end;
    // The envelope key material has been extracted, so we no longer need the user and ephemeral keys.
  finally
    EC_KEY_free(ephemeral);
  end;
  // Use the authenticated hash of the ciphered data to ensure it was not modified after being encrypted.
  HMAC_CTX_init(@hmac);
  try
    // At the moment we are generating the hash using encrypted data. At some point we may want to validate the original text instead.
    aux := Pointer(Integer(@envelope_key) + key_length);
    if (HMAC_Init_ex(@hmac, aux, key_length, ECIES_HASHER, Nil)<>1) Or
       (HMAC_Update(@hmac, secure_body_data(cryptex), secure_body_length(cryptex))<>1) Or
       (HMAC_Final(@hmac, @md, mac_length)<>1) then begin
       if logErrors then TLog.NewLog(lterror,'ECIES',Format('Unable to generate the authentication code needed for validation. {error = %s}',
         [ERR_error_string(ERR_get_error(), nil)]));
       exit;
    end;
  finally
    HMAC_CTX_cleanup(@hmac);
  end;
  // We can use the generated hash to ensure the encrypted data was not altered after being encrypted.
  if (mac_length<>secure_mac_length(cryptex)) OR ( Not CompareMem(@md,secure_mac_data(cryptex), mac_length)) then begin
    if logErrors then TLog.NewLog(lterror,'ECIES','The authentication code was invalid! The ciphered data has been corrupted!');
    exit;
  end;
  // Create a buffer to hold the result.
  output_length := secure_body_length(cryptex);
  output := AllocMem(output_length+1);
  block := output;
  try
    // For now we use an empty initialization vector. We also clear out the result buffer just to be on the safe side.
    FillMemory(@iv,EVP_MAX_IV_LENGTH,0);
    FillMemory(output,output_length+1,0);
    EVP_CIPHER_CTX_init(@cipher);
    try
      // Decrypt the data using the chosen symmetric cipher.
      if (EVP_DecryptInit_ex(@cipher, EVP_aes_256_cbc, nil,@envelope_key, @iv)<>1) Or
         (EVP_CIPHER_CTX_set_padding(@cipher, 0)<>1) Or
         (EVP_DecryptUpdate(@cipher, block, output_length, secure_body_data(cryptex), secure_body_length(cryptex))<>1) then begin
         if logErrors then TLog.NewLog(lterror,'ECIES',Format('Unable to decrypt the data using the chosen symmetric cipher. {error = %s}',[ERR_error_string(ERR_get_error(), nil)]));
         exit;
      end;
      block := Pointer(Integer(block) + output_length);
      output_length := secure_body_length(cryptex) - output_length;
      if (output_length<>0) then begin
        if logErrors then TLog.NewLog(lterror,'ECIES',Format('The symmetric cipher failed to properly decrypt the correct amount of data! {output_length:%d}',[output_length]));
        exit;
      end;
      if (EVP_DecryptFinal_ex(@cipher,block,output_length)<>1) then begin
        if logErrors then TLog.NewLog(lterror,'ECIES',Format('Unable to decrypt the data using the chosen symmetric cipher. {error = %s}',[ERR_error_string(ERR_get_error(), nil)]));
        exit;
      end;
      SetLength(Decrypted,secure_orig_length(cryptex));
      CopyMemory(@Decrypted[1],output,length(Decrypted));
      Result := true;
    finally
      FreeMemory(output);
    end;
  finally
     EVP_CIPHER_CTX_cleanup(@cipher);
  end;
End;


initialization
finalization
end.

