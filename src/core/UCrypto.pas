unit UCrypto;

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

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I config.inc}

{$IF (not Defined(Use_CryptoLib4Pascal)) and (not Defined(Use_OpenSSL))}
  ERROR: At least Use_CryptoLib4Pascal or Use_OpenSSL must be defined!
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF Use_OpenSSL}
  UOpenSSL,
  UPCOpenSSLSignature,
  {$ENDIF}
  {$IFDEF Use_CryptoLib4Pascal}
  UPCCryptoLib4Pascal,
  ClpBigInteger,
  ClpCryptoLibTypes,
  {$ENDIF}
  URandomHash, UBaseTypes, UPCDataTypes;

Type
  ECryptoException = Class(Exception);

  { TECPrivateKey }

  TECPrivateKey = Class
  private
    FPrivateKeyInfo: TECPrivateKeyInfo;
    FBufferedPublicKey : TECDSA_Public;
    procedure SetPrivateKeyInfo(const Value: TECPrivateKeyInfo);
    function GetPublicKey: TECDSA_Public;
    function GetEC_OpenSSL_NID: Word;
  public
    Constructor Create;
    Procedure GenerateRandomPrivateKey(EC_OpenSSL_NID : Word);
    Destructor Destroy; override;
    Property PrivateKey : TECPrivateKeyInfo read FPrivateKeyInfo;
    Property PublicKey : TECDSA_Public read GetPublicKey;
    Function SetPrivateKeyFromHexa(AEC_OpenSSL_NID : Word; const hexa : String) : Boolean;
    Property EC_OpenSSL_NID : Word Read GetEC_OpenSSL_NID;
    class function IsValidPublicKey(PubKey : TECDSA_Public; var errors : String) : Boolean; overload;
    class function IsValidPublicKey(PubKey : TECDSA_Public) : Boolean; overload;
    // Exports a Private key in a RAW saving 2 bytes for EC_OpenSSL_NID, 2 bytes for private key length and private key as a RAW
    Function ExportToRaw : TRawBytes;
    // Imports a Private key saved with "ExportToRaw" format
    class Function ImportFromRaw(Const raw : TRawBytes) : TECPrivateKey; static;
    // Exports only the private key as a Raw, without info of EC_OpenSSL_NID
    Function PrivateKeyAsRaw : TRawBytes; // Return only Private key without info of curve used
    function HasPrivateKey : Boolean;
  End;

  { TCrypto }

  TCrypto = Class
  private
  public
    class function IsHexString(const AHexString: String) : boolean;
    class function ToHexaString(const raw : TRawBytes) : String; // DEPRECATED: Use TRawBytes.ToHexaString instead
    class function HexaToRaw(const HexaString : String) : TRawBytes; overload;
    class function HexaToRaw(const HexaString : String; var raw : TRawBytes) : Boolean; overload;
    class function DoSha256(p : PAnsiChar; plength : Cardinal) : TRawBytes; overload;
    class function DoSha256(const TheMessage : TRawBytes) : TRawBytes; overload;
    class procedure DoSha256(const TheMessage : TRawBytes; out ResultSha256 : TRawBytes);  overload;
    class function DoDoubleSha256(const TheMessage : TRawBytes) : TRawBytes; overload;
    class procedure DoDoubleSha256(p : PAnsiChar; plength : Cardinal; out ResultSha256 : TRawBytes); overload;
    class function DoRandomHash(const TheMessage : TRawBytes) : TRawBytes; overload;
    class procedure DoRandomHash(p : PAnsiChar; plength : Cardinal; out ResultSha256 : TRawBytes); overload;
    class procedure DoRandomHash(AFastHasher : TRandomHashFast; p : PAnsiChar; plength : Cardinal; out ResultSha256 : TRawBytes); overload;
    class function DoRipeMD160_HEXASTRING(const TheMessage : TRawBytes) : TRawBytes; overload;
    class function DoRipeMD160AsRaw(p : PAnsiChar; plength : Cardinal) : TRawBytes; overload;
    class function DoRipeMD160AsRaw(const TheMessage : TRawBytes) : TRawBytes; overload;
    // Saves only the PrivKey value in Hexastring
    class function PrivateKey2Hexa(const APrivateKeyInfo : TECPrivateKeyInfo) : String;
    class function ECDSASign(const Key : TECPrivateKeyInfo; const digest : TRawBytes) : TECDSA_SIG;
    class function ECDSAVerify(const PubKey : TECDSA_Public; const digest : TRawBytes; const Signature : TECDSA_SIG) : Boolean; overload;
    class procedure InitCrypto;
    class function IsHumanReadable(Const ReadableText : TRawBytes) : Boolean;
    class function EncodeSignature(const signature : TECDSA_SIG) : TRawBytes;
    class function DecodeSignature(const rawSignature : TRawBytes; out signature : TECDSA_SIG) : Boolean;
  End;

  TBigNum = Class
  private
    {$IFDEF Use_OpenSSL}
    FBN : PBIGNUM;
    {$ELSE}
    FBigInteger : TBigInteger;
    {$ENDIF}
    procedure SetHexaValue(const Value: String);
    function GetHexaValue: String;
    procedure SetValue(const Value: Int64);
    function GetValue: Int64;
    function GetDecimalValue: String;
    procedure SetDecimalValue(const Value: String);
    function GetRawValue: TRawBytes;
    procedure SetRawValue(const Value: TRawBytes);
  public
    Constructor Create; overload;
    Constructor Create(initialValue : Int64); overload;
    Constructor Create(const hexaValue : String); overload;
    Destructor Destroy; override;
    Function Copy : TBigNum;
    Function Add(BN : TBigNum) : TBigNum; overload;
    Function Add(int : Int64) : TBigNum; overload;
    Function Sub(BN : TBigNum) : TBigNum; overload;
    Function Sub(int : Int64) : TBigNum; overload;
    Function Multiply(BN : TBigNum) : TBigNum; overload;
    Function Multiply(int : Int64) : TBigNum; overload;
    Function LShift(nbits : Integer) : TBigNum;
    Function RShift(nbits : Integer) : TBigNum;
    Function CompareTo(BN : TBigNum) : Integer;
    Function Divide(BN : TBigNum) : TBigNum; overload;
    Function Divide(int : Int64) : TBigNum; overload;
    Procedure Divide(dividend, remainder : TBigNum); overload;
    Function ToDecimal : String;
    Property HexaValue : String read GetHexaValue write SetHexaValue;
    Property RawValue : TRawBytes read GetRawValue write SetRawValue;
    Property DecimalValue : String read GetDecimalValue write SetDecimalValue;
    Property Value : Int64 read GetValue write SetValue;
    Function IsZero : Boolean;
    Class Function HexaToDecimal(hexa : String) : String;
    Class Function TargetToHashRate(EncodedTarget : Cardinal) : TBigNum;
  End;

Const
  CT_TECDSA_Public_Nul : TECDSA_Public = (EC_OpenSSL_NID:0;x:Nil;y:Nil);
  CT_TECDSA_SIG_Nul : TECDSA_SIG = (r:Nil;s:Nil);

implementation

uses
  ULog, UConst, UAccounts;

Var _initialized : Boolean = false;

{$IFDEF Use_OpenSSL}
Procedure _DoInit;
var err : String;
 c : Cardinal;
Begin
  if Not (_initialized) then begin
    _initialized := true;
    If Not InitSSLFunctions then begin
      err := 'Cannot load OpenSSL library '+SSL_C_LIB;
      TLog.NewLog(ltError,'OpenSSL',err);
      Raise Exception.Create(err);
    end;
    {$IFNDEF OpenSSL10}
    If Not Assigned(OpenSSL_version_num) then begin
      err := 'OpenSSL library is not v1.1 version: '+SSL_C_LIB;
      TLog.NewLog(ltError,'OpenSSL',err);
      Raise Exception.Create(err);
    end;
    c := OpenSSL_version_num;
    if (c<$10100000) Or (c>$1010FFFF) then begin
      err := 'OpenSSL library is not v1.1 version ('+IntToHex(c,8)+'): '+SSL_C_LIB;
      TLog.NewLog(ltError,'OpenSSL',err);
      Raise Exception.Create(err);
    end;
    {$ENDIF}
  end;
End;
{$ENDIF}

{ TECPrivateKey }

constructor TECPrivateKey.Create;
begin
  FPrivateKeyInfo.EC_KEY_Ptr := Nil;
  FPrivateKeyInfo.RAW_PrivKey := Nil;
  FPrivateKeyInfo.EC_OpenSSL_NID := CT_Default_EC_OpenSSL_NID;
  FBufferedPublicKey := CT_TECDSA_Public_Nul;
end;

destructor TECPrivateKey.Destroy;
begin
  {$IFDEF Use_OpenSSL}
  if Assigned(FPrivateKeyInfo.EC_KEY_Ptr) then EC_KEY_free(FPrivateKeyInfo.EC_KEY_Ptr);
  {$ENDIF}
  FPrivateKeyInfo.EC_KEY_Ptr := Nil;
  FPrivateKeyInfo.RAW_PrivKey := Nil;
  inherited;
end;

function TECPrivateKey.ExportToRaw: TRawBytes;
Var ms : TStream;
  aux : TRawBytes;
begin
  ms := TMemoryStream.Create;
  Try
    ms.Write(FPrivateKeyInfo.EC_OpenSSL_NID,sizeof(FPrivateKeyInfo.EC_OpenSSL_NID));
    {$IFDEF Use_OpenSSL}
    SetLength(aux,BN_num_bytes(EC_KEY_get0_private_key(FPrivateKeyInfo.EC_KEY_Ptr)));
    BN_bn2bin(EC_KEY_get0_private_key(FPrivateKeyInfo.EC_KEY_Ptr),@aux[Low(aux)]);
    {$ELSE}
    aux := FPrivateKeyInfo.RAW_PrivKey;
    {$ENDIF}
    TStreamOp.WriteAnsiString(ms,aux);
    SetLength(Result,ms.Size);
    ms.Position := 0;
    ms.Read(Result[Low(Result)],ms.Size);
  Finally
    ms.Free;
  End;
end;

function TECPrivateKey.PrivateKeyAsRaw: TRawBytes;
begin
  // NOTE: Only returns private key as a RAW without info of EC_OPENSSL_NID
  If Not HasPrivateKey then begin
    SetLength(Result,0);
    Exit;
  end;
  {$IFDEF Use_OpenSSL}
  SetLength(Result,BN_num_bytes(EC_KEY_get0_private_key(FPrivateKeyInfo.EC_KEY_Ptr)));
  BN_bn2bin(EC_KEY_get0_private_key(FPrivateKeyInfo.EC_KEY_Ptr),@Result[Low(Result)]);
  {$ELSE}
  Result := System.Copy(FPrivateKeyInfo.RAW_PrivKey);
  {$ENDIF}
end;

procedure TECPrivateKey.GenerateRandomPrivateKey(EC_OpenSSL_NID : Word);
{$IFDEF Use_OpenSSL}
Var i : Integer;
{$ENDIF}
begin
  FPrivateKeyInfo.EC_OpenSSL_NID := EC_OpenSSL_NID;
  {$IFDEF Use_OpenSSL}
  if Assigned(FPrivateKeyInfo.EC_KEY_Ptr) then EC_KEY_free(FPrivateKeyInfo.EC_KEY_Ptr);
  FPrivateKeyInfo.EC_KEY_Ptr := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
  i := EC_KEY_generate_key(FPrivateKeyInfo.EC_KEY_Ptr);
  if i<>1 then Raise ECryptoException.Create('Error generating new Random Private Key');
  {$ELSE}
  FPrivateKeyInfo.EC_KEY_Ptr := Nil;
  {$ENDIF}
  {$IFDEF Use_CryptoLib4Pascal}
  if Not Assigned(FPrivateKeyInfo.EC_KEY_Ptr) then begin
    FPrivateKeyInfo.RAW_PrivKey := TPCCryptoLib4Pascal.DoGetRandomPrivateKey(EC_OpenSSL_NID);
  end else FPrivateKeyInfo.RAW_PrivKey := Nil;
  {$ELSE}
  FPrivateKeyInfo.RAW_PrivKey := Nil;
  {$ENDIF}
  FBufferedPublicKey := CT_TECDSA_Public_Nul;
end;

function TECPrivateKey.GetEC_OpenSSL_NID: Word;
begin
  Result := FPrivateKeyInfo.EC_OpenSSL_NID;
end;

function TECPrivateKey.GetPublicKey: TECDSA_Public;
{$IFDEF Use_OpenSSL}
var BNx,BNy : PBIGNUM;
  ctx : PBN_CTX;
{$ENDIF}
begin
  if FBufferedPublicKey.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID then begin
    Exit(FBufferedPublicKey);
  end;
{$IFDEF Use_OpenSSL}
  Result.EC_OpenSSL_NID := FPrivateKeyInfo.EC_OpenSSL_NID;
  ctx := BN_CTX_new;
  BNx := BN_new;
  BNy := BN_new;
  Try
    EC_POINT_get_affine_coordinates_GFp(EC_KEY_get0_group(FPrivateKeyInfo.EC_KEY_Ptr),EC_KEY_get0_public_key(FPrivateKeyInfo.EC_KEY_Ptr),BNx,BNy,ctx);
    SetLength(Result.x,BN_num_bytes(BNx));
    BN_bn2bin(BNx,@Result.x[Low(Result.x)]);
    SetLength(Result.y,BN_num_bytes(BNy));
    BN_bn2bin(BNy,@Result.y[Low(Result.y)]);
  Finally
    BN_CTX_free(ctx);
    BN_free(BNx);
    BN_free(BNy);
  End;
{$ELSE}
  Result := TPCCryptoLib4Pascal.DoGetPublicKey(EC_OpenSSL_NID,FPrivateKeyInfo.RAW_PrivKey);
{$ENDIF}
  FBufferedPublicKey := Result;
end;

function TECPrivateKey.HasPrivateKey: Boolean;
begin
  {$IFDEF Use_OpenSSL}
  Result := Assigned(FPrivateKeyInfo.EC_KEY_Ptr);
  {$ELSE}
  Result := Length(FPrivateKeyInfo.RAW_PrivKey)>0;
  {$ENDIF}
end;

class function TECPrivateKey.ImportFromRaw(const raw: TRawBytes): TECPrivateKey;
Var ms : TStream;
  aux : TRawBytes;
  {$IFDEF Use_OpenSSL}
  BNx : PBIGNUM;
  PAC : PAnsiChar;
  {$ENDIF}
  LNewPrivateKeyInfo : TECPrivateKeyInfo;
begin
  Result := Nil;
  LNewPrivateKeyInfo.EC_OpenSSL_NID := 0;
  LNewPrivateKeyInfo.EC_KEY_Ptr := Nil;
  LNewPrivateKeyInfo.RAW_PrivKey := Nil;
  ms := TMemoryStream.Create;
  Try
    ms.WriteBuffer(raw[Low(raw)],Length(raw));
    ms.Position := 0;
    if ms.Read(LNewPrivateKeyInfo.EC_OpenSSL_NID,sizeof(LNewPrivateKeyInfo.EC_OpenSSL_NID))<>sizeof(LNewPrivateKeyInfo.EC_OpenSSL_NID) then exit;
    If TStreamOp.ReadAnsiString(ms,aux)<0 then exit;
    {$IFDEF Use_OpenSSL}
    BNx := BN_bin2bn(PAnsiChar(aux),Length(aux),nil);
    if assigned(BNx) then begin
      try
        PAC := BN_bn2hex(BNx);
        try
          Result := TECPrivateKey.Create;
          Try
            If Not Result.SetPrivateKeyFromHexa(LNewPrivateKeyInfo.EC_OpenSSL_NID,{$IFDEF NO_ANSISTRING}UBaseTypes.{$ENDIF}StrPas(PAC)) then begin
              FreeAndNil(Result);
            end;
          Except
            On E:Exception do begin
              FreeAndNil(Result);
              // Note: Will not raise Exception, only will log it
              TLog.NewLog(lterror,ClassName,'Error importing private key from '+TCrypto.ToHexaString(raw)+' ECID:'+IntToStr(LNewPrivateKeyInfo.EC_OpenSSL_NID)+' ('+E.ClassName+'): '+E.Message);
            end;
          end;
        finally
          OpenSSL_free(PAC);
        end;
      finally
        BN_free(BNx);
      end;
    end;
    {$ELSE}
    Result := TECPrivateKey.Create;
    Try
      LNewPrivateKeyInfo.RAW_PrivKey := aux;
      Result.SetPrivateKeyInfo(LNewPrivateKeyInfo);
    Except
      On E:Exception do begin
        FreeAndNil(Result);
        // Note: Will not raise Exception, only will log it
        TLog.NewLog(lterror,ClassName,'Error importing private key from '+raw.ToHexaString+' ECID:'+IntToStr(LNewPrivateKeyInfo.EC_OpenSSL_NID)+' ('+E.ClassName+'): '+E.Message);
      end;
    End;
    {$ENDIF}
  Finally
    ms.Free;
  End;
end;

class function TECPrivateKey.IsValidPublicKey(PubKey: TECDSA_Public; var errors : String): Boolean;
{$IFDEF Use_OpenSSL}
Var BNx,BNy : PBIGNUM;
  ECG : PEC_GROUP;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  Result := False;
  BNx := BN_bin2bn(PAnsiChar(PubKey.x),length(PubKey.x),nil);
  if Not Assigned(BNx) then Exit;
  try
    BNy := BN_bin2bn(PAnsiChar(PubKey.y),length(PubKey.y),nil);
    if Not Assigned(BNy) then Exit;
    try
      ECG := EC_GROUP_new_by_curve_name(PubKey.EC_OpenSSL_NID);
      if Not Assigned(ECG) then Exit;
      try
        pub_key := EC_POINT_new(ECG);
        try
          if Not Assigned(pub_key) then Exit;
          ctx := BN_CTX_new;
          try
            Result := EC_POINT_set_affine_coordinates_GFp(ECG,pub_key,BNx,BNy,ctx)=1;
            if not Result then begin
              errors := Format('Invalid Public key type:%d - Length x:%d y:%d Error:%s',[PubKey.EC_OpenSSL_NID,length(PubKey.x),length(PubKey.y), CaptureLastSSLError]);
            end;
          finally
            BN_CTX_free(ctx);
          end;
        finally
          EC_POINT_free(pub_key);
        end;
      finally
        EC_GROUP_free(ECG);
      end;
    finally
      BN_free(BNy);
    end;
  finally
    BN_free(BNx);
  end;
{$ELSE}
  Result := True;
  // TODO!!!!!
{$ENDIF}
end;

class function TECPrivateKey.IsValidPublicKey(PubKey: TECDSA_Public): Boolean;
var Ltmp : String;
begin
  Result := IsValidPublicKey(PubKey,Ltmp);
end;

procedure TECPrivateKey.SetPrivateKeyInfo(const Value: TECPrivateKeyInfo);
begin
  {$IFDEF Use_OpenSSL}
  if Assigned(FPrivateKeyInfo.EC_KEY_Ptr) then EC_KEY_free(FPrivateKeyInfo.EC_KEY_Ptr);
  {$ENDIF}
  FPrivateKeyInfo := Value;
  {$IFNDEF Use_OpenSSL}
  FPrivateKeyInfo.EC_KEY_Ptr := Nil;
  {$ENDIF}
  FBufferedPublicKey := CT_TECDSA_Public_Nul;
end;

function TECPrivateKey.SetPrivateKeyFromHexa(AEC_OpenSSL_NID : Word; const hexa : String) : Boolean;
{$IFDEF Use_OpenSSL}
var bn : PBIGNUM;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
  tmp_ansistring : RawByteString;
{$ELSE}
var tmp_raw : TRawBytes;
{$ENDIF}
begin
  Result := False;
  {$IFDEF Use_OpenSSL}
  bn := BN_new;
  try
    tmp_ansistring := hexa;
    if BN_hex2bn(@bn,PAnsiChar(tmp_ansistring))=0 then Raise ECryptoException.Create('Invalid hexa string to convert to Hexadecimal value');

    if Assigned(FPrivateKeyInfo.EC_KEY_Ptr) then EC_KEY_free(FPrivateKeyInfo.EC_KEY_Ptr);
    FPrivateKeyInfo.EC_KEY_Ptr := Nil;

    FPrivateKeyInfo.EC_OpenSSL_NID := AEC_OpenSSL_NID;
    FPrivateKeyInfo.EC_KEY_Ptr := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
    If Not Assigned(FPrivateKeyInfo.EC_KEY_Ptr) then Exit;
    if EC_KEY_set_private_key(FPrivateKeyInfo.EC_KEY_Ptr,bn)<>1 then raise ECryptoException.Create('Invalid num to set as private key');
    //
    ctx := BN_CTX_new;
    pub_key := EC_POINT_new(EC_KEY_get0_group(FPrivateKeyInfo.EC_KEY_Ptr));
    try
      if EC_POINT_mul(EC_KEY_get0_group(FPrivateKeyInfo.EC_KEY_Ptr),pub_key,bn,nil,nil,ctx)<>1 then raise ECryptoException.Create('Error obtaining public key');
      EC_KEY_set_public_key(FPrivateKeyInfo.EC_KEY_Ptr,pub_key);
    finally
      BN_CTX_free(ctx);
      EC_POINT_free(pub_key);
    end;
  finally
    BN_free(bn);
  end;
  {$ELSE}
  if Not TCrypto.HexaToRaw(hexa,tmp_raw) then Raise ECryptoException.Create('Invalid hexa string to convert to Hexadecimal value');
  FPrivateKeyInfo.EC_OpenSSL_NID := AEC_OpenSSL_NID;
  FPrivateKeyInfo.EC_KEY_Ptr := Nil;
  FPrivateKeyInfo.RAW_PrivKey := tmp_raw;
  // TODO: Check is valid!
  {$ENDIF}
  Result := True;
  FBufferedPublicKey := CT_TECDSA_Public_Nul;
end;

{ TCrypto }

{ New at Build 1.0.2
  Note: Delphi is slowly when working with Strings (allowing space)... so to
  increase speed we use a String as a pointer, and only increase speed if
  needed. Also the same with functions "GetMem" and "FreeMem" }
class procedure TCrypto.DoDoubleSha256(p: PAnsiChar; plength: Cardinal; out ResultSha256: TRawBytes);
{$IFDEF Use_OpenSSL}
Var PS : PAnsiChar;
{$ELSE}
var LRaw : TRawBytes;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  If length(ResultSha256)<>32 then SetLength(ResultSha256,32);
  PS := @ResultSha256[Low(ResultSha256)];
  SHA256(p,plength,PS);
  SHA256(PS,32,PS);
{$ELSE}
  SetLength(LRaw,plength);
  move(p^,LRaw[0],plength);
  TPCCryptoLib4Pascal.DoSHA256(LRaw,ResultSha256);
  LRaw := System.Copy(ResultSha256);
  TPCCryptoLib4Pascal.DoSHA256(LRaw,ResultSha256);
{$ENDIF}
end;

class function TCrypto.DoDoubleSha256(const TheMessage: TRawBytes): TRawBytes;
begin
  Result := DoSha256(DoSha256(TheMessage));
end;

class function TCrypto.DoRipeMD160_HEXASTRING(const TheMessage: TRawBytes): TRawBytes;
{$IFDEF Use_OpenSSL}
Var PS : PAnsiChar;
  PC : PAnsiChar;
  i : Integer;
  Ltmp : String;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  GetMem(PS,33);
  RIPEMD160(PAnsiChar(@TheMessage[Low(TheMessage)]),Length(TheMessage),PS);
  PC := PS;
  Ltmp := '';
  for i := 1 to 20 do begin
    Ltmp := Ltmp + IntToHex(PtrInt(PC^),2);
    inc(PC);
  end;
  FreeMem(PS,33);
  Result := TEncoding.ASCII.GetBytes(Ltmp);
{$ELSE}
  Result.FromString(DoRipeMD160AsRaw(TheMessage).ToHexaString.Substring(0,40));
{$ENDIF}
end;

class function TCrypto.DoRipeMD160AsRaw(p: PAnsiChar; plength: Cardinal): TRawBytes;
{$IFDEF Use_OpenSSL}
var PS : PAnsiChar;
{$ELSE}
var Ltmp : TRawBytes;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  SetLength(Result,20);
  PS := @Result[Low(Result)];
  RIPEMD160(p,plength,PS);
{$ELSE}
  SetLength(Ltmp,plength);
  move(p^,Ltmp[0],plength);
  Result := DoRipeMD160AsRaw(Ltmp);
{$ENDIF}
end;

class function TCrypto.DoRipeMD160AsRaw(const TheMessage: TRawBytes): TRawBytes;
{$IFDEF Use_OpenSSL}
Var PS : PAnsiChar;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  SetLength(Result,20);
  PS := @Result[Low(Result)];
  RIPEMD160(PAnsiChar(@TheMessage[Low(TheMessage)]),Length(TheMessage),PS);
{$ELSE}
  TPCCryptoLib4Pascal.DoRIPEMD160(TheMessage,Result);
{$ENDIF}
end;

class function TCrypto.DoSha256(p: PAnsiChar; plength: Cardinal): TRawBytes;
{$IFDEF Use_OpenSSL}
Var PS : PAnsiChar;
{$ELSE}
var Ltmp : TRawBytes;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  SetLength(Result,32);
  PS := @Result[Low(Result)];
  SHA256(p,plength,PS);
{$ELSE}
  SetLength(Ltmp,plength);
  move(p^,Ltmp[0],plength);
  Result := DoSha256(Ltmp);
{$ENDIF}
end;

class function TCrypto.DoSha256(const TheMessage: TRawBytes): TRawBytes;
{$IFDEF Use_OpenSSL}
Var PS : PAnsiChar;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  SetLength(Result,32);
  PS := @Result[Low(Result)];
  SHA256(@TheMessage[Low(TheMessage)],Length(TheMessage),PS);
{$ELSE}
  TPCCryptoLib4Pascal.DoSHA256(TheMessage,Result);
{$ENDIF}
end;

{ New at Build 2.1.6
  Note: Delphi is slowly when working with Strings (allowing space)... so to
  increase speed we use a String as a pointer, and only increase speed if
  needed. Also the same with functions "GetMem" and "FreeMem" }
class procedure TCrypto.DoSha256(const TheMessage: TRawBytes; out ResultSha256: TRawBytes);
{$IFDEF Use_OpenSSL}
Var PS : PAnsiChar;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  If length(ResultSha256)<>32 then SetLength(ResultSha256,32);
  PS := @ResultSha256[Low(ResultSha256)];
  SHA256(@TheMessage[Low(TheMessage)],Length(TheMessage),PS);
{$ELSE}
  TPCCryptoLib4Pascal.DoSHA256(TheMessage,ResultSha256);
{$ENDIF}
end;

class function TCrypto.ECDSASign(const Key: TECPrivateKeyInfo; const digest: TRawBytes): TECDSA_SIG;
begin
{$IFDEF Use_OpenSSL}
  TPCOpenSSLSignature.DoECDSASign(Key.EC_OpenSSL_NID,Key.EC_KEY_Ptr,digest,Result);
{$ELSE}
  TPCCryptoLib4Pascal.DoECDSASign(Key.EC_OpenSSL_NID,Key.RAW_PrivKey,digest,Result);
{$ENDIF}
end;

class function TCrypto.ECDSAVerify(const PubKey: TECDSA_Public; const digest: TRawBytes; const Signature: TECDSA_SIG): Boolean;
begin
{$IFDEF Use_OpenSSL}
  Result := TPCOpenSSLSignature.DoECDSAVerify(PubKey,digest,Signature);
{$ELSE}
  Result := TPCCryptoLib4Pascal.DoECDSAVerify(PubKey,digest,Signature);
{$ENDIF}
end;

class function TCrypto.HexaToRaw(const HexaString: String): TRawBytes;
begin
  HexaToRaw(HexaString,Result);
end;

class function TCrypto.HexaToRaw(const HexaString: String; var raw: TRawBytes): Boolean;
Var i : Integer;
  LHexaRaw : TRawBytes;
  {$IFDEF FPC}
  P : PAnsiChar;
  {$ENDIF}
begin
  LHexaRaw.FromString(LowerCase(HexaString));
  if (Length(LHexaRaw)=0) then begin
    SetLength(raw,0);
    Exit(True);
  end;
  if ((Length(LHexaRaw) MOD 2)<>0) then Exit(False); // odd string
  SetLength(raw,Length(LHexaRaw) DIV 2);
  {$IFDEF FPC}
  P := @raw[0];
  i := HexToBin(PAnsiChar(LHexaRaw.ToString),P,Length(raw));
  {$ELSE}
  i := HexToBin(LHexaRaw,0,raw,0,Length(raw));
  {$ENDIF}
  Result := (i = (Length(raw)));
end;

class procedure TCrypto.InitCrypto;
begin
{$IFDEF Use_OpenSSL}
  _DoInit;
{$ENDIF}
end;

class function TCrypto.IsHumanReadable(const ReadableText: TRawBytes): Boolean;
  // Will return TRUE if all bytes are between 32..126 (ASCII printable bytes)
Var i : Integer;
Begin
  Result := true;
  for i := Low(ReadableText) to High(ReadableText) do begin
    if (ord(ReadableText[i])<32) Or (ord(ReadableText[i])>=127) then begin
      Result := false;
      Exit;
    end;
  end;
end;

class function TCrypto.EncodeSignature(const signature: TECDSA_SIG): TRawBytes;
Var ms : TStream;
begin
  ms := TMemoryStream.Create;
  Try
    TStreamOp.WriteAnsiString(ms,signature.r);
    TStreamOp.WriteAnsiString(ms,signature.s);
    Result := TStreamOp.SaveStreamToRaw(ms);
  finally
    ms.Free;
  end;
end;

class function TCrypto.DecodeSignature(const rawSignature : TRawBytes; out signature : TECDSA_SIG) : Boolean;
var ms : TStream;
begin
  signature := CT_TECDSA_SIG_Nul;
  Result := False;
  ms := TMemoryStream.Create;
  Try
    TStreamOp.LoadStreamFromRaw(ms,rawSignature);
    ms.Position:=0;
    if TStreamOp.ReadAnsiString(ms,signature.r)<0 then Exit;
    if TStreamOp.ReadAnsiString(ms,signature.s)<0 then Exit;
    if ms.Position<ms.Size then Exit; // Invalid position
    Result := True;
  finally
    ms.Free;
  end;
end;

class function TCrypto.PrivateKey2Hexa(const APrivateKeyInfo : TECPrivateKeyInfo): String;
{$IFDEF Use_OpenSSL}
Var p : PAnsiChar;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  p := BN_bn2hex(EC_KEY_get0_private_key(APrivateKeyInfo.EC_KEY_Ptr));
  {$IFDEF NO_ANSISTRING}
  Result := UBaseTypes.StrPas(p); // TODO: Not tested when AnsiString not available!
  {$ELSE}
  Result := StrPas(p);
  {$ENDIF}
  OPENSSL_free(p);
{$ELSE}
  Result := APrivateKeyInfo.RAW_PrivKey.ToHexaString;
{$ENDIF}
end;

class function TCrypto.ToHexaString(const raw: TRawBytes): String;
begin
  Result := raw.ToHexaString;
end;

class function TCrypto.IsHexString(const AHexString: String) : boolean;
var
  i : Integer;
begin
  Result := true;
  for i := Low(AHexString) to High(AHexString) do
    if (NOT (AHexString[i] in ['0'..'9'])) AND
       (NOT (AHexString[i] in ['a'..'f'])) AND
       (NOT (AHexString[i] in ['A'..'F'])) then begin
       Result := false;
       exit;
    end;
end;

{ New at Build 4.0.0 }

class function TCrypto.DoRandomHash(const TheMessage: TRawBytes): TRawBytes;
begin
  Result := TRandomHashFast.Compute(TheMessage);
end;

class procedure TCrypto.DoRandomHash(p : PAnsiChar; plength : Cardinal; out ResultSha256 : TRawBytes);
var
  LInput : TBytes;
  LResult : TBytes;
begin
  if Length(ResultSha256) <> 32 then SetLength(ResultSha256, 32);
  SetLength(LInput, plength);
  Move(p^, LInput[0], plength);
  LResult := TRandomHashFast.Compute(LInput);
  Move(LResult[0], ResultSha256[Low(ResultSha256)], 32);
end;

class procedure TCrypto.DoRandomHash(AFastHasher : TRandomHashFast; p : PAnsiChar; plength : Cardinal; out ResultSha256 : TRawBytes);
var
  LInput : TBytes;
  LResult : TBytes;
begin
  if Length(ResultSha256) <> 32 then SetLength(ResultSha256, 32);
  SetLength(LInput, plength);
  Move(p^, LInput[0], plength);
  LResult := AFastHasher.Hash(LInput);
  Move(LResult[0], ResultSha256[Low(ResultSha256)], 32);
end;

{ TBigNum }

function TBigNum.Add(BN: TBigNum): TBigNum;
begin
  {$IFDEF Use_OpenSSL}
  BN_add(FBN,BN.FBN,FBN);
  {$ELSE}
  FBigInteger := FBigInteger.Add(BN.FBigInteger);
  {$ENDIF}
  Result := Self;
end;

function TBigNum.Add(int: Int64): TBigNum;
Var bn : TBigNum;
begin
  bn := TBigNum.Create(int);
  Result := Add(bn);
  bn.Free;
end;

function TBigNum.CompareTo(BN: TBigNum): Integer;
begin
  {$IFDEF Use_OpenSSL}
  Result := BN_cmp(FBN,BN.FBN);
  {$ELSE}
  Result := FBigInteger.CompareTo(BN.FBigInteger);
  {$ENDIF}
end;

function TBigNum.Copy: TBigNum;
begin
  Result := TBigNum.Create(0);
  {$IFDEF Use_OpenSSL}
  BN_copy(Result.FBN,FBN);
  {$ELSE}
  Result.FBigInteger := FBigInteger; // Make a copy
  {$ENDIF}
end;

constructor TBigNum.Create;
begin
  Create(0);
end;

constructor TBigNum.Create(const hexaValue: String);
begin
  Create(0);
  SetHexaValue(hexaValue);
end;

constructor TBigNum.Create(initialValue : Int64);
begin
  {$IFDEF Use_OpenSSL}
  FBN := BN_new;
  {$ELSE}
  FBigInteger := TBigInteger.Zero;
  {$ENDIF}
  SetValue(initialValue);
end;

destructor TBigNum.Destroy;
begin
  {$IFDEF Use_OpenSSL}
  BN_free(FBN);
  {$ENDIF}
  inherited;
end;

procedure TBigNum.Divide(dividend, remainder: TBigNum);
{$IFDEF Use_OpenSSL}
Var ctx : PBN_CTX;
{$ELSE}
var Ltmp : TCryptoLibGenericArray<TBigInteger>;
{$ENDIF}
begin
  {$IFDEF Use_OpenSSL}
  ctx := BN_CTX_new;
  BN_div(FBN,remainder.FBN,FBN,dividend.FBN,ctx);
  BN_CTX_free(ctx);
  {$ELSE}
  Ltmp := FBigInteger.DivideAndRemainder(dividend.FBigInteger);
  FBigInteger := Ltmp[0];
  remainder.FBigInteger := Ltmp[1];
  {$ENDIF}
end;

function TBigNum.Divide(int: Int64): TBigNum;
Var bn : TBigNum;
begin
  bn := TBigNum.Create(int);
  Result := Divide(bn);
  bn.Free;
end;

function TBigNum.Divide(BN: TBigNum): TBigNum;
{$IFDEF Use_OpenSSL}
Var _div,_rem : PBIGNUM;
  ctx : PBN_CTX;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  _div := BN_new;
  _rem := BN_new;
  ctx := BN_CTX_new;
  BN_div(FBN,_rem,FBN,BN.FBN,ctx);
  BN_free(_div);
  BN_free(_rem);
  BN_CTX_free(ctx);
{$ELSE}
  FBigInteger := FBigInteger.Divide(BN.FBigInteger);
{$ENDIF}
  Result := Self;
end;

function TBigNum.GetDecimalValue: String;
{$IFDEF Use_OpenSSL}
var p : PAnsiChar;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  p := BN_bn2dec(FBN);
  {$IFDEF NO_ANSISTRING}
  Result := UBaseTypes.StrPas(p); // TODO: Not tested when AnsiString not available!
  {$ELSE}
  Result := StrPas(p);
  {$ENDIF}
  OpenSSL_free(p);
{$ELSE}
  Result := FBigInteger.ToString;
{$ENDIF}
end;

function TBigNum.GetHexaValue: String;
{$IFDEF Use_OpenSSL}
Var p : PAnsiChar;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  p := BN_bn2hex(FBN);
  {$IFDEF NO_ANSISTRING}
  Result := UBaseTypes.StrPas(p); // TODO: Not tested when AnsiString not available!
  {$ELSE}
  Result := StrPas(p);
  {$ENDIF}
  OPENSSL_free(p);
{$ELSE}
  Result := FBigInteger.ToByteArrayUnsigned.ToHexaString;
{$ENDIF}
end;

function TBigNum.GetRawValue: TRawBytes;
{$IFDEF Use_OpenSSL}
Var p : PAnsiChar;
  i : Integer;
{$ELSE}
 var
 LBigInteger: TBigInteger;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  i := BN_num_bytes(FBN);
  SetLength(Result,i);
  p := @Result[Low(Result)];
  i := BN_bn2bin(FBN,p);
{$ELSE}
 if FBigInteger.SignValue < 0 then LBigInteger := FBigInteger.Negate // make copy !!! important
 else
  LBigInteger := FBigInteger;
  Result := LBigInteger.ToByteArrayUnsigned;
{$ENDIF}
end;

function TBigNum.GetValue: Int64;
{$IFDEF Use_OpenSSL}
Var p : PAnsiChar;
  a : RawByteString;
  err : Integer;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  p := BN_bn2dec(FBN);
  {$IFDEF NO_ANSISTRING}
  a := UBaseTypes.StrPas(p); // TODO: Not tested when AnsiString not available!
  {$ELSE}
  a := StrPas(p);
  {$ENDIF}
  OPENSSL_free(p);
  Val(a,Result,err);
{$ELSE}
  Result := FBigInteger.Int64Value;
{$ENDIF}
end;

class function TBigNum.HexaToDecimal(hexa: String): String;
Var bn : TBigNum;
begin
  bn := TBigNum.Create(hexa);
  result := bn.ToDecimal;
  bn.Free;
end;

function TBigNum.IsZero: Boolean;
Var dv : String;
begin
  dv := DecimalValue;
  Result := dv='0';
end;

function TBigNum.LShift(nbits: Integer): TBigNum;
begin
{$IFDEF Use_OpenSSL}
  if BN_lshift(FBN,FBN,nbits)<>1 then raise ECryptoException.Create('Error on LShift');
{$ELSE}
  FBigInteger := FBigInteger.ShiftLeft(nbits);
{$ENDIF}
  Result := Self;
end;

function TBigNum.Multiply(int: Int64): TBigNum;
{$IFDEF Use_OpenSSL}
Var n : TBigNum;
  ctx : PBN_CTX;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  n := TBigNum.Create(int);
  Try
    ctx := BN_CTX_new;
    if BN_mul(FBN,FBN,n.FBN,ctx)<>1 then raise ECryptoException.Create('Error on multiply');
  Finally
    BN_CTX_free(ctx);
    n.Free;
  End;
{$ELSE}
  FBigInteger := FBigInteger.Multiply(TBigInteger.Create(IntToStr(int)));
{$ENDIF}
  Result := Self;
end;

function TBigNum.RShift(nbits: Integer): TBigNum;
begin
{$IFDEF Use_OpenSSL}
  if BN_rshift(FBN,FBN,nbits)<>1 then raise ECryptoException.Create('Error on LShift');
{$ELSE}
  FBigInteger := FBigInteger.ShiftRight(nbits);
{$ENDIF}
  Result := Self;
end;

function TBigNum.Multiply(BN: TBigNum): TBigNum;
{$IFDEF Use_OpenSSL}
Var ctx : PBN_CTX;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  ctx := BN_CTX_new;
  if BN_mul(FBN,FBN,BN.FBN,ctx)<>1 then raise ECryptoException.Create('Error on multiply');
  Result := Self;
  BN_CTX_free(ctx);
{$ELSE}
  FBigInteger := FBigInteger.Multiply(BN.FBigInteger);
{$ENDIF}
  Result := Self;
end;

procedure TBigNum.SetDecimalValue(const Value: String);
{$IFDEF Use_OpenSSL}
Var i : Integer;
  tmp_ansistring : RawByteString;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  tmp_ansistring := Value;
  if BN_dec2bn(@FBN,PAnsiChar(tmp_ansistring))=0 then raise ECryptoException.Create('Error on dec2bn');
{$ELSE}
  FBigInteger := TBigInteger.Create(Value);
{$ENDIF}
end;

procedure TBigNum.SetHexaValue(const Value: String);
{$IFDEF Use_OpenSSL}
Var i : Integer;
  tmp_ansistring : RawByteString;
{$ELSE}
var
 LValue: String;
 LowIndex: Integer;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  tmp_ansistring := Value;
  i := BN_hex2bn(@FBN,PAnsiChar(tmp_ansistring));
  if i=0 then begin
      Raise ECryptoException.Create('Invalid Hexadecimal value:'+Value);
  end;
{$ELSE}
  LowIndex := Low(Value);
  if Value[LowIndex] = '-' then LValue := System.Copy(Value, LowIndex + 1, High(Value) - 1)
  else
 LValue := Value;
  if not TCrypto.IsHexString(LValue) then
      Raise ECryptoException.Create('Invalid Hexadecimal value:'+Value);
  FBigInteger := TBigInteger.Create(Value, 16);
{$ENDIF}
end;

procedure TBigNum.SetRawValue(const Value: TRawBytes);
{$IFDEF Use_OpenSSL}
var p : PBIGNUM;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  p := BN_bin2bn(PAnsiChar(Value),length(Value),FBN);
  if (p<>FBN) Or (p=Nil) then Raise ECryptoException.Create('Error decoding Raw value to BigNum "'+TCrypto.ToHexaString(Value)+'" ('+inttostr(length(value))+')'+#10+
    CaptureLastSSLError);
{$ELSE}
  FBigInteger := TBigInteger.Create(1,Value);
{$ENDIF}
end;

procedure TBigNum.SetValue(const Value: Int64);
{$IFDEF Use_OpenSSL}
var a : UInt64;
{$ENDIF}
begin
{$IFDEF Use_OpenSSL}
  if Value<0 then a := (Value * (-1))
  else a := Value;
  if BN_set_word(FBN,a)<>1 then raise ECryptoException.Create('Error on set Value');
  if Value<0 then BN_set_negative(FBN,1)
  else BN_set_negative(FBN,0);
{$ELSE}
  FBigInteger := TBigInteger.Create(IntToStr(Value));
{$ENDIF}
end;

function TBigNum.Sub(BN: TBigNum): TBigNum;
begin
{$IFDEF Use_OpenSSL}
  BN_sub(FBN,FBN,BN.FBN);
{$ELSE}
  FBigInteger := FBigInteger.Subtract(BN.FBigInteger);
{$ENDIF}
  Result := Self;
end;

function TBigNum.Sub(int: Int64): TBigNum;
Var bn : TBigNum;
begin
  bn := TBigNum.Create(int);
  Result := Sub(bn);
  bn.Free;
end;

class function TBigNum.TargetToHashRate(EncodedTarget: Cardinal): TBigNum;
Var
  part_A, part_B : Cardinal;
{$IFDEF Use_OpenSSL}
  ctx : PBN_CTX;
  bn1,bn2 : TBigNum;
{$ELSE}
  LBigInt : TBigInteger;
{$ENDIF}
begin
  { Target is 2 parts: First byte (A) is "0" bits on the left. Bytes 1,2,3 (B) are number after first "1" bit
    Example: Target 23FEBFCE
       Part_A: 23  -> 35 decimal
       Part_B: FEBFCE
    Target to Hash rate Formula:
      Result = 2^Part_A + ( (2^(Part_A-24)) * Part_B )
  }
  Result := TBigNum.Create(2);
  part_A := EncodedTarget shr 24;
  {$IFDEF Use_OpenSSL}
  bn1 := TBigNum.Create(part_A);
  ctx := BN_CTX_new;
  try
    if BN_exp(Result.FBN,Result.FBN,bn1.FBN,ctx)<>1 then raise Exception.Create('Error 20161017-3');
  finally
    BN_CTX_free(ctx);
    bn1.Free;
  end;
  {$ELSE}
  Result.FBigInteger := Result.FBigInteger.Pow(part_A);
  {$ENDIF}
  //
  part_B := (EncodedTarget shl 8) shr 8;
  //
  if (part_A<24) then begin
    // exponent is negative... 2^(Part_A-24)
    part_B := (part_B shr (24-part_A));
    {$IFDEF Use_OpenSSL}
    bn1 := TBigNum.Create(part_B);
    Try
      Result.Add(bn1);
      Exit;
    Finally
      bn1.Free;
    End;
    {$ELSE}
    Result.FBigInteger := Result.FBigInteger.Add(TBigInteger.Create(IntToStr(part_b)));
    Exit;
    {$ENDIF}
  end;
  //
  {$IFDEF Use_OpenSSL}
  bn2 := TBigNum.Create(2);
  Try
    bn1 := TBigNum.Create(Int64(part_A) - 24);
    ctx := BN_CTX_new;
    try
      If BN_exp(bn2.FBN,bn2.FBN,bn1.FBN,ctx)<>1 then raise Exception.Create('Error 20161017-4');
    finally
      BN_CTX_free(ctx);
      bn1.Free;
    end;
    bn2.Multiply(part_B);
    Result.Add(bn2);
  Finally
    bn2.Free;
  End;
  {$ELSE}
  LBigInt := TBigInteger.Two;
  LBigInt := LBigInt.Pow(Int64(part_A)-24);
  LBigInt := LBigInt.Multiply(TBigInteger.Create(IntToStr(part_b)));
  Result.FBigInteger := Result.FBigInteger.Add(LBigInt);
  {$ENDIF}
end;

function TBigNum.ToDecimal: String;
begin
  Result := GetDecimalValue;
end;


initialization
  Randomize; // Initial random generator based on system time
finalization
end.
