unit UCrypto;

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

{$I config.inc}

interface

uses
  Classes, SysUtils, UOpenSSL, UOpenSSLdef;

Type
  ECryptoException = Class(Exception);

  TRawBytes = AnsiString;
  PRawBytes = ^TRawBytes;

  TECDSA_SIG = record
     r: TRawBytes;
     s: TRawBytes;
  end; { record }

  TECDSA_Public = record
     EC_OpenSSL_NID : Word;
     x: TRawBytes;
     y: TRawBytes;
  end;
  PECDSA_Public = ^TECDSA_Public;

  TECPrivateKey = Class
  private
    FPrivateKey: PEC_KEY;
    FEC_OpenSSL_NID : Word;
    procedure SetPrivateKey(const Value: PEC_KEY);
    function GetPublicKey: TECDSA_Public;
    function GetPublicKeyPoint: PEC_POINT;
  public
    Constructor Create;
    Procedure GenerateRandomPrivateKey(EC_OpenSSL_NID : Word);
    Destructor Destroy; override;
    Property PrivateKey : PEC_KEY read FPrivateKey;
    Property PublicKey : TECDSA_Public read GetPublicKey;
    Property PublicKeyPoint : PEC_POINT read GetPublicKeyPoint;
    Function SetPrivateKeyFromHexa(EC_OpenSSL_NID : Word; hexa : AnsiString) : Boolean;
    Property EC_OpenSSL_NID : Word Read FEC_OpenSSL_NID;
    class function IsValidPublicKey(PubKey : TECDSA_Public) : Boolean;
    Function ExportToRaw : TRawBytes;
    class Function ImportFromRaw(Const raw : TRawBytes) : TECPrivateKey; static;
  End;

  { TCrypto }

  TCrypto = Class
  private
  public
    class function IsHexString(const AHexString: AnsiString) : boolean;
    class function ToHexaString(const raw : TRawBytes) : AnsiString;
    class function HexaToRaw(const HexaString : AnsiString) : TRawBytes;
    class function DoSha256(p : PAnsiChar; plength : Cardinal) : TRawBytes; overload;
    class function DoSha256(const TheMessage : AnsiString) : TRawBytes; overload;
    class procedure DoSha256(const TheMessage : AnsiString; var ResultSha256 : TRawBytes);  overload;
    class procedure DoDoubleSha256(p : PAnsiChar; plength : Cardinal; Var ResultSha256 : TRawBytes); overload;
    class function DoRipeMD160_HEXASTRING(const TheMessage : AnsiString) : TRawBytes; overload;
    class function DoRipeMD160AsRaw(p : PAnsiChar; plength : Cardinal) : TRawBytes; overload;
    class function DoRipeMD160AsRaw(const TheMessage : AnsiString) : TRawBytes; overload;
    class function PrivateKey2Hexa(Key : PEC_KEY) : AnsiString;
    class function ECDSASign(Key : PEC_KEY; const digest : AnsiString) : TECDSA_SIG;
    class function ECDSAVerify(EC_OpenSSL_NID : Word; PubKey : EC_POINT; const digest : AnsiString; Signature : TECDSA_SIG) : Boolean; overload;
    class function ECDSAVerify(PubKey : TECDSA_Public; const digest : AnsiString; Signature : TECDSA_SIG) : Boolean; overload;
    class procedure InitCrypto;
    class function IsHumanReadable(Const ReadableText : TRawBytes) : Boolean;
  End;

  TBigNum = Class
  private
    FBN : PBIGNUM;
    procedure SetHexaValue(const Value: AnsiString);
    function GetHexaValue: AnsiString;
    procedure SetValue(const Value: Int64);
    function GetValue: Int64;
    function GetDecimalValue: AnsiString;
    procedure SetDecimalValue(const Value: AnsiString);
    function GetRawValue: TRawBytes;
    procedure SetRawValue(const Value: TRawBytes);
  public
    Constructor Create; overload;
    Constructor Create(initialValue : Int64); overload;
    Constructor Create(hexaValue : AnsiString); overload;
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
    Function ToInt64(var int : Int64) : TBigNum;
    Function ToDecimal : AnsiString;
    Property HexaValue : AnsiString read GetHexaValue write SetHexaValue;
    Property RawValue : TRawBytes read GetRawValue write SetRawValue;
    Property DecimalValue : AnsiString read GetDecimalValue write SetDecimalValue;
    Property Value : Int64 read GetValue write SetValue;
    Function IsZero : Boolean;
    Class Function HexaToDecimal(hexa : AnsiString) : AnsiString;
    Class Function TargetToHashRate(EncodedTarget : Cardinal) : TBigNum;
  End;

Const
  CT_TECDSA_Public_Nul : TECDSA_Public = (EC_OpenSSL_NID:0;x:'';y:'');

implementation

uses
  ULog, UConst, UAccounts;

Var _initialized : Boolean = false;

Procedure _DoInit;
Begin
  if Not (_initialized) then begin
    _initialized := true;
    InitSSLFunctions;
  end;
End;

{ TECPrivateKey }

constructor TECPrivateKey.Create;
begin
  FPrivateKey := Nil;
  FEC_OpenSSL_NID := CT_Default_EC_OpenSSL_NID;
end;

destructor TECPrivateKey.Destroy;
begin
  if Assigned(FPrivateKey) then EC_KEY_free(FPrivateKey);
  inherited;
end;

function TECPrivateKey.ExportToRaw: TRawBytes;
Var ms : TStream;
  aux : TRawBytes;
begin
  ms := TMemoryStream.Create;
  Try
    ms.Write(FEC_OpenSSL_NID,sizeof(FEC_OpenSSL_NID));
    SetLength(aux,BN_num_bytes(EC_KEY_get0_private_key(FPrivateKey)));
    BN_bn2bin(EC_KEY_get0_private_key(FPrivateKey),@aux[1]);
    TStreamOp.WriteAnsiString(ms,aux);
    SetLength(Result,ms.Size);
    ms.Position := 0;
    ms.Read(Result[1],ms.Size);
  Finally
    ms.Free;
  End;
end;

procedure TECPrivateKey.GenerateRandomPrivateKey(EC_OpenSSL_NID : Word);
Var i : Integer;
begin
  if Assigned(FPrivateKey) then EC_KEY_free(FPrivateKey);
  FEC_OpenSSL_NID := EC_OpenSSL_NID;
  FPrivateKey := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
  i := EC_KEY_generate_key(FPrivateKey);
  if i<>1 then Raise ECryptoException.Create('Error generating new Random Private Key');
end;

function TECPrivateKey.GetPublicKey: TECDSA_Public;
var ps : PAnsiChar;
  BNx,BNy : PBIGNUM;
  ctx : PBN_CTX;
begin
  Result.EC_OpenSSL_NID := FEC_OpenSSL_NID;
  ctx := BN_CTX_new;
  BNx := BN_new;
  BNy := BN_new;
  Try
    EC_POINT_get_affine_coordinates_GFp(EC_KEY_get0_group(FPrivateKey),EC_KEY_get0_public_key(FPrivateKey),BNx,BNy,ctx);
    SetLength(Result.x,BN_num_bytes(BNx));
    BN_bn2bin(BNx,@Result.x[1]);
    SetLength(Result.y,BN_num_bytes(BNy));
    BN_bn2bin(BNy,@Result.y[1]);
  Finally
    BN_CTX_free(ctx);
    BN_free(BNx);
    BN_free(BNy);
  End;
end;

function TECPrivateKey.GetPublicKeyPoint: PEC_POINT;
begin
  Result := EC_KEY_get0_public_key(FPrivateKey);
end;

class function TECPrivateKey.ImportFromRaw(const raw: TRawBytes): TECPrivateKey;
Var ms : TStream;
  aux : TRawBytes;
  BNx : PBIGNUM;
  ECID : Word;
  PAC : PAnsiChar;
begin
  Result := Nil;
  ms := TMemoryStream.Create;
  Try
    ms.WriteBuffer(raw[1],length(raw));
    ms.Position := 0;
    if ms.Read(ECID,sizeof(ECID))<>sizeof(ECID) then exit;
    If TStreamOp.ReadAnsiString(ms,aux)<0 then exit;
    BNx := BN_bin2bn(PAnsiChar(aux),length(aux),nil);
    if assigned(BNx) then begin
      try
        PAC := BN_bn2hex(BNx);
        try
          Result := TECPrivateKey.Create;
          Try
            Result.SetPrivateKeyFromHexa(ECID,PAC);
          Except
            FreeAndNil(Result);
            Raise;
          end;
        finally
          OpenSSL_free(PAC);
        end;
      finally
        BN_free(BNx);
      end;
    end;
  Finally
    ms.Free;
  End;
end;

class function TECPrivateKey.IsValidPublicKey(PubKey: TECDSA_Public): Boolean;
Var BNx,BNy : PBIGNUM;
  ECG : PEC_GROUP;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
begin
  BNx := BN_bin2bn(PAnsiChar(PubKey.x),length(PubKey.x),nil);
  try
    BNy := BN_bin2bn(PAnsiChar(PubKey.y),length(PubKey.y),nil);
    try
      ECG := EC_GROUP_new_by_curve_name(PubKey.EC_OpenSSL_NID);
      try
        pub_key := EC_POINT_new(ECG);
        try
          ctx := BN_CTX_new;
          try
            Result := EC_POINT_set_affine_coordinates_GFp(ECG,pub_key,BNx,BNy,ctx)=1;
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
end;

procedure TECPrivateKey.SetPrivateKey(const Value: PEC_KEY);
begin
  if Assigned(FPrivateKey) then EC_KEY_free(FPrivateKey);
  FPrivateKey := Value;
end;

function TECPrivateKey.SetPrivateKeyFromHexa(EC_OpenSSL_NID : Word; hexa : AnsiString) : Boolean;
var bn : PBIGNUM;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
begin
  bn := BN_new;
  try
    if BN_hex2bn(@bn,PAnsiChar(hexa))=0 then Raise ECryptoException.Create('Invalid Hexadecimal value:'+hexa);

    if Assigned(FPrivateKey) then EC_KEY_free(FPrivateKey);
    FEC_OpenSSL_NID := EC_OpenSSL_NID;
    FPrivateKey := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);

    if EC_KEY_set_private_key(FPrivateKey,bn)<>1 then raise ECryptoException.Create('Invalid num to set as private key');
    //
    ctx := BN_CTX_new;
    pub_key := EC_POINT_new(EC_KEY_get0_group(FPrivateKey));
    try
      if EC_POINT_mul(EC_KEY_get0_group(FPrivateKey),pub_key,bn,nil,nil,ctx)<>1 then raise ECryptoException.Create('Error obtaining public key');
      EC_KEY_set_public_key(FPrivateKey,pub_key);
    finally
      BN_CTX_free(ctx);
      EC_POINT_free(pub_key);
    end;
  finally
    BN_free(bn);
  end;
end;

{ TCrypto }

{ New at Build 1.0.2
  Note: Delphi is slowly when working with Strings (allowing space)... so to
  increase speed we use a String as a pointer, and only increase speed if
  needed. Also the same with functions "GetMem" and "FreeMem" }
class procedure TCrypto.DoDoubleSha256(p: PAnsiChar; plength: Cardinal;
  Var ResultSha256: TRawBytes);
Var PS : PAnsiChar;
  PC : PAnsiChar;
begin
  If length(ResultSha256)<>32 then SetLength(ResultSha256,32);
  PS := @ResultSha256[1];
  SHA256(p,plength,PS);
  SHA256(PS,32,PS);
end;

class function TCrypto.DoRipeMD160_HEXASTRING(const TheMessage: AnsiString): TRawBytes;
Var PS : PAnsiChar;
  PC : PAnsiChar;
  i : Integer;
begin
  GetMem(PS,33);
  RIPEMD160(PAnsiChar(TheMessage),Length(TheMessage),PS);
  PC := PS;
  Result := '';
  for I := 1 to 20 do begin
    Result := Result + IntToHex(PtrInt(PC^),2);
    inc(PC);
  end;
  FreeMem(PS,33);
end;

class function TCrypto.DoRipeMD160AsRaw(p: PAnsiChar; plength: Cardinal): TRawBytes;
Var PS : PAnsiChar;
begin
  SetLength(Result,20);
  PS := @Result[1];
  RIPEMD160(p,plength,PS);
end;

class function TCrypto.DoRipeMD160AsRaw(const TheMessage: AnsiString): TRawBytes;
Var PS : PAnsiChar;
begin
  SetLength(Result,20);
  PS := @Result[1];
  RIPEMD160(PAnsiChar(TheMessage),Length(TheMessage),PS);
end;

class function TCrypto.DoSha256(p: PAnsiChar; plength: Cardinal): TRawBytes;
Var PS : PAnsiChar;
begin
  SetLength(Result,32);
  PS := @Result[1];
  SHA256(p,plength,PS);
end;

class function TCrypto.DoSha256(const TheMessage: AnsiString): TRawBytes;
Var PS : PAnsiChar;
begin
  SetLength(Result,32);
  PS := @Result[1];
  SHA256(PAnsiChar(TheMessage),Length(TheMessage),PS);
end;

{ New at Build 2.1.6
  Note: Delphi is slowly when working with Strings (allowing space)... so to
  increase speed we use a String as a pointer, and only increase speed if
  needed. Also the same with functions "GetMem" and "FreeMem" }
class procedure TCrypto.DoSha256(const TheMessage: AnsiString; var ResultSha256: TRawBytes);
Var PS : PAnsiChar;
  PC : PAnsiChar;
begin
  If length(ResultSha256)<>32 then SetLength(ResultSha256,32);
  PS := @ResultSha256[1];
  SHA256(PAnsiChar(TheMessage),Length(TheMessage),PS);
end;

class function TCrypto.ECDSASign(Key: PEC_KEY; const digest: AnsiString): TECDSA_SIG;
Var PECS : PECDSA_SIG;
  p, pr,ps : PAnsiChar;
  i : Integer;
  {$IFDEF OpenSSL10}
  {$ELSE}
  bnr,bns : PBIGNUM;
  {$ENDIF}
begin
  PECS := ECDSA_do_sign(PAnsiChar(digest),length(digest),Key);
  Try
    if PECS = Nil then raise ECryptoException.Create('Error signing');

    {$IFDEF OpenSSL10}
    i := BN_num_bytes(PECS^._r);
    SetLength(Result.r,i);
    p := @Result.r[1];
    i := BN_bn2bin(PECS^._r,p);

    i := BN_num_bytes(PECS^._s);
    SetLength(Result.s,i);
    p := @Result.s[1];
    i := BN_bn2bin(PECS^._s,p);
    {$ELSE}
    ECDSA_SIG_get0(PECS,@bnr,@bns);
    i := BN_num_bytes(bnr);
    SetLength(Result.r,i);
    p := @Result.r[1];
    i := BN_bn2bin(bnr,p);
    i := BN_num_bytes(bns);
    SetLength(Result.s,i);
    p := @Result.s[1];
    i := BN_bn2bin(bns,p);
    {$ENDIF}
  Finally
    ECDSA_SIG_free(PECS);
  End;
end;

class function TCrypto.ECDSAVerify(EC_OpenSSL_NID : Word; PubKey: EC_POINT; const digest: AnsiString; Signature: TECDSA_SIG): Boolean;
Var PECS : PECDSA_SIG;
  PK : PEC_KEY;
  {$IFDEF OpenSSL10}
  {$ELSE}
  bnr,bns : PBIGNUM;
  {$ENDIF}
begin
  PECS := ECDSA_SIG_new;
  Try
    {$IFDEF OpenSSL10}
    BN_bin2bn(PAnsiChar(Signature.r),length(Signature.r),PECS^._r);
    BN_bin2bn(PAnsiChar(Signature.s),length(Signature.s),PECS^._s);
    {$ELSE}
{    ECDSA_SIG_get0(PECS,@bnr,@bns);
    BN_bin2bn(PAnsiChar(Signature.r),length(Signature.r),bnr);
    BN_bin2bn(PAnsiChar(Signature.s),length(Signature.s),bns);}
    bnr := BN_bin2bn(PAnsiChar(Signature.r),length(Signature.r),nil);
    bns := BN_bin2bn(PAnsiChar(Signature.s),length(Signature.s),nil);
    if ECDSA_SIG_set0(PECS,bnr,bns)<>1 then Raise Exception.Create('Dev error 20161019-1 '+ERR_error_string(ERR_get_error(),nil));
    {$ENDIF}

    PK := EC_KEY_new_by_curve_name(EC_OpenSSL_NID);
    EC_KEY_set_public_key(PK,@PubKey);
    Case ECDSA_do_verify(PAnsiChar(digest),length(digest),PECS,PK) of
      1 : Result := true;
      0 : Result := false;
    Else
      raise ECryptoException.Create('Error on Verify');
    End;
    EC_KEY_free(PK);
  Finally
    ECDSA_SIG_free(PECS);
  End;
end;

class function TCrypto.ECDSAVerify(PubKey: TECDSA_Public; const digest: AnsiString; Signature: TECDSA_SIG): Boolean;
Var BNx,BNy : PBIGNUM;
  ECG : PEC_GROUP;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
begin
  BNx := BN_bin2bn(PAnsiChar(PubKey.x),length(PubKey.x),nil);
  BNy := BN_bin2bn(PAnsiChar(PubKey.y),length(PubKey.y),nil);

  ECG := EC_GROUP_new_by_curve_name(PubKey.EC_OpenSSL_NID);
  pub_key := EC_POINT_new(ECG);
  ctx := BN_CTX_new;
  if EC_POINT_set_affine_coordinates_GFp(ECG,pub_key,BNx,BNy,ctx)=1 then begin
    Result := ECDSAVerify(PubKey.EC_OpenSSL_NID, pub_key^,digest,signature);
  end else begin
    Result := false;
  end;
  BN_CTX_free(ctx);
  EC_POINT_free(pub_key);
  EC_GROUP_free(ECG);
  BN_free(BNx);
  BN_free(BNy);
end;

class function TCrypto.HexaToRaw(const HexaString: AnsiString): TRawBytes;
Var P : PAnsiChar;
 lc : AnsiString;
 i : Integer;
begin
  Result := '';
  if ((length(HexaString) MOD 2)<>0) Or (length(HexaString)=0) then exit;
  SetLength(result,length(HexaString) DIV 2);
  P := @Result[1];
  lc := LowerCase(HexaString);
  i := HexToBin(PAnsiChar(@lc[1]),P,length(Result));
  if (i<>(length(HexaString) DIV 2)) then begin
    TLog.NewLog(lterror,Classname,'Invalid HEXADECIMAL string result '+inttostr(i)+'<>'+inttostr(length(HexaString) DIV 2)+': '+HexaString);
    Result := '';
  end;
end;

class procedure TCrypto.InitCrypto;
begin
  _DoInit;
end;

class function TCrypto.IsHumanReadable(const ReadableText: TRawBytes): Boolean;
Var i : Integer;
Begin
  Result := true;
  for i := 1 to length(ReadableText) do begin
    if (ord(ReadableText[i])<32) Or (ord(ReadableText[i])>=255) then begin
      Result := false;
      Exit;
    end;
  end;
end;

class function TCrypto.PrivateKey2Hexa(Key: PEC_KEY): AnsiString;
Var p : PAnsiChar;
begin
  p := BN_bn2hex(EC_KEY_get0_private_key(Key));
//  p := BN_bn2hex(Key^.priv_key);
  Result := strpas(p);
  OPENSSL_free(p);
end;

class function TCrypto.ToHexaString(const raw: TRawBytes): AnsiString;
Var i : Integer;
  s : AnsiString;
  b : Byte;
begin
  SetLength(Result,length(raw)*2);
  for i := 0 to length(raw)-1 do begin
    b := Ord(raw[i+1]);
    s := IntToHex(b,2);
    Result[(i*2)+1] := s[1];
    Result[(i*2)+2] := s[2];
  end;
end;

class function TCrypto.IsHexString(const AHexString: AnsiString) : boolean;
var
  i : Integer;
begin
  Result := true;
  for i := 0 to length(AHexString) do
    if (NOT (AHexString[i] in ['0'..'9'])) AND
       (NOT (AHexString[i] in ['a'..'f'])) AND
       (NOT (AHexString[i] in ['A'..'F'])) then begin
       Result := false;
       exit;
    end;
end;

{ TBigNum }

function TBigNum.Add(BN: TBigNum): TBigNum;
begin
  BN_add(FBN,BN.FBN,FBN);
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
  Result := BN_cmp(FBN,BN.FBN);
end;

function TBigNum.Copy: TBigNum;
begin
  Result := TBigNum.Create(0);
  BN_copy(Result.FBN,FBN);
end;

constructor TBigNum.Create;
begin
  Create(0);
end;

constructor TBigNum.Create(hexaValue: AnsiString);
begin
  Create(0);
  SetHexaValue(hexaValue);
end;

constructor TBigNum.Create(initialValue : Int64);
begin
  FBN := BN_new;
  SetValue(initialValue);
end;

destructor TBigNum.Destroy;
begin
  BN_free(FBN);
  inherited;
end;

procedure TBigNum.Divide(dividend, remainder: TBigNum);
Var ctx : PBN_CTX;
begin
  ctx := BN_CTX_new;
  BN_div(FBN,remainder.FBN,FBN,dividend.FBN,ctx);
  BN_CTX_free(ctx);
end;

function TBigNum.Divide(int: Int64): TBigNum;
Var bn : TBigNum;
begin
  bn := TBigNum.Create(int);
  Result := Divide(bn);
  bn.Free;
end;

function TBigNum.Divide(BN: TBigNum): TBigNum;
Var _div,_rem : PBIGNUM;
  ctx : PBN_CTX;
begin
  _div := BN_new;
  _rem := BN_new;
  ctx := BN_CTX_new;
  BN_div(FBN,_rem,FBN,BN.FBN,ctx);
  BN_free(_div);
  BN_free(_rem);
  BN_CTX_free(ctx);
  Result := Self;
end;

function TBigNum.GetDecimalValue: AnsiString;
var p : PAnsiChar;
begin
  p := BN_bn2dec(FBN);
  Result := strpas(p);
  OpenSSL_free(p);
end;

function TBigNum.GetHexaValue: AnsiString;
Var p : PAnsiChar;
begin
  p := BN_bn2hex(FBN);
  Result := strpas( p );
  OPENSSL_free(p);
end;

function TBigNum.GetRawValue: TRawBytes;
Var p : PAnsiChar;
  i : Integer;
begin
  i := BN_num_bytes(FBN);
  SetLength(Result,i);
  p := @Result[1];
  i := BN_bn2bin(FBN,p);
end;

function TBigNum.GetValue: Int64;
Var p : PAnsiChar;
  a : AnsiString;
  err : Integer;
begin
  p := BN_bn2dec(FBN);
  a := strpas(p);
  OPENSSL_free(p);
  val(a,Result,err);
end;

class function TBigNum.HexaToDecimal(hexa: AnsiString): AnsiString;
Var bn : TBigNum;
begin
  bn := TBigNum.Create(hexa);
  result := bn.ToDecimal;
  bn.Free;
end;

function TBigNum.IsZero: Boolean;
Var dv : AnsiString;
begin
  dv := DecimalValue;
  Result := dv='0';
end;

function TBigNum.LShift(nbits: Integer): TBigNum;
begin
  if BN_lshift(FBN,FBN,nbits)<>1 then raise ECryptoException.Create('Error on LShift');
  Result := Self;
end;

function TBigNum.Multiply(int: Int64): TBigNum;
Var n : TBigNum;
  ctx : PBN_CTX;
begin
  n := TBigNum.Create(int);
  Try
    ctx := BN_CTX_new;
    if BN_mul(FBN,FBN,n.FBN,ctx)<>1 then raise ECryptoException.Create('Error on multiply');
    Result := Self;
  Finally
    BN_CTX_free(ctx);
    n.Free;
  End;
end;

function TBigNum.RShift(nbits: Integer): TBigNum;
begin
  if BN_rshift(FBN,FBN,nbits)<>1 then raise ECryptoException.Create('Error on LShift');
  Result := Self;
end;

function TBigNum.Multiply(BN: TBigNum): TBigNum;
Var ctx : PBN_CTX;
begin
  ctx := BN_CTX_new;
  if BN_mul(FBN,FBN,BN.FBN,ctx)<>1 then raise ECryptoException.Create('Error on multiply');
  Result := Self;
  BN_CTX_free(ctx);
  Result := Self;
end;

procedure TBigNum.SetDecimalValue(const Value: AnsiString);
Var i : Integer;
begin
  if BN_dec2bn(@FBN,PAnsiChar(Value))=0 then raise ECryptoException.Create('Error on dec2bn');
end;

procedure TBigNum.SetHexaValue(const Value: AnsiString);
Var i : Integer;
begin
  i := BN_hex2bn(@FBN,PAnsiChar(Value));
  if i=0 then begin
      Raise ECryptoException.Create('Invalid Hexadecimal value:'+Value);
  end;
end;

procedure TBigNum.SetRawValue(const Value: TRawBytes);
var p : PBIGNUM;
begin
  p := BN_bin2bn(PAnsiChar(Value),length(Value),FBN);
  if (p<>FBN) Or (p=Nil) then Raise ECryptoException.Create('Error decoding Raw value to BigNum "'+TCrypto.ToHexaString(Value)+'" ('+inttostr(length(value))+')'+#10+
    ERR_error_string(ERR_get_error(),nil));
end;

procedure TBigNum.SetValue(const Value: Int64);
var a : UInt64;
begin
  if Value<0 then a := (Value * (-1))
  else a := Value;
  if BN_set_word(FBN,a)<>1 then raise ECryptoException.Create('Error on set Value');
  if Value<0 then BN_set_negative(FBN,1)
  else BN_set_negative(FBN,0);
end;

function TBigNum.Sub(BN: TBigNum): TBigNum;
begin
  BN_sub(FBN,FBN,BN.FBN);
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
Var bn1,bn2 : TBigNum;
  part_A, part_B : Cardinal;
  ctx : PBN_CTX;
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
  bn1 := TBigNum.Create(part_A);
  ctx := BN_CTX_new;
  try
    if BN_exp(Result.FBN,Result.FBN,bn1.FBN,ctx)<>1 then raise Exception.Create('Error 20161017-3');
  finally
    BN_CTX_free(ctx);
    bn1.Free;
  end;
  //
  if part_A<=24 then exit;
  //
  part_B := (EncodedTarget shl 8) shr 8;
  bn2 := TBigNum.Create(2);
  Try
    bn1 := TBigNum.Create(part_A - 24);
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
end;

function TBigNum.ToDecimal: AnsiString;
var p : PAnsiChar;
begin
  p := BN_bn2dec(FBN);
  Result := strpas(p);
  OpenSSL_free(p);
end;

function TBigNum.ToInt64(var int: Int64): TBigNum;
Var s : AnsiString;
 err : Integer;
 p : PAnsiChar;
begin
  p := BN_bn2dec(FBN);
  s := strpas( p );
  OPENSSL_free(p);
  val(s,int,err);
  if err<>0 then int := 0;
  Result := Self;
end;


initialization
finalization
end.
