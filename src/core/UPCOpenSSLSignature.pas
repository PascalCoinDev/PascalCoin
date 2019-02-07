unit UPCOpenSSLSignature;

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
  This unit is used to call ECIES Signature and Verify when using OpenSSL
}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


{$I config.inc}

{$IF (not Defined(Use_OpenSSL))}
  {$Message Fatal 'ERROR: Use_OpenSSL is not defined!'}
  ERROR: Use_OpenSSL is not defined!
{$ENDIF}

Uses SysUtils,
  UOpenSSL,
  UPCDataTypes;

type
  TPCOpenSSLSignature = Class
  public
    class function DoECDSASign(const AEC_OpenSSL_NID : Word; APEC_KEY : PEC_KEY; const AMessage: TBytes; var ASignature : TECDSA_SIG) : Boolean;
    class function DoECDSAVerify(const APublicKey : TECDSA_Public; const AMessage: TBytes; const ASignature : TECDSA_SIG) : Boolean;
  End;

implementation

{ TPCOpenSSLSignature }

class function TPCOpenSSLSignature.DoECDSASign(const AEC_OpenSSL_NID: Word; APEC_KEY : PEC_KEY; const AMessage: TBytes; var ASignature: TECDSA_SIG): Boolean;
Var PECS : PECDSA_SIG;
  p : PAnsiChar;
  i : Integer;
begin
  PECS := ECDSA_do_sign(PAnsiChar(@AMessage[Low(AMessage)]),Length(AMessage),APEC_KEY);
  Try
    if PECS = Nil then raise Exception.Create('Error signing');

    i := BN_num_bytes(PECS^._r);
    SetLength(ASignature.r,i);
    p := @ASignature.r[Low(ASignature.r)];
    i := BN_bn2bin(PECS^._r,p);

    i := BN_num_bytes(PECS^._s);
    SetLength(ASignature.s,i);
    p := @ASignature.s[Low(ASignature.s)];
    i := BN_bn2bin(PECS^._s,p);
  Finally
    ECDSA_SIG_free(PECS);
  End;
  Result := True;
end;

class function TPCOpenSSLSignature.DoECDSAVerify(const APublicKey: TECDSA_Public; const AMessage: TBytes; const ASignature: TECDSA_SIG): Boolean;
Var BNx,BNy : PBIGNUM;
  ECG : PEC_GROUP;
  ctx : PBN_CTX;
  pub_key : PEC_POINT;
  //
  PECS : PECDSA_SIG;
  PK : PEC_KEY;
  {$IFNDEF OpenSSL10}
  bnr,bns : PBIGNUM;
  {$ENDIF}
begin
  BNx := BN_bin2bn(PAnsiChar(APublicKey.x),length(APublicKey.x),nil);
  BNy := BN_bin2bn(PAnsiChar(APublicKey.y),length(APublicKey.y),nil);

  ECG := EC_GROUP_new_by_curve_name(APublicKey.EC_OpenSSL_NID);
  pub_key := EC_POINT_new(ECG);
  ctx := BN_CTX_new;
  if EC_POINT_set_affine_coordinates_GFp(ECG,pub_key,BNx,BNy,ctx)=1 then begin
    PECS := ECDSA_SIG_new;
    Try
      {$IFDEF OpenSSL10}
      BN_bin2bn(PAnsiChar(Signature.r),Length(Signature.r),PECS^._r);
      BN_bin2bn(PAnsiChar(Signature.s),Length(Signature.s),PECS^._s);
      {$ELSE}
      bnr := BN_bin2bn(PAnsiChar(ASignature.r),Length(ASignature.r),nil);
      bns := BN_bin2bn(PAnsiChar(ASignature.s),Length(ASignature.s),nil);
      if ECDSA_SIG_set0(PECS,bnr,bns)<>1 then Raise Exception.Create('Dev error 20161019-1 '+CaptureLastSSLError);
      {$ENDIF}

      PK := EC_KEY_new_by_curve_name(APublicKey.EC_OpenSSL_NID);
      EC_KEY_set_public_key(PK,pub_key);
      Case ECDSA_do_verify(@AMessage[Low(AMessage)],Length(AMessage),PECS,PK) of
        1 : Result := true;
        0 : Result := false;
      Else
        raise Exception.Create('Error on Verify '+CaptureLastSSLError);
      End;
      EC_KEY_free(PK);
    Finally
      ECDSA_SIG_free(PECS);
    End;
  end else begin
    Result := false;
  end;
  BN_CTX_free(ctx);
  EC_POINT_free(pub_key);
  EC_GROUP_free(ECG);
  BN_free(BNx);
  BN_free(BNy);
end;

end.
