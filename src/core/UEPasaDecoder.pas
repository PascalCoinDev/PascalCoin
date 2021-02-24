unit UEPasaDecoder;

{ Copyright (c) PascalCoin Developers - Herman Schoenfeld - Albert Molina

  PIP-0027: E-PASA Reference Implementation
  See: https://github.com/PascalCoin/PascalCoin/blob/master/PIP/PIP-0027.md

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Source: https://github.com/PascalCoin/PascalCoin

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF FPC}

interface

{$I ./../config.inc}

uses
  SysUtils,
  TypInfo,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  UBlockChain, UNode, UBaseTypes, UPCDataTypes,
  UAccounts,
  UEncoding,
  UEPasa,
  UWallet,
  URPC, UJSONFunctions;

type
  TDecodeEPasaResult = (der_Decoded, der_Undefined, der_NonDeterministic, der_InvalidPayloadType, der_AccountNameNotFound, der_NotEnoughData, der_PrivateKeyNotFound, der_PasswordNotFound);

  TEPasaDecoder = Class
  public
    class Function TryDecodeEPASA(AAccount : Cardinal; const APayload : TOperationPayload; const ANode : TNode; const AWalletKeys : TWalletKeys; const APasswords : TList<String>;
      out ADecodeEPasaResult : TDecodeEPasaResult; out AEPasa : TEPasa) : Boolean; overload;
    class Function TryDecodeEPASA(AAccount : Cardinal; const APayload : TOperationPayload; const ANode : TNode; const AWalletKeys : TWalletKeys; const APasswords : TList<String>;
      out AEPasa : TEPasa) : Boolean; overload;
    class Function DecodeEPASA(AAccount : Cardinal; const APayload : TOperationPayload; const ANode : TNode; const AWalletKeys : TWalletKeys; const APasswords : TList<String>) : String; overload;
    class function CheckEPasa(const ASender : TRPCProcess; const AAccount_EPasa : String; AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean; overload;
    class function CheckEPasa(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean; overload;
    class function ValidateEPasa(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
  End;

implementation

uses UPCEncryption, UCommon, UCrypto;

{ TEPasaDecoder }

class function TEPasaDecoder.TryDecodeEPASA(AAccount: Cardinal;
  const APayload: TOperationPayload; const ANode : TNode; const AWalletKeys: TWalletKeys;
  const APasswords: TList<String>; out ADecodeEPasaResult: TDecodeEPasaResult;
  out AEPasa: TEPasa): Boolean;
var
  LUnencryptedPayloadBytes, LPwd : TBytes;
  LDone : Boolean;
  i : Integer;
  LAccount : TAccount;
begin
  LUnencryptedPayloadBytes := Nil;
  AEPasa.Clear;
  Result := False;
  ADecodeEPasaResult := der_Decoded;
  AEPasa.Account := AAccount;
  AEPasa.AccountChecksum := TEPasa.CalculateAccountChecksum(AAccount);
  AEPasa.PayloadType := TEPasaComp.FromProtocolValue(APayload.payload_type);
  if AEPasa.PayloadType.HasTrait(ptNonDeterministic) then begin
    ADecodeEPasaResult := der_NonDeterministic;
    Exit(False);
  end;
  if Not AEPasa.PayloadType.IsValid then begin
    ADecodeEPasaResult := der_InvalidPayloadType;
    Exit(False);
  end;

  if AEPasa.PayloadType.HasTrait(ptAddressedByName) then begin
    if (AEPasa.PayloadType.HasTrait(ptPublic) and
       AEPasa.PayloadType.HasTrait(ptBase58Formatted)) then begin
      // PayToKey candidate...
      AEPasa.AccountName := '@';
    end else begin
      if Assigned(ANode) then begin
        LAccount := ANode.GetMempoolAccount(AAccount);
        AEPasa.AccountName := LAccount.name.ToPrintable;
      end;
      if AEPasa.AccountName='' then begin
        ADecodeEPasaResult := der_AccountNameNotFound; // Will continue processing
      end;
    end;
  end;

  // payload data
  if (Length(APayload.payload_raw)=0) then begin
    // Nothing to decode...
  end else if (AEPasa.PayloadType.HasTrait(ptSenderKeyEncrypted)) or (AEPasa.PayloadType.HasTrait(ptRecipientKeyEncrypted)) then begin
    if Assigned(AWalletKeys) then begin
      LDone := False;
      i := 0;
      while (Not LDone) and (i < AWalletKeys.Count) do begin
        if Assigned(AWalletKeys.Key[i].PrivateKey) then begin
          if TPCEncryption.DoPascalCoinECIESDecrypt(AWalletKeys.Key[i].PrivateKey.PrivateKey,APayload.payload_raw,LUnencryptedPayloadBytes) then begin
            LDone := True;
          end;
        end;
        inc(i);
      end;
      if Not LDone then begin
        ADecodeEPasaResult := der_PrivateKeyNotFound;
        Exit(False);
      end;
    end else begin
      ADecodeEPasaResult := der_NotEnoughData;
      Exit(False);
    end;
  end else if (AEPasa.PayloadType.HasTrait(ptPasswordEncrypted)) then begin
    if Assigned(APasswords) then begin
      LDone := False;
      i := 0;
      while (Not LDone) and (i < APasswords.Count) do begin
        LPwd.FromString(APasswords[i]);
        if TPCEncryption.DoPascalCoinAESDecrypt(APayload.payload_raw,LPwd,LUnencryptedPayloadBytes) then begin
          AEPasa.Password := APasswords[i];
          LDone := True;
        end;
        inc(i);
      end;
      if Not LDone then begin
        ADecodeEPasaResult := der_PasswordNotFound;
        Exit(False);
      end;
    end else begin
      ADecodeEPasaResult := der_NotEnoughData;
      Exit(False);
    end;
  end else begin
    if (Not AEPasa.PayloadType.HasTrait(ptPublic)) then begin
      // Internal Error
      ADecodeEPasaResult := der_Undefined;
      Exit(False);
    end;
    LUnencryptedPayloadBytes := APayload.payload_raw;
  end;
  // LUnencryptedPayloadBytes Has Value in RAW
  if (AEPasa.PayloadType.HasTrait(ptAsciiFormatted)) then begin
    AEPasa.Payload := LUnencryptedPayloadBytes.ToString;
  end else if (AEPasa.PayloadType.HasTrait(ptHexFormatted)) then begin
    AEPasa.Payload := THexEncoding.Encode(LUnencryptedPayloadBytes,True);
  end else if (AEPasa.PayloadType.HasTrait(ptBase58Formatted)) then begin
    AEPasa.Payload := TPascalBase58Encoding.Encode(LUnencryptedPayloadBytes);
  end else begin
    // Internal error
    ADecodeEPasaResult := der_Undefined;
    Exit(False);
  end;
  Result := true;
end;

class function TEPasaDecoder.CheckEPasa(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;
begin
  Result := CheckEPAsa(ASender,AInputParams.AsString('account_epasa',''),AJSONResponse,AErrorNum,AErrorDesc);
end;

class function TEPasaDecoder.CheckEPasa(const ASender: TRPCProcess;
  const AAccount_EPasa: String; AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;
var LEPasa : TEPasa;
begin
  if Not TEPasa.TryParse(AAccount_EPasa,LEPasa) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidEPASA;
    AErrorDesc := 'Not a valid epasa: '+AAccount_EPasa;
    Result := False;
    Exit(False);
  end else begin
    Result := True;
    AJSONResponse.GetAsVariant('account_epasa').Value := LEPasa.ToClassicPASAString;
    AJSONResponse.GetAsVariant('account_epasa_checksum').Value := LEPasa.ToString(False);

    if LEPasa.PayloadType.HasTrait(ptAddressedByName) then begin
      AJSONResponse.GetAsVariant('account').Value := LEPasa.AccountName;
    end else begin
      AJSONResponse.GetAsVariant('account').Value := LEPasa.Account.Value;
    end;

    if LEPasa.PayloadType.HasTrait(ptPublic) then begin
      AJSONResponse.GetAsVariant('payload_method').Value := 'none';
    end else if LEPasa.PayloadType.HasTrait(ptSenderKeyEncrypted) then begin
      AJSONResponse.GetAsVariant('payload_method').Value := 'sender';
    end else if LEPasa.PayloadType.HasTrait(ptRecipientKeyEncrypted) then begin
      AJSONResponse.GetAsVariant('payload_method').Value := 'dest';
    end else if LEPasa.PayloadType.HasTrait(ptPasswordEncrypted) then begin
      AJSONResponse.GetAsVariant('payload_method').Value := 'aes';
      AJSONResponse.GetAsVariant('pwd').Value := LEPasa.Password;
    end;

    if LEPasa.PayloadType.HasTrait(ptAsciiFormatted) then begin
      AJSONResponse.GetAsVariant('payload_encode').Value := 'string';
    end else if LEPasa.PayloadType.HasTrait(ptHexFormatted) then begin
      AJSONResponse.GetAsVariant('payload_encode').Value := 'hexa';
    end else if LEPasa.PayloadType.HasTrait(ptBase58Formatted) then begin
      AJSONResponse.GetAsVariant('payload_encode').Value := 'base58';
    end;

    AJSONResponse.GetAsVariant('payload').Value := LEPasa.GetRawPayloadBytes.ToHexaString;
    AJSONResponse.GetAsVariant('payload_type').Value := LEPasa.PayloadType.ToProtocolValue;
  end;
end;

class function TEPasaDecoder.DecodeEPASA(AAccount: Cardinal;
  const APayload: TOperationPayload; const ANode: TNode;
  const AWalletKeys: TWalletKeys; const APasswords: TList<String>): String;
var LEPasa : TEPasa;
begin
  if TryDecodeEPASA(AAccount,APayload,ANode,AWalletKeys,APasswords,LEPasa) then begin
    Result := LEPasa.ToClassicPASAString;
  end else Result := '';
end;

class function TEPasaDecoder.TryDecodeEPASA(AAccount: Cardinal;
  const APayload: TOperationPayload; const ANode : TNode; const AWalletKeys: TWalletKeys;
  const APasswords: TList<String>; out AEPasa: TEPasa): Boolean;
var LDecodeEPasaResult: TDecodeEPasaResult;
begin
  Result := TryDecodeEPASA(AAccount,APayload,ANode,AWalletKeys,APasswords,LDecodeEPasaResult,AEPasa);
end;

class function TEPasaDecoder.ValidateEPasa(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;
var
  s : String;
  card : Cardinal;
  LEPasaStr, LDelimStart,LDelimEnd, LPwdZone, LPayload : String;
  LRawPayload : TRawBytes;
begin
  LEPasaStr := '';
  LPwdZone := '';
  LEPasaStr := AInputParams.AsString('account','');
  s := Trim(AInputParams.AsString('payload_method','none'));
  if s='dest' then begin
    LDelimStart := '(';
    LDelimEnd := ')';
  end else if s='sender' then begin
    LDelimStart := '<';
    LDelimEnd := '>';
  end else if s='aes' then begin
    LDelimStart := '{';
    LDelimEnd := '}';
    LPwdZone := ':' + AInputParams.AsString('pwd','');
  end else if (s='none') or (trim(s)='') then begin
    LDelimStart := '[';
    LDelimEnd := ']';
  end else begin
    AErrorNum := CT_RPC_ErrNum_InvalidData;
    AErrorDesc := Format('"payload_method" %s not valid',[s]);
    Exit(False);
  end;
  s := Trim(AInputParams.AsString('payload',''));
  if Not TCrypto.HexaToRaw(s,LRawPayload) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidData;
    AErrorDesc := Format('"payload" is not an HEXASTRING: %s',[s]);
    Exit(False);
  end;
  s := Trim(AInputParams.AsString('payload_encode','string'));
  if s='hexa' then begin
    LPayload := '0x'+LRawPayload.ToHexaString;
  end else if s='base58' then begin
    LPayload := TPascalBase58Encoding.Encode(LRawPayload);
  end else if (s='string') or (Trim(s)='') then begin
    LPayload := '"'+TPascalAsciiEncoding.Escape(LRawPayload.ToString)+'"';
  end else begin
    AErrorNum := CT_RPC_ErrNum_InvalidData;
    AErrorDesc := Format('"payload_encode" %s not valid',[s]);
    Exit(False);
  end;

  LEPasaStr := AInputParams.AsString('account','') + LDelimStart + LPayload + LPwdZone + LDelimEnd;
  Result := CheckEPasa(ASender,LEPasaStr,AJSONResponse,AErrorNum,AErrorDesc);
end;

initialization
  TRPCProcess.RegisterProcessMethod('validateepasa',TEPasaDecoder.ValidateEPasa);
  TRPCProcess.RegisterProcessMethod('checkepasa',TEPasaDecoder.CheckEPasa);
finalization
  TRPCProcess.UnregisterProcessMethod('validateepasa');
  TRPCProcess.UnregisterProcessMethod('checkepasa');
end.
