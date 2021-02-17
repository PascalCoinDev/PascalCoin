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
  UBlockChain, UNode, UBaseTypes,
  UAccounts,
  UEncoding,
  UEPasa,
  UWallet;

type
  TDecodeEPasaResult = (der_Decoded, der_Undefined, der_NonDeterministic, der_InvalidPayloadType, der_AccountNameNotFound, der_NotEnoughData, der_PrivateKeyNotFound, der_PasswordNotFound);

  TEPasaDecoder = Class
  public
    class Function TryDecodeEPASA(AAccount : Cardinal; const APayload : TOperationPayload; const ANode : TNode; const AWalletKeys : TWalletKeys; const APasswords : TList<String>;
      out ADecodeEPasaResult : TDecodeEPasaResult; out AEPasa : TEPasa) : Boolean; overload;
    class Function TryDecodeEPASA(AAccount : Cardinal; const APayload : TOperationPayload; const ANode : TNode; const AWalletKeys : TWalletKeys; const APasswords : TList<String>;
      out AEPasa : TEPasa) : Boolean; overload;
  End;

implementation

uses UPCEncryption, UCommon;

{ TEPasaDecoder }

class function TEPasaDecoder.TryDecodeEPASA(AAccount: Cardinal;
  const APayload: TOperationPayload; const ANode : TNode; const AWalletKeys: TWalletKeys;
  const APasswords: TList<String>; out ADecodeEPasaResult: TDecodeEPasaResult;
  out AEPasa: TEPasa): Boolean;
var
  LUnencryptedPayloadBytes, LPwd : TBytes;
  LDone : Boolean;
  i : Integer;
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
        AEPasa.AccountName := ANode.GetMempoolAccount(AAccount).name.ToPrintable;
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
    AEPasa.Payload := THexEncoding.Encode(LUnencryptedPayloadBytes,False);
  end else if (AEPasa.PayloadType.HasTrait(ptBase58Formatted)) then begin
    AEPasa.Payload := TPascalBase58Encoding.Encode(LUnencryptedPayloadBytes);
  end else begin
    // Internal error
    ADecodeEPasaResult := der_Undefined;
    Exit(False);
  end;
  Result := true;
end;

class function TEPasaDecoder.TryDecodeEPASA(AAccount: Cardinal;
  const APayload: TOperationPayload; const ANode : TNode; const AWalletKeys: TWalletKeys;
  const APasswords: TList<String>; out AEPasa: TEPasa): Boolean;
var LDecodeEPasaResult: TDecodeEPasaResult;
begin
  Result := TryDecodeEPASA(AAccount,APayload,ANode,AWalletKeys,APasswords,LDecodeEPasaResult,AEPasa);
end;

end.
