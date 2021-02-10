unit UPCRPCSend;

{ Copyright (c) 2021 by PascalCoin developers, orignal code by Albert Molina

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

interface

{$I ./../config.inc}

Uses classes, SysUtils,
  UJSONFunctions, UAccounts, UBaseTypes, UOpTransaction, UConst,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  URPC, UCrypto, UWallet, UBlockChain, UEPasa, ULog, UPCOrderedLists, UPCDataTypes;


Type
  TRPCSend = Class
  private
  public
    class function CreateOperationTransaction(const ARPCProcess : TRPCProcess; ACurrentProtocol : Word; ASender, ATarget : TAccount; AAmount, AFee : UInt64; const ARawPayload : TRawBytes; const APayloadMethod, AEncodePwd : String; const APayloadType : TPayloadType; var AErrorNum: Integer; var AErrorDesc: String) : TOpTransaction;
    class function CreatePayToKeyTransaction(const ARPCProcess : TRPCProcess; ACurrentProtocol: Word; ASender, APurchaseAccount : TAccount; const ANewKey : TAccountKey; AAmount, AFee: UInt64; const ARawPayload: TRawBytes;  const APayloadMethod, AEncodePwd: String; const APayloadType : TPayloadType; var AErrorNum: Integer; var AErrorDesc: String) : TOpTransaction;
    class function SendTo(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
    class function SignSendTo(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
  End;

implementation

{ TRPCSend }

class function TRPCSend.CreateOperationTransaction(const ARPCProcess : TRPCProcess; ACurrentProtocol : Word; ASender, ATarget : TAccount; AAmount, AFee : UInt64; const ARawPayload : TRawBytes; const APayloadMethod, AEncodePwd : String; const APayloadType : TPayloadType; var AErrorNum: Integer; var AErrorDesc: String): TOpTransaction;
var
  LOpPayload : TOperationPayload;
  LPrivateKey : TECPrivateKey;
Begin
  Result := Nil;
  if Not ARPCProcess.RPCServer.CheckAndGetPrivateKeyInWallet(ASender.accountInfo.accountKey, LPrivateKey, AErrorNum, AErrorDesc) then Exit(Nil);
  if Not TPascalCoinJSONComp.CheckAndGetEncodedRAWPayload(ARawPayload, APayloadType, APayloadMethod, AEncodePwd, ASender.accountInfo.accountKey, ATarget.accountInfo.accountKey, LOpPayload, AErrorNum, AErrorDesc) then Exit(Nil);
  Result := TOpTransaction.CreateTransaction(ACurrentProtocol, ASender.account, ASender.n_operation+1, ATarget.account, LPrivateKey, AAmount, AFee, LOpPayload);
  if Not Result.HasValidSignature then begin
    FreeAndNil(Result);
    AErrorNum:=CT_RPC_ErrNum_InternalError;
    AErrorDesc:='Invalid signature';
    exit;
  end;
end;

class function TRPCSend.CreatePayToKeyTransaction(const ARPCProcess : TRPCProcess; ACurrentProtocol: Word; ASender, APurchaseAccount : TAccount; const ANewKey : TAccountKey; AAmount, AFee: UInt64; const ARawPayload: TRawBytes;  const APayloadMethod, AEncodePwd: String; const APayloadType : TPayloadType; var AErrorNum: Integer; var AErrorDesc: String): TOpTransaction;
Var
  LOpPayload : TOperationPayload;
  LPrivateKey : TECPrivateKey;
Begin
  Result := Nil;
  if (AAmount < APurchaseAccount.accountInfo.price) then begin
    AErrorNum := CT_RPC_ErrNum_InternalError;
    AErrorDesc := 'Insufficient funds to purchase account for pay-to-key transaction';
    Exit(Nil);
  end;

  if Not ARPCProcess.RPCServer.CheckAndGetPrivateKeyInWallet(ASender.accountInfo.accountKey, LPrivateKey, AErrorNum, AErrorDesc) then Exit(Nil);
  if Not TPascalCoinJSONComp.CheckAndGetEncodedRAWPayload(ARawPayload, APayloadType, APayloadMethod, AEncodePwd, ASender.accountInfo.accountKey, ANewKey, LOpPayload, AErrorNum, AErrorDesc) then Exit(Nil);
  Result := TOpBuyAccount.CreateBuy(ACurrentProtocol, ASender.account, ASender.n_operation + 1, APurchaseAccount.account, APurchaseAccount.accountInfo.account_to_pay, APurchaseAccount.accountInfo.price, AAmount, AFee, ANewKey, LPrivateKey, LOpPayload);
  if Not Result.HasValidSignature then begin
    FreeAndNil(Result);
    AErrorNum:=CT_RPC_ErrNum_InternalError;
    AErrorDesc:='Invalid signature';
    exit;
  end;
end;

class function TRPCSend.SendTo(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;

{ JSON-RPC "sendto"

### sendto
Executes a transaction operation from "sender" to "target"

##### Params
- `sender` : Integer - Sender account
- `target` : Integer - Destination account
- `amount` : PASCURRENCY - Coins to be transferred
- `fee` : PASCURRENCY - Fee of the operation
- `payload` : HEXASTRING - Payload "item" that will be included in this operation
- `payload_method` : String - Encode type of the item payload
  - `none` : Not encoded. Will be visible for everybody
  - `dest` (default) : Using Public key of "target" account. Only "target" will be able to decrypt this payload
  - `sender` : Using sender Public key. Only "sender" will be able to decrypt this payload
  - `aes` : Encrypted data using `pwd` param
- `pwd` : String - Used to encrypt payload with `aes` as a `payload_method`. If none equals to empty password

##### Result
If transaction is successfull will return a JSON Object in "[Operation Object]" format.
Otherwise, will return a JSON-RPC error code with description

}

var
 LSender, LTarget : TAccount;
 LTargetEPASA : TEPasa;
 LTargetKey : TAccountKey;
 LTargetRequiresPurchase : Boolean;
 LAmount, LFee : UInt64;
 LRawPayload : TRawBytes;
 LPayload_method, LEncodePwd, LErrors : String;
 LOpt : TOpTransaction;
 LOpr : TOperationResume;
begin
  // Get Parameters
  Result := False;

  if (Not ASender.RPCServer.AllowUsePrivateKeys) then begin
    // Protection when server is locked to avoid private keys call
    AErrorNum := CT_RPC_ErrNum_NotAllowedCall;
    Exit;
  end;
  If Not ASender.RPCServer.WalletKeys.IsValidPassword then begin
    AErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
    AErrorDesc := 'Wallet is password protected. Unlock first';
    exit;
  end;

  if Not TPascalCoinJSONComp.CaptureAccountNumber(AInputParams,'sender',ASender.Node,LSender.account,AErrorDesc) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidAccount;
    Exit;
  end else LSender := ASender.Node.GetMempoolAccount(LSender.account);

  LTarget := CT_Account_NUL;
  if Not TPascalCoinJSONComp.CaptureEPASA(AInputParams,'target',ASender.Node, LTargetEPASA, LTarget.account, LTargetKey, LTargetRequiresPurchase, AErrorDesc) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidAccount;
    Exit;
  end else LTarget := ASender.Node.GetMempoolAccount(LTarget.account);

  if Not TPascalCoinJSONComp.OverridePayloadParams(AInputParams, LTargetEPASA) then begin
    AErrorNum := CT_RPC_ErrNum_AmbiguousPayload;
    AErrorDesc := 'Target EPASA payload conflicts with argument payload.';
    Exit;
  end;

  LAmount := TPascalCoinJSONComp.ToPascalCoins(AInputParams.AsDouble('amount',0));
  LFee := TPascalCoinJSONComp.ToPascalCoins(AInputParams.AsDouble('fee',0));
  LRawPayload := TCrypto.HexaToRaw(AInputParams.AsString('payload',''));
  LPayload_method := AInputParams.AsString('payload_method','dest');
  LEncodePwd := AInputParams.AsString('pwd','');

  // Do new operation
  ASender.Node.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent sends
  try
    // Create operation
    if LTargetRequiresPurchase then begin
      // Buy Account
      LOpt := CreatePayToKeyTransaction(
          ASender, ASender.Node.Bank.SafeBox.CurrentProtocol,
          LSender, LTarget, LTargetKey, LAmount, LFee,
          LRawPayload, LPayload_method, LEncodePwd, LTargetEPASA.PayloadType,
          AErrorNum, AErrorDesc);
    end else begin
      // Transaction
      LOpt := CreateOperationTransaction(
        ASender, ASender.Node.Bank.SafeBox.CurrentProtocol,
        LSender, LTarget, LAmount, LFee,
        LRawPayload, LPayload_method, LEncodePwd, LTargetEPASA.PayloadType,
        AErrorNum, AErrorDesc);
    end;
    // Execute operation
    if Assigned(LOpt) then
    try
      If not ASender.Node.AddOperation(Nil,LOpt,LErrors) then begin
        AErrorDesc := 'Error adding operation: '+LErrors;
        AErrorNum := CT_RPC_ErrNum_InvalidOperation;
        Exit;
      end;
      TPCOperation.OperationToOperationResume(0,LOpt,False,LSender.account,LOpr);
      TPascalCoinJSONComp.FillOperationObject(LOpr,ASender.Node.Bank.BlocksCount,AJSONResponse.GetAsObject('result'));
      Result := true;
    finally
      LOpt.free;
    end;
  finally
    ASender.Node.OperationSequenceLock.Release;
  end;
end;

class function TRPCSend.SignSendTo(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;

{ JSON-RPC "signsendto"

### signsendto

Creates and signs a "Send to" operation without checking information and without transfering to the network.
It's usefull for "cold wallets" that are off-line (not synchronized with the network) and only holds private keys

##### Params
- `rawoperations` : HEXASTRING (optional) - If we want to add a sign operation with other previous operations, here we must put previous `rawoperations` result
- `sender` : Integer - Sender account
- `target` : Integer - Target account
- `sender_enc_pubkey` or `sender_b58_pubkey` : HEXASTRING - Public key (in encoded format or b58 format) of the sender account
- `target_enc_pubkey` or `target_b58_pubkey` : HEXASTRING - Public key (in encoded format or b58 format) of the target account
- `last_n_operation` : Last value of `n_operation` obtained with an [Account object](#account-object), for example when called to [getaccount](#getaccount)
- `amount`,`fee`,`payload`,`payload_method`,`pwd` : Same values that calling [sendto](#sendto)

##### Result

Wallet must be unlocked and sender private key (searched with provided public key) must be in wallet.
No other checks are made (no checks for valid target, valid n_operation, valid amount or fee ...)
Returns a [Raw Operations Object](#raw-operations-object)

}
var
 LSender, LTarget : TAccount;
 LTargetEPASA : TEPasa;
 LTargetKey : TAccountKey;
 LTargetRequiresPurchase : Boolean;
 LHexaStringOperationsHashTree, LErrors : String;
 LProtocol : Integer;
 LOperationsHashTree : TOperationsHashTree;
 LOpt : TOpTransaction;
 LOpr : TOperationResume;
 LRawPayload : TRawBytes;
 LPayload_method, LEncodePwd : String;
 LAmount, LFee : UInt64;
begin
  Result := False;

  if (Not ASender.RPCServer.AllowUsePrivateKeys) then begin
    // Protection when server is locked to avoid private keys call
    AErrorNum := CT_RPC_ErrNum_NotAllowedCall;
    Exit;
  end;
  If Not ASender.RPCServer.WalletKeys.IsValidPassword then begin
    AErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
    AErrorDesc := 'Wallet is password protected. Unlock first';
    exit;
  end;
  if Not TPascalCoinJSONComp.CaptureAccountNumber(AInputParams,'sender',Nil,LSender.account,AErrorDesc) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidAccount;
    Exit;
  end;
  if Not TPascalCoinJSONComp.CaptureNOperation(AInputParams,'last_n_operation',Nil,LSender.n_operation,AErrorDesc) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidAccount;
    Exit;
  end;

  if Not TPascalCoinJSONComp.CaptureEPASA(AInputParams,'target', nil, LTargetEPASA, LTarget.account, LTargetKey, LTargetRequiresPurchase, AErrorDesc) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidEPASA;
    Exit;
  end;

  if Not TPascalCoinJSONComp.OverridePayloadParams(AInputParams, LTargetEPASA) then begin
    AErrorNum := CT_RPC_ErrNum_AmbiguousPayload;
    Exit;
  end;

  If Not TPascalCoinJSONComp.CapturePubKey(AInputParams,'sender_',LSender.accountInfo.accountKey,AErrorDesc) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidPubKey;
    exit;
  end;

  If Not TPascalCoinJSONComp.CapturePubKey(AInputParams,'target_',LTarget.accountInfo.accountKey,AErrorDesc) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidPubKey;
    exit;
  end;

  LAmount := TPascalCoinJSONComp.ToPascalCoins(AInputParams.AsDouble('amount',0));
  LFee := TPascalCoinJSONComp.ToPascalCoins(AInputParams.AsDouble('fee',0));
  LRawPayload := TCrypto.HexaToRaw(AInputParams.AsString('payload',''));
  LPayload_method := AInputParams.AsString('payload_method','dest');
  LEncodePwd := AInputParams.AsString('pwd','');

  LHexaStringOperationsHashTree := AInputParams.AsString('rawoperations','');
  LProtocol := AInputParams.AsCardinal('protocol',CT_BUILD_PROTOCOL);

  if Not TPascalCoinJSONComp.HexaStringToOperationsHashTree(LHexaStringOperationsHashTree,LProtocol,LOperationsHashTree,LErrors) then begin
    AErrorNum:=CT_RPC_ErrNum_InvalidData;
    AErrorDesc:= 'Error decoding param "rawoperations": '+LErrors;
    Exit;
  end;


  Try
    // Create operation
    if LTargetRequiresPurchase then begin
      // Buy Account
      LOpt := CreatePayToKeyTransaction(
          ASender, ASender.Node.Bank.SafeBox.CurrentProtocol,
          LSender, LTarget, LTargetKey, LAmount, LFee,
          LRawPayload, LPayload_method, LEncodePwd, LTargetEPASA.PayloadType,
          AErrorNum, AErrorDesc);
    end else begin
      // Transaction
      LOpt := CreateOperationTransaction(
        ASender, ASender.Node.Bank.SafeBox.CurrentProtocol,
        LSender, LTarget, LAmount, LFee,
        LRawPayload, LPayload_method, LEncodePwd, LTargetEPASA.PayloadType,
        AErrorNum, AErrorDesc);
    end;

    // Execute operation
    if Assigned(LOpt) then
    try
      LOperationsHashTree.AddOperationToHashTree(LOpt);
      TPascalCoinJSONComp.FillOperationsHashTreeObject(LOperationsHashTree,AJSONResponse.GetAsObject('result'));
      Result := true;
    finally
      LOpt.Free;
    end;

  Finally
    LOperationsHashTree.Free;
  End;
end;

initialization
  TRPCProcess.RegisterProcessMethod('signsendto',TRPCSend.SignSendTo);
  TRPCProcess.RegisterProcessMethod('sendto',TRPCSend.SendTo);
finalization
  TRPCProcess.UnregisterProcessMethod('signsendto');
  TRPCProcess.UnregisterProcessMethod('sendto');
end.
