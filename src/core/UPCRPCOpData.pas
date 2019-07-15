unit UPCRPCOpData;

{ Copyright (c) 2019 by Albert Molina

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

{$I config.inc}

Uses classes, SysUtils,
  UJSONFunctions, UAccounts, UBaseTypes, UOpTransaction, UConst,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  URPC, UCrypto, UWallet, UBlockChain;


Type
  TRPCOpData = Class
  private
    class Function CreateOpData(ARPCProcess : TRPCProcess; ACurrent_protocol : Word;
       AInputParams : TPCJSONObject;
       const AAccountSignerPublicKey : TAccountKey;
       AAccountSigner, AAccountSender, AAccountTarget, ASender_last_n_operation : Cardinal;
       Const ARawPayload : TRawBytes;
       var AErrorNum: Integer; var AErrorDesc: String; var AOpData : TOpData): Boolean;
  public
    class function OpData_SendOpData(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
    class function OpData_SignOpData(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
    class function OpData_FindOpDataOperations(const ASender : TRPCProcess; const AMethodName : String; AInputParams, AJSONResponse : TPCJSONObject; var AErrorNum : Integer; var AErrorDesc : String) : Boolean;
  End;

implementation

{ TRPCOpData }

class function TRPCOpData.CreateOpData(ARPCProcess: TRPCProcess;
  ACurrent_protocol: Word;
  AInputParams : TPCJSONObject;
  const AAccountSignerPublicKey : TAccountKey;
  AAccountSigner, AAccountSender, AAccountTarget, ASender_last_n_operation: Cardinal;
  const ARawPayload: TRawBytes;
  var AErrorNum: Integer; var AErrorDesc: String; var AOpData : TOpData): Boolean;
var LSignerKey : TECPrivateKey;
  LGUID : TGUID;
  LStringGuid : String;
begin
  Result := False;
  AOpData := Nil;
  if (Not ARPCProcess.RPCServer.AllowUsePrivateKeys) then begin
    // Protection when server is locked to avoid private keys call
    AErrorNum := CT_RPC_ErrNum_NotAllowedCall;
    Exit;
  end;
  If Not ARPCProcess.RPCServer.WalletKeys.IsValidPassword then begin
    AErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
    AErrorDesc := 'Wallet is password protected. Unlock first';
    Exit;
  end;
  if Not ARPCProcess.RPCServer.CheckAndGetPrivateKeyInWallet(AAccountSignerPublicKey,LSignerKey,AErrorNum,AErrorDesc) then Exit;

  LStringGuid := AInputParams.AsString('guid','');
  if LStringGuid<>'' then begin
    try
      LGUID := StringToGUID(LStringGuid);
    except
      On E:Exception do begin
        AErrorNum := CT_RPC_ErrNum_InvalidData;
        AErrordesc := 'Invalid "guid" value '+E.Message;
        Exit;
      end;
    end;
  end else LGUID := CT_TOpDataData_NUL.guid;

  AOpData := TOpData.CreateOpData(ACurrent_protocol,
    AAccountSigner,
    AAccountSender,
    AAccountTarget,
    LSignerKey,
    ASender_last_n_operation+1,
    AInputParams.AsInteger('data_type',0),
    AInputParams.AsInteger('data_sequence',0),
    LGUID,
    TPascalCoinJSONComp.ToPascalCoins(AInputParams.AsDouble('amount',0)),
    TPascalCoinJSONComp.ToPascalCoins(AInputParams.AsDouble('fee',0)),
    ARawPayload);
  Result := True;
end;

class function TRPCOpData.OpData_FindOpDataOperations(
  const ASender: TRPCProcess; const AMethodName: String; AInputParams,
  AJSONResponse: TPCJSONObject; var AErrorNum: Integer;
  var AErrorDesc: String): Boolean;
begin
  // TODO TODO TODO TODO
  // TODO TODO TODO TODO
  // TODO TODO TODO TODO
  // TODO TODO TODO TODO
  // TODO TODO TODO TODO
  AErrorNum := CT_RPC_ErrNum_NotImplemented;
  AErrorDesc := 'This method is not implemented. PENDING TODO';
  Result := False;
end;

class function TRPCOpData.OpData_SendOpData(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;
var LOpData : TOpData;
  LSigner, LSender, LTarget : TAccount;
  LEncodedRawPayload : TRawBytes;
  LErrors : String;
  LOPR : TOperationResume;
begin
  Result := False;
  if Not ASender.RPCServer.GetMempoolAccount(AInputParams.AsInteger('sender',CT_MaxAccount),LSender) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidAccount;
    AErrorDesc := 'Invalid "sender"';
    Exit;
  end;
  if (AInputParams.IndexOfName('signer')>=0) then begin
    if Not ASender.RPCServer.GetMempoolAccount(AInputParams.AsInteger('signer',CT_MaxAccount),LSigner) then begin
      AErrorNum := CT_RPC_ErrNum_InvalidAccount;
      AErrorDesc := 'Invalid "signer"';
      Exit;
    end;
  end else LSigner := LSender; // If no "signer" param, then use "sender" as signer by default
  if Not ASender.RPCServer.GetMempoolAccount(AInputParams.AsInteger('target',CT_MaxAccount),LTarget) then begin
    AErrorNum := CT_RPC_ErrNum_InvalidAccount;
    AErrorDesc := 'Invalid "target"';
    Exit;
  end;

  if not TPascalCoinJSONComp.CheckAndGetEncodedRAWPayload(
    TCrypto.HexaToRaw(AInputParams.AsString('payload','')),
    AInputParams.AsString('payload_method','none'),
    AInputParams.AsString('pwd',''),
    LSender.accountInfo.accountKey,
    LTarget.accountInfo.accountKey,
    LEncodedRawPayload,AErrorNum,AErrorDesc) Then Exit;


  if Not CreateOpData(ASender,
    ASender.Node.Bank.SafeBox.CurrentProtocol,
    AInputParams,
    LSigner.accountInfo.accountKey,
    LSigner.account,
    LSender.account,
    LTarget.account,
    LSigner.n_operation,
    LEncodedRawPayload,
    AErrorNum, AErrorDesc, LOpData) then Exit;

  if LOpData=nil then Exit;
  try
    If not ASender.Node.AddOperation(Nil,LOpData,LErrors) then begin
      AErrorDesc := 'Error adding operation: '+LErrors;
      AErrorNum := CT_RPC_ErrNum_InvalidOperation;
      Exit;
    end;
    TPCOperation.OperationToOperationResume(0,LOpData,False,LSender.account,LOPR);
    TPascalCoinJSONComp.FillOperationObject(LOPR,ASender.Node.Bank.BlocksCount,AJSONResponse.GetAsObject('result'));
    Result := True;
  finally
    LOpData.free;
  end;
end;

class function TRPCOpData.OpData_SignOpData(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;
var LOpData : TOpData;
  LEncodedRawPayload : TRawBytes;
  LErrors : String;
  LOPR : TOperationResume;
  LEncodePayloadType : String;
  LPayloadPubkey, LSignerPubkey : TAccountKey;
  LOperationsHashTree : TOperationsHashTree;
  LSigner : Cardinal;
begin
  Result := False;

  if Not TPascalCoinJSONComp.HexaStringToOperationsHashTree(AInputParams.AsString('rawoperations',''),LOperationsHashTree,LErrors) then begin
    AErrorNum:=CT_RPC_ErrNum_InvalidData;
    AErrorDesc:= 'Error decoding param "rawoperations": '+LErrors;
    Exit;
  end;
  try

    LEncodePayloadType := AInputParams.AsString('payload_method','none');
    if (LEncodePayloadType='sender') then begin
      if Not TPascalCoinJSONComp.CapturePubKey(AInputParams,'sender_',LPayloadPubkey,AErrorDesc) then begin
        AErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
    end else if (LEncodePayloadType='dest') then begin
      if Not TPascalCoinJSONComp.CapturePubKey(AInputParams,'target_',LPayloadPubkey,AErrorDesc) then begin
        AErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
    end;

    if Not TPascalCoinJSONComp.CapturePubKey(AInputParams,'signer_',LSignerPubkey,AErrorDesc) then begin
      AErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      Exit;
    end;

    if not TPascalCoinJSONComp.CheckAndGetEncodedRAWPayload(
      TCrypto.HexaToRaw(AInputParams.AsString('payload','')),
      AInputParams.AsString('payload_method','dest'),
      AInputParams.AsString('pwd',''),
      LPayloadPubkey,
      LPayloadPubkey,
      LEncodedRawPayload,AErrorNum,AErrorDesc) then Exit;

    if Not CreateOpData(ASender,
      AInputParams.AsInteger('protocol',CT_BUILD_PROTOCOL),
      AInputParams,
      LSignerPubkey,
      AInputParams.AsCardinal('signer',CT_MaxAccount),
      AInputParams.AsCardinal('sender',CT_MaxAccount),
      AInputParams.AsCardinal('target',CT_MaxAccount),
      AInputParams.AsCardinal('last_n_operation',0),
      LEncodedRawPayload,
      AErrorNum, AErrorDesc, LOpData) then Exit;
    if LOpData=nil then Exit;
    try
      LOperationsHashTree.AddOperationToHashTree(LOpData);
      TPascalCoinJSONComp.FillOperationsHashTreeObject(LOperationsHashTree,AJSONResponse.GetAsObject('result'));
      Result := true;
    finally
      LOpData.Free;
    end;
  finally
    LOperationsHashTree.Free;
  end;
end;

initialization
  TRPCProcess.RegisterProcessMethod('senddata',TRPCOpData.OpData_SendOpData);
  TRPCProcess.RegisterProcessMethod('signdata',TRPCOpData.OpData_SignOpData);
  TRPCProcess.RegisterProcessMethod('finddataoperations',TRPCOpData.OpData_FindOpDataOperations);
finalization
  TRPCProcess.UnregisterProcessMethod('senddata');
  TRPCProcess.UnregisterProcessMethod('signdata');
  TRPCProcess.UnregisterProcessMethod('finddataoperations');
end.
