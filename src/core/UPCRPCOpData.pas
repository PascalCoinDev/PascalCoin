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

{$I ./../config.inc}

Uses classes, SysUtils,
  UJSONFunctions, UAccounts, UBaseTypes, UOpTransaction, UConst,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  URPC, UCrypto, UWallet, UBlockChain, ULog;


Type
  TRPCOpData = Class
  private
    class Function CreateOpData(ARPCProcess : TRPCProcess; ACurrent_protocol : Word;
       AInputParams : TPCJSONObject;
       const AAccountSignerPublicKey : TAccountKey;
       AAccountSigner, AAccountSender, AAccountTarget, ASender_last_n_operation : Cardinal;
       Const AOperationPayload : TOperationPayload;
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
  const AOperationPayload: TOperationPayload;
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
    AOperationPayload);
  Result := True;
end;

class function TRPCOpData.OpData_FindOpDataOperations(
  const ASender: TRPCProcess; const AMethodName: String; AInputParams,
  AJSONResponse: TPCJSONObject; var AErrorNum: Integer;
  var AErrorDesc: String): Boolean;

    Procedure DoFindFromBlock(ABlock_number : Integer;
      ASearchedAccount_number: Cardinal;
      AStartOperation, AEndOperation: Integer;
      AAct_depth : Integer; AFirst_Block_Is_Unknown : Boolean;
      ASearchBySender : Boolean; ASearchSender : Cardinal;
      ASearchByTarget : Boolean; ASearchTarget : Cardinal;
      ASearchByGUID : Boolean; ASearchGUID : TGUID;
      ASearchByDataSequence : Boolean; ASearchDataSequence : Word;
      ASearchByDataType : Boolean; ASearchDataType : Word;
      AOperationsResumeList : TOperationsResumeList
      );

      Function EqualGUIDs(const AGuid_A, AGuid_B : TGUID) : Boolean;
      var i : Integer;
      begin
        Result := (AGuid_A.D1 = AGuid_B.D1)
          and (AGuid_A.D2 = AGuid_B.D2)
          and (AGuid_A.D3 = AGuid_B.D3);
        i := Low(AGuid_A.D4);
        while (Result) and (i<=High(AGuid_A.D4)) do begin
          Result := (AGuid_A.D4[i] = AGuid_B.D4[i]);
          inc(i);
        end;
      end;

    var LOpComp : TPCOperationsComp;
      LOperation : TPCOperation;
      LOpData : TOpData;
      LOperationResume : TOperationResume;
      LList : TList<Cardinal>;
      i : Integer;
      LLast_block_number : Integer;
      LFound_in_block : Boolean;
      LFoundCounter : Integer;
    begin
      if (AAct_depth<=0) then exit;
      LFoundCounter := 0;
      LOpComp := TPCOperationsComp.Create(Nil);
      Try
        LList := TList<Cardinal>.Create;
        try
          LLast_block_number := ABlock_number+1;
          while (LLast_block_number>ABlock_number) And (AAct_depth>0)
            And (ABlock_number >= (ASearchedAccount_number DIV CT_AccountsPerBlock))
            And (LFoundCounter <= AEndOperation) do begin

            if Assigned(ASender) then begin
              // Threading protection
              if ASender.Terminated then Exit;
            end;
            LFound_in_block := False;
            LLast_block_number := ABlock_number;
            LList.Clear;
            If not ASender.Node.Bank.Storage.LoadBlockChainBlock(LOpComp,ABlock_number) then begin
              TLog.NewLog(ltdebug,ClassName,'Block '+inttostr(ABlock_number)+' not found. Cannot read operations');
              Exit;
            end;
            LFound_in_block := LOpComp.OperationsHashTree.GetOperationsAffectingAccount(ASearchedAccount_number,LList) > 0;
              // Reverse order:
            for i := LList.Count - 1 downto 0 do begin
              LOperation := LOpComp.Operation[LList.Items[i]];
              if LOperation is TOpData then begin
                //
                LOpData := TOpData( LOperation );
                // Search by filter:
                if ((Not ASearchBySender) Or (ASearchSender = LOpData.Data.account_sender))
                   and ((Not ASearchByTarget) Or (ASearchTarget = LOpData.Data.account_target))
                   and ((Not ASearchByGUID) Or (EqualGUIDs(ASearchGUID,LOpData.Data.guid)))
                   and ((Not ASearchByDataSequence) Or (ASearchDataSequence = LOpData.Data.dataSequence))
                   and ((Not ASearchByDataType) Or (ASearchDataType = LOpData.Data.dataType))
                then begin
                  if (LFoundCounter>=AStartOperation) And (LFoundCounter<=AEndOperation) then begin
                    If TPCOperation.OperationToOperationResume(ABlock_number,LOpData,False,LOpData.SignerAccount,LOperationResume) then begin
                      LOperationResume.Balance:=-1;
                      LOperationResume.NOpInsideBlock:=LList.Items[i];
                      LOperationResume.Block:=ABlock_number;
                      AOperationsResumeList.Add(LOperationResume);
                    end;
                  end;
                  inc(LFoundCounter);
                end;
              end;
            end; // For LList...
            //
            dec(AAct_depth);
            If (Not LFound_in_block) And (AFirst_Block_Is_Unknown) then begin
              Dec(ABlock_number);
            end else begin
              ABlock_number := LOpComp.PreviousUpdatedBlocks.GetPreviousUpdatedBlock(ASearchedAccount_number,ABlock_number);
            end;
            LOpComp.Clear(true);
          end;
        finally
          LList.Free;
        end;
      Finally
        LOpComp.Free;
      End;
    end;

Var LAccount : TAccount;
  LStartBlock : Cardinal;
  LMaxDepth : Integer;
  LSearchedAccount_number: Cardinal;
  LStartOperation, LMaxOperations: Integer;
  LFirst_Block_Is_Unknown : Boolean;
  LSearchBySender : Boolean;
  LSearchByTarget : Boolean;
  LSearchByGUID : Boolean; LSearchGUID : TGUID;
  LSearchByDataSequence : Boolean; LSearchDataSequence : Word;
  LSearchByDataType : Boolean; LSearchDataType : Word;
  LOperationsResumeList : TOperationsResumeList;
  LSender, LTarget : Cardinal;
  LResultArray : TPCJSONArray;
  i : Integer;
begin
  Result := False;

  LSender := AInputParams.AsCardinal('sender',CT_MaxAccount);
  LTarget := AInputParams.AsCardinal('target',CT_MaxAccount);
  LSearchedAccount_number := CT_MaxAccount;
  LSearchBySender := (LSender>=0) And (LSender<ASender.Node.Bank.AccountsCount);
  LSearchByTarget := (LTarget>=0) And (LTarget<ASender.Node.Bank.AccountsCount);
  if (LSearchBySender) then begin
    LSearchedAccount_number := LSender;
  end else if (LSearchByTarget) then begin
    LSearchedAccount_number := LTarget;
  end else begin
    AErrorNum := CT_RPC_ErrNum_InvalidData;
    AErrorDesc := 'Must provide "sender" or "target" valid values';
    Exit;
  end;

  if AInputParams.IndexOfName('guid')>=0 then begin
    try
      LSearchGUID := StringToGUID( AInputParams.AsString('guid','') );
      LSearchByGUID := True;
    except
      on E:Exception do begin
        AErrorNum := CT_RPC_ErrNum_InvalidData;
        AErrorDesc := 'Invalid "guid" param '+E.Message;
        Exit;
      end;
    end;
  end else LSearchByGUID := False;

  if AInputParams.IndexOfName('data_sequence')>=0 then begin
    LSearchByDataSequence := True;
    LSearchDataSequence := AInputParams.AsInteger('data_sequence',0);
  end else LSearchByDataSequence := False;

  if AInputParams.IndexOfName('data_type')>=0 then begin
    LSearchByDataType := True;
    LSearchDataType := AInputParams.AsInteger('data_type',0);
  end else LSearchByDataType := False;

  LMaxDepth := AInputParams.AsInteger('depth',1000);
  LStartOperation := AInputParams.AsInteger('start',0);
  LMaxOperations := AInputParams.AsInteger('max',100);
  if AInputParams.IndexOfName('startblock')>=0 then begin
    LStartBlock := AInputParams.AsInteger('startblock',100);
    LFirst_Block_Is_Unknown := True;
  end else begin
    if not ASender.RPCServer.GetMempoolAccount(LSearchedAccount_number,LAccount) then begin
      AErrorNum := CT_RPC_ErrNum_InvalidData;
      AErrorDesc := 'Invalid account';
      Exit;
    end;
    LFirst_Block_Is_Unknown := False;
    LStartBlock := LAccount.GetLastUpdatedBlock;
    if LStartBlock>=ASender.Node.Bank.BlocksCount then Dec(LStartBlock); // If its updated on mempool, don't look the mempool
  end;

  LOperationsResumeList := TOperationsResumeList.Create;
  try
    DoFindFromBlock(LStartBlock,
      LSearchedAccount_number,
      LStartOperation, LStartOperation + LMaxOperations,
      LMaxDepth, LFirst_Block_Is_Unknown,
      LSearchBySender, LSender,
      LSearchByTarget, LTarget,
      LSearchByGUID, LSearchGUID,
      LSearchByDataSequence, LSearchDataSequence,
      LSearchByDataType, LSearchDataType,
      LOperationsResumeList
      );
    //
    LResultArray := AJSONResponse.GetAsArray('result');

    for i := 0 to LOperationsResumeList.Count-1 do begin
      TPascalCoinJSONComp.FillOperationObject(LOperationsResumeList.OperationResume[i],ASender.Node.Bank.BlocksCount,LResultArray.GetAsObject( LResultArray.Count ));
    end;
    Result := True;
  finally
    LOperationsResumeList.Free;
  end;
end;

class function TRPCOpData.OpData_SendOpData(const ASender: TRPCProcess;
  const AMethodName: String; AInputParams, AJSONResponse: TPCJSONObject;
  var AErrorNum: Integer; var AErrorDesc: String): Boolean;
var LOpData : TOpData;
  LSigner, LSender, LTarget : TAccount;
  LOperationPayload : TOperationPayload;
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
    LOperationPayload,AErrorNum,AErrorDesc) Then Exit;


  if Not CreateOpData(ASender,
    ASender.Node.Bank.SafeBox.CurrentProtocol,
    AInputParams,
    LSigner.accountInfo.accountKey,
    LSigner.account,
    LSender.account,
    LTarget.account,
    LSigner.n_operation,
    LOperationPayload,
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
  LOperationPayload : TOperationPayload;
  LErrors : String;
  LOPR : TOperationResume;
  LEncodePayloadType : String;
  LPayloadPubkey, LSignerPubkey : TAccountKey;
  LOperationsHashTree : TOperationsHashTree;
  LSigner : Cardinal;
begin
  Result := False;

  if Not TPascalCoinJSONComp.HexaStringToOperationsHashTree(AInputParams.AsString('rawoperations',''),CT_BUILD_PROTOCOL,LOperationsHashTree,LErrors) then begin
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
      LOperationPayload,AErrorNum,AErrorDesc) then Exit;

    if Not CreateOpData(ASender,
      AInputParams.AsInteger('protocol',CT_BUILD_PROTOCOL),
      AInputParams,
      LSignerPubkey,
      AInputParams.AsCardinal('signer',CT_MaxAccount),
      AInputParams.AsCardinal('sender',CT_MaxAccount),
      AInputParams.AsCardinal('target',CT_MaxAccount),
      AInputParams.AsCardinal('last_n_operation',0),
      LOperationPayload,
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
