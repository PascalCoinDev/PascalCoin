unit URPC;

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

interface

Uses UThread, ULog, UConst, UNode, UAccounts, UCrypto, UBlockChain,
  UNetProtocol, UOpTransaction, UWallet, UTime, UAES, UECIES,
  UJSONFunctions, classes, blcksock, synsock, IniFiles, Variants, math;

Const
  CT_RPC_ErrNum_InternalError = 100;
  CT_RPC_ErrNum_NotImplemented = 101;

  CT_RPC_ErrNum_MethodNotFound = 1001;
  CT_RPC_ErrNum_InvalidAccount = 1002;
  CT_RPC_ErrNum_InvalidBlock = 1003;
  CT_RPC_ErrNum_InvalidOperation = 1004;
  CT_RPC_ErrNum_InvalidPubKey = 1005;
  CT_RPC_ErrNum_InvalidAccountName = 1006;
  CT_RPC_ErrNum_NotFound = 1010;
  CT_RPC_ErrNum_WalletPasswordProtected = 1015;
  CT_RPC_ErrNum_InvalidData = 1016;

Type

  { TRPCServer }

  TRPCServerThread = Class;
  TRPCServer = Class
  private
    FRPCServerThread : TRPCServerThread;
    FActive: Boolean;
    FWalletKeys: TWalletKeysExt;
    FPort: Word;
    FJSON20Strict: Boolean;
    FIniFileName: AnsiString;
    FIniFile : TIniFile;
    FRPCLog : TLog;
    FCallsCounter : Int64;
    FValidIPs: AnsiString;
    procedure SetActive(AValue: Boolean);
    procedure SetIniFileName(const Value: AnsiString);
    procedure SetLogFileName(const Value: AnsiString);
    Function GetLogFileName : AnsiString;
    procedure SetValidIPs(const Value: AnsiString);  protected
    Function IsValidClientIP(Const clientIp : String; clientPort : Word) : Boolean;
    Procedure AddRPCLog(Const Sender : String; Const Message : String);
    Function GetNewCallCounter : Int64;
  public
    Constructor Create;
    Destructor Destroy; override;
    Property Port : Word read FPort Write FPort;
    Property Active : Boolean read FActive write SetActive;
    Property WalletKeys : TWalletKeysExt read FWalletKeys write FWalletKeys;
    //
    Property JSON20Strict : Boolean read FJSON20Strict write FJSON20Strict;
    Property IniFileName : AnsiString read FIniFileName write SetIniFileName;
    Property LogFileName : AnsiString read GetLogFileName write SetLogFileName;
    Property ValidIPs : AnsiString read FValidIPs write SetValidIPs;
  end;

  { TRPCServerThread }

  TRPCServerThread = Class(TPCThread)
    FServerSocket:TTCPBlockSocket;
    FPort : Word;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(Port : Word);
    Destructor Destroy; Override;
  End;

  { TRPCProcess }

  TRPCProcess = class(TPCThread)
  private
    FSock:TTCPBlockSocket;
    FNode : TNode;
  public
    Constructor Create (hsock:tSocket);
    Destructor Destroy; override;
    procedure BCExecute; override;
    function ProcessMethod(Const method : String; params : TPCJSONObject; jsonresponse : TPCJSONObject; Var ErrorNum : Integer; Var ErrorDesc : String) : Boolean;
  end;


implementation

Uses  {$IFNDEF FPC}windows,{$ENDIF}
  SysUtils, Synautil;

var _RPCServer : TRPCServer = Nil;

{ TRPCServer }

Procedure TRPCServer.AddRPCLog(Const Sender : String; Const Message : String);
Begin
  If Not Assigned(FRPCLog) then exit;
  FRPCLog.NotifyNewLog(ltinfo,Sender+' '+Inttostr(FCallsCounter),Message);
end;

Function TRPCServer.GetLogFileName : AnsiString;
Begin
  If Assigned(FRPCLog) then
    Result := FRPCLog.FileName
  else Result := '';
end;

function TRPCServer.GetNewCallCounter: Int64;
begin
  inc(FCallsCounter);
  Result := FCallsCounter;
end;

procedure TRPCServer.SetActive(AValue: Boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  if (FActive) then begin
    FRPCServerThread := TRPCServerThread.Create(FPort);
  end else begin
    FRPCServerThread.Terminate;
    FRPCServerThread.WaitFor;
    FreeAndNil(FRPCServerThread);
  end;
  TLog.NewLog(ltupdate,Classname,'Updated RPC Server to Active='+CT_TRUE_FALSE[FActive]);
end;

procedure TRPCServer.SetIniFileName(const Value: AnsiString);
begin
  if FIniFileName=Value then exit;
  FreeAndNil(FIniFile);
  FIniFileName := Value;
  if (FIniFileName<>'') And (FileExists(FIniFileName)) then begin
    FIniFile := TIniFile.Create(FIniFileName);
  end;
  if Assigned(FIniFile) then begin
    FJSON20Strict := FIniFile.ReadBool('general','json20strict',true)
  end;
end;

procedure TRPCServer.SetLogFileName(const Value: AnsiString);
begin
  If (Not Assigned(FRPCLog)) And (Trim(Value)<>'') then begin
    FRPCLog := TLog.Create(Nil);
    FRPCLog.ProcessGlobalLogs:=false;
    FRPCLog.SaveTypes:=CT_TLogTypes_ALL;
  end;
  If (trim(Value)<>'') then begin
    FRPCLog.FileName:=Value;
  end else FreeAndNil(FRPCLog);
end;

procedure TRPCServer.SetValidIPs(const Value: AnsiString);
begin
  if FValidIPs=Value then exit;
  FValidIPs := Value;
  if FValidIPs='' then TLog.NewLog(ltupdate,Classname,'Updated RPC Server valid IPs to ALL')
  else TLog.NewLog(ltupdate,Classname,'Updated RPC Server valid IPs to: '+FValidIPs)
end;

function TRPCServer.IsValidClientIP(const clientIp: String; clientPort: Word): Boolean;
begin
  if FValidIPs='' then Result := true
  else begin
    Result := pos(clientIP,FValidIPs) > 0;
  end;
end;

constructor TRPCServer.Create;
begin
  FActive := false;
  FRPCLog := Nil;
  FIniFile := Nil;
  FIniFileName := '';
  FJSON20Strict := true;
  FWalletKeys := Nil;
  FRPCServerThread := Nil;
  FPort := CT_JSONRPC_Port;
  FCallsCounter := 0;
  FValidIPs := '127.0.0.1;localhost'; // New Build 1.5 - By default, only localhost can access to RPC
  If Not assigned(_RPCServer) then _RPCServer := Self;
end;

destructor TRPCServer.Destroy;
begin
  FreeAndNil(FRPCLog);
  active := false;
  if _RPCServer=Self then _RPCServer:=Nil;
  inherited Destroy;
end;

{ TRPCProcess }

constructor TRPCProcess.Create(hsock: tSocket);
begin
  FSock:=TTCPBlockSocket.create;
  FSock.socket:=HSock;
  FreeOnTerminate:=true;
  FNode := TNode.Node;
  //Priority:=tpNormal;
  inherited create(false);
  FreeOnTerminate:=true;
end;

destructor TRPCProcess.Destroy;
begin
  FSock.free;
  inherited Destroy;
end;

procedure TRPCProcess.BCExecute;
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  size: integer;
  x, n: integer;
  resultcode: integer;
  inputdata : TBytes;
  js,jsresult : TPCJSONData;
  jsonobj,jsonresponse : TPCJSONObject;
  errNum : Integer; errDesc : String;
  jsonrequesttxt,
  jsonresponsetxt, methodName, paramsTxt : AnsiString;
  valid : Boolean;
  i : Integer;
  Headers : TStringList;
  tc : Cardinal;
  callcounter : Int64;
begin
  callcounter := _RPCServer.GetNewCallCounter;
  tc := GetTickCount;
  methodName := '';
  paramsTxt := '';
  // IP Protection
  If (Not _RPCServer.IsValidClientIP(FSock.GetRemoteSinIP,FSock.GetRemoteSinPort)) then begin
    TLog.NewLog(lterror,Classname,FSock.GetRemoteSinIP+':'+inttostr(FSock.GetRemoteSinPort)+' INVALID IP');
    _RPCServer.AddRPCLog(FSock.GetRemoteSinIP+':'+InttoStr(FSock.GetRemoteSinPort),' INVALID IP');
    exit;
  end;
  errNum := CT_RPC_ErrNum_InternalError;
  errDesc := 'No data';
  valid := false;
  SetLength(inputdata,0);
  Headers := TStringList.Create;
  jsonresponse := TPCJSONObject.Create;
  try
    timeout := 5000;
    resultcode:= 200;
    repeat
      //read request line
      s := Fsock.RecvString(timeout);
      if Fsock.lasterror <> 0 then Exit;
      if s = '' then Exit;
      method := fetch(s, ' ');
      if (s = '') or (method = '') then  Exit;
      uri := fetch(s, ' '); if uri = '' then  Exit;
      protocol := fetch(s, ' ');
      headers.Clear;
      size := -1;
      //read request headers
      if protocol <> '' then begin
        if pos('HTTP/1.1', protocol) <> 1 then begin
          errDesc := 'Invalid protocol '+protocol;
          Exit;
        end;
        repeat
          s := Fsock.RecvString(Timeout);
          if Fsock.lasterror <> 0 then
            Exit;
          if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
            Size := StrToIntDef(SeparateRight(s, ' '), -1);
        until s = '';
      end;
      //recv document...
      if size >= 0 then begin
        setLength(inputdata,size);
        x := FSock.RecvBufferEx(InputData, Size, Timeout);
        if Fsock.lasterror <> 0 then
          Exit;
        if (x<>size) And (x>0) then
          setLength(inputdata,x);
      end else setlength(inputdata,0);
      SetLength(jsonrequesttxt,length(inputdata));
      for i:=0 to high(inputdata) do begin
        jsonrequesttxt[i+1] := AnsiChar(inputdata[i]);
      end;
      // Convert InputData to JSON object
      try
        js := TPCJSONData.ParseJSONValue(jsonrequesttxt);
      except
        On E:Exception do begin
          errDesc:='Error decoding JSON: '+E.Message;
          TLog.NewLog(lterror,Classname,FSock.GetRemoteSinIP+':'+inttostr(FSock.GetRemoteSinPort)+' Error decoding JSON: '+E.Message);
          exit;
        end;
      end;
      If Assigned(js) then begin
        try
          If (js is TPCJSONObject) then begin
            jsonobj := TPCJSONObject(js);
            errNum := 0;
            errDesc := '';
            try
              methodName := jsonobj.AsString('method','');
              paramsTxt := jsonobj.GetAsObject('params').ToJSON(false);
              TLog.NewLog(ltinfo,Classname,FSock.GetRemoteSinIP+':'+inttostr(FSock.GetRemoteSinPort)+' Processing method '+jsonobj.AsString('method',''));
              Valid := ProcessMethod(jsonobj.AsString('method',''),jsonobj.GetAsObject('params'),jsonresponse,errNum,errDesc);
              if not Valid then begin
                if (errNum<>0) or (errDesc<>'') then begin
                  jsonresponse.GetAsObject('error').GetAsVariant('code').Value:=errNum;
                  jsonresponse.GetAsObject('error').GetAsVariant('message').Value:=errDesc;
                end else begin
                  jsonresponse.GetAsObject('error').GetAsVariant('code').Value:=CT_RPC_ErrNum_InternalError;
                  jsonresponse.GetAsObject('error').GetAsVariant('message').Value:='Unknown error processing method';
                end;
              end;
            Except
              on E:Exception do begin
                TLog.NewLog(lterror,Classname,'Exception processing method'+jsonobj.AsString('method','')+' ('+E.ClassName+'): '+E.Message);
                jsonresponse.GetAsObject('error').GetAsVariant('code').Value:=CT_RPC_ErrNum_InternalError;
                jsonresponse.GetAsObject('error').GetAsVariant('message').Value:=E.Message;
                valid:=False;
              end;
            end;
            jsonresponse.GetAsVariant('id').Value:= jsonobj.GetAsVariant('id').Value;
            jsonresponse.GetAsVariant('jsonrpc').Value:= '2.0';
          end;
        finally
          js.free;
        end;
      end else begin
        TLog.NewLog(lterror,ClassName,FSock.GetRemoteSinIP+':'+inttostr(FSock.GetRemoteSinPort)+' Received data is not a JSON: '+jsonrequesttxt+' (length '+inttostr(length(jsonrequesttxt))+' bytes)');
      end;
    until (FSock.LastError <> 0) Or (protocol<>'');
  Finally
    try
      // Send result:
      if Fsock.lasterror = 0 then begin
        // Save JSON response:
        If (Not Valid) then begin
          if Not assigned(jsonresponse.FindName('error')) then begin
            jsonresponse.GetAsObject('error').GetAsVariant('code').Value:=errNum;
            jsonresponse.GetAsObject('error').GetAsVariant('message').Value:=errDesc;
          end;
        end;
        jsonresponsetxt := jsonresponse.ToJSON(false);
        Fsock.SendString(protocol + ' ' + IntTostr(ResultCode) + CRLF);
        if (protocol <> '') then begin
          headers.Add('Server: PascalCoin HTTP JSON-RPC Server');
          headers.Add('Content-Type: application/json;charset=utf-8');
          headers.Add('Content-length: ' + IntTostr(length(jsonresponsetxt)));
          headers.Add('Connection: close');
          headers.Add('Access-Control-Allow-Origin: *');
          headers.Add('Date: ' + Rfc822DateTime(now));
          headers.Add('');
          for n := 0 to headers.count - 1 do
            Fsock.sendstring(headers[n] + CRLF);
        end;
        if Fsock.lasterror = 0 then begin
          FSock.SendBuffer(addr(jsonresponsetxt[1]),Length(jsonresponsetxt));
        end;
      end;
      _RPCServer.AddRPCLog(FSock.GetRemoteSinIP+':'+InttoStr(FSock.GetRemoteSinPort),'Method:'+methodName+' Params:'+paramsTxt+' '+Inttostr(errNum)+':'+errDesc+' Time:'+FormatFloat('0.000',(GetTickCount - tc)/1000));
    finally
      jsonresponse.free;
      Headers.Free;
    end;
  end;
end;

function TRPCProcess.ProcessMethod(const method: String; params: TPCJSONObject;
  jsonresponse: TPCJSONObject; var ErrorNum: Integer; var ErrorDesc: String): Boolean;
  var _ro : TPCJSONObject;
      _ra : TPCJSONArray;
  Function GetResultObject : TPCJSONObject;
  begin
    if not assigned(_ro) then begin
      _ro := jsonresponse.GetAsObject('result');
      _ra := Nil;
    end;
    Result := _ro;
  end;

  Function GetResultArray : TPCJSONArray;
  begin
    if not assigned(_ra) then begin
      _ra := jsonresponse.GetAsArray('result');
      _ro := Nil;
    end;
    Result := _ra;
  end;

  Function ToJSONCurrency(pascalCoins : Int64) : Real;
  Begin
    Result := RoundTo( pascalCoins / 10000 , -4);
  End;

  Function ToPascalCoins(jsonCurr : Real) : Int64;
  Begin
    Result := Round(jsonCurr * 10000);
  End;

  Function HexaStringToOperationsHashTree(Const HexaStringOperationsHashTree : AnsiString; out OperationsHashTree : TOperationsHashTree; var errors : AnsiString) : Boolean;
  var raw : TRawBytes;
    ms : TMemoryStream;
  Begin
    Result := False;
    raw := TCrypto.HexaToRaw(HexaStringOperationsHashTree);
    if (HexaStringOperationsHashTree<>'') And (raw='') then begin
      errors := 'Invalid HexaString as operations';
      exit;
    end;
    ms := TMemoryStream.Create;
    Try
      ms.WriteBuffer(raw[1],length(raw));
      ms.Position := 0;
      OperationsHashTree := TOperationsHashTree.Create;
      if (raw<>'') then begin
        If not OperationsHashTree.LoadOperationsHashTreeFromStream(ms,false,CT_PROTOCOL_1,Nil,errors) then begin
          FreeAndNil(OperationsHashTree);
          exit;
        end;
      end;
      Result := true;
    Finally
      ms.Free;
    End;
  End;

  Function OperationsHashTreeToHexaString(Const OperationsHashTree : TOperationsHashTree) : AnsiString;
  var ms : TMemoryStream;
    raw : TRawBytes;
  Begin
    ms := TMemoryStream.Create;
    Try
      OperationsHashTree.SaveOperationsHashTreeToStream(ms,false);
      ms.Position := 0;
      SetLength(raw,ms.Size);
      ms.ReadBuffer(raw[1],ms.Size);
      Result := TCrypto.ToHexaString(raw);
    Finally
      ms.Free;
    End;
  End;

  Function GetBlock(nBlock : Cardinal; jsonObject : TPCJSONObject) : Boolean;
  var pcops : TPCOperationsComp;
    ob : TOperationBlock;
  begin
    pcops := TPCOperationsComp.Create(Nil);
    try
      If FNode.Bank.BlocksCount<=nBlock then begin
        ErrorNum := CT_RPC_ErrNum_InvalidBlock;
        ErrorDesc := 'Cannot load Block: '+IntToStr(nBlock);
        Result := False;
        Exit;
      end;
      ob := FNode.Bank.SafeBox.Block(nBlock).blockchainInfo;

      jsonObject.GetAsVariant('block').Value:=ob.block;
      jsonObject.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(ob.account_key));
      jsonObject.GetAsVariant('reward').Value:=ToJSONCurrency(ob.reward);
      jsonObject.GetAsVariant('fee').Value:=ToJSONCurrency(ob.fee);
      jsonObject.GetAsVariant('ver').Value:=ob.protocol_version;
      jsonObject.GetAsVariant('ver_a').Value:=ob.protocol_available;
      jsonObject.GetAsVariant('timestamp').Value:=Int64(ob.timestamp);
      jsonObject.GetAsVariant('target').Value:=Int64(ob.compact_target);
      jsonObject.GetAsVariant('nonce').Value:=Int64(ob.nonce);
      jsonObject.GetAsVariant('payload').Value:=ob.block_payload;
      jsonObject.GetAsVariant('sbh').Value:=TCrypto.ToHexaString(ob.initial_safe_box_hash);
      jsonObject.GetAsVariant('oph').Value:=TCrypto.ToHexaString(ob.operations_hash);
      jsonObject.GetAsVariant('pow').Value:=TCrypto.ToHexaString(ob.proof_of_work);
      jsonObject.GetAsVariant('hashratekhs').Value := FNode.Bank.SafeBox.CalcBlockHashRateInKhs(ob.Block,50);
      jsonObject.GetAsVariant('maturation').Value := FNode.Bank.BlocksCount - ob.block - 1;
      If FNode.Bank.LoadOperations(pcops,nBlock) then begin
        jsonObject.GetAsVariant('operations').Value:=pcops.Count;
      end;
      Result := True;
    finally
      pcops.Free;
    end;
  end;

  Procedure FillOperationResumeToJSONObject(Const OPR : TOperationResume; jsonObject : TPCJSONObject);
  Var i : Integer;
    jsonArr : TPCJSONArray;
    auxObj : TPCJSONObject;
  Begin
    if Not OPR.valid then begin
      jsonObject.GetAsVariant('valid').Value := OPR.valid;
    end;
    if (OPR.errors<>'') And (Not OPR.valid) then begin
      jsonObject.GetAsVariant('errors').Value := OPR.errors;
    end;
    if OPR.valid then begin
      jsonObject.GetAsVariant('block').Value:=OPR.Block;
      jsonObject.GetAsVariant('time').Value:=OPR.time;
      jsonObject.GetAsVariant('opblock').Value:=OPR.NOpInsideBlock;
      if (OPR.Block>0) And (OPR.Block<FNode.Bank.BlocksCount) then
        jsonObject.GetAsVariant('maturation').Value := FNode.Bank.BlocksCount - OPR.Block - 1
      else jsonObject.GetAsVariant('maturation').Value := null;
    end;
    jsonObject.GetAsVariant('optype').Value:=OPR.OpType;
    jsonObject.GetAsVariant('subtype').Value:=OPR.OpSubtype;
    If (Not OPR.isMultiOperation) then Begin
      jsonObject.GetAsVariant('account').Value:=OPR.AffectedAccount;
      jsonObject.GetAsVariant('signer_account').Value:=OPR.SignerAccount;
      jsonObject.GetAsVariant('n_operation').Value:=OPR.n_operation;
    end else begin
      jsonArr := jsonObject.GetAsArray('senders');
      for i:=Low(OPR.senders) to High(OPR.Senders) do begin
        auxObj := jsonArr.GetAsObject(jsonArr.Count);
        auxObj.GetAsVariant('account').Value := OPR.Senders[i].Account;
        auxObj.GetAsVariant('n_operation').Value := OPR.Senders[i].N_Operation;
        auxObj.GetAsVariant('amount').Value := ToJSONCurrency(OPR.Senders[i].Amount * (-1));
        auxObj.GetAsVariant('payload').Value := TCrypto.ToHexaString(OPR.Senders[i].Payload);
      end;
      //
      jsonArr := jsonObject.GetAsArray('receivers');
      for i:=Low(OPR.Receivers) to High(OPR.Receivers) do begin
        auxObj := jsonArr.GetAsObject(jsonArr.Count);
        auxObj.GetAsVariant('account').Value := OPR.Receivers[i].Account;
        auxObj.GetAsVariant('amount').Value := ToJSONCurrency(OPR.Receivers[i].Amount);
        auxObj.GetAsVariant('payload').Value := TCrypto.ToHexaString(OPR.Receivers[i].Payload);
      end;
      jsonArr := jsonObject.GetAsArray('changers');
      for i:=Low(OPR.Changers) to High(OPR.Changers) do begin
        auxObj := jsonArr.GetAsObject(jsonArr.Count);
        auxObj.GetAsVariant('account').Value := OPR.Changers[i].Account;
        auxObj.GetAsVariant('n_operation').Value := OPR.Changers[i].N_Operation;
        If public_key in OPR.Changers[i].Changes_type then begin
          auxObj.GetAsVariant('new_enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(OPR.Changers[i].New_Accountkey));
        end;
        If account_name in OPR.Changers[i].Changes_type then begin
          auxObj.GetAsVariant('new_name').Value := OPR.Changers[i].New_Name;
        end;
        If account_type in OPR.Changers[i].Changes_type then begin
          auxObj.GetAsVariant('new_type').Value := OPR.Changers[i].New_Type;
        end;
      end;
    end;
    jsonObject.GetAsVariant('optxt').Value:=OPR.OperationTxt;
    jsonObject.GetAsVariant('fee').Value:=ToJSONCurrency(OPR.Fee);
    if (Not OPR.isMultiOperation) then begin
      jsonObject.GetAsVariant('amount').Value:=ToJSONCurrency(OPR.Amount);
      if (OPR.Balance>=0) And (OPR.valid) then jsonObject.GetAsVariant('balance').Value:=ToJSONCurrency(OPR.Balance);
      jsonObject.GetAsVariant('payload').Value:=TCrypto.ToHexaString(OPR.OriginalPayload);
    end else begin
      jsonObject.GetAsVariant('totalamount').Value:=ToJSONCurrency(OPR.Amount);
      if (OPR.Balance>=0) And (OPR.valid) then jsonObject.GetAsVariant('balance').Value:=ToJSONCurrency(OPR.Balance);
    end;
    If (OPR.OpType = CT_Op_Transaction) then begin
      If OPR.SignerAccount>=0 then begin
        jsonObject.GetAsVariant('sender_account').Value:=OPR.SignerAccount;
      end;
      If OPR.DestAccount>=0 then begin
        jsonObject.GetAsVariant('dest_account').Value:=OPR.DestAccount;
      end;
    end;
    If OPR.newKey.EC_OpenSSL_NID>0 then begin
      jsonObject.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(OPR.newKey));
    end;
    if (OPR.valid) And (OPR.OperationHash<>'') then begin
      jsonObject.GetAsVariant('ophash').Value := TCrypto.ToHexaString(OPR.OperationHash);
      if (OPR.Block<CT_Protocol_Upgrade_v2_MinBlock) then begin
        jsonObject.GetAsVariant('old_ophash').Value := TCrypto.ToHexaString(OPR.OperationHash_OLD);
      end;
    end;
  end;

  Procedure FillOperationsHashTreeToJSONObject(Const OperationsHashTree : TOperationsHashTree; jsonObject : TPCJSONObject);
  Begin
    jsonObject.GetAsVariant('operations').Value:=OperationsHashTree.OperationsCount;
    jsonObject.GetAsVariant('amount').Value:=ToJSONCurrency(OperationsHashTree.TotalAmount);
    jsonObject.GetAsVariant('fee').Value:=ToJSONCurrency(OperationsHashTree.TotalFee);
    jsonObject.GetAsVariant('rawoperations').Value:=OperationsHashTreeToHexaString(OperationsHashTree);
  End;

  Function GetAccountOperations(accountNumber : Cardinal; jsonArray : TPCJSONArray; maxBlocksDepth, startReg, maxReg: Integer; forceStartBlock : Cardinal) : Boolean;
  var list : TList;
    Op : TPCOperation;
    OPR : TOperationResume;
    Obj : TPCJSONObject;
    OperationsResume : TOperationsResumeList;
    i, nCounter : Integer;
  Begin
    Result := false;
    if (startReg<-1) or (maxReg<=0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid start or max value';
      Exit;
    end;
    nCounter := 0;
    OperationsResume := TOperationsResumeList.Create;
    try
      if ((startReg=-1) And (forceStartBlock=0)) then begin
        // 1.5.5 change: If start=-1 then will include PENDING OPERATIONS, otherwise not.
        // Only will return pending operations if start=0, otherwise
        list := TList.Create;
        Try
          FNode.Operations.OperationsHashTree.GetOperationsAffectingAccount(accountNumber,list);
          for i := list.Count - 1 downto 0 do begin
            Op := FNode.Operations.OperationsHashTree.GetOperation(PtrInt(list[i]));
            If TPCOperation.OperationToOperationResume(0,Op,accountNumber,OPR) then begin
              OPR.NOpInsideBlock := i;
              OPR.Block := FNode.Operations.OperationBlock.block;
              OPR.Balance := FNode.Operations.SafeBoxTransaction.Account(accountNumber).balance;
              if (nCounter>=startReg) And (nCounter<maxReg) then begin
                OperationsResume.Add(OPR);
              end;
              inc(nCounter);
            end;
          end;
        Finally
          list.Free;
        End;
      end;
      if (nCounter<maxReg) then begin
        if (startReg<0) then startReg := 0; // Prevent -1 value
        FNode.GetStoredOperationsFromAccount(OperationsResume,accountNumber,maxBlocksDepth,startReg,startReg+maxReg-1,forceStartBlock);
      end;
      for i:=0 to OperationsResume.Count-1 do begin
        Obj := jsonArray.GetAsObject(jsonArray.Count);
        OPR := OperationsResume[i];
        FillOperationResumeToJSONObject(OPR,Obj);
      end;
      Result := True;
    finally
      OperationsResume.Free;
    end;
  end;

  Procedure GetConnections;
  var i : Integer;
    l : TList;
    nc : TNetConnection;
    obj: TPCJSONObject;
  Begin
    l := TNetData.NetData.NetConnections.LockList;
    try
      for i:=0 to l.Count-1 do begin
        nc := TNetData.NetData.Connection(i);
        obj := jsonresponse.GetAsArray('result').GetAsObject(i);
        obj.GetAsVariant('server').Value := Not (nc is TNetServerClient);
        obj.GetAsVariant('ip').Value:=nc.Client.RemoteHost;
        obj.GetAsVariant('port').Value:=nc.Client.RemotePort;
        obj.GetAsVariant('secs').Value:=UnivDateTimeToUnix(now) - UnivDateTimeToUnix(nc.CreatedTime);
        obj.GetAsVariant('sent').Value:=nc.Client.BytesSent;
        obj.GetAsVariant('recv').Value:=nc.Client.BytesReceived;
        obj.GetAsVariant('appver').Value:=nc.ClientAppVersion;
        obj.GetAsVariant('netver').Value:=nc.NetProtocolVersion.protocol_version;
        obj.GetAsVariant('netver_a').Value:=nc.NetProtocolVersion.protocol_available;
        obj.GetAsVariant('timediff').Value:=nc.TimestampDiff;
      end;
    finally
      TNetData.NetData.NetConnections.UnlockList;
    end;
  end;

  // This function creates a TOpTransaction without looking for balance/private key of sender account
  // It assumes that sender,target,sender_last_n_operation,senderAccountKey and targetAccountKey are correct
  Function CreateOperationTransaction(sender, target, sender_last_n_operation : Cardinal; amount, fee : UInt64;
    Const senderAccounKey, targetAccountKey : TAccountKey; Const RawPayload : TRawBytes;
    Const Payload_method, EncodePwd : AnsiString) : TOpTransaction;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var i : Integer;
    f_raw : TRawBytes;
  Begin
    Result := Nil;
    i := _RPCServer.FWalletKeys.IndexOfAccountKey(senderAccounKey);
    if i<0 then begin
      ErrorDesc:='Sender Public Key not found in wallet: '+TAccountComp.AccountPublicKeyExport(senderAccounKey);
      ErrorNum:=CT_RPC_ErrNum_InvalidPubKey;
      Exit;
    end;
    if (Not assigned(_RPCServer.FWalletKeys.Key[i].PrivateKey)) then begin
      if _RPCServer.FWalletKeys.Key[i].CryptedKey<>'' then begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end else begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      end;
      exit;
    end;
    //
    if (length(RawPayload)>0) then begin
      if (Payload_method='none') then f_raw:=RawPayload
      else if (Payload_method='dest') then begin
        f_raw := ECIESEncrypt(targetAccountKey,RawPayload);
      end else if (Payload_method='sender') then begin
        f_raw := ECIESEncrypt(senderAccounKey,RawPayload);
      end else if (Payload_method='aes') then begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload,EncodePwd);
      end else begin
        ErrorNum:=CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc:='Invalid encode payload method: '+Payload_method;
        exit;
      end;
    end else f_raw := '';
    Result := TOpTransaction.CreateTransaction(sender,sender_last_n_operation+1,target,_RPCServer.FWalletKeys.Key[i].PrivateKey,amount,fee,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  Function OpSendTo(sender, target : Cardinal; amount, fee : UInt64; Const RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opt : TOpTransaction;
    sacc,tacc : TAccount;
    errors : AnsiString;
    opr : TOperationResume;
  begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent sends
    try
      Result := false;
      if (sender<0) or (sender>=FNode.Bank.AccountsCount) then begin
        If (sender=CT_MaxAccount) then ErrorDesc := 'Need sender'
        else ErrorDesc:='Invalid sender account '+Inttostr(sender);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      if (target<0) or (target>=FNode.Bank.AccountsCount) then begin
        If (target=CT_MaxAccount) then ErrorDesc := 'Need target'
        else ErrorDesc:='Invalid target account '+Inttostr(target);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      sacc := FNode.Operations.SafeBoxTransaction.Account(sender);
      tacc := FNode.Operations.SafeBoxTransaction.Account(target);

      opt := CreateOperationTransaction(sender,target,sacc.n_operation,amount,fee,sacc.accountInfo.accountKey,tacc.accountInfo.accountKey,RawPayload,Payload_method,EncodePwd);
      if opt=nil then exit;
      try
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorDesc := 'Error adding operation: '+errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          Exit;
        end;
        TPCOperation.OperationToOperationResume(0,opt,sender,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
        Result := true;
      finally
        opt.free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  end;

  Function SignOpSendTo(Const HexaStringOperationsHashTree : TRawBytes; sender, target : Cardinal;
    Const senderAccounKey, targetAccountKey : TAccountKey;
    last_sender_n_operation : Cardinal;
    amount, fee : UInt64; Const RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var OperationsHashTree : TOperationsHashTree;
    errors : AnsiString;
    opt : TOpTransaction;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param "rawoperations": '+errors;
      Exit;
    end;
    Try
      opt := CreateOperationTransaction(sender,target,last_sender_n_operation,amount,fee,senderAccounKey,targetAccountKey,RawPayload,Payload_method,EncodePwd);
      if opt=nil then exit;
      try
        OperationsHashTree.AddOperationToHashTree(opt);
        FillOperationsHashTreeToJSONObject(OperationsHashTree,GetResultObject);
        Result := true;
      finally
        opt.Free;
      end;
    Finally
      OperationsHashTree.Free;
    End;
  end;

  // This function creates a TOpChangeKey without looking for private key of account
  // It assumes that account_signer,account_last_n_operation, account_target and account_pubkey are correct
  Function CreateOperationChangeKey(account_signer, account_last_n_operation, account_target : Cardinal; const account_pubkey, new_pubkey : TAccountKey; fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : TOpChangeKey;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var i : Integer;
    errors : AnsiString;
    f_raw : TRawBytes;
  Begin
    Result := Nil;
    i := _RPCServer.FWalletKeys.IndexOfAccountKey(account_pubkey);
    if (i<0) then begin
      ErrorDesc:='Private key not found in wallet: '+TAccountComp.AccountPublicKeyExport(account_pubkey);
      ErrorNum:=CT_RPC_ErrNum_InvalidPubKey;
      Exit;
    end;
    if (Not assigned(_RPCServer.FWalletKeys.Key[i].PrivateKey)) then begin
      if _RPCServer.FWalletKeys.Key[i].CryptedKey<>'' then begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end else begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload)>0) then begin
      if (Payload_method='none') then f_raw:=RawPayload
      else if (Payload_method='dest') then begin
        f_raw := ECIESEncrypt(new_pubkey,RawPayload);
      end else if (Payload_method='sender') then begin
        f_raw := ECIESEncrypt(account_pubkey,RawPayload);
      end else if (Payload_method='aes') then begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload,EncodePwd);
      end else begin
        ErrorNum:=CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc:='Invalid encode payload method: '+Payload_method;
        exit;
      end;
    end else f_raw := '';
    If account_signer=account_target then begin
      Result := TOpChangeKey.Create(account_signer,account_last_n_operation+1,account_target,_RPCServer.FWalletKeys.Key[i].PrivateKey,new_pubkey,fee,f_raw);
    end else begin
      Result := TOpChangeKeySigned.Create(account_signer,account_last_n_operation+1,account_target,_RPCServer.FWalletKeys.Key[i].PrivateKey,new_pubkey,fee,f_raw);
    end;
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  Function ChangeAccountKey(account_signer, account_target : Cardinal; const new_pub_key : TAccountKey; fee : UInt64; const RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opck : TOpChangeKey;
    acc_signer : TAccount;
    errors : AnsiString;
    opr : TOperationResume;
  begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      if (account_signer<0) or (account_signer>=FNode.Bank.AccountsCount) then begin
        ErrorDesc:='Invalid account '+Inttostr(account_signer);
        ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
        Exit;
      end;
      acc_signer := FNode.Operations.SafeBoxTransaction.Account(account_signer);

      opck := CreateOperationChangeKey(account_signer,acc_signer.n_operation,account_target,acc_signer.accountInfo.accountKey,new_pub_key,fee,RawPayload,Payload_method,EncodePwd);
      if not assigned(opck) then exit;
      try
        If not FNode.AddOperation(Nil,opck,errors) then begin
          ErrorDesc := 'Error adding operation: '+errors;
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          Exit;
        end;
        TPCOperation.OperationToOperationResume(0,opck,account_signer,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
        Result := true;
      finally
        opck.free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  end;

  // This function creates a TOpListAccountForSale without looking for actual state (cold wallet)
  // It assumes that account_number,account_last_n_operation and account_pubkey are correct
  Function CreateOperationListAccountForSale(account_signer, account_last_n_operation, account_listed : Cardinal; const account_signer_pubkey: TAccountKey;
    account_price : UInt64; locked_until_block : Cardinal; account_to_pay : Cardinal; Const new_account_pubkey : TAccountKey;
    fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : TOpListAccountForSale;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var i : Integer;
    errors : AnsiString;
    f_raw : TRawBytes;
  Begin
    Result := Nil;
    i := _RPCServer.FWalletKeys.IndexOfAccountKey(account_signer_pubkey);
    if (i<0) then begin
      ErrorDesc:='Private key not found in wallet: '+TAccountComp.AccountPublicKeyExport(account_signer_pubkey);
      ErrorNum:=CT_RPC_ErrNum_InvalidPubKey;
      Exit;
    end;
    if (Not assigned(_RPCServer.FWalletKeys.Key[i].PrivateKey)) then begin
      if _RPCServer.FWalletKeys.Key[i].CryptedKey<>'' then begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end else begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload)>0) then begin
      if (Payload_method='none') then f_raw:=RawPayload
      else if (Payload_method='dest') And (new_account_pubkey.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
        // If using 'dest', only will apply if there is a fixed new public key, otherwise will use current public key of account
        f_raw := ECIESEncrypt(new_account_pubkey,RawPayload);
      end else if (Payload_method='dest') Or (Payload_method='sender') then begin
        f_raw := ECIESEncrypt(account_signer_pubkey,RawPayload);
      end else if (Payload_method='aes') then begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload,EncodePwd);
      end else begin
        ErrorNum:=CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc:='Invalid encode payload method: '+Payload_method;
        exit;
      end;
    end else f_raw := '';
    Result := TOpListAccountForSale.CreateListAccountForSale(account_signer,account_last_n_operation+1,account_listed,account_price,fee,account_to_pay,new_account_pubkey,locked_until_block,
     _RPCServer.FWalletKeys.Key[i].PrivateKey,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  // This function creates a TOpDelistAccountForSale without looking for actual state (cold wallet)
  // It assumes that account_number,account_last_n_operation are correct
  Function CreateOperationDelistAccountForSale(account_signer, account_last_n_operation, account_delisted : Cardinal; const account_signer_pubkey: TAccountKey;
    fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : TOpDelistAccountForSale;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var i : Integer;
    f_raw : TRawBytes;
  Begin
    Result := Nil;
    i := _RPCServer.FWalletKeys.IndexOfAccountKey(account_signer_pubkey);
    if (i<0) then begin
      ErrorDesc:='Private key not found in wallet: '+TAccountComp.AccountPublicKeyExport(account_signer_pubkey);
      ErrorNum:=CT_RPC_ErrNum_InvalidPubKey;
      Exit;
    end;
    if (Not assigned(_RPCServer.FWalletKeys.Key[i].PrivateKey)) then begin
      if _RPCServer.FWalletKeys.Key[i].CryptedKey<>'' then begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end else begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload)>0) then begin
      if (Payload_method='none') then f_raw:=RawPayload
      else if (Payload_method='dest') Or (Payload_method='sender') then begin
        f_raw := ECIESEncrypt(account_signer_pubkey,RawPayload);
      end else if (Payload_method='aes') then begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload,EncodePwd);
      end else begin
        ErrorNum:=CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc:='Invalid encode payload method: '+Payload_method;
        exit;
      end;
    end else f_raw := '';
    Result := TOpDelistAccountForSale.CreateDelistAccountForSale(account_signer,account_last_n_operation+1,account_delisted,fee,_RPCServer.FWalletKeys.Key[i].PrivateKey,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  // This function creates a TOpBuyAccount without looking for actual state (cold wallet)
  // It assumes that account_number,account_last_n_operation and account_pubkey are correct
  // Also asumes that amount is >= price and other needed conditions
  Function CreateOperationBuyAccount(account_number, account_last_n_operation : Cardinal; const account_pubkey: TAccountKey;
    account_to_buy : Cardinal; account_price, amount : UInt64; account_to_pay : Cardinal; Const new_account_pubkey : TAccountKey;
    fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : TOpBuyAccount;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var i : Integer;
    errors : AnsiString;
    f_raw : TRawBytes;
  Begin
    Result := Nil;
    i := _RPCServer.FWalletKeys.IndexOfAccountKey(account_pubkey);
    if (i<0) then begin
      ErrorDesc:='Private key not found in wallet: '+TAccountComp.AccountPublicKeyExport(account_pubkey);
      ErrorNum:=CT_RPC_ErrNum_InvalidPubKey;
      Exit;
    end;
    if (Not assigned(_RPCServer.FWalletKeys.Key[i].PrivateKey)) then begin
      if _RPCServer.FWalletKeys.Key[i].CryptedKey<>'' then begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end else begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload)>0) then begin
      if (Payload_method='none') then f_raw:=RawPayload
      else if (Payload_method='dest') then begin
        f_raw := ECIESEncrypt(new_account_pubkey,RawPayload);
      end else if (Payload_method='sender') then begin
        f_raw := ECIESEncrypt(account_pubkey,RawPayload);
      end else if (Payload_method='aes') then begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload,EncodePwd);
      end else begin
        ErrorNum:=CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc:='Invalid encode payload method: '+Payload_method;
        exit;
      end;
    end else f_raw := '';
    Result := TOpBuyAccount.CreateBuy(account_number,account_last_n_operation+1,account_to_buy,account_to_pay,account_price,amount,fee,new_account_pubkey,_RPCServer.FWalletKeys.Key[i].PrivateKey,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  Function GetCardinalsValues(ordinals_coma_separated : String; cardinals : TOrderedCardinalList; var errors : AnsiString) : Boolean;
  Var i,istart : Integer;
    ctxt : String;
    an : Cardinal;
  begin
    result := false;
    cardinals.Clear;
    errors := '';
    ctxt := '';
    istart := 1;
    for i := 1 to length(ordinals_coma_separated) do begin
      case ordinals_coma_separated[i] of
        '0'..'9','-' : ctxt := ctxt + ordinals_coma_separated[i];
        ',',';' : begin
          if trim(ctxt)<>'' then begin
            if Not TAccountComp.AccountTxtNumberToAccountNumber(trim(ctxt),an) then begin
              errors := 'Invalid account number at pos '+IntToStr(istart)+': '+ctxt;
              exit;
            end;
            cardinals.Add(an);
          end;
          ctxt := '';
          istart := i+1;
        end;
        ' ' : ; // Continue...
      else
        errors := 'Invalid char at pos '+inttostr(i)+': "'+ordinals_coma_separated[i]+'"';
        exit;
      end;
    end;
    //
    if (trim(ctxt)<>'') then begin
      if Not TAccountComp.AccountTxtNumberToAccountNumber(trim(ctxt),an) then begin
        errors := 'Invalid account number at pos '+IntToStr(istart)+': '+ctxt;
        exit;
      end;
      cardinals.Add(an);
    end;
    if cardinals.Count=0 then begin
      errors := 'No valid value';
      exit;
    end;
    Result := true;
  end;

  Function ChangeAccountsKey(accounts_txt : String; const new_pub_key : TAccountKey; fee : UInt64; const RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  Var opck : TOpChangeKey;
    acc : TAccount;
    i, ian : Integer;
    errors : AnsiString;
    opr : TOperationResume;
    accountsnumber : TOrderedCardinalList;
    operationsht : TOperationsHashTree;
    OperationsResumeList : TOperationsResumeList;
  begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := false;
      accountsnumber := TOrderedCardinalList.Create;
      try
        if not GetCardinalsValues(accounts_txt,accountsnumber,errors) then begin
          ErrorDesc := 'Error in accounts: '+errors;
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          Exit;
        end;
        operationsht := TOperationsHashTree.Create;
        try
          for ian := 0 to accountsnumber.Count - 1 do begin

            if (accountsnumber.Get(ian)<0) or (accountsnumber.Get(ian)>=FNode.Bank.AccountsCount) then begin
              ErrorDesc:='Invalid account '+Inttostr(accountsnumber.Get(ian));
              ErrorNum:=CT_RPC_ErrNum_InvalidAccount;
              Exit;
            end;
            acc := FNode.Operations.SafeBoxTransaction.Account(accountsnumber.Get(ian));
            opck := CreateOperationChangeKey(acc.account,acc.n_operation,acc.account,acc.accountInfo.accountKey,new_pub_key,fee,RawPayload,Payload_method,EncodePwd);
            if not assigned(opck) then exit;
            try
              operationsht.AddOperationToHashTree(opck);
            finally
              opck.free;
            end;
          end; // For
          // Ready to execute...
          OperationsResumeList := TOperationsResumeList.Create;
          Try
            i := FNode.AddOperations(Nil,operationsht,OperationsResumeList, errors);
            if (i<0) then begin
              ErrorNum:=CT_RPC_ErrNum_InternalError;
              ErrorDesc:=errors;
              exit;
            end;
            GetResultArray.Clear; // Inits an array
            for i := 0 to OperationsResumeList.Count - 1 do begin
              FillOperationResumeToJSONObject(OperationsResumeList[i],GetResultArray.GetAsObject(i));
            end;
          Finally
            OperationsResumeList.Free;
          End;
          Result := true;
        finally
          operationsht.Free;
        end;
      finally
        accountsnumber.Free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  end;

  Function SignOpChangeKey(Const HexaStringOperationsHashTree : TRawBytes; account_signer, account_target : Cardinal;
    Const actualAccounKey, newAccountKey : TAccountKey;
    last_n_operation : Cardinal;
    fee : UInt64; Const RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : Boolean;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var OperationsHashTree : TOperationsHashTree;
    errors : AnsiString;
    opck : TOpChangeKey;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param "rawoperations": '+errors;
      Exit;
    end;
    Try
      opck := CreateOperationChangeKey(account_signer,last_n_operation,account_target,actualAccounKey,newAccountKey,fee,RawPayload,Payload_method,EncodePwd);
      if opck=nil then exit;
      try
        OperationsHashTree.AddOperationToHashTree(opck);
        FillOperationsHashTreeToJSONObject(OperationsHashTree,GetResultObject);
        Result := true;
      finally
        opck.Free;
      end;
    Finally
      OperationsHashTree.Free;
    End;
  end;

  Function OperationsInfo(Const HexaStringOperationsHashTree : TRawBytes; jsonArray : TPCJSONArray) : Boolean;
  var OperationsHashTree : TOperationsHashTree;
    errors : AnsiString;
    OPR : TOperationResume;
    Obj : TPCJSONObject;
    Op : TPCOperation;
    i : Integer;
  Begin
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param "rawoperations": '+errors;
      Exit;
    end;
    Try
      jsonArray.Clear;
      for i := 0 to OperationsHashTree.OperationsCount - 1 do begin
        Op := OperationsHashTree.GetOperation(i);
        Obj := jsonArray.GetAsObject(i);
        If TPCOperation.OperationToOperationResume(0,Op,Op.SignerAccount,OPR) then begin
          OPR.NOpInsideBlock := i;
          OPR.Balance := -1;
        end else OPR := CT_TOperationResume_NUL;
        FillOperationResumeToJSONObject(OPR,Obj);
      end;
      Result := true;
    Finally
      OperationsHashTree.Free;
    End;
  End;

  Function ExecuteOperations(Const HexaStringOperationsHashTree : TRawBytes) : Boolean;
  var OperationsHashTree : TOperationsHashTree;
    errors : AnsiString;
    i : Integer;
    OperationsResumeList : TOperationsResumeList;
  Begin
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param "rawoperations": '+errors;
      Exit;
    end;
    Try
      errors := '';
      OperationsResumeList := TOperationsResumeList.Create;
      Try
        i := FNode.AddOperations(Nil,OperationsHashTree,OperationsResumeList,errors);
        if (i<0) then begin
          ErrorNum:=CT_RPC_ErrNum_InternalError;
          ErrorDesc:=errors;
          exit;
        end;
        GetResultArray.Clear; // Inits an array
        for i := 0 to OperationsResumeList.Count - 1 do begin
          FillOperationResumeToJSONObject(OperationsResumeList[i],GetResultArray.GetAsObject(i));
        end;
      Finally
        OperationsResumeList.Free;
      End;
      Result := true;
    Finally
      OperationsHashTree.Free;
    End;
  End;

  Procedure FillAccountObject(Const account : TAccount; jsonObj : TPCJSONObject);
  Begin
    jsonObj.GetAsVariant('account').Value:=account.account;
    jsonObj.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account.accountInfo.accountKey));
    jsonObj.GetAsVariant('balance').Value:=ToJSONCurrency(account.balance);
    jsonObj.GetAsVariant('n_operation').Value:=account.n_operation;
    jsonObj.GetAsVariant('updated_b').Value:=account.updated_block;
    case account.accountInfo.state of
      as_Normal : jsonObj.GetAsVariant('state').Value:='normal';
      as_ForSale : begin
        jsonObj.GetAsVariant('state').Value:='listed';
        jsonObj.GetAsVariant('locked_until_block').Value:=account.accountInfo.locked_until_block;
        jsonObj.GetAsVariant('price').Value:=account.accountInfo.price;
        jsonObj.GetAsVariant('seller_account').Value:=account.accountInfo.account_to_pay;
        jsonObj.GetAsVariant('private_sale').Value:= (account.accountInfo.new_publicKey.EC_OpenSSL_NID<>0);
        if not (account.accountInfo.new_publicKey.EC_OpenSSL_NID<>0) then begin
          jsonObj.GetAsVariant('new_enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account.accountInfo.new_publicKey));
        end;
      end
    else raise Exception.Create('ERROR DEV 20170425-1');
    end;
    jsonObj.GetAsVariant('name').Value := account.name;
    jsonObj.GetAsVariant('type').Value := account.account_type;
  end;

  Procedure FillPublicKeyObject(const PubKey : TAccountKey; jsonObj : TPCJSONObject);
  Begin
    jsonObj.GetAsVariant('ec_nid').Value := PubKey.EC_OpenSSL_NID;
    jsonObj.GetAsVariant('x').Value := TCrypto.ToHexaString(PubKey.x);
    jsonObj.GetAsVariant('y').Value := TCrypto.ToHexaString(PubKey.y);
    jsonObj.GetAsVariant('enc_pubkey').Value := TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(PubKey));
    jsonObj.GetAsVariant('b58_pubkey').Value := TAccountComp.AccountPublicKeyExport(PubKey);
  End;

  Function DoEncrypt(RawPayload : TRawBytes; pub_key : TAccountKey; Const Payload_method, EncodePwd : AnsiString) : Boolean;
  Var f_raw : TRawBytes;
  begin
    Result := false;
    if (length(RawPayload)>0) then begin
      if (Payload_method='none') then f_raw:=RawPayload
      else if (Payload_method='pubkey') then begin
        f_raw := ECIESEncrypt(pub_key,RawPayload);
      end else if (Payload_method='aes') then begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload,EncodePwd);
      end else begin
        ErrorNum:=CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc:='Invalid encode payload method: '+Payload_method;
        exit;
      end;
    end else f_raw := '';
    jsonresponse.GetAsVariant('result').Value := TCrypto.ToHexaString(f_raw);
    Result := true;
  end;

  Function DoDecrypt(RawEncryptedPayload : TRawBytes; jsonArrayPwds : TPCJSONArray) : Boolean;
  var i : Integer;
    pkey : TECPrivateKey;
    decrypted_payload : TRawBytes;
  Begin
    Result := false;
    if RawEncryptedPayload='' then begin
      GetResultObject.GetAsVariant('result').Value:= False;
      GetResultObject.GetAsVariant('enc_payload').Value:= '';
      // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
      Result := true;
      exit;
    end;
    for i := 0 to _RPCServer.WalletKeys.Count - 1 do begin
      pkey := _RPCServer.WalletKeys.Key[i].PrivateKey;
      if (assigned(pkey)) then begin
        If ECIESDecrypt(pkey.EC_OpenSSL_NID,pkey.PrivateKey,false,RawEncryptedPayload,decrypted_payload) then begin
          GetResultObject.GetAsVariant('result').Value:= true;
          GetResultObject.GetAsVariant('enc_payload').Value:= TCrypto.ToHexaString(RawEncryptedPayload);
          GetResultObject.GetAsVariant('unenc_payload').Value:= decrypted_payload;
          GetResultObject.GetAsVariant('unenc_hexpayload').Value:= TCrypto.ToHexaString(decrypted_payload);
          GetResultObject.GetAsVariant('payload_method').Value:= 'key';
          GetResultObject.GetAsVariant('enc_pubkey').Value:= TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(pkey.PublicKey));
          // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
          Result := true;
          exit;
        end;
      end;
    end;
    for i := 0 to jsonArrayPwds.Count - 1 do begin
      if TAESComp.EVP_Decrypt_AES256(RawEncryptedPayload,jsonArrayPwds.GetAsVariant(i).AsString(''),decrypted_payload) then begin
        GetResultObject.GetAsVariant('result').Value:= true;
        GetResultObject.GetAsVariant('enc_payload').Value:= TCrypto.ToHexaString(RawEncryptedPayload);
        GetResultObject.GetAsVariant('unenc_payload').Value:= decrypted_payload;
        GetResultObject.GetAsVariant('unenc_hexpayload').Value:= TCrypto.ToHexaString(decrypted_payload);
        GetResultObject.GetAsVariant('payload_method').Value:= 'pwd';
        GetResultObject.GetAsVariant('pwd').Value:= jsonArrayPwds.GetAsVariant(i).AsString('');
        // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
        Result := true;
        exit;
      end;
    end;
    // Not found
    GetResultObject.GetAsVariant('result').Value:= False;
    GetResultObject.GetAsVariant('enc_payload').Value:= TCrypto.ToHexaString(RawEncryptedPayload);
    Result := true;
  End;

  Function CapturePubKey(const prefix : String; var pubkey : TAccountKey; var errortxt : String) : Boolean;
  var ansistr : AnsiString;
    auxpubkey : TAccountKey;
  begin
    pubkey := CT_Account_NUL.accountInfo.accountKey;
    errortxt := '';
    Result := false;
    if (params.IndexOfName(prefix+'b58_pubkey')>=0) then begin
      If Not TAccountComp.AccountPublicKeyImport(params.AsString(prefix+'b58_pubkey',''),pubkey,ansistr) then begin
        errortxt:= 'Invalid value of param "'+prefix+'b58_pubkey": '+ansistr;
        exit;
      end;
      if (params.IndexOfName(prefix+'enc_pubkey')>=0) then begin
        auxpubkey := TAccountComp.RawString2Accountkey(TCrypto.HexaToRaw(params.AsString(prefix+'enc_pubkey','')));
        if (Not TAccountComp.EqualAccountKeys(auxpubkey,pubkey)) then begin
          errortxt := 'Params "'+prefix+'b58_pubkey" and "'+prefix+'enc_pubkey" public keys are not the same public key';
          exit;
        end;
      end;
    end else begin
      if (params.IndexOfName(prefix+'enc_pubkey')<0) then begin
        errortxt := 'Need param "'+prefix+'enc_pubkey" or "'+prefix+'b58_pubkey"';
        exit;
      end;
      pubkey := TAccountComp.RawString2Accountkey(TCrypto.HexaToRaw(params.AsString(prefix+'enc_pubkey','')));
    end;
    If Not TAccountComp.IsValidAccountKey(pubkey,ansistr) then begin
      errortxt := 'Invalid public key: '+ansistr;
    end else Result := true;
  end;

  function SignListAccountForSaleEx(params : TPCJSONObject; OperationsHashTree : TOperationsHashTree; const actualAccounKey : TAccountKey; last_n_operation : Cardinal) : boolean;
    // params:
    // "account_signer" is the account that signs operations and pays the fee
    // "account_target" is the account being listed
    // "locked_until_block" is until which block will be locked this account (Note: A locked account cannot change it's state until sold or finished lock)
    // "price" is the price
    // "seller_account" is the account to pay (seller account)
    // "new_b58_pubkey" or "new_enc_pubke" is the future public key for this sale (private sale), otherwise is open and everybody can buy
  var
    opSale: TOpListAccountForSale;
    account_signer, account_target, seller_account : Cardinal;
    locked_until_block : Cardinal;
    price,fee : Int64;
    new_pubkey : TAccountKey;
  begin
    Result := false;
    account_signer := params.AsInteger('account_signer',MaxInt);
    if (account_signer>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_signer" account';
      Exit;
    end;
    account_target := params.AsInteger('account_target',MaxInt);
    if (account_target>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_target" account';
      Exit;
    end;
    seller_account := params.AsInteger('seller_account',MaxInt);
    if (seller_account>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "seller_account" to pay';
      Exit;
    end;
    locked_until_block := params.AsInteger('locked_until_block',MaxInt);
    if (locked_until_block>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid "locked_until_block" value';
      Exit;
    end;
    price := ToPascalCoins(params.AsDouble('price',0));
    if (price=0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid price value';
      Exit;
    end;
    fee := ToPascalCoins(params.AsDouble('fee',0));
    if (fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;
    if (params.IndexOfName('new_b58_pubkey')>=0) or (params.IndexOfName('new_enc_pubkey')>=0) then begin
      If Not CapturePubKey('new_',new_pubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
    end else new_pubkey := CT_TECDSA_Public_Nul;
    opSale := CreateOperationListAccountForSale(account_signer,last_n_operation,account_target,actualAccounKey,price,locked_until_block,
      seller_account, new_pubkey,fee,
      TCrypto.HexaToRaw(params.AsString('payload','')),
      params.AsString('payload_method','dest'),params.AsString('pwd',''));
    if opSale=nil then exit;
    try
      OperationsHashTree.AddOperationToHashTree(opSale);
      Result := true;
    finally
      opSale.Free;
    end;
  end;

  function SignListAccountForSaleColdWallet(Const HexaStringOperationsHashTree : TRawBytes; params : TPCJSONObject) : boolean;
  var errors : AnsiString;
    OperationsHashTree : TOperationsHashTree;
    accountpubkey : TAccountKey;
    last_n_operation : Cardinal;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    try
      If Not CapturePubKey('signer_',accountpubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation',0);
      If not SignListAccountForSaleEx(params,OperationsHashTree,accountpubkey,last_n_operation) then Exit
      else Result := True;
      FillOperationsHashTreeToJSONObject(OperationsHashTree,GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function SignDelistAccountForSaleEx(params : TPCJSONObject; OperationsHashTree : TOperationsHashTree; const actualAccountKey : TAccountKey; last_n_operation : Cardinal) : boolean;
    // params:
    // "account_signer" is the account that signs operations and pays the fee
    // "account_target" is the delisted account
    // "locked_until_block" is until which block will be locked this account (Note: A locked account cannot change it's state until sold or finished lock)
    // "price" is the price
    // "seller_account" is the account to pay
    // "new_b58_pubkey" or "new_enc_pubke" is the future public key for this sale (private sale), otherwise is open and everybody can buy
  var
    opDelist: TOpDelistAccountForSale;
    account_signer, account_target : Cardinal;
    fee : Int64;
  begin
    Result := false;
    account_signer := params.AsInteger('account_signer',MaxInt);
    if (account_signer>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_signer" account';
      Exit;
    end;
    account_target := params.AsInteger('account_target',MaxInt);
    if (account_target>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_target" account';
      Exit;
    end;
    fee := ToPascalCoins(params.AsDouble('fee',0));
    if (fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;
    opDelist := CreateOperationDelistAccountForSale(account_signer,last_n_operation,account_target,actualAccountKey,fee,TCrypto.HexaToRaw(params.AsString('payload','')),
      params.AsString('payload_method','dest'),params.AsString('pwd',''));
    if opDelist=nil then exit;
    try
      OperationsHashTree.AddOperationToHashTree(opDelist);
      Result := true;
    finally
      opDelist.Free;
    end;
  end;

  // This function creates a TOpChangeAccountInfo without looking for actual state (cold wallet)
  // It assumes that account_number,account_last_n_operation and account_pubkey are correct
  Function CreateOperationChangeAccountInfo(account_signer, account_last_n_operation, account_target: Cardinal; const account_signer_pubkey: TAccountKey;
    changePubKey : Boolean; Const new_account_pubkey : TAccountKey;
    changeName: Boolean; Const new_name : TRawBytes;
    changeType: Boolean; new_type : Word;
    fee : UInt64; RawPayload : TRawBytes; Const Payload_method, EncodePwd : AnsiString) : TOpChangeAccountInfo;
  // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
  var i : Integer;
    errors : AnsiString;
    f_raw : TRawBytes;
  Begin
    Result := Nil;
    i := _RPCServer.FWalletKeys.IndexOfAccountKey(account_signer_pubkey);
    if (i<0) then begin
      ErrorDesc:='Private key not found in wallet: '+TAccountComp.AccountPublicKeyExport(account_signer_pubkey);
      ErrorNum:=CT_RPC_ErrNum_InvalidPubKey;
      Exit;
    end;
    if (Not assigned(_RPCServer.FWalletKeys.Key[i].PrivateKey)) then begin
      if _RPCServer.FWalletKeys.Key[i].CryptedKey<>'' then begin
        // Wallet is password protected
        ErrorDesc := 'Wallet is password protected';
        ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      end else begin
        ErrorDesc := 'Wallet private key not found in Wallet';
        ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      end;
      exit;
    end;
    if (length(RawPayload)>0) then begin
      if (Payload_method='none') then f_raw:=RawPayload
      else if (Payload_method='dest') And (new_account_pubkey.EC_OpenSSL_NID<>CT_TECDSA_Public_Nul.EC_OpenSSL_NID) then begin
        // If using 'dest', only will apply if there is a fixed new public key, otherwise will use current public key of account
        f_raw := ECIESEncrypt(new_account_pubkey,RawPayload);
      end else if (Payload_method='dest') Or (Payload_method='sender') then begin
        f_raw := ECIESEncrypt(account_signer_pubkey,RawPayload);
      end else if (Payload_method='aes') then begin
        f_raw := TAESComp.EVP_Encrypt_AES256(RawPayload,EncodePwd);
      end else begin
        ErrorNum:=CT_RPC_ErrNum_InvalidOperation;
        ErrorDesc:='Invalid encode payload method: '+Payload_method;
        exit;
      end;
    end else f_raw := '';
    Result := TOpChangeAccountInfo.CreateChangeAccountInfo(account_signer,account_last_n_operation+1,account_target,
      _RPCServer.FWalletKeys.Key[i].PrivateKey,
      changePubKey,new_account_pubkey,changeName,new_name,changeType,new_type,
      fee,f_raw);
    if Not Result.HasValidSignature then begin
      FreeAndNil(Result);
      ErrorNum:=CT_RPC_ErrNum_InternalError;
      ErrorDesc:='Invalid signature';
      exit;
    end;
  End;

  function SignChangeAccountInfoEx(params : TPCJSONObject; OperationsHashTree : TOperationsHashTree; const actualAccountKey : TAccountKey; last_n_operation : Cardinal) : boolean;
    // params:
    // "account_signer" is the account that signs operations and pays the fee
    // "account_target" is the target to change info
    // "new_b58_pubkey" or "new_enc_pubke" is the new public key for this account
    // "new_name" is the new account name
    // "new_type" is the new account type
  var
    opChangeInfo: TOpChangeAccountInfo;
    account_signer, account_target : Cardinal;
    fee : Int64;
    changeKey,changeName,changeType : Boolean;
    new_name : AnsiString;
    new_type : Word;
    new_typeI : Integer;
    new_pubkey : TAccountKey;
  begin
    Result := false;
    account_signer := params.AsInteger('account_signer',MaxInt);
    if (account_signer>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_signer" account';
      Exit;
    end;
    account_target := params.AsInteger('account_target',MaxInt);
    if (account_target>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid "account_target" account';
      Exit;
    end;
    fee := ToPascalCoins(params.AsDouble('fee',0));
    if (fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;
    if (params.IndexOfName('new_b58_pubkey')>=0) or (params.IndexOfName('new_enc_pubkey')>=0) then begin
      changeKey:=True;
      If Not CapturePubKey('new_',new_pubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
    end else begin
      new_pubkey := CT_TECDSA_Public_Nul;
      changeKey:=False;
    end;
    if (params.IndexOfName('new_name')>=0) then begin
      changeName:=True;
      new_name := params.AsString('new_name',' ');
    end else begin
      new_name := '';
      changeName:=False;
    end;
    if (params.IndexOfName('new_type')>=0) then begin
      changeType:=True;
      new_typeI := params.AsInteger('new_type',-1);
      if (new_typeI<0) Or (new_typeI>65536) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidData;
        ErrorDesc := 'Invalid new type value '+IntToStr(new_typeI);
        Exit;
      end;
      new_type := new_typeI;
    end else begin
      new_type := 0;
      changeType:=False;
    end;

    opChangeInfo := CreateOperationChangeAccountInfo(account_signer,last_n_operation,account_target,actualAccountKey,
      changeKey,new_pubkey,
      changeName,new_name,
      changeType,new_type,
      fee,TCrypto.HexaToRaw(params.AsString('payload','')),
      params.AsString('payload_method','dest'),params.AsString('pwd',''));
    if opChangeInfo=nil then exit;
    try
      OperationsHashTree.AddOperationToHashTree(opChangeInfo);
      Result := true;
    finally
      opChangeInfo.Free;
    end;
  end;

  function SignChangeAccountInfoColdWallet(Const HexaStringOperationsHashTree : TRawBytes; params : TPCJSONObject) : boolean;
  var errors : AnsiString;
    OperationsHashTree : TOperationsHashTree;
    accountpubkey : TAccountKey;
    last_n_operation : Cardinal;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    try
      If Not CapturePubKey('signer_',accountpubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation',0);
      If not SignChangeAccountInfoEx(params,OperationsHashTree,accountpubkey,last_n_operation) then Exit
      else Result := True;
      FillOperationsHashTreeToJSONObject(OperationsHashTree,GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function SignDelistAccountForSaleColdWallet(Const HexaStringOperationsHashTree : TRawBytes; params : TPCJSONObject) : boolean;
  var errors : AnsiString;
    OperationsHashTree : TOperationsHashTree;
    accountpubkey : TAccountKey;
    last_n_operation : Cardinal;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    try
      If Not CapturePubKey('signer_',accountpubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation',0);
      If not SignDelistAccountForSaleEx(params,OperationsHashTree,accountpubkey,last_n_operation) then Exit
      else Result := True;
      FillOperationsHashTreeToJSONObject(OperationsHashTree,GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function SignBuyAccountEx(params : TPCJSONObject; OperationsHashTree : TOperationsHashTree; const buyerAccountKey : TAccountKey; last_n_operation : Cardinal) : boolean;
    // params:
    // "buyer_account" is the buyer account
    // "account_to_purchase" is the account to purchase
    // "price" is the price
    // "seller_account" is the account to pay
    // "new_b58_pubkey" or "new_enc_pubkey" is the future public key for this sale (private sale), otherwise is open and everybody can buy
    // "amount" is the transferred amount to pay (can exceed price)
  var
    opBuy: TOpBuyAccount;
    buyer_account, account_to_purchase, seller_account : Cardinal;
    price,amount,fee : Int64;
    new_pubkey : TAccountKey;
  begin
    Result := false;
    buyer_account := params.AsInteger('buyer_account',MaxInt);
    if (buyer_account>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid buyer account';
      Exit;
    end;
    account_to_purchase := params.AsInteger('account_to_purchase',MaxInt);
    if (account_to_purchase>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid account to purchase';
      Exit;
    end;
    seller_account := params.AsInteger('seller_account',MaxInt);
    if (seller_account>=MaxInt) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Invalid seller account';
      Exit;
    end;
    price := ToPascalCoins(params.AsDouble('price',0));
    if (price<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid price value';
      Exit;
    end;
    amount := ToPascalCoins(params.AsDouble('amount',0));
    if (amount<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid amount value';
      Exit;
    end;
    fee := ToPascalCoins(params.AsDouble('fee',0));
    if (fee<0) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Invalid fee value';
      Exit;
    end;
    if (params.IndexOfName('new_b58_pubkey')>=0) or (params.IndexOfName('new_enc_pubkey')>=0) then begin
      If Not CapturePubKey('new_',new_pubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        Exit;
      end;
    end else new_pubkey := CT_TECDSA_Public_Nul;
    opBuy := CreateOperationBuyAccount(buyer_account,last_n_operation,buyerAccountKey,account_to_purchase,price,amount,seller_account,new_pubkey,fee,
      TCrypto.HexaToRaw(params.AsString('payload','')),
      params.AsString('payload_method','dest'),params.AsString('pwd',''));
    if opBuy=nil then exit;
    try
      OperationsHashTree.AddOperationToHashTree(opBuy);
      Result := true;
    finally
      opBuy.Free;
    end;
  end;

  function SignBuyAccountColdWallet(Const HexaStringOperationsHashTree : TRawBytes; params : TPCJSONObject) : boolean;
  var errors : AnsiString;
    OperationsHashTree : TOperationsHashTree;
    accountpubkey : TAccountKey;
    last_n_operation : Cardinal;
  begin
    Result := false;
    if Not HexaStringToOperationsHashTree(HexaStringOperationsHashTree,OperationsHashTree,errors) then begin
      ErrorNum:=CT_RPC_ErrNum_InvalidData;
      ErrorDesc:= 'Error decoding param previous operations hash tree raw value: '+errors;
      Exit;
    end;
    try
      If Not CapturePubKey('signer_',accountpubkey,ErrorDesc) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        ErrorDesc := 'Params "signer_b58_pubkey" or "signer_enc_pubkey" not found';
        exit;
      end;
      last_n_operation := params.AsCardinal('last_n_operation',0);
      If not SignBuyAccountEx(params,OperationsHashTree,accountpubkey,last_n_operation) then Exit
      else Result := True;
      FillOperationsHashTreeToJSONObject(OperationsHashTree,GetResultObject);
    finally
      OperationsHashTree.Free;
    end;
  end;

  function ListAccountForSale(params : TPCJSONObject) : boolean;
  Var OperationsHashTree : TOperationsHashTree;
    account_signer, account_target : TAccount;
    opt : TPCOperation;
    opr : TOperationResume;
    errors : AnsiString;
    c_account : Cardinal;
  Begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := False;
      OperationsHashTree := TOperationsHashTree.Create;
      try
        if (params.IndexOfName('account_signer')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_signer param';
          Exit;
        end;
        c_account := params.AsCardinal('account_signer',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_signer '+params.AsString('account_signer','');
          Exit;
        end;
        account_signer := FNode.Operations.SafeBoxTransaction.Account(c_account);
        if (params.IndexOfName('account_target')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_target param';
          Exit;
        end;
        c_account := params.AsCardinal('account_target',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_target '+params.AsString('account_target','');
          Exit;
        end;
        account_target := FNode.Operations.SafeBoxTransaction.Account(c_account);
        if (Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey)) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'account_signer and account_target have distinct keys. Cannot sign';
          Exit;
        end;
        If not SignListAccountForSaleEx(params,OperationsHashTree,account_signer.accountInfo.accountKey,account_signer.n_operation) then Exit;
        opt := OperationsHashTree.GetOperation(0);
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          Exit;
        end else Result := True;
        TPCOperation.OperationToOperationResume(0,opt,c_account,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  End;

  function DelistAccountForSale(params : TPCJSONObject) : boolean;
  Var OperationsHashTree : TOperationsHashTree;
    account_signer, account_target : TAccount;
    opt : TPCOperation;
    opr : TOperationResume;
    errors : AnsiString;
    c_account : Cardinal;
  Begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := False;
      OperationsHashTree := TOperationsHashTree.Create;
      try
        if (params.IndexOfName('account_signer')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_signer param';
          Exit;
        end;
        c_account := params.AsCardinal('account_signer',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_signer '+params.AsString('account_signer','');
          Exit;
        end;
        account_signer := FNode.Operations.SafeBoxTransaction.Account(c_account);
        if (params.IndexOfName('account_target')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_target param';
          Exit;
        end;
        c_account := params.AsCardinal('account_target',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_target '+params.AsString('account_target','');
          Exit;
        end;
        account_target := FNode.Operations.SafeBoxTransaction.Account(c_account);
        if (Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey)) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'account_signer and account_target have distinct keys. Cannot sign';
          Exit;
        end;
        If not SignDelistAccountForSaleEx(params,OperationsHashTree,account_signer.accountInfo.accountKey,account_signer.n_operation) then Exit;
        opt := OperationsHashTree.GetOperation(0);
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          Exit;
        end else Result := True;
        TPCOperation.OperationToOperationResume(0,opt,c_account,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.OperationSequenceLock.Acquire;
    end;
  End;

  function BuyAccount(params : TPCJSONObject) : boolean;
  Var OperationsHashTree : TOperationsHashTree;
    buyer_account : TAccount;
    opt : TPCOperation;
    opr : TOperationResume;
    errors : AnsiString;
    c_account : Cardinal;
  Begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := False;
      OperationsHashTree := TOperationsHashTree.Create;
      try
        if (params.IndexOfName('buyer_account')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need buyer_account param';
          Exit;
        end;
        c_account := params.AsCardinal('buyer_account',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account '+params.AsString('buyer_account','');
          Exit;
        end;
        buyer_account := FNode.Operations.SafeBoxTransaction.Account(c_account);
        If not SignBuyAccountEx(params,OperationsHashTree,buyer_account.accountInfo.accountKey,buyer_account.n_operation) then Exit;
        opt := OperationsHashTree.GetOperation(0);
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          Exit;
        end else Result := True;
        TPCOperation.OperationToOperationResume(0,opt,c_account,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  End;

  function ChangeAccountInfo(params : TPCJSONObject) : boolean;
  Var OperationsHashTree : TOperationsHashTree;
    account_signer, account_target : TAccount;
    opt : TPCOperation;
    opr : TOperationResume;
    errors : AnsiString;
    c_account : Cardinal;
  Begin
    FNode.OperationSequenceLock.Acquire;  // Use lock to prevent N_Operation race-condition on concurrent invocations
    try
      Result := False;
      OperationsHashTree := TOperationsHashTree.Create;
      try
        if (params.IndexOfName('account_signer')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_signer param';
          Exit;
        end;
        c_account := params.AsCardinal('account_signer',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_signer '+params.AsString('account_signer','');
          Exit;
        end;
        account_signer := FNode.Operations.SafeBoxTransaction.Account(c_account);
        if (params.IndexOfName('account_target')<0) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Need account_target param';
          Exit;
        end;
        c_account := params.AsCardinal('account_target',0);
        if (c_account<0) or (c_account>=FNode.Bank.AccountsCount) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'Invalid account_target '+params.AsString('account_target','');
          Exit;
        end;
        account_target := FNode.Operations.SafeBoxTransaction.Account(c_account);
        if (Not TAccountComp.EqualAccountKeys(account_signer.accountInfo.accountKey,account_target.accountInfo.accountKey)) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidAccount;
          ErrorDesc := 'account_signer and account_target have distinct keys. Cannot sign';
          Exit;
        end;
        If not SignChangeAccountInfoEx(params,OperationsHashTree,account_signer.accountInfo.accountKey,account_signer.n_operation) then Exit;
        opt := OperationsHashTree.GetOperation(0);
        If not FNode.AddOperation(Nil,opt,errors) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := errors;
          Exit;
        end else Result := True;
        TPCOperation.OperationToOperationResume(0,opt,c_account,opr);
        FillOperationResumeToJSONObject(opr,GetResultObject);
      finally
        OperationsHashTree.Free;
      end;
    finally
      FNode.OperationSequenceLock.Release;
    end;
  End;

  function FindAccounts(params : TPCJSONObject; var output: TPCJSONArray) : boolean;
  var
    accountName : TRawBytes;
    accountType : Integer;
    accountNumber : Integer;
    start, max : Integer;
    account : TAccount;
    i : Cardinal;
    errors : AnsiString;
  begin
    // Get Parameters
    Result := False;
    accountName := LowerCase(params.AsString('name', '')); // Convert to lowercase...
    accountType := params.AsInteger('type', -1);
    start := params.AsInteger('start', 0);
    max := params.AsInteger('max', 100);

    // Validate Parameters
    if accountName <> '' then begin
      if not FNode.Bank.SafeBox.ValidAccountName(accountName, errors) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidAccountName;
        ErrorDesc := errors;
        exit;
      end;
    end;

    if max <= 0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidData;
      ErrorDesc := '"max" param must be greater than zero';
      exit;
    end;

    // Declare return result (empty by default)
    output := jsonresponse.GetAsArray('result');

    // Search by name
    if accountName <> '' then begin
       accountNumber := FNode.Bank.SafeBox.FindAccountByName(accountName);
       if accountNumber >= 0 then begin
          account := FNode.Operations.SafeBoxTransaction.Account(accountNumber);
          if (accountType = -1) OR (Integer(account.account_type) = accountType) then
             FillAccountObject(account,output.GetAsObject(output.Count));
       end;
    end else begin
      // Search by type
      for i := start to FNode.Bank.AccountsCount - 1 do begin
        account := FNode.Operations.SafeBoxTransaction.Account(i);
        if (accountType = -1) OR (Integer(account.account_type) = accountType) then begin
          // Found a match
          FillAccountObject(account,output.GetAsObject(output.Count));
          if output.Count>=max then break;
        end;
      end;
    end;
    Result := True;
  end;

  function FindNOperations : Boolean;
  Var oprl : TOperationsResumeList;
    start_block, account, n_operation_min, n_operation_max : Cardinal;
    sor : TSearchOperationResult;
    jsonarr : TPCJSONArray;
    i : Integer;
  begin
    Result := False;
    oprl := TOperationsResumeList.Create;
    try
      account := params.AsCardinal('account',MaxInt);
      If (params.IndexOfName('n_operation_min')<0) Or (params.IndexOfName('n_operation_max')<0) then begin
        ErrorNum:=CT_RPC_ErrNum_NotFound;
        ErrorDesc:='Need n_operation_min and n_operation_max params';
        exit;
      end;
      n_operation_min := params.AsCardinal('n_operation_min',0);
      n_operation_max := params.AsCardinal('n_operation_max',0);
      start_block := params.AsCardinal('start_block',0); // Optional: 0 = Search all
      sor := FNode.FindNOperations(account,start_block,true,n_operation_min,n_operation_max,oprl);
      Case sor of
        found : Result := True;
        invalid_params : begin
            ErrorNum:=CT_RPC_ErrNum_NotFound;
            ErrorDesc:='Not found using block/account/n_operation';
            exit;
          end;
        blockchain_block_not_found : begin
            ErrorNum := CT_RPC_ErrNum_InvalidBlock;
            ErrorDesc:='Blockchain file does not contain all blocks to find';
            exit;
          end;
      else Raise Exception.Create('ERROR DEV 20171120-7');
      end;
      jsonarr := jsonresponse.GetAsArray('result');
      if oprl.Count>0 then begin;
        for i:=0 to oprl.Count-1 do begin
          FillOperationResumeToJSONObject(oprl.OperationResume[i],jsonarr.GetAsObject(jsonarr.Count));
        end;
      end;
    finally
      oprl.Free;
    end;
  end;

Var c,c2,c3 : Cardinal;
  i,j,k,l : Integer;
  account : TAccount;
  senderpubkey,destpubkey : TAccountKey;
  ansistr : AnsiString;
  nsaarr : TNodeServerAddressArray;
  pcops : TPCOperationsComp;
  ecpkey : TECPrivateKey;
  opr : TOperationResume;
  r1,r2 : TRawBytes;
  ocl : TOrderedCardinalList;
  jsonarr : TPCJSONArray;
  jso : TPCJSONObject;
begin
  _ro := Nil;
  _ra := Nil;
  ErrorNum:=0;
  ErrorDesc:='';
  Result := false;
  TLog.NewLog(ltdebug,ClassName,'Processing RPC-JSON method '+method);
  if (method='addnode') then begin
    // Param "nodes" contains ip's and ports in format "ip1:port1;ip2:port2 ...". If port is not specified, use default
    // Returns quantity of nodes added
    TNode.DecodeIpStringToNodeServerAddressArray(params.AsString('nodes',''),nsaarr);
    for i:=low(nsaarr) to high(nsaarr) do begin
      TNetData.NetData.AddServer(nsaarr[i]);
    end;
    jsonresponse.GetAsVariant('result').Value:=length(nsaarr);
    Result := true;
  end else if (method='getaccount') then begin
    // Param "account" contains account number
    // Returns JSON Object with account information based on BlockChain + Pending operations
    c := params.GetAsVariant('account').AsCardinal(CT_MaxAccount);
    if (c>=0) And (c<FNode.Bank.AccountsCount) then begin
      account := FNode.Operations.SafeBoxTransaction.Account(c);
      FillAccountObject(account,GetResultObject);
      Result := True;
    end else begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      if (c=CT_MaxAccount) then ErrorDesc := 'Need "account" param'
      else ErrorDesc := 'Account not found: '+IntToStr(c);
    end;
  end else if (method='findaccounts') then begin
    Result := FindAccounts(params, jsonarr);
  end else if (method='getwalletaccounts') then begin
    // Returns JSON array with accounts in Wallet
    jsonarr := jsonresponse.GetAsArray('result');
    if (params.IndexOfName('enc_pubkey')>=0) Or (params.IndexOfName('b58_pubkey')>=0) then begin
      if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
      i := _RPCServer.WalletKeys.AccountsKeyList.IndexOfAccountKey(opr.newKey);
      if (i<0) then begin
        ErrorNum := CT_RPC_ErrNum_NotFound;
        ErrorDesc := 'Public key not found in wallet';
        exit;
      end;
      ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
      k := params.AsInteger('max',100);
      l := params.AsInteger('start',0);
      for j := 0 to ocl.Count - 1 do begin
        if (j>=l) then begin
          account := FNode.Operations.SafeBoxTransaction.Account(ocl.Get(j));
          FillAccountObject(account,jsonarr.GetAsObject(jsonarr.Count));
        end;
        if (k>0) And ((j+1)>=(k+l)) then break;
      end;
      Result := true;
    end else begin
      k := params.AsInteger('max',100);
      l := params.AsInteger('start',0);
      c := 0;
      for i:=0 to _RPCServer.WalletKeys.AccountsKeyList.Count-1 do begin
        ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
        for j := 0 to ocl.Count - 1 do begin
          if (c>=l) then begin
            account := FNode.Operations.SafeBoxTransaction.Account(ocl.Get(j));
            FillAccountObject(account,jsonarr.GetAsObject(jsonarr.Count));
          end;
          inc(c);
          if (k>0) And (c>=(k+l)) then break;
        end;
        if (k>0) And (c>=(k+l)) then break;
      end;
      Result := true;
    end;
  end else if (method='getwalletaccountscount') then begin
    // New Build 1.1.1
    // Returns a number with count value
    if (params.IndexOfName('enc_pubkey')>=0) Or (params.IndexOfName('b58_pubkey')>=0) then begin
      if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
      i := _RPCServer.WalletKeys.AccountsKeyList.IndexOfAccountKey(opr.newKey);
      if (i<0) then begin
        ErrorNum := CT_RPC_ErrNum_NotFound;
        ErrorDesc := 'Public key not found in wallet';
        exit;
      end;
      ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
      jsonresponse.GetAsVariant('result').value := ocl.count;
      Result := true;
    end else begin
      ErrorDesc := '';
      c :=0;
      for i:=0 to _RPCServer.WalletKeys.AccountsKeyList.Count-1 do begin
        ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
        inc(c,ocl.count);
      end;
      jsonresponse.GetAsVariant('result').value := c;
      Result := true;
    end;
  end else if (method='getwalletcoins') then begin
    if (params.IndexOfName('enc_pubkey')>=0) Or (params.IndexOfName('b58_pubkey')>=0) then begin
      if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
      i := _RPCServer.WalletKeys.AccountsKeyList.IndexOfAccountKey(opr.newKey);
      if (i<0) then begin
        ErrorNum := CT_RPC_ErrNum_NotFound;
        ErrorDesc := 'Public key not found in wallet';
        exit;
      end;
      ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
      account.balance := 0;
      for j := 0 to ocl.Count - 1 do begin
        inc(account.balance, FNode.Operations.SafeBoxTransaction.Account(ocl.Get(j)).balance );
      end;
      jsonresponse.GetAsVariant('result').value := ToJSONCurrency(account.balance);
      Result := true;
    end else begin
      ErrorDesc := '';
      c :=0;
      account.balance := 0;
      for i:=0 to _RPCServer.WalletKeys.AccountsKeyList.Count-1 do begin
        ocl := _RPCServer.WalletKeys.AccountsKeyList.AccountKeyList[i];
        for j := 0 to ocl.Count - 1 do begin
          inc(account.balance, FNode.Operations.SafeBoxTransaction.Account(ocl.Get(j)).balance );
        end;
      end;
      jsonresponse.GetAsVariant('result').value := ToJSONCurrency(account.balance);
      Result := true;
    end;
  end else if (method='getwalletpubkeys') then begin
    // Returns JSON array with pubkeys in wallet
    k := params.AsInteger('max',100);
    j := params.AsInteger('start',0);
    jsonarr := jsonresponse.GetAsArray('result');
    for i:=0 to _RPCServer.WalletKeys.Count-1 do begin
      if (i>=j) then begin
        jso := jsonarr.GetAsObject(jsonarr.count);
        jso.GetAsVariant('name').Value := _RPCServer.WalletKeys.Key[i].Name;
        jso.GetAsVariant('can_use').Value := (_RPCServer.WalletKeys.Key[i].CryptedKey<>'');
        FillPublicKeyObject(_RPCServer.WalletKeys.Key[i].AccountKey,jso);
      end;
      if (k>0) And ((i+1)>=(j+k)) then break;
    end;
    Result := true;
  end else if (method='getwalletpubkey') then begin
    if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    i := _RPCServer.WalletKeys.AccountsKeyList.IndexOfAccountKey(opr.newKey);
    if (i<0) then begin
      ErrorNum := CT_RPC_ErrNum_NotFound;
      ErrorDesc := 'Public key not found in wallet';
      exit;
    end;
    FillPublicKeyObject(_RPCServer.WalletKeys.AccountsKeyList.AccountKey[i],GetResultObject);
    Result := true;
  end else if (method='getblock') then begin
    // Param "block" contains block number (0..getblockcount-1)
    // Returns JSON object with block information
    c := params.GetAsVariant('block').AsCardinal(CT_MaxBlock);
    if (c>=0) And (c<FNode.Bank.BlocksCount) then begin
      Result := GetBlock(c,GetResultObject);
    end else begin
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
      if (c=CT_MaxBlock) then ErrorDesc := 'Need block param'
      else ErrorDesc := 'Block not found: '+IntToStr(c);
    end;
  end else if (method='getblocks') then begin
    // Param "start" "end" contains blocks number (0..getblockcount-1)
    // Returns JSON Array with blocks information (limited to 1000 blocks)
    // Sorted by DESCENDING blocknumber
    i := params.AsCardinal('last',0);
    if (i>0) then begin
      if (i>1000) then i := 1000;
      c2 := FNode.Bank.BlocksCount-1;
      if (FNode.Bank.BlocksCount>=i) then
        c := (FNode.Bank.BlocksCount) - i
      else c := 0;
    end else begin
      c := params.GetAsVariant('start').AsCardinal(CT_MaxBlock);
      c2 := params.GetAsVariant('end').AsCardinal(CT_MaxBlock);
      i := params.AsInteger('max',0);
      if (c<FNode.Bank.BlocksCount) And (i>0) And (i<=1000) then begin
        if (c+i<FNode.Bank.BlocksCount) then c2 := c+i
        else c2 := FNode.Bank.BlocksCount-1;
      end;
    end;
    if ((c>=0) And (c<FNode.Bank.BlocksCount)) And (c2>=c) And (c2<FNode.Bank.BlocksCount) then begin
      i := 0; Result := true;
      while (c<=c2) And (Result) And (i<1000) do begin
        Result := GetBlock(c2,jsonresponse.GetAsArray('result').GetAsObject(i));
        dec(c2); inc(i);
      end;
    end else begin
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
      if (c>c2) then ErrorDesc := 'Block start > block end'
      else if (c=CT_MaxBlock) Or (c2=CT_MaxBlock) then ErrorDesc:='Need param "last" or "start" and "end"/"max"'
      else if (c2>=FNode.Bank.BlocksCount) then ErrorDesc := 'Block higher or equal to getblockccount: '+IntToStr(c2)
      else  ErrorDesc := 'Block not found: '+IntToStr(c);
    end;
  end else if (method='getblockcount') then begin
    // Returns a number with Node blocks count
    jsonresponse.GetAsVariant('result').Value:=FNode.Bank.BlocksCount;
    Result := True;
  end else if (method='getblockoperation') then begin
    // Param "block" contains block. Null = Pending operation
    // Param "opblock" contains operation inside a block: (0..getblock.operations-1)
    // Returns a JSON object with operation values as "Operation resume format"
    c := params.GetAsVariant('block').AsCardinal(CT_MaxBlock);
    if (c>=0) And (c<FNode.Bank.BlocksCount) then begin
      pcops := TPCOperationsComp.Create(Nil);
      try
        If Not FNode.Bank.LoadOperations(pcops,c) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := 'Cannot load Block: '+IntToStr(c);
          Exit;
        end;
        i := params.GetAsVariant('opblock').AsInteger(0);
        if (i<0) Or (i>=pcops.Count) then begin
          ErrorNum := CT_RPC_ErrNum_InvalidOperation;
          ErrorDesc := 'Block/Operation not found: '+IntToStr(c)+'/'+IntToStr(i)+' BlockOperations:'+IntToStr(pcops.Count);
          Exit;
        end;
        If TPCOperation.OperationToOperationResume(c,pcops.Operation[i],pcops.Operation[i].SignerAccount,opr) then begin
          opr.NOpInsideBlock:=i;
          opr.time:=pcops.OperationBlock.timestamp;
          opr.Balance := -1;
          FillOperationResumeToJSONObject(opr,GetResultObject);
        end;
        Result := True;
      finally
        pcops.Free;
      end;
    end else begin
      If (c=CT_MaxBlock) then ErrorDesc := 'Need block param'
      else ErrorDesc := 'Block not found: '+IntToStr(c);
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
    end;
  end else if (method='getblockoperations') then begin
    // Param "block" contains block
    // Returns a JSON array with items as "Operation resume format"
    c := params.GetAsVariant('block').AsCardinal(CT_MaxBlock);
    if (c>=0) And (c<FNode.Bank.BlocksCount) then begin
      pcops := TPCOperationsComp.Create(Nil);
      try
        If Not FNode.Bank.LoadOperations(pcops,c) then begin
          ErrorNum := CT_RPC_ErrNum_InternalError;
          ErrorDesc := 'Cannot load Block: '+IntToStr(c);
          Exit;
        end;
        jsonarr := GetResultArray;
        k := params.AsInteger('max',100);
        j := params.AsInteger('start',0);
        for i := 0 to pcops.Count - 1 do begin
          if (i>=j) then begin
            If TPCOperation.OperationToOperationResume(c,pcops.Operation[i],pcops.Operation[i].SignerAccount,opr) then begin
              opr.NOpInsideBlock:=i;
              opr.time:=pcops.OperationBlock.timestamp;
              opr.Balance := -1; // Don't include!
              FillOperationResumeToJSONObject(opr,jsonarr.GetAsObject(jsonarr.Count));
            end;
          end;
          if (k>0) And ((i+1)>=(j+k)) then break;
        end;
        Result := True;
      finally
        pcops.Free;
      end;
    end else begin
      If (c=CT_MaxBlock) then ErrorDesc := 'Need block param'
      else ErrorDesc := 'Block not found: '+IntToStr(c);
      ErrorNum := CT_RPC_ErrNum_InvalidBlock;
    end;
  end else if (method='getaccountoperations') then begin
    // Returns all the operations affecting an account in "Operation resume format" as an array
    // Param "account" contains account number
    // Param "depht" (optional or "deep") contains max blocks deep to search (Default: 100)
    // Param "start" and "max" contains starting index and max operations respectively
    // Param "startblock" forces to start searching backwards on a fixed block, will not give balance for each operation due it's unknown
    c := params.GetAsVariant('account').AsCardinal(CT_MaxAccount);
    if ((c>=0) And (c<FNode.Bank.AccountsCount)) then begin
      if (params.IndexOfName('depth')>=0) then i := params.AsInteger('depth',100) else i:=params.AsInteger('deep',100);
      If params.IndexOfName('startblock')>=0 then c2 := params.AsCardinal('startblock',0)
      else c2 := 0;
      Result := GetAccountOperations(c,GetResultArray,i,params.AsInteger('start',0),params.AsInteger('max',100),c2);
    end else begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      If (c=CT_MaxAccount) then ErrorDesc := 'Need account param'
      else ErrorDesc := 'Account not found: '+IntToStr(c);
    end;
  end else if (method='getpendings') then begin
    // Returns all the operations pending to be included in a block in "Operation resume format" as an array
    // Create result
    GetResultArray;
    for i:=FNode.Operations.Count-1 downto 0 do begin
      if not TPCOperation.OperationToOperationResume(0,FNode.Operations.Operation[i],FNode.Operations.Operation[i].SignerAccount,opr) then begin
        ErrorNum := CT_RPC_ErrNum_InternalError;
        ErrorDesc := 'Error converting data';
        exit;
      end;
      opr.NOpInsideBlock:=i;
      opr.Balance := FNode.Operations.SafeBoxTransaction.Account(FNode.Operations.Operation[i].SignerAccount).balance;
      FillOperationResumeToJSONObject(opr,GetResultArray.GetAsObject( FNode.Operations.Count-1-i ));
    end;
    Result := true;
  end else if (method='decodeophash') then begin
    // Search for an operation based on "ophash"
    r1 := TCrypto.HexaToRaw(params.AsString('ophash',''));
    if (r1='') then begin
      ErrorNum:=CT_RPC_ErrNum_NotFound;
      ErrorDesc:='param ophash not found or invalid hexadecimal value "'+params.AsString('ophash','')+'"';
      exit;
    end;
    If not TPCOperation.DecodeOperationHash(r1,c,c2,c3,r2) then begin
      ErrorNum:=CT_RPC_ErrNum_NotFound;
      ErrorDesc:='invalid ophash param value';
      exit;
    end;
    GetResultObject.GetAsVariant('block').Value:=c;
    GetResultObject.GetAsVariant('account').Value:=c2;
    GetResultObject.GetAsVariant('n_operation').Value:=c3;
    GetResultObject.GetAsVariant('md160hash').Value:=TCrypto.ToHexaString(r2);
    Result := true;
  end else if (method='findoperation') then begin
    // Search for an operation based on "ophash"
    r1 := TCrypto.HexaToRaw(params.AsString('ophash',''));
    if (r1='') then begin
      ErrorNum:=CT_RPC_ErrNum_NotFound;
      ErrorDesc:='param ophash not found or invalid hexadecimal value "'+params.AsString('ophash','')+'"';
      exit;
    end;
    pcops := TPCOperationsComp.Create(Nil);
    try
      Case FNode.FindOperationExt(pcops,r1,c,i) of
        found : ;
        invalid_params : begin
            ErrorNum:=CT_RPC_ErrNum_NotFound;
            ErrorDesc:='ophash not found: "'+params.AsString('ophash','')+'"';
            exit;
          end;
        blockchain_block_not_found : begin
            ErrorNum := CT_RPC_ErrNum_InternalError;
            ErrorDesc:='Blockchain block '+IntToStr(c)+' not found to search ophash: "'+params.AsString('ophash','')+'"';
            exit;
          end;
      else Raise Exception.Create('ERROR DEV 20171120-4');
      end;
      If not TPCOperation.OperationToOperationResume(c,pcops.Operation[i],pcops.Operation[i].SignerAccount,opr) then begin
        ErrorNum := CT_RPC_ErrNum_InternalError;
        ErrorDesc := 'Error 20161026-1';
      end;
      opr.NOpInsideBlock:=i;
      opr.time:=pcops.OperationBlock.timestamp;
      opr.Balance := -1; // don't include
      FillOperationResumeToJSONObject(opr,GetResultObject);
      Result := True;
    finally
      pcops.Free;
    end;
  end else if (method='findnoperation') then begin
    // Search for an operation signed by "account" and with "n_operation", start searching "block" (0=all)
    // "block" = 0 search in all blocks, pending operations included
    Case FNode.FindNOperation(params.AsCardinal('block',0),params.AsCardinal('account',MaxInt),params.AsCardinal('n_operation',0),opr) of
      found : ;
      invalid_params : begin
          ErrorNum:=CT_RPC_ErrNum_NotFound;
          ErrorDesc:='Not found using block/account/n_operation';
          exit;
        end;
      blockchain_block_not_found : begin
          ErrorNum := CT_RPC_ErrNum_InvalidBlock;
          ErrorDesc:='Blockchain file does not contain all blocks to find';
          exit;
        end;
    else Raise Exception.Create('ERROR DEV 20171120-5');
    end;
    FillOperationResumeToJSONObject(opr,GetResultObject);
    Result := True;
  end else if (method='findnoperations') then begin
    // Search for all operations signed by "account" and n_operation value between "n_operation_min" and "n_operation_max", start searching at "block" (0=all)
    // "block" = 0 search in all blocks, pending operations included
    Result := findNOperations;
  end else if (method='sendto') then begin
    // Sends "amount" coins from "sender" to "target" with "fee"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operation Resume format" object when successfull
    // Note: "ophash" will contain block "0" = "pending block"
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    Result := OpSendTo(params.AsCardinal('sender',CT_MaxAccount),params.AsCardinal('target',CT_MaxAccount),
       ToPascalCoins(params.AsDouble('amount',0)),
       ToPascalCoins(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='signsendto') then begin
    // Create a Transaction operation and adds it into a "rawoperations" (that can include
    // previous operations). This RPC method is usefull for cold storage, because doesn't
    // need to check or verify accounts status/public key, assuming that passed values
    // are ok.
    // Signs a transaction of "amount" coins from "sender" to "target" with "fee", using "sender_enc_pubkey" or "sender_b58_pubkey"
    // and "last_n_operation" of sender. Also, needs "target_enc_pubkey" or "target_b58_pubkey"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operations info" containing old "rawoperations" plus new Transaction
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    If Not CapturePubKey('sender_',senderpubkey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    If Not CapturePubKey('target_',destpubkey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := SignOpSendTo(
       params.AsString('rawoperations',''),
       params.AsCardinal('sender',CT_MaxAccount),params.AsCardinal('target',CT_MaxAccount),
       senderpubkey,destpubkey,
       params.AsCardinal('last_n_operation',0),
       ToPascalCoins(params.AsDouble('amount',0)),
       ToPascalCoins(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='changekey') then begin
    // Change key of "account" to "new_enc_pubkey" or "new_b58_pubkey" (encoded public key format) with "fee"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operation Resume format" object when successfull
    // Note: "ophash" will contain block "0" = "pending block"
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('account')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account" param';
      exit;
    end;
    c := params.AsCardinal('account',CT_MaxAccount);
    if params.IndexOfName('account_signer')>=0 then begin
      c2 := params.AsCardinal('account_signer',CT_MaxAccount);
    end else c2 := c;
    If Not CapturePubKey('new_',account.accountInfo.accountKey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := ChangeAccountKey(c2,c,
       account.accountInfo.accountKey,
       ToPascalCoins(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='changekeys') then begin
    // Allows a massive change key operation
    // Change key of "accounts" to "new_enc_pubkey" or "new_b58_pubkey" (encoded public key format) with "fee"
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON object with result information
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('accounts')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "accounts" param';
      exit;
    end;
    If Not CapturePubKey('new_',account.accountInfo.accountKey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := ChangeAccountsKey(params.AsString('accounts',''),
       account.accountInfo.accountKey,
       ToPascalCoins(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='signchangekey') then begin
    // Create a Change Key operation and adds it into a "rawoperations" (that can include
    // previous operations). This RPC method is usefull for cold storage, because doesn't
    // need to check or verify accounts status/public key, assuming that passed values
    // are ok.
    // Signs a change key of "account" to "new_enc_pubkey" or "new_b58_pubkey" (encoded public key format) with "fee"
    // needs "old_enc_pubkey" or "old_b58_pubkey" that will be used to find private key in wallet to sign
    // and "last_n_operation" of account.
    // If "payload" is present, it will be encoded using "payload_method"
    // "payload_method" types: "none","dest"(default),"sender","aes"(must provide "pwd" param)
    // Returns a JSON "Operations info" containing old "rawoperations" plus new Transaction
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    if params.IndexOfName('account')<0 then begin
      ErrorNum := CT_RPC_ErrNum_InvalidAccount;
      ErrorDesc := 'Need "account" param';
      exit;
    end;
    c := params.AsCardinal('account',CT_MaxAccount);
    if params.IndexOfName('account_signer')>=0 then begin
      c2 := params.AsCardinal('account_signer',CT_MaxAccount);
    end else c2 := c;
    If Not CapturePubKey('old_',senderpubkey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    If Not CapturePubKey('new_',destpubkey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    Result := SignOpChangeKey(params.AsString('rawoperations',''),c2,c,
       senderpubkey,destpubkey,
       params.AsCardinal('last_n_operation',0),
       ToPascalCoins(params.AsDouble('fee',0)),
       TCrypto.HexaToRaw(params.AsString('payload','')),
       params.AsString('payload_method','dest'),params.AsString('pwd',''));
  end else if (method='listaccountforsale') then begin
    // Will put a single account in "for sale" state
    Result := ListAccountForSale(params);
  end else if (method='signlistaccountforsale') then begin
    Result := SignListAccountForSaleColdWallet(params.AsString('rawoperations',''),params);
  end else if (method='delistaccountforsale') then begin
    Result := DelistAccountForSale(params);
  end else if (method='signdelistaccountforsale') then begin
    Result := SignDelistAccountForSaleColdWallet(params.AsString('rawoperations',''),params);
  end else if (method='buyaccount') then begin
    Result := BuyAccount(params);
  end else if (method='signbuyaccount') then begin
    Result := SignBuyAccountColdWallet(params.AsString('rawoperations',''),params);
  end else if (method='changeaccountinfo') then begin
    Result := ChangeAccountInfo(params);
  end else if (method='signchangeaccountinfo') then begin
    Result := SignChangeAccountInfoColdWallet(params.AsString('rawoperations',''),params);
  //
  end else if (method='operationsinfo') then begin
    Result := OperationsInfo(params.AsString('rawoperations',''),GetResultArray);
  end else if (method='executeoperations') then begin
    Result := ExecuteOperations(params.AsString('rawoperations',''));

  //
  end else if (method='nodestatus') then begin
    // Returns a JSON Object with Node status
    GetResultObject.GetAsVariant('ready').Value := False;
    If FNode.IsReady(ansistr) then begin
      GetResultObject.GetAsVariant('ready_s').Value := ansistr;
      if TNetData.NetData.NetStatistics.ActiveConnections>0 then begin
        GetResultObject.GetAsVariant('ready').Value := True;
        if TNetData.NetData.IsDiscoveringServers then begin
          GetResultObject.GetAsVariant('status_s').Value := 'Discovering servers';
        end else if TNetData.NetData.IsGettingNewBlockChainFromClient then begin
          GetResultObject.GetAsVariant('status_s').Value := 'Obtaining new blockchain';
        end else begin
          GetResultObject.GetAsVariant('status_s').Value := 'Running';
        end;
      end else begin
        GetResultObject.GetAsVariant('ready_s').Value := 'Alone in the world...';
      end;
    end else begin
      GetResultObject.GetAsVariant('ready_s').Value := ansistr;
    end;
    GetResultObject.GetAsVariant('port').Value:=FNode.NetServer.Port;
    GetResultObject.GetAsVariant('locked').Value:=Not _RPCServer.WalletKeys.IsValidPassword;
    GetResultObject.GetAsVariant('timestamp').Value:=UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    GetResultObject.GetAsVariant('version').Value:=CT_ClientAppVersion;
    GetResultObject.GetAsObject('netprotocol').GetAsVariant('ver').Value := CT_NetProtocol_Version;
    GetResultObject.GetAsObject('netprotocol').GetAsVariant('ver_a').Value := CT_NetProtocol_Available;
    GetResultObject.GetAsVariant('blocks').Value:=FNode.Bank.BlocksCount;
    GetResultObject.GetAsVariant('sbh').Value:=TCrypto.ToHexaString(FNode.Bank.LastOperationBlock.initial_safe_box_hash);
    GetResultObject.GetAsVariant('pow').Value:=TCrypto.ToHexaString(FNode.Bank.LastOperationBlock.proof_of_work);
    GetResultObject.GetAsObject('netstats').GetAsVariant('active').Value:=TNetData.NetData.NetStatistics.ActiveConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('clients').Value:=TNetData.NetData.NetStatistics.ClientsConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('servers').Value:=TNetData.NetData.NetStatistics.ServersConnectionsWithResponse;
    GetResultObject.GetAsObject('netstats').GetAsVariant('servers_t').Value:=TNetData.NetData.NetStatistics.ServersConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('total').Value:=TNetData.NetData.NetStatistics.TotalConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('tclients').Value:=TNetData.NetData.NetStatistics.TotalClientsConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('tservers').Value:=TNetData.NetData.NetStatistics.TotalServersConnections;
    GetResultObject.GetAsObject('netstats').GetAsVariant('breceived').Value:=TNetData.NetData.NetStatistics.BytesReceived;
    GetResultObject.GetAsObject('netstats').GetAsVariant('bsend').Value:=TNetData.NetData.NetStatistics.BytesSend;
    nsaarr := TNetData.NetData.NodeServersAddresses.GetValidNodeServers(true,20);
    for i := low(nsaarr) to High(nsaarr) do begin
      jso := GetResultObject.GetAsArray('nodeservers').GetAsObject(i);
      jso.GetAsVariant('ip').Value := nsaarr[i].ip;
      jso.GetAsVariant('port').Value := nsaarr[i].port;
      jso.GetAsVariant('lastcon').Value := nsaarr[i].last_connection;
      jso.GetAsVariant('attempts').Value := nsaarr[i].total_failed_attemps_to_connect;
    end;
    Result := True;
  end else if (method='encodepubkey') then begin
    // Creates a encoded public key based on params
    // Param "ec_nid" can be 714=secp256k1 715=secp384r1 729=secp283k1 716=secp521r1
    // Param "x","y" are x and y ec public keys values in hexadecimal based on ec_nid
    // Returns a hexadecimal value containing encoded public key
    account.accountInfo.accountKey.EC_OpenSSL_NID:=params.AsInteger('ec_nid',0);
    account.accountInfo.accountKey.x:=TCrypto.HexaToRaw(params.AsString('x',''));
    account.accountInfo.accountKey.y:=TCrypto.HexaToRaw(params.AsString('y',''));
    if (account.accountInfo.accountKey.EC_OpenSSL_NID=0) Or (account.accountInfo.accountKey.x='') Or (account.accountInfo.accountKey.y='') then begin
      ErrorDesc:= 'Need params "ec_nid","x","y" to encodepubkey';
      ErrorNum:= CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if TAccountComp.IsValidAccountKey(account.accountInfo.accountKey,ansistr) then begin
      jsonresponse.GetAsVariant('result').Value:=TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(account.accountInfo.accountKey));
      Result := True;
    end else begin
      ErrorDesc:= ansistr;
      ErrorNum:= CT_RPC_ErrNum_InvalidPubKey;
    end;
  end else if (method='decodepubkey') then begin
    // Returns "ec_nid", "x" and "y" of an encoded public key (x and y in hexadecimal)
    // Must provide:
    // - Param "enc_pubkey" is an hexadecimal encoded public key (see 'encodepubkey')
    // or
    // - Param "b58_pubkey" is a Base58 encoded public key
    If Not CapturePubKey('',account.accountInfo.accountKey,ErrorDesc) then begin
      ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
      exit;
    end;
    if (TAccountComp.IsValidAccountKey(account.accountInfo.accountKey,ansistr)) then begin
      FillPublicKeyObject(account.accountInfo.accountKey,GetResultObject);
      Result := True;
    end else begin
      ErrorDesc:= ansistr;
      ErrorNum:= CT_RPC_ErrNum_InvalidPubKey;
    end;
  end else if (method='payloadencrypt') then begin
    // Encrypts a "payload" using "payload_method"
    // "payload_method" types: "none","pubkey"(must provide "enc_pubkey" or "b58_pubkey"),"aes"(must provide "pwd" param)
    // If payload is "pubkey"
    // Returns an hexa string with encrypted payload
    if (params.AsString('payload','')='') then begin
      ErrorNum:= CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "payload"';
      exit;
    end;
    opr.newKey := CT_TWalletKey_NUL.AccountKey;
    if (params.IndexOfName('enc_pubkey')>=0) Or (params.IndexOfName('b58_pubkey')>=0) then begin
      if Not (CapturePubKey('',opr.newKey,ErrorDesc)) then begin
        ErrorNum := CT_RPC_ErrNum_InvalidPubKey;
        exit;
      end;
    end;
    Result := DoEncrypt(TCrypto.HexaToRaw(params.AsString('payload','')),
       opr.newKey,
       params.AsString('payload_method',''),params.AsString('pwd',''));
  end else if (method='payloaddecrypt') then begin
    // Decrypts a "payload" searching for wallet private keys and for array of strings in "pwds" param
    // Returns an JSON Object with "result" (Boolean) and
    if (params.AsString('payload','')='') then begin
      ErrorNum:= CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "payload"';
      exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    Result := DoDecrypt(TCrypto.HexaToRaw(params.AsString('payload','')),params.GetAsArray('pwds'));
  end else if (method='getconnections') then begin
    // Returns an array of connections objects with info about state
    GetConnections;
    Result := true;
  end else if (method='addnewkey') then begin
    // Creates a new private key and stores it on the wallet, returning Public key JSON object
    // Param "ec_nid" can be 714=secp256k1 715=secp384r1 729=secp283k1 716=secp521r1. (Default = CT_Default_EC_OpenSSL_NID)
    // Param "name" is name for this address
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    ecpkey := TECPrivateKey.Create;
    try
      ecpkey.GenerateRandomPrivateKey(params.AsInteger('ec_nid',CT_Default_EC_OpenSSL_NID));
      _RPCServer.FWalletKeys.AddPrivateKey(params.AsString('name',DateTimeToStr(now)),ecpkey);
      FillPublicKeyObject(ecpkey.PublicKey,GetResultObject);
      Result := true;
    finally
      ecpkey.Free;
    end;
  end else if (method='lock') then begin
    jsonresponse.GetAsVariant('result').Value := _RPCServer.WalletKeys.LockWallet;
    Result := true;
  end else if (method='unlock') then begin
    // Unlocks the Wallet with "pwd" password
    // Returns Boolean if wallet is unlocked
    if (params.IndexOfName('pwd')<0) then begin
      ErrorNum:= CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "pwd"';
      exit;
    end;
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      _RPCServer.WalletKeys.WalletPassword:=params.AsString('pwd','');
    end;
    jsonresponse.GetAsVariant('result').Value := _RPCServer.WalletKeys.IsValidPassword;
    Result := true;
  end else if (method='setwalletpassword') then begin
    // Changes the Wallet password with "pwd" param
    // Must be unlocked first
    // Returns Boolean if wallet password changed
    If Not _RPCServer.WalletKeys.IsValidPassword then begin
      ErrorNum := CT_RPC_ErrNum_WalletPasswordProtected;
      ErrorDesc := 'Wallet is password protected. Unlock first';
      exit;
    end;
    //
    if (params.IndexOfName('pwd')<0) then begin
      ErrorNum:= CT_RPC_ErrNum_InvalidData;
      ErrorDesc := 'Need param "pwd"';
      exit;
    end;
    _RPCServer.WalletKeys.WalletPassword:=params.AsString('pwd','');
    jsonresponse.GetAsVariant('result').Value := _RPCServer.WalletKeys.IsValidPassword;
    Result := true;
  end else if (method='stopnode') then begin
    // Stops communications to other nodes
    FNode.NetServer.Active := false;
    TNetData.NetData.NetConnectionsActive:=false;
    jsonresponse.GetAsVariant('result').Value := true;
    Result := true;
  end else if (method='startnode') then begin
    // Stops communications to other nodes
    FNode.NetServer.Active := true;
    TNetData.NetData.NetConnectionsActive:=true;
    jsonresponse.GetAsVariant('result').Value := true;
    Result := true;
  end else begin
    ErrorNum := CT_RPC_ErrNum_MethodNotFound;
    ErrorDesc := 'Method not found: "'+method+'"';
  end;
end;

{ TRPCServerThread }

procedure TRPCServerThread.BCExecute;
var
  ClientSock:TSocket;
begin
  with FServerSocket do begin
    CreateSocket;
    setLinger(true,10000);
    bind('0.0.0.0',Inttostr(FPort));
    listen;
    repeat
      if terminated then break;
      Try
        if canread(1000) then begin
          ClientSock:=accept;
          if lastError=0 then begin
            TRPCProcess.create(ClientSock);
          end;
        end;
      Except
        On E:Exception do begin
          TLog.NewLog(ltError,Classname,'Error '+E.ClassName+':'+E.Message);
        end;
      End;
      sleep(1);
    until false;
  end;
end;

constructor TRPCServerThread.Create(Port: Word);
begin
  TLog.NewLog(ltInfo,ClassName,'Activating RPC-JSON Server on port '+inttostr(Port));
  FServerSocket:=TTCPBlockSocket.create;
  FPort := Port;
  inherited create(false);
end;

destructor TRPCServerThread.Destroy;
begin
  TLog.NewLog(ltInfo,ClassName,'Stoping RPC-JSON Server');
  FreeAndNil(FServerSocket);
  inherited Destroy;
end;

end.
