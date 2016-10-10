unit UPoolMining;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

Uses UTCPIP, SysUtils, UThread, SyncObjs, Classes, Windows, UJSONFunctions, UNode, UCrypto,
  UAccounts;

Const
  CT_PoolMining_Method_STATUS = 'status';
  CT_PoolMining_Method_MINER_NOTIFY = 'miner-notify'; // Server message to clients to update miners PoW data
  CT_PoolMining_Method_MINER_SUBMIT = 'miner-submit'; // Client message to server to notify a PoW found

Type
  TMinerValuesForWork = Record
     block : Cardinal;
     version : Word;
     part1 : TRawBytes;
     payload_start : TRawBytes;
     part3 : TRawBytes;
     target : Cardinal;
     timestamp : Cardinal;
     target_pow : TRawBytes;
  End;

  TJSONRPCTcpIpClient = Class(TBufferedNetTcpIpClient)
  private
    FWaitingForResponseId : Cardinal;
    FMaxWaitingForResponseMiliseconds : Cardinal;
    FLastId : Cardinal;
    FReceivedBuffer : TBytes;
  protected
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure SendJSONRPCErrorResponse(const id : Variant; const error : String);
    Procedure SendJSONRPCResponse(result : TPCJSONObject; const id : Variant);
    Procedure SendJSONRPCMethod(const method : String; params : TPCJSONObject; const id : Variant);
    Function SendJSONRPCMethodAndWait(const method : String; params : TPCJSONObject; MaxWaitMiliseconds : Cardinal; resultObject : TPCJSONObject) : Boolean;
    Function DoProcessBuffer(SenderThread : TPCThread; MaxWaitMiliseconds : Cardinal; DeleteBufferOnExit : Boolean) : TPCJSONObject;
    Function GetNewId : Cardinal;
  End;

  TPoolMinerClient = Class(TJSONRPCTcpIpClient)
  private
    FMinerValuesForWork: TMinerValuesForWork;
    FOnMinerMustChangeValues: TNotifyEvent;
    procedure SetMinerValuesForWork(const Value: TMinerValuesForWork);
  protected
  public
    Constructor Create(AOwner : TComponent); override;
    Property OnMinerMustChangeValues : TNotifyEvent read FOnMinerMustChangeValues write FOnMinerMustChangeValues;
    Property MinerValuesForWork : TMinerValuesForWork read FMinerValuesForWork write SetMinerValuesForWork;
    Procedure SubmitBlockFound(Const Payload : TRawBytes; Timestamp, NOnce : Cardinal);
    Procedure DoProcessJSONObject(json : TPCJSONObject);
  End;

  TPoolMiningServer = Class(TNetTcpIpServer)
  private
    FIncomingsCounter : Integer;
    FNodeNotifyEvents : TNodeNotifyEvents;
    FMinerAccountKey: TAccountKey;
    FMinerPayload: TRawBytes;
    FClientsWins: Integer;
    FClientsCount: Integer;
    Procedure DoProcessJSON(json : TPCJSONObject; Client : TJSONRPCTcpIpClient);
    Procedure OnNodeNewBlock(Sender : TObject);
    Procedure OnNodeOperationsChanged(Sender : TObject);
    Procedure Send_mine_values_to_all;
    Procedure FillMineValue(mine_values : TPCJSONObject; Client : TJSONRPCTcpIpClient);
    Function MinerSubmit(Client : TJSONRPCTcpIpClient; params : TPCJSONObject; const id : Variant) : Boolean;
    procedure SetMinerAccountKey(const Value: TAccountKey);
    procedure SetMinerPayload(const Value: TRawBytes);
  protected
    Procedure OnNewIncommingConnection(Sender : TObject; Client : TNetTcpIpClient); override;
    procedure SetActive(const Value: Boolean); override;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    Property MinerAccountKey : TAccountKey read FMinerAccountKey write SetMinerAccountKey;
    Property MinerPayload : TRawBytes read FMinerPayload write SetMinerPayload;
    Procedure UpdateAccountAndPayload(AMinerAccountKey : TAccountKey; AMinerPayload : TRawBytes);
    Property ClientsCount : Integer read FClientsCount;
    Property ClientsWins : Integer read FClientsWins;
  End;

Const
  CT_TMinerValuesForWork_NULL : TMinerValuesForWork = (block:0;version:0;part1:'';payload_start:'';part3:'';target:0;timestamp:0;target_pow:'');

implementation

Uses ULog, Variants, UTime, UBlockChain;

{ TJSONRPCTcpIpClient }

constructor TJSONRPCTcpIpClient.Create(AOwner: TComponent);
begin
  inherited;
  FLastId := 1;
  FWaitingForResponseId := 0;
  FMaxWaitingForResponseMiliseconds := 0;
  SetLength(FReceivedBuffer,0);
end;

destructor TJSONRPCTcpIpClient.Destroy;
begin
  SetLength(FReceivedBuffer,0);
  inherited;
end;

function TJSONRPCTcpIpClient.DoProcessBuffer(SenderThread : TPCThread; MaxWaitMiliseconds : Cardinal; DeleteBufferOnExit : Boolean) : TPCJSONObject;
var last_bytes_read : Integer;
  jsonData : TPCJSONData;
  tc : Cardinal;
  ms : TMemoryStream;
  pac : PAnsiChar;
  lasti : Integer;
  continue : Boolean;
begin
  Result := Nil;
  tc := GetTickCount;
  if Assigned(SenderThread) then continue := Not SenderThread.Terminated
  else continue := true;
  while (Connected) And ((GetTickCount<=(tc+MaxWaitMiliseconds)) Or (MaxWaitMiliseconds=0)) And (continue) do begin
    last_bytes_read := 0;
    ms := ReadBufferLock;
    try
      if (ms.Size)>0 then begin
        lasti := length(FReceivedBuffer);
        setLength(FReceivedBuffer,length(FReceivedBuffer)+ms.Size);
        CopyMemory(@FReceivedBuffer[lasti],ms.Memory,ms.Size);
        last_bytes_read := ms.Size;
        ms.Size := 0;
      end;
    finally
      ReadBufferUnlock;
    end;
    if (last_bytes_read>0) then begin
      // Delete possible CR+LF or #0 at the end
      while (length(FReceivedBuffer)>1) And (FReceivedBuffer[length(FReceivedBuffer)-1] in [10,13,0]) do setLength(FReceivedBuffer,length(FReceivedBuffer)-1);
      // Decode
      jsonData := TPCJSONData.ParseJSONValue(FReceivedBuffer);
      if Assigned(jsonData) then begin
        setlength(FReceivedBuffer,0);
        if jsonData is TPCJSONObject then begin
          Result := TPCJSONObject(jsonData);
          exit;
        end else begin
          TLog.NewLog(lterror,ClassName,'Invalid JSON data: '+jsonData.ClassName);
          jsonData.Free;
        End;
      end else begin
        TLog.NewLog(ltDebug,ClassName,Format('Read %d bytes but no valid JSON inside',[last_bytes_read]));
      end;
    end;
    sleep(1);
    if Assigned(SenderThread) then continue := Not SenderThread.Terminated
    else continue := true;
  end;
  if (length(FReceivedBuffer)>0) And (DeleteBufferOnExit) then begin
    TLog.NewLog(lterror,ClassName,AnsiString( Format('Deleting %d bytes from buffer after waiting %d milis',[length(FReceivedBuffer),MaxWaitMiliseconds])));
    SetLength(FReceivedBuffer,0);
  end;
end;

function TJSONRPCTcpIpClient.GetNewId: Cardinal;
begin
  inc(FLastId);
  Result := FLastId;
end;

procedure TJSONRPCTcpIpClient.SendJSONRPCErrorResponse(const id: Variant; const error: String);
Var response : TPCJSONObject;
  stream : TMemoryStream;
  b : Byte;
begin
  TLog.NewLog(lterror,ClassName,'Sending Error JSON RPC id ('+VarToStr(id)+') : '+error);
  response := TPCJSONObject.Create;
  Try
    response.GetAsVariant('result').Value := Null;
    response.GetAsVariant('error').Value := error;
    response.GetAsVariant('id').Value := id;
    stream := TMemoryStream.Create;
    try
      response.SaveToStream(stream);
      b := 13;
      stream.Write(b,1);
      b := 10;
      stream.Write(b,1);
      b := 0;
      stream.Write(b,1);
      stream.Position := 0;
      WriteBufferToSend(stream);
    finally
      stream.Free;
    end;
  Finally
    response.Free;
  End;
end;

procedure TJSONRPCTcpIpClient.SendJSONRPCMethod(const method: String; params: TPCJSONObject; const id: Variant);
Var json : TPCJSONObject;
  stream : TMemoryStream;
  b : Byte;
begin
  json := TPCJSONObject.Create;
  Try
    json.GetAsVariant('method').Value := method;
    if Assigned(params) then begin
      json.GetAsArray('params').GetAsObject(0).Assign(params);
    end;
    json.GetAsVariant('id').Value := id;
    stream := TMemoryStream.Create;
    try
      json.SaveToStream(stream);
      b := 13;
      stream.Write(b,1);
      b := 10;
      stream.Write(b,1);
      b := 0;
      stream.Write(b,1);
      stream.Position := 0;
      WriteBufferToSend(stream);
    finally
      stream.Free;
    end;
  Finally
    json.Free;
  End;
end;

function TJSONRPCTcpIpClient.SendJSONRPCMethodAndWait(const method: String; params: TPCJSONObject; MaxWaitMiliseconds: Cardinal; resultObject : TPCJSONObject) : Boolean;
Var nId : Cardinal;
  tc : Cardinal;
  json : TPCJSONObject;
begin
  nId := GetNewId;
  Result := false;
  SendJSONRPCMethod(method,params,nId);
  tc := GetTickCount;
  json := DoProcessBuffer(nil,MaxWaitMiliseconds,true);
  if Assigned(json) then begin
    try
      resultObject.Assign(json);
      Result := true;
    finally
      json.Free;
    end;
  end;
end;

procedure TJSONRPCTcpIpClient.SendJSONRPCResponse(result: TPCJSONObject; const id: Variant);
Var response : TPCJSONObject;
  stream : TMemoryStream;
  b : Byte;
begin
  response := TPCJSONObject.Create;
  Try
    response.GetAsObject('result').Assign(result);
    response.GetAsVariant('error').Value := Null;
    response.GetAsVariant('id').Value := id;
    stream := TMemoryStream.Create;
    try
      response.SaveToStream(stream);
      b := 13;
      stream.Write(b,1);
      b := 10;
      stream.Write(b,1);
      b := 0;
      stream.Write(b,1);
      stream.Position := 0;
      WriteBufferToSend(stream);
    finally
      stream.Free;
    end;
  Finally
    response.Free;
  End;
end;

{ TPoolMiningServer }

constructor TPoolMiningServer.Create;
begin
  inherited;
  FIncomingsCounter := 0;
  FClientsWins := 0;
  FClientsCount := 0;
  NetTcpIpClientClass := TJSONRPCTcpIpClient;
  FNodeNotifyEvents := TNodeNotifyEvents.Create(Nil);
  FNodeNotifyEvents.OnBlocksChanged := OnNodeNewBlock;
  FNodeNotifyEvents.OnOperationsChanged := OnNodeOperationsChanged;
  FNodeNotifyEvents.Node := TNode.Node;
end;

destructor TPoolMiningServer.Destroy;
begin
  FNodeNotifyEvents.Node := Nil;
  FNodeNotifyEvents.OnBlocksChanged := Nil;
  FNodeNotifyEvents.OnOperationsChanged := Nil;
  FreeAndNil(FNodeNotifyEvents);
  inherited;
end;

procedure TPoolMiningServer.DoProcessJSON(json: TPCJSONObject; Client : TJSONRPCTcpIpClient);
Var method : String;
    params : TPCJSONArray;
    id_value : Variant;
    i : Integer;
  response_result : TPCJSONObject;
begin
  method := json.AsString('method','');
  params := json.GetAsArray('params');
  i := json.IndexOfName('id');
  if i<0 then begin
    id_value := Null;
  end else begin
    id_value := json.GetAsVariant('id').Value;
  end;
  if method=CT_PoolMining_Method_STATUS then begin
    response_result := TPCJSONObject.Create;
    Try
      response_result.GetAsVariant('block').Value := FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.block;
      response_result.GetAsVariant('account_key').Value := TCrypto.ToHexaString( TAccountComp.AccountKey2RawString(FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.account_key) );
      response_result.GetAsVariant('reward').Value := FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.reward;
      response_result.GetAsVariant('fee').Value := FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.fee;
      response_result.GetAsVariant('p_version').Value := FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.protocol_version;
      response_result.GetAsVariant('p_available').Value := FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.protocol_available;
      response_result.GetAsVariant('timestamp').Value := FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.timestamp;
      response_result.GetAsVariant('target').Value := FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.compact_target;
      response_result.GetAsVariant('nonce').Value := FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.nonce;
      response_result.GetAsVariant('payload').Value := TCrypto.ToHexaString( FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.block_payload );
      response_result.GetAsVariant('initial_sbh').Value := TCrypto.ToHexaString( FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.initial_safe_box_hash );
      response_result.GetAsVariant('operations_hash').Value := TCrypto.ToHexaString( FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.operations_hash );
      response_result.GetAsVariant('pow').Value := TCrypto.ToHexaString( FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.proof_of_work );
      Client.SendJSONRPCResponse(response_result,id_value);
    Finally
      response_result.Free;
    End;
  end else if method=CT_PoolMining_Method_MINER_NOTIFY then begin
    response_result := TPCJSONObject.Create;
    Try
      FillMineValue(response_result,Client);
      Client.SendJSONRPCResponse(response_result,id_value);
    Finally
      response_result.Free;
    End;
  end else if method=CT_PoolMining_Method_MINER_SUBMIT then begin
    // Try to submit a PoW
    if params.Count=1 then MinerSubmit(Client,params.GetAsObject(0),id_value)
    else TLog.NewLog(lterror,ClassName,'Invalid params array of method '+method);
  end else begin
    // Invalid command
    if (not VarIsNull(id_value)) then begin
      Client.SendJSONRPCErrorResponse(id_value,'method not found: '+method);
    end;
  end;
end;

procedure TPoolMiningServer.FillMineValue(mine_values: TPCJSONObject; Client : TJSONRPCTcpIpClient);
Var Op : TPCOperationsComp;
begin
  mine_values.GetAsVariant('block').Value := FNodeNotifyEvents.Node.Bank.LastBlockFound.OperationBlock.block;
  mine_values.GetAsVariant('version').Value := FNodeNotifyEvents.Node.Operations.OperationBlock.protocol_version;
  Op := TPCOperationsComp.Create(Nil);
  try
    Op.CopyFrom(FNodeNotifyEvents.Node.Operations);
    Op.AccountKey := FMinerAccountKey;
    Op.BlockPayload := FMinerPayload+IntToStr(Client.Tag);
    mine_values.GetAsVariant('part1').Value := TCrypto.ToHexaString( Op.PoW_Digest_Part1 );
    mine_values.GetAsVariant('payload_start').Value := TCrypto.ToHexaString( Op.OperationBlock.block_payload );
    mine_values.GetAsVariant('part3').Value := TCrypto.ToHexaString( Op.PoW_Digest_Part3 );
    mine_values.GetAsVariant('target').Value := Op.OperationBlock.compact_target;
    mine_values.GetAsVariant('target_pow').Value := TCrypto.ToHexaString(FNodeNotifyEvents.Node.Bank.GetActualTargetHash);
  finally
    Op.Free;
  end;
  mine_values.GetAsVariant('timestamp').Value := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
end;

function TPoolMiningServer.MinerSubmit(Client: TJSONRPCTcpIpClient; params: TPCJSONObject; const id : Variant): Boolean;
Var s : String;
  nbOperations : TPCOperationsComp;
  errors : AnsiString;
  nba : TBlockAccount;
  payload : TRawBytes;
  json : TPCJSONObject;
  p1,p2,p3 : TRawBytes;
begin
  { Miner params must submit:
    - "payload" as an Hexadecimal
    - "timestamp" as an unsigned integer 32 bits
    - "nonce" as an unsigned integer 32 bits
    If payload length is < Node payload then error
    If Node payload is not included in first bytes of payload then error
    If timestamp is not valid then error
    If calculated PoW does not match valid PoW then error
    If all ok... congrats!!! }
  Result := false;
  nbOperations := TPCOperationsComp.Create(Nil);
  try
    nbOperations.bank := FNodeNotifyEvents.Node.Bank;
    nbOperations.CopyFrom(FNodeNotifyEvents.Node.Operations);
    nbOperations.AccountKey := MinerAccountKey;
    s := params.AsString('payload','');
    payload := TCrypto.HexaToRaw(AnsiString(s));
    if FMinerPayload<>'' then begin
      if (copy(payload,1,length(FMinerPayload))<>FMinerPayload) then begin
        Client.SendJSONRPCErrorResponse(id,'Invalid payload ('+payload+'). Need start with: '+FMinerPayload);
        exit;
      end;
    end;
    nbOperations.BlockPayload := payload;
    nbOperations.timestamp := params.AsInteger('timestamp',0);
    nbOperations.nonce := params.AsInteger('nonce',0);
    p1 := nbOperations.PoW_Digest_Part1;
    p2 := nbOperations.PoW_Digest_Part2_Payload;
    p3 := nbOperations.PoW_Digest_Part3;
    If FNodeNotifyEvents.Node.AddNewBlockChain(nil,nil,nbOperations,nba,errors) then begin
      // CONGRATS !!!
      json := TPCJSONObject.Create;
      try
        json.GetAsVariant('block').Value := FNodeNotifyEvents.Node.Bank.LastOperationBlock.block;
        json.GetAsVariant('pow').Value := TCrypto.ToHexaString( FNodeNotifyEvents.Node.Bank.LastOperationBlock.proof_of_work );
        inc(FClientsWins);
        Client.SendJSONRPCResponse(json,id);
      finally
        json.Free;
      end;
    end else begin
      Client.SendJSONRPCErrorResponse(id,'Error: '+errors);
    end;
  finally
    nbOperations.Free;
  end;
end;

procedure TPoolMiningServer.OnNewIncommingConnection(Sender: TObject; Client: TNetTcpIpClient);
var bClient : TJSONRPCTcpIpClient;
  init_json : TPCJSONObject;
  jsonobj : TPCJSONObject;
  doDelete : Boolean;
begin
  inherited;
  inc(FClientsCount);
  Try
    TLog.NewLog(ltinfo,ClassName,'New Mining Pool Connection: '+Client.ClientRemoteAddr);
    bClient := TJSONRPCTcpIpClient(Client);
    inc(FIncomingsCounter);
    bClient.Tag := FIncomingsCounter;
    init_json := TPCJSONObject.Create;
    Try
      FillMineValue(init_json,bClient);
      bClient.SendJSONRPCMethod(CT_PoolMining_Method_MINER_NOTIFY,init_json,null);
    Finally
      init_json.Free;
    End;
    while (Active) And (Client.Connected) do begin
      doDelete := bClient.LastReadTC+1000<GetTickCount;  // TODO: Protect GetTickCount overflow
      jsonobj := bClient.DoProcessBuffer(nil,100,doDelete);
      if assigned(jsonobj) then begin
        try
          DoProcessJSON(jsonobj,bClient);
        finally
          jsonobj.Free;
        end;
      end;
      sleep(10);
    end;
  Finally
    Dec(FClientsCount);
    TLog.NewLog(ltinfo,ClassName,'Finalizing Mining Pool Connection: '+Client.ClientRemoteAddr);
  End;
end;

procedure TPoolMiningServer.OnNodeNewBlock(Sender: TObject);
begin
  // Send mine values to all clients
  Send_mine_values_to_all;
end;

procedure TPoolMiningServer.OnNodeOperationsChanged(Sender: TObject);
begin
  // Send mine values to all clients
  Send_mine_values_to_all;
end;

procedure TPoolMiningServer.Send_mine_values_to_all;
var params : TPCJSONObject;
  i : Integer;
  l : TList;
begin
  params := TPCJSONObject.Create;
  Try
    l := NetTcpIpClientsLock;
    Try
      for i := 0 to l.Count - 1 do begin
        if Not Active then exit;
        FillMineValue(params,TJSONRPCTcpIpClient(l[i]));
        TJSONRPCTcpIpClient(l[i]).SendJSONRPCMethod(CT_PoolMining_Method_MINER_NOTIFY,params,Null);
      end;
    Finally
      NetTcpIpClientsUnlock;
    End;
  Finally
    params.Free;
  End;
end;

procedure TPoolMiningServer.SetActive(const Value: Boolean);
begin
  inherited;
  if (Not Value) then begin
    WaitUntilNetTcpIpClientsFinalized;
  end;
end;


procedure TPoolMiningServer.SetMinerAccountKey(const Value: TAccountKey);
begin
  FMinerAccountKey := Value;
  TLog.NewLog(ltdebug,ClassName,'Assigning Miner account key to: '+TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(Value)));
  Send_mine_values_to_all;
end;

procedure TPoolMiningServer.SetMinerPayload(const Value: TRawBytes);
begin
  FMinerPayload := Value;
  TLog.NewLog(ltdebug,ClassName,'Assigning Miner new Payload: '+TCrypto.ToHexaString(Value));
  Send_mine_values_to_all;
end;

procedure TPoolMiningServer.UpdateAccountAndPayload(
  AMinerAccountKey: TAccountKey; AMinerPayload: TRawBytes);
begin
  FMinerAccountKey := AMinerAccountKey;
  TLog.NewLog(ltdebug,ClassName,'Assigning Miner account key to: '+TCrypto.ToHexaString(TAccountComp.AccountKey2RawString(AMinerAccountKey)));
  FMinerPayload := AMinerPayload;
  TLog.NewLog(ltdebug,ClassName,'Assigning Miner new Payload: '+TCrypto.ToHexaString(AMinerPayload));
  Send_mine_values_to_all;
end;

{ TPoolMinerClient }

constructor TPoolMinerClient.Create(AOwner: TComponent);
begin
  FMinerValuesForWork := CT_TMinerValuesForWork_NULL;
  inherited;
end;

procedure TPoolMinerClient.DoProcessJSONObject(json: TPCJSONObject);
Var method : String;
    params : TPCJSONArray;
    id_value : Variant;
    i : Integer;
  params_object : TPCJSONObject;
  mvfw : TMinerValuesForWork;
begin
  TLog.NewLog(ltdebug,ClassName,'Received JSON: '+json.ToJSON(false));
  method := json.AsString('method','');
  params := json.GetAsArray('params');
  i := json.IndexOfName('id');
  if i<0 then begin
    id_value := Null;
  end else begin
    id_value := json.GetAsVariant('id').Value;
  end;
  if method=CT_PoolMining_Method_MINER_NOTIFY then begin
    params_object := params.GetAsObject(0);
    mvfw := CT_TMinerValuesForWork_NULL;
    mvfw.block := params_object.AsInteger('block',0);
    mvfw.version := params_object.AsInteger('version',0);
    mvfw.part1 := TCrypto.HexaToRaw(params_object.AsString('part1',''));
    mvfw.payload_start := TCrypto.HexaToRaw(params_object.AsString('payload_start',''));
    mvfw.part3 := TCrypto.HexaToRaw(params_object.AsString('part3',''));
    mvfw.target := params_object.AsInteger('target',0);
    mvfw.timestamp := params_object.AsInteger('timestamp',0);
    mvfw.part1 := TCrypto.HexaToRaw(params_object.AsString('part1',''));
    mvfw.target_pow := TCrypto.HexaToRaw(params_object.AsString('target_pow',''));
    if Not VarIsNull(id_value) then begin
      SendJSONRPCResponse(params_object,id_value);
    end;
    MinerValuesForWork := mvfw;
  end;
end;

procedure TPoolMinerClient.SetMinerValuesForWork(const Value: TMinerValuesForWork);
begin
  FMinerValuesForWork := Value;
  if Assigned(FOnMinerMustChangeValues) then FOnMinerMustChangeValues(Self);
end;

procedure TPoolMinerClient.SubmitBlockFound(const Payload: TRawBytes; Timestamp, NOnce: Cardinal);
Var json : TPCJSONObject;
begin
  json := TPCJSONObject.Create;
  Try
    json.GetAsVariant('payload').Value := TCrypto.ToHexaString(Payload);
    json.GetAsVariant('timestamp').Value := Timestamp;
    json.GetAsVariant('nonce').Value := NOnce;
    SendJSONRPCMethod(CT_PoolMining_Method_MINER_SUBMIT,json,5000);
// Example: {"method":"miner-submit","params":[{"payload":"57617368696E67746F6E3117","timestamp":1234567890,"nonce":1234}]}
  Finally
    json.Free;
  End;
end;

end.
