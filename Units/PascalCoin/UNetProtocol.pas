unit UNetProtocol;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

Uses UBlockChain, Classes, SysUtils, UAccounts, UThread, Sockets, ExtCtrls,
  UCrypto,
  Windows;

Const
  CT_MagicNetIdentification = $0A043580; // Unix timestamp 168048000 ... It's Albert birthdate!
  CT_MagicRequest = $0001;
  CT_MagicResponse = $0002;
  CT_MagicAutoSend = $0003;

  CT_NetOp_Hello = $0001;              // Sends my last operationblock + servers. Receive last operationblock + servers + same operationblock number of sender
  CT_NetOp_Error = $0002;
  CT_NetOp_Message = $0003;
  CT_NetOp_GetBlocks = $0010;
  CT_NetOp_GetOperationsBlock = $0005; // Sends from and to. Receive a number of OperationsBlock to check
  CT_NetOp_NewBlock = $0011;
  CT_NetOp_AddOperations = $0020;


  CT_NetError_InvalidProtocolVersion = $0001;
  CT_NetError_IPBlackListed = $0002;
  CT_NetError_InvalidDataBufferInfo = $0010;
  CT_NetError_InternalServerError = $0011;
  CT_NetError_InvalidNewAccount = $0012;

Type
  {
  Net Protocol:

  3 different types: Request,Response or Auto-send
  Request:   <Magic Net Identification (4b)><request  (2b)><operation (2b)><0x0000 (2b)><request_id(4b)><protocol info(4b)><data_length(4b)><request_data (data_length bytes)>
  Response:  <Magic Net Identification (4b)><response (2b)><operation (2b)><error_code (2b)><request_id(4b)><protocol info(4b)><data_length(4b)><response_data (data_length bytes)>
  Auto-send: <Magic Net Identification (4b)><autosend (2b)><operation (2b)><0x0000 (2b)><0x00000000 (4b)><protocol info(4b)><data_length(4b)><data (data_length bytes)>

  Min size: 4b+2b+2b+2b+4b+4b+4b = 22 bytes
  Max size: (depends on last 4 bytes) = 22..65K
  }

  TNetTcpIpClient = TCustomIpClient;

  TNetTransferType = (ntp_unknown, ntp_request, ntp_response, ntp_autosend);

  TNetProtocolVersion = Record
    protocol_version,
    protocol_available : Word;
  end;

  TNetHeaderData = Record
    header_type : TNetTransferType;
    protocol : TNetProtocolVersion;
    operation : Word;
    request_id : Cardinal;
    buffer_data_length : Cardinal;
    //
    is_error : Boolean;
    error_code : Integer;
    error_text : AnsiString;
  end;

  TNetConnection = Class;

  TNodeServerAddress = Record
    ip : AnsiString;
    port : Word;
    last_connection : Cardinal;
    //
    netConnection : TNetConnection;
    its_myself : Boolean;
    last_attempt_to_connect : TDateTime;
    total_failed_attemps_to_connect : Integer;
    BlackListText : String;
  end;
  PNodeServerAddress = ^TNodeServerAddress;

  TNetMessage_Hello = Record
     last_operation : TOperationBlock;
     servers_address : Array of TNodeServerAddress;
  end;

  TNetRequestRegistered = Record
    NetClient : TNetConnection;
    Operation : Word;
    RequestId : Cardinal;
    SendTime : TDateTime;
  end;

  TThreadCheckConnections = Class(TPCThread)
  private
    FLastCheckTS : Cardinal;
  protected
    procedure BCExecute; override;
  public
  End;

  TNetStatistics = Record
    ActiveConnections : Integer; // All connections wiht "connected" state
    ClientsConnections : Integer; // All clients connected to me like a server with "connected" state
    ServersConnections : Integer; // All servers where I'm connected
    TotalConnections : Integer;
    TotalClientsConnections : Integer;
    TotalServersConnections : Integer;
    BytesReceived : Int64;
    BytesSend : Int64;
  end;

  TNetData = Class(TComponent)
  private
    FNodePrivateKey : TECPrivateKey;
    FNetConnections : TThreadList;
    FNodeServers : TThreadList;
    FBlackList : TThreadList;
    FLastRequestId : Cardinal;
    FRegisteredRequests : TThreadList;
    FIsDiscoveringServers : Boolean;
    FIsGettingNewBlockChainFromClient : Boolean;
    FOnNetConnectionsUpdated: TNotifyEvent;
    FOnNodeServersUpdated: TNotifyEvent;
    FOnBlackListUpdated: TNotifyEvent;
    FThreadCheckConnections : TThreadCheckConnections;
    FOnReceivedHelloResponse: TNotifyEvent;
    FNetStatistics: TNetStatistics;
    FOnStatisticsChanged: TNotifyEvent;
    FMaxRemoteOperationBlock : TOperationBlock;
    Procedure IncStatistics(incActiveConnections,incClientsConnections,incServersConnections : Integer; incBytesReceived,incBytesSend : Int64);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function IndexOfNetClient(ListToSearch : TList; ip : AnsiString; port : Word; indexStart : Integer = 0) : Integer;
    Procedure DeleteNetClient(List : TList; index : Integer);
    Procedure CleanBlackList;
    Procedure DiscoverServersTerminated(Sender : TObject);
  public
    Class function HeaderDataToText(const HeaderData : TNetHeaderData) : AnsiString;
    Class function ExtractHeaderInfo(buffer : TStream; var HeaderData : TNetHeaderData; DataBuffer : TStream; var IsValidHeaderButNeedMoreData : Boolean) : Boolean;
    Class Function OperationToText(operation : Word) : AnsiString;
    // Only 1 NetData
    Class Function NetData : TNetData;
    //
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function Bank : TPCBank;
    Function NewRequestId : Cardinal;
    Procedure RegisterRequest(Sender: TNetConnection; operation : Word; request_id : Cardinal);
    Function UnRegisterRequest(Sender: TNetConnection; operation : Word; request_id : Cardinal) : Boolean;
    Function PendingRequest(Sender : TNetConnection; var requests_data : AnsiString ) : Integer;
    Procedure AddServer(NodeServerAddress : TNodeServerAddress);
    Function IsBlackListed(const ip : AnsiString; port : Word) : Boolean;
    //
    Function ConnectionsCount(CountOnlyNetClients : Boolean) : Integer;
    Function Connection(index : Integer) : TNetConnection;
    Function ConnectionExists(ObjectPointer : TObject) : Boolean;
    Function FindConnectionByClientRandomValue(Sender : TNetConnection) : TNetConnection;
    Procedure DiscoverServers;
    Procedure DisconnectClients;
    Procedure GetNewBlockChainFromClient(Connection : TNetConnection);
    Property OnNetConnectionsUpdated : TNotifyEvent read FOnNetConnectionsUpdated write FOnNetConnectionsUpdated;
    Property OnNodeServersUpdated : TNotifyEvent read FOnNodeServersUpdated write FOnNodeServersUpdated;
    Property OnBlackListUpdated : TNotifyEvent read FOnBlackListUpdated write FOnBlackListUpdated;
    Property BlackList : TThreadList read FBlackList;
    Property NodeServers : TThreadList read FNodeServers;
    Property NetConnections : TThreadList read FNetConnections;
    Property OnReceivedHelloResponse : TNotifyEvent read FOnReceivedHelloResponse write FOnReceivedHelloResponse;
    Property NetStatistics : TNetStatistics read FNetStatistics;
    Property OnStatisticsChanged : TNotifyEvent read FOnStatisticsChanged write FOnStatisticsChanged;
    Property IsDiscoveringServers : Boolean read FIsDiscoveringServers;
    Property IsGettingNewBlockChainFromClient : Boolean read FIsGettingNewBlockChainFromClient;
    Property MaxRemoteOperationBlock : TOperationBlock read FMaxRemoteOperationBlock;
    Property NodePrivateKey : TECPrivateKey read FNodePrivateKey;
  End;

  TNetConnection = Class(TComponent)
  private
    FClient : TNetTcpIpClient;
    FRemoteOperationBlock : TOperationBlock;
    FSocketError : Integer;
    FLastDataReceivedTS : Cardinal;
    FLastDataSendedTS : Cardinal;
    FClientBufferRead : TStream;
    FNetLock : TRTLCriticalSection;
    FIsWaitingForResponse : Boolean;
    FLastKnownTimestampDiff : Int64;
    FIsMyselfServer : Boolean;
    FClientPublicKey : TAccountKey;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    procedure TcpClient_OnError(Sender: TObject; SocketError: Integer);
    procedure TcpClient_OnConnect(Sender: TObject);
    procedure TcpClient_OnDisconnect(Sender: TObject);
    procedure TcpClient_OnReceive(Sender: TObject; Buf: PAnsiChar; var DataLen: Integer);
    procedure TcpClient_OnSend(Sender: TObject; Buf: PAnsiChar; var DataLen: Integer);
    procedure TcpClient_OnCreateHandle(Sender : TObject);
    procedure TcpClient_OnDestroyHandle(Sender : TObject);
    Function DoSendAndWaitForResponse(operation: Word; RequestId: Integer; SendDataBuffer, ReceiveDataBuffer: TStream; MaxWaitTime : Cardinal; var HeaderData : TNetHeaderData) : Boolean;
    procedure DoProcessBuffer;
    Procedure DoProcess_Hello(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_Message(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetBlocks_Request(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetBlocks_Response(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetOperationsBlock_Request(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_NewBlock(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_AddOperations(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure SetClient(Const Value : TNetTcpIpClient);
    Function ReadTcpClientBuffer(MaxWaitMiliseconds : Cardinal; var HeaderData : TNetHeaderData; BufferData : TStream) : Boolean;
    Procedure DisconnectInvalidClient(ItsMyself : Boolean; Const why : AnsiString);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure Send(NetTranferType : TNetTransferType; operation, errorcode : Word; request_id : Integer; DataBuffer : TStream);
    Procedure SendError(NetTranferType : TNetTransferType; operation, request_id : Integer; error_code : Integer; error_text : AnsiString);
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function ConnectTo(ServerIP: String; ServerPort:Word) : Boolean;
    Property Connected : Boolean read GetConnected write SetConnected;
    Function Send_Hello(NetTranferType : TNetTransferType; request_id : Integer) : Boolean;
    Function Send_NewBlockFound : Boolean;
    Function Send_GetBlocks(StartAddress, quantity : Cardinal; var request_id : Cardinal) : Boolean;
    Function Send_AddOperations(Operations : TOperationsHashTree) : Boolean;
    Function Send_Message(Const TheMessage : AnsiString) : Boolean;
    Property Client : TNetTcpIpClient read FClient;
    Property IsMyselfServer : Boolean read FIsMyselfServer;
  End;

  TNetClient = Class;
  TNetClientThread = Class(TPCThread)
  private
    FNetClient : TNetClient;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(NetClient : TNetClient);
  End;

  TNetClient = Class(TNetConnection)
  private
    FNetClientThread : TNetClientThread;
    Procedure OnNetClientThreadTerminated(Sender : TObject);
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  End;

  TNetServerClient = Class(TNetConnection);

  TNetServer = Class(TComponent)
  private
    FNetClients : TList;  // When a connection is established to a new client, a TNetConnection is created (p2p)
    FTCPServer : TTcpServer;
    FPort: Word;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure OnTcpServerCreateHandle(Sender : TObject);
    procedure OnTcpServerDestroyHandle(Sender : TObject);
    procedure OnTcpServerListening(Sender : TObject);
    procedure OnTcpServerAccept(Sender: TObject; ClientSocket: TCustomIpClient);
    procedure OnTcpServerGetThread(Sender: TObject; var ClientSocketThread: TClientSocketThread);
    procedure SetPort(const Value: Word);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property Active : Boolean read GetActive write SetActive;
    Property Port : Word read FPort Write SetPort;
  End;

  TThreadDiscoverConnection = Class(TPCThread)
    FNodeServerAddress : TNodeServerAddress;
    procedure Synchronized_notify;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(NodeServerAddress: TNodeServerAddress; NotifyOnTerminate : TNotifyEvent);
  End;

  TThreadGetNewBlockChainFromClient = Class(TPCThread)
  protected
    procedure BCExecute; override;
  End;


Const
  CT_TNodeServerAddress_NUL : TNodeServerAddress = (ip:'';port:0;last_connection:0; netConnection:nil;its_myself:false;last_attempt_to_connect:0;total_failed_attemps_to_connect:0;BlackListText:'');
  CT_TNetStatistics_NUL : TNetStatistics = (ActiveConnections:0;ClientsConnections:0;ServersConnections:0;TotalConnections:0;TotalClientsConnections:0;TotalServersConnections:0;BytesReceived:0;BytesSend:0);

implementation

uses
  UConst, ULog, UNode, UTime, UECIES;

Const
  CT_NetTransferType : Array[TNetTransferType] of AnsiString = ('Unknown','Request','Response','Autosend');
  CT_NetHeaderData : TNetHeaderData = (header_type:ntp_unknown;protocol:(protocol_version:0;protocol_available:0);operation:0;request_id:0;buffer_data_length:0;is_error:false;error_code:0;error_text:'');

{ TNetData }

Var _NetData : TNetData = nil;

Type PNetRequestRegistered = ^TNetRequestRegistered;

procedure TNetData.AddServer(NodeServerAddress: TNodeServerAddress);
Var P : PNodeServerAddress;
  i : Integer;
  l : TList;
begin
  l := FNodeServers.LockList;
  try
    i := IndexOfNetClient(l,NodeServerAddress.ip,NodeServerAddress.port);
    if i>=0 then exit;
    New(P);
    P^ := NodeServerAddress;
    l.Add(P);
    TLog.NewLog(ltdebug,Classname,'Adding new server: '+NodeServerAddress.ip+':'+Inttostr(NodeServerAddress.port));
  finally
    FNodeServers.UnlockList;
  end;
  if Assigned(FOnNodeServersUpdated) then FOnNodeServersUpdated(Self);
end;

function TNetData.Bank: TPCBank;
begin
  Result := TNode.Node.Bank;
end;

procedure TNetData.CleanBlackList;
Var P : PNodeServerAddress;
  i,n : Integer;
  l : TList;
begin
  // This procedure cleans old blacklisted IPs
  n := 0;
  l := FBlackList.LockList;
  Try
    for i := l.Count - 1 downto 0 do begin
      P := l[i];
      if
        // Is an old blacklisted IP? (More than 1 hour)
        (((P^.last_connection+(60*60)) < (UnivDateTimeToUnix(DateTime2UnivDateTime(now)))) And (Not P^.its_myself))
        then begin
        l.Delete(i);
        Dispose(P);
        inc(n);
      end;
    end;
  Finally
    FBlackList.UnlockList;
  End;
  if (n>0) And (Assigned(FOnBlackListUpdated)) then FOnBlackListUpdated(Self);
end;

function TNetData.Connection(index: Integer): TNetConnection;
Var l : TList;
begin
  l := FNetConnections.LockList;
  try
    Result := TNetConnection( l[index] );
  finally
    FNetConnections.UnlockList;
  end;
end;

function TNetData.ConnectionExists(ObjectPointer: TObject): Boolean;
var i : Integer;
  l : TList;
begin
  Result := false;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      if TObject(l[i])=ObjectPointer then begin
        Result := TNetConnection(ObjectPointer).Connected;
        exit;
      end;
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TNetData.ConnectionsCount(CountOnlyNetClients : Boolean): Integer;
var i : Integer;
  l : TList;
begin
  l := FNetConnections.LockList;
  try
    if CountOnlyNetClients then begin
      Result := 0;
      for i := 0 to l.Count - 1 do begin
        if TObject(l[i]) is TNetClient then inc(Result);
      end;
    end else Result := l.Count;
  finally
    FNetConnections.UnlockList;
  end;
end;

constructor TNetData.Create;
begin
  FMaxRemoteOperationBlock := CT_OperationBlock_NUL;
  FNetStatistics := CT_TNetStatistics_NUL;
  FOnStatisticsChanged := Nil;
  FOnNetConnectionsUpdated := Nil;
  FOnNodeServersUpdated := Nil;
  FOnBlackListUpdated := Nil;
  FIsDiscoveringServers := false;
  FNodeServers := TThreadList.Create;
  FRegisteredRequests := TThreadList.Create;
  FLastRequestId := 0;
  FNetConnections := TThreadList.Create;
  FBlackList := TThreadList.Create;
  FIsGettingNewBlockChainFromClient := false;
  FNodePrivateKey := TECPrivateKey.Create;
  FNodePrivateKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FThreadCheckConnections := TThreadCheckConnections.Create(false);
end;

procedure TNetData.DeleteNetClient(List: TList; index: Integer);
Var P : PNodeServerAddress;
begin
  P := List.Items[index];
  List.Delete(index);
  Dispose(P);
end;

destructor TNetData.Destroy;
Var l : TList;
begin
  FOnStatisticsChanged := Nil;
  FOnNetConnectionsUpdated := Nil;
  FOnNodeServersUpdated := Nil;
  FOnBlackListUpdated := Nil;
  FThreadCheckConnections.Terminate;
  FThreadCheckConnections.WaitFor;
  CleanBlackList;
  l := FNodeServers.LockList;
  try
    while (l.Count>0) do DeleteNetClient(l,l.Count-1);
  finally
    FNodeServers.UnlockList;
    FNodeServers.Free;
  end;
  l := FBlackList.LockList;
  try
    while (l.Count>0) do DeleteNetClient(l,l.Count-1);
  finally
    FBlackList.UnlockList;
    FBlackList.Free;
  end;
  FNetConnections.Free;
  FNodePrivateKey.Free;
  inherited;
end;

procedure TNetData.DisconnectClients;
var i : Integer;
  l : TList;
begin
  l := FNetConnections.LockList;
  Try
    for i := l.Count - 1 downto 0 do begin
      if TObject(l[i]) is TNetClient then begin
        TNetClient(l[i]).Connected := false;
      end;
    end;
  Finally
    FNetConnections.UnlockList;
  End;
end;

procedure TNetData.DiscoverServers;
  Procedure sw(l : TList);
  Var i,j,x,y : Integer;
  begin
    if l.Count<=1 then exit;
    j := Random(l.Count);
    for i := 0 to j do begin
      x := Random(l.Count);
      y := Random(l.Count);
      if x<>y then l.Exchange(x,y);
    end;
  end;
Var P : PNodeServerAddress;
  i,j : Integer;
  l,lns : TList;
  tdc : TThreadDiscoverConnection;
begin
  if TPCThread.ThreadClassFound(TThreadDiscoverConnection,nil)>=0 then begin
    TLog.NewLog(ltInfo,ClassName,'Allready discovering servers...');
    exit;
  end;
  CleanBlackList;
  j := CT_MaxServersConnected - ConnectionsCount(true);
  if j<=0 then exit;
  // can discover up to j servers
  l := TList.Create;
  try
    lns := FNodeServers.LockList;
    try
      for i:=0 to lns.Count-1 do begin
        P := lns[i];
        If (Not Assigned(P.netConnection)) AND (Not IsBlackListed(P^.ip,P^.port)) AND (Not P^.its_myself) And
          ((P^.last_attempt_to_connect=0) Or ((P^.last_attempt_to_connect+EncodeTime(0,5,0,0)<now))) And
          ((P^.total_failed_attemps_to_connect<3) Or (P^.last_attempt_to_connect+EncodeTime(2,0,0,0)<now)) then begin
          l.Add(P);
        end;
      end;
    Finally
      FNodeServers.UnlockList;
    end;
    if l.Count<=0 then exit;
    sw(l);
    if j>=l.Count then j:=l.Count-1;
    TLog.NewLog(ltDebug,Classname,'Start discovering up to '+inttostr(j+1)+' servers... (max:'+inttostr(l.count)+')');
    //
    for i := 0 to j do begin
      FIsDiscoveringServers := true;
      P := PNodeServerAddress(l[i]);
      tdc := TThreadDiscoverConnection.Create(P^,DiscoverServersTerminated);
    end;
  finally
    l.Free;
  end;
end;

procedure TNetData.DiscoverServersTerminated(Sender: TObject);
begin
  if Assigned(FOnNodeServersUpdated) then FOnNodeServersUpdated(Self);
  if TPCThread.ThreadClassFound(TThreadDiscoverConnection,Sender)>=0 then exit;
  FIsDiscoveringServers := false;
  // If here, discover servers finished, so we can try to get/receive data
  TLog.NewLog(ltDebug,Classname,Format('Discovering servers finished. Now we have %d active connections and %d connections to other servers',
    [ConnectionsCount(false),ConnectionsCount(true)]));
  if TPCThread.ThreadClassFound(TThreadGetNewBlockChainFromClient,nil)>=0 then exit;
  TThreadGetNewBlockChainFromClient.Create(false).FreeOnTerminate := true;
end;

class function TNetData.ExtractHeaderInfo(buffer : TStream; var HeaderData : TNetHeaderData; DataBuffer : TStream; var IsValidHeaderButNeedMoreData : Boolean) : Boolean;
Var lastp : Integer;
  c : Cardinal;
  w : Word;
begin
  HeaderData := CT_NetHeaderData;
  Result := false;
  IsValidHeaderButNeedMoreData := false;
  lastp := buffer.Position;
  Try
    if buffer.Size-buffer.Position < 22 then exit;
    buffer.Read(c,4);
    if (c<>CT_MagicNetIdentification) then exit;
    buffer.Read(w,2);
    case w of
      CT_MagicRequest : HeaderData.header_type := ntp_request;
      CT_MagicResponse : HeaderData.header_type := ntp_response;
      CT_MagicAutoSend : HeaderData.header_type := ntp_autosend;
    else
      HeaderData.header_type := ntp_unknown;
      exit;
    end;
    buffer.Read(HeaderData.operation,2);
    buffer.Read(HeaderData.error_code,2);
    buffer.Read(HeaderData.request_id,4);
    buffer.Read(HeaderData.protocol.protocol_version,2);
    buffer.Read(HeaderData.protocol.protocol_available,2);
    buffer.Read(c,4);
    DataBuffer.Size := 0;
    if buffer.Size - buffer.Position < c then begin
      IsValidHeaderButNeedMoreData := true;
      exit;
    end;
    DataBuffer.CopyFrom(buffer,c);
    DataBuffer.Position := 0;
    HeaderData.buffer_data_length := c;
    //
    if HeaderData.header_type=ntp_response then begin
      HeaderData.is_error := HeaderData.error_code<>0;
      if HeaderData.is_error then begin
        TStreamOp.ReadAnsiString(DataBuffer,HeaderData.error_text);
      end;
    end else begin
      HeaderData.is_error := HeaderData.error_code<>0;
      if HeaderData.is_error then begin
        TStreamOp.ReadAnsiString(DataBuffer,HeaderData.error_text);
      end;
    end;
    if (HeaderData.is_error) then begin
      TLog.NewLog(lterror,Classname,'Response with error ('+IntToHex(HeaderData.error_code,4)+'): '+HeaderData.error_text+' ...on '+
        'operation: '+OperationToText(HeaderData.operation)+' id: '+Inttostr(HeaderData.request_id));
    end;
    Result := true;
  Finally
    if Not Result then buffer.Position := lastp;
  End;
end;

function TNetData.FindConnectionByClientRandomValue(Sender: TNetConnection): TNetConnection;
Var l : TList;
  i : Integer;
begin
  l := FNetConnections.LockList;
  try
    for i := 0 to L.Count - 1 do begin
      Result := TNetConnection( l[i] );
      If TAccountComp.Equal(Result.FClientPublicKey,Sender.FClientPublicKey) And (Sender<>Result) then exit;
    end;
  finally
    FNetConnections.UnlockList;
  end;
  Result := Nil;
end;

procedure TNetData.GetNewBlockChainFromClient(Connection: TNetConnection);
Const CT_LogSender = 'GetNewBlockChainFromClient';

  function Do_GetOperationsBlock(AssignToBank : TPCBank; block_start,block_end : Cardinal; OnlyOperationBlock : Boolean; BlocksList : TList) : Boolean;
  Var SendData,ReceiveData : TMemoryStream;
    headerdata : TNetHeaderData;
    op : TPCOperationsComp;
    request_id,opcount,i : Cardinal;
    errors : AnsiString;
    noperation : Integer;
  begin
    Result := false;
    BlocksList.Count := 0;
    if (Connection.FRemoteOperationBlock.block<block_end) then block_end := Connection.FRemoteOperationBlock.block;
    // First receive operations from
    SendData := TMemoryStream.Create;
    ReceiveData := TMemoryStream.Create;
    try
      if OnlyOperationBlock then begin
        noperation := CT_NetOp_GetOperationsBlock;
      end else begin
        noperation := CT_NetOp_GetBlocks;
      end;
      TLog.NewLog(ltdebug,CT_LogSender,Format('Sending %s from block %d to %d (Total: %d)',
        [TNetData.OperationToText(noperation),block_start,block_end,block_end-block_start+1]));
      SendData.Write(block_start,4);
      SendData.Write(block_end,4);
      request_id := TNetData.NetData.NewRequestId;
      if Connection.DoSendAndWaitForResponse(noperation,request_id,SendData,ReceiveData,5000,headerdata) then begin
        if HeaderData.is_error then exit;
        if ReceiveData.Read(opcount,4)<4 then exit; // Error in data
        i := 0;
        while (i<opcount) do begin
          // decode data
          op := TPCOperationsComp.Create(AssignToBank);
          If op.LoadFromStream(false,false,ReceiveData,errors) then begin
            BlocksList.Add(op);
          end else begin
            TLog.NewLog(lterror,CT_LogSender,Format('Error reading OperationBlock from received stream %d/%d: %s',[i+1,opcount,errors]));
            op.free;
            break;
          end;
          inc(i);
        end;
        Result := true;
      end else begin
        TLog.NewLog(lterror,CT_LogSender,Format('No received response after waiting request id %d operation %s',[request_id,TNetData.OperationToText(noperation)]));
      end;
    finally
      SendData.Free;
      ReceiveData.free;
    end;
  end;

  function Do_GetOperationBlock(block : Cardinal; var OperationBlock : TOperationBlock) : Boolean;
  Var BlocksList : TList;
    i : Integer;
  begin
    OperationBlock := CT_OperationBlock_NUL;
    BlocksList := TList.Create;
    try
      Result := Do_GetOperationsBlock(TNode.Node.Bank,block,block,false,BlocksList);
      if (Result) And (BlocksList.Count=1) then begin
        OperationBlock := TPCOperationsComp(BlocksList[0]).OperationBlock;
      end;
    finally
      for i := 0 to BlocksList.Count - 1 do TPCOperationsComp(BlocksList[i]).Free;
      BlocksList.Free;
    end;
  end;

  Function FindLastSameBlockByOperationsBlock(min,max : Cardinal; var OperationBlock : TOperationBlock) : Boolean;
  var i : Integer;
    ant_nblock : Int64;
    myops : TPCOperationsComp;
    auxBlock : TOperationBlock;
    distinctmax,distinctmin : Cardinal;
    BlocksList : TList;
  Begin
    Result := false;
    OperationBlock := CT_OperationBlock_NUL;
    repeat
      BlocksList := TList.Create;
      try
        If Not Do_GetOperationsBlock(Nil,min,max,true,BlocksList) then exit;
        distinctmin := min;
        distinctmax := max;
        myops := TPCOperationsComp.Create(TNode.Node.Bank);
        try
          ant_nblock := -1;
          for i := 0 to BlocksList.Count - 1 do begin
            auxBlock := TPCOperationsComp(BlocksList[i]).OperationBlock;
            // Protection of invalid clients:
            if (auxBlock.block<min) Or (auxBlock.block>max) Or (auxBlock.block=ant_nblock) then begin
              Connection.DisconnectInvalidClient(false,'Invalid response... '+inttostr(min)+'<'+inttostr(auxBlock.block)+'<'+inttostr(max)+' ant:'+inttostr(ant_nblock));
              exit;
            end;
            ant_nblock := auxBlock.block;
            //
            If Not TNode.Node.Bank.LoadOperations(myops,auxBlock.block) then exit;

            if ((myops.OperationBlock.proof_of_work = auxBlock.proof_of_work) And (myops.OperationBlock.nonce = auxBlock.nonce)) then begin
              distinctmin := auxBlock.block;
              OperationBlock := auxBlock;
            end else begin
              if auxBlock.block<=distinctmax then
                distinctmax := auxBlock.block-1;
            end;
          end;
        finally
          myops.Free;
        end;
        min := distinctmin;
        max := distinctmax;
      finally
        for i := 0 to BlocksList.Count - 1 do begin
          TPCOperationsComp(BlocksList[i]).Free;
        end;
        BlocksList.Free;
      end;
    until (distinctmin=distinctmax);
    Result := (OperationBlock.proof_of_work <> CT_OperationBlock_NUL.proof_of_work);
  End;

  Function GetNewBank(start_block : Int64) : Boolean;
  Var BlocksList : TList;
    i : Integer;
    tempfolder : AnsiString;
    OpComp,OpExecute : TPCOperationsComp;
    newBlock : TBlockAccount;
    errors : AnsiString;
    start : Cardinal;
    finished : Boolean;
    Bank : TPCBank;
    ms : TMemoryStream;
  Begin
    TLog.NewLog(ltdebug,CT_LogSender,Format('GetNewBank(new_start_block:%d)',[start_block]));
    Bank := TPCBank.Create(Nil);
    try

      Bank.StorageClass := TNode.Node.Bank.StorageClass;
      Bank.Storage.Orphan := TNode.Node.Bank.Storage.Orphan;
      Bank.Storage.CopyConfiguration(TNode.Node.Bank.Storage);
      if start_block>=0 then begin
        // Restore a part
        Bank.DiskRestoreFromOperations(start_block);
        start := start_block + 1;
      end else begin
        start := 0;
        start_block := 0;
      end;
      Bank.Storage.Orphan := FormatDateTime('yyyymmddhhnnss',DateTime2UnivDateTime(now));
      // Receive new blocks:
      finished := false;
      repeat
        BlocksList := TList.Create;
        try
          finished := NOT Do_GetOperationsBlock(Bank,start,start + 50,false,BlocksList);
          i := 0;
          while (i<BlocksList.Count) And (Not finished) do begin
            OpComp := TPCOperationsComp(BlocksList[i]);
            ms := TMemoryStream.Create;
            OpExecute := TPCOperationsComp.Create(Bank);
            try
              OpComp.SaveToStream(false,false,ms);
              ms.Position := 0;
              OpExecute.LoadFromStream(false,false,ms,errors);
              if Bank.AddNewBlockChainBlock(OpExecute,newBlock,errors) then begin
                inc(i);
              end else begin
                TLog.NewLog(lterror,CT_LogSender,'Error creating new bank with client Operations. Block:'+TPCOperationsComp.OperationBlockToText(OpExecute.OperationBlock)+' Error:'+errors);
                // Add to blacklist !
                Connection.DisconnectInvalidClient(false,'Invalid BlockChain on Block '+TPCOperationsComp.OperationBlockToText(OpExecute.OperationBlock)+' with errors:'+errors);
                finished := true;
                break;
              end;
            finally
              ms.Free;
              OpExecute.Free;
            end;
          end;
        finally
          for i := 0 to BlocksList.Count - 1 do TPCOperationsComp(BlocksList[i]).Free;
          BlocksList.Free;
        end;
        start := Bank.BlocksCount;
      until (Bank.BlocksCount=Connection.FRemoteOperationBlock.block+1) Or (finished);
      if Bank.BlocksCount>TNode.Node.Bank.BlocksCount then begin
        // I'm an orphan blockchain...
        TLog.NewLog(ltinfo,CT_LogSender,'New valid blockchain found. My block count='+inttostr(TNode.Node.Bank.BlocksCount)+
          ' found='+inttostr(Bank.BlocksCount)+' starting at block '+inttostr(start_block));
        TNode.Node.Bank.Storage.MoveBlockChainBlocks(start_block,Inttostr(start_block)+'_'+FormatDateTime('yyyymmddhhnnss',DateTime2UnivDateTime(now)));
        Bank.Storage.MoveBlockChainBlocks(start_block,TNode.Node.Bank.Storage.Orphan);
        TNode.Node.Bank.DiskRestoreFromOperations(CT_MaxBlock);
      end;
    finally
      Bank.Free;
    end;
  End;

var rid : Cardinal;
  bufferdata : TMemoryStream;
  headerdata : TNetHeaderData;
  op : TOperationBlock;
begin
  // Protection against discovering servers...
  if FIsDiscoveringServers then begin
    TLog.NewLog(ltdebug,CT_LogSender,'Is discovering servers...');
    exit;
  end;
  //
  If FIsGettingNewBlockChainFromClient then begin
    TLog.NewLog(ltdebug,CT_LogSender,'Is getting new blockchain from client...');
    exit;
  end else TLog.NewLog(ltdebug,CT_LogSender,'Starting receiving');
  Try
    FIsGettingNewBlockChainFromClient := true;
    FMaxRemoteOperationBlock := Connection.FRemoteOperationBlock;
    if TNode.Node.Bank.BlocksCount=0 then begin
      TLog.NewLog(ltdebug,CT_LogSender,'I have no blocks');
      Connection.Send_GetBlocks(0,10,rid);
      exit;
    end;
    TLog.NewLog(ltdebug,CT_LogSender,'Starting GetNewBlockChainFromClient at client:'+Connection.FClient.RemoteHost+':'+Connection.FClient.RemotePort+
      ' with OperationBlock:'+TPCOperationsComp.OperationBlockToText(Connection.FRemoteOperationBlock)+' (My block: '+TPCOperationsComp.OperationBlockToText(TNode.Node.Bank.LastOperationBlock));
    // NOTE: FRemoteOperationBlock.block >= TNode.Node.Bank.BlocksCount
    // First capture same block than me (TNode.Node.Bank.BlocksCount-1) to check if i'm an orphan block...
    If Not Do_GetOperationBlock(TNode.Node.Bank.BlocksCount-1,op) then begin
      exit;
    end;
    if op.proof_of_work<>TNode.Node.Bank.LastOperationBlock.proof_of_work then begin
      TLog.NewLog(ltinfo,CT_LogSender,'My blockchain is incorrect... received: '+TPCOperationsComp.OperationBlockToText(op)+' My: '+TPCOperationsComp.OperationBlockToText(TNode.Node.Bank.LastOperationBlock));
      if Not FindLastSameBlockByOperationsBlock(0,op.block,op) then begin
        TLog.NewLog(ltinfo,CT_LogSender,'No found base block to start process... Receiving ALL');
        GetNewBank(-1);
      end else begin
        TLog.NewLog(ltinfo,CT_LogSender,'Found base new block: '+TPCOperationsComp.OperationBlockToText(op));
        // Move operations to orphan folder... (temporal... waiting for a confirmation)
        GetNewBank(op.block);
      end;
    end else begin
      TLog.NewLog(ltinfo,CT_LogSender,'My blockchain is ok! Need to download new blocks starting at '+inttostr(TNode.Node.Bank.BlocksCount));
      // High to new value:
      Connection.Send_GetBlocks(TNode.Node.Bank.BlocksCount,0,rid);
    end;
  Finally
    TLog.NewLog(ltdebug,CT_LogSender,'Finalizing');
    FIsGettingNewBlockChainFromClient := false;
  end;
end;

class function TNetData.HeaderDataToText(const HeaderData: TNetHeaderData): AnsiString;
begin
  Result := CT_NetTransferType[HeaderData.header_type]+' Operation:'+TNetData.OperationToText(HeaderData.operation);
  if HeaderData.is_error then begin
    Result := Result +' ERROR:'+Inttostr(HeaderData.error_code)+' ERROR TEXT:'+HeaderData.error_text;
  end else begin
    Result := Result +' ReqId:'+Inttostr(HeaderData.request_id)+' BufferSize:'+Inttostr(HeaderData.buffer_data_length);
  end;
end;

procedure TNetData.IncStatistics(incActiveConnections, incClientsConnections,
  incServersConnections: Integer; incBytesReceived, incBytesSend: Int64);
begin
  FNetStatistics.ActiveConnections := FNetStatistics.ActiveConnections + incActiveConnections;
  FNetStatistics.ClientsConnections := FNetStatistics.ClientsConnections + incClientsConnections;
  FNetStatistics.ServersConnections := FNetStatistics.ServersConnections + incServersConnections;
  if (incActiveConnections>0) then FNetStatistics.TotalConnections := FNetStatistics.TotalConnections + incActiveConnections;
  if (incClientsConnections>0) then FNetStatistics.TotalClientsConnections := FNetStatistics.TotalClientsConnections + incClientsConnections;
  if (incServersConnections>0) then FNetStatistics.TotalServersConnections := FNetStatistics.TotalServersConnections + incServersConnections;
  FNetStatistics.BytesReceived := FNetStatistics.BytesReceived + incBytesReceived;
  FNetStatistics.BytesSend := FNetStatistics.BytesSend + incBytesSend;
  if Assigned(FOnStatisticsChanged) then FOnStatisticsChanged(Self);
  if (incBytesReceived<>0) Or (incBytesSend<>0) then begin
    if Assigned(FOnNetConnectionsUpdated) then begin
      FOnNetConnectionsUpdated(Self);
    end;
  end;
end;

function TNetData.IndexOfNetClient(ListToSearch: TList; ip: AnsiString; port: Word; indexStart : Integer = 0): Integer;
Var P : PNodeServerAddress;
begin
  if indexStart<0 then indexStart:=0;
  for Result := indexStart to ListToSearch.Count - 1 do begin
    P := ListToSearch[Result];
    if (AnsiSameText( P^.ip,ip)) And ((port=0) Or (P^.port=port)) then exit;
  end;
  Result := -1;
end;

function TNetData.IsBlackListed(const ip: AnsiString; port: Word): Boolean;
Var l : TList;
  i : Integer;
begin
  Result := false;
  l := FBlackList.LockList;
  Try
    i := -1;
    repeat
      i := IndexOfNetClient(l,ip,port,i+1);
      if (i>=0) then begin
        Result := Not PNodeServerAddress(l[i])^.its_myself;
      end;
    until (i<0) Or (Result);
  Finally
    FBlackList.UnlockList;
  End;
end;

class function TNetData.NetData: TNetData;
begin
  if Not Assigned(_NetData) then begin
    _NetData := TNetData.Create(nil);
  end;
  result := _NetData;
end;

function TNetData.NewRequestId: Cardinal;
begin
  Inc(FLastRequestId);
  Result := FLastRequestId;
end;

procedure TNetData.Notification(AComponent: TComponent; Operation: TOperation);
Var l : TList;
begin
  inherited;
  if Operation=OpRemove then begin
    if not (csDestroying in ComponentState) then begin
      l := FNetConnections.LockList;
      try
        if l.Remove(AComponent)>=0 then begin
          if Assigned(FOnNetConnectionsUpdated) then FOnNetConnectionsUpdated(Self);
        end;
      finally
        FNetConnections.UnlockList;
      end;
    end;
  end;
end;

class function TNetData.OperationToText(operation: Word): AnsiString;
begin
  case operation of
    CT_NetOp_Hello : Result := 'HELLO';
    CT_NetOp_Error : Result := 'ERROR';
    CT_NetOp_GetBlocks : Result := 'GET BLOCKS';
    CT_NetOp_Message : Result := 'MESSAGE';
    CT_NetOp_GetOperationsBlock : Result := 'GET OPERATIONS BLOCK';
    CT_NetOp_NewBlock : Result := 'NEW BLOCK';
    CT_NetOp_AddOperations : Result := 'ADD OPERATIONS';
  else Result := 'UNKNOWN OPERATION '+Inttohex(operation,4);
  end;
end;

function TNetData.PendingRequest(Sender: TNetConnection; var requests_data : AnsiString): Integer;
Var P : PNetRequestRegistered;
  i : Integer;
  l : TList;
begin
  requests_data := '';
  l := FRegisteredRequests.LockList;
  Try
    if Assigned(Sender) then begin
      Result := 0;
      for i := l.Count - 1 downto 0 do begin
        if (PNetRequestRegistered(l[i])^.NetClient=Sender) then begin
          requests_data := requests_data+'Op:'+OperationToText(PNetRequestRegistered(l[i])^.Operation)+' Id:'+Inttostr(PNetRequestRegistered(l[i])^.RequestId)+' - ';
          inc(Result);
        end;
      end;
    end else Result := l.Count;
  Finally
    FRegisteredRequests.UnlockList;
  End;
end;

procedure TNetData.RegisterRequest(Sender: TNetConnection; operation: Word; request_id: Cardinal);
Var P : PNetRequestRegistered;
  l : TList;
begin
  l := FRegisteredRequests.LockList;
  Try
    New(P);
    P^.NetClient := Sender;
    P^.Operation := operation;
    P^.RequestId := request_id;
    P^.SendTime := Now;
    l.Add(P);
    TLog.NewLog(ltdebug,Classname,'Registering request to '+Sender.FClient.RemoteHost+':'+Sender.FClient.RemotePort+' Op:'+OperationToText(operation)+' Id:'+inttostr(request_id)+' Total pending:'+Inttostr(l.Count));
  Finally
    FRegisteredRequests.UnlockList;
  End;
end;

function TNetData.UnRegisterRequest(Sender: TNetConnection; operation: Word; request_id: Cardinal): Boolean;
Var P : PNetRequestRegistered;
  i : Integer;
  l : TList;
begin
  Result := false;
  l := FRegisteredRequests.LockList;
  try
    for i := l.Count - 1 downto 0 do begin
      P := l[i];
      if (P^.NetClient=Sender) And
        ( ((Operation=P^.Operation) And (request_id = P^.RequestId))
          Or
          ((operation=0) And (request_id=0)) ) then begin
        l.Delete(i);
        Dispose(P);
        Result := true;
        if Assigned(Sender.FClient) then begin
          TLog.NewLog(ltdebug,Classname,'Unregistering request to '+Sender.FClient.RemoteHost+':'+Sender.FClient.RemotePort+' Op:'+OperationToText(operation)+' Id:'+inttostr(request_id)+' Total pending:'+Inttostr(l.Count));
        end;
        exit;
      end;
    end;
  finally
    FRegisteredRequests.UnlockList;
  end;
end;

{ TNetServer }

constructor TNetServer.Create(AOwner: TComponent);
begin
  inherited;
  FNetClients := TList.Create;
  FPort := CT_NetServer_Port;
  FTCPServer := TTcpServer.Create(Self);
  FTCPServer.LocalPort := Inttostr(CT_NetServer_Port);
  FTCPServer.OnAccept := OnTcpServerAccept;
  FTCPServer.OnGetThread := OnTcpServerGetThread;
  FTCPServer.OnListening := OnTcpServerListening;
  FTCPServer.OnCreateHandle := OnTcpServerCreateHandle;
  FTCPServer.OnDestroyHandle := OnTcpServerDestroyHandle;
end;

destructor TNetServer.Destroy;
begin
  FTCPServer.Free;
  FNetClients.Free;
  inherited;
end;

function TNetServer.GetActive: Boolean;
begin
  Result := FTCPServer.Active;
end;

procedure TNetServer.Notification(AComponent: TComponent; Operation: TOperation);
var i : Integer;
begin
  inherited;
  if (Operation=opRemove) then begin
    i := FNetClients.IndexOf(AComponent);
    if (i>=0) then begin
      FNetClients.Delete(i);
      TLog.NewLog(ltdebug,ClassName,'TNetConnection destroyed. Remaining: '+Inttostr(FNetclients.Count));
    end;
  end;
end;

procedure TNetServer.OnTcpServerAccept(Sender: TObject; ClientSocket: TCustomIpClient);
Var n : TNetServerClient;
begin
  // NOTE: I'm in a separate thread
  // While in this function the ClientSocket connection will be active, when finishes the ClientSocket will be destroyed
  TLog.NewLog(ltInfo,Classname,'Starting ClientSocket accept '+ClientSocket.RemoteHost+':'+ClientSocket.RemotePort);
  n := TNetServerClient.Create(Self);
  Try
    n.SetClient(ClientSocket);
    TNetData.NetData.CleanBlackList;
    if (TNetData.NetData.IsBlackListed(ClientSocket.RemoteHost,0)) then begin
      // Invalid!
      TLog.NewLog(ltinfo,Classname,'Refusing Blacklist ip: '+ClientSocket.RemoteHost+':'+ClientSocket.RemotePort);
      n.SendError(ntp_autosend,CT_NetOp_Error, 0,CT_NetError_IPBlackListed,'Your IP is blacklisted:'+ClientSocket.RemoteHost+':'+ClientSocket.RemotePort);
      // Wait some time before close connection
      sleep(5000);
    end else begin
      FNetClients.Add(n);
      TNetData.NetData.IncStatistics(1,1,0,0,0);
      while (n.Connected) And (FTCPServer.Active) do begin
        n.DoProcessBuffer;
        Sleep(10);
      end;
    end;
  Finally
    n.Free;
  End;
end;

procedure TNetServer.OnTcpServerCreateHandle(Sender: TObject);
begin
//  TLog.NewLog(ltdebug,Classname,'ServerCreateHandle');
end;

procedure TNetServer.OnTcpServerDestroyHandle(Sender: TObject);
begin
//  TLog.NewLog(ltdebug,Classname,'ServerDestroyHandle');
end;

procedure TNetServer.OnTcpServerGetThread(Sender: TObject; var ClientSocketThread: TClientSocketThread);
begin
//  TLog.NewLog(ltdebug,Classname,'ClientSocket Get Thread');
end;

procedure TNetServer.OnTcpServerListening(Sender: TObject);
begin
  TLog.NewLog(ltinfo,Classname,'Server listening');
end;

procedure TNetServer.SetActive(const Value: Boolean);
begin
  if Value then begin
    TLog.NewLog(ltinfo,Classname,'Activating server on port '+FTCPServer.LocalPort);
  end else begin
    TLog.NewLog(ltinfo,Classname,'Closing server');
  end;
  FTCPServer.Active := Value;
  if FTCPServer.Active then begin
    TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  end else begin
    TNetData.NetData.DisconnectClients;
  end;
end;

procedure TNetServer.SetPort(const Value: Word);
begin
  if FPort=Value then exit;
  Active := false;
  FPort := Value;
  FTCPServer.LocalPort := Inttostr(Value);
end;

{ TNetConnection }

function TNetConnection.ConnectTo(ServerIP: String; ServerPort: Word) : Boolean;
Var Pnsa : PNodeServerAddress;
  lns : TList;
  i : Integer;
begin
  if FClient.Connected then FClient.Disconnect;
  lns := TNetData.NetData.FNodeServers.LockList;
  try
    i := TNetData.NetData.IndexOfNetClient(lns,ServerIp,ServerPort);
    if (i>=0) then Pnsa := lns[i]
    else Pnsa := Nil;
  finally
    TNetData.NetData.FNodeServers.UnlockList;
  end;
  if Assigned(Pnsa) then Pnsa^.netConnection := Self;

  FClient.RemoteHost := ServerIP;
  if ServerPort<=0 then ServerPort := CT_NetServer_Port;
  FClient.RemotePort := Inttostr(ServerPort);
  TLog.NewLog(ltDebug,Classname,'Trying to connect to a server at: '+FClient.RemoteHost+':'+FClient.RemotePort);
  if Assigned(TNetData.NetData.FOnNetConnectionsUpdated) then TNetData.NetData.FOnNetConnectionsUpdated(Self);
  Result := FClient.Connect;
  if Result then begin
    TLog.NewLog(ltDebug,Classname,'Connected to a possible server at: '+FClient.RemoteHost+':'+FClient.RemotePort);
    Result := Send_Hello(ntp_request,TNetData.NetData.NewRequestId);
  end else begin
    TLog.NewLog(ltDebug,Classname,'Cannot connect to a server at: '+FClient.RemoteHost+':'+FClient.RemotePort);
  end;
end;

constructor TNetConnection.Create(AOwner: TComponent);
begin
  inherited;
  FClientPublicKey := CT_TECDSA_Public_Nul;
  FIsMyselfServer := false;
  FLastKnownTimestampDiff := 0;
  FIsWaitingForResponse := false;
  FClientBufferRead := TMemoryStream.Create;
  InitializeCriticalSection(FNetLock);
  FLastDataReceivedTS := 0;
  FLastDataSendedTS := 0;
  FClient := Nil;
  SetClient( TTcpClient.Create(Self) );
  FRemoteOperationBlock := CT_OperationBlock_NUL;
  FSocketError := 0;
  TNetData.NetData.FNetConnections.Add(Self);
  if Assigned(TNetData.NetData.FOnNetConnectionsUpdated) then TNetData.NetData.FOnNetConnectionsUpdated(Self);
end;

destructor TNetConnection.Destroy;
Var Pnsa : PNodeServerAddress;
  lns : TList;
  i : Integer;
begin
  Connected := false;

  lns := TNetData.NetData.FNodeServers.LockList;
  try
    for i := 0 to lns.Count - 1 do begin
      Pnsa := lns[i];
      if Pnsa^.netConnection=Self then Pnsa^.netConnection := Nil;
    end;
  finally
    TNetData.NetData.FNodeServers.UnlockList;
  end;
  TNetData.NetData.UnRegisterRequest(Self,0,0);
  TNetData.NetData.FNetConnections.Remove(Self);
  if Assigned(TNetData.NetData.FOnNetConnectionsUpdated) then TNetData.NetData.FOnNetConnectionsUpdated(Self);
  //
  DeleteCriticalSection(FNetLock);
  if FClient.Owner=Self then FClient.Free;
  FClientBufferRead.Free;
  inherited;
end;

procedure TNetConnection.DisconnectInvalidClient(ItsMyself : Boolean; const why: AnsiString);
Var P : PNodeServerAddress;
  l : TList;
  i : Integer;
begin
  if ItsMyself then begin
    TLog.NewLog(ltInfo,Classname,'Disconecting myself '+FClient.RemoteHost+':'+FClient.RemotePort+' > '+Why)
  end else begin
    TLog.NewLog(lterror,Classname,'Disconecting '+FClient.RemoteHost+':'+FClient.RemotePort+' > '+Why);
  end;
  FIsMyselfServer := ItsMyself;
  l := TNetData.NetData.FBlackList.LockList;
  try
    i := TNetData.NetData.IndexOfNetClient(l,FClient.RemoteHost,StrToIntDef( FClient.RemotePort,CT_NetServer_Port));
    if i<0 then begin
      new(P);
      P^ := CT_TNodeServerAddress_NUL;
      TNetData.NetData.FBlackList.add(P);
    end else P := l[i];
    P^.ip := Fclient.RemoteHost;
    P^.port := StrToIntDef( FClient.RemotePort,CT_NetServer_Port);
    P^.last_connection := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    P^.its_myself := ItsMyself;
    P^.BlackListText := Why;
  finally
    TNetData.NetData.FBlackList.UnlockList;
  end;
  if ItsMyself then begin
    l := TNetData.NetData.FNodeServers.LockList;
    try
      i := TNetData.NetData.IndexOfNetClient(l,FClient.RemoteHost,StrToIntDef( FClient.RemotePort,CT_NetServer_Port));
      if i>=0 then begin
        P := l[i];
        P^.its_myself := true;
      end;
    finally
      TNetData.NetData.FNodeServers.UnlockList;
    end;
  end;
  Connected := False;
  if Assigned(TNetData.NetData.FOnBlackListUpdated) then TNetData.NetData.FOnBlackListUpdated(Self);
  if Assigned(TNetData.NetData.FOnNodeServersUpdated) then TNetData.NetData.FOnNodeServersUpdated(Self);
end;

Procedure TNetConnection.DoProcessBuffer;
Var HeaderData : TNetHeaderData;
  ms : TMemoryStream;
  ops : AnsiString;
begin
  ms := TMemoryStream.Create;
  try
    TPCThread.ProtectEnterCriticalSection(Self,FNetLock);
    Try
      if Not FIsWaitingForResponse then begin
        DoSendAndWaitForResponse(0,0,Nil,ms,0,HeaderData);
      end;
    Finally
      LeaveCriticalSection(FNetLock);
    End;
  finally
    ms.Free;
  end;
  if ((FLastDataReceivedTS+(1000*60)<GetTickCount) Or (FLastDataSendedTS+(1000*60)<GetTickCount)) then begin
    If TNetData.NetData.PendingRequest(Self,ops)>=2 then begin
      TLog.NewLog(lterror,Classname,'Pending requests without response... closing connection to '+FClient.RemoteHost+':'+FClient.RemotePort+' > '+ops);
      Connected := false;
    end else begin
      TLog.NewLog(ltDebug,Classname,'Sending Hello to check connection to '+FClient.RemoteHost+':'+FClient.RemotePort+' > '+ops);
      Send_Hello(ntp_request,TNetData.NetData.NewRequestId);
    end;
  end;
end;

procedure TNetConnection.DoProcess_AddOperations(HeaderData: TNetHeaderData; DataBuffer: TStream);
var c,i : Integer;
    optype : Byte;
    opclass : TPCOperationClass;
    op : TPCOperation;
    operations : TOperationsHashTree;
    errors : AnsiString;
  DoDisconnect : Boolean;
begin
  DoDisconnect := true;
  operations := TOperationsHashTree.Create;
  try
    if HeaderData.header_type<>ntp_autosend then begin
      errors := 'Not autosend';
      exit;
    end;
    if DataBuffer.Size<4 then begin
      errors := 'Invalid databuffer size';
      exit;
    end;
    DataBuffer.Read(c,4);
    for i := 1 to c do begin
      errors := 'Invalid operation '+inttostr(i)+'/'+inttostr(c);
      if not DataBuffer.Read(optype,1)=1 then exit;
      opclass := TPCOperationsComp.GetOperationClassByOpType(optype);
      if Not Assigned(opclass) then exit;
      op := opclass.Create;
      op.LoadFromStream(DataBuffer);
      operations.AddOperationToHashTree(op);
    end;
    DoDisconnect := false;
  finally
    try
      if DoDisconnect then begin
        DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
      end else begin
        TNode.Node.AddOperations(Self,operations,errors);
      end;
    finally
      operations.Free;
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetBlocks_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
Var b,b_start,b_end:Cardinal;
    op : TPCOperationsComp;
    db : TMemoryStream;
    c : Cardinal;
  errors : AnsiString;
  DoDisconnect : Boolean;
begin
  DoDisconnect := true;
  try
    if HeaderData.header_type<>ntp_request then begin
      errors := 'Not request';
      exit;
    end;
     // DataBuffer contains: from and to
     errors := 'Invalid structure';
     if (DataBuffer.Size-DataBuffer.Position<8) then begin
       exit;
     end;
     DataBuffer.Read(b_start,4);
     DataBuffer.Read(b_end,4);
     if (b_start<0) Or (b_start>b_end) then begin
       errors := 'Invalid structure start or end: '+Inttostr(b_start)+' '+Inttostr(b_end);
       exit;
     end;
     if (b_end>=TNetData.NetData.Bank.BlocksCount) then b_end := TNetData.NetData.Bank.BlocksCount-1;

     DoDisconnect := false;

     db := TMemoryStream.Create;
     try
       op := TPCOperationsComp.Create(TNetData.NetData.bank);
       try
         c := b_end - b_start + 1;
         db.Write(c,4);
         for b := b_start to b_end do begin
           If TNetData.NetData.bank.LoadOperations(op,b) then begin
             op.SaveToStream(false,false,db);
           end else begin
             SendError(ntp_response,HeaderData.operation,HeaderData.request_id,CT_NetError_InternalServerError,'Operations of block:'+inttostr(b)+' not found');
             exit;
           end;
         end;
         Send(ntp_response,HeaderData.operation,0,HeaderData.request_id,db);
       finally
         op.Free;
       end;
     finally
       db.Free;
     end;
     TLog.NewLog(ltdebug,Classname,'Sending operations from block '+inttostr(b_start)+' to '+inttostr(b_end));
  finally
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetBlocks_Response(HeaderData: TNetHeaderData; DataBuffer: TStream);
  var op, localop : TPCOperationsComp;
    opcount,i : Cardinal;
    newBlockAccount : TBlockAccount;
  errors : AnsiString;
  DoDisconnect : Boolean;
begin
  DoDisconnect := true;
  try
    if HeaderData.header_type<>ntp_response then begin
      errors := 'Not response';
      exit;
    end;
    // DataBuffer contains: from and to
    errors := 'Invalid structure';
    op := TPCOperationsComp.Create(Self);
    Try
      op.bank := TNode.Node.Bank;
      if DataBuffer.Size-DataBuffer.Position<4 then begin
        DisconnectInvalidClient(false,'DoProcess_GetBlocks_Response invalid format: '+errors);
        exit;
      end;
      DataBuffer.Read(opcount,4);
      DoDisconnect :=false;
      for I := 1 to opcount do begin
        if Not op.LoadFromStream(false,false,DataBuffer,errors) then begin
           errors := 'Error decoding block '+inttostr(i)+'/'+inttostr(opcount)+' Errors:'+errors;
           DoDisconnect := true;
           exit;
        end;
        if (op.OperationBlock.block=TNode.Node.Bank.BlocksCount) then begin
          if (TNode.Node.Bank.AddNewBlockChainBlock(op,newBlockAccount,errors)) then begin
            // Ok, one more!
          end else begin
            // Is not a valid entry????
            // Perhaps an orphan blockchain: Me or Client!
            localop := TPCOperationsComp.Create(Self);
            Try
              TNode.Node.Bank.LoadOperations(localop,TNode.Node.Bank.BlocksCount-1);
              TLog.NewLog(ltinfo,Classname,'Distinct operation block found! My:'+
                  TPCOperationsComp.OperationBlockToText(localop.OperationBlock)+' remote:'+TPCOperationsComp.OperationBlockToText(op.OperationBlock)+' Errors: '+errors);
            Finally
              localop.Free;
            End;
          end;
        end else begin
          // Receiving an unexpected operationblock
          TLog.NewLog(lterror,classname,'ReceivedGetOperations an unexpected operationblock: '+TPCOperationsComp.OperationBlockToText(op.OperationBlock));
          exit;
        end;
      end;
      if ((opcount>0) And (FRemoteOperationBlock.block>=TNode.Node.Bank.BlocksCount)) then begin
        Send_GetBlocks(TNode.Node.Bank.BlocksCount,5,i);
      end;
    Finally
      op.Free;
    End;
  Finally
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetOperationsBlock_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
Const CT_Max_Positions = 10;
Var inc_b,b,b_start,b_end, total_b:Cardinal;
  op : TPCOperationsComp;
  db,msops : TMemoryStream;
  errors, blocksstr : AnsiString;
  DoDisconnect : Boolean;
begin
  blocksstr := '';
  DoDisconnect := true;
  try
    if HeaderData.header_type<>ntp_request then begin
      errors := 'Not request';
      exit;
    end;
    errors := 'Invalid structure';
    if (DataBuffer.Size-DataBuffer.Position<8) then begin
       exit;
    end;
    DataBuffer.Read(b_start,4);
    DataBuffer.Read(b_end,4);
    if (b_start<0) Or (b_start>b_end) Or (b_start>=TNode.Node.Bank.BlocksCount) then begin
      errors := 'Invalid start ('+Inttostr(b_start)+') or end ('+Inttostr(b_end)+') of count ('+Inttostr(TNode.Node.Bank.BlocksCount)+')';
      exit;
    end;

    DoDisconnect := false;

    if (b_end>=TNode.Node.Bank.BlocksCount) then b_end := TNode.Node.Bank.BlocksCount-1;
    inc_b := ((b_end - b_start) DIV CT_Max_Positions)+1;
    msops := TMemoryStream.Create;
    op := TPCOperationsComp.Create(TNode.Node.Bank);
     try
       b := b_start;
       total_b := 0;
       repeat
         If TNode.Node.bank.LoadOperations(op,b) then begin
           op.SaveToStream(false,true,msops);
           blocksstr := blocksstr + inttostr(b)+',';
           b := b + inc_b;
           inc(total_b);
         end else begin
           SendError(ntp_response,HeaderData.operation,HeaderData.request_id,CT_NetError_InternalServerError,'Operations of block:'+inttostr(b)+' not found');
           exit;
         end;
       until (b > b_end);
       db := TMemoryStream.Create;
       try
         db.Write(total_b,4);
         db.WriteBuffer(msops.Memory^,msops.Size);
         Send(ntp_response,HeaderData.operation,0,HeaderData.request_id,db);
       finally
         db.Free;
       end;
     finally
       msops.Free;
       op.Free;
     end;
     TLog.NewLog(ltdebug,Classname,'Sending '+inttostr(total_b)+' operations block from block '+inttostr(b_start)+' to '+inttostr(b_end)+' '+blocksstr);
  finally
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_Hello(HeaderData: TNetHeaderData; DataBuffer: TStream);
var op, myLastOp : TPCOperationsComp;
    errors : AnsiString;
    messagehello : TNetMessage_Hello;
    connection_has_a_server : Word;
    i,c : Integer;
    nsa : TNodeServerAddress;
    rid : Cardinal;
    connection_ts : Cardinal;
   Duplicate : TNetConnection;
   RawAccountKey : TRawBytes;
Begin
  SetLength(messagehello.servers_address,0);
  op := TPCOperationsComp.Create(Nil);
  try
    DataBuffer.Position:=0;
    if DataBuffer.Read(connection_has_a_server,2)<2 then begin
      DisconnectInvalidClient(false,'Invalid data on buffer: '+TNetData.HeaderDataToText(HeaderData));
      exit;
    end;
    If TStreamOp.ReadAnsiString(DataBuffer,RawAccountKey)<0 then begin
      DisconnectInvalidClient(false,'Invalid data on buffer. No Public key: '+TNetData.HeaderDataToText(HeaderData));
      exit;
    end;
    FClientPublicKey := TAccountComp.RawString2Accountkey(RawAccountKey);
    If Not TAccountComp.IsValidAccountKey(FClientPublicKey,errors) then begin
      DisconnectInvalidClient(false,'Invalid Public key: '+TNetData.HeaderDataToText(HeaderData)+' errors: '+errors);
      exit;
    end;
    if DataBuffer.Read(connection_ts,4)<4 then begin
      DisconnectInvalidClient(false,'Invalid data on buffer. No TS: '+TNetData.HeaderDataToText(HeaderData));
      exit;
    end;
    FLastKnownTimestampDiff := Int64(connection_ts) - Int64(UnivDateTimeToUnix( DateTime2UnivDateTime(now)));
    if (FLastKnownTimestampDiff<>0) then begin
      TLog.NewLog(ltdebug,Classname,'Processing a hello from a client with different time. Difference: '+Inttostr(FLastKnownTimestampDiff));
    end;
    If (connection_ts > (UnivDateTimeToUnix(DateTime2UnivDateTime(now))+CT_MaxSecondsDifferenceOfNetworkNodes)) then begin
      DisconnectInvalidClient(false,'Invalid remote timestamp. Difference:'+inttostr(FLastKnownTimestampDiff)+' > '+inttostr(CT_MaxSecondsDifferenceOfNetworkNodes));
    end;
    if (connection_has_a_server>0) And (Not SameText(FClient.RemoteHost,'localhost')) And (Not SameText(FClient.RemoteHost,'127.0.0.1'))
      And (Not SameText('192.168',Copy(FClient.RemoteHost,1,7)))
      And (Not TAccountComp.Equal(FClientPublicKey,TNetData.NetData.FNodePrivateKey.PublicKey)) then begin
      nsa := CT_TNodeServerAddress_NUL;
      nsa.ip := FClient.RemoteHost;
      nsa.port := connection_has_a_server;
      nsa.last_connection := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
      TNetData.NetData.AddServer(nsa);
    end;

    if op.LoadFromStream(false,false,DataBuffer,errors) then begin
      messagehello.last_operation := op.OperationBlock;
      FRemoteOperationBlock := op.OperationBlock;
      If (TNetData.NetData.FMaxRemoteOperationBlock.block<FRemoteOperationBlock.block) then begin
        TNetData.NetData.FMaxRemoteOperationBlock := FRemoteOperationBlock;
        if TPCThread.ThreadClassFound(TThreadGetNewBlockChainFromClient,nil)<0 then begin
          TThreadGetNewBlockChainFromClient.Create(false).FreeOnTerminate := true;
        end;
      end;
      if (DataBuffer.Size-DataBuffer.Position>=4) then begin
        DataBuffer.Read(c,4);
        for i := 1 to c do begin
          nsa := CT_TNodeServerAddress_NUL;
          TStreamOp.ReadAnsiString(DataBuffer,nsa.ip);
          DataBuffer.Read(nsa.port,2);
          DataBuffer.Read(nsa.last_connection,4);
          SetLength(messagehello.servers_address,length(messagehello.servers_address)+1);
          messagehello.servers_address[high(messagehello.servers_address)] := nsa;
          TNetData.NetData.AddServer(nsa);
        end;
      end;
      TLog.NewLog(ltdebug,Classname,'Hello received: '+TPCOperationsComp.OperationBlockToText(FRemoteOperationBlock));
      if (HeaderData.header_type in [ntp_request,ntp_response]) then begin
        // Response:
        if (HeaderData.header_type=ntp_request) then begin
          Send_Hello(ntp_response,HeaderData.request_id);
        end;
        if (TAccountComp.Equal(FClientPublicKey,TNetData.NetData.FNodePrivateKey.PublicKey)) then begin
          DisconnectInvalidClient(true,'MySelf disconnecting...');
          exit;
        end;
        Duplicate := TNetData.NetData.FindConnectionByClientRandomValue(Self);
        if (Duplicate<>Nil) And (Duplicate.Connected) then begin
          DisconnectInvalidClient(true,'Duplicate connection with '+Duplicate.FClient.RemoteHost+':'+Duplicate.FClient.RemotePort);
          exit;
        end;

        if (HeaderData.header_type = ntp_response) then begin
          if Assigned(TNetData.NetData.OnReceivedHelloResponse) then TNetData.NetData.OnReceivedHelloResponse(Self);
        end;
      end else begin
        DisconnectInvalidClient(false,'Invalid header type > '+TNetData.HeaderDataToText(HeaderData));
      end;
    end else begin
      TLog.NewLog(lterror,Classname,'Error decoding operations of HELLO: '+errors);
      DisconnectInvalidClient(false,'Error decoding operations of HELLO: '+errors);
    end;
  finally
    op.Free;
  end;
end;

procedure TNetConnection.DoProcess_Message(HeaderData: TNetHeaderData; DataBuffer: TStream);
Var   errors : AnsiString;
  decrypted,messagecrypted : AnsiString;
  DoDisconnect : boolean;
begin
  errors := '';
  DoDisconnect := true;
  try
    if HeaderData.header_type<>ntp_autosend then begin
      errors := 'Not autosend';
      exit;
    end;
    If TStreamOp.ReadAnsiString(DataBuffer,messagecrypted)<0 then begin
      errors := 'Invalid message data';
      exit;
    end;
    If Not ECIESDecrypt(TNetData.NetData.FNodePrivateKey.EC_OpenSSL_NID,TNetData.NetData.FNodePrivateKey.PrivateKey,false,messagecrypted,decrypted) then begin
      errors := 'Error on decrypting message';
      exit;
    end;

    DoDisconnect := false;
    if TCrypto.IsHumanReadable(decrypted) then
      TLog.NewLog(ltinfo,Classname,'Received new message from '+FClient.RemoteHost+':'+Fclient.RemotePort+' Message ('+inttostr(length(decrypted))+' bytes): '+decrypted)
    else
      TLog.NewLog(ltinfo,Classname,'Received new message from '+FClient.RemoteHost+':'+Fclient.RemotePort+' Message ('+inttostr(length(decrypted))+' bytes) in hexadecimal: '+TCrypto.ToHexaString(decrypted));
    Try
      TNode.Node.NotifyNetClientMessage(Self,decrypted);
    Except
      On E:Exception do begin
        TLog.NewLog(lterror,Classname,'Error processing received message. '+E.ClassName+' '+E.Message);
      end;
    end;
  finally
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_NewBlock(HeaderData: TNetHeaderData; DataBuffer: TStream);
var bacc : TBlockAccount;
    op : TPCOperationsComp;
  errors : AnsiString;
  DoDisconnect : Boolean;
begin
  errors := '';
  DoDisconnect := true;
  try
    if HeaderData.header_type<>ntp_autosend then begin
      errors := 'Not autosend';
      exit;
    end;
    op := TPCOperationsComp.Create(Self);
    try
      op.bank := TNode.Node.Bank;
      if Not op.LoadFromStream(false,false,DataBuffer,errors) then begin
        errors := 'Error decoding new account: '+errors;
        exit;
      end else begin
        DoDisconnect := false;
        FRemoteOperationBlock := op.OperationBlock;
        //
        if (op.OperationBlock.block>TNode.Node.Bank.BlocksCount) then begin
          TNetData.NetData.GetNewBlockChainFromClient(Self);
        end else if (op.OperationBlock.block=TNode.Node.Bank.BlocksCount) then begin
          // New block candidate:
          If Not TNode.Node.AddNewBlockChain(nil,Self,op,bacc,errors) then begin
            // Received a new invalid block... perhaps I'm an orphan blockchain
            TNetData.NetData.GetNewBlockChainFromClient(Self);
          end;
        end;
      end;
    finally
      op.Free;
    end;
  finally
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

function TNetConnection.DoSendAndWaitForResponse(operation: Word;
  RequestId: Integer; SendDataBuffer, ReceiveDataBuffer: TStream;
  MaxWaitTime: Cardinal; var HeaderData: TNetHeaderData): Boolean;
var tc : Cardinal;
  was_waiting_for_response : Boolean;
begin
  Result := false;
  HeaderData := CT_NetHeaderData;
  If FIsWaitingForResponse then begin
    TLog.NewLog(ltdebug,Classname,'Is waiting for response ...');
    exit;
  end;
  If Not Assigned(FClient) then exit;
  if Not FClient.Active then exit;
  TPCThread.ProtectEnterCriticalSection(Self,FNetLock);
  Try
    was_waiting_for_response := RequestId>0;
    try
      if was_waiting_for_response then begin
        FIsWaitingForResponse := true;
        Send(ntp_request,operation,0,RequestId,SendDataBuffer);
      end;
      FSocketError := 0;
      tc := GetTickCount;
      Repeat
        if Not FClient.WaitForData(100) then begin
          If FSocketError<>0 then begin
            TLog.NewLog(ltdebug,classname,'Broken connection by error '+Inttostr(FSocketError)+' to '+FClient.RemoteHost+':'+FClient.RemotePort);
            Connected := false;
            exit;
          end;
          if (GetTickCount-tc < 50) then begin
            // Broken!
            TLog.NewLog(ltdebug,classname,'Broken connection to '+FClient.RemoteHost+':'+FClient.RemotePort);
            Connected := false;
            exit;
          end;
          if (FClientBufferRead.Size=0) And (RequestId=0) then exit; // Nothing to read nor wait
        end;
        if (ReadTcpClientBuffer(MaxWaitTime,HeaderData,ReceiveDataBuffer)) then begin
          TLog.NewLog(ltDebug,Classname,'Received '+CT_NetTransferType[HeaderData.header_type]+' operation:'+TNetData.OperationToText(HeaderData.operation)+' id:'+Inttostr(HeaderData.request_id)+' Buffer size:'+Inttostr(HeaderData.buffer_data_length) );
          if (RequestId=HeaderData.request_id) And (HeaderData.header_type=ntp_response) then begin
            Result := true;
          end else begin
            case HeaderData.operation of
              CT_NetOp_Hello : Begin
                DoProcess_Hello(HeaderData,ReceiveDataBuffer);
              End;
              CT_NetOp_Message : Begin
                DoProcess_Message(HeaderData,ReceiveDataBuffer);
              End;
              CT_NetOp_GetBlocks : Begin
                if HeaderData.header_type=ntp_request then
                  DoProcess_GetBlocks_Request(HeaderData,ReceiveDataBuffer)
                else if HeaderData.header_type=ntp_response then
                  DoProcess_GetBlocks_Response(HeaderData,ReceiveDataBuffer)
                else DisconnectInvalidClient(false,'Not resquest or response: '+TNetData.HeaderDataToText(HeaderData));
              End;
              CT_NetOp_GetOperationsBlock : Begin
                if HeaderData.header_type=ntp_request then
                  DoProcess_GetOperationsBlock_Request(HeaderData,ReceiveDataBuffer)
                else TLog.NewLog(ltdebug,Classname,'Received old response of: '+TNetData.HeaderDataToText(HeaderData));
              End;
              CT_NetOp_NewBlock : Begin
                DoProcess_NewBlock(HeaderData,ReceiveDataBuffer);
              End;
              CT_NetOp_AddOperations : Begin
                DoProcess_AddOperations(HeaderData,ReceiveDataBuffer);
              End;
            else
              DisconnectInvalidClient(false,'Invalid operation: '+TNetData.HeaderDataToText(HeaderData));
            end;
          end;
        end;
      Until (Result) Or (GetTickCount>(MaxWaitTime+tc));
    finally
      if was_waiting_for_response then FIsWaitingForResponse := false;
    end;
  Finally
    LeaveCriticalSection(FNetLock);
  End;
end;

function TNetConnection.GetConnected: Boolean;
begin
  Result := Assigned(FClient) And (FClient.Connected);
end;

procedure TNetConnection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation=opRemove) And (AComponent = FClient) then FClient := Nil;
end;

function TNetConnection.ReadTcpClientBuffer(MaxWaitMiliseconds: Cardinal; var HeaderData: TNetHeaderData; BufferData: TStream): Boolean;
var buffer : Array[1..4096] of byte;
  auxstream : TMemoryStream;
  tc : Cardinal;
  last_bytes_read : Integer;
  //
  operation : Word;
  request_id : Integer;
  IsValidHeaderButNeedMoreData : Boolean;
begin
  Result := false;
  HeaderData := CT_NetHeaderData;
  BufferData.Size := 0;
  TPCThread.ProtectEnterCriticalSection(Self,FNetLock);
  try
    If not Connected then exit;
    if Not FClient.Active then exit;
    tc := GetTickCount;
    repeat
      last_bytes_read := 0;
      FClientBufferRead.Position := 0;
      Result := TNetData.ExtractHeaderInfo(FClientBufferRead,HeaderData,BufferData,IsValidHeaderButNeedMoreData);
      if Result then begin
        if HeaderData.protocol.protocol_version<>CT_Protocol_Version then begin
          DisconnectInvalidClient(false,Format('Invalid protocol version found: %d available: %d',[HeaderData.protocol.protocol_version,HeaderData.protocol.protocol_available]));
          Result := false;
          exit;
        end else begin
          auxstream := TMemoryStream.Create;
          try
            if FClientBufferRead.Position<FClientBufferRead.Size then begin
              auxstream.CopyFrom(FClientBufferRead,FClientBufferRead.Size-FClientBufferRead.Position);
            end;
            FClientBufferRead.Size := 0;
            FClientBufferRead.CopyFrom(auxstream,0);
          finally
            auxstream.Free;
          end;
        end;
      end else begin
        if Not FClient.WaitForData(100) then begin
          exit;
        end;
        last_bytes_read := FClient.ReceiveBuf(buffer,sizeof(buffer));
        if last_bytes_read>0 then begin
          FLastDataReceivedTS := GetTickCount;
          TNetData.NetData.IncStatistics(0,0,0,last_bytes_read,0);
        end;
        FClientBufferRead.Position := FClientBufferRead.size; // Go to the end
        FClientBufferRead.Write(buffer,last_bytes_read);
        FClientBufferRead.Position := 0;
      end;
    until (Result) Or ((GetTickCount > (tc+MaxWaitMiliseconds)) And (last_bytes_read=0));
  finally
    if (Not Result) And (FClientBufferRead.Size>0) And (Not IsValidHeaderButNeedMoreData) then begin
      TLog.NewLog(lterror,ClassName,Format('Deleting %d bytes from TcpClient buffer of %s:%s after max %d miliseconds. Passed: %d',
        [FClientBufferRead.Size, FClient.RemoteHost,FClient.RemotePort,MaxWaitMiliseconds,GetTickCount-tc]));
      FClientBufferRead.Size:=0;
    end;
    LeaveCriticalSection(FNetLock);
  end;
  if (Result) And (HeaderData.header_type=ntp_response) then begin
    TNetData.NetData.UnRegisterRequest(Self,HeaderData.operation,HeaderData.request_id);
  end;
end;

procedure TNetConnection.Send(NetTranferType: TNetTransferType; operation, errorcode: Word; request_id: Integer; DataBuffer: TStream);
Var l : Cardinal;
   w : Word;
  Buffer : TStream;
  s : AnsiString;
begin
  Buffer := TMemoryStream.Create;
  try
    l := CT_MagicNetIdentification;
    Buffer.Write(l,4);
    case NetTranferType of
      ntp_request: begin
        w := CT_MagicRequest;
        Buffer.Write(w,2);
        Buffer.Write(operation,2);
        w := 0;
        Buffer.Write(w,2);
        Buffer.Write(request_id,4);
      end;
      ntp_response: begin
        w := CT_MagicResponse;
        Buffer.Write(w,2);
        Buffer.Write(operation,2);
        Buffer.Write(errorcode,2);
        Buffer.Write(request_id,4);
      end;
      ntp_autosend: begin
        w := CT_MagicAutoSend;
        Buffer.Write(w,2);
        Buffer.Write(operation,2);
        w := errorcode;
        Buffer.Write(w,2);
        l := 0;
        Buffer.Write(l,4);
      end
    else
      raise Exception.Create('Invalid encoding');
    end;
    l := CT_Protocol_Version;
    Buffer.Write(l,2);
    l := CT_Protocol_Available;
    Buffer.Write(l,2);
    if Assigned(DataBuffer) then begin
      l := DataBuffer.Size;
      Buffer.Write(l,4);
      DataBuffer.Position := 0;
      Buffer.CopyFrom(DataBuffer,DataBuffer.Size);
      s := '(Data:'+inttostr(DataBuffer.Size)+'b) ';
    end else begin
      l := 0;
      Buffer.Write(l,4);
      s := '';
    end;
    Buffer.Position := 0;
    TPCThread.ProtectEnterCriticalSection(Self,FNetLock);
    Try
      TLog.NewLog(ltDebug,Classname,'Sending: '+CT_NetTransferType[NetTranferType]+' operation:'+
        TNetData.OperationToText(operation)+' id:'+Inttostr(request_id)+' errorcode:'+InttoStr(errorcode)+
        ' Size:'+InttoStr(Buffer.Size)+'b '+s+'to '+
        FClient.RemoteHost+':'+FClient.RemotePort);
      FClient.SendStream(Buffer);
      FLastDataSendedTS := GetTickCount;
      TNetData.NetData.IncStatistics(0,0,0,0,Buffer.Size);
    Finally
      LeaveCriticalSection(FNetLock);
    End;
  finally
    Buffer.Free;
  end;
end;

procedure TNetConnection.SendError(NetTranferType: TNetTransferType; operation, request_id, error_code: Integer; error_text: AnsiString);
var buffer : TStream;
begin
  buffer := TMemoryStream.Create;
  Try
    TStreamOp.WriteAnsiString(buffer,error_text);
    Send(NetTranferType,operation,error_code,request_id,buffer);
  Finally
    buffer.Free;
  End;
end;

function TNetConnection.Send_AddOperations(Operations : TOperationsHashTree) : Boolean;
Var data : TMemoryStream;
  c1,c2,request_id : Cardinal;
  i : Integer;
  optype : Byte;
begin
  Result := false;
  data := TMemoryStream.Create;
  try
    request_id := TNetData.NetData.NewRequestId;
    c1 := Operations.OperationsCount;
    data.Write(c1,4);
    for i := 0 to Operations.OperationsCount-1 do begin
      optype := Operations.GetOperation(i).OpType;
      data.Write(optype,1);
      Operations.GetOperation(i).SaveToStream(data);
    end;
    Send(ntp_autosend,CT_NetOp_AddOperations,0,request_id,data);
    Result := FClient.Active;
  finally
    data.Free;
  end;
end;

function TNetConnection.Send_GetBlocks(StartAddress, quantity : Cardinal; var request_id : Cardinal) : Boolean;
Var data : TMemoryStream;
  c1,c2 : Cardinal;
begin
  Result := false;
  request_id := 0;
  if (FRemoteOperationBlock.block<TNetData.NetData.Bank.BlocksCount) Or (FRemoteOperationBlock.block=0) then exit;
  // First receive operations from
  data := TMemoryStream.Create;
  try
    if TNetData.NetData.Bank.BlocksCount=0 then c1:=0
    else c1:=StartAddress;
    if (quantity=0) then begin
      if FRemoteOperationBlock.block>0 then c2 := FRemoteOperationBlock.block
      else c2 := c1+100;
    end else c2 := c1+quantity-1;
    if (FRemoteOperationBlock.block>0) And (c2>FRemoteOperationBlock.block) then c2 := FRemoteOperationBlock.block;
    data.Write(c1,4);
    data.Write(c2,4);
    request_id := TNetData.NetData.NewRequestId;
    TNetData.NetData.RegisterRequest(Self,CT_NetOp_GetBlocks,request_id);
    TLog.NewLog(ltdebug,ClassName,Format('Send GET BLOCKS start:%d quantity:%d (from:%d to %d)',[StartAddress,quantity,StartAddress,quantity+StartAddress]));
    Send(ntp_request,CT_NetOp_GetBlocks,0,request_id,data);
    Result := FClient.Active;
  finally
    data.Free;
  end;
end;

function TNetConnection.Send_Hello(NetTranferType : TNetTransferType; request_id : Integer) : Boolean;
  { HELLO command:
    - Operation stream
    - My Active server port (0 if no active). (2 bytes)
    - A Random Longint (4 bytes) to check if its myself connection to my server socket
    - My Unix Timestamp (4 bytes)
    - Registered node servers count
      (For each)
      - ip (string)
      - port (2 bytes)
      - last_connection UTS (4 bytes)
    - My Server port (2 bytes)
    - If this is a response:
      - If remote operation block is lower than me:
        - Send My Operation Stream in the same block thant requester
      }
var data : TStream;
  i : Integer;
  op : TPCOperationsComp;
  nsa : TNodeServerAddress;
  nsarr : Array of TNodeServerAddress;
  w : Word;
  c : Cardinal;
  l : TList;
begin
  Result := false;
  if Not Connected then exit;
  // Send Hello command:
  data := TMemoryStream.Create;
  try
    if NetTranferType=ntp_request then begin
      TNetData.NetData.RegisterRequest(Self,CT_NetOp_Hello,request_id);
    end;
    If TNode.Node.NetServer.Active then
      w := TNode.Node.NetServer.Port
    else w := 0;
    // Save active server port (2 bytes). 0 = No active server port
    data.Write(w,2);
    // Save a random value (4 bytes)
    TStreamOp.WriteAnsiString(data,TAccountComp.AccountKey2RawString(TNetData.NetData.FNodePrivateKey.PublicKey));
    // Save my Unix timestamp (4 bytes)
    c := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    data.Write(c,4);
    // Save last operations block
    op := TPCOperationsComp.Create(TNode.Node.Bank);
    try
      if (TNode.Node.Bank.BlocksCount>0) then TNode.Node.Bank.LoadOperations(op,TNode.Node.Bank.BlocksCount-1);
      op.SaveToStream(false,true,data);
      SetLength(nsarr,0);
      // Save other node servers
      l := TNetData.NetData.FNodeServers.LockList;
      try
        for i := 0 to l.Count - 1 do begin
          nsa := PNodeServerAddress( l[i] )^;
          if (Not (nsa.its_myself)) And
            (nsa.BlackListText='') And
            (nsa.last_connection>0) then begin
            SetLength(nsarr,length(nsarr)+1);
            nsarr[high(nsarr)] := nsa;
          end;
        end;
      finally
        TNetData.NetData.FNodeServers.UnlockList;
      end;
      i := length(nsarr);
      data.Write(i,4);
      for i := 0 to High(nsarr) do begin
        nsa := nsarr[i];
        TStreamOp.WriteAnsiString(data, nsa.ip);
        data.Write(nsa.port,2);
        data.Write(nsa.last_connection,4);
      end;
      //
      if NetTranferType=ntp_response then begin
        if (TNode.Node.Bank.BlocksCount>0) AND (FRemoteOperationBlock.block<TNode.Node.Bank.BlocksCount-1) then begin
          TNode.Node.Bank.LoadOperations(op,FRemoteOperationBlock.block);
          if FRemoteOperationBlock.proof_of_work<>op.OperationBlock.proof_of_work then begin
            TLog.NewLog(ltinfo,Classname,'Found a possible orphan block lower than me (me:'+inttostr(TNode.Node.Bank.BlocksCount-1)+' client:'+
              inttostr(FRemoteOperationBlock.block)+') ... at '+FClient.RemoteHost+':'+FClient.RemotePort );
          end;
          op.SaveToStream(false,true,data);
        end;
      end;
    finally
      op.free;
    end;
    //
    Send(NetTranferType,CT_NetOp_Hello,0,request_id,data);
    Result := FClient.Active;
  finally
    data.Free;
  end;
end;

function TNetConnection.Send_Message(const TheMessage: AnsiString): Boolean;
Var data : TStream;
  cyp : TRawBytes;
begin
  Result := false;
  if Not Connected then exit;
  data := TMemoryStream.Create;
  Try
    // Cypher message:
    cyp := ECIESEncrypt(FClientPublicKey,TheMessage);
    TStreamOp.WriteAnsiString(data,cyp);
    Send(ntp_autosend,CT_NetOp_Message,0,0,data);
    Result := true;
  Finally
    data.Free;
  End;
end;

function TNetConnection.Send_NewBlockFound: Boolean;
var data : TStream;
  request_id : Integer;
  op : TPCOperationsComp;
begin
  Result := false;
  if TNetData.NetData.Bank.BlocksCount=0 then exit;
  if Connected then begin
    // Send Hello command:
    data := TMemoryStream.Create;
    try
      request_id := TNetData.NetData.NewRequestId;
      op := TPCOperationsComp.Create(Self);
      try
        op.bank := TNetData.NetData.Bank;
        if Not TNetData.NetData.Bank.LoadOperations(op,TNetData.NetData.Bank.BlocksCount-1) then begin
          TLog.NewLog(lterror,Classname,'Error on Send_NewBlockFound. Cannot load BlockOperations '+inttostr(TNetData.NetData.Bank.BlocksCount-1));
          exit;
        end;
        op.SaveToStream(false,false,data);
        Send(ntp_autosend,CT_NetOp_NewBlock,0,request_id,data);
      finally
        op.free;
      end;
    finally
      data.Free;
    end;
    Result := FClient.Active;
  end;
end;

procedure TNetConnection.SetClient(const Value: TNetTcpIpClient);
begin
  if FClient<>Value then begin
    if Assigned(FClient) then begin
      FClient.RemoveFreeNotification(Self);
    end;
    TNetData.NetData.UnRegisterRequest(Self,0,0);
    FClient.Free;
    FClient := Value;
  end;
  if Assigned(FClient) then begin
    FClient.FreeNotification(Self);
    FClient.OnConnect := TcpClient_OnConnect;
    FClient.OnCreateHandle := TcpClient_OnCreateHandle;
    FClient.OnDestroyHandle := TcpClient_OnDestroyHandle;
    FClient.OnError := TcpClient_OnError;
    FClient.OnDisconnect := TcpClient_OnDisconnect;
    FClient.OnReceive := TcpClient_OnReceive;
    FClient.OnSend := TcpClient_OnSend;
  end;
  if Assigned(TNetData.NetData.FOnNetConnectionsUpdated) then TNetData.NetData.FOnNetConnectionsUpdated(Self);
end;

procedure TNetConnection.SetConnected(const Value: Boolean);
begin
  if (Value = GetConnected) then exit;
  if Value then ConnectTo(FClient.RemoteHost,StrToIntDef(FClient.RemotePort,CT_NetServer_Port))
  else FClient.Disconnect;
end;

procedure TNetConnection.TcpClient_OnConnect(Sender: TObject);
begin
  TNetData.NetData.IncStatistics(1,0,1,0,0);
  TLog.NewLog(ltInfo,Classname,'Connected to a server '+FClient.RemoteHost+':'+FClient.RemotePort);
  if Assigned(TNetData.NetData.FOnNetConnectionsUpdated) then TNetData.NetData.FOnNetConnectionsUpdated(Self);
end;

procedure TNetConnection.TcpClient_OnCreateHandle(Sender: TObject);
begin
  //
end;

procedure TNetConnection.TcpClient_OnDestroyHandle(Sender: TObject);
begin
  //
end;

procedure TNetConnection.TcpClient_OnDisconnect(Sender: TObject);
begin
  if self is TNetServerClient then TNetData.NetData.IncStatistics(-1,-1,0,0,0)
  else TNetData.NetData.IncStatistics(-1,0,-1,0,0);
  TLog.NewLog(ltInfo,Classname,'Disconnected from '+FClient.RemoteHost+':'+FClient.RemotePort);
  if Assigned(TNetData.NetData.FOnNetConnectionsUpdated) then TNetData.NetData.FOnNetConnectionsUpdated(Self);
end;

procedure TNetConnection.TcpClient_OnError(Sender: TObject; SocketError: Integer);
begin
  FSocketError := SocketError;
  TLog.NewLog(ltdebug,Classname,'Error '+inttohex(SocketError,8)+' with connection to '+FClient.RemoteHost+':'+FClient.RemotePort);
end;

procedure TNetConnection.TcpClient_OnReceive(Sender: TObject; Buf: PAnsiChar; var DataLen: Integer);
begin
  //
end;

procedure TNetConnection.TcpClient_OnSend(Sender: TObject; Buf: PAnsiChar; var DataLen: Integer);
begin
  //
end;

{ TNetClientThread }

procedure TNetClientThread.BCExecute;
begin
  while (Not Terminated) do begin
    If FNetClient.Connected then
      FNetClient.DoProcessBuffer;
    Sleep(1);
  end;
end;

constructor TNetClientThread.Create(NetClient: TNetClient);
begin
  FNetClient := NetClient;
  inherited Create(false);
end;

{ TNetClient }

constructor TNetClient.Create(AOwner: TComponent);
begin
  inherited;
  FNetClientThread := TNetClientThread.Create(Self);
  FNetClientThread.OnTerminate := OnNetClientThreadTerminated;
  FNetClientThread.FreeOnTerminate := false;
end;

destructor TNetClient.Destroy;
begin
  if Not FNetClientThread.Terminated then begin
    FNetClientThread.Terminate;
    FNetClientThread.WaitFor;
  end;
  inherited;
end;

procedure TNetClient.OnNetClientThreadTerminated(Sender: TObject);
begin

end;

{ TThreadDiscoverConnection }

procedure TThreadDiscoverConnection.BCExecute;
Var NC : TNetClient;
  ok : Boolean;
  lns : TList;
  i : Integer;
  Pnsa : PNodeServerAddress;
begin
  Pnsa := Nil;
  // Register attempt
  lns := TNetData.NetData.FNodeServers.LockList;
  try
    i := TNetData.NetData.IndexOfNetClient(lns,FNodeServerAddress.ip,FNodeServerAddress.port);
    if i>=0 then begin
      Pnsa := PNodeServerAddress(lns[i]);
      Pnsa.last_attempt_to_connect := Now;
      Inc(Pnsa.total_failed_attemps_to_connect);
    end;
  finally
    TNetData.NetData.FNodeServers.UnlockList;
  end;
  Synchronize(Synchronized_notify);
  // Try to connect
  ok := false;
  NC := TNetClient.Create(Nil);
  Try
    If NC.ConnectTo(FNodeServerAddress.ip,FNodeServerAddress.port) then begin
      Sleep(500);
      ok :=NC.Connected;
      lns := TNetData.NetData.FNodeServers.LockList;
      try
        i := TNetData.NetData.IndexOfNetClient(lns,FNodeServerAddress.ip,FNodeServerAddress.port);
        if i>=0 then begin
          PNodeServerAddress(lns[i])^.last_connection := (UnivDateTimeToUnix(DateTime2UnivDateTime(now)));
          PNodeServerAddress(lns[i])^.total_failed_attemps_to_connect := 0; // Clean attemps counter
        end;
      finally
        TNetData.NetData.FNodeServers.UnlockList;
      end;
    end;
  Finally
    if not ok then begin
      NC.Free;
    end;
  End;
  Synchronize(Synchronized_notify);
end;

constructor TThreadDiscoverConnection.Create(NodeServerAddress: TNodeServerAddress; NotifyOnTerminate : TNotifyEvent);
begin
  FNodeServerAddress := NodeServerAddress;
  inherited Create(true);
  OnTerminate := NotifyOnTerminate;
  FreeOnTerminate := true;
  Suspended := false;
end;

procedure TThreadDiscoverConnection.Synchronized_notify;
begin
  if Assigned(TNetData.NetData.FOnNodeServersUpdated) then TNetData.NetData.FOnNodeServersUpdated(Self);
end;

{ TThreadCheckConnections }

procedure TThreadCheckConnections.BCExecute;
Var l : TList;
  i, nactive,ndeleted,ntotal : Integer;
begin
  while (Not Terminated) do begin
    if ((GetTickCount>(FLastCheckTS+30000)) AND (Not TNetData.NetData.FIsDiscoveringServers)) then begin
      nactive := 0;
      ndeleted := 0;
      ntotal := 0;
      FLastCheckTS := GetTickCount;
      l := TNetData.NetData.FNetConnections.LockList;
      try
        ntotal := l.Count;
        for i := l.Count-1 downto 0 do begin
          if (TObject(l.Items[i]) is TNetClient) then begin
            if Not TNetClient(l[i]).Connected then begin
              // Free this!
              TNetClient(l[i]).Free;
              inc(ndeleted);
            end else inc(nactive);
          end;
        end;
      finally
        TNetData.NetData.FNetConnections.UnlockList;
      end;
      if (nactive<=CT_MaxServersConnected) And (Not Terminated) then begin
        // Discover
        TNetData.NetData.DiscoverServers;
      end;
    end;
    sleep(100);
  end;
end;

{ TThreadGetNewBlockChainFromClient }

procedure TThreadGetNewBlockChainFromClient.BCExecute;
Var i : Integer;
  nsa : TNodeServerAddress;
  candidates : TList;
  lop : TOperationBlock;
begin
  // Search better candidates:
  candidates := TList.Create;
  try
    lop := CT_OperationBlock_NUL;
    for i := 0 to TNetData.NetData.ConnectionsCount(false) - 1 do begin
      TNetData.NetData.FMaxRemoteOperationBlock := CT_OperationBlock_NUL;
      if (TNetData.NetData.Connection(i).FRemoteOperationBlock.block>=TNode.Node.Bank.BlocksCount) And
         (TNetData.NetData.Connection(i).FRemoteOperationBlock.block>=lop.block)
         then begin
         candidates.Add(TNetData.NetData.Connection(i));
         lop := TNetData.NetData.Connection(i).FRemoteOperationBlock;
      end;
    end;
    TNetData.NetData.FMaxRemoteOperationBlock := lop;
    if (candidates.Count>0) then begin
      // Random a candidate
      i := Random(candidates.Count); // i = 0..count-1
      TNetData.NetData.GetNewBlockChainFromClient(TNetConnection(candidates[i]));
    end;
  finally
    candidates.Free;
  end;
end;

end.
