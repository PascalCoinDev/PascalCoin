unit UNetProtocol;

{ Copyright (c) 2016 by Albert Molina

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

Uses
{$IFnDEF FPC}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
{$ELSE}
{$ENDIF}
  UBlockChain, Classes, SysUtils, UAccounts, UThread,
  UCrypto, UTCPIP, SyncObjs, UBaseTypes, UCommon, UPCOrderedLists,
  {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults
  {$ELSE}Generics.Collections,Generics.Defaults{$ENDIF},
  UNetProtection;

{$I config.inc}

Const
  CT_MagicRequest = $0001;
  CT_MagicResponse = $0002;
  CT_MagicAutoSend = $0003;

  CT_NetOp_Hello                = $0001; // Sends my last operationblock + servers. Receive last operationblock + servers + same operationblock number of sender
  CT_NetOp_Error                = $0002;
  CT_NetOp_Message              = $0003;
  CT_NetOp_GetBlockHeaders      = $0005; // Sends from and to. Receive a number of OperationsBlock to check
  CT_NetOp_GetBlocks            = $0010;
  CT_NetOp_NewBlock             = $0011;
  CT_NetOp_NewBlock_Fast_Propagation = $0012; // New V4 protocol: Allows PIP-0015 Fast block propagation
  CT_NetOp_GetBlockchainOperations   = $0013; // New V4 protocol: Allows PIP-0015 Fast block propagation
  CT_NetOp_AddOperations        = $0020;
  CT_NetOp_GetSafeBox           = $0021; // V2 Protocol: Allows to send/receive Safebox in chunk parts

  CT_NetOp_GetPendingOperations = $0030; // Obtain pending operations
  CT_NetOp_GetAccount           = $0031; // Obtain account info
  CT_NetOp_GetPubkeyAccounts    = $0032; // Obtain public key accounts

  CT_NetOp_Reserved_Start       = $1000; // This will provide a reserved area
  CT_NetOp_Reserved_End         = $1FFF; // End of reserved area
  CT_NetOp_ERRORCODE_NOT_IMPLEMENTED = $00FF;// This will be error code returned when using Reserved area and Op is not implemented


  CT_NetError_InvalidProtocolVersion = $0001;
  CT_NetError_IPBlackListed = $0002;
  CT_NetError_NotFound = $0003;
  CT_NetError_InvalidDataBufferInfo = $0010;
  CT_NetError_InternalServerError = $0011;
  CT_NetError_InvalidNewAccount = $0012;
  CT_NetError_SafeboxNotFound = $0020;
  CT_NetError_NotAvailable = $0021;

  CT_LAST_CONNECTION_BY_SERVER_MAX_MINUTES = 60*60*3;
  CT_LAST_CONNECTION_MAX_MINUTES = 60*60;
  CT_MAX_NODESERVERS_ON_HELLO = 10;
  CT_MIN_NODESERVERS_BUFFER = 50;
  CT_MAX_NODESERVERS_BUFFER = 300;

  CT_MAX_OPS_PER_BLOCKCHAINOPERATIONS = 10000;
  CT_MAX_SAFEBOXCHUNK_BLOCKS = 30000;

Type
  {
  Net Protocol:

  3 different types: Request,Response or Auto-send
  Request:   <Magic Net Identification (4b)><request  (2b)><operation (2b)><0x0000 (2b)><request_id(4b)><protocol info(4b)><data_length(4b)><request_data (data_length bytes)>
  Response:  <Magic Net Identification (4b)><response (2b)><operation (2b)><error_code (2b)><request_id(4b)><protocol info(4b)><data_length(4b)><response_data (data_length bytes)>
  Auto-send: <Magic Net Identification (4b)><autosend (2b)><operation (2b)><0x0000 (2b)><0x00000000 (4b)><protocol info(4b)><data_length(4b)><data (data_length bytes)>

  Min size: 4b+2b+2b+2b+4b+4b+4b = 22 bytes
  Max size: (depends on last 4 bytes) = 22..(2^32)-1
  }

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
    error_text : String;
  end;

  TNetConnection = Class;

  TNodeServerAddress = Record
    ip : String;
    port : Word;
    last_connection : Cardinal;
    last_connection_by_server : Cardinal;
    last_connection_by_me : Cardinal;
    //
    netConnection : TNetConnection;
    its_myself : Boolean;
    last_attempt_to_connect : TDateTime;
    total_failed_attemps_to_connect : Integer;
    is_blacklisted : Boolean; // Build 1.4.4
    BlackListText : String;
  end;
  TNodeServerAddressArray = Array of TNodeServerAddress;
  PNodeServerAddress = ^TNodeServerAddress;

  TNetData = Class;

  // This will maintain a list sorted by 2 values: ip/port and netConnection in thread safe mode
  // Using this object, NodeServerAddress can be more high in length and also more quick to search

  { TOrderedServerAddressListTS }

  TOrderedServerAddressListTS = Class
  private
    FAllowDeleteOnClean: Boolean;
    FNetData : TNetData;
    FCritical : TPCCriticalSection;
    FListByIp : TList<Pointer>;
    FListByNetConnection : TList<Pointer>;
    Procedure SecuredDeleteFromListByIp(index : Integer);
    Function SecuredFindByIp(const ip : String; port : Word; var Index: Integer): Boolean;
    Function SecuredFindByNetConnection(const search : TNetConnection; var Index: Integer): Boolean;
  protected
    function DeleteNetConnection(netConnection : TNetConnection) : Boolean;
  public
    Constructor Create(ANetData : TNetData);
    Destructor Destroy; Override;
    Procedure Clear;
    Function Count : Integer;
    Function CleanBlackList(forceCleanAll : Boolean) : Integer;
    procedure CleanNodeServersList;
    Function LockList : TList<Pointer>;
    Procedure UnlockList;
    function IsBlackListed(const ip: String): Boolean;
    function GetNodeServerAddress(const ip : String; port:Word; CanAdd : Boolean; var nodeServerAddress : TNodeServerAddress) : Boolean;
    procedure SetNodeServerAddress(const nodeServerAddress : TNodeServerAddress);
    Procedure UpdateNetConnection(netConnection : TNetConnection);
    procedure GetNodeServersToConnnect(maxNodes : Integer; useArray : Boolean; var nsa : TNodeServerAddressArray);
    Function GetValidNodeServers(OnlyWhereIConnected : Boolean; Max : Integer): TNodeServerAddressArray;
    property AllowDeleteOnClean : Boolean read FAllowDeleteOnClean write FAllowDeleteOnClean;
  End;


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

  TNetStatistics = Record
    ActiveConnections : Integer; // All connections wiht "connected" state
    ClientsConnections : Integer; // All clients connected to me like a server with "connected" state
    ServersConnections : Integer; // All servers where I'm connected
    ServersConnectionsWithResponse : Integer; // All servers where I'm connected and I've received data
    TotalConnections : Integer;
    TotalClientsConnections : Integer;
    TotalServersConnections : Integer;
    BytesReceived : Int64;
    BytesSend : Int64;
    NodeServersListCount : Integer;
    NodeServersDeleted : Integer;
  end;


  { TNetDataNotifyEventsThread ensures that notifications of TNetData object
    will be in main Thread calling a Synchronized method }
  TNetDataNotifyEventsThread = Class(TPCThread)
  private
    FNetData: TNetData;
    FNotifyOnReceivedHelloMessage : Boolean;
    FNotifyOnStatisticsChanged : Boolean;
    FNotifyOnNetConnectionsUpdated : Boolean;
    FNotifyOnNodeServersUpdated : Boolean;
    FNotifyOnBlackListUpdated : Boolean;
  protected
    procedure SynchronizedNotify;
    procedure BCExecute; override;
  public
    Constructor Create(ANetData : TNetData);
  End;

  TNetClientsDestroyThread = Class(TPCThread)
  private
    FNetData : TNetData;
    FTerminatedAllConnections : Boolean;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(NetData : TNetData);
    Procedure WaitForTerminatedAllConnections;
  End;

  TThreadCheckConnections = Class(TPCThread)
  private
    FNetData : TNetData;
    FLastCheckTS : TTickCount;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(NetData : TNetData);
  End;

  TNetworkAdjustedTime = Class
  private
    FTimesList : TPCThreadList<Pointer>;
    FTimeOffset : Integer;
    FTotalCounter : Integer;
    Function IndexOfClientIp(list : TList<Pointer>; const clientIp : String) : Integer;
    Procedure UpdateMedian(list : TList<Pointer>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateIp(const clientIp : String; clientTimestamp : Cardinal);
    procedure AddNewIp(const clientIp : String; clientTimestamp : Cardinal);
    procedure RemoveIp(const clientIp : String);
    function GetAdjustedTime : Cardinal;
    property TimeOffset : Integer read FTimeOffset;
    function GetMaxAllowedTimestampForNewBlock : Cardinal;
  end;

  TProcessReservedAreaMessage = procedure (netData : TNetData; senderConnection : TNetConnection; const HeaderData : TNetHeaderData; receivedData : TStream; responseData : TStream) of object;
  TGetNewBlockchainFromClientDownloadNewSafebox = procedure (netData : TNetData; clientConnection : TNetConnection; my_blocks_count, client_blocks_count : Integer; var download_new_safebox : Boolean) of object;

  TNetData = Class(TComponent)
  private
    FMaxNodeServersAddressesBuffer: Integer;
    FMaxServersConnected: Integer;
    FMinServersConnected: Integer;
    FNetDataNotifyEventsThread : TNetDataNotifyEventsThread;
    FNodePrivateKey : TECPrivateKey;
    FNetConnections : TPCThreadList<TNetConnection>;
    FNodeServersAddresses : TOrderedServerAddressListTS;
    FLastRequestId : Cardinal;
    FOnProcessReservedAreaMessage: TProcessReservedAreaMessage;
    FRegisteredRequests : TPCThreadList<Pointer>;
    FIsDiscoveringServers : Boolean;
    FLockGettingNewBlockChainFromClient : TPCCriticalSection;
    FNewBlockChainFromClientStatus : String;
    FOnConnectivityChanged : TNotifyManyEvent;
    FOnNetConnectionsUpdated: TNotifyEvent;
    FOnNodeServersUpdated: TNotifyEvent;
    FOnBlackListUpdated: TNotifyEvent;
    FThreadCheckConnections : TThreadCheckConnections;
    FOnReceivedHelloMessage: TNotifyEvent;
    FNetStatistics: TNetStatistics;
    FOnStatisticsChanged: TNotifyEvent;
    FMaxRemoteOperationBlock : TOperationBlock;
    FFixedServers : TNodeServerAddressArray;
    FNetClientsDestroyThread : TNetClientsDestroyThread;
    FNetConnectionsActive: Boolean;
    FMaxConnections : Integer;
    FNetworkAdjustedTime : TNetworkAdjustedTime;
    FIpInfos : TIpInfos;
    FMinFutureBlocksToDownloadNewSafebox: Integer;
    FOnGetNewBlockchainFromClientDownloadNewSafebox: TGetNewBlockchainFromClientDownloadNewSafebox;
    Procedure IncStatistics(incActiveConnections,incClientsConnections,incServersConnections,incServersConnectionsWithResponse : Integer; incBytesReceived, incBytesSend : Int64);
    procedure SetMaxNodeServersAddressesBuffer(AValue: Integer);
    procedure SetMaxServersConnected(AValue: Integer);
    procedure SetMinServersConnected(AValue: Integer);
    procedure SetNetConnectionsActive(const Value: Boolean);
    procedure SetMinFutureBlocksToDownloadNewSafebox(const Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure DiscoverServersTerminated(Sender : TObject);
    procedure DoProcessReservedAreaMessage(senderConnection : TNetConnection; const headerData : TNetHeaderData; receivedData : TStream; responseData : TStream); virtual;
  public
    Class function HeaderDataToText(const HeaderData : TNetHeaderData) : String;
    Class function ExtractHeaderInfo(buffer : TStream; var HeaderData : TNetHeaderData; DataBuffer : TStream; var IsValidHeaderButNeedMoreData : Boolean) : Boolean;
    Class Function OperationToText(operation : Word) : String;
    // Only 1 NetData
    Class Function NetData : TNetData;
    Class Function NetDataExists : Boolean;
    //
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function Bank : TPCBank;
    Function NewRequestId : Cardinal;
    Procedure RegisterRequest(Sender: TNetConnection; operation : Word; request_id : Cardinal);
    Function UnRegisterRequest(Sender: TNetConnection; operation : Word; request_id : Cardinal) : Boolean;
    Function PendingRequest(Sender : TNetConnection; var requests_data : String ) : Integer;
    Procedure AddServer(NodeServerAddress : TNodeServerAddress);
    //
    Procedure DiscoverFixedServersOnly(const FixedServers : TNodeServerAddressArray);
    //
    Function ConnectionsCountAll : Integer;
    Function ConnectionsCountServerClients : Integer;
    Function ConnectionsCountClients : Integer;
    Function GetConnection(index : Integer; var NetConnection : TNetConnection) : Boolean;
    Function ConnectionsCount(CountOnlyNetClients : Boolean) : Integer;
    Function Connection(index : Integer) : TNetConnection;
    Function ConnectionExistsAndActive(ObjectPointer : TObject) : Boolean;
    Function ConnectionExists(ObjectPointer : TObject) : Boolean;
    Function ConnectionLock(Sender : TObject; ObjectPointer : TObject; MaxWaitMiliseconds : Cardinal) : Boolean;
    Procedure ConnectionUnlock(ObjectPointer : TObject);
    Function FindConnectionByClientRandomValue(Sender : TNetConnection) : TNetConnection;
    Procedure DiscoverServers;
    Procedure DisconnectClients;
    procedure OnReadingNewSafeboxProgressNotify(sender : TObject; const mesage : String; curPos, totalCount : Int64);
    Procedure GetNewBlockChainFromClient(Connection : TNetConnection; const why : String);
    Property NodeServersAddresses : TOrderedServerAddressListTS read FNodeServersAddresses;
    Property NetConnections : TPCThreadList<TNetConnection> read FNetConnections;
    Property NetStatistics : TNetStatistics read FNetStatistics;
    Property IsDiscoveringServers : Boolean read FIsDiscoveringServers;
    function IsGettingNewBlockChainFromClient(var status : String) : Boolean;
    Property MaxRemoteOperationBlock : TOperationBlock read FMaxRemoteOperationBlock;
    Property NodePrivateKey : TECPrivateKey read FNodePrivateKey;
    property OnConnectivityChanged : TNotifyManyEvent read FOnConnectivityChanged;
    Property OnNetConnectionsUpdated : TNotifyEvent read FOnNetConnectionsUpdated write FOnNetConnectionsUpdated;
    Property OnNodeServersUpdated : TNotifyEvent read FOnNodeServersUpdated write FOnNodeServersUpdated;
    Property OnBlackListUpdated : TNotifyEvent read FOnBlackListUpdated write FOnBlackListUpdated;
    Property OnReceivedHelloMessage : TNotifyEvent read FOnReceivedHelloMessage write FOnReceivedHelloMessage;
    Property OnStatisticsChanged : TNotifyEvent read FOnStatisticsChanged write FOnStatisticsChanged;
    property OnGetNewBlockchainFromClientDownloadNewSafebox : TGetNewBlockchainFromClientDownloadNewSafebox read FOnGetNewBlockchainFromClientDownloadNewSafebox write FOnGetNewBlockchainFromClientDownloadNewSafebox;
    procedure NotifyConnectivityChanged;
    Procedure NotifyNetConnectionUpdated;
    Procedure NotifyNodeServersUpdated;
    Procedure NotifyBlackListUpdated;
    Procedure NotifyReceivedHelloMessage;
    Procedure NotifyStatisticsChanged;
    Property NetConnectionsActive : Boolean read FNetConnectionsActive write SetNetConnectionsActive;
    Property NetworkAdjustedTime : TNetworkAdjustedTime read FNetworkAdjustedTime;
    Property MaxNodeServersAddressesBuffer : Integer read FMaxNodeServersAddressesBuffer write SetMaxNodeServersAddressesBuffer;
    Property OnProcessReservedAreaMessage : TProcessReservedAreaMessage read FOnProcessReservedAreaMessage write FOnProcessReservedAreaMessage;
    Property MinServersConnected : Integer read FMinServersConnected write SetMinServersConnected;
    Property MaxServersConnected : Integer read FMaxServersConnected write SetMaxServersConnected;
    Property IpInfos : TIpInfos read FIpInfos;
    Property MinFutureBlocksToDownloadNewSafebox : Integer read FMinFutureBlocksToDownloadNewSafebox write SetMinFutureBlocksToDownloadNewSafebox;
  End;

  { TNetConnection }

  TNetConnection = Class(TComponent)
  private
    FIsConnecting: Boolean;
    FTcpIpClient : TNetTcpIpClient;
    FRemoteOperationBlock : TOperationBlock;
    FRemoteAccumulatedWork : UInt64;
    FLastHelloTS : TTickCount;
    FLastDataReceivedTS : TTickCount;
    FLastDataSendedTS : TTickCount;
    FClientBufferRead : TStream;
    FNetLock : TPCCriticalSection;
    FIsWaitingForResponse : Boolean;
    FTimestampDiff : Integer;
    FIsMyselfServer : Boolean;
    FClientPublicKey : TAccountKey;
    FCreatedTime: TDateTime;
    FClientAppVersion: String;
    FDoFinalizeConnection : Boolean;
    FNetProtocolVersion: TNetProtocolVersion;
    FAlertedForNewProtocolAvailable : Boolean;
    FHasReceivedData : Boolean;
    FIsDownloadingBlocks : Boolean;
    FRandomWaitSecondsSendHello : Cardinal;
    FBufferLock : TPCCriticalSection;
    FBufferReceivedOperationsHash : TOrderedRawList;
    FBufferToSendOperations : TOperationsHashTree;
    FClientTimestampIp : String;
    function GetConnected: Boolean;
    procedure SetConnected(const Value: Boolean);
    procedure TcpClient_OnConnect(Sender: TObject);
    procedure TcpClient_OnDisconnect(Sender: TObject);
    procedure DoProcessBuffer;
    Procedure DoProcess_Hello(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_Message(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetBlocks_Request(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetBlocks_Response(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetBlockchainOperations_Request(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetOperationsBlock_Request(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_NewBlock(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_AddOperations(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetSafeBox_Request(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetPendingOperations_Request(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetAccount_Request(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetPubkeyAccounts_Request(HeaderData : TNetHeaderData; DataBuffer: TStream);
    Procedure DoProcess_GetPendingOperations;
    Procedure SetClient(Const Value : TNetTcpIpClient);
    Function ReadTcpClientBuffer(MaxWaitMiliseconds : Cardinal; var HeaderData : TNetHeaderData; BufferData : TStream) : Boolean;
    Procedure DisconnectInvalidClient(ItsMyself : Boolean; Const why : String);
    function GetClient: TNetTcpIpClient;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure Send(NetTranferType : TNetTransferType; operation, errorcode : Word; request_id : Integer; DataBuffer : TStream);
    Procedure SendError(NetTranferType : TNetTransferType; operation, request_id : Integer; error_code : Integer; const error_text : String);
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function DoSendAndWaitForResponse(operation: Word; RequestId: Integer; SendDataBuffer, ReceiveDataBuffer: TStream; MaxWaitTime : Cardinal; var HeaderData : TNetHeaderData) : Boolean;
    Function ConnectTo(ServerIP: String; ServerPort:Word) : Boolean;
    Property Connected : Boolean read GetConnected write SetConnected;
    Property IsConnecting : Boolean read FIsConnecting;
    Function Send_Hello(NetTranferType : TNetTransferType; request_id : Integer) : Boolean;
    Function Send_NewBlockFound(Const NewBlock : TPCOperationsComp) : Boolean;
    Function Send_GetBlocks(StartAddress, quantity : Cardinal; var request_id : Cardinal) : Boolean;
    Function Send_AddOperations(Operations : TOperationsHashTree) : Boolean;
    Function Send_Message(Const TheMessage : String) : Boolean;
    Function AddOperationsToBufferForSend(Operations : TOperationsHashTree) : Integer;
    Property Client : TNetTcpIpClient read GetClient;
    Function ClientRemoteAddr : String;
    property TimestampDiff : Integer read FTimestampDiff;
    property RemoteOperationBlock : TOperationBlock read FRemoteOperationBlock;
    //
    Property NetProtocolVersion : TNetProtocolVersion read FNetProtocolVersion;
    //
    Property IsMyselfServer : Boolean read FIsMyselfServer;
    Property CreatedTime : TDateTime read FCreatedTime;
    Property ClientAppVersion : String read FClientAppVersion write FClientAppVersion;
    Procedure FinalizeConnection;
  End;

  TNetClient = Class;
  TNetClientThread = Class(TPCThread)
  private
    FNetClient : TNetClient;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(NetClient : TNetClient; AOnTerminateThread : TNotifyEvent);
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

  { TNetServer }

  TNetServer = Class(TNetTcpIpServer)
  private
  protected
    Procedure OnNewIncommingConnection(Sender : TObject; Client : TNetTcpIpClient); override;
    procedure SetActive(const Value: Boolean); override;
    procedure SetMaxConnections(AValue: Integer); override;
  public
    Constructor Create; override;
  End;

  TThreadDiscoverConnection = Class(TPCThread)
    FNodeServerAddress : TNodeServerAddress;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(NodeServerAddress: TNodeServerAddress; NotifyOnTerminate : TNotifyEvent);
  End;

  { TThreadGetNewBlockChainFromClient }

  TThreadGetNewBlockChainFromClient = Class(TPCThread)
  protected
    procedure BCExecute; override;
  public
    Constructor Create;
  End;


Const
  CT_TNodeServerAddress_NUL : TNodeServerAddress = (ip:'';port:0;last_connection:0;last_connection_by_server:0; last_connection_by_me:0; netConnection:nil;its_myself:false;last_attempt_to_connect:0;total_failed_attemps_to_connect:0;is_blacklisted:false;BlackListText:'');
  CT_TNetStatistics_NUL : TNetStatistics = (ActiveConnections:0;ClientsConnections:0;ServersConnections:0;ServersConnectionsWithResponse:0;TotalConnections:0;TotalClientsConnections:0;TotalServersConnections:0;BytesReceived:0;BytesSend:0;NodeServersListCount:0;NodeServersDeleted:0);

implementation

uses
  UConst, ULog, UNode, UTime, UPCEncryption, UChunk;

Const
  CT_NetTransferType : Array[TNetTransferType] of String = ('Unknown','Request','Response','Autosend');
  CT_NetHeaderData : TNetHeaderData = (header_type:ntp_unknown;protocol:(protocol_version:0;protocol_available:0);operation:0;request_id:0;buffer_data_length:0;is_error:false;error_code:0;error_text:'');


{ TOrderedServerAddressListTS }

function TOrderedServerAddressListTS.CleanBlackList(forceCleanAll : Boolean) : Integer;
Var P : PNodeServerAddress;
  i : Integer;
begin
  CleanNodeServersList;
  // This procedure cleans old blacklisted IPs
  Result := 0;
  FCritical.Acquire;
  Try
    for i := FListByIp.Count - 1 downto 0 do begin
      P := FListByIp[i];
      // Is an old blacklisted IP? (More than 1 hour)
      If (P^.is_blacklisted) AND
        ((forceCleanAll) OR ((P^.last_connection+(CT_LAST_CONNECTION_MAX_MINUTES)) < (UnivDateTimeToUnix(DateTime2UnivDateTime(now))))) then begin
        if (AllowDeleteOnClean) then begin
          SecuredDeleteFromListByIp(i);
        end else begin
          P^.is_blacklisted:=False;
        end;
        inc(Result);
      end;
    end;
  Finally
    FCritical.Release;
  End;
  if (Result>0) then FNetData.NotifyBlackListUpdated;
end;

procedure TOrderedServerAddressListTS.CleanNodeServersList;
var i : Integer;
  nsa : TNodeServerAddress;
  currunixtimestamp : Cardinal;
begin
  If Not (FAllowDeleteOnClean) then Exit;
  currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
  FCritical.Acquire;
  Try
    i := FListByIp.Count-1;
    while (i>=0) do begin
      nsa := PNodeServerAddress( FListByIp[i] )^;
      If (Not (nsa.is_blacklisted)) // Not blacklisted
        And ((nsa.netConnection = Nil)  // No connection
             OR  // Connected but a lot of time without data...
             ((Assigned(nsa.netConnection)) AND ((nsa.last_connection + (CT_LAST_CONNECTION_MAX_MINUTES)) < currunixtimestamp )))
        And (
          (nsa.total_failed_attemps_to_connect>0)
          OR
          (
           // I've not connected CT_LAST_CONNECTION_MAX_MINUTES minutes before
           ((nsa.last_connection + (CT_LAST_CONNECTION_MAX_MINUTES)) < (currunixtimestamp))
           And // Others have connected CT_LAST_CONNECTION_BY_SERVER_MAX_MINUTES minutes before
           ((nsa.last_connection_by_server + (CT_LAST_CONNECTION_BY_SERVER_MAX_MINUTES)) < (currunixtimestamp))
           And
           ((nsa.last_connection>0) Or (nsa.last_connection_by_server>0))
          ))
        And (
          (nsa.last_connection_by_me=0)
          Or
          ((nsa.last_connection_by_me + 86400) < (currunixtimestamp))  // Not connected in 24 hours
          )
      then begin
        TLog.NewLog(ltdebug,ClassName,Format('Delete node server address: %s : %d last_connection:%d last_connection_by_server:%d total_failed_attemps:%d last_attempt_to_connect:%s ',
          [nsa.ip,nsa.port,nsa.last_connection,nsa.last_connection_by_server,nsa.total_failed_attemps_to_connect,FormatDateTime('dd/mm/yyyy hh:nn:ss',nsa.last_attempt_to_connect)]));
        SecuredDeleteFromListByIp(i);
      end;
      dec(i);
    end;
  finally
    FCritical.Release;
  end;
end;

procedure TOrderedServerAddressListTS.Clear;
Var P : PNodeServerAddress;
  i : Integer;
begin
  FCritical.Acquire;
  Try
    for i := 0 to FListByIp.Count - 1 do begin
      P := FListByIp[i];
      Dispose(P);
    end;
    inc(FNetData.FNetStatistics.NodeServersDeleted,FListByIp.count);
    FListByIp.Clear;
    FListByNetConnection.Clear;
    FNetData.FNetStatistics.NodeServersListCount := 0;
  finally
    FCritical.Release;
  end;
end;

function TOrderedServerAddressListTS.Count: Integer;
begin
  FCritical.Acquire;
  try
    Result := FListByIp.Count;
  finally
    FCritical.Release;
  end;
end;

constructor TOrderedServerAddressListTS.Create(ANetData : TNetData);
begin
  FNetData := ANetData;
  FCritical := TPCCriticalSection.Create(Classname);
  FListByIp := TList<Pointer>.Create;
  FListByNetConnection := TList<Pointer>.Create;
  FAllowDeleteOnClean := True;
end;

function TOrderedServerAddressListTS.DeleteNetConnection(netConnection: TNetConnection) : Boolean;
Var i : Integer;
begin
  FCritical.Acquire;
  Try
    If SecuredFindByNetConnection(netConnection,i) then begin
      PNodeServerAddress( FListByNetConnection[i] )^.netConnection := Nil;
      FListByNetConnection.Delete(i);
      Result := True;
    end else Result := False;
  Finally
    FCritical.Release;
  end;
end;

destructor TOrderedServerAddressListTS.Destroy;
begin
  Clear;
  FreeAndNil(FCritical);
  FreeAndNil(FListByIp);
  FreeAndNil(FListByNetConnection);
  inherited Destroy;
end;

function TOrderedServerAddressListTS.GetNodeServerAddress(const ip: String; port: Word; CanAdd: Boolean; var nodeServerAddress: TNodeServerAddress): Boolean;
Var i : Integer;
  P : PNodeServerAddress;
begin
  FCritical.Acquire;
  Try
    if SecuredFindByIp(ip,port,i) then begin
      P := FListByIp.Items[i];
      nodeServerAddress := P^;
      Result := True;
    end else if CanAdd then begin
      New(P);
      P^ := CT_TNodeServerAddress_NUL;
      P^.ip := ip;
      P^.port := port;
      FListByIp.Insert(i,P);
      nodeServerAddress := P^;
      Result := True
    end else begin
      nodeServerAddress := CT_TNodeServerAddress_NUL;
      Result := False;
    end;
  Finally
    FCritical.Release;
  End;
end;

procedure TOrderedServerAddressListTS.GetNodeServersToConnnect(maxNodes: Integer; useArray : Boolean; var nsa: TNodeServerAddressArray);
  Procedure sw(l : TList<Pointer>);
  Var i,j,x,y : Integer;
  begin
    if l.Count<=1 then exit;
    j := Random(l.Count)*3;
    for i := 0 to j do begin
      x := Random(l.Count);
      y := Random(l.Count);
      if x<>y then l.Exchange(x,y);
    end;
  end;
  Function IsValid(Const ns : TNodeServerAddress) : Boolean;
  Begin
    Result := (Not Assigned(ns.netConnection)) AND (Not IsBlackListed(ns.ip)) AND (Not ns.its_myself) And
          ((ns.last_attempt_to_connect=0) Or ((ns.last_attempt_to_connect+EncodeTime(0,3,0,0)<now))) And
          ((ns.total_failed_attemps_to_connect<3) Or (ns.last_attempt_to_connect+EncodeTime(0,10,0,0)<now));
  End;
Var i,j, iStart : Integer;
  P : PNodeServerAddress;
  l : TList<Pointer>;
  ns : TNodeServerAddress;
begin
  FCritical.Acquire;
  Try
    l := TList<Pointer>.Create;
    Try
      if useArray then begin
        for i := 0 to High(nsa) do begin
          If GetNodeServerAddress(nsa[i].ip,nsa[i].port,true,ns) then begin
            if IsValid(ns) then begin
              new(P);
              P^ := ns;
              l.Add(P);
            end;
          end;
        end;
        SetLength(nsa,0);
      end else begin
        SetLength(nsa,0);
        if FListByIp.Count>0 then begin
          iStart := Random(FListByIp.Count);
          i := iStart;
          j := FListByIp.Count;
          while (l.Count<maxNodes) And (i<j) do begin
            P := FListByIp[i];
            If (Not Assigned(P.netConnection)) AND (Not IsBlackListed(P^.ip)) AND (Not P^.its_myself) And
              ((P^.last_attempt_to_connect=0) Or ((P^.last_attempt_to_connect+EncodeTime(0,3,0,0)<now))) And
              ((P^.total_failed_attemps_to_connect<3) Or (P^.last_attempt_to_connect+EncodeTime(0,10,0,0)<now)) then begin
              l.Add(P);
            end;
            // Second round
            inc(i);
            if (i>=j) and (iStart>0) then begin
              j := iStart;
              iStart := 0;
              i := 0;
            end;
          end;
        end;
      end;
      if (l.Count>0) then begin
        sw(l);
        if l.Count<maxNodes then setLength(nsa,l.Count)
        else setLength(nsa,maxNodes);
        for i := 0 to high(nsa) do begin
          nsa[i] := PNodeServerAddress(l[i])^;
        end;
      end;
    Finally
      if useArray then begin
        for i := 0 to l.Count - 1 do begin
          P := l[i];
          Dispose(P);
        end;
      end;
      l.Free;
    End;
  Finally
    FCritical.Release;
  end;
end;

function TOrderedServerAddressListTS.GetValidNodeServers(OnlyWhereIConnected: Boolean; Max: Integer): TNodeServerAddressArray;
var i,j,iStart : Integer;
  nsa : TNodeServerAddress;
  currunixtimestamp : Cardinal;
begin
  SetLength(Result,0);
  currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
  CleanNodeServersList;
  // Save other node servers
  FCritical.Acquire;
  try
    If Max>0 then iStart := Random(FListByIp.Count)
    else iStart := 0;
    i := iStart;
    j := FListByIp.Count;
    while ((length(Result)<Max) Or (Max<=0)) And (i<j) do begin
      nsa := PNodeServerAddress( FListByIp[i] )^;
      if (Not IsBlackListed(nsa.ip))
        And
        ( // I've connected 1h before
         ((nsa.last_connection>0) And ((Assigned(nsa.netConnection)) Or ((nsa.last_connection + (CT_LAST_CONNECTION_MAX_MINUTES)) > (currunixtimestamp))))
         Or // Others have connected 3h before
         ((nsa.last_connection_by_server>0) And ((nsa.last_connection_by_server + (CT_LAST_CONNECTION_BY_SERVER_MAX_MINUTES)) > (currunixtimestamp)))
         Or // Peer cache
         ((nsa.last_connection=0) And (nsa.last_connection_by_server=0))
        )
        And
        ( // Never tried to connect or successfully connected
          (nsa.total_failed_attemps_to_connect=0)
        )
        And
        ( (Not nsa.its_myself) Or (nsa.port=CT_NetServer_Port) )
        And
        (
          (Not OnlyWhereIConnected)
          Or
          (nsa.last_connection>0)
        )
      then begin
        SetLength(Result,length(Result)+1);
        Result[high(Result)] := nsa;
      end;
      // Second round
      inc(i);
      if (i>=j) and (iStart>0) then begin
        j := iStart;
        iStart := 0;
        i := 0;
      end;
    end;
  finally
    FCritical.Release;
  end;
end;

function TOrderedServerAddressListTS.IsBlackListed(const ip: String): Boolean;
Var i : Integer;
  P : PNodeServerAddress;
begin
  Result := false;
  FCritical.Acquire;
  Try
    SecuredFindByIp(ip,0,i);
    // Position will be the first by IP:
    while (i<FListByIp.Count) And (Not Result) do begin
      P := PNodeServerAddress(FListByIp[i]);
      if Not SameStr(P^.ip,ip) then exit;
      if P^.is_blacklisted then begin
        Result := Not P^.its_myself;
      end;
      inc(i);
    end;
  Finally
    FCritical.Release;
  End;
end;

function TOrderedServerAddressListTS.LockList: TList<Pointer>;
begin
  FCritical.Acquire;
  Result := FListByIp;
end;

procedure TOrderedServerAddressListTS.SecuredDeleteFromListByIp(index: Integer);
Var P : PNodeServerAddress;
  i2 : Integer;
begin
  P := FListByIp.Items[index];
  if (Assigned(P^.netConnection)) then begin
    If SecuredFindByNetConnection(P^.netConnection,i2) then begin
      FListByNetConnection.Delete(i2);
    end else TLog.NewLog(ltError,ClassName,'DEV ERROR 20180201-1 NetConnection not found!');
  end;
  Dispose(P);
  FListByIp.Delete(index);
  dec(FNetData.FNetStatistics.NodeServersListCount);
  inc(FNetData.FNetStatistics.NodeServersDeleted);
end;

function TOrderedServerAddressListTS.SecuredFindByIp(const ip: String; port: Word; var Index: Integer): Boolean;
var L, H, I, C: Integer;
  PN : PNodeServerAddress;
begin
  Result := False;
  L := 0;
  H := FListByIp.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    PN := FListByIp.Items[I];
    C := CompareStr( PN.ip, ip );
    If (C=0) then begin
      C := PN.port-port;
    end;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TOrderedServerAddressListTS.SecuredFindByNetConnection(const search: TNetConnection; var Index: Integer): Boolean;
var L, H, I: Integer;
  PN : PNodeServerAddress;
  C : PtrInt;
begin
  Result := False;
  L := 0;
  H := FListByNetConnection.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    PN := FListByNetConnection.Items[I];
    C := PtrInt(PN.netConnection) - PtrInt(search);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

procedure TOrderedServerAddressListTS.SetNodeServerAddress(
  const nodeServerAddress: TNodeServerAddress);
Var i : Integer;
  P : PNodeServerAddress;
begin
  FCritical.Acquire;
  Try
    if SecuredFindByIp(nodeServerAddress.ip,nodeServerAddress.port,i) then begin
      P := FListByIp.Items[i];
      if (P^.netConnection<>nodeServerAddress.netConnection) then begin
        // Updated netConnection
        if Assigned(P^.netConnection) then begin
          // Delete old value
          if Not DeleteNetConnection(P^.netConnection) then TLog.NewLog(lterror,Classname,'DEV ERROR 20180205-1');
        end;
      end;
      P^ := nodeServerAddress;
    end else begin
      New(P);
      P^ := nodeServerAddress;
      FListByIp.Insert(i,P);
      Inc(FNetData.FNetStatistics.NodeServersListCount);
      TLog.NewLog(ltdebug,Classname,'Adding new server: '+NodeServerAddress.ip+':'+Inttostr(NodeServerAddress.port));
    end;
    if Assigned(nodeServerAddress.netConnection) then begin
      If Not SecuredFindByNetConnection(nodeServerAddress.netConnection,i) then begin
        FListByNetConnection.Insert(i,P);
      end;
    end;
  Finally
    FCritical.Release;
  end;
end;

procedure TOrderedServerAddressListTS.UnlockList;
begin
  FCritical.Release;
end;

procedure TOrderedServerAddressListTS.UpdateNetConnection(netConnection: TNetConnection);
Var i : Integer;
begin
  FCritical.Acquire;
  Try
    If SecuredFindByNetConnection(netConnection,i) then begin
      PNodeServerAddress(FListByNetConnection[i])^.last_connection := (UnivDateTimeToUnix(DateTime2UnivDateTime(now)));
      PNodeServerAddress(FListByNetConnection[i])^.total_failed_attemps_to_connect := 0;
    end;
  Finally
    FCritical.Release;
  End;
end;

{ TNetData }

Var _NetData : TNetData = nil;

Type PNetRequestRegistered = ^TNetRequestRegistered;

procedure TNetData.AddServer(NodeServerAddress: TNodeServerAddress);
Var P : PNodeServerAddress;
  i : Integer;
  currunixtimestamp : Cardinal;
  nsa : TNodeServerAddress;
begin
  if (trim(NodeServerAddress.ip)='')
     or (SameText(NodeServerAddress.ip,'localhost'))
     or (SameText('127.',NodeServerAddress.ip.Substring(0,4))) then Exit;

  if (NodeServerAddress.port<=0) then NodeServerAddress.port := CT_NetServer_Port
  else if (NodeServerAddress.port<>CT_NetServer_Port) then exit;

  // Protection against fill with invalid nodes
  currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
  // If not connected CT_LAST_CONNECTION_MAX_MINUTES minutes ago...
  If (NodeServerAddress.last_connection_by_server=0) AND (NodeServerAddress.last_connection>0) AND ((NodeServerAddress.last_connection + (CT_LAST_CONNECTION_MAX_MINUTES)) < (currunixtimestamp)) then exit;
  // If not connected CT_LAST_CONNECTION_BY_SERVER_MAX_MINUTES minutes ago...
  If (NodeServerAddress.last_connection=0) AND (NodeServerAddress.last_connection_by_server>0) AND ((NodeServerAddress.last_connection_by_server + (CT_LAST_CONNECTION_BY_SERVER_MAX_MINUTES)) < (currunixtimestamp)) then exit;
  If (NodeServerAddress.last_connection_by_server>currunixtimestamp) OR (NodeServerAddress.last_connection>currunixtimestamp) then exit;
  FNodeServersAddresses.GetNodeServerAddress(NodeServerAddress.ip,NodeServerAddress.port,True,nsa);
  if NodeServerAddress.last_connection>nsa.last_connection then nsa.last_connection := NodeServerAddress.last_connection;
  if NodeServerAddress.last_connection_by_server>nsa.last_connection_by_server then nsa.last_connection_by_server := NodeServerAddress.last_connection_by_server;
  if NodeServerAddress.last_attempt_to_connect>nsa.last_attempt_to_connect then nsa.last_attempt_to_connect := NodeServerAddress.last_attempt_to_connect;
  FNodeServersAddresses.SetNodeServerAddress(nsa);

  NotifyNodeServersUpdated;
end;

function TNetData.Bank: TPCBank;
begin
  Result := TNode.Node.Bank;
end;

function TNetData.Connection(index: Integer): TNetConnection;
Var l : TList<TNetConnection>;
begin
  l := FNetConnections.LockList;
  try
    Result := ( l[index] );
  finally
    FNetConnections.UnlockList;
  end;
end;

function TNetData.ConnectionExists(ObjectPointer: TObject): Boolean;
var i : Integer;
  l : TList<TNetConnection>;
begin
  Result := false;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      if TObject(l[i])=ObjectPointer then begin
        Result := true;
        exit;
      end;
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TNetData.ConnectionExistsAndActive(ObjectPointer: TObject): Boolean;
var i : Integer;
  l : TList<TNetConnection>;
begin
  Result := false;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      if TObject(l[i])=ObjectPointer then begin
        Result := (TNetConnection(ObjectPointer).Connected);
        exit;
      end;
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TNetData.ConnectionLock(Sender : TObject; ObjectPointer: TObject; MaxWaitMiliseconds : Cardinal) : Boolean;
var i : Integer;
  l : TList<TNetConnection>;
  nc : TNetConnection;
  tc : TTickCount;
begin
  Result := False; nc := Nil;
  tc := TPlatform.GetTickCount;
  if MaxWaitMiliseconds>60000 then MaxWaitMiliseconds := 60000;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      if (TObject(l[i])=ObjectPointer) then begin
        if (Not (TNetConnection(l[i]).FDoFinalizeConnection)) And (TNetConnection(l[i]).Connected) then begin
          nc := TNetConnection(l[i]);
          Break;
        end else Exit;
      end;
    end;
  finally
    FNetConnections.UnlockList;
  end;
  if Assigned(nc) then begin
    repeat
      if (nc.Connected) and Assigned(nc.FNetLock) then begin
        If nc.FNetLock.TryEnter then Result := True
        else Sleep(1);
      end else Exit;
    until (Result) Or (TPlatform.GetElapsedMilliseconds(tc)>MaxWaitMiliseconds);
  end;
end;

function TNetData.ConnectionsCount(CountOnlyNetClients : Boolean): Integer;
var i : Integer;
  l : TList<TNetConnection>;
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

function TNetData.ConnectionsCountAll: Integer;
Var l : TList<TNetConnection>;
begin
  l := FNetConnections.LockList;
  try
    Result := l.Count;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TNetData.ConnectionsCountClients: Integer;
Var l : TList<TNetConnection>; i : Integer;
begin
  Result := 0;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      if TObject(l[i]) is TNetClient then inc(Result);
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

function TNetData.ConnectionsCountServerClients: Integer;
Var l : TList<TNetConnection>; i : Integer;
begin
  Result := 0;
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      if TObject(l[i]) is TNetServerClient then inc(Result);
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

procedure TNetData.ConnectionUnlock(ObjectPointer: TObject);
var i : Integer;
  l : TList<TNetConnection>;
  nc : TNetConnection;
begin
  l := FNetConnections.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      if TObject(l[i])=ObjectPointer then begin
        TNetConnection(l[i]).FNetLock.Release;
        exit;
      end;
    end;
  finally
    FNetConnections.UnlockList;
  end;
  Try
    nc := (ObjectPointer as TNetConnection);
    if (not assigned(nc.FNetLock)) then raise Exception.Create('NetLock object not assigned');
    nc.FNetLock.Release;
  Except
    on E:Exception do begin
      TLog.NewLog(ltError,Classname,'Error unlocking Object '+IntToHex(PtrInt(ObjectPointer),8)+' Errors ('+E.ClassName+'): '+E.Message);
    end;
  End;
  TLog.NewLog(ltDebug,ClassName,'Unlocked a NetLock object out of connections list');
end;

constructor TNetData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnProcessReservedAreaMessage:=Nil;
  TLog.NewLog(ltInfo,ClassName,'TNetData.Create');
  FMaxConnections := CT_MaxClientsConnected;
  FNetConnectionsActive := true;
  SetLength(FFixedServers,0);
  FMaxRemoteOperationBlock := CT_OperationBlock_NUL;
  FNetStatistics := CT_TNetStatistics_NUL;
  FOnStatisticsChanged := Nil;
  FOnNetConnectionsUpdated := Nil;
  FOnNodeServersUpdated := Nil;
  FOnBlackListUpdated := Nil;
  FOnReceivedHelloMessage := Nil;
  FOnGetNewBlockchainFromClientDownloadNewSafebox := Nil;
  FIsDiscoveringServers := false;
  FRegisteredRequests := TPCThreadList<Pointer>.Create('TNetData_RegisteredRequests');
  FNodeServersAddresses := TOrderedServerAddressListTS.Create(Self);
  FLastRequestId := 0;
  FNetConnections := TPCThreadList<TNetConnection>.Create('TNetData_NetConnections');
  FLockGettingNewBlockChainFromClient := TPCCriticalSection.Create('LockGettingNewBlockChainFromClient');
  FNewBlockChainFromClientStatus := '';
  FNodePrivateKey := TECPrivateKey.Create;
  FNodePrivateKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FThreadCheckConnections := TThreadCheckConnections.Create(Self);
  FNetDataNotifyEventsThread := TNetDataNotifyEventsThread.Create(Self);
  FNetClientsDestroyThread := TNetClientsDestroyThread.Create(Self);
  FNetworkAdjustedTime := TNetworkAdjustedTime.Create;
  FMaxNodeServersAddressesBuffer:=(CT_MAX_NODESERVERS_BUFFER DIV 2);
  FMinServersConnected:=CT_MinServersConnected;
  FMaxServersConnected:=CT_MaxServersConnected;
  FIpInfos := TIpInfos.Create;
  FIpInfos.MaxStatsLifetime := 60*60*4; // Max 4 hours
  FIpInfos.MaxStatsCount := 100; // Max lasts 100 values
  // By default, if our node is 7 days back vs highest blockchain detected, will not
  // download blocks, instead will download directly new safebox state
  MinFutureBlocksToDownloadNewSafebox := (86400 DIV CT_NewLineSecondsAvg) * {$IFDEF PRODUCTION}7{$ELSE}1{$ENDIF}; // Only 1 day for TESTNET, 7 for PRODUCTION
  //
  If Not Assigned(_NetData) then _NetData := Self;
end;

destructor TNetData.Destroy;
Var l : TList<TNetConnection>;
  i : Integer;
  tdc : TThreadDiscoverConnection;
begin
  TLog.NewLog(ltInfo,ClassName,'TNetData.Destroy START');
  FreeAndNil(FOnConnectivityChanged);
  FOnGetNewBlockchainFromClientDownloadNewSafebox := Nil;
  FOnStatisticsChanged := Nil;
  FOnNetConnectionsUpdated := Nil;
  FOnNodeServersUpdated := Nil;
  FOnBlackListUpdated := Nil;
  FOnReceivedHelloMessage := Nil;

  // First destroy ThreadCheckConnections to prevent a call to "DiscoverServers"
  TLog.NewLog(ltInfo,ClassName,'ThreadCheckConnections terminating...');
  FThreadCheckConnections.Terminate;
  FThreadCheckConnections.WaitFor;
  FreeAndNil(FThreadCheckConnections);

  // Now finish all DiscoverConnection threads
  Repeat
    tdc := TThreadDiscoverConnection( TPCThreadClass.GetThreadByClass(TThreadDiscoverConnection,nil) );
    if Assigned(tdc) then begin
      tdc.FreeOnTerminate := false;
      tdc.Terminate;
      tdc.WaitFor;
      tdc.Free;
      TLog.NewLog(ltInfo,ClassName,'TThreadDiscoverConnection finished');
    end;
  Until Not Assigned(tdc);

  // Closing connections
  l := FNetConnections.LockList;
  Try
    for i := 0 to l.Count - 1 do begin
      TNetConnection(l[i]).Connected := false;
      TNetConnection(l[i]).FinalizeConnection;
    end;
  Finally
    FNetConnections.UnlockList;
  End;


  FNetClientsDestroyThread.WaitForTerminatedAllConnections;
  FNetClientsDestroyThread.Terminate;
  FNetClientsDestroyThread.WaitFor;
  FreeAndNil(FNetClientsDestroyThread);

  FreeAndNil(FNodeServersAddresses);
  FreeAndNil(FNetConnections);
  FreeAndNil(FNodePrivateKey);
  FNetDataNotifyEventsThread.Terminate;
  FNetDataNotifyEventsThread.WaitFor;
  FreeAndNil(FNetDataNotifyEventsThread);
  SetLength(FFixedServers,0);
  FreeAndNil(FRegisteredRequests);
  FreeAndNil(FNetworkAdjustedTime);
  FreeAndNil(FIpInfos);
  FreeAndNil(FLockGettingNewBlockChainFromClient);
  inherited;
  if (_NetData=Self) then _NetData := Nil;
  TLog.NewLog(ltInfo,ClassName,'TNetData.Destroy END');
end;

procedure TNetData.DisconnectClients;
var i : Integer;
  l : TList<TNetConnection>;
begin
  l := FNetConnections.LockList;
  Try
    for i := l.Count - 1 downto 0 do begin
      if (l[i] is TNetClient) then begin
        TNetClient(l[i]).Connected := false;
        TNetClient(l[i]).FinalizeConnection;
      end;
    end;
  Finally
    FNetConnections.UnlockList;
  End;
end;

procedure TNetData.DiscoverFixedServersOnly(const FixedServers: TNodeServerAddressArray);
Var i : Integer;
begin
  SetLength(FFixedServers,length(FixedServers));
  for i := low(FixedServers) to high(FixedServers) do begin
    FFixedServers[i] := FixedServers[i];
  end;
  for i := low(FixedServers) to high(FixedServers) do begin
    AddServer(FixedServers[i]);
  end;
end;

procedure TNetData.DiscoverServers;
Var P : PNodeServerAddress;
  i,j,k : Integer;
  tdc : TThreadDiscoverConnection;
  canAdd : Boolean;
  nsa : TNodeServerAddressArray;
begin
  if Not FNetConnectionsActive then exit;
  if TPCThread.ThreadClassFound(TThreadDiscoverConnection,nil)>=0 then begin
    {$IFDEF HIGHLOG}TLog.NewLog(ltInfo,ClassName,'Already discovering servers...');{$ENDIF}
    exit;
  end;
  FNodeServersAddresses.CleanBlackList(False);
  If NetStatistics.ClientsConnections>0 then begin
    j := FMinServersConnected - NetStatistics.ServersConnectionsWithResponse;
  end else begin
    j := FMaxServersConnected - NetStatistics.ServersConnectionsWithResponse;
  end;
  if j<=0 then exit;
  {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,Classname,'Discover servers start process searching up to '+inttostr(j)+' servers');{$ENDIF}
  if (Length(FFixedServers)>0) then begin
    nsa := FFixedServers;
    FNodeServersAddresses.GetNodeServersToConnnect(j,true,nsa);
  end else begin
    SetLength(nsa,0);
    FNodeServersAddresses.GetNodeServersToConnnect(j,false,nsa);
  end;
  if length(nsa)>0 then begin
    TLog.NewLog(ltDebug,Classname,'Start discovering up to '+inttostr(length(nsa))+' servers... (max:'+inttostr(j)+')');
    //
    for i := 0 to high(nsa) do begin
      FIsDiscoveringServers := true;
      tdc := TThreadDiscoverConnection.Create(nsa[i],DiscoverServersTerminated);
    end;
  end;
end;

procedure TNetData.DiscoverServersTerminated(Sender: TObject);
begin
  NotifyNodeServersUpdated;
  if TPCThread.ThreadClassFound(TThreadDiscoverConnection,Nil)>=0 then exit;
  FIsDiscoveringServers := false;
  // If here, discover servers finished, so we can try to get/receive data
  {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,Classname,Format('Discovering servers finished. Now we have %d active connections and %d connections to other servers',
    [ConnectionsCount(false),ConnectionsCount(true)]));{$ENDIF}
  if TPCThread.ThreadClassFound(TThreadGetNewBlockChainFromClient,nil)>=0 then exit;
  TThreadGetNewBlockChainFromClient.Create;
end;

procedure TNetData.DoProcessReservedAreaMessage(senderConnection : TNetConnection; const headerData: TNetHeaderData; receivedData: TStream; responseData: TStream);
begin
  If Assigned(FOnProcessReservedAreaMessage) then begin
    FOnProcessReservedAreaMessage(Self,senderConnection,headerData,receivedData,responseData);
  end;
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
    HeaderData.buffer_data_length := c;
    DataBuffer.Size := 0;
    if (c>0) then begin
      if buffer.Size - buffer.Position < c then begin
        IsValidHeaderButNeedMoreData := true;
        {$IFDEF HIGHLOG}
        TLog.NewLog(ltdebug,className,Format('Need more data! Buffer size (%d) - position (%d) < %d - Header info: %s',
          [buffer.Size,buffer.Position,c,HeaderDataToText(HeaderData)]));
        {$ENDIF}
        exit;
      end;
      DataBuffer.CopyFrom(buffer,c);
      DataBuffer.Position := 0;
    end;
    //
    if HeaderData.header_type=ntp_response then begin
      HeaderData.is_error := HeaderData.error_code<>0;
      if HeaderData.is_error then begin
        TStreamOp.ReadString(DataBuffer,HeaderData.error_text);
      end;
    end else begin
      HeaderData.is_error := HeaderData.error_code<>0;
      if HeaderData.is_error then begin
        TStreamOp.ReadString(DataBuffer,HeaderData.error_text);
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
Var l : TList<TNetConnection>;
  i : Integer;
begin
  l := FNetConnections.LockList;
  try
    for i := 0 to L.Count - 1 do begin
      Result := TNetConnection( l[i] );
      If TAccountComp.EqualAccountKeys(Result.FClientPublicKey,Sender.FClientPublicKey) And (Sender<>Result) then exit;
    end;
  finally
    FNetConnections.UnlockList;
  end;
  Result := Nil;
end;

function TNetData.GetConnection(index: Integer; var NetConnection : TNetConnection) : Boolean;
Var l : TList<TNetConnection>;
begin
  Result := false; NetConnection := Nil;
  l := FNetConnections.LockList;
  try
    if (index>=0) And (index<l.Count) then begin
      NetConnection := TNetConnection( l[index] );
      Result := true;
      exit;
    end;
  finally
    FNetConnections.UnlockList;
  end;
end;

procedure TNetData.GetNewBlockChainFromClient(Connection: TNetConnection;
  const why: String);
Const CT_LogSender = 'GetNewBlockChainFromClient';

  function Do_GetOperationsBlock(AssignToBank : TPCBank; block_start,block_end, MaxWaitMilliseconds : Cardinal; OnlyOperationBlock : Boolean; BlocksList : TList<TPCOperationsComp>) : Boolean;
  Var SendData,ReceiveData : TMemoryStream;
    headerdata : TNetHeaderData;
    op : TPCOperationsComp;
    request_id,opcount,i, last_n_block : Cardinal;
    errors : String;
    noperation : Integer;
  begin
    Result := false;
    BlocksList.Clear;
    // First receive operations from
    SendData := TMemoryStream.Create;
    ReceiveData := TMemoryStream.Create;
    try
      if OnlyOperationBlock then begin
        noperation := CT_NetOp_GetBlockHeaders;
      end else begin
        noperation := CT_NetOp_GetBlocks;
      end;
      TLog.NewLog(ltdebug,CT_LogSender,Format('Sending %s from block %d to %d (Total: %d)',
        [TNetData.OperationToText(noperation),block_start,block_end,block_end-block_start+1]));
      SendData.Write(block_start,4);
      SendData.Write(block_end,4);
      request_id := TNetData.NetData.NewRequestId;
      if Connection.DoSendAndWaitForResponse(noperation,request_id,SendData,ReceiveData,MaxWaitMilliseconds,headerdata) then begin
        if HeaderData.is_error then exit;
        if ReceiveData.Read(opcount,4)<4 then exit; // Error in data
        i := 0; last_n_block := 0;
        while (i<opcount) do begin
          // decode data
          op := TPCOperationsComp.Create(AssignToBank);
          If op.LoadBlockFromStream(ReceiveData,errors) then begin
            // Build 2.1.7 Protection for invalid block number
            If ((i>0) And (last_n_block>=op.OperationBlock.block)) Or
               ((Not OnlyOperationBlock) And
                 ( ((i=0) And (op.OperationBlock.block<>block_start))
                   Or
                   ((i>0) And (op.OperationBlock.block<>last_n_block+1)) ) ) then begin
              Connection.DisconnectInvalidClient(false,Format('Invalid block sequence received last:%d received:%d',[last_n_block,op.OperationBlock.block]));
              op.free;
              break;
            end else BlocksList.Add(op);
            last_n_block := op.OperationBlock.block;
          end else begin
            Connection.DisconnectInvalidClient(false,Format('Error reading OperationBlock from received stream %d/%d: %s',[i+1,opcount,errors]));
            op.free;
            break;
          end;
          inc(i);
        end;
        Result := true;
      end else begin
        TLog.NewLog(lterror,CT_LogSender,Format('No received response after waiting %d request id %d operation %s',[MaxWaitMilliseconds,request_id,TNetData.OperationToText(noperation)]));
      end;
    finally
      SendData.Free;
      ReceiveData.free;
    end;
  end;

  function Do_GetOperationBlock(block, MaxWaitMilliseconds : Cardinal; var OperationBlock : TOperationBlock) : Boolean;
  Var BlocksList : TList<TPCOperationsComp>;
    i : Integer;
  begin
    OperationBlock := CT_OperationBlock_NUL;
    BlocksList := TList<TPCOperationsComp>.Create;
    try
      Result := Do_GetOperationsBlock(TNode.Node.Bank,block,block,MaxWaitMilliseconds,True,BlocksList);
      // Build 2.1.7 - Included protection agains not good block received
      if (Result) And (BlocksList.Count=1) then begin
        OperationBlock := TPCOperationsComp(BlocksList[0]).OperationBlock;
        If OperationBlock.block<>block then Result := False;
      end else begin
        Result := False;
      end;
    finally
      for i := 0 to BlocksList.Count - 1 do TPCOperationsComp(BlocksList[i]).Free;
      BlocksList.Free;
    end;
  end;

  Function FindLastSameBlockByOperationsBlock(min,max : Cardinal; var OperationBlock : TOperationBlock) : Boolean;
  var i : Integer;
    ant_nblock : Int64;
    auxBlock, sbBlock : TOperationBlock;
    distinctmax,distinctmin : Cardinal;
    BlocksList : TList<TPCOperationsComp>;
    errors : String;
  Begin
    Result := false;
    OperationBlock := CT_OperationBlock_NUL;
    repeat
      BlocksList := TList<TPCOperationsComp>.Create;
      try
        If Not Do_GetOperationsBlock(Nil,min,max,20000,true,BlocksList) then exit;
        if (BlocksList.Count=0) then begin
          Connection.DisconnectInvalidClient(false,'No received info for blocks from '+inttostr(min)+' to '+inttostr(max));
          exit;
        end;
        distinctmin := min;
        distinctmax := max;
        ant_nblock := -1;
        for i := 0 to BlocksList.Count - 1 do begin
          auxBlock := TPCOperationsComp(BlocksList[i]).OperationBlock;
          // Protection of invalid clients:
          if (auxBlock.block<min) Or (auxBlock.block>max) Or (auxBlock.block=ant_nblock) then begin
            Connection.DisconnectInvalidClient(false,'Invalid response... '+inttostr(min)+'<'+inttostr(auxBlock.block)+'<'+inttostr(max)+' ant:'+inttostr(ant_nblock));
            exit;
          end;
          // New Build 2.1.7 - Check valid operationblock
          If Not TPCSafeBox.IsValidOperationBlock(auxBlock,errors) then begin
            Connection.DisconnectInvalidClient(false,'Received invalid operation block searching '+TPCOperationsComp.OperationBlockToText(auxBlock)+' errors: '+errors);
            Exit;
          end;

          ant_nblock := auxBlock.block;
          //
          sbBlock := TNode.Node.Bank.SafeBox.Block(auxBlock.block).blockchainInfo;
          if TPCOperationsComp.EqualsOperationBlock(sbBlock,auxBlock) then begin
            distinctmin := auxBlock.block;
            OperationBlock := auxBlock;
          end else begin
            if auxBlock.block<=distinctmax then
              distinctmax := auxBlock.block-1;
          end;
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

  procedure GetNewBank(start_block : Int64);
  Var BlocksList : TList<TPCOperationsComp>;
    i : Integer;
    OpComp,OpExecute : TPCOperationsComp;
    oldBlockchainOperations : TOperationsHashTree;
    opsResume : TOperationsResumeList;
    newBlock : TBlockAccount;
    errors : String;
    start,start_c : Cardinal;
    finished : Boolean;
    Bank : TPCBank;
    ms : TMemoryStream;
    IsAScam, IsUsingSnapshot : Boolean;
  Begin
    IsAScam := false;
    TLog.NewLog(ltdebug,CT_LogSender,Format('GetNewBank(new_start_block:%d)',[start_block]));

    Bank := TPCBank.Create(Nil);
    try
      Bank.StorageClass := TNode.Node.Bank.StorageClass;
      Bank.Storage.Orphan := TNode.Node.Bank.Storage.Orphan;
      Bank.Storage.ReadOnly := true;
      Bank.Storage.CopyConfiguration(TNode.Node.Bank.Storage);
      if start_block>=0 then begin
        If (TNode.Node.Bank.SafeBox.HasSnapshotForBlock(start_block-1)) then begin
          // Restore from a Snapshot (New on V3) instead of restore reading from File
          Bank.SafeBox.SetToPrevious(TNode.Node.Bank.SafeBox,start_block-1);
          Bank.UpdateValuesFromSafebox;
          IsUsingSnapshot := True;
        end else begin
          // Restore a part from disk
          Bank.DiskRestoreFromOperations(start_block-1);
          Bank.Storage.SaveBank(True);
          if (Bank.BlocksCount<start_block) then begin
            TLog.NewLog(lterror,CT_LogSender,Format('No blockchain found start block %d, current %d',[start_block-1,Bank.BlocksCount]));
            start_block := Bank.BlocksCount;
          end;
          IsUsingSnapshot := False;
        end;
        start := start_block;
      end else begin
        start := 0;
        start_block := 0;
      end;
      start_c := start;
      Bank.Storage.Orphan := FormatDateTime('yyyymmddhhnnss',DateTime2UnivDateTime(now));
      Bank.Storage.ReadOnly := false;
      // Receive new blocks:
      finished := false;
      repeat
        BlocksList := TList<TPCOperationsComp>.Create;
        try
          finished := NOT Do_GetOperationsBlock(Bank,start,start + 50,30000,false,BlocksList);
          i := 0;
          while (i<BlocksList.Count) And (Not finished) do begin
            OpComp := TPCOperationsComp(BlocksList[i]);
            ms := TMemoryStream.Create;
            OpExecute := TPCOperationsComp.Create(Bank);
            try
              OpComp.SaveBlockToStream(false,ms);
              ms.Position := 0;
              If not OpExecute.LoadBlockFromStream(ms,errors) then begin
                Connection.DisconnectInvalidClient(false,'Invalid block stream received for block '+IntToStr(Bank.BlocksCount)+' errors: '+errors );
                finished := true;
                IsAScam := true;
                break;
              end;
              TNode.Node.MarkVerifiedECDSASignaturesFromMemPool(OpExecute); // Improvement speed v4.0.2
              if Bank.AddNewBlockChainBlock(OpExecute,TNetData.NetData.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock,newBlock,errors) then begin
                inc(i);
              end else begin
                TLog.NewLog(lterror,CT_LogSender,'Error creating new bank with client Operations. Block:'+TPCOperationsComp.OperationBlockToText(OpExecute.OperationBlock)+' Error:'+errors);
                // Add to blacklist !
                Connection.DisconnectInvalidClient(false,'Invalid BlockChain on Block '+TPCOperationsComp.OperationBlockToText(OpExecute.OperationBlock)+' with errors:'+errors);
                finished := true;
                IsAScam := true;
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
      until (Bank.BlocksCount=Connection.FRemoteOperationBlock.block+1) Or (finished)
        // Allow to do not download ALL new blockchain in a separate folder, only needed blocks!
        Or (Bank.SafeBox.WorkSum > (TNode.Node.Bank.SafeBox.WorkSum + $FFFFFFFF) );
      // New Build 1.5 more work vs more high
      // work = SUM(target) of all previous blocks (Int64)
      // -----------------------------
      // Before of version 1.5 was: "if Bank.BlocksCount>TNode.Node.Bank.BlocksCount then ..."
      // Starting on version 1.5 is: "if Bank.WORK > MyBank.WORK then ..."
      if Bank.SafeBox.WorkSum > TNode.Node.Bank.SafeBox.WorkSum then begin
        oldBlockchainOperations := TOperationsHashTree.Create;
        try
          TNode.Node.DisableNewBlocks;
          Try
            // I'm an orphan blockchain...
            TLog.NewLog(ltinfo,CT_LogSender,'New valid blockchain found. My block count='+inttostr(TNode.Node.Bank.BlocksCount)+' work: '+IntToStr(TNode.Node.Bank.SafeBox.WorkSum)+
              ' found count='+inttostr(Bank.BlocksCount)+' work: '+IntToStr(Bank.SafeBox.WorkSum)+' starting at block '+inttostr(start_block));
            if TNode.Node.Bank.BlocksCount>0 then begin
              OpExecute := TPCOperationsComp.Create(Nil);
              try
                for start:=start_c to TNode.Node.Bank.BlocksCount-1 do begin
                  If TNode.Node.Bank.LoadOperations(OpExecute,start) then begin
                    if (OpExecute.Count>0) then begin
                      for i:=0 to OpExecute.Count-1 do begin
                        // TODO: NEED TO EXCLUDE OPERATIONS ALREADY INCLUDED IN BLOCKCHAIN?
                        oldBlockchainOperations.AddOperationToHashTree(OpExecute.Operation[i]);
                      end;
                      TLog.NewLog(ltInfo,CT_LogSender,'Recovered '+IntToStr(OpExecute.Count)+' operations from block '+IntToStr(start));
                    end;
                  end else begin
                    TLog.NewLog(ltError,CT_LogSender,'Fatal error: Cannot read block '+IntToStr(start));
                  end;
                end;
              finally
                OpExecute.Free;
              end;
            end;
            TNode.Node.Bank.Storage.MoveBlockChainBlocks(start_block,Inttostr(start_block)+'_'+FormatDateTime('yyyymmddhhnnss',DateTime2UnivDateTime(now)),Nil);
            Bank.Storage.MoveBlockChainBlocks(start_block,TNode.Node.Bank.Storage.Orphan,TNode.Node.Bank.Storage);
            //
            If IsUsingSnapshot then begin
              TLog.NewLog(ltInfo,CT_LogSender,'Commiting new chain to Safebox');
              Bank.SafeBox.CommitToPrevious;
              TNode.Node.Bank.UpdateValuesFromSafebox; // BUG 2018-10-14 -> Must update TNode.Node.Bank instead of Bank, because FLastBlockCache must upgrade
              {$IFDEF Check_Safebox_Names_Consistency}
              If Not Check_Safebox_Names_Consistency(Bank.SafeBox,'Commited',errors) then begin
                TLog.NewLog(lterror,CT_LogSender,'Fatal safebox consistency error getting bank at block '+IntTosTr(start_block)+' : '+errors);
                Sleep(1000);
                halt(0);
              end;
              {$ENDIF}
            end else begin
              TLog.NewLog(ltInfo,CT_LogSender,'Restoring modified Safebox from Disk');
              TNode.Node.Bank.DiskRestoreFromOperations(CT_MaxBlock);
            end;
          Finally
            TNode.Node.EnableNewBlocks;
          End;
          TNode.Node.NotifyBlocksChanged;
          // Finally add new operations:
          // Rescue old operations from old blockchain to new blockchain
          If oldBlockchainOperations.OperationsCount>0 then begin
            TLog.NewLog(ltInfo,CT_LogSender,Format('Executing %d operations from block %d to %d',
             [oldBlockchainOperations.OperationsCount,start_c,TNode.Node.Bank.BlocksCount-1]));
            opsResume := TOperationsResumeList.Create;
            Try
              // Re-add orphaned operations back into the pending pool.
              // NIL is passed as senderConnection since localnode is considered
              // the origin, and current sender needs these operations.
              i := TNode.Node.AddOperations(NIL,oldBlockchainOperations,opsResume,errors);
              TLog.NewLog(ltInfo,CT_LogSender,Format('Executed %d/%d operations. Returned errors: %s',[i,oldBlockchainOperations.OperationsCount,errors]));
            finally
              opsResume.Free;
            end;
          end else TLog.NewLog(ltInfo,CT_LogSender,Format('No operations from block %d to %d',[start_c,TNode.Node.Bank.BlocksCount-1]));
        finally
          oldBlockchainOperations.Free;
        end;
      end else begin
        if (Not IsAScam) And (Connection.FRemoteAccumulatedWork > TNode.Node.Bank.SafeBox.WorkSum) then begin
          // Possible scammer!
          Connection.DisconnectInvalidClient(false,Format('Possible scammer! Says blocks:%d Work:%d - Obtained blocks:%d work:%d',
            [Connection.FRemoteOperationBlock.block+1,Connection.FRemoteAccumulatedWork,
             Bank.BlocksCount,Bank.SafeBox.WorkSum]));
        end;
      end;
    finally
      Bank.Free;
    end;
  End;

  Function DownloadSafeBoxChunk(safebox_blockscount : Cardinal; Const sbh : TRawBytes; from_block, to_block : Cardinal; receivedDataUnzipped : TStream;
    var safeBoxHeader : TPCSafeBoxHeader; var errors : String) : Boolean;
  Var sendData,receiveData : TStream;
    headerdata : TNetHeaderData;
    request_id : Cardinal;
    c : Cardinal;
  Begin
    Result := False;
    sendData := TMemoryStream.Create;
    receiveData := TMemoryStream.Create;
    try
      sendData.Write(safebox_blockscount,SizeOf(safebox_blockscount)); // 4 bytes for blockcount
      TStreamOp.WriteAnsiString(SendData,sbh);
      sendData.Write(from_block,SizeOf(from_block));
      c := to_block;
      if (c>=safebox_blockscount) then c := safebox_blockscount-1;
      sendData.Write(c,SizeOf(c));
      if (from_block>c) or (c>=safebox_blockscount) then begin
        errors := 'ERROR DEV 20170727-1';
        Exit;
      end;
      TLog.NewLog(ltDebug,CT_LogSender,Format('Call to GetSafeBox from blocks %d to %d of %d',[from_block,c,safebox_blockscount]));
      request_id := TNetData.NetData.NewRequestId;
      if Connection.DoSendAndWaitForResponse(CT_NetOp_GetSafeBox,request_id,sendData,receiveData,30000,headerdata) then begin
        if HeaderData.is_error then exit;
        receivedDataUnzipped.Size:=0;
        If Not TPCChunk.LoadSafeBoxFromChunk(receiveData,receivedDataUnzipped,safeBoxHeader,errors) then begin
          Connection.DisconnectInvalidClient(false,'Invalid received chunk: '+errors);
          exit;
        end;
        If (Not (TBaseType.Equals(safeBoxHeader.safeBoxHash,sbh))) or (safeBoxHeader.startBlock<>from_block) or (safeBoxHeader.endBlock<>c) or
          (safeBoxHeader.blocksCount<>safebox_blockscount) or (safeBoxHeader.protocol<CT_PROTOCOL_2) or
          (safeBoxHeader.protocol>CT_BlockChain_Protocol_Available) then begin
          errors := Format('Invalid received chunk based on call: Blockscount:%d %d - from:%d %d to %d %d - SafeboxHash:%s %s',
              [safeBoxHeader.blocksCount,safebox_blockscount,safeBoxHeader.startBlock,from_block,safeBoxHeader.endBlock,c,
               safeBoxHeader.safeBoxHash.ToHexaString,sbh.ToHexaString]);
          Connection.DisconnectInvalidClient(false,'Invalid received chunk: '+errors);
          exit;
        end;
        Result := True;
      end else errors := 'No response on DownloadSafeBoxChunk';
    finally
      receiveData.Free;
      SendData.Free;
    end;
  end;

  Type TSafeBoxChunkData = Record
    safeBoxHeader : TPCSafeBoxHeader;
    chunkStream : TStream;
  end;

  Function DownloadSafeboxStream(safeboxStream : TStream; var safebox_last_operation_block : TOperationBlock) : Boolean;
  var _blockcount, request_id : Cardinal;
    chunks : Array of TSafeBoxChunkData;
    receiveChunk, chunk1 : TStream;
    safeBoxHeader : TPCSafeBoxHeader;
    errors : String;
    i : Integer;
  Begin
    Result := False;
    safeboxStream.Size:=0;
    safeboxStream.Position:=0;
    // Will try to download penultimate saved safebox
    _blockcount := ((Connection.FRemoteOperationBlock.block DIV CT_BankToDiskEveryNBlocks)-1) * CT_BankToDiskEveryNBlocks;
    If not Do_GetOperationBlock(_blockcount,5000,safebox_last_operation_block) then begin
      Connection.DisconnectInvalidClient(false,Format('Cannot obtain operation block %d for downloading safebox',[_blockcount]));
      exit;
    end;
    // New Build 2.1.7 - Check valid operationblock
    If Not TPCSafeBox.IsValidOperationBlock(safebox_last_operation_block,errors) then begin
      Connection.DisconnectInvalidClient(false,'Invalid operation block at DownloadSafeBox '+TPCOperationsComp.OperationBlockToText(safebox_last_operation_block)+' errors: '+errors);
      Exit;
    end;
    SetLength(chunks,0);
    try
      // Will obtain chunks of 10000 blocks each -> Note: Maximum is CT_MAX_SAFEBOXCHUNK_BLOCKS
      for i:=0 to ((_blockcount-1) DIV 10000) do begin // Bug v3.0.1 and minors
        FNewBlockChainFromClientStatus := Format('Receiving new safebox with %d blocks (step %d/%d) from %s',
          [_blockcount,i+1,((_blockcount-1) DIV 10000)+1,Connection.ClientRemoteAddr]);
        receiveChunk := TMemoryStream.Create;
        if (Not DownloadSafeBoxChunk(_blockcount,safebox_last_operation_block.initial_safe_box_hash,(i*10000),((i+1)*10000)-1,receiveChunk,safeBoxHeader,errors)) then begin
          receiveChunk.Free;
          TLog.NewLog(ltError,CT_LogSender,errors);
          Exit;
        end;
        SetLength(chunks,length(chunks)+1);
        chunks[High(chunks)].safeBoxHeader := safeBoxHeader;
        chunks[High(chunks)].chunkStream := receiveChunk;
      end;
      // Will concat safeboxs:
      chunk1 := TMemoryStream.Create;
      try
        if (length(chunks)=1) then begin
          safeboxStream.CopyFrom(chunks[0].chunkStream,0);
        end else begin
          chunk1.CopyFrom(chunks[0].chunkStream,0);
        end;
        for i:=1 to high(chunks) do begin
          safeboxStream.Size:=0;
          chunk1.Position:=0;
          chunks[i].chunkStream.Position:=0;
          If Not TPCSafeBox.ConcatSafeBoxStream(chunk1,chunks[i].chunkStream,safeboxStream,errors) then begin
            TLog.NewLog(ltError,CT_LogSender,errors);
            exit;
          end;
          chunk1.Size := 0;
          chunk1.CopyFrom(safeboxStream,0);
        end;
      finally
        chunk1.Free;
      end;
    finally
      for i:=0 to high(chunks) do begin
        chunks[i].chunkStream.Free;
      end;
      SetLength(chunks,0);
    end;
    Result := True;
  End;

  Function DownloadSafeBox(IsMyBlockchainValid : Boolean) : Boolean;
  var receiveData : TStream;
    op : TOperationBlock;
    errors : String;
    request_id : Cardinal;
  Begin
    Result := False;
    receiveData := TMemoryStream.Create;
    try
      if Not DownloadSafeboxStream(receiveData,op) then Exit;
      // Now receiveData is the ALL safebox
      TNode.Node.DisableNewBlocks;
      try
          FNewBlockChainFromClientStatus := Format('Received new safebox with %d blocks from %s',[op.block+1,Connection.ClientRemoteAddr]);
          receiveData.Position:=0;
          If TNode.Node.Bank.LoadBankFromStream(receiveData,True,op.initial_safe_box_hash,TNode.Node.Bank.SafeBox,OnReadingNewSafeboxProgressNotify,errors) then begin
            TLog.NewLog(ltInfo,ClassName,'Received new safebox!');
            If Not IsMyBlockchainValid then begin
              TNode.Node.Bank.Storage.EraseStorage;
            end;
            TNode.Node.Bank.Storage.SaveBank(False);
            Connection.Send_GetBlocks(TNode.Node.Bank.BlocksCount,100,request_id);
            Result := true;
          end else begin
            Connection.DisconnectInvalidClient(false,'Cannot load from stream! '+errors);
            exit;
          end;
      finally
        TNode.Node.EnableNewBlocks;
      end;
    finally
      receiveData.Free;
    end;
  end;

  procedure DownloadNewBlockchain(start_block : Int64; IsMyBlockChainOk : Boolean);
  var safeboxStream : TMemoryStream;
    newTmpBank : TPCBank;
    safebox_last_operation_block : TOperationBlock;
    newBlock : TBlockAccount;
    opComp : TPCOperationsComp;
    errors : String;
    blocksList : TList<TPCOperationsComp>;
    i : Integer;
    rid : Cardinal;
    download_new_safebox : Boolean;
  begin
    download_new_safebox := (FMinFutureBlocksToDownloadNewSafebox>0) And ((TNode.Node.Bank.BlocksCount + FMinFutureBlocksToDownloadNewSafebox) <= Connection.RemoteOperationBlock.block);
    if Assigned(OnGetNewBlockchainFromClientDownloadNewSafebox) then begin
      // Note: Will call to an event inside a thread, not main thread, be careful
      OnGetNewBlockchainFromClientDownloadNewSafebox(Self,Connection,TNode.Node.Bank.BlocksCount,Connection.RemoteOperationBlock.block,download_new_safebox);
    end;
    if (download_new_safebox) then begin
      TLog.NewLog(ltinfo,ClassName,Format('Will download new safebox. My blocks:%d Remote blocks:%d Equal Block:%d (MaxFutureBlocksToDownloadNewSafebox:%d)',[TNode.Node.Bank.BlocksCount,Connection.RemoteOperationBlock.block+1,start_block-1,MinFutureBlocksToDownloadNewSafebox]));
      // Will try to download safebox
      safeboxStream := TMemoryStream.Create;
      Try
        if Not DownloadSafeboxStream(safeboxStream,safebox_last_operation_block) then Exit;
        safeboxStream.Position := 0;
        newTmpBank := TPCBank.Create(Nil);
        try
          newTmpBank.StorageClass := TNode.Node.Bank.StorageClass;
          newTmpBank.Storage.Orphan := TNode.Node.Bank.Storage.Orphan;
          newTmpBank.Storage.ReadOnly := true;
          newTmpBank.Storage.CopyConfiguration(TNode.Node.Bank.Storage);
          newTmpBank.Storage.Orphan := FormatDateTime('yyyymmddhhnnss',DateTime2UnivDateTime(now));
          newTmpBank.Storage.ReadOnly := false;
          if newTmpBank.LoadBankFromStream(safeboxStream,True,safebox_last_operation_block.initial_safe_box_hash,TNode.Node.Bank.SafeBox,OnReadingNewSafeboxProgressNotify,errors) then begin
            TNode.Node.DisableNewBlocks;
            try
              TLog.NewLog(ltInfo,ClassName,'Received new safebox!');
              newTmpBank.Storage.SaveBank(True); // Saving bank
              // Receive at least 1 new block
              blocksList := TList<TPCOperationsComp>.Create;
              try
                if Not Do_GetOperationsBlock(newTmpBank,safebox_last_operation_block.block,safebox_last_operation_block.block+10,20000,False,blocksList) then begin
                  TLog.NewLog(ltError,ClassName,Format('Cannot receive at least 1 new block:%d',[safebox_last_operation_block.block]));
                  Exit;
                end;
                for i:=0 to blocksList.Count-1 do begin
                  opComp := TPCOperationsComp( blocksList[i] );
                  if Not newTmpBank.AddNewBlockChainBlock(opComp,TNetData.NetData.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock,newBlock,errors) then begin
                    TLog.NewLog(lterror,CT_LogSender,'Error adding new block with client Operations. Block:'+TPCOperationsComp.OperationBlockToText(opComp.OperationBlock)+' Error:'+errors);
                    // Add to blacklist !
                    Connection.DisconnectInvalidClient(false,'Invalid BlockChain on Block '+TPCOperationsComp.OperationBlockToText(opComp.OperationBlock)+' with errors:'+errors);
                    Exit;
                  end;
                end;
              finally
                for i := 0 to blocksList.Count-1 do begin
                  TPCOperationsComp(blocksList[i]).Free;
                end;
                blocksList.Free;
              end;
              // We are ready to upgrade with newest safebox
              // Delete blocks since start_block at current TNode
              TNode.Node.Bank.Storage.MoveBlockChainBlocks(start_block,IntToStr(start_block)+'_'+FormatDateTime('yyyymmddhhnnss',DateTime2UnivDateTime(now)),Nil);
              TNode.Node.Bank.Storage.DeleteBlockChainBlocks(start_block);

              newTmpBank.Storage.MoveBlockChainBlocks(safebox_last_operation_block.block,'',TNode.Node.Bank.Storage);
              TNode.Node.Bank.DiskRestoreFromOperations(CT_MaxBlock);
            Finally
              TNode.Node.EnableNewBlocks;
            End;
            TNode.Node.NotifyBlocksChanged;
            // High to new value:
            Connection.Send_GetBlocks(TNode.Node.Bank.BlocksCount,100,rid);
          end else begin
            Connection.DisconnectInvalidClient(false,'Cannot load from stream! '+errors);
            exit;
          end;

        finally
          newTmpBank.Free;
        end;
      Finally
        safeboxStream.Free;
      End;
    end else begin
      if IsMyBlockChainOk then begin
        Connection.Send_GetBlocks(start_block,1,rid);
      end else begin
        GetNewBank(start_block);
      end;
    end;
  end;

var rid : Cardinal;
  my_op, client_op : TOperationBlock;
  errors : String;
begin
  // Protection against discovering servers...
  if FIsDiscoveringServers then begin
    {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,CT_LogSender,'Is discovering servers...');{$ENDIF}
    exit;
  end;
  if (Not TNode.Node.UpdateBlockchain) then Exit;

  if (Not Assigned(TNode.Node.Bank.StorageClass)) then Exit;
  //
  if Not FLockGettingNewBlockChainFromClient.TryEnter then begin
    TLog.NewLog(ltdebug,CT_LogSender,'Is getting new blockchain from client...');
    exit;
  end;
  Try
    TLog.NewLog(ltdebug,CT_LogSender,'Starting receiving: '+why);
    FNewBlockChainFromClientStatus := Format('Downloading block %d from %s',[Connection.RemoteOperationBlock.block,Connection.ClientRemoteAddr]);
    FMaxRemoteOperationBlock := Connection.FRemoteOperationBlock;
    if TNode.Node.Bank.BlocksCount=0 then begin
      TLog.NewLog(ltdebug,CT_LogSender,'I have no blocks');
      If Connection.FRemoteOperationBlock.protocol_version>=CT_PROTOCOL_2 then begin
        DownloadSafeBox(False);
      end else begin
        Connection.Send_GetBlocks(0,10,rid);
      end;
      exit;
    end;
    TLog.NewLog(ltdebug,CT_LogSender,'Starting GetNewBlockChainFromClient at client:'+Connection.ClientRemoteAddr+
      ' with OperationBlock:'+TPCOperationsComp.OperationBlockToText(Connection.FRemoteOperationBlock)+' (My block: '+TPCOperationsComp.OperationBlockToText(TNode.Node.Bank.LastOperationBlock)+')');
    // NOTE: FRemoteOperationBlock.block >= TNode.Node.Bank.BlocksCount
    // First capture same block than me (TNode.Node.Bank.BlocksCount-1) to check if i'm an orphan block...
    my_op := TNode.Node.Bank.LastOperationBlock;
    If Not Do_GetOperationBlock(my_op.block,5000,client_op) then begin
      TLog.NewLog(lterror,CT_LogSender,'Cannot receive information about my block ('+inttostr(my_op.block)+')...');
      // Disabled at Build 1.0.6 >  Connection.DisconnectInvalidClient(false,'Cannot receive information about my block ('+inttostr(my_op.block)+')... Invalid client. Disconnecting');
      Exit;
    end;
    // New Build 2.1.7 - Check valid operationblock
    If Not TPCSafeBox.IsValidOperationBlock(client_op,errors) then begin
      Connection.DisconnectInvalidClient(false,'Received invalid operation block '+TPCOperationsComp.OperationBlockToText(client_op)+' errors: '+errors);
      Exit;
    end;

    if (NOT TPCOperationsComp.EqualsOperationBlock(my_op,client_op)) then begin
      if (my_op.protocol_version > client_op.protocol_version) then begin // Version 4.0.2 protection against going back to previous protocol with highest blockchain
        TPCOperationsComp.OperationBlockToText(my_op);
        TLog.NewLog(lterror,CT_LogSender,Format('Detected an orphan highest blockchain in an old protocol. Detected: %s - My data: %s',[TPCOperationsComp.OperationBlockToText(client_op),TPCOperationsComp.OperationBlockToText(my_op)]));
        Connection.DisconnectInvalidClient(false,'Detected an orphan highest blockchain in an old protocol');
        Exit;
      end;
      TLog.NewLog(ltinfo,CT_LogSender,'My blockchain is not equal... received: '+TPCOperationsComp.OperationBlockToText(client_op)+' My: '+TPCOperationsComp.OperationBlockToText(my_op));
      if Not FindLastSameBlockByOperationsBlock(0,client_op.block,client_op) then begin
        Connection.DisconnectInvalidClient(false,'No found any base block to start process...');
        Exit;
      end else begin
        // Move operations to orphan folder... (temporal... waiting for a confirmation)
        if (TNode.Node.Bank.Storage.FirstBlock<client_op.block) then begin
          TLog.NewLog(ltinfo,CT_LogSender,'Found base new block: '+TPCOperationsComp.OperationBlockToText(client_op));
          DownloadNewBlockchain(client_op.block+1,False);
        end else begin
          TLog.NewLog(ltinfo,CT_LogSender,'Found base new block: '+TPCOperationsComp.OperationBlockToText(client_op)+' lower than saved:'+IntToStr(TNode.Node.Bank.Storage.FirstBlock));
          DownloadSafeBox(False);
        end;
      end;
    end else begin
      TLog.NewLog(ltinfo,CT_LogSender,'My blockchain is ok! Need to download new blocks starting at '+inttostr(my_op.block+1));
      // High to new value:
      DownloadNewBlockchain(my_op.block+1,True);
    end;
  Finally
    TLog.NewLog(ltdebug,CT_LogSender,'Finalizing');
    FLockGettingNewBlockChainFromClient.Release;
  end;
end;

class function TNetData.HeaderDataToText(const HeaderData: TNetHeaderData): String;
begin
  Result := CT_NetTransferType[HeaderData.header_type]+' Operation:'+TNetData.OperationToText(HeaderData.operation);
  if HeaderData.is_error then begin
    Result := Result +' ERRCODE:'+Inttostr(HeaderData.error_code)+' ERROR:'+HeaderData.error_text;
  end else begin
    Result := Result +' ReqId:'+Inttostr(HeaderData.request_id)+' BufferSize:'+Inttostr(HeaderData.buffer_data_length);
  end;
end;

procedure TNetData.IncStatistics(incActiveConnections, incClientsConnections,
  incServersConnections,incServersConnectionsWithResponse: Integer; incBytesReceived, incBytesSend: Int64);
begin
  // Multithread prevention
  FNodeServersAddresses.FCritical.Acquire;
  Try
    FNetStatistics.ActiveConnections := FNetStatistics.ActiveConnections + incActiveConnections;
    FNetStatistics.ClientsConnections := FNetStatistics.ClientsConnections + incClientsConnections;
    FNetStatistics.ServersConnections := FNetStatistics.ServersConnections + incServersConnections;
    FNetStatistics.ServersConnectionsWithResponse := FNetStatistics.ServersConnectionsWithResponse + incServersConnectionsWithResponse;
    if (incActiveConnections>0) then FNetStatistics.TotalConnections := FNetStatistics.TotalConnections + incActiveConnections;
    if (incClientsConnections>0) then FNetStatistics.TotalClientsConnections := FNetStatistics.TotalClientsConnections + incClientsConnections;
    if (incServersConnections>0) then FNetStatistics.TotalServersConnections := FNetStatistics.TotalServersConnections + incServersConnections;
    FNetStatistics.BytesReceived := FNetStatistics.BytesReceived + incBytesReceived;
    FNetStatistics.BytesSend := FNetStatistics.BytesSend + incBytesSend;
  Finally
    FNodeServersAddresses.FCritical.Release;
  End;
  NotifyStatisticsChanged;
  if (incBytesReceived<>0) Or (incBytesSend<>0) then begin
    NotifyNetConnectionUpdated;
  end;
end;

function TNetData.IsGettingNewBlockChainFromClient(var status: String): Boolean;
begin
  if FLockGettingNewBlockChainFromClient.TryEnter then begin
    try
      Result := False;
      status := '';
    finally
      FLockGettingNewBlockChainFromClient.Release;
    end;
  end else begin
    status := FNewBlockChainFromClientStatus;
    Result := True;
  end;
end;

procedure TNetData.SetMaxNodeServersAddressesBuffer(AValue: Integer);
begin
  if FMaxNodeServersAddressesBuffer=AValue then Exit;
  if (AValue<CT_MIN_NODESERVERS_BUFFER) then FMaxNodeServersAddressesBuffer:=CT_MIN_NODESERVERS_BUFFER
  else if (AValue>CT_MAX_NODESERVERS_BUFFER) then FMaxNodeServersAddressesBuffer:=CT_MAX_NODESERVERS_BUFFER
  else FMaxNodeServersAddressesBuffer:=AValue;
end;

procedure TNetData.SetMaxServersConnected(AValue: Integer);
begin
  if FMaxServersConnected=AValue then Exit;
  if AValue<1 then FMaxServersConnected:=1
  else FMaxServersConnected:=AValue;
  if FMaxServersConnected<FMinServersConnected then FMinServersConnected:=FMaxServersConnected;
end;

procedure TNetData.SetMinFutureBlocksToDownloadNewSafebox(const Value: Integer);
begin
  // Will allow a minimum of 200 future blocks fo enable download a new safebox
  if (Value<=200) then FMinFutureBlocksToDownloadNewSafebox := 0
  else FMinFutureBlocksToDownloadNewSafebox := Value;
end;

procedure TNetData.SetMinServersConnected(AValue: Integer);
begin
  if FMinServersConnected=AValue then Exit;
  if AValue<1 then FMinServersConnected:=1
  else FMinServersConnected:=AValue;
  if FMaxServersConnected<FMinServersConnected then FMaxServersConnected:=FMinServersConnected;
end;

class function TNetData.NetData: TNetData;
begin
  if Not Assigned(_NetData) then begin
    _NetData := TNetData.Create(nil);
  end;
  result := _NetData;
end;

class function TNetData.NetDataExists: Boolean;
begin
  Result := Assigned(_NetData);
end;

function TNetData.NewRequestId: Cardinal;
begin
  Inc(FLastRequestId);
  Result := FLastRequestId;
end;

procedure TNetData.Notification(AComponent: TComponent; Operation: TOperation);
Var l : TList<TNetConnection>;
begin
  inherited;
  if (Operation=OpRemove) and Assigned(AComponent) and (AComponent is TNetConnection) then begin
    if not (csDestroying in ComponentState) then begin
      l := FNetConnections.LockList;
      try
        if l.Remove(TNetConnection(AComponent))>=0 then begin
          NotifyNetConnectionUpdated;
        end;
      finally
        FNetConnections.UnlockList;
      end;
    end;
  end;
end;

procedure TNetData.NotifyBlackListUpdated;
begin
  FNetDataNotifyEventsThread.FNotifyOnBlackListUpdated := true;
end;

procedure TNetData.NotifyConnectivityChanged;
begin
  FOnConnectivityChanged.Invoke(Self);
end;

procedure TNetData.NotifyNetConnectionUpdated;
begin
  FNetDataNotifyEventsThread.FNotifyOnNetConnectionsUpdated := true;
end;

procedure TNetData.NotifyNodeServersUpdated;
begin
  FNetDataNotifyEventsThread.FNotifyOnNodeServersUpdated := true;
end;

procedure TNetData.NotifyReceivedHelloMessage;
begin
  FNetDataNotifyEventsThread.FNotifyOnReceivedHelloMessage := true;
end;

procedure TNetData.NotifyStatisticsChanged;
begin
  FNetDataNotifyEventsThread.FNotifyOnStatisticsChanged := true;
end;

procedure TNetData.OnReadingNewSafeboxProgressNotify(sender: TObject; const mesage: String; curPos, totalCount: Int64);
Var pct : String;
begin
  if (totalCount>0) then pct := FormatFloat('0.00',curPos*100/totalCount)+'%' else pct := '';

  FNewBlockChainFromClientStatus := Format('Checking new safebox: %s %s',[mesage,pct]);
end;

class function TNetData.OperationToText(operation: Word): String;
begin
  case operation of
    CT_NetOp_Hello : Result := 'HELLO';
    CT_NetOp_Error : Result := 'ERROR';
    CT_NetOp_GetBlocks : Result := 'GET_BLOCKS';
    CT_NetOp_Message : Result := 'MESSAGE';
    CT_NetOp_GetBlockHeaders : Result := 'GET_BLOCK_HEADERS';
    CT_NetOp_NewBlock : Result := 'NEW_BLOCK';
    CT_NetOp_NewBlock_Fast_Propagation : Result := 'NEW_BLOCK_FAST_PROPAGATION';
    CT_NetOp_GetBlockchainOperations : Result := 'GET_BLOCKCHAIN_OPERATIONS';
    CT_NetOp_AddOperations : Result := 'ADD_OPERATIONS';
    CT_NetOp_GetSafeBox : Result := 'GET_SAFEBOX';
    CT_NetOp_GetPendingOperations : Result := 'GET_PENDING_OPERATIONS';
    CT_NetOp_GetAccount : Result := 'GET_ACCOUNT';
    CT_NetOp_GetPubkeyAccounts : Result := 'GET_PUBKEY_ACCOUNTS';
  else Result := 'UNKNOWN_OPERATION_'+Inttohex(operation,4);
  end;
end;

function TNetData.PendingRequest(Sender: TNetConnection; var requests_data : String): Integer;
Var P : PNetRequestRegistered;
  i : Integer;
  l : TList<Pointer>;
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
  l : TList<Pointer>;
begin
  l := FRegisteredRequests.LockList;
  Try
    New(P);
    P^.NetClient := Sender;
    P^.Operation := operation;
    P^.RequestId := request_id;
    P^.SendTime := Now;
    l.Add(P);
    {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Registering request to '+Sender.ClientRemoteAddr+' Op:'+OperationToText(operation)+' Id:'+inttostr(request_id)+' Total pending:'+Inttostr(l.Count));{$ENDIF}
  Finally
    FRegisteredRequests.UnlockList;
  End;
end;

procedure TNetData.SetNetConnectionsActive(const Value: Boolean);
begin
  FNetConnectionsActive := Value;
  NotifyConnectivityChanged;
  if FNetConnectionsActive then DiscoverServers
  else DisconnectClients;
end;

function TNetData.UnRegisterRequest(Sender: TNetConnection; operation: Word; request_id: Cardinal): Boolean;
Var P : PNetRequestRegistered;
  i : Integer;
  l : TList<Pointer>;
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
        {$IFDEF HIGHLOG}
        if Assigned(Sender.FTcpIpClient) then begin
          TLog.NewLog(ltdebug,Classname,'Unregistering request to '+Sender.ClientRemoteAddr+' Op:'+OperationToText(operation)+' Id:'+inttostr(request_id)+' Total pending:'+Inttostr(l.Count));
        end else begin
          TLog.NewLog(ltdebug,Classname,'Unregistering request to (NIL) Op:'+OperationToText(operation)+' Id:'+inttostr(request_id)+' Total pending:'+Inttostr(l.Count));
        end;
        {$ENDIF}
      end;
    end;
  finally
    FRegisteredRequests.UnlockList;
  end;
end;

{ TNetServer }

constructor TNetServer.Create;
begin
  inherited;
  MaxConnections := CT_MaxClientsConnected;
  NetTcpIpClientClass := TBufferedNetTcpIpClient;
  Port := CT_NetServer_Port;
end;

procedure TNetServer.OnNewIncommingConnection(Sender : TObject; Client : TNetTcpIpClient);
Var n : TNetServerClient;
  DebugStep : String;
  tc : TTickCount;
begin
  DebugStep := '';
  Try
    if Not Client.Connected then exit;
    // NOTE: I'm in a separate thread
    // While in this function the ClientSocket connection will be active, when finishes the ClientSocket will be destroyed
    TLog.NewLog(ltInfo,Classname,'Starting ClientSocket accept '+Client.ClientRemoteAddr);
    n := TNetServerClient.Create(Nil);
    Try
      DebugStep := 'Assigning client';
      n.SetClient(Client);
      TNetData.NetData.IncStatistics(1,1,0,0,0,0);
      TNetData.NetData.NodeServersAddresses.CleanBlackList(False);
      DebugStep := 'Checking blacklisted';
      if (TNetData.NetData.NodeServersAddresses.IsBlackListed(Client.RemoteHost)) then begin
        // Invalid!
        TLog.NewLog(ltinfo,Classname,'Refusing Blacklist ip: '+Client.ClientRemoteAddr);
        n.SendError(ntp_autosend,CT_NetOp_Error, 0,CT_NetError_IPBlackListed,'Your IP is blacklisted:'+Client.ClientRemoteAddr);
        // Wait some time before close connection
        sleep(5000);
      end else begin
        DebugStep := 'Processing buffer and sleep...';
        while (n.Connected) And (Active) do begin
          n.DoProcessBuffer;
          Sleep(10);
        end;
      end;
    Finally
      Try
        TLog.NewLog(ltdebug,Classname,'Finalizing ServerAccept '+IntToHex(PtrInt(n),8)+' '+n.ClientRemoteAddr);
        DebugStep := 'Disconnecting NetServerClient';
        n.Connected := false;
        tc := TPlatform.GetTickCount;
        Repeat
          sleep(10); // 1.5.4 -> To prevent that not client disconnected (and not called OnDisconnect), increase sleep time
        Until (Not n.Connected) Or (tc + 5000 < TPlatform.GetTickCount);
        sleep(5);
        DebugStep := 'Assigning old client';
        n.SetClient( NetTcpIpClientClass.Create(Nil) );
        sleep(500); // Delay - Sleep time before destroying (1.5.3)
        DebugStep := 'Freeing NetServerClient';
      Finally
        n.Free;
      End;
    End;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Exception processing client thread at step: '+DebugStep+' - ('+E.ClassName+') '+E.Message);
    end;
  End;
end;

procedure TNetServer.SetActive(const Value: Boolean);
begin
  if Value then begin
    TLog.NewLog(ltinfo,Classname,'Activating server on port '+IntToStr(Port));
  end else begin
    TLog.NewLog(ltinfo,Classname,'Closing server');
  end;
  inherited;
  if Active then begin
    // TNode.Node.AutoDiscoverNodes(CT_Discover_IPs);
  end else if TNetData.NetDataExists then begin
    TNetData.NetData.DisconnectClients;
  end;
end;

procedure TNetServer.SetMaxConnections(AValue: Integer);
begin
  inherited SetMaxConnections(AValue);
  TNetData.NetData.FMaxConnections:=AValue;
end;

{ TNetConnection }

function TNetConnection.AddOperationsToBufferForSend(Operations: TOperationsHashTree): Integer;
Var i : Integer;
begin
  Result := 0;
  try
    FBufferLock.Acquire;
    Try
      for i := 0 to Operations.OperationsCount - 1 do begin
        if FBufferReceivedOperationsHash.IndexOf(Operations.GetOperation(i).Sha256)<0 then begin
          FBufferReceivedOperationsHash.Add(Operations.GetOperation(i).Sha256);
          If FBufferToSendOperations.IndexOfOperation(Operations.GetOperation(i))<0 then begin
            FBufferToSendOperations.AddOperationToHashTree(Operations.GetOperation(i));
            Inc(Result);
          end;
        end;
      end;
    finally
      FBufferLock.Release;
    end;
  Except
    On E:Exception do begin
      TLog.NewLog(ltError,ClassName,'Error at AddOperationsToBufferForSend ('+E.ClassName+'): '+E.Message);
      Result := 0;
    end;
  end;
end;

function TNetConnection.ClientRemoteAddr: String;
begin
  If Assigned(FTcpIpClient) then begin
    Result := FtcpIpClient.ClientRemoteAddr
  end else Result := 'NIL';
end;

function TNetConnection.ConnectTo(ServerIP: String; ServerPort: Word) : Boolean;
Var nsa : TNodeServerAddress;
  i : Integer;
begin
  If FIsConnecting then Exit;
  Try
    FIsConnecting:=True;
    if Client.Connected then Client.Disconnect;
    TPCThread.ProtectEnterCriticalSection(Self,FNetLock);
    Try
      Client.RemoteHost := ServerIP;
      if ServerPort<=0 then ServerPort := CT_NetServer_Port;
      Client.RemotePort := ServerPort;
      {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,Classname,'Trying to connect to a server at: '+ClientRemoteAddr);{$ENDIF}
      TNetData.NetData.NodeServersAddresses.GetNodeServerAddress(Client.RemoteHost,Client.RemotePort,true,nsa);
      nsa.netConnection := Self;
      TNetData.NetData.NodeServersAddresses.SetNodeServerAddress(nsa);
      TNetData.NetData.NotifyNetConnectionUpdated;
      Result := Client.Connect;
    Finally
      FNetLock.Release;
    End;
    if Result then begin
      {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,Classname,'Connected to a possible server at: '+ClientRemoteAddr);{$ENDIF}
      TNetData.NetData.NodeServersAddresses.GetNodeServerAddress(Client.RemoteHost,Client.RemotePort,true,nsa);
      nsa.netConnection := Self;
      nsa.last_connection_by_me := (UnivDateTimeToUnix(DateTime2UnivDateTime(now)));
      TNetData.NetData.NodeServersAddresses.SetNodeServerAddress(nsa);
      Result := Send_Hello(ntp_request,TNetData.NetData.NewRequestId);
    end else begin
      {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,Classname,'Cannot connect to a server at: '+ClientRemoteAddr);{$ENDIF}
    end;
  finally
    FIsConnecting:=False;
  end;
end;

constructor TNetConnection.Create(AOwner: TComponent);
begin
  inherited;
  FIsConnecting:=False;
  FIsDownloadingBlocks := false;
  FHasReceivedData := false;
  FNetProtocolVersion.protocol_version := 0; // 0 = unknown
  FNetProtocolVersion.protocol_available := 0;
  FAlertedForNewProtocolAvailable := false;
  FDoFinalizeConnection := false;
  FClientAppVersion := '';
  FClientPublicKey := CT_TECDSA_Public_Nul;
  FCreatedTime := Now;
  FIsMyselfServer := false;
  FTimestampDiff := 0;
  FIsWaitingForResponse := false;
  FClientBufferRead := TMemoryStream.Create;
  FNetLock := TPCCriticalSection.Create('TNetConnection_NetLock');
  FLastHelloTS := 0;
  FLastDataReceivedTS := 0;
  FLastDataSendedTS := 0;
  FRandomWaitSecondsSendHello := (CT_NewLineSecondsAvg DIV 3) + Random(CT_NewLineSecondsAvg DIV 2);
  FTcpIpClient := Nil;
  FRemoteOperationBlock := CT_OperationBlock_NUL;
  FRemoteAccumulatedWork := 0;
  SetClient( TBufferedNetTcpIpClient.Create(Self) );
  TNetData.NetData.FNetConnections.Add(Self);
  TNetData.NetData.NotifyNetConnectionUpdated;
  FBufferLock := TPCCriticalSection.Create('TNetConnection_BufferLock');
  FBufferReceivedOperationsHash := TOrderedRawList.Create;
  FBufferToSendOperations := TOperationsHashTree.Create;
  FClientTimestampIp := '';
end;

destructor TNetConnection.Destroy;
begin
  Try
    {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,'Destroying '+Classname+' '+IntToHex(PtrInt(Self),8));{$ENDIF}
    Connected := false;
  Finally
    TNetData.NetData.NodeServersAddresses.DeleteNetConnection(Self);
    TNetData.NetData.FNetConnections.Remove(Self);
  End;
  TNetData.NetData.UnRegisterRequest(Self,0,0);
  Try
    TNetData.NetData.NotifyNetConnectionUpdated;
  Finally
    FreeAndNil(FNetLock);
    FreeAndNil(FClientBufferRead);
    FreeAndNil(FTcpIpClient);
    FreeAndNil(FBufferLock);
    FreeAndNil(FBufferReceivedOperationsHash);
    FreeAndNil(FBufferToSendOperations);
    inherited;
  End;
end;

procedure TNetConnection.DisconnectInvalidClient(ItsMyself : Boolean; const why: String);
Var include_in_list : Boolean;
  ns : TNodeServerAddress;
  aux_s : String;
begin
  FIsDownloadingBlocks := false;
  if ItsMyself then begin
    TLog.NewLog(ltInfo,Classname,'Disconecting myself '+ClientRemoteAddr+' > '+Why)
  end else begin
    TLog.NewLog(lterror,Classname,'Disconecting '+ClientRemoteAddr+' > '+Why);
  end;
  FIsMyselfServer := ItsMyself;
  aux_s := Client.RemoteHost;
  include_in_list := (Not SameText(aux_s,'localhost')) And (Not SameText('127.',aux_s.Substring(0,4)))
    And (Not SameText('192.168.',aux_s.Substring(0,8)))
    And (Not SameText('10.',aux_s.Substring(0,3)));
  if include_in_list then begin
    If TNetData.NetData.NodeServersAddresses.GetNodeServerAddress(Client.RemoteHost,Client.RemotePort,true,ns) then begin
      ns.last_connection := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
      ns.its_myself := ItsMyself;
      ns.BlackListText := Why;
      ns.is_blacklisted := true;
      TNetData.NetData.NodeServersAddresses.SetNodeServerAddress(ns);
    end;
  end else if ItsMyself then begin
    If TNetData.NetData.NodeServersAddresses.GetNodeServerAddress(Client.RemoteHost,Client.RemotePort,true,ns) then begin
      ns.its_myself := ItsMyself;
      TNetData.NetData.NodeServersAddresses.SetNodeServerAddress(ns);
    end;
  end;
  TNetData.NetData.IpInfos.LogDisconnect(Client.RemoteHost,ClientRemoteAddr+' '+Why,ItsMyself);
  Connected := False;
  TNetData.NetData.NotifyBlackListUpdated;
  TNetData.NetData.NotifyNodeServersUpdated;
end;

procedure TNetConnection.DoProcessBuffer;
Var HeaderData : TNetHeaderData;
  ms : TMemoryStream;
  ops : String;
  iPending : Integer;
begin
  if FDoFinalizeConnection then begin
    if Connected then begin
      TLog.NewLog(ltdebug,Classname,'Executing DoFinalizeConnection at client '+ClientRemoteAddr);
      Connected := false;
    end;
  end;
  if Not Connected then exit;
  ms := TMemoryStream.Create;
  try
    if Not FIsWaitingForResponse then begin
      DoSendAndWaitForResponse(0,0,Nil,ms,0,HeaderData);
    end;
  finally
    ms.Free;
  end;
  If ((FLastDataReceivedTS>0) Or ( NOT (Self is TNetServerClient)))
    and (TPlatform.GetElapsedMilliseconds(FLastHelloTS)>(1000*FRandomWaitSecondsSendHello)) then begin
    iPending := TNetData.NetData.PendingRequest(Self,ops);
    If iPending>=2 then begin
      TLog.NewLog(ltDebug,Classname,'Pending requests without response... closing connection to '+ClientRemoteAddr+' > '+ops);
      Connected := false;
    end else begin
      if iPending>0 then begin
        TLog.NewLog(ltDebug,Classname,'Sending Hello to check connection to '+ClientRemoteAddr+' > '+ops);
      end;
      Send_Hello(ntp_request,TNetData.NetData.NewRequestId);
    end;
  end else if (Self is TNetServerClient) AND (FLastDataReceivedTS=0) And (FCreatedTime+EncodeTime(0,1,0,0)<Now) then begin
    // Disconnecting client without data...
    TLog.NewLog(ltDebug,Classname,'Disconnecting client without data '+ClientRemoteAddr);
    Connected := false;
  end;
end;

procedure TNetConnection.DoProcess_AddOperations(HeaderData: TNetHeaderData; DataBuffer: TStream);
var c,i : Integer;
    optype : Byte;
    opclass : TPCOperationClass;
    op : TPCOperation;
    operations : TOperationsHashTree;
    errors : String;
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
      Try
        op.LoadFromNettransfer(DataBuffer);
        operations.AddOperationToHashTree(op);
      Finally
        op.Free;
      End;
    end;
    DoDisconnect := false;
  finally
    try
      if DoDisconnect then begin
        DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
      end else begin
        // Add to received buffer
        FBufferLock.Acquire;
        Try
          for i := 0 to operations.OperationsCount - 1 do begin
            op := operations.GetOperation(i);
            FBufferReceivedOperationsHash.Add(op.Sha256);
            c := FBufferToSendOperations.IndexOfOperation(op);
            if (c>=0) then begin
              FBufferToSendOperations.Delete(c);
            end;
          end;
        Finally
          FBufferLock.Release;
        End;
        TNode.Node.AddOperations(Self,operations,Nil,errors);
      end;
    finally
      operations.Free;
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetBlockchainOperations_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
  {
  As described on PIP-0015 this will return Operations stored in a specified Block of the Blockchain
  Input:
    operations_count : 4 bytes
    foreach operations_count
      BLOCK_OP_REF : 8 bytes -> BLOCK_OP_REF = (QWord(BlockNumber) SHL 32) BIT-OR QWord(OperationIndex)
  Output:
    operations_count : 4 bytes -> Must match input.operations_count
    foreach operations_count
      op_size : 4 bytes
      op_data : (op_size) bytes
  }
  function GetBlock(bufferOperationsBlock : TList<TPCOperationsComp>; nBlock : Integer) : TPCOperationsComp;
  var i : Integer;
  begin
    // Search at buffer:
    i := 0; Result := Nil;
    while (i<bufferOperationsBlock.Count) And (TPCOperationsComp( bufferOperationsBlock[i] ).OperationBlock.block <> nBlock) do inc(i);
    if (i>=bufferOperationsBlock.Count) then begin
      // Not found !
      Result := TPCOperationsComp.Create(Nil);
      if Not TNode.Node.Bank.LoadOperations(Result,nBlock) then FreeAndNil(Result)
      else bufferOperationsBlock.Add(Result); // Memory leak on v4.0.0
    end else Result := TPCOperationsComp( bufferOperationsBlock[i] );
  end;
Var input_operations_count, cBlock, cBlockOpIndex, c : Cardinal;
  block_op_ref : UInt64;
  i : Integer;
  bufferOperationsBlock : TList<TPCOperationsComp>;
  opc : TPCOperationsComp;
  outputBuffer : TStream;
  opindexdata : TStream;
  opsdata : TBytes;
  errors : String;
  DoDisconnect : Boolean;

begin
  errors := 'Invalid GetBlockchainOperations_Request structure';
  DoDisconnect := true;
  outputBuffer := TMemoryStream.Create;
  try
    if HeaderData.header_type<>ntp_request then begin
      errors := 'Not request';
      Exit;
    end;
    if DataBuffer.Read(input_operations_count,SizeOf(input_operations_count))<>SizeOf(input_operations_count) then Exit;
    if (input_operations_count>CT_MAX_OPS_PER_BLOCKCHAINOPERATIONS) then begin
      errors := Format('Inputs %d > %d',[input_operations_count,CT_MAX_OPS_PER_BLOCKCHAINOPERATIONS]);
      Exit;
    end;
    outputBuffer.Write(input_operations_count,SizeOf(input_operations_count));
    bufferOperationsBlock := TList<TPCOperationsComp>.Create;
    opindexdata := TStream.Create;
    Try
      for i := 1 to input_operations_count do begin
        if DataBuffer.Read(block_op_ref,SizeOf(block_op_ref))<>SizeOf(block_op_ref) then begin
          errors := Format('Cannot read enough data at pos %d/%d',[i,input_operations_count]);
          Exit; // read 8 bytes
        end;
        cBlock := block_op_ref SHR 32;
        cBlockOpIndex := Cardinal(block_op_ref AND ($00000000FFFFFFFF));
        opc := GetBlock(bufferOperationsBlock, cBlock);
        if Assigned(opc) then begin
          if (cBlockOpIndex<opc.Count) then begin
            opsdata := opc.Operation[cBlockOpIndex].GetOperationStreamData;
            c := Length(opsdata);
            outputBuffer.Write(c,SizeOf(c));
            outputBuffer.WriteBuffer(opsdata[0],Length(opsdata)); // Fixed bug 4.0.0
            SetLength(opsdata,0);
          end else begin
            // OpIndex not found on block -> Add NIL reference: data 0 size = No operation
            c := 0;
            outputBuffer.Write(c,SizeOf(c));
          end;
        end else begin
          // Block operation not found -> Add NIL reference: data 0 size = No operation
          c := 0;
          outputBuffer.Write(c,SizeOf(c));
        end;
      end;
      DoDisconnect := False;
      // Send back
      outputBuffer.Position := 0;
      Send(ntp_response,HeaderData.operation,0,HeaderData.request_id,outputBuffer);
    Finally
      opindexdata.Free;
      for i := 0 to bufferOperationsBlock.Count-1 do begin
        TPCOperationsComp(bufferOperationsBlock[i]).Free;
      end;
      bufferOperationsBlock.Free;
    End;
  finally
    outputBuffer.Free;
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetBlocks_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
Var b,b_start,b_end:Cardinal;
    op : TPCOperationsComp;
    db : TMemoryStream;
    c : Cardinal;
  errors : String;
  DoDisconnect : Boolean;
  posquantity : Int64;
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
     if (b_end>=TNetData.NetData.Bank.BlocksCount) then begin
       errors := Format('b_end:%d >= current block:%d b_start:%d',[b_end,TNetData.NetData.Bank.BlocksCount,b_start]);
       b_end := TNetData.NetData.Bank.BlocksCount-1;
       if (b_start>b_end) then begin
         // No data:
         db := TMemoryStream.Create;
         try
           c := 0;
           db.Write(c,4);
           Send(ntp_response,HeaderData.operation,0,HeaderData.request_id,db);
           Exit;
         finally
           db.Free;
         end;
       end;
     end;

     DoDisconnect := false;

     db := TMemoryStream.Create;
     try
       op := TPCOperationsComp.Create(TNetData.NetData.bank);
       try
         c := b_end - b_start + 1;
         posquantity := db.position;
         db.Write(c,4);
         c := 0;
         b := b_start;
         for b := b_start to b_end do begin
           inc(c);
           If TNetData.NetData.bank.LoadOperations(op,b) then begin
             op.SaveBlockToStream(false,db);
           end else begin
             SendError(ntp_response,HeaderData.operation,HeaderData.request_id,CT_NetError_InternalServerError,'Operations of block:'+inttostr(b)+' not found');
             exit;
           end;
           // Build 1.0.5 To prevent high data over net in response (Max 2 Mb of data)
           if (db.size>(1024*1024*2)) then begin
             // Stop
             db.position := posquantity;
             db.Write(c,4);
             // BUG of Build 1.0.5 !!! Need to break bucle OH MY GOD!
             db.Position := db.Size;
             break;
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
  var op : TPCOperationsComp;
    opcount,i : Cardinal;
    newBlockAccount : TBlockAccount;
  errors : String;
  DoDisconnect : Boolean;
begin
  DoDisconnect := true;
  try
    if HeaderData.header_type<>ntp_response then begin
      errors := 'Not response';
      exit;
    end;
    If HeaderData.is_error then begin
      DoDisconnect := false;
      exit; //
    end;
    // DataBuffer contains: from and to
    errors := 'Invalid structure';
    op := TPCOperationsComp.Create(nil);
    Try
      op.bank := TNode.Node.Bank;
      if DataBuffer.Size-DataBuffer.Position<4 then begin
        DisconnectInvalidClient(false,'DoProcess_GetBlocks_Response invalid format: '+errors);
        exit;
      end;
      DataBuffer.Read(opcount,4);
      DoDisconnect :=false;
      for I := 1 to opcount do begin
        if Not op.LoadBlockFromStream(DataBuffer,errors) then begin
           errors := 'Error decoding block '+inttostr(i)+'/'+inttostr(opcount)+' Errors:'+errors;
           DoDisconnect := true;
           exit;
        end;
        if (op.OperationBlock.block=TNode.Node.Bank.BlocksCount) then begin
          TNode.Node.MarkVerifiedECDSASignaturesFromMemPool(op); // Improvement speed v4.0.2
          if (TNode.Node.Bank.AddNewBlockChainBlock(op,TNetData.NetData.NetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock, newBlockAccount,errors)) then begin
            // Ok, one more!
          end else begin
            // Is not a valid entry????
            // Perhaps an orphan blockchain: Me or Client!
            TLog.NewLog(ltinfo,Classname,'Distinct operation block found! My:'+
                TPCOperationsComp.OperationBlockToText(TNode.Node.Bank.SafeBox.Block(TNode.Node.Bank.BlocksCount-1).blockchainInfo)+
                ' remote:'+TPCOperationsComp.OperationBlockToText(op.OperationBlock)+' Errors: '+errors);
          end;
        end else begin
          // Receiving an unexpected operationblock
          TLog.NewLog(lterror,classname,'Received a distinct block, finalizing: '+TPCOperationsComp.OperationBlockToText(op.OperationBlock)+' (My block: '+TPCOperationsComp.OperationBlockToText(TNode.Node.Bank.LastOperationBlock)+')' );
          FIsDownloadingBlocks := false;
          exit;
        end;
        sleep(1);
      end;
      FIsDownloadingBlocks := false;
      if ((opcount>0) And (FRemoteOperationBlock.block>=TNode.Node.Bank.BlocksCount)) then begin
        Send_GetBlocks(TNode.Node.Bank.BlocksCount,100,i);
      end else begin
        // No more blocks to download, download Pending operations
        DoProcess_GetPendingOperations;
      end;
      TNode.Node.NotifyBlocksChanged;
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
  db,msops : TMemoryStream;
  errors, blocksstr : String;
  DoDisconnect : Boolean;
  ob : TOperationBlock;
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
    if (b_start<0) Or (b_start>b_end) then begin
      errors := 'Invalid start ('+Inttostr(b_start)+') or end ('+Inttostr(b_end)+') of count ('+Inttostr(TNode.Node.Bank.BlocksCount)+')';
      exit;
    end;

    DoDisconnect := false;

    if (b_start>=TNode.Node.Bank.BlocksCount) then begin
      SendError(ntp_response,HeaderData.operation,HeaderData.request_id,CT_NetError_NotFound,Format('Block %d not found',[b_start]));
      Exit;
    end;

    if (b_end>=TNode.Node.Bank.BlocksCount) then b_end := TNode.Node.Bank.BlocksCount-1;
    inc_b := ((b_end - b_start) DIV CT_Max_Positions)+1;
    msops := TMemoryStream.Create;
    try
      b := b_start;
      total_b := 0;
      repeat
        ob := TNode.Node.Bank.SafeBox.Block(b).blockchainInfo;
        If TPCOperationsComp.SaveOperationBlockToStream(ob,msops) then begin
          blocksstr := blocksstr + inttostr(b)+',';
          b := b + inc_b;
          inc(total_b);
        end else begin
          errors := 'ERROR DEV 20170522-1 block:'+inttostr(b);
          SendError(ntp_response,HeaderData.operation,HeaderData.request_id,CT_NetError_InternalServerError,errors);
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
    end;
    TLog.NewLog(ltdebug,Classname,'Sending '+inttostr(total_b)+' operations block from block '+inttostr(b_start)+' to '+inttostr(b_end)+' '+blocksstr);
  finally
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetSafeBox_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
Var _blockcount : Cardinal;
    _safeboxHash : TRawBytes;
    _from,_to : Cardinal;
  sbStream : TStream;
  responseStream : TStream;
  antPos : Int64;
  sbHeader : TPCSafeBoxHeader;
  errors : String;
begin
  {
  This call is used to obtain a chunk of the safebox
  Request:
  BlockCount (4 bytes) - The safebox checkpoint
  SafeboxHash (TRawBytes) - The safeboxhash of that checkpoint
  StartPos (4 bytes) - The start index (0..BlockCount-1)
  EndPos   (4 bytes) - The final index (0..BlockCount-1)
    If valid info:
      - If available will return a LZIP chunk of safebox
      - If not available (requesting for an old safebox) will retun not available
    If not valid will disconnect
  }
  DataBuffer.Read(_blockcount,SizeOf(_blockcount));
  TStreamOp.ReadAnsiString(DataBuffer,_safeboxHash);
  DataBuffer.Read(_from,SizeOf(_from));
  DataBuffer.Read(_to,SizeOf(_to));
  // Protections:
  if (_from>_to) Or (_from + CT_MAX_SAFEBOXCHUNK_BLOCKS <= _to) then begin
    DisconnectInvalidClient(False,Format('Invalid GetSafebox values on request. From:%d to:%d',[_from,_to]));
    Exit;
  end;
  //
  sbStream := TNode.Node.Bank.Storage.CreateSafeBoxStream(_blockcount);
  try
    responseStream := TMemoryStream.Create;
    try
      If Not Assigned(sbStream) then begin
        SendError(ntp_response,HeaderData.operation,CT_NetError_SafeboxNotFound,HeaderData.request_id,Format('Safebox for block %d not found',[_blockcount]));
        exit;
      end;
      antPos := sbStream.Position;
      TPCSafeBox.LoadSafeBoxStreamHeader(sbStream,sbHeader);
      If sbHeader.safeBoxHash<>_safeboxHash then begin
        DisconnectInvalidClient(false,Format('Invalid safeboxhash on GetSafeBox request (Real:%s > Requested:%s)',[TCrypto.ToHexaString(sbHeader.safeBoxHash),TCrypto.ToHexaString(_safeboxHash)]));
        exit;
      end;
      // Response:
      sbStream.Position:=antPos;
      If not TPCChunk.SaveSafeBoxChunkFromSafeBox(sbStream,responseStream,_from,_to,errors) then begin
        TLog.NewLog(ltError,Classname,'Error saving chunk: '+errors);
        exit;
      end;
      // Sending
      Send(ntp_response,HeaderData.operation,0,HeaderData.request_id,responseStream);
      TLog.NewLog(ltInfo,ClassName,Format('Sending Safebox(%d) chunk[%d..%d] to %s Bytes:%d',[_blockcount,_from,_to,ClientRemoteAddr,responseStream.Size]));
    finally
      responseStream.Free;
    end;
  finally
    FreeAndNil(sbStream);
  end;
end;

procedure TNetConnection.DoProcess_GetPendingOperations_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
var responseStream : TMemoryStream;
  i,start,max : Integer;
  b : Byte;
  c : Cardinal;
  DoDisconnect : Boolean;
  errors : String;
  opht : TOperationsHashTree;
begin
  {
  This call is used to obtain pending operations not included in blockchain
  Request:
  - Request type (1 byte) - Values
    - Value 1:
      Returns Count
    - Value 2:
      - start (4 bytes)
      - max (4 bytes)
      Returns Pending operations (from start to start+max) in a TOperationsHashTree Stream
  }
  errors := '';
  DoDisconnect := true;
  responseStream := TMemoryStream.Create;
  try
    if HeaderData.header_type<>ntp_request then begin
      errors := 'Not request';
      exit;
    end;
    DataBuffer.Read(b,1);
    if (b=1) then begin
      // Return count
      c := TNode.Node.Operations.Count;
      responseStream.Write(c,SizeOf(c));
    end else if (b=2) then begin
      // Return from start to start+max
      DataBuffer.Read(c,SizeOf(c)); // Start 4 bytes
      start:=c;
      DataBuffer.Read(c,SizeOf(c)); // max 4 bytes
      max:=c;
      //
      if (start<0) Or (max<0) then begin
        errors := 'Invalid start/max value';
        Exit;
      end;
      opht := TOperationsHashTree.Create;
      Try
        TNode.Node.Operations.Lock;
        Try
          if (start >= TNode.Node.Operations.Count) Or (max=0) then begin
          end else begin
            if (start + max >= TNode.Node.Operations.Count) then max := TNode.Node.Operations.Count - start;
            for i:=start to (start + max -1) do begin
              opht.AddOperationToHashTree(TNode.Node.Operations.OperationsHashTree.GetOperation(i));
            end;
          end;
        finally
          TNode.Node.Operations.Unlock;
        end;
        opht.SaveOperationsHashTreeToStream(responseStream,False);
      Finally
        opht.Free;
      End;
    end else begin
      errors := 'Invalid call type '+inttostr(b);
      Exit;
    end;
    DoDisconnect:=False;
    Send(ntp_response,HeaderData.operation,0,HeaderData.request_id,responseStream);
  finally
    responseStream.Free;
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetPendingOperations;
Var dataSend, dataReceived : TMemoryStream;
  request_id, cStart, cMax, cTotal, cTotalByOther, cReceived, cAddedOperations : Cardinal;
  b : Byte;
  headerData : TNetHeaderData;
  opht : TOperationsHashTree;
  errors : String;
  i : Integer;
begin
  {$IFDEF PRODUCTION}
  If FNetProtocolVersion.protocol_available<=6 then Exit; // Note: GetPendingOperations started on protocol_available=7
  {$ENDIF}
  request_id := 0;
  cAddedOperations := 0;
  if Not Connected then exit;
  // First receive operations from
  dataSend := TMemoryStream.Create;
  dataReceived := TMemoryStream.Create;
  try
    b := 1;
    dataSend.Write(b,1);
    request_id := TNetData.NetData.NewRequestId;
    If Not DoSendAndWaitForResponse(CT_NetOp_GetPendingOperations,request_id,dataSend,dataReceived,20000,headerData) then begin
      Exit;
    end;
    dataReceived.Position:=0;
    cTotalByOther := 0;
    If (dataReceived.Read(cTotalByOther,SizeOf(cTotal))<SizeOf(cTotal)) then begin
      DisconnectInvalidClient(False,'Invalid data returned on GetPendingOperations');
      Exit;
    end;
    cTotal := cTotalByOther;
    if (cTotal>5000) then begin
      // Limiting max pending operations to 5000
      cTotal := 5000;
    end;
    cReceived:=0;
    cStart := 0;
    While (Connected) And (cReceived<cTotal) do begin
      dataSend.Clear;
      dataReceived.Clear;
      b := 2;
      dataSend.Write(b,1);
      dataSend.Write(cStart,SizeOf(cStart));
      cMax := 1000;  // Limiting in 1000 by round
      dataSend.Write(cMax,SizeOf(cMax));
      request_id := TNetData.NetData.NewRequestId;
      If Not DoSendAndWaitForResponse(CT_NetOp_GetPendingOperations,request_id,dataSend,dataReceived,50000,headerData) then begin
        Exit;
      end;
      dataReceived.Position:=0;
      //
      opht := TOperationsHashTree.Create;
      try
        If Not opht.LoadOperationsHashTreeFromStream(dataReceived,False,0,Nil,errors) then begin
          DisconnectInvalidClient(False,'Invalid operations hash tree stream: '+errors);
          Exit;
        end;
        If (opht.OperationsCount>0) then begin
          inc(cReceived,opht.OperationsCount);
          i := TNode.Node.AddOperations(Self,opht,Nil,errors);
          inc(cAddedOperations,i);
        end else Break; // No more
        inc(cStart,opht.OperationsCount);
      finally
        opht.Free;
      end;
    end;
    TLog.NewLog(ltInfo,Classname,Format('Processed GetPendingOperations to %s obtaining %d (available %d) operations and added %d to Node',
      [Self.ClientRemoteAddr,cTotal,cTotalByOther,cAddedOperations]));
  finally
    dataSend.Free;
    dataReceived.Free;
  end;
end;

procedure TNetConnection.DoProcess_GetPubkeyAccounts_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
Const CT_Max_Accounts_per_call = 1000;
var responseStream, accountsStream : TMemoryStream;
  start,max,iPubKey : Integer;
  c, nAccounts : Cardinal;
  acc : TAccount;
  DoDisconnect : Boolean;
  errors : String;
  pubKey : TAccountKey;
  sbakl : TOrderedAccountKeysList;
  ocl : TOrderedCardinalList;
begin
  {
  This call is used to obtain Accounts used by a Public key
    - Also will return current node block number
    - If a returned data has updated_block value = (current block+1) that means that Account is currently affected by a pending operation in the pending operations
  Request fields
    - Public key
    - start position
    - max
  Returns:
    - current block number (4 bytes): Note, if an account has updated_block > current block means that has been updated and is in pending state
    - count (4 bytes)
    - for 1 to count:  TAccountComp.SaveAccountToAStream
  }
  errors := '';
  DoDisconnect := True;
  responseStream := TMemoryStream.Create;
  accountsStream := TMemoryStream.Create;
  try
    // Response first 4 bytes are current block number
    c := TNode.Node.Bank.BlocksCount-1;
    responseStream.Write(c,SizeOf(c));
    //
    if HeaderData.header_type<>ntp_request then begin
      errors := 'Not request';
      Exit;
    end;
    if TStreamOp.ReadAccountKey(DataBuffer,pubKey)<0 then begin
      errors := 'Invalid public key';
      Exit;
    end;
    DataBuffer.Read(c,SizeOf(c));
    start:=c;
    DataBuffer.Read(c,SizeOf(c));
    max:=c;
    If max>CT_Max_Accounts_per_call then max := CT_Max_Accounts_per_call;
    if (start<0) Or (max<0) then begin
      errors := 'Invalid start/max value';
      Exit;
    end;
    //
    nAccounts := 0;
    sbakl := TNode.Node.Bank.SafeBox.OrderedAccountKeysList;
    if Assigned(sbakl) then begin
      iPubKey := sbakl.IndexOfAccountKey(pubKey);
      if (iPubKey>=0) then begin
        ocl := sbakl.AccountKeyList[iPubKey];
        while (start<ocl.Count) And (max>0) do begin
          acc := TNode.Node.Operations.SafeBoxTransaction.Account(ocl.Get(start));
          TAccountComp.SaveAccountToAStream(accountsStream,acc);
          inc(nAccounts);
          inc(start);
          dec(max);
        end;
      end;
      // Save & send
      responseStream.Write(nAccounts,SizeOf(nAccounts));  // nAccounts = 4 bytes
      responseStream.CopyFrom(accountsStream,0); // Copy all
      DoDisconnect := False;
      Send(ntp_response,HeaderData.operation,0,HeaderData.request_id,responseStream);
    end else begin
      DoDisconnect := False;
      SendError(ntp_response,HeaderData.operation,HeaderData.request_id,CT_NetError_NotAvailable,'No OrderedAccountKeysList available');
    end;
  finally
    responseStream.Free;
    accountsStream.Free;
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_GetAccount_Request(HeaderData: TNetHeaderData; DataBuffer: TStream);
Const CT_Max_Accounts_per_call = 1000;
var responseStream : TMemoryStream;
  i,start,max : Integer;
  b : Byte;
  c : Cardinal;
  acc : TAccount;
  DoDisconnect : Boolean;
  errors : String;
begin
  {
  This call is used to obtain an Account data
    - Also will return current node block number
    - If a returned data has updated_block value = (current block+1) that means that Account is currently affected by a pending operation in the pending operations
  Request:
  Request type (1 byte) - Values
    - Value 1: Single account
    - Value 2: From account start to start+max  LIMITED AT MAX 1000
    - Value 3: Multiple accounts LIMITED AT MAX 1000
  On 1:
    - account (4 bytes)
  On 2:
    - start (4 bytes)
    - max (4 bytes)
  On 3:
    - count (4 bytes)
    - for 1 to count read account (4 bytes)
  Returns:
  - current block number (4 bytes): Note, if an account has updated_block > current block means that has been updated and is in pending state
  - count (4 bytes)
  - for 1 to count:  TAccountComp.SaveAccountToAStream
  }
  errors := '';
  DoDisconnect := true;
  responseStream := TMemoryStream.Create;
  try
    // Response first 4 bytes are current block number
    c := TNode.Node.Bank.BlocksCount-1;
    responseStream.Write(c,SizeOf(c));
    //
    if HeaderData.header_type<>ntp_request then begin
      errors := 'Not request';
      exit;
    end;
    if (DataBuffer.Size-DataBuffer.Position<5) then begin
      errors := 'Invalid structure';
      exit;
    end;
    DataBuffer.Read(b,1);
    if (b in [1,2]) then begin
      if (b=1) then begin
        DataBuffer.Read(c,SizeOf(c));
        start:=c;
        max:=1; // Bug 3.0.1 (was c instead of fixed 1)
      end else begin
        DataBuffer.Read(c,SizeOf(c));
        start:=c;
        DataBuffer.Read(c,SizeOf(c));
        max:=c;
      end;
      If max>CT_Max_Accounts_per_call then max := CT_Max_Accounts_per_call;
      if (start<0) Or (max<0) then begin
        errors := 'Invalid start/max value';
        Exit;
      end;
      if (start >= TNode.Node.Bank.AccountsCount) Or (max=0) then begin
        c := 0;
        responseStream.Write(c,SizeOf(c));
      end else begin
        if (start + max >= TNode.Node.Bank.AccountsCount) then max := TNode.Node.Bank.AccountsCount - start;
        c := max;
        responseStream.Write(c,SizeOf(c));
        for i:=start to (start + max -1) do begin
          acc := TNode.Node.Operations.SafeBoxTransaction.Account(i);
          TAccountComp.SaveAccountToAStream(responseStream,acc);
        end;
      end;
    end else if (b=3) then begin
      DataBuffer.Read(c,SizeOf(c));
      if (c>CT_Max_Accounts_per_call) then c := CT_Max_Accounts_per_call;
      responseStream.Write(c,SizeOf(c));
      max := c;
      for i:=1 to max do begin
        DataBuffer.Read(c,SizeOf(c));
        if (c>=0) And (c<TNode.Node.Bank.AccountsCount) then begin
          acc := TNode.Node.Operations.SafeBoxTransaction.Account(c);
          TAccountComp.SaveAccountToAStream(responseStream,acc);
        end else begin
          errors := 'Invalid account number '+Inttostr(c);
          Exit;
        end;
      end;
    end else begin
      errors := 'Invalid call type '+inttostr(b);
      Exit;
    end;
    DoDisconnect:=False;
    Send(ntp_response,HeaderData.operation,0,HeaderData.request_id,responseStream);
  finally
    responseStream.Free;
    if DoDisconnect then begin
      DisconnectInvalidClient(false,errors+' > '+TNetData.HeaderDataToText(HeaderData)+' BuffSize: '+inttostr(DataBuffer.Size));
    end;
  end;
end;

procedure TNetConnection.DoProcess_Hello(HeaderData: TNetHeaderData; DataBuffer: TStream);
var op, myLastOp : TPCOperationsComp;
    errors : String;
    connection_has_a_server : Word;
    i,c : Integer;
    nsa : TNodeServerAddress;
    rid : Cardinal;
    connection_ts : Cardinal;
   Duplicate : TNetConnection;
   RawAccountKey : TRawBytes;
   other_version : String;
   isFirstHello : Boolean;
   lastTimestampDiff : Integer;
Begin
  FRemoteAccumulatedWork := 0;
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
    lastTimestampDiff := FTimestampDiff;
    FTimestampDiff := Integer( Int64(connection_ts) - Int64(TNetData.NetData.NetworkAdjustedTime.GetAdjustedTime) );
    If FClientTimestampIp='' then begin
      isFirstHello := True;
      FClientTimestampIp := FTcpIpClient.RemoteHost;
      TNetData.NetData.NetworkAdjustedTime.AddNewIp(FClientTimestampIp,connection_ts);
      if (Abs(TNetData.NetData.NetworkAdjustedTime.TimeOffset)>CT_MaxFutureBlockTimestampOffset) then begin
        TNode.Node.NotifyNetClientMessage(Nil,'The detected network time is different from this system time in '+
          IntToStr(TNetData.NetData.NetworkAdjustedTime.TimeOffset)+' seconds! Please check your local time/timezone');
      end;
      if (Abs(FTimestampDiff) > CT_MaxFutureBlockTimestampOffset) then begin
        TLog.NewLog(ltDebug,ClassName,'Detected a node ('+ClientRemoteAddr+') with incorrect timestamp: '+IntToStr(connection_ts)+' offset '+IntToStr(FTimestampDiff) );
      end;
    end else begin
      isFirstHello := False;
      TNetData.NetData.NetworkAdjustedTime.UpdateIp(FClientTimestampIp,connection_ts);
    end;
    If (Abs(lastTimestampDiff) > CT_MaxFutureBlockTimestampOffset) And (Abs(FTimestampDiff) <= CT_MaxFutureBlockTimestampOffset) then begin
      TLog.NewLog(ltDebug,ClassName,'Corrected timestamp for node ('+ClientRemoteAddr+') old offset: '+IntToStr(lastTimestampDiff)+' current offset '+IntToStr(FTimestampDiff) );
    end;

    if (connection_has_a_server>0) And (Not SameText(Client.RemoteHost,'localhost')) And (Not SameText('127.',Client.RemoteHost.Substring(0,4)))
      And (Not SameText('192.168.',Client.RemoteHost.Substring(0,8)))
      And (Not SameText('10.',Client.RemoteHost.Substring(0,3)))
      And (Not TAccountComp.EqualAccountKeys(FClientPublicKey,TNetData.NetData.FNodePrivateKey.PublicKey)) then begin
      nsa := CT_TNodeServerAddress_NUL;
      nsa.ip := Client.RemoteHost;
      nsa.port := connection_has_a_server;
      nsa.last_connection := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
      TNetData.NetData.AddServer(nsa);
    end;

    if op.LoadBlockFromStream(DataBuffer,errors) then begin
      FRemoteOperationBlock := op.OperationBlock;
      if (DataBuffer.Size-DataBuffer.Position>=4) then begin
        DataBuffer.Read(c,4);
        for i := 1 to c do begin
          nsa := CT_TNodeServerAddress_NUL;
          TStreamOp.ReadString(DataBuffer,nsa.ip);
          DataBuffer.Read(nsa.port,2);
          DataBuffer.Read(nsa.last_connection_by_server,4);
          If (nsa.last_connection_by_server>0) And (i<=CT_MAX_NODESERVERS_ON_HELLO) then // Protect massive data
            TNetData.NetData.AddServer(nsa);
        end;
        if TStreamOp.ReadString(DataBuffer,other_version)>=0 then begin
          // Captures version
          ClientAppVersion := other_version;
          if (DataBuffer.Size-DataBuffer.Position>=SizeOf(FRemoteAccumulatedWork)) then begin
            DataBuffer.Read(FRemoteAccumulatedWork,SizeOf(FRemoteAccumulatedWork));
            TLog.NewLog(ltdebug,ClassName,'Received HELLO with height: '+inttostr(op.OperationBlock.block)+' Accumulated work '+IntToStr(FRemoteAccumulatedWork)+ ' Remote block: '+TPCOperationsComp.OperationBlockToText(FRemoteOperationBlock));
          end;
        end;
        //
        if (FRemoteAccumulatedWork>TNode.Node.Bank.SafeBox.WorkSum) Or
          ((FRemoteAccumulatedWork=0) And (TNetData.NetData.FMaxRemoteOperationBlock.block<FRemoteOperationBlock.block)) then begin
          TNetData.NetData.FMaxRemoteOperationBlock := FRemoteOperationBlock;
          if TPCThread.ThreadClassFound(TThreadGetNewBlockChainFromClient,nil)<0 then begin
            TThreadGetNewBlockChainFromClient.Create;
          end;
        end;
      end;

      FLastHelloTS:=TPlatform.GetTickCount;
      FRandomWaitSecondsSendHello := (CT_NewLineSecondsAvg DIV 3) + Random(CT_NewLineSecondsAvg DIV 2);

      {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Hello received: '+TPCOperationsComp.OperationBlockToText(FRemoteOperationBlock));{$ENDIF}
      if (HeaderData.header_type in [ntp_request,ntp_response]) then begin
        // Response:
        if (HeaderData.header_type=ntp_request) then begin
          Send_Hello(ntp_response,HeaderData.request_id);
        end;

        // Protection of invalid timestamp when is a new incoming connection due to wait time
        if (isFirstHello) And (Self is TNetServerClient) and (HeaderData.header_type=ntp_request) and (Abs(FTimestampDiff) > CT_MaxFutureBlockTimestampOffset) then begin
          TLog.NewLog(ltDebug,ClassName,'Sending HELLO again to ('+ClientRemoteAddr+') in order to check invalid current Timestamp offset: '+IntToStr(FTimestampDiff) );
          Send_Hello(ntp_request,TNetData.NetData.NewRequestId);
        end;

        if (TAccountComp.EqualAccountKeys(FClientPublicKey,TNetData.NetData.FNodePrivateKey.PublicKey)) then begin
          DisconnectInvalidClient(true,'MySelf disconnecting...');
          exit;
        end;
        Duplicate := TNetData.NetData.FindConnectionByClientRandomValue(Self);
        if (Duplicate<>Nil) And (Duplicate.Connected) then begin
          DisconnectInvalidClient(true,'Duplicate connection with '+Duplicate.ClientRemoteAddr);
          exit;
        end;
        TNetData.NetData.NotifyReceivedHelloMessage;
      end else begin
        DisconnectInvalidClient(false,'Invalid header type > '+TNetData.HeaderDataToText(HeaderData));
      end;
      //
      If (isFirstHello) And (HeaderData.header_type = ntp_response) then begin
        DoProcess_GetPendingOperations;
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
Var   errors : String;
  decrypted,messagecrypted : TRawBytes;
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
    if not TPCEncryption.DoPascalCoinECIESDecrypt(TNetData.NetData.NodePrivateKey.PrivateKey,messagecrypted,decrypted) then begin
      errors := 'Error on decrypting message';
      exit;
    end;

    DoDisconnect := false;
    if TCrypto.IsHumanReadable(decrypted) then
      TLog.NewLog(ltinfo,Classname,'Received new message from '+ClientRemoteAddr+' Message ('+inttostr(length(decrypted))+' bytes): '+decrypted.ToPrintable)
    else
      TLog.NewLog(ltinfo,Classname,'Received new message from '+ClientRemoteAddr+' Message ('+inttostr(length(decrypted))+' bytes) in hexadecimal: '+decrypted.ToHexaString);
    Try
      TNode.Node.NotifyNetClientMessage(Self,decrypted.ToString);
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
Type
  TNewFastPropagationBlockOperation = Record
    opReference : TOpReference;
    opStreamData : TBytes;
  end;
  TNewFastPropagationBlockOperationsArray = Array of TNewFastPropagationBlockOperation;

var operationsComp : TPCOperationsComp;
  DoDisconnect : Boolean;
  errors : String;

  function ProcessNewFastBlockPropagation : Boolean;
  var nfpboarr : TNewFastPropagationBlockOperationsArray;
    oprefcount, notFoundOpReferencesCount, c : Cardinal;
    i,iNodeOpReference : Integer;
    sendStream, receiveStream : TStream;
    block_op_ref : UInt64;
    headerData : TNetHeaderData;
    auxOp : TPCOperation;
    tc : TTickCount;
    original_OperationBlock : TOperationBlock;
  begin
    Result := False;
    DoDisconnect := True;
    original_OperationBlock := operationsComp.OperationBlock;
    errors := 'Invalid structure data in ProcessNewFastBlockPropagation';
    tc := TPlatform.GetTickCount;
    SetLength(nfpboarr,0);
    try
    if DataBuffer.Read(oprefcount,SizeOf(oprefcount))<>SizeOf(oprefcount) then Exit;
    if DataBuffer.Size - DataBuffer.Position < (oprefcount * SizeOf(TOpReference)) then Exit;
    SetLength(nfpboarr,oprefcount);
    if (oprefcount>0) then begin
      for i := 0 to Integer(Integer(oprefcount)-1) do begin
        if DataBuffer.Read(nfpboarr[i].opReference,SizeOf(TOpReference))<>SizeOf(TOpReference) then Exit;
        SetLength(nfpboarr[i].opStreamData,0);
      end;
    end;
    DoDisconnect := False;
    notFoundOpReferencesCount := 0;
    if (oprefcount>0) then begin
      // Try TNode locking process
      If Not TNode.Node.TryLockNode(3000) then Exit; // Cannot lock...
      Try
        if (operationsComp.OperationBlock.block<>TNode.Node.Bank.BlocksCount) then Exit; // Meanwhile other threads have added it
        // Fill not included operations:
        for i:=0 to High(nfpboarr) do begin
          iNodeOpReference := TNode.Node.Operations.OperationsHashTree.IndexOfOpReference(nfpboarr[i].opReference);
          if iNodeOpReference>=0 then begin
            nfpboarr[i].opStreamData := TNode.Node.Operations.OperationsHashTree.GetOperation(iNodeOpReference).GetOperationStreamData;
          end else begin
            inc(notFoundOpReferencesCount);
          end;
        end;
      Finally
        TNode.Node.UnlockNode;
      End;
    end;
    if (notFoundOpReferencesCount>CT_MAX_OPS_PER_BLOCKCHAINOPERATIONS) then begin
      // A lot of operations pending! Calling GetBlocks
      TLog.NewLog(ltdebug,ClassName,Format('Too many pending operations (%d of %d) in Fast propagation block %d',[notFoundOpReferencesCount,oprefcount,operationsComp.OperationBlock.block]));
      Exit;
    end else if (notFoundOpReferencesCount>0) then begin
      // Must obtain not found calling CT_NetOp_GetBlockchainOperations
      TLog.NewLog(ltdebug,ClassName,Format('Pending operations (%d of %d) in Fast propagation block %d',[notFoundOpReferencesCount,oprefcount,operationsComp.OperationBlock.block]));
      sendStream := TMemoryStream.Create;
      receiveStream := TMemoryStream.Create;
      Try
        sendStream.Write(notFoundOpReferencesCount,SizeOf(notFoundOpReferencesCount)); // 4 bytes for count
        for i:=0 to High(nfpboarr) do begin
          if Length(nfpboarr[i].opStreamData)=0 then begin
            // Need this!
            block_op_ref := UInt64(UInt64(operationsComp.OperationBlock.block) SHL 32) + i;
            sendStream.Write(block_op_ref,SizeOf(block_op_ref)); // 8 bytes for Block_op_ref
          end;
        end;
        // Send & wait
        if Not DoSendAndWaitForResponse(CT_NetOp_GetBlockchainOperations,TNetData.NetData.NewRequestId,sendStream,receiveStream,5000,headerData) then begin
          TLog.NewLog(ltdebug,ClassName,Format('Not received Pending operations (%d of %d) in Fast propagation block %d',[notFoundOpReferencesCount,oprefcount,operationsComp.OperationBlock.block]));
          Exit;
        end;
        DoDisconnect := True; // If bad received data... then DoDisconnect
        if (headerData.is_error) then Exit;
        receiveStream.Position := 0;
        receiveStream.Read(c,SizeOf(c));
        if (c<>notFoundOpReferencesCount) then Exit; // Error!
        // Process Response
        for i:=0 to High(nfpboarr) do begin
          if Length(nfpboarr[i].opStreamData)=0 then begin
            // Read it from response:
            if receiveStream.Read(c,SizeOf(c)) <> SizeOf(c) then Exit;
            if receiveStream.Size - receiveStream.Position < c then Exit; // Not enough received data
            SetLength(nfpboarr[i].opStreamData,c);
            receiveStream.ReadBuffer(nfpboarr[i].opStreamData[0],c); // Fixed bug 4.0.0
          end;
        end;
        DoDisconnect := False;
      finally
        sendStream.Free;
        receiveStream.Free;
      end;
    end;
    // Now we have nfpboarr with full data
    for i := 0 to High(nfpboarr) do begin
      auxOp := TPCOperation.GetOperationFromStreamData( nfpboarr[i].opStreamData );
      if not Assigned(auxOp) then begin
        errors := Format('Op index not available (%d/%d) OpReference:%d size:%d',[i,High(nfpboarr),nfpboarr[i].opReference,Length(nfpboarr[i].opStreamData)]);
        Exit;
      end else begin
        if Not operationsComp.AddOperation(False,auxOp,errors) then Exit;
        auxOp.Free;
      end;
    end;
    // Finished
    if (notFoundOpReferencesCount > 0) then begin
      TLog.NewLog(ltdebug,ClassName,Format('Processed NewFastBlockPropagation with Pending operations (%d of %d) in Fast propagation block %d for %d miliseconds',[notFoundOpReferencesCount,oprefcount,operationsComp.OperationBlock.block,TPlatform.GetElapsedMilliseconds(tc)]));
    end;
    // Check that operationsComp.operationBlock is equal to received
    If Not TAccountComp.EqualOperationBlocks(operationsComp.OperationBlock,original_OperationBlock) then begin
      // This can happen when a OpReference in my MEMPOOL is different to an OpReference in the miner, causing different OperationsHash value
      // This means a possible double spend found
      TLog.NewLog(lterror,ClassName,Format('Constructed a distinct FAST PROPAGATION block with my mempool operations. Received: %s Constructed: %s',
        [TPCOperationsComp.OperationBlockToText(original_OperationBlock),TPCOperationsComp.OperationBlockToText(operationsComp.OperationBlock)]));
      if Not TPCSafeBox.IsValidOperationBlock(original_OperationBlock,errors) then begin
        // This means a scammer!
        DoDisconnect := True;
      end;
      Exit;
    end;
    finally
      // Clean memory
      for i := 0 to High(nfpboarr) do begin
        SetLength(nfpboarr[i].opStreamData,0);
      end;
      SetLength(nfpboarr,0);
    end;
    DoDisconnect := False;
    Result := True;
  end;
var bacc : TBlockAccount;
  c : Cardinal;
begin
  errors := '';
  DoDisconnect := true;
  try
    if HeaderData.header_type<>ntp_autosend then begin
      errors := 'Not autosend';
      exit;
    end;
    operationsComp := TPCOperationsComp.Create(nil);
    try
      operationsComp.bank := TNode.Node.Bank;
      if Not operationsComp.LoadBlockFromStream(DataBuffer,errors) then begin
        errors := 'Error decoding new account: '+errors;
        exit;
      end else begin
        DoDisconnect := false;
        DataBuffer.Read(FRemoteAccumulatedWork,SizeOf(FRemoteAccumulatedWork));
        if operationsComp.IsOnlyOperationBlock then begin
          TLog.NewLog(ltdebug,ClassName,'Received NEW FAST PROPAGATION BLOCK with height: '+inttostr(operationsComp.OperationBlock.block)+' Accumulated work '+IntToStr(FRemoteAccumulatedWork)+' from '+ClientRemoteAddr);
        end else begin
          TLog.NewLog(ltdebug,ClassName,'Received NEW BLOCK with height: '+inttostr(operationsComp.OperationBlock.block)+' Accumulated work '+IntToStr(FRemoteAccumulatedWork)+' from '+ClientRemoteAddr);
        end;
        FRemoteOperationBlock := operationsComp.OperationBlock;
        if (FRemoteAccumulatedWork>TNode.Node.Bank.SafeBox.WorkSum) then begin
          if (operationsComp.OperationBlock.block=TNode.Node.Bank.BlocksCount) then begin
            // New block candidate:
            if (operationsComp.IsOnlyOperationBlock) then begin
              // Received a FAST PROPAGATION BLOCK as described at PIP-0015
              // Fill operations reference:
              If Not ProcessNewFastBlockPropagation then begin
                if DoDisconnect then Exit
                else begin
                  Send_GetBlocks(operationsComp.OperationBlock.block,1,c);
                  Exit;
                end;
              end;
            end;
            If Not TNode.Node.AddNewBlockChain(Self,operationsComp,bacc,errors) then begin
              // Check valid header, if not, scammer... Disconnect
              if Not TPCSafeBox.IsValidOperationBlock(operationsComp.OperationBlock,errors) then begin
                DoDisconnect := True;
                Exit;
              end;
              // Really is a new block? (Check it)
              if (operationsComp.OperationBlock.block=TNode.Node.Bank.BlocksCount) then begin
                // Received a new invalid block... perhaps I'm an orphan blockchain
                TNetData.NetData.GetNewBlockChainFromClient(Self,'Higher Work with same block height. I''m a orphan blockchain candidate');
              end;
            end;
          end else begin
            // Received a new higher work
            TNetData.NetData.GetNewBlockChainFromClient(Self,Format('Higher Work and distinct blocks count. Need to download BlocksCount:%d  my BlocksCount:%d',[operationsComp.OperationBlock.block+1,TNode.Node.Bank.BlocksCount]));
          end;
        end;
      end;
    finally
      operationsComp.Free;
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
var tc : TTickCount;
  was_waiting_for_response : Boolean;
  iDebugStep : Integer;
  reservedResponse : TMemoryStream;
begin
  iDebugStep := 0;
  Try
    Result := false;
    HeaderData := CT_NetHeaderData;
    If FIsWaitingForResponse then begin
      TLog.NewLog(ltdebug,Classname,'Is waiting for response ...');
      exit;
    end;
    iDebugStep := 100;
    If Not Assigned(FTcpIpClient) then exit;
    if Not Client.Connected then exit;
    iDebugStep := 110;
    tc := TPlatform.GetTickCount;
    If TPCThread.TryProtectEnterCriticalSection(Self,MaxWaitTime,FNetLock) then begin
      Try
        iDebugStep := 120;
        was_waiting_for_response := RequestId>0;
        try
          if was_waiting_for_response then begin
            iDebugStep := 200;
            FIsWaitingForResponse := true;
            Send(ntp_request,operation,0,RequestId,SendDataBuffer);
          end;
          iDebugStep := 300;
          Repeat
            iDebugStep := 400;
            if (MaxWaitTime > TPlatform.GetTickCount - tc) then MaxWaitTime := MaxWaitTime - (TPlatform.GetTickCount - tc)
            else MaxWaitTime := 1;
            If (MaxWaitTime>60000) then MaxWaitTime:=60000;
            tc := TPlatform.GetTickCount;
            if (ReadTcpClientBuffer(MaxWaitTime,HeaderData,ReceiveDataBuffer)) then begin
              iDebugStep := 500;
              TNetData.NetData.NodeServersAddresses.UpdateNetConnection(Self);
              iDebugStep := 800;
              {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,Classname,'Received '+CT_NetTransferType[HeaderData.header_type]+' operation:'+TNetData.OperationToText(HeaderData.operation)+' id:'+Inttostr(HeaderData.request_id)+' Buffer size:'+Inttostr(HeaderData.buffer_data_length) );{$ENDIF}
              if (RequestId=HeaderData.request_id) And (HeaderData.header_type=ntp_response) then begin
                Result := true;
              end else begin
                iDebugStep := 1000;
                case HeaderData.operation of
                  CT_NetOp_Hello : Begin
                    if TNetData.NetData.IpInfos.ReachesLimits(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length,
                      TArray<TLimitLifetime>.Create(TLimitLifetime.Create(CT_NewLineSecondsAvg * 2,20,20000))) then DisconnectInvalidClient(False,Format('Reached limit %s',[TNetData.OperationToText(HeaderData.operation)]))
                    else begin
                      iDebugStep := 1100;
                      DoProcess_Hello(HeaderData,ReceiveDataBuffer);
                    end;
                  End;
                  CT_NetOp_Message : Begin
                    if TNetData.NetData.IpInfos.ReachesLimits(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length,
                      TArray<TLimitLifetime>.Create(TLimitLifetime.Create(60,20,20000))) then DisconnectInvalidClient(False,Format('Reached limit %s',[TNetData.OperationToText(HeaderData.operation)]))
                    else DoProcess_Message(HeaderData,ReceiveDataBuffer);
                  End;
                  CT_NetOp_GetBlocks : Begin
                    if HeaderData.header_type=ntp_request then begin
                      if TNetData.NetData.IpInfos.ReachesLimits(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length,
                        TArray<TLimitLifetime>.Create(TLimitLifetime.Create(300,100,0),TLimitLifetime.Create(10,5,0))) then DisconnectInvalidClient(False,Format('Reached limit %s',[TNetData.OperationToText(HeaderData.operation)]))
                      else DoProcess_GetBlocks_Request(HeaderData,ReceiveDataBuffer)
                    end else if HeaderData.header_type=ntp_response then begin
                      DoProcess_GetBlocks_Response(HeaderData,ReceiveDataBuffer);
                    end else DisconnectInvalidClient(false,'Not resquest or response: '+TNetData.HeaderDataToText(HeaderData));
                  End;
                  CT_NetOp_GetBlockHeaders : Begin
                    if HeaderData.header_type=ntp_request then begin
                      if TNetData.NetData.IpInfos.ReachesLimits(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length,
                        TArray<TLimitLifetime>.Create(TLimitLifetime.Create(30,30,0))) then DisconnectInvalidClient(False,Format('Reached limit %s',[TNetData.OperationToText(HeaderData.operation)]))
                      else DoProcess_GetOperationsBlock_Request(HeaderData,ReceiveDataBuffer)
                    end else TLog.NewLog(ltdebug,Classname,'Received old response of: '+TNetData.HeaderDataToText(HeaderData));
                  End;
                  CT_NetOp_NewBlock, CT_NetOp_NewBlock_Fast_Propagation : Begin
                    DoProcess_NewBlock(HeaderData,ReceiveDataBuffer);
                  End;
                  CT_NetOp_GetBlockchainOperations : Begin
                    if HeaderData.header_type=ntp_request then begin
                      if TNetData.NetData.IpInfos.ReachesLimits(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length,
                        TArray<TLimitLifetime>.Create(TLimitLifetime.Create(60,10,0))) then DisconnectInvalidClient(False,Format('Reached limit %s',[TNetData.OperationToText(HeaderData.operation)]))
                      else DoProcess_GetBlockchainOperations_Request(HeaderData,ReceiveDataBuffer)
                    end else TLog.NewLog(ltdebug,Classname,'Received old response of: '+TNetData.HeaderDataToText(HeaderData));
                  End;
                  CT_NetOp_AddOperations : Begin
                    DoProcess_AddOperations(HeaderData,ReceiveDataBuffer);
                  End;
                  CT_NetOp_GetSafeBox : Begin
                    if HeaderData.header_type=ntp_request then begin
                      if TNetData.NetData.IpInfos.ReachesLimits(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length,
                        TArray<TLimitLifetime>.Create(TLimitLifetime.Create(1200,100,0),TLimitLifetime.Create(10,40,0))) then DisconnectInvalidClient(False,Format('Reached limit %s',[TNetData.OperationToText(HeaderData.operation)]))
                      else DoProcess_GetSafeBox_Request(HeaderData,ReceiveDataBuffer)
                    end else DisconnectInvalidClient(false,'Received '+TNetData.HeaderDataToText(HeaderData));
                  end;
                  CT_NetOp_GetPendingOperations : Begin
                    if (HeaderData.header_type=ntp_request) then begin
                      if TNetData.NetData.IpInfos.ReachesLimits(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length,
                        TArray<TLimitLifetime>.Create(TLimitLifetime.Create(300,100,0))) then DisconnectInvalidClient(False,Format('Reached limit %s',[TNetData.OperationToText(HeaderData.operation)]))
                      else DoProcess_GetPendingOperations_Request(HeaderData,ReceiveDataBuffer)
                    end else TLog.NewLog(ltdebug,Classname,'Received old response of: '+TNetData.HeaderDataToText(HeaderData));
                  end;
                  CT_NetOp_GetAccount : Begin
                    if (HeaderData.header_type=ntp_request) then begin
                      if TNetData.NetData.IpInfos.ReachesLimits(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length,
                        TArray<TLimitLifetime>.Create(TLimitLifetime.Create(10,60,0))) then DisconnectInvalidClient(False,Format('Reached limit %s',[TNetData.OperationToText(HeaderData.operation)]))
                      else DoProcess_GetAccount_Request(HeaderData,ReceiveDataBuffer)
                    end else TLog.NewLog(ltdebug,Classname,'Received old response of: '+TNetData.HeaderDataToText(HeaderData));
                  end;
                  CT_NetOp_GetPubkeyAccounts : Begin
                    if (HeaderData.header_type=ntp_request) then begin
                      if TNetData.NetData.IpInfos.ReachesLimits(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length,
                        TArray<TLimitLifetime>.Create(TLimitLifetime.Create(10,50,0))) then DisconnectInvalidClient(False,Format('Reached limit %s',[TNetData.OperationToText(HeaderData.operation)]))
                      else DoProcess_GetPubkeyAccounts_Request(HeaderData,ReceiveDataBuffer)
                    end else TLog.NewLog(ltdebug,Classname,'Received old response of: '+TNetData.HeaderDataToText(HeaderData));
                  End;
                  CT_NetOp_Reserved_Start..CT_NetOp_Reserved_End : Begin
                    // This will allow to do nothing if not implemented
                    reservedResponse := TMemoryStream.Create;
                    Try
                      TNetData.NetData.DoProcessReservedAreaMessage(Self,HeaderData,ReceiveDataBuffer,reservedResponse);
                      if (HeaderData.header_type=ntp_request) then begin
                        if (reservedResponse.Size>0) then begin
                          Send(ntp_response,HeaderData.operation,0,HeaderData.request_id,reservedResponse);
                        end else begin
                          // If is a request, and DoProcessReservedAreaMessage didn't filled reservedResponse, will response with ERRORCODE_NOT_IMPLEMENTED
                          Send(ntp_response,HeaderData.operation, CT_NetOp_ERRORCODE_NOT_IMPLEMENTED ,HeaderData.request_id,Nil);
                        end;
                      end;
                    finally
                      reservedResponse.Free;
                    end;
                  end
                else
                  DisconnectInvalidClient(false,'Invalid operation: '+TNetData.HeaderDataToText(HeaderData));
                end;
              end;
            end else sleep(1);
            iDebugStep := 900;
          Until (Result) Or (TPlatform.GetTickCount>(MaxWaitTime+tc)) Or (Not Connected) Or (FDoFinalizeConnection);
        finally
          if was_waiting_for_response then FIsWaitingForResponse := false;
        end;
        iDebugStep := 990;
      Finally
        FNetLock.Release;
      End;
    end;
  Except
    On E:Exception do begin
      E.Message := E.Message+' DoSendAndWaitForResponse step '+Inttostr(iDebugStep)+' Header.operation:'+Inttostr(HeaderData.operation);
      Raise;
    end;
  End;
end;

procedure TNetConnection.FinalizeConnection;
begin
  If FDoFinalizeConnection then exit;
  {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,'Executing FinalizeConnection to '+ClientRemoteAddr);{$ENDIF}
  FDoFinalizeConnection := true;
end;

function TNetConnection.GetClient: TNetTcpIpClient;
begin
  if Not Assigned(FTcpIpClient) then begin
    TLog.NewLog(ltError,Classname,'TcpIpClient=NIL');
    raise Exception.Create('TcpIpClient=NIL');
  end;
  Result := FTcpIpClient;
end;

function TNetConnection.GetConnected: Boolean;
begin
  Result := Assigned(FTcpIpClient) And (FTcpIpClient.Connected);
end;

procedure TNetConnection.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation=opRemove) And (AComponent = FTcpIpClient) then begin
    FTcpIpClient := Nil;
  end;
end;

function TNetConnection.ReadTcpClientBuffer(MaxWaitMiliseconds: Cardinal; var HeaderData: TNetHeaderData; BufferData: TStream): Boolean;
var
  auxstream : TMemoryStream;
  tc : TTickCount;
  last_bytes_read, t_bytes_read : Int64;
  //
  IsValidHeaderButNeedMoreData : Boolean;
  deletedBytes : Int64;


begin
  t_bytes_read := 0;
  Result := false;
  HeaderData := CT_NetHeaderData;
  BufferData.Size := 0;
  TPCThread.ProtectEnterCriticalSection(Self,FNetLock);
  try
    tc := TPlatform.GetTickCount;
    repeat
      If not Connected then exit;
      if Not Client.Connected then exit;
      last_bytes_read := 0;
      FClientBufferRead.Position := 0;
      Result := TNetData.ExtractHeaderInfo(FClientBufferRead,HeaderData,BufferData,IsValidHeaderButNeedMoreData);
      if Result then begin
        FNetProtocolVersion := HeaderData.protocol;
        // Build 1.0.4 accepts net protocol 1 and 2
        if HeaderData.protocol.protocol_version>CT_NetProtocol_Available then begin
          TNode.Node.NotifyNetClientMessage(Nil,
            'Detected a higher Net protocol version at '+
            ClientRemoteAddr+' (v '+inttostr(HeaderData.protocol.protocol_version)+' '+inttostr(HeaderData.protocol.protocol_available)+') '+
            '... check that your version is Ok! Visit official download website for possible updates: https://sourceforge.net/projects/pascalcoin/');
          DisconnectInvalidClient(false,Format('Invalid Net protocol version found: %d available: %d',[HeaderData.protocol.protocol_version,HeaderData.protocol.protocol_available]));
          Result := false;
          exit;
        end else begin
          if (FNetProtocolVersion.protocol_available>CT_NetProtocol_Available) And (Not FAlertedForNewProtocolAvailable) then begin
            FAlertedForNewProtocolAvailable := true;
            TNode.Node.NotifyNetClientMessage(Nil,
              'Detected a new Net protocol version at '+
              ClientRemoteAddr+' (v '+inttostr(HeaderData.protocol.protocol_version)+' '+inttostr(HeaderData.protocol.protocol_available)+') '+
              '... Visit official download website for possible updates: https://sourceforge.net/projects/pascalcoin/');
          end;
          // Remove data from buffer and save only data not processed (higher than stream.position)
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
        sleep(1);
        if Not Client.WaitForData(100) then begin
          exit;
        end;

        auxstream := (Client as TBufferedNetTcpIpClient).ReadBufferLock;
        try
          last_bytes_read := auxstream.size;
          if last_bytes_read>0 then begin
            FLastDataReceivedTS := TPlatform.GetTickCount;

            FClientBufferRead.Position := FClientBufferRead.size; // Go to the end
            auxstream.Position := 0;
            FClientBufferRead.CopyFrom(auxstream,last_bytes_read);
            FClientBufferRead.Position := 0;
            auxstream.Size := 0;
            inc(t_bytes_read,last_bytes_read);
          end;
        finally
          (Client as TBufferedNetTcpIpClient).ReadBufferUnlock;
        end;
      end;
    until (Result) Or ((TPlatform.GetTickCount > (tc+MaxWaitMiliseconds)) And (last_bytes_read=0));
  finally
    Try
      if (Connected) then begin
        if (Not Result) And (FClientBufferRead.Size>0) And (Not IsValidHeaderButNeedMoreData) then begin
          deletedBytes := FClientBufferRead.Size;
          TLog.NewLog(lterror,ClassName,Format('Deleting %d bytes from TcpClient buffer of %s after max %d miliseconds. Elapsed: %d',
            [deletedBytes, Client.ClientRemoteAddr,MaxWaitMiliseconds,TPlatform.GetTickCount-tc]));
          FClientBufferRead.Size:=0;
          DisconnectInvalidClient(false,'Invalid data received in buffer ('+inttostr(deletedBytes)+' bytes)');
        end else if (IsValidHeaderButNeedMoreData) then begin
          if (t_bytes_read>0) then begin
            TLog.NewLog(ltDebug,ClassName,Format('Not enough data received - Received %d bytes from TcpClient buffer of %s after max %d miliseconds. Elapsed: %d - HeaderData: %s',
              [FClientBufferRead.Size, Client.ClientRemoteAddr,MaxWaitMiliseconds,TPlatform.GetTickCount-tc,TNetData.HeaderDataToText(HeaderData)]));
          end else if (TPlatform.GetElapsedMilliseconds(FLastDataReceivedTS)>60000) then begin
            TLog.NewLog(lterror,ClassName,Format('Closing connection to %s due not received expected data. Received:%d Expected:%d ElapsedMilis:%d',
              [Client.ClientRemoteAddr,FClientBufferRead.Size,HeaderData.buffer_data_length,TPlatform.GetElapsedMilliseconds(FLastDataReceivedTS)]));
            Connected:=False;
          end;
        end;
      end;
    Finally
      FNetLock.Release;
    End;
  end;
  if t_bytes_read>0 then begin
    if Not FHasReceivedData then begin
      FHasReceivedData := true;
      if (Self is TNetClient) then
        TNetData.NetData.IncStatistics(0,0,0,1,t_bytes_read,0)
      else TNetData.NetData.IncStatistics(0,0,0,0,t_bytes_read,0);
    end else begin
      TNetData.NetData.IncStatistics(0,0,0,0,t_bytes_read,0);
    end;
  end;
  if (Result) And (HeaderData.header_type=ntp_response) then begin
    TNetData.NetData.UnRegisterRequest(Self,HeaderData.operation,HeaderData.request_id);
  end;
  // Update stats... only if not response (because we don't need to know/store stats for responses in general). This is minimal memory use
  if (Result) And (HeaderData.header_type<>ntp_response) then begin
    TNetData.NetData.IpInfos.UpdateIpInfo(Client.RemoteHost,CT_NetTransferType[HeaderData.header_type],TNetData.OperationToText(HeaderData.operation),HeaderData.buffer_data_length);
  end;
end;

procedure TNetConnection.Send(NetTranferType: TNetTransferType; operation, errorcode: Word; request_id: Integer; DataBuffer: TStream);
Var l : Cardinal;
   w : Word;
  Buffer : TStream;
  {$IFDEF HIGHLOG}
  s : String;
  {$ENDIF}
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
    l := CT_NetProtocol_Version;
    Buffer.Write(l,2);
    l := CT_NetProtocol_Available;
    Buffer.Write(l,2);
    if Assigned(DataBuffer) then begin
      l := DataBuffer.Size;
      Buffer.Write(l,4);
      DataBuffer.Position := 0;
      Buffer.CopyFrom(DataBuffer,DataBuffer.Size);
      {$IFDEF HIGHLOG}s := '(Data:'+inttostr(DataBuffer.Size)+'b) ';{$ENDIF}
    end else begin
      l := 0;
      Buffer.Write(l,4);
      {$IFDEF HIGHLOG}s := '';{$ENDIF}
    end;
    Buffer.Position := 0;
    TPCThread.ProtectEnterCriticalSection(Self,FNetLock);
    Try
      {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,Classname,'Sending: '+CT_NetTransferType[NetTranferType]+' operation:'+
        TNetData.OperationToText(operation)+' id:'+Inttostr(request_id)+' errorcode:'+InttoStr(errorcode)+
        ' Size:'+InttoStr(Buffer.Size)+'b '+s+'to '+
        ClientRemoteAddr);{$ENDIF}
      (Client as TBufferedNetTcpIpClient).WriteBufferToSend(Buffer);
      FLastDataSendedTS := TPlatform.GetTickCount;
    Finally
      FNetLock.Release;
    End;
    TNetData.NetData.IncStatistics(0,0,0,0,0,Buffer.Size);
  finally
    Buffer.Free;
  end;
end;

procedure TNetConnection.SendError(NetTranferType: TNetTransferType; operation,
  request_id: Integer; error_code: Integer; const error_text: String);
var buffer : TStream;
begin
  buffer := TMemoryStream.Create;
  Try
    TStreamOp.WriteAnsiString(buffer,TEncoding.ASCII.GetBytes(error_text));
    Send(NetTranferType,operation,error_code,request_id,buffer);
  Finally
    buffer.Free;
  End;
end;

function TNetConnection.Send_AddOperations(Operations : TOperationsHashTree) : Boolean;
Var data : TMemoryStream;
  c1, request_id : Cardinal;
  i, nOpsToSend : Integer;
  optype : Byte;
begin
  Result := false;
  if Not Connected then exit;
  FNetLock.Acquire;
  try
    nOpsToSend := 0;
    FBufferLock.Acquire;
    Try
      If Assigned(Operations) then begin
        for i := 0 to Operations.OperationsCount - 1 do begin
          if FBufferReceivedOperationsHash.IndexOf(Operations.GetOperation(i).Sha256)<0 then begin
            FBufferReceivedOperationsHash.Add(Operations.GetOperation(i).Sha256);
            If FBufferToSendOperations.IndexOfOperation(Operations.GetOperation(i))<0 then begin
              FBufferToSendOperations.AddOperationToHashTree(Operations.GetOperation(i));
            end;
          end;
        end;
        nOpsToSend := Operations.OperationsCount;
      end;
      if FBufferToSendOperations.OperationsCount>0 then begin
        TLog.NewLog(ltdebug,ClassName,Format('Sending %d Operations to %s (inProc:%d, Received:%d)',[FBufferToSendOperations.OperationsCount,ClientRemoteAddr,nOpsToSend,FBufferReceivedOperationsHash.Count]));
        data := TMemoryStream.Create;
        try
          request_id := TNetData.NetData.NewRequestId;
          c1 := FBufferToSendOperations.OperationsCount;
          data.Write(c1,4);
          for i := 0 to FBufferToSendOperations.OperationsCount-1 do begin
            optype := FBufferToSendOperations.GetOperation(i).OpType;
            data.Write(optype,1);
            FBufferToSendOperations.GetOperation(i).SaveToNettransfer(data);
          end;
          Send(ntp_autosend,CT_NetOp_AddOperations,0,request_id,data);
          FBufferToSendOperations.ClearHastThree;
        finally
          data.Free;
        end;
      end{$IFDEF HIGHLOG} else TLog.NewLog(ltdebug,ClassName,Format('Not sending any operations to %s (inProc:%d, Received:%d, Sent:%d)',[ClientRemoteAddr,nOpsToSend,FBufferReceivedOperationsHash.Count,FBufferToSendOperations.OperationsCount])){$ENDIF};
    finally
      FBufferLock.Release;
    end;
  finally
    FNetLock.Release;
  end;
  Result := Connected;
end;

function TNetConnection.Send_GetBlocks(StartAddress, quantity : Cardinal; var request_id : Cardinal) : Boolean;
Var data : TMemoryStream;
  c1,c2 : Cardinal;
begin
  Result := false;
  request_id := 0;
  if (FRemoteOperationBlock.block<TNetData.NetData.Bank.BlocksCount) Or (FRemoteOperationBlock.block=0) then exit;
  if Not Connected then exit;
  // First receive operations from
  data := TMemoryStream.Create;
  try
    if TNetData.NetData.Bank.BlocksCount=0 then c1:=0
    else c1:=StartAddress;
    if (quantity=0) then begin
      if FRemoteOperationBlock.block>0 then c2 := FRemoteOperationBlock.block
      else c2 := c1+100;
    end else c2 := c1+quantity-1;
    // Build 1.0.5 BUG - Always query for ONLY 1 if Build is lower or equal to 1.0.5
    if ((FClientAppVersion='') Or ( (length(FClientAppVersion)=5) And (FClientAppVersion<='1.0.5') )) then begin
      c2 := c1;
    end;
    data.Write(c1,4);
    data.Write(c2,4);
    request_id := TNetData.NetData.NewRequestId;
    TNetData.NetData.RegisterRequest(Self,CT_NetOp_GetBlocks,request_id);
    TLog.NewLog(ltdebug,ClassName,Format('Send GET BLOCKS start:%d quantity:%d (from:%d to %d)',[StartAddress,quantity,StartAddress,quantity+StartAddress]));
    FIsDownloadingBlocks := quantity>1;
    Send(ntp_request,CT_NetOp_GetBlocks,0,request_id,data);
    Result := Connected;
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
  nsa : TNodeServerAddress;
  nsarr : TNodeServerAddressArray;
  w : Word;
  currunixtimestamp : Cardinal;
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
    // Save My connection public key
    TStreamOp.WriteAnsiString(data,TAccountComp.AccountKey2RawString(TNetData.NetData.FNodePrivateKey.PublicKey));
    // Save my Unix timestamp (4 bytes)
    currunixtimestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    data.Write(currunixtimestamp,4);
    // Save last operations block
    TPCOperationsComp.SaveOperationBlockToStream(TNode.Node.Bank.LastOperationBlock,data);
    nsarr := TNetData.NetData.NodeServersAddresses.GetValidNodeServers(true,CT_MAX_NODESERVERS_ON_HELLO);
    i := length(nsarr);
    data.Write(i,4);
    for i := 0 to High(nsarr) do begin
      nsa := nsarr[i];
      TStreamOp.WriteAnsiString(data,TEncoding.ASCII.GetBytes(nsa.ip));
      data.Write(nsa.port,2);
      data.Write(nsa.last_connection,4);
    end;
    // Send client version
    TStreamOp.WriteAnsiString(data,TEncoding.ASCII.GetBytes(TNode.NodeVersion));
    // Build 1.5 send accumulated work
    data.Write(TNode.Node.Bank.SafeBox.WorkSum,SizeOf(TNode.Node.Bank.SafeBox.WorkSum));
    //
    Send(NetTranferType,CT_NetOp_Hello,0,request_id,data);
    Result := Client.Connected;
    FLastHelloTS := TPlatform.GetTickCount;
  finally
    data.Free;
  end;
end;

function TNetConnection.Send_Message(const TheMessage: String): Boolean;
Var data : TStream;
  cyp : TRawBytes;
begin
  Result := false;
  if Not Connected then exit;
  data := TMemoryStream.Create;
  Try
    // Cypher message:
    TPCEncryption.DoPascalCoinECIESEncrypt(FClientPublicKey,TEncoding.ASCII.GetBytes(TheMessage),cyp);
    TStreamOp.WriteAnsiString(data,cyp);
    Send(ntp_autosend,CT_NetOp_Message,0,0,data);
    Result := true;
  Finally
    data.Free;
  End;
end;

function TNetConnection.Send_NewBlockFound(const NewBlock: TPCOperationsComp): Boolean;
var data : TStream;
  request_id : Integer;
  netOp : Word;
  c : Cardinal;
  i : Integer;
  opRef : TOpReference;
begin
  Result := false;
  if Not Connected then exit;
  FNetLock.Acquire;
  Try
    // Clear buffers
    FBufferLock.Acquire;
    Try
      FBufferReceivedOperationsHash.Clear;
      FBufferToSendOperations.ClearHastThree;
    finally
      FBufferLock.Release;
    end;
    // Checking if operationblock is the same to prevent double messaging...
    If (TPCOperationsComp.EqualsOperationBlock(FRemoteOperationBlock,NewBlock.OperationBlock)) then begin
      {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,ClassName,'This connection has the same block, does not need to send');{$ENDIF}
      exit;
    end;
    if (TNode.Node.Bank.BlocksCount<>NewBlock.OperationBlock.block+1) then begin
      TLog.NewLog(ltDebug,ClassName,'The block number '+IntToStr(NewBlock.OperationBlock.block)+' is not equal to current blocks stored in bank ('+IntToStr(TNode.Node.Bank.BlocksCount)+'), finalizing');
      exit;
    end;
    data := TMemoryStream.Create;
    try
      request_id := TNetData.NetData.NewRequestId;
      // Will send a FAST PROPAGATION BLOCK as described at PIP-0015
      netOp := CT_NetOp_NewBlock_Fast_Propagation;
      NewBlock.SaveBlockToStream(netOp = CT_NetOp_NewBlock_Fast_Propagation,data); // Will save all only if not FAST PROPAGATION
      data.Write(TNode.Node.Bank.SafeBox.WorkSum,SizeOf(TNode.Node.Bank.SafeBox.WorkSum));
      if (netOp = CT_NetOp_NewBlock_Fast_Propagation) then begin
        // Fill with OpReference data:
        c := NewBlock.OperationsHashTree.OperationsCount;
        data.Write(c,SizeOf(c));
        if (c>0) then begin
          for i := 0 to (Integer(c)-1) do begin
            opRef := NewBlock.Operation[i].GetOpReference;
            data.Write(opRef,SizeOf(opRef));
          end;
        end;
      end;
      Send(ntp_autosend,netOp,0,request_id,data);
    finally
      data.Free;
    end;
  Finally
    FNetLock.Release;
  End;
  Result := Connected;
end;

procedure TNetConnection.SetClient(const Value: TNetTcpIpClient);
Var old : TNetTcpIpClient;
begin
  if FTcpIpClient<>Value then begin
    if Assigned(FTcpIpClient) then begin
      FTcpIpClient.OnConnect := Nil;
      FTcpIpClient.OnDisconnect := Nil;
      FTcpIpClient.RemoveFreeNotification(Self);
    end;
    TNetData.NetData.UnRegisterRequest(Self,0,0);
    old := FTcpIpClient;
    FTcpIpClient := Value;
    if Assigned(old) then begin
      if old.Owner=Self then begin
        old.Free;
      end;
    end;
  end;
  if Assigned(FTcpIpClient) then begin
    FTcpIpClient.FreeNotification(Self);
    FTcpIpClient.OnConnect := TcpClient_OnConnect;
    FTcpIpClient.OnDisconnect := TcpClient_OnDisconnect;
  end;
  TNetData.NetData.NotifyNetConnectionUpdated;
end;

procedure TNetConnection.SetConnected(const Value: Boolean);
begin
  if (Value = GetConnected) then exit;
  if Value then ConnectTo(Client.RemoteHost,Client.RemotePort)
  else begin
    FinalizeConnection;
    Client.Disconnect;
  end;
end;

procedure TNetConnection.TcpClient_OnConnect(Sender: TObject);
begin
  TNetData.NetData.IncStatistics(1,0,1,0,0,0);
  TLog.NewLog(ltdebug,Classname,'Connected to a server '+ClientRemoteAddr);
  TNetData.NetData.NotifyNetConnectionUpdated;
end;

procedure TNetConnection.TcpClient_OnDisconnect(Sender: TObject);
begin
  if self is TNetServerClient then TNetData.NetData.IncStatistics(-1,-1,0,0,0,0)
  else begin
    if FHasReceivedData then TNetData.NetData.IncStatistics(-1,0,-1,-1,0,0)
    else TNetData.NetData.IncStatistics(-1,0,-1,0,0,0);
  end;
  TLog.NewLog(ltInfo,Classname,'Disconnected from '+ClientRemoteAddr);
  TNetData.NetData.NotifyNetConnectionUpdated;
  if (FClientTimestampIp<>'') then begin
    TNetData.NetData.NetworkAdjustedTime.RemoveIp(FClientTimestampIp);
  end;
end;

{ TNetClientThread }

procedure TNetClientThread.BCExecute;
begin
  while (Not Terminated) do begin
    If FNetClient.Connected then begin
      FNetClient.DoProcessBuffer;
    end;
    Sleep(1);
  end;
end;

constructor TNetClientThread.Create(NetClient: TNetClient; AOnTerminateThread : TNotifyEvent);
begin
  FNetClient := NetClient;
  inherited Create(false);
  OnTerminate := AOnTerminateThread;
end;

{ TNetClient }

constructor TNetClient.Create(AOwner: TComponent);
begin
  inherited;
  FNetClientThread := TNetClientThread.Create(Self,OnNetClientThreadTerminated);
  FNetClientThread.FreeOnTerminate := false;
end;

destructor TNetClient.Destroy;
begin
  {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Starting TNetClient.Destroy');{$ENDIF}
  FNetClientThread.OnTerminate := Nil;
  if Not FNetClientThread.Terminated then begin
    FNetClientThread.Terminate;
    FNetClientThread.WaitFor;
  end;
  FreeAndNil(FNetClientThread);
  inherited;
end;

procedure TNetClient.OnNetClientThreadTerminated(Sender: TObject);
begin
  // Close connection
  if TNetData.NetData.ConnectionExistsAndActive(Self) then begin
    Connected := false;
  end;
end;

{ TThreadDiscoverConnection }

procedure TThreadDiscoverConnection.BCExecute;
Var NC : TNetClient;
  ok : Boolean;
  ns : TNodeServerAddress;
begin
  Repeat // Face to face conflict when 2 nodes connecting together
    Sleep(Random(1000));
  until (Terminated) Or (Random(5)=0);
  if Terminated then exit;
  {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,Classname,'Starting discovery of connection '+FNodeServerAddress.ip+':'+InttoStr(FNodeServerAddress.port));{$ENDIF}
  DebugStep := 'Locking list';
  // Register attempt
  If TNetData.NetData.NodeServersAddresses.GetNodeServerAddress(FNodeServerAddress.ip,FNodeServerAddress.port,true,ns) then begin
    ns.last_attempt_to_connect := Now;
    inc(ns.total_failed_attemps_to_connect);
    TNetData.NetData.NodeServersAddresses.SetNodeServerAddress(ns);
  end;
  DebugStep := 'Synchronizing notify';
  if Terminated then exit;
  TNetData.NetData.NotifyNodeServersUpdated;
  // Try to connect
  ok := false;
  DebugStep := 'Trying to connect';
  if Terminated then exit;
  NC := TNetClient.Create(Nil);
  Try
    DebugStep := 'Connecting';
    If NC.ConnectTo(FNodeServerAddress.ip,FNodeServerAddress.port) then begin
      if Terminated then exit;
      Sleep(500);
      DebugStep := 'Is connected now?';
      if Terminated then exit;
      ok :=NC.Connected;
    end;
    if Terminated then exit;
  Finally
    if (not ok) And (Not Terminated) then begin
      DebugStep := 'Destroying non connected';
      NC.FinalizeConnection;
    end;
  End;
  DebugStep := 'Synchronizing notify final';
  if Terminated then exit;
  TNetData.NetData.NotifyNodeServersUpdated;
end;

constructor TThreadDiscoverConnection.Create(NodeServerAddress: TNodeServerAddress; NotifyOnTerminate : TNotifyEvent);
begin
  FNodeServerAddress := NodeServerAddress;
  inherited Create(true);
  OnTerminate := NotifyOnTerminate;
  FreeOnTerminate := true;
  Suspended := false;
end;

{ TThreadCheckConnections }

procedure TThreadCheckConnections.BCExecute;
Var l : TList<TNetConnection>;
  i, nactive,ndeleted,nserverclients : Integer;
  netconn : TNetConnection;
  netserverclientstop : TNetServerClient;
  newstats : TNetStatistics;
begin
  FLastCheckTS := TPlatform.GetTickCount;
  while (Not Terminated) do begin
    if ((TPlatform.GetTickCount>(FLastCheckTS+1000)) AND (Not FNetData.FIsDiscoveringServers)) then begin
      nactive := 0;
      ndeleted := 0;
      nserverclients := 0;
      netserverclientstop := Nil;
      FLastCheckTS := TPlatform.GetTickCount;
      If (FNetData.FNetConnections.TryLockList(100,l)) then begin
        try
          newstats := CT_TNetStatistics_NUL;
          for i := l.Count-1 downto 0 do begin
            netconn := TNetConnection(l.Items[i]);
            if (netconn is TNetClient) then begin
              if (netconn.Connected) then begin
                inc(newstats.ServersConnections);
                if (netconn.FHasReceivedData) then inc(newstats.ServersConnectionsWithResponse);
              end;
              if (Not TNetClient(netconn).Connected) And (netconn.CreatedTime+EncodeTime(0,0,5,0)<now) then begin
                // Free this!
                TNetClient(netconn).FinalizeConnection;
                inc(ndeleted);
              end else inc(nactive);
            end else if (netconn is TNetServerClient) then begin
              if (netconn.Connected) then begin
                inc(newstats.ClientsConnections);
              end;
              inc(nserverclients);
              if (Not netconn.FDoFinalizeConnection) then begin
                // Build 1.0.9 BUG-101 Only disconnect old versions prior to 1.0.9
                if not assigned(netserverclientstop) then begin
                  netserverclientstop := TNetServerClient(netconn);
                end else if (netconn.CreatedTime<netserverclientstop.CreatedTime) then begin
                  netserverclientstop := TNetServerClient(netconn);
                end;
              end;
            end;
          end;
          // Update stats:
          FNetData.FNetStatistics.ActiveConnections := newstats.ClientsConnections + newstats.ServersConnections;
          FNetData.FNetStatistics.ClientsConnections := newstats.ClientsConnections;
          FNetData.FNetStatistics.ServersConnections := newstats.ServersConnections;
          FNetData.FNetStatistics.ServersConnectionsWithResponse := newstats.ServersConnectionsWithResponse;
          // Must stop clients?
          if (nserverclients>FNetData.MaxServersConnected) And // This is to ensure there are more serverclients than clients
             ((nserverclients + nactive + ndeleted)>=FNetData.FMaxConnections) And (Assigned(netserverclientstop)) then begin
            TLog.NewLog(ltinfo,Classname,Format('Sending FinalizeConnection to NodeConnection %s created on %s (working time %s) - NetServerClients:%d Servers_active:%d Servers_deleted:%d',
              [netserverclientstop.Client.ClientRemoteAddr,FormatDateTime('hh:nn:ss',netserverclientstop.CreatedTime),
               FormatDateTime('hh:nn:ss',Now - netserverclientstop.CreatedTime),
               nserverclients,nactive,ndeleted]));
            netserverclientstop.FinalizeConnection;
          end;
        finally
          FNetData.FNetConnections.UnlockList;
        end;
        if (nactive<=FNetData.MaxServersConnected) And (Not Terminated) then begin
          // Discover
          FNetData.DiscoverServers;
        end;
      end;
    end;
    sleep(100);
  end;
end;

constructor TThreadCheckConnections.Create(NetData: TNetData);
begin
  FNetData := NetData;
  inherited Create(false);
end;

{ TThreadGetNewBlockChainFromClient }

procedure TThreadGetNewBlockChainFromClient.BCExecute;
Var i,j : Integer;
  maxWork : UInt64;
  candidates : TList<TNetConnection>;
  lop : TOperationBlock;
  nc : TNetConnection;
begin
  if Not TNode.Node.UpdateBlockchain then Exit;

  // Search better candidates:
  candidates := TList<TNetConnection>.Create;
  try
    lop := CT_OperationBlock_NUL;
    TNetData.NetData.FMaxRemoteOperationBlock := CT_OperationBlock_NUL;
    // First round: Find by most work
    maxWork := 0;
    j := TNetData.NetData.ConnectionsCountAll;
    nc := Nil;
    for i := 0 to j - 1 do begin
      if TNetData.NetData.GetConnection(i,nc) then begin
        if (nc.FRemoteAccumulatedWork>maxWork) And (nc.FRemoteAccumulatedWork>TNode.Node.Bank.SafeBox.WorkSum) then begin
          maxWork := nc.FRemoteAccumulatedWork;
        end;
        // Preventing downloading
        if nc.FIsDownloadingBlocks then exit;
      end;
    end;
    if (maxWork>0) then begin
      for i := 0 to j - 1 do begin
        If TNetData.NetData.GetConnection(i,nc) then begin
          if (nc.FRemoteAccumulatedWork>=maxWork) then begin
            candidates.Add(nc);
            lop := nc.FRemoteOperationBlock;
          end;
        end;
      end;
    end;
    // Second round: Find by most height
    if candidates.Count=0 then begin
      for i := 0 to j - 1 do begin
        if (TNetData.NetData.GetConnection(i,nc)) then begin
          if (nc.FRemoteOperationBlock.block>=TNode.Node.Bank.BlocksCount) And
             (nc.FRemoteOperationBlock.block>=lop.block) then begin
             lop := nc.FRemoteOperationBlock;
          end;
        end;
      end;
      if (lop.block>0) then begin
        for i := 0 to j - 1 do begin
          If (TNetData.NetData.GetConnection(i,nc)) then begin
            if (nc.FRemoteOperationBlock.block>=lop.block) then begin
               candidates.Add(nc);
            end;
          end;
        end;
      end;
    end;
    TNetData.NetData.FMaxRemoteOperationBlock := lop;
    if (candidates.Count>0) then begin
      // Random a candidate
      i := 0;
      if (candidates.Count>1) then i := Random(candidates.Count); // i = 0..count-1
      nc := TNetConnection(candidates[i]);
      TNetData.NetData.GetNewBlockChainFromClient(nc,Format('Candidate block: %d sum: %d',[nc.FRemoteOperationBlock.block,nc.FRemoteAccumulatedWork]));
    end;
  finally
    candidates.Free;
  end;
end;

constructor TThreadGetNewBlockChainFromClient.Create;
begin
  Inherited Create(True);
  FreeOnTerminate := true;
  Suspended := false;
end;

{ TNetDataNotifyEventsThread }

procedure TNetDataNotifyEventsThread.BCExecute;
begin
  while (not Terminated) do begin
    if (FNotifyOnReceivedHelloMessage) Or
       (FNotifyOnStatisticsChanged) Or
       (FNotifyOnNetConnectionsUpdated) Or
       (FNotifyOnNodeServersUpdated) Or 
       (FNotifyOnBlackListUpdated) then begin
      Synchronize(SynchronizedNotify);
    end;
    Sleep(10);
  end;
end;

constructor TNetDataNotifyEventsThread.Create(ANetData: TNetData);
begin
  FNetData := ANetData;
  FNotifyOnReceivedHelloMessage := false;
  FNotifyOnStatisticsChanged := false;
  FNotifyOnNetConnectionsUpdated := false;
  FNotifyOnNodeServersUpdated := false;
  FNotifyOnBlackListUpdated := false;
  inherited Create(false);
end;

procedure TNetDataNotifyEventsThread.SynchronizedNotify;
begin
  if Terminated then exit;
  if Not Assigned(FNetData) then exit;

  if FNotifyOnReceivedHelloMessage then begin
    FNotifyOnReceivedHelloMessage := false;
    If Assigned(FNetData.FOnReceivedHelloMessage) then FNetData.FOnReceivedHelloMessage(FNetData);
  end;
  if FNotifyOnStatisticsChanged then begin
    FNotifyOnStatisticsChanged := false;
    If Assigned(FNetData.FOnStatisticsChanged) then FNetData.FOnStatisticsChanged(FNetData);
  end;
  if FNotifyOnNetConnectionsUpdated then begin
    FNotifyOnNetConnectionsUpdated := false;
    If Assigned(FNetData.FOnNetConnectionsUpdated) then FNetData.FOnNetConnectionsUpdated(FNetData);
  end;
  if FNotifyOnNodeServersUpdated then begin
    FNotifyOnNodeServersUpdated := false;
    If Assigned(FNetData.FOnNodeServersUpdated) then FNetData.FOnNodeServersUpdated(FNetData);
  end;
  if FNotifyOnBlackListUpdated then begin
    FNotifyOnBlackListUpdated := false;
    If Assigned(FNetData.FOnBlackListUpdated) then FNetData.FOnBlackListUpdated(FNetData);
  end;
end;

{ TNetClientsDestroyThread }

procedure TNetClientsDestroyThread.BCExecute;
Var l,l_to_del : TList<TNetConnection>;
  i : Integer;
  LNetConnection : TNetConnection;
begin
  l_to_del := TList<TNetConnection>.Create;
  Try
    while not Terminated do begin
      l_to_del.Clear;
      l := FNetData.NetConnections.LockList;
      try
        FTerminatedAllConnections := l.Count=0;
        for i := 0 to l.Count-1 do begin
          If (TObject(l[i]) is TNetClient) And (not TNetConnection(l[i]).Connected)
            And (TNetConnection(l[i]).FDoFinalizeConnection)
            And (Not TNetConnection(l[i]).IsConnecting) then begin
            l_to_del.Add(l[i]);
          end;
        end;
      finally
        FNetData.NetConnections.UnlockList;
      end;
      sleep(500); // Delay - Sleep time before destroying (1.5.3)
      if l_to_del.Count>0 then begin
        TLog.NewLog(ltDebug,ClassName,'Destroying NetClients: '+inttostr(l_to_del.Count));
        for i := 0 to l_to_del.Count - 1 do begin
          Try
            LNetConnection := l_to_del[i];
            DebugStep := 'Destroying NetClient '+LNetConnection.ClientRemoteAddr;
            {$IFDEF AUTOREFCOUNT}
            { On Delphi mobile, the Free method is not called. We need this
            manual calls to remove this connection from TNetData lists}
            TNetData.NetData.NodeServersAddresses.DeleteNetConnection(LNetConnection);
            TNetData.NetData.FNetConnections.Remove(LNetConnection);
            TNetData.NetData.UnRegisterRequest(LNetConnection,0,0);
            {$ENDIF}
            LNetConnection.Free;
          Except
            On E:Exception do begin
              TLog.NewLog(ltError,ClassName,'Exception destroying TNetConnection '+IntToHex(PtrInt(l_to_del[i]),8)+': ('+E.ClassName+') '+E.Message );
            end;
          End;
        end;
      end;
      Sleep(100);
    end;
  Finally
    l_to_del.Free;
  end;
end;

constructor TNetClientsDestroyThread.Create(NetData: TNetData);
begin
  FNetData:=NetData;
  FTerminatedAllConnections := true;
  Inherited Create(false);
end;

procedure TNetClientsDestroyThread.WaitForTerminatedAllConnections;
var LTC : TTickCount;
begin
  LTC := TPlatform.GetTickCount;
  while (Not FTerminatedAllConnections) do begin
    if TPlatform.GetElapsedMilliseconds(LTC)>1000 then begin
      LTC := TPlatform.GetTickCount;
      TLog.NewLog(ltdebug,ClassName,'Waiting all connections terminated');
    end;
    Sleep(50);
  end;
end;

{ TNetworkAdjustedTime }

Type TNetworkAdjustedTimeReg = Record
     clientIp : String; // Client IP allows only 1 connection per IP (not using port)
     timeOffset : Integer;
     counter : Integer; // To prevent a time attack from a single IP with multiple connections, only 1 will be used for calc NAT
   End;
   PNetworkAdjustedTimeReg = ^TNetworkAdjustedTimeReg;

procedure TNetworkAdjustedTime.AddNewIp(const clientIp: String; clientTimestamp : Cardinal);
Var l : TList<Pointer>;
  i : Integer;
  P : PNetworkAdjustedTimeReg;
begin
  l := FTimesList.LockList;
  try
    i := IndexOfClientIp(l,clientIp);
    if i<0 then begin
      New(P);
      P^.clientIp := clientIp;
      P^.counter := 0;
      l.Add(P);
    end else begin
      P := l[i];
    end;
    P^.timeOffset := clientTimestamp - UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    inc(P^.counter);
    inc(FTotalCounter);
    UpdateMedian(l);
    TLog.NewLog(ltDebug,ClassName,Format('AddNewIp (%s,%d) - Total:%d/%d Offset:%d',[clientIp,clientTimestamp,l.Count,FTotalCounter,FTimeOffset]));
  finally
    FTimesList.UnlockList;
  end;
end;

constructor TNetworkAdjustedTime.Create;
begin
  FTimesList := TPCThreadList<Pointer>.Create('TNetworkAdjustedTime_TimesList');
  FTimeOffset := 0;
  FTotalCounter := 0;
end;

destructor TNetworkAdjustedTime.Destroy;
Var P : PNetworkAdjustedTimeReg;
  i : Integer;
  l : TList<Pointer>;
begin
  l := FTimesList.LockList;
  try
    for i := 0 to l.Count - 1 do begin
      P := l[i];
      Dispose(P);
    end;
    l.Clear;
  finally
    FTimesList.UnlockList;
  end;
  FreeAndNil(FTimesList);
  inherited;
end;

function TNetworkAdjustedTime.GetAdjustedTime: Cardinal;
begin
  Result := UnivDateTimeToUnix(DateTime2UnivDateTime(now)) + FTimeOffset;
end;

function TNetworkAdjustedTime.GetMaxAllowedTimestampForNewBlock: Cardinal;
var l : TList<Pointer>;
begin
  l := FTimesList.LockList;
  try
    Result := (GetAdjustedTime + CT_MaxFutureBlockTimestampOffset);
  finally
    FTimesList.UnlockList;
  end;
end;

function TNetworkAdjustedTime.IndexOfClientIp(list: TList<Pointer>; const clientIp: String): Integer;
begin
  for Result := 0 to list.Count - 1 do begin
    if SameStr(PNetworkAdjustedTimeReg(list[result])^.clientIp,clientIp) then exit;
  end;
  Result := -1;
end;

procedure TNetworkAdjustedTime.RemoveIp(const clientIp: String);
Var l : TList<Pointer>;
  i : Integer;
  P : PNetworkAdjustedTimeReg;
begin
  l := FTimesList.LockList;
  try
    i := IndexOfClientIp(l,clientIp);
    if (i>=0) then begin
      P := l[i];
      Dec(P^.counter);
      if (P^.counter<=0) then begin
        l.Delete(i);
        Dispose(P);
      end;
      Dec(FTotalCounter);
    end;
    UpdateMedian(l);
    if (i>=0) then
      TLog.NewLog(ltDebug,ClassName,Format('RemoveIp (%s) - Total:%d/%d Offset:%d',[clientIp,l.Count,FTotalCounter,FTimeOffset]))
    else TLog.NewLog(ltError,ClassName,Format('RemoveIp not found (%s) - Total:%d/%d Offset:%d',[clientIp,l.Count,FTotalCounter,FTimeOffset]))
  finally
    FTimesList.UnlockList;
  end;
end;

procedure TNetworkAdjustedTime.UpdateIp(const clientIp: String; clientTimestamp: Cardinal);
Var l : TList<Pointer>;
  i : Integer;
  P : PNetworkAdjustedTimeReg;
  lastOffset : Integer;
begin
  l := FTimesList.LockList;
  try
    i := IndexOfClientIp(l,clientIp);
    if i<0 then begin
      TLog.NewLog(ltError,ClassName,Format('UpdateIP (%s,%d) not found',[clientIp,clientTimestamp]));
      exit;
    end else begin
      P := l[i];
    end;
    lastOffset := P^.timeOffset;
    P^.timeOffset := clientTimestamp - UnivDateTimeToUnix(DateTime2UnivDateTime(now));
    if (lastOffset<>P^.timeOffset) then begin
      UpdateMedian(l);
      {$IFDEF HIGHLOG}TLog.NewLog(ltDebug,ClassName,Format('UpdateIp (%s,%d) - Total:%d/%d Offset:%d',[clientIp,clientTimestamp,l.Count,FTotalCounter,FTimeOffset]));{$ENDIF}
    end;
  finally
    FTimesList.UnlockList;
  end;
end;

{$IFDEF FPC}
type
  TPNetworkAdjustedTimeReg = class(TInterfacedObject, IComparer<Pointer>)
  public
    function Compare(constref ALeft, ARight: Pointer): Integer;
  end;

{ TPNetworkAdjustedTimeReg }

function TPNetworkAdjustedTimeReg.Compare(constref ALeft, ARight: Pointer): Integer;
begin
  Result := PNetworkAdjustedTimeReg(ALeft)^.timeOffset - PNetworkAdjustedTimeReg(ARight)^.timeOffset;
end;
{$ENDIF}

procedure TNetworkAdjustedTime.UpdateMedian(list : TList<Pointer>);
Var last : Integer;
  i : Integer;
  s : String;
{$IFNDEF FPC}
  LComparison : TComparison<Pointer>;
{$ELSE}
  LComparer : TPNetworkAdjustedTimeReg;
{$ENDIF}
begin
  last := FTimeOffset;
  {$IFDEF FPC}
  LComparer := TPNetworkAdjustedTimeReg.Create;
  try
    list.Sort(LComparer);
  finally
    LComparer.Free;
  end;
  {$ELSE}
  LComparison :=
  function(const Left, Right: Pointer): Integer
  begin
    Result := PNetworkAdjustedTimeReg(Left)^.timeOffset - PNetworkAdjustedTimeReg(Right)^.timeOffset;
  end;
  List.Sort(TComparer<Pointer>.Construct(LComparison));
  {$ENDIF}
  if list.Count<CT_MinNodesToCalcNAT then begin
    FTimeOffset := 0;
  end else if ((list.Count MOD 2)=0) then begin
    FTimeOffset := (PNetworkAdjustedTimeReg(list[(list.Count DIV 2)-1])^.timeOffset + PNetworkAdjustedTimeReg(list[(list.Count DIV 2)])^.timeOffset) DIV 2;
  end else begin
    FTimeOffset := PNetworkAdjustedTimeReg(list[list.Count DIV 2])^.timeOffset;
  end;
  if (last<>FTimeOffset) then begin
    s := '';
    for i := 0 to list.Count - 1 do begin
      s := s + ',' + IntToStr(PNetworkAdjustedTimeReg(list[i])^.timeOffset);
    end;
    TLog.NewLog(ltdebug,ClassName,
      Format('Updated NAT median offset. My offset is now %d (before %d) based on %d/%d connections %s',[FTimeOffset,last,list.Count,FTotalCounter,s]));
  end;
end;

initialization
  _NetData := Nil;
finalization
  FreeAndNil(_NetData);
end.
