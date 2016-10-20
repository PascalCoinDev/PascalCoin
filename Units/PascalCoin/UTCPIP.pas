unit UTCPIP;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

{$IFDEF FPC}
  {$mode objfpc}
{$ENDIF}

{.$DEFINE DelphiSockets}
{$DEFINE Synapse}
{$IFDEF DelphiSockets}{$IFDEF Synapse}DelphiSockets and Synapse are defined! Choose one!{$ENDIF}{$ENDIF}
{$IFNDEF DelphiSockets}{$IFNDEF Synapse}Nor DelphiSockets nor Synapse are defined! Choose one!{$ENDIF}{$ENDIF}

uses
  {$IFDEF UNIX}
  //cthreads,
  {$ENDIF}
  {$IFDEF Synapse}
  blcksock,
  synsock,  // synsock choose Socket by OS
  {$ENDIF}
  {$IFDEF DelphiSockets}
  Sockets,
  {$ENDIF}
  Classes, Sysutils,
  UThread, SyncObjs;

type
  {$IFDEF DelphiSockets}
  TTCPBlockSocket = TCustomIpClient;
  {$ENDIF}

  TNetTcpIpClient = Class(TComponent)
  private
    FTcpBlockSocket : TTCPBlockSocket;
    {$IFDEF Synapse}
    FConnected : Boolean;
    FRemoteHost : AnsiString;
    FRemotePort : Word;
    FBytesReceived, FBytesSent : Int64;
    {$ENDIF}
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FSocketError: Integer;
    function GetConnected: Boolean;
    function GetRemoteHost: AnsiString;
    function GetRemotePort: Word;
    procedure SetRemoteHost(const Value: AnsiString);
    procedure SetRemotePort(const Value: Word);
    procedure SetOnConnect(const Value: TNotifyEvent);
    procedure SetOnDisconnect(const Value: TNotifyEvent);
    procedure SetSocketError(const Value: Integer);
    {$IFDEF DelphiSockets}
    procedure TCustomIpClient_OnError(Sender: TObject; ASocketError: Integer);
    {$ENDIF}
  protected
    function ReceiveBuf(var Buf; BufSize: Integer): Integer;
    Function SendStream(Stream : TStream) : Int64;
    Procedure DoWaitForData(WaitMilliseconds : Integer; var HasData : Boolean); virtual;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function ClientRemoteAddr : AnsiString;
    Property RemoteHost : AnsiString read GetRemoteHost Write SetRemoteHost;
    Property RemotePort : Word read GetRemotePort write SetRemotePort;
    Property Connected : Boolean read GetConnected;
    Procedure Disconnect;
    Function Connect : Boolean;
    //
    Function WaitForData(WaitMilliseconds : Integer) : Boolean;
    //
    Property OnConnect : TNotifyEvent read FOnConnect write SetOnConnect;
    Property OnDisconnect : TNotifyEvent read FOnDisconnect write SetOnDisconnect;
    Function BytesReceived : Int64;
    Function BytesSent : Int64;
    Property SocketError : Integer read FSocketError write SetSocketError;
  End;

  TNetTcpIpClientClass = Class of TNetTcpIpClient;

  TBufferedNetTcpIpClient = Class;

  TBufferedNetTcpIpClientThread = Class(TPCThread)
    FBufferedNetTcpIpClient : TBufferedNetTcpIpClient;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(ABufferedNetTcpIpClient : TBufferedNetTcpIpClient);
  End;

  TBufferedNetTcpIpClient = Class(TNetTcpIpClient)
  private
    FSendBuffer : TMemoryStream;
    FReadBuffer : TMemoryStream;
    FCritical : TCriticalSection;
    FLastReadTC : Cardinal;
    FBufferedNetTcpIpClientThread : TBufferedNetTcpIpClientThread;
  protected
    Function DoWaitForDataInherited(WaitMilliseconds : Integer) : Boolean;
    Procedure DoWaitForData(WaitMilliseconds : Integer; var HasData : Boolean); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure WriteBufferToSend(SendData : TStream);
    Function ReadBufferLock : TMemoryStream;
    Procedure ReadBufferUnlock;
    Property LastReadTC : Cardinal read FLastReadTC;
  End;

  {$IFDEF Synapse}
  TNetTcpIpServer = Class;
  TTcpIpServerListenerThread = Class;

  TTcpIpSocketThread = Class(TPCThread)
  private
    FSock: TTCPBlockSocket;
    FListenerThread : TTcpIpServerListenerThread;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(AListenerThread : TTcpIpServerListenerThread; ASocket : TSocket);
    Destructor Destroy; override;
  End;

  TTcpIpServerListenerThread = Class(TPCThread)
  private
    FNetTcpIpServerServer : TNetTcpIpServer;
    FServerSocket: TTCPBlockSocket;
    FTcpIpSocketsThread : TPCThreadList;
  protected
    procedure BCExecute; override;
  public
    Constructor Create(ANetTcpIpServer : TNetTcpIpServer);
    Destructor Destroy; override;
  End;
  {$ENDIF}

  TNetTcpIpServer = Class(TObject)
  private
    {$IFDEF DelphiSockets}
    FTcpIpServer : TTcpServer;
    {$ENDIF}
    {$IFDEF Synapse}
    FTcpIpServer : TTcpIpServerListenerThread;
    FPort : Word;
    FActive : Boolean;
    {$ENDIF}
    FNetClients : TPCThreadList;
    FMaxConnections : Integer;
    FNetTcpIpClientClass : TNetTcpIpClientClass;
    function GetActive: Boolean;
    procedure SetPort(const Value: Word);  // When a connection is established to a new client, a TNetConnection is created (p2p)
    function GetPort: Word;
    procedure OnTcpServerAccept(Sender: TObject; ClientSocket: TTCPBlockSocket);
    procedure SetNetTcpIpClientClass(const Value: TNetTcpIpClientClass);
  protected
    Procedure OnNewIncommingConnection(Sender : TObject; Client : TNetTcpIpClient); virtual;
    procedure SetActive(const Value: Boolean); virtual;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Property Active : Boolean read GetActive write SetActive;
    Property Port : Word read GetPort Write SetPort;
    Property MaxConnections : Integer read FMaxConnections Write FMaxConnections;
    Property NetTcpIpClientClass : TNetTcpIpClientClass read FNetTcpIpClientClass write SetNetTcpIpClientClass;
    Function NetTcpIpClientsLock : TList;
    Procedure NetTcpIpClientsUnlock;
    Procedure WaitUntilNetTcpIpClientsFinalized;
  End;


implementation

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  UConst, ULog;

{ TNetTcpIpClient }

function TNetTcpIpClient.BytesReceived: Int64;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpBlockSocket.BytesReceived;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FBytesReceived;
  {$ENDIF}
end;

function TNetTcpIpClient.BytesSent: Int64;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpBlockSocket.BytesSent;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FBytesSent;
  {$ENDIF}
end;

function TNetTcpIpClient.ClientRemoteAddr: AnsiString;
begin
  If Assigned(FTcpBlockSocket) then begin
    {$IFDEF DelphiSockets}
    Result := FTcpBlockSocket.RemoteHost+':'+FTcpBlockSocket.RemotePort;
    {$ENDIF}
    {$IFDEF Synapse}
    if FConnected then
      Result := FTcpBlockSocket.GetRemoteSinIP+':'+IntToStr(FTcpBlockSocket.GetRemoteSinPort)
    else Result := FRemoteHost+':'+inttostr(FRemotePort);
    {$ENDIF}
  end else Result := 'NIL';
end;

function TNetTcpIpClient.Connect: Boolean;
begin
  {$IFDEF DelphiSockets}
  FSocketError := 0;
  Result := FTcpBlockSocket.Connect;
  {$ENDIF}
  {$IFDEF Synapse}
  Try
    FTcpBlockSocket.Connect(FRemoteHost,IntToStr(FRemotePort));
    FConnected := FTcpBlockSocket.LastError=0;
    if (FConnected) And (Assigned(FOnConnect)) then FOnConnect(Self)
    else TLog.NewLog(ltdebug,Classname,'Cannot connect to a server at: '+ClientRemoteAddr+' Reason: '+FTcpBlockSocket.GetErrorDescEx);
  Except
    On E:Exception do begin
      FSocketError := FTcpBlockSocket.LastError;
      TLog.NewLog(lterror,ClassName,'Error Connecting to '+ClientRemoteAddr+': '+FTcpBlockSocket.GetErrorDescEx);
      Disconnect;
    end;
  End;
  Result := FConnected;
  {$ENDIF}
end;

constructor TNetTcpIpClient.Create(AOwner : TComponent);
begin
  inherited;
  FTcpBlockSocket := Nil;
  FSocketError := 0;
  {$IFDEF DelphiSockets}
  FTcpBlockSocket := TTcpClient.Create(Nil);
  FTcpBlockSocket.OnConnect := OnConnect;
  FTcpBlockSocket.OnDisconnect := OnDisconnect;
  FTcpBlockSocket.OnError := TCustomIpClient_OnError;
  {$ENDIF}
  {$IFDEF Synapse}
  FTcpBlockSocket := TTCPBlockSocket.Create;
  FTcpBlockSocket.OnAfterConnect := OnConnect;
  FTcpBlockSocket.SocksTimeout := 10000;
  FRemoteHost := '';
  FRemotePort  := 0;
  FBytesReceived := 0;
  FBytesSent := 0;
  FConnected := False;
  {$ENDIF}
end;

destructor TNetTcpIpClient.Destroy;
begin
  Disconnect;
  inherited;
  FreeAndNil(FTcpBlockSocket);
  TLog.NewLog(ltdebug,ClassName,'Destroying Socket end');
end;

procedure TNetTcpIpClient.Disconnect;
begin
  {$IFDEF DelphiSockets}
  FTcpBlockSocket.Disconnect;
  {$ENDIF}
  {$IFDEF Synapse}
  if Not FConnected then exit;
  FConnected := false;
  FTcpBlockSocket.CloseSocket;
  if Assigned(FOnDisconnect) then FOnDisconnect(Self);
  {$ENDIF}
end;

procedure TNetTcpIpClient.DoWaitForData(WaitMilliseconds: Integer; var HasData: Boolean);
Begin
  {$IFDEF DelphiSockets}
  FSocketError := 0;
  HasData := FTcpBlockSocket.WaitForData(WaitMilliseconds);
  {$ENDIF}
  {$IFDEF Synapse}
  Try
    HasData := FTcpBlockSocket.CanRead(WaitMilliseconds);
  Except
    On E:Exception do begin
      FSocketError := FTcpBlockSocket.LastError;
      TLog.NewLog(lterror,ClassName,'Error WaitingForData from '+ClientRemoteAddr+': '+FTcpBlockSocket.GetErrorDescEx);
      Disconnect;
    end;
  End;
  {$ENDIF}
end;

function TNetTcpIpClient.GetConnected: Boolean;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpBlockSocket.Connected;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FConnected;
  {$ENDIF}
end;

function TNetTcpIpClient.GetRemoteHost: AnsiString;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpBlockSocket.RemoteHost;
  {$ENDIF}
  {$IFDEF Synapse}
  if FConnected then Result := FTcpBlockSocket.GetRemoteSinIP
  else Result := FRemoteHost;
  {$ENDIF}
end;

function TNetTcpIpClient.GetRemotePort: Word;
begin
  {$IFDEF DelphiSockets}
  Result := StrToIntDef(FTcpBlockSocket.RemotePort,0);
  {$ENDIF}
  {$IFDEF Synapse}
  if FConnected then Result := FTcpBlockSocket.GetRemoteSinPort
  else Result := FRemotePort;
  {$ENDIF}
end;

function TNetTcpIpClient.ReceiveBuf(var Buf; BufSize: Integer): Integer;
begin
  {$IFDEF DelphiSockets}
  FSocketError := 0;
  Result := FTcpBlockSocket.ReceiveBuf(Buf,BufSize);
  {$ENDIF}
  {$IFDEF Synapse}
  Try
    Result := FTcpBlockSocket.RecvBuffer(@Buf,BufSize);
    if (Result<0) Or (FTcpBlockSocket.LastError<>0) then begin
      TLog.NewLog(ltInfo,ClassName,'Closing connection from '+ClientRemoteAddr+' (Receiving error): '+Inttostr(FTcpBlockSocket.LastError)+' '+FTcpBlockSocket.GetErrorDescEx);
      Disconnect;
    end else if Result>0 then inc(FBytesReceived,Result);
  Except
    On E:Exception do begin
      FSocketError := FTcpBlockSocket.LastError;
      TLog.NewLog(lterror,ClassName,'Exception receiving buffer from '+ClientRemoteAddr+': '+FTcpBlockSocket.GetErrorDescEx);
      Disconnect;
    end;
  End;
  {$ENDIF}
end;

function TNetTcpIpClient.SendStream(Stream: TStream): Int64;
Var sp : Int64;
begin
  sp := Stream.Position;
  {$IFDEF DelphiSockets}
  FSocketError := 0;
  FTcpBlockSocket.SendStream(Stream);
  Result := Stream.Position - sp;
  {$ENDIF}
  {$IFDEF Synapse}
  Try
    FTcpBlockSocket.SendStreamRaw(Stream);
    if FTcpBlockSocket.LastError<>0 then begin
      TLog.NewLog(ltInfo,ClassName,'Closing connection from '+ClientRemoteAddr+' (Sending error): '+Inttostr(FTcpBlockSocket.LastError)+' '+FTcpBlockSocket.GetErrorDescEx);
      Result := -1;
      Disconnect;
    end else begin
      Result := Stream.Position - sp;
      inc(FBytesSent,Result);
    end;
  Except
    On E:Exception do begin
      FSocketError := FTcpBlockSocket.LastError;
      TLog.NewLog(lterror,ClassName,'Exception sending stream to '+ClientRemoteAddr+': '+FTcpBlockSocket.GetErrorDescEx);
      Disconnect;
    end;
  End;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetOnConnect(const Value: TNotifyEvent);
begin
  FOnConnect := Value;
  {$IFDEF DelphiSockets}
  FTcpBlockSocket.OnConnect := FOnConnect;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetOnDisconnect(const Value: TNotifyEvent);
begin
  FOnDisconnect := Value;
  {$IFDEF DelphiSockets}
  FTcpBlockSocket.OnDisconnect := FOnDisconnect;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetRemoteHost(const Value: AnsiString);
begin
  {$IFDEF DelphiSockets}
  FTcpBlockSocket.RemoteHost := Value;
  {$ENDIF}
  {$IFDEF Synapse}
  FRemoteHost := Value;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetRemotePort(const Value: Word);
begin
  {$IFDEF DelphiSockets}
  FTcpBlockSocket.RemotePort := IntToStr(Value);
  {$ENDIF}
  {$IFDEF Synapse}
  FRemotePort := Value;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetSocketError(const Value: Integer);
begin
  FSocketError := Value;
  if Value<>0 then
    TLog.NewLog(ltdebug,Classname,'Error '+inttohex(SocketError,8)+' with connection to '+ClientRemoteAddr);
end;

{$IFDEF DelphiSockets}
procedure TNetTcpIpClient.TCustomIpClient_OnError(Sender: TObject; ASocketError: Integer);
begin
  SocketError := ASocketError;
  Disconnect;
end;
{$ENDIF}

function TNetTcpIpClient.WaitForData(WaitMilliseconds: Integer): Boolean;
begin
  DoWaitForData(WaitMilliseconds,Result);
end;

{ TBufferedNetTcpIpClientThread }

procedure TBufferedNetTcpIpClientThread.BCExecute;
var SendBuffStream : TStream;
  ReceiveBuffer : Array[0..4095] of byte;
  Procedure DoReceiveBuf;
  var last_bytes_read : Integer;
    ms : TMemoryStream;
    lastpos : Int64;
  begin
    If FBufferedNetTcpIpClient.DoWaitForDataInherited(10) then begin
      last_bytes_read := 0;
      repeat
        if last_bytes_read<>0 then begin
          // This is to prevent a 4096 buffer transmission only... and a loop
          If Not FBufferedNetTcpIpClient.DoWaitForDataInherited(10) then begin
            if FBufferedNetTcpIpClient.SocketError<>0 then FBufferedNetTcpIpClient.Disconnect
            else exit;
          end;
        end;

        last_bytes_read := FBufferedNetTcpIpClient.ReceiveBuf(ReceiveBuffer,sizeof(ReceiveBuffer));
        if (last_bytes_read>0) then begin
          ms := FBufferedNetTcpIpClient.ReadBufferLock;
          Try
            FBufferedNetTcpIpClient.FLastReadTC := GetTickCount;
            lastpos := ms.Position;
            ms.Position := ms.Size;
            ms.Write(ReceiveBuffer,last_bytes_read);
            ms.Position := lastpos;
            TLog.NewLog(ltdebug,ClassName,Format('Received %d bytes. Buffer length: %d bytes',[last_bytes_read,ms.Size]));
          Finally
            FBufferedNetTcpIpClient.ReadBufferUnlock;
          End;
        end;
      until (last_bytes_read<sizeof(ReceiveBuffer));
    end else begin
      if FBufferedNetTcpIpClient.SocketError<>0 then FBufferedNetTcpIpClient.Disconnect;
    end;
  end;
  Procedure DoSendBuf;
  begin
    FBufferedNetTcpIpClient.FCritical.Acquire;
    Try
      if FBufferedNetTcpIpClient.FSendBuffer.Size>0 then begin
        SendBuffStream.Size := 0;
        SendBuffStream.CopyFrom(FBufferedNetTcpIpClient.FSendBuffer,0);
        FBufferedNetTcpIpClient.FSendBuffer.Size := 0;
      end;
    Finally
      FBufferedNetTcpIpClient.FCritical.Release;
    End;
    if (SendBuffStream.Size>0) then begin
      SendBuffStream.Position := 0;
      FBufferedNetTcpIpClient.SendStream(SendBuffStream);
      TLog.NewLog(ltdebug,ClassName,Format('Sent %d bytes',[SendBuffStream.Size]));
      SendBuffStream.Size := 0;
    end;
  end;
begin
  SendBuffStream := TMemoryStream.Create;
  try
    while (Not Terminated) do begin
      while (Not Terminated) And (Not FBufferedNetTcpIpClient.Connected) do sleep(100);
      if (FBufferedNetTcpIpClient.Connected) then begin
        // Receive data
        DoReceiveBuf;
        // Send Data
        DoSendBuf;
      end else FBufferedNetTcpIpClient.FLastReadTC := GetTickCount;
      // Sleep
      Sleep(10); // Slepp 10 is better than sleep 1
    end;
  Finally
    SendBuffStream.Free;
  end;
end;

constructor TBufferedNetTcpIpClientThread.Create(
  ABufferedNetTcpIpClient: TBufferedNetTcpIpClient);
begin
  FBufferedNetTcpIpClient := ABufferedNetTcpIpClient;
  inherited Create(false);
end;

{ TBufferedNetTcpIpClient }

constructor TBufferedNetTcpIpClient.Create(AOwner: TComponent);
begin
  inherited;
  FLastReadTC := GetTickCount;
  FCritical := SyncObjs.TCriticalSection.Create;
  FSendBuffer := TMemoryStream.Create;
  FReadBuffer := TMemoryStream.Create;
  FBufferedNetTcpIpClientThread := TBufferedNetTcpIpClientThread.Create(Self);
end;

destructor TBufferedNetTcpIpClient.Destroy;
begin
  FBufferedNetTcpIpClientThread.Terminate;
  FBufferedNetTcpIpClientThread.WaitFor;
  FreeAndNil(FBufferedNetTcpIpClientThread);
  FreeAndNil(FCritical);
  FreeAndNil(FReadBuffer);
  FreeAndNil(FSendBuffer);
  inherited;
end;

procedure TBufferedNetTcpIpClient.DoWaitForData(WaitMilliseconds: Integer; var HasData: Boolean);
begin
  FCritical.Acquire;
  try
    if FReadBuffer.Size>0 then begin
      HasData := True;
      exit;
    end;
  finally
    FCritical.Release;
  end;
  inherited DoWaitForData(WaitMilliseconds,HasData);
end;

function TBufferedNetTcpIpClient.DoWaitForDataInherited(WaitMilliseconds : Integer) : Boolean;
begin
  inherited DoWaitForData(WaitMilliseconds,Result);
end;

function TBufferedNetTcpIpClient.ReadBufferLock: TMemoryStream;
begin
  FCritical.Acquire;
  Result := FReadBuffer;
end;

procedure TBufferedNetTcpIpClient.ReadBufferUnlock;
begin
  FCritical.Release;
end;

procedure TBufferedNetTcpIpClient.WriteBufferToSend(SendData: TStream);
var lastpos : Int64;
begin
  FCritical.Acquire;
  try
    lastpos := FSendBuffer.Position;
    FSendBuffer.Position := FSendBuffer.Size;
    SendData.Position := 0;
    FSendBuffer.CopyFrom(SendData,SendData.Size);
    FSendBuffer.Position := lastpos;
  finally
    FCritical.Release;
  end;
end;

{ TNetTcpIpServer }

constructor TNetTcpIpServer.Create;
begin
  FNetTcpIpClientClass := TNetTcpIpClient;
  FTcpIpServer := Nil;
  FMaxConnections := 10;
  {$IFDEF DelphiSockets}
  FTcpIpServer := TTcpServer.Create(Nil);
  FTcpIpServer.OnAccept := OnTcpServerAccept;
  FTcpIpServer.ServerSocketThread.ThreadCacheSize := CT_MaxClientsConnected;
  {$ELSE}
  FActive := false;
  {$ENDIF}
  FNetClients := TPCThreadList.Create;
end;

destructor TNetTcpIpServer.Destroy;
begin
  Active := false;
  {$IFDEF DelphiSockets}
  FreeAndNil(FTcpIpServer);
  {$ENDIF}
  inherited;
  FreeAndNil(FNetClients);
end;

function TNetTcpIpServer.GetActive: Boolean;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpServer.Active;
  {$ELSE}
  Result := Assigned(FTcpIpServer) And (FActive);
  {$ENDIF}
end;

function TNetTcpIpServer.GetPort: Word;
begin
  {$IFDEF DelphiSockets}
  Result := StrToIntDef(FTcpIpServer.LocalPort,0);
  {$ELSE}
  Result := FPort;
  {$ENDIF}
end;

function TNetTcpIpServer.NetTcpIpClientsLock: TList;
begin
  Result := FNetClients.LockList;
end;

procedure TNetTcpIpServer.NetTcpIpClientsUnlock;
begin
  FNetClients.UnlockList;
end;

procedure TNetTcpIpServer.OnNewIncommingConnection(Sender: TObject; Client: TNetTcpIpClient);
begin
  //
end;

procedure TNetTcpIpServer.OnTcpServerAccept(Sender: TObject; ClientSocket: TTCPBlockSocket);
Var n : TNetTcpIpClient;
  oldSocket : TTCPBlockSocket;
begin
  {$IFDEF DelphiSockets}
  If FTcpIpServer.ServerSocketThread.ThreadCacheSize <> MaxConnections then
      FTcpIpServer.ServerSocketThread.ThreadCacheSize := MaxConnections;
  {$ENDIF}

  n := FNetTcpIpClientClass.Create(Nil);
  Try
    oldSocket := n.FTcpBlockSocket;
    n.FTcpBlockSocket := ClientSocket;
    {$IFDEF Synapse}
    n.FConnected := True;
    n.RemoteHost := ClientSocket.GetRemoteSinIP;
    n.RemotePort := ClientSocket.GetRemoteSinPort;
    {$ENDIF}
    FNetClients.Add(n);
    try
      OnNewIncommingConnection(Sender,n);
    finally
      FNetClients.Remove(n);
    end;
  Finally
    n.FTcpBlockSocket := oldSocket;
    FreeAndNil(n);
  End;
end;

procedure TNetTcpIpServer.SetActive(const Value: Boolean);
begin
  {$IFDEF DelphiSockets}
  FTcpIpServer.Active := Value;
  {$ELSE}
  if Value then begin
    if (Assigned(FTcpIpServer)) then exit;
    FTcpIpServer := TTcpIpServerListenerThread.Create(Self);
    FActive := true;
  end else begin
    if (Not Assigned(FTcpIpServer)) then exit;
    FActive := false;
    FTcpIpServer.Terminate;
    FTcpIpServer.WaitFor;
    FreeAndNil(FTcpIpServer);
  end;
  {$ENDIF}
end;

procedure TNetTcpIpServer.SetNetTcpIpClientClass(const Value: TNetTcpIpClientClass);
begin
  if FNetTcpIpClientClass=Value then exit;
  FNetTcpIpClientClass := Value;
  Active := false;
end;

procedure TNetTcpIpServer.SetPort(const Value: Word);
begin
  {$IFDEF DelphiSockets}
  FTcpIpServer.LocalPort := IntToStr(Value);
  {$ELSE}
  FPort := Value;
  {$ENDIF}
end;

procedure TNetTcpIpServer.WaitUntilNetTcpIpClientsFinalized;
Var l : TList;
begin
  if Active then Active := false;
  Repeat
    l := FNetClients.LockList;
    try
      if (l.Count=0) then exit;
    finally
      FNetClients.UnlockList;
    end;
    sleep(10);
  Until false;
end;

{$IFDEF Synapse}
{ TTcpIpServerListenerThread }

procedure TTcpIpServerListenerThread.BCExecute;
var ClientSocket: TSocket;
    ClientThread: TTcpIpSocketThread;
    lSockets : TList;
    i : Integer;
begin
  FServerSocket.CreateSocket;
  if FServerSocket.LastError<>0 then begin
    TLog.NewLog(lterror,Classname,'Error initializing the socket: '+FServerSocket.GetErrorDescEx);
    exit;
  end;
  FServerSocket.Family := SF_IP4;
  FServerSocket.SetLinger(true,10000);
  FServerSocket.Bind('0.0.0.0',IntToStr(FNetTcpIpServerServer.Port));
  if FServerSocket.LastError<>0 then begin
    TLog.NewLog(lterror,Classname,'Cannot bind port '+IntToStr(FNetTcpIpServerServer.Port)+': '+FServerSocket.GetErrorDescEx);
    exit;
  end;
  FServerSocket.Listen;
  lSockets := TList.Create;
  try
    while (Not Terminated) And (FNetTcpIpServerServer.Active) do begin
      If (FServerSocket.CanRead(100)) And (lSockets.Count<FNetTcpIpServerServer.MaxConnections) then begin
        ClientSocket := FServerSocket.Accept;
        if FServerSocket.LastError = 0 then begin
          ClientThread := TTcpIpSocketThread.Create(Self,ClientSocket);
          lSockets.Add(ClientThread);
          ClientThread.Suspended := false;
        end;
      end;
      // Clean finished threads
      for i := lSockets.Count - 1 downto 0 do begin
        ClientThread := TTcpIpSocketThread(lSockets[i]);
        if ClientThread.Terminated then begin
          lSockets.Delete(i);
          ClientThread.Free;
        end;
      end;
      // Wait
      sleep(10); // Sleep 10 is better than sleep 1
    End;
  finally
    // Finalize all threads
    for i := 0 to lSockets.Count - 1 do begin
      // Here we no wait until terminated...
      TTcpIpSocketThread(lSockets[i]).FListenerThread := Nil;
      TTcpIpSocketThread(lSockets[i]).Terminate;
      TTcpIpSocketThread(lSockets[i]).WaitFor;
      TTcpIpSocketThread(lSockets[i]).Free;
    end;
    lSockets.free;
  end;
end;

constructor TTcpIpServerListenerThread.Create(ANetTcpIpServer: TNetTcpIpServer);
begin
  FServerSocket := TTCPBlockSocket.Create;
  FNetTcpIpServerServer := ANetTcpIpServer;
  FNetTcpIpServerServer.FTcpIpServer := Self;
  inherited Create(false);
end;

destructor TTcpIpServerListenerThread.Destroy;
begin
  FNetTcpIpServerServer.FTcpIpServer := Nil;
  FServerSocket.Free;
  inherited;
end;

{ TTcpIpSocketThread }

procedure TTcpIpSocketThread.BCExecute;
begin
  if (Not Terminated) And (Assigned(FSock)) And (Assigned(FListenerThread)) then
    FListenerThread.FNetTcpIpServerServer.OnTcpServerAccept(Self,FSock);
end;

constructor TTcpIpSocketThread.Create(AListenerThread: TTcpIpServerListenerThread; ASocket: TSocket);
begin
  FSock := TTCPBlockSocket.Create;
  FSock.Socket := ASocket;
  FListenerThread := AListenerThread;
  inherited Create(true);
end;

destructor TTcpIpSocketThread.Destroy;
begin
  Try
    if FSock.Socket<>INVALID_SOCKET then begin
      FSock.CloseSocket;
    end;
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Error closign socket: '+E.Message);
    end;
  End;
  Try
    FreeAndNil(FSock);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Error destroying socket: '+E.Message);
    end;
  End;
  inherited;
end;

{$ENDIF}

end.
