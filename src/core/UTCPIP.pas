unit UTCPIP;

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

interface

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{$I config.inc}

{
  Change log: 2019-01-31
  Due to Android development, can't use Synapse and will use Indy components provided by Delphi (See confi.inc file)

}

{$IFDEF DelphiSockets}{$IFDEF Synapse}DelphiSockets and Synapse are defined! Choose one!{$ENDIF}{$ENDIF}
{$IFNDEF DelphiSockets}{$IFNDEF Synapse}Nor DelphiSockets nor Synapse are defined! Choose one!{$ENDIF}{$ENDIF}

uses
  {$IFDEF Synapse}
  blcksock,
  synsock,  // synsock choose Socket by OS
  {$ENDIF}
  {$IFDEF DelphiSockets}
  IdTcpClient, IdTCPServer, IdGlobal, IdContext, IdTCPConnection,
  {$ENDIF}
  Classes, Sysutils, UBaseTypes,
  UThread,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF},
  SyncObjs;

type
  {$IFDEF DelphiSockets}
  TTCPBlockSocket = TIdTCPConnection;
  {$ENDIF}

  { TNetTcpIpClient }

  TNetTcpIpClient = Class(TComponent)
  private
    FTcpBlockSocket : TTCPBlockSocket;
    FBytesReceived, FBytesSent : Int64;
    FConnected : Boolean;
    FLock : TPCCriticalSection;
    {$IFDEF Synapse}
    FRemoteHost : String;
    FRemotePort : Word;
    FSendBufferLock : TPCCriticalSection;
    {$ENDIF}
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FSocketError: Integer;
    FLastCommunicationTime : TDateTime;
    function GetConnected: Boolean;
    function GetRemoteHost: String;
    function GetRemotePort: Word;
    procedure SetRemoteHost(const Value: String);
    procedure SetRemotePort(const Value: Word);
    procedure SetOnConnect(const Value: TNotifyEvent);
    procedure SetOnDisconnect(const Value: TNotifyEvent);
    procedure SetSocketError(const Value: Integer);
    {$IFDEF DelphiSockets}
    procedure TCustomIpClient_OnError(Sender: TObject; ASocketError: Integer);
    {$ENDIF}
  protected
    Procedure DoOnConnect; Virtual;
    function ReceiveBuf(var Buf; BufSize: Integer): Integer;
    Function SendStream(Stream : TStream) : Int64;
    Procedure DoWaitForData(WaitMilliseconds : Integer; var HasData : Boolean); virtual;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function ClientRemoteAddr : String;
    Property RemoteHost : String read GetRemoteHost Write SetRemoteHost;
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
    Property LastCommunicationTime : TDateTime read FLastCommunicationTime;
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
    FCritical : TPCCriticalSection;
    FLastReadTC : TTickCount;
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
    Property LastReadTC : TTickCount read FLastReadTC;
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
  protected
    procedure BCExecute; override;
  public
    Constructor Create(ANetTcpIpServer : TNetTcpIpServer);
    Destructor Destroy; override;
  End;
  {$ENDIF}

  { TNetTcpIpServer }

  TNetTcpIpServer = Class(TObject)
  private
    {$IFDEF DelphiSockets}
    FTcpIpServer : TIdTcpServer;
    {$ENDIF}
    {$IFDEF Synapse}
    FTcpIpServer : TTcpIpServerListenerThread;
    FPort : Word;
    FActive : Boolean;
    {$ENDIF}
    FNetClients : TPCThreadList<TNetTcpIpClient>;
    FMaxConnections : Integer;
    FNetTcpIpClientClass : TNetTcpIpClientClass;
    function GetActive: Boolean;
    procedure SetPort(const Value: Word);  // When a connection is established to a new client, a TNetConnection is created (p2p)
    function GetPort: Word;
    procedure OnTcpServerAccept({$IFDEF DelphiSockets}AContext: TIdContext{$ELSE}Sender: TObject; ClientSocket: TTCPBlockSocket{$ENDIF});
    procedure SetNetTcpIpClientClass(const Value: TNetTcpIpClientClass);
  protected
    Procedure OnNewIncommingConnection(Sender : TObject; Client : TNetTcpIpClient); virtual;
    procedure SetActive(const Value: Boolean); virtual;
    procedure SetMaxConnections(AValue: Integer); virtual;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Property Active : Boolean read GetActive write SetActive;
    Property Port : Word read GetPort Write SetPort;
    Property MaxConnections : Integer read FMaxConnections Write SetMaxConnections;
    Property NetTcpIpClientClass : TNetTcpIpClientClass read FNetTcpIpClientClass write SetNetTcpIpClientClass;
    Function NetTcpIpClientsLock : TList<TNetTcpIpClient>;
    Procedure NetTcpIpClientsUnlock;
    Procedure WaitUntilNetTcpIpClientsFinalized;
  End;


implementation

uses
{$IFnDEF FPC}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
{$ELSE}
  {LCLIntf, LCLType, LMessages,}
{$ENDIF}
  UConst, ULog;

{ TNetTcpIpClient }

function TNetTcpIpClient.BytesReceived: Int64;
begin
  Result := FBytesReceived;
end;

function TNetTcpIpClient.BytesSent: Int64;
begin
  Result := FBytesSent;
end;

function TNetTcpIpClient.ClientRemoteAddr: String;
begin
  If Assigned(FTcpBlockSocket) then begin
    {$IFDEF DelphiSockets}
    if (FTcpBlockSocket is TIdTCPClient) then
      Result := TIdTCPClient(FTcpBlockSocket).Host+':'+IntToStr(TIdTCPClient(FTcpBlockSocket).Port)
    else if Assigned(FTcpBlockSocket.IOHandler) then begin
      Result := FTcpBlockSocket.IOHandler.Host+':'+IntToStr(FTcpBlockSocket.IOHandler.Port);
    end else Result := '';
    {$ENDIF}
    {$IFDEF Synapse}
    Result := FRemoteHost+':'+inttostr(FRemotePort);
    {$ENDIF}
  end else Result := 'NIL';
end;

function TNetTcpIpClient.Connect: Boolean;
begin
  FLock.Acquire;
  try
    {$IFDEF DelphiSockets}
    FSocketError := 0;
    Try
      (FTcpBlockSocket as TIdTCPClient).Connect;
      FConnected := True;
      Result := True;
    Except
      On E:Exception do begin
        Result := False;
        {$IFDEF HIGHLOG}TLog.NewLog(lterror,ClassName,'Cannot connect to a server at: '+ClientRemoteAddr+' Reason: ('+E.ClassName+') '+E.Message);{$ENDIF}
        Disconnect;
      end;
    End;
    {$ENDIF}
    {$IFDEF Synapse}
    Try
      FTcpBlockSocket.Connect(FRemoteHost,IntToStr(FRemotePort));
      FConnected := FTcpBlockSocket.LastError=0;
      if (FConnected) then begin
        FRemoteHost := FTcpBlockSocket.GetRemoteSinIP;
        FRemotePort := FTcpBlockSocket.GetRemoteSinPort;
        DoOnConnect;
      end{$IFDEF HIGHLOG} else TLog.NewLog(ltdebug,Classname,'Cannot connect to a server at: '+ClientRemoteAddr+' Reason: '+FTcpBlockSocket.GetErrorDescEx){$ENDIF};
    Except
      On E:Exception do begin
        SocketError := FTcpBlockSocket.LastError;
        TLog.NewLog(lterror,ClassName,'Error Connecting to '+ClientRemoteAddr+': '+FTcpBlockSocket.GetErrorDescEx);
        Disconnect;
      end;
    End;
    {$ENDIF}
  finally
    FLock.Release;
  end;
  Result := FConnected;
end;

constructor TNetTcpIpClient.Create(AOwner : TComponent);
begin
  inherited;
  FOnConnect := Nil;
  FOnDisconnect := Nil;
  FTcpBlockSocket := Nil;
  FSocketError := 0;
  FLastCommunicationTime := 0;
  {$IFDEF DelphiSockets}
  FTcpBlockSocket := TIdTCPClient.Create(Nil);
  TIdTCPClient(FTcpBlockSocket).OnConnected := OnConnect;
  FTcpBlockSocket.OnDisconnected := OnDisconnect;
  TIdTCPClient(FTcpBlockSocket).ConnectTimeout := 5000;
  TIdTCPClient(FTcpBlockSocket).ReadTimeout := 5000;
  {$ENDIF}
  FLock := TPCCriticalSection.Create('TNetTcpIpClient_Lock');
  {$IFDEF Synapse}
  FSendBufferLock := TPCCriticalSection.Create('TNetTcpIpClient_SendBufferLock');
  FTcpBlockSocket := TTCPBlockSocket.Create;
  FTcpBlockSocket.OnAfterConnect := OnConnect;
  FTcpBlockSocket.SocksTimeout := 5000; //Build 1.5.0 was 10000;
  FTcpBlockSocket.ConnectionTimeout := 5000; // Build 1.5.0 was default
  FRemoteHost := '';
  FRemotePort  := 0;
  {$ENDIF}
  FConnected := False;
  FBytesReceived := 0;
  FBytesSent := 0;
end;

destructor TNetTcpIpClient.Destroy;
begin
  Disconnect;
  {$IFDEF Synapse}  // Memory leak on 1.5.0
  FreeAndNil(FSendBufferLock);
  {$ENDIF}
  FreeAndNil(FLock);
  inherited;
  FreeAndNil(FTcpBlockSocket);
  {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,'Destroying Socket end');{$ENDIF}
end;

procedure TNetTcpIpClient.Disconnect;
Var DebugStep : String;
begin
  if Not FConnected then exit;
  Try
    DebugStep := '';
    FLock.Acquire;
    Try
      if Not FConnected then begin
        exit; // Protection inside lock thread to prevent double disconnect sessions
      end;
      {$IFDEF DelphiSockets}
      Try
        FTcpBlockSocket.Disconnect;
      Finally
        FConnected := False;
      End;
      {$ENDIF}
      {$IFDEF Synapse}
      DebugStep := 'disconnecting';
      if Not FConnected then exit;
      DebugStep := 'Closing socket';
      FTcpBlockSocket.CloseSocket;
      DebugStep := 'Relasing flock';
      FConnected := false;
      DebugStep := 'Calling OnDisconnect';
      if Assigned(FOnDisconnect) then FOnDisconnect(Self)
      else TLog.NewLog(ltError,ClassName,'OnDisconnect is nil');
      {$ENDIF}
    Finally
      FLock.Release;
    End;
  Except
    On E:Exception do begin
      E.Message := 'Exception at TNetTcpIpClient.Discconnect step '+DebugStep+' - '+E.Message;
      Raise;
    end;
  end;
end;

procedure TNetTcpIpClient.DoOnConnect;
begin
  If (Assigned(FOnConnect)) then FOnConnect(Self);
end;

procedure TNetTcpIpClient.DoWaitForData(WaitMilliseconds: Integer; var HasData: Boolean);
Begin
  FLock.Acquire;
  Try
    if Not FConnected then begin
      HasData := False;
      Exit;
    end;
    {$IFDEF DelphiSockets}
    FSocketError := 0;
    HasData := Not FTcpBlockSocket.IOHandler.InputBufferIsEmpty;
    if Not HasData then begin
      FTcpBlockSocket.IOHandler.CheckForDataOnSource(WaitMilliseconds);
      HasData := Not FTcpBlockSocket.IOHandler.InputBufferIsEmpty;
    end;
    {$ENDIF}
    {$IFDEF Synapse}
    Try
      HasData := FTcpBlockSocket.CanRead(WaitMilliseconds);
    Except
      On E:Exception do begin
        SocketError := FTcpBlockSocket.LastError;
        HasData := false;
        TLog.NewLog(lterror,ClassName,'Error WaitingForData from '+ClientRemoteAddr+': '+FTcpBlockSocket.GetErrorDescEx);
        Disconnect;
      end;
    End;
    {$ENDIF}
  Finally
    FLock.Release;
  End;
end;

function TNetTcpIpClient.GetConnected: Boolean;
begin
  {$IFDEF DelphiSockets}
  Result := FConnected;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FConnected;
  {$ENDIF}
end;

function TNetTcpIpClient.GetRemoteHost: String;
begin
  {$IFDEF DelphiSockets}
  if (FTcpBlockSocket is TIdTCPClient) then
    Result := TIdTCPClient(FTcpBlockSocket).Host
  else if Assigned(FTcpBlockSocket.IOHandler) then begin
    Result := FTcpBlockSocket.IOHandler.Host
  end else Result := '';
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FRemoteHost;
  {$ENDIF}
end;

function TNetTcpIpClient.GetRemotePort: Word;
begin
  {$IFDEF DelphiSockets}
  if (FTcpBlockSocket is TIdTCPClient) then
    Result := TIdTCPClient(FTcpBlockSocket).Port
  else if Assigned(FTcpBlockSocket.IOHandler) then begin
    Result := FTcpBlockSocket.IOHandler.Port;
  end else Result := 0;
  {$ENDIF}
  {$IFDEF Synapse}
  Result := FRemotePort;
  {$ENDIF}
end;

function TNetTcpIpClient.ReceiveBuf(var Buf; BufSize: Integer): Integer;
{$IFDEF DelphiSockets}
var LBytes : TIdBytes;
  LHasData : Boolean;
  LExtractBytes : Integer;
{$ENDIF}
begin
  Result := 0;
  FLock.Acquire;
  Try
    {$IFDEF DelphiSockets}
    FSocketError := 0;
    if FTcpBlockSocket.IOHandler.InputBuffer.Size<=0 then begin
      LHasData := FTcpBlockSocket.IOHandler.CheckForDataOnSource(1);
    end else LHasData := True;
    if LHasData then begin
      LExtractBytes := FTcpBlockSocket.IOHandler.InputBuffer.Size;
      if LExtractBytes>BufSize then LExtractBytes := BufSize;
      FTcpBlockSocket.IOHandler.InputBuffer.ExtractToBytes(LBytes,LExtractBytes);
      move(LBytes[0],buf,Length(LBytes));
      Result := Length(LBytes);
      inc(FBytesReceived,Result);
    end else Result := 0;
    {$ENDIF}
    {$IFDEF Synapse}
    Try
      Result := FTcpBlockSocket.RecvBuffer(@Buf,BufSize);
      if (Result<0) Or (FTcpBlockSocket.LastError<>0) then begin
        TLog.NewLog(ltDebug,ClassName,'Closing connection from '+ClientRemoteAddr+' (Receiving error): '+Inttostr(FTcpBlockSocket.LastError)+' '+FTcpBlockSocket.GetErrorDescEx);
        Result := 0;
        Disconnect;
      end else if Result>0 then inc(FBytesReceived,Result);
    Except
      On E:Exception do begin
        SocketError := FTcpBlockSocket.LastError;
        TLog.NewLog(lterror,ClassName,'Exception receiving buffer from '+ClientRemoteAddr+' '+FTcpBlockSocket.GetErrorDescEx+' ('+E.ClassName+'):'+E.Message);
        Disconnect;
      end;
    End;
    {$ENDIF}
  Finally
    FLock.Release;
  End;
  if Result>0 then FLastCommunicationTime := Now;
end;

function TNetTcpIpClient.SendStream(Stream: TStream): Int64;
Var sp : Int64;
  unlocked : Boolean;
begin
  sp := Stream.Position;
  {$IFDEF DelphiSockets}
  FSocketError := 0;
  FTcpBlockSocket.IOHandler.Write(Stream);
  Result := Stream.Position - sp;
  inc(FBytesSent,Result);
  {$ENDIF}
  {$IFDEF Synapse}
  Result := 0;
  unlocked := false;
  // In order to allow a big stream sending, will cut up in small blocks
  FSendBufferLock.Acquire;
  Try
    Try
      FTcpBlockSocket.SendStreamRaw(Stream);
      if FTcpBlockSocket.LastError<>0 then begin
        TLog.NewLog(ltDebug,ClassName,'Closing connection from '+ClientRemoteAddr+' (Sending error): '+Inttostr(FTcpBlockSocket.LastError)+' '+FTcpBlockSocket.GetErrorDescEx);
        Result := -1;
        unlocked := true;
        FSendBufferLock.Release;
        Disconnect;
      end else begin
        Result := Stream.Position - sp;
        inc(FBytesSent,Result);
      end;
    Except
      On E:Exception do begin
        SocketError := FTcpBlockSocket.LastError;
        TLog.NewLog(lterror,ClassName,'Exception sending stream to '+ClientRemoteAddr+': '+FTcpBlockSocket.GetErrorDescEx);
        unlocked := true;
        FSendBufferLock.Release;
        Disconnect;
      end;
    End;
  Finally
    If not unlocked then FSendBufferLock.Release;
  end;
  {$ENDIF}
  if Result>0 then FLastCommunicationTime := Now;
end;

procedure TNetTcpIpClient.SetOnConnect(const Value: TNotifyEvent);
begin
  FOnConnect := Value;
  {$IFDEF DelphiSockets}
  if (FTcpBlockSocket is TIdTCPClient) then
    TIdTCPClient(FTcpBlockSocket).OnConnected := FOnConnect;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetOnDisconnect(const Value: TNotifyEvent);
begin
  FOnDisconnect := Value;
  {$IFDEF DelphiSockets}
  FTcpBlockSocket.OnDisconnected := FOnDisconnect;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetRemoteHost(const Value: String);
begin
  {$IFDEF DelphiSockets}
  if (FTcpBlockSocket is TIdTCPClient) then
    TIdTCPClient(FTcpBlockSocket).Host := Value;
  {$ENDIF}
  {$IFDEF Synapse}
  FRemoteHost := Value;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetRemotePort(const Value: Word);
begin
  {$IFDEF DelphiSockets}
  if (FTcpBlockSocket is TIdTCPClient) then
    TIdTCPClient(FTcpBlockSocket).Port := Value;
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
    total_read, total_size : Int64;
    ms : TMemoryStream;
    lastpos : Int64;
  begin
    total_read := 0; total_size := 0;
    If FBufferedNetTcpIpClient.DoWaitForDataInherited(10) then begin
      last_bytes_read := 0;
      repeat
        if last_bytes_read<>0 then begin
          // This is to prevent a 4096 buffer transmission only... and a loop
          If Not FBufferedNetTcpIpClient.DoWaitForDataInherited(10) then begin
            if FBufferedNetTcpIpClient.SocketError<>0 then FBufferedNetTcpIpClient.Disconnect;
            exit;
          end;
        end;


        last_bytes_read := FBufferedNetTcpIpClient.ReceiveBuf(ReceiveBuffer,sizeof(ReceiveBuffer));
        if (last_bytes_read>0) then begin
          ms := FBufferedNetTcpIpClient.ReadBufferLock;
          Try
            FBufferedNetTcpIpClient.FLastReadTC := TPlatform.GetTickCount;
            lastpos := ms.Position;
            ms.Position := ms.Size;
            ms.Write(ReceiveBuffer,last_bytes_read);
            ms.Position := lastpos;
            inc(total_read,last_bytes_read);
            total_size := ms.Size;
          Finally
            FBufferedNetTcpIpClient.ReadBufferUnlock;
          End;
        end;
      until (last_bytes_read<sizeof(ReceiveBuffer)) Or (Terminated) Or (Not FBufferedNetTcpIpClient.Connected);
      {$IFDEF HIGHLOG}If total_read>0 then TLog.NewLog(ltdebug,ClassName,Format('Received %d bytes. Buffer length: %d bytes',[total_read,total_size]));{$ENDIF}
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
      {$IFDEF HIGHLOG}TLog.NewLog(ltdebug,ClassName,Format('Sent %d bytes',[SendBuffStream.Size]));{$ENDIF}
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
        If (Not Terminated) And (FBufferedNetTcpIpClient.Connected) then DoReceiveBuf;
        // Send Data
        If (Not Terminated) And (FBufferedNetTcpIpClient.Connected) then DoSendBuf;
      end else FBufferedNetTcpIpClient.FLastReadTC := TPlatform.GetTickCount;
      // Sleep
      Sleep(10); // Slepp 10 is better than sleep 1
    end;
  Finally
    SendBuffStream.Free;
  end;
end;

constructor TBufferedNetTcpIpClientThread.Create(ABufferedNetTcpIpClient: TBufferedNetTcpIpClient);
begin
  FBufferedNetTcpIpClient := ABufferedNetTcpIpClient;
  inherited Create(false);
end;

{ TBufferedNetTcpIpClient }

constructor TBufferedNetTcpIpClient.Create(AOwner: TComponent);
begin
  inherited;
  FLastReadTC := TPlatform.GetTickCount;
  FCritical := TPCCriticalSection.Create('TBufferedNetTcpIpClient_Critical');
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
  FMaxConnections := CT_MaxClientsConnected;
  {$IFDEF DelphiSockets}
  FTcpIpServer := TIdTCPServer.Create(Nil);
  FTcpIpServer.OnExecute := OnTcpServerAccept;
  FTcpIpServer.MaxConnections := CT_MaxClientsConnected;
  {$ELSE}
  FActive := false;
  {$ENDIF}
  FNetClients := TPCThreadList<TNetTcpIpClient>.Create('TNetTcpIpServer_NetClients');
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

procedure TNetTcpIpServer.SetMaxConnections(AValue: Integer);
begin
  if FMaxConnections=AValue then Exit;
  FMaxConnections:=AValue;
  {$IFDEF DelphiSockets}
  FTcpIpServer.MaxConnections := AValue;
  {$ENDIF}
end;

function TNetTcpIpServer.GetPort: Word;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpServer.DefaultPort;
  {$ELSE}
  Result := FPort;
  {$ENDIF}
end;

function TNetTcpIpServer.NetTcpIpClientsLock: TList<TNetTcpIpClient>;
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

procedure TNetTcpIpServer.OnTcpServerAccept({$IFDEF DelphiSockets}AContext: TIdContext{$ELSE}Sender: TObject; ClientSocket: TTCPBlockSocket{$ENDIF});
Var n : TNetTcpIpClient;
  oldSocket : TTCPBlockSocket;
begin
  n := FNetTcpIpClientClass.Create(Nil);
  Try
    n.FLock.Acquire;
    try
      oldSocket := n.FTcpBlockSocket;
      {$IFDEF DelphiSockets}
      n.FTcpBlockSocket := AContext.Connection;
      {$ELSE}
      n.FTcpBlockSocket := ClientSocket;
      {$ENDIF}
      n.FConnected := True;
      {$IFDEF Synapse}
      n.RemoteHost := ClientSocket.GetRemoteSinIP;
      n.RemotePort := ClientSocket.GetRemoteSinPort;
      ClientSocket.SocksTimeout := 5000; //New 1.5.1
      ClientSocket.ConnectionTimeout := 5000; // New 1.5.1
      {$ENDIF}
    finally
      n.FLock.Release;
    end;
    FNetClients.Add(n);
    try
      OnNewIncommingConnection({$IFDEF DelphiSockets}Self{$ELSE}Sender{$ENDIF},n);
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
  FTcpIpServer.DefaultPort := Value;
  {$ELSE}
  FPort := Value;
  {$ENDIF}
end;

procedure TNetTcpIpServer.WaitUntilNetTcpIpClientsFinalized;
Var l : TList<TNetTcpIpClient>;
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
    lSockets : TList<TTcpIpSocketThread>;
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
  lSockets := TList<TTcpIpSocketThread>.Create;
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
      TTcpIpSocketThread(lSockets[i]).FListenerThread := Nil;
      TTcpIpSocketThread(lSockets[i]).Terminate;
    end;
    // Wait until terminated...
    for i := 0 to lSockets.Count - 1 do begin
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
      TLog.NewLog(lterror,ClassName,'Exception closing socket ('+E.ClassName+'):' +E.Message);
    end;
  End;
  Try
    FreeAndNil(FSock);
  Except
    On E:Exception do begin
      TLog.NewLog(lterror,ClassName,'Exception destroying socket ('+E.ClassName+'):' +E.Message);
    end;
  End;
  inherited;
end;

{$ENDIF}

end.
