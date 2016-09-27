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

{$DEFINE DelphiSockets}

uses
  {$IFDEF UNIX}
 cthreads,
 {$ENDIF}
  Classes, Sysutils,
  UThread,Sockets;

type
  TNetTcpIpClient = Class(TComponent)
  private
    {$IFDEF DelphiSockets}
    FTcpIpClient : TCustomIpClient;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FSocketError: Integer;
    {$ENDIF}
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
    function ReceiveBuf(var Buf; BufSize: Integer): Integer;
    Function SendStream(Stream : TStream) : Int64;
    //
    Property OnConnect : TNotifyEvent read FOnConnect write SetOnConnect;
    Property OnDisconnect : TNotifyEvent read FOnDisconnect write SetOnDisconnect;
    Function BytesReceived : Int64;
    Function BytesSent : Int64;
    Property SocketError : Integer read FSocketError write SetSocketError;
  End;

  TNetTcpIpServer = Class(TObject)
  private
    {$IFDEF DelphiSockets}
    FTcpIpServer : TTcpServer;
    FTcpIpClient : TCustomIpClient;
    {$ENDIF}
    FNetClients : TPCThreadList;
    function GetActive: Boolean;
    procedure SetPort(const Value: Word);  // When a connection is established to a new client, a TNetConnection is created (p2p)
    {$IFDEF DelphiSockets}
    procedure OnTcpServerAccept(Sender: TObject; ClientSocket: TCustomIpClient);
    function GetPort: Word;
    {$ENDIF}
  protected
    Procedure OnNewIncommingConnection(Sender : TObject; Client : TNetTcpIpClient); virtual;
    procedure SetActive(const Value: Boolean); virtual;
  public
    Constructor Create;
    Destructor Destroy; override;
    Property Active : Boolean read GetActive write SetActive;
    Property Port : Word read GetPort Write SetPort;
  End;


implementation

uses UConst, ULog;

{ TNetTcpIpClient }

function TNetTcpIpClient.BytesReceived: Int64;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpClient.BytesReceived;
  {$ENDIF}
end;

function TNetTcpIpClient.BytesSent: Int64;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpClient.BytesSent;
  {$ENDIF}
end;

function TNetTcpIpClient.ClientRemoteAddr: AnsiString;
begin
  If Assigned(FTcpIpClient) then begin
    {$IFDEF DelphiSockets}
    Result := FTcpIpClient.RemoteHost+':'+FTcpIpClient.RemotePort;
    {$ENDIF}
  end else Result := 'NIL';
end;

function TNetTcpIpClient.Connect: Boolean;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpClient.Connect;
  {$ENDIF}
end;

constructor TNetTcpIpClient.Create(AOwner : TComponent);
begin
  inherited;
  FTcpIpClient := Nil;
  FSocketError := 0;
  {$IFDEF DelphiSockets}
  FTcpIpClient := TTcpClient.Create(Nil);
  FTcpIpClient.OnConnect := OnConnect;
  FTcpIpClient.OnDisconnect := OnDisconnect;
  FTcpIpClient.OnError := TCustomIpClient_OnError;
  {$ENDIF}
end;

destructor TNetTcpIpClient.Destroy;
begin
  Disconnect;
  inherited;
  FreeAndNil(FTcpIpClient);
end;

procedure TNetTcpIpClient.Disconnect;
begin
  {$IFDEF DelphiSockets}
  FTcpIpClient.Disconnect;
  FTcpIpClient.OnConnect := Nil;
  FTcpIpClient.OnDisconnect := Nil;
  FTcpIpClient.OnError := Nil;
  {$ENDIF}
end;

function TNetTcpIpClient.GetConnected: Boolean;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpClient.Connected;
  {$ENDIF}
end;

function TNetTcpIpClient.GetRemoteHost: AnsiString;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpClient.RemoteHost;
  {$ENDIF}
end;

function TNetTcpIpClient.GetRemotePort: Word;
begin
  {$IFDEF DelphiSockets}
  Result := StrToIntDef(FTcpIpClient.RemotePort,0);
  {$ENDIF}
end;

function TNetTcpIpClient.ReceiveBuf(var Buf; BufSize: Integer): Integer;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpClient.ReceiveBuf(Buf,BufSize);
  {$ENDIF}
end;

function TNetTcpIpClient.SendStream(Stream: TStream): Int64;
Var sp : Int64;
begin
  {$IFDEF DelphiSockets}
  sp := Stream.Position;
  FTcpIpClient.SendStream(Stream);
  Result := Stream.Position - sp;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetOnConnect(const Value: TNotifyEvent);
begin
  FOnConnect := Value;
  {$IFDEF DelphiSockets}
  FTcpIpClient.OnConnect := OnConnect;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetOnDisconnect(const Value: TNotifyEvent);
begin
  FOnDisconnect := Value;
  {$IFDEF DelphiSockets}
  FTcpIpClient.OnDisconnect := OnDisconnect;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetRemoteHost(const Value: AnsiString);
begin
  {$IFDEF DelphiSockets}
  FTcpIpClient.RemoteHost := Value;
  {$ENDIF}
end;

procedure TNetTcpIpClient.SetRemotePort(const Value: Word);
begin
  {$IFDEF DelphiSockets}
  FTcpIpClient.RemotePort := IntToStr(Value);
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
end;
{$ENDIF}

function TNetTcpIpClient.WaitForData(WaitMilliseconds: Integer): Boolean;
begin
  {$IFDEF DelphiSockets}
  Result := FTcpIpClient.WaitForData(WaitMilliseconds);
  {$ENDIF}
end;

{ TNetTcpIpServer }

constructor TNetTcpIpServer.Create;
begin
  {$IFDEF DelphiSockets}
  FTcpIpServer := TTcpServer.Create(Nil);
  FTcpIpServer.OnAccept := OnTcpServerAccept;
  FTcpIpServer.ServerSocketThread.ThreadCacheSize := CT_MaxClientsConnected;
  {$ENDIF}
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
  {$ENDIF}
end;

function TNetTcpIpServer.GetPort: Word;
begin
  {$IFDEF DelphiSockets}
  Result := StrToIntDef(FTcpIpServer.LocalPort,0);
  {$ENDIF}
end;

procedure TNetTcpIpServer.OnNewIncommingConnection(Sender: TObject; Client: TNetTcpIpClient);
begin
  //
end;

{$IFDEF DelphiSockets}
procedure TNetTcpIpServer.OnTcpServerAccept(Sender: TObject; ClientSocket: TCustomIpClient);
Var n : TNetTcpIpClient;
  oldSocket : TCustomIpClient;
begin
  n := TNetTcpIpClient.Create(Nil);
  Try
    oldSocket := n.FTcpIpClient;
    n.FTcpIpClient := ClientSocket;
    OnNewIncommingConnection(Sender,n);
  Finally
    n.FTcpIpClient := oldSocket;
    FreeAndNil(n);
  End;
end;
{$ENDIF}

procedure TNetTcpIpServer.SetActive(const Value: Boolean);
begin
  {$IFDEF DelphiSockets}
  FTcpIpServer.Active := Value;
  {$ENDIF}
end;

procedure TNetTcpIpServer.SetPort(const Value: Word);
begin
  {$IFDEF DelphiSockets}
  FTcpIpServer.LocalPort := IntToStr(Value);
  {$ENDIF}
end;


end.
