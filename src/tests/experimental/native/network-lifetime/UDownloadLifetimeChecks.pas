unit UDownloadLifetimeChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  The real downloader owns its leased snapshot and executes BCExecute. Only its
  candidate choice and the subsequent download boundary are gated. Actual sockets,
  client destruction, listener retirement and owner shutdown remain in production. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

type TDownloadLifetimeCheck = (dlcOutgoing, dlcIncoming, dlcShutdown, dlcNodeCallbacks,
  dlcPendingDiscovery, dlcActiveDiscovery, dlcLateConnection);

function RunDownloadLifetimeChecks(const ALogFile: String): Integer;
function RunDownloadLifetimeCheck(ACheck: TDownloadLifetimeCheck; const ALogFile: String): Integer;

implementation

uses Classes, SysUtils, SyncObjs, Generics.Collections, blcksock, synsock,
  UThread, UBaseTypes, UNetProtocol, UTCPIP, UNode, UCrypto;

type
  TCheckState = class
  public
    Selected: TNetConnection;
    SelectedTransport: TNetTcpIpClient;
    Entered, ReleaseBody, BodyFinished, OwnerGone: TEvent;
    SelectedGone, OtherGone, TransportGone: TEvent;
    EmptyScan, ReleaseScan: TEvent;
    Destroyer: TNetClientsDestroyThread;
    FailFactory, HoldEmptyScan: Boolean;
    Factories, DiscoveryClients: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TGateWorker = class(TThreadGetNewBlockChainFromClient)
  private
    FState: TCheckState;
  protected
    function SelectConnection(const AConnections: TList<TNetConnection>): TNetConnection; override;
  public
    constructor Create(AOwner: TNetData; AState: TCheckState);
  end;

  TGateNetData = class(TNetData)
  private
    FState: TCheckState;
  protected
    function CreateClientsDestroyThread: TNetClientsDestroyThread; override;
    function CreateBlockChainWorker: TThreadGetNewBlockChainFromClient; override;
  public
    constructor Create(AState: TCheckState); reintroduce;
    destructor Destroy; override;
    procedure StartDownload;
    procedure GetNewBlockChainFromClient(Connection: TNetConnection; const why: String); override;
  end;

  TObservedDestroyThread = class(TNetClientsDestroyThread)
  private
    FState: TCheckState;
    FObserved: Boolean;
  protected
    procedure RegistryScanCompleted(AEmpty: Boolean); override;
  public
    constructor Create(AOwner: TNetData; AState: TCheckState);
  end;

  TDrainWaitAction = class(TPCThread)
  private
    FState: TCheckState;
    FDone: TEvent;
  protected
    procedure BCExecute; override;
  public
    constructor Create(AState: TCheckState);
    destructor Destroy; override;
    property Done: TEvent read FDone;
  end;

  TGateDiscoveryWorker = class(TThreadDiscoverConnection)
  private
    FState: TCheckState;
  protected
    procedure BCExecute; override;
  public
    constructor Create(AOwner: TNetData; const AAddress: TNodeServerAddress;
      AState: TCheckState);
  end;

  TDiscoveryNetData = class(TGateNetData)
  private
    FHoldStart: Boolean;
  protected
    function CreateDiscoveryWorker(const AAddress: TNodeServerAddress): TThreadDiscoverConnection; override;
    function CreateDiscoveryClient: TNetClient; override;
    procedure StartDiscoveryWorker(AWorker: TThreadDiscoverConnection); override;
  public
    procedure StartDiscovery(AHoldStart: Boolean);
  end;

  TDeathObserver = class(TComponent)
  private
    FState: TCheckState;
    FOther: TComponent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AState: TCheckState; AOther: TComponent); reintroduce;
  end;

  TPeer = class(TPCThread)
  private
    FListener, FSocket: TTCPBlockSocket;
    FPort: Word;
  protected
    procedure BCExecute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Port: Word read FPort;
  end;

  TStopAction = class(TPCThread)
  private
    FOwner: TGateNetData;
    FDestroyOwner: Boolean;
    FDone: TEvent;
    FError: String;
  protected
    procedure BCExecute; override;
  public
    constructor Create(AOwner: TGateNetData; ADestroyOwner: Boolean);
    destructor Destroy; override;
    property Done: TEvent read FDone;
    property Error: String read FError;
  end;

  TFinalizedReader = class
  public
    Done: TEvent;
    Error: String;
    constructor Create;
    destructor Destroy; override;
    procedure Finalized(Sender: TNetTcpIpServer; AClient: TNetTcpIpClient);
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function Signaled(AEvent: TEvent): Boolean;
begin
  Result := AEvent.WaitFor(0)=wrSignaled;
end;

procedure Await(AEvent: TEvent; AMilliseconds: Cardinal; const AMessage: String);
var LStart: TTickCount;
begin
  LStart := TPlatform.GetTickCount;
  while not Signaled(AEvent) and
    (TPlatform.GetElapsedMilliseconds(LStart)<AMilliseconds) do begin
    CheckSynchronize;
    Sleep(5);
  end;
  Require(Signaled(AEvent),AMessage);
end;

procedure Pump(AMilliseconds: Cardinal);
var LStart: TTickCount;
begin
  LStart := TPlatform.GetTickCount;
  repeat CheckSynchronize; Sleep(5);
  until TPlatform.GetElapsedMilliseconds(LStart)>=AMilliseconds;
end;

constructor TCheckState.Create;
begin
  inherited;
  Entered:=TEvent.Create(nil,True,False,'');
  ReleaseBody:=TEvent.Create(nil,True,False,'');
  BodyFinished:=TEvent.Create(nil,True,False,'');
  OwnerGone:=TEvent.Create(nil,True,False,'');
  SelectedGone:=TEvent.Create(nil,True,False,'');
  OtherGone:=TEvent.Create(nil,True,False,'');
  TransportGone:=TEvent.Create(nil,True,False,'');
  EmptyScan:=TEvent.Create(nil,True,False,'');
  ReleaseScan:=TEvent.Create(nil,True,False,'');
end;

destructor TCheckState.Destroy;
begin
  ReleaseScan.Free; EmptyScan.Free;
  TransportGone.Free; OtherGone.Free; SelectedGone.Free; OwnerGone.Free;
  BodyFinished.Free; ReleaseBody.Free; Entered.Free;
  inherited;
end;

constructor TGateWorker.Create(AOwner: TNetData; AState: TCheckState);
begin
  FState:=AState;
  inherited Create(AOwner);
end;

function TGateWorker.SelectConnection(const AConnections: TList<TNetConnection>): TNetConnection;
begin
  Require(AConnections.IndexOf(FState.Selected)>=0,'Selected live socket was not leased');
  Result:=FState.Selected;
end;

constructor TGateNetData.Create(AState: TCheckState);
begin
  FState:=AState;
  inherited Create(nil);
end;

destructor TGateNetData.Destroy;
var LState: TCheckState;
begin
  LState:=FState;
  inherited;
  // State belongs to the fixture and outlives both owner and worker.
  LState.OwnerGone.SetEvent;
end;

function TGateNetData.CreateBlockChainWorker: TThreadGetNewBlockChainFromClient;
begin
  Inc(FState.Factories);
  if FState.FailFactory then begin
    FState.FailFactory:=False;
    raise EAbort.Create('Expected downloader factory failure');
  end;
  Result:=TGateWorker.Create(Self,FState);
end;

function TGateNetData.CreateClientsDestroyThread: TNetClientsDestroyThread;
begin
  if FState.HoldEmptyScan then begin
    Result:=TObservedDestroyThread.Create(Self,FState);
    FState.Destroyer:=Result;
  end else Result:=inherited CreateClientsDestroyThread;
end;

constructor TObservedDestroyThread.Create(AOwner: TNetData; AState: TCheckState);
begin
  FState:=AState;
  inherited Create(AOwner);
end;

procedure TObservedDestroyThread.RegistryScanCompleted(AEmpty: Boolean);
begin
  if AEmpty and not FObserved then begin
    FObserved:=True;
    FState.EmptyScan.SetEvent;
    FState.ReleaseScan.WaitFor(High(Cardinal));
  end;
end;

constructor TDrainWaitAction.Create(AState: TCheckState);
begin
  FState:=AState;
  FDone:=TEvent.Create(nil,True,False,'');
  inherited Create(False);
  FreeOnTerminate:=False;
end;

procedure TDrainWaitAction.BCExecute;
begin
  try FState.Destroyer.WaitForTerminatedAllConnections;
  finally FDone.SetEvent; end;
end;

destructor TDrainWaitAction.Destroy;
begin
  WaitFor;
  FDone.Free;
  inherited;
end;

procedure TGateNetData.StartDownload;
begin
  StartBlockChainWorker;
end;

constructor TGateDiscoveryWorker.Create(AOwner: TNetData;
  const AAddress: TNodeServerAddress; AState: TCheckState);
begin
  FState:=AState;
  inherited Create(AOwner,AAddress);
end;

procedure TGateDiscoveryWorker.BCExecute;
begin
  // Hold a real owned worker before its production shutdown/admission check.
  // No client factory or network algorithm is replaced by this gate.
  FState.Entered.SetEvent;
  FState.ReleaseBody.WaitFor(High(Cardinal));
  try
    inherited;
  finally
    FState.BodyFinished.SetEvent;
  end;
end;

function TDiscoveryNetData.CreateDiscoveryWorker(
  const AAddress: TNodeServerAddress): TThreadDiscoverConnection;
begin
  Inc(FState.Factories);
  Result:=TGateDiscoveryWorker.Create(Self,AAddress,FState);
end;

function TDiscoveryNetData.CreateDiscoveryClient: TNetClient;
begin
  Inc(FState.DiscoveryClients);
  Result:=inherited CreateDiscoveryClient;
end;

procedure TDiscoveryNetData.StartDiscoveryWorker(AWorker: TThreadDiscoverConnection);
begin
  if FHoldStart then FState.Entered.SetEvent
  else inherited StartDiscoveryWorker(AWorker);
end;

procedure TDiscoveryNetData.StartDiscovery(AHoldStart: Boolean);
var LAddresses: TNodeServerAddressArray;
begin
  FHoldStart:=AHoldStart;
  SetLength(LAddresses,1);
  LAddresses[0]:=CT_TNodeServerAddress_NUL;
  LAddresses[0].ip:='127.0.0.1';
  LAddresses[0].port:=1;
  StartDiscoveryWorkers(LAddresses);
end;

procedure TGateNetData.GetNewBlockChainFromClient(Connection: TNetConnection; const why: String);
begin
  Require(Connection=FState.Selected,'Worker changed its selected connection');
  FState.SelectedTransport:=Connection.Client;
  FState.Entered.SetEvent;
  FState.ReleaseBody.WaitFor(High(Cardinal));
  // Do not dereference a prematurely destroyed pointer on a failing regression.
  // The main-thread assertions report that failure; cleanup must still join.
  if not Signaled(FState.SelectedGone) then
    Require(Connection.Client=FState.SelectedTransport,'Leased transport was replaced');
  FState.BodyFinished.SetEvent;
end;

constructor TDeathObserver.Create(AState: TCheckState; AOther: TComponent);
begin
  inherited Create(nil);
  FState:=AState;
  FOther:=AOther;
  if Assigned(AOther) then AOther.FreeNotification(Self);
  AState.Selected.FreeNotification(Self);
  AState.Selected.Client.FreeNotification(Self);
end;

procedure TDeathObserver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation<>opRemove then Exit;
  if AComponent=FState.Selected then FState.SelectedGone.SetEvent;
  if AComponent=FOther then FState.OtherGone.SetEvent;
  if AComponent=FState.SelectedTransport then FState.TransportGone.SetEvent;
end;

constructor TPeer.Create;
begin
  FListener:=TTCPBlockSocket.Create;
  FListener.CreateSocket;
  FListener.Bind('127.0.0.1','0');
  Require(FListener.LastError=0,'Cannot bind owned loopback peer');
  FListener.Listen;
  Require(FListener.LastError=0,'Cannot listen on owned loopback peer');
  FPort:=FListener.GetLocalSinPort;
  // All state is ready before the automatic start. Delphi starts this thread
  // in AfterConstruction; calling Start inside the constructor starts it twice.
  inherited Create(False);
  FreeOnTerminate:=False;
end;

procedure TPeer.BCExecute;
begin
  while not Terminated and not FListener.CanRead(20) do ;
  if Terminated then Exit;
  FSocket:=TTCPBlockSocket.Create;
  try
    FSocket.Socket:=FListener.Accept;
    while not Terminated do begin
      if FSocket.CanRead(20) then FSocket.RecvPacket(20);
      Sleep(1);
    end;
  finally FreeAndNil(FSocket); end;
end;

destructor TPeer.Destroy;
begin
  Terminate;
  WaitFor;
  FListener.Free;
  inherited;
end;

constructor TStopAction.Create(AOwner: TGateNetData; ADestroyOwner: Boolean);
begin
  FOwner:=AOwner;
  FDestroyOwner:=ADestroyOwner;
  FDone:=TEvent.Create(nil,True,False,'');
  inherited Create(False);
  FreeOnTerminate:=False;
end;

procedure TStopAction.BCExecute;
begin
  try
    try
      if FDestroyOwner then FOwner.Free
      else FOwner.NetConnectionsActive:=False;
    except on E: Exception do FError:=E.ClassName+': '+E.Message; end;
  finally FDone.SetEvent; end;
end;

destructor TStopAction.Destroy;
begin
  WaitFor;
  FDone.Free;
  inherited;
end;

constructor TFinalizedReader.Create;
begin
  inherited;
  Done:=TEvent.Create(nil,True,False,'');
end;

destructor TFinalizedReader.Destroy;
begin
  Done.Free;
  inherited;
end;

procedure TFinalizedReader.Finalized(Sender: TNetTcpIpServer; AClient: TNetTcpIpClient);
var LLocked: Boolean; LPending: TObject;
begin
  LLocked:=False;
  try
    try
      // A real accepted-client finalizer is still an owned node callback. Its
      // node resources must remain alive until the listener has joined it.
      LPending:=TNode.Node.LockMempoolRead;
      LLocked:=True;
      Require(Assigned(LPending),'Finalizer lost the pending template');
    finally
      if LLocked then TNode.Node.UnlockMempoolRead;
    end;
  except on E: Exception do Error:=E.ClassName+': '+E.Message; end;
  Done.SetEvent;
end;

procedure Prepare(AOwner: TGateNetData);
begin
  AOwner.NetConnectionsActive:=False;
  AOwner.MinServersConnected:=0;
  AOwner.MaxServersConnected:=0;
  TNode.Node.UpdateBlockchain:=True;
  TNode.Node.NetServer.Port:=0;
  AOwner.NetConnectionsActive:=True;
end;

procedure OutgoingAndFactoryCheck;
var S: TCheckState; N: TGateNetData; P1,P2: TPeer;
  C1,C2: TNetClient; O: TDeathObserver; LFailed: Boolean;
begin
  S:=TCheckState.Create; N:=nil; P1:=nil; P2:=nil; O:=nil;
  try
    N:=TGateNetData.Create(S); Prepare(N);
    P1:=TPeer.Create; P2:=TPeer.Create;
    C1:=TNetClient.Create(nil); C2:=TNetClient.Create(nil);
    Require(C1.ConnectTo('127.0.0.1',P1.Port),'First outgoing peer failed');
    Require(C2.ConnectTo('127.0.0.1',P2.Port),'Second outgoing peer failed');
    S.Selected:=C1; S.SelectedTransport:=C1.Client;
    O:=TDeathObserver.Create(S,C2);
    S.FailFactory:=True; LFailed:=False;
    try N.StartDownload; except on E: EAbort do LFailed:=True; end;
    Require(LFailed,'Factory failure was hidden');
    N.StartDownload;
    Await(S.Entered,3000,'Downloader did not start after factory failure');
    Require(S.Factories=2,'Unexpected downloader admission count');
    N.NetConnectionsActive:=False;
    Await(S.OtherGone,2500,'Unselected connection was retained by another download');
    Pump(700);
    Require(not Signaled(S.SelectedGone),'Selected connection was freed while leased');
    Require(not Signaled(S.TransportGone),'Selected transport was freed while leased');
    S.ReleaseBody.SetEvent;
    Await(S.BodyFinished,3000,'Selected download did not complete');
    Await(S.SelectedGone,3000,'Selected connection did not retire after release');
  finally
    S.ReleaseBody.SetEvent;
    N.Free;
    O.Free; P2.Free; P1.Free;
    if TNode.NodeExists then TNode.Node.Free;
    S.Free;
  end;
end;

procedure IncomingRetirementCheck;
var S: TCheckState; N: TGateNetData; Socket: TTCPBlockSocket;
  C: TNetConnection; O: TDeathObserver; Stop: TStopAction;
  LStart: TTickCount;
begin
  S:=TCheckState.Create; N:=nil; Socket:=nil; O:=nil; Stop:=nil;
  try
    N:=TGateNetData.Create(S); Prepare(N);
    // Pick a real ephemeral port without reading private listener fields.
    Socket:=TTCPBlockSocket.Create; Socket.CreateSocket;
    Socket.Bind('127.0.0.1','0');
    Require(Socket.LastError=0,'Cannot reserve incoming test port');
    N.NetConnectionsActive:=False;
    TNode.Node.NetServer.Port:=Socket.GetLocalSinPort;
    Socket.CloseSocket;
    N.NetConnectionsActive:=True;
    LStart:=TPlatform.GetTickCount;
    repeat
      Socket.CloseSocket;
      Socket.Connect('127.0.0.1',IntToStr(TNode.Node.NetServer.Port));
      if Socket.LastError=0 then Break;
      Pump(10);
    until TPlatform.GetElapsedMilliseconds(LStart)>2000;
    Require(Socket.LastError=0,'Incoming loopback socket failed');
    LStart:=TPlatform.GetTickCount; C:=nil;
    repeat
      if N.GetConnection(0,C) and (C is TNetServerClient) then Break;
      Pump(10);
    until TPlatform.GetElapsedMilliseconds(LStart)>3000;
    Require(Assigned(C) and (C is TNetServerClient),'Incoming wrapper was not accepted');
    S.Selected:=C; S.SelectedTransport:=C.Client;
    O:=TDeathObserver.Create(S,nil);
    N.StartDownload; Await(S.Entered,3000,'Incoming candidate was not leased');
    Stop:=TStopAction.Create(N,False);
    Pump(1200);
    Require(not Signaled(Stop.Done),'Listener retired before its selected lease ended');
    Require(not Signaled(S.SelectedGone),'Incoming wrapper was freed while leased');
    Require(not Signaled(S.TransportGone),'Incoming transport was replaced while leased');
    S.ReleaseBody.SetEvent;
    Await(Stop.Done,4000,'Listener did not finish after incoming lease release');
    Require(Stop.Error='',Stop.Error);
    Await(S.SelectedGone,2000,'Incoming wrapper did not retire');
  finally
    S.ReleaseBody.SetEvent;
    Stop.Free; N.Free;
    O.Free; Socket.Free;
    if TNode.NodeExists then TNode.Node.Free;
    S.Free;
  end;
end;

procedure ShutdownJoinCheck;
var S: TCheckState; N: TGateNetData; P: TPeer; C: TNetClient;
  O: TDeathObserver; Stop: TStopAction; LFactories: Integer;
begin
  S:=TCheckState.Create; N:=nil; P:=nil; O:=nil; Stop:=nil;
  try
    N:=TGateNetData.Create(S); Prepare(N);
    P:=TPeer.Create; C:=TNetClient.Create(nil);
    Require(C.ConnectTo('127.0.0.1',P.Port),'Shutdown loopback peer failed');
    S.Selected:=C; S.SelectedTransport:=C.Client;
    O:=TDeathObserver.Create(S,nil);
    N.StartDownload; Await(S.Entered,3000,'Shutdown worker did not acquire lease');
    N.BeginShutdown;
    LFactories:=S.Factories; N.StartDownload;
    Require(S.Factories=LFactories,'Shutdown admitted a new downloader');
    Stop:=TStopAction.Create(N,True); N:=nil;
    Pump(900);
    Require(not Signaled(S.OwnerGone),'Owner freed shared state before worker completion');
    Require(not Signaled(S.SelectedGone),'Shutdown freed a selected connection before release');
    S.ReleaseBody.SetEvent;
    Await(Stop.Done,4000,'Shutdown did not join downloader after release');
    Require(Stop.Error='',Stop.Error);
    Require(Signaled(S.BodyFinished) and Signaled(S.OwnerGone),
      'Owner completion did not follow downloader completion');
    Require(Signaled(S.SelectedGone),'Shutdown left its selected connection alive');
  finally
    S.ReleaseBody.SetEvent;
    Stop.Free; N.Free;
    O.Free; P.Free;
    if TNode.NodeExists then TNode.Node.Free;
    S.Free;
  end;
end;

procedure OwnedNodeCallbackCheck;
var S: TCheckState; N: TGateNetData; Socket: TTCPBlockSocket;
  Reader: TFinalizedReader; C: TNetConnection; LStart: TTickCount;
begin
  S:=TCheckState.Create; N:=nil; Socket:=nil; Reader:=nil;
  try
    N:=TGateNetData.Create(S); Prepare(N);
    Socket:=TTCPBlockSocket.Create; Socket.CreateSocket;
    Socket.Bind('127.0.0.1','0');
    Require(Socket.LastError=0,'Cannot reserve callback test port');
    N.NetConnectionsActive:=False;
    TNode.Node.NetServer.Port:=Socket.GetLocalSinPort;
    Socket.CloseSocket;
    N.NetConnectionsActive:=True;
    Reader:=TFinalizedReader.Create;
    TNode.Node.NetServer.OnClientFinalized:=Reader.Finalized;
    LStart:=TPlatform.GetTickCount;
    repeat
      Socket.CloseSocket;
      Socket.Connect('127.0.0.1',IntToStr(TNode.Node.NetServer.Port));
      if Socket.LastError=0 then Break;
      Pump(10);
    until TPlatform.GetElapsedMilliseconds(LStart)>2000;
    Require(Socket.LastError=0,'Callback test connection failed');
    LStart:=TPlatform.GetTickCount; C:=nil;
    repeat
      if N.GetConnection(0,C) and (C is TNetServerClient) then Break;
      Pump(10);
    until TPlatform.GetElapsedMilliseconds(LStart)>3000;
    Require(Assigned(C) and (C is TNetServerClient),'Callback test was not accepted');
    // Stop shared producers first, then test the node's own resource order.
    // This does not make the node an owner of the external NetData singleton.
    N.BeginShutdown;
    TNode.Node.Free;
    Await(Reader.Done,1000,'Accepted-client finalizer did not run');
    Require(Reader.Error='','Node resources retired before its callback: '+Reader.Error);
  finally
    N.Free;
    if TNode.NodeExists then TNode.Node.Free;
    Reader.Free; Socket.Free; S.Free;
  end;
end;

procedure DiscoveryShutdownCheck(AHoldStart: Boolean);
var S: TCheckState; N: TDiscoveryNetData; Stop: TStopAction;
  LFactories: Integer;
begin
  S:=TCheckState.Create; N:=nil; Stop:=nil;
  try
    N:=TDiscoveryNetData.Create(S); Prepare(N);
    N.StartDiscovery(AHoldStart);
    Await(S.Entered,3000,'Discovery was not registered before its start boundary');
    Require(S.Factories=1,'Unexpected discovery admission count');
    Require(S.DiscoveryClients=0,'Discovery gate was bypassed');
    N.BeginShutdown;
    LFactories:=S.Factories;
    N.StartDiscovery(False);
    Require(S.Factories=LFactories,'Shutdown admitted a new discovery worker');
    // A pending thread may be started solely to let RTL destruction join it.
    // Its inherited body must see shutdown before creating a client.
    if AHoldStart then S.ReleaseBody.SetEvent;
    Stop:=TStopAction.Create(N,True); N:=nil;
    if not AHoldStart then begin
      Pump(250);
      Require(not Signaled(S.OwnerGone),'Owner freed resources while discovery was running');
      S.ReleaseBody.SetEvent;
    end;
    Await(Stop.Done,4000,'Shutdown did not join its discovery worker');
    Require(Stop.Error='',Stop.Error);
    Require(Signaled(S.OwnerGone),'Discovery owner did not finish');
    Require(S.DiscoveryClients=0,'Retired discovery created a late client');
    // Pending workers may be skipped completely; a started worker must finish.
    if not AHoldStart then
      Require(Signaled(S.BodyFinished),'Started discovery outlived its owner');
  finally
    S.ReleaseBody.SetEvent;
    Stop.Free; N.Free;
    if TNode.NodeExists then TNode.Node.Free;
    S.Free;
  end;
end;

procedure LateConnectionDrainCheck;
var S: TCheckState; N: TGateNetData; C: TNetClient;
  O: TDeathObserver; Drain: TDrainWaitAction;
begin
  S:=TCheckState.Create; N:=nil; O:=nil; Drain:=nil;
  try
    S.HoldEmptyScan:=True;
    N:=TGateNetData.Create(S); Prepare(N);
    Await(S.EmptyScan,3000,'Owned destroyer never examined the empty registry');
    // Register after the actual empty scan while that one destroyer is paused.
    // A cached empty flag would return before this new client is retired.
    C:=TNetClient.Create(nil);
    S.Selected:=C; S.SelectedTransport:=C.Client;
    O:=TDeathObserver.Create(S,nil);
    N.NetConnectionsActive:=False;
    Drain:=TDrainWaitAction.Create(S);
    Pump(250);
    Require(not Signaled(Drain.Done),'Drain trusted an old empty scan over a live client');
    Require(not Signaled(S.SelectedGone),'Paused destroyer unexpectedly retired the client');
    S.ReleaseScan.SetEvent;
    Await(Drain.Done,4000,'Drain did not finish after late client retirement');
    Await(S.SelectedGone,1000,'Late client survived the registry drain');
  finally
    S.ReleaseScan.SetEvent;
    Drain.Free; N.Free; O.Free;
    if TNode.NodeExists then TNode.Node.Free;
    S.Free;
  end;
end;

function RunDownloadLifetimeCheck(ACheck: TDownloadLifetimeCheck; const ALogFile: String): Integer;
var LLog: TStringList; LRoot: String;
begin
  Result:=1;
  Require(not FileExists(ALogFile),'Refusing an existing lifetime log');
  LLog:=TStringList.Create;
  try
    // Other core acceptance fixtures may leave the process-wide services alive.
    // Retire that generated fixture context in the normal ownership order before
    // installing our virtual-factory owner; never change singleton registration
    // to make a second independent owner capture the first owner's connections.
    if TNetData.NetDataExists then TNetData.NetData.BeginShutdown;
    if TNode.NodeExists then TNode.Node.NetServer.Active:=False;
    if TNetData.NetDataExists then TNetData.NetData.Free;
    if TNode.NodeExists then TNode.Node.Free;
    TCrypto.InitCrypto;
    LRoot:=ChangeFileExt(ALogFile,'')+'-data';
    Require(not DirectoryExists(LRoot),'Refusing an existing lifetime data directory');
    ForceDirectories(LRoot);
    TNode.SetPascalCoinDataFolder(LRoot);
    case ACheck of
      dlcOutgoing: OutgoingAndFactoryCheck;
      dlcIncoming: IncomingRetirementCheck;
      dlcShutdown: ShutdownJoinCheck;
      dlcNodeCallbacks: OwnedNodeCallbackCheck;
      dlcPendingDiscovery: DiscoveryShutdownCheck(True);
      dlcActiveDiscovery: DiscoveryShutdownCheck(False);
      dlcLateConnection: LateConnectionDrainCheck;
    end;
    LLog.Add(Format('PASS %d',[Ord(ACheck)+1])); LLog.SaveToFile(ALogFile);
    Result:=0;
  except on E: Exception do begin
    LLog.Add('FAIL '+E.ClassName+': '+E.Message); LLog.SaveToFile(ALogFile);
  end; end;
  LLog.Free;
end;

function RunDownloadLifetimeChecks(const ALogFile: String): Integer;
var LCheck: TDownloadLifetimeCheck; LLog: TStringList;
begin
  Result:=1;
  Require(not FileExists(ALogFile),'Refusing an existing lifetime summary');
  LLog:=TStringList.Create;
  try
    for LCheck:=Low(TDownloadLifetimeCheck) to High(TDownloadLifetimeCheck) do begin
      if RunDownloadLifetimeCheck(LCheck,ALogFile+'.'+IntToStr(Ord(LCheck)+1)+'.log')<>0 then begin
        LLog.Add(Format('FAIL %d; see scenario log',[Ord(LCheck)+1]));
        LLog.SaveToFile(ALogFile);
        Exit;
      end;
      LLog.Add(Format('PASS %d',[Ord(LCheck)+1]));
      LLog.SaveToFile(ALogFile);
    end;
    Result:=0;
  finally LLog.Free; end;
end;

end.
