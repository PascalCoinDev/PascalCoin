unit ULegacyConnectionLifetimeChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Retirement is gated before inherited destruction, so a failing lifetime check
  never dereferences a freed connection or destroys an owned critical section. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

type TLegacyConnectionLifetimeCheck = (llcHeldLock, llcRecursiveLock, llcTimedAdmission);

function RunLegacyConnectionLifetimeChecks(const ALogFile: String): Integer;
function RunLegacyConnectionLifetimeCheck(ACheck: TLegacyConnectionLifetimeCheck;
  const ALogFile: String): Integer;

implementation

uses Classes, SysUtils, SyncObjs, blcksock, synsock, UThread, UBaseTypes,
  UNetProtocol, UNode, UCrypto;

type
  TLegacyState = class
  public
    Entered, Retired, AllowDestroy, Gone: TEvent;
    constructor Create;
    destructor Destroy; override;
  end;

  TObservedConnection = class(TNetConnection)
  private
    FState: TLegacyState;
  public
    constructor Create(AState: TLegacyState); reintroduce;
    destructor Destroy; override;
  end;

  TDeathObserver = class(TComponent)
  private
    FState: TLegacyState;
    FTarget: TComponent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AState: TLegacyState; ATarget: TComponent); reintroduce;
  end;

  TPeer = class(TPCThread)
  private
    FListener: TTCPBlockSocket;
    FPort: Word;
  protected
    procedure BCExecute; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Port: Word read FPort;
  end;

  TFreeConnection = class(TPCThread)
  private
    FConnection: TNetConnection;
  protected
    procedure BCExecute; override;
  public
    Done: TEvent;
    Error: String;
    constructor Create(AConnection: TNetConnection);
    destructor Destroy; override;
  end;

  TTimedLock = class(TPCThread)
  private
    FOwner: TNetData;
    FConnection: TNetConnection;
    FTimeout: Cardinal;
  protected
    procedure BCExecute; override;
  public
    Done: TEvent;
    Acquired: Boolean;
    Elapsed: Cardinal;
    Error: String;
    constructor Create(AOwner: TNetData; AConnection: TNetConnection; ATimeout: Cardinal);
    destructor Destroy; override;
  end;

procedure Require(AValue: Boolean; const AMessage: String);
begin
  if not AValue then raise Exception.Create(AMessage);
end;

function Signaled(AEvent: TEvent): Boolean;
begin
  Result := AEvent.WaitFor(0) = wrSignaled;
end;

procedure Await(AEvent: TEvent; const AMessage: String);
var LStart: TTickCount;
begin
  LStart := TPlatform.GetTickCount;
  while not Signaled(AEvent) and (TPlatform.GetElapsedMilliseconds(LStart)<3000) do begin
    CheckSynchronize;
    Sleep(1);
  end;
  Require(Signaled(AEvent),AMessage);
end;

constructor TLegacyState.Create;
begin
  inherited;
  Entered := TEvent.Create(nil,true,false,'');
  Retired := TEvent.Create(nil,true,false,'');
  AllowDestroy := TEvent.Create(nil,true,false,'');
  Gone := TEvent.Create(nil,true,false,'');
end;

destructor TLegacyState.Destroy;
begin
  Gone.Free; AllowDestroy.Free; Retired.Free; Entered.Free;
  inherited;
end;

constructor TObservedConnection.Create(AState: TLegacyState);
begin
  FState := AState;
  inherited Create(nil);
end;

destructor TObservedConnection.Destroy;
begin
  FState.Entered.SetEvent;
  // The real production retirement must not finish while a successful legacy
  // ConnectionLock is held. Hold fields alive afterwards even on the old RED.
  RetireDownloadUsage;
  FState.Retired.SetEvent;
  FState.AllowDestroy.WaitFor(5000);
  inherited;
end;

constructor TDeathObserver.Create(AState: TLegacyState; ATarget: TComponent);
begin
  inherited Create(nil);
  FState := AState;
  FTarget := ATarget;
  ATarget.FreeNotification(Self);
end;

procedure TDeathObserver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation=opRemove) and (AComponent=FTarget) then FState.Gone.SetEvent;
end;

constructor TPeer.Create;
begin
  FListener := TTCPBlockSocket.Create;
  FListener.CreateSocket;
  FListener.Bind('127.0.0.1','0');
  Require(FListener.LastError=0,'Loopback bind failed');
  FListener.Listen;
  Require(FListener.LastError=0,'Loopback listen failed');
  FPort := FListener.GetLocalSinPort;
  inherited Create(false);
  FreeOnTerminate := false;
end;

procedure TPeer.BCExecute;
var LSocket: TTCPBlockSocket;
begin
  while not Terminated and not FListener.CanRead(20) do ;
  if Terminated then Exit;
  LSocket := TTCPBlockSocket.Create;
  try
    LSocket.Socket := FListener.Accept;
    while not Terminated do begin
      if LSocket.CanRead(20) then LSocket.RecvPacket(20);
      Sleep(1);
    end;
  finally LSocket.Free; end;
end;

destructor TPeer.Destroy;
begin
  Terminate;
  WaitFor;
  FListener.Free;
  inherited;
end;

constructor TFreeConnection.Create(AConnection: TNetConnection);
begin
  FConnection := AConnection;
  Done := TEvent.Create(nil,true,false,'');
  inherited Create(false);
  FreeOnTerminate := false;
end;

procedure TFreeConnection.BCExecute;
begin
  try
    try FConnection.Free;
    except on E: Exception do Error := E.ClassName+': '+E.Message; end;
  finally Done.SetEvent; end;
end;

destructor TFreeConnection.Destroy;
begin
  WaitFor;
  Done.Free;
  inherited;
end;

constructor TTimedLock.Create(AOwner: TNetData; AConnection: TNetConnection; ATimeout: Cardinal);
begin
  FOwner := AOwner;
  FConnection := AConnection;
  FTimeout := ATimeout;
  Done := TEvent.Create(nil,true,false,'');
  inherited Create(false);
  FreeOnTerminate := false;
end;

procedure TTimedLock.BCExecute;
var LStart: TTickCount;
begin
  LStart := TPlatform.GetTickCount;
  try
    try
      Acquired := FOwner.ConnectionLock(Self,FConnection,FTimeout);
      if Acquired then FOwner.ConnectionUnlock(FConnection);
    except on E: Exception do Error := E.ClassName+': '+E.Message; end;
  finally
    Elapsed := TPlatform.GetElapsedMilliseconds(LStart);
    Done.SetEvent;
  end;
end;

destructor TTimedLock.Destroy;
begin
  WaitFor;
  Done.Free;
  inherited;
end;

procedure CheckLifetime(AOwner: TNetData; ADepth: Integer; ATestTimeout: Boolean);
var S: TLegacyState; P: TPeer; C: TObservedConnection;
  O: TDeathObserver; F: TFreeConnection; T: TTimedLock;
  LHeld, I: Integer;
  LPremature: Boolean;
begin
  S := TLegacyState.Create; P := nil; C := nil; O := nil; F := nil; T := nil;
  LHeld := 0;
  try
    P := TPeer.Create;
    C := TObservedConnection.Create(S);
    O := TDeathObserver.Create(S,C);
    Require(C.ConnectTo('127.0.0.1',P.Port),'Actual loopback connection failed');
    for I := 1 to ADepth do begin
      Require(AOwner.ConnectionLock(nil,C,100),'Legacy lock was not acquired');
      Inc(LHeld);
    end;
    if ATestTimeout then begin
      T := TTimedLock.Create(AOwner,C,40);
      Await(T.Done,'Timed admission did not return');
      Require(T.Error='','Timed admission raised: '+T.Error);
      Require(not T.Acquired,'Writer acquired another thread''s held connection lock');
      Require((T.Elapsed>=40) and (T.Elapsed<500),'Timed admission exceeded its finite bound');
      FreeAndNil(T);
      T := TTimedLock.Create(AOwner,C,0);
      Await(T.Done,'Nonblocking admission did not return');
      Require((T.Error='') and not T.Acquired and (T.Elapsed<100),'Nonblocking admission was not finite');
      FreeAndNil(T);
    end;
    F := TFreeConnection.Create(C);
    Await(S.Entered,'Actual retirement did not start');
    LPremature := S.Retired.WaitFor(200)=wrSignaled;
    while LHeld>0 do begin
      AOwner.ConnectionUnlock(C);
      Dec(LHeld);
      if (LHeld>0) and (S.Retired.WaitFor(100)=wrSignaled) then LPremature := true;
    end;
    Await(S.Retired,'Balanced releases did not retire the connection');
    S.AllowDestroy.SetEvent;
    Await(F.Done,'Connection destruction did not complete');
    C := nil;
    Require(F.Error='','Actual destruction raised: '+F.Error);
    Require(Signaled(S.Gone),'External lifetime observer was not notified');
    Require(not LPremature,'Production retirement passed a successfully held legacy connection lock');
  finally
    // Until AllowDestroy is set, the test gate keeps the real record/lock alive
    // even on RED. Never unlock or inspect a pointer after its Gone notification.
    while LHeld>0 do begin
      AOwner.ConnectionUnlock(C);
      Dec(LHeld);
    end;
    S.AllowDestroy.SetEvent;
    if Assigned(F) then begin
      F.Free;
      C := nil;
    end;
    T.Free;
    C.Free;
    O.Free;
    P.Free;
    S.Free;
  end;
end;

function RunLegacyConnectionLifetimeCheck(ACheck: TLegacyConnectionLifetimeCheck;
  const ALogFile: String): Integer;
var LLog: TStringList; LRoot: String; N: TNetData; LFixed: TNodeServerAddressArray;
begin
  Result := 1;
  Require(not FileExists(ALogFile),'Refusing an existing legacy lifetime log');
  LLog := TStringList.Create; N := nil;
  try
    try
      if TNetData.NetDataExists then TNetData.NetData.BeginShutdown;
      if TNode.NodeExists then TNode.Node.NetServer.Active := false;
      if TNetData.NetDataExists then TNetData.NetData.Free;
      if TNode.NodeExists then TNode.Node.Free;
      TCrypto.InitCrypto;
      LRoot := ALogFile+'-data';
      Require(not DirectoryExists(LRoot),'Refusing existing generated data');
      ForceDirectories(LRoot);
      TNode.SetPascalCoinDataFolder(LRoot);
      N := TNetData.NetData;
      N.NetConnectionsActive := false;
      SetLength(LFixed,1); LFixed[0] := CT_TNodeServerAddress_NUL;
      LFixed[0].ip := '127.0.0.1'; LFixed[0].port := 1;
      N.DiscoverFixedServersOnly(LFixed);
      TNode.Node.UpdateBlockchain := false;
      TNode.Node.NetServer.Port := 0;
      N.NetConnectionsActive := true;
      case ACheck of
        llcHeldLock: begin
          CheckLifetime(N,1,false);
          LLog.Add('PASS actual retirement waits for legacy lock release');
        end;
        llcRecursiveLock: begin
          CheckLifetime(N,2,false);
          LLog.Add('PASS nested legacy locks retain lifetime until both releases');
        end;
        llcTimedAdmission: begin
          CheckLifetime(N,1,true);
          LLog.Add('PASS timeout/nonblocking failures release admission pins');
        end;
      end;
      Result := 0;
    except on E: Exception do LLog.Add('FAIL '+E.ClassName+': '+E.Message); end;
  finally
    if Assigned(N) then begin
      N.BeginShutdown;
      N.Free;
    end;
    if TNode.NodeExists then TNode.Node.Free;
    LLog.SaveToFile(ALogFile);
    LLog.Free;
  end;
end;

function RunLegacyConnectionLifetimeChecks(const ALogFile: String): Integer;
var LCheck: TLegacyConnectionLifetimeCheck; LLog: TStringList;
begin
  Result := 1;
  Require(not FileExists(ALogFile),'Refusing an existing legacy lifetime summary');
  LLog := TStringList.Create;
  try
    for LCheck := Low(TLegacyConnectionLifetimeCheck) to High(TLegacyConnectionLifetimeCheck) do begin
      if RunLegacyConnectionLifetimeCheck(LCheck,ALogFile+'.'+IntToStr(Ord(LCheck)+1)+'.log')<>0 then begin
        LLog.Add(Format('FAIL %d; see scenario log',[Ord(LCheck)+1]));
        LLog.SaveToFile(ALogFile);
        Exit;
      end;
      LLog.Add(Format('PASS %d',[Ord(LCheck)+1]));
      LLog.SaveToFile(ALogFile);
    end;
    Result := 0;
  finally LLog.Free; end;
end;

end.
