unit UConnectionRestartChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Explicit network restart retries recent peers; automatic discovery retains
  its existing backoff. All sockets and node state belong to this probe. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunConnectionRestartChecks(const ALogFile: String): Integer;

implementation

uses Windows, Classes, SysUtils, SyncObjs, blcksock, UNode, UNetProtocol, UThread,
  UPCDataTypes, UCrypto, UTime;

type
  TQuickDiscoveryWorker = class(TThreadDiscoverConnection)
  protected
    procedure BCExecute; override;
  end;

  TRestartNetData = class(TNetData)
  private
    FCreatedWorkers: LongInt;
  protected
    function CreateDiscoveryWorker(const AAddress: TNodeServerAddress): TThreadDiscoverConnection; override;
  public
    function CreatedWorkers: LongInt;
  end;

procedure Require(AValue: Boolean; const AReason: String);
begin
  if not AValue then raise Exception.Create(AReason);
end;

procedure TQuickDiscoveryWorker.BCExecute;
var I, LJitter: Integer;
begin
  // Keep the actual production connect/HELLO/finalization path. Choose only
  // its random initial jitter so a regression is not hidden by a long delay.
  for I := 0 to 100000 do begin
    RandSeed := I;
    LJitter := Random(1000);
    if (LJitter<10) and (Random(5)=0) then begin
      RandSeed := I;
      Break;
    end;
  end;
  inherited BCExecute;
end;

function TRestartNetData.CreateDiscoveryWorker(const AAddress: TNodeServerAddress): TThreadDiscoverConnection;
begin
  InterlockedIncrement(FCreatedWorkers);
  Result := TQuickDiscoveryWorker.Create(Self,AAddress);
end;

function TRestartNetData.CreatedWorkers: LongInt;
begin
  Result := InterlockedCompareExchange(FCreatedWorkers,0,0);
end;

function CheckRestart(AExplicitRestart: Boolean): Cardinal;
var LNode: TNode; LNet: TRestartNetData; LListener, LPeer: TTCPBlockSocket;
  LAddress: TNodeServerAddress; LFixed, LCandidates: TNodeServerAddressArray;
  LOldDirectory, LDirectory, LHello: String; LID: TGUID; LUntil, LStarted: UInt64;
begin
  Result := 0;
  Require(not TNode.NodeExists and not TNetData.NetDataExists,
    Format('Restart probe needs fresh owned singletons (node:%s network:%s)',[
      BoolToStr(TNode.NodeExists,True),BoolToStr(TNetData.NetDataExists,True)]));
  Require(GetEnvironmentVariable('TEMP')<>'','Provide an owned test temporary directory');
  LOldDirectory := TNode.GetPascalCoinDataFolder;
  CreateGUID(LID);
  LDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))+'ConnectionRestart-'+GUIDToString(LID);
  Require(not DirectoryExists(LDirectory),'Refusing an existing restart data directory');
  ForceDirectories(LDirectory);
  TNode.SetPascalCoinDataFolder(LDirectory);
  LNode := nil; LNet := nil; LListener := nil; LPeer := nil;
  try
    TCrypto.InitCrypto;
    // Register the owned network before the node's saver/notification workers
    // can request the global network singleton during node construction.
    LNet := TRestartNetData.Create(nil);
    Require(TNetData.NetData=LNet,'Restart fixture did not register its owned network singleton');
    LNode := TNode.Create(nil);
    LNode.NetServer.Port := 0;
    LNet.NetConnectionsActive := False;
    LListener := TTCPBlockSocket.Create;
    LListener.CreateSocket;
    LListener.Bind('127.0.0.1','0');
    Require(LListener.LastError=0,'Cannot reserve owned loopback listener');
    LListener.Listen;
    Require(LListener.LastError=0,'Cannot listen on owned loopback port');
    // For the automatic case, turn ON before supplying a candidate. Its later
    // discovery calls must retain the established retry delay.
    if not AExplicitRestart then LNet.NetConnectionsActive := True;
    SetLength(LFixed,1);
    LFixed[0].ip := '127.0.0.1';
    LFixed[0].port := LListener.GetLocalSinPort;
    // Publish the candidate and its prior attempt atomically. The existing
    // core maintenance worker must not discover it before backoff is seeded.
    LNet.NodeServersAddresses.LockList;
    try
      LNet.DiscoverFixedServersOnly(LFixed);
      Require(LNet.NodeServersAddresses.GetNodeServerAddress(LFixed[0].ip,LFixed[0].port,True,LAddress),
        'Cannot prepare recent peer attempt');
      LAddress.last_attempt_to_connect := Now;
      LAddress.total_failed_attemps_to_connect := 3;
      // Keep this genuinely recent peer eligible for the normal retained-peer
      // retry rules; cleanup otherwise deletes never-successful failed peers.
      LAddress.last_connection_by_me := UnivDateTimeToUnix(DateTime2UnivDateTime(Now));
      LNet.NodeServersAddresses.SetNodeServerAddress(LAddress);
    finally LNet.NodeServersAddresses.UnlockList; end;
    LCandidates := Copy(LFixed,0,Length(LFixed));
    LNet.NodeServersAddresses.GetNodeServersToConnnect(1,True,LCandidates);
    Require(Length(LCandidates)=0,'The seeded recent attempt did not establish normal backoff');
    if AExplicitRestart then begin
      LStarted := GetTickCount64;
      LNet.NetConnectionsActive := True;
      Require(LNet.CreatedWorkers>0,'Explicit OFF -> ON skipped its first discovery pass for a recent peer');
      LUntil := GetTickCount64+5000;
      repeat
        CheckSynchronize(0);
        if LListener.CanRead(10) then Break;
        Sleep(1);
      until GetTickCount64>=LUntil;
      Require(LListener.CanRead(0),'Restart did not make an actual loopback connection');
      LPeer := TTCPBlockSocket.Create;
      LPeer.Socket := LListener.Accept;
      LHello := String(LPeer.RecvPacket(1000));
      Require(Length(LHello)>0,'Restarted discovery did not send its actual HELLO');
      Result := Cardinal(GetTickCount64-LStarted);
    end else begin
      LNet.DiscoverServers;
      Require(LNet.CreatedWorkers=0,'Automatic discovery discarded normal retry backoff');
      Require(not LListener.CanRead(100),'Automatic discovery connected during normal retry backoff');
    end;
  finally
    if Assigned(LNet) then LNet.NetConnectionsActive := False;
    LNet.Free;
    LNode.Free;
    LPeer.Free;
    LListener.Free;
    TNode.SetPascalCoinDataFolder(LOldDirectory);
  end;
end;

function RunConnectionRestartChecks(const ALogFile: String): Integer;
var LLog: TStringList; LElapsed: Cardinal;
begin
  Require(not FileExists(ALogFile),'Refusing an existing restart result log');
  Result := 1;
  LLog := TStringList.Create;
  try
    try
      CheckRestart(False);
      LLog.Add('PASS automatic discovery preserves backoff');
      LLog.SaveToFile(ALogFile);
      LElapsed := CheckRestart(True);
      LLog.Add(Format('PASS explicit OFF -> ON starts first discovery, connects recent peer and sends actual HELLO; milliseconds:%d',[LElapsed]));
      Result := 0;
    except on E: Exception do LLog.Add('FAIL '+E.ClassName+': '+E.Message); end;
    LLog.SaveToFile(ALogFile);
  finally LLog.Free; end;
end;

end.
