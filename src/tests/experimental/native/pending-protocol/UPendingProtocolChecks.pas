unit UPendingProtocolChecks;
{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license. }
{$IFDEF FPC}{$mode delphi}{$ENDIF}
interface
function RunPendingProtocolChecks(const ALogFile: String): Integer;
implementation
uses Windows, Classes, SysUtils, SyncObjs, Generics.Collections, blcksock, UNode, UNetProtocol,
  UThread, UBlockChain, UAccounts, UOpTransaction, UPoolMining,
  UPCTNetDataExtraMessages, UCrypto, UConst, UPCDataTypes, UBaseTypes, ULog, UFileStorage;
type
  TAccessConnection = class(TNetConnection)
    {$IFDEF PASCAL_PROTOCOL_INTERNAL_CHECKS}
    procedure Serve(const AHeader: TNetHeaderData; AData: TStream);
    procedure RequestPending;
    procedure Prime;
    {$ENDIF}
  end;
  TAccessNetData = class(TNetData)
    procedure ServeExtra(AConnection: TNetConnection; const AHeader: TNetHeaderData;
      ARequest, AResponse: TStream);
  end;
  TJobSnapshotPool = class(TPoolMiningServer)
  private
    FArm, FRequestHeld: LongInt;
  protected
    procedure CaptureJobValues(Operations: TPCOperationsComp;
      out AValues: TMinerValuesForWork; out AJobsCount, AOperationsCount: Integer); override;
  public
    Captured, Replacement, ReleaseCapture: TEvent;
    CapturedValues, ReplacementValues: TMinerValuesForWork;
    constructor Create; override;
    destructor Destroy; override;
    procedure Arm;
  end;
  {$IFDEF PASCAL_PROTOCOL_INTERNAL_CHECKS}
  TPendingRequest = class(TPCThread)
    Connection: TAccessConnection;
    Done: TEvent;
    Error: String;
    constructor Create(AConnection: TAccessConnection);
    destructor Destroy; override;
    procedure BCExecute; override;
  end;
  {$ENDIF}
  TFixture = class
    Node: TNode;
    Net: TAccessNetData;
    Connection: TAccessConnection;
    Peer: TTCPBlockSocket;
    Buffer: TMemoryStream;
    Key: TECPrivateKey;
    Scope: Boolean;
    OldDirectory: String;
    constructor Create;
    destructor Destroy; override;
    procedure BeginWork;
    procedure EndWork;
    procedure SeedRawOperation;
    procedure LoadGenesis;
  end;
procedure Require(AValue: Boolean; const AReason: String);
begin
  if not AValue then raise Exception.Create(AReason);
end;
procedure Pump;
begin
  CheckSynchronize(0);
  Sleep(1);
end;

constructor TJobSnapshotPool.Create;
begin
  inherited;
  Captured := TEvent.Create(nil,True,False,'');
  Replacement := TEvent.Create(nil,True,False,'');
  ReleaseCapture := TEvent.Create(nil,True,False,'');
end;

destructor TJobSnapshotPool.Destroy;
begin
  // Release a failed test's actual accepted-client callback before joining it.
  ReleaseCapture.SetEvent;
  inherited;
  ReleaseCapture.Free;
  Replacement.Free;
  Captured.Free;
end;

procedure TJobSnapshotPool.Arm;
begin
  InterlockedExchange(FArm,1);
end;

procedure TJobSnapshotPool.CaptureJobValues(Operations: TPCOperationsComp;
  out AValues: TMinerValuesForWork; out AJobsCount, AOperationsCount: Integer);
begin
  inherited CaptureJobValues(Operations,AValues,AJobsCount,AOperationsCount);
  if not Assigned(Operations) and (InterlockedCompareExchange(FArm,0,1)=1) then begin
    CapturedValues := AValues;
    InterlockedExchange(FRequestHeld,1);
    Captured.SetEvent;
    // Hold the real request after capture, with no pool/node/socket lock.
    // The separate actual pool worker must be able to retire its borrowed job.
    if ReleaseCapture.WaitFor(10000)<>wrSignaled then
      raise Exception.Create('Timed out releasing the captured miner request');
    InterlockedExchange(FRequestHeld,0);
  end else if Assigned(Operations) and
    (InterlockedCompareExchange(FRequestHeld,0,0)<>0) then begin
    ReplacementValues := AValues;
    Replacement.SetEvent;
  end;
end;
{$IFDEF PASCAL_PROTOCOL_INTERNAL_CHECKS}
procedure TAccessConnection.Serve(const AHeader: TNetHeaderData; AData: TStream);
begin
  DoProcess_GetPendingOperations_Request(AHeader,AData);
end;
procedure TAccessConnection.RequestPending;
begin
  DoProcess_GetPendingOperations;
end;
procedure TAccessConnection.Prime;
var H: TNetHeaderData; S: TMemoryStream; LUntil: UInt64; LRead: Boolean;
begin
  S:=TMemoryStream.Create;
  try
    // The real decoder may return after a single empty 100ms receive wait.
    // Retry its public read boundary while the owned receiver starts up.
    LUntil:=GetTickCount64+3000;
    repeat
      LRead:=ReadTcpClientBuffer(100,H,S);
      if not LRead then Pump;
    until LRead or not Connected or (GetTickCount64>=LUntil);
    Require(LRead,'Cannot prime the actual protocol decoder');
  finally S.Free; end;
end;
{$ENDIF}
procedure TAccessNetData.ServeExtra(AConnection: TNetConnection; const AHeader: TNetHeaderData;
  ARequest, AResponse: TStream);
begin
  DoProcessReservedAreaMessage(AConnection,AHeader,ARequest,AResponse);
end;
{$IFDEF PASCAL_PROTOCOL_INTERNAL_CHECKS}
constructor TPendingRequest.Create(AConnection: TAccessConnection);
begin
  Connection:=AConnection; Done:=TEvent.Create(nil,True,False,'');
  inherited Create(True); FreeOnTerminate:=False;
end;
destructor TPendingRequest.Destroy;
begin
  Terminate;
  if Done.WaitFor(0)<>wrSignaled then Connection.Connected:=False;
  WaitFor; Done.Free; inherited;
end;
procedure TPendingRequest.BCExecute;
begin
  try try Connection.RequestPending;
    except on E: Exception do Error:=E.ClassName+': '+E.Message; end;
  finally Done.SetEvent; end;
end;
{$ENDIF}
procedure SendFrame(APeer: TTCPBlockSocket; AOperation: Word; ARequest: Cardinal;
  AError: Word; APayload: TMemoryStream);
var S: TMemoryStream; C: Cardinal; W: Word;
begin
  S:=TMemoryStream.Create;
  try
    C:=CT_MagicNetIdentification; S.WriteBuffer(C,4);
    W:=CT_MagicResponse; S.WriteBuffer(W,2);
    W:=AOperation; S.WriteBuffer(W,2); W:=AError; S.WriteBuffer(W,2);
    S.WriteBuffer(ARequest,4);
    W:=CT_NetProtocol_Version; S.WriteBuffer(W,2);
    W:=CT_NetProtocol_Available; S.WriteBuffer(W,2);
    C:=APayload.Size; S.WriteBuffer(C,4);
    APayload.Position:=0; S.CopyFrom(APayload,APayload.Size);
    Require(APeer.SendBuffer(S.Memory,Integer(S.Size))=S.Size,'Cannot send the owned protocol frame');
  finally S.Free; end;
end;
function ReadFrame(APeer: TTCPBlockSocket; ABuffer: TMemoryStream;
  out AHeader: TNetHeaderData; APayload: TMemoryStream; ATimeout: Cardinal): Boolean;
var LUntil: UInt64; LBytes: RawByteString; LMore: Boolean; LRest: TMemoryStream;
begin
  Result:=False; LUntil:=GetTickCount64+ATimeout;
  repeat
    ABuffer.Position:=0;
    if TNetData.ExtractHeaderInfo(ABuffer,AHeader,APayload,LMore) then begin
      LRest:=TMemoryStream.Create;
      try
        if ABuffer.Position<ABuffer.Size then LRest.CopyFrom(ABuffer,ABuffer.Size-ABuffer.Position);
        ABuffer.Clear; LRest.Position:=0; ABuffer.CopyFrom(LRest,LRest.Size);
      finally LRest.Free; end;
      APayload.Position:=0; Exit(True);
    end;
    if APeer.CanRead(10) then begin
      LBytes:=APeer.RecvPacket(100);
      if Length(LBytes)>0 then begin
        ABuffer.Position:=ABuffer.Size; ABuffer.WriteBuffer(LBytes[1],Length(LBytes));
      end;
    end;
    Pump;
  until GetTickCount64>=LUntil;
end;
constructor TFixture.Create;
var LListener: TTCPBlockSocket; LID: TGUID; LDirectory: String; S: TMemoryStream;
begin
  Require(not TNode.NodeExists and not TNetData.NetDataExists,'Protocol checks need fresh owned singletons');
  OldDirectory:=TNode.GetPascalCoinDataFolder; CreateGUID(LID);
  LDirectory:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP'))+'PendingProtocol-'+GUIDToString(LID);
  Require(not DirectoryExists(LDirectory),'Refusing an existing protocol data directory');
  ForceDirectories(LDirectory); TNode.SetPascalCoinDataFolder(LDirectory);
  TCrypto.InitCrypto; Net:=TAccessNetData.Create(nil); Node:=TNode.Create(nil);
  Node.NetServer.Port:=0; Net.NetConnectionsActive:=False;
  Buffer:=TMemoryStream.Create; Key:=TECPrivateKey.Create;
  Key.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  Net.NetConnectionsActive:=True;
  LListener:=TTCPBlockSocket.Create;
  try
    LListener.CreateSocket; LListener.Bind('127.0.0.1','0'); LListener.Listen;
    Require(LListener.LastError=0,'Cannot listen on the owned raw peer');
    Connection:=TAccessConnection.Create(nil);
    Require(Connection.ConnectTo('127.0.0.1',LListener.GetLocalSinPort),'Cannot connect the actual transport');
    Require(LListener.CanRead(1000),'Owned listener did not accept the transport');
    Peer:=TTCPBlockSocket.Create; Peer.Socket:=LListener.Accept;
  finally LListener.Free; end;
  {$IFDEF PASCAL_PROTOCOL_INTERNAL_CHECKS}
  // Decode a benign response through the real transport to establish the
  // available protocol version; this base connection has no competing parser.
  S:=TMemoryStream.Create;
  try SendFrame(Peer,CT_NetOp_GetBlockHeaders,0,0,S); Connection.Prime;
  finally S.Free; end;
  {$ENDIF}
end;
destructor TFixture.Destroy;
begin
  if Scope then EndWork;
  Peer.Free; Connection.Free;
  if Assigned(Net) then Net.NetConnectionsActive:=False;
  Net.Free; Node.Free; Key.Free; Buffer.Free;
  TNode.SetPascalCoinDataFolder(OldDirectory); inherited;
end;
procedure TFixture.BeginWork;
begin
  Node.BeginBlockChainUpdate;
  Scope:=True;
  Require(not Node.PendingOperationsAvailable,'Scope did not hide pending overlay');
end;
procedure TFixture.EndWork;
begin
  Node.EndBlockChainUpdate;
  Scope:=False;
end;
procedure TFixture.SeedRawOperation;
var LOp: TPCOperation; LPool: TPCOperationsComp; LError: String;
begin
  // A signed raw package entry makes zero-available replies distinguishable
  // from the retained raw count. This case tests wire availability, not account
  // admission; the empty confirmed bank is never used to validate this entry.
  LOp:=TOpTransaction.CreateTransaction(CT_PROTOCOL_1,0,1,1,Key,1,1,CT_TOperationPayload_NUL);
  try
    LPool:=Node.LockMempoolWrite;
    try Require(LPool.AddOperation(False,LOp,LError),LError);
    finally Node.UnlockMempoolWrite; end;
  finally LOp.Free; end;
end;
procedure TFixture.LoadGenesis;
var S: TFileStream; B: TPCOperationsComp; C: Cardinal; E: String;
begin
  Node.Bank.StorageClass:=TFileStorage;
  S:=TFileStream.Create(ExtractFilePath(ParamStr(0))+'mainnet-blocks-0-4.bin',fmOpenRead or fmShareDenyNone);
  B:=TPCOperationsComp.Create(nil);
  try
    S.ReadBuffer(C,4); Require(C=5,'Unexpected public historic fixture');
    B.bank:=Node.Bank; Require(B.LoadBlockFromStream(S,E),E);
    Require(B.OperationBlock.block=0,'Expected the real public genesis block');
    Require(Node.AddNewBlockChain(nil,B,E),E);
    Require(Node.Bank.BlocksCount=1,'Public genesis did not commit through actual validation');
  finally B.Free; S.Free; end;
end;
{$IFDEF PASCAL_PROTOCOL_INTERNAL_CHECKS}
procedure CheckBusyWire;
var F: TFixture; H,R: TNetHeaderData; Q,S: TMemoryStream; B: Byte; C: Cardinal;
  Tree: TOperationsHashTree; E: String; P,PC: Integer; Extra: TPCTNetDataExtraMessages; W: Word;
begin
  F:=TFixture.Create; Q:=TMemoryStream.Create; S:=TMemoryStream.Create;
  try
    F.SeedRawOperation; F.BeginWork;
    H:=Default(TNetHeaderData); H.header_type:=ntp_request; H.operation:=CT_NetOp_GetPendingOperations;
    for B:=1 to 2 do begin
      Q.Clear; Q.WriteBuffer(B,1); C:=0;
      if B=2 then begin
        Q.WriteBuffer(C,4);
        C:=10;
        Q.WriteBuffer(C,4);
      end;
      H.request_id:=F.Net.NewRequestId; Q.Position:=0; F.Connection.Serve(H,Q);
      repeat Require(ReadFrame(F.Peer,F.Buffer,R,S,2000),'No pending response'); until R.header_type=ntp_response;
      Require((R.request_id=H.request_id) and not R.is_error,'Unavailable pending response broke legacy wire compatibility');
      if B=1 then begin
        Require((S.Size=4) and (S.Read(C,4)=4) and (C=0),'Legacy count decoder did not see zero available operations');
      end else begin
        Tree:=TOperationsHashTree.Create;
        try
          Require(Tree.LoadOperationsHashTreeFromStream(S,False,CT_PROTOCOL_1,CT_PROTOCOL_1,nil,
            CT_AllowPropagate0feeOperations,P,PC,E),E);
          Require(Tree.OperationsCount=0,'Legacy tree decoder did not see an empty available package');
        finally Tree.Free; end;
      end;
      Require(F.Connection.Connected,'A normal unavailable response disconnected the peer');
    end;
    Extra:=TPCTNetDataExtraMessages.Create(F.Node,F.Net,nil);
    try
      Q.Clear; S.Clear; TStreamOp.WriteAccountKey(Q,F.Key.PublicKey); Q.Position:=0;
      H.operation:=CT_NetProtocol_Extra_NetOp_GIVE_ME_AN_ACCOUNT;
      F.Net.ServeExtra(F.Connection,H,Q,S); S.Position:=0;
      Require((S.Size=2) and (S.Read(W,2)=2) and (W=0),'Catch-up did not use the existing free-account refusal reply');
    finally Extra.Free; end;
  finally S.Free; Q.Free; F.Free; end;
end;
procedure CheckMatchingError(AErrorOnPage: Boolean);
var F: TFixture; T: TPendingRequest; H: TNetHeaderData; S,Q: TMemoryStream;
  C: Cardinal; B: Byte; LUntil: UInt64;
begin
  F:=TFixture.Create; T:=nil; S:=TMemoryStream.Create; Q:=TMemoryStream.Create;
  try
    // Pending requests need a validated local tip that includes the peer's tip.
    F.LoadGenesis;
    T:=TPendingRequest.Create(F.Connection); T.Start;
    repeat Require(ReadFrame(F.Peer,F.Buffer,H,S,2000),'Requester did not send a pending request');
    until H.operation=CT_NetOp_GetPendingOperations;
    Require(S.Read(B,1)=1,'Missing pending request kind');
    if AErrorOnPage then begin
      Require(B=1,'First pending request is not a count'); C:=1; Q.WriteBuffer(C,4);
      SendFrame(F.Peer,H.operation,H.request_id,0,Q);
      Require(ReadFrame(F.Peer,F.Buffer,H,S,2000),'Requester did not ask for the available page');
      Require((H.operation=CT_NetOp_GetPendingOperations) and (S.Read(B,1)=1) and (B=2),'Missing pending page request');
    end;
    Q.Clear; TStreamOp.WriteAnsiString(Q,TEncoding.ASCII.GetBytes('Temporary pending unavailability'));
    SendFrame(F.Peer,H.operation,H.request_id,CT_NetError_NotAvailable,Q);
    LUntil:=GetTickCount64+1500;
    repeat Pump; until (T.Done.WaitFor(0)=wrSignaled) or (GetTickCount64>=LUntil);
    Require(T.Done.WaitFor(0)=wrSignaled,'Matching protocol error was parsed as another pending request');
    Require(T.Error='',T.Error);
    Require(F.Connection.Connected,'Matching protocol error was misclassified as malformed peer data');
  finally T.Free; Q.Free; S.Free; F.Free; end;
end;
{$ENDIF}
function AwaitJSON(APeer: TTCPBlockSocket; var ABuffer: String; const APhase: String): String;
var LUntil: UInt64; I: Integer;
begin
  LUntil:=GetTickCount64+4000;
  repeat
    I:=Pos(#10,ABuffer);
    if I>0 then begin
      Result:=Copy(ABuffer,1,I-1);
      Delete(ABuffer,1,I);
      Exit;
    end;
    if APeer.CanRead(10) then ABuffer:=ABuffer+String(APeer.RecvPacket(100));
    Pump;
  until GetTickCount64>=LUntil;
  raise Exception.Create('No JSON response from the actual miner owner: '+APhase);
end;
procedure CheckBusyMiner(ASeedBank: Boolean = True; AEmptyPayload: Boolean = False);
var F: TFixture; Pool: TPoolMiningServer; Miner, Reservation: TTCPBlockSocket;
  Port: Word; Payload: TRawBytes; Buffer, Line: String;
begin
  F:=TFixture.Create; Pool:=nil; Miner:=nil; Reservation:=TTCPBlockSocket.Create;
  try
    if ASeedBank then F.LoadGenesis;
    F.BeginWork; Reservation.CreateSocket; Reservation.Bind('127.0.0.1','0');
    Require(Reservation.LastError=0,'Cannot reserve an owned mining port');
    Port:=Reservation.GetLocalSinPort; Reservation.CloseSocket;
    Pool:=TPoolMiningServer.Create; Pool.Port:=Port;
    if AEmptyPayload then Payload:=nil else Payload.FromString('pending-protocol-qa');
    TLog.NewLog(ltinfo,'PendingProtocolQA',Format('Miner fixture blocks:%d payloadbytes:%d',[
      F.Node.Bank.BlocksCount,Length(Payload)]));
    Pool.UpdateAccountAndPayload(F.Key.PublicKey,Payload); Pool.Active:=True;
    Miner:=TTCPBlockSocket.Create; Miner.Connect('127.0.0.1',IntToStr(Port));
    Require(Miner.LastError=0,'Cannot connect to the actual mining owner');
    Miner.SendString('{"method":"miner-notify","params":[],"id":17}'+#10);
    Buffer:=''; Line:=StringReplace(AwaitJSON(Miner,Buffer,'busy request error'),' ','',[rfReplaceAll]);
    Require((Pos('"id":17',Line)>0) and (Pos('"error":',Line)>0) and
      (Pos('"error":null',Line)=0),'Busy miner request did not receive its normal JSON RPC error');
    Require(Pool.ClientsCount=1,'Busy miner request disconnected its connection');
    F.EndWork; Line:=AwaitJSON(Miner,Buffer,'resumed mining job');
    Require((Pos('miner-notify',Line)>0) and (Pos('"part1":',Line)>0),
      'The same miner connection did not receive a real job after availability');
    Require(Pool.ClientsCount=1,'Miner did not remain connected through availability');
  finally
    if Assigned(Miner) then Miner.CloseSocket;
    Pool.Free; Miner.Free; Reservation.Free; F.Free;
  end;
end;
procedure CheckNotificationLease;
var F: TFixture; LConnections: TList<TNetConnection>; LOperations: TOperationsHashTree;
  I: Integer; LStarted: UInt64;
begin
  F:=TFixture.Create; LConnections:=TList<TNetConnection>.Create;
  LOperations:=TOperationsHashTree.Create;
  try
    F.Net.AcquireDownloadConnections(LConnections);
    Require((LConnections.Count=1) and (LConnections[0]=F.Connection),
      'Cannot acquire the existing connection lifetime lease');
    Require(F.Net.ConnectionLock(F,F.Connection,0),'Cannot pin the notification enqueue boundary');
    try
      LConnections[0]:=nil;
      TThreadNodeNotifyOperations.Create(F.Connection,LOperations,F.Net);
    finally F.Net.ConnectionUnlock(F.Connection); end;
    LStarted:=GetTickCount64;
    F.Net.BeginShutdown;
    F.Connection.Connected:=False;
    FreeAndNil(F.Connection);
    Require(F.Net.ConnectionsCountAll=0,'Transferred notifier lease left a retired connection registered');
    Require(GetTickCount64-LStarted<5000,'Owned notification did not cooperate with orderly shutdown');
  finally
    for I:=0 to LConnections.Count-1 do
      if Assigned(LConnections[I]) then F.Net.ReleaseDownloadConnection(LConnections[I]);
    LOperations.Free; LConnections.Free; F.Free;
  end;
end;

procedure CheckMinerJobRetirement;
var F: TFixture; Pool: TJobSnapshotPool; Miner, Reservation: TTCPBlockSocket;
  Port: Word; LOldPayload, LNewPayload: TRawBytes; Buffer, Line: String;
  LUntil: UInt64;
begin
  F := TFixture.Create;
  Pool := nil;
  Miner := nil;
  Reservation := TTCPBlockSocket.Create;
  try
    F.LoadGenesis;
    Reservation.CreateSocket;
    Reservation.Bind('127.0.0.1','0');
    Require(Reservation.LastError=0,'Cannot reserve the snapshot mining port');
    Port := Reservation.GetLocalSinPort;
    Reservation.CloseSocket;
    LOldPayload.FromString('snapshot-old');
    LNewPayload.FromString('snapshot-new');
    Pool := TJobSnapshotPool.Create;
    Pool.Port := Port;
    Pool.UpdateAccountAndPayload(F.Key.PublicKey,LOldPayload);
    Pool.Active := True;
    Miner := TTCPBlockSocket.Create;
    Miner.Connect('127.0.0.1',IntToStr(Port));
    Require(Miner.LastError=0,'Cannot connect the snapshot miner');
    Buffer := '';
    Line := AwaitJSON(Miner,Buffer,'initial snapshot job');
    Require(Pos('miner-notify',Line)>0,'The snapshot miner has no initial job');
    Pool.Arm;
    Miner.SendString('{"method":"miner-notify","params":[],"id":88}'+#10);
    LUntil := GetTickCount64+4000;
    repeat Pump; until (Pool.Captured.WaitFor(0)=wrSignaled) or (GetTickCount64>=LUntil);
    Require(Pool.Captured.WaitFor(0)=wrSignaled,'The actual miner request did not capture its job');
    Require(TBaseType.Equals(Pool.CapturedValues.payload_start,LOldPayload),
      'The request did not capture the original job payload');
    Pool.UpdateAccountAndPayload(F.Key.PublicKey,LNewPayload);
    F.Node.NotifyBlocksChanged;
    LUntil := GetTickCount64+4000;
    repeat Pump; until (Pool.Replacement.WaitFor(0)=wrSignaled) or (GetTickCount64>=LUntil);
    Require(Pool.Replacement.WaitFor(0)=wrSignaled,
      'Job retirement was blocked by the paused miner request');
    Require(TBaseType.Equals(Pool.ReplacementValues.payload_start,LNewPayload),
      'The real pool worker did not clear and replace the old job');
    Require(TBaseType.Equals(Pool.CapturedValues.payload_start,LOldPayload),
      'Clearing the pool changed the independently captured request');
    Pool.ReleaseCapture.SetEvent;
    LUntil := GetTickCount64+4000;
    repeat
      Line := StringReplace(AwaitJSON(Miner,Buffer,'retired snapshot response'),' ','',[rfReplaceAll]);
    until (Pos('"id":88',Line)>0) or (GetTickCount64>=LUntil);
    Require((Pos('"id":88',Line)>0) and
      (Pos('"payload_start":"'+TCrypto.ToHexaString(LOldPayload)+'"',Line)>0),
      'The actual response used a retired/replacement job instead of its captured value');
    Require(Pos('"part1":"'+TCrypto.ToHexaString(Pool.CapturedValues.part1)+'"',Line)>0,
      'The response digest prefix differs from its captured job');
    Require(Pos('"part3":"'+TCrypto.ToHexaString(Pool.CapturedValues.part3)+'"',Line)>0,
      'The response digest suffix differs from its captured job');
    Require(Pool.ClientsCount=1,'Concurrent job retirement disconnected the miner');
  finally
    if Assigned(Pool) then Pool.ReleaseCapture.SetEvent;
    if Assigned(Miner) then Miner.CloseSocket;
    Pool.Free;
    Miner.Free;
    Reservation.Free;
    F.Free;
  end;
end;

procedure CheckProofOfWorkVectors;
const
  CExpected: array[0..1] of String = (
    '73E7793B06F15AC2B31520C315B3DC8C638BB34B2F2010E50976AE68F042C7B7',
    '3B274E21701CDD8721E209C76FCD079985D601F9630729F53970D75762E1CA53');
var B: TOperationBlock; O: TPCOperationsComp; K: TAccountKey;
  Payload, LReference: TRawBytes; I, J: Integer;
begin
  // Fixed SHA256d vectors were computed independently with Python struct/hashlib
  // from the protocol's little-endian fields, not from the method under test.
  K:=CT_TECDSA_Public_Nul; K.EC_OpenSSL_NID:=714;
  Require(TCrypto.HexaToRaw('79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798',K.x),'Invalid fixed X');
  Require(TCrypto.HexaToRaw('483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8',K.y),'Invalid fixed Y');
  for I:=0 to 1 do begin
    if (I=0) and (GetEnvironmentVariable('PASCAL_PENDING_POW_NONEMPTY_ONLY')='1') then Continue;
    O:=TPCOperationsComp.Create(nil);
    try
      B:=CT_OperationBlock_NUL; B.block:=42; B.reward:=500000; B.fee:=7;
      B.protocol_version:=CT_PROTOCOL_1; B.protocol_available:=6; B.compact_target:=$19000000;
      SetLength(B.initial_safe_box_hash,32);
      for J:=0 to 31 do B.initial_safe_box_hash[J]:=J;
      O.SetOperationBlock(B); O.AccountKey:=K;
      if I=0 then Payload:=nil else Payload.FromString('pending-protocol-qa');
      Require(O.Update_And_RecalcPOW(123456,1710000000,Payload),'Fixed vector did not recalculate');
      Require(SameText(TCrypto.ToHexaString(O.OperationBlock.proof_of_work),CExpected[I]),
        'Independent proof-of-work digest mismatch, payload='+IntToStr(I));
      TPascalCoinProtocol.CalcProofOfWork(O.OperationBlock,LReference);
      Require(SameText(TCrypto.ToHexaString(LReference),CExpected[I]),
        'Independent full-serializer digest mismatch, payload='+IntToStr(I));
      Require(TBaseType.Equals(LReference,O.OperationBlock.proof_of_work),
        'Cached and full proof-of-work serializers differ');
    finally O.Free; end;
  end;
end;

function RunPendingProtocolChecks(const ALogFile: String): Integer;
var LLog: TStringList; I, LOnly: Integer; LCoreLog: TLog;
begin
  Require(not FileExists(ALogFile),'Refusing an existing protocol result log');
  Result:=0; LLog:=TStringList.Create; LCoreLog:=TLog.Create(nil);
  try
    LCoreLog.ProcessGlobalLogs:=True; LCoreLog.SaveTypes:=CT_TLogTypes_ALL;
    LCoreLog.FileName:=ALogFile+'.core.log';
    LOnly:=StrToIntDef(GetEnvironmentVariable('PASCAL_PENDING_PROTOCOL_ONLY'),-1);
    {$IFDEF PASCAL_PROTOCOL_INTERNAL_CHECKS}
    for I:=0 to 8 do begin
    {$ELSE}
    for I:=3 to 8 do begin
    {$ENDIF}
      if (LOnly>=0) and (I<>LOnly) then Continue;
      try
        case I of
          {$IFDEF PASCAL_PROTOCOL_INTERNAL_CHECKS}
          0: CheckBusyWire;
          1: CheckMatchingError(False);
          2: CheckMatchingError(True);
          {$ENDIF}
          3: CheckBusyMiner;
          4: CheckNotificationLease;
          5: CheckBusyMiner(False,False);
          6: CheckBusyMiner(False,True);
          7: CheckProofOfWorkVectors;
          8: CheckMinerJobRetirement;
        end;
        LLog.Add('PASS pending protocol case='+IntToStr(I));
      except on E: Exception do begin
        Result:=1;
        LLog.Add('FAIL case='+IntToStr(I)+' '+E.ClassName+': '+E.Message);
      end; end;
      LLog.SaveToFile(ALogFile);
    end;
  finally LCoreLog.Free; LLog.Free; end;
end;
end.
