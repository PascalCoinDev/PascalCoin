unit UBufferedBlockFrameChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Actual incoming/outgoing socket owners receive a GetBlocks response followed
  by a full unsolicited NewBlock. Gates observe receipt, not simulated commits. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

type
  TBufferedFrameScenario = (bfsOutgoingTransportOff, bfsIncomingTransportOff,
    bfsOutgoingParserOff, bfsIncomingParserOff, bfsOutgoingEOF, bfsIncomingEOF,
    bfsOutgoingShutdown, bfsIncomingShutdown, bfsOutgoingFastComplete,
    bfsIncomingFastMissing, bfsOutgoingMalformed, bfsIncomingBadProtocol,
    bfsOutgoingFirstBufferedOff, bfsOutgoingMatchedOff, bfsReceivePublication);

procedure RunBufferedFrameScenario(AScenario: TBufferedFrameScenario);
function RunBufferedBlockFrameChecks(const ALogFile: String): Integer;

implementation

uses Windows, Classes, SysUtils, SyncObjs, Generics.Collections, blcksock,
  UNode, UNetProtocol, UTCPIP, UThread, UBlockChain, UAccounts, UFileStorage,
  UConst, UCrypto, UPCDataTypes, UBaseTypes;

type
  TPeerSafeBox = class(TPCSafeBox)
  public
    procedure AddHeader(const AHeader: TOperationBlock);
  end;

  TFrameCase = class;
  TMatchedResponseTask = class(TPCThread)
  private
    FOwner: TFrameCase;
    FStarted: Boolean;
  protected
    procedure BCExecute; override;
  public
    Done: TEvent;
    Error: String;
    constructor Create(AOwner: TFrameCase);
    destructor Destroy; override;
  end;

  TFrameConnectionAccess = class(TNetConnection)
  public
    procedure Apply(const AHeader: TNetHeaderData; AData: TStream);
    procedure FinishBuffered;
  end;

  TNetworkOff = class(TPCThread)
  private
    FOwner: TFrameCase;
    FStarted: Boolean;
  protected
    procedure BCExecute; override;
  public
    Done: TEvent;
    Error: String;
    constructor Create(AOwner: TFrameCase);
    destructor Destroy; override;
  end;

  TFrameCase = class(TComponent)
  private
    FNode: TNode;
    FNet: TNetData;
    FConnection: TNetConnection;
    FTransport: TBufferedNetTcpIpClient;
    FPeer, FListener: TTCPBlockSocket;
    FBankNotify: TPCBankNotify;
    FNodeNotify: TNodeNotifyEvents;
    FFirstCommit, FRelease: TEvent;
    FOff: TNetworkOff;
    FMatchedTask: TMatchedResponseTask;
    FMatched: Boolean;
    FRequestID: Cardinal;
    FOldDirectory, FDirectory: String;
    FExpectedLogSender, FExpectedLogRemote: String;
    FExpectedBatchBytes: Int64;
    FCommits, FReservedCalls, FDestroyed: LongInt;
    FIdleBeforeDrain: Boolean;
    FObserving: Boolean;
    FExpected: Integer;
    FFast, FMissing, FMalformed, FBadProtocol: Boolean;
    FWire: TMemoryStream;
    FBlocks: array[0..4] of TPCOperationsComp;
    FPeerSafeBox: TPeerSafeBox;
    procedure Committed(Sender: TObject);
    procedure SynchronizationChanged(Sender: TObject;
      const AStatus: TNodeSynchronizationStatus);
    procedure Reserved(NetData: TNetData; SenderConnection: TNetConnection;
      const HeaderData: TNetHeaderData; ReceivedData, ResponseData: TStream);
    procedure Await(AEvent: TEvent; ATimeout: Cardinal; const AReason: String);
    procedure Open(AIncoming: Boolean);
    procedure LoadBlocks;
    procedure CaptureFrame(AType: TNetTransferType; AOperation: Word;
      ARequest: Cardinal; AResult: TStream);
    procedure BuildFrames(AFirst, ASecond: TMemoryStream);
    procedure SendWire(AWire: TMemoryStream);
    function BufferedBytes: Int64;
    procedure CheckSecondReceived(ABytes: Int64; AParserSuffix: Boolean);
    procedure CheckStoredBlocks;
    procedure CheckBlockLog;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run(AScenario: TBufferedFrameScenario);
  end;

  TGateReadClient = class(TBufferedNetTcpIpClient)
  protected
    function ReceiveBuf(var Buf; BufSize: Integer): Integer; override;
  public
    Entered, ReleaseRead: TEvent;
    GateNext: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  TReadQuiescenceTask = class(TPCThread)
  private
    FStarted: Boolean;
  protected
    procedure BCExecute; override;
  public
    Client: TGateReadClient;
    Paused, Done: TEvent;
    Captured, Error: String;
    constructor Create(AClient: TGateReadClient);
    destructor Destroy; override;
  end;

var GLog: TStringList; GLogFile: String;

procedure Log(const AText: String);
begin
  if not Assigned(GLog) then Exit;
  GLog.Add(AText);
  GLog.SaveToFile(GLogFile);
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

procedure TFrameConnectionAccess.Apply(const AHeader: TNetHeaderData; AData: TStream);
begin
  DoProcess_GetBlocks_Response(AHeader,AData);
end;

procedure TFrameConnectionAccess.FinishBuffered;
begin
  DrainReceivedBlockFrames;
end;

constructor TMatchedResponseTask.Create(AOwner: TFrameCase);
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  FOwner:=AOwner;
  Done:=TEvent.Create(nil,True,False,'');
end;

destructor TMatchedResponseTask.Destroy;
begin
  if FStarted then WaitFor;
  Done.Free;
  inherited;
end;

procedure TMatchedResponseTask.BCExecute;
var LSend,LReceived: TMemoryStream; LHeader: TNetHeaderData; LStart,LEnd: Cardinal;
begin
  LSend:=TMemoryStream.Create; LReceived:=TMemoryStream.Create;
  try
    try
      FOwner.FNode.BeginBlockChainUpdate;
      try
        Require(FOwner.FNet.ConnectionLock(Self,FOwner.FConnection,1000),
          'Matched request cannot acquire its real recursive processing lock');
        try
          LStart:=0; LEnd:=3; LSend.WriteBuffer(LStart,4); LSend.WriteBuffer(LEnd,4);
          FOwner.FNet.RegisterRequest(FOwner.FConnection,CT_NetOp_GetBlocks,FOwner.FRequestID);
          Require(FOwner.FConnection.DoSendAndWaitForResponse(CT_NetOp_GetBlocks,
            FOwner.FRequestID,LSend,LReceived,5000,LHeader),'The real matching response did not arrive');
          Require((LHeader.operation=CT_NetOp_GetBlocks) and
            (LHeader.request_id=FOwner.FRequestID),'The matching branch returned the wrong frame');
          TFrameConnectionAccess(FOwner.FConnection).Apply(LHeader,LReceived);
        finally
          try TFrameConnectionAccess(FOwner.FConnection).FinishBuffered;
          finally FOwner.FNet.ConnectionUnlock(FOwner.FConnection); end;
        end;
      finally FOwner.FNode.EndBlockChainUpdate; end;
    except on E: Exception do Error:=E.ClassName+': '+E.Message; end;
  finally LReceived.Free; LSend.Free; Done.SetEvent; end;
end;

constructor TGateReadClient.Create;
begin
  inherited Create(nil);
  Entered := TEvent.Create(nil,True,False,'');
  ReleaseRead := TEvent.Create(nil,True,False,'');
end;

destructor TGateReadClient.Destroy;
begin
  ReleaseRead.SetEvent;
  inherited;
  ReleaseRead.Free;
  Entered.Free;
end;

function TGateReadClient.ReceiveBuf(var Buf; BufSize: Integer): Integer;
begin
  Result := inherited ReceiveBuf(Buf,BufSize);
  if (Result>0) and GateNext then begin
    GateNext := False;
    Entered.SetEvent;
    Require(ReleaseRead.WaitFor(5000)=wrSignaled,'The real returned-read gate expired');
  end;
end;

constructor TReadQuiescenceTask.Create(AClient: TGateReadClient);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Client := AClient;
  Paused := TEvent.Create(nil,True,False,'');
  Done := TEvent.Create(nil,True,False,'');
end;

destructor TReadQuiescenceTask.Destroy;
begin
  if FStarted then WaitFor;
  Done.Free; Paused.Free;
  inherited;
end;

procedure TReadQuiescenceTask.BCExecute;
var LBuffer: TMemoryStream; LBytes: RawByteString;
begin
  try
    try
      Client.PauseReceiving;
      try
        Client.Disconnect;
        Paused.SetEvent;
        Client.WaitForReceiveIdle;
        LBuffer := Client.ReadBufferLock;
        try
          SetLength(LBytes,LBuffer.Size);
          if Length(LBytes)>0 then Move(LBuffer.Memory^,LBytes[1],Length(LBytes));
        finally Client.ReadBufferUnlock; end;
        Captured := String(LBytes);
      finally Client.ResumeReceiving; end;
    except on E: Exception do Error := E.ClassName+': '+E.Message; end;
  finally Done.SetEvent; end;
end;

procedure CheckReceivePublication;
const CFirst: RawByteString='returned-before-append'; CSecond: RawByteString='reconnected-input';
var LClient: TGateReadClient; LListener, LPeer: TTCPBlockSocket;
  LTask: TReadQuiescenceTask; LBuffer: TMemoryStream; LBytes: RawByteString; LUntil: UInt64;
begin
  LClient:=nil; LPeer:=nil; LTask:=nil;
  LListener:=TTCPBlockSocket.Create;
  try
    LListener.CreateSocket; LListener.Bind('127.0.0.1','0'); LListener.Listen;
    Require(LListener.LastError=0,'Cannot open the receive-publication raw peer');
    LClient:=TGateReadClient.Create;
    LClient.RemoteHost:='127.0.0.1'; LClient.RemotePort:=LListener.GetLocalSinPort;
    LClient.GateNext:=True;
    Require(LClient.Connect,'Cannot connect the real buffered transport');
    Require(LListener.CanRead(1000),'Receive-publication peer did not accept');
    LPeer:=TTCPBlockSocket.Create; LPeer.Socket:=LListener.Accept;
    Require(LPeer.SendBuffer(@CFirst[1],Length(CFirst))=Length(CFirst),'Cannot send returned-read bytes');
    Require(LClient.Entered.WaitFor(3000)=wrSignaled,'The inherited socket read did not return');
    LTask:=TReadQuiescenceTask.Create(LClient); LTask.Start; LTask.FStarted:=True;
    Require(LTask.Paused.WaitFor(1000)=wrSignaled,'The owner did not pause/disconnect');
    Require(LTask.Done.WaitFor(200)=wrTimeout,'Idle was declared before returned bytes were published');
    LClient.ReleaseRead.SetEvent;
    Require(LTask.Done.WaitFor(3000)=wrSignaled,'Receive publication did not release the idle waiter');
    Require((LTask.Error='') and (LTask.Captured=String(CFirst)),
      'Disconnect lost bytes returned by the real inherited socket read: '+LTask.Error);
    LTask.Free; LTask:=nil;
    LBuffer:=LClient.ReadBufferLock;
    try LBuffer.Size:=0; finally LClient.ReadBufferUnlock; end;
    LPeer.Free; LPeer:=nil;
    Require(LClient.Connect,'Balanced resume did not permit same-object reconnect');
    Require(LListener.CanRead(1000),'Reconnect did not reach the raw peer');
    LPeer:=TTCPBlockSocket.Create; LPeer.Socket:=LListener.Accept;
    Require(LPeer.SendBuffer(@CSecond[1],Length(CSecond))=Length(CSecond),'Cannot send reconnected bytes');
    LUntil:=GetTickCount64+3000;
    repeat
      LBuffer:=LClient.ReadBufferLock;
      try
        SetLength(LBytes,LBuffer.Size);
        if Length(LBytes)>0 then Move(LBuffer.Memory^,LBytes[1],Length(LBytes));
      finally LClient.ReadBufferUnlock; end;
      if LBytes=CSecond then Break;
      Pump;
    until GetTickCount64>=LUntil;
    Require(LBytes=CSecond,'The reused IO worker did not receive after reconnect');
    Log('PASS real returned-read publication waits + same-object reconnect');
  finally
    if Assigned(LClient) then LClient.ReleaseRead.SetEvent;
    LTask.Free; LClient.Free; LPeer.Free; LListener.Free;
  end;
end;

procedure TPeerSafeBox.AddHeader(const AHeader: TOperationBlock);
begin
  AddNew(AHeader);
end;

constructor TNetworkOff.Create(AOwner: TFrameCase);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  Done := TEvent.Create(nil, True, False, '');
end;

destructor TNetworkOff.Destroy;
begin
  if FStarted then WaitFor;
  Done.Free;
  inherited;
end;

procedure TNetworkOff.BCExecute;
begin
  try
    try FOwner.FNet.NetConnectionsActive := False;
    except on E: Exception do Error := E.ClassName + ': ' + E.Message; end;
  finally Done.SetEvent; end;
end;

constructor TFrameCase.Create;
begin
  inherited Create(nil);
  FFirstCommit := TEvent.Create(nil, True, False, '');
  FRelease := TEvent.Create(nil, True, False, '');
  FWire := TMemoryStream.Create;
end;

destructor TFrameCase.Destroy;
var I: Integer;
begin
  FObserving := False;
  if Assigned(FNet) then FNet.BeginShutdown;
  FRelease.SetEvent;
  FOff.Free;
  FMatchedTask.Free;
  if Assigned(FNet) then begin
    FNet.NetConnectionsActive := False;
    FNet.Free;
  end;
  // Keep commit observers and their gate alive until every actual owner and
  // its callback has joined, including cleanup after a failed assertion.
  if Assigned(FNodeNotify) then FNodeNotify.Node := nil;
  FNodeNotify.Free;
  FBankNotify.Free;
  FNode.Free;
  FPeer.Free;
  FListener.Free;
  for I := 0 to 4 do FBlocks[I].Free;
  FPeerSafeBox.Free;
  FWire.Free;
  FRelease.Free;
  FFirstCommit.Free;
  if FOldDirectory <> '' then TNode.SetPascalCoinDataFolder(FOldDirectory);
  inherited;
end;

procedure TFrameCase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FConnection) then
    InterlockedExchange(FDestroyed, 1);
end;

procedure TFrameCase.Committed(Sender: TObject);
var LCount: Integer; LStatus: TNodeSynchronizationStatus;
begin
  LCount := InterlockedIncrement(FCommits);
  LStatus:=FNode.GetSynchronizationStatus;
  Require((LStatus.State = nssSynchronizing) and (LStatus.ActiveTransfers>0),
    'A committed buffered block lost its physical synchronization scope');
  Log(Format('COMMITTED block=%d physical_scopes=%d',[LCount-1,LStatus.ActiveTransfers]));
  if LCount = 1 then begin
    FFirstCommit.SetEvent;
    Require(FRelease.WaitFor(10000) = wrSignaled, 'First commit gate expired');
  end;
end;

procedure TFrameCase.SynchronizationChanged(Sender: TObject;
  const AStatus: TNodeSynchronizationStatus);
begin
  if FObserving and (InterlockedCompareExchange(FCommits, 0, 0) < FExpected) and
    (AStatus.State <> nssSynchronizing) then FIdleBeforeDrain := True;
end;

procedure TFrameCase.Reserved(NetData: TNetData; SenderConnection: TNetConnection;
  const HeaderData: TNetHeaderData; ReceivedData, ResponseData: TStream);
begin
  InterlockedIncrement(FReservedCalls);
end;

procedure TFrameCase.Await(AEvent: TEvent; ATimeout: Cardinal; const AReason: String);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + ATimeout;
  repeat
    Pump;
    if AEvent.WaitFor(0) = wrSignaled then Exit;
  until GetTickCount64 >= LUntil;
  raise Exception.Create(AReason);
end;

procedure TFrameCase.Open(AIncoming: Boolean);
var LID: TGUID; LDirectory: String; LPort: Word; LUntil: UInt64;
  LConnections: TList<TNetConnection>; LConnection: TNetConnection;
  LFixed: TNodeServerAddressArray;
begin
  Require(not TNode.NodeExists and not TNetData.NetDataExists,
    'A frame scenario requires fresh owned core singletons');
  FOldDirectory := TNode.GetPascalCoinDataFolder;
  CreateGUID(LID);
  Require(GetEnvironmentVariable('TEMP')<>'','Provide an owned temporary test directory');
  LDirectory := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
    'BufferedFrames-' + GUIDToString(LID);
  ForceDirectories(LDirectory);
  FDirectory := LDirectory;
  TNode.SetPascalCoinDataFolder(LDirectory);
  TCrypto.InitCrypto;
  FNode := TNode.Create(nil);
  FNode.NodeLogFilename := FDirectory + PathDelim + 'blocks.log';
  FNode.BroadcastData := False;
  FNode.Bank.StorageClass := TFileStorage;
  FNet := TNetData.NetData;
  FNet.NetConnectionsActive := False;
  TNode.DecodeIpStringToNodeServerAddressArray('127.0.0.1:1', LFixed);
  FNet.DiscoverFixedServersOnly(LFixed);
  FNet.OnProcessReservedAreaMessage := Reserved;
  FListener := TTCPBlockSocket.Create;
  FListener.CreateSocket;
  FListener.Bind('127.0.0.1', '0');
  Require(FListener.LastError = 0, 'Cannot reserve an owned loopback port');
  LPort := FListener.GetLocalSinPort;
  if AIncoming then begin
    FListener.CloseSocket;
    FNode.NetServer.Port := LPort;
  end else begin
    FListener.Listen;
    Require(FListener.LastError = 0, 'Cannot listen on the owned raw peer');
    FNode.NetServer.Port := 0;
  end;
  FNet.NetConnectionsActive := True;
  FPeer := TTCPBlockSocket.Create;
  if AIncoming then begin
    FPeer.Connect('127.0.0.1', IntToStr(LPort));
    Require(FPeer.LastError = 0, 'Cannot connect to the real incoming owner');
    LUntil := GetTickCount64 + 3000;
    repeat
      LConnections := FNet.NetConnections.LockList;
      try
        for LConnection in LConnections do
          if LConnection is TNetServerClient then FConnection := LConnection;
      finally FNet.NetConnections.UnlockList; end;
      if Assigned(FConnection) then Break;
      Pump;
    until GetTickCount64 >= LUntil;
  end else begin
    FConnection := TNetClient.Create(nil);
    Require(FConnection.ConnectTo('127.0.0.1', LPort), 'Cannot connect the real outgoing owner');
    Require(FListener.CanRead(1000), 'Raw peer did not accept the outgoing owner');
    FPeer.Socket := FListener.Accept;
  end;
  Require(Assigned(FConnection), 'The real transport owner did not register its connection');
  FExpectedLogSender := FConnection.ClassName;
  FExpectedLogRemote := FConnection.ClientRemoteAddr;
  FConnection.FreeNotification(Self);
  FTransport := TBufferedNetTcpIpClient(FConnection.Client);
  FBankNotify := TPCBankNotify.Create(Self);
  FBankNotify.Bank := FNode.Bank;
  FBankNotify.OnNewBlock := Committed;
  FNodeNotify := TNodeNotifyEvents.Create(nil);
  FNodeNotify.OnSynchronizationChanged := SynchronizationChanged;
  FNodeNotify.Node := FNode;
end;

procedure TFrameCase.LoadBlocks;
var LSource: TResourceStream; LCount: Cardinal; I: Integer; LError: String;
begin
  LSource := TResourceStream.Create(HInstance, 'HISTORIC_BUFFERED_BLOCKS', RT_RCDATA);
  try
    LSource.ReadBuffer(LCount, SizeOf(LCount));
    Require(LCount = 5, 'Expected the provenance-checked five historic blocks');
    FPeerSafeBox := TPeerSafeBox.Create;
    for I := 0 to 4 do begin
      FBlocks[I] := TPCOperationsComp.Create(nil);
      Require(FBlocks[I].LoadBlockFromStream(LSource, LError), LError);
      Require(FBlocks[I].OperationBlock.block = Cardinal(I), 'Wrong historic block order');
      FPeerSafeBox.AddHeader(FBlocks[I].OperationBlock);
    end;
    Require(LSource.Position = LSource.Size, 'Unexpected historic fixture suffix');
  finally LSource.Free; end;
end;

procedure TFrameCase.CaptureFrame(AType: TNetTransferType; AOperation: Word;
  ARequest: Cardinal; AResult: TStream);
var LUntil: UInt64; LPacket: RawByteString; LAt, LReadAt: Int64;
  LHeader: TNetHeaderData; LPayload: TMemoryStream; LMore: Boolean;
begin
  LPayload := TMemoryStream.Create;
  try
    LUntil := GetTickCount64 + 3000;
    repeat
      LAt := FWire.Position;
      while TNetData.ExtractHeaderInfo(FWire, LHeader, LPayload, LMore) do begin
        if (LHeader.header_type = AType) and (LHeader.operation = AOperation) and
          (LHeader.request_id = ARequest) then begin
          FWire.Position := LAt;
          AResult.CopyFrom(FWire, 22 + LHeader.buffer_data_length);
          Exit;
        end;
        LAt := FWire.Position;
      end;
      if FPeer.CanRead(10) then begin
        LPacket := FPeer.RecvPacket(10);
        if Length(LPacket) > 0 then begin
          LReadAt := FWire.Position;
          FWire.Position := FWire.Size;
          FWire.WriteBuffer(LPacket[1], Length(LPacket));
          FWire.Position := LReadAt;
        end;
      end;
      Pump;
    until GetTickCount64 >= LUntil;
    raise Exception.Create('The actual sender did not emit the expected wire frame');
  finally LPayload.Free; end;
end;

procedure TFrameCase.BuildFrames(AFirst, ASecond: TMemoryStream);
var LRequest, LStart, LEnd, LCount: Cardinal; I: Integer;
  LPayload, LRequestWire, LControl: TMemoryStream; LOpcode, LBadVersion: Word;
  LReference: UInt64;
begin
  LPayload := TMemoryStream.Create;
  LRequestWire := TMemoryStream.Create;
  LControl := TMemoryStream.Create;
  try
    LRequest := FNet.NewRequestId;
    FRequestID:=LRequest;
    LStart := 0; LEnd := 3;
    LPayload.WriteBuffer(LStart, 4); LPayload.WriteBuffer(LEnd, 4);
    if not FMatched then begin
      FNet.RegisterRequest(FConnection, CT_NetOp_GetBlocks, LRequest);
      FConnection.DoSend(ntp_request, CT_NetOp_GetBlocks, 0, LRequest, LPayload);
      CaptureFrame(ntp_request, CT_NetOp_GetBlocks, LRequest, LRequestWire);
    end;
    LPayload.Clear;
    LCount := 4; LPayload.WriteBuffer(LCount, 4);
    for I := 0 to 3 do Require(FBlocks[I].SaveBlockToStream(False, LPayload), 'Cannot save response block');
    FExpectedBatchBytes := LPayload.Size;
    FConnection.DoSend(ntp_response, CT_NetOp_GetBlocks, 0, LRequest, LPayload);
    CaptureFrame(ntp_response, CT_NetOp_GetBlocks, LRequest, AFirst);
    LPayload.Clear;
    Require(FBlocks[4].SaveBlockToStream(FFast, LPayload), 'Cannot save NewBlock');
    TStreamOp.WriteAnsiString(LPayload, FPeerSafeBox.AggregatedHashrate.RawValue);
    LOpcode:=CT_NetOp_NewBlock;
    if FFast then begin
      LOpcode:=CT_NetOp_NewBlock_Fast_Propagation;
      LCount:=0; if FMissing then LCount:=1;
      LPayload.WriteBuffer(LCount,SizeOf(LCount));
      if FMissing then begin
        LReference:=High(UInt64);
        LPayload.WriteBuffer(LReference,SizeOf(LReference));
      end;
    end;
    if FMalformed then begin
      LPayload.Clear;
      LCount:=0;
      LPayload.WriteBuffer(LCount,2);
    end;
    FConnection.DoSend(ntp_autosend, LOpcode, 0, 0, LPayload);
    CaptureFrame(ntp_autosend, LOpcode, 0, ASecond);
    if FBadProtocol then begin
      LBadVersion:=CT_NetProtocol_Available+1;
      ASecond.Position:=14; ASecond.WriteBuffer(LBadVersion,SizeOf(LBadVersion));
      ASecond.Position:=ASecond.Size;
    end;
    if FMalformed or FBadProtocol then begin
      // A valid complete block after rejected data must not bypass the normal
      // session invalidation through a disconnected second acceptance path.
      LPayload.Clear; Require(FBlocks[4].SaveBlockToStream(False,LPayload),'Cannot save trailing valid block');
      TStreamOp.WriteAnsiString(LPayload,FPeerSafeBox.AggregatedHashrate.RawValue);
      FConnection.DoSend(ntp_autosend,CT_NetOp_NewBlock,0,0,LPayload);
      CaptureFrame(ntp_autosend,CT_NetOp_NewBlock,0,ASecond);
    end;
    // A complete non-block control frame and an incomplete suffix must not
    // become fresh protocol work when the disconnected owner drains blocks.
    FConnection.DoSend(ntp_request, CT_NetOp_Reserved_Start, 0, 123, nil);
    CaptureFrame(ntp_request, CT_NetOp_Reserved_Start, 123, LControl);
    ASecond.CopyFrom(LControl, 0);
    LStart := CT_MagicNetIdentification;
    ASecond.WriteBuffer(LStart, 3);
  finally LControl.Free; LRequestWire.Free; LPayload.Free; end;
end;

procedure TFrameCase.SendWire(AWire: TMemoryStream);
begin
  Require(FPeer.SendBuffer(AWire.Memory, AWire.Size) = AWire.Size,
    'Raw peer did not send the complete production frame');
end;

function TFrameCase.BufferedBytes: Int64;
var LBuffer: TMemoryStream;
begin
  LBuffer := FTransport.ReadBufferLock;
  try Result := LBuffer.Size; finally FTransport.ReadBufferUnlock; end;
end;

procedure TFrameCase.CheckSecondReceived(ABytes: Int64; AParserSuffix: Boolean);
var LUntil: UInt64; LHeader: TNetHeaderData; LMore: Boolean; LSavedPosition: Int64;
  LCopy, LPayload, LBuffer: TMemoryStream;
begin
  LUntil := GetTickCount64 + 3000;
  repeat
    if FTransport.BytesReceived >= ABytes then Break;
    Pump;
  until GetTickCount64 >= LUntil;
  Require(FTransport.BytesReceived >= ABytes, 'Complete second wire frame was not received locally');
  if AParserSuffix then begin
    Require(BufferedBytes = 0, 'Coalesced frames were not transferred into the parser suffix');
    Log('RECEIVED complete NEW_BLOCK4 retained in parser suffix');
  end else begin
    LCopy := TMemoryStream.Create; LPayload := TMemoryStream.Create;
    try
      LBuffer := FTransport.ReadBufferLock;
      try
        LSavedPosition := LBuffer.Position;
        try LCopy.CopyFrom(LBuffer, 0); finally LBuffer.Position := LSavedPosition; end;
      finally FTransport.ReadBufferUnlock; end;
      LCopy.Position := 0;
      Require(TNetData.ExtractHeaderInfo(LCopy, LHeader, LPayload, LMore) and
        (LHeader.operation in [CT_NetOp_NewBlock,CT_NetOp_NewBlock_Fast_Propagation]) and
        (LHeader.header_type = ntp_autosend),
        'The retained transport bytes do not contain the complete NewBlock frame');
      Log('RECEIVED complete announcement frame retained in transport buffer');
    finally LPayload.Free; LCopy.Free; end;
  end;
end;

procedure TFrameCase.CheckStoredBlocks;
var LBank: TPCBank; LStorage: TFileStorage; LBlock: TPCOperationsComp;
  LExpected, LActual: TMemoryStream; I: Integer;
begin
  LBank := TPCBank.Create(nil);
  LBlock := TPCOperationsComp.Create(nil);
  LExpected := TMemoryStream.Create; LActual := TMemoryStream.Create;
  try
    LBank.StorageClass := TFileStorage;
    LStorage := TFileStorage(LBank.Storage);
    LStorage.ReadOnly := True;
    LStorage.SetBlockChainFile(AnsiString(FDirectory + PathDelim + 'Data' +
      PathDelim + 'BlockChainStream.blocks'));
    Require(LStorage.Initialize, 'Cannot reopen the actual stored blocks read-only');
    Require((LStorage.FirstBlock=0) and (LStorage.LastBlock=FExpected-1),
      'Stored terminal height includes missing or extra atomic blocks');
    for I := 0 to FExpected-1 do begin
      Require(LStorage.LoadBlockChainBlock(LBlock, I), 'A committed buffered block was not stored');
      LExpected.Clear; LActual.Clear;
      Require(FBlocks[I].SaveBlockToStream(False, LExpected), 'Cannot serialize the expected historic block');
      Require(LBlock.SaveBlockToStream(False, LActual), 'Cannot serialize the reopened block');
      Require((LExpected.Size=LActual.Size) and
        CompareMem(LExpected.Memory,LActual.Memory,LExpected.Size),
        'A reopened buffered block differs from its exact original network bytes');
    end;
  finally LActual.Free; LExpected.Free; LBlock.Free; LBank.Free; end;
  Log('REOPENED exact stored blocks=' + IntToStr(FExpected));
end;

procedure TFrameCase.CheckBlockLog;
var LLines, LUpdates, LColumns: TStringList; LLine, LPrefix, LMessage: String;
  LStream: TFileStream;
  I, LMarker, LBatchCount: Integer; LAcceptedAt, LAge: Int64;
begin
  LLines := TStringList.Create;
  LUpdates := TStringList.Create;
  LColumns := TStringList.Create;
  try
    // The logger deliberately retains its writable stream. Read with shared
    // access rather than TStringList.LoadFromFile's writer-denying default.
    LStream := TFileStream.Create(FDirectory + PathDelim + 'blocks.log',
      fmOpenRead or fmShareDenyNone);
    try LLines.LoadFromStream(LStream); finally LStream.Free; end;
    LBatchCount := 0;
    for I := 0 to LLines.Count-1 do begin
      if Pos('Block batch received:',LLines[I])>0 then begin
        Inc(LBatchCount);
        Require(LUpdates.Count=0,'Block receipt was logged after acceptance');
        LPrefix := Format('[Info] <%s> Block batch received: Count:4 First:0 Last:3 PayloadBytes:%d Peer:%s',[
          FExpectedLogSender,FExpectedBatchBytes,FExpectedLogRemote]);
        Require(Pos(LPrefix,LLines[I])>0,'Block receipt lost its count/range/payload/peer facts');
      end;
      if Pos(' [Update] ',LLines[I])>0 then LUpdates.Add(LLines[I]);
    end;
    Require(LBatchCount=1,'Block receipt log was omitted or duplicated');
    Require(LUpdates.Count=FExpected,
      'Block audit log omitted or duplicated an accepted batch/announced block');
    LColumns.Delimiter := ';';
    LColumns.StrictDelimiter := True;
    for I := 0 to FExpected-1 do begin
      LLine := LUpdates[I];
      Require(Pos('[Update] <' + FExpectedLogSender + '> ',LLine)>0,
        'Block audit log changed the original source class');
      LMarker := Pos('> ',LLine);
      LMessage := Copy(LLine,LMarker+2,MaxInt);
      LPrefix := Format(';%d;%s;%s;;%d;',[FBlocks[I].OperationBlock.block,
        FExpectedLogRemote,FBlocks[I].OperationBlock.block_payload.ToPrintable,
        FBlocks[I].OperationBlock.timestamp]);
      Require(Copy(LMessage,1,Length(LPrefix))=LPrefix,
        'Block audit log lost the retained header or changed original columns');
      LColumns.DelimitedText := Copy(LMessage,Length(LPrefix)+1,MaxInt);
      Require((LColumns.Count=3) and TryStrToInt64(LColumns[0],LAcceptedAt) and
        TryStrToInt64(LColumns[1],LAge),'Block audit log has invalid time columns');
      Require(LAge= LAcceptedAt-Int64(FBlocks[I].OperationBlock.timestamp),
        'Block audit log changed its age-at-acceptance column');
      Require(LColumns[2]=IntToHex(FBlocks[I].OperationBlock.compact_target,8),
        'Block audit log changed the original target column');
    end;
  finally LColumns.Free; LUpdates.Free; LLines.Free; end;
  Log('BLOCK LOG exact accepted entries=' + IntToStr(FExpected));
end;

procedure TFrameCase.Run(AScenario: TBufferedFrameScenario);
var LFirst, LSecond, LCombined: TMemoryStream; LIncoming, LParser, LEOF, LShutdown: Boolean;
  LUntil: UInt64; LRequestBefore: Cardinal; LStatus: TNodeSynchronizationStatus;
  LStoredChecked: Boolean;
begin
  LIncoming := AScenario in [bfsIncomingTransportOff, bfsIncomingParserOff,
    bfsIncomingEOF, bfsIncomingShutdown,bfsIncomingFastMissing,bfsIncomingBadProtocol];
  LParser := AScenario in [bfsOutgoingParserOff, bfsIncomingParserOff,bfsOutgoingFirstBufferedOff];
  LEOF := AScenario in [bfsOutgoingEOF, bfsIncomingEOF];
  LShutdown := AScenario in [bfsOutgoingShutdown, bfsIncomingShutdown];
  LStoredChecked := False;
  FExpected := 5; if LShutdown then FExpected := 1;
  FFast:=AScenario in [bfsOutgoingFastComplete,bfsIncomingFastMissing];
  FMissing:=AScenario=bfsIncomingFastMissing;
  FMalformed:=AScenario=bfsOutgoingMalformed;
  FBadProtocol:=AScenario=bfsIncomingBadProtocol;
  FMatched:=AScenario=bfsOutgoingMatchedOff;
  if FMissing or FMalformed or FBadProtocol then FExpected:=4;
  Open(LIncoming); LoadBlocks;
  LFirst := TMemoryStream.Create; LSecond := TMemoryStream.Create; LCombined := TMemoryStream.Create;
  try
    BuildFrames(LFirst, LSecond);
    if FMatched then begin
      FMatchedTask:=TMatchedResponseTask.Create(Self);
      FMatchedTask.Start; FMatchedTask.FStarted:=True;
      // The response is reflected only after this real outstanding request
      // crossed the socket. This exercises matching, not a second response.
      CaptureFrame(ntp_request,CT_NetOp_GetBlocks,FRequestID,LCombined);
      LCombined.Clear;
    end;
    if LParser then begin
      Require(FNet.ConnectionLock(Self, FConnection, 1000), 'Cannot gate the real parser before receipt');
      try
        LCombined.CopyFrom(LFirst, 0); LCombined.CopyFrom(LSecond, 0);
        SendWire(LCombined);
        LUntil := GetTickCount64 + 3000;
        repeat Pump; until (BufferedBytes = LCombined.Size) or (GetTickCount64 >= LUntil);
        Require(BufferedBytes = LCombined.Size, 'Coalesced frames not fully received before parser release');
        if AScenario=bfsOutgoingFirstBufferedOff then begin
          FOff:=TNetworkOff.Create(Self); FOff.Start; FOff.FStarted:=True;
          LUntil:=GetTickCount64+1000;
          repeat Pump; until not FNet.NetConnectionsActive or (GetTickCount64>=LUntil);
          Require(not FNet.NetConnectionsActive,'First-buffer-only disconnect was not published');
        end;
      finally FNet.ConnectionUnlock(FConnection); end;
    end else SendWire(LFirst);
    Await(FFirstCommit, 5000, 'Actual owner did not commit the first response block');
    if not LParser then SendWire(LSecond);
    CheckSecondReceived(LFirst.Size + LSecond.Size, LParser);
    FObserving := True;
    LRequestBefore := FNet.NewRequestId;
    if LShutdown then FNet.BeginShutdown;
    if LEOF then FPeer.CloseSocket
    else if not Assigned(FOff) then begin
      FOff := TNetworkOff.Create(Self); FOff.Start; FOff.FStarted := True;
      LUntil := GetTickCount64 + 1000;
      repeat Pump; until not FNet.NetConnectionsActive or (GetTickCount64 >= LUntil);
      Require(not FNet.NetConnectionsActive, 'OFF was not published before draining');
    end;
    FRelease.SetEvent;
    if Assigned(FOff) then begin
      Await(FOff.Done, 5000, 'Network owner did not finish OFF');
      Require(FOff.Error = '', FOff.Error);
    end;
    if Assigned(FMatchedTask) then begin
      Await(FMatchedTask.Done,5000,'The real matching-response owner did not finish');
      Require(FMatchedTask.Error='',FMatchedTask.Error);
    end;
    if LEOF then begin
      LUntil := GetTickCount64+5000;
      repeat
        Pump;
        LStatus := FNode.GetSynchronizationStatus;
        if (FNode.Bank.BlocksCount=Cardinal(FExpected)) and
          (LStatus.ActiveTransfers=0) and not FNode.IsUpdatingBlockChain then Break;
      until GetTickCount64>=LUntil;
      Require((FNode.Bank.BlocksCount=Cardinal(FExpected)) and
        (FCommits=FExpected) and (LStatus.ActiveTransfers=0) and not FNode.IsUpdatingBlockChain,
        'EOF did not finish the exact received blocks before cleanup');
      CheckStoredBlocks;
      LStoredChecked := True;
      // Passive outgoing cleanup has an age grace and can wait for discovery.
      // AFTER proving the EOF drain, ask the existing owner to close normally;
      // never dereference the now-disconnected borrowed connection pointer.
      FOff:=TNetworkOff.Create(Self); FOff.Start; FOff.FStarted:=True;
      Await(FOff.Done,5000,'EOF cleanup owner did not finish OFF');
      Require(FOff.Error='',FOff.Error);
    end;
    LUntil := GetTickCount64 + 5000;
    repeat
      Pump;
      LStatus := FNode.GetSynchronizationStatus;
      if not FNode.IsUpdatingBlockChain and (InterlockedCompareExchange(FDestroyed, 0, 0) <> 0) then Break;
    until GetTickCount64 >= LUntil;
    Log(Format('RESULT case=%d committed=%d blocks=%d destroyed=%d active=%d reserved=%d',
      [Ord(AScenario), FCommits, FNode.Bank.BlocksCount, FDestroyed,
       LStatus.ActiveTransfers, FReservedCalls]));
    Require(FNode.Bank.BlocksCount = Cardinal(FExpected),
      'The real owner discarded a complete already-received block frame');
    Require(FCommits = FExpected, 'Unexpected atomic block commit count');
    Require(not FIdleBeforeDrain, 'Node became available between retained block frames');
    Require(FReservedCalls = 0, 'Disconnected drain dispatched a non-block control callback');
    Require(FNet.NewRequestId = LRequestBefore + 1, 'Disconnected drain allocated a new request');
    Require(LStatus.ActiveTransfers = 0, 'Buffered physical scope did not retire');
    Require(not FNode.IsUpdatingBlockChain,'The node still owns a physical block scope after retirement');
    Require(FDestroyed <> 0, 'The completed real transport owner did not retire');
    Require((FNet.NetStatistics.ActiveConnections=0) and
      (FNet.NetStatistics.ServersConnections=0) and
      (FNet.NetStatistics.ServersConnectionsWithResponse=0),
      'Buffered disconnect resurrected or unbalanced live-peer statistics');
    if not LStoredChecked then CheckStoredBlocks;
    CheckBlockLog;
    Log('PASS case=' + IntToStr(Ord(AScenario)));
  finally LCombined.Free; LSecond.Free; LFirst.Free; end;
end;

procedure RunBufferedFrameScenario(AScenario: TBufferedFrameScenario);
var LCase: TFrameCase;
begin
  if AScenario=bfsReceivePublication then begin
    CheckReceivePublication;
    Exit;
  end;
  LCase := TFrameCase.Create;
  try LCase.Run(AScenario); finally LCase.Free; end;
end;

function RunBufferedBlockFrameChecks(const ALogFile: String): Integer;
var LScenario: TBufferedFrameScenario; LFailed: Boolean;
begin
  Result := 1;
  Require(not FileExists(ALogFile), 'Refusing an existing frame probe log');
  GLogFile := ALogFile; GLog := TStringList.Create;
  try
    LFailed := False;
    for LScenario := Low(TBufferedFrameScenario) to High(TBufferedFrameScenario) do
      try RunBufferedFrameScenario(LScenario);
      except on E: Exception do begin
        LFailed := True;
        Log('FAIL case=' + IntToStr(Ord(LScenario)) + ' ' + E.ClassName + ': ' + E.Message);
      end; end;
    if not LFailed then Result := 0;
  finally GLog.Free; GLog := nil; end;
end;

end.
