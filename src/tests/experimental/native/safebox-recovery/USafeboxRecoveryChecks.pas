unit USafeboxRecoveryChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Real loopback transports exercise interrupted safebox requests, endpoint
  discovery eligibility, request ownership and download-worker handoff. Synthetic
  metadata reaches the transport only; it never represents a valid checkpoint. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

function RunSafeboxRecoveryChecks(const ALogFile: String): Integer;

implementation

uses Windows, Classes, SysUtils, SyncObjs, Generics.Collections, blcksock,
  {$IFDEF FPC}{$IFDEF VER3_2}zStream,{$ELSE}paszlib_zStream,{$ENDIF}{$ELSE}ZLib,{$ENDIF}
  UNode, UNetProtocol, UTCPIP, UThread, UBaseTypes, UConst, UAccounts, UBlockChain, UFileStorage,
  UCrypto, UChunk, UPCDownloadSafebox, UPCTemporalFileStream, UPCDataTypes, ULog;

const
  CT_Deadline = 5000;
  CT_Sentinel: AnsiString = 'Existing completed archive sentinel: do not replace';

type
  TRecoveryCase = class;

  TRecoveryState = class
  public
    FirstBody, ReleaseBody, SecondBody: TEvent;
    WorkerReady, WriteEntered, ReleaseWrite: TEvent;
    Calls, Factories, ChunkEntries, ChunkExits, ChunkJoins: LongInt;
    constructor Create;
    destructor Destroy; override;
  end;

  THandoffWorker = class(TThreadGetNewBlockChainFromClient)
  protected
    function SelectConnection(const AConnections: TList<TNetConnection>): TNetConnection; override;
  end;

  TRecoveryNet = class(TNetData)
  private
    FState: TRecoveryState;
  protected
    function CreateBlockChainWorker: TThreadGetNewBlockChainFromClient; override;
  public
    constructor Create(AState: TRecoveryState); reintroduce;
    procedure RequestDownload;
    procedure GetNewBlockChainFromClient(Connection: TNetConnection; const why: String); override;
  end;

  TLateResponseTask = class(TPCThread)
  private
    FCase: TRecoveryCase;
  protected
    procedure BCExecute; override;
  public
    Done: TEvent;
    FirstID, SecondID: Cardinal;
    FirstResult, SecondResult: Boolean;
    Error: String;
    constructor Create(ACase: TRecoveryCase);
    destructor Destroy; override;
  end;

  TDownloadAccess = class(TPCDownloadSafebox)
  public type
    TChunkThread = {$IFDEF FPC}TDownloadSBThread{$ELSE}TPCDownloadSafebox.TDownloadSBThread{$ENDIF};
  end;

  TGateChunkStream = class(TPCTemporalFileStream)
  private
    FState: TRecoveryState;
    FEntered: LongInt;
  public
    constructor Create(AState: TRecoveryState); reintroduce;
    function Write(const Buffer; Count: LongInt): LongInt; override;
  end;

  TObservedChunk = class(TDownloadAccess.TChunkThread)
  private
    FState: TRecoveryState;
  protected
    procedure BCExecute; override;
  public
    constructor Create(AOwner: TPCDownloadSafebox; AStart, ACount: Cardinal;
      AState: TRecoveryState); reintroduce;
  end;

  TObservedDownload = class(TPCDownloadSafebox)
  private
    FState: TRecoveryState;
  protected
    function CreateChunkThread(ABlockStart, ACount: Cardinal): TDownloadAccess.TChunkThread; override;
    procedure WaitForChunkThread(AThread: TThread); override;
  public
    constructor Create(AState: TRecoveryState); reintroduce;
  end;

  TDownloadTask = class(TPCThread)
  private
    FCase: TRecoveryCase;
    FGateValidation: Boolean;
  protected
    procedure BCExecute; override;
  public
    Done: TEvent;
    DownloadResult: Boolean;
    ChunksPublished: Integer;
    Error: String;
    constructor Create(ACase: TRecoveryCase; AGateValidation: Boolean);
    destructor Destroy; override;
  end;

  TRecoveryCase = class(TComponent)
  private
    FState: TRecoveryState;
    FNet: TRecoveryNet;
    FNode: TNode;
    FListener, FPeer: TTCPBlockSocket;
    FOld, FReplacement: TNetConnection;
    FLeases: TList<TNetConnection>;
    FWire: TMemoryStream;
    FOldDirectory, FDirectory: String;
    FPort: Word;
    FOldGone, FReplacementGone: LongInt;
    FLate: TLateResponseTask;
    FDownload: TDownloadTask;
    FCoreLog: TLog;
    FProtocolRejections, FPacketRejections, FChunkRejections: LongInt;
    procedure CoreLog(AType: TLogType; ATime: TDateTime; AThread: TThreadID;
      const ASender, AText: String);
    procedure PinActiveConnections;
    procedure ReleasePins;
    procedure AcceptConnection(AReplacement: Boolean);
    procedure AcceptDiscoveredReplacement;
    procedure DropPeer;
    procedure AwaitDisconnected(AConnection: TNetConnection);
    procedure AwaitEndpointDetached(var AAddress: TNodeServerAddress);
    function AwaitRequest(ATimeout: Cardinal; AOperation: Word = CT_NetOp_GetSafeBox;
      ARequired: Boolean = True): Cardinal;
    procedure SendFrame(AType, AOperation: Word; ARequest: Cardinal;
      AError: Word; APayload: TMemoryStream);
    procedure SendReply(ARequest: Cardinal; AError: Word; APayload: TMemoryStream);
    procedure CheckCancelledFiles;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(const ABaseDirectory, AName: String);
    destructor Destroy; override;
    procedure CheckLateResponse;
    procedure CheckEndpointReplacement;
    procedure CheckDownloaderRetry;
    procedure CheckChunkRequestLease;
    procedure CheckBusyHandoff;
    procedure CheckEarlyPendingExchange;
    procedure CheckInvalidPacketType;
    procedure CheckMatchingInvalidChunk;
  end;

var
  GLog: TStringList;
  GLogFile: String;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

procedure Log(const AMessage: String);
begin
  GLog.Add(AMessage);
  GLog.SaveToFile(GLogFile);
end;

procedure Pump;
begin
  CheckSynchronize(0);
  Sleep(1);
end;

procedure Await(AEvent: TEvent; AMilliseconds: Cardinal; const AMessage: String);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + AMilliseconds;
  repeat
    if AEvent.WaitFor(0) = wrSignaled then Exit;
    Pump;
  until GetTickCount64 >= LUntil;
  raise Exception.Create(AMessage);
end;

function DesktopName(AHandle: THandle): String;
var LBuffer: array[0..255] of Char; LRequired: DWORD;
begin
  FillChar(LBuffer, SizeOf(LBuffer), 0);
  if not GetUserObjectInformation(AHandle, UOI_NAME, @LBuffer[0], SizeOf(LBuffer), LRequired) then
    RaiseLastOSError;
  Result := StrPas(PChar(@LBuffer[0]));
end;

procedure RequirePrivateDesktop;
var LInput: THandle; LName: String;
begin
  LInput := OpenInputDesktop(0, False, DESKTOP_READOBJECTS);
  if LInput = 0 then RaiseLastOSError;
  try
    LName := DesktopName(GetThreadDesktop(GetCurrentThreadId));
    Require((Pos('PascalCoin', LName) = 1) and not SameText(LName, DesktopName(LInput)),
      'Run through the private PascalCoin desktop broker');
  finally
    CloseDesktop(LInput);
  end;
end;

constructor TRecoveryState.Create;
begin
  inherited Create;
  FirstBody := TEvent.Create(nil, True, False, '');
  ReleaseBody := TEvent.Create(nil, True, False, '');
  SecondBody := TEvent.Create(nil, True, False, '');
  WorkerReady := TEvent.Create(nil, True, False, '');
  WriteEntered := TEvent.Create(nil, True, False, '');
  ReleaseWrite := TEvent.Create(nil, True, False, '');
end;

destructor TRecoveryState.Destroy;
begin
  ReleaseWrite.Free;
  WriteEntered.Free;
  WorkerReady.Free;
  SecondBody.Free;
  ReleaseBody.Free;
  FirstBody.Free;
  inherited Destroy;
end;

function THandoffWorker.SelectConnection(const AConnections: TList<TNetConnection>): TNetConnection;
begin
  // Candidate choice is deterministic; snapshot acquisition/leases stay real.
  if AConnections.Count > 0 then Result := AConnections[0]
  else Result := nil;
end;

constructor TRecoveryNet.Create(AState: TRecoveryState);
begin
  FState := AState;
  inherited Create(nil);
end;

function TRecoveryNet.CreateBlockChainWorker: TThreadGetNewBlockChainFromClient;
begin
  InterlockedIncrement(FState.Factories);
  Result := THandoffWorker.Create(Self);
end;

procedure TRecoveryNet.RequestDownload;
begin
  StartBlockChainWorker;
end;

procedure TRecoveryNet.GetNewBlockChainFromClient(Connection: TNetConnection; const why: String);
var LCall: LongInt;
begin
  // Only the expensive download body is a gate. Production worker admission,
  // completion publication and its entire connection lease remain unchanged.
  LCall := InterlockedIncrement(FState.Calls);
  if LCall = 1 then begin
    FState.FirstBody.SetEvent;
    Require(FState.ReleaseBody.WaitFor(15000) = wrSignaled, 'Handoff body gate expired');
  end else FState.SecondBody.SetEvent;
end;

constructor TLateResponseTask.Create(ACase: TRecoveryCase);
begin
  FCase := ACase;
  Done := TEvent.Create(nil, True, False, '');
  FirstID := ACase.FNet.NewRequestId;
  SecondID := ACase.FNet.NewRequestId;
  inherited Create(True);
  FreeOnTerminate := False;
end;

destructor TLateResponseTask.Destroy;
begin
  WaitFor;
  Done.Free;
  inherited Destroy;
end;

procedure TLateResponseTask.BCExecute;
var LSend, LReceive: TMemoryStream; LHeader: TNetHeaderData; LValue: Cardinal;
begin
  LSend := TMemoryStream.Create;
  LReceive := TMemoryStream.Create;
  try
    try
      Require(FCase.FNet.ConnectionLock(Self, FCase.FOld, 1000), 'Cannot lock request owner');
      try
        // A timeout leaves an old response possible while the next request waits.
        FirstResult := FCase.FOld.DoSendAndWaitForResponse(CT_NetOp_GetSafeBox,
          FirstID, LSend, LReceive, 200, LHeader);
        SecondResult := FCase.FOld.DoSendAndWaitForResponse(CT_NetOp_GetSafeBox,
          SecondID, LSend, LReceive, 3000, LHeader);
        if SecondResult then begin
          Require((LHeader.request_id = SecondID) and (not LHeader.is_error),
            'Old response was mistaken for the current response');
          LReceive.Position := 0;
          LReceive.ReadBuffer(LValue, SizeOf(LValue));
          Require(LValue = $C0FFEE, 'Matching response payload was not preserved');
        end;
      finally
        FCase.FNet.ConnectionUnlock(FCase.FOld);
      end;
    except
      on E: Exception do Error := E.ClassName + ': ' + E.Message;
    end;
  finally
    LReceive.Free;
    LSend.Free;
    Done.SetEvent;
  end;
end;

constructor TGateChunkStream.Create(AState: TRecoveryState);
begin
  FState := AState;
  inherited Create('RECOVERY_VALIDATION_');
end;

function TGateChunkStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  // Gate a real decompression write, after matching-response parsing but before
  // invalid safebox-header rejection dereferences the same peer again.
  if (Count > 0) and (InterlockedCompareExchange(FEntered, 1, 0) = 0) then begin
    FState.WriteEntered.SetEvent;
    Require(FState.ReleaseWrite.WaitFor(15000) = wrSignaled, 'Chunk write gate expired');
  end;
  Result := inherited Write(Buffer, Count);
end;

constructor TObservedChunk.Create(AOwner: TPCDownloadSafebox; AStart, ACount: Cardinal;
  AState: TRecoveryState);
begin
  FState := AState;
  inherited Create(AOwner, AStart, ACount);
end;

procedure TObservedChunk.BCExecute;
begin
  Require(FState.WorkerReady.WaitFor(15000) = wrSignaled, 'Chunk stream installation gate expired');
  // A failed factory still owns the downloader's chunk-list lock. Its
  // unpublished worker must exit before inherited retry tries that same lock.
  if Terminated then Exit;
  InterlockedIncrement(FState.ChunkEntries);
  try
    inherited BCExecute;
  finally
    InterlockedIncrement(FState.ChunkExits);
  end;
end;

constructor TObservedDownload.Create(AState: TRecoveryState);
begin
  FState := AState;
  inherited Create;
end;

function TObservedDownload.CreateChunkThread(ABlockStart, ACount: Cardinal): TDownloadAccess.TChunkThread;
var LOriginal: TPCTemporalFileStream; LWorker: TObservedChunk;
begin
  LWorker := TObservedChunk.Create(Self, ABlockStart, ACount, FState);
  try
    // The real worker is suspended at its construction gate until its owned
    // destination is installed; its selection and retry loop are inherited.
    LOriginal := LWorker.Stream;
    LWorker.Stream := TGateChunkStream.Create(FState);
    LOriginal.Free;
    FState.WorkerReady.SetEvent;
    Result := LWorker;
  except
    // A factory failure has not transferred ownership to the downloader. Join
    // this unpublished worker before its borrowed downloader can be destroyed.
    LWorker.Terminate;
    FState.WorkerReady.SetEvent;
    LWorker.WaitFor;
    LWorker.Free;
    raise;
  end;
end;

procedure TObservedDownload.WaitForChunkThread(AThread: TThread);
begin
  inherited WaitForChunkThread(AThread);
  InterlockedIncrement(FState.ChunkJoins);
end;

constructor TDownloadTask.Create(ACase: TRecoveryCase; AGateValidation: Boolean);
begin
  FCase := ACase;
  FGateValidation := AGateValidation;
  Done := TEvent.Create(nil, True, False, '');
  inherited Create(True);
  FreeOnTerminate := False;
end;

destructor TDownloadTask.Destroy;
begin
  WaitFor;
  Done.Free;
  inherited Destroy;
end;

procedure TDownloadTask.BCExecute;
var LDownload: TPCDownloadSafebox; LChunks: TPCSafeboxChunks;
  LOperation: TOperationBlock;
begin
  LDownload := nil;
  LChunks := TPCSafeboxChunks.Create;
  try
    try
      FCase.FNode.BeginBlockChainUpdate;
      try
        if FGateValidation then LDownload := TObservedDownload.Create(FCase.FState)
        else LDownload := TPCDownloadSafebox.Create;
        try
          LOperation := CT_OperationBlock_NUL;
          LOperation.block := CT_BankToDiskEveryNBlocks;
          // This satisfies chunk partitioning only. No synthetic archive is
          // accepted; all requests are interrupted before publication.
          DownloadResult := LDownload.DownloadSafebox(Self, LOperation, LChunks);
          ChunksPublished := LChunks.Count;
        finally
          LDownload.Free;
        end;
      finally
        FCase.FNode.EndBlockChainUpdate;
      end;
    except
      on E: Exception do Error := E.ClassName + ': ' + E.Message;
    end;
  finally
    LChunks.Free;
    Done.SetEvent;
  end;
end;

constructor TRecoveryCase.Create(const ABaseDirectory, AName: String);
var LID: TGUID; LFixed: TNodeServerAddressArray; LSaved: TFileStream;
  LAddress: TNodeServerAddress;
begin
  inherited Create(nil);
  Require(not TNode.NodeExists and not TNetData.NetDataExists,
    'Each recovery case must own fresh node/network singletons');
  FOldDirectory := TNode.GetPascalCoinDataFolder;
  CreateGUID(LID);
  FDirectory := IncludeTrailingPathDelimiter(ABaseDirectory) + AName + GUIDToString(LID);
  Require(not DirectoryExists(FDirectory), 'Refusing an existing data directory');
  ForceDirectories(FDirectory);
  TNode.SetPascalCoinDataFolder(FDirectory);
  FState := TRecoveryState.Create;
  FLeases := TList<TNetConnection>.Create;
  FWire := TMemoryStream.Create;
  // Install the observer before any network owner starts and retain it until
  // all owners join. Its callback publishes counters without entering UI code.
  FCoreLog := TLog.Create(nil);
  FCoreLog.OnInThreadNewLog := CoreLog;
  FCoreLog.FileName := FDirectory + PathDelim + 'core.log';
  TCrypto.InitCrypto;
  FNet := TRecoveryNet.Create(FState);
  // Network activation setters ask for TNode.Node. Publish the owned node
  // before invoking them, so setup cannot create a second node implicitly.
  FNode := TNode.Create(nil);
  FNode.Bank.StorageClass := TFileStorage;
  Log('SETUP owned node and network created');
  FNet.NetConnectionsActive := False;
  FNet.MinServersConnected := 0;
  FNet.MaxServersConnected := 0;
  FNode.BroadcastData := False;
  FNode.UpdateBlockchain := False;
  FNode.NodeLogFilename := FDirectory + PathDelim + 'blocks.log';
  FNode.NetServer.Port := 0;
  FListener := TTCPBlockSocket.Create;
  FListener.CreateSocket;
  FListener.Bind('127.0.0.1', '0');
  Require(FListener.LastError = 0, 'Cannot bind an owned loopback peer');
  FListener.Listen;
  Require(FListener.LastError = 0, 'Cannot listen on an owned loopback peer');
  FPort := FListener.GetLocalSinPort;
  // Reserve only this owned initial endpoint before activation's immediate
  // discovery pass. ConnectTo then executes the normal production mapping and
  // socket path. There is no unrelated placeholder discovery completion that
  // could spuriously provide the handoff test's second blockchain start.
  FOld := TNetClient.Create(nil);
  FOld.FreeNotification(Self);
  Require(FNet.NodeServersAddresses.GetNodeServerAddress('127.0.0.1', FPort, True, LAddress),
    'Cannot reserve the owned initial connection endpoint');
  LAddress.netConnection := FOld;
  FNet.NodeServersAddresses.SetNodeServerAddress(LAddress);
  TNode.DecodeIpStringToNodeServerAddressArray('127.0.0.1:' + IntToStr(FPort), LFixed);
  FNet.DiscoverFixedServersOnly(LFixed);
  FNet.NetConnectionsActive := True;
  AcceptConnection(False);
  Require(not FNet.IsDiscoveringServers and (FState.Factories = 0),
    'Initial fixture setup admitted an unrelated discovery/blockchain worker');
  // Retain normal automatic backoff until the retry case deliberately ages
  // this owned address. EOF cannot race an automatic reconnect ahead of that
  // case's endpoint-detachment observation or the negative rejection checks.
  Require(FNet.NodeServersAddresses.GetNodeServerAddress('127.0.0.1', FPort, False, LAddress),
    'Owned initial endpoint disappeared after connecting');
  LAddress.last_attempt_to_connect := Now;
  FNet.NodeServersAddresses.SetNodeServerAddress(LAddress);
  Log('SETUP leased outgoing loopback connection accepted');
  LSaved := TFileStream.Create(FDirectory + PathDelim + 'completed.safebox', fmCreate);
  try
    LSaved.WriteBuffer(CT_Sentinel[1], Length(CT_Sentinel));
    Require(FlushFileBuffers(LSaved.Handle), 'Cannot flush saved-file sentinel');
  finally
    LSaved.Free;
  end;
end;

destructor TRecoveryCase.Destroy;
begin
  // Release every test gate before joining. Cancellation never kills a thread
  // or frees a destination while inherited retry/validation still uses it.
  if Assigned(FState) then begin
    FState.ReleaseBody.SetEvent;
    FState.WorkerReady.SetEvent;
    FState.ReleaseWrite.SetEvent;
  end;
  if Assigned(FDownload) then FDownload.Terminate;
  if Assigned(FLate) then FLate.Terminate;
  if Assigned(FPeer) then FPeer.CloseSocket;
  FDownload.Free;
  FLate.Free;
  if Assigned(FNet) then FNet.BeginShutdown;
  ReleasePins;
  if Assigned(FNet) then begin
    FNet.NetConnectionsActive := False;
    FNet.Free;
  end;
  FNode.Free;
  FCoreLog.Free;
  FPeer.Free;
  FListener.Free;
  FWire.Free;
  FLeases.Free;
  FState.Free;
  if FOldDirectory <> '' then TNode.SetPascalCoinDataFolder(FOldDirectory);
  inherited Destroy;
end;

procedure TRecoveryCase.CoreLog(AType: TLogType; ATime: TDateTime; AThread: TThreadID;
  const ASender, AText: String);
begin
  if (AType <> ltError) or (ASender <> 'TNetClient') or
    (Pos('Disconecting ', AText) <> 1) then Exit;
  InterlockedIncrement(FProtocolRejections);
  if (Pos(' > Received ', AText) > 0) and (Pos('GET_SAFEBOX', AText) > 0) then
    InterlockedIncrement(FPacketRejections);
  if Pos('Invalid received chunk:', AText) > 0 then
    InterlockedIncrement(FChunkRejections);
end;

procedure TRecoveryCase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = FOld then InterlockedExchange(FOldGone, 1);
    if AComponent = FReplacement then InterlockedExchange(FReplacementGone, 1);
  end;
end;

procedure TRecoveryCase.PinActiveConnections;
var LNew: TList<TNetConnection>; LConnection: TNetConnection;
begin
  LNew := TList<TNetConnection>.Create;
  try
    FNet.AcquireDownloadConnections(LNew);
    for LConnection in LNew do FLeases.Add(LConnection);
  finally
    LNew.Free;
  end;
end;

procedure TRecoveryCase.ReleasePins;
var I: Integer;
begin
  if not Assigned(FLeases) then Exit;
  for I := FLeases.Count - 1 downto 0 do
    FNet.ReleaseDownloadConnection(FLeases[I]);
  FLeases.Clear;
end;

procedure TRecoveryCase.AcceptConnection(AReplacement: Boolean);
var LConnection: TNetConnection;
begin
  if (not AReplacement) and Assigned(FOld) then LConnection := FOld
  else begin
    LConnection := TNetClient.Create(nil);
    LConnection.FreeNotification(Self);
  end;
  if AReplacement then FReplacement := LConnection
  else FOld := LConnection;
  Require(LConnection.ConnectTo('127.0.0.1', FPort), 'Cannot connect the actual outgoing owner');
  Require(FListener.CanRead(1000), 'Loopback peer did not receive connection');
  FreeAndNil(FPeer);
  FPeer := TTCPBlockSocket.Create;
  FPeer.Socket := FListener.Accept;
  Require(FListener.LastError = 0, 'Cannot accept loopback connection');
  FWire.Clear;
  // The owner's existing long-lived lease is the essential regression setup:
  // a dropped socket must become reconnectable before this object is destroyed.
  PinActiveConnections;
end;

procedure TRecoveryCase.AwaitDisconnected(AConnection: TNetConnection);
var LUntil: UInt64;
begin
  LUntil := GetTickCount64 + CT_Deadline;
  repeat
    if not AConnection.Connected then Exit;
    Pump;
  until GetTickCount64 >= LUntil;
  raise Exception.Create('Actual outgoing transport did not observe peer EOF');
end;

procedure TRecoveryCase.AcceptDiscoveredReplacement;
var LAddress: TNodeServerAddress; LFixed: TNodeServerAddressArray;
  LUntil: UInt64; LConnections: TList<TNetConnection>; LConnection: TNetConnection;
begin
  AwaitEndpointDetached(LAddress);
  // Age only owned fixture metadata. Production discovery admission and its
  // randomized worker start remain intact, including the mapping regression.
  LAddress.last_attempt_to_connect := Now - EncodeTime(0, 4, 0, 0);
  LAddress.total_failed_attemps_to_connect := 0;
  FNet.NodeServersAddresses.SetNodeServerAddress(LAddress);
  TNode.DecodeIpStringToNodeServerAddressArray('127.0.0.1:' + IntToStr(FPort), LFixed);
  FNet.DiscoverFixedServersOnly(LFixed);
  FNet.MinServersConnected := 1;
  FNet.MaxServersConnected := 1;
  LUntil := GetTickCount64 + 20000;
  repeat
    if FListener.CanRead(10) then Break;
    Pump;
  until GetTickCount64 >= LUntil;
  Require(FListener.CanRead(0), 'Existing background discovery did not reconnect the sole dropped peer within 20 seconds');
  FreeAndNil(FPeer);
  FPeer := TTCPBlockSocket.Create;
  FPeer.Socket := FListener.Accept;
  FWire.Clear;
  LConnections := TList<TNetConnection>.Create;
  try
    LUntil := GetTickCount64 + CT_Deadline;
    repeat
      FNet.AcquireDownloadConnections(LConnections);
      for LConnection in LConnections do begin
        if (not Assigned(FReplacement)) and (LConnection <> FOld) and
          (LConnection.Client.RemotePort = FPort) then begin
          FReplacement := LConnection;
          FReplacement.FreeNotification(Self);
          FLeases.Add(LConnection);
        end else FNet.ReleaseDownloadConnection(LConnection);
      end;
      LConnections.Clear;
      if Assigned(FReplacement) then Break;
      Pump;
    until GetTickCount64 >= LUntil;
    Require(Assigned(FReplacement), 'Discovery socket did not publish a pinnable replacement connection');
  finally
    LConnections.Free;
  end;
end;

procedure TRecoveryCase.AwaitEndpointDetached(var AAddress: TNodeServerAddress);
var LUntil: UInt64;
begin
  // Connected becomes false immediately before the disconnect callback runs.
  // Wait for that callback's own address update rather than racing it with a
  // fixture read/modify/write that could accidentally restore a stale pointer.
  LUntil := GetTickCount64 + CT_Deadline;
  repeat
    Require(FNet.NodeServersAddresses.GetNodeServerAddress('127.0.0.1', FPort, False, AAddress),
      'Known endpoint disappeared');
    if AAddress.netConnection = nil then Exit;
    Pump;
  until GetTickCount64 >= LUntil;
  raise Exception.Create('Disconnected leased owner still occupies the endpoint discovery mapping');
end;

procedure TRecoveryCase.DropPeer;
begin
  FPeer.CloseSocket;
end;

function TRecoveryCase.AwaitRequest(ATimeout: Cardinal; AOperation: Word;
  ARequired: Boolean): Cardinal;
var LUntil: UInt64; LPacket: RawByteString; LReadAt: Int64;
  LHeader: TNetHeaderData; LPayload: TMemoryStream; LMore: Boolean;
begin
  Result := 0;
  LPayload := TMemoryStream.Create;
  try
    LUntil := GetTickCount64 + ATimeout;
    repeat
      while TNetData.ExtractHeaderInfo(FWire, LHeader, LPayload, LMore) do begin
        if (LHeader.header_type = ntp_request) and
          (LHeader.operation = AOperation) then Exit(LHeader.request_id);
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
    if ARequired then raise Exception.Create('Inherited transport did not issue ' +
      TNetData.OperationToText(AOperation) + ' before deadline');
  finally
    LPayload.Free;
  end;
end;

procedure TRecoveryCase.SendFrame(AType, AOperation: Word; ARequest: Cardinal;
  AError: Word; APayload: TMemoryStream);
var LWire: TMemoryStream; LMagic, LLength: Cardinal; LType, LVersion: Word;
  LPacket: RawByteString;
begin
  LWire := TMemoryStream.Create;
  try
    LMagic := CT_MagicNetIdentification;
    LType := AType;
    LVersion := CT_NetProtocol_Version;
    LWire.WriteBuffer(LMagic, SizeOf(LMagic));
    LWire.WriteBuffer(LType, SizeOf(LType));
    LType := AOperation;
    LWire.WriteBuffer(LType, SizeOf(LType));
    LWire.WriteBuffer(AError, SizeOf(AError));
    LWire.WriteBuffer(ARequest, SizeOf(ARequest));
    LWire.WriteBuffer(LVersion, SizeOf(LVersion));
    LVersion := CT_NetProtocol_Available;
    LWire.WriteBuffer(LVersion, SizeOf(LVersion));
    LLength := APayload.Size;
    LWire.WriteBuffer(LLength, SizeOf(LLength));
    APayload.Position := 0;
    LWire.CopyFrom(APayload, APayload.Size);
    SetLength(LPacket, LWire.Size);
    LWire.Position := 0;
    LWire.ReadBuffer(LPacket[1], Length(LPacket));
    FPeer.SendString(LPacket);
    Require(FPeer.LastError = 0, 'Raw peer failed to send response');
  finally
    LWire.Free;
  end;
end;

procedure TRecoveryCase.SendReply(ARequest: Cardinal; AError: Word; APayload: TMemoryStream);
begin
  SendFrame(CT_MagicResponse, CT_NetOp_GetSafeBox, ARequest, AError, APayload);
end;

procedure TRecoveryCase.CheckCancelledFiles;
var LStream: TFileStream; LBytes: AnsiString; LSearch: TSearchRec;
begin
  Require(not FDownload.DownloadResult, 'Cancelled transport-only fixture published a safebox');
  Require(FDownload.ChunksPublished = 0, 'Cancelled chunks were transferred for publication');
  Require(FDownload.Error = '', FDownload.Error);
  Require(not FileExists(FNode.Bank.GetStorageFolder('') + PathDelim + 'safebox_100.safebox'),
    'An interrupted chunk published a final archive');
  LStream := TFileStream.Create(FDirectory + PathDelim + 'completed.safebox', fmOpenRead);
  try
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[1], Length(LBytes));
    Require(LBytes = CT_Sentinel, 'Existing completed-file sentinel changed');
  finally
    LStream.Free;
  end;
  if FindFirst(FDirectory + PathDelim + 'Temp' + PathDelim + '*.tmp', faAnyFile, LSearch) = 0 then begin
    FindClose(LSearch);
    raise Exception.Create('Downloader left a partial temporary stream after joining');
  end;
  Require(FNode.GetSynchronizationStatus.ActiveTransfers = 0,
    'Cancelled downloader left a physical synchronization scope open');
end;

procedure TRecoveryCase.CheckLateResponse;
var LPayload: TMemoryStream; LRequest, LValue: Cardinal;
begin
  FLate := TLateResponseTask.Create(Self);
  FLate.Start;
  Log('LATE awaiting first outgoing request');
  Require(AwaitRequest(CT_Deadline) = FLate.FirstID, 'Missing first request');
  Log('LATE first request observed; awaiting timeout and second request');
  LRequest := AwaitRequest(CT_Deadline);
  Require(LRequest = FLate.SecondID, 'Timed-out request did not yield the second request');
  LPayload := TMemoryStream.Create;
  try
    LValue := $BAD;
    LPayload.WriteBuffer(LValue, SizeOf(LValue));
    SendReply(FLate.FirstID, 0, LPayload);
    LPayload.Clear;
    LValue := $C0FFEE;
    LPayload.WriteBuffer(LValue, SizeOf(LValue));
    SendReply(FLate.SecondID, 0, LPayload);
    Log('LATE obsolete and matching responses sent');
    Await(FLate.Done, CT_Deadline, 'Matching response wait did not finish');
    Require(FLate.Error = '', FLate.Error);
    Require(not FLate.FirstResult and FLate.SecondResult,
      'An obsolete GET_SAFEBOX reply interrupted the current matching reply');
    Require(FOld.Connected, 'Obsolete response disconnected a healthy peer');
    Require(InterlockedCompareExchange(FProtocolRejections, 0, 0) = 0,
      'Obsolete response entered invalid-client rejection');
    Require(not FNet.NodeServersAddresses.IsBlackListed('127.0.0.1'),
      'Obsolete response blacklisted a healthy peer');
    Log('PASS late response: timed-out GET_SAFEBOX discarded; current reply consumed, peer connected and not blacklisted');
  finally
    LPayload.Free;
  end;
end;

procedure TRecoveryCase.CheckEndpointReplacement;
var LAddress: TNodeServerAddress; LCandidates: TNodeServerAddressArray;
  LUntil: UInt64;
begin
  DropPeer;
  AwaitDisconnected(FOld);
  Require(InterlockedCompareExchange(FOldGone, 0, 0) = 0,
    'The fixture must retain its disconnected old owner lease');
  AwaitEndpointDetached(LAddress);
  // Preserve automatic backoff semantics, but age this owned test address so
  // eligibility can be asserted without waiting three real minutes.
  FNet.NodeServersAddresses.LockList;
  try
    LAddress.last_attempt_to_connect := Now - EncodeTime(0, 4, 0, 0);
    LAddress.total_failed_attemps_to_connect := 0;
    FNet.NodeServersAddresses.SetNodeServerAddress(LAddress);
    SetLength(LCandidates, 1);
    LCandidates[0] := LAddress;
    FNet.NodeServersAddresses.GetNodeServersToConnnect(1, True, LCandidates);
    Require((Length(LCandidates) = 1) and (LCandidates[0].port = FPort),
      'Actual discovery candidate filter cannot admit the sole dropped endpoint');
  finally
    // This case replaces the socket explicitly to test old-destructor mapping.
    // Restore backoff before admitting the independent background discovery;
    // the separate downloader case exercises its automatic reconnect path.
    try
      LAddress.last_attempt_to_connect := Now;
      FNet.NodeServersAddresses.SetNodeServerAddress(LAddress);
    finally
      FNet.NodeServersAddresses.UnlockList;
    end;
  end;
  AcceptConnection(True);
  // The first element pins only the old owner. Release it while keeping the
  // replacement's pin: old destruction must not erase the new pointer mapping.
  Require(FLeases[0] = FOld, 'Old connection lease setup changed');
  FOld.FinalizeConnection;
  FNet.ReleaseDownloadConnection(FOld);
  FLeases.Delete(0);
  LUntil := GetTickCount64 + CT_Deadline;
  repeat
    if InterlockedCompareExchange(FOldGone, 0, 0) <> 0 then Break;
    Pump;
  until GetTickCount64 >= LUntil;
  Require(InterlockedCompareExchange(FOldGone, 0, 0) = 1,
    'Old disconnected owner did not retire after releasing its final lease');
  Require(FNet.NodeServersAddresses.GetNodeServerAddress('127.0.0.1', FPort, False, LAddress)
    and (LAddress.netConnection = FReplacement),
    'Old destructor erased its replacement endpoint binding');
  Require(FReplacement.Connected, 'Replacement socket did not remain connected');
  Log('PASS endpoint: leased EOF owner detached, real candidate filter admitted endpoint, old destructor preserved replacement mapping');
end;

procedure TRecoveryCase.CheckDownloaderRetry;
var LFirst, LSecond: Cardinal;
begin
  FDownload := TDownloadTask.Create(Self, False);
  FDownload.Start;
  LFirst := AwaitRequest(CT_Deadline);
  DropPeer;
  AwaitDisconnected(FOld);
  AcceptDiscoveredReplacement;
  LSecond := AwaitRequest(CT_Deadline);
  Require(LSecond <> LFirst, 'Inherited chunk retry reused its interrupted request ID');
  FDownload.Terminate;
  DropPeer;
  Await(FDownload.Done, CT_Deadline, 'Actual inherited retry did not cancel cooperatively');
  CheckCancelledFiles;
  Log(Format('PASS downloader: automatic background discovery reconnected sole peer; real GET_SAFEBOX request %d retried as %d; cancelled workers joined, no archive published',
    [LFirst, LSecond]));
end;

procedure BuildInvalidHeaderChunk(AResult: TMemoryStream);
var LCompressed: TMemoryStream; LZlib: TCompressionStream;
  LData: array[0..4095] of Byte; LLength: Cardinal;
begin
  FillChar(LData, SizeOf(LData), 0);
  LCompressed := TMemoryStream.Create;
  try
    LZlib := TCompressionStream.Create(clFastest, LCompressed);
    try
      LZlib.WriteBuffer(LData[0], SizeOf(LData));
    finally
      LZlib.Free;
    end;
    TStreamOp.WriteAnsiString(AResult, TEncoding.ASCII.GetBytes(CT_SafeBoxChunkIdentificator));
    AResult.WriteBuffer(CT_SafeBoxBankVersion, SizeOf(CT_SafeBoxBankVersion));
    LLength := SizeOf(LData);
    AResult.WriteBuffer(LLength, SizeOf(LLength));
    LLength := LCompressed.Size;
    AResult.WriteBuffer(LLength, SizeOf(LLength));
    LCompressed.Position := 0;
    AResult.CopyFrom(LCompressed, LCompressed.Size);
  finally
    LCompressed.Free;
  end;
end;

procedure TRecoveryCase.CheckChunkRequestLease;
var LPayload: TMemoryStream; LRequest: Cardinal; LAcquired: Boolean;
begin
  FDownload := TDownloadTask.Create(Self, True);
  FDownload.Start;
  LRequest := AwaitRequest(CT_Deadline);
  LPayload := TMemoryStream.Create;
  try
    BuildInvalidHeaderChunk(LPayload);
    SendReply(LRequest, 0, LPayload);
    Await(FState.WriteEntered, CT_Deadline, 'Real chunk decompression did not reach its destination');
    LAcquired := FNet.ConnectionLock(Self, FOld, 0);
    if LAcquired then FNet.ConnectionUnlock(FOld);
    // Keep the fixture's independent lifetime pin even in RED; this allows a
    // failed ownership assertion to unwind safely rather than causing a UAF.
    DropPeer;
    AwaitDisconnected(FOld);
    FDownload.Terminate;
    FState.ReleaseWrite.SetEvent;
    Await(FDownload.Done, CT_Deadline, 'In-flight chunk validation did not finish before cleanup');
    Require(not LAcquired, 'Chunk worker released its processing lock before response validation finished');
    Require((FState.ChunkEntries = 1) and (FState.ChunkExits = 1) and (FState.ChunkJoins = 1),
      'The actual single chunk worker did not complete and join exactly once');
    CheckCancelledFiles;
    Log('PASS chunk lease: actual decompression retained request lock through peer EOF; cooperative validation returned before stream cleanup');
  finally
    LPayload.Free;
  end;
end;

procedure TRecoveryCase.CheckBusyHandoff;
var LUntil: UInt64;
begin
  FNode.UpdateBlockchain := True;
  FNet.RequestDownload;
  Await(FState.FirstBody, CT_Deadline, 'Initial real blockchain owner did not enter its body');
  FNet.RequestDownload;
  Require((FState.Calls = 1) and (FState.Factories = 1),
    'A busy start request must not create a concurrent blockchain worker');
  FState.ReleaseBody.SetEvent;
  Await(FState.SecondBody, CT_Deadline,
    'Busy replacement-start intent was lost after previous worker completed');
  // Let that body return and observe five nominal 100-ms maintenance periods.
  // A pending intent left latched must not keep creating further successors.
  LUntil := GetTickCount64 + 500;
  repeat
    Pump;
  until GetTickCount64 >= LUntil;
  Require((FState.Calls = 2) and (FState.Factories = 2),
    'Exactly one successor must run without another HELLO or explicit start');
  Log('PASS handoff: start while busy retained; exactly one successor admitted after normal completion without repeating HELLO');
end;

procedure TRecoveryCase.CheckEarlyPendingExchange;
var LHello: TMemoryStream; LKey: TECPrivateKey; LOperation: TOperationBlock;
  LPort: Word; LTimestamp, LCount, LRequest: Cardinal; LWork: UInt64;
  LUntil: UInt64;
begin
  Require(FNode.Bank.BlocksCount = 0, 'Early pending check requires an empty local chain');
  LHello := TMemoryStream.Create;
  LKey := TECPrivateKey.Create;
  try
    LRequest := AwaitRequest(CT_Deadline, CT_NetOp_Hello);
    LKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
    LPort := FPort;
    LHello.WriteBuffer(LPort, SizeOf(LPort));
    TStreamOp.WriteAnsiString(LHello, TAccountComp.AccountKey2RawString(LKey.PublicKey));
    LTimestamp := FNet.NetworkAdjustedTime.GetAdjustedTime;
    LHello.WriteBuffer(LTimestamp, SizeOf(LTimestamp));
    LOperation := CT_OperationBlock_NUL;
    LOperation.block := 100;
    LOperation.protocol_version := CT_PROTOCOL_5;
    LOperation.protocol_available := CT_BlockChain_Protocol_Available;
    LOperation.account_key := LKey.PublicKey;
    Require(TPCOperationsComp.SaveOperationBlockToStream(LOperation, LHello),
      'Cannot serialize peer-ahead HELLO operation header');
    LCount := 0;
    LHello.WriteBuffer(LCount, SizeOf(LCount));
    TStreamOp.WriteAnsiString(LHello, TEncoding.ASCII.GetBytes('RecoveryPeer'));
    LWork := 0;
    LHello.WriteBuffer(LWork, SizeOf(LWork));
    TStreamOp.WriteAnsiString(LHello, TEncoding.ASCII.GetBytes('1'));
    SendFrame(CT_MagicResponse, CT_NetOp_Hello, LRequest, 0, LHello);
    LUntil := GetTickCount64 + CT_Deadline;
    repeat
      if FOld.ClientAppVersion = 'RecoveryPeer' then Break;
      Pump;
    until GetTickCount64 >= LUntil;
    Require(FOld.ClientAppVersion = 'RecoveryPeer', 'Actual HELLO decoder did not accept owned peer metadata');
    // The real parser used to query pending operations immediately after this
    // HELLO, before a newly scheduled blockchain worker hid pending state.
    LRequest := AwaitRequest(700, CT_NetOp_GetPendingOperations, False);
    Require(LRequest = 0, 'Empty/behind local chain requested peer pending operations before catch-up');
    Require(FOld.Connected and not FNet.NodeServersAddresses.IsBlackListed('127.0.0.1'),
      'Deferring pending exchange must preserve the healthy peer');
    Log('PASS pending: actual peer-ahead HELLO on empty local chain did not request pending operations before catch-up');
  finally
    LKey.Free;
    LHello.Free;
  end;
end;

procedure TRecoveryCase.CheckInvalidPacketType;
var LPayload: TMemoryStream;
begin
  LPayload := TMemoryStream.Create;
  try
    SendFrame(CT_MagicAutoSend, CT_NetOp_GetSafeBox, FNet.NewRequestId, 0, LPayload);
    AwaitDisconnected(FOld);
    Require(InterlockedCompareExchange(FPacketRejections, 0, 0) = 1,
      'Malformed GET_SAFEBOX autosend did not enter its exact invalid-client rejection');
    Require(not FNet.NodeServersAddresses.IsBlackListed('127.0.0.1'),
      'Invalid-client rejection must preserve the existing loopback blacklist exemption');
    Log('PASS negative packet: malformed GET_SAFEBOX autosend logged exact invalid-client rejection and disconnected; loopback exemption preserved');
  finally
    LPayload.Free;
  end;
end;

procedure TRecoveryCase.CheckMatchingInvalidChunk;
var LPayload: TMemoryStream; LRequest: Cardinal;
begin
  FDownload := TDownloadTask.Create(Self, False);
  FDownload.Start;
  LRequest := AwaitRequest(CT_Deadline);
  LPayload := TMemoryStream.Create;
  try
    BuildInvalidHeaderChunk(LPayload);
    SendReply(LRequest, 0, LPayload);
    AwaitDisconnected(FOld);
    Require(InterlockedCompareExchange(FChunkRejections, 0, 0) = 1,
      'Matching invalid safebox header did not enter its exact invalid-chunk rejection');
    Require(not FNet.NodeServersAddresses.IsBlackListed('127.0.0.1'),
      'Invalid chunk rejection must preserve the existing loopback blacklist exemption');
    FDownload.Terminate;
    Await(FDownload.Done, CT_Deadline, 'Invalid chunk retry did not cancel cooperatively');
    CheckCancelledFiles;
    Log('PASS negative chunk: matching GET_SAFEBOX with invalid safebox header rejected; no archive published');
  finally
    LPayload.Free;
  end;
end;

function RunSafeboxRecoveryChecks(const ALogFile: String): Integer;
var LCase: TRecoveryCase; I: Integer; LName, LBase: String;
begin
  Result := 0;
  GLogFile := ExpandFileName(ALogFile);
  GLog := TStringList.Create;
  try
    RequirePrivateDesktop;
    LBase := IncludeTrailingPathDelimiter(ExtractFilePath(GLogFile)) + 'data';
    ForceDirectories(LBase);
    for I := 0 to 7 do begin
      case I of
        0: LName := 'late-response';
        1: LName := 'endpoint-replacement';
        2: LName := 'downloader-retry';
        3: LName := 'chunk-request-lease';
        4: LName := 'busy-handoff';
        5: LName := 'early-pending-exchange';
        6: LName := 'invalid-packet-type';
        7: LName := 'matching-invalid-chunk';
      end;
      Log('BEGIN ' + LName);
      LCase := nil;
      try
        LCase := TRecoveryCase.Create(LBase, LName);
        case I of
          0: LCase.CheckLateResponse;
          1: LCase.CheckEndpointReplacement;
          2: LCase.CheckDownloaderRetry;
          3: LCase.CheckChunkRequestLease;
          4: LCase.CheckBusyHandoff;
          5: LCase.CheckEarlyPendingExchange;
          6: LCase.CheckInvalidPacketType;
          7: LCase.CheckMatchingInvalidChunk;
        end;
      except
        on E: Exception do begin
          Inc(Result);
          Log('FAIL ' + LName + ': ' + E.ClassName + ': ' + E.Message);
        end;
      end;
      LCase.Free;
      Log('END ' + LName);
    end;
    Log(Format('RESULT cases=8 failures=%d', [Result]));
  except
    on E: Exception do begin
      Result := 99;
      Log('FATAL ' + E.ClassName + ': ' + E.Message);
    end;
  end;
  GLog.Free;
end;

end.
