unit UPoolMinerThreads;

{$mode delphi}

{ Copyright (c) 2017 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of Pascal Coin, a P2P crypto currency without need of
  historical operations.

  If you like it, consider a donation using BitCoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  }

interface

uses
  Classes, SysUtils, syncobjs, UThread, UPoolMining, UAccounts, UCrypto, ULog, UBlockChain, USha256;

type
  TMinerStats = Record
    Miners : Integer;
    RoundsCount : UInt64;
    WorkingMillisecondsHashing : Cardinal;
    WorkingMillisecondsTotal : Cardinal;
    WinsCount : Integer;
    Invalids : Integer;
  End;

Const
  CT_TMinerStats_NULL : TMinerStats = (Miners:0;RoundsCount:0;WorkingMillisecondsHashing:0;WorkingMillisecondsTotal:0;WinsCount:0;Invalids:0);

Type

  TCustomMinerDeviceThread = Class;
  TCustomMinerDeviceThreadClass = Class of TCustomMinerDeviceThread;

  TOnFoundNonce = Procedure(Sender : TCustomMinerDeviceThread; Timestamp, nOnce : Cardinal) of object;

  { TPoolMinerThread }

  TPoolMinerThread = Class(TPCThread)
  private
    FMinerAddName: String;
    FPoolMinerClient : TPoolMinerClient;
    FOnConnectionStateChanged: TNotifyEvent;
    FDevicesList : TPCThreadList;
    FMinerThreads: Integer;
    FGlobalMinerValuesForWork : TMinerValuesForWork;
    FTestingPoWLeftBits: Byte;
    Procedure OnPoolMinerClientConnectionChanged(Sender : TObject);
    Procedure OnPoolMinerMustChangeValues(Sender : TObject);
    Procedure OnMinerNewBlockFound(sender : TCustomMinerDeviceThread; Timestamp : Cardinal; NOnce : Cardinal);
    Procedure NotifyPoolMinerConnectionChanged;
    procedure SetMinerAddName(AValue: String);
    procedure SetTestingPoWLeftBits(AValue: Byte);
  protected
    procedure BCExecute; override;
  public
    Constructor Create(RemoteHost : String; RemotePort : Integer; InitialAccountKey : TAccountKey);
    Destructor Destroy; override;
    Property PoolMinerClient : TPoolMinerClient read FPoolMinerClient;
    Property OnConnectionStateChanged : TNotifyEvent read FOnConnectionStateChanged write FOnConnectionStateChanged;
    Function CurrentMinerStats : TMinerStats;
    Function GlobalMinerStats : TMinerStats;
    Property GlobalMinerValuesForWork : TMinerValuesForWork read FGlobalMinerValuesForWork;
    Function DevicesLock : TList;
    procedure DevicesUnlock;
    Property MinerAddName : String read FMinerAddName write SetMinerAddName;
    Property TestingPoWLeftBits : Byte read FTestingPoWLeftBits write SetTestingPoWLeftBits;
  End;

  { TCustomMinerDeviceThread }
  TCustomMinerDeviceThread = Class(TPCThread)
  private
    FIsMining: Boolean;
    FOnFoundNOnce: TOnFoundNonce;
    FOnMinerValuesChanged: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;
    FPaused: Boolean;
    FLastStats : TPCThreadList;
    FLastActiveTC : Cardinal;
    FGlobaDeviceStats : TMinerStats;
    FPartialDeviceStats : TMinerStats;
    FPoolMinerThread : TPoolMinerThread;
    procedure SetIsMining(AValue: Boolean);
    procedure SetPaused(AValue: Boolean);
  protected
    FMinerValuesForWork : TMinerValuesForWork;
    Procedure SetMinerValuesForWork(const Value : TMinerValuesForWork); virtual;
    Procedure UpdateState; virtual;
    Procedure UpdateDeviceStats(Stats : TMinerStats); virtual;
    Procedure FoundNOnce(Timestamp,nOnce : Cardinal);
  public
    Constructor Create(APoolMinerThread : TPoolMinerThread; InitialMinerValuesForWork : TMinerValuesForWork); virtual;
    Destructor Destroy; override;
    Function DeviceStats : TMinerStats;
    Function GlobalDeviceStats : TMinerStats;
    Function MinerDeviceName : String; virtual; abstract;
    Property Paused : Boolean read FPaused write SetPaused;
    Property OnStateChanged : TNotifyEvent read FOnStateChanged write FOnStateChanged;
    Property OnMinerValuesChanged : TNotifyEvent read FOnMinerValuesChanged write FOnMinerValuesChanged;
    Property OnFoundNOnce : TOnFoundNonce read FOnFoundNOnce write FOnFoundNOnce;
    Function GetState : String; virtual; abstract;
    property MinerValuesForWork : TMinerValuesForWork read FMinerValuesForWork;
    Property IsMining : Boolean read FIsMining write SetIsMining;
    Property PoolMinerThread : TPoolMinerThread read FPoolMinerThread;
  end;



  { TCPUDeviceThread }

  TCPUDeviceThread = Class(TCustomMinerDeviceThread)
  private
    FCPUs: Integer;
    FCPUsThreads : TPCThreadList;
    FUseOpenSSLFunctions: Boolean;
    procedure SetCPUs(AValue: Integer);
    Procedure CheckCPUs;
    procedure SetUseOpenSSLFunctions(AValue: Boolean);
  protected
    procedure BCExecute; override;
    procedure SetMinerValuesForWork(const Value: TMinerValuesForWork); override;
    Procedure UpdateState; override;
  public
    Constructor Create(PoolMinerThread : TPoolMinerThread; InitialMinerValuesForWork : TMinerValuesForWork); override;
    Destructor Destroy; override;
    Property CPUs : Integer read FCPUs write SetCPUs;
    Property UseOpenSSLFunctions : Boolean read FUseOpenSSLFunctions write SetUseOpenSSLFunctions;
    Function MinerDeviceName : String; override;
    Function GetState : String; override;
  end;

  { TCPUOpenSSLMinerThread }

  TCPUOpenSSLMinerThread = Class(TPCThread)
  private
    FCPUDeviceThread : TCPUDeviceThread;
    FLock : TCriticalSection;
  protected
    FInternalSha256 : TSHA256HASH;
    FInternalChunk : TChunk;
    FDigestMsg : TRawBytes;
    FChangeTimestampAndNOnceBytePos : Integer;
    FDigestStreamMsg : TMemoryStream;
    FMinNOnce,FMaxNOnce : Cardinal;
    procedure BCExecute; override;
  public
    Constructor Create(CPUDeviceThread : TCPUDeviceThread);
    Destructor Destroy; override;
  End;

implementation

uses UConst, UTime, UJSONFunctions, UNode, UNetProtocol;

{ TPoolMinerThread }

procedure TPoolMinerThread.BCExecute;
Var nID : Cardinal;
  json : TPCJSONObject;
  i : Integer;
  ResponseMethod : String;
  l : TList;
begin
  Try
    while not Terminated do begin
      if not FPoolMinerClient.Connected then begin
        If Not FPoolMinerClient.Connect then begin
        end else begin
          TLog.NewLog(ltinfo,ClassName,'Starting connection to '+FPoolMinerClient.ClientRemoteAddr);
        end;
      end else begin
          // Start Process
          nId:=FPoolMinerClient.GetNewId;
          FPoolMinerClient.SendJSONRPCMethod(CT_PoolMining_Method_MINER_NOTIFY,nil,nId);
          json := TPCJSONObject.create;
          try
            repeat
              if (FPoolMinerClient.DoProcessBuffer(Self,1000,true,ResponseMethod,json)) then begin
                FPoolMinerClient.DoProcessJSONObject(json,ResponseMethod);
                json.Clear;
              end;
            until (terminated) or (Not FPoolMinerClient.Connected);
          finally
            json.Free;
          end;
          FPoolMinerClient.Disconnect;
      end;
      for i := 1 to 100 do begin
        if terminated then exit;
        sleep(50);
      end;
    end;
  Finally
    FPoolMinerClient.Disconnect;
    l := FDevicesList.LockList;
    try
      for i:=0 to l.count-1 do begin
        TCustomMinerDeviceThread(l[i]).Terminate;
        TCustomMinerDeviceThread(l[i]).WaitFor;
      end;
    finally
      FDevicesList.UnlockList;
    end;
  End;
end;

constructor TPoolMinerThread.Create(RemoteHost: String; RemotePort: Integer; InitialAccountKey : TAccountKey);
begin
  FGlobalMinerValuesForWork := CT_TMinerValuesForWork_NULL;
  FPoolMinerClient := TPoolMinerClient.Create(Nil);
  FPoolMinerClient.RemoteHost := RemoteHost;
  FPoolMinerClient.RemotePort := RemotePort;
  FPoolMinerClient.OnMinerMustChangeValues := OnPoolMinerMustChangeValues;
  FPoolMinerClient.OnConnect := OnPoolMinerClientConnectionChanged;
  FPoolMinerClient.OnDisconnect := OnPoolMinerClientConnectionChanged;
  FOnConnectionStateChanged := Nil;
  FDevicesList := TPCThreadList.Create;
  FMinerThreads := 0;
  FMinerAddName:='';
  FTestingPoWLeftBits := 0;
  inherited Create(false);
end;

function TPoolMinerThread.CurrentMinerStats: TMinerStats;
Var l : TList;
  i : Integer;
  ms : TMinerStats;
begin
  Result := CT_TMinerStats_NULL;
  l := FDevicesList.LockList;
  try
    Result.Miners := l.Count;
    for i := 0 to l.Count - 1 do begin
      ms := TCustomMinerDeviceThread(l[i]).DeviceStats;
      inc(Result.Miners, ms.Miners);
      inc(Result.RoundsCount, ms.RoundsCount);
      inc(Result.WorkingMillisecondsHashing, ms.WorkingMillisecondsHashing);
      inc(Result.WorkingMillisecondsTotal, ms.WorkingMillisecondsTotal);
      inc(Result.WinsCount, ms.WinsCount);
    end;
  finally
    FDevicesList.UnlockList;
  end;
end;

destructor TPoolMinerThread.Destroy;
Var i : Integer;
  l : TList;
begin
  l := FDevicesList.LockList;
  try
    for i := l.Count - 1 downto 0 do begin
      TCustomMinerDeviceThread(l[i]).Terminate;
      TCustomMinerDeviceThread(l[i]).WaitFor;
      TCustomMinerDeviceThread(l[i]).Free;
    end;
    l.Clear;
  finally
    FDevicesList.UnlockList;
  end;
  FreeAndNil(FDevicesList);
  FPoolMinerClient.Disconnect;
  FreeAndNil(FPoolMinerClient);
  inherited;
end;

function TPoolMinerThread.DevicesLock: TList;
begin
  Result := FDevicesList.LockList;
end;

procedure TPoolMinerThread.DevicesUnlock;
begin
  FDevicesList.UnlockList;
end;

function TPoolMinerThread.GlobalMinerStats: TMinerStats;
Var l : TList;
  i : Integer;
  ms : TMinerStats;
begin
  Result := CT_TMinerStats_NULL;
  l := FDevicesList.LockList;
  try
    Result.Miners := l.Count;
    for i := 0 to l.Count - 1 do begin
      ms := TCustomMinerDeviceThread(l[i]).GlobalDeviceStats;
      inc(Result.Miners, ms.Miners);
      inc(Result.RoundsCount, ms.RoundsCount);
      inc(Result.WorkingMillisecondsHashing, ms.WorkingMillisecondsHashing);
      inc(Result.WorkingMillisecondsTotal, ms.WorkingMillisecondsTotal);
      inc(Result.WinsCount, ms.WinsCount);
    end;
  finally
    FDevicesList.UnlockList;
  end;
end;

procedure TPoolMinerThread.NotifyPoolMinerConnectionChanged;
begin
  if Assigned(FOnConnectionStateChanged) then FOnConnectionStateChanged(Self);
  TLog.NewLog(ltInfo,ClassName,'Pool Miner Client Connection changed to: '+Inttostr(integer(FPoolMinerClient.Connected)));
end;

procedure TPoolMinerThread.SetMinerAddName(AValue: String);
begin
  if FMinerAddName=AValue then Exit;
  FMinerAddName:=AValue;
  If Assigned(FPoolMinerClient) then OnPoolMinerMustChangeValues(Nil);
end;

procedure TPoolMinerThread.SetTestingPoWLeftBits(AValue: Byte);
begin
  if FTestingPoWLeftBits=AValue then Exit;
  If (AValue>=0) And (AValue<=32) then
    FTestingPoWLeftBits:=AValue
  else FTestingPoWLeftBits:=0;
end;

procedure TPoolMinerThread.OnMinerNewBlockFound(sender : TCustomMinerDeviceThread; Timestamp : Cardinal; NOnce : Cardinal);
Var mvfw : TMinerValuesForWork;
begin
  FDevicesList.LockList;
  try
    mvfw := sender.FMinerValuesForWork;
    TLog.NewLog(ltinfo,ClassName,'FOUND VALID NONCE!!! Timestamp:'+Inttostr(Timestamp)+ ' Nonce:'+Inttostr(NOnce));
    FPoolMinerClient.SubmitBlockFound(mvfw.payload_start,Timestamp,NOnce);
  finally
    FDevicesList.UnlockList;
  end;
end;

procedure TPoolMinerThread.OnPoolMinerClientConnectionChanged(Sender: TObject);
Var l : TList;
  i : Integer;
begin
  TLog.NewLog(ltInfo,ClassName,'Connection state changed. New Value:'+inttostr(Integer(FPoolMinerClient.Connected)));
  l := FDevicesList.LockList;
  try
    For i:=0 to l.count-1 do begin
      TCustomMinerDeviceThread(l[i]).UpdateState;
    end;
  finally
    FDevicesList.UnlockList;
  end;
  // XXXXXXXXXXXX Synchronize(NotifyPoolMinerConnectionChanged);
  NotifyPoolMinerConnectionChanged;
end;

procedure TPoolMinerThread.OnPoolMinerMustChangeValues(Sender: TObject);
Var l : TList;
  i,j : Integer;
  digest : TRawBytes;
  ok : Boolean;
  minervfw : TMinerValuesForWork;
  auxXXXXX : TMinerValuesForWork;
begin
  FGlobalMinerValuesForWork := FPoolMinerClient.MinerValuesForWork;
  TLog.NewLog(ltupdate,ClassName,Format('New miner values. Block %d Payload %s',[FPoolMinerClient.MinerValuesForWork.block,
    FPoolMinerClient.MinerValuesForWork.payload_start]));
  l := FDevicesList.LockList;
  Try
    for i := 0 to l.Count - 1 do begin
      minervfw := FGlobalMinerValuesForWork;
      minervfw.payload_start:=minervfw.payload_start+FMinerAddName;
      if (l.count>1) then minervfw.payload_start:=minervfw.payload_start+'/'+inttostr(i);
      repeat
        digest := minervfw.part1 + minervfw.payload_start + minervfw.part3 + '00000000';
        ok := CanBeModifiedOnLastChunk(length(digest),j);
        if (not ok) then minervfw.payload_start:=minervfw.payload_start+'-';
      until (Ok);
      If FTestingPoWLeftBits>0 then begin
        auxXXXXX := minervfw;
        auxXXXXX.target:= ((((auxXXXXX.target AND $FF000000) SHR 24)-FTestingPoWLeftBits) SHL 24) + (minervfw.target AND $00FFFFFF);
        If auxXXXXX.target<CT_MinCompactTarget then auxXXXXX.target:=CT_MinCompactTarget;
        auxXXXXX.target_pow:=TPCBank.TargetFromCompact(auxXXXXX.target);
        TCustomMinerDeviceThread(l[i]).SetMinerValuesForWork(auxXXXXX);
      end else begin
        TCustomMinerDeviceThread(l[i]).SetMinerValuesForWork(minervfw);
      end;
    end;
  Finally
    FDevicesList.UnlockList;
  End;
end;

Type
  TTimeMinerStats = Record
    tc : Cardinal;
    stats : TMinerStats;
  end;
  PTimeMinerStats = ^TTimeMinerStats;

{ TCustomMinerDeviceThread }

constructor TCustomMinerDeviceThread.Create(APoolMinerThread: TPoolMinerThread; InitialMinerValuesForWork: TMinerValuesForWork);
begin
  FPoolMinerThread := APoolMinerThread;
  FMinerValuesForWork := CT_TMinerValuesForWork_NULL;
  FPartialDeviceStats := CT_TMinerStats_NULL;
  FGlobaDeviceStats := CT_TMinerStats_NULL;
  FLastStats := TPCThreadList.Create;
  FOnFoundNOnce:=Nil;
  FOnMinerValuesChanged:=Nil;
  FOnStateChanged:=Nil;
  FPaused:=true;
  FLastActiveTC := 0;
  SetMinerValuesForWork(InitialMinerValuesForWork);
  PoolMinerThread.FDevicesList.Add(Self);
  inherited Create(false);
end;

destructor TCustomMinerDeviceThread.Destroy;
Var i : Integer;
  P : PTimeMinerStats;
  l : TList;
begin
  l := FPoolMinerThread.FDevicesList.LockList;
  try
    l.Remove(Self);
  finally
    FPoolMinerThread.FDevicesList.UnlockList;
  end;
  l := FLastStats.LockList;
  try
    for i:=0 to l.Count-1 do begin
      P := l[i];
      Dispose(P);
    end;
    l.clear;
  finally
    FLastStats.UnlockList;
  end;
  FreeAndNil(FLastStats);
  inherited Destroy;
end;

function TCustomMinerDeviceThread.DeviceStats: TMinerStats;
begin
  FLastStats.LockList;
  try
    Result := FPartialDeviceStats;
  finally
    FLastStats.UnlockList;
  end;
end;

procedure TCustomMinerDeviceThread.FoundNOnce(Timestamp, nOnce: Cardinal);
Var digest,dsha256  : TRawBytes;
begin
  // Validation
  digest := self.FMinerValuesForWork.part1+self.FMinerValuesForWork.payload_start+self.FMinerValuesForWork.part3+'00000000';
  if length(digest)<8 then exit;
  // Add timestamp and nonce
  move(Timestamp,digest[length(digest)-7],4);
  move(nOnce,digest[length(digest)-3],4);
  dsha256 := TCrypto.DoSha256(TCrypto.DoSha256(digest));
  if (dsha256 <= self.FMinerValuesForWork.target_pow) then begin
    FPoolMinerThread.OnMinerNewBlockFound(self,Timestamp,nOnce);
    If Assigned(FOnFoundNOnce) then FOnFoundNOnce(Self,Timestamp,nOnce);
  end else begin
    inc(FGlobaDeviceStats.Invalids);
    TLog.NewLog(lterror,Self.Classname,Format('Invalid Double Sha256 found. Timestamp %s nOnce %s DSHA256 %s Valid POW %s',
      [IntToHex(Timestamp,8),IntToHex(nOnce,8),TCrypto.ToHexaString(dsha256),TCrypto.ToHexaString(self.FMinerValuesForWork.target_pow)]));
  end;
end;

function TCustomMinerDeviceThread.GlobalDeviceStats: TMinerStats;
Var g : TMinerStats;
begin
  FLastStats.LockList;
  try
    g := FGlobaDeviceStats;
    If Not FPaused then begin
      g.WorkingMillisecondsHashing:= g.WorkingMillisecondsHashing + (GetTickCount - FLastActiveTC);
      g.WorkingMillisecondsTotal:= g.WorkingMillisecondsTotal + (GetTickCount - FLastActiveTC);
    end;
    Result := g;
  finally
    FLastStats.UnlockList;
  end;
end;

procedure TCustomMinerDeviceThread.SetIsMining(AValue: Boolean);
begin
  if FIsMining=AValue then Exit;
  FIsMining:=AValue;
  If Assigned(FOnStateChanged) then FOnStateChanged(Self);
end;

procedure TCustomMinerDeviceThread.SetMinerValuesForWork(const Value: TMinerValuesForWork);
var i,aux : Integer;
  canWork : Boolean;
  oldPayload : String;
begin
  FMinerValuesForWork := Value;
  oldPayload := FMinerValuesForWork.payload_start;
  Repeat
    i := Length(FMinerValuesForWork.part1)+Length(FMinerValuesForWork.payload_start)+Length(FMinerValuesForWork.part3)+8;
    canWork := CanBeModifiedOnLastChunk(i,aux);
    If Not canWork then FMinerValuesForWork.payload_start:=FMinerValuesForWork.payload_start+' ';
  until (canWork);
  TLog.NewLog(ltinfo,classname,Format('Updated MinerValuesForWork: Payload:%s',[FMinerValuesForWork.payload_start]));
  If Assigned(FOnMinerValuesChanged) then FOnMinerValuesChanged(Self);
end;

procedure TCustomMinerDeviceThread.SetPaused(AValue: Boolean);
begin
  if FPaused=AValue then Exit;
  FPaused:=AValue;
  If Not FPaused then FLastActiveTC := GetTickCount
  else begin
    FGlobaDeviceStats.WorkingMillisecondsHashing:=FGlobaDeviceStats.WorkingMillisecondsHashing + (GetTickCount - FLastActiveTC);
    FGlobaDeviceStats.WorkingMillisecondsTotal:=FGlobaDeviceStats.WorkingMillisecondsTotal + (GetTickCount - FLastActiveTC);
  end;
  UpdateState;
end;

procedure TCustomMinerDeviceThread.UpdateDeviceStats(Stats: TMinerStats);
Type TTimeMinerStats = Record
       tc : Cardinal;
       stats : TMinerStats;
     end;
  PTimeMinerStats = ^TTimeMinerStats;
Var l : TList;
  i : Integer;
  P : PTimeMinerStats;
  minTC, foundMaxMiners : Cardinal;
begin
  l := FLastStats.LockList;
  Try
    FPartialDeviceStats := CT_TMinerStats_NULL;
    New(P);
    P^.tc:=(GetTickCount - stats.WorkingMillisecondsTotal);
    P^.stats:=stats;
    l.add(P);
    minTC := GetTickCount - 10000; // Last 10 seconds average
    foundMaxMiners:=0;
    for i:=l.Count-1 downto 0 do begin
      P := l[i];
      If (P^.tc<minTC) then begin
        l.Delete(i);
        Dispose(P);
      end else begin
        inc(FPartialDeviceStats.RoundsCount,P^.stats.RoundsCount);
        inc(FPartialDeviceStats.WinsCount,P^.stats.WinsCount);
        if ((stats.Miners>foundMaxMiners)) then foundMaxMiners := stats.Miners;
      end;
    end;
    If l.count>0 then begin
      P := PTimeMinerStats(l[l.count-1]);
      FPartialDeviceStats.WorkingMillisecondsHashing:=P^.tc - PTimeMinerStats(l[0]).tc + P^.stats.WorkingMillisecondsHashing;
      FPartialDeviceStats.WorkingMillisecondsTotal:=P^.tc - PTimeMinerStats(l[0]).tc + P^.stats.WorkingMillisecondsTotal;
    end;
    FPartialDeviceStats.Miners:= foundMaxMiners;
    If foundMaxMiners>FGlobaDeviceStats.Miners then FGlobaDeviceStats.Miners:=foundMaxMiners;
    Inc(FGlobaDeviceStats.RoundsCount,Stats.RoundsCount);
    Inc(FGlobaDeviceStats.WinsCount,Stats.WinsCount);
  finally
    FLastStats.UnlockList;
  end;
end;

procedure TCustomMinerDeviceThread.UpdateState;
begin
  If Assigned(FOnStateChanged) then FOnStateChanged(Self);
end;

{ TCPUDeviceThread }

procedure TCPUDeviceThread.BCExecute;
begin
  while not terminated do begin
    sleep(1);
  end;
end;

procedure TCPUDeviceThread.CheckCPUs;
var l : TList;
  mt : TCPUOpenSSLMinerThread;
  needminers : Integer;
begin
  needminers := FCPUs;
  if (FMinerValuesForWork.part1='') or (FPaused) then needminers := 0;
  if Not FPoolMinerThread.PoolMinerClient.Connected then needminers := 0;
  l := FCPUsThreads.LockList;
  try
    if l.Count=needminers then exit;
    while (l.Count<needminers) do begin
      mt := TCPUOpenSSLMinerThread.Create(Self);
      l.Add(mt);
    end;
    while (l.Count>needminers)  do begin
      mt := TCPUOpenSSLMinerThread(l[l.Count-1]);
      mt.Terminate;
      mt.WaitFor;
      mt.Free;
      l.Delete(l.Count-1);
    end;
    SetMinerValuesForWork(FMinerValuesForWork);
  finally
    FCPUsThreads.UnlockList;
  end;
  IsMining := needminers>0;
end;

constructor TCPUDeviceThread.Create(PoolMinerThread: TPoolMinerThread; InitialMinerValuesForWork: TMinerValuesForWork);
begin
  FCPUsThreads := TPCThreadList.Create;
  FCPUs:=0;
  FUseOpenSSLFunctions := true;
  inherited Create(PoolMinerThread, InitialMinerValuesForWork);
end;

destructor TCPUDeviceThread.Destroy;
begin
  FCPUs:=0;
  CheckCPUs;
  FCPUsThreads.free;
  inherited Destroy;
end;

function TCPUDeviceThread.GetState: String;
begin
  If Paused then Result := 'CPU miner is paused'
  else Result := 'CPU miner is active for '+inttostr(FCPUs)+' CPU''s';
end;

function TCPUDeviceThread.MinerDeviceName: String;
begin
  Result := 'CPU miner with '+inttostr(FCPUs)+' ('+inttostr(CPUCount)+' CPU''s available)';
end;

procedure TCPUDeviceThread.SetCPUs(AValue: Integer);
begin
  if FCPUs=AValue then Exit;
  FCPUs:=AValue;
  if FCPUs<0 then FCPUs := 0;
  if (FCPUs>CPUCount) And (CPUCount>0) then FCPUs := CPUCount;
  CheckCPUs;
end;

procedure TCPUDeviceThread.SetMinerValuesForWork(const Value: TMinerValuesForWork);
Var l : TList;
  i : Integer;
  nextmin : Cardinal;
  npos : Integer;
  cpu : TCPUOpenSSLMinerThread;
  digest : TRawBytes;
  Ok : Boolean;
  sflc : TSHA256HASH;
  lc : TChunk;
begin
  inherited;
  l := FCPUsThreads.LockList;
  try
    // Prepare final data:
    CheckCPUs;
    npos := 0;
    repeat
      digest := FMinerValuesForWork.part1 + FMinerValuesForWork.payload_start + FMinerValuesForWork.part3 + '00000000';
      ok := CanBeModifiedOnLastChunk(length(digest),npos);
      if (not ok) then FMinerValuesForWork.payload_start:=FMinerValuesForWork.payload_start+'.';
    until (Ok);
    PascalCoinPrepareLastChunk(digest,sflc,lc);
    nextmin := 0;
    for i:=0 to l.count-1 do begin
      cpu := TCPUOpenSSLMinerThread(l[i]);
      cpu.FLock.Acquire;
      try
        cpu.FInternalSha256 := sflc;
        cpu.FInternalChunk := lc;
        cpu.FDigestMsg:=digest;
        cpu.FDigestStreamMsg.size := 0;
        cpu.FChangeTimestampAndNOnceBytePos:=npos;
        cpu.FMinNOnce:=nextmin;
        cpu.FMaxNOnce:=nextmin + (Cardinal($FFFFFFFF) DIV FCPUs) - 1;
        nextmin := cpu.FMaxNOnce+1;
        cpu.FDigestStreamMsg.WriteBuffer(digest[1],length(digest));
      finally
        cpu.Flock.Release;
      end;
    end;
  finally
    FCPUsThreads.UnlockList;
  end;
end;

procedure TCPUDeviceThread.SetUseOpenSSLFunctions(AValue: Boolean);
begin
  if FUseOpenSSLFunctions=AValue then Exit;
  FUseOpenSSLFunctions:=AValue;
end;

procedure TCPUDeviceThread.UpdateState;
begin
  CheckCPUs;
  inherited;
end;

{ TCPUOpenSSLMinerThread }

procedure TCPUOpenSSLMinerThread.BCExecute;
Const CT_Rounds = 10000;
Var
  ts : Cardinal;
  i : Integer;
  nonce, baseRealTC,baseHashingTC,finalHashingTC : Cardinal;
  resultPoW : TRawBytes;
  //
  AuxStats : TMinerStats;
begin
  AuxStats := CT_TMinerStats_NULL;
  nonce := 0;
    while (Not Terminated) do begin
      AuxStats := CT_TMinerStats_NULL;
      If (FCPUDeviceThread.Paused) then sleep(1)
      else begin
        FLock.Acquire;
        try
          baseRealTC := GetTickCount;
          If (nonce<FMinNOnce) Or (nonce>FMaxNOnce) then nonce:=FMinNOnce;
          // Timestamp
          ts := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
          if ts<=FCPUDeviceThread.FMinerValuesForWork.timestamp then ts := FCPUDeviceThread.FMinerValuesForWork.timestamp+1;
          If FDigestStreamMsg.Size>8 then begin
            if FCPUDeviceThread.FUseOpenSSLFunctions then begin
              FDigestStreamMsg.Position:=FDigestStreamMsg.Size - 8;
              FDigestStreamMsg.Write(ts,4);
              baseHashingTC:=GetTickCount;
              for i := 1 to CT_Rounds do begin
                FDigestStreamMsg.Position := FDigestStreamMsg.Size - 4;
                FDigestStreamMsg.Write(nonce,4);
                TCrypto.DoDoubleSha256(FDigestStreamMsg.Memory,FDigestStreamMsg.Size,resultPoW);
                if resultPoW < FCPUDeviceThread.FMinerValuesForWork.target_pow then begin
                  if Terminated then exit;
                  inc(AuxStats.WinsCount);
                  FLock.Release;
                  try
                    FCPUDeviceThread.FoundNOnce(ts,nonce);
                  finally
                    FLock.Acquire;
                  end;
                end;
                if (nonce)<FMaxNOnce then inc(nonce) else nonce := FMinNOnce;
              end;
              finalHashingTC:=GetTickCount;
            end else begin
              baseHashingTC:=GetTickCount;
              for i := 1 to CT_Rounds do begin
                PascalCoinExecuteLastChunkAndDoSha256(FInternalSha256,FInternalChunk,FChangeTimestampAndNOnceBytePos,nonce,ts,resultPoW);
                if resultPoW < FCPUDeviceThread.FMinerValuesForWork.target_pow then begin
                  if Terminated then exit;
                  inc(AuxStats.WinsCount);
                  FLock.Release;
                  try
                    FCPUDeviceThread.FoundNOnce(ts,nonce);
                  finally
                    FLock.Acquire;
                  end;
                end;
                if (nonce)<FMaxNOnce then inc(nonce) else nonce := FMinNOnce;
              end;
              finalHashingTC:=GetTickCount;
            end;
            AuxStats.Miners:=FCPUDeviceThread.FCPUs;
            AuxStats.RoundsCount:=CT_Rounds;
            AuxStats.WorkingMillisecondsTotal:=GetTickCount - baseRealTC;
            AuxStats.WorkingMillisecondsHashing:= finalHashingTC - baseHashingTC;
            FCPUDeviceThread.UpdateDeviceStats(AuxStats);
          end; // FDigestStreamMsg.size>8
        finally
          FLock.Release;
        end;
      end; // Not paused
    end; // while
end;

constructor TCPUOpenSSLMinerThread.Create(CPUDeviceThread : TCPUDeviceThread);
begin
  FCPUDeviceThread := CPUDeviceThread;
  FLock := TCriticalSection.Create;
  FDigestStreamMsg := TMemoryStream.Create;
  FMinNOnce := 0; FMaxNOnce:=$FFFFFFFF;
  inherited Create(false);
end;

destructor TCPUOpenSSLMinerThread.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FDigestStreamMsg);
  inherited Destroy;
end;

end.

