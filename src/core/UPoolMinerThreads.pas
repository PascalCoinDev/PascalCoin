unit UPoolMinerThreads;

{ Copyright (c) 2017 by Albert Molina

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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

{$I config.inc}

uses
  Classes, SysUtils, syncobjs, UThread, UPoolMining, UAccounts, UCrypto, ULog, UBlockChain, USha256, URandomHash, UBaseTypes, UCommon,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

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
    FDevicesList : TPCThreadList<TCustomMinerDeviceThread>;
    FMinerThreads: Integer;
    FGlobalMinerValuesForWork : TMinerValuesForWork;
    FTestingPoWLeftBits: Byte;
    FTestingMode: Boolean;
    Procedure OnPoolMinerClientConnectionChanged(Sender : TObject);
    Procedure OnPoolMinerMustChangeValues(Sender : TObject);
    Procedure OnMinerNewBlockFound(sender : TCustomMinerDeviceThread; const usedMinerValuesForWork : TMinerValuesForWork; Timestamp : Cardinal; NOnce : Cardinal);
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
    Function DevicesLock : TList<TCustomMinerDeviceThread>;
    procedure DevicesUnlock;
    Property MinerAddName : String read FMinerAddName write SetMinerAddName;
    Property TestingPoWLeftBits : Byte read FTestingPoWLeftBits write SetTestingPoWLeftBits;
    Property TestingMode : Boolean read FTestingMode write FTestingMode;
    class Function UseRandomHash(const AProtocolVersion : Word) : Boolean;
  End;

  { TCustomMinerDeviceThread }

  TCustomMinerDeviceThread = Class(TPCThread)
  private
    FIsMining: Boolean;
    FOnFoundNOnce: TOnFoundNonce;
    FOnMinerValuesChanged: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;
    FPaused: Boolean;
    FLastStats : TPCThreadList<Pointer>;
    FLastActiveTC : TTickCount;
    FGlobaDeviceStats : TMinerStats;
    FPartialDeviceStats : TMinerStats;
    FPoolMinerThread : TPoolMinerThread;
    FLastDigest : TRawBytes;
    procedure SetIsMining(AValue: Boolean);
    procedure SetPaused(AValue: Boolean);
  protected
    FMinerValuesForWork : TMinerValuesForWork;
    Procedure SetMinerValuesForWork(const Value : TMinerValuesForWork); virtual;
    Procedure UpdateState; virtual;
    Procedure UpdateDeviceStats(Stats : TMinerStats); virtual;
    Procedure FoundNOnce(const usedMinerValuesForWork : TMinerValuesForWork; Timestamp,nOnce : Cardinal);
    procedure CreateDigest(const AMinerValuesForWork : TMinerValuesForWork; Timestamp,nOnce : Cardinal; var ODigest : TRawBytes); overload;
    procedure UpdateMinerValuesForWorkLength(var OMinerValuesForWork : TMinerValuesForWork; out OChangeTimestampAndNOnceBytePos: Integer);
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

  TCPUOpenSSLMinerThread = Class;

  { TCPUDeviceThread }

  TCPUDeviceThread = Class(TCustomMinerDeviceThread)
  private
    FCPUs: Integer;
    FCPUsThreads : TPCThreadList<TCPUOpenSSLMinerThread>;
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
    FCurrentMinerValuesForWork : TMinerValuesForWork;
    FInternalSha256 : TSHA256HASH;
    FInternalChunk : TChunk;
    FDigestMsg : TRawBytes;
    FChangeTimestampAndNOnceBytePos : Integer;
    FDigestStreamMsg : TMemoryStream;
    FMinNOnce,FMaxNOnce : Cardinal;
    FResetNOnce : Boolean;						  
    procedure BCExecute; override;
  public
    Constructor Create(CPUDeviceThread : TCPUDeviceThread);
    Destructor Destroy; override;
  End;

implementation

uses UConst, UTime, UJSONFunctions, UNode, UNetProtocol, UMemory;

{ TPoolMinerThread }

procedure TPoolMinerThread.BCExecute;
Var nID : Cardinal;
  json : TPCJSONObject;
  i : Integer;
  ResponseMethod : String;
  l : TList<TCustomMinerDeviceThread>;
begin
  DebugStep:='Starting';
  Try
    while not Terminated do begin
      if FTestingMode then begin
      end else if not FPoolMinerClient.Connected then begin
        DebugStep:='Not connected';
        If Not FPoolMinerClient.Connect then begin
        end else begin
          TLog.NewLog(ltinfo,ClassName,'Starting connection to '+FPoolMinerClient.ClientRemoteAddr);
        end;
      end else begin
          DebugStep:='Starting process';
          // Start Process
          nId:=FPoolMinerClient.GetNewId;
          FPoolMinerClient.SendJSONRPCMethod(CT_PoolMining_Method_MINER_NOTIFY,nil,nId);
          json := TPCJSONObject.create;
          try
            DebugStep:='Starting repeat';
            repeat
              if (FPoolMinerClient.DoProcessBuffer(Self,1000,true,ResponseMethod,json)) then begin
                FPoolMinerClient.DoProcessJSONObject(json,ResponseMethod);
                json.Clear;
              end;
            until (terminated) or (Not FPoolMinerClient.Connected);
          finally
            json.Free;
          end;
          DebugStep:='Disconnecting';
          FPoolMinerClient.Disconnect;
      end;
      sleep(1);
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
  FDevicesList := TPCThreadList<TCustomMinerDeviceThread>.Create('TPoolMinerThread_DevicesList');
  FMinerThreads := 0;
  FMinerAddName:='';
  FTestingPoWLeftBits := 0;
  FTestingMode := False;
  inherited Create(false);
end;

function TPoolMinerThread.CurrentMinerStats: TMinerStats;
Var l : TList<TCustomMinerDeviceThread>;
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
  l : TList<TCustomMinerDeviceThread>;
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

function TPoolMinerThread.DevicesLock: TList<TCustomMinerDeviceThread>;
begin
  Result := FDevicesList.LockList;
end;

procedure TPoolMinerThread.DevicesUnlock;
begin
  FDevicesList.UnlockList;
end;

class function TPoolMinerThread.UseRandomHash(const AProtocolVersion : Word) : Boolean;
begin
  Result := (CT_ACTIVATE_RANDOMHASH_V4 AND (AProtocolVersion >= CT_PROTOCOL_4));
end;

function TPoolMinerThread.GlobalMinerStats: TMinerStats;
Var l : TList<TCustomMinerDeviceThread>;
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

procedure TPoolMinerThread.OnMinerNewBlockFound(sender : TCustomMinerDeviceThread; const usedMinerValuesForWork : TMinerValuesForWork; Timestamp : Cardinal; NOnce : Cardinal);
begin
  FDevicesList.LockList;
  try
    TLog.NewLog(ltinfo,ClassName,'FOUND VALID NONCE!!! Block:'+IntToStr(usedMinerValuesForWork.block)+' Timestamp:'+Inttostr(Timestamp)+ ' Nonce:'+Inttostr(NOnce)+' Payload:'+usedMinerValuesForWork.payload_start.ToString);
    FPoolMinerClient.SubmitBlockFound(usedMinerValuesForWork,usedMinerValuesForWork.payload_start,Timestamp,NOnce);
  finally
    FDevicesList.UnlockList;
  end;
end;

procedure TPoolMinerThread.OnPoolMinerClientConnectionChanged(Sender: TObject);
Var l : TList<TCustomMinerDeviceThread>;
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
  NotifyPoolMinerConnectionChanged;
end;

procedure TPoolMinerThread.OnPoolMinerMustChangeValues(Sender: TObject);
Var l : TList<TCustomMinerDeviceThread>;
  i,j : Integer;
  digest_length : Integer;
  ok : Boolean;
  minervfw : TMinerValuesForWork;
  auxminervfw : TMinerValuesForWork;
  auxRaw : TRawBytes;
begin
  FGlobalMinerValuesForWork := FPoolMinerClient.MinerValuesForWork;
  TLog.NewLog(ltupdate,ClassName,Format('New miner values. Block %d Target %s Payload %s',[FPoolMinerClient.MinerValuesForWork.block,
    IntToHex(FPoolMinerClient.MinerValuesForWork.target,8), FPoolMinerClient.MinerValuesForWork.payload_start.ToPrintable]));
  l := FDevicesList.LockList;
  Try
    for i := 0 to l.Count - 1 do begin
      minervfw := FGlobalMinerValuesForWork;
      auxRaw.FromString(FMinerAddName);
      TBaseType.Concat(minervfw.payload_start,auxRaw,minervfw.payload_start);
      if (l.count>1) then begin
        auxRaw.FromString(inttostr(i));
        TBaseType.Concat(minervfw.payload_start,auxRaw,minervfw.payload_start);
      end;
      if Not TPoolMinerThread.UseRandomHash(minervfw.version) then begin
        repeat // Prepare payload_start with SHA256 compatible last chunk size
          digest_length := Length(minervfw.part1) + Length(minervfw.payload_start) + Length(minervfw.part3) + 8; // 8 bytes for Timestamp(4) and Nonce(4)
          ok := CanBeModifiedOnLastChunk(digest_length,j);
          if (not ok) then begin
            SetLength(minervfw.payload_start,Length(minervfw.payload_start)+1);
            minervfw.payload_start[High(minervfw.payload_start)] := Byte('-');
          end;
        until (Ok);
      end;
      If FTestingPoWLeftBits>0 then begin
        auxminervfw := minervfw;
        auxminervfw.target:= ((((auxminervfw.target AND $FF000000) SHR 24)-FTestingPoWLeftBits) SHL 24) + (minervfw.target AND $00FFFFFF);
        If auxminervfw.target<(TPascalCoinProtocol.MinimumTarget(minervfw.version)) then auxminervfw.target:=TPascalCoinProtocol.MinimumTarget(minervfw.version);
        auxminervfw.target_pow:=TPascalCoinProtocol.TargetFromCompact(auxminervfw.target,minervfw.version);
        TCustomMinerDeviceThread(l[i]).SetMinerValuesForWork(auxminervfw);
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
  FLastStats := TPCThreadList<Pointer>.Create('TCustomMinerDeviceThread_LastStats');
  FOnFoundNOnce:=Nil;
  FOnMinerValuesChanged:=Nil;
  FOnStateChanged:=Nil;
  FPaused:=true;
  FLastActiveTC := 0;
  FLastDigest := Nil;
  SetMinerValuesForWork(InitialMinerValuesForWork);
  PoolMinerThread.FDevicesList.Add(Self);
  inherited Create(false);
end;

destructor TCustomMinerDeviceThread.Destroy;
Var i : Integer;
  P : PTimeMinerStats;
  ldevices : TList<TCustomMinerDeviceThread>;
  lstats : TList<Pointer>;
begin
  ldevices := FPoolMinerThread.FDevicesList.LockList;
  try
    ldevices.Remove(Self);
  finally
    FPoolMinerThread.FDevicesList.UnlockList;
  end;
  lstats := FLastStats.LockList;
  try
    for i:=0 to lstats.Count-1 do begin
      P := lstats[i];
      Dispose(P);
    end;
    lstats.clear;
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

procedure TCustomMinerDeviceThread.FoundNOnce(const usedMinerValuesForWork : TMinerValuesForWork; Timestamp, nOnce: Cardinal);
var
  digest,LHash  : TRawBytes;
  LUseRandomHash : Boolean;
begin
  // Validation
  CreateDigest(usedMinerValuesForWork,Timestamp,nOnce,digest);
  if TBaseType.Equals(digest,FLastDigest) then Exit;
  FLastDigest := digest;
  LUseRandomHash := TPoolMinerThread.UseRandomHash(usedMinerValuesForWork.version);
  if LUseRandomHash then
    LHash := TCrypto.DoRandomHash(digest)
  else
    LHash := TCrypto.DoSha256(TCrypto.DoSha256(digest));
  if (TBaseType.BinStrComp(LHash,usedMinerValuesForWork.target_pow)<=0) then begin
    inc(FGlobaDeviceStats.WinsCount);
    FPoolMinerThread.OnMinerNewBlockFound(self,usedMinerValuesForWork,Timestamp,nOnce);
    If Assigned(FOnFoundNOnce) then FOnFoundNOnce(Self,Timestamp,nOnce);
  end else begin
    inc(FGlobaDeviceStats.Invalids);
    if LUseRandomHash then
      TLog.NewLog(lterror,Self.Classname,Format('Invalid RandomHash found. Timestamp %s nOnce %s RNDHASH %s Valid POW %s',
        [IntToHex(Timestamp,8),IntToHex(nOnce,8),TCrypto.ToHexaString(LHash),TCrypto.ToHexaString(usedMinerValuesForWork.target_pow)]))
    else
      TLog.NewLog(lterror,Self.Classname,Format('Invalid SHA2-256D found. Timestamp %s nOnce %s DSHA256 %s Valid POW %s',
        [IntToHex(Timestamp,8),IntToHex(nOnce,8),TCrypto.ToHexaString(LHash),TCrypto.ToHexaString(usedMinerValuesForWork.target_pow)]));
  end;
end;

procedure TCustomMinerDeviceThread.CreateDigest(const AMinerValuesForWork: TMinerValuesForWork; Timestamp, nOnce: Cardinal; var ODigest: TRawBytes);
begin
  // Validation
  SetLength(ODigest,Length(AMinerValuesForWork.part1) + Length(AMinerValuesForWork.payload_start) + Length(AMinerValuesForWork.part3) + 8);
  move(AMinerValuesForWork.part1[0],
    ODigest[0],
    Length(AMinerValuesForWork.part1));
  move(AMinerValuesForWork.payload_start[0],
    ODigest[Length(AMinerValuesForWork.part1)],
    Length(AMinerValuesForWork.payload_start));
  move(AMinerValuesForWork.part3[0],
    ODigest[ Length(AMinerValuesForWork.part1) + Length(AMinerValuesForWork.payload_start) ],
    Length(AMinerValuesForWork.part3));
  // Add timestamp and nonce
  move(Timestamp,ODigest[length(ODigest)-8],4);
  move(nOnce,ODigest[length(ODigest)-4],4);
end;

procedure TCustomMinerDeviceThread.UpdateMinerValuesForWorkLength(var OMinerValuesForWork: TMinerValuesForWork; out OChangeTimestampAndNOnceBytePos: Integer);
var i : Integer;
  isOk : Boolean;
begin
  if Not TPoolMinerThread.UseRandomHash(OMinerValuesForWork.version) then begin
    repeat // Prepare payload_start with SHA256 compatible last chunk size
      i := Length(OMinerValuesForWork.part1)+Length(OMinerValuesForWork.payload_start)+Length(OMinerValuesForWork.part3)+8;
      isOk := CanBeModifiedOnLastChunk(i,OChangeTimestampAndNOnceBytePos);
      If Not isOk then begin
        SetLength(OMinerValuesForWork.payload_start,Length(OMinerValuesForWork.payload_start)+1);
        OMinerValuesForWork.payload_start[High(OMinerValuesForWork.payload_start)] := Byte('.');
      end;
    until (isOk);
  end else OChangeTimestampAndNOnceBytePos:=0;
end;

function TCustomMinerDeviceThread.GlobalDeviceStats: TMinerStats;
Var g : TMinerStats;
begin
  FLastStats.LockList;
  try
    g := FGlobaDeviceStats;
    If Not FPaused then begin
      g.WorkingMillisecondsHashing:= g.WorkingMillisecondsHashing + (TPlatform.GetTickCount - FLastActiveTC);
      g.WorkingMillisecondsTotal:= g.WorkingMillisecondsTotal + (TPlatform.GetTickCount - FLastActiveTC);
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
var aux : Integer;
begin
  FMinerValuesForWork := Value;
  UpdateMinerValuesForWorkLength(FMinerValuesForWork,aux);
  TLog.NewLog(ltinfo,classname,Format('Updated MinerValuesForWork: Target:%s Payload:%s Target_PoW:%s',[IntToHex(FMinerValuesForWork.target,8),FMinerValuesForWork.payload_start.ToString,TCrypto.ToHexaString(FMinerValuesForWork.target_pow)]));
  If Assigned(FOnMinerValuesChanged) then FOnMinerValuesChanged(Self);
end;

procedure TCustomMinerDeviceThread.SetPaused(AValue: Boolean);
begin
  if FPaused=AValue then Exit;
  FPaused:=AValue;
  If Not FPaused then FLastActiveTC := TPlatform.GetTickCount
  else begin
    FGlobaDeviceStats.WorkingMillisecondsHashing:=FGlobaDeviceStats.WorkingMillisecondsHashing + (TPlatform.GetTickCount - FLastActiveTC);
    FGlobaDeviceStats.WorkingMillisecondsTotal:=FGlobaDeviceStats.WorkingMillisecondsTotal + (TPlatform.GetTickCount - FLastActiveTC);
  end;
  UpdateState;
end;

procedure TCustomMinerDeviceThread.UpdateDeviceStats(Stats: TMinerStats);
Type TTimeMinerStats = Record
       tc : Cardinal;
       stats : TMinerStats;
     end;
  PTimeMinerStats = ^TTimeMinerStats;
Var l : TList<Pointer>;
  i : Integer;
  P : PTimeMinerStats;
  minTC : TTickCount;
  foundMaxMiners : Cardinal;
begin
  l := FLastStats.LockList;
  Try
    FPartialDeviceStats := CT_TMinerStats_NULL;
    New(P);
    P^.tc:=(TPlatform.GetTickCount - stats.WorkingMillisecondsTotal);
    P^.stats:=stats;
    l.add(P);
    minTC := TPlatform.GetTickCount - 10000; // Last 10 seconds average
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
  FCPUs:=0;
  CheckCPUs;
end;

procedure TCPUDeviceThread.CheckCPUs;
var l : TList<TCPUOpenSSLMinerThread>;
  mt : TCPUOpenSSLMinerThread;
  needminers : Integer;
begin
  needminers := FCPUs;
  if (Length(FMinerValuesForWork.part1)=0) or (FPaused) then needminers := 0;
  If (Not FPoolMinerThread.TestingMode) And
     (Not FPoolMinerThread.PoolMinerClient.Connected) then needminers := 0;
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
  FCPUsThreads := TPCThreadList<TCPUOpenSSLMinerThread>.Create('TCPUDeviceThread_CPUsThreads');
  FCPUs:=0;
  FUseOpenSSLFunctions := True;
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
  Result := 'CPU miner with '+inttostr(FCPUs)+' ('+inttostr(TLogicalCPUCount.GetLogicalCPUCount())+' CPU''s available)';
end;

procedure TCPUDeviceThread.SetCPUs(AValue: Integer);
begin
  if FCPUs=AValue then Exit;
  FCPUs:=AValue;
  if FCPUs<0 then FCPUs := 0;
  if (FCPUs>TLogicalCPUCount.GetLogicalCPUCount()) And (TLogicalCPUCount.GetLogicalCPUCount()>0) then FCPUs := TLogicalCPUCount.GetLogicalCPUCount();
  CheckCPUs;
end;

procedure TCPUDeviceThread.SetMinerValuesForWork(const Value: TMinerValuesForWork);
Var l : TList<TCPUOpenSSLMinerThread>;
  i : Integer;
  nextmin : Cardinal;
  npos : Integer;
  cpu : TCPUOpenSSLMinerThread;
  digest : TRawBytes;
  sflc : TSHA256HASH;
  lc : TChunk;
begin
  l := FCPUsThreads.LockList;
  try
    // Prepare final data:
    inherited;
    CheckCPUs;
    UpdateMinerValuesForWorkLength(FMinerValuesForWork,npos);
    CreateDigest(FMinerValuesForWork,0,0,digest);
    PascalCoinPrepareLastChunk(digest,sflc,lc);
    nextmin := 0;
    for i:=0 to l.count-1 do begin
      cpu := TCPUOpenSSLMinerThread(l[i]);
      cpu.FLock.Acquire;
      try
        cpu.FCurrentMinerValuesForWork := FMinerValuesForWork;
        cpu.FInternalSha256 := sflc;
        cpu.FInternalChunk := lc;
        cpu.FDigestMsg:=digest;
        cpu.FDigestStreamMsg.size := 0;
        cpu.FChangeTimestampAndNOnceBytePos:=npos;
        cpu.FMinNOnce:=nextmin;
        cpu.FResetNOnce:=True;
        if (FCPUs>0) then cpu.FMaxNOnce:=nextmin + (Cardinal($FFFFFFFF) DIV FCPUs) - 1
        else cpu.FMaxNOnce:= nextmin + (Cardinal($FFFFFFFF)) - 1;
        nextmin := cpu.FMaxNOnce+1;
        cpu.FDigestStreamMsg.WriteBuffer(digest[Low(digest)],Length(digest));
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
Var
  ts : Cardinal;
  i,roundsToDo : Integer;
  nonce : Cardinal;
  baseRealTC,baseHashingTC,finalHashingTC : TTickCount;
  resultPoW : TRawBytes;
  //
  AuxStats : TMinerStats;
  dstep : Integer;
  LUseRandomHash : boolean;
  LRandomHasher : TRandomHashFast;
  LDisposables : TDisposables;
begin
  DebugStep := '----------';
  AuxStats := CT_TMinerStats_NULL;
  nonce := 0;
  dstep := 0;
  LRandomHasher := LDisposables.AddObject( TRandomHashFast.Create ) as TRandomHashFast;
  Try
    while (Not Terminated) And (Not FCPUDeviceThread.Terminated) do begin
      Try
      sleep(1);
      dstep := 1;
      AuxStats := CT_TMinerStats_NULL;
      If (FCPUDeviceThread.Paused) then sleep(1)
      else begin
        dstep := 2;
        FLock.Acquire;
        try
          LUseRandomHash := TPoolMinerThread.UseRandomHash(FCurrentMinerValuesForWork.version);
          if (LUseRandomHash) then begin
            roundsToDo := 20;
          end else begin
            roundsToDo := 10000;
          end;
          baseRealTC := TPlatform.GetTickCount;
          If (FResetNOnce) then begin
            FResetNOnce := False;
            If (nonce<FMinNOnce) Or (nonce>FMaxNOnce) then begin
              nonce:=FMinNOnce;
            end;
          end;
          // Timestamp
          ts := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
          if ts<=FCurrentMinerValuesForWork.timestamp then ts := FCurrentMinerValuesForWork.timestamp+1;

          If FDigestStreamMsg.Size>8 then begin
            if FCPUDeviceThread.FUseOpenSSLFunctions OR LUseRandomHash then begin
              FDigestStreamMsg.Position:=FDigestStreamMsg.Size - 8;
              FDigestStreamMsg.Write(ts,4);
              baseHashingTC:=TPlatform.GetTickCount;
              dstep := 4;
              for i := 1 to roundsToDo do begin
                FDigestStreamMsg.Position := FDigestStreamMsg.Size - 4;
                FDigestStreamMsg.Write(nonce,4);
                if LUseRandomHash then begin
                  // Note if i > 1 then FDigestStreamMsg.Memory == LHasher.NextHeader (needs to be for CPU optimization to work)
                  TCrypto.DoRandomHash(LRandomHasher,FDigestStreamMsg.Memory,FDigestStreamMsg.Size,resultPoW);
                end else
                  TCrypto.DoDoubleSha256(FDigestStreamMsg.Memory,FDigestStreamMsg.Size,resultPoW);
                if (TBaseType.BinStrComp(resultPoW,FCurrentMinerValuesForWork.target_pow)<0) then begin
                  if (Terminated) Or (FCPUDeviceThread.Terminated) then exit;
                  dstep := 5;
                  FLock.Release;
                  try
                    dstep := 6;
                    FCPUDeviceThread.FoundNOnce(FCurrentMinerValuesForWork, ts, nonce);
                    dstep := 7;
                  finally
                    FLock.Acquire;
                  end;
                  dstep := 8;
                end;
                if LUseRandomHash then
                  nonce := LRandomHasher.NextNonce
                else if (nonce)<FMaxNOnce then inc(nonce) else nonce := FMinNOnce;
              end;
              finalHashingTC:=TPlatform.GetTickCount;
            end else begin
              baseHashingTC:=TPlatform.GetTickCount;
              for i := 1 to roundsToDo do begin
                PascalCoinExecuteLastChunkAndDoSha256(FInternalSha256,FInternalChunk,FChangeTimestampAndNOnceBytePos,nonce,ts,resultPoW); // Note: RandomHash is handled above
                if (TBaseType.BinStrComp(resultPoW,FCurrentMinerValuesForWork.target_pow)<0) then begin
                  if Terminated then exit;
                  FLock.Release;
                  try
                    FCPUDeviceThread.FoundNOnce(FCurrentMinerValuesForWork, ts,nonce);
                  finally
                    FLock.Acquire;
                  end;
                end;
                if (nonce)<FMaxNOnce then inc(nonce) else nonce := FMinNOnce;
              end;
              finalHashingTC:=TPlatform.GetTickCount;
            end;
            AuxStats.Miners:=FCPUDeviceThread.FCPUs;
            AuxStats.RoundsCount:=roundsToDo;
            AuxStats.WorkingMillisecondsTotal:=TPlatform.GetTickCount - baseRealTC;
            AuxStats.WorkingMillisecondsHashing:= finalHashingTC - baseHashingTC;
            dstep := 9;
            FCPUDeviceThread.UpdateDeviceStats(AuxStats);
          end; // FDigestStreamMsg.size>8
        finally
          FLock.Release;
        end;
      end; // Not paused
      Except
        On E:Exception do begin
          TLog.NewLog(ltError,ClassName,'EXCEPTION step:'+IntToStr(dstep)+' ' +E.ClassName+':'+E.Message);
        end;
      end;
    end; // while
  Finally
    DebugStep := IntToStr(dstep);
  End;
end;

constructor TCPUOpenSSLMinerThread.Create(CPUDeviceThread : TCPUDeviceThread);
begin
  FCPUDeviceThread := CPUDeviceThread;
  FLock := TCriticalSection.Create;
  FDigestStreamMsg := TMemoryStream.Create;
  FMinNOnce := 0; FMaxNOnce:=$FFFFFFFF;
  FResetNOnce:=True;
  inherited Create(false);
end;

destructor TCPUOpenSSLMinerThread.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FDigestStreamMsg);
  inherited Destroy;
end;

end.

