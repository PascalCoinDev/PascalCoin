unit UGPUMining;

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

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, CL_Platform, CL, CL_GL, DelphiCL, dglOpenGL, USha256, UPoolMinerThreads, UPoolMining,
  UThread, UTime, SyncObjs, UCrypto, ULog;

Type

  { TGPUDeviceThread }

  TGPUDeviceThread = Class(TCustomMinerDeviceThread)
  private
    FNeedNewDevice : Boolean;
    FDevice: Integer;
    FPlatform: Integer;
    FLock : TCriticalSection;
    FDCLDevice : TDCLDevice;
    FDCLProgram: TDCLProgram;
    FDCLCommandQueue: TDCLCommandQueue;
    FDCLKernel: TDCLKernel;
    FProgramFileName: String;
    FChangeTimestampAndNOnceBytePos : Integer;
    FKernelOutputBuffer : TDCLBuffer;
    FKernelInputBuffer : TDCLBuffer;
    FKernelArg1 : Array[0..28] of TCL_int;
    FKernelArg2 : TCL_int;
    FReadyToGPU : Boolean;
    procedure SetDevice(AValue: Integer);
    procedure SetPlatform(AValue: Integer);
    procedure SetProgramFileName(AValue: String);
  protected
    procedure BCExecute; override;
    procedure SetMinerValuesForWork(const Value: TMinerValuesForWork); override;
    Procedure UpdateState; override;
    Procedure UpdateBuffers;
  public
    Constructor Create(PoolMinerThread : TPoolMinerThread; InitialMinerValuesForWork : TMinerValuesForWork); override;
    Destructor Destroy; override;
    Property Platform : Integer read FPlatform write SetPlatform;
    Property Device : Integer read FDevice write SetDevice;
    Property ProgramFileName : String read FProgramFileName write SetProgramFileName;
    Function MinerDeviceName : String; override;
    Function GetState : String; override;
  end;

  { TGPUDriver }

  TGPUDriver = Class
  private
    FHasOpenCL: Boolean;
    FPlatforms: TDCLPlatforms;
    function GetPlatforms: TDCLPlatforms;
  public
    Constructor Create;
    Destructor Destroy; override;
    class function GPUDriver : TGPUDriver;
    Property HasOpenCL : Boolean read FHasOpenCL;
    Property Platforms : TDCLPlatforms read GetPlatforms;
  end;



implementation

var _initstatus : Integer;
  _GPUDriver : TGPUDriver;

function bswap(x: Cardinal): Cardinal;
begin
  bswap:=
    ((x and $000000FF) shl 24) +
    ((x and $0000FF00) shl  8) +
    ((x and $00FF0000) shr  8) +
    ((x and $FF000000) shr 24);
end;

{ TGPUDriver }

function TGPUDriver.GetPlatforms: TDCLPlatforms;
begin
  If Not Assigned(FPlatforms) then begin
    If Not FHasOpenCL then Raise Exception.create('No OpenCL available on this computer!');
    FPlatforms := TDCLPlatforms.Create;
  end;
  Result := FPlatforms;
end;

constructor TGPUDriver.Create;
begin
  FPlatforms := Nil;
  FHasOpenCL:=InitOpenCL;
  _GPUDriver := Self;
end;

destructor TGPUDriver.Destroy;
begin
  if _GPUDriver=Self then _GPUDriver := Nil;
  FreeAndNil(FPlatforms);
  inherited Destroy;
end;

class function TGPUDriver.GPUDriver: TGPUDriver;
begin
  If Not assigned(_GPUDriver) then begin
    _GPUDriver := TGPUDriver.Create;
  end;
  Result := _GPUDriver;
end;

{ TGPUDeviceThread }

procedure TGPUDeviceThread.BCExecute;
Const CT_LAPS_ROUND = 16777216; // 2^24 = 16777216 2^22 = 4194304     2^20 = 1048576
      CT_MAX_LAPS = 256; // 2^8 = 256 2^10 = 1024
Var Timestamp, nOnce : Cardinal;
  nLap : Cardinal;
  baseRealTC,baseHashingTC,finalHashingTC,lastNotifyTC : Cardinal;
  AuxStats : TMinerStats;
  cMVFW : TMinerValuesForWork;
begin
  UpdateState;
  nLap := 0;
  AuxStats := CT_TMinerStats_NULL;
  lastNotifyTC :=GetTickCount;
  while Not Terminated do begin
    If (Paused) then begin
      sleep(1);
    end else begin
      baseRealTC := GetTickCount;
      FLock.Acquire;
      try
        cMVFW := MinerValuesForWork;
      //  AuxStats := CT_TMinerStats_NULL;
        If FReadyToGPU then begin
          Timestamp := UnivDateTimeToUnix(DateTime2UnivDateTime(now));
          if Timestamp<=PoolMinerThread.GlobalMinerValuesForWork.timestamp then Timestamp := PoolMinerThread.GlobalMinerValuesForWork.timestamp+1;
          FKernelArg1[ (FChangeTimestampAndNOnceBytePos DIV 4) ] := bswap(Timestamp);
          // FKernelArg1[24] = Position to save nOnce
          FKernelArg1[24] := (FChangeTimestampAndNOnceBytePos DIV 4)+1;
          // FKernelArg1[25] = high-order 10 bits for nOnce (see .cl source file)
          FKernelArg1[25] := nLap;
          //
          FKernelInputBuffer := FDCLDevice.CreateBuffer(29*4, @FKernelArg1[0], [mfReadWrite, mfCopyHostPtr]);
          try
            FDCLKernel.SetArg(0,FKernelInputBuffer);
            FDCLKernel.SetArg(1,FKernelOutputBuffer);
            baseHashingTC := GetTickCount;
            FDCLCommandQueue.Execute(FDCLKernel,CT_LAPS_ROUND);
            finalHashingTC := GetTickCount;
          finally
            FreeAndNil(FKernelInputBuffer);
          end;
          FDCLCommandQueue.ReadBuffer(FKernelOutputBuffer,4,@FKernelArg2);
          If FKernelArg2<>0 then begin
            nOnce := bswap(FKernelArg2);
            FreeAndNil(FKernelOutputBuffer);
            FKernelArg2 := 0; // Save nOnce=0 (not valid)
            FKernelOutputBuffer := FDCLDevice.CreateBuffer(4,@FKernelArg2,[mfReadWrite, mfCopyHostPtr {mfCopyHostPtr mfUseHostPtr}]);
            // FOUND A NONCE !!!
            inc(AuxStats.WinsCount);
            FLock.Release;
            try
              FoundNOnce(cMVFW,Timestamp,nOnce);
            finally
              FLock.Acquire;
            end;
          end;
          if (nLap<CT_MAX_LAPS) then inc(nLap) else nLap := 0;
          inc(AuxStats.RoundsCount,CT_LAPS_ROUND);
        end else sleep(1);
      finally
        FLock.Release;
      end;
      If (AuxStats.RoundsCount>0) then begin
        inc(AuxStats.WorkingMillisecondsTotal,GetTickCount - baseRealTC);
        inc(AuxStats.WorkingMillisecondsHashing,finalHashingTC-baseHashingTC);
      end;
    end;
    If (lastNotifyTC + 200 < GetTickCount) then begin
      sleep(1);
      lastNotifyTC := GetTickCount;
      UpdateDeviceStats(AuxStats);
      AuxStats := CT_TMinerStats_NULL;
    end;
  end;
end;

constructor TGPUDeviceThread.Create(PoolMinerThread: TPoolMinerThread; InitialMinerValuesForWork: TMinerValuesForWork);
begin
  FReadyToGPU := false;
  FDevice:=-1;
  FPlatform:=-1;
  FNeedNewDevice:=false;
  FProgramFileName:='';
  FLock := TCriticalSection.Create;
  FDCLDevice := Nil;
  FDCLProgram := Nil;
  FDCLCommandQueue := Nil;
  FDCLKernel := Nil;
  FKernelInputBuffer := Nil;
  FKernelOutputBuffer := Nil;
  inherited Create(PoolMinerThread, InitialMinerValuesForWork);
end;

destructor TGPUDeviceThread.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FDCLCommandQueue);
  FreeAndNil(FKernelOutputBuffer);
  FreeAndNil(FKernelInputBuffer);
  FreeAndNil(FDCLKernel);
  FreeAndNil(FDCLProgram);
  inherited Destroy;
end;

function TGPUDeviceThread.GetState: String;
begin
  If Paused then result := 'GPU miner is paused'
  else if (IsMining) And Assigned(FDCLDevice) then Result := 'GPU is mining on p '+IntToStr(Platform)+' d '+IntToStr(Device)+' Compute units:'+IntToStr(FDCLDevice.MaxComputeUnits)+' Freq:'+IntToStr(FDCLDevice.MaxClockFrequency)
  else Result := 'GPU miner is waiting for configuration...';
end;

function TGPUDeviceThread.MinerDeviceName: String;
begin
  Result := 'GPU p'+inttostr(FPlatform)+' d'+IntToStr(FDevice);
  If assigned(FDCLDevice) then begin
    Result := Result+' Name:'+Trim(FDCLDevice.Name)+' CU:'+IntToStr(FDCLDevice.MaxComputeUnits)+' Freq:'+IntToStr(FDCLDevice.MaxClockFrequency);
  end else Result := Result + ' (no info)';
end;

procedure TGPUDeviceThread.SetDevice(AValue: Integer);
begin
  if FDevice=AValue then Exit;
  FDevice:=AValue;
  FNeedNewDevice := true;
  UpdateState;
end;

procedure TGPUDeviceThread.SetMinerValuesForWork(const Value: TMinerValuesForWork);
begin
  inherited;
  UpdateBuffers;
end;

procedure TGPUDeviceThread.SetPlatform(AValue: Integer);
begin
  if FPlatform=AValue then Exit;
  FPlatform:=AValue;
  FNeedNewDevice := true;
  UpdateState;
end;

procedure TGPUDeviceThread.SetProgramFileName(AValue: String);
begin
  if FProgramFileName=AValue then Exit;
  FProgramFileName:=AValue;
  FNeedNewDevice:=true;
  UpdateState;
end;

procedure TGPUDeviceThread.UpdateState;
begin
  FLock.Acquire;
  try
    If FNeedNewDevice then begin
      FDCLDevice := Nil;
      FreeAndNil(FDCLCommandQueue);
      FreeAndNil(FKernelInputBuffer);
      FreeAndNil(FKernelOutputBuffer);
      FreeAndNil(FDCLKernel);
      FreeAndNil(FDCLProgram);
      FNeedNewDevice:=false;
      If (FDevice>=0) And (FPlatform>=0) And (Assigned(_GPUDriver)) then begin
        If (_GPUDriver.platforms.PlatformCount>=FPlatform) then begin
          If (_GPUDriver.platforms.Platforms[FPlatform].DeviceCount>=FDevice) then begin
            FDCLDevice := _GPUDriver.Platforms.Platforms[FPlatform].Devices[FDevice]^;
          end;
        end;
      end;
      If Assigned(FDCLDevice) And (FProgramFileName<>'') And (FileExists(FProgramFileName)) then begin
        FDCLCommandQueue := FDCLDevice.CreateCommandQueue;
        FDCLProgram := FDCLDevice.CreateProgram(FProgramFileName);
        FKernelArg2 := 0; // Save nOnce=0 (not valid)
        FKernelOutputBuffer := FDCLDevice.CreateBuffer(4,@FKernelArg2,[mfReadWrite, mfCopyHostPtr {mfCopyHostPtr mfUseHostPtr}]);
        FDCLKernel := FDCLProgram.CreateKernel('pascalcoin');
      end;
    end;
    UpdateBuffers;
  finally
    FLock.Release;
  end;
  inherited;
end;

procedure TGPUDeviceThread.UpdateBuffers;
Var stateforlastchunk : TSHA256HASH;
  bufferForLastChunk : TChunk;
  i : Integer;
  digest,raw : TBytes;
  b : Byte;
  c1,c2 : Cardinal;
begin
  FLock.Acquire;
  try
    FReadyToGPU := (Length(MinerValuesForWork.part1)>0) And (Assigned(FDCLKernel));
    if (FReadyToGPU) And (TPoolMinerThread.UseRandomHash(MinerValuesForWork.version)) then begin
      // V4 RandomHash not available on GPU mining
      FReadyToGPU:=False;
      TLog.NewLog(ltError,ClassName,'RandomHash not available on GPU. Set to CPU');
    end;
    If (Not FReadyToGPU) then begin
      IsMining := false;
      exit;
    end;
    UpdateMinerValuesForWorkLength(FMinerValuesForWork,FChangeTimestampAndNOnceBytePos);
    FillChar(FKernelArg1[0],29*4,#0);
    CreateDigest(MinerValuesForWork,0,0,digest);
    PascalCoinPrepareLastChunk(digest,stateforlastchunk,bufferForLastChunk);
    // FKernelArg1[0..15] = data for last chunk
    move(bufferForLastChunk[0],FKernelArg1[0],16*4);
    For i:=0 to 15 do begin
      FKernelArg1[i] := bswap(FKernelArg1[i]);
    end;
    // FKernelArg1[16..23] = previous chunk result
    move(stateforlastchunk[0],FKernelArg1[16],8*4);
    // FKernelArg1[24] = Position to save nOnce
    // FKernelArg1[25] = high-order 12 bits for nOnce (see .cl file to know)
    // FKernelArg1[26..28] = Mask (obtained  from target_pow)
    FillChar(FKernelArg1[26],4*3,#0);
    i := 0;
    while (High(MinerValuesForWork.target_pow)>=i) And (i<4*3) do begin
      b := MinerValuesForWork.target_pow[i];
      b := b XOR $FF;
      c1 := FKernelArg1[26+(i DIV 4)]; // Last value
      c2 := b SHL ((3-(i MOD 4))*8);
      c2 := c1 OR c2;
      FKernelArg1[26+(i DIV 4)] := c2;
      if (b<>$FF) then break; // Found first 1 bit
      inc(i);
    end;
    IsMining := true;
  finally
    FLock.Release;
  end;
end;

initialization
  _initstatus := 0;
finalization
  FreeAndNil(_GPUDriver);
end.

