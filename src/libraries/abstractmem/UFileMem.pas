unit UFileMem;

{
  This file is part of AbstractMem framework

  Copyright (C) 2020-2021 Albert Molina - bpascalblockchain@gmail.com

  https://github.com/PascalCoinDev/

  *** BEGIN LICENSE BLOCK *****

  The contents of this files are subject to the Mozilla Public License Version
  2.0 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Initial Developer of the Original Code is Albert Molina.

  See ConfigAbstractMem.inc file for more info

  ***** END LICENSE BLOCK *****
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  SyncObjs,
  UAbstractMem, UCacheMem;

{$I ./ConfigAbstractMem.inc }

type
  EFileMem = Class(Exception);

  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  TFileMemStats = record
    ReadsCount : Integer;
    WriteCount : Integer;
    ReadsBytesCount : Integer;
    WriteBytesCount : Integer;
    IncreaseSizeCount : Integer;
    IncreaseSizeBytesCount : Integer;
    function ToString : String;
    procedure Clear;
  end;
  {$ENDIF}

  TFileMem = Class(TAbstractMem)
  private
    FFileStream : TFileStream;
    FCache : TCacheMem;
    FFileName: String;
    FIsStableCache: Boolean;
    FIsFlushingCache : Boolean;
    FUnstableMarkerPending: Boolean;
    FUncachedWriteFailed, FResizeFailed: Boolean;
    FFailedWriteStart, FFailedWriteEnd: Int64;
    FIncreaseFileBytes: Int64;
    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    FStats : TFileMemStats;
    {$ENDIF}
    function OnCacheNeedDataProc(var ABuffer; AStartPos : Int64; ASize: Integer): Integer;
    function OnCacheSaveDataProc(const ABuffer; AStartPos : Int64; ASize: Integer): Integer;
    procedure SetMaxCacheSize(const Value: Int64);
    function GetMaxCacheSize: Int64;
    function GetMaxCacheDataBlocks: Int64;
    procedure SetMaxCacheDataBlocks(const Value: Int64);
    procedure CacheIsNOTStable;
    procedure FlushFile;
    procedure RecordFailedWrite(APosition: Int64; ASize: Integer);
    procedure ReportCloseFailure(AError: Exception);
    function GetUseCache: Boolean;
    procedure SetUseCache(const Value: Boolean);
    procedure SetIncreaseFileBytes(const Value: Int64);
    function GetGridCache: Boolean;
    procedure SetDefaultCacheDataBlocksSize(const Value: Int64);
    procedure SetGridCache(const Value: Boolean);
    function GetDefaultCacheDataBlocksSize: Int64;
  protected
    // Owners of a partially updated logical index can preserve its unstable
    // marker instead of retrying incomplete cached work during destruction.
    function ShouldFlushOnDestroy: Boolean; virtual;
    function AbsoluteWrite(const AAbsolutePosition : Int64; const ABuffer; ASize : Integer) : Integer; override;
    function AbsoluteRead(const AAbsolutePosition : Int64; var ABuffer; ASize : Integer) : Integer; override;
    procedure DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos : Int64; ANeedSize : Integer); override;
    function IsAbstractMemInfoStable : Boolean; override;
  public
    Constructor Create(const AFileName : String; AReadOnly : Boolean); reintroduce;
    Destructor Destroy; override;
    function New(AMemSize : TAbstractMemSize) : TAMZone; override;
    function Write(const APosition : Int64; const ABuffer; ASize : Integer) : Integer; overload; override;
    function Read(const APosition : Int64; var ABuffer; ASize : Integer) : Integer; overload; override;
    {$IFDEF ABSTRACTMEM_TESTING_MODE}
    // Warning: Accessing Cache is not Safe Thread protected, use LockCache/UnlockCache instead
    property Cache : TCacheMem read FCache;
    {$ENDIF}
    procedure SetCachePerformance(AGridCache : Boolean; ADefaultCacheDataBlocksSize, AMaxCacheSize, AMaxCacheDataBlocks : Int64);
    property GridCache : Boolean read GetGridCache write SetGridCache;
    property DefaultCacheDataBlocksSize : Int64 read GetDefaultCacheDataBlocksSize write SetDefaultCacheDataBlocksSize;
    property MaxCacheSize : Int64 read GetMaxCacheSize write SetMaxCacheSize;
    property MaxCacheDataBlocks : Int64 read GetMaxCacheDataBlocks write SetMaxCacheDataBlocks;
    Function FlushCache : Boolean;
    //
    function LockCache : TCacheMem;
    procedure UnlockCache;
    property FileName : String read FFileName;
    property UseCache : Boolean read GetUseCache write SetUseCache;
    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    function GetStatsReport(AClearStats : Boolean) : String; override;
    {$ENDIF}
    property IncreaseFileBytes : Int64 read FIncreaseFileBytes write SetIncreaseFileBytes;
  End;

implementation

{$IFNDEF FPC}
uses
  {$IFDEF MSWINDOWS}Windows{$ELSE}Posix.Unistd{$ENDIF};
{$ENDIF}

{$IFDEF ABSTRACTMEM_ENABLE_STATS}
{ TFileMemStats }

function TFileMemStats.ToString: String;
begin
  Result := Format('FileMemStats Reads:%d (%d b) Writes:%d (%d b) Increases:%d (%d b)',
    [Self.ReadsCount,Self.ReadsBytesCount,
     Self.WriteCount,Self.WriteBytesCount,
     Self.IncreaseSizeCount,Self.IncreaseSizeBytesCount
     ]);
end;

procedure TFileMemStats.Clear;
begin
  Self.ReadsCount := 0;
  Self.WriteCount := 0;
  Self.ReadsBytesCount := 0;
  Self.WriteBytesCount := 0;
  Self.IncreaseSizeCount := 0;
  Self.IncreaseSizeBytesCount := 0;
end;
{$ENDIF}

{ TFileMem }

function TFileMem.AbsoluteRead(const AAbsolutePosition: Int64; var ABuffer; ASize: Integer): Integer;
begin
  if (AAbsolutePosition<0) then raise EFileMem.Create(Format('%s.AbsoluteRead out of range %d size %d',[ClassName,AAbsolutePosition,ASize]));
  FFileStream.Position := AAbsolutePosition;
  Result := FFileStream.Read(ABuffer,ASize);
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  inc(FStats.ReadsCount);
  inc(FStats.ReadsBytesCount,ASize);
  {$ENDIF}
end;

function TFileMem.AbsoluteWrite(const AAbsolutePosition: Int64; const ABuffer; ASize: Integer): Integer;
begin
  if (AAbsolutePosition<0) then raise EFileMem.Create(Format('%s.AbsoluteWrite out of range %d size %d',[ClassName,AAbsolutePosition,ASize]));
  // Invalidate the persisted marker before a partial write can change data.
  CacheIsNOTStable;
  FFileStream.Position := AAbsolutePosition;
  Result := FFileStream.Write(ABuffer,ASize);
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  inc(FStats.WriteCount);
  inc(FStats.WriteBytesCount,ASize);
  {$ENDIF}
end;

procedure TFileMem.FlushFile;
begin
  {$IFDEF FPC}
  if not FileFlush(FFileStream.Handle) then RaiseLastOSError;
  {$ELSE}
    {$IFDEF MSWINDOWS}
    if not Windows.FlushFileBuffers(FFileStream.Handle) then RaiseLastOSError;
    {$ELSE}
    if fsync(FFileStream.Handle)<>0 then RaiseLastOSError;
    {$ENDIF}
  {$ENDIF}
end;

procedure TFileMem.CacheIsNOTStable;
begin
  if (FIsStableCache or FUnstableMarkerPending) and (not FIsFlushingCache) and Assigned(FFileStream) then begin
    FIsStableCache := False;
    // A failed header write/flush must be retried before ANY later payload.
    // FIsStableCache alone cannot distinguish that failure from a durable zero.
    FUnstableMarkerPending := True;
    // Cache eviction calls this while iterating cache cells. Write the header
    // directly; reentering the cache here would invalidate that iteration.
    FIsFlushingCache := True;
    try
      SaveHeader;
      FlushFile;
      FUnstableMarkerPending := False;
    finally
      FIsFlushingCache := False;
    end;
  end;
end;

constructor TFileMem.Create(const AFileName: String; AReadOnly: Boolean);
var LFileMode : Integer;
begin
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  FStats.Clear;
  FStats.ReadsCount := 0;
  FStats.WriteCount := 0;
  FStats.ReadsBytesCount := 0;
  FStats.WriteBytesCount := 0;
  FStats.IncreaseSizeCount := 0;
  FStats.IncreaseSizeBytesCount := 0;
  {$ENDIF}
  FIsStableCache := True;
  FIsFlushingCache := False;
  FFileName := AFileName;
  FIncreaseFileBytes := 1024*4; // 4K by default
  if AReadOnly then LFileMode := fmOpenRead + fmShareDenyNone
  else begin
    if FileExists(AFileName) then LFileMode := fmOpenReadWrite else LFileMode := fmCreate;
    LFileMode := LFileMode + fmShareDenyWrite;
  end;

  FCache := TCacheMem.Create(OnCacheNeedDataProc,OnCacheSaveDataProc);
  // Do not initialize the abstract reader from a failed stream constructor:
  // that would mask the original open error with a nil-stream read exception.
  FFileStream := TFileStream.Create(AFileName,LFileMode);
  inherited Create(0,AReadOnly);
end;

procedure TFileMem.ReportCloseFailure(AError: Exception);
begin
  if Assigned(FCache) and Assigned(FCache.OnLog) then begin
    try
      FCache.OnLog(Self,'Cannot close AbstractMem file '+FFileName+': '+AError.ClassName+': '+AError.Message);
    except
      // A diagnostic callback must not prevent resource release while unwinding.
    end;
  end;
end;

destructor TFileMem.Destroy;
begin
  try
    try
      if Assigned(FFileStream) and HeaderInitialized and (not ReadOnly) and ShouldFlushOnDestroy then begin
        if not FlushCache then raise EWriteError.Create('Cannot flush AbstractMem file '+FFileName);
      end;
    except
      // Explicit Write/Flush/Save report failure to the owner. Cleanup must not
      // throw it a second time and leak Self or obscure an earlier exception.
      on E: EStreamError do ReportCloseFailure(E);
      on E: EFileMem do ReportCloseFailure(E);
      on E: EAbstractMem do ReportCloseFailure(E);
      on E: EOSError do ReportCloseFailure(E);
      on E: EInOutError do ReportCloseFailure(E);
    end;
  finally
    // TCacheMem.Destroy otherwise retries dirty writes implicitly. Discard only
    // this retiring object's cache; the persisted unstable marker stays intact.
    if Assigned(FCache) then FCache.Clear;
    FreeAndNil(FCache);
    try
      inherited Destroy;
    finally
      FreeAndNil(FFileStream);
    end;
  end;
end;

function TFileMem.ShouldFlushOnDestroy: Boolean;
begin
  Result := True;
end;

procedure TFileMem.DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos: Int64; ANeedSize: Integer);
var LBuff : TBytes;
begin
  // File growth/truncation is also a write boundary. A failed allocation must
  // never be certified by a later close merely because no cache cells remain.
  try
  CacheIsNOTStable;
  if (ANeedSize<=0) And (AMaxAvailablePos<=0) then begin
    If Assigned(FCache) then FCache.Clear;
    FFileStream.Seek(0,soFromEnd);
    FFileStream.Size := 0;
    Exit;
  end;

  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  inc(FStats.IncreaseSizeCount);
  {$ENDIF}
  FFileStream.Seek(0,soFromEnd);
  // GoTo ANextAvailablePos
  if (FFileStream.Position<ANextAvailablePos) then begin
    SetLength(LBuff,ANextAvailablePos - FFileStream.Position);
    FillChar(LBuff[0],Length(LBuff),0);
    FFileStream.WriteBuffer(LBuff[0],Length(LBuff));
    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    inc(FStats.IncreaseSizeBytesCount,Length(LBuff));
    {$ENDIF}
  end;
  if (FFileStream.Position<ANextAvailablePos) then raise EFileMem.Create(Format('End file position (%d) is less than next available pos %d',[FFileStream.Position,ANextAvailablePos]));
  // At this time ANextAvailablePos <= FFileStream.Position
  AMaxAvailablePos := ANextAvailablePos + ANeedSize;
  if FIncreaseFileBytes>0 then begin
    AMaxAvailablePos := ((((AMaxAvailablePos - 1) DIV FIncreaseFileBytes)+1) * FIncreaseFileBytes);
  end;

  if (FFileStream.Size<AMaxAvailablePos) then begin
    SetLength(LBuff,AMaxAvailablePos - FFileStream.Position);
    FillChar(LBuff[0],Length(LBuff),0);
    FFileStream.WriteBuffer(LBuff[0],Length(LBuff));
    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    inc(FStats.IncreaseSizeBytesCount,Length(LBuff));
    {$ENDIF}
  end else AMaxAvailablePos := FFileStream.Size;
  CacheIsNOTStable;
  except
    FResizeFailed := True;
    raise;
  end;
end;

function TFileMem.FlushCache: Boolean;
var LStable: Byte;
begin
  Result := True;
  if ReadOnly or (not Assigned(FFileStream)) then Exit;
  FLock.Acquire;
  try
    // Uncached writes have no dirty cells to replay. Only a complete covering
    // retry can repair a known partial range; failed growth requires recovery.
    if FUncachedWriteFailed or FResizeFailed then Exit(False);
    if FIsStableCache and (not FUnstableMarkerPending) then begin
      if not Assigned(FCache) then Exit;
      if FCache.PendingToSaveSize=0 then Exit;
    end;
    FIsStableCache := False;
    FUnstableMarkerPending := True;
    FIsFlushingCache := True;
    try
      // The on-disk marker must reach storage before any dirty cache data.
      SaveHeader;
      FlushFile;
      FUnstableMarkerPending := False;
    finally
      FIsFlushingCache := False;
    end;
    if Assigned(FCache) then begin
      SaveHeader; // Keep cached metadata and its unstable marker current.
      Result := FCache.FlushCache;
      if not Result then Exit;
    end;
    FlushFile;
    // Header byte 6 is the existing AbstractMem stable marker. Publish it last,
    // after all data, rather than marking a failed flush stable in a finally.
    LStable := 1;
    // If publishing or undoing this byte fails, the next payload must first
    // reestablish a durable unstable header rather than trusting our flag.
    FUnstableMarkerPending := True;
    try
      FFileStream.Position := 6;
      FFileStream.WriteBuffer(LStable,SizeOf(LStable));
      FlushFile;
      if Assigned(FCache) then FCache.SaveToCache(LStable,SizeOf(LStable),6,False);
      FIsStableCache := True;
      FUnstableMarkerPending := False;
    except
      // Preserve the original failure if even invalidating the marker fails.
      LStable := 0;
      try
        FFileStream.Position := 6;
        FFileStream.WriteBuffer(LStable,SizeOf(LStable));
        FlushFile;
        FUnstableMarkerPending := False;
      except
      end;
      raise;
    end;
  finally
    FIsFlushingCache := False;
    FLock.Release;
  end;
end;

function TFileMem.GetDefaultCacheDataBlocksSize: Int64;
begin
  if Not Assigned(FCache) then Exit(0);
  Result := FCache.DefaultCacheDataBlocksSize;
end;

function TFileMem.GetGridCache: Boolean;
begin
  if Not Assigned(FCache) then Exit(False);
  Result := FCache.GridCache;
end;

function TFileMem.GetMaxCacheDataBlocks: Int64;
begin
  if Not Assigned(FCache) then Exit(0);
  Result := FCache.MaxCacheDataBlocks;
end;

function TFileMem.GetMaxCacheSize: Int64;
begin
  if Not Assigned(FCache) then Exit(0);
  Result := FCache.MaxCacheSize;
end;

function TFileMem.GetUseCache: Boolean;
begin
  Result := Assigned(FCache);
end;

{$IFDEF ABSTRACTMEM_ENABLE_STATS}
function TFileMem.GetStatsReport(AClearStats : Boolean) : String;
begin
  Result := FStats.ToString;
  if Assigned(FCache) then Result := Result + #10 + FCache.GetStatsReport(AClearStats);
  if AClearStats then FStats.Clear;
end;
{$ENDIF}

function TFileMem.IsAbstractMemInfoStable: Boolean;
begin
  Result := FIsStableCache;
end;

function TFileMem.LockCache: TCacheMem;
begin
  FLock.Acquire;
  Result := FCache;
end;

function TFileMem.New(AMemSize: TAbstractMemSize): TAMZone;
var LBuffer : TBytes;
begin
  Result := inherited New(AMemSize);
  // Initialize cache
  if Not Assigned(FCache) then Exit;
  FLock.Acquire;
  try
    SetLength(LBuffer,Result.size);
    FillChar(LBuffer[0],Result.size,0);
    FCache.SaveToCache(LBuffer[0],Result.size,Result.position,True);
  finally
    FLock.Release;
  end;
end;

function TFileMem.OnCacheNeedDataProc(var ABuffer; AStartPos : Int64; ASize: Integer): Integer;
var LFileSize : Int64;
begin
  LFileSize := FFileStream.Size;
  // Allowing Cache to ask for "out of range" data
  if (LFileSize < (AStartPos + ASize)) then ASize := LFileSize-AStartPos;
  Result := inherited Read(AStartPos,ABuffer,ASize);
end;

function TFileMem.OnCacheSaveDataProc(const ABuffer; AStartPos: Int64; ASize: Integer): Integer;
var LBuffer: TBytes;
begin
  if (AStartPos<=6) and (ASize>6-AStartPos) then begin
    // An old cached header must not restore the stable bit during eviction or
    // a partial flush. Only FlushCache may publish that final commit marker.
    SetLength(LBuffer,ASize);
    Move(ABuffer,LBuffer[0],ASize);
    LBuffer[6-AStartPos] := 0;
    Result := inherited Write(AStartPos,LBuffer[0],ASize);
  end else begin
    Result := inherited Write(AStartPos,ABuffer,ASize);
  end;
end;

function TFileMem.Read(const APosition: Int64; var ABuffer; ASize: Integer): Integer;
begin
  if Not Assigned(FCache) then begin
    Result := inherited;
    Exit;
  end;

  FLock.Acquire;
  try
    if FCache.LoadData(ABuffer,APosition,ASize) then Result := ASize
    else Result := 0;
  finally
    FLock.Release;
  end;
end;

procedure TFileMem.SetCachePerformance(AGridCache: Boolean;
  ADefaultCacheDataBlocksSize, AMaxCacheSize, AMaxCacheDataBlocks: Int64);
begin
  FLock.Acquire;
  Try
    UseCache := True;
    FCache.GridCache := AGridCache;
    FCache.DefaultCacheDataBlocksSize := ADefaultCacheDataBlocksSize;
    FCache.MaxCacheSize := AMaxCacheSize;
    FCache.MaxCacheDataBlocks := AMaxCacheDataBlocks;
  Finally
    FLock.Release;
  End;
end;

procedure TFileMem.SetDefaultCacheDataBlocksSize(const Value: Int64);
begin
  if Not Assigned(FCache) then Exit;
  FCache.DefaultCacheDataBlocksSize := Value;
end;

procedure TFileMem.SetGridCache(const Value: Boolean);
begin
  if Not Assigned(FCache) then Exit;
  FCache.GridCache := Value;
end;

procedure TFileMem.SetIncreaseFileBytes(const Value: Int64);
begin
  if (Value<0) or (Value>(1024*1024*100)) then FIncreaseFileBytes := 0
  else FIncreaseFileBytes := Value;
end;

procedure TFileMem.SetMaxCacheDataBlocks(const Value: Int64);
begin
  if Not Assigned(FCache) then Exit;
  FLock.Acquire;
  Try
    FCache.MaxCacheDataBlocks := Value;
  Finally
    FLock.Release;
  End;
end;

procedure TFileMem.SetMaxCacheSize(const Value: Int64);
begin
  if Not Assigned(FCache) then Exit;
  FLock.Acquire;
  Try
    FCache.MaxCacheSize := Value;
  Finally
    FLock.Release;
  End;
end;

procedure TFileMem.SetUseCache(const Value: Boolean);
begin
  FLock.Acquire;
  Try
    if Not Value then begin
      if (not ReadOnly) and (not FlushCache) then
        raise EWriteError.Create('Cannot disable a dirty AbstractMem cache after failed flush: '+FFileName);
      FreeAndNil(FCache);
    end else if Not Assigned(FCache) then begin
      FCache := TCacheMem.Create(OnCacheNeedDataProc,OnCacheSaveDataProc);
    end;
  Finally
    FLock.Release;
  End;
end;

procedure TFileMem.UnlockCache;
begin
  FLock.Release;
end;

procedure TFileMem.RecordFailedWrite(APosition: Int64; ASize: Integer);
var LEnd: Int64;
begin
  if (APosition<0) or (ASize<0) or (APosition>High(Int64)-ASize) then begin
    FResizeFailed := True;
    Exit;
  end;
  LEnd := APosition+ASize;
  if not FUncachedWriteFailed then begin
    FFailedWriteStart := APosition;
    FFailedWriteEnd := LEnd;
  end else begin
    if APosition<FFailedWriteStart then FFailedWriteStart := APosition;
    if LEnd>FFailedWriteEnd then FFailedWriteEnd := LEnd;
  end;
  FUncachedWriteFailed := True;
end;

function TFileMem.Write(const APosition: Int64; const ABuffer; ASize: Integer): Integer;
begin
  if (not Assigned(FCache)) or FIsFlushingCache then begin
    FLock.Acquire;
    try
    try
      Result := inherited Write(APosition,ABuffer,ASize);
      if FIsFlushingCache and (Result<>ASize) then
        raise EWriteError.Create('Cannot write AbstractMem header: '+FFileName);
    except
      if (not FIsFlushingCache) and (not Assigned(FCache)) then RecordFailedWrite(APosition,ASize);
      raise;
    end;
    if (not FIsFlushingCache) and (not Assigned(FCache)) then begin
      if Result<>ASize then RecordFailedWrite(APosition,ASize)
      else if FUncachedWriteFailed and (APosition<=FFailedWriteStart) and
        (APosition<=High(Int64)-ASize) and (APosition+ASize>=FFailedWriteEnd) then
        FUncachedWriteFailed := False;
    end;
    finally
      FLock.Release;
    end;
    Exit;
  end;
  CheckInitialized(True);
  FLock.Acquire;
  try
    FCache.SaveToCache(ABuffer,ASize,APosition,True);
    Result := ASize;
  finally
    FLock.Release;
  end;
end;

end.
