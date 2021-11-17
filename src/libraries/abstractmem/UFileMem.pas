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
    FIncreaseFileBytes: Int64;
    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    FStats : TFileMemStats;
    {$ENDIF}
    function OnCacheNeedDataProc(var ABuffer; AStartPos : Int64; ASize: Integer): Integer;
    function OnCacheSaveDataProc(const ABuffer; AStartPos : Int64; ASize: Integer): Integer;
    procedure SetMaxCacheSize(const Value: Integer);
    function GetMaxCacheSize: Integer;
    function GetMaxCacheDataBlocks: Integer;
    procedure SetMaxCacheDataBlocks(const Value: Integer);
    procedure CacheIsNOTStable; inline;
    function GetUseCache: Boolean;
    procedure SetUseCache(const Value: Boolean);
    procedure SetIncreaseFileBytes(const Value: Int64);
  protected
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
    property MaxCacheSize : Integer read GetMaxCacheSize write SetMaxCacheSize;
    property MaxCacheDataBlocks : Integer read GetMaxCacheDataBlocks write SetMaxCacheDataBlocks;
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
  FFileStream.Position := AAbsolutePosition;
  Result := FFileStream.Write(ABuffer,ASize);
  CacheIsNOTStable;
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  inc(FStats.WriteCount);
  inc(FStats.WriteBytesCount,ASize);
  {$ENDIF}
end;

procedure TFileMem.CacheIsNOTStable;
begin
  If (FIsStableCache)          // Only will mark first time
    And (Not FIsFlushingCache) // Only will mark when not Flushing cache
    And (Assigned(FCache)) then begin
    FIsStableCache := False;
    SaveHeader;
  end;
end;

constructor TFileMem.Create(const AFileName: String; AReadOnly: Boolean);
var LFileMode : Integer;
  LReadOnly : Boolean;
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
  LReadOnly := True;
  try
    FFileStream := TFileStream.Create(AFileName,LFileMode);
    LReadOnly := AReadOnly; // To protect against raise exception
  finally
    inherited Create(0,LReadOnly);
  end;
end;

destructor TFileMem.Destroy;
begin
  if Not ReadOnly then FlushCache;
  FreeAndNil(FCache);
  inherited;
  FreeAndNil(FFileStream);
  FreeAndNil(FCache);
end;

procedure TFileMem.DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos: Int64; ANeedSize: Integer);
var LBuff : TBytes;
begin
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
    FFileStream.Write(LBuff[0],Length(LBuff));
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
    FFileStream.Write(LBuff[0],Length(LBuff));
    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    inc(FStats.IncreaseSizeBytesCount,Length(LBuff));
    {$ENDIF}
  end else AMaxAvailablePos := FFileStream.Size;
  CacheIsNOTStable;
end;

function TFileMem.FlushCache: Boolean;
begin
  if Not Assigned(FCache) then Exit(True);
  FLock.Acquire;
  try
    Result := FCache.FlushCache;
  finally
    FIsStableCache := True;
    FIsFlushingCache := True;
    try
      SaveHeader;
    finally
      FIsFlushingCache := False;
    end;
    FLock.Release;
  end;
end;

function TFileMem.GetMaxCacheDataBlocks: Integer;
begin
  if Not Assigned(FCache) then Exit(0);
  Result := FCache.MaxCacheDataBlocks;
end;

function TFileMem.GetMaxCacheSize: Integer;
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
begin
  Result := inherited Read(AStartPos,ABuffer,ASize);
end;

function TFileMem.OnCacheSaveDataProc(const ABuffer; AStartPos : Int64; ASize: Integer): Integer;
begin
  Result := inherited Write(AStartPos,ABuffer,ASize);
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

procedure TFileMem.SetIncreaseFileBytes(const Value: Int64);
begin
  if (Value<0) or (Value>(1024*1024*100)) then FIncreaseFileBytes := 0
  else FIncreaseFileBytes := Value;
end;

procedure TFileMem.SetMaxCacheDataBlocks(const Value: Integer);
begin
  if Not Assigned(FCache) then Exit;
  FLock.Acquire;
  Try
    FCache.MaxCacheDataBlocks := Value;
  Finally
    FLock.Release;
  End;
end;

procedure TFileMem.SetMaxCacheSize(const Value: Integer);
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
      If Not ReadOnly then FlushCache;
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

function TFileMem.Write(const APosition: Int64; const ABuffer; ASize: Integer) : Integer;
begin
  if (Not Assigned(FCache)) Or (FIsFlushingCache) then begin
    inherited;
    Exit;
  end;

  CheckInitialized(True);
  FLock.Acquire;
  try
    FCache.SaveToCache(ABuffer,ASize,APosition,True);
  finally
    FLock.Release;
  end;
end;

end.
