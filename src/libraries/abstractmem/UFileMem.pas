unit UFileMem;

{
  This file is part of AbstractMem framework

  Copyright (C) 2020 Albert Molina - bpascalblockchain@gmail.com

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
  UAbstractBTree, UAbstractMem, UCacheMem;

{$I ./ConfigAbstractMem.inc }

type
  EFileMem = Class(Exception);

  TFileMem = Class(TAbstractMem)
  private
    FFileStream : TFileStream;
    FCache : TCacheMem;
    FFileName: String;
    FIsStableCache: Boolean;
    FIsFlushingCache : Boolean;
    function OnCacheNeedDataProc(var ABuffer; AStartPos : Integer; ASize : Integer) : Boolean;
    function OnCacheSaveDataProc(const ABuffer; AStartPos : Integer; ASize : Integer) : Boolean;
    procedure SetMaxCacheSize(const Value: Integer);
    function GetMaxCacheSize: Integer;
    function GetMaxCacheDataBlocks: Integer;
    procedure SetMaxCacheDataBlocks(const Value: Integer);
    procedure CacheIsNOTStable; inline;
  protected
    function AbsoluteWrite(const AAbsolutePosition : Int64; const ABuffer; ASize : Integer) : Integer; override;
    function AbsoluteRead(const AAbsolutePosition : Int64; var ABuffer; ASize : Integer) : Integer; override;
    procedure DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos : Integer; ANeedSize : Integer); override;
    function IsAbstractMemInfoStable : Boolean; override;
  public
    Constructor Create(const AFileName : String; AReadOnly : Boolean); reintroduce;
    Destructor Destroy; override;
    function New(AMemSize : Integer) : TAMZone; override;
    procedure Write(const APosition : Integer; const ABuffer; ASize : Integer); overload; override;
    function Read(const APosition : Integer; var ABuffer; ASize : Integer) : Integer; overload; override;
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
  End;

implementation

{ TFileMem }

function TFileMem.AbsoluteRead(const AAbsolutePosition: Int64; var ABuffer; ASize: Integer): Integer;
begin
  FFileStream.Seek(AAbsolutePosition,soFromBeginning);
  Result := FFileStream.Read(ABuffer,ASize);
end;

function TFileMem.AbsoluteWrite(const AAbsolutePosition: Int64; const ABuffer; ASize: Integer): Integer;
begin
  FFileStream.Seek(AAbsolutePosition,soFromBeginning);
  Result := FFileStream.Write(ABuffer,ASize);
  CacheIsNOTStable;
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
  FIsStableCache := True;
  FIsFlushingCache := False;
  FFileName := AFileName;
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

procedure TFileMem.DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos: Integer; ANeedSize: Integer);
var LBuff : TBytes;
begin
  FFileStream.Seek(0,soFromEnd);
  // GoTo ANextAvailablePos
  if (FFileStream.Position<ANextAvailablePos) then begin
    SetLength(LBuff,ANextAvailablePos - FFileStream.Position);
    FillChar(LBuff[0],Length(LBuff),0);
    FFileStream.Write(LBuff[0],Length(LBuff));
  end;
  if (FFileStream.Position<ANextAvailablePos) then raise EFileMem.Create(Format('End file position (%d) is less than next available pos %d',[FFileStream.Position,ANextAvailablePos]));
  // At this time ANextAvailablePos <= FFileStream.Position
  AMaxAvailablePos := ANextAvailablePos + ANeedSize;
  if (FFileStream.Size<AMaxAvailablePos) then begin
    SetLength(LBuff,AMaxAvailablePos - FFileStream.Position);
    FillChar(LBuff[0],Length(LBuff),0);
    FFileStream.Write(LBuff[0],Length(LBuff));
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

function TFileMem.IsAbstractMemInfoStable: Boolean;
begin
  Result := FIsStableCache;
end;

function TFileMem.LockCache: TCacheMem;
begin
  FLock.Acquire;
  Result := FCache;
end;

function TFileMem.New(AMemSize: Integer): TAMZone;
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

function TFileMem.OnCacheNeedDataProc(var ABuffer; AStartPos, ASize: Integer): Boolean;
begin
  Result := inherited Read(AStartPos,ABuffer,ASize) = ASize;
end;

function TFileMem.OnCacheSaveDataProc(const ABuffer; AStartPos, ASize: Integer): Boolean;
begin
  inherited Write(AStartPos,ABuffer,ASize);
  Result := True;
end;

function TFileMem.Read(const APosition: Integer; var ABuffer; ASize: Integer): Integer;
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

procedure TFileMem.UnlockCache;
begin
  FLock.Release;
end;

procedure TFileMem.Write(const APosition: Integer; const ABuffer; ASize: Integer);
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
