unit UCacheMem;

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
  {$IFNDEF FPC}{$IFDEF MSWINDOWS}windows,{$ENDIF}{$ENDIF}
  UAbstractBTree, UOrderedList;

{$I ./ConfigAbstractMem.inc }

type
  TCacheMem = Class;

  PCacheMemData = ^TCacheMemData;

  { TCacheMemData }

  TCacheMemData = record
    parent : PCacheMemData;
    left : PCacheMemData;
    right : PCacheMemData;
    balance : Integer;
    //
    buffer : TBytes;
    startPos : Integer;
    used_previous : PCacheMemData;
    used_next : PCacheMemData;
    pendingToSave : Boolean;
    function GetSize : Integer;
    function GetEndPos : Integer;
    procedure Clear;
    function ToString : String;
    procedure DoMark(const ACacheMem : TCacheMem; AMySelfPointer : PCacheMemData; AAddToList : Boolean);
    procedure MarkAsUsed(const ACacheMem : TCacheMem; AMySelfPointer : PCacheMemData);
    procedure UnMark(const ACacheMem : TCacheMem; AMySelfPointer : PCacheMemData);
  end;

  TCacheMemDataTree = Class( TAVLAbstractTree<PCacheMemData> )
  private
    FRoot : PCacheMemData;
  protected
    function GetRoot: PCacheMemData; override;
    procedure SetRoot(const Value: PCacheMemData); override;
    function HasPosition(const ANode : PCacheMemData; APosition : TAVLTreePosition) : Boolean; override;
    procedure SetPosition(var ANode : PCacheMemData; APosition : TAVLTreePosition; const ANewValue : PCacheMemData); override;
    procedure ClearPosition(var ANode : PCacheMemData; APosition : TAVLTreePosition); override;
    function GetBalance(const ANode : PCacheMemData) : Integer; override;
    procedure SetBalance(var ANode : PCacheMemData; ANewBalance : Integer); override;
    function AreEquals(const ANode1, ANode2 : PCacheMemData) : Boolean; override;
    procedure ClearNode(var ANode : PCacheMemData); override;
    procedure DisposeNode(var ANode : PCacheMemData); override;
  public
    function IsNil(const ANode : PCacheMemData) : Boolean; override;
    function ToString(const ANode: PCacheMemData) : String; override;
    constructor Create; reintroduce;
    //
    function GetPosition(const ANode : PCacheMemData; APosition : TAVLTreePosition) : PCacheMemData; override;
  End;


  // TickCount is platform specific (32 or 64 bits)
  TTickCount = {$IFDEF CPU64}QWord{$ELSE}Cardinal{$ENDIF};

  TPlatform = Class
  public
    class function GetTickCount : TTickCount;
    class function GetElapsedMilliseconds(Const previousTickCount : TTickCount) : Int64;
  End;

  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  TCacheMemStats = record
    flushCount : Integer;
    flushSize : Integer;
    flushElapsedMillis : Int64;
    freememCount : Integer;
    freememSize : Integer;
    freememElaspedMillis : Int64;
    maxUsedCacheSize : Integer;
    reusedCacheMemDataCount : Integer;
    reusedCacheMemDataBytes : Int64;
    procedure Clear;
    function ToString : String;
  end;
  {$ENDIF}

  TOnNeedDataProc = function(var ABuffer; AStartPos : Integer; ASize : Integer) : Boolean of object;
  TOnSaveDataProc = function(const ABuffer; AStartPos : Integer; ASize : Integer) : Boolean of object;

  ECacheMem = Class(Exception);

  TCacheMem = Class
  private
    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    FCacheMemStats : TCacheMemStats;
    {$ENDIF}
    FOldestUsed : PCacheMemData;
    FNewestUsed : PCacheMemData;
    FCacheData : TCacheMemDataTree;
    FPendingToSaveBytes : Integer;
    FCacheDataBlocks : Integer;
    FCacheDataSize : Integer;
    FOnNeedDataProc : TOnNeedDataProc;
    FOnSaveDataProc : TOnSaveDataProc;
    FMaxCacheSize: Integer;
    FMaxCacheDataBlocks: Integer;
    function FindCacheMemDataByPosition(APosition : Integer; out APCacheMemData : PCacheMemData) : Boolean;
    procedure Delete(var APCacheMemData : PCacheMemData); overload;
    function FlushCache(const AFlushCacheList : TOrderedList<PCacheMemData>) : Boolean; overload;
    procedure CheckMaxMemUsage;
  public
    Constructor Create(AOnNeedDataProc : TOnNeedDataProc; AOnSaveDataProc : TOnSaveDataProc);
    Destructor Destroy; override;
    //
    procedure Clear;
    procedure SaveToCache(const ABuffer; ASize, AStartPos : Integer; AMarkAsPendingToSave : Boolean); overload;
    procedure SaveToCache(const ABuffer : TBytes; AStartPos : Integer; AMarkAsPendingToSave : Boolean); overload;
    function LoadData(var ABuffer; const AStartPos, ASize : Integer) : Boolean;
    function ToString : String; reintroduce;
    function FlushCache : Boolean; overload;
    function FreeMem(const AMaxMemSize, AMaxBlocks : Integer) : Boolean;

    procedure ConsistencyCheck;

    property CacheDataSize : Integer read FCacheDataSize;
    // Bytes in cache

    property PendingToSaveSize : Integer read FPendingToSaveBytes;
    // Bytes in cache pending to flush

    property CacheDataBlocks : Integer read FCacheDataBlocks;
    // Blocks in cache

    property MaxCacheSize : Integer read FMaxCacheSize write FMaxCacheSize;
    property MaxCacheDataBlocks : Integer read FMaxCacheDataBlocks write FMaxCacheDataBlocks;
    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    procedure ClearStats;
    property CacheMemStats : TCacheMemStats read FCacheMemStats;
    {$ENDIF}
    function GetStatsReport(AClearStats : Boolean) : String;
  End;

implementation

{ TPlatform }

class function TPlatform.GetElapsedMilliseconds(const previousTickCount: TTickCount): Int64;
begin
  Result := (Self.GetTickCount - previousTickCount);
end;

class function TPlatform.GetTickCount: TTickCount;
begin
  Result := {$IFDEF CPU64}GetTickCount64{$ELSE}
   {$IFDEF FPC}SysUtils.GetTickCount{$ELSE}
     {$IFDEF MSWINDOWS}Windows.GetTickCount{$ELSE}
     TThread.GetTickCount;
     {$ENDIF}
   {$ENDIF}
  {$ENDIF}
end;

type
  TBytesHelper = record helper for TBytes
    function ToString : String;
  end;

{ TBytesHelper }

function TBytesHelper.ToString: String;
var i : Integer;
begin
  Result := '';
  for i := Low(Self) to High(Self) do begin
    if Result<>'' then Result := Result + ',';
    Result := Result + IntToStr(Self[i]);
  end;
  Result := '['+Result+']';
end;

{ TCacheMem }

function _CacheMem_CacheData_Comparer(const Left, Right: PCacheMemData): Integer;
begin
  Result := Integer(Left^.startPos) - Integer(Right^.startPos);
end;

procedure TCacheMem.CheckMaxMemUsage;
begin
  if ((FMaxCacheSize < 0) or (FCacheDataSize<=FMaxCacheSize))
     and
     ((FMaxCacheDataBlocks < 0) or (FCacheDataBlocks<=FMaxCacheDataBlocks)) then Exit;
  // When calling FreeMem will increase call in order to speed
  FreeMem((FMaxCacheSize-1) SHR 1, (FMaxCacheDataBlocks-1) SHR 1);
end;

procedure TCacheMem.Clear;
var P, PCurr : PCacheMemData;
begin
  PCurr := FCacheData.FindLowest;
  while (Assigned(PCurr)) do begin
    P := PCurr;
    PCurr := FCacheData.FindSuccessor(P);
    FCacheData.Delete(P);
  end;

  FPendingToSaveBytes := 0;
  FCacheDataSize := 0;
  FCacheDataBlocks := 0;
  FOldestUsed := Nil;
  FNewestUsed := Nil;
end;

{$IFDEF ABSTRACTMEM_ENABLE_STATS}
procedure TCacheMem.ClearStats;
begin
  FCacheMemStats.Clear;
end;
{$ENDIF}

procedure TCacheMem.ConsistencyCheck;
var i, iLOrderPos : Integer;
  PLast, PCurrent : PCacheMemData;
  LTotalSize, LTotalPendingSize, LTotalNodes : Integer;
  LOrder : TOrderedList<PCacheMemData>;
begin
  //
  PLast := Nil;
  LTotalSize := 0;
  LTotalPendingSize := 0;
  LTotalNodes := 0;

  PCurrent := FCacheData.FindLowest;
  while (Assigned(PCurrent)) do begin
    inc(LTotalNodes);
    if PCurrent^.GetSize=0 then raise ECacheMem.Create(Format('Cache "%s" size 0',[PCurrent^.ToString]));

    if Assigned(PLast) then begin
      if PLast^.GetEndPos>=PCurrent^.startPos then raise ECacheMem.Create(Format('Cache "%s" end pos with previous "%s"',[PCurrent^.ToString,PLast^.ToString]));
    end;
    PLast := PCurrent;
    inc(LTotalSize,PCurrent^.GetSize);
    if PCurrent^.pendingToSave then begin
      inc(LTotalPendingSize,PCurrent^.GetSize);
    end;

    PCurrent := FCacheData.FindSuccessor(PCurrent);
  end;
  if (LTotalNodes<>FCacheDataBlocks) then raise ECacheMem.Create(Format('Found cache blocks %d <> %d',[LTotalNodes,FCacheDataBlocks]));
  if LTotalSize<>FCacheDataSize then raise ECacheMem.Create(Format('Cache size %d <> %d',[LTotalSize,FCacheDataSize]));
  if LTotalPendingSize<>FPendingToSaveBytes then raise ECacheMem.Create(Format('Total pending size %d <> %d',[LTotalPendingSize,FPendingToSaveBytes]));

  LOrder := TOrderedList<PCacheMemData>.Create(False,_CacheMem_CacheData_Comparer);
  try
    PLast := Nil;
    PCurrent := FOldestUsed;
    i := 0;
    while (Assigned(PCurrent)) do begin
      inc(i);
      if PCurrent^.used_previous<>PLast then raise ECacheMem.Create(Format('Previous <> Last at %d for %s',[i,PCurrent^.ToString]));
      if LOrder.Find( PCurrent, iLOrderPos ) then begin
        raise ECacheMem.Create(Format('Circular in mark at %d for %s',[i,PCurrent^.ToString]));
      end else if (iLOrderPos < LOrder.Count) then begin
        if LOrder.Get(iLOrderPos)^.startPos<=PCurrent^.GetEndPos then begin
          raise ECacheMem.Create(Format('Overused in mark at %d for %s vs (iLOrderPos=%d) %s',[i,PCurrent^.ToString, iLOrderPos, LOrder.Get(iLOrderPos)^.ToString]));
        end;
      end;
      if LOrder.Add(PCurrent)<0 then raise ECacheMem.Create(Format('Circular in mark at %d for %s',[i,PCurrent^.ToString]));
      PLast := PCurrent;
      PCurrent := PCurrent^.used_next;
    end;
    // Check last
    if (PLast<>FNewestUsed) then raise ECacheMem.Create(Format('Last <> Newest at %d/%d',[i,LTotalNodes]));
    if (i<>LTotalNodes) then raise ECacheMem.Create(Format('Marked nodes %d <> CacheData nodes %d',[i,LTotalNodes]));

  finally
    LOrder.Free;
  end;
end;

constructor TCacheMem.Create(AOnNeedDataProc : TOnNeedDataProc; AOnSaveDataProc : TOnSaveDataProc);
begin
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  FCacheMemStats.Clear;
  {$ENDIF}
  FMaxCacheSize := -1; // No limit by default
  FMaxCacheDataBlocks := -1; // No limit by default
  FCacheData := TCacheMemDataTree.Create;
  FCacheDataBlocks := 0;
  FPendingToSaveBytes := 0;
  FCacheDataSize := 0;
  FOnNeedDataProc := AOnNeedDataProc;
  FOnSaveDataProc := AOnSaveDataProc;
  FOldestUsed := Nil;
  FNewestUsed := Nil;
end;

procedure TCacheMem.Delete(var APCacheMemData : PCacheMemData);
var LConsistency : PCacheMemData;
begin
  if not FindCacheMemDataByPosition(APCacheMemData^.startPos,LConsistency) then Raise ECacheMem.Create(Format('Delete not found for %s',[APCacheMemData^.ToString]));
  Dec(FCacheDataSize,APCacheMemData.GetSize);
  if APCacheMemData^.pendingToSave then begin
    Dec(FPendingToSaveBytes,APCacheMemData^.GetSize);
  end;
  SetLength(APCacheMemData^.buffer,0);
  APCacheMemData^.UnMark(Self,APCacheMemData);
  FCacheData.Delete(APCacheMemData);
  Dec(FCacheDataBlocks);
end;

destructor TCacheMem.Destroy;
begin
  FlushCache;
  Clear;
  FreeAndNil(FCacheData);
  inherited;
end;

function TCacheMem.FindCacheMemDataByPosition(APosition: Integer; out APCacheMemData: PCacheMemData): Boolean;
  // Will return FCacheData index at AiCacheDataPos that contains APosition
  // When returning FALSE, AiCacheDataPos will be index of previous FCacheData position to use
var PSearch : PCacheMemData;
begin
  APCacheMemData := Nil;
  Result := False;

  New(PSearch);
  try
    PSearch^.Clear;
    SetLength(PSearch^.buffer,0);
    PSearch^.startPos := APosition;
    PSearch^.pendingToSave := False;
    // Will search a value
    APCacheMemData := FCacheData.FindInsertPos(PSearch);
    if (Assigned(APCacheMemData)) then begin
      // Watch if is contained in it
      if (APCacheMemData^.startPos>APosition) then begin
        APCacheMemData := FCacheData.FindPrecessor(APCacheMemData);
      end;
      if (Assigned(APCacheMemData)) then begin
        Result := (APCacheMemData^.startPos<=APosition) and (APCacheMemData^.GetEndPos >= APosition);
      end;
    end;
  finally
    Dispose(PSearch);
  end;
end;

function TCacheMem.FlushCache(const AFlushCacheList : TOrderedList<PCacheMemData>) : Boolean;
var i : Integer;
  PToCurrent, PToNext : PCacheMemData;
  LTotalBytesSaved, LTotalBytesError : Integer;
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  LTickCount : TTickCount;
  {$ENDIF}
begin
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  LTickCount := TPlatform.GetTickCount;
  {$ENDIF}
  LTotalBytesSaved := 0;
  LTotalBytesError := 0;
  Result := True;

  if (FPendingToSaveBytes<=0) then Exit;

  i := 0;
  PToNext := FOldestUsed;

  repeat
    if Assigned(AFlushCacheList) then begin
      if i < AFlushCacheList.Count then PToCurrent:=AFlushCacheList.Get(i)
      else PToCurrent := Nil;
      inc(i);
    end else PToCurrent := PToNext;

    if Assigned(PToCurrent) then begin
      if (PToCurrent^.pendingToSave) then begin

        if Not Assigned(FOnSaveDataProc) then Exit(False);
        if Not FOnSaveDataProc(PToCurrent^.buffer[0],PToCurrent^.startPos,PToCurrent^.GetSize) then begin
          Result := False;
          inc(LTotalBytesError,PToCurrent^.GetSize);
        end else begin
          inc(LTotalBytesSaved,PToCurrent^.GetSize);
          PToCurrent^.pendingToSave := False;
          Dec(FPendingToSaveBytes,PToCurrent^.GetSize);
        end;
      end;
      PToNext := PToCurrent^.used_next;
    end;
  until Not Assigned(PToCurrent);
  if (LTotalBytesSaved>0) or (LTotalBytesError>0) then begin
    {$IFDEF ABSTRACTMEM_ENABLE_STATS}
    Inc(FCacheMemStats.flushCount);
    Inc(FCacheMemStats.flushSize,LTotalBytesSaved);
    Inc(FCacheMemStats.flushElapsedMillis,TPlatform.GetElapsedMilliseconds(LTickCount));
    {$ENDIF}
  end;
  if (LTotalBytesError=0) and (Not Assigned(AFlushCacheList)) and (FPendingToSaveBytes<>0) then raise ECacheMem.Create(Format('Flush Inconsistency error Saved:%d Pending:%d',[LTotalBytesSaved,FPendingToSaveBytes]));

end;

function TCacheMem.FlushCache: Boolean;
begin
  Result := FlushCache(Nil); // FlushCache without a list, without order
end;

function TCacheMem.FreeMem(const AMaxMemSize, AMaxBlocks: Integer) : Boolean;
var
  i, LPreviousCacheDataSize, LTempCacheDataSize,
  LFinalMaxMemSize, LMaxPendingRounds : Integer;
  PToRemove, PToNext : PCacheMemData;
  LListToFlush : TOrderedList<PCacheMemData>;
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  LTickCount : TTickCount;
  {$ENDIF}
begin
  // Will delete FCacheData until AMaxMemSize >= FCacheDataSize
  if ((AMaxMemSize < 0) or (FCacheDataSize<=AMaxMemSize))
     and
     ((AMaxBlocks < 0) or (FCacheDataBlocks<=AMaxBlocks)) then Exit(True);
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  LTickCount := TPlatform.GetTickCount;
  {$ENDIF}
  LPreviousCacheDataSize := FCacheDataSize;

  if (AMaxMemSize<0) then LFinalMaxMemSize := FCacheDataSize
  else LFinalMaxMemSize := AMaxMemSize;
  if (AMaxBlocks<0) then LMaxPendingRounds := 0
  else LMaxPendingRounds := FCacheDataBlocks - AMaxBlocks;
  //
  PToRemove := FOldestUsed;
  LListToFlush := TOrderedList<PCacheMemData>.Create(False,_CacheMem_CacheData_Comparer);
  try
    LTempCacheDataSize := FCacheDataSize;
    while (Assigned(PToRemove)) and
      // Both conditions must be true
      ((LTempCacheDataSize > LFinalMaxMemSize) or (LMaxPendingRounds>0))
      do begin
      Dec(LMaxPendingRounds);
      PToNext := PToRemove^.used_next; // Capture now to avoid future PToRemove updates
      Dec(LTempCacheDataSize, PToRemove^.GetSize);
      if (PToRemove^.pendingToSave) then begin
        // Add to list to flush
        LListToFlush.Add(PToRemove);
      end else Delete(PToRemove);
      PToRemove := PToNext; // Point to next used
    end;
    // LListToFlush will have pending to save
    Result := FlushCache(LListToFlush);
    // Delete not deleted previously
    for i:=0 to LListToFlush.Count-1 do begin
      PToRemove := LListToFlush.Get(i);
      Delete( PToRemove );
    end;
  finally
    LListToFlush.Free;
  end;
  if (Result) and (LTempCacheDataSize <> FCacheDataSize) then raise ECacheMem.Create(Format('Inconsistent error on FreeMem Expected size %d <> obtained %d',[LTempCacheDataSize,FCacheDataSize]));
  if (Result) and (LMaxPendingRounds>0) then raise ECacheMem.Create(Format('Inconsistent error on FreeMem Expected Max Blocks %d <> obtained %d',[AMaxBlocks,FCacheDataBlocks]));

  Result := (Result) And (FCacheDataSize <= AMaxMemSize);
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  Inc(FCacheMemStats.freememCount);
  Inc(FCacheMemStats.freememSize,LPreviousCacheDataSize - FCacheDataSize);
  Inc(FCacheMemStats.freememElaspedMillis,TPlatform.GetElapsedMilliseconds(LTickCount));
  {$ENDIF}
end;

function TCacheMem.GetStatsReport(AClearStats: Boolean): String;
begin
  {$IFDEF ABSTRACTMEM_ENABLE_STATS}
  Result := FCacheMemStats.ToString;
  if AClearStats then ClearStats;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function TCacheMem.LoadData(var ABuffer; const AStartPos, ASize: Integer): Boolean;
  // Will return a Pointer to AStartPos

  function _CaptureDataFromOnNeedDataProc(ACapturePosStart, ACaptureSize : Integer; var ACapturedData : TBytes) : Boolean;
  {$IFDEF ABSTRACTMEM_TESTING_MODE}var i : integer;{$ENDIF}
  begin
    SetLength(ACapturedData,ACaptureSize);
    if Not Assigned(FOnNeedDataProc) then begin
      FillChar(ACapturedData[0],Length(ACapturedData),0);
      {$IFDEF ABSTRACTMEM_TESTING_MODE}
      // TESTING PURPOSE TESTING ONLY
      for i := 0 to High(ACapturedData) do begin
        ACapturedData[i] := Byte(ACapturePosStart + i);
      end;
      // END TESTING PURPOSE
      {$ENDIF}
      Exit(False);
    end;
    Result := FOnNeedDataProc(ACapturedData[0],ACapturePosStart,ACaptureSize);
  end;


var
  LNewP, PCurrent, PToDelete : PCacheMemData;
  LLastAddedPosition, LBytesCount, LSizeToStore : Integer;
  LTempData : TBytes;
  LTmpResult : Boolean;
begin
  if ASize<0 then raise ECacheMem.Create(Format('Invalid load size %d',[ASize]));
  if ASize=0 then Exit(True);
  if (FindCacheMemDataByPosition(AStartPos,PCurrent)) then begin
    if (PCurrent^.GetSize - (AStartPos - PCurrent^.startPos)) >= ASize then begin
      // PStart has all needed info
      Move(PCurrent^.buffer[ AStartPos-PCurrent^.startPos ],ABuffer,ASize);
      PCurrent^.MarkAsUsed(Self,PCurrent);
      Result := True;
      {$IFDEF ABSTRACTMEM_ENABLE_STATS}
      inc(FCacheMemStats.reusedCacheMemDataCount);
      inc(FCacheMemStats.reusedCacheMemDataBytes,ASize);
      {$ENDIF}
      Exit;
    end;
  end;

  // Will need to create a new "linar struct" because not found a linear struct previously
  New( LNewP );
  try
    LNewP.Clear;

    LSizeToStore := ASize;
    SetLength(LNewP^.buffer, LSizeToStore);

    LNewP.startPos := AStartPos;

    Result := True;

    LLastAddedPosition := AStartPos - 1;
    while (Assigned(PCurrent)) and ( (LLastAddedPosition) < (LNewP^.GetEndPos) ) do begin
      if (PCurrent^.GetEndPos <= LLastAddedPosition) then PCurrent := FCacheData.FindSuccessor(PCurrent)
      else if (PCurrent^.startPos > LNewP^.GetEndPos) then break
      else begin
        // PCurrent will be used:
        //
        if (PCurrent^.startPos <= LLastAddedPosition) then begin
          // PCurrent start before, increase buffer and set startPos
          SetLength(LNewP^.buffer ,Length(LNewP^.buffer) + (LLastAddedPosition - PCurrent^.startPos + 1));
          LNewP.startPos := PCurrent^.startPos;
          LLastAddedPosition := PCurrent^.startPos-1;
        end else if (PCurrent^.startPos > LLastAddedPosition+1) then begin
          // Need data "between"
          LBytesCount := PCurrent^.startPos - (LLastAddedPosition+1);
          LTmpResult := _CaptureDataFromOnNeedDataProc(LLastAddedPosition+1,LBytesCount,LTempData);
          Result := Result and LTmpResult;
          Move(LTempData[0],LNewP^.buffer[ (LLastAddedPosition+1) - LNewP^.startPos ], LBytesCount);
          inc(LLastAddedPosition,LBytesCount);
        end;
        // At this point (LLastAddedPosition+1 = PCurrent^.startPos)
        // Add available data
        if PCurrent^.GetEndPos>(LNewP^.GetEndPos) then begin
          // Will need to increase buffer size:
          SetLength( LNewP^.buffer , LNewP^.GetSize + (PCurrent^.GetEndPos - LNewP^.GetEndPos));
        end;
        LBytesCount := PCurrent^.GetEndPos - LLastAddedPosition;
        Move(PCurrent^.buffer[ 0 ],LNewP^.buffer[ (LLastAddedPosition+1) - LNewP^.startPos ], LBytesCount);
        inc(LLastAddedPosition,LBytesCount);

        // Has been used, delete
        LNewP.pendingToSave := (LNewP^.pendingToSave) or (PCurrent^.pendingToSave);
        PToDelete := PCurrent;
        PCurrent := FCacheData.FindSuccessor(PCurrent);
        Delete( PToDelete );
      end;
    end;
    if (LLastAddedPosition) < (LNewP^.GetEndPos) then begin
      // That means there is no data available at cache
      LBytesCount := LNewP^.GetSize - (LLastAddedPosition - LNewP^.startPos +1);
      LTmpResult := _CaptureDataFromOnNeedDataProc(LLastAddedPosition+1,LBytesCount,LTempData);
      Result := Result and LTmpResult;
      Move(LTempData[0],LNewP^.buffer[ (LLastAddedPosition+1) - LNewP^.startPos ], LBytesCount);
    end;
  Except
    on E:Exception do begin
      LNewP.Clear;
      Dispose(LNewP);
      Raise;
    end;
  end;

  // Save new
  LNewP^.MarkAsUsed(Self,LNewP);
  if Not FCacheData.Add( LNewP ) then raise ECacheMem.Create(Format('Inconsistent LoadData CacheData duplicate for %s',[LNewP^.ToString]));
  Inc(FCacheDataSize,Length(LNewP^.buffer));
  Inc(FCacheDataBlocks);
  //
  if (LNewP^.pendingToSave) then begin
    inc(FPendingToSaveBytes,LNewP^.GetSize);
  end;

  Move(LNewP^.buffer[ AStartPos-LNewP^.startPos ],ABuffer,ASize);

  CheckMaxMemUsage;
end;

procedure TCacheMem.SaveToCache(const ABuffer: TBytes; AStartPos: Integer; AMarkAsPendingToSave : Boolean);
begin
  SaveToCache(ABuffer[0],Length(ABuffer),AStartPos,AMarkAsPendingToSave);
end;

function TCacheMem.ToString: String;
var
  LLines : TStrings;
  LPct : Double;
  PCurrent : PCacheMemData;
begin
  LLines := TStringList.Create;
  try
    LLines.Add(Format('%s.ToString',[ClassName]));
    PCurrent := FCacheData.FindLowest;
    while (Assigned(PCurrent)) do begin
      LLines.Add( PCurrent^.ToString );
      PCurrent := FCacheData.FindSuccessor(PCurrent);
    end;
    if FCacheDataSize>0 then LPct := (FPendingToSaveBytes / FCacheDataSize)*100
    else LPct := 0.0;
    LLines.Add(Format('Total size %d bytes in %d blocks - Pending to Save %d bytes (%.2n%%)',[FCacheDataSize,FCacheDataBlocks,FPendingToSaveBytes,LPct]));
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

procedure TCacheMem.SaveToCache(const ABuffer; ASize, AStartPos: Integer; AMarkAsPendingToSave : Boolean);
var
  LNewP, PCurrent, PToDelete : PCacheMemData;
  LLastAddedPosition, LBytesCount : Integer;
begin
  if ASize<0 then raise ECacheMem.Create(Format('Invalid save size %d',[ASize]));
  if ASize=0 then Exit;

  if (FindCacheMemDataByPosition(AStartPos,PCurrent)) then begin
    if (PCurrent^.GetSize - (AStartPos - PCurrent^.startPos)) >= ASize then begin
      // PStart has all needed info
      Move(ABuffer,PCurrent^.buffer[ AStartPos - PCurrent^.startPos ], ASize);
      if (Not PCurrent^.pendingToSave) and (AMarkAsPendingToSave) then begin
        PCurrent^.pendingToSave := True;
        inc(FPendingToSaveBytes,PCurrent^.GetSize);
      end;
      PCurrent^.MarkAsUsed(Self,PCurrent);
      Exit;
    end;
  end;

  // Will need to create a new "linar struct" because not found a linear struct previously
  New( LNewP );
  try
    LNewP.Clear;
    SetLength(LNewP^.buffer, ASize);
    LNewP.startPos := AStartPos;
    LNewP^.pendingToSave := AMarkAsPendingToSave;

    LLastAddedPosition := AStartPos - 1;
    while (Assigned(PCurrent)) and ( (LLastAddedPosition+1) < (LNewP^.GetEndPos) ) do begin
      if (PCurrent^.GetEndPos <= LLastAddedPosition) then PCurrent := FCacheData.FindSuccessor( PCurrent )
      else if (PCurrent^.startPos > LNewP^.GetEndPos) then break
      else begin
        // PCurrent will be used:
        if (PCurrent^.startPos <= LLastAddedPosition) then begin
          // PCurrent start before, increase buffer and set startPos
          SetLength(LNewP^.buffer ,Length(LNewP^.buffer) + (LLastAddedPosition - PCurrent^.startPos + 1));
          LNewP.startPos := PCurrent^.startPos;
          Move(PCurrent^.buffer[ 0 ],LNewP^.buffer[ 0 ], (LLastAddedPosition - PCurrent^.startPos +1));
        end;
        // At this point (LLastAddedPosition+1 = PCurrent^.startPos)
        // Add available data
        if PCurrent^.GetEndPos>(LNewP^.GetEndPos) then begin
          // Will need to increase buffer size:
          LBytesCount := (PCurrent^.GetEndPos - LNewP^.GetEndPos);
          SetLength( LNewP^.buffer , LNewP^.GetSize + LBytesCount );
          Move(PCurrent^.buffer[ PCurrent^.GetSize - LBytesCount ],LNewP^.buffer[ LNewP^.GetSize - LBytesCount ], LBytesCount);
        end;

        // Has been used, delete
        LNewP.pendingToSave := (LNewP^.pendingToSave) or (PCurrent^.pendingToSave);
        PToDelete := PCurrent;
        PCurrent := FCacheData.FindSuccessor(PCurrent);
        Delete( PToDelete );
      end;
    end;
    // At this point LNewP^.buffer startPos <= AStartPos and LNewP^.buffer Size >= ASize
    Move( ABuffer, LNewP^.buffer[ (LLastAddedPosition+1) - LNewP^.startPos ], ASize );
  Except
    on E:Exception do begin
      LNewP.Clear;
      Dispose(LNewP);
      Raise;
    end;
  end;

  // Save new
  LNewP^.MarkAsUsed(Self,LNewP);
  if Not FCacheData.Add(LNewP) then raise ECacheMem.Create(Format('Inconsistent SaveToCache CacheData duplicate for %s',[LNewP^.ToString]));
  Inc(FCacheDataSize,Length(LNewP^.buffer));
  Inc(FCacheDataBlocks);
  //
  if (LNewP^.pendingToSave) then begin
    inc(FPendingToSaveBytes,LNewP^.GetSize);
  end;

  CheckMaxMemUsage;
end;

{ TCacheMemData }

procedure TCacheMemData.Clear;
begin
  SetLength(Self.buffer,0);
  Self.parent := Nil;
  Self.left := Nil;
  Self.right := Nil;
  Self.balance := 0;
  //
  Self.startPos := 0;
  Self.pendingToSave := False;
  Self.used_previous := Nil;
  Self.used_next := Nil;
end;

procedure TCacheMemData.DoMark(const ACacheMem: TCacheMem; AMySelfPointer: PCacheMemData; AAddToList: Boolean);
{
    O = ACacheMem.FOldest
    N = ACacheMem.FNewest

    O       N
    A - B - C   ( D = New CacheMem )
}

begin
  if Assigned(Self.used_previous) then begin
    // B or C
    if (Self.used_previous^.used_next<>AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent previous.next<>MySelf in %s',[Self.ToString]));
    if (ACacheMem.FOldestUsed = AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent B,C Oldest = MySelf in %s',[Self.ToString]));
    if Assigned(Self.used_next) then begin
      // B only
      if (Self.used_next^.used_previous<>AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent B next.previous<>MySelf in %s',[Self.ToString]));
      if (ACacheMem.FNewestUsed = AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent B Newest = MySelf in %s',[Self.ToString]));
      Self.used_previous^.used_next := Self.used_next;
      Self.used_next^.used_previous := Self.used_previous;
    end else begin
      // C only
      if (ACacheMem.FNewestUsed <> AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent Newest <> MySelf in %s',[Self.ToString]));
      if (Not AAddToList) then begin
        Self.used_previous^.used_next := Nil;
      end;
    end;
  end else if assigned(Self.used_next) then begin
    // A
    if (Self.used_next^.used_previous<>AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent A next.previous<>MySelf in %s',[Self.ToString]));
    if (ACacheMem.FOldestUsed <> AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent Oldest <> MySelf in %s',[Self.ToString]));
    if (ACacheMem.FNewestUsed = AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent A Newest = MySelf in %s',[Self.ToString]));
    Self.used_next^.used_previous := Self.used_previous; // = NIL
    ACacheMem.FOldestUsed:=Self.used_next; // Set oldest
  end else begin
    // D
    if (ACacheMem.FOldestUsed = AMySelfPointer) and (ACacheMem.FNewestUsed = AMySelfPointer) then begin
      // D is the "only one", no previous, no next, but added or removed
      if (Not AAddToList) then begin
        ACacheMem.FOldestUsed := Nil;
      end;
    end else begin
      if (ACacheMem.FOldestUsed = AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent D Oldest = MySelf in %s',[Self.ToString]));
      if (ACacheMem.FNewestUsed = AMySelfPointer) then raise ECacheMem.Create(Format('Inconsistent D Newest = MySelf in %s',[Self.ToString]));
    end;
    if Not Assigned(ACacheMem.FOldestUsed) and (AAddToList) then begin
        // D is first one to be added
        ACacheMem.FOldestUsed := AMySelfPointer; // Set oldest
    end;
  end;
  if Assigned(ACacheMem.FNewestUsed) then begin
    if Assigned(ACacheMem.FNewestUsed^.used_next) then raise ECacheMem.Create(Format('Inconsistent Newest.next <> Nil in %s',[Self.ToString]));
  end;
  // Update Self.used_previous and Self.used_next
  if AAddToList then begin
    // Adding to list
    if (ACacheMem.FNewestUsed<>AMySelfPointer) then begin
      // Link to previous if newest <> MySelf
      Self.used_previous := ACacheMem.FNewestUsed;
    end;
    if Assigned(ACacheMem.FNewestUsed) then begin
      ACacheMem.FNewestUsed^.used_next:= AMySelfPointer;
    end;
    ACacheMem.FNewestUsed:=AMySelfPointer;
  end else begin
    // Removing from list
    if ACacheMem.FNewestUsed = AMySelfPointer then begin
      if (Assigned(Self.used_next)) then raise ECacheMem.Create(Format('Inconsistent next <> Nil when Self = Newest in %s',[Self.ToString]));
      ACacheMem.FNewestUsed := Self.used_previous;
    end;
    Self.used_previous := Nil;
  end;
  Self.used_next := Nil;
end;


function TCacheMemData.GetEndPos: Integer;
begin
  Result := Self.startPos + Self.GetSize - 1;
end;

function TCacheMemData.GetSize: Integer;
begin
  Result := Length(Self.buffer);
end;

procedure TCacheMemData.MarkAsUsed(const ACacheMem: TCacheMem; AMySelfPointer : PCacheMemData);
begin
  DoMark(ACacheMem,AMySelfPointer,True);
end;

procedure TCacheMemData.UnMark(const ACacheMem: TCacheMem; AMySelfPointer: PCacheMemData);
begin
  DoMark(ACacheMem,AMySelfPointer,False);
end;

function TCacheMemData.ToString: String;
var i : Integer;
begin
  Result := Format('%d bytes from %d to %d',[Self.GetSize,Self.startPos,Self.GetEndPos]);
  if Self.pendingToSave then Result := Result + ' (updated)';
  Result := Result +' [';
  i := 0;
  while (Length(Result)<100) and (i<Self.GetSize) do begin
    if i>0 then Result := Result + ','+IntToStr(Self.buffer[i])
    else Result := Result + IntToStr(Self.buffer[i]);
    inc(i);
  end;
  if i<Self.GetSize then Result := Result + '...';
  Result := Result +']';
end;

{$IFDEF ABSTRACTMEM_ENABLE_STATS}
{ TCacheMemStats }

procedure TCacheMemStats.Clear;
begin
  flushCount := 0;
  flushSize := 0;
  flushElapsedMillis := 0;
  freememCount := 0;
  freememSize := 0;
  freememElaspedMillis := 0;
  reusedCacheMemDataCount := 0;
  reusedCacheMemDataBytes := 0;
end;

function TCacheMemStats.ToString: String;
begin
  Result := Format('CacheMemStats Reused:%d (%d bytes) - Flush:%d (%d bytes) %d millis - FreeMem:%d (%d bytes) %d millis',
     [Self.reusedCacheMemDataCount,Self.reusedCacheMemDataBytes,
      Self.flushCount,Self.flushSize,Self.flushElapsedMillis,
      Self.freememCount,Self.freememSize,
      Self.freememElaspedMillis]);
end;
{$ENDIF}

{ TCacheMemDataTree }

function _TCacheMemDataTree_Compare(const Left, Right: PCacheMemData): Integer;
begin
  Result := Left^.startPos - Right^.startPos;
end;

function TCacheMemDataTree.AreEquals(const ANode1, ANode2: PCacheMemData): Boolean;
begin
  Result := ANode1 = ANode2;
end;

procedure TCacheMemDataTree.ClearNode(var ANode: PCacheMemData);
begin
  ANode := Nil;
end;

procedure TCacheMemDataTree.ClearPosition(var ANode: PCacheMemData; APosition: TAVLTreePosition);
begin
  case APosition of
    poParent: ANode.parent := Nil;
    poLeft: ANode.left := Nil;
    poRight: ANode.right := Nil;
  end;
end;

constructor TCacheMemDataTree.Create;
begin
  FRoot := Nil;
  inherited Create(_TCacheMemDataTree_Compare,False);
end;

procedure TCacheMemDataTree.DisposeNode(var ANode: PCacheMemData);
begin
  if Not Assigned(ANode) then Exit;
  Dispose( ANode );
  ANode := Nil;
end;

function TCacheMemDataTree.GetBalance(const ANode: PCacheMemData): Integer;
begin
  Result := ANode.balance;
end;

function TCacheMemDataTree.GetPosition(const ANode: PCacheMemData;
  APosition: TAVLTreePosition): PCacheMemData;
begin
  case APosition of
    poParent: Result := ANode.parent;
    poLeft: Result := ANode.left;
    poRight: Result := ANode.right;
  end;
end;

function TCacheMemDataTree.GetRoot: PCacheMemData;
begin
  Result := FRoot;
end;

function TCacheMemDataTree.HasPosition(const ANode: PCacheMemData;
  APosition: TAVLTreePosition): Boolean;
begin
  Result := Assigned(GetPosition(ANode,APosition));
end;

function TCacheMemDataTree.IsNil(const ANode: PCacheMemData): Boolean;
begin
  Result := Not Assigned(ANode);
end;

procedure TCacheMemDataTree.SetBalance(var ANode: PCacheMemData; ANewBalance: Integer);
begin
  ANode.balance := ANewBalance;
end;

procedure TCacheMemDataTree.SetPosition(var ANode: PCacheMemData;
  APosition: TAVLTreePosition; const ANewValue: PCacheMemData);
begin
  case APosition of
    poParent: ANode.parent := ANewValue;
    poLeft: ANode.left := ANewValue;
    poRight: ANode.right := ANewValue;
  end;
end;

procedure TCacheMemDataTree.SetRoot(const Value: PCacheMemData);
begin
  FRoot := Value;
end;

function TCacheMemDataTree.ToString(const ANode: PCacheMemData): String;
begin
  Result := ANode.ToString;
end;

end.
