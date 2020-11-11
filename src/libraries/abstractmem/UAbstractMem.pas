unit UAbstractMem;

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
  UAbstractBTree;

{$I ./ConfigAbstractMem.inc }

Type
  TAbstractMemPosition = Integer;

  TAMZone = record
    position : TAbstractMemPosition;
    size : Integer;
    procedure Clear;
    function ToString : String;
  end;

  EAbstractMem = Class(Exception);

  TAbstractMem = Class;

  TAbstractMemMemoryLeaksComparer = function(const ABuffer1; ABufferSize1:Integer; const AData2: Integer): Integer;

  TAbstractMemMemoryLeaksNode = record
    myPosition,       // Position in the AbstractMem
    parentPosition,
    leftPosition,
    rigthPosition : TAbstractMemPosition;
    balance : ShortInt;
    units : Integer; // units equals to "4 bytes packet", 1=4 bytes 2=8 bytes ...
    function GetSize : Integer;
    procedure SetSize(ABytesSize : Integer); // ABytesSize will be converted to units
    function GetPosition(APosition : TAVLTreePosition) : TAbstractMemPosition;
    procedure SetPosition(APosition : TAVLTreePosition; AMemPosition : TAbstractMemPosition);
    procedure ReadFromMem(AMyPosition : TAbstractMemPosition; AAbstractMem : TAbstractMem);
    procedure WriteToMem(AAbstractMem : TAbstractMem);
    procedure Clear;
    function ToString : String;
  end;


  TAbstractMemMemoryLeaks = Class( TAVLAbstractTree<TAbstractMemMemoryLeaksNode> )
  private
    FAbstractMem : TAbstractMem;
    FRootPosition : TAbstractMemPosition;
  protected
    function GetRoot: TAbstractMemMemoryLeaksNode; override;
    procedure SetRoot(const Value: TAbstractMemMemoryLeaksNode); override;
    function HasPosition(const ANode : TAbstractMemMemoryLeaksNode; APosition : TAVLTreePosition) : Boolean; override;
    function GetPosition(const ANode : TAbstractMemMemoryLeaksNode; APosition : TAVLTreePosition) : TAbstractMemMemoryLeaksNode; override;
    procedure SetPosition(var ANode : TAbstractMemMemoryLeaksNode; APosition : TAVLTreePosition; const ANewValue : TAbstractMemMemoryLeaksNode); override;
    procedure ClearPosition(var ANode : TAbstractMemMemoryLeaksNode; APosition : TAVLTreePosition); override;
    function GetBalance(const ANode : TAbstractMemMemoryLeaksNode) : Integer; override;
    procedure SetBalance(var ANode : TAbstractMemMemoryLeaksNode; ANewBalance : Integer); override;
    function AreEquals(const ANode1, ANode2 : TAbstractMemMemoryLeaksNode) : Boolean; override;
    procedure ClearNode(var ANode : TAbstractMemMemoryLeaksNode); override;
    procedure DisposeNode(var ANode : TAbstractMemMemoryLeaksNode); override;
  public
    function IsNil(const ANode : TAbstractMemMemoryLeaksNode) : Boolean; override;
    function ToString(const ANode: TAbstractMemMemoryLeaksNode) : String; override;
    constructor Create(AAbstractMem : TAbstractMem; ARootPosition : TAbstractMemPosition); reintroduce;
    destructor Destroy; override;
  End;

  TAbstractMemZoneType = (amzt_unknown, amzt_memory_leak, amzt_used);

  { TAbstractMem }

  TAbstractMem = Class
  private
    FReadOnly : Boolean;
    FHeaderInitialized : Boolean;
    FInitialPosition : Integer;
    FNextAvailablePos : Integer;
    FMaxAvailablePos : Integer;
    FMemLeaks : TAbstractMemMemoryLeaks;
    //
  protected
    FLock : TCriticalSection;
    function AbsoluteWrite(const AAbsolutePosition : Int64; const ABuffer; ASize : Integer) : Integer; virtual; abstract;
    function AbsoluteRead(const AAbsolutePosition : Int64; var ABuffer; ASize : Integer) : Integer; virtual; abstract;
    procedure DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos : Integer; ANeedSize : Integer); virtual; abstract;
    //
    function PositionToAbsolute(const APosition : Integer) : Int64;
    procedure IncreaseSize(ANeedSize : Integer);
    //
    function GetZoneType(APosition : TAbstractMemPosition; out AAMZone : TAMZone) : TAbstractMemZoneType;
    procedure CheckInitialized(AWantsToWrite : Boolean);
    function IsAbstractMemInfoStable : Boolean; virtual;
    procedure SaveHeader;
  public
    procedure Write(const APosition : Integer; const ABuffer; ASize : Integer); overload; virtual;
    function Read(const APosition : Integer; var ABuffer; ASize : Integer) : Integer; overload; virtual;

    Constructor Create(AInitialPosition : Integer; AReadOnly : Boolean); virtual;
    Destructor Destroy; override;
    //
    procedure ClearContent;
    //
    function New(AMemSize : Integer) : TAMZone; virtual;
    procedure Dispose(const AAMZone : TAMZone); overload;
    procedure Dispose(const APosition : TAbstractMemPosition); overload;
    function GetUsedZoneInfo(const APosition : TAbstractMemPosition; ACheckForUsedZone : Boolean; out AAMZone : TAMZone) : Boolean;
    function ToString : String; override;
    function CheckConsistency(const AStructure : TStrings; out ATotalUsedSize, ATotalUsedBlocksCount, ATotalLeaksSize, ATotalLeaksBlocksCount : Integer) : Boolean;
    function ReadFirstData(var AFirstDataZone : TAMZone; var AFirstData : TBytes) : Boolean;
    class function GetAbstractMemVersion : String;
    property ReadOnly : Boolean read FReadOnly;
    procedure SaveToStream(AStream : TStream);
    procedure CopyFrom(ASource : TAbstractMem);
    function GetStatsReport(AClearStats : Boolean) : String; virtual;
  End;

  TMem = Class(TAbstractMem)
  private
    FMem : TBytes;
  protected
    function AbsoluteWrite(const AAbsolutePosition : Int64; const ABuffer; ASize : Integer) : Integer; override;
    function AbsoluteRead(const AAbsolutePosition : Int64; var ABuffer; ASize : Integer) : Integer; override;
    procedure DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos : Integer; ANeedSize : Integer); override;
  public
    Constructor Create(AInitialPosition : Integer; AReadOnly : Boolean); override;
  End;

  TAbstractMemAVLTreeNodeInfo = record
    parentPosition,
    leftPosition,
    rigthPosition : TAbstractMemPosition;
    balance : ShortInt;
    procedure Clear;
    function ToString : String;
  end;
    //
  TAbstractMemAVLTreeNodeInfoClass = Class
    class function ReadFromMem(AMyPosition : TAbstractMemPosition; AAbstractMem : TAbstractMem) : TAbstractMemAVLTreeNodeInfo;
    class procedure WriteToMem(AMyPosition : TAbstractMemPosition; AAbstractMem : TAbstractMem; const ANodeInfo : TAbstractMemAVLTreeNodeInfo);
    class procedure ClearPosition(AMyPosition : TAbstractMemPosition; AAbstractMem : TAbstractMem; APosition: TAVLTreePosition);
    class function GetPosition(AMyPosition : TAbstractMemPosition; AAbstractMem : TAbstractMem; APosition: TAVLTreePosition) : TAbstractMemPosition;
    class procedure SetPosition(AMyPosition : TAbstractMemPosition; AAbstractMem : TAbstractMem; APosition: TAVLTreePosition; ANewPosition : TAbstractMemPosition);
    class function GetBalance(AMyPosition : TAbstractMemPosition; AAbstractMem : TAbstractMem) : ShortInt;
    class procedure SetBalance(AMyPosition : TAbstractMemPosition; AAbstractMem : TAbstractMem; ANewBalance : ShortInt);
    class function GetSize : Integer;
  end;


implementation

const
  CT_Magic : Array[0..5] of byte = (7,6,5,4,3,2);
  CT_IsStable = 1;
  CT_Is_NOT_Stable = 0;
  CT_Version = 1;
  CT_HeaderSize = 16; // Magic(7) + Version(1) + MemLeak_root_position(4) + NextAvailable_position(4) = 16 bytes
  CT_ExtraSizeForUsedZoneType = 4;

{ TAbstractMem }

function TAbstractMem.CheckConsistency(const AStructure: TStrings; out ATotalUsedSize, ATotalUsedBlocksCount, ATotalLeaksSize, ATotalLeaksBlocksCount : Integer) : Boolean;
var LPosition : TAbstractMemPosition;
  LZone : TAMZone;
begin
  // Will check since first position:
  FLock.Acquire;
  Try
    ATotalUsedSize := 0;
    ATotalUsedBlocksCount := 0;
    ATotalLeaksSize := 0;
    ATotalLeaksBlocksCount := 0;
    LPosition := CT_HeaderSize;
    Result := True;
    while (Result) and (LPosition < FNextAvailablePos) do begin
      case GetZoneType(LPosition,LZone) of
        amzt_memory_leak : begin
          if Assigned(AStructure) then AStructure.Add( Format('%d to %d mem leak %d bytes',[LPosition,LZone.position + LZone.size,LZone.size]));
          Inc(LPosition, LZone.size);
          inc(ATotalLeaksSize,LZone.size);
          inc(ATotalLeaksBlocksCount);
        end;
        amzt_used : begin
          if Assigned(AStructure) then AStructure.Add( Format('%d to %d used %d bytes',[LPosition,LZone.position + LZone.size, LZone.size]));
          inc(LPosition, LZone.size + CT_ExtraSizeForUsedZoneType);
          inc(ATotalUsedSize,LZone.size + CT_ExtraSizeForUsedZoneType);
          inc(ATotalUsedBlocksCount);
        end;
      else
        if Assigned(AStructure) then AStructure.Add( Format('Consisteny error at %d (End position: %d)',[LPosition,FNextAvailablePos]));
        Result := False;
      end;
    end;
  Finally
    FLock.Release;
  End;
end;

procedure TAbstractMem.CheckInitialized(AWantsToWrite : Boolean);
begin
  if (AWantsToWrite and FReadOnly) then raise EAbstractMem.Create('Cannot write to a ReadOnly AbstractMem');
  if Not FHeaderInitialized then begin
    // Needs to write
    if FReadOnly then raise EAbstractMem.Create('Cannot initialize a ReadOnly AbstractMem');
    //
    IncreaseSize(CT_HeaderSize);
    // Write Header:
    SaveHeader;
  end;
end;

procedure TAbstractMem.ClearContent;
var LNewRoot : TAbstractMemMemoryLeaksNode;
begin
  // Will erase ALL content creating a new null header
  if FReadOnly then raise EAbstractMem.Create('Cannot ClearContent on a ReadOnly AbstractMem');
  CheckInitialized(True);

  FNextAvailablePos := CT_HeaderSize; // By Default

  FMaxAvailablePos := 0;
  IncreaseSize(0);

  FHeaderInitialized := False;
  CheckInitialized(True);

  LNewRoot.Clear;
  FMemLeaks.SetRoot( LNewRoot );
end;

procedure TAbstractMem.CopyFrom(ASource: TAbstractMem);
var LBuff : TBytes;
  iPos, LBuffDataCount : Integer;
  LMemLeakRelativeRootPos : TAbstractMemPosition;
begin
  ASource.FLock.Acquire;
  Self.FLock.Acquire;
  try
    ClearContent;

    CheckInitialized(True);
    IncreaseSize(ASource.FNextAvailablePos);
    FNextAvailablePos := ASource.FNextAvailablePos;

    SetLength(LBuff,1024*1024);
    iPos := 0;
    while (iPos < ASource.FNextAvailablePos) do begin
      LBuffDataCount := (ASource.FNextAvailablePos - iPos);
      if LBuffDataCount>Length(LBuff) then LBuffDataCount := Length(LBuff);
      ASource.Read(iPos,LBuff[0],LBuffDataCount);
      Self.Write(iPos,LBuff[0],LBuffDataCount);
      inc(iPos,LBuffDataCount);
    end;

    LMemLeakRelativeRootPos := ASource.FMemLeaks.FRootPosition;
    FMemLeaks.Free;
    FMemLeaks := TAbstractMemMemoryLeaks.Create(Self,LMemLeakRelativeRootPos);

    SaveHeader;
  finally
    Self.FLock.Release;
    ASource.FLock.Release;
  end;
end;

constructor TAbstractMem.Create(AInitialPosition: Integer; AReadOnly : Boolean);
var LBuffer : TBytes;
  LMemLeakRelativeRootPos : TAbstractMemPosition;
  LOk : Boolean;
begin
  FMemLeaks := Nil;
  FReadOnly := AReadOnly;
  LMemLeakRelativeRootPos := 0;
  FInitialPosition := AInitialPosition;
  //
  FNextAvailablePos := CT_HeaderSize; // By Default

  FMaxAvailablePos := 0;

  FLock := TCriticalSection.Create;
  // Try to initialize
  // Magic: 7 bytes
  // version: 1 byte
  // START OF FIRST BLOCK 1 = Header info
  FHeaderInitialized := True;
  LOk := False;
  Try
    SetLength(LBuffer,CT_HeaderSize);
    if Read(0,LBuffer[0],CT_HeaderSize)=CT_HeaderSize then begin
      if CompareMem(@LBuffer[0],@CT_Magic[0],6) then begin
        LOk := LBuffer[6] = CT_IsStable;
        if (LOk) And (LBuffer[7] = CT_Version) then begin
          Move(LBuffer[8],LMemLeakRelativeRootPos,4);
          Move(LBuffer[12],FNextAvailablePos,4);
          //
          LOk := (FNextAvailablePos >= CT_HeaderSize) and (LMemLeakRelativeRootPos<FNextAvailablePos);
        end;
      end;
    end;
  Finally
    FHeaderInitialized := LOk;
  End;
  FMemLeaks := TAbstractMemMemoryLeaks.Create(Self,LMemLeakRelativeRootPos);
end;

destructor TAbstractMem.Destroy;
begin
  FreeAndNil(FMemLeaks);
  FreeAndNil(FLock);
  inherited;
end;

procedure TAbstractMem.Dispose(const APosition: TAbstractMemPosition);
var LZone : TAMZone;
begin
  if APosition<=CT_HeaderSize then raise EAbstractMem.Create('Dispose: Invalid position '+IntToStr(APosition));
  // @[APosition] - 4 bytes = position to size
  LZone.position := APosition;
  if Read(APosition - 4,LZone.size,4) <> 4 then raise EAbstractMem.Create('Dispose: Cannot read size');
  Dispose(LZone);
end;

procedure TAbstractMem.Dispose(const AAMZone: TAMZone);
var LNewMemLeak : TAbstractMemMemoryLeaksNode;
  LZoneSize : UInt32;
begin
  CheckInitialized(True);

  LNewMemLeak.Clear;
  LNewMemLeak.myPosition := AAMZone.position - 4;
  LNewMemLeak.SetSize(AAMZone.size+4);

  if Read(LNewMemLeak.myPosition,LZoneSize,4)<>4 then raise EAbstractMem.Create('Dispose: Cannot read size');
  if Integer(LZoneSize)<>AAMZone.size then raise EAbstractMem.Create(Format('Dispose: Invalid size %d (expected %d) at position %d',[LZoneSize,AAMZone.size,AAMZone.position]));

  // Check valid units based on size
  if (LNewMemLeak.GetSize<>AAMZone.size+4) then raise EAbstractMem.Create(Format('Dispose: Invalid size %d at position %d',[AAMZone.size,AAMZone.position]));
  FLock.Acquire;
  Try
    // Save mem leak to mem
    LNewMemLeak.WriteToMem(Self);
    // Add leak to BTree
    FMemLeaks.Add( LNewMemLeak );
  Finally
    FLock.Release;
  End;
end;

class function TAbstractMem.GetAbstractMemVersion: String;
begin
  Result := ClassName+' v'+FloatToStr(CT_ABSTRACTMEM_VERSION);
end;

function TAbstractMem.GetStatsReport(AClearStats: Boolean): String;
begin
  Result := '';
end;

function TAbstractMem.GetUsedZoneInfo(const APosition: TAbstractMemPosition; ACheckForUsedZone: Boolean; out AAMZone: TAMZone): Boolean;
begin
  if (ACheckForUsedZone) then begin
    if GetZoneType(APosition - CT_ExtraSizeForUsedZoneType,AAMZone)<>amzt_used then Exit(False)
    else Exit(True);
  end else begin
    AAMZone.position := APosition;
    if Read(APosition - CT_ExtraSizeForUsedZoneType,AAMZone.size,4)<>4 then Exit(False); // This is the CT_ExtraSizeForUsedZoneType = 4 bytes for size indicator
    Result := (AAMZone.position + AAMZone.size <= FNextAvailablePos)  And ( ((((AAMZone.size-1) DIV 4)+1)*4) = AAMZone.size );
  end;
end;

function TAbstractMem.GetZoneType(APosition: TAbstractMemPosition; out AAMZone : TAMZone): TAbstractMemZoneType;
var LZone : TAMZone;
  LMemLeak, LSearchedMemLeak : TAbstractMemMemoryLeaksNode;
begin
  Result := amzt_unknown;
  AAMZone.position := APosition;
  AAMZone.size := 0;
  LZone.position := (((APosition-1) DIV 4)+1)*4;
  if (LZone.position <> APosition) or (LZone.position<CT_HeaderSize) or (LZone.position>=FNextAvailablePos) then Exit;
  // Check if Memory leak
  LMemLeak.myPosition := LZone.position;
  LMemLeak.ReadFromMem(LMemLeak.myPosition,Self);
  LSearchedMemLeak := FMemLeaks.Find(LMemLeak);
  if FMemLeaks.IsNil(LSearchedMemLeak) then begin
    if Read(APosition,LZone.size,4)<>4 then Exit; // This is the CT_ExtraSizeForUsedZoneType = 4 bytes for size indicator
    if (LZone.position + CT_ExtraSizeForUsedZoneType + LZone.size <= FNextAvailablePos)
      And ( ((((LZone.size-1) DIV 4)+1)*4) = LZone.size ) then begin
      Result := amzt_used;
      AAMZone.position := LZone.position + CT_ExtraSizeForUsedZoneType;
      AAMZone.size := LZone.size;
    end;
  end else begin
    AAMZone.size := LSearchedMemLeak.GetSize;
    Result := amzt_memory_leak;
  end;
end;

procedure TAbstractMem.IncreaseSize(ANeedSize: Integer);
  // This will guarantee at the end that FMaxAvailablePos-FNextAvailablePos+1 >= ANeededSize
var LTmpNextAvailablePos, LTmpMaxAvailablePos : Integer;
begin
  if FMaxAvailablePos-FNextAvailablePos+1 >= ANeedSize then Exit;
  LTmpNextAvailablePos := FNextAvailablePos;
  LTmpMaxAvailablePos := FMaxAvailablePos;
  DoIncreaseSize(LTmpNextAvailablePos,LTmpMaxAvailablePos,ANeedSize);
  // Check
  if (LTmpNextAvailablePos + LTmpMaxAvailablePos + 1 < ANeedSize) then raise EAbstractMem.Create(FormaT('IncreaseSize error. Needed %d obtained from %d to %d = %d',
    [ANeedSize,LTmpNextAvailablePos,LTmpMaxAvailablePos,(LTmpMaxAvailablePos-LTmpNextAvailablePos+1)]));
  //
  FNextAvailablePos := LTmpNextAvailablePos;
  FMaxAvailablePos := LTmpMaxAvailablePos;
  if ANeedSize>0 then SaveHeader;
end;

function TAbstractMem.IsAbstractMemInfoStable: Boolean;
begin
  Result := True;
end;

function TAbstractMem.New(AMemSize: Integer): TAMZone;
var LNeededMemSize : Integer;
  LMemLeakToFind, LMemLeakFound : TAbstractMemMemoryLeaksNode;
begin
  CheckInitialized(True);
  // AMemSize must be a value stored in 3 bytes (24 bits) where each value is a "unit" of 4 bytes, so:
  // (AMemSize > 0) and (AMemSize <= ((((2^24)-1)*4) - 4) )
  if (AMemSize<=0) or (AMemSize>=67108860) then raise EAbstractMem.Create('Invalid new size: '+IntToStr(AMemSize));

  FLock.Acquire;
  Try
    // First 4 bytes will be "how many units"
    LNeededMemSize := AMemSize + 4;
    // Minimum size is always 16 bytes (Mem needed for a mem leak = 4 * 4 bytes)
    if LNeededMemSize<16 then LNeededMemSize := 16
    else LNeededMemSize := LNeededMemSize;
    // Round LMemSize to a 4 bytes packet
    LNeededMemSize := (((LNeededMemSize-1) DIV 4)+1)*4;

    LMemLeakToFind.Clear;
    LMemLeakToFind.SetSize(LNeededMemSize);

    LMemLeakFound := FMemLeaks.Find( LMemLeakToFind );
    if Not FMemLeaks.IsNil(LMemLeakFound) then begin
      // Found a Memory leak with this size, REUSE
      Result.position := LMemLeakFound.myPosition + 4;
      Result.size := LMemLeakFound.GetSize - 4;
      // Remove leak
      FMemLeaks.Delete( LMemLeakFound );
    end else begin
      // Need a new available zone
      IncreaseSize( LNeededMemSize );
      //
      Result.position := FNextAvailablePos + 4; // 4 = "units"
      FNextAvailablePos := FNextAvailablePos + LNeededMemSize;
      Result.size := LNeededMemSize - 4;
      SaveHeader; // NextAvailablePos updated, save changes
    end;
    // Save "unit"
    Write(Result.position - 4,Result.size,4);
  Finally
    FLock.Release;
  End;
end;

function TAbstractMem.PositionToAbsolute(const APosition: Integer): Int64;
begin
  Result := FInitialPosition + APosition;
end;

procedure TAbstractMem.SaveHeader;
var LBuffer : TBytes;
  LUInt32 : UInt32;
begin
  if FReadOnly then raise EAbstractMem.Create('Cannot save Haeder on a ReadOnly AbstractMem');
  // Write Header:
  SetLength(LBuffer,CT_HeaderSize);
  Move(CT_Magic[0],LBuffer[0],6);
  if IsAbstractMemInfoStable then begin
    LBuffer[6] := CT_IsStable;
  end else begin
    LBuffer[6] := CT_Is_NOT_Stable;
  end;
  LBuffer[7] := CT_Version;
  LUInt32 := FMemLeaks.Root.myPosition;
  Move(LUInt32,LBuffer[8],4);  // position to memleak btree root
  LUInt32 := FNextAvailablePos;
  Move(LUInt32,LBuffer[12],4); // next available pos
  //
  FHeaderInitialized := True;  // Set before call to Write
  //
  Write(0,LBuffer[0],Length(LBuffer));
end;

procedure TAbstractMem.SaveToStream(AStream: TStream);
var LBuffer : TBytes;
  i : Integer;
  LNextStart : Integer;
begin
  CheckInitialized(False);
  LNextStart := 0;
  SetLength(LBuffer,1024*1024);
  FLock.Acquire;
  Try
    while (LNextStart < FNextAvailablePos) do begin
      i := FNextAvailablePos - LNextStart;
      if (i>Length(LBuffer)) then i := Length(LBuffer);
      Read(LNextStart,LBuffer[0],i);
      AStream.Write(LBuffer[0],i);
      inc(LNextStart,i);
    end;
  Finally
    FLock.Release;
  End;
end;

function TAbstractMem.ToString: String;
var LAnalize : TStrings;
  LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount : Integer;
begin
  LAnalize := TStringList.Create;
  try
    if Not CheckConsistency(LAnalize,LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount) then begin
      LAnalize.Add('CONSISTENCY ERROR FOUND');
    end else begin
      LAnalize.Clear;
    end;
    LAnalize.Add(Format('%s Start position %d - Used %d bytes in %d blocks - Available %d bytes in %d blocks',[Self.GetAbstractMemVersion, FInitialPosition,LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount]));
    Result := LAnalize.Text;
  finally
    LAnalize.Free;
  end;
end;

function TAbstractMem.Read(const APosition: Integer; var ABuffer; ASize: Integer): Integer;
begin
  FLock.Acquire;
  try
    if Not FHeaderInitialized then Result := 0
    else Result := AbsoluteRead(PositionToAbsolute(APosition),ABuffer,ASize);
  Finally
    FLock.Release;
  End;
end;

function TAbstractMem.ReadFirstData(var AFirstDataZone: TAMZone; var AFirstData: TBytes): Boolean;
var LPosition : TAbstractMemPosition;
begin
  LPosition := CT_HeaderSize;
  Result := False;
  AFirstDataZone.Clear;
  SetLength(AFirstData,0);
  if (LPosition < FNextAvailablePos) then begin
    case GetZoneType(LPosition,AFirstDataZone) of
      amzt_used : begin
        SetLength(AFirstData,AFirstDataZone.size);
        Result := Read(AFirstDataZone.position,AFirstData[0],Length(AFirstData))=AFirstDataZone.size;
      end;
    end;
  end;
end;

procedure TAbstractMem.Write(const APosition: Integer; const ABuffer; ASize: Integer);
begin
  FLock.Acquire;
  Try
    CheckInitialized(True);
    if AbsoluteWrite(PositionToAbsolute(APosition),ABuffer,ASize)<>ASize then raise EAbstractMem.Create('Cannot write expected size');
  Finally
    FLock.Release;
  End;
end;

{ TAbstractMemMemoryLeaksNode }

procedure TAbstractMemMemoryLeaksNode.Clear;
begin
  Self.myPosition := 0;
  Self.parentPosition := 0;
  Self.leftPosition := 0;
  Self.rigthPosition := 0;
  Self.balance := 0;
  Self.units := 0;
end;

function TAbstractMemMemoryLeaksNode.GetPosition(APosition: TAVLTreePosition): TAbstractMemPosition;
begin
  case APosition of
    poParent: Result := Self.parentPosition;
    poLeft: Result := Self.leftPosition;
    poRight: Result := Self.rigthPosition;
  else raise EAbstractMem.Create('Undefined 20200310-3');
  end;
end;

function TAbstractMemMemoryLeaksNode.GetSize: Integer;
begin
  Result := Self.units * 4;
end;

procedure TAbstractMemMemoryLeaksNode.ReadFromMem(AMyPosition: TAbstractMemPosition; AAbstractMem: TAbstractMem);
var LBuff : TBytes;
begin
  Self.Clear;
  Self.myPosition := AMyPosition;
  if Self.myPosition<=0 then Exit;
  SetLength(LBuff,16);
  AAbstractMem.Read(AMyPosition,LBuff[0],16);
  Move(LBuff[0],Self.parentPosition,4);
  Move(LBuff[4],Self.leftPosition,4);
  Move(LBuff[8],Self.rigthPosition,4);
  Move(LBuff[12],Self.balance,1);
  Move(LBuff[13],Self.units,3);
end;

procedure TAbstractMemMemoryLeaksNode.SetPosition(APosition: TAVLTreePosition; AMemPosition: TAbstractMemPosition);
begin
  case APosition of
    poParent: Self.parentPosition := AMemPosition;
    poLeft: Self.leftPosition := AMemPosition ;
    poRight: Self.rigthPosition := AMemPosition;
  else raise EAbstractMem.Create('Undefined 20200310-3');
  end;
end;

procedure TAbstractMemMemoryLeaksNode.SetSize(ABytesSize: Integer);
begin
  Self.units := (((ABytesSize-1) DIV 4)+1);
end;

function TAbstractMemMemoryLeaksNode.ToString: String;
begin
  Result := Format('%d Bytes at %d with p:%d l:%d r:%d b:%d u:%d',
    [Self.GetSize,
     Self.myPosition,Self.parentPosition,Self.leftPosition,Self.rigthPosition,
     Self.balance,Self.units]);
end;

procedure TAbstractMemMemoryLeaksNode.WriteToMem(AAbstractMem: TAbstractMem);
var LBuff : TBytes;
begin
  if Self.myPosition<=0 then Exit;
  SetLength(LBuff,16);
  Move(Self.parentPosition,LBuff[0],4);
  Move(Self.leftPosition,LBuff[4],4);
  Move(Self.rigthPosition,LBuff[8],4);
  Move(Self.balance,LBuff[12],1);
  Move(Self.units,LBuff[13],3);
  AAbstractMem.Write(Self.myPosition,LBuff[0],16);
end;

{ TAbstractMemMemoryLeaks }

function _TAbstractMemMemoryLeaksNode_CompareByUnits(const Left, Right: TAbstractMemMemoryLeaksNode): Integer;
begin
  Result := Left.units - Right.units;
  if (Result=0) and (Left.myPosition>0) and (Right.myPosition>0) then begin
    // This will allow to find exactly a node when both are real (otherwise is searching for a position)
    Result := Left.myPosition - Right.myPosition;
  end;
end;

function TAbstractMemMemoryLeaks.AreEquals(const ANode1, ANode2: TAbstractMemMemoryLeaksNode): Boolean;
begin
  Result := (ANode1.myPosition = ANode2.myPosition);
end;

procedure TAbstractMemMemoryLeaks.ClearNode(var ANode: TAbstractMemMemoryLeaksNode);
begin
  ANode.Clear;
end;

procedure TAbstractMemMemoryLeaks.ClearPosition(var ANode: TAbstractMemMemoryLeaksNode; APosition: TAVLTreePosition);
begin
  ANode.SetPosition(APosition,0);
  if ANode.myPosition>0 then begin
    ANode.WriteToMem(FAbstractMem);
  end;
end;

constructor TAbstractMemMemoryLeaks.Create(AAbstractMem: TAbstractMem; ARootPosition: TAbstractMemPosition);
begin
  FRootPosition := ARootPosition;
  FAbstractMem := AAbstractMem;
  inherited Create(_TAbstractMemMemoryLeaksNode_CompareByUnits,False);
end;

destructor TAbstractMemMemoryLeaks.Destroy;
var LTmp : TAbstractMemMemoryLeaksNode;
begin
  LTmp := Root;
  DisposeNode(LTmp);
  inherited;
end;

procedure TAbstractMemMemoryLeaks.DisposeNode(var ANode: TAbstractMemMemoryLeaksNode);
begin
  //
  ANode.Clear;
end;

function TAbstractMemMemoryLeaks.GetBalance(const ANode: TAbstractMemMemoryLeaksNode): Integer;
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(ANode.myPosition,Self.FAbstractMem);
  Result := ANode.balance;
end;

function TAbstractMemMemoryLeaks.GetPosition(const ANode: TAbstractMemMemoryLeaksNode;
  APosition: TAVLTreePosition): TAbstractMemMemoryLeaksNode;
var LPos : TAbstractMemPosition;
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(ANode.myPosition,Self.FAbstractMem);
  LPos := ANode.GetPosition(APosition);
  if LPos>0 then begin
    Result.ReadFromMem(LPos,FAbstractMem);
  end else Result.Clear;
end;

function TAbstractMemMemoryLeaks.GetRoot: TAbstractMemMemoryLeaksNode;
begin
  if FRootPosition>0 then begin
    Result.ReadFromMem(FRootPosition,FAbstractMem);
  end else Result.Clear;
end;

function TAbstractMemMemoryLeaks.HasPosition(const ANode: TAbstractMemMemoryLeaksNode;
  APosition: TAVLTreePosition): Boolean;
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(ANode.myPosition,Self.FAbstractMem);
  Result := ANode.GetPosition(APosition) > 0;
end;

function TAbstractMemMemoryLeaks.IsNil(const ANode: TAbstractMemMemoryLeaksNode): Boolean;
begin
  Result := ANode.myPosition = 0;
end;

procedure TAbstractMemMemoryLeaks.SetBalance(var ANode: TAbstractMemMemoryLeaksNode; ANewBalance: Integer);
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(ANode.myPosition,Self.FAbstractMem);
  ANode.balance := ANewBalance;
  if ANode.myPosition>0 then begin
    ANode.WriteToMem(FAbstractMem);
  end;
end;

procedure TAbstractMemMemoryLeaks.SetPosition(var ANode: TAbstractMemMemoryLeaksNode;
  APosition: TAVLTreePosition; const ANewValue: TAbstractMemMemoryLeaksNode);
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(ANode.myPosition,Self.FAbstractMem);
  ANode.SetPosition(APosition,ANewValue.myPosition);
  if ANode.myPosition>0 then begin
    ANode.WriteToMem(FAbstractMem);
  end;
end;

procedure TAbstractMemMemoryLeaks.SetRoot(const Value: TAbstractMemMemoryLeaksNode);
begin
  FRootPosition := Value.myPosition;
  // Save to header info
  FAbstractMem.SaveHeader;
end;

function TAbstractMemMemoryLeaks.ToString(const ANode: TAbstractMemMemoryLeaksNode): String;
begin
  Result := ANode.ToString;
end;

{ TMem }

function TMem.AbsoluteRead(const AAbsolutePosition: Int64; var ABuffer; ASize: Integer): Integer;
begin
  if AAbsolutePosition>=Length(FMem) then Exit(0)
  else begin
    if AAbsolutePosition + ASize > Length(FMem) then Result := Length(FMem) - AAbsolutePosition
    else Result := ASize;
    Move(FMem[AAbsolutePosition],ABuffer,Result);
  end;
end;

function TMem.AbsoluteWrite(const AAbsolutePosition: Int64; const ABuffer; ASize: Integer): Integer;
begin
  if ASize=0 then Exit(0);
  if (AAbsolutePosition + ASize > Length(FMem)) or (ASize<0) then
    raise EAbstractMem.Create(Format('Write out of mem range from %d to %d (max %d)',
    [AAbsolutePosition,AAbsolutePosition+ASize,High(FMem)]));
  Move(ABuffer,FMem[AAbsolutePosition],ASize);
  Result := ASize;
end;

constructor TMem.Create(AInitialPosition: Integer; AReadOnly: Boolean);
begin
  SetLength(FMem,0);
  inherited;
end;

procedure TMem.DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos: Integer; ANeedSize: Integer);
begin
  if (ANeedSize<=0) And (AMaxAvailablePos<=0) then begin
    SetLength(FMem,0); // Reset
    Exit;
  end;
  AMaxAvailablePos := Length(FMem);
  if (AMaxAvailablePos-ANextAvailablePos+1 >= ANeedSize) then Exit;

  ANeedSize := (((ANeedSize-1) DIV 256)+1)*256;

  SetLength(FMem, AMaxAvailablePos + ANeedSize);
  Inc(AMaxAvailablePos,ANeedSize);
  //
end;

{ TAMZone }

procedure TAMZone.Clear;
begin
  Self.position := 0;
  Self.size := 0;
end;

function TAMZone.ToString: String;
begin
  Result := Format('Pos:%d Size:%d bytes',[Self.position,Self.size]);
end;

{ TAbstractMemAVLTreeNodeInfo }

procedure TAbstractMemAVLTreeNodeInfo.Clear;
begin
  Self.parentPosition := 0;
  Self.leftPosition := 0;
  Self.rigthPosition := 0;
  Self.balance := 0;
end;

function TAbstractMemAVLTreeNodeInfo.ToString: String;
begin
  Result := Format('TreeBasicNode: Parent:%d Left:%d Right:%d Balance:%d',[Self.parentPosition,Self.leftPosition,Self.rigthPosition,Self.balance]);
end;

{ TAbstractMemAVLTreeNodeInfoClass }

class procedure TAbstractMemAVLTreeNodeInfoClass.ClearPosition(
  AMyPosition: TAbstractMemPosition; AAbstractMem: TAbstractMem;
  APosition: TAVLTreePosition);
var L : TAbstractMemAVLTreeNodeInfo;
begin
  L := ReadFromMem(AMyPosition,AAbstractMem);
  case APosition of
    poParent: L.parentPosition := 0;
    poLeft:   L.leftPosition := 0;
    poRight:  L.rigthPosition := 0;
  end;
  WriteToMem(AMyPosition,AAbstractMem,L);
end;

class function TAbstractMemAVLTreeNodeInfoClass.GetBalance(
  AMyPosition: TAbstractMemPosition; AAbstractMem: TAbstractMem): ShortInt;
var L : TAbstractMemAVLTreeNodeInfo;
begin
  L := ReadFromMem(AMyPosition,AAbstractMem);
  Result := L.balance;
end;

class function TAbstractMemAVLTreeNodeInfoClass.GetPosition(
  AMyPosition: TAbstractMemPosition; AAbstractMem: TAbstractMem;
  APosition: TAVLTreePosition): TAbstractMemPosition;
var L : TAbstractMemAVLTreeNodeInfo;
begin
  L := ReadFromMem(AMyPosition,AAbstractMem);
  case APosition of
    poParent: Result := L.parentPosition;
    poLeft:   Result := L.leftPosition;
    poRight:  Result := L.rigthPosition;
  end;
end;

class function TAbstractMemAVLTreeNodeInfoClass.GetSize: Integer;
begin
  Result := 13; // 4*3 + 1 (balance)
end;

class function TAbstractMemAVLTreeNodeInfoClass.ReadFromMem(
  AMyPosition: TAbstractMemPosition;
  AAbstractMem: TAbstractMem): TAbstractMemAVLTreeNodeInfo;
var LBytes : TBytes;
begin
  if (AMyPosition>=CT_HeaderSize) then begin
    Result.Clear;
    SetLength(LBytes,Self.GetSize);
    if AAbstractMem.Read(AMyPosition,LBytes[0],Length(LBytes))<>Length(LBytes) then raise EAbstractMem.Create(Format('Not enough data to read TreeNodeInfo at %d',[AMyPosition]));
    Move(LBytes[0],Result.parentPosition,4);
    Move(LBytes[4],Result.leftPosition,4);
    Move(LBytes[8],Result.rigthPosition,4);
    Move(LBytes[12],Result.balance,1);
  end else raise EAbstractMem.Create(Format('Invalid position read TAbstractMemAVLTreeNodeInfo.ReadFromMem(%d)',[AMyPosition]));
end;

class procedure TAbstractMemAVLTreeNodeInfoClass.SetBalance(
  AMyPosition: TAbstractMemPosition; AAbstractMem: TAbstractMem;
  ANewBalance: ShortInt);
var L : TAbstractMemAVLTreeNodeInfo;
begin
  L := ReadFromMem(AMyPosition,AAbstractMem);
  L.balance := ANewBalance;
  WriteToMem(AMyPosition,AAbstractMem,L);
end;

class procedure TAbstractMemAVLTreeNodeInfoClass.SetPosition(
  AMyPosition: TAbstractMemPosition; AAbstractMem: TAbstractMem;
  APosition: TAVLTreePosition; ANewPosition: TAbstractMemPosition);
var L : TAbstractMemAVLTreeNodeInfo;
begin
  L := ReadFromMem(AMyPosition,AAbstractMem);
  case APosition of
    poParent: L.parentPosition := ANewPosition;
    poLeft:   L.leftPosition := ANewPosition;
    poRight:  L.rigthPosition := ANewPosition;
  end;
  WriteToMem(AMyPosition,AAbstractMem,L);
end;

class procedure TAbstractMemAVLTreeNodeInfoClass.WriteToMem(
  AMyPosition: TAbstractMemPosition; AAbstractMem: TAbstractMem;
  const ANodeInfo: TAbstractMemAVLTreeNodeInfo);
var LBytes : TBytes;
begin
  if (AMyPosition>=CT_HeaderSize) then begin
    SetLength(LBytes,Self.GetSize);
    Move(ANodeInfo.parentPosition,LBytes[0],4);
    Move(ANodeInfo.leftPosition,LBytes[4],4);
    Move(ANodeInfo.rigthPosition,LBytes[8],4);
    Move(ANodeInfo.balance,LBytes[12],1);
    AAbstractMem.Write(AMyPosition,LBytes[0],Length(LBytes));
  end else raise EAbstractMem.Create(Format('Invalid position write TAbstractMemAVLTreeNodeInfo.WriteToMem(%d) for %s',[AMyPosition,ANodeInfo.ToString]));
end;

end.
