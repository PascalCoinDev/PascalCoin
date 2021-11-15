unit UAbstractMem;

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
  UAbstractAVLTree
  {$IFNDEF FPC},System.Generics.Collections,System.Generics.Defaults{$ELSE},Generics.Collections,Generics.Defaults{$ENDIF},
  UOrderedList;

{$I ./ConfigAbstractMem.inc }

Type
  TAbstractMemPosition = Int64;
  TAbstractMemSize = Int64;

  TAMZone = record
    position : TAbstractMemPosition;
    size : TAbstractMemSize;
    procedure Clear;
    function ToString : String;
  end;

  EAbstractMem = Class(Exception);

  TAbstractMemZoneType = (amzt_unknown, amzt_memory_leak, amzt_used);

  TAbstractMemZoneInfo = record
    AMZone : TAMZone;
    ZoneType : TAbstractMemZoneType;
  end;

  { TAbstractMem }

  TAbstractMem = Class
  private
    type
      TAbstractMemMemoryLeaksNode = record
        myPosition,       // Position in the AbstractMem
        parentPosition,
        leftPosition,
        rigthPosition : TAbstractMemPosition;
        balance : ShortInt;
        units : Integer; // units equals to "4 bytes packet", 1=4 bytes 2=8 bytes ...
        function GetSize(AAbstractMem : TAbstractMem) : Int64;
        procedure SetSize(AAbstractMem : TAbstractMem; ABytesSize : Int64); // ABytesSize will be converted to units
        function GetPosition(APosition : TAVLTreePosition) : TAbstractMemPosition;
        procedure SetPosition(APosition : TAVLTreePosition; AMemPosition : TAbstractMemPosition);
        procedure ReadFromMem(AAbstractMem : TAbstractMem; AMyPosition : TAbstractMemPosition);
        procedure WriteToMem(AAbstractMem : TAbstractMem);
        procedure Clear;
        function ToString(AAbstractMem : TAbstractMem) : String;
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
        function SizeOfMemoryLeak : TAbstractMemSize;
      End;
  private
    FReadOnly : Boolean;
    FHeaderInitialized : Boolean;
    FInitialPosition : Int64;
    FNextAvailablePos : Int64;
    FMaxAvailablePos : Int64;
    FMemLeaks : TAbstractMemMemoryLeaks;
    FIs64Bits : Boolean;
    FMemUnitsSize : Integer; // Multiple of 4 and >=4 and <=256
    //
    function RoundSize(ASize : TAbstractMemSize) : TAbstractMemSize;
    function IsValidUsedSize(ASize : TAbstractMemSize) : Boolean;
  protected
    FLock : TCriticalSection;
    function AbsoluteWrite(const AAbsolutePosition : Int64; const ABuffer; ASize : Integer) : Integer; virtual; abstract;
    function AbsoluteRead(const AAbsolutePosition : Int64; var ABuffer; ASize : Integer) : Integer; virtual; abstract;
    procedure DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos : Int64; ANeedSize : Integer); virtual; abstract;
    //
    function PositionToAbsolute(const APosition : Int64) : Int64;
    procedure IncreaseSize(ANeedSize : TAbstractMemSize);
    //
    function GetZoneType(APosition : TAbstractMemPosition; out AAMZone : TAMZone) : TAbstractMemZoneType;
    procedure CheckInitialized(AWantsToWrite : Boolean);
    function IsAbstractMemInfoStable : Boolean; virtual;
    procedure SaveHeader;
  public
    function Write(const APosition : Int64; const ABuffer; ASize : Integer) : Integer; overload; virtual;
    function Read(const APosition : Int64; var ABuffer; ASize : Integer) : Integer; overload; virtual;

    Constructor Create(AInitialPosition : Int64; AReadOnly : Boolean); virtual;
    Destructor Destroy; override;
    //
    procedure ClearContent(ASetTo64Bytes : Boolean; AMemUnitsSize : Integer = 4);
    //
    function New(AMemSize : TAbstractMemSize) : TAMZone; virtual;
    procedure Dispose(const AAMZone : TAMZone); overload;
    procedure Dispose(const APosition : TAbstractMemPosition); overload;
    function GetUsedZoneInfo(const APosition : TAbstractMemPosition; ACheckForUsedZone : Boolean; out AAMZone : TAMZone) : Boolean;
    function ToString : String; override;
    function CheckConsistency(const AStructure : TStrings; const AAbstractMemZoneInfoList : TList<TAbstractMemZoneInfo>; out ATotalUsedSize, ATotalUsedBlocksCount, ATotalLeaksSize, ATotalLeaksBlocksCount : Int64) : Boolean; overload;
    procedure CheckConsistency; overload;
    function ReadFirstData(var AFirstDataZone : TAMZone; var AFirstData : TBytes) : Boolean;
    class function GetAbstractMemVersion : String;
    property ReadOnly : Boolean read FReadOnly;
    procedure SaveToStream(AStream : TStream);
    procedure CopyFrom(ASource : TAbstractMem);
    function GetStatsReport(AClearStats : Boolean) : String; virtual;
    property NextAvailablePos : Int64 read FNextAvailablePos;
    property MaxAvailablePos : Int64 read FMaxAvailablePos;
    property HeaderInitialized : Boolean read FHeaderInitialized;
    property Is64Bits : Boolean read FIs64Bits;
    property MemUnitsSize : Integer read FMemUnitsSize;
    function Initialize(ASetTo64Bytes : Boolean; AMemUnitsSize : Integer) : Boolean;
    function HeaderSize : Integer;
    function SizeOfAbstractMemPosition : TAbstractMemSize; inline;
  End;

  { TMem }

  TMem = Class(TAbstractMem)
  private
    FMem : TBytes;
    FLastIncrease : Int64;
  protected
    function AbsoluteWrite(const AAbsolutePosition : Int64; const ABuffer; ASize : Integer) : Integer; override;
    function AbsoluteRead(const AAbsolutePosition : Int64; var ABuffer; ASize : Integer) : Integer; override;
    procedure DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos : Int64; ANeedSize : Integer); override;
  public
    Constructor Create(AInitialPosition : Int64; AReadOnly : Boolean); override;
  End;

  TStreamMem = Class(TAbstractMem)
  private
    FStream : TStream;
  protected
    function AbsoluteWrite(const AAbsolutePosition : Int64; const ABuffer; ASize : Integer) : Integer; override;
    function AbsoluteRead(const AAbsolutePosition : Int64; var ABuffer; ASize : Integer) : Integer; override;
    procedure DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos : Int64; ANeedSize : Integer); override;
  public
    Constructor Create(const AStream : TStream; AInitialPosition : Int64; AReadOnly : Boolean); reintroduce;
    Destructor Destroy; override;
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
    class function GetSize(AAbstractMem : TAbstractMem) : Integer;
  end;

function TComparison_TAbstractMemPosition(const ALeft, ARight: TAbstractMemPosition): Integer;

implementation

const
  CT_Magic : Array[0..5] of byte = (7,6,5,4,3,2);

  CT_IsStable = 1;
  CT_Is_NOT_Stable = 0;

  CT_Version_32b = 1;
  CT_Version_64b = 2;

  CT_HeaderSize_32b = 16; // Magic(7) + Version(1) + MemLeak_root_position(4) + NextAvailable_position(4) = 16 bytes
  CT_HeaderSize_64b = 64; // Magic(7) + Version(1) + MemLeak_root_position(8) + NextAvailable_position(8) + UnitsSize(1) + NotUsed(39) = 64 bytes

function TComparison_TAbstractMemPosition(const ALeft, ARight: TAbstractMemPosition): Integer;
begin
  if ALeft<ARight then Result := -1
  else if ALeft>ARight then Result := 1
  else Result := 0;
end;

{ TAbstractMem }

function TAbstractMem.CheckConsistency(const AStructure : TStrings; const AAbstractMemZoneInfoList : TList<TAbstractMemZoneInfo>;
  out ATotalUsedSize, ATotalUsedBlocksCount, ATotalLeaksSize, ATotalLeaksBlocksCount : Int64) : Boolean;
var LPosition : TAbstractMemPosition;
  LZone : TAMZone;
  LAMZoneInfo : TAbstractMemZoneInfo;
  i: Integer;
  LMemLeakFound,LMemLeakToFind : TAbstractMemMemoryLeaksNode;
begin
  // Will check since first position:
  FLock.Acquire;
  Try
    ATotalUsedSize := 0;
    ATotalUsedBlocksCount := 0;
    ATotalLeaksSize := 0;
    ATotalLeaksBlocksCount := 0;
    LPosition := Self.HeaderSize;
    Result := True;
    while (Result) and (LPosition < FNextAvailablePos) do begin
      case GetZoneType(LPosition,LZone) of
        amzt_memory_leak : begin
          if Assigned(AStructure) then AStructure.Add( Format('%d to %d mem leak %d bytes',[LPosition,LZone.position + LZone.size,LZone.size]));
          if Assigned(AAbstractMemZoneInfoList) then begin
            LAMZoneInfo.AMZone := LZone;
            LAMZoneInfo.ZoneType := amzt_memory_leak;
            AAbstractMemZoneInfoList.Add(LAMZoneInfo);
          end;
          LPosition := LPosition + LZone.size;
          ATotalLeaksSize := ATotalLeaksSize + LZone.size;
          inc(ATotalLeaksBlocksCount);
        end;
        amzt_used : begin
          if Assigned(AStructure) then AStructure.Add( Format('%d to %d used %d bytes',[LPosition,LZone.position + LZone.size, LZone.size]));
          if Assigned(AAbstractMemZoneInfoList) then begin
            LAMZoneInfo.AMZone := LZone;
            LAMZoneInfo.ZoneType := amzt_used;
            AAbstractMemZoneInfoList.Add(LAMZoneInfo);
          end;
          LPosition := Int64(LPosition) + Int64(LZone.size) + Int64( SizeOfAbstractMemPosition() );
          ATotalUsedSize := ATotalUsedSize + LZone.size + Int64( SizeOfAbstractMemPosition() );
          inc(ATotalUsedBlocksCount);
        end;
      else
        if Assigned(AStructure) then AStructure.Add( Format('Consisteny error at %d (End position: %d)',[LPosition,FNextAvailablePos]));
        Result := False;
      end;
    end;
    //
    if Assigned(AAbstractMemZoneInfoList) then begin
      // Try to find all blocks:
      for i := 0 to AAbstractMemZoneInfoList.Count-1 do begin
        if (AAbstractMemZoneInfoList.Items[i].ZoneType=amzt_memory_leak) then begin
          // Search it:
          LMemLeakToFind.Clear;
          LMemLeakToFind.SetSize( Self, AAbstractMemZoneInfoList.Items[i].AMZone.size );
          LMemLeakToFind.myPosition := AAbstractMemZoneInfoList.Items[i].AMZone.position;

          LMemLeakFound := FMemLeaks.Find( LMemLeakToFind );
          if Not FMemLeaks.IsNil(LMemLeakFound) then begin
            if (LMemLeakFound.myPosition<>AAbstractMemZoneInfoList.Items[i].AMZone.position) then begin
              if Assigned(AStructure) then AStructure.Add( Format('MemLeak of %d bytes at %d pos not equal at %d/%d',
                [LMemLeakToFind.GetSize(Self),AAbstractMemZoneInfoList.Items[i].AMZone.position,i+1,AAbstractMemZoneInfoList.Count]));
              Result := False;
            end;
          end else begin
            if Assigned(AStructure) then AStructure.Add( Format('MemLeak of %d bytes at pos %d pos not found %d/%d',
              [LMemLeakToFind.GetSize(Self),AAbstractMemZoneInfoList.Items[i].AMZone.position,i+1,AAbstractMemZoneInfoList.Count]));
            Result := False;
          end;

        end;
      end;
    end;
  Finally
    FLock.Release;
  End;
end;

procedure TAbstractMem.CheckConsistency;
var LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount : Int64;
  LAbstractMemZoneInfoList : TList<TAbstractMemZoneInfo>;
begin
  LAbstractMemZoneInfoList := TList<TAbstractMemZoneInfo>.Create;
  Try
    if Not CheckConsistency(Nil,LAbstractMemZoneInfoList,LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount) then
      raise EAbstractMem.Create('TAbstractMem not consistent');
  Finally
    LAbstractMemZoneInfoList.Free;
  End;
end;

procedure TAbstractMem.CheckInitialized(AWantsToWrite : Boolean);
begin
  if (AWantsToWrite and FReadOnly) then raise EAbstractMem.Create('Cannot write to a ReadOnly AbstractMem');
  if Not FHeaderInitialized then begin
    // Needs to write
    if FReadOnly then raise EAbstractMem.Create('Cannot initialize a ReadOnly AbstractMem');
    //
    IncreaseSize(HeaderSize);
    // Write Header:
    SaveHeader;
  end;
end;

procedure TAbstractMem.ClearContent(ASetTo64Bytes : Boolean; AMemUnitsSize : Integer);
var LNewRoot : TAbstractMemMemoryLeaksNode;
begin
  // Will erase ALL content creating a new null header
  if FReadOnly then raise EAbstractMem.Create('Cannot ClearContent on a ReadOnly AbstractMem');
  CheckInitialized(True);

  FNextAvailablePos := HeaderSize;
  FMaxAvailablePos := 0;
  IncreaseSize(0);

  FHeaderInitialized := False;

  Initialize(ASetTo64Bytes, AMemUnitsSize);

  CheckInitialized(True);

  LNewRoot.Clear;
  FMemLeaks.SetRoot( LNewRoot );
end;

procedure TAbstractMem.CopyFrom(ASource: TAbstractMem);
var LBuff : TBytes;
  iPos, LBuffDataCount : Int64;
  LMemLeakRelativeRootPos : TAbstractMemPosition;
begin
  ASource.FLock.Acquire;
  Self.FLock.Acquire;
  try
    ClearContent(ASource.Is64Bits,ASource.FMemUnitsSize);

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
      iPos := iPos + LBuffDataCount;
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

constructor TAbstractMem.Create(AInitialPosition: Int64; AReadOnly : Boolean);
var LBuffer : TBytes;
  LMemLeakRelativeRootPos, LNextAvailablePosAux : Int64;
  LOk : Boolean;
  LMemUnitsSizeAux : Integer;
begin
  FMemLeaks := Nil;
  FReadOnly := AReadOnly;
  LMemLeakRelativeRootPos := 0;
  FInitialPosition := AInitialPosition;
  FIs64Bits := False;
  FMemUnitsSize := 4; // Warning: Multiple of 4 >=4 and <=256!
  //
  LNextAvailablePosAux := 0;
  FNextAvailablePos := CT_HeaderSize_32b; // By Default

  FMaxAvailablePos := 0;

  FLock := TCriticalSection.Create;
  // Try to initialize
  // Magic: 7 bytes
  // version: 1 byte
  // START OF FIRST BLOCK 1 = Header info
  FHeaderInitialized := True;
  LOk := False;
  Try
    SetLength(LBuffer,CT_HeaderSize_32b);
    if Read(0,LBuffer[0],CT_HeaderSize_32b)=CT_HeaderSize_32b then begin
      if CompareMem(@LBuffer[0],@CT_Magic[0],6) then begin
        if (LBuffer[6] = CT_IsStable) then begin
          if (LBuffer[7] = CT_Version_32b) then begin
            Move(LBuffer[8],LMemLeakRelativeRootPos,4);
            Move(LBuffer[12],LNextAvailablePosAux,4);
            //
            if (LNextAvailablePosAux >= CT_HeaderSize_32b) and (LMemLeakRelativeRootPos<LNextAvailablePosAux) then begin
              Fis64Bits := False;
              FNextAvailablePos := LNextAvailablePosAux;
              LOk := True;
            end;
          end else if (LBuffer[7] = CT_Version_64b) then begin
            Fis64Bits := True;
            SetLength(LBuffer,CT_HeaderSize_64b);
            FNextAvailablePos := CT_HeaderSize_64b; // At least v2 size
            if Read(0,LBuffer[0],CT_HeaderSize_64b)=CT_HeaderSize_64b then begin
              Move(LBuffer[8],LMemLeakRelativeRootPos,8);
              Move(LBuffer[16],LNextAvailablePosAux,8);
              LMemUnitsSizeAux := 0;
              Move(LBuffer[24],LMemUnitsSizeAux,1);
              if (LMemUnitsSizeAux>=4) and (LMemUnitsSizeAux<256) and ((LMemUnitsSizeAux MOD 4)=0)  // Multiple of 4
                 and (LNextAvailablePosAux >= CT_HeaderSize_32b) and (LMemLeakRelativeRootPos<LNextAvailablePosAux) then begin
                FNextAvailablePos := LNextAvailablePosAux;
                FMemUnitsSize := LMemUnitsSizeAux;
                LOk := True;
              end;
            end else LOk := False;
          end else LOk := False;
        end;
      end;
    end;
  Finally
    FHeaderInitialized := LOk;
    if Not LOk then LMemLeakRelativeRootPos := 0;
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
  if APosition<=HeaderSize then raise EAbstractMem.Create('Dispose: Invalid position '+IntToStr(APosition));
  LZone.Clear;
  // @[APosition] - SizeOfAbstractMemPosition() = position to size
  LZone.position := APosition;
  if Read(APosition - Int64(SizeOfAbstractMemPosition()),LZone.size,SizeOfAbstractMemPosition()) <> SizeOfAbstractMemPosition() then raise EAbstractMem.Create('Dispose: Cannot read size');
  Dispose(LZone);
end;

procedure TAbstractMem.Dispose(const AAMZone: TAMZone);
var LNewMemLeak : TAbstractMemMemoryLeaksNode;
  LZoneSize : TAbstractMemSize;
begin
  CheckInitialized(True);

  LZoneSize := 0;

  LNewMemLeak.Clear;
  LNewMemLeak.myPosition := AAMZone.position - SizeOfAbstractMemPosition();
  LNewMemLeak.SetSize(Self, AAMZone.size+SizeOfAbstractMemPosition());

  if Read(LNewMemLeak.myPosition,LZoneSize,SizeOfAbstractMemPosition())<>SizeOfAbstractMemPosition() then raise EAbstractMem.Create('Dispose: Cannot read size');
  if (LZoneSize<>AAMZone.size) then raise EAbstractMem.Create(Format('Dispose: Invalid size %d (expected %d) at position %d',[LZoneSize,AAMZone.size,AAMZone.position]));

  // Check valid units based on size
  if (LNewMemLeak.GetSize(Self)<>AAMZone.size+SizeOfAbstractMemPosition()) then raise EAbstractMem.Create(Format('Dispose: Invalid size %d (Found %d) at position %d',[AAMZone.size,LNewMemLeak.GetSize(Self),AAMZone.position]));
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
    if GetZoneType(APosition - SizeOfAbstractMemPosition(),AAMZone)<>amzt_used then Exit(False)
    else Exit(True);
  end else begin
    AAMZone.Clear;
    AAMZone.position := APosition;
    if Read(APosition - Int64(SizeOfAbstractMemPosition()),AAMZone.size,SizeOfAbstractMemPosition())<>SizeOfAbstractMemPosition() then Exit(False);
    Result := (AAMZone.position + AAMZone.size <= FNextAvailablePos)  And (IsValidUsedSize(AAMZone.size));
  end;
end;

function TAbstractMem.GetZoneType(APosition: TAbstractMemPosition; out AAMZone : TAMZone): TAbstractMemZoneType;
var LZone : TAMZone;
  LMemLeak, LSearchedMemLeak : TAbstractMemMemoryLeaksNode;
begin
  Result := amzt_unknown;
  AAMZone.Clear;
  AAMZone.position := APosition;
  AAMZone.size := 0;
  LZone.position := RoundSize(APosition);
  LZone.size := 0;
  if (LZone.position <> APosition) or (LZone.position<HeaderSize)
    or (LZone.position>=FNextAvailablePos) then Exit;
  // Check if Memory leak
  LMemLeak.myPosition := LZone.position;
  LMemLeak.ReadFromMem(Self,LMemLeak.myPosition);
  LSearchedMemLeak := FMemLeaks.Find(LMemLeak);
  if FMemLeaks.IsNil(LSearchedMemLeak) then begin
    if Read(APosition,LZone.size,SizeOfAbstractMemPosition())<>SizeOfAbstractMemPosition() then Exit;
    if (LZone.position + SizeOfAbstractMemPosition() + LZone.size <= FNextAvailablePos)
      And (IsValidUsedSize(LZone.size)) then begin
      Result := amzt_used;
      AAMZone.position := LZone.position + SizeOfAbstractMemPosition();
      AAMZone.size := LZone.size;
    end;
  end else begin
    AAMZone.size := LSearchedMemLeak.GetSize(Self);
    Result := amzt_memory_leak;
  end;
end;

function TAbstractMem.HeaderSize: Integer;
begin
  if Fis64Bits then Result := CT_HeaderSize_64b
  else Result := CT_HeaderSize_32b;
  Result := RoundSize(Result);
end;

procedure TAbstractMem.IncreaseSize(ANeedSize: TAbstractMemSize);
  // This will guarantee at the end that FMaxAvailablePos-FNextAvailablePos+1 >= ANeededSize
var LTmpNextAvailablePos, LTmpMaxAvailablePos : Int64;
begin
  if FMaxAvailablePos-FNextAvailablePos+1 >= ANeedSize then Exit;

  if Not Fis64Bits then begin
    // Max 32 bits memory (4 Gb)
    if Int64(FNextAvailablePos + Int64(ANeedSize)) >= Int64($FFFFFFFF) then begin
      raise EAbstractMem.Create(Format('Cannot increase more size (Max 4Gb) current %d (max %d) needed %d overflow 0x%s',
        [FNextAvailablePos,FMaxAvailablePos, ANeedSize,IntToHex(Int64(FNextAvailablePos + Int64(ANeedSize)),16)]));
    end;
  end;

  LTmpNextAvailablePos := FNextAvailablePos;
  LTmpMaxAvailablePos := FMaxAvailablePos;

  DoIncreaseSize(LTmpNextAvailablePos,LTmpMaxAvailablePos,ANeedSize);
  // Check
  if ((LTmpMaxAvailablePos-LTmpNextAvailablePos)+1 < ANeedSize) and (ANeedSize>0) then raise EAbstractMem.Create(FormaT('IncreaseSize error. Needed %d obtained from %d to %d = %d',
    [ANeedSize,LTmpNextAvailablePos,LTmpMaxAvailablePos,(LTmpMaxAvailablePos-LTmpNextAvailablePos+1)]));
  //
  FNextAvailablePos := LTmpNextAvailablePos;
  FMaxAvailablePos := LTmpMaxAvailablePos;
  if ANeedSize>0 then SaveHeader;
end;

function TAbstractMem.Initialize(ASetTo64Bytes: Boolean; AMemUnitsSize: Integer): Boolean;
begin
  Result := False;
  if ReadOnly then raise EAbstractMem.Create('Cannot initialize a Readonly AbstractMem');
  if HeaderInitialized then Exit;
  Fis64Bits := ASetTo64Bytes;
  FMemUnitsSize := 4; // By Default
  if Fis64Bits then begin
    if (AMemUnitsSize>=4) and (AMemUnitsSize<256) and ((AMemUnitsSize MOD 4)=0) then begin
      FMemUnitsSize := AMemUnitsSize;
    end;
    Result := True;
  end else begin
    Result := True;
  end;
  FNextAvailablePos := HeaderSize;
end;

function TAbstractMem.IsAbstractMemInfoStable: Boolean;
begin
  Result := True;
end;

function TAbstractMem.IsValidUsedSize(ASize: TAbstractMemSize): Boolean;
begin
  Result := RoundSize(ASize + SizeOfAbstractMemPosition) = (ASize + SizeOfAbstractMemPosition);
end;

function TAbstractMem.New(AMemSize: TAbstractMemSize): TAMZone;
var LNeededMemSize : TAbstractMemSize;
  LMemLeakToFind, LMemLeakFound : TAbstractMemMemoryLeaksNode;
  LMaxMemSizePerUnits : TAbstractMemSize;
begin
  CheckInitialized(True);

  // In 32 bits:
  // AMemSize must be a value stored in 3 bytes (24 bits) where each value is a "unit" of 4 bytes (FMemUnitsSize = 4)
  // In 64 bits:
  // AMemSize must be a value stored in 4 bytes (32 bits) where each value is a "unit" of FMemUnitsSize bytes (FMemUnitsSize is multiple of 4 between 4..256)
  //
  LMaxMemSizePerUnits := Int64(256 * 256 * 256) * Int64(FMemUnitsSize); // 2^24 * FMemUnitsSize
  if Fis64Bits then begin
    LMaxMemSizePerUnits := LMaxMemSizePerUnits * 256; // On 64 bits is stored in 32 bits instead of 24 bits
  end;
  if (AMemSize<=0) or (AMemSize>(LMaxMemSizePerUnits - SizeOfAbstractMemPosition())) then raise EAbstractMem.Create('Invalid new size: '+AMemSize.ToString+' Max:'+LMaxMemSizePerUnits.ToString);

  FLock.Acquire;
  Try
    // First bytes will be "how many units"
    LNeededMemSize := AMemSize + SizeOfAbstractMemPosition();
    // Minimum size is always 16 bytes (Mem needed for a mem leak = 4 * 4 bytes)
    if LNeededMemSize<FMemLeaks.SizeOfMemoryLeak() then LNeededMemSize := FMemLeaks.SizeOfMemoryLeak()
    else LNeededMemSize := LNeededMemSize;
    // Round LMemSize to a FMemUnitsSize bytes packet
    LNeededMemSize := RoundSize(LNeededMemSize);

    LMemLeakToFind.Clear;
    LMemLeakToFind.SetSize(Self,LNeededMemSize);

    LMemLeakFound := FMemLeaks.Find( LMemLeakToFind );
    if Not FMemLeaks.IsNil(LMemLeakFound) then begin
      // Found a Memory leak with this size, REUSE
      Result.position := LMemLeakFound.myPosition + SizeOfAbstractMemPosition();
      Result.size := LMemLeakFound.GetSize(Self) - SizeOfAbstractMemPosition();
      // Remove leak
      FMemLeaks.Delete( LMemLeakFound );
    end else begin
      // Need a new available zone
      IncreaseSize( LNeededMemSize );
      //
      Result.position := FNextAvailablePos + SizeOfAbstractMemPosition(); // 4 = "units"
      FNextAvailablePos := FNextAvailablePos + LNeededMemSize;
      Result.size := LNeededMemSize - SizeOfAbstractMemPosition();
      SaveHeader; // NextAvailablePos updated, save changes
    end;
    // Save size at first position
    Write(Result.position - Int64(SizeOfAbstractMemPosition()),Result.size,SizeOfAbstractMemPosition());
  Finally
    FLock.Release;
  End;
end;

function TAbstractMem.PositionToAbsolute(const APosition: Int64): Int64;
begin
  Result := FInitialPosition + APosition;
end;

procedure TAbstractMem.SaveHeader;
var LBuffer : TBytes;
  LUInt32 : UInt32;
  LUInt64 : UInt64;
  LByte : Byte;
begin
  if FReadOnly then raise EAbstractMem.Create('Cannot save Header on a ReadOnly AbstractMem');
  // Write Header:
  SetLength(LBuffer,HeaderSize);
  if Fis64Bits then begin
    FillChar(LBuffer[0],Length(LBuffer),0);
    Move(CT_Magic[0],LBuffer[0],6);
    if IsAbstractMemInfoStable then begin
      LBuffer[6] := CT_IsStable;
    end else begin
      LBuffer[6] := CT_Is_NOT_Stable;
    end;
    LBuffer[7] := CT_Version_64b;
    LUInt64 := FMemLeaks.Root.myPosition;
    Move(LUInt64,LBuffer[8],8);  // position to memleak btree root
    LUInt64 := FNextAvailablePos;
    Move(LUInt64,LBuffer[16],8); // next available pos
    LByte := FMemUnitsSize;
    Move(LByte,LBuffer[24],1);
  end else begin
    Move(CT_Magic[0],LBuffer[0],6);
    if IsAbstractMemInfoStable then begin
      LBuffer[6] := CT_IsStable;
    end else begin
      LBuffer[6] := CT_Is_NOT_Stable;
    end;
    LBuffer[7] := CT_Version_32b;
    LUInt32 := FMemLeaks.Root.myPosition;
    Move(LUInt32,LBuffer[8],4);  // position to memleak btree root
    LUInt32 := FNextAvailablePos;
    Move(LUInt32,LBuffer[12],4); // next available pos
  end;
  //
  FHeaderInitialized := True;  // Set before call to Write
  //
  Write(0,LBuffer[0],Length(LBuffer));
end;

procedure TAbstractMem.SaveToStream(AStream: TStream);
var LBuffer : TBytes;
  i : Int64;
  LNextStart : Int64;
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
      LNextStart := LNextStart + i;
    end;
  Finally
    FLock.Release;
  End;
end;

function TAbstractMem.SizeOfAbstractMemPosition: TAbstractMemSize;
begin
  if Fis64Bits then Result := 8
  else Result := 4;
end;

function TAbstractMem.ToString: String;
var LAnalize : TStrings;
  LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount : Int64;
begin
  LAnalize := TStringList.Create;
  try
    if Not CheckConsistency(LAnalize, Nil, LTotalUsedSize, LTotalUsedBlocksCount, LTotalLeaksSize, LTotalLeaksBlocksCount) then begin
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

function TAbstractMem.Read(const APosition: Int64; var ABuffer; ASize: Integer): Integer;
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
  Result := False;
  AFirstDataZone.Clear;
  SetLength(AFirstData,0);
  if Not HeaderInitialized then Exit;
  LPosition := HeaderSize;
  if (LPosition < FNextAvailablePos) then begin
    case GetZoneType(LPosition,AFirstDataZone) of
      amzt_used : begin
        SetLength(AFirstData,AFirstDataZone.size);
        Result := Read(AFirstDataZone.position,AFirstData[0],Length(AFirstData))=AFirstDataZone.size;
      end;
    end;
  end;
end;

function TAbstractMem.RoundSize(ASize: TAbstractMemSize): TAbstractMemSize;
//  Rounds ASize to a FMemUnitsSize valid value
begin
  Assert(ASize>=0,Format('Invalid size:%d',[ASize]));
  Result := ((((ASize-1) DIV Int64(FMemUnitsSize))+1)*FMemUnitsSize);
end;

function TAbstractMem.Write(const APosition: Int64; const ABuffer; ASize: Integer) : Integer;
begin
  FLock.Acquire;
  Try
    CheckInitialized(True);
    if AbsoluteWrite(PositionToAbsolute(APosition),ABuffer,ASize)<>ASize then raise EAbstractMem.Create('Cannot write expected size');
    Result := ASize;
  Finally
    FLock.Release;
  End;
end;

{ TAbstractMem.TAbstractMemMemoryLeaksNode }

procedure TAbstractMem.TAbstractMemMemoryLeaksNode.Clear;
begin
  Self.myPosition := 0;
  Self.parentPosition := 0;
  Self.leftPosition := 0;
  Self.rigthPosition := 0;
  Self.balance := 0;
  Self.units := 0;
end;

function TAbstractMem.TAbstractMemMemoryLeaksNode.GetPosition(APosition: TAVLTreePosition): TAbstractMemPosition;
begin
  case APosition of
    poParent: Result := Self.parentPosition;
    poLeft: Result := Self.leftPosition;
    poRight: Result := Self.rigthPosition;
  else raise EAbstractMem.Create('Undefined 20200310-3');
  end;
end;

function TAbstractMem.TAbstractMemMemoryLeaksNode.GetSize(AAbstractMem : TAbstractMem): Int64;
begin
  Result := Int64(Self.units) * Int64(AAbstractMem.FMemUnitsSize);
end;

procedure TAbstractMem.TAbstractMemMemoryLeaksNode.ReadFromMem(AAbstractMem : TAbstractMem; AMyPosition: TAbstractMemPosition);
var LBuff : TBytes;
begin
  Self.Clear;
  Self.myPosition := AMyPosition;
  if Self.myPosition<=0 then Exit;
  if AAbstractMem.Is64Bits then begin
    SetLength(LBuff,32);
    AAbstractMem.Read(AMyPosition,LBuff[0],32);
    Move(LBuff[0],Self.parentPosition,8);
    Move(LBuff[8],Self.leftPosition,8);
    Move(LBuff[16],Self.rigthPosition,8);
    Move(LBuff[24],Self.units,4);
    Move(LBuff[28],Self.balance,1);
  end else begin
    SetLength(LBuff,16);
    AAbstractMem.Read(AMyPosition,LBuff[0],16);
    Move(LBuff[0],Self.parentPosition,4);
    Move(LBuff[4],Self.leftPosition,4);
    Move(LBuff[8],Self.rigthPosition,4);
    Move(LBuff[12],Self.balance,1);
    Move(LBuff[13],Self.units,3);
  end;
end;

procedure TAbstractMem.TAbstractMemMemoryLeaksNode.SetPosition(APosition: TAVLTreePosition; AMemPosition: TAbstractMemPosition);
begin
  case APosition of
    poParent: Self.parentPosition := AMemPosition;
    poLeft: Self.leftPosition := AMemPosition ;
    poRight: Self.rigthPosition := AMemPosition;
  else raise EAbstractMem.Create('Undefined 20200310-3');
  end;
end;

procedure TAbstractMem.TAbstractMemMemoryLeaksNode.SetSize(AAbstractMem : TAbstractMem; ABytesSize: Int64);
begin
  Self.units := (((ABytesSize-1) DIV AAbstractMem.FMemUnitsSize)+1);
end;

function TAbstractMem.TAbstractMemMemoryLeaksNode.ToString(AAbstractMem : TAbstractMem): String;
begin
  Result := Format('%d Bytes at %d with p:%d l:%d r:%d b:%d u:%d',
    [Self.GetSize(AAbstractMem),
     Self.myPosition,Self.parentPosition,Self.leftPosition,Self.rigthPosition,
     Self.balance,Self.units]);
end;

procedure TAbstractMem.TAbstractMemMemoryLeaksNode.WriteToMem(AAbstractMem: TAbstractMem);
var LBuff : TBytes;
begin
  if Self.myPosition<=0 then Exit;
  if (AAbstractMem.is64Bits) then begin
    SetLength(LBuff,32);
    Move(Self.parentPosition,LBuff[0],8);
    Move(Self.leftPosition,LBuff[8],8);
    Move(Self.rigthPosition,LBuff[16],8);
    Move(Self.units,LBuff[24],4);
    Move(Self.balance,LBuff[28],1);
  end else begin
    SetLength(LBuff,16);
    Move(Self.parentPosition,LBuff[0],4);
    Move(Self.leftPosition,LBuff[4],4);
    Move(Self.rigthPosition,LBuff[8],4);
    Move(Self.balance,LBuff[12],1);
    Move(Self.units,LBuff[13],3);
  end;
  AAbstractMem.Write(Self.myPosition,LBuff[0],Length(LBuff));
end;

{ TAbstractMem.TAbstractMemMemoryLeaks }

function _TAbstractMemMemoryLeaksNode_CompareByUnits(const Left, Right: TAbstractMem.TAbstractMemMemoryLeaksNode): Integer;
begin
  Result := Left.units - Right.units;
  if (Result=0) and (Left.myPosition>0) and (Right.myPosition>0) then begin
    // This will allow to find exactly a node when both are real (otherwise is searching for a position)
    Result := Left.myPosition - Right.myPosition;
  end;
end;

function TAbstractMem.TAbstractMemMemoryLeaks.AreEquals(const ANode1, ANode2: TAbstractMemMemoryLeaksNode): Boolean;
begin
  Result := (ANode1.myPosition = ANode2.myPosition);
end;

procedure TAbstractMem.TAbstractMemMemoryLeaks.ClearNode(var ANode: TAbstractMemMemoryLeaksNode);
begin
  ANode.Clear;
end;

procedure TAbstractMem.TAbstractMemMemoryLeaks.ClearPosition(var ANode: TAbstractMemMemoryLeaksNode; APosition: TAVLTreePosition);
begin
  ANode.SetPosition(APosition,0);
  if ANode.myPosition>0 then begin
    ANode.WriteToMem(FAbstractMem);
  end;
end;

constructor TAbstractMem.TAbstractMemMemoryLeaks.Create(AAbstractMem: TAbstractMem; ARootPosition: TAbstractMemPosition);
begin
  FRootPosition := ARootPosition;
  FAbstractMem := AAbstractMem;
  inherited Create(_TAbstractMemMemoryLeaksNode_CompareByUnits,False);
end;

destructor TAbstractMem.TAbstractMemMemoryLeaks.Destroy;
var LTmp : TAbstractMemMemoryLeaksNode;
begin
  LTmp := Root;
  DisposeNode(LTmp);
  inherited;
end;

procedure TAbstractMem.TAbstractMemMemoryLeaks.DisposeNode(var ANode: TAbstractMemMemoryLeaksNode);
begin
  //
  ANode.Clear;
end;

function TAbstractMem.TAbstractMemMemoryLeaks.GetBalance(const ANode: TAbstractMemMemoryLeaksNode): Integer;
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(Self.FAbstractMem,ANode.myPosition);
  Result := ANode.balance;
end;

function TAbstractMem.TAbstractMemMemoryLeaks.GetPosition(const ANode: TAbstractMemMemoryLeaksNode;
  APosition: TAVLTreePosition): TAbstractMemMemoryLeaksNode;
var LPos : TAbstractMemPosition;
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(Self.FAbstractMem,ANode.myPosition);
  LPos := ANode.GetPosition(APosition);
  if LPos>0 then begin
    Result.ReadFromMem(Self.FAbstractMem,LPos);
  end else Result.Clear;
end;

function TAbstractMem.TAbstractMemMemoryLeaks.GetRoot: TAbstractMemMemoryLeaksNode;
begin
  if FRootPosition>0 then begin
    Result.ReadFromMem(Self.FAbstractMem,FRootPosition);
  end else Result.Clear;
end;

function TAbstractMem.TAbstractMemMemoryLeaks.HasPosition(const ANode: TAbstractMemMemoryLeaksNode;
  APosition: TAVLTreePosition): Boolean;
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(Self.FAbstractMem,ANode.myPosition);
  Result := ANode.GetPosition(APosition) > 0;
end;

function TAbstractMem.TAbstractMemMemoryLeaks.IsNil(const ANode: TAbstractMemMemoryLeaksNode): Boolean;
begin
  Result := ANode.myPosition = 0;
end;

procedure TAbstractMem.TAbstractMemMemoryLeaks.SetBalance(var ANode: TAbstractMemMemoryLeaksNode; ANewBalance: Integer);
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(Self.FAbstractMem,ANode.myPosition);
  ANode.balance := ANewBalance;
  if ANode.myPosition>0 then begin
    ANode.WriteToMem(FAbstractMem);
  end;
end;

procedure TAbstractMem.TAbstractMemMemoryLeaks.SetPosition(var ANode: TAbstractMemMemoryLeaksNode;
  APosition: TAVLTreePosition; const ANewValue: TAbstractMemMemoryLeaksNode);
begin
  if ANode.myPosition>0 then ANode.ReadFromMem(Self.FAbstractMem,ANode.myPosition);
  ANode.SetPosition(APosition,ANewValue.myPosition);
  if ANode.myPosition>0 then begin
    ANode.WriteToMem(FAbstractMem);
  end;
end;

procedure TAbstractMem.TAbstractMemMemoryLeaks.SetRoot(const Value: TAbstractMemMemoryLeaksNode);
begin
  FRootPosition := Value.myPosition;
  // Save to header info
  FAbstractMem.SaveHeader;
end;

function TAbstractMem.TAbstractMemMemoryLeaks.SizeOfMemoryLeak: TAbstractMemSize;
begin
  if FAbstractMem.is64Bits then Result := 32
  else Result := 16;
end;

function TAbstractMem.TAbstractMemMemoryLeaks.ToString(const ANode: TAbstractMemMemoryLeaksNode): String;
begin
  Result := ANode.ToString(Self.FAbstractMem);
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

constructor TMem.Create(AInitialPosition: Int64; AReadOnly: Boolean);
begin
  SetLength(FMem,0);
  FLastIncrease := 0;
  inherited;
end;

procedure TMem.DoIncreaseSize(var ANextAvailablePos, AMaxAvailablePos: Int64; ANeedSize: Integer);
begin
  if (ANeedSize<=0) And (AMaxAvailablePos<=0) then begin
    SetLength(FMem,0); // Reset
    Exit;
  end;
  AMaxAvailablePos := Length(FMem);
  if (AMaxAvailablePos-ANextAvailablePos+1 >= ANeedSize) then Exit;

  ANeedSize := RoundSize( ((((ANeedSize + FLastIncrease)-1) DIV 256)+1)*256 );
  FLastIncrease := ANeedSize;
  SetLength(FMem, AMaxAvailablePos + ANeedSize);
  AMaxAvailablePos := AMaxAvailablePos + ANeedSize;
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

class function TAbstractMemAVLTreeNodeInfoClass.GetSize(AAbstractMem : TAbstractMem): Integer;
begin
  Result := (AAbstractMem.SizeOfAbstractMemPosition*3)+1;
end;

class function TAbstractMemAVLTreeNodeInfoClass.ReadFromMem(
  AMyPosition: TAbstractMemPosition;
  AAbstractMem: TAbstractMem): TAbstractMemAVLTreeNodeInfo;
var LBytes : TBytes;
begin
  if (AMyPosition>=AAbstractMem.HeaderSize) then begin
    Result.Clear;
    SetLength(LBytes,Self.GetSize(AAbstractMem));
    if AAbstractMem.Read(AMyPosition,LBytes[0],Length(LBytes))<>Length(LBytes) then raise EAbstractMem.Create(Format('Not enough data to read TreeNodeInfo at %d',[AMyPosition]));
    Move(LBytes[0],Result.parentPosition,AAbstractMem.SizeOfAbstractMemPosition);
    Move(LBytes[AAbstractMem.SizeOfAbstractMemPosition  ],Result.leftPosition,AAbstractMem.SizeOfAbstractMemPosition);
    Move(LBytes[AAbstractMem.SizeOfAbstractMemPosition*2],Result.rigthPosition,AAbstractMem.SizeOfAbstractMemPosition);
    Move(LBytes[AAbstractMem.SizeOfAbstractMemPosition*3],Result.balance,1);
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
  if (AMyPosition>=AAbstractMem.HeaderSize) then begin
    SetLength(LBytes,Self.GetSize(AAbstractMem));
    Move(ANodeInfo.parentPosition,LBytes[0],AAbstractMem.SizeOfAbstractMemPosition);
    Move(ANodeInfo.leftPosition,  LBytes[AAbstractMem.SizeOfAbstractMemPosition*1],AAbstractMem.SizeOfAbstractMemPosition);
    Move(ANodeInfo.rigthPosition, LBytes[AAbstractMem.SizeOfAbstractMemPosition*2],AAbstractMem.SizeOfAbstractMemPosition);
    Move(ANodeInfo.balance,       LBytes[AAbstractMem.SizeOfAbstractMemPosition*3],1);
    AAbstractMem.Write(AMyPosition,LBytes[0],Length(LBytes));
  end else raise EAbstractMem.Create(Format('Invalid position write TAbstractMemAVLTreeNodeInfo.WriteToMem(%d) for %s',[AMyPosition,ANodeInfo.ToString]));
end;

{ TStreamMem }

function TStreamMem.AbsoluteRead(const AAbsolutePosition: Int64; var ABuffer;
  ASize: Integer): Integer;
begin
  FStream.Position := AAbsolutePosition;
  Result := FStream.Read(ABuffer,ASize);
end;

function TStreamMem.AbsoluteWrite(const AAbsolutePosition: Int64; const ABuffer;
  ASize: Integer): Integer;
begin
  FStream.Position := AAbsolutePosition;
  Result := FStream.Write(ABuffer,ASize);
end;

constructor TStreamMem.Create(const AStream : TStream; AInitialPosition : Int64; AReadOnly : Boolean);
begin
  FStream := AStream;
  inherited Create(AInitialPosition,AReadOnly);
end;

destructor TStreamMem.Destroy;
begin
  inherited;
  FStream := Nil;
end;

procedure TStreamMem.DoIncreaseSize(var ANextAvailablePos,
  AMaxAvailablePos: Int64; ANeedSize: Integer);
var LBuff : TBytes;
begin
  if (ANeedSize<=0) And (AMaxAvailablePos<=0) then begin
    FStream.Seek(0,soFromEnd);
    FStream.Size := 0;
    Exit;
  end;

  FStream.Seek(0,soFromEnd);
  // GoTo ANextAvailablePos
  if (FStream.Position<ANextAvailablePos) then begin
    SetLength(LBuff,ANextAvailablePos - FStream.Position);
    FillChar(LBuff[0],Length(LBuff),0);
    FStream.Write(LBuff[0],Length(LBuff));
  end;
  if (FStream.Position<ANextAvailablePos) then raise EAbstractMem.Create(Format('End stream position (%d) is less than next available pos %d',[FStream.Position,ANextAvailablePos]));
  // At this time ANextAvailablePos <= FFileStream.Position
  AMaxAvailablePos := ANextAvailablePos + ANeedSize;
  if (FStream.Size<AMaxAvailablePos) then begin
    SetLength(LBuff,AMaxAvailablePos - FStream.Position);
    FillChar(LBuff[0],Length(LBuff),0);
    FStream.Write(LBuff[0],Length(LBuff));
  end else AMaxAvailablePos := FStream.Size;
end;

end.

