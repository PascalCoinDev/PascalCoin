unit UAbstractMemBTree;

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

{$ifdef FPC}
  {$mode DELPHI}
{$endif}
{$H+}

interface

uses
  Classes, SysUtils,
  // NOTE ABOUT FREEPASCAL (2020-03-10)
  // Current version 3.0.4 does not contain valid support for Generics, using Generics from this:
  // https://github.com/PascalCoinDev/PascalCoin/tree/master/src/libraries/generics.collections
  // (Download and set folder as a "units include folder" in compiler options)
  {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults,{$ELSE}Generics.Collections,Generics.Defaults,{$ENDIF}
  UOrderedList, UAbstractMem, UAbstractBTree;

{$I ./ConfigAbstractMem.inc }

type
  EAbstractMemBTree = Class(Exception);

  TAbstractMemBTree = Class( TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition> )
    // BTree implementation on AbstractMem will use TIdentify and TData as a TAbstractMemPosition (aka pointer inside AbstractMem)
    // Internal search process will convert TData pointer to final TData value for
    // comparisions
  private
    const
          CT_AbstractMemBTree_Magic = 'AMBT'; // DO NOT LOCALIZE MUST BE 4 BYTES LENGTH
    var
    FInitialZone : TAMZone;
    FrootPosition : TAbstractMemPosition;
    procedure SaveHeader;
    Procedure CheckInitialized;
    procedure LoadNodeHeader(const APosition : TAbstractMemPosition; var ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; var AChildsCount : Integer; var AChildsPosition : TAbstractMemPosition);
    procedure SaveNodeHeader(const ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; const AChildsPosition : TAbstractMemPosition);
    function GetNodeHeaderSize : Integer;
  protected
    FAbstractMem : TAbstractMem;
    function GetRoot: TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; override;
    procedure SetRoot(var Value: TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode); override;
    function NewNode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; override;
    procedure DisposeNode(var ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode); override;
    procedure SetNil(var AIdentify : TAbstractMemPosition); override;
    procedure SaveNode(var ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode); override;
    procedure SetCount(const ANewCount : Integer); override;
    //
    // NOTE: inherited classes will need to override DisposeData if Data is not a new AbstractMem memory region that must be freed
    //
    procedure DisposeData(var AData : TAbstractMemPosition); override;
    //
    // NOTE: inherited classes will need to override DoCompareData function in order to properly compare:
    // function DoCompareData(const ALeftData, ARightData: TAbstractMemPosition): Integer; override;
    //
  public
    function IsNil(const AIdentify : TAbstractMemPosition) : Boolean; override;
    constructor Create(AAbstractMem : TAbstractMem; const AInitialZone: TAMZone; AAllowDuplicates : Boolean; AOrder : Integer); virtual;
    destructor Destroy; override;
    function GetNode(AIdentify : TAbstractMemPosition) : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; override;
    class function MinAbstractMemInitialPositionSize(AAbstractMem : TAbstractMem) : Integer;
    property AbstractMem : TAbstractMem read FAbstractMem;
    property Count;
    function NodeDataToString(const AData : TAbstractMemPosition) : String; override;
    function NodeIdentifyToString(const AIdentify : TAbstractMemPosition) : String; override;
  End;

  TAbstractMemBTreeData<TData> = Class(TAbstractMemBTree)
  private
    // FLeft_ and FRight_ will be used as a cache for improvement calls on DoCompareData
    FLeft_Pos, FRight_Pos : TAbstractMemPosition;
    FLeft_Data, FRight_Data : TData;
    FSearchTarget : TData;
    FOnCompareAbstractMemData: TComparison<TData>;
  protected
    function DoCompareData(const ALeftData, ARightData: TAbstractMemPosition): Integer; override;
    //
    function LoadData(const APosition : TAbstractMemPosition) : TData; virtual; abstract;
    function SaveData(const AData : TData) : TAMZone; virtual; abstract;
  public
    constructor Create(AAbstractMem : TAbstractMem; const AInitialZone: TAMZone; AAllowDuplicates : Boolean; AOrder : Integer; const AOnCompareAbstractMemDataMethod: TComparison<TData>);
    function AddData(const AData: TData) : Boolean;
    function FindData(const AData: TData; var APosition : TAbstractMemPosition) : Boolean;
    function DeleteData(const AData: TData) : Boolean;
    function FindDataPrecessor(const AData : TData; var APrecessor : TData) : Boolean;
    function FindDataSuccessor(const AData : TData; var ASuccessor : TData) : Boolean;
    function FindDataLowest(out ALowest : TData) : Boolean;
    function FindDataHighest(out AHighest : TData) : Boolean;
  End;

implementation

{ TAbstractMemBTree<TData> }

procedure TAbstractMemBTree.CheckInitialized;
begin
  if (FInitialZone.position=0) then raise EAbstractMemBTree.Create(Format('%s initial position not initialized',[ClassName]));
end;

constructor TAbstractMemBTree.Create(AAbstractMem : TAbstractMem; const AInitialZone: TAMZone; AAllowDuplicates: Boolean; AOrder: Integer);
var LBuff : TBytes;
 i : Integer;
 LOrder : Integer;
begin
  FAbstractMem := AAbstractMem;
  FrootPosition := 0;

  inherited Create(TComparison_TAbstractMemPosition,TComparison_TAbstractMemPosition,AAllowDuplicates,AOrder);
  FCount := 0;
  //
  if Not FAbstractMem.GetUsedZoneInfo(AInitialZone.position,False,FInitialZone) then begin
    if FAbstractMem.ReadOnly then begin
      // Is not initialized and is Read Only
      FInitialZone.Clear;
      Exit;
    end;
    raise EAbstractMemBTree.Create('Cannot capture zone info for initialize');
  end else begin
    if FInitialZone.position=0 then Exit;
  end;
  if (FInitialZone.size<MinAbstractMemInitialPositionSize(AAbstractMem)) then begin
    raise EAbstractMemBTree.Create(Format('Invalid size %d for initialize',[FInitialZone.size]));
  end;
  SetLength(LBuff,MinAbstractMemInitialPositionSize(AAbstractMem));
  FAbstractMem.Read(FInitialZone.position,LBuff[0],Length(LBuff));
  try
    // Check magic
    for i := 0 to CT_AbstractMemBTree_Magic.Length-1 do begin
      if LBuff[i]<>Ord(CT_AbstractMemBTree_Magic.Chars[i]) then Exit;
    end;
    Move(LBuff[4],FrootPosition,FAbstractMem.SizeOfAbstractMemPosition);
    Move(LBuff[4+FAbstractMem.SizeOfAbstractMemPosition],FCount,4);
    LOrder := 0;
    Move(LBuff[8+FAbstractMem.SizeOfAbstractMemPosition],LOrder,4);
    if LOrder<>Order then raise EAbstractMemBTree.Create(Format('Invalid Order %d expected %d',[LOrder,Order]));
    if (((FrootPosition=0) and (FCount>0))) then raise EAbstractMemBTree.Create(Format('Invalid initial root %d vs count %d',[FrootPosition,FCount]));
  finally
  end;
end;

destructor TAbstractMemBTree.Destroy;
begin
  //
  inherited;
end;

procedure TAbstractMemBTree.DisposeData(var AData: TAbstractMemPosition);
begin
  inherited;
  // Will be called on EraseTreeEx
  FAbstractMem.Dispose(AData);
end;

procedure TAbstractMemBTree.DisposeNode(var ANode: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode);
var LOld : TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
  LChildsCount : Integer;
  LChildsPosition : TAbstractMemPosition;
begin
  LoadNodeHeader(ANode.identify,LOld,LChildsCount,LChildsPosition);
  FAbstractMem.Dispose( ANode.identify );
  ClearNode(ANode);
  Assert(((LChildsCount=0) and (LChildsPosition=0))
     or ((LChildsCount<>0) and (LChildsPosition<>0)),Format('Invalid Childs count %d and position %d',[LChildsCount,LChildsPosition]));
  if LChildsCount>0 then begin
    FAbstractMem.Dispose( LChildsPosition );
  end;
end;

function TAbstractMemBTree.GetNode(AIdentify: TAbstractMemPosition): TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
var LBuff : TBytes;
  i, LChildsCount : Integer;
  LChildsPosition : TAbstractMemPosition;
begin
  LoadNodeHeader(AIdentify,Result,LChildsCount,LChildsPosition);
  if LChildsCount>0 then begin
    SetLength(Result.childs,LChildsCount);
    SetLength(LBuff,(LChildsCount*FAbstractMem.SizeOfAbstractMemPosition));
    FAbstractMem.Read(LChildsPosition,LBuff[0],Length(LBuff));
    for i := 0 to LChildsCount-1 do begin
      Move(LBuff[i*FAbstractMem.SizeOfAbstractMemPosition],Result.childs[i],FAbstractMem.SizeOfAbstractMemPosition);
    end;
  end;
  if ((Result.Count=0) and (Result.parent=0) and (LChildsCount=0)) then begin
    // root without data
  end else begin
    if (Result.parent=0) then begin
      if ((Result.Count<1) or (Result.Count>MaxItemsPerNode)) then
        raise EAbstractMemBTree.Create(Format('Root Node items %d not in range [%d..%d]',[Result.Count,MinItemsPerNode,MaxItemsPerNode]));
    end else begin
      if ((Result.Count<MinItemsPerNode) or (Result.Count>MaxItemsPerNode)) then
        raise EAbstractMemBTree.Create(Format('Node items %d not in range [%d..%d]',[Result.Count,MinItemsPerNode,MaxItemsPerNode]));
    end;
    if ((LChildsCount<>0) and (LChildsCount<>(Result.Count+1))) then
      raise EAbstractMemBTree.Create(Format('Node childrens %d not %d+1 in range [%d..%d]',[LChildsCount,Result.Count,MinChildrenPerNode,MaxChildrenPerNode]));
  end;
end;

function TAbstractMemBTree.GetNodeHeaderSize: Integer;
begin
  Result := ((FAbstractMem.SizeOfAbstractMemPosition*2)+4) + (FAbstractMem.SizeOfAbstractMemPosition*MaxItemsPerNode);
end;

function TAbstractMemBTree.GetRoot: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
begin
  if FrootPosition>0 then begin
    Result := GetNode(FrootPosition);
  end else ClearNode(Result);
end;

function TAbstractMemBTree.IsNil(const AIdentify: TAbstractMemPosition): Boolean;
begin
  Result := AIdentify=0;
end;

procedure TAbstractMemBTree.LoadNodeHeader(
  const APosition : TAbstractMemPosition; var ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; var AChildsCount : Integer; var AChildsPosition : TAbstractMemPosition);
var LBuff : TBytes;
  i, LItemsCount : Integer;
begin
  // Node is stored in zone 2 positions:
  //
  // In 32 bits
  // Zone 1: Header
  //   Size = (4+2+2+4) + (4*MaxItemsPerNode)
  // 4 Bytes [0..3] : Parent
  // 1 Byte  [4]    : Used items (0..255)
  // 1 Byte  [5]    : Used childs (0 (leaf) or Used Items+1)
  // 2 Bytes [6..7] : 0 (unusued)
  // 4 Bytes [8..11]: Zone 2 position ( If is a leaf must be 0 )
  // For each item:
  //   4 Bytes : data (AbstractMemPosition or Data using 4 bytes)
  //
  // Zone 2: OPTIONAL Only if NOT a leaf
  // For each children:
  //   4 Bytes : Children AbstractMem position
  //
  // In 64 bits
  // Same but using 8 bytes (instead of 4) for position
  //   Size = (8+2+2+8) + (8*MaxItemsPerNode)
  //
  // Use FAbstractMem.SizeOfAbstractMemPosition (will return 4 or 8)
  //   Size = ((FAbstractMem.SizeOfAbstractMemPosition*2)+4) + (FAbstractMem.SizeOfAbstractMemPosition*MaxItemsPerNode)
  //
  SetLength(LBuff,GetNodeHeaderSize);

  FAbstractMem.Read(APosition,LBuff[0],Length(LBuff));
  ClearNode(ANode);
  LItemsCount := 0;
  AChildsCount := 0;
  AChildsPosition := 0;
  ANode.identify := APosition;
  Move(LBuff[0],ANode.parent , FAbstractMem.SizeOfAbstractMemPosition);
  Move(LBuff[FAbstractMem.SizeOfAbstractMemPosition],LItemsCount,1);
  Move(LBuff[FAbstractMem.SizeOfAbstractMemPosition+1],AChildsCount,1);
  Move(LBuff[FAbstractMem.SizeOfAbstractMemPosition+4],AChildsPosition,FAbstractMem.SizeOfAbstractMemPosition);
  SetLength(ANode.data,LItemsCount);
  for i := 0 to LItemsCount-1 do begin
    Move(LBuff[(FAbstractMem.SizeOfAbstractMemPosition*2)+4 + (i*FAbstractMem.SizeOfAbstractMemPosition)],
      ANode.data[i], FAbstractMem.SizeOfAbstractMemPosition);
  end;
end;

class function TAbstractMemBTree.MinAbstractMemInitialPositionSize(AAbstractMem : TAbstractMem) : Integer;
begin
  Result := (AAbstractMem.SizeOfAbstractMemPosition) + 12 ;
end;

function TAbstractMemBTree.NewNode: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
begin
  CheckInitialized;
  ClearNode(Result);
  Result.identify := FAbstractMem.New( GetNodeHeaderSize ).position;
  SaveNodeHeader(Result,0);
end;

function TAbstractMemBTree.NodeDataToString(const AData: TAbstractMemPosition): String;
begin
  Result := '0x'+AData.ToHexString;
end;

function TAbstractMemBTree.NodeIdentifyToString(
  const AIdentify: TAbstractMemPosition): String;
begin
  Result := '0x'+AIdentify.ToHexString;
end;

procedure TAbstractMemBTree.SaveHeader;
var LBuff : TBytes;
 i : Integer;
 LOrder : Integer;
begin
  CheckInitialized;
  SetLength(LBuff,MinAbstractMemInitialPositionSize(FAbstractMem));
  for i := 0 to CT_AbstractMemBTree_Magic.Length-1 do begin
    LBuff[i] := Byte(Ord(CT_AbstractMemBTree_Magic.Chars[i]));
  end;
  Move(FrootPosition,LBuff[4],FAbstractMem.SizeOfAbstractMemPosition);
  Move(FCount,LBuff[4+FAbstractMem.SizeOfAbstractMemPosition],4);
  LOrder := Order;
  Move(LOrder,LBuff[8+FAbstractMem.SizeOfAbstractMemPosition],4);
  FAbstractMem.Write(FInitialZone.position,LBuff[0],Length(LBuff));
end;

procedure TAbstractMemBTree.SaveNode(var ANode: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode);
var LBuff : TBytes;
  i, LChildsCount : Integer;
  LOld : TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
  LChildsPosition : TAbstractMemPosition;
  LZone : TAMZone;
begin
  CheckInitialized;
  if ((ANode.Count)>MaxItemsPerNode) or (Length(ANode.childs)>MaxChildrenPerNode) then begin
    // Protection against saving temporal Node info with extra datas or childs
    Exit;
  end;
  LoadNodeHeader(ANode.identify,LOld,LChildsCount,LChildsPosition);
  //
  if (LChildsCount>0) And (ANode.IsLeaf) then begin
    // Node wasn't a leaf previously
    Assert(LChildsPosition<>0,'Old childs position<>0');
    FAbstractMem.Dispose(LChildsPosition);
    LChildsPosition := 0;
  end else if (LChildsCount=0) And (Not ANode.IsLeaf) then begin
    // Node was a leaf previously, now not
    LZone := FAbstractMem.New( MaxChildrenPerNode * FAbstractMem.SizeOfAbstractMemPosition );
    LChildsPosition := LZone.position;
  end;
  LChildsCount := Length(ANode.childs);
  //
  SaveNodeHeader(ANode,LChildsPosition);
  //
  if LChildsCount>0 then begin
    SetLength(LBuff, MaxChildrenPerNode * FAbstractMem.SizeOfAbstractMemPosition );
    FillChar(LBuff[0],Length(LBuff),0);
    for i := 0 to LChildsCount-1 do begin
      Move(ANode.childs[i],LBuff[i*FAbstractMem.SizeOfAbstractMemPosition],FAbstractMem.SizeOfAbstractMemPosition);
    end;
    FAbstractMem.Write(LChildsPosition,LBuff[0],LChildsCount*FAbstractMem.SizeOfAbstractMemPosition);
  end;
end;

procedure TAbstractMemBTree.SaveNodeHeader(
  const ANode: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode; const AChildsPosition : TAbstractMemPosition);
var LBuff : TBytes;
  i, LItemsCount, LChildsCount: Integer;
begin
  SetLength(LBuff, GetNodeHeaderSize );

  FillChar(LBuff[0],Length(LBuff),0);
  Move(ANode.parent,LBuff[0],FAbstractMem.SizeOfAbstractMemPosition);
  LItemsCount := ANode.Count;
  Move(LItemsCount,LBuff[FAbstractMem.SizeOfAbstractMemPosition],1);
  LChildsCount := Length(ANode.childs);
  Move(LChildsCount,LBuff[FAbstractMem.SizeOfAbstractMemPosition+1],1);
  Move(AChildsPosition,LBuff[FAbstractMem.SizeOfAbstractMemPosition+4],FAbstractMem.SizeOfAbstractMemPosition);
  for i := 0 to LItemsCount-1 do begin
    Move(ANode.data[i], LBuff[(FAbstractMem.SizeOfAbstractMemPosition*2)+4 + (i*FAbstractMem.SizeOfAbstractMemPosition)],
      FAbstractMem.SizeOfAbstractMemPosition);
  end;
  FAbstractMem.Write(ANode.identify,LBuff[0],Length(LBuff));
end;

procedure TAbstractMemBTree.SetCount(const ANewCount: Integer);
begin
  inherited;
  SaveHeader;
end;

procedure TAbstractMemBTree.SetNil(var AIdentify: TAbstractMemPosition);
begin
  inherited;
  AIdentify := 0;
end;

procedure TAbstractMemBTree.SetRoot(var Value: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode);
begin
  CheckInitialized;
  inherited;
  FrootPosition := Value.identify;
  SaveHeader;
end;

{ TAbstractMemBTreeData<TData> }

function TAbstractMemBTreeData<TData>.AddData(const AData: TData): Boolean;
var Lzone : TAMZone;
begin
  Lzone := SaveData(AData);
  Result := inherited Add(Lzone.position);
  if Not Result then begin
    // Dispose
    FAbstractMem.Dispose(Lzone);
  end;
end;

constructor TAbstractMemBTreeData<TData>.Create(AAbstractMem: TAbstractMem;
  const AInitialZone: TAMZone; AAllowDuplicates: Boolean; AOrder: Integer;
  const AOnCompareAbstractMemDataMethod: TComparison<TData>);
begin
  inherited Create(AAbstractMem,AInitialZone,AAllowDuplicates,AOrder);
  FOnCompareAbstractMemData := AOnCompareAbstractMemDataMethod;
  FLeft_Pos  := 0;
  FRight_Pos := 0;
end;

function TAbstractMemBTreeData<TData>.DeleteData(const AData: TData): Boolean;
var LAbstractMemPos : TAbstractMemPosition;
begin
  if FindData(AData,LAbstractMemPos) then begin
    Delete(LAbstractMemPos);
    FAbstractMem.Dispose(LAbstractMemPos);
    Result := True;
    if FLeft_Pos=LAbstractMemPos then FLeft_Pos := 0;
    if FRight_Pos=LAbstractMemPos then FRight_Pos := 0;
  end else Result := False;
end;

function TAbstractMemBTreeData<TData>.DoCompareData(const ALeftData, ARightData: TAbstractMemPosition): Integer;
var Ltmp : TData;
begin
  Assert((ALeftData<>0) and (ARightData<>0) and (ARightData<>1),Format('DoCompareData: Invalid Left %d or Right %d (data cannot be 0 neither 1)',[ALeftData,ARightData]));
  if (ALeftData=ARightData) then begin
    // Comparing same data because stored on same position
    Exit(0);
  end;
  Assert(ALeftData<>ARightData,Format('DoCompareData: Left (%d) and Right (%d) are equals',[ALeftData,ARightData]));
  if (ALeftData=1) then begin
    if (FRight_Pos=0) or (FRight_Pos<>ARightData) then begin
      if (FLeft_Pos=ARightData) then begin
        Result := FOnCompareAbstractMemData(FSearchTarget,FLeft_Data);
        Exit;
      end;
      FRight_Pos := ARightData;
      FRight_Data := LoadData(ARightData);
    end;
    Result := FOnCompareAbstractMemData(FSearchTarget,FRight_Data);
  end else begin
    if (FLeft_Pos=0) or (FLeft_Pos<>ALeftData) then begin
      if (FRight_Pos=ALeftData) then begin
        // Use right as left
        if (FLeft_Pos<>ARightData) then begin
          // Left is not right, reload
          FLeft_Pos := ARightData;
          FLeft_Data := LoadData(ARightData);
        end;
        Result := FOnCompareAbstractMemData(FRight_Data,FLeft_Data);
        Exit;
      end;
      FLeft_Pos := ALeftData;
      FLeft_Data := LoadData(ALeftData);
    end;
    if (FRight_Pos=0) or (FRight_Pos<>ARightData) then begin
      FRight_Pos := ARightData;
      FRight_data := LoadData(ARightData);
    end;
    Result := FOnCompareAbstractMemData(FLeft_data,FRight_data);
  end;
end;

function TAbstractMemBTreeData<TData>.FindData(const AData: TData;
  var APosition: TAbstractMemPosition): Boolean;
var Lnode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode;
  LiPosNode : Integer;
begin
  FAbstractBTreeLock.Acquire;
  try
  FSearchTarget := AData;
  ClearNode(Lnode);
  if Find(1,Lnode,LiPosNode) then begin
    APosition := Lnode.data[LiPosNode];
    Result := True;
  end else begin
    // if Node exists will set APosition of previous value, otherwise will set 0
    if Lnode.Count>LiPosNode then APosition := Lnode.data[LiPosNode]
    else if Lnode.Count>0 then APosition := Lnode.data[Lnode.Count-1]
    else APosition := 0;
    Result := False;
  end;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractMemBTreeData<TData>.FindDataHighest(out AHighest: TData): Boolean;
var Lpos : TAbstractMemPosition;
begin
  if FindHighest(Lpos) then begin
    Result := True;
    AHighest := LoadData(Lpos);
  end else Result := False;
end;

function TAbstractMemBTreeData<TData>.FindDataLowest(out ALowest: TData): Boolean;
var Lpos : TAbstractMemPosition;
begin
  if FindLowest(Lpos) then begin
    Result := True;
    ALowest := LoadData(Lpos);
  end else Result := False;
end;

function TAbstractMemBTreeData<TData>.FindDataPrecessor(const AData: TData; var APrecessor: TData): Boolean;
var Lnode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode;
  LiPosNode : Integer;
  Lpos : TAbstractMemPosition;
begin
  FAbstractBTreeLock.Acquire;
  try
  FSearchTarget := AData;
  if Find(1,Lnode,LiPosNode) then begin
    if FindPrecessor(Lnode.data[LiPosNode],Lpos) then begin
      Result := True;
      APrecessor := LoadData(Lpos);
    end else Result := False;
  end else Result := False;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractMemBTreeData<TData>.FindDataSuccessor(const AData: TData; var ASuccessor: TData): Boolean;
var Lnode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode;
  LiPosNode : Integer;
  Lpos : TAbstractMemPosition;
begin
  FAbstractBTreeLock.Acquire;
  try
  FSearchTarget := AData;
  if Find(1,Lnode,LiPosNode) then begin
    if FindSuccessor(Lnode.data[LiPosNode],Lpos) then begin
      Result := True;
      ASuccessor := LoadData(Lpos);
    end else Result := False;
  end else Result := False;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

initialization

finalization

end.
