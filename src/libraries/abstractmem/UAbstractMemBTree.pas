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
    const CT_MIN_INITIAL_POSITION_SIZE = 16;
          CT_AbstractMemBTree_Magic = 'AMBT'; // DO NOT LOCALIZE MUST BE 4 BYTES LENGTH
    var
    FInitialZone : TAMZone;
    FrootPosition : TAbstractMemPosition;
    procedure SaveHeader;
    function GetNodeSize : Integer;
    Procedure CheckInitialized;
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
    class function MinAbstractMemInitialPositionSize : Integer;
    property AbstractMem : TAbstractMem read FAbstractMem;
    property Count;
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

constructor TAbstractMemBTree.Create(AAbstractMem : TAbstractMem; const AInitialZone: TAMZone; AAllowDuplicates: Boolean;  AOrder: Integer);
var LBuff : TBytes;
 i : Integer;
 LOrder : Integer;

begin
  FAbstractMem := AAbstractMem;
  FrootPosition := 0;
  inherited Create(TComparison_Integer,TComparison_Integer,AAllowDuplicates,AOrder);
  FCount := 0;
  //
  if Not FAbstractMem.GetUsedZoneInfo(AInitialZone.position,False,FInitialZone) then begin
    if FAbstractMem.ReadOnly then begin
      // Is not initialized and is Read Only
      FInitialZone.Clear;
      Exit;
    end;
    raise EAbstractMemBTree.Create('Cannot capture zone info for initialize');
  end;
  if (FInitialZone.size<MinAbstractMemInitialPositionSize) then begin
    raise EAbstractMemBTree.Create(Format('Invalid size %d for initialize',[FInitialZone.size]));
  end;
  SetLength(LBuff,CT_MIN_INITIAL_POSITION_SIZE);
  FAbstractMem.Read(FInitialZone.position,LBuff[0],Length(LBuff));
  try
    // Check magic
    for i := 0 to CT_AbstractMemBTree_Magic.Length-1 do begin
      if LBuff[i]<>Ord(CT_AbstractMemBTree_Magic.Chars[i]) then Exit;
    end;
    Move(LBuff[4],FrootPosition,4);
    Move(LBuff[8],FCount,4);
    LOrder := 0;
    Move(LBuff[12],LOrder,4);
    if LOrder<>Order then raise EAbstractMemBTree.Create(Format('Invalid Order %d expected %d',[LOrder,Order]));
    if (((FrootPosition=0) and (FCount>0))) then raise EAbstractMemBTree.Create(Format('Invalid initial root %d vs count %d',[FrootPosition,FCount]));
  finally
    if FrootPosition<=0 then begin
      FrootPosition := 0;
      FCount := 0;
      for i := 0 to CT_AbstractMemBTree_Magic.Length-1 do begin
        LBuff[i] := Byte(Ord(CT_AbstractMemBTree_Magic.Chars[i]));
      end;
      Move(FrootPosition,LBuff[4],4);
      Move(FCount,LBuff[8],4);
      LOrder := Order;
      Move(LOrder,LBuff[12],4);
      FAbstractMem.Write(FInitialZone.position,LBuff[0],16);
      SaveHeader;
    end;
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
begin
  FAbstractMem.Dispose( ANode.identify );
  ClearNode(ANode);
end;

function TAbstractMemBTree.GetNode(AIdentify: TAbstractMemPosition): TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
var LBuff : TBytes;
  LStream : TStream;
  LByte : Byte;
  i, LItemsCount, LChildsCount : Integer;
begin
  // For each node:
  // Size = (4+2+2)+(4*MaxItemsPerNode)+(4*MaxChildrenPerNode) = GetNodeSize
  // 4 Bytes [0..3] : Parent
  // 1 Byte  [4] : Used items (0..32)
  // 1 Byte  [5] : Used childs (0 (leaf) or Used Items+1)
  // 2 Bytes [6..7] : 0 (unusued)
  // For each item:
  //   4 Bytes : data (AbstractMemPosition or Data using 4 bytes)
  // For each children:
  //   4 Bytes : Children AbstractMem position
  ClearNode(Result);
  Result.identify := AIdentify;
  SetLength(LBuff, GetNodeSize );
  FAbstractMem.Read(AIdentify,LBuff[0],Length(LBuff));
  LStream := TMemoryStream.Create;
  try
    LStream.Write(LBuff[0],Length(LBuff));
    LStream.Position := 0;
    //
    LStream.Read(Result.parent,4); // Read parent position
    LStream.Read(LByte,1);
    LItemsCount := LByte;
    LStream.Read(LByte,1);
    LChildsCount := LByte;
    LStream.Read(LByte,1);
    Assert(LByte=0);
    LStream.Read(LByte,1);
    Assert(LByte=0);
    if ((LItemsCount=0) and (Result.parent=0) and (LChildsCount=0)) then begin
      // root without data
    end else begin
      if (Result.parent=0) then begin
        if ((LItemsCount<1) or (LItemsCount>MaxItemsPerNode)) then
          raise EAbstractMemBTree.Create(Format('Root Node items %d not in range [%d..%d]',[LItemsCount,MinItemsPerNode,MaxItemsPerNode]));
      end else begin
        if ((LItemsCount<MinItemsPerNode) or (LItemsCount>MaxItemsPerNode)) then
          raise EAbstractMemBTree.Create(Format('Node items %d not in range [%d..%d]',[LItemsCount,MinItemsPerNode,MaxItemsPerNode]));
      end;
      if ((LChildsCount<>0) and (LChildsCount<>(LItemsCount+1))) then
        raise EAbstractMemBTree.Create(Format('Node childrens %d not %d+1 in range [%d..%d]',[LChildsCount,LItemsCount,MinChildrenPerNode,MaxChildrenPerNode]));
    end;
    // Read items
    SetLength(Result.data,LItemsCount);
    SetLength(Result.childs,LChildsCount);
    for i := 0 to LItemsCount-1 do begin
      LStream.Read(Result.data[i],4);
    end;
    // Read childrens
    for i := 0 to LChildsCount-1 do begin
      LStream.Read(Result.childs[i],4);
    end;
  finally
    LStream.Free;
  end;
end;

function TAbstractMemBTree.GetNodeSize: Integer;
begin
  Result := 8 + (4 * MaxItemsPerNode) + (4 * MaxChildrenPerNode);
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

class function TAbstractMemBTree.MinAbstractMemInitialPositionSize: Integer;
begin
  Result := CT_MIN_INITIAL_POSITION_SIZE;
end;

function TAbstractMemBTree.NewNode: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
begin
  CheckInitialized;
  ClearNode(Result);
  Result.identify := FAbstractMem.New(GetNodeSize).position;
end;

procedure TAbstractMemBTree.SaveHeader;
var LBuff : TBytes;
 i : Integer;
 LOrder : Integer;
begin
  CheckInitialized;
  SetLength(LBuff,16);
  for i := 0 to CT_AbstractMemBTree_Magic.Length-1 do begin
    LBuff[i] := Byte(Ord(CT_AbstractMemBTree_Magic.Chars[i]));
  end;
  Move(FrootPosition,LBuff[4],4);
  Move(FCount,LBuff[8],4);
  LOrder := Order;
  Move(LOrder,LBuff[12],4);
  FAbstractMem.Write(FInitialZone.position,LBuff[0],16);
end;

procedure TAbstractMemBTree.SaveNode(var ANode: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode);
var LBuff : TBytes;
  LStream : TStream;
  LByte : Byte;
  i, LItemsCount, LChildsCount : Integer;
begin
  CheckInitialized;
  if ((ANode.Count)>MaxItemsPerNode) or (Length(ANode.childs)>MaxChildrenPerNode) then begin
    // Protection agains saving temporal Node info with extra datas or childs
    Exit;
  end;

  // See GetNode info
  LStream := TMemoryStream.Create;
  try
    LStream.Write(ANode.parent,4);
    LItemsCount := Length(ANode.data);
    LStream.Write(LItemsCount,1);
    LChildsCount := Length(ANode.childs);
    LStream.Write(LChildsCount,1);
    LByte := 0;
    LStream.Write(LByte,1);
    LStream.Write(LByte,1);
    for i := 0 to LItemsCount-1 do begin
      LStream.Write(ANode.data[i],4)
    end;
    // Read childrens
    for i := 0 to LChildsCount-1 do begin
      LStream.Write(ANode.childs[i],4);
    end;
    SetLength(LBuff,LStream.Size);
    LStream.Position := 0;
    LStream.Read(LBuff[0],LStream.Size);
    FAbstractMem.Write(ANode.identify,LBuff[0],Length(LBuff));
  finally
    LStream.Free;
  end;
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
