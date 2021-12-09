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
  UAVLCache,
  UOrderedList, UAbstractMem, UAbstractBTree;

{$I ./ConfigAbstractMem.inc }

type
  EAbstractMemBTree = Class(Exception);

  TAbstractMemBTree = Class( TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition> )
    // BTree implementation on AbstractMem will use TIdentify and TData as a TAbstractMemPosition (aka pointer inside AbstractMem)
    // Internal search process will convert TData pointer to final TData value for
    // comparisions
  public
    type
      TAVLABTreeCache = Class(TAVLCache<TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode>)
      end;
  private
    const
          CT_AbstractMemBTree_Magic = 'AMBT'; // DO NOT LOCALIZE MUST BE 4 BYTES LENGTH
    var
    FrootPosition : TAbstractMemPosition;
    FBTreeCache : TAVLABTreeCache;
    procedure SaveHeader;
    Procedure CheckInitialized;
    procedure LoadNodeHeader(const APosition : TAbstractMemPosition; var ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; var AChildsCount : Integer; var AChildsPosition : TAbstractMemPosition);
    procedure SaveNodeHeader(const ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; const AChildsPosition : TAbstractMemPosition);
    function GetNodeHeaderSize : Integer;
    function CacheCompareBTree(const ALeft,ARight : TAVLCache<TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode>.PAVLCacheMemData) : Integer;
    function OnGetCopyDataMethod(Const AData : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode) : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode;
  protected
    FInitialZone : TAMZone;
    FAbstractMem : TAbstractMem;
    function GetRoot: TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; override;
    procedure SetRoot(var Value: TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode); override;
    function NewNode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; override;
    procedure DisposeNode(var ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode); override;
    procedure SetNil(var AIdentify : TAbstractMemPosition); override;
    procedure SaveNode(var ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode); override;
    procedure SetCount(const ANewCount : Integer); override;
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
    property InitialZone : TAMZone read FInitialZone;
    function GetNullData : TAbstractMemPosition; override;
    property BTreeCache : TAVLABTreeCache read FBTreeCache;
  End;

  {$IFnDEF FPC}
  TAbstractMemBTreeDataIndex<TBTreeData> = Class;
  {$ENDIF}

  TAbstractMemBTreeDataAbstract<TBTreeData> = Class(TAbstractMemBTree)
  private
    var
    // FLeft_ and FRight_ will be used as a cache for improvement calls on DoCompareData
    FLeft_Pos, FRight_Pos : TAbstractMemPosition;
    FLeft_Data, FRight_Data : TBTreeData;
    FSearchTarget : TBTreeData;
    FOnCompareAbstractMemData: TComparison<TBTreeData>;
  protected
    function DoCompareData(const ALefTBTreeData, ARighTBTreeData: TAbstractMemPosition): Integer; override;
    //
    function LoadData(const APosition : TAbstractMemPosition) : TBTreeData; virtual; abstract;
    function SaveData(const AData : TBTreeData) : TAMZone; virtual; abstract;
    function GetCopyOfData(Const AData : TBTreeData) : TBTreeData;  virtual;
    procedure DoOnFindProcessStart; override;
    procedure DoOnFindProcessEnd; override;
    //
    function GetData(const APosition : TAbstractMemPosition) : TBTreeData; virtual;
    function AddInherited(const AAbstractMemPosition: TAbstractMemPosition) : Boolean;
    function DeleteInherited(const AAbstractMemPosition: TAbstractMemPosition) : Boolean;
  public
    constructor Create(AAbstractMem : TAbstractMem; const AInitialZone: TAMZone; AAllowDuplicates : Boolean; AOrder : Integer;
      const AOnCompareAbstractMemDataMethod: TComparison<TBTreeData>); reintroduce;
    destructor Destroy; override;
    procedure Add(); reintroduce;
    procedure Delete(); reintroduce;
    function FindData(const AData: TBTreeData; out ADataEqualOrPrecessorFound : TAbstractMemPosition; var AFoundData : TBTreeData) : Boolean; overload;
    function FindData(const AData: TBTreeData; var AFoundData : TBTreeData) : Boolean; overload;
    function FindDataPos(const AData: TBTreeData; out ADataEqualOrPrecessorFound : TAbstractMemPosition) : Boolean; overload;
    function FindDataPos(const AData: TBTreeData; out ADataEqualOrPrecessorFound : TAbstractMemPosition; out ANode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode; out iPos : Integer) : Boolean; overload;
    function FindDataPrecessor(const AData : TBTreeData; var APrecessor : TBTreeData) : Boolean;
    function FindDataSuccessor(const AData : TBTreeData; var ASuccessor : TBTreeData) : Boolean;
    function FindDataLowest(out ALowest : TBTreeData) : Boolean;
    function FindDataHighest(out AHighest : TBTreeData) : Boolean;
  End;

  TAbstractMemBTreeData<TBTreeData> = Class(TAbstractMemBTreeDataAbstract<TBTreeData>)
  public
    type
      TAVLABTreeDataCacheData = record
        position : TAbstractMemPosition;
        data : TBTreeData;
      end;
      TAVLABTreeDataCache = Class(TAVLCache<TAVLABTreeDataCacheData>)
      protected
      public
    function ToString(const AData : TAVLABTreeDataCacheData) : String; override;
      end;
  private
    //    Ref: 20211111-1  -- TODO
    //    FreePascal issue: Does not allow recursive Generics...
    //    due to this issue (on Delphi is allowed) then I must use TList< TOjbect > instead
    //    last FreePascal version with this issue: 3.2.0  (will need to check on future versions)
    {$IFDEF FPC}
    FIndexes : TList< TObject >;
    {$ELSE}
    //    Ref: 20211111-1 I can't use this... in Delphi it works! Not in FreePascal... SHIT!
    FIndexes : TList< TAbstractMemBTreeDataIndex<TBTreeData> >;
    {$ENDIF}
    FBTreeDataCache : TAVLABTreeDataCache;
    function CacheCompareBTreeData(const ALeft,ARight : TAVLCache<TAVLABTreeDataCacheData>.PAVLCacheMemData) : Integer;
  protected
    function GetData(const APosition : TAbstractMemPosition) : TBTreeData; override;
    procedure DisposeData(var AData : TAbstractMemPosition); override;
    procedure DeletedData(const AData: TBTreeData); virtual;
  public
    constructor Create(AAbstractMem : TAbstractMem; const AInitialZone: TAMZone; AAllowDuplicates : Boolean; AOrder : Integer;
      const AOnCompareAbstractMemDataMethod: TComparison<TBTreeData>);
    destructor Destroy; override;
    function CanAddData(const AData: TBTreeData) : Boolean;
    function AddData(const AData: TBTreeData) : Boolean;
    function DeleteData(const AData: TBTreeData) : Boolean;
    function IndexesCount : Integer;
    //    See ref: 20211111-1
    {$IFDEF FPC}
    function GetIndex(AIndex : Integer) : TObject;
    {$ELSE}
    function GetIndex(AIndex : Integer) : TAbstractMemBTreeDataIndex<TBTreeData>;
    {$ENDIF}
    procedure CheckConsistency; override;
    property BTreeDataCache : TAVLABTreeDataCache read FBTreeDataCache;
  End;

  TAbstractMemBTreeDataIndex<TBTreeData> = Class(TAbstractMemBTreeDataAbstract<TBTreeData>)
  private
    FIndexed : TAbstractMemBTreeData<TBTreeData>;
    FCompareModeInsertingOrDeleting : Boolean;
  protected
    function DoCompareData(const ALefTBTreeData, ARighTBTreeData: TAbstractMemPosition): Integer; override;
    function LoadData(const APosition : TAbstractMemPosition) : TBTreeData; override;
    function SaveData(const AData : TBTreeData) : TAMZone; override;
  public
    constructor Create(AAbstractMemBTreeData : TAbstractMemBTreeData<TBTreeData>;
      AInitialZone: TAMZone;
      AAllowDuplicates : Boolean; AOrder : Integer;
      const AOnCompareAbstractMemDataMethod: TComparison<TBTreeData>);
    destructor Destroy; override;
    procedure CheckConsistency; override;
    procedure Lock; override;
    procedure Unlock; override;
  End;

implementation

{ TAbstractMemBTree<TBTreeData> }

function TAbstractMemBTree.CacheCompareBTree(const ALeft,
  ARight: TAVLCache<TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode>.PAVLCacheMemData): Integer;
begin
  if ALeft.data.identify<ARight.data.identify then Result := -1
  else if ALeft.data.identify>ARight.data.identify then Result := 1
  else Result := 0;
end;

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
  //
  {$IFDEF FPC}
  //    Ref: 20211126-2  -- TODO
  //    FPC (Tested on 3.2.0) does not allow use "CacheCompareBTree" for problems withs generics...
  //    Nedd to deeply search why or to test on futures releases...
  FBTreeCache := Nil;
  {$ELSE}
  FBTreeCache :=  TAVLABTreeCache.Create(100000,CacheCompareBTree);
  {$ENDIF}
end;

destructor TAbstractMemBTree.Destroy;
begin
  //
  FreeAndNil(FBTreeCache);
  inherited;
end;

procedure TAbstractMemBTree.DisposeNode(var ANode: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode);
var LOld : TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
  LChildsCount : Integer;
  LChildsPosition : TAbstractMemPosition;
begin
  if Assigned(FBTreeCache) then begin
    FBTreeCache.Remove(ANode);
  end;
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
  LSearch,LFound : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode;
begin
  if Assigned(FBTreeCache) then begin
    LSearch.identify := AIdentify;
    if FBTreeCache.Find(LSearch,LFound) then begin
      Result := LFound;
      Exit;
    end;
  end;
  LoadNodeHeader(AIdentify,Result,LChildsCount,LChildsPosition);
  if LChildsCount>0 then begin
    SetLength(Result.childs,LChildsCount);
    if (LChildsCount>MaxChildrenPerNode) then raise EAbstractMemBTree.Create(Format('Childrens in node %d out of range [0..%d]',[LChildsCount,MaxChildrenPerNode]));
    SetLength(LBuff,(MaxChildrenPerNode*FAbstractMem.SizeOfAbstractMemPosition));
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
  if Assigned(FBTreeCache) then begin
    FBTreeCache.Add(Result);
  end;
end;

function TAbstractMemBTree.GetNodeHeaderSize: Integer;
begin
  Result := ((FAbstractMem.SizeOfAbstractMemPosition*2)+4) + (FAbstractMem.SizeOfAbstractMemPosition*MaxItemsPerNode);
end;

function TAbstractMemBTree.GetNullData: TAbstractMemPosition;
begin
  Result := 0;
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

function TAbstractMemBTree.OnGetCopyDataMethod(
  const AData: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode): TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
begin
  Result.identify := AData.identify;
  Result.parent := AData.parent;
  Result.data := Copy(AData.data);
  Result.childs := Copy(AData.childs);
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
  if Assigned(FBTreeCache) then begin
    FBTreeCache.Remove(ANode);
  end;
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
  if Assigned(FBTreeCache) then begin
    FBTreeCache.Add(ANode);
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

{ TAbstractMemBTreeDataAbstract<TBTreeData> }

procedure TAbstractMemBTreeDataAbstract<TBTreeData>.Add;
begin
  raise EAbstractMemBTree.Create('Invalid use of Abstract function '+ClassName+'.Add');
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.AddInherited(
  const AAbstractMemPosition: TAbstractMemPosition): Boolean;
begin
  Result := inherited Add(AAbstractMemPosition);
end;

constructor TAbstractMemBTreeDataAbstract<TBTreeData>.Create(
  AAbstractMem: TAbstractMem; const AInitialZone: TAMZone;
  AAllowDuplicates: Boolean; AOrder: Integer;
  const AOnCompareAbstractMemDataMethod: TComparison<TBTreeData>);
begin
  inherited Create(AAbstractMem,AInitialZone,AAllowDuplicates,AOrder);
  FOnCompareAbstractMemData := AOnCompareAbstractMemDataMethod;
  FLeft_Pos  := 0;
  FRight_Pos := 0;
end;

procedure TAbstractMemBTreeDataAbstract<TBTreeData>.Delete;
begin
  raise EAbstractMemBTree.Create('Invalid use of Abstract function '+ClassName+'.Delete');
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.DeleteInherited(
  const AAbstractMemPosition: TAbstractMemPosition): Boolean;
begin
  Result := Inherited Delete(AAbstractMemPosition);
end;

destructor TAbstractMemBTreeDataAbstract<TBTreeData>.Destroy;
begin
  inherited;
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.DoCompareData(const ALefTBTreeData,
  ARighTBTreeData: TAbstractMemPosition): Integer;
var Ltmp : TBTreeData;
begin
  Assert((ALefTBTreeData<>0) and (ARighTBTreeData<>0) and (ARighTBTreeData<>1),Format('DoCompareData: Invalid Left %d or Right %d (data cannot be 0 neither 1)',[ALefTBTreeData,ARighTBTreeData]));
  if (ALefTBTreeData=ARighTBTreeData) then begin
    // Comparing same data because stored on same position
    Exit(0);
  end;
  Assert(ALefTBTreeData<>ARighTBTreeData,Format('DoCompareData: Left (%d) and Right (%d) are equals',[ALefTBTreeData,ARighTBTreeData]));
  if (ALefTBTreeData=1) then begin
    if (FRight_Pos=0) or (FRight_Pos<>ARighTBTreeData) then begin
      if (FLeft_Pos=ARighTBTreeData) then begin
        Result := FOnCompareAbstractMemData(FSearchTarget,FLeft_Data);
        Exit;
      end;
      FRight_Pos := ARighTBTreeData;
      FRight_Data := GetData(ARighTBTreeData);
    end;
    Result := FOnCompareAbstractMemData(FSearchTarget,FRight_Data);
  end else begin
    if (FLeft_Pos=0) or (FLeft_Pos<>ALefTBTreeData) then begin
      if (FRight_Pos=ALefTBTreeData) then begin
        // Use right as left
        if (FLeft_Pos<>ARighTBTreeData) then begin
          // Left is not right, reload
          FLeft_Pos := ARighTBTreeData;
          FLeft_Data := GetData(ARighTBTreeData);
        end;
        Result := FOnCompareAbstractMemData(FRight_Data,FLeft_Data);
        Exit;
      end;
      FLeft_Pos := ALefTBTreeData;
      FLeft_Data := GetData(ALefTBTreeData);
    end;
    if (FRight_Pos=0) or (FRight_Pos<>ARighTBTreeData) then begin
      FRight_Pos := ARighTBTreeData;
      FRight_data := GetData(ARighTBTreeData);
    end;
    Result := FOnCompareAbstractMemData(FLeft_data,FRight_data);
  end;
end;

procedure TAbstractMemBTreeDataAbstract<TBTreeData>.DoOnFindProcessEnd;
begin
  inherited;
  FLeft_Pos  := 0;
  FRight_Pos := 0;
end;

procedure TAbstractMemBTreeDataAbstract<TBTreeData>.DoOnFindProcessStart;
begin
  inherited;
  FLeft_Pos  := 0;
  FRight_Pos := 0;
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.FindData(const AData: TBTreeData;
  out ADataEqualOrPrecessorFound : TAbstractMemPosition; var AFoundData : TBTreeData): Boolean;
begin
  if FindDataPos(AData,ADataEqualOrPrecessorFound) then begin
    Result := True;
    AFoundData := GetData(ADataEqualOrPrecessorFound);
  end else begin
    if IsNil(ADataEqualOrPrecessorFound) then FindDataLowest(AFoundData)
    else AFoundData := GetData(ADataEqualOrPrecessorFound);
    Result := False;
  end;
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.FindData(
  const AData: TBTreeData; var AFoundData: TBTreeData): Boolean;
var LPos : TAbstractMemPosition;
begin
  Result := FindData(AData,LPos,AFoundData);
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.FindDataHighest(
  out AHighest: TBTreeData): Boolean;
var Lpos : TAbstractMemPosition;
begin
  if FindHighest(Lpos) then begin
    Result := True;
    AHighest := GetData(Lpos);
  end else Result := False;
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.FindDataLowest(
  out ALowest: TBTreeData): Boolean;
var Lpos : TAbstractMemPosition;
begin
  if FindLowest(Lpos) then begin
    Result := True;
    ALowest := GetData(Lpos);
  end else Result := False;
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.FindDataPos(
  const AData: TBTreeData; out ADataEqualOrPrecessorFound : TAbstractMemPosition): Boolean;
var LNode: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
  iPos : Integer;
begin
  Result := FindDataPos(AData,ADataEqualOrPrecessorFound,LNode,iPos);
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.FindDataPos(
  const AData: TBTreeData; out ADataEqualOrPrecessorFound: TAbstractMemPosition;
  out ANode: TAbstractBTree<TAbstractMemPosition, TAbstractMemPosition>.TAbstractBTreeNode;
  out iPos: Integer): Boolean;
begin
  Lock;
  try
    FSearchTarget := AData;
    Result := FindExt(1,ADataEqualOrPrecessorFound,ANode,iPos);
  finally
    Unlock;
  end;
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.FindDataPrecessor(
  const AData: TBTreeData; var APrecessor: TBTreeData): Boolean;
var Lnode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode;
  LiPosNode : Integer;
  Lpos : TAbstractMemPosition;
begin
  Lock;
  try
  FSearchTarget := AData;
  if inherited Find(1,Lnode,LiPosNode) then begin
    if FindPrecessor(Lnode.data[LiPosNode],Lpos) then begin
      Result := True;
      APrecessor := GetData(Lpos);
    end else Result := False;
  end else Result := False;
  finally
    Unlock;
  end;
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.FindDataSuccessor(
  const AData: TBTreeData; var ASuccessor: TBTreeData): Boolean;
var Lnode : TAbstractBTree<TAbstractMemPosition,TAbstractMemPosition>.TAbstractBTreeNode;
  LiPosNode : Integer;
  Lpos : TAbstractMemPosition;
begin
  Lock;
  try
  FSearchTarget := AData;
  if inherited Find(1,Lnode,LiPosNode) then begin
    if FindSuccessor(Lnode.data[LiPosNode],Lpos) then begin
      Result := True;
      ASuccessor := GetData(Lpos);
    end else Result := False;
  end else Result := False;
  finally
    Unlock;
  end;
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.GetCopyOfData(const AData: TBTreeData): TBTreeData;
begin
  Result := AData;
end;

function TAbstractMemBTreeDataAbstract<TBTreeData>.GetData(const APosition: TAbstractMemPosition): TBTreeData;
begin
  Result := GetCopyOfData(LoadData(APosition));
end;

{ TAbstractMemBTreeData<TBTreeData> }

function TAbstractMemBTreeData<TBTreeData>.AddData(const AData: TBTreeData): Boolean;
var Lzone : TAMZone;
  i : Integer;
  LIndexPosition : TAbstractMemPosition;
  LBTreeIndex : TAbstractMemBTreeDataIndex<TBTreeData>;
  LCache : TAVLABTreeDataCacheData;
begin
  Lock;
  Try
  Result := True;
  i := 0;
  while (Result) and (i<FIndexes.Count) do begin
    LBTreeIndex := TAbstractMemBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
    if (Not LBTreeIndex.AllowDuplicates) then begin
      Result := Not LBTreeIndex.FindDataPos(AData,LIndexPosition);
    end;
    inc(i);
  end;
  if Result then begin
    Lzone := SaveData(AData);
    Try
      Result := AddInherited(Lzone.position);
      if Result then begin
        for i := 0 to FIndexes.Count-1 do begin
          LBTreeIndex := TAbstractMemBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
          Try
            LBTreeIndex.FCompareModeInsertingOrDeleting := True;
            if Not LBTreeIndex.AddInherited(LZone.position) then begin
              raise EAbstractMemBTree.Create(Format('Fatal error adding index %d/%d with data at %s',
                [i+1,FIndexes.Count,Lzone.ToString]));
            end;
          Finally
            LBTreeIndex.FCompareModeInsertingOrDeleting := False;
          End;
        end;
      end;
    Finally
      if Not Result then begin
        // Dispose
        FAbstractMem.Dispose(Lzone);
        If Assigned(FBTreeDataCache) then begin
          LCache.position := Lzone.position;
          FBTreeDataCache.Remove(LCache);
        end;
      end;
    End;
  end;
  Finally
    Unlock;
  End;
end;

function TAbstractMemBTreeData<TBTreeData>.CacheCompareBTreeData(
  const ALeft, ARight: TAVLCache<TAVLABTreeDataCacheData>.PAVLCacheMemData): Integer;
begin
  if ALeft.data.position<ARight.data.position then Result := -1
  else if ALeft.data.position>ARight.data.position then Result := 1
  else Result := 0;
end;

function TAbstractMemBTreeData<TBTreeData>.CanAddData(
  const AData: TBTreeData): Boolean;
var i : Integer;
  LIndexPosition : TAbstractMemPosition;
  LBTreeIndex : TAbstractMemBTreeDataIndex<TBTreeData>;
begin
  // Check in indexes
  Result := True;
  i := 0;
  while (Result) and (i<FIndexes.Count) do begin
    LBTreeIndex := TAbstractMemBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
    if (Not LBTreeIndex.AllowDuplicates) then begin
      Result :=  Not (LBTreeIndex.FindDataPos(AData,LIndexPosition));
    end;
    inc(i);
  end;
  if (Result) And (Not AllowDuplicates) then begin
    Result := Not FindDataPos(AData,LIndexPosition);
  end;
end;

procedure TAbstractMemBTreeData<TBTreeData>.CheckConsistency;
var i, nCount : Integer;
 LBTreeIndex : TAbstractMemBTreeDataIndex<TBTreeData>;
 LSearch,LFound : TBTreeData;
begin
  inherited;
  nCount := 0;
  if FindDataLowest(LFound) then begin
    inc(nCount);
    for i := 0 to FIndexes.Count-1 do begin
      LBTreeIndex := TAbstractMemBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
      if Not LBTreeIndex.FindData(LFound,LSearch) then raise EAbstractMemBTree.Create(Format('Consistency error data %d not found on index %d/%d',[nCount, i+1,FIndexes.Count]));
    end;
    while FindDataSuccessor(LSearch,LFound) do begin
      inc(nCount);
      for i := 0 to FIndexes.Count-1 do begin
        LBTreeIndex := TAbstractMemBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
        if Not LBTreeIndex.FindData(LFound,LSearch) then raise EAbstractMemBTree.Create(Format('Consistency error data %d not found on index %d/%d',[nCount, i+1,FIndexes.Count]));
      end;
    end;
  end;
  for i := 0 to FIndexes.Count-1 do begin
    LBTreeIndex := TAbstractMemBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
    if (LBTreeIndex.Count > Self.Count) then raise EAbstractMemBTree.Create(Format('Consistency error on index %d/%d count %d > %d',[i+1,FIndexes.Count,LBTreeIndex.Count,Self.Count]));
    LBTreeIndex.CheckConsistency;
  end;
end;

constructor TAbstractMemBTreeData<TBTreeData>.Create(AAbstractMem: TAbstractMem;
  const AInitialZone: TAMZone; AAllowDuplicates: Boolean; AOrder: Integer;
  const AOnCompareAbstractMemDataMethod: TComparison<TBTreeData>);
begin
  {$IFDEF FPC}
  FIndexes := TList< TObject >.Create;
  {$ELSE}
  FIndexes := TList< TAbstractMemBTreeDataIndex<TBTreeData> >.Create;
  {$ENDIF}
  inherited Create(AAbstractMem,AInitialZone,AAllowDuplicates,AOrder,AOnCompareAbstractMemDataMethod);
  {$IFDEF FPC}
  //    Ref: 20211126-1  -- TODO
  //    FPC (Tested on 3.2.0) does not allow use "CacheCompareBTreeData" for problems withs generics...
  //    Nedd to deeply search why or to test on futures releases...
  FBTreeDataCache := Nil;
  {$ELSE}
  FBTreeDataCache :=  TAVLABTreeDataCache.Create(100000,CacheCompareBTreeData);
  {$ENDIF}
end;

function TAbstractMemBTreeData<TBTreeData>.DeleteData(const AData: TBTreeData): Boolean;
var LAbstractMemPos, LindexPosition : TAbstractMemPosition;
  i : Integer;
  LBTreeIndex : TAbstractMemBTreeDataIndex<TBTreeData>;
  LCache : TAVLABTreeDataCacheData;
begin
  Lock;
  Try
  if FindDataPos(AData,LAbstractMemPos) then begin
    // Delete from indexes
    for i := 0 to FIndexes.Count-1 do begin
      LBTreeIndex := TAbstractMemBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
      try
        LBTreeIndex.FCompareModeInsertingOrDeleting := True;
        if Not LBTreeIndex.FindExt(LAbstractMemPos,LIndexPosition) then begin
          // Fatal error. Not found
          raise EAbstractMemBTree.Create(Format('Fatal error Data not found in index %d/%d to Delete from pos 0x%s',[i+1,Findexes.Count,LAbstractMemPos.ToHexString]));
        end;
        if not LBTreeIndex.DeleteInherited(LindexPosition) then begin
          raise EAbstractMemBTree.Create(Format('Fatal error Data not deleted in index %d/%d from pos 0x%s at pos 0x%s',[i+1,Findexes.Count,LAbstractMemPos.ToHexString,LindexPosition.ToHexString]));
        end;
      finally
        LBTreeIndex.FCompareModeInsertingOrDeleting := False;
      end;
    end;
    //
    DeleteInherited(LAbstractMemPos);
    FAbstractMem.Dispose(LAbstractMemPos);
    Result := True;
    if FLeft_Pos=LAbstractMemPos then FLeft_Pos := 0;
    if FRight_Pos=LAbstractMemPos then FRight_Pos := 0;
    //
    If Assigned(FBTreeDataCache) then begin
      LCache.position := LAbstractMemPos;
      FBTreeDataCache.Remove(LCache);
    end;
    //
    DeletedData(AData);
  End else Result := False;
  Finally
    Unlock;
  End;
end;

procedure TAbstractMemBTreeData<TBTreeData>.DeletedData(
  const AData: TBTreeData);
begin
  //
end;

destructor TAbstractMemBTreeData<TBTreeData>.Destroy;
var i : Integer;
 LBTreeIndex : TAbstractMemBTreeDataIndex<TBTreeData>;
begin
  for i := 0 to FIndexes.Count-1 do begin
    LBTreeIndex := TAbstractMemBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
    LBTreeIndex.FIndexed := Nil;
  end;
  FreeAndNil(Findexes);
  FreeAndNil(FBTreeDataCache);
  inherited;
end;

procedure TAbstractMemBTreeData<TBTreeData>.DisposeData(
  var AData: TAbstractMemPosition);
begin
  inherited;
  // Will be called on EraseTreeEx
  FAbstractMem.Dispose(AData);
end;

function TAbstractMemBTreeData<TBTreeData>.GetData(
  const APosition: TAbstractMemPosition): TBTreeData;
var LSearch,LFound : TAVLABTreeDataCacheData;
begin
  if (Assigned(FBTreeDataCache)) then begin
    LSearch.position := APosition;
    if FBTreeDataCache.Find(LSearch,LFound) then begin
      Result := GetCopyOfData( LFound.data );
    end else begin
      LSearch.data := LoadData(APosition);
      FBTreeDataCache.Add(LSearch);
      Result := GetCopyOfData( LSearch.data );
    end;
  end else begin
    Result := inherited GetData(APosition);
  end;
end;

{$IFDEF FPC}
function TAbstractMemBTreeData<TBTreeData>.GetIndex(AIndex: Integer): TObject;
begin
  Result := FIndexes.Items[AIndex];
end;
{$ELSE}
function TAbstractMemBTreeData<TBTreeData>.GetIndex(
  AIndex: Integer): TAbstractMemBTreeDataIndex<TBTreeData>;
begin
  Result := FIndexes.Items[AIndex];
end;
{$ENDIF}

function TAbstractMemBTreeData<TBTreeData>.IndexesCount: Integer;
begin
  Result := FIndexes.Count;
end;

{ TAbstractMemBTreeDataIndex<TBTreeData> }

procedure TAbstractMemBTreeDataIndex<TBTreeData>.CheckConsistency;
var i, nCount : Integer;
 APreviousData, ACurrentData : TBTreeData;
begin
  inherited;
  nCount := 0;
  if FindDataLowest(APreviousData) then begin
    nCount := 1;
    while FindDataSuccessor(APreviousData,ACurrentData) do begin
      inc(nCount);
      i := FOnCompareAbstractMemData(APreviousData,ACurrentData);
      if ((Not AllowDuplicates) and (i>=0)) or (i>=0) then raise EAbstractMemBTree.Create(Format('Invalid consistency on Index comparing pos %d and %d result %d',[nCount-1,nCount,i]));
      APreviousData := ACurrentData;
    end;
  end;
end;

constructor TAbstractMemBTreeDataIndex<TBTreeData>.Create(
  AAbstractMemBTreeData: TAbstractMemBTreeData<TBTreeData>;
  AInitialZone: TAMZone;
  AAllowDuplicates: Boolean; AOrder: Integer;
  const AOnCompareAbstractMemDataMethod: TComparison<TBTreeData>);
begin
  FIndexed := AAbstractMemBTreeData;
  FIndexed.FIndexes.Add(Self);
  FOnCompareAbstractMemData := AOnCompareAbstractMemDataMethod;
  FCompareModeInsertingOrDeleting := False;
  inherited Create(FIndexed.FAbstractMem,AInitialZone,AAllowDuplicates,AOrder,AOnCompareAbstractMemDataMethod);
end;

destructor TAbstractMemBTreeDataIndex<TBTreeData>.Destroy;
begin
  if Assigned(FIndexed) then begin
    FIndexed.FIndexes.Remove(Self);
  end;
  inherited;
end;

function TAbstractMemBTreeDataIndex<TBTreeData>.DoCompareData(
  const ALefTBTreeData, ARighTBTreeData: TAbstractMemPosition): Integer;
begin
  Result := inherited DoCompareData(ALeftBTreeData,ARightBTreeData);
  if (FCompareModeInsertingOrDeleting) and (Result=0) then begin
    if ALefTBTreeData<ARighTBTreeData then Result := -1
    else if ALefTBTreeData>ARighTBTreeData then Result := 1
    else Result := 0;
  end;
end;

function TAbstractMemBTreeDataIndex<TBTreeData>.LoadData(const APosition: TAbstractMemPosition): TBTreeData;
begin
  Result := FIndexed.GetData(APosition);
end;

procedure TAbstractMemBTreeDataIndex<TBTreeData>.Lock;
begin
  FIndexed.Lock;
  Try
    inherited;
  Except
    FIndexed.Unlock;
    raise;
  End;
end;

function TAbstractMemBTreeDataIndex<TBTreeData>.SaveData(const AData: TBTreeData): TAMZone;
begin
  // This is an index, never suposed to be called this function
  raise EAbstractMemBTree.Create('ERROR DEV 20211130-01');
end;

procedure TAbstractMemBTreeDataIndex<TBTreeData>.Unlock;
begin
  inherited;
  FIndexed.Unlock;
end;

{ TAbstractMemBTreeData<TBTreeData>.TAVLABTreeDataCache }

function TAbstractMemBTreeData<TBTreeData>.TAVLABTreeDataCache.ToString(
  const AData: TAVLABTreeDataCacheData): String;
begin
  inherited;
  Result := Format('p:%d sizeof:%d',[AData.position,SizeOf(AData.data)]);
end;

initialization

finalization

end.
