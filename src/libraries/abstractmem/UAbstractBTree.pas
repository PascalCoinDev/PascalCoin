unit UAbstractBTree;

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
  SyncObjs,
  // NOTE ABOUT FREEPASCAL (2020-03-10)
  // Current version 3.0.4 does not contain valid support for Generics, using Generics from this:
  // https://github.com/PascalCoinDev/PascalCoin/tree/master/src/libraries/generics.collections
  // (Download and set folder as a "units include folder" in compiler options)
  {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults,{$ELSE}Generics.Collections,Generics.Defaults,{$ENDIF}
  UOrderedList;

{$I ./ConfigAbstractMem.inc }

{$IFDEF ABSTRACTMEM_TESTING_MODE}
  {$DEFINE ABSTRACTMEM_CIRCULAR_SEARCH_PROTECTION}
{$ENDIF}

type
  EAbstractBTree = Class(Exception);

  TAbstractBTree<TIdentify, TData> = Class
  public
    type
      TDataSource = TData;
      TIdentifyArray = Array of TIdentify;
      TDataArray = Array of TData;
      TAbstractBTreeNode = record
        identify : TIdentify;
        parent : TIdentify;
        data : TDataArray;
        childs : TIdentifyArray;
        function IsLeaf : Boolean;
        procedure InsertData(const AData : TData; AIndex : Integer);
        procedure InsertChild(const AChild : TIdentify; AIndex : Integer);
        procedure RemoveInNode(AIndex : Integer);
        procedure DeleteData(AIndex : Integer);
        procedure DeleteChild(AChildIndex : Integer);
        function Count : Integer;
      end;
  private
    FOnCompareIdentify: TComparison<TIdentify>;
    FOnCompareData: TComparison<TData>;
    FAllowDuplicates: Boolean;
    FOrder: Integer;
    FCircularProtection : Boolean;
    procedure SplitAfterInsert(var ANode : TAbstractBTreeNode; const ACircularProtectionList : TOrderedList<TIdentify>);
    procedure MoveRange(var ASourceNode, ADestNode : TAbstractBTreeNode; AFromSource, ACount, AToDest : Integer);
    procedure MoveRangeBetweenSiblings(var ASourceNode, ADestNode : TAbstractBTreeNode);
    procedure BTreeNodeToString(const ANode : TAbstractBTreeNode; ALevel, ALevelIndex : Integer; const AStrings : TStrings);
    procedure CheckConsistencyEx(const ANode: TAbstractBTreeNode; AIsGoingDown : Boolean; AParentDataIndexLeft,AParentDataIndexRight : Integer; ADatas: TList<TData>; AIdents: TOrderedList<TIdentify>; ACurrentLevel : Integer; var ALevels, ANodesCount, AItemsCount : Integer);
    function FindPrecessorExt(const ACircularProtectionList : TOrderedList<TIdentify>; var ANode : TAbstractBTreeNode; var iPos : Integer) : Boolean;
    function FindSuccessorExt(const ACircularProtectionList : TOrderedList<TIdentify>; var ANode : TAbstractBTreeNode; var iPos : Integer) : Boolean;
    procedure EraseTreeExt(var ANode : TAbstractBTreeNode);
    function FindLowestNodeExt(const ACircularProtectionList : TOrderedList<TIdentify>): TAbstractBTreeNode;
    function FindHighestNodeExt(const ACircularProtectionList : TOrderedList<TIdentify>): TAbstractBTreeNode;
  protected
    FCount: integer;
    FAbstractBTreeLock : TCriticalSection;
    function GetRoot: TAbstractBTreeNode; virtual; abstract;
    procedure SetRoot(var Value: TAbstractBTreeNode); virtual; abstract;

    procedure ClearNode(var ANode : TAbstractBTreeNode); virtual;
    function NewNode : TAbstractBTreeNode; virtual; abstract;
    procedure DisposeNode(var ANode : TAbstractBTreeNode); virtual; abstract;
    procedure SetNil(var AIdentify : TIdentify); virtual; abstract;
    function BinarySearch(const AData : TData; const ADataArray : TDataArray; out AIndex : Integer) : Boolean; virtual;
    function AreEquals(const AIdentify1, AIdentify2 : TIdentify) : Boolean;
    procedure SaveNode(var ANode : TAbstractBTreeNode); virtual; abstract;
    function GetCount : Integer; virtual;
    procedure SetCount(const ANewCount : Integer); virtual;
    function GetHeight: Integer; virtual;
    property Count : Integer read GetCount;
    procedure CheckConsistencyFinalized(ADatas : TList<TData>; AIdents : TOrderedList<TIdentify>; Alevels, ANodesCount, AItemsCount : Integer); virtual;
    function FindChildPos(const AIdent : TIdentify; const AParent : TAbstractBTreeNode) : Integer;
    procedure DisposeData(var AData : TData); virtual;
    function DoCompareData(const ALeftData, ARightData: TData): Integer; virtual;
    procedure DoOnFindProcessStart; virtual;
    procedure DoOnFindProcessEnd; virtual;
    function DoFind(const AData: TData; const ACircularProtectionList : TOrderedList<TIdentify>; out ANode : TAbstractBTreeNode; out iPos : Integer): Boolean; virtual;
  public
    property AllowDuplicates : Boolean read FAllowDuplicates write FAllowDuplicates;
    function IsNil(const AIdentify : TIdentify) : Boolean; virtual; abstract;
    function ToString(const ANode : TAbstractBTreeNode) : String; overload;
    procedure EraseTree;
    //
    property Root: TAbstractBTreeNode read GetRoot;
    function Find(const AData: TData; out ANode : TAbstractBTreeNode; out iPos : Integer): Boolean;
    function GetNode(AIdentify : TIdentify) : TAbstractBTreeNode; virtual; abstract;
    function FindPrecessor(const AData : TData; out APrecessor : TData) : Boolean;
    function FindSuccessor(const AData : TData; out ASuccessor : TData) : Boolean;
    function FindLowestNode: TAbstractBTreeNode;
    function FindLowest(out ALowest : TData) : Boolean;
    function FindHighestNode: TAbstractBTreeNode;
    function FindHighest(out AHighest : TData) : Boolean;
    function FindIndex(AIndex : Integer; out AData : TData) : Boolean;
    function FillList(AStartIndex, ACount : Integer; const AList : TList<TData>) : Integer;
    function Add(const AData: TData) : Boolean;
    function Delete(const AData: TData) : Boolean;
    function NodeIdentifyToString(const AIdentify : TIdentify) : String; virtual;
    function NodeDataToString(const AData : TData) : String; virtual;
    constructor Create(const AOnCompareIdentifyMethod: TComparison<TIdentify>; const AOnCompareDataMethod: TComparison<TData>; AAllowDuplicates : Boolean; AOrder: Integer);
    destructor Destroy; override;
    property OnCompareIdentifyMethod: TComparison<TIdentify> read FOnCompareIdentify;
    property OnCompareDataMethod: TComparison<TData> read FOnCompareData;
    function BTreeToString : String;
    property Order : Integer  read FOrder;
    function MaxItemsPerNode : Integer;
    function MinItemsPerNode : Integer;
    function MinChildrenPerNode : Integer;
    function MaxChildrenPerNode : Integer;
    procedure CheckConsistency; virtual;
    property Height : Integer read GetHeight;
    property CircularProtection : Boolean read FCircularProtection write FCircularProtection;
    procedure Lock;
    procedure Unlock;
    function FindExt(const AData: TData; out ADataEqualOrPrecessorFound : TData) : Boolean;
    function GetNullData : TData; virtual;
  End;

  TMemoryBTree<TData> = Class( TAbstractBTree<Integer,TData> )
  private
    FBuffer : TList<TAbstractBTree<Integer,TData>.TAbstractBTreeNode> ;
    Froot : Integer;
    FDisposed : Integer;
    FDisposedMinPos : Integer;
  protected
    function GetRoot: TAbstractBTree<Integer,TData>.TAbstractBTreeNode; override;
    procedure SetRoot(var Value: TAbstractBTree<Integer,TData>.TAbstractBTreeNode); override;
    function NewNode : TAbstractBTree<Integer,TData>.TAbstractBTreeNode; override;
    procedure DisposeNode(var ANode : TAbstractBTree<Integer,TData>.TAbstractBTreeNode); override;
    procedure SetNil(var AIdentify : Integer); override;
    procedure SaveNode(var ANode : TAbstractBTree<Integer,TData>.TAbstractBTreeNode); override;
    procedure CheckConsistencyFinalized(ADatas : TList<TData>; AIdents : TOrderedList<Integer>; Alevels, ANodesCount, AItemsCount : Integer); override;
  public
    function IsNil(const AIdentify : Integer) : Boolean; override;
    constructor Create(const AOnCompareDataMethod: TComparison<TData>; AAllowDuplicates : Boolean; AOrder : Integer);
    destructor Destroy; override;
    function GetNode(AIdentify : Integer) : TAbstractBTree<Integer,TData>.TAbstractBTreeNode; override;
    property Count;
  End;

  TNoDuplicateData<TData> = Class
  private
    FBTree : TMemoryBTree<TData>;
  public
    function Add(const AData : TData) : Boolean;
    constructor Create(const AOnCompareDataMethod: TComparison<TData>);
    destructor Destroy; override;
  End;

implementation

uses UAbstractMem;

{ TAbstractBTree<TIdentify, TData> }

function TAbstractBTree<TIdentify, TData>.Add(const AData: TData): Boolean;
var Lnode  : TAbstractBTreeNode;
  iDataPos : Integer;
  LCircularProtectionList : TOrderedList<TIdentify>;
begin
  FAbstractBTreeLock.Acquire;
  Try
    if FCircularProtection then begin
      LCircularProtectionList := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
    end else LCircularProtectionList := Nil;
    Try
      if (DoFind(AData,LCircularProtectionList,Lnode,iDataPos)) then begin
        if (Not FAllowDuplicates) then Exit(False);
        // Follow childs until leaf node
        while (Not Lnode.IsLeaf) do begin
          Lnode := GetNode(Lnode.childs[iDataPos]); // Insert at right position
          if (BinarySearch(AData,Lnode.data,iDataPos)) then begin
            //
          end;
        end;
      end else if (IsNil(Lnode.identify)) then begin
        Lnode := NewNode;
        SetRoot(Lnode);
      end;
      Assert(Lnode.IsLeaf,'Node must be a leaf');
      // Lnode is a leaf and iDataPos is position to insert
      Lnode.InsertData(Adata,iDataPos);
      SaveNode(Lnode);
      if Lnode.Count>MaxItemsPerNode then begin
        // Split and up
        if Assigned(LCircularProtectionList) then LCircularProtectionList.Clear;
        SplitAfterInsert(Lnode,LCircularProtectionList);
      end;
      Result := True;
      if (FCount>=0) then begin
        SetCount(FCount+1);
      end;
    Finally
      if Assigned(LCircularProtectionList) then
        LCircularProtectionList.Free;
    End;
  Finally
    FAbstractBTreeLock.Release;
  End;
end;

function TAbstractBTree<TIdentify, TData>.AreEquals(const AIdentify1, AIdentify2: TIdentify): Boolean;
begin
  Result := FOnCompareIdentify(AIdentify1,AIdentify2)=0;
end;

function TAbstractBTree<TIdentify, TData>.BinarySearch(const AData : TData; const ADataArray: TDataArray; out AIndex: Integer): Boolean;
  // AIndex will be a value between 0..Count and will be the position to do a Insert if needed
var i, j, mid, cmp : integer;
begin
  Result := False;
  i := 0;
  j := Length(ADataArray)-1;
  while (i <= j) do begin
    mid := (i + j) shr 1;
    cmp := DoCompareData(AData,ADataArray[mid]);
    if (cmp<0) then begin
      j := mid - 1;
    end else if (cmp>0) then begin
      i := mid + 1;
    end else begin
      AIndex := mid;
      Exit(True);
    end;
  end;
  AIndex := i;
end;

procedure TAbstractBTree<TIdentify, TData>.BTreeNodeToString(const ANode: TAbstractBTreeNode; ALevel, ALevelIndex : Integer; const AStrings: TStrings);
var i : Integer;
  s : String;
begin
  while (AStrings.Count<=ALevel) do AStrings.Add('');
  s := '';
  for i := 0 to ANode.Count-1 do begin
    if (s<>'') then s := s + ',';
    s := s + NodeDataToString(ANode.data[i]);
  end;
  if (AStrings.Strings[ALevel]<>'') then AStrings.Strings[ALevel] := AStrings.Strings[ALevel]+' ';
  AStrings.Strings[ALevel] := AStrings.Strings[ALevel] + '['+s+']';
  for i := 0 to High(ANode.childs) do begin
    BTreeNodeToString( GetNode(ANode.childs[i]), ALevel+1, ALevelIndex+i, AStrings);
  end;
end;

function TAbstractBTree<TIdentify, TData>.BTreeToString: String;
var Lsl : TStrings;
  Lnode : TAbstractBTreeNode;
begin
  Lsl := TStringList.Create;
  FAbstractBTreeLock.Acquire;
  try
    Lnode := GetRoot;
    if Not IsNil(Lnode.identify) then BTreeNodeToString(Lnode,0,0,Lsl);
    Result := Lsl.Text;
  finally
    FAbstractBTreeLock.Release;
    Lsl.Free;
  end;
end;

procedure TAbstractBTree<TIdentify, TData>.CheckConsistency;
var
  FDatas : TList<TData>;
  FIdents : TOrderedList<TIdentify>;
  Lnode : TAbstractBTreeNode;
  Llevels, LnodesCount, LItemsCount : Integer;
begin
  FIdents := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
  FDatas := TList<TData>.Create;
  FAbstractBTreeLock.Acquire;
  try
    Llevels := 0;
    LnodesCount := 0;
    LItemsCount := 0;
    Lnode := GetRoot;
    if Not IsNil(Lnode.identify) then begin
      CheckConsistencyEx(Lnode,True,-1,-1,FDatas,FIdents,1,Llevels,LnodesCount,LItemsCount);
    end;
    if (FCount>=0) then begin
      if LItemsCount<>FCount then raise EAbstractBTree.Create(Format('Inconsistent items count %d vs register %d',[LItemsCount,FCount]));
    end;
    CheckConsistencyFinalized(FDatas,FIdents,Llevels,LnodesCount,LItemsCount);
  finally
    FAbstractBTreeLock.Release;
    FDatas.Free;
    FIdents.Free;
  end;
end;

procedure TAbstractBTree<TIdentify, TData>.CheckConsistencyEx(const ANode: TAbstractBTreeNode; AIsGoingDown : Boolean; AParentDataIndexLeft, AParentDataIndexRight : Integer; ADatas: TList<TData>; AIdents: TOrderedList<TIdentify>; ACurrentLevel : Integer; var ALevels, ANodesCount, AItemsCount : Integer);
var Lchild : TAbstractBTreeNode;
  i, Lcmp, iLeft, iRight : Integer;
begin
  if (assigned(AIdents)) then begin
    if (AIdents.Add(ANode.identify)<0) then raise EAbstractBTree.Create(Format('Inconsistent Identify',[]));
  end;
  Inc(ANodesCount);
  Inc(AItemsCount,ANode.Count);
  if AIsGoingDown then begin
    inc(ALevels);
  end;
  if (ALevels < ACurrentLevel) then raise EAbstractBTree.Create(Format('Inconsistent level %d < %d',[ALevels,ACurrentLevel]));
  if (ACurrentLevel>1) then begin
    if (ANode.Count=0) then raise EAbstractBTree.Create(Format('Inconsistent NIL node at level %d',[ACurrentLevel]));
    if (AParentDataIndexLeft>=0) then begin
      // Right must be < than parent
      Lcmp := DoCompareData(ADatas.Items[AParentDataIndexLeft], ANode.data[0]);
      if Lcmp>0 then raise EAbstractBTree.Create(Format('Inconsistent %d data [%s] vs parent left [%s] at level %d',
        [Lcmp,NodeDataToString(ANode.data[0]),NodeDataToString(ADatas.Items[AParentDataIndexLeft]), ACurrentLevel]));
    end;
    if (AParentDataIndexRight>=0) then begin
      // Right must be < than parent
      Lcmp := DoCompareData(ANode.data[ANode.Count-1],ADatas.Items[AParentDataIndexRight]);
      if Lcmp>0 then raise EAbstractBTree.Create(Format('Inconsistent %d data [%s] vs parent right [%s] at level %d',
        [Lcmp,NodeDataToString(ANode.data[ANode.Count-1]),NodeDataToString(ADatas.Items[AParentDataIndexRight]), ACurrentLevel]));
    end;
  end;
  if (MinItemsPerNode>ANode.Count) or (MaxItemsPerNode<ANode.Count) then begin
    if Not (IsNil(ANode.parent)) then begin
      raise EAbstractBTree.Create(Format('Inconsistent Items in Node (%d..%d) %s at level %d for order %d',[MinItemsPerNode,MaxItemsPerNode,ToString(ANode),ACurrentLevel,FOrder]));
    end;
  end;

  for i := 1 to ANode.Count-1 do begin
    if DoCompareData(ANode.data[i-1],ANode.data[i])>0 then raise EAbstractBTree.Create(Format('Inconsistent data (%d..%d)/%d [%s] > [%s] at level %d',
      [i-1,i,ANode.Count,NodeDataToString(ANode.data[i-1]),NodeDataToString(ANode.data[i]), ACurrentLevel]));
  end;

  if ANode.IsLeaf then begin
    if (ALevels<>ACurrentLevel) then raise EAbstractBTree.Create('Inconsistency error not balanced');
    Exit;
  end;
  if (Length(ANode.childs)<>(ANode.Count+1)) then raise EAbstractBTree.Create(Format('Inconsistency error %d childs vs %d items',[Length(ANode.childs),ANode.Count]));
  if (ACurrentLevel>1) and ((MinChildrenPerNode>Length(ANode.childs)) or (MaxChildrenPerNode<Length(ANode.childs))) then begin
    raise EAbstractBTree.Create(Format('Inconsistent %d Childs in Node (%d..%d) %s at level %d',[Length(ANode.childs),MinChildrenPerNode,MaxChildrenPerNode,ToString(ANode),ACurrentLevel]));
  end;

  iLeft := -1;
  iRight := -1;
  for i := 0 to High(ANode.childs) do begin
    if (i<High(ANode.childs)) then begin
      iLeft := iRight;
      iRight := ADatas.Add(ANode.data[i]);
    end else begin
      iLeft := iRight;
      iRight := -1;
    end;
    Lchild := GetNode(ANode.childs[i]);
    if Not AreEquals(Lchild.parent,ANode.identify) then begin
        raise EAbstractBTree.Create(Format('Inconsistent Identify child %d/%d %s invalid pointer to parent %s (%s)',
          [i+1,Length(ANode.childs),ToString(Lchild),NodeIdentifyToString(ANode.identify),NodeIdentifyToString(Lchild.parent)]));
    end;
    CheckConsistencyEx(Lchild,
      ((AIsGoingDown) and (i=0)),iLeft,iRight,
      ADatas,AIdents,
      ACurrentLevel+1,
      ALevels,ANodesCount,AItemsCount);
  end;

end;

procedure TAbstractBTree<TIdentify, TData>.CheckConsistencyFinalized(ADatas: TList<TData>; AIdents: TOrderedList<TIdentify>; Alevels, ANodesCount, AItemsCount: Integer);
begin
  //
end;

procedure TAbstractBTree<TIdentify, TData>.ClearNode(var ANode: TAbstractBTreeNode);
begin
  SetLength(ANode.data,0);
  SetLength(ANode.childs,0);
  SetNil(ANode.identify);
  SetNil(ANode.parent);
end;

constructor TAbstractBTree<TIdentify, TData>.Create(const AOnCompareIdentifyMethod: TComparison<TIdentify>; const AOnCompareDataMethod: TComparison<TData>; AAllowDuplicates : Boolean; AOrder: Integer);
begin
  FAbstractBTreeLock := TCriticalSection.Create;
  FOnCompareIdentify := AOnCompareIdentifyMethod;
  FOnCompareData := AOnCompareDataMethod;
  FAllowDuplicates := AAllowDuplicates;
  FOrder := AOrder;
  if FOrder<3 then FOrder := 3 // Minimum order for a BTree is 3. Order = Max childs
  else if FOrder>255 then FOrder := 255; // Maximum order will be established to 255
  FCount := -1;                 // -1 Means there is no control
  {$IFDEF ABSTRACTMEM_CIRCULAR_SEARCH_PROTECTION}
  FCircularProtection := True;
  {$ELSE}
  FCircularProtection := False;
  {$ENDIF}
end;

function TAbstractBTree<TIdentify, TData>.Delete(const AData: TData) : Boolean;
var Lnode, Lparent, Lparentparent : TAbstractBTreeNode;
  iPos, iPosParent, iPosParentParent, j : Integer;
  LmovingUp : Boolean;
  Lleft, Lright : TAbstractBTreeNode;
  LCircularProtectionList: TOrderedList<TIdentify>;
begin
  FAbstractBTreeLock.Acquire;
  try
    if FCircularProtection then begin
      LCircularProtectionList := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
    end else LCircularProtectionList := Nil;
    try

    if Not DoFind(AData,LCircularProtectionList,Lnode,iPos) then Exit(False);

    Assert(FCount<>0,'Cannot Delete when FCount = 0');

    if (FCount>0) then begin
      SetCount(FCount-1);
    end;

    LmovingUp := False;

    if (Lnode.IsLeaf) then begin
      Lnode.DeleteData(iPos);
    end;

    if Assigned(LCircularProtectionList) then LCircularProtectionList.Clear;

  repeat
    if Assigned(LCircularProtectionList) then begin
      if LCircularProtectionList.Add(Lnode.identify)<0 then raise EAbstractBTree.Create(ClassName+'.Delete Circular T structure at Find for T='+ToString(LNode)+ ' deleting '+NodeDataToString(AData));
    end;
    if (Lnode.IsLeaf) or (LmovingUp) then begin
      if (IsNil(Lnode.parent)) and (Length(Lnode.childs)=1) then begin
        // child will be root
        Lleft := GetNode(Lnode.childs[0]);
        DisposeNode(Lnode);
        SetNil(Lleft.parent);
        SaveNode(Lleft);
        SetRoot(Lleft);
        Exit(True);
      end;

      if (IsNil(Lnode.parent)) or (Lnode.Count>=MinItemsPerNode) then begin
        // Deleting from root where root is single node
        // or Node has more than minimum datas
        SaveNode(Lnode);
        Exit(True);
      end;
      // Can borrow from left or right?
      Lparent := GetNode( Lnode.parent );
      if (Not LmovingUp) then begin
        BinarySearch(AData,Lparent.data,iPosParent);
      end;
      if (iPosParent>0) then begin
        Lleft := GetNode(Lparent.childs[iPosParent-1]);
        // Use Left?
        if Lleft.Count>MinItemsPerNode then begin

          // Move Tri From Left To Right=Lnode
          if (Not Lleft.IsLeaf) then begin
            Lright := GetNode(Lleft.childs[High(Lleft.childs)]); // Right = left sibling last child (right child)
            Lright.parent := Lnode.identify;
            SaveNode(Lright);
            //
            Lnode.InsertChild(Lright.identify,0);
            Lleft.DeleteChild(High(Lleft.childs));
          end else Assert(Lnode.IsLeaf,'node must be a leaf because left sibling is a leaf');
          Lnode.InsertData(Lparent.data[iPosParent-1],0);
          Lparent.DeleteData(iPosParent-1);
          Lparent.InsertData(Lleft.data[Lleft.Count-1],iPosParent-1);
          Lleft.DeleteData(Lleft.Count-1);

          SaveNode(Lnode);
          SaveNode(Lparent);
          SaveNode(Lleft);
          Exit(True);
        end;
      end else ClearNode(Lleft);
      if (iPosParent<Lparent.Count) then begin
        Lright := GetNode(Lparent.childs[iPosParent+1]);
        // Use right?
        if (Lright.Count>MinItemsPerNode) then begin
          // Move Tri From Right To left=Lnode
          if (Not Lright.IsLeaf) then begin
            Lleft := GetNode(Lright.childs[0]); // Left = right sibling first child (left child)
            Lleft.parent := Lnode.identify;
            SaveNode(Lleft);
            //
            Lnode.InsertChild(Lleft.identify,Length(Lnode.childs));
            Lright.DeleteChild(0);
          end else Assert(Lnode.IsLeaf,'node must be a leaf because right sibling is a leaf');
          Lnode.InsertData(Lparent.data[iPosParent],Lnode.Count);
          Lparent.DeleteData(iPosParent);
          Lparent.InsertData(Lright.data[0],iPosParent);
          Lright.DeleteData(0);

          SaveNode(Lnode);
          SaveNode(Lparent);
          SaveNode(Lright);
          Exit(True);
        end;
      end;
      // Leaf but neither left or right > MinItemsPerNode
      // Parent can remove 1 item and move others to childs?
      if (Lnode.IsLeaf)
        and
        (Lparent.Count>MinItemsPerNode)
         then begin
        // Yes. Use parent
        if (iPosParent>0) then begin
          // Use Left Sibling as destination and remove Lnode
          Lleft := GetNode(Lparent.childs[iPosParent-1]);
          Lleft.InsertData(Lparent.data[iPosParent-1],Lleft.Count);
          Lparent.DeleteData(iPosParent-1);
          Lparent.DeleteChild(iPosParent);
          MoveRangeBetweenSiblings(Lnode,Lleft);
          DisposeNode(Lnode);
          SaveNode(Lparent);
          SaveNode(Lleft);
          Exit(True);
        end else begin
          // Use right sibling (loaded before)
          Lnode.InsertData(Lparent.data[iPosParent],Lnode.Count);
          Lparent.DeleteData(0);
          Lparent.DeleteChild(1); // 1 = Lright
          SaveNode(Lparent);
          for j := 0 to Lright.Count-1 do begin
            Lnode.InsertData(Lright.data[j],Lnode.Count);
          end;
          DisposeNode(Lright);
          SaveNode(Lnode);
          Exit(True);
        end;
      end;
      // Neither siblings neither parent are > MinItemsPernode
      // in this case, go up in the tree using Parent as node
      {
                [a,c]  MinItemsPerNode=2 Order=3,4
        [a1] [b1] [c1]

      }

      if (Not IsNil(Lparent.parent)) then begin
        Lparentparent := GetNode(Lparent.parent);
        iPosParentParent := FindChildPos(Lparent.identify,Lparentparent);
      end;

      // Lnode is empty
      if (iPosParent>0) then begin
        // Deleting  [b1] or [c1]
        // Move to Left sibling and dispose Lnode

        Lleft := GetNode(Lparent.childs[iPosParent-1]);
        Lleft.InsertData(Lparent.data[iPosParent-1],Lleft.Count);

        if (not AreEquals(Lnode.identify,Lleft.identify)) then begin
          MoveRangeBetweenSiblings(Lnode,Lleft);
        end;
        if (iPosParent<=Lparent.Count) and (not AreEquals(Lnode.identify,Lparent.childs[iPosParent])) then begin
          Lright := GetNode(Lparent.childs[iPosParent]);
          MoveRangeBetweenSiblings(Lright,Lleft);
          DisposeNode(Lright);
        end;

        Lparent.DeleteData(iPosParent-1);
        Lparent.DeleteChild(iPosParent);

        if (not AreEquals(Lnode.identify,Lleft.identify)) then begin
          DisposeNode(Lnode);
        end;
        SaveNode(Lparent);
        SaveNode(Lleft);
        Lnode := Lparent;
      end else begin
        // Move from right and dispose Lright
        // Lright was loaded before
        Lnode.InsertData(Lparent.data[iPosParent],Lnode.Count);

        Lparent.DeleteData(iPosParent);
        Lparent.DeleteChild(iPosParent+1);

        MoveRangeBetweenSiblings(Lright,Lnode);

        DisposeNode(Lright);
        SaveNode(Lparent);
        SaveNode(Lnode);
        Lnode := Lparent;
      end;

      iPosParent := iPosParentParent;

    end else begin
      // Internal node
      // Lnode[iPos] has not been deleted neither updated
      //
      // Search Indorder predecessor:
      Lleft := GetNode(Lnode.childs[iPos]);
      while (Not Lleft.IsLeaf) do begin
        if Assigned(LCircularProtectionList) then begin
          if LCircularProtectionList.Add(Lleft.childs[Lleft.Count])<0 then
            raise EAbstractBTree.Create(ClassName+'.Delete Circular T structure searching for inorder precessor at '+ToString(Lleft)+' deleting '+NodeDataToString(AData));
        end;
        Lleft := GetNode(Lleft.childs[Lleft.Count]);
      end;
      if (Lleft.Count>MinItemsPerNode) then begin
        // Inorder predecessor
        Lnode.data[iPos] := Lleft.data[Lleft.Count-1];
        SaveNode(Lnode);
        Lleft.RemoveInNode(Lleft.Count-1);
        SaveNode(Lleft);
        Exit(True);
      end;
      // Search Indorder successor:
      Lright := GetNode(Lnode.childs[iPos+1]);
      while (Not Lright.IsLeaf) do begin
        if Assigned(LCircularProtectionList) then begin
          if LCircularProtectionList.Add(Lright.childs[0])<0 then
            raise EAbstractBTree.Create(ClassName+'.Delete Circular T structure searching for inorder successor at '+ToString(Lright)+' deleting '+NodeDataToString(AData));
        end;
        Lright := GetNode(Lright.childs[0]);
      end;
      if (Lright.Count>MinItemsPerNode) then begin
        // Inorder successor
        Lnode.data[iPos] := Lright.data[0];
        SaveNode(Lnode);
        Lright.RemoveInNode(0);
        SaveNode(Lright);
        Exit(True);
      end;
      // Neither predecessor neither successor
      Assert((Lleft.IsLeaf),'Left must be leaf');
      Assert((Lright.IsLeaf),'Right must be leaf');
      if (Lnode.Count>MinItemsPerNode) and (AreEquals(Lnode.identify,Lleft.parent)) then begin
        // Both childs are = MinItemsPerNode and Lnode > MinItemsPerNode . Remove from Lnode
        {
                [a,b,c]  <-  Remove "b"
        [a1,a2] [b1,b2] [c1,c2]  <- MinItemsPerNode=2

                 [a,c]
        [a1,a2,b1,b2] [c1,c2]
        }

        Lnode.DeleteData(iPos);
        Lnode.DeleteChild(iPos+1); //iPos+1 = Right sibling
        MoveRangeBetweenSiblings(Lright,Lleft);
        SaveNode(Lnode);
        SaveNode(Lleft);
        DisposeNode(Lright);
        Exit(True);
      end else begin
        {
                [a,e]  <-  Remove "a" or "e" - MinItemsPerNode=2 Order=3
        [a1,a2] [b1,b2] [f1,f2]

                [a2,e]
        [a1] [b1,b2] [f1,f2]  <- Can remove "a2" or "b2", never "f1" or "f2"
        }
        // Set predecessor
        Lnode.data[iPos] := Lleft.data[Lleft.Count-1];
        SaveNode(Lnode);

        if (Not IsNil(Lleft.parent)) then begin
          Lparent := GetNode(Lleft.parent);
          iPosParent := FindChildPos(Lleft.identify,Lparent);
        end;

        Lleft.DeleteData(Lleft.Count-1);
        SaveNode(Lleft);
        Lnode := Lleft;
      end;

    end;

    if (Not LmovingUp) then begin
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Clear;
      LmovingUp := True;
    end;
  until (False);
    finally
      if Assigned(LCircularProtectionList) then
        LCircularProtectionList.Free;
    end;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

destructor TAbstractBTree<TIdentify, TData>.Destroy;
begin
  FAbstractBTreeLock.Free;
  inherited;
end;

procedure TAbstractBTree<TIdentify, TData>.DisposeData(var AData: TData);
begin
  // Nothing to do
end;

function TAbstractBTree<TIdentify, TData>.DoCompareData(const ALeftData, ARightData: TData): Integer;
begin
  Result := FOnCompareData(ALeftData,ARightData);
end;

function TAbstractBTree<TIdentify, TData>.DoFind(const AData: TData;
  const ACircularProtectionList: TOrderedList<TIdentify>;
  out ANode: TAbstractBTreeNode; out iPos: Integer): Boolean;
begin
  DoOnFindProcessStart;
  Try
    ANode := GetRoot;
    iPos := 0;
    repeat
      if Assigned(ACircularProtectionList) then begin
        if ACircularProtectionList.Add(ANode.identify)<0 then raise EAbstractBTree.Create(ClassName+'.Find Circular T structure at Find for T='+ToString(ANode)+ ' searching for '+NodeDataToString(AData));
      end;
      if (BinarySearch(AData,ANode.data,iPos)) then Exit(True)
      else if (Not ANode.IsLeaf) then ANode := GetNode( ANode.childs[ iPos ] )
      else Exit(False);
    until False;
  Finally
    DoOnFindProcessEnd;
  End;
end;

procedure TAbstractBTree<TIdentify, TData>.DoOnFindProcessEnd;
begin
  //
end;

procedure TAbstractBTree<TIdentify, TData>.DoOnFindProcessStart;
begin
  //
end;

procedure TAbstractBTree<TIdentify, TData>.EraseTree;
var Lnode : TAbstractBTreeNode;
begin
  FAbstractBTreeLock.Acquire;
  try
    Lnode := GetRoot;
    if Not IsNil(Lnode.identify) then EraseTreeExt(Lnode);
    ClearNode(Lnode);
    if Fcount>0 then SetCount(0);
    SetRoot(Lnode);
  finally
    FAbstractBTreeLock.Release;
  end;
end;

procedure TAbstractBTree<TIdentify, TData>.EraseTreeExt(var ANode: TAbstractBTreeNode);
var i : Integer;
  Lchild : TAbstractBTreeNode;
begin
  if Not (ANode.IsLeaf) then begin
    for i:=0 to Length(ANode.childs)-1 do begin
      Lchild := GetNode(ANode.childs[i]);
      EraseTreeExt(Lchild);
    end;
  end;
  for i:=0 to Length(ANode.data)-1 do begin
    DisposeData(ANode.data[i]);
  end;
  DisposeNode(ANode);
  ClearNode(ANode);
end;

function TAbstractBTree<TIdentify, TData>.FillList(AStartIndex, ACount: Integer; const AList: TList<TData>): Integer;
var Lnode : TAbstractBTreeNode;
  iPos : Integer;
  LCircularProtectionList: TOrderedList<TIdentify>;
begin
  Assert((AStartIndex>=0) and (ACount>=0),Format('Invalid start %d or count %d',[AStartIndex,ACount]));
  Result := 0;
  FAbstractBTreeLock.Acquire;
  try
    if FCircularProtection then begin
      LCircularProtectionList := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
    end else LCircularProtectionList := Nil;
    try
      if (ACount<=0) or (AStartIndex<0) then Exit;
      if (FCount>=0) And (FCount-1 < AStartIndex) then Exit;

      Lnode := FindLowestNodeExt(LCircularProtectionList);
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Clear;
      if Lnode.Count<=0 then Exit;
      //
      Dec(AStartIndex);
      iPos := 0;
      while (AStartIndex>=0) do begin
        if Not FindSuccessorExt(LCircularProtectionList,Lnode,iPos) then Exit;
        Dec(AStartIndex);
      end;
      if Not ( (AStartIndex=-1) and (iPos < Lnode.Count) and (iPos>=0) ) then Exit;
      // Lnode.data[iPos] = Start position
      repeat
        AList.Add(Lnode.data[iPos]);
        Dec(ACount);
        inc(Result);
      until (ACount<0) or (Not FindSuccessorExt(LCircularProtectionList,Lnode,iPos));
    finally
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Free;
    end;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.Find(const AData: TData; out ANode: TAbstractBTreeNode; out iPos: Integer): Boolean;
var LCircularProtectionList: TOrderedList<TIdentify>;
begin
  FAbstractBTreeLock.Acquire;
  try
    if FCircularProtection then begin
      LCircularProtectionList := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
    end else LCircularProtectionList := Nil;
    Try
      Result := DoFind(AData,LCircularProtectionList,ANode,iPos);
    Finally
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Free;
    End;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindChildPos(const AIdent: TIdentify; const AParent: TAbstractBTreeNode): Integer;
begin
  for Result := 0 to High(AParent.childs) do begin
    if AreEquals(AIdent,AParent.childs[Result]) then Exit;
  end;
  raise EAbstractBTree.Create(Format('Child not found at %s',[ToString(AParent)]));
end;

function TAbstractBTree<TIdentify, TData>.FindExt(const AData: TData; out ADataEqualOrPrecessorFound: TData): Boolean;
var Lnode : TAbstractBTreeNode;
  LiPosNode : Integer;
  LCircularProtectionList : TOrderedList<TIdentify>;
  LPrecessorFound : Boolean;
begin
  FAbstractBTreeLock.Acquire;
  try
    ClearNode(Lnode);
    if Find(AData,Lnode,LiPosNode) then begin
      ADataEqualOrPrecessorFound := Lnode.data[LiPosNode];
      Result := True;
    end else begin
      // At this point Lnode is a leaf OR a NIL (no root available at tree)
      // Lnode.Count = 0  -> NIL (no root/tree available)
      if Lnode.Count=0 then begin
        ADataEqualOrPrecessorFound := GetNullData;
      end else if Lnode.Count=LiPosNode then begin
        dec(LiPosNode);
        ADataEqualOrPrecessorFound := Lnode.data[LiPosNode];
      end else begin
        // Will find previous valid value by climbing tree
        LCircularProtectionList := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
        try
          LCircularProtectionList.Clear;
          LPrecessorFound := FindPrecessorExt(LCircularProtectionList,Lnode,LiPosNode);
          if LPrecessorFound then ADataEqualOrPrecessorFound := Lnode.data[LiPosNode]
          else ADataEqualOrPrecessorFound := GetNullData;
        finally
          LCircularProtectionList.Free;
        end;
      end;
      Result := False;
    end;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindHighest(out AHighest : TData) : Boolean;
var Lnode : TAbstractBTreeNode;
begin
  FAbstractBTreeLock.Acquire;
  try
    Lnode := FindHighestNode;
    if Lnode.Count>0 then begin
       AHighest := Lnode.data[Lnode.Count-1];
       Result := True;
    end else Result := False;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindHighestNode: TAbstractBTreeNode;
begin
  Result := FindHighestNodeExt(Nil);
end;

function TAbstractBTree<TIdentify, TData>.FindHighestNodeExt(
  const ACircularProtectionList: TOrderedList<TIdentify>): TAbstractBTreeNode;
begin
  FAbstractBTreeLock.Acquire;
  try
    Result := GetRoot;
    while (Not Result.IsLeaf) do begin
      if Assigned(ACircularProtectionList) then begin
        if ACircularProtectionList.Add(Result.childs[Result.Count])<0 then
          raise EAbstractBTree.Create(ClassName+'.FindHighestNode Circular T structure for T='+ToString(Result));
      end;
      Result := GetNode(Result.childs[Result.Count]);
    end;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindIndex(AIndex: Integer; out AData: TData): Boolean;
var Lnode : TAbstractBTreeNode;
  iPos : Integer;
  LCircularProtectionList: TOrderedList<TIdentify>;
begin
  FAbstractBTreeLock.Acquire;
  try
    if FCircularProtection then begin
      LCircularProtectionList := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
    end else LCircularProtectionList := Nil;
    try
      Lnode := FindLowestNodeExt(LCircularProtectionList);
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Clear;
      if Lnode.Count<=0 then Exit(False);
      //
      Dec(AIndex);
      iPos := 0;
      while (AIndex>=0) do begin
        if Not FindSuccessorExt(LCircularProtectionList,Lnode,iPos) then Exit(False);
        Dec(AIndex);
      end;
      if (AIndex=-1) and (iPos < Lnode.Count) and (iPos>=0) then begin
        Result := True;
        AData := Lnode.data[iPos];
      end else Result := False;
    finally
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Free;
    end;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindLowest(out ALowest : TData) : Boolean;
var Lnode : TAbstractBTreeNode;
begin
  FAbstractBTreeLock.Acquire;
  try
    Lnode := FindLowestNode;
    if Lnode.Count>0 then begin
      ALowest := Lnode.data[0];
      Result := True;
    end else Result := False;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindLowestNode: TAbstractBTreeNode;
begin
  Result := FindLowestNodeExt(Nil);
end;

function TAbstractBTree<TIdentify, TData>.FindLowestNodeExt(
  const ACircularProtectionList: TOrderedList<TIdentify>): TAbstractBTreeNode;
begin
  FAbstractBTreeLock.Acquire;
  try
    Result := GetRoot;
    while (Not Result.IsLeaf) do begin
      if Assigned(ACircularProtectionList) then begin
        if ACircularProtectionList.Add(Result.childs[0])<0 then
          raise EAbstractBTree.Create(ClassName+'.FindLowestNode Circular T structure for T='+ToString(Result));
      end;
      Result := GetNode(Result.childs[0]);
    end;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindPrecessor(const AData : TData; out APrecessor : TData) : Boolean;
var Lnode : TAbstractBTreeNode;
  iPos : Integer;
  LCircularProtectionList: TOrderedList<TIdentify>;
begin
  FAbstractBTreeLock.Acquire;
  try
    if FCircularProtection then begin
      LCircularProtectionList := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
    end else LCircularProtectionList := Nil;
    Try
      Result := False;
      if Not DoFind(AData,LCircularProtectionList,Lnode,iPos) then Exit(False);
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Clear;
      repeat
        Result := FindPrecessorExt(LCircularProtectionList,Lnode,iPos);
        if Result then begin
          APrecessor := Lnode.data[iPos];
        end;
      until (Not Result) or (Not FAllowDuplicates) or (DoCompareData(AData,APrecessor)>0);
    Finally
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Free;
    End;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindPrecessorExt(const ACircularProtectionList : TOrderedList<TIdentify>; var ANode: TAbstractBTreeNode; var iPos: Integer): Boolean;
var Lparent : TAbstractBTreeNode;
  Lsecondary : TOrderedList<TIdentify>;
begin
  Result := False;
  if (Not ANode.IsLeaf) then begin
    ANode := GetNode(ANode.childs[iPos]);
    while (Not ANode.IsLeaf) do begin
      if Assigned(ACircularProtectionList) then begin
        if ACircularProtectionList.Add(ANode.childs[ANode.Count])<0 then
          raise EAbstractBTree.Create(ClassName+'.FindPrecessor Circular T structure at Find for T='+ToString(ANode));
      end;
      ANode := GetNode(ANode.childs[ANode.Count]);
    end;
    iPos := ANode.Count-1;
    Exit(True);
  end else begin
    if iPos>0 then begin
      Dec(iPos);
      Exit(True);
    end else if (Not IsNil(ANode.parent)) then begin
      // Left sibling
      Lparent := GetNode(ANode.parent);
      iPos := FindChildPos(ANode.identify,Lparent);
      if iPos>0 then begin
        Dec(iPos);
        ANode := Lparent;
        Exit(True);
      end else begin
        // Search parents until parent iPos>0
        if Assigned(ACircularProtectionList) then begin
          Lsecondary := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
        end else Lsecondary := Nil;
        try
          while (iPos=0) and (Not IsNil(Lparent.parent)) do begin
            ANode := Lparent;
            if Assigned(Lsecondary) then begin
              if Lsecondary.Add(ANode.parent)<0 then
                raise EAbstractBTree.Create(ClassName+'.FindPrecessor Circular T structure at Find for parent of T='+ToString(ANode));
            end;
            Lparent := GetNode(ANode.parent);
            iPos := FindChildPos(ANode.identify,Lparent);
          end;
        finally
          if Assigned(Lsecondary) then Lsecondary.Free;
        end;
        if iPos>0 then begin
          Dec(iPos);
          ANode := Lparent;
          Exit(True);
        end;
      end;
    end;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindSuccessor(const AData : TData; out ASuccessor : TData) : Boolean;
var Lnode : TAbstractBTreeNode;
  iPos : Integer;
  LCircularProtectionList: TOrderedList<TIdentify>;
begin
  FAbstractBTreeLock.Acquire;
  try
    if FCircularProtection then begin
      LCircularProtectionList := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
    end else LCircularProtectionList := Nil;
    Try
      Result := False;
      if Not DoFind(AData,LCircularProtectionList,Lnode,iPos) then Exit(False);
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Clear;
      repeat
        Result := FindSuccessorExt(LCircularProtectionList,Lnode,iPos);
        if Result then begin
          ASuccessor := Lnode.data[iPos];
        end;
      until (Not Result) or (Not FAllowDuplicates) or (DoCompareData(AData,ASuccessor)<0);
    Finally
      if Assigned(LCircularProtectionList) then LCircularProtectionList.Free;
    End;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.FindSuccessorExt(const ACircularProtectionList : TOrderedList<TIdentify>; var ANode: TAbstractBTreeNode; var iPos: Integer): Boolean;
var Lparent : TAbstractBTreeNode;
  Lsecondary : TOrderedList<TIdentify>;
begin
  Result := False;
  if (Not ANode.IsLeaf) then begin
    ANode := GetNode(ANode.childs[iPos+1]);
    iPos := 0;
    while (Not ANode.IsLeaf) do begin
      if Assigned(ACircularProtectionList) then begin
        if ACircularProtectionList.Add(ANode.childs[0])<0 then
          raise EAbstractBTree.Create(ClassName+'.FindSuccessor Circular T structure at Find for T='+ToString(ANode));
      end;
      ANode := GetNode(ANode.childs[0]);
    end;
    Exit(True);
  end else begin
    if iPos+1<ANode.Count then begin
      inc(iPos);
      Exit(True);
    end else if (Not IsNil(ANode.parent)) then begin
      // right sibling
      Lparent := GetNode(ANode.parent);
      iPos := FindChildPos(ANode.identify,Lparent);
      if iPos<Lparent.Count then begin
        ANode := Lparent;
        Exit(True);
      end else begin
        // Search parents until parent iPos>0
        if Assigned(ACircularProtectionList) then begin
          Lsecondary := TOrderedList<TIdentify>.Create(False,FOnCompareIdentify);
        end else Lsecondary := Nil;
        try
          while (iPos=Lparent.Count) and (Not IsNil(Lparent.parent)) do begin
            ANode := Lparent;
            if Assigned(Lsecondary) then begin
              if Lsecondary.Add(ANode.parent)<0 then
                raise EAbstractBTree.Create(ClassName+'.FindSuccessor Circular T structure at Find for parent of T='+ToString(ANode));
            end;
            Lparent := GetNode(ANode.parent);
            iPos := FindChildPos(ANode.identify,Lparent);
          end;
        finally
          if Assigned(Lsecondary) then Lsecondary.Free;
        end;
        if iPos<Lparent.Count then begin
          ANode := Lparent;
          Exit(True);
        end;
      end;
    end;
  end;
end;

function TAbstractBTree<TIdentify, TData>.GetCount: Integer;
begin
  Result := FCount;
end;

function TAbstractBTree<TIdentify, TData>.GetHeight: Integer;
var Lnode : TAbstractBTreeNode;
begin
  FAbstractBTreeLock.Acquire;
  try
  Lnode := GetRoot;
  if (Lnode.Count=0) or (IsNil(Lnode.identify)) then Exit(0);
  Result := 1;
  while (Not Lnode.IsLeaf) do begin
    Lnode := GetNode(Lnode.childs[0]);
    inc(Result);
  end;
  finally
    FAbstractBTreeLock.Release;
  end;
end;

function TAbstractBTree<TIdentify, TData>.GetNullData: TData;
begin
  raise EAbstractBTree.Create('function '+Self.ClassName+'.GetNullData: TData; Not overrided');
end;

procedure TAbstractBTree<TIdentify, TData>.Lock;
begin
  FAbstractBTreeLock.Acquire;
end;

function TAbstractBTree<TIdentify, TData>.MaxChildrenPerNode: Integer;
begin
  Result := FOrder;
end;

function TAbstractBTree<TIdentify, TData>.MaxItemsPerNode: Integer;
begin
  Result := FOrder-1;
end;

function TAbstractBTree<TIdentify, TData>.MinChildrenPerNode: Integer;
begin
  // Order 3 -> 1-2 items 2-3 childrens
  // Order 4 -> 1-3 items 2-4 childrens
  // Order 5 -> 2-4 items 3-5 childrens
  // Order 6 -> 2-5 items 3-6 childrens
  // Order 7 -> 3-6 items 4-7 childrens
  // ...
  Result := ((FOrder+1) DIV 2);
end;

function TAbstractBTree<TIdentify, TData>.MinItemsPerNode: Integer;
begin
  Result := ((FOrder+1) DIV 2)-1;
end;

procedure TAbstractBTree<TIdentify, TData>.MoveRange(var ASourceNode, ADestNode: TAbstractBTreeNode; AFromSource, ACount, AToDest: Integer);
var i : Integer;
  Lchild : TAbstractBTreeNode;
begin
  // Will NOT save nodes because are passed as a variable, BUT will save child nodes!
  if (ACount<=0) then Exit; // Nothing to move...

  Assert(ACount>0,'Invalid move range count');
  Assert((AFromSource>=0) and (AFromSource<Length(ASourceNode.data)),'Invalid move range from source');
  Assert((AToDest>=0) and (AToDest<=Length(ADestNode.data)),'Invalid move range to dest');
  // MoveRange is only available to move LEFT or RIGHT of ASourceNode, never MIDDLE positions
  Assert((AFromSource=0) or ((AFromSource+ACount)=ASourceNode.Count),'Invalid MIDDLE positions of node');
  Assert(((AFromSource=0) and (AToDest=ADestNode.Count)) or
         ((AtoDest=0) and (AFromSource+ACount=ASourceNode.Count))
           ,Format('Invalid middle MoveRange from %d count %d to %d  source.count=%d dest.count=%d',[AFromSource,ACount,AToDest,ASourceNode.Count,ADestNode.Count]));

  for i := 0 to ACount-1 do begin
    ADestNode.InsertData(ASourceNode.data[AFromSource + i],AToDest+i);
    if Not ASourceNode.IsLeaf then begin
      Lchild := GetNode( ASourceNode.childs[AFromSource + i] );
      Lchild.parent := ADestNode.identify;
      SaveNode(Lchild);
      ADestNode.InsertChild( ASourceNode.childs[AFromSource + i], AToDest + i);
    end;
  end;
  if Not ASourceNode.IsLeaf then begin
    Lchild := GetNode( ASourceNode.childs[(AFromSource + ACount)] );
    Lchild.parent := ADestNode.identify;
    SaveNode(Lchild);
    ADestNode.InsertChild( ASourceNode.childs[AFromSource + ACount], AToDest + ACount );
  end;

  for i := 0 to ACount-1 do begin
    ASourceNode.DeleteData(AFromSource + i);
    if Not ASourceNode.IsLeaf then begin
      ASourceNode.DeleteChild(AFromSource + i);
    end;
  end;
  if Not ASourceNode.IsLeaf then begin
    ASourceNode.DeleteChild(AFromSource + ACount);
  end;

end;

procedure TAbstractBTree<TIdentify, TData>.MoveRangeBetweenSiblings(var ASourceNode, ADestNode: TAbstractBTreeNode);
var i, LdestStart : Integer;
  Lchild : TAbstractBTreeNode;
begin
  LdestStart := Length(ADestNode.data);
  SetLength(ADestNode.data,Length(ADestNode.data)+Length(ASourceNode.data));
  for i := 0 to Length(ASourceNode.data)-1 do begin
    ADestNode.data[LdestStart + i] := ASourceNode.data[i];
  end;

  LdestStart := Length(ADestNode.childs);
  SetLength(ADestNode.childs,Length(ADestNode.childs)+Length(ASourceNode.childs));
  for i := 0 to Length(ASourceNode.childs)-1 do begin
    ADestNode.childs[LdestStart + i] := ASourceNode.childs[i];
    Lchild := GetNode( ASourceNode.childs[i] );
    Lchild.parent := ADestNode.identify;
    SaveNode(Lchild);
  end;
end;

function TAbstractBTree<TIdentify, TData>.NodeDataToString(const AData: TData): String;
begin
  Result := IntToStr(SizeOf(AData));
end;

function TAbstractBTree<TIdentify, TData>.NodeIdentifyToString(
  const AIdentify: TIdentify): String;
begin
  Result := IntToStr(SizeOf(AIdentify));
end;

procedure TAbstractBTree<TIdentify, TData>.SetCount(const ANewCount: Integer);
begin
  FCount := ANewCount;
end;

procedure TAbstractBTree<TIdentify, TData>.SplitAfterInsert(var ANode: TAbstractBTreeNode; const ACircularProtectionList : TOrderedList<TIdentify>);
var iDataInsertPos : Integer;
  LnewNode, Lup : TAbstractBTreeNode;
begin
  Assert(ANode.Count>MaxItemsPerNode);
  LnewNode := NewNode;
  MoveRange(ANode,LnewNode,MinItemsPerNode+1,ANode.Count - (MinItemsPerNode+1),0);
  // Put ANode[MinItemsPerNode+1] up
  if IsNil(ANode.parent) then begin
    // Lup will be a new root
    Lup := NewNode;
  end else begin
    if Assigned(ACircularProtectionList) then begin
      if ACircularProtectionList.Add(ANode.parent)<0 then raise EAbstractBTree.Create(ClassName+'.SplitAfterInsert Circular T structure at Find for parent of T='+ToString(ANode));
    end;
    Lup := GetNode(ANode.parent);
  end;
  if Lup.Count=0 then begin
    Lup.InsertData(ANode.data[MinItemsPerNode], 0 );
    // Insert both childs because is a new root
    Lup.InsertChild(ANode.identify,0);
    SaveNode(LnewNode); // We need a valid identify value
    Lup.InsertChild(LnewNode.identify,1);
    SaveNode(Lup);
    SetRoot(Lup);
  end else begin
    iDataInsertPos := FindChildPos(ANode.identify,Lup);
    Lup.InsertData(ANode.data[MinItemsPerNode], iDataInsertPos );
    SaveNode(LnewNode); // We need a valid identify value
    Lup.InsertChild(LnewNode.identify, iDataInsertPos +1 );
    SaveNode(Lup);
  end;
  LnewNode.parent := Lup.identify;
  SaveNode(LnewNode);
  ANode.parent := Lup.identify;
  // Remove data&child
  ANode.DeleteData(MinItemsPerNode);
  SaveNode(ANode);
  if Lup.Count>MaxItemsPerNode then SplitAfterInsert(Lup,ACircularProtectionList);
end;

function TAbstractBTree<TIdentify, TData>.ToString(const ANode: TAbstractBTreeNode): String;
var i : Integer;
begin
  Result := '';
  for i := 0 to ANode.Count-1 do begin
    if Result<>'' then Result := Result + ',';
    Result := Result + NodeDataToString(ANode.data[i]);
  end;
  Result := NodeIdentifyToString(ANode.identify)+'@'+NodeIdentifyToString(ANode.parent)+'['+Result+']';
end;

procedure TAbstractBTree<TIdentify, TData>.Unlock;
begin
  FAbstractBTreeLock.Release;
end;

{ TAbstractBTree<TIdentify, TData>.TAbstractBTreeNode }

function TAbstractBTree<TIdentify, TData>.TAbstractBTreeNode.Count: Integer;
begin
  Result := Length(Self.data);
end;

procedure TAbstractBTree<TIdentify, TData>.TAbstractBTreeNode.DeleteChild(AChildIndex: Integer);
var i : Integer;
begin
  for i := AChildIndex to (High(Self.childs)-1) do begin
    Self.childs[i] := Self.childs[i+1];
  end;
  SetLength(Self.childs,Length(Self.childs)-1);
end;

procedure TAbstractBTree<TIdentify, TData>.TAbstractBTreeNode.DeleteData(AIndex: Integer);
var i : Integer;
begin
  for i := AIndex to (High(Self.data)-1) do begin
    Self.data[i] := Self.data[i+1];
  end;
  SetLength(Self.data,Length(Self.data)-1);
end;

procedure TAbstractBTree<TIdentify, TData>.TAbstractBTreeNode.InsertChild(const AChild: TIdentify; AIndex: Integer);
var i : Integer;
begin
  if (AIndex<0) or (AIndex>Length(Self.childs)) then raise EAbstractBTree.Create('Error 20201215-3');
  SetLength(Self.childs,Length(Self.childs)+1);
  for i := Length(Self.childs)-1 downto AIndex+1 do begin
    Self.childs[i] := Self.childs[i-1];
  end;
  Self.childs[AIndex] := AChild;
end;

procedure TAbstractBTree<TIdentify, TData>.TAbstractBTreeNode.InsertData(const AData: TData; AIndex: Integer);
var i : Integer;
begin
  if (AIndex<0) or (AIndex>Length(Self.data)) then raise EAbstractBTree.Create('Error 20201215-4');
  SetLength(Self.data,Length(Self.data)+1);
  for i := Length(Self.data)-1 downto AIndex+1 do begin
    Self.data[i] := Self.data[i-1];
  end;
  Self.data[AIndex] := AData;
end;

function TAbstractBTree<TIdentify, TData>.TAbstractBTreeNode.IsLeaf: Boolean;
begin
  Result := Length(Self.childs)=0;
end;

procedure TAbstractBTree<TIdentify, TData>.TAbstractBTreeNode.RemoveInNode(AIndex: Integer);
var i : Integer;
begin
  {
  Can only remove LEFT or RIGHT. Not Middle positions
  }
  if (AIndex<0) or (AIndex>=Length(Self.data)) then raise EAbstractBTree.Create('Error 20201215-5');
  Assert((AIndex=0) or (AIndex=High(Self.data)),'Must remove first or last position');
  for i := AIndex to (High(Self.data)-1) do begin
    Self.data[i] := Self.data[i+1];
  end;
  SetLength(Self.data,Length(Self.data)-1);
  if (Not Self.IsLeaf) then begin
    if (AIndex>=Length(Self.childs)) then raise EAbstractBTree.Create('Error 20201215-6');
    if (Aindex=0) and (Length(Self.childs)>2)  then begin
      for i := AIndex+1 to (High(Self.childs)) do begin
        Self.childs[i-1] := Self.childs[i];
      end;
    end;
    SetLength(Self.childs,Length(Self.childs)-1);
  end;
end;

{ TMemoryBTree<TData> }

procedure TMemoryBTree<TData>.CheckConsistencyFinalized(ADatas: TList<TData>; AIdents: TOrderedList<Integer>; Alevels, ANodesCount, AItemsCount: Integer);
var i,iPos,nDisposed, LDisposedMinPos : Integer;
begin
  inherited;
  nDisposed := 0;
  LDisposedMinPos := -1;
  for i := 0 to FBuffer.Count-1 do begin
    if (FBuffer.Items[i].identify=i) then begin
      if Assigned(AIdents) then begin
        if not AIdents.Find(i,iPos) then begin
          raise EAbstractBTree.Create(Format('CheckConsistency ident %d not found (%d idents)',[i,FBuffer.Count]));
        end;
      end;
    end else begin
      inc(nDisposed);
      if (LDisposedMinPos<0) then LDisposedMinPos := i;
    end;
  end;
  if FDisposed<>nDisposed then raise EAbstractBTree.Create(Format('CheckConsistency Disposed %d <> %d',[FDisposed,nDisposed]));
  if FDisposedMinPos>LDisposedMinPos then raise EAbstractBTree.Create(Format('CheckConsistency DisposedMinPos %d > %d',[FDisposedMinPos,LDisposedMinPos]));
end;

constructor TMemoryBTree<TData>.Create(const AOnCompareDataMethod: TComparison<TData>; AAllowDuplicates : Boolean; AOrder : Integer);
begin
  FBuffer := TList<TAbstractBTreeNode>.Create;
  Froot := -1;
  inherited Create(TComparison_Integer,AOnCompareDataMethod,AAllowDuplicates,AOrder);
  FCount := 0;
  FDisposed := 0;
  FDisposedMinPos := -1;
end;

destructor TMemoryBTree<TData>.Destroy;
begin
  EraseTree;
  FreeAndNil(FBuffer);
  inherited;
end;

procedure TMemoryBTree<TData>.DisposeNode(var ANode: TAbstractBTree<Integer, TData>.TAbstractBTreeNode);
var Lpos : Integer;
begin
  Lpos := ANode.identify;
  Assert((Lpos>=0) and (Lpos<FBuffer.Count),Format('Dispose %d out of range [0..%d]',[Lpos,FBuffer.Count-1]));
  ClearNode(ANode);
  FBuffer[Lpos] := ANode;
  inc(FDisposed);
  if (FDisposedMinPos<0) or (FDisposedMinPos>Lpos) then FDisposedMinPos := Lpos;
end;

function TMemoryBTree<TData>.GetNode(AIdentify: Integer): TAbstractBTree<Integer, TData>.TAbstractBTreeNode;
begin
  Result := FBuffer[AIdentify];
  if (Result.identify<>AIdentify) then raise EAbstractBTree.Create(Format('Found %d Identify instead of %d',[Result.identify,AIdentify]));
end;

function TMemoryBTree<TData>.GetRoot: TAbstractBTree<Integer, TData>.TAbstractBTreeNode;
begin
  if (Froot<0) then begin
    ClearNode(Result);
    Exit;
  end;
  Result := GetNode(Froot);
end;

function TMemoryBTree<TData>.IsNil(const AIdentify: Integer): Boolean;
begin
  Result := AIdentify<0;
end;

function TMemoryBTree<TData>.NewNode: TAbstractBTree<Integer, TData>.TAbstractBTreeNode;
begin
  ClearNode(Result);
  if (FDisposed > 0) And (FDisposed > (Count DIV 5)) then begin // 20% max disposed nodes
    // Reuse disposed node:
    if (FDisposedMinPos<0) then FDisposedMinPos := 0;
    while (FDisposedMinPos<FBuffer.Count) and (FBuffer.Items[FDisposedMinPos].identify = FDisposedMinPos) do inc(FDisposedMinPos);
    if (FDisposedMinPos>=0) and (FDisposedMinPos<FBuffer.Count) then begin
      Assert(FBuffer.Items[FDisposedMinPos].identify<0);
      Result.identify := FDisposedMinPos;
      inc(FDisposedMinPos);
      Dec(FDisposed);
      FBuffer.Items[Result.identify] := Result;
      Exit;
    end else raise EAbstractBTree.Create('Cannot reuse NewNode');
  end;
  Result.identify := FBuffer.Count;
  FBuffer.Insert(Result.identify,Result);
end;

procedure TMemoryBTree<TData>.SaveNode(var ANode: TAbstractBTree<Integer, TData>.TAbstractBTreeNode);
begin
  if (ANode.identify<0) then begin
    raise EAbstractBTree.Create('Save undefined node '+ToString(ANode));
    // New
    ANode.identify := FBuffer.Count;
    FBuffer.Insert(ANode.identify,ANode);
  end else begin
    FBuffer[ANode.identify] := ANode;
  end;
end;

procedure TMemoryBTree<TData>.SetNil(var AIdentify: Integer);
begin
  AIdentify := -1;
end;

procedure TMemoryBTree<TData>.SetRoot(var Value: TAbstractBTree<Integer, TData>.TAbstractBTreeNode);
begin
  Froot := Value.identify;
end;

{ TNoDuplicateData<TData> }

function TNoDuplicateData<TData>.Add(const AData: TData): Boolean;
begin
  Result := FBTree.Add(AData);
end;

constructor TNoDuplicateData<TData>.Create(const AOnCompareDataMethod: TComparison<TData>);
begin
  FBTree := TMemoryBTree<TData>.Create(AOnCompareDataMethod,False,7);
  FBTree.FCircularProtection := False;
end;

destructor TNoDuplicateData<TData>.Destroy;
begin
  FreeAndNil(FBTree);
  inherited;
end;

initialization

finalization

end.
