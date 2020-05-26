unit UAbstractBTree;

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

  SPECIAL CONTRIBUTOR:
  This unit contains TAVLAbstractTree component that
  is created based on work previously made
  by Mattias Gaertner at unit AVL_Tree for Free Component Library (FCL)
  and Lazarus: lazarus\components\lazutils\laz_avl_tree.pp
  Code object has been fully redo but algo is based on it... and on
  initial algo of AVL Tree created by Adelson-Velsky and Landis

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
  UOrderedList;

{$I ./ConfigAbstractMem.inc }

type
  TAVLTreePosition = (poParent, poLeft, poRight);

  EAVLAbstractTree = Class(Exception);

  { TAVLAbstractTree }

  TAVLAbstractTree<T> = class
  private
    FOnCompare: TComparison<T>;
    FDisabledsCount : Integer;
    FAllowDuplicates: Boolean;
    procedure BalanceAfterInsert(ANode: T);
    procedure BalanceAfterDelete(ANode: T);
    procedure CheckNode(const ANode: T); overload;
    function CheckNode(const ANode: T; ACheckedList:TOrderedList<T>; var ALeftDepth, ARightDepth : Integer; const AErrors : TStrings): integer; overload;
    procedure RotateLeft(var ANode: T);
    procedure RotateRight(var ANode: T);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SwitchPositionWithSuccessor(aNode, aSuccessor: T);
  protected
    FCount: integer;
    function GetRoot: T; virtual; abstract;
    procedure SetRoot(const Value: T); virtual; abstract;
    function HasPosition(const ANode : T; APosition : TAVLTreePosition) : Boolean; virtual; abstract;
    function GetPosition(const ANode : T; APosition : TAVLTreePosition) : T; virtual; abstract;
    procedure SetPosition(var ANode : T; APosition : TAVLTreePosition; const ANewValue : T); virtual; abstract;
    procedure ClearPosition(var ANode : T; APosition : TAVLTreePosition); virtual; abstract;
    function GetBalance(const ANode : T) : Integer; virtual; abstract;
    procedure SetBalance(var ANode : T; ANewBalance : Integer); virtual; abstract;
    function AreEquals(const ANode1, ANode2 : T) : Boolean; virtual; abstract;
    procedure ClearNode(var ANode : T); virtual; abstract;
    procedure DisposeNode(var ANode : T); virtual; abstract;
    //
    procedure UpdateFinished; virtual;
  public
    property AllowDuplicates : Boolean read FAllowDuplicates write FAllowDuplicates;
    property DisabledsCount:Integer read FDisabledsCount;
    function IsNil(const ANode : T) : Boolean; virtual; abstract;
    //
    property Root: T read GetRoot;
    function FindInsertPos(const AData: T): T;
    function Find(const AData: T): T;
    function FindSuccessor(const ANode: T): T;
    function FindPrecessor(const ANode: T): T;
    function FindLowest: T;
    function FindHighest: T;
    function Add(var ANode: T) : Boolean;
    procedure Delete(var ANode: T);
    constructor Create(const OnCompareMethod: TComparison<T>; AAllowDuplicates : Boolean); virtual;
    function ConsistencyCheck(const AErrors : TStrings): integer; virtual;
    function ToString(const ANode:T) : String; reintroduce; overload; virtual;
    function ToString : String; reintroduce; overload;
    property OnCompareMethod: TComparison<T> read FOnCompare;
  end;

  //

  PAVLPointerTreeNode = ^TAVLPointerTreeNode;
  TAVLPointerTreeNode = Record
    parent : PAVLPointerTreeNode;
    left : PAVLPointerTreeNode;
    right : PAVLPointerTreeNode;
    balance : Integer;
    data : Pointer;
  End;

  TPAVLPointerTree = Class( TAVLAbstractTree<PAVLPointerTreeNode> )
  private
    FRoot : PAVLPointerTreeNode;
  protected
    function GetRoot: PAVLPointerTreeNode; override;
    procedure SetRoot(const Value: PAVLPointerTreeNode); override;
    function HasPosition(const ANode : PAVLPointerTreeNode; APosition : TAVLTreePosition) : Boolean; override;
    procedure SetPosition(var ANode : PAVLPointerTreeNode; APosition : TAVLTreePosition; const ANewValue : PAVLPointerTreeNode); override;
    procedure ClearPosition(var ANode : PAVLPointerTreeNode; APosition : TAVLTreePosition); override;
    function GetBalance(const ANode : PAVLPointerTreeNode) : Integer; override;
    procedure SetBalance(var ANode : PAVLPointerTreeNode; ANewBalance : Integer); override;
    function AreEquals(const ANode1, ANode2 : PAVLPointerTreeNode) : Boolean; override;
    procedure ClearNode(var ANode : PAVLPointerTreeNode); override;
    procedure DisposeNode(var ANode : PAVLPointerTreeNode); override;
  public
    function IsNil(const ANode : PAVLPointerTreeNode) : Boolean; override;
    function ToString(const ANode: PAVLPointerTreeNode) : String; override;
    constructor Create(const OnCompareMethod: TComparison<PAVLPointerTreeNode>; AAllowDuplicates : Boolean); override;
    //
    function GetPosition(const ANode : PAVLPointerTreeNode; APosition : TAVLTreePosition) : PAVLPointerTreeNode; override;
  End;


const
  CT_TAVLPointerTreeNode_NULL : TAVLPointerTreeNode = (parent:Nil;left:Nil;right:Nil;balance:0;data:Nil);

implementation

{ TAVLAbstractTree }

function TAVLAbstractTree<T>.Add(var ANode : T) : Boolean;
var LInsertPos: T;
  LInsertComp: integer;
begin
  BeginUpdate;
  Try
    // Init T
    ClearPosition(ANode,poLeft);
    ClearPosition(ANode,poRight);
    SetBalance(ANode,0); // Init Balance to 0
    if Not IsNil(Root) then begin
      LInsertPos:=FindInsertPos(ANode);
      LInsertComp:=fOnCompare(ANode,LInsertPos);
      SetPosition(ANode,poParent,LInsertPos);
      if LInsertComp<0 then begin
        // insert to the left
        SetPosition(LInsertPos,poLeft,ANode);
      end else if (AllowDuplicates) Or (LInsertComp>0) then begin
        // insert to the right
        SetPosition(LInsertPos,poRight,ANode);
      end else begin
        Exit(False);
      end;
      BalanceAfterInsert(ANode);
    end else begin
      SetRoot( ANode );
      ClearPosition(ANode,poParent);
    end;
    inc(FCount);
    Result := True;
  Finally
    EndUpdate;
  End;
end;

function TAVLAbstractTree<T>.FindLowest: T;
begin
  Result:=Root;
  if Not IsNil(Result) then
    while HasPosition(Result,poLeft) do Result := GetPosition(Result,poLeft);
end;

function TAVLAbstractTree<T>.FindHighest: T;
begin
  Result:=Root;
  if Not IsNil(Result) then
    while HasPosition(Result,poRight) do Result := GetPosition(Result,poRight);
end;

procedure TAVLAbstractTree<T>.BalanceAfterDelete(ANode: T);
var
  OldParent, OldRight, OldRightLeft, OldLeft, OldLeftRight: T;
begin
  while Not IsNil(ANode) do begin
    if ((GetBalance(ANode)=+1) or (GetBalance(ANode)=-1)) then exit;
    OldParent:=GetPosition(ANode,poParent);
    if (GetBalance(ANode)=0) then begin
      // Treeheight has decreased by one
      if IsNil(OldParent) then
        exit;
      if (AreEquals(GetPosition(OldParent,poLeft),ANode)) then
        SetBalance(OldParent,GetBalance(OldParent)+1)
      else
      SetBalance(OldParent,GetBalance(OldParent)-1);
      ANode:=OldParent;
    end else if (GetBalance(ANode)=+2) then begin
      // Node is overweighted to the right
      OldRight:=GetPosition(ANode,poRight);
      if (GetBalance(OldRight)>=0) then begin
        // OldRight.Balance is 0 or +1
        // rotate ANode,OldRight left
        RotateLeft(ANode);
        SetBalance(ANode,(1-GetBalance(OldRight))); // toggle 0 and 1
        SetBalance(OldRight,GetBalance(OldRight)-1);
        ANode:=OldRight;
      end else begin
        // OldRight.Balance=-1
        { double rotate
          = rotate OldRightLeft,OldRight right
            and then rotate ANode,OldRightLeft left
                  OldParent                           OldParent
                      |                                  |
                    ANode                           OldRightLeft
                       \                               /      \
                    OldRight             =>          ANode    OldRight
                      /                                \         /
               OldRightLeft                OldRightLeftLeft OldRightLeftRight
                   /     \
        OldRightLeftLeft OldRightLeftRight
        }
        OldRightLeft:=GetPosition(OldRight,poLeft);
        RotateRight(OldRight);
        RotateLeft(ANode);
        if (GetBalance(OldRightLeft)<=0) then
          SetBalance(ANode,0)
        else
          SetBalance(ANode,-1);
        if (GetBalance(OldRightLeft)>=0) then
          SetBalance(OldRight,0)
        else
          SetBalance(OldRight,+1);
        SetBalance(OldRightLeft,0);
        ANode:=OldRightLeft;
      end;
    end else begin
      // Node.Balance=-2
      // Node is overweighted to the left
      OldLeft:=GetPosition(ANode,poLeft);
      if (GetBalance(OldLeft)<=0) then begin
        // rotate OldLeft,ANode right
        RotateRight(ANode);
        SetBalance(ANode,(-1-GetBalance(OldLeft))); // toggle 0 and -1
        SetBalance(OldLeft,GetBalance(OldLeft)+1);
        ANode:=OldLeft;
      end else begin
        // OldLeft.Balance = 1
        { double rotate left right
          = rotate OldLeft,OldLeftRight left
            and then rotate OldLeft,ANode right
                    OldParent                           OldParent
                        |                                  |
                      ANode                            OldLeftRight
                       /                               /         \
                    OldLeft             =>          OldLeft    ANode
                       \                                \         /
                   OldLeftRight               OldLeftRightLeft OldLeftRightRight
                     /     \
          OldLeftRightLeft OldLeftRightRight
        }
        OldLeftRight:=GetPosition(OldLeft,poRight);
        RotateLeft(OldLeft);
        RotateRight(ANode);
        if (GetBalance(OldLeftRight)>=0) then
          SetBalance(ANode,0)
        else
          SetBalance(ANode,+1);
        if (GetBalance(OldLeftRight)<=0) then
          SetBalance(OldLeft,0)
        else
          SetBalance(OldLeft,-1);
        SetBalance(OldLeftRight,0);
        ANode:=OldLeftRight;
      end;
    end;
  end;
end;

procedure TAVLAbstractTree<T>.BalanceAfterInsert(ANode : T);
var
  OldParent, OldRight, OldLeft: T;
begin
  OldParent:=GetPosition(ANode,poParent);
  while Not IsNil(OldParent) do begin
    if (AreEquals(GetPosition(OldParent,poLeft),ANode)) then begin
      // Node is left child
      SetBalance(OldParent,GetBalance(OldParent)-1);
      if (GetBalance(OldParent)=0) then exit;
      if (GetBalance(OldParent)=-1) then begin
        ANode:=OldParent;
        OldParent:=GetPosition(ANode,poParent);
        continue;
      end;
      // OldParent.Balance=-2
      if (GetBalance(ANode)=-1) then begin
        { rotate ANode,ANode.Parent right
             OldParentParent        OldParentParent
                   |                     |
               OldParent        =>     ANode
                 /                        \
              ANode                     OldParent
                \                        /
              OldRight               OldRight      }
        RotateRight(OldParent);
        SetBalance(ANode,0);
        SetBalance(OldParent,0);
      end else begin
        // Node.Balance = +1
        { double rotate
          = rotate ANode,OldRight left and then rotate OldRight,OldParent right
             OldParentParent             OldParentParent
                    |                           |
                OldParent                    OldRight
                   /            =>          /        \
                 ANode                   ANode      OldParent
                    \                       \          /
                   OldRight          OldRightLeft  OldRightRight
                     / \
          OldRightLeft OldRightRight
        }
        OldRight:=GetPosition(ANode,poRight);
        RotateLeft(ANode);
        RotateRight(OldParent);
        if (GetBalance(OldRight)<=0) then
          SetBalance(ANode,0)
        else
          SetBalance(ANode,-1);
        if (GetBalance(OldRight)=-1) then
          SetBalance(OldParent,1)
        else
          SetBalance(OldParent,0);
        SetBalance(OldRight,0);
      end;
      exit;
    end else begin
      // Node is right child
      SetBalance(OldParent, GetBalance(OldParent)+1);
      if (GetBalance(OldParent)=0) then exit;
      if (GetBalance(OldParent)=+1) then begin
        ANode:=OldParent;
        OldParent:=GetPosition(ANode,poParent);
        continue;
      end;
      // OldParent.Balance = +2
      if (GetBalance(ANode)=+1) then begin
        { rotate OldParent,ANode left
             OldParentParent        OldParentParent
                   |                     |
               OldParent        =>     ANode
                    \                   /
                  ANode               OldParent
                   /                      \
                OldLeft                 OldLeft      }
        RotateLeft(OldParent);
        SetBalance(ANode,0);
        SetBalance(OldParent,0);
      end else begin
        // Node.Balance = -1
        { double rotate
          = rotate OldLeft,ANode right and then rotate OldParent,OldLeft right
             OldParentParent             OldParentParent
                    |                           |
                OldParent                    OldLeft
                     \            =>        /       \
                    ANode               OldParent   ANode
                     /                     \          /
                  OldLeft          OldLeftLeft  OldLeftRight
                    / \
         OldLeftLeft OldLeftRight
        }
        OldLeft:=GetPosition(ANode,poLeft);
        RotateRight(ANode);
        RotateLeft(OldParent);
        if (GetBalance(OldLeft)>=0) then
          SetBalance(ANode,0)
        else
          SetBalance(ANode,+1);
        if (GetBalance(OldLeft)=+1) then
          SetBalance(OldParent,-1)
        else
          SetBalance(OldParent,0);
        SetBalance(OldLeft,0);
      end;
      exit;
    end;
  end;
end;

procedure TAVLAbstractTree<T>.BeginUpdate;
begin
  inc(FDisabledsCount);
end;

constructor TAVLAbstractTree<T>.Create(const OnCompareMethod: TComparison<T>; AAllowDuplicates : Boolean);
begin
  inherited Create;
  FOnCompare:=OnCompareMethod;
  FCount:=0;
  FDisabledsCount := 0;
  FAllowDuplicates := AAllowDuplicates;
end;

procedure TAVLAbstractTree<T>.Delete(var ANode: T);
var OldParent, Child, LSuccessor: T;
begin
  BeginUpdate;
  try
    if (Not IsNil(GetPosition(ANode,poLeft))) and (Not IsNil(GetPosition(ANode,poRight))) then begin
      // ANode has both: Left and Right
      // Switch ANode position with Successor
      // Because ANode.Right<>nil the Successor is a child of ANode
      LSuccessor := FindSuccessor(ANode);
      SwitchPositionWithSuccessor(ANode,LSuccessor);
    end;
    // left or right is nil
    OldParent:=GetPosition(ANode,poParent);
    ClearPosition(ANode,poParent);
    if Not IsNil(GetPosition(ANode,poLeft)) then
      Child:=GetPosition(ANode,poLeft)
    else
      Child:=GetPosition(ANode,poRight);
    if Not IsNil(Child) then
      SetPosition(Child,poParent,OldParent);
    if Not IsNil(OldParent) then begin
      // Node has parent
      if (AreEquals(GetPosition(OldParent,poLeft),ANode)) then begin
        // Node is left child of OldParent
        SetPosition(OldParent,poLeft,Child);
        SetBalance(OldParent, GetBalance(OldParent)+1);
      end else begin
        // Node is right child of OldParent
        SetPosition(OldParent,poRight,Child);
        SetBalance(OldParent, GetBalance(OldParent)-1);
      end;
      BalanceAfterDelete(OldParent);
    end else begin
      // Node was Root
      SetRoot( Child );
    end;
    dec(FCount);

    DisposeNode(ANode);

  finally
    EndUpdate;
  end;
end;


procedure TAVLAbstractTree<T>.EndUpdate;
begin
  if FDisabledsCount<=0 then Raise EAVLAbstractTree.Create('EndUpdate invalid');
  Dec(FDisabledsCount);
  if FDisabledsCount=0 then UpdateFinished;
end;

procedure TAVLAbstractTree<T>.SwitchPositionWithSuccessor(aNode, aSuccessor: T);
{ called by delete, when aNode.Left<>nil and aNode.Right<>nil
  Switch ANode position with Successor
  Because ANode.Right<>nil the Successor is a child of ANode }
var
  OldBalance: Integer;
  OldParent, OldLeft, OldRight,
  OldSuccParent, OldSuccLeft, OldSuccRight: T;
begin
  OldBalance:=GetBalance(aNode);
  SetBalance(aNode, GetBalance(aSuccessor));
  SetBalance(aSuccessor, OldBalance);

  OldParent:=GetPosition(aNode,poParent);
  OldLeft:=GetPosition(aNode,poLeft);
  OldRight:=GetPosition(aNode,poRight);
  OldSuccParent:=GetPosition(aSuccessor,poParent);
  OldSuccLeft:=GetPosition(aSuccessor,poLeft);
  OldSuccRight:=GetPosition(aSuccessor,poRight);

  if Not IsNil(OldParent) then begin
    if AreEquals(GetPosition(OldParent,poLeft),aNode) then
      SetPosition(OldParent,poLeft,aSuccessor)
    else
      SetPosition(OldParent,poRight,aSuccessor);
  end else
    SetRoot(aSuccessor);
  SetPosition(aSuccessor,poParent,OldParent);

  if Not AreEquals(OldSuccParent,aNode) then begin
    if AreEquals(GetPosition(OldSuccParent,poLeft),aSuccessor) then
      SetPosition(OldSuccParent,poLeft,aNode)
    else
      SetPosition(OldSuccParent,poRight,aNode);
    SetPosition(aSuccessor,poRight,OldRight);
    SetPosition(aNode,poParent,OldSuccParent);
    if Not IsNil(OldRight) then
      SetPosition(OldRight,poParent,aSuccessor);
  end else begin
    {  aNode            aSuccessor
         \          =>    \
         aSuccessor       aNode  }
    SetPosition(aSuccessor,poRight,aNode);
    SetPosition(aNode,poParent,aSuccessor);
  end;

  SetPosition(aNode,poLeft,OldSuccLeft);
  if Not IsNil(OldSuccLeft) then
    SetPosition(OldSuccLeft,poParent,aNode);
  SetPosition(aNode,poRight,OldSuccRight);
  if Not IsNil(OldSuccRight) then
    SetPosition(OldSuccRight,poParent,aNode);
  SetPosition(aSuccessor,poLeft,OldLeft);
  if Not IsNil(OldLeft) then
    SetPosition(OldLeft,poParent,aSuccessor);
end;

function TAVLAbstractTree<T>.Find(const AData: T): T;
var Comp: integer;
  LPreviousSearch : TOrderedList<T>;
begin
  LPreviousSearch := TOrderedList<T>.Create(False,FOnCompare); // Protection against circular "malformed" structure
  try
    Result:=Root;
    while (Not IsNil(Result)) do begin
      if LPreviousSearch.Add(Result)<0 then raise EAVLAbstractTree.Create('Circular T structure at Find for T='+ToString(Result)+ ' searching for '+ToString(AData));
      Comp:=fOnCompare(AData,Result);
      if Comp=0 then exit;
      if Comp<0 then begin
        Result:=GetPosition(Result,poLeft);
      end else begin
        Result:=GetPosition(Result,poRight);
      end;
    end;
  finally
    LPreviousSearch.Free;
  end;
end;

function TAVLAbstractTree<T>.FindInsertPos(const AData: T): T;
var Comp: integer;
  LPreviousSearch : TOrderedList<T>;
begin
  LPreviousSearch := TOrderedList<T>.Create(False,FOnCompare); // Protection against circular "malformed" structure
  try
    Result:=Root;
    while (Not IsNil(Result)) do begin
      if LPreviousSearch.Add(Result)<0 then raise EAVLAbstractTree.Create('Circular T structure at FindInsertPos for T='+ToString(Result)+ ' searching for '+ToString(AData));
      Comp:=fOnCompare(AData,Result);
      if Comp<0 then begin
        if (HasPosition(Result,poLeft)) then begin
          Result:=GetPosition(Result,poLeft);
        end else begin
          Exit;
        end;
      end else begin
        if (HasPosition(Result,poRight)) then begin
          Result:=GetPosition(Result,poRight);
        end else begin
          Exit;
        end;
      end;
    end;
  finally
    LPreviousSearch.Free;
  end;
end;

function TAVLAbstractTree<T>.FindSuccessor(const ANode: T): T;
begin
  if HasPosition(ANode,poRight) then begin
    Result := GetPosition(ANode,poRight);
    while (HasPosition(Result,poLeft)) do Result:=GetPosition(Result,poLeft);
  end else begin
    Result := ANode;
    while (HasPosition(Result,poParent)) and (AreEquals(GetPosition(GetPosition(Result,poParent),poRight),Result)) do
      Result:=GetPosition(Result,poParent);
    Result := GetPosition(Result,poParent);
  end;
end;

function TAVLAbstractTree<T>.ToString: String;
var i : Integer;
  LStrings : TStringList;
  LNode : T;
begin
  LStrings := TStringList.Create;
  try
    i := 0;
    LNode := FindLowest;
    while (Not IsNil(LNode)) do begin
      inc(i);
      LStrings.Add(Format('Pos:%d - %s',[i,ToString(LNode)]));
      LNode := FindSuccessor(LNode);
    end;
    LStrings.Add(Format('Total:%d',[i]));
    Result := LStrings.Text;
  finally
    LStrings.Free;
  end;
end;

procedure TAVLAbstractTree<T>.UpdateFinished;
{$IFDEF ABSTRACTMEM_TESTING_MODE}
var LErrors : TStrings;
{$ENDIF}
begin
  // Nothing to do here. Used in inheritance classes
  {$IFDEF ABSTRACTMEM_TESTING_MODE}
  LErrors := TStringList.Create;
  Try
    if ConsistencyCheck(LErrors)<>0 then begin
      raise EAVLAbstractTree.Create('CONSISTENCY ERRORS'+#10+LErrors.Text);
    end;
  Finally
    LErrors.Free;
  End;
  {$ENDIF}
end;

function TAVLAbstractTree<T>.ToString(const ANode: T): String;
begin
  Result := Format('Abstract T %d bytes',[SizeOf(T)]);
end;

function TAVLAbstractTree<T>.FindPrecessor(const ANode: T): T;
begin
  if HasPosition(ANode,poLeft) then begin
    Result := GetPosition(ANode,poLeft);
    while (HasPosition(Result,poRight)) do Result:=GetPosition(Result,poRight);
  end else begin
    Result := ANode;
    while (HasPosition(Result,poParent)) and (AreEquals(GetPosition(GetPosition(Result,poParent),poLeft),Result)) do
      Result:=GetPosition(Result,poParent);
    Result := GetPosition(Result,poParent);
  end;
end;

function TAVLAbstractTree<T>.CheckNode(const ANode: T; ACheckedList : TOrderedList<T>; var ALeftDepth, ARightDepth : Integer; const AErrors : TStrings): integer;
var i : Integer;
  LLeftDepth, LRightDepth : Integer;
  LParent, LLeft, LRight : T;
begin
  Result := 0;

  LLeftDepth := 0;
  LRightDepth := 0;

  ALeftDepth := 0;
  ARightDepth := 0;

  if IsNil(ANode) then begin
    exit(0);
  end;
  if Assigned(ACheckedList) then begin
    if ACheckedList.Find(ANode,i) then begin
      // Found in previous searchs...
      Result := -1;
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency circular found at %d of %d -> %s',[i,ACheckedList.Count,ToString(ANode)]));
      end;
      Exit;
    end;
    ACheckedList.Add(ANode);
  end;

  // test left son
  if HasPosition(ANode,poLeft) then begin
    LLeft := GetPosition(ANode,poLeft);
    if Not AreEquals(GetPosition(GetPosition(ANode,poLeft),poParent),ANode) then begin
      Result:=-2;
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency not equals in left for %s',[ToString(ANode)]));
      end;
      Exit;
    end;
    if fOnCompare(GetPosition(ANode,poLeft),ANode)>0 then begin
      Result:=-3;
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency compare>0 in left for %s',[ToString(ANode)]));
      end;
      Exit;
    end;
    Result:=CheckNode(GetPosition(ANode,poLeft),ACheckedList,LLeftDepth,LRightDepth,AErrors);
    if LLeftDepth>LRightDepth then inc(ALeftDepth,LLeftDepth+1)
    else inc(ALeftDepth,LRightDepth+1);
    if Result<>0 then Exit;
  end else ClearNode(LLeft);
  // test right son
  if HasPosition(ANode,poRight) then begin
    LRight := GetPosition(ANode,poRight);
    if Not AreEquals(GetPosition(GetPosition(ANode,poRight),poParent),ANode) then begin
      Result:=-4;
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency not equals in right for %s found %s at right.parent',[ToString(ANode),ToString(GetPosition(GetPosition(ANode,poRight),poParent))]));
      end;
      Exit;
    end;
    if fOnCompare(GetPosition(ANode,poRight),ANode)<0 then begin
      Result:=-5;
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency compare>0 in right for %s',[ToString(ANode)]));
      end;
      Exit;
    end;
    Result:=CheckNode(GetPosition(ANode,poRight),ACheckedList,LLeftDepth,LRightDepth,AErrors);
    if LLeftDepth>LRightDepth then inc(ARightDepth,LLeftDepth+1)
    else inc(ARightDepth,LRightDepth+1);
    if Result<>0 then Exit;
  end else ClearNode(LRight);

  if (HasPosition(ANode,poParent)) then begin
    LParent := GetPosition(ANode,poParent);
  end else ClearNode(LParent);

  if Not IsNil(LParent) then begin
    if AreEquals(ANode,LParent) then begin
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency Self=Parent for %s (Parent %s)',[ToString(ANode),ToString(LParent)]));
      end;
      Result := -7;
    end;
  end;
  if Not IsNil(LLeft) then begin
    if AreEquals(ANode,LLeft) then begin
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency Self=Left for %s (Left %s)',[ToString(ANode),ToString(LLeft)]));
      end;
      Result := -8;
    end;
  end;
  if Not IsNil(LRight) then begin
    if AreEquals(ANode,LRight) then begin
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency Self=Right for %s (Right %s)',[ToString(ANode),ToString(LRight)]));
      end;
      Result := -9;
    end;
  end;
  if (Not IsNil(LParent)) and (Not IsNil(LLeft)) then begin
    if AreEquals(LParent,LLeft) then begin
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency Parent=Left for %s (Parent %s)',[ToString(ANode),ToString(LParent)]));
      end;
      Result := -10;
    end;
  end;
  if (Not IsNil(LParent)) and (Not IsNil(LRight)) then begin
    if AreEquals(LParent,LRight) then begin
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency Parent=Right for %s (Parent %s)',[ToString(ANode),ToString(LParent)]));
      end;
      Result := -11;
    end;
  end;
  if (Not IsNil(LLeft)) and (Not IsNil(LRight)) then begin
    if AreEquals(LLeft,LRight) then begin
      if Assigned(AErrors) then begin
        AErrors.Add(Format('Error Consistency Left=Right for %s (Left %s)',[ToString(ANode),ToString(LLeft)]));
      end;
      Result := -12;
    end;
  end;

  // Check balance
  if GetBalance(ANode)<>(ARightDepth - ALeftDepth) then begin
    if Assigned(AErrors) then begin
      AErrors.Add(Format('Error Consistency balance (%d <> Right(%d) - Left(%d)) at %s',[GetBalance(ANode),ARightDepth,ALeftDepth,ToString(ANode)]));
    end;
    Result := -15;
    Exit;
  end;
end;

procedure TAVLAbstractTree<T>.RotateLeft(var ANode: T);
{    Parent                Parent
       |                     |
      Node        =>       OldRight
      /  \                  /
   Left OldRight          Node
          /               /  \
     OldRightLeft      Left OldRightLeft  }
var
  AParent, OldRight, OldRightLeft: T;
begin
  OldRight:=GetPosition(aNode,poRight);
  OldRightLeft:=GetPosition(OldRight,poLeft);
  AParent:=GetPosition(aNode,poParent);
  if Not IsNil(AParent) then begin
    if AreEquals(GetPosition(AParent,poLeft),aNode) then
      SetPosition(AParent,poLeft,OldRight)
    else
      SetPosition(AParent,poRight,OldRight);
  end else
    SetRoot( OldRight );
  SetPosition(OldRight,poParent,AParent);
  SetPosition(aNode,poParent,OldRight);
  SetPosition(aNode,poRight,OldRightLeft);
  if Not IsNil(OldRightLeft) then
    SetPosition(OldRightLeft,poParent,aNode);
  SetPosition(OldRight,poLeft,aNode);
end;

procedure TAVLAbstractTree<T>.RotateRight(var ANode: T);
{       Parent              Parent
          |                   |
         Node        =>     OldLeft
         /   \                 \
    OldLeft  Right            Node
        \                     /  \
   OldLeftRight      OldLeftRight Right  }
var
  AParent, OldLeft, OldLeftRight: T;
begin
  OldLeft:=GetPosition(ANode,poLeft);
  OldLeftRight:=GetPosition(OldLeft,poRight);
  AParent:=GetPosition(ANode,poParent);
  if Not IsNil(AParent) then begin
    if AreEquals(GetPosition(AParent,poLeft),aNode) then
      SetPosition(AParent,poLeft,OldLeft)
    else
      SetPosition(AParent,poRight,OldLeft);
  end else
    SetRoot( OldLeft );
  SetPosition(OldLeft,poParent,AParent);
  SetPosition(aNode,poParent,OldLeft);
  SetPosition(aNode,poLeft,OldLeftRight);
  if Not IsNil(OldLeftRight) then
    SetPosition(OldLeftRight,poParent,aNode);
  SetPosition(OldLeft,poRight,aNode);
end;

procedure TAVLAbstractTree<T>.CheckNode(const ANode: T);
var LLeft,LRight : Integer;
  LErrors : TStrings;
begin
  LErrors := TStringList.Create;
  try
    if CheckNode(ANode,Nil,LLeft,LRight,LErrors)<>0 then
      raise EAVLAbstractTree.Create('CHECK CONSISTENCY ERROR'+#10+LErrors.Text);
  finally
    LErrors.Free;
  end;
end;

function TAVLAbstractTree<T>.ConsistencyCheck(const AErrors : TStrings): integer;
var LCheckedList : TOrderedList<T>;
var LLeftDepth, LRightDepth : Integer;
begin
  LCheckedList := TOrderedList<T>.Create(False,FOnCompare);
  try
    LLeftDepth := 0;
    LRightDepth := 0;
    Result:=CheckNode(Root,LCheckedList,LLeftDepth,LRightDepth,AErrors);
  finally
    LCheckedList.Free;
  end;
end;

{ TPAVLPointerTree }

function TPAVLPointerTree.AreEquals(const ANode1, ANode2: PAVLPointerTreeNode): Boolean;
begin
  Result := ANode1 = ANode2;
end;

procedure TPAVLPointerTree.ClearNode(var ANode: PAVLPointerTreeNode);
begin
  ANode := Nil;
end;

procedure TPAVLPointerTree.ClearPosition(var ANode: PAVLPointerTreeNode; APosition: TAVLTreePosition);
begin
  case APosition of
    poParent: ANode.parent := Nil;
    poLeft: ANode.left := Nil;
    poRight: ANode.right := Nil;
  end;
end;

constructor TPAVLPointerTree.Create(const OnCompareMethod: TComparison<PAVLPointerTreeNode>; AAllowDuplicates : Boolean);
begin
  FRoot := Nil;
  inherited;
end;

procedure TPAVLPointerTree.DisposeNode(var ANode: PAVLPointerTreeNode);
begin
  if Not Assigned(ANode) then Exit;
  Dispose( ANode );
  ANode := Nil;
end;

function TPAVLPointerTree.GetBalance(const ANode: PAVLPointerTreeNode): Integer;
begin
  Result := ANode^.balance;
end;

function TPAVLPointerTree.GetPosition(const ANode: PAVLPointerTreeNode;
  APosition: TAVLTreePosition): PAVLPointerTreeNode;
begin
  case APosition of
    poParent: Result := ANode.parent;
    poLeft: Result := ANode.left;
    poRight: Result := ANode.right;
  else raise EAVLAbstractTree.Create('Undefined 20200310-1');
  end;
end;

function TPAVLPointerTree.GetRoot: PAVLPointerTreeNode;
begin
  Result := FRoot;
end;

function TPAVLPointerTree.HasPosition(const ANode: PAVLPointerTreeNode;
  APosition: TAVLTreePosition): Boolean;
begin
  case APosition of
    poParent: Result := Assigned( ANode.parent );
    poLeft: Result := Assigned( ANode.left );
    poRight: Result := Assigned( ANode.right );
  else raise EAVLAbstractTree.Create('Undefined 20200310-2');
  end;
end;

function TPAVLPointerTree.IsNil(const ANode: PAVLPointerTreeNode): Boolean;
begin
  Result := ANode = Nil;
end;

procedure TPAVLPointerTree.SetBalance(var ANode: PAVLPointerTreeNode;
  ANewBalance: Integer);
begin
  ANode^.balance := ANewBalance;
end;

procedure TPAVLPointerTree.SetPosition(var ANode: PAVLPointerTreeNode;
  APosition: TAVLTreePosition; const ANewValue: PAVLPointerTreeNode);
begin
  case APosition of
    poParent: ANode.parent := ANewValue;
    poLeft: ANode.left := ANewValue;
    poRight: ANode.right := ANewValue;
  end;
end;

procedure TPAVLPointerTree.SetRoot(const Value: PAVLPointerTreeNode);
begin
  FRoot := Value;
end;

function TPAVLPointerTree.ToString(const ANode: PAVLPointerTreeNode): String;
var LParent, LLeft, LRight : String;
begin
  if Assigned(ANode) then begin
    if Assigned(ANode.parent) then LParent := IntToStr(Integer(ANode.parent.data)) else LParent := 'NIL';
    if Assigned(ANode.left) then LLeft := IntToStr(Integer(ANode.left.data)) else LLeft := 'NIL';
    if Assigned(ANode.right) then LRight := IntToStr(Integer(ANode.right.data)) else LRight := 'NIL';

    Result := Format('%d (Parent:%s Left:%s Right:%s Balance:%d)',[Integer(ANode.data),LParent,LLeft,LRight,ANode.balance]);
  end else begin
    Result := 'NIL';
  end;
end;

initialization

finalization

end.
