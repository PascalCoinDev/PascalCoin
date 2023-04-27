unit UMemoryBTreeData;

{
  This file is part of AbstractMem framework

  Copyright (C) 2020-2023 Albert Molina - bpascalblockchain@gmail.com

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
  UOrderedList, UAbstractBTree;

{$I ./ConfigAbstractMem.inc }

{$IFDEF ABSTRACTMEM_TESTING_MODE}
  {$DEFINE ABSTRACTMEM_CIRCULAR_SEARCH_PROTECTION}
{$ENDIF}

type
  EMemoryBTreeData = Class(EAbstractBTree);

  TMemoryBTreeDataAbstract<TBTreeData> = Class(TMemoryBTree<Pointer>)
  public
    type PBTreeData = ^TBTreeData;
  private
    var
    FOnCompareBTreeDataMethod: TComparison<TBTreeData>;
  protected
    function DoCompareData(const ALefTBTreeData, ARighTBTreeData: Pointer): Integer; override;
    //
    function AddInherited(const AData: PBTreeData) : Boolean;
    function DeleteInherited(const AData: PBTreeData) : Boolean;
  public
    constructor Create(const AOnCompareBTreeDataMethod: TComparison<TBTreeData>; AAllowDuplicates : Boolean; AOrder : Integer); reintroduce;
    procedure Add(); reintroduce;
    procedure Delete(); reintroduce;
    function FindData(const AData: TBTreeData; var AFoundData : TBTreeData) : Boolean; overload;
    function FindData(const AData: TBTreeData; var AFoundData : PBTreeData) : Boolean; overload;
    function FindDataPos(const AData: TBTreeData; out ANode : TAbstractBTree<Integer,Pointer>.TAbstractBTreeNode; out iPos : Integer) : Boolean; overload;
    function FindDataPrecessor(const AData : TBTreeData; var APrecessor : TBTreeData) : Boolean;
    function FindDataSuccessor(const AData : TBTreeData; var ASuccessor : TBTreeData) : Boolean;
    function FindDataLowest(out ALowest : TBTreeData) : Boolean;
    function FindDataHighest(out AHighest : TBTreeData) : Boolean;
  End;

  {$IFnDEF FPC}
  TMemoryBTreeDataIndex<TBTreeData> = Class;
  {$ENDIF}

  TMemoryBTreeData<TBTreeData> = Class(TMemoryBTreeDataAbstract<TBTreeData>)
  public
  private
    //    Ref: 20211111-1  -- TODO
    //    FreePascal issue: Does not allow recursive Generics...
    //    due to this issue (on Delphi is allowed) then I must use TList< TOjbect > instead
    //    last FreePascal version with this issue: 3.2.0  (will need to check on future versions)
    {$IFDEF FPC}
    FIndexes : TList< TObject >;
    {$ELSE}
    //    Ref: 20211111-1 I can't use this... in Delphi it works! Not in FreePascal... SHIT!
    FIndexes : TList< TMemoryBTreeDataIndex<TBTreeData> >;
    {$ENDIF}
  protected
    procedure DisposeData(var AData : Pointer); override;
  public
    constructor Create(const AOnCompareBTreeDataMethod: TComparison<TBTreeData>; AAllowDuplicates : Boolean; AOrder : Integer);
    destructor Destroy; override;
    function CanAddData(const AData: TBTreeData) : Boolean;
    function AddData(const AData: TBTreeData) : Boolean;
    function DeleteData(const AData: TBTreeData) : Boolean;
    function IndexesCount : Integer;
    //    See ref: 20211111-1
    {$IFDEF FPC}
    function GetIndex(AIndex : Integer) : TObject;
    {$ELSE}
    function GetIndex(AIndex : Integer) : TMemoryBTreeDataIndex<TBTreeData>;
    {$ENDIF}
    procedure CheckConsistency; override;
    procedure AddIndex(const AOnCompareBTreeDataMethod: TComparison<TBTreeData>; AAllowDuplicates : Boolean);
  End;

  TMemoryBTreeDataIndex<TBTreeData> = Class(TMemoryBTreeDataAbstract<TBTreeData>)
  private
    FOwner : TMemoryBTreeData<TBTreeData>;
  protected
  public
    Property Owner : TMemoryBTreeData<TBTreeData> read FOwner;
    constructor Create(AOwner : TMemoryBTreeData<TBTreeData>;
      const AOnCompareBTreeDataMethod: TComparison<TBTreeData>; AAllowDuplicates : Boolean);
    destructor Destroy; override;
  End;


implementation

uses UAbstractMem;

{ TMemoryBTreeDataAbstract<TBTreeData> }

procedure TMemoryBTreeDataAbstract<TBTreeData>.Add;
begin
  raise EMemoryBTreeData.Create('Invalid use of Abstract function '+ClassName+'.Add');
end;

function TMemoryBTreeDataAbstract<TBTreeData>.AddInherited(const AData: PBTreeData): Boolean;
begin
  Result := inherited Add(AData);
end;

constructor TMemoryBTreeDataAbstract<TBTreeData>.Create(
  const AOnCompareBTreeDataMethod: TComparison<TBTreeData>;
  AAllowDuplicates: Boolean; AOrder: Integer);
begin
  FOnCompareBTreeDataMethod := AOnCompareBTreeDataMethod;
  inherited Create(OnCompareDataMethod,AAllowDuplicates,AOrder);
end;

procedure TMemoryBTreeDataAbstract<TBTreeData>.Delete;
begin
  raise EMemoryBTreeData.Create('Invalid use of Abstract function '+ClassName+'.Delete');
end;

function TMemoryBTreeDataAbstract<TBTreeData>.DeleteInherited(
  const AData: PBTreeData): Boolean;
begin
  Result := inherited Delete(AData);
end;

function TMemoryBTreeDataAbstract<TBTreeData>.DoCompareData(
  const ALefTBTreeData, ARighTBTreeData: Pointer): Integer;
begin
  if ALefTBTreeData=ARighTBTreeData then Result := 0
  else Result := FOnCompareBTreeDataMethod(PBTreeData(ALefTBTreeData)^,PBTreeData(ARighTBTreeData)^);
end;

function TMemoryBTreeDataAbstract<TBTreeData>.FindData(const AData: TBTreeData;
  var AFoundData: PBTreeData): Boolean;
var P : PBTreeData;
  node : TAbstractBTreeNode;
  iPos : Integer;
begin
  new(P);
  try
    P^ := AData;
    if Find(P,node,iPos) then begin
      Result := True;
      AFoundData := PBTreeData(node.data[iPos]);
    end else Result := False;
  finally
    Dispose(P);
  end;
end;

function TMemoryBTreeDataAbstract<TBTreeData>.FindData(const AData: TBTreeData;
  var AFoundData: TBTreeData): Boolean;
var P : PBTreeData;
begin
  if FindData(AData,P) then begin
    AFoundData := P^;
    Result := True;
  end else Result := False;
end;

function TMemoryBTreeDataAbstract<TBTreeData>.FindDataHighest(
  out AHighest: TBTreeData): Boolean;
var P : Pointer;
begin
  if FindHighest(P) then begin
    Result := True;
    AHighest := PBTreeData(P)^;
  end else Result := False;
end;

function TMemoryBTreeDataAbstract<TBTreeData>.FindDataLowest(
  out ALowest: TBTreeData): Boolean;
var P : Pointer;
begin
  if FindLowest(P) then begin
    Result := True;
    ALowest := PBTreeData(P)^;
  end else Result := False;
end;

function TMemoryBTreeDataAbstract<TBTreeData>.FindDataPos(
  const AData: TBTreeData;
  out ANode: TAbstractBTree<Integer, Pointer>.TAbstractBTreeNode;
  out iPos: Integer): Boolean;
var P : PBTreeData;
begin
  new(P);
  try
    P^ := AData;
    Result := Find(P,ANode,iPos);
  finally
    Dispose(P);
  end;
end;

function TMemoryBTreeDataAbstract<TBTreeData>.FindDataPrecessor(
  const AData: TBTreeData; var APrecessor: TBTreeData): Boolean;
var P : PBTreeData;
 PFound : Pointer;
begin
  new(P);
  try
    P^ := AData;
    Result := FindPrecessor(P,PFound);
    if Result then APrecessor := PBTreeData(PFound)^;
  finally
    Dispose(P);
  end;
end;

function TMemoryBTreeDataAbstract<TBTreeData>.FindDataSuccessor(
  const AData: TBTreeData; var ASuccessor: TBTreeData): Boolean;
var P : PBTreeData;
 PFound : Pointer;
begin
  new(P);
  try
    P^ := AData;
    Result := FindSuccessor(P,PFound);
    if Result then ASuccessor := PBTreeData(PFound)^;
  finally
    Dispose(P);
  end;
end;

{ TMemoryBTreeData<TBTreeData> }

function TMemoryBTreeData<TBTreeData>.AddData(const AData: TBTreeData): Boolean;
var iPos : Integer;
  LNode : TAbstractBTreeNode;
  i : Integer;
  LBTreeIndex : TMemoryBTreeDataIndex<TBTreeData>;
  P : PBTreeData;
begin
  Lock;
  Try
  Result := True;
  New(P);
  Try
    P^ := AData;
    i := 0;
    while (Result) and (i<FIndexes.Count) do begin
      LBTreeIndex := TMemoryBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
      if (Not LBTreeIndex.AllowDuplicates) then begin
        Result := Not LBTreeIndex.Find(P,LNode,iPos);
      end;
      inc(i);
    end;
    if Result then begin
      Result := AddInherited(P);
      if Result then begin
        for i := 0 to FIndexes.Count-1 do begin
          LBTreeIndex := TMemoryBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
          if Not LBTreeIndex.AddInherited(P) then begin
              raise EMemoryBTreeData.Create(Format('Fatal error adding index %d/%d',
                [i+1,FIndexes.Count]));
          end;
        end;
      end;
    end;
  Finally
    if Not Result then begin
      // Dispose
      Dispose(P);
    end;
  End;
  Finally
    Unlock;
  End;
end;

procedure TMemoryBTreeData<TBTreeData>.AddIndex(
  const AOnCompareBTreeDataMethod: TComparison<TBTreeData>;
  AAllowDuplicates: Boolean);
var Lindex : TMemoryBTreeDataIndex<TBTreeData>;
begin
  Lock;
  try
    Lindex := TMemoryBTreeDataIndex<TBTreeData>.Create(Self,AOnCompareBTreeDataMethod,AAllowDuplicates);
  finally
    Unlock;
  end;
end;

function TMemoryBTreeData<TBTreeData>.CanAddData(
  const AData: TBTreeData): Boolean;
var iPos : Integer;
  LNode : TAbstractBTreeNode;
  i : Integer;
  LBTreeIndex : TMemoryBTreeDataIndex<TBTreeData>;
  P : PBTreeData;
begin
  Result := True;
  New(P);
  Try
    P^ := AData;
    i := 0;
    while (Result) and (i<FIndexes.Count) do begin
      LBTreeIndex := TMemoryBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
      if (Not LBTreeIndex.AllowDuplicates) then begin
        Result := Not LBTreeIndex.Find(P,LNode,iPos);
      end;
      inc(i);
    end;
    if (Result) And (Not AllowDuplicates) then begin
      Result := Not Find(P,LNode,iPos);
    end;
  Finally
    Dispose(P);
  End;
end;

procedure TMemoryBTreeData<TBTreeData>.CheckConsistency;
var i, nCount : Integer;
 LBTreeIndex : TMemoryBTreeDataIndex<TBTreeData>;
 LSearch : PBTreeData;
 LFound : TBTreeData;
begin
  inherited;
  nCount := 0;
  if FindDataLowest(LFound) then begin
    inc(nCount);
    for i := 0 to FIndexes.Count-1 do begin
      LBTreeIndex := TMemoryBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
      if Not LBTreeIndex.FindData(LFound,LSearch) then raise EMemoryBTreeData.Create(Format('Consistency error data %d not found on index %d/%d',[nCount, i+1,FIndexes.Count]));
    end;
    while FindDataSuccessor(LSearch^,LFound) do begin
      inc(nCount);
      for i := 0 to FIndexes.Count-1 do begin
        LBTreeIndex := TMemoryBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
        if Not LBTreeIndex.FindData(LFound,LSearch) then raise EMemoryBTreeData.Create(Format('Consistency error data %d not found on index %d/%d',[nCount, i+1,FIndexes.Count]));
      end;
    end;
  end;
  for i := 0 to FIndexes.Count-1 do begin
    LBTreeIndex := TMemoryBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
    if (LBTreeIndex.Count > Self.Count) then raise EMemoryBTreeData.Create(Format('Consistency error on index %d/%d count %d > %d',[i+1,FIndexes.Count,LBTreeIndex.Count,Self.Count]));
    LBTreeIndex.CheckConsistency;
  end;

end;

constructor TMemoryBTreeData<TBTreeData>.Create(
  const AOnCompareBTreeDataMethod: TComparison<TBTreeData>;
  AAllowDuplicates: Boolean; AOrder: Integer);
begin
  {$IFDEF FPC}
  FIndexes := TList< TObject >.Create;
  {$ELSE}
  FIndexes := TList< TMemoryBTreeDataIndex<TBTreeData> >.Create;
  {$ENDIF}
  inherited Create(AOnCompareBTreeDataMethod,AAllowDuplicates,AOrder);
end;

function TMemoryBTreeData<TBTreeData>.DeleteData(
  const AData: TBTreeData): Boolean;
var i : Integer;
  LBTreeIndex : TMemoryBTreeDataIndex<TBTreeData>;
  PIndex : Pointer;
  PBData : PBTreeData;
begin
  Lock;
  Try
    if FindData(AData,PBData) then begin
      // Delete from indexes
      for i := 0 to FIndexes.Count-1 do begin
        LBTreeIndex := TMemoryBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
        if Not LBTreeIndex.FindExt(PBData,PIndex) then begin
          // Fatal error. Not found
          raise EMemoryBTreeData.Create(Format('Fatal error Data not found in index %d/%d to Delete',[i+1,Findexes.Count]));
        end;
        if not LBTreeIndex.DeleteInherited(PIndex) then begin
          raise EMemoryBTreeData.Create(Format('Fatal error Data not deleted in index %d/%d',[i+1,Findexes.Count]));
        end;
      end;
      //
      DeleteInherited(PBData);
      Dispose(PBData);
      Result := True;
    End else Result := False;
  Finally
    Unlock;
  End;
end;

destructor TMemoryBTreeData<TBTreeData>.Destroy;
var i : Integer;
  LBTreeIndex : TMemoryBTreeDataIndex<TBTreeData>;
begin
  EraseTree;
  for i := FIndexes.Count-1 downto 0 do begin
    LBTreeIndex := TMemoryBTreeDataIndex<TBTreeData>(FIndexes.Items[i]);
    LBTreeIndex.Free;
  end;
  FreeAndNil(FIndexes);
  inherited;
end;

procedure TMemoryBTreeData<TBTreeData>.DisposeData(var AData: Pointer);
var P : PBTreeData;
begin
  inherited;
  P := AData;
  Dispose(P);
  AData := Nil;
end;

{$IFDEF FPC}
function TMemoryBTreeData<TBTreeData>.GetIndex(AIndex : Integer) : TObject;
{$ELSE}
function TMemoryBTreeData<TBTreeData>.GetIndex(AIndex : Integer) : TMemoryBTreeDataIndex<TBTreeData>;
{$ENDIF}
begin
  Result := FIndexes.Items[AIndex];
end;

function TMemoryBTreeData<TBTreeData>.IndexesCount: Integer;
begin
  Result := FIndexes.Count;
end;

{ TMemoryBTreeDataIndex<TBTreeData> }

constructor TMemoryBTreeDataIndex<TBTreeData>.Create(
  AOwner: TMemoryBTreeData<TBTreeData>;
  const AOnCompareBTreeDataMethod: TComparison<TBTreeData>;
  AAllowDuplicates: Boolean);
begin
  FOwner := AOwner;
  AOwner.FIndexes.Add(Self);
  inherited Create(AOnCompareBTreeDataMethod,AAllowDuplicates,AOwner.Order);
end;

destructor TMemoryBTreeDataIndex<TBTreeData>.Destroy;
begin
  FOwner.FIndexes.Remove(Self);
  inherited;
end;

initialization

finalization

end.
