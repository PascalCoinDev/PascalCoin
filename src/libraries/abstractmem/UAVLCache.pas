unit UAVLCache;

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

uses Classes, SysUtils,
  SyncObjs,
  UAbstractBTree, UOrderedList,
  {$IFNDEF FPC}System.Generics.Collections,System.Generics.Defaults{$ELSE}Generics.Collections,Generics.Defaults{$ENDIF};

type
  EAVLCache = class(Exception);

  { TAVLCache }

  TAVLCache<T> = Class
  public
    type
      PAVLCacheMemData = ^TAVLCacheMemData;
      TAVLCacheMemData = record
        parent : PAVLCacheMemData;
        left : PAVLCacheMemData;
        right : PAVLCacheMemData;
        balance : ShortInt;
        //
        used_previous : PAVLCacheMemData;
        used_next : PAVLCacheMemData;
        pendingToSave : Boolean;
        //
        data : T;
        procedure Clear;
        function ToString : String;
      end;
  private
    type
    { TAVLCacheMem }
    TAVLCacheMem = Class(TAVLAbstractTree<PAVLCacheMemData>)
    private
      FRoot : PAVLCacheMemData;
      FOldestUsed, FNewestUsed : PAVLCacheMemData;
    protected
      function GetRoot: PAVLCacheMemData; override;
      procedure SetRoot(const Value: PAVLCacheMemData); override;
      function HasPosition(const ANode : PAVLCacheMemData; APosition : TAVLTreePosition) : Boolean; override;
      function GetPosition(const ANode : PAVLCacheMemData; APosition : TAVLTreePosition) : PAVLCacheMemData; override;
      procedure SetPosition(var ANode : PAVLCacheMemData; APosition : TAVLTreePosition; const ANewValue : PAVLCacheMemData); override;
      procedure ClearPosition(var ANode : PAVLCacheMemData; APosition : TAVLTreePosition); override;
      function GetBalance(const ANode : PAVLCacheMemData) : Integer; override;
      procedure SetBalance(var ANode : PAVLCacheMemData; ANewBalance : Integer); override;
      function AreEquals(const ANode1, ANode2 : PAVLCacheMemData) : Boolean; override;
      procedure ClearNode(var ANode : PAVLCacheMemData); override;
      procedure DisposeNode(var ANode : PAVLCacheMemData); override;

      procedure DoMark(var ANode : PAVLCacheMemData; AAddToList : Boolean);

    public
      function IsNil(const ANode : PAVLCacheMemData) : Boolean; override;
      Constructor Create(const OnCompareMethod: TComparison<PAVLCacheMemData>; AAllowDuplicates : Boolean); override;
      function ConsistencyCheck(const AErrors : TStrings): integer; override;
    end;
    var FAVLCacheMem : TAVLCacheMem;
    FDefaultMax : Integer;
    FAVLCacheLock : TCriticalSection;
  protected
    procedure BeforeDelete(var AData : T); virtual;
    procedure ConsistencyCheck;
  public
    Constructor Create(ADefaultMax : Integer; const AOnCompareMethod: TComparison<PAVLCacheMemData>);
    Destructor Destroy; override;
    //
    function Find(const AData : T; out AFound : T) : Boolean;
    procedure Add(const AData : T);
    procedure Remove(const AData : T);
    function Exists(const AData : T) : Boolean;
    procedure Clear;
    function TreeToString: String;
    function ToString(const AData : T) : String; overload; virtual;
  End;

implementation

{ TAVLCache.TAVLCacheMem }

function TAVLCache<T>.TAVLCacheMem.GetRoot: PAVLCacheMemData;
begin
  Result := FRoot;
end;

procedure TAVLCache<T>.TAVLCacheMem.SetRoot(const Value: PAVLCacheMemData);
begin
  FRoot := Value;
end;

function TAVLCache<T>.TAVLCacheMem.HasPosition(const ANode: PAVLCacheMemData;
  APosition: TAVLTreePosition): Boolean;
begin
  case APosition of
    poParent: Result := Assigned( ANode^.parent );
    poLeft: Result := Assigned( ANode^.left );
    poRight: Result := Assigned( ANode^.right );
  else raise EAVLAbstractTree.Create('Undefined 20200324-5');
  end;
end;

function TAVLCache<T>.TAVLCacheMem.GetPosition(const ANode: PAVLCacheMemData;
  APosition: TAVLTreePosition): PAVLCacheMemData;
begin
  case APosition of
    poParent: Result := ANode^.parent;
    poLeft: Result := ANode^.left;
    poRight: Result := ANode^.right;
  else raise EAVLAbstractTree.Create('Undefined 20200324-4');
  end;
end;

procedure TAVLCache<T>.TAVLCacheMem.SetPosition(var ANode: PAVLCacheMemData;
  APosition: TAVLTreePosition; const ANewValue: PAVLCacheMemData);
begin
  case APosition of
    poParent: ANode^.parent := ANewValue;
    poLeft: ANode^.left := ANewValue;
    poRight: ANode^.right := ANewValue;
  end;
end;

procedure TAVLCache<T>.TAVLCacheMem.ClearPosition(var ANode: PAVLCacheMemData;
  APosition: TAVLTreePosition);
begin
  case APosition of
    poParent: ANode^.parent := Nil;
    poLeft: ANode^.left := Nil;
    poRight: ANode^.right := Nil;
  end;
end;

function TAVLCache<T>.TAVLCacheMem.ConsistencyCheck(const AErrors: TStrings): integer;
var i, iLOrderPos : Integer;
  PLast, PCurrent : PAVLCacheMemData;
  LTotalNodes : Integer;
  LOrder : TOrderedList<PAVLCacheMemData>;
begin
  if Assigned(AErrors) then begin
    AErrors.Clear;
  end;
  Result := inherited ConsistencyCheck(AErrors);
  if Assigned(AErrors) then begin
    if (Result<>0) or (AErrors.Text<>'') then raise EAVLCache.Create(Format('Consistency error %d errors: %s',[Result,AErrors.Text]));

  end else if (Result<>0) then raise EAVLCache.Create(Format('Consistency error %d',[Result]));

  //
  LTotalNodes := 0;
  PCurrent := FindLowest;
  while (Assigned(PCurrent)) do begin
    inc(LTotalNodes);
    PCurrent := FindSuccessor(PCurrent);
  end;

  LOrder := TOrderedList<PAVLCacheMemData>.Create(False,OnCompareMethod);
  try
    PLast := Nil;
    PCurrent := FOldestUsed;
    i := 0;
    while (Assigned(PCurrent)) do begin
      inc(i);
      if PCurrent^.used_previous<>PLast then raise EAVLCache.Create(Format('Previous <> Last at %d for %s',[i,PCurrent^.ToString]));
      if LOrder.Find( PCurrent, iLOrderPos ) then begin
        raise EAVLCache.Create(Format('Circular in mark at %d for %s',[i,PCurrent^.ToString]));
      end;
      if LOrder.Add(PCurrent)<0 then raise EAVLCache.Create(Format('Circular in mark at %d for %s',[i,PCurrent^.ToString]));
      PLast := PCurrent;
      PCurrent := PCurrent^.used_next;
    end;
    // Check last
    if (PLast<>FNewestUsed) then raise EAVLCache.Create(Format('Last <> Newest at %d/%d',[i,LTotalNodes]));
    if (i<>LTotalNodes) then raise EAVLCache.Create(Format('Marked nodes %d <> CacheData nodes %d',[i,LTotalNodes]));

  finally
    LOrder.Free;
  end;

end;

constructor TAVLCache<T>.TAVLCacheMem.Create(
  const OnCompareMethod: TComparison<PAVLCacheMemData>;
  AAllowDuplicates: Boolean);
begin
  inherited;
  FRoot := Nil;
  FOldestUsed := Nil;
  FNewestUsed := Nil;
end;

function TAVLCache<T>.TAVLCacheMem.GetBalance(const ANode: PAVLCacheMemData
  ): Integer;
begin
  Result := ANode^.balance;
end;

procedure TAVLCache<T>.TAVLCacheMem.SetBalance(var ANode: PAVLCacheMemData;
  ANewBalance: Integer);
begin
  ANode^.balance := ANewBalance;
end;

function TAVLCache<T>.TAVLCacheMem.AreEquals(const ANode1,
  ANode2: PAVLCacheMemData): Boolean;
begin
  Result := ANode1 = ANode2;
end;

procedure TAVLCache<T>.TAVLCacheMem.ClearNode(var ANode: PAVLCacheMemData);
begin
  ANode := Nil;
end;

procedure TAVLCache<T>.TAVLCacheMem.DisposeNode(var ANode: PAVLCacheMemData);
begin
  if Not Assigned(ANode) then Exit;
  Dispose( ANode );
  ANode := Nil;
end;

procedure TAVLCache<T>.TAVLCacheMem.DoMark(var ANode: PAVLCacheMemData; AAddToList: Boolean);
{
    O = FOldestUsed
    N = FNewestUsed

    O       N
    A - B - C   ( D = New CacheMem )
}
begin
  if Assigned(ANode^.used_previous) then begin
    // B or C
    if (ANode^.used_previous^.used_next<>ANode) then raise EAVLCache.Create(Format('Inconsistent previous.next<>MySelf in %s',[ANode^.ToString]));
    if (FOldestUsed = ANode) then raise EAVLCache.Create(Format('Inconsistent B,C Oldest = MySelf in %s',[ANode^.ToString]));
    if Assigned(ANode^.used_next) then begin
      // B only
      if (ANode^.used_next^.used_previous<>ANode) then raise EAVLCache.Create(Format('Inconsistent B next.previous<>MySelf in %s',[ANode^.ToString]));
      if (FNewestUsed = ANode) then raise EAVLCache.Create(Format('Inconsistent B Newest = MySelf in %s',[ANode^.ToString]));
      ANode^.used_previous^.used_next := ANode^.used_next;
      ANode^.used_next^.used_previous := ANode^.used_previous;
    end else begin
      // C only
      if (FNewestUsed <> ANode) then raise EAVLCache.Create(Format('Inconsistent Newest <> MySelf in %s',[ANode^.ToString]));
      if (Not AAddToList) then begin
        ANode^.used_previous^.used_next := Nil;
      end;
    end;
  end else if assigned(ANode^.used_next) then begin
    // A
    if (ANode^.used_next^.used_previous<>ANode) then raise EAVLCache.Create(Format('Inconsistent A next.previous<>MySelf in %s',[ANode^.ToString]));
    if (FOldestUsed <> ANode) then raise EAVLCache.Create(Format('Inconsistent Oldest <> MySelf in %s',[ANode^.ToString]));
    if (FNewestUsed = ANode) then raise EAVLCache.Create(Format('Inconsistent A Newest = MySelf in %s',[ANode^.ToString]));
    ANode^.used_next^.used_previous := ANode^.used_previous; // = NIL
    FOldestUsed:=ANode^.used_next; // Set oldest
  end else begin
    // D
    if (FOldestUsed = ANode) and (FNewestUsed = ANode) then begin
      // D is the "only one", no previous, no next, but added or removed
      if (Not AAddToList) then begin
        FOldestUsed := Nil;
      end;
    end else begin
      if (FOldestUsed = ANode) then raise EAVLCache.Create(Format('Inconsistent D Oldest = MySelf in %s',[ANode^.ToString]));
      if (FNewestUsed = ANode) then raise EAVLCache.Create(Format('Inconsistent D Newest = MySelf in %s',[ANode^.ToString]));
    end;
    if Not Assigned(FOldestUsed) and (AAddToList) then begin
      // D is first one to be added
      FOldestUsed := ANode; // Set oldest
    end;
  end;
  if Assigned(FNewestUsed) then begin
    if Assigned(FNewestUsed^.used_next) then raise EAVLCache.Create(Format('Inconsistent Newest.next <> Nil in %s',[ANode^.ToString]));
  end;
  // Update ANode^.used_previous and ANode^.used_next
  if AAddToList then begin
    // Adding to list
    if (FNewestUsed<>ANode) then begin
      // Link to previous if newest <> MySelf
      ANode^.used_previous := FNewestUsed;
    end;
    if Assigned(FNewestUsed) then begin
      FNewestUsed^.used_next:= ANode;
    end;
    FNewestUsed:=ANode;
  end else begin
    // Removing from list
    if FNewestUsed = ANode then begin
      if (Assigned(ANode^.used_next)) then raise EAVLCache.Create(Format('Inconsistent next <> Nil when Self = Newest in %s',[ANode^.ToString]));
      FNewestUsed := ANode^.used_previous;
    end;
    ANode^.used_previous := Nil;
  end;
  ANode^.used_next := Nil;
end;

function TAVLCache<T>.TAVLCacheMem.IsNil(const ANode: PAVLCacheMemData): Boolean;
begin
  Result := Not Assigned(ANode);
end;

procedure TAVLCache<T>.Add(const AData: T);
var P, PToDelete : PAVLCacheMemData;
  i,LnToRemove : Integer;
begin
  FAVLCacheLock.Acquire;
  Try
  New(P);
  P^.Clear;
  P^.data := AData;
  FAVLCacheMem.Add(P);
  FAVLCacheMem.DoMark(P,True);
  if (FDefaultMax > 0) And (FAVLCacheMem.FCount>FDefaultMax) then begin
    // Dispose cache
    LnToRemove := FAVLCacheMem.FCount SHR 1;
    i := 1;
    P := FAVLCacheMem.FOldestUsed;
    while (Assigned(P)) And (i <= LnToRemove) do begin
      PToDelete := P;
      P := P^.used_next;

      FAVLCacheMem.DoMark(PToDelete,False);
      BeforeDelete(PToDelete^.data);
      FAVLCacheMem.Delete(PToDelete);

      inc(i);
    end;
  end;
  Finally
    FAVLCacheLock.Release;
  End;
end;

procedure TAVLCache<T>.BeforeDelete(var AData: T);
begin
//
end;

procedure TAVLCache<T>.Clear;
var P, PCurr : PAVLCacheMemData;
begin
  FAVLCacheLock.Acquire;
  Try
  PCurr := FAVLCacheMem.FindLowest;
  while (Assigned(PCurr)) do begin
    P := PCurr;
    PCurr := FAVLCacheMem.FindSuccessor(P);
    BeforeDelete(P^.data);
    FAVLCacheMem.DoMark(P,False);
    FAVLCacheMem.Delete(P);
  end;
  Finally
    FAVLCacheLock.Release;
  End;
end;

procedure TAVLCache<T>.ConsistencyCheck;
var LErrors : TStrings;
  LResult : Integer;
begin
  LErrors := TStringList.Create;
  Try
    LResult := FAVLCacheMem.ConsistencyCheck(LErrors);
  Finally
    LErrors.Free;
  End;
end;

constructor TAVLCache<T>.Create(ADefaultMax: Integer;  const AOnCompareMethod: TComparison<PAVLCacheMemData>);
begin
  FAVLCacheMem := TAVLCacheMem.Create(AOnCompareMethod,False);
  FDefaultMax := ADefaultMax;
  FAVLCacheLock := TCriticalSection.Create;
end;

destructor TAVLCache<T>.Destroy;
begin
  Clear;
  FAVLCacheMem.Free;
  FAVLCacheLock.Free;
  inherited Destroy;
end;

function TAVLCache<T>.Exists(const AData: T): Boolean;
var LFound : T;
begin
  Result := Find(AData,LFound);
end;

function TAVLCache<T>.Find(const AData: T; out AFound: T): Boolean;
var P, PFound: PAVLCacheMemData;
begin
  FAVLCacheLock.Acquire;
  Try
  New(P);
  try
    P^.Clear;
    P^.data := AData;
    PFound := FAVLCacheMem.Find(P);
    if Assigned(PFound) then begin
      AFound := PFound^.data;
      Result := True;
      FAVLCacheMem.DoMark(PFound,True);
    end else Result := False;
  finally
    Dispose(P);
  end;
  Finally
    FAVLCacheLock.Release;
  End;
end;

procedure TAVLCache<T>.Remove(const AData: T);
var P, PFound: PAVLCacheMemData;
begin
  FAVLCacheLock.Acquire;
  Try
  New(P);
  try
    P^.Clear;
    P^.data := AData;
    PFound := FAVLCacheMem.Find(P);
    if Assigned(PFound) then begin
      BeforeDelete(PFound^.data);
      FAVLCacheMem.DoMark(PFound,False);
      FAVLCacheMem.Delete(PFound);
    end;
  finally
    Dispose(P);
  end;
  Finally
    FAVLCacheLock.Release;
  End;
end;

function TAVLCache<T>.ToString(const AData: T): String;
begin
  Result := Self.ClassName+'.T '+IntToStr(SizeOf(AData));
end;

function TAVLCache<T>.TreeToString: String;
begin
  Result := FAVLCacheMem.ToString;
end;

{ TAVLCache<T>.TAVLCacheMemData }

procedure TAVLCache<T>.TAVLCacheMemData.Clear;
begin
  Self.parent := Nil;
  Self.left := Nil;
  Self.right := Nil;
  Self.balance := 0;
  Self.used_previous := Nil;
  Self.used_next := Nil;
  Self.pendingToSave := False;
end;

function TAVLCache<T>.TAVLCacheMemData.ToString: String;
begin
  Result := 'TAVLCache<T>.TAVLCacheMemData.'+IntToStr(SizeOf(Self.data));
end;

end.
