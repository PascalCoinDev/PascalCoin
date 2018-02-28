{
  Copyright (c) 2017 - 2018 Sphere 10 Software

  Common tools and extensions for Generics.Collections and collections in general usable
  across all tiers.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
    Herman Schoenfeld
}

unit UCommon.Collections;

{$mode delphi}

{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, UCommon;

type

  { Comparer API }

  // Note: this tries to follow pattern from Generics.Collections for supporting nested/object/global delegates.

  TNestedComparerFunc<T> = function (constref Left, Right: T): Integer is nested;

  TObjectComparerFunc<T> = function (constref Left, Right: T): Integer of object;

  TGlobalComparerFunc<T> = function (constref Left, Right: T): Integer;

  { TComparerTool }

  TComparerTool<T> = class
    private type
      __IComparer_T = IComparer<T>;
    public
      class function FromFunc(const AFunc: TNestedComparerFunc<T>) : IComparer<T>; overload;
      class function FromFunc(const AFunc: TObjectComparerFunc<T>) : IComparer<T>; overload;
      class function FromFunc(const AFunc: TGlobalComparerFunc<T>) : IComparer<T>; overload;
      class function Many(const comparers: array of TNestedComparerFunc<T>) : IComparer<T>; overload;
      class function Many(const comparers: array of TObjectComparerFunc<T>) : IComparer<T>; overload;
      class function Many(const comparers: array of TGlobalComparerFunc<T>) : IComparer<T>; overload;
      class function Many(const comparers: array of IComparer<T>) : IComparer<T>; overload;
      class function Many(const comparers: TEnumerable<__IComparer_T>) : IComparer<T>; overload;
      class function AlwaysEqual : IComparer<T>;
    private
      // These should be nested but FPC doesn't support nested functions in generics
      class function AlwaysEqualHandler(constref Left, Right: T) : Integer;
  end;

  { Predicate API }

  // Note: the pattern for nested/object/global delegates is custom

  TNestedPredicateFunc<T> = function (constref AVal : T) : boolean is nested;

  TObjectPredicateFunc<T> = function (constref AVal : T) : boolean of object;

  TGlobalPredicateFunc<T> = function (constref AVal : T) : boolean;

  IPredicate<T> = interface
    function Evaluate (constref AValue: T) : boolean;
  end;

  TPredicateTool<T> = class
    private type
      __IPredicate_T = IPredicate<T>;
    public
      class function FromFunc(const AFunc: TNestedPredicateFunc<T>) : IPredicate<T>; overload;
      class function FromFunc(const AFunc: TObjectPredicateFunc<T>) : IPredicate<T>; overload;
      class function FromFunc(const AFunc: TGlobalPredicateFunc<T>) : IPredicate<T>; overload;
      class function AndMany(const APredicates : array of IPredicate<T>) : IPredicate<T>; overload;
      class function AndMany(const APredicates : array of TNestedPredicateFunc<T>) : IPredicate<T>; overload;
      class function AndMany(const APredicates : array of TObjectPredicateFunc<T>) : IPredicate<T>; overload;
      class function AndMany(const APredicates : array of TGlobalPredicateFunc<T>) : IPredicate<T>; overload;
      class function OrMany(const APredicates : array of IPredicate<T>) : IPredicate<T>; overload;
      class function OrMany(const APredicates : array of TNestedPredicateFunc<T>) : IPredicate<T>; overload;
      class function OrMany(const APredicates : array of TObjectPredicateFunc<T>) : IPredicate<T>; overload;
      class function OrMany(const APredicates : array of TGlobalPredicateFunc<T>) : IPredicate<T>; overload;
      class function TruePredicate : IPredicate<T>;
      class function FalsePredicate : IPredicate<T>;
      class function NegatePredicate(const APredicate : IPredicate<T>) : IPredicate<T>;
    private
      // These should be nested but FPC doesn't support nested functions in generics
      class function TrueHandler(constref AItem: T) : boolean;
      class function FalseHandler(constref AItem: T) : boolean;
  end;

  { TListTool }

  TListTool<T> = class
    class function Copy(const AList: TList<T>; const AIndex, ACount : SizeInt) : TList<T>;
    class function Range(const AList: TList<T>; const AIndex, ACount : SizeInt) : SizeInt;
    class function Skip(const AList: TList<T>; const ACount : SizeInt) : SizeInt;
    class function Take(const AList: TList<T>; const ACount : SizeInt) : SizeInt;
    class function RemoveBy(const AList: TList<T>; const APredicate: IPredicate<T>) : SizeInt; overload;
    class function RemoveBy(const AList: TList<T>; const APredicate: IPredicate<T>; const ADisposePolicy : TItemDisposePolicy) : SizeInt; overload;
    class function FilterBy(const AList: TList<T>; const APredicate: IPredicate<T>) : SizeInt; overload;
    class function FilterBy(const AList: TList<T>; const APredicate: IPredicate<T>; const ADisposePolicy : TItemDisposePolicy) : SizeInt; overload;
    class procedure DiposeItem(const AList: TList<T>; const index : SizeInt; const ADisposePolicy : TItemDisposePolicy);
  end;

  { Private types (implementation only) - FPC Bug 'Global Generic template references static symtable' }

  { TNestedComparer }

  TNestedComparer<T> = class(TInterfacedObject, IComparer<T>)
   private
     FFunc: TNestedComparerFunc<T>;
   public
     constructor Create(const AComparerFunc: TNestedComparerFunc<T>); overload;
     function Compare(constref Left, Right: T): Integer;
  end;

  { TObjectComparer }

  TObjectComparer<T> = class(TInterfacedObject, IComparer<T>)
   private
     FFunc: TObjectComparerFunc<T>;
   public
     constructor Create(const AComparerFunc: TObjectComparerFunc<T>); overload;
     function Compare(constref Left, Right: T): Integer;
  end;

  { TGlobalComparer }

  TGlobalComparer<T> = class(TInterfacedObject, IComparer<T>)
   private
     FFunc: TGlobalComparerFunc<T>;
   public
     constructor Create(const AComparerFunc: TGlobalComparerFunc<T>); overload;
     function Compare(constref Left, Right: T): Integer;
  end;

  { TManyComparer }

  TManyComparer<T> = class(TInterfacedObject, IComparer<T>)
     private type
       IComparer_T = IComparer<T>;
     private
       FComparers : TArray<IComparer_T>;
     public
       constructor Create(const comparers: TArray<IComparer_T>); overload;
       function Compare(constref Left, Right: T): Integer;
   end;

  { TNestedPredicate }

  TNestedPredicate<T> = class (TInterfacedObject, IPredicate<T>)
   private
     FFunc : TNestedPredicateFunc<T>;
   public
     constructor Create(const AFunc: TNestedPredicateFunc<T>); overload;
     function Evaluate (constref AValue: T) : boolean;
  end;

  { TObjectPredicate }

  TObjectPredicate<T> = class (TInterfacedObject, IPredicate<T>)
   private
     FFunc : TObjectPredicateFunc<T>;
   public
     constructor Create(const AFunc: TObjectPredicateFunc<T>); overload;
     function Evaluate (constref AValue: T) : boolean;
  end;

  { TGlobalPredicate }

  TGlobalPredicate<T> = class (TInterfacedObject, IPredicate<T>)
   private
     FFunc : TGlobalPredicateFunc<T>;
   public
     constructor Create(const AFunc: TGlobalPredicateFunc<T>); overload;
     function Evaluate (constref AValue: T) : boolean;
  end;

  { TNotPredicate }

  TNotPredicate<T> = class (TInterfacedObject, IPredicate<T>)
    private
      FPredicate : IPredicate<T>;
    public
      constructor Create(const APredicate : IPredicate<T>); overload;
      function Evaluate (constref AValue: T) : boolean;
  end;

  { TAndManyPredicate }

  TAndManyPredicate<T> = class (TInterfacedObject, IPredicate<T>)
   private type
     __IPredicate_T = IPredicate<T>;
     __TArrayTool = TArrayTool<__IPredicate_T>;
   private
     FPredicates : TArray<__IPredicate_T>;
   public
     constructor Create(const APredicates: TArray<__IPredicate_T>); overload;
     function Evaluate (constref AValue: T) : boolean;
  end;

  { TOrManyPredicate }

  TOrManyPredicate<T> =  class (TInterfacedObject, IPredicate<T>)
   private type
     __IPredicate_T = IPredicate<T>;
     __TArrayTool = TArrayTool<__IPredicate_T>;
   private
     FPredicates : TArray<__IPredicate_T>;
   public
     constructor Create(const APredicates: TArray<__IPredicate_T>); overload;
     function Evaluate (constref AValue: T) : boolean;
  end;


implementation

{%region Comparer API}

class function TComparerTool<T>.FromFunc(const AFunc: TNestedComparerFunc<T>) : IComparer<T>;
begin
  Result := TNestedComparer<T>.Create(AFunc);
end;

class function TComparerTool<T>.FromFunc(const AFunc: TObjectComparerFunc<T>) : IComparer<T>;
begin
  Result := TObjectComparer<T>.Create(AFunc);
end;

class function TComparerTool<T>.FromFunc(const AFunc: TGlobalComparerFunc<T>) : IComparer<T>;
begin
  Result := TGlobalComparer<T>.Create(AFunc);
end;

class function TComparerTool<T>.Many(const comparers: array of TNestedComparerFunc<T>) : IComparer<T>;
var
  i : Integer;
  internalComparers : TArray<__IComparer_T>;
begin
  SetLength(internalComparers, Length(comparers));
  for i := 0 to High(comparers) do
    internalComparers[i] := TNestedComparer<T>.Create(comparers[i]);
  Result := TManyComparer<T>.Create(internalComparers);
end;

class function TComparerTool<T>.Many(const comparers: array of TObjectComparerFunc<T>) : IComparer<T>;
var
  i : Integer;
  internalComparers : TArray<__IComparer_T>;
begin
  SetLength(internalComparers, Length(comparers));
  for i := 0 to High(comparers) do
    internalComparers[i] := TObjectComparer<T>.Create(comparers[i]);
  Result := TManyComparer<T>.Create(internalComparers);
end;

class function TComparerTool<T>.Many(const comparers: array of TGlobalComparerFunc<T>) : IComparer<T>;
var
  i : Integer;
  internalComparers : TArray<__IComparer_T>;
begin
  SetLength(internalComparers, Length(comparers));
  for i := 0 to High(comparers) do
    internalComparers[i] := TGlobalComparer<T>.Create(comparers[i]);
  Result := TManyComparer<T>.Create(internalComparers);
end;

class function TComparerTool<T>.Many(const comparers: array of IComparer<T>) : IComparer<T>;
type
  __TArrayTool_IComparer_T = TArrayTool<__IComparer_T>;
begin
  Result := TManyComparer<T>.Create( __TArrayTool_IComparer_T.Copy(comparers) );
end;

class function TComparerTool<T>.Many(const comparers: TEnumerable<__IComparer_T>) : IComparer<T>;
var
  i : integer;
  comparer : __IComparer_T;
  internalComparers : TArray<__IComparer_T>;
begin
  for comparer in comparers do begin
    SetLength(internalComparers, Length(internalComparers) + 1);
    internalComparers[High(internalComparers)] := comparer;
  end;
  Result := TManyComparer<T>.Create(internalComparers);
end;

class function TComparerTool<T>.AlwaysEqual : IComparer<T>;
type
  __TGlobalComparerFunc_T = TGlobalComparerFunc<T>;
begin
  Result :=  TComparerTool<T>.FromFunc( AlwaysEqualHandler );
end;

class function TComparerTool<T>.AlwaysEqualHandler(constref Left, Right: T) : Integer;
begin
  Result := 0;
end;

{ TNestedComparer }

constructor TNestedComparer<T>.Create(const AComparerFunc: TNestedComparerFunc<T>);
begin
  FFunc := AComparerFunc;
end;

function TNestedComparer<T>.Compare(constref Left, Right: T): Integer;
begin
  Result := FFunc(Left, Right);
end;

{ TObjectComparer }

constructor TObjectComparer<T>.Create(const AComparerFunc: TObjectComparerFunc<T>);
begin
  FFunc := AComparerFunc;
end;

function TObjectComparer<T>.Compare(constref Left, Right: T): Integer;
begin
  Result := FFunc(Left, Right);
end;

{ TGlobalComparer }

constructor TGlobalComparer<T>.Create(const AComparerFunc: TGlobalComparerFunc<T>);
begin
  FFunc := AComparerFunc;
end;

function TGlobalComparer<T>.Compare(constref Left, Right: T): Integer;
begin
  Result := FFunc(Left, Right);
end;

{ TManyComparer }

constructor TManyComparer<T>.Create(const comparers: TArray<IComparer_T>);
begin
  FComparers := comparers;
end;

function TManyComparer<T>.Compare(constref Left, Right: T): Integer;
var
  i : Integer;
begin
  if Length(FComparers) = 0 then
    raise Exception.Create('No comparers defined');
  for i := Low(FComparers) to High(FComparers) do begin
    Result := FComparers[i].Compare(Left, Right);
    if (Result <> 0) or (i = High(FComparers)) then exit;
  end;
end;


{%endegion}

{%region Predicate API}

{ TPredicateTool }

class function TPredicateTool<T>.FromFunc(const AFunc: TNestedPredicateFunc<T>) : IPredicate<T>;
begin
  Result := TNestedPredicate<T>.Create(AFunc);
end;

class function TPredicateTool<T>.FromFunc(const AFunc: TObjectPredicateFunc<T>) : IPredicate<T>;
begin
  Result := TObjectPredicate<T>.Create(AFunc);
end;

class function TPredicateTool<T>.FromFunc(const AFunc: TGlobalPredicateFunc<T>) : IPredicate<T>;
begin
  Result := TGlobalPredicate<T>.Create(AFunc);
end;

class function TPredicateTool<T>.AndMany(const APredicates : array of IPredicate<T>) : IPredicate<T>;
type
  __TArrayTool_IPredicate_T = TArrayTool<__IPredicate_T>;
var
  arr : TArray<__IPredicate_T>;
  i : Integer;
begin
  SetLength(arr, Length(APredicates));
  for i := 0 to High(APredicates) do
    arr[i] := APredicates[i];
  //arr := __TArrayTool_IPredicate_T.Copy( APredicates);  // TODO: fix ArrayTool.Copy
  Result := TAndManyPredicate<T>.Create( arr );
end;

class function TPredicateTool<T>.AndMany(const APredicates : array of TNestedPredicateFunc<T>) : IPredicate<T>;
var
  i : integer;
  arr : TArray<__IPredicate_T>;
begin
  SetLength(arr, Length(APredicates));
  for i := Low(APredicates) to High(APredicates) do
    arr[i - Low(APredicates)] := TPredicateTool<T>.FromFunc(APredicates[i]);
  Result := AndMany(arr);
end;

class function TPredicateTool<T>.AndMany(const APredicates : array of TObjectPredicateFunc<T>) : IPredicate<T>;
var
  i : integer;
  arr : TArray<__IPredicate_T>;
begin
  SetLength(arr, Length(APredicates));
  for i := Low(APredicates) to High(APredicates) do
    arr[i - Low(APredicates)] := TPredicateTool<T>.FromFunc(APredicates[i]);
  Result := AndMany(arr);
end;

class function TPredicateTool<T>.AndMany(const APredicates : array of TGlobalPredicateFunc<T>) : IPredicate<T>;
var
  i : integer;
  arr : TArray<__IPredicate_T>;
begin
  SetLength(arr, Length(APredicates));
  for i := Low(APredicates) to High(APredicates) do
    arr[i - Low(APredicates)] := TPredicateTool<T>.FromFunc(APredicates[i]);
  Result := AndMany(arr);
end;

class function TPredicateTool<T>.OrMany(const APredicates : array of IPredicate<T>) : IPredicate<T>;
type
  __TArrayTool_IPredicate_T = TArrayTool<__IPredicate_T>;
begin
  Result := TOrManyPredicate<T>.Create( __TArrayTool_IPredicate_T.Copy( APredicates) );
end;

class function TPredicateTool<T>.OrMany(const APredicates : array of TNestedPredicateFunc<T>) : IPredicate<T>;
var
  i : integer;
  arr : TArray<__IPredicate_T>;
begin
  SetLength(arr, Length(APredicates));
  for i := Low(APredicates) to High(APredicates) do
    arr[i - Low(APredicates)] := TPredicateTool<T>.FromFunc(APredicates[i]);
  Result := OrMany(arr);
end;

class function TPredicateTool<T>.OrMany(const APredicates : array of TObjectPredicateFunc<T>) : IPredicate<T>;
var
  i : integer;
  arr : TArray<__IPredicate_T>;
begin
  SetLength(arr, Length(APredicates));
  for i := Low(APredicates) to High(APredicates) do
    arr[i - Low(APredicates)] := TPredicateTool<T>.FromFunc(APredicates[i]);
  Result := OrMany(arr);
end;

class function TPredicateTool<T>.OrMany(const APredicates : array of TGlobalPredicateFunc<T>) : IPredicate<T>;
var
  i : integer;
  arr : TArray<__IPredicate_T>;
begin
  SetLength(arr, Length(APredicates));
  for i := Low(APredicates) to High(APredicates) do
    arr[i - Low(APredicates)] := TPredicateTool<T>.FromFunc(APredicates[i]);
  Result := OrMany(arr);
end;

class function TPredicateTool<T>.TruePredicate : IPredicate<T>;
begin
  Result := TPredicateTool<T>.FromFunc(TrueHandler);
end;

class function TPredicateTool<T>.FalsePredicate : IPredicate<T>;
begin
  Result := TPredicateTool<T>.FromFunc(FalseHandler);
end;

class function TPredicateTool<T>.NegatePredicate(const APredicate : IPredicate<T>) : IPredicate<T>;
begin
  Result := TNotPredicate<T>.Create(APredicate);
end;

// Shold be nested funcion but generics can't have in FPC!
class function TPredicateTool<T>.TrueHandler(constref AItem: T) : boolean;
begin
  Result := true;
end;

// Shold be nested funcion but generics can't have in FPC!
class function TPredicateTool<T>.FalseHandler(constref AItem: T) : boolean;
begin
  Result := true;
end;

{ TNestedPredicate }

constructor TNestedPredicate<T>.Create(const AFunc: TNestedPredicateFunc<T>);
begin
  FFunc := AFunc;
end;

function TNestedPredicate<T>.Evaluate (constref AValue: T) : boolean;
begin
  Result := FFunc(AValue);
end;

{ TObjectPredicate }

constructor TObjectPredicate<T>.Create(const AFunc: TObjectPredicateFunc<T>);
begin
  FFunc := AFunc;
end;

function TObjectPredicate<T>.Evaluate (constref AValue: T) : boolean;
begin
  Result := FFunc(AValue);
end;

{ TGlobalPredicate }

constructor TGlobalPredicate<T>.Create(const AFunc: TGlobalPredicateFunc<T>);
begin
  FFunc := AFunc;
end;

function TGlobalPredicate<T>.Evaluate (constref AValue: T) : boolean;
begin
  Result := FFunc(AValue);
end;

{ TNotPredicate }

constructor TNotPredicate<T>.Create(const APredicate: IPredicate<T>);
begin
  FPredicate := APredicate;
end;

function TNotPredicate<T>.Evaluate (constref AValue: T) : boolean;
begin
  Result := NOT FPredicate.Evaluate(AValue);
end;

{ TAndManyPredicate }

constructor TAndManyPredicate<T>.Create(const APredicates: TArray<__IPredicate_T>);
begin
  if Length(APredicates) < 2 then
    raise EArgumentException.Create('APredicates Must contain at least 2 predicates');

  FPredicates := APredicates;
end;

function TAndManyPredicate<T>.Evaluate (constref AValue: T) : boolean;
var
  i : integer;
begin
  Result := FPredicates[0].Evaluate(AValue);
  for i := 1 to High(FPredicates) do begin
    if NOT Result then
      exit;
    Result := Result AND FPredicates[i].Evaluate(AValue);
  end;
end;

{ TOrManyPredicate }

constructor TOrManyPredicate<T>.Create(const APredicates: TArray<__IPredicate_T>);
begin
  if Length(APredicates) < 2 then
    raise EArgumentException.Create('APredicates Must contain at least 2 predicates');

  FPredicates := APredicates;
end;

function TOrManyPredicate<T>.Evaluate (constref AValue: T) : boolean;
var
  i : integer;
begin
  Result := FPredicates[0].Evaluate(AValue);
  for i := 1 to High(FPredicates) do begin
    if Result then
      exit;
    Result := Result OR FPredicates[i].Evaluate(AValue);
  end;
end;

{%endregion}

{%region TListTool}

class function TListTool<T>.Copy(const AList: TList<T>; const AIndex, ACount : SizeInt) : TList<T>;
var
  i : Integer;
begin
  Result := TList<T>.Create;

  for i := 0 to ACount do
      Result.Add(AList[AIndex + i]);

end;

class function TListTool<T>.Range(const AList: TList<T>; const AIndex, ACount : SizeInt) : SizeInt;
var
  from, to_, listCount : SizeInt;
begin

  listCount := AList.Count;

  from := ClipValue(AIndex, 0, listCount - 1);
  to_ := ClipValue(AIndex + ACount, 0, listCount - 1);

  if to_ <= from then begin
    Result := 0;
    exit;
  end;

  if from > 0 then
    AList.DeleteRange(0, from);

  if to_ < (listCount - 1) then
    AList.DeleteRange(to_ - from, AList.Count);

  Result := AList.Count - listCount;
end;

class function TListTool<T>.Skip(const AList: TList<T>; const ACount : SizeInt) : SizeInt;
begin
  Result := Range(AList, 0 + ACount, AList.Count - ACount);
end;

class function TListTool<T>.Take(const AList: TList<T>; const ACount : SizeInt) : SizeInt;
begin
  Result := Range(AList, 0, ACount);
end;

class function TListTool<T>.RemoveBy(const AList: TList<T>; const APredicate: IPredicate<T>) : SizeInt;
begin
  Result := RemoveBy(AList, APredicate, idpNone);
end;

class function TListTool<T>.RemoveBy(const AList: TList<T>; const APredicate: IPredicate<T>; const ADisposePolicy : TItemDisposePolicy) : SizeInt;
var
  i : SizeInt;
  item : T;
begin
  Result := 0;
  i := AList.Count-1;
  while i >= 0 do begin
    item := AList[i];
    if APredicate.Evaluate(item) then begin
      DiposeItem(AList, i, ADisposePolicy);
      AList.Delete(i);
      inc(Result);
    end;
    Dec(i);
  end;
end;

class function TListTool<T>.FilterBy(const AList: TList<T>; const APredicate: IPredicate<T>) : SizeInt;
begin
  Result := FilterBy(AList, APredicate, idpNone);
end;

class function TListTool<T>.FilterBy(const AList: TList<T>; const APredicate: IPredicate<T>; const ADisposePolicy : TItemDisposePolicy) : SizeInt;
begin
  Result := RemoveBy(AList, TPredicateTool<T>.NegatePredicate ( APredicate ) );
end;

class procedure TListTool<T>.DiposeItem(const AList: TList<T>; const index : SizeInt; const ADisposePolicy : TItemDisposePolicy);
var
  item : T;
begin
  item := AList[index];
  case ADisposePolicy of
    idpNone: ;
    idpNil: AList[index] := default(T);
    idpFreeAndNil: begin
      item := AList[index];
      FreeAndNil(item);
      AList[index] := default(T);
    end
    else raise ENotSupportedException(Format('TItemDisposePolicy: [%d]', [Ord(ADisposePolicy)]));
  end;
end;

{%endregion}

end.

