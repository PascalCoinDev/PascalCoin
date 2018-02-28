unit UCache;

{$ifdef FPC}
  {$mode Delphi}
{$endif}

{$modeswitch nestedprocvars}

interface

uses
  syncobjs, Classes, SysUtils, UCommon, Generics.Collections, LCLIntf;

type
  TCachedItem<TValue> = class(TInterfacedObject)
    public
      FInvalidated : boolean;
      FFetchedOn : TDateTime;
      FLastAccessedOn : TDateTime;
      FAccessedCount : DWord;
      FSize : DWord;
      FDisposePolicy : TItemDisposePolicy;
      FValue : TValue;
    public
      constructor Create(const AValue : TValue; ASize: DWord; ADisposePolicy : TItemDisposePolicy);
      destructor Destroy; override;
      property Invalidated : boolean read FInvalidated;
      property FetchedOn : TDateTime read FFetchedOn;
      property LastAccessedOn : TDateTime read FLastAccessedOn;
      property AccessedCount : DWord read FAccessedCount;
      property Size : DWord read FSize;
      property Value : TValue read FValue;
  end;

  TCacheBase<TKey, TValue> = class(TComponent)
    public type
        TExpirationPolicy = (epSinceFetchedTime, epSinceLastAccessedTime, epNone);
        TCacheReapPolicy = (crpLeastUsed, crpLargest, crpSmallest, crpOldest, crpIdle, crpASAP, crpNone);
        TNullValuePolicy = (nvpCacheNormally, nvpReturnButDontCache, nvpThrow);
    private type
       __TPair_TKey_TValue = TPair<TKey, TValue>;
       __TCachedItem_TValue = TCachedItem<TValue>;
    private
      FItemFetched: TNotifyManyEventEx; { Args = TKey, TValue }
      FItemRemoved: TNotifyManyEventEx; { Args = TKey, TValue }
      function ByExpiredCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
      function ByAccessedCountCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
      function ByIdleCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
      function ByIdleDescendingCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
      function ByFetchedOnCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
      function ByAccessedOnCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
      function BySizeCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
      function BySizeDescendingCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
      function IsExpiredFilter(const Item : __TPair_TKey_TValue) : boolean;
    protected type
      __TDictionary_TKey_TCachedItem_TValue = TDictionary<TKey, __TCachedItem_TValue>;
    protected
      FCurrentSize : SizeInt;
      FMaxCapacity : SizeInt;
      FItemDisposePolicy : TItemDisposePolicy;
      FExpirationDuration : TTimeSpan;
      FExpirationPolicy : TExpirationPolicy;
      FReapPolicy : TCacheReapPolicy;
      FNullValuePolicy : TNullValuePolicy;
      FInternalStorage : __TDictionary_TKey_TCachedItem_TValue;
      FLock : TSimpleRWSync;
      function EstimateSize(const AVal: TValue) : SizeInt; virtual; abstract;
      function Fetch(const AKey: TKey): TValue; virtual; abstract;
      function IsExpired(ACachedItem: TCachedItem<TValue>) : boolean;
      procedure MakeSpace(const requestedSpace: SizeInt);
      procedure MakeSpaceFast(const requestedSpace: SizeInt);
      procedure OnItemFetched(const AKey: TKey; const AVal: TValue); virtual;
      procedure OnItemRemoved(const AKey: TKey; const AVal: TValue); virtual;
      procedure NotifyItemFetched(const AKey: TKey; const AVal: TValue);
      procedure NotifyItemRemoved(const AKey: TKey; const AVal: TValue);
      function AddItemInternal(const Key: TKey; const AVal: TValue) : TCachedItem<TValue>;
      procedure RemoveItemInternal(const AKey: TKey);
    public
      constructor Create(AOwner:TComponent; const AItemDisposePolicy : TItemDisposePolicy; const AReapPolicy : TCacheReapPolicy; const AExpirationPolicy : TExpirationPolicy; const ANullValuePolicy : TNullValuePolicy; const AMaxCapacity : Cardinal; const AExpirationDuration : TTimeSpan); overload;
      destructor Destroy; override;
      property ItemFetched : TNotifyManyEventEx read FItemFetched;
      property ItemRemoved : TNotifyManyEventEx read FItemRemoved;
      property CurrentSize : SizeInt read FCurrentSize;
      property MaxCapacity : SizeInt read FMaxCapacity;
      property ExpirationDuration : TTimeSpan read FExpirationDuration;
      property ExpirationPolicy : TExpirationPolicy read FExpirationPolicy;
      property ReapPolicy : TCacheReapPolicy read FReapPolicy;
      property NullValuePolicy : TNullValuePolicy read FNullValuePolicy;
      function Get(const AKey: TKey) : TCachedItem<TValue>;
      function ContainsCachedItem(const AKey : TKey) : boolean;
      procedure BulkLoad(const ABulkValues : TEnumerable<__TPair_TKey_TValue>);
      procedure Invalidate(const AKey: TKey);
      procedure Remove(const AKey: TKey);
      procedure Flush;
  end;

{ TActionCache }

  TActionCache<TKey, TValue> = class(TCacheBase<TKey, TValue>)
    public type
      { IValueFetcher }
      IValueFetcher = interface
        function Fetch(const AKey: TKey): TValue;
      end;

      { ISizeEstimator }
      ISizeEstimator = interface
        function Estimate(const AKey: TKey) : SizeInt;
      end;

      { TNestedFetchFunc }
      TNestedFetchFunc = function (const AKey : TKey) : TValue is nested;

      { TObjectFetchFunc }
      TObjectFetchFunc = function (const AKey : TKey) : TValue of object;

      { TGlobalFetchFunc }
      TGlobalFetchFunc = function (const AKey : TKey) : TValue;

      { TValueFetcher }
      TValueFetcher = class (TInterfacedObject, IValueFetcher)
        private
          FFetcher : IValueFetcher;
        public
          function Fetch(const AKey: TKey): TValue;
          constructor Create(const AFetcher: IValueFetcher);
          class function FromNestedFunction(const AFunc: TNestedFetchFunc) : IValueFetcher;
          class function FromObjectFunction(const AFunc: TObjectFetchFunc) : IValueFetcher;
          class function FromGlobalFunction(const AFunc: TGlobalFetchFunc) : IValueFetcher;
      end;

      { TNestedEstimateFunc }
      TNestedEstimateFunc = function (const AKey : TKey) : SizeInt is nested;

      { TObjectEstimateFunc }
      TObjectEstimateFunc = function (const AKey : TKey) : SizeInt of object;

      { TGlobalEstimateFunc }
      TGlobalEstimateFunc = function (const AKey : TKey) : SizeInt;

      { TSizeEstimator }
      TSizeEstimator = class (TInterfacedObject, ISizeEstimator)
        private
          FEstimator : ISizeEstimator;
        public
          function Estimate(const AKey: TKey) : SizeInt;
          constructor Create(const AEstimator: ISizeEstimator);
          class function FromNestedFunction(const AFunc: TNestedEstimateFunc) : ISizeEstimator;
          class function FromObjectFunction(const AFunc: TObjectEstimateFunc) : ISizeEstimator;
          class function FromGlobalFunction(const AFunc: TGlobalEstimateFunc) : ISizeEstimator;
      end;

   private type
     { GlobalFunctionValueFetcher }
     GlobalFunctionValueFetcher = class (TInterfacedObject, IValueFetcher)
        private
          FFunc : TGlobalFetchFunc;
        public
          constructor Create(const AFunc: TGlobalFetchFunc);
          function Fetch(const AKey: TKey): TValue;
      end;

      { ObjectFunctionValueFetcher }
      ObjectFunctionValueFetcher = class (TInterfacedObject, IValueFetcher)
        private
          FFunc : TObjectFetchFunc;
        public
          constructor Create(const AFunc: TGlobalFetchFunc);
          function Fetch(const AKey: TKey): TValue;
      end;

      { NestedFunctionValueFetcher }
      NestedFunctionValueFetcher = class (TInterfacedObject, IValueFetcher)
        private
          FFunc : TNestedFetchFunc;
        public
          constructor Create(const AFunc: TNestedFetchFunc);
          function Fetch(const AKey: TKey): TValue;
      end;

     { GlobalSizeEstimator }
     GlobalFunctionEstimator = class (TInterfacedObject, ISizeEstimator)
        private
          FFunc : TGlobalEstimateFunc;
        public
          constructor Create(const AFunc: TGlobalEstimateFunc);
          function Estimate(const AKey: TKey): SizeInt;
      end;

      { ObjectFunctionValueFetcher }
      ObjectFunctionEstimator = class (TInterfacedObject, ISizeEstimator)
        private
          FFunc : TObjectEstimateFunc;
        public
          constructor Create(const AFunc: TObjectEstimateFunc);
          function Estimate(const AKey: TKey): SizeInt;
      end;

      { NestedFunctionValueFetcher }
      NestedFunctionEstimator = class (TInterfacedObject, ISizeEstimator)
        private
          FFunc : TNestedEstimateFunc;
        public
          constructor Create(const TNestedEstimateFunc);
          function Estimate(const AKey: TKey): SizeInt;
      end;

    private
      FValueFetcher : IValueFetcher;
      FSizeEstimator : ISizeEstimator;
    protected
      function EstimateSize(const AVal: TValue) : SizeInt; override;
      function Fetch(const AKey: TKey): TValue; override;
    public
      constructor Create(
            AOwner:TComponent;
            const AValueFetcher : IValueFetcher;
            const ASizeEstimator : ISizeEstimator;
            const AItemDisposePolicy : TItemDisposePolicy;
            const AReapPolicy : TCacheReapPolicy;
            const AExpirationPolicy : TExpirationPolicy;
            const ANullPolicy : TNullValuePolicy;
            const AMaxCapacity : Integer;
            const AExpirationDuration : TTimeSpan); overload;
  end;

implementation

uses
  Generics.Defaults, UAutoScope, UCommon.Collections;


{ TCachedItem }

constructor TCachedItem<TValue>.Create(const AValue : TValue; ASize: DWord; ADisposePolicy : TItemDisposePolicy);
begin
  FInvalidated := false;
  FFetchedOn := Now;
  FLastAccessedOn := Now;
  FFetchedOn := Now;
  FAccessedCount := 0;
  FSize := ASize;
  FDisposePolicy := TItemDisposePolicy;
  FValue :=  AValue;
end;

destructor TCachedItem<TValue>.Destroy;
begin
  case FDisposePolicy of
    idpFreeAndNil: FreeAndNil(FValue);
    idpNil: FValue := nil;
    idpNone: // do nothing
  end;
  inherited;
end;

{ TCacheBase }

constructor TCacheBase<TKey, TValue>.Create(AOwner:TComponent; const AItemDisposePolicy : TItemDisposePolicy; const AReapPolicy : TCacheReapPolicy; const AExpirationPolicy : TExpirationPolicy; const ANullValuePolicy : TNullValuePolicy; const AMaxCapacity : Cardinal; const AExpirationDuration : TTimeSpan);
begin
  inherited Create(AOwner);
  FItemDisposePolicy := AItemDisposePolicy;
  FNullValuePolicy := ANullValuePolicy;
  FInternalStorage := __TDictionary_TKey_TCachedItem_TValue.Create;
  FCurrentSize := 0;
  FReapPolicy := AReapPolicy;
  FExpirationPolicy := AExpirationPolicy;
  FMaxCapacity := AMaxCapacity;
  FExpirationDuration := AExpirationDuration;
  FLock := TSimpleRWSync.Create;
end;

destructor TCacheBase<TKey, TValue>.Destroy;
begin
  Flush;
  FreeAndNil(FInternalStorage);
  FreeAndNil(FLock);
  inherited;
end;

function TCacheBase<TKey, TValue>.Get(const AKey: TKey) : TCachedItem<TValue>;
var
  item: TCachedItem<TValue>;
begin
  if NOT FInternalStorage.TryGetValue(AKey, item) then begin
      try
        FLock.BeginWrite;
        if NOT FInternalStorage.TryGetValue(AKey, item) then
            item := AddItemInternal(AKey, Fetch(AKey));
      finally
        FLock.EndWrite;
      end
  end else if IsExpired(item) then begin
    try
      FLock.BeginWrite;
      RemoveItemInternal(AKey);
      item := AddItemInternal(AKey, Fetch(AKey));
    finally
      FLock.EndWrite;
    end
  end;
  inc(item.FAccessedCount);
  item.FLastAccessedOn := Now;
  if not Assigned(item.Value) then begin
    case NullValuePolicy of
      nvpThrow: raise Exception.Create('Cache fetched a null value and this cache NullValuePolicy prohibits null values.');
      nvpReturnButDontCache: Invalidate(AKey);
      nvpCacheNormally: ;
    end
  end;
  Result := item;
end;

function TCacheBase<TKey, TValue>.ContainsCachedItem(const AKey : TKey) : boolean;
var
 item : TCachedItem<TValue>;
begin
  Result := FInternalStorage.TryGetValue(AKey, item) AND (NOT IsExpired(item));
end;

function TCacheBase<TKey, TValue>.IsExpired(ACachedItem: TCachedItem<TValue>) : boolean;
var
  from : TDateTime;
begin
  if ACachedItem.Invalidated then begin
    Result := true;
    exit;
  end;

  case FExpirationPolicy of
    epSinceFetchedTime: from := ACachedItem.FetchedOn;
    epSinceLastAccessedTime: from := ACachedItem.LastAccessedOn;
    else from := Now;
  end;

  Result := (FExpirationPolicy <> epNone) AND (TTimeSpan.Subtract(Now, from) > FExpirationDuration);
end;

procedure TCacheBase<TKey, TValue>.Remove(const AKey: TKey);
begin
  try
    FLock.BeginWrite;
    RemoveItemInternal(AKey);
  finally
    FLock.EndWrite;
  end;
end;

procedure TCacheBase<TKey, TValue>.Flush;
var
  key: TKey;
begin
  try
    FLock.BeginWrite;
    for key in FInternalStorage.Keys do
      RemoveItemInternal(FInternalStorage[key]);
    FInternalStorage.Clear;
  finally
    FLock.EndWrite;
  end
end;

procedure TCacheBase<TKey, TValue>.BulkLoad(const ABulkValues: TEnumerable<__TPair_TKey_TValue>);
var
  kvp: __TPair_TKey_TValue;
begin
  try
    FLock.BeginWrite;
    for kvp in ABulkValues do begin
      if FInternalStorage.ContainsKey(kvp.Key) then begin
        RemoveItemInternal(kvp.Key);
        AddItemInternal(kvp.Key, kvp.Value);
      end else AddItemInternal(kvp.Key, kvp.Value);
    end
  finally
    FLock.EndWrite;
  end
end;

procedure TCacheBase<TKey, TValue>.Invalidate;
begin
  raise Exception.Create('Not implemented');
end;


procedure TCacheBase<TKey, TValue>.OnItemFetched(const AKey : TKey; const AVal: TValue);
begin
end;

procedure TCacheBase<TKey, TValue>.OnItemRemoved(const AKey : TKey; const AVal: TValue);
begin
end;

procedure TCacheBase<TKey, TValue>.MakeSpace(const requestedSpace: SizeInt);
type
  TSortFunc = TOnComparison<__TPair_TKey_TValue>;
var
  candidate : __TPair_TKey_TValue;
  deathRow: TList<__TPair_TKey_TValue>;
  sortOrder: array of TSortFunc;
  reapAll: boolean;
  GC : TScoped;
begin
  try
    FLock.BeginWrite;
    if requestedSpace > FMaxCapacity then
      raise Exception.Create(Format('Cache capacity insufficient for requested space [%0:d].',[requestedSpace]));

    // If need space
    if FReapPolicy = crpASAP then begin
      MakeSpaceFast(requestedSpace);
      exit;
    end;

    // Get elements to be purged (deathrow)
    deathRow := GC.AddObject(TList<__TPair_TKey_TValue>.Create) as TList<__TPair_TKey_TValue>;
    deathRow.AddRange(FInternalStorage.ToArray);
    case FReapPolicy of
      crpLeastUsed: sortOrder := TArrayTool<TSortFunc>.Create(ByExpiredCompare, ByAccessedCountCompare, BySizeDescendingCompare);
      crpOldest: sortOrder := TArrayTool<TSortFunc>.Create(ByExpiredCompare, ByFetchedOnCompare);
      crpIdle: sortOrder := TArrayTool<TSortFunc>.Create(ByExpiredCompare, ByAccessedOnCompare);
      crpLargest: sortOrder := TArrayTool<TSortFunc>.Create(ByExpiredCompare, BySizeDescendingCompare);
      crpSmallest: sortOrder := TArrayTool<TSortFunc>.Create(ByExpiredCompare, BySizeCompare);
      crpNone: begin
        if FExpirationPolicy <> epNone then begin
          TListTool<__TPair_TKey_TValue>.Filter(deathRow, TPredicateTool<__TPair_TKey_TValue>.FromFunc(IsExpiredFilter));
          sortOrder := nil;
        end else begin
          deathRow.Clear;
          sortOrder := nil;
        end;
        reapAll := true;
      end
      else raise ENotSupportedException(Format('FReapPolicy: [%d]', [FReapPolicy]));
    end;

    if Length(sortOrder) > 0 then
      deathRow.Sort(TComparerTool<__TPair_TKey_TValue>.Many(sortOrder));

    // remove items from cache until enough space
    for candidate in deathRow do
      if reapAll OR ((MaxCapacity - CurrentSize) < requestedSpace) then
        RemoveItemInternal(candidate.Key)
      else break;

    // check enough space was created
    if (MaxCapacity - CurrentSize) < requestedSpace then
      raise Exception.Create(Format('Insufficient cache memory (requested space [%d])', [requestedSpace]));

  finally
    FLock.EndWrite;
  end
end;

procedure TCacheBase<TKey, TValue>.MakeSpaceFast(const requestedSpace: SizeInt);
type
  THashSet_Key = THashSet<TKey>;
var
  savedSpace: SizeInt;
  deathRow: THashSet_Key;
  item: __TPair_TKey_TValue;
  GC : TScoped;
begin
  try
    FLock.BeginWrite;
    savedSpace := 0;
    deathRow := GC.AddObject(THashSet<TKey>.Create) as THashSet_Key;
    for item in FInternalStorage do begin
      if NOT IsExpired(item.Value) then
          continue;
      deathRow.Add(item.Key);
      inc(savedSpace, item.Value.Size);
      if requestedSpace >= savedSpace then
          break;
    end;
    if savedSpace < requestedSpace then begin
      for item in FInternalStorage do begin
          if (IsExpired(item.Value)) OR (deathRow.Contains(item.Key)) then
              continue;
          deathRow.Add(item.Key);
          inc(savedSpace, item.Value.Size);
          if requestedSpace >= savedSpace then
              break;
      end
    end;
    for item in deathRow do
      RemoveItemInternal(item);
  finally
    FLock.EndWrite;
  end
end;

function TCacheBase<TKey, TValue>.AddItemInternal(const Key: TKey; const AVal: TValue) : TCachedItem<TValue>;
var
  item : TCachedItem<TValue>;
  size : Cardinal;
begin
  item := TCachedItem<TValue>.Create(AVal, EstimateSize(AVal), FItemDisposePolicy);

  if (MaxCapacity - CurrentSize) < item.Size then
    MakeSpace(item.Size);

  FInternalStorage.Add(key, item);
  inc(FCurrentSize, item.Size);
  Result := item;
end;

procedure TCacheBase<TKey, TValue>.RemoveItemInternal(const AKey: TKey);
var
  item : TValue;
begin
    item := FInternalStorage[AKey];
    FInternalStorage.Remove(AKey);
    dec(FCurrentSize, item.Size);
    OnItemRemoved(AKey, item);
    NotifyItemRemoved(AKey, item);
end;

procedure TCacheBase<TKey, TValue>.NotifyItemFetched(const AKey: TKey; const AVal: TValue);
begin
  OnItemFetched(AKey, AVal);
  ItemFetched.Invoke(Self, [AKey, AVal]);
end;

procedure TCacheBase<TKey, TValue>.NotifyItemRemoved(const AKey: TKey; const AVal: TValue);
begin
  OnItemRemoved(AKey, AVal);
  ItemRemoved.Invoke(Self, [AKey, AVal]);
end;

{%region Sort Functions }

function TCacheBase<TKey, TValue>.ByExpiredCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
var
  leftExp, rightExp : boolean;
begin
  leftExp := IsExpired(Left.Value);
  rightExp := IsExpired(Right.Value);
  if leftExp <> rightExp then
    Result := 0
  else
    if not leftExp then Result := 1 else Result := -1
end;

function TCacheBase<TKey, TValue>.ByAccessedCountCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
begin
  Result := TCompare.Int32(Left.Value.AccessedCount, Right.Value.AccessedCount);
end;

function TCacheBase<TKey, TValue>.ByIdleCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
begin
  Result := TCompare.Int32(TTimeSpan.Subtract(Now,Left.Value.AccessedOn), TTimeSpan.Subtract(Now,Right.Value.AccessedOn));
end;

function TCacheBase<TKey, TValue>.ByIdleDescendingCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
begin
  Result := -ByIdleCompare(Left, Right);
end;

function TCacheBase<TKey, TValue>.ByFetchedOnCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
begin
  Result := TCompare.Int32(Left.Value.FetchedOn, Right.Value.FetchedOn);
end;

function TCacheBase<TKey, TValue>.ByAccessedOnCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
begin
  Result := TCompare.Int32(Left.Value.AccessedOn, Right.Value.AccessedOn);
end;

function TCacheBase<TKey, TValue>.BySizeCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
begin
  Result := TCompare.Int32(Left.Value.Size, Right.Value.Size);
end;

function TCacheBase<TKey, TValue>.BySizeDescendingCompare(constref Left, Right: __TPair_TKey_TValue): Integer;
begin
  Result := -BySizeCompare(Left, Right);
end;

function TCacheBase<TKey, TValue>.IsExpiredFilter(const Item : __TPair_TKey_TValue) : boolean;
begin
  Result := IsExpired(Item.Value);
end;

{%endregion}


{ ActionCache }

//  const AItemDisposePolicy : TItemDisposePolicy; const AReapPolicy : TCacheReapPolicy; const AExpirationPolicy : TExpirationPolicy; const ANullValuePolicy : TNullValuePolicy; const AMaxCapacity : Cardinal; const AExpirationDuration : TTimeSpan
constructor TActionCache<TKey, TValue>.Create(
            AOwner:TComponent;
            const AValueFetcher : IValueFetcher;
            const ASizeEstimator : ISizeEstimator;
            const AItemDisposePolicy : TItemDisposePolicy;
            const AReapPolicy : TCacheReapPolicy;
            const AExpirationPolicy : TExpirationPolicy;
            const ANullPolicy : TNullValuePolicy;
            const AMaxCapacity : Integer;
            const AExpirationDuration : TTimeSpan);
begin
  Inherited Create(AOwner, AItemDisposePolicy, AReapPolicy, AExpirationPolicy, ANullPolicy, AMaxCapacity, AExpirationDuration);
  FValueFetcher := AValueFetcher;
  FSizeEstimator := ASizeEstimator;
end;

function TActionCache<TKey, TValue>.Fetch(const AKey: TKey): TValue;
var
  val : TValue;
begin
  val := FValueFetcher.Fetch(AKey);
  NotifyItemFetched(AKey, val);
  result := val;
end;

function TActionCache<TKey, TValue>.EstimateSize(const AVal: TValue) : SizeInt;
begin
  if Assigned(FSizeEstimator) then
    Result := FSizeEstimator.Estimate(AVal)
  else
    Result := 0;
end;

end.

