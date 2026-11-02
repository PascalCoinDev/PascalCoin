unit UCache;

{ Copyright (c) 2017 - 2018 Herman Schoenfeld <https://www.sphere10.com>

  Generic caching with expiration and capacity management.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  Acknowledgements:
  - Herman Schoenfeld: main author

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{ Public types in this unit

  Type                                    | Purpose
  ----------------------------------------+-------------------------------------------------------------------------
  ICacheBulkValueFetcher<TKey, TValue>    | Fills a complete batch of cache values.
  ICacheSizeEstimator<TValue>             | Reports the capacity units used by a value.
  ICacheValueFetcher<TKey, TValue>        | Supplies one value for a requested key.
  TActionCache<TKey, TValue>              | Fetches and sizes individual values through callbacks.
  TBulkFetchActionCache<TKey, TValue>     | Fills bulk cache snapshots through a callback provider.
  TBulkFetchCacheBase<TKey, TValue>       | Loads and refreshes complete batches of values.
  TCacheBase<TKey, TValue>                | Manages cached entries, ownership, expiration and eviction.
  TCacheBulkValueFetcher<TKey, TValue>    | Adapts bulk-fill callbacks to a shared fetch interface.
  TCacheBulkValues<TKey, TValue>          | Holds the dictionary filled by a bulk-fetch provider.
  TCachedItem<TValue>                     | Retains a value and its lifetime, size and access metadata.
  TCacheExpirationPolicy                  | Selects fixed, sliding or unlimited entry lifetime.
  TCacheGlobalBulkFetchProc<TKey, TValue> | Defines a global bulk-fill callback.
  TCacheGlobalEstimateFunc<TValue>        | Defines a global value-size callback.
  TCacheGlobalFetchFunc<TKey, TValue>     | Defines a global per-key fetch callback.
  TCacheItemDisposePolicy                 | Selects how owned cache values are released.
  TCacheItemFetchingEvent<TKey>           | Notifies a listener before a per-key fetch.
  TCacheItemRemovingEvent<TKey, TValue>   | Notifies a listener before an entry is removed.
  TCacheItemValueEvent<TKey, TValue>      | Notifies a listener with a key and value.
  TCacheNestedBulkFetchProc<TKey, TValue> | Defines a captured bulk-fill callback.
  TCacheNestedEstimateFunc<TValue>        | Defines a captured value-size callback.
  TCacheNestedFetchFunc<TKey, TValue>     | Defines a captured per-key fetch callback.
  TCacheNullValuePolicy                   | Controls admission and return of null values.
  TCacheObjectBulkFetchProc<TKey, TValue> | Defines an object-method bulk-fill callback.
  TCacheObjectEstimateFunc<TValue>        | Defines an object-method value-size callback.
  TCacheObjectFetchFunc<TKey, TValue>     | Defines an object-method per-key fetch callback.
  TCacheReapPolicy                        | Selects eviction order when capacity is needed.
  TCacheSizeEstimator<TValue>             | Adapts size callbacks to a shared estimation interface.
  TCacheStaleValuePolicy                  | Controls on-demand checks for changed source values.
  TCacheValueFetcher<TKey, TValue>        | Adapts global, object and captured callbacks to a fetch interface.
  TPreloadedCache<TKey, TValue>           | Caches a borrowed dictionary as a fetch-once snapshot.
}

{$IFDEF FPC}
  {$MODE Delphi}
  {$MODESWITCH NESTEDPROCVARS}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  Classes, SysUtils, SyncObjs, Generics.Collections, Generics.Defaults,
  {$ELSE}
  System.Classes, System.SysUtils, System.SyncObjs, System.Generics.Collections,
  System.Generics.Defaults, System.TimeSpan,
  {$ENDIF}
  UCommon, UMemory;

type
  TCacheItemDisposePolicy = TDisposePolicy;

  // Cache policies are shared by every key/value specialization and cache kind.
  TCacheExpirationPolicy = (epSinceFetchedTime, epSinceLastAccessedTime, epNone);
  TCacheReapPolicy = (crpLeastUsed, crpLargest, crpSmallest, crpOldest, crpIdle, crpASAP, crpNone);
  TCacheNullValuePolicy = (nvpCacheNormally, nvpReturnButDontCache, nvpThrow);
  TCacheStaleValuePolicy = (svpAssumeNeverStale, svpCheckStaleOnDemand);

  { TCachedItem }

  TCachedItem<TValue> = class(TInterfacedObject)
    private
      FCanPurge : boolean;
      FPurged : boolean;
      FRemoving : boolean;
      FProtectedCount : Integer;
      FInsertionOrder : QWord;
      FAccessOrder : QWord;
      procedure ReleaseValue;
    public
      FInvalidated : boolean;
      FFetchedOn : TDateTime;
      FLastAccessedOn : TDateTime;
      FAccessedCount : DWord;
      FSize : SizeInt;
      FDisposePolicy : TCacheItemDisposePolicy;
      FValue : TValue;
      constructor Create(const AValue : TValue; ASize: SizeInt; ADisposePolicy : TCacheItemDisposePolicy);
      destructor Destroy; override;
      property Invalidated : boolean read FInvalidated;
      property Purged : boolean read FPurged;
      property CanPurge : boolean read FCanPurge write FCanPurge;
      property FetchedOn : TDateTime read FFetchedOn;
      property LastAccessedOn : TDateTime read FLastAccessedOn;
      property AccessedCount : DWord read FAccessedCount;
      property Size : SizeInt read FSize;
      property Value : TValue read FValue;
  end;

  // Public callback contracts are independent of a particular cache class.
  TCacheItemFetchingEvent<TKey> = procedure(Sender: TObject; const AKey: TKey) of object;
  TCacheItemValueEvent<TKey, TValue> = procedure(Sender: TObject; const AKey: TKey; const AValue: TValue) of object;
  TCacheItemRemovingEvent<TKey, TValue> = procedure(Sender: TObject; const AKey: TKey; AItem: TCachedItem<TValue>) of object;

  ICacheValueFetcher<TKey, TValue> = interface
    function Fetch(const AKey: TKey): TValue;
  end;

  ICacheSizeEstimator<TValue> = interface
    function Estimate(const AValue: TValue): SizeInt;
  end;

  {$IFDEF FPC}
  TCacheNestedFetchFunc<TKey, TValue> = function(const AKey: TKey): TValue is nested;
  TCacheNestedEstimateFunc<TValue> = function(const AValue: TValue): SizeInt is nested;
  {$ELSE}
  TCacheNestedFetchFunc<TKey, TValue> = reference to function(const AKey: TKey): TValue;
  TCacheNestedEstimateFunc<TValue> = reference to function(const AValue: TValue): SizeInt;
  {$ENDIF}
  TCacheObjectFetchFunc<TKey, TValue> = function(const AKey: TKey): TValue of object;
  TCacheGlobalFetchFunc<TKey, TValue> = function(const AKey: TKey): TValue;
  TCacheObjectEstimateFunc<TValue> = function(const AValue: TValue): SizeInt of object;
  TCacheGlobalEstimateFunc<TValue> = function(const AValue: TValue): SizeInt;

  TCacheValueFetcher<TKey, TValue> = class(TInterfacedObject, ICacheValueFetcher<TKey, TValue>)
  private type
    GlobalFunctionValueFetcher = class(TInterfacedObject, ICacheValueFetcher<TKey, TValue>)
    private
      FFunc: TCacheGlobalFetchFunc<TKey, TValue>;
    public
      constructor Create(const AFunc: TCacheGlobalFetchFunc<TKey, TValue>);
      function Fetch(const AKey: TKey): TValue;
    end;
    ObjectFunctionValueFetcher = class(TInterfacedObject, ICacheValueFetcher<TKey, TValue>)
    private
      FFunc: TCacheObjectFetchFunc<TKey, TValue>;
    public
      constructor Create(const AFunc: TCacheObjectFetchFunc<TKey, TValue>);
      function Fetch(const AKey: TKey): TValue;
    end;
    NestedFunctionValueFetcher = class(TInterfacedObject, ICacheValueFetcher<TKey, TValue>)
    private
      FFunc: TCacheNestedFetchFunc<TKey, TValue>;
    public
      constructor Create(const AFunc: TCacheNestedFetchFunc<TKey, TValue>);
      function Fetch(const AKey: TKey): TValue;
    end;
  private
    FFetcher: ICacheValueFetcher<TKey, TValue>;
  public
    constructor Create(const AFetcher: ICacheValueFetcher<TKey, TValue>);
    function Fetch(const AKey: TKey): TValue;
    class function FromNestedFunction(const AFunc: TCacheNestedFetchFunc<TKey, TValue>): ICacheValueFetcher<TKey, TValue>;
    class function FromObjectFunction(const AFunc: TCacheObjectFetchFunc<TKey, TValue>): ICacheValueFetcher<TKey, TValue>;
    class function FromGlobalFunction(const AFunc: TCacheGlobalFetchFunc<TKey, TValue>): ICacheValueFetcher<TKey, TValue>;
  end;

  TCacheSizeEstimator<TValue> = class(TInterfacedObject, ICacheSizeEstimator<TValue>)
  private type
    GlobalFunctionEstimator = class(TInterfacedObject, ICacheSizeEstimator<TValue>)
    private
      FFunc: TCacheGlobalEstimateFunc<TValue>;
    public
      constructor Create(const AFunc: TCacheGlobalEstimateFunc<TValue>);
      function Estimate(const AValue: TValue): SizeInt;
    end;
    ObjectFunctionEstimator = class(TInterfacedObject, ICacheSizeEstimator<TValue>)
    private
      FFunc: TCacheObjectEstimateFunc<TValue>;
    public
      constructor Create(const AFunc: TCacheObjectEstimateFunc<TValue>);
      function Estimate(const AValue: TValue): SizeInt;
    end;
    NestedFunctionEstimator = class(TInterfacedObject, ICacheSizeEstimator<TValue>)
    private
      FFunc: TCacheNestedEstimateFunc<TValue>;
    public
      constructor Create(const AFunc: TCacheNestedEstimateFunc<TValue>);
      function Estimate(const AValue: TValue): SizeInt;
    end;
  private
    FEstimator: ICacheSizeEstimator<TValue>;
  public
    constructor Create(const AEstimator: ICacheSizeEstimator<TValue>);
    function Estimate(const AValue: TValue): SizeInt;
    class function FromNestedFunction(const AFunc: TCacheNestedEstimateFunc<TValue>): ICacheSizeEstimator<TValue>;
    class function FromObjectFunction(const AFunc: TCacheObjectEstimateFunc<TValue>): ICacheSizeEstimator<TValue>;
    class function FromGlobalFunction(const AFunc: TCacheGlobalEstimateFunc<TValue>): ICacheSizeEstimator<TValue>;
  end;

  // A named dictionary subclass keeps this generic contract portable to FPC.
  TCacheBulkValues<TKey, TValue> = class(TDictionary<TKey, TValue>)
  end;
  ICacheBulkValueFetcher<TKey, TValue> = interface
    procedure Fetch(AValues: TCacheBulkValues<TKey, TValue>);
  end;
  {$IFDEF FPC}
  TCacheNestedBulkFetchProc<TKey, TValue> = procedure(AValues: TCacheBulkValues<TKey, TValue>) is nested;
  {$ELSE}
  TCacheNestedBulkFetchProc<TKey, TValue> = reference to procedure(AValues: TCacheBulkValues<TKey, TValue>);
  {$ENDIF}
  TCacheObjectBulkFetchProc<TKey, TValue> = procedure(AValues: TCacheBulkValues<TKey, TValue>) of object;
  TCacheGlobalBulkFetchProc<TKey, TValue> = procedure(AValues: TCacheBulkValues<TKey, TValue>);

  TCacheBulkValueFetcher<TKey, TValue> = class(TInterfacedObject, ICacheBulkValueFetcher<TKey, TValue>)
  private type
    TGlobalFetcher = class(TInterfacedObject, ICacheBulkValueFetcher<TKey, TValue>)
    private
      FProc: TCacheGlobalBulkFetchProc<TKey, TValue>;
    public
      constructor Create(const AProc: TCacheGlobalBulkFetchProc<TKey, TValue>);
      procedure Fetch(AValues: TCacheBulkValues<TKey, TValue>);
    end;
    TObjectFetcher = class(TInterfacedObject, ICacheBulkValueFetcher<TKey, TValue>)
    private
      FProc: TCacheObjectBulkFetchProc<TKey, TValue>;
    public
      constructor Create(const AProc: TCacheObjectBulkFetchProc<TKey, TValue>);
      procedure Fetch(AValues: TCacheBulkValues<TKey, TValue>);
    end;
    TNestedFetcher = class(TInterfacedObject, ICacheBulkValueFetcher<TKey, TValue>)
    private
      FProc: TCacheNestedBulkFetchProc<TKey, TValue>;
    public
      constructor Create(const AProc: TCacheNestedBulkFetchProc<TKey, TValue>);
      procedure Fetch(AValues: TCacheBulkValues<TKey, TValue>);
    end;
  private
    FFetcher: ICacheBulkValueFetcher<TKey, TValue>;
  public
    constructor Create(const AFetcher: ICacheBulkValueFetcher<TKey, TValue>);
    procedure Fetch(AValues: TCacheBulkValues<TKey, TValue>);
    class function FromNestedFunction(const AProc: TCacheNestedBulkFetchProc<TKey, TValue>): ICacheBulkValueFetcher<TKey, TValue>; static;
    class function FromObjectFunction(const AProc: TCacheObjectBulkFetchProc<TKey, TValue>): ICacheBulkValueFetcher<TKey, TValue>; static;
    class function FromGlobalFunction(const AProc: TCacheGlobalBulkFetchProc<TKey, TValue>): ICacheBulkValueFetcher<TKey, TValue>; static;
  end;

  { TCacheBase }

  // Operations and callbacks run under a recursive lock. A callback may call the
  // cache again, but must not destroy it. Borrowed entries/objects require caller
  // synchronization when used after an operation; the value indexer copies its
  // result while the lock is held. Standard Delphi events have one handler each.
  TCacheBase<TKey, TValue> = class(TComponent)
    protected type
      // Generic implementation aliases kept protected for FPC.
      __TCachedItem_TValue = TCachedItem<TValue>;
      __TPair_TKey_TValue = TPair<TKey, TValue>;
      __TPair_TKey_TCachedItem = TPair<TKey, __TCachedItem_TValue>;
    private
      FItemFetching : TCacheItemFetchingEvent<TKey>;
      FItemFetched : TCacheItemValueEvent<TKey, TValue>;
      FItemRemoving : TCacheItemRemovingEvent<TKey, TValue>;
      FItemRemoved : TCacheItemValueEvent<TKey, TValue>;
      FReapLevel : Double;
      FStaleValuePolicy : TCacheStaleValuePolicy;
      FLastUpdateOn : TDateTime;
      FLastAccessedOn : TDateTime;
      FTotalAccesses : QWord;
      FOrder : QWord;
      FLastResult : __TCachedItem_TValue;
      FDestroying : boolean;
      function GetItemCount : Integer;
      function GetCachedItems : TArray<__TCachedItem_TValue>;
      function GetValue(const AKey: TKey) : TValue;
      function IsExpiredAt(AItem: __TCachedItem_TValue; ATimestamp: TDateTime) : boolean;
      function CompareVictims(ALeft, ARight: __TCachedItem_TValue; ATimestamp: TDateTime) : Integer;
      procedure SortVictims(var AEntries: TArray<__TPair_TKey_TCachedItem>; ALeft, ARight: Integer; ATimestamp: TDateTime);
      function StoreValue(const AKey: TKey; const AValue: TValue; AFetched: boolean) : __TCachedItem_TValue;
      function IsNullValue(const AValue: TValue) : boolean;
      procedure CheckAvailable;
      procedure RememberResult(AItem: __TCachedItem_TValue);
    protected type
      __TDictionary_TKey_TCachedItem_TValue = TDictionary<TKey, __TCachedItem_TValue>;
    protected
      FCurrentSize : SizeInt;
      FMaxCapacity : SizeInt;
      FItemDisposePolicy : TCacheItemDisposePolicy;
      FExpirationDuration : TTimeSpan;
      FExpirationPolicy : TCacheExpirationPolicy;
      FReapPolicy : TCacheReapPolicy;
      FNullValuePolicy : TCacheNullValuePolicy;
      FInternalStorage : __TDictionary_TKey_TCachedItem_TValue;
      FLock : TCriticalSection;
      function EstimateSize(const AVal: TValue) : SizeInt; virtual; abstract;
      function Fetch(const AKey: TKey): TValue; virtual; abstract;
      function CheckStaleness(const AKey: TKey; AItem: __TCachedItem_TValue) : boolean; virtual;
      // Derived snapshot caches may prepare a complete read before delegating
      // to Get. Ordinary caches retain their per-entry expiry/staleness check.
      function ShouldFetch(const AKey: TKey; AItem: __TCachedItem_TValue) : boolean; virtual;
      procedure MakeSpace(const requestedSpace: SizeInt);
      procedure MakeSpaceFast(const requestedSpace: SizeInt);
      procedure OnItemFetching(const AKey: TKey); virtual;
      procedure OnItemFetched(const AKey: TKey; const AVal: TValue); virtual;
      // Raising here retains the entry and capacity. Recursive removal skips it;
      // replacement of the pending entry is rejected until preparation returns.
      procedure OnItemRemoving(const AKey: TKey; AItem: __TCachedItem_TValue); virtual;
      procedure OnItemRemoved(const AKey: TKey; const AVal: TValue); virtual;
      procedure NotifyItemFetching(const AKey: TKey);
      procedure NotifyItemFetched(const AKey: TKey; const AVal: TValue);
      procedure NotifyItemRemoving(const AKey: TKey; AItem: __TCachedItem_TValue);
      procedure NotifyItemRemoved(const AKey: TKey; const AVal: TValue);
      function AddItemInternal(const AKey: TKey; const AVal: TValue) : __TCachedItem_TValue;
      procedure RemoveItemInternal(const AKey: TKey);
    public
      constructor Create(AOwner:TComponent; const AItemDisposePolicy : TCacheItemDisposePolicy; const AReapPolicy : TCacheReapPolicy; const AExpirationPolicy : TCacheExpirationPolicy; const ANullValuePolicy : TCacheNullValuePolicy; const AMaxCapacity : SizeInt; const AExpirationDuration : TTimeSpan; AReapLevel: Double = 0.1; AStaleValuePolicy: TCacheStaleValuePolicy = svpAssumeNeverStale); overload;
      destructor Destroy; override;
      // Fetching precedes Fetch; Fetched follows successful admission (or an
      // uncached null result). Removing precedes removal; Removed follows it.
      property ItemFetching : TCacheItemFetchingEvent<TKey> read FItemFetching write FItemFetching;
      property ItemFetched : TCacheItemValueEvent<TKey, TValue> read FItemFetched write FItemFetched;
      property ItemRemoving : TCacheItemRemovingEvent<TKey, TValue> read FItemRemoving write FItemRemoving;
      property ItemRemoved : TCacheItemValueEvent<TKey, TValue> read FItemRemoved write FItemRemoved;
      property ItemCount : Integer read GetItemCount;
      // Snapshot of membership. Entries are borrowed and must not be freed by callers.
      property CachedItems : TArray<__TCachedItem_TValue> read GetCachedItems;
      property CurrentSize : SizeInt read FCurrentSize;
      property MaxCapacity : SizeInt read FMaxCapacity;
      property ReapLevel : Double read FReapLevel;
      // LastUpdateOn tracks explicit writes. FetchedOn begins after Fetch;
      // LastAccessedOn is updated after the fetched notification completes.
      property LastUpdateOn : TDateTime read FLastUpdateOn;
      property LastAccessedOn : TDateTime read FLastAccessedOn;
      property TotalAccesses : QWord read FTotalAccesses;
      property ExpirationDuration : TTimeSpan read FExpirationDuration;
      property ExpirationPolicy : TCacheExpirationPolicy read FExpirationPolicy;
      property ReapPolicy : TCacheReapPolicy read FReapPolicy;
      property NullValuePolicy : TCacheNullValuePolicy read FNullValuePolicy;
      property StaleValuePolicy : TCacheStaleValuePolicy read FStaleValuePolicy;

      // Borrowed until removal; the most recent Get result also survives until the next Get.
      function Get(const AKey: TKey) : __TCachedItem_TValue; virtual;
      // Admission transfers ownership according to the configured disposal
      // policy. A rejected explicit value remains owned by its caller; rejected
      // fetched values are disposed by the cache. Set preserves FetchedOn.
      procedure SetValue(const AKey: TKey; const AValue: TValue); virtual;
      function ContainsCachedItem(const AKey : TKey) : boolean; virtual;
      function IsExpired(ACachedItem: __TCachedItem_TValue) : boolean;
      function IsStale(const AKey: TKey; AItem: __TCachedItem_TValue) : boolean;
      procedure BulkLoad(const ABulkValues : TEnumerable<__TPair_TKey_TValue>);
      // Copies current values; object payloads remain borrowed from their owner.
      function GetAllCachedValues : TArray<TValue>; virtual;
      procedure Invalidate(const AKey: TKey);
      procedure Remove(const AKey: TKey); virtual;
      // Reap respects pins and selects expired entries first. crpNone reaps only
      // expired/invalidated entries; explicit Remove/Purge ignore pins.
      function Reap(AMinimumRelease: SizeInt) : SizeInt; virtual;
      // Purge/Flush remove only original membership, retaining callback additions.
      procedure Purge; virtual;
      procedure Flush;
      property Values[const AKey: TKey] : TValue read GetValue write SetValue; default;
  end;

  { TActionCache }

  TActionCache<TKey, TValue> = class(TCacheBase<TKey, TValue>)
  private
    FValueFetcher: ICacheValueFetcher<TKey, TValue>;
    FSizeEstimator: ICacheSizeEstimator<TValue>;
  protected
    function EstimateSize(const AVal: TValue): SizeInt; override;
    function Fetch(const AKey: TKey): TValue; override;
  public
    constructor Create(AOwner: TComponent; const AValueFetcher: ICacheValueFetcher<TKey, TValue>; const ASizeEstimator: ICacheSizeEstimator<TValue>; const AItemDisposePolicy: TCacheItemDisposePolicy; const AReapPolicy: TCacheReapPolicy; const AExpirationPolicy: TCacheExpirationPolicy; const ANullPolicy: TCacheNullValuePolicy; const AMaxCapacity: SizeInt; const AExpirationDuration: TTimeSpan; AReapLevel: Double = 0.1; AStaleValuePolicy: TCacheStaleValuePolicy = svpAssumeNeverStale); overload;
  end;

  { A whole-source cache. A first read loads every entry; an ordinary missing,
    expired or stale read reloads once. FetchOnceOnly retains its initial
    snapshot until explicit ForceRefresh or Purge. Payload objects are borrowed;
    the fetch callback fills a temporary dictionary owned by the cache. }
  TBulkFetchCacheBase<TKey, TValue> = class(TCacheBase<TKey, TValue>)
  private
    FFetchOnceOnly, FLoaded, FRefreshing: Boolean;
    FFetchCount: Cardinal;
    FKeyComparer: IEqualityComparer<TKey>;
  protected
    function EstimateSize(const AVal: TValue) : SizeInt; override;
    function Fetch(const AKey: TKey): TValue; override;
    function ShouldFetch(const AKey: TKey; AItem: TCachedItem<TValue>) : boolean; override;
    procedure BulkFetch(AValues: TCacheBulkValues<TKey, TValue>); virtual; abstract;
  public
    constructor Create(AOwner: TComponent; const AExpirationPolicy: TCacheExpirationPolicy; const AExpirationDuration: TTimeSpan; AFetchOnceOnly: Boolean = false; ANullValuePolicy: TCacheNullValuePolicy = nvpCacheNormally; AStaleValuePolicy: TCacheStaleValuePolicy = svpAssumeNeverStale; const AKeyComparer: IEqualityComparer<TKey> = nil); overload;
    function Get(const AKey: TKey): TCachedItem<TValue>; override;
    function ContainsCachedItem(const AKey: TKey): Boolean; override;
    function GetAllCachedValues: TArray<TValue>; override;
    // Purge-first refresh: a failed provider leaves no previous snapshot.
    procedure ForceRefresh;
    procedure Purge; override;
    procedure Remove(const AKey: TKey); override;
    // Whole snapshots have zero-sized entries; per-entry reaping cannot release
    // capacity and must not turn an otherwise retained snapshot into partial data.
    function Reap(AMinimumRelease: SizeInt): SizeInt; override;
    property FetchOnceOnly: Boolean read FFetchOnceOnly;
    property FetchCount: Cardinal read FFetchCount;
  end;

  TBulkFetchActionCache<TKey, TValue> = class(TBulkFetchCacheBase<TKey, TValue>)
  private
    FFetcher: ICacheBulkValueFetcher<TKey, TValue>;
  protected
    procedure BulkFetch(AValues: TCacheBulkValues<TKey, TValue>); override;
  public
    constructor Create(AOwner: TComponent; const AFetcher: ICacheBulkValueFetcher<TKey, TValue>; const AExpirationPolicy: TCacheExpirationPolicy; const AExpirationDuration: TTimeSpan; AFetchOnceOnly: Boolean = false; ANullValuePolicy: TCacheNullValuePolicy = nvpCacheNormally; AStaleValuePolicy: TCacheStaleValuePolicy = svpAssumeNeverStale; const AKeyComparer: IEqualityComparer<TKey> = nil); overload;
  end;

  { Borrows both the supplied dictionary and its object values. Keep the source
    alive until this cache is freed. ForceRefresh sees edits made to the source. }
  TPreloadedCache<TKey, TValue> = class(TBulkFetchCacheBase<TKey, TValue>)
  private
    FValues: TDictionary<TKey, TValue>;
  protected
    procedure BulkFetch(AValues: TCacheBulkValues<TKey, TValue>); override;
  public
    constructor Create(AOwner: TComponent; AValues: TDictionary<TKey, TValue>; const AKeyComparer: IEqualityComparer<TKey> = nil); overload;
  end;

implementation

uses
  {$IFDEF FPC}TypInfo, Math{$ELSE}System.TypInfo, System.Math{$ENDIF};

{ TCachedItem }

constructor TCachedItem<TValue>.Create(const AValue : TValue; ASize: SizeInt; ADisposePolicy : TCacheItemDisposePolicy);
begin
  inherited Create;
  FCanPurge := true;
  FFetchedOn := Now;
  FLastAccessedOn := FFetchedOn;
  FSize := ASize;
  FDisposePolicy := ADisposePolicy;
  FValue := AValue;
end;

procedure TCachedItem<TValue>.ReleaseValue;
begin
  case FDisposePolicy of
    idpFree, idpFreeAndNil: if PTypeInfo(TypeInfo(TValue))^.Kind = tkClass then
      TObject(PPointer(@FValue)^).Free;
    idpFreeMem: if PTypeInfo(TypeInfo(TValue))^.Kind = tkPointer then
      System.FreeMem(PPointer(@FValue)^);
  end;
  FValue := Default(TValue);
end;

destructor TCachedItem<TValue>.Destroy;
begin
  ReleaseValue;
  inherited;
end;

{ TCacheBase }

constructor TCacheBase<TKey, TValue>.Create(AOwner:TComponent; const AItemDisposePolicy : TCacheItemDisposePolicy; const AReapPolicy : TCacheReapPolicy; const AExpirationPolicy : TCacheExpirationPolicy; const ANullValuePolicy : TCacheNullValuePolicy; const AMaxCapacity : SizeInt; const AExpirationDuration : TTimeSpan; AReapLevel: Double; AStaleValuePolicy: TCacheStaleValuePolicy);
begin
  inherited Create(AOwner);
  if AMaxCapacity < 0 then
    raise EArgumentException.Create('Cache capacity must be nonnegative.');
  if not ((AReapLevel > 0) and (AReapLevel < 1)) then
    raise EArgumentException.Create('Reap level must be greater than zero and less than one.');
  if AExpirationDuration < TTimeSpan.FromMilliseconds(0) then
    raise EArgumentException.Create('Expiration duration must be nonnegative.');
  if (AItemDisposePolicy in [idpFree, idpFreeAndNil]) and (PTypeInfo(TypeInfo(TValue))^.Kind <> tkClass) then
    raise EArgumentException.Create('Object disposal requires an object-valued cache.');
  if (AItemDisposePolicy = idpFreeMem) and (PTypeInfo(TypeInfo(TValue))^.Kind <> tkPointer) then
    raise EArgumentException.Create('Memory disposal requires a pointer-valued cache.');
  FItemDisposePolicy := AItemDisposePolicy;
  FNullValuePolicy := ANullValuePolicy;
  FInternalStorage := __TDictionary_TKey_TCachedItem_TValue.Create;
  FReapPolicy := AReapPolicy;
  FExpirationPolicy := AExpirationPolicy;
  FMaxCapacity := AMaxCapacity;
  FExpirationDuration := AExpirationDuration;
  FReapLevel := AReapLevel;
  FStaleValuePolicy := AStaleValuePolicy;
  FLastUpdateOn := Now;
  FLastAccessedOn := FLastUpdateOn;
  FLock := TCriticalSection.Create;
end;

destructor TCacheBase<TKey, TValue>.Destroy;
var
  item : __TCachedItem_TValue;
begin
  FDestroying := true;
  // Destruction releases ownership without invoking application callbacks.
  if Assigned(FInternalStorage) then begin
    for item in FInternalStorage.Values do
      item._Release;
    FreeAndNil(FInternalStorage);
  end;
  if Assigned(FLastResult) then begin
    FLastResult._Release;
    FLastResult := nil;
  end;
  FreeAndNil(FLock);
  inherited;
end;

procedure TCacheBase<TKey, TValue>.CheckAvailable;
begin
  if FDestroying then
    raise EInvalidOperation.Create('The cache is being destroyed.');
end;

function TCacheBase<TKey, TValue>.GetItemCount : Integer;
begin
  FLock.Acquire;
  try
    Result := FInternalStorage.Count;
  finally
    FLock.Release;
  end;
end;

function TCacheBase<TKey, TValue>.GetCachedItems : TArray<__TCachedItem_TValue>;
begin
  FLock.Acquire;
  try
    Result := FInternalStorage.Values.ToArray;
  finally
    FLock.Release;
  end;
end;

function TCacheBase<TKey, TValue>.GetValue(const AKey: TKey) : TValue;
begin
  FLock.Acquire;
  try
    Result := Get(AKey).Value;
  finally
    FLock.Release;
  end;
end;

procedure TCacheBase<TKey, TValue>.RememberResult(AItem: __TCachedItem_TValue);
var
  previous : __TCachedItem_TValue;
begin
  AItem._AddRef;
  previous := FLastResult;
  FLastResult := AItem;
  if Assigned(previous) then
    previous._Release;
end;

function TCacheBase<TKey, TValue>.IsNullValue(const AValue: TValue) : boolean;
begin
  Result := false;
  case PTypeInfo(TypeInfo(TValue))^.Kind of
    tkClass, tkInterface, tkPointer, tkClassRef, {$IFDEF FPC}tkProcVar{$ELSE}tkProcedure{$ENDIF}:
      Result := PPointer(@AValue)^ = nil;
  end;
end;

function TCacheBase<TKey, TValue>.Get(const AKey: TKey) : __TCachedItem_TValue;
var
  item, current, original : __TCachedItem_TValue;
  value : TValue;
  requiresFetch : boolean;
begin
  original := nil;
  FLock.Acquire;
  try
    CheckAvailable;
    requiresFetch := not FInternalStorage.TryGetValue(AKey, item);
    if not requiresFetch then begin
      // A staleness hook may remove this entry and reenter Get, replacing the
      // retained last result. Keep our own reference until this operation ends.
      original := item;
      original._AddRef;
      requiresFetch := ShouldFetch(AKey, item);
    end;
    if requiresFetch then begin
      RemoveItemInternal(AKey);
      NotifyItemFetching(AKey);
      value := Fetch(AKey);
      if IsNullValue(value) and (FNullValuePolicy = nvpThrow) then
        raise EInvalidOperation.Create('Cache fetched a null value and its NullValuePolicy prohibits null values.');
      if IsNullValue(value) and (FNullValuePolicy = nvpReturnButDontCache) then begin
        item := __TCachedItem_TValue.Create(value, 0, FItemDisposePolicy);
        item.FInvalidated := true;
        item.FCanPurge := false;
      end else begin
        try
          item := StoreValue(AKey, value, true);
        except
          // A fetched value has no external owner when admission fails.
          item := __TCachedItem_TValue.Create(value, 0, FItemDisposePolicy);
          item.Free;
          raise;
        end;
      end;
      item._AddRef;
      try
        NotifyItemFetched(AKey, value);
        RememberResult(item);
      finally
        item._Release;
      end;
    end else
      RememberResult(item);
    if item.FAccessedCount < High(DWord) then
      Inc(item.FAccessedCount);
    if FTotalAccesses < High(QWord) then
      Inc(FTotalAccesses);
    FLastAccessedOn := Now;
    item.FLastAccessedOn := FLastAccessedOn;
    Inc(FOrder);
    item.FAccessOrder := FOrder;
    if IsNullValue(item.Value) then begin
      if FNullValuePolicy = nvpThrow then
        raise EInvalidOperation.Create('Cache returned a null value and its NullValuePolicy prohibits null values.');
      if FNullValuePolicy = nvpReturnButDontCache then begin
        if FInternalStorage.TryGetValue(AKey, current) and (current = item) then
          RemoveItemInternal(AKey);
        item.FInvalidated := true;
      end;
    end;
    Result := item;
  finally
    try
      if Assigned(original) then
        original._Release;
    finally
      FLock.Release;
    end;
  end;
end;

function TCacheBase<TKey, TValue>.StoreValue(const AKey: TKey; const AValue: TValue; AFetched: boolean) : __TCachedItem_TValue;
var
  item, original : __TCachedItem_TValue;
  size, delta : SizeInt;
  timestamp : TDateTime;
  sameObject : boolean;
  value : TValue;
begin
  value := AValue;
  size := EstimateSize(value);
  if size < 0 then
    raise EInvalidOperation.Create('A cache size estimator returned a negative size.');
  if size > FMaxCapacity then
    raise EInvalidOperation.CreateFmt('Cache capacity insufficient for requested space [%d].', [size]);
  timestamp := Now;
  original := nil;
  FInternalStorage.TryGetValue(AKey, original);
  if Assigned(original) and original.FRemoving then
    raise EInvalidOperation.Create('Cannot replace an entry during removal preparation.');
  if Assigned(original) then begin
    original._AddRef;
    Inc(original.FProtectedCount);
  end;
  try
    delta := size;
    if Assigned(original) then
      Dec(delta, original.Size);
    MakeSpace(delta);
    item := nil;
    FInternalStorage.TryGetValue(AKey, item);
    delta := size;
    if Assigned(item) then
      Dec(delta, item.Size);
    if delta > FMaxCapacity - FCurrentSize then
      raise EInvalidOperation.Create('A cache callback left insufficient capacity for replacement.');
    if not Assigned(item) then begin
      item := __TCachedItem_TValue.Create(value, size, FItemDisposePolicy);
      item._AddRef;
      try
        FInternalStorage.Add(AKey, item);
      except
        // Admission failed: ownership of AValue remains with the caller.
        item.FDisposePolicy := idpNone;
        item._Release;
        raise;
      end;
      Inc(FOrder);
      item.FInsertionOrder := FOrder;
      item.FFetchedOn := timestamp;
    end else begin
      sameObject := false;
      if PTypeInfo(TypeInfo(TValue))^.Kind in [tkClass, tkPointer] then
        sameObject := PPointer(@item.FValue)^ = PPointer(@value)^;
      if not sameObject then
        item.ReleaseValue;
      item.FValue := value;
      item.FSize := size;
    end;
    if AFetched then
      item.FFetchedOn := timestamp;
    item.FInvalidated := false;
    item.FPurged := false;
    item.FLastAccessedOn := timestamp;
    Inc(FOrder);
    item.FAccessOrder := FOrder;
    Inc(FCurrentSize, delta);
    Result := item;
  finally
    if Assigned(original) then begin
      Dec(original.FProtectedCount);
      original._Release;
    end;
  end;
end;

procedure TCacheBase<TKey, TValue>.SetValue(const AKey: TKey; const AValue: TValue);
begin
  FLock.Acquire;
  try
    CheckAvailable;
    StoreValue(AKey, AValue, false);
    FLastUpdateOn := Now;
    FLastAccessedOn := FLastUpdateOn;
  finally
    FLock.Release;
  end;
end;

function TCacheBase<TKey, TValue>.ContainsCachedItem(const AKey: TKey) : boolean;
var
  item : __TCachedItem_TValue;
begin
  FLock.Acquire;
  try
    CheckAvailable;
    Result := FInternalStorage.TryGetValue(AKey, item);
    if Result then
      Result := not IsExpired(item);
  finally
    FLock.Release;
  end;
end;

function TCacheBase<TKey, TValue>.IsExpired(ACachedItem: __TCachedItem_TValue) : boolean;
begin
  Result := IsExpiredAt(ACachedItem, Now);
end;

function TCacheBase<TKey, TValue>.IsExpiredAt(AItem: __TCachedItem_TValue; ATimestamp: TDateTime) : boolean;
var
  timestamp : TDateTime;
begin
  if not Assigned(AItem) then
    raise EArgumentNilException.Create('AItem');
  Result := AItem.Invalidated;
  if Result or (FExpirationPolicy = epNone) then
    exit;
  if FExpirationPolicy = epSinceFetchedTime then
    timestamp := AItem.FetchedOn
  else
    timestamp := AItem.LastAccessedOn;
  Result := TTimeSpan.Subtract(ATimestamp, timestamp) > FExpirationDuration;
end;

function TCacheBase<TKey, TValue>.CheckStaleness(const AKey: TKey; AItem: __TCachedItem_TValue) : boolean;
begin
  Result := false;
end;

function TCacheBase<TKey, TValue>.IsStale(const AKey: TKey; AItem: __TCachedItem_TValue) : boolean;
begin
  Result := (FStaleValuePolicy = svpCheckStaleOnDemand) and CheckStaleness(AKey, AItem);
end;

procedure TCacheBase<TKey, TValue>.Invalidate(const AKey: TKey);
var
  item : __TCachedItem_TValue;
begin
  FLock.Acquire;
  try
    CheckAvailable;
    if FInternalStorage.TryGetValue(AKey, item) then
      item.FInvalidated := true;
  finally
    FLock.Release;
  end;
end;

procedure TCacheBase<TKey, TValue>.Remove(const AKey: TKey);
begin
  FLock.Acquire;
  try
    CheckAvailable;
    RemoveItemInternal(AKey);
  finally
    FLock.Release;
  end;
end;

procedure TCacheBase<TKey, TValue>.RemoveItemInternal(const AKey: TKey);
var
  item : __TCachedItem_TValue;
begin
  if not FInternalStorage.TryGetValue(AKey, item) then
    exit;
  if item.FRemoving then
    exit;
  item.FRemoving := true;
  try
    NotifyItemRemoving(AKey, item);
  finally
    item.FRemoving := false;
  end;
  FInternalStorage.Remove(AKey);
  Dec(FCurrentSize, item.Size);
  item.FPurged := true;
  item.FCanPurge := false;
  try
    NotifyItemRemoved(AKey, item.Value);
  finally
    try
      if FItemDisposePolicy <> idpNone then
        item.ReleaseValue;
    finally
      item._Release;
    end;
  end;
end;

procedure TCacheBase<TKey, TValue>.Purge;
var
  entries : TArray<__TPair_TKey_TCachedItem>;
  entry : __TPair_TKey_TCachedItem;
  current : __TCachedItem_TValue;
begin
  FLock.Acquire;
  try
    CheckAvailable;
    entries := FInternalStorage.ToArray;
    for entry in entries do
      entry.Value._AddRef;
    try
      for entry in entries do
        if FInternalStorage.TryGetValue(entry.Key, current) and (current = entry.Value) then
          RemoveItemInternal(entry.Key);
    finally
      for entry in entries do
        entry.Value._Release;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TCacheBase<TKey, TValue>.Flush;
begin
  Purge;
end;

function TCacheBase<TKey, TValue>.ShouldFetch(const AKey: TKey; AItem: __TCachedItem_TValue): boolean;
begin
  Result := (not AItem.FRemoving) and (IsExpired(AItem) or IsStale(AKey, AItem));
end;

function TCacheBase<TKey, TValue>.GetAllCachedValues: TArray<TValue>;
var item: __TCachedItem_TValue; i: Integer;
begin
  FLock.Acquire;
  try
    CheckAvailable;
    SetLength(Result,FInternalStorage.Count);
    i := 0;
    for item in FInternalStorage.Values do begin
      Result[i] := item.Value;
      Inc(i);
    end;
  finally FLock.Release; end;
end;

procedure TCacheBase<TKey, TValue>.BulkLoad(const ABulkValues: TEnumerable<__TPair_TKey_TValue>);
var
  pair : __TPair_TKey_TValue;
begin
  if not Assigned(ABulkValues) then
    raise EArgumentNilException.Create('ABulkValues');
  FLock.Acquire;
  try
    CheckAvailable;
    for pair in ABulkValues do
      SetValue(pair.Key, pair.Value);
  finally
    FLock.Release;
  end;
end;

function TCacheBase<TKey, TValue>.CompareVictims(ALeft, ARight: __TCachedItem_TValue; ATimestamp: TDateTime) : Integer;
var
  leftExpired, rightExpired : boolean;
begin
  leftExpired := IsExpiredAt(ALeft, ATimestamp);
  rightExpired := IsExpiredAt(ARight, ATimestamp);
  if leftExpired <> rightExpired then begin
    if leftExpired then Result := -1 else Result := 1;
    exit;
  end;
  Result := 0;
  case FReapPolicy of
    crpLeastUsed: begin
      if ALeft.AccessedCount < ARight.AccessedCount then Result := -1;
      if ALeft.AccessedCount > ARight.AccessedCount then Result := 1;
      if Result = 0 then begin
        if ALeft.Size > ARight.Size then Result := -1;
        if ALeft.Size < ARight.Size then Result := 1;
      end;
    end;
    crpOldest: begin
      if ALeft.FInsertionOrder < ARight.FInsertionOrder then Result := -1;
      if ALeft.FInsertionOrder > ARight.FInsertionOrder then Result := 1;
    end;
    crpIdle: begin
      if ALeft.FAccessOrder < ARight.FAccessOrder then Result := -1;
      if ALeft.FAccessOrder > ARight.FAccessOrder then Result := 1;
    end;
    crpLargest: begin
      if ALeft.Size > ARight.Size then Result := -1;
      if ALeft.Size < ARight.Size then Result := 1;
    end;
    crpSmallest: begin
      if ALeft.Size < ARight.Size then Result := -1;
      if ALeft.Size > ARight.Size then Result := 1;
    end;
  end;
  if Result = 0 then begin
    if ALeft.FInsertionOrder < ARight.FInsertionOrder then Result := -1;
    if ALeft.FInsertionOrder > ARight.FInsertionOrder then Result := 1;
  end;
end;

procedure TCacheBase<TKey, TValue>.SortVictims(var AEntries: TArray<__TPair_TKey_TCachedItem>; ALeft, ARight: Integer; ATimestamp: TDateTime);
var
  i, j : Integer;
  pivot : __TCachedItem_TValue;
  temp : __TPair_TKey_TCachedItem;
begin
  if ALeft >= ARight then
    exit;
  i := ALeft;
  j := ARight;
  pivot := AEntries[ALeft + (ARight - ALeft) div 2].Value;
  repeat
    while CompareVictims(AEntries[i].Value, pivot, ATimestamp) < 0 do Inc(i);
    while CompareVictims(AEntries[j].Value, pivot, ATimestamp) > 0 do Dec(j);
    if i <= j then begin
      temp := AEntries[i];
      AEntries[i] := AEntries[j];
      AEntries[j] := temp;
      Inc(i);
      Dec(j);
    end;
  until i > j;
  if ALeft < j then SortVictims(AEntries, ALeft, j, ATimestamp);
  if i < ARight then SortVictims(AEntries, i, ARight, ATimestamp);
end;

function TCacheBase<TKey, TValue>.Reap(AMinimumRelease: SizeInt) : SizeInt;
var
  entries : TArray<__TPair_TKey_TCachedItem>;
  entry : __TPair_TKey_TCachedItem;
  current : __TCachedItem_TValue;
  before : SizeInt;
  timestamp : TDateTime;
begin
  if AMinimumRelease < 0 then
    raise EArgumentException.Create('The requested release must be nonnegative.');
  FLock.Acquire;
  try
    CheckAvailable;
    Result := 0;
    if AMinimumRelease = 0 then
      exit;
    before := FCurrentSize;
    timestamp := Now;
    entries := FInternalStorage.ToArray;
    for entry in entries do
      entry.Value._AddRef;
    try
      // Equal-sized and equal-time victims are ordered by insertion.
      if FReapPolicy <> crpNone then
        SortVictims(entries, 0, High(entries), timestamp);
      for entry in entries do begin
        if before - FCurrentSize >= AMinimumRelease then
          break;
        if not FInternalStorage.TryGetValue(entry.Key, current) then
          continue;
        if (current <> entry.Value) or (not current.CanPurge) or current.FRemoving or (current.FProtectedCount > 0) then
          continue;
        if (FReapPolicy = crpNone) and (not IsExpired(current)) then
          continue;
        RemoveItemInternal(entry.Key);
      end;
      Result := Max(0, before - FCurrentSize);
    finally
      for entry in entries do
        entry.Value._Release;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TCacheBase<TKey, TValue>.MakeSpace(const requestedSpace: SizeInt);
var
  deficit, target : SizeInt;
  fraction : Int64;
begin
  if requestedSpace <= FMaxCapacity - FCurrentSize then
    exit;
  deficit := requestedSpace - (FMaxCapacity - FCurrentSize);
  fraction := Ceil(FMaxCapacity * FReapLevel);
  if fraction >= FMaxCapacity then
    target := FMaxCapacity
  else
    target := Max(1, fraction);
  Reap(Max(deficit, target));
  if requestedSpace > FMaxCapacity - FCurrentSize then
    raise EInvalidOperation.CreateFmt('Insufficient cache capacity (requested space [%d]).', [requestedSpace]);
end;

procedure TCacheBase<TKey, TValue>.MakeSpaceFast(const requestedSpace: SizeInt);
begin
  MakeSpace(requestedSpace);
end;

function TCacheBase<TKey, TValue>.AddItemInternal(const AKey: TKey; const AVal: TValue) : __TCachedItem_TValue;
begin
  Result := StoreValue(AKey, AVal, true);
end;

procedure TCacheBase<TKey, TValue>.OnItemFetching(const AKey: TKey);
begin
end;

procedure TCacheBase<TKey, TValue>.OnItemFetched(const AKey: TKey; const AVal: TValue);
begin
end;

procedure TCacheBase<TKey, TValue>.OnItemRemoving(const AKey: TKey; AItem: __TCachedItem_TValue);
begin
end;

procedure TCacheBase<TKey, TValue>.OnItemRemoved(const AKey: TKey; const AVal: TValue);
begin
end;

procedure TCacheBase<TKey, TValue>.NotifyItemFetching(const AKey: TKey);
begin
  OnItemFetching(AKey);
  if Assigned(FItemFetching) then
    FItemFetching(Self, AKey);
end;

procedure TCacheBase<TKey, TValue>.NotifyItemFetched(const AKey: TKey; const AVal: TValue);
begin
  OnItemFetched(AKey, AVal);
  if Assigned(FItemFetched) then
    FItemFetched(Self, AKey, AVal);
end;

procedure TCacheBase<TKey, TValue>.NotifyItemRemoving(const AKey: TKey; AItem: __TCachedItem_TValue);
begin
  OnItemRemoving(AKey, AItem);
  if Assigned(FItemRemoving) then
    FItemRemoving(Self, AKey, AItem);
end;

procedure TCacheBase<TKey, TValue>.NotifyItemRemoved(const AKey: TKey; const AVal: TValue);
begin
  OnItemRemoved(AKey, AVal);
  if Assigned(FItemRemoved) then
    FItemRemoved(Self, AKey, AVal);
end;

{ TActionCache }

constructor TActionCache<TKey, TValue>.Create(AOwner:TComponent; const AValueFetcher : ICacheValueFetcher<TKey, TValue>; const ASizeEstimator : ICacheSizeEstimator<TValue>; const AItemDisposePolicy : TCacheItemDisposePolicy; const AReapPolicy : TCacheReapPolicy; const AExpirationPolicy : TCacheExpirationPolicy; const ANullPolicy : TCacheNullValuePolicy; const AMaxCapacity : SizeInt; const AExpirationDuration : TTimeSpan; AReapLevel: Double; AStaleValuePolicy: TCacheStaleValuePolicy);
begin
  inherited Create(AOwner, AItemDisposePolicy, AReapPolicy, AExpirationPolicy, ANullPolicy, AMaxCapacity, AExpirationDuration, AReapLevel, AStaleValuePolicy);
  if not Assigned(AValueFetcher) then
    raise EArgumentNilException.Create('AValueFetcher');
  FValueFetcher := AValueFetcher;
  FSizeEstimator := ASizeEstimator;
end;

function TActionCache<TKey, TValue>.Fetch(const AKey: TKey): TValue;
begin
  Result := FValueFetcher.Fetch(AKey);
end;

function TActionCache<TKey, TValue>.EstimateSize(const AVal: TValue) : SizeInt;
begin
  if Assigned(FSizeEstimator) then
    Result := FSizeEstimator.Estimate(AVal)
  else
    Result := 0;
end;

constructor TCacheValueFetcher<TKey, TValue>.Create(const AFetcher: ICacheValueFetcher<TKey, TValue>);
begin
  inherited Create;
  if not Assigned(AFetcher) then
    raise EArgumentNilException.Create('AFetcher');
  FFetcher := AFetcher;
end;

function TCacheValueFetcher<TKey, TValue>.Fetch(const AKey: TKey): TValue;
begin
  Result := FFetcher.Fetch(AKey);
end;

class function TCacheValueFetcher<TKey, TValue>.FromNestedFunction(const AFunc: TCacheNestedFetchFunc<TKey, TValue>) : ICacheValueFetcher<TKey, TValue>;
begin
  Result := NestedFunctionValueFetcher.Create(AFunc);
end;

class function TCacheValueFetcher<TKey, TValue>.FromObjectFunction(const AFunc: TCacheObjectFetchFunc<TKey, TValue>) : ICacheValueFetcher<TKey, TValue>;
begin
  Result := ObjectFunctionValueFetcher.Create(AFunc);
end;

class function TCacheValueFetcher<TKey, TValue>.FromGlobalFunction(const AFunc: TCacheGlobalFetchFunc<TKey, TValue>) : ICacheValueFetcher<TKey, TValue>;
begin
  Result := GlobalFunctionValueFetcher.Create(AFunc);
end;

constructor TCacheSizeEstimator<TValue>.Create(const AEstimator: ICacheSizeEstimator<TValue>);
begin
  inherited Create;
  if not Assigned(AEstimator) then
    raise EArgumentNilException.Create('AEstimator');
  FEstimator := AEstimator;
end;

function TCacheSizeEstimator<TValue>.Estimate(const AValue: TValue) : SizeInt;
begin
  Result := FEstimator.Estimate(AValue);
end;

class function TCacheSizeEstimator<TValue>.FromNestedFunction(const AFunc: TCacheNestedEstimateFunc<TValue>) : ICacheSizeEstimator<TValue>;
begin
  Result := NestedFunctionEstimator.Create(AFunc);
end;

class function TCacheSizeEstimator<TValue>.FromObjectFunction(const AFunc: TCacheObjectEstimateFunc<TValue>) : ICacheSizeEstimator<TValue>;
begin
  Result := ObjectFunctionEstimator.Create(AFunc);
end;

class function TCacheSizeEstimator<TValue>.FromGlobalFunction(const AFunc: TCacheGlobalEstimateFunc<TValue>) : ICacheSizeEstimator<TValue>;
begin
  Result := GlobalFunctionEstimator.Create(AFunc);
end;

constructor TCacheValueFetcher<TKey, TValue>.GlobalFunctionValueFetcher.Create(const AFunc: TCacheGlobalFetchFunc<TKey, TValue>);
begin
  inherited Create;
  if not Assigned(AFunc) then
    raise EArgumentNilException.Create('AFunc');
  FFunc := AFunc;
end;

function TCacheValueFetcher<TKey, TValue>.GlobalFunctionValueFetcher.Fetch(const AKey: TKey): TValue;
begin
  Result := FFunc(AKey);
end;

constructor TCacheValueFetcher<TKey, TValue>.ObjectFunctionValueFetcher.Create(const AFunc: TCacheObjectFetchFunc<TKey, TValue>);
begin
  inherited Create;
  if not Assigned(AFunc) then
    raise EArgumentNilException.Create('AFunc');
  FFunc := AFunc;
end;

function TCacheValueFetcher<TKey, TValue>.ObjectFunctionValueFetcher.Fetch(const AKey: TKey): TValue;
begin
  Result := FFunc(AKey);
end;

constructor TCacheValueFetcher<TKey, TValue>.NestedFunctionValueFetcher.Create(const AFunc: TCacheNestedFetchFunc<TKey, TValue>);
begin
  inherited Create;
  if not Assigned(AFunc) then
    raise EArgumentNilException.Create('AFunc');
  FFunc := AFunc;
end;

function TCacheValueFetcher<TKey, TValue>.NestedFunctionValueFetcher.Fetch(const AKey: TKey): TValue;
begin
  Result := FFunc(AKey);
end;

constructor TCacheSizeEstimator<TValue>.GlobalFunctionEstimator.Create(const AFunc: TCacheGlobalEstimateFunc<TValue>);
begin
  inherited Create;
  if not Assigned(AFunc) then
    raise EArgumentNilException.Create('AFunc');
  FFunc := AFunc;
end;

function TCacheSizeEstimator<TValue>.GlobalFunctionEstimator.Estimate(const AValue: TValue): SizeInt;
begin
  Result := FFunc(AValue);
end;

constructor TCacheSizeEstimator<TValue>.ObjectFunctionEstimator.Create(const AFunc: TCacheObjectEstimateFunc<TValue>);
begin
  inherited Create;
  if not Assigned(AFunc) then
    raise EArgumentNilException.Create('AFunc');
  FFunc := AFunc;
end;

function TCacheSizeEstimator<TValue>.ObjectFunctionEstimator.Estimate(const AValue: TValue): SizeInt;
begin
  Result := FFunc(AValue);
end;

constructor TCacheSizeEstimator<TValue>.NestedFunctionEstimator.Create(const AFunc: TCacheNestedEstimateFunc<TValue>);
begin
  inherited Create;
  if not Assigned(AFunc) then
    raise EArgumentNilException.Create('AFunc');
  FFunc := AFunc;
end;

function TCacheSizeEstimator<TValue>.NestedFunctionEstimator.Estimate(const AValue: TValue): SizeInt;
begin
  Result := FFunc(AValue);
end;

{ TBulkFetchCacheBase }

constructor TBulkFetchCacheBase<TKey, TValue>.Create(AOwner: TComponent; const AExpirationPolicy: TCacheExpirationPolicy; const AExpirationDuration: TTimeSpan; AFetchOnceOnly: Boolean; ANullValuePolicy: TCacheNullValuePolicy; AStaleValuePolicy: TCacheStaleValuePolicy; const AKeyComparer: IEqualityComparer<TKey>);
begin
  inherited Create(AOwner,idpNone,TCacheReapPolicy.crpNone,
    AExpirationPolicy,ANullValuePolicy,High(SizeInt),AExpirationDuration,0.1,AStaleValuePolicy);
  FFetchOnceOnly := AFetchOnceOnly;
  FKeyComparer := AKeyComparer;
  if Assigned(AKeyComparer) then begin
    FreeAndNil(FInternalStorage);
    FInternalStorage := TDictionary<TKey,{$IFDEF FPC}__TCachedItem_TValue{$ELSE}TCachedItem<TValue>{$ENDIF}>.Create(AKeyComparer);
  end;
end;

function TBulkFetchCacheBase<TKey, TValue>.EstimateSize(const AVal: TValue): SizeInt;
begin
  Result := 0; // A complete source snapshot has no per-entry capacity eviction.
end;

function TBulkFetchCacheBase<TKey, TValue>.ShouldFetch(const AKey: TKey; AItem: {$IFDEF FPC}__TCachedItem_TValue{$ELSE}TCachedItem<TValue>{$ENDIF}): boolean;
begin
  // Get has already checked/refreshed the whole snapshot. Rechecking here would
  // remove fetch-once values or refetch twice for one initial missing-key read.
  Result := False;
end;

function TBulkFetchCacheBase<TKey, TValue>.Fetch(const AKey: TKey): TValue;
var item: {$IFDEF FPC}__TCachedItem_TValue{$ELSE}TCachedItem<TValue>{$ENDIF};
begin
  if FInternalStorage.TryGetValue(AKey,item) then Result := item.Value
  else if FNullValuePolicy = TCacheNullValuePolicy.nvpThrow then
    raise EInvalidOperation.Create('The bulk cache source does not contain the requested key.')
  else Result := Default(TValue);
end;

function TBulkFetchCacheBase<TKey, TValue>.Get(const AKey: TKey): {$IFDEF FPC}__TCachedItem_TValue{$ELSE}TCachedItem<TValue>{$ENDIF};
var item: {$IFDEF FPC}__TCachedItem_TValue{$ELSE}TCachedItem<TValue>{$ENDIF}; refresh: Boolean;
begin
  FLock.Acquire;
  try
    CheckAvailable;
    if not FRefreshing then begin
      refresh := not FLoaded;
      if FLoaded and not FFetchOnceOnly then begin
        refresh := not FInternalStorage.TryGetValue(AKey,item);
        if not refresh then begin
          item._AddRef;
          try refresh := (not item.FRemoving) and (IsExpired(item) or IsStale(AKey,item));
          finally item._Release; end;
        end;
      end;
      if refresh then ForceRefresh;
    end;
    Result := inherited Get(AKey);
  finally FLock.Release; end;
end;

function TBulkFetchCacheBase<TKey, TValue>.ContainsCachedItem(const AKey: TKey): Boolean;
begin
  FLock.Acquire;
  try
    CheckAvailable;
    if (not FLoaded) and (not FRefreshing) then ForceRefresh;
    if FFetchOnceOnly then Result := FInternalStorage.ContainsKey(AKey)
    else Result := inherited ContainsCachedItem(AKey);
  finally FLock.Release; end;
end;

function TBulkFetchCacheBase<TKey, TValue>.GetAllCachedValues: TArray<TValue>;
begin
  FLock.Acquire;
  try
    CheckAvailable;
    if (not FLoaded) and (not FRefreshing) then ForceRefresh;
    Result := inherited GetAllCachedValues;
  finally FLock.Release; end;
end;

procedure TBulkFetchCacheBase<TKey, TValue>.ForceRefresh;
var values: TCacheBulkValues<TKey, TValue>; pair: TPair<TKey,TValue>;
begin
  FLock.Acquire;
  try
    CheckAvailable;
    if FRefreshing then
      raise EInvalidOperation.Create('A bulk refresh is already running on this cache.');
    FRefreshing := True;
    try
      // Keep purge-first semantics explicit: provider/admission failure does not
      // resurrect the previous snapshot. The temporary container is always ours;
      // class/pointer payloads stay borrowed from the provider/source owner.
      Purge;
      if Assigned(FKeyComparer) then values := TCacheBulkValues<TKey, TValue>.Create(FKeyComparer)
      else values := TCacheBulkValues<TKey, TValue>.Create;
      try
        BulkFetch(values);
        for pair in values do SetValue(pair.Key,pair.Value);
        FLoaded := True;
        Inc(FFetchCount);
      finally values.Free; end;
    finally FRefreshing := False; end;
  finally FLock.Release; end;
end;

procedure TBulkFetchCacheBase<TKey, TValue>.Purge;
begin
  FLock.Acquire;
  try
    inherited;
    FLoaded := False;
    FFetchCount := 0;
  finally FLock.Release; end;
end;

procedure TBulkFetchCacheBase<TKey, TValue>.Remove(const AKey: TKey);
begin
  raise ENotSupportedException.Create('Items cannot be manually removed from a bulk-fetch cache.');
end;

function TBulkFetchCacheBase<TKey, TValue>.Reap(AMinimumRelease: SizeInt): SizeInt;
begin
  if AMinimumRelease < 0 then
    raise EArgumentException.Create('The requested release must be nonnegative.');
  // Reap(0) still performs the base lifecycle check under its lock, without
  // removing any entry or asking the whole-source provider to run.
  Result := inherited Reap(0);
end;

{ TBulkFetchActionCache }

constructor TBulkFetchActionCache<TKey, TValue>.Create(AOwner: TComponent; const AFetcher: ICacheBulkValueFetcher<TKey, TValue>; const AExpirationPolicy: TCacheExpirationPolicy; const AExpirationDuration: TTimeSpan; AFetchOnceOnly: Boolean; ANullValuePolicy: TCacheNullValuePolicy; AStaleValuePolicy: TCacheStaleValuePolicy; const AKeyComparer: IEqualityComparer<TKey>);
begin
  inherited Create(AOwner,AExpirationPolicy,AExpirationDuration,AFetchOnceOnly,
    ANullValuePolicy,AStaleValuePolicy,AKeyComparer);
  if not Assigned(AFetcher) then raise EArgumentNilException.Create('AFetcher');
  FFetcher := AFetcher;
end;

procedure TBulkFetchActionCache<TKey, TValue>.BulkFetch(AValues: TCacheBulkValues<TKey, TValue>);
begin
  FFetcher.Fetch(AValues);
end;

constructor TCacheBulkValueFetcher<TKey, TValue>.Create(const AFetcher: ICacheBulkValueFetcher<TKey, TValue>);
begin
  inherited Create;
  if not Assigned(AFetcher) then raise EArgumentNilException.Create('AFetcher');
  FFetcher := AFetcher;
end;

procedure TCacheBulkValueFetcher<TKey, TValue>.Fetch(AValues: TCacheBulkValues<TKey, TValue>);
begin
  FFetcher.Fetch(AValues);
end;

class function TCacheBulkValueFetcher<TKey, TValue>.FromGlobalFunction(const AProc: TCacheGlobalBulkFetchProc<TKey, TValue>): ICacheBulkValueFetcher<TKey, TValue>;
begin
  Result := TGlobalFetcher.Create(AProc);
end;

class function TCacheBulkValueFetcher<TKey, TValue>.FromObjectFunction(const AProc: TCacheObjectBulkFetchProc<TKey, TValue>): ICacheBulkValueFetcher<TKey, TValue>;
begin
  Result := TObjectFetcher.Create(AProc);
end;

class function TCacheBulkValueFetcher<TKey, TValue>.FromNestedFunction(const AProc: TCacheNestedBulkFetchProc<TKey, TValue>): ICacheBulkValueFetcher<TKey, TValue>;
begin
  Result := TNestedFetcher.Create(AProc);
end;

constructor TCacheBulkValueFetcher<TKey, TValue>.TGlobalFetcher.Create(const AProc: TCacheGlobalBulkFetchProc<TKey, TValue>);
begin
  inherited Create;
  if not Assigned(AProc) then raise EArgumentNilException.Create('AProc');
  FProc := AProc;
end;

procedure TCacheBulkValueFetcher<TKey, TValue>.TGlobalFetcher.Fetch(AValues: TCacheBulkValues<TKey, TValue>);
begin
  FProc(AValues);
end;

constructor TCacheBulkValueFetcher<TKey, TValue>.TObjectFetcher.Create(const AProc: TCacheObjectBulkFetchProc<TKey, TValue>);
begin
  inherited Create;
  if not Assigned(AProc) then raise EArgumentNilException.Create('AProc');
  FProc := AProc;
end;

procedure TCacheBulkValueFetcher<TKey, TValue>.TObjectFetcher.Fetch(AValues: TCacheBulkValues<TKey, TValue>);
begin
  FProc(AValues);
end;

constructor TCacheBulkValueFetcher<TKey, TValue>.TNestedFetcher.Create(const AProc: TCacheNestedBulkFetchProc<TKey, TValue>);
begin
  inherited Create;
  if not Assigned(AProc) then raise EArgumentNilException.Create('AProc');
  FProc := AProc;
end;

procedure TCacheBulkValueFetcher<TKey, TValue>.TNestedFetcher.Fetch(AValues: TCacheBulkValues<TKey, TValue>);
begin
  FProc(AValues);
end;

{ TPreloadedCache }

constructor TPreloadedCache<TKey, TValue>.Create(AOwner: TComponent; AValues: TDictionary<TKey, TValue>; const AKeyComparer: IEqualityComparer<TKey>);
begin
  inherited Create(AOwner,TCacheExpirationPolicy.epNone,
    TTimeSpan.MaxValue,True,TCacheNullValuePolicy.nvpCacheNormally,
    TCacheStaleValuePolicy.svpAssumeNeverStale,AKeyComparer);
  if not Assigned(AValues) then raise EArgumentNilException.Create('AValues');
  FValues := AValues;
end;

procedure TPreloadedCache<TKey, TValue>.BulkFetch(AValues: TCacheBulkValues<TKey, TValue>);
var pair: TPair<TKey,TValue>;
begin
  for pair in FValues do AValues.AddOrSetValue(pair.Key,pair.Value);
end;

end.
