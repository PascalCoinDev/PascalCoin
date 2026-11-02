unit UCache.Tests;

{ Copyright (c) 2017 - 2018 Herman Schoenfeld <https://www.sphere10.com>

  Cache regression tests.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{$MODE Delphi}
{$H+}
{$MODESWITCH NESTEDPROCVARS}

interface

uses
  Classes, SysUtils, Generics.Collections, fpcunit, testregistry, UCache, UCommon, UMemory, UBulkCacheChecks;

type
  TStringPair = TPair<Integer, AnsiString>;
  TStringPairList = TList<TStringPair>;
  TStringCacheBase = TCacheBase<Integer, AnsiString>;
  TStringActionCache = TActionCache<Integer, AnsiString>;
  TObjectActionCache = TActionCache<Integer, TObject>;

  TTestStringCache = class(TStringCacheBase)
    public
      FetchCount : Integer;
      FetchValue : AnsiString;
      FetchCompletedOn : TDateTime;
      RaiseOnFetch : boolean;
      NegativeSize : boolean;
      Stale : boolean;
      RemoveDuringStaleness : boolean;
    protected
      function EstimateSize(const AValue: AnsiString) : SizeInt; override;
      function Fetch(const AKey: Integer) : AnsiString; override;
      function CheckStaleness(const AKey: Integer; AItem: TCachedItem<AnsiString>) : boolean; override;
  end;

  TCacheTests = class(TTestCase)
    private
      FCache : TTestStringCache;
      FFetchingCount : Integer;
      FFetchedCount : Integer;
      FRemovingCount : Integer;
      FRemovedCount : Integer;
      FCallbackMode : Integer;
      FNotificationOn : TDateTime;
      FObjectFetchCount : Integer;
      FCreateObjects : boolean;
      FObjectSize : SizeInt;
      function NewCache(ACapacity: SizeInt = 100; AReapPolicy: TCacheReapPolicy = TCacheReapPolicy.crpLeastUsed; AExpirationPolicy: TCacheExpirationPolicy = TCacheExpirationPolicy.epNone; AStalePolicy: TCacheStaleValuePolicy = TCacheStaleValuePolicy.svpAssumeNeverStale) : TTestStringCache;
      procedure Fetching(Sender: TObject; const AKey: Integer);
      procedure Fetched(Sender: TObject; const AKey: Integer; const AValue: AnsiString);
      procedure Removing(Sender: TObject; const AKey: Integer; AItem: TCachedItem<AnsiString>);
      procedure Removed(Sender: TObject; const AKey: Integer; const AValue: AnsiString);
      function FetchObject(const AKey: Integer) : TObject;
      function EstimateObject(const AValue: TObject) : SizeInt;
      procedure ObjectFetched(Sender: TObject; const AKey: Integer; const AValue: TObject);
      function FetchString(const AKey: Integer) : AnsiString;
      function EstimateString(const AValue: AnsiString) : SizeInt;
    protected
      procedure TearDown; override;
    published
      procedure CacheHitFetchesOnce;
      procedure InvalidationRefetches;
      procedure ExpirationSinceFetch;
      procedure ExpirationSinceAccess;
      procedure EmptyStringIsAValue;
      procedure ZeroIntegerIsAValue;
      procedure NullIsCachedNormally;
      procedure NullReturnDoesNotCache;
      procedure NullThrowDoesNotAdmit;
      procedure NullNotificationRetainsCallbackValue;
      procedure LeastUsedEviction;
      procedure LargestEviction;
      procedure SmallestEviction;
      procedure OldestEviction;
      procedure IdleEviction;
      procedure ASAPEvictsEnough;
      procedure ExpiredEntriesAreEvictedFirst;
      procedure NoneOnlyReapsInvalidatedEntries;
      procedure PinsBlockReaping;
      procedure ExplicitRemovalIgnoresPins;
      procedure OversizedValuePreservesCache;
      procedure NegativeSizePreservesCache;
      procedure ReplacementCreditsExistingSize;
      procedure SetPreservesFetchTimestamp;
      procedure BulkLoadDoesNotFetch;
      procedure BulkLoadPreservesEarlierWritesOnFailure;
      procedure BulkLoadRespectsPinsAndCapacity;
      procedure BulkLoadRejectedOwnedValueRemainsCallerOwned;
      procedure BulkLoadHonorsNullPolicies;
      procedure RemoveMissingIsHarmless;
      procedure FlushClearsMembershipAndSize;
      procedure ReapReturnsReleasedSize;
      procedure ZeroSizeFitsZeroCapacity;
      procedure InvalidConstructionRejected;
      procedure StalenessRefetchesOnDemand;
      procedure StalenessHookCanRemoveCurrentEntry;
      procedure StaleRemovedEntryRefetchesSafely;
      procedure FetchEventsFollowAdmission;
      procedure FetchFailureDoesNotAdmit;
      procedure AccessTimestampFollowsNotification;
      procedure RemovingFailureCanRetry;
      procedure RemovedFailureLeavesRemovalCommitted;
      procedure RecursiveRemovalSkipsPendingEntry;
      procedure ReplacementDuringRemovalRejected;
      procedure AdmissionCannotSpendPendingCapacity;
      procedure PurgeRetainsCallbackAddedEntries;
      procedure ObjectDisposalOccursOnce;
      procedure FetchedObjectRemovalDisposesImmediately;
      procedure RejectedFetchDisposesObject;
      procedure RejectedSetRetainsCallerOwnership;
      procedure ConcurrentMissFetchesOnce;
      procedure ConcurrentReadsAndRemovals;
      procedure GlobalAdapters;
      procedure ObjectAdapters;
      procedure NestedAdapters;
  end;

  TBulkCacheTests = class(TTestCase)
  published
    procedure InitialGetLoadsCompleteSnapshot;
    procedure InitialMissingKeyFetchesOnce;
    procedure ContainsLoadsOnlyOnce;
    procedure AllValuesReturnsCopiedSnapshot;
    procedure FetchOnceIgnoresInvalidation;
    procedure FetchOnceIgnoresExpiration;
    procedure FetchOnceIgnoresStaleness;
    procedure FetchOnceMissingDoesNotReload;
    procedure PurgeStartsNewSnapshot;
    procedure NullPoliciesKeepBorrowedOwnership;
    procedure InvalidatedSnapshotReloads;
    procedure ExpiredSnapshotReloads;
    procedure StaleSnapshotReloads;
    procedure ProviderFailurePurgesAndCanRetry;
    procedure PreloadedBorrowsOriginalDictionary;
    procedure MissingKeyThrowFetchesOnce;
    procedure RemoveIsUnsupportedThroughBaseReference;
    procedure GlobalObjectNestedAndInterfaceAdapters;
    procedure NilProvidersRejected;
    procedure ConcurrentReadsShareInitialBatch;
    procedure CallbackReentryIsBounded;
    procedure ConfiguredComparer;
    procedure InitiallyStaleFetchesOnce;
      procedure ReapRetainsInvalidatedFetchOnceEntry;
      procedure ReapRetainsExpiredFetchOnceEntry;
      procedure ReapPreservesRefreshableSnapshot;
      procedure ReapKeepsArgumentAndDestructionGuards;
    procedure UnitScopedPoliciesAreSharedAcrossCacheKinds;
    procedure UnitScopedCallbacksAreSharedAcrossCacheKinds;
  end;

implementation

var
  DisposedObjectCount : Integer;

type
  TTrackedObject = class(TObject)
    destructor Destroy; override;
  end;

  TCacheReadThread = class(TThread)
    private
      FCache : TTestStringCache;
      FMutate : boolean;
    protected
      procedure Execute; override;
    public
      ErrorMessage : AnsiString;
      constructor Create(ACache: TTestStringCache; AMutate: boolean = false);
  end;

constructor TCacheReadThread.Create(ACache: TTestStringCache; AMutate: boolean);
begin
  inherited Create(true);
  FCache := ACache;
  FMutate := AMutate;
end;

procedure TCacheReadThread.Execute;
var
  i : Integer;
begin
  try
    for i := 1 to 1000 do
      if FMutate then
        FCache.Remove(1)
      else if FCache[1] <> 'first' then
        raise EInvalidOperation.Create('Concurrent read returned an incorrect value');
  except
    on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
  end;
end;

destructor TTrackedObject.Destroy;
begin
  Inc(DisposedObjectCount);
  inherited;
end;

function GlobalFetch(const AKey: Integer) : AnsiString;
begin
  Result := IntToStr(AKey);
end;

function GlobalEstimate(const AValue: AnsiString) : SizeInt;
begin
  Result := Length(AValue);
end;

function IntegerFetch(const AKey: Integer) : Integer;
begin
  Result := AKey;
end;

function TTestStringCache.EstimateSize(const AValue: AnsiString) : SizeInt;
begin
  if NegativeSize then Result := -1 else Result := Length(AValue);
end;

function TTestStringCache.Fetch(const AKey: Integer) : AnsiString;
begin
  Inc(FetchCount);
  if RaiseOnFetch then
    raise EInvalidOperation.Create('fetch failed');
  FetchCompletedOn := Now;
  Result := FetchValue;
end;

function TTestStringCache.CheckStaleness(const AKey: Integer; AItem: TCachedItem<AnsiString>) : boolean;
begin
  if RemoveDuringStaleness then
    Remove(AKey);
  Result := Stale;
end;

function TCacheTests.NewCache(ACapacity: SizeInt; AReapPolicy: TCacheReapPolicy; AExpirationPolicy: TCacheExpirationPolicy; AStalePolicy: TCacheStaleValuePolicy) : TTestStringCache;
begin
  FreeAndNil(FCache);
  FCache := TTestStringCache.Create(nil, idpNone, AReapPolicy, AExpirationPolicy, TCacheNullValuePolicy.nvpCacheNormally, ACapacity, TTimeSpan.FromSeconds(1), 0.1, AStalePolicy);
  FCache.FetchValue := 'first';
  Result := FCache;
end;

procedure TCacheTests.TearDown;
begin
  FreeAndNil(FCache);
  inherited;
end;

procedure TCacheTests.Fetching(Sender: TObject; const AKey: Integer);
begin
  Inc(FFetchingCount);
  AssertFalse(FCache.ContainsCachedItem(AKey));
end;

procedure TCacheTests.Fetched(Sender: TObject; const AKey: Integer; const AValue: AnsiString);
begin
  Inc(FFetchedCount);
  AssertTrue('Fetched notification follows storage', FCache.ContainsCachedItem(AKey));
  FNotificationOn := Now;
end;

procedure TCacheTests.Removing(Sender: TObject; const AKey: Integer; AItem: TCachedItem<AnsiString>);
begin
  Inc(FRemovingCount);
  AssertTrue('Removal preparation retains membership', FCache.ContainsCachedItem(AKey));
  AssertTrue(FCache.CurrentSize >= AItem.Size);
  case FCallbackMode of
    1: raise EInvalidOperation.Create('removal preparation failed');
    2: begin
      FCache.Remove(AKey);
      FCache.Flush;
      FCache.Reap(100);
    end;
    3: FCache[AKey] := 'replacement';
    4: FCache[99] := 'new';
  end;
end;

procedure TCacheTests.Removed(Sender: TObject; const AKey: Integer; const AValue: AnsiString);
begin
  Inc(FRemovedCount);
  AssertFalse('Removed notification follows membership removal', FCache.ContainsCachedItem(AKey));
  if FCallbackMode = 5 then
    raise EInvalidOperation.Create('removal notification failed');
  if FCallbackMode = 6 then
    FCache[99] := 'new';
end;

function TCacheTests.FetchObject(const AKey: Integer) : TObject;
begin
  Inc(FObjectFetchCount);
  if FCreateObjects then Result := TTrackedObject.Create else Result := nil;
end;

function TCacheTests.EstimateObject(const AValue: TObject) : SizeInt;
begin
  Result := FObjectSize;
end;

procedure TCacheTests.ObjectFetched(Sender: TObject; const AKey: Integer; const AValue: TObject);
begin
  if not Assigned(AValue) then
    TObjectActionCache(Sender)[AKey] := Self;
end;

function TCacheTests.FetchString(const AKey: Integer) : AnsiString;
begin
  Result := IntToStr(AKey);
end;

function TCacheTests.EstimateString(const AValue: AnsiString) : SizeInt;
begin
  Result := Length(AValue);
end;

procedure TCacheTests.CacheHitFetchesOnce;
begin
  NewCache;
  AssertEquals('first', FCache[1]);
  FCache.FetchValue := 'second';
  AssertEquals('first', FCache[1]);
  AssertEquals(1, FCache.FetchCount);
  AssertEquals(1, FCache.ItemCount);
  AssertEquals(2, Integer(FCache.Get(1).AccessedCount) - 1);
  AssertEquals(3, Integer(FCache.TotalAccesses));
end;

procedure TCacheTests.InvalidationRefetches;
begin
  NewCache;
  FCache.Get(1);
  FCache.Invalidate(1);
  FCache.Invalidate(999);
  AssertFalse(FCache.ContainsCachedItem(1));
  AssertEquals(1, FCache.ItemCount);
  FCache.FetchValue := 'second';
  AssertEquals('second', FCache[1]);
  AssertEquals(2, FCache.FetchCount);
  AssertEquals(6, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.ExpirationSinceFetch;
begin
  NewCache(100, TCacheReapPolicy.crpLeastUsed, TCacheExpirationPolicy.epSinceFetchedTime);
  FCache.Get(1).FFetchedOn := Now - 1;
  AssertFalse(FCache.ContainsCachedItem(1));
  FCache.FetchValue := 'second';
  AssertEquals('second', FCache[1]);
  AssertEquals(2, FCache.FetchCount);
end;

procedure TCacheTests.ExpirationSinceAccess;
begin
  NewCache(100, TCacheReapPolicy.crpLeastUsed, TCacheExpirationPolicy.epSinceLastAccessedTime);
  FCache.Get(1).FFetchedOn := Now - 1;
  AssertTrue(FCache.ContainsCachedItem(1));
  FCache.Get(1).FLastAccessedOn := Now - 1;
  AssertFalse(FCache.ContainsCachedItem(1));
  AssertEquals('first', FCache[1]);
  AssertEquals(2, FCache.FetchCount);
end;

procedure TCacheTests.EmptyStringIsAValue;
begin
  NewCache;
  FCache.FetchValue := '';
  AssertEquals('', FCache[1]);
  AssertEquals('', FCache[1]);
  AssertEquals(1, FCache.FetchCount);
end;

procedure TCacheTests.ZeroIntegerIsAValue;
type
  TIntegerCache = TActionCache<Integer, Integer>;
var
  cache : TIntegerCache;
begin
  cache := TIntegerCache.Create(nil, TCacheValueFetcher<Integer,Integer>.FromGlobalFunction(IntegerFetch), nil, idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpThrow, 0, TTimeSpan.MaxValue);
  try
    AssertEquals(0, cache[0]);
    AssertEquals(1, cache.ItemCount);
  finally
    cache.Free;
  end;
end;

procedure TCacheTests.NullIsCachedNormally;
var
  cache : TObjectActionCache;
begin
  cache := TObjectActionCache.Create(nil, TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject), nil, idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, 10, TTimeSpan.MaxValue);
  try
    AssertNull(cache[1]);
    AssertNull(cache[1]);
    AssertEquals(1, FObjectFetchCount);
    AssertEquals(1, cache.ItemCount);
  finally
    cache.Free;
  end;
end;

procedure TCacheTests.NullReturnDoesNotCache;
var
  cache : TObjectActionCache;
begin
  cache := TObjectActionCache.Create(nil, TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject), nil, idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpReturnButDontCache, 10, TTimeSpan.MaxValue);
  try
    AssertNull(cache[1]);
    AssertNull(cache[1]);
    AssertEquals(2, FObjectFetchCount);
    AssertEquals(0, cache.ItemCount);
    AssertEquals(0, Integer(cache.CurrentSize));
  finally
    cache.Free;
  end;
end;

procedure TCacheTests.NullThrowDoesNotAdmit;
var
  cache : TObjectActionCache;
begin
  cache := TObjectActionCache.Create(nil, TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject), nil, idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpThrow, 10, TTimeSpan.MaxValue);
  try
    try
      cache.Get(1);
      Fail('Null fetch must be rejected');
    except
      on E: EInvalidOperation do ;
    end;
    AssertEquals(0, cache.ItemCount);
  finally
    cache.Free;
  end;
end;

procedure TCacheTests.NullNotificationRetainsCallbackValue;
var
  cache : TObjectActionCache;
begin
  cache := TObjectActionCache.Create(nil, TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject), nil, idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpReturnButDontCache, 10, TTimeSpan.MaxValue);
  try
    cache.ItemFetched := ObjectFetched;
    AssertNull(cache[1]);
    AssertTrue(cache.ContainsCachedItem(1));
    AssertTrue(cache[1] = Self);
    AssertEquals(1, FObjectFetchCount);
  finally
    cache.Free;
  end;
end;

procedure TCacheTests.LeastUsedEviction;
begin
  NewCache(3);
  FCache[1] := 'a';
  FCache[2] := 'b';
  FCache[3] := 'c';
  FCache.Get(1);
  FCache.Get(3);
  FCache[4] := 'd';
  AssertFalse(FCache.ContainsCachedItem(2));
  AssertTrue(FCache.ContainsCachedItem(1));
  AssertTrue(FCache.ContainsCachedItem(3));
  AssertEquals(3, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.LargestEviction;
begin
  NewCache(6, TCacheReapPolicy.crpLargest);
  FCache[1] := 'a';
  FCache[2] := 'bb';
  FCache[3] := 'ccc';
  AssertEquals(3, Integer(FCache.Reap(1)));
  AssertFalse(FCache.ContainsCachedItem(3));
end;

procedure TCacheTests.SmallestEviction;
begin
  NewCache(6, TCacheReapPolicy.crpSmallest);
  FCache[1] := 'a';
  FCache[2] := 'bb';
  FCache[3] := 'ccc';
  AssertEquals(1, Integer(FCache.Reap(1)));
  AssertFalse(FCache.ContainsCachedItem(1));
end;

procedure TCacheTests.OldestEviction;
begin
  NewCache(3, TCacheReapPolicy.crpOldest);
  FCache[1] := 'a';
  FCache[2] := 'b';
  FCache[3] := 'c';
  FCache.Get(1);
  FCache[4] := 'd';
  AssertFalse(FCache.ContainsCachedItem(1));
end;

procedure TCacheTests.IdleEviction;
begin
  NewCache(3, TCacheReapPolicy.crpIdle);
  FCache[1] := 'a';
  FCache[2] := 'b';
  FCache[3] := 'c';
  FCache.Get(1);
  FCache[4] := 'd';
  AssertFalse(FCache.ContainsCachedItem(2));
end;

procedure TCacheTests.ASAPEVIctsEnough;
begin
  NewCache(5, TCacheReapPolicy.crpASAP);
  FCache[1] := 'a';
  FCache[2] := 'b';
  FCache[3] := 'c';
  FCache[4] := 'd';
  FCache[5] := 'e';
  FCache[6] := 'long';
  AssertEquals(5, Integer(FCache.CurrentSize));
  AssertEquals(2, FCache.ItemCount);
  AssertTrue(FCache.ContainsCachedItem(6));
end;

procedure TCacheTests.ExpiredEntriesAreEvictedFirst;
begin
  NewCache(3, TCacheReapPolicy.crpOldest);
  FCache[1] := 'a';
  FCache[2] := 'b';
  FCache[3] := 'c';
  FCache.Invalidate(3);
  FCache[4] := 'd';
  AssertTrue(FCache.ContainsCachedItem(1));
  AssertEquals(3, FCache.ItemCount);
  AssertFalse(FCache.CachedItems[0].Invalidated);
end;

procedure TCacheTests.NoneOnlyReapsInvalidatedEntries;
begin
  NewCache(2, TCacheReapPolicy.crpNone);
  FCache[1] := 'a';
  FCache[2] := 'b';
  AssertEquals(0, Integer(FCache.Reap(2)));
  FCache.Invalidate(1);
  AssertEquals(1, Integer(FCache.Reap(2)));
  AssertTrue(FCache.ContainsCachedItem(2));
end;

procedure TCacheTests.PinsBlockReaping;
begin
  NewCache(1);
  FCache[1] := 'a';
  FCache.Get(1).CanPurge := false;
  AssertEquals(0, Integer(FCache.Reap(1)));
  try
    FCache[2] := 'b';
    Fail('A pinned cache cannot admit another value');
  except
    on E: EInvalidOperation do ;
  end;
  AssertTrue(FCache.ContainsCachedItem(1));
  AssertEquals(1, FCache.ItemCount);
end;

procedure TCacheTests.ExplicitRemovalIgnoresPins;
begin
  NewCache(2);
  FCache[1] := 'a';
  FCache[2] := 'b';
  FCache.Get(1).CanPurge := false;
  FCache.Get(2).CanPurge := false;
  FCache.Remove(1);
  FCache.Flush;
  AssertEquals(0, FCache.ItemCount);
end;

procedure TCacheTests.OversizedValuePreservesCache;
begin
  NewCache(2);
  FCache[1] := 'a';
  try
    FCache[2] := 'big';
    Fail('Oversized values must be rejected');
  except
    on E: EInvalidOperation do ;
  end;
  AssertEquals('a', FCache[1]);
  AssertEquals(1, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.NegativeSizePreservesCache;
begin
  NewCache;
  FCache[1] := 'a';
  FCache.NegativeSize := true;
  try
    FCache[1] := 'replacement';
    Fail('Negative estimates must be rejected');
  except
    on E: EInvalidOperation do ;
  end;
  AssertEquals('a', FCache[1]);
  AssertEquals(1, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.ReplacementCreditsExistingSize;
begin
  NewCache(3, TCacheReapPolicy.crpNone);
  FCache[1] := 'aa';
  FCache[2] := 'b';
  FCache[1] := 'cc';
  AssertEquals(2, FCache.ItemCount);
  AssertEquals(3, Integer(FCache.CurrentSize));
  AssertEquals('cc', FCache[1]);
end;

procedure TCacheTests.SetPreservesFetchTimestamp;
var
  item : TCachedItem<AnsiString>;
  timestamp : TDateTime;
begin
  NewCache;
  item := FCache.Get(1);
  timestamp := Now - 1;
  item.FFetchedOn := timestamp;
  FCache[1] := 'replacement';
  AssertTrue(item = FCache.Get(1));
  AssertTrue(timestamp = item.FetchedOn);
  AssertEquals(1, FCache.FetchCount);
end;

procedure TCacheTests.BulkLoadDoesNotFetch;
var
  values : TStringPairList;
begin
  NewCache;
  FCache.ItemFetched := Fetched;
  values := TStringPairList.Create;
  try
    values.Add(TStringPair.Create(1, 'a'));
    values.Add(TStringPair.Create(2, 'bb'));
    values.Add(TStringPair.Create(1, 'ccc'));
    FCache.BulkLoad(values);
    AssertEquals('ccc', FCache[1]);
    AssertEquals(0, FCache.FetchCount);
    AssertEquals(0, FFetchedCount);
    AssertEquals(2, FCache.ItemCount);
    AssertEquals(5, Integer(FCache.CurrentSize));
  finally
    values.Free;
  end;
end;

procedure TCacheTests.BulkLoadPreservesEarlierWritesOnFailure;
var
  values : TStringPairList;
begin
  NewCache(3, TCacheReapPolicy.crpOldest);
  values := TStringPairList.Create;
  try
    values.Add(TStringPair.Create(1, 'a'));
    values.Add(TStringPair.Create(2, 'oversized'));
    values.Add(TStringPair.Create(3, 'b'));
    try
      FCache.BulkLoad(values);
      Fail('An oversized batch value must be rejected');
    except
      on E: EInvalidOperation do ;
    end;
    AssertEquals('a', FCache[1]);
    AssertFalse(FCache.ContainsCachedItem(2));
    AssertFalse(FCache.ContainsCachedItem(3));
    AssertEquals(1, FCache.ItemCount);
    AssertEquals(1, Integer(FCache.CurrentSize));
    AssertEquals(0, FCache.FetchCount);
  finally values.Free; end;
end;

procedure TCacheTests.BulkLoadRespectsPinsAndCapacity;
var
  values : TStringPairList;
begin
  NewCache(6, TCacheReapPolicy.crpOldest);
  FCache[1] := 'pin';
  FCache.Get(1).CanPurge := false;
  values := TStringPairList.Create;
  try
    values.Add(TStringPair.Create(2, 'two'));
    values.Add(TStringPair.Create(3, 'new'));
    FCache.BulkLoad(values);
    AssertEquals('pin', FCache[1]);
    AssertFalse(FCache.ContainsCachedItem(2));
    AssertEquals('new', FCache[3]);
    AssertEquals(2, FCache.ItemCount);
    AssertEquals(6, Integer(FCache.CurrentSize));
    AssertEquals(0, FCache.FetchCount);
  finally values.Free; end;
end;

procedure TCacheTests.BulkLoadRejectedOwnedValueRemainsCallerOwned;
type
  TObjectPair = TPair<Integer, TObject>;
var
  cache : TObjectActionCache;
  values : TList<TObjectPair>;
  first, second : TObject;
begin
  DisposedObjectCount := 0;
  FObjectSize := 1;
  cache := TObjectActionCache.Create(nil,
    TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject),
    TCacheSizeEstimator<TObject>.FromObjectFunction(EstimateObject),
    idpFreeAndNil, TCacheReapPolicy.crpNone,
    TCacheExpirationPolicy.epNone,
    TCacheNullValuePolicy.nvpCacheNormally, 1, TTimeSpan.MaxValue);
  values := TList<TObjectPair>.Create;
  first := TTrackedObject.Create;
  second := TTrackedObject.Create;
  try
    values.Add(TObjectPair.Create(1, first));
    values.Add(TObjectPair.Create(2, second));
    try
      cache.BulkLoad(values);
      Fail('A full non-reaping cache must reject the second batch value');
    except
      on E: EInvalidOperation do ;
    end;
    AssertEquals(0, DisposedObjectCount);
    AssertTrue(first = cache[1]);
    AssertFalse(cache.ContainsCachedItem(2));
    second.Free;
    second := nil;
    AssertEquals(1, DisposedObjectCount);
  finally
    // Only admitted values transfer ownership; the list itself owns no values.
    if Assigned(second) and (not cache.ContainsCachedItem(2)) then second.Free;
    cache.Free;
    values.Free;
  end;
  AssertEquals(2, DisposedObjectCount);
end;

procedure TCacheTests.BulkLoadHonorsNullPolicies;
type
  TObjectPair = TPair<Integer, TObject>;
var
  cache : TObjectActionCache;
  values : TList<TObjectPair>;
  value : TObject;
  policy : TCacheNullValuePolicy;
begin
  FObjectFetchCount := 0;
  values := TList<TObjectPair>.Create;
  value := TObject.Create;
  try
    values.Add(TObjectPair.Create(1, nil));
    values.Add(TObjectPair.Create(2, value));
    for policy := Low(TCacheNullValuePolicy) to High(TCacheNullValuePolicy) do begin
      cache := TObjectActionCache.Create(nil,
        TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject), nil,
        idpNone, TCacheReapPolicy.crpNone,
        TCacheExpirationPolicy.epNone, policy, 1, TTimeSpan.MaxValue);
      try
        // Prefetched nulls obey the configured policy when read, just as SetValue does.
        cache.BulkLoad(values);
        AssertEquals(2, cache.ItemCount);
        if policy = TCacheNullValuePolicy.nvpThrow then begin
          try
            cache.Get(1);
            Fail('A prohibited prefetched null must throw on read');
          except on E: EInvalidOperation do ; end;
          AssertEquals(2, cache.ItemCount);
        end else begin
          AssertNull(cache[1]);
          AssertEquals(policy = TCacheNullValuePolicy.nvpCacheNormally,
            cache.ContainsCachedItem(1));
        end;
        AssertTrue(value = cache[2]);
      finally cache.Free; end;
    end;
    AssertEquals(0, FObjectFetchCount);
  finally value.Free; values.Free; end;
end;

procedure TCacheTests.RemoveMissingIsHarmless;
begin
  NewCache;
  FCache.Remove(1);
  AssertEquals(0, FCache.ItemCount);
end;

procedure TCacheTests.FlushClearsMembershipAndSize;
begin
  NewCache;
  FCache[1] := 'a';
  FCache[2] := 'bb';
  FCache.Flush;
  AssertEquals(0, FCache.ItemCount);
  AssertEquals(0, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.ReapReturnsReleasedSize;
begin
  NewCache;
  FCache[1] := 'aa';
  FCache[2] := 'bb';
  AssertEquals(2, Integer(FCache.Reap(1)));
  AssertEquals(2, Integer(FCache.CurrentSize));
  AssertEquals(0, Integer(FCache.Reap(0)));
end;

procedure TCacheTests.ZeroSizeFitsZeroCapacity;
begin
  NewCache(0);
  FCache[1] := '';
  FCache[2] := '';
  AssertEquals(2, FCache.ItemCount);
  AssertEquals(0, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.InvalidConstructionRejected;
var
  cache : TTestStringCache;
begin
  cache := nil;
  try
    try
      cache := TTestStringCache.Create(nil, idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, -1, TTimeSpan.MaxValue);
      Fail('Negative capacity must be rejected');
    except
      on E: EArgumentException do ;
    end;
    try
      cache := TTestStringCache.Create(nil, idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, 1, TTimeSpan.MaxValue, 1);
      Fail('Invalid reap level must be rejected');
    except
      on E: EArgumentException do ;
    end;
  finally
    cache.Free;
  end;
end;

procedure TCacheTests.StalenessRefetchesOnDemand;
begin
  NewCache(100, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheStaleValuePolicy.svpCheckStaleOnDemand);
  FCache.Get(1);
  FCache.Stale := true;
  FCache.FetchValue := 'second';
  AssertEquals('second', FCache[1]);
  AssertEquals(2, FCache.FetchCount);
end;

procedure TCacheTests.StalenessHookCanRemoveCurrentEntry;
var
  item : TCachedItem<AnsiString>;
begin
  NewCache(100, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheStaleValuePolicy.svpCheckStaleOnDemand);
  FCache[1] := 'original';
  FCache.RemoveDuringStaleness := true;
  item := FCache.Get(1);
  AssertEquals('original', item.Value);
  AssertTrue(item.Purged);
  AssertEquals(0, FCache.ItemCount);
  AssertEquals(0, FCache.FetchCount);
  AssertEquals(0, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.StaleRemovedEntryRefetchesSafely;
var
  item : TCachedItem<AnsiString>;
begin
  NewCache(100, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheStaleValuePolicy.svpCheckStaleOnDemand);
  FCache[1] := 'original';
  FCache.RemoveDuringStaleness := true;
  FCache.Stale := true;
  item := FCache.Get(1);
  AssertEquals('first', item.Value);
  AssertFalse(item.Purged);
  AssertTrue(FCache.ContainsCachedItem(1));
  AssertEquals(1, FCache.FetchCount);
  AssertEquals(5, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.FetchEventsFollowAdmission;
begin
  NewCache;
  FCache.ItemFetching := Fetching;
  FCache.ItemFetched := Fetched;
  FCache.Get(1);
  FCache.Get(1);
  AssertEquals(1, FFetchingCount);
  AssertEquals(1, FFetchedCount);
  AssertTrue(FCache.Get(1).FetchedOn >= FCache.FetchCompletedOn);
end;

procedure TCacheTests.FetchFailureDoesNotAdmit;
begin
  NewCache;
  FCache.RaiseOnFetch := true;
  FCache.ItemFetching := Fetching;
  FCache.ItemFetched := Fetched;
  try
    FCache.Get(1);
    Fail('Fetch failure must propagate');
  except
    on E: EInvalidOperation do ;
  end;
  AssertEquals(1, FFetchingCount);
  AssertEquals(0, FFetchedCount);
  AssertEquals(0, FCache.ItemCount);
end;

procedure TCacheTests.AccessTimestampFollowsNotification;
var
  item : TCachedItem<AnsiString>;
begin
  NewCache;
  FCache.ItemFetched := Fetched;
  item := FCache.Get(1);
  AssertTrue(item.LastAccessedOn >= FNotificationOn);
  AssertTrue(FCache.LastAccessedOn >= FNotificationOn);
end;

procedure TCacheTests.RemovingFailureCanRetry;
begin
  NewCache;
  FCache[1] := 'a';
  FCache.ItemRemoving := Removing;
  FCache.ItemRemoved := Removed;
  FCallbackMode := 1;
  try
    FCache.Remove(1);
    Fail('Preparation failure must propagate');
  except
    on E: EInvalidOperation do ;
  end;
  AssertEquals(1, FCache.ItemCount);
  AssertEquals(1, Integer(FCache.CurrentSize));
  AssertEquals(0, FRemovedCount);
  FCallbackMode := 0;
  FCache.Remove(1);
  AssertEquals(2, FRemovingCount);
  AssertEquals(1, FRemovedCount);
  AssertEquals(0, FCache.ItemCount);
end;

procedure TCacheTests.RemovedFailureLeavesRemovalCommitted;
begin
  NewCache;
  FCache[1] := 'a';
  FCache.ItemRemoved := Removed;
  FCallbackMode := 5;
  try
    FCache.Remove(1);
    Fail('Notification failure must propagate');
  except
    on E: EInvalidOperation do ;
  end;
  AssertEquals(0, FCache.ItemCount);
  AssertEquals(0, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.RecursiveRemovalSkipsPendingEntry;
begin
  NewCache;
  FCache[1] := 'a';
  FCache.ItemRemoving := Removing;
  FCache.ItemRemoved := Removed;
  FCallbackMode := 2;
  FCache.Remove(1);
  AssertEquals(1, FRemovingCount);
  AssertEquals(1, FRemovedCount);
  AssertEquals(0, FCache.ItemCount);
end;

procedure TCacheTests.ReplacementDuringRemovalRejected;
begin
  NewCache;
  FCache[1] := 'a';
  FCache.ItemRemoving := Removing;
  FCallbackMode := 3;
  try
    FCache.Remove(1);
    Fail('Replacing an entry while preparing removal must fail');
  except
    on E: EInvalidOperation do ;
  end;
  AssertEquals('a', FCache[1]);
  AssertEquals(1, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.AdmissionCannotSpendPendingCapacity;
begin
  NewCache(3);
  FCache[1] := 'aaa';
  FCache.ItemRemoving := Removing;
  FCallbackMode := 4;
  try
    FCache.Remove(1);
    Fail('Pending removals cannot fund admissions');
  except
    on E: EInvalidOperation do ;
  end;
  AssertTrue(FCache.ContainsCachedItem(1));
  AssertFalse(FCache.ContainsCachedItem(99));
  AssertEquals(3, Integer(FCache.CurrentSize));
end;

procedure TCacheTests.PurgeRetainsCallbackAddedEntries;
begin
  NewCache;
  FCache[1] := 'a';
  FCache.ItemRemoved := Removed;
  FCallbackMode := 6;
  FCache.Purge;
  AssertEquals(1, FCache.ItemCount);
  AssertEquals('new', FCache[99]);
end;

procedure TCacheTests.ObjectDisposalOccursOnce;
var
  cache : TObjectActionCache;
  first, second : TObject;
begin
  DisposedObjectCount := 0;
  cache := TObjectActionCache.Create(nil, TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject), nil, idpFreeAndNil, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, 10, TTimeSpan.MaxValue);
  first := TTrackedObject.Create;
  second := TTrackedObject.Create;
  try
    cache[1] := first;
    cache[1] := first;
    AssertEquals(0, DisposedObjectCount);
    cache[1] := second;
    AssertEquals(1, DisposedObjectCount);
    cache.Remove(1);
    AssertEquals(2, DisposedObjectCount);
  finally
    cache.Free;
  end;
  AssertEquals(2, DisposedObjectCount);
end;

procedure TCacheTests.FetchedObjectRemovalDisposesImmediately;
var
  cache : TObjectActionCache;
begin
  DisposedObjectCount := 0;
  FCreateObjects := true;
  cache := TObjectActionCache.Create(nil, TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject), nil, idpFreeAndNil, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, 1, TTimeSpan.MaxValue);
  try
    AssertNotNull(cache[1]);
    cache.Remove(1);
    AssertEquals(1, DisposedObjectCount);
  finally
    cache.Free;
  end;
  AssertEquals(1, DisposedObjectCount);
end;

procedure TCacheTests.RejectedFetchDisposesObject;
var
  cache : TObjectActionCache;
begin
  DisposedObjectCount := 0;
  FCreateObjects := true;
  FObjectSize := 2;
  cache := TObjectActionCache.Create(nil, TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject), TCacheSizeEstimator<TObject>.FromObjectFunction(EstimateObject), idpFreeAndNil, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, 1, TTimeSpan.MaxValue);
  try
    try
      cache.Get(1);
      Fail('Oversized fetched object must be rejected');
    except
      on E: EInvalidOperation do ;
    end;
    AssertEquals(1, DisposedObjectCount);
    AssertEquals(0, cache.ItemCount);
  finally
    cache.Free;
  end;
  AssertEquals(1, DisposedObjectCount);
end;

procedure TCacheTests.RejectedSetRetainsCallerOwnership;
var
  cache : TObjectActionCache;
  value : TObject;
begin
  DisposedObjectCount := 0;
  FObjectSize := 2;
  value := TTrackedObject.Create;
  cache := TObjectActionCache.Create(nil, TCacheValueFetcher<Integer,TObject>.FromObjectFunction(FetchObject), TCacheSizeEstimator<TObject>.FromObjectFunction(EstimateObject), idpFreeAndNil, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, 1, TTimeSpan.MaxValue);
  try
    try
      cache[1] := value;
      Fail('Oversized explicit object must be rejected');
    except
      on E: EInvalidOperation do ;
    end;
    AssertEquals(0, DisposedObjectCount);
    AssertEquals(0, cache.ItemCount);
  finally
    cache.Free;
    value.Free;
  end;
  AssertEquals(1, DisposedObjectCount);
end;

procedure TCacheTests.ConcurrentMissFetchesOnce;
var
  readers : array[0..3] of TCacheReadThread;
  i : Integer;
begin
  NewCache;
  for i := Low(readers) to High(readers) do
    readers[i] := TCacheReadThread.Create(FCache);
  try
    for i := Low(readers) to High(readers) do
      readers[i].Start;
    for i := Low(readers) to High(readers) do
      readers[i].WaitFor;
    for i := Low(readers) to High(readers) do
      AssertEquals('', readers[i].ErrorMessage);
    AssertEquals(1, FCache.FetchCount);
    AssertEquals(4000, Integer(FCache.TotalAccesses));
  finally
    for i := Low(readers) to High(readers) do
      readers[i].Free;
  end;
end;

procedure TCacheTests.ConcurrentReadsAndRemovals;
var
  readers : array[0..3] of TCacheReadThread;
  i : Integer;
begin
  NewCache;
  for i := Low(readers) to High(readers) do
    readers[i] := TCacheReadThread.Create(FCache, i = 0);
  try
    for i := Low(readers) to High(readers) do
      readers[i].Start;
    for i := Low(readers) to High(readers) do
      readers[i].WaitFor;
    for i := Low(readers) to High(readers) do
      AssertEquals('', readers[i].ErrorMessage);
    AssertEquals(3000, Integer(FCache.TotalAccesses));
  finally
    for i := Low(readers) to High(readers) do
      readers[i].Free;
  end;
end;

procedure TCacheTests.GlobalAdapters;
var
  cache : TStringActionCache;
begin
  cache := TStringActionCache.Create(nil, TCacheValueFetcher<Integer,AnsiString>.FromGlobalFunction(GlobalFetch), TCacheSizeEstimator<AnsiString>.FromGlobalFunction(GlobalEstimate), idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, 10, TTimeSpan.MaxValue);
  try
    AssertEquals('123', cache[123]);
    AssertEquals(3, Integer(cache.CurrentSize));
  finally
    cache.Free;
  end;
end;

procedure TCacheTests.ObjectAdapters;
var
  cache : TStringActionCache;
begin
  cache := TStringActionCache.Create(nil, TCacheValueFetcher<Integer,AnsiString>.FromObjectFunction(FetchString), TCacheSizeEstimator<AnsiString>.FromObjectFunction(EstimateString), idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, 10, TTimeSpan.MaxValue);
  try
    AssertEquals('123', cache[123]);
    AssertEquals(3, Integer(cache.CurrentSize));
  finally
    cache.Free;
  end;
end;

procedure TCacheTests.NestedAdapters;
var
  cache : TStringActionCache;
  suffix : AnsiString;
  function Fetch(const AKey: Integer) : AnsiString;
  begin
    Result := IntToStr(AKey) + suffix;
  end;
  function Estimate(const AValue: AnsiString) : SizeInt;
  begin
    Result := Length(AValue);
  end;
begin
  suffix := '!';
  cache := TStringActionCache.Create(nil, TCacheValueFetcher<Integer,AnsiString>.FromNestedFunction(Fetch), TCacheSizeEstimator<AnsiString>.FromNestedFunction(Estimate), idpNone, TCacheReapPolicy.crpNone, TCacheExpirationPolicy.epNone, TCacheNullValuePolicy.nvpCacheNormally, 10, TTimeSpan.MaxValue);
  try
    AssertEquals('123!', cache[123]);
    AssertEquals(4, Integer(cache.CurrentSize));
  finally
    cache.Free;
  end;
end;

procedure TBulkCacheTests.InitialGetLoadsCompleteSnapshot;
begin
  RunBulkCacheCheck(1);
end;

procedure TBulkCacheTests.InitialMissingKeyFetchesOnce;
begin
  RunBulkCacheCheck(2);
end;

procedure TBulkCacheTests.ContainsLoadsOnlyOnce;
begin
  RunBulkCacheCheck(3);
end;

procedure TBulkCacheTests.AllValuesReturnsCopiedSnapshot;
begin
  RunBulkCacheCheck(4);
end;

procedure TBulkCacheTests.FetchOnceIgnoresInvalidation;
begin
  RunBulkCacheCheck(5);
end;

procedure TBulkCacheTests.FetchOnceIgnoresExpiration;
begin
  RunBulkCacheCheck(6);
end;

procedure TBulkCacheTests.FetchOnceIgnoresStaleness;
begin
  RunBulkCacheCheck(7);
end;

procedure TBulkCacheTests.FetchOnceMissingDoesNotReload;
begin
  RunBulkCacheCheck(8);
end;

procedure TBulkCacheTests.PurgeStartsNewSnapshot;
begin
  RunBulkCacheCheck(9);
end;

procedure TBulkCacheTests.NullPoliciesKeepBorrowedOwnership;
begin
  RunBulkCacheCheck(10);
end;

procedure TBulkCacheTests.InvalidatedSnapshotReloads;
begin
  RunBulkCacheCheck(11);
end;

procedure TBulkCacheTests.ExpiredSnapshotReloads;
begin
  RunBulkCacheCheck(12);
end;

procedure TBulkCacheTests.StaleSnapshotReloads;
begin
  RunBulkCacheCheck(13);
end;

procedure TBulkCacheTests.ProviderFailurePurgesAndCanRetry;
begin
  RunBulkCacheCheck(14);
end;

procedure TBulkCacheTests.PreloadedBorrowsOriginalDictionary;
begin
  RunBulkCacheCheck(15);
end;

procedure TBulkCacheTests.MissingKeyThrowFetchesOnce;
begin
  RunBulkCacheCheck(16);
end;

procedure TBulkCacheTests.RemoveIsUnsupportedThroughBaseReference;
begin
  RunBulkCacheCheck(17);
end;

procedure TBulkCacheTests.GlobalObjectNestedAndInterfaceAdapters;
begin
  RunBulkCacheCheck(18);
end;

procedure TBulkCacheTests.NilProvidersRejected;
begin
  RunBulkCacheCheck(19);
end;

procedure TBulkCacheTests.ConcurrentReadsShareInitialBatch;
begin
  RunBulkCacheCheck(20);
end;

procedure TBulkCacheTests.CallbackReentryIsBounded;
begin
  RunBulkCacheCheck(21);
end;

procedure TBulkCacheTests.ConfiguredComparer;
begin
  RunBulkCacheCheck(22);
end;

procedure TBulkCacheTests.InitiallyStaleFetchesOnce;
begin
  RunBulkCacheCheck(23);
end;

procedure TBulkCacheTests.ReapRetainsInvalidatedFetchOnceEntry;
begin
  RunBulkCacheCheck(24);
end;

procedure TBulkCacheTests.ReapRetainsExpiredFetchOnceEntry;
begin
  RunBulkCacheCheck(25);
end;

procedure TBulkCacheTests.ReapPreservesRefreshableSnapshot;
begin
  RunBulkCacheCheck(26);
end;

procedure TBulkCacheTests.ReapKeepsArgumentAndDestructionGuards;
begin
  RunBulkCacheCheck(27);
end;

procedure TBulkCacheTests.UnitScopedPoliciesAreSharedAcrossCacheKinds;
begin
  RunBulkCacheCheck(28);
end;

procedure TBulkCacheTests.UnitScopedCallbacksAreSharedAcrossCacheKinds;
begin
  RunBulkCacheCheck(29);
end;

initialization
  RegisterTest(TCacheTests);
  RegisterTest(TBulkCacheTests);
end.
