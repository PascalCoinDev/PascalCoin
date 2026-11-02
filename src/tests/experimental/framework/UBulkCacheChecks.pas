unit UBulkCacheChecks;

{ Copyright © 2026 Herman Schoenfeld <https://www.sphere10.com>
  Distributed under the MIT software license, see LICENSE. }

{$IFDEF FPC}
  {$MODE Delphi}
  {$MODESWITCH NESTEDPROCVARS}
{$ENDIF}

interface

const BulkCacheCheckCount = 29;

// The same assertions run from FPCUnit and the native Delphi probe.
procedure RunBulkCacheCheck(AIndex: Integer);

implementation

uses
  {$IFDEF FPC}Classes, SysUtils, Generics.Collections, Generics.Defaults,
  {$ELSE}System.Classes, System.SysUtils, System.Generics.Collections,
  System.Generics.Defaults, System.TimeSpan,{$ENDIF}
  UCommon, UMemory, UCache;

type
  TPlainBase = TCacheBase<Integer, String>;
  TBulkBase = TBulkFetchCacheBase<Integer, String>;
  TBulkAction = TBulkFetchActionCache<Integer, String>;
  TPerKeyAction = TActionCache<Integer, String>;
  TScalarAction = TActionCache<String, Integer>;
  TValues = TCacheBulkValues<Integer, String>;
  TTestBulk = class(TBulkBase)
  public
    Source: TValues;
    Calls, StaleChecks: Integer;
    RaiseOnFetch, Stale, ReenterRefresh, ReenterRead: Boolean;
  protected
    procedure BulkFetch(AValues: TValues); override;
    function CheckStaleness(const AKey: Integer; AItem: TCachedItem<String>): Boolean; override;
  end;

  TContext = class
  private
    FResources: TDisposables;
  public
    Source: TValues;
    Cache: TTestBulk;
    FetchingCount, FetchedCount, RemovingCount, RemovedCount: Integer;
    constructor Create(AOnce: Boolean;
      AExpiry: TCacheExpirationPolicy;
      AStale: TCacheStaleValuePolicy;
      ANull: TCacheNullValuePolicy);
    procedure FillValues(AValues: TValues);
    function FetchValue(const AKey: Integer): String;
    function EstimateValue(const AValue: String): SizeInt;
    procedure Fetching(Sender: TObject; const AKey: Integer);
    procedure Fetched(Sender: TObject; const AKey: Integer; const AValue: String);
    procedure Removing(Sender: TObject; const AKey: Integer; AItem: TCachedItem<String>);
    procedure Removed(Sender: TObject; const AKey: Integer; const AValue: String);
  end;

  TBulkReader = class(TThread)
  private FCache: TTestBulk;
  protected procedure Execute; override;
  public
    Error: String;
    constructor Create(ACache: TTestBulk);
  end;

  TObjectBulk = TBulkFetchActionCache<Integer, TObject>;
  TObjectValues = TCacheBulkValues<Integer, TObject>;
  TInterfaceBulk = TBulkFetchActionCache<Integer, IInterface>;
  TInterfaceCache = TCacheBase<Integer, IInterface>;
  TInterfaceValues = TCacheBulkValues<Integer, IInterface>;
  TDestructionCheckValue = class(TInterfacedObject)
  private
    FCache: TInterfaceCache;
    FRejected: PBoolean;
  public
    constructor Create(ACache: TInterfaceCache; ARejected: PBoolean);
    destructor Destroy; override;
  end;
  TTrackedValue = class
    destructor Destroy; override;
  end;

var DestroyedValues: Integer;

procedure Check(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

constructor TContext.Create(AOnce: Boolean; AExpiry: TCacheExpirationPolicy;
  AStale: TCacheStaleValuePolicy; ANull: TCacheNullValuePolicy);
begin
  inherited Create;
  Source := TValues(FResources.AddObject(TValues.Create));
  Source.Add(1,'one'); Source.Add(2,'two');
  Cache := TTestBulk(FResources.AddObject(TTestBulk.Create(nil,AExpiry,
    TTimeSpan.FromSeconds(1),AOnce,ANull,AStale)));
  Cache.Source := Source;
end;

procedure TContext.FillValues(AValues: TValues);
var pair: TPair<Integer,String>;
begin
  for pair in Source do AValues.AddOrSetValue(pair.Key,pair.Value);
end;

function TContext.FetchValue(const AKey: Integer): String;
begin
  Result := IntToStr(AKey);
end;

function TContext.EstimateValue(const AValue: String): SizeInt;
begin
  Result := Length(AValue);
end;

procedure TContext.Fetching(Sender: TObject; const AKey: Integer);
begin
  Inc(FetchingCount);
end;

procedure TContext.Fetched(Sender: TObject; const AKey: Integer; const AValue: String);
begin
  Inc(FetchedCount);
end;

procedure TContext.Removing(Sender: TObject; const AKey: Integer; AItem: TCachedItem<String>);
begin
  Inc(RemovingCount);
end;

procedure TContext.Removed(Sender: TObject; const AKey: Integer; const AValue: String);
begin
  Inc(RemovedCount);
end;

procedure TTestBulk.BulkFetch(AValues: TValues);
var pair: TPair<Integer,String>;
begin
  Inc(Calls);
  if RaiseOnFetch then raise EInvalidOperation.Create('provider failed');
  if ReenterRefresh then ForceRefresh;
  if ReenterRead then Check(not ContainsCachedItem(99),'Reentrant query must see the current snapshot');
  for pair in Source do AValues.AddOrSetValue(pair.Key,pair.Value);
end;

function TTestBulk.CheckStaleness(const AKey: Integer; AItem: TCachedItem<String>): Boolean;
begin
  Inc(StaleChecks); Result := Stale;
end;

constructor TBulkReader.Create(ACache: TTestBulk);
begin
  inherited Create(True);
  FCache := ACache;
end;

procedure TBulkReader.Execute;
var i: Integer;
begin
  try
    for i := 1 to 100 do Check(FCache[1] = 'one','Concurrent snapshot read changed');
  except on E: Exception do Error := E.Message; end;
end;

destructor TTrackedValue.Destroy;
begin
  Inc(DestroyedValues);
  inherited;
end;

constructor TDestructionCheckValue.Create(ACache: TInterfaceCache; ARejected: PBoolean);
begin
  inherited Create;
  FCache := ACache;
  FRejected := ARejected;
end;

destructor TDestructionCheckValue.Destroy;
begin
  // The last managed value reference is released while the cache destructor
  // still owns its lock. This tests rejection without using a freed instance.
  try FCache.Reap(1);
  except on E: EInvalidOperation do FRejected^ := True; end;
  inherited;
end;

procedure GlobalFill(AValues: TValues);
begin
  AValues.Add(7,'global');
end;

function FetchPerKey(const AKey: Integer): String;
begin
  Result := IntToStr(AKey);
end;

function FetchScalar(const AKey: String): Integer;
begin
  Result := Length(AKey);
end;

function EstimateText(const AValue: String): SizeInt;
begin
  Result := Length(AValue);
end;

procedure CheckSiblingCallbacks;
var
  resources: TDisposables;
  ctx: TContext;
  perKey: TPerKeyAction;
  bulk: TBulkAction;
  preloaded: TPreloadedCache<Integer, String>;
  values: TCacheBulkValues<Integer, String>;
  globalFetch: TCacheGlobalFetchFunc<Integer, String>;
  objectFetch: TCacheObjectFetchFunc<Integer, String>;
  nestedFetch: TCacheNestedFetchFunc<Integer, String>;
  globalEstimate: TCacheGlobalEstimateFunc<String>;
  objectEstimate: TCacheObjectEstimateFunc<String>;
  nestedEstimate: TCacheNestedEstimateFunc<String>;
  globalBulk: TCacheGlobalBulkFetchProc<Integer, String>;
  objectBulk: TCacheObjectBulkFetchProc<Integer, String>;
  nestedBulk: TCacheNestedBulkFetchProc<Integer, String>;
  fetcher: ICacheValueFetcher<Integer, String>;
  estimator: ICacheSizeEstimator<String>;
  bulkFetcher: ICacheBulkValueFetcher<Integer, String>;
  fetching: TCacheItemFetchingEvent<Integer>;
  fetched, removed: TCacheItemValueEvent<Integer, String>;
  removing: TCacheItemRemovingEvent<Integer, String>;
  suffix: String;
  {$IFDEF FPC}
  function FetchNested(const AKey: Integer): String;
  begin
    Result := IntToStr(AKey) + suffix;
  end;
  function EstimateNested(const AValue: String): SizeInt;
  begin
    Result := Length(AValue) + Length(suffix);
  end;
  procedure FillNested(AValues: TCacheBulkValues<Integer, String>);
  begin
    AValues.Add(9,suffix);
  end;
  {$ENDIF}
begin
  ctx := TContext(resources.AddObject(TContext.Create(False,TCacheExpirationPolicy.epNone,
    TCacheStaleValuePolicy.svpAssumeNeverStale,TCacheNullValuePolicy.nvpCacheNormally)));
  values := TCacheBulkValues<Integer, String>(resources.AddObject(TCacheBulkValues<Integer, String>.Create));
  globalFetch := FetchPerKey;
  globalEstimate := EstimateText;
  fetcher := TCacheValueFetcher<Integer, String>.FromGlobalFunction(globalFetch);
  estimator := TCacheSizeEstimator<String>.FromGlobalFunction(globalEstimate);
  fetcher := TCacheValueFetcher<Integer, String>.Create(fetcher);
  estimator := TCacheSizeEstimator<String>.Create(estimator);
  Check(fetcher.Fetch(7) = '7','Unit-scoped global fetch adapter and wrapper');
  Check(estimator.Estimate('seven') = 5,'Unit-scoped global estimator and wrapper');
  objectFetch := ctx.FetchValue;
  objectEstimate := ctx.EstimateValue;
  Check(TCacheValueFetcher<Integer, String>.FromObjectFunction(objectFetch).Fetch(8) = '8',
    'Unit-scoped object fetch adapter');
  Check(TCacheSizeEstimator<String>.FromObjectFunction(objectEstimate).Estimate('eight') = 5,
    'Unit-scoped object estimator');
  globalBulk := GlobalFill;
  objectBulk := ctx.FillValues;
  bulkFetcher := TCacheBulkValueFetcher<Integer, String>.FromGlobalFunction(globalBulk);
  bulkFetcher := TCacheBulkValueFetcher<Integer, String>.Create(bulkFetcher);
  bulkFetcher.Fetch(values);
  Check(values[7] = 'global','Unit-scoped global bulk adapter and wrapper');
  values.Clear;
  TCacheBulkValueFetcher<Integer, String>.FromObjectFunction(objectBulk).Fetch(values);
  Check(values[2] = 'two','Unit-scoped object bulk adapter');
  values.Clear;
  suffix := 'nested';
  {$IFDEF FPC}
  nestedFetch := FetchNested;
  nestedEstimate := EstimateNested;
  nestedBulk := FillNested;
  {$ELSE}
  nestedFetch := function(const AKey: Integer): String
    begin
      Result := IntToStr(AKey) + suffix;
    end;
  nestedEstimate := function(const AValue: String): SizeInt
    begin
      Result := Length(AValue) + Length(suffix);
    end;
  nestedBulk := procedure(AValues: TCacheBulkValues<Integer, String>)
    begin
      AValues.Add(9,suffix);
    end;
  {$ENDIF}
  Check(TCacheValueFetcher<Integer, String>.FromNestedFunction(nestedFetch).Fetch(9) = '9nested',
    'Unit-scoped nested fetch adapter');
  Check(TCacheSizeEstimator<String>.FromNestedFunction(nestedEstimate).Estimate('nine') = 10,
    'Unit-scoped nested estimator');
  TCacheBulkValueFetcher<Integer, String>.FromNestedFunction(nestedBulk).Fetch(values);
  Check(values[9] = 'nested','Unit-scoped nested bulk adapter');

  // One event variable now binds to each cache kind without inherited-type aliases or casts.
  fetching := ctx.Fetching;
  fetched := ctx.Fetched;
  removing := ctx.Removing;
  removed := ctx.Removed;
  perKey := TPerKeyAction(resources.AddObject(TPerKeyAction.Create(nil,fetcher,estimator,
    idpNone,TCacheReapPolicy.crpNone,TCacheExpirationPolicy.epNone,
    TCacheNullValuePolicy.nvpCacheNormally,10,TTimeSpan.MaxValue)));
  bulk := TBulkAction(resources.AddObject(TBulkAction.Create(nil,bulkFetcher,
    TCacheExpirationPolicy.epNone,TTimeSpan.MaxValue)));
  preloaded := TPreloadedCache<Integer, String>(resources.AddObject(
    TPreloadedCache<Integer, String>.Create(nil,ctx.Source)));
  perKey.ItemFetching := fetching;
  bulk.ItemFetching := fetching;
  preloaded.ItemFetching := fetching;
  perKey.ItemFetched := fetched;
  bulk.ItemFetched := fetched;
  preloaded.ItemFetched := fetched;
  perKey.ItemRemoving := removing;
  bulk.ItemRemoving := removing;
  preloaded.ItemRemoving := removing;
  perKey.ItemRemoved := removed;
  bulk.ItemRemoved := removed;
  preloaded.ItemRemoved := removed;
  Check(perKey[7] = '7','Ordinary cache uses sibling adapters and shared events');
  Check(bulk[7] = 'global','Bulk cache uses sibling adapters and shared events');
  Check(preloaded[1] = 'one','Preloaded cache uses the shared events');
  perKey.Purge;
  bulk.Purge;
  preloaded.Purge;
  // Bulk snapshots use explicit admission; only the ordinary request invokes per-key fetching.
  Check(ctx.FetchingCount = 1,'Bulk admission does not invent per-key fetching notifications');
  Check(ctx.FetchedCount = 1,'Bulk admission does not invent per-key fetched notifications');
  Check(ctx.RemovingCount = 4,'All cache kinds publish removing through one event type');
  Check(ctx.RemovedCount = 4,'All cache kinds publish removed through one event type');
end;

procedure CheckUnitScopedPolicies;
var
  resources: TDisposables;
  perKey: TPerKeyAction;
  scalar: TScalarAction;
  bulk: TBulkAction;
  preloaded: TPreloadedCache<Integer, String>;
  source: TDictionary<Integer, String>;
  expiration: TCacheExpirationPolicy;
  reap: TCacheReapPolicy;
  nulls: TCacheNullValuePolicy;
  stale: TCacheStaleValuePolicy;
begin
  // Every specialization must accept the same policy variables without a base alias or cast.
  for expiration := Low(TCacheExpirationPolicy) to High(TCacheExpirationPolicy) do
    for reap := Low(TCacheReapPolicy) to High(TCacheReapPolicy) do
      for nulls := Low(TCacheNullValuePolicy) to High(TCacheNullValuePolicy) do
        for stale := Low(TCacheStaleValuePolicy) to High(TCacheStaleValuePolicy) do begin
          perKey := TPerKeyAction.Create(nil,
            TCacheValueFetcher<Integer,String>.FromGlobalFunction(FetchPerKey),nil,
            idpNone,reap,expiration,nulls,4,TTimeSpan.MaxValue,0.1,stale);
          try
            scalar := TScalarAction.Create(nil,
              TCacheValueFetcher<String,Integer>.FromGlobalFunction(FetchScalar),nil,
              idpNone,perKey.ReapPolicy,perKey.ExpirationPolicy,
              perKey.NullValuePolicy,4,TTimeSpan.MaxValue,0.1,perKey.StaleValuePolicy);
            try
              Check(scalar.ReapPolicy = reap,'Reap policy is shared across specializations');
              Check(scalar.ExpirationPolicy = expiration,'Expiration policy is shared across specializations');
              Check(scalar.NullValuePolicy = nulls,'Null policy is shared across specializations');
              Check(scalar.StaleValuePolicy = stale,'Stale policy is shared across specializations');
              Check(perKey[7] = '7','Per-key cache uses shared policy configuration');
              Check(scalar['seven'] = 5,'A different key/value specialization uses the same policies');
            finally
              scalar.Free;
            end;
          finally
            perKey.Free;
          end;
        end;

  for expiration := Low(TCacheExpirationPolicy) to High(TCacheExpirationPolicy) do
    for nulls := Low(TCacheNullValuePolicy) to High(TCacheNullValuePolicy) do
      for stale := Low(TCacheStaleValuePolicy) to High(TCacheStaleValuePolicy) do begin
        bulk := TBulkAction.Create(nil,
          TCacheBulkValueFetcher<Integer,String>.FromGlobalFunction(GlobalFill),
          expiration,TTimeSpan.MaxValue,False,nulls,stale);
        try
          Check(bulk.ExpirationPolicy = expiration,'Bulk constructor accepts the unit expiration type');
          Check(bulk.NullValuePolicy = nulls,'Bulk constructor accepts the unit null type');
          Check(bulk.StaleValuePolicy = stale,'Bulk constructor accepts the unit stale type');
          Check(bulk[7] = 'global','Bulk cache remains usable with every policy configuration');
        finally
          bulk.Free;
        end;
      end;

  // Preloaded sources still accept an ordinary caller-owned RTL dictionary.
  source := TDictionary<Integer, String>(resources.AddObject(TDictionary<Integer, String>.Create));
  source.Add(7,'preloaded');
  preloaded := TPreloadedCache<Integer, String>(resources.AddObject(
    TPreloadedCache<Integer, String>.Create(nil,source)));
  expiration := preloaded.ExpirationPolicy;
  reap := preloaded.ReapPolicy;
  nulls := preloaded.NullValuePolicy;
  stale := preloaded.StaleValuePolicy;
  Check(expiration = TCacheExpirationPolicy.epNone,'Preloaded cache exposes the shared expiration type');
  Check(reap = TCacheReapPolicy.crpNone,'Preloaded cache exposes the shared reap type');
  Check(nulls = TCacheNullValuePolicy.nvpCacheNormally,'Preloaded cache exposes the shared null type');
  Check(stale = TCacheStaleValuePolicy.svpAssumeNeverStale,'Preloaded cache exposes the shared stale type');
  Check(preloaded[7] = 'preloaded','Preloaded cache retains its source behavior');
end;

procedure RunBulkCacheCheck(AIndex: Integer);
var
  ctx: TContext;
  resources: TDisposables;
  action: TBulkAction;
  preloaded: TPreloadedCache<Integer,String>;
  values: TArray<String>;
  failed: Boolean;
  i: Integer;
  readers: array[0..3] of TBulkReader;
  fetcher: ICacheBulkValueFetcher<Integer,String>;
  wrapper: TCacheBulkValueFetcher<Integer,String>;
  nullPolicy: TCacheNullValuePolicy;
  objectCache: TObjectBulk;
  objectValue: TTrackedValue;
  nestedValue: String;
  original: TCachedItem<String>;
  interfaceCache: TInterfaceBulk;
  interfaceFetcher: TCacheNestedBulkFetchProc<Integer,IInterface>;
  destructionRejected: Boolean;
  objectFetcher: TCacheNestedBulkFetchProc<Integer,TObject>;
  nestedFetcher: TCacheNestedBulkFetchProc<Integer,String>;
  {$IFDEF FPC}
  procedure NestedFill(AValues: TValues);
  begin
    AValues.Add(9,nestedValue);
  end;
  procedure ObjectFill(AValues: TObjectValues);
  begin
    AValues.Add(1,nil);
    AValues.Add(2,objectValue);
  end;
  procedure InterfaceFill(AValues: TInterfaceValues);
  begin
    AValues.Add(1,TDestructionCheckValue.Create(interfaceCache,@destructionRejected));
  end;
  {$ENDIF}
begin
  if AIndex = 29 then begin
    CheckSiblingCallbacks;
    Exit;
  end;
  if AIndex = 28 then begin
    CheckUnitScopedPolicies;
    Exit;
  end;
  if AIndex = 10 then begin
    // Bulk cache payloads stay owned by the source, including nil policies.
    DestroyedValues := 0;
    objectValue := TTrackedValue(resources.AddObject(TTrackedValue.Create));
    {$IFDEF FPC}
    objectFetcher := ObjectFill;
    {$ELSE}
    objectFetcher := procedure(AValues: TObjectValues)
      begin
        AValues.Add(1,nil);
        AValues.Add(2,objectValue);
      end;
    {$ENDIF}
    for nullPolicy := Low(TCacheNullValuePolicy) to High(TCacheNullValuePolicy) do begin
      objectCache := TObjectBulk.Create(nil,TCacheBulkValueFetcher<Integer,TObject>.FromNestedFunction(objectFetcher),
        TCacheExpirationPolicy.epNone,TTimeSpan.MaxValue,True,nullPolicy);
      try
        failed := False;
        try Check(objectCache[1] = nil,'Null snapshot entry must stay nil');
        except on E: EInvalidOperation do failed := True; end;
        Check(failed = (nullPolicy = TCacheNullValuePolicy.nvpThrow),'Null read policy');
        Check(objectCache[2] = objectValue,'Non-null borrowed payload');
        Check(objectCache.ContainsCachedItem(1) = (nullPolicy <> TCacheNullValuePolicy.nvpReturnButDontCache),
          'Null membership policy');
        objectCache.ForceRefresh;
      finally objectCache.Free; end;
      Check(DestroyedValues = 0,'Refresh/destruction must not free borrowed payloads');
    end;
    Exit;
  end;

  ctx := TContext(resources.AddObject(TContext.Create((AIndex in [5,6,7,8,9,16,17,24,25]),
    TCacheExpirationPolicy.epSinceFetchedTime,
    TCacheStaleValuePolicy.svpCheckStaleOnDemand,TCacheNullValuePolicy.nvpCacheNormally)));
  case AIndex of
    1: begin
      Check(ctx.Cache[1] = 'one','Initial get');
      Check((ctx.Cache.ItemCount = 2) and (ctx.Cache.Calls = 1),'One get loads the whole source');
      Check(ctx.Cache[2] = 'two','Second prefetched key');
      Check(ctx.Cache.Calls = 1,'Hit must not fetch');
      Check(ctx.Cache.CurrentSize = 0,'Bulk snapshots do not consume per-entry eviction capacity');
    end;
    2: begin
      Check(ctx.Cache[99] = '','Missing key default');
      Check(ctx.Cache.Calls = 1,'Cold missing-key request must fetch exactly once');
      Check(ctx.Cache[100] = '','Subsequent missing key');
      Check(ctx.Cache.Calls = 2,'Each ordinary missing request fetches at most once');
    end;
    3: begin
      Check(ctx.Cache.ContainsCachedItem(1),'Contains initializes the complete snapshot');
      Check(not ctx.Cache.ContainsCachedItem(99),'Contains miss');
      Check(ctx.Cache.Calls = 1,'Contains miss does not refetch');
    end;
    4: begin
      values := ctx.Cache.GetAllCachedValues;
      Check(Length(values) = 2,'All-values initializes snapshot');
      ctx.Source.Add(3,'three');
      Check(Length(values) = 2,'All-values result is a copied array');
      Check(Length(ctx.Cache.GetAllCachedValues) = 2,'All-values does not silently refresh');
      Check(ctx.Cache.Calls = 1,'All-values loads once');
    end;
    5,6,7: begin
      Check(ctx.Cache[1] = 'one','Fetch-once initial value');
      ctx.Source[1] := 'changed';
      case AIndex of
        5: ctx.Cache.Invalidate(1);
        6: ctx.Cache.Get(1).FFetchedOn := Now - 1;
        7: ctx.Cache.Stale := True;
      end;
      Check(ctx.Cache.ContainsCachedItem(1),'Fetch-once keeps original membership');
      Check(ctx.Cache[1] = 'one','Fetch-once retains original snapshot despite invalidation/expiry/staleness');
      Check(ctx.Cache.Calls = 1,'Fetch-once must not refetch automatically');
      ctx.Cache.ForceRefresh;
      Check(ctx.Cache[1] = 'changed','Explicit refresh replaces fetch-once snapshot');
      Check(ctx.Cache.Calls = 2,'Explicit refresh provider call');
    end;
    8: begin
      Check(ctx.Cache[99] = '','Fetch-once initial missing');
      ctx.Source.Add(99,'later');
      Check(ctx.Cache[99] = '','Missing remains part of fetch-once snapshot');
      Check(ctx.Cache.Calls = 1,'Fetch-once missing never refetches');
    end;
    9: begin
      Check(ctx.Cache[1] = 'one','Initial snapshot'); ctx.Cache.Purge;
      Check(ctx.Cache.FetchCount = 0,'Purge resets load count');
      ctx.Source[1] := 'changed';
      Check(ctx.Cache[1] = 'changed','Purge permits new first snapshot');
      Check(ctx.Cache.Calls = 2,'Provider called after purge');
    end;
    11,12,13: begin
      Check(ctx.Cache[1] = 'one','Initial snapshot'); ctx.Source[1] := 'changed';
      case AIndex of
        11: ctx.Cache.Invalidate(1);
        12: ctx.Cache.Get(1).FFetchedOn := Now - 1;
        13: ctx.Cache.Stale := True;
      end;
      Check(ctx.Cache[1] = 'changed','Ordinary invalidated/expired/stale snapshot reloads');
      Check(ctx.Cache.Calls = 2,'One refresh per triggering request');
      Check(ctx.Cache.ItemCount = 2,'Refresh restores all source keys');
    end;
    14: begin
      Check(ctx.Cache[1] = 'one','Initial snapshot'); ctx.Cache.RaiseOnFetch := True;
      failed := False;
      try ctx.Cache.ForceRefresh; except on E: EInvalidOperation do failed := True; end;
      Check(failed,'Provider failure must propagate');
      Check(ctx.Cache.ItemCount = 0,'Failed purge-first refresh leaves no old snapshot');
      Check(ctx.Cache.FetchCount = 0,'Failed refresh does not mark loaded');
      ctx.Cache.RaiseOnFetch := False; ctx.Source[1] := 'recovered';
      Check(ctx.Cache[1] = 'recovered','A failed provider can be retried');
      Check(ctx.Cache.Calls = 3,'Failed batch is attempted once');
    end;
    15: begin
      preloaded := TPreloadedCache<Integer,String>(resources.AddObject(TPreloadedCache<Integer,String>.Create(nil,ctx.Source)));
      Check(preloaded[1] = 'one','Preloaded initial snapshot'); ctx.Source[1] := 'changed';
      Check(preloaded[1] = 'one','Preloaded snapshot retained');
      preloaded.ForceRefresh;
      Check(preloaded[1] = 'changed','Preloaded refresh sees changes to the original borrowed dictionary');
      Check(ctx.Source.Count = 2,'Preloaded does not clear source');
    end;
    16: begin
      ctx := TContext(resources.AddObject(TContext.Create(True,TCacheExpirationPolicy.epNone,
        TCacheStaleValuePolicy.svpAssumeNeverStale,TCacheNullValuePolicy.nvpThrow)));
      failed := False;
      try nestedValue := ctx.Cache[99];
      except on E: EInvalidOperation do failed := True; end;
      Check(failed,'Missing key obeys throw policy');
      Check(ctx.Cache.Calls = 1,'Cold missing key with throw policy fetches once');
      Check(ctx.Cache[1] = 'one','A missing key does not invalidate the complete snapshot');
      Check(ctx.Cache.Calls = 1,'Fetch-once hit after missing key retains loaded snapshot');
    end;
    17: begin
      Check(ctx.Cache[1] = 'one','Initial snapshot'); failed := False;
      try TCacheBase<Integer,String>(ctx.Cache).Remove(1);
      except on E: ENotSupportedException do failed := True; end;
      Check(failed and ctx.Cache.ContainsCachedItem(1),'Bulk removal is rejected through a base reference');
    end;
    18: begin
      action := TBulkAction(resources.AddObject(TBulkAction.Create(nil,
        TCacheBulkValueFetcher<Integer,String>.FromGlobalFunction(GlobalFill),
        TCacheExpirationPolicy.epNone,TTimeSpan.MaxValue)));
      Check(action[7] = 'global','Global adapter');
      action := TBulkAction(resources.AddObject(TBulkAction.Create(nil,
        TCacheBulkValueFetcher<Integer,String>.FromObjectFunction(ctx.FillValues),
        TCacheExpirationPolicy.epNone,TTimeSpan.MaxValue)));
      Check(action[2] = 'two','Object adapter');
      nestedValue := 'nested';
      {$IFDEF FPC}
      nestedFetcher := NestedFill;
      {$ELSE}
      nestedFetcher := procedure(AValues: TValues)
        begin
          AValues.Add(9,nestedValue);
        end;
      {$ENDIF}
      fetcher := TCacheBulkValueFetcher<Integer,String>.FromNestedFunction(nestedFetcher);
      wrapper := TCacheBulkValueFetcher<Integer,String>.Create(fetcher);
      fetcher := wrapper;
      action := TBulkAction(resources.AddObject(TBulkAction.Create(nil,fetcher,
        TCacheExpirationPolicy.epNone,TTimeSpan.MaxValue)));
      Check(action[9] = 'nested','Nested adapter and interface wrapper');
    end;
    19: begin
      failed := False;
      try fetcher := TCacheBulkValueFetcher<Integer,String>.FromGlobalFunction(nil);
      except on E: EArgumentNilException do failed := True; end;
      Check(failed,'Nil global callback rejected'); failed := False;
      try fetcher := TCacheBulkValueFetcher<Integer,String>.FromObjectFunction(nil);
      except on E: EArgumentNilException do failed := True; end;
      Check(failed,'Nil object callback rejected'); failed := False;
      try fetcher := TCacheBulkValueFetcher<Integer,String>.FromNestedFunction(nil);
      except on E: EArgumentNilException do failed := True; end;
      Check(failed,'Nil nested callback rejected'); failed := False;
      try action := TBulkAction.Create(nil,nil,TCacheExpirationPolicy.epNone,TTimeSpan.MaxValue);
      except on E: EArgumentNilException do failed := True; end;
      Check(failed,'Nil fetcher rejected'); failed := False;
      try wrapper := TCacheBulkValueFetcher<Integer,String>.Create(nil);
      except on E: EArgumentNilException do failed := True; end;
      Check(failed,'Nil wrapper fetcher rejected'); failed := False;
      try preloaded := TPreloadedCache<Integer,String>.Create(nil,nil);
      except on E: EArgumentNilException do failed := True; end;
      Check(failed,'Nil preloaded source rejected');
    end;
    20: begin
      for i := 0 to High(readers) do readers[i] := TBulkReader(resources.AddObject(TBulkReader.Create(ctx.Cache)));
      for i := 0 to High(readers) do readers[i].Start;
      for i := 0 to High(readers) do readers[i].WaitFor;
      for i := 0 to High(readers) do Check(readers[i].Error = '',readers[i].Error);
      Check(ctx.Cache.Calls = 1,'Concurrent reads share one initial batch');
    end;
    21: begin
      ctx.Cache.ReenterRefresh := True; failed := False;
      try values := ctx.Cache.GetAllCachedValues;
      except on E: EInvalidOperation do failed := True; end;
      Check(failed,'Recursive ForceRefresh is rejected instead of recursively fetching forever');
      Check(ctx.Cache.Calls = 1,'One attempt for recursive provider');
      ctx.Cache.ReenterRefresh := False; ctx.Cache.ReenterRead := True;
      Check(ctx.Cache[1] = 'one','Read-only callback reentry does not refetch');
      Check(ctx.Cache.Calls = 2,'One subsequent retry');
    end;
    22: begin
      preloaded := TPreloadedCache<Integer,String>(resources.AddObject(TPreloadedCache<Integer,String>.Create(
        nil,ctx.Source,TEqualityComparer<Integer>.Default)));
      Check(preloaded[2] = 'two','Configured key comparer works for both storage and source snapshot');
    end;
    23: begin
      ctx.Cache.Stale := True;
      Check(ctx.Cache[1] = 'one','Always-stale initial source still resolves');
      Check(ctx.Cache.Calls = 1,'Initial stale snapshot cannot cause a second fetch in the same request');
      Check(ctx.Cache[1] = 'one','Subsequent stale source resolves');
      Check(ctx.Cache.Calls = 2,'Next request may refresh once');
    end;
    24,25: begin
      original := ctx.Cache.Get(1);
      ctx.Source[1] := 'changed';
      if AIndex = 24 then ctx.Cache.Invalidate(1)
      else original.FFetchedOn := Now - 1;
      Check(TPlainBase(ctx.Cache).Reap(1) = 0,'Bulk reaping through base reference releases no capacity');
      Check(ctx.Cache.ItemCount = 2,'Reaping must preserve the complete snapshot');
      Check(ctx.Cache.Get(1) = original,'Fetch-once must retain the original entry after reaping');
      Check(ctx.Cache[1] = 'one','Fetch-once retained original value');
      Check(ctx.Cache.Calls = 1,'Reaping must not refetch the source');
    end;
    26: begin
      original := ctx.Cache.Get(1);
      original.FFetchedOn := Now - 1;
      ctx.Cache.Invalidate(2);
      ctx.Source[1] := 'changed';
      Check(TPlainBase(ctx.Cache).Reap(1) = 0,'Refreshable bulk snapshot also has no per-entry reaping');
      Check(ctx.Cache.ItemCount = 2,'Refreshable snapshot remains complete until next read');
      Check(ctx.Cache.Calls = 1,'Reaping does not fetch');
      Check(ctx.Cache[1] = 'changed','Ordinary expired read still refreshes after no-op reaping');
      Check(ctx.Cache.Calls = 2,'Ordinary expired read refreshes once');
    end;
    27: begin
      failed := False;
      try TPlainBase(ctx.Cache).Reap(-1);
      except on E: EArgumentException do failed := True; end;
      Check(failed,'Negative bulk release remains invalid');
      Check(TPlainBase(ctx.Cache).Reap(0) = 0,'Zero release is harmless');
      Check(ctx.Cache.Calls = 0,'Reaping does not initialize the source');
      destructionRejected := False;
      {$IFDEF FPC}
      interfaceFetcher := InterfaceFill;
      {$ELSE}
      interfaceFetcher := procedure(AValues: TInterfaceValues)
        begin
          AValues.Add(1,TDestructionCheckValue.Create(interfaceCache,@destructionRejected));
        end;
      {$ENDIF}
      interfaceCache := TInterfaceBulk.Create(nil,TCacheBulkValueFetcher<Integer,IInterface>.FromNestedFunction(interfaceFetcher),
        TCacheExpirationPolicy.epNone,TTimeSpan.MaxValue,True);
      try
        interfaceCache.Get(1); // The entry owns the last managed interface reference.
      finally interfaceCache.Free; end;
      Check(destructionRejected,'Bulk reaping rejects destruction while the object is still alive');
    end;
  else raise EArgumentOutOfRangeException.Create('AIndex'); end;
end;

end.
