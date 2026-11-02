unit UExplorerAccountSnapshots;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE.

  Bounded snapshots for the explorer's existing renderer. Painting only reads
  the cache; node locks and disk reads belong to the owned worker. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses Classes, SysUtils, ExtCtrls, Generics.Collections, UCommon, UCache,
  UMemory, UNode, UPCDataTypes, UThread;

type
  TExplorerCachedAccount = record
    Account: TAccount;
    Generation: UInt64;
  end;

  TExplorerAccountSnapshots = class;

  TExplorerAccountSnapshotThread = class(TPCThread)
  private
    FSource: TExplorerAccountSnapshots;
  protected
    procedure BCExecute; override;
  public
    Generation: UInt64;
    Accounts: TArray<Cardinal>;
    Rows: TArray<TAccount>;
    AccountCount, BlockCount: Integer;
    Balance: Int64;
    Succeeded: Boolean;
    ErrorMessage: String;
    constructor Create(ASource: TExplorerAccountSnapshots;
      AGeneration: UInt64; const AAccounts: TArray<Cardinal>);
  end;

  TExplorerAccountSnapshots = class(TComponent)
  private type
    TAccountCache = TActionCache<Cardinal, TExplorerCachedAccount>;
  private
    FNode: TNode;
    FCache: TAccountCache;
    FPending: TList<Cardinal>;
    FWorker: TExplorerAccountSnapshotThread;
    FTimer: TTimer;
    FGeneration: UInt64;
    FForceRefresh, FHasSummary, FStopping: Boolean;
    FAccountCount, FBlockCount: Integer;
    FBalance: Int64;
    FErrorMessage: String;
    FOnUpdated: TNotifyEvent;
    FOnCompleted: TNotifyEvent;
    procedure QueueAccount(AAccount: Cardinal);
    function IsUpdating: Boolean;
    procedure OnTimer(Sender: TObject);
    function MissingAccount(const AAccount: Cardinal): TExplorerCachedAccount;
    function AccountSize(const AValue: TExplorerCachedAccount): SizeInt;
  protected
    { Runs exclusively on the owned worker. Overrides must not touch controls. }
    function ReadSnapshot(const AAccounts: TArray<Cardinal>;
      out ARows: TArray<TAccount>; out AAccountCount, ABlockCount: Integer;
      out ABalance: Int64): Boolean; virtual;
  public
    constructor Create(AOwner: TComponent; ANode: TNode); reintroduce;
    destructor Destroy; override;
    procedure Invalidate;
    procedure Stop;
    function TryGetAccount(AAccountNumber: Integer; out AAccount: TAccount;
      out ABlockCount, AAccountCount: Integer): Boolean;
    function TryGetSummary(out AAccountCount: Integer; out ABalance: Int64): Boolean;
    property Updating: Boolean read IsUpdating;
    property ErrorMessage: String read FErrorMessage;
    property OnUpdated: TNotifyEvent read FOnUpdated write FOnUpdated;
    // Every retired read, including stale/error results; no new data is implied.
    property OnCompleted: TNotifyEvent read FOnCompleted write FOnCompleted;
  end;

implementation

uses UAccounts, UBlockChain;

constructor TExplorerAccountSnapshotThread.Create(ASource: TExplorerAccountSnapshots;
  AGeneration: UInt64; const AAccounts: TArray<Cardinal>);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FSource := ASource;
  Generation := AGeneration;
  Accounts := Copy(AAccounts);
end;

procedure TExplorerAccountSnapshotThread.BCExecute;
begin
  try
    Succeeded := FSource.ReadSnapshot(Accounts, Rows, AccountCount, BlockCount, Balance);
  except
    on E: EAbort do Succeeded := False;
    on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
  end;
end;

constructor TExplorerAccountSnapshots.Create(AOwner: TComponent; ANode: TNode);
begin
  inherited Create(AOwner);
  FNode := ANode;
  FGeneration := 1;
  FForceRefresh := True;
  FPending := TList<Cardinal>.Create;
  FCache := TAccountCache.Create(Self,
    TCacheValueFetcher<Cardinal, TExplorerCachedAccount>.FromObjectFunction(MissingAccount),
    TCacheSizeEstimator<TExplorerCachedAccount>.FromObjectFunction(AccountSize), idpNone,
    TCacheReapPolicy.crpOldest,
    TCacheExpirationPolicy.epNone,
    TCacheNullValuePolicy.nvpCacheNormally, 512, TTimeSpan.MaxValue);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := True;
end;

destructor TExplorerAccountSnapshots.Destroy;
begin
  Stop;
  FPending.Free;
  inherited;
end;

function TExplorerAccountSnapshots.AccountSize(const AValue: TExplorerCachedAccount): SizeInt;
begin
  Result := 1;
end;

function TExplorerAccountSnapshots.MissingAccount(const AAccount: Cardinal): TExplorerCachedAccount;
begin
  // Only completed workers publish records. Painting never fetches a cache miss.
  raise EInvalidOperation.Create('Account snapshot is not cached');
end;

procedure TExplorerAccountSnapshots.Stop;
begin
  FStopping := True;
  FOnUpdated := nil;
  if Assigned(FTimer) then FTimer.Enabled := False;
  if Assigned(FWorker) then begin
    FWorker.Terminate;
    FWorker.WaitFor;
    FreeAndNil(FWorker);
  end;
end;

procedure TExplorerAccountSnapshots.Invalidate;
begin
  if FStopping then Exit;
  Inc(FGeneration);
  FForceRefresh := True;
  FErrorMessage := '';
  if Assigned(FWorker) then FWorker.Terminate;
end;

function TExplorerAccountSnapshots.IsUpdating: Boolean;
begin
  Result := not FStopping and (FForceRefresh or Assigned(FWorker) or (FPending.Count > 0));
end;

procedure TExplorerAccountSnapshots.QueueAccount(AAccount: Cardinal);
var I: Integer;
begin
  if FStopping or (FPending.IndexOf(AAccount) >= 0) then Exit;
  if Assigned(FWorker) and (FWorker.Generation = FGeneration) then
    for I := 0 to High(FWorker.Accounts) do
      if FWorker.Accounts[I] = AAccount then Exit;
  if FPending.Count < 128 then FPending.Add(AAccount);
end;

function TExplorerAccountSnapshots.TryGetAccount(AAccountNumber: Integer;
  out AAccount: TAccount; out ABlockCount, AAccountCount: Integer): Boolean;
var LValue: TExplorerCachedAccount;
begin
  ABlockCount := FBlockCount;
  AAccountCount := FAccountCount;
  Result := False;
  if FStopping or (AAccountNumber < 0) then Exit;
  if FCache.ContainsCachedItem(Cardinal(AAccountNumber)) then begin
    LValue := FCache[Cardinal(AAccountNumber)];
    AAccount := LValue.Account;
    Result := True;
    if LValue.Generation = FGeneration then Exit;
  end;
  QueueAccount(Cardinal(AAccountNumber));
end;

function TExplorerAccountSnapshots.TryGetSummary(out AAccountCount: Integer;
  out ABalance: Int64): Boolean;
begin
  AAccountCount := FAccountCount;
  ABalance := FBalance;
  Result := FHasSummary;
end;

function TExplorerAccountSnapshots.ReadSnapshot(const AAccounts: TArray<Cardinal>;
  out ARows: TArray<TAccount>; out AAccountCount, ABlockCount: Integer;
  out ABalance: Int64): Boolean;
var I, LCount: Integer; LMempool: TPCOperationsComp;
begin
  Result := False;
  if not Assigned(FNode) then Exit;
  // Snapshot readers share the mempool; only writers exclude this bounded copy.
  while not FNode.TryLockMempoolRead(0, LMempool) do begin
    if TThread.CheckTerminated then Exit;
    Sleep(10);
  end;
  try
    if TThread.CheckTerminated then Exit;
    FNode.Bank.SafeBox.StartThreadSafe;
    try
      AAccountCount := FNode.Bank.AccountsCount;
      ABlockCount := FNode.Bank.BlocksCount;
      ABalance := FNode.Bank.SafeBox.TotalBalance;
      SetLength(ARows, Length(AAccounts));
      LCount := 0;
      for I := 0 to High(AAccounts) do begin
        if TThread.CheckTerminated then Exit;
        if AAccounts[I] >= Cardinal(AAccountCount) then Continue;
        ARows[LCount] := LMempool.SafeBoxTransaction.Account(AAccounts[I]);
        Inc(LCount);
      end;
      SetLength(ARows, LCount);
      Result := True;
    finally
      FNode.Bank.SafeBox.EndThreadSave;
    end;
  finally
    FNode.UnlockMempoolRead;
  end;
end;

procedure TExplorerAccountSnapshots.OnTimer(Sender: TObject);
var
  LWorker: TExplorerAccountSnapshotThread;
  LValue: TExplorerCachedAccount;
  I: Integer;
begin
  if FStopping then Exit;
  if Assigned(FWorker) and FWorker.Finished then begin
    LWorker := FWorker;
    FWorker := nil;
    try
      LWorker.WaitFor;
      if (LWorker.Generation = FGeneration) and LWorker.Succeeded then begin
        FAccountCount := LWorker.AccountCount;
        FBlockCount := LWorker.BlockCount;
        FBalance := LWorker.Balance;
        FHasSummary := True;
        for I := 0 to High(LWorker.Rows) do begin
          LValue.Account := LWorker.Rows[I];
          LValue.Generation := FGeneration;
          FCache[LValue.Account.account] := LValue;
        end;
        if Assigned(FOnUpdated) then FOnUpdated(Self);
      end else if (LWorker.Generation = FGeneration) and (LWorker.ErrorMessage <> '') then
        FErrorMessage := LWorker.ErrorMessage;
    finally
      LWorker.Free;
    end;
    // Readiness consumes completion even after an error. Keep data publication
    // separate so a failed read cannot create a repaint/request retry loop.
    if Assigned(FOnCompleted) then FOnCompleted(Self);
  end;
  if FStopping or Assigned(FWorker) or not (FForceRefresh or (FPending.Count > 0)) then Exit;
  FWorker := TExplorerAccountSnapshotThread.Create(Self, FGeneration, FPending.ToArray);
  FPending.Clear;
  FForceRefresh := False;
  FWorker.Start;
end;

end.
