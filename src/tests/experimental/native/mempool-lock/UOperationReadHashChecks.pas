unit UOperationReadHashChecks;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>
  Distributed under the MIT software license; see the accompanying LICENSE.

  Shared Delphi/FPC checks for immutable published operation digests and the
  existing hash-tree lock. No node, wallet data or network is required. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

procedure CheckPublishedOperationReadHashes;
procedure CheckCachedTreeReadTakesExistingLock;
procedure CheckParallelOperationReadHashes;

implementation

uses
  Classes, SysUtils, SyncObjs, UThread, UMemory, UBlockChain, UOpTransaction,
  UCrypto, UConst, UBaseTypes, UPCDataTypes;

type
  TOperationCacheAccess = class(TPCOperation)
  public
    function HasReadHashes: Boolean;
  end;

  THashReadThread = class(TPCThread)
  private
    FTree: TOperationsHashTree;
    FStart: TEvent;
    FExpected: TRawBytes;
    FIterations: Integer;
  protected
    procedure BCExecute; override;
  public
    Entered, Completed: TEvent;
    ErrorMessage: String;
    constructor Create(ATree: TOperationsHashTree; AStart: TEvent;
      const AExpected: TRawBytes; AIterations: Integer);
    destructor Destroy; override;
  end;

  THeldTreeWriter = class(TPCThread)
  private
    FTree: TOperationsHashTree;
    FOperation: TPCOperation;
    procedure HoldChange(Sender: TObject);
  protected
    procedure BCExecute; override;
  public
    Entered, ReleaseChange, Completed: TEvent;
    ErrorMessage: String;
    constructor Create(ATree: TOperationsHashTree; AOperation: TPCOperation);
    destructor Destroy; override;
  end;

procedure Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create(AMessage);
end;

function SigningKey: TECPrivateKey;
begin
  TCrypto.InitCrypto;
  Result := TECPrivateKey.Create;
  try
    Result.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  except
    Result.Free;
    raise;
  end;
end;

function Transaction(AKey: TECPrivateKey; ANOperation: Cardinal): TPCOperation;
begin
  Result := TOpTransaction.CreateTransaction(CT_PROTOCOL_6, 0, ANOperation, 1,
    AKey, 100, 1, CT_TOperationPayload_NUL);
end;

function TOperationCacheAccess.HasReadHashes: Boolean;
begin
  Result := (Length(FBufferedSha256) = 32) and
    (Length(FBufferedRipeMD160) = 20);
end;

constructor THashReadThread.Create(ATree: TOperationsHashTree; AStart: TEvent;
  const AExpected: TRawBytes; AIterations: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTree := ATree;
  FStart := AStart;
  FExpected := Copy(AExpected);
  FIterations := AIterations;
  Entered := TEvent.Create(nil, True, False, '');
  Completed := TEvent.Create(nil, True, False, '');
end;

destructor THashReadThread.Destroy;
begin
  Terminate;
  if Assigned(FStart) then FStart.SetEvent;
  if Suspended then Start;
  WaitFor;
  Completed.Free;
  Entered.Free;
  inherited;
end;

procedure THashReadThread.BCExecute;
var
  I: Integer;
  LHash: TRawBytes;
  LOperation: TPCOperation;
  LResume: TOperationResume;
begin
  try
    try
      Require(FStart.WaitFor(5000) = wrSignaled, 'Hash reader start timed out');
      Entered.SetEvent;
      for I := 0 to FIterations - 1 do begin
        if Terminated then Exit;
        LHash := FTree.HashTree;
        Require(Length(LHash) = 32, 'A reader observed an incomplete tree digest');
        if Length(FExpected) > 0 then
          Require(TBaseType.Equals(FExpected, LHash), 'Parallel tree digest changed');
        LOperation := FTree.GetOperation(I mod FTree.OperationsCount);
        Require(TOperationCacheAccess(LOperation).HasReadHashes,
          'Published operation digest caches are not initialized');
        Require(TPCOperation.OperationToOperationResume(0, LOperation, False,
          LOperation.SignerAccount, LResume), 'Operation resume conversion failed');
      end;
    except
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally
    Completed.SetEvent;
  end;
end;

constructor THeldTreeWriter.Create(ATree: TOperationsHashTree;
  AOperation: TPCOperation);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FTree := ATree;
  FOperation := AOperation;
  Entered := TEvent.Create(nil, True, False, '');
  ReleaseChange := TEvent.Create(nil, True, False, '');
  Completed := TEvent.Create(nil, True, False, '');
end;

destructor THeldTreeWriter.Destroy;
begin
  Terminate;
  if Assigned(ReleaseChange) then ReleaseChange.SetEvent;
  if Suspended then Start;
  WaitFor;
  Completed.Free;
  ReleaseChange.Free;
  Entered.Free;
  inherited;
end;

procedure THeldTreeWriter.HoldChange(Sender: TObject);
begin
  // The real public notification runs while InternalAdd retains its list lock.
  Entered.SetEvent;
  Require(ReleaseChange.WaitFor(5000) = wrSignaled, 'Held tree writer timed out');
end;

procedure THeldTreeWriter.BCExecute;
begin
  try
    if Terminated then Exit;
    FTree.OnChanged := HoldChange;
    try
      Require(FTree.AddOperationToHashTree(FOperation), 'Held tree write failed');
    except
      on E: Exception do ErrorMessage := E.ClassName + ': ' + E.Message;
    end;
  finally
    FTree.OnChanged := nil;
    Completed.SetEvent;
  end;
end;

procedure CheckPublishedOperationReadHashes;
var
  LKey: TECPrivateKey;
  LOperation, LPublished: TPCOperation;
  LFirst, LSecond: TOperationsHashTree;
  LScope: TDisposables;
begin
  LKey := LScope.AddObject(SigningKey) as TECPrivateKey;
  LOperation := LScope.AddObject(Transaction(LKey, 1)) as TPCOperation;
  LFirst := LScope.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  LSecond := LScope.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  Require(LFirst.AddOperationToHashTree(LOperation), 'Initial publication failed');
  LPublished := LFirst.GetOperation(0);
  // Inspect without calling digest getters, which would conceal a cold cache.
  Require(TOperationCacheAccess(LPublished).HasReadHashes,
    'An operation was published before both read digest caches were complete');
  Require(LSecond.AddOperationToHashTree(LPublished), 'Alias publication failed');
  Require(LSecond.GetOperation(0) = LPublished, 'Existing storage identity was not reused');
  Require(TOperationCacheAccess(LSecond.GetOperation(0)).HasReadHashes,
    'A shared operation alias lost its initialized read digests');
  Require(TBaseType.Equals(LPublished.Sha256,
    TCrypto.DoSha256(LPublished.GetBufferForOpHash(True))), 'Published SHA256 is incorrect');
  Require(TBaseType.Equals(LPublished.RipeMD160,
    TCrypto.DoRipeMD160AsRaw(LPublished.GetBufferForOpHash(True))), 'Published RIPEMD160 is incorrect');
end;

procedure CheckCachedTreeReadTakesExistingLock;
var
  LKey: TECPrivateKey;
  LTree: TOperationsHashTree;
  LFirst, LNext: TPCOperation;
  LWarmHash: TRawBytes;
  LStart: TEvent;
  LWriter: THeldTreeWriter;
  LReader: THashReadThread;
  LScope: TDisposables;
begin
  LKey := LScope.AddObject(SigningKey) as TECPrivateKey;
  LTree := LScope.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  LFirst := LScope.AddObject(Transaction(LKey, 1)) as TPCOperation;
  LNext := LScope.AddObject(Transaction(LKey, 2)) as TPCOperation;
  Require(LTree.AddOperationToHashTree(LFirst), 'Initial tree write failed');
  LWarmHash := LTree.HashTree;
  Require(Length(LWarmHash) = 32, 'Initial tree cache did not warm');
  LStart := LScope.AddObject(TEvent.Create(nil, True, False, '')) as TEvent;
  LWriter := LScope.AddObject(THeldTreeWriter.Create(LTree, LNext)) as THeldTreeWriter;
  LReader := LScope.AddObject(THashReadThread.Create(LTree, LStart, nil, 1)) as THashReadThread;
  LWriter.Start;
  LReader.Start;
  try
    Require(LWriter.Entered.WaitFor(5000) = wrSignaled, 'Writer did not hold the real tree lock');
    LStart.SetEvent;
    Require(LReader.Entered.WaitFor(5000) = wrSignaled, 'Reader did not enter its query');
    Require(LReader.Completed.WaitFor(250) = wrTimeout,
      'A cached digest read bypassed an active tree writer');
  finally
    LWriter.ReleaseChange.SetEvent;
    LStart.SetEvent;
    LWriter.WaitFor;
    LReader.WaitFor;
  end;
  Require(LWriter.ErrorMessage = '', LWriter.ErrorMessage);
  Require(LReader.ErrorMessage = '', LReader.ErrorMessage);
end;

procedure CheckParallelOperationReadHashes;
var
  LKey: TECPrivateKey;
  LTree, LReference: TOperationsHashTree;
  LOperation: TPCOperation;
  LReaders: array[0..3] of THashReadThread;
  LStart: TEvent;
  LExpected: TRawBytes;
  I: Integer;
  LScope: TDisposables;
begin
  LKey := LScope.AddObject(SigningKey) as TECPrivateKey;
  LTree := LScope.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  LReference := LScope.AddObject(TOperationsHashTree.Create) as TOperationsHashTree;
  for I := 1 to 64 do begin
    LOperation := Transaction(LKey, I);
    try
      Require(LTree.AddOperationToHashTree(LOperation), 'Parallel fixture publication failed');
    finally
      LOperation.Free;
    end;
  end;
  // Warm only the separate reference: the readers race on a cold source tree.
  LReference.CopyFromHashTree(LTree);
  LExpected := LReference.HashTree;
  LStart := LScope.AddObject(TEvent.Create(nil, True, False, '')) as TEvent;
  for I := 0 to High(LReaders) do begin
    LReaders[I] := LScope.AddObject(THashReadThread.Create(LTree, LStart,
      LExpected, 128)) as THashReadThread;
    LReaders[I].Start;
  end;
  LStart.SetEvent;
  try
    for I := 0 to High(LReaders) do begin
      Require(LReaders[I].Completed.WaitFor(5000) = wrSignaled,
        'A parallel digest reader did not finish');
      Require(LReaders[I].ErrorMessage = '', LReaders[I].ErrorMessage);
    end;
  finally
    LStart.SetEvent;
    for I := 0 to High(LReaders) do LReaders[I].WaitFor;
  end;
end;

end.
