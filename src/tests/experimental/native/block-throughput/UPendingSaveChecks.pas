unit UPendingSaveChecks;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Actual Node.AddOperations, bank notification, saver thread and file storage.
  Only the generated bank and observing storage/failure boundary are test data. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

type
  TPendingSaveCheck = (pscFinalDirty, pscRetryAfterFailure, pscSanitizeRemoval,
    pscAtomicStorage, pscFinalPublicationFailure);

function RunPendingSaveChecks(const ALogPath: String): Integer;
function RunPendingSaveCheck(ACheck: TPendingSaveCheck; const ALogPath: String): Integer;

implementation

uses Classes, SysUtils, SyncObjs, UAccounts, UBlockChain, UNode, UFileStorage,
  UCrypto, UOpTransaction, UThread, UConst, ULog, UPCDataTypes, UBaseTypes;

type
  TSaveObservation = class
  private
    FLock: SyncObjs.TCriticalSection;
    FLog: TStringList;
    FPath: String;
    FAttempts,FSuccesses,FLastCount,FFailuresRemaining,FLoggedFailures: Integer;
    FHoldErrorLog: Boolean;
  public
    Succeeded,Failed,ErrorLogged,ReleaseErrorLog: TEvent;
    constructor Create(const APath: String);
    destructor Destroy; override;
    procedure Log(const AText: String);
    function BeginSave: Boolean;
    procedure Saved(ACount: Integer);
    procedure FailNextSave;
    procedure ArmErrorLogGate;
    procedure CoreLog(AType: TLogType; ATime: TDateTime; AThread: TThreadID;
      const ASender,AText: String);
    function LastCount: Integer;
    function Attempts: Integer;
    function LoggedFailures: Integer;
  end;

  TObservedFileStorage = class(TFileStorage)
  protected
    procedure DoSavePendingBufferOperations(ATree: TOperationsHashTree); override;
    function DoLoadBlockChain(AOperations: TPCOperationsComp; ABlock: Cardinal): Boolean; override;
    function GetFirstBlockNumber: Int64; override;
    function GetLastBlockNumber: Int64; override;
  end;

  TPendingSaveSafeBox = class(TPCSafeBox)
    procedure Build(const AKey: TAccountKey);
    procedure ConfirmFirstOperation;
  end;

  TSerializationFailureTransaction = class(TOpTransaction)
  protected
    function SaveOpToStream(AStream: TStream; AExtended: Boolean): Boolean; override;
  end;

  TPendingSaveChecks = class
  private
    FNode: TNode;
    FKey: TECPrivateKey;
    FObservation: TSaveObservation;
    FCoreLog: TLog;
    FDirectory,FPreviousDirectory: String;
    procedure Check(AValue: Boolean; const AText: String);
    procedure AddOperation(ANonce: Cardinal);
    function PendingBytes: TMemoryStream;
    procedure StopNode;
    procedure CheckStoredBytes(AExpected: TStream);
    procedure FirstSave;
    procedure FinalDirty;
    procedure RetryAfterFailure;
    procedure SanitizeRemoval;
    procedure AtomicStorage;
    procedure FinalPublicationFailure;
  public
    constructor Create(const ALogPath: String);
    destructor Destroy; override;
    procedure Run(ACheck: TPendingSaveCheck);
  end;

var GObservation: TSaveObservation;
  GFailSerialization: Boolean;

function TSerializationFailureTransaction.SaveOpToStream(AStream: TStream;
  AExtended: Boolean): Boolean;
begin
  if GFailSerialization then
    raise EWriteError.Create('Injected serialization failure before temporary-file write');
  Result := inherited SaveOpToStream(AStream,AExtended);
end;

constructor TSaveObservation.Create(const APath: String);
begin
  inherited Create;
  FPath := APath;
  FLock := SyncObjs.TCriticalSection.Create;
  FLog := TStringList.Create;
  Succeeded := TEvent.Create(nil,False,False,'');
  Failed := TEvent.Create(nil,False,False,'');
  ErrorLogged := TEvent.Create(nil,False,False,'');
  ReleaseErrorLog := TEvent.Create(nil,True,False,'');
end;

destructor TSaveObservation.Destroy;
begin
  ReleaseErrorLog.Free;
  ErrorLogged.Free;
  Failed.Free;
  Succeeded.Free;
  FLog.Free;
  FLock.Free;
  inherited;
end;

procedure TSaveObservation.Log(const AText: String);
begin
  FLock.Acquire;
  try
    FLog.Add(FormatDateTime('hh:nn:ss.zzz',Now)+' '+AText);
    FLog.SaveToFile(FPath);
  finally FLock.Release; end;
end;

function TSaveObservation.BeginSave: Boolean;
begin
  FLock.Acquire;
  try
    Inc(FAttempts);
    Result := FFailuresRemaining>0;
    if Result then Dec(FFailuresRemaining);
  finally FLock.Release; end;
  if Result then begin
    Log('INJECT one save failure before storage mutation');
    Failed.SetEvent;
  end;
end;

procedure TSaveObservation.Saved(ACount: Integer);
begin
  FLock.Acquire;
  try
    Inc(FSuccesses);
    FLastCount := ACount;
  finally FLock.Release; end;
  Log('STORED actual pending count='+IntToStr(ACount));
  Succeeded.SetEvent;
end;

procedure TSaveObservation.FailNextSave;
begin
  FLock.Acquire;
  try FFailuresRemaining := 1;
  finally FLock.Release; end;
end;

procedure TSaveObservation.ArmErrorLogGate;
begin
  FLock.Acquire;
  try
    ReleaseErrorLog.ResetEvent;
    FHoldErrorLog := True;
  finally FLock.Release; end;
end;

procedure TSaveObservation.CoreLog(AType: TLogType; ATime: TDateTime;
  AThread: TThreadID; const ASender,AText: String);
var LHold: Boolean;
begin
  if (AType<>lterror) or (Pos('Cannot save pending operations:',AText)=0) then Exit;
  FLock.Acquire;
  try
    Inc(FLoggedFailures);
    LHold := FHoldErrorLog;
    FHoldErrorLog := False;
  finally FLock.Release; end;
  Log('CORE reported '+ASender+': '+AText);
  ErrorLogged.SetEvent;
  if LHold then ReleaseErrorLog.WaitFor(5000);
end;

function TSaveObservation.LastCount: Integer;
begin
  FLock.Acquire;
  try Result := FLastCount;
  finally FLock.Release; end;
end;

function TSaveObservation.Attempts: Integer;
begin
  FLock.Acquire;
  try Result := FAttempts;
  finally FLock.Release; end;
end;

function TSaveObservation.LoggedFailures: Integer;
begin
  FLock.Acquire;
  try Result := FLoggedFailures;
  finally FLock.Release; end;
end;

procedure TObservedFileStorage.DoSavePendingBufferOperations(ATree: TOperationsHashTree);
begin
  if GObservation.BeginSave then
    raise EWriteError.Create('pending-save fixture failure');
  inherited DoSavePendingBufferOperations(ATree);
  GObservation.Saved(ATree.OperationsCount);
end;

function TObservedFileStorage.DoLoadBlockChain(AOperations: TPCOperationsComp;
  ABlock: Cardinal): Boolean;
begin
  // The generated confirmed bank has no mined chain file. Supply its current
  // header for public UpdateValuesFromSafebox, which invokes the real bank
  // subscribers and sanitizer. This fixture does not assert PoW acceptance.
  if ABlock=Bank.BlocksCount-1 then begin
    AOperations.Clear(True);
    AOperations.SetOperationBlock(Bank.SafeBox.GetBlockInfo(ABlock));
    Result := True;
  end else Result := inherited DoLoadBlockChain(AOperations,ABlock);
end;

function TObservedFileStorage.GetFirstBlockNumber: Int64;
begin
  Result := 0;
end;

function TObservedFileStorage.GetLastBlockNumber: Int64;
begin
  Result := Int64(Bank.BlocksCount)-1;
end;

procedure TPendingSaveSafeBox.Build(const AKey: TAccountKey);
var I: Integer; LBlock: TOperationBlock; LAccount: TAccount;
begin
  for I := 0 to 159 do begin
    LBlock := CT_OperationBlock_NUL;
    LBlock.block := I;
    LBlock.account_key := AKey;
    LBlock.protocol_version := CT_PROTOCOL_1;
    LBlock.protocol_available := CT_PROTOCOL_6;
    LBlock.compact_target := CT_MinCompactTarget_v1;
    LBlock.timestamp := I*CT_NewLineSecondsAvg;
    AddNew(LBlock);
  end;
  DoUpgradeToProtocol5;
  LAccount := Account(0);
  UpdateAccount(0,LAccount.accountInfo,LAccount.name,LAccount.account_type,
    1000000,0,LAccount.account_data,LAccount.account_seal,
    aus_transaction_commit,0,0,False,False);
end;

procedure TPendingSaveSafeBox.ConfirmFirstOperation;
var LAccount: TAccount;
begin
  LAccount := Account(0);
  UpdateAccount(0,LAccount.accountInfo,LAccount.name,LAccount.account_type,
    LAccount.balance-2,1,LAccount.account_data,LAccount.account_seal,
    aus_transaction_commit,0,0,False,False);
end;

constructor TPendingSaveChecks.Create(const ALogPath: String);
var LID: TGUID; LTemporaryRoot: String;
begin
  inherited Create;
  FObservation := TSaveObservation.Create(ALogPath);
  GObservation := FObservation;
  FCoreLog := TLog.Create(nil);
  FCoreLog.OnInThreadNewLog := FObservation.CoreLog;
  Check(not TNode.NodeExists,'Each saver fixture requires a fresh node');
  CreateGUID(LID);
  LTemporaryRoot := GetEnvironmentVariable('TEMP');
  Check(LTemporaryRoot<>'','TEMP must identify an owned test directory');
  FDirectory := IncludeTrailingPathDelimiter(LTemporaryRoot)+'PendingSave-'+GUIDToString(LID);
  ForceDirectories(FDirectory);
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  TNode.SetPascalCoinDataFolder(FDirectory);
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FNode := TNode.Create(nil);
  FNode.BroadcastData := False;
  Check(not FNode.NetServer.Active,'The saver fixture must never listen');
  FNode.Bank.StorageClass := TObservedFileStorage;
  Check(FNode.Bank.Storage.Initialize,'Real file storage initialization');
  TPendingSaveSafeBox(FNode.Bank.SafeBox).Build(FKey.PublicKey);
  FNode.Bank.UpdateValuesFromSafebox;
  FObservation.Log('READY generated bank and actual saver; data='+FDirectory);
end;

destructor TPendingSaveChecks.Destroy;
begin
  FNode.Free;
  FCoreLog.Free;
  GObservation := nil;
  FKey.Free;
  TNode.SetPascalCoinDataFolder(FPreviousDirectory);
  FObservation.Free;
  inherited;
end;

procedure TPendingSaveChecks.Check(AValue: Boolean; const AText: String);
begin
  if not AValue then raise Exception.Create(AText);
end;

procedure TPendingSaveChecks.AddOperation(ANonce: Cardinal);
var LOperation: TPCOperation; LError: String;
begin
  LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_5,0,ANonce,1,
    FKey,1,1,CT_TOperationPayload_NUL);
  try
    Check(FNode.AddOperation(nil,LOperation,LError),'Real AddOperation failed: '+LError);
  finally LOperation.Free; end;
end;

function TPendingSaveChecks.PendingBytes: TMemoryStream;
var LPool: TPCOperationsComp;
begin
  Result := TMemoryStream.Create;
  try
    LPool := FNode.LockMempoolRead;
    try Check(LPool.OperationsHashTree.SaveOperationsHashTreeToStream(Result,True),'Expected pending serialization');
    finally FNode.UnlockMempoolRead; end;
  except Result.Free; raise; end;
end;

procedure TPendingSaveChecks.StopNode;
var LStarted: TTickCount;
begin
  LStarted := TPlatform.GetTickCount;
  FreeAndNil(FNode);
  Check(TPlatform.GetElapsedMilliseconds(LStarted)<3000,'Dirty saver shutdown must be bounded');
end;

procedure TPendingSaveChecks.CheckStoredBytes(AExpected: TStream);
var LActual: TMemoryStream; LExpected: TMemoryStream;
begin
  LActual := TMemoryStream.Create;
  LExpected := TMemoryStream.Create;
  try
    LActual.LoadFromFile(FDirectory+PathDelim+'Data'+PathDelim+'pendingbuffer.ops');
    AExpected.Position := 0;
    LExpected.CopyFrom(AExpected,AExpected.Size);
    Check(LExpected.Size=LActual.Size,'Latest saved pending package length');
    Check(CompareMem(LExpected.Memory,LActual.Memory,LExpected.Size),
      'Latest saved pending package must contain exact current operations');
  finally LExpected.Free; LActual.Free; end;
end;

procedure TPendingSaveChecks.FirstSave;
begin
  AddOperation(1);
  Check(FObservation.Succeeded.WaitFor(20000)=wrSignaled,'First actual saver completion');
  Check(FObservation.LastCount=1,'First durable package contains one actual accepted operation');
end;

procedure TPendingSaveChecks.FinalDirty;
var LExpected: TMemoryStream;
begin
  FirstSave;
  AddOperation(2);
  LExpected := PendingBytes;
  try
    StopNode;
    CheckStoredBytes(LExpected);
    Check(FObservation.LastCount=2,'Final quiet-wait shutdown flushed the latest accepted operation');
  finally LExpected.Free; end;
  FObservation.Log('PASS final dirty save while quiet-waiting, actual accepted operations and bounded natural shutdown');
end;

procedure TPendingSaveChecks.RetryAfterFailure;
var LExpected: TMemoryStream; LWorker: TPCThread;
begin
  FirstSave;
  FObservation.FailNextSave;
  FObservation.ArmErrorLogGate;
  AddOperation(2);
  LExpected := PendingBytes;
  try
    Check(FObservation.Failed.WaitFor(20000)=wrSignaled,'Injected actual saver failure reached');
    Check(FObservation.ErrorLogged.WaitFor(3000)=wrSignaled,'Core reports the failed save');
    try
      // Request the actual worker's normal cooperative termination while its
      // running-attempt error callback is held. This deterministically reaches
      // the attempt-start/shutdown race without writing private dirty flags.
      LWorker := TPCThread.GetThreadByClass(TSaveMempoolOperationsThread,nil);
      Check(Assigned(LWorker),'The actual saver still exists after reported failure');
      LWorker.Terminate;
    finally FObservation.ReleaseErrorLog.SetEvent; end;
    StopNode;
    CheckStoredBytes(LExpected);
    Check(FObservation.Attempts>=3,'Failed save was retried by the actual saver');
    Check(FObservation.LastCount=2,'Retry stored the latest complete package');
  finally FObservation.ReleaseErrorLog.SetEvent; LExpected.Free; end;
  FObservation.Log('PASS one failed save is reported and retried, with latest bytes preserved on shutdown');
end;

procedure TPendingSaveChecks.SanitizeRemoval;
var LPool: TPCOperationsComp; LExpected: TMemoryStream;
begin
  FirstSave;
  LPool := FNode.LockMempoolWrite;
  try
    TPendingSaveSafeBox(FNode.Bank.SafeBox).ConfirmFirstOperation;
    FNode.Bank.UpdateValuesFromSafebox;
    Check(LPool.Count=0,'Actual bank notification sanitized the now-confirmed pending operation');
  finally FNode.UnlockMempoolWrite; end;
  LExpected := PendingBytes;
  try
    Check(FObservation.Succeeded.WaitFor(20000)=wrSignaled,
      'Sanitization removal scheduled an actual save without new AddOperations');
    Check(FObservation.LastCount=0,'Removal save stores the current empty pool');
    StopNode;
    CheckStoredBytes(LExpected);
  finally LExpected.Free; end;
  FObservation.Log('PASS actual sanitizer removal marks durable pending state dirty through bank notification');
end;

procedure TPendingSaveChecks.AtomicStorage;
var LOldTree,LNewTree,LRead: TOperationsHashTree;
  LOperation: TPCOperation; LExpected: TMemoryStream;
  LFileName: String; LReader: TFileStream; LRaised: Boolean;
  LSearch: TSearchRec;

  procedure RequireNoTemporaryFiles;
  var LFound: Boolean;
  begin
    LFound := FindFirst(LFileName+'.*.tmp',faAnyFile,LSearch)=0;
    if LFound then FindClose(LSearch);
    Check(not LFound,'Failed publication cleans only its owned temporary package');
  end;

  procedure CheckLoadedPackage(ATree: TOperationsHashTree);
  begin
    LRead.ClearHastThree;
    FNode.Bank.Storage.LoadPendingBufferOperations(LRead);
    Check(LRead.OperationsCount=ATree.OperationsCount,'Cached file getter reopens the expected complete package');
    Check(TBaseType.Equals(LRead.HashTree,ATree.HashTree),'Reopened package ordered hash');
  end;

begin
  LOldTree := TOperationsHashTree.Create;
  LNewTree := TOperationsHashTree.Create;
  LRead := TOperationsHashTree.Create;
  LExpected := TMemoryStream.Create;
  try
    LFileName := FDirectory+PathDelim+'Data'+PathDelim+'pendingbuffer.ops';
    LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_5,0,1,1,FKey,1,1,CT_TOperationPayload_NUL);
    try Check(LOldTree.AddOperationToHashTree(LOperation),'Original atomic package operation');
    finally LOperation.Free; end;
    LNewTree.CopyFromHashTree(LOldTree);
    LOperation := TSerializationFailureTransaction.CreateTransaction(CT_PROTOCOL_5,0,2,1,FKey,1,1,CT_TOperationPayload_NUL);
    try Check(LNewTree.AddOperationToHashTree(LOperation),'Replacement atomic package operation');
    finally LOperation.Free; end;
    FNode.Bank.Storage.SavePendingBufferOperations(LOldTree);
    CheckLoadedPackage(LOldTree); // Establish a cached handle before failure.
    GFailSerialization := True;
    LRaised := False;
    try
      try FNode.Bank.Storage.SavePendingBufferOperations(LNewTree);
      except on E: EWriteError do LRaised := True; end;
    finally GFailSerialization := False; end;
    Check(LRaised,'Serialization failure propagates before temporary write');
    CheckLoadedPackage(LOldTree);
    RequireNoTemporaryFiles;
    // Close the cached handle by successfully republishing the unchanged tree.
    FNode.Bank.Storage.SavePendingBufferOperations(LOldTree);
    {$IFDEF MSWINDOWS}
    LReader := TFileStream.Create(LFileName,fmOpenRead+fmShareDenyNone);
    try
      LRaised := False;
      try FNode.Bank.Storage.SavePendingBufferOperations(LNewTree);
      except
        on E: EOSError do begin
          Check(E.ErrorCode in [5,32],'Publication failed at the intended Windows sharing boundary');
          LRaised := True;
        end;
      end;
      Check(LRaised,'An open handle denying deletion rejects atomic publication');
    finally LReader.Free; end;
    RequireNoTemporaryFiles;
    CheckLoadedPackage(LOldTree);
    {$ENDIF}
    FNode.Bank.Storage.SavePendingBufferOperations(LNewTree);
    CheckLoadedPackage(LNewTree);
    Check(LNewTree.SaveOperationsHashTreeToStream(LExpected,True),'Expected replacement serialization');
    StopNode;
    CheckStoredBytes(LExpected);
    RequireNoTemporaryFiles;
    FObservation.Log('PASS serialization failure and post-write replacement failure preserve complete bytes, cleanup and cached reopen');
  finally
    GFailSerialization := False;
    LExpected.Free; LRead.Free; LNewTree.Free; LOldTree.Free;
  end;
end;

procedure TPendingSaveChecks.FinalPublicationFailure;
{$IFDEF MSWINDOWS}
var LPrevious: TMemoryStream; LReader: TFileStream;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  FirstSave;
  LPrevious := PendingBytes;
  try
    // This is a genuine post-write filesystem failure, not a replaced saver:
    // the reader allows reads/writes but denies atomic deletion/replacement.
    LReader := TFileStream.Create(FDirectory+PathDelim+'Data'+PathDelim+'pendingbuffer.ops',
      fmOpenRead+fmShareDenyNone);
    try
      AddOperation(2);
      StopNode;
      Check(FObservation.ErrorLogged.WaitFor(3000)=wrSignaled,'A failed final publication is reported');
      Check(FObservation.Attempts=2,'Final publication failure exits after one bounded attempt');
      CheckStoredBytes(LPrevious);
    finally LReader.Free; end;
  finally LPrevious.Free; end;
  FObservation.Log('PASS actual failed final atomic publication reports error, retains last complete bytes and exits naturally');
  {$ELSE}
  FObservation.Log('Windows-only sharing-denial final-publication fixture is not exercised on this platform');
  {$ENDIF}
end;

procedure TPendingSaveChecks.Run(ACheck: TPendingSaveCheck);
begin
  case ACheck of
    pscFinalDirty: FinalDirty;
    pscRetryAfterFailure: RetryAfterFailure;
    pscSanitizeRemoval: SanitizeRemoval;
    pscAtomicStorage: AtomicStorage;
    pscFinalPublicationFailure: FinalPublicationFailure;
  end;
end;

function RunPendingSaveCheck(ACheck: TPendingSaveCheck; const ALogPath: String): Integer;
var LChecks: TPendingSaveChecks;
begin
  Result := 1;
  LChecks := TPendingSaveChecks.Create(ALogPath);
  try
    try LChecks.Run(ACheck); Result := 0;
    except on E: Exception do begin
      LChecks.FObservation.Log('FAIL '+E.ClassName+': '+E.Message);
      raise;
    end; end;
  finally LChecks.Free; end;
end;

function RunPendingSaveChecks(const ALogPath: String): Integer;
var LCheck: TPendingSaveCheck; LLog: TStringList; LPath: String;
begin
  Result := 1;
  LLog := TStringList.Create;
  try
    for LCheck := Low(TPendingSaveCheck) to High(TPendingSaveCheck) do begin
      LPath := ALogPath+'.'+IntToStr(Ord(LCheck))+'.log';
      if RunPendingSaveCheck(LCheck,LPath)<>0 then Exit;
      LLog.Add('PASS pending-save scenario '+IntToStr(Ord(LCheck))+'; '+LPath);
      LLog.SaveToFile(ALogPath);
    end;
    Result := 0;
  finally LLog.Free; end;
end;

end.
