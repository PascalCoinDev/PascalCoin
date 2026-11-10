unit UAbstractMemStorageFailureChecks;

{ Copyright (c) 2026 Herman Schoenfeld <https://sphere10.com>. MIT license.
  Checks the optional AbstractMem operation index with real storage workers and
  owned temporary files. No existing wallet, peers or private keys are opened. }

{$IFDEF FPC}{$mode delphi}{$ENDIF}
{$I ../../../../config.inc}

interface

type
  TAbstractMemStorageFailureCheck = (amscFalseCompletionFlush,
    amscBackgroundWriteFailure, amscFailedQueueDrain,
    amscFailedCleanupPreservesMarker, amscSuccessfulDrain, amscInFlightShutdown);

function RunAbstractMemStorageFailureCheck(ACheck: TAbstractMemStorageFailureCheck; const ALogPath: String): Integer;
function RunAbstractMemStorageFailureChecks(const ALogPath: String): Integer;

implementation

uses Classes, SysUtils, SyncObjs, TypInfo, UNode, UBlockChain, UAccounts,
  UAbstractMemBlockchainStorage, UAbstractMem, UFileMem, UConst, UCrypto,
  UPCDataTypes, UOpTransaction;

type
  TWriteControl = class
  public
    MainThread, ProducerThread: TThreadID;
    Entered, ReleaseWrite: TEvent;
    PauseWorker, FailWorker, ShortFlush: Boolean;
    FailureHits, SkippedCloseFlush: Integer;
    constructor Create;
    destructor Destroy; override;
    function IsWorker: Boolean;
  end;

  // FPC resolves protected nested types through a descendant access alias,
  // rather than through the declaring ancestor's qualified name.
  TStorageFileAccess = class(TAbstractMemBlockchainStorage)
  public type
    TGuardedFileMem = {$IFDEF FPC}TStorageFileMem{$ELSE}TAbstractMemBlockchainStorage.TStorageFileMem{$ENDIF};
  end;

  TFailureStorage = class(TAbstractMemBlockchainStorageSecondary)
  protected type
    TFailureFileMem = class(TStorageFileAccess.TGuardedFileMem)
    protected
      function AbsoluteWrite(const AAbsolutePosition: Int64; const ABuffer; ASize: Integer): Integer; override;
      function ShouldFlushOnDestroy: Boolean; override;
    public
      function Write(const APosition: Int64; const ABuffer; ASize: Integer): Integer; override;
    end;
  protected
    function CreateFileMem(const AFileName: String; AReadOnly: Boolean): TFileMem; override;
  public
    Control: TWriteControl;
  end;

  TStorageAction = (saSave, saFlush, saStop, saCloseNode);
  TStorageActionThread = class(TThread)
  public
    Storage: TFailureStorage;
    Node: TNode;
    Operations: TPCOperationsComp;
    Action: TStorageAction;
    Entered, Done: TEvent;
    ErrorMessage: String;
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TStorageFixture = class
  private
    FNode: TNode;
    FStorage: TFailureStorage;
    FControl: TWriteControl;
    FKey: TECPrivateKey;
    FOperations: TPCOperationsComp;
    FPreviousDirectory, FDirectory: String;
    procedure Require(ACondition: Boolean; const AMessage: String);
    procedure PrepareOperations;
    procedure WaitForFailure;
    procedure CheckFalseFlush(AClose: Boolean);
    procedure CheckWorker(ACheck: TAbstractMemStorageFailureCheck);
    procedure CheckSuccessfulDrain;
  public
    constructor Create(const ALogPath: String);
    destructor Destroy; override;
    procedure Run(ACheck: TAbstractMemStorageFailureCheck);
  end;

constructor TWriteControl.Create;
begin
  MainThread := TThread.CurrentThread.ThreadID;
  Entered := TEvent.Create(nil,True,False,'');
  ReleaseWrite := TEvent.Create(nil,True,False,'');
end;

destructor TWriteControl.Destroy;
begin
  ReleaseWrite.Free;
  Entered.Free;
  inherited;
end;

function TWriteControl.IsWorker: Boolean;
begin
  Result := (TThread.CurrentThread.ThreadID<>MainThread) and
    (TThread.CurrentThread.ThreadID<>ProducerThread);
end;

function TFailureStorage.CreateFileMem(const AFileName: String; AReadOnly: Boolean): TFileMem;
begin
  Result := TFailureFileMem.Create(Self,AFileName,AReadOnly);
end;

function TFailureStorage.TFailureFileMem.Write(const APosition: Int64; const ABuffer; ASize: Integer): Integer;
var LControl: TWriteControl;
begin
  LControl := TFailureStorage(FStorage).Control;
  if Assigned(LControl) and LControl.PauseWorker and LControl.IsWorker and
    (APosition>64) then begin
    LControl.Entered.SetEvent;
    if LControl.ReleaseWrite.WaitFor(10000)<>wrSignaled then
      raise EWriteError.Create('Owned worker write gate timed out');
  end;
  Result := inherited Write(APosition,ABuffer,ASize);
end;

function TFailureStorage.TFailureFileMem.AbsoluteWrite(const AAbsolutePosition: Int64; const ABuffer; ASize: Integer): Integer;
var LControl: TWriteControl;
begin
  LControl := TFailureStorage(FStorage).Control;
  if Assigned(LControl) and LControl.ShortFlush and (ASize>64) then
    Exit(0);
  if Assigned(LControl) and LControl.FailWorker and LControl.IsWorker and
    (AAbsolutePosition>64) then begin
    Inc(LControl.FailureHits);
    // A real partial payload reaches the file after its unstable marker was
    // flushed. Cleanup must not later certify the incomplete logical index.
    if ASize>1 then inherited AbsoluteWrite(AAbsolutePosition,ABuffer,ASize div 2);
    raise EWriteError.Create('Injected AbstractMem background index write failure');
  end;
  Result := inherited AbsoluteWrite(AAbsolutePosition,ABuffer,ASize);
end;

function TFailureStorage.TFailureFileMem.ShouldFlushOnDestroy: Boolean;
begin
  Result := inherited ShouldFlushOnDestroy;
  if not Result and Assigned(TFailureStorage(FStorage).Control) then
    Inc(TFailureStorage(FStorage).Control.SkippedCloseFlush);
end;

constructor TStorageActionThread.Create;
begin
  Entered := TEvent.Create(nil,True,False,'');
  Done := TEvent.Create(nil,True,False,'');
  inherited Create(True);
  FreeOnTerminate := False;
end;

destructor TStorageActionThread.Destroy;
begin
  WaitFor;
  Done.Free;
  Entered.Free;
  inherited;
end;

procedure TStorageActionThread.Execute;
begin
  try
    try
      if Action=saSave then Storage.Control.ProducerThread := ThreadID;
      Entered.SetEvent;
      case Action of
        saSave: Storage.SaveBlockChainBlock(Operations);
        saFlush: Storage.FlushCache;
        saStop: Storage.UseMultithread := False;
        saCloseNode: Node.Free;
      end;
    except
      on E: Exception do ErrorMessage := E.ClassName+': '+E.Message;
    end;
  finally
    Done.SetEvent;
  end;
end;

procedure TStorageFixture.Require(ACondition: Boolean; const AMessage: String);
begin
  if not ACondition then raise Exception.Create('AbstractMem storage: '+AMessage);
end;

constructor TStorageFixture.Create(const ALogPath: String);
var LID: TGUID;
begin
  inherited Create;
  Require(not TNode.NodeExists,'fixture requires its own node');
  FPreviousDirectory := TNode.GetPascalCoinDataFolder;
  CreateGUID(LID);
  FDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(ExpandFileName(ALogPath)))+
    'owned-am-storage-'+GUIDToString(LID);
  Require(not DirectoryExists(FDirectory),'fixture directory must be new');
  Require(ForceDirectories(FDirectory),'cannot create fixture directory');
  TNode.SetPascalCoinDataFolder(FDirectory);
  FControl := TWriteControl.Create;
  TCrypto.InitCrypto;
  FKey := TECPrivateKey.Create;
  FKey.GenerateRandomPrivateKey(CT_Default_EC_OpenSSL_NID);
  FNode := TNode.Create(nil);
  FNode.Bank.StorageClass := TFailureStorage;
  FStorage := TFailureStorage(FNode.Bank.Storage);
  FStorage.Control := FControl;
  Require(FStorage.Initialize,'optional backend initialization failed');
  FStorage.AutoFlushCache := False;
  FOperations := TPCOperationsComp.Create(nil);
  PrepareOperations;
end;

destructor TStorageFixture.Destroy;
begin
  if Assigned(FControl) then FControl.ReleaseWrite.SetEvent;
  FOperations.Free;
  FNode.Free;
  FKey.Free;
  FControl.Free;
  TNode.SetPascalCoinDataFolder(FPreviousDirectory);
  inherited;
end;

procedure TStorageFixture.PrepareOperations;
var I: Integer; LOperation: TOpTransaction; LBlock: TOperationBlock;
begin
  for I := 1 to 64 do begin
    LOperation := TOpTransaction.CreateTransaction(CT_PROTOCOL_5,0,I,1,FKey,
      1,1,CT_TOperationPayload_NUL);
    try
      Require(FOperations.OperationsHashTree.AddOperationToHashTree(LOperation),
        'cannot prepare distinct stored operations');
    finally
      LOperation.Free;
    end;
  end;
  LBlock := CT_OperationBlock_NUL;
  LBlock.block := 0;
  LBlock.account_key := FKey.PublicKey;
  LBlock.protocol_version := CT_PROTOCOL_5;
  LBlock.protocol_available := CT_PROTOCOL_5;
  LBlock.compact_target := CT_MinCompactTarget_v1;
  LBlock.fee := 64;
  LBlock.operations_hash := FOperations.OperationsHashTree.HashTree;
  FOperations.SetOperationBlock(LBlock);
end;

procedure TStorageFixture.WaitForFailure;
var I: Integer;
begin
  for I := 1 to 500 do begin
    if FNode.HasStorageFailure then Exit;
    Sleep(10);
  end;
  Require(False,'background failure did not reach the canonical node');
end;

function ReadBytes(const AFileName: String): TBytes;
var LFile: TFileStream;
begin
  LFile := TFileStream.Create(AFileName,fmOpenRead+fmShareDenyNone);
  try
    SetLength(Result,LFile.Size);
    if Length(Result)>0 then LFile.ReadBuffer(Result[0],Length(Result));
  finally
    LFile.Free;
  end;
end;

function SameBytes(const ALeft, ARight: TBytes): Boolean;
begin
  Result := Length(ALeft)=Length(ARight);
  if Result and (Length(ALeft)>0) then
    Result := CompareMem(@ALeft[0],@ARight[0],Length(ALeft));
end;

procedure TStorageFixture.CheckFalseFlush(AClose: Boolean);
var LZone: TAMZone; LBuffer: array[0..127] of Byte; LRaised: Boolean;
  LFileName: String; LBefore, LAfter: TBytes;
begin
  FStorage.UseMultithread := False;
  FStorage.FlushCache;
  if not AClose then FStorage.FileMem.UseCache := False;
  LZone := FStorage.FileMem.New(128);
  FillChar(LBuffer,SizeOf(LBuffer),42);
  if AClose then FStorage.FileMem.Write(LZone.position,LBuffer,SizeOf(LBuffer));
  FControl.ShortFlush := True;
  if not AClose then begin
    // An unrepaired uncached range makes the real FileMem flush return False.
    // The owner has already captured its first write error and must also reject
    // a subsequent completion, rather than acknowledge that False as success.
    LRaised := False;
    try
      FStorage.FileMem.Write(LZone.position,LBuffer,SizeOf(LBuffer));
    except
      on E: EAbstractMem do LRaised := True;
    end;
    Require(LRaised,'uncached short-write fixture did not fail');
    Require(not FStorage.FileMem.FlushCache,'failed uncached range did not produce a false flush');
  end;
  FStorage.AutoFlushCache := True;
  LRaised := False;
  try
    FStorage.FinalizedUpdating;
  except
    on E: Exception do begin
      if not ((E is EWriteError) or (E is EAbstractMem)) then raise;
      LRaised := True;
    end;
  end;
  Require(LRaised,'false completion flush was ignored');
  Require(FNode.HasStorageFailure,'false flush did not latch node failure');
  Require(FNode.GetSynchronizationStatus.StorageFailure.FileName=FStorage.FileMem.FileName,
    'failure must name the actual .am_blocks file');
  if not AClose then Exit;
  LFileName := FStorage.FileMem.FileName;
  LBefore := ReadBytes(LFileName);
  Require((Length(LBefore)>6) and (LBefore[6]=0),'failed flush must leave an unstable marker');
  FControl.ShortFlush := False;
  FreeAndNil(FNode);
  FStorage := nil;
  LAfter := ReadBytes(LFileName);
  Require(SameBytes(LBefore,LAfter),'close retried or certified the incomplete cached index');
  Require(FControl.SkippedCloseFlush=1,'backend did not suppress unsafe destructor retry');
end;

procedure TStorageFixture.CheckWorker(ACheck: TAbstractMemStorageFailureCheck);
var LSave, LOther: TStorageActionThread; LBackupName, LIndexName: String;
  LBackupBefore, LIndexBefore: TBytes; LRaised: Boolean;
begin
  FStorage.FileMem.UseCache := False;
  FControl.PauseWorker := True;
  FControl.FailWorker := True;
  LSave := TStorageActionThread.Create;
  LOther := nil;
  try
    LSave.Storage := FStorage;
    LSave.Operations := FOperations;
    LSave.Action := saSave;
    LSave.Start;
    Require(FControl.Entered.WaitFor(5000)=wrSignaled,'real indexing worker did not reach the write gate');
    Require(LSave.Done.WaitFor(5000)=wrSignaled,'producer did not finish while indexing was gated');
    Require(LSave.ErrorMessage='','raw block producer failed before indexing: '+LSave.ErrorMessage);
    Require(FStorage.PendingToSave>0,'fixture did not retain queued index jobs');
    LBackupName := TPCBank.GetStorageFolder('')+PathDelim+'BlockChainStream.blocks';
    LIndexName := FStorage.FileMem.FileName;
    LBackupBefore := ReadBytes(LBackupName);
    Require(Length(LBackupBefore)>0,'raw-block backup was not persisted');
    LOther := TStorageActionThread.Create;
    LOther.Storage := FStorage;
    if ACheck=amscInFlightShutdown then begin
      LOther.Action := saCloseNode;
      LOther.Node := FNode;
    end else if ACheck=amscFailedQueueDrain then LOther.Action := saStop
    else LOther.Action := saFlush;
    LOther.Start;
    Require(LOther.Entered.WaitFor(5000)=wrSignaled,'competing owner did not start');
    Require(LOther.Done.WaitFor(50)=wrTimeout,'competing flush/close did not wait for the in-flight write');
    FControl.ReleaseWrite.SetEvent;
    Require(LOther.Done.WaitFor(5000)=wrSignaled,'failure left a flush or cooperative shutdown blocked');
    if ACheck=amscInFlightShutdown then begin
      FNode := nil;
      FStorage := nil;
      Require(LOther.ErrorMessage='','node shutdown failed: '+LOther.ErrorMessage);
      Require(not TNode.NodeExists,'node survived cooperative shutdown');
    end else begin
      WaitForFailure;
      if ACheck=amscBackgroundWriteFailure then
        Require(LOther.ErrorMessage<>'','an admitted competing flush certified a failed index')
      else Require(LOther.ErrorMessage='','failed queue teardown raised: '+LOther.ErrorMessage);
      FStorage.UseMultithread := False;
      LRaised := False;
      try
        FStorage.SaveBlockChainBlock(FOperations);
      except
        on E: EWriteError do LRaised := True;
      end;
      Require(LRaised,'a later block/index write bypassed the failure latch');
      LIndexBefore := ReadBytes(LIndexName);
      FreeAndNil(FNode);
      FStorage := nil;
      Require(SameBytes(LIndexBefore,ReadBytes(LIndexName)),
        'failed index was retried or modified during cleanup');
    end;
    Require(FControl.FailureHits>0,'no actual background payload write failed');
    Require(FControl.SkippedCloseFlush=1,'unsafe failed-index close flush was not suppressed');
    Require(SameBytes(LBackupBefore,ReadBytes(LBackupName)),
      'failed indexing changed the previous raw-block backup');
    LIndexBefore := ReadBytes(LIndexName);
    Require((Length(LIndexBefore)>6) and (LIndexBefore[6]=0),
      'failed logical index was marked stable');
  finally
    FControl.ReleaseWrite.SetEvent;
    LOther.Free;
    LSave.Free;
  end;
end;

procedure TStorageFixture.CheckSuccessfulDrain;
var LLoaded: TPCOperationsComp; LFileName: String; LBytes: TBytes;
begin
  Require(FStorage.SaveBlockChainBlock(FOperations),'normal save failed');
  FStorage.UseMultithread := False;
  Require(FStorage.PendingToSave=0,'normal shutdown did not drain the index queue');
  FStorage.FlushCache;
  Require(not FNode.HasStorageFailure,'normal drain latched a false failure');
  LLoaded := TPCOperationsComp.Create(nil);
  try
    Require(FStorage.LoadBlockChainBlock(LLoaded,0),'normal saved block is unreadable');
    Require(LLoaded.Count=64,'normal drain lost operations');
  finally
    LLoaded.Free;
  end;
  LFileName := FStorage.FileMem.FileName;
  FreeAndNil(FNode);
  FStorage := nil;
  LBytes := ReadBytes(LFileName);
  Require((Length(LBytes)>6) and (LBytes[6]=1),'successful close did not persist a stable index');
end;

procedure TStorageFixture.Run(ACheck: TAbstractMemStorageFailureCheck);
begin
  case ACheck of
    amscFalseCompletionFlush: CheckFalseFlush(False);
    amscBackgroundWriteFailure: CheckWorker(ACheck);
    amscFailedQueueDrain: CheckWorker(ACheck);
    amscFailedCleanupPreservesMarker: CheckFalseFlush(True);
    amscSuccessfulDrain: CheckSuccessfulDrain;
    amscInFlightShutdown: CheckWorker(ACheck);
  end;
end;

function RunAbstractMemStorageFailureCheck(ACheck: TAbstractMemStorageFailureCheck; const ALogPath: String): Integer;
var LFixture: TStorageFixture; LLog: TStringList;
begin
  Result := 1;
  LLog := TStringList.Create;
  try
    LLog.Add('CHECK '+GetEnumName(TypeInfo(TAbstractMemStorageFailureCheck),Ord(ACheck)));
    try
      LFixture := TStorageFixture.Create(ALogPath);
      try
        LFixture.Run(ACheck);
      finally
        LFixture.Free;
      end;
      LLog.Add('PASS');
      Result := 0;
    except
      on E: Exception do LLog.Add('FAIL '+E.ClassName+': '+E.Message);
    end;
    LLog.SaveToFile(ALogPath);
  finally
    LLog.Free;
  end;
end;

function RunAbstractMemStorageFailureChecks(const ALogPath: String): Integer;
var LCheck: TAbstractMemStorageFailureCheck; LLog: TStringList;
begin
  Result := 0;
  LLog := TStringList.Create;
  try
    for LCheck := Low(TAbstractMemStorageFailureCheck) to High(TAbstractMemStorageFailureCheck) do begin
      if RunAbstractMemStorageFailureCheck(LCheck,ALogPath+'.case-'+IntToStr(Ord(LCheck))+'.log')<>0 then
        Result := 1;
      LLog.Add('Completed '+GetEnumName(TypeInfo(TAbstractMemStorageFailureCheck),Ord(LCheck)));
      LLog.SaveToFile(ALogPath);
    end;
    if Result=0 then LLog.Add('PASS 6') else LLog.Add('FAIL');
    LLog.SaveToFile(ALogPath);
  finally
    LLog.Free;
  end;
end;

end.
