unit UThread.Tests;

{$mode delphi}
{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  UThread, Generics.Collections;

type

  TTestRecord = record
    Number : Integer;
  end;

  TSimplePipeline = class(TPipelineQueue<TTestRecord>)
  private
   FSucceeded : TList<TTestRecord>;
   FErrors : TList<TErrorResult>;
   function StageHandler(const AItems : TArray<TTestRecord>; out AErrors : TArray<TErrorResult>) : TArray<TTestRecord>;
  protected
    function ProcessStage(AStageNum : Integer; const AItems : TArray<TTestRecord>; out AErrors : TArray<TErrorResult>) : TArray<TTestRecord>; override;
    procedure HandleErrorItems(const AErrorItems : array of TErrorResult); override;
    procedure HandleFinishedItems(const AItems : array of TTestRecord); override;
  public
    property Succeeded : TList<TTestRecord> read FSucceeded;
    property Errors : TList<TErrorResult> read FErrors;
    constructor Create(AOwner : TComponent; AStageCount, AMaxWorkerThreads : Integer); overload;
    destructor Destroy; overload;
  end;

  TMempoolOperation = record
    Sender : String;
    Operation : String; // in production TPCOperationResume
    CurrentSignerNOperation : UInt32;
    SignerKey : String;
    OtherNOperations : TArray<UInt32>;
    OtherKeys : TArray<String>;
  end;

  TMempoolMock = class(TPipelineQueue<TMempoolOperation>)
  private const
    EXTRACT_ACCOUNT_DATA_LAG_MS = 2;
    VERIFY_SIGNATURE_LAG = 5;
    SEND_OPERATION_LAG = 5;
    LOG_OPERATION_LAG = 1;
    ACCEPT_OPERATION_LAG = 1;

    ILLEGAL_OP_RATE = 0.05; // percent of operations that are bad
    SEND_FAILURE_RATE = 0.05; // percent of operations that fail to propagate

  private
    FSafeBoxLock : TPCCriticalSection;
  protected
    function ProcessStage(AStageNum : Integer; const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>; override;
    procedure HandleErrorItems(const AErrorItems : array of TErrorResult); override;
    procedure HandleFinishedItems(const AItems : array of TMempoolOperation); override;

    // Mempool stages
    function ExtractSafeBoxData(const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>;
    function VerifyOperations(const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>;
    function PropagateOperations(const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>;
    function AcceptOperations(const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure SimulateClassicProcess(const AItems : array of TMempoolOperation);
  end;

  TPipelineTests = class(TTestCase)
  private
    procedure TestSimplePipeline(AStages, AWorkers : Integer);
  published
    procedure TestSimplePipeline_1Stage_1Worker;
    procedure TestSimplePipeline_3Stages_1Workers;
    procedure TestSimplePipeline_3Stages_2Workers;
    procedure TestSimplePipeline_3Stages_3Workers;
    procedure TestSimplePipeline_3Stages_4Workers;
    procedure TestMempoolSimulation;
    procedure TestMempoolClassicSimulation;
  end;

implementation

uses UMemory;

{ Implementation Functions }

procedure DoSomeWork(ANumAdditions, ANumMuls, ANumDivs : Integer);
var i, j : Integer;

begin
 for i := 1 to ANumAdditions do
   j := i + (i+1);
 for i := 1 to ANumMuls do
   j := i * (i+1);
 for i := 1 to ANumDivs do
   j := i div (i+1);
end;

{ TSimplePipeline }

constructor TSimplePipeline.Create(AOwner: TComponent; AStageCount, AMaxWorkerThreads : Integer);
begin
  inherited Create(AOwner, AStageCount, AMaxWorkerThreads);
  FSucceeded := TList<TTestRecord>.Create;
  FErrors := TList<TErrorResult>.Create;
end;

destructor TSimplePipeline.Destroy;
begin
 FSucceeded.Destroy;
 FErrors.Destroy;
end;

function TSimplePipeline.ProcessStage(AStageNum : Integer; const AItems : TArray<TTestRecord>; out AErrors : TArray<TErrorResult>) : TArray<TTestRecord>;
begin
 // Just repeat same stage each time, typically would use different stage per AStageNum
 Result := StageHandler(AItems, AErrors);
end;

procedure TSimplePipeline.HandleErrorItems(const AErrorItems: array of TErrorResult);
begin
  FErrors.AddRange(AErrorItems);
end;

procedure TSimplePipeline.HandleFinishedItems(const AItems : array of TTestRecord);
begin
  FSucceeded.AddRange(AItems);
end;

function TSimplePipeline.StageHandler(const AItems : TArray<TTestRecord>; out AErrors : TArray<TErrorResult>) : TArray<TTestRecord>;
var
  i : Integer;
  LSuccess : TList<TTestRecord>;
  LErrors : TList<TErrorResult>;
  LError : TErrorResult;
  LDisposables : TDisposables;
begin
  // Setup temp collections to build processed items
  LSuccess := LDisposables.AddObject( TList<TTestRecord>.Create ) as TList<TTestRecord>;
  LErrors := LDisposables.AddObject( TList<TErrorResult>.Create ) as TList<TErrorResult>;

  // Filter out odd numbers
  for i := Low(AItems) to High(AItems) do begin
    if AItems[i].Number mod 2 = 0 then begin
      LSuccess.Add(AItems[i]);
    end else begin
      LError.Item := AItems[i];
      LError.ErrorMessage := 'Item number was odd';
      LErrors.Add(LError);
    end;
    Sleep(5);
  end;

  // Return processed items
  AErrors := LErrors.ToArray;
  Result := LSuccess.ToArray;
end;

{ TMempoolMock }

constructor TMempoolMock.Create(AOwner : TComponent);
begin
  inherited Create(AOwner, 4, 2); // 4 stages with 2 worker threads
  FSafeBoxLock := TPCCriticalSection.Create('TMempoolMock');
end;

destructor TMempoolMock.Destroy;
begin
  inherited;
  FreeAndNil(FSafeBoxLock);
end;

function TMempoolMock.ProcessStage(AStageNum : Integer; const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>;
begin
  case AStageNum of
    0: Result := ExtractSafeBoxData(AItems, AErrors); // Stage 1: Extract Safebox data (NOperation/Key)
    1: Result := VerifyOperations(AItems, AErrors); // Stage 2: Verify operations
    2: begin
        // Stage 3: Propagate operations to other nodes
         PropagateOperations(AItems, AErrors);
         Result := AItems; // use argument, since we don't care about send failures in next phase
       end;
    3: Result := AcceptOperations(AItems, AErrors); // Stage 3: Accept operations in local SafeBox transaction
  end;
end;

procedure TMempoolMock.HandleErrorItems(const AErrorItems : array of TErrorResult);
var i : Integer;
begin
 for i := Low(AErrorItems) to High(AErrorItems) do
   Sleep(LOG_OPERATION_LAG);
end;

procedure TMempoolMock.HandleFinishedItems(const AItems : array of TMempoolOperation);
begin
  // end-of-pipeline, maybe save to disk?
end;

// Mempool stages
function TMempoolMock.ExtractSafeBoxData(const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>;
var
  LDisposables : TDisposables;
  LOutput : TList<TMempoolOperation>;
  i : Integer;
begin
 LOutput := LDisposables.AddObject( TList<TMempoolOperation>.Create ) as TList<TMempoolOperation>;
 FSafeBoxLock.Enter;
 try
   for i := Low(AItems) to High(AItems) do begin
     //AItems[i].Signer.PublicKey := lookup pubkey from safebox
     //AItems[i].Signer.NOperation := lookup n-operation from safebox
     DoSomeWork(10000, 10000, 10000);
     LOutput.Add(AItems[i]);
   end;
 finally
   FSafeBoxLock.Release;
 end;
 Result := LOutput.ToArray;
 SetLength(AErrors, 0);
end;

function TMempoolMock.VerifyOperations(const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>;
var
  LDisposables : TDisposables;
  LOutput : TList<TMempoolOperation>;
  LErrors : TList<TErrorResult>;
  LError : TErrorResult;
  i : Integer;
begin
  LOutput := LDisposables.AddObject( TList<TMempoolOperation>.Create ) as TList<TMempoolOperation>;
  LErrors := LDisposables.AddObject( TList<TErrorResult>.Create ) as TList<TErrorResult>;
  for i := Low(AItems) to High(AItems) do begin
    // if VerifySignature(AItems[i].Signature, AItems[i].SignerPubKey) then error
    DoSomeWork(10000, 10000, 10000);
    if random() >= ILLEGAL_OP_RATE then
      LOutput.Add(AItems[i])
    else begin
      LError.Item := AItems[i];
      LError.ErrorMessage := 'Invalid Operations';
      LErrors.Add(LError);
    end;
  end;
  Result := LOutput.ToArray;
  AErrors := LErrors.ToArray;
end;

function TMempoolMock.PropagateOperations(const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>;
var
  LDisposables : TDisposables;
  LOutput : TList<TMempoolOperation>;
  LErrors : TList<TErrorResult>;
  LError : TErrorResult;
  i : Integer;
begin
  LOutput := LDisposables.AddObject( TList<TMempoolOperation>.Create ) as TList<TMempoolOperation>;
  LErrors := LDisposables.AddObject( TList<TErrorResult>.Create ) as TList<TErrorResult>;
  for i := Low(AItems) to High(AItems) do begin
    // Send AItems[i] to all connected peers (except sender of AItems[i])
    DoSomeWork(10000, 10000, 10000);
    if random() >= SEND_FAILURE_RATE then
      LOutput.Add(AItems[i])
    else begin
      LError.Item := AItems[i];
      LError.ErrorMessage := 'Failed to propagate operation';
      LErrors.Add(LError);
    end;
  end;
  Result := LOutput.ToArray;
  AErrors := LErrors.ToArray;
end;

function TMempoolMock.AcceptOperations(const AItems : TArray<TMempoolOperation>; out AErrors : TArray<TErrorResult>) : TArray<TMempoolOperation>;
var
  LDisposables : TDisposables;
  LOutput : TList<TMempoolOperation>;
  LErrors : TList<TErrorResult>;
  LError : TErrorResult;
  i : Integer;
begin
  LOutput := LDisposables.AddObject( TList<TMempoolOperation>.Create ) as TList<TMempoolOperation>;
  LErrors := LDisposables.AddObject( TList<TErrorResult>.Create ) as TList<TErrorResult>;
  FSafeBoxLock.Enter;
  try
    for i := Low(AItems) to High(AItems) do begin
      DoSomeWork(10000, 10000, 10000);
      if random() >= ILLEGAL_OP_RATE then
        LOutput.Add(AItems[i])
      else begin
        LError.Item := AItems[i];
        LError.ErrorMessage := 'Failed to add operation to mempool';
        LErrors.Add(LError);
      end;
    end;
  finally
    FSafeBoxLock.Release;
  end;
  Result := LOutput.ToArray;
  AErrors := LErrors.ToArray;
end;

// Simulates the existing workflow of single-threaded single-operation processing
procedure TMempoolMock.SimulateClassicProcess(const AItems : array of TMempoolOperation);
var
  i : Integer;
  LOperations : TArray<TMempoolOperation>;
  LErrors : TArray<TErrorResult>;
begin
  for i := Low(AItems) to High(AItems) do begin
    LOperations := TArray<TMempoolOperation>.Create( AItems[i] );  // only 1 operation
    FSafeBoxLock.Enter;
    try
      LOperations := ExtractSafeBoxData(LOperations, LErrors);
      if Length(LErrors) > 0  then
        HandleErrorItems(LErrors);

      LOperations := VerifyOperations(LOperations, LErrors);
      if Length(LErrors) > 0  then
        HandleErrorItems(LErrors);

      PropagateOperations(LOperations, LErrors);
      if Length(LErrors) > 0  then
        HandleErrorItems(LErrors);

      AcceptOperations(LOperations, LErrors);
      if Length(LErrors) > 0  then
        HandleErrorItems(LErrors);

    finally
      FSafeBoxLock.Release;
    end;
  end;
end;

{ TPipelineTests }

procedure TPipelineTests.TestSimplePipeline_1Stage_1Worker;
begin
  TestSimplePipeline(1, 1);
end;

procedure TPipelineTests.TestSimplePipeline_3Stages_1Workers;
begin
  TestSimplePipeline(3, 1);
end;

procedure TPipelineTests.TestSimplePipeline_3Stages_2Workers;
begin
  TestSimplePipeline(3, 2);
end;

procedure TPipelineTests.TestSimplePipeline_3Stages_3Workers;
begin
  TestSimplePipeline(3, 3);
end;

procedure TPipelineTests.TestSimplePipeline_3Stages_4Workers;
begin
  TestSimplePipeline(3, 4);
end;

procedure TPipelineTests.TestSimplePipeline(AStages, AWorkers : Integer);
var
  LPipeline : TSimplePipeline;
  LDisposables : TDisposables;
  LList : TList<TTestRecord>;
  LRec : TTestRecord;
  i : Integer;
begin
  LPipeline := LDisposables.AddObject( TSimplePipeline.Create(nil, AStages, AWorkers) ) as TSimplePipeline;
  LList := LDisposables.AddObject( TList<TTestRecord>.Create ) as TList<TTestRecord>;
  for i := 1 to 100 do begin
    LRec.Number := i;
    LList.Add(LRec);
    if i mod 5 = 0 then begin
      LPipeline.EnqueueRange(LList.ToArray);
      LList.Clear;
    end;
  end;
  while NOT LPipeline.HasCompleted do
    Sleep(10);
  AssertEquals(50, LPipeline.Succeeded.Count);
  AssertEquals(50, LPipeline.Errors.Count);
  AssertEquals(AWorkers, LPipeline.HistoricalMaxActiveWorkerThreads);
  for i := 0 to LPipeline.Succeeded.Count - 1 do
    AssertEquals(LPipeline.Succeeded[i].Number mod 2, 0);
  for i := 0 to LPipeline.Errors.Count - 1 do
    AssertEquals(LPipeline.Errors[i].Item.Number mod 2, 1);
end;

procedure TPipelineTests.TestMempoolSimulation;
var
  LMempool : TMempoolMock;
  LDisposables : TDisposables;
  LList : TList<TMempoolOperation>;
  LRec : TMempoolOperation;
  i : Integer;
begin
  LMempool := LDisposables.AddObject( TMempoolMock.Create(nil) ) as TMempoolMock;
  LList := LDisposables.AddObject( TList<TMempoolOperation>.Create ) as TList<TMempoolOperation>;
  for i := 1 to 3000 do begin
    LList.Add(LRec);
    if i mod 5 = 0 then begin
      LMempool.SimulateClassicProcess(LList.ToArray);
      LList.Clear;
    end;
  end;
  while NOT LMempool.HasCompleted do
    Sleep(10);
end;

procedure TPipelineTests.TestMempoolClassicSimulation;
var
  LMempool : TMempoolMock;
  LDisposables : TDisposables;
  LList : TList<TMempoolOperation>;
  LRec : TMempoolOperation;
  i : Integer;
begin
  LMempool := LDisposables.AddObject( TMempoolMock.Create(nil) ) as TMempoolMock;
  LList := LDisposables.AddObject( TList<TMempoolOperation>.Create ) as TList<TMempoolOperation>;
  for i := 1 to 3000 do begin
    LList.Add(LRec);
    if i mod 5 = 0 then begin
      LMempool.SimulateClassicProcess(LList.ToArray);
      LList.Clear;
    end;
  end;
end;

initialization
  RegisterTest(TPipelineTests);

end.

