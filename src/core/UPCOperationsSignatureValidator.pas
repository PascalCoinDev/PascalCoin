unit UPCOperationsSignatureValidator;

{ Copyright (c) 2019 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://www.pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

{
  This unit adds a TPCOperationsSignatureValidator class that will check
  signature of operations in a multithread mode

  NOTE: This object is only a helper for fast processing speed when
  multithreading can help. There is no warranty that will validate all

  }


interface

{$I config.inc}

Uses UThread, UAccounts, UPCOrderedLists, UBlockChain,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type
  TPCOperationsSignatureValidator = Class;

  TPCOperationsSignatureValidatorThread = Class(TPCThread)
  private
    FValidator : TPCOperationsSignatureValidator;
    //
  protected
    procedure BCExecute; override;
  public
    Constructor Create(AValidator : TPCOperationsSignatureValidator);
  End;

  TPCOperationsSignatureValidator = Class
  private
    FLock : TPCCriticalSection;
    //
    FOperationsList : TList<TPCOperation>;
    FLastIndexOperations : Integer;
    //
    FSafeBoxTransaction : TPCSafeBoxTransaction;
    FValidatedOkCount : Integer;
    FValidatedErrorCount : Integer;
  protected
    function GetNextOperation(AValidatorThread : TPCOperationsSignatureValidatorThread) : TPCOperation;
    procedure SetOperationCheckResult(AValidatorThread : TPCOperationsSignatureValidatorThread; APCOperation : TPCOperation; AValidated : Boolean);
  public
    Constructor Create(ASafeBoxTransaction : TPCSafeBoxTransaction);
    destructor Destroy; override;
    function Validate(AOperationsList : TList<TPCOperation>) : Integer;
    class procedure MultiThreadPreValidateSignatures(ASafeBoxTransaction : TPCSafeBoxTransaction; AOperationsHashTree : TOperationsHashTree); overload;
    class procedure MultiThreadPreValidateSignatures(ASafeBoxTransaction : TPCSafeBoxTransaction; APCOperationsCompList: TList<TPCOperationsComp>); overload;
  End;

implementation

Uses
  SysUtils,
  ULog, UBaseTypes,
  UCommon;

var _Cpus : Integer = 0;

{ TPCOperationsSignatureValidator }

constructor TPCOperationsSignatureValidator.Create(ASafeBoxTransaction: TPCSafeBoxTransaction);
begin
  FSafeBoxTransaction := ASafeBoxTransaction;
  FLastIndexOperations := -1;
  FLock := TPCCriticalSection.Create('');
  FValidatedOkCount := 0;
  FValidatedErrorCount := 0;
end;

destructor TPCOperationsSignatureValidator.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

function TPCOperationsSignatureValidator.GetNextOperation(AValidatorThread : TPCOperationsSignatureValidatorThread) : TPCOperation;
var LIndex : Integer;
begin
  FLock.Acquire;
  try
    // Search new
    LIndex := FLastIndexOperations + 1; // Move to next
    if (LIndex<FOperationsList.Count) then begin
      Result := FOperationsList[LIndex];
      FLastIndexOperations := Lindex;
    end else Result := Nil;
  finally
    FLock.Release;
  end;
end;

class procedure TPCOperationsSignatureValidator.MultiThreadPreValidateSignatures(
  ASafeBoxTransaction: TPCSafeBoxTransaction; APCOperationsCompList: TList<TPCOperationsComp>);
var LList : TList<TPCOperation>;
  i : Integer;
  LMultiThreadValidator : TPCOperationsSignatureValidator;
  LValidatedOk, LValidatedError, LValidatedTotal : Integer;
  LTC : TTickCount;
begin
  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  if _Cpus<=1 then Exit;

  LList := TList<TPCOperation>.Create;
  Try
    for i := 0 to APCOperationsCompList.Count-1 do begin
      APCOperationsCompList[i].OperationsHashTree.GetOperationsList(LList,True);
    end;
    LTC := TPlatform.GetTickCount;
    LMultiThreadValidator := TPCOperationsSignatureValidator.Create(ASafeBoxTransaction);
    try
      LValidatedTotal := LMultiThreadValidator.Validate(LList);
      LValidatedOk := LMultiThreadValidator.FValidatedOkCount;
      LValidatedError := LMultiThreadValidator.FValidatedErrorCount;
      LTC := TPlatform.GetElapsedMilliseconds(LTC);
      if (LValidatedTotal>0) and (LTC>0) and ((LValidatedOk>0) or (LValidatedError>0))  then begin
        TLog.NewLog(ltdebug,ClassName,Format('Validated %d operations from %d Blocks with %d signatures Ok and %d signatures Error in %d miliseconds avg %.2f op/sec',[LValidatedTotal,APCOperationsCompList.Count,LValidatedOk,LValidatedError,LTC,LValidatedTotal*1000/LTC]));
      end;
    finally
      LMultiThreadValidator.Free;
    end;

  Finally
    LList.Free;
  End;

end;


class procedure TPCOperationsSignatureValidator.MultiThreadPreValidateSignatures(
  ASafeBoxTransaction: TPCSafeBoxTransaction; AOperationsHashTree: TOperationsHashTree);
var LMultiThreadValidator : TPCOperationsSignatureValidator;
  LValidatedOk, LValidatedError, LValidatedTotal : Integer;
  LTC : TTickCount;
  LList : TList<TPCOperation>;
begin
  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  if _Cpus<=1 then Exit;
  if AOperationsHashTree.OperationsCount<_Cpus then Exit;   // If less than cpus, no need for multithreading...

  LTC := TPlatform.GetTickCount;
  LMultiThreadValidator := TPCOperationsSignatureValidator.Create(ASafeBoxTransaction);
  try
    LList := TList<TPCOperation>.Create;
    Try
      AOperationsHashTree.GetOperationsList(Llist,True);
      if LList.Count<_Cpus then Exit; // No need for multithreading...

      LValidatedTotal := LMultiThreadValidator.Validate(LList);
      LValidatedOk := LMultiThreadValidator.FValidatedOkCount;
      LValidatedError := LMultiThreadValidator.FValidatedErrorCount;
      LTC := TPlatform.GetElapsedMilliseconds(LTC);
      if (LValidatedTotal>0) and (LTC>0) and ((LValidatedOk>0) or (LValidatedError>0))  then begin
        TLog.NewLog(ltdebug,ClassName,Format('Validated %d operations with %d signatures Ok and %d signatures Error in %d miliseconds avg %.2f op/sec',[LValidatedTotal,LValidatedOk,LValidatedError,LTC,LValidatedTotal*1000/LTC]));
      end;
    Finally
      LList.Free;
    End;
  finally
    LMultiThreadValidator.Free;
  end;
end;

procedure TPCOperationsSignatureValidator.SetOperationCheckResult(
  AValidatorThread: TPCOperationsSignatureValidatorThread;
  APCOperation: TPCOperation; AValidated: Boolean);
begin
  FLock.Acquire;
  try
    if AValidated then inc(FValidatedOkCount)
    else inc(FValidatedErrorCount);
  finally
    FLock.Release;
  end;
end;

function TPCOperationsSignatureValidator.Validate(AOperationsList : TList<TPCOperation>) : Integer;
var LMaxThreads : Integer;
  LThreads : TList<TPCOperationsSignatureValidatorThread>;
  i,LTerminatedThreads : Integer;
begin
  FValidatedOkCount := 0;
  FValidatedErrorCount := 0;
  if AOperationsList.Count<=0 then Exit(0);

  FLastIndexOperations := -1;

  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  LMaxThreads := _Cpus-1;
  if (LMaxThreads<=0) then LMaxThreads := 1;
  LThreads := TList<TPCOperationsSignatureValidatorThread>.Create;
  Try
    // Init values
    FLastIndexOperations := -1;
    FOperationsList := AOperationsList;

    // Step 1: Create the threads:
    for i := 1 to LMaxThreads do begin
      LThreads.Add( TPCOperationsSignatureValidatorThread.Create(Self) );
    end;
    // Step 2: Start the threads
    for i := 0 to LThreads.Count-1 do begin
      LThreads[i].Suspended := False;
    end;
    // Step 3: Wait until error of finalized
    repeat
      LTerminatedThreads := 0;
      for i := 0 to LThreads.Count-1 do begin
        if LThreads[i].Terminated then inc(LTerminatedThreads);
      end;
      Sleep(1);
    until (LTerminatedThreads>=LThreads.Count);
  Finally
    for i := 0 to LThreads.Count-1 do begin
      LThreads[i].Terminate;
      LThreads[i].WaitFor;
      LThreads[i].Free;
    end;
  End;
  Result := FOperationsList.Count;
end;

{ TPCOperationsSignatureValidatorThread }

procedure TPCOperationsSignatureValidatorThread.BCExecute;
var LOperation : TPCOperation;
begin
  repeat
    LOperation := FValidator.GetNextOperation(Self);
    if Assigned(LOperation) then begin
      if Not LOperation.HasValidSignature then begin
        // Only will validate if HasValidSignature is False (Not validated before)
        FValidator.SetOperationCheckResult(Self,LOperation, LOperation.IsValidSignatureBasedOnCurrentSafeboxState(FValidator.FSafeBoxTransaction));
      end;
    end;
  until (Not Assigned(LOperation)) or (Terminated);
end;

constructor TPCOperationsSignatureValidatorThread.Create(AValidator: TPCOperationsSignatureValidator);
begin
  FValidator := AValidator;
  inherited Create(True);
  FreeOnTerminate := False;
end;

end.
