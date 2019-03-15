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

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

Uses Classes, UThread, UAccounts, UBlockChain,
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
    FProgressNotify : TProgressNotify;
  protected
    function GetNextOperation(AValidatorThread : TPCOperationsSignatureValidatorThread) : TPCOperation;
    procedure SetOperationCheckResult(AValidatorThread : TPCOperationsSignatureValidatorThread; APCOperation : TPCOperation; AValidated : Boolean);
  public
    Constructor Create(ASafeBoxTransaction : TPCSafeBoxTransaction; AProgressNotify : TProgressNotify);
    destructor Destroy; override;
    function Validate(AOperationsList : TList<TPCOperation>) : Integer;
    class procedure MultiThreadPreValidateSignatures(ASafeBoxTransaction : TPCSafeBoxTransaction; AOperationsHashTree : TOperationsHashTree; AProgressNotify : TProgressNotify); overload;
    class procedure MultiThreadPreValidateSignatures(ASafeBoxTransaction : TPCSafeBoxTransaction; APCOperationsList : TList<TPCOperation>; AProgressNotify : TProgressNotify); overload;
    class procedure MultiThreadPreValidateSignatures(ASafeBoxTransaction : TPCSafeBoxTransaction; APCOperationsCompList: TList<TPCOperationsComp>; AProgressNotify : TProgressNotify); overload;
  End;

implementation

Uses
  SysUtils,
  ULog, UBaseTypes,
  UCommon;

var _Cpus : Integer = 0;

{ TPCOperationsSignatureValidator }

constructor TPCOperationsSignatureValidator.Create(ASafeBoxTransaction: TPCSafeBoxTransaction; AProgressNotify : TProgressNotify);
begin
  FSafeBoxTransaction := ASafeBoxTransaction;
  FLastIndexOperations := -1;
  FLock := TPCCriticalSection.Create('');
  FValidatedOkCount := 0;
  FValidatedErrorCount := 0;
  FProgressNotify := AProgressNotify;
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

class procedure TPCOperationsSignatureValidator.MultiThreadPreValidateSignatures(ASafeBoxTransaction: TPCSafeBoxTransaction;
  APCOperationsList: TList<TPCOperation>; AProgressNotify: TProgressNotify);
var
  i : Integer;
  LMultiThreadValidator : TPCOperationsSignatureValidator;
  LValidatedOk, LValidatedError, LValidatedTotal : Integer;
  LTC : TTickCount;
begin
  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  if _Cpus<=1 then Exit;

    LTC := TPlatform.GetTickCount;
    LMultiThreadValidator := TPCOperationsSignatureValidator.Create(ASafeBoxTransaction,AProgressNotify);
    try
      LValidatedTotal := LMultiThreadValidator.Validate(APCOperationsList);
      LValidatedOk := LMultiThreadValidator.FValidatedOkCount;
      LValidatedError := LMultiThreadValidator.FValidatedErrorCount;
      LTC := TPlatform.GetElapsedMilliseconds(LTC);
      if (LValidatedTotal>0) and (LTC>0) and ((LValidatedOk>0) or (LValidatedError>0))  then begin
        TLog.NewLog(ltdebug,ClassName,Format('Validated %d operations with %d signatures Ok and %d signatures Error in %d miliseconds avg %.2f op/sec',[LValidatedTotal,LValidatedOk,LValidatedError,LTC,LValidatedTotal*1000/LTC]));
      end;
    finally
      LMultiThreadValidator.Free;
    end;

end;

class procedure TPCOperationsSignatureValidator.MultiThreadPreValidateSignatures(
  ASafeBoxTransaction: TPCSafeBoxTransaction; APCOperationsCompList: TList<TPCOperationsComp>; AProgressNotify : TProgressNotify);
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
    LMultiThreadValidator := TPCOperationsSignatureValidator.Create(ASafeBoxTransaction,AProgressNotify);
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
  ASafeBoxTransaction: TPCSafeBoxTransaction; AOperationsHashTree: TOperationsHashTree; AProgressNotify : TProgressNotify);
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
  LMultiThreadValidator := TPCOperationsSignatureValidator.Create(ASafeBoxTransaction,AProgressNotify);
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
var LLastTC : TTickCount;
  procedure DoNotify;
  var LMsg : String;
    LCurPos,LTotal : Int64;
  begin
    if (Assigned(FProgressNotify)) and (TPlatform.GetElapsedMilliseconds(LLastTC)>200) then begin
      LLastTC := TPlatform.GetTickCount;
      FLock.Acquire;
      try
        LMsg := Format('Validating signatures %d/%d',[FLastIndexOperations,FOperationsList.Count]);
        LCurPos := FLastIndexOperations;
        LTotal := FOperationsList.Count;
      finally
        FLock.Release;
      end;
      //
      FProgressNotify(Self,LMsg,LCurPos,LTotal);
    end;
  end;
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
  if (_Cpus>2) then LMaxThreads := _Cpus-1
  else LMaxThreads := _Cpus;
  if (LMaxThreads<=0) then LMaxThreads := 1;
  if (LMaxThreads>7) then LMaxThreads := 7;

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
    LLastTC := TPlatform.GetTickCount;
    repeat
      DoNotify;
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
    LThreads.Free;
  End;
  Result := FOperationsList.Count;
end;

{ TPCOperationsSignatureValidatorThread }

procedure TPCOperationsSignatureValidatorThread.BCExecute;
var LOperation : TPCOperation;
    LIsValid : Boolean;
begin
  repeat
    LOperation := FValidator.GetNextOperation(Self);
    if Assigned(LOperation) then begin
      if Not LOperation.HasValidSignature then begin
        // Only will validate if HasValidSignature is False (Not validated before)
        try
          LIsValid := LOperation.IsValidSignatureBasedOnCurrentSafeboxState(FValidator.FSafeBoxTransaction);
        except
          LIsValid := False;
        end;
        FValidator.SetOperationCheckResult(Self,LOperation, LIsValid);
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
