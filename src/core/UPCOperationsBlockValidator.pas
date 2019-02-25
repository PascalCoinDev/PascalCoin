unit UPCOperationsBlockValidator;

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
  This unit adds a TPCOperationsBlockValidator class that will check
  TOperationBlock in a multithread mode

  NOTE: This object is only a helper for fast processing speed when
  multithreading can help. There is no warranty that will validate all

  }


interface

{$I config.inc}

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}


Uses UThread, UAccounts, UPCOrderedLists, UBlockChain,
  {$IFNDEF FPC}System.Generics.Collections{$ELSE}Generics.Collections{$ENDIF};

type
  TPCOperationsBlockValidator = Class;

  TPCOperationsBlockValidatorThread = Class(TPCThread)
  private
    FValidator : TPCOperationsBlockValidator;
    //
  protected
    procedure BCExecute; override;
  public
    Constructor Create(AValidator : TPCOperationsBlockValidator);
  End;

  TPCOperationsBlockValidator = Class
  private
    FLock : TPCCriticalSection;
    //
    FPCOperationsCompList : TList<TPCOperationsComp>;
    FLastIndexOperationsBlock : Integer;
    //
    FValidatedOkCount : Integer;
    FValidatedErrorCount : Integer;
  protected
    function GetNextOperationBlock(var ANextOperationBlock : TOperationBlock; var AIndex : Integer) : Boolean;
    procedure SetOperationBlockResult(const AOperationBlock : TOperationBlock; AIndex : Integer; AValidated : Boolean);
  public
    Constructor Create;
    destructor Destroy; override;
    function Validate(APCOperationsCompList : TList<TPCOperationsComp>; var AValidatedOkCount, AValidatedErrorCount : Integer) : Integer;
    class function MultiThreadValidateOperationsBlock(APCOperationsCompList : TList<TPCOperationsComp>) : Boolean;
  End;

implementation

Uses
  SysUtils,
  ULog, UBaseTypes,
  UCommon;

var _Cpus : Integer = 0;

{ TPCOperationsBlockValidator }

constructor TPCOperationsBlockValidator.Create;
begin
  FLastIndexOperationsBlock := -1;
  FLock := TPCCriticalSection.Create('');
end;

destructor TPCOperationsBlockValidator.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

function TPCOperationsBlockValidator.GetNextOperationBlock(var ANextOperationBlock : TOperationBlock; var AIndex : Integer) : Boolean;
begin
  FLock.Acquire;
  try
    // Search new
    AIndex := FLastIndexOperationsBlock + 1; // Move to next
    if (AIndex<FPCOperationsCompList.Count) then begin
      ANextOperationBlock := FPCOperationsCompList[AIndex].OperationBlock;
      Result := True;
      FLastIndexOperationsBlock := AIndex;
    end else Result := False;
  finally
    FLock.Release;
  end;
end;

class function TPCOperationsBlockValidator.MultiThreadValidateOperationsBlock(APCOperationsCompList: TList<TPCOperationsComp>): Boolean;
var LMultiThreadValidator : TPCOperationsBlockValidator;
  LValidatedOk, LValidatedError, LValidatedTotal : Integer;
  LTC : TTickCount;
begin
  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  if _Cpus<=1 then Exit;
  if APCOperationsCompList.Count<_Cpus then Exit;   // If less than cpus, no need for multithreading...

  LTC := TPlatform.GetTickCount;
  LMultiThreadValidator := TPCOperationsBlockValidator.Create;
  try
    LValidatedTotal := LMultiThreadValidator.Validate(APCOperationsCompList,LValidatedOk,LValidatedError);
    LTC := TPlatform.GetElapsedMilliseconds(LTC);
    if (LValidatedTotal>0) and (LTC>0) then begin
      TLog.NewLog(ltdebug,ClassName,Format('Validated %d Operation blocks info with %d valids and %d Errors in %d miliseconds avg %.2f op/sec',[LValidatedTotal,LValidatedOk,LValidatedError,LTC,LValidatedTotal*1000/LTC]));
    end;
    Result := LValidatedOk = LValidatedTotal;
  finally
    LMultiThreadValidator.Free;
  end;
end;

procedure TPCOperationsBlockValidator.SetOperationBlockResult(const AOperationBlock : TOperationBlock; AIndex : Integer; AValidated: Boolean);
begin
  FLock.Acquire;
  try
    if AValidated then inc(FValidatedOkCount)
    else inc(FValidatedErrorCount);
    FPCOperationsCompList[AIndex].HasValidOperationBlockInfo := AValidated;
  finally
    FLock.Release;
  end;
end;

function TPCOperationsBlockValidator.Validate(APCOperationsCompList : TList<TPCOperationsComp>; var AValidatedOkCount, AValidatedErrorCount : Integer) : Integer;
var LMaxThreads : Integer;
  LThreads : TList<TPCOperationsBlockValidatorThread>;
  i,LTerminatedThreads : Integer;
begin
  FValidatedOkCount := 0;
  FValidatedErrorCount := 0;
  if APCOperationsCompList.Count<=0 then Exit(0);

  FLastIndexOperationsBlock := -1;

  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  LMaxThreads := _Cpus-1;
  if (LMaxThreads<=0) then LMaxThreads := 1;
  LThreads := TList<TPCOperationsBlockValidatorThread>.Create;
  Try
    // Init values
    FLastIndexOperationsBlock := -1;
    FPCOperationsCompList := APCOperationsCompList;

    // Step 1: Create the threads:
    for i := 1 to LMaxThreads do begin
      LThreads.Add( TPCOperationsBlockValidatorThread.Create(Self) );
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
    LThreads.Free;
  End;
  AValidatedOkCount := FValidatedOkCount;
  AValidatedErrorCount := FValidatedErrorCount;
  Result := FPCOperationsCompList.Count;
end;

{ TPCOperationsBlockValidatorThread }

procedure TPCOperationsBlockValidatorThread.BCExecute;
var LOperationBlock : TOperationBlock;
  LErrors : String;
  LValidated : Boolean;
  LIndex : Integer;
begin
  repeat
    if FValidator.GetNextOperationBlock( LOperationBlock, LIndex ) then begin
      LValidated := TPCSafeBox.IsValidOperationBlock(LOperationBlock,LErrors);
      FValidator.SetOperationBlockResult(LOperationBlock, LIndex, LValidated);
    end else Terminate;
  until (Terminated);
end;

constructor TPCOperationsBlockValidatorThread.Create(AValidator : TPCOperationsBlockValidator);
begin
  FValidator := AValidator;
  inherited Create(True);
  FreeOnTerminate := False;
end;

end.
