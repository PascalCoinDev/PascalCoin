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


Uses UThread, UAccounts, UPCOrderedLists, UBlockChain, Classes,
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
    FLockedList : TPCThreadList<TOperationBlock>;
    FThreads : TList<TPCOperationsBlockValidatorThread>;
    FErrorsList : TStringList;
    //
    FPCOperationsCompList : TList<TPCOperationsComp>; // Optional field
    FLastIndexOperationsBlock : Integer;
    //
    FValidatedOkCount : Integer;
    FValidatedErrorCount : Integer;
  protected
    function GetNextOperationBlock(var ANextOperationBlock : TOperationBlock; var AIndex : Integer) : Boolean;
    procedure SetOperationBlockResult(const AOperationBlock : TOperationBlock; AIndex : Integer; AValidated : Boolean; const AErrorDetected : String);
  public
    Constructor Create;
    destructor Destroy; override;
    procedure AddToValidate(AList : TList<TOperationBlock>); overload;
    procedure AddToValidate(AOperationBlock : TOperationBlock); overload;
    procedure StartThreads;
    procedure GetStatus(out AValidatedOk, AValidatedError, APendingToValidate : Integer);
    procedure WaitUntilAllValidatedOrErrorFound;
    procedure FillErrors(var AErrors : String);
    procedure EndThreads;
    property ValidatedOkCount : Integer read FValidatedOkCount;
    property ValidatedErrorCount : Integer read FValidatedErrorCount;
    function ValidateAndWaitUntilTerminate(APCOperationsCompList : TList<TPCOperationsComp>; var AValidatedOkCount, AValidatedErrorCount : Integer) : Integer;
    class function MultiThreadValidateOperationsBlock(APCOperationsCompList : TList<TPCOperationsComp>) : Boolean;
  End;

implementation

Uses
  SysUtils,
  ULog, UBaseTypes,
  UCommon;

var _Cpus : Integer = 0;

{ TPCOperationsBlockValidator }

procedure TPCOperationsBlockValidator.AddToValidate(AList: TList<TOperationBlock>);
var i : Integer;
  LList : TList<TOperationBlock>;
begin
  LList := FLockedList.LockList;
  try
    for i := 0 to AList.Count-1 do begin
      LList.Add(AList[i]);
    end;
  finally
    FLockedList.UnlockList;
  end;
end;

procedure TPCOperationsBlockValidator.AddToValidate(AOperationBlock: TOperationBlock);
var LList : TList<TOperationBlock>;
begin
  LList := FLockedList.LockList;
  try
    LList.Add(AOperationBlock);
  finally
    FLockedList.UnlockList;
  end;
end;

constructor TPCOperationsBlockValidator.Create;
begin
  FLastIndexOperationsBlock := -1;
  FLockedList := TPCThreadList<TOperationBlock>.Create(ClassName);
  FPCOperationsCompList := Nil; // This field is external
  FThreads := Nil;
  FErrorsList := TStringList.Create;
end;

destructor TPCOperationsBlockValidator.Destroy;
begin
  EndThreads;
  FreeAndNil(FLockedList);
  FreeAndNil(FErrorsList);
  inherited;
end;

procedure TPCOperationsBlockValidator.EndThreads;
var i : Integer;
begin
  FLockedList.LockList;
  try
    if Not Assigned(FThreads) then Exit;

    for i := 0 to FThreads.Count-1 do begin
      FThreads[i].Terminate;
    end;
  finally
    FLockedList.UnlockList;
  end;
  // WaitFor without locking
  for i := 0 to FThreads.Count-1 do begin
    FThreads[i].WaitFor;
  end;
  // Finished
  FLockedList.LockList;
  try
    for i := 0 to FThreads.Count-1 do begin
      FThreads[i].Free;
    end;
    FreeAndNil(FThreads);
  finally
    FLockedList.UnlockList;
  end;
end;

procedure TPCOperationsBlockValidator.FillErrors(var AErrors: String);
begin
  FLockedList.LockList;
  try
    AErrors := FErrorsList.Text;
  finally
    FLockedList.UnlockList;
  end;
end;

function TPCOperationsBlockValidator.GetNextOperationBlock(var ANextOperationBlock : TOperationBlock; var AIndex : Integer) : Boolean;
var LList : TList<TOperationBlock>;
begin
  LList := FLockedList.LockList;
  try
    // Search new
    AIndex := FLastIndexOperationsBlock + 1; // Move to next
    if (AIndex<LList.Count) then begin
      ANextOperationBlock := LList[AIndex];
      Result := True;
      FLastIndexOperationsBlock := AIndex;
    end else Result := False;
  finally
    FLockedList.UnlockList;
  end;
end;

procedure TPCOperationsBlockValidator.GetStatus(out AValidatedOk, AValidatedError, APendingToValidate: Integer);
var LList : TList<TOperationBlock>;
begin
  LList := FLockedList.LockList;
  try
    AValidatedOk := FValidatedOkCount;
    AValidatedError := FValidatedErrorCount;
    if LList.Count>0 then
      APendingToValidate := LList.Count - (FLastIndexOperationsBlock + 1)
    else APendingToValidate := 0;
  finally
    FLockedList.UnlockList;
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
    LValidatedTotal := LMultiThreadValidator.ValidateAndWaitUntilTerminate(APCOperationsCompList,LValidatedOk,LValidatedError);
    LTC := TPlatform.GetElapsedMilliseconds(LTC);
    if (LValidatedTotal>0) and (LTC>0) then begin
      TLog.NewLog(ltdebug,ClassName,Format('Validated %d Operation blocks info with %d valids and %d Errors in %d miliseconds avg %.2f op/sec',[LValidatedTotal,LValidatedOk,LValidatedError,LTC,LValidatedTotal*1000/LTC]));
    end;
    Result := LValidatedOk = LValidatedTotal;
  finally
    LMultiThreadValidator.Free;
  end;
end;

procedure TPCOperationsBlockValidator.SetOperationBlockResult(const AOperationBlock : TOperationBlock; AIndex : Integer; AValidated: Boolean; const AErrorDetected : String);
var LList : TList<TOperationBlock>;
begin
  LList := FLockedList.LockList;
  try
    if AValidated then inc(FValidatedOkCount)
    else begin
      FErrorsList.Add(AErrorDetected + ' ' + TPCOperationsComp.OperationBlockToText(AOperationBlock) );
      inc(FValidatedErrorCount);
    end;
    if Assigned(FPCOperationsCompList) then begin
      FPCOperationsCompList[AIndex].HasValidOperationBlockInfo := AValidated;
    end;
  finally
    FLockedList.UnlockList;
  end;
end;

procedure TPCOperationsBlockValidator.StartThreads;
var LMaxThreads : Integer;
  i : Integer;
  LList : TList<TOperationBlock>;
begin
  EndThreads; // Ensure no active threads

  FValidatedOkCount := 0;
  FValidatedErrorCount := 0;
  FLastIndexOperationsBlock := -1;
  FErrorsList.Clear;

  if _Cpus<=0 then begin
    _Cpus := TLogicalCPUCount.GetLogicalCPUCount;
  end;
  if (_Cpus>2) then LMaxThreads := _Cpus-1
  else LMaxThreads := _Cpus;
  if (LMaxThreads<=0) then LMaxThreads := 1;
  if (LMaxThreads>7) then LMaxThreads := 7;

  FThreads := TList<TPCOperationsBlockValidatorThread>.Create;
  FLockedList.LockList;
  try
    for i := 1 to LMaxThreads do begin
      FThreads.Add( TPCOperationsBlockValidatorThread.Create(Self) );
    end;
    for i := 0 to FThreads.Count-1 do begin
      FThreads[i].Suspended := False;
    end;
  finally
    FLockedList.UnlockList;
  end;

end;

function TPCOperationsBlockValidator.ValidateAndWaitUntilTerminate(APCOperationsCompList : TList<TPCOperationsComp>; var AValidatedOkCount, AValidatedErrorCount : Integer) : Integer;
var
  i,LTerminatedThreads : Integer;
  LList : TList<TOperationBlock>;
begin
  FValidatedOkCount := 0;
  FValidatedErrorCount := 0;
  if APCOperationsCompList.Count<=0 then Exit(0);

  EndThreads;

  LList := FLockedList.LockList;
  try
    for i := 0 to APCOperationsCompList.Count-1 do begin
      LList.Add(APCOperationsCompList[i].OperationBlock);
    end;
  finally
    FLockedList.UnlockList;
  end;

  FPCOperationsCompList := APCOperationsCompList;

  StartThreads;
  try
    // Wait until error of finalized
    WaitUntilAllValidatedOrErrorFound;
  Finally
    EndThreads;
  End;
  AValidatedOkCount := FValidatedOkCount;
  AValidatedErrorCount := FValidatedErrorCount;
  Result := FPCOperationsCompList.Count;
end;

procedure TPCOperationsBlockValidator.WaitUntilAllValidatedOrErrorFound;
var LContinue : Boolean;
  LList : TList<TOperationBlock>;
begin
  repeat
    Sleep(10);
    LList := FLockedList.LockList;
    try
      LContinue := (FValidatedErrorCount=0) And (FLastIndexOperationsBlock+1<LList.Count);
    finally
      FLockedList.UnlockList;
    end;
  until (Not LContinue);
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
      FValidator.SetOperationBlockResult(LOperationBlock, LIndex, LValidated, LErrors);
    end else Sleep(1);
  until (Terminated);
end;

constructor TPCOperationsBlockValidatorThread.Create(AValidator : TPCOperationsBlockValidator);
begin
  FValidator := AValidator;
  inherited Create(True);
  FreeOnTerminate := False;
end;

end.
